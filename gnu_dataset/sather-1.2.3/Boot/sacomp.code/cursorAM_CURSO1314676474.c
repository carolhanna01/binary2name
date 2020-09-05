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

typedef struct dGET_OPTIONS_struct {
 OB_HEADER header;
 } *dGET_OPTIONS;

typedef struct dSTR_struct {
 OB_HEADER header;
 } *dSTR;

typedef struct AM_CALL_ARG_struct {/* layout for AM_CALL_ARG */
 OB_HEADER header;
 struct dAM_EXPR_struct *expr;
 struct dMODE_struct *mode1;
 } *AM_CALL_ARG;

typedef struct ARRAYdAM_EXPR_struct {/* layout for ARRAY{$AM_EXPR} */
 OB_HEADER header;
 INT asize;
 struct dAM_EXPR_struct *arr_part[1];
 } *ARRAYdAM_EXPR;

typedef struct A_STAC1244966098_struct {/* layout for A_STACK{AM_CURSOR_POS} */
 OB_HEADER header;
 struct FLISTA886630705_struct *s;
 } *A_STAC1244966098;

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

typedef struct INT_do676118316_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT arg1;/* Formal argument: i */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_I,I_u_Im; /* used by builtin iter */
 } *INT_do676118316_frame;

typedef struct OUT_struct {/* layout for OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OUT;

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

typedef struct AM_ARRAY_EXPR_struct {/* layout for AM_ARRAY_EXPR */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 INT asize;
 struct dAM_EXPR_struct *arr_part[1];
 } *AM_ARRAY_EXPR;

typedef struct AM_ARR1990213312_frame_struct {
 INT state;
 AM_ARRAY_EXPR self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ARR1990213312_frame;

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

typedef struct AM_BND1640868266_frame_struct {
 INT state;
 AM_BND1124877163 self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_BND1640868266_frame;

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

typedef struct AM_BND83643009_frame_struct {
 INT state;
 AM_BND367211769 self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_BND83643009_frame;

typedef struct AM_BND260301329_struct {/* layout for AM_BND_ROUT_CALL_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *br;
 struct SFILE_ID_struct source1;
 struct TP_ROUT_struct *br_tp;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *AM_BND260301329;

typedef struct AM_BND1037254841_frame_struct {
 INT state;
 AM_BND260301329 self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_BND1037254841_frame;

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

typedef struct AM_CURSOR_struct {/* layout for AM_CURSOR */
 OB_HEADER header;
 struct dAM_struct *cur;
 struct dAM_struct *top1;
 struct A_STAC1244966098_struct *stack1;
 INT indent;
 INT loops;
 struct PROG_struct *prog;
 BOOL all_in_eval_orde;
 BOOL all_in_pre_order;
 BOOL assign_in_order;
 BOOL calls_in_eval_or;
 BOOL ignore_assert;
 BOOL ignore_next;
 BOOL ignore_post;
 BOOL ignore_pre;
 BOOL mark;
 BOOL started;
 BOOL with_side_effect;
 } *AM_CURSOR;

typedef struct AM_CURSOR_POS_struct {/* layout for AM_CURSOR_POS */
 OB_HEADER header;
 struct dAM_struct *stmt;
 INT branch;
 BOOL mark;
 } *AM_CURSOR_POS;

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

typedef struct AM_EXT1469221262_frame_struct {
 INT state;
 AM_EXT_CALL_EXPR self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_EXT1469221262_frame;

typedef struct AM_FAR_EXPR_struct {/* layout for AM_FAR_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *arg;
 struct SFILE_ID_struct source1;
 } *AM_FAR_EXPR;

typedef struct AM_FORK_STMT_struct {/* layout for AM_FORK_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *at;
 struct dAM_STMT_struct *body;
 struct dAM_STMT_struct *next;
 struct AM_PAR_STMT_struct *par_stmt;
 struct SFILE_ID_struct source1;
 } *AM_FORK_STMT;

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

typedef struct AM_ITE1159913222_frame_struct {
 INT state;
 AM_ITE1809135850 self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ITE1159913222_frame;

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

typedef struct AM_ROU2113525054_frame_struct {
 INT state;
 AM_ROU1916046290 self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ROU2113525054_frame;

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

typedef struct CS_OPTIONS_struct {/* layout for CS_OPTIONS */
 OB_HEADER header;
 struct ARRAYSTR_struct *arg_locations;
 struct ARRAYSTR_struct *args;
 struct CGEN_struct *cgen;
 struct CS_struct *cs;
 struct FMAPSTRSTR_struct *module_inclusion;
 struct FSETSTR_struct *arith_in;
 struct FSETSTR_struct *arith_out;
 struct FSETSTR_struct *assert_in;
 struct FSETSTR_struct *assert_out;
 struct FSETSTR_struct *bounds_in;
 struct FSETSTR_struct *bounds_out;
 struct FSETSTR_struct *classes1;
 struct FSETSTR_struct *invariant_in;
 struct FSETSTR_struct *invariant_out;
 struct FSETSTR_struct *paths;
 struct FSETSTR_struct *post_in;
 struct FSETSTR_struct *post_out;
 struct FSETSTR_struct *pre_in;
 struct FSETSTR_struct *pre_out;
 struct FSETSTR_struct *return_in;
 struct FSETSTR_struct *return_out;
 struct FSETSTR_struct *void_in;
 struct FSETSTR_struct *void_out;
 struct FSETSTR_struct *when_in;
 struct FSETSTR_struct *when_out;
 struct INLINE_struct *inliner;
 INT cache_size;
 INT cache_slot_size;
 INT local_call_acces;
 INT local_call_dynam;
 INT next;
 INT prefetch_weight;
 INT remote_call_acce;
 struct PARSE_struct *parser;
 struct PROG_struct *prog;
 BOOL all1;
 BOOL arith_all;
 BOOL assert_all;
 BOOL bounds_all;
 BOOL cache1;
 BOOL cse;
 BOOL cse_debug;
 BOOL debug_C_seen;
 BOOL debug_source_see;
 BOOL destroy_chk;
 BOOL got_libs;
 BOOL invariant_all;
 BOOL local_call;
 BOOL local_call_dynam1;
 BOOL local_opt;
 BOOL loop_poll;
 BOOL loop_prefetch;
 BOOL parloops;
 BOOL post_all;
 BOOL post_write;
 BOOL pre_all;
 BOOL prefetch;
 BOOL psather_chk;
 BOOL psather_stats;
 BOOL psather_trace;
 BOOL remote_call;
 BOOL remote_call_crea;
 BOOL return_all;
 BOOL side_debug;
 BOOL side_effects;
 BOOL specul_prefetch;
 BOOL stats;
 BOOL void_all;
 BOOL when_all;
 } *CS_OPTIONS;

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

typedef struct FLISTd739102917_frame_struct {
 INT state;
 FLISTdAM_STMT self;/* Formal argument: self */
 INT ret_val;
 INT L51;
 INT L61;
 BOOL f_L31_; /* used by builtin iter */
 INT L31_,L31_m; /* used by builtin iter */
 } *FLISTd739102917_frame;

typedef struct FLISTA886630705_struct {/* layout for FLIST{AM_CURSOR_POS} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_CURSOR_POS_struct *arr_part[1];
 } *FLISTA886630705;

typedef struct FLISTA1660160_frame_struct {
 INT state;
 FLISTA886630705 self;/* Formal argument: self */
 AM_CURSOR_POS ret_val1;
 INT i;
 INT sz;
 FLISTA886630705 aget_self;
 INT aget_ind;
 AM_CURSOR_POS ret_val;
 } *FLISTA1660160_frame;

typedef struct FLISTA1327600838_struct {/* layout for FLIST{ARRAY{$AM_EXPR}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct ARRAYdAM_EXPR_struct *arr_part[1];
 } *FLISTA1327600838;

typedef struct FLISTA1857087356_frame_struct {
 INT state;
 FLISTA1327600838 self;/* Formal argument: self */
 ARRAYdAM_EXPR ret_val1;
 INT i;
 INT sz;
 FLISTA1327600838 aget_self;
 INT aget_ind;
 ARRAYdAM_EXPR ret_val;
 } *FLISTA1857087356_frame;

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

extern BOOL AM_CURSOR_debug;

/* Function declarations */


extern RETURNED_CONST dAM_STMT (**dAM_ST775224435)(dAM_STMT);
AM_CURSOR AM_CUR1277440868(AM_CURSOR, PROG, dAM);
AM_CURSOR_POS FLISTA1660160(FLISTA1660160_frame);
AM_CURSOR_POS FLISTA860779230(FLISTA886630705);
AM_CURSOR_POS FLISTA971253650(FLISTA886630705);
ARRAYdAM_EXPR FLISTA1857087356(FLISTA1857087356_frame);
BOOL AM_CUR1683770295(AM_CURSOR, OB);
BOOL AM_CUR1885691330(AM_CURSOR, AM_CURSOR);
BOOL AM_CUR1972821118(AM_CURSOR, dAM);
FLISTA886630705 FLISTA1475379054(FLISTA886630705, AM_CURSOR_POS);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
INT AM_ARR1990213312(AM_ARR1990213312_frame);
INT AM_BND1037254841(AM_BND1037254841_frame);
INT AM_BND1640868266(AM_BND1640868266_frame);
INT AM_BND83643009(AM_BND83643009_frame);
INT AM_EXT1469221262(AM_EXT1469221262_frame);
INT AM_ITE1159913222(AM_ITE1159913222_frame);
INT AM_ROU2113525054(AM_ROU2113525054_frame);
INT FLISTA587993904(FLISTA886630705);
INT FLISTd1406591968(FLISTdAM_EXPR);
INT FLISTd355912233(FLISTdAM_STMT);
INT FLISTd739102917(FLISTd739102917_frame);
INT INT_do676118316(INT_do676118316_frame);
STR AM_CUR555854443(AM_CURSOR_POS);
STR AM_CUR720390882(AM_CURSOR);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap2004550586(STR, STR);
STR STR_fr1097270334(STR, FSTR);
STR STR_pl1270664985(STR, dSTR);
dAM AM_CUR52795524(AM_CURSOR);
dAM AM_CUR710387089(AM_CURSOR, dAM, INT);
void AM_CUR1869307717(AM_CURSOR);
void FILE_plus_STR(FILE1, STR);
void FILE_plus_dSTR(FILE1, dSTR);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

AM_CURSOR AM_CUR1277440868(AM_CURSOR self, PROG prog, dAM am) {
 AM_CURSOR ret_val;
 AM_CURSOR r;
 dGET_OPTIONS tmp1;
 A_STAC1244966098 create_self;
 A_STAC1244966098 ret_val1;
 A_STAC1244966098 res;
 FLISTA886630705 create_self1;
 FLISTA886630705 ret_val2;
 AM_CURSOR L1;
 OB L2;
 A_STAC1244966098 L3;
 OB L4;
 L2=ZALLOC(sizeof(struct AM_CURSOR_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_CURSOR_tag;
 L1 = ((AM_CURSOR) L2);
 r = L1;
 create_self = ((A_STAC1244966098) NULL);
 L4=ZALLOC(sizeof(struct A_STAC1244966098_struct));
 if (L4==NULL) FATAL("Unable to allocate more memory");
 ((OB)L4)->header.tag=A_STAC1244966098_tag;
 L3 = ((A_STAC1244966098) L4);
 res = L3;
 create_self1 = ((FLISTA886630705) NULL);
 ret_val2 = ((FLISTA886630705) NULL);
 SATTR(res,s,ret_val2);
 ret_val1 = res;
 SATTR(r,stack1,ret_val1);
 SATTR(r,cur,((dAM) NULL));
 SATTR(r,top1,am);
 SATTR(r,loops,0);
 SATTR(r,indent,0);
 SATTR(r,prog,prog);
 SATTR(r,started,FALSE);
 SATTR(r,calls_in_eval_or,TRUE);
 tmp1 = ATTR(prog,get_options);
 if (tmp1==NULL) {
  goto other222;
 } else
 switch (TAG(tmp1)) {
  case CS_OPTIONS_tag:
   SATTR(r,with_side_effect,ATTR(((CS_OPTIONS) tmp1),side_effects)); break;
  default: ;
  other222: ;
   SATTR(r,with_side_effect,FALSE);
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CUR1683770295(AM_CURSOR self, OB arg) {
 BOOL ret_val = BOOL_zero;
 if (arg==NULL) {
  goto other158;
 } else
 switch (TAG(arg)) {
  case AM_CURSOR_tag:
   ret_val = AM_CUR1885691330(self, ((AM_CURSOR) arg));
   return ret_val; break;
  default: ;
  other158: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CUR1885691330(AM_CURSOR self, AM_CURSOR c) {
 BOOL ret_val = BOOL_zero;
 A_STAC1244966098 L11;
 A_STAC1244966098 L21;
 OUT create_self;
 OUT ret_val1;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val2;
 FILE1 r;
 OUT create_self1;
 OUT ret_val3;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val4;
 FILE1 r1;
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
 dSTR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val8;
 FILE1 r3;
 OUT create_self4;
 OUT ret_val9;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val10;
 FILE1 r4;
 OUT create_self5;
 OUT ret_val11;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val12;
 FILE1 r5;
 OUT create_self6;
 OUT ret_val13;
 OUT plus_self6;
 dSTR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val14;
 FILE1 r6;
 OUT create_self7;
 OUT ret_val15;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val16;
 FILE1 r7;
 OUT create_self8;
 OUT ret_val17;
 OUT plus_self8;
 STR plus_s8;
 FILE1 stdout_self8;
 FILE1 ret_val18;
 FILE1 r8;
 OUT create_self9;
 OUT ret_val19;
 OUT plus_self9;
 STR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val20;
 FILE1 r9;
 OUT create_self10;
 OUT ret_val21;
 OUT plus_self10;
 dSTR plus_s10;
 FILE1 stdout_self10;
 FILE1 ret_val22;
 FILE1 r10;
 OUT create_self11;
 OUT ret_val23;
 OUT plus_self11;
 STR plus_s11;
 FILE1 stdout_self11;
 FILE1 ret_val24;
 FILE1 r11;
 OUT create_self12;
 OUT ret_val25;
 OUT plus_self12;
 STR plus_s12;
 OUT ret_val26;
 FILE1 stdout_self12;
 FILE1 ret_val27;
 FILE1 r12;
 OUT plus_self13;
 dSTR plus_s13;
 OUT ret_val28;
 FILE1 stdout_self13;
 FILE1 ret_val29;
 FILE1 r13;
 OUT plus_self14;
 STR plus_s14;
 FILE1 stdout_self14;
 FILE1 ret_val30;
 FILE1 r14;
 OUT create_self13;
 OUT ret_val31;
 OUT plus_self15;
 STR plus_s15;
 OUT ret_val32;
 FILE1 stdout_self15;
 FILE1 ret_val33;
 FILE1 r15;
 OUT plus_self16;
 dSTR plus_s16;
 OUT ret_val34;
 FILE1 stdout_self16;
 FILE1 ret_val35;
 FILE1 r16;
 OUT plus_self17;
 STR plus_s17;
 OUT ret_val36;
 FILE1 stdout_self17;
 FILE1 ret_val37;
 FILE1 r17;
 OUT plus_self18;
 dSTR plus_s18;
 OUT ret_val38;
 FILE1 stdout_self18;
 FILE1 ret_val39;
 FILE1 r18;
 OUT plus_self19;
 STR plus_s19;
 FILE1 stdout_self19;
 FILE1 ret_val40;
 FILE1 r19;
 OUT create_self14;
 OUT ret_val41;
 OUT plus_self20;
 STR plus_s20;
 OUT ret_val42;
 FILE1 stdout_self20;
 FILE1 ret_val43;
 FILE1 r20;
 OUT plus_self21;
 dSTR plus_s21;
 OUT ret_val44;
 FILE1 stdout_self21;
 FILE1 ret_val45;
 FILE1 r21;
 OUT plus_self22;
 STR plus_s22;
 OUT ret_val46;
 FILE1 stdout_self22;
 FILE1 ret_val47;
 FILE1 r22;
 OUT plus_self23;
 dSTR plus_s23;
 OUT ret_val48;
 FILE1 stdout_self23;
 FILE1 ret_val49;
 FILE1 r23;
 OUT plus_self24;
 STR plus_s24;
 FILE1 stdout_self24;
 FILE1 ret_val50;
 FILE1 r24;
 OUT create_self15;
 OUT ret_val51;
 OUT plus_self25;
 STR plus_s25;
 OUT ret_val52;
 FILE1 stdout_self25;
 FILE1 ret_val53;
 FILE1 r25;
 A_STAC1244966098 size_self;
 INT ret_val54 = INT_zero;
 OUT plus_self26;
 dSTR plus_s26;
 OUT ret_val55;
 FILE1 stdout_self26;
 FILE1 ret_val56;
 FILE1 r26;
 OUT plus_self27;
 STR plus_s27;
 OUT ret_val57;
 FILE1 stdout_self27;
 FILE1 ret_val58;
 FILE1 r27;
 A_STAC1244966098 size_self1;
 INT ret_val59 = INT_zero;
 OUT plus_self28;
 dSTR plus_s28;
 OUT ret_val60;
 FILE1 stdout_self28;
 FILE1 ret_val61;
 FILE1 r28;
 OUT plus_self29;
 STR plus_s29;
 FILE1 stdout_self29;
 FILE1 ret_val62;
 FILE1 r29;
 OUT create_self16;
 OUT ret_val63;
 OUT plus_self30;
 STR plus_s30;
 OUT ret_val64;
 FILE1 stdout_self30;
 FILE1 ret_val65;
 FILE1 r30;
 OUT plus_self31;
 STR plus_s31;
 FILE1 stdout_self31;
 FILE1 ret_val66;
 FILE1 r31;
 A_STAC1244966098 size_self2;
 INT ret_val67 = INT_zero;
 A_STAC1244966098 size_self3;
 INT ret_val68 = INT_zero;
 A_STAC1244966098 reverse_eltb_sel;
 AM_CURSOR_POS ret_val69;
 FLISTA886630705 L31;
 BOOL reverse_eltb_if_ = BOOL_zero;
 A_STAC1244966098 reverse_eltb_sel1;
 AM_CURSOR_POS ret_val70;
 FLISTA886630705 L41;
 BOOL reverse_eltb_if_1 = BOOL_zero;
 AM_CURSOR_POS is_eq_self;
 AM_CURSOR_POS is_eq_t;
 BOOL ret_val71 = BOOL_zero;
 extern STR compar1504846497;
 FILE1 L5;
 OB L6;
 extern STR top2;
 FILE1 L8;
 OB L9;
 extern STR void4;
 FILE1 L12;
 OB L13;
 dAM L15;
 INT L161_;
 dSTR L17;
 OB L18;
 FILE1 L19;
 OB L20;
 extern STR name41;
 FILE1 L23;
 OB L24;
 extern STR void4;
 FILE1 L26;
 OB L27;
 dAM L29;
 INT L301_;
 dSTR L32;
 OB L33;
 FILE1 L34;
 OB L35;
 extern STR name9;
 FILE1 L37;
 OB L38;
 extern STR cur2;
 FILE1 L40;
 OB L42;
 extern STR void4;
 FILE1 L44;
 OB L45;
 dAM L47;
 INT L481_;
 dSTR L49;
 OB L50;
 FILE1 L51;
 OB L52;
 extern STR void5;
 FILE1 L54;
 OB L55;
 extern STR name41;
 FILE1 L57;
 OB L58;
 dAM L60;
 INT L611_;
 dSTR L62;
 OB L63;
 FILE1 L64;
 OB L65;
 extern STR name9;
 FILE1 L67;
 OB L68;
 extern STR loops1;
 FILE1 L70;
 OB L72;
 dSTR L74;
 OB L75;
 FILE1 L76;
 OB L77;
 extern STR name41;
 FILE1 L79;
 OB L80;
 dSTR L82;
 OB L83;
 FILE1 L84;
 OB L85;
 extern STR name9;
 FILE1 L87;
 OB L88;
 extern STR indent1;
 FILE1 L90;
 OB L91;
 dSTR L93;
 OB L94;
 FILE1 L95;
 OB L96;
 extern STR name41;
 FILE1 L98;
 OB L99;
 dSTR L102;
 OB L103;
 FILE1 L104;
 OB L105;
 extern STR name9;
 FILE1 L107;
 OB L108;
 extern STR size1;
 FILE1 L110;
 OB L111;
 dSTR L113;
 OB L114;
 FILE1 L115;
 OB L116;
 extern STR name41;
 FILE1 L118;
 OB L119;
 dSTR L121;
 OB L122;
 FILE1 L123;
 OB L124;
 extern STR name9;
 FILE1 L126;
 OB L127;
 FILE1 L129;
 OB L130;
 FILE1 L132;
 OB L133;
 BOOL L135;
 BOOL L136;
 BOOL L137;
 BOOL L138;
 BOOL L1391_;
 BOOL L140;
 BOOL L1421_;
 BOOL L143;
 BOOL L144;
 BOOL L1451_;
 BOOL L146;
 BOOL L1471_;
 BOOL L148;
 BOOL L149;
 BOOL L1501_;
 BOOL L151;
 BOOL L1521_;
 BOOL L153;
 BOOL L154;
 BOOL L155;
 BOOL L156;
 BOOL L157;
 BOOL L1581_;
 BOOL L159;
 BOOL L1601_;
 INT L162;
 INT L163;
 BOOL L1641_;
 BOOL L165;
 BOOL L1661_;
 INT L167;
 INT L168;
 BOOL L1691_;
 BOOL L170;
 BOOL L1711_;
 INT L172;
 INT L173;
 BOOL L1741_;
 BOOL L175;
 BOOL L1761_;
 AM_CURSOR_POS L177;
 AM_CURSOR_POS L178;
 BOOL L179;
 BOOL L180;
 INT L181;
 INT L182;
 BOOL L1831_;
 BOOL L184;
 BOOL L185;
 BOOL L1861_;
 BOOL L187;
 BOOL L1881_;
 if (AM_CURSOR_debug) {
  create_self = ((OUT) NULL);
  ret_val1 = create_self;
  plus_self = ret_val1;
  plus_s = ((STR) &compar1504846497);
  stdout_self = ((FILE1) NULL);
  L6=ZALLOC(sizeof(struct FILE1_struct));
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FILE1_tag;
  L5 = ((FILE1) L6);
  r = L5;
  
  SATTR(r,fp,stdout);
  ret_val2 = r;
  FILE_plus_STR(ret_val2, plus_s);
  create_self1 = ((OUT) NULL);
  ret_val3 = create_self1;
  plus_self1 = ret_val3;
  plus_s1 = ((STR) &top2);
  stdout_self1 = ((FILE1) NULL);
  L9=ZALLOC(sizeof(struct FILE1_struct));
  if (L9==NULL) FATAL("Unable to allocate more memory");
  ((OB)L9)->header.tag=FILE1_tag;
  L8 = ((FILE1) L9);
  r1 = L8;
  
  SATTR(r1,fp,stdout);
  ret_val4 = r1;
  FILE_plus_STR(ret_val4, plus_s1);
  if ((ATTR(self,top1)==((dAM) NULL))) {
   create_self2 = ((OUT) NULL);
   ret_val5 = create_self2;
   plus_self2 = ret_val5;
   plus_s2 = ((STR) &void4);
   stdout_self2 = ((FILE1) NULL);
   L13=ZALLOC(sizeof(struct FILE1_struct));
   if (L13==NULL) FATAL("Unable to allocate more memory");
   ((OB)L13)->header.tag=FILE1_tag;
   L12 = ((FILE1) L13);
   r2 = L12;
   
   SATTR(r2,fp,stdout);
   ret_val6 = r2;
   FILE_plus_STR(ret_val6, plus_s2);
  }
  else {
   create_self3 = ((OUT) NULL);
   ret_val7 = create_self3;
   plus_self3 = ret_val7;
   L15 = ATTR(self,top1);
   L161_=SYSID(L15); 
   L18=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
   if (L18==NULL) FATAL("Unable to allocate more memory");
   memset(L18,0,sizeof(struct INT_boxed_struct));
   ((OB)L18)->header.tag=INT_tag;
   L17 = (dSTR)((INT_boxed) L18);
   ((INT_boxed) L17)->immutable_part = L161_;
   plus_s3 = L17;
   stdout_self3 = ((FILE1) NULL);
   L20=ZALLOC(sizeof(struct FILE1_struct));
   if (L20==NULL) FATAL("Unable to allocate more memory");
   ((OB)L20)->header.tag=FILE1_tag;
   L19 = ((FILE1) L20);
   r3 = L19;
   
   SATTR(r3,fp,stdout);
   ret_val8 = r3;
   FILE_plus_dSTR(ret_val8, plus_s3);
  }
  create_self4 = ((OUT) NULL);
  ret_val9 = create_self4;
  plus_self4 = ret_val9;
  plus_s4 = ((STR) &name41);
  stdout_self4 = ((FILE1) NULL);
  L24=ZALLOC(sizeof(struct FILE1_struct));
  if (L24==NULL) FATAL("Unable to allocate more memory");
  ((OB)L24)->header.tag=FILE1_tag;
  L23 = ((FILE1) L24);
  r4 = L23;
  
  SATTR(r4,fp,stdout);
  ret_val10 = r4;
  FILE_plus_STR(ret_val10, plus_s4);
  if ((ATTR(c,top1)==((dAM) NULL))) {
   create_self5 = ((OUT) NULL);
   ret_val11 = create_self5;
   plus_self5 = ret_val11;
   plus_s5 = ((STR) &void4);
   stdout_self5 = ((FILE1) NULL);
   L27=ZALLOC(sizeof(struct FILE1_struct));
   if (L27==NULL) FATAL("Unable to allocate more memory");
   ((OB)L27)->header.tag=FILE1_tag;
   L26 = ((FILE1) L27);
   r5 = L26;
   
   SATTR(r5,fp,stdout);
   ret_val12 = r5;
   FILE_plus_STR(ret_val12, plus_s5);
  }
  else {
   create_self6 = ((OUT) NULL);
   ret_val13 = create_self6;
   plus_self6 = ret_val13;
   L29 = ATTR(c,top1);
   L301_=SYSID(L29); 
   L33=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
   if (L33==NULL) FATAL("Unable to allocate more memory");
   memset(L33,0,sizeof(struct INT_boxed_struct));
   ((OB)L33)->header.tag=INT_tag;
   L32 = (dSTR)((INT_boxed) L33);
   ((INT_boxed) L32)->immutable_part = L301_;
   plus_s6 = L32;
   stdout_self6 = ((FILE1) NULL);
   L35=ZALLOC(sizeof(struct FILE1_struct));
   if (L35==NULL) FATAL("Unable to allocate more memory");
   ((OB)L35)->header.tag=FILE1_tag;
   L34 = ((FILE1) L35);
   r6 = L34;
   
   SATTR(r6,fp,stdout);
   ret_val14 = r6;
   FILE_plus_dSTR(ret_val14, plus_s6);
  }
  create_self7 = ((OUT) NULL);
  ret_val15 = create_self7;
  plus_self7 = ret_val15;
  plus_s7 = ((STR) &name9);
  stdout_self7 = ((FILE1) NULL);
  L38=ZALLOC(sizeof(struct FILE1_struct));
  if (L38==NULL) FATAL("Unable to allocate more memory");
  ((OB)L38)->header.tag=FILE1_tag;
  L37 = ((FILE1) L38);
  r7 = L37;
  
  SATTR(r7,fp,stdout);
  ret_val16 = r7;
  FILE_plus_STR(ret_val16, plus_s7);
  create_self8 = ((OUT) NULL);
  ret_val17 = create_self8;
  plus_self8 = ret_val17;
  plus_s8 = ((STR) &cur2);
  stdout_self8 = ((FILE1) NULL);
  L42=ZALLOC(sizeof(struct FILE1_struct));
  if (L42==NULL) FATAL("Unable to allocate more memory");
  ((OB)L42)->header.tag=FILE1_tag;
  L40 = ((FILE1) L42);
  r8 = L40;
  
  SATTR(r8,fp,stdout);
  ret_val18 = r8;
  FILE_plus_STR(ret_val18, plus_s8);
  if ((ATTR(self,cur)==((dAM) NULL))) {
   create_self9 = ((OUT) NULL);
   ret_val19 = create_self9;
   plus_self9 = ret_val19;
   plus_s9 = ((STR) &void4);
   stdout_self9 = ((FILE1) NULL);
   L45=ZALLOC(sizeof(struct FILE1_struct));
   if (L45==NULL) FATAL("Unable to allocate more memory");
   ((OB)L45)->header.tag=FILE1_tag;
   L44 = ((FILE1) L45);
   r9 = L44;
   
   SATTR(r9,fp,stdout);
   ret_val20 = r9;
   FILE_plus_STR(ret_val20, plus_s9);
  }
  else {
   create_self10 = ((OUT) NULL);
   ret_val21 = create_self10;
   plus_self10 = ret_val21;
   L47 = ATTR(self,cur);
   L481_=SYSID(L47); 
   L50=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
   if (L50==NULL) FATAL("Unable to allocate more memory");
   memset(L50,0,sizeof(struct INT_boxed_struct));
   ((OB)L50)->header.tag=INT_tag;
   L49 = (dSTR)((INT_boxed) L50);
   ((INT_boxed) L49)->immutable_part = L481_;
   plus_s10 = L49;
   stdout_self10 = ((FILE1) NULL);
   L52=ZALLOC(sizeof(struct FILE1_struct));
   if (L52==NULL) FATAL("Unable to allocate more memory");
   ((OB)L52)->header.tag=FILE1_tag;
   L51 = ((FILE1) L52);
   r10 = L51;
   
   SATTR(r10,fp,stdout);
   ret_val22 = r10;
   FILE_plus_dSTR(ret_val22, plus_s10);
  }
  if ((ATTR(c,cur)==((dAM) NULL))) {
   create_self11 = ((OUT) NULL);
   ret_val23 = create_self11;
   plus_self11 = ret_val23;
   plus_s11 = ((STR) &void5);
   stdout_self11 = ((FILE1) NULL);
   L55=ZALLOC(sizeof(struct FILE1_struct));
   if (L55==NULL) FATAL("Unable to allocate more memory");
   ((OB)L55)->header.tag=FILE1_tag;
   L54 = ((FILE1) L55);
   r11 = L54;
   
   SATTR(r11,fp,stdout);
   ret_val24 = r11;
   FILE_plus_STR(ret_val24, plus_s11);
  }
  else {
   create_self12 = ((OUT) NULL);
   ret_val25 = create_self12;
   plus_self12 = ret_val25;
   plus_s12 = ((STR) &name41);
   stdout_self12 = ((FILE1) NULL);
   L58=ZALLOC(sizeof(struct FILE1_struct));
   if (L58==NULL) FATAL("Unable to allocate more memory");
   ((OB)L58)->header.tag=FILE1_tag;
   L57 = ((FILE1) L58);
   r12 = L57;
   
   SATTR(r12,fp,stdout);
   ret_val27 = r12;
   FILE_plus_STR(ret_val27, plus_s12);
   ret_val26 = plus_self12;
   plus_self13 = ret_val26;
   L60 = ATTR(c,cur);
   L611_=SYSID(L60); 
   L63=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
   if (L63==NULL) FATAL("Unable to allocate more memory");
   memset(L63,0,sizeof(struct INT_boxed_struct));
   ((OB)L63)->header.tag=INT_tag;
   L62 = (dSTR)((INT_boxed) L63);
   ((INT_boxed) L62)->immutable_part = L611_;
   plus_s13 = L62;
   stdout_self13 = ((FILE1) NULL);
   L65=ZALLOC(sizeof(struct FILE1_struct));
   if (L65==NULL) FATAL("Unable to allocate more memory");
   ((OB)L65)->header.tag=FILE1_tag;
   L64 = ((FILE1) L65);
   r13 = L64;
   
   SATTR(r13,fp,stdout);
   ret_val29 = r13;
   FILE_plus_dSTR(ret_val29, plus_s13);
   ret_val28 = plus_self13;
   plus_self14 = ret_val28;
   plus_s14 = ((STR) &name9);
   stdout_self14 = ((FILE1) NULL);
   L68=ZALLOC(sizeof(struct FILE1_struct));
   if (L68==NULL) FATAL("Unable to allocate more memory");
   ((OB)L68)->header.tag=FILE1_tag;
   L67 = ((FILE1) L68);
   r14 = L67;
   
   SATTR(r14,fp,stdout);
   ret_val30 = r14;
   FILE_plus_STR(ret_val30, plus_s14);
  }
  create_self13 = ((OUT) NULL);
  ret_val31 = create_self13;
  plus_self15 = ret_val31;
  plus_s15 = ((STR) &loops1);
  stdout_self15 = ((FILE1) NULL);
  L72=ZALLOC(sizeof(struct FILE1_struct));
  if (L72==NULL) FATAL("Unable to allocate more memory");
  ((OB)L72)->header.tag=FILE1_tag;
  L70 = ((FILE1) L72);
  r15 = L70;
  
  SATTR(r15,fp,stdout);
  ret_val33 = r15;
  FILE_plus_STR(ret_val33, plus_s15);
  ret_val32 = plus_self15;
  plus_self16 = ret_val32;
  L75=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L75==NULL) FATAL("Unable to allocate more memory");
  memset(L75,0,sizeof(struct INT_boxed_struct));
  ((OB)L75)->header.tag=INT_tag;
  L74 = (dSTR)((INT_boxed) L75);
  ((INT_boxed) L74)->immutable_part = ATTR(self,loops);
  plus_s16 = L74;
  stdout_self16 = ((FILE1) NULL);
  L77=ZALLOC(sizeof(struct FILE1_struct));
  if (L77==NULL) FATAL("Unable to allocate more memory");
  ((OB)L77)->header.tag=FILE1_tag;
  L76 = ((FILE1) L77);
  r16 = L76;
  
  SATTR(r16,fp,stdout);
  ret_val35 = r16;
  FILE_plus_dSTR(ret_val35, plus_s16);
  ret_val34 = plus_self16;
  plus_self17 = ret_val34;
  plus_s17 = ((STR) &name41);
  stdout_self17 = ((FILE1) NULL);
  L80=ZALLOC(sizeof(struct FILE1_struct));
  if (L80==NULL) FATAL("Unable to allocate more memory");
  ((OB)L80)->header.tag=FILE1_tag;
  L79 = ((FILE1) L80);
  r17 = L79;
  
  SATTR(r17,fp,stdout);
  ret_val37 = r17;
  FILE_plus_STR(ret_val37, plus_s17);
  ret_val36 = plus_self17;
  plus_self18 = ret_val36;
  L83=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L83==NULL) FATAL("Unable to allocate more memory");
  memset(L83,0,sizeof(struct INT_boxed_struct));
  ((OB)L83)->header.tag=INT_tag;
  L82 = (dSTR)((INT_boxed) L83);
  ((INT_boxed) L82)->immutable_part = ATTR(c,loops);
  plus_s18 = L82;
  stdout_self18 = ((FILE1) NULL);
  L85=ZALLOC(sizeof(struct FILE1_struct));
  if (L85==NULL) FATAL("Unable to allocate more memory");
  ((OB)L85)->header.tag=FILE1_tag;
  L84 = ((FILE1) L85);
  r18 = L84;
  
  SATTR(r18,fp,stdout);
  ret_val39 = r18;
  FILE_plus_dSTR(ret_val39, plus_s18);
  ret_val38 = plus_self18;
  plus_self19 = ret_val38;
  plus_s19 = ((STR) &name9);
  stdout_self19 = ((FILE1) NULL);
  L88=ZALLOC(sizeof(struct FILE1_struct));
  if (L88==NULL) FATAL("Unable to allocate more memory");
  ((OB)L88)->header.tag=FILE1_tag;
  L87 = ((FILE1) L88);
  r19 = L87;
  
  SATTR(r19,fp,stdout);
  ret_val40 = r19;
  FILE_plus_STR(ret_val40, plus_s19);
  create_self14 = ((OUT) NULL);
  ret_val41 = create_self14;
  plus_self20 = ret_val41;
  plus_s20 = ((STR) &indent1);
  stdout_self20 = ((FILE1) NULL);
  L91=ZALLOC(sizeof(struct FILE1_struct));
  if (L91==NULL) FATAL("Unable to allocate more memory");
  ((OB)L91)->header.tag=FILE1_tag;
  L90 = ((FILE1) L91);
  r20 = L90;
  
  SATTR(r20,fp,stdout);
  ret_val43 = r20;
  FILE_plus_STR(ret_val43, plus_s20);
  ret_val42 = plus_self20;
  plus_self21 = ret_val42;
  L94=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L94==NULL) FATAL("Unable to allocate more memory");
  memset(L94,0,sizeof(struct INT_boxed_struct));
  ((OB)L94)->header.tag=INT_tag;
  L93 = (dSTR)((INT_boxed) L94);
  ((INT_boxed) L93)->immutable_part = ATTR(self,indent);
  plus_s21 = L93;
  stdout_self21 = ((FILE1) NULL);
  L96=ZALLOC(sizeof(struct FILE1_struct));
  if (L96==NULL) FATAL("Unable to allocate more memory");
  ((OB)L96)->header.tag=FILE1_tag;
  L95 = ((FILE1) L96);
  r21 = L95;
  
  SATTR(r21,fp,stdout);
  ret_val45 = r21;
  FILE_plus_dSTR(ret_val45, plus_s21);
  ret_val44 = plus_self21;
  plus_self22 = ret_val44;
  plus_s22 = ((STR) &name41);
  stdout_self22 = ((FILE1) NULL);
  L99=ZALLOC(sizeof(struct FILE1_struct));
  if (L99==NULL) FATAL("Unable to allocate more memory");
  ((OB)L99)->header.tag=FILE1_tag;
  L98 = ((FILE1) L99);
  r22 = L98;
  
  SATTR(r22,fp,stdout);
  ret_val47 = r22;
  FILE_plus_STR(ret_val47, plus_s22);
  ret_val46 = plus_self22;
  plus_self23 = ret_val46;
  L103=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L103==NULL) FATAL("Unable to allocate more memory");
  memset(L103,0,sizeof(struct INT_boxed_struct));
  ((OB)L103)->header.tag=INT_tag;
  L102 = (dSTR)((INT_boxed) L103);
  ((INT_boxed) L102)->immutable_part = ATTR(c,indent);
  plus_s23 = L102;
  stdout_self23 = ((FILE1) NULL);
  L105=ZALLOC(sizeof(struct FILE1_struct));
  if (L105==NULL) FATAL("Unable to allocate more memory");
  ((OB)L105)->header.tag=FILE1_tag;
  L104 = ((FILE1) L105);
  r23 = L104;
  
  SATTR(r23,fp,stdout);
  ret_val49 = r23;
  FILE_plus_dSTR(ret_val49, plus_s23);
  ret_val48 = plus_self23;
  plus_self24 = ret_val48;
  plus_s24 = ((STR) &name9);
  stdout_self24 = ((FILE1) NULL);
  L108=ZALLOC(sizeof(struct FILE1_struct));
  if (L108==NULL) FATAL("Unable to allocate more memory");
  ((OB)L108)->header.tag=FILE1_tag;
  L107 = ((FILE1) L108);
  r24 = L107;
  
  SATTR(r24,fp,stdout);
  ret_val50 = r24;
  FILE_plus_STR(ret_val50, plus_s24);
  create_self15 = ((OUT) NULL);
  ret_val51 = create_self15;
  plus_self25 = ret_val51;
  plus_s25 = ((STR) &size1);
  stdout_self25 = ((FILE1) NULL);
  L111=ZALLOC(sizeof(struct FILE1_struct));
  if (L111==NULL) FATAL("Unable to allocate more memory");
  ((OB)L111)->header.tag=FILE1_tag;
  L110 = ((FILE1) L111);
  r25 = L110;
  
  SATTR(r25,fp,stdout);
  ret_val53 = r25;
  FILE_plus_STR(ret_val53, plus_s25);
  ret_val52 = plus_self25;
  plus_self26 = ret_val52;
  size_self = ATTR(self,stack1);
  ret_val54 = FLISTA587993904(ATTR(size_self,s));
  L114=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L114==NULL) FATAL("Unable to allocate more memory");
  memset(L114,0,sizeof(struct INT_boxed_struct));
  ((OB)L114)->header.tag=INT_tag;
  L113 = (dSTR)((INT_boxed) L114);
  ((INT_boxed) L113)->immutable_part = ret_val54;
  plus_s26 = L113;
  stdout_self26 = ((FILE1) NULL);
  L116=ZALLOC(sizeof(struct FILE1_struct));
  if (L116==NULL) FATAL("Unable to allocate more memory");
  ((OB)L116)->header.tag=FILE1_tag;
  L115 = ((FILE1) L116);
  r26 = L115;
  
  SATTR(r26,fp,stdout);
  ret_val56 = r26;
  FILE_plus_dSTR(ret_val56, plus_s26);
  ret_val55 = plus_self26;
  plus_self27 = ret_val55;
  plus_s27 = ((STR) &name41);
  stdout_self27 = ((FILE1) NULL);
  L119=ZALLOC(sizeof(struct FILE1_struct));
  if (L119==NULL) FATAL("Unable to allocate more memory");
  ((OB)L119)->header.tag=FILE1_tag;
  L118 = ((FILE1) L119);
  r27 = L118;
  
  SATTR(r27,fp,stdout);
  ret_val58 = r27;
  FILE_plus_STR(ret_val58, plus_s27);
  ret_val57 = plus_self27;
  plus_self28 = ret_val57;
  size_self1 = ATTR(c,stack1);
  ret_val59 = FLISTA587993904(ATTR(size_self1,s));
  L122=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L122==NULL) FATAL("Unable to allocate more memory");
  memset(L122,0,sizeof(struct INT_boxed_struct));
  ((OB)L122)->header.tag=INT_tag;
  L121 = (dSTR)((INT_boxed) L122);
  ((INT_boxed) L121)->immutable_part = ret_val59;
  plus_s28 = L121;
  stdout_self28 = ((FILE1) NULL);
  L124=ZALLOC(sizeof(struct FILE1_struct));
  if (L124==NULL) FATAL("Unable to allocate more memory");
  ((OB)L124)->header.tag=FILE1_tag;
  L123 = ((FILE1) L124);
  r28 = L123;
  
  SATTR(r28,fp,stdout);
  ret_val61 = r28;
  FILE_plus_dSTR(ret_val61, plus_s28);
  ret_val60 = plus_self28;
  plus_self29 = ret_val60;
  plus_s29 = ((STR) &name9);
  stdout_self29 = ((FILE1) NULL);
  L127=ZALLOC(sizeof(struct FILE1_struct));
  if (L127==NULL) FATAL("Unable to allocate more memory");
  ((OB)L127)->header.tag=FILE1_tag;
  L126 = ((FILE1) L127);
  r29 = L126;
  
  SATTR(r29,fp,stdout);
  ret_val62 = r29;
  FILE_plus_STR(ret_val62, plus_s29);
  create_self16 = ((OUT) NULL);
  ret_val63 = create_self16;
  plus_self30 = ret_val63;
  plus_s30 = AM_CUR720390882(self);
  stdout_self30 = ((FILE1) NULL);
  L130=ZALLOC(sizeof(struct FILE1_struct));
  if (L130==NULL) FATAL("Unable to allocate more memory");
  ((OB)L130)->header.tag=FILE1_tag;
  L129 = ((FILE1) L130);
  r30 = L129;
  
  SATTR(r30,fp,stdout);
  ret_val65 = r30;
  FILE_plus_STR(ret_val65, plus_s30);
  ret_val64 = plus_self30;
  plus_self31 = ret_val64;
  plus_s31 = AM_CUR720390882(c);
  stdout_self31 = ((FILE1) NULL);
  L133=ZALLOC(sizeof(struct FILE1_struct));
  if (L133==NULL) FATAL("Unable to allocate more memory");
  ((OB)L133)->header.tag=FILE1_tag;
  L132 = ((FILE1) L133);
  r31 = L132;
  
  SATTR(r31,fp,stdout);
  ret_val66 = r31;
  FILE_plus_STR(ret_val66, plus_s31);
 }
 if ((self==((AM_CURSOR) NULL))) {
  ret_val = (c==((AM_CURSOR) NULL));
  return ret_val;
 }
 if ((c==((AM_CURSOR) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 L137 = ATTR(self,ignore_pre);
 L138 = ATTR(c,ignore_pre);
 L1391_=L137==L138; 
 L140 = L1391_;
 L1421_=!(L140); 
 if (L1421_) {
  L136 = TRUE;
 } else {
  L143 = ATTR(self,ignore_post);
  L144 = ATTR(c,ignore_post);
  L1451_=L143==L144; 
  L146 = L1451_;
  L1471_=!(L146); 
  L136 = L1471_;
 }
 if (L136) {
  L135 = TRUE;
 } else {
  L148 = ATTR(self,ignore_assert);
  L149 = ATTR(c,ignore_assert);
  L1501_=L148==L149; 
  L151 = L1501_;
  L1521_=!(L151); 
  L135 = L1521_;
 }
 if (L135) {
  ret_val = FALSE;
  return ret_val;
 }
 L157 = SYSOBEQ(((OB) ATTR(self,cur)),((OB) ATTR(c,cur)));
 L1581_=!(L157); 
 if (L1581_) {
  L156 = TRUE;
 } else {
  L159 = SYSOBEQ(((OB) ATTR(self,top1)),((OB) ATTR(c,top1)));
  L1601_=!(L159); 
  L156 = L1601_;
 }
 if (L156) {
  L155 = TRUE;
 } else {
  L162 = ATTR(self,loops);
  L163 = ATTR(c,loops);
  L1641_=(L162)==(L163); 
  L165 = L1641_;
  L1661_=!(L165); 
  L155 = L1661_;
 }
 if (L155) {
  L154 = TRUE;
 } else {
  L167 = ATTR(self,indent);
  L168 = ATTR(c,indent);
  L1691_=(L167)==(L168); 
  L170 = L1691_;
  L1711_=!(L170); 
  L154 = L1711_;
 }
 if (L154) {
  L153 = TRUE;
 } else {
  size_self2 = ATTR(self,stack1);
  ret_val67 = FLISTA587993904(ATTR(size_self2,s));
  L172 = ret_val67;
  size_self3 = ATTR(c,stack1);
  ret_val68 = FLISTA587993904(ATTR(size_self3,s));
  L173 = ret_val68;
  L1741_=(L172)==(L173); 
  L175 = L1741_;
  L1761_=!(L175); 
  L153 = L1761_;
 }
 if (L153) {
  ret_val = FALSE;
  return ret_val;
 }
 {
  struct FLISTA1660160_frame_struct other1_0;
FLISTA1660160_frame noname1 = &other1_0;
  struct FLISTA1660160_frame_struct other1_1;
FLISTA1660160_frame noname2 = &other1_1;
  /* Initialization of inlined iter A_STACK{AM_CURSOR_POS}::reverse_elt!:AM_CURSOR_POS */
  L11 = ATTR(self,stack1);
  reverse_eltb_sel = L11;
  L31 = ATTRs(reverse_eltb_sel,s,((FLISTA886630705) NULL));
  /* Initialization of inlined iter A_STACK{AM_CURSOR_POS}::reverse_elt!:AM_CURSOR_POS */
  L21 = ATTRs(c,stack1,((A_STAC1244966098) NULL));
  reverse_eltb_sel1 = L21;
  L41 = ATTRs(reverse_eltb_sel1,s,((FLISTA886630705) NULL));
  noname1->self = L31;
  noname1->state = 0;
  noname2->self = L41;
  noname2->state = 0;
  reverse_eltb_if_ = TRUE;
  reverse_eltb_if_1 = TRUE;
  while (1) {
   L177 = FLISTA1660160(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   is_eq_self = L177;
   L178 = FLISTA1660160(noname2);
   if (noname2->state == -1) {
    goto after_loop;
   }
   is_eq_t = L178;
   if (SYSOBEQ(((OB) ATTR(is_eq_self,stmt)),((OB) ATTR(is_eq_t,stmt)))) {
    L181 = ATTR(is_eq_self,branch);
    L182 = ATTR(is_eq_t,branch);
    L1831_=(L181)==(L182); 
    L180 = L1831_;
   } else {
    L180 = FALSE;
   }
   if (L180) {
    L184 = ATTR(is_eq_self,mark);
    L185 = ATTR(is_eq_t,mark);
    L1861_=L184==L185; 
    L179 = L1861_;
   } else {
    L179 = FALSE;
   }
   ret_val71 = L179;
   L187 = ret_val71;
   L1881_=!(L187); 
   if (L1881_) {
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

BOOL AM_CUR1972821118(AM_CURSOR self, dAM am) {
 BOOL ret_val = BOOL_zero;
 if (ATTR(self,all_in_pre_order)) {
  ret_val = FALSE;
  return ret_val;
 }
 if (ATTR(self,all_in_eval_orde)) {
  ret_val = TRUE;
  return ret_val;
 }
 if (am==NULL) {
  goto other223;
 } else
 switch (TAG(am)) {
  case AM_BND367211769_tag:
  case AM_BND260301329_tag:
  case AM_EXT_CALL_EXPR_tag:
  case AM_ITE1809135850_tag:
  case AM_ROU1916046290_tag:
   ret_val = ATTR(self,calls_in_eval_or);
   return ret_val; break;
  case AM_ASSIGN_STMT_tag:
   ret_val = ATTR(self,assign_in_order);
   return ret_val; break;
  case AM_WAITFOR_STMT_tag:
   ret_val = ATTR(self,assign_in_order);
   return ret_val; break;
  case AM_PREFETCH_STMT_tag:
   ret_val = ATTR(self,assign_in_order);
   return ret_val; break;
  default: ;
  other223: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR AM_CUR720390882(AM_CURSOR self) {
 STR ret_val;
 STR ret;
 A_STAC1244966098 L11;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 INT plus_arg = INT_zero;
 STR ret_val2;
 STR s;
 INT str_self = INT_zero;
 STR ret_val3;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val4;
 FSTR str_self1;
 STR ret_val5;
 STR plus_self2;
 STR plus_sarg1;
 STR ret_val6;
 STR plus_self3;
 STR plus_sarg2;
 STR ret_val7;
 STR plus_self4;
 STR plus_sarg3;
 STR ret_val8;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val9;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val10;
 STR plus_self7;
 INT plus_arg1 = INT_zero;
 STR ret_val11;
 STR s1;
 INT str_self2 = INT_zero;
 STR ret_val12;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val13;
 FSTR str_self3;
 STR ret_val14;
 STR plus_self8;
 STR plus_sarg6;
 STR ret_val15;
 STR plus_self9;
 STR plus_sarg7;
 STR ret_val16;
 STR plus_self10;
 STR plus_sarg8;
 STR ret_val17;
 STR plus_self11;
 STR plus_sarg9;
 STR ret_val18;
 STR plus_self12;
 STR plus_sarg10;
 STR ret_val19;
 STR plus_self13;
 INT plus_arg2 = INT_zero;
 STR ret_val20;
 STR s2;
 INT str_self4 = INT_zero;
 STR ret_val21;
 FSTR buf3 = ((FSTR) NULL);
 FSTR clear_self2;
 INT str_in_self2 = INT_zero;
 FSTR str_in_s2;
 FSTR ret_val22;
 FSTR str_self5;
 STR ret_val23;
 STR plus_self14;
 STR plus_sarg11;
 STR ret_val24;
 STR plus_self15;
 STR plus_sarg12;
 STR ret_val25;
 STR plus_self16;
 STR plus_sarg13;
 STR ret_val26;
 STR plus_self17;
 STR plus_sarg14;
 STR ret_val27;
 STR plus_self18;
 STR plus_sarg15;
 STR ret_val28;
 STR plus_self19;
 STR plus_sarg16;
 STR ret_val29;
 STR plus_self20;
 STR plus_sarg17;
 STR ret_val30;
 STR plus_self21;
 STR plus_sarg18;
 STR ret_val31;
 STR plus_self22;
 INT plus_arg3 = INT_zero;
 STR ret_val32;
 STR s3;
 INT str_self6 = INT_zero;
 STR ret_val33;
 FSTR buf4 = ((FSTR) NULL);
 FSTR clear_self3;
 INT str_in_self3 = INT_zero;
 FSTR str_in_s3;
 FSTR ret_val34;
 FSTR str_self7;
 STR ret_val35;
 STR plus_self23;
 STR plus_sarg19;
 STR ret_val36;
 STR plus_self24;
 INT plus_arg4 = INT_zero;
 STR ret_val37;
 STR s4;
 INT str_self8 = INT_zero;
 STR ret_val38;
 FSTR buf5 = ((FSTR) NULL);
 FSTR clear_self4;
 INT str_in_self4 = INT_zero;
 FSTR str_in_s4;
 FSTR ret_val39;
 FSTR str_self9;
 STR ret_val40;
 STR plus_self25;
 STR plus_sarg20;
 STR ret_val41;
 STR plus_self26;
 STR plus_sarg21;
 STR ret_val42;
 A_STAC1244966098 eltb_self;
 AM_CURSOR_POS ret_val43;
 INT L21 = INT_zero;
 INT L31 = INT_zero;
 BOOL eltb_if_first = BOOL_zero;
 FLISTA886630705 aget_self;
 INT aget_ind = INT_zero;
 AM_CURSOR_POS ret_val44;
 STR plus_self27;
 STR plus_sarg22;
 STR ret_val45;
 STR plus_self28;
 STR plus_sarg23;
 STR ret_val46;
 STR plus_self29;
 STR plus_sarg24;
 STR ret_val47;
 extern STR name1;
 extern STR AM_CURSOR1;
 INT L41_;
 BOOL L5;
 BOOL L61_;
 extern STR name38;
 extern STR curVOID;
 extern STR cur1;
 dAM L7;
 INT L81_;
 INT L9;
 STR L101_;
 extern STR name14;
 dAM L12;
 INT L131_;
 BOOL L14;
 BOOL L151_;
 extern STR name39;
 extern STR topVOID;
 extern STR top1;
 dAM L16;
 INT L171_;
 INT L18;
 STR L191_;
 extern STR name14;
 dAM L20;
 INT L221_;
 BOOL L23;
 BOOL L241_;
 extern STR name7;
 extern STR all_in_pre_order;
 dSTR L25;
 OB L26;
 extern STR all_in1635012407;
 dSTR L27;
 OB L28;
 extern STR name9;
 extern STR assign_in_order;
 dSTR L29;
 OB L30;
 extern STR calls_894534736;
 dSTR L32;
 OB L33;
 extern STR name9;
 extern STR loops;
 BOOL L34;
 BOOL L351_;
 extern STR indent;
 BOOL L36;
 BOOL L371_;
 extern STR name40;
 extern STR name8;
 INT L39;
 INT L40;
 INT L421_;
 INT L381_,L381_m;
 AM_CURSOR_POS L431_;
 extern STR name9;
 extern STR name32;
 ret = ((STR) &name1);
 plus_self = ret;
 plus_sarg = ((STR) &AM_CURSOR1);
 ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
 plus_self1 = ret_val1;
 L41_=SYSID(self); 
 plus_arg = L41_;
 str_self = plus_arg;
 clear_self = buf1;
 L5 = (clear_self==((FSTR) NULL));
 L61_=!(L5); 
 if (L61_) {
  SATTR(clear_self,loc,0);
 }
 str_in_self = str_self;
 str_in_s = buf1;
 ret_val4 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
 buf1 = ret_val4;
 str_self1 = buf1;
 ret_val5 = STR_fr1097270334(((STR) NULL), str_self1);
 ret_val3 = ret_val5;
 s = ret_val3;
 ret_val2 = STR_ap1077157958(plus_self1, s, TRUE);
 plus_self2 = ret_val2;
 plus_sarg1 = ((STR) &name38);
 ret_val6 = STR_ap2004550586(plus_self2, plus_sarg1);
 ret = ret_val6;
 if ((ATTR(self,cur)==((dAM) NULL))) {
  plus_self3 = ret;
  plus_sarg2 = ((STR) &curVOID);
  ret_val7 = STR_ap2004550586(plus_self3, plus_sarg2);
  ret = ret_val7;
 }
 else {
  plus_self4 = ret;
  plus_sarg3 = ((STR) &cur1);
  ret_val8 = STR_ap2004550586(plus_self4, plus_sarg3);
  plus_self5 = ret_val8;
  L7 = ATTR(self,cur);
  L81_=SYSTP(L7); 
  L9 = L81_;
  L101_=SYSSTRFORTP(L9); 
  plus_sarg4 = L101_;
  ret_val9 = STR_ap2004550586(plus_self5, plus_sarg4);
  plus_self6 = ret_val9;
  plus_sarg5 = ((STR) &name14);
  ret_val10 = STR_ap2004550586(plus_self6, plus_sarg5);
  plus_self7 = ret_val10;
  L12 = ATTR(self,cur);
  L131_=SYSID(L12); 
  plus_arg1 = L131_;
  str_self2 = plus_arg1;
  clear_self1 = buf2;
  L14 = (clear_self1==((FSTR) NULL));
  L151_=!(L14); 
  if (L151_) {
   SATTR(clear_self1,loc,0);
  }
  str_in_self1 = str_self2;
  str_in_s1 = buf2;
  ret_val13 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
  buf2 = ret_val13;
  str_self3 = buf2;
  ret_val14 = STR_fr1097270334(((STR) NULL), str_self3);
  ret_val12 = ret_val14;
  s1 = ret_val12;
  ret_val11 = STR_ap1077157958(plus_self7, s1, TRUE);
  plus_self8 = ret_val11;
  plus_sarg6 = ((STR) &name39);
  ret_val15 = STR_ap2004550586(plus_self8, plus_sarg6);
  ret = ret_val15;
 }
 if ((ATTR(self,top1)==((dAM) NULL))) {
  plus_self9 = ret;
  plus_sarg7 = ((STR) &topVOID);
  ret_val16 = STR_ap2004550586(plus_self9, plus_sarg7);
  ret = ret_val16;
 }
 else {
  plus_self10 = ret;
  plus_sarg8 = ((STR) &top1);
  ret_val17 = STR_ap2004550586(plus_self10, plus_sarg8);
  plus_self11 = ret_val17;
  L16 = ATTR(self,top1);
  L171_=SYSTP(L16); 
  L18 = L171_;
  L191_=SYSSTRFORTP(L18); 
  plus_sarg9 = L191_;
  ret_val18 = STR_ap2004550586(plus_self11, plus_sarg9);
  plus_self12 = ret_val18;
  plus_sarg10 = ((STR) &name14);
  ret_val19 = STR_ap2004550586(plus_self12, plus_sarg10);
  plus_self13 = ret_val19;
  L20 = ATTR(self,top1);
  L221_=SYSID(L20); 
  plus_arg2 = L221_;
  str_self4 = plus_arg2;
  clear_self2 = buf3;
  L23 = (clear_self2==((FSTR) NULL));
  L241_=!(L23); 
  if (L241_) {
   SATTR(clear_self2,loc,0);
  }
  str_in_self2 = str_self4;
  str_in_s2 = buf3;
  ret_val22 = INT_st367594495(str_in_self2, str_in_s2, 0, 10, ' ');
  buf3 = ret_val22;
  str_self5 = buf3;
  ret_val23 = STR_fr1097270334(((STR) NULL), str_self5);
  ret_val21 = ret_val23;
  s2 = ret_val21;
  ret_val20 = STR_ap1077157958(plus_self13, s2, TRUE);
  plus_self14 = ret_val20;
  plus_sarg11 = ((STR) &name7);
  ret_val24 = STR_ap2004550586(plus_self14, plus_sarg11);
  ret = ret_val24;
 }
 plus_self15 = ret;
 plus_sarg12 = ((STR) &all_in_pre_order);
 ret_val25 = STR_ap2004550586(plus_self15, plus_sarg12);
 L26=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
 if (L26==NULL) FATAL("Unable to allocate more memory");
 memset(L26,0,sizeof(struct BOOL_boxed_struct));
 ((OB)L26)->header.tag=BOOL_tag;
 L25 = (dSTR)((BOOL_boxed) L26);
 ((BOOL_boxed) L25)->immutable_part = ATTR(self,all_in_pre_order);
 plus_self16 = STR_pl1270664985(ret_val25, L25);
 plus_sarg13 = ((STR) &all_in1635012407);
 ret_val26 = STR_ap2004550586(plus_self16, plus_sarg13);
 L28=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
 if (L28==NULL) FATAL("Unable to allocate more memory");
 memset(L28,0,sizeof(struct BOOL_boxed_struct));
 ((OB)L28)->header.tag=BOOL_tag;
 L27 = (dSTR)((BOOL_boxed) L28);
 ((BOOL_boxed) L27)->immutable_part = ATTR(self,all_in_eval_orde);
 plus_self17 = STR_pl1270664985(ret_val26, L27);
 plus_sarg14 = ((STR) &name9);
 ret_val27 = STR_ap2004550586(plus_self17, plus_sarg14);
 ret = ret_val27;
 plus_self18 = ret;
 plus_sarg15 = ((STR) &assign_in_order);
 ret_val28 = STR_ap2004550586(plus_self18, plus_sarg15);
 L30=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
 if (L30==NULL) FATAL("Unable to allocate more memory");
 memset(L30,0,sizeof(struct BOOL_boxed_struct));
 ((OB)L30)->header.tag=BOOL_tag;
 L29 = (dSTR)((BOOL_boxed) L30);
 ((BOOL_boxed) L29)->immutable_part = ATTR(self,assign_in_order);
 plus_self19 = STR_pl1270664985(ret_val28, L29);
 plus_sarg16 = ((STR) &calls_894534736);
 ret_val29 = STR_ap2004550586(plus_self19, plus_sarg16);
 L33=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
 if (L33==NULL) FATAL("Unable to allocate more memory");
 memset(L33,0,sizeof(struct BOOL_boxed_struct));
 ((OB)L33)->header.tag=BOOL_tag;
 L32 = (dSTR)((BOOL_boxed) L33);
 ((BOOL_boxed) L32)->immutable_part = ATTR(self,calls_in_eval_or);
 plus_self20 = STR_pl1270664985(ret_val29, L32);
 plus_sarg17 = ((STR) &name9);
 ret_val30 = STR_ap2004550586(plus_self20, plus_sarg17);
 ret = ret_val30;
 plus_self21 = ret;
 plus_sarg18 = ((STR) &loops);
 ret_val31 = STR_ap2004550586(plus_self21, plus_sarg18);
 plus_self22 = ret_val31;
 plus_arg3 = ATTR(self,loops);
 str_self6 = plus_arg3;
 clear_self3 = buf4;
 L34 = (clear_self3==((FSTR) NULL));
 L351_=!(L34); 
 if (L351_) {
  SATTR(clear_self3,loc,0);
 }
 str_in_self3 = str_self6;
 str_in_s3 = buf4;
 ret_val34 = INT_st367594495(str_in_self3, str_in_s3, 0, 10, ' ');
 buf4 = ret_val34;
 str_self7 = buf4;
 ret_val35 = STR_fr1097270334(((STR) NULL), str_self7);
 ret_val33 = ret_val35;
 s3 = ret_val33;
 ret_val32 = STR_ap1077157958(plus_self22, s3, TRUE);
 plus_self23 = ret_val32;
 plus_sarg19 = ((STR) &indent);
 ret_val36 = STR_ap2004550586(plus_self23, plus_sarg19);
 plus_self24 = ret_val36;
 plus_arg4 = ATTR(self,indent);
 str_self8 = plus_arg4;
 clear_self4 = buf5;
 L36 = (clear_self4==((FSTR) NULL));
 L371_=!(L36); 
 if (L371_) {
  SATTR(clear_self4,loc,0);
 }
 str_in_self4 = str_self8;
 str_in_s4 = buf5;
 ret_val39 = INT_st367594495(str_in_self4, str_in_s4, 0, 10, ' ');
 buf5 = ret_val39;
 str_self9 = buf5;
 ret_val40 = STR_fr1097270334(((STR) NULL), str_self9);
 ret_val38 = ret_val40;
 s4 = ret_val38;
 ret_val37 = STR_ap1077157958(plus_self24, s4, TRUE);
 plus_self25 = ret_val37;
 plus_sarg20 = ((STR) &name40);
 ret_val41 = STR_ap2004550586(plus_self25, plus_sarg20);
 ret = ret_val41;
 {
  BOOL f_L381_ = TRUE;
  /* Initialization of inlined iter A_STACK{AM_CURSOR_POS}::elt!:AM_CURSOR_POS */
  L11 = ATTR(self,stack1);
  eltb_self = L11;
  eltb_if_first = TRUE;
  while (1) {
   plus_self26 = ret;
   plus_sarg21 = ((STR) &name8);
   ret_val42 = STR_ap2004550586(plus_self26, plus_sarg21);
   plus_self27 = ret_val42;
   aget_self = ATTR(eltb_self,s);
   if (f_L381_) {
    f_L381_ = FALSE;
    L40 = FLISTA587993904(ATTR(eltb_self,s));
    L421_=INTMINUS(L40,1); 
    L21 = L421_;
    L31 = 0;
    L381_=L21+1;L381_m=L31; 
   }
   if(L381_--<=L381_m)  goto after_loop; 
   aget_ind = L381_;
   L431_=(AM_CURSOR_POS)ARR((FLISTA886630705)aget_self,aget_ind); 
   ret_val44 = L431_;
   plus_sarg22 = AM_CUR555854443(ret_val44);
   ret_val45 = STR_ap2004550586(plus_self27, plus_sarg22);
   plus_self28 = ret_val45;
   plus_sarg23 = ((STR) &name9);
   ret_val46 = STR_ap2004550586(plus_self28, plus_sarg23);
   ret = ret_val46;
  }
 }
 after_loop: ;
 plus_self29 = ret;
 plus_sarg24 = ((STR) &name32);
 ret_val47 = STR_ap2004550586(plus_self29, plus_sarg24);
 ret = ret_val47;
 ret_val = ret;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM AM_CUR52795524(AM_CURSOR self) {
 dAM ret_val;
 INT branch1 = INT_zero;
 dAM n = ((dAM) NULL);
 dAM tcur;
 AM_CURSOR_POS s;
 dAM tcur1;
 AM_CURSOR_POS d;
 dAM m;
 AM_CURSOR_POS d11;
 A_STAC1244966098 size_self;
 INT ret_val1 = INT_zero;
 A_STAC1244966098 pop_self;
 AM_CURSOR_POS ret_val2;
 A_STAC1244966098 top_self;
 AM_CURSOR_POS ret_val3;
 A_STAC1244966098 pop_self1;
 AM_CURSOR_POS ret_val4;
 A_STAC1244966098 top_self1;
 AM_CURSOR_POS ret_val5;
 A_STAC1244966098 pop_self2;
 AM_CURSOR_POS ret_val6;
 BOOL L1;
 BOOL L21_;
 INT L3;
 BOOL L41_;
 BOOL L51_;
 INT L6;
 INT L71_;
 BOOL L81_;
 INT L9;
 INT L101_;
 BOOL L11;
 BOOL L12;
 BOOL L131_;
 AM_CURSOR_POS L14;
 INT L15;
 BOOL L161_;
 BOOL L17;
 BOOL L181_;
 BOOL L19;
 BOOL L201_;
 BOOL L22;
 AM_CURSOR_POS L23;
 INT L24;
 BOOL L251_;
 BOOL L26;
 BOOL L271_;
 if (ATTR(self,ignore_next)) {
  SATTR(self,ignore_next,FALSE);
  ret_val = ATTR(self,cur);
  return ret_val;
 }
 branch1 = 0;
 SATTR(self,started,TRUE);
 n = ((dAM) NULL);
 if ((ATTR(self,cur)==((dAM) NULL))) {
  SATTR(self,cur,ATTR(self,top1));
  SATTR(self,mark,FALSE);
  n = ATTR(self,cur);
 }
 else {
  L1 = AM_CUR1972821118(self, ATTR(self,cur));
  L21_=!(L1); 
  if (L21_) {
   n = AM_CUR710387089(self, ATTR(self,cur), 0);
  }
  else {
   tcur = ATTR(self,cur);
   if (tcur==NULL) {
    goto other226;
   } else
   switch (TAG(tcur)) {
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
     n = AM_CUR710387089(self, ATTR(self,cur), 999998);
     branch1 = 0; break;
    default: ;
    other226: ;
   }
  }
 }
 while (1) {
  if ((n==((dAM) NULL))) {
  }
  else {
   goto after_loop;
  }
  size_self = ATTR(self,stack1);
  ret_val1 = FLISTA587993904(ATTR(size_self,s));
  L3 = ret_val1;
  L41_=(L3)==(0); 
  if (L41_) {
   SATTR(self,cur,((dAM) NULL));
   ret_val = ((dAM) NULL);
   return ret_val;
  }
  pop_self = ATTR(self,stack1);
  ret_val2 = FLISTA971253650(ATTR(pop_self,s));
  s = ret_val2;
  branch1 = ATTR(s,branch);
  tcur1 = ATTR(s,stmt);
  if (tcur1==NULL) {
   goto other227;
  } else
  switch (TAG(tcur1)) {
   case AM_LOOP_STMT_tag:
    L51_=(branch1)==(2); 
    if (L51_) {
     L6 = ATTR(self,loops);
     L71_=INTMINUS(L6,1); 
     SATTR(self,loops,L71_);
    } break;
   default: ;
   other227: ;
  }
  L81_=(branch1)<(999999); 
  if (L81_) {
   L9 = ATTR(self,indent);
   L101_=INTMINUS(L9,1); 
   SATTR(self,indent,L101_);
  }
  SATTR(self,cur,ATTR(s,stmt));
  SATTR(self,mark,ATTR(s,mark));
  n = AM_CUR710387089(self, ATTR(self,cur), branch1);
  if (AM_CUR1972821118(self, ATTR(self,cur))) {
   L12 = (n==((dAM) NULL));
   L131_=!(L12); 
   if (L131_) {
    top_self = ATTR(self,stack1);
    ret_val3 = FLISTA860779230(ATTR(top_self,s));
    L14=ret_val3;
    L15 = ATTR(L14,branch);
    L161_=(L15)==(999999); 
    L11 = L161_;
   } else {
    L11 = FALSE;
   }
   if (L11) {
    pop_self1 = ATTR(self,stack1);
    ret_val4 = FLISTA971253650(ATTR(pop_self1,s));
    d = ret_val4;
    ret_val = ATTR(self,cur);
    return ret_val;
   }
   else {
    if ((n==((dAM) NULL))) {
     L181_=(branch1)==(999999); 
     L19 = L181_;
     L201_=!(L19); 
     L17 = L201_;
    } else {
     L17 = FALSE;
    }
    if (L17) {
     ret_val = ATTR(self,cur);
     return ret_val;
    }
   }
  }
  branch1 = 0;
 }
 after_loop: ;
 while (1) {
  if (AM_CUR1972821118(self, n)) {
   m = AM_CUR710387089(self, n, branch1);
   if ((m==((dAM) NULL))) {
    L22 = TRUE;
   } else {
    top_self1 = ATTR(self,stack1);
    ret_val5 = FLISTA860779230(ATTR(top_self1,s));
    L23=ret_val5;
    L24 = ATTR(L23,branch);
    L251_=(L24)==(999999); 
    L22 = L251_;
   }
   if (L22) {
    L26 = (m==((dAM) NULL));
    L271_=!(L26); 
    if (L271_) {
     pop_self2 = ATTR(self,stack1);
     ret_val6 = FLISTA971253650(ATTR(pop_self2,s));
     d11 = ret_val6;
    }
    SATTR(self,cur,n);
    ret_val = n;
    return ret_val;
   }
   else {
    n = m;
    branch1 = 0;
   }
  }
  else {
   SATTR(self,cur,n);
   ret_val = n;
   return ret_val;
  }
 }
 after_loop1: ;
}


#undef IS_ITER
#define IS_ITER 0

dAM AM_CUR710387089(AM_CURSOR self, dAM cur, INT branch1) {
 dAM ret_val;
 FLISTdAM_STMT L11;
 INT a = INT_zero;
 FLISTdAM_STMT L21;
 INT a11 = INT_zero;
 FLISTdAM_STMT L31;
 INT a21 = INT_zero;
 INT b1 = INT_zero;
 FLISTA1327600838 L41;
 ARRAYdAM_EXPR l;
 AM_ROU1916046290 L51;
 INT a31 = INT_zero;
 AM_EXT_CALL_EXPR L61;
 INT a41 = INT_zero;
 AM_ITE1809135850 L71;
 INT a5 = INT_zero;
 AM_BND260301329 L81;
 INT a6 = INT_zero;
 AM_BND367211769 L91;
 INT a7 = INT_zero;
 AM_ARRAY_EXPR L101;
 INT a8 = INT_zero;
 AM_BND1124877163 L121;
 INT a9 = INT_zero;
 AM_CURSOR_POS create_self;
 dAM create_curr_stmt;
 BOOL create_m = BOOL_zero;
 INT create_branch = INT_zero;
 AM_CURSOR_POS ret_val1;
 AM_CURSOR_POS r;
 A_STAC1244966098 push_self;
 AM_CURSOR_POS push_e;
 AM_CURSOR_POS create_self1;
 dAM create_curr_stmt1;
 BOOL create_m1 = BOOL_zero;
 INT create_branch1 = INT_zero;
 AM_CURSOR_POS ret_val2;
 AM_CURSOR_POS r1;
 A_STAC1244966098 push_self1;
 AM_CURSOR_POS push_e1;
 AM_CURSOR_POS create_self2;
 dAM create_curr_stmt2;
 BOOL create_m2 = BOOL_zero;
 INT create_branch2 = INT_zero;
 AM_CURSOR_POS ret_val3;
 AM_CURSOR_POS r2;
 A_STAC1244966098 push_self2;
 AM_CURSOR_POS push_e2;
 AM_CURSOR_POS create_self3;
 dAM create_curr_stmt3;
 BOOL create_m3 = BOOL_zero;
 INT create_branch3 = INT_zero;
 AM_CURSOR_POS ret_val4;
 AM_CURSOR_POS r3;
 A_STAC1244966098 push_self3;
 AM_CURSOR_POS push_e3;
 AM_CURSOR_POS create_self4;
 dAM create_curr_stmt4;
 BOOL create_m4 = BOOL_zero;
 INT create_branch4 = INT_zero;
 AM_CURSOR_POS ret_val5;
 AM_CURSOR_POS r4;
 A_STAC1244966098 push_self4;
 AM_CURSOR_POS push_e4;
 AM_CURSOR_POS create_self5;
 dAM create_curr_stmt5;
 BOOL create_m5 = BOOL_zero;
 INT create_branch5 = INT_zero;
 AM_CURSOR_POS ret_val6;
 AM_CURSOR_POS r5;
 A_STAC1244966098 push_self5;
 AM_CURSOR_POS push_e5;
 AM_CURSOR_POS create_self6;
 dAM create_curr_stmt6;
 BOOL create_m6 = BOOL_zero;
 INT create_branch6 = INT_zero;
 AM_CURSOR_POS ret_val7;
 AM_CURSOR_POS r6;
 A_STAC1244966098 push_self6;
 AM_CURSOR_POS push_e6;
 AM_CURSOR_POS create_self7;
 dAM create_curr_stmt7;
 BOOL create_m7 = BOOL_zero;
 INT create_branch7 = INT_zero;
 AM_CURSOR_POS ret_val8;
 AM_CURSOR_POS r7;
 A_STAC1244966098 push_self7;
 AM_CURSOR_POS push_e7;
 AM_CURSOR_POS create_self8;
 dAM create_curr_stmt8;
 BOOL create_m8 = BOOL_zero;
 INT create_branch8 = INT_zero;
 AM_CURSOR_POS ret_val9;
 AM_CURSOR_POS r8;
 A_STAC1244966098 push_self8;
 AM_CURSOR_POS push_e8;
 AM_CURSOR_POS create_self9;
 dAM create_curr_stmt9;
 BOOL create_m9 = BOOL_zero;
 INT create_branch9 = INT_zero;
 AM_CURSOR_POS ret_val10;
 AM_CURSOR_POS r9;
 A_STAC1244966098 push_self9;
 AM_CURSOR_POS push_e9;
 AM_CURSOR_POS create_self10;
 dAM create_curr_stmt10;
 BOOL create_m10 = BOOL_zero;
 INT create_branch10 = INT_zero;
 AM_CURSOR_POS ret_val11;
 AM_CURSOR_POS r10;
 A_STAC1244966098 push_self10;
 AM_CURSOR_POS push_e10;
 AM_CURSOR_POS create_self11;
 dAM create_curr_stmt11;
 BOOL create_m11 = BOOL_zero;
 INT create_branch11 = INT_zero;
 AM_CURSOR_POS ret_val12;
 AM_CURSOR_POS r11;
 A_STAC1244966098 push_self11;
 AM_CURSOR_POS push_e11;
 AM_CURSOR_POS create_self12;
 dAM create_curr_stmt12;
 BOOL create_m12 = BOOL_zero;
 INT create_branch12 = INT_zero;
 AM_CURSOR_POS ret_val13;
 AM_CURSOR_POS r12;
 A_STAC1244966098 push_self12;
 AM_CURSOR_POS push_e12;
 AM_CURSOR_POS create_self13;
 dAM create_curr_stmt13;
 BOOL create_m13 = BOOL_zero;
 INT create_branch13 = INT_zero;
 AM_CURSOR_POS ret_val14;
 AM_CURSOR_POS r13;
 A_STAC1244966098 push_self13;
 AM_CURSOR_POS push_e13;
 AM_CURSOR_POS create_self14;
 dAM create_curr_stmt14;
 BOOL create_m14 = BOOL_zero;
 INT create_branch14 = INT_zero;
 AM_CURSOR_POS ret_val15;
 AM_CURSOR_POS r14;
 A_STAC1244966098 push_self14;
 AM_CURSOR_POS push_e14;
 AM_CURSOR_POS create_self15;
 dAM create_curr_stmt15;
 BOOL create_m15 = BOOL_zero;
 INT create_branch15 = INT_zero;
 AM_CURSOR_POS ret_val16;
 AM_CURSOR_POS r15;
 A_STAC1244966098 push_self15;
 AM_CURSOR_POS push_e15;
 FLISTdAM_STMT aget_self;
 INT aget_ind = INT_zero;
 dAM_STMT ret_val17;
 AM_CURSOR_POS create_self16;
 dAM create_curr_stmt16;
 BOOL create_m16 = BOOL_zero;
 INT create_branch16 = INT_zero;
 AM_CURSOR_POS ret_val18;
 AM_CURSOR_POS r16;
 A_STAC1244966098 push_self16;
 AM_CURSOR_POS push_e16;
 FLISTdAM_STMT aget_self1;
 INT aget_ind1 = INT_zero;
 dAM_STMT ret_val19;
 AM_CURSOR_POS create_self17;
 dAM create_curr_stmt17;
 BOOL create_m17 = BOOL_zero;
 INT create_branch17 = INT_zero;
 AM_CURSOR_POS ret_val20;
 AM_CURSOR_POS r17;
 A_STAC1244966098 push_self17;
 AM_CURSOR_POS push_e17;
 AM_CURSOR_POS create_self18;
 dAM create_curr_stmt18;
 BOOL create_m18 = BOOL_zero;
 INT create_branch18 = INT_zero;
 AM_CURSOR_POS ret_val21;
 AM_CURSOR_POS r18;
 A_STAC1244966098 push_self18;
 AM_CURSOR_POS push_e18;
 FLISTdAM_STMT aget_self2;
 INT aget_ind2 = INT_zero;
 dAM_STMT ret_val22;
 AM_CURSOR_POS create_self19;
 dAM create_curr_stmt19;
 BOOL create_m19 = BOOL_zero;
 INT create_branch19 = INT_zero;
 AM_CURSOR_POS ret_val23;
 AM_CURSOR_POS r19;
 A_STAC1244966098 push_self19;
 AM_CURSOR_POS push_e19;
 FLISTdAM_STMT aget_self3;
 INT aget_ind3 = INT_zero;
 dAM_STMT ret_val24;
 AM_CURSOR_POS create_self20;
 dAM create_curr_stmt20;
 BOOL create_m20 = BOOL_zero;
 INT create_branch20 = INT_zero;
 AM_CURSOR_POS ret_val25;
 AM_CURSOR_POS r20;
 A_STAC1244966098 push_self20;
 AM_CURSOR_POS push_e20;
 AM_CURSOR_POS create_self21;
 dAM create_curr_stmt21;
 BOOL create_m21 = BOOL_zero;
 INT create_branch21 = INT_zero;
 AM_CURSOR_POS ret_val26;
 AM_CURSOR_POS r21;
 A_STAC1244966098 push_self21;
 AM_CURSOR_POS push_e21;
 AM_CURSOR_POS create_self22;
 dAM create_curr_stmt22;
 BOOL create_m22 = BOOL_zero;
 INT create_branch22 = INT_zero;
 AM_CURSOR_POS ret_val27;
 AM_CURSOR_POS r22;
 A_STAC1244966098 push_self22;
 AM_CURSOR_POS push_e22;
 AM_CURSOR_POS create_self23;
 dAM create_curr_stmt23;
 BOOL create_m23 = BOOL_zero;
 INT create_branch23 = INT_zero;
 AM_CURSOR_POS ret_val28;
 AM_CURSOR_POS r23;
 A_STAC1244966098 push_self23;
 AM_CURSOR_POS push_e23;
 AM_CURSOR_POS create_self24;
 dAM create_curr_stmt24;
 BOOL create_m24 = BOOL_zero;
 INT create_branch24 = INT_zero;
 AM_CURSOR_POS ret_val29;
 AM_CURSOR_POS r24;
 A_STAC1244966098 push_self24;
 AM_CURSOR_POS push_e24;
 AM_CURSOR_POS create_self25;
 dAM create_curr_stmt25;
 BOOL create_m25 = BOOL_zero;
 INT create_branch25 = INT_zero;
 AM_CURSOR_POS ret_val30;
 AM_CURSOR_POS r25;
 A_STAC1244966098 push_self25;
 AM_CURSOR_POS push_e25;
 FLISTdAM_STMT aget_self4;
 INT aget_ind4 = INT_zero;
 dAM_STMT ret_val31;
 AM_CURSOR_POS create_self26;
 dAM create_curr_stmt26;
 BOOL create_m26 = BOOL_zero;
 INT create_branch26 = INT_zero;
 AM_CURSOR_POS ret_val32;
 AM_CURSOR_POS r26;
 A_STAC1244966098 push_self26;
 AM_CURSOR_POS push_e26;
 FLISTdAM_STMT aget_self5;
 INT aget_ind5 = INT_zero;
 dAM_STMT ret_val33;
 AM_CURSOR_POS create_self27;
 dAM create_curr_stmt27;
 BOOL create_m27 = BOOL_zero;
 INT create_branch27 = INT_zero;
 AM_CURSOR_POS ret_val34;
 AM_CURSOR_POS r27;
 A_STAC1244966098 push_self27;
 AM_CURSOR_POS push_e27;
 AM_CURSOR_POS create_self28;
 dAM create_curr_stmt28;
 BOOL create_m28 = BOOL_zero;
 INT create_branch28 = INT_zero;
 AM_CURSOR_POS ret_val35;
 AM_CURSOR_POS r28;
 A_STAC1244966098 push_self28;
 AM_CURSOR_POS push_e28;
 AM_CURSOR_POS create_self29;
 dAM create_curr_stmt29;
 BOOL create_m29 = BOOL_zero;
 INT create_branch29 = INT_zero;
 AM_CURSOR_POS ret_val36;
 AM_CURSOR_POS r29;
 A_STAC1244966098 push_self29;
 AM_CURSOR_POS push_e29;
 AM_CURSOR_POS create_self30;
 dAM create_curr_stmt30;
 BOOL create_m30 = BOOL_zero;
 INT create_branch30 = INT_zero;
 AM_CURSOR_POS ret_val37;
 AM_CURSOR_POS r30;
 A_STAC1244966098 push_self30;
 AM_CURSOR_POS push_e30;
 FLISTdAM_EXPR aget_self6;
 INT aget_ind6 = INT_zero;
 dAM_EXPR ret_val38;
 AM_CURSOR_POS create_self31;
 dAM create_curr_stmt31;
 BOOL create_m31 = BOOL_zero;
 INT create_branch31 = INT_zero;
 AM_CURSOR_POS ret_val39;
 AM_CURSOR_POS r31;
 A_STAC1244966098 push_self31;
 AM_CURSOR_POS push_e31;
 FLISTdAM_STMT aget_self7;
 INT aget_ind7 = INT_zero;
 dAM_STMT ret_val40;
 AM_CURSOR_POS create_self32;
 dAM create_curr_stmt32;
 BOOL create_m32 = BOOL_zero;
 INT create_branch32 = INT_zero;
 AM_CURSOR_POS ret_val41;
 AM_CURSOR_POS r32;
 A_STAC1244966098 push_self32;
 AM_CURSOR_POS push_e32;
 FLISTdAM_STMT aget_self8;
 INT aget_ind8 = INT_zero;
 dAM_STMT ret_val42;
 AM_CURSOR_POS create_self33;
 dAM create_curr_stmt33;
 BOOL create_m33 = BOOL_zero;
 INT create_branch33 = INT_zero;
 AM_CURSOR_POS ret_val43;
 AM_CURSOR_POS r33;
 A_STAC1244966098 push_self33;
 AM_CURSOR_POS push_e33;
 AM_CURSOR_POS create_self34;
 dAM create_curr_stmt34;
 BOOL create_m34 = BOOL_zero;
 INT create_branch34 = INT_zero;
 AM_CURSOR_POS ret_val44;
 AM_CURSOR_POS r34;
 A_STAC1244966098 push_self34;
 AM_CURSOR_POS push_e34;
 AM_CURSOR_POS create_self35;
 dAM create_curr_stmt35;
 BOOL create_m35 = BOOL_zero;
 INT create_branch35 = INT_zero;
 AM_CURSOR_POS ret_val45;
 AM_CURSOR_POS r35;
 A_STAC1244966098 push_self35;
 AM_CURSOR_POS push_e35;
 AM_CURSOR_POS create_self36;
 dAM create_curr_stmt36;
 BOOL create_m36 = BOOL_zero;
 INT create_branch36 = INT_zero;
 AM_CURSOR_POS ret_val46;
 AM_CURSOR_POS r36;
 A_STAC1244966098 push_self36;
 AM_CURSOR_POS push_e36;
 AM_CURSOR_POS create_self37;
 dAM create_curr_stmt37;
 BOOL create_m37 = BOOL_zero;
 INT create_branch37 = INT_zero;
 AM_CURSOR_POS ret_val47;
 AM_CURSOR_POS r37;
 A_STAC1244966098 push_self37;
 AM_CURSOR_POS push_e37;
 AM_CURSOR_POS create_self38;
 dAM create_curr_stmt38;
 BOOL create_m38 = BOOL_zero;
 INT create_branch38 = INT_zero;
 AM_CURSOR_POS ret_val48;
 AM_CURSOR_POS r38;
 A_STAC1244966098 push_self38;
 AM_CURSOR_POS push_e38;
 AM_CURSOR_POS create_self39;
 dAM create_curr_stmt39;
 BOOL create_m39 = BOOL_zero;
 INT create_branch39 = INT_zero;
 AM_CURSOR_POS ret_val49;
 AM_CURSOR_POS r39;
 A_STAC1244966098 push_self39;
 AM_CURSOR_POS push_e39;
 AM_CURSOR_POS create_self40;
 dAM create_curr_stmt40;
 BOOL create_m40 = BOOL_zero;
 INT create_branch40 = INT_zero;
 AM_CURSOR_POS ret_val50;
 AM_CURSOR_POS r40;
 A_STAC1244966098 push_self40;
 AM_CURSOR_POS push_e40;
 AM_CURSOR_POS create_self41;
 dAM create_curr_stmt41;
 BOOL create_m41 = BOOL_zero;
 INT create_branch41 = INT_zero;
 AM_CURSOR_POS ret_val51;
 AM_CURSOR_POS r41;
 A_STAC1244966098 push_self41;
 AM_CURSOR_POS push_e41;
 AM_CURSOR_POS create_self42;
 dAM create_curr_stmt42;
 BOOL create_m42 = BOOL_zero;
 INT create_branch42 = INT_zero;
 AM_CURSOR_POS ret_val52;
 AM_CURSOR_POS r42;
 A_STAC1244966098 push_self42;
 AM_CURSOR_POS push_e42;
 INT L131 = INT_zero;
 AM_CURSOR_POS create_self43;
 dAM create_curr_stmt43;
 BOOL create_m43 = BOOL_zero;
 INT create_branch43 = INT_zero;
 AM_CURSOR_POS ret_val53;
 AM_CURSOR_POS r43;
 A_STAC1244966098 push_self43;
 AM_CURSOR_POS push_e43;
 INT L141 = INT_zero;
 AM_CURSOR_POS create_self44;
 dAM create_curr_stmt44;
 BOOL create_m44 = BOOL_zero;
 INT create_branch44 = INT_zero;
 AM_CURSOR_POS ret_val54;
 AM_CURSOR_POS r44;
 A_STAC1244966098 push_self44;
 AM_CURSOR_POS push_e44;
 AM_CURSOR_POS create_self45;
 dAM create_curr_stmt45;
 BOOL create_m45 = BOOL_zero;
 INT create_branch45 = INT_zero;
 AM_CURSOR_POS ret_val55;
 AM_CURSOR_POS r45;
 A_STAC1244966098 push_self45;
 AM_CURSOR_POS push_e45;
 INT L151 = INT_zero;
 AM_CURSOR_POS create_self46;
 dAM create_curr_stmt46;
 BOOL create_m46 = BOOL_zero;
 INT create_branch46 = INT_zero;
 AM_CURSOR_POS ret_val56;
 AM_CURSOR_POS r46;
 A_STAC1244966098 push_self46;
 AM_CURSOR_POS push_e46;
 AM_CURSOR_POS create_self47;
 dAM create_curr_stmt47;
 BOOL create_m47 = BOOL_zero;
 INT create_branch47 = INT_zero;
 AM_CURSOR_POS ret_val57;
 AM_CURSOR_POS r47;
 A_STAC1244966098 push_self47;
 AM_CURSOR_POS push_e47;
 INT L161 = INT_zero;
 AM_CURSOR_POS create_self48;
 dAM create_curr_stmt48;
 BOOL create_m48 = BOOL_zero;
 INT create_branch48 = INT_zero;
 AM_CURSOR_POS ret_val58;
 AM_CURSOR_POS r48;
 A_STAC1244966098 push_self48;
 AM_CURSOR_POS push_e48;
 AM_CURSOR_POS create_self49;
 dAM create_curr_stmt49;
 BOOL create_m49 = BOOL_zero;
 INT create_branch49 = INT_zero;
 AM_CURSOR_POS ret_val59;
 AM_CURSOR_POS r49;
 A_STAC1244966098 push_self49;
 AM_CURSOR_POS push_e49;
 AM_CURSOR_POS create_self50;
 dAM create_curr_stmt50;
 BOOL create_m50 = BOOL_zero;
 INT create_branch50 = INT_zero;
 AM_CURSOR_POS ret_val60;
 AM_CURSOR_POS r50;
 A_STAC1244966098 push_self50;
 AM_CURSOR_POS push_e50;
 INT L171 = INT_zero;
 AM_CURSOR_POS create_self51;
 dAM create_curr_stmt51;
 BOOL create_m51 = BOOL_zero;
 INT create_branch51 = INT_zero;
 AM_CURSOR_POS ret_val61;
 AM_CURSOR_POS r51;
 A_STAC1244966098 push_self51;
 AM_CURSOR_POS push_e51;
 INT L181 = INT_zero;
 AM_CURSOR_POS create_self52;
 dAM create_curr_stmt52;
 BOOL create_m52 = BOOL_zero;
 INT create_branch52 = INT_zero;
 AM_CURSOR_POS ret_val62;
 AM_CURSOR_POS r52;
 A_STAC1244966098 push_self52;
 AM_CURSOR_POS push_e52;
 INT L191 = INT_zero;
 AM_CURSOR_POS create_self53;
 dAM create_curr_stmt53;
 BOOL create_m53 = BOOL_zero;
 INT create_branch53 = INT_zero;
 AM_CURSOR_POS ret_val63;
 AM_CURSOR_POS r53;
 A_STAC1244966098 push_self53;
 AM_CURSOR_POS push_e53;
 AM_CURSOR_POS create_self54;
 dAM create_curr_stmt54;
 BOOL create_m54 = BOOL_zero;
 INT create_branch54 = INT_zero;
 AM_CURSOR_POS ret_val64;
 AM_CURSOR_POS r54;
 A_STAC1244966098 push_self54;
 AM_CURSOR_POS push_e54;
 AM_CURSOR_POS create_self55;
 dAM create_curr_stmt55;
 BOOL create_m55 = BOOL_zero;
 INT create_branch55 = INT_zero;
 AM_CURSOR_POS ret_val65;
 AM_CURSOR_POS r55;
 A_STAC1244966098 push_self55;
 AM_CURSOR_POS push_e55;
 AM_CURSOR_POS create_self56;
 dAM create_curr_stmt56;
 BOOL create_m56 = BOOL_zero;
 INT create_branch56 = INT_zero;
 AM_CURSOR_POS ret_val66;
 AM_CURSOR_POS r56;
 A_STAC1244966098 push_self56;
 AM_CURSOR_POS push_e56;
 AM_CURSOR_POS create_self57;
 dAM create_curr_stmt57;
 BOOL create_m57 = BOOL_zero;
 INT create_branch57 = INT_zero;
 AM_CURSOR_POS ret_val67;
 AM_CURSOR_POS r57;
 A_STAC1244966098 push_self57;
 AM_CURSOR_POS push_e57;
 AM_CURSOR_POS create_self58;
 dAM create_curr_stmt58;
 BOOL create_m58 = BOOL_zero;
 INT create_branch58 = INT_zero;
 AM_CURSOR_POS ret_val68;
 AM_CURSOR_POS r58;
 A_STAC1244966098 push_self58;
 AM_CURSOR_POS push_e58;
 AM_CURSOR_POS create_self59;
 dAM create_curr_stmt59;
 BOOL create_m59 = BOOL_zero;
 INT create_branch59 = INT_zero;
 AM_CURSOR_POS ret_val69;
 AM_CURSOR_POS r59;
 A_STAC1244966098 push_self59;
 AM_CURSOR_POS push_e59;
 AM_CURSOR_POS create_self60;
 dAM create_curr_stmt60;
 BOOL create_m60 = BOOL_zero;
 INT create_branch60 = INT_zero;
 AM_CURSOR_POS ret_val70;
 AM_CURSOR_POS r60;
 A_STAC1244966098 push_self60;
 AM_CURSOR_POS push_e60;
 AM_CURSOR_POS create_self61;
 dAM create_curr_stmt61;
 BOOL create_m61 = BOOL_zero;
 INT create_branch61 = INT_zero;
 AM_CURSOR_POS ret_val71;
 AM_CURSOR_POS r61;
 A_STAC1244966098 push_self61;
 AM_CURSOR_POS push_e61;
 AM_CURSOR_POS create_self62;
 dAM create_curr_stmt62;
 BOOL create_m62 = BOOL_zero;
 INT create_branch62 = INT_zero;
 AM_CURSOR_POS ret_val72;
 AM_CURSOR_POS r62;
 A_STAC1244966098 push_self62;
 AM_CURSOR_POS push_e62;
 AM_CURSOR_POS create_self63;
 dAM create_curr_stmt63;
 BOOL create_m63 = BOOL_zero;
 INT create_branch63 = INT_zero;
 AM_CURSOR_POS ret_val73;
 AM_CURSOR_POS r63;
 A_STAC1244966098 push_self63;
 AM_CURSOR_POS push_e63;
 AM_CURSOR_POS create_self64;
 dAM create_curr_stmt64;
 BOOL create_m64 = BOOL_zero;
 INT create_branch64 = INT_zero;
 AM_CURSOR_POS ret_val74;
 AM_CURSOR_POS r64;
 A_STAC1244966098 push_self64;
 AM_CURSOR_POS push_e64;
 AM_CURSOR_POS create_self65;
 dAM create_curr_stmt65;
 BOOL create_m65 = BOOL_zero;
 INT create_branch65 = INT_zero;
 AM_CURSOR_POS ret_val75;
 AM_CURSOR_POS r65;
 A_STAC1244966098 push_self65;
 AM_CURSOR_POS push_e65;
 AM_CURSOR_POS create_self66;
 dAM create_curr_stmt66;
 BOOL create_m66 = BOOL_zero;
 INT create_branch66 = INT_zero;
 AM_CURSOR_POS ret_val76;
 AM_CURSOR_POS r66;
 A_STAC1244966098 push_self66;
 AM_CURSOR_POS push_e66;
 AM_CURSOR_POS create_self67;
 dAM create_curr_stmt67;
 BOOL create_m67 = BOOL_zero;
 INT create_branch67 = INT_zero;
 AM_CURSOR_POS ret_val77;
 AM_CURSOR_POS r67;
 A_STAC1244966098 push_self67;
 AM_CURSOR_POS push_e67;
 AM_CURSOR_POS create_self68;
 dAM create_curr_stmt68;
 BOOL create_m68 = BOOL_zero;
 INT create_branch68 = INT_zero;
 AM_CURSOR_POS ret_val78;
 AM_CURSOR_POS r68;
 A_STAC1244966098 push_self68;
 AM_CURSOR_POS push_e68;
 AM_CURSOR_POS create_self69;
 dAM create_curr_stmt69;
 BOOL create_m69 = BOOL_zero;
 INT create_branch69 = INT_zero;
 AM_CURSOR_POS ret_val79;
 AM_CURSOR_POS r69;
 A_STAC1244966098 push_self69;
 AM_CURSOR_POS push_e69;
 AM_CURSOR_POS create_self70;
 dAM create_curr_stmt70;
 BOOL create_m70 = BOOL_zero;
 INT create_branch70 = INT_zero;
 AM_CURSOR_POS ret_val80;
 AM_CURSOR_POS r70;
 A_STAC1244966098 push_self70;
 AM_CURSOR_POS push_e70;
 AM_CURSOR_POS create_self71;
 dAM create_curr_stmt71;
 BOOL create_m71 = BOOL_zero;
 INT create_branch71 = INT_zero;
 AM_CURSOR_POS ret_val81;
 AM_CURSOR_POS r71;
 A_STAC1244966098 push_self71;
 AM_CURSOR_POS push_e71;
 OUT create_self72;
 OUT ret_val82;
 OUT plus_self;
 STR plus_s;
 OUT ret_val83;
 FILE1 stdout_self;
 FILE1 ret_val84;
 FILE1 r72;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val85;
 FILE1 stdout_self1;
 FILE1 ret_val86;
 FILE1 r73;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val87;
 FILE1 r74;
 AM_CURSOR_POS create_self73;
 dAM create_curr_stmt72;
 BOOL create_m72 = BOOL_zero;
 INT create_branch72 = INT_zero;
 AM_CURSOR_POS ret_val88;
 AM_CURSOR_POS r75;
 A_STAC1244966098 push_self72;
 AM_CURSOR_POS push_e72;
 INT L20;
 INT L221_;
 BOOL L23;
 BOOL L24;
 BOOL L251_;
 BOOL L261_;
 AM_CURSOR_POS L27;
 OB L28;
 BOOL L29;
 BOOL L30;
 BOOL L321_;
 BOOL L331_;
 AM_CURSOR_POS L34;
 OB L35;
 BOOL L36;
 BOOL L37;
 BOOL L381_;
 BOOL L391_;
 AM_CURSOR_POS L40;
 OB L42;
 BOOL L43;
 BOOL L44;
 BOOL L451_;
 BOOL L461_;
 AM_CURSOR_POS L47;
 OB L48;
 BOOL L49;
 BOOL L50;
 BOOL L521_;
 BOOL L531_;
 AM_CURSOR_POS L54;
 OB L55;
 BOOL L56;
 BOOL L57;
 BOOL L581_;
 BOOL L591_;
 AM_CURSOR_POS L60;
 OB L62;
 BOOL L63;
 BOOL L64;
 BOOL L651_;
 BOOL L661_;
 AM_CURSOR_POS L67;
 OB L68;
 BOOL L69;
 BOOL L70;
 BOOL L721_;
 BOOL L731_;
 AM_CURSOR_POS L74;
 OB L75;
 BOOL L76;
 BOOL L77;
 BOOL L781_;
 BOOL L791_;
 AM_CURSOR_POS L80;
 OB L82;
 BOOL L83;
 BOOL L84;
 BOOL L851_;
 BOOL L861_;
 AM_CURSOR_POS L87;
 OB L88;
 BOOL L89;
 BOOL L90;
 BOOL L921_;
 BOOL L931_;
 AM_CURSOR_POS L94;
 OB L95;
 BOOL L96;
 BOOL L97;
 BOOL L981_;
 BOOL L991_;
 AM_CURSOR_POS L100;
 OB L102;
 BOOL L103;
 BOOL L104;
 BOOL L1051_;
 BOOL L1061_;
 AM_CURSOR_POS L107;
 OB L108;
 INT L109;
 INT L1101_;
 BOOL L111;
 BOOL L112;
 BOOL L1131_;
 BOOL L1141_;
 AM_CURSOR_POS L115;
 OB L116;
 BOOL L117;
 BOOL L118;
 BOOL L1191_;
 BOOL L1201_;
 AM_CURSOR_POS L122;
 OB L123;
 BOOL L124;
 BOOL L125;
 BOOL L1261_;
 BOOL L1271_;
 AM_CURSOR_POS L128;
 OB L129;
 INT L130;
 BOOL L1321_;
 INT L133;
 BOOL L134;
 dAM_STMT L1351_;
 BOOL L136;
 BOOL L1371_;
 INT L1381_;
 INT L139;
 BOOL L1401_;
 INT L1421_;
 AM_CURSOR_POS L143;
 OB L144;
 dAM_STMT L1451_;
 BOOL L146;
 BOOL L147;
 BOOL L1481_;
 INT L149;
 INT L1501_;
 INT L152;
 BOOL L1531_;
 INT L154;
 INT L1551_;
 AM_CURSOR_POS L156;
 OB L157;
 BOOL L158;
 BOOL L159;
 BOOL L1601_;
 BOOL L1621_;
 AM_CURSOR_POS L163;
 OB L164;
 INT L165;
 BOOL L1661_;
 INT L167;
 BOOL L168;
 dAM_STMT L1691_;
 BOOL L170;
 BOOL L1721_;
 INT L1731_;
 INT L174;
 BOOL L1751_;
 INT L1761_;
 AM_CURSOR_POS L177;
 OB L178;
 dAM_STMT L1791_;
 BOOL L180;
 BOOL L182;
 BOOL L1831_;
 INT L184;
 INT L1851_;
 INT L186;
 BOOL L1871_;
 INT L188;
 INT L1891_;
 AM_CURSOR_POS L190;
 OB L192;
 BOOL L193;
 BOOL L194;
 BOOL L195;
 BOOL L1961_;
 BOOL L1971_;
 BOOL L198;
 BOOL L1991_;
 AM_CURSOR_POS L200;
 OB L201;
 BOOL L202;
 BOOL L203;
 BOOL L204;
 BOOL L2051_;
 BOOL L2061_;
 BOOL L207;
 BOOL L2081_;
 AM_CURSOR_POS L209;
 OB L210;
 BOOL L211;
 BOOL L212;
 BOOL L2131_;
 BOOL L2141_;
 AM_CURSOR_POS L215;
 OB L216;
 BOOL L217;
 BOOL L218;
 BOOL L219;
 BOOL L2201_;
 BOOL L2221_;
 BOOL L223;
 BOOL L2241_;
 AM_CURSOR_POS L225;
 OB L226;
 BOOL L227;
 BOOL L228;
 BOOL L2291_;
 BOOL L2301_;
 AM_CURSOR_POS L231;
 OB L232;
 INT L233;
 BOOL L2341_;
 INT L235;
 BOOL L236;
 dAM_STMT L2371_;
 BOOL L238;
 BOOL L2391_;
 INT L2401_;
 INT L241;
 BOOL L2421_;
 INT L2431_;
 AM_CURSOR_POS L244;
 OB L245;
 dAM_STMT L2461_;
 BOOL L247;
 BOOL L248;
 BOOL L2491_;
 INT L250;
 INT L2521_;
 INT L253;
 BOOL L2541_;
 INT L255;
 INT L2561_;
 AM_CURSOR_POS L257;
 OB L258;
 BOOL L259;
 BOOL L260;
 BOOL L2621_;
 BOOL L2631_;
 AM_CURSOR_POS L264;
 OB L265;
 BOOL L266;
 BOOL L267;
 BOOL L2681_;
 BOOL L2691_;
 AM_CURSOR_POS L270;
 OB L271;
 INT L272;
 BOOL L2731_;
 INT L2741_;
 AM_CURSOR_POS L275;
 OB L276;
 dAM_EXPR L2771_;
 INT L278;
 INT L2791_;
 ARRAYdAM_EXPR L280;
 INT L2811_;
 INT L282;
 BOOL L2831_;
 INT L2841_;
 AM_CURSOR_POS L285;
 OB L286;
 dAM_EXPR L2871_;
 INT L2881_;
 INT L289;
 INT L2901_;
 INT L291;
 BOOL L2921_;
 dAM_STMT L2931_;
 BOOL L294;
 BOOL L2951_;
 INT L2961_;
 AM_CURSOR_POS L297;
 OB L298;
 dAM_STMT L2991_;
 INT L3001_;
 INT L301;
 INT L3021_;
 BOOL L303;
 BOOL L3041_;
 BOOL L305;
 BOOL L3061_;
 INT L3071_;
 AM_CURSOR_POS L308;
 OB L309;
 BOOL L310;
 BOOL L311;
 BOOL L3121_;
 BOOL L3131_;
 AM_CURSOR_POS L314;
 OB L315;
 BOOL L316;
 BOOL L317;
 BOOL L3181_;
 BOOL L3191_;
 AM_CURSOR_POS L320;
 OB L322;
 BOOL L323;
 BOOL L324;
 BOOL L3251_;
 BOOL L3261_;
 AM_CURSOR_POS L327;
 OB L328;
 BOOL L329;
 BOOL L330;
 BOOL L3321_;
 BOOL L3331_;
 AM_CURSOR_POS L334;
 OB L335;
 BOOL L336;
 BOOL L337;
 BOOL L3381_;
 BOOL L3391_;
 AM_CURSOR_POS L340;
 OB L341;
 BOOL L342;
 BOOL L343;
 BOOL L3441_;
 ARRAYdAM_EXPR L345;
 INT L3461_;
 INT L347;
 BOOL L3481_;
 INT L3491_;
 AM_CURSOR_POS L350;
 OB L351;
 ARRAYdAM_EXPR L352;
 dAM_EXPR L3531_;
 BOOL L354;
 ARRAYdAM_EXPR L355;
 INT L3561_;
 INT L357;
 INT L3581_;
 INT L359;
 BOOL L3601_;
 BOOL L361;
 BOOL L3621_;
 ARRAYdAM_EXPR L363;
 INT L3641_;
 INT L365;
 INT L3661_;
 AM_CURSOR_POS L367;
 OB L368;
 BOOL L369;
 ARRAYdAM_EXPR L370;
 INT L3711_;
 INT L372;
 INT L3731_;
 INT L374;
 BOOL L3751_;
 BOOL L376;
 BOOL L3771_;
 ARRAYdAM_EXPR L378;
 INT L3791_;
 INT L380;
 INT L3821_;
 AM_CURSOR_POS L383;
 OB L384;
 BOOL L385;
 BOOL L386;
 BOOL L3871_;
 BOOL L3881_;
 AM_CURSOR_POS L389;
 OB L390;
 INT L3921_br;
 INT rL3921_;
 INT L393;
 BOOL L394;
 AM_CALL_ARG L3951_;
 BOOL L396;
 BOOL L3971_;
 INT L3981_;
 INT L399;
 BOOL L4001_;
 INT L4011_;
 AM_CURSOR_POS L402;
 OB L403;
 AM_CALL_ARG L4041_;
 INT L4051_;
 INT L4061_br;
 INT rL4061_;
 INT L407;
 BOOL L408;
 AM_CALL_ARG L4091_;
 BOOL L410;
 BOOL L4111_;
 INT L4121_;
 INT L413;
 BOOL L4141_;
 INT L4151_;
 AM_CURSOR_POS L416;
 OB L417;
 AM_CALL_ARG L4181_;
 INT L4191_;
 BOOL L420;
 BOOL L421;
 BOOL L4221_;
 BOOL L4231_;
 AM_CURSOR_POS L424;
 OB L425;
 INT L4261_br;
 INT rL4261_;
 INT L427;
 BOOL L428;
 AM_CALL_ARG L4291_;
 BOOL L430;
 BOOL L4311_;
 INT L4321_;
 INT L433;
 BOOL L4341_;
 INT L4351_;
 AM_CURSOR_POS L436;
 OB L437;
 AM_CALL_ARG L4381_;
 INT L4391_;
 BOOL L440;
 BOOL L441;
 BOOL L4421_;
 BOOL L4431_;
 AM_CURSOR_POS L444;
 OB L445;
 INT L4461_br;
 INT rL4461_;
 INT L447;
 BOOL L448;
 AM_CALL_ARG L4491_;
 BOOL L450;
 BOOL L4521_;
 INT L4531_;
 INT L454;
 BOOL L4551_;
 INT L4561_;
 AM_CURSOR_POS L457;
 OB L458;
 AM_CALL_ARG L4591_;
 INT L4601_;
 BOOL L462;
 BOOL L463;
 BOOL L4641_;
 BOOL L4651_;
 AM_CURSOR_POS L466;
 OB L467;
 BOOL L468;
 BOOL L469;
 BOOL L4701_;
 BOOL L4711_;
 AM_CURSOR_POS L472;
 OB L473;
 INT L4741_br;
 INT rL4741_;
 INT L475;
 BOOL L476;
 AM_CALL_ARG L4771_;
 BOOL L478;
 BOOL L4791_;
 INT L4801_;
 INT L481;
 BOOL L4821_;
 INT L4831_;
 AM_CURSOR_POS L484;
 OB L485;
 AM_CALL_ARG L4861_;
 INT L4871_;
 INT L4881_br;
 INT rL4881_;
 INT L489;
 BOOL L490;
 dAM_EXPR L4911_;
 BOOL L492;
 BOOL L4931_;
 INT L4941_;
 INT L495;
 BOOL L4961_;
 INT L4971_;
 AM_CURSOR_POS L498;
 OB L499;
 dAM_EXPR L5001_;
 INT L5011_;
 INT L5021_br;
 INT rL5021_;
 INT L503;
 BOOL L504;
 AM_CALL_ARG L5051_;
 BOOL L506;
 BOOL L5071_;
 INT L5081_;
 INT L509;
 BOOL L5101_;
 INT L5111_;
 AM_CURSOR_POS L512;
 OB L513;
 AM_CALL_ARG L5141_;
 INT L5151_;
 BOOL L516;
 BOOL L517;
 BOOL L5181_;
 BOOL L5191_;
 AM_CURSOR_POS L520;
 OB L522;
 BOOL L523;
 BOOL L524;
 BOOL L5251_;
 BOOL L5261_;
 AM_CURSOR_POS L527;
 OB L528;
 BOOL L529;
 BOOL L530;
 BOOL L5321_;
 BOOL L5331_;
 AM_CURSOR_POS L534;
 OB L535;
 BOOL L536;
 BOOL L537;
 BOOL L5381_;
 BOOL L5391_;
 AM_CURSOR_POS L540;
 OB L541;
 BOOL L542;
 BOOL L543;
 BOOL L5441_;
 BOOL L5451_;
 AM_CURSOR_POS L546;
 OB L547;
 BOOL L548;
 BOOL L549;
 BOOL L5501_;
 BOOL L5511_;
 AM_CURSOR_POS L552;
 OB L553;
 BOOL L554;
 BOOL L555;
 BOOL L5561_;
 BOOL L5571_;
 AM_CURSOR_POS L558;
 OB L559;
 BOOL L560;
 BOOL L561;
 BOOL L5621_;
 BOOL L5631_;
 AM_CURSOR_POS L564;
 OB L565;
 BOOL L566;
 BOOL L567;
 BOOL L5681_;
 BOOL L5691_;
 AM_CURSOR_POS L570;
 OB L571;
 BOOL L572;
 BOOL L573;
 BOOL L5741_;
 BOOL L5751_;
 AM_CURSOR_POS L576;
 OB L577;
 BOOL L578;
 BOOL L579;
 BOOL L5801_;
 BOOL L5821_;
 AM_CURSOR_POS L583;
 OB L584;
 BOOL L585;
 BOOL L586;
 BOOL L5871_;
 BOOL L5881_;
 AM_CURSOR_POS L589;
 OB L590;
 BOOL L592;
 BOOL L593;
 BOOL L5941_;
 BOOL L5951_;
 AM_CURSOR_POS L596;
 OB L597;
 BOOL L598;
 BOOL L599;
 BOOL L6001_;
 BOOL L6011_;
 AM_CURSOR_POS L602;
 OB L603;
 BOOL L604;
 BOOL L605;
 BOOL L6061_;
 BOOL L6071_;
 AM_CURSOR_POS L608;
 OB L609;
 BOOL L610;
 BOOL L611;
 BOOL L6121_;
 BOOL L6131_;
 AM_CURSOR_POS L614;
 OB L615;
 BOOL L616;
 BOOL L617;
 BOOL L6181_;
 BOOL L6191_;
 AM_CURSOR_POS L620;
 OB L621;
 BOOL L622;
 BOOL L623;
 BOOL L6241_;
 BOOL L6251_;
 AM_CURSOR_POS L626;
 OB L627;
 extern STR thefol1538856223;
 FILE1 L628;
 OB L629;
 INT L6311_;
 INT L632;
 STR L6331_;
 FILE1 L634;
 OB L635;
 extern STR name9;
 FILE1 L637;
 OB L638;
 INT L640;
 INT L6411_;
 BOOL L642;
 dAM_STMT L643;
 BOOL L644;
 BOOL L6451_;
 BOOL L6461_;
 AM_CURSOR_POS L647;
 OB L648;
 dAM_STMT L649;
 L20 = ATTR(self,indent);
 L221_=INTPLUS(L20,1); 
 SATTR(self,indent,L221_);
 if (cur==NULL) {
  goto other224;
 } else
 switch (TAG(cur)) {
  case AM_ASSIGN_STMT_tag:
   L24 = (ATTR(((AM_ASSIGN_STMT) cur),dest)==((dAM_EXPR) NULL));
   L251_=!(L24); 
   if (L251_) {
    L261_=(branch1)<(1); 
    L23 = L261_;
   } else {
    L23 = FALSE;
   }
   if (L23) {
    push_self = ATTR(self,stack1);
    create_self = ((AM_CURSOR_POS) NULL);
    create_curr_stmt = cur;
    create_m = ATTR(self,mark);
    create_branch = 1;
    L28=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L28==NULL) FATAL("Unable to allocate more memory");
    ((OB)L28)->header.tag=AM_CURSOR_POS_tag;
    L27 = ((AM_CURSOR_POS) L28);
    r = L27;
    SATTR(r,stmt,create_curr_stmt);
    SATTR(r,branch,create_branch);
    SATTR(r,mark,create_m);
    ret_val1 = r;
    push_e = ret_val1;
    SATTR(push_self,s,FLISTA1475379054(ATTR(push_self,s), push_e));
    ret_val = ((dAM) ATTR(((AM_ASSIGN_STMT) cur),dest));
    return ret_val;
   }
   L30 = (ATTR(((AM_ASSIGN_STMT) cur),src1)==((dAM_EXPR) NULL));
   L321_=!(L30); 
   if (L321_) {
    L331_=(branch1)<(2); 
    L29 = L331_;
   } else {
    L29 = FALSE;
   }
   if (L29) {
    push_self1 = ATTR(self,stack1);
    create_self1 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt1 = cur;
    create_m1 = ATTR(self,mark);
    create_branch1 = 2;
    L35=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L35==NULL) FATAL("Unable to allocate more memory");
    ((OB)L35)->header.tag=AM_CURSOR_POS_tag;
    L34 = ((AM_CURSOR_POS) L35);
    r1 = L34;
    SATTR(r1,stmt,create_curr_stmt1);
    SATTR(r1,branch,create_branch1);
    SATTR(r1,mark,create_m1);
    ret_val2 = r1;
    push_e1 = ret_val2;
    SATTR(push_self1,s,FLISTA1475379054(ATTR(push_self1,s), push_e1));
    ret_val = ((dAM) ATTR(((AM_ASSIGN_STMT) cur),src1));
    return ret_val;
   } break;
  case AM_WAITFOR_STMT_tag:
   L37 = (ATTR(((AM_WAITFOR_STMT) cur),dest)==((dAM_EXPR) NULL));
   L381_=!(L37); 
   if (L381_) {
    L391_=(branch1)<(1); 
    L36 = L391_;
   } else {
    L36 = FALSE;
   }
   if (L36) {
    push_self2 = ATTR(self,stack1);
    create_self2 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt2 = cur;
    create_m2 = ATTR(self,mark);
    create_branch2 = 1;
    L42=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L42==NULL) FATAL("Unable to allocate more memory");
    ((OB)L42)->header.tag=AM_CURSOR_POS_tag;
    L40 = ((AM_CURSOR_POS) L42);
    r2 = L40;
    SATTR(r2,stmt,create_curr_stmt2);
    SATTR(r2,branch,create_branch2);
    SATTR(r2,mark,create_m2);
    ret_val3 = r2;
    push_e2 = ret_val3;
    SATTR(push_self2,s,FLISTA1475379054(ATTR(push_self2,s), push_e2));
    ret_val = ((dAM) ATTR(((AM_WAITFOR_STMT) cur),dest));
    return ret_val;
   }
   L44 = (ATTR(((AM_WAITFOR_STMT) cur),src1)==((dAM_EXPR) NULL));
   L451_=!(L44); 
   if (L451_) {
    L461_=(branch1)<(2); 
    L43 = L461_;
   } else {
    L43 = FALSE;
   }
   if (L43) {
    push_self3 = ATTR(self,stack1);
    create_self3 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt3 = cur;
    create_m3 = ATTR(self,mark);
    create_branch3 = 2;
    L48=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L48==NULL) FATAL("Unable to allocate more memory");
    ((OB)L48)->header.tag=AM_CURSOR_POS_tag;
    L47 = ((AM_CURSOR_POS) L48);
    r3 = L47;
    SATTR(r3,stmt,create_curr_stmt3);
    SATTR(r3,branch,create_branch3);
    SATTR(r3,mark,create_m3);
    ret_val4 = r3;
    push_e3 = ret_val4;
    SATTR(push_self3,s,FLISTA1475379054(ATTR(push_self3,s), push_e3));
    ret_val = ((dAM) ATTR(((AM_WAITFOR_STMT) cur),src1));
    return ret_val;
   }
   L50 = (ATTR(((AM_WAITFOR_STMT) cur),prefetch)==((AM_LOCAL_EXPR) NULL));
   L521_=!(L50); 
   if (L521_) {
    L531_=(branch1)<(3); 
    L49 = L531_;
   } else {
    L49 = FALSE;
   }
   if (L49) {
    push_self4 = ATTR(self,stack1);
    create_self4 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt4 = cur;
    create_m4 = ATTR(self,mark);
    create_branch4 = 3;
    L55=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L55==NULL) FATAL("Unable to allocate more memory");
    ((OB)L55)->header.tag=AM_CURSOR_POS_tag;
    L54 = ((AM_CURSOR_POS) L55);
    r4 = L54;
    SATTR(r4,stmt,create_curr_stmt4);
    SATTR(r4,branch,create_branch4);
    SATTR(r4,mark,create_m4);
    ret_val5 = r4;
    push_e4 = ret_val5;
    SATTR(push_self4,s,FLISTA1475379054(ATTR(push_self4,s), push_e4));
    ret_val = ((dAM) ATTR(((AM_WAITFOR_STMT) cur),prefetch));
    return ret_val;
   } break;
  case AM_PREFETCH_STMT_tag:
   L57 = (ATTR(((AM_PREFETCH_STMT) cur),dest)==((dAM_EXPR) NULL));
   L581_=!(L57); 
   if (L581_) {
    L591_=(branch1)<(1); 
    L56 = L591_;
   } else {
    L56 = FALSE;
   }
   if (L56) {
    push_self5 = ATTR(self,stack1);
    create_self5 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt5 = cur;
    create_m5 = ATTR(self,mark);
    create_branch5 = 1;
    L62=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L62==NULL) FATAL("Unable to allocate more memory");
    ((OB)L62)->header.tag=AM_CURSOR_POS_tag;
    L60 = ((AM_CURSOR_POS) L62);
    r5 = L60;
    SATTR(r5,stmt,create_curr_stmt5);
    SATTR(r5,branch,create_branch5);
    SATTR(r5,mark,create_m5);
    ret_val6 = r5;
    push_e5 = ret_val6;
    SATTR(push_self5,s,FLISTA1475379054(ATTR(push_self5,s), push_e5));
    ret_val = ((dAM) ATTR(((AM_PREFETCH_STMT) cur),dest));
    return ret_val;
   }
   L64 = (ATTR(((AM_PREFETCH_STMT) cur),src1)==((dAM_EXPR) NULL));
   L651_=!(L64); 
   if (L651_) {
    L661_=(branch1)<(2); 
    L63 = L661_;
   } else {
    L63 = FALSE;
   }
   if (L63) {
    push_self6 = ATTR(self,stack1);
    create_self6 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt6 = cur;
    create_m6 = ATTR(self,mark);
    create_branch6 = 2;
    L68=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L68==NULL) FATAL("Unable to allocate more memory");
    ((OB)L68)->header.tag=AM_CURSOR_POS_tag;
    L67 = ((AM_CURSOR_POS) L68);
    r6 = L67;
    SATTR(r6,stmt,create_curr_stmt6);
    SATTR(r6,branch,create_branch6);
    SATTR(r6,mark,create_m6);
    ret_val7 = r6;
    push_e6 = ret_val7;
    SATTR(push_self6,s,FLISTA1475379054(ATTR(push_self6,s), push_e6));
    ret_val = ((dAM) ATTR(((AM_PREFETCH_STMT) cur),src1));
    return ret_val;
   }
   L70 = (ATTR(((AM_PREFETCH_STMT) cur),prefetch)==((AM_LOCAL_EXPR) NULL));
   L721_=!(L70); 
   if (L721_) {
    L731_=(branch1)<(3); 
    L69 = L731_;
   } else {
    L69 = FALSE;
   }
   if (L69) {
    push_self7 = ATTR(self,stack1);
    create_self7 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt7 = cur;
    create_m7 = ATTR(self,mark);
    create_branch7 = 3;
    L75=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L75==NULL) FATAL("Unable to allocate more memory");
    ((OB)L75)->header.tag=AM_CURSOR_POS_tag;
    L74 = ((AM_CURSOR_POS) L75);
    r7 = L74;
    SATTR(r7,stmt,create_curr_stmt7);
    SATTR(r7,branch,create_branch7);
    SATTR(r7,mark,create_m7);
    ret_val8 = r7;
    push_e7 = ret_val8;
    SATTR(push_self7,s,FLISTA1475379054(ATTR(push_self7,s), push_e7));
    ret_val = ((dAM) ATTR(((AM_PREFETCH_STMT) cur),prefetch));
    return ret_val;
   } break;
  case AM_IF_STMT_tag:
   L77 = (ATTR(((AM_IF_STMT) cur),test1)==((dAM_EXPR) NULL));
   L781_=!(L77); 
   if (L781_) {
    L791_=(branch1)<(1); 
    L76 = L791_;
   } else {
    L76 = FALSE;
   }
   if (L76) {
    push_self8 = ATTR(self,stack1);
    create_self8 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt8 = cur;
    create_m8 = ATTR(self,mark);
    create_branch8 = 1;
    L82=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L82==NULL) FATAL("Unable to allocate more memory");
    ((OB)L82)->header.tag=AM_CURSOR_POS_tag;
    L80 = ((AM_CURSOR_POS) L82);
    r8 = L80;
    SATTR(r8,stmt,create_curr_stmt8);
    SATTR(r8,branch,create_branch8);
    SATTR(r8,mark,create_m8);
    ret_val9 = r8;
    push_e8 = ret_val9;
    SATTR(push_self8,s,FLISTA1475379054(ATTR(push_self8,s), push_e8));
    ret_val = ((dAM) ATTR(((AM_IF_STMT) cur),test1));
    return ret_val;
   }
   L84 = (ATTR(((AM_IF_STMT) cur),if_true)==((dAM_STMT) NULL));
   L851_=!(L84); 
   if (L851_) {
    L861_=(branch1)<(2); 
    L83 = L861_;
   } else {
    L83 = FALSE;
   }
   if (L83) {
    push_self9 = ATTR(self,stack1);
    create_self9 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt9 = cur;
    create_m9 = ATTR(self,mark);
    create_branch9 = 2;
    L88=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L88==NULL) FATAL("Unable to allocate more memory");
    ((OB)L88)->header.tag=AM_CURSOR_POS_tag;
    L87 = ((AM_CURSOR_POS) L88);
    r9 = L87;
    SATTR(r9,stmt,create_curr_stmt9);
    SATTR(r9,branch,create_branch9);
    SATTR(r9,mark,create_m9);
    ret_val10 = r9;
    push_e9 = ret_val10;
    SATTR(push_self9,s,FLISTA1475379054(ATTR(push_self9,s), push_e9));
    ret_val = ((dAM) ATTR(((AM_IF_STMT) cur),if_true));
    return ret_val;
   }
   L90 = (ATTR(((AM_IF_STMT) cur),if_false)==((dAM_STMT) NULL));
   L921_=!(L90); 
   if (L921_) {
    L931_=(branch1)<(3); 
    L89 = L931_;
   } else {
    L89 = FALSE;
   }
   if (L89) {
    push_self10 = ATTR(self,stack1);
    create_self10 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt10 = cur;
    create_m10 = ATTR(self,mark);
    create_branch10 = 3;
    L95=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L95==NULL) FATAL("Unable to allocate more memory");
    ((OB)L95)->header.tag=AM_CURSOR_POS_tag;
    L94 = ((AM_CURSOR_POS) L95);
    r10 = L94;
    SATTR(r10,stmt,create_curr_stmt10);
    SATTR(r10,branch,create_branch10);
    SATTR(r10,mark,create_m10);
    ret_val11 = r10;
    push_e10 = ret_val11;
    SATTR(push_self10,s,FLISTA1475379054(ATTR(push_self10,s), push_e10));
    ret_val = ((dAM) ATTR(((AM_IF_STMT) cur),if_false));
    return ret_val;
   } break;
  case AM_LOOP_STMT_tag:
   L97 = (ATTR(((AM_LOOP_STMT) cur),init)==((dAM_STMT) NULL));
   L981_=!(L97); 
   if (L981_) {
    L991_=(branch1)<(1); 
    L96 = L991_;
   } else {
    L96 = FALSE;
   }
   if (L96) {
    push_self11 = ATTR(self,stack1);
    create_self11 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt11 = cur;
    create_m11 = ATTR(self,mark);
    create_branch11 = 1;
    L102=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L102==NULL) FATAL("Unable to allocate more memory");
    ((OB)L102)->header.tag=AM_CURSOR_POS_tag;
    L100 = ((AM_CURSOR_POS) L102);
    r11 = L100;
    SATTR(r11,stmt,create_curr_stmt11);
    SATTR(r11,branch,create_branch11);
    SATTR(r11,mark,create_m11);
    ret_val12 = r11;
    push_e11 = ret_val12;
    SATTR(push_self11,s,FLISTA1475379054(ATTR(push_self11,s), push_e11));
    ret_val = ((dAM) ATTR(((AM_LOOP_STMT) cur),init));
    return ret_val;
   }
   L104 = (ATTR(((AM_LOOP_STMT) cur),body)==((dAM_STMT) NULL));
   L1051_=!(L104); 
   if (L1051_) {
    L1061_=(branch1)<(2); 
    L103 = L1061_;
   } else {
    L103 = FALSE;
   }
   if (L103) {
    push_self12 = ATTR(self,stack1);
    create_self12 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt12 = cur;
    create_m12 = ATTR(self,mark);
    create_branch12 = 2;
    L108=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L108==NULL) FATAL("Unable to allocate more memory");
    ((OB)L108)->header.tag=AM_CURSOR_POS_tag;
    L107 = ((AM_CURSOR_POS) L108);
    r12 = L107;
    SATTR(r12,stmt,create_curr_stmt12);
    SATTR(r12,branch,create_branch12);
    SATTR(r12,mark,create_m12);
    ret_val13 = r12;
    push_e12 = ret_val13;
    SATTR(push_self12,s,FLISTA1475379054(ATTR(push_self12,s), push_e12));
    L109 = ATTR(self,loops);
    L1101_=INTPLUS(L109,1); 
    SATTR(self,loops,L1101_);
    ret_val = ((dAM) ATTR(((AM_LOOP_STMT) cur),body));
    return ret_val;
   } break;
  case AM_RETURN_STMT_tag:
   L112 = (ATTR(((AM_RETURN_STMT) cur),val1)==((dAM_EXPR) NULL));
   L1131_=!(L112); 
   if (L1131_) {
    L1141_=(branch1)<(1); 
    L111 = L1141_;
   } else {
    L111 = FALSE;
   }
   if (L111) {
    push_self13 = ATTR(self,stack1);
    create_self13 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt13 = cur;
    create_m13 = ATTR(self,mark);
    create_branch13 = 1;
    L116=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L116==NULL) FATAL("Unable to allocate more memory");
    ((OB)L116)->header.tag=AM_CURSOR_POS_tag;
    L115 = ((AM_CURSOR_POS) L116);
    r13 = L115;
    SATTR(r13,stmt,create_curr_stmt13);
    SATTR(r13,branch,create_branch13);
    SATTR(r13,mark,create_m13);
    ret_val14 = r13;
    push_e13 = ret_val14;
    SATTR(push_self13,s,FLISTA1475379054(ATTR(push_self13,s), push_e13));
    ret_val = ((dAM) ATTR(((AM_RETURN_STMT) cur),val1));
    return ret_val;
   } break;
  case AM_YIELD_STMT_tag:
   L118 = (ATTR(((AM_YIELD_STMT) cur),val1)==((dAM_EXPR) NULL));
   L1191_=!(L118); 
   if (L1191_) {
    L1201_=(branch1)<(1); 
    L117 = L1201_;
   } else {
    L117 = FALSE;
   }
   if (L117) {
    push_self14 = ATTR(self,stack1);
    create_self14 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt14 = cur;
    create_m14 = ATTR(self,mark);
    create_branch14 = 1;
    L123=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L123==NULL) FATAL("Unable to allocate more memory");
    ((OB)L123)->header.tag=AM_CURSOR_POS_tag;
    L122 = ((AM_CURSOR_POS) L123);
    r14 = L122;
    SATTR(r14,stmt,create_curr_stmt14);
    SATTR(r14,branch,create_branch14);
    SATTR(r14,mark,create_m14);
    ret_val15 = r14;
    push_e14 = ret_val15;
    SATTR(push_self14,s,FLISTA1475379054(ATTR(push_self14,s), push_e14));
    ret_val = ((dAM) ATTR(((AM_YIELD_STMT) cur),val1));
    return ret_val;
   } break;
  case AM_CASE_STMT_tag:
   L125 = (ATTR(((AM_CASE_STMT) cur),test1)==((dAM_EXPR) NULL));
   L1261_=!(L125); 
   if (L1261_) {
    L1271_=(branch1)<(1); 
    L124 = L1271_;
   } else {
    L124 = FALSE;
   }
   if (L124) {
    push_self15 = ATTR(self,stack1);
    create_self15 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt15 = cur;
    create_m15 = ATTR(self,mark);
    create_branch15 = 1;
    L129=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L129==NULL) FATAL("Unable to allocate more memory");
    ((OB)L129)->header.tag=AM_CURSOR_POS_tag;
    L128 = ((AM_CURSOR_POS) L129);
    r15 = L128;
    SATTR(r15,stmt,create_curr_stmt15);
    SATTR(r15,branch,create_branch15);
    SATTR(r15,mark,create_m15);
    ret_val16 = r15;
    push_e15 = ret_val16;
    SATTR(push_self15,s,FLISTA1475379054(ATTR(push_self15,s), push_e15));
    ret_val = ((dAM) ATTR(((AM_CASE_STMT) cur),test1));
    return ret_val;
   }
   L130 = FLISTd355912233(ATTR(((AM_CASE_STMT) cur),stmts));
   L1321_=(0)<(L130); 
   if (L1321_) {
    {
     struct FLISTd739102917_frame_struct other1_0;
FLISTd739102917_frame noname1 = &other1_0;
     L11 = ATTR(((AM_CASE_STMT) cur),stmts);
     noname1->self = L11;
     noname1->state = 0;
     while (1) {
      L133 = FLISTd739102917(noname1);
      if (noname1->state == -1) {
       goto after_loop;
      }
      a = L133;
      aget_self = ATTR(((AM_CASE_STMT) cur),stmts);
      aget_ind = a;
      L1351_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self,aget_ind); 
      ret_val17 = L1351_;
      L136 = (ret_val17==((dAM_STMT) NULL));
      L1371_=!(L136); 
      if (L1371_) {
       L1381_=INTPLUS(a,2); 
       L139 = L1381_;
       L1401_=(branch1)<(L139); 
       L134 = L1401_;
      } else {
       L134 = FALSE;
      }
      if (L134) {
       push_self16 = ATTR(self,stack1);
       create_self16 = ((AM_CURSOR_POS) NULL);
       create_curr_stmt16 = cur;
       create_m16 = ATTR(self,mark);
       L1421_=INTPLUS(a,2); 
       create_branch16 = L1421_;
       L144=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
       if (L144==NULL) FATAL("Unable to allocate more memory");
       ((OB)L144)->header.tag=AM_CURSOR_POS_tag;
       L143 = ((AM_CURSOR_POS) L144);
       r16 = L143;
       SATTR(r16,stmt,create_curr_stmt16);
       SATTR(r16,branch,create_branch16);
       SATTR(r16,mark,create_m16);
       ret_val18 = r16;
       push_e16 = ret_val18;
       SATTR(push_self16,s,FLISTA1475379054(ATTR(push_self16,s), push_e16));
       aget_self1 = ATTR(((AM_CASE_STMT) cur),stmts);
       aget_ind1 = a;
       L1451_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self1,aget_ind1); 
       ret_val19 = L1451_;
       ret_val = ((dAM) ret_val19);
       return ret_val;
      }
     }
    }
    after_loop: ;
   }
   L147 = (ATTR(((AM_CASE_STMT) cur),else_stmts)==((dAM_STMT) NULL));
   L1481_=!(L147); 
   if (L1481_) {
    L149 = FLISTd355912233(ATTR(((AM_CASE_STMT) cur),stmts));
    L1501_=INTPLUS(L149,2); 
    L152 = L1501_;
    L1531_=(branch1)<(L152); 
    L146 = L1531_;
   } else {
    L146 = FALSE;
   }
   if (L146) {
    push_self17 = ATTR(self,stack1);
    create_self17 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt17 = cur;
    create_m17 = ATTR(self,mark);
    L154 = FLISTd355912233(ATTR(((AM_CASE_STMT) cur),stmts));
    L1551_=INTPLUS(L154,2); 
    create_branch17 = L1551_;
    L157=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L157==NULL) FATAL("Unable to allocate more memory");
    ((OB)L157)->header.tag=AM_CURSOR_POS_tag;
    L156 = ((AM_CURSOR_POS) L157);
    r17 = L156;
    SATTR(r17,stmt,create_curr_stmt17);
    SATTR(r17,branch,create_branch17);
    SATTR(r17,mark,create_m17);
    ret_val20 = r17;
    push_e17 = ret_val20;
    SATTR(push_self17,s,FLISTA1475379054(ATTR(push_self17,s), push_e17));
    ret_val = ((dAM) ATTR(((AM_CASE_STMT) cur),else_stmts));
    return ret_val;
   } break;
  case AM_TYPECASE_STMT_tag:
   L159 = (ATTR(((AM_TYPECASE_STMT) cur),test1)==((AM_LOCAL_EXPR) NULL));
   L1601_=!(L159); 
   if (L1601_) {
    L1621_=(branch1)<(1); 
    L158 = L1621_;
   } else {
    L158 = FALSE;
   }
   if (L158) {
    push_self18 = ATTR(self,stack1);
    create_self18 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt18 = cur;
    create_m18 = ATTR(self,mark);
    create_branch18 = 1;
    L164=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L164==NULL) FATAL("Unable to allocate more memory");
    ((OB)L164)->header.tag=AM_CURSOR_POS_tag;
    L163 = ((AM_CURSOR_POS) L164);
    r18 = L163;
    SATTR(r18,stmt,create_curr_stmt18);
    SATTR(r18,branch,create_branch18);
    SATTR(r18,mark,create_m18);
    ret_val21 = r18;
    push_e18 = ret_val21;
    SATTR(push_self18,s,FLISTA1475379054(ATTR(push_self18,s), push_e18));
    ret_val = ((dAM) ATTR(((AM_TYPECASE_STMT) cur),test1));
    return ret_val;
   }
   L165 = FLISTd355912233(ATTR(((AM_TYPECASE_STMT) cur),stmts));
   L1661_=(0)<(L165); 
   if (L1661_) {
    {
     struct FLISTd739102917_frame_struct other2_0;
FLISTd739102917_frame noname2 = &other2_0;
     L21 = ATTR(((AM_TYPECASE_STMT) cur),stmts);
     noname2->self = L21;
     noname2->state = 0;
     while (1) {
      L167 = FLISTd739102917(noname2);
      if (noname2->state == -1) {
       goto after_loop1;
      }
      a11 = L167;
      aget_self2 = ATTR(((AM_TYPECASE_STMT) cur),stmts);
      aget_ind2 = a11;
      L1691_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self2,aget_ind2); 
      ret_val22 = L1691_;
      L170 = (ret_val22==((dAM_STMT) NULL));
      L1721_=!(L170); 
      if (L1721_) {
       L1731_=INTPLUS(a11,2); 
       L174 = L1731_;
       L1751_=(branch1)<(L174); 
       L168 = L1751_;
      } else {
       L168 = FALSE;
      }
      if (L168) {
       push_self19 = ATTR(self,stack1);
       create_self19 = ((AM_CURSOR_POS) NULL);
       create_curr_stmt19 = cur;
       create_m19 = ATTR(self,mark);
       L1761_=INTPLUS(a11,2); 
       create_branch19 = L1761_;
       L178=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
       if (L178==NULL) FATAL("Unable to allocate more memory");
       ((OB)L178)->header.tag=AM_CURSOR_POS_tag;
       L177 = ((AM_CURSOR_POS) L178);
       r19 = L177;
       SATTR(r19,stmt,create_curr_stmt19);
       SATTR(r19,branch,create_branch19);
       SATTR(r19,mark,create_m19);
       ret_val23 = r19;
       push_e19 = ret_val23;
       SATTR(push_self19,s,FLISTA1475379054(ATTR(push_self19,s), push_e19));
       aget_self3 = ATTR(((AM_TYPECASE_STMT) cur),stmts);
       aget_ind3 = a11;
       L1791_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self3,aget_ind3); 
       ret_val24 = L1791_;
       ret_val = ((dAM) ret_val24);
       return ret_val;
      }
     }
    }
    after_loop1: ;
   }
   L182 = (ATTR(((AM_TYPECASE_STMT) cur),else_stmts)==((dAM_STMT) NULL));
   L1831_=!(L182); 
   if (L1831_) {
    L184 = FLISTd355912233(ATTR(((AM_TYPECASE_STMT) cur),stmts));
    L1851_=INTPLUS(L184,2); 
    L186 = L1851_;
    L1871_=(branch1)<(L186); 
    L180 = L1871_;
   } else {
    L180 = FALSE;
   }
   if (L180) {
    push_self20 = ATTR(self,stack1);
    create_self20 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt20 = cur;
    create_m20 = ATTR(self,mark);
    L188 = FLISTd355912233(ATTR(((AM_TYPECASE_STMT) cur),stmts));
    L1891_=INTPLUS(L188,2); 
    create_branch20 = L1891_;
    L192=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L192==NULL) FATAL("Unable to allocate more memory");
    ((OB)L192)->header.tag=AM_CURSOR_POS_tag;
    L190 = ((AM_CURSOR_POS) L192);
    r20 = L190;
    SATTR(r20,stmt,create_curr_stmt20);
    SATTR(r20,branch,create_branch20);
    SATTR(r20,mark,create_m20);
    ret_val25 = r20;
    push_e20 = ret_val25;
    SATTR(push_self20,s,FLISTA1475379054(ATTR(push_self20,s), push_e20));
    ret_val = ((dAM) ATTR(((AM_TYPECASE_STMT) cur),else_stmts));
    return ret_val;
   } break;
  case AM_PRE_STMT_tag:
   L195 = (ATTR(((AM_PRE_STMT) cur),test1)==((dAM_EXPR) NULL));
   L1961_=!(L195); 
   if (L1961_) {
    L1971_=(branch1)<(1); 
    L194 = L1971_;
   } else {
    L194 = FALSE;
   }
   if (L194) {
    L198 = ATTR(self,ignore_pre);
    L1991_=!(L198); 
    L193 = L1991_;
   } else {
    L193 = FALSE;
   }
   if (L193) {
    push_self21 = ATTR(self,stack1);
    create_self21 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt21 = cur;
    create_m21 = ATTR(self,mark);
    create_branch21 = 1;
    L201=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L201==NULL) FATAL("Unable to allocate more memory");
    ((OB)L201)->header.tag=AM_CURSOR_POS_tag;
    L200 = ((AM_CURSOR_POS) L201);
    r21 = L200;
    SATTR(r21,stmt,create_curr_stmt21);
    SATTR(r21,branch,create_branch21);
    SATTR(r21,mark,create_m21);
    ret_val26 = r21;
    push_e21 = ret_val26;
    SATTR(push_self21,s,FLISTA1475379054(ATTR(push_self21,s), push_e21));
    ret_val = ((dAM) ATTR(((AM_PRE_STMT) cur),test1));
    return ret_val;
   } break;
  case AM_POST_STMT_tag:
   L204 = (ATTR(((AM_POST_STMT) cur),test1)==((dAM_EXPR) NULL));
   L2051_=!(L204); 
   if (L2051_) {
    L2061_=(branch1)<(1); 
    L203 = L2061_;
   } else {
    L203 = FALSE;
   }
   if (L203) {
    L207 = ATTR(self,ignore_post);
    L2081_=!(L207); 
    L202 = L2081_;
   } else {
    L202 = FALSE;
   }
   if (L202) {
    push_self22 = ATTR(self,stack1);
    create_self22 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt22 = cur;
    create_m22 = ATTR(self,mark);
    create_branch22 = 1;
    L210=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L210==NULL) FATAL("Unable to allocate more memory");
    ((OB)L210)->header.tag=AM_CURSOR_POS_tag;
    L209 = ((AM_CURSOR_POS) L210);
    r22 = L209;
    SATTR(r22,stmt,create_curr_stmt22);
    SATTR(r22,branch,create_branch22);
    SATTR(r22,mark,create_m22);
    ret_val27 = r22;
    push_e22 = ret_val27;
    SATTR(push_self22,s,FLISTA1475379054(ATTR(push_self22,s), push_e22));
    ret_val = ((dAM) ATTR(((AM_POST_STMT) cur),test1));
    return ret_val;
   } break;
  case AM_INITIAL_STMT_tag:
   L212 = (ATTR(((AM_INITIAL_STMT) cur),stmts)==((dAM_STMT) NULL));
   L2131_=!(L212); 
   if (L2131_) {
    L2141_=(branch1)<(1); 
    L211 = L2141_;
   } else {
    L211 = FALSE;
   }
   if (L211) {
    push_self23 = ATTR(self,stack1);
    create_self23 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt23 = cur;
    create_m23 = ATTR(self,mark);
    create_branch23 = 1;
    L216=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L216==NULL) FATAL("Unable to allocate more memory");
    ((OB)L216)->header.tag=AM_CURSOR_POS_tag;
    L215 = ((AM_CURSOR_POS) L216);
    r23 = L215;
    SATTR(r23,stmt,create_curr_stmt23);
    SATTR(r23,branch,create_branch23);
    SATTR(r23,mark,create_m23);
    ret_val28 = r23;
    push_e23 = ret_val28;
    SATTR(push_self23,s,FLISTA1475379054(ATTR(push_self23,s), push_e23));
    ret_val = ((dAM) ATTR(((AM_INITIAL_STMT) cur),stmts));
    return ret_val;
   } break;
  case AM_ASSERT_STMT_tag:
   L219 = (ATTR(((AM_ASSERT_STMT) cur),test1)==((dAM_EXPR) NULL));
   L2201_=!(L219); 
   if (L2201_) {
    L2221_=(branch1)<(1); 
    L218 = L2221_;
   } else {
    L218 = FALSE;
   }
   if (L218) {
    L223 = ATTR(self,ignore_assert);
    L2241_=!(L223); 
    L217 = L2241_;
   } else {
    L217 = FALSE;
   }
   if (L217) {
    push_self24 = ATTR(self,stack1);
    create_self24 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt24 = cur;
    create_m24 = ATTR(self,mark);
    create_branch24 = 1;
    L226=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L226==NULL) FATAL("Unable to allocate more memory");
    ((OB)L226)->header.tag=AM_CURSOR_POS_tag;
    L225 = ((AM_CURSOR_POS) L226);
    r24 = L225;
    SATTR(r24,stmt,create_curr_stmt24);
    SATTR(r24,branch,create_branch24);
    SATTR(r24,mark,create_m24);
    ret_val29 = r24;
    push_e24 = ret_val29;
    SATTR(push_self24,s,FLISTA1475379054(ATTR(push_self24,s), push_e24));
    ret_val = ((dAM) ATTR(((AM_ASSERT_STMT) cur),test1));
    return ret_val;
   } break;
  case AM_PROTECT_STMT_tag:
   L228 = (ATTR(((AM_PROTECT_STMT) cur),body)==((dAM_STMT) NULL));
   L2291_=!(L228); 
   if (L2291_) {
    L2301_=(branch1)<(1); 
    L227 = L2301_;
   } else {
    L227 = FALSE;
   }
   if (L227) {
    push_self25 = ATTR(self,stack1);
    create_self25 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt25 = cur;
    create_m25 = ATTR(self,mark);
    create_branch25 = 1;
    L232=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L232==NULL) FATAL("Unable to allocate more memory");
    ((OB)L232)->header.tag=AM_CURSOR_POS_tag;
    L231 = ((AM_CURSOR_POS) L232);
    r25 = L231;
    SATTR(r25,stmt,create_curr_stmt25);
    SATTR(r25,branch,create_branch25);
    SATTR(r25,mark,create_m25);
    ret_val30 = r25;
    push_e25 = ret_val30;
    SATTR(push_self25,s,FLISTA1475379054(ATTR(push_self25,s), push_e25));
    ret_val = ((dAM) ATTR(((AM_PROTECT_STMT) cur),body));
    return ret_val;
   }
   L233 = FLISTd355912233(ATTR(((AM_PROTECT_STMT) cur),stmts));
   L2341_=(0)<(L233); 
   if (L2341_) {
    {
     struct FLISTd739102917_frame_struct other3_0;
FLISTd739102917_frame noname3 = &other3_0;
     L31 = ATTR(((AM_PROTECT_STMT) cur),stmts);
     noname3->self = L31;
     noname3->state = 0;
     while (1) {
      L235 = FLISTd739102917(noname3);
      if (noname3->state == -1) {
       goto after_loop2;
      }
      a21 = L235;
      aget_self4 = ATTR(((AM_PROTECT_STMT) cur),stmts);
      aget_ind4 = a21;
      L2371_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self4,aget_ind4); 
      ret_val31 = L2371_;
      L238 = (ret_val31==((dAM_STMT) NULL));
      L2391_=!(L238); 
      if (L2391_) {
       L2401_=INTPLUS(a21,2); 
       L241 = L2401_;
       L2421_=(branch1)<(L241); 
       L236 = L2421_;
      } else {
       L236 = FALSE;
      }
      if (L236) {
       push_self26 = ATTR(self,stack1);
       create_self26 = ((AM_CURSOR_POS) NULL);
       create_curr_stmt26 = cur;
       create_m26 = ATTR(self,mark);
       L2431_=INTPLUS(a21,2); 
       create_branch26 = L2431_;
       L245=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
       if (L245==NULL) FATAL("Unable to allocate more memory");
       ((OB)L245)->header.tag=AM_CURSOR_POS_tag;
       L244 = ((AM_CURSOR_POS) L245);
       r26 = L244;
       SATTR(r26,stmt,create_curr_stmt26);
       SATTR(r26,branch,create_branch26);
       SATTR(r26,mark,create_m26);
       ret_val32 = r26;
       push_e26 = ret_val32;
       SATTR(push_self26,s,FLISTA1475379054(ATTR(push_self26,s), push_e26));
       aget_self5 = ATTR(((AM_PROTECT_STMT) cur),stmts);
       aget_ind5 = a21;
       L2461_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self5,aget_ind5); 
       ret_val33 = L2461_;
       ret_val = ((dAM) ret_val33);
       return ret_val;
      }
     }
    }
    after_loop2: ;
   }
   L248 = (ATTR(((AM_PROTECT_STMT) cur),else_stmts)==((dAM_STMT) NULL));
   L2491_=!(L248); 
   if (L2491_) {
    L250 = FLISTd355912233(ATTR(((AM_PROTECT_STMT) cur),stmts));
    L2521_=INTPLUS(L250,2); 
    L253 = L2521_;
    L2541_=(branch1)<(L253); 
    L247 = L2541_;
   } else {
    L247 = FALSE;
   }
   if (L247) {
    push_self27 = ATTR(self,stack1);
    create_self27 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt27 = cur;
    create_m27 = ATTR(self,mark);
    L255 = FLISTd355912233(ATTR(((AM_PROTECT_STMT) cur),stmts));
    L2561_=INTPLUS(L255,2); 
    create_branch27 = L2561_;
    L258=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L258==NULL) FATAL("Unable to allocate more memory");
    ((OB)L258)->header.tag=AM_CURSOR_POS_tag;
    L257 = ((AM_CURSOR_POS) L258);
    r27 = L257;
    SATTR(r27,stmt,create_curr_stmt27);
    SATTR(r27,branch,create_branch27);
    SATTR(r27,mark,create_m27);
    ret_val34 = r27;
    push_e27 = ret_val34;
    SATTR(push_self27,s,FLISTA1475379054(ATTR(push_self27,s), push_e27));
    ret_val = ((dAM) ATTR(((AM_PROTECT_STMT) cur),else_stmts));
    return ret_val;
   } break;
  case AM_RAISE_STMT_tag:
   L260 = (ATTR(((AM_RAISE_STMT) cur),val1)==((dAM_EXPR) NULL));
   L2621_=!(L260); 
   if (L2621_) {
    L2631_=(branch1)<(1); 
    L259 = L2631_;
   } else {
    L259 = FALSE;
   }
   if (L259) {
    push_self28 = ATTR(self,stack1);
    create_self28 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt28 = cur;
    create_m28 = ATTR(self,mark);
    create_branch28 = 1;
    L265=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L265==NULL) FATAL("Unable to allocate more memory");
    ((OB)L265)->header.tag=AM_CURSOR_POS_tag;
    L264 = ((AM_CURSOR_POS) L265);
    r28 = L264;
    SATTR(r28,stmt,create_curr_stmt28);
    SATTR(r28,branch,create_branch28);
    SATTR(r28,mark,create_m28);
    ret_val35 = r28;
    push_e28 = ret_val35;
    SATTR(push_self28,s,FLISTA1475379054(ATTR(push_self28,s), push_e28));
    ret_val = ((dAM) ATTR(((AM_RAISE_STMT) cur),val1));
    return ret_val;
   } break;
  case AM_EXPR_STMT_tag:
   L267 = (ATTR(((AM_EXPR_STMT) cur),expr)==((dAM_EXPR) NULL));
   L2681_=!(L267); 
   if (L2681_) {
    L2691_=(branch1)<(1); 
    L266 = L2691_;
   } else {
    L266 = FALSE;
   }
   if (L266) {
    push_self29 = ATTR(self,stack1);
    create_self29 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt29 = cur;
    create_m29 = ATTR(self,mark);
    create_branch29 = 1;
    L271=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L271==NULL) FATAL("Unable to allocate more memory");
    ((OB)L271)->header.tag=AM_CURSOR_POS_tag;
    L270 = ((AM_CURSOR_POS) L271);
    r29 = L270;
    SATTR(r29,stmt,create_curr_stmt29);
    SATTR(r29,branch,create_branch29);
    SATTR(r29,mark,create_m29);
    ret_val36 = r29;
    push_e29 = ret_val36;
    SATTR(push_self29,s,FLISTA1475379054(ATTR(push_self29,s), push_e29));
    ret_val = ((dAM) ATTR(((AM_EXPR_STMT) cur),expr));
    return ret_val;
   } break;
  case AM_LOCK_STMT_tag:
   b1 = branch1;
   L272 = FLISTd1406591968(ATTR(((AM_LOCK_STMT) cur),guards));
   L2731_=(branch1)<(L272); 
   if (L2731_) {
    push_self30 = ATTR(self,stack1);
    create_self30 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt30 = cur;
    create_m30 = ATTR(self,mark);
    L2741_=INTPLUS(branch1,1); 
    create_branch30 = L2741_;
    L276=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L276==NULL) FATAL("Unable to allocate more memory");
    ((OB)L276)->header.tag=AM_CURSOR_POS_tag;
    L275 = ((AM_CURSOR_POS) L276);
    r30 = L275;
    SATTR(r30,stmt,create_curr_stmt30);
    SATTR(r30,branch,create_branch30);
    SATTR(r30,mark,create_m30);
    ret_val37 = r30;
    push_e30 = ret_val37;
    SATTR(push_self30,s,FLISTA1475379054(ATTR(push_self30,s), push_e30));
    aget_self6 = ATTR(((AM_LOCK_STMT) cur),guards);
    aget_ind6 = branch1;
    L2771_=(dAM_EXPR)ARR((FLISTdAM_EXPR)aget_self6,aget_ind6); 
    ret_val38 = L2771_;
    ret_val = ((dAM) ret_val38);
    return ret_val;
   }
   L278 = FLISTd1406591968(ATTR(((AM_LOCK_STMT) cur),guards));
   L2791_=INTMINUS(b1,L278); 
   b1 = L2791_;
   {
    struct FLISTA1857087356_frame_struct other4_0;
FLISTA1857087356_frame noname4 = &other4_0;
    L41 = ATTR(((AM_LOCK_STMT) cur),locks1);
    noname4->self = L41;
    noname4->state = 0;
    while (1) {
     L280 = FLISTA1857087356(noname4);
     if (noname4->state == -1) {
      goto after_loop3;
     }
     l = L280;
     L2811_=(l)==NULL?0:ASIZE((ARRAYdAM_EXPR)l); 
     L282 = L2811_;
     L2831_=(b1)<(L282); 
     if (L2831_) {
      push_self31 = ATTR(self,stack1);
      create_self31 = ((AM_CURSOR_POS) NULL);
      create_curr_stmt31 = cur;
      create_m31 = ATTR(self,mark);
      L2841_=INTPLUS(branch1,1); 
      create_branch31 = L2841_;
      L286=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
      if (L286==NULL) FATAL("Unable to allocate more memory");
      ((OB)L286)->header.tag=AM_CURSOR_POS_tag;
      L285 = ((AM_CURSOR_POS) L286);
      r31 = L285;
      SATTR(r31,stmt,create_curr_stmt31);
      SATTR(r31,branch,create_branch31);
      SATTR(r31,mark,create_m31);
      ret_val39 = r31;
      push_e31 = ret_val39;
      SATTR(push_self31,s,FLISTA1475379054(ATTR(push_self31,s), push_e31));
      L2871_=(dAM_EXPR)ARR((ARRAYdAM_EXPR)l,b1); 
      ret_val = ((dAM) L2871_);
      return ret_val;
     }
     L2881_=(l)==NULL?0:ASIZE((ARRAYdAM_EXPR)l); 
     L289 = L2881_;
     L2901_=INTMINUS(b1,L289); 
     b1 = L2901_;
    }
   }
   after_loop3: ;
   L291 = FLISTd355912233(ATTR(((AM_LOCK_STMT) cur),stmts));
   L2921_=(b1)<(L291); 
   if (L2921_) {
    aget_self7 = ATTR(((AM_LOCK_STMT) cur),stmts);
    aget_ind7 = b1;
    L2931_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self7,aget_ind7); 
    ret_val40 = L2931_;
    L294 = (ret_val40==((dAM_STMT) NULL));
    L2951_=!(L294); 
    if (L2951_) {
     push_self32 = ATTR(self,stack1);
     create_self32 = ((AM_CURSOR_POS) NULL);
     create_curr_stmt32 = cur;
     create_m32 = ATTR(self,mark);
     L2961_=INTPLUS(branch1,1); 
     create_branch32 = L2961_;
     L298=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
     if (L298==NULL) FATAL("Unable to allocate more memory");
     ((OB)L298)->header.tag=AM_CURSOR_POS_tag;
     L297 = ((AM_CURSOR_POS) L298);
     r32 = L297;
     SATTR(r32,stmt,create_curr_stmt32);
     SATTR(r32,branch,create_branch32);
     SATTR(r32,mark,create_m32);
     ret_val41 = r32;
     push_e32 = ret_val41;
     SATTR(push_self32,s,FLISTA1475379054(ATTR(push_self32,s), push_e32));
     aget_self8 = ATTR(((AM_LOCK_STMT) cur),stmts);
     aget_ind8 = b1;
     L2991_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self8,aget_ind8); 
     ret_val42 = L2991_;
     ret_val = ((dAM) ret_val42);
     return ret_val;
    }
    else {
     L3001_=INTPLUS(branch1,1); 
     branch1 = L3001_;
    }
   }
   L301 = FLISTd355912233(ATTR(((AM_LOCK_STMT) cur),stmts));
   L3021_=INTMINUS(b1,L301); 
   b1 = L3021_;
   L3041_=(b1)==(0); 
   if (L3041_) {
    L305 = (ATTR(((AM_LOCK_STMT) cur),else_stmts)==((dAM_STMT) NULL));
    L3061_=!(L305); 
    L303 = L3061_;
   } else {
    L303 = FALSE;
   }
   if (L303) {
    push_self33 = ATTR(self,stack1);
    create_self33 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt33 = cur;
    create_m33 = ATTR(self,mark);
    L3071_=INTPLUS(branch1,1); 
    create_branch33 = L3071_;
    L309=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L309==NULL) FATAL("Unable to allocate more memory");
    ((OB)L309)->header.tag=AM_CURSOR_POS_tag;
    L308 = ((AM_CURSOR_POS) L309);
    r33 = L308;
    SATTR(r33,stmt,create_curr_stmt33);
    SATTR(r33,branch,create_branch33);
    SATTR(r33,mark,create_m33);
    ret_val43 = r33;
    push_e33 = ret_val43;
    SATTR(push_self33,s,FLISTA1475379054(ATTR(push_self33,s), push_e33));
    ret_val = ((dAM) ATTR(((AM_LOCK_STMT) cur),else_stmts));
    return ret_val;
   } break;
  case AM_ATTACH_STMT_tag:
   L311 = (ATTR(((AM_ATTACH_STMT) cur),at)==((dAM_EXPR) NULL));
   L3121_=!(L311); 
   if (L3121_) {
    L3131_=(branch1)<(1); 
    L310 = L3131_;
   } else {
    L310 = FALSE;
   }
   if (L310) {
    push_self34 = ATTR(self,stack1);
    create_self34 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt34 = cur;
    create_m34 = ATTR(self,mark);
    create_branch34 = 1;
    L315=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L315==NULL) FATAL("Unable to allocate more memory");
    ((OB)L315)->header.tag=AM_CURSOR_POS_tag;
    L314 = ((AM_CURSOR_POS) L315);
    r34 = L314;
    SATTR(r34,stmt,create_curr_stmt34);
    SATTR(r34,branch,create_branch34);
    SATTR(r34,mark,create_m34);
    ret_val44 = r34;
    push_e34 = ret_val44;
    SATTR(push_self34,s,FLISTA1475379054(ATTR(push_self34,s), push_e34));
    ret_val = ((dAM) ATTR(((AM_ATTACH_STMT) cur),at));
    return ret_val;
   } break;
  case AM_FORK_STMT_tag:
   L317 = (ATTR(((AM_FORK_STMT) cur),at)==((dAM_EXPR) NULL));
   L3181_=!(L317); 
   if (L3181_) {
    L3191_=(branch1)<(1); 
    L316 = L3191_;
   } else {
    L316 = FALSE;
   }
   if (L316) {
    push_self35 = ATTR(self,stack1);
    create_self35 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt35 = cur;
    create_m35 = ATTR(self,mark);
    create_branch35 = 1;
    L322=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L322==NULL) FATAL("Unable to allocate more memory");
    ((OB)L322)->header.tag=AM_CURSOR_POS_tag;
    L320 = ((AM_CURSOR_POS) L322);
    r35 = L320;
    SATTR(r35,stmt,create_curr_stmt35);
    SATTR(r35,branch,create_branch35);
    SATTR(r35,mark,create_m35);
    ret_val45 = r35;
    push_e35 = ret_val45;
    SATTR(push_self35,s,FLISTA1475379054(ATTR(push_self35,s), push_e35));
    ret_val = ((dAM) ATTR(((AM_FORK_STMT) cur),at));
    return ret_val;
   }
   L324 = (ATTR(((AM_FORK_STMT) cur),body)==((dAM_STMT) NULL));
   L3251_=!(L324); 
   if (L3251_) {
    L3261_=(branch1)<(2); 
    L323 = L3261_;
   } else {
    L323 = FALSE;
   }
   if (L323) {
    push_self36 = ATTR(self,stack1);
    create_self36 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt36 = cur;
    create_m36 = ATTR(self,mark);
    create_branch36 = 2;
    L328=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L328==NULL) FATAL("Unable to allocate more memory");
    ((OB)L328)->header.tag=AM_CURSOR_POS_tag;
    L327 = ((AM_CURSOR_POS) L328);
    r36 = L327;
    SATTR(r36,stmt,create_curr_stmt36);
    SATTR(r36,branch,create_branch36);
    SATTR(r36,mark,create_m36);
    ret_val46 = r36;
    push_e36 = ret_val46;
    SATTR(push_self36,s,FLISTA1475379054(ATTR(push_self36,s), push_e36));
    ret_val = ((dAM) ATTR(((AM_FORK_STMT) cur),body));
    return ret_val;
   } break;
  case AM_PAR_STMT_tag:
   L330 = (ATTR(((AM_PAR_STMT) cur),body)==((dAM_STMT) NULL));
   L3321_=!(L330); 
   if (L3321_) {
    L3331_=(branch1)<(1); 
    L329 = L3331_;
   } else {
    L329 = FALSE;
   }
   if (L329) {
    push_self37 = ATTR(self,stack1);
    create_self37 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt37 = cur;
    create_m37 = ATTR(self,mark);
    create_branch37 = 1;
    L335=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L335==NULL) FATAL("Unable to allocate more memory");
    ((OB)L335)->header.tag=AM_CURSOR_POS_tag;
    L334 = ((AM_CURSOR_POS) L335);
    r37 = L334;
    SATTR(r37,stmt,create_curr_stmt37);
    SATTR(r37,branch,create_branch37);
    SATTR(r37,mark,create_m37);
    ret_val47 = r37;
    push_e37 = ret_val47;
    SATTR(push_self37,s,FLISTA1475379054(ATTR(push_self37,s), push_e37));
    ret_val = ((dAM) ATTR(((AM_PAR_STMT) cur),body));
    return ret_val;
   } break;
  case AM_SYNC_STMT_tag: break;
  case AM_UNLOCK_STMT_tag:
   L337 = (ATTR(((AM_UNLOCK_STMT) cur),lock_ob)==((dAM_EXPR) NULL));
   L3381_=!(L337); 
   if (L3381_) {
    L3391_=(branch1)<(1); 
    L336 = L3391_;
   } else {
    L336 = FALSE;
   }
   if (L336) {
    push_self38 = ATTR(self,stack1);
    create_self38 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt38 = cur;
    create_m38 = ATTR(self,mark);
    create_branch38 = 1;
    L341=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L341==NULL) FATAL("Unable to allocate more memory");
    ((OB)L341)->header.tag=AM_CURSOR_POS_tag;
    L340 = ((AM_CURSOR_POS) L341);
    r38 = L340;
    SATTR(r38,stmt,create_curr_stmt38);
    SATTR(r38,branch,create_branch38);
    SATTR(r38,mark,create_m38);
    ret_val48 = r38;
    push_e38 = ret_val48;
    SATTR(push_self38,s,FLISTA1475379054(ATTR(push_self38,s), push_e38));
    ret_val = ((dAM) ATTR(((AM_UNLOCK_STMT) cur),lock_ob));
    return ret_val;
   } break;
  case AM_WIT1996971603_tag:
   L343 = (ATTR(((AM_WIT1996971603) cur),objects1)==((ARRAYdAM_EXPR) NULL));
   L3441_=!(L343); 
   if (L3441_) {
    L345 = ATTR(((AM_WIT1996971603) cur),objects1);
    L3461_=(L345)==NULL?0:ASIZE((ARRAYdAM_EXPR)L345); 
    L347 = L3461_;
    L3481_=(branch1)<(L347); 
    L342 = L3481_;
   } else {
    L342 = FALSE;
   }
   if (L342) {
    push_self39 = ATTR(self,stack1);
    create_self39 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt39 = cur;
    create_m39 = ATTR(self,mark);
    L3491_=INTPLUS(branch1,1); 
    create_branch39 = L3491_;
    L351=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L351==NULL) FATAL("Unable to allocate more memory");
    ((OB)L351)->header.tag=AM_CURSOR_POS_tag;
    L350 = ((AM_CURSOR_POS) L351);
    r39 = L350;
    SATTR(r39,stmt,create_curr_stmt39);
    SATTR(r39,branch,create_branch39);
    SATTR(r39,mark,create_m39);
    ret_val49 = r39;
    push_e39 = ret_val49;
    SATTR(push_self39,s,FLISTA1475379054(ATTR(push_self39,s), push_e39));
    L352 = ATTR(((AM_WIT1996971603) cur),objects1);
    L3531_=(dAM_EXPR)ARR((ARRAYdAM_EXPR)L352,branch1); 
    ret_val = ((dAM) L3531_);
    return ret_val;
   }
   L355 = ATTR(((AM_WIT1996971603) cur),objects1);
   L3561_=(L355)==NULL?0:ASIZE((ARRAYdAM_EXPR)L355); 
   L357 = L3561_;
   L3581_=INTPLUS(L357,1); 
   L359 = L3581_;
   L3601_=(branch1)<(L359); 
   if (L3601_) {
    L361 = (ATTR(((AM_WIT1996971603) cur),near_part)==((dAM_STMT) NULL));
    L3621_=!(L361); 
    L354 = L3621_;
   } else {
    L354 = FALSE;
   }
   if (L354) {
    push_self40 = ATTR(self,stack1);
    create_self40 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt40 = cur;
    create_m40 = ATTR(self,mark);
    L363 = ATTR(((AM_WIT1996971603) cur),objects1);
    L3641_=(L363)==NULL?0:ASIZE((ARRAYdAM_EXPR)L363); 
    L365 = L3641_;
    L3661_=INTPLUS(L365,1); 
    create_branch40 = L3661_;
    L368=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L368==NULL) FATAL("Unable to allocate more memory");
    ((OB)L368)->header.tag=AM_CURSOR_POS_tag;
    L367 = ((AM_CURSOR_POS) L368);
    r40 = L367;
    SATTR(r40,stmt,create_curr_stmt40);
    SATTR(r40,branch,create_branch40);
    SATTR(r40,mark,create_m40);
    ret_val50 = r40;
    push_e40 = ret_val50;
    SATTR(push_self40,s,FLISTA1475379054(ATTR(push_self40,s), push_e40));
    ret_val = ((dAM) ATTR(((AM_WIT1996971603) cur),near_part));
    return ret_val;
   }
   L370 = ATTR(((AM_WIT1996971603) cur),objects1);
   L3711_=(L370)==NULL?0:ASIZE((ARRAYdAM_EXPR)L370); 
   L372 = L3711_;
   L3731_=INTPLUS(L372,2); 
   L374 = L3731_;
   L3751_=(branch1)<(L374); 
   if (L3751_) {
    L376 = (ATTR(((AM_WIT1996971603) cur),else_part)==((dAM_STMT) NULL));
    L3771_=!(L376); 
    L369 = L3771_;
   } else {
    L369 = FALSE;
   }
   if (L369) {
    push_self41 = ATTR(self,stack1);
    create_self41 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt41 = cur;
    create_m41 = ATTR(self,mark);
    L378 = ATTR(((AM_WIT1996971603) cur),objects1);
    L3791_=(L378)==NULL?0:ASIZE((ARRAYdAM_EXPR)L378); 
    L380 = L3791_;
    L3821_=INTPLUS(L380,2); 
    create_branch41 = L3821_;
    L384=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L384==NULL) FATAL("Unable to allocate more memory");
    ((OB)L384)->header.tag=AM_CURSOR_POS_tag;
    L383 = ((AM_CURSOR_POS) L384);
    r41 = L383;
    SATTR(r41,stmt,create_curr_stmt41);
    SATTR(r41,branch,create_branch41);
    SATTR(r41,mark,create_m41);
    ret_val51 = r41;
    push_e41 = ret_val51;
    SATTR(push_self41,s,FLISTA1475379054(ATTR(push_self41,s), push_e41));
    ret_val = ((dAM) ATTR(((AM_WIT1996971603) cur),else_part));
    return ret_val;
   } break;
  case AM_CALL_ARG_tag:
   L386 = (ATTR(((AM_CALL_ARG) cur),expr)==((dAM_EXPR) NULL));
   L3871_=!(L386); 
   if (L3871_) {
    L3881_=(branch1)<(1); 
    L385 = L3881_;
   } else {
    L385 = FALSE;
   }
   if (L385) {
    push_self42 = ATTR(self,stack1);
    create_self42 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt42 = cur;
    create_m42 = ATTR(self,mark);
    create_branch42 = 1;
    L390=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L390==NULL) FATAL("Unable to allocate more memory");
    ((OB)L390)->header.tag=AM_CURSOR_POS_tag;
    L389 = ((AM_CURSOR_POS) L390);
    r42 = L389;
    SATTR(r42,stmt,create_curr_stmt42);
    SATTR(r42,branch,create_branch42);
    SATTR(r42,mark,create_m42);
    ret_val52 = r42;
    push_e42 = ret_val52;
    SATTR(push_self42,s,FLISTA1475379054(ATTR(push_self42,s), push_e42));
    ret_val = ((dAM) ATTR(((AM_CALL_ARG) cur),expr));
    return ret_val;
   } break;
  case AM_ROU1916046290_tag:
   {
    BOOL f_L3921_ = TRUE;
    /* loop index variable */
    L131 = 0;
    L51 = ((AM_ROU1916046290) cur);
    L3921_br=L51==NULL?0:ASIZE((AM_ROU1916046290)L51); 
    while (1) {
     if(L131>=L3921_br)  goto after_loop4; 
     rL3921_=L131; 
     a31 = rL3921_;
     L3951_=(AM_CALL_ARG)ARR((AM_ROU1916046290)cur,a31); 
     L396 = (L3951_==((AM_CALL_ARG) NULL));
     L3971_=!(L396); 
     if (L3971_) {
      L3981_=INTPLUS(a31,1); 
      L399 = L3981_;
      L4001_=(branch1)<(L399); 
      L394 = L4001_;
     } else {
      L394 = FALSE;
     }
     if (L394) {
      push_self43 = ATTR(self,stack1);
      create_self43 = ((AM_CURSOR_POS) NULL);
      create_curr_stmt43 = cur;
      create_m43 = ATTR(self,mark);
      L4011_=INTPLUS(a31,1); 
      create_branch43 = L4011_;
      L403=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
      if (L403==NULL) FATAL("Unable to allocate more memory");
      ((OB)L403)->header.tag=AM_CURSOR_POS_tag;
      L402 = ((AM_CURSOR_POS) L403);
      r43 = L402;
      SATTR(r43,stmt,create_curr_stmt43);
      SATTR(r43,branch,create_branch43);
      SATTR(r43,mark,create_m43);
      ret_val53 = r43;
      push_e43 = ret_val53;
      SATTR(push_self43,s,FLISTA1475379054(ATTR(push_self43,s), push_e43));
      L4041_=(AM_CALL_ARG)ARR((AM_ROU1916046290)cur,a31); 
      ret_val = ((dAM) L4041_);
      return ret_val;
     }
     L4051_=INTPLUS(L131,1); 
     L131 = L4051_;
    }
   }
   after_loop4: ; break;
  case AM_EXT_CALL_EXPR_tag:
   {
    BOOL f_L4061_ = TRUE;
    /* loop index variable */
    L141 = 0;
    L61 = ((AM_EXT_CALL_EXPR) cur);
    L4061_br=L61==NULL?0:ASIZE((AM_EXT_CALL_EXPR)L61); 
    while (1) {
     if(L141>=L4061_br)  goto after_loop5; 
     rL4061_=L141; 
     a41 = rL4061_;
     L4091_=(AM_CALL_ARG)ARR((AM_EXT_CALL_EXPR)cur,a41); 
     L410 = (L4091_==((AM_CALL_ARG) NULL));
     L4111_=!(L410); 
     if (L4111_) {
      L4121_=INTPLUS(a41,1); 
      L413 = L4121_;
      L4141_=(branch1)<(L413); 
      L408 = L4141_;
     } else {
      L408 = FALSE;
     }
     if (L408) {
      push_self44 = ATTR(self,stack1);
      create_self44 = ((AM_CURSOR_POS) NULL);
      create_curr_stmt44 = cur;
      create_m44 = ATTR(self,mark);
      L4151_=INTPLUS(a41,1); 
      create_branch44 = L4151_;
      L417=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
      if (L417==NULL) FATAL("Unable to allocate more memory");
      ((OB)L417)->header.tag=AM_CURSOR_POS_tag;
      L416 = ((AM_CURSOR_POS) L417);
      r44 = L416;
      SATTR(r44,stmt,create_curr_stmt44);
      SATTR(r44,branch,create_branch44);
      SATTR(r44,mark,create_m44);
      ret_val54 = r44;
      push_e44 = ret_val54;
      SATTR(push_self44,s,FLISTA1475379054(ATTR(push_self44,s), push_e44));
      L4181_=(AM_CALL_ARG)ARR((AM_EXT_CALL_EXPR)cur,a41); 
      ret_val = ((dAM) L4181_);
      return ret_val;
     }
     L4191_=INTPLUS(L141,1); 
     L141 = L4191_;
    }
   }
   after_loop5: ; break;
  case AM_ITE1809135850_tag:
   L421 = (ATTR(((AM_ITE1809135850) cur),init)==((dAM_STMT) NULL));
   L4221_=!(L421); 
   if (L4221_) {
    L4231_=(branch1)<(1); 
    L420 = L4231_;
   } else {
    L420 = FALSE;
   }
   if (L420) {
    push_self45 = ATTR(self,stack1);
    create_self45 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt45 = cur;
    create_m45 = ATTR(self,mark);
    create_branch45 = 1;
    L425=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L425==NULL) FATAL("Unable to allocate more memory");
    ((OB)L425)->header.tag=AM_CURSOR_POS_tag;
    L424 = ((AM_CURSOR_POS) L425);
    r45 = L424;
    SATTR(r45,stmt,create_curr_stmt45);
    SATTR(r45,branch,create_branch45);
    SATTR(r45,mark,create_m45);
    ret_val55 = r45;
    push_e45 = ret_val55;
    SATTR(push_self45,s,FLISTA1475379054(ATTR(push_self45,s), push_e45));
    ret_val = ((dAM) ATTR(((AM_ITE1809135850) cur),init));
    return ret_val;
   }
   {
    BOOL f_L4261_ = TRUE;
    /* loop index variable */
    L151 = 0;
    L71 = ((AM_ITE1809135850) cur);
    L4261_br=L71==NULL?0:ASIZE((AM_ITE1809135850)L71); 
    while (1) {
     if(L151>=L4261_br)  goto after_loop6; 
     rL4261_=L151; 
     a5 = rL4261_;
     L4291_=(AM_CALL_ARG)ARR((AM_ITE1809135850)cur,a5); 
     L430 = (L4291_==((AM_CALL_ARG) NULL));
     L4311_=!(L430); 
     if (L4311_) {
      L4321_=INTPLUS(a5,2); 
      L433 = L4321_;
      L4341_=(branch1)<(L433); 
      L428 = L4341_;
     } else {
      L428 = FALSE;
     }
     if (L428) {
      push_self46 = ATTR(self,stack1);
      create_self46 = ((AM_CURSOR_POS) NULL);
      create_curr_stmt46 = cur;
      create_m46 = ATTR(self,mark);
      L4351_=INTPLUS(a5,2); 
      create_branch46 = L4351_;
      L437=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
      if (L437==NULL) FATAL("Unable to allocate more memory");
      ((OB)L437)->header.tag=AM_CURSOR_POS_tag;
      L436 = ((AM_CURSOR_POS) L437);
      r46 = L436;
      SATTR(r46,stmt,create_curr_stmt46);
      SATTR(r46,branch,create_branch46);
      SATTR(r46,mark,create_m46);
      ret_val56 = r46;
      push_e46 = ret_val56;
      SATTR(push_self46,s,FLISTA1475379054(ATTR(push_self46,s), push_e46));
      L4381_=(AM_CALL_ARG)ARR((AM_ITE1809135850)cur,a5); 
      ret_val = ((dAM) L4381_);
      return ret_val;
     }
     L4391_=INTPLUS(L151,1); 
     L151 = L4391_;
    }
   }
   after_loop6: ; break;
  case AM_BND260301329_tag:
   L441 = (ATTR(((AM_BND260301329) cur),br)==((dAM_EXPR) NULL));
   L4421_=!(L441); 
   if (L4421_) {
    L4431_=(branch1)<(1); 
    L440 = L4431_;
   } else {
    L440 = FALSE;
   }
   if (L440) {
    push_self47 = ATTR(self,stack1);
    create_self47 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt47 = cur;
    create_m47 = ATTR(self,mark);
    create_branch47 = 1;
    L445=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L445==NULL) FATAL("Unable to allocate more memory");
    ((OB)L445)->header.tag=AM_CURSOR_POS_tag;
    L444 = ((AM_CURSOR_POS) L445);
    r47 = L444;
    SATTR(r47,stmt,create_curr_stmt47);
    SATTR(r47,branch,create_branch47);
    SATTR(r47,mark,create_m47);
    ret_val57 = r47;
    push_e47 = ret_val57;
    SATTR(push_self47,s,FLISTA1475379054(ATTR(push_self47,s), push_e47));
    ret_val = ((dAM) ATTR(((AM_BND260301329) cur),br));
    return ret_val;
   }
   {
    BOOL f_L4461_ = TRUE;
    /* loop index variable */
    L161 = 0;
    L81 = ((AM_BND260301329) cur);
    L4461_br=L81==NULL?0:ASIZE((AM_BND260301329)L81); 
    while (1) {
     if(L161>=L4461_br)  goto after_loop7; 
     rL4461_=L161; 
     a6 = rL4461_;
     L4491_=(AM_CALL_ARG)ARR((AM_BND260301329)cur,a6); 
     L450 = (L4491_==((AM_CALL_ARG) NULL));
     L4521_=!(L450); 
     if (L4521_) {
      L4531_=INTPLUS(a6,2); 
      L454 = L4531_;
      L4551_=(branch1)<(L454); 
      L448 = L4551_;
     } else {
      L448 = FALSE;
     }
     if (L448) {
      push_self48 = ATTR(self,stack1);
      create_self48 = ((AM_CURSOR_POS) NULL);
      create_curr_stmt48 = cur;
      create_m48 = ATTR(self,mark);
      L4561_=INTPLUS(a6,2); 
      create_branch48 = L4561_;
      L458=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
      if (L458==NULL) FATAL("Unable to allocate more memory");
      ((OB)L458)->header.tag=AM_CURSOR_POS_tag;
      L457 = ((AM_CURSOR_POS) L458);
      r48 = L457;
      SATTR(r48,stmt,create_curr_stmt48);
      SATTR(r48,branch,create_branch48);
      SATTR(r48,mark,create_m48);
      ret_val58 = r48;
      push_e48 = ret_val58;
      SATTR(push_self48,s,FLISTA1475379054(ATTR(push_self48,s), push_e48));
      L4591_=(AM_CALL_ARG)ARR((AM_BND260301329)cur,a6); 
      ret_val = ((dAM) L4591_);
      return ret_val;
     }
     L4601_=INTPLUS(L161,1); 
     L161 = L4601_;
    }
   }
   after_loop7: ; break;
  case AM_BND367211769_tag:
   L463 = (ATTR(((AM_BND367211769) cur),bi)==((dAM_EXPR) NULL));
   L4641_=!(L463); 
   if (L4641_) {
    L4651_=(branch1)<(1); 
    L462 = L4651_;
   } else {
    L462 = FALSE;
   }
   if (L462) {
    push_self49 = ATTR(self,stack1);
    create_self49 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt49 = cur;
    create_m49 = ATTR(self,mark);
    create_branch49 = 1;
    L467=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L467==NULL) FATAL("Unable to allocate more memory");
    ((OB)L467)->header.tag=AM_CURSOR_POS_tag;
    L466 = ((AM_CURSOR_POS) L467);
    r49 = L466;
    SATTR(r49,stmt,create_curr_stmt49);
    SATTR(r49,branch,create_branch49);
    SATTR(r49,mark,create_m49);
    ret_val59 = r49;
    push_e49 = ret_val59;
    SATTR(push_self49,s,FLISTA1475379054(ATTR(push_self49,s), push_e49));
    ret_val = ((dAM) ATTR(((AM_BND367211769) cur),bi));
    return ret_val;
   }
   L469 = (ATTR(((AM_BND367211769) cur),init)==((dAM_STMT) NULL));
   L4701_=!(L469); 
   if (L4701_) {
    L4711_=(branch1)<(2); 
    L468 = L4711_;
   } else {
    L468 = FALSE;
   }
   if (L468) {
    push_self50 = ATTR(self,stack1);
    create_self50 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt50 = cur;
    create_m50 = ATTR(self,mark);
    create_branch50 = 2;
    L473=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L473==NULL) FATAL("Unable to allocate more memory");
    ((OB)L473)->header.tag=AM_CURSOR_POS_tag;
    L472 = ((AM_CURSOR_POS) L473);
    r50 = L472;
    SATTR(r50,stmt,create_curr_stmt50);
    SATTR(r50,branch,create_branch50);
    SATTR(r50,mark,create_m50);
    ret_val60 = r50;
    push_e50 = ret_val60;
    SATTR(push_self50,s,FLISTA1475379054(ATTR(push_self50,s), push_e50));
    ret_val = ((dAM) ATTR(((AM_BND367211769) cur),init));
    return ret_val;
   }
   {
    BOOL f_L4741_ = TRUE;
    /* loop index variable */
    L171 = 0;
    L91 = ((AM_BND367211769) cur);
    L4741_br=L91==NULL?0:ASIZE((AM_BND367211769)L91); 
    while (1) {
     if(L171>=L4741_br)  goto after_loop8; 
     rL4741_=L171; 
     a7 = rL4741_;
     L4771_=(AM_CALL_ARG)ARR((AM_BND367211769)cur,a7); 
     L478 = (L4771_==((AM_CALL_ARG) NULL));
     L4791_=!(L478); 
     if (L4791_) {
      L4801_=INTPLUS(a7,3); 
      L481 = L4801_;
      L4821_=(branch1)<(L481); 
      L476 = L4821_;
     } else {
      L476 = FALSE;
     }
     if (L476) {
      push_self51 = ATTR(self,stack1);
      create_self51 = ((AM_CURSOR_POS) NULL);
      create_curr_stmt51 = cur;
      create_m51 = ATTR(self,mark);
      L4831_=INTPLUS(a7,3); 
      create_branch51 = L4831_;
      L485=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
      if (L485==NULL) FATAL("Unable to allocate more memory");
      ((OB)L485)->header.tag=AM_CURSOR_POS_tag;
      L484 = ((AM_CURSOR_POS) L485);
      r51 = L484;
      SATTR(r51,stmt,create_curr_stmt51);
      SATTR(r51,branch,create_branch51);
      SATTR(r51,mark,create_m51);
      ret_val61 = r51;
      push_e51 = ret_val61;
      SATTR(push_self51,s,FLISTA1475379054(ATTR(push_self51,s), push_e51));
      L4861_=(AM_CALL_ARG)ARR((AM_BND367211769)cur,a7); 
      ret_val = ((dAM) L4861_);
      return ret_val;
     }
     L4871_=INTPLUS(L171,1); 
     L171 = L4871_;
    }
   }
   after_loop8: ; break;
  case AM_ARRAY_EXPR_tag:
   {
    BOOL f_L4881_ = TRUE;
    /* loop index variable */
    L181 = 0;
    L101 = ((AM_ARRAY_EXPR) cur);
    L4881_br=L101==NULL?0:ASIZE((AM_ARRAY_EXPR)L101); 
    while (1) {
     if(L181>=L4881_br)  goto after_loop9; 
     rL4881_=L181; 
     a8 = rL4881_;
     L4911_=(dAM_EXPR)ARR((AM_ARRAY_EXPR)cur,a8); 
     L492 = (L4911_==((dAM_EXPR) NULL));
     L4931_=!(L492); 
     if (L4931_) {
      L4941_=INTPLUS(a8,1); 
      L495 = L4941_;
      L4961_=(branch1)<(L495); 
      L490 = L4961_;
     } else {
      L490 = FALSE;
     }
     if (L490) {
      push_self52 = ATTR(self,stack1);
      create_self52 = ((AM_CURSOR_POS) NULL);
      create_curr_stmt52 = cur;
      create_m52 = ATTR(self,mark);
      L4971_=INTPLUS(a8,1); 
      create_branch52 = L4971_;
      L499=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
      if (L499==NULL) FATAL("Unable to allocate more memory");
      ((OB)L499)->header.tag=AM_CURSOR_POS_tag;
      L498 = ((AM_CURSOR_POS) L499);
      r52 = L498;
      SATTR(r52,stmt,create_curr_stmt52);
      SATTR(r52,branch,create_branch52);
      SATTR(r52,mark,create_m52);
      ret_val62 = r52;
      push_e52 = ret_val62;
      SATTR(push_self52,s,FLISTA1475379054(ATTR(push_self52,s), push_e52));
      L5001_=(dAM_EXPR)ARR((AM_ARRAY_EXPR)cur,a8); 
      ret_val = ((dAM) L5001_);
      return ret_val;
     }
     L5011_=INTPLUS(L181,1); 
     L181 = L5011_;
    }
   }
   after_loop9: ; break;
  case AM_BND1124877163_tag:
   {
    BOOL f_L5021_ = TRUE;
    /* loop index variable */
    L191 = 0;
    L121 = ((AM_BND1124877163) cur);
    L5021_br=L121==NULL?0:ASIZE((AM_BND1124877163)L121); 
    while (1) {
     if(L191>=L5021_br)  goto after_loop10; 
     rL5021_=L191; 
     a9 = rL5021_;
     L5051_=(AM_CALL_ARG)ARR((AM_BND1124877163)cur,a9); 
     L506 = (L5051_==((AM_CALL_ARG) NULL));
     L5071_=!(L506); 
     if (L5071_) {
      L5081_=INTPLUS(a9,1); 
      L509 = L5081_;
      L5101_=(branch1)<(L509); 
      L504 = L5101_;
     } else {
      L504 = FALSE;
     }
     if (L504) {
      push_self53 = ATTR(self,stack1);
      create_self53 = ((AM_CURSOR_POS) NULL);
      create_curr_stmt53 = cur;
      create_m53 = ATTR(self,mark);
      L5111_=INTPLUS(a9,1); 
      create_branch53 = L5111_;
      L513=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
      if (L513==NULL) FATAL("Unable to allocate more memory");
      ((OB)L513)->header.tag=AM_CURSOR_POS_tag;
      L512 = ((AM_CURSOR_POS) L513);
      r53 = L512;
      SATTR(r53,stmt,create_curr_stmt53);
      SATTR(r53,branch,create_branch53);
      SATTR(r53,mark,create_m53);
      ret_val63 = r53;
      push_e53 = ret_val63;
      SATTR(push_self53,s,FLISTA1475379054(ATTR(push_self53,s), push_e53));
      L5141_=(AM_CALL_ARG)ARR((AM_BND1124877163)cur,a9); 
      ret_val = ((dAM) L5141_);
      return ret_val;
     }
     L5151_=INTPLUS(L191,1); 
     L191 = L5151_;
    }
   }
   after_loop10: ; break;
  case AM_IF_EXPR_tag:
   L517 = (ATTR(((AM_IF_EXPR) cur),test1)==((dAM_EXPR) NULL));
   L5181_=!(L517); 
   if (L5181_) {
    L5191_=(branch1)<(1); 
    L516 = L5191_;
   } else {
    L516 = FALSE;
   }
   if (L516) {
    push_self54 = ATTR(self,stack1);
    create_self54 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt54 = cur;
    create_m54 = ATTR(self,mark);
    create_branch54 = 1;
    L522=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L522==NULL) FATAL("Unable to allocate more memory");
    ((OB)L522)->header.tag=AM_CURSOR_POS_tag;
    L520 = ((AM_CURSOR_POS) L522);
    r54 = L520;
    SATTR(r54,stmt,create_curr_stmt54);
    SATTR(r54,branch,create_branch54);
    SATTR(r54,mark,create_m54);
    ret_val64 = r54;
    push_e54 = ret_val64;
    SATTR(push_self54,s,FLISTA1475379054(ATTR(push_self54,s), push_e54));
    ret_val = ((dAM) ATTR(((AM_IF_EXPR) cur),test1));
    return ret_val;
   }
   L524 = (ATTR(((AM_IF_EXPR) cur),if_true)==((dAM_EXPR) NULL));
   L5251_=!(L524); 
   if (L5251_) {
    L5261_=(branch1)<(2); 
    L523 = L5261_;
   } else {
    L523 = FALSE;
   }
   if (L523) {
    push_self55 = ATTR(self,stack1);
    create_self55 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt55 = cur;
    create_m55 = ATTR(self,mark);
    create_branch55 = 2;
    L528=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L528==NULL) FATAL("Unable to allocate more memory");
    ((OB)L528)->header.tag=AM_CURSOR_POS_tag;
    L527 = ((AM_CURSOR_POS) L528);
    r55 = L527;
    SATTR(r55,stmt,create_curr_stmt55);
    SATTR(r55,branch,create_branch55);
    SATTR(r55,mark,create_m55);
    ret_val65 = r55;
    push_e55 = ret_val65;
    SATTR(push_self55,s,FLISTA1475379054(ATTR(push_self55,s), push_e55));
    ret_val = ((dAM) ATTR(((AM_IF_EXPR) cur),if_true));
    return ret_val;
   }
   L530 = (ATTR(((AM_IF_EXPR) cur),if_false)==((dAM_EXPR) NULL));
   L5321_=!(L530); 
   if (L5321_) {
    L5331_=(branch1)<(3); 
    L529 = L5331_;
   } else {
    L529 = FALSE;
   }
   if (L529) {
    push_self56 = ATTR(self,stack1);
    create_self56 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt56 = cur;
    create_m56 = ATTR(self,mark);
    create_branch56 = 3;
    L535=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L535==NULL) FATAL("Unable to allocate more memory");
    ((OB)L535)->header.tag=AM_CURSOR_POS_tag;
    L534 = ((AM_CURSOR_POS) L535);
    r56 = L534;
    SATTR(r56,stmt,create_curr_stmt56);
    SATTR(r56,branch,create_branch56);
    SATTR(r56,mark,create_m56);
    ret_val66 = r56;
    push_e56 = ret_val66;
    SATTR(push_self56,s,FLISTA1475379054(ATTR(push_self56,s), push_e56));
    ret_val = ((dAM) ATTR(((AM_IF_EXPR) cur),if_false));
    return ret_val;
   } break;
  case AM_NEW_EXPR_tag:
   L537 = (ATTR(((AM_NEW_EXPR) cur),asz)==((dAM_EXPR) NULL));
   L5381_=!(L537); 
   if (L5381_) {
    L5391_=(branch1)<(1); 
    L536 = L5391_;
   } else {
    L536 = FALSE;
   }
   if (L536) {
    push_self57 = ATTR(self,stack1);
    create_self57 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt57 = cur;
    create_m57 = ATTR(self,mark);
    create_branch57 = 1;
    L541=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L541==NULL) FATAL("Unable to allocate more memory");
    ((OB)L541)->header.tag=AM_CURSOR_POS_tag;
    L540 = ((AM_CURSOR_POS) L541);
    r57 = L540;
    SATTR(r57,stmt,create_curr_stmt57);
    SATTR(r57,branch,create_branch57);
    SATTR(r57,mark,create_m57);
    ret_val67 = r57;
    push_e57 = ret_val67;
    SATTR(push_self57,s,FLISTA1475379054(ATTR(push_self57,s), push_e57));
    ret_val = ((dAM) ATTR(((AM_NEW_EXPR) cur),asz));
    return ret_val;
   } break;
  case AM_IS_VOID_EXPR_tag:
   L543 = (ATTR(((AM_IS_VOID_EXPR) cur),arg)==((dAM_EXPR) NULL));
   L5441_=!(L543); 
   if (L5441_) {
    L5451_=(branch1)<(1); 
    L542 = L5451_;
   } else {
    L542 = FALSE;
   }
   if (L542) {
    push_self58 = ATTR(self,stack1);
    create_self58 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt58 = cur;
    create_m58 = ATTR(self,mark);
    create_branch58 = 1;
    L547=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L547==NULL) FATAL("Unable to allocate more memory");
    ((OB)L547)->header.tag=AM_CURSOR_POS_tag;
    L546 = ((AM_CURSOR_POS) L547);
    r58 = L546;
    SATTR(r58,stmt,create_curr_stmt58);
    SATTR(r58,branch,create_branch58);
    SATTR(r58,mark,create_m58);
    ret_val68 = r58;
    push_e58 = ret_val68;
    SATTR(push_self58,s,FLISTA1475379054(ATTR(push_self58,s), push_e58));
    ret_val = ((dAM) ATTR(((AM_IS_VOID_EXPR) cur),arg));
    return ret_val;
   } break;
  case AM_SHARED_EXPR_tag: break;
  case AM_ATTR_EXPR_tag:
   L549 = (ATTR(((AM_ATTR_EXPR) cur),ob)==((dAM_EXPR) NULL));
   L5501_=!(L549); 
   if (L5501_) {
    L5511_=(branch1)<(1); 
    L548 = L5511_;
   } else {
    L548 = FALSE;
   }
   if (L548) {
    push_self59 = ATTR(self,stack1);
    create_self59 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt59 = cur;
    create_m59 = ATTR(self,mark);
    create_branch59 = 1;
    L553=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L553==NULL) FATAL("Unable to allocate more memory");
    ((OB)L553)->header.tag=AM_CURSOR_POS_tag;
    L552 = ((AM_CURSOR_POS) L553);
    r59 = L552;
    SATTR(r59,stmt,create_curr_stmt59);
    SATTR(r59,branch,create_branch59);
    SATTR(r59,mark,create_m59);
    ret_val69 = r59;
    push_e59 = ret_val69;
    SATTR(push_self59,s,FLISTA1475379054(ATTR(push_self59,s), push_e59));
    ret_val = ((dAM) ATTR(((AM_ATTR_EXPR) cur),ob));
    return ret_val;
   } break;
  case AM_VAT319982528_tag:
   L555 = (ATTR(((AM_VAT319982528) cur),ob)==((dAM_EXPR) NULL));
   L5561_=!(L555); 
   if (L5561_) {
    L5571_=(branch1)<(1); 
    L554 = L5571_;
   } else {
    L554 = FALSE;
   }
   if (L554) {
    push_self60 = ATTR(self,stack1);
    create_self60 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt60 = cur;
    create_m60 = ATTR(self,mark);
    create_branch60 = 1;
    L559=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L559==NULL) FATAL("Unable to allocate more memory");
    ((OB)L559)->header.tag=AM_CURSOR_POS_tag;
    L558 = ((AM_CURSOR_POS) L559);
    r60 = L558;
    SATTR(r60,stmt,create_curr_stmt60);
    SATTR(r60,branch,create_branch60);
    SATTR(r60,mark,create_m60);
    ret_val70 = r60;
    push_e60 = ret_val70;
    SATTR(push_self60,s,FLISTA1475379054(ATTR(push_self60,s), push_e60));
    ret_val = ((dAM) ATTR(((AM_VAT319982528) cur),ob));
    return ret_val;
   }
   L561 = (ATTR(((AM_VAT319982528) cur),val1)==((dAM_EXPR) NULL));
   L5621_=!(L561); 
   if (L5621_) {
    L5631_=(branch1)<(2); 
    L560 = L5631_;
   } else {
    L560 = FALSE;
   }
   if (L560) {
    push_self61 = ATTR(self,stack1);
    create_self61 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt61 = cur;
    create_m61 = ATTR(self,mark);
    create_branch61 = 2;
    L565=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L565==NULL) FATAL("Unable to allocate more memory");
    ((OB)L565)->header.tag=AM_CURSOR_POS_tag;
    L564 = ((AM_CURSOR_POS) L565);
    r61 = L564;
    SATTR(r61,stmt,create_curr_stmt61);
    SATTR(r61,branch,create_branch61);
    SATTR(r61,mark,create_m61);
    ret_val71 = r61;
    push_e61 = ret_val71;
    SATTR(push_self61,s,FLISTA1475379054(ATTR(push_self61,s), push_e61));
    ret_val = ((dAM) ATTR(((AM_VAT319982528) cur),val1));
    return ret_val;
   } break;
  case AM_VAR744470097_tag:
   L567 = (ATTR(((AM_VAR744470097) cur),ob)==((dAM_EXPR) NULL));
   L5681_=!(L567); 
   if (L5681_) {
    L5691_=(branch1)<(1); 
    L566 = L5691_;
   } else {
    L566 = FALSE;
   }
   if (L566) {
    push_self62 = ATTR(self,stack1);
    create_self62 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt62 = cur;
    create_m62 = ATTR(self,mark);
    create_branch62 = 1;
    L571=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L571==NULL) FATAL("Unable to allocate more memory");
    ((OB)L571)->header.tag=AM_CURSOR_POS_tag;
    L570 = ((AM_CURSOR_POS) L571);
    r62 = L570;
    SATTR(r62,stmt,create_curr_stmt62);
    SATTR(r62,branch,create_branch62);
    SATTR(r62,mark,create_m62);
    ret_val72 = r62;
    push_e62 = ret_val72;
    SATTR(push_self62,s,FLISTA1475379054(ATTR(push_self62,s), push_e62));
    ret_val = ((dAM) ATTR(((AM_VAR744470097) cur),ob));
    return ret_val;
   }
   L573 = (ATTR(((AM_VAR744470097) cur),ind)==((dAM_EXPR) NULL));
   L5741_=!(L573); 
   if (L5741_) {
    L5751_=(branch1)<(2); 
    L572 = L5751_;
   } else {
    L572 = FALSE;
   }
   if (L572) {
    push_self63 = ATTR(self,stack1);
    create_self63 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt63 = cur;
    create_m63 = ATTR(self,mark);
    create_branch63 = 2;
    L577=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L577==NULL) FATAL("Unable to allocate more memory");
    ((OB)L577)->header.tag=AM_CURSOR_POS_tag;
    L576 = ((AM_CURSOR_POS) L577);
    r63 = L576;
    SATTR(r63,stmt,create_curr_stmt63);
    SATTR(r63,branch,create_branch63);
    SATTR(r63,mark,create_m63);
    ret_val73 = r63;
    push_e63 = ret_val73;
    SATTR(push_self63,s,FLISTA1475379054(ATTR(push_self63,s), push_e63));
    ret_val = ((dAM) ATTR(((AM_VAR744470097) cur),ind));
    return ret_val;
   }
   L579 = (ATTR(((AM_VAR744470097) cur),val1)==((dAM_EXPR) NULL));
   L5801_=!(L579); 
   if (L5801_) {
    L5821_=(branch1)<(3); 
    L578 = L5821_;
   } else {
    L578 = FALSE;
   }
   if (L578) {
    push_self64 = ATTR(self,stack1);
    create_self64 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt64 = cur;
    create_m64 = ATTR(self,mark);
    create_branch64 = 3;
    L584=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L584==NULL) FATAL("Unable to allocate more memory");
    ((OB)L584)->header.tag=AM_CURSOR_POS_tag;
    L583 = ((AM_CURSOR_POS) L584);
    r64 = L583;
    SATTR(r64,stmt,create_curr_stmt64);
    SATTR(r64,branch,create_branch64);
    SATTR(r64,mark,create_m64);
    ret_val74 = r64;
    push_e64 = ret_val74;
    SATTR(push_self64,s,FLISTA1475379054(ATTR(push_self64,s), push_e64));
    ret_val = ((dAM) ATTR(((AM_VAR744470097) cur),val1));
    return ret_val;
   } break;
  case AM_STMT_EXPR_tag:
   L586 = (ATTR(((AM_STMT_EXPR) cur),stmts)==((dAM_STMT) NULL));
   L5871_=!(L586); 
   if (L5871_) {
    L5881_=(branch1)<(1); 
    L585 = L5881_;
   } else {
    L585 = FALSE;
   }
   if (L585) {
    push_self65 = ATTR(self,stack1);
    create_self65 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt65 = cur;
    create_m65 = ATTR(self,mark);
    create_branch65 = 1;
    L590=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L590==NULL) FATAL("Unable to allocate more memory");
    ((OB)L590)->header.tag=AM_CURSOR_POS_tag;
    L589 = ((AM_CURSOR_POS) L590);
    r65 = L589;
    SATTR(r65,stmt,create_curr_stmt65);
    SATTR(r65,branch,create_branch65);
    SATTR(r65,mark,create_m65);
    ret_val75 = r65;
    push_e65 = ret_val75;
    SATTR(push_self65,s,FLISTA1475379054(ATTR(push_self65,s), push_e65));
    ret_val = ((dAM) ATTR(((AM_STMT_EXPR) cur),stmts));
    return ret_val;
   }
   L593 = (ATTR(((AM_STMT_EXPR) cur),expr)==((dAM_EXPR) NULL));
   L5941_=!(L593); 
   if (L5941_) {
    L5951_=(branch1)<(2); 
    L592 = L5951_;
   } else {
    L592 = FALSE;
   }
   if (L592) {
    push_self66 = ATTR(self,stack1);
    create_self66 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt66 = cur;
    create_m66 = ATTR(self,mark);
    create_branch66 = 2;
    L597=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L597==NULL) FATAL("Unable to allocate more memory");
    ((OB)L597)->header.tag=AM_CURSOR_POS_tag;
    L596 = ((AM_CURSOR_POS) L597);
    r66 = L596;
    SATTR(r66,stmt,create_curr_stmt66);
    SATTR(r66,branch,create_branch66);
    SATTR(r66,mark,create_m66);
    ret_val76 = r66;
    push_e66 = ret_val76;
    SATTR(push_self66,s,FLISTA1475379054(ATTR(push_self66,s), push_e66));
    ret_val = ((dAM) ATTR(((AM_STMT_EXPR) cur),expr));
    return ret_val;
   } break;
  case AM_LOCAL_EXPR_tag: break;
  case AM_BREAK_STMT_tag: break;
  case AM_COMMENT_STMT_tag: break;
  case AM_ARR_CONST_tag:
  case AM_BOOL_CONST_tag:
  case AM_CHAR_CONST_tag:
  case AM_CONST_tag:
  case AM_FLTDX_CONST_tag:
  case AM_FLTD_CONST_tag:
  case AM_FLTI_CONST_tag:
  case AM_FLTX_CONST_tag:
  case AM_FLT_CONST_tag:
  case AM_INTI_CONST_tag:
  case AM_INT_CONST_tag:
  case AM_STR_CONST_tag:
  case AM_VOID_CONST_tag: break;
  case AM_EXCEPT_EXPR_tag: break;
  case AM_INV1167837230_tag: break;
  case AM_ANY_EXPR_tag: break;
  case AM_HERE_EXPR_tag: break;
  case AM_WHERE_EXPR_tag:
   L599 = (ATTR(((AM_WHERE_EXPR) cur),arg)==((dAM_EXPR) NULL));
   L6001_=!(L599); 
   if (L6001_) {
    L6011_=(branch1)<(1); 
    L598 = L6011_;
   } else {
    L598 = FALSE;
   }
   if (L598) {
    push_self67 = ATTR(self,stack1);
    create_self67 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt67 = cur;
    create_m67 = ATTR(self,mark);
    create_branch67 = 1;
    L603=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L603==NULL) FATAL("Unable to allocate more memory");
    ((OB)L603)->header.tag=AM_CURSOR_POS_tag;
    L602 = ((AM_CURSOR_POS) L603);
    r67 = L602;
    SATTR(r67,stmt,create_curr_stmt67);
    SATTR(r67,branch,create_branch67);
    SATTR(r67,mark,create_m67);
    ret_val77 = r67;
    push_e67 = ret_val77;
    SATTR(push_self67,s,FLISTA1475379054(ATTR(push_self67,s), push_e67));
    ret_val = ((dAM) ATTR(((AM_WHERE_EXPR) cur),arg));
    return ret_val;
   } break;
  case AM_FAR_EXPR_tag:
   L605 = (ATTR(((AM_FAR_EXPR) cur),arg)==((dAM_EXPR) NULL));
   L6061_=!(L605); 
   if (L6061_) {
    L6071_=(branch1)<(1); 
    L604 = L6071_;
   } else {
    L604 = FALSE;
   }
   if (L604) {
    push_self68 = ATTR(self,stack1);
    create_self68 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt68 = cur;
    create_m68 = ATTR(self,mark);
    create_branch68 = 1;
    L609=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L609==NULL) FATAL("Unable to allocate more memory");
    ((OB)L609)->header.tag=AM_CURSOR_POS_tag;
    L608 = ((AM_CURSOR_POS) L609);
    r68 = L608;
    SATTR(r68,stmt,create_curr_stmt68);
    SATTR(r68,branch,create_branch68);
    SATTR(r68,mark,create_m68);
    ret_val78 = r68;
    push_e68 = ret_val78;
    SATTR(push_self68,s,FLISTA1475379054(ATTR(push_self68,s), push_e68));
    ret_val = ((dAM) ATTR(((AM_FAR_EXPR) cur),arg));
    return ret_val;
   } break;
  case AM_NEAR_EXPR_tag:
   L611 = (ATTR(((AM_NEAR_EXPR) cur),arg)==((dAM_EXPR) NULL));
   L6121_=!(L611); 
   if (L6121_) {
    L6131_=(branch1)<(1); 
    L610 = L6131_;
   } else {
    L610 = FALSE;
   }
   if (L610) {
    push_self69 = ATTR(self,stack1);
    create_self69 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt69 = cur;
    create_m69 = ATTR(self,mark);
    create_branch69 = 1;
    L615=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L615==NULL) FATAL("Unable to allocate more memory");
    ((OB)L615)->header.tag=AM_CURSOR_POS_tag;
    L614 = ((AM_CURSOR_POS) L615);
    r69 = L614;
    SATTR(r69,stmt,create_curr_stmt69);
    SATTR(r69,branch,create_branch69);
    SATTR(r69,mark,create_m69);
    ret_val79 = r69;
    push_e69 = ret_val79;
    SATTR(push_self69,s,FLISTA1475379054(ATTR(push_self69,s), push_e69));
    ret_val = ((dAM) ATTR(((AM_NEAR_EXPR) cur),arg));
    return ret_val;
   } break;
  case AM_AT_EXPR_tag:
   L617 = (ATTR(((AM_AT_EXPR) cur),at)==((dAM_EXPR) NULL));
   L6181_=!(L617); 
   if (L6181_) {
    L6191_=(branch1)<(1); 
    L616 = L6191_;
   } else {
    L616 = FALSE;
   }
   if (L616) {
    push_self70 = ATTR(self,stack1);
    create_self70 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt70 = cur;
    create_m70 = ATTR(self,mark);
    create_branch70 = 1;
    L621=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L621==NULL) FATAL("Unable to allocate more memory");
    ((OB)L621)->header.tag=AM_CURSOR_POS_tag;
    L620 = ((AM_CURSOR_POS) L621);
    r70 = L620;
    SATTR(r70,stmt,create_curr_stmt70);
    SATTR(r70,branch,create_branch70);
    SATTR(r70,mark,create_m70);
    ret_val80 = r70;
    push_e70 = ret_val80;
    SATTR(push_self70,s,FLISTA1475379054(ATTR(push_self70,s), push_e70));
    ret_val = ((dAM) ATTR(((AM_AT_EXPR) cur),at));
    return ret_val;
   }
   L623 = (ATTR(((AM_AT_EXPR) cur),e)==((dAM_EXPR) NULL));
   L6241_=!(L623); 
   if (L6241_) {
    L6251_=(branch1)<(2); 
    L622 = L6251_;
   } else {
    L622 = FALSE;
   }
   if (L622) {
    push_self71 = ATTR(self,stack1);
    create_self71 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt71 = cur;
    create_m71 = ATTR(self,mark);
    create_branch71 = 2;
    L627=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L627==NULL) FATAL("Unable to allocate more memory");
    ((OB)L627)->header.tag=AM_CURSOR_POS_tag;
    L626 = ((AM_CURSOR_POS) L627);
    r71 = L626;
    SATTR(r71,stmt,create_curr_stmt71);
    SATTR(r71,branch,create_branch71);
    SATTR(r71,mark,create_m71);
    ret_val81 = r71;
    push_e71 = ret_val81;
    SATTR(push_self71,s,FLISTA1475379054(ATTR(push_self71,s), push_e71));
    ret_val = ((dAM) ATTR(((AM_AT_EXPR) cur),e));
    return ret_val;
   } break;
  case AM_CLUSTER_EXPR_tag: break;
  case AM_CLU1286269335_tag: break;
  case AM_GLOBAL_EXPR_tag: break;
  default: ;
  other224: ;
   create_self72 = ((OUT) NULL);
   ret_val82 = create_self72;
   plus_self = ret_val82;
   plus_s = ((STR) &thefol1538856223);
   stdout_self = ((FILE1) NULL);
   L629=ZALLOC(sizeof(struct FILE1_struct));
   if (L629==NULL) FATAL("Unable to allocate more memory");
   ((OB)L629)->header.tag=FILE1_tag;
   L628 = ((FILE1) L629);
   r72 = L628;
   
   SATTR(r72,fp,stdout);
   ret_val84 = r72;
   FILE_plus_STR(ret_val84, plus_s);
   ret_val83 = plus_self;
   plus_self1 = ret_val83;
   L6311_=SYSTP(cur); 
   L632 = L6311_;
   L6331_=SYSSTRFORTP(L632); 
   plus_s1 = L6331_;
   stdout_self1 = ((FILE1) NULL);
   L635=ZALLOC(sizeof(struct FILE1_struct));
   if (L635==NULL) FATAL("Unable to allocate more memory");
   ((OB)L635)->header.tag=FILE1_tag;
   L634 = ((FILE1) L635);
   r73 = L634;
   
   SATTR(r73,fp,stdout);
   ret_val86 = r73;
   FILE_plus_STR(ret_val86, plus_s1);
   ret_val85 = plus_self1;
   plus_self2 = ret_val85;
   plus_s2 = ((STR) &name9);
   stdout_self2 = ((FILE1) NULL);
   L638=ZALLOC(sizeof(struct FILE1_struct));
   if (L638==NULL) FATAL("Unable to allocate more memory");
   ((OB)L638)->header.tag=FILE1_tag;
   L637 = ((FILE1) L638);
   r74 = L637;
   
   SATTR(r74,fp,stdout);
   ret_val87 = r74;
   FILE_plus_STR(ret_val87, plus_s2);
 }
 L640 = ATTR(self,indent);
 L6411_=INTMINUS(L640,1); 
 SATTR(self,indent,L6411_);
 if (cur==NULL) {
  goto other225;
 } else
 switch (TAG(cur)) {
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
   L643 = ((dAM_STMT) cur);
   L644 = ((*dAM_ST775224435[TAG(L643)])(L643)==((dAM_STMT) NULL));
   L6451_=!(L644); 
   if (L6451_) {
    L6461_=(branch1)<(999999); 
    L642 = L6461_;
   } else {
    L642 = FALSE;
   }
   if (L642) {
    push_self72 = ATTR(self,stack1);
    create_self73 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt72 = cur;
    create_m72 = ATTR(self,mark);
    create_branch72 = 999999;
    L648=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L648==NULL) FATAL("Unable to allocate more memory");
    ((OB)L648)->header.tag=AM_CURSOR_POS_tag;
    L647 = ((AM_CURSOR_POS) L648);
    r75 = L647;
    SATTR(r75,stmt,create_curr_stmt72);
    SATTR(r75,branch,create_branch72);
    SATTR(r75,mark,create_m72);
    ret_val88 = r75;
    push_e72 = ret_val88;
    SATTR(push_self72,s,FLISTA1475379054(ATTR(push_self72,s), push_e72));
    SATTR(self,mark,FALSE);
    L649 = ((dAM_STMT) cur);
    ret_val = ((dAM) (*dAM_ST775224435[TAG(L649)])(L649));
    return ret_val;
   } break;
  default: ;
  other225: ;
 }
 ret_val = ((dAM) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void AM_CUR1869307717(AM_CURSOR self) {
 AM_CURSOR_POS p;
 A_STAC1244966098 is_empty_self;
 BOOL ret_val = BOOL_zero;
 A_STAC1244966098 size_self;
 INT ret_val1 = INT_zero;
 A_STAC1244966098 pop_self;
 AM_CURSOR_POS ret_val2;
 INT L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 while (1) {
  is_empty_self = ATTR(self,stack1);
  size_self = is_empty_self;
  ret_val1 = FLISTA587993904(ATTR(size_self,s));
  L1 = ret_val1;
  L21_=(L1)==(0); 
  ret_val = L21_;
  L3 = ret_val;
  L41_=!(L3); 
  if (L41_) {
  }
  else {
   goto after_loop;
  }
  pop_self = ATTR(self,stack1);
  ret_val2 = FLISTA971253650(ATTR(pop_self,s));
  p = ret_val2;
 }
 after_loop: ;
 SATTR(self,cur,((dAM) NULL));
 SATTR(self,mark,FALSE);
 SATTR(self,indent,0);
 SATTR(self,loops,0);
 SATTR(self,started,FALSE);
}
