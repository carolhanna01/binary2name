#include "sather.h"

/* Layouts */

typedef struct dAM_EXPR_struct {
 OB_HEADER header;
 } *dAM_EXPR;

typedef struct dAM_STMT_struct {
 OB_HEADER header;
 } *dAM_STMT;

typedef struct dAS_NODE_struct {
 OB_HEADER header;
 } *dAS_NODE;

typedef struct dAS_STMT_struct {
 OB_HEADER header;
 } *dAS_STMT;

typedef struct dSTR_struct {
 OB_HEADER header;
 } *dSTR;

typedef struct dTP_struct {
 OB_HEADER header;
 } *dTP;

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

typedef struct ARRAYd1329429954_frame_struct {
 INT state;
 ARRAYdAM_EXPR self;/* Formal argument: self */
 dAM_EXPR arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYd1329429954_frame;

typedef struct AS_STMT_CURSOR_struct {/* layout for AS_STMT_CURSOR */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *AS_STMT_CURSOR;

typedef struct AS_STM1504735434_frame_struct {
 INT state;
 AS_STMT_CURSOR self;/* Formal argument: self */
 dAS_NODE arg1;/* Formal argument: cur */
 dAS_STMT ret_val;
 dAS_NODE as;
 AS_STMT_CURSOR L21;
 AS_STMT_CURSOR L41;
 AS_STMT_CURSOR L61;
 AS_STMT_CURSOR L81;
 AS_STMT_CURSOR L101;
 AS_STMT_CURSOR L121;
 AS_STMT_CURSOR L141;
 AS_STMT_CURSOR L161;
 AS_STMT_CURSOR L181;
 AS_STMT_CURSOR L201;
 AS_STMT_CURSOR L231;
 AS_STMT_CURSOR L251;
 AS_STMT_CURSOR L271;
 AS_STMT_CURSOR L291;
 AS_STMT_CURSOR L311;
 AS_STMT_CURSOR L331;
 AS_STMT_CURSOR L351;
 AS_STMT_CURSOR L371;
 AS_STMT_CURSOR L391;
 AS_STMT_CURSOR L421;
 AS_STMT_CURSOR L441;
 AS_STMT_CURSOR L461;
 AS_STMT_CURSOR L481;
 AS_STMT_CURSOR L501;
 AS_STMT_CURSOR L521;
 AS_STMT_CURSOR L541;
 AS_STMT_CURSOR L561;
 struct AS_STM1504735434_frame_struct *nested1; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested2; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested3; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested4; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested5; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested6; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested7; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested8; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested9; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested10; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested11; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested12; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested13; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested14; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested15; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested16; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested17; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested18; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested19; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested20; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested21; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested22; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested23; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested24; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested25; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested26; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested27; /* nested iter frame */
 } *AS_STM1504735434_frame;

typedef struct BFILE_struct {/* layout for BFILE */
 OB_HEADER header;
 EXT_OB fp;
 } *BFILE;

typedef struct ERR1_struct {/* layout for ERR */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ERR1;

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

typedef struct SFILE_ID_struct {/* layout for SFILE_ID */
 INT loc;
 } SFILE_ID;
static SFILE_ID SFILE_ID_zero;

typedef struct SFILE_ID_boxed_struct {
 OB_HEADER header;
 SFILE_ID immutable_part;
 } *SFILE_ID_boxed;

typedef struct SIG_struct {/* layout for SIG */
 OB_HEADER header;
 struct dTP_struct *ret;
 struct dTP_struct *tp;
 struct ARRAYdTP_struct *src_tparams;
 struct ARRAYARG_struct *args;
 struct ARRAYBOOL_struct *hot;
 struct CONFIG_ROUT_struct *builtin_info;
 struct IDENT_struct name1;
 struct OPT_LOCAL_CALL_struct *opt_info;
 struct SE_CONTEXT_struct *se_context;
 struct SIG_struct *srcsig;
 STR str;
 BOOL am_created;
 BOOL got_builtin_info;
 BOOL is_attach_routin;
 BOOL is_fork_routine;
 BOOL is_par_routine;
 BOOL needs_export;
 BOOL needs_import;
 BOOL opt_fork;
 BOOL returns_same;
 } *SIG;

typedef struct STR_ae1826948584_frame_struct {
 INT state;
 STR self;/* Formal argument: self */
 INT arg1;/* Formal argument: beg */
 CHAR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ic,I_u_Is; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *STR_ae1826948584_frame;

typedef struct STR_ae828550501_frame_struct {
 INT state;
 STR self;/* Formal argument: self */
 INT arg1;/* Formal argument: beg */
 INT arg2;/* Formal argument: num */
 CHAR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ic,I_u_Im; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *STR_ae828550501_frame;

typedef struct STR_aeltbrCHAR_frame_struct {
 INT state;
 STR self;/* Formal argument: self */
 CHAR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *STR_aeltbrCHAR_frame;

typedef struct STR_asetb_CHAR_frame_struct {
 INT state;
 STR self;/* Formal argument: self */
 CHAR arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *STR_asetb_CHAR_frame;

typedef struct STR_el1843619312_frame_struct {
 INT state;
 STR self;/* Formal argument: self */
 INT arg1;/* Formal argument: beg */
 CHAR ret_val;
 INT i;
 INT sz;
 } *STR_el1843619312_frame;

typedef struct STR_eltbrCHAR_frame_struct {
 INT state;
 STR self;/* Formal argument: self */
 CHAR ret_val;
 INT i;
 INT sz;
 } *STR_eltbrCHAR_frame;

typedef struct STR_indbrINT_frame_struct {
 INT state;
 STR self;/* Formal argument: self */
 INT ret_val;
 INT i;
 INT sz;
 } *STR_indbrINT_frame;

typedef struct STR_se405450305_frame_struct {
 INT state;
 STR self;/* Formal argument: self */
 dSTR arg1;/* Formal argument: s */
 STR ret_val1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 } *STR_se405450305_frame;

typedef struct STR_sp663650766_frame_struct {
 INT state;
 STR self;/* Formal argument: self */
 CHAR arg1;/* Formal argument: c */
 STR ret_val;
 INT cur_loc;
 INT next_loc;
 } *STR_sp663650766_frame;

typedef struct STR_CURSOR_struct {/* layout for STR_CURSOR */
 OB_HEADER header;
 INT error1;
 INT index1;
 INT line_no;
 STR buf1;
 CHAR comment_char1;
 CHAR comment_char2;
 BOOL is_done;
 } *STR_CURSOR;

typedef struct SYS_struct {/* layout for SYS */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *SYS;

typedef struct SYSTEM_LEX_struct {/* layout for SYSTEM_LEX */
 OB_HEADER header;
 struct FSTR_struct *buf1;
 INT lex_state;
 INT pos;
 STR name1;
 } *SYSTEM_LEX;

typedef struct SYSTEM675309847_frame_struct {
 INT state;
 SYSTEM_LEX self;/* Formal argument: self */
 STR ret_val;
 STR s;
 } *SYSTEM675309847_frame;

typedef struct UNIX_struct {/* layout for UNIX */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *UNIX;

typedef struct AM_FORK_STMT_struct {/* layout for AM_FORK_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *at;
 struct dAM_STMT_struct *body;
 struct dAM_STMT_struct *next;
 struct AM_PAR_STMT_struct *par_stmt;
 struct SFILE_ID_struct source1;
 } *AM_FORK_STMT;

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

typedef struct AM_INV1167837230_struct {/* layout for AM_INVARIANT_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 struct SIG_struct *sig1;
 } *AM_INV1167837230;

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

typedef struct AM_WAITFOR_STMT_struct {/* layout for AM_WAITFOR_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *dest;
 struct dAM_EXPR_struct *src1;
 struct dAM_STMT_struct *next;
 struct AM_LOCAL_EXPR_struct *prefetch;
 struct SFILE_ID_struct source1;
 } *AM_WAITFOR_STMT;

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

typedef struct FLISTdAM_EXPR_struct {/* layout for FLIST{$AM_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dAM_EXPR_struct *arr_part[1];
 } *FLISTdAM_EXPR;

typedef struct FLISTd2122660729_frame_struct {
 INT state;
 FLISTdAM_EXPR self;/* Formal argument: self */
 dAM_EXPR ret_val1;
 INT i;
 INT sz;
 FLISTdAM_EXPR aget_self;
 INT aget_ind;
 dAM_EXPR ret_val;
 } *FLISTd2122660729_frame;

typedef struct FLISTdAM_STMT_struct {/* layout for FLIST{$AM_STMT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dAM_STMT_struct *arr_part[1];
 } *FLISTdAM_STMT;

typedef struct FLISTd1374595573_frame_struct {
 INT state;
 FLISTdAM_STMT self;/* Formal argument: self */
 dAM_STMT ret_val1;
 INT i;
 INT sz;
 FLISTdAM_STMT aget_self;
 INT aget_ind;
 dAM_STMT ret_val;
 } *FLISTd1374595573_frame;

typedef struct FLISTdTP_struct {/* layout for FLIST{$TP} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dTP_struct *arr_part[1];
 } *FLISTdTP;

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

#include "tags.h"

/* Globals */

extern FSTR STR_buf;
extern INT STR_CU1107107574;
extern INT STR_CU1114535871;
extern INT STR_CU1474621917;
extern INT STR_CU1719265904;
extern INT STR_CU1921064154;
extern INT STR_CU254621724;
extern INT STR_CU610585082;

/* Function declarations */


extern RETURNED_CONST STR (**dSTR_strrSTR)(dSTR);

extern RETURNED_CONST dAM_EXPR (**dAM_EX1830308833)(dAM_EXPR);

extern RETURNED_CONST dAM_STMT (**dAM_ST1032597839)(dAM_STMT);

extern RETURNED_CONST dAM_STMT (**dAM_ST775224435)(dAM_STMT);

extern RETURNED_CONST dAS_STMT (**dAS_ST460279343)(dAS_STMT);

extern RETURNED_CONST void (**dAM_ST1372701974)(dAM_STMT, dAM_STMT);
AM_FORK_STMT AM_FOR214044719(AM_FORK_STMT);
AM_IF_STMT AM_IF_1702280121(AM_IF_STMT);
AM_INITIAL_STMT AM_INI1487747776(AM_INITIAL_STMT);
AM_INV1167837230 AM_INV1353049642(AM_INV1167837230);
AM_LOCK_STMT AM_LOC1474178511(AM_LOCK_STMT);
AM_LOOP_STMT AM_LOO1572875803(AM_LOOP_STMT);
AM_PAR_STMT AM_PAR784084248(AM_PAR_STMT);
AM_POST_STMT AM_POS1078136755(AM_POST_STMT);
AM_PREFETCH_STMT AM_PRE896696117(AM_PREFETCH_STMT);
AM_PRE_STMT AM_PRE202271760(AM_PRE_STMT);
AM_PROTECT_STMT AM_PRO1363632140(AM_PROTECT_STMT);
AM_RAISE_STMT AM_RAI1825238682(AM_RAISE_STMT);
AM_RETURN_STMT AM_RET1651971761(AM_RETURN_STMT);
AM_SYNC_STMT AM_SYN1962040721(AM_SYNC_STMT);
AM_TYPECASE_STMT AM_TYP1756924987(AM_TYPECASE_STMT);
AM_UNLOCK_STMT AM_UNL460993177(AM_UNLOCK_STMT);
AM_WAITFOR_STMT AM_WAI1946639376(AM_WAITFOR_STMT);
AM_WIT1996971603 AM_WIT1785216238(AM_WIT1996971603);
AM_YIELD_STMT AM_YIE197772790(AM_YIELD_STMT);
ARRAYdAM_EXPR FLISTA1857087356(FLISTA1857087356_frame);
BOOL CHAR_i1303832348(CHAR);
BOOL CHAR_i696824757(CHAR);
BOOL CHAR_i705101630(CHAR);
BOOL CHAR_i725352188(CHAR);
BOOL CHAR_i84735023(CHAR);
BOOL STR_CU1807790508(STR_CURSOR);
BOOL STR_CU2108715877(STR_CURSOR);
BOOL STR_CU557658657(STR_CURSOR);
BOOL STR_is111530248(STR, STR);
BOOL STR_is142167618(STR, OB);
BOOL STR_is171745744(STR, STR);
BOOL STR_is2136368518(STR, STR, INT);
CHAR CHAR_upperrCHAR(CHAR);
CHAR STR_CU2113130984(STR_CURSOR);
CHAR STR_ae1826948584(STR_ae1826948584_frame);
CHAR STR_ae828550501(STR_ae828550501_frame);
CHAR STR_aeltbrCHAR(STR_aeltbrCHAR_frame);
CHAR STR_el1843619312(STR_el1843619312_frame);
CHAR STR_eltbrCHAR(STR_eltbrCHAR_frame);
FLISTA1327600838 FLISTA475813755(FLISTA1327600838, ARRAYdAM_EXPR);
FLISTdAM_EXPR FLISTd805769956(FLISTdAM_EXPR, dAM_EXPR);
FLISTdAM_STMT FLISTd506668277(FLISTdAM_STMT, dAM_STMT);
FSTR BFILE_fstrrFSTR(BFILE);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR FSTR_p399773021(FSTR, CHAR);
INT CHAR_d198943415(CHAR);
INT CHAR_h840494345(CHAR);
INT CHAR_o1022220512(CHAR);
INT FSTR_sizerINT(FSTR);
INT INT_do676118316(INT_do676118316_frame);
INT INT_up418511718(INT_up418511718_frame);
INT STR_CU1297585490(STR_CURSOR);
INT STR_CU1312667011(STR_CURSOR);
INT STR_CU1697515046(STR_CURSOR);
INT STR_CU476945601(STR_CURSOR);
INT STR_CU745418827(STR_CURSOR);
INT STR_hashrINT(STR);
INT STR_indbrINT(STR_indbrINT_frame);
INT STR_lengthrINT(STR);
INT STR_se1235911597(STR, CHAR, INT);
INT STR_se1633361857(STR, CHAR);
INT STR_se1725540125(STR, CHAR);
INT STR_se2132153226(STR, STR);
INT STR_sizerINT(STR);
SFILE_ID AM_FOR1474728133(AM_FORK_STMT);
SFILE_ID AM_IF_1192492569(AM_IF_STMT);
SFILE_ID AM_INI1792787320(AM_INITIAL_STMT);
SFILE_ID AM_INV963567817(AM_INV1167837230);
SFILE_ID AM_LOC748262786(AM_LOCK_STMT);
SFILE_ID AM_LOO738236003(AM_LOOP_STMT);
SFILE_ID AM_PAR1239764599(AM_PAR_STMT);
SFILE_ID AM_POS2123304705(AM_POST_STMT);
SFILE_ID AM_PRE1797857661(AM_PRE_STMT);
SFILE_ID AM_PRE2094346420(AM_PREFETCH_STMT);
SFILE_ID AM_PRO1670847403(AM_PROTECT_STMT);
SFILE_ID AM_RAI1393475571(AM_RAISE_STMT);
SFILE_ID AM_RET702658942(AM_RETURN_STMT);
SFILE_ID AM_SYN416733298(AM_SYNC_STMT);
SFILE_ID AM_TYP720211797(AM_TYPECASE_STMT);
SFILE_ID AM_UNL1937638738(AM_UNLOCK_STMT);
SFILE_ID AM_WAI442200356(AM_WAITFOR_STMT);
SFILE_ID AM_WIT1120935944(AM_WIT1996971603);
SFILE_ID AM_YIE1303084854(AM_YIELD_STMT);
STR CHAR_prettyrSTR(CHAR);
STR INT_oc2110318348(INT);
STR STR_CU1062701511(STR_CURSOR, CHAR);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap1182453786(STR, STR, STR, STR, STR, STR, STR);
STR STR_ap1417971546(STR, STR, STR, STR);
STR STR_ap1693864410(STR, STR, STR);
STR STR_ap1925291910(STR, STR, STR, STR, STR, STR);
STR STR_ap2004550586(STR, STR);
STR STR_ap44172742(STR, STR, STR, STR, STR);
STR STR_as1755591216(STR);
STR STR_cr1492743419(STR, EXT_OB);
STR STR_fr1097270334(STR, FSTR);
STR STR_head_INTrSTR(STR, INT);
STR STR_pl1270664985(STR, dSTR);
STR STR_prettyrSTR(STR);
STR STR_se405450305(STR_se405450305_frame);
STR STR_sp663650766(STR_sp663650766_frame);
STR STR_strrSTR(STR);
STR STR_su1627796404(STR, INT);
STR STR_su203383946(STR, INT, INT);
STR STR_tail_INTrSTR(STR, INT);
STR STR_upperrSTR(STR);
STR SYSTEM1338733266(SYSTEM_LEX);
STR SYSTEM675309847(SYSTEM675309847_frame);
SYSTEM_LEX SYSTEM2034253983(SYSTEM_LEX, STR);
dAM_EXPR ARRAYd1413132360(ARRAYd1413132360_frame);
dAM_EXPR FLISTd2122660729(FLISTd2122660729_frame);
dAM_STMT AM_FOR1357281358(AM_FORK_STMT);
dAM_STMT AM_IF_1153855302(AM_IF_STMT);
dAM_STMT AM_INI81424971(AM_INITIAL_STMT);
dAM_STMT AM_INV1267245156(AM_INV1167837230);
dAM_STMT AM_LOC724705923(AM_LOCK_STMT);
dAM_STMT AM_LOO439140156(AM_LOOP_STMT);
dAM_STMT AM_PAR309007796(AM_PAR_STMT);
dAM_STMT AM_POS1775621496(AM_POST_STMT);
dAM_STMT AM_PRE1502061133(AM_PREFETCH_STMT);
dAM_STMT AM_PRE1762688610(AM_PRE_STMT);
dAM_STMT AM_PRO557659408(AM_PROTECT_STMT);
dAM_STMT AM_RAI1337438778(AM_RAISE_STMT);
dAM_STMT AM_RET1976133913(AM_RETURN_STMT);
dAM_STMT AM_SYN2020605939(AM_SYNC_STMT);
dAM_STMT AM_TYP1504178732(AM_TYPECASE_STMT);
dAM_STMT AM_UNL1996507749(AM_UNLOCK_STMT);
dAM_STMT AM_WAI1727009663(AM_WAITFOR_STMT);
dAM_STMT AM_WIT315547139(AM_WIT1996971603);
dAM_STMT AM_YIE1267845009(AM_YIELD_STMT);
dAM_STMT FLISTd1374595573(FLISTd1374595573_frame);
dAS_STMT AS_STM1504735434(AS_STM1504735434_frame);
void AM_FOR1689225795(AM_FORK_STMT, dAM_STMT);
void AM_FOR790645051(AM_FORK_STMT, dAM_STMT);
void AM_IF_1406990231(AM_IF_STMT, dAM_STMT);
void AM_IF_993185585(AM_IF_STMT, dAM_STMT);
void AM_INI2007284982(AM_INITIAL_STMT, dAM_STMT);
void AM_INI2065615916(AM_INITIAL_STMT, dAM_STMT);
void AM_INV749070155(AM_INV1167837230, dAM_STMT);
void AM_INV880681253(AM_INV1167837230, dAM_STMT);
void AM_LOC1423220486(AM_LOCK_STMT, dAM_STMT);
void AM_LOC962760448(AM_LOCK_STMT, dAM_STMT);
void AM_LOO1708786253(AM_LOOP_STMT, dAM_STMT);
void AM_LOO952733665(AM_LOOP_STMT, dAM_STMT);
void AM_PAR1454262261(AM_PAR_STMT, dAM_STMT);
void AM_PAR1838033091(AM_PAR_STMT, dAM_STMT);
void AM_POS1908807043(AM_POST_STMT, dAM_STMT);
void AM_POS372304913(AM_POST_STMT, dAM_STMT);
void AM_PRE1879848758(AM_PREFETCH_STMT, dAM_STMT);
void AM_PRE2012355323(AM_PRE_STMT, dAM_STMT);
void AM_PRE385237799(AM_PRE_STMT, dAM_STMT);
void AM_PRE645865276(AM_PREFETCH_STMT, dAM_STMT);
void AM_PRO1589381479(AM_PROTECT_STMT, dAM_STMT);
void AM_PRO1885345065(AM_PROTECT_STMT, dAM_STMT);
void AM_RAI1178977909(AM_RAISE_STMT, dAM_STMT);
void AM_RAI810487631(AM_RAISE_STMT, dAM_STMT);
void AM_RET170906974(AM_RETURN_STMT, dAM_STMT);
void AM_RET917156604(AM_RETURN_STMT, dAM_STMT);
void AM_SYN127320470(AM_SYNC_STMT, dAM_STMT);
void AM_SYN631230960(AM_SYNC_STMT, dAM_STMT);
void AM_TYP505714135(AM_TYPECASE_STMT, dAM_STMT);
void AM_TYP642862155(AM_TYPECASE_STMT, dAM_STMT);
void AM_UNL150533138(AM_UNLOCK_STMT, dAM_STMT);
void AM_UNL2142830896(AM_UNLOCK_STMT, dAM_STMT);
void AM_WAI420031224(AM_WAITFOR_STMT, dAM_STMT);
void AM_WAI656698018(AM_WAITFOR_STMT, dAM_STMT);
void AM_WIT1831493748(AM_WIT1996971603, dAM_STMT);
void AM_WIT906438282(AM_WIT1996971603, dAM_STMT);
void AM_YIE1088587192(AM_YIELD_STMT, dAM_STMT);
void AM_YIE879195878(AM_YIELD_STMT, dAM_STMT);
void ARRAYd1329429954(ARRAYd1329429954_frame);
void FILE_plus_STR(FILE1, STR);
void FILE_plus_dSTR(FILE1, dSTR);
void STR_CU1034930507(STR_CURSOR);
void STR_CU1154930915(STR_CURSOR);
void STR_CU1355884946(STR_CURSOR, STR);
void STR_CU713159692(STR_CURSOR);
void STR_asetb_CHAR(STR_asetb_CHAR_frame);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

AM_FORK_STMT AM_FOR214044719(AM_FORK_STMT self) {
 AM_FORK_STMT ret_val;
 OUT create_self;
 OUT ret_val1;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val2;
 FILE1 r;
 extern STR itisno820384125;
 FILE1 L1;
 OB L2;
 if ((self==((AM_FORK_STMT) NULL))) {
  ret_val = ((AM_FORK_STMT) NULL);
  return ret_val;
 }
 create_self = ((OUT) NULL);
 ret_val1 = create_self;
 plus_self = ret_val1;
 plus_s = ((STR) &itisno820384125);
 stdout_self = ((FILE1) NULL);
 L2=ZALLOC(sizeof(struct FILE1_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=FILE1_tag;
 L1 = ((FILE1) L2);
 r = L1;
 
 SATTR(r,fp,stdout);
 ret_val2 = r;
 FILE_plus_STR(ret_val2, plus_s);
 ret_val = ((AM_FORK_STMT) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_IF_STMT AM_IF_1702280121(AM_IF_STMT self) {
 AM_IF_STMT ret_val;
 AM_IF_STMT r;
 AM_IF_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_IF_STMT ret_val1;
 AM_IF_STMT r1;
 AM_IF_STMT L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 dAM_STMT L5;
 BOOL L6;
 BOOL L71_;
 dAM_EXPR L8;
 BOOL L9;
 BOOL L101_;
 dAM_STMT L11;
 BOOL L12;
 BOOL L131_;
 dAM_STMT L14;
 if ((self==((AM_IF_STMT) NULL))) {
  ret_val = ((AM_IF_STMT) NULL);
  return ret_val;
 }
 create_self = ((AM_IF_STMT) NULL);
 create_source = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_IF_STMT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_IF_STMT_tag;
 L1 = ((AM_IF_STMT) L2);
 r1 = L1;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 L3 = (ATTR(self,next)==((dAM_STMT) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(self,next);
  SATTR(r,next,(*dAM_ST1032597839[TAG(L5)])(L5));
 }
 L6 = (ATTR(self,test1)==((dAM_EXPR) NULL));
 L71_=!(L6); 
 if (L71_) {
  L8 = ATTR(self,test1);
  SATTR(r,test1,(*dAM_EX1830308833[TAG(L8)])(L8));
 }
 L9 = (ATTR(self,if_true)==((dAM_STMT) NULL));
 L101_=!(L9); 
 if (L101_) {
  L11 = ATTR(self,if_true);
  SATTR(r,if_true,(*dAM_ST1032597839[TAG(L11)])(L11));
 }
 L12 = (ATTR(self,if_false)==((dAM_STMT) NULL));
 L131_=!(L12); 
 if (L131_) {
  L14 = ATTR(self,if_false);
  SATTR(r,if_false,(*dAM_ST1032597839[TAG(L14)])(L14));
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_INITIAL_STMT AM_INI1487747776(AM_INITIAL_STMT self) {
 AM_INITIAL_STMT ret_val;
 AM_INITIAL_STMT r;
 AM_INITIAL_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_INITIAL_STMT ret_val1;
 AM_INITIAL_STMT r1;
 AM_INITIAL_STMT L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 dAM_STMT L5;
 BOOL L6;
 BOOL L71_;
 dAM_STMT L8;
 if ((self==((AM_INITIAL_STMT) NULL))) {
  ret_val = ((AM_INITIAL_STMT) NULL);
  return ret_val;
 }
 create_self = ((AM_INITIAL_STMT) NULL);
 create_source = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_INITIAL_STMT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_INITIAL_STMT_tag;
 L1 = ((AM_INITIAL_STMT) L2);
 r1 = L1;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 L3 = (ATTR(self,next)==((dAM_STMT) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(self,next);
  SATTR(r,next,(*dAM_ST1032597839[TAG(L5)])(L5));
 }
 SATTR(r,tp,ATTR(self,tp));
 L6 = (ATTR(self,stmts)==((dAM_STMT) NULL));
 L71_=!(L6); 
 if (L71_) {
  L8 = ATTR(self,stmts);
  SATTR(r,stmts,(*dAM_ST1032597839[TAG(L8)])(L8));
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_INV1167837230 AM_INV1353049642(AM_INV1167837230 self) {
 AM_INV1167837230 ret_val;
 AM_INV1167837230 r;
 AM_INV1167837230 create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_INV1167837230 ret_val1;
 AM_INV1167837230 r1;
 AM_INV1167837230 L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 dAM_STMT L5;
 if ((self==((AM_INV1167837230) NULL))) {
  ret_val = ((AM_INV1167837230) NULL);
  return ret_val;
 }
 create_self = ((AM_INV1167837230) NULL);
 create_source = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_INV1167837230_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_INV1167837230_tag;
 L1 = ((AM_INV1167837230) L2);
 r1 = L1;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 L3 = (ATTR(self,next)==((dAM_STMT) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(self,next);
  SATTR(r,next,(*dAM_ST1032597839[TAG(L5)])(L5));
 }
 SATTR(r,sig1,ATTR(self,sig1));
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_LOCK_STMT AM_LOC1474178511(AM_LOCK_STMT self) {
 AM_LOCK_STMT ret_val;
 AM_LOCK_STMT r;
 FLISTdAM_EXPR L11;
 FLISTdAM_STMT L21;
 FLISTA1327600838 L31;
 ARRAYdAM_EXPR e;
 ARRAYdAM_EXPR n;
 ARRAYdAM_EXPR L41;
 ARRAYdAM_EXPR L51;
 ARRAYdAM_EXPR create_self;
 INT create_n = INT_zero;
 ARRAYdAM_EXPR ret_val1;
 INT L61 = INT_zero;
 AM_LOCK_STMT L7;
 OB L8;
 FLISTdAM_EXPR L9;
 dAM_EXPR L10;
 dAM_EXPR L12;
 FLISTdAM_STMT L13;
 dAM_STMT L14;
 dAM_STMT L15;
 ARRAYdAM_EXPR L16;
 INT L171_;
 ARRAYdAM_EXPR L18;
 INT L19;
 OB L20;
 INT L22;
 INT L231_br;
 dAM_EXPR aL231_;
 INT L241_br;
 dAM_EXPR L25;
 dAM_EXPR L26;
 INT L271_;
 dAM_STMT L28;
 if ((self==((AM_LOCK_STMT) NULL))) {
  ret_val = ((AM_LOCK_STMT) NULL);
  return ret_val;
 }
 L8=ZALLOC(sizeof(struct AM_LOCK_STMT_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=AM_LOCK_STMT_tag;
 L7 = ((AM_LOCK_STMT) L8);
 r = L7;
 {
  struct FLISTd2122660729_frame_struct other1_0;
FLISTd2122660729_frame noname1 = &other1_0;
  noname1->state = 0;
  while (1) {
   L9 = ATTR(r,guards);
   if (noname1->state == 0) {
    L11 = ATTR(self,guards);
    noname1->self = L11;
   }
   L10 = FLISTd2122660729(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   L12 = L10;
   SATTR(r,guards,FLISTd805769956(L9, (*dAM_EX1830308833[TAG(L12)])(L12)));
  }
 }
 after_loop: ;
 {
  struct FLISTd1374595573_frame_struct other2_0;
FLISTd1374595573_frame noname2 = &other2_0;
  noname2->state = 0;
  while (1) {
   L13 = ATTR(r,stmts);
   if (noname2->state == 0) {
    L21 = ATTR(self,stmts);
    noname2->self = L21;
   }
   L14 = FLISTd1374595573(noname2);
   if (noname2->state == -1) {
    goto after_loop1;
   }
   L15 = L14;
   SATTR(r,stmts,FLISTd506668277(L13, (*dAM_ST1032597839[TAG(L15)])(L15)));
  }
 }
 after_loop1: ;
 {
  struct FLISTA1857087356_frame_struct other3_0;
FLISTA1857087356_frame noname3 = &other3_0;
  L31 = ATTR(self,locks1);
  noname3->self = L31;
  noname3->state = 0;
  while (1) {
   L16 = FLISTA1857087356(noname3);
   if (noname3->state == -1) {
    goto after_loop2;
   }
   e = L16;
   create_self = ((ARRAYdAM_EXPR) NULL);
   L171_=(e)==NULL?0:ASIZE((ARRAYdAM_EXPR)e); 
   create_n = L171_;
   L19 = create_n;
   L22=(sizeof(struct ARRAYdAM_EXPR_struct)-sizeof(dAM_EXPR))+(L19)*sizeof(dAM_EXPR);
   L20=ZALLOC_BIG(L22);
   if (L20==NULL) FATAL("Unable to allocate more memory");
   ((OB)L20)->header.tag=ARRAYdAM_EXPR_tag;
#ifdef DESTROY_CHK

     ((OB)L20)->header.destroyed=0;
#endif

   L18 = ((ARRAYdAM_EXPR) L20);
   L18->asize = L19;
   ret_val1 = L18;
   n = ret_val1;
   {
    BOOL f_L231_ = TRUE;
    BOOL f_L241_ = TRUE;
    /* loop index variable */
    L61 = 0;
    L41 = e;
    L51 = n;
    L231_br=L41==NULL?0:ASIZE((ARRAYdAM_EXPR)L41); 
    L241_br=L51==NULL?0:ASIZE((ARRAYdAM_EXPR)L51); 
    while (1) {
     if(L61>=L241_br)  goto after_loop3; 
     if(L61>=L231_br)  goto after_loop3; 
     aL231_=ARR((ARRAYdAM_EXPR)L41,L61); 
     L26 = aL231_;
     SARR((ARRAYdAM_EXPR)L51,L61,(dAM_EXPR)L26); 
     ;
     L271_=INTPLUS(L61,1); 
     L61 = L271_;
    }
   }
   after_loop3: ;
   SATTR(r,locks1,FLISTA475813755(ATTR(r,locks1), n));
  }
 }
 after_loop2: ;
 L28 = ATTR(self,else_stmts);
 SATTR(r,else_stmts,(*dAM_ST1032597839[TAG(L28)])(L28));
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_LOOP_STMT AM_LOO1572875803(AM_LOOP_STMT self) {
 AM_LOOP_STMT ret_val;
 OUT create_self;
 OUT ret_val1;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val2;
 FILE1 r;
 extern STR itisno1744185157;
 FILE1 L1;
 OB L2;
 if ((self==((AM_LOOP_STMT) NULL))) {
  ret_val = ((AM_LOOP_STMT) NULL);
  return ret_val;
 }
 create_self = ((OUT) NULL);
 ret_val1 = create_self;
 plus_self = ret_val1;
 plus_s = ((STR) &itisno1744185157);
 stdout_self = ((FILE1) NULL);
 L2=ZALLOC(sizeof(struct FILE1_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=FILE1_tag;
 L1 = ((FILE1) L2);
 r = L1;
 
 SATTR(r,fp,stdout);
 ret_val2 = r;
 FILE_plus_STR(ret_val2, plus_s);
 ret_val = ((AM_LOOP_STMT) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_PAR_STMT AM_PAR784084248(AM_PAR_STMT self) {
 AM_PAR_STMT ret_val;
 OUT create_self;
 OUT ret_val1;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val2;
 FILE1 r;
 extern STR itisno393817775;
 FILE1 L1;
 OB L2;
 if ((self==((AM_PAR_STMT) NULL))) {
  ret_val = ((AM_PAR_STMT) NULL);
  return ret_val;
 }
 create_self = ((OUT) NULL);
 ret_val1 = create_self;
 plus_self = ret_val1;
 plus_s = ((STR) &itisno393817775);
 stdout_self = ((FILE1) NULL);
 L2=ZALLOC(sizeof(struct FILE1_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=FILE1_tag;
 L1 = ((FILE1) L2);
 r = L1;
 
 SATTR(r,fp,stdout);
 ret_val2 = r;
 FILE_plus_STR(ret_val2, plus_s);
 ret_val = ((AM_PAR_STMT) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_POST_STMT AM_POS1078136755(AM_POST_STMT self) {
 AM_POST_STMT ret_val;
 AM_POST_STMT r;
 AM_POST_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_POST_STMT ret_val1;
 AM_POST_STMT r1;
 AM_POST_STMT L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 dAM_STMT L5;
 BOOL L6;
 BOOL L71_;
 dAM_EXPR L8;
 if ((self==((AM_POST_STMT) NULL))) {
  ret_val = ((AM_POST_STMT) NULL);
  return ret_val;
 }
 create_self = ((AM_POST_STMT) NULL);
 create_source = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_POST_STMT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_POST_STMT_tag;
 L1 = ((AM_POST_STMT) L2);
 r1 = L1;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 L3 = (ATTR(self,next)==((dAM_STMT) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(self,next);
  SATTR(r,next,(*dAM_ST1032597839[TAG(L5)])(L5));
 }
 SATTR(r,tp,ATTR(self,tp));
 L6 = (ATTR(self,test1)==((dAM_EXPR) NULL));
 L71_=!(L6); 
 if (L71_) {
  L8 = ATTR(self,test1);
  SATTR(r,test1,(*dAM_EX1830308833[TAG(L8)])(L8));
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_PREFETCH_STMT AM_PRE896696117(AM_PREFETCH_STMT self) {
 AM_PREFETCH_STMT ret_val;
 AM_PREFETCH_STMT r;
 AM_PREFETCH_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_PREFETCH_STMT ret_val1;
 AM_PREFETCH_STMT r1;
 AM_PREFETCH_STMT L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 dAM_STMT L5;
 BOOL L6;
 BOOL L71_;
 dAM_EXPR L8;
 BOOL L9;
 BOOL L101_;
 dAM_EXPR L11;
 BOOL L12;
 BOOL L131_;
 if ((self==((AM_PREFETCH_STMT) NULL))) {
  ret_val = ((AM_PREFETCH_STMT) NULL);
  return ret_val;
 }
 create_self = ((AM_PREFETCH_STMT) NULL);
 create_source = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_PREFETCH_STMT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_PREFETCH_STMT_tag;
 L1 = ((AM_PREFETCH_STMT) L2);
 r1 = L1;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 L3 = (ATTR(self,next)==((dAM_STMT) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(self,next);
  SATTR(r,next,(*dAM_ST1032597839[TAG(L5)])(L5));
 }
 L6 = (ATTR(self,dest)==((dAM_EXPR) NULL));
 L71_=!(L6); 
 if (L71_) {
  L8 = ATTR(self,dest);
  SATTR(r,dest,(*dAM_EX1830308833[TAG(L8)])(L8));
 }
 L9 = (ATTR(self,src1)==((dAM_EXPR) NULL));
 L101_=!(L9); 
 if (L101_) {
  L11 = ATTR(self,src1);
  SATTR(r,src1,(*dAM_EX1830308833[TAG(L11)])(L11));
 }
 L12 = (ATTR(self,prefetch)==((AM_LOCAL_EXPR) NULL));
 L131_=!(L12); 
 if (L131_) {
  SATTR(r,prefetch,ATTR(self,prefetch));
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_PRE_STMT AM_PRE202271760(AM_PRE_STMT self) {
 AM_PRE_STMT ret_val;
 AM_PRE_STMT r;
 AM_PRE_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_PRE_STMT ret_val1;
 AM_PRE_STMT r1;
 AM_PRE_STMT L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 dAM_STMT L5;
 BOOL L6;
 BOOL L71_;
 dAM_EXPR L8;
 if ((self==((AM_PRE_STMT) NULL))) {
  ret_val = ((AM_PRE_STMT) NULL);
  return ret_val;
 }
 create_self = ((AM_PRE_STMT) NULL);
 create_source = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_PRE_STMT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_PRE_STMT_tag;
 L1 = ((AM_PRE_STMT) L2);
 r1 = L1;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 L3 = (ATTR(self,next)==((dAM_STMT) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(self,next);
  SATTR(r,next,(*dAM_ST1032597839[TAG(L5)])(L5));
 }
 SATTR(r,tp,ATTR(self,tp));
 L6 = (ATTR(self,test1)==((dAM_EXPR) NULL));
 L71_=!(L6); 
 if (L71_) {
  L8 = ATTR(self,test1);
  SATTR(r,test1,(*dAM_EX1830308833[TAG(L8)])(L8));
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_PROTECT_STMT AM_PRO1363632140(AM_PROTECT_STMT self) {
 AM_PROTECT_STMT ret_val;
 AM_PROTECT_STMT r;
 FLISTdAM_STMT L11;
 dAM_STMT x;
 AM_PROTECT_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_PROTECT_STMT ret_val1;
 AM_PROTECT_STMT r1;
 FLISTdAM_STMT create_self1;
 FLISTdAM_STMT ret_val2;
 AM_PROTECT_STMT L2;
 OB L3;
 BOOL L4;
 BOOL L51_;
 dAM_STMT L6;
 BOOL L7;
 BOOL L81_;
 dAM_STMT L9;
 dAM_STMT L10;
 BOOL L12;
 BOOL L131_;
 dAM_STMT L14;
 BOOL L15;
 BOOL L161_;
 dAM_STMT L17;
 if ((self==((AM_PROTECT_STMT) NULL))) {
  ret_val = ((AM_PROTECT_STMT) NULL);
  return ret_val;
 }
 create_self = ((AM_PROTECT_STMT) NULL);
 create_source = ATTR(self,source1);
 L3=ZALLOC(sizeof(struct AM_PROTECT_STMT_struct));
 if (L3==NULL) FATAL("Unable to allocate more memory");
 ((OB)L3)->header.tag=AM_PROTECT_STMT_tag;
 L2 = ((AM_PROTECT_STMT) L3);
 r1 = L2;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 L4 = (ATTR(self,next)==((dAM_STMT) NULL));
 L51_=!(L4); 
 if (L51_) {
  L6 = ATTR(self,next);
  SATTR(r,next,(*dAM_ST1032597839[TAG(L6)])(L6));
 }
 L7 = (ATTR(self,body)==((dAM_STMT) NULL));
 L81_=!(L7); 
 if (L81_) {
  L9 = ATTR(self,body);
  SATTR(r,body,(*dAM_ST1032597839[TAG(L9)])(L9));
 }
 SATTR(r,tgts,ATTR(self,tgts));
 create_self1 = ((FLISTdAM_STMT) NULL);
 ret_val2 = ((FLISTdAM_STMT) NULL);
 SATTR(r,stmts,ret_val2);
 {
  struct FLISTd1374595573_frame_struct other1_0;
FLISTd1374595573_frame noname1 = &other1_0;
  L11 = ATTR(self,stmts);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L10 = FLISTd1374595573(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   x = L10;
   L12 = (x==((dAM_STMT) NULL));
   L131_=!(L12); 
   if (L131_) {
    L14 = x;
    x = (*dAM_ST1032597839[TAG(L14)])(L14);
   }
   SATTR(r,stmts,FLISTd506668277(ATTR(r,stmts), x));
  }
 }
 after_loop: ;
 L15 = (ATTR(self,else_stmts)==((dAM_STMT) NULL));
 L161_=!(L15); 
 if (L161_) {
  L17 = ATTR(self,else_stmts);
  SATTR(r,else_stmts,(*dAM_ST1032597839[TAG(L17)])(L17));
 }
 SATTR(r,no_else,ATTR(self,no_else));
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_RAISE_STMT AM_RAI1825238682(AM_RAISE_STMT self) {
 AM_RAISE_STMT ret_val;
 AM_RAISE_STMT r;
 AM_RAISE_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_RAISE_STMT ret_val1;
 AM_RAISE_STMT r1;
 AM_RAISE_STMT L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 dAM_STMT L5;
 BOOL L6;
 BOOL L71_;
 dAM_EXPR L8;
 if ((self==((AM_RAISE_STMT) NULL))) {
  ret_val = ((AM_RAISE_STMT) NULL);
  return ret_val;
 }
 create_self = ((AM_RAISE_STMT) NULL);
 create_source = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_RAISE_STMT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_RAISE_STMT_tag;
 L1 = ((AM_RAISE_STMT) L2);
 r1 = L1;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 L3 = (ATTR(self,next)==((dAM_STMT) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(self,next);
  SATTR(r,next,(*dAM_ST1032597839[TAG(L5)])(L5));
 }
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

AM_RETURN_STMT AM_RET1651971761(AM_RETURN_STMT self) {
 AM_RETURN_STMT ret_val;
 AM_RETURN_STMT r;
 AM_RETURN_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_RETURN_STMT ret_val1;
 AM_RETURN_STMT r1;
 AM_RETURN_STMT L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 dAM_STMT L5;
 BOOL L6;
 BOOL L71_;
 dAM_EXPR L8;
 if ((self==((AM_RETURN_STMT) NULL))) {
  ret_val = ((AM_RETURN_STMT) NULL);
  return ret_val;
 }
 create_self = ((AM_RETURN_STMT) NULL);
 create_source = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_RETURN_STMT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_RETURN_STMT_tag;
 L1 = ((AM_RETURN_STMT) L2);
 r1 = L1;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 L3 = (ATTR(self,next)==((dAM_STMT) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(self,next);
  SATTR(r,next,(*dAM_ST1032597839[TAG(L5)])(L5));
 }
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

AM_SYNC_STMT AM_SYN1962040721(AM_SYNC_STMT self) {
 AM_SYNC_STMT ret_val;
 AM_SYNC_STMT r;
 AM_SYNC_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_SYNC_STMT ret_val1;
 AM_SYNC_STMT r1;
 AM_SYNC_STMT L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 dAM_STMT L5;
 if ((self==((AM_SYNC_STMT) NULL))) {
  ret_val = ((AM_SYNC_STMT) NULL);
  return ret_val;
 }
 create_self = ((AM_SYNC_STMT) NULL);
 create_source = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_SYNC_STMT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_SYNC_STMT_tag;
 L1 = ((AM_SYNC_STMT) L2);
 r1 = L1;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 L3 = (ATTR(self,next)==((dAM_STMT) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(self,next);
  SATTR(r,next,(*dAM_ST1032597839[TAG(L5)])(L5));
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_TYPECASE_STMT AM_TYP1756924987(AM_TYPECASE_STMT self) {
 AM_TYPECASE_STMT ret_val;
 AM_TYPECASE_STMT r;
 FLISTdAM_STMT L11;
 dAM_STMT x;
 AM_TYPECASE_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_TYPECASE_STMT ret_val1;
 AM_TYPECASE_STMT r1;
 FLISTdAM_STMT create_self1;
 FLISTdAM_STMT ret_val2;
 AM_TYPECASE_STMT L2;
 OB L3;
 BOOL L4;
 BOOL L51_;
 dAM_STMT L6;
 dAM_STMT L7;
 BOOL L8;
 BOOL L91_;
 dAM_STMT L10;
 BOOL L12;
 BOOL L131_;
 dAM_STMT L14;
 if ((self==((AM_TYPECASE_STMT) NULL))) {
  ret_val = ((AM_TYPECASE_STMT) NULL);
  return ret_val;
 }
 create_self = ((AM_TYPECASE_STMT) NULL);
 create_source = ATTR(self,source1);
 L3=ZALLOC(sizeof(struct AM_TYPECASE_STMT_struct));
 if (L3==NULL) FATAL("Unable to allocate more memory");
 ((OB)L3)->header.tag=AM_TYPECASE_STMT_tag;
 L2 = ((AM_TYPECASE_STMT) L3);
 r1 = L2;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 L4 = (ATTR(self,next)==((dAM_STMT) NULL));
 L51_=!(L4); 
 if (L51_) {
  L6 = ATTR(self,next);
  SATTR(r,next,(*dAM_ST1032597839[TAG(L6)])(L6));
 }
 SATTR(r,test1,ATTR(self,test1));
 SATTR(r,tgts,ATTR(self,tgts));
 create_self1 = ((FLISTdAM_STMT) NULL);
 ret_val2 = ((FLISTdAM_STMT) NULL);
 SATTR(r,stmts,ret_val2);
 {
  struct FLISTd1374595573_frame_struct other1_0;
FLISTd1374595573_frame noname1 = &other1_0;
  L11 = ATTR(self,stmts);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L7 = FLISTd1374595573(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   x = L7;
   L8 = (x==((dAM_STMT) NULL));
   L91_=!(L8); 
   if (L91_) {
    L10 = x;
    x = (*dAM_ST1032597839[TAG(L10)])(L10);
   }
   SATTR(r,stmts,FLISTd506668277(ATTR(r,stmts), x));
  }
 }
 after_loop: ;
 L12 = (ATTR(self,else_stmts)==((dAM_STMT) NULL));
 L131_=!(L12); 
 if (L131_) {
  L14 = ATTR(self,else_stmts);
  SATTR(r,else_stmts,(*dAM_ST1032597839[TAG(L14)])(L14));
 }
 SATTR(r,no_else,ATTR(self,no_else));
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_UNLOCK_STMT AM_UNL460993177(AM_UNLOCK_STMT self) {
 AM_UNLOCK_STMT ret_val;
 AM_UNLOCK_STMT r;
 AM_UNLOCK_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_UNLOCK_STMT ret_val1;
 AM_UNLOCK_STMT r1;
 AM_UNLOCK_STMT L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 dAM_STMT L5;
 BOOL L6;
 BOOL L71_;
 dAM_EXPR L8;
 if ((self==((AM_UNLOCK_STMT) NULL))) {
  ret_val = ((AM_UNLOCK_STMT) NULL);
  return ret_val;
 }
 create_self = ((AM_UNLOCK_STMT) NULL);
 create_source = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_UNLOCK_STMT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_UNLOCK_STMT_tag;
 L1 = ((AM_UNLOCK_STMT) L2);
 r1 = L1;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 L3 = (ATTR(self,next)==((dAM_STMT) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(self,next);
  SATTR(r,next,(*dAM_ST1032597839[TAG(L5)])(L5));
 }
 L6 = (ATTR(self,lock_ob)==((dAM_EXPR) NULL));
 L71_=!(L6); 
 if (L71_) {
  L8 = ATTR(self,lock_ob);
  SATTR(r,lock_ob,(*dAM_EX1830308833[TAG(L8)])(L8));
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_WAITFOR_STMT AM_WAI1946639376(AM_WAITFOR_STMT self) {
 AM_WAITFOR_STMT ret_val;
 AM_WAITFOR_STMT r;
 AM_WAITFOR_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_WAITFOR_STMT ret_val1;
 AM_WAITFOR_STMT r1;
 AM_WAITFOR_STMT L1;
 OB L2;
 if ((self==((AM_WAITFOR_STMT) NULL))) {
  ret_val = ((AM_WAITFOR_STMT) NULL);
  return ret_val;
 }
 create_self = ((AM_WAITFOR_STMT) NULL);
 create_source = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_WAITFOR_STMT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_WAITFOR_STMT_tag;
 L1 = ((AM_WAITFOR_STMT) L2);
 r1 = L1;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 SATTR(r,prefetch,ATTR(self,prefetch));
 SATTR(r,src1,ATTR(self,src1));
 SATTR(r,dest,ATTR(self,dest));
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_WIT1996971603 AM_WIT1785216238(AM_WIT1996971603 self) {
 AM_WIT1996971603 ret_val;
 AM_WIT1996971603 r;
 ARRAYdAM_EXPR L11;
 dAM_EXPR x;
 ARRAYdAM_EXPR L21;
 AM_WIT1996971603 create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_WIT1996971603 ret_val1;
 AM_WIT1996971603 r1;
 ARRAYdAM_EXPR create_self1;
 INT create_n = INT_zero;
 ARRAYdAM_EXPR ret_val2;
 INT L31 = INT_zero;
 AM_WIT1996971603 L4;
 OB L5;
 BOOL L6;
 BOOL L71_;
 dAM_STMT L8;
 ARRAYdAM_EXPR L9;
 INT L101_;
 ARRAYdAM_EXPR L12;
 INT L13;
 OB L14;
 INT L15;
 INT L161_br;
 dAM_EXPR aL161_;
 dAM_EXPR L18;
 BOOL L19;
 BOOL L201_;
 dAM_EXPR L22;
 INT L171_br;
INT i_L171_=0;
 INT L231_;
 dAM_STMT L24;
 dAM_STMT L25;
 if ((self==((AM_WIT1996971603) NULL))) {
  ret_val = ((AM_WIT1996971603) NULL);
  return ret_val;
 }
 create_self = ((AM_WIT1996971603) NULL);
 create_source = ATTR(self,source1);
 L5=ZALLOC(sizeof(struct AM_WIT1996971603_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=AM_WIT1996971603_tag;
 L4 = ((AM_WIT1996971603) L5);
 r1 = L4;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 L6 = (ATTR(self,next)==((dAM_STMT) NULL));
 L71_=!(L6); 
 if (L71_) {
  L8 = ATTR(self,next);
  SATTR(r,next,(*dAM_ST1032597839[TAG(L8)])(L8));
 }
 create_self1 = ((ARRAYdAM_EXPR) NULL);
 L9 = ATTR(self,objects1);
 L101_=(L9)==NULL?0:ASIZE((ARRAYdAM_EXPR)L9); 
 create_n = L101_;
 L13 = create_n;
 L15=(sizeof(struct ARRAYdAM_EXPR_struct)-sizeof(dAM_EXPR))+(L13)*sizeof(dAM_EXPR);
 L14=ZALLOC_BIG(L15);
 if (L14==NULL) FATAL("Unable to allocate more memory");
 ((OB)L14)->header.tag=ARRAYdAM_EXPR_tag;
#ifdef DESTROY_CHK

   ((OB)L14)->header.destroyed=0;
#endif

 L12 = ((ARRAYdAM_EXPR) L14);
 L12->asize = L13;
 ret_val2 = L12;
 SATTR(r,objects1,ret_val2);
 {
  BOOL f_L161_ = TRUE;
  BOOL f_L171_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = ATTR(self,objects1);
  L161_br=L11==NULL?0:ASIZE((ARRAYdAM_EXPR)L11); 
  while (1) {
   if(L31>=L161_br)  goto after_loop; 
   aL161_=ARR((ARRAYdAM_EXPR)L11,L31); 
   x = aL161_;
   L19 = (x==((dAM_EXPR) NULL));
   L201_=!(L19); 
   if (L201_) {
    L22 = x;
    x = (*dAM_EX1830308833[TAG(L22)])(L22);
   }
   if (f_L171_) {
    f_L171_ = FALSE;
    L21 = ATTR(r,objects1);
    L171_br=L21==NULL?0:ASIZE((ARRAYdAM_EXPR)L21); 
    i_L171_=0;
   }
   if(i_L171_>=L171_br)  goto after_loop; 
   SARR((ARRAYdAM_EXPR)L21,i_L171_,(dAM_EXPR)x); i_L171_++;
   ;
   L231_=INTPLUS(L31,1); 
   L31 = L231_;
  }
 }
 after_loop: ;
 L24 = ATTR(self,near_part);
 SATTR(r,near_part,(*dAM_ST1032597839[TAG(L24)])(L24));
 L25 = ATTR(self,else_part);
 SATTR(r,else_part,(*dAM_ST1032597839[TAG(L25)])(L25));
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_YIELD_STMT AM_YIE197772790(AM_YIELD_STMT self) {
 AM_YIELD_STMT ret_val;
 AM_YIELD_STMT r;
 AM_YIELD_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_YIELD_STMT ret_val1;
 AM_YIELD_STMT r1;
 AM_YIELD_STMT L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 dAM_STMT L5;
 BOOL L6;
 BOOL L71_;
 dAM_EXPR L8;
 if ((self==((AM_YIELD_STMT) NULL))) {
  ret_val = ((AM_YIELD_STMT) NULL);
  return ret_val;
 }
 create_self = ((AM_YIELD_STMT) NULL);
 create_source = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_YIELD_STMT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_YIELD_STMT_tag;
 L1 = ((AM_YIELD_STMT) L2);
 r1 = L1;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 L3 = (ATTR(self,next)==((dAM_STMT) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(self,next);
  SATTR(r,next,(*dAM_ST1032597839[TAG(L5)])(L5));
 }
 L6 = (ATTR(self,val1)==((dAM_EXPR) NULL));
 L71_=!(L6); 
 if (L71_) {
  L8 = ATTR(self,val1);
  SATTR(r,val1,(*dAM_EX1830308833[TAG(L8)])(L8));
 }
 SATTR(r,ret,ATTR(self,ret));
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL STR_CU1807790508(STR_CURSOR self) {
 BOOL ret_val = BOOL_zero;
 STR discard1;
 CHAR L1;
 CHAR L2;
 BOOL L31_;
 L1 = STR_CU2113130984(self);
 L2 = ATTR(self,comment_char1);
 L31_=L1==L2; 
 if (L31_) {
  discard1 = STR_CU1062701511(self, '\n');
  STR_CU713159692(self);
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

BOOL STR_CU2108715877(STR_CURSOR self) {
 BOOL ret_val = BOOL_zero;
 CHAR L1;
 BOOL L21_;
 CHAR L3;
 BOOL L41_;
 L1 = ATTR(self,comment_char1);
 L21_=L1==' '; 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  L3 = ATTR(self,comment_char2);
  L41_=L3==' '; 
  if (L41_) {
   ret_val = STR_CU1807790508(self);
   return ret_val;
  }
  else {
   ret_val = STR_CU557658657(self);
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL STR_CU557658657(STR_CURSOR self) {
 BOOL ret_val = BOOL_zero;
 STR discard1;
 INT L1;
 INT L21_;
 INT L3;
 INT L4;
 BOOL L51_;
 BOOL L6;
 CHAR L7;
 CHAR L8;
 BOOL L91_;
 STR L10;
 INT L11;
 INT L121_;
 INT L13;
 CHAR L141_;
 CHAR L15;
 CHAR L16;
 BOOL L171_;
 L1 = STR_sizerINT(ATTR(self,buf1));
 L21_=INTMINUS(L1,2); 
 L3 = L21_;
 L4 = ATTR(self,index1);
 L51_=(L3)<(L4); 
 if (L51_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  L7 = STR_CU2113130984(self);
  L8 = ATTR(self,comment_char1);
  L91_=L7==L8; 
  if (L91_) {
   L10 = ATTR(self,buf1);
   L11 = ATTR(self,index1);
   L121_=INTPLUS(L11,1); 
   L13 = L121_;
   L141_=ARR((STR)L10,L13); 
   L15 = L141_;
   L16 = ATTR(self,comment_char2);
   L171_=L15==L16; 
   L6 = L171_;
  } else {
   L6 = FALSE;
  }
  if (L6) {
   discard1 = STR_CU1062701511(self, '\n');
   STR_CU713159692(self);
   ret_val = TRUE;
   return ret_val;
  }
  else {
   ret_val = FALSE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL STR_is111530248(STR self, STR s) {
 BOOL ret_val = BOOL_zero;
 INT L11_;
 INT L2;
 BOOL L31_;
 INT L41_;
 INT L5;
 BOOL L61_;
 INT L71_;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 BOOL L13;
 BOOL L141_;
 INT L151_;
 if ((self==((STR) NULL))) {
  if ((s==((STR) NULL))) {
   ret_val = TRUE;
   return ret_val;
  }
  else {
   L11_=ASIZE((STR)s); 
   L2 = L11_;
   L31_=(L2)==(0); 
   if (L31_) {
    ret_val = TRUE;
    return ret_val;
   }
   else {
    ret_val = FALSE;
    return ret_val;
   }
  }
 }
 else {
  if ((s==((STR) NULL))) {
   L41_=ASIZE((STR)self); 
   L5 = L41_;
   L61_=(L5)==(0); 
   if (L61_) {
    ret_val = TRUE;
    return ret_val;
   }
   else {
    ret_val = FALSE;
    return ret_val;
   }
  }
  else {
   L71_=ASIZE((STR)self); 
   L8 = L71_;
   L91_=ASIZE((STR)s); 
   L10 = L91_;
   L121_=(L8)==(L10); 
   L13 = L121_;
   L141_=!(L13); 
   if (L141_) {
    ret_val = FALSE;
    return ret_val;
   }
   else {
    L151_=ASIZE((STR)self); 
    ret_val = STR_is2136368518(self, s, L151_);
    return ret_val;
   }
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL STR_is142167618(STR self, OB arg) {
 BOOL ret_val = BOOL_zero;
 if (arg==NULL) {
  goto other174;
 } else
 switch (TAG(arg)) {
  case STR_tag:
   ret_val = STR_is111530248(self, ((STR) arg));
   return ret_val; break;
  default: ;
  other174: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL STR_is171745744(STR self, STR s) {
 BOOL ret_val = BOOL_zero;
 STR L11;
 CHAR c = CHAR_zero;
 STR L21;
 CHAR sc = CHAR_zero;
 INT L31 = INT_zero;
 CHAR is_lt_self = CHAR_zero;
 CHAR is_lt_c = CHAR_zero;
 BOOL ret_val1 = BOOL_zero;
 CHAR is_lt_self1 = CHAR_zero;
 CHAR is_lt_c1 = CHAR_zero;
 BOOL ret_val2 = BOOL_zero;
 INT L4;
 BOOL L51_;
 INT L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 INT L10;
 BOOL L121_;
 INT L131_br;
 CHAR aL131_;
 INT L141_br;
 CHAR aL141_;
 CHAR L15;
 CHAR L16;
 INT L171_;
 INT L18;
 INT L191_;
 INT L20;
 BOOL L221_;
 INT L231_;
 INT L24;
 INT L251_;
 INT L26;
 BOOL L271_;
 INT L281_;
 INT L29;
 INT L30;
 BOOL L321_;
 L4 = STR_sizerINT(self);
 L51_=(L4)==(0); 
 if (L51_) {
  L6 = STR_sizerINT(s);
  L71_=(L6)==(0); 
  L8 = L71_;
  L91_=!(L8); 
  if (L91_) {
   ret_val = TRUE;
   return ret_val;
  }
  else {
   ret_val = FALSE;
   return ret_val;
  }
 }
 else {
  L10 = STR_sizerINT(s);
  L121_=(L10)==(0); 
  if (L121_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   {
    BOOL f_L131_ = TRUE;
    BOOL f_L141_ = TRUE;
    /* loop index variable */
    L31 = 0;
    L11 = self;
    L21 = s;
    L131_br=ASIZE((STR)L11); 
    L141_br=ASIZE((STR)L21); 
    while (1) {
     if(L31>=L131_br)  goto after_loop; 
     aL131_=ARR((STR)L11,L31); 
     c = aL131_;
     if(L31>=L141_br)  goto after_loop; 
     aL141_=ARR((STR)L21,L31); 
     sc = aL141_;
     is_lt_self = sc;
     is_lt_c = c;
     L171_=(INT)((unsigned char)is_lt_self); 
     L18 = L171_;
     L191_=(INT)((unsigned char)is_lt_c); 
     L20 = L191_;
     L221_=(L18)<(L20); 
     ret_val1 = L221_;
     if (ret_val1) {
      ret_val = FALSE;
      return ret_val;
     }
     else {
      is_lt_self1 = c;
      is_lt_c1 = sc;
      L231_=(INT)((unsigned char)is_lt_self1); 
      L24 = L231_;
      L251_=(INT)((unsigned char)is_lt_c1); 
      L26 = L251_;
      L271_=(L24)<(L26); 
      ret_val2 = L271_;
      if (ret_val2) {
       ret_val = TRUE;
       return ret_val;
      }
     }
     L281_=INTPLUS(L31,1); 
     L31 = L281_;
    }
   }
   after_loop: ;
   L29 = STR_sizerINT(self);
   L30 = STR_sizerINT(s);
   L321_=(L29)<(L30); 
   if (L321_) {
    ret_val = TRUE;
    return ret_val;
   }
   else {
    ret_val = FALSE;
    return ret_val;
   }
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL STR_is2136368518(STR self, STR s, INT i) {
 BOOL ret_val = BOOL_zero;
 STR L11;
 STR L21;
 INT L31 = INT_zero;
 INT L41_br;
 CHAR aL41_;
 INT L51_br;
 CHAR aL51_;
 CHAR L6;
 CHAR L7;
 CHAR L8;
 CHAR L9;
 BOOL L101_;
 BOOL L12;
 BOOL L131_;
 INT L141_;
 {
  BOOL f_L41_ = TRUE;
  BOOL f_L51_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = self;
  L21 = s;
  L41_br=ASIZE((STR)L11); 
  L51_br=ASIZE((STR)L21); 
  while (1) {
   if(L31>=L41_br)  goto after_loop; 
   aL41_=ARR((STR)L11,L31); 
   L7 = aL41_;
   if(L31>=L51_br)  goto after_loop; 
   aL51_=ARR((STR)L21,L31); 
   L9 = aL51_;
   L101_=L7==L9; 
   L12 = L101_;
   L131_=!(L12); 
   if (L131_) {
    ret_val = FALSE;
    return ret_val;
   }
   L141_=INTPLUS(L31,1); 
   L31 = L141_;
  }
 }
 after_loop: ;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

CHAR STR_CU2113130984(STR_CURSOR self) {
 CHAR ret_val = CHAR_zero;
 STR L1;
 INT L2;
 CHAR L31_;
 if (ATTR(self,is_done)) {
  ret_val = '\0';
  return ret_val;
 }
 else {
  L1 = ATTR(self,buf1);
  L2 = ATTR(self,index1);
  L31_=ARR((STR)L1,L2); 
  ret_val = L31_;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT STR_CU1297585490(STR_CURSOR self) {
 INT ret_val = INT_zero;
 INT res = INT_zero;
 BOOL neg_sign = BOOL_zero;
 CHAR L11 = CHAR_zero;
 STR_CURSOR get_opt_sign_sel;
 BOOL ret_val1 = BOOL_zero;
 BOOL res1 = BOOL_zero;
 CHAR L2;
 BOOL L31_;
 CHAR L4;
 BOOL L51_;
 CHAR L6;
 BOOL L71_;
 INT L81_;
 STR_CU1154930915(self);
 get_opt_sign_sel = self;
 L2 = STR_CU2113130984(get_opt_sign_sel);
 L31_=L2=='-'; 
 if (L31_) {
  res1 = TRUE;
  STR_CU713159692(get_opt_sign_sel);
 }
 else {
  L4 = STR_CU2113130984(get_opt_sign_sel);
  L51_=L4=='+'; 
  if (L51_) {
   STR_CU713159692(get_opt_sign_sel);
  }
 }
 ret_val1 = res1;
 neg_sign = ret_val1;
 L6 = STR_CU2113130984(self);
 L71_=L6=='0'; 
 if (L71_) {
  STR_CU713159692(self);
  L11 = STR_CU2113130984(self);
  switch (L11) {
   case 'b': 
    STR_CU713159692(self);
    res = STR_CU745418827(self);
    break;
   case 'o': 
    STR_CU713159692(self);
    res = STR_CU1312667011(self);
    break;
   case 'x': 
    STR_CU713159692(self);
    res = STR_CU1697515046(self);
    break;
   default: ;
    STR_CU1034930507(self);
    res = STR_CU476945601(self);
  }
 }
 else {
  res = STR_CU476945601(self);
 }
 if (neg_sign) {
  L81_=INTMINUS(0,res); 
  res = L81_;
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT STR_CU1312667011(STR_CURSOR self) {
 INT ret_val = INT_zero;
 INT res = INT_zero;
 INT count = INT_zero;
 CHAR oct = CHAR_zero;
 CHAR L11 = CHAR_zero;
 STR_CURSOR get_char_self;
 CHAR ret_val1 = CHAR_zero;
 CHAR res1 = CHAR_zero;
 BOOL L2;
 INT L3;
 BOOL L41_;
 BOOL L5;
 BOOL L61_;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 BOOL L101_;
 BOOL L121_;
 BOOL L131_;
 BOOL L141_;
 BOOL L151_;
 BOOL L161_;
 BOOL L171_;
 INT L181_;
 INT L19;
 INT L20;
 INT L211_;
 INT L221_;
 BOOL L231_;
 while (1) {
  L3 = STR_CU254621724;
  L41_=(count)<(L3); 
  L5 = L41_;
  L61_=!(L5); 
  if (L61_) {
   L71_=(res)==(0); 
   L8 = L71_;
   L91_=!(L8); 
   L2 = L91_;
  } else {
   L2 = FALSE;
  }
  if (L2) {
   SATTR(self,error1,STR_CU1474621917);
   goto after_loop;
  }
  oct = STR_CU2113130984(self);
  L11 = oct;
  switch (L11) {
   case '0': 
    break;
   case '1': 
    L101_=(res)==(0); 
    if (L101_) {
     count = -2;
    }
    break;
   case '2': 
    L121_=(res)==(0); 
    if (L121_) {
     count = -1;
    }
    break;
   case '3': 
    L131_=(res)==(0); 
    if (L131_) {
     count = -1;
    }
    break;
   case '4': 
    L141_=(res)==(0); 
    if (L141_) {
     count = 0;
    }
    break;
   case '5': 
    L151_=(res)==(0); 
    if (L151_) {
     count = 0;
    }
    break;
   case '6': 
    L161_=(res)==(0); 
    if (L161_) {
     count = 0;
    }
    break;
   case '7': 
    L171_=(res)==(0); 
    if (L171_) {
     count = 0;
    }
    break;
   default: ;
    goto after_loop;
  }
  L181_=((unsigned)res)<<((unsigned)3); 
  L19 = L181_;
  get_char_self = self;
  res1 = STR_CU2113130984(get_char_self);
  STR_CU713159692(get_char_self);
  ret_val1 = res1;
  L20 = CHAR_o1022220512(ret_val1);
  L211_=L19|L20; 
  res = L211_;
  L221_=INTPLUS(count,3); 
  count = L221_;
  if (ATTR(self,is_done)) {
   goto after_loop;
  }
 }
 after_loop: ;
 L231_=(count)==(0); 
 if (L231_) {
  SATTR(self,error1,STR_CU610585082);
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT STR_CU1697515046(STR_CURSOR self) {
 INT ret_val = INT_zero;
 INT res = INT_zero;
 INT count = INT_zero;
 CHAR hex = CHAR_zero;
 CHAR L11 = CHAR_zero;
 STR_CURSOR get_char_self;
 CHAR ret_val1 = CHAR_zero;
 CHAR res1 = CHAR_zero;
 BOOL L2;
 BOOL L31_;
 BOOL L4;
 INT L5;
 BOOL L61_;
 BOOL L7;
 BOOL L81_;
 BOOL L91_;
 BOOL L10;
 BOOL L121_;
 BOOL L131_;
 BOOL L141_;
 BOOL L151_;
 BOOL L161_;
 BOOL L171_;
 BOOL L181_;
 BOOL L191_;
 INT L201_;
 INT L21;
 INT L22;
 INT L231_;
 INT L241_;
 L2 = CHAR_i705101630(STR_CU2113130984(self));
 L31_=!(L2); 
 if (L31_) {
  SATTR(self,error1,STR_CU610585082);
  ret_val = 0;
  return ret_val;
 }
 while (1) {
  L5 = STR_CU254621724;
  L61_=(count)<(L5); 
  L7 = L61_;
  L81_=!(L7); 
  if (L81_) {
   L91_=(res)==(0); 
   L10 = L91_;
   L121_=!(L10); 
   L4 = L121_;
  } else {
   L4 = FALSE;
  }
  if (L4) {
   SATTR(self,error1,STR_CU1474621917);
   goto after_loop;
  }
  hex = STR_CU2113130984(self);
  L11 = hex;
  switch (L11) {
   case '0': 
    break;
   case '1': 
    L131_=(res)==(0); 
    if (L131_) {
     count = -3;
    }
    break;
   case '2': 
    L141_=(res)==(0); 
    if (L141_) {
     count = -2;
    }
    break;
   case '3': 
    L151_=(res)==(0); 
    if (L151_) {
     count = -2;
    }
    break;
   case '4': 
    L161_=(res)==(0); 
    if (L161_) {
     count = -1;
    }
    break;
   case '5': 
    L171_=(res)==(0); 
    if (L171_) {
     count = -1;
    }
    break;
   case '6': 
    L181_=(res)==(0); 
    if (L181_) {
     count = -1;
    }
    break;
   case '7': 
    L191_=(res)==(0); 
    if (L191_) {
     count = -1;
    }
    break;
   case '8': 
    break;
   case '9': 
    break;
   case 'a': 
    break;
   case 'b': 
    break;
   case 'c': 
    break;
   case 'd': 
    break;
   case 'e': 
    break;
   case 'f': 
    break;
   case 'A': 
    break;
   case 'B': 
    break;
   case 'C': 
    break;
   case 'D': 
    break;
   case 'E': 
    break;
   case 'F': 
    break;
   default: ;
    goto after_loop;
  }
  L201_=((unsigned)res)<<((unsigned)4); 
  L21 = L201_;
  get_char_self = self;
  res1 = STR_CU2113130984(get_char_self);
  STR_CU713159692(get_char_self);
  ret_val1 = res1;
  L22 = CHAR_h840494345(ret_val1);
  L231_=L21|L22; 
  res = L231_;
  L241_=INTPLUS(count,4); 
  count = L241_;
  if (ATTR(self,is_done)) {
   goto after_loop;
  }
 }
 after_loop: ;
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT STR_CU476945601(STR_CURSOR self) {
 INT ret_val = INT_zero;
 INT res = INT_zero;
 INT num_digits = INT_zero;
 STR_CURSOR get_char_self;
 CHAR ret_val1 = CHAR_zero;
 CHAR res1 = CHAR_zero;
 BOOL L1;
 BOOL L2;
 BOOL L31_;
 BOOL L4;
 BOOL L51_;
 BOOL L6;
 BOOL L71_;
 INT L8;
 BOOL L91_;
 BOOL L10;
 BOOL L111_;
 CHAR L12;
 BOOL L131_;
 BOOL L14;
 BOOL L151_;
 INT L161_;
 INT L171_;
 INT L18;
 INT L19;
 INT L201_;
 BOOL L211_;
 while (1) {
  L2 = ATTR(self,is_done);
  L31_=!(L2); 
  if (L31_) {
   L1 = CHAR_i1303832348(STR_CU2113130984(self));
  } else {
   L1 = FALSE;
  }
  if (L1) {
  }
  else {
   goto after_loop;
  }
  L51_=(res)==(0); 
  L6 = L51_;
  L71_=!(L6); 
  if (L71_) {
   L8 = STR_CU1719265904;
   L91_=(L8)<(num_digits); 
   L4 = L91_;
  } else {
   L4 = FALSE;
  }
  if (L4) {
   SATTR(self,error1,STR_CU1474621917);
   goto after_loop;
  }
  L111_=(res)==(0); 
  if (L111_) {
   L12 = STR_CU2113130984(self);
   L131_=L12=='0'; 
   L14 = L131_;
   L151_=!(L14); 
   L10 = L151_;
  } else {
   L10 = FALSE;
  }
  if (L10) {
   num_digits = 1;
  }
  L161_=INTPLUS(num_digits,1); 
  num_digits = L161_;
  L171_=INTTIMES(res,10); 
  L18 = L171_;
  get_char_self = self;
  res1 = STR_CU2113130984(get_char_self);
  STR_CU713159692(get_char_self);
  ret_val1 = res1;
  L19 = CHAR_d198943415(ret_val1);
  L201_=INTPLUS(L18,L19); 
  res = L201_;
 }
 after_loop: ;
 L211_=(num_digits)==(0); 
 if (L211_) {
  SATTR(self,error1,STR_CU610585082);
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT STR_CU745418827(STR_CURSOR self) {
 INT ret_val = INT_zero;
 INT res = INT_zero;
 INT count = INT_zero;
 CHAR bit1 = CHAR_zero;
 CHAR L11 = CHAR_zero;
 STR_CURSOR get_char_self;
 CHAR ret_val1 = CHAR_zero;
 CHAR res1 = CHAR_zero;
 BOOL L2;
 INT L3;
 BOOL L41_;
 BOOL L5;
 BOOL L61_;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 BOOL L101_;
 INT L121_;
 INT L13;
 INT L14;
 INT L151_;
 INT L161_;
 BOOL L171_;
 while (1) {
  L3 = STR_CU254621724;
  L41_=(count)<(L3); 
  L5 = L41_;
  L61_=!(L5); 
  if (L61_) {
   L71_=(res)==(0); 
   L8 = L71_;
   L91_=!(L8); 
   L2 = L91_;
  } else {
   L2 = FALSE;
  }
  if (L2) {
   SATTR(self,error1,STR_CU1474621917);
   goto after_loop;
  }
  bit1 = STR_CU2113130984(self);
  L11 = bit1;
  switch (L11) {
   case '0': 
    break;
   case '1': 
    L101_=(res)==(1); 
    if (L101_) {
     count = 0;
    }
    break;
   default: ;
    goto after_loop;
  }
  L121_=((unsigned)res)<<((unsigned)1); 
  L13 = L121_;
  get_char_self = self;
  res1 = STR_CU2113130984(get_char_self);
  STR_CU713159692(get_char_self);
  ret_val1 = res1;
  L14 = CHAR_d198943415(ret_val1);
  L151_=L13|L14; 
  res = L151_;
  L161_=INTPLUS(count,1); 
  count = L161_;
  if (ATTR(self,is_done)) {
   goto after_loop;
  }
 }
 after_loop: ;
 L171_=(count)==(0); 
 if (L171_) {
  SATTR(self,error1,STR_CU610585082);
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT STR_hashrINT(STR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 INT i = INT_zero;
 BOOL L1;
 INT L21_;
 INT L3;
 BOOL L41_;
 CHAR L51_;
 CHAR L6;
 INT L71_;
 INT L8;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 INT L131_;
 INT L141_;
 INT L151_;
 INT L16;
 BOOL L171_;
 BOOL L18;
 BOOL L191_;
 if ((self==((STR) NULL))) {
  L1 = TRUE;
 } else {
  L21_=ASIZE((STR)self); 
  L3 = L21_;
  L41_=(L3)==(0); 
  L1 = L41_;
 }
 if (L1) {
  ret_val = 0;
  return ret_val;
 }
 r = 0;
 i = 0;
 while (1) {
  L51_=ARR((STR)self,i); 
  L6 = L51_;
  L71_=(INT)((unsigned char)L6); 
  L8 = L71_;
  L91_=((unsigned)r+(unsigned)L8); 
  L10 = L91_;
  L111_=((unsigned)L10+(unsigned)i); 
  L12 = L111_;
  L131_=((unsigned)L12*(unsigned)19); 
  r = L131_;
  L141_=INTPLUS(i,1); 
  i = L141_;
  L151_=ASIZE((STR)self); 
  L16 = L151_;
  L171_=(i)<(L16); 
  L18 = L171_;
  L191_=!(L18); 
  if (L191_) {
   goto after_loop;
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT STR_lengthrINT(STR self) {
 INT ret_val = INT_zero;
 INT L11_;
 if ((self==((STR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  L11_=ASIZE((STR)self); 
  ret_val = L11_;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT STR_se1235911597(STR self, CHAR c, INT st) {
 INT ret_val = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT r = INT_zero;
 INT L4;
 INT L51_;
 INT L31_,L31_m;
 INT L6;
 CHAR L71_;
 CHAR L8;
 BOOL L91_;
 {
  BOOL f_L31_ = TRUE;
  L11 = st;
  L4 = STR_sizerINT(self);
  L51_=INTMINUS(L4,1); 
  L21 = L51_;
  L31_=L11-1;L31_m=L21; 
  while (1) {
   if(L31_++>=L31_m)  goto after_loop; 
   r = L31_;
   L71_=ARR((STR)self,r); 
   L8 = L71_;
   L91_=L8==c; 
   if (L91_) {
    ret_val = r;
    return ret_val;
   }
  }
 }
 after_loop: ;
 ret_val = -1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT STR_se1633361857(STR self, CHAR c) {
 INT ret_val = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT r = INT_zero;
 INT L4;
 INT L51_;
 INT L31_,L31_m;
 INT L6;
 CHAR L71_;
 CHAR L8;
 BOOL L91_;
 if (CHAR_IS_VOID(c)) {
  ret_val = -1;
  return ret_val;
 }
 {
  BOOL f_L31_ = TRUE;
  L4 = STR_sizerINT(self);
  L51_=INTMINUS(L4,1); 
  L11 = L51_;
  L21 = 0;
  L31_=L11+1;L31_m=L21; 
  while (1) {
   if(L31_--<=L31_m)  goto after_loop; 
   r = L31_;
   L71_=ARR((STR)self,r); 
   L8 = L71_;
   L91_=L8==c; 
   if (L91_) {
    ret_val = r;
    return ret_val;
   }
  }
 }
 after_loop: ;
 ret_val = -1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT STR_se1725540125(STR self, CHAR c) {
 INT ret_val = INT_zero;
 STR L11;
 INT r = INT_zero;
 INT L2;
 CHAR L31_;
 CHAR L4;
 BOOL L51_;
 {
  struct STR_indbrINT_frame_struct other1_0;
STR_indbrINT_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = STR_indbrINT(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = L2;
   L31_=ARR((STR)self,r); 
   L4 = L31_;
   L51_=L4==c; 
   if (L51_) {
    ret_val = r;
    return ret_val;
   }
  }
 }
 after_loop: ;
 ret_val = -1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT STR_se2132153226(STR self, STR s) {
 INT ret_val = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT r = INT_zero;
 BOOL match1 = BOOL_zero;
 STR L31;
 INT L41 = INT_zero;
 STR L51;
 INT L61 = INT_zero;
 INT L81_;
 INT L9;
 INT L101_;
 INT L12;
 INT L131_;
 INT L71_,L71_m;
 INT L14;
 INT L151_c,L151_s;
 CHAR aL151_;
 INT L161_br;
 CHAR aL161_;
 CHAR L17;
 CHAR L18;
 CHAR L19;
 CHAR L20;
 BOOL L221_;
 BOOL L23;
 BOOL L241_;
 INT L251_;
 BOOL L261_;
 if ((s==((STR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 if ((self==((STR) NULL))) {
  ret_val = -1;
  return ret_val;
 }
 {
  BOOL f_L71_ = TRUE;
  L11 = 0;
  L81_=ASIZE((STR)self); 
  L9 = L81_;
  L101_=ASIZE((STR)s); 
  L12 = L101_;
  L131_=INTMINUS(L9,L12); 
  L21 = L131_;
  L71_=L11-1;L71_m=L21; 
  while (1) {
   if(L71_++>=L71_m)  goto after_loop; 
   r = L71_;
   match1 = TRUE;
   {
    BOOL f_L151_ = TRUE;
    BOOL f_L161_ = TRUE;
    /* loop index variable */
    L61 = 0;
    L31 = self;
    L41 = r;
    L51 = s;
    L151_c=L41-1;L151_s=ASIZE((STR)L31); 
    L161_br=ASIZE((STR)L51); 
    while (1) {
     if(++L151_c>=L151_s)  goto after_loop1; aL151_=ARR((STR)L31,L151_c); 
     L18 = aL151_;
     if(L61>=L161_br)  goto after_loop1; 
     aL161_=ARR((STR)L51,L61); 
     L20 = aL161_;
     L221_=L18==L20; 
     L23 = L221_;
     L241_=!(L23); 
     if (L241_) {
      match1 = FALSE;
      goto after_loop1;
     }
     L251_=INTPLUS(L61,1); 
     L61 = L251_;
    }
   }
   after_loop1: ;
   L261_=match1==TRUE; 
   if (L261_) {
    ret_val = r;
    return ret_val;
   }
  }
 }
 after_loop: ;
 ret_val = -1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT STR_sizerINT(STR self) {
 INT ret_val = INT_zero;
 INT L11_;
 if ((self==((STR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  L11_=ASIZE((STR)self); 
  ret_val = L11_;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_FOR1474728133(AM_FORK_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_IF_1192492569(AM_IF_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_INI1792787320(AM_INITIAL_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_INV963567817(AM_INV1167837230 self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_LOC748262786(AM_LOCK_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_LOO738236003(AM_LOOP_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_PAR1239764599(AM_PAR_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_POS2123304705(AM_POST_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_PRE1797857661(AM_PRE_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_PRE2094346420(AM_PREFETCH_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_PRO1670847403(AM_PROTECT_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_RAI1393475571(AM_RAISE_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_RET702658942(AM_RETURN_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_SYN416733298(AM_SYNC_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_TYP720211797(AM_TYPECASE_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_UNL1937638738(AM_UNLOCK_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_WAI442200356(AM_WAITFOR_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_WIT1120935944(AM_WIT1996971603 self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_YIE1303084854(AM_YIELD_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

STR STR_CU1062701511(STR_CURSOR self, CHAR c) {
 STR ret_val;
 STR res;
 STR create_self;
 STR ret_val1;
 STR_CURSOR get_char_self;
 CHAR ret_val2 = CHAR_zero;
 CHAR res1 = CHAR_zero;
 STR plus_self;
 CHAR plus_arg = CHAR_zero;
 STR ret_val3;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val4;
 STR create_self1;
 CHAR create_c = CHAR_zero;
 STR ret_val5;
 STR r;
 extern STR name1;
 BOOL L1;
 CHAR L2;
 BOOL L31_;
 STR L4;
 INT L5;
 OB L6;
 INT L7;
 create_self = ((STR) NULL);
 ret_val1 = ((STR) &name1);
 res = ret_val1;
 while (1) {
  if (ATTR(self,is_done)) {
   L1 = TRUE;
  } else {
   L2 = STR_CU2113130984(self);
   L31_=L2==c; 
   L1 = L31_;
  }
  if (L1) {
   goto after_loop;
  }
  plus_self = res;
  get_char_self = self;
  res1 = STR_CU2113130984(get_char_self);
  STR_CU713159692(get_char_self);
  ret_val2 = res1;
  plus_arg = ret_val2;
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
  ret_val5 = r;
  ret_val4 = ret_val5;
  s = ret_val4;
  ret_val3 = STR_ap1077157958(plus_self, s, TRUE);
  res = ret_val3;
 }
 after_loop: ;
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR STR_ap1077157958(STR self, STR s, BOOL destroy) {
 STR ret_val;
 STR r = ((STR) NULL);
 INT sz = INT_zero;
 INT L1;
 BOOL L21_;
 BOOL L31_;
 STR L4;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 OB L9;
 INT L10;
 INT L111_;
 INT L12;
 INT L141_;
 INT L15;
 BOOL L17;
 BOOL L18;
 BOOL L191_;
 L1 = STR_sizerINT(self);
 L21_=(L1)==(0); 
 if (L21_) {
  ret_val = s;
  return ret_val;
 }
 else {
  sz = STR_sizerINT(s);
  L31_=(sz)==(0); 
  if (L31_) {
   ret_val = self;
   return ret_val;
  }
  else {
   L61_=ASIZE((STR)self); 
   L7 = L61_;
   L81_=INTPLUS(L7,sz); 
   L5 = L81_;
   L10=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L5)*sizeof(CHAR);
   L9=ZALLOC_LEAF_BIG(L10);
   if (L9==NULL) FATAL("Unable to allocate more memory");
   memset(L9,0,L10);
   ((OB)L9)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L9)->header.destroyed=0;
#endif

   L4 = ((STR) L9);
   L4->asize = L5;
   r = L4;
   L111_=ASIZE((STR)self); 
   L12 = L111_;
   STRACOPYN(r,self,L12); 
   ;
   L141_=ASIZE((STR)self); 
   L15 = L141_;
   AREFACOPYB(r,L15,s); 
   ;
  }
 }
 if (destroy) {
  L18 = (s==((STR) NULL));
  L191_=!(L18); 
  L17 = L191_;
 } else {
  L17 = FALSE;
 }
 if (L17) {
  SYSDESTROY(s); 
  ;
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR STR_ap1182453786(STR self, STR s1, STR s2, STR s3, STR s4, STR s5, STR s6) {
 STR ret_val;
 INT p11 = INT_zero;
 INT p2 = INT_zero;
 INT p3 = INT_zero;
 INT p4 = INT_zero;
 INT p5 = INT_zero;
 INT p6 = INT_zero;
 INT p7 = INT_zero;
 STR r;
 INT L11_;
 INT L21_;
 INT L3;
 INT L41_;
 INT L51_;
 INT L6;
 INT L71_;
 INT L81_;
 INT L9;
 INT L101_;
 INT L121_;
 INT L13;
 INT L141_;
 INT L151_;
 INT L16;
 INT L171_;
 INT L181_;
 INT L19;
 INT L201_;
 STR L22;
 INT L23;
 OB L24;
 INT L25;
 if ((self==((STR) NULL))) {
  ret_val = STR_ap1925291910(s1, s2, s3, s4, s5, s6);
  return ret_val;
 }
 if ((s1==((STR) NULL))) {
  ret_val = STR_ap1925291910(self, s2, s3, s4, s5, s6);
  return ret_val;
 }
 if ((s2==((STR) NULL))) {
  ret_val = STR_ap1925291910(self, s1, s3, s4, s5, s6);
  return ret_val;
 }
 if ((s3==((STR) NULL))) {
  ret_val = STR_ap1925291910(self, s1, s2, s4, s5, s6);
  return ret_val;
 }
 if ((s3==((STR) NULL))) {
  ret_val = STR_ap1925291910(self, s1, s2, s3, s5, s6);
  return ret_val;
 }
 if ((s3==((STR) NULL))) {
  ret_val = STR_ap1925291910(self, s1, s2, s3, s4, s6);
  return ret_val;
 }
 if ((s3==((STR) NULL))) {
  ret_val = STR_ap1925291910(self, s1, s2, s3, s4, s5);
  return ret_val;
 }
 L11_=ASIZE((STR)self); 
 p11 = L11_;
 L21_=ASIZE((STR)s1); 
 L3 = L21_;
 L41_=INTPLUS(p11,L3); 
 p2 = L41_;
 L51_=ASIZE((STR)s2); 
 L6 = L51_;
 L71_=INTPLUS(p2,L6); 
 p3 = L71_;
 L81_=ASIZE((STR)s3); 
 L9 = L81_;
 L101_=INTPLUS(p3,L9); 
 p4 = L101_;
 L121_=ASIZE((STR)s4); 
 L13 = L121_;
 L141_=INTPLUS(p4,L13); 
 p5 = L141_;
 L151_=ASIZE((STR)s5); 
 L16 = L151_;
 L171_=INTPLUS(p5,L16); 
 p6 = L171_;
 L181_=ASIZE((STR)s6); 
 L19 = L181_;
 L201_=INTPLUS(p6,L19); 
 p7 = L201_;
 L23 = p7;
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
 AREFACOPY(r,self); 
 ;
 AREFACOPYB(r,p11,s1); 
 ;
 AREFACOPYB(r,p2,s2); 
 ;
 AREFACOPYB(r,p3,s3); 
 ;
 AREFACOPYB(r,p4,s4); 
 ;
 AREFACOPYB(r,p5,s5); 
 ;
 AREFACOPYB(r,p6,s6); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR STR_ap1417971546(STR self, STR s1, STR s2, STR s3) {
 STR ret_val;
 INT p11 = INT_zero;
 INT p2 = INT_zero;
 INT p3 = INT_zero;
 INT p4 = INT_zero;
 STR r;
 INT L11_;
 INT L21_;
 INT L3;
 INT L41_;
 INT L51_;
 INT L6;
 INT L71_;
 INT L81_;
 INT L9;
 INT L101_;
 STR L12;
 INT L13;
 OB L14;
 INT L15;
 if ((self==((STR) NULL))) {
  ret_val = STR_ap1693864410(s1, s2, s3);
  return ret_val;
 }
 if ((s1==((STR) NULL))) {
  ret_val = STR_ap1693864410(self, s2, s3);
  return ret_val;
 }
 if ((s2==((STR) NULL))) {
  ret_val = STR_ap1693864410(self, s1, s3);
  return ret_val;
 }
 if ((s3==((STR) NULL))) {
  ret_val = STR_ap1693864410(self, s1, s2);
  return ret_val;
 }
 L11_=ASIZE((STR)self); 
 p11 = L11_;
 L21_=ASIZE((STR)s1); 
 L3 = L21_;
 L41_=INTPLUS(p11,L3); 
 p2 = L41_;
 L51_=ASIZE((STR)s2); 
 L6 = L51_;
 L71_=INTPLUS(p2,L6); 
 p3 = L71_;
 L81_=ASIZE((STR)s3); 
 L9 = L81_;
 L101_=INTPLUS(p3,L9); 
 p4 = L101_;
 L13 = p4;
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
 AREFACOPY(r,self); 
 ;
 AREFACOPYB(r,p11,s1); 
 ;
 AREFACOPYB(r,p2,s2); 
 ;
 AREFACOPYB(r,p3,s3); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR STR_ap1693864410(STR self, STR s1, STR s2) {
 STR ret_val;
 INT p11 = INT_zero;
 INT p2 = INT_zero;
 INT p3 = INT_zero;
 STR r;
 INT L11_;
 INT L21_;
 INT L3;
 INT L41_;
 INT L51_;
 INT L6;
 INT L71_;
 STR L8;
 INT L9;
 OB L10;
 INT L12;
 if ((self==((STR) NULL))) {
  ret_val = STR_ap2004550586(s1, s2);
  return ret_val;
 }
 if ((s1==((STR) NULL))) {
  ret_val = STR_ap2004550586(self, s2);
  return ret_val;
 }
 if ((s2==((STR) NULL))) {
  ret_val = STR_ap2004550586(self, s1);
  return ret_val;
 }
 L11_=ASIZE((STR)self); 
 p11 = L11_;
 L21_=ASIZE((STR)s1); 
 L3 = L21_;
 L41_=INTPLUS(p11,L3); 
 p2 = L41_;
 L51_=ASIZE((STR)s2); 
 L6 = L51_;
 L71_=INTPLUS(p2,L6); 
 p3 = L71_;
 L9 = p3;
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
 AREFACOPY(r,self); 
 ;
 AREFACOPYB(r,p11,s1); 
 ;
 AREFACOPYB(r,p2,s2); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR STR_ap1925291910(STR self, STR s1, STR s2, STR s3, STR s4, STR s5) {
 STR ret_val;
 INT p11 = INT_zero;
 INT p2 = INT_zero;
 INT p3 = INT_zero;
 INT p4 = INT_zero;
 INT p5 = INT_zero;
 INT p6 = INT_zero;
 STR r;
 INT L11_;
 INT L21_;
 INT L3;
 INT L41_;
 INT L51_;
 INT L6;
 INT L71_;
 INT L81_;
 INT L9;
 INT L101_;
 INT L121_;
 INT L13;
 INT L141_;
 INT L151_;
 INT L16;
 INT L171_;
 STR L18;
 INT L19;
 OB L20;
 INT L22;
 if ((self==((STR) NULL))) {
  ret_val = STR_ap44172742(s1, s2, s3, s4, s5);
  return ret_val;
 }
 if ((s1==((STR) NULL))) {
  ret_val = STR_ap44172742(self, s2, s3, s4, s5);
  return ret_val;
 }
 if ((s2==((STR) NULL))) {
  ret_val = STR_ap44172742(self, s1, s3, s4, s5);
  return ret_val;
 }
 if ((s3==((STR) NULL))) {
  ret_val = STR_ap44172742(self, s1, s2, s4, s5);
  return ret_val;
 }
 if ((s3==((STR) NULL))) {
  ret_val = STR_ap44172742(self, s1, s2, s3, s5);
  return ret_val;
 }
 if ((s3==((STR) NULL))) {
  ret_val = STR_ap44172742(self, s1, s2, s3, s4);
  return ret_val;
 }
 L11_=ASIZE((STR)self); 
 p11 = L11_;
 L21_=ASIZE((STR)s1); 
 L3 = L21_;
 L41_=INTPLUS(p11,L3); 
 p2 = L41_;
 L51_=ASIZE((STR)s2); 
 L6 = L51_;
 L71_=INTPLUS(p2,L6); 
 p3 = L71_;
 L81_=ASIZE((STR)s3); 
 L9 = L81_;
 L101_=INTPLUS(p3,L9); 
 p4 = L101_;
 L121_=ASIZE((STR)s4); 
 L13 = L121_;
 L141_=INTPLUS(p4,L13); 
 p5 = L141_;
 L151_=ASIZE((STR)s5); 
 L16 = L151_;
 L171_=INTPLUS(p5,L16); 
 p6 = L171_;
 L19 = p6;
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
 r = L18;
 AREFACOPY(r,self); 
 ;
 AREFACOPYB(r,p11,s1); 
 ;
 AREFACOPYB(r,p2,s2); 
 ;
 AREFACOPYB(r,p3,s3); 
 ;
 AREFACOPYB(r,p4,s4); 
 ;
 AREFACOPYB(r,p5,s5); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR STR_ap2004550586(STR self, STR s) {
 STR ret_val;
 INT selfsize = INT_zero;
 INT ssize = INT_zero;
 STR r;
 INT L11_;
 INT L21_;
 STR L3;
 INT L4;
 INT L51_;
 OB L6;
 INT L7;
 if ((self==((STR) NULL))) {
  ret_val = s;
  return ret_val;
 }
 if ((s==((STR) NULL))) {
  ret_val = self;
  return ret_val;
 }
 L11_=ASIZE((STR)self); 
 selfsize = L11_;
 L21_=ASIZE((STR)s); 
 ssize = L21_;
 L51_=INTPLUS(selfsize,ssize); 
 L4 = L51_;
 L7=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L4)*sizeof(CHAR);
 L6=ZALLOC_LEAF_BIG(L7);
 if (L6==NULL) FATAL("Unable to allocate more memory");
 memset(L6,0,L7);
 ((OB)L6)->header.tag=STR_tag;
#ifdef DESTROY_CHK

   ((OB)L6)->header.destroyed=0;
#endif

 L3 = ((STR) L6);
 L3->asize = L4;
 r = L3;
 AREFACOPY(r,self); 
 ;
 AREFACOPYB(r,selfsize,s); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR STR_ap44172742(STR self, STR s1, STR s2, STR s3, STR s4) {
 STR ret_val;
 INT p11 = INT_zero;
 INT p2 = INT_zero;
 INT p3 = INT_zero;
 INT p4 = INT_zero;
 INT p5 = INT_zero;
 STR r;
 INT L11_;
 INT L21_;
 INT L3;
 INT L41_;
 INT L51_;
 INT L6;
 INT L71_;
 INT L81_;
 INT L9;
 INT L101_;
 INT L121_;
 INT L13;
 INT L141_;
 STR L15;
 INT L16;
 OB L17;
 INT L18;
 if ((self==((STR) NULL))) {
  ret_val = STR_ap1417971546(s1, s2, s3, s4);
  return ret_val;
 }
 if ((s1==((STR) NULL))) {
  ret_val = STR_ap1417971546(self, s2, s3, s4);
  return ret_val;
 }
 if ((s2==((STR) NULL))) {
  ret_val = STR_ap1417971546(self, s1, s3, s4);
  return ret_val;
 }
 if ((s3==((STR) NULL))) {
  ret_val = STR_ap1417971546(self, s1, s2, s4);
  return ret_val;
 }
 if ((s3==((STR) NULL))) {
  ret_val = STR_ap1417971546(self, s1, s2, s3);
  return ret_val;
 }
 L11_=ASIZE((STR)self); 
 p11 = L11_;
 L21_=ASIZE((STR)s1); 
 L3 = L21_;
 L41_=INTPLUS(p11,L3); 
 p2 = L41_;
 L51_=ASIZE((STR)s2); 
 L6 = L51_;
 L71_=INTPLUS(p2,L6); 
 p3 = L71_;
 L81_=ASIZE((STR)s3); 
 L9 = L81_;
 L101_=INTPLUS(p3,L9); 
 p4 = L101_;
 L121_=ASIZE((STR)s4); 
 L13 = L121_;
 L141_=INTPLUS(p4,L13); 
 p5 = L141_;
 L16 = p5;
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
 AREFACOPY(r,self); 
 ;
 AREFACOPYB(r,p11,s1); 
 ;
 AREFACOPYB(r,p2,s2); 
 ;
 AREFACOPYB(r,p3,s3); 
 ;
 AREFACOPYB(r,p4,s4); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR STR_as1755591216(STR self) {
 STR ret_val;
 BOOL esc = BOOL_zero;
 BOOL oct = BOOL_zero;
 BOOL qt = BOOL_zero;
 INT oval = INT_zero;
 STR L11;
 INT L21 = INT_zero;
 INT L31 = INT_zero;
 CHAR c = CHAR_zero;
 CHAR L41 = CHAR_zero;
 FSTR clear_self;
 INT L51 = INT_zero;
 FSTR plus_self;
 CHAR plus_c = CHAR_zero;
 FSTR ret_val1;
 FSTR plus_self1;
 CHAR plus_c1 = CHAR_zero;
 FSTR ret_val2;
 FSTR plus_self2;
 CHAR plus_c2 = CHAR_zero;
 FSTR ret_val3;
 FSTR plus_self3;
 CHAR plus_c3 = CHAR_zero;
 FSTR ret_val4;
 FSTR plus_self4;
 CHAR plus_c4 = CHAR_zero;
 FSTR ret_val5;
 FSTR plus_self5;
 CHAR plus_c5 = CHAR_zero;
 FSTR ret_val6;
 FSTR plus_self6;
 CHAR plus_c6 = CHAR_zero;
 FSTR ret_val7;
 FSTR plus_self7;
 CHAR plus_c7 = CHAR_zero;
 FSTR ret_val8;
 FSTR plus_self8;
 CHAR plus_c8 = CHAR_zero;
 FSTR ret_val9;
 FSTR plus_self9;
 CHAR plus_c9 = CHAR_zero;
 FSTR ret_val10;
 FSTR plus_self10;
 CHAR plus_c10 = CHAR_zero;
 FSTR ret_val11;
 FSTR plus_self11;
 CHAR plus_c11 = CHAR_zero;
 FSTR ret_val12;
 FSTR plus_self12;
 CHAR plus_c12 = CHAR_zero;
 FSTR ret_val13;
 FSTR str_self;
 STR ret_val14;
 BOOL L6;
 BOOL L7;
 BOOL L8;
 INT L9;
 BOOL L101_;
 CHAR L121_;
 CHAR L13;
 BOOL L141_;
 BOOL L15;
 BOOL L161_;
 INT L17;
 INT L181_;
 INT L19;
 CHAR L201_;
 CHAR L22;
 BOOL L231_;
 BOOL L24;
 BOOL L251_;
 BOOL L26;
 BOOL L271_;
 INT L29;
 INT L301_;
 INT L281_c,L281_m;
 CHAR aL281_;
 CHAR L32;
 BOOL L331_;
 BOOL L34;
 BOOL L351_;
 INT L361_;
 INT L37;
 INT L38;
 INT L391_;
 CHAR L401_;
 BOOL L421_;
 BOOL L431_;
 BOOL L441_;
 INT L451_;
 CHAR L461_;
 if ((self==((STR) NULL))) {
  L8 = TRUE;
 } else {
  L9 = STR_sizerINT(self);
  L101_=(L9)<(2); 
  L8 = L101_;
 }
 if (L8) {
  L7 = TRUE;
 } else {
  L121_=ARR((STR)self,0); 
  L13 = L121_;
  L141_=L13=='\"'; 
  L15 = L141_;
  L161_=!(L15); 
  L7 = L161_;
 }
 if (L7) {
  L6 = TRUE;
 } else {
  L17 = STR_sizerINT(self);
  L181_=INTMINUS(L17,1); 
  L19 = L181_;
  L201_=ARR((STR)self,L19); 
  L22 = L201_;
  L231_=L22=='\"'; 
  L24 = L231_;
  L251_=!(L24); 
  L6 = L251_;
 }
 if (L6) {
  ret_val = ((STR) NULL);
  return ret_val;
 }
 clear_self = STR_buf;
 L26 = (clear_self==((FSTR) NULL));
 L271_=!(L26); 
 if (L271_) {
  SATTR(clear_self,loc,0);
 }
 {
  BOOL f_L281_ = TRUE;
  /* loop index variable */
  L51 = 0;
  L11 = self;
  L21 = 1;
  L29 = STR_sizerINT(self);
  L301_=INTMINUS(L29,2); 
  L31 = L301_;
  L281_c=L21-1;L281_m=L21+L31; 
  while (1) {
   if(++L281_c>=L281_m)  goto after_loop; aL281_=ARR((STR)L11,L281_c); 
   c = aL281_;
   if (qt) {
    L331_=c=='\"'; 
    if (L331_) {
     qt = FALSE;
    }
    else {
     L34 = CHAR_i84735023(c);
     L351_=!(L34); 
     if (L351_) {
      ret_val = ((STR) NULL);
      return ret_val;
     }
    }
   }
   else {
    if (oct) {
     if (CHAR_i696824757(c)) {
      L361_=INTTIMES(oval,8); 
      L37 = L361_;
      L38 = CHAR_o1022220512(c);
      L391_=INTPLUS(L37,L38); 
      oval = L391_;
     }
     else {
      plus_self = STR_buf;
      L401_=(CHAR)oval; 
      plus_c = L401_;
      ret_val1 = FSTR_p399773021(plus_self, plus_c);
      STR_buf = ret_val1;
      oct = FALSE;
      esc = FALSE;
     }
    }
    L421_=!(oct); 
    if (L421_) {
     if (esc) {
      L41 = c;
      switch (L41) {
       case 'a': 
        plus_self1 = STR_buf;
        plus_c1 = '\7';
        ret_val2 = FSTR_p399773021(plus_self1, plus_c1);
        STR_buf = ret_val2;
        esc = FALSE;
        break;
       case 'b': 
        plus_self2 = STR_buf;
        plus_c2 = '\b';
        ret_val3 = FSTR_p399773021(plus_self2, plus_c2);
        STR_buf = ret_val3;
        esc = FALSE;
        break;
       case 'f': 
        plus_self3 = STR_buf;
        plus_c3 = '\f';
        ret_val4 = FSTR_p399773021(plus_self3, plus_c3);
        STR_buf = ret_val4;
        esc = FALSE;
        break;
       case 'n': 
        plus_self4 = STR_buf;
        plus_c4 = '\n';
        ret_val5 = FSTR_p399773021(plus_self4, plus_c4);
        STR_buf = ret_val5;
        esc = FALSE;
        break;
       case 'r': 
        plus_self5 = STR_buf;
        plus_c5 = '\r';
        ret_val6 = FSTR_p399773021(plus_self5, plus_c5);
        STR_buf = ret_val6;
        esc = FALSE;
        break;
       case 't': 
        plus_self6 = STR_buf;
        plus_c6 = '\t';
        ret_val7 = FSTR_p399773021(plus_self6, plus_c6);
        STR_buf = ret_val7;
        esc = FALSE;
        break;
       case 'v': 
        plus_self7 = STR_buf;
        plus_c7 = '\v';
        ret_val8 = FSTR_p399773021(plus_self7, plus_c7);
        STR_buf = ret_val8;
        esc = FALSE;
        break;
       case '\\': 
        plus_self8 = STR_buf;
        plus_c8 = '\\';
        ret_val9 = FSTR_p399773021(plus_self8, plus_c8);
        STR_buf = ret_val9;
        esc = FALSE;
        break;
       case '\"': 
        plus_self9 = STR_buf;
        plus_c9 = '\"';
        ret_val10 = FSTR_p399773021(plus_self9, plus_c9);
        STR_buf = ret_val10;
        esc = FALSE;
        break;
       case '\'': 
        plus_self10 = STR_buf;
        plus_c10 = '\'';
        ret_val11 = FSTR_p399773021(plus_self10, plus_c10);
        STR_buf = ret_val11;
        esc = FALSE;
        break;
       case '0': 
        oct = TRUE;
        oval = CHAR_o1022220512(c);
        break;
       case '1': 
        oct = TRUE;
        oval = CHAR_o1022220512(c);
        break;
       case '2': 
        oct = TRUE;
        oval = CHAR_o1022220512(c);
        break;
       case '3': 
        oct = TRUE;
        oval = CHAR_o1022220512(c);
        break;
       case '4': 
        oct = TRUE;
        oval = CHAR_o1022220512(c);
        break;
       case '5': 
        oct = TRUE;
        oval = CHAR_o1022220512(c);
        break;
       case '6': 
        oct = TRUE;
        oval = CHAR_o1022220512(c);
        break;
       case '7': 
        oct = TRUE;
        oval = CHAR_o1022220512(c);
        break;
       default: ;
        ret_val = ((STR) NULL);
        return ret_val;
      }
     }
     else {
      L431_=c=='\\'; 
      if (L431_) {
       esc = TRUE;
      }
      else {
       L441_=c=='\"'; 
       if (L441_) {
        qt = TRUE;
       }
       else {
        if (CHAR_i725352188(c)) {
         plus_self11 = STR_buf;
         plus_c11 = c;
         ret_val12 = FSTR_p399773021(plus_self11, plus_c11);
         STR_buf = ret_val12;
        }
        else {
         ret_val = ((STR) NULL);
         return ret_val;
        }
       }
      }
     }
    }
   }
   L451_=INTPLUS(L51,1); 
   L51 = L451_;
  }
 }
 after_loop: ;
 if (qt) {
  ret_val = ((STR) NULL);
  return ret_val;
 }
 else {
  if (oct) {
   plus_self12 = STR_buf;
   L461_=(CHAR)oval; 
   plus_c12 = L461_;
   ret_val13 = FSTR_p399773021(plus_self12, plus_c12);
   STR_buf = ret_val13;
  }
  else {
   if (esc) {
    ret_val = ((STR) NULL);
    return ret_val;
   }
  }
 }
 str_self = STR_buf;
 ret_val14 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val14;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR STR_cr1492743419(STR self, EXT_OB s) {
 STR ret_val;
 INT len1 = INT_zero;
 STR r;
 EXT_OB ext;
 STR L1;
 INT L2;
 OB L3;
 INT L4;
 STR L5;
 if ((s==((EXT_OB) NULL))) {
  ret_val = ((STR) NULL);
  return ret_val;
 }
 len1 = strlen(s);
 L2 = len1;
 L4=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L2)*sizeof(CHAR);
 L3=ZALLOC_LEAF_BIG(L4);
 if (L3==NULL) FATAL("Unable to allocate more memory");
 memset(L3,0,L4);
 ((OB)L3)->header.tag=STR_tag;
#ifdef DESTROY_CHK

   ((OB)L3)->header.destroyed=0;
#endif

 L1 = ((STR) L3);
 L1->asize = L2;
 r = L1;
 L5 = r;
 ext = memcpy(((L5==NULL)?NULL:L5->arr_part), s, len1);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR STR_fr1097270334(STR self, FSTR s) {
 STR ret_val;
 INT sz = INT_zero;
 STR r;
 BOOL L11_;
 extern STR name1;
 STR L2;
 INT L3;
 OB L4;
 INT L5;
 sz = FSTR_sizerINT(s);
 L11_=(sz)==(0); 
 if (L11_) {
  ret_val = ((STR) &name1);
  return ret_val;
 }
 L3 = sz;
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
 STRACOPYNF(r,s,sz); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR STR_head_INTrSTR(STR self, INT i) {
 STR ret_val;
 STR r;
 STR L1;
 INT L2;
 OB L3;
 INT L4;
 if ((self==((STR) NULL))) {
  ret_val = ((STR) NULL);
  return ret_val;
 }
 L2 = i;
 L4=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L2)*sizeof(CHAR);
 L3=ZALLOC_LEAF_BIG(L4);
 if (L3==NULL) FATAL("Unable to allocate more memory");
 memset(L3,0,L4);
 ((OB)L3)->header.tag=STR_tag;
#ifdef DESTROY_CHK

   ((OB)L3)->header.destroyed=0;
#endif

 L1 = ((STR) L3);
 L1->asize = L2;
 r = L1;
 AREFACOPY(r,self); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR STR_pl1270664985(STR self, dSTR sarg) {
 STR ret_val;
 BOOL can_delete = BOOL_zero;
 STR s = ((STR) NULL);
 dSTR L1;
 can_delete = FALSE;
 if ((sarg==((dSTR) NULL))) {
  ret_val = self;
  return ret_val;
 }
 L1 = sarg;
 s = (*dSTR_strrSTR[TAG(L1)])(L1);
 ret_val = STR_ap1077157958(self, s, FALSE);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR STR_prettyrSTR(STR self) {
 STR ret_val;
 STR L11;
 CHAR c = CHAR_zero;
 CHAR L21 = CHAR_zero;
 STR s;
 FSTR clear_self;
 FSTR plus_self;
 CHAR plus_c = CHAR_zero;
 FSTR ret_val1;
 FSTR plus_self1;
 CHAR plus_c1 = CHAR_zero;
 FSTR ret_val2;
 FSTR plus_self2;
 CHAR plus_c2 = CHAR_zero;
 FSTR ret_val3;
 FSTR plus_self3;
 CHAR plus_c3 = CHAR_zero;
 FSTR ret_val4;
 FSTR plus_self4;
 CHAR plus_c4 = CHAR_zero;
 FSTR ret_val5;
 FSTR plus_self5;
 CHAR plus_c5 = CHAR_zero;
 FSTR ret_val6;
 FSTR plus_self6;
 CHAR plus_c6 = CHAR_zero;
 FSTR ret_val7;
 FSTR plus_self7;
 CHAR plus_c7 = CHAR_zero;
 FSTR ret_val8;
 FSTR plus_self8;
 CHAR plus_c8 = CHAR_zero;
 FSTR ret_val9;
 FSTR plus_self9;
 CHAR plus_c9 = CHAR_zero;
 FSTR ret_val10;
 FSTR plus_self10;
 CHAR plus_c10 = CHAR_zero;
 FSTR ret_val11;
 FSTR plus_self11;
 CHAR plus_c11 = CHAR_zero;
 FSTR ret_val12;
 FSTR plus_self12;
 CHAR plus_c12 = CHAR_zero;
 FSTR ret_val13;
 FSTR str_self;
 STR ret_val14;
 BOOL L3;
 BOOL L41_;
 CHAR L5;
 BOOL L6;
 BOOL L7;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 BOOL L121_;
 BOOL L13;
 BOOL L141_;
 INT L151_;
 FSTR L16;
 INT L17;
 INT L181_;
 clear_self = STR_buf;
 L3 = (clear_self==((FSTR) NULL));
 L41_=!(L3); 
 if (L41_) {
  SATTR(clear_self,loc,0);
 }
 plus_self = STR_buf;
 plus_c = '\"';
 ret_val1 = FSTR_p399773021(plus_self, plus_c);
 STR_buf = ret_val1;
 {
  struct STR_eltbrCHAR_frame_struct other1_0;
STR_eltbrCHAR_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L5 = STR_eltbrCHAR(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   c = L5;
   if (CHAR_i725352188(c)) {
    L81_=c=='\"'; 
    L9 = L81_;
    L101_=!(L9); 
    L7 = L101_;
   } else {
    L7 = FALSE;
   }
   if (L7) {
    L121_=c=='\\'; 
    L13 = L121_;
    L141_=!(L13); 
    L6 = L141_;
   } else {
    L6 = FALSE;
   }
   if (L6) {
    plus_self1 = STR_buf;
    plus_c1 = c;
    ret_val2 = FSTR_p399773021(plus_self1, plus_c1);
    STR_buf = ret_val2;
   }
   else {
    plus_self2 = STR_buf;
    plus_c2 = '\\';
    ret_val3 = FSTR_p399773021(plus_self2, plus_c2);
    STR_buf = ret_val3;
    L21 = c;
    switch (L21) {
     case '\7': 
      plus_self3 = STR_buf;
      plus_c3 = 'a';
      ret_val4 = FSTR_p399773021(plus_self3, plus_c3);
      STR_buf = ret_val4;
      break;
     case '\b': 
      plus_self4 = STR_buf;
      plus_c4 = 'b';
      ret_val5 = FSTR_p399773021(plus_self4, plus_c4);
      STR_buf = ret_val5;
      break;
     case '\f': 
      plus_self5 = STR_buf;
      plus_c5 = 'f';
      ret_val6 = FSTR_p399773021(plus_self5, plus_c5);
      STR_buf = ret_val6;
      break;
     case '\n': 
      plus_self6 = STR_buf;
      plus_c6 = 'n';
      ret_val7 = FSTR_p399773021(plus_self6, plus_c6);
      STR_buf = ret_val7;
      break;
     case '\r': 
      plus_self7 = STR_buf;
      plus_c7 = 'r';
      ret_val8 = FSTR_p399773021(plus_self7, plus_c7);
      STR_buf = ret_val8;
      break;
     case '\t': 
      plus_self8 = STR_buf;
      plus_c8 = 't';
      ret_val9 = FSTR_p399773021(plus_self8, plus_c8);
      STR_buf = ret_val9;
      break;
     case '\v': 
      plus_self9 = STR_buf;
      plus_c9 = 'v';
      ret_val10 = FSTR_p399773021(plus_self9, plus_c9);
      STR_buf = ret_val10;
      break;
     case '\\': 
      plus_self10 = STR_buf;
      plus_c10 = '\\';
      ret_val11 = FSTR_p399773021(plus_self10, plus_c10);
      STR_buf = ret_val11;
      break;
     case '\"': 
      plus_self11 = STR_buf;
      plus_c11 = '\"';
      ret_val12 = FSTR_p399773021(plus_self11, plus_c11);
      STR_buf = ret_val12;
      break;
     default: ;
      L151_=(INT)((unsigned char)c); 
      s = INT_oc2110318348(L151_);
      L16 = STR_buf;
      L17 = STR_sizerINT(s);
      L181_=INTMINUS(L17,2); 
      STR_buf = FSTR_p1752847026(L16, STR_tail_INTrSTR(s, L181_));
    }
   }
  }
 }
 after_loop: ;
 plus_self12 = STR_buf;
 plus_c12 = '\"';
 ret_val13 = FSTR_p399773021(plus_self12, plus_c12);
 STR_buf = ret_val13;
 str_self = STR_buf;
 ret_val14 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val14;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR STR_strrSTR(STR self) {
 STR ret_val;
 ret_val = self;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR STR_su1627796404(STR self, INT beg) {
 STR ret_val;
 STR r;
 BOOL L11_;
 STR L2;
 INT L3;
 INT L4;
 INT L51_;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 L11_=(beg)==(0); 
 if (L11_) {
  ret_val = ((STR) NULL);
  return ret_val;
 }
 L4 = STR_sizerINT(self);
 L51_=INTMINUS(L4,beg); 
 L3 = L51_;
 L7=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L3)*sizeof(CHAR);
 L6=ZALLOC_LEAF_BIG(L7);
 if (L6==NULL) FATAL("Unable to allocate more memory");
 memset(L6,0,L7);
 ((OB)L6)->header.tag=STR_tag;
#ifdef DESTROY_CHK

   ((OB)L6)->header.destroyed=0;
#endif

 L2 = ((STR) L6);
 L2->asize = L3;
 r = L2;
 L8 = STR_sizerINT(self);
 L91_=INTMINUS(L8,beg); 
 L10 = L91_;
 AREFACOPYBNS(r,0,L10,beg,self); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR STR_su203383946(STR self, INT beg, INT num1) {
 STR ret_val;
 STR r;
 STR L1;
 INT L2;
 OB L3;
 INT L4;
 if ((self==((STR) NULL))) {
  ret_val = ((STR) NULL);
  return ret_val;
 }
 L2 = num1;
 L4=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L2)*sizeof(CHAR);
 L3=ZALLOC_LEAF_BIG(L4);
 if (L3==NULL) FATAL("Unable to allocate more memory");
 memset(L3,0,L4);
 ((OB)L3)->header.tag=STR_tag;
#ifdef DESTROY_CHK

   ((OB)L3)->header.destroyed=0;
#endif

 L1 = ((STR) L3);
 L1->asize = L2;
 r = L1;
 AREFACOPYBNS(r,0,num1,beg,self); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR STR_tail_INTrSTR(STR self, INT i) {
 STR ret_val;
 STR r;
 STR L1;
 INT L2;
 OB L3;
 INT L4;
 INT L51_;
 INT L6;
 INT L71_;
 INT L8;
 if ((self==((STR) NULL))) {
  ret_val = self;
  return ret_val;
 }
 L2 = i;
 L4=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L2)*sizeof(CHAR);
 L3=ZALLOC_LEAF_BIG(L4);
 if (L3==NULL) FATAL("Unable to allocate more memory");
 memset(L3,0,L4);
 ((OB)L3)->header.tag=STR_tag;
#ifdef DESTROY_CHK

   ((OB)L3)->header.destroyed=0;
#endif

 L1 = ((STR) L3);
 L1->asize = L2;
 r = L1;
 L51_=ASIZE((STR)self); 
 L6 = L51_;
 L71_=INTMINUS(L6,i); 
 L8 = L71_;
 AREFACOPYBNS(r,0,i,L8,self); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR STR_upperrSTR(STR self) {
 STR ret_val;
 STR r;
 STR L11;
 STR L21;
 INT L31 = INT_zero;
 STR L4;
 INT L5;
 INT L61_;
 OB L7;
 INT L8;
 INT L91_br;
 CHAR aL91_;
 INT L101_br;
 CHAR L12;
 CHAR L13;
 INT L141_;
 if ((self==((STR) NULL))) {
  ret_val = ((STR) NULL);
  return ret_val;
 }
 L61_=ASIZE((STR)self); 
 L5 = L61_;
 L8=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L5)*sizeof(CHAR);
 L7=ZALLOC_LEAF_BIG(L8);
 if (L7==NULL) FATAL("Unable to allocate more memory");
 memset(L7,0,L8);
 ((OB)L7)->header.tag=STR_tag;
#ifdef DESTROY_CHK

   ((OB)L7)->header.destroyed=0;
#endif

 L4 = ((STR) L7);
 L4->asize = L5;
 r = L4;
 {
  BOOL f_L91_ = TRUE;
  BOOL f_L101_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = self;
  L21 = r;
  L91_br=ASIZE((STR)L11); 
  L101_br=ASIZE((STR)L21); 
  while (1) {
   if(L31>=L101_br)  goto after_loop; 
   if(L31>=L91_br)  goto after_loop; 
   aL91_=ARR((STR)L11,L31); 
   L13 = CHAR_upperrCHAR(aL91_);
   SARR((STR)L21,L31,L13); 
   ;
   L141_=INTPLUS(L31,1); 
   L31 = L141_;
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR SYSTEM1338733266(SYSTEM_LEX self) {
 STR ret_val;
 CHAR c = CHAR_zero;
 FSTR tmp1;
 INT L11 = INT_zero;
 CHAR L21 = CHAR_zero;
 CHAR L31 = CHAR_zero;
 CHAR L41 = CHAR_zero;
 FSTR create_self;
 FSTR ret_val1;
 FSTR aget_self;
 INT aget_ind = INT_zero;
 CHAR ret_val2 = CHAR_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val4;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val5;
 SYSTEM_LEX barf_self;
 STR barf_msg;
 ERR1 create_self1;
 ERR1 ret_val6;
 ERR1 plus_self3;
 dSTR plus_s;
 ERR1 ret_val7;
 FILE1 stderr_self;
 FILE1 ret_val8;
 FILE1 r;
 ERR1 plus_self4;
 dSTR plus_s1;
 FILE1 stderr_self1;
 FILE1 ret_val9;
 FILE1 r1;
 UNIX exit_self;
 INT exit_code = INT_zero;
 FSTR str_self;
 STR ret_val10;
 FSTR plus_self5;
 CHAR plus_c = CHAR_zero;
 FSTR ret_val11;
 SYSTEM_LEX barf_self1;
 STR barf_msg1;
 ERR1 create_self2;
 ERR1 ret_val12;
 ERR1 plus_self6;
 dSTR plus_s2;
 ERR1 ret_val13;
 FILE1 stderr_self2;
 FILE1 ret_val14;
 FILE1 r2;
 ERR1 plus_self7;
 dSTR plus_s3;
 FILE1 stderr_self3;
 FILE1 ret_val15;
 FILE1 r3;
 UNIX exit_self1;
 INT exit_code1 = INT_zero;
 FSTR L5;
 INT L6;
 OB L7;
 INT L8;
 INT L9;
 INT L10;
 BOOL L121_;
 BOOL L13;
 BOOL L141_;
 CHAR L151_;
 INT L16;
 INT L171_;
 extern STR Illegalcharacter;
 extern STR ininputfile;
 FILE1 L18;
 OB L19;
 dSTR L22;
 OB L23;
 FILE1 L24;
 OB L25;
 extern STR Unknow1333352903;
 FILE1 L27;
 OB L28;
 dSTR L30;
 OB L32;
 FILE1 L33;
 OB L34;
 create_self = ((FSTR) NULL);
 L6 = 16;
 L8=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L6)*sizeof(CHAR);
 L7=ZALLOC_LEAF_BIG(L8);
 if (L7==NULL) FATAL("Unable to allocate more memory");
 memset(L7,0,L8);
 ((OB)L7)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L7)->header.destroyed=0;
#endif

 L5 = ((FSTR) L7);
 L5->asize = L6;
 ret_val1 = L5;
 tmp1 = ret_val1;
 while (1) {
  L9 = ATTR(self,pos);
  L10 = FSTR_sizerINT(ATTR(self,buf1));
  L121_=(L9)<(L10); 
  L13 = L121_;
  L141_=!(L13); 
  if (L141_) {
   goto after_loop;
  }
  aget_self = ATTR(self,buf1);
  aget_ind = ATTR(self,pos);
  L151_=ARR((FSTR)aget_self,aget_ind); 
  ret_val2 = L151_;
  c = ret_val2;
  L16 = ATTR(self,pos);
  L171_=INTPLUS(L16,1); 
  SATTR(self,pos,L171_);
  L11 = ATTR(self,lex_state);
  switch (L11) {
   case 0: 
    L21 = c;
    switch (L21) {
     case '-': 
      SATTR(self,lex_state,1);
      break;
     case '\"': 
      SATTR(self,lex_state,2);
      break;
     case '\n': 
      break;
     case ' ': 
      break;
     case '\t': 
      break;
     case '\r': 
      break;
     case '\\': 
      break;
     default: ;
      barf_self = self;
      plus_self = ((STR) &Illegalcharacter);
      plus_sarg = CHAR_prettyrSTR(c);
      ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
      plus_self1 = ret_val3;
      plus_sarg1 = ((STR) &ininputfile);
      ret_val4 = STR_ap2004550586(plus_self1, plus_sarg1);
      plus_self2 = ret_val4;
      plus_sarg2 = ATTR(self,name1);
      ret_val5 = STR_ap2004550586(plus_self2, plus_sarg2);
      barf_msg = ret_val5;
      create_self1 = ((ERR1) NULL);
      ret_val6 = create_self1;
      plus_self3 = ret_val6;
      plus_s = ((dSTR) barf_msg);
      stderr_self = ((FILE1) NULL);
      L19=ZALLOC(sizeof(struct FILE1_struct));
      if (L19==NULL) FATAL("Unable to allocate more memory");
      ((OB)L19)->header.tag=FILE1_tag;
      L18 = ((FILE1) L19);
      r = L18;
      
      SATTR(r,fp,stderr);
      ret_val8 = r;
      FILE_plus_dSTR(ret_val8, plus_s);
      ret_val7 = plus_self3;
      plus_self4 = ret_val7;
      L23=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
      if (L23==NULL) FATAL("Unable to allocate more memory");
      memset(L23,0,sizeof(struct CHAR_boxed_struct));
      ((OB)L23)->header.tag=CHAR_tag;
      L22 = (dSTR)((CHAR_boxed) L23);
      ((CHAR_boxed) L22)->immutable_part = '\n';
      plus_s1 = L22;
      stderr_self1 = ((FILE1) NULL);
      L25=ZALLOC(sizeof(struct FILE1_struct));
      if (L25==NULL) FATAL("Unable to allocate more memory");
      ((OB)L25)->header.tag=FILE1_tag;
      L24 = ((FILE1) L25);
      r1 = L24;
      
      SATTR(r1,fp,stderr);
      ret_val9 = r1;
      FILE_plus_dSTR(ret_val9, plus_s1);
      exit_self = ((UNIX) NULL);
      exit_code = 1;
      exit(exit_code);
    }
    break;
   case 1: 
    L31 = c;
    switch (L31) {
     case '\n': 
      SATTR(self,lex_state,0);
      break;
     case '\r': 
      SATTR(self,lex_state,0);
      break;
     default: ;
    }
    break;
   case 2: 
    L41 = c;
    switch (L41) {
     case '\"': 
      SATTR(self,lex_state,0);
      str_self = tmp1;
      ret_val10 = STR_fr1097270334(((STR) NULL), str_self);
      ret_val = ret_val10;
      return ret_val;
      break;
     case '\\': 
      break;
     default: ;
      plus_self5 = tmp1;
      plus_c = c;
      ret_val11 = FSTR_p399773021(plus_self5, plus_c);
      tmp1 = ret_val11;
    }
    break;
   default: ;
    barf_self1 = self;
    barf_msg1 = ((STR) &Unknow1333352903);
    create_self2 = ((ERR1) NULL);
    ret_val12 = create_self2;
    plus_self6 = ret_val12;
    plus_s2 = ((dSTR) barf_msg1);
    stderr_self2 = ((FILE1) NULL);
    L28=ZALLOC(sizeof(struct FILE1_struct));
    if (L28==NULL) FATAL("Unable to allocate more memory");
    ((OB)L28)->header.tag=FILE1_tag;
    L27 = ((FILE1) L28);
    r2 = L27;
    
    SATTR(r2,fp,stderr);
    ret_val14 = r2;
    FILE_plus_dSTR(ret_val14, plus_s2);
    ret_val13 = plus_self6;
    plus_self7 = ret_val13;
    L32=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
    if (L32==NULL) FATAL("Unable to allocate more memory");
    memset(L32,0,sizeof(struct CHAR_boxed_struct));
    ((OB)L32)->header.tag=CHAR_tag;
    L30 = (dSTR)((CHAR_boxed) L32);
    ((CHAR_boxed) L30)->immutable_part = '\n';
    plus_s3 = L30;
    stderr_self3 = ((FILE1) NULL);
    L34=ZALLOC(sizeof(struct FILE1_struct));
    if (L34==NULL) FATAL("Unable to allocate more memory");
    ((OB)L34)->header.tag=FILE1_tag;
    L33 = ((FILE1) L34);
    r3 = L33;
    
    SATTR(r3,fp,stderr);
    ret_val15 = r3;
    FILE_plus_dSTR(ret_val15, plus_s3);
    exit_self1 = ((UNIX) NULL);
    exit_code1 = 1;
    exit(exit_code1);
  }
 }
 after_loop: ;
 ret_val = ((STR) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SYSTEM_LEX SYSTEM2034253983(SYSTEM_LEX self, STR s) {
 SYSTEM_LEX ret_val;
 SYSTEM_LEX res;
 BFILE f;
 BFILE open_for_read_se;
 STR open_for_read_nm;
 BFILE ret_val1;
 BFILE r;
 BFILE error_self;
 BOOL ret_val2 = BOOL_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 SYSTEM_LEX barf_self;
 STR barf_msg;
 ERR1 create_self;
 ERR1 ret_val4;
 ERR1 plus_self1;
 dSTR plus_s;
 ERR1 ret_val5;
 FILE1 stderr_self;
 FILE1 ret_val6;
 FILE1 r1;
 ERR1 plus_self2;
 dSTR plus_s1;
 FILE1 stderr_self1;
 FILE1 ret_val7;
 FILE1 r2;
 UNIX exit_self;
 INT exit_code = INT_zero;
 SYSTEM_LEX L1;
 OB L2;
 BFILE L3;
 OB L4;
 extern STR rb;
 STR L5;
 STR L6;
 BOOL L7;
 extern STR Couldn2011387578;
 FILE1 L8;
 OB L9;
 dSTR L11;
 OB L12;
 FILE1 L13;
 OB L14;
 L2=ZALLOC(sizeof(struct SYSTEM_LEX_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=SYSTEM_LEX_tag;
 L1 = ((SYSTEM_LEX) L2);
 res = L1;
 SATTR(res,name1,s);
 open_for_read_se = ((BFILE) NULL);
 open_for_read_nm = s;
 L4=ZALLOC(sizeof(struct BFILE_struct));
 if (L4==NULL) FATAL("Unable to allocate more memory");
 ((OB)L4)->header.tag=BFILE_tag;
 L3 = ((BFILE) L4);
 r = L3;
 L5 = open_for_read_nm;
 L6 = ((STR) &rb);
 SATTR(r,fp,fopen(((L5==NULL)?NULL:L5->arr_part), ((L6==NULL)?NULL:L6->arr_part)));
 ret_val1 = r;
 f = ret_val1;
 error_self = f;
 if ((ATTR(error_self,fp)==((EXT_OB) NULL))) {
  L7 = TRUE;
 } else {
  L7 = ferror(ATTR(error_self,fp));
 }
 ret_val2 = L7;
 if (ret_val2) {
  barf_self = self;
  plus_self = ((STR) &Couldn2011387578);
  plus_sarg = s;
  ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
  barf_msg = ret_val3;
  create_self = ((ERR1) NULL);
  ret_val4 = create_self;
  plus_self1 = ret_val4;
  plus_s = ((dSTR) barf_msg);
  stderr_self = ((FILE1) NULL);
  L9=ZALLOC(sizeof(struct FILE1_struct));
  if (L9==NULL) FATAL("Unable to allocate more memory");
  ((OB)L9)->header.tag=FILE1_tag;
  L8 = ((FILE1) L9);
  r1 = L8;
  
  SATTR(r1,fp,stderr);
  ret_val6 = r1;
  FILE_plus_dSTR(ret_val6, plus_s);
  ret_val5 = plus_self1;
  plus_self2 = ret_val5;
  L12=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
  if (L12==NULL) FATAL("Unable to allocate more memory");
  memset(L12,0,sizeof(struct CHAR_boxed_struct));
  ((OB)L12)->header.tag=CHAR_tag;
  L11 = (dSTR)((CHAR_boxed) L12);
  ((CHAR_boxed) L11)->immutable_part = '\n';
  plus_s1 = L11;
  stderr_self1 = ((FILE1) NULL);
  L14=ZALLOC(sizeof(struct FILE1_struct));
  if (L14==NULL) FATAL("Unable to allocate more memory");
  ((OB)L14)->header.tag=FILE1_tag;
  L13 = ((FILE1) L14);
  r2 = L13;
  
  SATTR(r2,fp,stderr);
  ret_val7 = r2;
  FILE_plus_dSTR(ret_val7, plus_s1);
  exit_self = ((UNIX) NULL);
  exit_code = 1;
  exit(exit_code);
 }
 SATTR(res,lex_state,0);
 SATTR(res,buf1,BFILE_fstrrFSTR(f));
 SATTR(res,pos,0);
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT AM_FOR1357281358(AM_FORK_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT AM_IF_1153855302(AM_IF_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT AM_INI81424971(AM_INITIAL_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT AM_INV1267245156(AM_INV1167837230 self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT AM_LOC724705923(AM_LOCK_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT AM_LOO439140156(AM_LOOP_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT AM_PAR309007796(AM_PAR_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT AM_POS1775621496(AM_POST_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT AM_PRE1502061133(AM_PREFETCH_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT AM_PRE1762688610(AM_PRE_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT AM_PRO557659408(AM_PROTECT_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT AM_RAI1337438778(AM_RAISE_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT AM_RET1976133913(AM_RETURN_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT AM_SYN2020605939(AM_SYNC_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT AM_TYP1504178732(AM_TYPECASE_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT AM_UNL1996507749(AM_UNLOCK_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT AM_WAI1727009663(AM_WAITFOR_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT AM_WIT315547139(AM_WIT1996971603 self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT AM_YIE1267845009(AM_YIELD_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

void AM_FOR1689225795(AM_FORK_STMT self, dAM_STMT l) {
 dAM_STMT last1;
 dAM_STMT L1;
 dAM_STMT L2;
 dAM_STMT L3;
 if ((ATTR(self,next)==((dAM_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAM_ST775224435[TAG(L1)])(L1)==((dAM_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAM_ST775224435[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAM_ST1372701974[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AM_FOR790645051(AM_FORK_STMT self, dAM_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AM_IF_1406990231(AM_IF_STMT self, dAM_STMT l) {
 dAM_STMT last1;
 dAM_STMT L1;
 dAM_STMT L2;
 dAM_STMT L3;
 if ((ATTR(self,next)==((dAM_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAM_ST775224435[TAG(L1)])(L1)==((dAM_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAM_ST775224435[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAM_ST1372701974[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AM_IF_993185585(AM_IF_STMT self, dAM_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AM_INI2007284982(AM_INITIAL_STMT self, dAM_STMT l) {
 dAM_STMT last1;
 dAM_STMT L1;
 dAM_STMT L2;
 dAM_STMT L3;
 if ((ATTR(self,next)==((dAM_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAM_ST775224435[TAG(L1)])(L1)==((dAM_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAM_ST775224435[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAM_ST1372701974[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AM_INI2065615916(AM_INITIAL_STMT self, dAM_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AM_INV749070155(AM_INV1167837230 self, dAM_STMT l) {
 dAM_STMT last1;
 dAM_STMT L1;
 dAM_STMT L2;
 dAM_STMT L3;
 if ((ATTR(self,next)==((dAM_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAM_ST775224435[TAG(L1)])(L1)==((dAM_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAM_ST775224435[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAM_ST1372701974[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AM_INV880681253(AM_INV1167837230 self, dAM_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AM_LOC1423220486(AM_LOCK_STMT self, dAM_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AM_LOC962760448(AM_LOCK_STMT self, dAM_STMT l) {
 dAM_STMT last1;
 dAM_STMT L1;
 dAM_STMT L2;
 dAM_STMT L3;
 if ((ATTR(self,next)==((dAM_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAM_ST775224435[TAG(L1)])(L1)==((dAM_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAM_ST775224435[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAM_ST1372701974[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AM_LOO1708786253(AM_LOOP_STMT self, dAM_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AM_LOO952733665(AM_LOOP_STMT self, dAM_STMT l) {
 dAM_STMT last1;
 dAM_STMT L1;
 dAM_STMT L2;
 dAM_STMT L3;
 if ((ATTR(self,next)==((dAM_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAM_ST775224435[TAG(L1)])(L1)==((dAM_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAM_ST775224435[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAM_ST1372701974[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AM_PAR1454262261(AM_PAR_STMT self, dAM_STMT l) {
 dAM_STMT last1;
 dAM_STMT L1;
 dAM_STMT L2;
 dAM_STMT L3;
 if ((ATTR(self,next)==((dAM_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAM_ST775224435[TAG(L1)])(L1)==((dAM_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAM_ST775224435[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAM_ST1372701974[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AM_PAR1838033091(AM_PAR_STMT self, dAM_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AM_POS1908807043(AM_POST_STMT self, dAM_STMT l) {
 dAM_STMT last1;
 dAM_STMT L1;
 dAM_STMT L2;
 dAM_STMT L3;
 if ((ATTR(self,next)==((dAM_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAM_ST775224435[TAG(L1)])(L1)==((dAM_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAM_ST775224435[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAM_ST1372701974[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AM_POS372304913(AM_POST_STMT self, dAM_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AM_PRE1879848758(AM_PREFETCH_STMT self, dAM_STMT l) {
 dAM_STMT last1;
 dAM_STMT L1;
 dAM_STMT L2;
 dAM_STMT L3;
 if ((ATTR(self,next)==((dAM_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAM_ST775224435[TAG(L1)])(L1)==((dAM_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAM_ST775224435[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAM_ST1372701974[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AM_PRE2012355323(AM_PRE_STMT self, dAM_STMT l) {
 dAM_STMT last1;
 dAM_STMT L1;
 dAM_STMT L2;
 dAM_STMT L3;
 if ((ATTR(self,next)==((dAM_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAM_ST775224435[TAG(L1)])(L1)==((dAM_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAM_ST775224435[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAM_ST1372701974[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AM_PRE385237799(AM_PRE_STMT self, dAM_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AM_PRE645865276(AM_PREFETCH_STMT self, dAM_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AM_PRO1589381479(AM_PROTECT_STMT self, dAM_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AM_PRO1885345065(AM_PROTECT_STMT self, dAM_STMT l) {
 dAM_STMT last1;
 dAM_STMT L1;
 dAM_STMT L2;
 dAM_STMT L3;
 if ((ATTR(self,next)==((dAM_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAM_ST775224435[TAG(L1)])(L1)==((dAM_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAM_ST775224435[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAM_ST1372701974[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AM_RAI1178977909(AM_RAISE_STMT self, dAM_STMT l) {
 dAM_STMT last1;
 dAM_STMT L1;
 dAM_STMT L2;
 dAM_STMT L3;
 if ((ATTR(self,next)==((dAM_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAM_ST775224435[TAG(L1)])(L1)==((dAM_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAM_ST775224435[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAM_ST1372701974[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AM_RAI810487631(AM_RAISE_STMT self, dAM_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AM_RET170906974(AM_RETURN_STMT self, dAM_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AM_RET917156604(AM_RETURN_STMT self, dAM_STMT l) {
 dAM_STMT last1;
 dAM_STMT L1;
 dAM_STMT L2;
 dAM_STMT L3;
 if ((ATTR(self,next)==((dAM_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAM_ST775224435[TAG(L1)])(L1)==((dAM_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAM_ST775224435[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAM_ST1372701974[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AM_SYN127320470(AM_SYNC_STMT self, dAM_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AM_SYN631230960(AM_SYNC_STMT self, dAM_STMT l) {
 dAM_STMT last1;
 dAM_STMT L1;
 dAM_STMT L2;
 dAM_STMT L3;
 if ((ATTR(self,next)==((dAM_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAM_ST775224435[TAG(L1)])(L1)==((dAM_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAM_ST775224435[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAM_ST1372701974[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AM_TYP505714135(AM_TYPECASE_STMT self, dAM_STMT l) {
 dAM_STMT last1;
 dAM_STMT L1;
 dAM_STMT L2;
 dAM_STMT L3;
 if ((ATTR(self,next)==((dAM_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAM_ST775224435[TAG(L1)])(L1)==((dAM_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAM_ST775224435[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAM_ST1372701974[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AM_TYP642862155(AM_TYPECASE_STMT self, dAM_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AM_UNL150533138(AM_UNLOCK_STMT self, dAM_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AM_UNL2142830896(AM_UNLOCK_STMT self, dAM_STMT l) {
 dAM_STMT last1;
 dAM_STMT L1;
 dAM_STMT L2;
 dAM_STMT L3;
 if ((ATTR(self,next)==((dAM_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAM_ST775224435[TAG(L1)])(L1)==((dAM_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAM_ST775224435[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAM_ST1372701974[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AM_WAI420031224(AM_WAITFOR_STMT self, dAM_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AM_WAI656698018(AM_WAITFOR_STMT self, dAM_STMT l) {
 dAM_STMT last1;
 dAM_STMT L1;
 dAM_STMT L2;
 dAM_STMT L3;
 if ((ATTR(self,next)==((dAM_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAM_ST775224435[TAG(L1)])(L1)==((dAM_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAM_ST775224435[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAM_ST1372701974[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AM_WIT1831493748(AM_WIT1996971603 self, dAM_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AM_WIT906438282(AM_WIT1996971603 self, dAM_STMT l) {
 dAM_STMT last1;
 dAM_STMT L1;
 dAM_STMT L2;
 dAM_STMT L3;
 if ((ATTR(self,next)==((dAM_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAM_ST775224435[TAG(L1)])(L1)==((dAM_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAM_ST775224435[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAM_ST1372701974[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AM_YIE1088587192(AM_YIELD_STMT self, dAM_STMT l) {
 dAM_STMT last1;
 dAM_STMT L1;
 dAM_STMT L2;
 dAM_STMT L3;
 if ((ATTR(self,next)==((dAM_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAM_ST775224435[TAG(L1)])(L1)==((dAM_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAM_ST775224435[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAM_ST1372701974[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AM_YIE879195878(AM_YIELD_STMT self, dAM_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void STR_CU1034930507(STR_CURSOR self) {
 INT L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 INT L5;
 INT L61_;
 CHAR L7;
 BOOL L81_;
 INT L9;
 INT L101_;
 L1 = ATTR(self,index1);
 L21_=(0)<(L1); 
 L3 = L21_;
 L41_=!(L3); 
 if (L41_) {
  SATTR(self,error1,STR_CU1107107574);
 }
 else {
  L5 = ATTR(self,index1);
  L61_=INTMINUS(L5,1); 
  SATTR(self,index1,L61_);
  L7 = STR_CU2113130984(self);
  L81_=L7=='\n'; 
  if (L81_) {
   L9 = ATTR(self,line_no);
   L101_=INTMINUS(L9,1); 
   SATTR(self,line_no,L101_);
  }
  SATTR(self,is_done,FALSE);
 }
}


#undef IS_ITER
#define IS_ITER 0

void STR_CU1154930915(STR_CURSOR self) {
 BOOL L1;
 BOOL L21_;
 while (1) {
  L1 = ATTR(self,is_done);
  L21_=!(L1); 
  if (L21_) {
  }
  else {
   goto after_loop;
  }
  if (CHAR_i84735023(STR_CU2113130984(self))) {
   STR_CU713159692(self);
  }
  else {
   if (STR_CU2108715877(self)) {
   }
   else {
    goto after_loop;
   }
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void STR_CU1355884946(STR_CURSOR self, STR s) {
 STR_CURSOR clear_error_self;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 INT L4;
 BOOL L51_;
 L1 = (self==((STR_CURSOR) NULL));
 L21_=!(L1); 
 if (L21_) {
  SATTR(self,buf1,s);
  SATTR(self,index1,0);
  SATTR(self,line_no,1);
  SATTR(self,is_done,FALSE);
  clear_error_self = self;
  SATTR(clear_error_self,error1,STR_CU1921064154);
  if ((ATTR(self,buf1)==((STR) NULL))) {
   L3 = TRUE;
  } else {
   L4 = STR_lengthrINT(ATTR(self,buf1));
   L51_=(L4)==(0); 
   L3 = L51_;
  }
  if (L3) {
   SATTR(self,is_done,TRUE);
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

void STR_CU713159692(STR_CURSOR self) {
 INT L1;
 INT L21_;
 INT L3;
 INT L4;
 BOOL L51_;
 BOOL L6;
 BOOL L71_;
 CHAR L8;
 BOOL L91_;
 INT L10;
 INT L111_;
 if (ATTR(self,is_done)) {
  SATTR(self,error1,STR_CU1114535871);
 }
 else {
  L1 = ATTR(self,index1);
  L21_=INTPLUS(L1,1); 
  SATTR(self,index1,L21_);
  L3 = ATTR(self,index1);
  L4 = STR_lengthrINT(ATTR(self,buf1));
  L51_=(L3)<(L4); 
  L6 = L51_;
  L71_=!(L6); 
  if (L71_) {
   SATTR(self,is_done,TRUE);
  }
  else {
   L8 = STR_CU2113130984(self);
   L91_=L8=='\n'; 
   if (L91_) {
    L10 = ATTR(self,line_no);
    L111_=INTPLUS(L10,1); 
    SATTR(self,line_no,L111_);
   }
  }
 }
}


#undef IS_ITER
#define IS_ITER 1

CHAR STR_ae1826948584(STR_ae1826948584_frame frame) {
 CHAR dummy = CHAR_zero;
 CHAR aI_u_I;
 CHAR L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ic=frame->arg1-1;frame->I_u_Is=ASIZE((STR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(++frame->I_u_Ic>=frame->I_u_Is)  goto after_loop; aI_u_I=ARR((STR)frame->self,frame->I_u_Ic); frame->i_I_u_I++;
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

CHAR STR_ae828550501(STR_ae828550501_frame frame) {
 CHAR dummy = CHAR_zero;
 CHAR aI_u_I;
 CHAR L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ic=frame->arg1-1;frame->I_u_Im=frame->arg1+frame->arg2; 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(++frame->I_u_Ic>=frame->I_u_Im)  goto after_loop; aI_u_I=ARR((STR)frame->self,frame->I_u_Ic); frame->i_I_u_I++;
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

CHAR STR_aeltbrCHAR(STR_aeltbrCHAR_frame frame) {
 CHAR dummy = CHAR_zero;
 CHAR aI_u_I;
 CHAR L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((STR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((STR)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

CHAR STR_el1843619312(STR_el1843619312_frame frame) {
 CHAR dummy = CHAR_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 CHAR L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((STR) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = frame->arg1;
  frame->sz = STR_sizerINT(frame->self);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   L41_=ARR((STR)frame->self,frame->i); 
   return L41_;
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

CHAR STR_eltbrCHAR(STR_eltbrCHAR_frame frame) {
 CHAR dummy = CHAR_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 CHAR L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((STR) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = STR_sizerINT(frame->self);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   L41_=ARR((STR)frame->self,frame->i); 
   return L41_;
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

INT STR_indbrINT(STR_indbrINT_frame frame) {
 INT dummy = INT_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 INT L41_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((STR) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = STR_sizerINT(frame->self);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   return frame->i;
   state1:;
   L41_=INTPLUS(frame->i,1); 
   frame->i = L41_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

STR STR_se405450305(STR_se405450305_frame frame) {
 STR dummy = ((STR) NULL);
 dSTR L1;
 dSTR L2;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  }
 state0:;
 frame->state = 1;
 L1 = frame->arg1;
 return (*dSTR_strrSTR[TAG(L1)])(L1);
 state1:;
 while (1) {
  frame->state = 2;
  frame->plus_self = frame->self;
  L2 = frame->arg1;
  frame->plus_sarg = (*dSTR_strrSTR[TAG(L2)])(L2);
  frame->ret_val = STR_ap2004550586(frame->plus_self, frame->plus_sarg);
  return frame->ret_val;
  state2:;
 }
 after_loop: ;
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

STR STR_sp663650766(STR_sp663650766_frame frame) {
 STR dummy = ((STR) NULL);
 INT L1;
 INT L21_;
 INT L3;
 BOOL L41_;
 BOOL L5;
 BOOL L61_;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 INT L101_;
 INT L11;
 INT L121_;
 INT L131_;
 INT L14;
 INT L151_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  }
 state0:;
 if ((frame->self==((STR) NULL))) {
  frame->state = -1;
  return;
 }
 frame->cur_loc = 0;
 while (1) {
  L1 = STR_sizerINT(frame->self);
  L21_=INTMINUS(L1,1); 
  L3 = L21_;
  L41_=(frame->cur_loc)<(L3); 
  L5 = L41_;
  L61_=!(L5); 
  if (L61_) {
   goto after_loop;
  }
  frame->next_loc = STR_se1235911597(frame->self, frame->arg1, frame->cur_loc);
  L71_=(frame->next_loc)<(0); 
  L8 = L71_;
  L91_=!(L8); 
  if (L91_) {
   frame->state = 1;
   L101_=INTMINUS(frame->next_loc,frame->cur_loc); 
   L11 = L101_;
   L121_=INTPLUS(L11,1); 
   return STR_su203383946(frame->self, frame->cur_loc, L121_);
   state1:;
   L131_=INTPLUS(frame->next_loc,1); 
   frame->cur_loc = L131_;
  }
  else {
   frame->state = 2;
   L14 = STR_sizerINT(frame->self);
   L151_=INTMINUS(L14,frame->cur_loc); 
   return STR_su203383946(frame->self, frame->cur_loc, L151_);
   state2:;
   frame->cur_loc = STR_sizerINT(frame->self);
  }
 }
 after_loop: ;
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

STR SYSTEM675309847(SYSTEM675309847_frame frame) {
 STR dummy = ((STR) NULL);
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 while (1) {
  frame->s = SYSTEM1338733266(frame->self);
  if ((frame->s==((STR) NULL))) {
   frame->state = -1;
   return;
  }
  else {
   frame->state = 1;
   return frame->s;
   state1:;
  }
 }
 after_loop: ;
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

dAS_STMT AS_STM1504735434(AS_STM1504735434_frame frame) {
 dAS_STMT dummy = ((dAS_STMT) NULL);
 dAS_STMT L1;
 dAS_STMT L3;
 dAS_STMT L5;
 dAS_STMT L7;
 dAS_STMT L9;
 dAS_STMT L11;
 dAS_STMT L13;
 dAS_STMT L15;
 dAS_STMT L17;
 dAS_STMT L19;
 dAS_STMT L22;
 dAS_STMT L24;
 dAS_STMT L26;
 dAS_STMT L28;
 dAS_STMT L30;
 dAS_STMT L32;
 dAS_STMT L34;
 dAS_STMT L36;
 dAS_STMT L38;
 dAS_STMT L40;
 dAS_STMT L43;
 dAS_STMT L45;
 dAS_STMT L47;
 dAS_STMT L49;
 dAS_STMT L51;
 dAS_STMT L53;
 dAS_STMT L55;
 dAS_STMT L57;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  case 3: goto state3;
  case 4: goto state4;
  case 5: goto state5;
  case 6: goto state6;
  case 7: goto state7;
  case 8: goto state8;
  case 9: goto state9;
  case 10: goto state10;
  case 11: goto state11;
  case 12: goto state12;
  case 13: goto state13;
  case 14: goto state14;
  case 15: goto state15;
  case 16: goto state16;
  case 17: goto state17;
  case 18: goto state18;
  case 19: goto state19;
  case 20: goto state20;
  case 21: goto state21;
  case 22: goto state22;
  case 23: goto state23;
  case 24: goto state24;
  case 25: goto state25;
  case 26: goto state26;
  case 27: goto state27;
  case 28: goto state28;
  case 29: goto state29;
  case 30: goto state30;
  case 31: goto state31;
  case 32: goto state32;
  case 33: goto state33;
  case 34: goto state34;
  case 35: goto state35;
  case 36: goto state36;
  case 37: goto state37;
  case 38: goto state38;
  case 39: goto state39;
  case 40: goto state40;
  case 41: goto state41;
  case 42: goto state42;
  case 43: goto state43;
  case 44: goto state44;
  case 45: goto state45;
  case 46: goto state46;
  case 47: goto state47;
  case 48: goto state48;
  }
 state0:;
 if ((frame->arg1==((dAS_NODE) NULL))) {
  frame->state = -1;
  return;
 }
 frame->as = frame->arg1;
 if (frame->as==NULL) {
  goto other431;
 } else
 switch (TAG(frame->as)) {
  case AS_STMT_LIST_tag:
   {
    frame->nested1 = OBALLOC(AS_STM1504735434_frame);
    frame->nested1->state = 0;
    while (1) {
     frame->state = 1;
     if (frame->nested1->state == 0) {
      frame->L21 = frame->self;
      frame->nested1->self = frame->L21;
     }
     frame->nested1->arg1 = ((dAS_NODE) ATTR(((AS_STMT_LIST) frame->as),stmts));
     L1 = AS_STM1504735434(frame->nested1);
     if (frame->nested1->state == -1) {
      goto after_loop;
     }
     return L1;
     state1:;
    }
   }
   after_loop: ;
   ZFREE(frame->nested1); frame->nested1 = NULL; break;
  case AS_DEC_STMT_tag:
   frame->state = 2;
   return ((dAS_STMT) frame->as);
   state2:; break;
  case AS_ASSIGN_STMT_tag:
   frame->state = 3;
   return ((dAS_STMT) frame->as);
   state3:; break;
  case AS_IF_STMT_tag:
   frame->state = 4;
   return ((dAS_STMT) frame->as);
   state4:;
   {
    frame->nested2 = OBALLOC(AS_STM1504735434_frame);
    frame->nested2->state = 0;
    while (1) {
     frame->state = 5;
     if (frame->nested2->state == 0) {
      frame->L41 = frame->self;
      frame->nested2->self = frame->L41;
     }
     frame->nested2->arg1 = ((dAS_NODE) ATTR(((AS_IF_STMT) frame->as),then_part));
     L3 = AS_STM1504735434(frame->nested2);
     if (frame->nested2->state == -1) {
      goto after_loop1;
     }
     return L3;
     state5:;
    }
   }
   after_loop1: ;
   ZFREE(frame->nested2); frame->nested2 = NULL;
   {
    frame->nested3 = OBALLOC(AS_STM1504735434_frame);
    frame->nested3->state = 0;
    while (1) {
     frame->state = 6;
     if (frame->nested3->state == 0) {
      frame->L61 = frame->self;
      frame->nested3->self = frame->L61;
     }
     frame->nested3->arg1 = ((dAS_NODE) ATTR(((AS_IF_STMT) frame->as),else_part));
     L5 = AS_STM1504735434(frame->nested3);
     if (frame->nested3->state == -1) {
      goto after_loop2;
     }
     return L5;
     state6:;
    }
   }
   after_loop2: ;
   ZFREE(frame->nested3); frame->nested3 = NULL; break;
  case AS_LOOP_STMT_tag:
   frame->state = 7;
   return ((dAS_STMT) frame->as);
   state7:;
   {
    frame->nested4 = OBALLOC(AS_STM1504735434_frame);
    frame->nested4->state = 0;
    while (1) {
     frame->state = 8;
     if (frame->nested4->state == 0) {
      frame->L81 = frame->self;
      frame->nested4->self = frame->L81;
     }
     frame->nested4->arg1 = ((dAS_NODE) ATTR(((AS_LOOP_STMT) frame->as),body));
     L7 = AS_STM1504735434(frame->nested4);
     if (frame->nested4->state == -1) {
      goto after_loop3;
     }
     return L7;
     state8:;
    }
   }
   after_loop3: ;
   ZFREE(frame->nested4); frame->nested4 = NULL; break;
  case AS_RETURN_STMT_tag:
   frame->state = 9;
   return ((dAS_STMT) frame->as);
   state9:; break;
  case AS_YIELD_STMT_tag:
   frame->state = 10;
   return ((dAS_STMT) frame->as);
   state10:; break;
  case AS_QUIT_STMT_tag:
   frame->state = 11;
   return ((dAS_STMT) frame->as);
   state11:; break;
  case AS_CASE_STMT_tag:
   frame->state = 12;
   return ((dAS_STMT) frame->as);
   state12:;
   {
    frame->nested5 = OBALLOC(AS_STM1504735434_frame);
    frame->nested5->state = 0;
    while (1) {
     frame->state = 13;
     if (frame->nested5->state == 0) {
      frame->L101 = frame->self;
      frame->nested5->self = frame->L101;
     }
     frame->nested5->arg1 = ((dAS_NODE) ATTR(((AS_CASE_STMT) frame->as),when_part));
     L9 = AS_STM1504735434(frame->nested5);
     if (frame->nested5->state == -1) {
      goto after_loop4;
     }
     return L9;
     state13:;
    }
   }
   after_loop4: ;
   ZFREE(frame->nested5); frame->nested5 = NULL;
   {
    frame->nested6 = OBALLOC(AS_STM1504735434_frame);
    frame->nested6->state = 0;
    while (1) {
     frame->state = 14;
     if (frame->nested6->state == 0) {
      frame->L121 = frame->self;
      frame->nested6->self = frame->L121;
     }
     frame->nested6->arg1 = ((dAS_NODE) ATTR(((AS_CASE_STMT) frame->as),else_part));
     L11 = AS_STM1504735434(frame->nested6);
     if (frame->nested6->state == -1) {
      goto after_loop5;
     }
     return L11;
     state14:;
    }
   }
   after_loop5: ;
   ZFREE(frame->nested6); frame->nested6 = NULL; break;
  case AS_CASE_WHEN_tag:
   {
    frame->nested7 = OBALLOC(AS_STM1504735434_frame);
    frame->nested7->state = 0;
    while (1) {
     frame->state = 15;
     if (frame->nested7->state == 0) {
      frame->L141 = frame->self;
      frame->nested7->self = frame->L141;
     }
     frame->nested7->arg1 = ((dAS_NODE) ATTR(((AS_CASE_WHEN) frame->as),then_part));
     L13 = AS_STM1504735434(frame->nested7);
     if (frame->nested7->state == -1) {
      goto after_loop6;
     }
     return L13;
     state15:;
    }
   }
   after_loop6: ;
   ZFREE(frame->nested7); frame->nested7 = NULL;
   {
    frame->nested8 = OBALLOC(AS_STM1504735434_frame);
    frame->nested8->state = 0;
    while (1) {
     frame->state = 16;
     if (frame->nested8->state == 0) {
      frame->L161 = frame->self;
      frame->nested8->self = frame->L161;
     }
     frame->nested8->arg1 = ((dAS_NODE) ATTR(((AS_CASE_WHEN) frame->as),next));
     L15 = AS_STM1504735434(frame->nested8);
     if (frame->nested8->state == -1) {
      goto after_loop7;
     }
     return L15;
     state16:;
    }
   }
   after_loop7: ;
   ZFREE(frame->nested8); frame->nested8 = NULL; break;
  case AS_TYPECASE_STMT_tag:
   frame->state = 17;
   return ((dAS_STMT) frame->as);
   state17:;
   {
    frame->nested9 = OBALLOC(AS_STM1504735434_frame);
    frame->nested9->state = 0;
    while (1) {
     frame->state = 18;
     if (frame->nested9->state == 0) {
      frame->L181 = frame->self;
      frame->nested9->self = frame->L181;
     }
     frame->nested9->arg1 = ((dAS_NODE) ATTR(((AS_TYPECASE_STMT) frame->as),when_part));
     L17 = AS_STM1504735434(frame->nested9);
     if (frame->nested9->state == -1) {
      goto after_loop8;
     }
     return L17;
     state18:;
    }
   }
   after_loop8: ;
   ZFREE(frame->nested9); frame->nested9 = NULL;
   {
    frame->nested10 = OBALLOC(AS_STM1504735434_frame);
    frame->nested10->state = 0;
    while (1) {
     frame->state = 19;
     if (frame->nested10->state == 0) {
      frame->L201 = frame->self;
      frame->nested10->self = frame->L201;
     }
     frame->nested10->arg1 = ((dAS_NODE) ATTR(((AS_TYPECASE_STMT) frame->as),else_part));
     L19 = AS_STM1504735434(frame->nested10);
     if (frame->nested10->state == -1) {
      goto after_loop9;
     }
     return L19;
     state19:;
    }
   }
   after_loop9: ;
   ZFREE(frame->nested10); frame->nested10 = NULL; break;
  case AS_TYPECASE_WHEN_tag:
   {
    frame->nested11 = OBALLOC(AS_STM1504735434_frame);
    frame->nested11->state = 0;
    while (1) {
     frame->state = 20;
     if (frame->nested11->state == 0) {
      frame->L231 = frame->self;
      frame->nested11->self = frame->L231;
     }
     frame->nested11->arg1 = ((dAS_NODE) ATTR(((AS_TYPECASE_WHEN) frame->as),then_part));
     L22 = AS_STM1504735434(frame->nested11);
     if (frame->nested11->state == -1) {
      goto after_loop10;
     }
     return L22;
     state20:;
    }
   }
   after_loop10: ;
   ZFREE(frame->nested11); frame->nested11 = NULL;
   {
    frame->nested12 = OBALLOC(AS_STM1504735434_frame);
    frame->nested12->state = 0;
    while (1) {
     frame->state = 21;
     if (frame->nested12->state == 0) {
      frame->L251 = frame->self;
      frame->nested12->self = frame->L251;
     }
     frame->nested12->arg1 = ((dAS_NODE) ATTR(((AS_TYPECASE_WHEN) frame->as),next));
     L24 = AS_STM1504735434(frame->nested12);
     if (frame->nested12->state == -1) {
      goto after_loop11;
     }
     return L24;
     state21:;
    }
   }
   after_loop11: ;
   ZFREE(frame->nested12); frame->nested12 = NULL; break;
  case AS_ASSERT_STMT_tag:
   frame->state = 22;
   return ((dAS_STMT) frame->as);
   state22:; break;
  case AS_PROTECT_STMT_tag:
   frame->state = 23;
   return ((dAS_STMT) frame->as);
   state23:;
   {
    frame->nested13 = OBALLOC(AS_STM1504735434_frame);
    frame->nested13->state = 0;
    while (1) {
     frame->state = 24;
     if (frame->nested13->state == 0) {
      frame->L271 = frame->self;
      frame->nested13->self = frame->L271;
     }
     frame->nested13->arg1 = ((dAS_NODE) ATTR(((AS_PROTECT_STMT) frame->as),body));
     L26 = AS_STM1504735434(frame->nested13);
     if (frame->nested13->state == -1) {
      goto after_loop12;
     }
     return L26;
     state24:;
    }
   }
   after_loop12: ;
   ZFREE(frame->nested13); frame->nested13 = NULL;
   {
    frame->nested14 = OBALLOC(AS_STM1504735434_frame);
    frame->nested14->state = 0;
    while (1) {
     frame->state = 25;
     if (frame->nested14->state == 0) {
      frame->L291 = frame->self;
      frame->nested14->self = frame->L291;
     }
     frame->nested14->arg1 = ((dAS_NODE) ATTR(((AS_PROTECT_STMT) frame->as),when_part));
     L28 = AS_STM1504735434(frame->nested14);
     if (frame->nested14->state == -1) {
      goto after_loop13;
     }
     return L28;
     state25:;
    }
   }
   after_loop13: ;
   ZFREE(frame->nested14); frame->nested14 = NULL;
   {
    frame->nested15 = OBALLOC(AS_STM1504735434_frame);
    frame->nested15->state = 0;
    while (1) {
     frame->state = 26;
     if (frame->nested15->state == 0) {
      frame->L311 = frame->self;
      frame->nested15->self = frame->L311;
     }
     frame->nested15->arg1 = ((dAS_NODE) ATTR(((AS_PROTECT_STMT) frame->as),else_part));
     L30 = AS_STM1504735434(frame->nested15);
     if (frame->nested15->state == -1) {
      goto after_loop14;
     }
     return L30;
     state26:;
    }
   }
   after_loop14: ;
   ZFREE(frame->nested15); frame->nested15 = NULL; break;
  case AS_PROTECT_WHEN_tag:
   {
    frame->nested16 = OBALLOC(AS_STM1504735434_frame);
    frame->nested16->state = 0;
    while (1) {
     frame->state = 27;
     if (frame->nested16->state == 0) {
      frame->L331 = frame->self;
      frame->nested16->self = frame->L331;
     }
     frame->nested16->arg1 = ((dAS_NODE) ATTR(((AS_PROTECT_WHEN) frame->as),then_part));
     L32 = AS_STM1504735434(frame->nested16);
     if (frame->nested16->state == -1) {
      goto after_loop15;
     }
     return L32;
     state27:;
    }
   }
   after_loop15: ;
   ZFREE(frame->nested16); frame->nested16 = NULL;
   {
    frame->nested17 = OBALLOC(AS_STM1504735434_frame);
    frame->nested17->state = 0;
    while (1) {
     frame->state = 28;
     if (frame->nested17->state == 0) {
      frame->L351 = frame->self;
      frame->nested17->self = frame->L351;
     }
     frame->nested17->arg1 = ((dAS_NODE) ATTR(((AS_PROTECT_WHEN) frame->as),next));
     L34 = AS_STM1504735434(frame->nested17);
     if (frame->nested17->state == -1) {
      goto after_loop16;
     }
     return L34;
     state28:;
    }
   }
   after_loop16: ;
   ZFREE(frame->nested17); frame->nested17 = NULL; break;
  case AS_PAR_STMT_tag:
   frame->state = 29;
   return ((dAS_STMT) frame->as);
   state29:;
   {
    frame->nested18 = OBALLOC(AS_STM1504735434_frame);
    frame->nested18->state = 0;
    while (1) {
     frame->state = 30;
     if (frame->nested18->state == 0) {
      frame->L371 = frame->self;
      frame->nested18->self = frame->L371;
     }
     frame->nested18->arg1 = ((dAS_NODE) ATTR(((AS_PAR_STMT) frame->as),body));
     L36 = AS_STM1504735434(frame->nested18);
     if (frame->nested18->state == -1) {
      goto after_loop17;
     }
     return L36;
     state30:;
    }
   }
   after_loop17: ;
   ZFREE(frame->nested18); frame->nested18 = NULL; break;
  case AS_INT2031239268_tag:
   frame->state = 31;
   return ((dAS_STMT) frame->as);
   state31:; break;
  case AS_LOCK_STMT_tag:
   frame->state = 32;
   return ((dAS_STMT) frame->as);
   state32:;
   {
    frame->nested19 = OBALLOC(AS_STM1504735434_frame);
    frame->nested19->state = 0;
    while (1) {
     frame->state = 33;
     if (frame->nested19->state == 0) {
      frame->L391 = frame->self;
      frame->nested19->self = frame->L391;
     }
     frame->nested19->arg1 = ((dAS_NODE) ATTR(((AS_LOCK_STMT) frame->as),then_part));
     L38 = AS_STM1504735434(frame->nested19);
     if (frame->nested19->state == -1) {
      goto after_loop18;
     }
     return L38;
     state33:;
    }
   }
   after_loop18: ;
   ZFREE(frame->nested19); frame->nested19 = NULL;
   {
    frame->nested20 = OBALLOC(AS_STM1504735434_frame);
    frame->nested20->state = 0;
    while (1) {
     frame->state = 34;
     if (frame->nested20->state == 0) {
      frame->L421 = frame->self;
      frame->nested20->self = frame->L421;
     }
     frame->nested20->arg1 = ((dAS_NODE) ATTR(((AS_LOCK_STMT) frame->as),if_when_part));
     L40 = AS_STM1504735434(frame->nested20);
     if (frame->nested20->state == -1) {
      goto after_loop19;
     }
     return L40;
     state34:;
    }
   }
   after_loop19: ;
   ZFREE(frame->nested20); frame->nested20 = NULL;
   {
    frame->nested21 = OBALLOC(AS_STM1504735434_frame);
    frame->nested21->state = 0;
    while (1) {
     frame->state = 35;
     if (frame->nested21->state == 0) {
      frame->L441 = frame->self;
      frame->nested21->self = frame->L441;
     }
     frame->nested21->arg1 = ((dAS_NODE) ATTR(((AS_LOCK_STMT) frame->as),else_part));
     L43 = AS_STM1504735434(frame->nested21);
     if (frame->nested21->state == -1) {
      goto after_loop20;
     }
     return L43;
     state35:;
    }
   }
   after_loop20: ;
   ZFREE(frame->nested21); frame->nested21 = NULL; break;
  case AS_LOCK_IF_WHEN_tag:
   {
    frame->nested22 = OBALLOC(AS_STM1504735434_frame);
    frame->nested22->state = 0;
    while (1) {
     frame->state = 36;
     if (frame->nested22->state == 0) {
      frame->L461 = frame->self;
      frame->nested22->self = frame->L461;
     }
     frame->nested22->arg1 = ((dAS_NODE) ATTR(((AS_LOCK_IF_WHEN) frame->as),then_part));
     L45 = AS_STM1504735434(frame->nested22);
     if (frame->nested22->state == -1) {
      goto after_loop21;
     }
     return L45;
     state36:;
    }
   }
   after_loop21: ;
   ZFREE(frame->nested22); frame->nested22 = NULL;
   {
    frame->nested23 = OBALLOC(AS_STM1504735434_frame);
    frame->nested23->state = 0;
    while (1) {
     frame->state = 37;
     if (frame->nested23->state == 0) {
      frame->L481 = frame->self;
      frame->nested23->self = frame->L481;
     }
     frame->nested23->arg1 = ((dAS_NODE) ATTR(((AS_LOCK_IF_WHEN) frame->as),next));
     L47 = AS_STM1504735434(frame->nested23);
     if (frame->nested23->state == -1) {
      goto after_loop22;
     }
     return L47;
     state37:;
    }
   }
   after_loop22: ;
   ZFREE(frame->nested23); frame->nested23 = NULL; break;
  case AS_UNLOCK_STMT_tag:
   frame->state = 38;
   return ((dAS_STMT) frame->as);
   state38:; break;
  case AS_SYNC_STMT_tag:
   frame->state = 39;
   return ((dAS_STMT) frame->as);
   state39:; break;
  case AS_WIT1063437351_tag:
   frame->state = 40;
   return ((dAS_STMT) frame->as);
   state40:;
   {
    frame->nested24 = OBALLOC(AS_STM1504735434_frame);
    frame->nested24->state = 0;
    while (1) {
     frame->state = 41;
     if (frame->nested24->state == 0) {
      frame->L501 = frame->self;
      frame->nested24->self = frame->L501;
     }
     frame->nested24->arg1 = ((dAS_NODE) ATTR(((AS_WIT1063437351) frame->as),near_part));
     L49 = AS_STM1504735434(frame->nested24);
     if (frame->nested24->state == -1) {
      goto after_loop23;
     }
     return L49;
     state41:;
    }
   }
   after_loop23: ;
   ZFREE(frame->nested24); frame->nested24 = NULL;
   {
    frame->nested25 = OBALLOC(AS_STM1504735434_frame);
    frame->nested25->state = 0;
    while (1) {
     frame->state = 42;
     if (frame->nested25->state == 0) {
      frame->L521 = frame->self;
      frame->nested25->self = frame->L521;
     }
     frame->nested25->arg1 = ((dAS_NODE) ATTR(((AS_WIT1063437351) frame->as),else_part));
     L51 = AS_STM1504735434(frame->nested25);
     if (frame->nested25->state == -1) {
      goto after_loop24;
     }
     return L51;
     state42:;
    }
   }
   after_loop24: ;
   ZFREE(frame->nested25); frame->nested25 = NULL; break;
  case AS_ATTACH_STMT_tag:
   frame->state = 43;
   return ((dAS_STMT) frame->as);
   state43:; break;
  case AS_FORK_STMT_tag:
   frame->state = 44;
   return ((dAS_STMT) frame->as);
   state44:;
   {
    frame->nested26 = OBALLOC(AS_STM1504735434_frame);
    frame->nested26->state = 0;
    while (1) {
     frame->state = 45;
     if (frame->nested26->state == 0) {
      frame->L541 = frame->self;
      frame->nested26->self = frame->L541;
     }
     frame->nested26->arg1 = ((dAS_NODE) ATTR(((AS_FORK_STMT) frame->as),body));
     L53 = AS_STM1504735434(frame->nested26);
     if (frame->nested26->state == -1) {
      goto after_loop25;
     }
     return L53;
     state45:;
    }
   }
   after_loop25: ;
   ZFREE(frame->nested26); frame->nested26 = NULL; break;
  case AS_RAISE_STMT_tag:
   frame->state = 46;
   return ((dAS_STMT) frame->as);
   state46:; break;
  case AS_EXPR_STMT_tag:
   frame->state = 47;
   return ((dAS_STMT) frame->as);
   state47:; break;
  default: ;
  other431: ;
 }
 if (frame->as==NULL) {
  goto other432;
 } else
 switch (TAG(frame->as)) {
  case AS_ASSERT_STMT_tag:
  case AS_ASSIGN_STMT_tag:
  case AS_ATTACH_STMT_tag:
  case AS_CASE_STMT_tag:
  case AS_DEC_STMT_tag:
  case AS_EXPR_STMT_tag:
  case AS_FORK_STMT_tag:
  case AS_IF_STMT_tag:
  case AS_INT2031239268_tag:
  case AS_LOCK_STMT_tag:
  case AS_LOOP_STMT_tag:
  case AS_PAR_STMT_tag:
  case AS_PROTECT_STMT_tag:
  case AS_QUIT_STMT_tag:
  case AS_RAISE_STMT_tag:
  case AS_RETURN_STMT_tag:
  case AS_STMT_tag:
  case AS_SYNC_STMT_tag:
  case AS_TYPECASE_STMT_tag:
  case AS_UNLOCK_STMT_tag:
  case AS_WIT1063437351_tag:
  case AS_YIELD_STMT_tag:
   {
    frame->nested27 = OBALLOC(AS_STM1504735434_frame);
    frame->nested27->state = 0;
    while (1) {
     frame->state = 48;
     if (frame->nested27->state == 0) {
      frame->L561 = frame->self;
      frame->nested27->self = frame->L561;
     }
     L57 = ((dAS_STMT) frame->as);
     frame->nested27->arg1 = ((dAS_NODE) (*dAS_ST460279343[TAG(L57)])(L57));
     L55 = AS_STM1504735434(frame->nested27);
     if (frame->nested27->state == -1) {
      goto after_loop26;
     }
     return L55;
     state48:;
    }
   }
   after_loop26: ;
   ZFREE(frame->nested27); frame->nested27 = NULL; break;
  default: ;
  other432: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

void STR_asetb_CHAR(STR_asetb_CHAR_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((STR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((STR)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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
