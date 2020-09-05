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

typedef struct dGENERATE_AM_struct {
 OB_HEADER header;
 } *dGENERATE_AM;

typedef struct dGET_OPTIONS_struct {
 OB_HEADER header;
 } *dGET_OPTIONS;

typedef struct dMODE_struct {
 OB_HEADER header;
 } *dMODE;

typedef struct dPROG_ERR_struct {
 OB_HEADER header;
 } *dPROG_ERR;

typedef struct dSTR_struct {
 OB_HEADER header;
 } *dSTR;

typedef struct dTP_struct {
 OB_HEADER header;
 } *dTP;

typedef struct ARRAYdTP_struct {/* layout for ARRAY{$TP} */
 OB_HEADER header;
 INT asize;
 struct dTP_struct *arr_part[1];
 } *ARRAYdTP;

typedef struct ARRAYARG_struct {/* layout for ARRAY{ARG} */
 OB_HEADER header;
 INT asize;
 struct ARG_struct *arr_part[1];
 } *ARRAYARG;

typedef struct ARRAYSTR_struct {/* layout for ARRAY{STR} */
 OB_HEADER header;
 INT asize;
 STR arr_part[1];
 } *ARRAYSTR;

typedef struct ARRAYTP_CLASS_struct {/* layout for ARRAY{TP_CLASS} */
 OB_HEADER header;
 INT asize;
 struct TP_CLASS_struct *arr_part[1];
 } *ARRAYTP_CLASS;

typedef struct CONFIG_ROUT_struct {/* layout for CONFIG_ROUT */
 OB_HEADER header;
 struct ARRAYINT_struct *attr_access;
 struct ARRAYINT_struct *near_depends_on;
 struct ARRAYSTR_struct *break1;
 struct ARRAYSTR_struct *declare;
 struct ARRAYSTR_struct *exec;
 struct ARRAYSTR_struct *f_break;
 struct ARRAYSTR_struct *f_declare;
 struct ARRAYSTR_struct *f_exec;
 struct ARRAYSTR_struct *f_init;
 struct ARRAYSTR_struct *f_iter;
 struct ARRAYSTR_struct *f_temp;
 struct ARRAYSTR_struct *f_var;
 struct ARRAYSTR_struct *init;
 struct ARRAYSTR_struct *iter;
 struct ARRAYSTR_struct *raises;
 struct ARRAYSTR_struct *reads;
 struct ARRAYSTR_struct *temp1;
 struct ARRAYSTR_struct *var;
 struct ARRAYSTR_struct *writes;
 STR name1;
 BOOL arith;
 BOOL block1;
 BOOL does_export;
 BOOL does_import;
 BOOL fragile;
 BOOL is_near;
 BOOL no_post;
 BOOL no_pre;
 BOOL raises_any;
 BOOL reads_any;
 BOOL use_index;
 BOOL volatile1;
 BOOL writes_any;
 } *CONFIG_ROUT;

typedef struct CONFIG_TBL_struct {/* layout for CONFIG_TBL */
 OB_HEADER header;
 struct FMAPSTRdCONFIG_struct *tbl;
 } *CONFIG_TBL;

typedef struct ELT_NI351013237_struct {/* layout for ELT_NIL{SIDE_EFFECT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI351013237;

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

typedef struct IFC_struct {/* layout for IFC */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct SIG_TBL_struct *sigs;
 } *IFC;

typedef struct IMPL_struct {/* layout for IMPL */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct ELT_TBL_struct *elts;
 struct IFC_struct *ifc;
 struct TP_CLASS_struct *arr;
 BOOL is_ref_cache;
 BOOL use_ref_cache;
 } *IMPL;

typedef struct INOUT_MODE_struct {/* layout for INOUT_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *INOUT_MODE;

typedef struct IN_MODE_struct {/* layout for IN_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *IN_MODE;

typedef struct MODE_struct {/* layout for MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *MODE;

typedef struct NULL_STAT_struct {/* layout for NULL_STAT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *NULL_STAT;

typedef struct ONCE_MODE_struct {/* layout for ONCE_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ONCE_MODE;

typedef struct OPT_LOCAL_CALL_struct {/* layout for OPT_LOCAL_CALL */
 OB_HEADER header;
 struct AREFFL1698233817_struct *near_local;
 struct ARRAYINT_struct *attr_access;
 struct ARRAYINT_struct *consider;
 struct ARRAYINT_struct *near_depends_on_;
 BOOL is_near;
 } *OPT_LOCAL_CALL;

typedef struct OUT_struct {/* layout for OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OUT;

typedef struct OUT_MODE_struct {/* layout for OUT_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OUT_MODE;

typedef struct SE_CONTEXT_struct {/* layout for SE_CONTEXT */
 OB_HEADER header;
 struct FSETSIDE_EFFECT_struct *set;
 INT weight;
 struct SIG_struct *rsig;
 BOOL forks;
 BOOL has_arith_error;
 BOOL has_export;
 BOOL has_fatal_error;
 BOOL has_import;
 BOOL has_new;
 BOOL has_raise;
 BOOL has_yield_in_loc;
 BOOL is_full;
 BOOL may_block;
 BOOL unsafe;
 } *SE_CONTEXT;

typedef struct SFILE_ID_struct {/* layout for SFILE_ID */
 INT loc;
 } SFILE_ID;
static SFILE_ID SFILE_ID_zero;

typedef struct SFILE_ID_boxed_struct {
 OB_HEADER header;
 SFILE_ID immutable_part;
 } *SFILE_ID_boxed;

typedef struct SIDE_EFFECT_struct {/* layout for SIDE_EFFECT */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct IDENT_struct name1;
 BOOL doeswrite;
 } *SIDE_EFFECT;

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

typedef struct STAT1_struct {/* layout for STAT */
 OB_HEADER header;
 struct FMAPSTRINT_struct *map;
 } *STAT1;

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

typedef struct TP_CONTEXT_struct {/* layout for TP_CONTEXT */
 OB_HEADER header;
 struct ARRAYdTP_struct *ptypes;
 struct ARRAYIDENT_struct *pnames;
 struct PROG_struct *prog;
 struct TP_CLASS_struct *same1;
 BOOL is_abs;
 } *TP_CONTEXT;

typedef struct TP_GRAPH_ABS_DES_struct {/* layout for TP_GRAPH_ABS_DES */
 OB_HEADER header;
 struct FMAPTP291739594_struct *tbl;
 struct PROG_struct *prog;
 struct TP_CLASS_struct *dollar_lock;
 struct TP_CLASS_struct *gate;
 struct TP_CLASS_struct *mutex;
 } *TP_GRAPH_ABS_DES;

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

typedef struct TUPSTRINT_struct {/* layout for TUP{STR,INT} */
 INT t2;
 STR t1;
 } TUPSTRINT;
static TUPSTRINT TUPSTRINT_zero;

typedef struct TUPSTRINT_boxed_struct {
 OB_HEADER header;
 TUPSTRINT immutable_part;
 } *TUPSTRINT_boxed;

typedef struct TUPSTRSIG_struct {/* layout for TUP{STR,SIG} */
 struct SIG_struct *t2;
 STR t1;
 } TUPSTRSIG;
static TUPSTRSIG TUPSTRSIG_zero;

typedef struct TUPSTRSIG_boxed_struct {
 OB_HEADER header;
 TUPSTRSIG immutable_part;
 } *TUPSTRSIG_boxed;

typedef struct TUPTP_858321267_struct {/* layout for TUP{TP_CLASS,FSET{$TP}} */
 struct FSETdTP_struct *t2;
 struct TP_CLASS_struct *t1;
 } TUPTP_858321267;
static TUPTP_858321267 TUPTP_858321267_zero;

typedef struct TUPTP_858321267_boxed_struct {
 OB_HEADER header;
 TUPTP_858321267 immutable_part;
 } *TUPTP_858321267_boxed;

typedef struct UNIX_struct {/* layout for UNIX */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *UNIX;

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

typedef struct AM_COMMENT_STMT_struct {/* layout for AM_COMMENT_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 STR comment1;
 } *AM_COMMENT_STMT;

typedef struct AM_EXPR_STMT_struct {/* layout for AM_EXPR_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *at;
 struct dAM_EXPR_struct *expr;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_EXPR_STMT;

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

typedef struct AM_ROU1916046290_struct {/* layout for AM_ROUT_CALL_EXPR */
 OB_HEADER header;
 struct AS_TYPE_SPEC_struct *as_type;
 struct SFILE_ID_struct source1;
 struct SIG_struct *fun;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *AM_ROU1916046290;

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

typedef struct AM_VAT319982528_struct {/* layout for AM_VATTR_ASSIGN_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *ob;
 struct dAM_EXPR_struct *val1;
 struct dTP_struct *real_tp;
 struct IDENT_struct at;
 struct SFILE_ID_struct source1;
 } *AM_VAT319982528;

typedef struct ARG_struct {/* layout for ARG */
 OB_HEADER header;
 struct dMODE_struct *mode1;
 struct dTP_struct *tp;
 BOOL issame;
 } *ARG;

typedef struct ARRAYA827580689_frame_struct {
 INT state;
 ARRAYARG self;/* Formal argument: self */
 ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA827580689_frame;

typedef struct ARRAYA409651007_frame_struct {
 INT state;
 ARRAYARG self;/* Formal argument: self */
 ARG arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA409651007_frame;

typedef struct ARRAYBOOL_struct {/* layout for ARRAY{BOOL} */
 OB_HEADER header;
 INT asize;
 BOOL arr_part[1];
 } *ARRAYBOOL;

typedef struct ARRAYB1390856792_frame_struct {
 INT state;
 ARRAYBOOL self;/* Formal argument: self */
 BOOL ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYB1390856792_frame;

typedef struct ARRAYB741586158_frame_struct {
 INT state;
 ARRAYBOOL self;/* Formal argument: self */
 BOOL arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYB741586158_frame;

typedef struct ARRAYINT_struct {/* layout for ARRAY{INT} */
 OB_HEADER header;
 INT asize;
 INT arr_part[1];
 } *ARRAYINT;

typedef struct ARRAYS1578977701_frame_struct {
 INT state;
 ARRAYSTR self;/* Formal argument: self */
 STR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYS1578977701_frame;

typedef struct ARRAYS1161048019_frame_struct {
 INT state;
 ARRAYSTR self;/* Formal argument: self */
 STR arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYS1161048019_frame;

typedef struct ARRAYT1851819274_frame_struct {
 INT state;
 ARRAYTP_CLASS self;/* Formal argument: self */
 TP_CLASS ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT1851819274_frame;

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

typedef struct AS_CONST_DEF_struct {/* layout for AS_CONST_DEF */
 OB_HEADER header;
 struct dAS_CLASS_ELT_struct *next;
 struct dAS_EXPR_struct *init;
 struct AS_TYPE_SPEC_struct *tp;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL is_private;
 } *AS_CONST_DEF;

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

typedef struct FLISTF7594819_struct {/* layout for FLIST{FLIST{$AM_CONST}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct FLISTdAM_CONST_struct *arr_part[1];
 } *FLISTF7594819;

typedef struct FMAPSTRINT_struct {/* layout for FMAP{STR,INT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRINT_struct arr_part[1];
 } *FMAPSTRINT;

typedef struct FMAPST961226608_frame_struct {
 INT state;
 FMAPSTRINT self;/* Formal argument: self */
 STR ret_val2;
 FMAPSTRINT L61;
 STR r;
 INT L31;
 FMAPSTRINT is_key_nil_self;
 STR is_key_nil_e;
 BOOL ret_val;
 ELT_NILSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPST961226608_frame;

typedef struct FMAPST1937251125_frame_struct {
 INT state;
 FMAPSTRINT self;/* Formal argument: self */
 INT ret_val2;
 FMAPSTRINT L61;
 TUPSTRINT e;
 INT L31;
 FMAPSTRINT is_key_nil_self;
 STR is_key_nil_e;
 BOOL ret_val;
 ELT_NILSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPST1937251125_frame;

typedef struct FMAPSTRSIG_struct {/* layout for FMAP{STR,SIG} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRSIG_struct arr_part[1];
 } *FMAPSTRSIG;

typedef struct FMAPTP291739594_struct {/* layout for FMAP{TP_CLASS,FSET{$TP}} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPTP_858321267_struct arr_part[1];
 } *FMAPTP291739594;

typedef struct FSETdTP_struct {/* layout for FSET{$TP} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct dTP_struct *arr_part[1];
 } *FSETdTP;

typedef struct FSETdTP_eltbrdTP_frame_struct {
 INT state;
 FSETdTP self;/* Formal argument: self */
 dTP ret_val1;
 FSETdTP L61;
 dTP r;
 INT i;
 INT sz;
 INT L31;
 FSETdTP is_elt_nil_self;
 dTP is_elt_nil_e;
 BOOL ret_val;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETdTP_eltbrdTP_frame;

typedef struct FSETSIDE_EFFECT_struct {/* layout for FSET{SIDE_EFFECT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct SIDE_EFFECT_struct *arr_part[1];
 } *FSETSIDE_EFFECT;

typedef struct FSETSI2093978094_frame_struct {
 INT state;
 FSETSIDE_EFFECT self;/* Formal argument: self */
 SIDE_EFFECT ret_val2;
 FSETSIDE_EFFECT L61;
 SIDE_EFFECT r;
 INT i;
 INT sz;
 INT L31;
 FSETSIDE_EFFECT is_elt_nil_self;
 SIDE_EFFECT is_elt_nil_e;
 BOOL ret_val;
 ELT_NI351013237 is_elt_nil_self1;
 SIDE_EFFECT is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETSI2093978094_frame;

typedef struct FSETSIG_struct {/* layout for FSET{SIG} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct SIG_struct *arr_part[1];
 } *FSETSIG;

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

extern ARRAYTP_CLASS TP_BUI1236844942;
extern ARRAYTP_CLASS TP_BUI1236844961;
extern ARRAYTP_CLASS TP_BUI1517259338;
extern CS_OPTIONS SE_CON1373267262;
extern FMAPSTRSIG SIG_sigs;
extern IDENT IDENT_1666445894;
extern INOUT_MODE MODES_inout_mode;
extern INT AS_TYP1853679690;
extern INT TP_KIN462481068;
extern INT TP_KIND_ext_c_tp;
extern INT TP_KIND_val_tp;
extern IN_MODE MODES_in_mode;
extern ONCE_MODE MODES_once_mode;
extern OUT_MODE MODES_out_mode;
extern PROG SE_CONTEXT_prog;
extern TP_CLASS TP_BUI1032249067;
extern TP_CLASS TP_BUI1116976640;
extern TP_CLASS TP_BUI1194559023;
extern TP_CLASS TP_BUI1370948386;
extern TP_CLASS TP_BUI1805763264;
extern TP_CLASS TP_BUI2088674906;
extern TP_CLASS TP_BUI374366290;
extern TP_CLASS TP_BUI389447236;
extern TP_CLASS TP_BUI403444172;
extern TP_CLASS TP_BUI633637838;
extern TP_CLASS TP_BUILTIN_bool;
extern TP_CLASS TP_BUILTIN_char;
extern TP_CLASS TP_BUILTIN_flt;
extern TP_CLASS TP_BUILTIN_fltd;
extern TP_CLASS TP_BUILTIN_fltdx;
extern TP_CLASS TP_BUILTIN_fltx;
extern TP_CLASS TP_BUILTIN_int;

/* Function declarations */


extern RETURNED_CONST BOOL (**dMODE_814247358)(dMODE, dMODE);

extern RETURNED_CONST BOOL (**dTP_is1811059018)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1999456142)(dTP, dTP);

extern RETURNED_CONST BOOL (**dTP_is242312711)(dTP, OB);

extern RETURNED_CONST BOOL (**dTP_is411624931)(dTP);

extern RETURNED_CONST BOOL (**dTP_is735947028)(dTP);

extern RETURNED_CONST BOOL (**dTP_is_crBOOL)(dTP);

extern RETURNED_CONST FSETSIG (**dGENER354174443)(dGENERATE_AM);

extern RETURNED_CONST IFC (**dTP_ifcrIFC)(dTP);

extern RETURNED_CONST INT (**dTP_kindrINT)(dTP);

extern RETURNED_CONST PROG (**dTP_progrPROG)(dTP);

extern RETURNED_CONST STR (**dMODE_strrSTR)(dMODE);

extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);

extern RETURNED_CONST dAM_EXPR (**dAM_EX1830308833)(dAM_EXPR);

extern RETURNED_CONST dAM_STMT (**dAM_ST1032597839)(dAM_STMT);

extern RETURNED_CONST dAM_STMT (**dAM_ST775224435)(dAM_STMT);

extern RETURNED_CONST dTP (**dAM_EXPR_tprdTP)(dAM_EXPR);

extern RETURNED_CONST void (**dAM_ST1372701974)(dAM_STMT, dAM_STMT);

extern RETURNED_CONST void (**dGENER651759373)(dGENERATE_AM, SIG);
AM_ASSERT_STMT AM_ASS1481858691(AM_ASSERT_STMT);
AM_ASSIGN_STMT AM_ASS782632663(AM_ASSIGN_STMT);
AM_ATTACH_STMT AM_ATT191404837(AM_ATTACH_STMT);
AM_BREAK_STMT AM_BRE1937656818(AM_BREAK_STMT);
AM_CASE_STMT AM_CAS1898951205(AM_CASE_STMT);
AM_COMMENT_STMT AM_COM977130404(AM_COMMENT_STMT);
AM_EXPR_STMT AM_EXP359209435(AM_EXPR_STMT);
ARG ARG_cr1806638063(ARG, dTP);
ARG ARRAYA827580689(ARRAYA827580689_frame);
ARRAYINT SIG_at89227380(SIG);
ARRAYINT SIG_ne271652128(SIG);
BOOL ARRAYB1390856792(ARRAYB1390856792_frame);
BOOL FMAPST800079349(FMAPSTRINT, STR);
BOOL FSETSI722431140(FSETSIG, SIG);
BOOL SE_CON1026918814(SE_CONTEXT);
BOOL SE_CON765348064(SE_CONTEXT);
BOOL SIDE_E1243435218(SIDE_EFFECT, OB);
BOOL SIDE_E526657440(SIDE_EFFECT, SIDE_EFFECT);
BOOL SIG_co165522953(SIG, SIG);
BOOL SIG_co1757762355(SIG, SIG);
BOOL SIG_is1444519892(SIG);
BOOL SIG_is152200802(SIG);
BOOL SIG_is1739708917(SIG);
BOOL SIG_is1893596208(SIG, SIG);
BOOL SIG_is275586960(SIG);
BOOL SIG_is418491101(SIG);
BOOL SIG_is600379186(SIG);
BOOL SIG_is683883765(SIG);
BOOL SIG_is688012902(SIG, OB);
BOOL SIG_is784970610(SIG, dTP);
BOOL SIG_is785829834(SIG, dTP);
BOOL STR_is111530248(STR, STR);
BOOL TP_CLA1568240525(TP_CLASS);
CONFIG_ROUT CONFIG525418741(CONFIG_TBL, STR);
FLISTdAM_STMT FLISTd506668277(FLISTdAM_STMT, dAM_STMT);
FMAPSTRINT FMAPST951578658(FMAPSTRINT, STR, INT);
FMAPSTRSIG FMAPST451382576(FMAPSTRSIG, STR, SIG);
FSETSIDE_EFFECT FSETSI784163459(FSETSIDE_EFFECT, SIDE_EFFECT);
FSETdTP FMAPTP861983338(FMAPTP291739594, TP_CLASS);
FSTR FSTR_c1998740379(FSTR, INT);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR FSTR_p399773021(FSTR, CHAR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
IDENT IDENT_1150413730(IDENT, STR);
IMPL TP_CLA1109727233(TP_CLASS);
INT AS_ARG1759480338(AS_ARG_DEC);
INT FMAPST1755966813(FMAPSTRINT, STR);
INT FMAPST1937251125(FMAPST1937251125_frame);
INT FMAPST868850642(FMAPSTRINT);
INT FSETSI2058428832(FSETSIDE_EFFECT);
INT INT_hashrINT(INT);
INT SIDE_E484651583(SIDE_EFFECT);
INT SIG_hashrINT(SIG);
INT STR_hashrINT(STR);
INT STR_se1725540125(STR, CHAR);
INT STR_se2132153226(STR, STR);
INT STR_sizerINT(STR);
IN_MODE IN_MOD403789248(IN_MODE);
SE_CONTEXT SE_CON1395318461(SE_CONTEXT, PROG, SIG);
SE_CONTEXT SE_CON2084169946(SE_CONTEXT, SIG);
SE_CONTEXT SE_CON32319044(SE_CONTEXT, PROG, SIG);
SE_CONTEXT SIG_ge642497605(SIG, PROG);
SFILE_ID AM_ASS1066541977(AM_ASSIGN_STMT);
SFILE_ID AM_ASS142463220(AM_ASSERT_STMT);
SFILE_ID AM_ATT2128304053(AM_ATTACH_STMT);
SFILE_ID AM_BRE475880308(AM_BREAK_STMT);
SFILE_ID AM_CAS881290947(AM_CASE_STMT);
SFILE_ID AM_COM1046808385(AM_COMMENT_STMT);
SFILE_ID AM_EXP2073687788(AM_EXPR_STMT);
SIDE_EFFECT FSETSI2093978094(FSETSI2093978094_frame);
SIDE_EFFECT FSETSI937439554(FSETSIDE_EFFECT, SIDE_EFFECT);
SIDE_EFFECT SIDE_E524783111(SIDE_EFFECT, OB, BOOL);
SIG FMAPST879183273(FMAPSTRSIG, STR);
SIG IFC_si2085565042(IFC, SIG);
SIG SIG_at1749337727(SIG, AS_ATTR_DEF, IDENT, ARRAYdTP, TP_CONTEXT);
SIG SIG_co2044018076(SIG, AS_CONST_DEF, IDENT, ARRAYdTP, TP_CONTEXT);
SIG SIG_ma1442297600(SIG);
SIG SIG_ro1276623596(SIG, AS_ROUT_DEF, IDENT, ARRAYdTP, TP_CONTEXT);
SIG SIG_sh1677498347(SIG, AS_SHARED_DEF, IDENT, ARRAYdTP, TP_CONTEXT);
SIG SIG_wi1810596376(SIG, dTP);
STR ARG_strrSTR(ARG);
STR ARRAYS1578977701(ARRAYS1578977701_frame);
STR FMAPST961226608(FMAPST961226608_frame);
STR NULL_S1933820575(NULL_STAT);
STR SE_CON1246526640(SE_CONTEXT, STR, SIG);
STR SIDE_E1431483993(SIDE_EFFECT);
STR SIG_strrSTR(SIG);
STR STAT_strrSTR(STAT1);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap2004550586(STR, STR);
STR STR_fr1097270334(STR, FSTR);
STR STR_head_INTrSTR(STR, INT);
STR STR_se405450305(STR_se405450305_frame);
STR STR_tail_INTrSTR(STR, INT);
TP_CLASS ARRAYT1851819274(ARRAYT1851819274_frame);
TP_CLASS TP_TBL1484824239(TP_TBL, STR, INT*);
dAM_STMT AM_ASS1201484234(AM_ASSIGN_STMT);
dAM_STMT AM_ASS1841384941(AM_ASSERT_STMT);
dAM_STMT AM_ATT1156941996(AM_ATTACH_STMT);
dAM_STMT AM_BRE1241758751(AM_BREAK_STMT);
dAM_STMT AM_CAS1510303900(AM_CASE_STMT);
dAM_STMT AM_COM1124366108(AM_COMMENT_STMT);
dAM_STMT AM_EXP1561605445(AM_EXPR_STMT);
dAM_STMT FLISTd1374595573(FLISTd1374595573_frame);
dMODE MODE_c998271775(MODE, AS_ARG_MODE);
dTP FSETdTP_eltbrdTP(FSETdTP_eltbrdTP_frame);
dTP TP_CON1800432689(TP_CONTEXT, AS_TYPE_SPEC);
void AM_ASS1281039639(AM_ASSIGN_STMT, dAM_STMT);
void AM_ASS306541468(AM_ASSERT_STMT, dAM_STMT);
void AM_ASS356960882(AM_ASSERT_STMT, dAM_STMT);
void AM_ASS946442175(AM_ASSIGN_STMT, dAM_STMT);
void AM_ATT1913806391(AM_ATTACH_STMT, dAM_STMT);
void AM_ATT990984413(AM_ATTACH_STMT, dAM_STMT);
void AM_BRE261382646(AM_BREAK_STMT, dAM_STMT);
void AM_BRE905282136(AM_BREAK_STMT, dAM_STMT);
void AM_CAS1095788609(AM_CASE_STMT, dAM_STMT);
void AM_CAS637622509(AM_CASE_STMT, dAM_STMT);
void AM_COM1022674779(AM_COMMENT_STMT, dAM_STMT);
void AM_COM832310723(AM_COMMENT_STMT, dAM_STMT);
void AM_EXP1859190126(AM_EXPR_STMT, dAM_STMT);
void AM_EXP586320964(AM_EXPR_STMT, dAM_STMT);
void ARRAYA409651007(ARRAYA409651007_frame);
void ARRAYB741586158(ARRAYB741586158_frame);
void ARRAYS1161048019(ARRAYS1161048019_frame);
void ARRAYS1450857938(ARRAYSTR, INT, INT);
void FILE_plus_STR(FILE1, STR);
void NULL_S1721445066(NULL_STAT, STR);
void NULL_STAT_print(NULL_STAT);
void PROG_e176405615(PROG, dPROG_ERR);
void PROG_err_STR(PROG, STR);
void SE_CON1013798744(SE_CONTEXT, SIDE_EFFECT);
void SE_CON1088557572(SE_CONTEXT, SE_CONTEXT);
void SE_CON1222696490(SE_CONTEXT, dAM, BOOL);
void SE_CON1786426658(SE_CONTEXT);
void SE_CON184777890(SE_CONTEXT, ARRAYSTR, BOOL);
void SE_CON320528666(SE_CONTEXT);
void SE_CON412941612(SE_CONTEXT, dAM);
void SE_CON435970114(SE_CONTEXT);
void SE_CON621544985(SE_CONTEXT);
void SE_CON773743077(SE_CONTEXT);
void SE_CON897506020(SE_CONTEXT);
void SIG_compute_str(SIG);
void STAT_incr_STR(STAT1, STR);
void STAT_print(STAT1);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

AM_ASSERT_STMT AM_ASS1481858691(AM_ASSERT_STMT self) {
 AM_ASSERT_STMT ret_val;
 AM_ASSERT_STMT r;
 AM_ASSERT_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_ASSERT_STMT ret_val1;
 AM_ASSERT_STMT r1;
 AM_ASSERT_STMT L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 dAM_STMT L5;
 BOOL L6;
 BOOL L71_;
 dAM_EXPR L8;
 if ((self==((AM_ASSERT_STMT) NULL))) {
  ret_val = ((AM_ASSERT_STMT) NULL);
  return ret_val;
 }
 create_self = ((AM_ASSERT_STMT) NULL);
 create_source = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_ASSERT_STMT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_ASSERT_STMT_tag;
 L1 = ((AM_ASSERT_STMT) L2);
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
 SATTR(r,tp,ATTR(self,tp));
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_ASSIGN_STMT AM_ASS782632663(AM_ASSIGN_STMT self) {
 AM_ASSIGN_STMT ret_val;
 AM_ASSIGN_STMT r;
 AM_ASSIGN_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val1;
 AM_ASSIGN_STMT r1;
 AM_ASSIGN_STMT L1;
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
 if ((self==((AM_ASSIGN_STMT) NULL))) {
  ret_val = ((AM_ASSIGN_STMT) NULL);
  return ret_val;
 }
 create_self = ((AM_ASSIGN_STMT) NULL);
 create_source = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_ASSIGN_STMT_tag;
 L1 = ((AM_ASSIGN_STMT) L2);
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
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_ATTACH_STMT AM_ATT191404837(AM_ATTACH_STMT self) {
 AM_ATTACH_STMT ret_val;
 OUT create_self;
 OUT ret_val1;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val2;
 FILE1 r;
 extern STR itisno947483851;
 FILE1 L1;
 OB L2;
 if ((self==((AM_ATTACH_STMT) NULL))) {
  ret_val = ((AM_ATTACH_STMT) NULL);
  return ret_val;
 }
 create_self = ((OUT) NULL);
 ret_val1 = create_self;
 plus_self = ret_val1;
 plus_s = ((STR) &itisno947483851);
 stdout_self = ((FILE1) NULL);
 L2=ZALLOC(sizeof(struct FILE1_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=FILE1_tag;
 L1 = ((FILE1) L2);
 r = L1;
 
 SATTR(r,fp,stdout);
 ret_val2 = r;
 FILE_plus_STR(ret_val2, plus_s);
 ret_val = ((AM_ATTACH_STMT) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_BREAK_STMT AM_BRE1937656818(AM_BREAK_STMT self) {
 AM_BREAK_STMT ret_val;
 AM_BREAK_STMT r;
 AM_BREAK_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_BREAK_STMT ret_val1;
 AM_BREAK_STMT r1;
 AM_BREAK_STMT L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 dAM_STMT L5;
 if ((self==((AM_BREAK_STMT) NULL))) {
  ret_val = ((AM_BREAK_STMT) NULL);
  return ret_val;
 }
 create_self = ((AM_BREAK_STMT) NULL);
 create_source = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_BREAK_STMT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_BREAK_STMT_tag;
 L1 = ((AM_BREAK_STMT) L2);
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

AM_CASE_STMT AM_CAS1898951205(AM_CASE_STMT self) {
 AM_CASE_STMT ret_val;
 AM_CASE_STMT r;
 FLISTdAM_STMT L11;
 dAM_STMT x;
 AM_CASE_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_CASE_STMT ret_val1;
 AM_CASE_STMT r1;
 FLISTdAM_STMT create_self1;
 FLISTdAM_STMT ret_val2;
 AM_CASE_STMT L2;
 OB L3;
 BOOL L4;
 BOOL L51_;
 dAM_STMT L6;
 BOOL L7;
 BOOL L81_;
 dAM_EXPR L9;
 dAM_STMT L10;
 BOOL L12;
 BOOL L131_;
 dAM_STMT L14;
 BOOL L15;
 BOOL L161_;
 dAM_STMT L17;
 if ((self==((AM_CASE_STMT) NULL))) {
  ret_val = ((AM_CASE_STMT) NULL);
  return ret_val;
 }
 create_self = ((AM_CASE_STMT) NULL);
 create_source = ATTR(self,source1);
 L3=ZALLOC(sizeof(struct AM_CASE_STMT_struct));
 if (L3==NULL) FATAL("Unable to allocate more memory");
 ((OB)L3)->header.tag=AM_CASE_STMT_tag;
 L2 = ((AM_CASE_STMT) L3);
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
 L7 = (ATTR(self,test1)==((dAM_EXPR) NULL));
 L81_=!(L7); 
 if (L81_) {
  L9 = ATTR(self,test1);
  SATTR(r,test1,(*dAM_EX1830308833[TAG(L9)])(L9));
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
   SATTR(r,stmts,FLISTd506668277(ATTR(r,stmts), ((dAM_STMT) NULL)));
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

AM_COMMENT_STMT AM_COM977130404(AM_COMMENT_STMT self) {
 AM_COMMENT_STMT ret_val;
 AM_COMMENT_STMT r;
 AM_COMMENT_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_COMMENT_STMT ret_val1;
 AM_COMMENT_STMT r1;
 AM_COMMENT_STMT L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 dAM_STMT L5;
 if ((self==((AM_COMMENT_STMT) NULL))) {
  ret_val = ((AM_COMMENT_STMT) NULL);
  return ret_val;
 }
 create_self = ((AM_COMMENT_STMT) NULL);
 create_source = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_COMMENT_STMT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_COMMENT_STMT_tag;
 L1 = ((AM_COMMENT_STMT) L2);
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
 SATTR(r,comment1,ATTR(self,comment1));
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_EXPR_STMT AM_EXP359209435(AM_EXPR_STMT self) {
 AM_EXPR_STMT ret_val;
 AM_EXPR_STMT r;
 AM_EXPR_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_EXPR_STMT ret_val1;
 AM_EXPR_STMT r1;
 AM_EXPR_STMT L1;
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
 if ((self==((AM_EXPR_STMT) NULL))) {
  ret_val = ((AM_EXPR_STMT) NULL);
  return ret_val;
 }
 create_self = ((AM_EXPR_STMT) NULL);
 create_source = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_EXPR_STMT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_EXPR_STMT_tag;
 L1 = ((AM_EXPR_STMT) L2);
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
 L6 = (ATTR(self,expr)==((dAM_EXPR) NULL));
 L71_=!(L6); 
 if (L71_) {
  L8 = ATTR(self,expr);
  SATTR(r,expr,(*dAM_EX1830308833[TAG(L8)])(L8));
 }
 L9 = (ATTR(self,at)==((dAM_EXPR) NULL));
 L101_=!(L9); 
 if (L101_) {
  L11 = ATTR(self,at);
  SATTR(r,at,(*dAM_EX1830308833[TAG(L11)])(L11));
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

ARG ARG_cr1806638063(ARG self, dTP t) {
 ARG ret_val;
 ARG init_self;
 dTP init_t;
 dMODE init_m;
 BOOL init_same = BOOL_zero;
 ARG ret_val1;
 ARG L1;
 OB L2;
 if (ATTR(self,issame)) {
  L2=ZALLOC(sizeof(struct ARG_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=ARG_tag;
  L1 = ((ARG) L2);
  init_self = L1;
  init_t = t;
  init_m = ATTR(self,mode1);
  init_same = TRUE;
  SATTR(init_self,tp,init_t);
  SATTR(init_self,mode1,init_m);
  SATTR(init_self,issame,init_same);
  ret_val1 = init_self;
  ret_val = ret_val1;
  return ret_val;
 }
 else {
  ret_val = self;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

ARRAYINT SIG_at89227380(SIG self) {
 ARRAYINT ret_val;
 if (SIG_is418491101(self)) {
  ret_val = ATTR(ATTR(self,builtin_info),attr_access);
  return ret_val;
 }
 if ((ATTR(self,opt_info)==((OPT_LOCAL_CALL) NULL))) {
  ret_val = ((ARRAYINT) NULL);
  return ret_val;
 }
 ret_val = ATTR(ATTR(self,opt_info),attr_access);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

ARRAYINT SIG_ne271652128(SIG self) {
 ARRAYINT ret_val;
 if (SIG_is418491101(self)) {
  ret_val = ATTR(ATTR(self,builtin_info),near_depends_on);
  return ret_val;
 }
 if ((ATTR(self,opt_info)==((OPT_LOCAL_CALL) NULL))) {
  ret_val = ((ARRAYINT) NULL);
  return ret_val;
 }
 ret_val = ATTR(ATTR(self,opt_info),near_depends_on_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL SE_CON1026918814(SE_CONTEXT self) {
 BOOL ret_val = BOOL_zero;
 BOOL L1;
 BOOL L2;
 BOOL L31_;
 if ((self==((SE_CONTEXT) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 L2 = ATTR(SE_CONTEXT_prog,all_reached);
 L31_=!(L2); 
 if (L31_) {
  L1 = ATTR(SE_CON1373267262,side_effects);
 } else {
  L1 = FALSE;
 }
 ret_val = L1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL SE_CON765348064(SE_CONTEXT self) {
 BOOL ret_val = BOOL_zero;
 if ((self==((SE_CONTEXT) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 ret_val = ATTR(SE_CON1373267262,side_debug);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL SIDE_E1243435218(SIDE_EFFECT self, OB arg) {
 BOOL ret_val = BOOL_zero;
 if (arg==NULL) {
  goto other172;
 } else
 switch (TAG(arg)) {
  case SIDE_EFFECT_tag:
   ret_val = SIDE_E526657440(self, ((SIDE_EFFECT) arg));
   return ret_val; break;
  default: ;
  other172: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL SIDE_E526657440(SIDE_EFFECT self, SIDE_EFFECT other1) {
 BOOL ret_val = BOOL_zero;
 SIDE_EFFECT is_array_self;
 BOOL ret_val1 = BOOL_zero;
 SIDE_EFFECT is_array_self1;
 BOOL ret_val2 = BOOL_zero;
 SIDE_EFFECT is_local_self;
 BOOL ret_val3 = BOOL_zero;
 SIDE_EFFECT is_local_self1;
 BOOL ret_val4 = BOOL_zero;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val5 = BOOL_zero;
 IDENT is_eq_self1 = IDENT_zero;
 IDENT is_eq_i1 = IDENT_zero;
 BOOL ret_val6 = BOOL_zero;
 BOOL L1;
 BOOL L2;
 dTP L3;
 BOOL L4;
 BOOL L5;
 dTP L6;
 if ((self==((SIDE_EFFECT) NULL))) {
  L1 = TRUE;
 } else {
  L1 = (other1==((SIDE_EFFECT) NULL));
 }
 if (L1) {
  ret_val = FALSE;
  return ret_val;
 }
 is_array_self = self;
 ret_val1 = (ATTR(is_array_self,name1).str==(STR)0);
 if (ret_val1) {
  is_array_self1 = other1;
  ret_val2 = (ATTR(is_array_self1,name1).str==(STR)0);
  if (ret_val2) {
   L3 = ATTR(self,tp);
   L2 = (*dTP_is242312711[TAG(L3)])(L3, ((OB) ATTR(other1,tp)));
  } else {
   L2 = FALSE;
  }
  ret_val = L2;
  return ret_val;
 }
 else {
  is_local_self = self;
  ret_val3 = (ATTR(is_local_self,tp)==((dTP) NULL));
  if (ret_val3) {
   is_local_self1 = other1;
   ret_val4 = (ATTR(is_local_self1,tp)==((dTP) NULL));
   if (ret_val4) {
    is_eq_self = ATTR(self,name1);
    is_eq_i = ATTR(other1,name1);
    ret_val5 = (is_eq_self.str==is_eq_i.str);
    L4 = ret_val5;
   } else {
    L4 = FALSE;
   }
   ret_val = L4;
   return ret_val;
  }
  else {
   L6 = ATTR(self,tp);
   if ((*dTP_is242312711[TAG(L6)])(L6, ((OB) ATTR(other1,tp)))) {
    is_eq_self1 = ATTR(self,name1);
    is_eq_i1 = ATTR(other1,name1);
    ret_val6 = (is_eq_self1.str==is_eq_i1.str);
    L5 = ret_val6;
   } else {
    L5 = FALSE;
   }
   ret_val = L5;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL SIG_co165522953(SIG self, SIG s) {
 BOOL ret_val = BOOL_zero;
 ARG arg = ((ARG) NULL);
 ARG s_arg = ((ARG) NULL);
 dTP t = ((dTP) NULL);
 dTP st = ((dTP) NULL);
 BOOL conflict = BOOL_zero;
 BOOL abs1 = BOOL_zero;
 BOOL same_args = BOOL_zero;
 ARRAYARG L11;
 ARRAYARG L21;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 SIG has_ret_self;
 BOOL ret_val2 = BOOL_zero;
 SIG has_ret_self1;
 BOOL ret_val3 = BOOL_zero;
 INT L31 = INT_zero;
 BOOL L4;
 BOOL L5;
 BOOL L6;
 BOOL L71_;
 ARRAYARG L8;
 INT L91_;
 INT L10;
 ARRAYARG L12;
 INT L131_;
 INT L14;
 BOOL L151_;
 BOOL L16;
 BOOL L171_;
 BOOL L18;
 BOOL L191_;
 BOOL L20;
 BOOL L22;
 BOOL L231_;
 BOOL L24;
 BOOL L251_;
 BOOL L26;
 BOOL L271_;
 INT L281_br;
 ARG aL281_;
 INT L291_br;
 ARG aL291_;
 ARG L30;
 ARG L32;
 dTP L33;
 BOOL L34;
 BOOL L351_;
 BOOL L36;
 BOOL L37;
 BOOL L38;
 BOOL L39;
 dTP L40;
 BOOL L41;
 BOOL L421_;
 dTP L43;
 BOOL L44;
 BOOL L451_;
 dTP L46;
 BOOL L47;
 BOOL L481_;
 dTP L49;
 BOOL L50;
 BOOL L511_;
 dTP L52;
 BOOL L53;
 BOOL L541_;
 BOOL L55;
 BOOL L56;
 dTP L57;
 dTP L58;
 BOOL L59;
 dTP L60;
 dTP L61;
 BOOL L62;
 dTP L63;
 dTP L64;
 BOOL L65;
 BOOL L661_;
 dTP L67;
 BOOL L68;
 BOOL L691_;
 INT L701_;
 BOOL L72;
 BOOL L73;
 BOOL L74;
 BOOL L751_;
 is_eq_self = ATTR(self,name1);
 is_eq_i = ATTR(s,name1);
 ret_val1 = (is_eq_self.str==is_eq_i.str);
 L6 = ret_val1;
 L71_=!(L6); 
 if (L71_) {
  L5 = TRUE;
 } else {
  L8 = ATTR(self,args);
  L91_=(L8)==NULL?0:ASIZE((ARRAYARG)L8); 
  L10 = L91_;
  L12 = ATTR(s,args);
  L131_=(L12)==NULL?0:ASIZE((ARRAYARG)L12); 
  L14 = L131_;
  L151_=(L10)==(L14); 
  L16 = L151_;
  L171_=!(L16); 
  L5 = L171_;
 }
 if (L5) {
  L4 = TRUE;
 } else {
  has_ret_self = self;
  L18 = (ATTR(has_ret_self,ret)==((dTP) NULL));
  L191_=!(L18); 
  ret_val2 = L191_;
  L20 = ret_val2;
  has_ret_self1 = s;
  L22 = (ATTR(has_ret_self1,ret)==((dTP) NULL));
  L231_=!(L22); 
  ret_val3 = L231_;
  L24 = ret_val3;
  L251_=L20==L24; 
  L26 = L251_;
  L271_=!(L26); 
  L4 = L271_;
 }
 if (L4) {
  ret_val = FALSE;
  return ret_val;
 }
 conflict = FALSE;
 abs1 = FALSE;
 same_args = TRUE;
 {
  BOOL f_L281_ = TRUE;
  BOOL f_L291_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = ATTR(self,args);
  L21 = ATTRs(s,args,((ARRAYARG) NULL));
  L281_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
  L291_br=L21==NULL?0:ASIZE((ARRAYARG)L21); 
  while (1) {
   if(L31>=L281_br)  goto after_loop; 
   aL281_=ARR((ARRAYARG)L11,L31); 
   arg = aL281_;
   if(L31>=L291_br)  goto after_loop; 
   aL291_=ARR((ARRAYARG)L21,L31); 
   s_arg = aL291_;
   t = ATTR(arg,tp);
   st = ATTR(s_arg,tp);
   L33 = t;
   L34 = (*dTP_is242312711[TAG(L33)])(L33, ((OB) st));
   L351_=!(L34); 
   if (L351_) {
    same_args = FALSE;
   }
   L40 = t;
   L41 = (*dTP_is242312711[TAG(L40)])(L40, ((OB) st));
   L421_=!(L41); 
   if (L421_) {
    L43 = t;
    L44 = (*dTP_is1811059018[TAG(L43)])(L43);
    L451_=!(L44); 
    L39 = L451_;
   } else {
    L39 = FALSE;
   }
   if (L39) {
    L46 = t;
    L47 = (*dTP_is735947028[TAG(L46)])(L46);
    L481_=!(L47); 
    L38 = L481_;
   } else {
    L38 = FALSE;
   }
   if (L38) {
    L49 = st;
    L50 = (*dTP_is1811059018[TAG(L49)])(L49);
    L511_=!(L50); 
    L37 = L511_;
   } else {
    L37 = FALSE;
   }
   if (L37) {
    L52 = st;
    L53 = (*dTP_is735947028[TAG(L52)])(L52);
    L541_=!(L53); 
    L36 = L541_;
   } else {
    L36 = FALSE;
   }
   if (L36) {
    ret_val = FALSE;
    return ret_val;
   }
   else {
    L57 = t;
    if ((*dTP_is1811059018[TAG(L57)])(L57)) {
     L58 = st;
     L56 = (*dTP_is1811059018[TAG(L58)])(L58);
    } else {
     L56 = FALSE;
    }
    if (L56) {
     L55 = TRUE;
    } else {
     L60 = t;
     if ((*dTP_is735947028[TAG(L60)])(L60)) {
      L61 = st;
      L59 = (*dTP_is735947028[TAG(L61)])(L61);
     } else {
      L59 = FALSE;
     }
     L55 = L59;
    }
    if (L55) {
     abs1 = TRUE;
     L63 = t;
     if ((*dTP_is1999456142[TAG(L63)])(L63, st)) {
      L62 = TRUE;
     } else {
      L64 = st;
      L62 = (*dTP_is1999456142[TAG(L64)])(L64, t);
     }
     L65 = L62;
     L661_=!(L65); 
     if (L661_) {
      conflict = TRUE;
     }
    }
    else {
     L67 = t;
     L68 = (*dTP_is242312711[TAG(L67)])(L67, ((OB) st));
     L691_=!(L68); 
     if (L691_) {
      ret_val = FALSE;
      return ret_val;
     }
    }
   }
   L701_=INTPLUS(L31,1); 
   L31 = L701_;
  }
 }
 after_loop: ;
 if (abs1) {
  L74 = conflict;
 } else {
  L74 = FALSE;
 }
 if (L74) {
  L73 = TRUE;
 } else {
  L751_=!(abs1); 
  L73 = L751_;
 }
 if (L73) {
  L72 = TRUE;
 } else {
  L72 = same_args;
 }
 ret_val = L72;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL SIG_co1757762355(SIG self, SIG s) {
 BOOL ret_val = BOOL_zero;
 ARRAYBOOL L11;
 ARRAYBOOL L21;
 ARRAYARG L31;
 ARRAYARG L41;
 ARRAYARG L51;
 ARG self_arg;
 ARRAYARG L61;
 ARG s_arg;
 dMODE self_arg_mode;
 dMODE s_arg_mode;
 dMODE L71;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 SIG has_ret_self;
 BOOL ret_val2 = BOOL_zero;
 SIG has_ret_self1;
 BOOL ret_val3 = BOOL_zero;
 INT L81 = INT_zero;
 INT L91 = INT_zero;
 SIG has_ret_self2;
 BOOL ret_val4 = BOOL_zero;
 INT L101 = INT_zero;
 BOOL L12;
 BOOL L13;
 BOOL L14;
 BOOL L15;
 BOOL L161_;
 ARRAYARG L17;
 INT L181_;
 INT L19;
 ARRAYARG L20;
 INT L221_;
 INT L23;
 BOOL L241_;
 BOOL L25;
 BOOL L261_;
 ARRAYBOOL L27;
 INT L281_;
 INT L29;
 ARRAYBOOL L30;
 INT L321_;
 INT L33;
 BOOL L341_;
 BOOL L35;
 BOOL L361_;
 BOOL L37;
 BOOL L381_;
 BOOL L39;
 BOOL L40;
 BOOL L421_;
 BOOL L43;
 BOOL L441_;
 BOOL L45;
 BOOL L461_;
 INT L471_br;
 BOOL aL471_;
 INT L481_br;
 BOOL aL481_;
 BOOL L49;
 BOOL L50;
 BOOL L52;
 BOOL L53;
 BOOL L541_;
 BOOL L55;
 BOOL L561_;
 INT L571_;
 INT L581_br;
 ARG aL581_;
 INT L591_br;
 ARG aL591_;
 ARG L60;
 ARG L62;
 ARG L63;
 ARG L64;
 dMODE L65;
 BOOL L66;
 BOOL L671_;
 INT L681_;
 BOOL L69;
 BOOL L701_;
 BOOL L72;
 BOOL L73;
 BOOL L741_;
 dTP L75;
 BOOL L76;
 BOOL L771_;
 INT L781_br;
 ARG aL781_;
 INT L791_br;
 ARG aL791_;
 ARG L80;
 ARG L82;
 dMODE L83;
 dTP L84;
 BOOL L85;
 BOOL L861_;
 dMODE L87;
 BOOL L88;
 BOOL L89;
 BOOL L901_;
 dTP L92;
 BOOL L93;
 BOOL L941_;
 dMODE L95;
 dTP L96;
 BOOL L97;
 BOOL L981_;
 dMODE L99;
 dTP L100;
 BOOL L102;
 BOOL L1031_;
 INT L1041_;
 is_eq_self = ATTR(self,name1);
 is_eq_i = ATTR(s,name1);
 ret_val1 = (is_eq_self.str==is_eq_i.str);
 L15 = ret_val1;
 L161_=!(L15); 
 if (L161_) {
  L14 = TRUE;
 } else {
  L17 = ATTR(self,args);
  L181_=(L17)==NULL?0:ASIZE((ARRAYARG)L17); 
  L19 = L181_;
  L20 = ATTR(s,args);
  L221_=(L20)==NULL?0:ASIZE((ARRAYARG)L20); 
  L23 = L221_;
  L241_=(L19)==(L23); 
  L25 = L241_;
  L261_=!(L25); 
  L14 = L261_;
 }
 if (L14) {
  L13 = TRUE;
 } else {
  L27 = ATTR(self,hot);
  L281_=(L27)==NULL?0:ASIZE((ARRAYBOOL)L27); 
  L29 = L281_;
  L30 = ATTR(s,hot);
  L321_=(L30)==NULL?0:ASIZE((ARRAYBOOL)L30); 
  L33 = L321_;
  L341_=(L29)==(L33); 
  L35 = L341_;
  L361_=!(L35); 
  L13 = L361_;
 }
 if (L13) {
  L12 = TRUE;
 } else {
  has_ret_self = self;
  L37 = (ATTR(has_ret_self,ret)==((dTP) NULL));
  L381_=!(L37); 
  ret_val2 = L381_;
  L39 = ret_val2;
  has_ret_self1 = s;
  L40 = (ATTR(has_ret_self1,ret)==((dTP) NULL));
  L421_=!(L40); 
  ret_val3 = L421_;
  L43 = ret_val3;
  L441_=L39==L43; 
  L45 = L441_;
  L461_=!(L45); 
  L12 = L461_;
 }
 if (L12) {
  ret_val = FALSE;
  return ret_val;
 }
 {
  BOOL f_L471_ = TRUE;
  BOOL f_L481_ = TRUE;
  /* loop index variable */
  L81 = 0;
  L11 = ATTR(self,hot);
  L21 = ATTRs(s,hot,((ARRAYBOOL) NULL));
  L471_br=L11==NULL?0:ASIZE((ARRAYBOOL)L11); 
  L481_br=L21==NULL?0:ASIZE((ARRAYBOOL)L21); 
  while (1) {
   if(L81>=L471_br)  goto after_loop; 
   aL471_=ARR((ARRAYBOOL)L11,L81); 
   L50 = aL471_;
   if(L81>=L481_br)  goto after_loop; 
   aL481_=ARR((ARRAYBOOL)L21,L81); 
   L53 = aL481_;
   L541_=L50==L53; 
   L55 = L541_;
   L561_=!(L55); 
   if (L561_) {
    ret_val = FALSE;
    return ret_val;
   }
   L571_=INTPLUS(L81,1); 
   L81 = L571_;
  }
 }
 after_loop: ;
 {
  BOOL f_L581_ = TRUE;
  BOOL f_L591_ = TRUE;
  /* loop index variable */
  L91 = 0;
  L31 = ATTR(self,args);
  L41 = ATTRs(s,args,((ARRAYARG) NULL));
  L581_br=L31==NULL?0:ASIZE((ARRAYARG)L31); 
  L591_br=L41==NULL?0:ASIZE((ARRAYARG)L41); 
  while (1) {
   if(L91>=L581_br)  goto after_loop1; 
   aL581_=ARR((ARRAYARG)L31,L91); 
   L62=aL581_;
   if(L91>=L591_br)  goto after_loop1; 
   aL591_=ARR((ARRAYARG)L41,L91); 
   L64=aL591_;
   L65 = ATTR(L62,mode1);
   L66 = (*dMODE_814247358[TAG(L65)])(L65, ATTR(L64,mode1));
   L671_=!(L66); 
   if (L671_) {
    ret_val = FALSE;
    return ret_val;
   }
   L681_=INTPLUS(L91,1); 
   L91 = L681_;
  }
 }
 after_loop1: ;
 has_ret_self2 = self;
 L69 = (ATTR(has_ret_self2,ret)==((dTP) NULL));
 L701_=!(L69); 
 ret_val4 = L701_;
 if (ret_val4) {
  if (ATTR(s,returns_same)) {
   L73 = ATTR(self,returns_same);
   L741_=!(L73); 
   L72 = L741_;
  } else {
   L72 = FALSE;
  }
  if (L72) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   L75 = ATTR(self,ret);
   L76 = (*dTP_is1999456142[TAG(L75)])(L75, ATTR(s,ret));
   L771_=!(L76); 
   if (L771_) {
    ret_val = FALSE;
    return ret_val;
   }
  }
 }
 {
  BOOL f_L781_ = TRUE;
  BOOL f_L791_ = TRUE;
  /* loop index variable */
  L101 = 0;
  L51 = ATTR(self,args);
  L61 = ATTRs(s,args,((ARRAYARG) NULL));
  L781_br=L51==NULL?0:ASIZE((ARRAYARG)L51); 
  L791_br=L61==NULL?0:ASIZE((ARRAYARG)L61); 
  while (1) {
   if(L101>=L781_br)  goto after_loop2; 
   aL781_=ARR((ARRAYARG)L51,L101); 
   self_arg = aL781_;
   if(L101>=L791_br)  goto after_loop2; 
   aL791_=ARR((ARRAYARG)L61,L101); 
   s_arg = aL791_;
   self_arg_mode = ATTR(self_arg,mode1);
   s_arg_mode = ATTR(s_arg,mode1);
   L71 = self_arg_mode;
   L83 = L71;
   if ((*dMODE_814247358[TAG(L83)])(L83, ((dMODE) MODES_in_mode))) {
    L84 = ATTR(s_arg,tp);
    L85 = (*dTP_is1999456142[TAG(L84)])(L84, ATTR(self_arg,tp));
    L861_=!(L85); 
    if (L861_) {
     ret_val = FALSE;
     return ret_val;
    }
   }
   else {
    L87 = L71;
    if ((*dMODE_814247358[TAG(L87)])(L87, ((dMODE) MODES_out_mode))) {
     if (ATTR(s_arg,issame)) {
      L89 = ATTR(self_arg,issame);
      L901_=!(L89); 
      L88 = L901_;
     } else {
      L88 = FALSE;
     }
     if (L88) {
      ret_val = FALSE;
      return ret_val;
     }
     L92 = ATTR(self_arg,tp);
     L93 = (*dTP_is1999456142[TAG(L92)])(L92, ATTR(s_arg,tp));
     L941_=!(L93); 
     if (L941_) {
      ret_val = FALSE;
      return ret_val;
     }
    }
    else {
     L95 = L71;
     if ((*dMODE_814247358[TAG(L95)])(L95, ((dMODE) MODES_inout_mode))) {
      L96 = ATTR(self_arg,tp);
      L97 = (*dTP_is242312711[TAG(L96)])(L96, ((OB) ATTR(s_arg,tp)));
      L981_=!(L97); 
      if (L981_) {
       ret_val = FALSE;
       return ret_val;
      }
     }
     else {
      L99 = L71;
      if ((*dMODE_814247358[TAG(L99)])(L99, ((dMODE) MODES_once_mode))) {
       L100 = ATTR(s_arg,tp);
       L102 = (*dTP_is1999456142[TAG(L100)])(L100, ATTR(self_arg,tp));
       L1031_=!(L102); 
       if (L1031_) {
        ret_val = FALSE;
        return ret_val;
       }
      }
      else {
       FATAL("No applicable target in case statement\nin SIG::conforms_to(SIG):BOOL\n./Representation/sig.sa:320:16");
      }
     }
    }
   }
   L1041_=INTPLUS(L101,1); 
   L101 = L1041_;
  }
 }
 after_loop2: ;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL SIG_is1444519892(SIG self) {
 BOOL ret_val = BOOL_zero;
 ARRAYARG L11;
 dTP a;
 IDENT is_iter_self = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 SIG prog_self;
 PROG ret_val2;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val4;
 INT L21 = INT_zero;
 SIG prog_self1;
 PROG ret_val5;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val6;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val7;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val8;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val9;
 SIG prog_self2;
 PROG ret_val10;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val11;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val12;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val13;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val14;
 BOOL L3;
 BOOL L4;
 BOOL L51_;
 STR L6;
 INT L7;
 INT L81_;
 INT L9;
 CHAR L101_;
 CHAR L12;
 BOOL L131_;
 dTP L14;
 extern STR Theitersignature;
 extern STR isntal884974273;
 INT L151_br;
 ARG aL151_;
 ARG L16;
 ARG L17;
 BOOL L18;
 dTP L19;
 dTP L20;
 extern STR Thesignature;
 extern STR isnotl310318433;
 dTP L22;
 extern STR isnoto1261563554;
 INT L231_;
 BOOL L24;
 BOOL L25;
 BOOL L26;
 BOOL L271_;
 BOOL L28;
 BOOL L291_;
 dTP L30;
 BOOL L31;
 BOOL L321_;
 dTP L33;
 extern STR Thesignature;
 extern STR isnotl1628682231;
 dTP L34;
 extern STR isnoto1261563554;
 is_iter_self = ATTR(self,name1);
 L4 = (is_iter_self.str==((STR) NULL));
 L51_=!(L4); 
 if (L51_) {
  L6 = is_iter_self.str;
  L7 = STR_sizerINT(is_iter_self.str);
  L81_=INTMINUS(L7,1); 
  L9 = L81_;
  L101_=ARR((STR)L6,L9); 
  L12 = L101_;
  L131_=L12=='!'; 
  L3 = L131_;
 } else {
  L3 = FALSE;
 }
 ret_val1 = L3;
 if (ret_val1) {
  prog_self = self;
  L14 = ATTR(prog_self,tp);
  ret_val2 = (*dTP_progrPROG[TAG(L14)])(L14);
  plus_self = ((STR) &Theitersignature);
  plus_sarg = ATTR(self,str);
  ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val3;
  plus_sarg1 = ((STR) &isntal884974273);
  ret_val4 = STR_ap2004550586(plus_self1, plus_sarg1);
  PROG_err_STR(ret_val2, ret_val4);
  ret_val = FALSE;
  return ret_val;
 }
 {
  BOOL f_L151_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = ATTR(self,args);
  L151_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
  while (1) {
   if(L21>=L151_br)  goto after_loop; 
   aL151_=ARR((ARRAYARG)L11,L21); 
   L17=aL151_;
   a = ATTR(L17,tp);
   if (SIG_is785829834(self, a)) {
    L18 = TRUE;
   } else {
    L19 = a;
    L18 = (*dTP_is_crBOOL[TAG(L19)])(L19);
   }
   if (L18) {
   }
   else {
    prog_self1 = self;
    L20 = ATTR(prog_self1,tp);
    ret_val5 = (*dTP_progrPROG[TAG(L20)])(L20);
    plus_self2 = ((STR) &Thesignature);
    plus_sarg2 = ATTR(self,str);
    ret_val6 = STR_ap2004550586(plus_self2, plus_sarg2);
    plus_self3 = ret_val6;
    plus_sarg3 = ((STR) &isnotl310318433);
    ret_val7 = STR_ap2004550586(plus_self3, plus_sarg3);
    plus_self4 = ret_val7;
    L22 = a;
    plus_sarg4 = (*dTP_strrSTR[TAG(L22)])(L22);
    ret_val8 = STR_ap2004550586(plus_self4, plus_sarg4);
    plus_self5 = ret_val8;
    plus_sarg5 = ((STR) &isnoto1261563554);
    ret_val9 = STR_ap2004550586(plus_self5, plus_sarg5);
    PROG_err_STR(ret_val5, ret_val9);
    ret_val = FALSE;
    return ret_val;
   }
   L231_=INTPLUS(L21,1); 
   L21 = L231_;
  }
 }
 after_loop: ;
 L26 = (ATTR(self,ret)==((dTP) NULL));
 L271_=!(L26); 
 if (L271_) {
  L28 = SIG_is785829834(self, ATTR(self,ret));
  L291_=!(L28); 
  L25 = L291_;
 } else {
  L25 = FALSE;
 }
 if (L25) {
  L30 = ATTR(self,ret);
  L31 = (*dTP_is_crBOOL[TAG(L30)])(L30);
  L321_=!(L31); 
  L24 = L321_;
 } else {
  L24 = FALSE;
 }
 if (L24) {
  prog_self2 = self;
  L33 = ATTR(prog_self2,tp);
  ret_val10 = (*dTP_progrPROG[TAG(L33)])(L33);
  plus_self6 = ((STR) &Thesignature);
  plus_sarg6 = ATTR(self,str);
  ret_val11 = STR_ap2004550586(plus_self6, plus_sarg6);
  plus_self7 = ret_val11;
  plus_sarg7 = ((STR) &isnotl1628682231);
  ret_val12 = STR_ap2004550586(plus_self7, plus_sarg7);
  plus_self8 = ret_val12;
  L34 = ATTR(self,ret);
  plus_sarg8 = (*dTP_strrSTR[TAG(L34)])(L34);
  ret_val13 = STR_ap2004550586(plus_self8, plus_sarg8);
  plus_self9 = ret_val13;
  plus_sarg9 = ((STR) &isnoto1261563554);
  ret_val14 = STR_ap2004550586(plus_self9, plus_sarg9);
  PROG_err_STR(ret_val10, ret_val14);
  ret_val = FALSE;
  return ret_val;
 }
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL SIG_is152200802(SIG self) {
 BOOL ret_val = BOOL_zero;
 BOOL L1;
 ARRAYARG L2;
 INT L31_;
 INT L4;
 BOOL L51_;
 ARRAYARG L6;
 ARG L71_;
 ARG L8;
 dMODE L9;
 L2 = ATTR(self,args);
 L31_=(L2)==NULL?0:ASIZE((ARRAYARG)L2); 
 L4 = L31_;
 L51_=(L4)==(1); 
 if (L51_) {
  L1 = (ATTR(self,ret)==((dTP) NULL));
 } else {
  L1 = FALSE;
 }
 if (L1) {
  L6 = ATTR(self,args);
  L71_=(ARG)ARR((ARRAYARG)L6,0); 
  L8=L71_;
  L9 = ATTR(L8,mode1);
  ret_val = (*dMODE_814247358[TAG(L9)])(L9, ((dMODE) MODES_in_mode));
  return ret_val;
 }
 else {
  ret_val = FALSE;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL SIG_is1739708917(SIG self) {
 BOOL ret_val = BOOL_zero;
 dTP L1;
 INT L2;
 INT L3;
 BOOL L41_;
 BOOL L5;
 ARRAYARG L6;
 INT L71_;
 INT L8;
 BOOL L91_;
 dTP L10;
 ARRAYARG L11;
 ARG L121_;
 ARG L13;
 dMODE L14;
 BOOL L15;
 ARRAYARG L16;
 INT L171_;
 INT L18;
 BOOL L191_;
 ARRAYARG L20;
 ARG L211_;
 ARG L22;
 dMODE L23;
 L1 = ATTR(self,tp);
 L2 = (*dTP_kindrINT[TAG(L1)])(L1);
 L3 = TP_KIND_val_tp;
 L41_=(L2)==(L3); 
 if (L41_) {
  L6 = ATTR(self,args);
  L71_=(L6)==NULL?0:ASIZE((ARRAYARG)L6); 
  L8 = L71_;
  L91_=(L8)==(1); 
  if (L91_) {
   L10 = ATTR(self,tp);
   L5 = (*dTP_is242312711[TAG(L10)])(L10, ((OB) ATTR(self,ret)));
  } else {
   L5 = FALSE;
  }
  if (L5) {
   L11 = ATTR(self,args);
   L121_=(ARG)ARR((ARRAYARG)L11,0); 
   L13=L121_;
   L14 = ATTR(L13,mode1);
   ret_val = (*dMODE_814247358[TAG(L14)])(L14, ((dMODE) MODES_in_mode));
   return ret_val;
  }
 }
 else {
  L16 = ATTR(self,args);
  L171_=(L16)==NULL?0:ASIZE((ARRAYARG)L16); 
  L18 = L171_;
  L191_=(L18)==(1); 
  if (L191_) {
   L15 = (ATTR(self,ret)==((dTP) NULL));
  } else {
   L15 = FALSE;
  }
  if (L15) {
   L20 = ATTR(self,args);
   L211_=(ARG)ARR((ARRAYARG)L20,0); 
   L22=L211_;
   L23 = ATTR(L22,mode1);
   ret_val = (*dMODE_814247358[TAG(L23)])(L23, ((dMODE) MODES_in_mode));
   return ret_val;
  }
 }
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL SIG_is1893596208(SIG self, SIG s) {
 BOOL ret_val = BOOL_zero;
 ARRAYARG L11;
 ARG arg;
 ARRAYARG L21;
 ARG s_arg;
 ARRAYBOOL L31;
 ARRAYBOOL L41;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 SIG has_ret_self;
 BOOL ret_val2 = BOOL_zero;
 SIG has_ret_self1;
 BOOL ret_val3 = BOOL_zero;
 INT L51 = INT_zero;
 INT L61 = INT_zero;
 SIG has_ret_self2;
 BOOL ret_val4 = BOOL_zero;
 BOOL L7;
 BOOL L8;
 BOOL L9;
 BOOL L10;
 BOOL L121_;
 ARRAYARG L13;
 INT L141_;
 INT L15;
 ARRAYARG L16;
 INT L171_;
 INT L18;
 BOOL L191_;
 BOOL L20;
 BOOL L221_;
 ARRAYBOOL L23;
 INT L241_;
 INT L25;
 ARRAYBOOL L26;
 INT L271_;
 INT L28;
 BOOL L291_;
 BOOL L30;
 BOOL L321_;
 BOOL L33;
 BOOL L341_;
 BOOL L35;
 BOOL L36;
 BOOL L371_;
 BOOL L38;
 BOOL L391_;
 BOOL L40;
 BOOL L421_;
 INT L431_br;
 ARG aL431_;
 INT L441_br;
 ARG aL441_;
 ARG L45;
 ARG L46;
 BOOL L47;
 dTP L48;
 BOOL L49;
 BOOL L501_;
 dMODE L52;
 BOOL L53;
 BOOL L541_;
 INT L551_;
 INT L561_br;
 BOOL aL561_;
 INT L571_br;
 BOOL aL571_;
 BOOL L58;
 BOOL L59;
 BOOL L60;
 BOOL L62;
 BOOL L631_;
 BOOL L64;
 BOOL L651_;
 INT L661_;
 BOOL L67;
 BOOL L68;
 BOOL L69;
 BOOL L701_;
 dTP L71;
 BOOL L72;
 BOOL L731_;
 BOOL L74;
 BOOL L75;
 BOOL L761_;
 BOOL L77;
 BOOL L781_;
 if ((self==((SIG) NULL))) {
  ret_val = (s==((SIG) NULL));
  return ret_val;
 }
 else {
  if ((s==((SIG) NULL))) {
   ret_val = FALSE;
   return ret_val;
  }
 }
 if ((self==s)) {
  ret_val = TRUE;
  return ret_val;
 }
 is_eq_self = ATTR(self,name1);
 is_eq_i = ATTR(s,name1);
 ret_val1 = (is_eq_self.str==is_eq_i.str);
 L10 = ret_val1;
 L121_=!(L10); 
 if (L121_) {
  L9 = TRUE;
 } else {
  L13 = ATTR(self,args);
  L141_=(L13)==NULL?0:ASIZE((ARRAYARG)L13); 
  L15 = L141_;
  L16 = ATTR(s,args);
  L171_=(L16)==NULL?0:ASIZE((ARRAYARG)L16); 
  L18 = L171_;
  L191_=(L15)==(L18); 
  L20 = L191_;
  L221_=!(L20); 
  L9 = L221_;
 }
 if (L9) {
  L8 = TRUE;
 } else {
  L23 = ATTR(self,hot);
  L241_=(L23)==NULL?0:ASIZE((ARRAYBOOL)L23); 
  L25 = L241_;
  L26 = ATTR(s,hot);
  L271_=(L26)==NULL?0:ASIZE((ARRAYBOOL)L26); 
  L28 = L271_;
  L291_=(L25)==(L28); 
  L30 = L291_;
  L321_=!(L30); 
  L8 = L321_;
 }
 if (L8) {
  L7 = TRUE;
 } else {
  has_ret_self = self;
  L33 = (ATTR(has_ret_self,ret)==((dTP) NULL));
  L341_=!(L33); 
  ret_val2 = L341_;
  L35 = ret_val2;
  has_ret_self1 = s;
  L36 = (ATTR(has_ret_self1,ret)==((dTP) NULL));
  L371_=!(L36); 
  ret_val3 = L371_;
  L38 = ret_val3;
  L391_=L35==L38; 
  L40 = L391_;
  L421_=!(L40); 
  L7 = L421_;
 }
 if (L7) {
  ret_val = FALSE;
  return ret_val;
 }
 {
  BOOL f_L431_ = TRUE;
  BOOL f_L441_ = TRUE;
  /* loop index variable */
  L51 = 0;
  L11 = ATTR(self,args);
  L21 = ATTRs(s,args,((ARRAYARG) NULL));
  L431_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
  L441_br=L21==NULL?0:ASIZE((ARRAYARG)L21); 
  while (1) {
   if(L51>=L431_br)  goto after_loop; 
   aL431_=ARR((ARRAYARG)L11,L51); 
   arg = aL431_;
   if(L51>=L441_br)  goto after_loop; 
   aL441_=ARR((ARRAYARG)L21,L51); 
   s_arg = aL441_;
   L48 = ATTR(arg,tp);
   L49 = (*dTP_is242312711[TAG(L48)])(L48, ((OB) ATTR(s_arg,tp)));
   L501_=!(L49); 
   if (L501_) {
    L47 = TRUE;
   } else {
    L52 = ATTR(arg,mode1);
    L53 = (*dMODE_814247358[TAG(L52)])(L52, ATTR(s_arg,mode1));
    L541_=!(L53); 
    L47 = L541_;
   }
   if (L47) {
    ret_val = FALSE;
    return ret_val;
   }
   L551_=INTPLUS(L51,1); 
   L51 = L551_;
  }
 }
 after_loop: ;
 {
  BOOL f_L561_ = TRUE;
  BOOL f_L571_ = TRUE;
  /* loop index variable */
  L61 = 0;
  L31 = ATTR(self,hot);
  L41 = ATTRs(s,hot,((ARRAYBOOL) NULL));
  L561_br=L31==NULL?0:ASIZE((ARRAYBOOL)L31); 
  L571_br=L41==NULL?0:ASIZE((ARRAYBOOL)L41); 
  while (1) {
   if(L61>=L561_br)  goto after_loop1; 
   aL561_=ARR((ARRAYBOOL)L31,L61); 
   L59 = aL561_;
   if(L61>=L571_br)  goto after_loop1; 
   aL571_=ARR((ARRAYBOOL)L41,L61); 
   L62 = aL571_;
   L631_=L59==L62; 
   L64 = L631_;
   L651_=!(L64); 
   if (L651_) {
    ret_val = FALSE;
    return ret_val;
   }
   L661_=INTPLUS(L61,1); 
   L61 = L661_;
  }
 }
 after_loop1: ;
 has_ret_self2 = self;
 L69 = (ATTR(has_ret_self2,ret)==((dTP) NULL));
 L701_=!(L69); 
 ret_val4 = L701_;
 if (ret_val4) {
  L71 = ATTR(self,ret);
  L72 = (*dTP_is242312711[TAG(L71)])(L71, ((OB) ATTR(s,ret)));
  L731_=!(L72); 
  L68 = L731_;
 } else {
  L68 = FALSE;
 }
 if (L68) {
  L75 = ATTR(self,returns_same);
  L761_=!(L75); 
  if (L761_) {
   L74 = TRUE;
  } else {
   L77 = ATTR(s,returns_same);
   L781_=!(L77); 
   L74 = L781_;
  }
  L67 = L74;
 } else {
  L67 = FALSE;
 }
 if (L67) {
  ret_val = FALSE;
  return ret_val;
 }
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL SIG_is275586960(SIG self) {
 BOOL ret_val = BOOL_zero;
 ARRAYARG L11;
 dTP tp;
 INT L21 = INT_zero;
 dTP L3;
 INT L4;
 INT L5;
 BOOL L61_;
 BOOL L7;
 BOOL L81_;
 BOOL L9;
 ARRAYARG L10;
 INT L121_;
 INT L13;
 BOOL L141_;
 INT L151_br;
 ARG aL151_;
 ARG L16;
 ARG L17;
 dTP L18;
 BOOL L19;
 BOOL L201_;
 INT L221_;
 BOOL L23;
 BOOL L241_;
 dTP L25;
 BOOL L26;
 BOOL L271_;
 L3 = ATTR(self,tp);
 L4 = (*dTP_kindrINT[TAG(L3)])(L3);
 L5 = TP_KIN462481068;
 L61_=(L4)==(L5); 
 L7 = L61_;
 L81_=!(L7); 
 if (L81_) {
  ret_val = FALSE;
  return ret_val;
 }
 L10 = ATTR(self,args);
 L121_=(L10)==NULL?0:ASIZE((ARRAYARG)L10); 
 L13 = L121_;
 L141_=(L13)==(0); 
 if (L141_) {
  L9 = (ATTR(self,ret)==((dTP) NULL));
 } else {
  L9 = FALSE;
 }
 if (L9) {
  ret_val = FALSE;
  return ret_val;
 }
 {
  BOOL f_L151_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = ATTR(self,args);
  L151_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
  while (1) {
   if(L21>=L151_br)  goto after_loop; 
   aL151_=ARR((ARRAYARG)L11,L21); 
   L17=aL151_;
   tp = ATTR(L17,tp);
   L18 = tp;
   L19 = (*dTP_is411624931[TAG(L18)])(L18);
   L201_=!(L19); 
   if (L201_) {
    ret_val = FALSE;
    return ret_val;
   }
   L221_=INTPLUS(L21,1); 
   L21 = L221_;
  }
 }
 after_loop: ;
 L23 = (ATTR(self,ret)==((dTP) NULL));
 L241_=!(L23); 
 if (L241_) {
  L25 = ATTR(self,ret);
  L26 = (*dTP_is411624931[TAG(L25)])(L25);
  L271_=!(L26); 
  if (L271_) {
   ret_val = FALSE;
   return ret_val;
  }
 }
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL SIG_is418491101(SIG self) {
 BOOL ret_val = BOOL_zero;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val1 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L4;
 BOOL L5;
 BOOL L61_;
 BOOL L7;
 BOOL L81_;
 L1 = (ATTR(self,builtin_info)==((CONFIG_ROUT) NULL));
 L21_=!(L1); 
 if (L21_) {
  ret_val = TRUE;
  return ret_val;
 }
 L5 = (ATTR(self,srcsig)==((SIG) NULL));
 L61_=!(L5); 
 if (L61_) {
  is_eq_self = ATTR(self,srcsig);
  is_eq_s = self;
  ret_val1 = (is_eq_self==is_eq_s);
  L7 = ret_val1;
  L81_=!(L7); 
  L4 = L81_;
 } else {
  L4 = FALSE;
 }
 if (L4) {
  L3 = SIG_is418491101(ATTR(self,srcsig));
 } else {
  L3 = FALSE;
 }
 ret_val = L3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL SIG_is600379186(SIG self) {
 BOOL ret_val = BOOL_zero;
 ARRAYARG L11;
 dTP tp;
 INT L21 = INT_zero;
 dTP L3;
 INT L4;
 INT L5;
 BOOL L61_;
 BOOL L7;
 BOOL L81_;
 BOOL L9;
 ARRAYARG L10;
 INT L121_;
 INT L13;
 BOOL L141_;
 INT L151_br;
 ARG aL151_;
 ARG L16;
 ARG L17;
 dTP L18;
 BOOL L19;
 BOOL L201_;
 INT L221_;
 BOOL L23;
 BOOL L241_;
 dTP L25;
 BOOL L26;
 BOOL L271_;
 L3 = ATTR(self,tp);
 L4 = (*dTP_kindrINT[TAG(L3)])(L3);
 L5 = TP_KIND_ext_c_tp;
 L61_=(L4)==(L5); 
 L7 = L61_;
 L81_=!(L7); 
 if (L81_) {
  ret_val = FALSE;
  return ret_val;
 }
 L10 = ATTR(self,args);
 L121_=(L10)==NULL?0:ASIZE((ARRAYARG)L10); 
 L13 = L121_;
 L141_=(L13)==(0); 
 if (L141_) {
  L9 = (ATTR(self,ret)==((dTP) NULL));
 } else {
  L9 = FALSE;
 }
 if (L9) {
  ret_val = FALSE;
  return ret_val;
 }
 {
  BOOL f_L151_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = ATTR(self,args);
  L151_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
  while (1) {
   if(L21>=L151_br)  goto after_loop; 
   aL151_=ARR((ARRAYARG)L11,L21); 
   L17=aL151_;
   tp = ATTR(L17,tp);
   L18 = tp;
   L19 = (*dTP_is_crBOOL[TAG(L18)])(L18);
   L201_=!(L19); 
   if (L201_) {
    ret_val = FALSE;
    return ret_val;
   }
   L221_=INTPLUS(L21,1); 
   L21 = L221_;
  }
 }
 after_loop: ;
 L23 = (ATTR(self,ret)==((dTP) NULL));
 L241_=!(L23); 
 if (L241_) {
  L25 = ATTR(self,ret);
  L26 = (*dTP_is_crBOOL[TAG(L25)])(L25);
  L271_=!(L26); 
  if (L271_) {
   ret_val = FALSE;
   return ret_val;
  }
 }
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL SIG_is683883765(SIG self) {
 BOOL ret_val = BOOL_zero;
 ARRAYARG L11;
 dTP a;
 ARRAYARG L21;
 dTP a11;
 IDENT is_iter_self = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 SIG prog_self;
 PROG ret_val2;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val4;
 INT L31 = INT_zero;
 SIG prog_self1;
 PROG ret_val5;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val6;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val7;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val8;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val9;
 SIG prog_self2;
 PROG ret_val10;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val11;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val12;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val13;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val14;
 INT L41 = INT_zero;
 SIG prog_self3;
 PROG ret_val15;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val16;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val17;
 STR plus_self12;
 STR plus_sarg12;
 STR ret_val18;
 STR plus_self13;
 STR plus_sarg13;
 STR ret_val19;
 SIG prog_self4;
 PROG ret_val20;
 STR plus_self14;
 STR plus_sarg14;
 STR ret_val21;
 STR plus_self15;
 STR plus_sarg15;
 STR ret_val22;
 STR plus_self16;
 STR plus_sarg16;
 STR ret_val23;
 STR plus_self17;
 STR plus_sarg17;
 STR ret_val24;
 BOOL L5;
 BOOL L6;
 BOOL L71_;
 STR L8;
 INT L9;
 INT L101_;
 INT L12;
 CHAR L131_;
 CHAR L14;
 BOOL L151_;
 dTP L16;
 extern STR Theitersignature;
 extern STR isntal884974273;
 dTP L17;
 INT L18;
 INT L19;
 BOOL L201_;
 INT L221_br;
 ARG aL221_;
 ARG L23;
 ARG L24;
 BOOL L25;
 dTP L26;
 BOOL L27;
 BOOL L28;
 dTP L29;
 BOOL L30;
 BOOL L321_;
 BOOL L33;
 BOOL L341_;
 BOOL L35;
 BOOL L361_;
 dTP L37;
 extern STR Thesignature;
 extern STR isnotl1032304084;
 dTP L38;
 extern STR isnoto1261563554;
 INT L391_;
 BOOL L40;
 BOOL L42;
 BOOL L431_;
 BOOL L44;
 BOOL L45;
 dTP L46;
 BOOL L47;
 BOOL L481_;
 BOOL L49;
 BOOL L501_;
 BOOL L51;
 BOOL L521_;
 dTP L53;
 extern STR Thesignature;
 extern STR isnotl409495142;
 dTP L54;
 extern STR isnoto1261563554;
 dTP L55;
 INT L56;
 INT L57;
 BOOL L581_;
 INT L591_br;
 ARG aL591_;
 ARG L60;
 ARG L61;
 BOOL L62;
 dTP L63;
 dTP L64;
 BOOL L65;
 BOOL L661_;
 dTP L67;
 extern STR Thesignature;
 extern STR isnotl1858055900;
 dTP L68;
 extern STR isnoto1261563554;
 INT L691_;
 BOOL L70;
 BOOL L72;
 BOOL L731_;
 dTP L74;
 BOOL L75;
 BOOL L761_;
 dTP L77;
 extern STR Thesignature;
 extern STR isnotl2146161962;
 dTP L78;
 extern STR isnoto1261563554;
 is_iter_self = ATTR(self,name1);
 L6 = (is_iter_self.str==((STR) NULL));
 L71_=!(L6); 
 if (L71_) {
  L8 = is_iter_self.str;
  L9 = STR_sizerINT(is_iter_self.str);
  L101_=INTMINUS(L9,1); 
  L12 = L101_;
  L131_=ARR((STR)L8,L12); 
  L14 = L131_;
  L151_=L14=='!'; 
  L5 = L151_;
 } else {
  L5 = FALSE;
 }
 ret_val1 = L5;
 if (ret_val1) {
  prog_self = self;
  L16 = ATTR(prog_self,tp);
  ret_val2 = (*dTP_progrPROG[TAG(L16)])(L16);
  plus_self = ((STR) &Theitersignature);
  plus_sarg = ATTR(self,str);
  ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val3;
  plus_sarg1 = ((STR) &isntal884974273);
  ret_val4 = STR_ap2004550586(plus_self1, plus_sarg1);
  PROG_err_STR(ret_val2, ret_val4);
  ret_val = FALSE;
  return ret_val;
 }
 L17 = ATTR(self,tp);
 L18 = (*dTP_kindrINT[TAG(L17)])(L17);
 L19 = TP_KIND_ext_c_tp;
 L201_=(L18)==(L19); 
 if (L201_) {
  {
   BOOL f_L221_ = TRUE;
   /* loop index variable */
   L31 = 0;
   L11 = ATTR(self,args);
   L221_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
   while (1) {
    if(L31>=L221_br)  goto after_loop; 
    aL221_=ARR((ARRAYARG)L11,L31); 
    L24=aL221_;
    a = ATTR(L24,tp);
    L26 = a;
    if ((*dTP_is1811059018[TAG(L26)])(L26)) {
     L25 = TRUE;
    } else {
     L29 = a;
     L30 = (*dTP_is_crBOOL[TAG(L29)])(L29);
     L321_=!(L30); 
     if (L321_) {
      L33 = SIG_is785829834(self, a);
      L341_=!(L33); 
      L28 = L341_;
     } else {
      L28 = FALSE;
     }
     if (L28) {
      L35 = SIG_is784970610(self, a);
      L361_=!(L35); 
      L27 = L361_;
     } else {
      L27 = FALSE;
     }
     L25 = L27;
    }
    if (L25) {
     prog_self1 = self;
     L37 = ATTR(prog_self1,tp);
     ret_val5 = (*dTP_progrPROG[TAG(L37)])(L37);
     plus_self2 = ((STR) &Thesignature);
     plus_sarg2 = ATTR(self,str);
     ret_val6 = STR_ap2004550586(plus_self2, plus_sarg2);
     plus_self3 = ret_val6;
     plus_sarg3 = ((STR) &isnotl1032304084);
     ret_val7 = STR_ap2004550586(plus_self3, plus_sarg3);
     plus_self4 = ret_val7;
     L38 = a;
     plus_sarg4 = (*dTP_strrSTR[TAG(L38)])(L38);
     ret_val8 = STR_ap2004550586(plus_self4, plus_sarg4);
     plus_self5 = ret_val8;
     plus_sarg5 = ((STR) &isnoto1261563554);
     ret_val9 = STR_ap2004550586(plus_self5, plus_sarg5);
     PROG_err_STR(ret_val5, ret_val9);
     ret_val = FALSE;
     return ret_val;
    }
    L391_=INTPLUS(L31,1); 
    L31 = L391_;
   }
  }
  after_loop: ;
  L42 = (ATTR(self,ret)==((dTP) NULL));
  L431_=!(L42); 
  if (L431_) {
   L46 = ATTR(self,ret);
   L47 = (*dTP_is_crBOOL[TAG(L46)])(L46);
   L481_=!(L47); 
   if (L481_) {
    L49 = SIG_is785829834(self, ATTR(self,ret));
    L501_=!(L49); 
    L45 = L501_;
   } else {
    L45 = FALSE;
   }
   if (L45) {
    L51 = SIG_is784970610(self, ATTR(self,ret));
    L521_=!(L51); 
    L44 = L521_;
   } else {
    L44 = FALSE;
   }
   L40 = L44;
  } else {
   L40 = FALSE;
  }
  if (L40) {
   prog_self2 = self;
   L53 = ATTR(prog_self2,tp);
   ret_val10 = (*dTP_progrPROG[TAG(L53)])(L53);
   plus_self6 = ((STR) &Thesignature);
   plus_sarg6 = ATTR(self,str);
   ret_val11 = STR_ap2004550586(plus_self6, plus_sarg6);
   plus_self7 = ret_val11;
   plus_sarg7 = ((STR) &isnotl409495142);
   ret_val12 = STR_ap2004550586(plus_self7, plus_sarg7);
   plus_self8 = ret_val12;
   L54 = ATTR(self,ret);
   plus_sarg8 = (*dTP_strrSTR[TAG(L54)])(L54);
   ret_val13 = STR_ap2004550586(plus_self8, plus_sarg8);
   plus_self9 = ret_val13;
   plus_sarg9 = ((STR) &isnoto1261563554);
   ret_val14 = STR_ap2004550586(plus_self9, plus_sarg9);
   PROG_err_STR(ret_val10, ret_val14);
   ret_val = FALSE;
   return ret_val;
  }
 }
 else {
  L55 = ATTR(self,tp);
  L56 = (*dTP_kindrINT[TAG(L55)])(L55);
  L57 = TP_KIN462481068;
  L581_=(L56)==(L57); 
  if (L581_) {
   {
    BOOL f_L591_ = TRUE;
    /* loop index variable */
    L41 = 0;
    L21 = ATTR(self,args);
    L591_br=L21==NULL?0:ASIZE((ARRAYARG)L21); 
    while (1) {
     if(L41>=L591_br)  goto after_loop1; 
     aL591_=ARR((ARRAYARG)L21,L41); 
     L61=aL591_;
     a11 = ATTR(L61,tp);
     L63 = a11;
     if ((*dTP_is1811059018[TAG(L63)])(L63)) {
      L62 = TRUE;
     } else {
      L64 = a11;
      L65 = (*dTP_is411624931[TAG(L64)])(L64);
      L661_=!(L65); 
      L62 = L661_;
     }
     if (L62) {
      prog_self3 = self;
      L67 = ATTR(prog_self3,tp);
      ret_val15 = (*dTP_progrPROG[TAG(L67)])(L67);
      plus_self10 = ((STR) &Thesignature);
      plus_sarg10 = ATTR(self,str);
      ret_val16 = STR_ap2004550586(plus_self10, plus_sarg10);
      plus_self11 = ret_val16;
      plus_sarg11 = ((STR) &isnotl1858055900);
      ret_val17 = STR_ap2004550586(plus_self11, plus_sarg11);
      plus_self12 = ret_val17;
      L68 = a11;
      plus_sarg12 = (*dTP_strrSTR[TAG(L68)])(L68);
      ret_val18 = STR_ap2004550586(plus_self12, plus_sarg12);
      plus_self13 = ret_val18;
      plus_sarg13 = ((STR) &isnoto1261563554);
      ret_val19 = STR_ap2004550586(plus_self13, plus_sarg13);
      PROG_err_STR(ret_val15, ret_val19);
      ret_val = FALSE;
      return ret_val;
     }
     L691_=INTPLUS(L41,1); 
     L41 = L691_;
    }
   }
   after_loop1: ;
   L72 = (ATTR(self,ret)==((dTP) NULL));
   L731_=!(L72); 
   if (L731_) {
    L74 = ATTR(self,ret);
    L75 = (*dTP_is411624931[TAG(L74)])(L74);
    L761_=!(L75); 
    L70 = L761_;
   } else {
    L70 = FALSE;
   }
   if (L70) {
    prog_self4 = self;
    L77 = ATTR(prog_self4,tp);
    ret_val20 = (*dTP_progrPROG[TAG(L77)])(L77);
    plus_self14 = ((STR) &Thesignature);
    plus_sarg14 = ATTR(self,str);
    ret_val21 = STR_ap2004550586(plus_self14, plus_sarg14);
    plus_self15 = ret_val21;
    plus_sarg15 = ((STR) &isnotl2146161962);
    ret_val22 = STR_ap2004550586(plus_self15, plus_sarg15);
    plus_self16 = ret_val22;
    L78 = ATTR(self,ret);
    plus_sarg16 = (*dTP_strrSTR[TAG(L78)])(L78);
    ret_val23 = STR_ap2004550586(plus_self16, plus_sarg16);
    plus_self17 = ret_val23;
    plus_sarg17 = ((STR) &isnoto1261563554);
    ret_val24 = STR_ap2004550586(plus_self17, plus_sarg17);
    PROG_err_STR(ret_val20, ret_val24);
    ret_val = FALSE;
    return ret_val;
   }
  }
 }
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL SIG_is688012902(SIG self, OB s) {
 BOOL ret_val = BOOL_zero;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val1 = BOOL_zero;
 if (s==NULL) {
  goto other173;
 } else
 switch (TAG(s)) {
  case SIG_tag:
   is_eq_self = self;
   is_eq_s = ((SIG) s);
   ret_val1 = (is_eq_self==is_eq_s);
   ret_val = ret_val1;
   return ret_val; break;
  default: ;
  other173: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL SIG_is784970610(SIG self, dTP tp) {
 BOOL ret_val = BOOL_zero;
 IMPL im;
 TP_CLASS ar;
 dTP L11;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 OUT create_self;
 OUT ret_val2;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val3;
 FILE1 r;
 OUT create_self1;
 OUT ret_val4;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val5;
 FILE1 r1;
 BOOL L2;
 BOOL L31_;
 extern STR Compil1400918383;
 FILE1 L4;
 OB L5;
 ARRAYdTP L7;
 INT L81_;
 INT L9;
 BOOL L101_;
 BOOL L12;
 BOOL L131_;
 extern STR Compil839553990;
 FILE1 L14;
 OB L15;
 ARRAYdTP L17;
 dTP L181_;
 dTP L19;
 dTP L20;
 dTP L21;
 dTP L22;
 dTP L23;
 dTP L24;
 dTP L25;
 dTP L26;
 dTP L27;
 dTP L28;
 dTP L29;
 dTP L30;
 dTP L32;
 dTP L33;
 dTP L34;
 dTP L35;
 if (tp==NULL) {
  goto other124;
 } else
 switch (TAG(tp)) {
  case TP_CLASS_tag:
   im = TP_CLA1109727233(((TP_CLASS) tp));
   if ((im==((IMPL) NULL))) {
    ret_val = FALSE;
    return ret_val;
   }
   ar = ATTR(im,arr);
   if ((ar==((TP_CLASS) NULL))) {
    ret_val = FALSE;
    return ret_val;
   }
   is_eq_self = ATTR(ar,name1);
   is_eq_i = IDENT_1666445894;
   ret_val1 = (is_eq_self.str==is_eq_i.str);
   L2 = ret_val1;
   L31_=!(L2); 
   if (L31_) {
    ret_val = FALSE;
    return ret_val;
   }
   if ((ATTR(ar,params)==((ARRAYdTP) NULL))) {
    create_self = ((OUT) NULL);
    ret_val2 = create_self;
    plus_self = ret_val2;
    plus_s = ((STR) &Compil1400918383);
    stdout_self = ((FILE1) NULL);
    L5=ZALLOC(sizeof(struct FILE1_struct));
    if (L5==NULL) FATAL("Unable to allocate more memory");
    ((OB)L5)->header.tag=FILE1_tag;
    L4 = ((FILE1) L5);
    r = L4;
    
    SATTR(r,fp,stdout);
    ret_val3 = r;
    FILE_plus_STR(ret_val3, plus_s);
    ret_val = FALSE;
    return ret_val;
   }
   L7 = ATTR(ar,params);
   L81_=(L7)==NULL?0:ASIZE((ARRAYdTP)L7); 
   L9 = L81_;
   L101_=(L9)==(1); 
   L12 = L101_;
   L131_=!(L12); 
   if (L131_) {
    create_self1 = ((OUT) NULL);
    ret_val4 = create_self1;
    plus_self1 = ret_val4;
    plus_s1 = ((STR) &Compil839553990);
    stdout_self1 = ((FILE1) NULL);
    L15=ZALLOC(sizeof(struct FILE1_struct));
    if (L15==NULL) FATAL("Unable to allocate more memory");
    ((OB)L15)->header.tag=FILE1_tag;
    L14 = ((FILE1) L15);
    r1 = L14;
    
    SATTR(r1,fp,stdout);
    ret_val5 = r1;
    FILE_plus_STR(ret_val5, plus_s1);
    ret_val = FALSE;
    return ret_val;
   }
   L17 = ATTR(ar,params);
   L181_=(dTP)ARR((ARRAYdTP)L17,0); 
   L11 = L181_;
   L19 = L11;
   if ((*dTP_is242312711[TAG(L19)])(L19, ((OB) TP_BUILTIN_char))) {
    ret_val = TRUE;
    return ret_val;
   }
   else {
    L20 = L11;
    if ((*dTP_is242312711[TAG(L20)])(L20, ((OB) TP_BUILTIN_int))) {
     ret_val = TRUE;
     return ret_val;
    }
    else {
     L21 = L11;
     if ((*dTP_is242312711[TAG(L21)])(L21, ((OB) TP_BUILTIN_flt))) {
      ret_val = TRUE;
      return ret_val;
     }
     else {
      L22 = L11;
      if ((*dTP_is242312711[TAG(L22)])(L22, ((OB) TP_BUILTIN_fltd))) {
       ret_val = TRUE;
       return ret_val;
      }
      else {
       L23 = L11;
       if ((*dTP_is242312711[TAG(L23)])(L23, ((OB) TP_BUILTIN_fltx))) {
        ret_val = TRUE;
        return ret_val;
       }
       else {
        L24 = L11;
        if ((*dTP_is242312711[TAG(L24)])(L24, ((OB) TP_BUILTIN_fltdx))) {
         ret_val = TRUE;
         return ret_val;
        }
        else {
         L25 = L11;
         if ((*dTP_is242312711[TAG(L25)])(L25, ((OB) TP_BUI389447236))) {
          ret_val = TRUE;
          return ret_val;
         }
         else {
          L26 = L11;
          if ((*dTP_is242312711[TAG(L26)])(L26, ((OB) TP_BUI403444172))) {
           ret_val = TRUE;
           return ret_val;
          }
          else {
           L27 = L11;
           if ((*dTP_is242312711[TAG(L27)])(L27, ((OB) TP_BUI374366290))) {
            ret_val = TRUE;
            return ret_val;
           }
           else {
            L28 = L11;
            if ((*dTP_is242312711[TAG(L28)])(L28, ((OB) TP_BUI1805763264))) {
             ret_val = TRUE;
             return ret_val;
            }
            else {
             L29 = L11;
             if ((*dTP_is242312711[TAG(L29)])(L29, ((OB) TP_BUI2088674906))) {
              ret_val = TRUE;
              return ret_val;
             }
             else {
              L30 = L11;
              if ((*dTP_is242312711[TAG(L30)])(L30, ((OB) TP_BUI1194559023))) {
               ret_val = TRUE;
               return ret_val;
              }
              else {
               L32 = L11;
               if ((*dTP_is242312711[TAG(L32)])(L32, ((OB) TP_BUI1370948386))) {
                ret_val = TRUE;
                return ret_val;
               }
               else {
                L33 = L11;
                if ((*dTP_is242312711[TAG(L33)])(L33, ((OB) TP_BUI633637838))) {
                 ret_val = TRUE;
                 return ret_val;
                }
                else {
                 L34 = L11;
                 if ((*dTP_is242312711[TAG(L34)])(L34, ((OB) TP_BUI1032249067))) {
                  ret_val = TRUE;
                  return ret_val;
                 }
                 else {
                  L35 = L11;
                  if ((*dTP_is242312711[TAG(L35)])(L35, ((OB) TP_BUI1116976640))) {
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
              }
             }
            }
           }
          }
         }
        }
       }
      }
     }
    }
   } break;
  default: ;
  other124: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL SIG_is785829834(SIG self, dTP tp) {
 BOOL ret_val = BOOL_zero;
 dTP L11;
 ARRAYTP_CLASS L21;
 ARRAYTP_CLASS L31;
 ARRAYTP_CLASS L41;
 INT L51 = INT_zero;
 dTP L6;
 dTP L7;
 dTP L8;
 dTP L9;
 dTP L10;
 dTP L12;
 dTP L13;
 dTP L14;
 dTP L15;
 dTP L16;
 dTP L17;
 dTP L18;
 dTP L19;
 dTP L20;
 dTP L22;
 dTP L23;
 dTP L24;
 INT L251_br;
 TP_CLASS aL251_;
 INT L261_br;
INT i_L261_=0;
 TP_CLASS aL261_;
 INT L271_br;
INT i_L271_=0;
 TP_CLASS aL271_;
 BOOL L28;
 BOOL L29;
 TP_CLASS L30;
 dTP L32;
 TP_CLASS L33;
 dTP L34;
 TP_CLASS L35;
 dTP L36;
 INT L371_;
 L11 = tp;
 L6 = L11;
 if ((*dTP_is242312711[TAG(L6)])(L6, ((OB) TP_BUILTIN_bool))) {
  ret_val = TRUE;
  return ret_val;
 }
 else {
  L7 = L11;
  if ((*dTP_is242312711[TAG(L7)])(L7, ((OB) TP_BUILTIN_char))) {
   ret_val = TRUE;
   return ret_val;
  }
  else {
   L8 = L11;
   if ((*dTP_is242312711[TAG(L8)])(L8, ((OB) TP_BUILTIN_int))) {
    ret_val = TRUE;
    return ret_val;
   }
   else {
    L9 = L11;
    if ((*dTP_is242312711[TAG(L9)])(L9, ((OB) TP_BUILTIN_flt))) {
     ret_val = TRUE;
     return ret_val;
    }
    else {
     L10 = L11;
     if ((*dTP_is242312711[TAG(L10)])(L10, ((OB) TP_BUILTIN_fltd))) {
      ret_val = TRUE;
      return ret_val;
     }
     else {
      L12 = L11;
      if ((*dTP_is242312711[TAG(L12)])(L12, ((OB) TP_BUILTIN_fltx))) {
       ret_val = TRUE;
       return ret_val;
      }
      else {
       L13 = L11;
       if ((*dTP_is242312711[TAG(L13)])(L13, ((OB) TP_BUILTIN_fltdx))) {
        ret_val = TRUE;
        return ret_val;
       }
       else {
        L14 = L11;
        if ((*dTP_is242312711[TAG(L14)])(L14, ((OB) TP_BUI389447236))) {
         ret_val = TRUE;
         return ret_val;
        }
        else {
         L15 = L11;
         if ((*dTP_is242312711[TAG(L15)])(L15, ((OB) TP_BUI403444172))) {
          ret_val = TRUE;
          return ret_val;
         }
         else {
          L16 = L11;
          if ((*dTP_is242312711[TAG(L16)])(L16, ((OB) TP_BUI374366290))) {
           ret_val = TRUE;
           return ret_val;
          }
          else {
           L17 = L11;
           if ((*dTP_is242312711[TAG(L17)])(L17, ((OB) TP_BUI1805763264))) {
            ret_val = TRUE;
            return ret_val;
           }
           else {
            L18 = L11;
            if ((*dTP_is242312711[TAG(L18)])(L18, ((OB) TP_BUI2088674906))) {
             ret_val = TRUE;
             return ret_val;
            }
            else {
             L19 = L11;
             if ((*dTP_is242312711[TAG(L19)])(L19, ((OB) TP_BUI1194559023))) {
              ret_val = TRUE;
              return ret_val;
             }
             else {
              L20 = L11;
              if ((*dTP_is242312711[TAG(L20)])(L20, ((OB) TP_BUI1370948386))) {
               ret_val = TRUE;
               return ret_val;
              }
              else {
               L22 = L11;
               if ((*dTP_is242312711[TAG(L22)])(L22, ((OB) TP_BUI633637838))) {
                ret_val = TRUE;
                return ret_val;
               }
               else {
                L23 = L11;
                if ((*dTP_is242312711[TAG(L23)])(L23, ((OB) TP_BUI1032249067))) {
                 ret_val = TRUE;
                 return ret_val;
                }
                else {
                 L24 = L11;
                 if ((*dTP_is242312711[TAG(L24)])(L24, ((OB) TP_BUI1116976640))) {
                  ret_val = TRUE;
                  return ret_val;
                 }
                 else {
                  {
                   BOOL f_L251_ = TRUE;
                   BOOL f_L261_ = TRUE;
                   BOOL f_L271_ = TRUE;
                   /* loop index variable */
                   L51 = 0;
                   L21 = TP_BUI1517259338;
                   L31 = TP_BUI1236844942;
                   L41 = TP_BUI1236844961;
                   L251_br=L21==NULL?0:ASIZE((ARRAYTP_CLASS)L21); 
                   L261_br=L31==NULL?0:ASIZE((ARRAYTP_CLASS)L31); 
                   i_L261_=0;
                   L271_br=L41==NULL?0:ASIZE((ARRAYTP_CLASS)L41); 
                   i_L271_=0;
                   while (1) {
                    if(L51>=L251_br)  goto after_loop; 
                    aL251_=ARR((ARRAYTP_CLASS)L21,L51); 
                    L32 = tp;
                    if ((*dTP_is242312711[TAG(L32)])(L32, ((OB) aL251_))) {
                     L29 = TRUE;
                    } else {
                     if(i_L261_>=L261_br)  goto after_loop; 
                     aL261_=ARR((ARRAYTP_CLASS)L31,i_L261_); i_L261_++;
                     L34 = tp;
                     L29 = (*dTP_is242312711[TAG(L34)])(L34, ((OB) aL261_));
                    }
                    if (L29) {
                     L28 = TRUE;
                    } else {
                     if(i_L271_>=L271_br)  goto after_loop; 
                     aL271_=ARR((ARRAYTP_CLASS)L41,i_L271_); i_L271_++;
                     L36 = tp;
                     L28 = (*dTP_is242312711[TAG(L36)])(L36, ((OB) aL271_));
                    }
                    if (L28) {
                     ret_val = TRUE;
                     return ret_val;
                    }
                    L371_=INTPLUS(L51,1); 
                    L51 = L371_;
                   }
                  }
                  after_loop: ;
                  if (tp==NULL) {
                   goto other123;
                  } else
                  switch (TAG(tp)) {
                   case TP_ROUT_tag:
                    if (ATTR(((TP_ROUT) tp),is_fortran)) {
                     ret_val = TRUE;
                     return ret_val;
                    } break;
                   default: ;
                   other123: ;
                  }
                  ret_val = FALSE;
                  return ret_val;
                 }
                }
               }
              }
             }
            }
           }
          }
         }
        }
       }
      }
     }
    }
   }
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

INT SIDE_E484651583(SIDE_EFFECT self) {
 INT ret_val = INT_zero;
 SIDE_EFFECT is_array_self;
 BOOL ret_val1 = BOOL_zero;
 SIDE_EFFECT is_local_self;
 BOOL ret_val2 = BOOL_zero;
 IDENT hash_self = IDENT_zero;
 INT ret_val3 = INT_zero;
 INT x = INT_zero;
 IDENT hash_self1 = IDENT_zero;
 INT ret_val4 = INT_zero;
 INT x1 = INT_zero;
 dTP L1;
 STR L2;
 INT L31_;
 INT L41_;
 INT L5;
 INT L61_;
 dTP L7;
 INT L8;
 STR L9;
 INT L101_;
 INT L111_;
 INT L12;
 INT L131_;
 INT L14;
 INT L151_;
 is_array_self = self;
 ret_val1 = (ATTR(is_array_self,name1).str==(STR)0);
 if (ret_val1) {
  L1 = ATTR(self,tp);
  ret_val = STR_hashrINT((*dTP_strrSTR[TAG(L1)])(L1));
  return ret_val;
 }
 else {
  is_local_self = self;
  ret_val2 = (ATTR(is_local_self,tp)==((dTP) NULL));
  if (ret_val2) {
   hash_self = ATTR(self,name1);
   L2 = hash_self.str;
   L31_=SYSID(L2); 
   x = L31_;
   L41_=((unsigned)x*(unsigned)1664525); 
   L5 = L41_;
   L61_=((unsigned)L5+(unsigned)1013904223); 
   ret_val3 = L61_;
   ret_val = ret_val3;
   return ret_val;
  }
  else {
   L7 = ATTR(self,tp);
   L8 = STR_hashrINT((*dTP_strrSTR[TAG(L7)])(L7));
   hash_self1 = ATTR(self,name1);
   L9 = hash_self1.str;
   L101_=SYSID(L9); 
   x1 = L101_;
   L111_=((unsigned)x1*(unsigned)1664525); 
   L12 = L111_;
   L131_=((unsigned)L12+(unsigned)1013904223); 
   ret_val4 = L131_;
   L14 = ret_val4;
   L151_=L8^L14; 
   ret_val = L151_;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

INT SIG_hashrINT(SIG self) {
 INT ret_val = INT_zero;
 INT L11_;
 L11_=SYSID(self); 
 ret_val = INT_hashrINT(L11_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SE_CONTEXT SE_CON1395318461(SE_CONTEXT self, PROG for_prog, SIG for_sig) {
 SE_CONTEXT ret_val;
 SE_CONTEXT new_sec;
 TP_CLASS stp = ((TP_CLASS) NULL);
 dTP ostp;
 FSETdTP desof;
 FSETdTP L11;
 dTP tp;
 IFC ifc;
 SIG cs;
 SE_CONTEXT nse;
 TP_GRAPH_ABS_DES des_of_self;
 TP_CLASS des_of_tp;
 FSETdTP ret_val1;
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
 FILE1 stdout_self2;
 FILE1 ret_val7;
 FILE1 r2;
 dTP L2;
 dTP L3;
 extern STR Missingsignature;
 FILE1 L4;
 OB L5;
 FILE1 L7;
 OB L8;
 extern STR markingfull;
 FILE1 L10;
 OB L12;
 new_sec = SE_CON32319044(((SE_CONTEXT) NULL), for_prog, for_sig);
 ostp = ATTR(for_sig,tp);
 switch (TAG(ostp)) {
  case TP_CLASS_tag:
   stp = ((TP_CLASS) ostp); break;
  default: ;
   FATAL("No applicable type in typecase\nin SE_CONTEXT::get_abstract_union(PROG,SIG):SE_CONTEXT\n./Middle/side_effect.sa:557:14");
 }
 des_of_self = ATTR(for_prog,tp_graph_abs_des);
 des_of_tp = stp;
 ret_val1 = FMAPTP861983338(ATTR(des_of_self,tbl), des_of_tp);
 desof = ret_val1;
 {
  struct FSETdTP_eltbrdTP_frame_struct other1_0;
FSETdTP_eltbrdTP_frame noname1 = &other1_0;
  L11 = desof;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FSETdTP_eltbrdTP(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   tp = L2;
   L3 = tp;
   ifc = (*dTP_ifcrIFC[TAG(L3)])(L3);
   cs = IFC_si2085565042(ifc, for_sig);
   nse = SIG_ge642497605(cs, for_prog);
   if ((nse==((SE_CONTEXT) NULL))) {
    if (SE_CON765348064(self)) {
     create_self = ((OUT) NULL);
     ret_val2 = create_self;
     plus_self = ret_val2;
     plus_s = ((STR) &Missingsignature);
     stdout_self = ((FILE1) NULL);
     L5=ZALLOC(sizeof(struct FILE1_struct));
     if (L5==NULL) FATAL("Unable to allocate more memory");
     ((OB)L5)->header.tag=FILE1_tag;
     L4 = ((FILE1) L5);
     r = L4;
     
     SATTR(r,fp,stdout);
     ret_val4 = r;
     FILE_plus_STR(ret_val4, plus_s);
     ret_val3 = plus_self;
     plus_self1 = ret_val3;
     plus_s1 = ATTR(cs,str);
     stdout_self1 = ((FILE1) NULL);
     L8=ZALLOC(sizeof(struct FILE1_struct));
     if (L8==NULL) FATAL("Unable to allocate more memory");
     ((OB)L8)->header.tag=FILE1_tag;
     L7 = ((FILE1) L8);
     r1 = L7;
     
     SATTR(r1,fp,stdout);
     ret_val6 = r1;
     FILE_plus_STR(ret_val6, plus_s1);
     ret_val5 = plus_self1;
     plus_self2 = ret_val5;
     plus_s2 = ((STR) &markingfull);
     stdout_self2 = ((FILE1) NULL);
     L12=ZALLOC(sizeof(struct FILE1_struct));
     if (L12==NULL) FATAL("Unable to allocate more memory");
     ((OB)L12)->header.tag=FILE1_tag;
     L10 = ((FILE1) L12);
     r2 = L10;
     
     SATTR(r2,fp,stdout);
     ret_val7 = r2;
     FILE_plus_STR(ret_val7, plus_s2);
    }
    SE_CON320528666(new_sec);
   }
   else {
    SE_CON1088557572(new_sec, nse);
   }
  }
 }
 after_loop: ;
 ret_val = new_sec;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SE_CONTEXT SE_CON2084169946(SE_CONTEXT self, SIG sig1) {
 SE_CONTEXT ret_val;
 SE_CONTEXT nsec;
 OUT create_self;
 OUT ret_val1;
 OUT plus_self;
 STR plus_s;
 OUT ret_val2;
 FILE1 stdout_self;
 FILE1 ret_val3;
 FILE1 r;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val4;
 FILE1 stdout_self1;
 FILE1 ret_val5;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val6;
 FILE1 r2;
 OUT create_self1;
 OUT ret_val7;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val8;
 FILE1 stdout_self3;
 FILE1 ret_val9;
 FILE1 r3;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val10;
 FILE1 r4;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val11 = BOOL_zero;
 SE_CONTEXT is_recursive_sel;
 SIG is_recursive_sig;
 BOOL ret_val12 = BOOL_zero;
 OUT create_self2;
 OUT ret_val13;
 OUT plus_self5;
 STR plus_s5;
 OUT ret_val14;
 FILE1 stdout_self5;
 FILE1 ret_val15;
 FILE1 r5;
 OUT plus_self6;
 STR plus_s6;
 OUT ret_val16;
 FILE1 stdout_self6;
 FILE1 ret_val17;
 FILE1 r6;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val18;
 FILE1 r7;
 SIG is_eq_self1;
 SIG is_eq_s1;
 BOOL ret_val19 = BOOL_zero;
 OUT create_self3;
 OUT ret_val20;
 OUT plus_self8;
 STR plus_s8;
 OUT ret_val21;
 FILE1 stdout_self8;
 FILE1 ret_val22;
 FILE1 r8;
 OUT plus_self9;
 STR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val23;
 FILE1 r9;
 OUT create_self4;
 OUT ret_val24;
 OUT plus_self10;
 STR plus_s10;
 OUT ret_val25;
 FILE1 stdout_self10;
 FILE1 ret_val26;
 FILE1 r10;
 OUT plus_self11;
 STR plus_s11;
 OUT ret_val27;
 FILE1 stdout_self11;
 FILE1 ret_val28;
 FILE1 r11;
 OUT plus_self12;
 STR plus_s12;
 FILE1 stdout_self12;
 FILE1 ret_val29;
 FILE1 r12;
 dGENERATE_AM L1;
 extern STR Lookin852250521;
 FILE1 L2;
 OB L3;
 FILE1 L5;
 OB L6;
 extern STR name9;
 FILE1 L8;
 OB L9;
 FILE1 L11;
 OB L12;
 extern STR isbuiltin;
 FILE1 L14;
 OB L15;
 BOOL L17;
 BOOL L18;
 BOOL L191_;
 dGENERATE_AM L20;
 FSETSIG L21;
 extern STR Markin664934141;
 FILE1 L22;
 OB L23;
 FILE1 L25;
 OB L26;
 extern STR iscyclic;
 FILE1 L28;
 OB L29;
 FILE1 L31;
 OB L32;
 extern STR isrecursive;
 FILE1 L34;
 OB L35;
 extern STR Unknownsignature;
 FILE1 L37;
 OB L38;
 FILE1 L40;
 OB L42;
 extern STR usingfull;
 FILE1 L44;
 OB L45;
 L1 = ATTR(SE_CONTEXT_prog,generate_am);
 (*dGENER651759373[TAG(L1)])(L1, sig1);
 if (SE_CON765348064(self)) {
  create_self = ((OUT) NULL);
  ret_val1 = create_self;
  plus_self = ret_val1;
  plus_s = ((STR) &Lookin852250521);
  stdout_self = ((FILE1) NULL);
  L3=ZALLOC(sizeof(struct FILE1_struct));
  if (L3==NULL) FATAL("Unable to allocate more memory");
  ((OB)L3)->header.tag=FILE1_tag;
  L2 = ((FILE1) L3);
  r = L2;
  
  SATTR(r,fp,stdout);
  ret_val3 = r;
  FILE_plus_STR(ret_val3, plus_s);
  ret_val2 = plus_self;
  plus_self1 = ret_val2;
  plus_s1 = ATTR(sig1,str);
  stdout_self1 = ((FILE1) NULL);
  L6=ZALLOC(sizeof(struct FILE1_struct));
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FILE1_tag;
  L5 = ((FILE1) L6);
  r1 = L5;
  
  SATTR(r1,fp,stdout);
  ret_val5 = r1;
  FILE_plus_STR(ret_val5, plus_s1);
  ret_val4 = plus_self1;
  plus_self2 = ret_val4;
  plus_s2 = ((STR) &name9);
  stdout_self2 = ((FILE1) NULL);
  L9=ZALLOC(sizeof(struct FILE1_struct));
  if (L9==NULL) FATAL("Unable to allocate more memory");
  ((OB)L9)->header.tag=FILE1_tag;
  L8 = ((FILE1) L9);
  r2 = L8;
  
  SATTR(r2,fp,stdout);
  ret_val6 = r2;
  FILE_plus_STR(ret_val6, plus_s2);
 }
 nsec = SIG_ge642497605(sig1, SE_CONTEXT_prog);
 if ((nsec==((SE_CONTEXT) NULL))) {
  nsec = SE_CON32319044(((SE_CONTEXT) NULL), SE_CONTEXT_prog, sig1);
  if (SIG_is418491101(sig1)) {
   SE_CON621544985(nsec);
   if (SE_CON765348064(self)) {
    create_self1 = ((OUT) NULL);
    ret_val7 = create_self1;
    plus_self3 = ret_val7;
    plus_s3 = ATTR(sig1,str);
    stdout_self3 = ((FILE1) NULL);
    L12=ZALLOC(sizeof(struct FILE1_struct));
    if (L12==NULL) FATAL("Unable to allocate more memory");
    ((OB)L12)->header.tag=FILE1_tag;
    L11 = ((FILE1) L12);
    r3 = L11;
    
    SATTR(r3,fp,stdout);
    ret_val9 = r3;
    FILE_plus_STR(ret_val9, plus_s3);
    ret_val8 = plus_self3;
    plus_self4 = ret_val8;
    plus_s4 = ((STR) &isbuiltin);
    stdout_self4 = ((FILE1) NULL);
    L15=ZALLOC(sizeof(struct FILE1_struct));
    if (L15==NULL) FATAL("Unable to allocate more memory");
    ((OB)L15)->header.tag=FILE1_tag;
    L14 = ((FILE1) L15);
    r4 = L14;
    
    SATTR(r4,fp,stdout);
    ret_val10 = r4;
    FILE_plus_STR(ret_val10, plus_s4);
   }
  }
  else {
   is_eq_self = sig1;
   is_eq_s = ATTR(self,rsig);
   ret_val11 = (is_eq_self==is_eq_s);
   L18 = ret_val11;
   L191_=!(L18); 
   if (L191_) {
    is_recursive_sel = self;
    is_recursive_sig = sig1;
    L20 = ATTR(SE_CONTEXT_prog,generate_am);
    L21 = (*dGENER354174443[TAG(L20)])(L20);
    ret_val12 = FSETSI722431140(L21, is_recursive_sig);
    L17 = ret_val12;
   } else {
    L17 = FALSE;
   }
   if (L17) {
    SE_CON320528666(nsec);
    SE_CON621544985(nsec);
    if (SE_CON765348064(self)) {
     create_self2 = ((OUT) NULL);
     ret_val13 = create_self2;
     plus_self5 = ret_val13;
     plus_s5 = ((STR) &Markin664934141);
     stdout_self5 = ((FILE1) NULL);
     L23=ZALLOC(sizeof(struct FILE1_struct));
     if (L23==NULL) FATAL("Unable to allocate more memory");
     ((OB)L23)->header.tag=FILE1_tag;
     L22 = ((FILE1) L23);
     r5 = L22;
     
     SATTR(r5,fp,stdout);
     ret_val15 = r5;
     FILE_plus_STR(ret_val15, plus_s5);
     ret_val14 = plus_self5;
     plus_self6 = ret_val14;
     plus_s6 = ATTR(ATTR(self,rsig),str);
     stdout_self6 = ((FILE1) NULL);
     L26=ZALLOC(sizeof(struct FILE1_struct));
     if (L26==NULL) FATAL("Unable to allocate more memory");
     ((OB)L26)->header.tag=FILE1_tag;
     L25 = ((FILE1) L26);
     r6 = L25;
     
     SATTR(r6,fp,stdout);
     ret_val17 = r6;
     FILE_plus_STR(ret_val17, plus_s6);
     ret_val16 = plus_self6;
     plus_self7 = ret_val16;
     plus_s7 = ((STR) &iscyclic);
     stdout_self7 = ((FILE1) NULL);
     L29=ZALLOC(sizeof(struct FILE1_struct));
     if (L29==NULL) FATAL("Unable to allocate more memory");
     ((OB)L29)->header.tag=FILE1_tag;
     L28 = ((FILE1) L29);
     r7 = L28;
     
     SATTR(r7,fp,stdout);
     ret_val18 = r7;
     FILE_plus_STR(ret_val18, plus_s7);
    }
   }
   else {
    is_eq_self1 = sig1;
    is_eq_s1 = ATTR(self,rsig);
    ret_val19 = (is_eq_self1==is_eq_s1);
    if (ret_val19) {
     if (SE_CON765348064(self)) {
      create_self3 = ((OUT) NULL);
      ret_val20 = create_self3;
      plus_self8 = ret_val20;
      plus_s8 = ATTR(sig1,str);
      stdout_self8 = ((FILE1) NULL);
      L32=ZALLOC(sizeof(struct FILE1_struct));
      if (L32==NULL) FATAL("Unable to allocate more memory");
      ((OB)L32)->header.tag=FILE1_tag;
      L31 = ((FILE1) L32);
      r8 = L31;
      
      SATTR(r8,fp,stdout);
      ret_val22 = r8;
      FILE_plus_STR(ret_val22, plus_s8);
      ret_val21 = plus_self8;
      plus_self9 = ret_val21;
      plus_s9 = ((STR) &isrecursive);
      stdout_self9 = ((FILE1) NULL);
      L35=ZALLOC(sizeof(struct FILE1_struct));
      if (L35==NULL) FATAL("Unable to allocate more memory");
      ((OB)L35)->header.tag=FILE1_tag;
      L34 = ((FILE1) L35);
      r9 = L34;
      
      SATTR(r9,fp,stdout);
      ret_val23 = r9;
      FILE_plus_STR(ret_val23, plus_s9);
     }
    }
    else {
     if (SE_CON765348064(self)) {
      create_self4 = ((OUT) NULL);
      ret_val24 = create_self4;
      plus_self10 = ret_val24;
      plus_s10 = ((STR) &Unknownsignature);
      stdout_self10 = ((FILE1) NULL);
      L38=ZALLOC(sizeof(struct FILE1_struct));
      if (L38==NULL) FATAL("Unable to allocate more memory");
      ((OB)L38)->header.tag=FILE1_tag;
      L37 = ((FILE1) L38);
      r10 = L37;
      
      SATTR(r10,fp,stdout);
      ret_val26 = r10;
      FILE_plus_STR(ret_val26, plus_s10);
      ret_val25 = plus_self10;
      plus_self11 = ret_val25;
      plus_s11 = ATTR(ATTR(self,rsig),str);
      stdout_self11 = ((FILE1) NULL);
      L42=ZALLOC(sizeof(struct FILE1_struct));
      if (L42==NULL) FATAL("Unable to allocate more memory");
      ((OB)L42)->header.tag=FILE1_tag;
      L40 = ((FILE1) L42);
      r11 = L40;
      
      SATTR(r11,fp,stdout);
      ret_val28 = r11;
      FILE_plus_STR(ret_val28, plus_s11);
      ret_val27 = plus_self11;
      plus_self12 = ret_val27;
      plus_s12 = ((STR) &usingfull);
      stdout_self12 = ((FILE1) NULL);
      L45=ZALLOC(sizeof(struct FILE1_struct));
      if (L45==NULL) FATAL("Unable to allocate more memory");
      ((OB)L45)->header.tag=FILE1_tag;
      L44 = ((FILE1) L45);
      r12 = L44;
      
      SATTR(r12,fp,stdout);
      ret_val29 = r12;
      FILE_plus_STR(ret_val29, plus_s12);
     }
     SE_CON320528666(nsec);
    }
   }
  }
 }
 ret_val = nsec;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SE_CONTEXT SE_CON32319044(SE_CONTEXT self, PROG prog, SIG sig1) {
 SE_CONTEXT ret_val;
 SE_CONTEXT r;
 OUT create_self;
 OUT ret_val1;
 OUT plus_self;
 STR plus_s;
 OUT ret_val2;
 FILE1 stdout_self;
 FILE1 ret_val3;
 FILE1 r1;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val4;
 FILE1 stdout_self1;
 FILE1 ret_val5;
 FILE1 r2;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val6;
 FILE1 r3;
 SE_CONTEXT L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 extern STR Create1476216634;
 FILE1 L5;
 OB L6;
 FILE1 L8;
 OB L9;
 extern STR name9;
 FILE1 L11;
 OB L12;
 L2=ZALLOC(sizeof(struct SE_CONTEXT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=SE_CONTEXT_tag;
 L1 = ((SE_CONTEXT) L2);
 r = L1;
 SE_CONTEXT_prog = prog;
 SATTR(r,rsig,sig1);
 SE_CON773743077(r);
 if ((SE_CON1373267262==((CS_OPTIONS) NULL))) {
  ret_val = ((SE_CONTEXT) NULL);
  return ret_val;
 }
 L3 = SE_CON1026918814(r);
 L41_=!(L3); 
 if (L41_) {
  ret_val = ((SE_CONTEXT) NULL);
  return ret_val;
 }
 if (SE_CON765348064(r)) {
  create_self = ((OUT) NULL);
  ret_val1 = create_self;
  plus_self = ret_val1;
  plus_s = ((STR) &Create1476216634);
  stdout_self = ((FILE1) NULL);
  L6=ZALLOC(sizeof(struct FILE1_struct));
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FILE1_tag;
  L5 = ((FILE1) L6);
  r1 = L5;
  
  SATTR(r1,fp,stdout);
  ret_val3 = r1;
  FILE_plus_STR(ret_val3, plus_s);
  ret_val2 = plus_self;
  plus_self1 = ret_val2;
  plus_s1 = ATTR(sig1,str);
  stdout_self1 = ((FILE1) NULL);
  L9=ZALLOC(sizeof(struct FILE1_struct));
  if (L9==NULL) FATAL("Unable to allocate more memory");
  ((OB)L9)->header.tag=FILE1_tag;
  L8 = ((FILE1) L9);
  r2 = L8;
  
  SATTR(r2,fp,stdout);
  ret_val5 = r2;
  FILE_plus_STR(ret_val5, plus_s1);
  ret_val4 = plus_self1;
  plus_self2 = ret_val4;
  plus_s2 = ((STR) &name9);
  stdout_self2 = ((FILE1) NULL);
  L12=ZALLOC(sizeof(struct FILE1_struct));
  if (L12==NULL) FATAL("Unable to allocate more memory");
  ((OB)L12)->header.tag=FILE1_tag;
  L11 = ((FILE1) L12);
  r3 = L11;
  
  SATTR(r3,fp,stdout);
  ret_val6 = r3;
  FILE_plus_STR(ret_val6, plus_s2);
 }
 SATTR(r,weight,1);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SE_CONTEXT SIG_ge642497605(SIG self, PROG prog) {
 SE_CONTEXT ret_val;
 dTP L1;
 L1 = ATTR(self,tp);
 if ((*dTP_is1811059018[TAG(L1)])(L1)) {
  ret_val = SE_CON1395318461(((SE_CONTEXT) NULL), prog, self);
  return ret_val;
 }
 else {
  ret_val = ATTR(self,se_context);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_ASS1066541977(AM_ASSIGN_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_ASS142463220(AM_ASSERT_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_ATT2128304053(AM_ATTACH_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_BRE475880308(AM_BREAK_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_CAS881290947(AM_CASE_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_COM1046808385(AM_COMMENT_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_EXP2073687788(AM_EXPR_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SIDE_EFFECT SIDE_E524783111(SIDE_EFFECT self, OB amattr, BOOL doeswrite1) {
 SIDE_EFFECT ret_val;
 SIDE_EFFECT r;
 SIDE_EFFECT L1;
 OB L2;
 dAM_EXPR L3;
 L2=ZALLOC(sizeof(struct SIDE_EFFECT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=SIDE_EFFECT_tag;
 L1 = ((SIDE_EFFECT) L2);
 r = L1;
 SATTR(r,doeswrite,doeswrite1);
 switch (TAG(amattr)) {
  case AM_ATTR_EXPR_tag:
   SATTR(r,tp,ATTR(((AM_ATTR_EXPR) amattr),self_tp));
   SATTR(r,name1,ATTR(((AM_ATTR_EXPR) amattr),at)); break;
  case AM_VAT319982528_tag:
   L3 = ATTR(((AM_VAT319982528) amattr),ob);
   SATTR(r,tp,(*dAM_EXPR_tprdTP[TAG(L3)])(L3));
   SATTR(r,name1,ATTR(((AM_VAT319982528) amattr),at)); break;
  case AM_SHARED_EXPR_tag:
   SATTR(r,tp,ATTR(((AM_SHARED_EXPR) amattr),class_tp));
   SATTR(r,name1,ATTR(((AM_SHARED_EXPR) amattr),name1)); break;
  case AM_LOCAL_EXPR_tag:
   if ((ATTR(((AM_LOCAL_EXPR) amattr),name1).str==(STR)0)) {
    ret_val = ((SIDE_EFFECT) NULL);
    return ret_val;
   }
   SATTR(r,tp,((dTP) NULL));
   SATTR(r,name1,ATTR(((AM_LOCAL_EXPR) amattr),name1)); break;
  case SIDE_EFFECT_tag:
   SATTR(r,tp,ATTR(((SIDE_EFFECT) amattr),tp));
   SATTR(r,name1,ATTR(((SIDE_EFFECT) amattr),name1)); break;
  default: ;
   FATAL("No applicable type in typecase\nin SIDE_EFFECT::create($OB,BOOL):SAME\n./Middle/side_effect.sa:59:14");
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SIG SIG_at1749337727(SIG self, AS_ATTR_DEF as, IDENT nm, ARRAYdTP srcparams, TP_CONTEXT con) {
 SIG ret_val;
 SIG r;
 ARRAYARG create_self;
 INT create_n = INT_zero;
 ARRAYARG ret_val1;
 ARG create_self1;
 dTP create_t;
 ARG ret_val2;
 ARG init_self;
 dTP init_t;
 dMODE init_m;
 BOOL init_same = BOOL_zero;
 ARG ret_val3;
 SIG L1;
 OB L2;
 ARRAYARG L3;
 INT L4;
 OB L5;
 INT L6;
 ARRAYARG L7;
 ARG L8;
 OB L9;
 ARG L10;
 dTP L12;
 INT L13;
 INT L14;
 BOOL L151_;
 L2=ZALLOC(sizeof(struct SIG_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=SIG_tag;
 L1 = ((SIG) L2);
 r = L1;
 SATTR(r,tp,((dTP) ATTR(con,same1)));
 SATTR(r,name1,nm);
 SATTR(r,src_tparams,srcparams);
 create_self = ((ARRAYARG) NULL);
 create_n = 1;
 L4 = create_n;
 L6=(sizeof(struct ARRAYARG_struct)-sizeof(ARG))+(L4)*sizeof(ARG);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=ARRAYARG_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((ARRAYARG) L5);
 L3->asize = L4;
 ret_val1 = L3;
 SATTR(r,args,ret_val1);
 L7 = ATTR(r,args);
 create_self1 = ((ARG) NULL);
 create_t = TP_CON1800432689(con, ATTR(as,tp));
 L9=ZALLOC(sizeof(struct ARG_struct));
 if (L9==NULL) FATAL("Unable to allocate more memory");
 ((OB)L9)->header.tag=ARG_tag;
 L8 = ((ARG) L9);
 init_self = L8;
 init_t = create_t;
 init_m = ((dMODE) IN_MOD403789248(((IN_MODE) NULL)));
 init_same = FALSE;
 SATTR(init_self,tp,init_t);
 SATTR(init_self,mode1,init_m);
 SATTR(init_self,issame,init_same);
 ret_val3 = init_self;
 ret_val2 = ret_val3;
 L10 = ret_val2;
 SARR((ARRAYARG)L7,0,(ARG)L10); 
 ;
 L12 = ATTR(r,tp);
 L13 = (*dTP_kindrINT[TAG(L12)])(L12);
 L14 = TP_KIND_val_tp;
 L151_=(L13)==(L14); 
 if (L151_) {
  SATTR(r,ret,ATTR(r,tp));
 }
 ret_val = SIG_ma1442297600(r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SIG SIG_co2044018076(SIG self, AS_CONST_DEF as, IDENT nm, ARRAYdTP srcparams, TP_CONTEXT con) {
 SIG ret_val;
 SIG r;
 SIG L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 L2=ZALLOC(sizeof(struct SIG_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=SIG_tag;
 L1 = ((SIG) L2);
 r = L1;
 SATTR(r,tp,((dTP) ATTR(con,same1)));
 SATTR(r,name1,nm);
 SATTR(r,src_tparams,srcparams);
 L3 = (ATTR(as,tp)==((AS_TYPE_SPEC) NULL));
 L41_=!(L3); 
 if (L41_) {
  SATTR(r,ret,TP_CON1800432689(con, ATTR(as,tp)));
 }
 else {
  SATTR(r,ret,((dTP) TP_BUILTIN_int));
 }
 ret_val = SIG_ma1442297600(r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SIG SIG_ma1442297600(SIG self) {
 SIG ret_val;
 SIG u;
 BOOL L1;
 BOOL L2;
 BOOL L31_;
 if ((ATTR(self,str)==((STR) NULL))) {
  SIG_compute_str(self);
 }
 u = FMAPST879183273(SIG_sigs, ATTR(self,str));
 if ((u==((SIG) NULL))) {
  SIG_sigs = FMAPST451382576(SIG_sigs, ATTR(self,str), self);
  ret_val = self;
  return ret_val;
 }
 else {
  if (SIG_is418491101(self)) {
   L2 = ATTR(u,got_builtin_info);
   L31_=!(L2); 
   L1 = L31_;
  } else {
   L1 = FALSE;
  }
  if (L1) {
   SATTR(u,builtin_info,ATTR(self,builtin_info));
   SATTR(u,got_builtin_info,TRUE);
  }
 }
 ret_val = u;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SIG SIG_ro1276623596(SIG self, AS_ROUT_DEF as, IDENT nm, ARRAYdTP srcparams, TP_CONTEXT con) {
 SIG ret_val;
 SIG r;
 INT na = INT_zero;
 AS_ARG_DEC ad;
 BOOL is_samearg = BOOL_zero;
 ARRAYARG L11;
 BOOL dohot = BOOL_zero;
 AS_ARG_DEC tae;
 AS_ARG_DEC ad1;
 ARRAYBOOL L21;
 SIG prog_self;
 PROG ret_val1;
 SIG prog_self1;
 PROG ret_val2;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val4;
 ARRAYARG create_self;
 INT create_n = INT_zero;
 ARRAYARG ret_val5;
 SIG prog_self2;
 PROG ret_val6;
 SIG prog_self3;
 PROG ret_val7;
 ARG create_self1;
 dTP create_t;
 dMODE create_m;
 BOOL create_same = BOOL_zero;
 ARG ret_val8;
 ARG init_self;
 dTP init_t;
 dMODE init_m;
 BOOL init_same = BOOL_zero;
 ARG ret_val9;
 ARRAYBOOL create_self2;
 INT create_n1 = INT_zero;
 ARRAYBOOL ret_val10;
 SIG L3;
 OB L4;
 dTP L5;
 PROG L6;
 dTP L7;
 extern STR builtin2;
 extern STR notfoundinCONFIG;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 ARRAYARG L12;
 INT L13;
 OB L14;
 INT L15;
 INT L17;
 INT L18;
 BOOL L191_;
 BOOL L20;
 BOOL L22;
 dMODE L23;
 dMODE L24;
 BOOL L25;
 BOOL L261_;
 dTP L27;
 dTP L28;
 extern STR Inabst1442389651;
 INT L161_br;
INT i_L161_=0;
 ARG L29;
 OB L30;
 ARG L31;
 ARRAYBOOL L32;
 INT L33;
 OB L34;
 INT L35;
 INT L361_br;
INT i_L361_=0;
 BOOL L37;
 BOOL L38;
 BOOL L39;
 BOOL L401_;
 INT L41;
 INT L42;
 BOOL L431_;
 L4=ZALLOC(sizeof(struct SIG_struct));
 if (L4==NULL) FATAL("Unable to allocate more memory");
 ((OB)L4)->header.tag=SIG_tag;
 L3 = ((SIG) L4);
 r = L3;
 SATTR(r,tp,((dTP) ATTR(con,same1)));
 SATTR(r,src_tparams,srcparams);
 SATTR(r,name1,nm);
 na = AS_ARG1759480338(ATTR(as,args_dec));
 if (ATTR(as,is_builtin)) {
  prog_self = r;
  L5 = ATTR(prog_self,tp);
  ret_val1 = (*dTP_progrPROG[TAG(L5)])(L5);
  L6=ret_val1;
  SATTR(r,builtin_info,CONFIG525418741(ATTR(L6,config), ATTR(as,builtin_name).str));
  if ((ATTR(r,builtin_info)==((CONFIG_ROUT) NULL))) {
   prog_self1 = r;
   L7 = ATTR(prog_self1,tp);
   ret_val2 = (*dTP_progrPROG[TAG(L7)])(L7);
   plus_self = ((STR) &builtin2);
   plus_sarg = ATTR(as,builtin_name).str;
   ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val3;
   plus_sarg1 = ((STR) &notfoundinCONFIG);
   ret_val4 = STR_ap2004550586(plus_self1, plus_sarg1);
   PROG_err_STR(ret_val2, ret_val4);
  }
 }
 L81_=(na)==(0); 
 L9 = L81_;
 L101_=!(L9); 
 if (L101_) {
  create_self = ((ARRAYARG) NULL);
  create_n = na;
  L13 = create_n;
  L15=(sizeof(struct ARRAYARG_struct)-sizeof(ARG))+(L13)*sizeof(ARG);
  L14=ZALLOC_BIG(L15);
  if (L14==NULL) FATAL("Unable to allocate more memory");
  ((OB)L14)->header.tag=ARRAYARG_tag;
#ifdef DESTROY_CHK

    ((OB)L14)->header.destroyed=0;
#endif

  L12 = ((ARRAYARG) L14);
  L12->asize = L13;
  ret_val5 = L12;
  SATTR(r,args,ret_val5);
  ad = ATTR(as,args_dec);
  {
   BOOL f_L161_ = TRUE;
   while (1) {
    if ((ad==((AS_ARG_DEC) NULL))) {
     goto after_loop;
    }
    L17 = ATTR(ATTR(ad,tp),kind);
    L18 = AS_TYP1853679690;
    L191_=(L17)==(L18); 
    is_samearg = L191_;
    if (TP_CLA1568240525(ATTR(con,same1))) {
     L22 = is_samearg;
    } else {
     L22 = FALSE;
    }
    if (L22) {
     L23 = MODE_c998271775(((MODE) NULL), ATTR(ad,mode1));
     L24 = L23;
     L25 = (*dMODE_814247358[TAG(L24)])(L24, ((dMODE) MODES_out_mode));
     L261_=!(L25); 
     L20 = L261_;
    } else {
     L20 = FALSE;
    }
    if (L20) {
     prog_self2 = r;
     L27 = ATTR(prog_self2,tp);
     ret_val6 = (*dTP_progrPROG[TAG(L27)])(L27);
     PROG_e176405615(ret_val6, ((dPROG_ERR) ad));
     prog_self3 = r;
     L28 = ATTR(prog_self3,tp);
     ret_val7 = (*dTP_progrPROG[TAG(L28)])(L28);
     PROG_err_STR(ret_val7, ((STR) &Inabst1442389651));
    }
    if (f_L161_) {
     f_L161_ = FALSE;
     L11 = ATTR(r,args);
     L161_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
     i_L161_=0;
    }
    if(i_L161_>=L161_br)  goto after_loop; 
    create_self1 = ((ARG) NULL);
    create_t = TP_CON1800432689(con, ATTR(ad,tp));
    create_m = MODE_c998271775(((MODE) NULL), ATTR(ad,mode1));
    create_same = is_samearg;
    L30=ZALLOC(sizeof(struct ARG_struct));
    if (L30==NULL) FATAL("Unable to allocate more memory");
    ((OB)L30)->header.tag=ARG_tag;
    L29 = ((ARG) L30);
    init_self = L29;
    init_t = create_t;
    init_m = create_m;
    init_same = create_same;
    SATTR(init_self,tp,init_t);
    SATTR(init_self,mode1,init_m);
    SATTR(init_self,issame,init_same);
    ret_val9 = init_self;
    ret_val8 = ret_val9;
    L31 = ret_val8;
    SARR((ARRAYARG)L11,i_L161_,(ARG)L31); i_L161_++;
    ;
    ad = ATTR(ad,next);
   }
  }
  after_loop: ;
 }
 tae = ATTR(as,args_dec);
 while (1) {
  if ((tae==((AS_ARG_DEC) NULL))) {
   goto after_loop1;
  }
  if (ATTR(tae,is_hot)) {
   dohot = TRUE;
  }
  tae = ATTR(tae,next);
 }
 after_loop1: ;
 if (dohot) {
  create_self2 = ((ARRAYBOOL) NULL);
  create_n1 = na;
  L33 = create_n1;
  L35=(sizeof(struct ARRAYBOOL_struct)+1-sizeof(BOOL))+(L33)*sizeof(BOOL);
  L34=ZALLOC_LEAF_BIG(L35);
  if (L34==NULL) FATAL("Unable to allocate more memory");
  memset(L34,0,L35);
  ((OB)L34)->header.tag=ARRAYBOOL_tag;
#ifdef DESTROY_CHK

    ((OB)L34)->header.destroyed=0;
#endif

  L32 = ((ARRAYBOOL) L34);
  L32->asize = L33;
  ret_val10 = L32;
  SATTR(r,hot,ret_val10);
  ad1 = ATTR(as,args_dec);
  {
   BOOL f_L361_ = TRUE;
   L21 = ATTRs(r,hot,((ARRAYBOOL) NULL));
   L361_br=L21==NULL?0:ASIZE((ARRAYBOOL)L21); 
   i_L361_=0;
   while (1) {
    if ((ad1==((AS_ARG_DEC) NULL))) {
     goto after_loop2;
    }
    if(i_L361_>=L361_br)  goto after_loop2; 
    L37 = ATTR(ad1,is_hot);
    SARR((ARRAYBOOL)L21,i_L361_,L37); i_L361_++;
    ;
    ad1 = ATTR(ad1,next);
   }
  }
  after_loop2: ;
 }
 SATTR(r,ret,TP_CON1800432689(con, ATTR(as,ret_dec)));
 L39 = (ATTR(as,ret_dec)==((AS_TYPE_SPEC) NULL));
 L401_=!(L39); 
 if (L401_) {
  L41 = ATTR(ATTR(as,ret_dec),kind);
  L42 = AS_TYP1853679690;
  L431_=(L41)==(L42); 
  L38 = L431_;
 } else {
  L38 = FALSE;
 }
 SATTR(r,returns_same,L38);
 SATTR(r,got_builtin_info,TRUE);
 ret_val = SIG_ma1442297600(r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SIG SIG_sh1677498347(SIG self, AS_SHARED_DEF as, IDENT nm, ARRAYdTP srcparams, TP_CONTEXT con) {
 SIG ret_val;
 SIG r;
 ARRAYARG create_self;
 INT create_n = INT_zero;
 ARRAYARG ret_val1;
 ARG create_self1;
 dTP create_t;
 ARG ret_val2;
 ARG init_self;
 dTP init_t;
 dMODE init_m;
 BOOL init_same = BOOL_zero;
 ARG ret_val3;
 SIG L1;
 OB L2;
 ARRAYARG L3;
 INT L4;
 OB L5;
 INT L6;
 ARRAYARG L7;
 ARG L8;
 OB L9;
 ARG L10;
 L2=ZALLOC(sizeof(struct SIG_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=SIG_tag;
 L1 = ((SIG) L2);
 r = L1;
 SATTR(r,tp,((dTP) ATTR(con,same1)));
 SATTR(r,name1,nm);
 SATTR(r,src_tparams,srcparams);
 create_self = ((ARRAYARG) NULL);
 create_n = 1;
 L4 = create_n;
 L6=(sizeof(struct ARRAYARG_struct)-sizeof(ARG))+(L4)*sizeof(ARG);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=ARRAYARG_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((ARRAYARG) L5);
 L3->asize = L4;
 ret_val1 = L3;
 SATTR(r,args,ret_val1);
 L7 = ATTR(r,args);
 create_self1 = ((ARG) NULL);
 create_t = TP_CON1800432689(con, ATTR(as,tp));
 L9=ZALLOC(sizeof(struct ARG_struct));
 if (L9==NULL) FATAL("Unable to allocate more memory");
 ((OB)L9)->header.tag=ARG_tag;
 L8 = ((ARG) L9);
 init_self = L8;
 init_t = create_t;
 init_m = ((dMODE) IN_MOD403789248(((IN_MODE) NULL)));
 init_same = FALSE;
 SATTR(init_self,tp,init_t);
 SATTR(init_self,mode1,init_m);
 SATTR(init_self,issame,init_same);
 ret_val3 = init_self;
 ret_val2 = ret_val3;
 L10 = ret_val2;
 SARR((ARRAYARG)L7,0,(ARG)L10); 
 ;
 ret_val = SIG_ma1442297600(r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SIG SIG_wi1810596376(SIG self, dTP t) {
 SIG ret_val;
 SIG r;
 ARRAYARG L11;
 ARRAYARG L21;
 INT L31 = INT_zero;
 SIG L4;
 OB L5;
 INT L61_br;
 ARG aL61_;
 INT L71_br;
 ARG L8;
 ARG L9;
 ARG L10;
 INT L121_;
 L5=ZALLOC(sizeof(struct SIG_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=SIG_tag;
 L4 = ((SIG) L5);
 r = L4;
 SATTR(r,tp,t);
 SATTR(r,name1,ATTR(self,name1));
 SATTR(r,args,ATTR(self,args));
 SATTR(r,hot,ATTR(self,hot));
 {
  BOOL f_L61_ = TRUE;
  BOOL f_L71_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = ATTR(self,args);
  L21 = ATTRs(r,args,((ARRAYARG) NULL));
  L61_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
  L71_br=L21==NULL?0:ASIZE((ARRAYARG)L21); 
  while (1) {
   if(L31>=L71_br)  goto after_loop; 
   if(L31>=L61_br)  goto after_loop; 
   aL61_=ARR((ARRAYARG)L11,L31); 
   L9 = aL61_;
   L10 = ARG_cr1806638063(L9, t);
   SARR((ARRAYARG)L21,L31,(ARG)L10); 
   ;
   L121_=INTPLUS(L31,1); 
   L31 = L121_;
  }
 }
 after_loop: ;
 if (ATTR(self,returns_same)) {
  SATTR(r,returns_same,TRUE);
  SATTR(r,ret,t);
 }
 else {
  SATTR(r,ret,ATTR(self,ret));
 }
 ret_val = SIG_ma1442297600(r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARG_strrSTR(ARG self) {
 STR ret_val;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 BOOL L1;
 dMODE L2;
 dMODE L3;
 extern STR SAME;
 dMODE L4;
 dTP L5;
 if (ATTR(self,issame)) {
  L2 = ATTR(self,mode1);
  L1 = (*dMODE_814247358[TAG(L2)])(L2, ((dMODE) MODES_out_mode));
 } else {
  L1 = FALSE;
 }
 if (L1) {
  L3 = ATTR(self,mode1);
  plus_self = (*dMODE_strrSTR[TAG(L3)])(L3);
  plus_sarg = ((STR) &SAME);
  ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
  ret_val = ret_val1;
  return ret_val;
 }
 L4 = ATTR(self,mode1);
 plus_self1 = (*dMODE_strrSTR[TAG(L4)])(L4);
 L5 = ATTR(self,tp);
 plus_sarg1 = (*dTP_strrSTR[TAG(L5)])(L5);
 ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
 ret_val = ret_val2;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR NULL_S1933820575(NULL_STAT self) {
 STR ret_val;
 extern STR name1;
 ret_val = ((STR) &name1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR SE_CON1246526640(SE_CONTEXT self, STR r, SIG sig1) {
 STR ret_val;
 STR res;
 INT i = INT_zero;
 CHAR L11 = CHAR_zero;
 CHAR L21 = CHAR_zero;
 INT l = INT_zero;
 INT l1 = INT_zero;
 INT l2 = INT_zero;
 INT l3 = INT_zero;
 INT l4 = INT_zero;
 INT l5 = INT_zero;
 INT l6 = INT_zero;
 INT l7 = INT_zero;
 INT l8 = INT_zero;
 INT l9 = INT_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val3;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val4;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val5;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val6;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val7;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val8;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val9;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val10;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val11;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val12;
 STR plus_self12;
 CHAR plus_arg = CHAR_zero;
 STR ret_val13;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val14;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val15;
 STR r1;
 PROG barf_self;
 STR barf_msg;
 PROG barf_at_self;
 STR barf_at_msg;
 dPROG_ERR barf_at_at;
 STR plus_self13;
 STR plus_sarg12;
 STR ret_val16;
 UNIX exit_self;
 INT exit_code = INT_zero;
 STR plus_self14;
 STR plus_sarg13;
 STR ret_val17;
 STR plus_self15;
 STR plus_sarg14;
 STR ret_val18;
 STR plus_self16;
 STR plus_sarg15;
 STR ret_val19;
 PROG barf_self1;
 STR barf_msg1;
 PROG barf_at_self1;
 STR barf_at_msg1;
 dPROG_ERR barf_at_at1;
 STR plus_self17;
 STR plus_sarg16;
 STR ret_val20;
 UNIX exit_self1;
 INT exit_code1 = INT_zero;
 STR plus_self18;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val21;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val22;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val23;
 STR r2;
 extern STR name1;
 INT L3;
 BOOL L41_;
 CHAR L51_;
 INT L61_;
 CHAR L71_;
 dTP L8;
 CHAR L91_;
 CHAR L10;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 INT L161_;
 ARRAYARG L17;
 ARG L181_;
 ARG L19;
 dTP L20;
 CHAR L221_;
 CHAR L23;
 INT L241_;
 INT L25;
 INT L261_;
 INT L27;
 INT L281_;
 ARRAYARG L29;
 ARG L301_;
 ARG L31;
 dTP L32;
 CHAR L331_;
 CHAR L34;
 INT L351_;
 INT L36;
 INT L371_;
 INT L38;
 INT L391_;
 ARRAYARG L40;
 ARG L421_;
 ARG L43;
 dTP L44;
 CHAR L451_;
 CHAR L46;
 INT L471_;
 INT L48;
 INT L491_;
 INT L50;
 INT L521_;
 ARRAYARG L53;
 ARG L541_;
 ARG L55;
 dTP L56;
 CHAR L571_;
 CHAR L58;
 INT L591_;
 INT L60;
 INT L621_;
 INT L63;
 INT L641_;
 ARRAYARG L65;
 ARG L661_;
 ARG L67;
 dTP L68;
 CHAR L691_;
 CHAR L70;
 INT L721_;
 INT L73;
 INT L741_;
 INT L75;
 INT L761_;
 ARRAYARG L77;
 ARG L781_;
 ARG L79;
 dTP L80;
 CHAR L811_;
 CHAR L82;
 INT L831_;
 INT L84;
 INT L851_;
 INT L86;
 INT L871_;
 ARRAYARG L88;
 ARG L891_;
 ARG L90;
 dTP L92;
 CHAR L931_;
 CHAR L94;
 INT L951_;
 INT L96;
 INT L971_;
 INT L98;
 INT L991_;
 ARRAYARG L100;
 ARG L1011_;
 ARG L102;
 dTP L103;
 CHAR L1041_;
 CHAR L105;
 INT L1061_;
 INT L107;
 INT L1081_;
 INT L109;
 INT L1101_;
 ARRAYARG L111;
 ARG L1121_;
 ARG L113;
 dTP L114;
 dTP L115;
 CHAR L1161_;
 CHAR L117;
 INT L1181_;
 INT L119;
 INT L1201_;
 INT L122;
 INT L1231_;
 BOOL L124;
 BOOL L1251_;
 ARRAYdTP L126;
 INT L1271_;
 INT L128;
 BOOL L1291_;
 BOOL L130;
 BOOL L1311_;
 extern STR Pnbounderrorin;
 STR L132;
 INT L133;
 OB L134;
 INT L135;
 extern STR Intern28965746;
 ARRAYdTP L137;
 dTP L1381_;
 dTP L139;
 extern STR unknow2080262473;
 extern STR readswrites;
 extern STR Intern28965746;
 CHAR L1401_;
 STR L142;
 INT L143;
 OB L144;
 INT L145;
 INT L1471_;
 res = ((STR) &name1);
 i = 0;
 while (1) {
  L3 = STR_sizerINT(r);
  L41_=(i)<(L3); 
  if (L41_) {
  }
  else {
   goto after_loop;
  }
  L51_=ARR((STR)r,i); 
  L11 = L51_;
  switch (L11) {
   case '@': 
    L61_=INTPLUS(i,1); 
    i = L61_;
    L71_=ARR((STR)r,i); 
    L21 = L71_;
    switch (L21) {
     case '0': 
      plus_self = res;
      L8 = ATTR(sig1,tp);
      plus_sarg = (*dTP_strrSTR[TAG(L8)])(L8);
      ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
      res = ret_val1;
      break;
     case '1': 
      L91_=ARR((STR)r,i); 
      L10 = L91_;
      L121_=(INT)((unsigned char)L10); 
      L13 = L121_;
      L141_=(INT)((unsigned char)'1'); 
      L15 = L141_;
      L161_=INTMINUS(L13,L15); 
      l = L161_;
      plus_self1 = res;
      L17 = ATTR(sig1,args);
      L181_=(ARG)ARR((ARRAYARG)L17,l); 
      L19=L181_;
      L20 = ATTR(L19,tp);
      plus_sarg1 = (*dTP_strrSTR[TAG(L20)])(L20);
      ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
      res = ret_val2;
      break;
     case '2': 
      L221_=ARR((STR)r,i); 
      L23 = L221_;
      L241_=(INT)((unsigned char)L23); 
      L25 = L241_;
      L261_=(INT)((unsigned char)'1'); 
      L27 = L261_;
      L281_=INTMINUS(L25,L27); 
      l1 = L281_;
      plus_self2 = res;
      L29 = ATTR(sig1,args);
      L301_=(ARG)ARR((ARRAYARG)L29,l1); 
      L31=L301_;
      L32 = ATTR(L31,tp);
      plus_sarg2 = (*dTP_strrSTR[TAG(L32)])(L32);
      ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
      res = ret_val3;
      break;
     case '3': 
      L331_=ARR((STR)r,i); 
      L34 = L331_;
      L351_=(INT)((unsigned char)L34); 
      L36 = L351_;
      L371_=(INT)((unsigned char)'1'); 
      L38 = L371_;
      L391_=INTMINUS(L36,L38); 
      l2 = L391_;
      plus_self3 = res;
      L40 = ATTR(sig1,args);
      L421_=(ARG)ARR((ARRAYARG)L40,l2); 
      L43=L421_;
      L44 = ATTR(L43,tp);
      plus_sarg3 = (*dTP_strrSTR[TAG(L44)])(L44);
      ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
      res = ret_val4;
      break;
     case '4': 
      L451_=ARR((STR)r,i); 
      L46 = L451_;
      L471_=(INT)((unsigned char)L46); 
      L48 = L471_;
      L491_=(INT)((unsigned char)'1'); 
      L50 = L491_;
      L521_=INTMINUS(L48,L50); 
      l3 = L521_;
      plus_self4 = res;
      L53 = ATTR(sig1,args);
      L541_=(ARG)ARR((ARRAYARG)L53,l3); 
      L55=L541_;
      L56 = ATTR(L55,tp);
      plus_sarg4 = (*dTP_strrSTR[TAG(L56)])(L56);
      ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
      res = ret_val5;
      break;
     case '5': 
      L571_=ARR((STR)r,i); 
      L58 = L571_;
      L591_=(INT)((unsigned char)L58); 
      L60 = L591_;
      L621_=(INT)((unsigned char)'1'); 
      L63 = L621_;
      L641_=INTMINUS(L60,L63); 
      l4 = L641_;
      plus_self5 = res;
      L65 = ATTR(sig1,args);
      L661_=(ARG)ARR((ARRAYARG)L65,l4); 
      L67=L661_;
      L68 = ATTR(L67,tp);
      plus_sarg5 = (*dTP_strrSTR[TAG(L68)])(L68);
      ret_val6 = STR_ap2004550586(plus_self5, plus_sarg5);
      res = ret_val6;
      break;
     case '6': 
      L691_=ARR((STR)r,i); 
      L70 = L691_;
      L721_=(INT)((unsigned char)L70); 
      L73 = L721_;
      L741_=(INT)((unsigned char)'1'); 
      L75 = L741_;
      L761_=INTMINUS(L73,L75); 
      l5 = L761_;
      plus_self6 = res;
      L77 = ATTR(sig1,args);
      L781_=(ARG)ARR((ARRAYARG)L77,l5); 
      L79=L781_;
      L80 = ATTR(L79,tp);
      plus_sarg6 = (*dTP_strrSTR[TAG(L80)])(L80);
      ret_val7 = STR_ap2004550586(plus_self6, plus_sarg6);
      res = ret_val7;
      break;
     case '7': 
      L811_=ARR((STR)r,i); 
      L82 = L811_;
      L831_=(INT)((unsigned char)L82); 
      L84 = L831_;
      L851_=(INT)((unsigned char)'1'); 
      L86 = L851_;
      L871_=INTMINUS(L84,L86); 
      l6 = L871_;
      plus_self7 = res;
      L88 = ATTR(sig1,args);
      L891_=(ARG)ARR((ARRAYARG)L88,l6); 
      L90=L891_;
      L92 = ATTR(L90,tp);
      plus_sarg7 = (*dTP_strrSTR[TAG(L92)])(L92);
      ret_val8 = STR_ap2004550586(plus_self7, plus_sarg7);
      res = ret_val8;
      break;
     case '8': 
      L931_=ARR((STR)r,i); 
      L94 = L931_;
      L951_=(INT)((unsigned char)L94); 
      L96 = L951_;
      L971_=(INT)((unsigned char)'1'); 
      L98 = L971_;
      L991_=INTMINUS(L96,L98); 
      l7 = L991_;
      plus_self8 = res;
      L100 = ATTR(sig1,args);
      L1011_=(ARG)ARR((ARRAYARG)L100,l7); 
      L102=L1011_;
      L103 = ATTR(L102,tp);
      plus_sarg8 = (*dTP_strrSTR[TAG(L103)])(L103);
      ret_val9 = STR_ap2004550586(plus_self8, plus_sarg8);
      res = ret_val9;
      break;
     case '9': 
      L1041_=ARR((STR)r,i); 
      L105 = L1041_;
      L1061_=(INT)((unsigned char)L105); 
      L107 = L1061_;
      L1081_=(INT)((unsigned char)'1'); 
      L109 = L1081_;
      L1101_=INTMINUS(L107,L109); 
      l8 = L1101_;
      plus_self9 = res;
      L111 = ATTR(sig1,args);
      L1121_=(ARG)ARR((ARRAYARG)L111,l8); 
      L113=L1121_;
      L114 = ATTR(L113,tp);
      plus_sarg9 = (*dTP_strrSTR[TAG(L114)])(L114);
      ret_val10 = STR_ap2004550586(plus_self9, plus_sarg9);
      res = ret_val10;
      break;
     case 'r': 
      plus_self10 = res;
      L115 = ATTR(sig1,ret);
      plus_sarg10 = (*dTP_strrSTR[TAG(L115)])(L115);
      ret_val11 = STR_ap2004550586(plus_self10, plus_sarg10);
      res = ret_val11;
      break;
     case 'P': 
      L1161_=ARR((STR)r,i); 
      L117 = L1161_;
      L1181_=(INT)((unsigned char)L117); 
      L119 = L1181_;
      L1201_=(INT)((unsigned char)'0'); 
      L122 = L1201_;
      L1231_=INTMINUS(L119,L122); 
      l9 = L1231_;
      L1251_=(l9)<(0); 
      if (L1251_) {
       L124 = TRUE;
      } else {
       L126 = ATTR(sig1,src_tparams);
       L1271_=(L126)==NULL?0:ASIZE((ARRAYdTP)L126); 
       L128 = L1271_;
       L1291_=(l9)<(L128); 
       L130 = L1291_;
       L1311_=!(L130); 
       L124 = L1311_;
      }
      if (L124) {
       barf_self = SE_CONTEXT_prog;
       plus_self11 = ((STR) &Pnbounderrorin);
       plus_sarg11 = r;
       ret_val12 = STR_ap2004550586(plus_self11, plus_sarg11);
       plus_self12 = ret_val12;
       plus_arg = '\'';
       str_self = plus_arg;
       create_self = ((STR) NULL);
       create_c = str_self;
       L133 = 1;
       L135=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L133)*sizeof(CHAR);
       L134=ZALLOC_LEAF_BIG(L135);
       if (L134==NULL) FATAL("Unable to allocate more memory");
       memset(L134,0,L135);
       ((OB)L134)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L134)->header.destroyed=0;
#endif

       L132 = ((STR) L134);
       L132->asize = L133;
       r1 = L132;
       SARR((STR)r1,0,create_c); 
       ;
       ret_val15 = r1;
       ret_val14 = ret_val15;
       s = ret_val14;
       ret_val13 = STR_ap1077157958(plus_self12, s, TRUE);
       barf_msg = ret_val13;
       barf_at_self = barf_self;
       barf_at_msg = barf_msg;
       barf_at_at = ((dPROG_ERR) NULL);
       PROG_e176405615(barf_at_self, barf_at_at);
       plus_self13 = ((STR) &Intern28965746);
       plus_sarg12 = barf_at_msg;
       ret_val16 = STR_ap2004550586(plus_self13, plus_sarg12);
       PROG_err_STR(barf_at_self, ret_val16);
       exit_self = ((UNIX) NULL);
       exit_code = 1;
       exit(exit_code);
      }
      plus_self14 = res;
      L137 = ATTR(sig1,src_tparams);
      L1381_=(dTP)ARR((ARRAYdTP)L137,l9); 
      L139 = L1381_;
      plus_sarg13 = (*dTP_strrSTR[TAG(L139)])(L139);
      ret_val17 = STR_ap2004550586(plus_self14, plus_sarg13);
      res = ret_val17;
      break;
     default: ;
      barf_self1 = SE_CONTEXT_prog;
      plus_self15 = ((STR) &unknow2080262473);
      plus_sarg14 = ATTR(sig1,str);
      ret_val18 = STR_ap2004550586(plus_self15, plus_sarg14);
      plus_self16 = ret_val18;
      plus_sarg15 = ((STR) &readswrites);
      ret_val19 = STR_ap2004550586(plus_self16, plus_sarg15);
      barf_msg1 = ret_val19;
      barf_at_self1 = barf_self1;
      barf_at_msg1 = barf_msg1;
      barf_at_at1 = ((dPROG_ERR) NULL);
      PROG_e176405615(barf_at_self1, barf_at_at1);
      plus_self17 = ((STR) &Intern28965746);
      plus_sarg16 = barf_at_msg1;
      ret_val20 = STR_ap2004550586(plus_self17, plus_sarg16);
      PROG_err_STR(barf_at_self1, ret_val20);
      exit_self1 = ((UNIX) NULL);
      exit_code1 = 1;
      exit(exit_code1);
    }
    break;
   case ' ': 
    break;
   default: ;
    plus_self18 = res;
    L1401_=ARR((STR)r,i); 
    plus_arg1 = L1401_;
    str_self1 = plus_arg1;
    create_self1 = ((STR) NULL);
    create_c1 = str_self1;
    L143 = 1;
    L145=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L143)*sizeof(CHAR);
    L144=ZALLOC_LEAF_BIG(L145);
    if (L144==NULL) FATAL("Unable to allocate more memory");
    memset(L144,0,L145);
    ((OB)L144)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L144)->header.destroyed=0;
#endif

    L142 = ((STR) L144);
    L142->asize = L143;
    r2 = L142;
    SARR((STR)r2,0,create_c1); 
    ;
    ret_val23 = r2;
    ret_val22 = ret_val23;
    s1 = ret_val22;
    ret_val21 = STR_ap1077157958(plus_self18, s1, TRUE);
    res = ret_val21;
  }
  L1471_=INTPLUS(i,1); 
  i = L1471_;
 }
 after_loop: ;
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR SIDE_E1431483993(SIDE_EFFECT self) {
 STR ret_val;
 STR ret;
 SIDE_EFFECT is_local_self;
 BOOL ret_val1 = BOOL_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 SIDE_EFFECT is_array_self;
 BOOL ret_val3 = BOOL_zero;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val4;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val5;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val6;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val7;
 extern STR name1;
 BOOL L1;
 BOOL L21_;
 dTP L3;
 extern STR name57;
 extern STR name21;
 extern STR name58;
 ret = ((STR) &name1);
 is_local_self = self;
 ret_val1 = (ATTR(is_local_self,tp)==((dTP) NULL));
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  plus_self = ret;
  L3 = ATTR(self,tp);
  plus_sarg = (*dTP_strrSTR[TAG(L3)])(L3);
  ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
  ret = ret_val2;
 }
 is_array_self = self;
 ret_val3 = (ATTR(is_array_self,name1).str==(STR)0);
 if (ret_val3) {
  plus_self1 = ret;
  plus_sarg1 = ((STR) &name57);
  ret_val4 = STR_ap2004550586(plus_self1, plus_sarg1);
  ret = ret_val4;
 }
 else {
  plus_self2 = ret;
  plus_sarg2 = ((STR) &name21);
  ret_val5 = STR_ap2004550586(plus_self2, plus_sarg2);
  plus_self3 = ret_val5;
  plus_sarg3 = ATTR(self,name1).str;
  ret_val6 = STR_ap2004550586(plus_self3, plus_sarg3);
  ret = ret_val6;
 }
 if (ATTR(self,doeswrite)) {
  plus_self4 = ret;
  plus_sarg4 = ((STR) &name58);
  ret_val7 = STR_ap2004550586(plus_self4, plus_sarg4);
  ret = ret_val7;
 }
 ret_val = ret;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR SIG_strrSTR(SIG self) {
 return ATTR(self,str);
}


#undef IS_ITER
#define IS_ITER 0

STR STAT_strrSTR(STAT1 self) {
 STR ret_val;
 ARRAYSTR a;
 FMAPSTRINT L11;
 FMAPSTRINT L21;
 ARRAYSTR L31;
 STR s;
 ARRAYSTR L41;
 ARRAYSTR create_self;
 INT create_n = INT_zero;
 ARRAYSTR ret_val1;
 FMAPSTRINT indb_self;
 STR ret_val2;
 FMAPSTRINT L51;
 BOOL indb_if_first = BOOL_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 FMAPSTRINT eltb_self;
 INT ret_val4 = INT_zero;
 FMAPSTRINT L61;
 BOOL eltb_if_first = BOOL_zero;
 STR plus_self1;
 INT plus_arg = INT_zero;
 STR ret_val5;
 STR s1;
 INT str_self = INT_zero;
 STR ret_val6;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val7;
 FSTR str_self1;
 STR ret_val8;
 INT L71 = INT_zero;
 ARRAYSTR sort_self;
 INT L81 = INT_zero;
 STR plus_self2;
 STR plus_sarg1;
 STR ret_val9;
 STR plus_self3;
 STR plus_sarg2;
 STR ret_val10;
 ARRAYSTR L9;
 INT L10;
 OB L12;
 INT L13;
 INT L141_br;
 STR L15;
 extern STR name17;
 INT L16;
 BOOL L17;
 BOOL L181_;
 STR L19;
 INT L201_;
 BOOL L22;
 BOOL L231_;
 INT L241_;
 INT L25;
 INT L261_;
 extern STR name1;
 INT L27;
 BOOL L281_;
 extern STR Statistics;
 INT L291_br;
 STR aL291_;
 STR L30;
 extern STR name9;
 INT L321_;
 create_self = ((ARRAYSTR) NULL);
 create_n = FMAPST868850642(ATTR(self,map));
 L10 = create_n;
 L13=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L10)*sizeof(STR);
 L12=ZALLOC_BIG(L13);
 if (L12==NULL) FATAL("Unable to allocate more memory");
 ((OB)L12)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L12)->header.destroyed=0;
#endif

 L9 = ((ARRAYSTR) L12);
 L9->asize = L10;
 ret_val1 = L9;
 a = ret_val1;
 {
  BOOL f_L141_ = TRUE;
  struct FMAPST961226608_frame_struct other1_0;
FMAPST961226608_frame noname1 = &other1_0;
  struct FMAPST1937251125_frame_struct other1_1;
FMAPST1937251125_frame noname2 = &other1_1;
  /* Initialization of inlined iter FMAP{STR,INT}::ind!:STR */
  L11 = ATTR(self,map);
  indb_self = L11;
  L51 = indb_self;
  /* Initialization of inlined iter FMAP{STR,INT}::elt!:INT */
  L21 = ATTRs(self,map,((FMAPSTRINT) NULL));
  eltb_self = L21;
  L61 = eltb_self;
  /* loop index variable */
  L71 = 0;
  L31 = a;
  L141_br=L31==NULL?0:ASIZE((ARRAYSTR)L31); 
  noname1->self = L51;
  noname1->state = 0;
  noname2->self = L61;
  noname2->state = 0;
  indb_if_first = TRUE;
  eltb_if_first = TRUE;
  while (1) {
   if(L71>=L141_br)  goto after_loop; 
   L15 = FMAPST961226608(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   plus_self = L15;
   plus_sarg = ((STR) &name17);
   ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val3;
   L16 = FMAPST1937251125(noname2);
   if (noname2->state == -1) {
    goto after_loop;
   }
   plus_arg = L16;
   str_self = plus_arg;
   clear_self = buf1;
   L17 = (clear_self==((FSTR) NULL));
   L181_=!(L17); 
   if (L181_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val7 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val7;
   str_self1 = buf1;
   ret_val8 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val6 = ret_val8;
   s1 = ret_val6;
   ret_val5 = STR_ap1077157958(plus_self1, s1, TRUE);
   L19 = ret_val5;
   SARR((ARRAYSTR)L31,L71,(STR)L19); 
   ;
   L201_=INTPLUS(L71,1); 
   L71 = L201_;
  }
 }
 after_loop: ;
 sort_self = a;
 L22 = (sort_self==((ARRAYSTR) NULL));
 L231_=!(L22); 
 if (L231_) {
  L241_=ASIZE((ARRAYSTR)sort_self); 
  L25 = L241_;
  L261_=INTMINUS(L25,1); 
  ARRAYS1450857938(sort_self, 0, L261_);
 }
 s = ((STR) &name1);
 L27 = FMAPST868850642(ATTR(self,map));
 L281_=(0)<(L27); 
 if (L281_) {
  s = ((STR) &Statistics);
  {
   BOOL f_L291_ = TRUE;
   /* loop index variable */
   L81 = 0;
   L41 = a;
   L291_br=L41==NULL?0:ASIZE((ARRAYSTR)L41); 
   while (1) {
    plus_self2 = s;
    if(L81>=L291_br)  goto after_loop1; 
    aL291_=ARR((ARRAYSTR)L41,L81); 
    plus_sarg1 = aL291_;
    ret_val9 = STR_ap2004550586(plus_self2, plus_sarg1);
    plus_self3 = ret_val9;
    plus_sarg2 = ((STR) &name9);
    ret_val10 = STR_ap2004550586(plus_self3, plus_sarg2);
    s = ret_val10;
    L321_=INTPLUS(L81,1); 
    L81 = L321_;
   }
  }
  after_loop1: ;
 }
 ret_val = s;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT AM_ASS1201484234(AM_ASSIGN_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT AM_ASS1841384941(AM_ASSERT_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT AM_ATT1156941996(AM_ATTACH_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT AM_BRE1241758751(AM_BREAK_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT AM_CAS1510303900(AM_CASE_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT AM_COM1124366108(AM_COMMENT_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT AM_EXP1561605445(AM_EXPR_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

void AM_ASS1281039639(AM_ASSIGN_STMT self, dAM_STMT l) {
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

void AM_ASS306541468(AM_ASSERT_STMT self, dAM_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AM_ASS356960882(AM_ASSERT_STMT self, dAM_STMT l) {
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

void AM_ASS946442175(AM_ASSIGN_STMT self, dAM_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AM_ATT1913806391(AM_ATTACH_STMT self, dAM_STMT l) {
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

void AM_ATT990984413(AM_ATTACH_STMT self, dAM_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AM_BRE261382646(AM_BREAK_STMT self, dAM_STMT l) {
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

void AM_BRE905282136(AM_BREAK_STMT self, dAM_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AM_CAS1095788609(AM_CASE_STMT self, dAM_STMT l) {
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

void AM_CAS637622509(AM_CASE_STMT self, dAM_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AM_COM1022674779(AM_COMMENT_STMT self, dAM_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AM_COM832310723(AM_COMMENT_STMT self, dAM_STMT l) {
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

void AM_EXP1859190126(AM_EXPR_STMT self, dAM_STMT l) {
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

void AM_EXP586320964(AM_EXPR_STMT self, dAM_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void NULL_S1721445066(NULL_STAT self, STR K1) {
}


#undef IS_ITER
#define IS_ITER 0

void NULL_STAT_print(NULL_STAT self) {
}


#undef IS_ITER
#define IS_ITER 0

void SE_CON1013798744(SE_CONTEXT self, SIDE_EFFECT nse) {
 SIDE_EFFECT old_se;
 SE_CONTEXT full_self;
 BOOL ret_val = BOOL_zero;
 SIDE_EFFECT is_local_self;
 BOOL ret_val1 = BOOL_zero;
 FSETSIDE_EFFECT is_elt_nil_self;
 SIDE_EFFECT is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI351013237 is_elt_nil_self1;
 SIDE_EFFECT is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 SIDE_EFFECT is_local_self1;
 BOOL ret_val4 = BOOL_zero;
 SE_CONTEXT maxsize_self;
 INT ret_val5 = INT_zero;
 OUT create_self;
 OUT ret_val6;
 OUT plus_self;
 STR plus_s;
 OUT ret_val7;
 FILE1 stdout_self;
 FILE1 ret_val8;
 FILE1 r;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val9;
 FILE1 stdout_self1;
 FILE1 ret_val10;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val11;
 FILE1 r2;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L4;
 BOOL L5;
 BOOL L6;
 BOOL L71_;
 BOOL L8;
 BOOL L9;
 BOOL L101_;
 INT L11;
 INT L12;
 BOOL L131_;
 extern STR Markin664934141;
 FILE1 L14;
 OB L15;
 FILE1 L17;
 OB L18;
 extern STR istoobig;
 FILE1 L20;
 OB L22;
 L1 = SE_CON1026918814(self);
 L21_=!(L1); 
 if (L21_) {
  return;
 }
 full_self = self;
 if (ATTR(full_self,is_full)) {
  L4 = TRUE;
 } else {
  L4 = ATTR(full_self,has_import);
 }
 if (L4) {
  L3 = TRUE;
 } else {
  L3 = ATTR(full_self,has_export);
 }
 ret_val = L3;
 if (ret_val) {
  return;
 }
 is_local_self = nse;
 ret_val1 = (ATTR(is_local_self,tp)==((dTP) NULL));
 if (ret_val1) {
  return;
 }
 old_se = FSETSI937439554(ATTR(self,set), nse);
 is_elt_nil_self = ATTR(self,set);
 is_elt_nil_e = old_se;
 is_elt_nil_self1 = ((ELT_NI351013237) NULL);
 is_elt_nil_e1 = is_elt_nil_e;
 ret_val3 = (is_elt_nil_e1==((SIDE_EFFECT) NULL));
 ret_val2 = ret_val3;
 if (ret_val2) {
  SATTR(self,set,FSETSI784163459(ATTR(self,set), nse));
 }
 else {
  if (ATTR(nse,doeswrite)) {
   L6 = ATTR(old_se,doeswrite);
   L71_=!(L6); 
   L5 = L71_;
  } else {
   L5 = FALSE;
  }
  if (L5) {
   SATTR(self,set,FSETSI784163459(ATTR(self,set), nse));
  }
 }
 if (ATTR(nse,doeswrite)) {
  is_local_self1 = nse;
  ret_val4 = (ATTR(is_local_self1,tp)==((dTP) NULL));
  L9 = ret_val4;
  L101_=!(L9); 
  L8 = L101_;
 } else {
  L8 = FALSE;
 }
 if (L8) {
  SE_CON435970114(self);
 }
 maxsize_self = self;
 ret_val5 = 100;
 L11 = ret_val5;
 L12 = FSETSI2058428832(ATTR(self,set));
 L131_=(L11)<(L12); 
 if (L131_) {
  if (SE_CON765348064(self)) {
   create_self = ((OUT) NULL);
   ret_val6 = create_self;
   plus_self = ret_val6;
   plus_s = ((STR) &Markin664934141);
   stdout_self = ((FILE1) NULL);
   L15=ZALLOC(sizeof(struct FILE1_struct));
   if (L15==NULL) FATAL("Unable to allocate more memory");
   ((OB)L15)->header.tag=FILE1_tag;
   L14 = ((FILE1) L15);
   r = L14;
   
   SATTR(r,fp,stdout);
   ret_val8 = r;
   FILE_plus_STR(ret_val8, plus_s);
   ret_val7 = plus_self;
   plus_self1 = ret_val7;
   plus_s1 = ATTR(ATTR(self,rsig),str);
   stdout_self1 = ((FILE1) NULL);
   L18=ZALLOC(sizeof(struct FILE1_struct));
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FILE1_tag;
   L17 = ((FILE1) L18);
   r1 = L17;
   
   SATTR(r1,fp,stdout);
   ret_val10 = r1;
   FILE_plus_STR(ret_val10, plus_s1);
   ret_val9 = plus_self1;
   plus_self2 = ret_val9;
   plus_s2 = ((STR) &istoobig);
   stdout_self2 = ((FILE1) NULL);
   L22=ZALLOC(sizeof(struct FILE1_struct));
   if (L22==NULL) FATAL("Unable to allocate more memory");
   ((OB)L22)->header.tag=FILE1_tag;
   L20 = ((FILE1) L22);
   r2 = L20;
   
   SATTR(r2,fp,stdout);
   ret_val11 = r2;
   FILE_plus_STR(ret_val11, plus_s2);
  }
  SE_CON320528666(self);
 }
}


#undef IS_ITER
#define IS_ITER 0

void SE_CON1088557572(SE_CONTEXT self, SE_CONTEXT nsec) {
 SE_CONTEXT L11;
 SIG is_iter_self;
 BOOL ret_val = BOOL_zero;
 IDENT is_iter_self1 = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 SE_CONTEXT full_self;
 BOOL ret_val2 = BOOL_zero;
 SE_CONTEXT maxsize_self;
 INT ret_val3 = INT_zero;
 OUT create_self;
 OUT ret_val4;
 OUT plus_self;
 STR plus_s;
 OUT ret_val5;
 FILE1 stdout_self;
 FILE1 ret_val6;
 FILE1 r;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val7;
 FILE1 stdout_self1;
 FILE1 ret_val8;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val9;
 FILE1 r2;
 SE_CONTEXT full_self1;
 BOOL ret_val10 = BOOL_zero;
 SE_CONTEXT eltb_self;
 SIDE_EFFECT ret_val11;
 FSETSIDE_EFFECT L21;
 BOOL eltb_if_first = BOOL_zero;
 BOOL L3;
 BOOL L41_;
 BOOL L5;
 BOOL L6;
 BOOL L7;
 BOOL L8;
 BOOL L9;
 BOOL L10;
 BOOL L12;
 BOOL L13;
 BOOL L14;
 BOOL L15;
 BOOL L161_;
 STR L17;
 INT L18;
 INT L191_;
 INT L20;
 CHAR L221_;
 CHAR L23;
 BOOL L241_;
 BOOL L25;
 INT L26;
 INT L27;
 INT L281_;
 INT L29;
 INT L301_;
 BOOL L31;
 BOOL L32;
 INT L33;
 INT L34;
 INT L35;
 INT L361_;
 INT L37;
 BOOL L381_;
 extern STR Markin664934141;
 FILE1 L39;
 OB L40;
 FILE1 L43;
 OB L44;
 extern STR istoobig1;
 FILE1 L46;
 OB L47;
 BOOL L49;
 BOOL L50;
 SIDE_EFFECT L51;
 L3 = SE_CON1026918814(self);
 L41_=!(L3); 
 if (L41_) {
  return;
 }
 if ((nsec==((SE_CONTEXT) NULL))) {
  return;
 }
 if (ATTR(self,has_raise)) {
  L5 = TRUE;
 } else {
  L5 = ATTR(nsec,has_raise);
 }
 SATTR(self,has_raise,L5);
 if (ATTR(self,has_fatal_error)) {
  L6 = TRUE;
 } else {
  L6 = ATTR(nsec,has_fatal_error);
 }
 SATTR(self,has_fatal_error,L6);
 if (ATTR(self,has_import)) {
  L7 = TRUE;
 } else {
  L7 = ATTR(nsec,has_import);
 }
 SATTR(self,has_import,L7);
 if (ATTR(self,has_export)) {
  L8 = TRUE;
 } else {
  L8 = ATTR(nsec,has_export);
 }
 SATTR(self,has_export,L8);
 if (ATTR(self,has_arith_error)) {
  L9 = TRUE;
 } else {
  L9 = ATTR(nsec,has_arith_error);
 }
 SATTR(self,has_arith_error,L9);
 if (ATTR(self,may_block)) {
  L10 = TRUE;
 } else {
  L10 = ATTR(nsec,may_block);
 }
 SATTR(self,may_block,L10);
 if (ATTR(self,forks)) {
  L12 = TRUE;
 } else {
  L12 = ATTR(nsec,forks);
 }
 SATTR(self,forks,L12);
 if (ATTR(self,has_new)) {
  L13 = TRUE;
 } else {
  L13 = ATTR(nsec,has_new);
 }
 SATTR(self,has_new,L13);
 is_iter_self = ATTR(self,rsig);
 is_iter_self1 = ATTR(is_iter_self,name1);
 L15 = (is_iter_self1.str==((STR) NULL));
 L161_=!(L15); 
 if (L161_) {
  L17 = is_iter_self1.str;
  L18 = STR_sizerINT(is_iter_self1.str);
  L191_=INTMINUS(L18,1); 
  L20 = L191_;
  L221_=ARR((STR)L17,L20); 
  L23 = L221_;
  L241_=L23=='!'; 
  L14 = L241_;
 } else {
  L14 = FALSE;
 }
 ret_val1 = L14;
 ret_val = ret_val1;
 if (ret_val) {
  if (ATTR(self,has_yield_in_loc)) {
   L25 = TRUE;
  } else {
   L25 = ATTR(nsec,has_yield_in_loc);
  }
  SATTR(self,has_yield_in_loc,L25);
 }
 L26 = ATTR(self,weight);
 L27 = ATTR(nsec,weight);
 L281_=INTPLUS(L26,L27); 
 SATTR(self,weight,L281_);
 L29 = ATTR(self,weight);
 L301_=(L29<1000000)?L29:1000000; 
 SATTR(self,weight,L301_);
 full_self = self;
 if (ATTR(full_self,is_full)) {
  L32 = TRUE;
 } else {
  L32 = ATTR(full_self,has_import);
 }
 if (L32) {
  L31 = TRUE;
 } else {
  L31 = ATTR(full_self,has_export);
 }
 ret_val2 = L31;
 if (ret_val2) {
  return;
 }
 if (ATTR(nsec,unsafe)) {
  SE_CON435970114(self);
 }
 maxsize_self = self;
 ret_val3 = 100;
 L33 = ret_val3;
 L34 = FSETSI2058428832(ATTR(self,set));
 L35 = FSETSI2058428832(ATTR(nsec,set));
 L361_=INTPLUS(L34,L35); 
 L37 = L361_;
 L381_=(L33)<(L37); 
 if (L381_) {
  if (SE_CON765348064(self)) {
   create_self = ((OUT) NULL);
   ret_val4 = create_self;
   plus_self = ret_val4;
   plus_s = ((STR) &Markin664934141);
   stdout_self = ((FILE1) NULL);
   L40=ZALLOC(sizeof(struct FILE1_struct));
   if (L40==NULL) FATAL("Unable to allocate more memory");
   ((OB)L40)->header.tag=FILE1_tag;
   L39 = ((FILE1) L40);
   r = L39;
   
   SATTR(r,fp,stdout);
   ret_val6 = r;
   FILE_plus_STR(ret_val6, plus_s);
   ret_val5 = plus_self;
   plus_self1 = ret_val5;
   plus_s1 = ATTR(ATTR(self,rsig),str);
   stdout_self1 = ((FILE1) NULL);
   L44=ZALLOC(sizeof(struct FILE1_struct));
   if (L44==NULL) FATAL("Unable to allocate more memory");
   ((OB)L44)->header.tag=FILE1_tag;
   L43 = ((FILE1) L44);
   r1 = L43;
   
   SATTR(r1,fp,stdout);
   ret_val8 = r1;
   FILE_plus_STR(ret_val8, plus_s1);
   ret_val7 = plus_self1;
   plus_self2 = ret_val7;
   plus_s2 = ((STR) &istoobig1);
   stdout_self2 = ((FILE1) NULL);
   L47=ZALLOC(sizeof(struct FILE1_struct));
   if (L47==NULL) FATAL("Unable to allocate more memory");
   ((OB)L47)->header.tag=FILE1_tag;
   L46 = ((FILE1) L47);
   r2 = L46;
   
   SATTR(r2,fp,stdout);
   ret_val9 = r2;
   FILE_plus_STR(ret_val9, plus_s2);
  }
  SE_CON320528666(self);
 }
 else {
  full_self1 = nsec;
  if (ATTR(full_self1,is_full)) {
   L50 = TRUE;
  } else {
   L50 = ATTR(full_self1,has_import);
  }
  if (L50) {
   L49 = TRUE;
  } else {
   L49 = ATTR(full_self1,has_export);
  }
  ret_val10 = L49;
  if (ret_val10) {
   SE_CON320528666(self);
  }
  else {
   {
    struct FSETSI2093978094_frame_struct other1_0;
FSETSI2093978094_frame noname1 = &other1_0;
    /* Initialization of inlined iter SE_CONTEXT::elt!:SIDE_EFFECT */
    L11 = nsec;
    eltb_self = L11;
    L21 = ATTRs(eltb_self,set,((FSETSIDE_EFFECT) NULL));
    noname1->self = L21;
    noname1->state = 0;
    eltb_if_first = TRUE;
    while (1) {
     L51 = FSETSI2093978094(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     SE_CON1013798744(self, L51);
    }
   }
   after_loop: ;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

void SE_CON1222696490(SE_CONTEXT self, dAM amattr, BOOL doeswrite1) {
 SIDE_EFFECT nse;
 BOOL L1;
 BOOL L21_;
 L1 = SE_CON1026918814(self);
 L21_=!(L1); 
 if (L21_) {
  return;
 }
 nse = SIDE_E524783111(((SIDE_EFFECT) NULL), ((OB) amattr), doeswrite1);
 SE_CON1013798744(self, nse);
}


#undef IS_ITER
#define IS_ITER 0

void SE_CON1786426658(SE_CONTEXT self) {
 SE_CONTEXT full_self;
 BOOL full_b = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 L1 = SE_CON1026918814(self);
 L21_=!(L1); 
 if (L21_) {
  return;
 }
 full_self = self;
 full_b = FALSE;
 SATTR(full_self,is_full,full_b);
 SATTR(self,set,((FSETSIDE_EFFECT) NULL));
 SATTR(self,unsafe,FALSE);
 SATTR(self,has_fatal_error,FALSE);
 SATTR(self,has_import,FALSE);
 SATTR(self,has_export,FALSE);
 SATTR(self,has_arith_error,FALSE);
 SATTR(self,has_new,FALSE);
}


#undef IS_ITER
#define IS_ITER 0

void SE_CON184777890(SE_CONTEXT self, ARRAYSTR a, BOOL doeswrite1) {
 ARRAYSTR L11;
 STR s;
 INT i = INT_zero;
 STR tpname;
 STR aname;
 TP_CLASS tp;
 SIDE_EFFECT se;
 INT L21 = INT_zero;
 TP_TBL tp_class_for_sel;
 STR tp_class_for_s;
 TP_CLASS ret_val;
 INT i1 = INT_zero;
 SIDE_EFFECT create_self;
 SIDE_EFFECT ret_val1;
 INT L31_br;
 STR aL31_;
 STR L4;
 STR L5;
 INT L6;
 INT L71_;
 INT L8;
 INT L91_;
 BOOL L10;
 extern STR ROUT1;
 INT L12;
 BOOL L131_;
 BOOL L14;
 BOOL L151_;
 extern STR ITER;
 INT L16;
 BOOL L171_;
 BOOL L18;
 BOOL L191_;
 INT* L20;
 TP_CLASS L22;
 SIDE_EFFECT L23;
 OB L24;
 extern STR name57;
 BOOL L25;
 BOOL L261_;
 INT L271_;
 if ((a==((ARRAYSTR) NULL))) {
  return;
 }
 {
  BOOL f_L31_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = a;
  L31_br=L11==NULL?0:ASIZE((ARRAYSTR)L11); 
  while (1) {
   if(L21>=L31_br)  goto after_loop; 
   aL31_=ARR((ARRAYSTR)L11,L21); 
   L5 = aL31_;
   s = SE_CON1246526640(self, L5, ATTR(self,rsig));
   i = STR_se1725540125(s, ':');
   tpname = STR_head_INTrSTR(s, i);
   L6 = STR_sizerINT(s);
   L71_=INTMINUS(L6,i); 
   L8 = L71_;
   L91_=INTMINUS(L8,2); 
   aname = STR_tail_INTrSTR(s, L91_);
   L12 = STR_se2132153226(tpname, ((STR) &ROUT1));
   L131_=(L12)==(-1); 
   L14 = L131_;
   L151_=!(L14); 
   if (L151_) {
    L10 = TRUE;
   } else {
    L16 = STR_se2132153226(tpname, ((STR) &ITER));
    L171_=(L16)==(-1); 
    L18 = L171_;
    L191_=!(L18); 
    L10 = L191_;
   }
   if (L10) {
    SE_CON320528666(self);
    return;
   }
   tp_class_for_sel = ATTR(SE_CONTEXT_prog,tp_tbl);
   tp_class_for_s = tpname;
   i1 = 0;
   L20 = &i1;
   L22 = TP_TBL1484824239(tp_class_for_sel, tp_class_for_s, L20);
   ret_val = L22;
   tp = ret_val;
   create_self = ((SIDE_EFFECT) NULL);
   L24=ZALLOC(sizeof(struct SIDE_EFFECT_struct));
   if (L24==NULL) FATAL("Unable to allocate more memory");
   ((OB)L24)->header.tag=SIDE_EFFECT_tag;
   L23 = ((SIDE_EFFECT) L24);
   ret_val1 = L23;
   se = ret_val1;
   SATTR(se,tp,((dTP) tp));
   L25 = STR_is111530248(aname, ((STR) &name57));
   L261_=!(L25); 
   if (L261_) {
    SATTR(se,name1,IDENT_1150413730(IDENT_zero, aname));
   }
   SATTR(se,doeswrite,doeswrite1);
   SE_CON1013798744(self, se);
   L271_=INTPLUS(L21,1); 
   L21 = L271_;
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void SE_CON320528666(SE_CONTEXT self) {
 SE_CONTEXT full_self;
 BOOL full_b = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 L1 = SE_CON1026918814(self);
 L21_=!(L1); 
 if (L21_) {
  return;
 }
 full_self = self;
 full_b = TRUE;
 SATTR(full_self,is_full,full_b);
 SATTR(self,set,((FSETSIDE_EFFECT) NULL));
 SATTR(self,unsafe,TRUE);
}


#undef IS_ITER
#define IS_ITER 0

void SE_CON412941612(SE_CONTEXT self, dAM amattr) {
 SE_CONTEXT new_sec;
 SE_CONTEXT new_sec1;
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
 OUT ret_val11;
 FILE1 stdout_self5;
 FILE1 ret_val12;
 FILE1 r5;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val13;
 FILE1 r6;
 BOOL L1;
 BOOL L21_;
 extern STR Markin891501023;
 FILE1 L3;
 OB L4;
 extern STR Markin664934141;
 FILE1 L6;
 OB L7;
 FILE1 L9;
 OB L10;
 extern STR isbound;
 FILE1 L12;
 OB L13;
 extern STR Markin664934141;
 FILE1 L15;
 OB L16;
 FILE1 L18;
 OB L19;
 extern STR isbound;
 FILE1 L22;
 OB L23;
 L1 = SE_CON1026918814(self);
 L21_=!(L1); 
 if (L21_) {
  return;
 }
 if (amattr==NULL) {
  goto other402;
 } else
 switch (TAG(amattr)) {
  case AM_ROU1916046290_tag:
   new_sec = SE_CON2084169946(self, ATTR(((AM_ROU1916046290) amattr),fun));
   SE_CON1088557572(self, new_sec); break;
  case AM_ITE1809135850_tag:
   new_sec1 = SE_CON2084169946(self, ATTR(((AM_ITE1809135850) amattr),fun));
   SE_CON1088557572(self, new_sec1); break;
  case AM_EXT_CALL_EXPR_tag:
   SE_CON320528666(self);
   if (SE_CON765348064(self)) {
    create_self = ((OUT) NULL);
    ret_val = create_self;
    plus_self = ret_val;
    plus_s = ((STR) &Markin891501023);
    stdout_self = ((FILE1) NULL);
    L4=ZALLOC(sizeof(struct FILE1_struct));
    if (L4==NULL) FATAL("Unable to allocate more memory");
    ((OB)L4)->header.tag=FILE1_tag;
    L3 = ((FILE1) L4);
    r = L3;
    
    SATTR(r,fp,stdout);
    ret_val1 = r;
    FILE_plus_STR(ret_val1, plus_s);
   } break;
  case AM_BND1124877163_tag: break;
  case AM_BND260301329_tag:
   SE_CON320528666(self);
   if (SE_CON765348064(self)) {
    create_self1 = ((OUT) NULL);
    ret_val2 = create_self1;
    plus_self1 = ret_val2;
    plus_s1 = ((STR) &Markin664934141);
    stdout_self1 = ((FILE1) NULL);
    L7=ZALLOC(sizeof(struct FILE1_struct));
    if (L7==NULL) FATAL("Unable to allocate more memory");
    ((OB)L7)->header.tag=FILE1_tag;
    L6 = ((FILE1) L7);
    r1 = L6;
    
    SATTR(r1,fp,stdout);
    ret_val4 = r1;
    FILE_plus_STR(ret_val4, plus_s1);
    ret_val3 = plus_self1;
    plus_self2 = ret_val3;
    plus_s2 = ATTR(ATTR(self,rsig),str);
    stdout_self2 = ((FILE1) NULL);
    L10=ZALLOC(sizeof(struct FILE1_struct));
    if (L10==NULL) FATAL("Unable to allocate more memory");
    ((OB)L10)->header.tag=FILE1_tag;
    L9 = ((FILE1) L10);
    r2 = L9;
    
    SATTR(r2,fp,stdout);
    ret_val6 = r2;
    FILE_plus_STR(ret_val6, plus_s2);
    ret_val5 = plus_self2;
    plus_self3 = ret_val5;
    plus_s3 = ((STR) &isbound);
    stdout_self3 = ((FILE1) NULL);
    L13=ZALLOC(sizeof(struct FILE1_struct));
    if (L13==NULL) FATAL("Unable to allocate more memory");
    ((OB)L13)->header.tag=FILE1_tag;
    L12 = ((FILE1) L13);
    r3 = L12;
    
    SATTR(r3,fp,stdout);
    ret_val7 = r3;
    FILE_plus_STR(ret_val7, plus_s3);
   } break;
  case AM_BND367211769_tag:
   SE_CON320528666(self);
   if (SE_CON765348064(self)) {
    create_self2 = ((OUT) NULL);
    ret_val8 = create_self2;
    plus_self4 = ret_val8;
    plus_s4 = ((STR) &Markin664934141);
    stdout_self4 = ((FILE1) NULL);
    L16=ZALLOC(sizeof(struct FILE1_struct));
    if (L16==NULL) FATAL("Unable to allocate more memory");
    ((OB)L16)->header.tag=FILE1_tag;
    L15 = ((FILE1) L16);
    r4 = L15;
    
    SATTR(r4,fp,stdout);
    ret_val10 = r4;
    FILE_plus_STR(ret_val10, plus_s4);
    ret_val9 = plus_self4;
    plus_self5 = ret_val9;
    plus_s5 = ATTR(ATTR(self,rsig),str);
    stdout_self5 = ((FILE1) NULL);
    L19=ZALLOC(sizeof(struct FILE1_struct));
    if (L19==NULL) FATAL("Unable to allocate more memory");
    ((OB)L19)->header.tag=FILE1_tag;
    L18 = ((FILE1) L19);
    r5 = L18;
    
    SATTR(r5,fp,stdout);
    ret_val12 = r5;
    FILE_plus_STR(ret_val12, plus_s5);
    ret_val11 = plus_self5;
    plus_self6 = ret_val11;
    plus_s6 = ((STR) &isbound);
    stdout_self6 = ((FILE1) NULL);
    L23=ZALLOC(sizeof(struct FILE1_struct));
    if (L23==NULL) FATAL("Unable to allocate more memory");
    ((OB)L23)->header.tag=FILE1_tag;
    L22 = ((FILE1) L23);
    r6 = L22;
    
    SATTR(r6,fp,stdout);
    ret_val13 = r6;
    FILE_plus_STR(ret_val13, plus_s6);
   } break;
  default: ;
  other402: ;
 }
}


#undef IS_ITER
#define IS_ITER 0

void SE_CON435970114(SE_CONTEXT self) {
 BOOL L1;
 BOOL L21_;
 L1 = SE_CON1026918814(self);
 L21_=!(L1); 
 if (L21_) {
  return;
 }
 SATTR(self,unsafe,TRUE);
}


#undef IS_ITER
#define IS_ITER 0

void SE_CON621544985(SE_CONTEXT self) {
 SE_CONTEXT L11;
 SIG set_se_context_s;
 SE_CONTEXT set_se_context_s1;
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
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 SE_CONTEXT full_self;
 BOOL ret_val4 = BOOL_zero;
 OUT create_self1;
 OUT ret_val5;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val6;
 FILE1 r2;
 OUT create_self2;
 OUT ret_val7;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val8;
 FILE1 r3;
 OUT create_self3;
 OUT ret_val9;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val10;
 FILE1 r4;
 OUT create_self4;
 OUT ret_val11;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val12;
 FILE1 r5;
 OUT create_self5;
 OUT ret_val13;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val14;
 FILE1 r6;
 OUT create_self6;
 OUT ret_val15;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val16;
 FILE1 r7;
 OUT create_self7;
 OUT ret_val17;
 OUT plus_self8;
 STR plus_s8;
 FILE1 stdout_self8;
 FILE1 ret_val18;
 FILE1 r8;
 OUT create_self8;
 OUT ret_val19;
 OUT plus_self9;
 STR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val20;
 FILE1 r9;
 OUT create_self9;
 OUT ret_val21;
 OUT plus_self10;
 STR plus_s10;
 FILE1 stdout_self10;
 FILE1 ret_val22;
 FILE1 r10;
 OUT create_self10;
 OUT ret_val23;
 OUT plus_self11;
 STR plus_s11;
 FILE1 stdout_self11;
 FILE1 ret_val24;
 FILE1 r11;
 OUT create_self11;
 OUT ret_val25;
 OUT plus_self12;
 STR plus_s12;
 FILE1 stdout_self12;
 FILE1 ret_val26;
 FILE1 r12;
 OUT create_self12;
 OUT ret_val27;
 SE_CONTEXT eltb_self;
 SIDE_EFFECT ret_val28;
 FSETSIDE_EFFECT L21;
 BOOL eltb_if_first = BOOL_zero;
 STR plus_self13;
 STR plus_sarg;
 STR ret_val29;
 STR plus_self14;
 STR plus_sarg1;
 STR ret_val30;
 OUT plus_self15;
 STR plus_s13;
 FILE1 stdout_self13;
 FILE1 ret_val31;
 FILE1 r13;
 BOOL L3;
 dTP L4;
 STR L5;
 extern STR OPT_DEBUG;
 extern STR Sideeffectsfor;
 FILE1 L6;
 OB L7;
 FILE1 L9;
 OB L10;
 BOOL L13;
 BOOL L14;
 extern STR full1;
 FILE1 L15;
 OB L16;
 extern STR unsafe;
 FILE1 L18;
 OB L19;
 extern STR has_raise;
 FILE1 L22;
 OB L23;
 extern STR has_fatal_error;
 FILE1 L25;
 OB L26;
 extern STR has_import;
 FILE1 L28;
 OB L29;
 extern STR has_export;
 FILE1 L31;
 OB L32;
 extern STR has_arith_error;
 FILE1 L34;
 OB L35;
 extern STR may_block1;
 FILE1 L37;
 OB L38;
 extern STR forks;
 FILE1 L40;
 OB L41;
 extern STR has_new;
 FILE1 L43;
 OB L44;
 extern STR name9;
 FILE1 L46;
 OB L47;
 extern STR name41;
 SIDE_EFFECT L49;
 extern STR name9;
 FILE1 L50;
 OB L51;
 if ((self==((SE_CONTEXT) NULL))) {
  return;
 }
 if (SIG_is418491101(ATTR(self,rsig))) {
  SE_CON897506020(self);
 }
 if (ATTR(SE_CONTEXT_prog,opt_debug)) {
  L4 = ATTR(ATTR(self,rsig),tp);
  L5 = (*dTP_strrSTR[TAG(L4)])(L4);
  L3 = STR_is111530248(L5, ((STR) &OPT_DEBUG));
 } else {
  L3 = FALSE;
 }
 if (L3) {
  SE_CON1786426658(self);
  SATTR(self,has_fatal_error,FALSE);
  SATTR(self,has_raise,FALSE);
 }
 set_se_context_s = ATTR(self,rsig);
 set_se_context_s1 = self;
 SATTR(set_se_context_s,se_context,set_se_context_s1);
 if (SE_CON765348064(self)) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &Sideeffectsfor);
  stdout_self = ((FILE1) NULL);
  L7=ZALLOC(sizeof(struct FILE1_struct));
  if (L7==NULL) FATAL("Unable to allocate more memory");
  ((OB)L7)->header.tag=FILE1_tag;
  L6 = ((FILE1) L7);
  r = L6;
  
  SATTR(r,fp,stdout);
  ret_val2 = r;
  FILE_plus_STR(ret_val2, plus_s);
  ret_val1 = plus_self;
  plus_self1 = ret_val1;
  plus_s1 = ATTR(ATTR(self,rsig),str);
  stdout_self1 = ((FILE1) NULL);
  L10=ZALLOC(sizeof(struct FILE1_struct));
  if (L10==NULL) FATAL("Unable to allocate more memory");
  ((OB)L10)->header.tag=FILE1_tag;
  L9 = ((FILE1) L10);
  r1 = L9;
  
  SATTR(r1,fp,stdout);
  ret_val3 = r1;
  FILE_plus_STR(ret_val3, plus_s1);
  full_self = self;
  if (ATTR(full_self,is_full)) {
   L14 = TRUE;
  } else {
   L14 = ATTR(full_self,has_import);
  }
  if (L14) {
   L13 = TRUE;
  } else {
   L13 = ATTR(full_self,has_export);
  }
  ret_val4 = L13;
  if (ret_val4) {
   create_self1 = ((OUT) NULL);
   ret_val5 = create_self1;
   plus_self2 = ret_val5;
   plus_s2 = ((STR) &full1);
   stdout_self2 = ((FILE1) NULL);
   L16=ZALLOC(sizeof(struct FILE1_struct));
   if (L16==NULL) FATAL("Unable to allocate more memory");
   ((OB)L16)->header.tag=FILE1_tag;
   L15 = ((FILE1) L16);
   r2 = L15;
   
   SATTR(r2,fp,stdout);
   ret_val6 = r2;
   FILE_plus_STR(ret_val6, plus_s2);
  }
  if (ATTR(self,unsafe)) {
   create_self2 = ((OUT) NULL);
   ret_val7 = create_self2;
   plus_self3 = ret_val7;
   plus_s3 = ((STR) &unsafe);
   stdout_self3 = ((FILE1) NULL);
   L19=ZALLOC(sizeof(struct FILE1_struct));
   if (L19==NULL) FATAL("Unable to allocate more memory");
   ((OB)L19)->header.tag=FILE1_tag;
   L18 = ((FILE1) L19);
   r3 = L18;
   
   SATTR(r3,fp,stdout);
   ret_val8 = r3;
   FILE_plus_STR(ret_val8, plus_s3);
  }
  if (ATTR(self,has_raise)) {
   create_self3 = ((OUT) NULL);
   ret_val9 = create_self3;
   plus_self4 = ret_val9;
   plus_s4 = ((STR) &has_raise);
   stdout_self4 = ((FILE1) NULL);
   L23=ZALLOC(sizeof(struct FILE1_struct));
   if (L23==NULL) FATAL("Unable to allocate more memory");
   ((OB)L23)->header.tag=FILE1_tag;
   L22 = ((FILE1) L23);
   r4 = L22;
   
   SATTR(r4,fp,stdout);
   ret_val10 = r4;
   FILE_plus_STR(ret_val10, plus_s4);
  }
  if (ATTR(self,has_fatal_error)) {
   create_self4 = ((OUT) NULL);
   ret_val11 = create_self4;
   plus_self5 = ret_val11;
   plus_s5 = ((STR) &has_fatal_error);
   stdout_self5 = ((FILE1) NULL);
   L26=ZALLOC(sizeof(struct FILE1_struct));
   if (L26==NULL) FATAL("Unable to allocate more memory");
   ((OB)L26)->header.tag=FILE1_tag;
   L25 = ((FILE1) L26);
   r5 = L25;
   
   SATTR(r5,fp,stdout);
   ret_val12 = r5;
   FILE_plus_STR(ret_val12, plus_s5);
  }
  if (ATTR(self,has_import)) {
   create_self5 = ((OUT) NULL);
   ret_val13 = create_self5;
   plus_self6 = ret_val13;
   plus_s6 = ((STR) &has_import);
   stdout_self6 = ((FILE1) NULL);
   L29=ZALLOC(sizeof(struct FILE1_struct));
   if (L29==NULL) FATAL("Unable to allocate more memory");
   ((OB)L29)->header.tag=FILE1_tag;
   L28 = ((FILE1) L29);
   r6 = L28;
   
   SATTR(r6,fp,stdout);
   ret_val14 = r6;
   FILE_plus_STR(ret_val14, plus_s6);
  }
  if (ATTR(self,has_export)) {
   create_self6 = ((OUT) NULL);
   ret_val15 = create_self6;
   plus_self7 = ret_val15;
   plus_s7 = ((STR) &has_export);
   stdout_self7 = ((FILE1) NULL);
   L32=ZALLOC(sizeof(struct FILE1_struct));
   if (L32==NULL) FATAL("Unable to allocate more memory");
   ((OB)L32)->header.tag=FILE1_tag;
   L31 = ((FILE1) L32);
   r7 = L31;
   
   SATTR(r7,fp,stdout);
   ret_val16 = r7;
   FILE_plus_STR(ret_val16, plus_s7);
  }
  if (ATTR(self,has_arith_error)) {
   create_self7 = ((OUT) NULL);
   ret_val17 = create_self7;
   plus_self8 = ret_val17;
   plus_s8 = ((STR) &has_arith_error);
   stdout_self8 = ((FILE1) NULL);
   L35=ZALLOC(sizeof(struct FILE1_struct));
   if (L35==NULL) FATAL("Unable to allocate more memory");
   ((OB)L35)->header.tag=FILE1_tag;
   L34 = ((FILE1) L35);
   r8 = L34;
   
   SATTR(r8,fp,stdout);
   ret_val18 = r8;
   FILE_plus_STR(ret_val18, plus_s8);
  }
  if (ATTR(self,may_block)) {
   create_self8 = ((OUT) NULL);
   ret_val19 = create_self8;
   plus_self9 = ret_val19;
   plus_s9 = ((STR) &may_block1);
   stdout_self9 = ((FILE1) NULL);
   L38=ZALLOC(sizeof(struct FILE1_struct));
   if (L38==NULL) FATAL("Unable to allocate more memory");
   ((OB)L38)->header.tag=FILE1_tag;
   L37 = ((FILE1) L38);
   r9 = L37;
   
   SATTR(r9,fp,stdout);
   ret_val20 = r9;
   FILE_plus_STR(ret_val20, plus_s9);
  }
  if (ATTR(self,forks)) {
   create_self9 = ((OUT) NULL);
   ret_val21 = create_self9;
   plus_self10 = ret_val21;
   plus_s10 = ((STR) &forks);
   stdout_self10 = ((FILE1) NULL);
   L41=ZALLOC(sizeof(struct FILE1_struct));
   if (L41==NULL) FATAL("Unable to allocate more memory");
   ((OB)L41)->header.tag=FILE1_tag;
   L40 = ((FILE1) L41);
   r10 = L40;
   
   SATTR(r10,fp,stdout);
   ret_val22 = r10;
   FILE_plus_STR(ret_val22, plus_s10);
  }
  if (ATTR(self,has_new)) {
   create_self10 = ((OUT) NULL);
   ret_val23 = create_self10;
   plus_self11 = ret_val23;
   plus_s11 = ((STR) &has_new);
   stdout_self11 = ((FILE1) NULL);
   L44=ZALLOC(sizeof(struct FILE1_struct));
   if (L44==NULL) FATAL("Unable to allocate more memory");
   ((OB)L44)->header.tag=FILE1_tag;
   L43 = ((FILE1) L44);
   r11 = L43;
   
   SATTR(r11,fp,stdout);
   ret_val24 = r11;
   FILE_plus_STR(ret_val24, plus_s11);
  }
  create_self11 = ((OUT) NULL);
  ret_val25 = create_self11;
  plus_self12 = ret_val25;
  plus_s12 = ((STR) &name9);
  stdout_self12 = ((FILE1) NULL);
  L47=ZALLOC(sizeof(struct FILE1_struct));
  if (L47==NULL) FATAL("Unable to allocate more memory");
  ((OB)L47)->header.tag=FILE1_tag;
  L46 = ((FILE1) L47);
  r12 = L46;
  
  SATTR(r12,fp,stdout);
  ret_val26 = r12;
  FILE_plus_STR(ret_val26, plus_s12);
  {
   struct FSETSI2093978094_frame_struct other1_0;
FSETSI2093978094_frame noname1 = &other1_0;
   /* Initialization of inlined iter SE_CONTEXT::elt!:SIDE_EFFECT */
   L11 = self;
   eltb_self = L11;
   L21 = ATTRs(eltb_self,set,((FSETSIDE_EFFECT) NULL));
   noname1->self = L21;
   noname1->state = 0;
   eltb_if_first = TRUE;
   while (1) {
    create_self12 = ((OUT) NULL);
    ret_val27 = create_self12;
    plus_self15 = ret_val27;
    plus_self13 = ((STR) &name41);
    L49 = FSETSI2093978094(noname1);
    if (noname1->state == -1) {
     goto after_loop;
    }
    plus_sarg = SIDE_E1431483993(L49);
    ret_val29 = STR_ap2004550586(plus_self13, plus_sarg);
    plus_self14 = ret_val29;
    plus_sarg1 = ((STR) &name9);
    ret_val30 = STR_ap2004550586(plus_self14, plus_sarg1);
    plus_s13 = ret_val30;
    stdout_self13 = ((FILE1) NULL);
    L51=ZALLOC(sizeof(struct FILE1_struct));
    if (L51==NULL) FATAL("Unable to allocate more memory");
    ((OB)L51)->header.tag=FILE1_tag;
    L50 = ((FILE1) L51);
    r13 = L50;
    
    SATTR(r13,fp,stdout);
    ret_val31 = r13;
    FILE_plus_STR(ret_val31, plus_s13);
   }
  }
  after_loop: ;
 }
}


#undef IS_ITER
#define IS_ITER 0

void SE_CON773743077(SE_CONTEXT self) {
 dGET_OPTIONS temp11;
 BOOL L1;
 BOOL L21_;
 L1 = (SE_CON1373267262==((CS_OPTIONS) NULL));
 L21_=!(L1); 
 if (L21_) {
  return;
 }
 temp11 = ATTR(SE_CONTEXT_prog,get_options);
 switch (TAG(temp11)) {
  case CS_OPTIONS_tag:
   SE_CON1373267262 = ((CS_OPTIONS) temp11); break;
  default: ;
   FATAL("No applicable type in typecase\nin SE_CONTEXT::get_options\n./Middle/side_effect.sa:139:14");
 }
}


#undef IS_ITER
#define IS_ITER 0

void SE_CON897506020(SE_CONTEXT self) {
 CONFIG_ROUT raises_none_self;
 BOOL ret_val = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L4;
 BOOL L5;
 BOOL L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 L1 = SE_CON1026918814(self);
 L21_=!(L1); 
 if (L21_) {
  return;
 }
 if (ATTR(ATTR(ATTR(self,rsig),builtin_info),volatile1)) {
  L4 = TRUE;
 } else {
  L4 = ATTR(ATTR(ATTR(self,rsig),builtin_info),reads_any);
 }
 if (L4) {
  L3 = TRUE;
 } else {
  L3 = ATTR(ATTR(ATTR(self,rsig),builtin_info),writes_any);
 }
 if (L3) {
  SE_CON320528666(self);
 }
 if (ATTR(ATTR(ATTR(self,rsig),builtin_info),fragile)) {
  SE_CON435970114(self);
 }
 SATTR(self,has_import,ATTR(ATTR(ATTR(self,rsig),builtin_info),does_import));
 SATTR(self,has_export,ATTR(ATTR(ATTR(self,rsig),builtin_info),does_export));
 SATTR(self,has_arith_error,ATTR(ATTR(ATTR(self,rsig),builtin_info),arith));
 SATTR(self,may_block,ATTR(ATTR(ATTR(self,rsig),builtin_info),block1));
 raises_none_self = ATTR(ATTR(self,rsig),builtin_info);
 if ((ATTR(raises_none_self,raises)==((ARRAYSTR) NULL))) {
  L6 = ATTR(raises_none_self,raises_any);
  L71_=!(L6); 
  L5 = L71_;
 } else {
  L5 = FALSE;
 }
 ret_val = L5;
 L8 = ret_val;
 L91_=!(L8); 
 SATTR(self,has_raise,L91_);
 SE_CON184777890(self, ATTR(ATTR(ATTR(self,rsig),builtin_info),reads), FALSE);
 SE_CON184777890(self, ATTR(ATTR(ATTR(self,rsig),builtin_info),writes), TRUE);
}


#undef IS_ITER
#define IS_ITER 0

void SIG_compute_str(SIG self) {
 FSTR s;
 ARRAYARG L11;
 ARG arg;
 STR argtpname = ((STR) NULL);
 STR L21;
 ARRAYBOOL L31;
 FSTR plus_self;
 CHAR plus_c = CHAR_zero;
 FSTR ret_val;
 INT L41 = INT_zero;
 FSTR plus_self1;
 CHAR plus_c1 = CHAR_zero;
 FSTR ret_val1;
 FSTR plus_self2;
 CHAR plus_c2 = CHAR_zero;
 FSTR ret_val2;
 FSTR plus_self3;
 CHAR plus_c3 = CHAR_zero;
 FSTR ret_val3;
 FSTR str_self;
 STR ret_val4;
 FSTR L5;
 dTP L6;
 FSTR L7;
 extern STR name34;
 FSTR L8;
 BOOL L9;
 BOOL L101_;
 extern STR name3;
 INT L121_br;
 ARG aL121_;
 ARG L14;
 BOOL L15;
 BOOL L161_;
 dMODE L17;
 dTP L18;
 STR L19;
 BOOL L20;
 BOOL L22;
 BOOL L231_;
 BOOL L24;
 INT L131_br;
INT i_L131_=0;
 BOOL aL131_;
 INT L251_;
 BOOL L26;
 BOOL L271_;
 extern STR SAME2;
 FSTR L28;
 dTP L29;
 L5 = FSTR_c1998740379(((FSTR) NULL), 64);
 L6 = ATTR(self,tp);
 L7 = FSTR_p1752847026(L5, (*dTP_strrSTR[TAG(L6)])(L6));
 L8 = FSTR_p1752847026(L7, ((STR) &name34));
 s = FSTR_p1752847026(L8, ATTR(self,name1).str);
 L9 = (ATTR(self,args)==((ARRAYARG) NULL));
 L101_=!(L9); 
 if (L101_) {
  plus_self = s;
  plus_c = '(';
  ret_val = FSTR_p399773021(plus_self, plus_c);
  s = ret_val;
  {
   BOOL f_L121_ = TRUE;
   struct STR_se405450305_frame_struct other1_0;
STR_se405450305_frame noname1 = &other1_0;
   BOOL f_L131_ = TRUE;
   /* loop index variable */
   L41 = 0;
   L11 = ATTR(self,args);
   L21 = ((STR) &name3);
   L121_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
   noname1->self = L21;
   noname1->state = 0;
   while (1) {
    if(L41>=L121_br)  goto after_loop; 
    aL121_=ARR((ARRAYARG)L11,L41); 
    arg = aL121_;
    L15 = (arg==((ARG) NULL));
    L161_=!(L15); 
    if (L161_) {
     L17 = ATTR(arg,mode1);
     if ((*dMODE_814247358[TAG(L17)])(L17, ((dMODE) MODES_once_mode))) {
      L18 = ATTR(arg,tp);
      argtpname = (*dTP_strrSTR[TAG(L18)])(L18);
     }
     else {
      argtpname = ARG_strrSTR(arg);
     }
    }
    noname1->arg1 = ((dSTR) argtpname);
    L19 = STR_se405450305(noname1);
    if (noname1->state == -1) {
     goto after_loop;
    }
    s = FSTR_p1752847026(s, L19);
    L22 = (ATTR(self,hot)==((ARRAYBOOL) NULL));
    L231_=!(L22); 
    if (L231_) {
     if (f_L131_) {
      f_L131_ = FALSE;
      L31 = ATTR(self,hot);
      L131_br=L31==NULL?0:ASIZE((ARRAYBOOL)L31); 
      i_L131_=0;
     }
     if(i_L131_>=L131_br)  goto after_loop; 
     aL131_=ARR((ARRAYBOOL)L31,i_L131_); i_L131_++;
     L20 = aL131_;
    } else {
     L20 = FALSE;
    }
    if (L20) {
     plus_self1 = s;
     plus_c1 = '!';
     ret_val1 = FSTR_p399773021(plus_self1, plus_c1);
     s = ret_val1;
    }
    L251_=INTPLUS(L41,1); 
    L41 = L251_;
   }
  }
  after_loop: ;
  plus_self2 = s;
  plus_c2 = ')';
  ret_val2 = FSTR_p399773021(plus_self2, plus_c2);
  s = ret_val2;
 }
 L26 = (ATTR(self,ret)==((dTP) NULL));
 L271_=!(L26); 
 if (L271_) {
  if (ATTR(self,returns_same)) {
   s = FSTR_p1752847026(s, ((STR) &SAME2));
  }
  else {
   plus_self3 = s;
   plus_c3 = ':';
   ret_val3 = FSTR_p399773021(plus_self3, plus_c3);
   L28 = ret_val3;
   L29 = ATTR(self,ret);
   s = FSTR_p1752847026(L28, (*dTP_strrSTR[TAG(L29)])(L29));
  }
 }
 str_self = s;
 ret_val4 = STR_fr1097270334(((STR) NULL), str_self);
 SATTR(self,str,ret_val4);
}


#undef IS_ITER
#define IS_ITER 0

void STAT_incr_STR(STAT1 self, STR k) {
 STAT1 incr_self;
 STR incr_k;
 INT incr_by = INT_zero;
 INT v = INT_zero;
 FMAPSTRINT L1;
 INT L21_;
 incr_self = self;
 incr_k = k;
 incr_by = 1;
 if (FMAPST800079349(ATTR(incr_self,map), incr_k)) {
  v = FMAPST1755966813(ATTR(incr_self,map), incr_k);
  L1 = ATTR(incr_self,map);
  L21_=INTPLUS(v,incr_by); 
  SATTR(incr_self,map,FMAPST951578658(L1, incr_k, L21_));
 }
 else {
  SATTR(incr_self,map,FMAPST951578658(ATTR(incr_self,map), incr_k, incr_by));
 }
}


#undef IS_ITER
#define IS_ITER 0

void STAT_print(STAT1 self) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 FILE1 L1;
 OB L2;
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = STAT_strrSTR(self);
 stdout_self = ((FILE1) NULL);
 L2=ZALLOC(sizeof(struct FILE1_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=FILE1_tag;
 L1 = ((FILE1) L2);
 r = L1;
 
 SATTR(r,fp,stdout);
 ret_val1 = r;
 FILE_plus_STR(ret_val1, plus_s);
}
