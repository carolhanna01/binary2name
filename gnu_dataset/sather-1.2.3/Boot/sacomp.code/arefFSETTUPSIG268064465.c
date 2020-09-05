#include "sather.h"

/* Layouts */

typedef struct dAM_EXPR_struct {
 OB_HEADER header;
 } *dAM_EXPR;

typedef struct dAM_STMT_struct {
 OB_HEADER header;
 } *dAM_STMT;

typedef struct dINLINE_ITER_SIG_struct {
 OB_HEADER header;
 } *dINLINE_ITER_SIG;

typedef struct dINLINE_ROUT_SIG_struct {
 OB_HEADER header;
 } *dINLINE_ROUT_SIG;

typedef struct dLAYOUT_struct {
 OB_HEADER header;
 } *dLAYOUT;

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

typedef struct ARRAYA1870808380_struct {/* layout for ARRAY{ABSTRACT_LAYOUT} */
 OB_HEADER header;
 INT asize;
 struct ABSTRACT_LAYOUT_struct *arr_part[1];
 } *ARRAYA1870808380;

typedef struct ARRAYA967821567_frame_struct {
 INT state;
 ARRAYA1870808380 self;/* Formal argument: self */
 ABSTRACT_LAYOUT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA967821567_frame;

typedef struct ARRAYA855692714_struct {/* layout for ARRAY{AM_BND_CREATE_EXPR} */
 OB_HEADER header;
 INT asize;
 struct AM_BND1124877163_struct *arr_part[1];
 } *ARRAYA855692714;

typedef struct ARRAYA132553625_struct {/* layout for ARRAY{AM_BND_ITER_CALL_EXPR} */
 OB_HEADER header;
 INT asize;
 struct AM_BND367211769_struct *arr_part[1];
 } *ARRAYA132553625;

typedef struct ARRAYAM_CALL_ARG_struct {/* layout for ARRAY{AM_CALL_ARG} */
 OB_HEADER header;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *ARRAYAM_CALL_ARG;

typedef struct ARRAYA44788561_frame_struct {
 INT state;
 ARRAYAM_CALL_ARG self;/* Formal argument: self */
 AM_CALL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA44788561_frame;

typedef struct ARRAYA36280886_struct {/* layout for ARRAY{AM_CURSOR_POS} */
 OB_HEADER header;
 INT asize;
 struct AM_CURSOR_POS_struct *arr_part[1];
 } *ARRAYA36280886;

typedef struct ARRAYAM_CURSOR_struct {/* layout for ARRAY{AM_CURSOR} */
 OB_HEADER header;
 INT asize;
 struct AM_CURSOR_struct *arr_part[1];
 } *ARRAYAM_CURSOR;

typedef struct ARRAYA1347292543_struct {/* layout for ARRAY{AM_FORMAL_ARG} */
 OB_HEADER header;
 INT asize;
 struct AM_FORMAL_ARG_struct *arr_part[1];
 } *ARRAYA1347292543;

typedef struct ARRAYA1315754333_frame_struct {
 INT state;
 ARRAYA1347292543 self;/* Formal argument: self */
 AM_FORMAL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA1315754333_frame;

typedef struct ARRAYARG_struct {/* layout for ARRAY{ARG} */
 OB_HEADER header;
 INT asize;
 struct ARG_struct *arr_part[1];
 } *ARRAYARG;

typedef struct CHANGE_VARS_struct {/* layout for CHANGE_VARS */
 OB_HEADER header;
 struct dAM_STMT_struct *init_hot;
 struct dAM_STMT_struct *init_nonhot;
 struct dAM_STMT_struct *init_parms;
 struct dAM_STMT_struct *init_vars;
 struct ARRAYINT_struct *pind_hot;
 struct ARRAYINT_struct *pind_nonhot;
 struct FLISTA725283029_struct *new_vars;
 struct FLISTA725283029_struct *saved_vars;
 struct FMAPAM1145008811_struct *new_subst;
 struct FMAPAM1145008811_struct *saved_subst;
 } *CHANGE_VARS;

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

typedef struct ELT_EQ773986801_struct {/* layout for ELT_EQ{$INLINE_ITER_SIG} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ773986801;

typedef struct ELT_EQ1957447193_struct {/* layout for ELT_EQ{$INLINE_ROUT_SIG} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ1957447193;

typedef struct ELT_EQdLAYOUT_struct {/* layout for ELT_EQ{$LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQdLAYOUT;

typedef struct ELT_EQdTP_struct {/* layout for ELT_EQ{$TP} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQdTP;

typedef struct ELT_EQTUPSIGSIG_struct {/* layout for ELT_EQ{TUP{SIG,SIG}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQTUPSIGSIG;

typedef struct ELT_HASH_struct {/* layout for ELT_HASH */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_HASH;

typedef struct ELT_NI1131308118_struct {/* layout for ELT_NIL{$INLINE_ITER_SIG} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI1131308118;

typedef struct ELT_NI52152274_struct {/* layout for ELT_NIL{$INLINE_ROUT_SIG} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI52152274;

typedef struct ELT_NILdLAYOUT_struct {/* layout for ELT_NIL{$LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILdLAYOUT;

typedef struct ELT_NILdTP_struct {/* layout for ELT_NIL{$TP} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILdTP;

typedef struct ELT_NILTUPSIGSIG_struct {/* layout for ELT_NIL{TUP{SIG,SIG}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILTUPSIGSIG;

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

typedef struct INLINE_ATTR_READ_struct {/* layout for INLINE_ATTR_READ */
 OB_HEADER header;
 struct dTP_struct *self_tp;
 struct dTP_struct *tp_at;
 struct IDENT_struct at;
 struct SIG_struct *sig1;
 BOOL is_special;
 } *INLINE_ATTR_READ;

typedef struct INLINE1596311811_struct {/* layout for INLINE_ATTR_WRITE */
 OB_HEADER header;
 struct dTP_struct *self_tp;
 struct dTP_struct *tp_at;
 struct IDENT_struct at;
 struct SIG_struct *sig1;
 BOOL is_special;
 } *INLINE1596311811;

typedef struct INLINE1433668901_struct {/* layout for INLINE_GLOBAL_READ */
 OB_HEADER header;
 struct dAM_EXPR_struct *init;
 struct dTP_struct *class_tp;
 struct dTP_struct *tp_at;
 struct AS_TYPE_SPEC_struct *as_type;
 struct IDENT_struct name1;
 struct SIG_struct *sig1;
 BOOL is_const;
 BOOL is_special;
 } *INLINE1433668901;

typedef struct INLINE1455768374_struct {/* layout for INLINE_GLOBAL_WRITE */
 OB_HEADER header;
 struct dAM_EXPR_struct *init;
 struct dTP_struct *class_tp;
 struct dTP_struct *tp_at;
 struct AS_TYPE_SPEC_struct *as_type;
 struct IDENT_struct name1;
 struct SIG_struct *sig1;
 BOOL is_const;
 BOOL is_special;
 } *INLINE1455768374;

typedef struct INLINE_ITER_struct {/* layout for INLINE_ITER */
 OB_HEADER header;
 struct dAM_EXPR_struct *yield_cond;
 struct dAM_EXPR_struct *yield_val;
 struct dAM_STMT_struct *BEFLOOP_code;
 struct dAM_STMT_struct *IINIT_code;
 struct dAM_STMT_struct *ITERM_code;
 struct dAM_STMT_struct *WNOY_code;
 struct dAM_STMT_struct *WYAFT_code;
 struct dAM_STMT_struct *WYBEF_code;
 struct CHANGE_VARS_struct *change_vars;
 struct FLISTA725283029_struct *loop_firsts;
 struct SIG_struct *signature;
 BOOL flat_yield;
 BOOL is_special;
 } *INLINE_ITER;

typedef struct INLINE_ROUT_struct {/* layout for INLINE_ROUT */
 OB_HEADER header;
 struct dAM_EXPR_struct *return_val;
 struct dAM_STMT_struct *code1;
 struct CHANGE_VARS_struct *change_vars;
 struct SIG_struct *signature;
 BOOL is_special;
 } *INLINE_ROUT;

typedef struct INLINE1164904870_struct {/* layout for INLINE_VATTR_WRITE */
 OB_HEADER header;
 struct IDENT_struct at;
 struct SIG_struct *sig1;
 BOOL is_special;
 } *INLINE1164904870;

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

typedef struct SYS_struct {/* layout for SYS */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *SYS;

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

typedef struct TUPAM_1519040877_struct {/* layout for TUP{AM_LOCAL_EXPR,AM_LOCAL_EXPR} */
 struct AM_LOCAL_EXPR_struct *t1;
 struct AM_LOCAL_EXPR_struct *t2;
 } TUPAM_1519040877;
static TUPAM_1519040877 TUPAM_1519040877_zero;

typedef struct TUPAM_1519040877_boxed_struct {
 OB_HEADER header;
 TUPAM_1519040877 immutable_part;
 } *TUPAM_1519040877_boxed;

typedef struct TUPSIGSIG_struct {/* layout for TUP{SIG,SIG} */
 struct SIG_struct *t1;
 struct SIG_struct *t2;
 } TUPSIGSIG;
static TUPSIGSIG TUPSIGSIG_zero;

typedef struct TUPSIGSIG_boxed_struct {
 OB_HEADER header;
 TUPSIGSIG immutable_part;
 } *TUPSIGSIG_boxed;

typedef struct XFORM_CODE_struct {/* layout for XFORM_CODE */
 OB_HEADER header;
 struct dAM_EXPR_struct *return_val;
 struct dAM_EXPR_struct *yield_val;
 struct dAM_STMT_struct *after_yield;
 struct AM_IF_STMT_struct *enclosing_if;
 struct AM_IF_STMT_struct *if_encl_yield;
 struct AM_LOOP_STMT_struct *loop_stmt;
 struct AM_LOOP_STMT_struct *new_loop_stmt;
 struct FLISTdAM_EXPR_struct *calls1;
 struct FMAPAM1145008811_struct *subst;
 BOOL record_calls;
 BOOL true_br_yield;
 BOOL true_branch;
 } *XFORM_CODE;

typedef struct AM_ARRAY_EXPR_struct {/* layout for AM_ARRAY_EXPR */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 INT asize;
 struct dAM_EXPR_struct *arr_part[1];
 } *AM_ARRAY_EXPR;

typedef struct AM_ARR535890289_frame_struct {
 INT state;
 AM_ARRAY_EXPR self;/* Formal argument: self */
 dAM_EXPR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ARR535890289_frame;

typedef struct AM_ARR1990213312_frame_struct {
 INT state;
 AM_ARRAY_EXPR self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ARR1990213312_frame;

typedef struct AM_ARR2088295271_frame_struct {
 INT state;
 AM_ARRAY_EXPR self;/* Formal argument: self */
 dAM_EXPR arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ARR2088295271_frame;

typedef struct AM_ASSIGN_STMT_struct {/* layout for AM_ASSIGN_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *dest;
 struct dAM_EXPR_struct *src1;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_ASSIGN_STMT;

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

typedef struct AM_BND1502113605_frame_struct {
 INT state;
 AM_BND1124877163 self;/* Formal argument: self */
 AM_CALL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_BND1502113605_frame;

typedef struct AM_BND1640868266_frame_struct {
 INT state;
 AM_BND1124877163 self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_BND1640868266_frame;

typedef struct AM_BND2071008937_frame_struct {
 INT state;
 AM_BND1124877163 self;/* Formal argument: self */
 AM_CALL_ARG arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_BND2071008937_frame;

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

typedef struct AM_BND745688768_frame_struct {
 INT state;
 AM_BND367211769 self;/* Formal argument: self */
 AM_CALL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_BND745688768_frame;

typedef struct AM_BND83643009_frame_struct {
 INT state;
 AM_BND367211769 self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_BND83643009_frame;

typedef struct AM_BND23844014_frame_struct {
 INT state;
 AM_BND367211769 self;/* Formal argument: self */
 AM_CALL_ARG arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_BND23844014_frame;

typedef struct AM_BND260301329_struct {/* layout for AM_BND_ROUT_CALL_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *br;
 struct SFILE_ID_struct source1;
 struct TP_ROUT_struct *br_tp;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *AM_BND260301329;

typedef struct AM_BND956869000_frame_struct {
 INT state;
 AM_BND260301329 self;/* Formal argument: self */
 AM_CALL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_BND956869000_frame;

typedef struct AM_BND1037254841_frame_struct {
 INT state;
 AM_BND260301329 self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_BND1037254841_frame;

typedef struct AM_BND235024246_frame_struct {
 INT state;
 AM_BND260301329 self;/* Formal argument: self */
 AM_CALL_ARG arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_BND235024246_frame;

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

typedef struct AM_EXT_CALL_EXPR_struct {/* layout for AM_EXT_CALL_EXPR */
 OB_HEADER header;
 struct IDENT_struct nm;
 struct SFILE_ID_struct source1;
 struct SIG_struct *fun;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *AM_EXT_CALL_EXPR;

typedef struct AM_EXT204419640_frame_struct {
 INT state;
 AM_EXT_CALL_EXPR self;/* Formal argument: self */
 INT arg1;/* Formal argument: beg */
 AM_CALL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ic,I_u_Is; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_EXT204419640_frame;

typedef struct AM_EXT150734657_frame_struct {
 INT state;
 AM_EXT_CALL_EXPR self;/* Formal argument: self */
 AM_CALL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_EXT150734657_frame;

typedef struct AM_EXT1469221262_frame_struct {
 INT state;
 AM_EXT_CALL_EXPR self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_EXT1469221262_frame;

typedef struct AM_EXT872579411_frame_struct {
 INT state;
 AM_EXT_CALL_EXPR self;/* Formal argument: self */
 AM_CALL_ARG arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_EXT872579411_frame;

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

typedef struct AM_ITE235385604_frame_struct {
 INT state;
 AM_ITE1809135850 self;/* Formal argument: self */
 INT arg1;/* Formal argument: beg */
 AM_CALL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ic,I_u_Is; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ITE235385604_frame;

typedef struct AM_ITE35020325_frame_struct {
 INT state;
 AM_ITE1809135850 self;/* Formal argument: self */
 AM_CALL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ITE35020325_frame;

typedef struct AM_ITE1159913222_frame_struct {
 INT state;
 AM_ITE1809135850 self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ITE1159913222_frame;

typedef struct AM_ITE756865079_frame_struct {
 INT state;
 AM_ITE1809135850 self;/* Formal argument: self */
 AM_CALL_ARG arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ITE756865079_frame;

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

typedef struct AM_RETURN_STMT_struct {/* layout for AM_RETURN_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *val1;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_RETURN_STMT;

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

typedef struct AM_ROU1235468764_frame_struct {
 INT state;
 AM_ROUT_DEF self;/* Formal argument: self */
 INT arg1;/* Formal argument: beg */
 AM_FORMAL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ic,I_u_Is; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ROU1235468764_frame;

typedef struct AM_ROU948739923_frame_struct {
 INT state;
 AM_ROUT_DEF self;/* Formal argument: self */
 AM_FORMAL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ROU948739923_frame;

typedef struct AM_ROU1569581452_frame_struct {
 INT state;
 AM_ROUT_DEF self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ROU1569581452_frame;

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

typedef struct ARRAYA945146506_frame_struct {
 INT state;
 ARRAYA855692714 self;/* Formal argument: self */
 AM_BND1124877163 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA945146506_frame;

typedef struct ARRAYA1632293813_frame_struct {
 INT state;
 ARRAYA132553625 self;/* Formal argument: self */
 AM_BND367211769 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA1632293813_frame;

typedef struct ARRAYA537529312_frame_struct {
 INT state;
 ARRAYAM_CALL_ARG self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA537529312_frame;

typedef struct ARRAYA2146369463_frame_struct {
 INT state;
 ARRAYA36280886 self;/* Formal argument: self */
 AM_CURSOR_POS ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA2146369463_frame;

typedef struct ARRAYA900901929_frame_struct {
 INT state;
 ARRAYAM_CURSOR self;/* Formal argument: self */
 AM_CURSOR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA900901929_frame;

typedef struct ARRAYA827580689_frame_struct {
 INT state;
 ARRAYARG self;/* Formal argument: self */
 ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA827580689_frame;

typedef struct ARRAYBOOL_struct {/* layout for ARRAY{BOOL} */
 OB_HEADER header;
 INT asize;
 BOOL arr_part[1];
 } *ARRAYBOOL;

typedef struct ARRAYINT_struct {/* layout for ARRAY{INT} */
 OB_HEADER header;
 INT asize;
 INT arr_part[1];
 } *ARRAYINT;

typedef struct ARRAYI69049459_frame_struct {
 INT state;
 ARRAYINT self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYI69049459_frame;

typedef struct FLISTA725283029_struct {/* layout for FLIST{AM_LOCAL_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_LOCAL_EXPR_struct *arr_part[1];
 } *FLISTA725283029;

typedef struct FMAPAM1145008811_struct {/* layout for FMAP{AM_LOCAL_EXPR,AM_LOCAL_EXPR} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPAM_1519040877_struct arr_part[1];
 } *FMAPAM1145008811;

typedef struct FSETdLAYOUT_struct {/* layout for FSET{$LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct dLAYOUT_struct *arr_part[1];
 } *FSETdLAYOUT;

typedef struct FSETdL416996713_frame_struct {
 INT state;
 FSETdLAYOUT self;/* Formal argument: self */
 dLAYOUT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETdL416996713_frame;

typedef struct FSETdL950804059_frame_struct {
 INT state;
 FSETdLAYOUT self;/* Formal argument: self */
 dLAYOUT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETdL950804059_frame;

typedef struct FSETdL485771726_frame_struct {
 INT state;
 FSETdLAYOUT self;/* Formal argument: self */
 dLAYOUT ret_val1;
 FSETdLAYOUT L61;
 dLAYOUT r;
 INT i;
 INT sz;
 INT L31;
 FSETdLAYOUT is_elt_nil_self;
 dLAYOUT is_elt_nil_e;
 BOOL ret_val;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETdL485771726_frame;

typedef struct FSETdTP_struct {/* layout for FSET{$TP} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct dTP_struct *arr_part[1];
 } *FSETdTP;

typedef struct FSETdT1957050743_frame_struct {
 INT state;
 FSETdTP self;/* Formal argument: self */
 dTP ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETdT1957050743_frame;

typedef struct FSETdT1539121061_frame_struct {
 INT state;
 FSETdTP self;/* Formal argument: self */
 dTP arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETdT1539121061_frame;

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

typedef struct FSETTUPSIGSIG_struct {/* layout for FSET{TUP{SIG,SIG}} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct TUPSIGSIG_struct arr_part[1];
 } *FSETTUPSIGSIG;

typedef struct FSETTU853729645_frame_struct {
 INT state;
 FSETTUPSIGSIG self;/* Formal argument: self */
 TUPSIGSIG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETTU853729645_frame;

typedef struct FSETTU284653231_frame_struct {
 INT state;
 FSETTUPSIGSIG self;/* Formal argument: self */
 TUPSIGSIG arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETTU284653231_frame;

typedef struct FSETTU845419336_frame_struct {
 INT state;
 FSETTUPSIGSIG self;/* Formal argument: self */
 TUPSIGSIG ret_val2;
 FSETTUPSIGSIG L61;
 TUPSIGSIG r;
 INT i;
 INT sz;
 INT L31;
 FSETTUPSIGSIG is_elt_nil_self;
 TUPSIGSIG is_elt_nil_e;
 BOOL ret_val;
 ELT_NILTUPSIGSIG is_elt_nil_self1;
 TUPSIGSIG is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETTU845419336_frame;

typedef struct FSTR_struct {/* layout for FSTR */
 OB_HEADER header;
 INT loc;
 INT asize;
 CHAR arr_part[1];
 } *FSTR;

typedef struct INLINE_ITER_TBL_struct {/* layout for INLINE_ITER_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct dINLINE_ITER_SIG_struct *arr_part[1];
 } *INLINE_ITER_TBL;

typedef struct INLINE546763677_frame_struct {
 INT state;
 INLINE_ITER_TBL self;/* Formal argument: self */
 dINLINE_ITER_SIG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *INLINE546763677_frame;

typedef struct INLINE390133331_frame_struct {
 INT state;
 INLINE_ITER_TBL self;/* Formal argument: self */
 dINLINE_ITER_SIG arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *INLINE390133331_frame;

typedef struct INLINE106514693_frame_struct {
 INT state;
 INLINE_ITER_TBL self;/* Formal argument: self */
 dINLINE_ITER_SIG ret_val1;
 INLINE_ITER_TBL L61;
 dINLINE_ITER_SIG r;
 INT L31;
 INLINE_ITER_TBL is_elt_nil_self;
 dINLINE_ITER_SIG is_elt_nil_e;
 BOOL ret_val;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *INLINE106514693_frame;

typedef struct INLINE_ROUT_TBL_struct {/* layout for INLINE_ROUT_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct dINLINE_ROUT_SIG_struct *arr_part[1];
 } *INLINE_ROUT_TBL;

typedef struct INLINE1519043837_frame_struct {
 INT state;
 INLINE_ROUT_TBL self;/* Formal argument: self */
 dINLINE_ROUT_SIG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *INLINE1519043837_frame;

typedef struct INLINE1362413491_frame_struct {
 INT state;
 INLINE_ROUT_TBL self;/* Formal argument: self */
 dINLINE_ROUT_SIG arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *INLINE1362413491_frame;

typedef struct INLINE600707605_frame_struct {
 INT state;
 INLINE_ROUT_TBL self;/* Formal argument: self */
 dINLINE_ROUT_SIG ret_val1;
 INLINE_ROUT_TBL L61;
 dINLINE_ROUT_SIG r;
 INT L31;
 INLINE_ROUT_TBL is_elt_nil_self;
 dINLINE_ROUT_SIG is_elt_nil_e;
 BOOL ret_val;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *INLINE600707605_frame;

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

extern BOOL FSETTU1642694154;
extern BOOL FSETTU215930357;
extern BOOL FSETdL1534870640;
extern BOOL FSETdL901472145;
extern BOOL FSETdT1052485343;
extern BOOL FSETdT806139168;
extern IDENT IDENT_985337352;
extern INT FSETTU1908122889;
extern INT FSETTU697583666;
extern INT FSETdL1013454714;
extern INT FSETdL344753731;
extern INT FSETdT1655792354;
extern INT FSETdT2053737937;
extern INT INLINE1401026874;
extern INT INLINE1484868694;
extern INT INLINE1516217386;
extern INT INLINE2014794524;
extern INT INLINE29411189;
extern INT INLINE494873411;
extern INT INLINE598259372;

/* Function declarations */


extern RETURNED_CONST BOOL (**dTP_is1334578382)(dTP);

extern RETURNED_CONST BOOL (**dTP_is242312711)(dTP, OB);

extern RETURNED_CONST FLISTA725283029 (**dAM_EX1656723676)(dAM_EXPR);

extern RETURNED_CONST SIG (**dINLIN1366832129)(dINLINE_ROUT_SIG);

extern RETURNED_CONST SIG (**dINLIN1426868953)(dINLINE_ITER_SIG);

extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);

extern RETURNED_CONST dAM_EXPR (**dAM_EX1830308833)(dAM_EXPR);

extern RETURNED_CONST dAM_STMT (**dAM_ST775224435)(dAM_STMT);
ABSTRACT_LAYOUT ARRAYA967821567(ARRAYA967821567_frame);
AM_ARRAY_EXPR AM_ARR1852652918(AM_ARRAY_EXPR);
AM_BND1124877163 AM_BND831738871(AM_BND1124877163);
AM_BND1124877163 ARRAYA945146506(ARRAYA945146506_frame);
AM_BND260301329 AM_BND2052779026(AM_BND260301329);
AM_BND367211769 AM_BND1416432078(AM_BND367211769);
AM_BND367211769 ARRAYA1632293813(ARRAYA1632293813_frame);
AM_CALL_ARG AM_BND1502113605(AM_BND1502113605_frame);
AM_CALL_ARG AM_BND745688768(AM_BND745688768_frame);
AM_CALL_ARG AM_BND956869000(AM_BND956869000_frame);
AM_CALL_ARG AM_CAL1030878588(AM_CALL_ARG);
AM_CALL_ARG AM_EXT150734657(AM_EXT150734657_frame);
AM_CALL_ARG AM_EXT204419640(AM_EXT204419640_frame);
AM_CALL_ARG AM_ITE235385604(AM_ITE235385604_frame);
AM_CALL_ARG AM_ITE35020325(AM_ITE35020325_frame);
AM_CALL_ARG ARRAYA44788561(ARRAYA44788561_frame);
AM_CURSOR ARRAYA900901929(ARRAYA900901929_frame);
AM_CURSOR_POS ARRAYA2146369463(ARRAYA2146369463_frame);
AM_EXT_CALL_EXPR AM_EXT1687283405(AM_EXT_CALL_EXPR);
AM_FORMAL_ARG AM_ROU1235468764(AM_ROU1235468764_frame);
AM_FORMAL_ARG AM_ROU948739923(AM_ROU948739923_frame);
AM_FORMAL_ARG ARRAYA1315754333(ARRAYA1315754333_frame);
AM_ITE1809135850 AM_ITE380500422(AM_ITE1809135850);
ARG ARRAYA827580689(ARRAYA827580689_frame);
ARRAYINT ARRAYI413484363(ARRAYINT);
ARRAYINT SIG_ne271652128(SIG);
BOOL AM_ARR1751046750(AM_ARRAY_EXPR, OB);
BOOL AM_ARR437965311(AM_ARRAY_EXPR);
BOOL AM_BND1565517842(AM_BND367211769);
BOOL AM_BND1588574812(AM_BND1124877163, OB);
BOOL AM_BND1666770893(AM_BND260301329, OB);
BOOL AM_BND1960601142(AM_BND260301329);
BOOL AM_BND48137285(AM_BND367211769, OB);
BOOL AM_BND89426869(AM_BND1124877163, AM_BND1124877163);
BOOL AM_BND9208257(AM_BND1124877163);
BOOL AM_EXT1010597579(AM_EXT_CALL_EXPR);
BOOL AM_EXT1489112584(AM_EXT_CALL_EXPR, OB);
BOOL AM_ITE1429510983(AM_ITE1809135850);
BOOL AM_ITE1901971608(AM_ITE1809135850, OB);
BOOL AM_ROU389945790(AM_ROUT_DEF, AM_LOCAL_EXPR);
BOOL ELT_EQ1393525399(ELT_EQdLAYOUT, dLAYOUT, dLAYOUT);
BOOL ELT_EQ182879965(ELT_EQ773986801, dINLINE_ITER_SIG, dINLINE_ITER_SIG);
BOOL ELT_EQ398237803(ELT_EQ1957447193, dINLINE_ROUT_SIG, dINLINE_ROUT_SIG);
BOOL ELT_NI125473534(ELT_NILdTP, dTP);
BOOL ELT_NI1893940731(ELT_NI52152274, dINLINE_ROUT_SIG);
BOOL ELT_NI569783157(ELT_NI1131308118, dINLINE_ITER_SIG);
BOOL ELT_NI978759818(ELT_NILdLAYOUT, dLAYOUT);
BOOL FSETdL1260128492(FSETdLAYOUT, dLAYOUT);
BOOL FSETdL304031057(FSETdLAYOUT, dLAYOUT);
BOOL FSETdL995777604(FSETdLAYOUT, dLAYOUT);
BOOL FSETdT1179327129(FSETdTP);
BOOL FSETdT1276075755(FSETdTP, dTP);
BOOL FSETdT1764379880(FSETdTP, dTP);
BOOL FSETdT679144660(FSETdTP, dTP);
BOOL INLINE105629928(INLINE_ITER_TBL, SIG, dINLINE_ITER_SIG);
BOOL INLINE1537257477(INLINE_ROUT, AM_ROUT_DEF, INT, PROG);
BOOL INLINE522121224(INLINE_ROUT_TBL, SIG, dINLINE_ROUT_SIG);
BOOL INLINE803584675(INLINE_ITER, AM_ROUT_DEF, INT, PROG);
BOOL SIG_is152200802(SIG);
BOOL SIG_is418491101(SIG);
BOOL STR_is111530248(STR, STR);
BOOL TUPSIG975230438(TUPSIGSIG, TUPSIGSIG);
CHANGE_VARS CHANGE1489663732(CHANGE_VARS, AM_ROUT_DEF);
FLISTA725283029 AM_ARR1917250979(AM_ARRAY_EXPR);
FLISTA725283029 AM_BND1015009410(AM_BND260301329);
FLISTA725283029 AM_BND1086659687(AM_BND1124877163);
FLISTA725283029 AM_BND776534650(AM_BND367211769);
FLISTA725283029 AM_EXT908131195(AM_EXT_CALL_EXPR);
FLISTA725283029 AM_ITE757218595(AM_ITE1809135850);
FLISTA725283029 FLISTA189055531(FLISTA725283029, FLISTA725283029);
FSETTUPSIGSIG FSETTU1002669134(FSETTUPSIGSIG);
FSETTUPSIGSIG FSETTU1359771536(FSETTUPSIGSIG, FSETTUPSIGSIG, TUPSIGSIG);
FSETTUPSIGSIG FSETTU1446784814(FSETTUPSIGSIG);
FSETTUPSIGSIG FSETTU1786855230(FSETTUPSIGSIG, INT);
FSETTUPSIGSIG FSETTU184468031(FSETTUPSIGSIG);
FSETTUPSIGSIG FSETTU2063384524(FSETTUPSIGSIG, TUPSIGSIG);
FSETTUPSIGSIG FSETTU63822064(FSETTUPSIGSIG, FSETTUPSIGSIG, TUPSIGSIG);
FSETTUPSIGSIG FSETTU814351307(FSETTUPSIGSIG, FSETTUPSIGSIG);
FSETdLAYOUT FSETdL1582668907(FSETdLAYOUT, FSETdLAYOUT);
FSETdLAYOUT FSETdL1585396236(FSETdLAYOUT);
FSETdLAYOUT FSETdL1596378552(FSETdLAYOUT, FSETdLAYOUT, dLAYOUT);
FSETdLAYOUT FSETdL25338792(FSETdLAYOUT, INT);
FSETdLAYOUT FSETdL335753900(FSETdLAYOUT);
FSETdLAYOUT FSETdL49659336(FSETdLAYOUT, FSETdLAYOUT, dLAYOUT);
FSETdLAYOUT FSETdL539710251(FSETdLAYOUT);
FSETdLAYOUT FSETdL835744292(FSETdLAYOUT, dLAYOUT);
FSETdTP FSETdT1016814448(FSETdTP, dTP);
FSETdTP FSETdT1256875665(FSETdTP, dTP);
FSETdTP FSETdT1260763896(FSETdTP, FSETdTP, dTP);
FSETdTP FSETdT1290716916(FSETdTP, INT);
FSETdTP FSETdT1327793984(FSETdTP);
FSETdTP FSETdT1537066464(FSETdTP);
FSETdTP FSETdT1713796851(FSETdTP, dTP);
FSETdTP FSETdT1757972683(FSETdTP);
FSETdTP FSETdT1854975287(FSETdTP, FSETdTP);
FSETdTP FSETdT2034766666(FSETdTP, dTP);
FSETdTP FSETdT217460096(FSETdTP);
FSETdTP FSETdT466016120(FSETdTP, FSETdTP, dTP);
FSETdTP FSETdT575601071(FSETdTP, FSETdTP);
FSETdTP FSETdT653600961(FSETdTP, FSETdTP);
FSTR FSTR_c1307336863(FSTR, STR);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
INLINE1433668901 INLINE433490271(INLINE1433668901, AM_ROUT_DEF);
INLINE1455768374 INLINE1362941708(INLINE1455768374, AM_ROUT_DEF);
INLINE_ITER_TBL INLINE1476677165(INLINE_ITER_TBL);
INLINE_ITER_TBL INLINE245315800(INLINE_ITER_TBL, INT);
INLINE_ITER_TBL INLINE263789299(INLINE_ITER_TBL, AM_ROUT_DEF, BOOL, INT, PROG);
INLINE_ITER_TBL INLINE450010170(INLINE_ITER_TBL, dINLINE_ITER_SIG);
INLINE_ITER_TBL INLINE785230726(INLINE_ITER_TBL, INT);
INLINE_ROUT_TBL INLINE1117751299(INLINE_ROUT_TBL);
INLINE_ROUT_TBL INLINE1394857882(INLINE_ROUT_TBL, INT);
INLINE_ROUT_TBL INLINE180230301(INLINE_ROUT_TBL, AM_ROUT_DEF, BOOL, INT, PROG);
INLINE_ROUT_TBL INLINE2122266936(INLINE_ROUT_TBL, INT);
INLINE_ROUT_TBL INLINE525216690(INLINE_ROUT_TBL, dINLINE_ROUT_SIG);
INT AM_ARR191206921(AM_ARRAY_EXPR);
INT AM_ARR1990213312(AM_ARR1990213312_frame);
INT AM_BND1037254841(AM_BND1037254841_frame);
INT AM_BND1144165392(AM_BND260301329);
INT AM_BND1640868266(AM_BND1640868266_frame);
INT AM_BND2097777224(AM_BND367211769);
INT AM_BND472678797(AM_BND1124877163);
INT AM_BND83643009(AM_BND83643009_frame);
INT AM_EXT1469221262(AM_EXT1469221262_frame);
INT AM_EXT644325801(AM_EXT_CALL_EXPR);
INT AM_ITE1021507011(AM_ITE1809135850);
INT AM_ITE1159913222(AM_ITE1159913222_frame);
INT AM_ROU1569581452(AM_ROU1569581452_frame);
INT ARRAYA537529312(ARRAYA537529312_frame);
INT ARRAYI69049459(ARRAYI69049459_frame);
INT ELT_HA1612720024(ELT_HASH, OB);
INT FSETTU1591851819(FSETTUPSIGSIG);
INT FSETdL858951727(FSETdLAYOUT);
INT FSETdTP_sizerINT(FSETdTP);
INT INLINE1047927461(INLINE_ROUT_TBL, SIG);
INT INLINE1816775773(INLINE_ITER_TBL, SIG);
INT INT_hashrINT(INT);
INT INT_hi740923800(INT);
INT STR_sizerINT(STR);
SFILE_ID AM_ARR592984289(AM_ARRAY_EXPR);
SFILE_ID AM_BND1008256842(AM_BND260301329);
SFILE_ID AM_BND1697524322(AM_BND367211769);
SFILE_ID AM_BND1800984858(AM_BND1124877163);
SFILE_ID AM_EXT840500856(AM_EXT_CALL_EXPR);
SFILE_ID AM_ITE834410629(AM_ITE1809135850);
SFILE_ID AM_ROU231894201(AM_ROUT_DEF);
STR ARRAYA1490975097(ARRAYA1870808380);
STR ARRAYA1621913879(ARRAYA855692714);
STR ARRAYA175904446(ARRAYA1347292543);
STR ARRAYA1826145484(ARRAYAM_CALL_ARG);
STR ARRAYA1836477836(ARRAYAM_CURSOR);
STR ARRAYA56089049(ARRAYA36280886);
STR ARRAYA92706474(ARRAYA132553625);
STR STR_ap2004550586(STR, STR);
STR STR_fr1097270334(STR, FSTR);
TUPSIGSIG FSETTU845419336(FSETTU845419336_frame);
TUPSIGSIG FSETTU853729645(FSETTU853729645_frame);
dAM_EXPR AM_ARR535890289(AM_ARR535890289_frame);
dAM_STMT XFORM_1427763452(XFORM_CODE, dAM_STMT);
dINLINE_ITER_SIG ELT_NI1331652151(ELT_NI1131308118);
dINLINE_ITER_SIG INLINE106514693(INLINE106514693_frame);
dINLINE_ITER_SIG INLINE1838056742(INLINE_ITER_TBL, SIG);
dINLINE_ITER_SIG INLINE546763677(INLINE546763677_frame);
dINLINE_ROUT_SIG ELT_NI706507177(ELT_NI52152274);
dINLINE_ROUT_SIG INLINE1519043837(INLINE1519043837_frame);
dINLINE_ROUT_SIG INLINE2019550598(INLINE_ROUT_TBL, SIG);
dINLINE_ROUT_SIG INLINE600707605(INLINE600707605_frame);
dLAYOUT ELT_NI947803854(ELT_NILdLAYOUT);
dLAYOUT FSETdL416996713(FSETdL416996713_frame);
dLAYOUT FSETdL485771726(FSETdL485771726_frame);
dTP AM_ARR1099277377(AM_ARRAY_EXPR);
dTP AM_BND192433970(AM_BND260301329);
dTP AM_BND540727053(AM_BND1124877163);
dTP AM_BND95745606(AM_BND367211769);
dTP AM_EXT195226505(AM_EXT_CALL_EXPR);
dTP AM_ITE597285895(AM_ITE1809135850);
dTP ELT_NI1328289538(ELT_NILdTP);
dTP FSETdT1957050743(FSETdT1957050743_frame);
dTP FSETdTP_eltbrdTP(FSETdTP_eltbrdTP_frame);
void AM_ARR2088295271(AM_ARR2088295271_frame);
void AM_BND2071008937(AM_BND2071008937_frame);
void AM_BND235024246(AM_BND235024246_frame);
void AM_BND23844014(AM_BND23844014_frame);
void AM_EXT872579411(AM_EXT872579411_frame);
void AM_ITE756865079(AM_ITE756865079_frame);
void FILE_plus_STR(FILE1, STR);
void FSETTU284653231(FSETTU284653231_frame);
void FSETdL950804059(FSETdL950804059_frame);
void FSETdT1539121061(FSETdT1539121061_frame);
void INLINE1362413491(INLINE1362413491_frame);
void INLINE15806555(INLINE_ITER, AM_ROUT_DEF);
void INLINE390133331(INLINE390133331_frame);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

AM_ARRAY_EXPR AM_ARR1852652918(AM_ARRAY_EXPR self) {
 AM_ARRAY_EXPR ret_val;
 AM_ARRAY_EXPR r;
 AM_ARRAY_EXPR L11;
 dAM_EXPR x;
 AM_ARRAY_EXPR L21;
 AM_ARRAY_EXPR create_self;
 INT create_nargs = INT_zero;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_ARRAY_EXPR ret_val1;
 AM_ARRAY_EXPR r1;
 INT L31 = INT_zero;
 INT L41_;
 AM_ARRAY_EXPR L5;
 INT L6;
 OB L7;
 INT L8;
 INT L91_br;
 dAM_EXPR aL91_;
 INT L101_br;
INT i_L101_=0;
 dAM_EXPR L12;
 BOOL L13;
 BOOL L141_;
 dAM_EXPR L15;
 INT L161_;
 if ((self==((AM_ARRAY_EXPR) NULL))) {
  ret_val = ((AM_ARRAY_EXPR) NULL);
  return ret_val;
 }
 create_self = ((AM_ARRAY_EXPR) NULL);
 L41_=(self)==NULL?0:ASIZE((AM_ARRAY_EXPR)self); 
 create_nargs = L41_;
 create_source = ATTR(self,source1);
 L6 = create_nargs;
 L8=(sizeof(struct AM_ARRAY_EXPR_struct)-sizeof(dAM_EXPR))+(L6)*sizeof(dAM_EXPR);
 L7=ZALLOC_BIG(L8);
 if (L7==NULL) FATAL("Unable to allocate more memory");
 ((OB)L7)->header.tag=AM_ARRAY_EXPR_tag;
#ifdef DESTROY_CHK

   ((OB)L7)->header.destroyed=0;
#endif

 L5 = ((AM_ARRAY_EXPR) L7);
 L5->asize = L6;
 r1 = L5;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 {
  BOOL f_L91_ = TRUE;
  BOOL f_L101_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = self;
  L21 = r;
  L91_br=L11==NULL?0:ASIZE((AM_ARRAY_EXPR)L11); 
  L101_br=L21==NULL?0:ASIZE((AM_ARRAY_EXPR)L21); 
  i_L101_=0;
  while (1) {
   if(L31>=L91_br)  goto after_loop; 
   aL91_=ARR((AM_ARRAY_EXPR)L11,L31); 
   x = aL91_;
   L13 = (x==((dAM_EXPR) NULL));
   L141_=!(L13); 
   if (L141_) {
    L15 = x;
    x = (*dAM_EX1830308833[TAG(L15)])(L15);
   }
   if(i_L101_>=L101_br)  goto after_loop; 
   SARR((AM_ARRAY_EXPR)L21,i_L101_,(dAM_EXPR)x); i_L101_++;
   ;
   L161_=INTPLUS(L31,1); 
   L31 = L161_;
  }
 }
 after_loop: ;
 SATTR(r,tp_at,ATTR(self,tp_at));
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_BND1124877163 AM_BND831738871(AM_BND1124877163 self) {
 AM_BND1124877163 ret_val;
 AM_BND1124877163 r;
 AM_BND1124877163 create_self;
 INT create_nargs = INT_zero;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_BND1124877163 ret_val1;
 AM_BND1124877163 r1;
 INT L11_;
 AM_BND1124877163 L2;
 INT L3;
 OB L4;
 INT L5;
 BOOL L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 if ((self==((AM_BND1124877163) NULL))) {
  ret_val = ((AM_BND1124877163) NULL);
  return ret_val;
 }
 create_self = ((AM_BND1124877163) NULL);
 L11_=(self)==NULL?0:ASIZE((AM_BND1124877163)self); 
 create_nargs = L11_;
 create_source = ATTR(self,source1);
 L3 = create_nargs;
 L5=(sizeof(struct AM_BND1124877163_struct)-sizeof(AM_CALL_ARG))+(L3)*sizeof(AM_CALL_ARG);
 L4=ZALLOC_BIG(L5);
 if (L4==NULL) FATAL("Unable to allocate more memory");
 ((OB)L4)->header.tag=AM_BND1124877163_tag;
#ifdef DESTROY_CHK

   ((OB)L4)->header.destroyed=0;
#endif

 L2 = ((AM_BND1124877163) L4);
 L2->asize = L3;
 r1 = L2;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 SATTR(r,fun,ATTR(self,fun));
 L6 = (ATTR(self,unbnd_args)==((ARRAYINT) NULL));
 L71_=!(L6); 
 if (L71_) {
  SATTR(r,unbnd_args,ARRAYI413484363(ATTR(self,unbnd_args)));
 }
 L8 = (ATTR(self,bnd_args)==((ARRAYINT) NULL));
 L91_=!(L8); 
 if (L91_) {
  SATTR(r,bnd_args,ARRAYI413484363(ATTR(self,bnd_args)));
 }
 SATTR(r,tp_at,ATTR(self,tp_at));
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_BND260301329 AM_BND2052779026(AM_BND260301329 self) {
 AM_BND260301329 ret_val;
 AM_BND260301329 r;
 AM_BND260301329 L11;
 AM_CALL_ARG x;
 AM_BND260301329 L21;
 AM_BND260301329 create_self;
 INT create_nargs = INT_zero;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_BND260301329 ret_val1;
 AM_BND260301329 r1;
 INT L31 = INT_zero;
 INT L41_;
 AM_BND260301329 L5;
 INT L6;
 OB L7;
 INT L8;
 BOOL L9;
 BOOL L101_;
 dAM_EXPR L12;
 INT L131_br;
 AM_CALL_ARG aL131_;
 INT L141_br;
INT i_L141_=0;
 AM_CALL_ARG L15;
 BOOL L16;
 BOOL L171_;
 INT L181_;
 if ((self==((AM_BND260301329) NULL))) {
  ret_val = ((AM_BND260301329) NULL);
  return ret_val;
 }
 create_self = ((AM_BND260301329) NULL);
 L41_=(self)==NULL?0:ASIZE((AM_BND260301329)self); 
 create_nargs = L41_;
 create_source = ATTR(self,source1);
 L6 = create_nargs;
 L8=(sizeof(struct AM_BND260301329_struct)-sizeof(AM_CALL_ARG))+(L6)*sizeof(AM_CALL_ARG);
 L7=ZALLOC_BIG(L8);
 if (L7==NULL) FATAL("Unable to allocate more memory");
 ((OB)L7)->header.tag=AM_BND260301329_tag;
#ifdef DESTROY_CHK

   ((OB)L7)->header.destroyed=0;
#endif

 L5 = ((AM_BND260301329) L7);
 L5->asize = L6;
 r1 = L5;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 L9 = (ATTR(self,br)==((dAM_EXPR) NULL));
 L101_=!(L9); 
 if (L101_) {
  L12 = ATTR(self,br);
  SATTR(r,br,(*dAM_EX1830308833[TAG(L12)])(L12));
 }
 SATTR(r,br_tp,ATTR(self,br_tp));
 {
  BOOL f_L131_ = TRUE;
  BOOL f_L141_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = self;
  L21 = r;
  L131_br=L11==NULL?0:ASIZE((AM_BND260301329)L11); 
  L141_br=L21==NULL?0:ASIZE((AM_BND260301329)L21); 
  i_L141_=0;
  while (1) {
   if(L31>=L131_br)  goto after_loop; 
   aL131_=ARR((AM_BND260301329)L11,L31); 
   x = aL131_;
   L16 = (x==((AM_CALL_ARG) NULL));
   L171_=!(L16); 
   if (L171_) {
    x = AM_CAL1030878588(x);
   }
   if(i_L141_>=L141_br)  goto after_loop; 
   SARR((AM_BND260301329)L21,i_L141_,(AM_CALL_ARG)x); i_L141_++;
   ;
   L181_=INTPLUS(L31,1); 
   L31 = L181_;
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_BND367211769 AM_BND1416432078(AM_BND367211769 self) {
 AM_BND367211769 ret_val;
 OUT create_self;
 OUT ret_val1;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val2;
 FILE1 r;
 extern STR Itisno2078445089;
 FILE1 L1;
 OB L2;
 create_self = ((OUT) NULL);
 ret_val1 = create_self;
 plus_self = ret_val1;
 plus_s = ((STR) &Itisno2078445089);
 stdout_self = ((FILE1) NULL);
 L2=ZALLOC(sizeof(struct FILE1_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=FILE1_tag;
 L1 = ((FILE1) L2);
 r = L1;
 
 SATTR(r,fp,stdout);
 ret_val2 = r;
 FILE_plus_STR(ret_val2, plus_s);
 ret_val = ((AM_BND367211769) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_EXT_CALL_EXPR AM_EXT1687283405(AM_EXT_CALL_EXPR self) {
 AM_EXT_CALL_EXPR ret_val;
 AM_EXT_CALL_EXPR r;
 AM_EXT_CALL_EXPR L11;
 AM_CALL_ARG x;
 AM_EXT_CALL_EXPR L21;
 AM_EXT_CALL_EXPR create_self;
 INT create_nargs = INT_zero;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_EXT_CALL_EXPR ret_val1;
 AM_EXT_CALL_EXPR r1;
 INT L31 = INT_zero;
 INT L41_;
 AM_EXT_CALL_EXPR L5;
 INT L6;
 OB L7;
 INT L8;
 INT L91_br;
 AM_CALL_ARG aL91_;
 INT L101_br;
INT i_L101_=0;
 AM_CALL_ARG L12;
 BOOL L13;
 BOOL L141_;
 INT L151_;
 if ((self==((AM_EXT_CALL_EXPR) NULL))) {
  ret_val = ((AM_EXT_CALL_EXPR) NULL);
  return ret_val;
 }
 create_self = ((AM_EXT_CALL_EXPR) NULL);
 L41_=(self)==NULL?0:ASIZE((AM_EXT_CALL_EXPR)self); 
 create_nargs = L41_;
 create_source = ATTR(self,source1);
 L6 = create_nargs;
 L8=(sizeof(struct AM_EXT_CALL_EXPR_struct)-sizeof(AM_CALL_ARG))+(L6)*sizeof(AM_CALL_ARG);
 L7=ZALLOC_BIG(L8);
 if (L7==NULL) FATAL("Unable to allocate more memory");
 ((OB)L7)->header.tag=AM_EXT_CALL_EXPR_tag;
#ifdef DESTROY_CHK

   ((OB)L7)->header.destroyed=0;
#endif

 L5 = ((AM_EXT_CALL_EXPR) L7);
 L5->asize = L6;
 r1 = L5;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 SATTR(r,fun,ATTR(self,fun));
 SATTR(r,nm,ATTR(self,nm));
 {
  BOOL f_L91_ = TRUE;
  BOOL f_L101_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = self;
  L21 = r;
  L91_br=L11==NULL?0:ASIZE((AM_EXT_CALL_EXPR)L11); 
  L101_br=L21==NULL?0:ASIZE((AM_EXT_CALL_EXPR)L21); 
  i_L101_=0;
  while (1) {
   if(L31>=L91_br)  goto after_loop; 
   aL91_=ARR((AM_EXT_CALL_EXPR)L11,L31); 
   x = aL91_;
   L13 = (x==((AM_CALL_ARG) NULL));
   L141_=!(L13); 
   if (L141_) {
    x = AM_CAL1030878588(x);
   }
   if(i_L101_>=L101_br)  goto after_loop; 
   SARR((AM_EXT_CALL_EXPR)L21,i_L101_,(AM_CALL_ARG)x); i_L101_++;
   ;
   L151_=INTPLUS(L31,1); 
   L31 = L151_;
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_ITE1809135850 AM_ITE380500422(AM_ITE1809135850 self) {
 AM_ITE1809135850 ret_val;
 OUT create_self;
 OUT ret_val1;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val2;
 FILE1 r;
 extern STR Itisno832547114;
 FILE1 L1;
 OB L2;
 create_self = ((OUT) NULL);
 ret_val1 = create_self;
 plus_self = ret_val1;
 plus_s = ((STR) &Itisno832547114);
 stdout_self = ((FILE1) NULL);
 L2=ZALLOC(sizeof(struct FILE1_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=FILE1_tag;
 L1 = ((FILE1) L2);
 r = L1;
 
 SATTR(r,fp,stdout);
 ret_val2 = r;
 FILE_plus_STR(ret_val2, plus_s);
 ret_val = ((AM_ITE1809135850) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_ARR1751046750(AM_ARRAY_EXPR self, OB a) {
 BOOL ret_val = BOOL_zero;
 ret_val = SYSOBEQ(((OB) self),a);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_ARR437965311(AM_ARRAY_EXPR self) {
 BOOL ret_val = BOOL_zero;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_BND1565517842(AM_BND367211769 self) {
 BOOL ret_val = BOOL_zero;
 AM_BND367211769 tp_self;
 dTP ret_val1;
 dTP L1;
 tp_self = self;
 ret_val1 = ATTR(ATTR(tp_self,bi_tp),ret);
 L1 = ret_val1;
 ret_val = (*dTP_is1334578382[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_BND1588574812(AM_BND1124877163 self, OB a) {
 BOOL ret_val = BOOL_zero;
 ret_val = SYSOBEQ(((OB) self),a);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_BND1666770893(AM_BND260301329 self, OB a) {
 BOOL ret_val = BOOL_zero;
 ret_val = SYSOBEQ(((OB) self),a);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_BND1960601142(AM_BND260301329 self) {
 BOOL ret_val = BOOL_zero;
 AM_BND260301329 tp_self;
 dTP ret_val1;
 dTP L1;
 tp_self = self;
 ret_val1 = ATTR(ATTR(tp_self,br_tp),ret);
 L1 = ret_val1;
 ret_val = (*dTP_is1334578382[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_BND48137285(AM_BND367211769 self, OB a) {
 BOOL ret_val = BOOL_zero;
 ret_val = SYSOBEQ(((OB) self),a);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_BND89426869(AM_BND1124877163 self, AM_BND1124877163 a) {
 BOOL ret_val = BOOL_zero;
 ARRAYINT L11;
 ARRAYINT L21;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val1 = BOOL_zero;
 INT L31 = INT_zero;
 BOOL L4;
 BOOL L5;
 BOOL L6;
 BOOL L71_;
 ARRAYINT L8;
 INT L91_;
 INT L10;
 ARRAYINT L12;
 INT L131_;
 INT L14;
 BOOL L151_;
 BOOL L16;
 BOOL L171_;
 ARRAYINT L18;
 INT L191_;
 INT L20;
 ARRAYINT L22;
 INT L231_;
 INT L24;
 BOOL L251_;
 BOOL L26;
 BOOL L271_;
 INT L281_br;
 INT aL281_;
 INT L291_br;
 INT aL291_;
 INT L30;
 INT L32;
 INT L33;
 INT L34;
 BOOL L351_;
 BOOL L36;
 BOOL L371_;
 INT L381_;
 is_eq_self = ATTR(self,fun);
 is_eq_s = ATTR(a,fun);
 ret_val1 = (is_eq_self==is_eq_s);
 L6 = ret_val1;
 L71_=!(L6); 
 if (L71_) {
  L5 = TRUE;
 } else {
  L8 = ATTR(self,bnd_args);
  L91_=(L8)==NULL?0:ASIZE((ARRAYINT)L8); 
  L10 = L91_;
  L12 = ATTR(a,bnd_args);
  L131_=(L12)==NULL?0:ASIZE((ARRAYINT)L12); 
  L14 = L131_;
  L151_=(L10)==(L14); 
  L16 = L151_;
  L171_=!(L16); 
  L5 = L171_;
 }
 if (L5) {
  L4 = TRUE;
 } else {
  L18 = ATTR(self,unbnd_args);
  L191_=(L18)==NULL?0:ASIZE((ARRAYINT)L18); 
  L20 = L191_;
  L22 = ATTR(a,unbnd_args);
  L231_=(L22)==NULL?0:ASIZE((ARRAYINT)L22); 
  L24 = L231_;
  L251_=(L20)==(L24); 
  L26 = L251_;
  L271_=!(L26); 
  L4 = L271_;
 }
 if (L4) {
  ret_val = FALSE;
  return ret_val;
 }
 {
  BOOL f_L281_ = TRUE;
  BOOL f_L291_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = ATTR(self,bnd_args);
  L21 = ATTRs(a,bnd_args,((ARRAYINT) NULL));
  L281_br=L11==NULL?0:ASIZE((ARRAYINT)L11); 
  L291_br=L21==NULL?0:ASIZE((ARRAYINT)L21); 
  while (1) {
   if(L31>=L281_br)  goto after_loop; 
   aL281_=ARR((ARRAYINT)L11,L31); 
   L32 = aL281_;
   if(L31>=L291_br)  goto after_loop; 
   aL291_=ARR((ARRAYINT)L21,L31); 
   L34 = aL291_;
   L351_=(L32)==(L34); 
   L36 = L351_;
   L371_=!(L36); 
   if (L371_) {
    ret_val = FALSE;
    return ret_val;
   }
   L381_=INTPLUS(L31,1); 
   L31 = L381_;
  }
 }
 after_loop: ;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_BND9208257(AM_BND1124877163 self) {
 BOOL ret_val = BOOL_zero;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_EXT1010597579(AM_EXT_CALL_EXPR self) {
 BOOL ret_val = BOOL_zero;
 AM_EXT_CALL_EXPR tp_self;
 dTP ret_val1;
 dTP L1;
 tp_self = self;
 ret_val1 = ATTR(ATTR(tp_self,fun),ret);
 L1 = ret_val1;
 ret_val = (*dTP_is1334578382[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_EXT1489112584(AM_EXT_CALL_EXPR self, OB a) {
 BOOL ret_val = BOOL_zero;
 ret_val = SYSOBEQ(((OB) self),a);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_ITE1429510983(AM_ITE1809135850 self) {
 BOOL ret_val = BOOL_zero;
 SIG is_near_self;
 BOOL ret_val1 = BOOL_zero;
 BOOL L1;
 BOOL L2;
 BOOL L3;
 dTP L4;
 BOOL L5;
 BOOL L6;
 BOOL L71_;
 is_near_self = ATTR(self,fun);
 if (SIG_is418491101(is_near_self)) {
  L3 = ATTR(ATTR(is_near_self,builtin_info),is_near);
 } else {
  L3 = FALSE;
 }
 if (L3) {
  L2 = TRUE;
 } else {
  L4 = ATTR(is_near_self,ret);
  L2 = (*dTP_is1334578382[TAG(L4)])(L4);
 }
 if (L2) {
  L1 = TRUE;
 } else {
  L6 = (ATTR(is_near_self,opt_info)==((OPT_LOCAL_CALL) NULL));
  L71_=!(L6); 
  if (L71_) {
   L5 = ATTR(ATTR(is_near_self,opt_info),is_near);
  } else {
   L5 = FALSE;
  }
  L1 = L5;
 }
 ret_val1 = L1;
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_ITE1901971608(AM_ITE1809135850 self, OB a) {
 BOOL ret_val = BOOL_zero;
 ret_val = SYSOBEQ(((OB) self),a);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_ROU389945790(AM_ROUT_DEF self, AM_LOCAL_EXPR l) {
 BOOL ret_val = BOOL_zero;
 INT i = INT_zero;
 AM_ROUT_DEF is_iter_self;
 BOOL ret_val1 = BOOL_zero;
 SIG is_iter_self1;
 BOOL ret_val2 = BOOL_zero;
 IDENT is_iter_self2 = IDENT_zero;
 BOOL ret_val3 = BOOL_zero;
 BOOL L1;
 BOOL L2;
 BOOL L3;
 BOOL L41_;
 STR L5;
 INT L6;
 INT L71_;
 INT L8;
 CHAR L91_;
 CHAR L10;
 BOOL L111_;
 BOOL L12;
 BOOL L131_;
 INT L141_;
 INT L15;
 BOOL L161_;
 AM_FORMAL_ARG L171_;
 AM_FORMAL_ARG L18;
 ARRAYBOOL L19;
 BOOL L201_;
 INT L211_;
 is_iter_self = self;
 is_iter_self1 = ATTR(is_iter_self,sig1);
 is_iter_self2 = ATTR(is_iter_self1,name1);
 L3 = (is_iter_self2.str==((STR) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = is_iter_self2.str;
  L6 = STR_sizerINT(is_iter_self2.str);
  L71_=INTMINUS(L6,1); 
  L8 = L71_;
  L91_=ARR((STR)L5,L8); 
  L10 = L91_;
  L111_=L10=='!'; 
  L2 = L111_;
 } else {
  L2 = FALSE;
 }
 ret_val3 = L2;
 ret_val2 = ret_val3;
 ret_val1 = ret_val2;
 L12 = ret_val1;
 L131_=!(L12); 
 if (L131_) {
  L1 = TRUE;
 } else {
  L1 = (ATTR(ATTR(self,sig1),hot)==((ARRAYBOOL) NULL));
 }
 if (L1) {
  ret_val = FALSE;
  return ret_val;
 }
 i = 0;
 while (1) {
  L141_=ASIZE((AM_ROUT_DEF)self); 
  L15 = L141_;
  L161_=(i)<(L15); 
  if (L161_) {
  }
  else {
   goto after_loop;
  }
  L171_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)self,i); 
  L18=L171_;
  if ((ATTR(L18,expr)==l)) {
   L19 = ATTR(ATTR(self,sig1),hot);
   L201_=ARR((ARRAYBOOL)L19,i); 
   ret_val = L201_;
   return ret_val;
  }
  L211_=INTPLUS(i,1); 
  i = L211_;
 }
 after_loop: ;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL FSETdL1260128492(FSETdLAYOUT self, dLAYOUT e) {
 BOOL ret_val = BOOL_zero;
 if ((self==((FSETdLAYOUT) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 if (ATTR(self,use_map)) {
  ret_val = FSETdL304031057(self, e);
  return ret_val;
 }
 else {
  ret_val = FSETdL995777604(self, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL FSETdL304031057(FSETdLAYOUT self, dLAYOUT e) {
 BOOL ret_val = BOOL_zero;
 INT h = INT_zero;
 dLAYOUT te;
 dLAYOUT te1;
 FSETdLAYOUT elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETdLAYOUT is_elt_nil_self;
 dLAYOUT is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 FSETdLAYOUT elt_eq_self;
 dLAYOUT elt_eq_e1;
 dLAYOUT elt_eq_e2;
 BOOL ret_val3 = BOOL_zero;
 FSETdLAYOUT is_elt_nil_self1;
 dLAYOUT is_elt_nil_e1;
 BOOL ret_val4 = BOOL_zero;
 FSETdLAYOUT elt_eq_self1;
 dLAYOUT elt_eq_e11;
 dLAYOUT elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 dLAYOUT L71_;
 INT L81_;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 BOOL L131_;
 dLAYOUT L141_;
 INT L151_;
 elt_hash_self = self;
 elt_hash_e = ((OB) e);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FSETdLAYOUT)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=(dLAYOUT)ARR((FSETdLAYOUT)self,h); 
  te = L71_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  ret_val2 = ELT_NI978759818(((ELT_NILdLAYOUT) NULL), is_elt_nil_e);
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   ret_val3 = ELT_EQ1393525399(((ELT_EQdLAYOUT) NULL), elt_eq_e1, elt_eq_e2);
   if (ret_val3) {
    ret_val = TRUE;
    return ret_val;
   }
  }
  L81_=INTPLUS(h,1); 
  h = L81_;
 }
 after_loop: ;
 L91_=ASIZE((FSETdLAYOUT)self); 
 L10 = L91_;
 L111_=INTMINUS(L10,1); 
 L12 = L111_;
 L131_=(h)==(L12); 
 if (L131_) {
  h = 0;
  while (1) {
   L141_=(dLAYOUT)ARR((FSETdLAYOUT)self,h); 
   te1 = L141_;
   is_elt_nil_self1 = self;
   is_elt_nil_e1 = te1;
   ret_val4 = ELT_NI978759818(((ELT_NILdLAYOUT) NULL), is_elt_nil_e1);
   if (ret_val4) {
    goto after_loop1;
   }
   else {
    elt_eq_self1 = self;
    elt_eq_e11 = te1;
    elt_eq_e21 = e;
    ret_val5 = ELT_EQ1393525399(((ELT_EQdLAYOUT) NULL), elt_eq_e11, elt_eq_e21);
    if (ret_val5) {
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

BOOL FSETdL995777604(FSETdLAYOUT self, dLAYOUT e) {
 BOOL ret_val = BOOL_zero;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETdLAYOUT elt_eq_self;
 dLAYOUT elt_eq_e1;
 dLAYOUT elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 BOOL L11_;
 dLAYOUT L21_;
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
  L21_=(dLAYOUT)ARR((FSETdLAYOUT)self,i); 
  elt_eq_e2 = L21_;
  ret_val1 = ELT_EQ1393525399(((ELT_EQdLAYOUT) NULL), elt_eq_e1, elt_eq_e2);
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

BOOL FSETdT1179327129(FSETdTP self) {
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
 L21_=ASIZE((FSETdTP)self); 
 L3 = L21_;
 L41_=(L3)<(33); 
 L5 = L41_;
 L61_=!(L5); 
 if (L61_) {
  L7 = ATTR(self,hsize);
  L81_=ASIZE((FSETdTP)self); 
  L9 = L81_;
  L101_=INTMINUS(L9,1); 
  L11 = L101_;
  L12 = FSETdT1655792354;
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

BOOL FSETdT1276075755(FSETdTP self, dTP e) {
 BOOL ret_val = BOOL_zero;
 INT h = INT_zero;
 dTP te;
 dTP te1;
 FSETdTP elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETdTP is_elt_nil_self;
 dTP is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 FSETdTP elt_eq_self;
 dTP elt_eq_e1;
 dTP elt_eq_e2;
 BOOL ret_val3 = BOOL_zero;
 ELT_EQdTP elt_eq_self1;
 dTP elt_eq_e11;
 dTP elt_eq_e21;
 BOOL ret_val4 = BOOL_zero;
 FSETdTP is_elt_nil_self1;
 dTP is_elt_nil_e1;
 BOOL ret_val5 = BOOL_zero;
 FSETdTP elt_eq_self2;
 dTP elt_eq_e12;
 dTP elt_eq_e22;
 BOOL ret_val6 = BOOL_zero;
 ELT_EQdTP elt_eq_self3;
 dTP elt_eq_e13;
 dTP elt_eq_e23;
 BOOL ret_val7 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 dTP L71_;
 dTP L8;
 INT L91_;
 INT L101_;
 INT L11;
 INT L121_;
 INT L13;
 BOOL L141_;
 dTP L151_;
 dTP L16;
 INT L171_;
 elt_hash_self = self;
 elt_hash_e = ((OB) e);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FSETdTP)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=(dTP)ARR((FSETdTP)self,h); 
  te = L71_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  ret_val2 = ELT_NI125473534(((ELT_NILdTP) NULL), is_elt_nil_e);
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQdTP) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   L8 = elt_eq_e11;
   ret_val4 = (*dTP_is242312711[TAG(L8)])(L8, ((OB) elt_eq_e21));
   ret_val3 = ret_val4;
   if (ret_val3) {
    ret_val = TRUE;
    return ret_val;
   }
  }
  L91_=INTPLUS(h,1); 
  h = L91_;
 }
 after_loop: ;
 L101_=ASIZE((FSETdTP)self); 
 L11 = L101_;
 L121_=INTMINUS(L11,1); 
 L13 = L121_;
 L141_=(h)==(L13); 
 if (L141_) {
  h = 0;
  while (1) {
   L151_=(dTP)ARR((FSETdTP)self,h); 
   te1 = L151_;
   is_elt_nil_self1 = self;
   is_elt_nil_e1 = te1;
   ret_val5 = ELT_NI125473534(((ELT_NILdTP) NULL), is_elt_nil_e1);
   if (ret_val5) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQdTP) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    L16 = elt_eq_e13;
    ret_val7 = (*dTP_is242312711[TAG(L16)])(L16, ((OB) elt_eq_e23));
    ret_val6 = ret_val7;
    if (ret_val6) {
     ret_val = TRUE;
     return ret_val;
    }
   }
   L171_=INTPLUS(h,1); 
   h = L171_;
  }
  after_loop1: ;
 }
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL FSETdT1764379880(FSETdTP self, dTP e) {
 BOOL ret_val = BOOL_zero;
 if ((self==((FSETdTP) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 if (ATTR(self,use_map)) {
  ret_val = FSETdT1276075755(self, e);
  return ret_val;
 }
 else {
  ret_val = FSETdT679144660(self, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL FSETdT679144660(FSETdTP self, dTP e) {
 BOOL ret_val = BOOL_zero;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETdTP elt_eq_self;
 dTP elt_eq_e1;
 dTP elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQdTP elt_eq_self1;
 dTP elt_eq_e11;
 dTP elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 BOOL L11_;
 dTP L21_;
 dTP L3;
 INT L41_;
 i = 0;
 sz = ATTR(self,hsize);
 while (1) {
  L11_=(i)==(sz); 
  if (L11_) {
   goto after_loop;
  }
  elt_eq_self = self;
  elt_eq_e1 = e;
  L21_=(dTP)ARR((FSETdTP)self,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQdTP) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  L3 = elt_eq_e11;
  ret_val2 = (*dTP_is242312711[TAG(L3)])(L3, ((OB) elt_eq_e21));
  ret_val1 = ret_val2;
  if (ret_val1) {
   ret_val = TRUE;
   return ret_val;
  }
  L41_=INTPLUS(i,1); 
  i = L41_;
 }
 after_loop: ;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL INLINE105629928(INLINE_ITER_TBL self, SIG s, dINLINE_ITER_SIG in11) {
 BOOL ret_val = BOOL_zero;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val1 = BOOL_zero;
 dINLINE_ITER_SIG L1;
 if ((in11==((dINLINE_ITER_SIG) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 L1 = in11;
 is_eq_self = (*dINLIN1426868953[TAG(L1)])(L1);
 is_eq_s = s;
 ret_val1 = (is_eq_self==is_eq_s);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL INLINE522121224(INLINE_ROUT_TBL self, SIG s, dINLINE_ROUT_SIG in11) {
 BOOL ret_val = BOOL_zero;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val1 = BOOL_zero;
 dINLINE_ROUT_SIG L1;
 if ((in11==((dINLINE_ROUT_SIG) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 L1 = in11;
 is_eq_self = (*dINLIN1366832129[TAG(L1)])(L1);
 is_eq_s = s;
 ret_val1 = (is_eq_self==is_eq_s);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_ARR1917250979(AM_ARRAY_EXPR self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_BND1015009410(AM_BND260301329 self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_BND1086659687(AM_BND1124877163 self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_BND776534650(AM_BND367211769 self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_EXT908131195(AM_EXT_CALL_EXPR self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_ITE757218595(AM_ITE1809135850 self) {
 FLISTA725283029 ret_val;
 ARRAYINT na;
 FLISTA725283029 nd = ((FLISTA725283029) NULL);
 ARRAYINT L11;
 INT L21 = INT_zero;
 AM_CALL_ARG near_depends_on_;
 FLISTA725283029 ret_val1;
 INT L31_br;
 INT aL31_;
 INT L4;
 INT L5;
 AM_CALL_ARG L61_;
 dAM_EXPR L7;
 INT L81_;
 na = SIG_ne271652128(ATTR(self,fun));
 if ((na==((ARRAYINT) NULL))) {
  ret_val = ((FLISTA725283029) NULL);
  return ret_val;
 }
 {
  BOOL f_L31_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = na;
  L31_br=L11==NULL?0:ASIZE((ARRAYINT)L11); 
  while (1) {
   if(L21>=L31_br)  goto after_loop; 
   aL31_=ARR((ARRAYINT)L11,L21); 
   L5 = aL31_;
   L61_=(AM_CALL_ARG)ARR((AM_ITE1809135850)self,L5); 
   near_depends_on_ = L61_;
   L7 = ATTR(near_depends_on_,expr);
   ret_val1 = (*dAM_EX1656723676[TAG(L7)])(L7);
   nd = FLISTA189055531(nd, ret_val1);
   L81_=INTPLUS(L21,1); 
   L21 = L81_;
  }
 }
 after_loop: ;
 ret_val = nd;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETTUPSIGSIG FSETTU1002669134(FSETTUPSIGSIG self) {
 FSETTUPSIGSIG ret_val;
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
  L11_=ASIZE((FSETTUPSIGSIG)self); 
  L2 = L11_;
  L3 = ATTR(self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FSETTU697583666;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  if (L91_) {
   ret_val = FSETTU184468031(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
 else {
  L10 = ATTR(self,hsize);
  L121_=ASIZE((FSETTUPSIGSIG)self); 
  L13 = L121_;
  L141_=(L10)<(L13); 
  L15 = L141_;
  L161_=!(L15); 
  if (L161_) {
   ret_val = FSETTU184468031(self);
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

FSETTUPSIGSIG FSETTU1359771536(FSETTUPSIGSIG self, FSETTUPSIGSIG r, TUPSIGSIG e) {
 FSETTUPSIGSIG ret_val;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 TUPSIGSIG te = TUPSIGSIG_zero;
 TUPSIGSIG te1 = TUPSIGSIG_zero;
 FSETTUPSIGSIG elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETTUPSIGSIG is_elt_nil_self;
 TUPSIGSIG is_elt_nil_e = TUPSIGSIG_zero;
 BOOL ret_val2 = BOOL_zero;
 ELT_NILTUPSIGSIG is_elt_nil_self1;
 TUPSIGSIG is_elt_nil_e1 = TUPSIGSIG_zero;
 BOOL ret_val3 = BOOL_zero;
 FSETTUPSIGSIG elt_eq_self;
 TUPSIGSIG elt_eq_e1 = TUPSIGSIG_zero;
 TUPSIGSIG elt_eq_e2 = TUPSIGSIG_zero;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQTUPSIGSIG elt_eq_self1;
 TUPSIGSIG elt_eq_e11 = TUPSIGSIG_zero;
 TUPSIGSIG elt_eq_e21 = TUPSIGSIG_zero;
 BOOL ret_val5 = BOOL_zero;
 FSETTUPSIGSIG is_elt_nil_self2;
 TUPSIGSIG is_elt_nil_e2 = TUPSIGSIG_zero;
 BOOL ret_val6 = BOOL_zero;
 ELT_NILTUPSIGSIG is_elt_nil_self3;
 TUPSIGSIG is_elt_nil_e3 = TUPSIGSIG_zero;
 BOOL ret_val7 = BOOL_zero;
 FSETTUPSIGSIG elt_eq_self2;
 TUPSIGSIG elt_eq_e12 = TUPSIGSIG_zero;
 TUPSIGSIG elt_eq_e22 = TUPSIGSIG_zero;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQTUPSIGSIG elt_eq_self3;
 TUPSIGSIG elt_eq_e13 = TUPSIGSIG_zero;
 TUPSIGSIG elt_eq_e23 = TUPSIGSIG_zero;
 BOOL ret_val9 = BOOL_zero;
 INT L11_;
 OB L2;
 OB L3;
 INT L4;
 INT L51_;
 INT L6;
 INT L71_;
 TUPSIGSIG L81_;
 INT L101_;
 INT L121_;
 INT L13;
 BOOL L141_;
 TUPSIGSIG L151_;
 INT L171_;
 INT L19;
 INT L201_;
 L11_=ASIZE((FSETTUPSIGSIG)r); 
 asz = L11_;
 elt_hash_self = r;
 L3=ZALLOC(sizeof(struct TUPSIGSIG_boxed_struct));
 if (L3==NULL) FATAL("Unable to allocate more memory");
 ((OB)L3)->header.tag=TUPSIGSIG_tag;
 L2 = (OB)((TUPSIGSIG_boxed) L3);
 ((TUPSIGSIG_boxed) L2)->immutable_part = e;
 elt_hash_e = L2;
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L4 = ret_val1;
 L51_=INTMINUS(asz,2); 
 L6 = L51_;
 L71_=L4&L6; 
 orig_h = L71_;
 h = orig_h;
 while (1) {
  L81_=ARR((FSETTUPSIGSIG)r,h); 
  te = L81_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NILTUPSIGSIG) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1.t1==(SIG)0)&&(is_elt_nil_e1.t2==(SIG)0);
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQTUPSIGSIG) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   ret_val5 = TUPSIG975230438(elt_eq_e11, elt_eq_e21);
   ret_val4 = ret_val5;
   if (ret_val4) {
    SARR((FSETTUPSIGSIG)r,h,e); 
    ;
    ret_val = r;
    return ret_val;
   }
  }
  L101_=INTPLUS(h,1); 
  h = L101_;
 }
 after_loop: ;
 L121_=INTMINUS(asz,1); 
 L13 = L121_;
 L141_=(h)==(L13); 
 if (L141_) {
  h = 0;
  while (1) {
   L151_=ARR((FSETTUPSIGSIG)r,h); 
   te1 = L151_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NILTUPSIGSIG) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val7 = (is_elt_nil_e3.t1==(SIG)0)&&(is_elt_nil_e3.t2==(SIG)0);
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQTUPSIGSIG) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    ret_val9 = TUPSIG975230438(elt_eq_e13, elt_eq_e23);
    ret_val8 = ret_val9;
    if (ret_val8) {
     SARR((FSETTUPSIGSIG)r,h,e); 
     ;
     ret_val = r;
     return ret_val;
    }
   }
   L171_=INTPLUS(h,1); 
   h = L171_;
  }
  after_loop1: ;
 }
 SARR((FSETTUPSIGSIG)r,h,e); 
 ;
 L19 = ATTR(r,hsize);
 L201_=INTPLUS(L19,1); 
 SATTR(r,hsize,L201_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETTUPSIGSIG FSETTU1446784814(FSETTUPSIGSIG self) {
 FSETTUPSIGSIG ret_val;
 FSETTUPSIGSIG r = ((FSETTUPSIGSIG) NULL);
 FSETTUPSIGSIG L11;
 TUPSIGSIG L2;
 {
  struct FSETTU845419336_frame_struct other1_0;
FSETTU845419336_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FSETTU845419336(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETTU2063384524(r, L2);
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETTUPSIGSIG FSETTU1786855230(FSETTUPSIGSIG self, INT n) {
 FSETTUPSIGSIG ret_val;
 FSETTUPSIGSIG r;
 FSETTUPSIGSIG L11;
 FSETTUPSIGSIG elt_nil_self;
 TUPSIGSIG ret_val1 = TUPSIGSIG_zero;
 ELT_NILTUPSIGSIG elt_nil_self1;
 TUPSIGSIG ret_val2 = TUPSIGSIG_zero;
 TUPSIGSIG e = TUPSIGSIG_zero;
 FSETTUPSIGSIG elt_nil_self2;
 TUPSIGSIG ret_val3 = TUPSIGSIG_zero;
 ELT_NILTUPSIGSIG elt_nil_self3;
 TUPSIGSIG ret_val4 = TUPSIGSIG_zero;
 TUPSIGSIG e1 = TUPSIGSIG_zero;
 INT L21 = INT_zero;
 FSETTUPSIGSIG L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TUPSIGSIG L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct FSETTUPSIGSIG_struct)-sizeof(TUPSIGSIG))+(L4)*sizeof(TUPSIGSIG);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FSETTUPSIGSIG_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FSETTUPSIGSIG) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NILTUPSIGSIG) NULL);
 ret_val2 = TUPSIGSIG_zero;
 ret_val1 = ret_val2;
 L7 = (ret_val1.t1==(SIG)0)&&(ret_val1.t2==(SIG)0);
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FSETTUPSIGSIG)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NILTUPSIGSIG) NULL);
    ret_val4 = TUPSIGSIG_zero;
    ret_val3 = ret_val4;
    L10 = ret_val3;
    SARR((FSETTUPSIGSIG)L11,L21,L10); 
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

FSETTUPSIGSIG FSETTU184468031(FSETTUPSIGSIG self) {
 FSETTUPSIGSIG ret_val;
 FSETTUPSIGSIG r;
 FSETTUPSIGSIG L11;
 FSETTUPSIGSIG switch_structure;
 BOOL switch_structure1 = BOOL_zero;
 BOOL switch_structure2 = BOOL_zero;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPSIGSIG L9;
 L21_=ASIZE((FSETTUPSIGSIG)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = FSETTU1786855230(self, L81_);
 switch_structure = r;
 switch_structure1 = ATTR(self,use_map);
 switch_structure2 = TRUE;
 if (FSETTU1642694154) {
  SATTR(switch_structure,use_map,switch_structure2);
 }
 else {
  SATTR(switch_structure,use_map,switch_structure1);
 }
 {
  struct FSETTU845419336_frame_struct other1_0;
FSETTU845419336_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETTU845419336(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETTU2063384524(r, L9);
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

FSETTUPSIGSIG FSETTU2063384524(FSETTUPSIGSIG self, TUPSIGSIG e) {
 FSETTUPSIGSIG ret_val;
 FSETTUPSIGSIG r;
 FSETTUPSIGSIG set_initial_stru;
 r = self;
 if ((r==((FSETTUPSIGSIG) NULL))) {
  r = FSETTU1786855230(self, FSETTU1908122889);
  set_initial_stru = r;
  SATTR(set_initial_stru,use_map,FSETTU215930357);
 }
 else {
  r = FSETTU1002669134(self);
 }
 if (ATTR(r,use_map)) {
  ret_val = FSETTU1359771536(self, r, e);
  return ret_val;
 }
 else {
  ret_val = FSETTU63822064(self, r, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETTUPSIGSIG FSETTU63822064(FSETTUPSIGSIG self, FSETTUPSIGSIG r, TUPSIGSIG e) {
 FSETTUPSIGSIG ret_val;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETTUPSIGSIG elt_eq_self;
 TUPSIGSIG elt_eq_e1 = TUPSIGSIG_zero;
 TUPSIGSIG elt_eq_e2 = TUPSIGSIG_zero;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQTUPSIGSIG elt_eq_self1;
 TUPSIGSIG elt_eq_e11 = TUPSIGSIG_zero;
 TUPSIGSIG elt_eq_e21 = TUPSIGSIG_zero;
 BOOL ret_val2 = BOOL_zero;
 BOOL L11_;
 TUPSIGSIG L21_;
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
  L21_=ARR((FSETTUPSIGSIG)r,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQTUPSIGSIG) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  ret_val2 = TUPSIG975230438(elt_eq_e11, elt_eq_e21);
  ret_val1 = ret_val2;
  if (ret_val1) {
   SARR((FSETTUPSIGSIG)r,i,e); 
   ;
   ret_val = r;
   return ret_val;
  }
  L41_=INTPLUS(i,1); 
  i = L41_;
 }
 after_loop: ;
 L5 = ATTR(r,hsize);
 SARR((FSETTUPSIGSIG)r,L5,e); 
 ;
 L7 = ATTR(r,hsize);
 L81_=INTPLUS(L7,1); 
 SATTR(r,hsize,L81_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETTUPSIGSIG FSETTU814351307(FSETTUPSIGSIG self, FSETTUPSIGSIG s) {
 FSETTUPSIGSIG ret_val;
 FSETTUPSIGSIG r;
 FSETTUPSIGSIG L11;
 TUPSIGSIG L2;
 r = self;
 {
  struct FSETTU845419336_frame_struct other1_0;
FSETTU845419336_frame noname1 = &other1_0;
  L11 = s;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FSETTU845419336(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETTU2063384524(r, L2);
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETdLAYOUT FSETdL1582668907(FSETdLAYOUT self, FSETdLAYOUT s) {
 FSETdLAYOUT ret_val;
 FSETdLAYOUT r;
 FSETdLAYOUT L11;
 dLAYOUT L2;
 r = self;
 {
  struct FSETdL485771726_frame_struct other1_0;
FSETdL485771726_frame noname1 = &other1_0;
  L11 = s;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FSETdL485771726(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETdL835744292(r, L2);
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETdLAYOUT FSETdL1585396236(FSETdLAYOUT self) {
 FSETdLAYOUT ret_val;
 FSETdLAYOUT r = ((FSETdLAYOUT) NULL);
 FSETdLAYOUT L11;
 dLAYOUT L2;
 {
  struct FSETdL485771726_frame_struct other1_0;
FSETdL485771726_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FSETdL485771726(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETdL835744292(r, L2);
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETdLAYOUT FSETdL1596378552(FSETdLAYOUT self, FSETdLAYOUT r, dLAYOUT e) {
 FSETdLAYOUT ret_val;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETdLAYOUT elt_eq_self;
 dLAYOUT elt_eq_e1;
 dLAYOUT elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 BOOL L11_;
 dLAYOUT L21_;
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
  L21_=(dLAYOUT)ARR((FSETdLAYOUT)r,i); 
  elt_eq_e2 = L21_;
  ret_val1 = ELT_EQ1393525399(((ELT_EQdLAYOUT) NULL), elt_eq_e1, elt_eq_e2);
  if (ret_val1) {
   SARR((FSETdLAYOUT)r,i,(dLAYOUT)e); 
   ;
   ret_val = r;
   return ret_val;
  }
  L41_=INTPLUS(i,1); 
  i = L41_;
 }
 after_loop: ;
 L5 = ATTR(r,hsize);
 SARR((FSETdLAYOUT)r,L5,(dLAYOUT)e); 
 ;
 L7 = ATTR(r,hsize);
 L81_=INTPLUS(L7,1); 
 SATTR(r,hsize,L81_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETdLAYOUT FSETdL25338792(FSETdLAYOUT self, INT n) {
 FSETdLAYOUT ret_val;
 FSETdLAYOUT r;
 FSETdLAYOUT L11;
 FSETdLAYOUT elt_nil_self;
 dLAYOUT ret_val1;
 FSETdLAYOUT elt_nil_self1;
 dLAYOUT ret_val2;
 INT L21 = INT_zero;
 FSETdLAYOUT L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 dLAYOUT L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct FSETdLAYOUT_struct)-sizeof(dLAYOUT))+(L4)*sizeof(dLAYOUT);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FSETdLAYOUT_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FSETdLAYOUT) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 ret_val1 = ELT_NI947803854(((ELT_NILdLAYOUT) NULL));
 L7 = (ret_val1==((dLAYOUT) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FSETdLAYOUT)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self1 = self;
    ret_val2 = ELT_NI947803854(((ELT_NILdLAYOUT) NULL));
    L10 = ret_val2;
    SARR((FSETdLAYOUT)L11,L21,(dLAYOUT)L10); 
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

FSETdLAYOUT FSETdL335753900(FSETdLAYOUT self) {
 FSETdLAYOUT ret_val;
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
  L11_=ASIZE((FSETdLAYOUT)self); 
  L2 = L11_;
  L3 = ATTR(self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FSETdL1013454714;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  if (L91_) {
   ret_val = FSETdL539710251(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
 else {
  L10 = ATTR(self,hsize);
  L121_=ASIZE((FSETdLAYOUT)self); 
  L13 = L121_;
  L141_=(L10)<(L13); 
  L15 = L141_;
  L161_=!(L15); 
  if (L161_) {
   ret_val = FSETdL539710251(self);
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

FSETdLAYOUT FSETdL49659336(FSETdLAYOUT self, FSETdLAYOUT r, dLAYOUT e) {
 FSETdLAYOUT ret_val;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 dLAYOUT te;
 dLAYOUT te1;
 FSETdLAYOUT elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETdLAYOUT is_elt_nil_self;
 dLAYOUT is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 FSETdLAYOUT elt_eq_self;
 dLAYOUT elt_eq_e1;
 dLAYOUT elt_eq_e2;
 BOOL ret_val3 = BOOL_zero;
 FSETdLAYOUT is_elt_nil_self1;
 dLAYOUT is_elt_nil_e1;
 BOOL ret_val4 = BOOL_zero;
 FSETdLAYOUT elt_eq_self1;
 dLAYOUT elt_eq_e11;
 dLAYOUT elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 INT L11_;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 dLAYOUT L61_;
 INT L81_;
 INT L91_;
 INT L10;
 BOOL L121_;
 dLAYOUT L131_;
 INT L151_;
 INT L17;
 INT L181_;
 L11_=ASIZE((FSETdLAYOUT)r); 
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
  L61_=(dLAYOUT)ARR((FSETdLAYOUT)r,h); 
  te = L61_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  ret_val2 = ELT_NI978759818(((ELT_NILdLAYOUT) NULL), is_elt_nil_e);
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   ret_val3 = ELT_EQ1393525399(((ELT_EQdLAYOUT) NULL), elt_eq_e1, elt_eq_e2);
   if (ret_val3) {
    SARR((FSETdLAYOUT)r,h,(dLAYOUT)e); 
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
   L131_=(dLAYOUT)ARR((FSETdLAYOUT)r,h); 
   te1 = L131_;
   is_elt_nil_self1 = self;
   is_elt_nil_e1 = te1;
   ret_val4 = ELT_NI978759818(((ELT_NILdLAYOUT) NULL), is_elt_nil_e1);
   if (ret_val4) {
    goto after_loop1;
   }
   else {
    elt_eq_self1 = self;
    elt_eq_e11 = te1;
    elt_eq_e21 = e;
    ret_val5 = ELT_EQ1393525399(((ELT_EQdLAYOUT) NULL), elt_eq_e11, elt_eq_e21);
    if (ret_val5) {
     SARR((FSETdLAYOUT)r,h,(dLAYOUT)e); 
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
 SARR((FSETdLAYOUT)r,h,(dLAYOUT)e); 
 ;
 L17 = ATTR(r,hsize);
 L181_=INTPLUS(L17,1); 
 SATTR(r,hsize,L181_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETdLAYOUT FSETdL539710251(FSETdLAYOUT self) {
 FSETdLAYOUT ret_val;
 FSETdLAYOUT r;
 FSETdLAYOUT L11;
 FSETdLAYOUT switch_structure;
 BOOL switch_structure1 = BOOL_zero;
 BOOL switch_structure2 = BOOL_zero;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 dLAYOUT L9;
 L21_=ASIZE((FSETdLAYOUT)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = FSETdL25338792(self, L81_);
 switch_structure = r;
 switch_structure1 = ATTR(self,use_map);
 switch_structure2 = TRUE;
 if (FSETdL1534870640) {
  SATTR(switch_structure,use_map,switch_structure2);
 }
 else {
  SATTR(switch_structure,use_map,switch_structure1);
 }
 {
  struct FSETdL485771726_frame_struct other1_0;
FSETdL485771726_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETdL485771726(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETdL835744292(r, L9);
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

FSETdLAYOUT FSETdL835744292(FSETdLAYOUT self, dLAYOUT e) {
 FSETdLAYOUT ret_val;
 FSETdLAYOUT r;
 FSETdLAYOUT set_initial_stru;
 r = self;
 if ((r==((FSETdLAYOUT) NULL))) {
  r = FSETdL25338792(self, FSETdL344753731);
  set_initial_stru = r;
  SATTR(set_initial_stru,use_map,FSETdL901472145);
 }
 else {
  r = FSETdL335753900(self);
 }
 if (ATTR(r,use_map)) {
  ret_val = FSETdL49659336(self, r, e);
  return ret_val;
 }
 else {
  ret_val = FSETdL1596378552(self, r, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETdTP FSETdT1016814448(FSETdTP self, dTP e) {
 FSETdTP ret_val;
 FSETdTP r;
 FSETdTP set_initial_stru;
 r = self;
 if ((r==((FSETdTP) NULL))) {
  r = FSETdT1290716916(self, FSETdT2053737937);
  set_initial_stru = r;
  SATTR(set_initial_stru,use_map,FSETdT1052485343);
 }
 else {
  r = FSETdT1537066464(self);
 }
 if (ATTR(r,use_map)) {
  ret_val = FSETdT466016120(self, r, e);
  return ret_val;
 }
 else {
  ret_val = FSETdT1260763896(self, r, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETdTP FSETdT1256875665(FSETdTP self, dTP e) {
 FSETdTP ret_val;
 INT delete_elt_ind = INT_zero;
 INT hash_table_size = INT_zero;
 INT i = INT_zero;
 INT empty_loc = INT_zero;
 INT second_to_last_i = INT_zero;
 INT next = INT_zero;
 FSETdTP elt_eq_self;
 dTP elt_eq_e1;
 dTP elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQdTP elt_eq_self1;
 dTP elt_eq_e11;
 dTP elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 BOOL L11_;
 BOOL L2;
 BOOL L31_;
 dTP L41_;
 dTP L5;
 INT L61_;
 BOOL L7;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 INT L12;
 BOOL L131_;
 INT L14;
 INT L151_;
 BOOL L161_;
 INT L171_;
 dTP L181_;
 dTP L19;
 INT L21;
 INT L221_;
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
  L41_=(dTP)ARR((FSETdTP)self,i); 
  elt_eq_e2 = L41_;
  elt_eq_self1 = ((ELT_EQdTP) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  L5 = elt_eq_e11;
  ret_val2 = (*dTP_is242312711[TAG(L5)])(L5, ((OB) elt_eq_e21));
  ret_val1 = ret_val2;
  if (ret_val1) {
   delete_elt_ind = i;
   goto after_loop;
  }
  L61_=INTPLUS(i,1); 
  i = L61_;
 }
 after_loop: ;
 L81_=(delete_elt_ind)<(0); 
 L9 = L81_;
 L101_=!(L9); 
 if (L101_) {
  L12 = ATTR(self,hsize);
  L131_=(delete_elt_ind)<(L12); 
  L7 = L131_;
 } else {
  L7 = FALSE;
 }
 if (L7) {
  empty_loc = delete_elt_ind;
  L14 = ATTR(self,hsize);
  L151_=INTMINUS(L14,2); 
  second_to_last_i = L151_;
  while (1) {
   L161_=(second_to_last_i)<(empty_loc); 
   if (L161_) {
    goto after_loop1;
   }
   L171_=INTPLUS(empty_loc,1); 
   next = L171_;
   L181_=(dTP)ARR((FSETdTP)self,next); 
   L19 = L181_;
   SARR((FSETdTP)self,empty_loc,(dTP)L19); 
   ;
   empty_loc = next;
  }
  after_loop1: ;
  L21 = ATTR(self,hsize);
  L221_=INTMINUS(L21,1); 
  SATTR(self,hsize,L221_);
 }
 ret_val = self;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETdTP FSETdT1260763896(FSETdTP self, FSETdTP r, dTP e) {
 FSETdTP ret_val;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETdTP elt_eq_self;
 dTP elt_eq_e1;
 dTP elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQdTP elt_eq_self1;
 dTP elt_eq_e11;
 dTP elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 BOOL L11_;
 dTP L21_;
 dTP L3;
 INT L51_;
 INT L6;
 INT L8;
 INT L91_;
 i = 0;
 sz = ATTR(r,hsize);
 while (1) {
  L11_=(i)==(sz); 
  if (L11_) {
   goto after_loop;
  }
  elt_eq_self = self;
  elt_eq_e1 = e;
  L21_=(dTP)ARR((FSETdTP)r,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQdTP) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  L3 = elt_eq_e11;
  ret_val2 = (*dTP_is242312711[TAG(L3)])(L3, ((OB) elt_eq_e21));
  ret_val1 = ret_val2;
  if (ret_val1) {
   SARR((FSETdTP)r,i,(dTP)e); 
   ;
   ret_val = r;
   return ret_val;
  }
  L51_=INTPLUS(i,1); 
  i = L51_;
 }
 after_loop: ;
 L6 = ATTR(r,hsize);
 SARR((FSETdTP)r,L6,(dTP)e); 
 ;
 L8 = ATTR(r,hsize);
 L91_=INTPLUS(L8,1); 
 SATTR(r,hsize,L91_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETdTP FSETdT1290716916(FSETdTP self, INT n) {
 FSETdTP ret_val;
 FSETdTP r;
 FSETdTP L11;
 FSETdTP elt_nil_self;
 dTP ret_val1;
 FSETdTP elt_nil_self1;
 dTP ret_val2;
 INT L21 = INT_zero;
 FSETdTP L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 dTP L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct FSETdTP_struct)-sizeof(dTP))+(L4)*sizeof(dTP);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FSETdTP_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FSETdTP) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 ret_val1 = ELT_NI1328289538(((ELT_NILdTP) NULL));
 L7 = (ret_val1==((dTP) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FSETdTP)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self1 = self;
    ret_val2 = ELT_NI1328289538(((ELT_NILdTP) NULL));
    L10 = ret_val2;
    SARR((FSETdTP)L11,L21,(dTP)L10); 
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

FSETdTP FSETdT1327793984(FSETdTP self) {
 FSETdTP ret_val;
 FSETdTP r;
 FSETdTP L11;
 FSETdTP switch_structure;
 BOOL switch_structure1 = BOOL_zero;
 BOOL switch_structure2 = BOOL_zero;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 dTP L9;
 L21_=ASIZE((FSETdTP)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTDIV(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = FSETdT1290716916(self, L81_);
 switch_structure = r;
 switch_structure1 = ATTR(self,use_map);
 switch_structure2 = TRUE;
 if (FSETdT806139168) {
  SATTR(switch_structure,use_map,switch_structure2);
 }
 else {
  SATTR(switch_structure,use_map,switch_structure1);
 }
 {
  struct FSETdTP_eltbrdTP_frame_struct other1_0;
FSETdTP_eltbrdTP_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETdTP_eltbrdTP(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETdT1016814448(r, L9);
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

FSETdTP FSETdT1537066464(FSETdTP self) {
 FSETdTP ret_val;
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
  L11_=ASIZE((FSETdTP)self); 
  L2 = L11_;
  L3 = ATTR(self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FSETdT1655792354;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  if (L91_) {
   ret_val = FSETdT1757972683(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
 else {
  L10 = ATTR(self,hsize);
  L121_=ASIZE((FSETdTP)self); 
  L13 = L121_;
  L141_=(L10)<(L13); 
  L15 = L141_;
  L161_=!(L15); 
  if (L161_) {
   ret_val = FSETdT1757972683(self);
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

FSETdTP FSETdT1713796851(FSETdTP self, dTP e) {
 FSETdTP ret_val;
 INT h = INT_zero;
 dTP te;
 INT i = INT_zero;
 dTP te1;
 INT hsh = INT_zero;
 FSETdTP elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETdTP is_elt_nil_self;
 dTP is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 FSETdTP elt_eq_self;
 dTP elt_eq_e1;
 dTP elt_eq_e2;
 BOOL ret_val3 = BOOL_zero;
 ELT_EQdTP elt_eq_self1;
 dTP elt_eq_e11;
 dTP elt_eq_e21;
 BOOL ret_val4 = BOOL_zero;
 FSETdTP elt_nil_self;
 dTP ret_val5;
 FSETdTP is_elt_nil_self1;
 dTP is_elt_nil_e1;
 BOOL ret_val6 = BOOL_zero;
 FSETdTP elt_hash_self1;
 OB elt_hash_e1;
 INT ret_val7 = INT_zero;
 FSETdTP elt_nil_self1;
 dTP ret_val8;
 FSETdTP elt_nil_self2;
 dTP ret_val9;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 dTP L71_;
 dTP L8;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 BOOL L131_;
 INT L141_;
 dTP L15;
 INT L17;
 INT L181_;
 INT L191_;
 INT L20;
 INT L221_;
 INT L23;
 BOOL L241_;
 INT L251_;
 dTP L261_;
 INT L27;
 INT L281_;
 INT L29;
 INT L301_;
 INT L31;
 INT L321_;
 BOOL L331_;
 BOOL L34;
 BOOL L351_;
 BOOL L36;
 BOOL L371_;
 BOOL L381_;
 BOOL L39;
 BOOL L401_;
 dTP L421_;
 dTP L43;
 dTP L45;
 BOOL L47;
 BOOL L481_;
 BOOL L49;
 BOOL L501_;
 BOOL L511_;
 dTP L521_;
 dTP L53;
 dTP L55;
 elt_hash_self = self;
 elt_hash_e = ((OB) e);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FSETdTP)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=(dTP)ARR((FSETdTP)self,h); 
  te = L71_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  ret_val2 = ELT_NI125473534(((ELT_NILdTP) NULL), is_elt_nil_e);
  if (ret_val2) {
   ret_val = self;
   return ret_val;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQdTP) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   L8 = elt_eq_e11;
   ret_val4 = (*dTP_is242312711[TAG(L8)])(L8, ((OB) elt_eq_e21));
   ret_val3 = ret_val4;
   if (ret_val3) {
    goto after_loop;
   }
  }
  L91_=ASIZE((FSETdTP)self); 
  L10 = L91_;
  L111_=INTMINUS(L10,2); 
  L12 = L111_;
  L131_=(h)==(L12); 
  if (L131_) {
   h = 0;
  }
  else {
   L141_=INTPLUS(h,1); 
   h = L141_;
  }
 }
 after_loop: ;
 elt_nil_self = self;
 ret_val5 = ELT_NI1328289538(((ELT_NILdTP) NULL));
 L15 = ret_val5;
 SARR((FSETdTP)self,h,(dTP)L15); 
 ;
 L17 = ATTR(self,hsize);
 L181_=INTMINUS(L17,1); 
 SATTR(self,hsize,L181_);
 i = h;
 while (1) {
  L191_=ASIZE((FSETdTP)self); 
  L20 = L191_;
  L221_=INTMINUS(L20,2); 
  L23 = L221_;
  L241_=(i)==(L23); 
  if (L241_) {
   i = 0;
  }
  else {
   L251_=INTPLUS(i,1); 
   i = L251_;
  }
  L261_=(dTP)ARR((FSETdTP)self,i); 
  te1 = L261_;
  is_elt_nil_self1 = self;
  is_elt_nil_e1 = te1;
  ret_val6 = ELT_NI125473534(((ELT_NILdTP) NULL), is_elt_nil_e1);
  if (ret_val6) {
   goto after_loop1;
  }
  elt_hash_self1 = self;
  elt_hash_e1 = ((OB) te1);
  ret_val7 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e1);
  L27 = ret_val7;
  L281_=ASIZE((FSETdTP)self); 
  L29 = L281_;
  L301_=INTMINUS(L29,2); 
  L31 = L301_;
  L321_=L27&L31; 
  hsh = L321_;
  L331_=(i)<(hsh); 
  L34 = L331_;
  L351_=!(L34); 
  if (L351_) {
   L371_=(h)<(i); 
   if (L371_) {
    L381_=(h)<(hsh); 
    L39 = L381_;
    L401_=!(L39); 
    L36 = L401_;
   } else {
    L36 = FALSE;
   }
   if (L36) {
    L421_=(dTP)ARR((FSETdTP)self,i); 
    L43 = L421_;
    SARR((FSETdTP)self,h,(dTP)L43); 
    ;
    h = i;
    elt_nil_self1 = self;
    ret_val8 = ELT_NI1328289538(((ELT_NILdTP) NULL));
    L45 = ret_val8;
    SARR((FSETdTP)self,i,(dTP)L45); 
    ;
   }
  }
  else {
   L481_=(h)<(hsh); 
   L49 = L481_;
   L501_=!(L49); 
   if (L501_) {
    L47 = TRUE;
   } else {
    L511_=(h)<(i); 
    L47 = L511_;
   }
   if (L47) {
    L521_=(dTP)ARR((FSETdTP)self,i); 
    L53 = L521_;
    SARR((FSETdTP)self,h,(dTP)L53); 
    ;
    h = i;
    elt_nil_self2 = self;
    ret_val9 = ELT_NI1328289538(((ELT_NILdTP) NULL));
    L55 = ret_val9;
    SARR((FSETdTP)self,i,(dTP)L55); 
    ;
   }
  }
 }
 after_loop1: ;
 if (FSETdT1179327129(self)) {
  ret_val = FSETdT1327793984(self);
  return ret_val;
 }
 else {
  ret_val = self;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETdTP FSETdT1757972683(FSETdTP self) {
 FSETdTP ret_val;
 FSETdTP r;
 FSETdTP L11;
 FSETdTP switch_structure;
 BOOL switch_structure1 = BOOL_zero;
 BOOL switch_structure2 = BOOL_zero;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 dTP L9;
 L21_=ASIZE((FSETdTP)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = FSETdT1290716916(self, L81_);
 switch_structure = r;
 switch_structure1 = ATTR(self,use_map);
 switch_structure2 = TRUE;
 if (FSETdT806139168) {
  SATTR(switch_structure,use_map,switch_structure2);
 }
 else {
  SATTR(switch_structure,use_map,switch_structure1);
 }
 {
  struct FSETdTP_eltbrdTP_frame_struct other1_0;
FSETdTP_eltbrdTP_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETdTP_eltbrdTP(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETdT1016814448(r, L9);
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

FSETdTP FSETdT1854975287(FSETdTP self, FSETdTP s) {
 FSETdTP ret_val;
 FSETdTP r;
 FSETdTP L11;
 dTP L2;
 r = self;
 {
  struct FSETdTP_eltbrdTP_frame_struct other1_0;
FSETdTP_eltbrdTP_frame noname1 = &other1_0;
  L11 = s;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FSETdTP_eltbrdTP(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETdT2034766666(r, L2);
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETdTP FSETdT2034766666(FSETdTP self, dTP e) {
 FSETdTP ret_val;
 if ((self==((FSETdTP) NULL))) {
  ret_val = ((FSETdTP) NULL);
  return ret_val;
 }
 if (ATTR(self,use_map)) {
  ret_val = FSETdT1713796851(self, e);
  return ret_val;
 }
 else {
  ret_val = FSETdT1256875665(self, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETdTP FSETdT217460096(FSETdTP self) {
 FSETdTP ret_val;
 FSETdTP r = ((FSETdTP) NULL);
 FSETdTP L11;
 dTP L2;
 {
  struct FSETdTP_eltbrdTP_frame_struct other1_0;
FSETdTP_eltbrdTP_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FSETdTP_eltbrdTP(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETdT1016814448(r, L2);
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETdTP FSETdT466016120(FSETdTP self, FSETdTP r, dTP e) {
 FSETdTP ret_val;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 dTP te;
 dTP te1;
 FSETdTP elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETdTP is_elt_nil_self;
 dTP is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 FSETdTP elt_eq_self;
 dTP elt_eq_e1;
 dTP elt_eq_e2;
 BOOL ret_val3 = BOOL_zero;
 ELT_EQdTP elt_eq_self1;
 dTP elt_eq_e11;
 dTP elt_eq_e21;
 BOOL ret_val4 = BOOL_zero;
 FSETdTP is_elt_nil_self1;
 dTP is_elt_nil_e1;
 BOOL ret_val5 = BOOL_zero;
 FSETdTP elt_eq_self2;
 dTP elt_eq_e12;
 dTP elt_eq_e22;
 BOOL ret_val6 = BOOL_zero;
 ELT_EQdTP elt_eq_self3;
 dTP elt_eq_e13;
 dTP elt_eq_e23;
 BOOL ret_val7 = BOOL_zero;
 INT L11_;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 dTP L61_;
 dTP L7;
 INT L91_;
 INT L101_;
 INT L12;
 BOOL L131_;
 dTP L141_;
 dTP L15;
 INT L171_;
 INT L19;
 INT L201_;
 L11_=ASIZE((FSETdTP)r); 
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
  L61_=(dTP)ARR((FSETdTP)r,h); 
  te = L61_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  ret_val2 = ELT_NI125473534(((ELT_NILdTP) NULL), is_elt_nil_e);
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQdTP) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   L7 = elt_eq_e11;
   ret_val4 = (*dTP_is242312711[TAG(L7)])(L7, ((OB) elt_eq_e21));
   ret_val3 = ret_val4;
   if (ret_val3) {
    SARR((FSETdTP)r,h,(dTP)e); 
    ;
    ret_val = r;
    return ret_val;
   }
  }
  L91_=INTPLUS(h,1); 
  h = L91_;
 }
 after_loop: ;
 L101_=INTMINUS(asz,1); 
 L12 = L101_;
 L131_=(h)==(L12); 
 if (L131_) {
  h = 0;
  while (1) {
   L141_=(dTP)ARR((FSETdTP)r,h); 
   te1 = L141_;
   is_elt_nil_self1 = self;
   is_elt_nil_e1 = te1;
   ret_val5 = ELT_NI125473534(((ELT_NILdTP) NULL), is_elt_nil_e1);
   if (ret_val5) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQdTP) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    L15 = elt_eq_e13;
    ret_val7 = (*dTP_is242312711[TAG(L15)])(L15, ((OB) elt_eq_e23));
    ret_val6 = ret_val7;
    if (ret_val6) {
     SARR((FSETdTP)r,h,(dTP)e); 
     ;
     ret_val = r;
     return ret_val;
    }
   }
   L171_=INTPLUS(h,1); 
   h = L171_;
  }
  after_loop1: ;
 }
 SARR((FSETdTP)r,h,(dTP)e); 
 ;
 L19 = ATTR(r,hsize);
 L201_=INTPLUS(L19,1); 
 SATTR(r,hsize,L201_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETdTP FSETdT575601071(FSETdTP self, FSETdTP s) {
 FSETdTP ret_val;
 FSETdTP r;
 FSETdTP L11;
 dTP L2;
 r = self;
 {
  struct FSETdTP_eltbrdTP_frame_struct other1_0;
FSETdTP_eltbrdTP_frame noname1 = &other1_0;
  L11 = s;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FSETdTP_eltbrdTP(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETdT1016814448(r, L2);
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETdTP FSETdT653600961(FSETdTP self, FSETdTP s) {
 FSETdTP ret_val;
 FSETdTP r = ((FSETdTP) NULL);
 FSETdTP L11;
 dTP e;
 dTP L2;
 {
  struct FSETdTP_eltbrdTP_frame_struct other1_0;
FSETdTP_eltbrdTP_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FSETdTP_eltbrdTP(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L2;
   if (FSETdT1764379880(s, e)) {
    r = FSETdT1016814448(r, e);
   }
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INLINE_ITER_TBL INLINE1476677165(INLINE_ITER_TBL self) {
 INLINE_ITER_TBL ret_val;
 INLINE_ITER_TBL r;
 INLINE_ITER_TBL L11;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 dINLINE_ITER_SIG L9;
 L21_=ASIZE((INLINE_ITER_TBL)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = INLINE245315800(self, L81_);
 {
  struct INLINE106514693_frame_struct other1_0;
INLINE106514693_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = INLINE106514693(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = INLINE450010170(r, L9);
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

INLINE_ITER_TBL INLINE245315800(INLINE_ITER_TBL self, INT n) {
 INLINE_ITER_TBL ret_val;
 INLINE_ITER_TBL r;
 INLINE_ITER_TBL L11;
 INLINE_ITER_TBL elt_nil_self;
 dINLINE_ITER_SIG ret_val1;
 INLINE_ITER_TBL elt_nil_self1;
 dINLINE_ITER_SIG ret_val2;
 INT L21 = INT_zero;
 INLINE_ITER_TBL L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 dINLINE_ITER_SIG L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct INLINE_ITER_TBL_struct)-sizeof(dINLINE_ITER_SIG))+(L4)*sizeof(dINLINE_ITER_SIG);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=INLINE_ITER_TBL_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((INLINE_ITER_TBL) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 ret_val1 = ELT_NI1331652151(((ELT_NI1131308118) NULL));
 L7 = (ret_val1==((dINLINE_ITER_SIG) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((INLINE_ITER_TBL)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self1 = self;
    ret_val2 = ELT_NI1331652151(((ELT_NI1131308118) NULL));
    L10 = ret_val2;
    SARR((INLINE_ITER_TBL)L11,L21,(dINLINE_ITER_SIG)L10); 
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

INLINE_ITER_TBL INLINE263789299(INLINE_ITER_TBL self, AM_ROUT_DEF am, BOOL inline_short, INT threshold1, PROG prog) {
 INLINE_ITER_TBL ret_val;
 INLINE_ITER create_self;
 AM_ROUT_DEF create_rout;
 INLINE_ITER ret_val1;
 INLINE_ITER r;
 BOOL L1;
 BOOL L2;
 BOOL L3;
 dTP L4;
 STR L5;
 extern STR OPT_DEBUG;
 BOOL L6;
 INLINE_ITER L7;
 OB L8;
 INT L9;
 INT L101_;
 if (ATTR(am,is_abstract)) {
  L2 = TRUE;
 } else {
  L2 = ATTR(am,is_external);
 }
 if (L2) {
  L1 = TRUE;
 } else {
  L1 = (ATTR(am,code1)==((dAM_STMT) NULL));
 }
 if (L1) {
  ret_val = self;
  return ret_val;
 }
 if (ATTR(prog,opt_debug)) {
  L4 = ATTR(ATTR(am,sig1),tp);
  L5 = (*dTP_strrSTR[TAG(L4)])(L4);
  L3 = STR_is111530248(L5, ((STR) &OPT_DEBUG));
 } else {
  L3 = FALSE;
 }
 if (L3) {
  ret_val = self;
  return ret_val;
 }
 if (inline_short) {
  L6 = INLINE803584675(((INLINE_ITER) NULL), am, threshold1, prog);
 } else {
  L6 = FALSE;
 }
 if (L6) {
  create_self = ((INLINE_ITER) NULL);
  create_rout = am;
  L8=ZALLOC(sizeof(struct INLINE_ITER_struct));
  if (L8==NULL) FATAL("Unable to allocate more memory");
  ((OB)L8)->header.tag=INLINE_ITER_tag;
  L7 = ((INLINE_ITER) L8);
  r = L7;
  SATTR(r,signature,ATTR(create_rout,sig1));
  SATTR(r,is_special,FALSE);
  INLINE15806555(r, create_rout);
  L9 = INLINE1401026874;
  L101_=INTPLUS(L9,1); 
  INLINE1401026874 = L101_;
  ret_val1 = r;
  ret_val = INLINE450010170(self, ((dINLINE_ITER_SIG) ret_val1));
  return ret_val;
 }
 else {
  ret_val = self;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INLINE_ITER_TBL INLINE450010170(INLINE_ITER_TBL self, dINLINE_ITER_SIG e) {
 INLINE_ITER_TBL ret_val;
 INLINE_ITER_TBL r;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 dINLINE_ITER_SIG te;
 dINLINE_ITER_SIG te1;
 INLINE_ITER_TBL is_elt_nil_self;
 dINLINE_ITER_SIG is_elt_nil_e;
 BOOL ret_val1 = BOOL_zero;
 INLINE_ITER_TBL should_grow_self;
 BOOL ret_val2 = BOOL_zero;
 INLINE_ITER_TBL elt_hash_self;
 dINLINE_ITER_SIG elt_hash_in;
 INT ret_val3 = INT_zero;
 INLINE_ITER_TBL is_elt_nil_self1;
 dINLINE_ITER_SIG is_elt_nil_e1;
 BOOL ret_val4 = BOOL_zero;
 INLINE_ITER_TBL elt_eq_self;
 dINLINE_ITER_SIG elt_eq_e1;
 dINLINE_ITER_SIG elt_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 INLINE_ITER_TBL is_elt_nil_self2;
 dINLINE_ITER_SIG is_elt_nil_e2;
 BOOL ret_val6 = BOOL_zero;
 INLINE_ITER_TBL elt_eq_self1;
 dINLINE_ITER_SIG elt_eq_e11;
 dINLINE_ITER_SIG elt_eq_e21;
 BOOL ret_val7 = BOOL_zero;
 INT L11_;
 INT L2;
 INT L3;
 INT L41_;
 INT L5;
 INT L6;
 INT L71_;
 INT L8;
 BOOL L91_;
 INT L101_;
 dINLINE_ITER_SIG L12;
 INT L13;
 INT L141_;
 INT L15;
 INT L161_;
 dINLINE_ITER_SIG L171_;
 INT L191_;
 INT L201_;
 INT L21;
 BOOL L221_;
 dINLINE_ITER_SIG L231_;
 INT L251_;
 INT L27;
 INT L281_;
 r = self;
 is_elt_nil_self = self;
 is_elt_nil_e = e;
 ret_val1 = ELT_NI569783157(((ELT_NI1131308118) NULL), is_elt_nil_e);
 if (ret_val1) {
  ret_val = r;
  return ret_val;
 }
 if ((r==((INLINE_ITER_TBL) NULL))) {
  r = INLINE245315800(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((INLINE_ITER_TBL)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = INLINE494873411;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val2 = L91_;
  if (ret_val2) {
   r = INLINE1476677165(r);
  }
 }
 L101_=ASIZE((INLINE_ITER_TBL)r); 
 asz = L101_;
 elt_hash_self = r;
 elt_hash_in = e;
 L12 = elt_hash_in;
 ret_val3 = INLINE1816775773(elt_hash_self, (*dINLIN1426868953[TAG(L12)])(L12));
 L13 = ret_val3;
 L141_=INTMINUS(asz,2); 
 L15 = L141_;
 L161_=L13&L15; 
 orig_h = L161_;
 h = orig_h;
 while (1) {
  L171_=(dINLINE_ITER_SIG)ARR((INLINE_ITER_TBL)r,h); 
  te = L171_;
  is_elt_nil_self1 = self;
  is_elt_nil_e1 = te;
  ret_val4 = ELT_NI569783157(((ELT_NI1131308118) NULL), is_elt_nil_e1);
  if (ret_val4) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   ret_val5 = ELT_EQ182879965(((ELT_EQ773986801) NULL), elt_eq_e1, elt_eq_e2);
   if (ret_val5) {
    SARR((INLINE_ITER_TBL)r,h,(dINLINE_ITER_SIG)e); 
    ;
    ret_val = r;
    return ret_val;
   }
  }
  L191_=INTPLUS(h,1); 
  h = L191_;
 }
 after_loop: ;
 L201_=INTMINUS(asz,1); 
 L21 = L201_;
 L221_=(h)==(L21); 
 if (L221_) {
  h = 0;
  while (1) {
   L231_=(dINLINE_ITER_SIG)ARR((INLINE_ITER_TBL)r,h); 
   te1 = L231_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   ret_val6 = ELT_NI569783157(((ELT_NI1131308118) NULL), is_elt_nil_e2);
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    elt_eq_self1 = self;
    elt_eq_e11 = te1;
    elt_eq_e21 = e;
    ret_val7 = ELT_EQ182879965(((ELT_EQ773986801) NULL), elt_eq_e11, elt_eq_e21);
    if (ret_val7) {
     SARR((INLINE_ITER_TBL)r,h,(dINLINE_ITER_SIG)e); 
     ;
     ret_val = r;
     return ret_val;
    }
   }
   L251_=INTPLUS(h,1); 
   h = L251_;
  }
  after_loop1: ;
 }
 SARR((INLINE_ITER_TBL)r,h,(dINLINE_ITER_SIG)e); 
 ;
 L27 = ATTR(r,hsize);
 L281_=INTPLUS(L27,1); 
 SATTR(r,hsize,L281_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INLINE_ITER_TBL INLINE785230726(INLINE_ITER_TBL self, INT n) {
 INLINE_ITER_TBL ret_val;
 INLINE_ITER_TBL r;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 INT L9;
 INT L101_;
 INT L11;
 INT L121_;
 L1 = INLINE494873411;
 L21_=INTTIMES(3,L1); 
 L3 = L21_;
 L41_=INTTIMES(L3,n); 
 L5 = L41_;
 L61_=INTDIV(L5,4); 
 L7 = INT_hi740923800(L61_);
 L81_=INTPLUS(L7,1); 
 L9 = L81_;
 L101_=((unsigned)1)<<((unsigned)L9); 
 L11 = L101_;
 L121_=INTPLUS(L11,1); 
 r = INLINE245315800(self, L121_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INLINE_ROUT_TBL INLINE1117751299(INLINE_ROUT_TBL self) {
 INLINE_ROUT_TBL ret_val;
 INLINE_ROUT_TBL r;
 INLINE_ROUT_TBL L11;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 dINLINE_ROUT_SIG L9;
 L21_=ASIZE((INLINE_ROUT_TBL)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = INLINE2122266936(self, L81_);
 {
  struct INLINE600707605_frame_struct other1_0;
INLINE600707605_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = INLINE600707605(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = INLINE525216690(r, L9);
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

INLINE_ROUT_TBL INLINE1394857882(INLINE_ROUT_TBL self, INT n) {
 INLINE_ROUT_TBL ret_val;
 INLINE_ROUT_TBL r;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 INT L9;
 INT L101_;
 INT L11;
 INT L121_;
 L1 = INLINE29411189;
 L21_=INTTIMES(3,L1); 
 L3 = L21_;
 L41_=INTTIMES(L3,n); 
 L5 = L41_;
 L61_=INTDIV(L5,4); 
 L7 = INT_hi740923800(L61_);
 L81_=INTPLUS(L7,1); 
 L9 = L81_;
 L101_=((unsigned)1)<<((unsigned)L9); 
 L11 = L101_;
 L121_=INTPLUS(L11,1); 
 r = INLINE2122266936(self, L121_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INLINE_ROUT_TBL INLINE180230301(INLINE_ROUT_TBL self, AM_ROUT_DEF am, BOOL inline_short, INT threshold1, PROG prog) {
 INLINE_ROUT_TBL ret_val;
 dAM_STMT stmts11;
 dAM_EXPR val11;
 dAM_EXPR dest1;
 dAM_EXPR src11;
 INLINE_ATTR_READ create_self;
 AM_ROUT_DEF create_am;
 INLINE_ATTR_READ ret_val1;
 INLINE_ATTR_READ r;
 dAM_STMT stmt1;
 dAM_EXPR val21;
 INLINE1164904870 create_self1;
 AM_ROUT_DEF create_am1;
 INLINE1164904870 ret_val2;
 INLINE1164904870 r1;
 dAM_STMT stmt2;
 dAM_EXPR val3;
 INLINE1596311811 create_self2;
 AM_ROUT_DEF create_am2;
 INLINE1596311811 ret_val3;
 INLINE1596311811 r2;
 dAM_STMT stmt3;
 dAM_EXPR dest2;
 AM_FORMAL_ARG name_self;
 IDENT ret_val4 = IDENT_zero;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val5 = BOOL_zero;
 INLINE_ROUT create_self3;
 AM_ROUT_DEF create_rout;
 INLINE_ROUT ret_val6;
 INLINE_ROUT r3;
 INLINE_ROUT copy_rout_self;
 AM_ROUT_DEF copy_rout_rout;
 XFORM_CODE xc;
 XFORM_CODE create_self4;
 FMAPAM1145008811 create_subst;
 XFORM_CODE ret_val7;
 XFORM_CODE xc1;
 CHANGE_VARS after_saving_sel;
 BOOL L1;
 BOOL L2;
 dAM_STMT L3;
 AM_FORMAL_ARG L41_;
 AM_FORMAL_ARG L5;
 INLINE_ATTR_READ L6;
 OB L7;
 INT L8;
 INT L91_;
 AM_FORMAL_ARG L101_;
 AM_FORMAL_ARG L11;
 INLINE1164904870 L12;
 OB L13;
 INT L14;
 INT L151_;
 BOOL L16;
 BOOL L17;
 INT L181_;
 INT L19;
 BOOL L201_;
 AM_FORMAL_ARG L211_;
 AM_FORMAL_ARG L22;
 AM_FORMAL_ARG L231_;
 AM_FORMAL_ARG L24;
 INLINE1596311811 L25;
 OB L26;
 INT L27;
 INT L281_;
 BOOL L29;
 BOOL L30;
 BOOL L31;
 INT L321_;
 INT L33;
 BOOL L341_;
 AM_FORMAL_ARG L351_;
 AM_FORMAL_ARG L36;
 AM_FORMAL_ARG L371_;
 BOOL L38;
 dTP L39;
 STR L40;
 extern STR OPT_DEBUG;
 BOOL L42;
 INLINE_ROUT L43;
 OB L44;
 XFORM_CODE L45;
 OB L46;
 FMAPAM1145008811 L47;
 INT L50;
 INT L511_;
 if (ATTR(am,is_abstract)) {
  L2 = TRUE;
 } else {
  L2 = ATTR(am,is_external);
 }
 if (L2) {
  L1 = TRUE;
 } else {
  L1 = (ATTR(am,code1)==((dAM_STMT) NULL));
 }
 if (L1) {
  ret_val = self;
  return ret_val;
 }
 L3 = ATTR(am,code1);
 if (((*dAM_ST775224435[TAG(L3)])(L3)==((dAM_STMT) NULL))) {
  stmts11 = ATTR(am,code1);
  if (stmts11==NULL) {
   goto other458;
  } else
  switch (TAG(stmts11)) {
   case AM_RETURN_STMT_tag:
    val11 = ATTR(((AM_RETURN_STMT) stmts11),val1);
    if (val11==NULL) {
     goto other459;
    } else
    switch (TAG(val11)) {
     case AM_ATTR_EXPR_tag:
      L41_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)am,0); 
      L5=L41_;
      if (SYSOBEQ(((OB) ATTR(((AM_ATTR_EXPR) val11),ob)),((OB) ATTR(L5,expr)))) {
       create_self = ((INLINE_ATTR_READ) NULL);
       create_am = am;
       L7=ZALLOC(sizeof(struct INLINE_ATTR_READ_struct));
       if (L7==NULL) FATAL("Unable to allocate more memory");
       ((OB)L7)->header.tag=INLINE_ATTR_READ_tag;
       L6 = ((INLINE_ATTR_READ) L7);
       r = L6;
       SATTR(r,sig1,ATTR(create_am,sig1));
       SATTR(r,is_special,TRUE);
       stmt1 = ATTR(create_am,code1);
       switch (TAG(stmt1)) {
        case AM_RETURN_STMT_tag:
         val21 = ATTR(((AM_RETURN_STMT) stmt1),val1);
         switch (TAG(val21)) {
          case AM_ATTR_EXPR_tag:
           SATTR(r,at,ATTR(((AM_ATTR_EXPR) val21),at));
           SATTR(r,self_tp,ATTR(((AM_ATTR_EXPR) val21),self_tp));
           SATTR(r,tp_at,ATTR(((AM_ATTR_EXPR) val21),tp_at)); break;
          default: ;
           FATAL("No applicable type in typecase\nin INLINE_ROUT_TBL::test_and_insert(AM_ROUT_DEF,BOOL,INT,PROG):SAME\n./Middle/inline.sa:72:20");
         } break;
        default: ;
         FATAL("No applicable type in typecase\nin INLINE_ROUT_TBL::test_and_insert(AM_ROUT_DEF,BOOL,INT,PROG):SAME\n./Middle/inline.sa:70:16");
       }
       L8 = INLINE598259372;
       L91_=INTPLUS(L8,1); 
       INLINE598259372 = L91_;
       ret_val1 = r;
       ret_val = INLINE525216690(self, ((dINLINE_ROUT_SIG) ret_val1));
       return ret_val;
      } break;
     case AM_VAT319982528_tag:
      L101_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)am,0); 
      L11=L101_;
      if (SYSOBEQ(((OB) ATTR(((AM_VAT319982528) val11),ob)),((OB) ATTR(L11,expr)))) {
       create_self1 = ((INLINE1164904870) NULL);
       create_am1 = am;
       L13=ZALLOC(sizeof(struct INLINE1164904870_struct));
       if (L13==NULL) FATAL("Unable to allocate more memory");
       ((OB)L13)->header.tag=INLINE1164904870_tag;
       L12 = ((INLINE1164904870) L13);
       r1 = L12;
       SATTR(r1,sig1,ATTR(create_am1,sig1));
       SATTR(r1,is_special,TRUE);
       stmt2 = ATTR(create_am1,code1);
       switch (TAG(stmt2)) {
        case AM_RETURN_STMT_tag:
         val3 = ATTR(((AM_RETURN_STMT) stmt2),val1);
         switch (TAG(val3)) {
          case AM_VAT319982528_tag:
           SATTR(r1,at,ATTR(((AM_VAT319982528) val3),at)); break;
          default: ;
           FATAL("No applicable type in typecase\nin INLINE_ROUT_TBL::test_and_insert(AM_ROUT_DEF,BOOL,INT,PROG):SAME\n./Middle/inline.sa:303:20");
         } break;
        default: ;
         FATAL("No applicable type in typecase\nin INLINE_ROUT_TBL::test_and_insert(AM_ROUT_DEF,BOOL,INT,PROG):SAME\n./Middle/inline.sa:301:16");
       }
       L14 = INLINE1484868694;
       L151_=INTPLUS(L14,1); 
       INLINE1484868694 = L151_;
       ret_val2 = r1;
       ret_val = INLINE525216690(self, ((dINLINE_ROUT_SIG) ret_val2));
       return ret_val;
      } break;
     case AM_SHARED_EXPR_tag:
      ret_val = INLINE525216690(self, ((dINLINE_ROUT_SIG) INLINE433490271(((INLINE1433668901) NULL), am)));
      return ret_val; break;
     default: ;
     other459: ;
    } break;
   case AM_ASSIGN_STMT_tag:
    dest1 = ATTR(((AM_ASSIGN_STMT) stmts11),dest);
    src11 = ATTR(((AM_ASSIGN_STMT) stmts11),src1);
    if (dest1==NULL) {
     goto other460;
    } else
    switch (TAG(dest1)) {
     case AM_ATTR_EXPR_tag:
      L181_=ASIZE((AM_ROUT_DEF)am); 
      L19 = L181_;
      L201_=(1)<(L19); 
      if (L201_) {
       L211_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)am,0); 
       L22=L211_;
       L17 = SYSOBEQ(((OB) ATTR(((AM_ATTR_EXPR) dest1),ob)),((OB) ATTR(L22,expr)));
      } else {
       L17 = FALSE;
      }
      if (L17) {
       L231_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)am,1); 
       L24=L231_;
       L16 = SYSOBEQ(((OB) src11),((OB) ATTR(L24,expr)));
      } else {
       L16 = FALSE;
      }
      if (L16) {
       create_self2 = ((INLINE1596311811) NULL);
       create_am2 = am;
       L26=ZALLOC(sizeof(struct INLINE1596311811_struct));
       if (L26==NULL) FATAL("Unable to allocate more memory");
       ((OB)L26)->header.tag=INLINE1596311811_tag;
       L25 = ((INLINE1596311811) L26);
       r2 = L25;
       SATTR(r2,sig1,ATTR(create_am2,sig1));
       SATTR(r2,is_special,TRUE);
       stmt3 = ATTR(create_am2,code1);
       switch (TAG(stmt3)) {
        case AM_ASSIGN_STMT_tag:
         dest2 = ATTR(((AM_ASSIGN_STMT) stmt3),dest);
         switch (TAG(dest2)) {
          case AM_ATTR_EXPR_tag:
           SATTR(r2,self_tp,ATTR(((AM_ATTR_EXPR) dest2),self_tp));
           SATTR(r2,at,ATTR(((AM_ATTR_EXPR) dest2),at));
           SATTR(r2,tp_at,ATTR(((AM_ATTR_EXPR) dest2),tp_at)); break;
          default: ;
           FATAL("No applicable type in typecase\nin INLINE_ROUT_TBL::test_and_insert(AM_ROUT_DEF,BOOL,INT,PROG):SAME\n./Middle/inline.sa:116:20");
         } break;
        default: ;
         FATAL("No applicable type in typecase\nin INLINE_ROUT_TBL::test_and_insert(AM_ROUT_DEF,BOOL,INT,PROG):SAME\n./Middle/inline.sa:114:16");
       }
       L27 = INLINE2014794524;
       L281_=INTPLUS(L27,1); 
       INLINE2014794524 = L281_;
       ret_val3 = r2;
       ret_val = INLINE525216690(self, ((dINLINE_ROUT_SIG) ret_val3));
       return ret_val;
      } break;
     case AM_SHARED_EXPR_tag:
      L321_=(am)==NULL?0:ASIZE((AM_ROUT_DEF)am); 
      L33 = L321_;
      L341_=(1)<(L33); 
      if (L341_) {
       L351_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)am,1); 
       L36=L351_;
       L31 = SYSOBEQ(((OB) src11),((OB) ATTR(L36,expr)));
      } else {
       L31 = FALSE;
      }
      if (L31) {
       L371_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)am,0); 
       name_self = L371_;
       ret_val4 = ATTR(ATTR(name_self,expr),name1);
       is_eq_self = ret_val4;
       is_eq_i = IDENT_985337352;
       ret_val5 = (is_eq_self.str==is_eq_i.str);
       L30 = ret_val5;
      } else {
       L30 = FALSE;
      }
      if (L30) {
       L29 = SIG_is152200802(ATTR(am,sig1));
      } else {
       L29 = FALSE;
      }
      if (L29) {
       ret_val = INLINE525216690(self, ((dINLINE_ROUT_SIG) INLINE1362941708(((INLINE1455768374) NULL), am)));
       return ret_val;
      } break;
     default: ;
     other460: ;
    } break;
   default: ;
   other458: ;
  }
 }
 if (ATTR(prog,opt_debug)) {
  L39 = ATTR(ATTR(am,sig1),tp);
  L40 = (*dTP_strrSTR[TAG(L39)])(L39);
  L38 = STR_is111530248(L40, ((STR) &OPT_DEBUG));
 } else {
  L38 = FALSE;
 }
 if (L38) {
  ret_val = self;
  return ret_val;
 }
 if (inline_short) {
  L42 = INLINE1537257477(((INLINE_ROUT) NULL), am, threshold1, prog);
 } else {
  L42 = FALSE;
 }
 if (L42) {
  create_self3 = ((INLINE_ROUT) NULL);
  create_rout = am;
  L44=ZALLOC(sizeof(struct INLINE_ROUT_struct));
  if (L44==NULL) FATAL("Unable to allocate more memory");
  ((OB)L44)->header.tag=INLINE_ROUT_tag;
  L43 = ((INLINE_ROUT) L44);
  r3 = L43;
  SATTR(r3,signature,ATTR(create_rout,sig1));
  SATTR(r3,is_special,FALSE);
  copy_rout_self = r3;
  copy_rout_rout = create_rout;
  SATTR(copy_rout_self,change_vars,CHANGE1489663732(((CHANGE_VARS) NULL), copy_rout_rout));
  create_self4 = ((XFORM_CODE) NULL);
  create_subst = ATTR(ATTR(copy_rout_self,change_vars),saved_subst);
  L46=ZALLOC(sizeof(struct XFORM_CODE_struct));
  if (L46==NULL) FATAL("Unable to allocate more memory");
  ((OB)L46)->header.tag=XFORM_CODE_tag;
  L45 = ((XFORM_CODE) L46);
  xc1 = L45;
  SATTR(xc1,subst,create_subst);
  ret_val7 = xc1;
  xc = ret_val7;
  SATTR(xc,record_calls,FALSE);
  SATTR(copy_rout_self,code1,XFORM_1427763452(xc, ATTR(copy_rout_rout,code1)));
  SATTR(copy_rout_self,return_val,ATTR(xc,return_val));
  after_saving_sel = ATTR(copy_rout_self,change_vars);
  L47 = ATTR(after_saving_sel,saved_subst);
  SYSDESTROY(L47); 
  ;
  SYSDESTROY(xc); 
  ;
  L50 = INLINE1516217386;
  L511_=INTPLUS(L50,1); 
  INLINE1516217386 = L511_;
  ret_val6 = r3;
  ret_val = INLINE525216690(self, ((dINLINE_ROUT_SIG) ret_val6));
  return ret_val;
 }
 ret_val = self;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INLINE_ROUT_TBL INLINE2122266936(INLINE_ROUT_TBL self, INT n) {
 INLINE_ROUT_TBL ret_val;
 INLINE_ROUT_TBL r;
 INLINE_ROUT_TBL L11;
 INLINE_ROUT_TBL elt_nil_self;
 dINLINE_ROUT_SIG ret_val1;
 INLINE_ROUT_TBL elt_nil_self1;
 dINLINE_ROUT_SIG ret_val2;
 INT L21 = INT_zero;
 INLINE_ROUT_TBL L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 dINLINE_ROUT_SIG L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct INLINE_ROUT_TBL_struct)-sizeof(dINLINE_ROUT_SIG))+(L4)*sizeof(dINLINE_ROUT_SIG);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=INLINE_ROUT_TBL_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((INLINE_ROUT_TBL) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 ret_val1 = ELT_NI706507177(((ELT_NI52152274) NULL));
 L7 = (ret_val1==((dINLINE_ROUT_SIG) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((INLINE_ROUT_TBL)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self1 = self;
    ret_val2 = ELT_NI706507177(((ELT_NI52152274) NULL));
    L10 = ret_val2;
    SARR((INLINE_ROUT_TBL)L11,L21,(dINLINE_ROUT_SIG)L10); 
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

INLINE_ROUT_TBL INLINE525216690(INLINE_ROUT_TBL self, dINLINE_ROUT_SIG e) {
 INLINE_ROUT_TBL ret_val;
 INLINE_ROUT_TBL r;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 dINLINE_ROUT_SIG te;
 dINLINE_ROUT_SIG te1;
 INLINE_ROUT_TBL is_elt_nil_self;
 dINLINE_ROUT_SIG is_elt_nil_e;
 BOOL ret_val1 = BOOL_zero;
 INLINE_ROUT_TBL should_grow_self;
 BOOL ret_val2 = BOOL_zero;
 INLINE_ROUT_TBL elt_hash_self;
 dINLINE_ROUT_SIG elt_hash_in;
 INT ret_val3 = INT_zero;
 INLINE_ROUT_TBL is_elt_nil_self1;
 dINLINE_ROUT_SIG is_elt_nil_e1;
 BOOL ret_val4 = BOOL_zero;
 INLINE_ROUT_TBL elt_eq_self;
 dINLINE_ROUT_SIG elt_eq_e1;
 dINLINE_ROUT_SIG elt_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 INLINE_ROUT_TBL is_elt_nil_self2;
 dINLINE_ROUT_SIG is_elt_nil_e2;
 BOOL ret_val6 = BOOL_zero;
 INLINE_ROUT_TBL elt_eq_self1;
 dINLINE_ROUT_SIG elt_eq_e11;
 dINLINE_ROUT_SIG elt_eq_e21;
 BOOL ret_val7 = BOOL_zero;
 INT L11_;
 INT L2;
 INT L3;
 INT L41_;
 INT L5;
 INT L6;
 INT L71_;
 INT L8;
 BOOL L91_;
 INT L101_;
 dINLINE_ROUT_SIG L12;
 INT L13;
 INT L141_;
 INT L15;
 INT L161_;
 dINLINE_ROUT_SIG L171_;
 INT L191_;
 INT L201_;
 INT L21;
 BOOL L221_;
 dINLINE_ROUT_SIG L231_;
 INT L251_;
 INT L27;
 INT L281_;
 r = self;
 is_elt_nil_self = self;
 is_elt_nil_e = e;
 ret_val1 = ELT_NI1893940731(((ELT_NI52152274) NULL), is_elt_nil_e);
 if (ret_val1) {
  ret_val = r;
  return ret_val;
 }
 if ((r==((INLINE_ROUT_TBL) NULL))) {
  r = INLINE2122266936(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((INLINE_ROUT_TBL)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = INLINE29411189;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val2 = L91_;
  if (ret_val2) {
   r = INLINE1117751299(r);
  }
 }
 L101_=ASIZE((INLINE_ROUT_TBL)r); 
 asz = L101_;
 elt_hash_self = r;
 elt_hash_in = e;
 L12 = elt_hash_in;
 ret_val3 = INLINE1047927461(elt_hash_self, (*dINLIN1366832129[TAG(L12)])(L12));
 L13 = ret_val3;
 L141_=INTMINUS(asz,2); 
 L15 = L141_;
 L161_=L13&L15; 
 orig_h = L161_;
 h = orig_h;
 while (1) {
  L171_=(dINLINE_ROUT_SIG)ARR((INLINE_ROUT_TBL)r,h); 
  te = L171_;
  is_elt_nil_self1 = self;
  is_elt_nil_e1 = te;
  ret_val4 = ELT_NI1893940731(((ELT_NI52152274) NULL), is_elt_nil_e1);
  if (ret_val4) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   ret_val5 = ELT_EQ398237803(((ELT_EQ1957447193) NULL), elt_eq_e1, elt_eq_e2);
   if (ret_val5) {
    SARR((INLINE_ROUT_TBL)r,h,(dINLINE_ROUT_SIG)e); 
    ;
    ret_val = r;
    return ret_val;
   }
  }
  L191_=INTPLUS(h,1); 
  h = L191_;
 }
 after_loop: ;
 L201_=INTMINUS(asz,1); 
 L21 = L201_;
 L221_=(h)==(L21); 
 if (L221_) {
  h = 0;
  while (1) {
   L231_=(dINLINE_ROUT_SIG)ARR((INLINE_ROUT_TBL)r,h); 
   te1 = L231_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   ret_val6 = ELT_NI1893940731(((ELT_NI52152274) NULL), is_elt_nil_e2);
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    elt_eq_self1 = self;
    elt_eq_e11 = te1;
    elt_eq_e21 = e;
    ret_val7 = ELT_EQ398237803(((ELT_EQ1957447193) NULL), elt_eq_e11, elt_eq_e21);
    if (ret_val7) {
     SARR((INLINE_ROUT_TBL)r,h,(dINLINE_ROUT_SIG)e); 
     ;
     ret_val = r;
     return ret_val;
    }
   }
   L251_=INTPLUS(h,1); 
   h = L251_;
  }
  after_loop1: ;
 }
 SARR((INLINE_ROUT_TBL)r,h,(dINLINE_ROUT_SIG)e); 
 ;
 L27 = ATTR(r,hsize);
 L281_=INTPLUS(L27,1); 
 SATTR(r,hsize,L281_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_ARR191206921(AM_ARRAY_EXPR self) {
 INT ret_val = INT_zero;
 INT L11_;
 L11_=SYSID(self); 
 ret_val = INT_hashrINT(L11_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_BND1144165392(AM_BND260301329 self) {
 INT ret_val = INT_zero;
 INT L11_;
 L11_=SYSID(self); 
 ret_val = INT_hashrINT(L11_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_BND2097777224(AM_BND367211769 self) {
 INT ret_val = INT_zero;
 INT L11_;
 L11_=SYSID(self); 
 ret_val = INT_hashrINT(L11_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_BND472678797(AM_BND1124877163 self) {
 INT ret_val = INT_zero;
 INT L11_;
 L11_=SYSID(self); 
 ret_val = INT_hashrINT(L11_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_EXT644325801(AM_EXT_CALL_EXPR self) {
 INT ret_val = INT_zero;
 INT L11_;
 L11_=SYSID(self); 
 ret_val = INT_hashrINT(L11_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_ITE1021507011(AM_ITE1809135850 self) {
 INT ret_val = INT_zero;
 INT L11_;
 L11_=SYSID(self); 
 ret_val = INT_hashrINT(L11_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT FSETTU1591851819(FSETTUPSIGSIG self) {
 INT ret_val = INT_zero;
 if ((self==((FSETTUPSIGSIG) NULL))) {
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

INT FSETdL858951727(FSETdLAYOUT self) {
 INT ret_val = INT_zero;
 if ((self==((FSETdLAYOUT) NULL))) {
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

INT FSETdTP_sizerINT(FSETdTP self) {
 INT ret_val = INT_zero;
 if ((self==((FSETdTP) NULL))) {
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

INT INLINE1047927461(INLINE_ROUT_TBL self, SIG s) {
 INT ret_val = INT_zero;
 INT sc = INT_zero;
 INT r = INT_zero;
 ARRAYARG L11;
 IDENT hash_self = IDENT_zero;
 INT ret_val1 = INT_zero;
 INT x = INT_zero;
 INT L21 = INT_zero;
 STR L3;
 INT L41_;
 INT L51_;
 INT L6;
 INT L71_;
 dTP L8;
 INT L91_;
 INT L10;
 INT L121_;
 INT L13;
 INT L141_;
 INT L151_br;
 ARG aL151_;
 INT L161_;
 ARG L17;
 ARG L18;
 dTP L19;
 INT L201_;
 INT L22;
 INT L231_;
 INT L24;
 INT L251_;
 INT L261_;
 sc = 3;
 hash_self = ATTR(s,name1);
 L3 = hash_self.str;
 L41_=SYSID(L3); 
 x = L41_;
 L51_=((unsigned)x*(unsigned)1664525); 
 L6 = L51_;
 L71_=((unsigned)L6+(unsigned)1013904223); 
 ret_val1 = L71_;
 r = ret_val1;
 L8 = ATTR(s,tp);
 L91_=SYSID(L8); 
 L10 = L91_;
 L121_=INTTIMES(L10,sc); 
 L13 = L121_;
 L141_=r^L13; 
 r = L141_;
 {
  BOOL f_L151_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = ATTR(s,args);
  L151_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
  while (1) {
   L161_=((unsigned)sc+(unsigned)98); 
   sc = L161_;
   if(L21>=L151_br)  goto after_loop; 
   aL151_=ARR((ARRAYARG)L11,L21); 
   L18=aL151_;
   L19 = ATTR(L18,tp);
   L201_=SYSID(L19); 
   L22 = L201_;
   L231_=((unsigned)L22*(unsigned)sc); 
   L24 = L231_;
   L251_=r^L24; 
   r = L251_;
   L261_=INTPLUS(L21,1); 
   L21 = L261_;
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT INLINE1816775773(INLINE_ITER_TBL self, SIG s) {
 INT ret_val = INT_zero;
 INT sc = INT_zero;
 INT r = INT_zero;
 ARRAYARG L11;
 IDENT hash_self = IDENT_zero;
 INT ret_val1 = INT_zero;
 INT x = INT_zero;
 INT L21 = INT_zero;
 STR L3;
 INT L41_;
 INT L51_;
 INT L6;
 INT L71_;
 dTP L8;
 INT L91_;
 INT L10;
 INT L121_;
 INT L13;
 INT L141_;
 INT L151_br;
 ARG aL151_;
 INT L161_;
 ARG L17;
 ARG L18;
 dTP L19;
 INT L201_;
 INT L22;
 INT L231_;
 INT L24;
 INT L251_;
 INT L261_;
 sc = 3;
 hash_self = ATTR(s,name1);
 L3 = hash_self.str;
 L41_=SYSID(L3); 
 x = L41_;
 L51_=((unsigned)x*(unsigned)1664525); 
 L6 = L51_;
 L71_=((unsigned)L6+(unsigned)1013904223); 
 ret_val1 = L71_;
 r = ret_val1;
 L8 = ATTR(s,tp);
 L91_=SYSID(L8); 
 L10 = L91_;
 L121_=INTTIMES(L10,sc); 
 L13 = L121_;
 L141_=r^L13; 
 r = L141_;
 {
  BOOL f_L151_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = ATTR(s,args);
  L151_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
  while (1) {
   L161_=((unsigned)sc+(unsigned)98); 
   sc = L161_;
   if(L21>=L151_br)  goto after_loop; 
   aL151_=ARR((ARRAYARG)L11,L21); 
   L18=aL151_;
   L19 = ATTR(L18,tp);
   L201_=SYSID(L19); 
   L22 = L201_;
   L231_=((unsigned)L22*(unsigned)sc); 
   L24 = L231_;
   L251_=r^L24; 
   r = L251_;
   L261_=INTPLUS(L21,1); 
   L21 = L261_;
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_ARR592984289(AM_ARRAY_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_BND1008256842(AM_BND260301329 self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_BND1697524322(AM_BND367211769 self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_BND1800984858(AM_BND1124877163 self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_EXT840500856(AM_EXT_CALL_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_ITE834410629(AM_ITE1809135850 self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_ROU231894201(AM_ROUT_DEF self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYA1490975097(ARRAYA1870808380 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 ABSTRACT_LAYOUT e;
 ARRAYA1870808380 elt_str_self;
 ABSTRACT_LAYOUT elt_str_e;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYA1870808380 elt_str_self1;
 ABSTRACT_LAYOUT elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 ABSTRACT_LAYOUT L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYA1870808380)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(ABSTRACT_LAYOUT)ARR((ARRAYA1870808380)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = ATTR(elt_str_e,str);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = ATTR(elt_str_e1,str);
   res = FSTR_p1752847026(L6, ret_val2);
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYA1621913879(ARRAYA855692714 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 AM_BND1124877163 e;
 ARRAYA855692714 elt_str_self;
 AM_BND1124877163 elt_str_e;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 INT str_self = INT_zero;
 STR ret_val2;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val3;
 FSTR str_self1;
 STR ret_val4;
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 ARRAYA855692714 elt_str_self1;
 AM_BND1124877163 elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val6;
 INT str_self2 = INT_zero;
 STR ret_val7;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val8;
 FSTR str_self3;
 STR ret_val9;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val10;
 FSTR str_self4;
 STR ret_val11;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 AM_BND1124877163 L41_;
 BOOL L51_;
 extern STR Unprintable;
 BOOL L6;
 BOOL L71_;
 extern STR name3;
 FSTR L8;
 extern STR Unprintable;
 BOOL L9;
 BOOL L101_;
 INT L121_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYA855692714)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(AM_BND1124877163)ARR((ARRAYA855692714)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   plus_self = ((STR) &Unprintable);
   str_self = elt_str_i;
   clear_self = buf1;
   L6 = (clear_self==((FSTR) NULL));
   L71_=!(L6); 
   if (L71_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val3;
   str_self1 = buf1;
   ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val2 = ret_val4;
   plus_sarg = ret_val2;
   ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val1 = ret_val5;
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L8 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   plus_self1 = ((STR) &Unprintable);
   str_self2 = elt_str_i1;
   clear_self1 = buf2;
   L9 = (clear_self1==((FSTR) NULL));
   L101_=!(L9); 
   if (L101_) {
    SATTR(clear_self1,loc,0);
   }
   str_in_self1 = str_self2;
   str_in_s1 = buf2;
   ret_val8 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
   buf2 = ret_val8;
   str_self3 = buf2;
   ret_val9 = STR_fr1097270334(((STR) NULL), str_self3);
   ret_val7 = ret_val9;
   plus_sarg1 = ret_val7;
   ret_val10 = STR_ap2004550586(plus_self1, plus_sarg1);
   ret_val6 = ret_val10;
   res = FSTR_p1752847026(L8, ret_val6);
  }
  L121_=INTPLUS(i,1); 
  i = L121_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self4 = res;
 ret_val11 = STR_fr1097270334(((STR) NULL), str_self4);
 ret_val = ret_val11;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYA175904446(ARRAYA1347292543 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 AM_FORMAL_ARG e;
 ARRAYA1347292543 elt_str_self;
 AM_FORMAL_ARG elt_str_e;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 INT str_self = INT_zero;
 STR ret_val2;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val3;
 FSTR str_self1;
 STR ret_val4;
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 ARRAYA1347292543 elt_str_self1;
 AM_FORMAL_ARG elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val6;
 INT str_self2 = INT_zero;
 STR ret_val7;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val8;
 FSTR str_self3;
 STR ret_val9;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val10;
 FSTR str_self4;
 STR ret_val11;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 AM_FORMAL_ARG L41_;
 BOOL L51_;
 extern STR Unprintable;
 BOOL L6;
 BOOL L71_;
 extern STR name3;
 FSTR L8;
 extern STR Unprintable;
 BOOL L9;
 BOOL L101_;
 INT L121_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYA1347292543)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(AM_FORMAL_ARG)ARR((ARRAYA1347292543)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   plus_self = ((STR) &Unprintable);
   str_self = elt_str_i;
   clear_self = buf1;
   L6 = (clear_self==((FSTR) NULL));
   L71_=!(L6); 
   if (L71_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val3;
   str_self1 = buf1;
   ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val2 = ret_val4;
   plus_sarg = ret_val2;
   ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val1 = ret_val5;
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L8 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   plus_self1 = ((STR) &Unprintable);
   str_self2 = elt_str_i1;
   clear_self1 = buf2;
   L9 = (clear_self1==((FSTR) NULL));
   L101_=!(L9); 
   if (L101_) {
    SATTR(clear_self1,loc,0);
   }
   str_in_self1 = str_self2;
   str_in_s1 = buf2;
   ret_val8 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
   buf2 = ret_val8;
   str_self3 = buf2;
   ret_val9 = STR_fr1097270334(((STR) NULL), str_self3);
   ret_val7 = ret_val9;
   plus_sarg1 = ret_val7;
   ret_val10 = STR_ap2004550586(plus_self1, plus_sarg1);
   ret_val6 = ret_val10;
   res = FSTR_p1752847026(L8, ret_val6);
  }
  L121_=INTPLUS(i,1); 
  i = L121_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self4 = res;
 ret_val11 = STR_fr1097270334(((STR) NULL), str_self4);
 ret_val = ret_val11;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYA1826145484(ARRAYAM_CALL_ARG self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 AM_CALL_ARG e;
 ARRAYAM_CALL_ARG elt_str_self;
 AM_CALL_ARG elt_str_e;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 INT str_self = INT_zero;
 STR ret_val2;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val3;
 FSTR str_self1;
 STR ret_val4;
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 ARRAYAM_CALL_ARG elt_str_self1;
 AM_CALL_ARG elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val6;
 INT str_self2 = INT_zero;
 STR ret_val7;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val8;
 FSTR str_self3;
 STR ret_val9;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val10;
 FSTR str_self4;
 STR ret_val11;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 AM_CALL_ARG L41_;
 BOOL L51_;
 extern STR Unprintable;
 BOOL L6;
 BOOL L71_;
 extern STR name3;
 FSTR L8;
 extern STR Unprintable;
 BOOL L9;
 BOOL L101_;
 INT L121_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYAM_CALL_ARG)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(AM_CALL_ARG)ARR((ARRAYAM_CALL_ARG)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   plus_self = ((STR) &Unprintable);
   str_self = elt_str_i;
   clear_self = buf1;
   L6 = (clear_self==((FSTR) NULL));
   L71_=!(L6); 
   if (L71_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val3;
   str_self1 = buf1;
   ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val2 = ret_val4;
   plus_sarg = ret_val2;
   ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val1 = ret_val5;
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L8 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   plus_self1 = ((STR) &Unprintable);
   str_self2 = elt_str_i1;
   clear_self1 = buf2;
   L9 = (clear_self1==((FSTR) NULL));
   L101_=!(L9); 
   if (L101_) {
    SATTR(clear_self1,loc,0);
   }
   str_in_self1 = str_self2;
   str_in_s1 = buf2;
   ret_val8 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
   buf2 = ret_val8;
   str_self3 = buf2;
   ret_val9 = STR_fr1097270334(((STR) NULL), str_self3);
   ret_val7 = ret_val9;
   plus_sarg1 = ret_val7;
   ret_val10 = STR_ap2004550586(plus_self1, plus_sarg1);
   ret_val6 = ret_val10;
   res = FSTR_p1752847026(L8, ret_val6);
  }
  L121_=INTPLUS(i,1); 
  i = L121_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self4 = res;
 ret_val11 = STR_fr1097270334(((STR) NULL), str_self4);
 ret_val = ret_val11;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYA1836477836(ARRAYAM_CURSOR self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 AM_CURSOR e;
 ARRAYAM_CURSOR elt_str_self;
 AM_CURSOR elt_str_e;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 INT str_self = INT_zero;
 STR ret_val2;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val3;
 FSTR str_self1;
 STR ret_val4;
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 ARRAYAM_CURSOR elt_str_self1;
 AM_CURSOR elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val6;
 INT str_self2 = INT_zero;
 STR ret_val7;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val8;
 FSTR str_self3;
 STR ret_val9;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val10;
 FSTR str_self4;
 STR ret_val11;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 AM_CURSOR L41_;
 BOOL L51_;
 extern STR Unprintable;
 BOOL L6;
 BOOL L71_;
 extern STR name3;
 FSTR L8;
 extern STR Unprintable;
 BOOL L9;
 BOOL L101_;
 INT L121_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYAM_CURSOR)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(AM_CURSOR)ARR((ARRAYAM_CURSOR)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   plus_self = ((STR) &Unprintable);
   str_self = elt_str_i;
   clear_self = buf1;
   L6 = (clear_self==((FSTR) NULL));
   L71_=!(L6); 
   if (L71_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val3;
   str_self1 = buf1;
   ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val2 = ret_val4;
   plus_sarg = ret_val2;
   ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val1 = ret_val5;
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L8 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   plus_self1 = ((STR) &Unprintable);
   str_self2 = elt_str_i1;
   clear_self1 = buf2;
   L9 = (clear_self1==((FSTR) NULL));
   L101_=!(L9); 
   if (L101_) {
    SATTR(clear_self1,loc,0);
   }
   str_in_self1 = str_self2;
   str_in_s1 = buf2;
   ret_val8 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
   buf2 = ret_val8;
   str_self3 = buf2;
   ret_val9 = STR_fr1097270334(((STR) NULL), str_self3);
   ret_val7 = ret_val9;
   plus_sarg1 = ret_val7;
   ret_val10 = STR_ap2004550586(plus_self1, plus_sarg1);
   ret_val6 = ret_val10;
   res = FSTR_p1752847026(L8, ret_val6);
  }
  L121_=INTPLUS(i,1); 
  i = L121_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self4 = res;
 ret_val11 = STR_fr1097270334(((STR) NULL), str_self4);
 ret_val = ret_val11;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYA56089049(ARRAYA36280886 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 AM_CURSOR_POS e;
 ARRAYA36280886 elt_str_self;
 AM_CURSOR_POS elt_str_e;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 INT str_self = INT_zero;
 STR ret_val2;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val3;
 FSTR str_self1;
 STR ret_val4;
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 ARRAYA36280886 elt_str_self1;
 AM_CURSOR_POS elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val6;
 INT str_self2 = INT_zero;
 STR ret_val7;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val8;
 FSTR str_self3;
 STR ret_val9;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val10;
 FSTR str_self4;
 STR ret_val11;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 AM_CURSOR_POS L41_;
 BOOL L51_;
 extern STR Unprintable;
 BOOL L6;
 BOOL L71_;
 extern STR name3;
 FSTR L8;
 extern STR Unprintable;
 BOOL L9;
 BOOL L101_;
 INT L121_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYA36280886)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(AM_CURSOR_POS)ARR((ARRAYA36280886)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   plus_self = ((STR) &Unprintable);
   str_self = elt_str_i;
   clear_self = buf1;
   L6 = (clear_self==((FSTR) NULL));
   L71_=!(L6); 
   if (L71_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val3;
   str_self1 = buf1;
   ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val2 = ret_val4;
   plus_sarg = ret_val2;
   ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val1 = ret_val5;
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L8 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   plus_self1 = ((STR) &Unprintable);
   str_self2 = elt_str_i1;
   clear_self1 = buf2;
   L9 = (clear_self1==((FSTR) NULL));
   L101_=!(L9); 
   if (L101_) {
    SATTR(clear_self1,loc,0);
   }
   str_in_self1 = str_self2;
   str_in_s1 = buf2;
   ret_val8 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
   buf2 = ret_val8;
   str_self3 = buf2;
   ret_val9 = STR_fr1097270334(((STR) NULL), str_self3);
   ret_val7 = ret_val9;
   plus_sarg1 = ret_val7;
   ret_val10 = STR_ap2004550586(plus_self1, plus_sarg1);
   ret_val6 = ret_val10;
   res = FSTR_p1752847026(L8, ret_val6);
  }
  L121_=INTPLUS(i,1); 
  i = L121_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self4 = res;
 ret_val11 = STR_fr1097270334(((STR) NULL), str_self4);
 ret_val = ret_val11;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYA92706474(ARRAYA132553625 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 AM_BND367211769 e;
 ARRAYA132553625 elt_str_self;
 AM_BND367211769 elt_str_e;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 INT str_self = INT_zero;
 STR ret_val2;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val3;
 FSTR str_self1;
 STR ret_val4;
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 ARRAYA132553625 elt_str_self1;
 AM_BND367211769 elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val6;
 INT str_self2 = INT_zero;
 STR ret_val7;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val8;
 FSTR str_self3;
 STR ret_val9;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val10;
 FSTR str_self4;
 STR ret_val11;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 AM_BND367211769 L41_;
 BOOL L51_;
 extern STR Unprintable;
 BOOL L6;
 BOOL L71_;
 extern STR name3;
 FSTR L8;
 extern STR Unprintable;
 BOOL L9;
 BOOL L101_;
 INT L121_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYA132553625)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(AM_BND367211769)ARR((ARRAYA132553625)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   plus_self = ((STR) &Unprintable);
   str_self = elt_str_i;
   clear_self = buf1;
   L6 = (clear_self==((FSTR) NULL));
   L71_=!(L6); 
   if (L71_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val3;
   str_self1 = buf1;
   ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val2 = ret_val4;
   plus_sarg = ret_val2;
   ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val1 = ret_val5;
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L8 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   plus_self1 = ((STR) &Unprintable);
   str_self2 = elt_str_i1;
   clear_self1 = buf2;
   L9 = (clear_self1==((FSTR) NULL));
   L101_=!(L9); 
   if (L101_) {
    SATTR(clear_self1,loc,0);
   }
   str_in_self1 = str_self2;
   str_in_s1 = buf2;
   ret_val8 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
   buf2 = ret_val8;
   str_self3 = buf2;
   ret_val9 = STR_fr1097270334(((STR) NULL), str_self3);
   ret_val7 = ret_val9;
   plus_sarg1 = ret_val7;
   ret_val10 = STR_ap2004550586(plus_self1, plus_sarg1);
   ret_val6 = ret_val10;
   res = FSTR_p1752847026(L8, ret_val6);
  }
  L121_=INTPLUS(i,1); 
  i = L121_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self4 = res;
 ret_val11 = STR_fr1097270334(((STR) NULL), str_self4);
 ret_val = ret_val11;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dINLINE_ITER_SIG INLINE1838056742(INLINE_ITER_TBL self, SIG q) {
 dINLINE_ITER_SIG ret_val;
 INT h = INT_zero;
 dINLINE_ITER_SIG e;
 dINLINE_ITER_SIG e1;
 INLINE_ITER_TBL is_elt_nil_self;
 dINLINE_ITER_SIG is_elt_nil_e;
 BOOL ret_val1 = BOOL_zero;
 INLINE_ITER_TBL is_elt_nil_self1;
 dINLINE_ITER_SIG is_elt_nil_e1;
 BOOL ret_val2 = BOOL_zero;
 INLINE_ITER_TBL elt_nil_self;
 dINLINE_ITER_SIG ret_val3;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 dINLINE_ITER_SIG L71_;
 INT L81_;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 BOOL L131_;
 dINLINE_ITER_SIG L141_;
 INT L151_;
 if ((self==((INLINE_ITER_TBL) NULL))) {
  ret_val = ((dINLINE_ITER_SIG) NULL);
  return ret_val;
 }
 L1 = INLINE1816775773(self, q);
 L21_=ASIZE((INLINE_ITER_TBL)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=(dINLINE_ITER_SIG)ARR((INLINE_ITER_TBL)self,h); 
  e = L71_;
  is_elt_nil_self = self;
  is_elt_nil_e = e;
  ret_val1 = ELT_NI569783157(((ELT_NI1131308118) NULL), is_elt_nil_e);
  if (ret_val1) {
   goto after_loop;
  }
  else {
   if (INLINE105629928(self, q, e)) {
    ret_val = e;
    return ret_val;
   }
  }
  L81_=INTPLUS(h,1); 
  h = L81_;
 }
 after_loop: ;
 L91_=ASIZE((INLINE_ITER_TBL)self); 
 L10 = L91_;
 L111_=INTMINUS(L10,1); 
 L12 = L111_;
 L131_=(h)==(L12); 
 if (L131_) {
  h = 0;
  while (1) {
   L141_=(dINLINE_ITER_SIG)ARR((INLINE_ITER_TBL)self,h); 
   e1 = L141_;
   is_elt_nil_self1 = self;
   is_elt_nil_e1 = e1;
   ret_val2 = ELT_NI569783157(((ELT_NI1131308118) NULL), is_elt_nil_e1);
   if (ret_val2) {
    goto after_loop1;
   }
   else {
    if (INLINE105629928(self, q, e1)) {
     ret_val = e1;
     return ret_val;
    }
   }
   L151_=INTPLUS(h,1); 
   h = L151_;
  }
  after_loop1: ;
 }
 elt_nil_self = self;
 ret_val3 = ELT_NI1331652151(((ELT_NI1131308118) NULL));
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dINLINE_ROUT_SIG INLINE2019550598(INLINE_ROUT_TBL self, SIG q) {
 dINLINE_ROUT_SIG ret_val;
 INT h = INT_zero;
 dINLINE_ROUT_SIG e;
 dINLINE_ROUT_SIG e1;
 INLINE_ROUT_TBL is_elt_nil_self;
 dINLINE_ROUT_SIG is_elt_nil_e;
 BOOL ret_val1 = BOOL_zero;
 INLINE_ROUT_TBL is_elt_nil_self1;
 dINLINE_ROUT_SIG is_elt_nil_e1;
 BOOL ret_val2 = BOOL_zero;
 INLINE_ROUT_TBL elt_nil_self;
 dINLINE_ROUT_SIG ret_val3;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 dINLINE_ROUT_SIG L71_;
 INT L81_;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 BOOL L131_;
 dINLINE_ROUT_SIG L141_;
 INT L151_;
 if ((self==((INLINE_ROUT_TBL) NULL))) {
  ret_val = ((dINLINE_ROUT_SIG) NULL);
  return ret_val;
 }
 L1 = INLINE1047927461(self, q);
 L21_=ASIZE((INLINE_ROUT_TBL)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=(dINLINE_ROUT_SIG)ARR((INLINE_ROUT_TBL)self,h); 
  e = L71_;
  is_elt_nil_self = self;
  is_elt_nil_e = e;
  ret_val1 = ELT_NI1893940731(((ELT_NI52152274) NULL), is_elt_nil_e);
  if (ret_val1) {
   goto after_loop;
  }
  else {
   if (INLINE522121224(self, q, e)) {
    ret_val = e;
    return ret_val;
   }
  }
  L81_=INTPLUS(h,1); 
  h = L81_;
 }
 after_loop: ;
 L91_=ASIZE((INLINE_ROUT_TBL)self); 
 L10 = L91_;
 L111_=INTMINUS(L10,1); 
 L12 = L111_;
 L131_=(h)==(L12); 
 if (L131_) {
  h = 0;
  while (1) {
   L141_=(dINLINE_ROUT_SIG)ARR((INLINE_ROUT_TBL)self,h); 
   e1 = L141_;
   is_elt_nil_self1 = self;
   is_elt_nil_e1 = e1;
   ret_val2 = ELT_NI1893940731(((ELT_NI52152274) NULL), is_elt_nil_e1);
   if (ret_val2) {
    goto after_loop1;
   }
   else {
    if (INLINE522121224(self, q, e1)) {
     ret_val = e1;
     return ret_val;
    }
   }
   L151_=INTPLUS(h,1); 
   h = L151_;
  }
  after_loop1: ;
 }
 elt_nil_self = self;
 ret_val3 = ELT_NI706507177(((ELT_NI52152274) NULL));
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_ARR1099277377(AM_ARRAY_EXPR self) {
 dTP ret_val;
 ret_val = ATTR(self,tp_at);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_BND192433970(AM_BND260301329 self) {
 dTP ret_val;
 ret_val = ATTR(ATTR(self,br_tp),ret);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_BND540727053(AM_BND1124877163 self) {
 dTP ret_val;
 ret_val = ATTR(self,tp_at);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_BND95745606(AM_BND367211769 self) {
 dTP ret_val;
 ret_val = ATTR(ATTR(self,bi_tp),ret);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_EXT195226505(AM_EXT_CALL_EXPR self) {
 dTP ret_val;
 ret_val = ATTR(ATTR(self,fun),ret);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_ITE597285895(AM_ITE1809135850 self) {
 dTP ret_val;
 ret_val = ATTR(ATTR(self,fun),ret);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 1

ABSTRACT_LAYOUT ARRAYA967821567(ARRAYA967821567_frame frame) {
 ABSTRACT_LAYOUT dummy = ((ABSTRACT_LAYOUT) NULL);
 ABSTRACT_LAYOUT aI_u_I;
 ABSTRACT_LAYOUT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYA1870808380)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYA1870808380)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

AM_BND1124877163 ARRAYA945146506(ARRAYA945146506_frame frame) {
 AM_BND1124877163 dummy = ((AM_BND1124877163) NULL);
 AM_BND1124877163 aI_u_I;
 AM_BND1124877163 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYA855692714)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYA855692714)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

AM_BND367211769 ARRAYA1632293813(ARRAYA1632293813_frame frame) {
 AM_BND367211769 dummy = ((AM_BND367211769) NULL);
 AM_BND367211769 aI_u_I;
 AM_BND367211769 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYA132553625)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYA132553625)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

AM_CALL_ARG AM_BND1502113605(AM_BND1502113605_frame frame) {
 AM_CALL_ARG dummy = ((AM_CALL_ARG) NULL);
 AM_CALL_ARG aI_u_I;
 AM_CALL_ARG L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((AM_BND1124877163)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((AM_BND1124877163)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

AM_CALL_ARG AM_BND745688768(AM_BND745688768_frame frame) {
 AM_CALL_ARG dummy = ((AM_CALL_ARG) NULL);
 AM_CALL_ARG aI_u_I;
 AM_CALL_ARG L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((AM_BND367211769)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((AM_BND367211769)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

AM_CALL_ARG AM_BND956869000(AM_BND956869000_frame frame) {
 AM_CALL_ARG dummy = ((AM_CALL_ARG) NULL);
 AM_CALL_ARG aI_u_I;
 AM_CALL_ARG L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((AM_BND260301329)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((AM_BND260301329)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

AM_CALL_ARG AM_EXT150734657(AM_EXT150734657_frame frame) {
 AM_CALL_ARG dummy = ((AM_CALL_ARG) NULL);
 AM_CALL_ARG aI_u_I;
 AM_CALL_ARG L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((AM_EXT_CALL_EXPR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((AM_EXT_CALL_EXPR)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

AM_CALL_ARG AM_EXT204419640(AM_EXT204419640_frame frame) {
 AM_CALL_ARG dummy = ((AM_CALL_ARG) NULL);
 AM_CALL_ARG aI_u_I;
 AM_CALL_ARG L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ic=frame->arg1-1;frame->I_u_Is=frame->self==0?0:ASIZE((AM_EXT_CALL_EXPR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(++frame->I_u_Ic>=frame->I_u_Is)  goto after_loop; aI_u_I=ARR((AM_EXT_CALL_EXPR)frame->self,frame->I_u_Ic); frame->i_I_u_I++;
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

AM_CALL_ARG AM_ITE235385604(AM_ITE235385604_frame frame) {
 AM_CALL_ARG dummy = ((AM_CALL_ARG) NULL);
 AM_CALL_ARG aI_u_I;
 AM_CALL_ARG L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ic=frame->arg1-1;frame->I_u_Is=frame->self==0?0:ASIZE((AM_ITE1809135850)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(++frame->I_u_Ic>=frame->I_u_Is)  goto after_loop; aI_u_I=ARR((AM_ITE1809135850)frame->self,frame->I_u_Ic); frame->i_I_u_I++;
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

AM_CALL_ARG AM_ITE35020325(AM_ITE35020325_frame frame) {
 AM_CALL_ARG dummy = ((AM_CALL_ARG) NULL);
 AM_CALL_ARG aI_u_I;
 AM_CALL_ARG L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((AM_ITE1809135850)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((AM_ITE1809135850)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

AM_CALL_ARG ARRAYA44788561(ARRAYA44788561_frame frame) {
 AM_CALL_ARG dummy = ((AM_CALL_ARG) NULL);
 AM_CALL_ARG aI_u_I;
 AM_CALL_ARG L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYAM_CALL_ARG)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYAM_CALL_ARG)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

AM_CURSOR ARRAYA900901929(ARRAYA900901929_frame frame) {
 AM_CURSOR dummy = ((AM_CURSOR) NULL);
 AM_CURSOR aI_u_I;
 AM_CURSOR L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYAM_CURSOR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYAM_CURSOR)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

AM_CURSOR_POS ARRAYA2146369463(ARRAYA2146369463_frame frame) {
 AM_CURSOR_POS dummy = ((AM_CURSOR_POS) NULL);
 AM_CURSOR_POS aI_u_I;
 AM_CURSOR_POS L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYA36280886)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYA36280886)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

AM_FORMAL_ARG AM_ROU1235468764(AM_ROU1235468764_frame frame) {
 AM_FORMAL_ARG dummy = ((AM_FORMAL_ARG) NULL);
 AM_FORMAL_ARG aI_u_I;
 AM_FORMAL_ARG L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ic=frame->arg1-1;frame->I_u_Is=frame->self==0?0:ASIZE((AM_ROUT_DEF)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(++frame->I_u_Ic>=frame->I_u_Is)  goto after_loop; aI_u_I=ARR((AM_ROUT_DEF)frame->self,frame->I_u_Ic); frame->i_I_u_I++;
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

AM_FORMAL_ARG AM_ROU948739923(AM_ROU948739923_frame frame) {
 AM_FORMAL_ARG dummy = ((AM_FORMAL_ARG) NULL);
 AM_FORMAL_ARG aI_u_I;
 AM_FORMAL_ARG L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((AM_ROUT_DEF)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((AM_ROUT_DEF)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

AM_FORMAL_ARG ARRAYA1315754333(ARRAYA1315754333_frame frame) {
 AM_FORMAL_ARG dummy = ((AM_FORMAL_ARG) NULL);
 AM_FORMAL_ARG aI_u_I;
 AM_FORMAL_ARG L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYA1347292543)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYA1347292543)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

INT AM_ARR1990213312(AM_ARR1990213312_frame frame) {
 INT dummy = INT_zero;
 INT rI_u_I;
 INT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((AM_ARRAY_EXPR)frame->self); 
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

INT AM_BND1037254841(AM_BND1037254841_frame frame) {
 INT dummy = INT_zero;
 INT rI_u_I;
 INT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((AM_BND260301329)frame->self); 
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

INT AM_BND1640868266(AM_BND1640868266_frame frame) {
 INT dummy = INT_zero;
 INT rI_u_I;
 INT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((AM_BND1124877163)frame->self); 
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

INT AM_BND83643009(AM_BND83643009_frame frame) {
 INT dummy = INT_zero;
 INT rI_u_I;
 INT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((AM_BND367211769)frame->self); 
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

INT AM_EXT1469221262(AM_EXT1469221262_frame frame) {
 INT dummy = INT_zero;
 INT rI_u_I;
 INT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((AM_EXT_CALL_EXPR)frame->self); 
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

INT AM_ITE1159913222(AM_ITE1159913222_frame frame) {
 INT dummy = INT_zero;
 INT rI_u_I;
 INT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((AM_ITE1809135850)frame->self); 
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

INT AM_ROU1569581452(AM_ROU1569581452_frame frame) {
 INT dummy = INT_zero;
 INT rI_u_I;
 INT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((AM_ROUT_DEF)frame->self); 
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

INT ARRAYA537529312(ARRAYA537529312_frame frame) {
 INT dummy = INT_zero;
 INT rI_u_I;
 INT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYAM_CALL_ARG)frame->self); 
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

TUPSIGSIG FSETTU845419336(FSETTU845419336_frame frame) {
 TUPSIGSIG dummy = TUPSIGSIG_zero;
 BOOL L1;
 BOOL L21_;
 TUPSIGSIG L5;
 TUPSIGSIG aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 BOOL L111_;
 TUPSIGSIG L121_;
 INT L131_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  }
 state0:;
 L1 = (frame->self==((FSETTUPSIGSIG) NULL));
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
      frame->L41_br=ASIZE((FSETTUPSIGSIG)frame->L61); 
     }
     if(frame->L31>=frame->L41_br)  goto after_loop; 
     aL41_=ARR((FSETTUPSIGSIG)frame->L61,frame->L31); 
     frame->r = aL41_;
     frame->is_elt_nil_self = frame->self;
     frame->is_elt_nil_e = frame->r;
     frame->is_elt_nil_self1 = ((ELT_NILTUPSIGSIG) NULL);
     frame->is_elt_nil_e1 = frame->is_elt_nil_e;
     frame->ret_val1 = (frame->is_elt_nil_e1.t1==(SIG)0)&&(frame->is_elt_nil_e1.t2==(SIG)0);
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
    L121_=ARR((FSETTUPSIGSIG)frame->self,frame->i); 
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

TUPSIGSIG FSETTU853729645(FSETTU853729645_frame frame) {
 TUPSIGSIG dummy = TUPSIGSIG_zero;
 TUPSIGSIG aI_u_I;
 TUPSIGSIG L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETTUPSIGSIG)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FSETTUPSIGSIG)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

dAM_EXPR AM_ARR535890289(AM_ARR535890289_frame frame) {
 dAM_EXPR dummy = ((dAM_EXPR) NULL);
 dAM_EXPR aI_u_I;
 dAM_EXPR L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((AM_ARRAY_EXPR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((AM_ARRAY_EXPR)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

dINLINE_ITER_SIG INLINE106514693(INLINE106514693_frame frame) {
 dINLINE_ITER_SIG dummy = ((dINLINE_ITER_SIG) NULL);
 BOOL L1;
 BOOL L21_;
 dINLINE_ITER_SIG L5;
 dINLINE_ITER_SIG aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((INLINE_ITER_TBL) NULL));
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
     frame->L41_br=ASIZE((INLINE_ITER_TBL)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((INLINE_ITER_TBL)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_elt_nil_self = frame->self;
    frame->is_elt_nil_e = frame->r;
    frame->ret_val = ELT_NI569783157(((ELT_NI1131308118) NULL), frame->is_elt_nil_e);
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

dINLINE_ITER_SIG INLINE546763677(INLINE546763677_frame frame) {
 dINLINE_ITER_SIG dummy = ((dINLINE_ITER_SIG) NULL);
 dINLINE_ITER_SIG aI_u_I;
 dINLINE_ITER_SIG L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((INLINE_ITER_TBL)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((INLINE_ITER_TBL)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

dINLINE_ROUT_SIG INLINE1519043837(INLINE1519043837_frame frame) {
 dINLINE_ROUT_SIG dummy = ((dINLINE_ROUT_SIG) NULL);
 dINLINE_ROUT_SIG aI_u_I;
 dINLINE_ROUT_SIG L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((INLINE_ROUT_TBL)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((INLINE_ROUT_TBL)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

dINLINE_ROUT_SIG INLINE600707605(INLINE600707605_frame frame) {
 dINLINE_ROUT_SIG dummy = ((dINLINE_ROUT_SIG) NULL);
 BOOL L1;
 BOOL L21_;
 dINLINE_ROUT_SIG L5;
 dINLINE_ROUT_SIG aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((INLINE_ROUT_TBL) NULL));
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
     frame->L41_br=ASIZE((INLINE_ROUT_TBL)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((INLINE_ROUT_TBL)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_elt_nil_self = frame->self;
    frame->is_elt_nil_e = frame->r;
    frame->ret_val = ELT_NI1893940731(((ELT_NI52152274) NULL), frame->is_elt_nil_e);
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

dLAYOUT FSETdL416996713(FSETdL416996713_frame frame) {
 dLAYOUT dummy = ((dLAYOUT) NULL);
 dLAYOUT aI_u_I;
 dLAYOUT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETdLAYOUT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FSETdLAYOUT)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

dLAYOUT FSETdL485771726(FSETdL485771726_frame frame) {
 dLAYOUT dummy = ((dLAYOUT) NULL);
 BOOL L1;
 BOOL L21_;
 dLAYOUT L5;
 dLAYOUT aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 BOOL L111_;
 dLAYOUT L121_;
 INT L131_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  }
 state0:;
 L1 = (frame->self==((FSETdLAYOUT) NULL));
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
      frame->L41_br=ASIZE((FSETdLAYOUT)frame->L61); 
     }
     if(frame->L31>=frame->L41_br)  goto after_loop; 
     aL41_=ARR((FSETdLAYOUT)frame->L61,frame->L31); 
     frame->r = aL41_;
     frame->is_elt_nil_self = frame->self;
     frame->is_elt_nil_e = frame->r;
     frame->ret_val = ELT_NI978759818(((ELT_NILdLAYOUT) NULL), frame->is_elt_nil_e);
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
    L121_=(dLAYOUT)ARR((FSETdLAYOUT)frame->self,frame->i); 
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

dTP FSETdT1957050743(FSETdT1957050743_frame frame) {
 dTP dummy = ((dTP) NULL);
 dTP aI_u_I;
 dTP L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETdTP)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FSETdTP)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

dTP FSETdTP_eltbrdTP(FSETdTP_eltbrdTP_frame frame) {
 dTP dummy = ((dTP) NULL);
 BOOL L1;
 BOOL L21_;
 dTP L5;
 dTP aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 BOOL L111_;
 dTP L121_;
 INT L131_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  }
 state0:;
 L1 = (frame->self==((FSETdTP) NULL));
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
      frame->L41_br=ASIZE((FSETdTP)frame->L61); 
     }
     if(frame->L31>=frame->L41_br)  goto after_loop; 
     aL41_=ARR((FSETdTP)frame->L61,frame->L31); 
     frame->r = aL41_;
     frame->is_elt_nil_self = frame->self;
     frame->is_elt_nil_e = frame->r;
     frame->ret_val = ELT_NI125473534(((ELT_NILdTP) NULL), frame->is_elt_nil_e);
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
    L121_=(dTP)ARR((FSETdTP)frame->self,frame->i); 
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

void AM_ARR2088295271(AM_ARR2088295271_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((AM_ARRAY_EXPR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((AM_ARRAY_EXPR)frame->self,frame->i_I_u_I,(dAM_EXPR)frame->arg1); frame->i_I_u_I++;
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

void AM_BND2071008937(AM_BND2071008937_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((AM_BND1124877163)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((AM_BND1124877163)frame->self,frame->i_I_u_I,(AM_CALL_ARG)frame->arg1); frame->i_I_u_I++;
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

void AM_BND235024246(AM_BND235024246_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((AM_BND260301329)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((AM_BND260301329)frame->self,frame->i_I_u_I,(AM_CALL_ARG)frame->arg1); frame->i_I_u_I++;
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

void AM_BND23844014(AM_BND23844014_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((AM_BND367211769)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((AM_BND367211769)frame->self,frame->i_I_u_I,(AM_CALL_ARG)frame->arg1); frame->i_I_u_I++;
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

void AM_EXT872579411(AM_EXT872579411_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((AM_EXT_CALL_EXPR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((AM_EXT_CALL_EXPR)frame->self,frame->i_I_u_I,(AM_CALL_ARG)frame->arg1); frame->i_I_u_I++;
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

void AM_ITE756865079(AM_ITE756865079_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((AM_ITE1809135850)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((AM_ITE1809135850)frame->self,frame->i_I_u_I,(AM_CALL_ARG)frame->arg1); frame->i_I_u_I++;
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

void FSETTU284653231(FSETTU284653231_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETTUPSIGSIG)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FSETTUPSIGSIG)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void FSETdL950804059(FSETdL950804059_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETdLAYOUT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FSETdLAYOUT)frame->self,frame->i_I_u_I,(dLAYOUT)frame->arg1); frame->i_I_u_I++;
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

void FSETdT1539121061(FSETdT1539121061_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETdTP)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FSETdTP)frame->self,frame->i_I_u_I,(dTP)frame->arg1); frame->i_I_u_I++;
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

void INLINE1362413491(INLINE1362413491_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((INLINE_ROUT_TBL)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((INLINE_ROUT_TBL)frame->self,frame->i_I_u_I,(dINLINE_ROUT_SIG)frame->arg1); frame->i_I_u_I++;
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

void INLINE390133331(INLINE390133331_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((INLINE_ITER_TBL)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((INLINE_ITER_TBL)frame->self,frame->i_I_u_I,(dINLINE_ITER_SIG)frame->arg1); frame->i_I_u_I++;
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
