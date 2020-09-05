#include "sather.h"

/* Layouts */

typedef struct dAM_EXPR_struct {
 OB_HEADER header;
 } *dAM_EXPR;

typedef struct dAM_STMT_struct {
 OB_HEADER header;
 } *dAM_STMT;

typedef struct dGENERATE_AM_struct {
 OB_HEADER header;
 } *dGENERATE_AM;

typedef struct dGET_MAIN_SIG_struct {
 OB_HEADER header;
 } *dGET_MAIN_SIG;

typedef struct dGET_OPTIONS_struct {
 OB_HEADER header;
 } *dGET_OPTIONS;

typedef struct dLAYOUT_struct {
 OB_HEADER header;
 } *dLAYOUT;

typedef struct dMODE_struct {
 OB_HEADER header;
 } *dMODE;

typedef struct dSTR_struct {
 OB_HEADER header;
 } *dSTR;

typedef struct dTP_struct {
 OB_HEADER header;
 } *dTP;

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

typedef struct ARRAYARG_struct {/* layout for ARRAY{ARG} */
 OB_HEADER header;
 INT asize;
 struct ARG_struct *arr_part[1];
 } *ARRAYARG;

typedef struct ARRAYARRAYSTR_struct {/* layout for ARRAY{ARRAY{STR}} */
 OB_HEADER header;
 INT asize;
 struct ARRAYSTR_struct *arr_part[1];
 } *ARRAYARRAYSTR;

typedef struct ARRAYSTR_struct {/* layout for ARRAY{STR} */
 OB_HEADER header;
 INT asize;
 STR arr_part[1];
 } *ARRAYSTR;

typedef struct BIND_FORTRAN_struct {/* layout for BIND_FORTRAN */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *BIND_FORTRAN;

typedef struct CONFIG_DEF_struct {/* layout for CONFIG_DEF */
 OB_HEADER header;
 struct ARRAYARRAYSTR_struct *expr;
 STR name1;
 } *CONFIG_DEF;

typedef struct CONFIG_TBL_struct {/* layout for CONFIG_TBL */
 OB_HEADER header;
 struct FMAPSTRdCONFIG_struct *tbl;
 } *CONFIG_TBL;

typedef struct ELT_struct {/* layout for ELT */
 OB_HEADER header;
 struct dAS_CLASS_ELT_struct *as;
 struct SIG_struct *sig1;
 struct SIG_struct *srcsig;
 struct TP_CONTEXT_struct *con;
 BOOL is_external;
 BOOL is_private;
 } *ELT;

typedef struct ELT_NILELT_struct {/* layout for ELT_NIL{ELT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILELT;

typedef struct ELT_NILIDENT_struct {/* layout for ELT_NIL{IDENT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILIDENT;

typedef struct FILE1_struct {/* layout for FILE */
 OB_HEADER header;
 EXT_OB fp;
 } *FILE1;

typedef struct FILE_MANGLE_struct {/* layout for FILE_MANGLE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *FILE_MANGLE;

typedef struct GENERATE_AM_struct {/* layout for GENERATE_AM */
 OB_HEADER header;
 struct FLISTSIG_struct *sig_list;
 struct FSETSIG_struct *sig_inprocout;
 struct FSETSIG_struct *sig_recursive;
 struct GLOBAL_TBL_struct *global_tbl;
 struct PROG_struct *prog;
 BOOL only_check;
 BOOL optimize;
 } *GENERATE_AM;

typedef struct IDENT_struct {/* layout for IDENT */
 STR str;
 } IDENT;
static IDENT IDENT_zero;

typedef struct IDENT_boxed_struct {
 OB_HEADER header;
 IDENT immutable_part;
 } *IDENT_boxed;

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

typedef struct INT_timesb_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 BOOL f_I_u_I; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *INT_timesb_frame;

typedef struct INT_timesbrINT_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *INT_timesbrINT_frame;

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

typedef struct LAYOUT_TBL_struct {/* layout for LAYOUT_TBL */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *LAYOUT_TBL;

typedef struct MANGLE_struct {/* layout for MANGLE */
 OB_HEADER header;
 struct FMAPdOBNAMESPACE_struct *namespaces;
 struct FSETSTR_struct *forbidden;
 struct FSETSTR_struct *used_by_local;
 INT max_identifier_l;
 struct NAMESPACE_struct *global_space;
 struct PROG_struct *prog;
 BOOL append_hash_to_i;
 } *MANGLE;

typedef struct NAMESPACE_struct {/* layout for NAMESPACE */
 OB_HEADER header;
 struct FMAPSTRINT_struct *uniques;
 struct FSETSTR_struct *set;
 INT counter;
 struct NAMEMAP_struct *map;
 } *NAMESPACE;

typedef struct OUT_struct {/* layout for OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OUT;

typedef struct OUT_MODE_struct {/* layout for OUT_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OUT_MODE;

typedef struct PRINT_OB_struct {/* layout for PRINT_OB */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *PRINT_OB;

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

typedef struct STR_se405450305_frame_struct {
 INT state;
 STR self;/* Formal argument: self */
 dSTR arg1;/* Formal argument: s */
 STR ret_val1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 } *STR_se405450305_frame;

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

typedef struct TUPdOBNAMESPACE_struct {/* layout for TUP{$OB,NAMESPACE} */
 OB t1;
 struct NAMESPACE_struct *t2;
 } TUPdOBNAMESPACE;
static TUPdOBNAMESPACE TUPdOBNAMESPACE_zero;

typedef struct TUPdOBNAMESPACE_boxed_struct {
 OB_HEADER header;
 TUPdOBNAMESPACE immutable_part;
 } *TUPdOBNAMESPACE_boxed;

typedef struct TUPdOBSTR_struct {/* layout for TUP{$OB,STR} */
 OB t1;
 STR t2;
 } TUPdOBSTR;
static TUPdOBSTR TUPdOBSTR_zero;

typedef struct TUPdOBSTR_boxed_struct {
 OB_HEADER header;
 TUPdOBSTR immutable_part;
 } *TUPdOBSTR_boxed;

typedef struct TUPdTPCODE_FILE_struct {/* layout for TUP{$TP,CODE_FILE} */
 struct dTP_struct *t1;
 struct CODE_FILE_struct *t2;
 } TUPdTPCODE_FILE;
static TUPdTPCODE_FILE TUPdTPCODE_FILE_zero;

typedef struct TUPdTPCODE_FILE_boxed_struct {
 OB_HEADER header;
 TUPdTPCODE_FILE immutable_part;
 } *TUPdTPCODE_FILE_boxed;

typedef struct TUPdTPINT_struct {/* layout for TUP{$TP,INT} */
 struct dTP_struct *t1;
 INT t2;
 } TUPdTPINT;
static TUPdTPINT TUPdTPINT_zero;

typedef struct TUPdTPINT_boxed_struct {
 OB_HEADER header;
 TUPdTPINT immutable_part;
 } *TUPdTPINT_boxed;

typedef struct TUPAM_1024994801_struct {/* layout for TUP{AM_ROUT_DEF,FLIST{AM_BND_ITER_CALL_EXPR}} */
 struct AM_ROUT_DEF_struct *t1;
 struct FLISTA1409846210_struct *t2;
 } TUPAM_1024994801;
static TUPAM_1024994801 TUPAM_1024994801_zero;

typedef struct TUPAM_1024994801_boxed_struct {
 OB_HEADER header;
 TUPAM_1024994801 immutable_part;
 } *TUPAM_1024994801_boxed;

typedef struct TUPAM_653469574_struct {/* layout for TUP{AM_ROUT_DEF,FLIST{AM_ITER_CALL_EXPR}} */
 struct AM_ROUT_DEF_struct *t1;
 struct FLISTA1062334999_struct *t2;
 } TUPAM_653469574;
static TUPAM_653469574 TUPAM_653469574_zero;

typedef struct TUPAM_653469574_boxed_struct {
 OB_HEADER header;
 TUPAM_653469574 immutable_part;
 } *TUPAM_653469574_boxed;

typedef struct TUPIDENTdTP_struct {/* layout for TUP{IDENT,$TP} */
 struct dTP_struct *t2;
 struct IDENT_struct t1;
 } TUPIDENTdTP;
static TUPIDENTdTP TUPIDENTdTP_zero;

typedef struct TUPIDENTdTP_boxed_struct {
 OB_HEADER header;
 TUPIDENTdTP immutable_part;
 } *TUPIDENTdTP_boxed;

typedef struct TUPSIG578692189_struct {/* layout for TUP{SIG,AM_ROUT_DEF} */
 struct AM_ROUT_DEF_struct *t2;
 struct SIG_struct *t1;
 } TUPSIG578692189;
static TUPSIG578692189 TUPSIG578692189_zero;

typedef struct TUPSIG578692189_boxed_struct {
 OB_HEADER header;
 TUPSIG578692189 immutable_part;
 } *TUPSIG578692189_boxed;

typedef struct TUPSIGSTR_struct {/* layout for TUP{SIG,STR} */
 struct SIG_struct *t1;
 STR t2;
 } TUPSIGSTR;
static TUPSIGSTR TUPSIGSTR_zero;

typedef struct TUPSIGSTR_boxed_struct {
 OB_HEADER header;
 TUPSIGSTR immutable_part;
 } *TUPSIGSTR_boxed;

typedef struct TUPSTRINT_struct {/* layout for TUP{STR,INT} */
 INT t2;
 STR t1;
 } TUPSTRINT;
static TUPSTRINT TUPSTRINT_zero;

typedef struct TUPSTRINT_boxed_struct {
 OB_HEADER header;
 TUPSTRINT immutable_part;
 } *TUPSTRINT_boxed;

typedef struct TUPSTRSTR_struct {/* layout for TUP{STR,STR} */
 STR t1;
 STR t2;
 } TUPSTRSTR;
static TUPSTRSTR TUPSTRSTR_zero;

typedef struct TUPSTRSTR_boxed_struct {
 OB_HEADER header;
 TUPSTRSTR immutable_part;
 } *TUPSTRSTR_boxed;

typedef struct UNIX_struct {/* layout for UNIX */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *UNIX;

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

typedef struct AM_OB_DEF_struct {/* layout for AM_OB_DEF */
 OB_HEADER header;
 struct dTP_struct *arr;
 struct dTP_struct *tp;
 struct ARRAYIDENT_struct *sorted_at;
 struct FMAPIDENTdTP_struct *at;
 INT asize;
 struct SFILE_ID_struct source1;
 } *AM_OB_DEF;

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

typedef struct AM_ROU948739923_frame_struct {
 INT state;
 AM_ROUT_DEF self;/* Formal argument: self */
 AM_FORMAL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ROU948739923_frame;

typedef struct AM_YIELD_STMT_struct {/* layout for AM_YIELD_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *val1;
 struct dAM_STMT_struct *next;
 INT ret;
 struct SFILE_ID_struct source1;
 } *AM_YIELD_STMT;

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

typedef struct ARRAYA1729769776_frame_struct {
 INT state;
 ARRAYARRAYSTR self;/* Formal argument: self */
 ARRAYSTR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA1729769776_frame;

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

typedef struct ARRAYS1578977701_frame_struct {
 INT state;
 ARRAYSTR self;/* Formal argument: self */
 STR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYS1578977701_frame;

typedef struct CGEN_struct {/* layout for CGEN */
 OB_HEADER header;
 struct AM_ROUT_DEF_struct *current_am_rout_;
 struct ARRAYSTR_struct *current_arg_list;
 struct ARRAYSTR_struct *current_c_arg_li;
 struct ARRAYSTR_struct *current_fortran_;
 struct CODE_FILE_struct *code_c;
 struct CODE_FILE_struct *comp_options;
 struct CODE_FILE_struct *dispatch_c;
 struct CODE_FILE_struct *globals_c;
 struct CODE_FILE_struct *link_options;
 struct CODE_FILE_struct *makefile;
 struct CODE_FILE_struct *print_c;
 struct CODE_FILE_struct *sather_h;
 struct CODE_FILE_struct *strings_c;
 struct CODE_FILE_struct *system_c;
 struct CODE_FILE_struct *tags_h;
 struct CODE_FILE_struct *unbox_c;
 struct CS_OPTIONS_struct *options1;
 struct FLISTA1893461511_struct *bnd_iter_creates;
 struct FLISTA1893461511_struct *bnd_rout_creates;
 struct FLISTA1893461511_struct *bnd_rout_creates1;
 struct FLISTA725283029_struct *current_iter_out;
 struct FLISTAM_ROUT_DEF_struct *abstract_iters;
 struct FLISTAM_ROUT_DEF_struct *abstract_routs;
 struct FLISTSTR_struct *archive_files;
 struct FLISTSTR_struct *c_flags;
 struct FMAPdTPCODE_FILE_struct *code_files;
 struct FMAPdTPINT_struct *tags;
 struct FMAPAM1244483504_struct *nested_bits;
 struct FMAPAM1787628573_struct *nested_its;
 struct FMAPSI518162669_struct *itersig_map;
 struct FMAPSI518162669_struct *not_emitted;
 struct FMAPSIGSTR_struct *emitted_dispatch;
 struct FMAPSTRFSETSTR_struct *externals;
 struct FMAPSTRSTR_struct *string_constants;
 struct FSETdTP_struct *needs_tag;
 struct FSETAM_ROUT_DEF_struct *emitted_leftover;
 struct FSETAM_ROUT_DEF_struct *leftovers;
 struct FSETSIG_struct *arg_frames;
 struct FSETSIG_struct *emitted_iter_all;
 struct FSETSTR_struct *c_files;
 struct FSETSTR_struct *force_routines;
 struct FSETSTR_struct *object_files;
 struct FSTR_struct *routine_code;
 struct GENERATE_AM_struct *gen;
 INT abstract_calls;
 INT builtin_cntr_n;
 INT concrete_calls;
 INT current_loop_ex_;
 INT ex_nesting;
 INT indent;
 INT inlined_iter_cou;
 INT inlined_pointer_;
 INT inlined_value_ob;
 INT last_lineno;
 INT routine_count;
 INT state_counter;
 INT str_count;
 INT threshold;
 struct MANGLE_struct *mangler;
 struct PROG_struct *prog;
 struct SIG_struct *current_sig;
 struct SIG_struct *main_sig;
 STR code_dir;
 STR comp_home;
 STR current_function;
 STR current_loop;
 STR executable1;
 STR fortran_append_u;
 STR fortran_prepend_;
 STR func_name_commen;
 STR func_name_suffix;
 STR last_file;
 BOOL chk_arith;
 BOOL chk_assert;
 BOOL chk_bounds;
 BOOL chk_invariant;
 BOOL chk_post;
 BOOL chk_pre;
 BOOL chk_return;
 BOOL chk_void;
 BOOL chk_when;
 BOOL debug1;
 BOOL deterministic1;
 BOOL force_all;
 BOOL fortran_bind_fun;
 BOOL func_tables;
 BOOL gen_c;
 BOOL ignore_lock;
 BOOL in_bnd_iter_call;
 BOOL in_bnd_rout_call;
 BOOL in_constant;
 BOOL manual_loop_unlo;
 BOOL manual_unlock;
 BOOL no_destroy;
 BOOL no_destroy_lock;
 BOOL null_segfaults;
 BOOL only_C;
 BOOL only_check;
 BOOL optimize;
 BOOL polling;
 BOOL pretty;
 BOOL print_ob;
 BOOL print_ob_g;
 BOOL saw_outer_return;
 BOOL siva;
 BOOL type_tables;
 } *CGEN;

typedef struct CODE_FILE_struct {/* layout for CODE_FILE */
 OB_HEADER header;
 struct FLISTA1893461511_struct *bnd_iter_creates;
 struct FLISTFSTR_struct *text1;
 struct FSETdLAYOUT_struct *layouts;
 struct FSETAM1032778315_struct *bnd_rout_creates;
 struct FSETAM1542678947_struct *globals;
 struct FSETSIG_struct *decs;
 struct FSETSIG_struct *iters;
 struct FSETSTR_struct *externs1;
 struct FSETTUPSIGSIG_struct *unboxes;
 struct FSTR_struct *ntext;
 INT length1;
 STR name1;
 BOOL chk_arith;
 BOOL chk_assert;
 BOOL chk_bounds;
 BOOL chk_invariant;
 BOOL chk_post;
 BOOL chk_pre;
 BOOL chk_return;
 BOOL chk_void;
 BOOL chk_when;
 BOOL do_not_merge;
 BOOL is_c_code;
 } *CODE_FILE;

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

typedef struct ELT_TBL_struct {/* layout for ELT_TBL */
 OB_HEADER header;
 INT aref_asize;
 INT aref_get;
 INT aref_set;
 INT hsize;
 INT asize;
 struct ELT_struct *arr_part[1];
 } *ELT_TBL;

typedef struct ELT_TBL_eltbrELT_frame_struct {
 INT state;
 ELT_TBL self;/* Formal argument: self */
 ELT ret_val2;
 ELT_TBL L61;
 ELT r;
 INT L31;
 ELT_TBL is_elt_nil_self;
 ELT is_elt_nil_e;
 BOOL ret_val;
 ELT_NILELT is_elt_nil_self1;
 ELT is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *ELT_TBL_eltbrELT_frame;

typedef struct FLISTdAM_EXPR_struct {/* layout for FLIST{$AM_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dAM_EXPR_struct *arr_part[1];
 } *FLISTdAM_EXPR;

typedef struct FLISTA1893461511_struct {/* layout for FLIST{AM_BND_CREATE_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_BND1124877163_struct *arr_part[1];
 } *FLISTA1893461511;

typedef struct FLISTA1409846210_struct {/* layout for FLIST{AM_BND_ITER_CALL_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_BND367211769_struct *arr_part[1];
 } *FLISTA1409846210;

typedef struct FLISTA1062334999_struct {/* layout for FLIST{AM_ITER_CALL_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_ITE1809135850_struct *arr_part[1];
 } *FLISTA1062334999;

typedef struct FLISTA725283029_struct {/* layout for FLIST{AM_LOCAL_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_LOCAL_EXPR_struct *arr_part[1];
 } *FLISTA725283029;

typedef struct FLISTA2119642552_frame_struct {
 INT state;
 FLISTA725283029 self;/* Formal argument: self */
 AM_LOCAL_EXPR ret_val1;
 INT i;
 INT sz;
 FLISTA725283029 aget_self;
 INT aget_ind;
 AM_LOCAL_EXPR ret_val;
 } *FLISTA2119642552_frame;

typedef struct FLISTAM_ROUT_DEF_struct {/* layout for FLIST{AM_ROUT_DEF} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_ROUT_DEF_struct *arr_part[1];
 } *FLISTAM_ROUT_DEF;

typedef struct FLISTSIG_struct {/* layout for FLIST{SIG} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct SIG_struct *arr_part[1];
 } *FLISTSIG;

typedef struct FLISTSTR_struct {/* layout for FLIST{STR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 STR arr_part[1];
 } *FLISTSTR;

typedef struct FLISTS1171631134_frame_struct {
 INT state;
 FLISTSTR self;/* Formal argument: self */
 STR ret_val1;
 INT i;
 INT sz;
 FLISTSTR aget_self;
 INT aget_ind;
 STR ret_val;
 } *FLISTS1171631134_frame;

typedef struct FMAPdOBNAMESPACE_struct {/* layout for FMAP{$OB,NAMESPACE} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdOBNAMESPACE_struct arr_part[1];
 } *FMAPdOBNAMESPACE;

typedef struct FMAPdTPCODE_FILE_struct {/* layout for FMAP{$TP,CODE_FILE} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdTPCODE_FILE_struct arr_part[1];
 } *FMAPdTPCODE_FILE;

typedef struct FMAPdTPINT_struct {/* layout for FMAP{$TP,INT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdTPINT_struct arr_part[1];
 } *FMAPdTPINT;

typedef struct FMAPAM1244483504_struct {/* layout for FMAP{AM_ROUT_DEF,FLIST{AM_BND_ITER_CALL_EXPR}} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPAM_1024994801_struct arr_part[1];
 } *FMAPAM1244483504;

typedef struct FMAPAM1787628573_struct {/* layout for FMAP{AM_ROUT_DEF,FLIST{AM_ITER_CALL_EXPR}} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPAM_653469574_struct arr_part[1];
 } *FMAPAM1787628573;

typedef struct FMAPIDENTdTP_struct {/* layout for FMAP{IDENT,$TP} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPIDENTdTP_struct arr_part[1];
 } *FMAPIDENTdTP;

typedef struct FMAPID1294197827_frame_struct {
 INT state;
 FMAPIDENTdTP self;/* Formal argument: self */
 IDENT ret_val2;
 FMAPIDENTdTP L61;
 IDENT r;
 INT L31;
 FMAPIDENTdTP is_key_nil_self;
 IDENT is_key_nil_e;
 BOOL ret_val;
 ELT_NILIDENT is_elt_nil_self;
 IDENT is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPID1294197827_frame;

typedef struct FMAPSI518162669_struct {/* layout for FMAP{SIG,AM_ROUT_DEF} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSIG578692189_struct arr_part[1];
 } *FMAPSI518162669;

typedef struct FMAPSIGSTR_struct {/* layout for FMAP{SIG,STR} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSIGSTR_struct arr_part[1];
 } *FMAPSIGSTR;

typedef struct FMAPSTRINT_struct {/* layout for FMAP{STR,INT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRINT_struct arr_part[1];
 } *FMAPSTRINT;

typedef struct FMAPSTRSTR_struct {/* layout for FMAP{STR,STR} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRSTR_struct arr_part[1];
 } *FMAPSTRSTR;

typedef struct FSETdTP_struct {/* layout for FSET{$TP} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct dTP_struct *arr_part[1];
 } *FSETdTP;

typedef struct FSETAM_ROUT_DEF_struct {/* layout for FSET{AM_ROUT_DEF} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct AM_ROUT_DEF_struct *arr_part[1];
 } *FSETAM_ROUT_DEF;

typedef struct FSETSIG_struct {/* layout for FSET{SIG} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct SIG_struct *arr_part[1];
 } *FSETSIG;

typedef struct FSETSTR_struct {/* layout for FSET{STR} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 STR arr_part[1];
 } *FSETSTR;

typedef struct FSTR_struct {/* layout for FSTR */
 OB_HEADER header;
 INT loc;
 INT asize;
 CHAR arr_part[1];
 } *FSTR;

typedef struct NAMEMAP_struct {/* layout for NAMEMAP */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdOBSTR_struct arr_part[1];
 } *NAMEMAP;

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

extern FLISTSIG PRINT_OB_funcs;
extern FLISTSTR SFILE_ID_files;
extern INOUT_MODE MODES_inout_mode;
extern INT CGEN_m1427112264;
extern OUT_MODE MODES_out_mode;
extern TP_CLASS TP_BUI1032249067;
extern TP_CLASS TP_BUI1116976640;
extern TP_CLASS TP_BUI1194559023;
extern TP_CLASS TP_BUI1325635093;
extern TP_CLASS TP_BUI1370948386;
extern TP_CLASS TP_BUI1805763264;
extern TP_CLASS TP_BUI2088674906;
extern TP_CLASS TP_BUI374366290;
extern TP_CLASS TP_BUI389447236;
extern TP_CLASS TP_BUI403444172;
extern TP_CLASS TP_BUI633637838;
extern TP_CLASS TP_BUILTIN_sys;

/* Function declarations */


extern RETURNED_CONST BOOL (**dMODE_814247358)(dMODE, dMODE);

extern RETURNED_CONST BOOL (**dTP_is1334578382)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1811059018)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1999456142)(dTP, dTP);

extern RETURNED_CONST BOOL (**dTP_is242312711)(dTP, OB);

extern RETURNED_CONST BOOL (**dTP_is244083069)(dTP);

extern RETURNED_CONST BOOL (**dTP_is418055720)(dTP);

extern RETURNED_CONST IMPL (**dTP_implrIMPL)(dTP);

extern RETURNED_CONST SIG (**dGET_M422427726)(dGET_MAIN_SIG);

extern RETURNED_CONST STR (**dSTR_strrSTR)(dSTR);

extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);
AM_FORMAL_ARG AM_ROU948739923(AM_ROU948739923_frame);
AM_LOCAL_EXPR FLISTA2119642552(FLISTA2119642552_frame);
AM_OB_DEF CGEN_a1221397173(CGEN, dTP);
ARG ARRAYA827580689(ARRAYA827580689_frame);
ARRAYSTR ARRAYA1729769776(ARRAYA1729769776_frame);
BOOL BOOL_f112211388(BOOL, STR);
BOOL CGEN_i1538413090(CGEN, dTP);
BOOL ELT_is380037919(ELT);
BOOL FSETST1025458804(FSETSTR, STR);
BOOL STR_is111530248(STR, STR);
CODE_FILE CGEN_c2146664558(CGEN, dTP, SFILE_ID);
CODE_FILE CODE_F1432464034(CODE_FILE, STR);
CODE_FILE FMAPdT1940836411(FMAPdTPCODE_FILE, dTP);
CONFIG_DEF CONFIG1152920950(CONFIG_TBL, STR);
ELT ELT_TBL_eltbrELT(ELT_TBL_eltbrELT_frame);
FLISTA1062334999 FLISTA1106467315(FLISTA1062334999, INT);
FLISTA1062334999 FLISTA943034044(FLISTA1062334999, AM_ITE1809135850);
FLISTA1409846210 FLISTA681914133(FLISTA1409846210, INT);
FLISTSIG FLISTS69519306(FLISTSIG, SIG);
FLISTSTR FLISTS1425610062(FLISTSTR, STR);
FLISTdAM_EXPR FLISTd805769956(FLISTdAM_EXPR, dAM_EXPR);
FMAPAM1244483504 FMAPAM1818297900(FMAPAM1244483504, AM_ROUT_DEF, FLISTA1409846210);
FMAPAM1787628573 FMAPAM293039037(FMAPAM1787628573, AM_ROUT_DEF, FLISTA1062334999);
FMAPIDENTdTP FMAPID1786193115(FMAPIDENTdTP, IDENT, dTP);
FMAPdOBNAMESPACE FMAPdO1199466663(FMAPdOBNAMESPACE, OB, NAMESPACE);
FMAPdTPCODE_FILE FMAPdT2092797743(FMAPdTPCODE_FILE, dTP, CODE_FILE);
FSETSTR FSETST1404644833(FSETSTR, STR);
FSETdTP FSETdT1016814448(FSETdTP, dTP);
FSTR FSTR_c1998740379(FSTR, INT);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR FSTR_p399773021(FSTR, CHAR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
IDENT FMAPID1294197827(FMAPID1294197827_frame);
INT AM_ITE1159913222(AM_ITE1159913222_frame);
INT CGEN_c507424193(CGEN, AM_ROUT_DEF, INT*);
INT FLISTS228922656(FLISTSIG);
INT FMAPID99794443(FMAPIDENTdTP);
INT IMPL_a1897060099(IMPL);
INT INT_timesbrINT(INT_timesbrINT_frame);
INT INT_up418511718(INT_up418511718_frame);
INT INT_upbrINT(INT_upbrINT_frame);
INT SFILE_1102877860(SFILE_ID);
INT SFILE_2070755978(SFILE_ID);
INT STR_sizerINT(STR);
MANGLE MANGLE2136606480(MANGLE, PROG);
NAMESPACE FMAPdO790518483(FMAPdOBNAMESPACE, OB);
STR ARRAYS1578977701(ARRAYS1578977701_frame);
STR CGEN_a1155118157(CGEN, dTP);
STR CGEN_d1279589030(CGEN, AM_FORMAL_ARG, SIG, STR);
STR CGEN_d1425664957(CGEN, dTP);
STR CGEN_d1915593494(CGEN, dTP, dTP, STR, BOOL, dMODE);
STR CGEN_d531153180(CGEN, dTP, STR);
STR CGEN_d815167520(CGEN, dTP);
STR CGEN_eolrSTR(CGEN);
STR CGEN_f2084214268(CGEN, dTP);
STR CGEN_g1210307583(CGEN, AM_ROUT_DEF, ARRAYSTR*, ARRAYSTR*, INT*);
STR CGEN_s51495157(CGEN, dTP);
STR CONFIG1397479570(CONFIG_TBL, STR, INT);
STR FILE_M1987416824(FILE_MANGLE, STR, dTP);
STR FLISTS1171631134(FLISTS1171631134_frame);
STR MANGLE119219986(MANGLE, OB, OB);
STR NAMESP1003761134(NAMESPACE, STR);
STR NAMESP180237388(NAMESPACE, STR);
STR NAMESP8552102(NAMESPACE, STR);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap1417971546(STR, STR, STR, STR);
STR STR_ap1693864410(STR, STR, STR);
STR STR_ap2004550586(STR, STR);
STR STR_ap44172742(STR, STR, STR, STR, STR);
STR STR_fr1097270334(STR, FSTR);
STR STR_head_INTrSTR(STR, INT);
STR STR_se405450305(STR_se405450305_frame);
STR STR_tail_INTrSTR(STR, INT);
TP_CLASS TP_TBL1484824239(TP_TBL, STR, INT*);
dLAYOUT LAYOUT79932797(LAYOUT_TBL, dTP);
dTP ARRAYd2147017943(ARRAYd2147017943_frame);
void ARRAYI1050890878(ARRAYIDENT, INT, INT);
void ARRAYI1177186037(ARRAYI1177186037_frame);
void CGEN_a1893106143(CGEN, SFILE_ID);
void CGEN_c1869222825(CGEN, AM_ROUT_DEF);
void CGEN_d695831906(CGEN);
void CGEN_e233957487(CGEN, AM_ROUT_DEF);
void CGEN_init(CGEN);
void CGEN_newline(CGEN);
void CGEN_s1419723103(CGEN, AM_ROUT_DEF);
void CGEN_set_chks(CGEN);
void CODE_F1291349628(CODE_FILE, SIG);
void CODE_F1479352172(CODE_FILE, PROG, STR);
void CODE_F525364269(CODE_FILE, dLAYOUT);
void FILE_plus_STR(FILE1, STR);
void INT_timesb(INT_timesb_frame);
void MANGLE1827205184(MANGLE, OB, STR, OB);
void PROG_warning_STR(PROG, STR);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

AM_OB_DEF CGEN_a1221397173(CGEN self, dTP tp) {
 AM_OB_DEF ret_val;
 IMPL impl;
 AM_OB_DEF r;
 TP_CLASS a;
 ELT_TBL L11;
 ELT e;
 ARRAYIDENT sorted_attrs;
 FMAPIDENTdTP L21;
 ARRAYIDENT L31;
 AM_OB_DEF create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_OB_DEF ret_val1;
 AM_OB_DEF r1;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val2;
 CODE_FILE uses_extern_self;
 STR uses_extern_dec;
 ARRAYIDENT create_self1;
 INT create_n = INT_zero;
 ARRAYIDENT ret_val3;
 INT L41 = INT_zero;
 ARRAYIDENT sort_self;
 dTP L5;
 AM_OB_DEF L6;
 OB L7;
 BOOL L8;
 BOOL L91_;
 ARRAYdTP L10;
 dTP L121_;
 INT L13;
 BOOL L141_;
 BOOL L15;
 BOOL L161_;
 extern STR externINT;
 STR L17;
 extern STR S_asize;
 ELT L18;
 ARRAYIDENT L19;
 INT L20;
 OB L22;
 INT L23;
 INT L241_br;
 IDENT L25;
 IDENT L26;
 INT L271_;
 BOOL L28;
 BOOL L291_;
 INT L301_;
 INT L32;
 INT L331_;
 L5 = tp;
 impl = (*dTP_implrIMPL[TAG(L5)])(L5);
 if ((impl==((IMPL) NULL))) {
  ret_val = ((AM_OB_DEF) NULL);
  return ret_val;
 }
 create_self = ((AM_OB_DEF) NULL);
 create_source = SFILE_ID_zero;
 L7=ZALLOC(sizeof(struct AM_OB_DEF_struct));
 if (L7==NULL) FATAL("Unable to allocate more memory");
 ((OB)L7)->header.tag=AM_OB_DEF_tag;
 L6 = ((AM_OB_DEF) L7);
 r1 = L6;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 SATTR(r,tp,ATTR(impl,tp));
 L8 = (ATTR(impl,arr)==((TP_CLASS) NULL));
 L91_=!(L8); 
 if (L91_) {
  a = ATTR(impl,arr);
  L10 = ATTR(a,params);
  L121_=(dTP)ARR((ARRAYdTP)L10,0); 
  SATTR(r,arr,L121_);
 }
 SATTR(r,asize,IMPL_a1897060099(impl));
 L13 = ATTR(r,asize);
 L141_=(L13)==(-1); 
 L15 = L141_;
 L161_=!(L15); 
 if (L161_) {
  uses_extern_self = ATTR(self,code_c);
  mang_self = self;
  mang_ob = ((OB) tp);
  ret_val2 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
  L17 = ret_val2;
  uses_extern_dec = STR_ap1417971546(((STR) &externINT), L17, ((STR) &S_asize), CGEN_eolrSTR(self));
  SATTR(uses_extern_self,externs1,FSETST1404644833(ATTR(uses_extern_self,externs1), uses_extern_dec));
 }
 {
  struct ELT_TBL_eltbrELT_frame_struct other1_0;
ELT_TBL_eltbrELT_frame noname1 = &other1_0;
  L11 = ATTR(impl,elts);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L18 = ELT_TBL_eltbrELT(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L18;
   if (ELT_is380037919(e)) {
    SATTR(r,at,FMAPID1786193115(ATTR(r,at), ATTR(ATTR(e,sig1),name1), ATTR(ATTR(e,sig1),ret)));
   }
  }
 }
 after_loop: ;
 create_self1 = ((ARRAYIDENT) NULL);
 create_n = FMAPID99794443(ATTR(r,at));
 L20 = create_n;
 L23=(sizeof(struct ARRAYIDENT_struct)-sizeof(IDENT))+(L20)*sizeof(IDENT);
 L22=ZALLOC_BIG(L23);
 if (L22==NULL) FATAL("Unable to allocate more memory");
 ((OB)L22)->header.tag=ARRAYIDENT_tag;
#ifdef DESTROY_CHK

   ((OB)L22)->header.destroyed=0;
#endif

 L19 = ((ARRAYIDENT) L22);
 L19->asize = L20;
 ret_val3 = L19;
 sorted_attrs = ret_val3;
 {
  struct FMAPID1294197827_frame_struct other2_0;
FMAPID1294197827_frame noname2 = &other2_0;
  BOOL f_L241_ = TRUE;
  L21 = ATTR(r,at);
  /* loop index variable */
  L41 = 0;
  L31 = sorted_attrs;
  noname2->self = L21;
  noname2->state = 0;
  L241_br=L31==NULL?0:ASIZE((ARRAYIDENT)L31); 
  while (1) {
   if(L41>=L241_br)  goto after_loop1; 
   L25 = FMAPID1294197827(noname2);
   if (noname2->state == -1) {
    goto after_loop1;
   }
   L26 = L25;
   SARR((ARRAYIDENT)L31,L41,L26); 
   ;
   L271_=INTPLUS(L41,1); 
   L41 = L271_;
  }
 }
 after_loop1: ;
 sort_self = sorted_attrs;
 L28 = (sort_self==((ARRAYIDENT) NULL));
 L291_=!(L28); 
 if (L291_) {
  L301_=ASIZE((ARRAYIDENT)sort_self); 
  L32 = L301_;
  L331_=INTMINUS(L32,1); 
  ARRAYI1050890878(sort_self, 0, L331_);
 }
 SATTR(r,sorted_at,sorted_attrs);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL CGEN_i1538413090(CGEN self, dTP tp) {
 BOOL ret_val = BOOL_zero;
 dTP L11;
 dTP L2;
 dTP L3;
 dTP L4;
 dTP L5;
 dTP L6;
 dTP L7;
 dTP L8;
 L11 = tp;
 L2 = L11;
 if ((*dTP_is242312711[TAG(L2)])(L2, ((OB) TP_BUI1805763264))) {
  ret_val = TRUE;
  return ret_val;
 }
 else {
  L3 = L11;
  if ((*dTP_is242312711[TAG(L3)])(L3, ((OB) TP_BUI403444172))) {
   ret_val = TRUE;
   return ret_val;
  }
  else {
   L4 = L11;
   if ((*dTP_is242312711[TAG(L4)])(L4, ((OB) TP_BUI1194559023))) {
    ret_val = TRUE;
    return ret_val;
   }
   else {
    L5 = L11;
    if ((*dTP_is242312711[TAG(L5)])(L5, ((OB) TP_BUI374366290))) {
     ret_val = TRUE;
     return ret_val;
    }
    else {
     L6 = L11;
     if ((*dTP_is242312711[TAG(L6)])(L6, ((OB) TP_BUI1370948386))) {
      ret_val = TRUE;
      return ret_val;
     }
     else {
      L7 = L11;
      if ((*dTP_is242312711[TAG(L7)])(L7, ((OB) TP_BUI633637838))) {
       ret_val = TRUE;
       return ret_val;
      }
      else {
       L8 = L11;
       if ((*dTP_is242312711[TAG(L8)])(L8, ((OB) TP_BUI1032249067))) {
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


#undef IS_ITER
#define IS_ITER 0

CODE_FILE CGEN_c2146664558(CGEN self, dTP t, SFILE_ID srcloc) {
 CODE_FILE ret_val;
 CODE_FILE cc;
 STR mangled_file_nam = ((STR) NULL);
 STR source_file_name;
 INT nchars = INT_zero;
 FSTR clean_name;
 INT last_dir_point = INT_zero;
 INT i = INT_zero;
 INT L11 = INT_zero;
 CHAR c = CHAR_zero;
 CHAR L21 = CHAR_zero;
 STR clean_name_str;
 STR file_prefix;
 SFILE_ID file_in_self = SFILE_ID_zero;
 STR ret_val1;
 FLISTSTR aget_self;
 INT aget_ind = INT_zero;
 STR ret_val2;
 INT L31 = INT_zero;
 FSTR plus_self;
 CHAR plus_c = CHAR_zero;
 FSTR ret_val3;
 FSTR plus_self1;
 CHAR plus_c1 = CHAR_zero;
 FSTR ret_val4;
 FSTR plus_self2;
 CHAR plus_c2 = CHAR_zero;
 FSTR ret_val5;
 FSTR plus_self3;
 CHAR plus_c3 = CHAR_zero;
 FSTR ret_val6;
 FSTR plus_self4;
 CHAR plus_c4 = CHAR_zero;
 FSTR ret_val7;
 FSTR str_self;
 STR ret_val8;
 STR plus_self5;
 STR plus_sarg;
 STR ret_val9;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val10;
 STR plus_self6;
 STR plus_sarg1;
 STR ret_val11;
 BOOL L4;
 BOOL L51_;
 STR L61_;
 INT aL71_;
 INT L8;
 CHAR L91_;
 INT L101_;
 INT L121_;
 INT L131_;
 INT L141_;
 INT L15;
 INT L161_;
 INT L17;
 INT L181_;
 INT L19;
 INT L201_;
 extern STR c1;
 extern STR c1;
 cc = FMAPdT1940836411(ATTR(self,code_files), t);
 if ((cc==((CODE_FILE) NULL))) {
  L4 = INT_IS_VOID(srcloc.loc);
  L51_=!(L4); 
  if (L51_) {
   file_in_self = srcloc;
   aget_self = SFILE_ID_files;
   aget_ind = SFILE_1102877860(file_in_self);
   L61_=(STR)ARR((FLISTSTR)aget_self,aget_ind); 
   ret_val2 = L61_;
   ret_val1 = ret_val2;
   source_file_name = ret_val1;
   nchars = STR_sizerINT(source_file_name);
   clean_name = FSTR_c1998740379(((FSTR) NULL), nchars);
   last_dir_point = 0;
   {
    BOOL f_L71_ = TRUE;
    /* loop index variable */
    L31 = 0;
    L11 = nchars;
    while (1) {
     if(L31>=L11)  goto after_loop; 
     aL71_=L31; 
     i = aL71_;
     L91_=ARR((STR)source_file_name,i); 
     c = L91_;
     L21 = c;
     switch (L21) {
      case '.': 
       plus_self = clean_name;
       plus_c = '_';
       ret_val3 = FSTR_p399773021(plus_self, plus_c);
       clean_name = ret_val3;
       break;
      case ' ': 
       plus_self1 = clean_name;
       plus_c1 = '_';
       ret_val4 = FSTR_p399773021(plus_self1, plus_c1);
       clean_name = ret_val4;
       break;
      case '/': 
       plus_self2 = clean_name;
       plus_c2 = '_';
       ret_val5 = FSTR_p399773021(plus_self2, plus_c2);
       clean_name = ret_val5;
       L101_=INTPLUS(i,1); 
       last_dir_point = L101_;
       break;
      case '\\': 
       plus_self3 = clean_name;
       plus_c3 = '_';
       ret_val6 = FSTR_p399773021(plus_self3, plus_c3);
       clean_name = ret_val6;
       L121_=INTPLUS(i,1); 
       last_dir_point = L121_;
       break;
      default: ;
       plus_self4 = clean_name;
       plus_c4 = c;
       ret_val7 = FSTR_p399773021(plus_self4, plus_c4);
       clean_name = ret_val7;
     }
     L131_=INTPLUS(L31,1); 
     L31 = L131_;
    }
   }
   after_loop: ;
   str_self = clean_name;
   ret_val8 = STR_fr1097270334(((STR) NULL), str_self);
   clean_name_str = ret_val8;
   L141_=INTMINUS(nchars,last_dir_point); 
   L15 = L141_;
   L161_=(L15>0)?L15:0; 
   file_prefix = STR_tail_INTrSTR(clean_name_str, L161_);
   L17 = STR_sizerINT(file_prefix);
   L181_=INTMINUS(L17,3); 
   L19 = L181_;
   L201_=(L19>2)?L19:2; 
   file_prefix = STR_head_INTrSTR(file_prefix, L201_);
   plus_self5 = FILE_M1987416824(((FILE_MANGLE) NULL), file_prefix, t);
   plus_sarg = ((STR) &c1);
   ret_val9 = STR_ap2004550586(plus_self5, plus_sarg);
   mangled_file_nam = ret_val9;
  }
  else {
   mang_self = self;
   mang_ob = ((OB) t);
   ret_val10 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
   plus_self6 = ret_val10;
   plus_sarg1 = ((STR) &c1);
   ret_val11 = STR_ap2004550586(plus_self6, plus_sarg1);
   mangled_file_nam = ret_val11;
  }
  cc = CODE_F1432464034(((CODE_FILE) NULL), mangled_file_nam);
  SATTR(cc,chk_pre,ATTR(self,chk_pre));
  SATTR(cc,chk_post,ATTR(self,chk_post));
  SATTR(cc,chk_invariant,ATTR(self,chk_invariant));
  SATTR(cc,chk_return,ATTR(self,chk_return));
  SATTR(cc,chk_when,ATTR(self,chk_when));
  SATTR(cc,chk_arith,ATTR(self,chk_arith));
  SATTR(cc,chk_assert,ATTR(self,chk_assert));
  SATTR(cc,chk_bounds,ATTR(self,chk_bounds));
  SATTR(cc,chk_void,ATTR(self,chk_void));
  SATTR(cc,is_c_code,TRUE);
  SATTR(self,code_files,FMAPdT2092797743(ATTR(self,code_files), t, cc));
 }
 ret_val = cc;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT CGEN_c507424193(CGEN self, AM_ROUT_DEF rout_def, INT* num_character_ar) {
 INT L1 = INT_zero;/*Local for arg*/
 INT ret_val = INT_zero;
 INT num_args = INT_zero;
 SIG sig1;
 dTP L21;
 ARRAYARG L31;
 dTP tp;
 dTP L41;
 INT L51 = INT_zero;
 BOOL L6;
 BOOL L71_;
 dTP L8;
 INT L91_;
 dTP L10;
 INT L111_;
 dTP L12;
 INT L131_;
 dTP L14;
 INT L151_;
 INT L161_br;
 ARG aL161_;
 ARG L17;
 ARG L18;
 dTP L19;
 INT L201_;
 INT L221_;
 dTP L23;
 INT L241_;
 INT L251_;
 INT L261_;
 INT L271_;
 num_args = 0;
 L1 = 0;
 sig1 = ATTR(rout_def,sig1);
 L6 = (ATTR(sig1,ret)==((dTP) NULL));
 L71_=!(L6); 
 if (L71_) {
  L21 = ATTR(sig1,ret);
  L8 = L21;
  if ((*dTP_is242312711[TAG(L8)])(L8, ((OB) TP_BUI1805763264))) {
   L91_=INTPLUS(num_args,2); 
   num_args = L91_;
  }
  else {
   L10 = L21;
   if ((*dTP_is242312711[TAG(L10)])(L10, ((OB) TP_BUI2088674906))) {
    L111_=INTPLUS(num_args,2); 
    num_args = L111_;
   }
   else {
    L12 = L21;
    if ((*dTP_is242312711[TAG(L12)])(L12, ((OB) TP_BUI633637838))) {
     L131_=INTPLUS(num_args,1); 
     num_args = L131_;
    }
    else {
     L14 = L21;
     if ((*dTP_is242312711[TAG(L14)])(L14, ((OB) TP_BUI1032249067))) {
      L151_=INTPLUS(num_args,1); 
      num_args = L151_;
     }
    }
   }
  }
 }
 {
  BOOL f_L161_ = TRUE;
  /* loop index variable */
  L51 = 0;
  L31 = ATTR(sig1,args);
  L161_br=L31==NULL?0:ASIZE((ARRAYARG)L31); 
  while (1) {
   if(L51>=L161_br)  goto after_loop; 
   aL161_=ARR((ARRAYARG)L31,L51); 
   L18=aL161_;
   tp = ATTR(L18,tp);
   L41 = tp;
   L19 = L41;
   if ((*dTP_is242312711[TAG(L19)])(L19, ((OB) TP_BUI1805763264))) {
    L201_=INTPLUS(num_args,2); 
    num_args = L201_;
    L221_=INTPLUS(L1,1); 
    L1 = L221_;
   }
   else {
    L23 = L41;
    if ((*dTP_is242312711[TAG(L23)])(L23, ((OB) TP_BUI2088674906))) {
     L241_=INTPLUS(num_args,2); 
     num_args = L241_;
     L251_=INTPLUS(L1,1); 
     L1 = L251_;
    }
    else {
     L261_=INTPLUS(num_args,1); 
     num_args = L261_;
    }
   }
   L271_=INTPLUS(L51,1); 
   L51 = L271_;
  }
 }
 after_loop: ;
 ret_val = num_args;
 *num_character_ar = L1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_a1155118157(CGEN self, dTP t) {
 STR ret_val;
 STR r;
 BOOL leaf1 = BOOL_zero;
 STR sz = ((STR) NULL);
 CGEN sizeof_boxed_sel;
 dTP sizeof_boxed_tp;
 STR ret_val1;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val2;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
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
 CGEN ndefer_self;
 STR ndefer_s;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val8;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val9;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val10;
 CGEN ndefer_self1;
 STR ndefer_s1;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val11;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val12;
 CGEN ndefer_self2;
 STR ndefer_s2;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val13;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val14;
 STR plus_self12;
 STR plus_sarg12;
 STR ret_val15;
 STR plus_self13;
 STR plus_sarg13;
 STR ret_val16;
 CGEN ndefer_self3;
 STR ndefer_s3;
 STR plus_self14;
 STR plus_sarg14;
 STR ret_val17;
 STR plus_self15;
 STR plus_sarg15;
 STR ret_val18;
 CGEN tag_for_self;
 dTP tag_for_tp;
 STR ret_val19;
 STR res;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val20;
 CGEN forbid_self;
 STR forbid_s;
 MANGLE forbid_self1;
 STR forbid_s1;
 STR plus_self16;
 STR plus_sarg16;
 STR ret_val21;
 STR plus_self17;
 STR plus_sarg17;
 STR ret_val22;
 CGEN ndefer_self4;
 STR ndefer_s4;
 STR plus_self18;
 STR plus_sarg18;
 STR ret_val23;
 STR plus_self19;
 STR plus_sarg19;
 STR ret_val24;
 CGEN ndefer_self5;
 STR ndefer_s5;
 CGEN mang_self2;
 OB mang_ob2;
 STR ret_val25;
 STR plus_self20;
 STR plus_sarg20;
 STR ret_val26;
 STR plus_self21;
 STR plus_sarg21;
 STR ret_val27;
 STR plus_self22;
 STR plus_sarg22;
 STR ret_val28;
 STR plus_self23;
 STR plus_sarg23;
 STR ret_val29;
 STR plus_self24;
 STR plus_sarg24;
 STR ret_val30;
 STR plus_self25;
 STR plus_sarg25;
 STR ret_val31;
 CGEN mang_self3;
 OB mang_ob3;
 STR ret_val32;
 STR plus_self26;
 STR plus_sarg26;
 STR ret_val33;
 STR plus_self27;
 STR plus_sarg27;
 STR ret_val34;
 STR plus_self28;
 STR plus_sarg28;
 STR ret_val35;
 STR plus_self29;
 STR plus_sarg29;
 STR ret_val36;
 dTP L1;
 BOOL L2;
 dTP L3;
 dTP L4;
 extern STR sizeofstruct;
 extern STR S_boxed_struct;
 extern STR ZALLOC_LEAF1;
 extern STR name76;
 extern STR ZALLOC1;
 extern STR name76;
 BOOL L5;
 BOOL L61_;
 extern STR if3;
 extern STR NULLFA1853100247;
 extern STR memset1;
 extern STR S0;
 extern STR name76;
 extern STR OB1;
 extern STR headertag;
 extern STR S_tag;
 extern STR name16;
 extern STR OB1;
 extern STR headeridob_count;
 dTP L7;
 extern STR name75;
 extern STR S_boxed;
 extern STR name26;
 dTP L8;
 extern STR EXT_OB_boxed1;
 extern STR name26;
 extern STR name75;
 extern STR name23;
 extern STR name26;
 r = CGEN_d1425664957(self, ((dTP) TP_BUI1325635093));
 L1 = t;
 leaf1 = (*dTP_is244083069[TAG(L1)])(L1);
 L3 = t;
 if ((*dTP_is1334578382[TAG(L3)])(L3)) {
  L2 = TRUE;
 } else {
  L4 = t;
  L2 = (*dTP_is242312711[TAG(L4)])(L4, ((OB) TP_BUI389447236));
 }
 if (L2) {
  sizeof_boxed_sel = self;
  sizeof_boxed_tp = t;
  plus_self = ((STR) &sizeofstruct);
  mang_self = sizeof_boxed_sel;
  mang_ob = ((OB) sizeof_boxed_tp);
  ret_val2 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
  plus_sarg = ret_val2;
  ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val3;
  plus_sarg1 = ((STR) &S_boxed_struct);
  ret_val4 = STR_ap2004550586(plus_self1, plus_sarg1);
  ret_val1 = ret_val4;
  sz = ret_val1;
 }
 else {
  sz = CGEN_s51495157(self, t);
 }
 if (leaf1) {
  ndefer_self = self;
  plus_self2 = r;
  plus_sarg2 = ((STR) &ZALLOC_LEAF1);
  ret_val5 = STR_ap2004550586(plus_self2, plus_sarg2);
  plus_self3 = ret_val5;
  plus_sarg3 = sz;
  ret_val6 = STR_ap2004550586(plus_self3, plus_sarg3);
  plus_self4 = ret_val6;
  plus_sarg4 = ((STR) &name76);
  ret_val7 = STR_ap2004550586(plus_self4, plus_sarg4);
  ndefer_s = ret_val7;
  CGEN_d695831906(ndefer_self);
  SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
 }
 else {
  ndefer_self1 = self;
  plus_self5 = r;
  plus_sarg5 = ((STR) &ZALLOC1);
  ret_val8 = STR_ap2004550586(plus_self5, plus_sarg5);
  plus_self6 = ret_val8;
  plus_sarg6 = sz;
  ret_val9 = STR_ap2004550586(plus_self6, plus_sarg6);
  plus_self7 = ret_val9;
  plus_sarg7 = ((STR) &name76);
  ret_val10 = STR_ap2004550586(plus_self7, plus_sarg7);
  ndefer_s1 = ret_val10;
  CGEN_d695831906(ndefer_self1);
  SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
 }
 L5 = ATTR(self,siva);
 L61_=!(L5); 
 if (L61_) {
  ndefer_self2 = self;
  plus_self8 = ((STR) &if3);
  plus_sarg8 = r;
  ret_val11 = STR_ap2004550586(plus_self8, plus_sarg8);
  plus_self9 = ret_val11;
  plus_sarg9 = ((STR) &NULLFA1853100247);
  ret_val12 = STR_ap2004550586(plus_self9, plus_sarg9);
  ndefer_s2 = ret_val12;
  CGEN_d695831906(ndefer_self2);
  SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
  if (leaf1) {
   ndefer_self3 = self;
   plus_self10 = ((STR) &memset1);
   plus_sarg10 = r;
   ret_val13 = STR_ap2004550586(plus_self10, plus_sarg10);
   plus_self11 = ret_val13;
   plus_sarg11 = ((STR) &S0);
   ret_val14 = STR_ap2004550586(plus_self11, plus_sarg11);
   plus_self12 = ret_val14;
   plus_sarg12 = sz;
   ret_val15 = STR_ap2004550586(plus_self12, plus_sarg12);
   plus_self13 = ret_val15;
   plus_sarg13 = ((STR) &name76);
   ret_val16 = STR_ap2004550586(plus_self13, plus_sarg13);
   ndefer_s3 = ret_val16;
   CGEN_d695831906(ndefer_self3);
   SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
  }
 }
 ndefer_self4 = self;
 plus_self14 = ((STR) &OB1);
 plus_sarg14 = r;
 ret_val17 = STR_ap2004550586(plus_self14, plus_sarg14);
 plus_self15 = ret_val17;
 plus_sarg15 = ((STR) &headertag);
 ret_val18 = STR_ap2004550586(plus_self15, plus_sarg15);
 plus_self16 = ret_val18;
 tag_for_self = self;
 tag_for_tp = t;
 SATTR(tag_for_self,needs_tag,FSETdT1016814448(ATTR(tag_for_self,needs_tag), tag_for_tp));
 mang_self1 = tag_for_self;
 mang_ob1 = ((OB) tag_for_tp);
 ret_val20 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
 res = STR_ap2004550586(ret_val20, ((STR) &S_tag));
 forbid_self = tag_for_self;
 forbid_s = res;
 forbid_self1 = ATTR(forbid_self,mangler);
 forbid_s1 = forbid_s;
 SATTR(forbid_self1,forbidden,FSETST1404644833(ATTR(forbid_self1,forbidden), forbid_s1));
 ret_val19 = res;
 plus_sarg16 = ret_val19;
 ret_val21 = STR_ap2004550586(plus_self16, plus_sarg16);
 plus_self17 = ret_val21;
 plus_sarg17 = ((STR) &name16);
 ret_val22 = STR_ap2004550586(plus_self17, plus_sarg17);
 ndefer_s4 = ret_val22;
 CGEN_d695831906(ndefer_self4);
 SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
 if (ATTR(self,deterministic1)) {
  ndefer_self5 = self;
  plus_self18 = ((STR) &OB1);
  plus_sarg18 = r;
  ret_val23 = STR_ap2004550586(plus_self18, plus_sarg18);
  plus_self19 = ret_val23;
  plus_sarg19 = ((STR) &headeridob_count);
  ret_val24 = STR_ap2004550586(plus_self19, plus_sarg19);
  ndefer_s5 = ret_val24;
  CGEN_d695831906(ndefer_self5);
  SATTR(ndefer_self5,routine_code,FSTR_p1752847026(ATTR(ndefer_self5,routine_code), ndefer_s5));
 }
 L7 = t;
 if ((*dTP_is1334578382[TAG(L7)])(L7)) {
  plus_self20 = ((STR) &name75);
  mang_self2 = self;
  mang_ob2 = ((OB) t);
  ret_val25 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, ((OB) NULL));
  plus_sarg20 = ret_val25;
  ret_val26 = STR_ap2004550586(plus_self20, plus_sarg20);
  plus_self21 = ret_val26;
  plus_sarg21 = ((STR) &S_boxed);
  ret_val27 = STR_ap2004550586(plus_self21, plus_sarg21);
  plus_self22 = ret_val27;
  plus_sarg22 = r;
  ret_val28 = STR_ap2004550586(plus_self22, plus_sarg22);
  plus_self23 = ret_val28;
  plus_sarg23 = ((STR) &name26);
  ret_val29 = STR_ap2004550586(plus_self23, plus_sarg23);
  ret_val = ret_val29;
  return ret_val;
 }
 else {
  L8 = t;
  if ((*dTP_is242312711[TAG(L8)])(L8, ((OB) TP_BUI389447236))) {
   plus_self24 = ((STR) &EXT_OB_boxed1);
   plus_sarg24 = r;
   ret_val30 = STR_ap2004550586(plus_self24, plus_sarg24);
   plus_self25 = ret_val30;
   plus_sarg25 = ((STR) &name26);
   ret_val31 = STR_ap2004550586(plus_self25, plus_sarg25);
   ret_val = ret_val31;
   return ret_val;
  }
  else {
   plus_self26 = ((STR) &name75);
   mang_self3 = self;
   mang_ob3 = ((OB) t);
   ret_val32 = MANGLE119219986(ATTR(mang_self3,mangler), mang_ob3, ((OB) NULL));
   plus_sarg26 = ret_val32;
   ret_val33 = STR_ap2004550586(plus_self26, plus_sarg26);
   plus_self27 = ret_val33;
   plus_sarg27 = ((STR) &name23);
   ret_val34 = STR_ap2004550586(plus_self27, plus_sarg27);
   plus_self28 = ret_val34;
   plus_sarg28 = r;
   ret_val35 = STR_ap2004550586(plus_self28, plus_sarg28);
   plus_self29 = ret_val35;
   plus_sarg29 = ((STR) &name26);
   ret_val36 = STR_ap2004550586(plus_self29, plus_sarg29);
   ret_val = ret_val36;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_d1279589030(CGEN self, AM_FORMAL_ARG a, SIG sig1, STR s) {
 STR ret_val;
 STR res;
 MANGLE space_self;
 OB space_ns;
 NAMESPACE ret_val1;
 NAMESPACE r;
 NAMESPACE create_self;
 NAMESPACE ret_val2;
 NAMESPACE r1;
 NAMEMAP create_self1;
 NAMEMAP ret_val3;
 FSETSTR create_self2;
 FSETSTR ret_val4;
 FMAPSTRINT create_self3;
 FMAPSTRINT ret_val5;
 CODE_FILE plus_self;
 dSTR plus_s;
 CODE_FILE ret_val6;
 CODE_FILE plus_self1;
 dSTR plus_s1;
 CODE_FILE ret_val7;
 AM_FORMAL_ARG tp_self;
 dTP ret_val8;
 AM_LOCAL_EXPR tp_self1;
 dTP ret_val9;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val10;
 CODE_FILE plus_self2;
 dSTR plus_s2;
 CODE_FILE ret_val11;
 CODE_FILE plus_self3;
 dSTR plus_s3;
 CODE_FILE ret_val12;
 CODE_FILE plus_self4;
 dSTR plus_s4;
 CODE_FILE ret_val13;
 CODE_FILE plus_self5;
 dSTR plus_s5;
 CODE_FILE ret_val14;
 CODE_FILE plus_self6;
 dSTR plus_s6;
 CODE_FILE ret_val15;
 CODE_FILE plus_self7;
 dSTR plus_s7;
 CODE_FILE ret_val16;
 CODE_FILE plus_self8;
 dSTR plus_s8;
 NAMESPACE L1;
 OB L2;
 extern STR L;
 BOOL L3;
 BOOL L41_;
 FSTR L5;
 dSTR L6;
 dSTR L7;
 OB L8;
 BOOL L9;
 BOOL L101_;
 FSTR L11;
 dSTR L12;
 BOOL L13;
 BOOL L141_;
 FSTR L15;
 dSTR L16;
 dSTR L17;
 OB L18;
 BOOL L19;
 BOOL L201_;
 FSTR L21;
 dSTR L22;
 BOOL L23;
 BOOL L241_;
 FSTR L25;
 dSTR L26;
 extern STR name36;
 BOOL L27;
 BOOL L281_;
 FSTR L29;
 dSTR L30;
 BOOL L31;
 BOOL L321_;
 FSTR L33;
 dSTR L34;
 dSTR L35;
 OB L36;
 BOOL L37;
 BOOL L381_;
 FSTR L39;
 dSTR L40;
 extern STR Localforarg;
 FSTR L42;
 dSTR L43;
 space_self = ATTR(self,mangler);
 space_ns = ((OB) sig1);
 if ((space_ns==((OB) NULL))) {
  space_ns = ((OB) space_self);
 }
 r = FMAPdO790518483(ATTR(space_self,namespaces), space_ns);
 if ((r==((NAMESPACE) NULL))) {
  create_self = ((NAMESPACE) NULL);
  L2=ZALLOC(sizeof(struct NAMESPACE_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=NAMESPACE_tag;
  L1 = ((NAMESPACE) L2);
  r1 = L1;
  create_self1 = ((NAMEMAP) NULL);
  ret_val3 = ((NAMEMAP) NULL);
  SATTR(r1,map,ret_val3);
  create_self2 = ((FSETSTR) NULL);
  ret_val4 = ((FSETSTR) NULL);
  SATTR(r1,set,ret_val4);
  SATTR(r1,counter,0);
  create_self3 = ((FMAPSTRINT) NULL);
  ret_val5 = ((FMAPSTRINT) NULL);
  SATTR(r1,uniques,ret_val5);
  ret_val2 = r1;
  r = ret_val2;
  SATTR(space_self,namespaces,FMAPdO1199466663(ATTR(space_self,namespaces), space_ns, r));
 }
 ret_val1 = r;
 res = NAMESP1003761134(ret_val1, ((STR) &L));
 MANGLE1827205184(ATTR(self,mangler), ((OB) ATTR(a,expr)), res, ((OB) sig1));
 plus_self = ATTR(self,code_c);
 plus_s = ((dSTR) CGEN_eolrSTR(self));
 L3 = (plus_s==((dSTR) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(plus_self,ntext);
  L6 = plus_s;
  SATTR(plus_self,ntext,FSTR_p1752847026(L5, (*dSTR_strrSTR[TAG(L6)])(L6)));
 }
 ret_val6 = plus_self;
 plus_self1 = ret_val6;
 L8=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 memset(L8,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L8)->header.tag=CHAR_tag;
 L7 = (dSTR)((CHAR_boxed) L8);
 ((CHAR_boxed) L7)->immutable_part = ' ';
 plus_s1 = L7;
 L9 = (plus_s1==((dSTR) NULL));
 L101_=!(L9); 
 if (L101_) {
  L11 = ATTR(plus_self1,ntext);
  L12 = plus_s1;
  SATTR(plus_self1,ntext,FSTR_p1752847026(L11, (*dSTR_strrSTR[TAG(L12)])(L12)));
 }
 ret_val7 = plus_self1;
 plus_self2 = ret_val7;
 mang_self = self;
 tp_self = a;
 tp_self1 = ATTR(tp_self,expr);
 ret_val9 = ATTR(tp_self1,tp_at);
 ret_val8 = ret_val9;
 mang_ob = ((OB) ret_val8);
 ret_val10 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
 plus_s2 = ((dSTR) ret_val10);
 L13 = (plus_s2==((dSTR) NULL));
 L141_=!(L13); 
 if (L141_) {
  L15 = ATTR(plus_self2,ntext);
  L16 = plus_s2;
  SATTR(plus_self2,ntext,FSTR_p1752847026(L15, (*dSTR_strrSTR[TAG(L16)])(L16)));
 }
 ret_val11 = plus_self2;
 plus_self3 = ret_val11;
 L18=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L18==NULL) FATAL("Unable to allocate more memory");
 memset(L18,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L18)->header.tag=CHAR_tag;
 L17 = (dSTR)((CHAR_boxed) L18);
 ((CHAR_boxed) L17)->immutable_part = ' ';
 plus_s3 = L17;
 L19 = (plus_s3==((dSTR) NULL));
 L201_=!(L19); 
 if (L201_) {
  L21 = ATTR(plus_self3,ntext);
  L22 = plus_s3;
  SATTR(plus_self3,ntext,FSTR_p1752847026(L21, (*dSTR_strrSTR[TAG(L22)])(L22)));
 }
 ret_val12 = plus_self3;
 plus_self4 = ret_val12;
 plus_s4 = ((dSTR) res);
 L23 = (plus_s4==((dSTR) NULL));
 L241_=!(L23); 
 if (L241_) {
  L25 = ATTR(plus_self4,ntext);
  L26 = plus_s4;
  SATTR(plus_self4,ntext,FSTR_p1752847026(L25, (*dSTR_strrSTR[TAG(L26)])(L26)));
 }
 ret_val13 = plus_self4;
 plus_self5 = ret_val13;
 plus_s5 = ((dSTR) ((STR) &name36));
 L27 = (plus_s5==((dSTR) NULL));
 L281_=!(L27); 
 if (L281_) {
  L29 = ATTR(plus_self5,ntext);
  L30 = plus_s5;
  SATTR(plus_self5,ntext,FSTR_p1752847026(L29, (*dSTR_strrSTR[TAG(L30)])(L30)));
 }
 ret_val14 = plus_self5;
 plus_self6 = ret_val14;
 plus_s6 = ((dSTR) s);
 L31 = (plus_s6==((dSTR) NULL));
 L321_=!(L31); 
 if (L321_) {
  L33 = ATTR(plus_self6,ntext);
  L34 = plus_s6;
  SATTR(plus_self6,ntext,FSTR_p1752847026(L33, (*dSTR_strrSTR[TAG(L34)])(L34)));
 }
 ret_val15 = plus_self6;
 plus_self7 = ret_val15;
 L36=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L36==NULL) FATAL("Unable to allocate more memory");
 memset(L36,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L36)->header.tag=CHAR_tag;
 L35 = (dSTR)((CHAR_boxed) L36);
 ((CHAR_boxed) L35)->immutable_part = ';';
 plus_s7 = L35;
 L37 = (plus_s7==((dSTR) NULL));
 L381_=!(L37); 
 if (L381_) {
  L39 = ATTR(plus_self7,ntext);
  L40 = plus_s7;
  SATTR(plus_self7,ntext,FSTR_p1752847026(L39, (*dSTR_strrSTR[TAG(L40)])(L40)));
 }
 ret_val16 = plus_self7;
 plus_self8 = ret_val16;
 plus_s8 = ((dSTR) ((STR) &Localforarg));
 L42 = ATTR(plus_self8,ntext);
 L43 = plus_s8;
 SATTR(plus_self8,ntext,FSTR_p1752847026(L42, (*dSTR_strrSTR[TAG(L43)])(L43)));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_d1425664957(CGEN self, dTP t) {
 STR ret_val;
 STR res;
 MANGLE genlocal_self;
 OB genlocal_ns;
 STR ret_val1;
 STR name111;
 MANGLE space_self;
 OB space_ns;
 NAMESPACE ret_val2;
 NAMESPACE r;
 NAMESPACE create_self;
 NAMESPACE ret_val3;
 NAMESPACE r1;
 NAMEMAP create_self1;
 NAMEMAP ret_val4;
 FSETSTR create_self2;
 FSETSTR ret_val5;
 FMAPSTRINT create_self3;
 FMAPSTRINT ret_val6;
 CODE_FILE plus_self;
 dSTR plus_s;
 CODE_FILE ret_val7;
 CODE_FILE plus_self1;
 dSTR plus_s1;
 CODE_FILE ret_val8;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val9;
 CODE_FILE plus_self2;
 dSTR plus_s2;
 CODE_FILE ret_val10;
 CODE_FILE plus_self3;
 dSTR plus_s3;
 CODE_FILE ret_val11;
 CODE_FILE plus_self4;
 dSTR plus_s4;
 CODE_FILE ret_val12;
 CODE_FILE plus_self5;
 dSTR plus_s5;
 NAMESPACE L1;
 OB L2;
 extern STR L;
 BOOL L3;
 BOOL L41_;
 FSTR L5;
 dSTR L6;
 dSTR L7;
 OB L8;
 BOOL L9;
 BOOL L101_;
 FSTR L11;
 dSTR L12;
 BOOL L13;
 BOOL L141_;
 FSTR L15;
 dSTR L16;
 dSTR L17;
 OB L18;
 BOOL L19;
 BOOL L201_;
 FSTR L21;
 dSTR L22;
 BOOL L23;
 BOOL L241_;
 FSTR L25;
 dSTR L26;
 dSTR L27;
 OB L28;
 FSTR L29;
 dSTR L30;
 genlocal_self = ATTR(self,mangler);
 genlocal_ns = ((OB) ATTR(self,current_sig));
 space_self = genlocal_self;
 space_ns = genlocal_ns;
 if ((space_ns==((OB) NULL))) {
  space_ns = ((OB) space_self);
 }
 r = FMAPdO790518483(ATTR(space_self,namespaces), space_ns);
 if ((r==((NAMESPACE) NULL))) {
  create_self = ((NAMESPACE) NULL);
  L2=ZALLOC(sizeof(struct NAMESPACE_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=NAMESPACE_tag;
  L1 = ((NAMESPACE) L2);
  r1 = L1;
  create_self1 = ((NAMEMAP) NULL);
  ret_val4 = ((NAMEMAP) NULL);
  SATTR(r1,map,ret_val4);
  create_self2 = ((FSETSTR) NULL);
  ret_val5 = ((FSETSTR) NULL);
  SATTR(r1,set,ret_val5);
  SATTR(r1,counter,0);
  create_self3 = ((FMAPSTRINT) NULL);
  ret_val6 = ((FMAPSTRINT) NULL);
  SATTR(r1,uniques,ret_val6);
  ret_val3 = r1;
  r = ret_val3;
  SATTR(space_self,namespaces,FMAPdO1199466663(ATTR(space_self,namespaces), space_ns, r));
 }
 ret_val2 = r;
 name111 = NAMESP180237388(ret_val2, ((STR) &L));
 SATTR(genlocal_self,used_by_local,FSETST1404644833(ATTR(genlocal_self,used_by_local), name111));
 ret_val1 = name111;
 res = ret_val1;
 plus_self = ATTR(self,code_c);
 plus_s = ((dSTR) CGEN_eolrSTR(self));
 L3 = (plus_s==((dSTR) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(plus_self,ntext);
  L6 = plus_s;
  SATTR(plus_self,ntext,FSTR_p1752847026(L5, (*dSTR_strrSTR[TAG(L6)])(L6)));
 }
 ret_val7 = plus_self;
 plus_self1 = ret_val7;
 L8=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 memset(L8,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L8)->header.tag=CHAR_tag;
 L7 = (dSTR)((CHAR_boxed) L8);
 ((CHAR_boxed) L7)->immutable_part = ' ';
 plus_s1 = L7;
 L9 = (plus_s1==((dSTR) NULL));
 L101_=!(L9); 
 if (L101_) {
  L11 = ATTR(plus_self1,ntext);
  L12 = plus_s1;
  SATTR(plus_self1,ntext,FSTR_p1752847026(L11, (*dSTR_strrSTR[TAG(L12)])(L12)));
 }
 ret_val8 = plus_self1;
 plus_self2 = ret_val8;
 mang_self = self;
 mang_ob = ((OB) t);
 ret_val9 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
 plus_s2 = ((dSTR) ret_val9);
 L13 = (plus_s2==((dSTR) NULL));
 L141_=!(L13); 
 if (L141_) {
  L15 = ATTR(plus_self2,ntext);
  L16 = plus_s2;
  SATTR(plus_self2,ntext,FSTR_p1752847026(L15, (*dSTR_strrSTR[TAG(L16)])(L16)));
 }
 ret_val10 = plus_self2;
 plus_self3 = ret_val10;
 L18=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L18==NULL) FATAL("Unable to allocate more memory");
 memset(L18,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L18)->header.tag=CHAR_tag;
 L17 = (dSTR)((CHAR_boxed) L18);
 ((CHAR_boxed) L17)->immutable_part = ' ';
 plus_s3 = L17;
 L19 = (plus_s3==((dSTR) NULL));
 L201_=!(L19); 
 if (L201_) {
  L21 = ATTR(plus_self3,ntext);
  L22 = plus_s3;
  SATTR(plus_self3,ntext,FSTR_p1752847026(L21, (*dSTR_strrSTR[TAG(L22)])(L22)));
 }
 ret_val11 = plus_self3;
 plus_self4 = ret_val11;
 plus_s4 = ((dSTR) res);
 L23 = (plus_s4==((dSTR) NULL));
 L241_=!(L23); 
 if (L241_) {
  L25 = ATTR(plus_self4,ntext);
  L26 = plus_s4;
  SATTR(plus_self4,ntext,FSTR_p1752847026(L25, (*dSTR_strrSTR[TAG(L26)])(L26)));
 }
 ret_val12 = plus_self4;
 plus_self5 = ret_val12;
 L28=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L28==NULL) FATAL("Unable to allocate more memory");
 memset(L28,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L28)->header.tag=CHAR_tag;
 L27 = (dSTR)((CHAR_boxed) L28);
 ((CHAR_boxed) L27)->immutable_part = ';';
 plus_s5 = L27;
 L29 = ATTR(plus_self5,ntext);
 L30 = plus_s5;
 SATTR(plus_self5,ntext,FSTR_p1752847026(L29, (*dSTR_strrSTR[TAG(L30)])(L30)));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_d1915593494(CGEN self, dTP dest_tp, dTP src_tp, STR expr11, BOOL only_boxing, dMODE mode1) {
 STR ret_val;
 BOOL use_ptr = BOOL_zero;
 STR res = ((STR) NULL);
 STR immutable_part;
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
 STR plus_s3;
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
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val14;
 FILE1 r6;
 OUT create_self7;
 OUT ret_val15;
 OUT plus_self7;
 STR plus_s7;
 OUT ret_val16;
 FILE1 stdout_self7;
 FILE1 ret_val17;
 FILE1 r7;
 OUT plus_self8;
 STR plus_s8;
 OUT ret_val18;
 FILE1 stdout_self8;
 FILE1 ret_val19;
 FILE1 r8;
 OUT plus_self9;
 STR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val20;
 FILE1 r9;
 UNIX exit_self;
 INT exit_code = INT_zero;
 CODE_FILE uses_tp_self;
 dTP uses_tp_t;
 STR plus_self10;
 STR plus_sarg;
 STR ret_val21;
 CGEN dec_local_commen;
 dTP dec_local_commen1;
 STR dec_local_commen2;
 STR ret_val22;
 STR res1;
 CGEN comment_self;
 CODE_FILE comment_f;
 STR comment_com;
 CODE_FILE plus_self11;
 dSTR plus_s10;
 CODE_FILE ret_val23;
 CODE_FILE plus_self12;
 dSTR plus_s11;
 CODE_FILE ret_val24;
 CODE_FILE plus_self13;
 dSTR plus_s12;
 STR plus_self14;
 STR plus_sarg1;
 STR ret_val25;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val26;
 STR plus_self15;
 STR plus_sarg2;
 STR ret_val27;
 STR plus_self16;
 STR plus_sarg3;
 STR ret_val28;
 STR plus_self17;
 STR plus_sarg4;
 STR ret_val29;
 STR plus_self18;
 STR plus_sarg5;
 STR ret_val30;
 CGEN ndefer_self;
 STR ndefer_s;
 STR plus_self19;
 STR plus_sarg6;
 STR ret_val31;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val32;
 STR plus_self20;
 STR plus_sarg7;
 STR ret_val33;
 STR plus_self21;
 STR plus_sarg8;
 STR ret_val34;
 STR plus_self22;
 STR plus_sarg9;
 STR ret_val35;
 STR plus_self23;
 STR plus_sarg10;
 STR ret_val36;
 STR plus_self24;
 STR plus_sarg11;
 STR ret_val37;
 STR plus_self25;
 STR plus_sarg12;
 STR ret_val38;
 CGEN ndefer_self1;
 STR ndefer_s1;
 STR plus_self26;
 STR plus_sarg13;
 STR ret_val39;
 STR plus_self27;
 STR plus_sarg14;
 STR ret_val40;
 STR plus_self28;
 STR plus_sarg15;
 STR ret_val41;
 CGEN dec_local_commen3;
 dTP dec_local_commen4;
 STR dec_local_commen5;
 STR ret_val42;
 STR res2;
 CGEN comment_self1;
 CODE_FILE comment_f1;
 STR comment_com1;
 CODE_FILE plus_self29;
 dSTR plus_s13;
 CODE_FILE ret_val43;
 CODE_FILE plus_self30;
 dSTR plus_s14;
 CODE_FILE ret_val44;
 CODE_FILE plus_self31;
 dSTR plus_s15;
 STR plus_self32;
 STR plus_sarg16;
 STR ret_val45;
 STR plus_self33;
 STR plus_sarg17;
 STR ret_val46;
 CGEN mang_self2;
 OB mang_ob2;
 STR ret_val47;
 STR plus_self34;
 STR plus_sarg18;
 STR ret_val48;
 STR plus_self35;
 STR plus_sarg19;
 STR ret_val49;
 STR plus_self36;
 STR plus_sarg20;
 STR ret_val50;
 STR plus_self37;
 STR plus_sarg21;
 STR ret_val51;
 CGEN ndefer_self2;
 STR ndefer_s2;
 CGEN mang_self3;
 OB mang_ob3;
 STR ret_val52;
 STR plus_self38;
 STR plus_sarg22;
 STR ret_val53;
 STR plus_self39;
 STR plus_sarg23;
 STR ret_val54;
 STR plus_self40;
 STR plus_sarg24;
 STR ret_val55;
 STR plus_self41;
 STR plus_sarg25;
 STR ret_val56;
 CGEN mang_self4;
 OB mang_ob4;
 STR ret_val57;
 STR plus_self42;
 STR plus_sarg26;
 STR ret_val58;
 STR plus_self43;
 STR plus_sarg27;
 STR ret_val59;
 STR plus_self44;
 STR plus_sarg28;
 STR ret_val60;
 STR plus_self45;
 STR plus_sarg29;
 STR ret_val61;
 CGEN ndefer_self3;
 STR ndefer_s3;
 PROG psather_self;
 BOOL ret_val62 = BOOL_zero;
 CGEN mang_self5;
 OB mang_ob5;
 STR ret_val63;
 STR plus_self46;
 STR plus_sarg30;
 STR ret_val64;
 STR plus_self47;
 STR plus_sarg31;
 STR ret_val65;
 STR plus_self48;
 STR plus_sarg32;
 STR ret_val66;
 STR plus_self49;
 STR plus_sarg33;
 STR ret_val67;
 CGEN ndefer_self4;
 STR ndefer_s4;
 STR plus_self50;
 STR plus_sarg34;
 STR ret_val68;
 STR plus_self51;
 STR plus_sarg35;
 STR ret_val69;
 CGEN mang_self6;
 OB mang_ob6;
 STR ret_val70;
 STR plus_self52;
 STR plus_sarg36;
 STR ret_val71;
 STR plus_self53;
 STR plus_sarg37;
 STR ret_val72;
 CGEN mang_self7;
 OB mang_ob7;
 STR ret_val73;
 STR plus_self54;
 STR plus_sarg38;
 STR ret_val74;
 STR plus_self55;
 STR plus_sarg39;
 STR ret_val75;
 STR plus_self56;
 STR plus_sarg40;
 STR ret_val76;
 STR plus_self57;
 STR plus_sarg41;
 STR ret_val77;
 CGEN ndefer_self5;
 STR ndefer_s5;
 CGEN mang_self8;
 OB mang_ob8;
 STR ret_val78;
 STR plus_self58;
 STR plus_sarg42;
 STR ret_val79;
 STR plus_self59;
 STR plus_sarg43;
 STR ret_val80;
 STR plus_self60;
 STR plus_sarg44;
 STR ret_val81;
 STR plus_self61;
 STR plus_sarg45;
 STR ret_val82;
 STR plus_self62;
 STR plus_sarg46;
 STR ret_val83;
 BOOL L1;
 BOOL L2;
 BOOL L3;
 dTP L4;
 BOOL L5;
 dTP L6;
 dTP L7;
 dTP L8;
 dTP L9;
 BOOL L10;
 BOOL L111_;
 extern STR CASTINGERROR;
 FILE1 L12;
 OB L13;
 extern STR dest_tp1;
 FILE1 L15;
 OB L16;
 extern STR void4;
 FILE1 L18;
 OB L19;
 dTP L21;
 FILE1 L22;
 OB L23;
 extern STR src_tp1;
 FILE1 L25;
 OB L26;
 extern STR void4;
 FILE1 L28;
 OB L29;
 dTP L31;
 FILE1 L32;
 OB L33;
 extern STR expr3;
 FILE1 L35;
 OB L36;
 FILE1 L38;
 OB L39;
 extern STR name9;
 FILE1 L41;
 OB L42;
 BOOL L44;
 BOOL L451_;
 BOOL L46;
 dMODE L47;
 dMODE L48;
 dTP L49;
 BOOL L50;
 dTP L51;
 BOOL L52;
 dTP L53;
 dTP L54;
 extern STR localforboxed;
 dTP L55;
 extern STR name78;
 BOOL L56;
 BOOL L571_;
 FSTR L58;
 dSTR L59;
 BOOL L60;
 BOOL L611_;
 FSTR L62;
 dSTR L63;
 extern STR name79;
 FSTR L64;
 dSTR L65;
 extern STR name80;
 extern STR name26;
 extern STR name16;
 extern STR name58;
 extern STR name75;
 extern STR S_boxed;
 extern STR immutable_part1;
 extern STR name16;
 BOOL L66;
 BOOL L67;
 dTP L68;
 dTP L69;
 dTP L70;
 extern STR name58;
 extern STR localp1561414680;
 dTP L71;
 extern STR localforunboxed;
 dTP L72;
 extern STR name78;
 BOOL L73;
 BOOL L741_;
 FSTR L75;
 dSTR L76;
 BOOL L77;
 BOOL L781_;
 FSTR L79;
 dSTR L80;
 extern STR name79;
 FSTR L81;
 dSTR L82;
 dTP L83;
 extern STR F_VA_RATTR_NN1;
 extern STR name3;
 extern STR S_boxed1;
 extern STR immutable_part2;
 extern STR F_V_RATTR_LL1;
 extern STR name3;
 extern STR name3;
 extern STR S_boxed1;
 extern STR immutable_part2;
 BOOL L84;
 dTP L85;
 BOOL L86;
 BOOL L87;
 BOOL L881_;
 extern STR name75;
 extern STR S_boxed;
 extern STR immutable_part3;
 extern STR name81;
 extern STR name26;
 extern STR name36;
 extern STR name16;
 extern STR VASS_LL1;
 extern STR name3;
 extern STR name82;
 extern STR S_boxed;
 extern STR immutable_part4;
 BOOL L891_;
 extern STR name75;
 extern STR name58;
 extern STR name23;
 extern STR name26;
 L4 = dest_tp;
 if ((*dTP_is242312711[TAG(L4)])(L4, ((OB) src_tp))) {
  L3 = TRUE;
 } else {
  L6 = dest_tp;
  if ((*dTP_is1811059018[TAG(L6)])(L6)) {
   L7 = src_tp;
   L5 = (*dTP_is1811059018[TAG(L7)])(L7);
  } else {
   L5 = FALSE;
  }
  L3 = L5;
 }
 if (L3) {
  L2 = TRUE;
 } else {
  L8 = dest_tp;
  L2 = (*dTP_is1999456142[TAG(L8)])(L8, src_tp);
 }
 if (L2) {
  L1 = TRUE;
 } else {
  L9 = src_tp;
  L1 = (*dTP_is1999456142[TAG(L9)])(L9, dest_tp);
 }
 L10 = L1;
 L111_=!(L10); 
 if (L111_) {
  create_self = ((OUT) NULL);
  ret_val1 = create_self;
  plus_self = ret_val1;
  plus_s = ((STR) &CASTINGERROR);
  stdout_self = ((FILE1) NULL);
  L13=ZALLOC(sizeof(struct FILE1_struct));
  if (L13==NULL) FATAL("Unable to allocate more memory");
  ((OB)L13)->header.tag=FILE1_tag;
  L12 = ((FILE1) L13);
  r = L12;
  
  SATTR(r,fp,stdout);
  ret_val2 = r;
  FILE_plus_STR(ret_val2, plus_s);
  create_self1 = ((OUT) NULL);
  ret_val3 = create_self1;
  plus_self1 = ret_val3;
  plus_s1 = ((STR) &dest_tp1);
  stdout_self1 = ((FILE1) NULL);
  L16=ZALLOC(sizeof(struct FILE1_struct));
  if (L16==NULL) FATAL("Unable to allocate more memory");
  ((OB)L16)->header.tag=FILE1_tag;
  L15 = ((FILE1) L16);
  r1 = L15;
  
  SATTR(r1,fp,stdout);
  ret_val4 = r1;
  FILE_plus_STR(ret_val4, plus_s1);
  if ((dest_tp==((dTP) NULL))) {
   create_self2 = ((OUT) NULL);
   ret_val5 = create_self2;
   plus_self2 = ret_val5;
   plus_s2 = ((STR) &void4);
   stdout_self2 = ((FILE1) NULL);
   L19=ZALLOC(sizeof(struct FILE1_struct));
   if (L19==NULL) FATAL("Unable to allocate more memory");
   ((OB)L19)->header.tag=FILE1_tag;
   L18 = ((FILE1) L19);
   r2 = L18;
   
   SATTR(r2,fp,stdout);
   ret_val6 = r2;
   FILE_plus_STR(ret_val6, plus_s2);
  }
  else {
   create_self3 = ((OUT) NULL);
   ret_val7 = create_self3;
   plus_self3 = ret_val7;
   L21 = dest_tp;
   plus_s3 = (*dTP_strrSTR[TAG(L21)])(L21);
   stdout_self3 = ((FILE1) NULL);
   L23=ZALLOC(sizeof(struct FILE1_struct));
   if (L23==NULL) FATAL("Unable to allocate more memory");
   ((OB)L23)->header.tag=FILE1_tag;
   L22 = ((FILE1) L23);
   r3 = L22;
   
   SATTR(r3,fp,stdout);
   ret_val8 = r3;
   FILE_plus_STR(ret_val8, plus_s3);
  }
  create_self4 = ((OUT) NULL);
  ret_val9 = create_self4;
  plus_self4 = ret_val9;
  plus_s4 = ((STR) &src_tp1);
  stdout_self4 = ((FILE1) NULL);
  L26=ZALLOC(sizeof(struct FILE1_struct));
  if (L26==NULL) FATAL("Unable to allocate more memory");
  ((OB)L26)->header.tag=FILE1_tag;
  L25 = ((FILE1) L26);
  r4 = L25;
  
  SATTR(r4,fp,stdout);
  ret_val10 = r4;
  FILE_plus_STR(ret_val10, plus_s4);
  if ((src_tp==((dTP) NULL))) {
   create_self5 = ((OUT) NULL);
   ret_val11 = create_self5;
   plus_self5 = ret_val11;
   plus_s5 = ((STR) &void4);
   stdout_self5 = ((FILE1) NULL);
   L29=ZALLOC(sizeof(struct FILE1_struct));
   if (L29==NULL) FATAL("Unable to allocate more memory");
   ((OB)L29)->header.tag=FILE1_tag;
   L28 = ((FILE1) L29);
   r5 = L28;
   
   SATTR(r5,fp,stdout);
   ret_val12 = r5;
   FILE_plus_STR(ret_val12, plus_s5);
  }
  else {
   create_self6 = ((OUT) NULL);
   ret_val13 = create_self6;
   plus_self6 = ret_val13;
   L31 = src_tp;
   plus_s6 = (*dTP_strrSTR[TAG(L31)])(L31);
   stdout_self6 = ((FILE1) NULL);
   L33=ZALLOC(sizeof(struct FILE1_struct));
   if (L33==NULL) FATAL("Unable to allocate more memory");
   ((OB)L33)->header.tag=FILE1_tag;
   L32 = ((FILE1) L33);
   r6 = L32;
   
   SATTR(r6,fp,stdout);
   ret_val14 = r6;
   FILE_plus_STR(ret_val14, plus_s6);
  }
  create_self7 = ((OUT) NULL);
  ret_val15 = create_self7;
  plus_self7 = ret_val15;
  plus_s7 = ((STR) &expr3);
  stdout_self7 = ((FILE1) NULL);
  L36=ZALLOC(sizeof(struct FILE1_struct));
  if (L36==NULL) FATAL("Unable to allocate more memory");
  ((OB)L36)->header.tag=FILE1_tag;
  L35 = ((FILE1) L36);
  r7 = L35;
  
  SATTR(r7,fp,stdout);
  ret_val17 = r7;
  FILE_plus_STR(ret_val17, plus_s7);
  ret_val16 = plus_self7;
  plus_self8 = ret_val16;
  plus_s8 = expr11;
  stdout_self8 = ((FILE1) NULL);
  L39=ZALLOC(sizeof(struct FILE1_struct));
  if (L39==NULL) FATAL("Unable to allocate more memory");
  ((OB)L39)->header.tag=FILE1_tag;
  L38 = ((FILE1) L39);
  r8 = L38;
  
  SATTR(r8,fp,stdout);
  ret_val19 = r8;
  FILE_plus_STR(ret_val19, plus_s8);
  ret_val18 = plus_self8;
  plus_self9 = ret_val18;
  plus_s9 = ((STR) &name9);
  stdout_self9 = ((FILE1) NULL);
  L42=ZALLOC(sizeof(struct FILE1_struct));
  if (L42==NULL) FATAL("Unable to allocate more memory");
  ((OB)L42)->header.tag=FILE1_tag;
  L41 = ((FILE1) L42);
  r9 = L41;
  
  SATTR(r9,fp,stdout);
  ret_val20 = r9;
  FILE_plus_STR(ret_val20, plus_s9);
  exit_self = ((UNIX) NULL);
  exit_code = 1;
  exit(exit_code);
 }
 use_ptr = FALSE;
 L44 = (mode1==((dMODE) NULL));
 L451_=!(L44); 
 if (L451_) {
  L47 = mode1;
  if ((*dMODE_814247358[TAG(L47)])(L47, ((dMODE) MODES_out_mode))) {
   L46 = TRUE;
  } else {
   L48 = mode1;
   L46 = (*dMODE_814247358[TAG(L48)])(L48, ((dMODE) MODES_inout_mode));
  }
  use_ptr = L46;
 }
 uses_tp_self = ATTR(self,code_c);
 uses_tp_t = dest_tp;
 CODE_F525364269(uses_tp_self, LAYOUT79932797(((LAYOUT_TBL) NULL), uses_tp_t));
 L49 = dest_tp;
 if ((*dTP_is242312711[TAG(L49)])(L49, ((OB) src_tp))) {
  res = expr11;
 }
 else {
  L51 = dest_tp;
  if ((*dTP_is1811059018[TAG(L51)])(L51)) {
   L53 = src_tp;
   if ((*dTP_is1334578382[TAG(L53)])(L53)) {
    L52 = TRUE;
   } else {
    L54 = src_tp;
    L52 = (*dTP_is242312711[TAG(L54)])(L54, ((OB) TP_BUI389447236));
   }
   L50 = L52;
  } else {
   L50 = FALSE;
  }
  if (L50) {
   dec_local_commen = self;
   dec_local_commen1 = dest_tp;
   plus_self10 = ((STR) &localforboxed);
   L55 = src_tp;
   plus_sarg = (*dTP_strrSTR[TAG(L55)])(L55);
   ret_val21 = STR_ap2004550586(plus_self10, plus_sarg);
   dec_local_commen2 = ret_val21;
   res1 = CGEN_d1425664957(dec_local_commen, dec_local_commen1);
   comment_self = dec_local_commen;
   comment_f = ATTR(dec_local_commen,code_c);
   comment_com = dec_local_commen2;
   if (ATTR(comment_self,pretty)) {
    plus_self11 = comment_f;
    plus_s10 = ((dSTR) ((STR) &name78));
    L56 = (plus_s10==((dSTR) NULL));
    L571_=!(L56); 
    if (L571_) {
     L58 = ATTR(plus_self11,ntext);
     L59 = plus_s10;
     SATTR(plus_self11,ntext,FSTR_p1752847026(L58, (*dSTR_strrSTR[TAG(L59)])(L59)));
    }
    ret_val23 = plus_self11;
    plus_self12 = ret_val23;
    plus_s11 = ((dSTR) comment_com);
    L60 = (plus_s11==((dSTR) NULL));
    L611_=!(L60); 
    if (L611_) {
     L62 = ATTR(plus_self12,ntext);
     L63 = plus_s11;
     SATTR(plus_self12,ntext,FSTR_p1752847026(L62, (*dSTR_strrSTR[TAG(L63)])(L63)));
    }
    ret_val24 = plus_self12;
    plus_self13 = ret_val24;
    plus_s12 = ((dSTR) ((STR) &name79));
    L64 = ATTR(plus_self13,ntext);
    L65 = plus_s12;
    SATTR(plus_self13,ntext,FSTR_p1752847026(L64, (*dSTR_strrSTR[TAG(L65)])(L65)));
   }
   ret_val22 = res1;
   res = ret_val22;
   ndefer_self = self;
   plus_self14 = res;
   plus_sarg1 = ((STR) &name80);
   ret_val25 = STR_ap2004550586(plus_self14, plus_sarg1);
   plus_self15 = ret_val25;
   mang_self = self;
   mang_ob = ((OB) dest_tp);
   ret_val26 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
   plus_sarg2 = ret_val26;
   ret_val27 = STR_ap2004550586(plus_self15, plus_sarg2);
   plus_self16 = ret_val27;
   plus_sarg3 = ((STR) &name26);
   ret_val28 = STR_ap2004550586(plus_self16, plus_sarg3);
   plus_self17 = ret_val28;
   plus_sarg4 = CGEN_a1155118157(self, src_tp);
   ret_val29 = STR_ap2004550586(plus_self17, plus_sarg4);
   plus_self18 = ret_val29;
   plus_sarg5 = ((STR) &name16);
   ret_val30 = STR_ap2004550586(plus_self18, plus_sarg5);
   ndefer_s = ret_val30;
   CGEN_d695831906(ndefer_self);
   SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
   if (use_ptr) {
    plus_self19 = ((STR) &name58);
    plus_sarg6 = expr11;
    ret_val31 = STR_ap2004550586(plus_self19, plus_sarg6);
    expr11 = ret_val31;
   }
   ndefer_self1 = self;
   plus_self20 = ((STR) &name75);
   mang_self1 = self;
   mang_ob1 = ((OB) src_tp);
   ret_val32 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
   plus_sarg7 = ret_val32;
   ret_val33 = STR_ap2004550586(plus_self20, plus_sarg7);
   plus_self21 = ret_val33;
   plus_sarg8 = ((STR) &S_boxed);
   ret_val34 = STR_ap2004550586(plus_self21, plus_sarg8);
   plus_self22 = ret_val34;
   plus_sarg9 = res;
   ret_val35 = STR_ap2004550586(plus_self22, plus_sarg9);
   plus_self23 = ret_val35;
   plus_sarg10 = ((STR) &immutable_part1);
   ret_val36 = STR_ap2004550586(plus_self23, plus_sarg10);
   plus_self24 = ret_val36;
   plus_sarg11 = expr11;
   ret_val37 = STR_ap2004550586(plus_self24, plus_sarg11);
   plus_self25 = ret_val37;
   plus_sarg12 = ((STR) &name16);
   ret_val38 = STR_ap2004550586(plus_self25, plus_sarg12);
   ndefer_s1 = ret_val38;
   CGEN_d695831906(ndefer_self1);
   SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
  }
  else {
   L68 = dest_tp;
   if ((*dTP_is1334578382[TAG(L68)])(L68)) {
    L67 = TRUE;
   } else {
    L69 = dest_tp;
    L67 = (*dTP_is242312711[TAG(L69)])(L69, ((OB) TP_BUI389447236));
   }
   if (L67) {
    L70 = src_tp;
    L66 = (*dTP_is1811059018[TAG(L70)])(L70);
   } else {
    L66 = FALSE;
   }
   if (L66) {
    if (use_ptr) {
     plus_self26 = ((STR) &name58);
     plus_sarg13 = expr11;
     ret_val39 = STR_ap2004550586(plus_self26, plus_sarg13);
     expr11 = ret_val39;
     plus_self27 = ((STR) &localp1561414680);
     L71 = src_tp;
     plus_sarg14 = (*dTP_strrSTR[TAG(L71)])(L71);
     ret_val40 = STR_ap2004550586(plus_self27, plus_sarg14);
     res = CGEN_d531153180(self, dest_tp, ret_val40);
    }
    else {
     dec_local_commen3 = self;
     dec_local_commen4 = dest_tp;
     plus_self28 = ((STR) &localforunboxed);
     L72 = src_tp;
     plus_sarg15 = (*dTP_strrSTR[TAG(L72)])(L72);
     ret_val41 = STR_ap2004550586(plus_self28, plus_sarg15);
     dec_local_commen5 = ret_val41;
     res2 = CGEN_d1425664957(dec_local_commen3, dec_local_commen4);
     comment_self1 = dec_local_commen3;
     comment_f1 = ATTR(dec_local_commen3,code_c);
     comment_com1 = dec_local_commen5;
     if (ATTR(comment_self1,pretty)) {
      plus_self29 = comment_f1;
      plus_s13 = ((dSTR) ((STR) &name78));
      L73 = (plus_s13==((dSTR) NULL));
      L741_=!(L73); 
      if (L741_) {
       L75 = ATTR(plus_self29,ntext);
       L76 = plus_s13;
       SATTR(plus_self29,ntext,FSTR_p1752847026(L75, (*dSTR_strrSTR[TAG(L76)])(L76)));
      }
      ret_val43 = plus_self29;
      plus_self30 = ret_val43;
      plus_s14 = ((dSTR) comment_com1);
      L77 = (plus_s14==((dSTR) NULL));
      L781_=!(L77); 
      if (L781_) {
       L79 = ATTR(plus_self30,ntext);
       L80 = plus_s14;
       SATTR(plus_self30,ntext,FSTR_p1752847026(L79, (*dSTR_strrSTR[TAG(L80)])(L80)));
      }
      ret_val44 = plus_self30;
      plus_self31 = ret_val44;
      plus_s15 = ((dSTR) ((STR) &name79));
      L81 = ATTR(plus_self31,ntext);
      L82 = plus_s15;
      SATTR(plus_self31,ntext,FSTR_p1752847026(L81, (*dSTR_strrSTR[TAG(L82)])(L82)));
     }
     ret_val42 = res2;
     res = ret_val42;
    }
    if (ATTR(ATTR(self,prog),distributed)) {
     L83 = dest_tp;
     if ((*dTP_is418055720[TAG(L83)])(L83)) {
      ndefer_self2 = self;
      plus_self32 = ((STR) &F_VA_RATTR_NN1);
      plus_sarg16 = res;
      ret_val45 = STR_ap2004550586(plus_self32, plus_sarg16);
      plus_self33 = ret_val45;
      plus_sarg17 = ((STR) &name3);
      ret_val46 = STR_ap2004550586(plus_self33, plus_sarg17);
      plus_self34 = ret_val46;
      mang_self2 = self;
      mang_ob2 = ((OB) dest_tp);
      ret_val47 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, ((OB) NULL));
      plus_sarg18 = ret_val47;
      ret_val48 = STR_ap2004550586(plus_self34, plus_sarg18);
      plus_self35 = ret_val48;
      plus_sarg19 = ((STR) &S_boxed1);
      ret_val49 = STR_ap2004550586(plus_self35, plus_sarg19);
      plus_self36 = ret_val49;
      plus_sarg20 = expr11;
      ret_val50 = STR_ap2004550586(plus_self36, plus_sarg20);
      plus_self37 = ret_val50;
      plus_sarg21 = ((STR) &immutable_part2);
      ret_val51 = STR_ap2004550586(plus_self37, plus_sarg21);
      ndefer_s2 = ret_val51;
      CGEN_d695831906(ndefer_self2);
      SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
     }
     else {
      ndefer_self3 = self;
      plus_self38 = ((STR) &F_V_RATTR_LL1);
      mang_self3 = self;
      mang_ob3 = ((OB) dest_tp);
      ret_val52 = MANGLE119219986(ATTR(mang_self3,mangler), mang_ob3, ((OB) NULL));
      plus_sarg22 = ret_val52;
      ret_val53 = STR_ap2004550586(plus_self38, plus_sarg22);
      plus_self39 = ret_val53;
      plus_sarg23 = ((STR) &name3);
      ret_val54 = STR_ap2004550586(plus_self39, plus_sarg23);
      plus_self40 = ret_val54;
      plus_sarg24 = res;
      ret_val55 = STR_ap2004550586(plus_self40, plus_sarg24);
      plus_self41 = ret_val55;
      plus_sarg25 = ((STR) &name3);
      ret_val56 = STR_ap2004550586(plus_self41, plus_sarg25);
      plus_self42 = ret_val56;
      mang_self4 = self;
      mang_ob4 = ((OB) dest_tp);
      ret_val57 = MANGLE119219986(ATTR(mang_self4,mangler), mang_ob4, ((OB) NULL));
      plus_sarg26 = ret_val57;
      ret_val58 = STR_ap2004550586(plus_self42, plus_sarg26);
      plus_self43 = ret_val58;
      plus_sarg27 = ((STR) &S_boxed1);
      ret_val59 = STR_ap2004550586(plus_self43, plus_sarg27);
      plus_self44 = ret_val59;
      plus_sarg28 = expr11;
      ret_val60 = STR_ap2004550586(plus_self44, plus_sarg28);
      plus_self45 = ret_val60;
      plus_sarg29 = ((STR) &immutable_part2);
      ret_val61 = STR_ap2004550586(plus_self45, plus_sarg29);
      ndefer_s3 = ret_val61;
      CGEN_d695831906(ndefer_self3);
      SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
     }
    }
    else {
     L85 = dest_tp;
     if ((*dTP_is418055720[TAG(L85)])(L85)) {
      L84 = TRUE;
     } else {
      psather_self = ATTR(self,prog);
      if (ATTR(psather_self,threads)) {
       L86 = TRUE;
      } else {
       L86 = ATTR(psather_self,distributed);
      }
      ret_val62 = L86;
      L87 = ret_val62;
      L881_=!(L87); 
      L84 = L881_;
     }
     if (L84) {
      plus_self46 = ((STR) &name75);
      mang_self5 = self;
      mang_ob5 = ((OB) dest_tp);
      ret_val63 = MANGLE119219986(ATTR(mang_self5,mangler), mang_ob5, ((OB) NULL));
      plus_sarg30 = ret_val63;
      ret_val64 = STR_ap2004550586(plus_self46, plus_sarg30);
      plus_self47 = ret_val64;
      plus_sarg31 = ((STR) &S_boxed);
      ret_val65 = STR_ap2004550586(plus_self47, plus_sarg31);
      plus_self48 = ret_val65;
      plus_sarg32 = expr11;
      ret_val66 = STR_ap2004550586(plus_self48, plus_sarg32);
      plus_self49 = ret_val66;
      plus_sarg33 = ((STR) &immutable_part3);
      ret_val67 = STR_ap2004550586(plus_self49, plus_sarg33);
      immutable_part = ret_val67;
      if (use_ptr) {
       immutable_part = STR_ap1693864410(((STR) &name81), immutable_part, ((STR) &name26));
      }
      ndefer_self4 = self;
      ndefer_s4 = STR_ap1417971546(res, ((STR) &name36), immutable_part, ((STR) &name16));
      CGEN_d695831906(ndefer_self4);
      SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
     }
     else {
      ndefer_self5 = self;
      plus_self50 = ((STR) &VASS_LL1);
      plus_sarg34 = res;
      ret_val68 = STR_ap2004550586(plus_self50, plus_sarg34);
      plus_self51 = ret_val68;
      plus_sarg35 = ((STR) &name3);
      ret_val69 = STR_ap2004550586(plus_self51, plus_sarg35);
      plus_self52 = ret_val69;
      mang_self6 = self;
      mang_ob6 = ((OB) dest_tp);
      ret_val70 = MANGLE119219986(ATTR(mang_self6,mangler), mang_ob6, ((OB) NULL));
      plus_sarg36 = ret_val70;
      ret_val71 = STR_ap2004550586(plus_self52, plus_sarg36);
      plus_self53 = ret_val71;
      plus_sarg37 = ((STR) &name82);
      ret_val72 = STR_ap2004550586(plus_self53, plus_sarg37);
      plus_self54 = ret_val72;
      mang_self7 = self;
      mang_ob7 = ((OB) dest_tp);
      ret_val73 = MANGLE119219986(ATTR(mang_self7,mangler), mang_ob7, ((OB) NULL));
      plus_sarg38 = ret_val73;
      ret_val74 = STR_ap2004550586(plus_self54, plus_sarg38);
      plus_self55 = ret_val74;
      plus_sarg39 = ((STR) &S_boxed);
      ret_val75 = STR_ap2004550586(plus_self55, plus_sarg39);
      plus_self56 = ret_val75;
      plus_sarg40 = expr11;
      ret_val76 = STR_ap2004550586(plus_self56, plus_sarg40);
      plus_self57 = ret_val76;
      plus_sarg41 = ((STR) &immutable_part4);
      ret_val77 = STR_ap2004550586(plus_self57, plus_sarg41);
      ndefer_s5 = ret_val77;
      CGEN_d695831906(ndefer_self5);
      SATTR(ndefer_self5,routine_code,FSTR_p1752847026(ATTR(ndefer_self5,routine_code), ndefer_s5));
     }
    }
   }
   else {
    L891_=!(only_boxing); 
    if (L891_) {
     plus_self58 = ((STR) &name75);
     mang_self8 = self;
     mang_ob8 = ((OB) dest_tp);
     ret_val78 = MANGLE119219986(ATTR(mang_self8,mangler), mang_ob8, ((OB) NULL));
     plus_sarg42 = ret_val78;
     ret_val79 = STR_ap2004550586(plus_self58, plus_sarg42);
     res = ret_val79;
     if (use_ptr) {
      plus_self59 = res;
      plus_sarg43 = ((STR) &name58);
      ret_val80 = STR_ap2004550586(plus_self59, plus_sarg43);
      res = ret_val80;
     }
     plus_self60 = res;
     plus_sarg44 = ((STR) &name23);
     ret_val81 = STR_ap2004550586(plus_self60, plus_sarg44);
     plus_self61 = ret_val81;
     plus_sarg45 = expr11;
     ret_val82 = STR_ap2004550586(plus_self61, plus_sarg45);
     plus_self62 = ret_val82;
     plus_sarg46 = ((STR) &name26);
     ret_val83 = STR_ap2004550586(plus_self62, plus_sarg46);
     res = ret_val83;
    }
    else {
     res = expr11;
    }
   }
  }
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_d531153180(CGEN self, dTP t, STR com) {
 STR ret_val;
 STR res;
 MANGLE genlocal_self;
 OB genlocal_ns;
 STR ret_val1;
 STR name111;
 MANGLE space_self;
 OB space_ns;
 NAMESPACE ret_val2;
 NAMESPACE r;
 NAMESPACE create_self;
 NAMESPACE ret_val3;
 NAMESPACE r1;
 NAMEMAP create_self1;
 NAMEMAP ret_val4;
 FSETSTR create_self2;
 FSETSTR ret_val5;
 FMAPSTRINT create_self3;
 FMAPSTRINT ret_val6;
 CODE_FILE plus_self;
 dSTR plus_s;
 CODE_FILE ret_val7;
 CODE_FILE plus_self1;
 dSTR plus_s1;
 CODE_FILE ret_val8;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val9;
 CODE_FILE plus_self2;
 dSTR plus_s2;
 CODE_FILE ret_val10;
 CODE_FILE plus_self3;
 dSTR plus_s3;
 CODE_FILE ret_val11;
 CODE_FILE plus_self4;
 dSTR plus_s4;
 CODE_FILE ret_val12;
 CODE_FILE plus_self5;
 dSTR plus_s5;
 CGEN comment_self;
 CODE_FILE comment_f;
 STR comment_com;
 CODE_FILE plus_self6;
 dSTR plus_s6;
 CODE_FILE ret_val13;
 CODE_FILE plus_self7;
 dSTR plus_s7;
 CODE_FILE ret_val14;
 CODE_FILE plus_self8;
 dSTR plus_s8;
 NAMESPACE L1;
 OB L2;
 extern STR L;
 BOOL L3;
 BOOL L41_;
 FSTR L5;
 dSTR L6;
 dSTR L7;
 OB L8;
 BOOL L9;
 BOOL L101_;
 FSTR L11;
 dSTR L12;
 BOOL L13;
 BOOL L141_;
 FSTR L15;
 dSTR L16;
 extern STR name77;
 BOOL L17;
 BOOL L181_;
 FSTR L19;
 dSTR L20;
 BOOL L21;
 BOOL L221_;
 FSTR L23;
 dSTR L24;
 dSTR L25;
 OB L26;
 FSTR L27;
 dSTR L28;
 extern STR name78;
 BOOL L29;
 BOOL L301_;
 FSTR L31;
 dSTR L32;
 BOOL L33;
 BOOL L341_;
 FSTR L35;
 dSTR L36;
 extern STR name79;
 FSTR L37;
 dSTR L38;
 genlocal_self = ATTR(self,mangler);
 genlocal_ns = ((OB) ATTR(self,current_sig));
 space_self = genlocal_self;
 space_ns = genlocal_ns;
 if ((space_ns==((OB) NULL))) {
  space_ns = ((OB) space_self);
 }
 r = FMAPdO790518483(ATTR(space_self,namespaces), space_ns);
 if ((r==((NAMESPACE) NULL))) {
  create_self = ((NAMESPACE) NULL);
  L2=ZALLOC(sizeof(struct NAMESPACE_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=NAMESPACE_tag;
  L1 = ((NAMESPACE) L2);
  r1 = L1;
  create_self1 = ((NAMEMAP) NULL);
  ret_val4 = ((NAMEMAP) NULL);
  SATTR(r1,map,ret_val4);
  create_self2 = ((FSETSTR) NULL);
  ret_val5 = ((FSETSTR) NULL);
  SATTR(r1,set,ret_val5);
  SATTR(r1,counter,0);
  create_self3 = ((FMAPSTRINT) NULL);
  ret_val6 = ((FMAPSTRINT) NULL);
  SATTR(r1,uniques,ret_val6);
  ret_val3 = r1;
  r = ret_val3;
  SATTR(space_self,namespaces,FMAPdO1199466663(ATTR(space_self,namespaces), space_ns, r));
 }
 ret_val2 = r;
 name111 = NAMESP180237388(ret_val2, ((STR) &L));
 SATTR(genlocal_self,used_by_local,FSETST1404644833(ATTR(genlocal_self,used_by_local), name111));
 ret_val1 = name111;
 res = ret_val1;
 plus_self = ATTR(self,code_c);
 plus_s = ((dSTR) CGEN_eolrSTR(self));
 L3 = (plus_s==((dSTR) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(plus_self,ntext);
  L6 = plus_s;
  SATTR(plus_self,ntext,FSTR_p1752847026(L5, (*dSTR_strrSTR[TAG(L6)])(L6)));
 }
 ret_val7 = plus_self;
 plus_self1 = ret_val7;
 L8=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 memset(L8,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L8)->header.tag=CHAR_tag;
 L7 = (dSTR)((CHAR_boxed) L8);
 ((CHAR_boxed) L7)->immutable_part = ' ';
 plus_s1 = L7;
 L9 = (plus_s1==((dSTR) NULL));
 L101_=!(L9); 
 if (L101_) {
  L11 = ATTR(plus_self1,ntext);
  L12 = plus_s1;
  SATTR(plus_self1,ntext,FSTR_p1752847026(L11, (*dSTR_strrSTR[TAG(L12)])(L12)));
 }
 ret_val8 = plus_self1;
 plus_self2 = ret_val8;
 mang_self = self;
 mang_ob = ((OB) t);
 ret_val9 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
 plus_s2 = ((dSTR) ret_val9);
 L13 = (plus_s2==((dSTR) NULL));
 L141_=!(L13); 
 if (L141_) {
  L15 = ATTR(plus_self2,ntext);
  L16 = plus_s2;
  SATTR(plus_self2,ntext,FSTR_p1752847026(L15, (*dSTR_strrSTR[TAG(L16)])(L16)));
 }
 ret_val10 = plus_self2;
 plus_self3 = ret_val10;
 plus_s3 = ((dSTR) ((STR) &name77));
 L17 = (plus_s3==((dSTR) NULL));
 L181_=!(L17); 
 if (L181_) {
  L19 = ATTR(plus_self3,ntext);
  L20 = plus_s3;
  SATTR(plus_self3,ntext,FSTR_p1752847026(L19, (*dSTR_strrSTR[TAG(L20)])(L20)));
 }
 ret_val11 = plus_self3;
 plus_self4 = ret_val11;
 plus_s4 = ((dSTR) res);
 L21 = (plus_s4==((dSTR) NULL));
 L221_=!(L21); 
 if (L221_) {
  L23 = ATTR(plus_self4,ntext);
  L24 = plus_s4;
  SATTR(plus_self4,ntext,FSTR_p1752847026(L23, (*dSTR_strrSTR[TAG(L24)])(L24)));
 }
 ret_val12 = plus_self4;
 plus_self5 = ret_val12;
 L26=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L26==NULL) FATAL("Unable to allocate more memory");
 memset(L26,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L26)->header.tag=CHAR_tag;
 L25 = (dSTR)((CHAR_boxed) L26);
 ((CHAR_boxed) L25)->immutable_part = ';';
 plus_s5 = L25;
 L27 = ATTR(plus_self5,ntext);
 L28 = plus_s5;
 SATTR(plus_self5,ntext,FSTR_p1752847026(L27, (*dSTR_strrSTR[TAG(L28)])(L28)));
 comment_self = self;
 comment_f = ATTR(self,code_c);
 comment_com = com;
 if (ATTR(comment_self,pretty)) {
  plus_self6 = comment_f;
  plus_s6 = ((dSTR) ((STR) &name78));
  L29 = (plus_s6==((dSTR) NULL));
  L301_=!(L29); 
  if (L301_) {
   L31 = ATTR(plus_self6,ntext);
   L32 = plus_s6;
   SATTR(plus_self6,ntext,FSTR_p1752847026(L31, (*dSTR_strrSTR[TAG(L32)])(L32)));
  }
  ret_val13 = plus_self6;
  plus_self7 = ret_val13;
  plus_s7 = ((dSTR) comment_com);
  L33 = (plus_s7==((dSTR) NULL));
  L341_=!(L33); 
  if (L341_) {
   L35 = ATTR(plus_self7,ntext);
   L36 = plus_s7;
   SATTR(plus_self7,ntext,FSTR_p1752847026(L35, (*dSTR_strrSTR[TAG(L36)])(L36)));
  }
  ret_val14 = plus_self7;
  plus_self8 = ret_val14;
  plus_s8 = ((dSTR) ((STR) &name79));
  L37 = ATTR(plus_self8,ntext);
  L38 = plus_s8;
  SATTR(plus_self8,ntext,FSTR_p1752847026(L37, (*dSTR_strrSTR[TAG(L38)])(L38)));
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_d815167520(CGEN self, dTP t) {
 STR ret_val;
 CODE_FILE uses_tp_self;
 dTP uses_tp_t;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val3;
 dTP L1;
 extern STR S_zero;
 extern STR name75;
 extern STR NULL1;
 uses_tp_self = ATTR(self,code_c);
 uses_tp_t = t;
 CODE_F525364269(uses_tp_self, LAYOUT79932797(((LAYOUT_TBL) NULL), uses_tp_t));
 L1 = t;
 if ((*dTP_is1334578382[TAG(L1)])(L1)) {
  mang_self = self;
  mang_ob = ((OB) t);
  ret_val1 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
  plus_self = ret_val1;
  plus_sarg = ((STR) &S_zero);
  ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
  ret_val = ret_val2;
  return ret_val;
 }
 else {
  mang_self1 = self;
  mang_ob1 = ((OB) t);
  ret_val3 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
  ret_val = STR_ap1693864410(((STR) &name75), ret_val3, ((STR) &NULL1));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_eolrSTR(CGEN self) {
 STR ret_val;
 STR plus_self;
 INT plus_arg = INT_zero;
 STR ret_val1;
 STR s;
 INT str_self = INT_zero;
 STR ret_val2;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val3;
 FSTR str_self1;
 STR ret_val4;
 STR plus_self1;
 STR plus_sarg;
 STR ret_val5;
 extern STR line2;
 BOOL L1;
 BOOL L21_;
 extern STR name9;
 extern STR name1;
 extern STR name9;
 if (ATTR(self,debug1)) {
  if (ATTR(self,pretty)) {
   plus_self = ((STR) &line2);
   plus_arg = ATTR(self,last_lineno);
   str_self = plus_arg;
   clear_self = buf1;
   L1 = (clear_self==((FSTR) NULL));
   L21_=!(L1); 
   if (L21_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val3;
   str_self1 = buf1;
   ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val2 = ret_val4;
   s = ret_val2;
   ret_val1 = STR_ap1077157958(plus_self, s, TRUE);
   plus_self1 = ret_val1;
   plus_sarg = ((STR) &name9);
   ret_val5 = STR_ap2004550586(plus_self1, plus_sarg);
   ret_val = ret_val5;
   return ret_val;
  }
  else {
   ret_val = ((STR) &name1);
   return ret_val;
  }
 }
 else {
  ret_val = ((STR) &name9);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_f2084214268(CGEN self, dTP tp) {
 STR ret_val;
 dTP L11;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val1;
 STR plus_self;
 CHAR plus_arg = CHAR_zero;
 STR ret_val2;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val3;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val4;
 STR r;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val5;
 STR plus_self1;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val6;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val7;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val8;
 STR r1;
 CGEN mang_self2;
 OB mang_ob2;
 STR ret_val9;
 STR plus_self2;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val10;
 STR s2;
 CHAR str_self2 = CHAR_zero;
 STR ret_val11;
 STR create_self2;
 CHAR create_c2 = CHAR_zero;
 STR ret_val12;
 STR r2;
 CGEN mang_self3;
 OB mang_ob3;
 STR ret_val13;
 STR plus_self3;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val14;
 STR s3;
 CHAR str_self3 = CHAR_zero;
 STR ret_val15;
 STR create_self3;
 CHAR create_c3 = CHAR_zero;
 STR ret_val16;
 STR r3;
 CGEN mang_self4;
 OB mang_ob4;
 STR ret_val17;
 STR plus_self4;
 CHAR plus_arg4 = CHAR_zero;
 STR ret_val18;
 STR s4;
 CHAR str_self4 = CHAR_zero;
 STR ret_val19;
 STR create_self4;
 CHAR create_c4 = CHAR_zero;
 STR ret_val20;
 STR r4;
 CGEN mang_self5;
 OB mang_ob5;
 STR ret_val21;
 STR plus_self5;
 CHAR plus_arg5 = CHAR_zero;
 STR ret_val22;
 STR s5;
 CHAR str_self5 = CHAR_zero;
 STR ret_val23;
 STR create_self5;
 CHAR create_c5 = CHAR_zero;
 STR ret_val24;
 STR r5;
 CGEN mang_self6;
 OB mang_ob6;
 STR ret_val25;
 STR plus_self6;
 CHAR plus_arg6 = CHAR_zero;
 STR ret_val26;
 STR s6;
 CHAR str_self6 = CHAR_zero;
 STR ret_val27;
 STR create_self6;
 CHAR create_c6 = CHAR_zero;
 STR ret_val28;
 STR r6;
 CGEN mang_self7;
 OB mang_ob7;
 STR ret_val29;
 STR plus_self7;
 STR plus_sarg;
 STR ret_val30;
 CGEN mang_self8;
 OB mang_ob8;
 STR ret_val31;
 CGEN mang_self9;
 OB mang_ob9;
 STR ret_val32;
 dTP L2;
 STR L3;
 INT L4;
 OB L5;
 INT L6;
 dTP L8;
 STR L9;
 INT L10;
 OB L12;
 INT L13;
 dTP L15;
 STR L16;
 INT L17;
 OB L18;
 INT L19;
 dTP L21;
 STR L22;
 INT L23;
 OB L24;
 INT L25;
 dTP L27;
 STR L28;
 INT L29;
 OB L30;
 INT L31;
 dTP L33;
 STR L34;
 INT L35;
 OB L36;
 INT L37;
 dTP L39;
 STR L40;
 INT L41;
 OB L42;
 INT L43;
 dTP L45;
 extern STR name58;
 dTP L46;
 L11 = tp;
 L2 = L11;
 if ((*dTP_is242312711[TAG(L2)])(L2, ((OB) TP_BUI403444172))) {
  mang_self = self;
  mang_ob = ((OB) tp);
  ret_val1 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
  plus_self = ret_val1;
  plus_arg = '*';
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
  ret_val4 = r;
  ret_val3 = ret_val4;
  s = ret_val3;
  ret_val2 = STR_ap1077157958(plus_self, s, TRUE);
  ret_val = ret_val2;
  return ret_val;
 }
 else {
  L8 = L11;
  if ((*dTP_is242312711[TAG(L8)])(L8, ((OB) TP_BUI1194559023))) {
   mang_self1 = self;
   mang_ob1 = ((OB) tp);
   ret_val5 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
   plus_self1 = ret_val5;
   plus_arg1 = '*';
   str_self1 = plus_arg1;
   create_self1 = ((STR) NULL);
   create_c1 = str_self1;
   L10 = 1;
   L13=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L10)*sizeof(CHAR);
   L12=ZALLOC_LEAF_BIG(L13);
   if (L12==NULL) FATAL("Unable to allocate more memory");
   memset(L12,0,L13);
   ((OB)L12)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L12)->header.destroyed=0;
#endif

   L9 = ((STR) L12);
   L9->asize = L10;
   r1 = L9;
   SARR((STR)r1,0,create_c1); 
   ;
   ret_val8 = r1;
   ret_val7 = ret_val8;
   s1 = ret_val7;
   ret_val6 = STR_ap1077157958(plus_self1, s1, TRUE);
   ret_val = ret_val6;
   return ret_val;
  }
  else {
   L15 = L11;
   if ((*dTP_is242312711[TAG(L15)])(L15, ((OB) TP_BUI374366290))) {
    mang_self2 = self;
    mang_ob2 = ((OB) tp);
    ret_val9 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, ((OB) NULL));
    plus_self2 = ret_val9;
    plus_arg2 = '*';
    str_self2 = plus_arg2;
    create_self2 = ((STR) NULL);
    create_c2 = str_self2;
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
    r2 = L16;
    SARR((STR)r2,0,create_c2); 
    ;
    ret_val12 = r2;
    ret_val11 = ret_val12;
    s2 = ret_val11;
    ret_val10 = STR_ap1077157958(plus_self2, s2, TRUE);
    ret_val = ret_val10;
    return ret_val;
   }
   else {
    L21 = L11;
    if ((*dTP_is242312711[TAG(L21)])(L21, ((OB) TP_BUI1370948386))) {
     mang_self3 = self;
     mang_ob3 = ((OB) tp);
     ret_val13 = MANGLE119219986(ATTR(mang_self3,mangler), mang_ob3, ((OB) NULL));
     plus_self3 = ret_val13;
     plus_arg3 = '*';
     str_self3 = plus_arg3;
     create_self3 = ((STR) NULL);
     create_c3 = str_self3;
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
     r3 = L22;
     SARR((STR)r3,0,create_c3); 
     ;
     ret_val16 = r3;
     ret_val15 = ret_val16;
     s3 = ret_val15;
     ret_val14 = STR_ap1077157958(plus_self3, s3, TRUE);
     ret_val = ret_val14;
     return ret_val;
    }
    else {
     L27 = L11;
     if ((*dTP_is242312711[TAG(L27)])(L27, ((OB) TP_BUI1805763264))) {
      mang_self4 = self;
      mang_ob4 = ((OB) tp);
      ret_val17 = MANGLE119219986(ATTR(mang_self4,mangler), mang_ob4, ((OB) NULL));
      plus_self4 = ret_val17;
      plus_arg4 = '*';
      str_self4 = plus_arg4;
      create_self4 = ((STR) NULL);
      create_c4 = str_self4;
      L29 = 1;
      L31=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L29)*sizeof(CHAR);
      L30=ZALLOC_LEAF_BIG(L31);
      if (L30==NULL) FATAL("Unable to allocate more memory");
      memset(L30,0,L31);
      ((OB)L30)->header.tag=STR_tag;
#ifdef DESTROY_CHK

        ((OB)L30)->header.destroyed=0;
#endif

      L28 = ((STR) L30);
      L28->asize = L29;
      r4 = L28;
      SARR((STR)r4,0,create_c4); 
      ;
      ret_val20 = r4;
      ret_val19 = ret_val20;
      s4 = ret_val19;
      ret_val18 = STR_ap1077157958(plus_self4, s4, TRUE);
      ret_val = ret_val18;
      return ret_val;
     }
     else {
      L33 = L11;
      if ((*dTP_is242312711[TAG(L33)])(L33, ((OB) TP_BUI633637838))) {
       mang_self5 = self;
       mang_ob5 = ((OB) tp);
       ret_val21 = MANGLE119219986(ATTR(mang_self5,mangler), mang_ob5, ((OB) NULL));
       plus_self5 = ret_val21;
       plus_arg5 = '*';
       str_self5 = plus_arg5;
       create_self5 = ((STR) NULL);
       create_c5 = str_self5;
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
       r5 = L34;
       SARR((STR)r5,0,create_c5); 
       ;
       ret_val24 = r5;
       ret_val23 = ret_val24;
       s5 = ret_val23;
       ret_val22 = STR_ap1077157958(plus_self5, s5, TRUE);
       ret_val = ret_val22;
       return ret_val;
      }
      else {
       L39 = L11;
       if ((*dTP_is242312711[TAG(L39)])(L39, ((OB) TP_BUI1032249067))) {
        mang_self6 = self;
        mang_ob6 = ((OB) tp);
        ret_val25 = MANGLE119219986(ATTR(mang_self6,mangler), mang_ob6, ((OB) NULL));
        plus_self6 = ret_val25;
        plus_arg6 = '*';
        str_self6 = plus_arg6;
        create_self6 = ((STR) NULL);
        create_c6 = str_self6;
        L41 = 1;
        L43=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L41)*sizeof(CHAR);
        L42=ZALLOC_LEAF_BIG(L43);
        if (L42==NULL) FATAL("Unable to allocate more memory");
        memset(L42,0,L43);
        ((OB)L42)->header.tag=STR_tag;
#ifdef DESTROY_CHK

          ((OB)L42)->header.destroyed=0;
#endif

        L40 = ((STR) L42);
        L40->asize = L41;
        r6 = L40;
        SARR((STR)r6,0,create_c6); 
        ;
        ret_val28 = r6;
        ret_val27 = ret_val28;
        s6 = ret_val27;
        ret_val26 = STR_ap1077157958(plus_self6, s6, TRUE);
        ret_val = ret_val26;
        return ret_val;
       }
       else {
        L45 = L11;
        if ((*dTP_is242312711[TAG(L45)])(L45, ((OB) TP_BUI2088674906))) {
         mang_self7 = self;
         mang_ob7 = ((OB) TP_BUI1805763264);
         ret_val29 = MANGLE119219986(ATTR(mang_self7,mangler), mang_ob7, ((OB) NULL));
         plus_self7 = ret_val29;
         plus_sarg = ((STR) &name58);
         ret_val30 = STR_ap2004550586(plus_self7, plus_sarg);
         ret_val = ret_val30;
         return ret_val;
        }
        else {
         L46 = L11;
         if ((*dTP_is242312711[TAG(L46)])(L46, ((OB) TP_BUI1116976640))) {
          mang_self8 = self;
          mang_ob8 = ((OB) TP_BUI1116976640);
          ret_val31 = MANGLE119219986(ATTR(mang_self8,mangler), mang_ob8, ((OB) NULL));
          ret_val = ret_val31;
          return ret_val;
         }
         else {
          mang_self9 = self;
          mang_ob9 = ((OB) tp);
          ret_val32 = MANGLE119219986(ATTR(mang_self9,mangler), mang_ob9, ((OB) NULL));
          ret_val = ret_val32;
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


#undef IS_ITER
#define IS_ITER 0

STR CGEN_g1210307583(CGEN self, AM_ROUT_DEF rout_def, ARRAYSTR* arg_list, ARRAYSTR* type_list, INT* first_arg) {
 ARRAYSTR L1 = ((ARRAYSTR) NULL);/*Local for arg*/
 ARRAYSTR L2 = ((ARRAYSTR) NULL);/*Local for arg*/
 INT L3 = INT_zero;/*Local for arg*/
 STR ret_val;
 STR res = ((STR) NULL);
 INT num_args = INT_zero;
 INT num_character_ar = INT_zero;
 SIG sig1;
 INT arg_pos = INT_zero;
 dTP L41;
 INT string_length_in = INT_zero;
 INT L51 = INT_zero;
 INT L61 = INT_zero;
 INT i = INT_zero;
 AM_FORMAL_ARG arg;
 STR arg_name = ((STR) NULL);
 AM_LOCAL_EXPR le;
 dTP tp;
 dTP L71;
 ARRAYSTR create_self;
 INT create_n = INT_zero;
 ARRAYSTR ret_val1;
 ARRAYSTR create_self1;
 INT create_n1 = INT_zero;
 ARRAYSTR ret_val2;
 MANGLE genother_self;
 OB genother_ns;
 STR ret_val3;
 MANGLE space_self;
 OB space_ns;
 NAMESPACE ret_val4;
 NAMESPACE r;
 NAMESPACE create_self2;
 NAMESPACE ret_val5;
 NAMESPACE r1;
 NAMEMAP create_self3;
 NAMEMAP ret_val6;
 FSETSTR create_self4;
 FSETSTR ret_val7;
 FMAPSTRINT create_self5;
 FMAPSTRINT ret_val8;
 MANGLE genother_self1;
 OB genother_ns1;
 STR ret_val9;
 MANGLE space_self1;
 OB space_ns1;
 NAMESPACE ret_val10;
 NAMESPACE r2;
 NAMESPACE create_self6;
 NAMESPACE ret_val11;
 NAMESPACE r3;
 NAMEMAP create_self7;
 NAMEMAP ret_val12;
 FSETSTR create_self8;
 FSETSTR ret_val13;
 FMAPSTRINT create_self9;
 FMAPSTRINT ret_val14;
 MANGLE genother_self2;
 OB genother_ns2;
 STR ret_val15;
 MANGLE space_self2;
 OB space_ns2;
 NAMESPACE ret_val16;
 NAMESPACE r4;
 NAMESPACE create_self10;
 NAMESPACE ret_val17;
 NAMESPACE r5;
 NAMEMAP create_self11;
 NAMEMAP ret_val18;
 FSETSTR create_self12;
 FSETSTR ret_val19;
 FMAPSTRINT create_self13;
 FMAPSTRINT ret_val20;
 MANGLE genother_self3;
 OB genother_ns3;
 STR ret_val21;
 MANGLE space_self3;
 OB space_ns3;
 NAMESPACE ret_val22;
 NAMESPACE r6;
 NAMESPACE create_self14;
 NAMESPACE ret_val23;
 NAMESPACE r7;
 NAMEMAP create_self15;
 NAMEMAP ret_val24;
 FSETSTR create_self16;
 FSETSTR ret_val25;
 FMAPSTRINT create_self17;
 FMAPSTRINT ret_val26;
 MANGLE genother_self4;
 OB genother_ns4;
 STR ret_val27;
 MANGLE space_self4;
 OB space_ns4;
 NAMESPACE ret_val28;
 NAMESPACE r8;
 NAMESPACE create_self18;
 NAMESPACE ret_val29;
 NAMESPACE r9;
 NAMEMAP create_self19;
 NAMEMAP ret_val30;
 FSETSTR create_self20;
 FSETSTR ret_val31;
 FMAPSTRINT create_self21;
 FMAPSTRINT ret_val32;
 MANGLE genother_self5;
 OB genother_ns5;
 STR ret_val33;
 MANGLE space_self5;
 OB space_ns5;
 NAMESPACE ret_val34;
 NAMESPACE r10;
 NAMESPACE create_self22;
 NAMESPACE ret_val35;
 NAMESPACE r11;
 NAMEMAP create_self23;
 NAMEMAP ret_val36;
 FSETSTR create_self24;
 FSETSTR ret_val37;
 FMAPSTRINT create_self25;
 FMAPSTRINT ret_val38;
 AM_LOCAL_EXPR tp_self;
 dTP ret_val39;
 MANGLE genother_self6;
 OB genother_ns6;
 STR ret_val40;
 MANGLE space_self6;
 OB space_ns6;
 NAMESPACE ret_val41;
 NAMESPACE r12;
 NAMESPACE create_self26;
 NAMESPACE ret_val42;
 NAMESPACE r13;
 NAMEMAP create_self27;
 NAMEMAP ret_val43;
 FSETSTR create_self28;
 FSETSTR ret_val44;
 FMAPSTRINT create_self29;
 FMAPSTRINT ret_val45;
 MANGLE genother_self7;
 OB genother_ns7;
 STR ret_val46;
 MANGLE space_self7;
 OB space_ns7;
 NAMESPACE ret_val47;
 NAMESPACE r14;
 NAMESPACE create_self30;
 NAMESPACE ret_val48;
 NAMESPACE r15;
 NAMEMAP create_self31;
 NAMEMAP ret_val49;
 FSETSTR create_self32;
 FSETSTR ret_val50;
 FMAPSTRINT create_self33;
 FMAPSTRINT ret_val51;
 MANGLE genother_self8;
 OB genother_ns8;
 STR ret_val52;
 MANGLE space_self8;
 OB space_ns8;
 NAMESPACE ret_val53;
 NAMESPACE r16;
 NAMESPACE create_self34;
 NAMESPACE ret_val54;
 NAMESPACE r17;
 NAMEMAP create_self35;
 NAMEMAP ret_val55;
 FSETSTR create_self36;
 FSETSTR ret_val56;
 FMAPSTRINT create_self37;
 FMAPSTRINT ret_val57;
 MANGLE genother_self9;
 OB genother_ns9;
 STR ret_val58;
 MANGLE space_self9;
 OB space_ns9;
 NAMESPACE ret_val59;
 NAMESPACE r18;
 NAMESPACE create_self38;
 NAMESPACE ret_val60;
 NAMESPACE r19;
 NAMEMAP create_self39;
 NAMEMAP ret_val61;
 FSETSTR create_self40;
 FSETSTR ret_val62;
 FMAPSTRINT create_self41;
 FMAPSTRINT ret_val63;
 MANGLE genother_self10;
 OB genother_ns10;
 STR ret_val64;
 MANGLE space_self10;
 OB space_ns10;
 NAMESPACE ret_val65;
 NAMESPACE r20;
 NAMESPACE create_self42;
 NAMESPACE ret_val66;
 NAMESPACE r21;
 NAMEMAP create_self43;
 NAMEMAP ret_val67;
 FSETSTR create_self44;
 FSETSTR ret_val68;
 FMAPSTRINT create_self45;
 FMAPSTRINT ret_val69;
 CGEN mang_self;
 OB mang_ob;
 OB mang_ns;
 STR ret_val70;
 INT* L8;
 ARRAYSTR L9;
 INT L10;
 OB L11;
 INT L12;
 ARRAYSTR L13;
 INT L14;
 OB L15;
 INT L16;
 BOOL L17;
 BOOL L181_;
 dTP L19;
 NAMESPACE L20;
 OB L21;
 extern STR other359;
 STR L22;
 STR L24;
 STR L261_;
 INT L271_;
 dTP L28;
 NAMESPACE L29;
 OB L30;
 extern STR other359;
 STR L31;
 STR L33;
 STR L351_;
 INT L361_;
 dTP L37;
 NAMESPACE L38;
 OB L39;
 extern STR other359;
 STR L40;
 STR L43;
 STR L451_;
 INT L461_;
 NAMESPACE L47;
 OB L48;
 extern STR other359;
 STR L49;
 extern STR F_LENGTH1;
 INT L531_;
 dTP L54;
 NAMESPACE L55;
 OB L56;
 extern STR other359;
 STR L57;
 STR L59;
 STR L621_;
 INT L631_;
 NAMESPACE L64;
 OB L65;
 extern STR other359;
 STR L66;
 extern STR F_LENGTH1;
 INT L691_;
 INT L701_;
 ARRAYARG L72;
 INT L731_;
 INT L74;
 BOOL L751_;
 INT L77;
 INT L781_;
 INT L79;
 INT L801_;
 INT L761_,L761_m;
 AM_FORMAL_ARG L811_;
 AM_FORMAL_ARG L821_;
 AM_FORMAL_ARG L83;
 STR L84;
 dTP L86;
 NAMESPACE L87;
 OB L88;
 extern STR other359;
 STR L89;
 NAMESPACE L91;
 OB L92;
 extern STR other359;
 STR L93;
 extern STR F_LENGTH1;
 INT L961_;
 dTP L97;
 NAMESPACE L98;
 OB L99;
 extern STR other359;
 STR L100;
 NAMESPACE L102;
 OB L103;
 extern STR other359;
 STR L104;
 extern STR F_LENGTH1;
 INT L1071_;
 NAMESPACE L108;
 OB L109;
 extern STR other359;
 STR L110;
 AM_FORMAL_ARG L1121_;
 AM_FORMAL_ARG L113;
 STR L114;
 INT L1161_;
 L8 = &num_character_ar;
 num_args = CGEN_c507424193(self, rout_def, L8);
 create_self = ((ARRAYSTR) NULL);
 create_n = num_args;
 L10 = create_n;
 L12=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L10)*sizeof(STR);
 L11=ZALLOC_BIG(L12);
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L11)->header.destroyed=0;
#endif

 L9 = ((ARRAYSTR) L11);
 L9->asize = L10;
 ret_val1 = L9;
 L1 = ret_val1;
 create_self1 = ((ARRAYSTR) NULL);
 create_n1 = num_args;
 L14 = create_n1;
 L16=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L14)*sizeof(STR);
 L15=ZALLOC_BIG(L16);
 if (L15==NULL) FATAL("Unable to allocate more memory");
 ((OB)L15)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L15)->header.destroyed=0;
#endif

 L13 = ((ARRAYSTR) L15);
 L13->asize = L14;
 ret_val2 = L13;
 L2 = ret_val2;
 sig1 = ATTR(rout_def,sig1);
 arg_pos = 0;
 L17 = (ATTR(sig1,ret)==((dTP) NULL));
 L181_=!(L17); 
 if (L181_) {
  L41 = ATTR(sig1,ret);
  L19 = L41;
  if ((*dTP_is242312711[TAG(L19)])(L19, ((OB) TP_BUI633637838))) {
   genother_self = ATTR(self,mangler);
   genother_ns = ((OB) sig1);
   space_self = genother_self;
   space_ns = genother_ns;
   if ((space_ns==((OB) NULL))) {
    space_ns = ((OB) space_self);
   }
   r = FMAPdO790518483(ATTR(space_self,namespaces), space_ns);
   if ((r==((NAMESPACE) NULL))) {
    create_self2 = ((NAMESPACE) NULL);
    L21=ZALLOC(sizeof(struct NAMESPACE_struct));
    if (L21==NULL) FATAL("Unable to allocate more memory");
    ((OB)L21)->header.tag=NAMESPACE_tag;
    L20 = ((NAMESPACE) L21);
    r1 = L20;
    create_self3 = ((NAMEMAP) NULL);
    ret_val6 = ((NAMEMAP) NULL);
    SATTR(r1,map,ret_val6);
    create_self4 = ((FSETSTR) NULL);
    ret_val7 = ((FSETSTR) NULL);
    SATTR(r1,set,ret_val7);
    SATTR(r1,counter,0);
    create_self5 = ((FMAPSTRINT) NULL);
    ret_val8 = ((FMAPSTRINT) NULL);
    SATTR(r1,uniques,ret_val8);
    ret_val5 = r1;
    r = ret_val5;
    SATTR(space_self,namespaces,FMAPdO1199466663(ATTR(space_self,namespaces), space_ns, r));
   }
   ret_val4 = r;
   ret_val3 = NAMESP8552102(ret_val4, ((STR) &other359));
   L22 = ret_val3;
   SARR((ARRAYSTR)L1,arg_pos,(STR)L22); 
   ;
   L24 = CGEN_f2084214268(self, ATTR(sig1,ret));
   SARR((ARRAYSTR)L2,arg_pos,(STR)L24); 
   ;
   L261_=(STR)ARR((ARRAYSTR)L1,arg_pos); 
   res = L261_;
   L271_=INTPLUS(arg_pos,1); 
   arg_pos = L271_;
  }
  else {
   L28 = L41;
   if ((*dTP_is242312711[TAG(L28)])(L28, ((OB) TP_BUI1032249067))) {
    genother_self1 = ATTR(self,mangler);
    genother_ns1 = ((OB) sig1);
    space_self1 = genother_self1;
    space_ns1 = genother_ns1;
    if ((space_ns1==((OB) NULL))) {
     space_ns1 = ((OB) space_self1);
    }
    r2 = FMAPdO790518483(ATTR(space_self1,namespaces), space_ns1);
    if ((r2==((NAMESPACE) NULL))) {
     create_self6 = ((NAMESPACE) NULL);
     L30=ZALLOC(sizeof(struct NAMESPACE_struct));
     if (L30==NULL) FATAL("Unable to allocate more memory");
     ((OB)L30)->header.tag=NAMESPACE_tag;
     L29 = ((NAMESPACE) L30);
     r3 = L29;
     create_self7 = ((NAMEMAP) NULL);
     ret_val12 = ((NAMEMAP) NULL);
     SATTR(r3,map,ret_val12);
     create_self8 = ((FSETSTR) NULL);
     ret_val13 = ((FSETSTR) NULL);
     SATTR(r3,set,ret_val13);
     SATTR(r3,counter,0);
     create_self9 = ((FMAPSTRINT) NULL);
     ret_val14 = ((FMAPSTRINT) NULL);
     SATTR(r3,uniques,ret_val14);
     ret_val11 = r3;
     r2 = ret_val11;
     SATTR(space_self1,namespaces,FMAPdO1199466663(ATTR(space_self1,namespaces), space_ns1, r2));
    }
    ret_val10 = r2;
    ret_val9 = NAMESP8552102(ret_val10, ((STR) &other359));
    L31 = ret_val9;
    SARR((ARRAYSTR)L1,arg_pos,(STR)L31); 
    ;
    L33 = CGEN_f2084214268(self, ATTR(sig1,ret));
    SARR((ARRAYSTR)L2,arg_pos,(STR)L33); 
    ;
    L351_=(STR)ARR((ARRAYSTR)L1,arg_pos); 
    res = L351_;
    L361_=INTPLUS(arg_pos,1); 
    arg_pos = L361_;
   }
   else {
    L37 = L41;
    if ((*dTP_is242312711[TAG(L37)])(L37, ((OB) TP_BUI1805763264))) {
     genother_self2 = ATTR(self,mangler);
     genother_ns2 = ((OB) sig1);
     space_self2 = genother_self2;
     space_ns2 = genother_ns2;
     if ((space_ns2==((OB) NULL))) {
      space_ns2 = ((OB) space_self2);
     }
     r4 = FMAPdO790518483(ATTR(space_self2,namespaces), space_ns2);
     if ((r4==((NAMESPACE) NULL))) {
      create_self10 = ((NAMESPACE) NULL);
      L39=ZALLOC(sizeof(struct NAMESPACE_struct));
      if (L39==NULL) FATAL("Unable to allocate more memory");
      ((OB)L39)->header.tag=NAMESPACE_tag;
      L38 = ((NAMESPACE) L39);
      r5 = L38;
      create_self11 = ((NAMEMAP) NULL);
      ret_val18 = ((NAMEMAP) NULL);
      SATTR(r5,map,ret_val18);
      create_self12 = ((FSETSTR) NULL);
      ret_val19 = ((FSETSTR) NULL);
      SATTR(r5,set,ret_val19);
      SATTR(r5,counter,0);
      create_self13 = ((FMAPSTRINT) NULL);
      ret_val20 = ((FMAPSTRINT) NULL);
      SATTR(r5,uniques,ret_val20);
      ret_val17 = r5;
      r4 = ret_val17;
      SATTR(space_self2,namespaces,FMAPdO1199466663(ATTR(space_self2,namespaces), space_ns2, r4));
     }
     ret_val16 = r4;
     ret_val15 = NAMESP8552102(ret_val16, ((STR) &other359));
     L40 = ret_val15;
     SARR((ARRAYSTR)L1,arg_pos,(STR)L40); 
     ;
     L43 = CGEN_f2084214268(self, ATTR(sig1,ret));
     SARR((ARRAYSTR)L2,arg_pos,(STR)L43); 
     ;
     L451_=(STR)ARR((ARRAYSTR)L1,arg_pos); 
     res = L451_;
     L461_=INTPLUS(arg_pos,1); 
     arg_pos = L461_;
     genother_self3 = ATTR(self,mangler);
     genother_ns3 = ((OB) sig1);
     space_self3 = genother_self3;
     space_ns3 = genother_ns3;
     if ((space_ns3==((OB) NULL))) {
      space_ns3 = ((OB) space_self3);
     }
     r6 = FMAPdO790518483(ATTR(space_self3,namespaces), space_ns3);
     if ((r6==((NAMESPACE) NULL))) {
      create_self14 = ((NAMESPACE) NULL);
      L48=ZALLOC(sizeof(struct NAMESPACE_struct));
      if (L48==NULL) FATAL("Unable to allocate more memory");
      ((OB)L48)->header.tag=NAMESPACE_tag;
      L47 = ((NAMESPACE) L48);
      r7 = L47;
      create_self15 = ((NAMEMAP) NULL);
      ret_val24 = ((NAMEMAP) NULL);
      SATTR(r7,map,ret_val24);
      create_self16 = ((FSETSTR) NULL);
      ret_val25 = ((FSETSTR) NULL);
      SATTR(r7,set,ret_val25);
      SATTR(r7,counter,0);
      create_self17 = ((FMAPSTRINT) NULL);
      ret_val26 = ((FMAPSTRINT) NULL);
      SATTR(r7,uniques,ret_val26);
      ret_val23 = r7;
      r6 = ret_val23;
      SATTR(space_self3,namespaces,FMAPdO1199466663(ATTR(space_self3,namespaces), space_ns3, r6));
     }
     ret_val22 = r6;
     ret_val21 = NAMESP8552102(ret_val22, ((STR) &other359));
     L49 = ret_val21;
     SARR((ARRAYSTR)L1,arg_pos,(STR)L49); 
     ;
     SARR((ARRAYSTR)L2,arg_pos,(STR)((STR) &F_LENGTH1)); 
     ;
     L531_=INTPLUS(arg_pos,1); 
     arg_pos = L531_;
    }
    else {
     L54 = L41;
     if ((*dTP_is242312711[TAG(L54)])(L54, ((OB) TP_BUI2088674906))) {
      genother_self4 = ATTR(self,mangler);
      genother_ns4 = ((OB) sig1);
      space_self4 = genother_self4;
      space_ns4 = genother_ns4;
      if ((space_ns4==((OB) NULL))) {
       space_ns4 = ((OB) space_self4);
      }
      r8 = FMAPdO790518483(ATTR(space_self4,namespaces), space_ns4);
      if ((r8==((NAMESPACE) NULL))) {
       create_self18 = ((NAMESPACE) NULL);
       L56=ZALLOC(sizeof(struct NAMESPACE_struct));
       if (L56==NULL) FATAL("Unable to allocate more memory");
       ((OB)L56)->header.tag=NAMESPACE_tag;
       L55 = ((NAMESPACE) L56);
       r9 = L55;
       create_self19 = ((NAMEMAP) NULL);
       ret_val30 = ((NAMEMAP) NULL);
       SATTR(r9,map,ret_val30);
       create_self20 = ((FSETSTR) NULL);
       ret_val31 = ((FSETSTR) NULL);
       SATTR(r9,set,ret_val31);
       SATTR(r9,counter,0);
       create_self21 = ((FMAPSTRINT) NULL);
       ret_val32 = ((FMAPSTRINT) NULL);
       SATTR(r9,uniques,ret_val32);
       ret_val29 = r9;
       r8 = ret_val29;
       SATTR(space_self4,namespaces,FMAPdO1199466663(ATTR(space_self4,namespaces), space_ns4, r8));
      }
      ret_val28 = r8;
      ret_val27 = NAMESP8552102(ret_val28, ((STR) &other359));
      L57 = ret_val27;
      SARR((ARRAYSTR)L1,arg_pos,(STR)L57); 
      ;
      L59 = CGEN_f2084214268(self, ATTR(sig1,ret));
      SARR((ARRAYSTR)L2,arg_pos,(STR)L59); 
      ;
      L621_=(STR)ARR((ARRAYSTR)L1,arg_pos); 
      res = L621_;
      L631_=INTPLUS(arg_pos,1); 
      arg_pos = L631_;
      genother_self5 = ATTR(self,mangler);
      genother_ns5 = ((OB) sig1);
      space_self5 = genother_self5;
      space_ns5 = genother_ns5;
      if ((space_ns5==((OB) NULL))) {
       space_ns5 = ((OB) space_self5);
      }
      r10 = FMAPdO790518483(ATTR(space_self5,namespaces), space_ns5);
      if ((r10==((NAMESPACE) NULL))) {
       create_self22 = ((NAMESPACE) NULL);
       L65=ZALLOC(sizeof(struct NAMESPACE_struct));
       if (L65==NULL) FATAL("Unable to allocate more memory");
       ((OB)L65)->header.tag=NAMESPACE_tag;
       L64 = ((NAMESPACE) L65);
       r11 = L64;
       create_self23 = ((NAMEMAP) NULL);
       ret_val36 = ((NAMEMAP) NULL);
       SATTR(r11,map,ret_val36);
       create_self24 = ((FSETSTR) NULL);
       ret_val37 = ((FSETSTR) NULL);
       SATTR(r11,set,ret_val37);
       SATTR(r11,counter,0);
       create_self25 = ((FMAPSTRINT) NULL);
       ret_val38 = ((FMAPSTRINT) NULL);
       SATTR(r11,uniques,ret_val38);
       ret_val35 = r11;
       r10 = ret_val35;
       SATTR(space_self5,namespaces,FMAPdO1199466663(ATTR(space_self5,namespaces), space_ns5, r10));
      }
      ret_val34 = r10;
      ret_val33 = NAMESP8552102(ret_val34, ((STR) &other359));
      L66 = ret_val33;
      SARR((ARRAYSTR)L1,arg_pos,(STR)L66); 
      ;
      SARR((ARRAYSTR)L2,arg_pos,(STR)((STR) &F_LENGTH1)); 
      ;
      L691_=INTPLUS(arg_pos,1); 
      arg_pos = L691_;
     }
    }
   }
  }
 }
 L3 = arg_pos;
 L701_=INTMINUS(num_args,num_character_ar); 
 string_length_in = L701_;
 L72 = ATTR(sig1,args);
 L731_=(L72)==NULL?0:ASIZE((ARRAYARG)L72); 
 L74 = L731_;
 L751_=(0)<(L74); 
 if (L751_) {
  {
   BOOL f_L761_ = TRUE;
   while (1) {
    if (f_L761_) {
     f_L761_ = FALSE;
     L51 = 1;
     L781_=(rout_def)==NULL?0:ASIZE((AM_ROUT_DEF)rout_def); 
     L79 = L781_;
     L801_=INTMINUS(L79,1); 
     L61 = L801_;
     L761_=L51-1;L761_m=L61; 
    }
    if(L761_++>=L761_m)  goto after_loop; 
    i = L761_;
    L811_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)rout_def,i); 
    arg = L811_;
    le = ATTR(arg,expr);
    arg_name = ATTR(le,name1).str;
    L821_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)rout_def,i); 
    L83=L821_;
    tp_self = ATTR(L83,expr);
    ret_val39 = ATTR(tp_self,tp_at);
    tp = ret_val39;
    L84 = CGEN_f2084214268(self, tp);
    SARR((ARRAYSTR)L2,arg_pos,(STR)L84); 
    ;
    L71 = tp;
    L86 = L71;
    if ((*dTP_is242312711[TAG(L86)])(L86, ((OB) TP_BUI1805763264))) {
     genother_self6 = ATTR(self,mangler);
     genother_ns6 = ((OB) sig1);
     space_self6 = genother_self6;
     space_ns6 = genother_ns6;
     if ((space_ns6==((OB) NULL))) {
      space_ns6 = ((OB) space_self6);
     }
     r12 = FMAPdO790518483(ATTR(space_self6,namespaces), space_ns6);
     if ((r12==((NAMESPACE) NULL))) {
      create_self26 = ((NAMESPACE) NULL);
      L88=ZALLOC(sizeof(struct NAMESPACE_struct));
      if (L88==NULL) FATAL("Unable to allocate more memory");
      ((OB)L88)->header.tag=NAMESPACE_tag;
      L87 = ((NAMESPACE) L88);
      r13 = L87;
      create_self27 = ((NAMEMAP) NULL);
      ret_val43 = ((NAMEMAP) NULL);
      SATTR(r13,map,ret_val43);
      create_self28 = ((FSETSTR) NULL);
      ret_val44 = ((FSETSTR) NULL);
      SATTR(r13,set,ret_val44);
      SATTR(r13,counter,0);
      create_self29 = ((FMAPSTRINT) NULL);
      ret_val45 = ((FMAPSTRINT) NULL);
      SATTR(r13,uniques,ret_val45);
      ret_val42 = r13;
      r12 = ret_val42;
      SATTR(space_self6,namespaces,FMAPdO1199466663(ATTR(space_self6,namespaces), space_ns6, r12));
     }
     ret_val41 = r12;
     ret_val40 = NAMESP8552102(ret_val41, ((STR) &other359));
     L89 = ret_val40;
     SARR((ARRAYSTR)L1,arg_pos,(STR)L89); 
     ;
     genother_self7 = ATTR(self,mangler);
     genother_ns7 = ((OB) sig1);
     space_self7 = genother_self7;
     space_ns7 = genother_ns7;
     if ((space_ns7==((OB) NULL))) {
      space_ns7 = ((OB) space_self7);
     }
     r14 = FMAPdO790518483(ATTR(space_self7,namespaces), space_ns7);
     if ((r14==((NAMESPACE) NULL))) {
      create_self30 = ((NAMESPACE) NULL);
      L92=ZALLOC(sizeof(struct NAMESPACE_struct));
      if (L92==NULL) FATAL("Unable to allocate more memory");
      ((OB)L92)->header.tag=NAMESPACE_tag;
      L91 = ((NAMESPACE) L92);
      r15 = L91;
      create_self31 = ((NAMEMAP) NULL);
      ret_val49 = ((NAMEMAP) NULL);
      SATTR(r15,map,ret_val49);
      create_self32 = ((FSETSTR) NULL);
      ret_val50 = ((FSETSTR) NULL);
      SATTR(r15,set,ret_val50);
      SATTR(r15,counter,0);
      create_self33 = ((FMAPSTRINT) NULL);
      ret_val51 = ((FMAPSTRINT) NULL);
      SATTR(r15,uniques,ret_val51);
      ret_val48 = r15;
      r14 = ret_val48;
      SATTR(space_self7,namespaces,FMAPdO1199466663(ATTR(space_self7,namespaces), space_ns7, r14));
     }
     ret_val47 = r14;
     ret_val46 = NAMESP8552102(ret_val47, ((STR) &other359));
     L93 = ret_val46;
     SARR((ARRAYSTR)L1,string_length_in,(STR)L93); 
     ;
     SARR((ARRAYSTR)L2,string_length_in,(STR)((STR) &F_LENGTH1)); 
     ;
     L961_=INTPLUS(string_length_in,1); 
     string_length_in = L961_;
    }
    else {
     L97 = L71;
     if ((*dTP_is242312711[TAG(L97)])(L97, ((OB) TP_BUI2088674906))) {
      genother_self8 = ATTR(self,mangler);
      genother_ns8 = ((OB) sig1);
      space_self8 = genother_self8;
      space_ns8 = genother_ns8;
      if ((space_ns8==((OB) NULL))) {
       space_ns8 = ((OB) space_self8);
      }
      r16 = FMAPdO790518483(ATTR(space_self8,namespaces), space_ns8);
      if ((r16==((NAMESPACE) NULL))) {
       create_self34 = ((NAMESPACE) NULL);
       L99=ZALLOC(sizeof(struct NAMESPACE_struct));
       if (L99==NULL) FATAL("Unable to allocate more memory");
       ((OB)L99)->header.tag=NAMESPACE_tag;
       L98 = ((NAMESPACE) L99);
       r17 = L98;
       create_self35 = ((NAMEMAP) NULL);
       ret_val55 = ((NAMEMAP) NULL);
       SATTR(r17,map,ret_val55);
       create_self36 = ((FSETSTR) NULL);
       ret_val56 = ((FSETSTR) NULL);
       SATTR(r17,set,ret_val56);
       SATTR(r17,counter,0);
       create_self37 = ((FMAPSTRINT) NULL);
       ret_val57 = ((FMAPSTRINT) NULL);
       SATTR(r17,uniques,ret_val57);
       ret_val54 = r17;
       r16 = ret_val54;
       SATTR(space_self8,namespaces,FMAPdO1199466663(ATTR(space_self8,namespaces), space_ns8, r16));
      }
      ret_val53 = r16;
      ret_val52 = NAMESP8552102(ret_val53, ((STR) &other359));
      L100 = ret_val52;
      SARR((ARRAYSTR)L1,arg_pos,(STR)L100); 
      ;
      genother_self9 = ATTR(self,mangler);
      genother_ns9 = ((OB) sig1);
      space_self9 = genother_self9;
      space_ns9 = genother_ns9;
      if ((space_ns9==((OB) NULL))) {
       space_ns9 = ((OB) space_self9);
      }
      r18 = FMAPdO790518483(ATTR(space_self9,namespaces), space_ns9);
      if ((r18==((NAMESPACE) NULL))) {
       create_self38 = ((NAMESPACE) NULL);
       L103=ZALLOC(sizeof(struct NAMESPACE_struct));
       if (L103==NULL) FATAL("Unable to allocate more memory");
       ((OB)L103)->header.tag=NAMESPACE_tag;
       L102 = ((NAMESPACE) L103);
       r19 = L102;
       create_self39 = ((NAMEMAP) NULL);
       ret_val61 = ((NAMEMAP) NULL);
       SATTR(r19,map,ret_val61);
       create_self40 = ((FSETSTR) NULL);
       ret_val62 = ((FSETSTR) NULL);
       SATTR(r19,set,ret_val62);
       SATTR(r19,counter,0);
       create_self41 = ((FMAPSTRINT) NULL);
       ret_val63 = ((FMAPSTRINT) NULL);
       SATTR(r19,uniques,ret_val63);
       ret_val60 = r19;
       r18 = ret_val60;
       SATTR(space_self9,namespaces,FMAPdO1199466663(ATTR(space_self9,namespaces), space_ns9, r18));
      }
      ret_val59 = r18;
      ret_val58 = NAMESP8552102(ret_val59, ((STR) &other359));
      L104 = ret_val58;
      SARR((ARRAYSTR)L1,string_length_in,(STR)L104); 
      ;
      SARR((ARRAYSTR)L2,string_length_in,(STR)((STR) &F_LENGTH1)); 
      ;
      L1071_=INTPLUS(string_length_in,1); 
      string_length_in = L1071_;
     }
     else {
      if (CGEN_i1538413090(self, tp)) {
       genother_self10 = ATTR(self,mangler);
       genother_ns10 = ((OB) sig1);
       space_self10 = genother_self10;
       space_ns10 = genother_ns10;
       if ((space_ns10==((OB) NULL))) {
        space_ns10 = ((OB) space_self10);
       }
       r20 = FMAPdO790518483(ATTR(space_self10,namespaces), space_ns10);
       if ((r20==((NAMESPACE) NULL))) {
        create_self42 = ((NAMESPACE) NULL);
        L109=ZALLOC(sizeof(struct NAMESPACE_struct));
        if (L109==NULL) FATAL("Unable to allocate more memory");
        ((OB)L109)->header.tag=NAMESPACE_tag;
        L108 = ((NAMESPACE) L109);
        r21 = L108;
        create_self43 = ((NAMEMAP) NULL);
        ret_val67 = ((NAMEMAP) NULL);
        SATTR(r21,map,ret_val67);
        create_self44 = ((FSETSTR) NULL);
        ret_val68 = ((FSETSTR) NULL);
        SATTR(r21,set,ret_val68);
        SATTR(r21,counter,0);
        create_self45 = ((FMAPSTRINT) NULL);
        ret_val69 = ((FMAPSTRINT) NULL);
        SATTR(r21,uniques,ret_val69);
        ret_val66 = r21;
        r20 = ret_val66;
        SATTR(space_self10,namespaces,FMAPdO1199466663(ATTR(space_self10,namespaces), space_ns10, r20));
       }
       ret_val65 = r20;
       ret_val64 = NAMESP8552102(ret_val65, ((STR) &other359));
       L110 = ret_val64;
       SARR((ARRAYSTR)L1,arg_pos,(STR)L110); 
       ;
      }
      else {
       mang_self = self;
       L1121_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)rout_def,i); 
       L113=L1121_;
       mang_ob = ((OB) ATTR(L113,expr));
       mang_ns = ((OB) sig1);
       ret_val70 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, mang_ns);
       L114 = ret_val70;
       SARR((ARRAYSTR)L1,arg_pos,(STR)L114); 
       ;
      }
     }
    }
    L1161_=INTPLUS(arg_pos,1); 
    arg_pos = L1161_;
   }
  }
  after_loop: ;
 }
 ret_val = res;
 *arg_list = L1;
 *type_list = L2;
 *first_arg = L3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_s51495157(CGEN self, dTP tp) {
 STR ret_val;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val3;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val4;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val5;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val6;
 dTP L1;
 extern STR sizeof1;
 extern STR name26;
 extern STR sizeofstruct;
 extern STR S_struct;
 L1 = tp;
 if ((*dTP_is1334578382[TAG(L1)])(L1)) {
  plus_self = ((STR) &sizeof1);
  mang_self = self;
  mang_ob = ((OB) tp);
  ret_val1 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
  plus_sarg = ret_val1;
  ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val2;
  plus_sarg1 = ((STR) &name26);
  ret_val3 = STR_ap2004550586(plus_self1, plus_sarg1);
  ret_val = ret_val3;
  return ret_val;
 }
 else {
  plus_self2 = ((STR) &sizeofstruct);
  mang_self1 = self;
  mang_ob1 = ((OB) tp);
  ret_val4 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
  plus_sarg2 = ret_val4;
  ret_val5 = STR_ap2004550586(plus_self2, plus_sarg2);
  plus_self3 = ret_val5;
  plus_sarg3 = ((STR) &S_struct);
  ret_val6 = STR_ap2004550586(plus_self3, plus_sarg3);
  ret_val = ret_val6;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_a1893106143(CGEN self, SFILE_ID s) {
 INT lineno = INT_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 SFILE_ID file_in_self = SFILE_ID_zero;
 STR ret_val;
 FLISTSTR aget_self;
 INT aget_ind = INT_zero;
 STR ret_val1;
 FSTR plus_self;
 INT plus_i = INT_zero;
 FSTR ret_val2;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val3;
 SFILE_ID file_in_self1 = SFILE_ID_zero;
 STR ret_val4;
 FLISTSTR aget_self1;
 INT aget_ind1 = INT_zero;
 STR ret_val5;
 FSTR plus_self1;
 CHAR plus_c = CHAR_zero;
 FSTR ret_val6;
 SFILE_ID file_in_self2 = SFILE_ID_zero;
 STR ret_val7;
 FLISTSTR aget_self2;
 INT aget_ind2 = INT_zero;
 STR ret_val8;
 FSTR plus_self2;
 INT plus_i1 = INT_zero;
 FSTR ret_val9;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val10;
 SFILE_ID file_in_self3 = SFILE_ID_zero;
 STR ret_val11;
 FLISTSTR aget_self3;
 INT aget_ind3 = INT_zero;
 STR ret_val12;
 FSTR plus_self3;
 INT plus_i2 = INT_zero;
 FSTR ret_val13;
 INT str_in_self2 = INT_zero;
 FSTR str_in_s2;
 FSTR ret_val14;
 SFILE_ID file_in_self4 = SFILE_ID_zero;
 STR ret_val15;
 FLISTSTR aget_self4;
 INT aget_ind4 = INT_zero;
 STR ret_val16;
 SFILE_ID file_in_self5 = SFILE_ID_zero;
 STR ret_val17;
 FLISTSTR aget_self5;
 INT aget_ind5 = INT_zero;
 STR ret_val18;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 INT L4;
 INT L51_;
 INT L6;
 BOOL L71_;
 STR L81_;
 extern STR line1;
 extern STR name10;
 FSTR L9;
 extern STR name71;
 FSTR L10;
 STR L111_;
 FSTR L12;
 extern STR name72;
 BOOL L13;
 INT L14;
 BOOL L151_;
 STR L161_;
 extern STR line2;
 extern STR name10;
 FSTR L17;
 extern STR name71;
 FSTR L18;
 STR L191_;
 FSTR L20;
 extern STR name73;
 extern STR line1;
 extern STR name10;
 FSTR L22;
 extern STR name71;
 FSTR L23;
 STR L241_;
 FSTR L25;
 extern STR name72;
 STR L261_;
 if (ATTR(self,debug1)) {
  L1 = TRUE;
 } else {
  L1 = ATTR(self,pretty);
 }
 if (L1) {
  lineno = SFILE_2070755978(s);
  L21_=(0)<(lineno); 
  if (L21_) {
   set_eloc_self = ATTR(self,prog);
   set_eloc_l = s;
   SATTR(set_eloc_self,eloc,set_eloc_l);
   L4 = ATTR(self,last_lineno);
   L51_=INTPLUS(L4,1); 
   L6 = L51_;
   L71_=(lineno)==(L6); 
   if (L71_) {
    file_in_self = s;
    aget_self = SFILE_ID_files;
    aget_ind = SFILE_1102877860(file_in_self);
    L81_=(STR)ARR((FLISTSTR)aget_self,aget_ind); 
    ret_val1 = L81_;
    ret_val = ret_val1;
    L3 = STR_is111530248(ATTR(self,last_file), ret_val);
   } else {
    L3 = FALSE;
   }
   if (L3) {
    if (ATTR(self,pretty)) {
     plus_self = FSTR_p1752847026(ATTR(self,routine_code), ((STR) &line1));
     plus_i = lineno;
     str_in_self = plus_i;
     str_in_s = plus_self;
     ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
     ret_val2 = ret_val3;
     L9 = FSTR_p1752847026(ret_val2, ((STR) &name10));
     L10 = FSTR_p1752847026(L9, ((STR) &name71));
     file_in_self1 = s;
     aget_self1 = SFILE_ID_files;
     aget_ind1 = SFILE_1102877860(file_in_self1);
     L111_=(STR)ARR((FLISTSTR)aget_self1,aget_ind1); 
     ret_val5 = L111_;
     ret_val4 = ret_val5;
     L12 = FSTR_p1752847026(L10, ret_val4);
     SATTR(self,routine_code,FSTR_p1752847026(L12, ((STR) &name72)));
    }
    else {
     plus_self1 = ATTR(self,routine_code);
     plus_c = '\n';
     ret_val6 = FSTR_p399773021(plus_self1, plus_c);
     SATTR(self,routine_code,ret_val6);
    }
   }
   else {
    L14 = ATTR(self,last_lineno);
    L151_=(lineno)==(L14); 
    if (L151_) {
     file_in_self2 = s;
     aget_self2 = SFILE_ID_files;
     aget_ind2 = SFILE_1102877860(file_in_self2);
     L161_=(STR)ARR((FLISTSTR)aget_self2,aget_ind2); 
     ret_val8 = L161_;
     ret_val7 = ret_val8;
     L13 = STR_is111530248(ATTR(self,last_file), ret_val7);
    } else {
     L13 = FALSE;
    }
    if (L13) {
    }
    else {
     if (ATTR(self,debug1)) {
      plus_self2 = FSTR_p1752847026(ATTR(self,routine_code), ((STR) &line2));
      plus_i1 = lineno;
      str_in_self1 = plus_i1;
      str_in_s1 = plus_self2;
      ret_val10 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
      ret_val9 = ret_val10;
      L17 = FSTR_p1752847026(ret_val9, ((STR) &name10));
      L18 = FSTR_p1752847026(L17, ((STR) &name71));
      file_in_self3 = s;
      aget_self3 = SFILE_ID_files;
      aget_ind3 = SFILE_1102877860(file_in_self3);
      L191_=(STR)ARR((FLISTSTR)aget_self3,aget_ind3); 
      ret_val12 = L191_;
      ret_val11 = ret_val12;
      L20 = FSTR_p1752847026(L18, ret_val11);
      SATTR(self,routine_code,FSTR_p1752847026(L20, ((STR) &name73)));
     }
     else {
      plus_self3 = FSTR_p1752847026(ATTR(self,routine_code), ((STR) &line1));
      plus_i2 = lineno;
      str_in_self2 = plus_i2;
      str_in_s2 = plus_self3;
      ret_val14 = INT_st367594495(str_in_self2, str_in_s2, 0, 10, ' ');
      ret_val13 = ret_val14;
      L22 = FSTR_p1752847026(ret_val13, ((STR) &name10));
      L23 = FSTR_p1752847026(L22, ((STR) &name71));
      file_in_self4 = s;
      aget_self4 = SFILE_ID_files;
      aget_ind4 = SFILE_1102877860(file_in_self4);
      L241_=(STR)ARR((FLISTSTR)aget_self4,aget_ind4); 
      ret_val16 = L241_;
      ret_val15 = ret_val16;
      L25 = FSTR_p1752847026(L23, ret_val15);
      SATTR(self,routine_code,FSTR_p1752847026(L25, ((STR) &name72)));
     }
    }
   }
   SATTR(self,last_lineno,lineno);
   file_in_self5 = s;
   aget_self5 = SFILE_ID_files;
   aget_ind5 = SFILE_1102877860(file_in_self5);
   L261_=(STR)ARR((FLISTSTR)aget_self5,aget_ind5); 
   ret_val18 = L261_;
   ret_val17 = ret_val18;
   SATTR(self,last_file,ret_val17);
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_c1869222825(CGEN self, AM_ROUT_DEF func) {
 AM_ITE1809135850 it;
 AM_LOOP_STMT lp;
 AM_ITE1809135850 L11;
 INT i = INT_zero;
 AM_YIELD_STMT yl;
 AM_EXPR_STMT st;
 AM_ITE1809135850 create_self;
 INT create_nargs = INT_zero;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_ITE1809135850 ret_val;
 AM_ITE1809135850 r;
 AM_LOOP_STMT create_self1;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_LOOP_STMT ret_val1;
 AM_LOOP_STMT r1;
 INT L21 = INT_zero;
 AM_CALL_ARG create_self2;
 dAM_EXPR create_e;
 dMODE create_m;
 AM_CALL_ARG ret_val2;
 AM_CALL_ARG res;
 AM_YIELD_STMT create_self3;
 SFILE_ID create_source2 = SFILE_ID_zero;
 AM_YIELD_STMT ret_val3;
 AM_YIELD_STMT r2;
 SIG has_ret_self;
 BOOL ret_val4 = BOOL_zero;
 AM_EXPR_STMT create_self4;
 SFILE_ID create_source3 = SFILE_ID_zero;
 AM_EXPR_STMT ret_val5;
 AM_EXPR_STMT r3;
 INT L31_;
 AM_ITE1809135850 L4;
 INT L5;
 OB L6;
 INT L7;
 AM_LOOP_STMT L8;
 OB L9;
 extern STR I_u_I;
 INT L101_br;
 INT rL101_;
 INT L12;
 AM_FORMAL_ARG L131_;
 AM_FORMAL_ARG L14;
 AM_FORMAL_ARG L151_;
 AM_FORMAL_ARG L16;
 AM_CALL_ARG L17;
 OB L18;
 AM_CALL_ARG L19;
 INT L221_;
 AM_YIELD_STMT L23;
 OB L24;
 BOOL L25;
 BOOL L261_;
 AM_EXPR_STMT L27;
 OB L28;
 SATTR(func,num_yields,1);
 create_self = ((AM_ITE1809135850) NULL);
 L31_=(func)==NULL?0:ASIZE((AM_ROUT_DEF)func); 
 create_nargs = L31_;
 create_source = ATTR(func,source1);
 L5 = create_nargs;
 L7=(sizeof(struct AM_ITE1809135850_struct)-sizeof(AM_CALL_ARG))+(L5)*sizeof(AM_CALL_ARG);
 L6=ZALLOC_BIG(L7);
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=AM_ITE1809135850_tag;
#ifdef DESTROY_CHK

   ((OB)L6)->header.destroyed=0;
#endif

 L4 = ((AM_ITE1809135850) L6);
 L4->asize = L5;
 r = L4;
 SATTR(r,source1,create_source);
 ret_val = r;
 it = ret_val;
 SATTR(it,fun,ATTR(func,sig1));
 create_self1 = ((AM_LOOP_STMT) NULL);
 create_source1 = ATTR(func,source1);
 L9=ZALLOC(sizeof(struct AM_LOOP_STMT_struct));
 if (L9==NULL) FATAL("Unable to allocate more memory");
 ((OB)L9)->header.tag=AM_LOOP_STMT_tag;
 L8 = ((AM_LOOP_STMT) L9);
 r1 = L8;
 SATTR(r1,source1,create_source1);
 ret_val1 = r1;
 lp = ret_val1;
 SATTR(func,calls1,FLISTd805769956(ATTR(func,calls1), ((dAM_EXPR) it)));
 SATTR(lp,its1,FLISTA943034044(ATTR(lp,its1), it));
 SATTR(lp,has_yield,TRUE);
 SATTR(it,lp,lp);
 SATTR(it,init_before_loop,TRUE);
 SATTR(it,uniq,((STR) &I_u_I));
 {
  BOOL f_L101_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = it;
  L101_br=L11==NULL?0:ASIZE((AM_ITE1809135850)L11); 
  while (1) {
   if(L21>=L101_br)  goto after_loop; 
   rL101_=L21; 
   i = rL101_;
   create_self2 = ((AM_CALL_ARG) NULL);
   L131_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)func,i); 
   L14=L131_;
   create_e = ((dAM_EXPR) ATTR(L14,expr));
   L151_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)func,i); 
   L16=L151_;
   create_m = ATTR(L16,mode1);
   L18=ZALLOC(sizeof(struct AM_CALL_ARG_struct));
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=AM_CALL_ARG_tag;
   L17 = ((AM_CALL_ARG) L18);
   res = L17;
   SATTR(res,expr,create_e);
   SATTR(res,mode1,create_m);
   ret_val2 = res;
   L19 = ret_val2;
   SARR((AM_ITE1809135850)it,i,(AM_CALL_ARG)L19); 
   ;
   L221_=INTPLUS(L21,1); 
   L21 = L221_;
  }
 }
 after_loop: ;
 create_self3 = ((AM_YIELD_STMT) NULL);
 create_source2 = ATTR(func,source1);
 L24=ZALLOC(sizeof(struct AM_YIELD_STMT_struct));
 if (L24==NULL) FATAL("Unable to allocate more memory");
 ((OB)L24)->header.tag=AM_YIELD_STMT_tag;
 L23 = ((AM_YIELD_STMT) L24);
 r2 = L23;
 SATTR(r2,source1,create_source2);
 ret_val3 = r2;
 yl = ret_val3;
 SATTR(yl,ret,1);
 has_ret_self = ATTR(func,sig1);
 L25 = (ATTR(has_ret_self,ret)==((dTP) NULL));
 L261_=!(L25); 
 ret_val4 = L261_;
 if (ret_val4) {
  SATTR(yl,val1,((dAM_EXPR) it));
  SATTR(lp,body,((dAM_STMT) yl));
 }
 else {
  create_self4 = ((AM_EXPR_STMT) NULL);
  create_source3 = ATTR(func,source1);
  L28=ZALLOC(sizeof(struct AM_EXPR_STMT_struct));
  if (L28==NULL) FATAL("Unable to allocate more memory");
  ((OB)L28)->header.tag=AM_EXPR_STMT_tag;
  L27 = ((AM_EXPR_STMT) L28);
  r3 = L27;
  SATTR(r3,source1,create_source3);
  ret_val5 = r3;
  st = ret_val5;
  SATTR(st,expr,((dAM_EXPR) it));
  SATTR(st,next,((dAM_STMT) yl));
  SATTR(lp,body,((dAM_STMT) st));
 }
 SATTR(func,code1,((dAM_STMT) lp));
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_d695831906(CGEN self) {
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 FSTR plus_self;
 CHAR plus_c = CHAR_zero;
 FSTR ret_val;
 FSTR L3;
 INT L51_;
 L3 = ATTR(self,routine_code);
 SATTR(self,routine_code,FSTR_p1752847026(L3, CGEN_eolrSTR(self)));
 {
  BOOL f_L41_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = ATTR(self,indent);
  while (1) {
   if(L21>=L11)  goto after_loop; 
   ;
   plus_self = ATTR(self,routine_code);
   plus_c = ' ';
   ret_val = FSTR_p399773021(plus_self, plus_c);
   SATTR(self,routine_code,ret_val);
   L51_=INTPLUS(L21,1); 
   L21 = L51_;
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e233957487(CGEN self, AM_ROUT_DEF f) {
 STR res_arg = ((STR) NULL);
 ARRAYSTR arg_list = ((ARRAYSTR) NULL);
 ARRAYSTR final_arg_list = ((ARRAYSTR) NULL);
 ARRAYSTR type_list = ((ARRAYSTR) NULL);
 INT num_args = INT_zero;
 INT num_character_ar = INT_zero;
 BOOL has_return = BOOL_zero;
 INT first_arg = INT_zero;
 SIG sig1;
 STR rout_index = ((STR) NULL);
 STR name111;
 BOOL out_args = BOOL_zero;
 ARRAYSTR L11;
 STR arg_str;
 ARRAYSTR L21;
 STR type_str;
 STR L31;
 INT arg_pos = INT_zero;
 INT string_length_po = INT_zero;
 INT L41 = INT_zero;
 INT L51 = INT_zero;
 INT i = INT_zero;
 dTP L61;
 STR local11;
 STR local2;
 FLISTSTR ll;
 INT cn = INT_zero;
 AM_ROUT_DEF L71;
 STR k;
 INT L81 = INT_zero;
 FLISTA725283029 L91;
 AM_LOCAL_EXPR l;
 INT L101 = INT_zero;
 INT L121 = INT_zero;
 INT i1 = INT_zero;
 FLISTSTR L131;
 AM_LOCAL_EXPR lv;
 FLISTA725283029 L141;
 STR def;
 CODE_FILE plus_self;
 dSTR plus_s;
 STR plus_self1;
 STR plus_sarg;
 STR ret_val;
 STR plus_self2;
 STR plus_sarg1;
 STR ret_val1;
 CGEN comment_self;
 CODE_FILE comment_f;
 STR comment_com;
 CODE_FILE plus_self3;
 dSTR plus_s1;
 CODE_FILE ret_val2;
 CODE_FILE plus_self4;
 dSTR plus_s2;
 CODE_FILE ret_val3;
 CODE_FILE plus_self5;
 dSTR plus_s3;
 CODE_FILE plus_self6;
 dSTR plus_s4;
 PRINT_OB register_self;
 SIG register_s;
 STR ret_val4;
 STR plus_self7;
 INT plus_arg = INT_zero;
 STR ret_val5;
 STR s;
 INT str_self = INT_zero;
 STR ret_val6;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val7;
 FSTR str_self1;
 STR ret_val8;
 CODE_FILE plus_self8;
 dSTR plus_s5;
 CODE_FILE ret_val9;
 CODE_FILE plus_self9;
 dSTR plus_s6;
 CODE_FILE ret_val10;
 CODE_FILE plus_self10;
 dSTR plus_s7;
 CODE_FILE ret_val11;
 SFILE_ID file_in_self = SFILE_ID_zero;
 STR ret_val12;
 FLISTSTR aget_self;
 INT aget_ind = INT_zero;
 STR ret_val13;
 CODE_FILE plus_self11;
 dSTR plus_s8;
 CODE_FILE ret_val14;
 CODE_FILE plus_self12;
 dSTR plus_s9;
 CODE_FILE plus_self13;
 dSTR plus_s10;
 CODE_FILE ret_val15;
 CODE_FILE plus_self14;
 dSTR plus_s11;
 CODE_FILE ret_val16;
 CODE_FILE plus_self15;
 dSTR plus_s12;
 CODE_FILE ret_val17;
 SFILE_ID file_in_self1 = SFILE_ID_zero;
 STR ret_val18;
 FLISTSTR aget_self1;
 INT aget_ind1 = INT_zero;
 STR ret_val19;
 CODE_FILE plus_self16;
 dSTR plus_s13;
 CODE_FILE ret_val20;
 CODE_FILE plus_self17;
 dSTR plus_s14;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val21;
 CODE_FILE plus_self18;
 dSTR plus_s15;
 CODE_FILE ret_val22;
 CODE_FILE plus_self19;
 dSTR plus_s16;
 CODE_FILE plus_self20;
 dSTR plus_s17;
 CGEN bind_fortran_nam;
 STR bind_fortran_nam1;
 STR ret_val23;
 STR res = ((STR) NULL);
 BIND_FORTRAN bind_name_self;
 STR bind_name_name;
 STR ret_val24;
 STR res1 = ((STR) NULL);
 BIND_FORTRAN append_underscor;
 STR append_underscor1;
 STR ret_val25;
 STR plus_self21;
 STR plus_sarg2;
 STR ret_val26;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val27;
 CODE_FILE plus_self22;
 dSTR plus_s18;
 CODE_FILE ret_val28;
 CODE_FILE plus_self23;
 dSTR plus_s19;
 CODE_FILE ret_val29;
 CODE_FILE plus_self24;
 dSTR plus_s20;
 INT L151 = INT_zero;
 STR plus_self25;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val30;
 STR s1;
 CHAR str_self2 = CHAR_zero;
 STR ret_val31;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val32;
 STR r;
 STR plus_self26;
 STR plus_sarg3;
 STR ret_val33;
 CODE_FILE plus_self27;
 dSTR plus_s21;
 CODE_FILE plus_self28;
 dSTR plus_s22;
 CODE_FILE plus_self29;
 dSTR plus_s23;
 CGEN in_self;
 AM_LOCAL_EXPR tp_self;
 dTP ret_val34;
 STR plus_self30;
 STR plus_sarg4;
 STR ret_val35;
 AM_LOCAL_EXPR tp_self1;
 dTP ret_val36;
 CGEN mang_self2;
 OB mang_ob2;
 OB mang_ns;
 STR ret_val37;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN mang_self3;
 OB mang_ob3;
 OB mang_ns1;
 STR ret_val38;
 CGEN ndefer_self1;
 STR ndefer_s1;
 AM_LOCAL_EXPR tp_self2;
 dTP ret_val39;
 STR plus_self31;
 STR plus_sarg5;
 STR ret_val40;
 STR plus_self32;
 STR plus_sarg6;
 STR ret_val41;
 FLISTSTR create_self1;
 FLISTSTR ret_val42;
 INT L161 = INT_zero;
 CGEN mang_self4;
 OB mang_ob4;
 OB mang_ns2;
 STR ret_val43;
 STR plus_self33;
 STR plus_sarg7;
 STR ret_val44;
 AM_LOCAL_EXPR is_eq_self;
 OB is_eq_a;
 BOOL ret_val45 = BOOL_zero;
 CGEN mang_self5;
 OB mang_ob5;
 OB mang_ns3;
 STR ret_val46;
 STR plus_self34;
 STR plus_sarg8;
 STR ret_val47;
 STR plus_self35;
 INT plus_arg2 = INT_zero;
 STR ret_val48;
 STR s2;
 INT str_self3 = INT_zero;
 STR ret_val49;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val50;
 FSTR str_self4;
 STR ret_val51;
 STR plus_self36;
 STR plus_sarg9;
 STR ret_val52;
 CGEN ndefer_self2;
 STR ndefer_s2;
 CGEN ndefer_self3;
 STR ndefer_s3;
 INT str_self5 = INT_zero;
 STR ret_val53;
 FSTR buf3 = ((FSTR) NULL);
 FSTR clear_self2;
 INT str_in_self2 = INT_zero;
 FSTR str_in_s2;
 FSTR ret_val54;
 FSTR str_self6;
 STR ret_val55;
 CGEN defer_self;
 STR defer_s;
 CGEN defer_self1;
 STR defer_s1;
 SIG is_forked_self;
 BOOL ret_val56 = BOOL_zero;
 CGEN defer_self2;
 STR defer_s2;
 CGEN defer_self3;
 STR defer_s3;
 CGEN defer_self4;
 STR defer_s4;
 AM_ROUT_DEF is_iter_self;
 BOOL ret_val57 = BOOL_zero;
 SIG is_iter_self1;
 BOOL ret_val58 = BOOL_zero;
 IDENT is_iter_self2 = IDENT_zero;
 BOOL ret_val59 = BOOL_zero;
 AM_LOCAL_EXPR tp_self3;
 dTP ret_val60;
 CODE_FILE uses_tp_self;
 dTP uses_tp_t;
 CODE_FILE plus_self37;
 dSTR plus_s24;
 AM_LOCAL_EXPR tp_self4;
 dTP ret_val61;
 AM_LOCAL_EXPR tp_self5;
 dTP ret_val62;
 AM_LOCAL_EXPR tp_self6;
 dTP ret_val63;
 CGEN mang_self6;
 OB mang_ob6;
 STR ret_val64;
 CODE_FILE plus_self38;
 dSTR plus_s25;
 CODE_FILE ret_val65;
 CODE_FILE plus_self39;
 dSTR plus_s26;
 CODE_FILE ret_val66;
 CGEN mang_self7;
 OB mang_ob7;
 OB mang_ns4;
 STR ret_val67;
 CODE_FILE plus_self40;
 dSTR plus_s27;
 CODE_FILE ret_val68;
 CODE_FILE plus_self41;
 dSTR plus_s28;
 CODE_FILE ret_val69;
 CODE_FILE plus_self42;
 dSTR plus_s29;
 CODE_FILE ret_val70;
 CODE_FILE plus_self43;
 dSTR plus_s30;
 AM_LOCAL_EXPR tp_self7;
 dTP ret_val71;
 CGEN mang_self8;
 OB mang_ob8;
 STR ret_val72;
 CODE_FILE plus_self44;
 dSTR plus_s31;
 CODE_FILE ret_val73;
 CODE_FILE plus_self45;
 dSTR plus_s32;
 CODE_FILE ret_val74;
 CGEN mang_self9;
 OB mang_ob9;
 OB mang_ns5;
 STR ret_val75;
 CODE_FILE plus_self46;
 dSTR plus_s33;
 CODE_FILE ret_val76;
 CODE_FILE plus_self47;
 dSTR plus_s34;
 SIG is_forked_self1;
 BOOL ret_val77 = BOOL_zero;
 AM_LOCAL_EXPR tp_self8;
 dTP ret_val78;
 CGEN mang_self10;
 OB mang_ob10;
 STR ret_val79;
 CODE_FILE plus_self48;
 dSTR plus_s35;
 CODE_FILE ret_val80;
 CODE_FILE plus_self49;
 dSTR plus_s36;
 CGEN cast_self;
 dTP cast_dest_tp;
 dTP cast_src_tp;
 STR cast_expr;
 BOOL cast_only_boxing = BOOL_zero;
 STR ret_val81;
 STR plus_self50;
 STR plus_sarg10;
 STR ret_val82;
 STR plus_self51;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val83;
 STR s3;
 CHAR str_self7 = CHAR_zero;
 STR ret_val84;
 STR create_self2;
 CHAR create_c1 = CHAR_zero;
 STR ret_val85;
 STR r1;
 CGEN ndefer_self4;
 STR ndefer_s4;
 AM_FORMAL_ARG L171_;
 AM_FORMAL_ARG L18;
 extern STR self1;
 extern STR undefI419930828;
 FSTR L19;
 dSTR L20;
 extern STR Definitionof;
 extern STR name78;
 BOOL L22;
 BOOL L231_;
 FSTR L24;
 dSTR L25;
 BOOL L26;
 BOOL L271_;
 FSTR L28;
 dSTR L29;
 extern STR name79;
 FSTR L30;
 dSTR L32;
 dSTR L33;
 OB L34;
 FSTR L35;
 dSTR L36;
 extern STR F_tag_;
 INT L37;
 INT L381_;
 BOOL L39;
 BOOL L401_;
 extern STR line2;
 BOOL L42;
 BOOL L431_;
 FSTR L44;
 dSTR L45;
 dSTR L46;
 OB L47;
 BOOL L48;
 BOOL L491_;
 FSTR L50;
 dSTR L52;
 extern STR name71;
 BOOL L53;
 BOOL L541_;
 FSTR L55;
 dSTR L56;
 STR L571_;
 BOOL L58;
 BOOL L591_;
 FSTR L60;
 dSTR L62;
 extern STR name73;
 FSTR L63;
 dSTR L64;
 extern STR line1;
 BOOL L65;
 BOOL L661_;
 FSTR L67;
 dSTR L68;
 dSTR L69;
 OB L70;
 BOOL L72;
 BOOL L731_;
 FSTR L74;
 dSTR L75;
 extern STR name71;
 BOOL L76;
 BOOL L771_;
 FSTR L78;
 dSTR L79;
 STR L801_;
 BOOL L82;
 BOOL L831_;
 FSTR L84;
 dSTR L85;
 extern STR name83;
 FSTR L86;
 dSTR L87;
 ARRAYSTR* L88;
 ARRAYSTR* L89;
 INT* L90;
 BOOL L92;
 BOOL L93;
 BOOL L941_;
 BOOL L95;
 BOOL L961_;
 FSTR L97;
 dSTR L98;
 dSTR L99;
 OB L100;
 FSTR L102;
 dSTR L103;
 extern STR void6;
 FSTR L104;
 dSTR L105;
 extern STR S_;
 BOOL L106;
 BOOL L1071_;
 FSTR L108;
 dSTR L109;
 BOOL L110;
 BOOL L1111_;
 FSTR L112;
 dSTR L113;
 dSTR L114;
 OB L115;
 FSTR L116;
 dSTR L117;
 extern STR name29;
 INT L1181_br;
 STR aL1181_;
 INT L1191_br;
 STR aL1191_;
 STR L120;
 STR L122;
 STR L123;
 STR L124;
 INT L125;
 OB L126;
 INT L127;
 FSTR L129;
 dSTR L130;
 INT L1321_;
 extern STR struct167654362;
 FSTR L133;
 dSTR L134;
 extern STR name84;
 FSTR L135;
 dSTR L136;
 INT L137;
 INT L1381_;
 INT L1391_;
 INT L140;
 INT L1421_;
 INT L143;
 INT L1441_;
 INT L146;
 INT L1471_;
 INT L148;
 INT L1491_;
 INT L1451_,L1451_m;
 AM_FORMAL_ARG L1501_;
 AM_FORMAL_ARG L152;
 dTP L153;
 AM_FORMAL_ARG L1541_;
 AM_FORMAL_ARG L155;
 extern STR name58;
 STR L1561_;
 INT L1571_;
 dTP L158;
 AM_FORMAL_ARG L1591_;
 AM_FORMAL_ARG L160;
 AM_FORMAL_ARG L1621_;
 AM_FORMAL_ARG L163;
 AM_FORMAL_ARG L1641_;
 AM_FORMAL_ARG L165;
 STR L166;
 extern STR address1;
 STR L1671_;
 STR L168;
 extern STR name16;
 AM_FORMAL_ARG L1691_;
 AM_FORMAL_ARG L170;
 STR L172;
 extern STR size2;
 STR L1731_;
 STR L174;
 extern STR name16;
 INT L1751_;
 AM_FORMAL_ARG L1761_;
 AM_FORMAL_ARG L177;
 AM_FORMAL_ARG L1781_;
 AM_FORMAL_ARG L179;
 extern STR name85;
 STR L1801_;
 extern STR name26;
 INT L1811_;
 INT L1821_br;
 AM_FORMAL_ARG aL1821_;
 INT L1831_;
 extern STR void7;
 AM_FORMAL_ARG L184;
 AM_FORMAL_ARG L185;
 BOOL L186;
 INT L187;
 INT L188;
 BOOL L1891_;
 extern STR NULL2;
 INT L1901_;
 INT L1911_;
 AM_LOCAL_EXPR L192;
 BOOL L193;
 BOOL L194;
 BOOL L1951_;
 BOOL L196;
 BOOL L1971_;
 extern STR void7;
 INT L1981_;
 extern STR void_local_frame;
 BOOL L199;
 BOOL L2001_;
 extern STR name86;
 extern STR struct1270974273;
 INT L2021_;
 INT L2011_,L2011_m;
 INT L203;
 extern STR S_local_frame;
 BOOL L204;
 BOOL L2051_;
 STR L206;
 extern STR name15;
 STR L207;
 STR L208;
 extern STR name16;
 extern STR FFfunc;
 extern STR name16;
 BOOL L209;
 BOOL L210;
 BOOL L211;
 extern STR FFprevNULL;
 extern STR FFprev530765537;
 extern STR FFargs931318564;
 BOOL L212;
 BOOL L213;
 BOOL L214;
 BOOL L2151_;
 STR L216;
 INT L217;
 INT L2181_;
 INT L219;
 CHAR L2201_;
 CHAR L221;
 BOOL L2221_;
 BOOL L223;
 BOOL L2241_;
 BOOL L225;
 BOOL L2261_;
 AM_LOCAL_EXPR L227;
 extern STR volatile3;
 FSTR L228;
 dSTR L229;
 BOOL L230;
 dTP L232;
 BOOL L233;
 BOOL L2341_;
 FSTR L235;
 dSTR L236;
 dSTR L237;
 OB L238;
 BOOL L239;
 BOOL L2401_;
 FSTR L241;
 dSTR L242;
 BOOL L243;
 BOOL L2441_;
 FSTR L245;
 dSTR L246;
 extern STR name36;
 BOOL L247;
 BOOL L2481_;
 FSTR L249;
 dSTR L250;
 BOOL L251;
 BOOL L2521_;
 FSTR L253;
 dSTR L254;
 dSTR L255;
 OB L256;
 FSTR L257;
 dSTR L258;
 BOOL L259;
 BOOL L2601_;
 FSTR L261;
 dSTR L262;
 dSTR L263;
 OB L264;
 BOOL L265;
 BOOL L2661_;
 FSTR L267;
 dSTR L268;
 BOOL L269;
 BOOL L2701_;
 FSTR L272;
 dSTR L273;
 dSTR L274;
 OB L275;
 FSTR L276;
 dSTR L277;
 BOOL L278;
 BOOL L279;
 AM_FORMAL_ARG L2801_;
 AM_FORMAL_ARG L281;
 BOOL L282;
 BOOL L2831_;
 FSTR L284;
 dSTR L285;
 extern STR self4;
 FSTR L286;
 dSTR L287;
 extern STR self5;
 extern STR self_to_be;
 STR L288;
 INT L289;
 OB L290;
 INT L291;
 SATTR(self,saw_outer_return,FALSE);
 sig1 = ATTR(f,sig1);
 L171_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)f,0); 
 L18=L171_;
 MANGLE1827205184(ATTR(self,mangler), ((OB) ATTR(L18,expr)), ((STR) &self1), ((OB) sig1));
 CGEN_newline(self);
 CGEN_newline(self);
 plus_self = ATTR(self,code_c);
 plus_s = ((dSTR) ((STR) &undefI419930828));
 L19 = ATTR(plus_self,ntext);
 L20 = plus_s;
 SATTR(plus_self,ntext,FSTR_p1752847026(L19, (*dSTR_strrSTR[TAG(L20)])(L20)));
 comment_self = self;
 comment_f = ATTR(self,code_c);
 plus_self1 = ((STR) &Definitionof);
 plus_sarg = ATTR(sig1,str);
 ret_val = STR_ap2004550586(plus_self1, plus_sarg);
 plus_self2 = ret_val;
 plus_sarg1 = ATTR(self,func_name_commen);
 ret_val1 = STR_ap2004550586(plus_self2, plus_sarg1);
 comment_com = ret_val1;
 if (ATTR(comment_self,pretty)) {
  plus_self3 = comment_f;
  plus_s1 = ((dSTR) ((STR) &name78));
  L22 = (plus_s1==((dSTR) NULL));
  L231_=!(L22); 
  if (L231_) {
   L24 = ATTR(plus_self3,ntext);
   L25 = plus_s1;
   SATTR(plus_self3,ntext,FSTR_p1752847026(L24, (*dSTR_strrSTR[TAG(L25)])(L25)));
  }
  ret_val2 = plus_self3;
  plus_self4 = ret_val2;
  plus_s2 = ((dSTR) comment_com);
  L26 = (plus_s2==((dSTR) NULL));
  L271_=!(L26); 
  if (L271_) {
   L28 = ATTR(plus_self4,ntext);
   L29 = plus_s2;
   SATTR(plus_self4,ntext,FSTR_p1752847026(L28, (*dSTR_strrSTR[TAG(L29)])(L29)));
  }
  ret_val3 = plus_self4;
  plus_self5 = ret_val3;
  plus_s3 = ((dSTR) ((STR) &name79));
  L30 = ATTR(plus_self5,ntext);
  L32 = plus_s3;
  SATTR(plus_self5,ntext,FSTR_p1752847026(L30, (*dSTR_strrSTR[TAG(L32)])(L32)));
 }
 plus_self6 = ATTR(self,code_c);
 L34=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L34==NULL) FATAL("Unable to allocate more memory");
 memset(L34,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L34)->header.tag=CHAR_tag;
 L33 = (dSTR)((CHAR_boxed) L34);
 ((CHAR_boxed) L33)->immutable_part = '\n';
 plus_s4 = L33;
 L35 = ATTR(plus_self6,ntext);
 L36 = plus_s4;
 SATTR(plus_self6,ntext,FSTR_p1752847026(L35, (*dSTR_strrSTR[TAG(L36)])(L36)));
 if (ATTR(self,func_tables)) {
  register_self = ((PRINT_OB) NULL);
  register_s = ATTR(f,sig1);
  PRINT_OB_funcs = FLISTS69519306(PRINT_OB_funcs, register_s);
  plus_self7 = ((STR) &F_tag_);
  L37 = FLISTS228922656(PRINT_OB_funcs);
  L381_=INTMINUS(L37,1); 
  plus_arg = L381_;
  str_self = plus_arg;
  clear_self = buf1;
  L39 = (clear_self==((FSTR) NULL));
  L401_=!(L39); 
  if (L401_) {
   SATTR(clear_self,loc,0);
  }
  str_in_self = str_self;
  str_in_s = buf1;
  ret_val7 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
  buf1 = ret_val7;
  str_self1 = buf1;
  ret_val8 = STR_fr1097270334(((STR) NULL), str_self1);
  ret_val6 = ret_val8;
  s = ret_val6;
  ret_val5 = STR_ap1077157958(plus_self7, s, TRUE);
  ret_val4 = ret_val5;
  rout_index = ret_val4;
 }
 if (ATTR(self,debug1)) {
  plus_self8 = ATTR(self,code_c);
  plus_s5 = ((dSTR) ((STR) &line2));
  L42 = (plus_s5==((dSTR) NULL));
  L431_=!(L42); 
  if (L431_) {
   L44 = ATTR(plus_self8,ntext);
   L45 = plus_s5;
   SATTR(plus_self8,ntext,FSTR_p1752847026(L44, (*dSTR_strrSTR[TAG(L45)])(L45)));
  }
  ret_val9 = plus_self8;
  plus_self9 = ret_val9;
  L47=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L47==NULL) FATAL("Unable to allocate more memory");
  memset(L47,0,sizeof(struct INT_boxed_struct));
  ((OB)L47)->header.tag=INT_tag;
  L46 = (dSTR)((INT_boxed) L47);
  ((INT_boxed) L46)->immutable_part = SFILE_2070755978(ATTR(f,source1));
  plus_s6 = L46;
  L48 = (plus_s6==((dSTR) NULL));
  L491_=!(L48); 
  if (L491_) {
   L50 = ATTR(plus_self9,ntext);
   L52 = plus_s6;
   SATTR(plus_self9,ntext,FSTR_p1752847026(L50, (*dSTR_strrSTR[TAG(L52)])(L52)));
  }
  ret_val10 = plus_self9;
  plus_self10 = ret_val10;
  plus_s7 = ((dSTR) ((STR) &name71));
  L53 = (plus_s7==((dSTR) NULL));
  L541_=!(L53); 
  if (L541_) {
   L55 = ATTR(plus_self10,ntext);
   L56 = plus_s7;
   SATTR(plus_self10,ntext,FSTR_p1752847026(L55, (*dSTR_strrSTR[TAG(L56)])(L56)));
  }
  ret_val11 = plus_self10;
  plus_self11 = ret_val11;
  file_in_self = ATTR(f,source1);
  aget_self = SFILE_ID_files;
  aget_ind = SFILE_1102877860(file_in_self);
  L571_=(STR)ARR((FLISTSTR)aget_self,aget_ind); 
  ret_val13 = L571_;
  ret_val12 = ret_val13;
  plus_s8 = ((dSTR) ret_val12);
  L58 = (plus_s8==((dSTR) NULL));
  L591_=!(L58); 
  if (L591_) {
   L60 = ATTR(plus_self11,ntext);
   L62 = plus_s8;
   SATTR(plus_self11,ntext,FSTR_p1752847026(L60, (*dSTR_strrSTR[TAG(L62)])(L62)));
  }
  ret_val14 = plus_self11;
  plus_self12 = ret_val14;
  plus_s9 = ((dSTR) ((STR) &name73));
  L63 = ATTR(plus_self12,ntext);
  L64 = plus_s9;
  SATTR(plus_self12,ntext,FSTR_p1752847026(L63, (*dSTR_strrSTR[TAG(L64)])(L64)));
  SATTR(self,last_lineno,SFILE_2070755978(ATTR(f,source1)));
 }
 else {
  if (ATTR(self,pretty)) {
   plus_self13 = ATTR(self,code_c);
   plus_s10 = ((dSTR) ((STR) &line1));
   L65 = (plus_s10==((dSTR) NULL));
   L661_=!(L65); 
   if (L661_) {
    L67 = ATTR(plus_self13,ntext);
    L68 = plus_s10;
    SATTR(plus_self13,ntext,FSTR_p1752847026(L67, (*dSTR_strrSTR[TAG(L68)])(L68)));
   }
   ret_val15 = plus_self13;
   plus_self14 = ret_val15;
   L70=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
   if (L70==NULL) FATAL("Unable to allocate more memory");
   memset(L70,0,sizeof(struct INT_boxed_struct));
   ((OB)L70)->header.tag=INT_tag;
   L69 = (dSTR)((INT_boxed) L70);
   ((INT_boxed) L69)->immutable_part = SFILE_2070755978(ATTR(f,source1));
   plus_s11 = L69;
   L72 = (plus_s11==((dSTR) NULL));
   L731_=!(L72); 
   if (L731_) {
    L74 = ATTR(plus_self14,ntext);
    L75 = plus_s11;
    SATTR(plus_self14,ntext,FSTR_p1752847026(L74, (*dSTR_strrSTR[TAG(L75)])(L75)));
   }
   ret_val16 = plus_self14;
   plus_self15 = ret_val16;
   plus_s12 = ((dSTR) ((STR) &name71));
   L76 = (plus_s12==((dSTR) NULL));
   L771_=!(L76); 
   if (L771_) {
    L78 = ATTR(plus_self15,ntext);
    L79 = plus_s12;
    SATTR(plus_self15,ntext,FSTR_p1752847026(L78, (*dSTR_strrSTR[TAG(L79)])(L79)));
   }
   ret_val17 = plus_self15;
   plus_self16 = ret_val17;
   file_in_self1 = ATTR(f,source1);
   aget_self1 = SFILE_ID_files;
   aget_ind1 = SFILE_1102877860(file_in_self1);
   L801_=(STR)ARR((FLISTSTR)aget_self1,aget_ind1); 
   ret_val19 = L801_;
   ret_val18 = ret_val19;
   plus_s13 = ((dSTR) ret_val18);
   L82 = (plus_s13==((dSTR) NULL));
   L831_=!(L82); 
   if (L831_) {
    L84 = ATTR(plus_self16,ntext);
    L85 = plus_s13;
    SATTR(plus_self16,ntext,FSTR_p1752847026(L84, (*dSTR_strrSTR[TAG(L85)])(L85)));
   }
   ret_val20 = plus_self16;
   plus_self17 = ret_val20;
   plus_s14 = ((dSTR) ((STR) &name83));
   L86 = ATTR(plus_self17,ntext);
   L87 = plus_s14;
   SATTR(plus_self17,ntext,FSTR_p1752847026(L86, (*dSTR_strrSTR[TAG(L87)])(L87)));
   SATTR(self,last_lineno,SFILE_2070755978(ATTR(f,source1)));
  }
 }
 L88 = &arg_list;
 L89 = &type_list;
 L90 = &first_arg;
 res_arg = CGEN_g1210307583(self, f, L88, L89, L90);
 L93 = (ATTR(ATTR(f,sig1),ret)==((dTP) NULL));
 L941_=!(L93); 
 if (L941_) {
  L92 = (res_arg==((STR) NULL));
 } else {
  L92 = FALSE;
 }
 has_return = L92;
 final_arg_list = arg_list;
 SATTR(self,current_fortran_,final_arg_list);
 if (has_return) {
  plus_self18 = ATTR(self,code_c);
  mang_self = self;
  mang_ob = ((OB) ATTR(sig1,ret));
  ret_val21 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
  plus_s15 = ((dSTR) ret_val21);
  L95 = (plus_s15==((dSTR) NULL));
  L961_=!(L95); 
  if (L961_) {
   L97 = ATTR(plus_self18,ntext);
   L98 = plus_s15;
   SATTR(plus_self18,ntext,FSTR_p1752847026(L97, (*dSTR_strrSTR[TAG(L98)])(L98)));
  }
  ret_val22 = plus_self18;
  plus_self19 = ret_val22;
  L100=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
  if (L100==NULL) FATAL("Unable to allocate more memory");
  memset(L100,0,sizeof(struct CHAR_boxed_struct));
  ((OB)L100)->header.tag=CHAR_tag;
  L99 = (dSTR)((CHAR_boxed) L100);
  ((CHAR_boxed) L99)->immutable_part = ' ';
  plus_s16 = L99;
  L102 = ATTR(plus_self19,ntext);
  L103 = plus_s16;
  SATTR(plus_self19,ntext,FSTR_p1752847026(L102, (*dSTR_strrSTR[TAG(L103)])(L103)));
 }
 else {
  plus_self20 = ATTR(self,code_c);
  plus_s17 = ((dSTR) ((STR) &void6));
  L104 = ATTR(plus_self20,ntext);
  L105 = plus_s17;
  SATTR(plus_self20,ntext,FSTR_p1752847026(L104, (*dSTR_strrSTR[TAG(L105)])(L105)));
 }
 bind_fortran_nam = self;
 bind_fortran_nam1 = ATTR(sig1,name1).str;
 res = STR_ap1693864410(ATTR(bind_fortran_nam,fortran_prepend_), bind_fortran_nam1, ATTR(bind_fortran_nam,fortran_append_u));
 if (ATTR(bind_fortran_nam,fortran_bind_fun)) {
  bind_name_self = ((BIND_FORTRAN) NULL);
  bind_name_name = res;
  append_underscor = bind_name_self;
  append_underscor1 = bind_name_name;
  plus_self21 = append_underscor1;
  plus_sarg2 = ((STR) &S_);
  ret_val26 = STR_ap2004550586(plus_self21, plus_sarg2);
  ret_val25 = ret_val26;
  res1 = ret_val25;
  ret_val24 = res1;
  res = ret_val24;
 }
 ret_val23 = res;
 name111 = ret_val23;
 MANGLE1827205184(ATTR(self,mangler), ((OB) sig1), name111, ((OB) NULL));
 plus_self22 = ATTR(self,code_c);
 mang_self1 = self;
 mang_ob1 = ((OB) sig1);
 ret_val27 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
 plus_s18 = ((dSTR) ret_val27);
 L106 = (plus_s18==((dSTR) NULL));
 L1071_=!(L106); 
 if (L1071_) {
  L108 = ATTR(plus_self22,ntext);
  L109 = plus_s18;
  SATTR(plus_self22,ntext,FSTR_p1752847026(L108, (*dSTR_strrSTR[TAG(L109)])(L109)));
 }
 ret_val28 = plus_self22;
 plus_self23 = ret_val28;
 plus_s19 = ((dSTR) ATTR(self,func_name_suffix));
 L110 = (plus_s19==((dSTR) NULL));
 L1111_=!(L110); 
 if (L1111_) {
  L112 = ATTR(plus_self23,ntext);
  L113 = plus_s19;
  SATTR(plus_self23,ntext,FSTR_p1752847026(L112, (*dSTR_strrSTR[TAG(L113)])(L113)));
 }
 ret_val29 = plus_self23;
 plus_self24 = ret_val29;
 L115=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L115==NULL) FATAL("Unable to allocate more memory");
 memset(L115,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L115)->header.tag=CHAR_tag;
 L114 = (dSTR)((CHAR_boxed) L115);
 ((CHAR_boxed) L114)->immutable_part = '(';
 plus_s20 = L114;
 L116 = ATTR(plus_self24,ntext);
 L117 = plus_s20;
 SATTR(plus_self24,ntext,FSTR_p1752847026(L116, (*dSTR_strrSTR[TAG(L117)])(L117)));
 CODE_F1291349628(ATTR(self,code_c), sig1);
 out_args = FALSE;
 {
  BOOL f_L1181_ = TRUE;
  BOOL f_L1191_ = TRUE;
  struct STR_se405450305_frame_struct other1_0;
STR_se405450305_frame noname1 = &other1_0;
  /* loop index variable */
  L151 = 0;
  L11 = final_arg_list;
  L21 = type_list;
  L31 = ((STR) &name29);
  L1181_br=L11==NULL?0:ASIZE((ARRAYSTR)L11); 
  L1191_br=L21==NULL?0:ASIZE((ARRAYSTR)L21); 
  noname1->self = L31;
  noname1->state = 0;
  while (1) {
   if(L151>=L1181_br)  goto after_loop; 
   aL1181_=ARR((ARRAYSTR)L11,L151); 
   arg_str = aL1181_;
   if(L151>=L1191_br)  goto after_loop; 
   aL1191_=ARR((ARRAYSTR)L21,L151); 
   type_str = aL1191_;
   plus_self27 = ATTR(self,code_c);
   plus_self25 = type_str;
   plus_arg1 = ' ';
   str_self2 = plus_arg1;
   create_self = ((STR) NULL);
   create_c = str_self2;
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
   r = L124;
   SARR((STR)r,0,create_c); 
   ;
   ret_val32 = r;
   ret_val31 = ret_val32;
   s1 = ret_val31;
   ret_val30 = STR_ap1077157958(plus_self25, s1, TRUE);
   plus_self26 = ret_val30;
   plus_sarg3 = arg_str;
   ret_val33 = STR_ap2004550586(plus_self26, plus_sarg3);
   noname1->arg1 = ((dSTR) ret_val33);
   L123 = STR_se405450305(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   plus_s21 = ((dSTR) L123);
   L129 = ATTR(plus_self27,ntext);
   L130 = plus_s21;
   SATTR(plus_self27,ntext,FSTR_p1752847026(L129, (*dSTR_strrSTR[TAG(L130)])(L130)));
   L1321_=INTPLUS(L151,1); 
   L151 = L1321_;
  }
 }
 after_loop: ;
 if (ATTR(self,func_tables)) {
  plus_self28 = ATTR(self,code_c);
  plus_s22 = ((dSTR) ((STR) &struct167654362));
  L133 = ATTR(plus_self28,ntext);
  L134 = plus_s22;
  SATTR(plus_self28,ntext,FSTR_p1752847026(L133, (*dSTR_strrSTR[TAG(L134)])(L134)));
 }
 plus_self29 = ATTR(self,code_c);
 plus_s23 = ((dSTR) ((STR) &name84));
 L135 = ATTR(plus_self29,ntext);
 L136 = plus_s23;
 SATTR(plus_self29,ntext,FSTR_p1752847026(L135, (*dSTR_strrSTR[TAG(L136)])(L136)));
 in_self = self;
 L137 = ATTR(in_self,indent);
 L1381_=INTPLUS(L137,1); 
 SATTR(in_self,indent,L1381_);
 arg_pos = first_arg;
 L1391_=(f)==NULL?0:ASIZE((AM_ROUT_DEF)f); 
 L140 = L1391_;
 L1421_=INTPLUS(first_arg,L140); 
 L143 = L1421_;
 L1441_=INTMINUS(L143,1); 
 string_length_po = L1441_;
 {
  BOOL f_L1451_ = TRUE;
  while (1) {
   if (f_L1451_) {
    f_L1451_ = FALSE;
    L41 = 1;
    L1471_=(f)==NULL?0:ASIZE((AM_ROUT_DEF)f); 
    L148 = L1471_;
    L1491_=INTMINUS(L148,1); 
    L51 = L1491_;
    L1451_=L41-1;L1451_m=L51; 
   }
   if(L1451_++>=L1451_m)  goto after_loop1; 
   i = L1451_;
   L1501_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)f,i); 
   L152=L1501_;
   tp_self = ATTR(L152,expr);
   ret_val34 = ATTR(tp_self,tp_at);
   L61 = ret_val34;
   L153 = L61;
   if ((*dTP_is242312711[TAG(L153)])(L153, ((OB) TP_BUI1805763264))) {
    L1541_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)f,i); 
    L155 = L1541_;
    plus_self30 = ((STR) &name58);
    L1561_=(STR)ARR((ARRAYSTR)arg_list,arg_pos); 
    plus_sarg4 = L1561_;
    ret_val35 = STR_ap2004550586(plus_self30, plus_sarg4);
    local11 = CGEN_d1279589030(self, L155, sig1, ret_val35);
    L1571_=INTPLUS(string_length_po,1); 
    string_length_po = L1571_;
   }
   else {
    L158 = L61;
    if ((*dTP_is242312711[TAG(L158)])(L158, ((OB) TP_BUI2088674906))) {
     L1591_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)f,i); 
     L160 = L1591_;
     L1621_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)f,i); 
     L163=L1621_;
     tp_self1 = ATTR(L163,expr);
     ret_val36 = ATTR(tp_self1,tp_at);
     local2 = CGEN_d1279589030(self, L160, sig1, CGEN_d815167520(self, ret_val36));
     ndefer_self = self;
     mang_self2 = self;
     L1641_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)f,i); 
     L165=L1641_;
     mang_ob2 = ((OB) ATTR(L165,expr));
     mang_ns = ((OB) sig1);
     ret_val37 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, mang_ns);
     L166 = ret_val37;
     L1671_=(STR)ARR((ARRAYSTR)arg_list,arg_pos); 
     L168 = L1671_;
     ndefer_s = STR_ap1417971546(L166, ((STR) &address1), L168, ((STR) &name16));
     CGEN_d695831906(ndefer_self);
     SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
     ndefer_self1 = self;
     mang_self3 = self;
     L1691_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)f,i); 
     L170=L1691_;
     mang_ob3 = ((OB) ATTR(L170,expr));
     mang_ns1 = ((OB) sig1);
     ret_val38 = MANGLE119219986(ATTR(mang_self3,mangler), mang_ob3, mang_ns1);
     L172 = ret_val38;
     L1731_=(STR)ARR((ARRAYSTR)arg_list,string_length_po); 
     L174 = L1731_;
     ndefer_s1 = STR_ap1417971546(L172, ((STR) &size2), L174, ((STR) &name16));
     CGEN_d695831906(ndefer_self1);
     SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
     L1751_=INTPLUS(string_length_po,1); 
     string_length_po = L1751_;
    }
    else {
     L1761_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)f,i); 
     L177=L1761_;
     tp_self2 = ATTR(L177,expr);
     ret_val39 = ATTR(tp_self2,tp_at);
     if (CGEN_i1538413090(self, ret_val39)) {
      L1781_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)f,i); 
      L179=L1781_;
      plus_self31 = ((STR) &name85);
      L1801_=(STR)ARR((ARRAYSTR)arg_list,arg_pos); 
      plus_sarg5 = L1801_;
      ret_val40 = STR_ap2004550586(plus_self31, plus_sarg5);
      plus_self32 = ret_val40;
      plus_sarg6 = ((STR) &name26);
      ret_val41 = STR_ap2004550586(plus_self32, plus_sarg6);
      MANGLE1827205184(ATTR(self,mangler), ((OB) ATTR(L179,expr)), ret_val41, ((OB) sig1));
     }
    }
   }
   L1811_=INTPLUS(arg_pos,1); 
   arg_pos = L1811_;
  }
 }
 after_loop1: ;
 if (ATTR(self,func_tables)) {
  create_self1 = ((FLISTSTR) NULL);
  ret_val42 = ((FLISTSTR) NULL);
  ll = ret_val42;
  cn = 0;
  {
   BOOL f_L1821_ = TRUE;
   BOOL f_L1831_ = TRUE;
   /* loop index variable */
   L161 = 0;
   L71 = f;
   L81 = 1;
   L1821_br=L71==NULL?0:ASIZE((AM_ROUT_DEF)L71); 
   L1831_=L81-1; 
   while (1) {
    plus_self33 = ((STR) &void7);
    mang_self4 = self;
    if(L161>=L1821_br)  goto after_loop2; 
    aL1821_=ARR((AM_ROUT_DEF)L71,L161); 
    L185=aL1821_;
    mang_ob4 = ((OB) ATTR(L185,expr));
    mang_ns2 = ((OB) sig1);
    ret_val43 = MANGLE119219986(ATTR(mang_self4,mangler), mang_ob4, mang_ns2);
    plus_sarg7 = ret_val43;
    ret_val44 = STR_ap2004550586(plus_self33, plus_sarg7);
    k = ret_val44;
    if (ATTR(f,is_external)) {
     L1831_++; 
     L188 = L1831_;
     L1891_=(L188)==(1); 
     L186 = L1891_;
    } else {
     L186 = FALSE;
    }
    if (L186) {
     k = ((STR) &NULL2);
    }
    ll = FLISTS1425610062(ll, k);
    L1901_=INTPLUS(cn,1); 
    cn = L1901_;
    L1911_=INTPLUS(L161,1); 
    L161 = L1911_;
   }
  }
  after_loop2: ;
  {
   struct FLISTA2119642552_frame_struct other4_0;
FLISTA2119642552_frame noname2 = &other4_0;
   L91 = ATTR(f,locals1);
   noname2->self = L91;
   noname2->state = 0;
   while (1) {
    L192 = FLISTA2119642552(noname2);
    if (noname2->state == -1) {
     goto after_loop3;
    }
    l = L192;
    L194 = (ATTR(l,name1).str==(STR)0);
    L1951_=!(L194); 
    if (L1951_) {
     is_eq_self = l;
     is_eq_a = ((OB) ATTR(f,rres));
     ret_val45 = SYSOBEQ(((OB) is_eq_self),is_eq_a);
     L196 = ret_val45;
     L1971_=!(L196); 
     L193 = L1971_;
    } else {
     L193 = FALSE;
    }
    if (L193) {
     SATTR(l,needs_init,TRUE);
     plus_self34 = ((STR) &void7);
     mang_self5 = self;
     mang_ob5 = ((OB) l);
     mang_ns3 = ((OB) sig1);
     ret_val46 = MANGLE119219986(ATTR(mang_self5,mangler), mang_ob5, mang_ns3);
     plus_sarg8 = ret_val46;
     ret_val47 = STR_ap2004550586(plus_self34, plus_sarg8);
     ll = FLISTS1425610062(ll, ret_val47);
     L1981_=INTPLUS(cn,1); 
     cn = L1981_;
    }
   }
  }
  after_loop3: ;
  ndefer_self2 = self;
  plus_self35 = ((STR) &void_local_frame);
  plus_arg2 = cn;
  str_self3 = plus_arg2;
  clear_self1 = buf2;
  L199 = (clear_self1==((FSTR) NULL));
  L2001_=!(L199); 
  if (L2001_) {
   SATTR(clear_self1,loc,0);
  }
  str_in_self1 = str_self3;
  str_in_s1 = buf2;
  ret_val50 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
  buf2 = ret_val50;
  str_self4 = buf2;
  ret_val51 = STR_fr1097270334(((STR) NULL), str_self4);
  ret_val49 = ret_val51;
  s2 = ret_val49;
  ret_val48 = STR_ap1077157958(plus_self35, s2, TRUE);
  plus_self36 = ret_val48;
  plus_sarg9 = ((STR) &name86);
  ret_val52 = STR_ap2004550586(plus_self36, plus_sarg9);
  ndefer_s2 = ret_val52;
  CGEN_d695831906(ndefer_self2);
  SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
  ndefer_self3 = self;
  ndefer_s3 = ((STR) &struct1270974273);
  CGEN_d695831906(ndefer_self3);
  SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
  {
   BOOL f_L2011_ = TRUE;
   struct FLISTS1171631134_frame_struct other5_0;
FLISTS1171631134_frame noname3 = &other5_0;
   L101 = 0;
   L2021_=INTMINUS(cn,1); 
   L121 = L2021_;
   L131 = ll;
   L2011_=L101-1;L2011_m=L121; 
   noname3->self = L131;
   noname3->state = 0;
   while (1) {
    if(L2011_++>=L2011_m)  goto after_loop4; 
    i1 = L2011_;
    defer_self = self;
    str_self5 = i1;
    clear_self2 = buf3;
    L204 = (clear_self2==((FSTR) NULL));
    L2051_=!(L204); 
    if (L2051_) {
     SATTR(clear_self2,loc,0);
    }
    str_in_self2 = str_self5;
    str_in_s2 = buf3;
    ret_val54 = INT_st367594495(str_in_self2, str_in_s2, 0, 10, ' ');
    buf3 = ret_val54;
    str_self6 = buf3;
    ret_val55 = STR_fr1097270334(((STR) NULL), str_self6);
    ret_val53 = ret_val55;
    L206 = ret_val53;
    L207 = FLISTS1171631134(noname3);
    if (noname3->state == -1) {
     goto after_loop4;
    }
    L208 = L207;
    defer_s = STR_ap44172742(((STR) &S_local_frame), L206, ((STR) &name15), L208, ((STR) &name16));
    SATTR(defer_self,routine_code,FSTR_p1752847026(ATTR(defer_self,routine_code), defer_s));
   }
  }
  after_loop4: ;
  defer_self1 = self;
  defer_s1 = STR_ap1693864410(((STR) &FFfunc), rout_index, ((STR) &name16));
  SATTR(defer_self1,routine_code,FSTR_p1752847026(ATTR(defer_self1,routine_code), defer_s1));
  if (ATTR(f,is_external)) {
   L209 = TRUE;
  } else {
   is_forked_self = sig1;
   if (ATTR(is_forked_self,is_par_routine)) {
    L211 = TRUE;
   } else {
    L211 = ATTR(is_forked_self,is_fork_routine);
   }
   if (L211) {
    L210 = TRUE;
   } else {
    L210 = ATTR(is_forked_self,is_attach_routin);
   }
   ret_val56 = L210;
   L209 = ret_val56;
  }
  if (L209) {
   defer_self2 = self;
   defer_s2 = ((STR) &FFprevNULL);
   SATTR(defer_self2,routine_code,FSTR_p1752847026(ATTR(defer_self2,routine_code), defer_s2));
  }
  else {
   defer_self3 = self;
   defer_s3 = ((STR) &FFprev530765537);
   SATTR(defer_self3,routine_code,FSTR_p1752847026(ATTR(defer_self3,routine_code), defer_s3));
  }
  defer_self4 = self;
  defer_s4 = ((STR) &FFargs931318564);
  SATTR(defer_self4,routine_code,FSTR_p1752847026(ATTR(defer_self4,routine_code), defer_s4));
 }
 is_iter_self = f;
 is_iter_self1 = ATTR(is_iter_self,sig1);
 is_iter_self2 = ATTR(is_iter_self1,name1);
 L214 = (is_iter_self2.str==((STR) NULL));
 L2151_=!(L214); 
 if (L2151_) {
  L216 = is_iter_self2.str;
  L217 = STR_sizerINT(is_iter_self2.str);
  L2181_=INTMINUS(L217,1); 
  L219 = L2181_;
  L2201_=ARR((STR)L216,L219); 
  L221 = L2201_;
  L2221_=L221=='!'; 
  L213 = L2221_;
 } else {
  L213 = FALSE;
 }
 ret_val59 = L213;
 ret_val58 = ret_val59;
 ret_val57 = ret_val58;
 L223 = ret_val57;
 L2241_=!(L223); 
 if (L2241_) {
  L225 = (ATTR(f,locals1)==((FLISTA725283029) NULL));
  L2261_=!(L225); 
  L212 = L2261_;
 } else {
  L212 = FALSE;
 }
 if (L212) {
  {
   struct FLISTA2119642552_frame_struct other6_0;
FLISTA2119642552_frame noname4 = &other6_0;
   L141 = ATTR(f,locals1);
   noname4->self = L141;
   noname4->state = 0;
   while (1) {
    L227 = FLISTA2119642552(noname4);
    if (noname4->state == -1) {
     goto after_loop5;
    }
    lv = L227;
    uses_tp_self = ATTR(self,code_c);
    tp_self3 = lv;
    ret_val60 = ATTR(tp_self3,tp_at);
    uses_tp_t = ret_val60;
    CODE_F525364269(uses_tp_self, LAYOUT79932797(((LAYOUT_TBL) NULL), uses_tp_t));
    CGEN_newline(self);
    if (ATTR(lv,is_volatile)) {
     plus_self37 = ATTR(self,code_c);
     plus_s24 = ((dSTR) ((STR) &volatile3));
     L228 = ATTR(plus_self37,ntext);
     L229 = plus_s24;
     SATTR(plus_self37,ntext,FSTR_p1752847026(L228, (*dSTR_strrSTR[TAG(L229)])(L229)));
    }
    if (ATTR(lv,needs_init)) {
     L230 = TRUE;
    } else {
     tp_self4 = lv;
     ret_val61 = ATTR(tp_self4,tp_at);
     L232 = ret_val61;
     L230 = (*dTP_is1334578382[TAG(L232)])(L232);
    }
    if (L230) {
     tp_self5 = lv;
     ret_val62 = ATTR(tp_self5,tp_at);
     def = CGEN_d815167520(self, ret_val62);
     plus_self38 = ATTR(self,code_c);
     mang_self6 = self;
     tp_self6 = lv;
     ret_val63 = ATTR(tp_self6,tp_at);
     mang_ob6 = ((OB) ret_val63);
     ret_val64 = MANGLE119219986(ATTR(mang_self6,mangler), mang_ob6, ((OB) NULL));
     plus_s25 = ((dSTR) ret_val64);
     L233 = (plus_s25==((dSTR) NULL));
     L2341_=!(L233); 
     if (L2341_) {
      L235 = ATTR(plus_self38,ntext);
      L236 = plus_s25;
      SATTR(plus_self38,ntext,FSTR_p1752847026(L235, (*dSTR_strrSTR[TAG(L236)])(L236)));
     }
     ret_val65 = plus_self38;
     plus_self39 = ret_val65;
     L238=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L238==NULL) FATAL("Unable to allocate more memory");
     memset(L238,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L238)->header.tag=CHAR_tag;
     L237 = (dSTR)((CHAR_boxed) L238);
     ((CHAR_boxed) L237)->immutable_part = ' ';
     plus_s26 = L237;
     L239 = (plus_s26==((dSTR) NULL));
     L2401_=!(L239); 
     if (L2401_) {
      L241 = ATTR(plus_self39,ntext);
      L242 = plus_s26;
      SATTR(plus_self39,ntext,FSTR_p1752847026(L241, (*dSTR_strrSTR[TAG(L242)])(L242)));
     }
     ret_val66 = plus_self39;
     plus_self40 = ret_val66;
     mang_self7 = self;
     mang_ob7 = ((OB) lv);
     mang_ns4 = ((OB) sig1);
     ret_val67 = MANGLE119219986(ATTR(mang_self7,mangler), mang_ob7, mang_ns4);
     plus_s27 = ((dSTR) ret_val67);
     L243 = (plus_s27==((dSTR) NULL));
     L2441_=!(L243); 
     if (L2441_) {
      L245 = ATTR(plus_self40,ntext);
      L246 = plus_s27;
      SATTR(plus_self40,ntext,FSTR_p1752847026(L245, (*dSTR_strrSTR[TAG(L246)])(L246)));
     }
     ret_val68 = plus_self40;
     plus_self41 = ret_val68;
     plus_s28 = ((dSTR) ((STR) &name36));
     L247 = (plus_s28==((dSTR) NULL));
     L2481_=!(L247); 
     if (L2481_) {
      L249 = ATTR(plus_self41,ntext);
      L250 = plus_s28;
      SATTR(plus_self41,ntext,FSTR_p1752847026(L249, (*dSTR_strrSTR[TAG(L250)])(L250)));
     }
     ret_val69 = plus_self41;
     plus_self42 = ret_val69;
     plus_s29 = ((dSTR) def);
     L251 = (plus_s29==((dSTR) NULL));
     L2521_=!(L251); 
     if (L2521_) {
      L253 = ATTR(plus_self42,ntext);
      L254 = plus_s29;
      SATTR(plus_self42,ntext,FSTR_p1752847026(L253, (*dSTR_strrSTR[TAG(L254)])(L254)));
     }
     ret_val70 = plus_self42;
     plus_self43 = ret_val70;
     L256=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L256==NULL) FATAL("Unable to allocate more memory");
     memset(L256,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L256)->header.tag=CHAR_tag;
     L255 = (dSTR)((CHAR_boxed) L256);
     ((CHAR_boxed) L255)->immutable_part = ';';
     plus_s30 = L255;
     L257 = ATTR(plus_self43,ntext);
     L258 = plus_s30;
     SATTR(plus_self43,ntext,FSTR_p1752847026(L257, (*dSTR_strrSTR[TAG(L258)])(L258)));
    }
    else {
     plus_self44 = ATTR(self,code_c);
     mang_self8 = self;
     tp_self7 = lv;
     ret_val71 = ATTR(tp_self7,tp_at);
     mang_ob8 = ((OB) ret_val71);
     ret_val72 = MANGLE119219986(ATTR(mang_self8,mangler), mang_ob8, ((OB) NULL));
     plus_s31 = ((dSTR) ret_val72);
     L259 = (plus_s31==((dSTR) NULL));
     L2601_=!(L259); 
     if (L2601_) {
      L261 = ATTR(plus_self44,ntext);
      L262 = plus_s31;
      SATTR(plus_self44,ntext,FSTR_p1752847026(L261, (*dSTR_strrSTR[TAG(L262)])(L262)));
     }
     ret_val73 = plus_self44;
     plus_self45 = ret_val73;
     L264=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L264==NULL) FATAL("Unable to allocate more memory");
     memset(L264,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L264)->header.tag=CHAR_tag;
     L263 = (dSTR)((CHAR_boxed) L264);
     ((CHAR_boxed) L263)->immutable_part = ' ';
     plus_s32 = L263;
     L265 = (plus_s32==((dSTR) NULL));
     L2661_=!(L265); 
     if (L2661_) {
      L267 = ATTR(plus_self45,ntext);
      L268 = plus_s32;
      SATTR(plus_self45,ntext,FSTR_p1752847026(L267, (*dSTR_strrSTR[TAG(L268)])(L268)));
     }
     ret_val74 = plus_self45;
     plus_self46 = ret_val74;
     mang_self9 = self;
     mang_ob9 = ((OB) lv);
     mang_ns5 = ((OB) sig1);
     ret_val75 = MANGLE119219986(ATTR(mang_self9,mangler), mang_ob9, mang_ns5);
     plus_s33 = ((dSTR) ret_val75);
     L269 = (plus_s33==((dSTR) NULL));
     L2701_=!(L269); 
     if (L2701_) {
      L272 = ATTR(plus_self46,ntext);
      L273 = plus_s33;
      SATTR(plus_self46,ntext,FSTR_p1752847026(L272, (*dSTR_strrSTR[TAG(L273)])(L273)));
     }
     ret_val76 = plus_self46;
     plus_self47 = ret_val76;
     L275=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L275==NULL) FATAL("Unable to allocate more memory");
     memset(L275,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L275)->header.tag=CHAR_tag;
     L274 = (dSTR)((CHAR_boxed) L275);
     ((CHAR_boxed) L274)->immutable_part = ';';
     plus_s34 = L274;
     L276 = ATTR(plus_self47,ntext);
     L277 = plus_s34;
     SATTR(plus_self47,ntext,FSTR_p1752847026(L276, (*dSTR_strrSTR[TAG(L277)])(L277)));
    }
   }
  }
  after_loop5: ;
 }
 is_forked_self1 = sig1;
 if (ATTR(is_forked_self1,is_par_routine)) {
  L279 = TRUE;
 } else {
  L279 = ATTR(is_forked_self1,is_fork_routine);
 }
 if (L279) {
  L278 = TRUE;
 } else {
  L278 = ATTR(is_forked_self1,is_attach_routin);
 }
 ret_val77 = L278;
 if (ret_val77) {
  CGEN_newline(self);
  plus_self48 = ATTR(self,code_c);
  mang_self10 = self;
  L2801_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)f,0); 
  L281=L2801_;
  tp_self8 = ATTR(L281,expr);
  ret_val78 = ATTR(tp_self8,tp_at);
  mang_ob10 = ((OB) ret_val78);
  ret_val79 = MANGLE119219986(ATTR(mang_self10,mangler), mang_ob10, ((OB) NULL));
  plus_s35 = ((dSTR) ret_val79);
  L282 = (plus_s35==((dSTR) NULL));
  L2831_=!(L282); 
  if (L2831_) {
   L284 = ATTR(plus_self48,ntext);
   L285 = plus_s35;
   SATTR(plus_self48,ntext,FSTR_p1752847026(L284, (*dSTR_strrSTR[TAG(L285)])(L285)));
  }
  ret_val80 = plus_self48;
  plus_self49 = ret_val80;
  plus_s36 = ((dSTR) ((STR) &self4));
  L286 = ATTR(plus_self49,ntext);
  L287 = plus_s36;
  SATTR(plus_self49,ntext,FSTR_p1752847026(L286, (*dSTR_strrSTR[TAG(L287)])(L287)));
  ndefer_self4 = self;
  plus_self50 = ((STR) &self5);
  cast_self = self;
  cast_dest_tp = ATTR(sig1,tp);
  cast_src_tp = ((dTP) TP_BUI1325635093);
  cast_expr = ((STR) &self_to_be);
  cast_only_boxing = FALSE;
  ret_val81 = CGEN_d1915593494(cast_self, cast_dest_tp, cast_src_tp, cast_expr, cast_only_boxing, ((dMODE) NULL));
  plus_sarg10 = ret_val81;
  ret_val82 = STR_ap2004550586(plus_self50, plus_sarg10);
  plus_self51 = ret_val82;
  plus_arg3 = ';';
  str_self7 = plus_arg3;
  create_self2 = ((STR) NULL);
  create_c1 = str_self7;
  L289 = 1;
  L291=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L289)*sizeof(CHAR);
  L290=ZALLOC_LEAF_BIG(L291);
  if (L290==NULL) FATAL("Unable to allocate more memory");
  memset(L290,0,L291);
  ((OB)L290)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L290)->header.destroyed=0;
#endif

  L288 = ((STR) L290);
  L288->asize = L289;
  r1 = L288;
  SARR((STR)r1,0,create_c1); 
  ;
  ret_val85 = r1;
  ret_val84 = ret_val85;
  s3 = ret_val84;
  ret_val83 = STR_ap1077157958(plus_self51, s3, TRUE);
  ndefer_s4 = ret_val83;
  CGEN_d695831906(ndefer_self4);
  SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
 }
 SATTR(self,current_arg_list,arg_list);
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_init(CGEN self) {
 BOOL need_to_append = BOOL_zero;
 BOOL need_to_prepend = BOOL_zero;
 CONFIG_DEF d;
 CONFIG_DEF L11;
 ARRAYSTR e;
 ARRAYSTR L21;
 STR c;
 TP_CLASS tp_class;
 STR class_name = ((STR) NULL);
 STR nm;
 ARRAYdTP L31;
 STR pname;
 dGET_OPTIONS op;
 dGENERATE_AM genam;
 FLISTSTR L41;
 STR n;
 PROG psather_self;
 BOOL ret_val = BOOL_zero;
 CONFIG_TBL get_bool_self;
 STR get_bool_s;
 BOOL ret_val1 = BOOL_zero;
 CONFIG_DEF r;
 CONFIG_DEF aget_self;
 INT aget_i = INT_zero;
 ARRAYSTR ret_val2;
 CONFIG_TBL get_bool_self1;
 STR get_bool_s1;
 BOOL ret_val3 = BOOL_zero;
 CONFIG_DEF r1;
 CONFIG_DEF aget_self1;
 INT aget_i1 = INT_zero;
 ARRAYSTR ret_val4;
 CONFIG_TBL get_bool_self2;
 STR get_bool_s2;
 BOOL ret_val5 = BOOL_zero;
 CONFIG_DEF r2;
 CONFIG_DEF aget_self2;
 INT aget_i2 = INT_zero;
 ARRAYSTR ret_val6;
 FSETSTR is_empty_self;
 BOOL ret_val7 = BOOL_zero;
 CGEN warning_self;
 STR warning_s;
 CONFIG_DEF eltb_self;
 ARRAYSTR ret_val8;
 ARRAYARRAYSTR L51;
 BOOL eltb_if_first = BOOL_zero;
 INT L61 = INT_zero;
 INT L71 = INT_zero;
 TP_TBL tp_class_for_sel;
 STR tp_class_for_s;
 TP_CLASS ret_val9;
 INT i = INT_zero;
 INT L81 = INT_zero;
 FMAPAM1787628573 create_self;
 FMAPAM1787628573 ret_val10;
 FMAPAM1244483504 create_self1;
 FMAPAM1244483504 ret_val11;
 FLISTAM_ROUT_DEF create_self2;
 FLISTAM_ROUT_DEF ret_val12;
 FLISTAM_ROUT_DEF create_self3;
 FLISTAM_ROUT_DEF ret_val13;
 FMAPSI518162669 create_self4;
 FMAPSI518162669 ret_val14;
 FSETAM_ROUT_DEF create_self5;
 FSETAM_ROUT_DEF ret_val15;
 FSETAM_ROUT_DEF create_self6;
 FSETAM_ROUT_DEF ret_val16;
 FLISTA1893461511 create_self7;
 FLISTA1893461511 ret_val17;
 FLISTA1893461511 create_self8;
 FLISTA1893461511 ret_val18;
 FLISTA1893461511 create_self9;
 FLISTA1893461511 ret_val19;
 FSETSIG create_self10;
 FSETSIG ret_val20;
 FSETSIG create_self11;
 FSETSIG ret_val21;
 FMAPSTRSTR create_self12;
 FMAPSTRSTR ret_val22;
 FMAPSIGSTR create_self13;
 FMAPSIGSTR ret_val23;
 FMAPdTPCODE_FILE create_self14;
 FMAPdTPCODE_FILE ret_val24;
 STR plus_self;
 STR plus_sarg;
 STR ret_val25;
 CODE_FILE plus_self1;
 dSTR plus_s;
 STR plus_self2;
 STR plus_sarg1;
 STR ret_val26;
 CODE_FILE plus_self3;
 dSTR plus_s1;
 CODE_FILE plus_self4;
 dSTR plus_s2;
 CODE_FILE plus_self5;
 dSTR plus_s3;
 CODE_FILE plus_self6;
 dSTR plus_s4;
 CODE_FILE plus_self7;
 dSTR plus_s5;
 CODE_FILE plus_self8;
 dSTR plus_s6;
 CODE_FILE plus_self9;
 dSTR plus_s7;
 CODE_FILE plus_self10;
 dSTR plus_s8;
 CODE_FILE plus_self11;
 dSTR plus_s9;
 CONFIG_TBL get_bool_self3;
 STR get_bool_s3;
 BOOL ret_val27 = BOOL_zero;
 CONFIG_DEF r3;
 CONFIG_DEF aget_self3;
 INT aget_i3 = INT_zero;
 ARRAYSTR ret_val28;
 CODE_FILE plus_self12;
 dSTR plus_s10;
 CODE_FILE plus_self13;
 dSTR plus_s11;
 CODE_FILE plus_self14;
 dSTR plus_s12;
 CODE_FILE ret_val29;
 CODE_FILE plus_self15;
 dSTR plus_s13;
 CODE_FILE ret_val30;
 CODE_FILE plus_self16;
 dSTR plus_s14;
 CODE_FILE ret_val31;
 CODE_FILE plus_self17;
 dSTR plus_s15;
 CODE_FILE ret_val32;
 CODE_FILE plus_self18;
 dSTR plus_s16;
 CODE_FILE ret_val33;
 CODE_FILE plus_self19;
 dSTR plus_s17;
 CODE_FILE ret_val34;
 CODE_FILE plus_self20;
 dSTR plus_s18;
 CODE_FILE plus_self21;
 dSTR plus_s19;
 CODE_FILE plus_self22;
 dSTR plus_s20;
 CODE_FILE ret_val35;
 CODE_FILE plus_self23;
 dSTR plus_s21;
 CODE_FILE ret_val36;
 CODE_FILE plus_self24;
 dSTR plus_s22;
 STR plus_self25;
 STR plus_sarg2;
 STR ret_val37;
 CODE_FILE plus_self26;
 dSTR plus_s23;
 CODE_FILE ret_val38;
 CODE_FILE plus_self27;
 dSTR plus_s24;
 CODE_FILE ret_val39;
 CODE_FILE plus_self28;
 dSTR plus_s25;
 CODE_FILE plus_self29;
 dSTR plus_s26;
 FSETdTP create_self15;
 FSETdTP ret_val40;
 FMAPdTPINT create_self16;
 FMAPdTPINT ret_val41;
 INT L9;
 BOOL L101_;
 BOOL L12;
 extern STR POLLING;
 STR L13;
 INT L141_;
 INT L15;
 BOOL L161_;
 extern STR NULL_SEGFAULTS1;
 extern STR FORTRA1082766496;
 BOOL L17;
 BOOL L18;
 BOOL L191_;
 ARRAYARRAYSTR L20;
 ARRAYSTR L221_;
 ARRAYSTR L23;
 STR L241_;
 STR L25;
 extern STR true1;
 extern STR S_;
 extern STR FORTRA2067593620;
 BOOL L26;
 BOOL L27;
 BOOL L281_;
 ARRAYARRAYSTR L29;
 ARRAYSTR L301_;
 ARRAYSTR L32;
 STR L331_;
 STR L34;
 extern STR true1;
 extern STR S_;
 extern STR FORTRA1117417849;
 BOOL L35;
 BOOL L36;
 BOOL L371_;
 ARRAYARRAYSTR L38;
 ARRAYSTR L391_;
 ARRAYSTR L40;
 STR L421_;
 STR L43;
 extern STR true1;
 BOOL L44;
 BOOL L45;
 INT L46;
 BOOL L471_;
 BOOL L48;
 BOOL L491_;
 extern STR forceo1611549167;
 extern STR BUILTIN_CLASSES;
 INT L501_br;
 ARRAYSTR aL501_;
 ARRAYSTR L52;
 INT L531_br;
 STR aL531_;
 STR L54;
 INT* L55;
 TP_CLASS L56;
 CHAR L571_;
 CHAR L58;
 BOOL L591_;
 INT L60;
 INT L621_;
 INT L631_br;
 dTP aL631_;
 dTP L64;
 dTP L65;
 extern STR S_;
 INT L661_;
 INT L671_;
 INT L681_;
 extern STR code1;
 extern STR satherh;
 extern STR tagsh;
 extern STR Makefile;
 extern STR COMPILE_OPTIONS;
 extern STR LINK_OPTIONS;
 extern STR systemc;
 extern STR globalsc;
 extern STR stringsc;
 extern STR dispatchc;
 extern STR unboxc;
 BOOL L69;
 extern STR defineFFpFF;
 FSTR L70;
 dSTR L72;
 STR L73;
 CHAR L741_;
 CHAR L75;
 BOOL L761_;
 BOOL L77;
 BOOL L781_;
 extern STR name46;
 extern STR ifndef581135037;
 FSTR L79;
 dSTR L80;
 BOOL L82;
 extern STR definePRINT_PO;
 FSTR L83;
 dSTR L84;
 BOOL L85;
 BOOL L86;
 extern STR define1754196501;
 FSTR L87;
 dSTR L88;
 extern STR definePRINT_POG;
 FSTR L89;
 dSTR L90;
 extern STR define341738156;
 FSTR L91;
 dSTR L92;
 extern STR defineDEBUG;
 FSTR L93;
 dSTR L94;
 extern STR define2066094158;
 FSTR L95;
 dSTR L96;
 extern STR define1447004461;
 FSTR L97;
 dSTR L98;
 extern STR defineSTATS;
 FSTR L99;
 dSTR L100;
 extern STR ZONES1;
 BOOL L102;
 BOOL L103;
 BOOL L1041_;
 ARRAYARRAYSTR L105;
 ARRAYSTR L1061_;
 ARRAYSTR L107;
 STR L1081_;
 STR L109;
 extern STR true1;
 extern STR defineSIVA;
 FSTR L110;
 dSTR L111;
 extern STR defineZONES;
 FSTR L112;
 dSTR L113;
 extern STR define2120327584;
 BOOL L114;
 BOOL L1151_;
 FSTR L116;
 dSTR L117;
 extern STR define948640192;
 BOOL L118;
 BOOL L1191_;
 FSTR L120;
 dSTR L121;
 dSTR L122;
 OB L123;
 BOOL L124;
 BOOL L1251_;
 FSTR L126;
 dSTR L127;
 extern STR name9;
 BOOL L128;
 BOOL L1291_;
 FSTR L130;
 dSTR L131;
 extern STR define1213585748;
 BOOL L132;
 BOOL L1331_;
 FSTR L134;
 dSTR L135;
 dSTR L136;
 OB L137;
 BOOL L138;
 BOOL L1391_;
 FSTR L140;
 dSTR L142;
 extern STR name9;
 FSTR L143;
 dSTR L144;
 extern STR definePOLLING;
 FSTR L145;
 dSTR L146;
 extern STR include3;
 BOOL L147;
 BOOL L1481_;
 FSTR L149;
 dSTR L150;
 BOOL L151;
 BOOL L1521_;
 FSTR L153;
 dSTR L154;
 extern STR System1504128282;
 FSTR L155;
 dSTR L156;
 STR L157;
 CHAR L1581_;
 CHAR L159;
 BOOL L1601_;
 BOOL L162;
 BOOL L1631_;
 extern STR name46;
 extern STR include3;
 BOOL L164;
 BOOL L1651_;
 FSTR L166;
 dSTR L167;
 BOOL L168;
 BOOL L1691_;
 FSTR L170;
 dSTR L171;
 extern STR headerh;
 FSTR L172;
 dSTR L173;
 extern STR endif1;
 FSTR L174;
 dSTR L175;
 dGET_MAIN_SIG L176;
 if (ATTR(self,only_check)) {
  return;
 }
 L9 = CGEN_m1427112264;
 L101_=(L9)==(0); 
 if (L101_) {
  CGEN_m1427112264 = 24;
 }
 psather_self = ATTR(self,prog);
 if (ATTR(psather_self,threads)) {
  L12 = TRUE;
 } else {
  L12 = ATTR(psather_self,distributed);
 }
 ret_val = L12;
 if (ret_val) {
  SATTR(self,type_tables,TRUE);
 }
 L13 = CONFIG1397479570(ATTR(ATTR(self,prog),config), ((STR) &POLLING), 0);
 L141_=atoi(((STR)L13)->arr_part); 
 L15 = L141_;
 L161_=(0)<(L15); 
 SATTR(self,polling,L161_);
 SATTR(self,null_segfaults,BOOL_f112211388(BOOL_zero, CONFIG1397479570(ATTR(ATTR(self,prog),config), ((STR) &NULL_SEGFAULTS1), 0)));
 get_bool_self = ATTR(ATTR(self,prog),config);
 get_bool_s = ((STR) &FORTRA1082766496);
 r = CONFIG1152920950(get_bool_self, get_bool_s);
 L18 = (r==((CONFIG_DEF) NULL));
 L191_=!(L18); 
 if (L191_) {
  aget_self = r;
  aget_i = 0;
  L20 = ATTR(aget_self,expr);
  L221_=(ARRAYSTR)ARR((ARRAYARRAYSTR)L20,aget_i); 
  ret_val2 = L221_;
  L23 = ret_val2;
  L241_=(STR)ARR((ARRAYSTR)L23,0); 
  L25 = L241_;
  L17 = STR_is111530248(L25, ((STR) &true1));
 } else {
  L17 = FALSE;
 }
 ret_val1 = L17;
 need_to_append = ret_val1;
 if (need_to_append) {
  SATTR(self,fortran_append_u,((STR) &S_));
 }
 get_bool_self1 = ATTR(ATTR(self,prog),config);
 get_bool_s1 = ((STR) &FORTRA2067593620);
 r1 = CONFIG1152920950(get_bool_self1, get_bool_s1);
 L27 = (r1==((CONFIG_DEF) NULL));
 L281_=!(L27); 
 if (L281_) {
  aget_self1 = r1;
  aget_i1 = 0;
  L29 = ATTR(aget_self1,expr);
  L301_=(ARRAYSTR)ARR((ARRAYARRAYSTR)L29,aget_i1); 
  ret_val4 = L301_;
  L32 = ret_val4;
  L331_=(STR)ARR((ARRAYSTR)L32,0); 
  L34 = L331_;
  L26 = STR_is111530248(L34, ((STR) &true1));
 } else {
  L26 = FALSE;
 }
 ret_val3 = L26;
 need_to_prepend = ret_val3;
 if (need_to_prepend) {
  SATTR(self,fortran_prepend_,((STR) &S_));
 }
 get_bool_self2 = ATTR(ATTR(self,prog),config);
 get_bool_s2 = ((STR) &FORTRA1117417849);
 r2 = CONFIG1152920950(get_bool_self2, get_bool_s2);
 L36 = (r2==((CONFIG_DEF) NULL));
 L371_=!(L36); 
 if (L371_) {
  aget_self2 = r2;
  aget_i2 = 0;
  L38 = ATTR(aget_self2,expr);
  L391_=(ARRAYSTR)ARR((ARRAYARRAYSTR)L38,aget_i2); 
  ret_val6 = L391_;
  L40 = ret_val6;
  L421_=(STR)ARR((ARRAYSTR)L40,0); 
  L43 = L421_;
  L35 = STR_is111530248(L43, ((STR) &true1));
 } else {
  L35 = FALSE;
 }
 ret_val5 = L35;
 SATTR(self,fortran_bind_fun,ret_val5);
 if (ATTR(self,force_all)) {
  L44 = TRUE;
 } else {
  is_empty_self = ATTR(self,force_routines);
  if ((is_empty_self==((FSETSTR) NULL))) {
   L45 = TRUE;
  } else {
   L46 = ATTR(is_empty_self,hsize);
   L471_=(L46)==(0); 
   L45 = L471_;
  }
  ret_val7 = L45;
  L48 = ret_val7;
  L491_=!(L48); 
  L44 = L491_;
 }
 if (L44) {
  warning_self = self;
  warning_s = ((STR) &forceo1611549167);
  PROG_warning_STR(ATTR(warning_self,prog), warning_s);
 }
 SATTR(self,mangler,MANGLE2136606480(((MANGLE) NULL), ATTR(self,prog)));
 d = CONFIG1152920950(ATTR(ATTR(self,prog),config), ((STR) &BUILTIN_CLASSES));
 {
  BOOL f_L501_ = TRUE;
  /* Initialization of inlined iter CONFIG_DEF::elt!:ARRAY{STR} */
  L11 = d;
  eltb_self = L11;
  /* loop index variable */
  L61 = 0;
  L51 = ATTRs(eltb_self,expr,((ARRAYARRAYSTR) NULL));
  L501_br=L51==NULL?0:ASIZE((ARRAYARRAYSTR)L51); 
  eltb_if_first = TRUE;
  while (1) {
   if(L61>=L501_br)  goto after_loop; 
   aL501_=ARR((ARRAYARRAYSTR)L51,L61); 
   e = aL501_;
   {
    BOOL f_L531_ = TRUE;
    /* loop index variable */
    L71 = 0;
    L21 = e;
    L531_br=L21==NULL?0:ASIZE((ARRAYSTR)L21); 
    while (1) {
     if(L71>=L531_br)  goto after_loop1; 
     aL531_=ARR((ARRAYSTR)L21,L71); 
     c = aL531_;
     tp_class_for_sel = ATTR(ATTR(self,prog),tp_tbl);
     tp_class_for_s = c;
     i = 0;
     L55 = &i;
     L56 = TP_TBL1484824239(tp_class_for_sel, tp_class_for_s, L55);
     ret_val9 = L56;
     tp_class = ret_val9;
     nm = ATTR(tp_class,name1).str;
     L571_=ARR((STR)nm,0); 
     L58 = L571_;
     L591_=L58=='$'; 
     if (L591_) {
      L60 = STR_sizerINT(nm);
      L621_=INTMINUS(L60,1); 
      class_name = STR_tail_INTrSTR(nm, L621_);
     }
     else {
      class_name = nm;
     }
     {
      BOOL f_L631_ = TRUE;
      /* loop index variable */
      L81 = 0;
      L31 = ATTR(tp_class,params);
      L631_br=L31==NULL?0:ASIZE((ARRAYdTP)L31); 
      while (1) {
       if(L81>=L631_br)  goto after_loop2; 
       aL631_=ARR((ARRAYdTP)L31,L81); 
       L65 = aL631_;
       pname = (*dTP_strrSTR[TAG(L65)])(L65);
       class_name = STR_ap1693864410(class_name, ((STR) &S_), pname);
       L661_=INTPLUS(L81,1); 
       L81 = L661_;
      }
     }
     after_loop2: ;
     MANGLE1827205184(ATTR(self,mangler), ((OB) tp_class), class_name, ((OB) NULL));
     L671_=INTPLUS(L71,1); 
     L71 = L671_;
    }
   }
   after_loop1: ;
   L681_=INTPLUS(L61,1); 
   L61 = L681_;
  }
 }
 after_loop: ;
 op = ATTR(ATTR(self,prog),get_options);
 switch (TAG(op)) {
  case CS_OPTIONS_tag:
   SATTR(self,options1,((CS_OPTIONS) op)); break;
  default: ;
   FATAL("No applicable type in typecase\nin CGEN::init\n./Back/cgen.sa:293:14");
 }
 genam = ATTR(ATTR(self,prog),generate_am);
 switch (TAG(genam)) {
  case GENERATE_AM_tag:
   SATTR(self,gen,((GENERATE_AM) genam)); break;
  default: ;
   FATAL("No applicable type in typecase\nin CGEN::init\n./Back/cgen.sa:295:14");
 }
 create_self = ((FMAPAM1787628573) NULL);
 ret_val10 = ((FMAPAM1787628573) NULL);
 SATTR(self,nested_its,ret_val10);
 create_self1 = ((FMAPAM1244483504) NULL);
 ret_val11 = ((FMAPAM1244483504) NULL);
 SATTR(self,nested_bits,ret_val11);
 create_self2 = ((FLISTAM_ROUT_DEF) NULL);
 ret_val12 = ((FLISTAM_ROUT_DEF) NULL);
 SATTR(self,abstract_routs,ret_val12);
 create_self3 = ((FLISTAM_ROUT_DEF) NULL);
 ret_val13 = ((FLISTAM_ROUT_DEF) NULL);
 SATTR(self,abstract_iters,ret_val13);
 create_self4 = ((FMAPSI518162669) NULL);
 ret_val14 = ((FMAPSI518162669) NULL);
 SATTR(self,not_emitted,ret_val14);
 create_self5 = ((FSETAM_ROUT_DEF) NULL);
 ret_val15 = ((FSETAM_ROUT_DEF) NULL);
 SATTR(self,leftovers,ret_val15);
 create_self6 = ((FSETAM_ROUT_DEF) NULL);
 ret_val16 = ((FSETAM_ROUT_DEF) NULL);
 SATTR(self,emitted_leftover,ret_val16);
 create_self7 = ((FLISTA1893461511) NULL);
 ret_val17 = ((FLISTA1893461511) NULL);
 SATTR(self,bnd_rout_creates,ret_val17);
 create_self8 = ((FLISTA1893461511) NULL);
 ret_val18 = ((FLISTA1893461511) NULL);
 SATTR(self,bnd_rout_creates1,ret_val18);
 create_self9 = ((FLISTA1893461511) NULL);
 ret_val19 = ((FLISTA1893461511) NULL);
 SATTR(self,bnd_iter_creates,ret_val19);
 create_self10 = ((FSETSIG) NULL);
 ret_val20 = ((FSETSIG) NULL);
 SATTR(self,arg_frames,ret_val20);
 create_self11 = ((FSETSIG) NULL);
 ret_val21 = ((FSETSIG) NULL);
 SATTR(self,emitted_iter_all,ret_val21);
 SATTR(self,str_count,1);
 create_self12 = ((FMAPSTRSTR) NULL);
 ret_val22 = ((FMAPSTRSTR) NULL);
 SATTR(self,string_constants,ret_val22);
 create_self13 = ((FMAPSIGSTR) NULL);
 ret_val23 = ((FMAPSIGSTR) NULL);
 SATTR(self,emitted_dispatch,ret_val23);
 create_self14 = ((FMAPdTPCODE_FILE) NULL);
 ret_val24 = ((FMAPdTPCODE_FILE) NULL);
 SATTR(self,code_files,ret_val24);
 SATTR(self,indent,0);
 SATTR(self,abstract_calls,0);
 SATTR(self,concrete_calls,0);
 SATTR(self,inlined_value_ob,0);
 SATTR(self,inlined_pointer_,0);
 SATTR(self,in_constant,FALSE);
 plus_self = ATTR(self,executable1);
 plus_sarg = ((STR) &code1);
 ret_val25 = STR_ap2004550586(plus_self, plus_sarg);
 SATTR(self,code_dir,ret_val25);
 CODE_F1479352172(((CODE_FILE) NULL), ATTR(self,prog), ATTR(self,code_dir));
 SATTR(self,sather_h,CODE_F1432464034(((CODE_FILE) NULL), ((STR) &satherh)));
 SATTR(self,tags_h,CODE_F1432464034(((CODE_FILE) NULL), ((STR) &tagsh)));
 SATTR(self,makefile,CODE_F1432464034(((CODE_FILE) NULL), ((STR) &Makefile)));
 SATTR(self,comp_options,CODE_F1432464034(((CODE_FILE) NULL), ((STR) &COMPILE_OPTIONS)));
 SATTR(self,link_options,CODE_F1432464034(((CODE_FILE) NULL), ((STR) &LINK_OPTIONS)));
 SATTR(self,system_c,CODE_F1432464034(((CODE_FILE) NULL), ((STR) &systemc)));
 SATTR(self,code_files,FMAPdT2092797743(ATTR(self,code_files), ((dTP) TP_BUILTIN_sys), ATTR(self,system_c)));
 SATTR(ATTR(self,system_c),is_c_code,TRUE);
 SATTR(ATTR(self,system_c),do_not_merge,TRUE);
 SATTR(self,globals_c,CODE_F1432464034(((CODE_FILE) NULL), ((STR) &globalsc)));
 SATTR(ATTR(self,globals_c),is_c_code,TRUE);
 SATTR(ATTR(self,globals_c),do_not_merge,TRUE);
 SATTR(self,strings_c,CODE_F1432464034(((CODE_FILE) NULL), ((STR) &stringsc)));
 SATTR(ATTR(self,strings_c),is_c_code,TRUE);
 SATTR(ATTR(self,strings_c),do_not_merge,TRUE);
 SATTR(self,dispatch_c,CODE_F1432464034(((CODE_FILE) NULL), ((STR) &dispatchc)));
 SATTR(ATTR(self,dispatch_c),is_c_code,TRUE);
 SATTR(self,unbox_c,CODE_F1432464034(((CODE_FILE) NULL), ((STR) &unboxc)));
 SATTR(ATTR(self,unbox_c),is_c_code,TRUE);
 if (ATTR(self,print_ob)) {
  L69 = TRUE;
 } else {
  L69 = ATTR(self,print_ob_g);
 }
 if (L69) {
  plus_self1 = ATTR(self,unbox_c);
  plus_s = ((dSTR) ((STR) &defineFFpFF));
  L70 = ATTR(plus_self1,ntext);
  L72 = plus_s;
  SATTR(plus_self1,ntext,FSTR_p1752847026(L70, (*dSTR_strrSTR[TAG(L72)])(L72)));
 }
 SATTR(self,comp_home,ATTR(ATTR(self,prog),home));
 L73 = ATTR(self,comp_home);
 L741_=ARR((STR)L73,0); 
 L75 = L741_;
 L761_=L75=='/'; 
 L77 = L761_;
 L781_=!(L77); 
 if (L781_) {
  plus_self2 = ((STR) &name46);
  plus_sarg1 = ATTR(self,comp_home);
  ret_val26 = STR_ap2004550586(plus_self2, plus_sarg1);
  SATTR(self,comp_home,ret_val26);
 }
 plus_self3 = ATTR(self,sather_h);
 plus_s1 = ((dSTR) ((STR) &ifndef581135037));
 L79 = ATTR(plus_self3,ntext);
 L80 = plus_s1;
 SATTR(plus_self3,ntext,FSTR_p1752847026(L79, (*dSTR_strrSTR[TAG(L80)])(L80)));
 if (ATTR(self,print_ob)) {
  L82 = TRUE;
 } else {
  L82 = ATTR(self,print_ob_g);
 }
 if (L82) {
  plus_self4 = ATTR(self,sather_h);
  plus_s2 = ((dSTR) ((STR) &definePRINT_PO));
  L83 = ATTR(plus_self4,ntext);
  L84 = plus_s2;
  SATTR(plus_self4,ntext,FSTR_p1752847026(L83, (*dSTR_strrSTR[TAG(L84)])(L84)));
 }
 if (ATTR(self,print_ob)) {
  L86 = TRUE;
 } else {
  L86 = ATTR(self,print_ob_g);
 }
 if (L86) {
  L85 = ATTR(self,func_tables);
 } else {
  L85 = FALSE;
 }
 if (L85) {
  plus_self5 = ATTR(self,sather_h);
  plus_s3 = ((dSTR) ((STR) &define1754196501));
  L87 = ATTR(plus_self5,ntext);
  L88 = plus_s3;
  SATTR(plus_self5,ntext,FSTR_p1752847026(L87, (*dSTR_strrSTR[TAG(L88)])(L88)));
 }
 if (ATTR(self,print_ob_g)) {
  plus_self6 = ATTR(self,sather_h);
  plus_s4 = ((dSTR) ((STR) &definePRINT_POG));
  L89 = ATTR(plus_self6,ntext);
  L90 = plus_s4;
  SATTR(plus_self6,ntext,FSTR_p1752847026(L89, (*dSTR_strrSTR[TAG(L90)])(L90)));
 }
 if (ATTR(self,deterministic1)) {
  plus_self7 = ATTR(self,sather_h);
  plus_s5 = ((dSTR) ((STR) &define341738156));
  L91 = ATTR(plus_self7,ntext);
  L92 = plus_s5;
  SATTR(plus_self7,ntext,FSTR_p1752847026(L91, (*dSTR_strrSTR[TAG(L92)])(L92)));
 }
 if (ATTR(self,debug1)) {
  plus_self8 = ATTR(self,sather_h);
  plus_s6 = ((dSTR) ((STR) &defineDEBUG));
  L93 = ATTR(plus_self8,ntext);
  L94 = plus_s6;
  SATTR(plus_self8,ntext,FSTR_p1752847026(L93, (*dSTR_strrSTR[TAG(L94)])(L94)));
 }
 if (ATTR(ATTR(self,options1),destroy_chk)) {
  plus_self9 = ATTR(self,sather_h);
  plus_s7 = ((dSTR) ((STR) &define2066094158));
  L95 = ATTR(plus_self9,ntext);
  L96 = plus_s7;
  SATTR(plus_self9,ntext,FSTR_p1752847026(L95, (*dSTR_strrSTR[TAG(L96)])(L96)));
 }
 if (ATTR(self,null_segfaults)) {
  plus_self10 = ATTR(self,sather_h);
  plus_s8 = ((dSTR) ((STR) &define1447004461));
  L97 = ATTR(plus_self10,ntext);
  L98 = plus_s8;
  SATTR(plus_self10,ntext,FSTR_p1752847026(L97, (*dSTR_strrSTR[TAG(L98)])(L98)));
 }
 if (ATTR(ATTR(self,options1),stats)) {
  plus_self11 = ATTR(self,sather_h);
  plus_s9 = ((dSTR) ((STR) &defineSTATS));
  L99 = ATTR(plus_self11,ntext);
  L100 = plus_s9;
  SATTR(plus_self11,ntext,FSTR_p1752847026(L99, (*dSTR_strrSTR[TAG(L100)])(L100)));
 }
 get_bool_self3 = ATTR(ATTR(self,prog),config);
 get_bool_s3 = ((STR) &ZONES1);
 r3 = CONFIG1152920950(get_bool_self3, get_bool_s3);
 L103 = (r3==((CONFIG_DEF) NULL));
 L1041_=!(L103); 
 if (L1041_) {
  aget_self3 = r3;
  aget_i3 = 0;
  L105 = ATTR(aget_self3,expr);
  L1061_=(ARRAYSTR)ARR((ARRAYARRAYSTR)L105,aget_i3); 
  ret_val28 = L1061_;
  L107 = ret_val28;
  L1081_=(STR)ARR((ARRAYSTR)L107,0); 
  L109 = L1081_;
  L102 = STR_is111530248(L109, ((STR) &true1));
 } else {
  L102 = FALSE;
 }
 ret_val27 = L102;
 if (ret_val27) {
  SATTR(self,siva,TRUE);
  plus_self12 = ATTR(self,sather_h);
  plus_s10 = ((dSTR) ((STR) &defineSIVA));
  L110 = ATTR(plus_self12,ntext);
  L111 = plus_s10;
  SATTR(plus_self12,ntext,FSTR_p1752847026(L110, (*dSTR_strrSTR[TAG(L111)])(L111)));
 }
 if (ATTR(ATTR(self,prog),zones)) {
  plus_self13 = ATTR(self,sather_h);
  plus_s11 = ((dSTR) ((STR) &defineZONES));
  L112 = ATTR(plus_self13,ntext);
  L113 = plus_s11;
  SATTR(plus_self13,ntext,FSTR_p1752847026(L112, (*dSTR_strrSTR[TAG(L113)])(L113)));
 }
 if (ATTR(ATTR(self,options1),cache1)) {
  plus_self14 = ATTR(self,sather_h);
  plus_s12 = ((dSTR) ((STR) &define2120327584));
  L114 = (plus_s12==((dSTR) NULL));
  L1151_=!(L114); 
  if (L1151_) {
   L116 = ATTR(plus_self14,ntext);
   L117 = plus_s12;
   SATTR(plus_self14,ntext,FSTR_p1752847026(L116, (*dSTR_strrSTR[TAG(L117)])(L117)));
  }
  ret_val29 = plus_self14;
  plus_self15 = ret_val29;
  plus_s13 = ((dSTR) ((STR) &define948640192));
  L118 = (plus_s13==((dSTR) NULL));
  L1191_=!(L118); 
  if (L1191_) {
   L120 = ATTR(plus_self15,ntext);
   L121 = plus_s13;
   SATTR(plus_self15,ntext,FSTR_p1752847026(L120, (*dSTR_strrSTR[TAG(L121)])(L121)));
  }
  ret_val30 = plus_self15;
  plus_self16 = ret_val30;
  L123=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L123==NULL) FATAL("Unable to allocate more memory");
  memset(L123,0,sizeof(struct INT_boxed_struct));
  ((OB)L123)->header.tag=INT_tag;
  L122 = (dSTR)((INT_boxed) L123);
  ((INT_boxed) L122)->immutable_part = ATTR(ATTR(self,options1),cache_size);
  plus_s14 = L122;
  L124 = (plus_s14==((dSTR) NULL));
  L1251_=!(L124); 
  if (L1251_) {
   L126 = ATTR(plus_self16,ntext);
   L127 = plus_s14;
   SATTR(plus_self16,ntext,FSTR_p1752847026(L126, (*dSTR_strrSTR[TAG(L127)])(L127)));
  }
  ret_val31 = plus_self16;
  plus_self17 = ret_val31;
  plus_s15 = ((dSTR) ((STR) &name9));
  L128 = (plus_s15==((dSTR) NULL));
  L1291_=!(L128); 
  if (L1291_) {
   L130 = ATTR(plus_self17,ntext);
   L131 = plus_s15;
   SATTR(plus_self17,ntext,FSTR_p1752847026(L130, (*dSTR_strrSTR[TAG(L131)])(L131)));
  }
  ret_val32 = plus_self17;
  plus_self18 = ret_val32;
  plus_s16 = ((dSTR) ((STR) &define1213585748));
  L132 = (plus_s16==((dSTR) NULL));
  L1331_=!(L132); 
  if (L1331_) {
   L134 = ATTR(plus_self18,ntext);
   L135 = plus_s16;
   SATTR(plus_self18,ntext,FSTR_p1752847026(L134, (*dSTR_strrSTR[TAG(L135)])(L135)));
  }
  ret_val33 = plus_self18;
  plus_self19 = ret_val33;
  L137=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L137==NULL) FATAL("Unable to allocate more memory");
  memset(L137,0,sizeof(struct INT_boxed_struct));
  ((OB)L137)->header.tag=INT_tag;
  L136 = (dSTR)((INT_boxed) L137);
  ((INT_boxed) L136)->immutable_part = ATTR(ATTR(self,options1),cache_slot_size);
  plus_s17 = L136;
  L138 = (plus_s17==((dSTR) NULL));
  L1391_=!(L138); 
  if (L1391_) {
   L140 = ATTR(plus_self19,ntext);
   L142 = plus_s17;
   SATTR(plus_self19,ntext,FSTR_p1752847026(L140, (*dSTR_strrSTR[TAG(L142)])(L142)));
  }
  ret_val34 = plus_self19;
  plus_self20 = ret_val34;
  plus_s18 = ((dSTR) ((STR) &name9));
  L143 = ATTR(plus_self20,ntext);
  L144 = plus_s18;
  SATTR(plus_self20,ntext,FSTR_p1752847026(L143, (*dSTR_strrSTR[TAG(L144)])(L144)));
 }
 if (ATTR(self,polling)) {
  plus_self21 = ATTR(self,sather_h);
  plus_s19 = ((dSTR) ((STR) &definePOLLING));
  L145 = ATTR(plus_self21,ntext);
  L146 = plus_s19;
  SATTR(plus_self21,ntext,FSTR_p1752847026(L145, (*dSTR_strrSTR[TAG(L146)])(L146)));
 }
 plus_self22 = ATTR(self,sather_h);
 plus_s20 = ((dSTR) ((STR) &include3));
 L147 = (plus_s20==((dSTR) NULL));
 L1481_=!(L147); 
 if (L1481_) {
  L149 = ATTR(plus_self22,ntext);
  L150 = plus_s20;
  SATTR(plus_self22,ntext,FSTR_p1752847026(L149, (*dSTR_strrSTR[TAG(L150)])(L150)));
 }
 ret_val35 = plus_self22;
 plus_self23 = ret_val35;
 plus_s21 = ((dSTR) ATTR(self,comp_home));
 L151 = (plus_s21==((dSTR) NULL));
 L1521_=!(L151); 
 if (L1521_) {
  L153 = ATTR(plus_self23,ntext);
  L154 = plus_s21;
  SATTR(plus_self23,ntext,FSTR_p1752847026(L153, (*dSTR_strrSTR[TAG(L154)])(L154)));
 }
 ret_val36 = plus_self23;
 plus_self24 = ret_val36;
 plus_s22 = ((dSTR) ((STR) &System1504128282));
 L155 = ATTR(plus_self24,ntext);
 L156 = plus_s22;
 SATTR(plus_self24,ntext,FSTR_p1752847026(L155, (*dSTR_strrSTR[TAG(L156)])(L156)));
 {
  struct FLISTS1171631134_frame_struct other4_0;
FLISTS1171631134_frame noname1 = &other4_0;
  L41 = ATTR(ATTR(self,prog),platforms);
  noname1->self = L41;
  noname1->state = 0;
  while (1) {
   L157 = FLISTS1171631134(noname1);
   if (noname1->state == -1) {
    goto after_loop3;
   }
   n = L157;
   L1581_=ARR((STR)n,0); 
   L159 = L1581_;
   L1601_=L159=='/'; 
   L162 = L1601_;
   L1631_=!(L162); 
   if (L1631_) {
    plus_self25 = ((STR) &name46);
    plus_sarg2 = n;
    ret_val37 = STR_ap2004550586(plus_self25, plus_sarg2);
    n = ret_val37;
   }
   plus_self26 = ATTR(self,sather_h);
   plus_s23 = ((dSTR) ((STR) &include3));
   L164 = (plus_s23==((dSTR) NULL));
   L1651_=!(L164); 
   if (L1651_) {
    L166 = ATTR(plus_self26,ntext);
    L167 = plus_s23;
    SATTR(plus_self26,ntext,FSTR_p1752847026(L166, (*dSTR_strrSTR[TAG(L167)])(L167)));
   }
   ret_val38 = plus_self26;
   plus_self27 = ret_val38;
   plus_s24 = ((dSTR) n);
   L168 = (plus_s24==((dSTR) NULL));
   L1691_=!(L168); 
   if (L1691_) {
    L170 = ATTR(plus_self27,ntext);
    L171 = plus_s24;
    SATTR(plus_self27,ntext,FSTR_p1752847026(L170, (*dSTR_strrSTR[TAG(L171)])(L171)));
   }
   ret_val39 = plus_self27;
   plus_self28 = ret_val39;
   plus_s25 = ((dSTR) ((STR) &headerh));
   L172 = ATTR(plus_self28,ntext);
   L173 = plus_s25;
   SATTR(plus_self28,ntext,FSTR_p1752847026(L172, (*dSTR_strrSTR[TAG(L173)])(L173)));
  }
 }
 after_loop3: ;
 plus_self29 = ATTR(self,sather_h);
 plus_s26 = ((dSTR) ((STR) &endif1));
 L174 = ATTR(plus_self29,ntext);
 L175 = plus_s26;
 SATTR(plus_self29,ntext,FSTR_p1752847026(L174, (*dSTR_strrSTR[TAG(L175)])(L175)));
 create_self15 = ((FSETdTP) NULL);
 ret_val40 = ((FSETdTP) NULL);
 SATTR(self,needs_tag,ret_val40);
 create_self16 = ((FMAPdTPINT) NULL);
 ret_val41 = ((FMAPdTPINT) NULL);
 SATTR(self,tags,ret_val41);
 L176 = ATTR(ATTR(self,prog),get_main_sig);
 SATTR(self,main_sig,(*dGET_M422427726[TAG(L176)])(L176));
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_newline(CGEN self) {
 INT L11 = INT_zero;
 CODE_FILE plus_self;
 dSTR plus_s;
 INT L21 = INT_zero;
 CODE_FILE plus_self1;
 dSTR plus_s1;
 FSTR L3;
 dSTR L4;
 dSTR L6;
 OB L7;
 FSTR L8;
 dSTR L9;
 INT L101_;
 plus_self = ATTR(self,code_c);
 plus_s = ((dSTR) CGEN_eolrSTR(self));
 L3 = ATTR(plus_self,ntext);
 L4 = plus_s;
 SATTR(plus_self,ntext,FSTR_p1752847026(L3, (*dSTR_strrSTR[TAG(L4)])(L4)));
 {
  BOOL f_L51_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = ATTR(self,indent);
  while (1) {
   if(L21>=L11)  goto after_loop; 
   ;
   plus_self1 = ATTR(self,code_c);
   L7=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
   if (L7==NULL) FATAL("Unable to allocate more memory");
   memset(L7,0,sizeof(struct CHAR_boxed_struct));
   ((OB)L7)->header.tag=CHAR_tag;
   L6 = (dSTR)((CHAR_boxed) L7);
   ((CHAR_boxed) L6)->immutable_part = ' ';
   plus_s1 = L6;
   L8 = ATTR(plus_self1,ntext);
   L9 = plus_s1;
   SATTR(plus_self1,ntext,FSTR_p1752847026(L8, (*dSTR_strrSTR[TAG(L9)])(L9)));
   L101_=INTPLUS(L21,1); 
   L21 = L101_;
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_s1419723103(CGEN self, AM_ROUT_DEF func) {
 SIG is_iter_self;
 BOOL ret_val = BOOL_zero;
 IDENT is_iter_self1 = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 FSTR create_self;
 FSTR ret_val2;
 BOOL L1;
 BOOL L2;
 BOOL L31_;
 STR L4;
 INT L5;
 INT L61_;
 INT L7;
 CHAR L81_;
 CHAR L9;
 BOOL L101_;
 FMAPAM1787628573 L11;
 FMAPAM1244483504 L12;
 FSTR L13;
 INT L14;
 OB L15;
 INT L16;
 SATTR(self,current_sig,ATTR(func,sig1));
 SATTR(self,current_am_rout_,func);
 SATTR(self,current_function,ATTR(ATTR(func,sig1),str));
 CGEN_set_chks(self);
 SATTR(self,code_c,CGEN_c2146664558(self, ATTR(ATTR(self,current_sig),tp), ATTR(func,source1)));
 is_iter_self = ATTR(func,sig1);
 is_iter_self1 = ATTR(is_iter_self,name1);
 L2 = (is_iter_self1.str==((STR) NULL));
 L31_=!(L2); 
 if (L31_) {
  L4 = is_iter_self1.str;
  L5 = STR_sizerINT(is_iter_self1.str);
  L61_=INTMINUS(L5,1); 
  L7 = L61_;
  L81_=ARR((STR)L4,L7); 
  L9 = L81_;
  L101_=L9=='!'; 
  L1 = L101_;
 } else {
  L1 = FALSE;
 }
 ret_val1 = L1;
 ret_val = ret_val1;
 if (ret_val) {
  L11 = ATTR(self,nested_its);
  SATTR(self,nested_its,FMAPAM293039037(L11, func, FLISTA1106467315(((FLISTA1062334999) NULL), 5)));
  L12 = ATTR(self,nested_bits);
  SATTR(self,nested_bits,FMAPAM1818297900(L12, func, FLISTA681914133(((FLISTA1409846210) NULL), 5)));
 }
 create_self = ((FSTR) NULL);
 L14 = 16;
 L16=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L14)*sizeof(CHAR);
 L15=ZALLOC_LEAF_BIG(L16);
 if (L15==NULL) FATAL("Unable to allocate more memory");
 memset(L15,0,L16);
 ((OB)L15)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L15)->header.destroyed=0;
#endif

 L13 = ((FSTR) L15);
 L13->asize = L14;
 ret_val2 = L13;
 SATTR(self,routine_code,ret_val2);
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_set_chks(CGEN self) {
 STR n;
 CS_OPTIONS pre_chk_self;
 STR pre_chk_n;
 BOOL ret_val = BOOL_zero;
 CS_OPTIONS post_chk_self;
 STR post_chk_n;
 BOOL ret_val1 = BOOL_zero;
 CS_OPTIONS invariant_chk_se;
 STR invariant_chk_n;
 BOOL ret_val2 = BOOL_zero;
 CS_OPTIONS assert_chk_self;
 STR assert_chk_n;
 BOOL ret_val3 = BOOL_zero;
 CS_OPTIONS arith_chk_self;
 STR arith_chk_n;
 BOOL ret_val4 = BOOL_zero;
 CS_OPTIONS bounds_chk_self;
 STR bounds_chk_n;
 BOOL ret_val5 = BOOL_zero;
 CS_OPTIONS void_chk_self;
 STR void_chk_n;
 BOOL ret_val6 = BOOL_zero;
 CS_OPTIONS when_chk_self;
 STR when_chk_n;
 BOOL ret_val7 = BOOL_zero;
 CS_OPTIONS return_chk_self;
 STR return_chk_n;
 BOOL ret_val8 = BOOL_zero;
 dTP L1;
 BOOL L2;
 BOOL L3;
 BOOL L4;
 BOOL L51_;
 BOOL L6;
 BOOL L7;
 BOOL L8;
 BOOL L91_;
 BOOL L10;
 BOOL L11;
 BOOL L12;
 BOOL L131_;
 BOOL L14;
 BOOL L15;
 BOOL L16;
 BOOL L171_;
 BOOL L18;
 BOOL L19;
 BOOL L20;
 BOOL L211_;
 BOOL L22;
 BOOL L23;
 BOOL L24;
 BOOL L251_;
 BOOL L26;
 BOOL L27;
 BOOL L28;
 BOOL L291_;
 BOOL L30;
 BOOL L31;
 BOOL L32;
 BOOL L331_;
 BOOL L34;
 BOOL L35;
 BOOL L36;
 BOOL L371_;
 L1 = ATTR(ATTR(self,current_sig),tp);
 n = (*dTP_strrSTR[TAG(L1)])(L1);
 pre_chk_self = ATTR(self,options1);
 pre_chk_n = n;
 if (ATTR(pre_chk_self,pre_all)) {
  L3 = TRUE;
 } else {
  L3 = FSETST1025458804(ATTR(pre_chk_self,pre_in), pre_chk_n);
 }
 if (L3) {
  L4 = FSETST1025458804(ATTR(pre_chk_self,pre_out), pre_chk_n);
  L51_=!(L4); 
  L2 = L51_;
 } else {
  L2 = FALSE;
 }
 ret_val = L2;
 SATTR(self,chk_pre,ret_val);
 post_chk_self = ATTR(self,options1);
 post_chk_n = n;
 if (ATTR(post_chk_self,post_all)) {
  L7 = TRUE;
 } else {
  L7 = FSETST1025458804(ATTR(post_chk_self,post_in), post_chk_n);
 }
 if (L7) {
  L8 = FSETST1025458804(ATTR(post_chk_self,post_out), post_chk_n);
  L91_=!(L8); 
  L6 = L91_;
 } else {
  L6 = FALSE;
 }
 ret_val1 = L6;
 SATTR(self,chk_post,ret_val1);
 invariant_chk_se = ATTR(self,options1);
 invariant_chk_n = n;
 if (ATTR(invariant_chk_se,invariant_all)) {
  L11 = TRUE;
 } else {
  L11 = FSETST1025458804(ATTR(invariant_chk_se,invariant_in), invariant_chk_n);
 }
 if (L11) {
  L12 = FSETST1025458804(ATTR(invariant_chk_se,invariant_out), invariant_chk_n);
  L131_=!(L12); 
  L10 = L131_;
 } else {
  L10 = FALSE;
 }
 ret_val2 = L10;
 SATTR(self,chk_invariant,ret_val2);
 assert_chk_self = ATTR(self,options1);
 assert_chk_n = n;
 if (ATTR(assert_chk_self,assert_all)) {
  L15 = TRUE;
 } else {
  L15 = FSETST1025458804(ATTR(assert_chk_self,assert_in), assert_chk_n);
 }
 if (L15) {
  L16 = FSETST1025458804(ATTR(assert_chk_self,assert_out), assert_chk_n);
  L171_=!(L16); 
  L14 = L171_;
 } else {
  L14 = FALSE;
 }
 ret_val3 = L14;
 SATTR(self,chk_assert,ret_val3);
 arith_chk_self = ATTR(self,options1);
 arith_chk_n = n;
 if (ATTR(arith_chk_self,arith_all)) {
  L19 = TRUE;
 } else {
  L19 = FSETST1025458804(ATTR(arith_chk_self,arith_in), arith_chk_n);
 }
 if (L19) {
  L20 = FSETST1025458804(ATTR(arith_chk_self,arith_out), arith_chk_n);
  L211_=!(L20); 
  L18 = L211_;
 } else {
  L18 = FALSE;
 }
 ret_val4 = L18;
 SATTR(self,chk_arith,ret_val4);
 bounds_chk_self = ATTR(self,options1);
 bounds_chk_n = n;
 if (ATTR(bounds_chk_self,bounds_all)) {
  L23 = TRUE;
 } else {
  L23 = FSETST1025458804(ATTR(bounds_chk_self,bounds_in), bounds_chk_n);
 }
 if (L23) {
  L24 = FSETST1025458804(ATTR(bounds_chk_self,bounds_out), bounds_chk_n);
  L251_=!(L24); 
  L22 = L251_;
 } else {
  L22 = FALSE;
 }
 ret_val5 = L22;
 SATTR(self,chk_bounds,ret_val5);
 void_chk_self = ATTR(self,options1);
 void_chk_n = n;
 if (ATTR(void_chk_self,void_all)) {
  L27 = TRUE;
 } else {
  L27 = FSETST1025458804(ATTR(void_chk_self,void_in), void_chk_n);
 }
 if (L27) {
  L28 = FSETST1025458804(ATTR(void_chk_self,void_out), void_chk_n);
  L291_=!(L28); 
  L26 = L291_;
 } else {
  L26 = FALSE;
 }
 ret_val6 = L26;
 SATTR(self,chk_void,ret_val6);
 when_chk_self = ATTR(self,options1);
 when_chk_n = n;
 if (ATTR(when_chk_self,when_all)) {
  L31 = TRUE;
 } else {
  L31 = FSETST1025458804(ATTR(when_chk_self,when_in), when_chk_n);
 }
 if (L31) {
  L32 = FSETST1025458804(ATTR(when_chk_self,when_out), when_chk_n);
  L331_=!(L32); 
  L30 = L331_;
 } else {
  L30 = FALSE;
 }
 ret_val7 = L30;
 SATTR(self,chk_when,ret_val7);
 return_chk_self = ATTR(self,options1);
 return_chk_n = n;
 if (ATTR(return_chk_self,return_all)) {
  L35 = TRUE;
 } else {
  L35 = FSETST1025458804(ATTR(return_chk_self,return_in), return_chk_n);
 }
 if (L35) {
  L36 = FSETST1025458804(ATTR(return_chk_self,return_out), return_chk_n);
  L371_=!(L36); 
  L34 = L371_;
 } else {
  L34 = FALSE;
 }
 ret_val8 = L34;
 SATTR(self,chk_return,ret_val8);
}
