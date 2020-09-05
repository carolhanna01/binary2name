#include "sather.h"

/* Layouts */

typedef struct dAM_EXPR_struct {
 OB_HEADER header;
 } *dAM_EXPR;

typedef struct dAM_STMT_struct {
 OB_HEADER header;
 } *dAM_STMT;

typedef struct dLAYOUT_struct {
 OB_HEADER header;
 } *dLAYOUT;

typedef struct dMODE_struct {
 OB_HEADER header;
 } *dMODE;

typedef struct dPROG_ERR_struct {
 OB_HEADER header;
 } *dPROG_ERR;

typedef struct dSTAT_struct {
 OB_HEADER header;
 } *dSTAT;

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

typedef struct ARRAYSTR_struct {/* layout for ARRAY{STR} */
 OB_HEADER header;
 INT asize;
 STR arr_part[1];
 } *ARRAYSTR;

typedef struct IDENT_struct {/* layout for IDENT */
 STR str;
 } IDENT;
static IDENT IDENT_zero;

typedef struct IDENT_boxed_struct {
 OB_HEADER header;
 IDENT immutable_part;
 } *IDENT_boxed;

typedef struct INOUT_MODE_struct {/* layout for INOUT_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *INOUT_MODE;

typedef struct INT_up418511718_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT arg1;/* Formal argument: i */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_I,I_u_Im; /* used by builtin iter */
 } *INT_up418511718_frame;

typedef struct INTI1_struct {/* layout for INTI */
 OB_HEADER header;
 INT len1;
 INT asize;
 INT arr_part[1];
 } *INTI1;

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

typedef struct OUT_MODE_struct {/* layout for OUT_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OUT_MODE;

typedef struct RAT_struct {/* layout for RAT */
 struct INTI1_struct *u;
 struct INTI1_struct *v;
 } RAT;
static RAT RAT_zero;

typedef struct RAT_boxed_struct {
 OB_HEADER header;
 RAT immutable_part;
 } *RAT_boxed;

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

typedef struct TUPSTRINT_struct {/* layout for TUP{STR,INT} */
 INT t2;
 STR t1;
 } TUPSTRINT;
static TUPSTRINT TUPSTRINT_zero;

typedef struct TUPSTRINT_boxed_struct {
 OB_HEADER header;
 TUPSTRINT immutable_part;
 } *TUPSTRINT_boxed;

typedef struct UNIX_struct {/* layout for UNIX */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *UNIX;

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

typedef struct AM_EXCEPT_EXPR_struct {/* layout for AM_EXCEPT_EXPR */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 } *AM_EXCEPT_EXPR;

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

typedef struct AM_FLTD_CONST_struct {/* layout for AM_FLTD_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct RAT_struct val1;
 struct SFILE_ID_struct source1;
 } *AM_FLTD_CONST;

typedef struct AM_FLT_CONST_struct {/* layout for AM_FLT_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct RAT_struct val1;
 struct SFILE_ID_struct source1;
 } *AM_FLT_CONST;

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

typedef struct AM_IF_STMT_struct {/* layout for AM_IF_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *test1;
 struct dAM_STMT_struct *if_false;
 struct dAM_STMT_struct *if_true;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_IF_STMT;

typedef struct AM_INT_CONST_struct {/* layout for AM_INT_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct INTI1_struct *val1;
 struct SFILE_ID_struct source1;
 } *AM_INT_CONST;

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

typedef struct AM_WHERE_EXPR_struct {/* layout for AM_WHERE_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *arg;
 struct dTP_struct *tp;
 struct SFILE_ID_struct source1;
 } *AM_WHERE_EXPR;

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

typedef struct FLISTA1409846210_struct {/* layout for FLIST{AM_BND_ITER_CALL_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_BND367211769_struct *arr_part[1];
 } *FLISTA1409846210;

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

typedef struct FLISTA1062334999_struct {/* layout for FLIST{AM_ITER_CALL_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_ITE1809135850_struct *arr_part[1];
 } *FLISTA1062334999;

typedef struct FLISTA20621028_frame_struct {
 INT state;
 FLISTA1062334999 self;/* Formal argument: self */
 AM_ITE1809135850 ret_val1;
 INT i;
 INT sz;
 FLISTA1062334999 aget_self;
 INT aget_ind;
 AM_ITE1809135850 ret_val;
 } *FLISTA20621028_frame;

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

typedef struct FMAPdOBNAMESPACE_struct {/* layout for FMAP{$OB,NAMESPACE} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdOBNAMESPACE_struct arr_part[1];
 } *FMAPdOBNAMESPACE;

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

typedef struct FMAPSTRINT_struct {/* layout for FMAP{STR,INT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRINT_struct arr_part[1];
 } *FMAPSTRINT;

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

extern FSTR INTI_buf;
extern INOUT_MODE MODES_inout_mode;
extern OUT_MODE MODES_out_mode;
extern TP_CLASS TP_BUI1032249067;
extern TP_CLASS TP_BUI1325635093;
extern TP_CLASS TP_BUI1805763264;
extern TP_CLASS TP_BUI2088674906;
extern TP_CLASS TP_BUI633637838;

/* Function declarations */


extern RETURNED_CONST BOOL (**dAM_EX1117610980)(dAM_EXPR);

extern RETURNED_CONST BOOL (**dMODE_814247358)(dMODE, dMODE);

extern RETURNED_CONST BOOL (**dTP_is1334578382)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1811059018)(dTP);

extern RETURNED_CONST BOOL (**dTP_is242312711)(dTP, OB);

extern RETURNED_CONST BOOL (**dTP_is418055720)(dTP);

extern RETURNED_CONST STR (**dSTR_strrSTR)(dSTR);

extern RETURNED_CONST dTP (**dAM_EXPR_tprdTP)(dAM_EXPR);

extern RETURNED_CONST void (**dSTAT_incr_STR)(dSTAT, STR);
AM_BND367211769 FLISTA1446310868(FLISTA1446310868_frame);
AM_ITE1809135850 FLISTA20621028(FLISTA20621028_frame);
AM_LOCAL_EXPR FLISTA2119642552(FLISTA2119642552_frame);
BOOL CGEN_i1595194353(CGEN, dAM_EXPR);
BOOL CGEN_s395856114(CGEN, AM_ROUT_DEF, STR);
BOOL SIG_is275586960(SIG);
BOOL SIG_is418491101(SIG);
BOOL SIG_is600379186(SIG);
FLISTA1062334999 FLISTA943034044(FLISTA1062334999, AM_ITE1809135850);
FLISTA1062334999 FMAPAM925529692(FMAPAM1787628573, AM_ROUT_DEF);
FLISTA1409846210 FLISTA1715286767(FLISTA1409846210, AM_BND367211769);
FLISTA1409846210 FMAPAM338804820(FMAPAM1244483504, AM_ROUT_DEF);
FMAPAM1244483504 FMAPAM1818297900(FMAPAM1244483504, AM_ROUT_DEF, FLISTA1409846210);
FMAPAM1787628573 FMAPAM293039037(FMAPAM1787628573, AM_ROUT_DEF, FLISTA1062334999);
FMAPdOBNAMESPACE FMAPdO1199466663(FMAPdOBNAMESPACE, OB, NAMESPACE);
FSETSIG FSETSI671175003(FSETSIG, SIG);
FSETSTR FSETST1404644833(FSETSTR, STR);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR INTI_s96356965(INTI1, FSTR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
INT FLISTA1962912942(FLISTA1062334999);
INT FLISTA2089597941(FLISTA1409846210);
INT INT_up418511718(INT_up418511718_frame);
INT STR_sizerINT(STR);
NAMESPACE FMAPdO790518483(FMAPdOBNAMESPACE, OB);
SE_CONTEXT SIG_ge642497605(SIG, PROG);
STR CGEN_d1425664957(CGEN, dTP);
STR CGEN_d1915593494(CGEN, dTP, dTP, STR, BOOL, dMODE);
STR CGEN_d815167520(CGEN, dTP);
STR CGEN_e1038326897(CGEN, AM_EXT_CALL_EXPR);
STR CGEN_e124364216(CGEN, AM_BOOL_CONST);
STR CGEN_e1345827062(CGEN, AM_ARRAY_EXPR);
STR CGEN_e134814882(CGEN, AM_ITE1809135850);
STR CGEN_e1445279685(CGEN, AM_STMT_EXPR);
STR CGEN_e162685546(CGEN, AM_CLUSTER_EXPR);
STR CGEN_e1633118543(CGEN, AM_STR_CONST);
STR CGEN_e1639504572(CGEN, dAM_EXPR);
STR CGEN_e1650375497(CGEN, AM_VAT319982528);
STR CGEN_e1818527256(CGEN, AM_BND367211769);
STR CGEN_e2038430067(CGEN, AM_IF_EXPR);
STR CGEN_e2079333002(CGEN, AM_WHERE_EXPR);
STR CGEN_e231282482(CGEN, AM_NEW_EXPR);
STR CGEN_e236145586(CGEN, AM_LOCAL_EXPR);
STR CGEN_e237424728(CGEN, AM_BND260301329);
STR CGEN_e453436338(CGEN, AM_ROU1916046290);
STR CGEN_e565064885(CGEN, AM_NEAR_EXPR);
STR CGEN_e604348447(CGEN, AM_SHARED_EXPR);
STR CGEN_e713597563(CGEN, AM_HERE_EXPR);
STR CGEN_e73452157(CGEN, AM_BND1124877163);
STR CGEN_e840009948(CGEN, AM_FAR_EXPR);
STR CGEN_e848933497(CGEN, AM_ATTR_EXPR);
STR CGEN_e887129228(CGEN, AM_IS_VOID_EXPR);
STR CGEN_e969629179(CGEN, AM_AT_EXPR);
STR CGEN_eolrSTR(CGEN);
STR CGEN_i87475608(CGEN);
STR MANGLE119219986(MANGLE, OB, OB);
STR MANGLE868076060(MANGLE, CHAR);
STR NAMESP180237388(NAMESPACE, STR);
STR NAMESP8552102(NAMESPACE, STR);
STR RAT_str_INTrSTR(RAT, INT);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap1182453786(STR, STR, STR, STR, STR, STR, STR);
STR STR_ap1417971546(STR, STR, STR, STR);
STR STR_ap1693864410(STR, STR, STR);
STR STR_ap1925291910(STR, STR, STR, STR, STR, STR);
STR STR_ap2004550586(STR, STR);
STR STR_ap44172742(STR, STR, STR, STR, STR);
STR STR_fr1097270334(STR, FSTR);
dLAYOUT LAYOUT79932797(LAYOUT_TBL, dTP);
void CGEN_b2123071570(CGEN, AM_SHARED_EXPR);
void CGEN_c9785167(CGEN);
void CGEN_d695831906(CGEN);
void CGEN_e101922820(CGEN, AM_RETURN_STMT);
void CGEN_e1197535054(CGEN, AM_LOOP_STMT);
void CGEN_e1389302728(CGEN, AM_ASSIGN_STMT);
void CGEN_e494460490(CGEN, dAM_STMT);
void CGEN_e638028220(CGEN, AM_IF_STMT);
void CGEN_e689545343(CGEN, AM_RETURN_STMT);
void CGEN_e844901491(CGEN, AM_ITE1809135850);
void CODE_F1291349628(CODE_FILE, SIG);
void CODE_F525364269(CODE_FILE, dLAYOUT);
void MANGLE1827205184(MANGLE, OB, STR, OB);
void PROG_e176405615(PROG, dPROG_ERR);
void PROG_err_STR(PROG, STR);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */




#undef IS_ITER
#define IS_ITER 0

BOOL CGEN_i1595194353(CGEN self, dAM_EXPR e) {
 BOOL ret_val = BOOL_zero;
 if (e==NULL) {
  goto other376;
 } else
 switch (TAG(e)) {
  case AM_LOCAL_EXPR_tag:
   ret_val = TRUE;
   return ret_val; break;
  case AM_SHARED_EXPR_tag:
   ret_val = TRUE;
   return ret_val; break;
  case AM_ATTR_EXPR_tag:
   ret_val = TRUE;
   return ret_val; break;
  default: ;
  other376: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL CGEN_s395856114(CGEN self, AM_ROUT_DEF rout_def, STR ret_val) {
 BOOL ret_val1 = BOOL_zero;
 SIG sig1;
 dTP L11;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN ndefer_self1;
 STR ndefer_s1;
 CGEN ndefer_self2;
 STR ndefer_s2;
 CGEN ndefer_self3;
 STR ndefer_s3;
 CGEN ndefer_self4;
 STR ndefer_s4;
 CGEN ndefer_self5;
 STR ndefer_s5;
 dTP L2;
 extern STR name58;
 ARRAYSTR L3;
 STR L41_;
 STR L5;
 extern STR name33;
 extern STR name16;
 dTP L6;
 extern STR name58;
 ARRAYSTR L7;
 STR L81_;
 STR L9;
 extern STR name33;
 extern STR name16;
 dTP L10;
 extern STR name58;
 ARRAYSTR L12;
 STR L131_;
 STR L14;
 extern STR name33;
 extern STR name16;
 ARRAYSTR L15;
 STR L161_;
 STR L17;
 extern STR name33;
 extern STR S1L1;
 dTP L18;
 extern STR F_STRING_COPY_TO;
 ARRAYSTR L19;
 STR L201_;
 STR L21;
 extern STR name3;
 extern STR address3;
 extern STR size4;
 ARRAYSTR L22;
 STR L231_;
 STR L24;
 extern STR name33;
 extern STR size5;
 sig1 = ATTR(rout_def,sig1);
 if ((ATTR(sig1,ret)==((dTP) NULL))) {
  ret_val1 = FALSE;
  return ret_val1;
 }
 else {
  L11 = ATTR(sig1,ret);
  L2 = L11;
  if ((*dTP_is242312711[TAG(L2)])(L2, ((OB) TP_BUI633637838))) {
   ndefer_self = self;
   L3 = ATTR(self,current_fortran_);
   L41_=(STR)ARR((ARRAYSTR)L3,0); 
   L5 = L41_;
   ndefer_s = STR_ap44172742(((STR) &name58), L5, ((STR) &name33), ret_val, ((STR) &name16));
   CGEN_d695831906(ndefer_self);
   SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
   ret_val1 = TRUE;
   return ret_val1;
  }
  else {
   L6 = L11;
   if ((*dTP_is242312711[TAG(L6)])(L6, ((OB) TP_BUI1032249067))) {
    ndefer_self1 = self;
    L7 = ATTR(self,current_fortran_);
    L81_=(STR)ARR((ARRAYSTR)L7,0); 
    L9 = L81_;
    ndefer_s1 = STR_ap44172742(((STR) &name58), L9, ((STR) &name33), ret_val, ((STR) &name16));
    CGEN_d695831906(ndefer_self1);
    SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
    ret_val1 = TRUE;
    return ret_val1;
   }
   else {
    L10 = L11;
    if ((*dTP_is242312711[TAG(L10)])(L10, ((OB) TP_BUI1805763264))) {
     ndefer_self2 = self;
     L12 = ATTR(self,current_fortran_);
     L131_=(STR)ARR((ARRAYSTR)L12,0); 
     L14 = L131_;
     ndefer_s2 = STR_ap44172742(((STR) &name58), L14, ((STR) &name33), ret_val, ((STR) &name16));
     CGEN_d695831906(ndefer_self2);
     SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
     ndefer_self3 = self;
     L15 = ATTR(self,current_fortran_);
     L161_=(STR)ARR((ARRAYSTR)L15,1); 
     L17 = L161_;
     ndefer_s3 = STR_ap1693864410(L17, ((STR) &name33), ((STR) &S1L1));
     CGEN_d695831906(ndefer_self3);
     SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
     ret_val1 = TRUE;
     return ret_val1;
    }
    else {
     L18 = L11;
     if ((*dTP_is242312711[TAG(L18)])(L18, ((OB) TP_BUI2088674906))) {
      ndefer_self4 = self;
      L19 = ATTR(self,current_fortran_);
      L201_=(STR)ARR((ARRAYSTR)L19,0); 
      L21 = L201_;
      ndefer_s4 = STR_ap1182453786(((STR) &F_STRING_COPY_TO), L21, ((STR) &name3), ret_val, ((STR) &address3), ret_val, ((STR) &size4));
      CGEN_d695831906(ndefer_self4);
      SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
      ndefer_self5 = self;
      L22 = ATTR(self,current_fortran_);
      L231_=(STR)ARR((ARRAYSTR)L22,1); 
      L24 = L231_;
      ndefer_s5 = STR_ap1417971546(L24, ((STR) &name33), ret_val, ((STR) &size5));
      CGEN_d695831906(ndefer_self5);
      SATTR(ndefer_self5,routine_code,FSTR_p1752847026(ATTR(ndefer_self5,routine_code), ndefer_s5));
      ret_val1 = TRUE;
      return ret_val1;
     }
    }
   }
  }
 }
 ret_val1 = FALSE;
 return ret_val1;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e162685546(CGEN self, AM_CLUSTER_EXPR arg) {
 STR ret_val;
 BOOL L1;
 BOOL L21_;
 extern STR S11;
 extern STR CLUSTERS1;
 L1 = ATTR(ATTR(self,prog),distributed);
 L21_=!(L1); 
 if (L21_) {
  ret_val = ((STR) &S11);
  return ret_val;
 }
 ret_val = ((STR) &CLUSTERS1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e1639504572(CGEN self, dAM_EXPR arg) {
 STR ret_val;
 dTP t;
 CODE_FILE uses_tp_self;
 dTP uses_tp_t;
 CGEN emit_am_void_con;
 AM_VOID_CONST emit_am_void_con1;
 STR ret_val1;
 AM_VOID_CONST tp_self;
 dTP ret_val2;
 CGEN emit_am_int_cons;
 AM_INT_CONST emit_am_int_cons1;
 STR ret_val3;
 INTI1 str_self;
 STR ret_val4;
 FSTR clear_self;
 FSTR str_self1;
 STR ret_val5;
 CGEN emit_am_char_con;
 AM_CHAR_CONST emit_am_char_con1;
 STR ret_val6;
 CGEN emit_am_flt_cons;
 AM_FLT_CONST emit_am_flt_cons1;
 STR ret_val7;
 CGEN emit_am_fltd_con;
 AM_FLTD_CONST emit_am_fltd_con1;
 STR ret_val8;
 CGEN emit_am_except_e;
 AM_EXCEPT_EXPR emit_am_except_e1;
 STR ret_val9;
 AM_EXCEPT_EXPR tp_self1;
 dTP ret_val10;
 CGEN cast_self;
 dTP cast_dest_tp;
 dTP cast_src_tp;
 STR cast_expr;
 BOOL cast_only_boxing = BOOL_zero;
 STR ret_val11;
 CGEN emit_am_any_expr;
 AM_ANY_EXPR emit_am_any_expr1;
 STR ret_val12;
 CGEN emit_am_cluster_;
 AM_CLU1286269335 emit_am_cluster_1;
 STR ret_val13;
 CGEN emit_am_global_e;
 AM_GLOBAL_EXPR emit_am_global_e1;
 STR ret_val14;
 CGEN barf_at_self;
 STR barf_at_msg;
 dPROG_ERR barf_at_at;
 PROG barf_at_self1;
 STR barf_at_msg1;
 dPROG_ERR barf_at_at1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val15;
 UNIX exit_self;
 INT exit_code = INT_zero;
 CGEN barf_at_self2;
 STR barf_at_msg2;
 dPROG_ERR barf_at_at2;
 PROG barf_at_self3;
 STR barf_at_msg3;
 dPROG_ERR barf_at_at3;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val16;
 UNIX exit_self1;
 INT exit_code1 = INT_zero;
 CGEN barf_at_self4;
 STR barf_at_msg4;
 dPROG_ERR barf_at_at4;
 PROG barf_at_self5;
 STR barf_at_msg5;
 dPROG_ERR barf_at_at5;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val17;
 UNIX exit_self2;
 INT exit_code2 = INT_zero;
 CGEN barf_at_self6;
 STR barf_at_msg6;
 dPROG_ERR barf_at_at6;
 PROG barf_at_self7;
 STR barf_at_msg7;
 dPROG_ERR barf_at_at7;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val18;
 UNIX exit_self3;
 INT exit_code3 = INT_zero;
 CGEN barf_at_self8;
 STR barf_at_msg8;
 dPROG_ERR barf_at_at8;
 PROG barf_at_self9;
 STR barf_at_msg9;
 dPROG_ERR barf_at_at9;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val19;
 UNIX exit_self4;
 INT exit_code4 = INT_zero;
 CGEN barf_self;
 STR barf_msg;
 PROG barf_self1;
 STR barf_msg1;
 PROG barf_at_self10;
 STR barf_at_msg10;
 dPROG_ERR barf_at_at10;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val20;
 UNIX exit_self5;
 INT exit_code5 = INT_zero;
 dAM_EXPR L1;
 BOOL L2;
 BOOL L31_;
 BOOL L4;
 BOOL L51_;
 extern STR name42;
 STR L6;
 extern STR name42;
 extern STR EXCEPTION1;
 extern STR ANY1;
 extern STR MY_CLUSTER_SIZE1;
 extern STR SI_global1;
 extern STR consta1495253793;
 extern STR Intern28965746;
 extern STR INTIli1551441099;
 extern STR Intern28965746;
 extern STR FLTIco1160577432;
 extern STR Intern28965746;
 extern STR FLTXli793215367;
 extern STR Intern28965746;
 extern STR FLTDXl660293550;
 extern STR Intern28965746;
 extern STR Gottoe704218937;
 extern STR Intern28965746;
 extern STR name1;
 L1 = arg;
 t = (*dAM_EXPR_tprdTP[TAG(L1)])(L1);
 L2 = (t==((dTP) NULL));
 L31_=!(L2); 
 if (L31_) {
  uses_tp_self = ATTR(self,code_c);
  uses_tp_t = t;
  CODE_F525364269(uses_tp_self, LAYOUT79932797(((LAYOUT_TBL) NULL), uses_tp_t));
 }
 switch (TAG(arg)) {
  case AM_LOCAL_EXPR_tag:
   ret_val = CGEN_e236145586(self, ((AM_LOCAL_EXPR) arg));
   return ret_val; break;
  case AM_ROU1916046290_tag:
   ret_val = CGEN_e453436338(self, ((AM_ROU1916046290) arg));
   return ret_val; break;
  case AM_ITE1809135850_tag:
   ret_val = CGEN_e134814882(self, ((AM_ITE1809135850) arg));
   return ret_val; break;
  case AM_VOID_CONST_tag:
   emit_am_void_con = self;
   emit_am_void_con1 = ((AM_VOID_CONST) arg);
   tp_self = emit_am_void_con1;
   ret_val2 = ATTR(tp_self,tp_at);
   ret_val1 = CGEN_d815167520(emit_am_void_con, ret_val2);
   ret_val = ret_val1;
   return ret_val; break;
  case AM_STR_CONST_tag:
   ret_val = CGEN_e1633118543(self, ((AM_STR_CONST) arg));
   return ret_val; break;
  case AM_BOOL_CONST_tag:
   ret_val = CGEN_e124364216(self, ((AM_BOOL_CONST) arg));
   return ret_val; break;
  case AM_INT_CONST_tag:
   emit_am_int_cons = self;
   emit_am_int_cons1 = ((AM_INT_CONST) arg);
   str_self = ATTR(emit_am_int_cons1,val1);
   clear_self = INTI_buf;
   L4 = (clear_self==((FSTR) NULL));
   L51_=!(L4); 
   if (L51_) {
    SATTR(clear_self,loc,0);
   }
   INTI_buf = INTI_s96356965(str_self, INTI_buf);
   str_self1 = INTI_buf;
   ret_val5 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val4 = ret_val5;
   ret_val3 = ret_val4;
   ret_val = ret_val3;
   return ret_val; break;
  case AM_CHAR_CONST_tag:
   emit_am_char_con = self;
   emit_am_char_con1 = ((AM_CHAR_CONST) arg);
   L6 = MANGLE868076060(ATTR(emit_am_char_con,mangler), ATTR(emit_am_char_con1,bval));
   ret_val6 = STR_ap1693864410(((STR) &name42), L6, ((STR) &name42));
   ret_val = ret_val6;
   return ret_val; break;
  case AM_FLT_CONST_tag:
   emit_am_flt_cons = self;
   emit_am_flt_cons1 = ((AM_FLT_CONST) arg);
   ret_val7 = RAT_str_INTrSTR(ATTR(emit_am_flt_cons1,val1), 8);
   ret_val = ret_val7;
   return ret_val; break;
  case AM_FLTD_CONST_tag:
   emit_am_fltd_con = self;
   emit_am_fltd_con1 = ((AM_FLTD_CONST) arg);
   ret_val8 = RAT_str_INTrSTR(ATTR(emit_am_fltd_con1,val1), 17);
   ret_val = ret_val8;
   return ret_val; break;
  case AM_IF_EXPR_tag:
   ret_val = CGEN_e2038430067(self, ((AM_IF_EXPR) arg));
   return ret_val; break;
  case AM_NEW_EXPR_tag:
   ret_val = CGEN_e231282482(self, ((AM_NEW_EXPR) arg));
   return ret_val; break;
  case AM_ATTR_EXPR_tag:
   ret_val = CGEN_e848933497(self, ((AM_ATTR_EXPR) arg));
   return ret_val; break;
  case AM_VAT319982528_tag:
   ret_val = CGEN_e1650375497(self, ((AM_VAT319982528) arg));
   return ret_val; break;
  case AM_EXT_CALL_EXPR_tag:
   ret_val = CGEN_e1038326897(self, ((AM_EXT_CALL_EXPR) arg));
   return ret_val; break;
  case AM_SHARED_EXPR_tag:
   ret_val = CGEN_e604348447(self, ((AM_SHARED_EXPR) arg));
   return ret_val; break;
  case AM_ARRAY_EXPR_tag:
   ret_val = CGEN_e1345827062(self, ((AM_ARRAY_EXPR) arg));
   return ret_val; break;
  case AM_IS_VOID_EXPR_tag:
   ret_val = CGEN_e887129228(self, ((AM_IS_VOID_EXPR) arg));
   return ret_val; break;
  case AM_STMT_EXPR_tag:
   ret_val = CGEN_e1445279685(self, ((AM_STMT_EXPR) arg));
   return ret_val; break;
  case AM_EXCEPT_EXPR_tag:
   emit_am_except_e = self;
   emit_am_except_e1 = ((AM_EXCEPT_EXPR) arg);
   cast_self = emit_am_except_e;
   tp_self1 = emit_am_except_e1;
   ret_val10 = ATTR(tp_self1,tp_at);
   cast_dest_tp = ret_val10;
   cast_src_tp = ((dTP) TP_BUI1325635093);
   cast_expr = ((STR) &EXCEPTION1);
   cast_only_boxing = FALSE;
   ret_val11 = CGEN_d1915593494(cast_self, cast_dest_tp, cast_src_tp, cast_expr, cast_only_boxing, ((dMODE) NULL));
   ret_val9 = ret_val11;
   ret_val = ret_val9;
   return ret_val; break;
  case AM_BND1124877163_tag:
   ret_val = CGEN_e73452157(self, ((AM_BND1124877163) arg));
   return ret_val; break;
  case AM_BND260301329_tag:
   ret_val = CGEN_e237424728(self, ((AM_BND260301329) arg));
   return ret_val; break;
  case AM_BND367211769_tag:
   ret_val = CGEN_e1818527256(self, ((AM_BND367211769) arg));
   return ret_val; break;
  case AM_HERE_EXPR_tag:
   ret_val = CGEN_e713597563(self, ((AM_HERE_EXPR) arg));
   return ret_val; break;
  case AM_ANY_EXPR_tag:
   emit_am_any_expr = self;
   emit_am_any_expr1 = ((AM_ANY_EXPR) arg);
   ret_val12 = ((STR) &ANY1);
   ret_val = ret_val12;
   return ret_val; break;
  case AM_CLUSTER_EXPR_tag:
   ret_val = CGEN_e162685546(self, ((AM_CLUSTER_EXPR) arg));
   return ret_val; break;
  case AM_CLU1286269335_tag:
   emit_am_cluster_ = self;
   emit_am_cluster_1 = ((AM_CLU1286269335) arg);
   ret_val13 = ((STR) &MY_CLUSTER_SIZE1);
   ret_val = ret_val13;
   return ret_val; break;
  case AM_WHERE_EXPR_tag:
   ret_val = CGEN_e2079333002(self, ((AM_WHERE_EXPR) arg));
   return ret_val; break;
  case AM_NEAR_EXPR_tag:
   ret_val = CGEN_e565064885(self, ((AM_NEAR_EXPR) arg));
   return ret_val; break;
  case AM_FAR_EXPR_tag:
   ret_val = CGEN_e840009948(self, ((AM_FAR_EXPR) arg));
   return ret_val; break;
  case AM_AT_EXPR_tag:
   ret_val = CGEN_e969629179(self, ((AM_AT_EXPR) arg));
   return ret_val; break;
  case AM_GLOBAL_EXPR_tag:
   emit_am_global_e = self;
   emit_am_global_e1 = ((AM_GLOBAL_EXPR) arg);
   ret_val14 = ((STR) &SI_global1);
   ret_val = ret_val14;
   return ret_val; break;
  case AM_ARR_CONST_tag:
   barf_at_self = self;
   barf_at_msg = ((STR) &consta1495253793);
   barf_at_at = ((dPROG_ERR) arg);
   barf_at_self1 = ATTR(barf_at_self,prog);
   barf_at_msg1 = barf_at_msg;
   barf_at_at1 = barf_at_at;
   PROG_e176405615(barf_at_self1, barf_at_at1);
   plus_self = ((STR) &Intern28965746);
   plus_sarg = barf_at_msg1;
   ret_val15 = STR_ap2004550586(plus_self, plus_sarg);
   PROG_err_STR(barf_at_self1, ret_val15);
   exit_self = ((UNIX) NULL);
   exit_code = 1;
   exit(exit_code); break;
  case AM_INTI_CONST_tag:
   barf_at_self2 = self;
   barf_at_msg2 = ((STR) &INTIli1551441099);
   barf_at_at2 = ((dPROG_ERR) arg);
   barf_at_self3 = ATTR(barf_at_self2,prog);
   barf_at_msg3 = barf_at_msg2;
   barf_at_at3 = barf_at_at2;
   PROG_e176405615(barf_at_self3, barf_at_at3);
   plus_self1 = ((STR) &Intern28965746);
   plus_sarg1 = barf_at_msg3;
   ret_val16 = STR_ap2004550586(plus_self1, plus_sarg1);
   PROG_err_STR(barf_at_self3, ret_val16);
   exit_self1 = ((UNIX) NULL);
   exit_code1 = 1;
   exit(exit_code1); break;
  case AM_FLTI_CONST_tag:
   barf_at_self4 = self;
   barf_at_msg4 = ((STR) &FLTIco1160577432);
   barf_at_at4 = ((dPROG_ERR) arg);
   barf_at_self5 = ATTR(barf_at_self4,prog);
   barf_at_msg5 = barf_at_msg4;
   barf_at_at5 = barf_at_at4;
   PROG_e176405615(barf_at_self5, barf_at_at5);
   plus_self2 = ((STR) &Intern28965746);
   plus_sarg2 = barf_at_msg5;
   ret_val17 = STR_ap2004550586(plus_self2, plus_sarg2);
   PROG_err_STR(barf_at_self5, ret_val17);
   exit_self2 = ((UNIX) NULL);
   exit_code2 = 1;
   exit(exit_code2); break;
  case AM_FLTX_CONST_tag:
   barf_at_self6 = self;
   barf_at_msg6 = ((STR) &FLTXli793215367);
   barf_at_at6 = ((dPROG_ERR) arg);
   barf_at_self7 = ATTR(barf_at_self6,prog);
   barf_at_msg7 = barf_at_msg6;
   barf_at_at7 = barf_at_at6;
   PROG_e176405615(barf_at_self7, barf_at_at7);
   plus_self3 = ((STR) &Intern28965746);
   plus_sarg3 = barf_at_msg7;
   ret_val18 = STR_ap2004550586(plus_self3, plus_sarg3);
   PROG_err_STR(barf_at_self7, ret_val18);
   exit_self3 = ((UNIX) NULL);
   exit_code3 = 1;
   exit(exit_code3); break;
  case AM_FLTDX_CONST_tag:
   barf_at_self8 = self;
   barf_at_msg8 = ((STR) &FLTDXl660293550);
   barf_at_at8 = ((dPROG_ERR) arg);
   barf_at_self9 = ATTR(barf_at_self8,prog);
   barf_at_msg9 = barf_at_msg8;
   barf_at_at9 = barf_at_at8;
   PROG_e176405615(barf_at_self9, barf_at_at9);
   plus_self4 = ((STR) &Intern28965746);
   plus_sarg4 = barf_at_msg9;
   ret_val19 = STR_ap2004550586(plus_self4, plus_sarg4);
   PROG_err_STR(barf_at_self9, ret_val19);
   exit_self4 = ((UNIX) NULL);
   exit_code4 = 1;
   exit(exit_code4); break;
  default: ;
   FATAL("No applicable type in typecase\nin CGEN::emit_expr($AM_EXPR):STR\n./Back/cgen.sa:5371:14");
 }
 barf_self = self;
 barf_msg = ((STR) &Gottoe704218937);
 barf_self1 = ATTR(barf_self,prog);
 barf_msg1 = barf_msg;
 barf_at_self10 = barf_self1;
 barf_at_msg10 = barf_msg1;
 barf_at_at10 = ((dPROG_ERR) NULL);
 PROG_e176405615(barf_at_self10, barf_at_at10);
 plus_self5 = ((STR) &Intern28965746);
 plus_sarg5 = barf_at_msg10;
 ret_val20 = STR_ap2004550586(plus_self5, plus_sarg5);
 PROG_err_STR(barf_at_self10, ret_val20);
 exit_self5 = ((UNIX) NULL);
 exit_code5 = 1;
 exit(exit_code5);
 ret_val = ((STR) &name1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e2079333002(CGEN self, AM_WHERE_EXPR arg) {
 STR ret_val;
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
 extern STR SI_WHERE1;
 extern STR name26;
 BOOL L1;
 BOOL L21_;
 extern STR S01;
 dAM_EXPR L3;
 dTP L4;
 extern STR HERE1;
 dAM_EXPR L5;
 dTP L6;
 extern STR PS_WHERE;
 extern STR name26;
 extern STR PSR_WHERE;
 extern STR name26;
 if (ATTR(ATTR(self,prog),zones)) {
  plus_self = ((STR) &SI_WHERE1);
  plus_sarg = CGEN_e1639504572(self, ATTR(arg,arg));
  ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val1;
  plus_sarg1 = ((STR) &name26);
  ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
  ret_val = ret_val2;
  return ret_val;
 }
 else {
  L1 = ATTR(ATTR(self,prog),distributed);
  L21_=!(L1); 
  if (L21_) {
   ret_val = ((STR) &S01);
   return ret_val;
  }
  L3 = ATTR(arg,arg);
  L4 = (*dAM_EXPR_tprdTP[TAG(L3)])(L3);
  if ((*dTP_is1334578382[TAG(L4)])(L4)) {
   ret_val = ((STR) &HERE1);
   return ret_val;
  }
  L5 = ATTR(arg,arg);
  L6 = (*dAM_EXPR_tprdTP[TAG(L5)])(L5);
  if ((*dTP_is1811059018[TAG(L6)])(L6)) {
   plus_self2 = ((STR) &PS_WHERE);
   plus_sarg2 = CGEN_e1639504572(self, ATTR(arg,arg));
   ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
   plus_self3 = ret_val3;
   plus_sarg3 = ((STR) &name26);
   ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
   ret_val = ret_val4;
   return ret_val;
  }
  else {
   plus_self4 = ((STR) &PSR_WHERE);
   plus_sarg4 = CGEN_e1639504572(self, ATTR(arg,arg));
   ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
   plus_self5 = ret_val5;
   plus_sarg5 = ((STR) &name26);
   ret_val6 = STR_ap2004550586(plus_self5, plus_sarg5);
   ret_val = ret_val6;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e565064885(CGEN self, AM_NEAR_EXPR arg) {
 STR ret_val;
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
 dAM_EXPR L1;
 dTP L2;
 extern STR S01;
 BOOL L3;
 BOOL L41_;
 extern STR S11;
 dAM_EXPR L5;
 dTP L6;
 extern STR PS_NEAR;
 extern STR name26;
 extern STR PSR_NEAR;
 extern STR name26;
 L1 = ATTR(arg,arg);
 L2 = (*dAM_EXPR_tprdTP[TAG(L1)])(L1);
 if ((*dTP_is1334578382[TAG(L2)])(L2)) {
  ret_val = ((STR) &S01);
  return ret_val;
 }
 L3 = ATTR(ATTR(self,prog),distributed);
 L41_=!(L3); 
 if (L41_) {
  ret_val = ((STR) &S11);
  return ret_val;
 }
 L5 = ATTR(arg,arg);
 L6 = (*dAM_EXPR_tprdTP[TAG(L5)])(L5);
 if ((*dTP_is1811059018[TAG(L6)])(L6)) {
  plus_self = ((STR) &PS_NEAR);
  plus_sarg = CGEN_e1639504572(self, ATTR(arg,arg));
  ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val1;
  plus_sarg1 = ((STR) &name26);
  ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
  ret_val = ret_val2;
  return ret_val;
 }
 else {
  plus_self2 = ((STR) &PSR_NEAR);
  plus_sarg2 = CGEN_e1639504572(self, ATTR(arg,arg));
  ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
  plus_self3 = ret_val3;
  plus_sarg3 = ((STR) &name26);
  ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
  ret_val = ret_val4;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e713597563(CGEN self, AM_HERE_EXPR arg) {
 STR ret_val;
 extern STR SI_HERE2;
 BOOL L1;
 BOOL L21_;
 extern STR S01;
 extern STR HERE1;
 if (ATTR(ATTR(self,prog),zones)) {
  ret_val = ((STR) &SI_HERE2);
  return ret_val;
 }
 else {
  L1 = ATTR(ATTR(self,prog),distributed);
  L21_=!(L1); 
  if (L21_) {
   ret_val = ((STR) &S01);
   return ret_val;
  }
  ret_val = ((STR) &HERE1);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e840009948(CGEN self, AM_FAR_EXPR arg) {
 STR ret_val;
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
 dAM_EXPR L1;
 dTP L2;
 extern STR S01;
 BOOL L3;
 BOOL L41_;
 extern STR S01;
 dAM_EXPR L5;
 dTP L6;
 extern STR PS_FAR;
 extern STR name26;
 extern STR PSR_FAR;
 extern STR name26;
 L1 = ATTR(arg,arg);
 L2 = (*dAM_EXPR_tprdTP[TAG(L1)])(L1);
 if ((*dTP_is1334578382[TAG(L2)])(L2)) {
  ret_val = ((STR) &S01);
  return ret_val;
 }
 L3 = ATTR(ATTR(self,prog),distributed);
 L41_=!(L3); 
 if (L41_) {
  ret_val = ((STR) &S01);
  return ret_val;
 }
 L5 = ATTR(arg,arg);
 L6 = (*dAM_EXPR_tprdTP[TAG(L5)])(L5);
 if ((*dTP_is1811059018[TAG(L6)])(L6)) {
  plus_self = ((STR) &PS_FAR);
  plus_sarg = CGEN_e1639504572(self, ATTR(arg,arg));
  ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val1;
  plus_sarg1 = ((STR) &name26);
  ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
  ret_val = ret_val2;
  return ret_val;
 }
 else {
  plus_self2 = ((STR) &PSR_FAR);
  plus_sarg2 = CGEN_e1639504572(self, ATTR(arg,arg));
  ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
  plus_self3 = ret_val3;
  plus_sarg3 = ((STR) &name26);
  ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
  ret_val = ret_val4;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_c9785167(CGEN self) {
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 FLISTA725283029 L31;
 AM_LOCAL_EXPR l;
 INT arg_pos = INT_zero;
 AM_ROUT_DEF is_iter_self;
 BOOL ret_val = BOOL_zero;
 SIG is_iter_self1;
 BOOL ret_val1 = BOOL_zero;
 IDENT is_iter_self2 = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val4;
 INT str_self = INT_zero;
 STR ret_val5;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val6;
 FSTR str_self1;
 STR ret_val7;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val8;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val9;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val10;
 CGEN mang_self;
 OB mang_ob;
 OB mang_ns;
 STR ret_val11;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val12;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val13;
 CGEN ndefer_self;
 STR ndefer_s;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val14;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val15;
 CGEN mang_self1;
 OB mang_ob1;
 OB mang_ns1;
 STR ret_val16;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val17;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val18;
 CGEN ndefer_self1;
 STR ndefer_s1;
 INT L5;
 AM_ROUT_DEF L6;
 INT L71_;
 INT L8;
 INT L91_;
 INT L41_,L41_m;
 BOOL L10;
 AM_ROUT_DEF L12;
 AM_FORMAL_ARG L131_;
 AM_FORMAL_ARG L14;
 dMODE L15;
 AM_ROUT_DEF L16;
 AM_FORMAL_ARG L171_;
 AM_FORMAL_ARG L18;
 dMODE L19;
 BOOL L20;
 BOOL L22;
 BOOL L231_;
 STR L24;
 INT L25;
 INT L261_;
 INT L27;
 CHAR L281_;
 CHAR L29;
 BOOL L301_;
 AM_LOCAL_EXPR L32;
 extern STR name58;
 extern STR frame1;
 extern STR arg21;
 BOOL L33;
 BOOL L341_;
 extern STR name36;
 extern STR frame1;
 extern STR name16;
 BOOL L35;
 INT L361_;
 extern STR name58;
 ARRAYSTR L37;
 STR L381_;
 extern STR name36;
 AM_ROUT_DEF L39;
 AM_FORMAL_ARG L401_;
 AM_FORMAL_ARG L42;
 extern STR name16;
 {
  BOOL f_L41_ = TRUE;
  struct FLISTA2119642552_frame_struct other1_0;
FLISTA2119642552_frame noname1 = &other1_0;
  noname1->state = 0;
  while (1) {
   if (f_L41_) {
    f_L41_ = FALSE;
    L11 = 1;
    L6 = ATTR(self,current_am_rout_);
    L71_=(L6)==NULL?0:ASIZE((AM_ROUT_DEF)L6); 
    L8 = L71_;
    L91_=INTMINUS(L8,1); 
    L21 = L91_;
    L41_=L11-1;L41_m=L21; 
   }
   if(L41_++>=L41_m)  goto after_loop; 
   i = L41_;
   L12 = ATTR(self,current_am_rout_);
   L131_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)L12,i); 
   L14=L131_;
   L15 = ATTR(L14,mode1);
   if ((*dMODE_814247358[TAG(L15)])(L15, ((dMODE) MODES_inout_mode))) {
    L10 = TRUE;
   } else {
    L16 = ATTR(self,current_am_rout_);
    L171_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)L16,i); 
    L18=L171_;
    L19 = ATTR(L18,mode1);
    L10 = (*dMODE_814247358[TAG(L19)])(L19, ((dMODE) MODES_out_mode));
   }
   if (L10) {
    is_iter_self = ATTR(self,current_am_rout_);
    is_iter_self1 = ATTR(is_iter_self,sig1);
    is_iter_self2 = ATTR(is_iter_self1,name1);
    L22 = (is_iter_self2.str==((STR) NULL));
    L231_=!(L22); 
    if (L231_) {
     L24 = is_iter_self2.str;
     L25 = STR_sizerINT(is_iter_self2.str);
     L261_=INTMINUS(L25,1); 
     L27 = L261_;
     L281_=ARR((STR)L24,L27); 
     L29 = L281_;
     L301_=L29=='!'; 
     L20 = L301_;
    } else {
     L20 = FALSE;
    }
    ret_val2 = L20;
    ret_val1 = ret_val2;
    ret_val = ret_val1;
    if (ret_val) {
     if (noname1->state == 0) {
      L31 = ATTR(self,current_iter_out);
      noname1->self = L31;
     }
     L32 = FLISTA2119642552(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     l = L32;
     ndefer_self = self;
     plus_self = ((STR) &name58);
     plus_sarg = ((STR) &frame1);
     ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
     plus_self1 = ret_val3;
     plus_sarg1 = ((STR) &arg21);
     ret_val4 = STR_ap2004550586(plus_self1, plus_sarg1);
     plus_self2 = ret_val4;
     str_self = i;
     clear_self = buf1;
     L33 = (clear_self==((FSTR) NULL));
     L341_=!(L33); 
     if (L341_) {
      SATTR(clear_self,loc,0);
     }
     str_in_self = str_self;
     str_in_s = buf1;
     ret_val6 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
     buf1 = ret_val6;
     str_self1 = buf1;
     ret_val7 = STR_fr1097270334(((STR) NULL), str_self1);
     ret_val5 = ret_val7;
     plus_sarg2 = ret_val5;
     ret_val8 = STR_ap2004550586(plus_self2, plus_sarg2);
     plus_self3 = ret_val8;
     plus_sarg3 = ((STR) &name36);
     ret_val9 = STR_ap2004550586(plus_self3, plus_sarg3);
     plus_self4 = ret_val9;
     plus_sarg4 = ((STR) &frame1);
     ret_val10 = STR_ap2004550586(plus_self4, plus_sarg4);
     plus_self5 = ret_val10;
     mang_self = self;
     mang_ob = ((OB) l);
     mang_ns = ((OB) ATTR(ATTR(self,current_am_rout_),sig1));
     ret_val11 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, mang_ns);
     plus_sarg5 = ret_val11;
     ret_val12 = STR_ap2004550586(plus_self5, plus_sarg5);
     plus_self6 = ret_val12;
     plus_sarg6 = ((STR) &name16);
     ret_val13 = STR_ap2004550586(plus_self6, plus_sarg6);
     ndefer_s = ret_val13;
     CGEN_d695831906(ndefer_self);
     SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
    }
    else {
     if (SIG_is275586960(ATTR(ATTR(self,current_am_rout_),sig1))) {
      L35 = TRUE;
     } else {
      L35 = SIG_is600379186(ATTR(ATTR(self,current_am_rout_),sig1));
     }
     if (L35) {
      L361_=INTMINUS(i,1); 
      arg_pos = L361_;
     }
     else {
      arg_pos = i;
     }
     ndefer_self1 = self;
     plus_self7 = ((STR) &name58);
     L37 = ATTR(self,current_arg_list);
     L381_=(STR)ARR((ARRAYSTR)L37,arg_pos); 
     plus_sarg7 = L381_;
     ret_val14 = STR_ap2004550586(plus_self7, plus_sarg7);
     plus_self8 = ret_val14;
     plus_sarg8 = ((STR) &name36);
     ret_val15 = STR_ap2004550586(plus_self8, plus_sarg8);
     plus_self9 = ret_val15;
     mang_self1 = self;
     L39 = ATTR(self,current_am_rout_);
     L401_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)L39,i); 
     L42=L401_;
     mang_ob1 = ((OB) ATTR(L42,expr));
     mang_ns1 = ((OB) ATTR(ATTR(self,current_am_rout_),sig1));
     ret_val16 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, mang_ns1);
     plus_sarg9 = ret_val16;
     ret_val17 = STR_ap2004550586(plus_self9, plus_sarg9);
     plus_self10 = ret_val17;
     plus_sarg10 = ((STR) &name16);
     ret_val18 = STR_ap2004550586(plus_self10, plus_sarg10);
     ndefer_s1 = ret_val18;
     CGEN_d695831906(ndefer_self1);
     SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
    }
   }
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e101922820(CGEN self, AM_RETURN_STMT arg) {
 STR s1;
 SIG is_iter_self;
 BOOL ret_val = BOOL_zero;
 IDENT is_iter_self1 = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN ndefer_self1;
 STR ndefer_s1;
 CGEN pop_exceptions_s;
 INT pop_exceptions_n = INT_zero;
 INT L11 = INT_zero;
 CGEN ndefer_self2;
 STR ndefer_s2;
 INT str_self = INT_zero;
 STR ret_val2;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val3;
 FSTR str_self1;
 STR ret_val4;
 CGEN ndefer_self3;
 STR ndefer_s3;
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val6;
 CGEN ndefer_self4;
 STR ndefer_s4;
 CGEN cast_self;
 dTP cast_dest_tp;
 dTP cast_src_tp;
 STR cast_expr;
 BOOL cast_only_boxing = BOOL_zero;
 STR ret_val7;
 CGEN ndefer_self5;
 STR ndefer_s5;
 CGEN ndefer_self6;
 STR ndefer_s6;
 BOOL L2;
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
 extern STR framestate1;
 extern STR LM_LOC1545209594;
 extern STR POP_EXCEPTION12;
 extern STR POP_EXCEPTION11;
 BOOL L14;
 BOOL L151_;
 extern STR name76;
 BOOL L16;
 BOOL L171_;
 extern STR PREFET1824934344;
 extern STR name76;
 BOOL L18;
 BOOL L191_;
 extern STR return4;
 dAM_EXPR L20;
 extern STR name16;
 extern STR return5;
 INT L21;
 BOOL L221_;
 if (ATTR(ATTR(self,current_am_rout_),is_external)) {
  L2 = SIG_is275586960(ATTR(ATTR(self,current_am_rout_),sig1));
 } else {
  L2 = FALSE;
 }
 if (L2) {
  CGEN_e689545343(self, arg);
  return;
 }
 is_iter_self = ATTR(self,current_sig);
 is_iter_self1 = ATTR(is_iter_self,name1);
 L4 = (is_iter_self1.str==((STR) NULL));
 L51_=!(L4); 
 if (L51_) {
  L6 = is_iter_self1.str;
  L7 = STR_sizerINT(is_iter_self1.str);
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
 ret_val = ret_val1;
 if (ret_val) {
  ndefer_self = self;
  ndefer_s = ((STR) &framestate1);
  CGEN_d695831906(ndefer_self);
  SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
 }
 if (ATTR(self,manual_unlock)) {
  ndefer_self1 = self;
  ndefer_s1 = ((STR) &LM_LOC1545209594);
  CGEN_d695831906(ndefer_self1);
  SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
 }
 pop_exceptions_s = self;
 pop_exceptions_n = ATTR(self,ex_nesting);
 L11 = pop_exceptions_n;
 switch (L11) {
  case 0: 
   break;
  case 1: 
   ndefer_self2 = pop_exceptions_s;
   ndefer_s2 = ((STR) &POP_EXCEPTION12);
   CGEN_d695831906(ndefer_self2);
   SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
   break;
  default: ;
   ndefer_self3 = pop_exceptions_s;
   str_self = pop_exceptions_n;
   clear_self = buf1;
   L14 = (clear_self==((FSTR) NULL));
   L151_=!(L14); 
   if (L151_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val3;
   str_self1 = buf1;
   ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val2 = ret_val4;
   ndefer_s3 = STR_ap1693864410(((STR) &POP_EXCEPTION11), ret_val2, ((STR) &name76));
   CGEN_d695831906(ndefer_self3);
   SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
 }
 CGEN_c9785167(self);
 L16 = (ATTR(ATTR(self,current_am_rout_),specul_prefetch)==((AM_LOCAL_EXPR) NULL));
 L171_=!(L16); 
 if (L171_) {
  ndefer_self4 = self;
  plus_self = ((STR) &PREFET1824934344);
  plus_sarg = CGEN_e1639504572(self, ((dAM_EXPR) ATTR(ATTR(self,current_am_rout_),specul_prefetch)));
  ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val5;
  plus_sarg1 = ((STR) &name76);
  ret_val6 = STR_ap2004550586(plus_self1, plus_sarg1);
  ndefer_s4 = ret_val6;
  CGEN_d695831906(ndefer_self4);
  SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
 }
 L18 = (ATTR(arg,val1)==((dAM_EXPR) NULL));
 L191_=!(L18); 
 if (L191_) {
  s1 = CGEN_e1639504572(self, ATTR(arg,val1));
  ndefer_self5 = self;
  cast_self = self;
  cast_dest_tp = ATTR(ATTR(self,current_sig),ret);
  L20 = ATTR(arg,val1);
  cast_src_tp = (*dAM_EXPR_tprdTP[TAG(L20)])(L20);
  cast_expr = s1;
  cast_only_boxing = FALSE;
  ret_val7 = CGEN_d1915593494(cast_self, cast_dest_tp, cast_src_tp, cast_expr, cast_only_boxing, ((dMODE) NULL));
  ndefer_s5 = STR_ap1693864410(((STR) &return4), ret_val7, ((STR) &name16));
  CGEN_d695831906(ndefer_self5);
  SATTR(ndefer_self5,routine_code,FSTR_p1752847026(ATTR(ndefer_self5,routine_code), ndefer_s5));
 }
 else {
  ndefer_self6 = self;
  ndefer_s6 = ((STR) &return5);
  CGEN_d695831906(ndefer_self6);
  SATTR(ndefer_self6,routine_code,FSTR_p1752847026(ATTR(ndefer_self6,routine_code), ndefer_s6));
 }
 L21 = ATTR(self,indent);
 L221_=(L21)==(1); 
 if (L221_) {
  SATTR(self,saw_outer_return,TRUE);
 }
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e1197535054(CGEN self, AM_LOOP_STMT arg) {
 dAM_STMT arg_init;
 BOOL mlunl = BOOL_zero;
 STR outer_loop;
 INT outer_loop_ex_ne = INT_zero;
 STR fname;
 INT fnamecount = INT_zero;
 FLISTA1062334999 ni;
 FLISTA1409846210 nbi;
 FLISTA1062334999 L11;
 AM_ITE1809135850 e;
 STR tname;
 FLISTA1062334999 L21;
 AM_ITE1809135850 d;
 STR abs_frame_cast;
 STR self_ob;
 STR the_self;
 dAM_STMT init_self;
 STR tag1;
 FLISTA1062334999 L31;
 AM_ITE1809135850 e1;
 FLISTA1062334999 L41;
 AM_ITE1809135850 it;
 FLISTA725283029 L51;
 AM_LOCAL_EXPR e2;
 FLISTA1409846210 L61;
 AM_BND367211769 e3;
 STR name111;
 STR bnd_iter_expr;
 STR l;
 STR bnd_iter_expr1;
 STR l1;
 STR name211;
 FLISTA1062334999 L71;
 AM_ITE1809135850 e4;
 STR ep;
 CGEN mang_self;
 OB mang_ob;
 OB mang_ns;
 STR ret_val;
 MANGLE genother_self;
 OB genother_ns;
 STR ret_val1;
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
 PROG psather_self;
 BOOL ret_val7 = BOOL_zero;
 SIG is_iter_self;
 BOOL ret_val8 = BOOL_zero;
 IDENT is_iter_self1 = IDENT_zero;
 BOOL ret_val9 = BOOL_zero;
 STR plus_self;
 INT plus_arg = INT_zero;
 STR ret_val10;
 STR s;
 INT str_self = INT_zero;
 STR ret_val11;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val12;
 FSTR str_self1;
 STR ret_val13;
 STR plus_self1;
 STR plus_sarg;
 STR ret_val14;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN in_self;
 CGEN comment_self;
 STR comment_com;
 STR plus_self2;
 STR plus_sarg1;
 STR ret_val15;
 STR plus_self3;
 STR plus_sarg2;
 STR ret_val16;
 CGEN ndefer_self1;
 STR ndefer_s1;
 CGEN ndefer_self2;
 STR ndefer_s2;
 CGEN in_self1;
 CODE_FILE uses_iter_self;
 SIG uses_iter_s;
 STR plus_self4;
 INT plus_arg1 = INT_zero;
 STR ret_val17;
 STR s1;
 INT str_self2 = INT_zero;
 STR ret_val18;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val19;
 FSTR str_self3;
 STR ret_val20;
 STR plus_self5;
 STR plus_sarg3;
 STR ret_val21;
 CGEN comment_self1;
 STR comment_com1;
 STR plus_self6;
 STR plus_sarg4;
 STR ret_val22;
 STR plus_self7;
 STR plus_sarg5;
 STR ret_val23;
 CGEN ndefer_self3;
 STR ndefer_s3;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val24;
 STR plus_self8;
 STR plus_sarg6;
 STR ret_val25;
 CGEN ndefer_self4;
 STR ndefer_s4;
 CGEN mang_self2;
 OB mang_ob2;
 OB mang_ns1;
 STR ret_val26;
 CGEN defer_self;
 STR defer_s;
 CGEN mang_self3;
 OB mang_ob3;
 STR ret_val27;
 STR plus_self9;
 STR plus_sarg7;
 STR ret_val28;
 STR plus_self10;
 STR plus_sarg8;
 STR ret_val29;
 CGEN defer_self1;
 STR defer_s1;
 CGEN defer_self2;
 STR defer_s2;
 STR plus_self11;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val30;
 STR s2;
 CHAR str_self4 = CHAR_zero;
 STR ret_val31;
 STR create_self4;
 CHAR create_c = CHAR_zero;
 STR ret_val32;
 STR r2;
 STR plus_self12;
 INT plus_arg3 = INT_zero;
 STR ret_val33;
 STR s3;
 INT str_self5 = INT_zero;
 STR ret_val34;
 FSTR buf3 = ((FSTR) NULL);
 FSTR clear_self2;
 INT str_in_self2 = INT_zero;
 FSTR str_in_s2;
 FSTR ret_val35;
 FSTR str_self6;
 STR ret_val36;
 CGEN mang_self4;
 OB mang_ob4;
 STR ret_val37;
 STR plus_self13;
 STR plus_sarg9;
 STR ret_val38;
 CGEN ndefer_self5;
 STR ndefer_s5;
 CGEN mang_self5;
 OB mang_ob5;
 OB mang_ns2;
 STR ret_val39;
 STR plus_self14;
 STR plus_sarg10;
 STR ret_val40;
 CGEN ndefer_self6;
 STR ndefer_s6;
 CGEN mang_self6;
 OB mang_ob6;
 STR ret_val41;
 STR plus_self15;
 STR plus_sarg11;
 STR ret_val42;
 STR plus_self16;
 STR plus_sarg12;
 STR ret_val43;
 STR plus_self17;
 STR plus_sarg13;
 STR ret_val44;
 STR plus_self18;
 STR plus_sarg14;
 STR ret_val45;
 CGEN defer_self3;
 STR defer_s3;
 CGEN mang_self7;
 OB mang_ob7;
 STR ret_val46;
 STR plus_self19;
 STR plus_sarg15;
 STR ret_val47;
 CGEN defer_self4;
 STR defer_s4;
 CGEN mang_self8;
 OB mang_ob8;
 OB mang_ns3;
 STR ret_val48;
 STR plus_self20;
 STR plus_sarg16;
 STR ret_val49;
 STR plus_self21;
 STR plus_sarg17;
 STR ret_val50;
 STR plus_self22;
 CHAR plus_arg4 = CHAR_zero;
 STR ret_val51;
 STR s4;
 CHAR str_self7 = CHAR_zero;
 STR ret_val52;
 STR create_self5;
 CHAR create_c1 = CHAR_zero;
 STR ret_val53;
 STR r3;
 CGEN defer_self5;
 STR defer_s5;
 CGEN builtin_cntr_sel;
 INT ret_val54 = INT_zero;
 CGEN mang_self9;
 OB mang_ob9;
 OB mang_ns4;
 STR ret_val55;
 STR plus_self23;
 STR plus_sarg18;
 STR ret_val56;
 STR plus_self24;
 INT plus_arg5 = INT_zero;
 STR ret_val57;
 STR s5;
 INT str_self8 = INT_zero;
 STR ret_val58;
 FSTR buf4 = ((FSTR) NULL);
 FSTR clear_self3;
 INT str_in_self3 = INT_zero;
 FSTR str_in_s3;
 FSTR ret_val59;
 FSTR str_self9;
 STR ret_val60;
 SIG is_iter_self2;
 BOOL ret_val61 = BOOL_zero;
 IDENT is_iter_self3 = IDENT_zero;
 BOOL ret_val62 = BOOL_zero;
 STR plus_self25;
 STR plus_sarg19;
 STR ret_val63;
 STR plus_self26;
 STR plus_sarg20;
 STR ret_val64;
 STR plus_self27;
 STR plus_sarg21;
 STR ret_val65;
 CGEN ndefer_self7;
 STR ndefer_s7;
 CGEN mang_self10;
 OB mang_ob10;
 STR ret_val66;
 STR plus_self28;
 STR plus_sarg22;
 STR ret_val67;
 STR plus_self29;
 STR plus_sarg23;
 STR ret_val68;
 CGEN ndefer_self8;
 STR ndefer_s8;
 CGEN ndefer_self9;
 STR ndefer_s9;
 CGEN in_self2;
 STR plus_self30;
 STR plus_sarg24;
 STR ret_val69;
 STR plus_self31;
 STR plus_sarg25;
 STR ret_val70;
 STR plus_self32;
 STR plus_sarg26;
 STR ret_val71;
 CGEN runtime_error_se;
 STR runtime_error_s;
 STR plus_self33;
 STR plus_sarg27;
 STR ret_val72;
 STR plus_self34;
 STR plus_sarg28;
 STR ret_val73;
 CGEN ndefer_self10;
 STR ndefer_s10;
 CGEN move_out_self;
 CGEN ndefer_self11;
 STR ndefer_s11;
 CGEN mang_self11;
 OB mang_ob11;
 OB mang_ns5;
 STR ret_val74;
 STR plus_self35;
 STR plus_sarg29;
 STR ret_val75;
 STR plus_self36;
 STR plus_sarg30;
 STR ret_val76;
 STR plus_self37;
 STR plus_sarg31;
 STR ret_val77;
 CGEN mang_self12;
 OB mang_ob12;
 STR ret_val78;
 STR plus_self38;
 STR plus_sarg32;
 STR ret_val79;
 STR plus_self39;
 STR plus_sarg33;
 STR ret_val80;
 STR plus_self40;
 STR plus_sarg34;
 STR ret_val81;
 STR plus_self41;
 STR plus_sarg35;
 STR ret_val82;
 STR plus_self42;
 STR plus_sarg36;
 STR ret_val83;
 STR plus_self43;
 STR plus_sarg37;
 STR ret_val84;
 CGEN ndefer_self12;
 STR ndefer_s12;
 SIG is_builtin_iter_;
 BOOL ret_val85 = BOOL_zero;
 SIG is_iter_self4;
 BOOL ret_val86 = BOOL_zero;
 IDENT is_iter_self5 = IDENT_zero;
 BOOL ret_val87 = BOOL_zero;
 CGEN mang_self13;
 OB mang_ob13;
 OB mang_ns6;
 STR ret_val88;
 STR plus_self44;
 STR plus_sarg38;
 STR ret_val89;
 STR plus_self45;
 STR plus_sarg39;
 STR ret_val90;
 STR plus_self46;
 CHAR plus_arg6 = CHAR_zero;
 STR ret_val91;
 STR s6;
 CHAR str_self10 = CHAR_zero;
 STR ret_val92;
 STR create_self6;
 CHAR create_c2 = CHAR_zero;
 STR ret_val93;
 STR r4;
 STR plus_self47;
 INT plus_arg7 = INT_zero;
 STR ret_val94;
 STR s7;
 INT str_self11 = INT_zero;
 STR ret_val95;
 FSTR buf5 = ((FSTR) NULL);
 FSTR clear_self4;
 INT str_in_self4 = INT_zero;
 FSTR str_in_s4;
 FSTR ret_val96;
 FSTR str_self12;
 STR ret_val97;
 STR plus_self48;
 CHAR plus_arg8 = CHAR_zero;
 STR ret_val98;
 STR s8;
 CHAR str_self13 = CHAR_zero;
 STR ret_val99;
 STR create_self7;
 CHAR create_c3 = CHAR_zero;
 STR ret_val100;
 STR r5;
 CGEN ndefer_self13;
 STR ndefer_s13;
 SIG is_iter_self6;
 BOOL ret_val101 = BOOL_zero;
 IDENT is_iter_self7 = IDENT_zero;
 BOOL ret_val102 = BOOL_zero;
 STR plus_self49;
 STR plus_sarg40;
 STR ret_val103;
 STR plus_self50;
 STR plus_sarg41;
 STR ret_val104;
 STR plus_self51;
 STR plus_sarg42;
 STR ret_val105;
 CGEN ndefer_self14;
 STR ndefer_s14;
 CGEN mang_self14;
 OB mang_ob14;
 OB mang_ns7;
 STR ret_val106;
 STR plus_self52;
 STR plus_sarg43;
 STR ret_val107;
 CGEN ndefer_self15;
 STR ndefer_s15;
 STR plus_self53;
 STR plus_sarg44;
 STR ret_val108;
 CGEN ndefer_self16;
 STR ndefer_s16;
 CGEN comment_self2;
 STR comment_com2;
 STR plus_self54;
 STR plus_sarg45;
 STR ret_val109;
 STR plus_self55;
 STR plus_sarg46;
 STR ret_val110;
 CGEN ndefer_self17;
 STR ndefer_s17;
 SIG is_iter_self8;
 BOOL ret_val1111 = BOOL_zero;
 IDENT is_iter_self9 = IDENT_zero;
 BOOL ret_val112 = BOOL_zero;
 STR plus_self56;
 INT plus_arg9 = INT_zero;
 STR ret_val113;
 STR s9;
 INT str_self14 = INT_zero;
 STR ret_val114;
 FSTR buf6 = ((FSTR) NULL);
 FSTR clear_self5;
 INT str_in_self5 = INT_zero;
 FSTR str_in_s5;
 FSTR ret_val115;
 FSTR str_self15;
 STR ret_val116;
 STR plus_self57;
 INT plus_arg10 = INT_zero;
 STR ret_val117;
 STR s10;
 INT str_self16 = INT_zero;
 STR ret_val118;
 FSTR buf7 = ((FSTR) NULL);
 FSTR clear_self6;
 INT str_in_self6 = INT_zero;
 FSTR str_in_s6;
 FSTR ret_val119;
 FSTR str_self17;
 STR ret_val120;
 MANGLE genlocal_self;
 OB genlocal_ns;
 STR ret_val121;
 STR name311;
 MANGLE space_self1;
 OB space_ns1;
 NAMESPACE ret_val122;
 NAMESPACE r6;
 NAMESPACE create_self8;
 NAMESPACE ret_val123;
 NAMESPACE r7;
 NAMEMAP create_self9;
 NAMEMAP ret_val124;
 FSETSTR create_self10;
 FSETSTR ret_val125;
 FMAPSTRINT create_self11;
 FMAPSTRINT ret_val126;
 CODE_FILE plus_self58;
 dSTR plus_s;
 CODE_FILE ret_val127;
 CODE_FILE plus_self59;
 dSTR plus_s1;
 CODE_FILE ret_val128;
 CGEN mang_self15;
 OB mang_ob15;
 STR ret_val129;
 CODE_FILE plus_self60;
 dSTR plus_s2;
 CODE_FILE ret_val130;
 CODE_FILE plus_self61;
 dSTR plus_s3;
 CODE_FILE ret_val131;
 CODE_FILE plus_self62;
 dSTR plus_s4;
 CODE_FILE ret_val132;
 CODE_FILE plus_self63;
 dSTR plus_s5;
 STR plus_self64;
 STR plus_sarg47;
 STR ret_val133;
 STR plus_self65;
 STR plus_sarg48;
 STR ret_val134;
 STR plus_self66;
 STR plus_sarg49;
 STR ret_val135;
 CGEN ndefer_self18;
 STR ndefer_s18;
 STR plus_self67;
 STR plus_sarg50;
 STR ret_val136;
 STR plus_self68;
 STR plus_sarg51;
 STR ret_val137;
 STR plus_self69;
 STR plus_sarg52;
 STR ret_val138;
 CGEN ndefer_self19;
 STR ndefer_s19;
 STR plus_self70;
 STR plus_sarg53;
 STR ret_val139;
 CGEN mang_self16;
 OB mang_ob16;
 STR ret_val140;
 STR plus_self71;
 STR plus_sarg54;
 STR ret_val141;
 STR plus_self72;
 STR plus_sarg55;
 STR ret_val142;
 STR plus_self73;
 STR plus_sarg56;
 STR ret_val143;
 STR plus_self74;
 STR plus_sarg57;
 STR ret_val144;
 STR plus_self75;
 STR plus_sarg58;
 STR ret_val145;
 STR plus_self76;
 STR plus_sarg59;
 STR ret_val146;
 STR plus_self77;
 STR plus_sarg60;
 STR ret_val147;
 STR plus_self78;
 STR plus_sarg61;
 STR ret_val148;
 CGEN ndefer_self20;
 STR ndefer_s20;
 MANGLE genlocal_self1;
 OB genlocal_ns1;
 STR ret_val149;
 STR name411;
 MANGLE space_self2;
 OB space_ns2;
 NAMESPACE ret_val150;
 NAMESPACE r8;
 NAMESPACE create_self12;
 NAMESPACE ret_val151;
 NAMESPACE r9;
 NAMEMAP create_self13;
 NAMEMAP ret_val152;
 FSETSTR create_self14;
 FSETSTR ret_val153;
 FMAPSTRINT create_self15;
 FMAPSTRINT ret_val154;
 CODE_FILE plus_self79;
 dSTR plus_s6;
 CODE_FILE ret_val155;
 CODE_FILE plus_self80;
 dSTR plus_s7;
 CODE_FILE ret_val156;
 CGEN mang_self17;
 OB mang_ob17;
 STR ret_val157;
 CODE_FILE plus_self81;
 dSTR plus_s8;
 CODE_FILE ret_val158;
 CODE_FILE plus_self82;
 dSTR plus_s9;
 CODE_FILE ret_val159;
 CODE_FILE plus_self83;
 dSTR plus_s10;
 CODE_FILE ret_val160;
 CODE_FILE plus_self84;
 dSTR plus_s11;
 STR plus_self85;
 STR plus_sarg62;
 STR ret_val161;
 STR plus_self86;
 STR plus_sarg63;
 STR ret_val162;
 STR plus_self87;
 STR plus_sarg64;
 STR ret_val163;
 CGEN ndefer_self21;
 STR ndefer_s21;
 CGEN mang_self18;
 OB mang_ob18;
 OB mang_ns8;
 STR ret_val164;
 CODE_FILE plus_self88;
 dSTR plus_s12;
 CODE_FILE ret_val165;
 CODE_FILE plus_self89;
 dSTR plus_s13;
 CODE_FILE ret_val166;
 CGEN mang_self19;
 OB mang_ob19;
 STR ret_val167;
 CODE_FILE plus_self90;
 dSTR plus_s14;
 CODE_FILE ret_val168;
 CODE_FILE plus_self91;
 dSTR plus_s15;
 CODE_FILE ret_val169;
 CODE_FILE plus_self92;
 dSTR plus_s16;
 CODE_FILE ret_val170;
 CODE_FILE plus_self93;
 dSTR plus_s17;
 STR plus_self94;
 STR plus_sarg65;
 STR ret_val171;
 STR plus_self95;
 STR plus_sarg66;
 STR ret_val172;
 STR plus_self96;
 STR plus_sarg67;
 STR ret_val173;
 CGEN ndefer_self22;
 STR ndefer_s22;
 STR plus_self97;
 STR plus_sarg68;
 STR ret_val174;
 CGEN mang_self20;
 OB mang_ob20;
 STR ret_val175;
 STR plus_self98;
 STR plus_sarg69;
 STR ret_val176;
 STR plus_self99;
 STR plus_sarg70;
 STR ret_val177;
 STR plus_self100;
 STR plus_sarg71;
 STR ret_val178;
 STR plus_self101;
 STR plus_sarg72;
 STR ret_val179;
 STR plus_self102;
 STR plus_sarg73;
 STR ret_val180;
 STR plus_self103;
 STR plus_sarg74;
 STR ret_val181;
 STR plus_self104;
 STR plus_sarg75;
 STR ret_val182;
 STR plus_self105;
 STR plus_sarg76;
 STR ret_val183;
 CGEN ndefer_self23;
 STR ndefer_s23;
 CGEN ndefer_self24;
 STR ndefer_s24;
 CGEN in_self3;
 CGEN ndefer_self25;
 STR ndefer_s25;
 CGEN move_out_self1;
 CGEN ndefer_self26;
 STR ndefer_s26;
 CGEN move_out_self2;
 CGEN ndefer_self27;
 STR ndefer_s27;
 CGEN ndefer_self28;
 STR ndefer_s28;
 SIG is_builtin_iter_1;
 BOOL ret_val184 = BOOL_zero;
 SIG is_iter_self10;
 BOOL ret_val185 = BOOL_zero;
 IDENT is_iter_self11 = IDENT_zero;
 BOOL ret_val186 = BOOL_zero;
 CGEN mang_self21;
 OB mang_ob21;
 OB mang_ns9;
 STR ret_val187;
 CGEN ndefer_self29;
 STR ndefer_s29;
 PROG psather_self1;
 BOOL ret_val188 = BOOL_zero;
 SIG is_iter_self12;
 BOOL ret_val189 = BOOL_zero;
 IDENT is_iter_self13 = IDENT_zero;
 BOOL ret_val190 = BOOL_zero;
 CGEN move_out_self3;
 CGEN ndefer_self30;
 STR ndefer_s30;
 NAMESPACE L8;
 OB L9;
 extern STR other359;
 BOOL L10;
 BOOL L121_;
 BOOL L13;
 BOOL L14;
 BOOL L15;
 INT L16;
 INT L17;
 INT L181_;
 INT L19;
 BOOL L201_;
 BOOL L22;
 BOOL L231_;
 BOOL L24;
 BOOL L25;
 BOOL L26;
 BOOL L271_;
 STR L28;
 INT L29;
 INT L301_;
 INT L32;
 CHAR L331_;
 CHAR L34;
 BOOL L351_;
 BOOL L36;
 BOOL L371_;
 BOOL L38;
 BOOL L39;
 BOOL L40;
 BOOL L421_;
 BOOL L43;
 BOOL L441_;
 SE_CONTEXT L45;
 extern STR LOOP_BEGIN1;
 INT L46;
 INT L47;
 INT L481_;
 BOOL L49;
 BOOL L501_;
 extern STR name26;
 INT L52;
 INT L531_;
 INT L54;
 INT L551_;
 INT L56;
 INT L571_;
 BOOL L58;
 BOOL L59;
 BOOL L60;
 BOOL L62;
 BOOL L631_;
 BOOL L64;
 BOOL L651_;
 BOOL L66;
 BOOL L671_;
 BOOL L68;
 BOOL L691_;
 extern STR loop;
 extern STR name54;
 extern STR name79;
 extern STR name2;
 INT L70;
 INT L721_;
 BOOL L73;
 BOOL L741_;
 AM_ITE1809135850 L75;
 BOOL L76;
 BOOL L771_;
 BOOL L78;
 BOOL L791_;
 extern STR framenested;
 BOOL L80;
 BOOL L811_;
 extern STR Frameforcallto;
 extern STR name54;
 extern STR name79;
 extern STR S_frame;
 BOOL L82;
 BOOL L83;
 BOOL L841_;
 dTP L85;
 BOOL L86;
 BOOL L871_;
 dTP L88;
 BOOL L89;
 BOOL L901_;
 extern STR OBALLOC1;
 extern STR S_frame1;
 extern STR name16;
 STR L91;
 INT L92;
 OB L93;
 INT L94;
 BOOL L96;
 BOOL L971_;
 INT L981_;
 dTP L99;
 extern STR S_frame;
 extern STR name16;
 extern STR struct2;
 extern STR S_frame_struct;
 extern STR name13;
 extern STR S_frame;
 extern STR name91;
 STR L100;
 INT L101;
 OB L102;
 INT L103;
 INT L105;
 INT L1061_;
 OB L107;
 OB L108;
 extern STR S_;
 BOOL L109;
 BOOL L1101_;
 extern STR framenested;
 BOOL L111;
 BOOL L1121_;
 BOOL L113;
 BOOL L114;
 BOOL L1151_;
 STR L116;
 INT L117;
 INT L1181_;
 INT L119;
 CHAR L1201_;
 CHAR L122;
 BOOL L1231_;
 BOOL L124;
 BOOL L1251_;
 extern STR BOOL2;
 extern STR f_;
 extern STR TRUE3;
 BOOL L126;
 BOOL L1271_;
 AM_ITE1809135850 L128;
 dTP L129;
 extern STR name24;
 extern STR S_frame2;
 AM_CALL_ARG L1301_;
 AM_CALL_ARG L131;
 extern STR name36;
 extern STR name16;
 BOOL L132;
 BOOL L133;
 BOOL L1341_;
 extern STR if3;
 extern STR NULL3;
 INT L135;
 INT L1361_;
 extern STR Dispatchedcallto;
 extern STR onvoidselfin;
 extern STR FATAL1;
 extern STR name95;
 INT L137;
 INT L1381_;
 extern STR name4;
 extern STR TAG2;
 extern STR F_TAG2;
 extern STR name36;
 extern STR name85;
 extern STR name14;
 extern STR name24;
 extern STR alloc_frame;
 BOOL L139;
 BOOL L1401_;
 AM_ITE1809135850 L141;
 BOOL L142;
 BOOL L143;
 BOOL L144;
 BOOL L1451_;
 STR L146;
 INT L147;
 INT L1481_;
 INT L149;
 CHAR L1501_;
 CHAR L151;
 BOOL L1521_;
 BOOL L153;
 BOOL L1541_;
 BOOL L155;
 BOOL L156;
 BOOL L1571_;
 dTP L158;
 BOOL L159;
 BOOL L1601_;
 extern STR name91;
 STR L161;
 INT L162;
 OB L163;
 INT L164;
 BOOL L166;
 BOOL L1671_;
 STR L168;
 INT L169;
 OB L170;
 INT L171;
 INT L1731_;
 AM_ITE1809135850 L174;
 BOOL L175;
 BOOL L176;
 BOOL L1771_;
 STR L178;
 INT L179;
 INT L1801_;
 INT L182;
 CHAR L1831_;
 CHAR L184;
 BOOL L1851_;
 extern STR f_;
 extern STR TRUE3;
 extern STR state01;
 BOOL L186;
 BOOL L1871_;
 BOOL L188;
 BOOL L1891_;
 AM_LOCAL_EXPR L190;
 extern STR TRUE3;
 BOOL L191;
 BOOL L1921_;
 extern STR Binary376592169;
 extern STR name54;
 extern STR name79;
 AM_BND367211769 L193;
 BOOL L194;
 BOOL L195;
 BOOL L1961_;
 BOOL L197;
 BOOL L198;
 BOOL L1991_;
 STR L200;
 INT L202;
 INT L2031_;
 INT L204;
 CHAR L2051_;
 CHAR L206;
 BOOL L2071_;
 extern STR framen219781171;
 BOOL L208;
 BOOL L2091_;
 extern STR framen219781171;
 BOOL L210;
 BOOL L2111_;
 NAMESPACE L212;
 OB L213;
 extern STR L;
 BOOL L214;
 BOOL L2151_;
 FSTR L216;
 dSTR L217;
 dSTR L218;
 OB L219;
 BOOL L220;
 BOOL L2211_;
 FSTR L222;
 dSTR L223;
 BOOL L224;
 BOOL L2251_;
 FSTR L226;
 dSTR L227;
 dSTR L228;
 OB L229;
 BOOL L230;
 BOOL L2321_;
 FSTR L233;
 dSTR L234;
 BOOL L235;
 BOOL L2361_;
 FSTR L237;
 dSTR L238;
 extern STR name16;
 FSTR L239;
 dSTR L240;
 extern STR name36;
 extern STR name16;
 extern STR ZALLOC2;
 extern STR size4;
 extern STR name80;
 extern STR memcpy1;
 extern STR name3;
 extern STR name3;
 extern STR size4;
 NAMESPACE L241;
 OB L242;
 extern STR L;
 BOOL L243;
 BOOL L2441_;
 FSTR L245;
 dSTR L246;
 dSTR L247;
 OB L248;
 BOOL L249;
 BOOL L2501_;
 FSTR L251;
 dSTR L252;
 BOOL L253;
 BOOL L2541_;
 FSTR L255;
 dSTR L256;
 dSTR L257;
 OB L258;
 BOOL L259;
 BOOL L2601_;
 FSTR L261;
 dSTR L262;
 BOOL L263;
 BOOL L2641_;
 FSTR L265;
 dSTR L266;
 extern STR name16;
 FSTR L267;
 dSTR L268;
 extern STR name36;
 extern STR name16;
 BOOL L269;
 BOOL L2701_;
 FSTR L272;
 dSTR L273;
 dSTR L274;
 OB L275;
 BOOL L276;
 BOOL L2771_;
 FSTR L278;
 dSTR L279;
 BOOL L280;
 BOOL L2811_;
 FSTR L282;
 dSTR L283;
 dSTR L284;
 OB L285;
 BOOL L286;
 BOOL L2871_;
 FSTR L288;
 dSTR L289;
 BOOL L290;
 BOOL L2911_;
 FSTR L292;
 dSTR L293;
 extern STR name16;
 FSTR L294;
 dSTR L295;
 extern STR ZALLOC2;
 extern STR size4;
 extern STR name80;
 extern STR memcpy1;
 extern STR name3;
 extern STR name3;
 extern STR size4;
 extern STR while11;
 INT L296;
 INT L2971_;
 BOOL L298;
 extern STR AM_POLL_NETWORK;
 INT L299;
 INT L3001_;
 extern STR name4;
 INT L302;
 INT L3031_;
 extern STR name4;
 extern STR name136;
 BOOL L304;
 BOOL L305;
 BOOL L3061_;
 AM_ITE1809135850 L307;
 BOOL L308;
 BOOL L309;
 BOOL L310;
 BOOL L3111_;
 STR L312;
 INT L313;
 INT L3141_;
 INT L315;
 CHAR L3161_;
 CHAR L317;
 BOOL L3181_;
 BOOL L319;
 BOOL L3201_;
 extern STR ZFREE1;
 extern STR name137;
 extern STR NULL4;
 BOOL L321;
 BOOL L322;
 BOOL L323;
 INT L324;
 INT L325;
 INT L3261_;
 INT L327;
 BOOL L3281_;
 BOOL L329;
 BOOL L3301_;
 BOOL L332;
 BOOL L333;
 BOOL L334;
 BOOL L3351_;
 STR L336;
 INT L337;
 INT L3381_;
 INT L339;
 CHAR L3401_;
 CHAR L341;
 BOOL L3421_;
 BOOL L343;
 BOOL L3441_;
 BOOL L345;
 BOOL L346;
 BOOL L347;
 BOOL L3481_;
 BOOL L349;
 BOOL L3501_;
 SE_CONTEXT L352;
 INT L353;
 INT L3541_;
 extern STR LOOP_END1;
 INT L355;
 INT L3561_;
 arg_init = ATTR(arg,init);
 mlunl = ATTR(self,manual_loop_unlo);
 SATTR(self,manual_loop_unlo,FALSE);
 outer_loop = ATTR(self,current_loop);
 mang_self = self;
 mang_ob = ((OB) arg);
 mang_ns = ((OB) ATTR(self,current_sig));
 ret_val = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, mang_ns);
 SATTR(self,current_loop,ret_val);
 outer_loop_ex_ne = ATTR(self,current_loop_ex_);
 SATTR(self,current_loop_ex_,ATTR(self,ex_nesting));
 genother_self = ATTR(self,mangler);
 genother_ns = ((OB) ATTR(self,current_sig));
 space_self = genother_self;
 space_ns = genother_ns;
 if ((space_ns==((OB) NULL))) {
  space_ns = ((OB) space_self);
 }
 r = FMAPdO790518483(ATTR(space_self,namespaces), space_ns);
 if ((r==((NAMESPACE) NULL))) {
  create_self = ((NAMESPACE) NULL);
  L9=ZALLOC(sizeof(struct NAMESPACE_struct));
  if (L9==NULL) FATAL("Unable to allocate more memory");
  ((OB)L9)->header.tag=NAMESPACE_tag;
  L8 = ((NAMESPACE) L9);
  r1 = L8;
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
 ret_val1 = NAMESP8552102(ret_val2, ((STR) &other359));
 fname = ret_val1;
 fnamecount = 0;
 ni = FMAPAM925529692(ATTR(self,nested_its), ATTR(self,current_am_rout_));
 nbi = FMAPAM338804820(ATTR(self,nested_bits), ATTR(self,current_am_rout_));
 L10 = (ATTR(arg,loop_index_var)==((AM_LOCAL_EXPR) NULL));
 L121_=!(L10); 
 if (L121_) {
  SATTR(arg,loop_index,CGEN_e236145586(self, ATTR(arg,loop_index_var)));
 }
 SATTR(arg,has_init_stmt,FALSE);
 psather_self = ATTR(self,prog);
 if (ATTR(psather_self,threads)) {
  L13 = TRUE;
 } else {
  L13 = ATTR(psather_self,distributed);
 }
 ret_val7 = L13;
 if (ret_val7) {
  L16 = FLISTA2089597941(ATTR(arg,bits1));
  L17 = FLISTA1962912942(ATTR(arg,its1));
  L181_=INTPLUS(L16,L17); 
  L19 = L181_;
  L201_=(0)<(L19); 
  if (L201_) {
   L22 = ATTR(arg,no_begin_loop);
   L231_=!(L22); 
   L15 = L231_;
  } else {
   L15 = FALSE;
  }
  if (L15) {
   is_iter_self = ATTR(self,current_sig);
   is_iter_self1 = ATTR(is_iter_self,name1);
   L26 = (is_iter_self1.str==((STR) NULL));
   L271_=!(L26); 
   if (L271_) {
    L28 = is_iter_self1.str;
    L29 = STR_sizerINT(is_iter_self1.str);
    L301_=INTMINUS(L29,1); 
    L32 = L301_;
    L331_=ARR((STR)L28,L32); 
    L34 = L331_;
    L351_=L34=='!'; 
    L25 = L351_;
   } else {
    L25 = FALSE;
   }
   ret_val9 = L25;
   ret_val8 = ret_val9;
   L36 = ret_val8;
   L371_=!(L36); 
   if (L371_) {
    L24 = TRUE;
   } else {
    L40 = ATTR(ATTR(self,prog),yields_in_locks);
    L421_=!(L40); 
    if (L421_) {
     L39 = TRUE;
    } else {
     L43 = ATTR(ATTR(self,options1),side_effects);
     L441_=!(L43); 
     L39 = L441_;
    }
    if (L39) {
     L38 = TRUE;
    } else {
     L45=SIG_ge642497605(ATTR(self,current_sig), ATTR(self,prog));
     L38 = ATTR(L45,has_yield_in_loc);
    }
    L24 = L38;
   }
   L14 = L24;
  } else {
   L14 = FALSE;
  }
  if (L14) {
   ndefer_self = self;
   plus_self = ((STR) &LOOP_BEGIN1);
   L46 = FLISTA2089597941(ATTR(arg,bits1));
   L47 = FLISTA1962912942(ATTR(arg,its1));
   L481_=INTPLUS(L46,L47); 
   plus_arg = L481_;
   str_self = plus_arg;
   clear_self = buf1;
   L49 = (clear_self==((FSTR) NULL));
   L501_=!(L49); 
   if (L501_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val12 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val12;
   str_self1 = buf1;
   ret_val13 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val11 = ret_val13;
   s = ret_val11;
   ret_val10 = STR_ap1077157958(plus_self, s, TRUE);
   plus_self1 = ret_val10;
   plus_sarg = ((STR) &name26);
   ret_val14 = STR_ap2004550586(plus_self1, plus_sarg);
   ndefer_s = ret_val14;
   CGEN_d695831906(ndefer_self);
   SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
   in_self = self;
   L52 = ATTR(in_self,indent);
   L531_=INTPLUS(L52,1); 
   SATTR(in_self,indent,L531_);
   L54 = ATTR(self,ex_nesting);
   L551_=INTPLUS(L54,1); 
   SATTR(self,ex_nesting,L551_);
   L56 = ATTR(self,current_loop_ex_);
   L571_=INTPLUS(L56,1); 
   SATTR(self,current_loop_ex_,L571_);
  }
 }
 L62 = (ATTR(arg,bits1)==((FLISTA1409846210) NULL));
 L631_=!(L62); 
 if (L631_) {
  L60 = TRUE;
 } else {
  L64 = (ATTR(arg,its1)==((FLISTA1062334999) NULL));
  L651_=!(L64); 
  L60 = L651_;
 }
 if (L60) {
  L59 = TRUE;
 } else {
  L66 = (ATTR(arg,firsts)==((FLISTA725283029) NULL));
  L671_=!(L66); 
  L59 = L671_;
 }
 if (L59) {
  L58 = TRUE;
 } else {
  L68 = (ATTR(arg,init)==((dAM_STMT) NULL));
  L691_=!(L68); 
  L58 = L691_;
 }
 if (L58) {
  comment_self = self;
  comment_com = ((STR) &loop);
  if (ATTR(comment_self,pretty)) {
   ndefer_self1 = comment_self;
   plus_self2 = ((STR) &name54);
   plus_sarg1 = comment_com;
   ret_val15 = STR_ap2004550586(plus_self2, plus_sarg1);
   plus_self3 = ret_val15;
   plus_sarg2 = ((STR) &name79);
   ret_val16 = STR_ap2004550586(plus_self3, plus_sarg2);
   ndefer_s1 = ret_val16;
   CGEN_d695831906(ndefer_self1);
   SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
  }
  SATTR(arg,has_init_stmt,TRUE);
  ndefer_self2 = self;
  ndefer_s2 = ((STR) &name2);
  CGEN_d695831906(ndefer_self2);
  SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
  in_self1 = self;
  L70 = ATTR(in_self1,indent);
  L721_=INTPLUS(L70,1); 
  SATTR(in_self1,indent,L721_);
 }
 L73 = (ATTR(arg,its1)==((FLISTA1062334999) NULL));
 L741_=!(L73); 
 if (L741_) {
  {
   struct FLISTA20621028_frame_struct other1_0;
FLISTA20621028_frame noname1 = &other1_0;
   L11 = ATTR(arg,its1);
   noname1->self = L11;
   noname1->state = 0;
   while (1) {
    L75 = FLISTA20621028(noname1);
    if (noname1->state == -1) {
     goto after_loop;
    }
    e = L75;
    CODE_F1291349628(ATTR(self,code_c), ATTR(e,fun));
    uses_iter_self = ATTR(self,code_c);
    uses_iter_s = ATTR(e,fun);
    SATTR(uses_iter_self,iters,FSETSI671175003(ATTR(uses_iter_self,iters), uses_iter_s));
    L76 = SIG_is418491101(ATTR(e,fun));
    L771_=!(L76); 
    if (L771_) {
     L78 = (ni==((FLISTA1062334999) NULL));
     L791_=!(L78); 
     if (L791_) {
      ni = FLISTA943034044(ni, e);
      plus_self4 = ((STR) &framenested);
      plus_arg1 = FLISTA1962912942(ni);
      str_self2 = plus_arg1;
      clear_self1 = buf2;
      L80 = (clear_self1==((FSTR) NULL));
      L811_=!(L80); 
      if (L811_) {
       SATTR(clear_self1,loc,0);
      }
      str_in_self1 = str_self2;
      str_in_s1 = buf2;
      ret_val19 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
      buf2 = ret_val19;
      str_self3 = buf2;
      ret_val20 = STR_fr1097270334(((STR) NULL), str_self3);
      ret_val18 = ret_val20;
      s1 = ret_val18;
      ret_val17 = STR_ap1077157958(plus_self4, s1, TRUE);
      MANGLE1827205184(ATTR(self,mangler), ((OB) e), ret_val17, ((OB) ATTR(self,current_sig)));
     }
     else {
      comment_self1 = self;
      plus_self5 = ((STR) &Frameforcallto);
      plus_sarg3 = ATTR(ATTR(e,fun),str);
      ret_val21 = STR_ap2004550586(plus_self5, plus_sarg3);
      comment_com1 = ret_val21;
      if (ATTR(comment_self1,pretty)) {
       ndefer_self3 = comment_self1;
       plus_self6 = ((STR) &name54);
       plus_sarg4 = comment_com1;
       ret_val22 = STR_ap2004550586(plus_self6, plus_sarg4);
       plus_self7 = ret_val22;
       plus_sarg5 = ((STR) &name79);
       ret_val23 = STR_ap2004550586(plus_self7, plus_sarg5);
       ndefer_s3 = ret_val23;
       CGEN_d695831906(ndefer_self3);
       SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
      }
     }
     CGEN_d695831906(self);
     if (ATTR(arg,has_yield)) {
      if ((ni==((FLISTA1062334999) NULL))) {
       ndefer_self4 = self;
       mang_self1 = self;
       mang_ob1 = ((OB) ATTR(e,fun));
       ret_val24 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
       plus_self8 = ret_val24;
       plus_sarg6 = ((STR) &S_frame);
       ret_val25 = STR_ap2004550586(plus_self8, plus_sarg6);
       ndefer_s4 = ret_val25;
       CGEN_d695831906(ndefer_self4);
       SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
      }
      L83 = (ni==((FLISTA1062334999) NULL));
      L841_=!(L83); 
      if (L841_) {
       L85 = ATTR(ATTR(e,fun),tp);
       L82 = (*dTP_is1811059018[TAG(L85)])(L85);
      } else {
       L82 = FALSE;
      }
      L86 = L82;
      L871_=!(L86); 
      if (L871_) {
       defer_self = self;
       mang_self2 = self;
       mang_ob2 = ((OB) e);
       mang_ns1 = ((OB) ATTR(self,current_sig));
       ret_val26 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, mang_ns1);
       defer_s = ret_val26;
       SATTR(defer_self,routine_code,FSTR_p1752847026(ATTR(defer_self,routine_code), defer_s));
      }
      L88 = ATTR(ATTR(e,fun),tp);
      L89 = (*dTP_is1811059018[TAG(L88)])(L88);
      L901_=!(L89); 
      if (L901_) {
       defer_self1 = self;
       plus_self9 = ((STR) &OBALLOC1);
       mang_self3 = self;
       mang_ob3 = ((OB) ATTR(e,fun));
       ret_val27 = MANGLE119219986(ATTR(mang_self3,mangler), mang_ob3, ((OB) NULL));
       plus_sarg7 = ret_val27;
       ret_val28 = STR_ap2004550586(plus_self9, plus_sarg7);
       plus_self10 = ret_val28;
       plus_sarg8 = ((STR) &S_frame1);
       ret_val29 = STR_ap2004550586(plus_self10, plus_sarg8);
       defer_s1 = ret_val29;
       SATTR(defer_self1,routine_code,FSTR_p1752847026(ATTR(defer_self1,routine_code), defer_s1));
      }
      else {
       defer_self2 = self;
       defer_s2 = ((STR) &name16);
       SATTR(defer_self2,routine_code,FSTR_p1752847026(ATTR(defer_self2,routine_code), defer_s2));
      }
     }
     else {
      plus_self11 = fname;
      plus_arg2 = '_';
      str_self4 = plus_arg2;
      create_self4 = ((STR) NULL);
      create_c = str_self4;
      L92 = 1;
      L94=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L92)*sizeof(CHAR);
      L93=ZALLOC_LEAF_BIG(L94);
      if (L93==NULL) FATAL("Unable to allocate more memory");
      memset(L93,0,L94);
      ((OB)L93)->header.tag=STR_tag;
#ifdef DESTROY_CHK

        ((OB)L93)->header.destroyed=0;
#endif

      L91 = ((STR) L93);
      L91->asize = L92;
      r2 = L91;
      SARR((STR)r2,0,create_c); 
      ;
      ret_val32 = r2;
      ret_val31 = ret_val32;
      s2 = ret_val31;
      ret_val30 = STR_ap1077157958(plus_self11, s2, TRUE);
      plus_self12 = ret_val30;
      plus_arg3 = fnamecount;
      str_self5 = plus_arg3;
      clear_self2 = buf3;
      L96 = (clear_self2==((FSTR) NULL));
      L971_=!(L96); 
      if (L971_) {
       SATTR(clear_self2,loc,0);
      }
      str_in_self2 = str_self5;
      str_in_s2 = buf3;
      ret_val35 = INT_st367594495(str_in_self2, str_in_s2, 0, 10, ' ');
      buf3 = ret_val35;
      str_self6 = buf3;
      ret_val36 = STR_fr1097270334(((STR) NULL), str_self6);
      ret_val34 = ret_val36;
      s3 = ret_val34;
      ret_val33 = STR_ap1077157958(plus_self12, s3, TRUE);
      tname = ret_val33;
      L981_=INTPLUS(fnamecount,1); 
      fnamecount = L981_;
      L99 = ATTR(ATTR(e,fun),tp);
      if ((*dTP_is1811059018[TAG(L99)])(L99)) {
       if ((ni==((FLISTA1062334999) NULL))) {
        ndefer_self5 = self;
        mang_self4 = self;
        mang_ob4 = ((OB) ATTR(e,fun));
        ret_val37 = MANGLE119219986(ATTR(mang_self4,mangler), mang_ob4, ((OB) NULL));
        plus_self13 = ret_val37;
        plus_sarg9 = ((STR) &S_frame);
        ret_val38 = STR_ap2004550586(plus_self13, plus_sarg9);
        ndefer_s5 = ret_val38;
        CGEN_d695831906(ndefer_self5);
        SATTR(ndefer_self5,routine_code,FSTR_p1752847026(ATTR(ndefer_self5,routine_code), ndefer_s5));
        ndefer_self6 = self;
        mang_self5 = self;
        mang_ob5 = ((OB) e);
        mang_ns2 = ((OB) ATTR(self,current_sig));
        ret_val39 = MANGLE119219986(ATTR(mang_self5,mangler), mang_ob5, mang_ns2);
        plus_self14 = ret_val39;
        plus_sarg10 = ((STR) &name16);
        ret_val40 = STR_ap2004550586(plus_self14, plus_sarg10);
        ndefer_s6 = ret_val40;
        CGEN_d695831906(ndefer_self6);
        SATTR(ndefer_self6,routine_code,FSTR_p1752847026(ATTR(ndefer_self6,routine_code), ndefer_s6));
       }
      }
      else {
       defer_self3 = self;
       plus_self15 = ((STR) &struct2);
       mang_self6 = self;
       mang_ob6 = ((OB) ATTR(e,fun));
       ret_val41 = MANGLE119219986(ATTR(mang_self6,mangler), mang_ob6, ((OB) NULL));
       plus_sarg11 = ret_val41;
       ret_val42 = STR_ap2004550586(plus_self15, plus_sarg11);
       plus_self16 = ret_val42;
       plus_sarg12 = ((STR) &S_frame_struct);
       ret_val43 = STR_ap2004550586(plus_self16, plus_sarg12);
       plus_self17 = ret_val43;
       plus_sarg13 = tname;
       ret_val44 = STR_ap2004550586(plus_self17, plus_sarg13);
       plus_self18 = ret_val44;
       plus_sarg14 = ((STR) &name13);
       ret_val45 = STR_ap2004550586(plus_self18, plus_sarg14);
       defer_s3 = ret_val45;
       SATTR(defer_self3,routine_code,FSTR_p1752847026(ATTR(defer_self3,routine_code), defer_s3));
       if ((ni==((FLISTA1062334999) NULL))) {
        defer_self4 = self;
        mang_self7 = self;
        mang_ob7 = ((OB) ATTR(e,fun));
        ret_val46 = MANGLE119219986(ATTR(mang_self7,mangler), mang_ob7, ((OB) NULL));
        plus_self19 = ret_val46;
        plus_sarg15 = ((STR) &S_frame);
        ret_val47 = STR_ap2004550586(plus_self19, plus_sarg15);
        defer_s4 = ret_val47;
        SATTR(defer_self4,routine_code,FSTR_p1752847026(ATTR(defer_self4,routine_code), defer_s4));
        defer_self5 = self;
        mang_self8 = self;
        mang_ob8 = ((OB) e);
        mang_ns3 = ((OB) ATTR(self,current_sig));
        ret_val48 = MANGLE119219986(ATTR(mang_self8,mangler), mang_ob8, mang_ns3);
        plus_self20 = ret_val48;
        plus_sarg16 = ((STR) &name91);
        ret_val49 = STR_ap2004550586(plus_self20, plus_sarg16);
        plus_self21 = ret_val49;
        plus_sarg17 = tname;
        ret_val50 = STR_ap2004550586(plus_self21, plus_sarg17);
        plus_self22 = ret_val50;
        plus_arg4 = ';';
        str_self7 = plus_arg4;
        create_self5 = ((STR) NULL);
        create_c1 = str_self7;
        L101 = 1;
        L103=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L101)*sizeof(CHAR);
        L102=ZALLOC_LEAF_BIG(L103);
        if (L102==NULL) FATAL("Unable to allocate more memory");
        memset(L102,0,L103);
        ((OB)L102)->header.tag=STR_tag;
#ifdef DESTROY_CHK

          ((OB)L102)->header.destroyed=0;
#endif

        L100 = ((STR) L102);
        L100->asize = L101;
        r3 = L100;
        SARR((STR)r3,0,create_c1); 
        ;
        ret_val53 = r3;
        ret_val52 = ret_val53;
        s4 = ret_val52;
        ret_val51 = STR_ap1077157958(plus_self22, s4, TRUE);
        defer_s5 = ret_val51;
        SATTR(defer_self5,routine_code,FSTR_p1752847026(ATTR(defer_self5,routine_code), defer_s5));
       }
      }
     }
    }
    else {
     if ((ATTR(e,uniq)==((STR) NULL))) {
      mang_self9 = self;
      builtin_cntr_sel = self;
      L105 = ATTR(builtin_cntr_sel,builtin_cntr_n);
      L1061_=INTPLUS(L105,1); 
      SATTR(builtin_cntr_sel,builtin_cntr_n,L1061_);
      ret_val54 = ATTR(builtin_cntr_sel,builtin_cntr_n);
      L108=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
      if (L108==NULL) FATAL("Unable to allocate more memory");
      memset(L108,0,sizeof(struct INT_boxed_struct));
      ((OB)L108)->header.tag=INT_tag;
      L107 = (OB)((INT_boxed) L108);
      ((INT_boxed) L107)->immutable_part = ret_val54;
      mang_ob9 = L107;
      mang_ns4 = ((OB) ATTR(self,current_sig));
      ret_val55 = MANGLE119219986(ATTR(mang_self9,mangler), mang_ob9, mang_ns4);
      plus_self23 = ret_val55;
      plus_sarg18 = ((STR) &S_);
      ret_val56 = STR_ap2004550586(plus_self23, plus_sarg18);
      SATTR(e,uniq,ret_val56);
     }
     L109 = (ni==((FLISTA1062334999) NULL));
     L1101_=!(L109); 
     if (L1101_) {
      ni = FLISTA943034044(ni, e);
      plus_self24 = ((STR) &framenested);
      plus_arg5 = FLISTA1962912942(ni);
      str_self8 = plus_arg5;
      clear_self3 = buf4;
      L111 = (clear_self3==((FSTR) NULL));
      L1121_=!(L111); 
      if (L1121_) {
       SATTR(clear_self3,loc,0);
      }
      str_in_self3 = str_self8;
      str_in_s3 = buf4;
      ret_val59 = INT_st367594495(str_in_self3, str_in_s3, 0, 10, ' ');
      buf4 = ret_val59;
      str_self9 = buf4;
      ret_val60 = STR_fr1097270334(((STR) NULL), str_self9);
      ret_val58 = ret_val60;
      s5 = ret_val58;
      ret_val57 = STR_ap1077157958(plus_self24, s5, TRUE);
      MANGLE1827205184(ATTR(self,mangler), ((OB) e), ret_val57, ((OB) ATTR(self,current_sig)));
     }
     is_iter_self2 = ATTR(self,current_sig);
     is_iter_self3 = ATTR(is_iter_self2,name1);
     L114 = (is_iter_self3.str==((STR) NULL));
     L1151_=!(L114); 
     if (L1151_) {
      L116 = is_iter_self3.str;
      L117 = STR_sizerINT(is_iter_self3.str);
      L1181_=INTMINUS(L117,1); 
      L119 = L1181_;
      L1201_=ARR((STR)L116,L119); 
      L122 = L1201_;
      L1231_=L122=='!'; 
      L113 = L1231_;
     } else {
      L113 = FALSE;
     }
     ret_val62 = L113;
     ret_val61 = ret_val62;
     L124 = ret_val61;
     L1251_=!(L124); 
     if (L1251_) {
      ndefer_self7 = self;
      plus_self25 = ((STR) &BOOL2);
      plus_sarg19 = ((STR) &f_);
      ret_val63 = STR_ap2004550586(plus_self25, plus_sarg19);
      plus_self26 = ret_val63;
      plus_sarg20 = ATTR(e,uniq);
      ret_val64 = STR_ap2004550586(plus_self26, plus_sarg20);
      plus_self27 = ret_val64;
      plus_sarg21 = ((STR) &TRUE3);
      ret_val65 = STR_ap2004550586(plus_self27, plus_sarg21);
      ndefer_s7 = ret_val65;
      CGEN_d695831906(ndefer_self7);
      SATTR(ndefer_self7,routine_code,FSTR_p1752847026(ATTR(ndefer_self7,routine_code), ndefer_s7));
     }
    }
   }
  }
  after_loop: ;
  L126 = (ATTR(arg,init)==((dAM_STMT) NULL));
  L1271_=!(L126); 
  if (L1271_) {
   CGEN_e494460490(self, ATTR(arg,init));
   SATTR(arg,init,((dAM_STMT) NULL));
  }
  {
   struct FLISTA20621028_frame_struct other2_0;
FLISTA20621028_frame noname2 = &other2_0;
   L21 = ATTR(arg,its1);
   noname2->self = L21;
   noname2->state = 0;
   while (1) {
    L128 = FLISTA20621028(noname2);
    if (noname2->state == -1) {
     goto after_loop1;
    }
    d = L128;
    L129 = ATTR(ATTR(d,fun),tp);
    if ((*dTP_is1811059018[TAG(L129)])(L129)) {
     plus_self28 = ((STR) &name24);
     mang_self10 = self;
     mang_ob10 = ((OB) ATTR(d,fun));
     ret_val66 = MANGLE119219986(ATTR(mang_self10,mangler), mang_ob10, ((OB) NULL));
     plus_sarg22 = ret_val66;
     ret_val67 = STR_ap2004550586(plus_self28, plus_sarg22);
     plus_self29 = ret_val67;
     plus_sarg23 = ((STR) &S_frame2);
     ret_val68 = STR_ap2004550586(plus_self29, plus_sarg23);
     abs_frame_cast = ret_val68;
     self_ob = CGEN_d1425664957(self, ATTR(ATTR(d,fun),tp));
     L1301_=(AM_CALL_ARG)ARR((AM_ITE1809135850)d,0); 
     L131=L1301_;
     the_self = CGEN_e1639504572(self, ATTR(L131,expr));
     init_self = ATTR(d,init);
     if (init_self==NULL) {
      goto other381;
     } else
     switch (TAG(init_self)) {
      case AM_ASSIGN_STMT_tag:
       CGEN_e1389302728(self, ((AM_ASSIGN_STMT) init_self)); break;
      default: ;
      other381: ;
     }
     ndefer_self8 = self;
     ndefer_s8 = STR_ap1417971546(self_ob, ((STR) &name36), the_self, ((STR) &name16));
     CGEN_d695831906(ndefer_self8);
     SATTR(ndefer_self8,routine_code,FSTR_p1752847026(ATTR(ndefer_self8,routine_code), ndefer_s8));
     if (ATTR(self,chk_void)) {
      L133 = ATTR(self,null_segfaults);
      L1341_=!(L133); 
      L132 = L1341_;
     } else {
      L132 = FALSE;
     }
     if (L132) {
      ndefer_self9 = self;
      ndefer_s9 = STR_ap1693864410(((STR) &if3), self_ob, ((STR) &NULL3));
      CGEN_d695831906(ndefer_self9);
      SATTR(ndefer_self9,routine_code,FSTR_p1752847026(ATTR(ndefer_self9,routine_code), ndefer_s9));
      in_self2 = self;
      L135 = ATTR(in_self2,indent);
      L1361_=INTPLUS(L135,1); 
      SATTR(in_self2,indent,L1361_);
      runtime_error_se = self;
      plus_self30 = ((STR) &Dispatchedcallto);
      plus_sarg24 = ATTR(ATTR(d,fun),str);
      ret_val69 = STR_ap2004550586(plus_self30, plus_sarg24);
      plus_self31 = ret_val69;
      plus_sarg25 = ((STR) &onvoidselfin);
      ret_val70 = STR_ap2004550586(plus_self31, plus_sarg25);
      plus_self32 = ret_val70;
      plus_sarg26 = ATTR(self,current_function);
      ret_val71 = STR_ap2004550586(plus_self32, plus_sarg26);
      runtime_error_s = ret_val71;
      ndefer_self10 = runtime_error_se;
      plus_self33 = ((STR) &FATAL1);
      plus_sarg27 = runtime_error_s;
      ret_val72 = STR_ap2004550586(plus_self33, plus_sarg27);
      plus_self34 = ret_val72;
      plus_sarg28 = ((STR) &name95);
      ret_val73 = STR_ap2004550586(plus_self34, plus_sarg28);
      ndefer_s10 = ret_val73;
      CGEN_d695831906(ndefer_self10);
      SATTR(ndefer_self10,routine_code,FSTR_p1752847026(ATTR(ndefer_self10,routine_code), ndefer_s10));
      move_out_self = self;
      L137 = ATTR(move_out_self,indent);
      L1381_=INTMINUS(L137,1); 
      SATTR(move_out_self,indent,L1381_);
      ndefer_self11 = self;
      ndefer_s11 = ((STR) &name4);
      CGEN_d695831906(ndefer_self11);
      SATTR(ndefer_self11,routine_code,FSTR_p1752847026(ATTR(ndefer_self11,routine_code), ndefer_s11));
     }
     tag1 = ((STR) &TAG2);
     if (ATTR(ATTR(self,prog),distributed)) {
      tag1 = ((STR) &F_TAG2);
     }
     ndefer_self12 = self;
     mang_self11 = self;
     mang_ob11 = ((OB) d);
     mang_ns5 = ((OB) ATTR(self,current_sig));
     ret_val74 = MANGLE119219986(ATTR(mang_self11,mangler), mang_ob11, mang_ns5);
     plus_self35 = ret_val74;
     plus_sarg29 = ((STR) &name36);
     ret_val75 = STR_ap2004550586(plus_self35, plus_sarg29);
     plus_self36 = ret_val75;
     plus_sarg30 = abs_frame_cast;
     ret_val76 = STR_ap2004550586(plus_self36, plus_sarg30);
     plus_self37 = ret_val76;
     plus_sarg31 = ((STR) &name85);
     ret_val77 = STR_ap2004550586(plus_self37, plus_sarg31);
     plus_self38 = ret_val77;
     mang_self12 = self;
     mang_ob12 = ((OB) ATTR(d,fun));
     ret_val78 = MANGLE119219986(ATTR(mang_self12,mangler), mang_ob12, ((OB) NULL));
     plus_sarg32 = ret_val78;
     ret_val79 = STR_ap2004550586(plus_self38, plus_sarg32);
     plus_self39 = ret_val79;
     plus_sarg33 = ((STR) &name14);
     ret_val80 = STR_ap2004550586(plus_self39, plus_sarg33);
     plus_self40 = ret_val80;
     plus_sarg34 = tag1;
     ret_val81 = STR_ap2004550586(plus_self40, plus_sarg34);
     plus_self41 = ret_val81;
     plus_sarg35 = ((STR) &name24);
     ret_val82 = STR_ap2004550586(plus_self41, plus_sarg35);
     plus_self42 = ret_val82;
     plus_sarg36 = self_ob;
     ret_val83 = STR_ap2004550586(plus_self42, plus_sarg36);
     plus_self43 = ret_val83;
     plus_sarg37 = ((STR) &alloc_frame);
     ret_val84 = STR_ap2004550586(plus_self43, plus_sarg37);
     ndefer_s12 = ret_val84;
     CGEN_d695831906(ndefer_self12);
     SATTR(ndefer_self12,routine_code,FSTR_p1752847026(ATTR(ndefer_self12,routine_code), ndefer_s12));
    }
   }
  }
  after_loop1: ;
  L139 = (ni==((FLISTA1062334999) NULL));
  L1401_=!(L139); 
  if (L1401_) {
   fnamecount = 0;
   {
    struct FLISTA20621028_frame_struct other3_0;
FLISTA20621028_frame noname3 = &other3_0;
    L31 = ATTR(arg,its1);
    noname3->self = L31;
    noname3->state = 0;
    while (1) {
     L141 = FLISTA20621028(noname3);
     if (noname3->state == -1) {
      goto after_loop2;
     }
     e1 = L141;
     is_builtin_iter_ = ATTR(e1,fun);
     if (SIG_is418491101(is_builtin_iter_)) {
      is_iter_self4 = is_builtin_iter_;
      is_iter_self5 = ATTR(is_iter_self4,name1);
      L144 = (is_iter_self5.str==((STR) NULL));
      L1451_=!(L144); 
      if (L1451_) {
       L146 = is_iter_self5.str;
       L147 = STR_sizerINT(is_iter_self5.str);
       L1481_=INTMINUS(L147,1); 
       L149 = L1481_;
       L1501_=ARR((STR)L146,L149); 
       L151 = L1501_;
       L1521_=L151=='!'; 
       L143 = L1521_;
      } else {
       L143 = FALSE;
      }
      ret_val87 = L143;
      ret_val86 = ret_val87;
      L142 = ret_val86;
     } else {
      L142 = FALSE;
     }
     ret_val85 = L142;
     L153 = ret_val85;
     L1541_=!(L153); 
     if (L1541_) {
      L156 = ATTR(arg,has_yield);
      L1571_=!(L156); 
      if (L1571_) {
       L158 = ATTR(ATTR(e1,fun),tp);
       L159 = (*dTP_is1811059018[TAG(L158)])(L158);
       L1601_=!(L159); 
       L155 = L1601_;
      } else {
       L155 = FALSE;
      }
      if (L155) {
       ndefer_self13 = self;
       mang_self13 = self;
       mang_ob13 = ((OB) e1);
       mang_ns6 = ((OB) ATTR(self,current_sig));
       ret_val88 = MANGLE119219986(ATTR(mang_self13,mangler), mang_ob13, mang_ns6);
       plus_self44 = ret_val88;
       plus_sarg38 = ((STR) &name91);
       ret_val89 = STR_ap2004550586(plus_self44, plus_sarg38);
       plus_self45 = ret_val89;
       plus_sarg39 = fname;
       ret_val90 = STR_ap2004550586(plus_self45, plus_sarg39);
       plus_self46 = ret_val90;
       plus_arg6 = '_';
       str_self10 = plus_arg6;
       create_self6 = ((STR) NULL);
       create_c2 = str_self10;
       L162 = 1;
       L164=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L162)*sizeof(CHAR);
       L163=ZALLOC_LEAF_BIG(L164);
       if (L163==NULL) FATAL("Unable to allocate more memory");
       memset(L163,0,L164);
       ((OB)L163)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L163)->header.destroyed=0;
#endif

       L161 = ((STR) L163);
       L161->asize = L162;
       r4 = L161;
       SARR((STR)r4,0,create_c2); 
       ;
       ret_val93 = r4;
       ret_val92 = ret_val93;
       s6 = ret_val92;
       ret_val91 = STR_ap1077157958(plus_self46, s6, TRUE);
       plus_self47 = ret_val91;
       plus_arg7 = fnamecount;
       str_self11 = plus_arg7;
       clear_self4 = buf5;
       L166 = (clear_self4==((FSTR) NULL));
       L1671_=!(L166); 
       if (L1671_) {
        SATTR(clear_self4,loc,0);
       }
       str_in_self4 = str_self11;
       str_in_s4 = buf5;
       ret_val96 = INT_st367594495(str_in_self4, str_in_s4, 0, 10, ' ');
       buf5 = ret_val96;
       str_self12 = buf5;
       ret_val97 = STR_fr1097270334(((STR) NULL), str_self12);
       ret_val95 = ret_val97;
       s7 = ret_val95;
       ret_val94 = STR_ap1077157958(plus_self47, s7, TRUE);
       plus_self48 = ret_val94;
       plus_arg8 = ';';
       str_self13 = plus_arg8;
       create_self7 = ((STR) NULL);
       create_c3 = str_self13;
       L169 = 1;
       L171=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L169)*sizeof(CHAR);
       L170=ZALLOC_LEAF_BIG(L171);
       if (L170==NULL) FATAL("Unable to allocate more memory");
       memset(L170,0,L171);
       ((OB)L170)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L170)->header.destroyed=0;
#endif

       L168 = ((STR) L170);
       L168->asize = L169;
       r5 = L168;
       SARR((STR)r5,0,create_c3); 
       ;
       ret_val100 = r5;
       ret_val99 = ret_val100;
       s8 = ret_val99;
       ret_val98 = STR_ap1077157958(plus_self48, s8, TRUE);
       ndefer_s13 = ret_val98;
       CGEN_d695831906(ndefer_self13);
       SATTR(ndefer_self13,routine_code,FSTR_p1752847026(ATTR(ndefer_self13,routine_code), ndefer_s13));
       L1731_=INTPLUS(fnamecount,1); 
       fnamecount = L1731_;
      }
     }
    }
   }
   after_loop2: ;
  }
  {
   struct FLISTA20621028_frame_struct other4_0;
FLISTA20621028_frame noname4 = &other4_0;
   L41 = ATTR(arg,its1);
   noname4->self = L41;
   noname4->state = 0;
   while (1) {
    L174 = FLISTA20621028(noname4);
    if (noname4->state == -1) {
     goto after_loop3;
    }
    it = L174;
    if (ATTR(it,init_before_loop)) {
     CGEN_e844901491(self, it);
    }
    if (SIG_is418491101(ATTR(it,fun))) {
     is_iter_self6 = ATTR(self,current_sig);
     is_iter_self7 = ATTR(is_iter_self6,name1);
     L176 = (is_iter_self7.str==((STR) NULL));
     L1771_=!(L176); 
     if (L1771_) {
      L178 = is_iter_self7.str;
      L179 = STR_sizerINT(is_iter_self7.str);
      L1801_=INTMINUS(L179,1); 
      L182 = L1801_;
      L1831_=ARR((STR)L178,L182); 
      L184 = L1831_;
      L1851_=L184=='!'; 
      L175 = L1851_;
     } else {
      L175 = FALSE;
     }
     ret_val102 = L175;
     ret_val101 = ret_val102;
     if (ret_val101) {
      ndefer_self14 = self;
      plus_self49 = CGEN_i87475608(self);
      plus_sarg40 = ((STR) &f_);
      ret_val103 = STR_ap2004550586(plus_self49, plus_sarg40);
      plus_self50 = ret_val103;
      plus_sarg41 = ATTR(it,uniq);
      ret_val104 = STR_ap2004550586(plus_self50, plus_sarg41);
      plus_self51 = ret_val104;
      plus_sarg42 = ((STR) &TRUE3);
      ret_val105 = STR_ap2004550586(plus_self51, plus_sarg42);
      ndefer_s14 = ret_val105;
      CGEN_d695831906(ndefer_self14);
      SATTR(ndefer_self14,routine_code,FSTR_p1752847026(ATTR(ndefer_self14,routine_code), ndefer_s14));
     }
    }
    else {
     ndefer_self15 = self;
     mang_self14 = self;
     mang_ob14 = ((OB) it);
     mang_ns7 = ((OB) ATTR(self,current_sig));
     ret_val106 = MANGLE119219986(ATTR(mang_self14,mangler), mang_ob14, mang_ns7);
     plus_self52 = ret_val106;
     plus_sarg43 = ((STR) &state01);
     ret_val107 = STR_ap2004550586(plus_self52, plus_sarg43);
     ndefer_s15 = ret_val107;
     CGEN_d695831906(ndefer_self15);
     SATTR(ndefer_self15,routine_code,FSTR_p1752847026(ATTR(ndefer_self15,routine_code), ndefer_s15));
    }
   }
  }
  after_loop3: ;
 }
 else {
  L186 = (ATTR(arg,init)==((dAM_STMT) NULL));
  L1871_=!(L186); 
  if (L1871_) {
   CGEN_e494460490(self, ATTR(arg,init));
   SATTR(arg,init,((dAM_STMT) NULL));
  }
 }
 L188 = (ATTR(arg,firsts)==((FLISTA725283029) NULL));
 L1891_=!(L188); 
 if (L1891_) {
  {
   struct FLISTA2119642552_frame_struct other5_0;
FLISTA2119642552_frame noname5 = &other5_0;
   L51 = ATTR(arg,firsts);
   noname5->self = L51;
   noname5->state = 0;
   while (1) {
    L190 = FLISTA2119642552(noname5);
    if (noname5->state == -1) {
     goto after_loop4;
    }
    e2 = L190;
    ndefer_self16 = self;
    plus_self53 = CGEN_e236145586(self, e2);
    plus_sarg44 = ((STR) &TRUE3);
    ret_val108 = STR_ap2004550586(plus_self53, plus_sarg44);
    ndefer_s16 = ret_val108;
    CGEN_d695831906(ndefer_self16);
    SATTR(ndefer_self16,routine_code,FSTR_p1752847026(ATTR(ndefer_self16,routine_code), ndefer_s16));
   }
  }
  after_loop4: ;
 }
 SATTR(self,nested_its,FMAPAM293039037(ATTR(self,nested_its), ATTR(self,current_am_rout_), ni));
 L191 = (ATTR(arg,bits1)==((FLISTA1409846210) NULL));
 L1921_=!(L191); 
 if (L1921_) {
  CGEN_d695831906(self);
  comment_self2 = self;
  comment_com2 = ((STR) &Binary376592169);
  if (ATTR(comment_self2,pretty)) {
   ndefer_self17 = comment_self2;
   plus_self54 = ((STR) &name54);
   plus_sarg45 = comment_com2;
   ret_val109 = STR_ap2004550586(plus_self54, plus_sarg45);
   plus_self55 = ret_val109;
   plus_sarg46 = ((STR) &name79);
   ret_val110 = STR_ap2004550586(plus_self55, plus_sarg46);
   ndefer_s17 = ret_val110;
   CGEN_d695831906(ndefer_self17);
   SATTR(ndefer_self17,routine_code,FSTR_p1752847026(ATTR(ndefer_self17,routine_code), ndefer_s17));
  }
  {
   struct FLISTA1446310868_frame_struct other6_0;
FLISTA1446310868_frame noname6 = &other6_0;
   L61 = ATTR(arg,bits1);
   noname6->self = L61;
   noname6->state = 0;
   while (1) {
    L193 = FLISTA1446310868(noname6);
    if (noname6->state == -1) {
     goto after_loop5;
    }
    e3 = L193;
    L195 = (nbi==((FLISTA1409846210) NULL));
    L1961_=!(L195); 
    if (L1961_) {
     is_iter_self8 = ATTR(self,current_sig);
     is_iter_self9 = ATTR(is_iter_self8,name1);
     L198 = (is_iter_self9.str==((STR) NULL));
     L1991_=!(L198); 
     if (L1991_) {
      L200 = is_iter_self9.str;
      L202 = STR_sizerINT(is_iter_self9.str);
      L2031_=INTMINUS(L202,1); 
      L204 = L2031_;
      L2051_=ARR((STR)L200,L204); 
      L206 = L2051_;
      L2071_=L206=='!'; 
      L197 = L2071_;
     } else {
      L197 = FALSE;
     }
     ret_val112 = L197;
     ret_val1111 = ret_val112;
     L194 = ret_val1111;
    } else {
     L194 = FALSE;
    }
    if (L194) {
     nbi = FLISTA1715286767(nbi, e3);
     plus_self56 = ((STR) &framen219781171);
     plus_arg9 = FLISTA2089597941(nbi);
     str_self14 = plus_arg9;
     clear_self5 = buf6;
     L208 = (clear_self5==((FSTR) NULL));
     L2091_=!(L208); 
     if (L2091_) {
      SATTR(clear_self5,loc,0);
     }
     str_in_self5 = str_self14;
     str_in_s5 = buf6;
     ret_val115 = INT_st367594495(str_in_self5, str_in_s5, 0, 10, ' ');
     buf6 = ret_val115;
     str_self15 = buf6;
     ret_val116 = STR_fr1097270334(((STR) NULL), str_self15);
     ret_val114 = ret_val116;
     s9 = ret_val114;
     ret_val113 = STR_ap1077157958(plus_self56, s9, TRUE);
     MANGLE1827205184(ATTR(self,mangler), ((OB) e3), ret_val113, ((OB) ATTR(self,current_sig)));
     plus_self57 = ((STR) &framen219781171);
     plus_arg10 = FLISTA2089597941(nbi);
     str_self16 = plus_arg10;
     clear_self6 = buf7;
     L210 = (clear_self6==((FSTR) NULL));
     L2111_=!(L210); 
     if (L2111_) {
      SATTR(clear_self6,loc,0);
     }
     str_in_self6 = str_self16;
     str_in_s6 = buf7;
     ret_val119 = INT_st367594495(str_in_self6, str_in_s6, 0, 10, ' ');
     buf7 = ret_val119;
     str_self17 = buf7;
     ret_val120 = STR_fr1097270334(((STR) NULL), str_self17);
     ret_val118 = ret_val120;
     s10 = ret_val118;
     ret_val117 = STR_ap1077157958(plus_self57, s10, TRUE);
     name111 = ret_val117;
     bnd_iter_expr = CGEN_e1639504572(self, ATTR(e3,bi));
     genlocal_self = ATTR(self,mangler);
     genlocal_ns = ((OB) ATTR(self,current_sig));
     space_self1 = genlocal_self;
     space_ns1 = genlocal_ns;
     if ((space_ns1==((OB) NULL))) {
      space_ns1 = ((OB) space_self1);
     }
     r6 = FMAPdO790518483(ATTR(space_self1,namespaces), space_ns1);
     if ((r6==((NAMESPACE) NULL))) {
      create_self8 = ((NAMESPACE) NULL);
      L213=ZALLOC(sizeof(struct NAMESPACE_struct));
      if (L213==NULL) FATAL("Unable to allocate more memory");
      ((OB)L213)->header.tag=NAMESPACE_tag;
      L212 = ((NAMESPACE) L213);
      r7 = L212;
      create_self9 = ((NAMEMAP) NULL);
      ret_val124 = ((NAMEMAP) NULL);
      SATTR(r7,map,ret_val124);
      create_self10 = ((FSETSTR) NULL);
      ret_val125 = ((FSETSTR) NULL);
      SATTR(r7,set,ret_val125);
      SATTR(r7,counter,0);
      create_self11 = ((FMAPSTRINT) NULL);
      ret_val126 = ((FMAPSTRINT) NULL);
      SATTR(r7,uniques,ret_val126);
      ret_val123 = r7;
      r6 = ret_val123;
      SATTR(space_self1,namespaces,FMAPdO1199466663(ATTR(space_self1,namespaces), space_ns1, r6));
     }
     ret_val122 = r6;
     name311 = NAMESP180237388(ret_val122, ((STR) &L));
     SATTR(genlocal_self,used_by_local,FSETST1404644833(ATTR(genlocal_self,used_by_local), name311));
     ret_val121 = name311;
     l = ret_val121;
     plus_self58 = ATTR(self,code_c);
     plus_s = ((dSTR) CGEN_eolrSTR(self));
     L214 = (plus_s==((dSTR) NULL));
     L2151_=!(L214); 
     if (L2151_) {
      L216 = ATTR(plus_self58,ntext);
      L217 = plus_s;
      SATTR(plus_self58,ntext,FSTR_p1752847026(L216, (*dSTR_strrSTR[TAG(L217)])(L217)));
     }
     ret_val127 = plus_self58;
     plus_self59 = ret_val127;
     L219=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L219==NULL) FATAL("Unable to allocate more memory");
     memset(L219,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L219)->header.tag=CHAR_tag;
     L218 = (dSTR)((CHAR_boxed) L219);
     ((CHAR_boxed) L218)->immutable_part = ' ';
     plus_s1 = L218;
     L220 = (plus_s1==((dSTR) NULL));
     L2211_=!(L220); 
     if (L2211_) {
      L222 = ATTR(plus_self59,ntext);
      L223 = plus_s1;
      SATTR(plus_self59,ntext,FSTR_p1752847026(L222, (*dSTR_strrSTR[TAG(L223)])(L223)));
     }
     ret_val128 = plus_self59;
     plus_self60 = ret_val128;
     mang_self15 = self;
     mang_ob15 = ((OB) ATTR(e3,bi_tp));
     ret_val129 = MANGLE119219986(ATTR(mang_self15,mangler), mang_ob15, ((OB) NULL));
     plus_s2 = ((dSTR) ret_val129);
     L224 = (plus_s2==((dSTR) NULL));
     L2251_=!(L224); 
     if (L2251_) {
      L226 = ATTR(plus_self60,ntext);
      L227 = plus_s2;
      SATTR(plus_self60,ntext,FSTR_p1752847026(L226, (*dSTR_strrSTR[TAG(L227)])(L227)));
     }
     ret_val130 = plus_self60;
     plus_self61 = ret_val130;
     L229=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L229==NULL) FATAL("Unable to allocate more memory");
     memset(L229,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L229)->header.tag=CHAR_tag;
     L228 = (dSTR)((CHAR_boxed) L229);
     ((CHAR_boxed) L228)->immutable_part = ' ';
     plus_s3 = L228;
     L230 = (plus_s3==((dSTR) NULL));
     L2321_=!(L230); 
     if (L2321_) {
      L233 = ATTR(plus_self61,ntext);
      L234 = plus_s3;
      SATTR(plus_self61,ntext,FSTR_p1752847026(L233, (*dSTR_strrSTR[TAG(L234)])(L234)));
     }
     ret_val131 = plus_self61;
     plus_self62 = ret_val131;
     plus_s4 = ((dSTR) l);
     L235 = (plus_s4==((dSTR) NULL));
     L2361_=!(L235); 
     if (L2361_) {
      L237 = ATTR(plus_self62,ntext);
      L238 = plus_s4;
      SATTR(plus_self62,ntext,FSTR_p1752847026(L237, (*dSTR_strrSTR[TAG(L238)])(L238)));
     }
     ret_val132 = plus_self62;
     plus_self63 = ret_val132;
     plus_s5 = ((dSTR) ((STR) &name16));
     L239 = ATTR(plus_self63,ntext);
     L240 = plus_s5;
     SATTR(plus_self63,ntext,FSTR_p1752847026(L239, (*dSTR_strrSTR[TAG(L240)])(L240)));
     ndefer_self18 = self;
     plus_self64 = l;
     plus_sarg47 = ((STR) &name36);
     ret_val133 = STR_ap2004550586(plus_self64, plus_sarg47);
     plus_self65 = ret_val133;
     plus_sarg48 = bnd_iter_expr;
     ret_val134 = STR_ap2004550586(plus_self65, plus_sarg48);
     plus_self66 = ret_val134;
     plus_sarg49 = ((STR) &name16);
     ret_val135 = STR_ap2004550586(plus_self66, plus_sarg49);
     ndefer_s18 = ret_val135;
     CGEN_d695831906(ndefer_self18);
     SATTR(ndefer_self18,routine_code,FSTR_p1752847026(ATTR(ndefer_self18,routine_code), ndefer_s18));
     ndefer_self19 = self;
     plus_self67 = name111;
     plus_sarg50 = ((STR) &ZALLOC2);
     ret_val136 = STR_ap2004550586(plus_self67, plus_sarg50);
     plus_self68 = ret_val136;
     plus_sarg51 = l;
     ret_val137 = STR_ap2004550586(plus_self68, plus_sarg51);
     plus_self69 = ret_val137;
     plus_sarg52 = ((STR) &size4);
     ret_val138 = STR_ap2004550586(plus_self69, plus_sarg52);
     ndefer_s19 = ret_val138;
     CGEN_d695831906(ndefer_self19);
     SATTR(ndefer_self19,routine_code,FSTR_p1752847026(ATTR(ndefer_self19,routine_code), ndefer_s19));
     ndefer_self20 = self;
     plus_self70 = name111;
     plus_sarg53 = ((STR) &name80);
     ret_val139 = STR_ap2004550586(plus_self70, plus_sarg53);
     plus_self71 = ret_val139;
     mang_self16 = self;
     mang_ob16 = ((OB) ATTR(e3,bi_tp));
     ret_val140 = MANGLE119219986(ATTR(mang_self16,mangler), mang_ob16, ((OB) NULL));
     plus_sarg54 = ret_val140;
     ret_val141 = STR_ap2004550586(plus_self71, plus_sarg54);
     plus_self72 = ret_val141;
     plus_sarg55 = ((STR) &memcpy1);
     ret_val142 = STR_ap2004550586(plus_self72, plus_sarg55);
     plus_self73 = ret_val142;
     plus_sarg56 = name111;
     ret_val143 = STR_ap2004550586(plus_self73, plus_sarg56);
     plus_self74 = ret_val143;
     plus_sarg57 = ((STR) &name3);
     ret_val144 = STR_ap2004550586(plus_self74, plus_sarg57);
     plus_self75 = ret_val144;
     plus_sarg58 = l;
     ret_val145 = STR_ap2004550586(plus_self75, plus_sarg58);
     plus_self76 = ret_val145;
     plus_sarg59 = ((STR) &name3);
     ret_val146 = STR_ap2004550586(plus_self76, plus_sarg59);
     plus_self77 = ret_val146;
     plus_sarg60 = l;
     ret_val147 = STR_ap2004550586(plus_self77, plus_sarg60);
     plus_self78 = ret_val147;
     plus_sarg61 = ((STR) &size4);
     ret_val148 = STR_ap2004550586(plus_self78, plus_sarg61);
     ndefer_s20 = ret_val148;
     CGEN_d695831906(ndefer_self20);
     SATTR(ndefer_self20,routine_code,FSTR_p1752847026(ATTR(ndefer_self20,routine_code), ndefer_s20));
     CGEN_d695831906(self);
    }
    else {
     bnd_iter_expr1 = CGEN_e1639504572(self, ATTR(e3,bi));
     genlocal_self1 = ATTR(self,mangler);
     genlocal_ns1 = ((OB) ATTR(self,current_sig));
     space_self2 = genlocal_self1;
     space_ns2 = genlocal_ns1;
     if ((space_ns2==((OB) NULL))) {
      space_ns2 = ((OB) space_self2);
     }
     r8 = FMAPdO790518483(ATTR(space_self2,namespaces), space_ns2);
     if ((r8==((NAMESPACE) NULL))) {
      create_self12 = ((NAMESPACE) NULL);
      L242=ZALLOC(sizeof(struct NAMESPACE_struct));
      if (L242==NULL) FATAL("Unable to allocate more memory");
      ((OB)L242)->header.tag=NAMESPACE_tag;
      L241 = ((NAMESPACE) L242);
      r9 = L241;
      create_self13 = ((NAMEMAP) NULL);
      ret_val152 = ((NAMEMAP) NULL);
      SATTR(r9,map,ret_val152);
      create_self14 = ((FSETSTR) NULL);
      ret_val153 = ((FSETSTR) NULL);
      SATTR(r9,set,ret_val153);
      SATTR(r9,counter,0);
      create_self15 = ((FMAPSTRINT) NULL);
      ret_val154 = ((FMAPSTRINT) NULL);
      SATTR(r9,uniques,ret_val154);
      ret_val151 = r9;
      r8 = ret_val151;
      SATTR(space_self2,namespaces,FMAPdO1199466663(ATTR(space_self2,namespaces), space_ns2, r8));
     }
     ret_val150 = r8;
     name411 = NAMESP180237388(ret_val150, ((STR) &L));
     SATTR(genlocal_self1,used_by_local,FSETST1404644833(ATTR(genlocal_self1,used_by_local), name411));
     ret_val149 = name411;
     l1 = ret_val149;
     plus_self79 = ATTR(self,code_c);
     plus_s6 = ((dSTR) CGEN_eolrSTR(self));
     L243 = (plus_s6==((dSTR) NULL));
     L2441_=!(L243); 
     if (L2441_) {
      L245 = ATTR(plus_self79,ntext);
      L246 = plus_s6;
      SATTR(plus_self79,ntext,FSTR_p1752847026(L245, (*dSTR_strrSTR[TAG(L246)])(L246)));
     }
     ret_val155 = plus_self79;
     plus_self80 = ret_val155;
     L248=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L248==NULL) FATAL("Unable to allocate more memory");
     memset(L248,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L248)->header.tag=CHAR_tag;
     L247 = (dSTR)((CHAR_boxed) L248);
     ((CHAR_boxed) L247)->immutable_part = ' ';
     plus_s7 = L247;
     L249 = (plus_s7==((dSTR) NULL));
     L2501_=!(L249); 
     if (L2501_) {
      L251 = ATTR(plus_self80,ntext);
      L252 = plus_s7;
      SATTR(plus_self80,ntext,FSTR_p1752847026(L251, (*dSTR_strrSTR[TAG(L252)])(L252)));
     }
     ret_val156 = plus_self80;
     plus_self81 = ret_val156;
     mang_self17 = self;
     mang_ob17 = ((OB) ATTR(e3,bi_tp));
     ret_val157 = MANGLE119219986(ATTR(mang_self17,mangler), mang_ob17, ((OB) NULL));
     plus_s8 = ((dSTR) ret_val157);
     L253 = (plus_s8==((dSTR) NULL));
     L2541_=!(L253); 
     if (L2541_) {
      L255 = ATTR(plus_self81,ntext);
      L256 = plus_s8;
      SATTR(plus_self81,ntext,FSTR_p1752847026(L255, (*dSTR_strrSTR[TAG(L256)])(L256)));
     }
     ret_val158 = plus_self81;
     plus_self82 = ret_val158;
     L258=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L258==NULL) FATAL("Unable to allocate more memory");
     memset(L258,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L258)->header.tag=CHAR_tag;
     L257 = (dSTR)((CHAR_boxed) L258);
     ((CHAR_boxed) L257)->immutable_part = ' ';
     plus_s9 = L257;
     L259 = (plus_s9==((dSTR) NULL));
     L2601_=!(L259); 
     if (L2601_) {
      L261 = ATTR(plus_self82,ntext);
      L262 = plus_s9;
      SATTR(plus_self82,ntext,FSTR_p1752847026(L261, (*dSTR_strrSTR[TAG(L262)])(L262)));
     }
     ret_val159 = plus_self82;
     plus_self83 = ret_val159;
     plus_s10 = ((dSTR) l1);
     L263 = (plus_s10==((dSTR) NULL));
     L2641_=!(L263); 
     if (L2641_) {
      L265 = ATTR(plus_self83,ntext);
      L266 = plus_s10;
      SATTR(plus_self83,ntext,FSTR_p1752847026(L265, (*dSTR_strrSTR[TAG(L266)])(L266)));
     }
     ret_val160 = plus_self83;
     plus_self84 = ret_val160;
     plus_s11 = ((dSTR) ((STR) &name16));
     L267 = ATTR(plus_self84,ntext);
     L268 = plus_s11;
     SATTR(plus_self84,ntext,FSTR_p1752847026(L267, (*dSTR_strrSTR[TAG(L268)])(L268)));
     ndefer_self21 = self;
     plus_self85 = l1;
     plus_sarg62 = ((STR) &name36);
     ret_val161 = STR_ap2004550586(plus_self85, plus_sarg62);
     plus_self86 = ret_val161;
     plus_sarg63 = bnd_iter_expr1;
     ret_val162 = STR_ap2004550586(plus_self86, plus_sarg63);
     plus_self87 = ret_val162;
     plus_sarg64 = ((STR) &name16);
     ret_val163 = STR_ap2004550586(plus_self87, plus_sarg64);
     ndefer_s21 = ret_val163;
     CGEN_d695831906(ndefer_self21);
     SATTR(ndefer_self21,routine_code,FSTR_p1752847026(ATTR(ndefer_self21,routine_code), ndefer_s21));
     mang_self18 = self;
     mang_ob18 = ((OB) e3);
     mang_ns8 = ((OB) ATTR(self,current_sig));
     ret_val164 = MANGLE119219986(ATTR(mang_self18,mangler), mang_ob18, mang_ns8);
     name211 = ret_val164;
     plus_self88 = ATTR(self,code_c);
     plus_s12 = ((dSTR) CGEN_eolrSTR(self));
     L269 = (plus_s12==((dSTR) NULL));
     L2701_=!(L269); 
     if (L2701_) {
      L272 = ATTR(plus_self88,ntext);
      L273 = plus_s12;
      SATTR(plus_self88,ntext,FSTR_p1752847026(L272, (*dSTR_strrSTR[TAG(L273)])(L273)));
     }
     ret_val165 = plus_self88;
     plus_self89 = ret_val165;
     L275=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L275==NULL) FATAL("Unable to allocate more memory");
     memset(L275,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L275)->header.tag=CHAR_tag;
     L274 = (dSTR)((CHAR_boxed) L275);
     ((CHAR_boxed) L274)->immutable_part = ' ';
     plus_s13 = L274;
     L276 = (plus_s13==((dSTR) NULL));
     L2771_=!(L276); 
     if (L2771_) {
      L278 = ATTR(plus_self89,ntext);
      L279 = plus_s13;
      SATTR(plus_self89,ntext,FSTR_p1752847026(L278, (*dSTR_strrSTR[TAG(L279)])(L279)));
     }
     ret_val166 = plus_self89;
     plus_self90 = ret_val166;
     mang_self19 = self;
     mang_ob19 = ((OB) ATTR(e3,bi_tp));
     ret_val167 = MANGLE119219986(ATTR(mang_self19,mangler), mang_ob19, ((OB) NULL));
     plus_s14 = ((dSTR) ret_val167);
     L280 = (plus_s14==((dSTR) NULL));
     L2811_=!(L280); 
     if (L2811_) {
      L282 = ATTR(plus_self90,ntext);
      L283 = plus_s14;
      SATTR(plus_self90,ntext,FSTR_p1752847026(L282, (*dSTR_strrSTR[TAG(L283)])(L283)));
     }
     ret_val168 = plus_self90;
     plus_self91 = ret_val168;
     L285=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L285==NULL) FATAL("Unable to allocate more memory");
     memset(L285,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L285)->header.tag=CHAR_tag;
     L284 = (dSTR)((CHAR_boxed) L285);
     ((CHAR_boxed) L284)->immutable_part = ' ';
     plus_s15 = L284;
     L286 = (plus_s15==((dSTR) NULL));
     L2871_=!(L286); 
     if (L2871_) {
      L288 = ATTR(plus_self91,ntext);
      L289 = plus_s15;
      SATTR(plus_self91,ntext,FSTR_p1752847026(L288, (*dSTR_strrSTR[TAG(L289)])(L289)));
     }
     ret_val169 = plus_self91;
     plus_self92 = ret_val169;
     plus_s16 = ((dSTR) name211);
     L290 = (plus_s16==((dSTR) NULL));
     L2911_=!(L290); 
     if (L2911_) {
      L292 = ATTR(plus_self92,ntext);
      L293 = plus_s16;
      SATTR(plus_self92,ntext,FSTR_p1752847026(L292, (*dSTR_strrSTR[TAG(L293)])(L293)));
     }
     ret_val170 = plus_self92;
     plus_self93 = ret_val170;
     plus_s17 = ((dSTR) ((STR) &name16));
     L294 = ATTR(plus_self93,ntext);
     L295 = plus_s17;
     SATTR(plus_self93,ntext,FSTR_p1752847026(L294, (*dSTR_strrSTR[TAG(L295)])(L295)));
     ndefer_self22 = self;
     plus_self94 = name211;
     plus_sarg65 = ((STR) &ZALLOC2);
     ret_val171 = STR_ap2004550586(plus_self94, plus_sarg65);
     plus_self95 = ret_val171;
     plus_sarg66 = l1;
     ret_val172 = STR_ap2004550586(plus_self95, plus_sarg66);
     plus_self96 = ret_val172;
     plus_sarg67 = ((STR) &size4);
     ret_val173 = STR_ap2004550586(plus_self96, plus_sarg67);
     ndefer_s22 = ret_val173;
     CGEN_d695831906(ndefer_self22);
     SATTR(ndefer_self22,routine_code,FSTR_p1752847026(ATTR(ndefer_self22,routine_code), ndefer_s22));
     ndefer_self23 = self;
     plus_self97 = name211;
     plus_sarg68 = ((STR) &name80);
     ret_val174 = STR_ap2004550586(plus_self97, plus_sarg68);
     plus_self98 = ret_val174;
     mang_self20 = self;
     mang_ob20 = ((OB) ATTR(e3,bi_tp));
     ret_val175 = MANGLE119219986(ATTR(mang_self20,mangler), mang_ob20, ((OB) NULL));
     plus_sarg69 = ret_val175;
     ret_val176 = STR_ap2004550586(plus_self98, plus_sarg69);
     plus_self99 = ret_val176;
     plus_sarg70 = ((STR) &memcpy1);
     ret_val177 = STR_ap2004550586(plus_self99, plus_sarg70);
     plus_self100 = ret_val177;
     plus_sarg71 = name211;
     ret_val178 = STR_ap2004550586(plus_self100, plus_sarg71);
     plus_self101 = ret_val178;
     plus_sarg72 = ((STR) &name3);
     ret_val179 = STR_ap2004550586(plus_self101, plus_sarg72);
     plus_self102 = ret_val179;
     plus_sarg73 = l1;
     ret_val180 = STR_ap2004550586(plus_self102, plus_sarg73);
     plus_self103 = ret_val180;
     plus_sarg74 = ((STR) &name3);
     ret_val181 = STR_ap2004550586(plus_self103, plus_sarg74);
     plus_self104 = ret_val181;
     plus_sarg75 = l1;
     ret_val182 = STR_ap2004550586(plus_self104, plus_sarg75);
     plus_self105 = ret_val182;
     plus_sarg76 = ((STR) &size4);
     ret_val183 = STR_ap2004550586(plus_self105, plus_sarg76);
     ndefer_s23 = ret_val183;
     CGEN_d695831906(ndefer_self23);
     SATTR(ndefer_self23,routine_code,FSTR_p1752847026(ATTR(ndefer_self23,routine_code), ndefer_s23));
     CGEN_d695831906(self);
    }
   }
  }
  after_loop5: ;
 }
 SATTR(self,nested_bits,FMAPAM1818297900(ATTR(self,nested_bits), ATTR(self,current_am_rout_), nbi));
 ndefer_self24 = self;
 ndefer_s24 = ((STR) &while11);
 CGEN_d695831906(ndefer_self24);
 SATTR(ndefer_self24,routine_code,FSTR_p1752847026(ATTR(ndefer_self24,routine_code), ndefer_s24));
 in_self3 = self;
 L296 = ATTR(in_self3,indent);
 L2971_=INTPLUS(L296,1); 
 SATTR(in_self3,indent,L2971_);
 if (ATTR(self,polling)) {
  L298 = ATTR(ATTR(self,options1),loop_poll);
 } else {
  L298 = FALSE;
 }
 if (L298) {
  ndefer_self25 = self;
  ndefer_s25 = ((STR) &AM_POLL_NETWORK);
  CGEN_d695831906(ndefer_self25);
  SATTR(ndefer_self25,routine_code,FSTR_p1752847026(ATTR(ndefer_self25,routine_code), ndefer_s25));
 }
 CGEN_e494460490(self, ATTR(arg,body));
 move_out_self1 = self;
 L299 = ATTR(move_out_self1,indent);
 L3001_=INTMINUS(L299,1); 
 SATTR(move_out_self1,indent,L3001_);
 ndefer_self26 = self;
 ndefer_s26 = ((STR) &name4);
 CGEN_d695831906(ndefer_self26);
 SATTR(ndefer_self26,routine_code,FSTR_p1752847026(ATTR(ndefer_self26,routine_code), ndefer_s26));
 if (ATTR(arg,has_init_stmt)) {
  move_out_self2 = self;
  L302 = ATTR(move_out_self2,indent);
  L3031_=INTMINUS(L302,1); 
  SATTR(move_out_self2,indent,L3031_);
  ndefer_self27 = self;
  ndefer_s27 = ((STR) &name4);
  CGEN_d695831906(ndefer_self27);
  SATTR(ndefer_self27,routine_code,FSTR_p1752847026(ATTR(ndefer_self27,routine_code), ndefer_s27));
 }
 ndefer_self28 = self;
 ndefer_s28 = STR_ap2004550586(ATTR(self,current_loop), ((STR) &name136));
 CGEN_d695831906(ndefer_self28);
 SATTR(ndefer_self28,routine_code,FSTR_p1752847026(ATTR(ndefer_self28,routine_code), ndefer_s28));
 if (ATTR(arg,has_yield)) {
  L305 = (ATTR(arg,its1)==((FLISTA1062334999) NULL));
  L3061_=!(L305); 
  L304 = L3061_;
 } else {
  L304 = FALSE;
 }
 if (L304) {
  {
   struct FLISTA20621028_frame_struct other7_0;
FLISTA20621028_frame noname7 = &other7_0;
   L71 = ATTR(arg,its1);
   noname7->self = L71;
   noname7->state = 0;
   while (1) {
    L307 = FLISTA20621028(noname7);
    if (noname7->state == -1) {
     goto after_loop6;
    }
    e4 = L307;
    is_builtin_iter_1 = ATTR(e4,fun);
    if (SIG_is418491101(is_builtin_iter_1)) {
     is_iter_self10 = is_builtin_iter_1;
     is_iter_self11 = ATTR(is_iter_self10,name1);
     L310 = (is_iter_self11.str==((STR) NULL));
     L3111_=!(L310); 
     if (L3111_) {
      L312 = is_iter_self11.str;
      L313 = STR_sizerINT(is_iter_self11.str);
      L3141_=INTMINUS(L313,1); 
      L315 = L3141_;
      L3161_=ARR((STR)L312,L315); 
      L317 = L3161_;
      L3181_=L317=='!'; 
      L309 = L3181_;
     } else {
      L309 = FALSE;
     }
     ret_val186 = L309;
     ret_val185 = ret_val186;
     L308 = ret_val185;
    } else {
     L308 = FALSE;
    }
    ret_val184 = L308;
    L319 = ret_val184;
    L3201_=!(L319); 
    if (L3201_) {
     mang_self21 = self;
     mang_ob21 = ((OB) e4);
     mang_ns9 = ((OB) ATTR(self,current_sig));
     ret_val187 = MANGLE119219986(ATTR(mang_self21,mangler), mang_ob21, mang_ns9);
     ep = ret_val187;
     ndefer_self29 = self;
     ndefer_s29 = STR_ap44172742(((STR) &ZFREE1), ep, ((STR) &name137), ep, ((STR) &NULL4));
     CGEN_d695831906(ndefer_self29);
     SATTR(ndefer_self29,routine_code,FSTR_p1752847026(ATTR(ndefer_self29,routine_code), ndefer_s29));
    }
   }
  }
  after_loop6: ;
 }
 psather_self1 = ATTR(self,prog);
 if (ATTR(psather_self1,threads)) {
  L321 = TRUE;
 } else {
  L321 = ATTR(psather_self1,distributed);
 }
 ret_val188 = L321;
 if (ret_val188) {
  L324 = FLISTA2089597941(ATTR(arg,bits1));
  L325 = FLISTA1962912942(ATTR(arg,its1));
  L3261_=INTPLUS(L324,L325); 
  L327 = L3261_;
  L3281_=(0)<(L327); 
  if (L3281_) {
   L329 = ATTR(arg,no_begin_loop);
   L3301_=!(L329); 
   L323 = L3301_;
  } else {
   L323 = FALSE;
  }
  if (L323) {
   is_iter_self12 = ATTR(self,current_sig);
   is_iter_self13 = ATTR(is_iter_self12,name1);
   L334 = (is_iter_self13.str==((STR) NULL));
   L3351_=!(L334); 
   if (L3351_) {
    L336 = is_iter_self13.str;
    L337 = STR_sizerINT(is_iter_self13.str);
    L3381_=INTMINUS(L337,1); 
    L339 = L3381_;
    L3401_=ARR((STR)L336,L339); 
    L341 = L3401_;
    L3421_=L341=='!'; 
    L333 = L3421_;
   } else {
    L333 = FALSE;
   }
   ret_val190 = L333;
   ret_val189 = ret_val190;
   L343 = ret_val189;
   L3441_=!(L343); 
   if (L3441_) {
    L332 = TRUE;
   } else {
    L347 = ATTR(ATTR(self,prog),yields_in_locks);
    L3481_=!(L347); 
    if (L3481_) {
     L346 = TRUE;
    } else {
     L349 = ATTR(ATTR(self,options1),side_effects);
     L3501_=!(L349); 
     L346 = L3501_;
    }
    if (L346) {
     L345 = TRUE;
    } else {
     L352=SIG_ge642497605(ATTR(self,current_sig), ATTR(self,prog));
     L345 = ATTR(L352,has_yield_in_loc);
    }
    L332 = L345;
   }
   L322 = L332;
  } else {
   L322 = FALSE;
  }
  if (L322) {
   move_out_self3 = self;
   L353 = ATTR(move_out_self3,indent);
   L3541_=INTMINUS(L353,1); 
   SATTR(move_out_self3,indent,L3541_);
   ndefer_self30 = self;
   ndefer_s30 = ((STR) &LOOP_END1);
   CGEN_d695831906(ndefer_self30);
   SATTR(ndefer_self30,routine_code,FSTR_p1752847026(ATTR(ndefer_self30,routine_code), ndefer_s30));
   L355 = ATTR(self,ex_nesting);
   L3561_=INTMINUS(L355,1); 
   SATTR(self,ex_nesting,L3561_);
  }
 }
 SATTR(self,current_loop,outer_loop);
 SATTR(self,current_loop_ex_,outer_loop_ex_ne);
 SATTR(self,manual_loop_unlo,mlunl);
 SATTR(arg,init,arg_init);
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e1389302728(CGEN self, AM_ASSIGN_STMT arg) {
 dAM_EXPR lside;
 AM_SHARED_EXPR global11;
 STR s1 = ((STR) NULL);
 STR sp = ((STR) NULL);
 BOOL is_attr = BOOL_zero;
 BOOL is_immutable = BOOL_zero;
 dTP lside_tp = ((dTP) NULL);
 STR sft;
 BOOL lhs_near = BOOL_zero;
 STR tmp1;
 STR attrname;
 dAM_EXPR lhsob;
 STR l;
 STR rhs;
 STR l1;
 STR pw;
 BOOL is_val_attr_expr = BOOL_zero;
 dAM_EXPR before_dot;
 dAM_EXPR arg_before_dot;
 STR rhs1;
 STR l2;
 STR rhs2;
 STR l3;
 CODE_FILE uses_tp_self;
 dTP uses_tp_t;
 CGEN cast_self;
 dTP cast_dest_tp;
 dTP cast_src_tp;
 STR cast_expr;
 BOOL cast_only_boxing = BOOL_zero;
 STR ret_val;
 CGEN mang_self;
 OB mang_ob;
 OB mang_ns;
 STR ret_val1;
 PROG psather_self;
 BOOL ret_val2 = BOOL_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val4;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val5;
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
 CGEN ndefer_self;
 STR ndefer_s;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val10;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val11;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val12;
 CGEN ndefer_self1;
 STR ndefer_s1;
 PROG psather_self1;
 BOOL ret_val13 = BOOL_zero;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val14;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val15;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val16;
 STR plus_self12;
 CHAR plus_arg = CHAR_zero;
 STR ret_val17;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val18;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val19;
 STR r;
 STR plus_self13;
 STR plus_sarg12;
 STR ret_val20;
 PROG psather_self2;
 BOOL ret_val21 = BOOL_zero;
 STR plus_self14;
 STR plus_sarg13;
 STR ret_val22;
 STR plus_self15;
 STR plus_sarg14;
 STR ret_val23;
 STR plus_self16;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val24;
 STR s2;
 CHAR str_self1 = CHAR_zero;
 STR ret_val25;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val26;
 STR r1;
 STR plus_self17;
 STR plus_sarg15;
 STR ret_val27;
 CGEN cast_self1;
 dTP cast_dest_tp1;
 dTP cast_src_tp1;
 STR cast_expr1;
 BOOL cast_only_boxing1 = BOOL_zero;
 STR ret_val28;
 PROG psather_self3;
 BOOL ret_val29 = BOOL_zero;
 CGEN mang_self2;
 OB mang_ob2;
 STR ret_val30;
 STR plus_self18;
 STR plus_sarg16;
 STR ret_val31;
 CGEN ndefer_self2;
 STR ndefer_s2;
 CGEN ndefer_self3;
 STR ndefer_s3;
 STR plus_self19;
 STR plus_sarg17;
 STR ret_val32;
 STR plus_self20;
 STR plus_sarg18;
 STR ret_val33;
 CGEN mang_self3;
 OB mang_ob3;
 STR ret_val34;
 STR plus_self21;
 STR plus_sarg19;
 STR ret_val35;
 STR plus_self22;
 STR plus_sarg20;
 STR ret_val36;
 STR plus_self23;
 STR plus_sarg21;
 STR ret_val37;
 STR plus_self24;
 STR plus_sarg22;
 STR ret_val38;
 CGEN ndefer_self4;
 STR ndefer_s4;
 CGEN ndefer_self5;
 STR ndefer_s5;
 CGEN mang_self4;
 OB mang_ob4;
 STR ret_val39;
 STR plus_self25;
 STR plus_sarg23;
 STR ret_val40;
 STR plus_self26;
 STR plus_sarg24;
 STR ret_val41;
 STR plus_self27;
 STR plus_sarg25;
 STR ret_val42;
 CGEN ndefer_self6;
 STR ndefer_s6;
 CGEN mang_self5;
 OB mang_ob5;
 STR ret_val43;
 STR plus_self28;
 STR plus_sarg26;
 STR ret_val44;
 CGEN mang_self6;
 OB mang_ob6;
 STR ret_val45;
 STR plus_self29;
 STR plus_sarg27;
 STR ret_val46;
 STR plus_self30;
 STR plus_sarg28;
 STR ret_val47;
 STR plus_self31;
 STR plus_sarg29;
 STR ret_val48;
 STR plus_self32;
 STR plus_sarg30;
 STR ret_val49;
 CGEN ndefer_self7;
 STR ndefer_s7;
 CGEN mang_self7;
 OB mang_ob7;
 STR ret_val50;
 STR plus_self33;
 STR plus_sarg31;
 STR ret_val51;
 STR plus_self34;
 STR plus_sarg32;
 STR ret_val52;
 STR plus_self35;
 STR plus_sarg33;
 STR ret_val53;
 CGEN ndefer_self8;
 STR ndefer_s8;
 CGEN ndefer_self9;
 STR ndefer_s9;
 CGEN cast_self2;
 dTP cast_dest_tp2;
 dTP cast_src_tp2;
 STR cast_expr2;
 BOOL cast_only_boxing2 = BOOL_zero;
 STR ret_val54;
 PROG psather_self4;
 BOOL ret_val55 = BOOL_zero;
 AM_VAT319982528 tp_self;
 dTP ret_val56;
 AM_VAT319982528 tp_self1;
 dTP ret_val57;
 CGEN mang_self8;
 OB mang_ob8;
 STR ret_val58;
 STR plus_self36;
 STR plus_sarg34;
 STR ret_val59;
 CGEN ndefer_self10;
 STR ndefer_s10;
 CGEN ndefer_self11;
 STR ndefer_s11;
 STR plus_self37;
 STR plus_sarg35;
 STR ret_val60;
 STR plus_self38;
 STR plus_sarg36;
 STR ret_val61;
 CGEN ndefer_self12;
 STR ndefer_s12;
 CGEN mang_self9;
 OB mang_ob9;
 OB mang_ns1;
 STR ret_val62;
 CGEN defer_self;
 STR defer_s;
 STR plus_self39;
 STR plus_sarg37;
 STR ret_val63;
 STR plus_self40;
 STR plus_sarg38;
 STR ret_val64;
 CGEN defer_self1;
 STR defer_s1;
 CGEN mang_self10;
 OB mang_ob10;
 OB mang_ns2;
 STR ret_val65;
 CGEN ndefer_self13;
 STR ndefer_s13;
 CGEN cast_self3;
 dTP cast_dest_tp3;
 dTP cast_src_tp3;
 STR cast_expr3;
 BOOL cast_only_boxing3 = BOOL_zero;
 STR ret_val66;
 PROG psather_self5;
 BOOL ret_val67 = BOOL_zero;
 CGEN ndefer_self14;
 STR ndefer_s14;
 STR plus_self41;
 STR plus_sarg39;
 STR ret_val68;
 STR plus_self42;
 STR plus_sarg40;
 STR ret_val69;
 STR plus_self43;
 STR plus_sarg41;
 STR ret_val70;
 CGEN ndefer_self15;
 STR ndefer_s15;
 STR plus_self44;
 STR plus_sarg42;
 STR ret_val71;
 STR plus_self45;
 STR plus_sarg43;
 STR ret_val72;
 CGEN mang_self11;
 OB mang_ob11;
 STR ret_val73;
 STR plus_self46;
 STR plus_sarg44;
 STR ret_val74;
 STR plus_self47;
 STR plus_sarg45;
 STR ret_val75;
 STR plus_self48;
 STR plus_sarg46;
 STR ret_val76;
 STR plus_self49;
 STR plus_sarg47;
 STR ret_val77;
 CGEN ndefer_self16;
 STR ndefer_s16;
 STR plus_self50;
 STR plus_sarg48;
 STR ret_val78;
 STR plus_self51;
 STR plus_sarg49;
 STR ret_val79;
 STR plus_self52;
 STR plus_sarg50;
 STR ret_val80;
 CGEN ndefer_self17;
 STR ndefer_s17;
 STR plus_self53;
 STR plus_sarg51;
 STR ret_val81;
 STR plus_self54;
 STR plus_sarg52;
 STR ret_val82;
 CGEN mang_self12;
 OB mang_ob12;
 STR ret_val83;
 STR plus_self55;
 STR plus_sarg53;
 STR ret_val84;
 STR plus_self56;
 STR plus_sarg54;
 STR ret_val85;
 STR plus_self57;
 STR plus_sarg55;
 STR ret_val86;
 STR plus_self58;
 STR plus_sarg56;
 STR ret_val87;
 CGEN ndefer_self18;
 STR ndefer_s18;
 CGEN ndefer_self19;
 STR ndefer_s19;
 PROG psather_self6;
 BOOL ret_val88 = BOOL_zero;
 dAM_EXPR L1;
 extern STR name1;
 dAM_EXPR L2;
 dAM_EXPR L3;
 OB L4;
 OB L5;
 BOOL L6;
 dAM_EXPR L7;
 BOOL L8;
 dTP L9;
 dTP L10;
 BOOL L11;
 BOOL L121_;
 extern STR VASS_LP1;
 extern STR name3;
 extern STR name3;
 extern STR name76;
 extern STR name33;
 extern STR name16;
 dTP L13;
 BOOL L14;
 BOOL L15;
 BOOL L161_;
 extern STR name18;
 extern STR name92;
 STR L17;
 INT L18;
 OB L19;
 INT L20;
 extern STR name58;
 BOOL L22;
 BOOL L23;
 BOOL L24;
 BOOL L25;
 dAM_EXPR L26;
 dTP L27;
 dAM_EXPR L28;
 dTP L29;
 BOOL L30;
 BOOL L311_;
 extern STR name35;
 STR L32;
 INT L33;
 OB L34;
 INT L35;
 dAM_EXPR L37;
 dAM_EXPR L38;
 BOOL L39;
 dAM_EXPR L40;
 BOOL L41;
 BOOL L42;
 dAM_EXPR L43;
 dTP L44;
 dAM_EXPR L45;
 dTP L46;
 BOOL L47;
 BOOL L481_;
 extern STR VASS_LP1;
 extern STR name3;
 dAM_EXPR L49;
 extern STR name3;
 extern STR name76;
 extern STR name33;
 extern STR name16;
 extern STR F1;
 extern STR POST;
 extern STR Ooflocalaccesses;
 dSTAT L50;
 BOOL L51;
 dAM_EXPR L52;
 dTP L53;
 dAM_EXPR L54;
 dTP L55;
 BOOL L56;
 BOOL L571_;
 extern STR VASS_PL1;
 extern STR name3;
 dAM_EXPR L58;
 extern STR name3;
 extern STR name76;
 extern STR SATTR1;
 extern STR name3;
 extern STR name76;
 dAM_EXPR L59;
 dTP L60;
 dAM_EXPR L61;
 dTP L62;
 extern STR S_VA_WATTR_AA;
 extern STR name3;
 extern STR name3;
 extern STR name76;
 extern STR S_V_WATTR_PP;
 extern STR name3;
 extern STR name3;
 dAM_EXPR L63;
 extern STR name3;
 extern STR name76;
 extern STR S_R_WATTR_AA;
 extern STR name3;
 extern STR name3;
 extern STR name76;
 extern STR SATTR1;
 extern STR name3;
 extern STR name76;
 dAM_EXPR L64;
 dTP L65;
 dAM_EXPR L66;
 BOOL L67;
 BOOL L68;
 BOOL L69;
 dAM_EXPR L70;
 dTP L71;
 dAM_EXPR L72;
 dTP L73;
 BOOL L74;
 BOOL L751_;
 dAM_EXPR L76;
 BOOL L77;
 BOOL L78;
 dAM_EXPR L79;
 dTP L80;
 dAM_EXPR L81;
 dTP L82;
 BOOL L83;
 BOOL L841_;
 extern STR VASS_LP1;
 extern STR name3;
 dAM_EXPR L85;
 extern STR name3;
 extern STR name76;
 extern STR name33;
 extern STR name16;
 extern STR LOCKV1;
 extern STR name76;
 extern STR name18;
 OB L86;
 OB L87;
 dAM_EXPR L88;
 extern STR name36;
 extern STR name16;
 extern STR UNLOCKV1;
 extern STR name76;
 extern STR name18;
 OB L89;
 OB L90;
 dAM_EXPR L91;
 extern STR name36;
 extern STR name16;
 BOOL L921_;
 dAM_EXPR L93;
 dAM_EXPR L94;
 BOOL L95;
 BOOL L96;
 BOOL L97;
 dAM_EXPR L98;
 dTP L99;
 dAM_EXPR L100;
 dTP L101;
 BOOL L102;
 BOOL L1031_;
 BOOL L104;
 BOOL L1051_;
 dAM_EXPR L106;
 extern STR name33;
 extern STR name16;
 dAM_EXPR L107;
 dTP L108;
 extern STR name33;
 extern STR name16;
 extern STR VASS_PL1;
 extern STR name3;
 dAM_EXPR L109;
 extern STR name3;
 extern STR name76;
 dAM_EXPR L110;
 dTP L111;
 extern STR name33;
 extern STR name16;
 extern STR VASS_PP1;
 extern STR name3;
 dAM_EXPR L112;
 extern STR name3;
 extern STR name76;
 extern STR name36;
 extern STR name16;
 BOOL L113;
 BOOL L114;
 BOOL L1151_;
 BOOL L116;
 lside = ATTR(arg,dest);
 global11 = ((AM_SHARED_EXPR) NULL);
 uses_tp_self = ATTR(self,code_c);
 L1 = lside;
 uses_tp_t = (*dAM_EXPR_tprdTP[TAG(L1)])(L1);
 CODE_F525364269(uses_tp_self, LAYOUT79932797(((LAYOUT_TBL) NULL), uses_tp_t));
 is_attr = FALSE;
 is_immutable = FALSE;
 sft = ((STR) &name1);
 L2 = lside;
 lhs_near = (*dAM_EX1117610980[TAG(L2)])(L2);
 if (lside==NULL) {
  goto other377;
 } else
 switch (TAG(lside)) {
  case AM_ATTR_EXPR_tag:
   tmp1 = CGEN_e1639504572(self, ATTR(((AM_ATTR_EXPR) lside),ob));
   cast_self = self;
   cast_dest_tp = ATTR(((AM_ATTR_EXPR) lside),self_tp);
   L3 = ATTR(((AM_ATTR_EXPR) lside),ob);
   cast_src_tp = (*dAM_EXPR_tprdTP[TAG(L3)])(L3);
   cast_expr = tmp1;
   cast_only_boxing = FALSE;
   ret_val = CGEN_d1915593494(cast_self, cast_dest_tp, cast_src_tp, cast_expr, cast_only_boxing, ((dMODE) NULL));
   tmp1 = ret_val;
   mang_self = self;
   L5=ZALLOC(sizeof(struct IDENT_boxed_struct));
   if (L5==NULL) FATAL("Unable to allocate more memory");
   ((OB)L5)->header.tag=IDENT_tag;
   L4 = (OB)((IDENT_boxed) L5);
   ((IDENT_boxed) L4)->immutable_part = ATTR(((AM_ATTR_EXPR) lside),at);
   mang_ob = L4;
   mang_ns = ((OB) ATTR(((AM_ATTR_EXPR) lside),self_tp));
   ret_val1 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, mang_ns);
   attrname = ret_val1;
   lside_tp = ATTR(((AM_ATTR_EXPR) lside),self_tp);
   psather_self = ATTR(self,prog);
   if (ATTR(psather_self,threads)) {
    L6 = TRUE;
   } else {
    L6 = ATTR(psather_self,distributed);
   }
   ret_val2 = L6;
   if (ret_val2) {
    lhsob = ATTR(((AM_ATTR_EXPR) lside),ob);
    L7 = lhsob;
    lhs_near = (*dAM_EX1117610980[TAG(L7)])(L7);
    if (lhsob==NULL) {
     goto other378;
    } else
    switch (TAG(lhsob)) {
     case AM_LOCAL_EXPR_tag: break;
     case AM_SHARED_EXPR_tag: break;
     default: ;
     other378: ;
      l = CGEN_d1425664957(self, ATTR(((AM_ATTR_EXPR) lside),self_tp));
      L9 = ATTR(((AM_ATTR_EXPR) lside),self_tp);
      if ((*dTP_is1334578382[TAG(L9)])(L9)) {
       L10 = ATTR(((AM_ATTR_EXPR) lside),self_tp);
       L11 = (*dTP_is418055720[TAG(L10)])(L10);
       L121_=!(L11); 
       L8 = L121_;
      } else {
       L8 = FALSE;
      }
      if (L8) {
       ndefer_self = self;
       plus_self = ((STR) &VASS_LP1);
       plus_sarg = l;
       ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
       plus_self1 = ret_val3;
       plus_sarg1 = ((STR) &name3);
       ret_val4 = STR_ap2004550586(plus_self1, plus_sarg1);
       plus_self2 = ret_val4;
       mang_self1 = self;
       mang_ob1 = ((OB) ATTR(((AM_ATTR_EXPR) lside),self_tp));
       ret_val5 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
       plus_sarg2 = ret_val5;
       ret_val6 = STR_ap2004550586(plus_self2, plus_sarg2);
       plus_self3 = ret_val6;
       plus_sarg3 = ((STR) &name3);
       ret_val7 = STR_ap2004550586(plus_self3, plus_sarg3);
       plus_self4 = ret_val7;
       plus_sarg4 = tmp1;
       ret_val8 = STR_ap2004550586(plus_self4, plus_sarg4);
       plus_self5 = ret_val8;
       plus_sarg5 = ((STR) &name76);
       ret_val9 = STR_ap2004550586(plus_self5, plus_sarg5);
       ndefer_s = ret_val9;
       CGEN_d695831906(ndefer_self);
       SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
      }
      else {
       ndefer_self1 = self;
       plus_self6 = l;
       plus_sarg6 = ((STR) &name33);
       ret_val10 = STR_ap2004550586(plus_self6, plus_sarg6);
       plus_self7 = ret_val10;
       plus_sarg7 = tmp1;
       ret_val11 = STR_ap2004550586(plus_self7, plus_sarg7);
       plus_self8 = ret_val11;
       plus_sarg8 = ((STR) &name16);
       ret_val12 = STR_ap2004550586(plus_self8, plus_sarg8);
       ndefer_s1 = ret_val12;
       CGEN_d695831906(ndefer_self1);
       SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
      }
      tmp1 = l;
    }
   }
   L13 = ATTR(((AM_ATTR_EXPR) lside),self_tp);
   if ((*dTP_is1334578382[TAG(L13)])(L13)) {
    psather_self1 = ATTR(self,prog);
    if (ATTR(psather_self1,threads)) {
     L14 = TRUE;
    } else {
     L14 = ATTR(psather_self1,distributed);
    }
    ret_val13 = L14;
    L15 = ret_val13;
    L161_=!(L15); 
    if (L161_) {
     plus_self9 = tmp1;
     plus_sarg9 = ((STR) &name18);
     ret_val14 = STR_ap2004550586(plus_self9, plus_sarg9);
     plus_self10 = ret_val14;
     plus_sarg10 = attrname;
     ret_val15 = STR_ap2004550586(plus_self10, plus_sarg10);
     s1 = ret_val15;
    }
    else {
     plus_self11 = ((STR) &name92);
     plus_sarg11 = tmp1;
     ret_val16 = STR_ap2004550586(plus_self11, plus_sarg11);
     plus_self12 = ret_val16;
     plus_arg = ',';
     str_self = plus_arg;
     create_self = ((STR) NULL);
     create_c = str_self;
     L18 = 1;
     L20=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L18)*sizeof(CHAR);
     L19=ZALLOC_LEAF_BIG(L20);
     if (L19==NULL) FATAL("Unable to allocate more memory");
     memset(L19,0,L20);
     ((OB)L19)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L19)->header.destroyed=0;
#endif

     L17 = ((STR) L19);
     L17->asize = L18;
     r = L17;
     SARR((STR)r,0,create_c); 
     ;
     ret_val19 = r;
     ret_val18 = ret_val19;
     s = ret_val18;
     ret_val17 = STR_ap1077157958(plus_self12, s, TRUE);
     plus_self13 = ret_val17;
     plus_sarg12 = attrname;
     ret_val20 = STR_ap2004550586(plus_self13, plus_sarg12);
     s1 = ret_val20;
     sft = ((STR) &name58);
    }
   }
   else {
    psather_self2 = ATTR(self,prog);
    if (ATTR(psather_self2,threads)) {
     L25 = TRUE;
    } else {
     L25 = ATTR(psather_self2,distributed);
    }
    ret_val21 = L25;
    if (ret_val21) {
     L24 = lhs_near;
    } else {
     L24 = FALSE;
    }
    if (L24) {
     L26 = ATTR(arg,dest);
     L27 = (*dAM_EXPR_tprdTP[TAG(L26)])(L26);
     L23 = (*dTP_is1334578382[TAG(L27)])(L27);
    } else {
     L23 = FALSE;
    }
    if (L23) {
     L28 = ATTR(arg,dest);
     L29 = (*dAM_EXPR_tprdTP[TAG(L28)])(L28);
     L30 = (*dTP_is418055720[TAG(L29)])(L29);
     L311_=!(L30); 
     L22 = L311_;
    } else {
     L22 = FALSE;
    }
    if (L22) {
     plus_self14 = tmp1;
     plus_sarg13 = ((STR) &name35);
     ret_val22 = STR_ap2004550586(plus_self14, plus_sarg13);
     plus_self15 = ret_val22;
     plus_sarg14 = attrname;
     ret_val23 = STR_ap2004550586(plus_self15, plus_sarg14);
     s1 = ret_val23;
    }
    else {
     plus_self16 = tmp1;
     plus_arg1 = ',';
     str_self1 = plus_arg1;
     create_self1 = ((STR) NULL);
     create_c1 = str_self1;
     L33 = 1;
     L35=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L33)*sizeof(CHAR);
     L34=ZALLOC_LEAF_BIG(L35);
     if (L34==NULL) FATAL("Unable to allocate more memory");
     memset(L34,0,L35);
     ((OB)L34)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L34)->header.destroyed=0;
#endif

     L32 = ((STR) L34);
     L32->asize = L33;
     r1 = L32;
     SARR((STR)r1,0,create_c1); 
     ;
     ret_val26 = r1;
     ret_val25 = ret_val26;
     s2 = ret_val25;
     ret_val24 = STR_ap1077157958(plus_self16, s2, TRUE);
     plus_self17 = ret_val24;
     plus_sarg15 = attrname;
     ret_val27 = STR_ap2004550586(plus_self17, plus_sarg15);
     s1 = ret_val27;
    }
   }
   is_attr = TRUE;
   sp = tmp1; break;
  case AM_SHARED_EXPR_tag:
   global11 = ((AM_SHARED_EXPR) lside);
   s1 = CGEN_e1639504572(self, lside); break;
  default: ;
  other377: ;
   s1 = CGEN_e1639504572(self, lside);
 }
 if (is_attr) {
  cast_self1 = self;
  L37 = ATTR(arg,dest);
  cast_dest_tp1 = (*dAM_EXPR_tprdTP[TAG(L37)])(L37);
  L38 = ATTR(arg,src1);
  cast_src_tp1 = (*dAM_EXPR_tprdTP[TAG(L38)])(L38);
  cast_expr1 = CGEN_e1639504572(self, ATTR(arg,src1));
  cast_only_boxing1 = FALSE;
  ret_val28 = CGEN_d1915593494(cast_self1, cast_dest_tp1, cast_src_tp1, cast_expr1, cast_only_boxing1, ((dMODE) NULL));
  rhs = ret_val28;
  psather_self3 = ATTR(self,prog);
  if (ATTR(psather_self3,threads)) {
   L39 = TRUE;
  } else {
   L39 = ATTR(psather_self3,distributed);
  }
  ret_val29 = L39;
  if (ret_val29) {
   L40 = ATTR(arg,dest);
   l1 = CGEN_d1425664957(self, (*dAM_EXPR_tprdTP[TAG(L40)])(L40));
   L43 = ATTR(arg,src1);
   L44 = (*dAM_EXPR_tprdTP[TAG(L43)])(L43);
   if ((*dTP_is1334578382[TAG(L44)])(L44)) {
    L45 = ATTR(arg,src1);
    L46 = (*dAM_EXPR_tprdTP[TAG(L45)])(L45);
    L47 = (*dTP_is418055720[TAG(L46)])(L46);
    L481_=!(L47); 
    L42 = L481_;
   } else {
    L42 = FALSE;
   }
   if (L42) {
    L41 = CGEN_i1595194353(self, ATTR(arg,src1));
   } else {
    L41 = FALSE;
   }
   if (L41) {
    ndefer_self2 = self;
    plus_self18 = ((STR) &VASS_LP1);
    mang_self2 = self;
    L49 = ATTR(arg,dest);
    mang_ob2 = ((OB) (*dAM_EXPR_tprdTP[TAG(L49)])(L49));
    ret_val30 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, ((OB) NULL));
    plus_sarg16 = STR_ap1925291910(l1, ((STR) &name3), ret_val30, ((STR) &name3), rhs, ((STR) &name76));
    ret_val31 = STR_ap2004550586(plus_self18, plus_sarg16);
    ndefer_s2 = ret_val31;
    CGEN_d695831906(ndefer_self2);
    SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
   }
   else {
    ndefer_self3 = self;
    ndefer_s3 = STR_ap1417971546(l1, ((STR) &name33), rhs, ((STR) &name16));
    CGEN_d695831906(ndefer_self3);
    SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
   }
   pw = ((STR) &F1);
   if (ATTR(ATTR(self,options1),post_write)) {
    pw = ((STR) &POST);
   }
   if (lhs_near) {
    L50 = ATTR(ATTR(self,prog),stat1);
    (*dSTAT_incr_STR[TAG(L50)])(L50, ((STR) &Ooflocalaccesses));
    L52 = ATTR(arg,dest);
    L53 = (*dAM_EXPR_tprdTP[TAG(L52)])(L52);
    if ((*dTP_is1334578382[TAG(L53)])(L53)) {
     L54 = ATTR(arg,dest);
     L55 = (*dAM_EXPR_tprdTP[TAG(L54)])(L54);
     L56 = (*dTP_is418055720[TAG(L55)])(L55);
     L571_=!(L56); 
     L51 = L571_;
    } else {
     L51 = FALSE;
    }
    if (L51) {
     ndefer_self4 = self;
     plus_self19 = ((STR) &VASS_PL1);
     plus_sarg17 = s1;
     ret_val32 = STR_ap2004550586(plus_self19, plus_sarg17);
     plus_self20 = ret_val32;
     plus_sarg18 = ((STR) &name3);
     ret_val33 = STR_ap2004550586(plus_self20, plus_sarg18);
     plus_self21 = ret_val33;
     mang_self3 = self;
     L58 = ATTR(arg,dest);
     mang_ob3 = ((OB) (*dAM_EXPR_tprdTP[TAG(L58)])(L58));
     ret_val34 = MANGLE119219986(ATTR(mang_self3,mangler), mang_ob3, ((OB) NULL));
     plus_sarg19 = ret_val34;
     ret_val35 = STR_ap2004550586(plus_self21, plus_sarg19);
     plus_self22 = ret_val35;
     plus_sarg20 = ((STR) &name3);
     ret_val36 = STR_ap2004550586(plus_self22, plus_sarg20);
     plus_self23 = ret_val36;
     plus_sarg21 = l1;
     ret_val37 = STR_ap2004550586(plus_self23, plus_sarg21);
     plus_self24 = ret_val37;
     plus_sarg22 = ((STR) &name76);
     ret_val38 = STR_ap2004550586(plus_self24, plus_sarg22);
     ndefer_s4 = ret_val38;
     CGEN_d695831906(ndefer_self4);
     SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
    }
    else {
     ndefer_self5 = self;
     ndefer_s5 = STR_ap44172742(((STR) &SATTR1), s1, ((STR) &name3), l1, ((STR) &name76));
     CGEN_d695831906(ndefer_self5);
     SATTR(ndefer_self5,routine_code,FSTR_p1752847026(ATTR(ndefer_self5,routine_code), ndefer_s5));
    }
   }
   else {
    L59 = ATTR(arg,dest);
    L60 = (*dAM_EXPR_tprdTP[TAG(L59)])(L59);
    if ((*dTP_is1334578382[TAG(L60)])(L60)) {
     L61 = ATTR(arg,dest);
     L62 = (*dAM_EXPR_tprdTP[TAG(L61)])(L61);
     if ((*dTP_is418055720[TAG(L62)])(L62)) {
      ndefer_self6 = self;
      plus_self27 = pw;
      mang_self4 = self;
      mang_ob4 = ((OB) lside_tp);
      ret_val39 = MANGLE119219986(ATTR(mang_self4,mangler), mang_ob4, ((OB) NULL));
      plus_self25 = ret_val39;
      plus_sarg23 = sft;
      ret_val40 = STR_ap2004550586(plus_self25, plus_sarg23);
      plus_self26 = l1;
      plus_sarg24 = ((STR) &name76);
      ret_val41 = STR_ap2004550586(plus_self26, plus_sarg24);
      plus_sarg25 = STR_ap1925291910(((STR) &S_VA_WATTR_AA), ret_val40, ((STR) &name3), s1, ((STR) &name3), ret_val41);
      ret_val42 = STR_ap2004550586(plus_self27, plus_sarg25);
      ndefer_s6 = ret_val42;
      CGEN_d695831906(ndefer_self6);
      SATTR(ndefer_self6,routine_code,FSTR_p1752847026(ATTR(ndefer_self6,routine_code), ndefer_s6));
     }
     else {
      ndefer_self7 = self;
      plus_self32 = pw;
      mang_self5 = self;
      mang_ob5 = ((OB) lside_tp);
      ret_val43 = MANGLE119219986(ATTR(mang_self5,mangler), mang_ob5, ((OB) NULL));
      plus_self28 = ret_val43;
      plus_sarg26 = sft;
      ret_val44 = STR_ap2004550586(plus_self28, plus_sarg26);
      mang_self6 = self;
      L63 = ATTR(arg,dest);
      mang_ob6 = ((OB) (*dAM_EXPR_tprdTP[TAG(L63)])(L63));
      ret_val45 = MANGLE119219986(ATTR(mang_self6,mangler), mang_ob6, ((OB) NULL));
      plus_self29 = ret_val45;
      plus_sarg27 = ((STR) &name3);
      ret_val46 = STR_ap2004550586(plus_self29, plus_sarg27);
      plus_self30 = ret_val46;
      plus_sarg28 = l1;
      ret_val47 = STR_ap2004550586(plus_self30, plus_sarg28);
      plus_self31 = ret_val47;
      plus_sarg29 = ((STR) &name76);
      ret_val48 = STR_ap2004550586(plus_self31, plus_sarg29);
      plus_sarg30 = STR_ap1925291910(((STR) &S_V_WATTR_PP), ret_val44, ((STR) &name3), s1, ((STR) &name3), ret_val48);
      ret_val49 = STR_ap2004550586(plus_self32, plus_sarg30);
      ndefer_s7 = ret_val49;
      CGEN_d695831906(ndefer_self7);
      SATTR(ndefer_self7,routine_code,FSTR_p1752847026(ATTR(ndefer_self7,routine_code), ndefer_s7));
     }
    }
    else {
     ndefer_self8 = self;
     plus_self35 = pw;
     mang_self7 = self;
     mang_ob7 = ((OB) lside_tp);
     ret_val50 = MANGLE119219986(ATTR(mang_self7,mangler), mang_ob7, ((OB) NULL));
     plus_self33 = ret_val50;
     plus_sarg31 = sft;
     ret_val51 = STR_ap2004550586(plus_self33, plus_sarg31);
     plus_self34 = l1;
     plus_sarg32 = ((STR) &name76);
     ret_val52 = STR_ap2004550586(plus_self34, plus_sarg32);
     plus_sarg33 = STR_ap1925291910(((STR) &S_R_WATTR_AA), ret_val51, ((STR) &name3), s1, ((STR) &name3), ret_val52);
     ret_val53 = STR_ap2004550586(plus_self35, plus_sarg33);
     ndefer_s8 = ret_val53;
     CGEN_d695831906(ndefer_self8);
     SATTR(ndefer_self8,routine_code,FSTR_p1752847026(ATTR(ndefer_self8,routine_code), ndefer_s8));
    }
   }
  }
  else {
   ndefer_self9 = self;
   ndefer_s9 = STR_ap44172742(((STR) &SATTR1), s1, ((STR) &name3), rhs, ((STR) &name76));
   CGEN_d695831906(ndefer_self9);
   SATTR(ndefer_self9,routine_code,FSTR_p1752847026(ATTR(ndefer_self9,routine_code), ndefer_s9));
  }
 }
 else {
  is_val_attr_expr = FALSE;
  L64 = lside;
  L65 = (*dAM_EXPR_tprdTP[TAG(L64)])(L64);
  if ((*dTP_is1334578382[TAG(L65)])(L65)) {
   before_dot = ATTR(arg,src1);
   if (before_dot==NULL) {
    goto other379;
   } else
   switch (TAG(before_dot)) {
    case AM_VAT319982528_tag:
     arg_before_dot = ATTR(((AM_VAT319982528) before_dot),ob);
     if (arg_before_dot==NULL) {
      goto other380;
     } else
     switch (TAG(arg_before_dot)) {
      case AM_LOCAL_EXPR_tag:
       if (SYSOBEQ(((OB) arg_before_dot),((OB) lside))) {
        is_val_attr_expr = TRUE;
        cast_self2 = self;
        cast_dest_tp2 = ATTR(((AM_VAT319982528) before_dot),real_tp);
        L66 = ATTR(((AM_VAT319982528) before_dot),val1);
        cast_src_tp2 = (*dAM_EXPR_tprdTP[TAG(L66)])(L66);
        cast_expr2 = CGEN_e1639504572(self, ATTR(((AM_VAT319982528) before_dot),val1));
        cast_only_boxing2 = FALSE;
        ret_val54 = CGEN_d1915593494(cast_self2, cast_dest_tp2, cast_src_tp2, cast_expr2, cast_only_boxing2, ((dMODE) NULL));
        rhs1 = ret_val54;
        psather_self4 = ATTR(self,prog);
        if (ATTR(psather_self4,threads)) {
         L69 = TRUE;
        } else {
         L69 = ATTR(psather_self4,distributed);
        }
        ret_val55 = L69;
        if (ret_val55) {
         L70 = lside;
         L71 = (*dAM_EXPR_tprdTP[TAG(L70)])(L70);
         L68 = (*dTP_is1334578382[TAG(L71)])(L71);
        } else {
         L68 = FALSE;
        }
        if (L68) {
         L72 = lside;
         L73 = (*dAM_EXPR_tprdTP[TAG(L72)])(L72);
         L74 = (*dTP_is418055720[TAG(L73)])(L73);
         L751_=!(L74); 
         L67 = L751_;
        } else {
         L67 = FALSE;
        }
        if (L67) {
         L76 = ATTR(((AM_VAT319982528) before_dot),val1);
         l2 = CGEN_d1425664957(self, (*dAM_EXPR_tprdTP[TAG(L76)])(L76));
         tp_self = ((AM_VAT319982528) before_dot);
         L79 = ATTR(tp_self,ob);
         ret_val56 = (*dAM_EXPR_tprdTP[TAG(L79)])(L79);
         L80 = ret_val56;
         if ((*dTP_is1334578382[TAG(L80)])(L80)) {
          tp_self1 = ((AM_VAT319982528) before_dot);
          L81 = ATTR(tp_self1,ob);
          ret_val57 = (*dAM_EXPR_tprdTP[TAG(L81)])(L81);
          L82 = ret_val57;
          L83 = (*dTP_is418055720[TAG(L82)])(L82);
          L841_=!(L83); 
          L78 = L841_;
         } else {
          L78 = FALSE;
         }
         if (L78) {
          L77 = CGEN_i1595194353(self, ATTR(((AM_VAT319982528) before_dot),val1));
         } else {
          L77 = FALSE;
         }
         if (L77) {
          ndefer_self10 = self;
          plus_self36 = ((STR) &VASS_LP1);
          mang_self8 = self;
          L85 = ATTR(((AM_VAT319982528) before_dot),val1);
          mang_ob8 = ((OB) (*dAM_EXPR_tprdTP[TAG(L85)])(L85));
          ret_val58 = MANGLE119219986(ATTR(mang_self8,mangler), mang_ob8, ((OB) NULL));
          plus_sarg34 = STR_ap1925291910(l2, ((STR) &name3), ret_val58, ((STR) &name3), rhs1, ((STR) &name76));
          ret_val59 = STR_ap2004550586(plus_self36, plus_sarg34);
          ndefer_s10 = ret_val59;
          CGEN_d695831906(ndefer_self10);
          SATTR(ndefer_self10,routine_code,FSTR_p1752847026(ATTR(ndefer_self10,routine_code), ndefer_s10));
         }
         else {
          ndefer_self11 = self;
          ndefer_s11 = STR_ap1417971546(l2, ((STR) &name33), rhs1, ((STR) &name16));
          CGEN_d695831906(ndefer_self11);
          SATTR(ndefer_self11,routine_code,FSTR_p1752847026(ATTR(ndefer_self11,routine_code), ndefer_s11));
         }
         ndefer_self12 = self;
         plus_self37 = ((STR) &LOCKV1);
         plus_sarg35 = s1;
         ret_val60 = STR_ap2004550586(plus_self37, plus_sarg35);
         plus_self38 = ret_val60;
         plus_sarg36 = ((STR) &name76);
         ret_val61 = STR_ap2004550586(plus_self38, plus_sarg36);
         ndefer_s12 = ret_val61;
         CGEN_d695831906(ndefer_self12);
         SATTR(ndefer_self12,routine_code,FSTR_p1752847026(ATTR(ndefer_self12,routine_code), ndefer_s12));
         defer_self = self;
         mang_self9 = self;
         L87=ZALLOC(sizeof(struct IDENT_boxed_struct));
         if (L87==NULL) FATAL("Unable to allocate more memory");
         ((OB)L87)->header.tag=IDENT_tag;
         L86 = (OB)((IDENT_boxed) L87);
         ((IDENT_boxed) L86)->immutable_part = ATTR(((AM_VAT319982528) before_dot),at);
         mang_ob9 = L86;
         L88 = ATTR(((AM_VAT319982528) before_dot),ob);
         mang_ns1 = ((OB) (*dAM_EXPR_tprdTP[TAG(L88)])(L88));
         ret_val62 = MANGLE119219986(ATTR(mang_self9,mangler), mang_ob9, mang_ns1);
         defer_s = STR_ap1925291910(s1, ((STR) &name18), ret_val62, ((STR) &name36), l2, ((STR) &name16));
         SATTR(defer_self,routine_code,FSTR_p1752847026(ATTR(defer_self,routine_code), defer_s));
         defer_self1 = self;
         plus_self39 = ((STR) &UNLOCKV1);
         plus_sarg37 = s1;
         ret_val63 = STR_ap2004550586(plus_self39, plus_sarg37);
         plus_self40 = ret_val63;
         plus_sarg38 = ((STR) &name76);
         ret_val64 = STR_ap2004550586(plus_self40, plus_sarg38);
         defer_s1 = ret_val64;
         SATTR(defer_self1,routine_code,FSTR_p1752847026(ATTR(defer_self1,routine_code), defer_s1));
        }
        else {
         ndefer_self13 = self;
         mang_self10 = self;
         L90=ZALLOC(sizeof(struct IDENT_boxed_struct));
         if (L90==NULL) FATAL("Unable to allocate more memory");
         ((OB)L90)->header.tag=IDENT_tag;
         L89 = (OB)((IDENT_boxed) L90);
         ((IDENT_boxed) L89)->immutable_part = ATTR(((AM_VAT319982528) before_dot),at);
         mang_ob10 = L89;
         L91 = ATTR(((AM_VAT319982528) before_dot),ob);
         mang_ns2 = ((OB) (*dAM_EXPR_tprdTP[TAG(L91)])(L91));
         ret_val65 = MANGLE119219986(ATTR(mang_self10,mangler), mang_ob10, mang_ns2);
         ndefer_s13 = STR_ap1925291910(s1, ((STR) &name18), ret_val65, ((STR) &name36), rhs1, ((STR) &name16));
         CGEN_d695831906(ndefer_self13);
         SATTR(ndefer_self13,routine_code,FSTR_p1752847026(ATTR(ndefer_self13,routine_code), ndefer_s13));
        }
       } break;
      default: ;
      other380: ;
     } break;
    default: ;
    other379: ;
   }
  }
  L921_=!(is_val_attr_expr); 
  if (L921_) {
   cast_self3 = self;
   L93 = ATTR(arg,dest);
   cast_dest_tp3 = (*dAM_EXPR_tprdTP[TAG(L93)])(L93);
   L94 = ATTR(arg,src1);
   cast_src_tp3 = (*dAM_EXPR_tprdTP[TAG(L94)])(L94);
   cast_expr3 = CGEN_e1639504572(self, ATTR(arg,src1));
   cast_only_boxing3 = FALSE;
   ret_val66 = CGEN_d1915593494(cast_self3, cast_dest_tp3, cast_src_tp3, cast_expr3, cast_only_boxing3, ((dMODE) NULL));
   rhs2 = ret_val66;
   psather_self5 = ATTR(self,prog);
   if (ATTR(psather_self5,threads)) {
    L97 = TRUE;
   } else {
    L97 = ATTR(psather_self5,distributed);
   }
   ret_val67 = L97;
   if (ret_val67) {
    L98 = lside;
    L99 = (*dAM_EXPR_tprdTP[TAG(L98)])(L98);
    L96 = (*dTP_is1334578382[TAG(L99)])(L99);
   } else {
    L96 = FALSE;
   }
   if (L96) {
    L100 = lside;
    L101 = (*dAM_EXPR_tprdTP[TAG(L100)])(L100);
    L102 = (*dTP_is418055720[TAG(L101)])(L101);
    L1031_=!(L102); 
    L95 = L1031_;
   } else {
    L95 = FALSE;
   }
   if (L95) {
    L104 = CGEN_i1595194353(self, ATTR(arg,src1));
    L1051_=!(L104); 
    if (L1051_) {
     L106 = ATTR(arg,dest);
     l3 = CGEN_d1425664957(self, (*dAM_EXPR_tprdTP[TAG(L106)])(L106));
     ndefer_self14 = self;
     ndefer_s14 = STR_ap1417971546(l3, ((STR) &name33), rhs2, ((STR) &name16));
     CGEN_d695831906(ndefer_self14);
     SATTR(ndefer_self14,routine_code,FSTR_p1752847026(ATTR(ndefer_self14,routine_code), ndefer_s14));
     L107 = lside;
     L108 = (*dAM_EXPR_tprdTP[TAG(L107)])(L107);
     if ((*dTP_is418055720[TAG(L108)])(L108)) {
      ndefer_self15 = self;
      plus_self41 = s1;
      plus_sarg39 = ((STR) &name33);
      ret_val68 = STR_ap2004550586(plus_self41, plus_sarg39);
      plus_self42 = ret_val68;
      plus_sarg40 = l3;
      ret_val69 = STR_ap2004550586(plus_self42, plus_sarg40);
      plus_self43 = ret_val69;
      plus_sarg41 = ((STR) &name16);
      ret_val70 = STR_ap2004550586(plus_self43, plus_sarg41);
      ndefer_s15 = ret_val70;
      CGEN_d695831906(ndefer_self15);
      SATTR(ndefer_self15,routine_code,FSTR_p1752847026(ATTR(ndefer_self15,routine_code), ndefer_s15));
     }
     else {
      ndefer_self16 = self;
      plus_self44 = ((STR) &VASS_PL1);
      plus_sarg42 = s1;
      ret_val71 = STR_ap2004550586(plus_self44, plus_sarg42);
      plus_self45 = ret_val71;
      plus_sarg43 = ((STR) &name3);
      ret_val72 = STR_ap2004550586(plus_self45, plus_sarg43);
      plus_self46 = ret_val72;
      mang_self11 = self;
      L109 = lside;
      mang_ob11 = ((OB) (*dAM_EXPR_tprdTP[TAG(L109)])(L109));
      ret_val73 = MANGLE119219986(ATTR(mang_self11,mangler), mang_ob11, ((OB) NULL));
      plus_sarg44 = ret_val73;
      ret_val74 = STR_ap2004550586(plus_self46, plus_sarg44);
      plus_self47 = ret_val74;
      plus_sarg45 = ((STR) &name3);
      ret_val75 = STR_ap2004550586(plus_self47, plus_sarg45);
      plus_self48 = ret_val75;
      plus_sarg46 = l3;
      ret_val76 = STR_ap2004550586(plus_self48, plus_sarg46);
      plus_self49 = ret_val76;
      plus_sarg47 = ((STR) &name76);
      ret_val77 = STR_ap2004550586(plus_self49, plus_sarg47);
      ndefer_s16 = ret_val77;
      CGEN_d695831906(ndefer_self16);
      SATTR(ndefer_self16,routine_code,FSTR_p1752847026(ATTR(ndefer_self16,routine_code), ndefer_s16));
     }
    }
    else {
     L110 = lside;
     L111 = (*dAM_EXPR_tprdTP[TAG(L110)])(L110);
     if ((*dTP_is418055720[TAG(L111)])(L111)) {
      ndefer_self17 = self;
      plus_self50 = s1;
      plus_sarg48 = ((STR) &name33);
      ret_val78 = STR_ap2004550586(plus_self50, plus_sarg48);
      plus_self51 = ret_val78;
      plus_sarg49 = rhs2;
      ret_val79 = STR_ap2004550586(plus_self51, plus_sarg49);
      plus_self52 = ret_val79;
      plus_sarg50 = ((STR) &name16);
      ret_val80 = STR_ap2004550586(plus_self52, plus_sarg50);
      ndefer_s17 = ret_val80;
      CGEN_d695831906(ndefer_self17);
      SATTR(ndefer_self17,routine_code,FSTR_p1752847026(ATTR(ndefer_self17,routine_code), ndefer_s17));
     }
     else {
      ndefer_self18 = self;
      plus_self53 = ((STR) &VASS_PP1);
      plus_sarg51 = s1;
      ret_val81 = STR_ap2004550586(plus_self53, plus_sarg51);
      plus_self54 = ret_val81;
      plus_sarg52 = ((STR) &name3);
      ret_val82 = STR_ap2004550586(plus_self54, plus_sarg52);
      plus_self55 = ret_val82;
      mang_self12 = self;
      L112 = lside;
      mang_ob12 = ((OB) (*dAM_EXPR_tprdTP[TAG(L112)])(L112));
      ret_val83 = MANGLE119219986(ATTR(mang_self12,mangler), mang_ob12, ((OB) NULL));
      plus_sarg53 = ret_val83;
      ret_val84 = STR_ap2004550586(plus_self55, plus_sarg53);
      plus_self56 = ret_val84;
      plus_sarg54 = ((STR) &name3);
      ret_val85 = STR_ap2004550586(plus_self56, plus_sarg54);
      plus_self57 = ret_val85;
      plus_sarg55 = rhs2;
      ret_val86 = STR_ap2004550586(plus_self57, plus_sarg55);
      plus_self58 = ret_val86;
      plus_sarg56 = ((STR) &name76);
      ret_val87 = STR_ap2004550586(plus_self58, plus_sarg56);
      ndefer_s18 = ret_val87;
      CGEN_d695831906(ndefer_self18);
      SATTR(ndefer_self18,routine_code,FSTR_p1752847026(ATTR(ndefer_self18,routine_code), ndefer_s18));
     }
    }
   }
   else {
    ndefer_self19 = self;
    ndefer_s19 = STR_ap1417971546(s1, ((STR) &name36), rhs2, ((STR) &name16));
    CGEN_d695831906(ndefer_self19);
    SATTR(ndefer_self19,routine_code,FSTR_p1752847026(ATTR(ndefer_self19,routine_code), ndefer_s19));
   }
  }
 }
 L114 = (global11==((AM_SHARED_EXPR) NULL));
 L1151_=!(L114); 
 if (L1151_) {
  psather_self6 = ATTR(self,prog);
  if (ATTR(psather_self6,threads)) {
   L116 = TRUE;
  } else {
   L116 = ATTR(psather_self6,distributed);
  }
  ret_val88 = L116;
  L113 = ret_val88;
 } else {
  L113 = FALSE;
 }
 if (L113) {
  CGEN_b2123071570(self, global11);
 }
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e638028220(CGEN self, AM_IF_STMT arg) {
 STR s1;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN in_self;
 CGEN move_out_self;
 CGEN ndefer_self1;
 STR ndefer_s1;
 CGEN ndefer_self2;
 STR ndefer_s2;
 CGEN in_self1;
 CGEN move_out_self1;
 CGEN ndefer_self3;
 STR ndefer_s3;
 extern STR if3;
 extern STR name84;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 extern STR name4;
 BOOL L5;
 BOOL L61_;
 extern STR else6;
 INT L7;
 INT L81_;
 INT L9;
 INT L101_;
 extern STR name4;
 s1 = CGEN_e1639504572(self, ATTR(arg,test1));
 ndefer_self = self;
 ndefer_s = STR_ap1693864410(((STR) &if3), s1, ((STR) &name84));
 CGEN_d695831906(ndefer_self);
 SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
 in_self = self;
 L1 = ATTR(in_self,indent);
 L21_=INTPLUS(L1,1); 
 SATTR(in_self,indent,L21_);
 CGEN_e494460490(self, ATTR(arg,if_true));
 move_out_self = self;
 L3 = ATTR(move_out_self,indent);
 L41_=INTMINUS(L3,1); 
 SATTR(move_out_self,indent,L41_);
 ndefer_self1 = self;
 ndefer_s1 = ((STR) &name4);
 CGEN_d695831906(ndefer_self1);
 SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
 L5 = (ATTR(arg,if_false)==((dAM_STMT) NULL));
 L61_=!(L5); 
 if (L61_) {
  ndefer_self2 = self;
  ndefer_s2 = ((STR) &else6);
  CGEN_d695831906(ndefer_self2);
  SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
  in_self1 = self;
  L7 = ATTR(in_self1,indent);
  L81_=INTPLUS(L7,1); 
  SATTR(in_self1,indent,L81_);
  CGEN_e494460490(self, ATTR(arg,if_false));
  move_out_self1 = self;
  L9 = ATTR(move_out_self1,indent);
  L101_=INTMINUS(L9,1); 
  SATTR(move_out_self1,indent,L101_);
  ndefer_self3 = self;
  ndefer_s3 = ((STR) &name4);
  CGEN_d695831906(ndefer_self3);
  SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
 }
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e689545343(CGEN self, AM_RETURN_STMT arg) {
 STR s1;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN pop_exceptions_s;
 INT pop_exceptions_n = INT_zero;
 INT L11 = INT_zero;
 CGEN ndefer_self1;
 STR ndefer_s1;
 INT str_self = INT_zero;
 STR ret_val;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val1;
 FSTR str_self1;
 STR ret_val2;
 CGEN ndefer_self2;
 STR ndefer_s2;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val4;
 CGEN ndefer_self3;
 STR ndefer_s3;
 CGEN cast_self;
 dTP cast_dest_tp;
 dTP cast_src_tp;
 STR cast_expr;
 BOOL cast_only_boxing = BOOL_zero;
 STR ret_val5;
 CGEN ndefer_self4;
 STR ndefer_s4;
 CGEN ndefer_self5;
 STR ndefer_s5;
 extern STR LOCK_UNLOCK_NOW;
 extern STR POP_EXCEPTION12;
 extern STR POP_EXCEPTION11;
 BOOL L2;
 BOOL L31_;
 extern STR name76;
 BOOL L4;
 BOOL L51_;
 extern STR PREFET1824934344;
 extern STR name76;
 BOOL L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 extern STR return4;
 dAM_EXPR L10;
 extern STR name16;
 extern STR return5;
 INT L12;
 BOOL L131_;
 if (ATTR(self,manual_unlock)) {
  ndefer_self = self;
  ndefer_s = ((STR) &LOCK_UNLOCK_NOW);
  CGEN_d695831906(ndefer_self);
  SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
 }
 pop_exceptions_s = self;
 pop_exceptions_n = ATTR(self,ex_nesting);
 L11 = pop_exceptions_n;
 switch (L11) {
  case 0: 
   break;
  case 1: 
   ndefer_self1 = pop_exceptions_s;
   ndefer_s1 = ((STR) &POP_EXCEPTION12);
   CGEN_d695831906(ndefer_self1);
   SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
   break;
  default: ;
   ndefer_self2 = pop_exceptions_s;
   str_self = pop_exceptions_n;
   clear_self = buf1;
   L2 = (clear_self==((FSTR) NULL));
   L31_=!(L2); 
   if (L31_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val1 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val1;
   str_self1 = buf1;
   ret_val2 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val = ret_val2;
   ndefer_s2 = STR_ap1693864410(((STR) &POP_EXCEPTION11), ret_val, ((STR) &name76));
   CGEN_d695831906(ndefer_self2);
   SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
 }
 L4 = (ATTR(ATTR(self,current_am_rout_),specul_prefetch)==((AM_LOCAL_EXPR) NULL));
 L51_=!(L4); 
 if (L51_) {
  ndefer_self3 = self;
  plus_self = ((STR) &PREFET1824934344);
  plus_sarg = CGEN_e1639504572(self, ((dAM_EXPR) ATTR(ATTR(self,current_am_rout_),specul_prefetch)));
  ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val3;
  plus_sarg1 = ((STR) &name76);
  ret_val4 = STR_ap2004550586(plus_self1, plus_sarg1);
  ndefer_s3 = ret_val4;
  CGEN_d695831906(ndefer_self3);
  SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
 }
 L6 = (ATTR(arg,val1)==((dAM_EXPR) NULL));
 L71_=!(L6); 
 if (L71_) {
  s1 = CGEN_e1639504572(self, ATTR(arg,val1));
  L8 = CGEN_s395856114(self, ATTR(self,current_am_rout_), s1);
  L91_=!(L8); 
  if (L91_) {
   ndefer_self4 = self;
   cast_self = self;
   cast_dest_tp = ATTR(ATTR(self,current_sig),ret);
   L10 = ATTR(arg,val1);
   cast_src_tp = (*dAM_EXPR_tprdTP[TAG(L10)])(L10);
   cast_expr = s1;
   cast_only_boxing = FALSE;
   ret_val5 = CGEN_d1915593494(cast_self, cast_dest_tp, cast_src_tp, cast_expr, cast_only_boxing, ((dMODE) NULL));
   ndefer_s4 = STR_ap1693864410(((STR) &return4), ret_val5, ((STR) &name16));
   CGEN_d695831906(ndefer_self4);
   SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
  }
 }
 else {
  ndefer_self5 = self;
  ndefer_s5 = ((STR) &return5);
  CGEN_d695831906(ndefer_self5);
  SATTR(ndefer_self5,routine_code,FSTR_p1752847026(ATTR(ndefer_self5,routine_code), ndefer_s5));
 }
 L12 = ATTR(self,indent);
 L131_=(L12)==(1); 
 if (L131_) {
  SATTR(self,saw_outer_return,TRUE);
 }
}
