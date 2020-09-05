#include "sather.h"

/* Layouts */

typedef struct dAM_CALL_EXPR_struct {
 OB_HEADER header;
 } *dAM_CALL_EXPR;

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

typedef struct dPARSE_struct {
 OB_HEADER header;
 } *dPARSE;

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

typedef struct ABSTRA151498155_struct {/* layout for ABSTRACT_FRAME_LAYOUT */
 OB_HEADER header;
 struct PROG_struct *prog;
 struct SIG_struct *sig1;
 STR str;
 } *ABSTRA151498155;

typedef struct AM_CALL_ARG_struct {/* layout for AM_CALL_ARG */
 OB_HEADER header;
 struct dAM_EXPR_struct *expr;
 struct dMODE_struct *mode1;
 } *AM_CALL_ARG;

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

typedef struct BIND_FORTRAN_struct {/* layout for BIND_FORTRAN */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *BIND_FORTRAN;

typedef struct BOUND_1007407331_struct {/* layout for BOUND_ITER_FRAME_LAYOUT */
 OB_HEADER header;
 struct AM_BND1124877163_struct *e;
 struct PROG_struct *prog;
 STR str;
 } *BOUND_1007407331;

typedef struct FRAME_LAYOUT_struct {/* layout for FRAME_LAYOUT */
 OB_HEADER header;
 struct AM_ROUT_DEF_struct *f;
 struct PROG_struct *prog;
 STR str;
 } *FRAME_LAYOUT;

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

typedef struct INT_fo686245905_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT arg1;/* Formal argument: i */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_I,I_u_Im; /* used by builtin iter */
 } *INT_fo686245905_frame;

typedef struct INT_up418511718_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT arg1;/* Formal argument: i */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_I,I_u_Im; /* used by builtin iter */
 } *INT_up418511718_frame;

typedef struct IN_MODE_struct {/* layout for IN_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *IN_MODE;

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

typedef struct AM_IS_VOID_EXPR_struct {/* layout for AM_IS_VOID_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *arg;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 } *AM_IS_VOID_EXPR;

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

typedef struct ARRAYBOOL_struct {/* layout for ARRAY{BOOL} */
 OB_HEADER header;
 INT asize;
 BOOL arr_part[1];
 } *ARRAYBOOL;

typedef struct ARRAYIDENT_struct {/* layout for ARRAY{IDENT} */
 OB_HEADER header;
 INT asize;
 struct IDENT_struct arr_part[1];
 } *ARRAYIDENT;

typedef struct ARRAYI1725945879_frame_struct {
 INT state;
 ARRAYIDENT self;/* Formal argument: self */
 IDENT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYI1725945879_frame;

typedef struct ARRAYINT_struct {/* layout for ARRAY{INT} */
 OB_HEADER header;
 INT asize;
 INT arr_part[1];
 } *ARRAYINT;

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

typedef struct FLISTA1893461511_struct {/* layout for FLIST{AM_BND_CREATE_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_BND1124877163_struct *arr_part[1];
 } *FLISTA1893461511;

typedef struct FLISTSTR_struct {/* layout for FLIST{STR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 STR arr_part[1];
 } *FLISTSTR;

typedef struct FMAPdOBNAMESPACE_struct {/* layout for FMAP{$OB,NAMESPACE} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdOBNAMESPACE_struct arr_part[1];
 } *FMAPdOBNAMESPACE;

typedef struct FMAPIDENTdTP_struct {/* layout for FMAP{IDENT,$TP} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPIDENTdTP_struct arr_part[1];
 } *FMAPIDENTdTP;

typedef struct FMAPSI518162669_struct {/* layout for FMAP{SIG,AM_ROUT_DEF} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSIG578692189_struct arr_part[1];
 } *FMAPSI518162669;

typedef struct FMAPSTRINT_struct {/* layout for FMAP{STR,INT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRINT_struct arr_part[1];
 } *FMAPSTRINT;

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

extern FMAPSI518162669 CODE_F508943357;
extern INOUT_MODE MODES_inout_mode;
extern INT TP_KIN138039446;
extern INT TP_KIN462481068;
extern INT TP_KIND_ref_tp;
extern IN_MODE MODES_in_mode;
extern OUT_MODE MODES_out_mode;
extern TP_CLASS TP_BUI1032249067;
extern TP_CLASS TP_BUI1116976640;
extern TP_CLASS TP_BUI1805763264;
extern TP_CLASS TP_BUI2088674906;
extern TP_CLASS TP_BUI633637838;
extern TP_CLASS TP_BUILTIN_int;

/* Function declarations */


extern RETURNED_CONST AS_CLASS_DEF (**dPARSE1581792481)(dPARSE, IDENT, INT);

extern RETURNED_CONST BOOL (**dAM_EX1117610980)(dAM_EXPR);

extern RETURNED_CONST BOOL (**dMODE_2144735300)(dMODE);

extern RETURNED_CONST BOOL (**dMODE_814247358)(dMODE, dMODE);

extern RETURNED_CONST BOOL (**dTP_is1334578382)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1811059018)(dTP);

extern RETURNED_CONST BOOL (**dTP_is242312711)(dTP, OB);

extern RETURNED_CONST BOOL (**dTP_is418055720)(dTP);

extern RETURNED_CONST BOOL (**dTP_is914808564)(dTP);

extern RETURNED_CONST IMPL (**dTP_implrIMPL)(dTP);

extern RETURNED_CONST INT (**dTP_kindrINT)(dTP);

extern RETURNED_CONST STR (**dSTR_strrSTR)(dSTR);

extern RETURNED_CONST dTP (**dAM_EXPR_tprdTP)(dAM_EXPR);

extern RETURNED_CONST void (**dSTAT_incr_STR)(dSTAT, STR);
ABSTRA151498155 ABSTRA1493033801(ABSTRA151498155, SIG, PROG);
AM_CALL_ARG AM_EXT204419640(AM_EXT204419640_frame);
AM_OB_DEF CGEN_a1221397173(CGEN, dTP);
AM_ROUT_DEF FMAPSI2016001247(FMAPSI518162669, SIG);
ARRAYSTR CGEN_c1561580930(CGEN, AM_EXT_CALL_EXPR, ARRAYSTR);
ARRAYSTR CGEN_e1933193154(CGEN, dAM_CALL_EXPR);
ARRAYSTR CGEN_e862722519(CGEN, dAM_CALL_EXPR);
ARRAYSTR CGEN_f618693208(CGEN, AM_EXT_CALL_EXPR, INT, ARRAYSTR);
BOOL CGEN_i1231884161(CGEN, AM_EXT_CALL_EXPR);
BOOL CGEN_i1538413090(CGEN, dTP);
BOOL CGEN_n1296275061(CGEN, dAM_CALL_EXPR);
BOOL SIG_is600379186(SIG);
BOUND_1007407331 BOUND_11324444(BOUND_1007407331, AM_BND1124877163, PROG);
FLISTA1893461511 FLISTA80425519(FLISTA1893461511, AM_BND1124877163);
FMAPSI518162669 FMAPSI1625125906(FMAPSI518162669, SIG);
FMAPSI518162669 FMAPSI396914288(FMAPSI518162669, SIG, AM_ROUT_DEF);
FMAPdOBNAMESPACE FMAPdO1199466663(FMAPdOBNAMESPACE, OB, NAMESPACE);
FRAME_LAYOUT FRAME_248172754(FRAME_LAYOUT, AM_ROUT_DEF, PROG);
FSETAM_ROUT_DEF FSETAM2044524049(FSETAM_ROUT_DEF, AM_ROUT_DEF);
FSETSIG FSETSI671175003(FSETSIG, SIG);
FSETSTR FSETST1404644833(FSETSTR, STR);
FSETdTP FSETdT1016814448(FSETdTP, dTP);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
IDENT ARRAYI1725945879(ARRAYI1725945879_frame);
IDENT IDENT_1150413730(IDENT, STR);
INT AM_ARR1990213312(AM_ARR1990213312_frame);
INT AM_BND1640868266(AM_BND1640868266_frame);
INT AM_BND83643009(AM_BND83643009_frame);
INT CGEN_c1394504933(CGEN, AM_EXT_CALL_EXPR, INT*);
INT INT_fo686245905(INT_fo686245905_frame);
INT INT_up418511718(INT_up418511718_frame);
INT STR_sizerINT(STR);
NAMESPACE FMAPdO790518483(FMAPdOBNAMESPACE, OB);
STR CGEN_a59983474(CGEN, dTP, STR);
STR CGEN_d1425664957(CGEN, dTP);
STR CGEN_d1915593494(CGEN, dTP, dTP, STR, BOOL, dMODE);
STR CGEN_d815167520(CGEN, dTP);
STR CGEN_d933619848(CGEN, dTP);
STR CGEN_e1038326897(CGEN, AM_EXT_CALL_EXPR);
STR CGEN_e1186969123(CGEN, AM_CALL_ARG);
STR CGEN_e1216665739(CGEN, AM_CALL_ARG);
STR CGEN_e1248284631(CGEN, AM_EXT_CALL_EXPR);
STR CGEN_e1305366955(CGEN, AM_BND1124877163);
STR CGEN_e1345827062(CGEN, AM_ARRAY_EXPR);
STR CGEN_e1445279685(CGEN, AM_STMT_EXPR);
STR CGEN_e1471104673(CGEN, AM_EXT_CALL_EXPR);
STR CGEN_e1639504572(CGEN, dAM_EXPR);
STR CGEN_e1650375497(CGEN, AM_VAT319982528);
STR CGEN_e1818527256(CGEN, AM_BND367211769);
STR CGEN_e237424728(CGEN, AM_BND260301329);
STR CGEN_e409242292(CGEN, AM_BND1124877163, BOOL, FLISTSTR*);
STR CGEN_e414796278(CGEN, AM_CALL_ARG, ARG, BOOL, BOOL);
STR CGEN_e601674892(CGEN, AM_EXT_CALL_EXPR, ARRAYSTR*, ARRAYSTR*, INT*);
STR CGEN_e725358689(CGEN, AM_BND1124877163);
STR CGEN_e73452157(CGEN, AM_BND1124877163);
STR CGEN_e848933497(CGEN, AM_ATTR_EXPR);
STR CGEN_e887129228(CGEN, AM_IS_VOID_EXPR);
STR CGEN_eolrSTR(CGEN);
STR CGEN_f2084214268(CGEN, dTP);
STR CGEN_v1144762521(CGEN, dTP, STR);
STR MANGLE119219986(MANGLE, OB, OB);
STR NAMESP180237388(NAMESPACE, STR);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap1182453786(STR, STR, STR, STR, STR, STR, STR);
STR STR_ap1417971546(STR, STR, STR, STR);
STR STR_ap1693864410(STR, STR, STR);
STR STR_ap1925291910(STR, STR, STR, STR, STR, STR);
STR STR_ap2004550586(STR, STR);
STR STR_ap44172742(STR, STR, STR, STR, STR);
STR STR_fr1097270334(STR, FSTR);
STR TP_ITER_strrSTR(TP_ITER);
TP_CLASS CGEN_a1247402136(CGEN, dTP);
dLAYOUT LAYOUT79932797(LAYOUT_TBL, dTP);
dTP FMAPID86862037(FMAPIDENTdTP, IDENT);
void CGEN_c777074314(CGEN, AM_EXT_CALL_EXPR, ARRAYSTR, ARRAYSTR);
void CGEN_c922285894(CGEN, dAM_CALL_EXPR, ARRAYSTR);
void CGEN_d695831906(CGEN);
void CGEN_e1386576419(CGEN, AM_EXT_CALL_EXPR, ARRAYSTR*, ARRAYSTR*);
void CGEN_e494460490(CGEN, dAM_STMT);
void CGEN_e986815307(CGEN, AM_BND367211769);
void CGEN_f412018626(CGEN, AM_EXT_CALL_EXPR, INT, ARRAYSTR, ARRAYSTR);
void CGEN_p535532953(CGEN, AM_EXT_CALL_EXPR, ARRAYSTR, INT, STR);
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

ARRAYSTR CGEN_c1561580930(CGEN self, AM_EXT_CALL_EXPR ext_call, ARRAYSTR arg_list) {
 ARRAYSTR ret_val;
 ARRAYSTR final_arg_list;
 INT arg_pos = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 STR tmp1;
 ARRAYSTR create_self;
 INT create_n = INT_zero;
 ARRAYSTR ret_val1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 CGEN dec_local_commen;
 dTP dec_local_commen1;
 STR dec_local_commen2;
 STR ret_val3;
 STR res;
 CGEN comment_self;
 CODE_FILE comment_f;
 STR comment_com;
 CODE_FILE plus_self1;
 dSTR plus_s;
 CODE_FILE ret_val4;
 CODE_FILE plus_self2;
 dSTR plus_s1;
 CODE_FILE ret_val5;
 CODE_FILE plus_self3;
 dSTR plus_s2;
 CGEN ndefer_self;
 STR ndefer_s;
 INT L31_;
 ARRAYSTR L4;
 INT L5;
 OB L6;
 INT L7;
 INT L81_;
 INT L9;
 BOOL L101_;
 INT L131_;
 INT L14;
 INT L151_;
 INT L121_,L121_m;
 INT L16;
 AM_CALL_ARG L171_;
 AM_CALL_ARG L18;
 dMODE L19;
 AM_CALL_ARG L201_;
 AM_CALL_ARG L22;
 dAM_EXPR L23;
 extern STR Temporaryfor;
 STR L241_;
 extern STR name78;
 BOOL L25;
 BOOL L261_;
 FSTR L27;
 dSTR L28;
 BOOL L29;
 BOOL L301_;
 FSTR L32;
 dSTR L33;
 extern STR name79;
 FSTR L34;
 dSTR L35;
 extern STR name33;
 STR L361_;
 STR L37;
 extern STR name16;
 STR L391_;
 STR L40;
 INT L421_;
 create_self = ((ARRAYSTR) NULL);
 L31_=(arg_list)==NULL?0:ASIZE((ARRAYSTR)arg_list); 
 create_n = L31_;
 L5 = create_n;
 L7=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L5)*sizeof(STR);
 L6=ZALLOC_BIG(L7);
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L6)->header.destroyed=0;
#endif

 L4 = ((ARRAYSTR) L6);
 L4->asize = L5;
 ret_val1 = L4;
 final_arg_list = ret_val1;
 arg_pos = 0;
 L81_=ASIZE((AM_EXT_CALL_EXPR)ext_call); 
 L9 = L81_;
 L101_=(1)<(L9); 
 if (L101_) {
  {
   BOOL f_L121_ = TRUE;
   L11 = 1;
   L131_=(ext_call)==NULL?0:ASIZE((AM_EXT_CALL_EXPR)ext_call); 
   L14 = L131_;
   L151_=INTMINUS(L14,1); 
   L21 = L151_;
   L121_=L11-1;L121_m=L21; 
   while (1) {
    if(L121_++>=L121_m)  goto after_loop; 
    i = L121_;
    L171_=(AM_CALL_ARG)ARR((AM_EXT_CALL_EXPR)ext_call,i); 
    L18=L171_;
    L19 = ATTR(L18,mode1);
    if ((*dMODE_2144735300[TAG(L19)])(L19)) {
     dec_local_commen = self;
     L201_=(AM_CALL_ARG)ARR((AM_EXT_CALL_EXPR)ext_call,i); 
     L22=L201_;
     L23 = ATTR(L22,expr);
     dec_local_commen1 = (*dAM_EXPR_tprdTP[TAG(L23)])(L23);
     plus_self = ((STR) &Temporaryfor);
     L241_=(STR)ARR((ARRAYSTR)arg_list,arg_pos); 
     plus_sarg = L241_;
     ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
     dec_local_commen2 = ret_val2;
     res = CGEN_d1425664957(dec_local_commen, dec_local_commen1);
     comment_self = dec_local_commen;
     comment_f = ATTR(dec_local_commen,code_c);
     comment_com = dec_local_commen2;
     if (ATTR(comment_self,pretty)) {
      plus_self1 = comment_f;
      plus_s = ((dSTR) ((STR) &name78));
      L25 = (plus_s==((dSTR) NULL));
      L261_=!(L25); 
      if (L261_) {
       L27 = ATTR(plus_self1,ntext);
       L28 = plus_s;
       SATTR(plus_self1,ntext,FSTR_p1752847026(L27, (*dSTR_strrSTR[TAG(L28)])(L28)));
      }
      ret_val4 = plus_self1;
      plus_self2 = ret_val4;
      plus_s1 = ((dSTR) comment_com);
      L29 = (plus_s1==((dSTR) NULL));
      L301_=!(L29); 
      if (L301_) {
       L32 = ATTR(plus_self2,ntext);
       L33 = plus_s1;
       SATTR(plus_self2,ntext,FSTR_p1752847026(L32, (*dSTR_strrSTR[TAG(L33)])(L33)));
      }
      ret_val5 = plus_self2;
      plus_self3 = ret_val5;
      plus_s2 = ((dSTR) ((STR) &name79));
      L34 = ATTR(plus_self3,ntext);
      L35 = plus_s2;
      SATTR(plus_self3,ntext,FSTR_p1752847026(L34, (*dSTR_strrSTR[TAG(L35)])(L35)));
     }
     ret_val3 = res;
     tmp1 = ret_val3;
     ndefer_self = self;
     L361_=(STR)ARR((ARRAYSTR)arg_list,arg_pos); 
     L37 = L361_;
     ndefer_s = STR_ap1417971546(tmp1, ((STR) &name33), L37, ((STR) &name16));
     CGEN_d695831906(ndefer_self);
     SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
     SARR((ARRAYSTR)final_arg_list,arg_pos,(STR)tmp1); 
     ;
    }
    else {
     L391_=(STR)ARR((ARRAYSTR)arg_list,arg_pos); 
     L40 = L391_;
     SARR((ARRAYSTR)final_arg_list,arg_pos,(STR)L40); 
     ;
    }
    L421_=INTPLUS(arg_pos,1); 
    arg_pos = L421_;
   }
  }
  after_loop: ;
 }
 ret_val = final_arg_list;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

ARRAYSTR CGEN_f618693208(CGEN self, AM_EXT_CALL_EXPR ext_call, INT first_arg, ARRAYSTR arg_list) {
 ARRAYSTR ret_val;
 ARRAYSTR final_arg_list;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 INT arg_pos = INT_zero;
 INT L31 = INT_zero;
 INT L41 = INT_zero;
 INT i1 = INT_zero;
 STR tmp1;
 ARRAYSTR create_self;
 INT create_n = INT_zero;
 ARRAYSTR ret_val1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 CGEN dec_local_commen;
 dTP dec_local_commen1;
 STR dec_local_commen2;
 STR ret_val3;
 STR res;
 CGEN comment_self;
 CODE_FILE comment_f;
 STR comment_com;
 CODE_FILE plus_self1;
 dSTR plus_s;
 CODE_FILE ret_val4;
 CODE_FILE plus_self2;
 dSTR plus_s1;
 CODE_FILE ret_val5;
 CODE_FILE plus_self3;
 dSTR plus_s2;
 CGEN ndefer_self;
 STR ndefer_s;
 INT L51_;
 ARRAYSTR L6;
 INT L7;
 OB L8;
 INT L9;
 INT L121_;
 INT L101_,L101_m;
 INT L13;
 STR L141_;
 STR L15;
 INT L171_;
 INT L18;
 BOOL L191_;
 INT L221_;
 INT L23;
 INT L241_;
 INT L201_,L201_m;
 INT L25;
 AM_CALL_ARG L261_;
 AM_CALL_ARG L27;
 dMODE L28;
 AM_CALL_ARG L291_;
 AM_CALL_ARG L30;
 dAM_EXPR L32;
 extern STR Temporaryfor;
 STR L331_;
 extern STR name78;
 BOOL L34;
 BOOL L351_;
 FSTR L36;
 dSTR L37;
 BOOL L38;
 BOOL L391_;
 FSTR L40;
 dSTR L42;
 extern STR name79;
 FSTR L43;
 dSTR L44;
 extern STR name33;
 STR L451_;
 STR L46;
 extern STR name16;
 STR L481_;
 STR L49;
 INT L521_;
 INT L531_;
 INT L54;
 BOOL L551_;
 STR L561_;
 STR L57;
 INT L591_;
 create_self = ((ARRAYSTR) NULL);
 L51_=(arg_list)==NULL?0:ASIZE((ARRAYSTR)arg_list); 
 create_n = L51_;
 L7 = create_n;
 L9=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L7)*sizeof(STR);
 L8=ZALLOC_BIG(L9);
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L8)->header.destroyed=0;
#endif

 L6 = ((ARRAYSTR) L8);
 L6->asize = L7;
 ret_val1 = L6;
 final_arg_list = ret_val1;
 {
  BOOL f_L101_ = TRUE;
  L11 = 0;
  L121_=INTMINUS(first_arg,1); 
  L21 = L121_;
  L101_=L11-1;L101_m=L21; 
  while (1) {
   if(L101_++>=L101_m)  goto after_loop; 
   i = L101_;
   L141_=(STR)ARR((ARRAYSTR)arg_list,i); 
   L15 = L141_;
   SARR((ARRAYSTR)final_arg_list,i,(STR)L15); 
   ;
  }
 }
 after_loop: ;
 arg_pos = first_arg;
 L171_=ASIZE((AM_EXT_CALL_EXPR)ext_call); 
 L18 = L171_;
 L191_=(1)<(L18); 
 if (L191_) {
  {
   BOOL f_L201_ = TRUE;
   L31 = 1;
   L221_=(ext_call)==NULL?0:ASIZE((AM_EXT_CALL_EXPR)ext_call); 
   L23 = L221_;
   L241_=INTMINUS(L23,1); 
   L41 = L241_;
   L201_=L31-1;L201_m=L41; 
   while (1) {
    if(L201_++>=L201_m)  goto after_loop1; 
    i1 = L201_;
    L261_=(AM_CALL_ARG)ARR((AM_EXT_CALL_EXPR)ext_call,i1); 
    L27=L261_;
    L28 = ATTR(L27,mode1);
    if ((*dMODE_2144735300[TAG(L28)])(L28)) {
     dec_local_commen = self;
     L291_=(AM_CALL_ARG)ARR((AM_EXT_CALL_EXPR)ext_call,i1); 
     L30=L291_;
     L32 = ATTR(L30,expr);
     dec_local_commen1 = (*dAM_EXPR_tprdTP[TAG(L32)])(L32);
     plus_self = ((STR) &Temporaryfor);
     L331_=(STR)ARR((ARRAYSTR)arg_list,arg_pos); 
     plus_sarg = L331_;
     ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
     dec_local_commen2 = ret_val2;
     res = CGEN_d1425664957(dec_local_commen, dec_local_commen1);
     comment_self = dec_local_commen;
     comment_f = ATTR(dec_local_commen,code_c);
     comment_com = dec_local_commen2;
     if (ATTR(comment_self,pretty)) {
      plus_self1 = comment_f;
      plus_s = ((dSTR) ((STR) &name78));
      L34 = (plus_s==((dSTR) NULL));
      L351_=!(L34); 
      if (L351_) {
       L36 = ATTR(plus_self1,ntext);
       L37 = plus_s;
       SATTR(plus_self1,ntext,FSTR_p1752847026(L36, (*dSTR_strrSTR[TAG(L37)])(L37)));
      }
      ret_val4 = plus_self1;
      plus_self2 = ret_val4;
      plus_s1 = ((dSTR) comment_com);
      L38 = (plus_s1==((dSTR) NULL));
      L391_=!(L38); 
      if (L391_) {
       L40 = ATTR(plus_self2,ntext);
       L42 = plus_s1;
       SATTR(plus_self2,ntext,FSTR_p1752847026(L40, (*dSTR_strrSTR[TAG(L42)])(L42)));
      }
      ret_val5 = plus_self2;
      plus_self3 = ret_val5;
      plus_s2 = ((dSTR) ((STR) &name79));
      L43 = ATTR(plus_self3,ntext);
      L44 = plus_s2;
      SATTR(plus_self3,ntext,FSTR_p1752847026(L43, (*dSTR_strrSTR[TAG(L44)])(L44)));
     }
     ret_val3 = res;
     tmp1 = ret_val3;
     ndefer_self = self;
     L451_=(STR)ARR((ARRAYSTR)arg_list,arg_pos); 
     L46 = L451_;
     ndefer_s = STR_ap1417971546(tmp1, ((STR) &name33), L46, ((STR) &name16));
     CGEN_d695831906(ndefer_self);
     SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
     SARR((ARRAYSTR)final_arg_list,arg_pos,(STR)tmp1); 
     ;
    }
    else {
     L481_=(STR)ARR((ARRAYSTR)arg_list,arg_pos); 
     L49 = L481_;
     SARR((ARRAYSTR)final_arg_list,arg_pos,(STR)L49); 
     ;
    }
    L521_=INTPLUS(arg_pos,1); 
    arg_pos = L521_;
   }
  }
  after_loop1: ;
  while (1) {
   L531_=(arg_list)==NULL?0:ASIZE((ARRAYSTR)arg_list); 
   L54 = L531_;
   L551_=(arg_pos)<(L54); 
   if (L551_) {
   }
   else {
    goto after_loop2;
   }
   L561_=(STR)ARR((ARRAYSTR)arg_list,arg_pos); 
   L57 = L561_;
   SARR((ARRAYSTR)final_arg_list,arg_pos,(STR)L57); 
   ;
   L591_=INTPLUS(arg_pos,1); 
   arg_pos = L591_;
  }
  after_loop2: ;
 }
 ret_val = final_arg_list;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL CGEN_i1231884161(CGEN self, AM_EXT_CALL_EXPR ext_call) {
 BOOL ret_val = BOOL_zero;
 AM_EXT_CALL_EXPR L11;
 INT L21 = INT_zero;
 AM_CALL_ARG ca;
 dTP L31;
 INT L41 = INT_zero;
 INT L51_c,L51_s;
 AM_CALL_ARG aL51_;
 AM_CALL_ARG L6;
 dAM_EXPR L7;
 dTP L8;
 INT L91_;
 {
  BOOL f_L51_ = TRUE;
  /* loop index variable */
  L41 = 0;
  L11 = ext_call;
  L21 = 1;
  L51_c=L21-1;L51_s=L11==0?0:ASIZE((AM_EXT_CALL_EXPR)L11); 
  while (1) {
   if(++L51_c>=L51_s)  goto after_loop; aL51_=ARR((AM_EXT_CALL_EXPR)L11,L51_c); 
   ca = aL51_;
   L7 = ATTR(ca,expr);
   L31 = (*dAM_EXPR_tprdTP[TAG(L7)])(L7);
   L8 = L31;
   if ((*dTP_is242312711[TAG(L8)])(L8, ((OB) TP_BUI1116976640))) {
    ret_val = TRUE;
    return ret_val;
   }
   L91_=INTPLUS(L41,1); 
   L41 = L91_;
  }
 }
 after_loop: ;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT CGEN_c1394504933(CGEN self, AM_EXT_CALL_EXPR ext_call, INT* num_character_ar) {
 INT L1 = INT_zero;/*Local for arg*/
 INT ret_val = INT_zero;
 INT num_args = INT_zero;
 dTP L21;
 INT L31 = INT_zero;
 INT L41 = INT_zero;
 INT i = INT_zero;
 dTP tp;
 dTP L51;
 AM_EXT_CALL_EXPR tp_self;
 dTP ret_val1;
 AM_EXT_CALL_EXPR tp_self1;
 dTP ret_val2;
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
 INT L171_;
 INT L18;
 INT L191_;
 INT L161_,L161_m;
 INT L20;
 AM_CALL_ARG L221_;
 AM_CALL_ARG L23;
 dAM_EXPR L24;
 dTP L25;
 INT L261_;
 INT L271_;
 dTP L28;
 INT L291_;
 INT L301_;
 INT L321_;
 num_args = 0;
 L1 = 0;
 tp_self = ext_call;
 ret_val1 = ATTR(ATTR(tp_self,fun),ret);
 L6 = (ret_val1==((dTP) NULL));
 L71_=!(L6); 
 if (L71_) {
  tp_self1 = ext_call;
  ret_val2 = ATTR(ATTR(tp_self1,fun),ret);
  L21 = ret_val2;
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
  L31 = 1;
  L171_=(ext_call)==NULL?0:ASIZE((AM_EXT_CALL_EXPR)ext_call); 
  L18 = L171_;
  L191_=INTMINUS(L18,1); 
  L41 = L191_;
  L161_=L31-1;L161_m=L41; 
  while (1) {
   if(L161_++>=L161_m)  goto after_loop; 
   i = L161_;
   L221_=(AM_CALL_ARG)ARR((AM_EXT_CALL_EXPR)ext_call,i); 
   L23=L221_;
   L24 = ATTR(L23,expr);
   tp = (*dAM_EXPR_tprdTP[TAG(L24)])(L24);
   L51 = tp;
   L25 = L51;
   if ((*dTP_is242312711[TAG(L25)])(L25, ((OB) TP_BUI1805763264))) {
    L261_=INTPLUS(num_args,2); 
    num_args = L261_;
    L271_=INTPLUS(L1,1); 
    L1 = L271_;
   }
   else {
    L28 = L51;
    if ((*dTP_is242312711[TAG(L28)])(L28, ((OB) TP_BUI2088674906))) {
     L291_=INTPLUS(num_args,2); 
     num_args = L291_;
     L301_=INTPLUS(L1,1); 
     L1 = L301_;
    }
    else {
     L321_=INTPLUS(num_args,1); 
     num_args = L321_;
    }
   }
  }
 }
 after_loop: ;
 ret_val = num_args;
 *num_character_ar = L1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_d933619848(CGEN self, dTP t) {
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
 extern STR S_struct1;
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
 plus_s3 = ((dSTR) ((STR) &S_struct1));
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
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e1038326897(CGEN self, AM_EXT_CALL_EXPR arg) {
 STR ret_val;
 dTP ext_tp;
 INT L11 = INT_zero;
 ARRAYSTR arg_list;
 STR extern11;
 STR res;
 INT i = INT_zero;
 ARG sa1;
 dTP tp;
 TP_CLASS arr;
 STR local11;
 TP_CLASS kind_self;
 INT ret_val1 = INT_zero;
 TP_CLASS as_tree_self;
 AS_CLASS_DEF ret_val2;
 AM_EXT_CALL_EXPR tp_self;
 dTP ret_val3;
 AM_EXT_CALL_EXPR tp_self1;
 dTP ret_val4;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val5;
 CGEN dec_local_commen;
 dTP dec_local_commen1;
 STR dec_local_commen2;
 STR ret_val6;
 STR res1;
 CGEN comment_self;
 CODE_FILE comment_f;
 STR comment_com;
 CODE_FILE plus_self;
 dSTR plus_s;
 CODE_FILE ret_val7;
 CODE_FILE plus_self1;
 dSTR plus_s1;
 CODE_FILE ret_val8;
 CODE_FILE plus_self2;
 dSTR plus_s2;
 CGEN cast_self;
 dTP cast_dest_tp;
 dTP cast_src_tp;
 STR cast_expr;
 BOOL cast_only_boxing = BOOL_zero;
 STR ret_val9;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val10;
 CGEN arg_type_str_sel;
 ARG arg_type_str_e;
 STR ret_val11;
 STR res2;
 CGEN mang_self2;
 OB mang_ob2;
 STR ret_val12;
 STR plus_self3;
 STR plus_sarg;
 STR ret_val13;
 CGEN cast_self1;
 dTP cast_dest_tp1;
 dTP cast_src_tp1;
 STR cast_expr1;
 BOOL cast_only_boxing1 = BOOL_zero;
 STR ret_val14;
 STR plus_self4;
 STR plus_sarg1;
 STR ret_val15;
 AM_EXT_CALL_EXPR ext_tp_self;
 dTP ret_val16;
 CODE_FILE uses_extern_self;
 STR uses_extern_dec;
 INT L2;
 INT L3;
 BOOL L41_;
 dPARSE L5;
 IDENT L6;
 ARRAYdTP L7;
 INT L81_;
 dPARSE L9;
 BOOL L10;
 BOOL L121_;
 extern STR extern2;
 BOOL L13;
 BOOL L141_;
 extern STR name10;
 extern STR void6;
 extern STR name24;
 dTP L15;
 INT L16;
 INT L17;
 BOOL L181_;
 extern STR name24;
 INT L191_;
 INT L20;
 BOOL L211_;
 BOOL L22;
 BOOL L231_;
 ARRAYARG L24;
 INT L251_;
 INT L26;
 ARG L271_;
 BOOL L28;
 BOOL L291_;
 extern STR Localf164518964;
 extern STR name78;
 BOOL L30;
 BOOL L311_;
 FSTR L32;
 dSTR L33;
 BOOL L34;
 BOOL L351_;
 FSTR L36;
 dSTR L37;
 extern STR name79;
 FSTR L38;
 dSTR L39;
 extern STR name36;
 AM_CALL_ARG L401_;
 AM_CALL_ARG L42;
 dAM_EXPR L43;
 STR L441_;
 extern STR name16;
 ARRAYdTP L45;
 dTP L461_;
 extern STR name128;
 extern STR name75;
 extern STR NULLNULL;
 extern STR arr_part2;
 BOOL L47;
 dMODE L48;
 dMODE L49;
 extern STR name58;
 AM_CALL_ARG L501_;
 AM_CALL_ARG L51;
 dAM_EXPR L52;
 STR L531_;
 INT L541_;
 INT L551_;
 INT L56;
 BOOL L571_;
 extern STR name29;
 extern STR name29;
 extern STR name76;
 dTP L58;
 BOOL L59;
 BOOL L601_;
 extern STR name26;
 ext_tp = ATTR(ATTR(arg,fun),tp);
 switch (TAG(ext_tp)) {
  case TP_CLASS_tag:
   kind_self = ((TP_CLASS) ext_tp);
   L2 = ATTR(kind_self,kind_cache);
   L3 = TP_KIN138039446;
   L41_=(L2)==(L3); 
   if (L41_) {
    as_tree_self = kind_self;
    if ((ATTR(as_tree_self,my_as_tree)==((AS_CLASS_DEF) NULL))) {
     L5 = ATTR(ATTR(as_tree_self,prog),parse);
     L6 = ATTR(as_tree_self,name1);
     L7 = ATTR(as_tree_self,params);
     L81_=(L7)==NULL?0:ASIZE((ARRAYdTP)L7); 
     L9 = L5;
     SATTR(as_tree_self,my_as_tree,(*dPARSE1581792481[TAG(L9)])(L9, L6, L81_));
    }
    ret_val2 = ATTR(as_tree_self,my_as_tree);
    L10 = (ret_val2==((AS_CLASS_DEF) NULL));
    L121_=!(L10); 
    if (L121_) {
     SATTR(kind_self,kind_cache,ATTR(ATTR(kind_self,my_as_tree),kind));
    }
   }
   ret_val1 = ATTR(kind_self,kind_cache);
   L11 = ret_val1;
   switch (L11) {
    case 6: 
     ret_val = CGEN_e1471104673(self, arg);
     return ret_val;
     break;
    case 5: 
     if (SIG_is600379186(ATTR(arg,fun))) {
      ret_val = CGEN_e1248284631(self, arg);
      return ret_val;
     }
     break;
    default: ;
   } break;
  default: ;
   FATAL("No applicable type in typecase\nin CGEN::emit_am_ext_call_expr(AM_EXT_CALL_EXPR):STR\n./Back/cgen.sa:5973:14");
 }
 arg_list = CGEN_e862722519(self, ((dAM_CALL_EXPR) arg));
 extern11 = ((STR) &extern2);
 tp_self = arg;
 ret_val3 = ATTR(ATTR(tp_self,fun),ret);
 L13 = (ret_val3==((dTP) NULL));
 L141_=!(L13); 
 if (L141_) {
  mang_self = self;
  tp_self1 = arg;
  ret_val4 = ATTR(ATTR(tp_self1,fun),ret);
  mang_ob = ((OB) ret_val4);
  ret_val5 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
  extern11 = STR_ap1693864410(extern11, ret_val5, ((STR) &name10));
 }
 else {
  extern11 = STR_ap2004550586(extern11, ((STR) &void6));
 }
 extern11 = STR_ap1693864410(extern11, ATTR(arg,nm).str, ((STR) &name24));
 res = ATTR(arg,nm).str;
 L15 = ATTR(ATTR(arg,fun),tp);
 L16 = (*dTP_kindrINT[TAG(L15)])(L15);
 L17 = TP_KIN462481068;
 L181_=(L16)==(L17); 
 if (L181_) {
  res = STR_ap1693864410(ATTR(self,fortran_prepend_), res, ATTR(self,fortran_append_u));
 }
 res = STR_ap2004550586(res, ((STR) &name24));
 i = 1;
 while (1) {
  L191_=ASIZE((ARRAYSTR)arg_list); 
  L20 = L191_;
  L211_=(i)<(L20); 
  L22 = L211_;
  L231_=!(L22); 
  if (L231_) {
   goto after_loop;
  }
  L24 = ATTR(ATTR(arg,fun),args);
  L251_=INTMINUS(i,1); 
  L26 = L251_;
  L271_=(ARG)ARR((ARRAYARG)L24,L26); 
  sa1 = L271_;
  tp = ATTR(sa1,tp);
  arr = CGEN_a1247402136(self, tp);
  L28 = (arr==((TP_CLASS) NULL));
  L291_=!(L28); 
  if (L291_) {
   dec_local_commen = self;
   dec_local_commen1 = tp;
   dec_local_commen2 = ((STR) &Localf164518964);
   res1 = CGEN_d1425664957(dec_local_commen, dec_local_commen1);
   comment_self = dec_local_commen;
   comment_f = ATTR(dec_local_commen,code_c);
   comment_com = dec_local_commen2;
   if (ATTR(comment_self,pretty)) {
    plus_self = comment_f;
    plus_s = ((dSTR) ((STR) &name78));
    L30 = (plus_s==((dSTR) NULL));
    L311_=!(L30); 
    if (L311_) {
     L32 = ATTR(plus_self,ntext);
     L33 = plus_s;
     SATTR(plus_self,ntext,FSTR_p1752847026(L32, (*dSTR_strrSTR[TAG(L33)])(L33)));
    }
    ret_val7 = plus_self;
    plus_self1 = ret_val7;
    plus_s1 = ((dSTR) comment_com);
    L34 = (plus_s1==((dSTR) NULL));
    L351_=!(L34); 
    if (L351_) {
     L36 = ATTR(plus_self1,ntext);
     L37 = plus_s1;
     SATTR(plus_self1,ntext,FSTR_p1752847026(L36, (*dSTR_strrSTR[TAG(L37)])(L37)));
    }
    ret_val8 = plus_self1;
    plus_self2 = ret_val8;
    plus_s2 = ((dSTR) ((STR) &name79));
    L38 = ATTR(plus_self2,ntext);
    L39 = plus_s2;
    SATTR(plus_self2,ntext,FSTR_p1752847026(L38, (*dSTR_strrSTR[TAG(L39)])(L39)));
   }
   ret_val6 = res1;
   local11 = ret_val6;
   ndefer_self = self;
   cast_self = self;
   cast_dest_tp = tp;
   L401_=(AM_CALL_ARG)ARR((AM_EXT_CALL_EXPR)arg,i); 
   L42=L401_;
   L43 = ATTR(L42,expr);
   cast_src_tp = (*dAM_EXPR_tprdTP[TAG(L43)])(L43);
   L441_=(STR)ARR((ARRAYSTR)arg_list,i); 
   cast_expr = L441_;
   cast_only_boxing = FALSE;
   ret_val9 = CGEN_d1915593494(cast_self, cast_dest_tp, cast_src_tp, cast_expr, cast_only_boxing, ((dMODE) NULL));
   ndefer_s = STR_ap1417971546(local11, ((STR) &name36), ret_val9, ((STR) &name16));
   CGEN_d695831906(ndefer_self);
   SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
   mang_self1 = self;
   L45 = ATTR(arr,params);
   L461_=(dTP)ARR((ARRAYdTP)L45,0); 
   mang_ob1 = ((OB) L461_);
   ret_val10 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
   extern11 = STR_ap1693864410(extern11, ret_val10, ((STR) &name128));
   res = STR_ap1925291910(res, ((STR) &name75), local11, ((STR) &NULLNULL), local11, ((STR) &arr_part2));
  }
  else {
   arg_type_str_sel = self;
   arg_type_str_e = sa1;
   mang_self2 = arg_type_str_sel;
   mang_ob2 = ((OB) ATTR(arg_type_str_e,tp));
   ret_val12 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, ((OB) NULL));
   res2 = ret_val12;
   L48 = ATTR(arg_type_str_e,mode1);
   if ((*dMODE_814247358[TAG(L48)])(L48, ((dMODE) MODES_inout_mode))) {
    L47 = TRUE;
   } else {
    L49 = ATTR(arg_type_str_e,mode1);
    L47 = (*dMODE_814247358[TAG(L49)])(L49, ((dMODE) MODES_out_mode));
   }
   if (L47) {
    plus_self3 = res2;
    plus_sarg = ((STR) &name58);
    ret_val13 = STR_ap2004550586(plus_self3, plus_sarg);
    res2 = ret_val13;
   }
   ret_val11 = res2;
   extern11 = STR_ap2004550586(extern11, ret_val11);
   cast_self1 = self;
   cast_dest_tp1 = tp;
   L501_=(AM_CALL_ARG)ARR((AM_EXT_CALL_EXPR)arg,i); 
   L51=L501_;
   L52 = ATTR(L51,expr);
   cast_src_tp1 = (*dAM_EXPR_tprdTP[TAG(L52)])(L52);
   L531_=(STR)ARR((ARRAYSTR)arg_list,i); 
   cast_expr1 = L531_;
   cast_only_boxing1 = FALSE;
   ret_val14 = CGEN_d1915593494(cast_self1, cast_dest_tp1, cast_src_tp1, cast_expr1, cast_only_boxing1, ((dMODE) NULL));
   res = STR_ap2004550586(res, ret_val14);
  }
  L541_=INTPLUS(i,1); 
  i = L541_;
  L551_=ASIZE((ARRAYSTR)arg_list); 
  L56 = L551_;
  L571_=(i)<(L56); 
  if (L571_) {
   extern11 = STR_ap2004550586(extern11, ((STR) &name29));
   res = STR_ap2004550586(res, ((STR) &name29));
  }
 }
 after_loop: ;
 plus_self4 = ((STR) &name76);
 plus_sarg1 = CGEN_eolrSTR(self);
 ret_val15 = STR_ap2004550586(plus_self4, plus_sarg1);
 extern11 = STR_ap2004550586(extern11, ret_val15);
 ext_tp_self = arg;
 ret_val16 = ATTR(ATTR(ext_tp_self,fun),tp);
 L58 = ret_val16;
 L59 = (*dTP_is914808564[TAG(L58)])(L58);
 L601_=!(L59); 
 if (L601_) {
  uses_extern_self = ATTR(self,code_c);
  uses_extern_dec = extern11;
  SATTR(uses_extern_self,externs1,FSETST1404644833(ATTR(uses_extern_self,externs1), uses_extern_dec));
 }
 ret_val = STR_ap2004550586(res, ((STR) &name26));
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e1186969123(CGEN self, AM_CALL_ARG a) {
 STR ret_val;
 STR res = ((STR) NULL);
 STR se = ((STR) NULL);
 dAM_EXPR aexpr;
 dMODE amode1;
 dTP atp;
 dMODE L11;
 STR tmp1;
 STR tmp2;
 STR tmp3;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN ndefer_self1;
 STR ndefer_s1;
 CGEN ndefer_self2;
 STR ndefer_s2;
 dAM_EXPR L2;
 dMODE L3;
 BOOL L4;
 dTP L5;
 dAM_EXPR L6;
 extern STR name36;
 extern STR name16;
 dMODE L7;
 BOOL L8;
 dTP L9;
 extern STR name36;
 extern STR name16;
 dMODE L10;
 BOOL L12;
 dTP L13;
 extern STR name36;
 extern STR name16;
 se = CGEN_e1639504572(self, ATTR(a,expr));
 aexpr = ATTR(a,expr);
 amode1 = ATTR(a,mode1);
 L2 = aexpr;
 atp = (*dAM_EXPR_tprdTP[TAG(L2)])(L2);
 L11 = amode1;
 L3 = L11;
 if ((*dMODE_814247358[TAG(L3)])(L3, ((dMODE) MODES_in_mode))) {
  if (aexpr==NULL) {
   goto other373;
  } else
  switch (TAG(aexpr)) {
   case AM_LOCAL_EXPR_tag:
    res = se; break;
   default: ;
   other373: ;
    if (CGEN_i1538413090(self, atp)) {
     L4 = TRUE;
    } else {
     L5 = atp;
     L4 = (*dTP_is242312711[TAG(L5)])(L5, ((OB) TP_BUI1116976640));
    }
    if (L4) {
     L6 = aexpr;
     tmp1 = CGEN_d1425664957(self, (*dAM_EXPR_tprdTP[TAG(L6)])(L6));
     ndefer_self = self;
     ndefer_s = STR_ap1417971546(tmp1, ((STR) &name36), se, ((STR) &name16));
     CGEN_d695831906(ndefer_self);
     SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
     res = tmp1;
    }
    else {
     res = se;
    }
  }
 }
 else {
  L7 = L11;
  if ((*dMODE_814247358[TAG(L7)])(L7, ((dMODE) MODES_out_mode))) {
   if (aexpr==NULL) {
    goto other374;
   } else
   switch (TAG(aexpr)) {
    case AM_LOCAL_EXPR_tag:
     res = se; break;
    default: ;
    other374: ;
     if (CGEN_i1538413090(self, atp)) {
      L8 = TRUE;
     } else {
      L9 = atp;
      L8 = (*dTP_is242312711[TAG(L9)])(L9, ((OB) TP_BUI1116976640));
     }
     if (L8) {
      tmp2 = CGEN_d1425664957(self, atp);
      ndefer_self1 = self;
      ndefer_s1 = STR_ap1417971546(tmp2, ((STR) &name36), se, ((STR) &name16));
      CGEN_d695831906(ndefer_self1);
      SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
      res = se;
     }
     else {
      res = se;
     }
   }
  }
  else {
   L10 = L11;
   if ((*dMODE_814247358[TAG(L10)])(L10, ((dMODE) MODES_inout_mode))) {
    if (aexpr==NULL) {
     goto other375;
    } else
    switch (TAG(aexpr)) {
     case AM_LOCAL_EXPR_tag:
      res = se; break;
     default: ;
     other375: ;
      if (CGEN_i1538413090(self, atp)) {
       L12 = TRUE;
      } else {
       L13 = atp;
       L12 = (*dTP_is242312711[TAG(L13)])(L13, ((OB) TP_BUI1116976640));
      }
      if (L12) {
       tmp3 = CGEN_d1425664957(self, atp);
       ndefer_self2 = self;
       ndefer_s2 = STR_ap1417971546(tmp3, ((STR) &name36), se, ((STR) &name16));
       CGEN_d695831906(ndefer_self2);
       SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
       res = se;
      }
      else {
       res = se;
      }
    }
   }
   else {
    FATAL("No applicable target in case statement\nin CGEN::emit_fortran_call_arg(AM_CALL_ARG):STR\n./Back/fortran_call.sa:386:10");
   }
  }
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e1216665739(CGEN self, AM_CALL_ARG a) {
 STR ret_val;
 STR res = ((STR) NULL);
 STR se = ((STR) NULL);
 dAM_EXPR aexpr;
 dMODE amode1;
 dTP atp;
 dMODE L11;
 dAM_EXPR L2;
 dMODE L3;
 dTP L4;
 dMODE L5;
 dMODE L6;
 se = CGEN_e1639504572(self, ATTR(a,expr));
 aexpr = ATTR(a,expr);
 amode1 = ATTR(a,mode1);
 L2 = aexpr;
 atp = (*dAM_EXPR_tprdTP[TAG(L2)])(L2);
 L11 = amode1;
 L3 = L11;
 if ((*dMODE_814247358[TAG(L3)])(L3, ((dMODE) MODES_in_mode))) {
  L4 = atp;
  if ((*dTP_is1334578382[TAG(L4)])(L4)) {
   res = se;
  }
  else {
   res = se;
  }
 }
 else {
  L5 = L11;
  if ((*dMODE_814247358[TAG(L5)])(L5, ((dMODE) MODES_out_mode))) {
   res = se;
  }
  else {
   L6 = L11;
   if ((*dMODE_814247358[TAG(L6)])(L6, ((dMODE) MODES_inout_mode))) {
    res = se;
   }
   else {
    FATAL("No applicable target in case statement\nin CGEN::emit_c_call_arg(AM_CALL_ARG):STR\n./Back/c_call.sa:165:10");
   }
  }
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e1248284631(CGEN self, AM_EXT_CALL_EXPR ext_call) {
 STR ret_val;
 STR res_arg = ((STR) NULL);
 STR res = ((STR) NULL);
 ARRAYSTR arg_list = ((ARRAYSTR) NULL);
 ARRAYSTR final_arg_list = ((ARRAYSTR) NULL);
 ARRAYSTR type_list = ((ARRAYSTR) NULL);
 BOOL need_copy = BOOL_zero;
 STR extern11;
 INT arg_pos = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 STR tmp_ret;
 AM_EXT_CALL_EXPR tp_self;
 dTP ret_val1;
 AM_EXT_CALL_EXPR tp_self1;
 dTP ret_val2;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val3;
 CGEN bind_c_name_self;
 STR bind_c_name_name;
 STR ret_val4;
 STR res1 = ((STR) NULL);
 AM_EXT_CALL_EXPR ext_tp_self;
 dTP ret_val5;
 CODE_FILE uses_extern_self;
 STR uses_extern_dec;
 AM_EXT_CALL_EXPR tp_self2;
 dTP ret_val6;
 AM_EXT_CALL_EXPR tp_self3;
 dTP ret_val7;
 CGEN ndefer_self;
 STR ndefer_s;
 STR plus_self;
 STR plus_sarg;
 STR ret_val8;
 CGEN ndefer_self1;
 STR ndefer_s1;
 ARRAYSTR* L3;
 ARRAYSTR* L4;
 extern STR extern2;
 BOOL L5;
 BOOL L61_;
 extern STR name10;
 extern STR void6;
 extern STR name24;
 extern STR name24;
 INT L81_;
 INT L9;
 INT L101_;
 INT L71_,L71_m;
 INT L12;
 AM_CALL_ARG L131_;
 AM_CALL_ARG L14;
 dMODE L15;
 extern STR name92;
 STR L161_;
 STR L171_;
 STR L18;
 extern STR name58;
 STR L191_;
 STR L201_;
 INT L221_;
 INT L23;
 INT L241_;
 INT L25;
 BOOL L261_;
 extern STR name29;
 extern STR name29;
 INT L271_;
 extern STR name127;
 extern STR name26;
 dTP L28;
 BOOL L29;
 BOOL L301_;
 BOOL L31;
 BOOL L321_;
 extern STR name33;
 extern STR name16;
 extern STR name16;
 L3 = &arg_list;
 L4 = &type_list;
 CGEN_e1386576419(self, ext_call, L3, L4);
 need_copy = CGEN_n1296275061(self, ((dAM_CALL_EXPR) ext_call));
 if (need_copy) {
  final_arg_list = CGEN_c1561580930(self, ext_call, arg_list);
 }
 else {
  final_arg_list = arg_list;
 }
 extern11 = ((STR) &extern2);
 tp_self = ext_call;
 ret_val1 = ATTR(ATTR(tp_self,fun),ret);
 L5 = (ret_val1==((dTP) NULL));
 L61_=!(L5); 
 if (L61_) {
  mang_self = self;
  tp_self1 = ext_call;
  ret_val2 = ATTR(ATTR(tp_self1,fun),ret);
  mang_ob = ((OB) ret_val2);
  ret_val3 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
  extern11 = STR_ap1693864410(extern11, ret_val3, ((STR) &name10));
 }
 else {
  extern11 = STR_ap2004550586(extern11, ((STR) &void6));
 }
 bind_c_name_self = self;
 bind_c_name_name = ATTR(ext_call,nm).str;
 res1 = bind_c_name_name;
 ret_val4 = res1;
 res = ret_val4;
 MANGLE1827205184(ATTR(self,mangler), ((OB) ATTR(ext_call,fun)), res, ((OB) NULL));
 extern11 = STR_ap1693864410(extern11, res, ((STR) &name24));
 res = STR_ap2004550586(res, ((STR) &name24));
 arg_pos = 0;
 {
  BOOL f_L71_ = TRUE;
  L11 = 1;
  L81_=(ext_call)==NULL?0:ASIZE((AM_EXT_CALL_EXPR)ext_call); 
  L9 = L81_;
  L101_=INTMINUS(L9,1); 
  L21 = L101_;
  L71_=L11-1;L71_m=L21; 
  while (1) {
   if(L71_++>=L71_m)  goto after_loop; 
   i = L71_;
   L131_=(AM_CALL_ARG)ARR((AM_EXT_CALL_EXPR)ext_call,i); 
   L14=L131_;
   L15 = ATTR(L14,mode1);
   if ((*dMODE_2144735300[TAG(L15)])(L15)) {
    L161_=(STR)ARR((ARRAYSTR)final_arg_list,arg_pos); 
    res = STR_ap1693864410(res, ((STR) &name92), L161_);
    L171_=(STR)ARR((ARRAYSTR)type_list,arg_pos); 
    L18 = L171_;
    extern11 = STR_ap1693864410(extern11, L18, ((STR) &name58));
   }
   else {
    L191_=(STR)ARR((ARRAYSTR)final_arg_list,arg_pos); 
    res = STR_ap2004550586(res, L191_);
    L201_=(STR)ARR((ARRAYSTR)type_list,arg_pos); 
    extern11 = STR_ap2004550586(extern11, L201_);
   }
   L221_=(final_arg_list)==NULL?0:ASIZE((ARRAYSTR)final_arg_list); 
   L23 = L221_;
   L241_=INTMINUS(L23,1); 
   L25 = L241_;
   L261_=(arg_pos)<(L25); 
   if (L261_) {
    extern11 = STR_ap2004550586(extern11, ((STR) &name29));
    res = STR_ap2004550586(res, ((STR) &name29));
   }
   L271_=INTPLUS(arg_pos,1); 
   arg_pos = L271_;
  }
 }
 after_loop: ;
 extern11 = STR_ap2004550586(extern11, ((STR) &name127));
 res = STR_ap2004550586(res, ((STR) &name26));
 ext_tp_self = ext_call;
 ret_val5 = ATTR(ATTR(ext_tp_self,fun),tp);
 L28 = ret_val5;
 L29 = (*dTP_is914808564[TAG(L28)])(L28);
 L301_=!(L29); 
 if (L301_) {
  uses_extern_self = ATTR(self,code_c);
  uses_extern_dec = extern11;
  SATTR(uses_extern_self,externs1,FSETST1404644833(ATTR(uses_extern_self,externs1), uses_extern_dec));
 }
 if (need_copy) {
  tp_self2 = ext_call;
  ret_val6 = ATTR(ATTR(tp_self2,fun),ret);
  L31 = (ret_val6==((dTP) NULL));
  L321_=!(L31); 
  if (L321_) {
   tp_self3 = ext_call;
   ret_val7 = ATTR(ATTR(tp_self3,fun),ret);
   tmp_ret = CGEN_d1425664957(self, ret_val7);
   ndefer_self = self;
   ndefer_s = STR_ap1417971546(tmp_ret, ((STR) &name33), res, ((STR) &name16));
   CGEN_d695831906(ndefer_self);
   SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
   res = tmp_ret;
  }
  else {
   ndefer_self1 = self;
   plus_self = res;
   plus_sarg = ((STR) &name16);
   ret_val8 = STR_ap2004550586(plus_self, plus_sarg);
   ndefer_s1 = ret_val8;
   CGEN_d695831906(ndefer_self1);
   SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
   res = ((STR) NULL);
  }
  CGEN_c777074314(self, ext_call, arg_list, final_arg_list);
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e1305366955(CGEN self, AM_BND1124877163 arg) {
 STR ret_val;
 BOOL binding_iter_clo = BOOL_zero;
 IDENT ident1 = IDENT_zero;
 AM_ROUT_DEF a;
 STR l_res;
 AM_BND1124877163 L11;
 INT i = INT_zero;
 STR entry1 = ((STR) NULL);
 INT idx = INT_zero;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 CODE_FILE uses_bnd_iter_cr;
 AM_BND1124877163 uses_bnd_iter_cr1;
 CODE_FILE uses_iter_self;
 SIG uses_iter_s;
 CGEN barf_self;
 STR barf_msg;
 PROG barf_self1;
 STR barf_msg1;
 PROG barf_at_self;
 STR barf_at_msg;
 dPROG_ERR barf_at_at;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 UNIX exit_self;
 INT exit_code = INT_zero;
 MANGLE genlocal_self;
 OB genlocal_ns;
 STR ret_val3;
 STR name111;
 MANGLE space_self;
 OB space_ns;
 NAMESPACE ret_val4;
 NAMESPACE r;
 NAMESPACE create_self;
 NAMESPACE ret_val5;
 NAMESPACE r1;
 NAMEMAP create_self1;
 NAMEMAP ret_val6;
 FSETSTR create_self2;
 FSETSTR ret_val7;
 FMAPSTRINT create_self3;
 FMAPSTRINT ret_val8;
 CODE_FILE plus_self1;
 dSTR plus_s;
 CODE_FILE ret_val9;
 CODE_FILE plus_self2;
 dSTR plus_s1;
 CODE_FILE ret_val10;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val11;
 CODE_FILE plus_self3;
 dSTR plus_s2;
 CODE_FILE ret_val12;
 CODE_FILE plus_self4;
 dSTR plus_s3;
 CODE_FILE ret_val13;
 CODE_FILE plus_self5;
 dSTR plus_s4;
 CODE_FILE ret_val14;
 CODE_FILE plus_self6;
 dSTR plus_s5;
 CGEN ndefer_self;
 STR ndefer_s;
 STR plus_self7;
 STR plus_sarg1;
 STR ret_val15;
 STR plus_self8;
 STR plus_sarg2;
 STR ret_val16;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val17;
 STR plus_self9;
 STR plus_sarg3;
 STR ret_val18;
 STR plus_self10;
 STR plus_sarg4;
 STR ret_val19;
 CGEN ndefer_self1;
 STR ndefer_s1;
 STR plus_self11;
 STR plus_sarg5;
 STR ret_val20;
 AM_BND1124877163 tp_self;
 dTP ret_val21;
 CGEN tag_for_self;
 dTP tag_for_tp;
 STR ret_val22;
 STR res;
 CGEN mang_self2;
 OB mang_ob2;
 STR ret_val23;
 CGEN forbid_self;
 STR forbid_s;
 MANGLE forbid_self1;
 STR forbid_s1;
 STR plus_self12;
 STR plus_sarg6;
 STR ret_val24;
 STR plus_self13;
 CHAR plus_arg = CHAR_zero;
 STR ret_val25;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val26;
 STR create_self4;
 CHAR create_c = CHAR_zero;
 STR ret_val27;
 STR r2;
 CGEN ndefer_self2;
 STR ndefer_s2;
 STR plus_self14;
 STR plus_sarg7;
 STR ret_val28;
 CGEN mang_self3;
 OB mang_ob3;
 STR ret_val29;
 STR plus_self15;
 STR plus_sarg8;
 STR ret_val30;
 STR plus_self16;
 STR plus_sarg9;
 STR ret_val31;
 CGEN ndefer_self3;
 STR ndefer_s3;
 STR plus_self17;
 STR plus_sarg10;
 STR ret_val32;
 STR plus_self18;
 STR plus_sarg11;
 STR ret_val33;
 CGEN ndefer_self4;
 STR ndefer_s4;
 STR plus_self19;
 STR plus_sarg12;
 STR ret_val34;
 CGEN ndefer_self5;
 STR ndefer_s5;
 CGEN mang_self4;
 OB mang_ob4;
 OB mang_ns;
 STR ret_val35;
 STR plus_self20;
 STR plus_sarg13;
 STR ret_val36;
 STR plus_self21;
 STR plus_sarg14;
 STR ret_val37;
 CGEN defer_self;
 STR defer_s;
 CGEN defer_self1;
 STR defer_s1;
 CGEN mang_self5;
 OB mang_ob5;
 STR ret_val38;
 STR plus_self22;
 STR plus_sarg15;
 STR ret_val39;
 CGEN defer_self2;
 STR defer_s2;
 CGEN defer_self3;
 STR defer_s3;
 CGEN mang_self6;
 OB mang_ob6;
 STR ret_val40;
 STR plus_self23;
 STR plus_sarg16;
 STR ret_val41;
 STR plus_self24;
 STR plus_sarg17;
 STR ret_val42;
 CGEN defer_self4;
 STR defer_s4;
 CGEN comment_self;
 STR comment_com;
 STR plus_self25;
 STR plus_sarg18;
 STR ret_val43;
 STR plus_self26;
 STR plus_sarg19;
 STR ret_val44;
 CGEN ndefer_self6;
 STR ndefer_s6;
 INT L21 = INT_zero;
 CGEN cast_self;
 dTP cast_dest_tp;
 dTP cast_src_tp;
 STR cast_expr;
 BOOL cast_only_boxing = BOOL_zero;
 STR ret_val45;
 STR plus_self27;
 STR plus_sarg20;
 STR ret_val46;
 STR plus_self28;
 INT plus_arg1 = INT_zero;
 STR ret_val47;
 STR s1;
 INT str_self1 = INT_zero;
 STR ret_val48;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val49;
 FSTR str_self2;
 STR ret_val50;
 STR plus_self29;
 STR plus_sarg21;
 STR ret_val51;
 STR plus_self30;
 STR plus_sarg22;
 STR ret_val52;
 STR plus_self31;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val53;
 STR s2;
 CHAR str_self3 = CHAR_zero;
 STR ret_val54;
 STR create_self5;
 CHAR create_c1 = CHAR_zero;
 STR ret_val55;
 STR r3;
 CGEN ndefer_self7;
 STR ndefer_s7;
 CGEN cast_self1;
 dTP cast_dest_tp1;
 dTP cast_src_tp1;
 STR cast_expr1;
 BOOL cast_only_boxing1 = BOOL_zero;
 STR ret_val56;
 STR plus_self32;
 STR plus_sarg23;
 STR ret_val57;
 STR plus_self33;
 INT plus_arg3 = INT_zero;
 STR ret_val58;
 STR s3;
 INT str_self4 = INT_zero;
 STR ret_val59;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val60;
 FSTR str_self5;
 STR ret_val61;
 STR plus_self34;
 STR plus_sarg24;
 STR ret_val62;
 STR plus_self35;
 STR plus_sarg25;
 STR ret_val63;
 STR plus_self36;
 CHAR plus_arg4 = CHAR_zero;
 STR ret_val64;
 STR s4;
 CHAR str_self6 = CHAR_zero;
 STR ret_val65;
 STR create_self6;
 CHAR create_c2 = CHAR_zero;
 STR ret_val66;
 STR r4;
 CGEN ndefer_self8;
 STR ndefer_s8;
 CODE_FILE plus_self37;
 dSTR plus_s6;
 CGEN make_sure_emitte;
 SIG make_sure_emitte1;
 AM_ROUT_DEF ard;
 AM_BND1124877163 tp_self1;
 dTP ret_val67;
 CGEN mang_self7;
 OB mang_ob7;
 STR ret_val68;
 STR plus_self38;
 STR plus_sarg26;
 STR ret_val69;
 STR plus_self39;
 STR plus_sarg27;
 STR ret_val70;
 STR plus_self40;
 STR plus_sarg28;
 STR ret_val71;
 extern STR callb;
 BOOL L31_;
 dTP L4;
 CODE_FILE L5;
 extern STR Itersi1831213836;
 extern STR Intern28965746;
 CODE_FILE L6;
 CODE_FILE L7;
 NAMESPACE L8;
 OB L9;
 extern STR L;
 BOOL L10;
 BOOL L121_;
 FSTR L13;
 dSTR L14;
 dSTR L15;
 OB L16;
 BOOL L17;
 BOOL L181_;
 FSTR L19;
 dSTR L20;
 BOOL L22;
 BOOL L231_;
 FSTR L24;
 dSTR L25;
 extern STR S_iter_ob;
 BOOL L26;
 BOOL L271_;
 FSTR L28;
 dSTR L29;
 BOOL L30;
 BOOL L321_;
 FSTR L33;
 dSTR L34;
 dSTR L35;
 OB L36;
 FSTR L37;
 dSTR L38;
 extern STR name10;
 extern STR OBALLOC1;
 extern STR S_iter_ob1;
 extern STR headertag1;
 extern STR S_tag;
 STR L39;
 INT L40;
 OB L41;
 INT L42;
 extern STR sizesizeofstruct;
 extern STR S_iter_ob_struct;
 extern STR state01;
 extern STR initializestate;
 extern STR call3;
 BOOL L44;
 BOOL L451_;
 extern STR name24;
 extern STR name132;
 extern STR void8;
 extern STR S_iter_ob2;
 extern STR struct139248200;
 extern STR name87;
 extern STR S_call_function;
 extern STR setbou495125082;
 extern STR name54;
 extern STR name79;
 INT L461_br;
 INT rL461_;
 INT L47;
 ARRAYINT L48;
 INT L491_;
 BOOL L501_;
 AM_CALL_ARG L511_;
 AM_CALL_ARG L52;
 dAM_EXPR L53;
 AM_CALL_ARG L541_;
 AM_CALL_ARG L55;
 extern STR bound_arg;
 BOOL L56;
 BOOL L571_;
 extern STR name36;
 STR L58;
 INT L59;
 OB L60;
 INT L61;
 ARRAYARG L63;
 INT L641_;
 INT L65;
 ARG L661_;
 ARG L67;
 AM_CALL_ARG L681_;
 AM_CALL_ARG L69;
 dAM_EXPR L70;
 AM_CALL_ARG L711_;
 AM_CALL_ARG L72;
 extern STR bound_arg;
 BOOL L73;
 BOOL L741_;
 extern STR name36;
 STR L75;
 INT L76;
 OB L77;
 INT L78;
 INT L801_;
 extern STR name133;
 FSTR L81;
 dSTR L82;
 BOOL L83;
 BOOL L841_;
 extern STR name24;
 extern STR name23;
 binding_iter_clo = FALSE;
 ident1 = IDENT_1150413730(IDENT_zero, ((STR) &callb));
 is_eq_self = ATTR(ATTR(arg,fun),name1);
 is_eq_i = ident1;
 ret_val1 = (is_eq_self.str==is_eq_i.str);
 if (ret_val1) {
  binding_iter_clo = TRUE;
 }
 SATTR(self,bnd_iter_creates,FLISTA80425519(ATTR(self,bnd_iter_creates), arg));
 uses_bnd_iter_cr = ATTR(self,code_c);
 uses_bnd_iter_cr1 = arg;
 SATTR(uses_bnd_iter_cr,bnd_iter_creates,FLISTA80425519(ATTR(uses_bnd_iter_cr,bnd_iter_creates), uses_bnd_iter_cr1));
 L31_=!(binding_iter_clo); 
 if (L31_) {
  CODE_F1291349628(ATTR(self,code_c), ATTR(arg,fun));
 }
 uses_iter_self = ATTR(self,code_c);
 uses_iter_s = ATTR(arg,fun);
 SATTR(uses_iter_self,iters,FSETSI671175003(ATTR(uses_iter_self,iters), uses_iter_s));
 L4 = ATTR(ATTR(arg,fun),tp);
 if ((*dTP_is1811059018[TAG(L4)])(L4)) {
  L5 = ATTR(self,code_c);
  CODE_F525364269(L5, ((dLAYOUT) ABSTRA1493033801(((ABSTRA151498155) NULL), ATTR(arg,fun), ATTR(self,prog))));
 }
 else {
  if (binding_iter_clo) {
  }
  else {
   a = FMAPSI2016001247(CODE_F508943357, ATTR(arg,fun));
   if ((a==((AM_ROUT_DEF) NULL))) {
    a = FMAPSI2016001247(ATTR(self,itersig_map), ATTR(arg,fun));
    if ((a==((AM_ROUT_DEF) NULL))) {
     barf_self = self;
     barf_msg = ((STR) &Itersi1831213836);
     barf_self1 = ATTR(barf_self,prog);
     barf_msg1 = barf_msg;
     barf_at_self = barf_self1;
     barf_at_msg = barf_msg1;
     barf_at_at = ((dPROG_ERR) NULL);
     PROG_e176405615(barf_at_self, barf_at_at);
     plus_self = ((STR) &Intern28965746);
     plus_sarg = barf_at_msg;
     ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
     PROG_err_STR(barf_at_self, ret_val2);
     exit_self = ((UNIX) NULL);
     exit_code = 1;
     exit(exit_code);
    }
   }
   SATTR(self,itersig_map,FMAPSI396914288(ATTR(self,itersig_map), ATTR(arg,fun), a));
   L6 = ATTR(self,code_c);
   CODE_F525364269(L6, ((dLAYOUT) FRAME_248172754(((FRAME_LAYOUT) NULL), a, ATTR(self,prog))));
  }
 }
 L7 = ATTR(self,code_c);
 CODE_F525364269(L7, ((dLAYOUT) BOUND_11324444(((BOUND_1007407331) NULL), arg, ATTR(self,prog))));
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
  L9=ZALLOC(sizeof(struct NAMESPACE_struct));
  if (L9==NULL) FATAL("Unable to allocate more memory");
  ((OB)L9)->header.tag=NAMESPACE_tag;
  L8 = ((NAMESPACE) L9);
  r1 = L8;
  create_self1 = ((NAMEMAP) NULL);
  ret_val6 = ((NAMEMAP) NULL);
  SATTR(r1,map,ret_val6);
  create_self2 = ((FSETSTR) NULL);
  ret_val7 = ((FSETSTR) NULL);
  SATTR(r1,set,ret_val7);
  SATTR(r1,counter,0);
  create_self3 = ((FMAPSTRINT) NULL);
  ret_val8 = ((FMAPSTRINT) NULL);
  SATTR(r1,uniques,ret_val8);
  ret_val5 = r1;
  r = ret_val5;
  SATTR(space_self,namespaces,FMAPdO1199466663(ATTR(space_self,namespaces), space_ns, r));
 }
 ret_val4 = r;
 name111 = NAMESP180237388(ret_val4, ((STR) &L));
 SATTR(genlocal_self,used_by_local,FSETST1404644833(ATTR(genlocal_self,used_by_local), name111));
 ret_val3 = name111;
 l_res = ret_val3;
 plus_self1 = ATTR(self,code_c);
 plus_s = ((dSTR) CGEN_eolrSTR(self));
 L10 = (plus_s==((dSTR) NULL));
 L121_=!(L10); 
 if (L121_) {
  L13 = ATTR(plus_self1,ntext);
  L14 = plus_s;
  SATTR(plus_self1,ntext,FSTR_p1752847026(L13, (*dSTR_strrSTR[TAG(L14)])(L14)));
 }
 ret_val9 = plus_self1;
 plus_self2 = ret_val9;
 L16=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L16==NULL) FATAL("Unable to allocate more memory");
 memset(L16,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L16)->header.tag=CHAR_tag;
 L15 = (dSTR)((CHAR_boxed) L16);
 ((CHAR_boxed) L15)->immutable_part = ' ';
 plus_s1 = L15;
 L17 = (plus_s1==((dSTR) NULL));
 L181_=!(L17); 
 if (L181_) {
  L19 = ATTR(plus_self2,ntext);
  L20 = plus_s1;
  SATTR(plus_self2,ntext,FSTR_p1752847026(L19, (*dSTR_strrSTR[TAG(L20)])(L20)));
 }
 ret_val10 = plus_self2;
 plus_self3 = ret_val10;
 mang_self = self;
 mang_ob = ((OB) arg);
 ret_val11 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
 plus_s2 = ((dSTR) ret_val11);
 L22 = (plus_s2==((dSTR) NULL));
 L231_=!(L22); 
 if (L231_) {
  L24 = ATTR(plus_self3,ntext);
  L25 = plus_s2;
  SATTR(plus_self3,ntext,FSTR_p1752847026(L24, (*dSTR_strrSTR[TAG(L25)])(L25)));
 }
 ret_val12 = plus_self3;
 plus_self4 = ret_val12;
 plus_s3 = ((dSTR) ((STR) &S_iter_ob));
 L26 = (plus_s3==((dSTR) NULL));
 L271_=!(L26); 
 if (L271_) {
  L28 = ATTR(plus_self4,ntext);
  L29 = plus_s3;
  SATTR(plus_self4,ntext,FSTR_p1752847026(L28, (*dSTR_strrSTR[TAG(L29)])(L29)));
 }
 ret_val13 = plus_self4;
 plus_self5 = ret_val13;
 plus_s4 = ((dSTR) l_res);
 L30 = (plus_s4==((dSTR) NULL));
 L321_=!(L30); 
 if (L321_) {
  L33 = ATTR(plus_self5,ntext);
  L34 = plus_s4;
  SATTR(plus_self5,ntext,FSTR_p1752847026(L33, (*dSTR_strrSTR[TAG(L34)])(L34)));
 }
 ret_val14 = plus_self5;
 plus_self6 = ret_val14;
 L36=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L36==NULL) FATAL("Unable to allocate more memory");
 memset(L36,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L36)->header.tag=CHAR_tag;
 L35 = (dSTR)((CHAR_boxed) L36);
 ((CHAR_boxed) L35)->immutable_part = ';';
 plus_s5 = L35;
 L37 = ATTR(plus_self6,ntext);
 L38 = plus_s5;
 SATTR(plus_self6,ntext,FSTR_p1752847026(L37, (*dSTR_strrSTR[TAG(L38)])(L38)));
 ndefer_self = self;
 ndefer_s = ((STR) &name10);
 CGEN_d695831906(ndefer_self);
 SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
 ndefer_self1 = self;
 plus_self7 = CGEN_eolrSTR(self);
 plus_sarg1 = l_res;
 ret_val15 = STR_ap2004550586(plus_self7, plus_sarg1);
 plus_self8 = ret_val15;
 plus_sarg2 = ((STR) &OBALLOC1);
 ret_val16 = STR_ap2004550586(plus_self8, plus_sarg2);
 plus_self9 = ret_val16;
 mang_self1 = self;
 mang_ob1 = ((OB) arg);
 ret_val17 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
 plus_sarg3 = ret_val17;
 ret_val18 = STR_ap2004550586(plus_self9, plus_sarg3);
 plus_self10 = ret_val18;
 plus_sarg4 = ((STR) &S_iter_ob1);
 ret_val19 = STR_ap2004550586(plus_self10, plus_sarg4);
 ndefer_s1 = ret_val19;
 CGEN_d695831906(ndefer_self1);
 SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
 ndefer_self2 = self;
 plus_self11 = l_res;
 plus_sarg5 = ((STR) &headertag1);
 ret_val20 = STR_ap2004550586(plus_self11, plus_sarg5);
 plus_self12 = ret_val20;
 tag_for_self = self;
 tp_self = arg;
 ret_val21 = ATTR(tp_self,tp_at);
 tag_for_tp = ret_val21;
 SATTR(tag_for_self,needs_tag,FSETdT1016814448(ATTR(tag_for_self,needs_tag), tag_for_tp));
 mang_self2 = tag_for_self;
 mang_ob2 = ((OB) tag_for_tp);
 ret_val23 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, ((OB) NULL));
 res = STR_ap2004550586(ret_val23, ((STR) &S_tag));
 forbid_self = tag_for_self;
 forbid_s = res;
 forbid_self1 = ATTR(forbid_self,mangler);
 forbid_s1 = forbid_s;
 SATTR(forbid_self1,forbidden,FSETST1404644833(ATTR(forbid_self1,forbidden), forbid_s1));
 ret_val22 = res;
 plus_sarg6 = ret_val22;
 ret_val24 = STR_ap2004550586(plus_self12, plus_sarg6);
 plus_self13 = ret_val24;
 plus_arg = ';';
 str_self = plus_arg;
 create_self4 = ((STR) NULL);
 create_c = str_self;
 L40 = 1;
 L42=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L40)*sizeof(CHAR);
 L41=ZALLOC_LEAF_BIG(L42);
 if (L41==NULL) FATAL("Unable to allocate more memory");
 memset(L41,0,L42);
 ((OB)L41)->header.tag=STR_tag;
#ifdef DESTROY_CHK

   ((OB)L41)->header.destroyed=0;
#endif

 L39 = ((STR) L41);
 L39->asize = L40;
 r2 = L39;
 SARR((STR)r2,0,create_c); 
 ;
 ret_val27 = r2;
 ret_val26 = ret_val27;
 s = ret_val26;
 ret_val25 = STR_ap1077157958(plus_self13, s, TRUE);
 ndefer_s2 = ret_val25;
 CGEN_d695831906(ndefer_self2);
 SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
 ndefer_self3 = self;
 plus_self14 = l_res;
 plus_sarg7 = ((STR) &sizesizeofstruct);
 ret_val28 = STR_ap2004550586(plus_self14, plus_sarg7);
 plus_self15 = ret_val28;
 mang_self3 = self;
 mang_ob3 = ((OB) arg);
 ret_val29 = MANGLE119219986(ATTR(mang_self3,mangler), mang_ob3, ((OB) NULL));
 plus_sarg8 = ret_val29;
 ret_val30 = STR_ap2004550586(plus_self15, plus_sarg8);
 plus_self16 = ret_val30;
 plus_sarg9 = ((STR) &S_iter_ob_struct);
 ret_val31 = STR_ap2004550586(plus_self16, plus_sarg9);
 ndefer_s3 = ret_val31;
 CGEN_d695831906(ndefer_self3);
 SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
 ndefer_self4 = self;
 plus_self17 = l_res;
 plus_sarg10 = ((STR) &state01);
 ret_val32 = STR_ap2004550586(plus_self17, plus_sarg10);
 plus_self18 = ret_val32;
 plus_sarg11 = ((STR) &initializestate);
 ret_val33 = STR_ap2004550586(plus_self18, plus_sarg11);
 ndefer_s4 = ret_val33;
 CGEN_d695831906(ndefer_self4);
 SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
 ndefer_self5 = self;
 plus_self19 = l_res;
 plus_sarg12 = ((STR) &call3);
 ret_val34 = STR_ap2004550586(plus_self19, plus_sarg12);
 ndefer_s5 = ret_val34;
 CGEN_d695831906(ndefer_self5);
 SATTR(ndefer_self5,routine_code,FSTR_p1752847026(ATTR(ndefer_self5,routine_code), ndefer_s5));
 L44 = (ATTR(ATTR(arg,fun),ret)==((dTP) NULL));
 L451_=!(L44); 
 if (L451_) {
  defer_self = self;
  plus_self20 = ((STR) &name24);
  mang_self4 = self;
  mang_ob4 = ((OB) ATTR(ATTR(arg,fun),ret));
  mang_ns = ((OB) NULL);
  ret_val35 = MANGLE119219986(ATTR(mang_self4,mangler), mang_ob4, mang_ns);
  plus_sarg13 = ret_val35;
  ret_val36 = STR_ap2004550586(plus_self20, plus_sarg13);
  plus_self21 = ret_val36;
  plus_sarg14 = ((STR) &name132);
  ret_val37 = STR_ap2004550586(plus_self21, plus_sarg14);
  defer_s = ret_val37;
  SATTR(defer_self,routine_code,FSTR_p1752847026(ATTR(defer_self,routine_code), defer_s));
 }
 else {
  defer_self1 = self;
  defer_s1 = ((STR) &void8);
  SATTR(defer_self1,routine_code,FSTR_p1752847026(ATTR(defer_self1,routine_code), defer_s1));
 }
 defer_self2 = self;
 mang_self5 = self;
 mang_ob5 = ((OB) arg);
 ret_val38 = MANGLE119219986(ATTR(mang_self5,mangler), mang_ob5, ((OB) NULL));
 plus_self22 = ret_val38;
 plus_sarg15 = ((STR) &S_iter_ob2);
 ret_val39 = STR_ap2004550586(plus_self22, plus_sarg15);
 defer_s2 = ret_val39;
 SATTR(defer_self2,routine_code,FSTR_p1752847026(ATTR(defer_self2,routine_code), defer_s2));
 if (ATTR(self,func_tables)) {
  defer_self3 = self;
  defer_s3 = ((STR) &struct139248200);
  SATTR(defer_self3,routine_code,FSTR_p1752847026(ATTR(defer_self3,routine_code), defer_s3));
 }
 defer_self4 = self;
 plus_self23 = ((STR) &name87);
 mang_self6 = self;
 mang_ob6 = ((OB) arg);
 ret_val40 = MANGLE119219986(ATTR(mang_self6,mangler), mang_ob6, ((OB) NULL));
 plus_sarg16 = ret_val40;
 ret_val41 = STR_ap2004550586(plus_self23, plus_sarg16);
 plus_self24 = ret_val41;
 plus_sarg17 = ((STR) &S_call_function);
 ret_val42 = STR_ap2004550586(plus_self24, plus_sarg17);
 defer_s4 = ret_val42;
 SATTR(defer_self4,routine_code,FSTR_p1752847026(ATTR(defer_self4,routine_code), defer_s4));
 comment_self = self;
 comment_com = ((STR) &setbou495125082);
 if (ATTR(comment_self,pretty)) {
  ndefer_self6 = comment_self;
  plus_self25 = ((STR) &name54);
  plus_sarg18 = comment_com;
  ret_val43 = STR_ap2004550586(plus_self25, plus_sarg18);
  plus_self26 = ret_val43;
  plus_sarg19 = ((STR) &name79);
  ret_val44 = STR_ap2004550586(plus_self26, plus_sarg19);
  ndefer_s6 = ret_val44;
  CGEN_d695831906(ndefer_self6);
  SATTR(ndefer_self6,routine_code,FSTR_p1752847026(ATTR(ndefer_self6,routine_code), ndefer_s6));
 }
 {
  BOOL f_L461_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = arg;
  L461_br=L11==NULL?0:ASIZE((AM_BND1124877163)L11); 
  while (1) {
   if(L21>=L461_br)  goto after_loop; 
   rL461_=L21; 
   i = rL461_;
   L48 = ATTR(arg,bnd_args);
   L491_=ARR((ARRAYINT)L48,i); 
   idx = L491_;
   L501_=(idx)==(0); 
   if (L501_) {
    cast_self = self;
    cast_dest_tp = ATTR(ATTR(arg,fun),tp);
    L511_=(AM_CALL_ARG)ARR((AM_BND1124877163)arg,i); 
    L52=L511_;
    L53 = ATTR(L52,expr);
    cast_src_tp = (*dAM_EXPR_tprdTP[TAG(L53)])(L53);
    L541_=(AM_CALL_ARG)ARR((AM_BND1124877163)arg,i); 
    L55=L541_;
    cast_expr = CGEN_e1639504572(self, ATTR(L55,expr));
    cast_only_boxing = FALSE;
    ret_val45 = CGEN_d1915593494(cast_self, cast_dest_tp, cast_src_tp, cast_expr, cast_only_boxing, ((dMODE) NULL));
    entry1 = ret_val45;
    ndefer_self7 = self;
    plus_self27 = l_res;
    plus_sarg20 = ((STR) &bound_arg);
    ret_val46 = STR_ap2004550586(plus_self27, plus_sarg20);
    plus_self28 = ret_val46;
    plus_arg1 = i;
    str_self1 = plus_arg1;
    clear_self = buf1;
    L56 = (clear_self==((FSTR) NULL));
    L571_=!(L56); 
    if (L571_) {
     SATTR(clear_self,loc,0);
    }
    str_in_self = str_self1;
    str_in_s = buf1;
    ret_val49 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
    buf1 = ret_val49;
    str_self2 = buf1;
    ret_val50 = STR_fr1097270334(((STR) NULL), str_self2);
    ret_val48 = ret_val50;
    s1 = ret_val48;
    ret_val47 = STR_ap1077157958(plus_self28, s1, TRUE);
    plus_self29 = ret_val47;
    plus_sarg21 = ((STR) &name36);
    ret_val51 = STR_ap2004550586(plus_self29, plus_sarg21);
    plus_self30 = ret_val51;
    plus_sarg22 = entry1;
    ret_val52 = STR_ap2004550586(plus_self30, plus_sarg22);
    plus_self31 = ret_val52;
    plus_arg2 = ';';
    str_self3 = plus_arg2;
    create_self5 = ((STR) NULL);
    create_c1 = str_self3;
    L59 = 1;
    L61=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L59)*sizeof(CHAR);
    L60=ZALLOC_LEAF_BIG(L61);
    if (L60==NULL) FATAL("Unable to allocate more memory");
    memset(L60,0,L61);
    ((OB)L60)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L60)->header.destroyed=0;
#endif

    L58 = ((STR) L60);
    L58->asize = L59;
    r3 = L58;
    SARR((STR)r3,0,create_c1); 
    ;
    ret_val55 = r3;
    ret_val54 = ret_val55;
    s2 = ret_val54;
    ret_val53 = STR_ap1077157958(plus_self31, s2, TRUE);
    ndefer_s7 = ret_val53;
    CGEN_d695831906(ndefer_self7);
    SATTR(ndefer_self7,routine_code,FSTR_p1752847026(ATTR(ndefer_self7,routine_code), ndefer_s7));
   }
   else {
    cast_self1 = self;
    L63 = ATTR(ATTR(arg,fun),args);
    L641_=INTMINUS(idx,1); 
    L65 = L641_;
    L661_=(ARG)ARR((ARRAYARG)L63,L65); 
    L67=L661_;
    cast_dest_tp1 = ATTR(L67,tp);
    L681_=(AM_CALL_ARG)ARR((AM_BND1124877163)arg,i); 
    L69=L681_;
    L70 = ATTR(L69,expr);
    cast_src_tp1 = (*dAM_EXPR_tprdTP[TAG(L70)])(L70);
    L711_=(AM_CALL_ARG)ARR((AM_BND1124877163)arg,i); 
    L72=L711_;
    cast_expr1 = CGEN_e1639504572(self, ATTR(L72,expr));
    cast_only_boxing1 = FALSE;
    ret_val56 = CGEN_d1915593494(cast_self1, cast_dest_tp1, cast_src_tp1, cast_expr1, cast_only_boxing1, ((dMODE) NULL));
    entry1 = ret_val56;
    ndefer_self8 = self;
    plus_self32 = l_res;
    plus_sarg23 = ((STR) &bound_arg);
    ret_val57 = STR_ap2004550586(plus_self32, plus_sarg23);
    plus_self33 = ret_val57;
    plus_arg3 = i;
    str_self4 = plus_arg3;
    clear_self1 = buf2;
    L73 = (clear_self1==((FSTR) NULL));
    L741_=!(L73); 
    if (L741_) {
     SATTR(clear_self1,loc,0);
    }
    str_in_self1 = str_self4;
    str_in_s1 = buf2;
    ret_val60 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
    buf2 = ret_val60;
    str_self5 = buf2;
    ret_val61 = STR_fr1097270334(((STR) NULL), str_self5);
    ret_val59 = ret_val61;
    s3 = ret_val59;
    ret_val58 = STR_ap1077157958(plus_self33, s3, TRUE);
    plus_self34 = ret_val58;
    plus_sarg24 = ((STR) &name36);
    ret_val62 = STR_ap2004550586(plus_self34, plus_sarg24);
    plus_self35 = ret_val62;
    plus_sarg25 = entry1;
    ret_val63 = STR_ap2004550586(plus_self35, plus_sarg25);
    plus_self36 = ret_val63;
    plus_arg4 = ';';
    str_self6 = plus_arg4;
    create_self6 = ((STR) NULL);
    create_c2 = str_self6;
    L76 = 1;
    L78=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L76)*sizeof(CHAR);
    L77=ZALLOC_LEAF_BIG(L78);
    if (L77==NULL) FATAL("Unable to allocate more memory");
    memset(L77,0,L78);
    ((OB)L77)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L77)->header.destroyed=0;
#endif

    L75 = ((STR) L77);
    L75->asize = L76;
    r4 = L75;
    SARR((STR)r4,0,create_c2); 
    ;
    ret_val66 = r4;
    ret_val65 = ret_val66;
    s4 = ret_val65;
    ret_val64 = STR_ap1077157958(plus_self36, s4, TRUE);
    ndefer_s8 = ret_val64;
    CGEN_d695831906(ndefer_self8);
    SATTR(ndefer_self8,routine_code,FSTR_p1752847026(ATTR(ndefer_self8,routine_code), ndefer_s8));
   }
   L801_=INTPLUS(L21,1); 
   L21 = L801_;
  }
 }
 after_loop: ;
 plus_self37 = ATTR(self,code_c);
 plus_s6 = ((dSTR) ((STR) &name133));
 L81 = ATTR(plus_self37,ntext);
 L82 = plus_s6;
 SATTR(plus_self37,ntext,FSTR_p1752847026(L81, (*dSTR_strrSTR[TAG(L82)])(L82)));
 make_sure_emitte = self;
 make_sure_emitte1 = ATTR(arg,fun);
 ard = FMAPSI2016001247(ATTR(make_sure_emitte,not_emitted), make_sure_emitte1);
 L83 = (ard==((AM_ROUT_DEF) NULL));
 L841_=!(L83); 
 if (L841_) {
  SATTR(make_sure_emitte,leftovers,FSETAM2044524049(ATTR(make_sure_emitte,leftovers), ard));
  SATTR(make_sure_emitte,not_emitted,FMAPSI1625125906(ATTR(make_sure_emitte,not_emitted), make_sure_emitte1));
 }
 plus_self38 = ((STR) &name24);
 mang_self7 = self;
 tp_self1 = arg;
 ret_val67 = ATTR(tp_self1,tp_at);
 mang_ob7 = ((OB) ret_val67);
 ret_val68 = MANGLE119219986(ATTR(mang_self7,mangler), mang_ob7, ((OB) NULL));
 plus_sarg26 = ret_val68;
 ret_val69 = STR_ap2004550586(plus_self38, plus_sarg26);
 plus_self39 = ret_val69;
 plus_sarg27 = ((STR) &name23);
 ret_val70 = STR_ap2004550586(plus_self39, plus_sarg27);
 plus_self40 = ret_val70;
 plus_sarg28 = l_res;
 ret_val71 = STR_ap2004550586(plus_self40, plus_sarg28);
 ret_val = ret_val71;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e1345827062(CGEN self, AM_ARRAY_EXPR arg) {
 STR ret_val;
 dTP t;
 dTP t2;
 STR res;
 AM_ARRAY_EXPR L11;
 INT i = INT_zero;
 STR rhs;
 CGEN dec_local_commen;
 dTP dec_local_commen1;
 STR dec_local_commen2;
 STR ret_val1;
 STR res1;
 CGEN comment_self;
 CODE_FILE comment_f;
 STR comment_com;
 CODE_FILE plus_self;
 dSTR plus_s;
 CODE_FILE ret_val2;
 CODE_FILE plus_self1;
 dSTR plus_s1;
 CODE_FILE ret_val3;
 CODE_FILE plus_self2;
 dSTR plus_s2;
 INT str_self = INT_zero;
 STR ret_val4;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val5;
 FSTR str_self1;
 STR ret_val6;
 CGEN ndefer_self;
 STR ndefer_s;
 INT str_self2 = INT_zero;
 STR ret_val7;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val8;
 FSTR str_self3;
 STR ret_val9;
 CGEN ndefer_self1;
 STR ndefer_s1;
 INT L21 = INT_zero;
 CGEN cast_self;
 dTP cast_dest_tp;
 dTP cast_src_tp;
 STR cast_expr;
 BOOL cast_only_boxing = BOOL_zero;
 STR ret_val10;
 INT str_self4 = INT_zero;
 STR ret_val11;
 FSTR buf3 = ((FSTR) NULL);
 FSTR clear_self2;
 INT str_in_self2 = INT_zero;
 FSTR str_in_s2;
 FSTR ret_val12;
 FSTR str_self5;
 STR ret_val13;
 CGEN ndefer_self2;
 STR ndefer_s2;
 AM_OB_DEF L3;
 extern STR localf930677623;
 extern STR name78;
 BOOL L4;
 BOOL L51_;
 FSTR L6;
 dSTR L7;
 BOOL L8;
 BOOL L91_;
 FSTR L10;
 dSTR L12;
 extern STR name79;
 FSTR L13;
 dSTR L14;
 extern STR name36;
 INT L151_;
 BOOL L16;
 BOOL L171_;
 STR L18;
 extern STR name16;
 extern STR asize1;
 INT L191_;
 BOOL L20;
 BOOL L221_;
 extern STR name16;
 INT L231_br;
 INT rL231_;
 INT L24;
 dAM_EXPR L251_;
 dAM_EXPR L26;
 dAM_EXPR L271_;
 extern STR arr_part3;
 BOOL L28;
 BOOL L291_;
 extern STR name129;
 extern STR name16;
 INT L301_;
 t = ATTR(arg,tp_at);
 L3=CGEN_a1221397173(self, t);
 t2 = ATTR(L3,arr);
 dec_local_commen = self;
 dec_local_commen1 = t;
 dec_local_commen2 = ((STR) &localf930677623);
 res1 = CGEN_d1425664957(dec_local_commen, dec_local_commen1);
 comment_self = dec_local_commen;
 comment_f = ATTR(dec_local_commen,code_c);
 comment_com = dec_local_commen2;
 if (ATTR(comment_self,pretty)) {
  plus_self = comment_f;
  plus_s = ((dSTR) ((STR) &name78));
  L4 = (plus_s==((dSTR) NULL));
  L51_=!(L4); 
  if (L51_) {
   L6 = ATTR(plus_self,ntext);
   L7 = plus_s;
   SATTR(plus_self,ntext,FSTR_p1752847026(L6, (*dSTR_strrSTR[TAG(L7)])(L7)));
  }
  ret_val2 = plus_self;
  plus_self1 = ret_val2;
  plus_s1 = ((dSTR) comment_com);
  L8 = (plus_s1==((dSTR) NULL));
  L91_=!(L8); 
  if (L91_) {
   L10 = ATTR(plus_self1,ntext);
   L12 = plus_s1;
   SATTR(plus_self1,ntext,FSTR_p1752847026(L10, (*dSTR_strrSTR[TAG(L12)])(L12)));
  }
  ret_val3 = plus_self1;
  plus_self2 = ret_val3;
  plus_s2 = ((dSTR) ((STR) &name79));
  L13 = ATTR(plus_self2,ntext);
  L14 = plus_s2;
  SATTR(plus_self2,ntext,FSTR_p1752847026(L13, (*dSTR_strrSTR[TAG(L14)])(L14)));
 }
 ret_val1 = res1;
 res = ret_val1;
 ndefer_self = self;
 L151_=ASIZE((AM_ARRAY_EXPR)arg); 
 str_self = L151_;
 clear_self = buf1;
 L16 = (clear_self==((FSTR) NULL));
 L171_=!(L16); 
 if (L171_) {
  SATTR(clear_self,loc,0);
 }
 str_in_self = str_self;
 str_in_s = buf1;
 ret_val5 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
 buf1 = ret_val5;
 str_self1 = buf1;
 ret_val6 = STR_fr1097270334(((STR) NULL), str_self1);
 ret_val4 = ret_val6;
 L18 = CGEN_a59983474(self, t, ret_val4);
 ndefer_s = STR_ap1417971546(res, ((STR) &name36), L18, ((STR) &name16));
 CGEN_d695831906(ndefer_self);
 SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
 ndefer_self1 = self;
 L191_=ASIZE((AM_ARRAY_EXPR)arg); 
 str_self2 = L191_;
 clear_self1 = buf2;
 L20 = (clear_self1==((FSTR) NULL));
 L221_=!(L20); 
 if (L221_) {
  SATTR(clear_self1,loc,0);
 }
 str_in_self1 = str_self2;
 str_in_s1 = buf2;
 ret_val8 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
 buf2 = ret_val8;
 str_self3 = buf2;
 ret_val9 = STR_fr1097270334(((STR) NULL), str_self3);
 ret_val7 = ret_val9;
 ndefer_s1 = STR_ap1417971546(res, ((STR) &asize1), ret_val7, ((STR) &name16));
 CGEN_d695831906(ndefer_self1);
 SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
 {
  BOOL f_L231_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = arg;
  L231_br=L11==NULL?0:ASIZE((AM_ARRAY_EXPR)L11); 
  while (1) {
   if(L21>=L231_br)  goto after_loop; 
   rL231_=L21; 
   i = rL231_;
   cast_self = self;
   cast_dest_tp = t2;
   L251_=(dAM_EXPR)ARR((AM_ARRAY_EXPR)arg,i); 
   L26 = L251_;
   cast_src_tp = (*dAM_EXPR_tprdTP[TAG(L26)])(L26);
   L271_=(dAM_EXPR)ARR((AM_ARRAY_EXPR)arg,i); 
   cast_expr = CGEN_e1639504572(self, L271_);
   cast_only_boxing = FALSE;
   ret_val10 = CGEN_d1915593494(cast_self, cast_dest_tp, cast_src_tp, cast_expr, cast_only_boxing, ((dMODE) NULL));
   rhs = ret_val10;
   ndefer_self2 = self;
   str_self4 = i;
   clear_self2 = buf3;
   L28 = (clear_self2==((FSTR) NULL));
   L291_=!(L28); 
   if (L291_) {
    SATTR(clear_self2,loc,0);
   }
   str_in_self2 = str_self4;
   str_in_s2 = buf3;
   ret_val12 = INT_st367594495(str_in_self2, str_in_s2, 0, 10, ' ');
   buf3 = ret_val12;
   str_self5 = buf3;
   ret_val13 = STR_fr1097270334(((STR) NULL), str_self5);
   ret_val11 = ret_val13;
   ndefer_s2 = STR_ap1925291910(res, ((STR) &arr_part3), ret_val11, ((STR) &name129), rhs, ((STR) &name16));
   CGEN_d695831906(ndefer_self2);
   SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
   L301_=INTPLUS(L21,1); 
   L21 = L301_;
  }
 }
 after_loop: ;
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e1445279685(CGEN self, AM_STMT_EXPR arg) {
 STR ret_val;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 L1 = (ATTR(arg,stmts)==((dAM_STMT) NULL));
 L21_=!(L1); 
 if (L21_) {
  CGEN_e494460490(self, ATTR(arg,stmts));
 }
 L3 = (ATTR(arg,expr)==((dAM_EXPR) NULL));
 L41_=!(L3); 
 if (L41_) {
  ret_val = CGEN_e1639504572(self, ATTR(arg,expr));
  return ret_val;
 }
 else {
  ret_val = ((STR) NULL);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e1471104673(CGEN self, AM_EXT_CALL_EXPR ext_call) {
 STR ret_val;
 STR res_arg = ((STR) NULL);
 STR res = ((STR) NULL);
 BOOL has_return = BOOL_zero;
 ARRAYSTR arg_list = ((ARRAYSTR) NULL);
 ARRAYSTR final_arg_list = ((ARRAYSTR) NULL);
 ARRAYSTR type_list = ((ARRAYSTR) NULL);
 INT first_arg = INT_zero;
 STR tmp_res = ((STR) NULL);
 BOOL need_copy = BOOL_zero;
 BOOL alternate_return = BOOL_zero;
 STR extern11;
 INT arg_pos = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 STR alt_ret = ((STR) NULL);
 STR tmp_ret;
 AM_EXT_CALL_EXPR tp_self;
 dTP ret_val1;
 AM_EXT_CALL_EXPR tp_self1;
 dTP ret_val2;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val3;
 CGEN bind_fortran_nam;
 STR bind_fortran_nam1;
 STR ret_val4;
 STR res1 = ((STR) NULL);
 BIND_FORTRAN bind_name_self;
 STR bind_name_name;
 STR ret_val5;
 STR res2 = ((STR) NULL);
 BIND_FORTRAN append_underscor;
 STR append_underscor1;
 STR ret_val6;
 STR plus_self;
 STR plus_sarg;
 STR ret_val7;
 AM_EXT_CALL_EXPR tp_self2;
 dTP ret_val8;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val9;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val10;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val11;
 AM_EXT_CALL_EXPR ext_tp_self;
 dTP ret_val12;
 CODE_FILE uses_extern_self;
 STR uses_extern_dec;
 CGEN dec_local_commen;
 dTP dec_local_commen1;
 STR dec_local_commen2;
 STR ret_val13;
 STR res3;
 CGEN comment_self;
 CODE_FILE comment_f;
 STR comment_com;
 CODE_FILE plus_self4;
 dSTR plus_s;
 CODE_FILE ret_val14;
 CODE_FILE plus_self5;
 dSTR plus_s1;
 CODE_FILE ret_val15;
 CODE_FILE plus_self6;
 dSTR plus_s2;
 AM_EXT_CALL_EXPR tp_self3;
 dTP ret_val16;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN ndefer_self1;
 STR ndefer_s1;
 STR plus_self7;
 STR plus_sarg4;
 STR ret_val17;
 CGEN ndefer_self2;
 STR ndefer_s2;
 STR plus_self8;
 STR plus_sarg5;
 STR ret_val18;
 CGEN ndefer_self3;
 STR ndefer_s3;
 CGEN ndefer_self4;
 STR ndefer_s4;
 ARRAYSTR* L3;
 ARRAYSTR* L4;
 INT* L5;
 BOOL L6;
 BOOL L7;
 BOOL L81_;
 extern STR extern2;
 extern STR name10;
 extern STR int1;
 extern STR void6;
 extern STR S_;
 extern STR name24;
 extern STR name24;
 BOOL L91_;
 extern STR name92;
 STR L101_;
 STR L121_;
 STR L131_;
 INT L141_;
 INT L15;
 BOOL L161_;
 extern STR name3;
 extern STR name3;
 BOOL L171_;
 STR L181_;
 STR L191_;
 INT L201_;
 INT L22;
 BOOL L231_;
 extern STR name3;
 extern STR name3;
 INT L251_;
 INT L26;
 INT L271_;
 INT L241_,L241_m;
 INT L28;
 AM_CALL_ARG L291_;
 AM_CALL_ARG L30;
 dAM_EXPR L31;
 extern STR name92;
 STR L321_;
 STR L331_;
 INT L341_;
 INT L35;
 INT L361_;
 INT L37;
 BOOL L381_;
 extern STR name29;
 extern STR name29;
 INT L391_;
 INT L401_;
 INT L41;
 BOOL L421_;
 STR L431_;
 STR L441_;
 INT L451_;
 INT L46;
 INT L471_;
 INT L48;
 BOOL L491_;
 extern STR name29;
 extern STR name29;
 INT L501_;
 extern STR name127;
 extern STR name26;
 dTP L51;
 BOOL L52;
 BOOL L531_;
 extern STR Tempor1361444261;
 extern STR name78;
 BOOL L54;
 BOOL L551_;
 FSTR L56;
 dSTR L57;
 BOOL L58;
 BOOL L591_;
 FSTR L60;
 dSTR L61;
 extern STR name79;
 FSTR L62;
 dSTR L63;
 extern STR name33;
 extern STR name16;
 extern STR name33;
 extern STR name16;
 extern STR name16;
 BOOL L64;
 BOOL L651_;
 BOOL L661_;
 BOOL L67;
 BOOL L681_;
 extern STR name16;
 extern STR name33;
 extern STR name16;
 L3 = &arg_list;
 L4 = &type_list;
 L5 = &first_arg;
 res_arg = CGEN_e601674892(self, ext_call, L3, L4, L5);
 tp_self = ext_call;
 ret_val1 = ATTR(ATTR(tp_self,fun),ret);
 L7 = (ret_val1==((dTP) NULL));
 L81_=!(L7); 
 if (L81_) {
  L6 = (res_arg==((STR) NULL));
 } else {
  L6 = FALSE;
 }
 has_return = L6;
 need_copy = CGEN_n1296275061(self, ((dAM_CALL_EXPR) ext_call));
 alternate_return = CGEN_i1231884161(self, ext_call);
 if (need_copy) {
  final_arg_list = CGEN_f618693208(self, ext_call, first_arg, arg_list);
 }
 else {
  final_arg_list = arg_list;
 }
 extern11 = ((STR) &extern2);
 if (has_return) {
  mang_self = self;
  tp_self1 = ext_call;
  ret_val2 = ATTR(ATTR(tp_self1,fun),ret);
  mang_ob = ((OB) ret_val2);
  ret_val3 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
  extern11 = STR_ap1693864410(extern11, ret_val3, ((STR) &name10));
 }
 else {
  if (alternate_return) {
   extern11 = STR_ap2004550586(extern11, ((STR) &int1));
  }
  else {
   extern11 = STR_ap2004550586(extern11, ((STR) &void6));
  }
 }
 bind_fortran_nam = self;
 bind_fortran_nam1 = ATTR(ext_call,nm).str;
 res1 = STR_ap1693864410(ATTR(bind_fortran_nam,fortran_prepend_), bind_fortran_nam1, ATTR(bind_fortran_nam,fortran_append_u));
 if (ATTR(bind_fortran_nam,fortran_bind_fun)) {
  bind_name_self = ((BIND_FORTRAN) NULL);
  bind_name_name = res1;
  append_underscor = bind_name_self;
  append_underscor1 = bind_name_name;
  plus_self = append_underscor1;
  plus_sarg = ((STR) &S_);
  ret_val7 = STR_ap2004550586(plus_self, plus_sarg);
  ret_val6 = ret_val7;
  res2 = ret_val6;
  ret_val5 = res2;
  res1 = ret_val5;
 }
 ret_val4 = res1;
 res = ret_val4;
 extern11 = STR_ap1693864410(extern11, res, ((STR) &name24));
 res = STR_ap2004550586(res, ((STR) &name24));
 L91_=(0)<(first_arg); 
 if (L91_) {
  tp_self2 = ext_call;
  ret_val8 = ATTR(ATTR(tp_self2,fun),ret);
  if (CGEN_i1538413090(self, ret_val8)) {
   L101_=(STR)ARR((ARRAYSTR)final_arg_list,0); 
   res = STR_ap1693864410(res, ((STR) &name92), L101_);
  }
  else {
   L121_=(STR)ARR((ARRAYSTR)final_arg_list,0); 
   res = STR_ap2004550586(res, L121_);
  }
  L131_=(STR)ARR((ARRAYSTR)type_list,0); 
  extern11 = STR_ap2004550586(extern11, L131_);
  L141_=(final_arg_list)==NULL?0:ASIZE((ARRAYSTR)final_arg_list); 
  L15 = L141_;
  L161_=(1)<(L15); 
  if (L161_) {
   plus_self1 = res;
   plus_sarg1 = ((STR) &name3);
   ret_val9 = STR_ap2004550586(plus_self1, plus_sarg1);
   res = ret_val9;
   extern11 = STR_ap2004550586(extern11, ((STR) &name3));
  }
  L171_=(1)<(first_arg); 
  if (L171_) {
   L181_=(STR)ARR((ARRAYSTR)final_arg_list,1); 
   res = STR_ap2004550586(res, L181_);
   L191_=(STR)ARR((ARRAYSTR)type_list,1); 
   extern11 = STR_ap2004550586(extern11, L191_);
   L201_=(final_arg_list)==NULL?0:ASIZE((ARRAYSTR)final_arg_list); 
   L22 = L201_;
   L231_=(2)<(L22); 
   if (L231_) {
    plus_self2 = res;
    plus_sarg2 = ((STR) &name3);
    ret_val10 = STR_ap2004550586(plus_self2, plus_sarg2);
    res = ret_val10;
    extern11 = STR_ap2004550586(extern11, ((STR) &name3));
   }
  }
 }
 arg_pos = first_arg;
 {
  BOOL f_L241_ = TRUE;
  L11 = 1;
  L251_=(ext_call)==NULL?0:ASIZE((AM_EXT_CALL_EXPR)ext_call); 
  L26 = L251_;
  L271_=INTMINUS(L26,1); 
  L21 = L271_;
  L241_=L11-1;L241_m=L21; 
  while (1) {
   if(L241_++>=L241_m)  goto after_loop; 
   i = L241_;
   L291_=(AM_CALL_ARG)ARR((AM_EXT_CALL_EXPR)ext_call,i); 
   L30=L291_;
   L31 = ATTR(L30,expr);
   if (CGEN_i1538413090(self, (*dAM_EXPR_tprdTP[TAG(L31)])(L31))) {
    plus_self3 = res;
    plus_sarg3 = ((STR) &name92);
    ret_val11 = STR_ap2004550586(plus_self3, plus_sarg3);
    res = ret_val11;
   }
   L321_=(STR)ARR((ARRAYSTR)final_arg_list,arg_pos); 
   res = STR_ap2004550586(res, L321_);
   L331_=(STR)ARR((ARRAYSTR)type_list,arg_pos); 
   extern11 = STR_ap2004550586(extern11, L331_);
   L341_=(final_arg_list)==NULL?0:ASIZE((ARRAYSTR)final_arg_list); 
   L35 = L341_;
   L361_=INTMINUS(L35,1); 
   L37 = L361_;
   L381_=(arg_pos)<(L37); 
   if (L381_) {
    extern11 = STR_ap2004550586(extern11, ((STR) &name29));
    res = STR_ap2004550586(res, ((STR) &name29));
   }
   L391_=INTPLUS(arg_pos,1); 
   arg_pos = L391_;
  }
 }
 after_loop: ;
 while (1) {
  L401_=(final_arg_list)==NULL?0:ASIZE((ARRAYSTR)final_arg_list); 
  L41 = L401_;
  L421_=(arg_pos)<(L41); 
  if (L421_) {
  }
  else {
   goto after_loop1;
  }
  L431_=(STR)ARR((ARRAYSTR)final_arg_list,arg_pos); 
  res = STR_ap2004550586(res, L431_);
  L441_=(STR)ARR((ARRAYSTR)type_list,arg_pos); 
  extern11 = STR_ap2004550586(extern11, L441_);
  L451_=(final_arg_list)==NULL?0:ASIZE((ARRAYSTR)final_arg_list); 
  L46 = L451_;
  L471_=INTMINUS(L46,1); 
  L48 = L471_;
  L491_=(arg_pos)<(L48); 
  if (L491_) {
   extern11 = STR_ap2004550586(extern11, ((STR) &name29));
   res = STR_ap2004550586(res, ((STR) &name29));
  }
  L501_=INTPLUS(arg_pos,1); 
  arg_pos = L501_;
 }
 after_loop1: ;
 extern11 = STR_ap2004550586(extern11, ((STR) &name127));
 res = STR_ap2004550586(res, ((STR) &name26));
 ext_tp_self = ext_call;
 ret_val12 = ATTR(ATTR(ext_tp_self,fun),tp);
 L51 = ret_val12;
 L52 = (*dTP_is914808564[TAG(L51)])(L51);
 L531_=!(L52); 
 if (L531_) {
  uses_extern_self = ATTR(self,code_c);
  uses_extern_dec = extern11;
  SATTR(uses_extern_self,externs1,FSETST1404644833(ATTR(uses_extern_self,externs1), uses_extern_dec));
 }
 if (alternate_return) {
  dec_local_commen = self;
  dec_local_commen1 = ((dTP) TP_BUILTIN_int);
  dec_local_commen2 = ((STR) &Tempor1361444261);
  res3 = CGEN_d1425664957(dec_local_commen, dec_local_commen1);
  comment_self = dec_local_commen;
  comment_f = ATTR(dec_local_commen,code_c);
  comment_com = dec_local_commen2;
  if (ATTR(comment_self,pretty)) {
   plus_self4 = comment_f;
   plus_s = ((dSTR) ((STR) &name78));
   L54 = (plus_s==((dSTR) NULL));
   L551_=!(L54); 
   if (L551_) {
    L56 = ATTR(plus_self4,ntext);
    L57 = plus_s;
    SATTR(plus_self4,ntext,FSTR_p1752847026(L56, (*dSTR_strrSTR[TAG(L57)])(L57)));
   }
   ret_val14 = plus_self4;
   plus_self5 = ret_val14;
   plus_s1 = ((dSTR) comment_com);
   L58 = (plus_s1==((dSTR) NULL));
   L591_=!(L58); 
   if (L591_) {
    L60 = ATTR(plus_self5,ntext);
    L61 = plus_s1;
    SATTR(plus_self5,ntext,FSTR_p1752847026(L60, (*dSTR_strrSTR[TAG(L61)])(L61)));
   }
   ret_val15 = plus_self5;
   plus_self6 = ret_val15;
   plus_s2 = ((dSTR) ((STR) &name79));
   L62 = ATTR(plus_self6,ntext);
   L63 = plus_s2;
   SATTR(plus_self6,ntext,FSTR_p1752847026(L62, (*dSTR_strrSTR[TAG(L63)])(L63)));
  }
  ret_val13 = res3;
  alt_ret = ret_val13;
 }
 if (need_copy) {
  if (has_return) {
   tp_self3 = ext_call;
   ret_val16 = ATTR(ATTR(tp_self3,fun),ret);
   tmp_ret = CGEN_d1425664957(self, ret_val16);
   ndefer_self = self;
   ndefer_s = STR_ap1417971546(tmp_ret, ((STR) &name33), res, ((STR) &name16));
   CGEN_d695831906(ndefer_self);
   SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
   res = tmp_ret;
  }
  else {
   if (alternate_return) {
    ndefer_self1 = self;
    ndefer_s1 = STR_ap1417971546(alt_ret, ((STR) &name33), res, ((STR) &name16));
    CGEN_d695831906(ndefer_self1);
    SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
   }
   else {
    ndefer_self2 = self;
    plus_self7 = res;
    plus_sarg4 = ((STR) &name16);
    ret_val17 = STR_ap2004550586(plus_self7, plus_sarg4);
    ndefer_s2 = ret_val17;
    CGEN_d695831906(ndefer_self2);
    SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
   }
   L64 = (ATTR(ATTR(ext_call,fun),ret)==((dTP) NULL));
   L651_=!(L64); 
   if (L651_) {
    res = res_arg;
   }
   else {
    res = ((STR) NULL);
   }
  }
  CGEN_f412018626(self, ext_call, first_arg, arg_list, final_arg_list);
 }
 else {
  L661_=!(has_return); 
  if (L661_) {
   L67 = (ATTR(ATTR(ext_call,fun),ret)==((dTP) NULL));
   L681_=!(L67); 
   if (L681_) {
    ndefer_self3 = self;
    plus_self8 = res;
    plus_sarg5 = ((STR) &name16);
    ret_val18 = STR_ap2004550586(plus_self8, plus_sarg5);
    ndefer_s3 = ret_val18;
    CGEN_d695831906(ndefer_self3);
    SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
    res = res_arg;
   }
   else {
    if (alternate_return) {
     ndefer_self4 = self;
     ndefer_s4 = STR_ap1417971546(alt_ret, ((STR) &name33), res, ((STR) &name16));
     CGEN_d695831906(ndefer_self4);
     SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
     res = ((STR) NULL);
    }
   }
  }
 }
 if (alternate_return) {
  CGEN_p535532953(self, ext_call, final_arg_list, first_arg, alt_ret);
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e1650375497(CGEN self, AM_VAT319982528 arg) {
 STR ret_val;
 STR s1;
 STR s2;
 STR s3;
 STR res;
 CGEN mang_self;
 OB mang_ob;
 OB mang_ns;
 STR ret_val1;
 CGEN cast_self;
 dTP cast_dest_tp;
 dTP cast_src_tp;
 STR cast_expr;
 BOOL cast_only_boxing = BOOL_zero;
 STR ret_val2;
 AM_VAT319982528 tp_self;
 dTP ret_val3;
 CGEN dec_local_commen;
 dTP dec_local_commen1;
 STR dec_local_commen2;
 STR ret_val4;
 STR res1;
 CGEN comment_self;
 CODE_FILE comment_f;
 STR comment_com;
 CODE_FILE plus_self;
 dSTR plus_s;
 CODE_FILE ret_val5;
 CODE_FILE plus_self1;
 dSTR plus_s1;
 CODE_FILE ret_val6;
 CODE_FILE plus_self2;
 dSTR plus_s2;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN ndefer_self1;
 STR ndefer_s1;
 OB L1;
 OB L2;
 dAM_EXPR L3;
 dAM_EXPR L4;
 dAM_EXPR L5;
 extern STR localf217231907;
 extern STR name78;
 BOOL L6;
 BOOL L71_;
 FSTR L8;
 dSTR L9;
 BOOL L10;
 BOOL L111_;
 FSTR L12;
 dSTR L13;
 extern STR name79;
 FSTR L14;
 dSTR L15;
 extern STR name36;
 extern STR name16;
 extern STR name18;
 extern STR name36;
 extern STR name16;
 s1 = CGEN_e1639504572(self, ATTR(arg,ob));
 mang_self = self;
 L2=ZALLOC(sizeof(struct IDENT_boxed_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=IDENT_tag;
 L1 = (OB)((IDENT_boxed) L2);
 ((IDENT_boxed) L1)->immutable_part = ATTR(arg,at);
 mang_ob = L1;
 L3 = ATTR(arg,ob);
 mang_ns = ((OB) (*dAM_EXPR_tprdTP[TAG(L3)])(L3));
 ret_val1 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, mang_ns);
 s2 = ret_val1;
 cast_self = self;
 cast_dest_tp = ATTR(arg,real_tp);
 L4 = ATTR(arg,val1);
 cast_src_tp = (*dAM_EXPR_tprdTP[TAG(L4)])(L4);
 cast_expr = CGEN_e1639504572(self, ATTR(arg,val1));
 cast_only_boxing = FALSE;
 ret_val2 = CGEN_d1915593494(cast_self, cast_dest_tp, cast_src_tp, cast_expr, cast_only_boxing, ((dMODE) NULL));
 s3 = ret_val2;
 dec_local_commen = self;
 tp_self = arg;
 L5 = ATTR(tp_self,ob);
 ret_val3 = (*dAM_EXPR_tprdTP[TAG(L5)])(L5);
 dec_local_commen1 = ret_val3;
 dec_local_commen2 = ((STR) &localf217231907);
 res1 = CGEN_d1425664957(dec_local_commen, dec_local_commen1);
 comment_self = dec_local_commen;
 comment_f = ATTR(dec_local_commen,code_c);
 comment_com = dec_local_commen2;
 if (ATTR(comment_self,pretty)) {
  plus_self = comment_f;
  plus_s = ((dSTR) ((STR) &name78));
  L6 = (plus_s==((dSTR) NULL));
  L71_=!(L6); 
  if (L71_) {
   L8 = ATTR(plus_self,ntext);
   L9 = plus_s;
   SATTR(plus_self,ntext,FSTR_p1752847026(L8, (*dSTR_strrSTR[TAG(L9)])(L9)));
  }
  ret_val5 = plus_self;
  plus_self1 = ret_val5;
  plus_s1 = ((dSTR) comment_com);
  L10 = (plus_s1==((dSTR) NULL));
  L111_=!(L10); 
  if (L111_) {
   L12 = ATTR(plus_self1,ntext);
   L13 = plus_s1;
   SATTR(plus_self1,ntext,FSTR_p1752847026(L12, (*dSTR_strrSTR[TAG(L13)])(L13)));
  }
  ret_val6 = plus_self1;
  plus_self2 = ret_val6;
  plus_s2 = ((dSTR) ((STR) &name79));
  L14 = ATTR(plus_self2,ntext);
  L15 = plus_s2;
  SATTR(plus_self2,ntext,FSTR_p1752847026(L14, (*dSTR_strrSTR[TAG(L15)])(L15)));
 }
 ret_val4 = res1;
 res = ret_val4;
 ndefer_self = self;
 ndefer_s = STR_ap1417971546(res, ((STR) &name36), s1, ((STR) &name16));
 CGEN_d695831906(ndefer_self);
 SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
 ndefer_self1 = self;
 ndefer_s1 = STR_ap1925291910(res, ((STR) &name18), s2, ((STR) &name36), s3, ((STR) &name16));
 CGEN_d695831906(ndefer_self1);
 SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e1818527256(CGEN self, AM_BND367211769 arg) {
 STR ret_val;
 TP_ITER tp;
 STR bi1;
 STR s1 = ((STR) NULL);
 STR res = ((STR) NULL);
 STR biter_name;
 AM_BND367211769 L11;
 INT i = INT_zero;
 STR arg_str;
 STR pro;
 AM_BND367211769 tp_self;
 dTP ret_val1;
 AM_BND367211769 tp_self1;
 dTP ret_val2;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 CGEN dec_local_commen;
 dTP dec_local_commen1;
 STR dec_local_commen2;
 STR ret_val4;
 STR res1;
 CGEN comment_self;
 CODE_FILE comment_f;
 STR comment_com;
 CODE_FILE plus_self1;
 dSTR plus_s;
 CODE_FILE ret_val5;
 CODE_FILE plus_self2;
 dSTR plus_s1;
 CODE_FILE ret_val6;
 CODE_FILE plus_self3;
 dSTR plus_s2;
 CGEN mang_self;
 OB mang_ob;
 OB mang_ns;
 STR ret_val7;
 STR plus_self4;
 STR plus_sarg1;
 STR ret_val8;
 CGEN barf_self;
 STR barf_msg;
 PROG barf_self1;
 STR barf_msg1;
 PROG barf_at_self;
 STR barf_at_msg;
 dPROG_ERR barf_at_at;
 STR plus_self5;
 STR plus_sarg2;
 STR ret_val9;
 UNIX exit_self;
 INT exit_code = INT_zero;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN in_self;
 CGEN move_out_self;
 CGEN ndefer_self1;
 STR ndefer_s1;
 INT L21 = INT_zero;
 CGEN comment_self1;
 STR comment_com1;
 STR plus_self6;
 STR plus_sarg3;
 STR ret_val10;
 STR plus_self7;
 STR plus_sarg4;
 STR ret_val11;
 CGEN ndefer_self2;
 STR ndefer_s2;
 INT str_self = INT_zero;
 STR ret_val12;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val13;
 FSTR str_self1;
 STR ret_val14;
 CGEN ndefer_self3;
 STR ndefer_s3;
 INT str_self2 = INT_zero;
 STR ret_val15;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val16;
 FSTR str_self3;
 STR ret_val17;
 CGEN cast_arg_self;
 dTP cast_arg_dest_tp;
 dTP cast_arg_src_tp;
 STR cast_arg_expr;
 BOOL cast_arg_only_bo = BOOL_zero;
 dMODE cast_arg_mode;
 STR ret_val18;
 CGEN ndefer_self4;
 STR ndefer_s4;
 AM_BND367211769 tp_self2;
 dTP ret_val19;
 STR plus_self8;
 STR plus_sarg5;
 STR ret_val20;
 STR plus_self9;
 STR plus_sarg6;
 STR ret_val21;
 STR plus_self10;
 STR plus_sarg7;
 STR ret_val22;
 STR plus_self11;
 STR plus_sarg8;
 STR ret_val23;
 CGEN ndefer_self5;
 STR ndefer_s5;
 STR plus_self12;
 STR plus_sarg9;
 STR ret_val24;
 STR plus_self13;
 STR plus_sarg10;
 STR ret_val25;
 STR plus_self14;
 STR plus_sarg11;
 STR ret_val26;
 CGEN ndefer_self6;
 STR ndefer_s6;
 CGEN ndefer_self7;
 STR ndefer_s7;
 CGEN in_self1;
 CGEN ndefer_self8;
 STR ndefer_s8;
 CGEN move_out_self1;
 CGEN ndefer_self9;
 STR ndefer_s9;
 CGEN pop_exceptions_s;
 INT pop_exceptions_n = INT_zero;
 INT L31 = INT_zero;
 CGEN ndefer_self10;
 STR ndefer_s10;
 INT str_self4 = INT_zero;
 STR ret_val27;
 FSTR buf3 = ((FSTR) NULL);
 FSTR clear_self2;
 INT str_in_self2 = INT_zero;
 FSTR str_in_s2;
 FSTR ret_val28;
 FSTR str_self5;
 STR ret_val29;
 CGEN ndefer_self11;
 STR ndefer_s11;
 BOOL L4;
 BOOL L51_;
 extern STR Holdsr2142202862;
 extern STR name78;
 BOOL L6;
 BOOL L71_;
 FSTR L8;
 dSTR L9;
 BOOL L10;
 BOOL L121_;
 FSTR L13;
 dSTR L14;
 extern STR name79;
 FSTR L15;
 dSTR L16;
 extern STR Unreco1110335422;
 extern STR Intern28965746;
 extern STR if3;
 extern STR state0;
 INT L17;
 INT L181_;
 INT L19;
 INT L201_;
 extern STR name4;
 BOOL L22;
 BOOL L231_;
 INT L241_br;
 INT rL241_;
 INT L25;
 ARRAYBOOL L26;
 BOOL L271_;
 extern STR hotargument;
 extern STR name54;
 extern STR name79;
 AM_CALL_ARG L281_;
 AM_CALL_ARG L29;
 ARRAYARG L30;
 ARG L321_;
 ARG L33;
 BOOL L34;
 BOOL L35;
 ARRAYARG L36;
 ARG L371_;
 ARG L38;
 dTP L39;
 AM_CALL_ARG L401_;
 AM_CALL_ARG L41;
 AM_CALL_ARG L421_;
 AM_CALL_ARG L43;
 dAM_EXPR L44;
 dTP L45;
 extern STR hotarg;
 BOOL L46;
 BOOL L471_;
 extern STR name36;
 extern STR name16;
 extern STR hotarg;
 BOOL L48;
 BOOL L491_;
 extern STR name36;
 ARRAYARG L50;
 ARG L521_;
 ARG L53;
 AM_CALL_ARG L541_;
 AM_CALL_ARG L55;
 dAM_EXPR L56;
 AM_CALL_ARG L571_;
 AM_CALL_ARG L58;
 extern STR name16;
 INT L591_;
 extern STR name1;
 extern STR FF2;
 BOOL L60;
 BOOL L611_;
 extern STR name135;
 extern STR callvoid;
 extern STR name76;
 extern STR name134;
 extern STR callvoid;
 extern STR name76;
 extern STR Noretu151888568;
 extern STR if3;
 extern STR state1;
 INT L62;
 INT L631_;
 extern STR goto1;
 extern STR name16;
 INT L64;
 INT L651_;
 extern STR name4;
 INT L66;
 INT L67;
 INT L681_;
 extern STR POP_EXCEPTION12;
 extern STR POP_EXCEPTION11;
 BOOL L69;
 BOOL L701_;
 extern STR name76;
 tp = ATTR(arg,bi_tp);
 bi1 = CGEN_d1425664957(self, ((dTP) tp));
 tp_self = arg;
 ret_val1 = ATTR(ATTR(tp_self,bi_tp),ret);
 L4 = (ret_val1==((dTP) NULL));
 L51_=!(L4); 
 if (L51_) {
  dec_local_commen = self;
  tp_self1 = arg;
  ret_val2 = ATTR(ATTR(tp_self1,bi_tp),ret);
  dec_local_commen1 = ret_val2;
  plus_self = ((STR) &Holdsr2142202862);
  plus_sarg = TP_ITER_strrSTR(ATTR(arg,bi_tp));
  ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
  dec_local_commen2 = ret_val3;
  res1 = CGEN_d1425664957(dec_local_commen, dec_local_commen1);
  comment_self = dec_local_commen;
  comment_f = ATTR(dec_local_commen,code_c);
  comment_com = dec_local_commen2;
  if (ATTR(comment_self,pretty)) {
   plus_self1 = comment_f;
   plus_s = ((dSTR) ((STR) &name78));
   L6 = (plus_s==((dSTR) NULL));
   L71_=!(L6); 
   if (L71_) {
    L8 = ATTR(plus_self1,ntext);
    L9 = plus_s;
    SATTR(plus_self1,ntext,FSTR_p1752847026(L8, (*dSTR_strrSTR[TAG(L9)])(L9)));
   }
   ret_val5 = plus_self1;
   plus_self2 = ret_val5;
   plus_s1 = ((dSTR) comment_com);
   L10 = (plus_s1==((dSTR) NULL));
   L121_=!(L10); 
   if (L121_) {
    L13 = ATTR(plus_self2,ntext);
    L14 = plus_s1;
    SATTR(plus_self2,ntext,FSTR_p1752847026(L13, (*dSTR_strrSTR[TAG(L14)])(L14)));
   }
   ret_val6 = plus_self2;
   plus_self3 = ret_val6;
   plus_s2 = ((dSTR) ((STR) &name79));
   L15 = ATTR(plus_self3,ntext);
   L16 = plus_s2;
   SATTR(plus_self3,ntext,FSTR_p1752847026(L15, (*dSTR_strrSTR[TAG(L16)])(L16)));
  }
  ret_val4 = res1;
  s1 = ret_val4;
 }
 mang_self = self;
 mang_ob = ((OB) arg);
 mang_ns = ((OB) ATTR(self,current_sig));
 ret_val7 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, mang_ns);
 biter_name = ret_val7;
 if ((biter_name==((STR) NULL))) {
  barf_self = self;
  plus_self4 = ((STR) &Unreco1110335422);
  plus_sarg1 = CGEN_e1639504572(self, ATTR(arg,bi));
  ret_val8 = STR_ap2004550586(plus_self4, plus_sarg1);
  barf_msg = ret_val8;
  barf_self1 = ATTR(barf_self,prog);
  barf_msg1 = barf_msg;
  barf_at_self = barf_self1;
  barf_at_msg = barf_msg1;
  barf_at_at = ((dPROG_ERR) NULL);
  PROG_e176405615(barf_at_self, barf_at_at);
  plus_self5 = ((STR) &Intern28965746);
  plus_sarg2 = barf_at_msg;
  ret_val9 = STR_ap2004550586(plus_self5, plus_sarg2);
  PROG_err_STR(barf_at_self, ret_val9);
  exit_self = ((UNIX) NULL);
  exit_code = 1;
  exit(exit_code);
 }
 ndefer_self = self;
 ndefer_s = STR_ap1693864410(((STR) &if3), biter_name, ((STR) &state0));
 CGEN_d695831906(ndefer_self);
 SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
 in_self = self;
 L17 = ATTR(in_self,indent);
 L181_=INTPLUS(L17,1); 
 SATTR(in_self,indent,L181_);
 CGEN_e986815307(self, arg);
 move_out_self = self;
 L19 = ATTR(move_out_self,indent);
 L201_=INTMINUS(L19,1); 
 SATTR(move_out_self,indent,L201_);
 ndefer_self1 = self;
 ndefer_s1 = ((STR) &name4);
 CGEN_d695831906(ndefer_self1);
 SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
 L22 = (ATTR(ATTR(arg,bi_tp),hot)==((ARRAYBOOL) NULL));
 L231_=!(L22); 
 if (L231_) {
  {
   BOOL f_L241_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = arg;
   L241_br=L11==NULL?0:ASIZE((AM_BND367211769)L11); 
   while (1) {
    if(L21>=L241_br)  goto after_loop; 
    rL241_=L21; 
    i = rL241_;
    L26 = ATTR(ATTR(arg,bi_tp),hot);
    L271_=ARR((ARRAYBOOL)L26,i); 
    if (L271_) {
     comment_self1 = self;
     comment_com1 = ((STR) &hotargument);
     if (ATTR(comment_self1,pretty)) {
      ndefer_self2 = comment_self1;
      plus_self6 = ((STR) &name54);
      plus_sarg3 = comment_com1;
      ret_val10 = STR_ap2004550586(plus_self6, plus_sarg3);
      plus_self7 = ret_val10;
      plus_sarg4 = ((STR) &name79);
      ret_val11 = STR_ap2004550586(plus_self7, plus_sarg4);
      ndefer_s2 = ret_val11;
      CGEN_d695831906(ndefer_self2);
      SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
     }
     L281_=(AM_CALL_ARG)ARR((AM_BND367211769)arg,i); 
     L29 = L281_;
     L30 = ATTR(tp,args);
     L321_=(ARG)ARR((ARRAYARG)L30,i); 
     L33 = L321_;
     arg_str = CGEN_e414796278(self, L29, L33, FALSE, FALSE);
     L36 = ATTR(tp,args);
     L371_=(ARG)ARR((ARRAYARG)L36,i); 
     L38=L371_;
     L39 = ATTR(L38,tp);
     if ((*dTP_is1334578382[TAG(L39)])(L39)) {
      L401_=(AM_CALL_ARG)ARR((AM_BND367211769)arg,i); 
      L41=L401_;
      L35 = SYSOBEQ(((OB) ATTR(L41,mode1)),((OB) MODES_out_mode));
     } else {
      L35 = FALSE;
     }
     if (L35) {
      L421_=(AM_CALL_ARG)ARR((AM_BND367211769)arg,i); 
      L43=L421_;
      L44 = ATTR(L43,expr);
      L45 = (*dAM_EXPR_tprdTP[TAG(L44)])(L44);
      L34 = (*dTP_is1811059018[TAG(L45)])(L45);
     } else {
      L34 = FALSE;
     }
     if (L34) {
      ndefer_self3 = self;
      str_self = i;
      clear_self = buf1;
      L46 = (clear_self==((FSTR) NULL));
      L471_=!(L46); 
      if (L471_) {
       SATTR(clear_self,loc,0);
      }
      str_in_self = str_self;
      str_in_s = buf1;
      ret_val13 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
      buf1 = ret_val13;
      str_self1 = buf1;
      ret_val14 = STR_fr1097270334(((STR) NULL), str_self1);
      ret_val12 = ret_val14;
      ndefer_s3 = STR_ap1925291910(biter_name, ((STR) &hotarg), ret_val12, ((STR) &name36), arg_str, ((STR) &name16));
      CGEN_d695831906(ndefer_self3);
      SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
     }
     else {
      ndefer_self4 = self;
      str_self2 = i;
      clear_self1 = buf2;
      L48 = (clear_self1==((FSTR) NULL));
      L491_=!(L48); 
      if (L491_) {
       SATTR(clear_self1,loc,0);
      }
      str_in_self1 = str_self2;
      str_in_s1 = buf2;
      ret_val16 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
      buf2 = ret_val16;
      str_self3 = buf2;
      ret_val17 = STR_fr1097270334(((STR) NULL), str_self3);
      ret_val15 = ret_val17;
      cast_arg_self = self;
      L50 = ATTR(tp,args);
      L521_=(ARG)ARR((ARRAYARG)L50,i); 
      L53=L521_;
      cast_arg_dest_tp = ATTR(L53,tp);
      L541_=(AM_CALL_ARG)ARR((AM_BND367211769)arg,i); 
      L55=L541_;
      L56 = ATTR(L55,expr);
      cast_arg_src_tp = (*dAM_EXPR_tprdTP[TAG(L56)])(L56);
      cast_arg_expr = arg_str;
      cast_arg_only_bo = FALSE;
      L571_=(AM_CALL_ARG)ARR((AM_BND367211769)arg,i); 
      L58=L571_;
      cast_arg_mode = ATTR(L58,mode1);
      ret_val18 = CGEN_d1915593494(cast_arg_self, cast_arg_dest_tp, cast_arg_src_tp, cast_arg_expr, cast_arg_only_bo, cast_arg_mode);
      ndefer_s4 = STR_ap1925291910(biter_name, ((STR) &hotarg), ret_val15, ((STR) &name36), ret_val18, ((STR) &name16));
      CGEN_d695831906(ndefer_self4);
      SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
     }
    }
    L591_=INTPLUS(L21,1); 
    L21 = L591_;
   }
  }
  after_loop: ;
 }
 pro = ((STR) &name1);
 if (ATTR(self,func_tables)) {
  pro = ((STR) &FF2);
 }
 tp_self2 = arg;
 ret_val19 = ATTR(ATTR(tp_self2,bi_tp),ret);
 L60 = (ret_val19==((dTP) NULL));
 L611_=!(L60); 
 if (L611_) {
  ndefer_self5 = self;
  plus_self9 = s1;
  plus_self8 = ((STR) &callvoid);
  plus_sarg5 = biter_name;
  ret_val20 = STR_ap2004550586(plus_self8, plus_sarg5);
  plus_sarg6 = STR_ap1693864410(((STR) &name135), biter_name, ret_val20);
  ret_val21 = STR_ap2004550586(plus_self9, plus_sarg6);
  plus_self10 = ret_val21;
  plus_sarg7 = pro;
  ret_val22 = STR_ap2004550586(plus_self10, plus_sarg7);
  plus_self11 = ret_val22;
  plus_sarg8 = ((STR) &name76);
  ret_val23 = STR_ap2004550586(plus_self11, plus_sarg8);
  ndefer_s5 = ret_val23;
  CGEN_d695831906(ndefer_self5);
  SATTR(ndefer_self5,routine_code,FSTR_p1752847026(ATTR(ndefer_self5,routine_code), ndefer_s5));
  res = s1;
 }
 else {
  ndefer_self6 = self;
  plus_self12 = ((STR) &callvoid);
  plus_sarg9 = biter_name;
  ret_val24 = STR_ap2004550586(plus_self12, plus_sarg9);
  plus_self13 = STR_ap1693864410(((STR) &name134), biter_name, ret_val24);
  plus_sarg10 = pro;
  ret_val25 = STR_ap2004550586(plus_self13, plus_sarg10);
  plus_self14 = ret_val25;
  plus_sarg11 = ((STR) &name76);
  ret_val26 = STR_ap2004550586(plus_self14, plus_sarg11);
  ndefer_s6 = ret_val26;
  CGEN_d695831906(ndefer_self6);
  SATTR(ndefer_self6,routine_code,FSTR_p1752847026(ATTR(ndefer_self6,routine_code), ndefer_s6));
  res = ((STR) &Noretu151888568);
 }
 ndefer_self7 = self;
 ndefer_s7 = STR_ap1693864410(((STR) &if3), biter_name, ((STR) &state1));
 CGEN_d695831906(ndefer_self7);
 SATTR(ndefer_self7,routine_code,FSTR_p1752847026(ATTR(ndefer_self7,routine_code), ndefer_s7));
 in_self1 = self;
 L62 = ATTR(in_self1,indent);
 L631_=INTPLUS(L62,1); 
 SATTR(in_self1,indent,L631_);
 ndefer_self8 = self;
 ndefer_s8 = STR_ap1693864410(((STR) &goto1), ATTR(self,current_loop), ((STR) &name16));
 CGEN_d695831906(ndefer_self8);
 SATTR(ndefer_self8,routine_code,FSTR_p1752847026(ATTR(ndefer_self8,routine_code), ndefer_s8));
 move_out_self1 = self;
 L64 = ATTR(move_out_self1,indent);
 L651_=INTMINUS(L64,1); 
 SATTR(move_out_self1,indent,L651_);
 ndefer_self9 = self;
 ndefer_s9 = ((STR) &name4);
 CGEN_d695831906(ndefer_self9);
 SATTR(ndefer_self9,routine_code,FSTR_p1752847026(ATTR(ndefer_self9,routine_code), ndefer_s9));
 pop_exceptions_s = self;
 L66 = ATTR(self,ex_nesting);
 L67 = ATTR(self,current_loop_ex_);
 L681_=INTMINUS(L66,L67); 
 pop_exceptions_n = L681_;
 L31 = pop_exceptions_n;
 switch (L31) {
  case 0: 
   break;
  case 1: 
   ndefer_self10 = pop_exceptions_s;
   ndefer_s10 = ((STR) &POP_EXCEPTION12);
   CGEN_d695831906(ndefer_self10);
   SATTR(ndefer_self10,routine_code,FSTR_p1752847026(ATTR(ndefer_self10,routine_code), ndefer_s10));
   break;
  default: ;
   ndefer_self11 = pop_exceptions_s;
   str_self4 = pop_exceptions_n;
   clear_self2 = buf3;
   L69 = (clear_self2==((FSTR) NULL));
   L701_=!(L69); 
   if (L701_) {
    SATTR(clear_self2,loc,0);
   }
   str_in_self2 = str_self4;
   str_in_s2 = buf3;
   ret_val28 = INT_st367594495(str_in_self2, str_in_s2, 0, 10, ' ');
   buf3 = ret_val28;
   str_self5 = buf3;
   ret_val29 = STR_fr1097270334(((STR) NULL), str_self5);
   ret_val27 = ret_val29;
   ndefer_s11 = STR_ap1693864410(((STR) &POP_EXCEPTION11), ret_val27, ((STR) &name76));
   CGEN_d695831906(ndefer_self11);
   SATTR(ndefer_self11,routine_code,FSTR_p1752847026(ATTR(ndefer_self11,routine_code), ndefer_s11));
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e237424728(CGEN self, AM_BND260301329 arg) {
 STR ret_val;
 TP_ROUT tp;
 STR br1;
 STR res;
 ARRAYSTR arg_list;
 STR arg_strs = ((STR) NULL);
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 STR func_res;
 CGEN cast_self;
 dTP cast_dest_tp;
 dTP cast_src_tp;
 STR cast_expr;
 BOOL cast_only_boxing = BOOL_zero;
 STR ret_val1;
 CGEN ndefer_self;
 STR ndefer_s;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val3;
 AM_BND260301329 tp_self;
 dTP ret_val4;
 AM_BND260301329 tp_self1;
 dTP ret_val5;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val6;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val7;
 STR plus_self4;
 CHAR plus_arg = CHAR_zero;
 STR ret_val8;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val9;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val10;
 STR r;
 CGEN ndefer_self1;
 STR ndefer_s1;
 STR plus_self5;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val11;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val12;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val13;
 STR r1;
 CGEN ndefer_self2;
 STR ndefer_s2;
 extern STR name36;
 dAM_EXPR L3;
 extern STR name16;
 extern STR name134;
 extern STR funcptr2;
 INT L51_;
 INT L6;
 INT L71_;
 INT L41_,L41_m;
 INT L8;
 extern STR name3;
 STR L91_;
 BOOL L10;
 BOOL L121_;
 extern STR FF2;
 extern STR name26;
 BOOL L13;
 BOOL L141_;
 extern STR name36;
 STR L15;
 INT L16;
 OB L17;
 INT L18;
 STR L20;
 INT L22;
 OB L23;
 INT L24;
 tp = ATTR(arg,br_tp);
 br1 = CGEN_d1425664957(self, ((dTP) tp));
 ndefer_self = self;
 cast_self = self;
 cast_dest_tp = ((dTP) tp);
 L3 = ATTR(arg,br);
 cast_src_tp = (*dAM_EXPR_tprdTP[TAG(L3)])(L3);
 cast_expr = CGEN_e1639504572(self, ATTR(arg,br));
 cast_only_boxing = FALSE;
 ret_val1 = CGEN_d1915593494(cast_self, cast_dest_tp, cast_src_tp, cast_expr, cast_only_boxing, ((dMODE) NULL));
 ndefer_s = STR_ap1417971546(br1, ((STR) &name36), ret_val1, ((STR) &name16));
 CGEN_d695831906(ndefer_self);
 SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
 res = STR_ap1417971546(((STR) &name134), br1, ((STR) &funcptr2), br1);
 arg_list = CGEN_e1933193154(self, ((dAM_CALL_EXPR) arg));
 {
  BOOL f_L41_ = TRUE;
  L11 = 0;
  L51_=(arg_list)==NULL?0:ASIZE((ARRAYSTR)arg_list); 
  L6 = L51_;
  L71_=INTMINUS(L6,1); 
  L21 = L71_;
  L41_=L11-1;L41_m=L21; 
  while (1) {
   if(L41_++>=L41_m)  goto after_loop; 
   i = L41_;
   plus_self = arg_strs;
   plus_sarg = ((STR) &name3);
   ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val2;
   L91_=(STR)ARR((ARRAYSTR)arg_list,i); 
   plus_sarg1 = L91_;
   ret_val3 = STR_ap2004550586(plus_self1, plus_sarg1);
   arg_strs = ret_val3;
  }
 }
 after_loop: ;
 L10 = (arg_strs==((STR) NULL));
 L121_=!(L10); 
 if (L121_) {
  res = STR_ap2004550586(res, arg_strs);
 }
 if (ATTR(self,func_tables)) {
  res = STR_ap2004550586(res, ((STR) &FF2));
 }
 res = STR_ap2004550586(res, ((STR) &name26));
 if (CGEN_n1296275061(self, ((dAM_CALL_EXPR) arg))) {
  tp_self = arg;
  ret_val4 = ATTR(ATTR(tp_self,br_tp),ret);
  L13 = (ret_val4==((dTP) NULL));
  L141_=!(L13); 
  if (L141_) {
   tp_self1 = arg;
   ret_val5 = ATTR(ATTR(tp_self1,br_tp),ret);
   func_res = CGEN_d1425664957(self, ret_val5);
   ndefer_self1 = self;
   plus_self2 = func_res;
   plus_sarg2 = ((STR) &name36);
   ret_val6 = STR_ap2004550586(plus_self2, plus_sarg2);
   plus_self3 = ret_val6;
   plus_sarg3 = res;
   ret_val7 = STR_ap2004550586(plus_self3, plus_sarg3);
   plus_self4 = ret_val7;
   plus_arg = ';';
   str_self = plus_arg;
   create_self = ((STR) NULL);
   create_c = str_self;
   L16 = 1;
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
   SARR((STR)r,0,create_c); 
   ;
   ret_val10 = r;
   ret_val9 = ret_val10;
   s = ret_val9;
   ret_val8 = STR_ap1077157958(plus_self4, s, TRUE);
   ndefer_s1 = ret_val8;
   CGEN_d695831906(ndefer_self1);
   SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
   CGEN_c922285894(self, ((dAM_CALL_EXPR) arg), arg_list);
   ret_val = func_res;
   return ret_val;
  }
  else {
   ndefer_self2 = self;
   plus_self5 = res;
   plus_arg1 = ';';
   str_self1 = plus_arg1;
   create_self1 = ((STR) NULL);
   create_c1 = str_self1;
   L22 = 1;
   L24=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L22)*sizeof(CHAR);
   L23=ZALLOC_LEAF_BIG(L24);
   if (L23==NULL) FATAL("Unable to allocate more memory");
   memset(L23,0,L24);
   ((OB)L23)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L23)->header.destroyed=0;
#endif

   L20 = ((STR) L23);
   L20->asize = L22;
   r1 = L20;
   SARR((STR)r1,0,create_c1); 
   ;
   ret_val13 = r1;
   ret_val12 = ret_val13;
   s1 = ret_val12;
   ret_val11 = STR_ap1077157958(plus_self5, s1, TRUE);
   ndefer_s2 = ret_val11;
   CGEN_d695831906(ndefer_self2);
   SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
   CGEN_c922285894(self, ((dAM_CALL_EXPR) arg), arg_list);
   ret_val = ((STR) NULL);
   return ret_val;
  }
 }
 else {
  ret_val = res;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e601674892(CGEN self, AM_EXT_CALL_EXPR ext_call, ARRAYSTR* arg_list, ARRAYSTR* type_list, INT* first_arg) {
 ARRAYSTR L1 = ((ARRAYSTR) NULL);/*Local for arg*/
 ARRAYSTR L2 = ((ARRAYSTR) NULL);/*Local for arg*/
 INT L3 = INT_zero;/*Local for arg*/
 STR ret_val;
 STR res = ((STR) NULL);
 INT num_args = INT_zero;
 INT num_character_ar = INT_zero;
 INT arg_pos = INT_zero;
 dTP L41;
 STR tmp1;
 INT string_length_in = INT_zero;
 INT L51 = INT_zero;
 INT L61 = INT_zero;
 INT i = INT_zero;
 STR arg_str;
 dTP tp;
 dTP L71;
 ARRAYSTR create_self;
 INT create_n = INT_zero;
 ARRAYSTR ret_val1;
 ARRAYSTR create_self1;
 INT create_n1 = INT_zero;
 ARRAYSTR ret_val2;
 AM_EXT_CALL_EXPR tp_self;
 dTP ret_val3;
 AM_EXT_CALL_EXPR tp_self1;
 dTP ret_val4;
 AM_EXT_CALL_EXPR tp_self2;
 dTP ret_val5;
 AM_EXT_CALL_EXPR tp_self3;
 dTP ret_val6;
 AM_EXT_CALL_EXPR tp_self4;
 dTP ret_val7;
 AM_EXT_CALL_EXPR tp_self5;
 dTP ret_val8;
 AM_EXT_CALL_EXPR tp_self6;
 dTP ret_val9;
 AM_EXT_CALL_EXPR tp_self7;
 dTP ret_val10;
 AM_EXT_CALL_EXPR tp_self8;
 dTP ret_val11;
 STR plus_self;
 STR plus_sarg;
 STR ret_val12;
 CGEN ndefer_self;
 STR ndefer_s;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val13;
 CGEN ndefer_self1;
 STR ndefer_s1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val14;
 CGEN ndefer_self2;
 STR ndefer_s2;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val15;
 AM_EXT_CALL_EXPR tp_self9;
 dTP ret_val16;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val17;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val18;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val19;
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
 STR L20;
 STR L22;
 STR L241_;
 INT L251_;
 dTP L26;
 STR L27;
 STR L29;
 STR L311_;
 INT L321_;
 dTP L33;
 STR L34;
 STR L36;
 STR L381_;
 INT L391_;
 extern STR S1L;
 extern STR F_LENGTH1;
 INT L431_;
 dTP L44;
 extern STR ZALLOC946595004;
 extern STR sizeF_133617273;
 extern STR addres395294157;
 extern STR address2;
 STR L45;
 STR L47;
 INT L491_;
 extern STR size3;
 STR L50;
 extern STR F_LENGTH1;
 INT L541_;
 INT L551_;
 INT L561_;
 INT L57;
 BOOL L581_;
 INT L601_;
 INT L62;
 INT L631_;
 INT L591_,L591_m;
 INT L64;
 AM_CALL_ARG L651_;
 AM_CALL_ARG L661_;
 AM_CALL_ARG L67;
 dAM_EXPR L68;
 STR L69;
 dTP L72;
 extern STR S1L;
 extern STR F_LENGTH1;
 INT L761_;
 dTP L77;
 extern STR address2;
 STR L78;
 extern STR size3;
 STR L80;
 extern STR F_LENGTH1;
 INT L831_;
 INT L851_;
 L8 = &num_character_ar;
 num_args = CGEN_c1394504933(self, ext_call, L8);
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
 arg_pos = 0;
 tp_self = ext_call;
 ret_val3 = ATTR(ATTR(tp_self,fun),ret);
 L17 = (ret_val3==((dTP) NULL));
 L181_=!(L17); 
 if (L181_) {
  tp_self1 = ext_call;
  ret_val4 = ATTR(ATTR(tp_self1,fun),ret);
  L41 = ret_val4;
  L19 = L41;
  if ((*dTP_is242312711[TAG(L19)])(L19, ((OB) TP_BUI633637838))) {
   tp_self2 = ext_call;
   ret_val5 = ATTR(ATTR(tp_self2,fun),ret);
   L20 = CGEN_d933619848(self, ret_val5);
   SARR((ARRAYSTR)L1,arg_pos,(STR)L20); 
   ;
   tp_self3 = ext_call;
   ret_val6 = ATTR(ATTR(tp_self3,fun),ret);
   L22 = CGEN_f2084214268(self, ret_val6);
   SARR((ARRAYSTR)L2,arg_pos,(STR)L22); 
   ;
   L241_=(STR)ARR((ARRAYSTR)L1,arg_pos); 
   res = L241_;
   L251_=INTPLUS(arg_pos,1); 
   arg_pos = L251_;
  }
  else {
   L26 = L41;
   if ((*dTP_is242312711[TAG(L26)])(L26, ((OB) TP_BUI1032249067))) {
    tp_self4 = ext_call;
    ret_val7 = ATTR(ATTR(tp_self4,fun),ret);
    L27 = CGEN_d933619848(self, ret_val7);
    SARR((ARRAYSTR)L1,arg_pos,(STR)L27); 
    ;
    tp_self5 = ext_call;
    ret_val8 = ATTR(ATTR(tp_self5,fun),ret);
    L29 = CGEN_f2084214268(self, ret_val8);
    SARR((ARRAYSTR)L2,arg_pos,(STR)L29); 
    ;
    L311_=(STR)ARR((ARRAYSTR)L1,arg_pos); 
    res = L311_;
    L321_=INTPLUS(arg_pos,1); 
    arg_pos = L321_;
   }
   else {
    L33 = L41;
    if ((*dTP_is242312711[TAG(L33)])(L33, ((OB) TP_BUI1805763264))) {
     tp_self6 = ext_call;
     ret_val9 = ATTR(ATTR(tp_self6,fun),ret);
     L34 = CGEN_d1425664957(self, ret_val9);
     SARR((ARRAYSTR)L1,arg_pos,(STR)L34); 
     ;
     tp_self7 = ext_call;
     ret_val10 = ATTR(ATTR(tp_self7,fun),ret);
     L36 = CGEN_f2084214268(self, ret_val10);
     SARR((ARRAYSTR)L2,arg_pos,(STR)L36); 
     ;
     L381_=(STR)ARR((ARRAYSTR)L1,arg_pos); 
     res = L381_;
     L391_=INTPLUS(arg_pos,1); 
     arg_pos = L391_;
     SARR((ARRAYSTR)L1,arg_pos,(STR)((STR) &S1L)); 
     ;
     SARR((ARRAYSTR)L2,arg_pos,(STR)((STR) &F_LENGTH1)); 
     ;
     L431_=INTPLUS(arg_pos,1); 
     arg_pos = L431_;
    }
    else {
     L44 = L41;
     if ((*dTP_is242312711[TAG(L44)])(L44, ((OB) TP_BUI2088674906))) {
      tp_self8 = ext_call;
      ret_val11 = ATTR(ATTR(tp_self8,fun),ret);
      tmp1 = CGEN_d1425664957(self, ret_val11);
      ndefer_self = self;
      plus_self = tmp1;
      plus_sarg = ((STR) &ZALLOC946595004);
      ret_val12 = STR_ap2004550586(plus_self, plus_sarg);
      ndefer_s = ret_val12;
      CGEN_d695831906(ndefer_self);
      SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
      ndefer_self1 = self;
      plus_self1 = tmp1;
      plus_sarg1 = ((STR) &sizeF_133617273);
      ret_val13 = STR_ap2004550586(plus_self1, plus_sarg1);
      ndefer_s1 = ret_val13;
      CGEN_d695831906(ndefer_self1);
      SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
      ndefer_self2 = self;
      plus_self2 = tmp1;
      plus_sarg2 = ((STR) &addres395294157);
      ret_val14 = STR_ap2004550586(plus_self2, plus_sarg2);
      ndefer_s2 = ret_val14;
      CGEN_d695831906(ndefer_self2);
      SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
      plus_self3 = tmp1;
      plus_sarg3 = ((STR) &address2);
      ret_val15 = STR_ap2004550586(plus_self3, plus_sarg3);
      L45 = ret_val15;
      SARR((ARRAYSTR)L1,arg_pos,(STR)L45); 
      ;
      tp_self9 = ext_call;
      ret_val16 = ATTR(ATTR(tp_self9,fun),ret);
      L47 = CGEN_f2084214268(self, ret_val16);
      SARR((ARRAYSTR)L2,arg_pos,(STR)L47); 
      ;
      res = tmp1;
      L491_=INTPLUS(arg_pos,1); 
      arg_pos = L491_;
      plus_self4 = tmp1;
      plus_sarg4 = ((STR) &size3);
      ret_val17 = STR_ap2004550586(plus_self4, plus_sarg4);
      L50 = ret_val17;
      SARR((ARRAYSTR)L1,arg_pos,(STR)L50); 
      ;
      SARR((ARRAYSTR)L2,arg_pos,(STR)((STR) &F_LENGTH1)); 
      ;
      L541_=INTPLUS(arg_pos,1); 
      arg_pos = L541_;
     }
    }
   }
  }
 }
 L3 = arg_pos;
 L551_=INTMINUS(num_args,num_character_ar); 
 string_length_in = L551_;
 L561_=ASIZE((AM_EXT_CALL_EXPR)ext_call); 
 L57 = L561_;
 L581_=(1)<(L57); 
 if (L581_) {
  {
   BOOL f_L591_ = TRUE;
   L51 = 1;
   L601_=ASIZE((AM_EXT_CALL_EXPR)ext_call); 
   L62 = L601_;
   L631_=INTMINUS(L62,1); 
   L61 = L631_;
   L591_=L51-1;L591_m=L61; 
   while (1) {
    if(L591_++>=L591_m)  goto after_loop; 
    i = L591_;
    L651_=(AM_CALL_ARG)ARR((AM_EXT_CALL_EXPR)ext_call,i); 
    arg_str = CGEN_e1186969123(self, L651_);
    L661_=(AM_CALL_ARG)ARR((AM_EXT_CALL_EXPR)ext_call,i); 
    L67=L661_;
    L68 = ATTR(L67,expr);
    tp = (*dAM_EXPR_tprdTP[TAG(L68)])(L68);
    L69 = CGEN_f2084214268(self, tp);
    SARR((ARRAYSTR)L2,arg_pos,(STR)L69); 
    ;
    L71 = tp;
    L72 = L71;
    if ((*dTP_is242312711[TAG(L72)])(L72, ((OB) TP_BUI1805763264))) {
     SARR((ARRAYSTR)L1,arg_pos,(STR)arg_str); 
     ;
     SARR((ARRAYSTR)L1,string_length_in,(STR)((STR) &S1L)); 
     ;
     SARR((ARRAYSTR)L2,string_length_in,(STR)((STR) &F_LENGTH1)); 
     ;
     L761_=INTPLUS(string_length_in,1); 
     string_length_in = L761_;
    }
    else {
     L77 = L71;
     if ((*dTP_is242312711[TAG(L77)])(L77, ((OB) TP_BUI2088674906))) {
      plus_self5 = arg_str;
      plus_sarg5 = ((STR) &address2);
      ret_val18 = STR_ap2004550586(plus_self5, plus_sarg5);
      L78 = ret_val18;
      SARR((ARRAYSTR)L1,arg_pos,(STR)L78); 
      ;
      plus_self6 = arg_str;
      plus_sarg6 = ((STR) &size3);
      ret_val19 = STR_ap2004550586(plus_self6, plus_sarg6);
      L80 = ret_val19;
      SARR((ARRAYSTR)L1,string_length_in,(STR)L80); 
      ;
      SARR((ARRAYSTR)L2,string_length_in,(STR)((STR) &F_LENGTH1)); 
      ;
      L831_=INTPLUS(string_length_in,1); 
      string_length_in = L831_;
     }
     else {
      SARR((ARRAYSTR)L1,arg_pos,(STR)arg_str); 
      ;
     }
    }
    L851_=INTPLUS(arg_pos,1); 
    arg_pos = L851_;
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

STR CGEN_e725358689(CGEN self, AM_BND1124877163 arg) {
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
 CODE_FILE plus_self2;
 dSTR plus_s2;
 CODE_FILE ret_val9;
 CODE_FILE plus_self3;
 dSTR plus_s3;
 CGEN bind_fortran_nam;
 STR bind_fortran_nam1;
 STR ret_val10;
 STR res1 = ((STR) NULL);
 BIND_FORTRAN bind_name_self;
 STR bind_name_name;
 STR ret_val11;
 STR res2 = ((STR) NULL);
 BIND_FORTRAN append_underscor;
 STR append_underscor1;
 STR ret_val12;
 STR plus_self4;
 STR plus_sarg;
 STR ret_val13;
 STR plus_self5;
 STR plus_sarg1;
 STR ret_val14;
 STR plus_self6;
 STR plus_sarg2;
 STR ret_val15;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN make_sure_emitte;
 SIG make_sure_emitte1;
 AM_ROUT_DEF ard;
 NAMESPACE L1;
 OB L2;
 extern STR L;
 BOOL L3;
 BOOL L41_;
 FSTR L5;
 dSTR L6;
 extern STR F_ROUT3;
 BOOL L7;
 BOOL L81_;
 FSTR L9;
 dSTR L10;
 BOOL L11;
 BOOL L121_;
 FSTR L13;
 dSTR L14;
 dSTR L15;
 OB L16;
 FSTR L17;
 dSTR L18;
 extern STR F_ROUT4;
 extern STR S_;
 extern STR name16;
 BOOL L19;
 BOOL L201_;
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
 plus_s1 = ((dSTR) ((STR) &F_ROUT3));
 L7 = (plus_s1==((dSTR) NULL));
 L81_=!(L7); 
 if (L81_) {
  L9 = ATTR(plus_self1,ntext);
  L10 = plus_s1;
  SATTR(plus_self1,ntext,FSTR_p1752847026(L9, (*dSTR_strrSTR[TAG(L10)])(L10)));
 }
 ret_val8 = plus_self1;
 plus_self2 = ret_val8;
 plus_s2 = ((dSTR) res);
 L11 = (plus_s2==((dSTR) NULL));
 L121_=!(L11); 
 if (L121_) {
  L13 = ATTR(plus_self2,ntext);
  L14 = plus_s2;
  SATTR(plus_self2,ntext,FSTR_p1752847026(L13, (*dSTR_strrSTR[TAG(L14)])(L14)));
 }
 ret_val9 = plus_self2;
 plus_self3 = ret_val9;
 L16=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L16==NULL) FATAL("Unable to allocate more memory");
 memset(L16,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L16)->header.tag=CHAR_tag;
 L15 = (dSTR)((CHAR_boxed) L16);
 ((CHAR_boxed) L15)->immutable_part = ';';
 plus_s3 = L15;
 L17 = ATTR(plus_self3,ntext);
 L18 = plus_s3;
 SATTR(plus_self3,ntext,FSTR_p1752847026(L17, (*dSTR_strrSTR[TAG(L18)])(L18)));
 ndefer_self = self;
 plus_self6 = res;
 bind_fortran_nam = self;
 bind_fortran_nam1 = ATTR(ATTR(arg,fun),name1).str;
 res1 = STR_ap1693864410(ATTR(bind_fortran_nam,fortran_prepend_), bind_fortran_nam1, ATTR(bind_fortran_nam,fortran_append_u));
 if (ATTR(bind_fortran_nam,fortran_bind_fun)) {
  bind_name_self = ((BIND_FORTRAN) NULL);
  bind_name_name = res1;
  append_underscor = bind_name_self;
  append_underscor1 = bind_name_name;
  plus_self4 = append_underscor1;
  plus_sarg = ((STR) &S_);
  ret_val13 = STR_ap2004550586(plus_self4, plus_sarg);
  ret_val12 = ret_val13;
  res2 = ret_val12;
  ret_val11 = res2;
  res1 = ret_val11;
 }
 ret_val10 = res1;
 plus_self5 = ret_val10;
 plus_sarg1 = ((STR) &name16);
 ret_val14 = STR_ap2004550586(plus_self5, plus_sarg1);
 plus_sarg2 = STR_ap2004550586(((STR) &F_ROUT4), ret_val14);
 ret_val15 = STR_ap2004550586(plus_self6, plus_sarg2);
 ndefer_s = ret_val15;
 CGEN_d695831906(ndefer_self);
 SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
 make_sure_emitte = self;
 make_sure_emitte1 = ATTR(arg,fun);
 ard = FMAPSI2016001247(ATTR(make_sure_emitte,not_emitted), make_sure_emitte1);
 L19 = (ard==((AM_ROUT_DEF) NULL));
 L201_=!(L19); 
 if (L201_) {
  SATTR(make_sure_emitte,leftovers,FSETAM2044524049(ATTR(make_sure_emitte,leftovers), ard));
  SATTR(make_sure_emitte,not_emitted,FMAPSI1625125906(ATTR(make_sure_emitte,not_emitted), make_sure_emitte1));
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e73452157(CGEN self, AM_BND1124877163 arg) {
 STR ret_val;
 FLISTSTR dummy1 = ((FLISTSTR) NULL);
 STR res = ((STR) NULL);
 dTP tp;
 SIG is_iter_self;
 BOOL ret_val1 = BOOL_zero;
 IDENT is_iter_self1 = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
 AM_BND1124877163 tp_self;
 dTP ret_val3;
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
 FLISTSTR* L11;
 is_iter_self = ATTR(arg,fun);
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
 ret_val2 = L1;
 ret_val1 = ret_val2;
 if (ret_val1) {
  res = CGEN_e1305366955(self, arg);
 }
 else {
  tp_self = arg;
  ret_val3 = ATTR(tp_self,tp_at);
  tp = ret_val3;
  switch (TAG(tp)) {
   case TP_ROUT_tag:
    if (ATTR(((TP_ROUT) tp),is_fortran)) {
     res = CGEN_e725358689(self, arg);
    }
    else {
     L11 = &dummy1;
     res = CGEN_e409242292(self, arg, TRUE, L11);
    } break;
   default: ;
    FATAL("No applicable type in typecase\nin CGEN::emit_am_bnd_create_expr(AM_BND_CREATE_EXPR):STR\n./Back/cgen.sa:6165:17");
  }
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e848933497(CGEN self, AM_ATTR_EXPR arg) {
 STR ret_val;
 STR s1;
 STR attrname;
 dAM_EXPR sfe;
 STR l4;
 STR l;
 STR l1;
 STR sf;
 STR sft;
 dAM_EXPR sfe1;
 STR l41;
 STR a;
 CGEN mang_self;
 OB mang_ob;
 OB mang_ns;
 STR ret_val1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val3;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val4;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN cast_self;
 dTP cast_dest_tp;
 dTP cast_src_tp;
 STR cast_expr;
 BOOL cast_only_boxing = BOOL_zero;
 STR ret_val5;
 PROG psather_self;
 BOOL ret_val6 = BOOL_zero;
 AM_ATTR_EXPR tp_self;
 dTP ret_val7;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val8;
 AM_ATTR_EXPR tp_self1;
 dTP ret_val9;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val10;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val11;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val12;
 AM_ATTR_EXPR tp_self2;
 dTP ret_val13;
 AM_ATTR_EXPR tp_self3;
 dTP ret_val14;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val15;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val16;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val17;
 CGEN ndefer_self1;
 STR ndefer_s1;
 CODE_FILE uses_tp_self;
 dTP uses_tp_t;
 AM_ATTR_EXPR tp_self4;
 dTP ret_val18;
 CODE_FILE uses_tp_self1;
 dTP uses_tp_t1;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val19;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val20;
 STR plus_self12;
 STR plus_sarg12;
 STR ret_val21;
 STR plus_self13;
 STR plus_sarg13;
 STR ret_val22;
 AM_ATTR_EXPR tp_self5;
 dTP ret_val23;
 AM_ATTR_EXPR tp_self6;
 dTP ret_val24;
 STR plus_self14;
 STR plus_sarg14;
 STR ret_val25;
 STR plus_self15;
 STR plus_sarg15;
 STR ret_val26;
 AM_ATTR_EXPR tp_self7;
 dTP ret_val27;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val28;
 STR plus_self16;
 STR plus_sarg16;
 STR ret_val29;
 STR plus_self17;
 STR plus_sarg17;
 STR ret_val30;
 STR plus_self18;
 STR plus_sarg18;
 STR ret_val31;
 STR plus_self19;
 STR plus_sarg19;
 STR ret_val32;
 CGEN ndefer_self2;
 STR ndefer_s2;
 STR plus_self20;
 STR plus_sarg20;
 STR ret_val33;
 STR plus_self21;
 STR plus_sarg21;
 STR ret_val34;
 STR plus_self22;
 STR plus_sarg22;
 STR ret_val35;
 CGEN ndefer_self3;
 STR ndefer_s3;
 AM_ATTR_EXPR tp_self8;
 dTP ret_val36;
 AM_ATTR_EXPR tp_self9;
 dTP ret_val37;
 CGEN mang_self2;
 OB mang_ob2;
 STR ret_val38;
 STR plus_self23;
 STR plus_sarg23;
 STR ret_val39;
 STR plus_self24;
 STR plus_sarg24;
 STR ret_val40;
 CGEN ndefer_self4;
 STR ndefer_s4;
 AM_ATTR_EXPR tp_self10;
 dTP ret_val41;
 CGEN mang_self3;
 OB mang_ob3;
 STR ret_val42;
 STR plus_self25;
 STR plus_sarg25;
 STR ret_val43;
 STR plus_self26;
 STR plus_sarg26;
 STR ret_val44;
 CGEN mang_self4;
 OB mang_ob4;
 STR ret_val45;
 STR plus_self27;
 STR plus_sarg27;
 STR ret_val46;
 STR plus_self28;
 STR plus_sarg28;
 STR ret_val47;
 CGEN ndefer_self5;
 STR ndefer_s5;
 CGEN mang_self5;
 OB mang_ob5;
 STR ret_val48;
 STR plus_self29;
 STR plus_sarg29;
 STR ret_val49;
 STR plus_self30;
 STR plus_sarg30;
 STR ret_val50;
 CGEN ndefer_self6;
 STR ndefer_s6;
 OB L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 extern STR name33;
 extern STR name16;
 dAM_EXPR L5;
 dTP L6;
 extern STR name18;
 BOOL L7;
 extern STR name75;
 extern STR ATTRs1;
 extern STR name3;
 extern STR name3;
 STR L8;
 extern STR name126;
 extern STR name26;
 extern STR ATTRs2;
 extern STR name3;
 extern STR name3;
 STR L9;
 extern STR name26;
 extern STR ATTR1;
 extern STR name3;
 extern STR name26;
 extern STR name1;
 extern STR name1;
 dTP L10;
 extern STR name92;
 extern STR name58;
 extern STR name33;
 extern STR name16;
 dAM_EXPR L11;
 extern STR Ooflocalaccesses;
 dSTAT L12;
 extern STR name1;
 dTP L13;
 extern STR name18;
 extern STR ATTR1;
 extern STR name3;
 extern STR name26;
 BOOL L14;
 dTP L15;
 dTP L16;
 BOOL L17;
 BOOL L181_;
 extern STR VASS_LP1;
 extern STR name3;
 extern STR name3;
 extern STR name76;
 extern STR name33;
 extern STR name16;
 dTP L19;
 dTP L20;
 extern STR F_VA_RATTR_NA1;
 extern STR name3;
 STR L21;
 extern STR name3;
 extern STR name3;
 extern STR name76;
 extern STR F_V_RATTR_LP1;
 extern STR name3;
 STR L22;
 extern STR name3;
 STR L23;
 extern STR name3;
 extern STR name3;
 extern STR name76;
 extern STR F_R_RATTR_NA1;
 extern STR name3;
 STR L24;
 extern STR name3;
 extern STR name3;
 extern STR name76;
 s1 = CGEN_e1639504572(self, ATTR(arg,ob));
 mang_self = self;
 L2=ZALLOC(sizeof(struct IDENT_boxed_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=IDENT_tag;
 L1 = (OB)((IDENT_boxed) L2);
 ((IDENT_boxed) L1)->immutable_part = ATTR(arg,at);
 mang_ob = L1;
 mang_ns = ((OB) ATTR(arg,self_tp));
 ret_val1 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, mang_ns);
 attrname = ret_val1;
 L3 = ATTR(ATTR(self,prog),distributed);
 L41_=!(L3); 
 if (L41_) {
  sfe = ATTR(arg,ob);
  if (sfe==NULL) {
   goto other371;
  } else
  switch (TAG(sfe)) {
   case AM_LOCAL_EXPR_tag: break;
   case AM_SHARED_EXPR_tag: break;
   case AM_ATTR_EXPR_tag: break;
   default: ;
   other371: ;
    l4 = CGEN_d1425664957(self, ATTR(arg,self_tp));
    ndefer_self = self;
    plus_self = l4;
    plus_sarg = ((STR) &name33);
    ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
    plus_self1 = ret_val2;
    plus_sarg1 = s1;
    ret_val3 = STR_ap2004550586(plus_self1, plus_sarg1);
    plus_self2 = ret_val3;
    plus_sarg2 = ((STR) &name16);
    ret_val4 = STR_ap2004550586(plus_self2, plus_sarg2);
    ndefer_s = ret_val4;
    CGEN_d695831906(ndefer_self);
    SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
    s1 = l4;
  }
  cast_self = self;
  cast_dest_tp = ATTR(arg,self_tp);
  L5 = ATTR(arg,ob);
  cast_src_tp = (*dAM_EXPR_tprdTP[TAG(L5)])(L5);
  cast_expr = s1;
  cast_only_boxing = FALSE;
  ret_val5 = CGEN_d1915593494(cast_self, cast_dest_tp, cast_src_tp, cast_expr, cast_only_boxing, ((dMODE) NULL));
  s1 = ret_val5;
  L6 = ATTR(arg,self_tp);
  if ((*dTP_is1334578382[TAG(L6)])(L6)) {
   ret_val = STR_ap1693864410(s1, ((STR) &name18), attrname);
   return ret_val;
  }
  else {
   if (ATTR(arg,secure1)) {
    psather_self = ATTR(self,prog);
    if (ATTR(psather_self,threads)) {
     L7 = TRUE;
    } else {
     L7 = ATTR(psather_self,distributed);
    }
    ret_val6 = L7;
    if (ret_val6) {
     tp_self = arg;
     ret_val7 = ATTR(tp_self,tp_at);
     l = CGEN_d1425664957(self, ret_val7);
     plus_self3 = ((STR) &name75);
     plus_sarg3 = l;
     ret_val8 = STR_ap2004550586(plus_self3, plus_sarg3);
     plus_self6 = ret_val8;
     tp_self1 = arg;
     ret_val9 = ATTR(tp_self1,tp_at);
     L8 = CGEN_d815167520(self, ret_val9);
     plus_self4 = ((STR) &name126);
     plus_sarg4 = l;
     ret_val10 = STR_ap2004550586(plus_self4, plus_sarg4);
     plus_self5 = ret_val10;
     plus_sarg5 = ((STR) &name26);
     ret_val11 = STR_ap2004550586(plus_self5, plus_sarg5);
     plus_sarg6 = STR_ap1182453786(((STR) &ATTRs1), s1, ((STR) &name3), attrname, ((STR) &name3), L8, ret_val11);
     ret_val12 = STR_ap2004550586(plus_self6, plus_sarg6);
     ret_val = ret_val12;
     return ret_val;
    }
    else {
     tp_self2 = arg;
     ret_val13 = ATTR(tp_self2,tp_at);
     L9 = CGEN_d815167520(self, ret_val13);
     ret_val = STR_ap1182453786(((STR) &ATTRs2), s1, ((STR) &name3), attrname, ((STR) &name3), L9, ((STR) &name26));
     return ret_val;
    }
   }
   else {
    ret_val = STR_ap44172742(((STR) &ATTR1), s1, ((STR) &name3), attrname, ((STR) &name26));
    return ret_val;
   }
  }
 }
 else {
  tp_self3 = arg;
  ret_val14 = ATTR(tp_self3,tp_at);
  l1 = CGEN_d1425664957(self, ret_val14);
  sf = ((STR) &name1);
  sft = ((STR) &name1);
  L10 = ATTR(arg,self_tp);
  if ((*dTP_is1334578382[TAG(L10)])(L10)) {
   sf = ((STR) &name92);
   sft = ((STR) &name58);
  }
  sfe1 = ATTR(arg,ob);
  if (sfe1==NULL) {
   goto other372;
  } else
  switch (TAG(sfe1)) {
   case AM_LOCAL_EXPR_tag: break;
   case AM_SHARED_EXPR_tag: break;
   default: ;
   other372: ;
    l41 = CGEN_d1425664957(self, ATTR(arg,self_tp));
    ndefer_self1 = self;
    plus_self7 = l41;
    plus_sarg7 = ((STR) &name33);
    ret_val15 = STR_ap2004550586(plus_self7, plus_sarg7);
    plus_self8 = ret_val15;
    plus_sarg8 = s1;
    ret_val16 = STR_ap2004550586(plus_self8, plus_sarg8);
    plus_self9 = ret_val16;
    plus_sarg9 = ((STR) &name16);
    ret_val17 = STR_ap2004550586(plus_self9, plus_sarg9);
    ndefer_s1 = ret_val17;
    CGEN_d695831906(ndefer_self1);
    SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
    s1 = l41;
  }
  uses_tp_self = ATTR(self,code_c);
  uses_tp_t = ATTR(arg,self_tp);
  CODE_F525364269(uses_tp_self, LAYOUT79932797(((LAYOUT_TBL) NULL), uses_tp_t));
  uses_tp_self1 = ATTR(self,code_c);
  tp_self4 = arg;
  ret_val18 = ATTR(tp_self4,tp_at);
  uses_tp_t1 = ret_val18;
  CODE_F525364269(uses_tp_self1, LAYOUT79932797(((LAYOUT_TBL) NULL), uses_tp_t1));
  L11 = ATTR(arg,ob);
  if ((*dAM_EX1117610980[TAG(L11)])(L11)) {
   L12 = ATTR(ATTR(self,prog),stat1);
   (*dSTAT_incr_STR[TAG(L12)])(L12, ((STR) &Ooflocalaccesses));
   a = ((STR) &name1);
   L13 = ATTR(arg,self_tp);
   if ((*dTP_is1334578382[TAG(L13)])(L13)) {
    a = STR_ap1693864410(s1, ((STR) &name18), attrname);
   }
   else {
    plus_self10 = ((STR) &ATTR1);
    plus_sarg10 = s1;
    ret_val19 = STR_ap2004550586(plus_self10, plus_sarg10);
    plus_self11 = ret_val19;
    plus_sarg11 = ((STR) &name3);
    ret_val20 = STR_ap2004550586(plus_self11, plus_sarg11);
    plus_self12 = ret_val20;
    plus_sarg12 = attrname;
    ret_val21 = STR_ap2004550586(plus_self12, plus_sarg12);
    plus_self13 = ret_val21;
    plus_sarg13 = ((STR) &name26);
    ret_val22 = STR_ap2004550586(plus_self13, plus_sarg13);
    a = ret_val22;
   }
   tp_self5 = arg;
   ret_val23 = ATTR(tp_self5,tp_at);
   L15 = ret_val23;
   if ((*dTP_is1334578382[TAG(L15)])(L15)) {
    tp_self6 = arg;
    ret_val24 = ATTR(tp_self6,tp_at);
    L16 = ret_val24;
    L17 = (*dTP_is418055720[TAG(L16)])(L16);
    L181_=!(L17); 
    L14 = L181_;
   } else {
    L14 = FALSE;
   }
   if (L14) {
    ndefer_self2 = self;
    plus_self14 = ((STR) &VASS_LP1);
    plus_sarg14 = l1;
    ret_val25 = STR_ap2004550586(plus_self14, plus_sarg14);
    plus_self15 = ret_val25;
    plus_sarg15 = ((STR) &name3);
    ret_val26 = STR_ap2004550586(plus_self15, plus_sarg15);
    plus_self16 = ret_val26;
    mang_self1 = self;
    tp_self7 = arg;
    ret_val27 = ATTR(tp_self7,tp_at);
    mang_ob1 = ((OB) ret_val27);
    ret_val28 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
    plus_sarg16 = ret_val28;
    ret_val29 = STR_ap2004550586(plus_self16, plus_sarg16);
    plus_self17 = ret_val29;
    plus_sarg17 = ((STR) &name3);
    ret_val30 = STR_ap2004550586(plus_self17, plus_sarg17);
    plus_self18 = ret_val30;
    plus_sarg18 = a;
    ret_val31 = STR_ap2004550586(plus_self18, plus_sarg18);
    plus_self19 = ret_val31;
    plus_sarg19 = ((STR) &name76);
    ret_val32 = STR_ap2004550586(plus_self19, plus_sarg19);
    ndefer_s2 = ret_val32;
    CGEN_d695831906(ndefer_self2);
    SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
   }
   else {
    ndefer_self3 = self;
    plus_self20 = l1;
    plus_sarg20 = ((STR) &name33);
    ret_val33 = STR_ap2004550586(plus_self20, plus_sarg20);
    plus_self21 = ret_val33;
    plus_sarg21 = a;
    ret_val34 = STR_ap2004550586(plus_self21, plus_sarg21);
    plus_self22 = ret_val34;
    plus_sarg22 = ((STR) &name16);
    ret_val35 = STR_ap2004550586(plus_self22, plus_sarg22);
    ndefer_s3 = ret_val35;
    CGEN_d695831906(ndefer_self3);
    SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
   }
  }
  else {
   tp_self8 = arg;
   ret_val36 = ATTR(tp_self8,tp_at);
   L19 = ret_val36;
   if ((*dTP_is1334578382[TAG(L19)])(L19)) {
    tp_self9 = arg;
    ret_val37 = ATTR(tp_self9,tp_at);
    L20 = ret_val37;
    if ((*dTP_is418055720[TAG(L20)])(L20)) {
     ndefer_self4 = self;
     mang_self2 = self;
     mang_ob2 = ((OB) ATTR(arg,self_tp));
     ret_val38 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, ((OB) NULL));
     plus_self23 = ret_val38;
     plus_sarg23 = sft;
     ret_val39 = STR_ap2004550586(plus_self23, plus_sarg23);
     L21 = ret_val39;
     plus_self24 = sf;
     plus_sarg24 = s1;
     ret_val40 = STR_ap2004550586(plus_self24, plus_sarg24);
     ndefer_s4 = STR_ap44172742(((STR) &F_VA_RATTR_NA1), l1, ((STR) &name3), L21, STR_ap44172742(((STR) &name3), ret_val40, ((STR) &name3), attrname, ((STR) &name76)));
     CGEN_d695831906(ndefer_self4);
     SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
    }
    else {
     ndefer_self5 = self;
     mang_self3 = self;
     tp_self10 = arg;
     ret_val41 = ATTR(tp_self10,tp_at);
     mang_ob3 = ((OB) ret_val41);
     ret_val42 = MANGLE119219986(ATTR(mang_self3,mangler), mang_ob3, ((OB) NULL));
     plus_self25 = ret_val42;
     plus_sarg25 = ((STR) &name3);
     ret_val43 = STR_ap2004550586(plus_self25, plus_sarg25);
     plus_self26 = ret_val43;
     plus_sarg26 = l1;
     ret_val44 = STR_ap2004550586(plus_self26, plus_sarg26);
     L22 = ret_val44;
     mang_self4 = self;
     mang_ob4 = ((OB) ATTR(arg,self_tp));
     ret_val45 = MANGLE119219986(ATTR(mang_self4,mangler), mang_ob4, ((OB) NULL));
     plus_self27 = ret_val45;
     plus_sarg27 = sft;
     ret_val46 = STR_ap2004550586(plus_self27, plus_sarg27);
     L23 = ret_val46;
     plus_self28 = sf;
     plus_sarg28 = s1;
     ret_val47 = STR_ap2004550586(plus_self28, plus_sarg28);
     ndefer_s5 = STR_ap44172742(((STR) &F_V_RATTR_LP1), L22, ((STR) &name3), L23, STR_ap44172742(((STR) &name3), ret_val47, ((STR) &name3), attrname, ((STR) &name76)));
     CGEN_d695831906(ndefer_self5);
     SATTR(ndefer_self5,routine_code,FSTR_p1752847026(ATTR(ndefer_self5,routine_code), ndefer_s5));
    }
   }
   else {
    ndefer_self6 = self;
    mang_self5 = self;
    mang_ob5 = ((OB) ATTR(arg,self_tp));
    ret_val48 = MANGLE119219986(ATTR(mang_self5,mangler), mang_ob5, ((OB) NULL));
    plus_self29 = ret_val48;
    plus_sarg29 = sft;
    ret_val49 = STR_ap2004550586(plus_self29, plus_sarg29);
    L24 = ret_val49;
    plus_self30 = sf;
    plus_sarg30 = s1;
    ret_val50 = STR_ap2004550586(plus_self30, plus_sarg30);
    ndefer_s6 = STR_ap44172742(((STR) &F_R_RATTR_NA1), l1, ((STR) &name3), L24, STR_ap44172742(((STR) &name3), ret_val50, ((STR) &name3), attrname, ((STR) &name76)));
    CGEN_d695831906(ndefer_self6);
    SATTR(ndefer_self6,routine_code,FSTR_p1752847026(ATTR(ndefer_self6,routine_code), ndefer_s6));
   }
  }
  ret_val = l1;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e887129228(CGEN self, AM_IS_VOID_EXPR arg) {
 STR ret_val;
 dTP arg_tp;
 dAM_EXPR L1;
 dTP L2;
 extern STR FVOID1;
 STR L3;
 extern STR name26;
 extern STR name24;
 STR L4;
 extern STR name110;
 STR L5;
 extern STR name26;
 L1 = ATTR(arg,arg);
 arg_tp = (*dAM_EXPR_tprdTP[TAG(L1)])(L1);
 L2 = arg_tp;
 if ((*dTP_is1334578382[TAG(L2)])(L2)) {
  ret_val = CGEN_v1144762521(self, arg_tp, CGEN_e1639504572(self, ATTR(arg,arg)));
  return ret_val;
 }
 else {
  if (ATTR(ATTR(self,prog),distributed)) {
   L3 = CGEN_e1639504572(self, ATTR(arg,arg));
   ret_val = STR_ap1693864410(((STR) &FVOID1), L3, ((STR) &name26));
   return ret_val;
  }
  else {
   L4 = CGEN_e1639504572(self, ATTR(arg,arg));
   L5 = CGEN_d815167520(self, arg_tp);
   ret_val = STR_ap44172742(((STR) &name24), L4, ((STR) &name110), L5, ((STR) &name26));
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_v1144762521(CGEN self, dTP tp, STR e) {
 STR ret_val;
 AM_OB_DEF aod;
 BOOL after_first = BOOL_zero;
 STR res;
 FMAPIDENTdTP aod_at;
 ARRAYIDENT aod_idents;
 IDENT p_key = IDENT_zero;
 ARRAYIDENT L11;
 dTP p_tp;
 STR key1;
 INT L21 = INT_zero;
 INT L31 = INT_zero;
 INT i = INT_zero;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val3;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val4;
 CODE_FILE uses_tp_self;
 dTP uses_tp_t;
 INT L41 = INT_zero;
 CGEN mang_self1;
 OB mang_ob1;
 OB mang_ns;
 STR ret_val5;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val6;
 STR plus_self4;
 CHAR plus_arg = CHAR_zero;
 STR ret_val7;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val8;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val9;
 STR r;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val10;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val11;
 CODE_FILE uses_tp_self1;
 dTP uses_tp_t1;
 STR plus_self7;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val12;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val13;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val14;
 STR r1;
 STR plus_self8;
 STR plus_sarg6;
 STR ret_val15;
 STR plus_self9;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val16;
 STR s2;
 CHAR str_self2 = CHAR_zero;
 STR ret_val17;
 STR create_self2;
 CHAR create_c2 = CHAR_zero;
 STR ret_val18;
 STR r2;
 STR plus_self10;
 STR plus_sarg7;
 STR ret_val19;
 STR plus_self11;
 STR plus_sarg8;
 STR ret_val20;
 CGEN mang_self2;
 OB mang_ob2;
 STR ret_val21;
 STR plus_self12;
 STR plus_sarg9;
 STR ret_val22;
 STR plus_self13;
 STR plus_sarg10;
 STR ret_val23;
 CODE_FILE uses_tp_self2;
 dTP uses_tp_t2;
 STR plus_self14;
 STR plus_sarg11;
 STR ret_val24;
 STR plus_self15;
 STR plus_sarg12;
 STR ret_val25;
 STR plus_self16;
 INT plus_arg3 = INT_zero;
 STR ret_val26;
 STR s3;
 INT str_self3 = INT_zero;
 STR ret_val27;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val28;
 FSTR str_self4;
 STR ret_val29;
 STR plus_self17;
 STR plus_sarg13;
 STR ret_val30;
 STR plus_self18;
 STR plus_sarg14;
 STR ret_val31;
 STR plus_self19;
 CHAR plus_arg4 = CHAR_zero;
 STR ret_val32;
 STR s4;
 CHAR str_self5 = CHAR_zero;
 STR ret_val33;
 STR create_self3;
 CHAR create_c3 = CHAR_zero;
 STR ret_val34;
 STR r3;
 STR plus_self20;
 STR plus_sarg15;
 STR ret_val35;
 STR plus_self21;
 STR plus_sarg16;
 STR ret_val36;
 STR plus_self22;
 INT plus_arg5 = INT_zero;
 STR ret_val37;
 STR s5;
 INT str_self6 = INT_zero;
 STR ret_val38;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val39;
 FSTR str_self7;
 STR ret_val40;
 STR plus_self23;
 STR plus_sarg17;
 STR ret_val41;
 CGEN mang_self3;
 OB mang_ob3;
 STR ret_val42;
 STR plus_self24;
 STR plus_sarg18;
 STR ret_val43;
 STR plus_self25;
 STR plus_sarg19;
 STR ret_val44;
 BOOL L5;
 dTP L6;
 dTP L7;
 extern STR S_IS_VOID;
 extern STR name26;
 extern STR name1;
 BOOL L8;
 BOOL L91_;
 INT L101_br;
 IDENT aL101_;
 IDENT L12;
 OB L13;
 OB L14;
 extern STR name88;
 dTP L15;
 STR L16;
 INT L17;
 OB L18;
 INT L19;
 STR L22;
 INT L23;
 OB L24;
 INT L25;
 STR L27;
 INT L28;
 OB L29;
 INT L30;
 extern STR name130;
 extern STR S04;
 INT L331_;
 BOOL L34;
 BOOL L351_;
 INT L361_,L361_m;
 INT L37;
 extern STR name88;
 dTP L38;
 extern STR arr_part1;
 BOOL L39;
 BOOL L401_;
 extern STR name7;
 STR L42;
 INT L43;
 OB L44;
 INT L45;
 extern STR arr_part1;
 BOOL L47;
 BOOL L481_;
 extern STR name131;
 extern STR S04;
 L6 = tp;
 if ((*dTP_is1334578382[TAG(L6)])(L6)) {
  L7 = tp;
  L5 = (*dTP_is914808564[TAG(L7)])(L7);
 } else {
  L5 = FALSE;
 }
 if (L5) {
  mang_self = self;
  mang_ob = ((OB) tp);
  ret_val1 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
  plus_self = ret_val1;
  plus_sarg = ((STR) &S_IS_VOID);
  ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val2;
  plus_sarg1 = e;
  ret_val3 = STR_ap2004550586(plus_self1, plus_sarg1);
  plus_self2 = ret_val3;
  plus_sarg2 = ((STR) &name26);
  ret_val4 = STR_ap2004550586(plus_self2, plus_sarg2);
  ret_val = ret_val4;
  return ret_val;
 }
 aod = CGEN_a1221397173(self, tp);
 after_first = FALSE;
 res = ((STR) &name1);
 uses_tp_self = ATTR(self,code_c);
 uses_tp_t = tp;
 CODE_F525364269(uses_tp_self, LAYOUT79932797(((LAYOUT_TBL) NULL), uses_tp_t));
 L8 = (ATTR(aod,at)==((FMAPIDENTdTP) NULL));
 L91_=!(L8); 
 if (L91_) {
  aod_at = ATTR(aod,at);
  aod_idents = ATTR(aod,sorted_at);
  {
   BOOL f_L101_ = TRUE;
   /* loop index variable */
   L41 = 0;
   L11 = aod_idents;
   L101_br=L11==NULL?0:ASIZE((ARRAYIDENT)L11); 
   while (1) {
    if(L41>=L101_br)  goto after_loop; 
    aL101_=ARR((ARRAYIDENT)L11,L41); 
    p_key = aL101_;
    p_tp = FMAPID86862037(aod_at, p_key);
    mang_self1 = self;
    L14=ZALLOC(sizeof(struct IDENT_boxed_struct));
    if (L14==NULL) FATAL("Unable to allocate more memory");
    ((OB)L14)->header.tag=IDENT_tag;
    L13 = (OB)((IDENT_boxed) L14);
    ((IDENT_boxed) L13)->immutable_part = p_key;
    mang_ob1 = L13;
    mang_ns = ((OB) tp);
    ret_val5 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, mang_ns);
    key1 = ret_val5;
    if (after_first) {
     plus_self3 = res;
     plus_sarg3 = ((STR) &name88);
     ret_val6 = STR_ap2004550586(plus_self3, plus_sarg3);
     res = ret_val6;
    }
    L15 = p_tp;
    if ((*dTP_is1334578382[TAG(L15)])(L15)) {
     plus_self6 = res;
     plus_self4 = e;
     plus_arg = '.';
     str_self = plus_arg;
     create_self = ((STR) NULL);
     create_c = str_self;
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
     r = L16;
     SARR((STR)r,0,create_c); 
     ;
     ret_val9 = r;
     ret_val8 = ret_val9;
     s = ret_val8;
     ret_val7 = STR_ap1077157958(plus_self4, s, TRUE);
     plus_self5 = ret_val7;
     plus_sarg4 = key1;
     ret_val10 = STR_ap2004550586(plus_self5, plus_sarg4);
     plus_sarg5 = CGEN_v1144762521(self, p_tp, ret_val10);
     ret_val11 = STR_ap2004550586(plus_self6, plus_sarg5);
     res = ret_val11;
    }
    else {
     uses_tp_self1 = ATTR(self,code_c);
     uses_tp_t1 = p_tp;
     CODE_F525364269(uses_tp_self1, LAYOUT79932797(((LAYOUT_TBL) NULL), uses_tp_t1));
     plus_self7 = res;
     plus_arg1 = '(';
     str_self1 = plus_arg1;
     create_self1 = ((STR) NULL);
     create_c1 = str_self1;
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
     r1 = L22;
     SARR((STR)r1,0,create_c1); 
     ;
     ret_val14 = r1;
     ret_val13 = ret_val14;
     s1 = ret_val13;
     ret_val12 = STR_ap1077157958(plus_self7, s1, TRUE);
     plus_self8 = ret_val12;
     plus_sarg6 = e;
     ret_val15 = STR_ap2004550586(plus_self8, plus_sarg6);
     plus_self9 = ret_val15;
     plus_arg2 = '.';
     str_self2 = plus_arg2;
     create_self2 = ((STR) NULL);
     create_c2 = str_self2;
     L28 = 1;
     L30=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L28)*sizeof(CHAR);
     L29=ZALLOC_LEAF_BIG(L30);
     if (L29==NULL) FATAL("Unable to allocate more memory");
     memset(L29,0,L30);
     ((OB)L29)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L29)->header.destroyed=0;
#endif

     L27 = ((STR) L29);
     L27->asize = L28;
     r2 = L27;
     SARR((STR)r2,0,create_c2); 
     ;
     ret_val18 = r2;
     ret_val17 = ret_val18;
     s2 = ret_val17;
     ret_val16 = STR_ap1077157958(plus_self9, s2, TRUE);
     plus_self10 = ret_val16;
     plus_sarg7 = key1;
     ret_val19 = STR_ap2004550586(plus_self10, plus_sarg7);
     plus_self11 = ret_val19;
     plus_sarg8 = ((STR) &name130);
     ret_val20 = STR_ap2004550586(plus_self11, plus_sarg8);
     plus_self12 = ret_val20;
     mang_self2 = self;
     mang_ob2 = ((OB) p_tp);
     ret_val21 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, ((OB) NULL));
     plus_sarg9 = ret_val21;
     ret_val22 = STR_ap2004550586(plus_self12, plus_sarg9);
     plus_self13 = ret_val22;
     plus_sarg10 = ((STR) &S04);
     ret_val23 = STR_ap2004550586(plus_self13, plus_sarg10);
     res = ret_val23;
    }
    after_first = TRUE;
    L331_=INTPLUS(L41,1); 
    L41 = L331_;
   }
  }
  after_loop: ;
 }
 L34 = (ATTR(aod,arr)==((dTP) NULL));
 L351_=!(L34); 
 if (L351_) {
  uses_tp_self2 = ATTR(self,code_c);
  uses_tp_t2 = ATTR(aod,arr);
  CODE_F525364269(uses_tp_self2, LAYOUT79932797(((LAYOUT_TBL) NULL), uses_tp_t2));
  {
   BOOL f_L361_ = TRUE;
   L21 = 0;
   L31 = ATTR(aod,asize);
   L361_=L21-1;L361_m=L31; 
   while (1) {
    if((L361_m--)<=0)  goto after_loop1;L361_++; 
    i = L361_;
    if (after_first) {
     plus_self14 = res;
     plus_sarg11 = ((STR) &name88);
     ret_val24 = STR_ap2004550586(plus_self14, plus_sarg11);
     res = ret_val24;
    }
    L38 = ATTR(aod,arr);
    if ((*dTP_is1334578382[TAG(L38)])(L38)) {
     plus_self18 = res;
     plus_self15 = e;
     plus_sarg12 = ((STR) &arr_part1);
     ret_val25 = STR_ap2004550586(plus_self15, plus_sarg12);
     plus_self16 = ret_val25;
     plus_arg3 = i;
     str_self3 = plus_arg3;
     clear_self = buf1;
     L39 = (clear_self==((FSTR) NULL));
     L401_=!(L39); 
     if (L401_) {
      SATTR(clear_self,loc,0);
     }
     str_in_self = str_self3;
     str_in_s = buf1;
     ret_val28 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
     buf1 = ret_val28;
     str_self4 = buf1;
     ret_val29 = STR_fr1097270334(((STR) NULL), str_self4);
     ret_val27 = ret_val29;
     s3 = ret_val27;
     ret_val26 = STR_ap1077157958(plus_self16, s3, TRUE);
     plus_self17 = ret_val26;
     plus_sarg13 = ((STR) &name7);
     ret_val30 = STR_ap2004550586(plus_self17, plus_sarg13);
     plus_sarg14 = CGEN_v1144762521(self, ATTR(aod,arr), ret_val30);
     ret_val31 = STR_ap2004550586(plus_self18, plus_sarg14);
     res = ret_val31;
    }
    else {
     plus_self19 = res;
     plus_arg4 = '(';
     str_self5 = plus_arg4;
     create_self3 = ((STR) NULL);
     create_c3 = str_self5;
     L43 = 1;
     L45=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L43)*sizeof(CHAR);
     L44=ZALLOC_LEAF_BIG(L45);
     if (L44==NULL) FATAL("Unable to allocate more memory");
     memset(L44,0,L45);
     ((OB)L44)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L44)->header.destroyed=0;
#endif

     L42 = ((STR) L44);
     L42->asize = L43;
     r3 = L42;
     SARR((STR)r3,0,create_c3); 
     ;
     ret_val34 = r3;
     ret_val33 = ret_val34;
     s4 = ret_val33;
     ret_val32 = STR_ap1077157958(plus_self19, s4, TRUE);
     plus_self20 = ret_val32;
     plus_sarg15 = e;
     ret_val35 = STR_ap2004550586(plus_self20, plus_sarg15);
     plus_self21 = ret_val35;
     plus_sarg16 = ((STR) &arr_part1);
     ret_val36 = STR_ap2004550586(plus_self21, plus_sarg16);
     plus_self22 = ret_val36;
     plus_arg5 = i;
     str_self6 = plus_arg5;
     clear_self1 = buf2;
     L47 = (clear_self1==((FSTR) NULL));
     L481_=!(L47); 
     if (L481_) {
      SATTR(clear_self1,loc,0);
     }
     str_in_self1 = str_self6;
     str_in_s1 = buf2;
     ret_val39 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
     buf2 = ret_val39;
     str_self7 = buf2;
     ret_val40 = STR_fr1097270334(((STR) NULL), str_self7);
     ret_val38 = ret_val40;
     s5 = ret_val38;
     ret_val37 = STR_ap1077157958(plus_self22, s5, TRUE);
     plus_self23 = ret_val37;
     plus_sarg17 = ((STR) &name131);
     ret_val41 = STR_ap2004550586(plus_self23, plus_sarg17);
     plus_self24 = ret_val41;
     mang_self3 = self;
     mang_ob3 = ((OB) ATTR(aod,arr));
     ret_val42 = MANGLE119219986(ATTR(mang_self3,mangler), mang_ob3, ((OB) NULL));
     plus_sarg18 = ret_val42;
     ret_val43 = STR_ap2004550586(plus_self24, plus_sarg18);
     plus_self25 = ret_val43;
     plus_sarg19 = ((STR) &S04);
     ret_val44 = STR_ap2004550586(plus_self25, plus_sarg19);
     res = ret_val44;
    }
    after_first = TRUE;
   }
  }
  after_loop1: ;
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

TP_CLASS CGEN_a1247402136(CGEN self, dTP tp) {
 TP_CLASS ret_val;
 TP_CLASS arr = ((TP_CLASS) NULL);
 dTP L1;
 INT L2;
 INT L3;
 BOOL L41_;
 dTP L5;
 IMPL L6;
 BOOL L7;
 BOOL L81_;
 L1 = tp;
 L2 = (*dTP_kindrINT[TAG(L1)])(L1);
 L3 = TP_KIND_ref_tp;
 L41_=(L2)==(L3); 
 if (L41_) {
  L5 = tp;
  L6=(*dTP_implrIMPL[TAG(L5)])(L5);
  arr = ATTR(L6,arr);
  L7 = (arr==((TP_CLASS) NULL));
  L81_=!(L7); 
  if (L81_) {
   ret_val = arr;
   return ret_val;
  }
 }
 ret_val = ((TP_CLASS) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_c777074314(CGEN self, AM_EXT_CALL_EXPR ext_call, ARRAYSTR arg_list, ARRAYSTR final_arg_list) {
 INT arg_pos = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 CGEN ndefer_self;
 STR ndefer_s;
 INT L41_;
 INT L5;
 INT L61_;
 INT L31_,L31_m;
 INT L7;
 AM_CALL_ARG L81_;
 AM_CALL_ARG L9;
 dMODE L10;
 STR L121_;
 STR L13;
 extern STR name33;
 STR L141_;
 STR L15;
 extern STR name16;
 INT L161_;
 arg_pos = 0;
 {
  BOOL f_L31_ = TRUE;
  L11 = 1;
  L41_=(ext_call)==NULL?0:ASIZE((AM_EXT_CALL_EXPR)ext_call); 
  L5 = L41_;
  L61_=INTMINUS(L5,1); 
  L21 = L61_;
  L31_=L11-1;L31_m=L21; 
  while (1) {
   if(L31_++>=L31_m)  goto after_loop; 
   i = L31_;
   L81_=(AM_CALL_ARG)ARR((AM_EXT_CALL_EXPR)ext_call,i); 
   L9=L81_;
   L10 = ATTR(L9,mode1);
   if ((*dMODE_2144735300[TAG(L10)])(L10)) {
    ndefer_self = self;
    L121_=(STR)ARR((ARRAYSTR)arg_list,arg_pos); 
    L13 = L121_;
    L141_=(STR)ARR((ARRAYSTR)final_arg_list,arg_pos); 
    L15 = L141_;
    ndefer_s = STR_ap1417971546(L13, ((STR) &name33), L15, ((STR) &name16));
    CGEN_d695831906(ndefer_self);
    SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
   }
   L161_=INTPLUS(arg_pos,1); 
   arg_pos = L161_;
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e1386576419(CGEN self, AM_EXT_CALL_EXPR ext_call, ARRAYSTR* arg_list, ARRAYSTR* type_list) {
 ARRAYSTR L1 = ((ARRAYSTR) NULL);/*Local for arg*/
 ARRAYSTR L2 = ((ARRAYSTR) NULL);/*Local for arg*/
 INT num_args = INT_zero;
 INT arg_pos = INT_zero;
 INT L31 = INT_zero;
 INT L41 = INT_zero;
 INT i = INT_zero;
 dTP tp;
 ARRAYSTR create_self;
 INT create_n = INT_zero;
 ARRAYSTR ret_val;
 ARRAYSTR create_self1;
 INT create_n1 = INT_zero;
 ARRAYSTR ret_val1;
 CGEN c_type_for_arg_s;
 dTP c_type_for_arg_t;
 STR ret_val2;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val3;
 INT L51_;
 INT L6;
 INT L71_;
 ARRAYSTR L8;
 INT L9;
 OB L10;
 INT L11;
 ARRAYSTR L12;
 INT L13;
 OB L14;
 INT L15;
 INT L161_;
 INT L17;
 BOOL L181_;
 INT L201_;
 INT L21;
 INT L221_;
 INT L191_,L191_m;
 INT L23;
 AM_CALL_ARG L241_;
 STR L25;
 AM_CALL_ARG L271_;
 AM_CALL_ARG L28;
 dAM_EXPR L29;
 STR L30;
 INT L331_;
 L51_=(ext_call)==NULL?0:ASIZE((AM_EXT_CALL_EXPR)ext_call); 
 L6 = L51_;
 L71_=INTMINUS(L6,1); 
 num_args = L71_;
 create_self = ((ARRAYSTR) NULL);
 create_n = num_args;
 L9 = create_n;
 L11=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L9)*sizeof(STR);
 L10=ZALLOC_BIG(L11);
 if (L10==NULL) FATAL("Unable to allocate more memory");
 ((OB)L10)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L10)->header.destroyed=0;
#endif

 L8 = ((ARRAYSTR) L10);
 L8->asize = L9;
 ret_val = L8;
 L1 = ret_val;
 create_self1 = ((ARRAYSTR) NULL);
 create_n1 = num_args;
 L13 = create_n1;
 L15=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L13)*sizeof(STR);
 L14=ZALLOC_BIG(L15);
 if (L14==NULL) FATAL("Unable to allocate more memory");
 ((OB)L14)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L14)->header.destroyed=0;
#endif

 L12 = ((ARRAYSTR) L14);
 L12->asize = L13;
 ret_val1 = L12;
 L2 = ret_val1;
 arg_pos = 0;
 L161_=ASIZE((AM_EXT_CALL_EXPR)ext_call); 
 L17 = L161_;
 L181_=(1)<(L17); 
 if (L181_) {
  {
   BOOL f_L191_ = TRUE;
   L31 = 1;
   L201_=ASIZE((AM_EXT_CALL_EXPR)ext_call); 
   L21 = L201_;
   L221_=INTMINUS(L21,1); 
   L41 = L221_;
   L191_=L31-1;L191_m=L41; 
   while (1) {
    if(L191_++>=L191_m)  goto after_loop; 
    i = L191_;
    L241_=(AM_CALL_ARG)ARR((AM_EXT_CALL_EXPR)ext_call,i); 
    L25 = CGEN_e1216665739(self, L241_);
    SARR((ARRAYSTR)L1,arg_pos,(STR)L25); 
    ;
    L271_=(AM_CALL_ARG)ARR((AM_EXT_CALL_EXPR)ext_call,i); 
    L28=L271_;
    L29 = ATTR(L28,expr);
    tp = (*dAM_EXPR_tprdTP[TAG(L29)])(L29);
    c_type_for_arg_s = self;
    c_type_for_arg_t = tp;
    mang_self = c_type_for_arg_s;
    mang_ob = ((OB) c_type_for_arg_t);
    ret_val3 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
    ret_val2 = ret_val3;
    L30 = ret_val2;
    SARR((ARRAYSTR)L2,arg_pos,(STR)L30); 
    ;
    L331_=INTPLUS(arg_pos,1); 
    arg_pos = L331_;
   }
  }
  after_loop: ;
 }
 *arg_list = L1;
 *type_list = L2;
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e986815307(CGEN self, AM_BND367211769 biter) {
 STR biter_name;
 AM_BND367211769 L11;
 INT i = INT_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 CGEN comment_self;
 STR comment_com;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val2;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN mang_self;
 OB mang_ob;
 OB mang_ns;
 STR ret_val3;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val4;
 CGEN barf_self;
 STR barf_msg;
 PROG barf_self1;
 STR barf_msg1;
 PROG barf_at_self;
 STR barf_at_msg;
 dPROG_ERR barf_at_at;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val5;
 UNIX exit_self;
 INT exit_code = INT_zero;
 INT L21 = INT_zero;
 STR plus_self5;
 INT plus_arg = INT_zero;
 STR ret_val6;
 STR s;
 INT str_self = INT_zero;
 STR ret_val7;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val8;
 FSTR str_self1;
 STR ret_val9;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val10;
 CGEN cast_self;
 dTP cast_dest_tp;
 dTP cast_src_tp;
 STR cast_expr;
 BOOL cast_only_boxing = BOOL_zero;
 STR ret_val11;
 STR plus_self7;
 STR plus_sarg6;
 STR ret_val12;
 CGEN ndefer_self1;
 STR ndefer_s1;
 extern STR Initia354244688;
 extern STR name54;
 extern STR name79;
 extern STR Unreco1110335422;
 extern STR Intern28965746;
 INT L31_br;
 INT rL31_;
 INT L4;
 BOOL L5;
 ARRAYBOOL L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 extern STR oncearg;
 BOOL L10;
 BOOL L121_;
 extern STR name36;
 ARRAYARG L13;
 ARG L141_;
 ARG L15;
 AM_CALL_ARG L161_;
 AM_CALL_ARG L17;
 dAM_EXPR L18;
 AM_CALL_ARG L191_;
 AM_CALL_ARG L20;
 extern STR name16;
 INT L221_;
 comment_self = self;
 plus_self = ((STR) &Initia354244688);
 plus_sarg = TP_ITER_strrSTR(ATTR(biter,bi_tp));
 ret_val = STR_ap2004550586(plus_self, plus_sarg);
 comment_com = ret_val;
 if (ATTR(comment_self,pretty)) {
  ndefer_self = comment_self;
  plus_self1 = ((STR) &name54);
  plus_sarg1 = comment_com;
  ret_val1 = STR_ap2004550586(plus_self1, plus_sarg1);
  plus_self2 = ret_val1;
  plus_sarg2 = ((STR) &name79);
  ret_val2 = STR_ap2004550586(plus_self2, plus_sarg2);
  ndefer_s = ret_val2;
  CGEN_d695831906(ndefer_self);
  SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
 }
 CGEN_e494460490(self, ATTR(biter,init));
 SATTR(biter,init,((dAM_STMT) NULL));
 mang_self = self;
 mang_ob = ((OB) biter);
 mang_ns = ((OB) ATTR(self,current_sig));
 ret_val3 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, mang_ns);
 biter_name = ret_val3;
 if ((biter_name==((STR) NULL))) {
  barf_self = self;
  plus_self3 = ((STR) &Unreco1110335422);
  plus_sarg3 = CGEN_e1639504572(self, ATTR(biter,bi));
  ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
  barf_msg = ret_val4;
  barf_self1 = ATTR(barf_self,prog);
  barf_msg1 = barf_msg;
  barf_at_self = barf_self1;
  barf_at_msg = barf_msg1;
  barf_at_at = ((dPROG_ERR) NULL);
  PROG_e176405615(barf_at_self, barf_at_at);
  plus_self4 = ((STR) &Intern28965746);
  plus_sarg4 = barf_at_msg;
  ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
  PROG_err_STR(barf_at_self, ret_val5);
  exit_self = ((UNIX) NULL);
  exit_code = 1;
  exit(exit_code);
 }
 {
  BOOL f_L31_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = biter;
  L31_br=L11==NULL?0:ASIZE((AM_BND367211769)L11); 
  while (1) {
   if(L21>=L31_br)  goto after_loop; 
   rL31_=L21; 
   i = rL31_;
   if ((ATTR(ATTR(biter,bi_tp),hot)==((ARRAYBOOL) NULL))) {
    L5 = TRUE;
   } else {
    L6 = ATTR(ATTR(biter,bi_tp),hot);
    L71_=ARR((ARRAYBOOL)L6,i); 
    L8 = L71_;
    L91_=!(L8); 
    L5 = L91_;
   }
   if (L5) {
    ndefer_self1 = self;
    plus_self5 = ((STR) &oncearg);
    plus_arg = i;
    str_self = plus_arg;
    clear_self = buf1;
    L10 = (clear_self==((FSTR) NULL));
    L121_=!(L10); 
    if (L121_) {
     SATTR(clear_self,loc,0);
    }
    str_in_self = str_self;
    str_in_s = buf1;
    ret_val8 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
    buf1 = ret_val8;
    str_self1 = buf1;
    ret_val9 = STR_fr1097270334(((STR) NULL), str_self1);
    ret_val7 = ret_val9;
    s = ret_val7;
    ret_val6 = STR_ap1077157958(plus_self5, s, TRUE);
    plus_self6 = ret_val6;
    plus_sarg5 = ((STR) &name36);
    ret_val10 = STR_ap2004550586(plus_self6, plus_sarg5);
    cast_self = self;
    L13 = ATTR(ATTR(biter,bi_tp),args);
    L141_=(ARG)ARR((ARRAYARG)L13,i); 
    L15=L141_;
    cast_dest_tp = ATTR(L15,tp);
    L161_=(AM_CALL_ARG)ARR((AM_BND367211769)biter,i); 
    L17=L161_;
    L18 = ATTR(L17,expr);
    cast_src_tp = (*dAM_EXPR_tprdTP[TAG(L18)])(L18);
    L191_=(AM_CALL_ARG)ARR((AM_BND367211769)biter,i); 
    L20=L191_;
    cast_expr = CGEN_e1639504572(self, ATTR(L20,expr));
    cast_only_boxing = FALSE;
    ret_val11 = CGEN_d1915593494(cast_self, cast_dest_tp, cast_src_tp, cast_expr, cast_only_boxing, ((dMODE) NULL));
    plus_self7 = ret_val11;
    plus_sarg6 = ((STR) &name16);
    ret_val12 = STR_ap2004550586(plus_self7, plus_sarg6);
    ndefer_s1 = STR_ap1693864410(biter_name, ret_val10, ret_val12);
    CGEN_d695831906(ndefer_self1);
    SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
   }
   L221_=INTPLUS(L21,1); 
   L21 = L221_;
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_f412018626(CGEN self, AM_EXT_CALL_EXPR ext_call, INT first_arg, ARRAYSTR arg_list, ARRAYSTR final_arg_list) {
 INT arg_pos = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 CGEN ndefer_self;
 STR ndefer_s;
 INT L41_;
 INT L5;
 INT L61_;
 INT L31_,L31_m;
 INT L7;
 AM_CALL_ARG L81_;
 AM_CALL_ARG L9;
 dMODE L10;
 STR L121_;
 STR L13;
 extern STR name33;
 STR L141_;
 STR L15;
 extern STR name16;
 INT L161_;
 arg_pos = first_arg;
 {
  BOOL f_L31_ = TRUE;
  L11 = 1;
  L41_=(ext_call)==NULL?0:ASIZE((AM_EXT_CALL_EXPR)ext_call); 
  L5 = L41_;
  L61_=INTMINUS(L5,1); 
  L21 = L61_;
  L31_=L11-1;L31_m=L21; 
  while (1) {
   if(L31_++>=L31_m)  goto after_loop; 
   i = L31_;
   L81_=(AM_CALL_ARG)ARR((AM_EXT_CALL_EXPR)ext_call,i); 
   L9=L81_;
   L10 = ATTR(L9,mode1);
   if ((*dMODE_2144735300[TAG(L10)])(L10)) {
    ndefer_self = self;
    L121_=(STR)ARR((ARRAYSTR)arg_list,arg_pos); 
    L13 = L121_;
    L141_=(STR)ARR((ARRAYSTR)final_arg_list,arg_pos); 
    L15 = L141_;
    ndefer_s = STR_ap1417971546(L13, ((STR) &name33), L15, ((STR) &name16));
    CGEN_d695831906(ndefer_self);
    SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
   }
   L161_=INTPLUS(arg_pos,1); 
   arg_pos = L161_;
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_p535532953(CGEN self, AM_EXT_CALL_EXPR ext_call, ARRAYSTR final_arg_list, INT first_arg, STR alt_ret) {
 INT case_num = INT_zero;
 INT arg_pos = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 AM_CALL_ARG ca;
 STR invoke_handler;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN in_self;
 INT str_self = INT_zero;
 STR ret_val;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val1;
 FSTR str_self1;
 STR ret_val2;
 CGEN ndefer_self1;
 STR ndefer_s1;
 CGEN move_out_self;
 CGEN ndefer_self2;
 STR ndefer_s2;
 extern STR switch1;
 extern STR name99;
 INT L3;
 INT L41_;
 INT L61_;
 INT L7;
 INT L81_;
 INT L51_,L51_m;
 INT L9;
 AM_CALL_ARG L101_;
 dAM_EXPR L12;
 dTP L13;
 dTP L14;
 extern STR F_HAND989235039;
 STR L151_;
 STR L16;
 extern STR name26;
 extern STR case3;
 BOOL L17;
 BOOL L181_;
 extern STR name20;
 extern STR break4;
 INT L191_;
 INT L201_;
 INT L22;
 INT L231_;
 extern STR name4;
 ndefer_self = self;
 ndefer_s = STR_ap1693864410(((STR) &switch1), alt_ret, ((STR) &name99));
 CGEN_d695831906(ndefer_self);
 SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
 case_num = 1;
 arg_pos = first_arg;
 in_self = self;
 L3 = ATTR(in_self,indent);
 L41_=INTPLUS(L3,1); 
 SATTR(in_self,indent,L41_);
 {
  BOOL f_L51_ = TRUE;
  L11 = 1;
  L61_=(ext_call)==NULL?0:ASIZE((AM_EXT_CALL_EXPR)ext_call); 
  L7 = L61_;
  L81_=INTMINUS(L7,1); 
  L21 = L81_;
  L51_=L11-1;L51_m=L21; 
  while (1) {
   if(L51_++>=L51_m)  goto after_loop; 
   i = L51_;
   L101_=(AM_CALL_ARG)ARR((AM_EXT_CALL_EXPR)ext_call,i); 
   ca = L101_;
   L12 = ATTR(ca,expr);
   L13 = (*dAM_EXPR_tprdTP[TAG(L12)])(L12);
   L14 = L13;
   if ((*dTP_is242312711[TAG(L14)])(L14, ((OB) TP_BUI1116976640))) {
    L151_=(STR)ARR((ARRAYSTR)final_arg_list,arg_pos); 
    L16 = L151_;
    invoke_handler = STR_ap1693864410(((STR) &F_HAND989235039), L16, ((STR) &name26));
    ndefer_self1 = self;
    str_self = case_num;
    clear_self = buf1;
    L17 = (clear_self==((FSTR) NULL));
    L181_=!(L17); 
    if (L181_) {
     SATTR(clear_self,loc,0);
    }
    str_in_self = str_self;
    str_in_s = buf1;
    ret_val1 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
    buf1 = ret_val1;
    str_self1 = buf1;
    ret_val2 = STR_fr1097270334(((STR) NULL), str_self1);
    ret_val = ret_val2;
    ndefer_s1 = STR_ap44172742(((STR) &case3), ret_val, ((STR) &name20), invoke_handler, ((STR) &break4));
    CGEN_d695831906(ndefer_self1);
    SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
    L191_=INTPLUS(case_num,1); 
    case_num = L191_;
   }
   L201_=INTPLUS(arg_pos,1); 
   arg_pos = L201_;
  }
 }
 after_loop: ;
 move_out_self = self;
 L22 = ATTR(move_out_self,indent);
 L231_=INTMINUS(L22,1); 
 SATTR(move_out_self,indent,L231_);
 ndefer_self2 = self;
 ndefer_s2 = ((STR) &name4);
 CGEN_d695831906(ndefer_self2);
 SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
}
