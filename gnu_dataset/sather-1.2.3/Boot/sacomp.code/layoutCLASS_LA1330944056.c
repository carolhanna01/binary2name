#include "sather.h"

/* Layouts */

typedef struct dAM_CALL_EXPR_struct {
 OB_HEADER header;
 } *dAM_CALL_EXPR;

typedef struct dLAYOUT_struct {
 OB_HEADER header;
 } *dLAYOUT;

typedef struct dMODE_struct {
 OB_HEADER header;
 } *dMODE;

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

typedef struct BOUND_2140373124_struct {/* layout for BOUND_ITER_TYPE_LAYOUT */
 OB_HEADER header;
 STR str;
 struct TP_ITER_struct *tp;
 } *BOUND_2140373124;

typedef struct BOUND_809390774_struct {/* layout for BOUND_TYPE_LAYOUT */
 OB_HEADER header;
 STR str;
 struct TP_ROUT_struct *tp;
 } *BOUND_809390774;

typedef struct BUILTIN_LAYOUT_struct {/* layout for BUILTIN_LAYOUT */
 OB_HEADER header;
 struct dTP_struct *tp;
 STR str;
 } *BUILTIN_LAYOUT;

typedef struct CLASS_LAYOUT_struct {/* layout for CLASS_LAYOUT */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct AM_OB_DEF_struct *l;
 struct FLISTdLAYOUT_struct *dependencies;
 struct PROG_struct *prog;
 STR str;
 } *CLASS_LAYOUT;

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

typedef struct ELT_NILIDENT_struct {/* layout for ELT_NIL{IDENT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILIDENT;

typedef struct EXTERNAL_LAYOUT_struct {/* layout for EXTERNAL_LAYOUT */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct AM_OB_DEF_struct *l;
 struct PROG_struct *prog;
 STR str;
 } *EXTERNAL_LAYOUT;

typedef struct FAST_IDENTIFIER_struct {/* layout for FAST_IDENTIFIER */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *FAST_IDENTIFIER;

typedef struct FILE1_struct {/* layout for FILE */
 OB_HEADER header;
 EXT_OB fp;
 } *FILE1;

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

typedef struct IMMUTA421759893_struct {/* layout for IMMUTABLE_CLASS_LAYOUT */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct AM_OB_DEF_struct *l;
 struct FLISTdLAYOUT_struct *dependencies;
 struct PROG_struct *prog;
 STR str;
 } *IMMUTA421759893;

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

typedef struct INT_upbrINT_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_I; /* used by builtin iter */
 } *INT_upbrINT_frame;

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

typedef struct OUT_struct {/* layout for OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OUT;

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

typedef struct STR_sp663650766_frame_struct {
 INT state;
 STR self;/* Formal argument: self */
 CHAR arg1;/* Formal argument: c */
 STR ret_val;
 INT cur_loc;
 INT next_loc;
 } *STR_sp663650766_frame;

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

typedef struct TP_LAYOUT_struct {/* layout for TP_LAYOUT */
 OB_HEADER header;
 struct dTP_struct *tp;
 STR str;
 } *TP_LAYOUT;

typedef struct TUPdTPdLAYOUT_struct {/* layout for TUP{$TP,$LAYOUT} */
 struct dLAYOUT_struct *t2;
 struct dTP_struct *t1;
 } TUPdTPdLAYOUT;
static TUPdTPdLAYOUT TUPdTPdLAYOUT_zero;

typedef struct TUPdTPdLAYOUT_boxed_struct {
 OB_HEADER header;
 TUPdTPdLAYOUT immutable_part;
 } *TUPdTPdLAYOUT_boxed;

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

typedef struct TUPSIG2023533247_struct {/* layout for TUP{SIG,FRAME_LAYOUT} */
 struct FRAME_LAYOUT_struct *t2;
 struct SIG_struct *t1;
 } TUPSIG2023533247;
static TUPSIG2023533247 TUPSIG2023533247_zero;

typedef struct TUPSIG2023533247_boxed_struct {
 OB_HEADER header;
 TUPSIG2023533247 immutable_part;
 } *TUPSIG2023533247_boxed;

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

typedef struct FLISTdLAYOUT_struct {/* layout for FLIST{$LAYOUT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dLAYOUT_struct *arr_part[1];
 } *FLISTdLAYOUT;

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

typedef struct FMAPdTPdLAYOUT_struct {/* layout for FMAP{$TP,$LAYOUT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdTPdLAYOUT_struct arr_part[1];
 } *FMAPdTPdLAYOUT;

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

typedef struct FMAPID1741388334_frame_struct {
 INT state;
 FMAPIDENTdTP self;/* Formal argument: self */
 TUPIDENTdTP ret_val2;
 FMAPIDENTdTP L61;
 TUPIDENTdTP r;
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
 } *FMAPID1741388334_frame;

typedef struct FMAPID944928219_frame_struct {
 INT state;
 FMAPIDENTdTP self;/* Formal argument: self */
 dTP ret_val2;
 FMAPIDENTdTP L61;
 TUPIDENTdTP e;
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
 } *FMAPID944928219_frame;

typedef struct FMAPSI1121373188_struct {/* layout for FMAP{SIG,FRAME_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSIG2023533247_struct arr_part[1];
 } *FMAPSI1121373188;

typedef struct FSETCLASS_LAYOUT_struct {/* layout for FSET{CLASS_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct CLASS_LAYOUT_struct *arr_part[1];
 } *FSETCLASS_LAYOUT;

typedef struct FSETEX1260947371_struct {/* layout for FSET{EXTERNAL_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct EXTERNAL_LAYOUT_struct *arr_part[1];
 } *FSETEX1260947371;

typedef struct FSETIM184436961_struct {/* layout for FSET{IMMUTABLE_CLASS_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct IMMUTA421759893_struct *arr_part[1];
 } *FSETIM184436961;

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

typedef struct LAYOUT_ARR_struct {/* layout for LAYOUT_ARR */
 OB_HEADER header;
 INT asize;
 struct TUPIDENTdTP_struct arr_part[1];
 } *LAYOUT_ARR;

typedef struct LAYOUT1415846778_frame_struct {
 INT state;
 LAYOUT_ARR self;/* Formal argument: self */
 TUPIDENTdTP ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *LAYOUT1415846778_frame;

typedef struct LAYOUT2137691532_frame_struct {
 INT state;
 LAYOUT_ARR self;/* Formal argument: self */
 TUPIDENTdTP arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *LAYOUT2137691532_frame;

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

extern CGEN LAYOUT_TBL_cgen;
extern FMAPSI1121373188 FRAME_1643038396;
extern FMAPdTPdLAYOUT LAYOUT1191461457;
extern FSETCLASS_LAYOUT CLASS_368803185;
extern FSETEX1260947371 EXTERN1555805875;
extern FSETIM184436961 IMMUTA489824193;
extern FSTR FAST_I1322544971;
extern IDENT IDENT_1137952589;
extern INOUT_MODE MODES_inout_mode;
extern INT TP_KIND_iter_tp;
extern INT TP_KIND_rout_tp;
extern OUT_MODE MODES_out_mode;
extern PROG LAYOUT_TBL_prog;
extern TP_CLASS TP_BUI1813155122;
extern TP_CLASS TP_BUILTIN_bool;
extern TP_CLASS TP_BUILTIN_char;

/* Function declarations */


extern RETURNED_CONST BOOL (**dMODE_814247358)(dMODE, dMODE);

extern RETURNED_CONST BOOL (**dTP_is124163553)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1334578382)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1811059018)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1999456142)(dTP, dTP);

extern RETURNED_CONST BOOL (**dTP_is242312711)(dTP, OB);

extern RETURNED_CONST BOOL (**dTP_is735947028)(dTP);

extern RETURNED_CONST BOOL (**dTP_is914808564)(dTP);

extern RETURNED_CONST BOOL (**dTP_is_crBOOL)(dTP);

extern RETURNED_CONST IMPL (**dTP_implrIMPL)(dTP);

extern RETURNED_CONST INT (**dTP_kindrINT)(dTP);

extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);
ABSTRACT_LAYOUT ABSTRA762331980(ABSTRACT_LAYOUT, dTP, PROG);
AM_BND367211769 FLISTA1446310868(FLISTA1446310868_frame);
AM_FORMAL_ARG AM_ROU948739923(AM_ROU948739923_frame);
AM_ITE1809135850 FLISTA20621028(FLISTA20621028_frame);
AM_LOCAL_EXPR FLISTA2119642552(FLISTA2119642552_frame);
AM_OB_DEF CGEN_a1221397173(CGEN, dTP);
BOOL SIG_is418491101(SIG);
BOUND_2140373124 BOUND_166620816(BOUND_2140373124, dTP);
BOUND_809390774 BOUND_848335287(BOUND_809390774, dTP);
BUILTIN_LAYOUT BUILTI1036809386(BUILTIN_LAYOUT, dTP);
CLASS_LAYOUT CLASS_1959884515(CLASS_LAYOUT, dTP, PROG);
EXTERNAL_LAYOUT EXTERN1518354897(EXTERNAL_LAYOUT, dTP);
FLISTA1062334999 FMAPAM925529692(FMAPAM1787628573, AM_ROUT_DEF);
FLISTA1409846210 FMAPAM338804820(FMAPAM1244483504, AM_ROUT_DEF);
FLISTdLAYOUT CLASS_1345471732(CLASS_LAYOUT);
FLISTdLAYOUT EXTERN1033411318(EXTERNAL_LAYOUT);
FLISTdLAYOUT FLISTd1429133189(FLISTdLAYOUT, dLAYOUT);
FLISTdLAYOUT FRAME_31536271(FRAME_LAYOUT);
FLISTdLAYOUT IMMUTA414538126(IMMUTA421759893);
FLISTdLAYOUT TP_LAY265125546(TP_LAYOUT);
FMAPSI1121373188 FMAPSI1358355632(FMAPSI1121373188, SIG, FRAME_LAYOUT);
FMAPdTPdLAYOUT FMAPdT1248247834(FMAPdTPdLAYOUT, dTP, dLAYOUT);
FRAME_LAYOUT FMAPSI547426166(FMAPSI1121373188, SIG);
FRAME_LAYOUT FRAME_248172754(FRAME_LAYOUT, AM_ROUT_DEF, PROG);
FSETCLASS_LAYOUT FSETCL686728372(FSETCLASS_LAYOUT, CLASS_LAYOUT);
FSETEX1260947371 FSETEX1746916700(FSETEX1260947371, EXTERNAL_LAYOUT);
FSETIM184436961 FSETIM1706711326(FSETIM184436961, IMMUTA421759893);
FSETSTR FSETST1404644833(FSETSTR, STR);
FSTR FSTR_c1998740379(FSTR, INT);
FSTR FSTR_p399773021(FSTR, CHAR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
IMMUTA421759893 IMMUTA431464701(IMMUTA421759893, dTP, PROG);
INT CHAR_a641490070(CHAR);
INT FMAPID99794443(FMAPIDENTdTP);
INT FSTR_sizerINT(FSTR);
INT INT_upbrINT(INT_upbrINT_frame);
INT STR_sizerINT(STR);
LAYOUT_ARR CLASS_1559721113(CLASS_LAYOUT, FMAPIDENTdTP);
LAYOUT_ARR IMMUTA877143907(IMMUTA421759893, FMAPIDENTdTP);
STR ARRAYS1578977701(ARRAYS1578977701_frame);
STR CGEN_e1606764211(CGEN, STR, dAM_CALL_EXPR, SIG, ARRAYSTR, STR, STR, STR);
STR CLASS_1824950980(CLASS_LAYOUT);
STR CLASS_954167052(CLASS_LAYOUT, CGEN);
STR EXTERN1283013626(EXTERNAL_LAYOUT);
STR EXTERN1472641871(EXTERNAL_LAYOUT, CGEN);
STR FAST_I535598365(FAST_IDENTIFIER);
STR FRAME_1195781105(FRAME_LAYOUT);
STR FRAME_1898279078(FRAME_LAYOUT, AM_FORMAL_ARG, STR);
STR FRAME_691068581(FRAME_LAYOUT, CGEN);
STR IMMUTA2093463840(IMMUTA421759893, CGEN);
STR IMMUTA966323602(IMMUTA421759893);
STR IMPL_s709672763(IMPL, IDENT);
STR MANGLE119219986(MANGLE, OB, OB);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap1417971546(STR, STR, STR, STR);
STR STR_ap1693864410(STR, STR, STR);
STR STR_ap2004550586(STR, STR);
STR STR_fr1097270334(STR, FSTR);
STR STR_head_INTrSTR(STR, INT);
STR STR_sp663650766(STR_sp663650766_frame);
STR TP_LAY473390218(TP_LAYOUT);
STR TP_LAY690809295(TP_LAYOUT, CGEN);
TUPIDENTdTP FMAPID1741388334(FMAPID1741388334_frame);
TUPIDENTdTP LAYOUT1415846778(LAYOUT1415846778_frame);
dLAYOUT FMAPdT7482853(FMAPdTPdLAYOUT, dTP);
dLAYOUT LAYOUT79932797(LAYOUT_TBL, dTP);
dTP FMAPID944928219(FMAPID944928219_frame);
void CLASS_1727096581(CLASS_LAYOUT);
void FILE_plus_STR(FILE1, STR);
void IMMUTA343315329(IMMUTA421759893);
void LAYOUT1075979363(LAYOUT_ARR, INT, INT);
void LAYOUT2137691532(LAYOUT2137691532_frame);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

CLASS_LAYOUT CLASS_1959884515(CLASS_LAYOUT self, dTP tp, PROG prog) {
 CLASS_LAYOUT ret_val;
 dLAYOUT r;
 CLASS_LAYOUT res = ((CLASS_LAYOUT) NULL);
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 BOOL L1;
 BOOL L21_;
 CLASS_LAYOUT L3;
 OB L4;
 dTP L5;
 extern STR Class;
 r = FMAPdT7482853(LAYOUT1191461457, tp);
 L1 = (r==((dLAYOUT) NULL));
 L21_=!(L1); 
 if (L21_) {
  switch (TAG(r)) {
   case CLASS_LAYOUT_tag:
    res = ((CLASS_LAYOUT) r); break;
   default: ;
    FATAL("No applicable type in typecase\nin CLASS_LAYOUT::create($TP,PROG):SAME\n./Back/layout.sa:413:17");
  }
 }
 else {
  L4=ZALLOC(sizeof(struct CLASS_LAYOUT_struct));
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=CLASS_LAYOUT_tag;
  L3 = ((CLASS_LAYOUT) L4);
  res = L3;
  SATTR(res,tp,tp);
  SATTR(res,prog,prog);
  SATTR(res,l,CGEN_a1221397173(LAYOUT_TBL_cgen, tp));
  L5 = ATTR(res,tp);
  plus_self = (*dTP_strrSTR[TAG(L5)])(L5);
  plus_sarg = ((STR) &Class);
  ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
  SATTR(res,str,ret_val1);
  CLASS_368803185 = FSETCL686728372(CLASS_368803185, res);
  LAYOUT1191461457 = FMAPdT1248247834(LAYOUT1191461457, tp, ((dLAYOUT) res));
  CLASS_1727096581(res);
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

EXTERNAL_LAYOUT EXTERN1518354897(EXTERNAL_LAYOUT self, dTP tp) {
 EXTERNAL_LAYOUT ret_val;
 dLAYOUT r;
 EXTERNAL_LAYOUT res = ((EXTERNAL_LAYOUT) NULL);
 EXTERNAL_LAYOUT makestr_self;
 STR ret_val1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 BOOL L1;
 BOOL L21_;
 EXTERNAL_LAYOUT L3;
 OB L4;
 dTP L5;
 extern STR External;
 r = FMAPdT7482853(LAYOUT1191461457, tp);
 L1 = (r==((dLAYOUT) NULL));
 L21_=!(L1); 
 if (L21_) {
  switch (TAG(r)) {
   case EXTERNAL_LAYOUT_tag:
    res = ((EXTERNAL_LAYOUT) r); break;
   default: ;
    FATAL("No applicable type in typecase\nin EXTERNAL_LAYOUT::create($TP):SAME\n./Back/layout.sa:83:17");
  }
 }
 else {
  L4=ZALLOC(sizeof(struct EXTERNAL_LAYOUT_struct));
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=EXTERNAL_LAYOUT_tag;
  L3 = ((EXTERNAL_LAYOUT) L4);
  res = L3;
  SATTR(res,tp,tp);
  makestr_self = res;
  L5 = ATTR(makestr_self,tp);
  plus_self = (*dTP_strrSTR[TAG(L5)])(L5);
  plus_sarg = ((STR) &External);
  ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
  ret_val1 = ret_val2;
  SATTR(res,str,ret_val1);
  EXTERN1555805875 = FSETEX1746916700(EXTERN1555805875, res);
  LAYOUT1191461457 = FMAPdT1248247834(LAYOUT1191461457, tp, ((dLAYOUT) res));
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTdLAYOUT CLASS_1345471732(CLASS_LAYOUT self) {
 return ATTR(self,dependencies);
}


#undef IS_ITER
#define IS_ITER 0

FLISTdLAYOUT EXTERN1033411318(EXTERNAL_LAYOUT self) {
 FLISTdLAYOUT ret_val;
 FLISTdLAYOUT create_self;
 FLISTdLAYOUT ret_val1;
 create_self = ((FLISTdLAYOUT) NULL);
 ret_val1 = ((FLISTdLAYOUT) NULL);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTdLAYOUT FRAME_31536271(FRAME_LAYOUT self) {
 FLISTdLAYOUT ret_val;
 FLISTdLAYOUT res;
 AM_ROUT_DEF L11;
 FLISTA725283029 L21;
 FLISTdLAYOUT create_self;
 FLISTdLAYOUT ret_val1;
 INT L31 = INT_zero;
 AM_FORMAL_ARG tp_self;
 dTP ret_val2;
 AM_LOCAL_EXPR tp_self1;
 dTP ret_val3;
 AM_LOCAL_EXPR tp_self2;
 dTP ret_val4;
 INT L41_br;
 AM_FORMAL_ARG aL41_;
 AM_FORMAL_ARG L5;
 INT L61_;
 AM_LOCAL_EXPR L7;
 create_self = ((FLISTdLAYOUT) NULL);
 ret_val1 = ((FLISTdLAYOUT) NULL);
 res = ret_val1;
 {
  BOOL f_L41_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = ATTR(self,f);
  L41_br=L11==NULL?0:ASIZE((AM_ROUT_DEF)L11); 
  while (1) {
   if(L31>=L41_br)  goto after_loop; 
   aL41_=ARR((AM_ROUT_DEF)L11,L31); 
   tp_self = aL41_;
   tp_self1 = ATTR(tp_self,expr);
   ret_val3 = ATTR(tp_self1,tp_at);
   ret_val2 = ret_val3;
   res = FLISTd1429133189(res, LAYOUT79932797(((LAYOUT_TBL) NULL), ret_val2));
   L61_=INTPLUS(L31,1); 
   L31 = L61_;
  }
 }
 after_loop: ;
 {
  struct FLISTA2119642552_frame_struct other2_0;
FLISTA2119642552_frame noname1 = &other2_0;
  L21 = ATTR(ATTR(self,f),locals1);
  noname1->self = L21;
  noname1->state = 0;
  while (1) {
   L7 = FLISTA2119642552(noname1);
   if (noname1->state == -1) {
    goto after_loop1;
   }
   tp_self2 = L7;
   ret_val4 = ATTR(tp_self2,tp_at);
   res = FLISTd1429133189(res, LAYOUT79932797(((LAYOUT_TBL) NULL), ret_val4));
  }
 }
 after_loop1: ;
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTdLAYOUT IMMUTA414538126(IMMUTA421759893 self) {
 return ATTR(self,dependencies);
}


#undef IS_ITER
#define IS_ITER 0

FLISTdLAYOUT TP_LAY265125546(TP_LAYOUT self) {
 FLISTdLAYOUT ret_val;
 FLISTdLAYOUT create_self;
 FLISTdLAYOUT ret_val1;
 create_self = ((FLISTdLAYOUT) NULL);
 ret_val1 = ((FLISTdLAYOUT) NULL);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FRAME_LAYOUT FRAME_248172754(FRAME_LAYOUT self, AM_ROUT_DEF f, PROG p) {
 FRAME_LAYOUT ret_val;
 FRAME_LAYOUT res;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 FRAME_LAYOUT L1;
 OB L2;
 extern STR frame2;
 res = FMAPSI547426166(FRAME_1643038396, ATTR(f,sig1));
 if ((res==((FRAME_LAYOUT) NULL))) {
  L2=ZALLOC(sizeof(struct FRAME_LAYOUT_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FRAME_LAYOUT_tag;
  L1 = ((FRAME_LAYOUT) L2);
  res = L1;
  SATTR(res,f,f);
  SATTR(res,prog,p);
  plus_self = ATTR(ATTR(ATTR(res,f),sig1),str);
  plus_sarg = ((STR) &frame2);
  ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
  SATTR(res,str,ret_val1);
  FRAME_1643038396 = FMAPSI1358355632(FRAME_1643038396, ATTR(f,sig1), res);
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

IMMUTA421759893 IMMUTA431464701(IMMUTA421759893 self, dTP tp, PROG prog) {
 IMMUTA421759893 ret_val;
 IMMUTA421759893 res = ((IMMUTA421759893) NULL);
 dLAYOUT r;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 BOOL L1;
 BOOL L21_;
 IMMUTA421759893 L3;
 OB L4;
 dTP L5;
 extern STR immutable3;
 r = FMAPdT7482853(LAYOUT1191461457, tp);
 L1 = (r==((dLAYOUT) NULL));
 L21_=!(L1); 
 if (L21_) {
  switch (TAG(r)) {
   case IMMUTA421759893_tag:
    res = ((IMMUTA421759893) r); break;
   default: ;
    FATAL("No applicable type in typecase\nin IMMUTABLE_CLASS_LAYOUT::create($TP,PROG):SAME\n./Back/layout.sa:503:17");
  }
 }
 else {
  L4=ZALLOC(sizeof(struct IMMUTA421759893_struct));
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=IMMUTA421759893_tag;
  L3 = ((IMMUTA421759893) L4);
  res = L3;
  SATTR(res,tp,tp);
  SATTR(res,prog,prog);
  SATTR(res,l,CGEN_a1221397173(LAYOUT_TBL_cgen, tp));
  L5 = ATTR(res,tp);
  plus_self = (*dTP_strrSTR[TAG(L5)])(L5);
  plus_sarg = ((STR) &immutable3);
  ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
  SATTR(res,str,ret_val1);
  IMMUTA489824193 = FSETIM1706711326(IMMUTA489824193, res);
  LAYOUT1191461457 = FMAPdT1248247834(LAYOUT1191461457, tp, ((dLAYOUT) res));
  IMMUTA343315329(res);
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

LAYOUT_ARR CLASS_1559721113(CLASS_LAYOUT self, FMAPIDENTdTP map) {
 LAYOUT_ARR ret_val;
 LAYOUT_ARR res;
 FMAPIDENTdTP L11;
 LAYOUT_ARR L21;
 LAYOUT_ARR create_self;
 INT create_n = INT_zero;
 LAYOUT_ARR ret_val1;
 INT L31 = INT_zero;
 LAYOUT_ARR sort_self;
 LAYOUT_ARR L4;
 INT L5;
 OB L6;
 INT L7;
 INT L81_br;
 TUPIDENTdTP L9;
 TUPIDENTdTP L10;
 INT L121_;
 BOOL L13;
 BOOL L141_;
 INT L151_;
 INT L16;
 INT L171_;
 create_self = ((LAYOUT_ARR) NULL);
 create_n = FMAPID99794443(map);
 L5 = create_n;
 L7=(sizeof(struct LAYOUT_ARR_struct)-sizeof(TUPIDENTdTP))+(L5)*sizeof(TUPIDENTdTP);
 L6=ZALLOC_BIG(L7);
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=LAYOUT_ARR_tag;
#ifdef DESTROY_CHK

   ((OB)L6)->header.destroyed=0;
#endif

 L4 = ((LAYOUT_ARR) L6);
 L4->asize = L5;
 ret_val1 = L4;
 res = ret_val1;
 {
  struct FMAPID1741388334_frame_struct other1_0;
FMAPID1741388334_frame noname1 = &other1_0;
  BOOL f_L81_ = TRUE;
  L11 = map;
  /* loop index variable */
  L31 = 0;
  L21 = res;
  noname1->self = L11;
  noname1->state = 0;
  L81_br=L21==NULL?0:ASIZE((LAYOUT_ARR)L21); 
  while (1) {
   if(L31>=L81_br)  goto after_loop; 
   L9 = FMAPID1741388334(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   L10 = L9;
   SARR((LAYOUT_ARR)L21,L31,L10); 
   ;
   L121_=INTPLUS(L31,1); 
   L31 = L121_;
  }
 }
 after_loop: ;
 sort_self = res;
 L13 = (sort_self==((LAYOUT_ARR) NULL));
 L141_=!(L13); 
 if (L141_) {
  L151_=ASIZE((LAYOUT_ARR)sort_self); 
  L16 = L151_;
  L171_=INTMINUS(L16,1); 
  LAYOUT1075979363(sort_self, 0, L171_);
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

LAYOUT_ARR IMMUTA877143907(IMMUTA421759893 self, FMAPIDENTdTP map) {
 LAYOUT_ARR ret_val;
 LAYOUT_ARR res;
 FMAPIDENTdTP L11;
 LAYOUT_ARR L21;
 LAYOUT_ARR create_self;
 INT create_n = INT_zero;
 LAYOUT_ARR ret_val1;
 INT L31 = INT_zero;
 LAYOUT_ARR sort_self;
 LAYOUT_ARR L4;
 INT L5;
 OB L6;
 INT L7;
 INT L81_br;
 TUPIDENTdTP L9;
 TUPIDENTdTP L10;
 INT L121_;
 BOOL L13;
 BOOL L141_;
 INT L151_;
 INT L16;
 INT L171_;
 create_self = ((LAYOUT_ARR) NULL);
 create_n = FMAPID99794443(map);
 L5 = create_n;
 L7=(sizeof(struct LAYOUT_ARR_struct)-sizeof(TUPIDENTdTP))+(L5)*sizeof(TUPIDENTdTP);
 L6=ZALLOC_BIG(L7);
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=LAYOUT_ARR_tag;
#ifdef DESTROY_CHK

   ((OB)L6)->header.destroyed=0;
#endif

 L4 = ((LAYOUT_ARR) L6);
 L4->asize = L5;
 ret_val1 = L4;
 res = ret_val1;
 {
  struct FMAPID1741388334_frame_struct other1_0;
FMAPID1741388334_frame noname1 = &other1_0;
  BOOL f_L81_ = TRUE;
  L11 = map;
  /* loop index variable */
  L31 = 0;
  L21 = res;
  noname1->self = L11;
  noname1->state = 0;
  L81_br=L21==NULL?0:ASIZE((LAYOUT_ARR)L21); 
  while (1) {
   if(L31>=L81_br)  goto after_loop; 
   L9 = FMAPID1741388334(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   L10 = L9;
   SARR((LAYOUT_ARR)L21,L31,L10); 
   ;
   L121_=INTPLUS(L31,1); 
   L31 = L121_;
  }
 }
 after_loop: ;
 sort_self = res;
 L13 = (sort_self==((LAYOUT_ARR) NULL));
 L141_=!(L13); 
 if (L141_) {
  L151_=ASIZE((LAYOUT_ARR)sort_self); 
  L16 = L151_;
  L171_=INTMINUS(L16,1); 
  LAYOUT1075979363(sort_self, 0, L171_);
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CLASS_1824950980(CLASS_LAYOUT self) {
 return ATTR(self,str);
}


#undef IS_ITER
#define IS_ITER 0

STR CLASS_954167052(CLASS_LAYOUT self, CGEN c) {
 STR ret_val;
 MANGLE m;
 STR cname;
 STR res;
 LAYOUT_ARR L11;
 TUPIDENTdTP p = TUPIDENTdTP_zero;
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
 MANGLE forbid_self;
 STR forbid_s;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val7;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val8;
 INT L21 = INT_zero;
 STR plus_self8;
 CHAR plus_arg = CHAR_zero;
 STR ret_val9;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val10;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val11;
 STR r;
 STR plus_self9;
 STR plus_sarg8;
 STR ret_val12;
 STR plus_self10;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val13;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val14;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val15;
 STR r1;
 STR plus_self11;
 STR plus_sarg9;
 STR ret_val16;
 STR plus_self12;
 STR plus_sarg10;
 STR ret_val17;
 STR plus_self13;
 STR plus_sarg11;
 STR ret_val18;
 STR plus_self14;
 STR plus_sarg12;
 STR ret_val19;
 STR plus_self15;
 STR plus_sarg13;
 STR ret_val20;
 STR plus_self16;
 STR plus_sarg14;
 STR ret_val21;
 STR plus_self17;
 STR plus_sarg15;
 STR ret_val22;
 STR plus_self18;
 STR plus_sarg16;
 STR ret_val23;
 STR plus_self19;
 STR plus_sarg17;
 STR ret_val24;
 STR plus_self20;
 STR plus_sarg18;
 STR ret_val25;
 STR plus_self21;
 STR plus_sarg19;
 STR ret_val26;
 STR plus_self22;
 STR plus_sarg20;
 STR ret_val27;
 STR plus_self23;
 STR plus_sarg21;
 STR ret_val28;
 STR plus_self24;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val29;
 STR s2;
 CHAR str_self2 = CHAR_zero;
 STR ret_val30;
 STR create_self2;
 CHAR create_c2 = CHAR_zero;
 STR ret_val31;
 STR r2;
 STR plus_self25;
 STR plus_sarg22;
 STR ret_val32;
 STR plus_self26;
 STR plus_sarg23;
 STR ret_val33;
 STR plus_self27;
 STR plus_sarg24;
 STR ret_val34;
 STR plus_self28;
 STR plus_sarg25;
 STR ret_val35;
 STR plus_self29;
 STR plus_sarg26;
 STR ret_val36;
 STR plus_self30;
 STR plus_sarg27;
 STR ret_val37;
 STR plus_self31;
 STR plus_sarg28;
 STR ret_val38;
 STR plus_self32;
 STR plus_sarg29;
 STR ret_val39;
 STR plus_self33;
 INT plus_arg3 = INT_zero;
 STR ret_val40;
 STR s3;
 INT str_self3 = INT_zero;
 STR ret_val41;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val42;
 FSTR str_self4;
 STR ret_val43;
 STR plus_self34;
 STR plus_sarg30;
 STR ret_val44;
 STR plus_self35;
 STR plus_sarg31;
 STR ret_val45;
 STR plus_self36;
 STR plus_sarg32;
 STR ret_val46;
 STR plus_self37;
 STR plus_sarg33;
 STR ret_val47;
 STR plus_self38;
 STR plus_sarg34;
 STR ret_val48;
 extern STR typedefstruct;
 extern STR S_struct5;
 extern STR layoutfor;
 dTP L3;
 extern STR name55;
 extern STR S_struct2;
 extern STR OB_HEADERheader;
 dTP L4;
 extern STR LOCK_H1410610176;
 BOOL L5;
 BOOL L61_;
 TUPIDENTdTP L8;
 INT L71_br;
 TUPIDENTdTP aL71_;
 dTP L9;
 STR L10;
 INT L12;
 OB L13;
 INT L14;
 STR L16;
 INT L17;
 OB L18;
 INT L19;
 OB L22;
 OB L23;
 extern STR name13;
 dTP L24;
 BOOL L25;
 BOOL L261_;
 extern STR struct3;
 extern STR S_struct4;
 OB L27;
 OB L28;
 extern STR name13;
 extern STR struct3;
 extern STR S_struct1;
 OB L29;
 OB L30;
 extern STR name13;
 INT L311_;
 BOOL L32;
 BOOL L331_;
 extern STR INTasize;
 dTP L34;
 STR L35;
 INT L36;
 OB L37;
 INT L38;
 extern STR arr_part6;
 dTP L40;
 BOOL L41;
 BOOL L421_;
 extern STR struct3;
 extern STR S_structarr_part;
 extern STR struct3;
 extern STR S_structarr_part1;
 INT L43;
 INT L441_;
 BOOL L45;
 BOOL L461_;
 extern STR name188;
 BOOL L47;
 extern STR CHARfi1162829196;
 extern STR name183;
 extern STR name185;
 m = ATTR(c,mangler);
 cname = MANGLE119219986(m, ((OB) ATTR(ATTR(self,l),tp)), ((OB) NULL));
 plus_self = ((STR) &typedefstruct);
 plus_sarg = cname;
 ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
 plus_self1 = ret_val1;
 plus_sarg1 = ((STR) &S_struct5);
 ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
 res = ret_val2;
 plus_self2 = res;
 plus_sarg2 = ((STR) &layoutfor);
 ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
 plus_self3 = ret_val3;
 L3 = ATTR(ATTR(self,l),tp);
 plus_sarg3 = (*dTP_strrSTR[TAG(L3)])(L3);
 ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
 plus_self4 = ret_val4;
 plus_sarg4 = ((STR) &name55);
 ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
 res = ret_val5;
 forbid_self = m;
 plus_self5 = MANGLE119219986(m, ((OB) ATTR(self,tp)), ((OB) NULL));
 plus_sarg5 = ((STR) &S_struct2);
 ret_val6 = STR_ap2004550586(plus_self5, plus_sarg5);
 forbid_s = ret_val6;
 SATTR(forbid_self,forbidden,FSETST1404644833(ATTR(forbid_self,forbidden), forbid_s));
 plus_self6 = res;
 plus_sarg6 = ((STR) &OB_HEADERheader);
 ret_val7 = STR_ap2004550586(plus_self6, plus_sarg6);
 res = ret_val7;
 L4 = ATTR(self,tp);
 if ((*dTP_is1999456142[TAG(L4)])(L4, ((dTP) TP_BUI1813155122))) {
  plus_self7 = res;
  plus_sarg7 = ((STR) &LOCK_H1410610176);
  ret_val8 = STR_ap2004550586(plus_self7, plus_sarg7);
  res = ret_val8;
 }
 L5 = (ATTR(ATTR(self,l),at)==((FMAPIDENTdTP) NULL));
 L61_=!(L5); 
 if (L61_) {
  {
   BOOL f_L71_ = TRUE;
   /* loop index variable */
   L21 = 0;
   while (1) {
    if (f_L71_) {
     f_L71_ = FALSE;
     L11 = CLASS_1559721113(self, ATTR(ATTR(self,l),at));
     L71_br=L11==NULL?0:ASIZE((LAYOUT_ARR)L11); 
    }
    if(L21>=L71_br)  goto after_loop; 
    aL71_=ARR((LAYOUT_ARR)L11,L21); 
    p = aL71_;
    L9 = p.t2;
    if ((*dTP_is914808564[TAG(L9)])(L9)) {
     plus_self8 = res;
     plus_arg = ' ';
     str_self = plus_arg;
     create_self = ((STR) NULL);
     create_c = str_self;
     L12 = 1;
     L14=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L12)*sizeof(CHAR);
     L13=ZALLOC_LEAF_BIG(L14);
     if (L13==NULL) FATAL("Unable to allocate more memory");
     memset(L13,0,L14);
     ((OB)L13)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L13)->header.destroyed=0;
#endif

     L10 = ((STR) L13);
     L10->asize = L12;
     r = L10;
     SARR((STR)r,0,create_c); 
     ;
     ret_val11 = r;
     ret_val10 = ret_val11;
     s = ret_val10;
     ret_val9 = STR_ap1077157958(plus_self8, s, TRUE);
     plus_self9 = ret_val9;
     plus_sarg8 = MANGLE119219986(m, ((OB) p.t2), ((OB) NULL));
     ret_val12 = STR_ap2004550586(plus_self9, plus_sarg8);
     plus_self10 = ret_val12;
     plus_arg1 = ' ';
     str_self1 = plus_arg1;
     create_self1 = ((STR) NULL);
     create_c1 = str_self1;
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
     r1 = L16;
     SARR((STR)r1,0,create_c1); 
     ;
     ret_val15 = r1;
     ret_val14 = ret_val15;
     s1 = ret_val14;
     ret_val13 = STR_ap1077157958(plus_self10, s1, TRUE);
     plus_self11 = ret_val13;
     L23=ZALLOC(sizeof(struct IDENT_boxed_struct));
     if (L23==NULL) FATAL("Unable to allocate more memory");
     ((OB)L23)->header.tag=IDENT_tag;
     L22 = (OB)((IDENT_boxed) L23);
     ((IDENT_boxed) L22)->immutable_part = p.t1;
     plus_sarg9 = MANGLE119219986(m, L22, ((OB) ATTR(ATTR(self,l),tp)));
     ret_val16 = STR_ap2004550586(plus_self11, plus_sarg9);
     plus_self12 = ret_val16;
     plus_sarg10 = ((STR) &name13);
     ret_val17 = STR_ap2004550586(plus_self12, plus_sarg10);
     res = ret_val17;
    }
    else {
     L24 = p.t2;
     L25 = (*dTP_is1334578382[TAG(L24)])(L24);
     L261_=!(L25); 
     if (L261_) {
      plus_self13 = res;
      plus_sarg11 = ((STR) &struct3);
      ret_val18 = STR_ap2004550586(plus_self13, plus_sarg11);
      plus_self14 = ret_val18;
      plus_sarg12 = MANGLE119219986(m, ((OB) p.t2), ((OB) NULL));
      ret_val19 = STR_ap2004550586(plus_self14, plus_sarg12);
      plus_self15 = ret_val19;
      plus_sarg13 = ((STR) &S_struct4);
      ret_val20 = STR_ap2004550586(plus_self15, plus_sarg13);
      plus_self16 = ret_val20;
      L28=ZALLOC(sizeof(struct IDENT_boxed_struct));
      if (L28==NULL) FATAL("Unable to allocate more memory");
      ((OB)L28)->header.tag=IDENT_tag;
      L27 = (OB)((IDENT_boxed) L28);
      ((IDENT_boxed) L27)->immutable_part = p.t1;
      plus_sarg14 = MANGLE119219986(m, L27, ((OB) ATTR(ATTR(self,l),tp)));
      ret_val21 = STR_ap2004550586(plus_self16, plus_sarg14);
      plus_self17 = ret_val21;
      plus_sarg15 = ((STR) &name13);
      ret_val22 = STR_ap2004550586(plus_self17, plus_sarg15);
      res = ret_val22;
     }
     else {
      plus_self18 = res;
      plus_sarg16 = ((STR) &struct3);
      ret_val23 = STR_ap2004550586(plus_self18, plus_sarg16);
      plus_self19 = ret_val23;
      plus_sarg17 = MANGLE119219986(m, ((OB) p.t2), ((OB) NULL));
      ret_val24 = STR_ap2004550586(plus_self19, plus_sarg17);
      plus_self20 = ret_val24;
      plus_sarg18 = ((STR) &S_struct1);
      ret_val25 = STR_ap2004550586(plus_self20, plus_sarg18);
      plus_self21 = ret_val25;
      L30=ZALLOC(sizeof(struct IDENT_boxed_struct));
      if (L30==NULL) FATAL("Unable to allocate more memory");
      ((OB)L30)->header.tag=IDENT_tag;
      L29 = (OB)((IDENT_boxed) L30);
      ((IDENT_boxed) L29)->immutable_part = p.t1;
      plus_sarg19 = MANGLE119219986(m, L29, ((OB) ATTR(ATTR(self,l),tp)));
      ret_val26 = STR_ap2004550586(plus_self21, plus_sarg19);
      plus_self22 = ret_val26;
      plus_sarg20 = ((STR) &name13);
      ret_val27 = STR_ap2004550586(plus_self22, plus_sarg20);
      res = ret_val27;
     }
    }
    L311_=INTPLUS(L21,1); 
    L21 = L311_;
   }
  }
  after_loop: ;
 }
 L32 = (ATTR(ATTR(self,l),arr)==((dTP) NULL));
 L331_=!(L32); 
 if (L331_) {
  plus_self23 = res;
  plus_sarg21 = ((STR) &INTasize);
  ret_val28 = STR_ap2004550586(plus_self23, plus_sarg21);
  res = ret_val28;
  L34 = ATTR(ATTR(self,l),arr);
  if ((*dTP_is914808564[TAG(L34)])(L34)) {
   plus_self24 = res;
   plus_arg2 = ' ';
   str_self2 = plus_arg2;
   create_self2 = ((STR) NULL);
   create_c2 = str_self2;
   L36 = 1;
   L38=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L36)*sizeof(CHAR);
   L37=ZALLOC_LEAF_BIG(L38);
   if (L37==NULL) FATAL("Unable to allocate more memory");
   memset(L37,0,L38);
   ((OB)L37)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L37)->header.destroyed=0;
#endif

   L35 = ((STR) L37);
   L35->asize = L36;
   r2 = L35;
   SARR((STR)r2,0,create_c2); 
   ;
   ret_val31 = r2;
   ret_val30 = ret_val31;
   s2 = ret_val30;
   ret_val29 = STR_ap1077157958(plus_self24, s2, TRUE);
   plus_self25 = ret_val29;
   plus_sarg22 = MANGLE119219986(m, ((OB) ATTR(ATTR(self,l),arr)), ((OB) NULL));
   ret_val32 = STR_ap2004550586(plus_self25, plus_sarg22);
   plus_self26 = ret_val32;
   plus_sarg23 = ((STR) &arr_part6);
   ret_val33 = STR_ap2004550586(plus_self26, plus_sarg23);
   res = ret_val33;
  }
  else {
   L40 = ATTR(ATTR(self,l),arr);
   L41 = (*dTP_is1334578382[TAG(L40)])(L40);
   L421_=!(L41); 
   if (L421_) {
    plus_self27 = res;
    plus_sarg24 = ((STR) &struct3);
    ret_val34 = STR_ap2004550586(plus_self27, plus_sarg24);
    plus_self28 = ret_val34;
    plus_sarg25 = MANGLE119219986(m, ((OB) ATTR(ATTR(self,l),arr)), ((OB) NULL));
    ret_val35 = STR_ap2004550586(plus_self28, plus_sarg25);
    plus_self29 = ret_val35;
    plus_sarg26 = ((STR) &S_structarr_part);
    ret_val36 = STR_ap2004550586(plus_self29, plus_sarg26);
    res = ret_val36;
   }
   else {
    plus_self30 = res;
    plus_sarg27 = ((STR) &struct3);
    ret_val37 = STR_ap2004550586(plus_self30, plus_sarg27);
    plus_self31 = ret_val37;
    plus_sarg28 = MANGLE119219986(m, ((OB) ATTR(ATTR(self,l),arr)), ((OB) NULL));
    ret_val38 = STR_ap2004550586(plus_self31, plus_sarg28);
    plus_self32 = ret_val38;
    plus_sarg29 = ((STR) &S_structarr_part1);
    ret_val39 = STR_ap2004550586(plus_self32, plus_sarg29);
    res = ret_val39;
   }
  }
  plus_self33 = res;
  L43 = ATTR(ATTR(self,l),asize);
  L441_=(1>L43)?1:L43; 
  plus_arg3 = L441_;
  str_self3 = plus_arg3;
  clear_self = buf1;
  L45 = (clear_self==((FSTR) NULL));
  L461_=!(L45); 
  if (L461_) {
   SATTR(clear_self,loc,0);
  }
  str_in_self = str_self3;
  str_in_s = buf1;
  ret_val42 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
  buf1 = ret_val42;
  str_self4 = buf1;
  ret_val43 = STR_fr1097270334(((STR) NULL), str_self4);
  ret_val41 = ret_val43;
  s3 = ret_val41;
  ret_val40 = STR_ap1077157958(plus_self33, s3, TRUE);
  plus_self34 = ret_val40;
  plus_sarg30 = ((STR) &name188);
  ret_val44 = STR_ap2004550586(plus_self34, plus_sarg30);
  res = ret_val44;
 }
 if ((ATTR(ATTR(self,l),at)==((FMAPIDENTdTP) NULL))) {
  L47 = (ATTR(ATTR(self,l),arr)==((dTP) NULL));
 } else {
  L47 = FALSE;
 }
 if (L47) {
  plus_self35 = res;
  plus_sarg31 = ((STR) &CHARfi1162829196);
  ret_val45 = STR_ap2004550586(plus_self35, plus_sarg31);
  res = ret_val45;
 }
 plus_self36 = res;
 plus_sarg32 = ((STR) &name183);
 ret_val46 = STR_ap2004550586(plus_self36, plus_sarg32);
 plus_self37 = ret_val46;
 plus_sarg33 = cname;
 ret_val47 = STR_ap2004550586(plus_self37, plus_sarg33);
 plus_self38 = ret_val47;
 plus_sarg34 = ((STR) &name185);
 ret_val48 = STR_ap2004550586(plus_self38, plus_sarg34);
 res = ret_val48;
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR EXTERN1283013626(EXTERNAL_LAYOUT self) {
 return ATTR(self,str);
}


#undef IS_ITER
#define IS_ITER 0

STR EXTERN1472641871(EXTERNAL_LAYOUT self, CGEN c) {
 STR ret_val;
 STR res;
 STR header_list;
 STR header1;
 STR L11;
 CHAR L21 = CHAR_zero;
 MANGLE m;
 STR cname;
 FMAPIDENTdTP L31;
 TUPIDENTdTP p = TUPIDENTdTP_zero;
 IMPL C_header_val_sel;
 STR ret_val1;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val2;
 STR r;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val3;
 STR r1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val4;
 STR plus_self1;
 STR plus_sarg1;
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
 MANGLE forbid_self;
 STR forbid_s;
 STR plus_self6;
 CHAR plus_arg = CHAR_zero;
 STR ret_val10;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val11;
 STR create_self2;
 CHAR create_c2 = CHAR_zero;
 STR ret_val12;
 STR r2;
 STR plus_self7;
 STR plus_sarg6;
 STR ret_val13;
 STR plus_self8;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val14;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val15;
 STR create_self3;
 CHAR create_c3 = CHAR_zero;
 STR ret_val16;
 STR r3;
 STR plus_self9;
 STR plus_sarg7;
 STR ret_val17;
 STR plus_self10;
 STR plus_sarg8;
 STR ret_val18;
 STR plus_self11;
 STR plus_sarg9;
 STR ret_val19;
 STR plus_self12;
 STR plus_sarg10;
 STR ret_val20;
 STR plus_self13;
 STR plus_sarg11;
 STR ret_val21;
 STR plus_self14;
 STR plus_sarg12;
 STR ret_val22;
 extern STR name1;
 dTP L4;
 dTP L5;
 BOOL L6;
 BOOL L71_;
 STR L8;
 INT L9;
 INT L101_;
 INT L12;
 CHAR L131_;
 CHAR L14;
 BOOL L151_;
 INT L16;
 INT L171_;
 CHAR L181_;
 CHAR L19;
 BOOL L201_;
 BOOL L22;
 BOOL L231_;
 STR L24;
 INT L25;
 OB L26;
 INT L27;
 STR L29;
 INT L30;
 OB L32;
 INT L33;
 extern STR include5;
 extern STR name9;
 extern STR typedefstruct;
 extern STR S_struct5;
 extern STR layoutfor;
 dTP L35;
 extern STR name55;
 extern STR S_struct2;
 BOOL L36;
 BOOL L371_;
 TUPIDENTdTP L38;
 STR L39;
 INT L40;
 OB L41;
 INT L42;
 STR L44;
 INT L45;
 OB L46;
 INT L47;
 OB L49;
 OB L50;
 extern STR name13;
 extern STR CHARfi1162829196;
 extern STR name183;
 extern STR name185;
 res = ((STR) &name1);
 L4 = ATTR(self,tp);
 if ((*dTP_is_crBOOL[TAG(L4)])(L4)) {
  L5 = ATTR(self,tp);
  C_header_val_sel = (*dTP_implrIMPL[TAG(L5)])(L5);
  ret_val1 = IMPL_s709672763(C_header_val_sel, IDENT_1137952589);
  header_list = ret_val1;
  L6 = (header_list==((STR) NULL));
  L71_=!(L6); 
  if (L71_) {
   {
    struct STR_sp663650766_frame_struct other1_0;
STR_sp663650766_frame noname1 = &other1_0;
    L11 = header_list;
    L21 = ' ';
    noname1->self = L11;
    noname1->arg1 = L21;
    noname1->state = 0;
    while (1) {
     L8 = STR_sp663650766(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     header1 = L8;
     L9 = STR_sizerINT(header1);
     L101_=INTMINUS(L9,1); 
     L12 = L101_;
     L131_=ARR((STR)header1,L12); 
     L14 = L131_;
     L151_=L14==' '; 
     if (L151_) {
      L16 = STR_sizerINT(header1);
      L171_=INTMINUS(L16,1); 
      header1 = STR_head_INTrSTR(header1, L171_);
     }
     L181_=ARR((STR)header1,0); 
     L19 = L181_;
     L201_=L19=='<'; 
     L22 = L201_;
     L231_=!(L22); 
     if (L231_) {
      create_self = ((STR) NULL);
      create_c = '\"';
      L25 = 1;
      L27=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L25)*sizeof(CHAR);
      L26=ZALLOC_LEAF_BIG(L27);
      if (L26==NULL) FATAL("Unable to allocate more memory");
      memset(L26,0,L27);
      ((OB)L26)->header.tag=STR_tag;
#ifdef DESTROY_CHK

        ((OB)L26)->header.destroyed=0;
#endif

      L24 = ((STR) L26);
      L24->asize = L25;
      r = L24;
      SARR((STR)r,0,create_c); 
      ;
      ret_val2 = r;
      create_self1 = ((STR) NULL);
      create_c1 = '\"';
      L30 = 1;
      L33=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L30)*sizeof(CHAR);
      L32=ZALLOC_LEAF_BIG(L33);
      if (L32==NULL) FATAL("Unable to allocate more memory");
      memset(L32,0,L33);
      ((OB)L32)->header.tag=STR_tag;
#ifdef DESTROY_CHK

        ((OB)L32)->header.destroyed=0;
#endif

      L29 = ((STR) L32);
      L29->asize = L30;
      r1 = L29;
      SARR((STR)r1,0,create_c1); 
      ;
      ret_val3 = r1;
      header1 = STR_ap1693864410(ret_val2, header1, ret_val3);
     }
     res = STR_ap1417971546(res, ((STR) &include5), header1, ((STR) &name9));
    }
   }
   after_loop: ;
  }
  else {
   m = ATTR(c,mangler);
   cname = MANGLE119219986(m, ((OB) ATTR(self,tp)), ((OB) NULL));
   plus_self = ((STR) &typedefstruct);
   plus_sarg = cname;
   ret_val4 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val4;
   plus_sarg1 = ((STR) &S_struct5);
   ret_val5 = STR_ap2004550586(plus_self1, plus_sarg1);
   res = ret_val5;
   plus_self2 = res;
   plus_sarg2 = ((STR) &layoutfor);
   ret_val6 = STR_ap2004550586(plus_self2, plus_sarg2);
   plus_self3 = ret_val6;
   L35 = ATTR(self,tp);
   plus_sarg3 = (*dTP_strrSTR[TAG(L35)])(L35);
   ret_val7 = STR_ap2004550586(plus_self3, plus_sarg3);
   plus_self4 = ret_val7;
   plus_sarg4 = ((STR) &name55);
   ret_val8 = STR_ap2004550586(plus_self4, plus_sarg4);
   res = ret_val8;
   forbid_self = m;
   plus_self5 = MANGLE119219986(m, ((OB) ATTR(self,tp)), ((OB) NULL));
   plus_sarg5 = ((STR) &S_struct2);
   ret_val9 = STR_ap2004550586(plus_self5, plus_sarg5);
   forbid_s = ret_val9;
   SATTR(forbid_self,forbidden,FSETST1404644833(ATTR(forbid_self,forbidden), forbid_s));
   L36 = (ATTR(ATTR(self,l),at)==((FMAPIDENTdTP) NULL));
   L371_=!(L36); 
   if (L371_) {
    {
     struct FMAPID1741388334_frame_struct other2_0;
FMAPID1741388334_frame noname2 = &other2_0;
     L31 = ATTR(ATTR(self,l),at);
     noname2->self = L31;
     noname2->state = 0;
     while (1) {
      L38 = FMAPID1741388334(noname2);
      if (noname2->state == -1) {
       goto after_loop1;
      }
      p = L38;
      plus_self6 = res;
      plus_arg = ' ';
      str_self = plus_arg;
      create_self2 = ((STR) NULL);
      create_c2 = str_self;
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
      SARR((STR)r2,0,create_c2); 
      ;
      ret_val12 = r2;
      ret_val11 = ret_val12;
      s = ret_val11;
      ret_val10 = STR_ap1077157958(plus_self6, s, TRUE);
      plus_self7 = ret_val10;
      plus_sarg6 = MANGLE119219986(m, ((OB) p.t2), ((OB) NULL));
      ret_val13 = STR_ap2004550586(plus_self7, plus_sarg6);
      plus_self8 = ret_val13;
      plus_arg1 = ' ';
      str_self1 = plus_arg1;
      create_self3 = ((STR) NULL);
      create_c3 = str_self1;
      L45 = 1;
      L47=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L45)*sizeof(CHAR);
      L46=ZALLOC_LEAF_BIG(L47);
      if (L46==NULL) FATAL("Unable to allocate more memory");
      memset(L46,0,L47);
      ((OB)L46)->header.tag=STR_tag;
#ifdef DESTROY_CHK

        ((OB)L46)->header.destroyed=0;
#endif

      L44 = ((STR) L46);
      L44->asize = L45;
      r3 = L44;
      SARR((STR)r3,0,create_c3); 
      ;
      ret_val16 = r3;
      ret_val15 = ret_val16;
      s1 = ret_val15;
      ret_val14 = STR_ap1077157958(plus_self8, s1, TRUE);
      plus_self9 = ret_val14;
      L50=ZALLOC(sizeof(struct IDENT_boxed_struct));
      if (L50==NULL) FATAL("Unable to allocate more memory");
      ((OB)L50)->header.tag=IDENT_tag;
      L49 = (OB)((IDENT_boxed) L50);
      ((IDENT_boxed) L49)->immutable_part = p.t1;
      plus_sarg7 = MANGLE119219986(m, L49, ((OB) ATTR(ATTR(self,l),tp)));
      ret_val17 = STR_ap2004550586(plus_self9, plus_sarg7);
      plus_self10 = ret_val17;
      plus_sarg8 = ((STR) &name13);
      ret_val18 = STR_ap2004550586(plus_self10, plus_sarg8);
      res = ret_val18;
     }
    }
    after_loop1: ;
   }
   if ((ATTR(ATTR(self,l),at)==((FMAPIDENTdTP) NULL))) {
    plus_self11 = res;
    plus_sarg9 = ((STR) &CHARfi1162829196);
    ret_val19 = STR_ap2004550586(plus_self11, plus_sarg9);
    res = ret_val19;
   }
   plus_self12 = res;
   plus_sarg10 = ((STR) &name183);
   ret_val20 = STR_ap2004550586(plus_self12, plus_sarg10);
   plus_self13 = ret_val20;
   plus_sarg11 = cname;
   ret_val21 = STR_ap2004550586(plus_self13, plus_sarg11);
   plus_self14 = ret_val21;
   plus_sarg12 = ((STR) &name185);
   ret_val22 = STR_ap2004550586(plus_self14, plus_sarg12);
   res = ret_val22;
  }
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR FAST_I535598365(FAST_IDENTIFIER self) {
 STR ret_val;
 INT L11 = INT_zero;
 INT d = INT_zero;
 CHAR L21 = CHAR_zero;
 FSTR aget_self;
 INT aget_ind = INT_zero;
 CHAR ret_val1 = CHAR_zero;
 FSTR aget_self1;
 INT aget_ind1 = INT_zero;
 CHAR ret_val2 = CHAR_zero;
 CHAR from_ascii_int_s = CHAR_zero;
 INT from_ascii_int_i = INT_zero;
 CHAR ret_val3 = CHAR_zero;
 FSTR aset_self;
 INT aset_ind = INT_zero;
 CHAR aset_val = CHAR_zero;
 FSTR aget_self2;
 INT aget_ind2 = INT_zero;
 CHAR ret_val4 = CHAR_zero;
 CHAR from_ascii_int_s1 = CHAR_zero;
 INT from_ascii_int_i1 = INT_zero;
 CHAR ret_val5 = CHAR_zero;
 FSTR aset_self1;
 INT aset_ind1 = INT_zero;
 CHAR aset_val1 = CHAR_zero;
 FSTR aget_self3;
 INT aget_ind3 = INT_zero;
 CHAR ret_val6 = CHAR_zero;
 CHAR from_ascii_int_s2 = CHAR_zero;
 INT from_ascii_int_i2 = INT_zero;
 CHAR ret_val7 = CHAR_zero;
 FSTR aset_self2;
 INT aset_ind2 = INT_zero;
 CHAR aset_val2 = CHAR_zero;
 FSTR aget_self4;
 INT aget_ind4 = INT_zero;
 CHAR ret_val8 = CHAR_zero;
 CHAR from_ascii_int_s3 = CHAR_zero;
 INT from_ascii_int_i3 = INT_zero;
 CHAR ret_val9 = CHAR_zero;
 FSTR aset_self3;
 INT aset_ind3 = INT_zero;
 CHAR aset_val3 = CHAR_zero;
 FSTR aget_self5;
 INT aget_ind5 = INT_zero;
 CHAR ret_val10 = CHAR_zero;
 CHAR from_ascii_int_s4 = CHAR_zero;
 INT from_ascii_int_i4 = INT_zero;
 CHAR ret_val11 = CHAR_zero;
 FSTR aset_self4;
 INT aset_ind4 = INT_zero;
 CHAR aset_val4 = CHAR_zero;
 FSTR aget_self6;
 INT aget_ind6 = INT_zero;
 CHAR ret_val12 = CHAR_zero;
 CHAR from_ascii_int_s5 = CHAR_zero;
 INT from_ascii_int_i5 = INT_zero;
 CHAR ret_val13 = CHAR_zero;
 FSTR aset_self5;
 INT aset_ind5 = INT_zero;
 CHAR aset_val5 = CHAR_zero;
 FSTR aget_self7;
 INT aget_ind7 = INT_zero;
 CHAR ret_val14 = CHAR_zero;
 CHAR from_ascii_int_s6 = CHAR_zero;
 INT from_ascii_int_i6 = INT_zero;
 CHAR ret_val15 = CHAR_zero;
 FSTR aset_self6;
 INT aset_ind6 = INT_zero;
 CHAR aset_val6 = CHAR_zero;
 FSTR aget_self8;
 INT aget_ind8 = INT_zero;
 CHAR ret_val16 = CHAR_zero;
 CHAR from_ascii_int_s7 = CHAR_zero;
 INT from_ascii_int_i7 = INT_zero;
 CHAR ret_val17 = CHAR_zero;
 FSTR aset_self7;
 INT aset_ind7 = INT_zero;
 CHAR aset_val7 = CHAR_zero;
 FSTR aget_self9;
 INT aget_ind9 = INT_zero;
 CHAR ret_val18 = CHAR_zero;
 CHAR from_ascii_int_s8 = CHAR_zero;
 INT from_ascii_int_i8 = INT_zero;
 CHAR ret_val19 = CHAR_zero;
 FSTR aset_self8;
 INT aset_ind8 = INT_zero;
 CHAR aset_val8 = CHAR_zero;
 FSTR aget_self10;
 INT aget_ind10 = INT_zero;
 CHAR ret_val20 = CHAR_zero;
 CHAR from_ascii_int_s9 = CHAR_zero;
 INT from_ascii_int_i9 = INT_zero;
 CHAR ret_val21 = CHAR_zero;
 FSTR aset_self9;
 INT aset_ind9 = INT_zero;
 CHAR aset_val9 = CHAR_zero;
 FSTR aget_self11;
 INT aget_ind11 = INT_zero;
 CHAR ret_val22 = CHAR_zero;
 CHAR from_ascii_int_s10 = CHAR_zero;
 INT from_ascii_int_i10 = INT_zero;
 CHAR ret_val23 = CHAR_zero;
 FSTR aset_self10;
 INT aset_ind10 = INT_zero;
 CHAR aset_val10 = CHAR_zero;
 FSTR aget_self12;
 INT aget_ind12 = INT_zero;
 CHAR ret_val24 = CHAR_zero;
 CHAR from_ascii_int_s11 = CHAR_zero;
 INT from_ascii_int_i11 = INT_zero;
 CHAR ret_val25 = CHAR_zero;
 FSTR aset_self11;
 INT aset_ind11 = INT_zero;
 CHAR aset_val11 = CHAR_zero;
 FSTR aget_self13;
 INT aget_ind13 = INT_zero;
 CHAR ret_val26 = CHAR_zero;
 CHAR from_ascii_int_s12 = CHAR_zero;
 INT from_ascii_int_i12 = INT_zero;
 CHAR ret_val27 = CHAR_zero;
 FSTR aset_self12;
 INT aset_ind12 = INT_zero;
 CHAR aset_val12 = CHAR_zero;
 FSTR aget_self14;
 INT aget_ind14 = INT_zero;
 CHAR ret_val28 = CHAR_zero;
 CHAR from_ascii_int_s13 = CHAR_zero;
 INT from_ascii_int_i13 = INT_zero;
 CHAR ret_val29 = CHAR_zero;
 FSTR aset_self13;
 INT aset_ind13 = INT_zero;
 CHAR aset_val13 = CHAR_zero;
 FSTR aget_self15;
 INT aget_ind15 = INT_zero;
 CHAR ret_val30 = CHAR_zero;
 CHAR from_ascii_int_s14 = CHAR_zero;
 INT from_ascii_int_i14 = INT_zero;
 CHAR ret_val31 = CHAR_zero;
 FSTR aset_self14;
 INT aset_ind14 = INT_zero;
 CHAR aset_val14 = CHAR_zero;
 FSTR aget_self16;
 INT aget_ind16 = INT_zero;
 CHAR ret_val32 = CHAR_zero;
 CHAR from_ascii_int_s15 = CHAR_zero;
 INT from_ascii_int_i15 = INT_zero;
 CHAR ret_val33 = CHAR_zero;
 FSTR aset_self15;
 INT aset_ind15 = INT_zero;
 CHAR aset_val15 = CHAR_zero;
 FSTR aget_self17;
 INT aget_ind17 = INT_zero;
 CHAR ret_val34 = CHAR_zero;
 CHAR from_ascii_int_s16 = CHAR_zero;
 INT from_ascii_int_i16 = INT_zero;
 CHAR ret_val35 = CHAR_zero;
 FSTR aset_self16;
 INT aset_ind16 = INT_zero;
 CHAR aset_val16 = CHAR_zero;
 FSTR aget_self18;
 INT aget_ind18 = INT_zero;
 CHAR ret_val36 = CHAR_zero;
 CHAR from_ascii_int_s17 = CHAR_zero;
 INT from_ascii_int_i17 = INT_zero;
 CHAR ret_val37 = CHAR_zero;
 FSTR aset_self17;
 INT aset_ind17 = INT_zero;
 CHAR aset_val17 = CHAR_zero;
 FSTR aget_self19;
 INT aget_ind19 = INT_zero;
 CHAR ret_val38 = CHAR_zero;
 CHAR from_ascii_int_s18 = CHAR_zero;
 INT from_ascii_int_i18 = INT_zero;
 CHAR ret_val39 = CHAR_zero;
 FSTR aset_self18;
 INT aset_ind18 = INT_zero;
 CHAR aset_val18 = CHAR_zero;
 FSTR aget_self20;
 INT aget_ind20 = INT_zero;
 CHAR ret_val40 = CHAR_zero;
 CHAR from_ascii_int_s19 = CHAR_zero;
 INT from_ascii_int_i19 = INT_zero;
 CHAR ret_val41 = CHAR_zero;
 FSTR aset_self19;
 INT aset_ind19 = INT_zero;
 CHAR aset_val19 = CHAR_zero;
 FSTR aget_self21;
 INT aget_ind21 = INT_zero;
 CHAR ret_val42 = CHAR_zero;
 CHAR from_ascii_int_s20 = CHAR_zero;
 INT from_ascii_int_i20 = INT_zero;
 CHAR ret_val43 = CHAR_zero;
 FSTR aset_self20;
 INT aset_ind20 = INT_zero;
 CHAR aset_val20 = CHAR_zero;
 FSTR aget_self22;
 INT aget_ind22 = INT_zero;
 CHAR ret_val44 = CHAR_zero;
 CHAR from_ascii_int_s21 = CHAR_zero;
 INT from_ascii_int_i21 = INT_zero;
 CHAR ret_val45 = CHAR_zero;
 FSTR aset_self21;
 INT aset_ind21 = INT_zero;
 CHAR aset_val21 = CHAR_zero;
 FSTR aget_self23;
 INT aget_ind23 = INT_zero;
 CHAR ret_val46 = CHAR_zero;
 CHAR from_ascii_int_s22 = CHAR_zero;
 INT from_ascii_int_i22 = INT_zero;
 CHAR ret_val47 = CHAR_zero;
 FSTR aset_self22;
 INT aset_ind22 = INT_zero;
 CHAR aset_val22 = CHAR_zero;
 FSTR aget_self24;
 INT aget_ind24 = INT_zero;
 CHAR ret_val48 = CHAR_zero;
 CHAR from_ascii_int_s23 = CHAR_zero;
 INT from_ascii_int_i23 = INT_zero;
 CHAR ret_val49 = CHAR_zero;
 FSTR aset_self23;
 INT aset_ind23 = INT_zero;
 CHAR aset_val23 = CHAR_zero;
 FSTR aget_self25;
 INT aget_ind25 = INT_zero;
 CHAR ret_val50 = CHAR_zero;
 CHAR from_ascii_int_s24 = CHAR_zero;
 INT from_ascii_int_i24 = INT_zero;
 CHAR ret_val51 = CHAR_zero;
 FSTR aset_self24;
 INT aset_ind24 = INT_zero;
 CHAR aset_val24 = CHAR_zero;
 FSTR aget_self26;
 INT aget_ind26 = INT_zero;
 CHAR ret_val52 = CHAR_zero;
 CHAR from_ascii_int_s25 = CHAR_zero;
 INT from_ascii_int_i25 = INT_zero;
 CHAR ret_val53 = CHAR_zero;
 FSTR aset_self25;
 INT aset_ind25 = INT_zero;
 CHAR aset_val25 = CHAR_zero;
 FSTR aget_self27;
 INT aget_ind27 = INT_zero;
 CHAR ret_val54 = CHAR_zero;
 CHAR from_ascii_int_s26 = CHAR_zero;
 INT from_ascii_int_i26 = INT_zero;
 CHAR ret_val55 = CHAR_zero;
 FSTR aset_self26;
 INT aset_ind26 = INT_zero;
 CHAR aset_val26 = CHAR_zero;
 FSTR aget_self28;
 INT aget_ind28 = INT_zero;
 CHAR ret_val56 = CHAR_zero;
 CHAR from_ascii_int_s27 = CHAR_zero;
 INT from_ascii_int_i27 = INT_zero;
 CHAR ret_val57 = CHAR_zero;
 FSTR aset_self27;
 INT aset_ind27 = INT_zero;
 CHAR aset_val27 = CHAR_zero;
 FSTR aget_self29;
 INT aget_ind29 = INT_zero;
 CHAR ret_val58 = CHAR_zero;
 CHAR from_ascii_int_s28 = CHAR_zero;
 INT from_ascii_int_i28 = INT_zero;
 CHAR ret_val59 = CHAR_zero;
 FSTR aset_self28;
 INT aset_ind28 = INT_zero;
 CHAR aset_val28 = CHAR_zero;
 FSTR aget_self30;
 INT aget_ind30 = INT_zero;
 CHAR ret_val60 = CHAR_zero;
 CHAR from_ascii_int_s29 = CHAR_zero;
 INT from_ascii_int_i29 = INT_zero;
 CHAR ret_val61 = CHAR_zero;
 FSTR aset_self29;
 INT aset_ind29 = INT_zero;
 CHAR aset_val29 = CHAR_zero;
 FSTR aget_self31;
 INT aget_ind31 = INT_zero;
 CHAR ret_val62 = CHAR_zero;
 CHAR from_ascii_int_s30 = CHAR_zero;
 INT from_ascii_int_i30 = INT_zero;
 CHAR ret_val63 = CHAR_zero;
 FSTR aset_self30;
 INT aset_ind30 = INT_zero;
 CHAR aset_val30 = CHAR_zero;
 FSTR aget_self32;
 INT aget_ind32 = INT_zero;
 CHAR ret_val64 = CHAR_zero;
 CHAR from_ascii_int_s31 = CHAR_zero;
 INT from_ascii_int_i31 = INT_zero;
 CHAR ret_val65 = CHAR_zero;
 FSTR aset_self31;
 INT aset_ind31 = INT_zero;
 CHAR aset_val31 = CHAR_zero;
 FSTR aget_self33;
 INT aget_ind33 = INT_zero;
 CHAR ret_val66 = CHAR_zero;
 CHAR from_ascii_int_s32 = CHAR_zero;
 INT from_ascii_int_i32 = INT_zero;
 CHAR ret_val67 = CHAR_zero;
 FSTR aset_self32;
 INT aset_ind32 = INT_zero;
 CHAR aset_val32 = CHAR_zero;
 FSTR aget_self34;
 INT aget_ind34 = INT_zero;
 CHAR ret_val68 = CHAR_zero;
 CHAR from_ascii_int_s33 = CHAR_zero;
 INT from_ascii_int_i33 = INT_zero;
 CHAR ret_val69 = CHAR_zero;
 FSTR aset_self33;
 INT aset_ind33 = INT_zero;
 CHAR aset_val33 = CHAR_zero;
 FSTR aget_self35;
 INT aget_ind35 = INT_zero;
 CHAR ret_val70 = CHAR_zero;
 CHAR from_ascii_int_s34 = CHAR_zero;
 INT from_ascii_int_i34 = INT_zero;
 CHAR ret_val71 = CHAR_zero;
 FSTR aset_self34;
 INT aset_ind34 = INT_zero;
 CHAR aset_val34 = CHAR_zero;
 FSTR aget_self36;
 INT aget_ind36 = INT_zero;
 CHAR ret_val72 = CHAR_zero;
 CHAR from_ascii_int_s35 = CHAR_zero;
 INT from_ascii_int_i35 = INT_zero;
 CHAR ret_val73 = CHAR_zero;
 FSTR aset_self35;
 INT aset_ind35 = INT_zero;
 CHAR aset_val35 = CHAR_zero;
 FSTR aget_self37;
 INT aget_ind37 = INT_zero;
 CHAR ret_val74 = CHAR_zero;
 CHAR from_ascii_int_s36 = CHAR_zero;
 INT from_ascii_int_i36 = INT_zero;
 CHAR ret_val75 = CHAR_zero;
 FSTR aset_self36;
 INT aset_ind36 = INT_zero;
 CHAR aset_val36 = CHAR_zero;
 FSTR aget_self38;
 INT aget_ind38 = INT_zero;
 CHAR ret_val76 = CHAR_zero;
 CHAR from_ascii_int_s37 = CHAR_zero;
 INT from_ascii_int_i37 = INT_zero;
 CHAR ret_val77 = CHAR_zero;
 FSTR aset_self37;
 INT aset_ind37 = INT_zero;
 CHAR aset_val37 = CHAR_zero;
 FSTR aget_self39;
 INT aget_ind39 = INT_zero;
 CHAR ret_val78 = CHAR_zero;
 CHAR from_ascii_int_s38 = CHAR_zero;
 INT from_ascii_int_i38 = INT_zero;
 CHAR ret_val79 = CHAR_zero;
 FSTR aset_self38;
 INT aset_ind38 = INT_zero;
 CHAR aset_val38 = CHAR_zero;
 FSTR aget_self40;
 INT aget_ind40 = INT_zero;
 CHAR ret_val80 = CHAR_zero;
 CHAR from_ascii_int_s39 = CHAR_zero;
 INT from_ascii_int_i39 = INT_zero;
 CHAR ret_val81 = CHAR_zero;
 FSTR aset_self39;
 INT aset_ind39 = INT_zero;
 CHAR aset_val39 = CHAR_zero;
 FSTR aget_self41;
 INT aget_ind41 = INT_zero;
 CHAR ret_val82 = CHAR_zero;
 CHAR from_ascii_int_s40 = CHAR_zero;
 INT from_ascii_int_i40 = INT_zero;
 CHAR ret_val83 = CHAR_zero;
 FSTR aset_self40;
 INT aset_ind40 = INT_zero;
 CHAR aset_val40 = CHAR_zero;
 FSTR aget_self42;
 INT aget_ind42 = INT_zero;
 CHAR ret_val84 = CHAR_zero;
 CHAR from_ascii_int_s41 = CHAR_zero;
 INT from_ascii_int_i41 = INT_zero;
 CHAR ret_val85 = CHAR_zero;
 FSTR aset_self41;
 INT aset_ind41 = INT_zero;
 CHAR aset_val41 = CHAR_zero;
 FSTR aget_self43;
 INT aget_ind43 = INT_zero;
 CHAR ret_val86 = CHAR_zero;
 CHAR from_ascii_int_s42 = CHAR_zero;
 INT from_ascii_int_i42 = INT_zero;
 CHAR ret_val87 = CHAR_zero;
 FSTR aset_self42;
 INT aset_ind42 = INT_zero;
 CHAR aset_val42 = CHAR_zero;
 FSTR aget_self44;
 INT aget_ind44 = INT_zero;
 CHAR ret_val88 = CHAR_zero;
 CHAR from_ascii_int_s43 = CHAR_zero;
 INT from_ascii_int_i43 = INT_zero;
 CHAR ret_val89 = CHAR_zero;
 FSTR aset_self43;
 INT aset_ind43 = INT_zero;
 CHAR aset_val43 = CHAR_zero;
 FSTR aget_self45;
 INT aget_ind45 = INT_zero;
 CHAR ret_val90 = CHAR_zero;
 CHAR from_ascii_int_s44 = CHAR_zero;
 INT from_ascii_int_i44 = INT_zero;
 CHAR ret_val91 = CHAR_zero;
 FSTR aset_self44;
 INT aset_ind44 = INT_zero;
 CHAR aset_val44 = CHAR_zero;
 FSTR aget_self46;
 INT aget_ind46 = INT_zero;
 CHAR ret_val92 = CHAR_zero;
 CHAR from_ascii_int_s45 = CHAR_zero;
 INT from_ascii_int_i45 = INT_zero;
 CHAR ret_val93 = CHAR_zero;
 FSTR aset_self45;
 INT aset_ind45 = INT_zero;
 CHAR aset_val45 = CHAR_zero;
 FSTR aget_self47;
 INT aget_ind47 = INT_zero;
 CHAR ret_val94 = CHAR_zero;
 CHAR from_ascii_int_s46 = CHAR_zero;
 INT from_ascii_int_i46 = INT_zero;
 CHAR ret_val95 = CHAR_zero;
 FSTR aset_self46;
 INT aset_ind46 = INT_zero;
 CHAR aset_val46 = CHAR_zero;
 FSTR aget_self48;
 INT aget_ind48 = INT_zero;
 CHAR ret_val96 = CHAR_zero;
 CHAR from_ascii_int_s47 = CHAR_zero;
 INT from_ascii_int_i47 = INT_zero;
 CHAR ret_val97 = CHAR_zero;
 FSTR aset_self47;
 INT aset_ind47 = INT_zero;
 CHAR aset_val47 = CHAR_zero;
 FSTR aget_self49;
 INT aget_ind49 = INT_zero;
 CHAR ret_val98 = CHAR_zero;
 CHAR from_ascii_int_s48 = CHAR_zero;
 INT from_ascii_int_i48 = INT_zero;
 CHAR ret_val99 = CHAR_zero;
 FSTR aset_self48;
 INT aset_ind48 = INT_zero;
 CHAR aset_val48 = CHAR_zero;
 FSTR aget_self50;
 INT aget_ind50 = INT_zero;
 CHAR ret_val100 = CHAR_zero;
 CHAR from_ascii_int_s49 = CHAR_zero;
 INT from_ascii_int_i49 = INT_zero;
 CHAR ret_val101 = CHAR_zero;
 FSTR aset_self49;
 INT aset_ind49 = INT_zero;
 CHAR aset_val49 = CHAR_zero;
 FSTR aset_self50;
 INT aset_ind50 = INT_zero;
 CHAR aset_val50 = CHAR_zero;
 FSTR aset_self51;
 INT aset_ind51 = INT_zero;
 CHAR aset_val51 = CHAR_zero;
 FSTR str_self;
 STR ret_val102;
 INT L31_;
 INT L4;
 CHAR L51_;
 CHAR L61_;
 INT L7;
 INT L81_;
 extern STR bd01232683264;
 CHAR L91_;
 CHAR L121_;
 INT L13;
 INT L141_;
 extern STR bd01232683264;
 CHAR L151_;
 CHAR L171_;
 INT L18;
 INT L191_;
 extern STR bd01232683264;
 CHAR L201_;
 CHAR L231_;
 INT L24;
 INT L251_;
 extern STR bd01232683264;
 CHAR L261_;
 CHAR L281_;
 INT L29;
 INT L301_;
 extern STR bd01232683264;
 CHAR L321_;
 CHAR L341_;
 INT L35;
 INT L361_;
 extern STR bd01232683264;
 CHAR L371_;
 CHAR L391_;
 INT L40;
 INT L411_;
 extern STR bd01232683264;
 CHAR L421_;
 CHAR L441_;
 INT L45;
 INT L461_;
 extern STR bd01232683264;
 CHAR L471_;
 CHAR L491_;
 INT L50;
 INT L521_;
 extern STR bd01232683264;
 CHAR L531_;
 CHAR L551_;
 INT L56;
 INT L571_;
 extern STR bd01232683264;
 CHAR L581_;
 CHAR L601_;
 INT L62;
 INT L631_;
 extern STR bd01232683264;
 CHAR L641_;
 CHAR L661_;
 INT L67;
 INT L681_;
 extern STR bd01232683264;
 CHAR L691_;
 CHAR L711_;
 INT L72;
 INT L731_;
 extern STR bd01232683264;
 CHAR L741_;
 CHAR L761_;
 INT L77;
 INT L781_;
 extern STR bd01232683264;
 CHAR L791_;
 CHAR L821_;
 INT L83;
 INT L841_;
 extern STR bd01232683264;
 CHAR L851_;
 CHAR L871_;
 INT L88;
 INT L891_;
 extern STR bd01232683264;
 CHAR L901_;
 CHAR L931_;
 INT L94;
 INT L951_;
 extern STR bd01232683264;
 CHAR L961_;
 CHAR L981_;
 INT L99;
 INT L1001_;
 extern STR bd01232683264;
 CHAR L1021_;
 CHAR L1041_;
 INT L105;
 INT L1061_;
 extern STR bd01232683264;
 CHAR L1071_;
 CHAR L1091_;
 INT L110;
 INT L1111_;
 extern STR bd01232683264;
 CHAR L1121_;
 CHAR L1141_;
 INT L115;
 INT L1161_;
 extern STR bd01232683264;
 CHAR L1171_;
 CHAR L1191_;
 INT L120;
 INT L1221_;
 extern STR bd01232683264;
 CHAR L1231_;
 CHAR L1251_;
 INT L126;
 INT L1271_;
 extern STR bd01232683264;
 CHAR L1281_;
 CHAR L1301_;
 INT L131;
 INT L1321_;
 extern STR bd01232683264;
 CHAR L1331_;
 CHAR L1351_;
 INT L136;
 INT L1371_;
 extern STR bd01232683264;
 CHAR L1381_;
 CHAR L1401_;
 INT L142;
 INT L1431_;
 extern STR bd01232683264;
 CHAR L1441_;
 CHAR L1461_;
 INT L147;
 INT L1481_;
 extern STR bd01232683264;
 CHAR L1491_;
 CHAR L1521_;
 INT L153;
 INT L1541_;
 extern STR bd01232683264;
 CHAR L1551_;
 CHAR L1571_;
 INT L158;
 INT L1591_;
 extern STR bd01232683264;
 CHAR L1601_;
 CHAR L1631_;
 INT L164;
 INT L1651_;
 extern STR bd01232683264;
 CHAR L1661_;
 CHAR L1681_;
 INT L169;
 INT L1701_;
 extern STR bd01232683264;
 CHAR L1721_;
 CHAR L1741_;
 INT L175;
 INT L1761_;
 extern STR bd01232683264;
 CHAR L1771_;
 CHAR L1791_;
 INT L180;
 INT L1811_;
 extern STR bd01232683264;
 CHAR L1821_;
 CHAR L1841_;
 INT L185;
 INT L1861_;
 extern STR bd01232683264;
 CHAR L1871_;
 CHAR L1891_;
 INT L190;
 INT L1921_;
 extern STR bd01232683264;
 CHAR L1931_;
 CHAR L1951_;
 INT L196;
 INT L1971_;
 extern STR bd01232683264;
 CHAR L1981_;
 CHAR L2001_;
 INT L202;
 INT L2031_;
 extern STR bd01232683264;
 CHAR L2041_;
 CHAR L2061_;
 INT L207;
 INT L2081_;
 extern STR bd01232683264;
 CHAR L2091_;
 CHAR L2111_;
 INT L212;
 INT L2131_;
 extern STR bd01232683264;
 CHAR L2141_;
 CHAR L2161_;
 INT L217;
 INT L2181_;
 extern STR bd01232683264;
 CHAR L2191_;
 CHAR L2221_;
 INT L223;
 INT L2241_;
 extern STR bd01232683264;
 CHAR L2251_;
 CHAR L2271_;
 INT L228;
 INT L2291_;
 extern STR bd01232683264;
 CHAR L2301_;
 CHAR L2331_;
 INT L234;
 INT L2351_;
 extern STR bd01232683264;
 CHAR L2361_;
 CHAR L2381_;
 INT L239;
 INT L2401_;
 extern STR bd01232683264;
 CHAR L2411_;
 CHAR L2431_;
 INT L244;
 INT L2451_;
 extern STR bd01232683264;
 CHAR L2461_;
 CHAR L2481_;
 INT L249;
 INT L2501_;
 extern STR bd01232683264;
 CHAR L2521_;
 CHAR L2541_;
 INT L255;
 INT L2561_;
 extern STR bd01232683264;
 CHAR L2571_;
 CHAR L2591_;
 INT L260;
 INT L2621_;
 extern STR bd01232683264;
 CHAR L2631_;
 CHAR L2651_;
 INT L266;
 INT L2671_;
 extern STR bd01232683264;
 CHAR L2681_;
 CHAR L2701_;
 INT L272;
 INT L2731_;
 extern STR bd01232683264;
 CHAR L2741_;
 INT L278;
 INT L2791_;
 INT L280;
 BOOL L2821_;
 if ((FAST_I1322544971==((FSTR) NULL))) {
  FAST_I1322544971 = FSTR_c1998740379(((FSTR) NULL), 10);
  FAST_I1322544971 = FSTR_p399773021(FAST_I1322544971, 'Q');
  FAST_I1322544971 = FSTR_p399773021(FAST_I1322544971, 'Q');
  FAST_I1322544971 = FSTR_p399773021(FAST_I1322544971, 'a');
 }
 else {
  {
   BOOL f_L31_ = TRUE;
   L11 = 2;
   L31_=L11-1; 
   while (1) {
    L31_++; 
    d = L31_;
    aget_self = FAST_I1322544971;
    aget_ind = d;
    L51_=ARR((FSTR)aget_self,aget_ind); 
    ret_val1 = L51_;
    L21 = ret_val1;
    switch (L21) {
     case 'a': 
      aset_self = FAST_I1322544971;
      aset_ind = d;
      from_ascii_int_s = CHAR_zero;
      aget_self1 = FAST_I1322544971;
      aget_ind1 = d;
      L61_=ARR((FSTR)aget_self1,aget_ind1); 
      ret_val2 = L61_;
      L7 = CHAR_a641490070(ret_val2);
      L81_=INTPLUS(L7,1); 
      from_ascii_int_i = L81_;
      L91_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i); 
      ret_val3 = L91_;
      aset_val = ret_val3;
      SARR((FSTR)aset_self,aset_ind,aset_val); 
      ;
      goto after_loop;
      break;
     case 'b': 
      aset_self1 = FAST_I1322544971;
      aset_ind1 = d;
      from_ascii_int_s1 = CHAR_zero;
      aget_self2 = FAST_I1322544971;
      aget_ind2 = d;
      L121_=ARR((FSTR)aget_self2,aget_ind2); 
      ret_val4 = L121_;
      L13 = CHAR_a641490070(ret_val4);
      L141_=INTPLUS(L13,1); 
      from_ascii_int_i1 = L141_;
      L151_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i1); 
      ret_val5 = L151_;
      aset_val1 = ret_val5;
      SARR((FSTR)aset_self1,aset_ind1,aset_val1); 
      ;
      goto after_loop;
      break;
     case 'c': 
      aset_self2 = FAST_I1322544971;
      aset_ind2 = d;
      from_ascii_int_s2 = CHAR_zero;
      aget_self3 = FAST_I1322544971;
      aget_ind3 = d;
      L171_=ARR((FSTR)aget_self3,aget_ind3); 
      ret_val6 = L171_;
      L18 = CHAR_a641490070(ret_val6);
      L191_=INTPLUS(L18,1); 
      from_ascii_int_i2 = L191_;
      L201_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i2); 
      ret_val7 = L201_;
      aset_val2 = ret_val7;
      SARR((FSTR)aset_self2,aset_ind2,aset_val2); 
      ;
      goto after_loop;
      break;
     case 'd': 
      aset_self3 = FAST_I1322544971;
      aset_ind3 = d;
      from_ascii_int_s3 = CHAR_zero;
      aget_self4 = FAST_I1322544971;
      aget_ind4 = d;
      L231_=ARR((FSTR)aget_self4,aget_ind4); 
      ret_val8 = L231_;
      L24 = CHAR_a641490070(ret_val8);
      L251_=INTPLUS(L24,1); 
      from_ascii_int_i3 = L251_;
      L261_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i3); 
      ret_val9 = L261_;
      aset_val3 = ret_val9;
      SARR((FSTR)aset_self3,aset_ind3,aset_val3); 
      ;
      goto after_loop;
      break;
     case 'e': 
      aset_self4 = FAST_I1322544971;
      aset_ind4 = d;
      from_ascii_int_s4 = CHAR_zero;
      aget_self5 = FAST_I1322544971;
      aget_ind5 = d;
      L281_=ARR((FSTR)aget_self5,aget_ind5); 
      ret_val10 = L281_;
      L29 = CHAR_a641490070(ret_val10);
      L301_=INTPLUS(L29,1); 
      from_ascii_int_i4 = L301_;
      L321_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i4); 
      ret_val11 = L321_;
      aset_val4 = ret_val11;
      SARR((FSTR)aset_self4,aset_ind4,aset_val4); 
      ;
      goto after_loop;
      break;
     case 'f': 
      aset_self5 = FAST_I1322544971;
      aset_ind5 = d;
      from_ascii_int_s5 = CHAR_zero;
      aget_self6 = FAST_I1322544971;
      aget_ind6 = d;
      L341_=ARR((FSTR)aget_self6,aget_ind6); 
      ret_val12 = L341_;
      L35 = CHAR_a641490070(ret_val12);
      L361_=INTPLUS(L35,1); 
      from_ascii_int_i5 = L361_;
      L371_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i5); 
      ret_val13 = L371_;
      aset_val5 = ret_val13;
      SARR((FSTR)aset_self5,aset_ind5,aset_val5); 
      ;
      goto after_loop;
      break;
     case 'g': 
      aset_self6 = FAST_I1322544971;
      aset_ind6 = d;
      from_ascii_int_s6 = CHAR_zero;
      aget_self7 = FAST_I1322544971;
      aget_ind7 = d;
      L391_=ARR((FSTR)aget_self7,aget_ind7); 
      ret_val14 = L391_;
      L40 = CHAR_a641490070(ret_val14);
      L411_=INTPLUS(L40,1); 
      from_ascii_int_i6 = L411_;
      L421_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i6); 
      ret_val15 = L421_;
      aset_val6 = ret_val15;
      SARR((FSTR)aset_self6,aset_ind6,aset_val6); 
      ;
      goto after_loop;
      break;
     case 'h': 
      aset_self7 = FAST_I1322544971;
      aset_ind7 = d;
      from_ascii_int_s7 = CHAR_zero;
      aget_self8 = FAST_I1322544971;
      aget_ind8 = d;
      L441_=ARR((FSTR)aget_self8,aget_ind8); 
      ret_val16 = L441_;
      L45 = CHAR_a641490070(ret_val16);
      L461_=INTPLUS(L45,1); 
      from_ascii_int_i7 = L461_;
      L471_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i7); 
      ret_val17 = L471_;
      aset_val7 = ret_val17;
      SARR((FSTR)aset_self7,aset_ind7,aset_val7); 
      ;
      goto after_loop;
      break;
     case 'i': 
      aset_self8 = FAST_I1322544971;
      aset_ind8 = d;
      from_ascii_int_s8 = CHAR_zero;
      aget_self9 = FAST_I1322544971;
      aget_ind9 = d;
      L491_=ARR((FSTR)aget_self9,aget_ind9); 
      ret_val18 = L491_;
      L50 = CHAR_a641490070(ret_val18);
      L521_=INTPLUS(L50,1); 
      from_ascii_int_i8 = L521_;
      L531_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i8); 
      ret_val19 = L531_;
      aset_val8 = ret_val19;
      SARR((FSTR)aset_self8,aset_ind8,aset_val8); 
      ;
      goto after_loop;
      break;
     case 'j': 
      aset_self9 = FAST_I1322544971;
      aset_ind9 = d;
      from_ascii_int_s9 = CHAR_zero;
      aget_self10 = FAST_I1322544971;
      aget_ind10 = d;
      L551_=ARR((FSTR)aget_self10,aget_ind10); 
      ret_val20 = L551_;
      L56 = CHAR_a641490070(ret_val20);
      L571_=INTPLUS(L56,1); 
      from_ascii_int_i9 = L571_;
      L581_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i9); 
      ret_val21 = L581_;
      aset_val9 = ret_val21;
      SARR((FSTR)aset_self9,aset_ind9,aset_val9); 
      ;
      goto after_loop;
      break;
     case 'k': 
      aset_self10 = FAST_I1322544971;
      aset_ind10 = d;
      from_ascii_int_s10 = CHAR_zero;
      aget_self11 = FAST_I1322544971;
      aget_ind11 = d;
      L601_=ARR((FSTR)aget_self11,aget_ind11); 
      ret_val22 = L601_;
      L62 = CHAR_a641490070(ret_val22);
      L631_=INTPLUS(L62,1); 
      from_ascii_int_i10 = L631_;
      L641_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i10); 
      ret_val23 = L641_;
      aset_val10 = ret_val23;
      SARR((FSTR)aset_self10,aset_ind10,aset_val10); 
      ;
      goto after_loop;
      break;
     case 'l': 
      aset_self11 = FAST_I1322544971;
      aset_ind11 = d;
      from_ascii_int_s11 = CHAR_zero;
      aget_self12 = FAST_I1322544971;
      aget_ind12 = d;
      L661_=ARR((FSTR)aget_self12,aget_ind12); 
      ret_val24 = L661_;
      L67 = CHAR_a641490070(ret_val24);
      L681_=INTPLUS(L67,1); 
      from_ascii_int_i11 = L681_;
      L691_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i11); 
      ret_val25 = L691_;
      aset_val11 = ret_val25;
      SARR((FSTR)aset_self11,aset_ind11,aset_val11); 
      ;
      goto after_loop;
      break;
     case 'm': 
      aset_self12 = FAST_I1322544971;
      aset_ind12 = d;
      from_ascii_int_s12 = CHAR_zero;
      aget_self13 = FAST_I1322544971;
      aget_ind13 = d;
      L711_=ARR((FSTR)aget_self13,aget_ind13); 
      ret_val26 = L711_;
      L72 = CHAR_a641490070(ret_val26);
      L731_=INTPLUS(L72,1); 
      from_ascii_int_i12 = L731_;
      L741_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i12); 
      ret_val27 = L741_;
      aset_val12 = ret_val27;
      SARR((FSTR)aset_self12,aset_ind12,aset_val12); 
      ;
      goto after_loop;
      break;
     case 'n': 
      aset_self13 = FAST_I1322544971;
      aset_ind13 = d;
      from_ascii_int_s13 = CHAR_zero;
      aget_self14 = FAST_I1322544971;
      aget_ind14 = d;
      L761_=ARR((FSTR)aget_self14,aget_ind14); 
      ret_val28 = L761_;
      L77 = CHAR_a641490070(ret_val28);
      L781_=INTPLUS(L77,1); 
      from_ascii_int_i13 = L781_;
      L791_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i13); 
      ret_val29 = L791_;
      aset_val13 = ret_val29;
      SARR((FSTR)aset_self13,aset_ind13,aset_val13); 
      ;
      goto after_loop;
      break;
     case 'o': 
      aset_self14 = FAST_I1322544971;
      aset_ind14 = d;
      from_ascii_int_s14 = CHAR_zero;
      aget_self15 = FAST_I1322544971;
      aget_ind15 = d;
      L821_=ARR((FSTR)aget_self15,aget_ind15); 
      ret_val30 = L821_;
      L83 = CHAR_a641490070(ret_val30);
      L841_=INTPLUS(L83,1); 
      from_ascii_int_i14 = L841_;
      L851_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i14); 
      ret_val31 = L851_;
      aset_val14 = ret_val31;
      SARR((FSTR)aset_self14,aset_ind14,aset_val14); 
      ;
      goto after_loop;
      break;
     case 'p': 
      aset_self15 = FAST_I1322544971;
      aset_ind15 = d;
      from_ascii_int_s15 = CHAR_zero;
      aget_self16 = FAST_I1322544971;
      aget_ind16 = d;
      L871_=ARR((FSTR)aget_self16,aget_ind16); 
      ret_val32 = L871_;
      L88 = CHAR_a641490070(ret_val32);
      L891_=INTPLUS(L88,1); 
      from_ascii_int_i15 = L891_;
      L901_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i15); 
      ret_val33 = L901_;
      aset_val15 = ret_val33;
      SARR((FSTR)aset_self15,aset_ind15,aset_val15); 
      ;
      goto after_loop;
      break;
     case 'q': 
      aset_self16 = FAST_I1322544971;
      aset_ind16 = d;
      from_ascii_int_s16 = CHAR_zero;
      aget_self17 = FAST_I1322544971;
      aget_ind17 = d;
      L931_=ARR((FSTR)aget_self17,aget_ind17); 
      ret_val34 = L931_;
      L94 = CHAR_a641490070(ret_val34);
      L951_=INTPLUS(L94,1); 
      from_ascii_int_i16 = L951_;
      L961_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i16); 
      ret_val35 = L961_;
      aset_val16 = ret_val35;
      SARR((FSTR)aset_self16,aset_ind16,aset_val16); 
      ;
      goto after_loop;
      break;
     case 'r': 
      aset_self17 = FAST_I1322544971;
      aset_ind17 = d;
      from_ascii_int_s17 = CHAR_zero;
      aget_self18 = FAST_I1322544971;
      aget_ind18 = d;
      L981_=ARR((FSTR)aget_self18,aget_ind18); 
      ret_val36 = L981_;
      L99 = CHAR_a641490070(ret_val36);
      L1001_=INTPLUS(L99,1); 
      from_ascii_int_i17 = L1001_;
      L1021_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i17); 
      ret_val37 = L1021_;
      aset_val17 = ret_val37;
      SARR((FSTR)aset_self17,aset_ind17,aset_val17); 
      ;
      goto after_loop;
      break;
     case 's': 
      aset_self18 = FAST_I1322544971;
      aset_ind18 = d;
      from_ascii_int_s18 = CHAR_zero;
      aget_self19 = FAST_I1322544971;
      aget_ind19 = d;
      L1041_=ARR((FSTR)aget_self19,aget_ind19); 
      ret_val38 = L1041_;
      L105 = CHAR_a641490070(ret_val38);
      L1061_=INTPLUS(L105,1); 
      from_ascii_int_i18 = L1061_;
      L1071_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i18); 
      ret_val39 = L1071_;
      aset_val18 = ret_val39;
      SARR((FSTR)aset_self18,aset_ind18,aset_val18); 
      ;
      goto after_loop;
      break;
     case 't': 
      aset_self19 = FAST_I1322544971;
      aset_ind19 = d;
      from_ascii_int_s19 = CHAR_zero;
      aget_self20 = FAST_I1322544971;
      aget_ind20 = d;
      L1091_=ARR((FSTR)aget_self20,aget_ind20); 
      ret_val40 = L1091_;
      L110 = CHAR_a641490070(ret_val40);
      L1111_=INTPLUS(L110,1); 
      from_ascii_int_i19 = L1111_;
      L1121_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i19); 
      ret_val41 = L1121_;
      aset_val19 = ret_val41;
      SARR((FSTR)aset_self19,aset_ind19,aset_val19); 
      ;
      goto after_loop;
      break;
     case 'u': 
      aset_self20 = FAST_I1322544971;
      aset_ind20 = d;
      from_ascii_int_s20 = CHAR_zero;
      aget_self21 = FAST_I1322544971;
      aget_ind21 = d;
      L1141_=ARR((FSTR)aget_self21,aget_ind21); 
      ret_val42 = L1141_;
      L115 = CHAR_a641490070(ret_val42);
      L1161_=INTPLUS(L115,1); 
      from_ascii_int_i20 = L1161_;
      L1171_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i20); 
      ret_val43 = L1171_;
      aset_val20 = ret_val43;
      SARR((FSTR)aset_self20,aset_ind20,aset_val20); 
      ;
      goto after_loop;
      break;
     case 'v': 
      aset_self21 = FAST_I1322544971;
      aset_ind21 = d;
      from_ascii_int_s21 = CHAR_zero;
      aget_self22 = FAST_I1322544971;
      aget_ind22 = d;
      L1191_=ARR((FSTR)aget_self22,aget_ind22); 
      ret_val44 = L1191_;
      L120 = CHAR_a641490070(ret_val44);
      L1221_=INTPLUS(L120,1); 
      from_ascii_int_i21 = L1221_;
      L1231_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i21); 
      ret_val45 = L1231_;
      aset_val21 = ret_val45;
      SARR((FSTR)aset_self21,aset_ind21,aset_val21); 
      ;
      goto after_loop;
      break;
     case 'w': 
      aset_self22 = FAST_I1322544971;
      aset_ind22 = d;
      from_ascii_int_s22 = CHAR_zero;
      aget_self23 = FAST_I1322544971;
      aget_ind23 = d;
      L1251_=ARR((FSTR)aget_self23,aget_ind23); 
      ret_val46 = L1251_;
      L126 = CHAR_a641490070(ret_val46);
      L1271_=INTPLUS(L126,1); 
      from_ascii_int_i22 = L1271_;
      L1281_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i22); 
      ret_val47 = L1281_;
      aset_val22 = ret_val47;
      SARR((FSTR)aset_self22,aset_ind22,aset_val22); 
      ;
      goto after_loop;
      break;
     case 'x': 
      aset_self23 = FAST_I1322544971;
      aset_ind23 = d;
      from_ascii_int_s23 = CHAR_zero;
      aget_self24 = FAST_I1322544971;
      aget_ind24 = d;
      L1301_=ARR((FSTR)aget_self24,aget_ind24); 
      ret_val48 = L1301_;
      L131 = CHAR_a641490070(ret_val48);
      L1321_=INTPLUS(L131,1); 
      from_ascii_int_i23 = L1321_;
      L1331_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i23); 
      ret_val49 = L1331_;
      aset_val23 = ret_val49;
      SARR((FSTR)aset_self23,aset_ind23,aset_val23); 
      ;
      goto after_loop;
      break;
     case 'y': 
      aset_self24 = FAST_I1322544971;
      aset_ind24 = d;
      from_ascii_int_s24 = CHAR_zero;
      aget_self25 = FAST_I1322544971;
      aget_ind25 = d;
      L1351_=ARR((FSTR)aget_self25,aget_ind25); 
      ret_val50 = L1351_;
      L136 = CHAR_a641490070(ret_val50);
      L1371_=INTPLUS(L136,1); 
      from_ascii_int_i24 = L1371_;
      L1381_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i24); 
      ret_val51 = L1381_;
      aset_val24 = ret_val51;
      SARR((FSTR)aset_self24,aset_ind24,aset_val24); 
      ;
      goto after_loop;
      break;
     case 'A': 
      aset_self25 = FAST_I1322544971;
      aset_ind25 = d;
      from_ascii_int_s25 = CHAR_zero;
      aget_self26 = FAST_I1322544971;
      aget_ind26 = d;
      L1401_=ARR((FSTR)aget_self26,aget_ind26); 
      ret_val52 = L1401_;
      L142 = CHAR_a641490070(ret_val52);
      L1431_=INTPLUS(L142,1); 
      from_ascii_int_i25 = L1431_;
      L1441_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i25); 
      ret_val53 = L1441_;
      aset_val25 = ret_val53;
      SARR((FSTR)aset_self25,aset_ind25,aset_val25); 
      ;
      goto after_loop;
      break;
     case 'B': 
      aset_self26 = FAST_I1322544971;
      aset_ind26 = d;
      from_ascii_int_s26 = CHAR_zero;
      aget_self27 = FAST_I1322544971;
      aget_ind27 = d;
      L1461_=ARR((FSTR)aget_self27,aget_ind27); 
      ret_val54 = L1461_;
      L147 = CHAR_a641490070(ret_val54);
      L1481_=INTPLUS(L147,1); 
      from_ascii_int_i26 = L1481_;
      L1491_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i26); 
      ret_val55 = L1491_;
      aset_val26 = ret_val55;
      SARR((FSTR)aset_self26,aset_ind26,aset_val26); 
      ;
      goto after_loop;
      break;
     case 'C': 
      aset_self27 = FAST_I1322544971;
      aset_ind27 = d;
      from_ascii_int_s27 = CHAR_zero;
      aget_self28 = FAST_I1322544971;
      aget_ind28 = d;
      L1521_=ARR((FSTR)aget_self28,aget_ind28); 
      ret_val56 = L1521_;
      L153 = CHAR_a641490070(ret_val56);
      L1541_=INTPLUS(L153,1); 
      from_ascii_int_i27 = L1541_;
      L1551_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i27); 
      ret_val57 = L1551_;
      aset_val27 = ret_val57;
      SARR((FSTR)aset_self27,aset_ind27,aset_val27); 
      ;
      goto after_loop;
      break;
     case 'D': 
      aset_self28 = FAST_I1322544971;
      aset_ind28 = d;
      from_ascii_int_s28 = CHAR_zero;
      aget_self29 = FAST_I1322544971;
      aget_ind29 = d;
      L1571_=ARR((FSTR)aget_self29,aget_ind29); 
      ret_val58 = L1571_;
      L158 = CHAR_a641490070(ret_val58);
      L1591_=INTPLUS(L158,1); 
      from_ascii_int_i28 = L1591_;
      L1601_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i28); 
      ret_val59 = L1601_;
      aset_val28 = ret_val59;
      SARR((FSTR)aset_self28,aset_ind28,aset_val28); 
      ;
      goto after_loop;
      break;
     case 'E': 
      aset_self29 = FAST_I1322544971;
      aset_ind29 = d;
      from_ascii_int_s29 = CHAR_zero;
      aget_self30 = FAST_I1322544971;
      aget_ind30 = d;
      L1631_=ARR((FSTR)aget_self30,aget_ind30); 
      ret_val60 = L1631_;
      L164 = CHAR_a641490070(ret_val60);
      L1651_=INTPLUS(L164,1); 
      from_ascii_int_i29 = L1651_;
      L1661_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i29); 
      ret_val61 = L1661_;
      aset_val29 = ret_val61;
      SARR((FSTR)aset_self29,aset_ind29,aset_val29); 
      ;
      goto after_loop;
      break;
     case 'F': 
      aset_self30 = FAST_I1322544971;
      aset_ind30 = d;
      from_ascii_int_s30 = CHAR_zero;
      aget_self31 = FAST_I1322544971;
      aget_ind31 = d;
      L1681_=ARR((FSTR)aget_self31,aget_ind31); 
      ret_val62 = L1681_;
      L169 = CHAR_a641490070(ret_val62);
      L1701_=INTPLUS(L169,1); 
      from_ascii_int_i30 = L1701_;
      L1721_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i30); 
      ret_val63 = L1721_;
      aset_val30 = ret_val63;
      SARR((FSTR)aset_self30,aset_ind30,aset_val30); 
      ;
      goto after_loop;
      break;
     case 'G': 
      aset_self31 = FAST_I1322544971;
      aset_ind31 = d;
      from_ascii_int_s31 = CHAR_zero;
      aget_self32 = FAST_I1322544971;
      aget_ind32 = d;
      L1741_=ARR((FSTR)aget_self32,aget_ind32); 
      ret_val64 = L1741_;
      L175 = CHAR_a641490070(ret_val64);
      L1761_=INTPLUS(L175,1); 
      from_ascii_int_i31 = L1761_;
      L1771_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i31); 
      ret_val65 = L1771_;
      aset_val31 = ret_val65;
      SARR((FSTR)aset_self31,aset_ind31,aset_val31); 
      ;
      goto after_loop;
      break;
     case 'H': 
      aset_self32 = FAST_I1322544971;
      aset_ind32 = d;
      from_ascii_int_s32 = CHAR_zero;
      aget_self33 = FAST_I1322544971;
      aget_ind33 = d;
      L1791_=ARR((FSTR)aget_self33,aget_ind33); 
      ret_val66 = L1791_;
      L180 = CHAR_a641490070(ret_val66);
      L1811_=INTPLUS(L180,1); 
      from_ascii_int_i32 = L1811_;
      L1821_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i32); 
      ret_val67 = L1821_;
      aset_val32 = ret_val67;
      SARR((FSTR)aset_self32,aset_ind32,aset_val32); 
      ;
      goto after_loop;
      break;
     case 'I': 
      aset_self33 = FAST_I1322544971;
      aset_ind33 = d;
      from_ascii_int_s33 = CHAR_zero;
      aget_self34 = FAST_I1322544971;
      aget_ind34 = d;
      L1841_=ARR((FSTR)aget_self34,aget_ind34); 
      ret_val68 = L1841_;
      L185 = CHAR_a641490070(ret_val68);
      L1861_=INTPLUS(L185,1); 
      from_ascii_int_i33 = L1861_;
      L1871_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i33); 
      ret_val69 = L1871_;
      aset_val33 = ret_val69;
      SARR((FSTR)aset_self33,aset_ind33,aset_val33); 
      ;
      goto after_loop;
      break;
     case 'J': 
      aset_self34 = FAST_I1322544971;
      aset_ind34 = d;
      from_ascii_int_s34 = CHAR_zero;
      aget_self35 = FAST_I1322544971;
      aget_ind35 = d;
      L1891_=ARR((FSTR)aget_self35,aget_ind35); 
      ret_val70 = L1891_;
      L190 = CHAR_a641490070(ret_val70);
      L1921_=INTPLUS(L190,1); 
      from_ascii_int_i34 = L1921_;
      L1931_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i34); 
      ret_val71 = L1931_;
      aset_val34 = ret_val71;
      SARR((FSTR)aset_self34,aset_ind34,aset_val34); 
      ;
      goto after_loop;
      break;
     case 'K': 
      aset_self35 = FAST_I1322544971;
      aset_ind35 = d;
      from_ascii_int_s35 = CHAR_zero;
      aget_self36 = FAST_I1322544971;
      aget_ind36 = d;
      L1951_=ARR((FSTR)aget_self36,aget_ind36); 
      ret_val72 = L1951_;
      L196 = CHAR_a641490070(ret_val72);
      L1971_=INTPLUS(L196,1); 
      from_ascii_int_i35 = L1971_;
      L1981_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i35); 
      ret_val73 = L1981_;
      aset_val35 = ret_val73;
      SARR((FSTR)aset_self35,aset_ind35,aset_val35); 
      ;
      goto after_loop;
      break;
     case 'L': 
      aset_self36 = FAST_I1322544971;
      aset_ind36 = d;
      from_ascii_int_s36 = CHAR_zero;
      aget_self37 = FAST_I1322544971;
      aget_ind37 = d;
      L2001_=ARR((FSTR)aget_self37,aget_ind37); 
      ret_val74 = L2001_;
      L202 = CHAR_a641490070(ret_val74);
      L2031_=INTPLUS(L202,1); 
      from_ascii_int_i36 = L2031_;
      L2041_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i36); 
      ret_val75 = L2041_;
      aset_val36 = ret_val75;
      SARR((FSTR)aset_self36,aset_ind36,aset_val36); 
      ;
      goto after_loop;
      break;
     case 'M': 
      aset_self37 = FAST_I1322544971;
      aset_ind37 = d;
      from_ascii_int_s37 = CHAR_zero;
      aget_self38 = FAST_I1322544971;
      aget_ind38 = d;
      L2061_=ARR((FSTR)aget_self38,aget_ind38); 
      ret_val76 = L2061_;
      L207 = CHAR_a641490070(ret_val76);
      L2081_=INTPLUS(L207,1); 
      from_ascii_int_i37 = L2081_;
      L2091_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i37); 
      ret_val77 = L2091_;
      aset_val37 = ret_val77;
      SARR((FSTR)aset_self37,aset_ind37,aset_val37); 
      ;
      goto after_loop;
      break;
     case 'N': 
      aset_self38 = FAST_I1322544971;
      aset_ind38 = d;
      from_ascii_int_s38 = CHAR_zero;
      aget_self39 = FAST_I1322544971;
      aget_ind39 = d;
      L2111_=ARR((FSTR)aget_self39,aget_ind39); 
      ret_val78 = L2111_;
      L212 = CHAR_a641490070(ret_val78);
      L2131_=INTPLUS(L212,1); 
      from_ascii_int_i38 = L2131_;
      L2141_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i38); 
      ret_val79 = L2141_;
      aset_val38 = ret_val79;
      SARR((FSTR)aset_self38,aset_ind38,aset_val38); 
      ;
      goto after_loop;
      break;
     case 'O': 
      aset_self39 = FAST_I1322544971;
      aset_ind39 = d;
      from_ascii_int_s39 = CHAR_zero;
      aget_self40 = FAST_I1322544971;
      aget_ind40 = d;
      L2161_=ARR((FSTR)aget_self40,aget_ind40); 
      ret_val80 = L2161_;
      L217 = CHAR_a641490070(ret_val80);
      L2181_=INTPLUS(L217,1); 
      from_ascii_int_i39 = L2181_;
      L2191_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i39); 
      ret_val81 = L2191_;
      aset_val39 = ret_val81;
      SARR((FSTR)aset_self39,aset_ind39,aset_val39); 
      ;
      goto after_loop;
      break;
     case 'P': 
      aset_self40 = FAST_I1322544971;
      aset_ind40 = d;
      from_ascii_int_s40 = CHAR_zero;
      aget_self41 = FAST_I1322544971;
      aget_ind41 = d;
      L2221_=ARR((FSTR)aget_self41,aget_ind41); 
      ret_val82 = L2221_;
      L223 = CHAR_a641490070(ret_val82);
      L2241_=INTPLUS(L223,1); 
      from_ascii_int_i40 = L2241_;
      L2251_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i40); 
      ret_val83 = L2251_;
      aset_val40 = ret_val83;
      SARR((FSTR)aset_self40,aset_ind40,aset_val40); 
      ;
      goto after_loop;
      break;
     case 'Q': 
      aset_self41 = FAST_I1322544971;
      aset_ind41 = d;
      from_ascii_int_s41 = CHAR_zero;
      aget_self42 = FAST_I1322544971;
      aget_ind42 = d;
      L2271_=ARR((FSTR)aget_self42,aget_ind42); 
      ret_val84 = L2271_;
      L228 = CHAR_a641490070(ret_val84);
      L2291_=INTPLUS(L228,1); 
      from_ascii_int_i41 = L2291_;
      L2301_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i41); 
      ret_val85 = L2301_;
      aset_val41 = ret_val85;
      SARR((FSTR)aset_self41,aset_ind41,aset_val41); 
      ;
      goto after_loop;
      break;
     case 'R': 
      aset_self42 = FAST_I1322544971;
      aset_ind42 = d;
      from_ascii_int_s42 = CHAR_zero;
      aget_self43 = FAST_I1322544971;
      aget_ind43 = d;
      L2331_=ARR((FSTR)aget_self43,aget_ind43); 
      ret_val86 = L2331_;
      L234 = CHAR_a641490070(ret_val86);
      L2351_=INTPLUS(L234,1); 
      from_ascii_int_i42 = L2351_;
      L2361_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i42); 
      ret_val87 = L2361_;
      aset_val42 = ret_val87;
      SARR((FSTR)aset_self42,aset_ind42,aset_val42); 
      ;
      goto after_loop;
      break;
     case 'S': 
      aset_self43 = FAST_I1322544971;
      aset_ind43 = d;
      from_ascii_int_s43 = CHAR_zero;
      aget_self44 = FAST_I1322544971;
      aget_ind44 = d;
      L2381_=ARR((FSTR)aget_self44,aget_ind44); 
      ret_val88 = L2381_;
      L239 = CHAR_a641490070(ret_val88);
      L2401_=INTPLUS(L239,1); 
      from_ascii_int_i43 = L2401_;
      L2411_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i43); 
      ret_val89 = L2411_;
      aset_val43 = ret_val89;
      SARR((FSTR)aset_self43,aset_ind43,aset_val43); 
      ;
      goto after_loop;
      break;
     case 'T': 
      aset_self44 = FAST_I1322544971;
      aset_ind44 = d;
      from_ascii_int_s44 = CHAR_zero;
      aget_self45 = FAST_I1322544971;
      aget_ind45 = d;
      L2431_=ARR((FSTR)aget_self45,aget_ind45); 
      ret_val90 = L2431_;
      L244 = CHAR_a641490070(ret_val90);
      L2451_=INTPLUS(L244,1); 
      from_ascii_int_i44 = L2451_;
      L2461_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i44); 
      ret_val91 = L2461_;
      aset_val44 = ret_val91;
      SARR((FSTR)aset_self44,aset_ind44,aset_val44); 
      ;
      goto after_loop;
      break;
     case 'U': 
      aset_self45 = FAST_I1322544971;
      aset_ind45 = d;
      from_ascii_int_s45 = CHAR_zero;
      aget_self46 = FAST_I1322544971;
      aget_ind46 = d;
      L2481_=ARR((FSTR)aget_self46,aget_ind46); 
      ret_val92 = L2481_;
      L249 = CHAR_a641490070(ret_val92);
      L2501_=INTPLUS(L249,1); 
      from_ascii_int_i45 = L2501_;
      L2521_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i45); 
      ret_val93 = L2521_;
      aset_val45 = ret_val93;
      SARR((FSTR)aset_self45,aset_ind45,aset_val45); 
      ;
      goto after_loop;
      break;
     case 'V': 
      aset_self46 = FAST_I1322544971;
      aset_ind46 = d;
      from_ascii_int_s46 = CHAR_zero;
      aget_self47 = FAST_I1322544971;
      aget_ind47 = d;
      L2541_=ARR((FSTR)aget_self47,aget_ind47); 
      ret_val94 = L2541_;
      L255 = CHAR_a641490070(ret_val94);
      L2561_=INTPLUS(L255,1); 
      from_ascii_int_i46 = L2561_;
      L2571_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i46); 
      ret_val95 = L2571_;
      aset_val46 = ret_val95;
      SARR((FSTR)aset_self46,aset_ind46,aset_val46); 
      ;
      goto after_loop;
      break;
     case 'W': 
      aset_self47 = FAST_I1322544971;
      aset_ind47 = d;
      from_ascii_int_s47 = CHAR_zero;
      aget_self48 = FAST_I1322544971;
      aget_ind48 = d;
      L2591_=ARR((FSTR)aget_self48,aget_ind48); 
      ret_val96 = L2591_;
      L260 = CHAR_a641490070(ret_val96);
      L2621_=INTPLUS(L260,1); 
      from_ascii_int_i47 = L2621_;
      L2631_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i47); 
      ret_val97 = L2631_;
      aset_val47 = ret_val97;
      SARR((FSTR)aset_self47,aset_ind47,aset_val47); 
      ;
      goto after_loop;
      break;
     case 'X': 
      aset_self48 = FAST_I1322544971;
      aset_ind48 = d;
      from_ascii_int_s48 = CHAR_zero;
      aget_self49 = FAST_I1322544971;
      aget_ind49 = d;
      L2651_=ARR((FSTR)aget_self49,aget_ind49); 
      ret_val98 = L2651_;
      L266 = CHAR_a641490070(ret_val98);
      L2671_=INTPLUS(L266,1); 
      from_ascii_int_i48 = L2671_;
      L2681_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i48); 
      ret_val99 = L2681_;
      aset_val48 = ret_val99;
      SARR((FSTR)aset_self48,aset_ind48,aset_val48); 
      ;
      goto after_loop;
      break;
     case 'Y': 
      aset_self49 = FAST_I1322544971;
      aset_ind49 = d;
      from_ascii_int_s49 = CHAR_zero;
      aget_self50 = FAST_I1322544971;
      aget_ind50 = d;
      L2701_=ARR((FSTR)aget_self50,aget_ind50); 
      ret_val100 = L2701_;
      L272 = CHAR_a641490070(ret_val100);
      L2731_=INTPLUS(L272,1); 
      from_ascii_int_i49 = L2731_;
      L2741_=ARR((STR)((STR) &bd01232683264),from_ascii_int_i49); 
      ret_val101 = L2741_;
      aset_val49 = ret_val101;
      SARR((FSTR)aset_self49,aset_ind49,aset_val49); 
      ;
      goto after_loop;
      break;
     case 'z': 
      aset_self50 = FAST_I1322544971;
      aset_ind50 = d;
      aset_val50 = 'A';
      SARR((FSTR)aset_self50,aset_ind50,aset_val50); 
      ;
      goto after_loop;
      break;
     case 'Z': 
      aset_self51 = FAST_I1322544971;
      aset_ind51 = d;
      aset_val51 = 'a';
      SARR((FSTR)aset_self51,aset_ind51,aset_val51); 
      ;
      L278 = FSTR_sizerINT(FAST_I1322544971);
      L2791_=INTMINUS(L278,1); 
      L280 = L2791_;
      L2821_=(d)==(L280); 
      if (L2821_) {
       FAST_I1322544971 = FSTR_p399773021(FAST_I1322544971, 'a');
       goto after_loop;
      }
      break;
     default: ;
      FATAL("No applicable target in case statement\nin FAST_IDENTIFIER::next:STR\n./Back/mangle.sa:501:20");
    }
   }
  }
  after_loop: ;
 }
 str_self = FAST_I1322544971;
 ret_val102 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val102;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR FRAME_1195781105(FRAME_LAYOUT self) {
 return ATTR(self,str);
}


#undef IS_ITER
#define IS_ITER 0

STR FRAME_1898279078(FRAME_LAYOUT self, AM_FORMAL_ARG e, STR s) {
 STR ret_val;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 BOOL L1;
 dMODE L2;
 dMODE L3;
 extern STR name58;
 L2 = ATTR(e,mode1);
 if ((*dMODE_814247358[TAG(L2)])(L2, ((dMODE) MODES_out_mode))) {
  L1 = TRUE;
 } else {
  L3 = ATTR(e,mode1);
  L1 = (*dMODE_814247358[TAG(L3)])(L3, ((dMODE) MODES_inout_mode));
 }
 if (L1) {
  plus_self = s;
  plus_sarg = ((STR) &name58);
  ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
  ret_val = ret_val1;
  return ret_val;
 }
 ret_val = s;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR FRAME_691068581(FRAME_LAYOUT self, CGEN c) {
 STR ret_val;
 MANGLE m;
 STR res;
 STR name111 = ((STR) NULL);
 AM_ROUT_DEF L11;
 AM_FORMAL_ARG fi;
 FLISTA725283029 L21;
 AM_LOCAL_EXPR fli;
 FLISTA1062334999 L31;
 AM_ITE1809135850 ni;
 INT L41 = INT_zero;
 INT nl1 = INT_zero;
 ARRAYSTR v;
 ARRAYSTR L51;
 STR s;
 STR s1;
 FLISTA1409846210 L61;
 AM_BND367211769 nbi;
 INT L71 = INT_zero;
 INT nb = INT_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val3;
 MANGLE forbid_self;
 STR forbid_s;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val4;
 INT L81 = INT_zero;
 STR plus_self4;
 CHAR plus_arg = CHAR_zero;
 STR ret_val5;
 STR s2;
 CHAR str_self = CHAR_zero;
 STR ret_val6;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val7;
 STR r;
 AM_FORMAL_ARG tp_self;
 dTP ret_val8;
 AM_LOCAL_EXPR tp_self1;
 dTP ret_val9;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val10;
 STR plus_self6;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val11;
 STR s3;
 CHAR str_self1 = CHAR_zero;
 STR ret_val12;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val13;
 STR r1;
 STR plus_self7;
 STR plus_sarg5;
 STR ret_val14;
 STR plus_self8;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val15;
 STR s4;
 CHAR str_self2 = CHAR_zero;
 STR ret_val16;
 STR create_self2;
 CHAR create_c2 = CHAR_zero;
 STR ret_val17;
 STR r2;
 STR plus_self9;
 STR plus_sarg6;
 STR ret_val18;
 AM_FORMAL_ARG name_self;
 IDENT ret_val19 = IDENT_zero;
 STR plus_self10;
 STR plus_sarg7;
 STR ret_val20;
 STR plus_self11;
 STR plus_sarg8;
 STR ret_val21;
 STR plus_self12;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val22;
 STR s5;
 CHAR str_self3 = CHAR_zero;
 STR ret_val23;
 STR create_self3;
 CHAR create_c3 = CHAR_zero;
 STR ret_val24;
 STR r3;
 AM_LOCAL_EXPR tp_self2;
 dTP ret_val25;
 STR plus_self13;
 STR plus_sarg9;
 STR ret_val26;
 STR plus_self14;
 CHAR plus_arg4 = CHAR_zero;
 STR ret_val27;
 STR s6;
 CHAR str_self4 = CHAR_zero;
 STR ret_val28;
 STR create_self4;
 CHAR create_c4 = CHAR_zero;
 STR ret_val29;
 STR r4;
 STR plus_self15;
 STR plus_sarg10;
 STR ret_val30;
 STR plus_self16;
 STR plus_sarg11;
 STR ret_val31;
 STR plus_self17;
 STR plus_sarg12;
 STR ret_val32;
 STR plus_self18;
 STR plus_sarg13;
 STR ret_val33;
 STR plus_self19;
 STR plus_sarg14;
 STR ret_val34;
 INT L91 = INT_zero;
 CGEN expand_macro_sel;
 STR expand_macro_r;
 dAM_CALL_EXPR expand_macro_cal;
 SIG expand_macro_fun;
 ARRAYSTR expand_macro_arg;
 STR expand_macro_uni;
 STR ret_val35;
 STR plus_self20;
 STR plus_sarg15;
 STR ret_val36;
 STR plus_self21;
 STR plus_sarg16;
 STR ret_val37;
 STR plus_self22;
 STR plus_sarg17;
 STR ret_val38;
 CGEN expand_macro_sel1;
 STR expand_macro_r1;
 dAM_CALL_EXPR expand_macro_cal1;
 SIG expand_macro_fun1;
 ARRAYSTR expand_macro_arg1;
 STR expand_macro_uni1;
 STR ret_val39;
 STR plus_self23;
 STR plus_sarg18;
 STR ret_val40;
 STR plus_self24;
 STR plus_sarg19;
 STR ret_val41;
 STR plus_self25;
 STR plus_sarg20;
 STR ret_val42;
 STR plus_self26;
 STR plus_sarg21;
 STR ret_val43;
 STR plus_self27;
 STR plus_sarg22;
 STR ret_val44;
 STR plus_self28;
 STR plus_sarg23;
 STR ret_val45;
 STR plus_self29;
 INT plus_arg5 = INT_zero;
 STR ret_val46;
 STR s7;
 INT str_self5 = INT_zero;
 STR ret_val47;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val48;
 FSTR str_self6;
 STR ret_val49;
 STR plus_self30;
 STR plus_sarg24;
 STR ret_val50;
 STR plus_self31;
 STR plus_sarg25;
 STR ret_val51;
 STR plus_self32;
 STR plus_sarg26;
 STR ret_val52;
 STR plus_self33;
 STR plus_sarg27;
 STR ret_val53;
 STR plus_self34;
 STR plus_sarg28;
 STR ret_val54;
 STR plus_self35;
 INT plus_arg6 = INT_zero;
 STR ret_val55;
 STR s8;
 INT str_self7 = INT_zero;
 STR ret_val56;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val57;
 FSTR str_self8;
 STR ret_val58;
 STR plus_self36;
 STR plus_sarg29;
 STR ret_val59;
 STR plus_self37;
 STR plus_sarg30;
 STR ret_val60;
 PROG psather_self;
 BOOL ret_val61 = BOOL_zero;
 STR plus_self38;
 STR plus_sarg31;
 STR ret_val62;
 STR plus_self39;
 STR plus_sarg32;
 STR ret_val63;
 STR plus_self40;
 STR plus_sarg33;
 STR ret_val64;
 STR plus_self41;
 STR plus_sarg34;
 STR ret_val65;
 STR plus_self42;
 STR plus_sarg35;
 STR ret_val66;
 MANGLE forbid_self1;
 STR forbid_s1;
 extern STR typedefstruct;
 extern STR S_frame_struct1;
 extern STR S_frame_struct2;
 extern STR INTstate;
 INT L101_br;
 AM_FORMAL_ARG aL101_;
 AM_FORMAL_ARG L12;
 STR L13;
 INT L14;
 OB L15;
 INT L16;
 STR L18;
 INT L19;
 OB L20;
 INT L22;
 STR L24;
 INT L25;
 OB L26;
 INT L27;
 extern STR Formalargument;
 IDENT L29;
 extern STR name55;
 INT L301_;
 BOOL L32;
 BOOL L331_;
 AM_LOCAL_EXPR L34;
 STR L35;
 INT L36;
 OB L37;
 INT L38;
 STR L40;
 INT L42;
 OB L43;
 INT L44;
 extern STR name13;
 INT L461_;
 AM_ITE1809135850 L47;
 INT L48;
 extern STR BOOLf_;
 extern STR usedby264297277;
 BOOL L49;
 BOOL L501_;
 INT L521_br;
 STR aL521_;
 STR L53;
 extern STR name1;
 extern STR name10;
 extern STR usedby2642972771;
 INT L541_;
 extern STR INTd;
 extern STR name1;
 extern STR name10;
 extern STR usedby2642972771;
 extern STR struct3;
 extern STR S_frame_struct3;
 extern STR nested;
 BOOL L55;
 BOOL L561_;
 extern STR nestediterframe;
 INT L571_;
 AM_BND367211769 L58;
 INT L59;
 extern STR struct3;
 extern STR S_struct4;
 extern STR nested_biter;
 BOOL L60;
 BOOL L621_;
 extern STR nestedbiterframe;
 BOOL L63;
 extern STR voidex;
 extern STR name183;
 extern STR S_frame6;
 extern STR S_frame4;
 m = ATTR(c,mangler);
 plus_self = ((STR) &typedefstruct);
 plus_sarg = MANGLE119219986(m, ((OB) ATTR(ATTR(self,f),sig1)), ((OB) NULL));
 ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
 plus_self1 = ret_val1;
 plus_sarg1 = ((STR) &S_frame_struct1);
 ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
 res = ret_val2;
 forbid_self = m;
 plus_self2 = MANGLE119219986(m, ((OB) ATTR(ATTR(self,f),sig1)), ((OB) NULL));
 plus_sarg2 = ((STR) &S_frame_struct2);
 ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
 forbid_s = ret_val3;
 SATTR(forbid_self,forbidden,FSETST1404644833(ATTR(forbid_self,forbidden), forbid_s));
 plus_self3 = res;
 plus_sarg3 = ((STR) &INTstate);
 ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
 res = ret_val4;
 {
  BOOL f_L101_ = TRUE;
  /* loop index variable */
  L81 = 0;
  L11 = ATTR(self,f);
  L101_br=L11==NULL?0:ASIZE((AM_ROUT_DEF)L11); 
  while (1) {
   if(L81>=L101_br)  goto after_loop; 
   aL101_=ARR((AM_ROUT_DEF)L11,L81); 
   fi = aL101_;
   name111 = MANGLE119219986(m, ((OB) ATTR(fi,expr)), ((OB) ATTR(ATTR(self,f),sig1)));
   plus_self4 = res;
   plus_arg = ' ';
   str_self = plus_arg;
   create_self = ((STR) NULL);
   create_c = str_self;
   L14 = 1;
   L16=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L14)*sizeof(CHAR);
   L15=ZALLOC_LEAF_BIG(L16);
   if (L15==NULL) FATAL("Unable to allocate more memory");
   memset(L15,0,L16);
   ((OB)L15)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L15)->header.destroyed=0;
#endif

   L13 = ((STR) L15);
   L13->asize = L14;
   r = L13;
   SARR((STR)r,0,create_c); 
   ;
   ret_val7 = r;
   ret_val6 = ret_val7;
   s2 = ret_val6;
   ret_val5 = STR_ap1077157958(plus_self4, s2, TRUE);
   plus_self5 = ret_val5;
   tp_self = fi;
   tp_self1 = ATTR(tp_self,expr);
   ret_val9 = ATTR(tp_self1,tp_at);
   ret_val8 = ret_val9;
   plus_sarg4 = FRAME_1898279078(self, fi, MANGLE119219986(m, ((OB) ret_val8), ((OB) NULL)));
   ret_val10 = STR_ap2004550586(plus_self5, plus_sarg4);
   plus_self6 = ret_val10;
   plus_arg1 = ' ';
   str_self1 = plus_arg1;
   create_self1 = ((STR) NULL);
   create_c1 = str_self1;
   L19 = 1;
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
   r1 = L18;
   SARR((STR)r1,0,create_c1); 
   ;
   ret_val13 = r1;
   ret_val12 = ret_val13;
   s3 = ret_val12;
   ret_val11 = STR_ap1077157958(plus_self6, s3, TRUE);
   plus_self7 = ret_val11;
   plus_sarg5 = name111;
   ret_val14 = STR_ap2004550586(plus_self7, plus_sarg5);
   plus_self8 = ret_val14;
   plus_arg2 = ';';
   str_self2 = plus_arg2;
   create_self2 = ((STR) NULL);
   create_c2 = str_self2;
   L25 = 1;
   L27=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L25)*sizeof(CHAR);
   L26=ZALLOC_LEAF_BIG(L27);
   if (L26==NULL) FATAL("Unable to allocate more memory");
   memset(L26,0,L27);
   ((OB)L26)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L26)->header.destroyed=0;
#endif

   L24 = ((STR) L26);
   L24->asize = L25;
   r2 = L24;
   SARR((STR)r2,0,create_c2); 
   ;
   ret_val17 = r2;
   ret_val16 = ret_val17;
   s4 = ret_val16;
   ret_val15 = STR_ap1077157958(plus_self8, s4, TRUE);
   res = ret_val15;
   plus_self9 = res;
   plus_sarg6 = ((STR) &Formalargument);
   ret_val18 = STR_ap2004550586(plus_self9, plus_sarg6);
   plus_self10 = ret_val18;
   name_self = fi;
   ret_val19 = ATTR(ATTR(name_self,expr),name1);
   L29=ret_val19;
   plus_sarg7 = L29.str;
   ret_val20 = STR_ap2004550586(plus_self10, plus_sarg7);
   plus_self11 = ret_val20;
   plus_sarg8 = ((STR) &name55);
   ret_val21 = STR_ap2004550586(plus_self11, plus_sarg8);
   res = ret_val21;
   L301_=INTPLUS(L81,1); 
   L81 = L301_;
  }
 }
 after_loop: ;
 L32 = (ATTR(ATTR(self,f),locals1)==((FLISTA725283029) NULL));
 L331_=!(L32); 
 if (L331_) {
  {
   struct FLISTA2119642552_frame_struct other2_0;
FLISTA2119642552_frame noname1 = &other2_0;
   L21 = ATTR(ATTR(self,f),locals1);
   noname1->self = L21;
   noname1->state = 0;
   while (1) {
    L34 = FLISTA2119642552(noname1);
    if (noname1->state == -1) {
     goto after_loop1;
    }
    fli = L34;
    name111 = MANGLE119219986(m, ((OB) fli), ((OB) ATTR(ATTR(self,f),sig1)));
    plus_self12 = res;
    plus_arg3 = ' ';
    str_self3 = plus_arg3;
    create_self3 = ((STR) NULL);
    create_c3 = str_self3;
    L36 = 1;
    L38=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L36)*sizeof(CHAR);
    L37=ZALLOC_LEAF_BIG(L38);
    if (L37==NULL) FATAL("Unable to allocate more memory");
    memset(L37,0,L38);
    ((OB)L37)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L37)->header.destroyed=0;
#endif

    L35 = ((STR) L37);
    L35->asize = L36;
    r3 = L35;
    SARR((STR)r3,0,create_c3); 
    ;
    ret_val24 = r3;
    ret_val23 = ret_val24;
    s5 = ret_val23;
    ret_val22 = STR_ap1077157958(plus_self12, s5, TRUE);
    plus_self13 = ret_val22;
    tp_self2 = fli;
    ret_val25 = ATTR(tp_self2,tp_at);
    plus_sarg9 = MANGLE119219986(m, ((OB) ret_val25), ((OB) NULL));
    ret_val26 = STR_ap2004550586(plus_self13, plus_sarg9);
    plus_self14 = ret_val26;
    plus_arg4 = ' ';
    str_self4 = plus_arg4;
    create_self4 = ((STR) NULL);
    create_c4 = str_self4;
    L42 = 1;
    L44=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L42)*sizeof(CHAR);
    L43=ZALLOC_LEAF_BIG(L44);
    if (L43==NULL) FATAL("Unable to allocate more memory");
    memset(L43,0,L44);
    ((OB)L43)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L43)->header.destroyed=0;
#endif

    L40 = ((STR) L43);
    L40->asize = L42;
    r4 = L40;
    SARR((STR)r4,0,create_c4); 
    ;
    ret_val29 = r4;
    ret_val28 = ret_val29;
    s6 = ret_val28;
    ret_val27 = STR_ap1077157958(plus_self14, s6, TRUE);
    plus_self15 = ret_val27;
    plus_sarg10 = name111;
    ret_val30 = STR_ap2004550586(plus_self15, plus_sarg10);
    plus_self16 = ret_val30;
    plus_sarg11 = ((STR) &name13);
    ret_val31 = STR_ap2004550586(plus_self16, plus_sarg11);
    res = ret_val31;
   }
  }
  after_loop1: ;
 }
 {
  struct FLISTA20621028_frame_struct other3_0;
FLISTA20621028_frame noname2 = &other3_0;
  BOOL f_L461_ = TRUE;
  L41 = 1;
  noname2->state = 0;
  L461_=L41-1; 
  while (1) {
   if (noname2->state == 0) {
    L31 = FMAPAM925529692(ATTR(LAYOUT_TBL_cgen,nested_its), ATTR(self,f));
    noname2->self = L31;
   }
   L47 = FLISTA20621028(noname2);
   if (noname2->state == -1) {
    goto after_loop2;
   }
   ni = L47;
   L461_++; 
   nl1 = L461_;
   if (SIG_is418491101(ATTR(ni,fun))) {
    plus_self17 = res;
    plus_sarg12 = ((STR) &BOOLf_);
    ret_val32 = STR_ap2004550586(plus_self17, plus_sarg12);
    plus_self18 = ret_val32;
    plus_sarg13 = ATTR(ni,uniq);
    ret_val33 = STR_ap2004550586(plus_self18, plus_sarg13);
    plus_self19 = ret_val33;
    plus_sarg14 = ((STR) &usedby264297277);
    ret_val34 = STR_ap2004550586(plus_self19, plus_sarg14);
    res = ret_val34;
    v = ATTR(ATTR(ATTR(ni,fun),builtin_info),var);
    L49 = (v==((ARRAYSTR) NULL));
    L501_=!(L49); 
    if (L501_) {
     {
      BOOL f_L521_ = TRUE;
      /* loop index variable */
      L91 = 0;
      L51 = v;
      L521_br=L51==NULL?0:ASIZE((ARRAYSTR)L51); 
      while (1) {
       expand_macro_sel = ((CGEN) NULL);
       if(L91>=L521_br)  goto after_loop3; 
       aL521_=ARR((ARRAYSTR)L51,L91); 
       expand_macro_r = aL521_;
       expand_macro_cal = ((dAM_CALL_EXPR) NULL);
       expand_macro_fun = ATTR(ni,fun);
       expand_macro_arg = ((ARRAYSTR) NULL);
       expand_macro_uni = ATTR(ni,uniq);
       ret_val35 = CGEN_e1606764211(expand_macro_sel, expand_macro_r, expand_macro_cal, expand_macro_fun, expand_macro_arg, expand_macro_uni, ((STR) &name1), ((STR) NULL));
       s = ret_val35;
       plus_self20 = res;
       plus_sarg15 = ((STR) &name10);
       ret_val36 = STR_ap2004550586(plus_self20, plus_sarg15);
       plus_self21 = ret_val36;
       plus_sarg16 = s;
       ret_val37 = STR_ap2004550586(plus_self21, plus_sarg16);
       plus_self22 = ret_val37;
       plus_sarg17 = ((STR) &usedby2642972771);
       ret_val38 = STR_ap2004550586(plus_self22, plus_sarg17);
       res = ret_val38;
       L541_=INTPLUS(L91,1); 
       L91 = L541_;
      }
     }
     after_loop3: ;
    }
    if (ATTR(ATTR(ATTR(ni,fun),builtin_info),use_index)) {
     expand_macro_sel1 = ((CGEN) NULL);
     expand_macro_r1 = ((STR) &INTd);
     expand_macro_cal1 = ((dAM_CALL_EXPR) NULL);
     expand_macro_fun1 = ATTR(ni,fun);
     expand_macro_arg1 = ((ARRAYSTR) NULL);
     expand_macro_uni1 = ATTR(ni,uniq);
     ret_val39 = CGEN_e1606764211(expand_macro_sel1, expand_macro_r1, expand_macro_cal1, expand_macro_fun1, expand_macro_arg1, expand_macro_uni1, ((STR) &name1), ((STR) NULL));
     s1 = ret_val39;
     plus_self23 = res;
     plus_sarg18 = ((STR) &name10);
     ret_val40 = STR_ap2004550586(plus_self23, plus_sarg18);
     plus_self24 = ret_val40;
     plus_sarg19 = s1;
     ret_val41 = STR_ap2004550586(plus_self24, plus_sarg19);
     plus_self25 = ret_val41;
     plus_sarg20 = ((STR) &usedby2642972771);
     ret_val42 = STR_ap2004550586(plus_self25, plus_sarg20);
     res = ret_val42;
    }
   }
   else {
    plus_self26 = res;
    plus_sarg21 = ((STR) &struct3);
    ret_val43 = STR_ap2004550586(plus_self26, plus_sarg21);
    plus_self27 = ret_val43;
    plus_sarg22 = MANGLE119219986(m, ((OB) ATTR(ni,fun)), ((OB) NULL));
    ret_val44 = STR_ap2004550586(plus_self27, plus_sarg22);
    plus_self28 = ret_val44;
    plus_sarg23 = ((STR) &S_frame_struct3);
    ret_val45 = STR_ap2004550586(plus_self28, plus_sarg23);
    res = ret_val45;
    plus_self29 = ((STR) &nested);
    plus_arg5 = nl1;
    str_self5 = plus_arg5;
    clear_self = buf1;
    L55 = (clear_self==((FSTR) NULL));
    L561_=!(L55); 
    if (L561_) {
     SATTR(clear_self,loc,0);
    }
    str_in_self = str_self5;
    str_in_s = buf1;
    ret_val48 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
    buf1 = ret_val48;
    str_self6 = buf1;
    ret_val49 = STR_fr1097270334(((STR) NULL), str_self6);
    ret_val47 = ret_val49;
    s7 = ret_val47;
    ret_val46 = STR_ap1077157958(plus_self29, s7, TRUE);
    name111 = ret_val46;
    plus_self30 = res;
    plus_sarg24 = name111;
    ret_val50 = STR_ap2004550586(plus_self30, plus_sarg24);
    plus_self31 = ret_val50;
    plus_sarg25 = ((STR) &nestediterframe);
    ret_val51 = STR_ap2004550586(plus_self31, plus_sarg25);
    res = ret_val51;
   }
  }
 }
 after_loop2: ;
 {
  struct FLISTA1446310868_frame_struct other5_0;
FLISTA1446310868_frame noname3 = &other5_0;
  BOOL f_L571_ = TRUE;
  L71 = 1;
  noname3->state = 0;
  L571_=L71-1; 
  while (1) {
   if (noname3->state == 0) {
    L61 = FMAPAM338804820(ATTR(LAYOUT_TBL_cgen,nested_bits), ATTR(self,f));
    noname3->self = L61;
   }
   L58 = FLISTA1446310868(noname3);
   if (noname3->state == -1) {
    goto after_loop4;
   }
   nbi = L58;
   L571_++; 
   nb = L571_;
   plus_self32 = res;
   plus_sarg26 = ((STR) &struct3);
   ret_val52 = STR_ap2004550586(plus_self32, plus_sarg26);
   plus_self33 = ret_val52;
   plus_sarg27 = MANGLE119219986(m, ((OB) ATTR(nbi,bi_tp)), ((OB) NULL));
   ret_val53 = STR_ap2004550586(plus_self33, plus_sarg27);
   plus_self34 = ret_val53;
   plus_sarg28 = ((STR) &S_struct4);
   ret_val54 = STR_ap2004550586(plus_self34, plus_sarg28);
   res = ret_val54;
   plus_self35 = ((STR) &nested_biter);
   plus_arg6 = nb;
   str_self7 = plus_arg6;
   clear_self1 = buf2;
   L60 = (clear_self1==((FSTR) NULL));
   L621_=!(L60); 
   if (L621_) {
    SATTR(clear_self1,loc,0);
   }
   str_in_self1 = str_self7;
   str_in_s1 = buf2;
   ret_val57 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
   buf2 = ret_val57;
   str_self8 = buf2;
   ret_val58 = STR_fr1097270334(((STR) NULL), str_self8);
   ret_val56 = ret_val58;
   s8 = ret_val56;
   ret_val55 = STR_ap1077157958(plus_self35, s8, TRUE);
   name111 = ret_val55;
   plus_self36 = res;
   plus_sarg29 = name111;
   ret_val59 = STR_ap2004550586(plus_self36, plus_sarg29);
   plus_self37 = ret_val59;
   plus_sarg30 = ((STR) &nestedbiterframe);
   ret_val60 = STR_ap2004550586(plus_self37, plus_sarg30);
   res = ret_val60;
  }
 }
 after_loop4: ;
 psather_self = ATTR(c,prog);
 if (ATTR(psather_self,threads)) {
  L63 = TRUE;
 } else {
  L63 = ATTR(psather_self,distributed);
 }
 ret_val61 = L63;
 if (ret_val61) {
  plus_self38 = res;
  plus_sarg31 = ((STR) &voidex);
  ret_val62 = STR_ap2004550586(plus_self38, plus_sarg31);
  res = ret_val62;
 }
 plus_self39 = res;
 plus_sarg32 = ((STR) &name183);
 ret_val63 = STR_ap2004550586(plus_self39, plus_sarg32);
 plus_self40 = ret_val63;
 plus_sarg33 = MANGLE119219986(m, ((OB) ATTR(ATTR(self,f),sig1)), ((OB) NULL));
 ret_val64 = STR_ap2004550586(plus_self40, plus_sarg33);
 plus_self41 = ret_val64;
 plus_sarg34 = ((STR) &S_frame6);
 ret_val65 = STR_ap2004550586(plus_self41, plus_sarg34);
 res = ret_val65;
 forbid_self1 = m;
 plus_self42 = MANGLE119219986(m, ((OB) ATTR(ATTR(self,f),sig1)), ((OB) NULL));
 plus_sarg35 = ((STR) &S_frame4);
 ret_val66 = STR_ap2004550586(plus_self42, plus_sarg35);
 forbid_s1 = ret_val66;
 SATTR(forbid_self1,forbidden,FSETST1404644833(ATTR(forbid_self1,forbidden), forbid_s1));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR IMMUTA2093463840(IMMUTA421759893 self, CGEN c) {
 STR ret_val;
 MANGLE m;
 STR cname;
 STR res;
 INT chars_and_bools = INT_zero;
 LAYOUT_ARR L11;
 TUPIDENTdTP p = TUPIDENTdTP_zero;
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
 MANGLE forbid_self;
 STR forbid_s;
 INT L21 = INT_zero;
 STR plus_self6;
 CHAR plus_arg = CHAR_zero;
 STR ret_val7;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val8;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val9;
 STR r;
 STR plus_self7;
 STR plus_sarg6;
 STR ret_val10;
 STR plus_self8;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val11;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val12;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val13;
 STR r1;
 STR plus_self9;
 STR plus_sarg7;
 STR ret_val14;
 STR plus_self10;
 STR plus_sarg8;
 STR ret_val15;
 STR plus_self11;
 STR plus_sarg9;
 STR ret_val16;
 STR plus_self12;
 STR plus_sarg10;
 STR ret_val17;
 STR plus_self13;
 STR plus_sarg11;
 STR ret_val18;
 STR plus_self14;
 STR plus_sarg12;
 STR ret_val19;
 STR plus_self15;
 STR plus_sarg13;
 STR ret_val20;
 STR plus_self16;
 STR plus_sarg14;
 STR ret_val21;
 STR plus_self17;
 STR plus_sarg15;
 STR ret_val22;
 STR plus_self18;
 STR plus_sarg16;
 STR ret_val23;
 STR plus_self19;
 STR plus_sarg17;
 STR ret_val24;
 STR plus_self20;
 STR plus_sarg18;
 STR ret_val25;
 PROG psather_self;
 BOOL ret_val26 = BOOL_zero;
 STR plus_self21;
 STR plus_sarg19;
 STR ret_val27;
 STR plus_self22;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val28;
 STR s2;
 CHAR str_self2 = CHAR_zero;
 STR ret_val29;
 STR create_self2;
 CHAR create_c2 = CHAR_zero;
 STR ret_val30;
 STR r2;
 STR plus_self23;
 STR plus_sarg20;
 STR ret_val31;
 STR plus_self24;
 STR plus_sarg21;
 STR ret_val32;
 STR plus_self25;
 STR plus_sarg22;
 STR ret_val33;
 STR plus_self26;
 STR plus_sarg23;
 STR ret_val34;
 STR plus_self27;
 STR plus_sarg24;
 STR ret_val35;
 STR plus_self28;
 STR plus_sarg25;
 STR ret_val36;
 STR plus_self29;
 STR plus_sarg26;
 STR ret_val37;
 STR plus_self30;
 STR plus_sarg27;
 STR ret_val38;
 STR plus_self31;
 INT plus_arg3 = INT_zero;
 STR ret_val39;
 STR s3;
 INT str_self3 = INT_zero;
 STR ret_val40;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val41;
 FSTR str_self4;
 STR ret_val42;
 STR plus_self32;
 STR plus_sarg28;
 STR ret_val43;
 STR plus_self33;
 STR plus_sarg29;
 STR ret_val44;
 STR plus_self34;
 STR plus_sarg30;
 STR ret_val45;
 STR plus_self35;
 STR plus_sarg31;
 STR ret_val46;
 STR plus_self36;
 STR plus_sarg32;
 STR ret_val47;
 STR plus_self37;
 STR plus_sarg33;
 STR ret_val48;
 STR plus_self38;
 STR plus_sarg34;
 STR ret_val49;
 STR plus_self39;
 STR plus_sarg35;
 STR ret_val50;
 STR plus_self40;
 STR plus_sarg36;
 STR ret_val51;
 STR plus_self41;
 STR plus_sarg37;
 STR ret_val52;
 STR plus_self42;
 STR plus_sarg38;
 STR ret_val53;
 MANGLE forbid_self1;
 STR forbid_s1;
 STR plus_self43;
 STR plus_sarg39;
 STR ret_val54;
 STR plus_self44;
 STR plus_sarg40;
 STR ret_val55;
 STR plus_self45;
 STR plus_sarg41;
 STR ret_val56;
 STR plus_self46;
 STR plus_sarg42;
 STR ret_val57;
 STR plus_self47;
 CHAR plus_arg4 = CHAR_zero;
 STR ret_val58;
 STR s4;
 CHAR str_self5 = CHAR_zero;
 STR ret_val59;
 STR create_self3;
 CHAR create_c3 = CHAR_zero;
 STR ret_val60;
 STR r3;
 STR plus_self48;
 STR plus_sarg43;
 STR ret_val61;
 STR plus_self49;
 STR plus_sarg44;
 STR ret_val62;
 STR plus_self50;
 STR plus_sarg45;
 STR ret_val63;
 STR plus_self51;
 STR plus_sarg46;
 STR ret_val64;
 STR plus_self52;
 STR plus_sarg47;
 STR ret_val65;
 STR plus_self53;
 STR plus_sarg48;
 STR ret_val66;
 MANGLE forbid_self2;
 STR forbid_s2;
 STR plus_self54;
 STR plus_sarg49;
 STR ret_val67;
 MANGLE forbid_self3;
 STR forbid_s3;
 extern STR typedefstruct;
 extern STR S_struct5;
 extern STR layoutfor;
 dTP L3;
 extern STR name55;
 extern STR S_struct2;
 BOOL L4;
 BOOL L51_;
 TUPIDENTdTP L7;
 INT L61_br;
 TUPIDENTdTP aL61_;
 dTP L8;
 BOOL L9;
 dTP L10;
 dTP L12;
 INT L131_;
 STR L14;
 INT L15;
 OB L16;
 INT L17;
 STR L19;
 INT L20;
 OB L22;
 INT L23;
 OB L25;
 OB L26;
 extern STR name13;
 dTP L27;
 BOOL L28;
 BOOL L291_;
 extern STR struct3;
 extern STR S_struct4;
 OB L30;
 OB L31;
 extern STR name13;
 extern STR struct3;
 extern STR S_struct1;
 OB L32;
 OB L33;
 extern STR name13;
 INT L341_;
 BOOL L35;
 BOOL L361_;
 BOOL L37;
 extern STR char_d202626486;
 BOOL L38;
 BOOL L391_;
 dTP L40;
 STR L41;
 INT L42;
 OB L43;
 INT L44;
 extern STR arr_part6;
 dTP L46;
 BOOL L47;
 BOOL L481_;
 extern STR struct3;
 extern STR S_structarr_part;
 extern STR struct3;
 extern STR S_structarr_part1;
 INT L49;
 INT L501_;
 BOOL L52;
 BOOL L531_;
 extern STR name188;
 BOOL L54;
 extern STR CHARfi1162829196;
 extern STR name140;
 extern STR name13;
 extern STR static1;
 extern STR name10;
 extern STR S_zero2;
 extern STR S_zero;
 extern STR typedefstruct;
 extern STR S_boxed_struct1;
 extern STR OB_HEADERheader;
 STR L55;
 INT L56;
 OB L57;
 INT L58;
 extern STR immutable_part5;
 extern STR name183;
 extern STR S_boxed3;
 extern STR S_boxed2;
 extern STR S_boxed_struct2;
 m = ATTR(c,mangler);
 cname = MANGLE119219986(m, ((OB) ATTR(ATTR(self,l),tp)), ((OB) NULL));
 plus_self = ((STR) &typedefstruct);
 plus_sarg = cname;
 ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
 plus_self1 = ret_val1;
 plus_sarg1 = ((STR) &S_struct5);
 ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
 res = ret_val2;
 plus_self2 = res;
 plus_sarg2 = ((STR) &layoutfor);
 ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
 plus_self3 = ret_val3;
 L3 = ATTR(ATTR(self,l),tp);
 plus_sarg3 = (*dTP_strrSTR[TAG(L3)])(L3);
 ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
 plus_self4 = ret_val4;
 plus_sarg4 = ((STR) &name55);
 ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
 res = ret_val5;
 forbid_self = m;
 plus_self5 = MANGLE119219986(m, ((OB) ATTR(self,tp)), ((OB) NULL));
 plus_sarg5 = ((STR) &S_struct2);
 ret_val6 = STR_ap2004550586(plus_self5, plus_sarg5);
 forbid_s = ret_val6;
 SATTR(forbid_self,forbidden,FSETST1404644833(ATTR(forbid_self,forbidden), forbid_s));
 L4 = (ATTR(ATTR(self,l),at)==((FMAPIDENTdTP) NULL));
 L51_=!(L4); 
 if (L51_) {
  chars_and_bools = 0;
  {
   BOOL f_L61_ = TRUE;
   /* loop index variable */
   L21 = 0;
   while (1) {
    if (f_L61_) {
     f_L61_ = FALSE;
     L11 = IMMUTA877143907(self, ATTR(ATTR(self,l),at));
     L61_br=L11==NULL?0:ASIZE((LAYOUT_ARR)L11); 
    }
    if(L21>=L61_br)  goto after_loop; 
    aL61_=ARR((LAYOUT_ARR)L11,L21); 
    p = aL61_;
    L8 = p.t2;
    if ((*dTP_is914808564[TAG(L8)])(L8)) {
     L10 = p.t2;
     if ((*dTP_is242312711[TAG(L10)])(L10, ((OB) TP_BUILTIN_char))) {
      L9 = TRUE;
     } else {
      L12 = p.t2;
      L9 = (*dTP_is242312711[TAG(L12)])(L12, ((OB) TP_BUILTIN_bool));
     }
     if (L9) {
      L131_=INTPLUS(chars_and_bools,1); 
      chars_and_bools = L131_;
     }
     plus_self6 = res;
     plus_arg = ' ';
     str_self = plus_arg;
     create_self = ((STR) NULL);
     create_c = str_self;
     L15 = 1;
     L17=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L15)*sizeof(CHAR);
     L16=ZALLOC_LEAF_BIG(L17);
     if (L16==NULL) FATAL("Unable to allocate more memory");
     memset(L16,0,L17);
     ((OB)L16)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L16)->header.destroyed=0;
#endif

     L14 = ((STR) L16);
     L14->asize = L15;
     r = L14;
     SARR((STR)r,0,create_c); 
     ;
     ret_val9 = r;
     ret_val8 = ret_val9;
     s = ret_val8;
     ret_val7 = STR_ap1077157958(plus_self6, s, TRUE);
     plus_self7 = ret_val7;
     plus_sarg6 = MANGLE119219986(m, ((OB) p.t2), ((OB) NULL));
     ret_val10 = STR_ap2004550586(plus_self7, plus_sarg6);
     plus_self8 = ret_val10;
     plus_arg1 = ' ';
     str_self1 = plus_arg1;
     create_self1 = ((STR) NULL);
     create_c1 = str_self1;
     L20 = 1;
     L23=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L20)*sizeof(CHAR);
     L22=ZALLOC_LEAF_BIG(L23);
     if (L22==NULL) FATAL("Unable to allocate more memory");
     memset(L22,0,L23);
     ((OB)L22)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L22)->header.destroyed=0;
#endif

     L19 = ((STR) L22);
     L19->asize = L20;
     r1 = L19;
     SARR((STR)r1,0,create_c1); 
     ;
     ret_val13 = r1;
     ret_val12 = ret_val13;
     s1 = ret_val12;
     ret_val11 = STR_ap1077157958(plus_self8, s1, TRUE);
     plus_self9 = ret_val11;
     L26=ZALLOC(sizeof(struct IDENT_boxed_struct));
     if (L26==NULL) FATAL("Unable to allocate more memory");
     ((OB)L26)->header.tag=IDENT_tag;
     L25 = (OB)((IDENT_boxed) L26);
     ((IDENT_boxed) L25)->immutable_part = p.t1;
     plus_sarg7 = MANGLE119219986(m, L25, ((OB) ATTR(ATTR(self,l),tp)));
     ret_val14 = STR_ap2004550586(plus_self9, plus_sarg7);
     plus_self10 = ret_val14;
     plus_sarg8 = ((STR) &name13);
     ret_val15 = STR_ap2004550586(plus_self10, plus_sarg8);
     res = ret_val15;
    }
    else {
     L27 = p.t2;
     L28 = (*dTP_is1334578382[TAG(L27)])(L27);
     L291_=!(L28); 
     if (L291_) {
      plus_self11 = res;
      plus_sarg9 = ((STR) &struct3);
      ret_val16 = STR_ap2004550586(plus_self11, plus_sarg9);
      plus_self12 = ret_val16;
      plus_sarg10 = MANGLE119219986(m, ((OB) p.t2), ((OB) NULL));
      ret_val17 = STR_ap2004550586(plus_self12, plus_sarg10);
      plus_self13 = ret_val17;
      plus_sarg11 = ((STR) &S_struct4);
      ret_val18 = STR_ap2004550586(plus_self13, plus_sarg11);
      plus_self14 = ret_val18;
      L31=ZALLOC(sizeof(struct IDENT_boxed_struct));
      if (L31==NULL) FATAL("Unable to allocate more memory");
      ((OB)L31)->header.tag=IDENT_tag;
      L30 = (OB)((IDENT_boxed) L31);
      ((IDENT_boxed) L30)->immutable_part = p.t1;
      plus_sarg12 = MANGLE119219986(m, L30, ((OB) ATTR(ATTR(self,l),tp)));
      ret_val19 = STR_ap2004550586(plus_self14, plus_sarg12);
      plus_self15 = ret_val19;
      plus_sarg13 = ((STR) &name13);
      ret_val20 = STR_ap2004550586(plus_self15, plus_sarg13);
      res = ret_val20;
     }
     else {
      plus_self16 = res;
      plus_sarg14 = ((STR) &struct3);
      ret_val21 = STR_ap2004550586(plus_self16, plus_sarg14);
      plus_self17 = ret_val21;
      plus_sarg15 = MANGLE119219986(m, ((OB) p.t2), ((OB) NULL));
      ret_val22 = STR_ap2004550586(plus_self17, plus_sarg15);
      plus_self18 = ret_val22;
      plus_sarg16 = ((STR) &S_struct1);
      ret_val23 = STR_ap2004550586(plus_self18, plus_sarg16);
      plus_self19 = ret_val23;
      L33=ZALLOC(sizeof(struct IDENT_boxed_struct));
      if (L33==NULL) FATAL("Unable to allocate more memory");
      ((OB)L33)->header.tag=IDENT_tag;
      L32 = (OB)((IDENT_boxed) L33);
      ((IDENT_boxed) L32)->immutable_part = p.t1;
      plus_sarg17 = MANGLE119219986(m, L32, ((OB) ATTR(ATTR(self,l),tp)));
      ret_val24 = STR_ap2004550586(plus_self19, plus_sarg17);
      plus_self20 = ret_val24;
      plus_sarg18 = ((STR) &name13);
      ret_val25 = STR_ap2004550586(plus_self20, plus_sarg18);
      res = ret_val25;
     }
    }
    L341_=INTPLUS(L21,1); 
    L21 = L341_;
   }
  }
  after_loop: ;
  L361_=(chars_and_bools&1)!=0; 
  if (L361_) {
   psather_self = ATTR(c,prog);
   if (ATTR(psather_self,threads)) {
    L37 = TRUE;
   } else {
    L37 = ATTR(psather_self,distributed);
   }
   ret_val26 = L37;
   L35 = ret_val26;
  } else {
   L35 = FALSE;
  }
  if (L35) {
   plus_self21 = res;
   plus_sarg19 = ((STR) &char_d202626486);
   ret_val27 = STR_ap2004550586(plus_self21, plus_sarg19);
   res = ret_val27;
  }
 }
 L38 = (ATTR(ATTR(self,l),arr)==((dTP) NULL));
 L391_=!(L38); 
 if (L391_) {
  L40 = ATTR(ATTR(self,l),arr);
  if ((*dTP_is914808564[TAG(L40)])(L40)) {
   plus_self22 = res;
   plus_arg2 = ' ';
   str_self2 = plus_arg2;
   create_self2 = ((STR) NULL);
   create_c2 = str_self2;
   L42 = 1;
   L44=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L42)*sizeof(CHAR);
   L43=ZALLOC_LEAF_BIG(L44);
   if (L43==NULL) FATAL("Unable to allocate more memory");
   memset(L43,0,L44);
   ((OB)L43)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L43)->header.destroyed=0;
#endif

   L41 = ((STR) L43);
   L41->asize = L42;
   r2 = L41;
   SARR((STR)r2,0,create_c2); 
   ;
   ret_val30 = r2;
   ret_val29 = ret_val30;
   s2 = ret_val29;
   ret_val28 = STR_ap1077157958(plus_self22, s2, TRUE);
   plus_self23 = ret_val28;
   plus_sarg20 = MANGLE119219986(m, ((OB) ATTR(ATTR(self,l),arr)), ((OB) NULL));
   ret_val31 = STR_ap2004550586(plus_self23, plus_sarg20);
   plus_self24 = ret_val31;
   plus_sarg21 = ((STR) &arr_part6);
   ret_val32 = STR_ap2004550586(plus_self24, plus_sarg21);
   res = ret_val32;
  }
  else {
   L46 = ATTR(ATTR(self,l),arr);
   L47 = (*dTP_is1334578382[TAG(L46)])(L46);
   L481_=!(L47); 
   if (L481_) {
    plus_self25 = res;
    plus_sarg22 = ((STR) &struct3);
    ret_val33 = STR_ap2004550586(plus_self25, plus_sarg22);
    plus_self26 = ret_val33;
    plus_sarg23 = MANGLE119219986(m, ((OB) ATTR(ATTR(self,l),arr)), ((OB) NULL));
    ret_val34 = STR_ap2004550586(plus_self26, plus_sarg23);
    plus_self27 = ret_val34;
    plus_sarg24 = ((STR) &S_structarr_part);
    ret_val35 = STR_ap2004550586(plus_self27, plus_sarg24);
    res = ret_val35;
   }
   else {
    plus_self28 = res;
    plus_sarg25 = ((STR) &struct3);
    ret_val36 = STR_ap2004550586(plus_self28, plus_sarg25);
    plus_self29 = ret_val36;
    plus_sarg26 = MANGLE119219986(m, ((OB) ATTR(ATTR(self,l),arr)), ((OB) NULL));
    ret_val37 = STR_ap2004550586(plus_self29, plus_sarg26);
    plus_self30 = ret_val37;
    plus_sarg27 = ((STR) &S_structarr_part1);
    ret_val38 = STR_ap2004550586(plus_self30, plus_sarg27);
    res = ret_val38;
   }
  }
  plus_self31 = res;
  L49 = ATTR(ATTR(self,l),asize);
  L501_=(1>L49)?1:L49; 
  plus_arg3 = L501_;
  str_self3 = plus_arg3;
  clear_self = buf1;
  L52 = (clear_self==((FSTR) NULL));
  L531_=!(L52); 
  if (L531_) {
   SATTR(clear_self,loc,0);
  }
  str_in_self = str_self3;
  str_in_s = buf1;
  ret_val41 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
  buf1 = ret_val41;
  str_self4 = buf1;
  ret_val42 = STR_fr1097270334(((STR) NULL), str_self4);
  ret_val40 = ret_val42;
  s3 = ret_val40;
  ret_val39 = STR_ap1077157958(plus_self31, s3, TRUE);
  plus_self32 = ret_val39;
  plus_sarg28 = ((STR) &name188);
  ret_val43 = STR_ap2004550586(plus_self32, plus_sarg28);
  res = ret_val43;
 }
 if ((ATTR(ATTR(self,l),at)==((FMAPIDENTdTP) NULL))) {
  L54 = (ATTR(ATTR(self,l),arr)==((dTP) NULL));
 } else {
  L54 = FALSE;
 }
 if (L54) {
  plus_self33 = res;
  plus_sarg29 = ((STR) &CHARfi1162829196);
  ret_val44 = STR_ap2004550586(plus_self33, plus_sarg29);
  res = ret_val44;
 }
 plus_self34 = res;
 plus_sarg30 = ((STR) &name140);
 ret_val45 = STR_ap2004550586(plus_self34, plus_sarg30);
 plus_self35 = ret_val45;
 plus_sarg31 = cname;
 ret_val46 = STR_ap2004550586(plus_self35, plus_sarg31);
 plus_self36 = ret_val46;
 plus_sarg32 = ((STR) &name13);
 ret_val47 = STR_ap2004550586(plus_self36, plus_sarg32);
 plus_self37 = ret_val47;
 plus_sarg33 = ((STR) &static1);
 ret_val48 = STR_ap2004550586(plus_self37, plus_sarg33);
 plus_self38 = ret_val48;
 plus_sarg34 = cname;
 ret_val49 = STR_ap2004550586(plus_self38, plus_sarg34);
 plus_self39 = ret_val49;
 plus_sarg35 = ((STR) &name10);
 ret_val50 = STR_ap2004550586(plus_self39, plus_sarg35);
 plus_self40 = ret_val50;
 plus_sarg36 = cname;
 ret_val51 = STR_ap2004550586(plus_self40, plus_sarg36);
 plus_self41 = ret_val51;
 plus_sarg37 = ((STR) &S_zero2);
 ret_val52 = STR_ap2004550586(plus_self41, plus_sarg37);
 res = ret_val52;
 forbid_self1 = m;
 plus_self42 = cname;
 plus_sarg38 = ((STR) &S_zero);
 ret_val53 = STR_ap2004550586(plus_self42, plus_sarg38);
 forbid_s1 = ret_val53;
 SATTR(forbid_self1,forbidden,FSETST1404644833(ATTR(forbid_self1,forbidden), forbid_s1));
 plus_self43 = res;
 plus_sarg39 = ((STR) &typedefstruct);
 ret_val54 = STR_ap2004550586(plus_self43, plus_sarg39);
 plus_self44 = ret_val54;
 plus_sarg40 = cname;
 ret_val55 = STR_ap2004550586(plus_self44, plus_sarg40);
 plus_self45 = ret_val55;
 plus_sarg41 = ((STR) &S_boxed_struct1);
 ret_val56 = STR_ap2004550586(plus_self45, plus_sarg41);
 plus_self46 = ret_val56;
 plus_sarg42 = ((STR) &OB_HEADERheader);
 ret_val57 = STR_ap2004550586(plus_self46, plus_sarg42);
 plus_self47 = ret_val57;
 plus_arg4 = ' ';
 str_self5 = plus_arg4;
 create_self3 = ((STR) NULL);
 create_c3 = str_self5;
 L56 = 1;
 L58=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L56)*sizeof(CHAR);
 L57=ZALLOC_LEAF_BIG(L58);
 if (L57==NULL) FATAL("Unable to allocate more memory");
 memset(L57,0,L58);
 ((OB)L57)->header.tag=STR_tag;
#ifdef DESTROY_CHK

   ((OB)L57)->header.destroyed=0;
#endif

 L55 = ((STR) L57);
 L55->asize = L56;
 r3 = L55;
 SARR((STR)r3,0,create_c3); 
 ;
 ret_val60 = r3;
 ret_val59 = ret_val60;
 s4 = ret_val59;
 ret_val58 = STR_ap1077157958(plus_self47, s4, TRUE);
 plus_self48 = ret_val58;
 plus_sarg43 = cname;
 ret_val61 = STR_ap2004550586(plus_self48, plus_sarg43);
 plus_self49 = ret_val61;
 plus_sarg44 = ((STR) &immutable_part5);
 ret_val62 = STR_ap2004550586(plus_self49, plus_sarg44);
 plus_self50 = ret_val62;
 plus_sarg45 = ((STR) &name183);
 ret_val63 = STR_ap2004550586(plus_self50, plus_sarg45);
 plus_self51 = ret_val63;
 plus_sarg46 = cname;
 ret_val64 = STR_ap2004550586(plus_self51, plus_sarg46);
 plus_self52 = ret_val64;
 plus_sarg47 = ((STR) &S_boxed3);
 ret_val65 = STR_ap2004550586(plus_self52, plus_sarg47);
 res = ret_val65;
 forbid_self2 = m;
 plus_self53 = cname;
 plus_sarg48 = ((STR) &S_boxed2);
 ret_val66 = STR_ap2004550586(plus_self53, plus_sarg48);
 forbid_s2 = ret_val66;
 SATTR(forbid_self2,forbidden,FSETST1404644833(ATTR(forbid_self2,forbidden), forbid_s2));
 forbid_self3 = m;
 plus_self54 = cname;
 plus_sarg49 = ((STR) &S_boxed_struct2);
 ret_val67 = STR_ap2004550586(plus_self54, plus_sarg49);
 forbid_s3 = ret_val67;
 SATTR(forbid_self3,forbidden,FSETST1404644833(ATTR(forbid_self3,forbidden), forbid_s3));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR IMMUTA966323602(IMMUTA421759893 self) {
 return ATTR(self,str);
}


#undef IS_ITER
#define IS_ITER 0

STR TP_LAY473390218(TP_LAYOUT self) {
 return ATTR(self,str);
}


#undef IS_ITER
#define IS_ITER 0

STR TP_LAY690809295(TP_LAYOUT self, CGEN c) {
 STR ret_val;
 extern STR name1;
 ret_val = ((STR) &name1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dLAYOUT LAYOUT79932797(LAYOUT_TBL self, dTP t) {
 dLAYOUT ret_val;
 dLAYOUT l = ((dLAYOUT) NULL);
 EXTERNAL_LAYOUT create_self;
 dTP create_tp;
 PROG create_prog;
 EXTERNAL_LAYOUT ret_val1;
 EXTERNAL_LAYOUT r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self;
 STR plus_s;
 OUT ret_val3;
 FILE1 stdout_self;
 FILE1 ret_val4;
 FILE1 r1;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val5;
 FILE1 r2;
 dTP L1;
 dTP L2;
 dTP L3;
 dTP L4;
 dTP L5;
 INT L6;
 INT L7;
 BOOL L81_;
 dTP L9;
 INT L10;
 INT L11;
 BOOL L121_;
 dTP L13;
 FILE1 L14;
 OB L15;
 extern STR unrecognizedtype;
 FILE1 L17;
 OB L18;
 dTP L20;
 L1 = t;
 if ((*dTP_is914808564[TAG(L1)])(L1)) {
  l = ((dLAYOUT) BUILTI1036809386(((BUILTIN_LAYOUT) NULL), t));
 }
 else {
  L2 = t;
  if ((*dTP_is124163553[TAG(L2)])(L2)) {
   create_self = ((EXTERNAL_LAYOUT) NULL);
   create_tp = t;
   create_prog = LAYOUT_TBL_prog;
   r = EXTERN1518354897(((EXTERNAL_LAYOUT) NULL), create_tp);
   SATTR(r,l,CGEN_a1221397173(LAYOUT_TBL_cgen, create_tp));
   SATTR(r,prog,create_prog);
   ret_val1 = r;
   l = ((dLAYOUT) ret_val1);
  }
  else {
   L3 = t;
   if ((*dTP_is1811059018[TAG(L3)])(L3)) {
    l = ((dLAYOUT) ABSTRA762331980(((ABSTRACT_LAYOUT) NULL), t, LAYOUT_TBL_prog));
   }
   else {
    L4 = t;
    if ((*dTP_is735947028[TAG(L4)])(L4)) {
     L5 = t;
     L6 = (*dTP_kindrINT[TAG(L5)])(L5);
     L7 = TP_KIND_rout_tp;
     L81_=(L6)==(L7); 
     if (L81_) {
      l = ((dLAYOUT) BOUND_848335287(((BOUND_809390774) NULL), t));
     }
     else {
      L9 = t;
      L10 = (*dTP_kindrINT[TAG(L9)])(L9);
      L11 = TP_KIND_iter_tp;
      L121_=(L10)==(L11); 
      if (L121_) {
       l = ((dLAYOUT) BOUND_166620816(((BOUND_2140373124) NULL), t));
      }
      else {
       create_self1 = ((OUT) NULL);
       ret_val2 = create_self1;
       plus_self = ret_val2;
       L13 = t;
       plus_s = (*dTP_strrSTR[TAG(L13)])(L13);
       stdout_self = ((FILE1) NULL);
       L15=ZALLOC(sizeof(struct FILE1_struct));
       if (L15==NULL) FATAL("Unable to allocate more memory");
       ((OB)L15)->header.tag=FILE1_tag;
       L14 = ((FILE1) L15);
       r1 = L14;
       
       SATTR(r1,fp,stdout);
       ret_val4 = r1;
       FILE_plus_STR(ret_val4, plus_s);
       ret_val3 = plus_self;
       plus_self1 = ret_val3;
       plus_s1 = ((STR) &unrecognizedtype);
       stdout_self1 = ((FILE1) NULL);
       L18=ZALLOC(sizeof(struct FILE1_struct));
       if (L18==NULL) FATAL("Unable to allocate more memory");
       ((OB)L18)->header.tag=FILE1_tag;
       L17 = ((FILE1) L18);
       r2 = L17;
       
       SATTR(r2,fp,stdout);
       ret_val5 = r2;
       FILE_plus_STR(ret_val5, plus_s1);
      }
     }
    }
    else {
     L20 = t;
     if ((*dTP_is1334578382[TAG(L20)])(L20)) {
      l = ((dLAYOUT) IMMUTA431464701(((IMMUTA421759893) NULL), t, LAYOUT_TBL_prog));
     }
     else {
      l = ((dLAYOUT) CLASS_1959884515(((CLASS_LAYOUT) NULL), t, LAYOUT_TBL_prog));
     }
    }
   }
  }
 }
 ret_val = l;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void CLASS_1727096581(CLASS_LAYOUT self) {
 FLISTdLAYOUT res;
 FMAPIDENTdTP L11;
 dTP tp;
 FLISTdLAYOUT create_self;
 FLISTdLAYOUT ret_val;
 dTP L2;
 dTP L3;
 BOOL L4;
 BOOL L5;
 BOOL L61_;
 dTP L7;
 create_self = ((FLISTdLAYOUT) NULL);
 ret_val = ((FLISTdLAYOUT) NULL);
 res = ret_val;
 {
  struct FMAPID944928219_frame_struct other1_0;
FMAPID944928219_frame noname1 = &other1_0;
  L11 = ATTR(ATTR(self,l),at);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FMAPID944928219(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   tp = L2;
   L3 = tp;
   if ((*dTP_is1334578382[TAG(L3)])(L3)) {
    res = FLISTd1429133189(res, LAYOUT79932797(((LAYOUT_TBL) NULL), tp));
   }
  }
 }
 after_loop: ;
 L5 = (ATTR(ATTR(self,l),arr)==((dTP) NULL));
 L61_=!(L5); 
 if (L61_) {
  L7 = ATTR(ATTR(self,l),arr);
  L4 = (*dTP_is1334578382[TAG(L7)])(L7);
 } else {
  L4 = FALSE;
 }
 if (L4) {
  res = FLISTd1429133189(res, LAYOUT79932797(((LAYOUT_TBL) NULL), ATTR(ATTR(self,l),arr)));
 }
 SATTR(self,dependencies,res);
}


#undef IS_ITER
#define IS_ITER 0

void IMMUTA343315329(IMMUTA421759893 self) {
 FLISTdLAYOUT res;
 FMAPIDENTdTP L11;
 dTP tp;
 FLISTdLAYOUT create_self;
 FLISTdLAYOUT ret_val;
 dTP L2;
 dTP L3;
 BOOL L4;
 BOOL L5;
 BOOL L61_;
 dTP L7;
 create_self = ((FLISTdLAYOUT) NULL);
 ret_val = ((FLISTdLAYOUT) NULL);
 res = ret_val;
 {
  struct FMAPID944928219_frame_struct other1_0;
FMAPID944928219_frame noname1 = &other1_0;
  L11 = ATTR(ATTR(self,l),at);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FMAPID944928219(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   tp = L2;
   L3 = tp;
   if ((*dTP_is1334578382[TAG(L3)])(L3)) {
    res = FLISTd1429133189(res, LAYOUT79932797(((LAYOUT_TBL) NULL), tp));
   }
  }
 }
 after_loop: ;
 L5 = (ATTR(ATTR(self,l),arr)==((dTP) NULL));
 L61_=!(L5); 
 if (L61_) {
  L7 = ATTR(ATTR(self,l),arr);
  L4 = (*dTP_is1334578382[TAG(L7)])(L7);
 } else {
  L4 = FALSE;
 }
 if (L4) {
  res = FLISTd1429133189(res, LAYOUT79932797(((LAYOUT_TBL) NULL), ATTR(ATTR(self,l),arr)));
 }
 SATTR(self,dependencies,res);
}
