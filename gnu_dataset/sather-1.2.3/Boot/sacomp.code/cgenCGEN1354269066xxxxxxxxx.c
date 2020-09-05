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

typedef struct ARRAYSTR_struct {/* layout for ARRAY{STR} */
 OB_HEADER header;
 INT asize;
 STR arr_part[1];
 } *ARRAYSTR;

typedef struct BFILE_struct {/* layout for BFILE */
 OB_HEADER header;
 EXT_OB fp;
 } *BFILE;

typedef struct BIND_FORTRAN_struct {/* layout for BIND_FORTRAN */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *BIND_FORTRAN;

typedef struct CONFIG_TBL_struct {/* layout for CONFIG_TBL */
 OB_HEADER header;
 struct FMAPSTRdCONFIG_struct *tbl;
 } *CONFIG_TBL;

typedef struct ELT_NILSTR_struct {/* layout for ELT_NIL{STR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILSTR;

typedef struct ELT_NILTP_CLASS_struct {/* layout for ELT_NIL{TP_CLASS} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILTP_CLASS;

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

typedef struct INT_do676118316_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT arg1;/* Formal argument: i */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_I,I_u_Im; /* used by builtin iter */
 } *INT_do676118316_frame;

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

typedef struct TP_GRAPH_ABS_DES_struct {/* layout for TP_GRAPH_ABS_DES */
 OB_HEADER header;
 struct FMAPTP291739594_struct *tbl;
 struct PROG_struct *prog;
 struct TP_CLASS_struct *dollar_lock;
 struct TP_CLASS_struct *gate;
 struct TP_CLASS_struct *mutex;
 } *TP_GRAPH_ABS_DES;

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

typedef struct TUPdTPINT_struct {/* layout for TUP{$TP,INT} */
 struct dTP_struct *t1;
 INT t2;
 } TUPdTPINT;
static TUPdTPINT TUPdTPINT_zero;

typedef struct TUPdTPINT_boxed_struct {
 OB_HEADER header;
 TUPdTPINT immutable_part;
 } *TUPdTPINT_boxed;

typedef struct TUPSIG578692189_struct {/* layout for TUP{SIG,AM_ROUT_DEF} */
 struct AM_ROUT_DEF_struct *t2;
 struct SIG_struct *t1;
 } TUPSIG578692189;
static TUPSIG578692189 TUPSIG578692189_zero;

typedef struct TUPSIG578692189_boxed_struct {
 OB_HEADER header;
 TUPSIG578692189 immutable_part;
 } *TUPSIG578692189_boxed;

typedef struct TUPSIGSIG_struct {/* layout for TUP{SIG,SIG} */
 struct SIG_struct *t1;
 struct SIG_struct *t2;
 } TUPSIGSIG;
static TUPSIGSIG TUPSIGSIG_zero;

typedef struct TUPSIGSIG_boxed_struct {
 OB_HEADER header;
 TUPSIGSIG immutable_part;
 } *TUPSIGSIG_boxed;

typedef struct TUPSIGSTR_struct {/* layout for TUP{SIG,STR} */
 struct SIG_struct *t1;
 STR t2;
 } TUPSIGSTR;
static TUPSIGSTR TUPSIGSTR_zero;

typedef struct TUPSIGSTR_boxed_struct {
 OB_HEADER header;
 TUPSIGSTR immutable_part;
 } *TUPSIGSTR_boxed;

typedef struct TUPSTRFSETSTR_struct {/* layout for TUP{STR,FSET{STR}} */
 struct FSETSTR_struct *t2;
 STR t1;
 } TUPSTRFSETSTR;
static TUPSTRFSETSTR TUPSTRFSETSTR_zero;

typedef struct TUPSTRFSETSTR_boxed_struct {
 OB_HEADER header;
 TUPSTRFSETSTR immutable_part;
 } *TUPSTRFSETSTR_boxed;

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

typedef struct AM_STR_CONST_struct {/* layout for AM_STR_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 STR bval;
 } *AM_STR_CONST;

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

typedef struct FLISTdTP_struct {/* layout for FLIST{$TP} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dTP_struct *arr_part[1];
 } *FLISTdTP;

typedef struct FLISTd1740602786_frame_struct {
 INT state;
 FLISTdTP self;/* Formal argument: self */
 dTP ret_val1;
 INT i;
 INT sz;
 FLISTdTP aget_self;
 INT aget_ind;
 dTP ret_val;
 } *FLISTd1740602786_frame;

typedef struct FLISTA1893461511_struct {/* layout for FLIST{AM_BND_CREATE_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_BND1124877163_struct *arr_part[1];
 } *FLISTA1893461511;

typedef struct FLISTAM_ROUT_DEF_struct {/* layout for FLIST{AM_ROUT_DEF} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_ROUT_DEF_struct *arr_part[1];
 } *FLISTAM_ROUT_DEF;

typedef struct FLISTA2121868826_frame_struct {
 INT state;
 FLISTAM_ROUT_DEF self;/* Formal argument: self */
 AM_ROUT_DEF ret_val1;
 INT i;
 INT sz;
 FLISTAM_ROUT_DEF aget_self;
 INT aget_ind;
 AM_ROUT_DEF ret_val;
 } *FLISTA2121868826_frame;

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

typedef struct FMAPdTPINT_struct {/* layout for FMAP{$TP,INT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdTPINT_struct arr_part[1];
 } *FMAPdTPINT;

typedef struct FMAPdT705360156_frame_struct {
 INT state;
 FMAPdTPINT self;/* Formal argument: self */
 dTP ret_val1;
 FMAPdTPINT L61;
 dTP r;
 INT L31;
 FMAPdTPINT is_key_nil_self;
 dTP is_key_nil_e;
 BOOL ret_val;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPdT705360156_frame;

typedef struct FMAPdT1857608820_frame_struct {
 INT state;
 FMAPdTPINT self;/* Formal argument: self */
 TUPdTPINT ret_val1;
 FMAPdTPINT L61;
 TUPdTPINT r;
 INT L31;
 FMAPdTPINT is_key_nil_self;
 dTP is_key_nil_e;
 BOOL ret_val;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPdT1857608820_frame;

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

typedef struct FMAPSTRFSETSTR_struct {/* layout for FMAP{STR,FSET{STR}} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRFSETSTR_struct arr_part[1];
 } *FMAPSTRFSETSTR;

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

typedef struct FSETSTR_eltbrSTR_frame_struct {
 INT state;
 FSETSTR self;/* Formal argument: self */
 STR ret_val2;
 FSETSTR L61;
 STR r;
 INT i;
 INT sz;
 INT L31;
 FSETSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val;
 ELT_NILSTR is_elt_nil_self1;
 STR is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETSTR_eltbrSTR_frame;

typedef struct FSETTUPSIGSIG_struct {/* layout for FSET{TUP{SIG,SIG}} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct TUPSIGSIG_struct arr_part[1];
 } *FSETTUPSIGSIG;

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

extern INOUT_MODE MODES_inout_mode;
extern INT INLINE1173790597;
extern INT INLINE1232309177;
extern INT INLINE1258550073;
extern INT INLINE1301544351;
extern INT INLINE1306635001;
extern INT INLINE1401026874;
extern INT INLINE1516217386;
extern INT INLINE1950426034;
extern INT INLINE2014794524;
extern INT INLINE520918393;
extern INT INLINE598259372;
extern INT INLINE957944682;
extern OUT_MODE MODES_out_mode;
extern TP_CLASS TP_BUI1032249067;
extern TP_CLASS TP_BUI1805763264;
extern TP_CLASS TP_BUI2088674906;
extern TP_CLASS TP_BUI389447236;
extern TP_CLASS TP_BUI633637838;
extern TP_CLASS TP_BUILTIN_bool;
extern TP_CLASS TP_BUILTIN_char;
extern TP_CLASS TP_BUILTIN_flt;
extern TP_CLASS TP_BUILTIN_fltd;
extern TP_CLASS TP_BUILTIN_int;
extern TP_CLASS TP_BUILTIN_str;

/* Function declarations */


extern RETURNED_CONST BOOL (**dMODE_814247358)(dMODE, dMODE);

extern RETURNED_CONST BOOL (**dTP_is1334578382)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1553335918)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1811059018)(dTP);

extern RETURNED_CONST BOOL (**dTP_is242312711)(dTP, OB);

extern RETURNED_CONST BOOL (**dTP_is418055720)(dTP);

extern RETURNED_CONST IFC (**dTP_ifcrIFC)(dTP);

extern RETURNED_CONST IMPL (**dTP_implrIMPL)(dTP);

extern RETURNED_CONST STR (**dSTR_strrSTR)(dSTR);

extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);
AM_ROUT_DEF FLISTA2121868826(FLISTA2121868826_frame);
AM_ROUT_DEF FMAPSI2016001247(FMAPSI518162669, SIG);
ARG ARRAYA827580689(ARRAYA827580689_frame);
BOOL CGEN_a952489530(CGEN);
BOOL FSETSI722431140(FSETSIG, SIG);
BOOL SIG_is418491101(SIG);
BOOL STR_is111530248(STR, STR);
FLISTdTP FLISTd1953924847(FLISTdTP, dTP);
FMAPSI518162669 FMAPSI1625125906(FMAPSI518162669, SIG);
FMAPSIGSTR FMAPSI1897406528(FMAPSIGSTR, SIG, STR);
FSETAM_ROUT_DEF FSETAM2044524049(FSETAM_ROUT_DEF, AM_ROUT_DEF);
FSETSIG FSETSI671175003(FSETSIG, SIG);
FSETSTR FMAPST521088757(FMAPSTRFSETSTR, STR);
FSETSTR FSETST1404644833(FSETSTR, STR);
FSETTUPSIGSIG FSETTU2063384524(FSETTUPSIGSIG, TUPSIGSIG);
FSETdTP FMAPTP861983338(FMAPTP291739594, TP_CLASS);
FSETdTP FSETdT1016814448(FSETdTP, dTP);
FSTR BFILE_fstrrFSTR(BFILE);
FSTR FSTR_p1410513982(FSTR, FSTR);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR FSTR_p399773021(FSTR, CHAR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
FSTR PRINT_1557717939(PRINT_OB, PROG, CGEN);
INT FLISTA919540447(FLISTA1893461511);
INT FMAPdT1077886323(FMAPdTPINT);
INT FMAPdT165518335(FMAPdTPINT, dTP);
INT FSETAM1999114804(FSETAM_ROUT_DEF);
INT IMPL_a1897060099(IMPL);
INT INT_do676118316(INT_do676118316_frame);
INT INT_up418511718(INT_up418511718_frame);
INT INT_upbrINT(INT_upbrINT_frame);
INT STR_lengthrINT(STR);
INT STR_se1633361857(STR, CHAR);
INT STR_sizerINT(STR);
SIG IFC_si2085565042(IFC, SIG);
STR ARRAYS1578977701(ARRAYS1578977701_frame);
STR CGEN_d1915593494(CGEN, dTP, dTP, STR, BOOL, dMODE);
STR CGEN_e1296302677(CGEN, SIG, SIG);
STR CGEN_e1633118543(CGEN, AM_STR_CONST);
STR CGEN_e56867199(CGEN, SIG, SIG);
STR CGEN_f1593827635(CGEN, SIG);
STR CGEN_f2084214268(CGEN, dTP);
STR CGEN_g1520354813(CGEN, SIG);
STR CGEN_p1740273317(CGEN, SIG, dAM_CALL_EXPR, ARRAYSTR);
STR CGEN_v205291382(CGEN, dTP, STR, STR);
STR CODE_F1366222275(CODE_FILE);
STR CONFIG1397479570(CONFIG_TBL, STR, INT);
STR FLISTS1171631134(FLISTS1171631134_frame);
STR FMAPSI1519487311(FMAPSIGSTR, SIG);
STR FSETSTR_eltbrSTR(FSETSTR_eltbrSTR_frame);
STR MANGLE119219986(MANGLE, OB, OB);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap1417971546(STR, STR, STR, STR);
STR STR_ap1693864410(STR, STR, STR);
STR STR_ap2004550586(STR, STR);
STR STR_ap44172742(STR, STR, STR, STR, STR);
STR STR_fr1097270334(STR, FSTR);
STR STR_head_INTrSTR(STR, INT);
STR STR_se405450305(STR_se405450305_frame);
STR STR_su1627796404(STR, INT);
STR STR_tail_INTrSTR(STR, INT);
STR TP_CLASS_strrSTR(TP_CLASS);
TP_CLASS TP_CLA115798516(TP_CLA115798516_frame);
TUPdTPINT FMAPdT1857608820(FMAPdT1857608820_frame);
dLAYOUT LAYOUT79932797(LAYOUT_TBL, dTP);
dTP ARRAYd2147017943(ARRAYd2147017943_frame);
dTP FLISTd1740602786(FLISTd1740602786_frame);
dTP FMAPdT705360156(FMAPdT705360156_frame);
dTP FSETdTP_eltbrdTP(FSETdTP_eltbrdTP_frame);
void ARRAYS1161048019(ARRAYS1161048019_frame);
void ARRAYS1450857938(ARRAYSTR, INT, INT);
void ARRAYd1239612935(ARRAYdTP, INT, INT);
void ARRAYd1730019671(ARRAYd1730019671_frame);
void CGEN_d498181638(CGEN);
void CGEN_d695831906(CGEN);
void CGEN_e1130765144(CGEN, AM_ROUT_DEF);
void CGEN_e1935776612(CGEN, AM_ROUT_DEF);
void CGEN_finalize(CGEN);
void CGEN_g1348388497(CGEN);
void CGEN_g1356172775(CGEN);
void CGEN_g1629839129(CGEN);
void CGEN_g1874773404(CGEN);
void CGEN_g333195909(CGEN);
void CGEN_g637688959(CGEN);
void CGEN_m875729145(CGEN);
void CODE_F1291349628(CODE_FILE, SIG);
void CODE_F2105979088(CODE_FILE);
void CODE_F525364269(CODE_FILE, dLAYOUT);
void FILE_plus_STR(FILE1, STR);
void FILE_plus_dSTR(FILE1, dSTR);
void PROG_e176405615(PROG, dPROG_ERR);
void PROG_err_STR(PROG, STR);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

STR CGEN_e1296302677(CGEN self, SIG s, SIG abs1) {
 STR ret_val;
 STR res;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 CODE_FILE plus_self1;
 dSTR plus_s;
 CODE_FILE ret_val3;
 CODE_FILE plus_self2;
 dSTR plus_s1;
 CODE_FILE ret_val4;
 CODE_FILE plus_self3;
 dSTR plus_s2;
 CODE_FILE ret_val5;
 CODE_FILE plus_self4;
 dSTR plus_s3;
 CODE_FILE ret_val6;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val7;
 CODE_FILE plus_self5;
 dSTR plus_s4;
 CODE_FILE ret_val8;
 CODE_FILE plus_self6;
 dSTR plus_s5;
 CODE_FILE ret_val9;
 CODE_FILE plus_self7;
 dSTR plus_s6;
 extern STR S_frame_alloc;
 BOOL L1;
 BOOL L21_;
 extern STR void12;
 BOOL L3;
 BOOL L41_;
 FSTR L5;
 dSTR L6;
 BOOL L7;
 BOOL L81_;
 FSTR L9;
 dSTR L10;
 extern STR name172;
 BOOL L11;
 BOOL L121_;
 FSTR L13;
 dSTR L14;
 extern STR returnOBALLOC;
 BOOL L15;
 BOOL L161_;
 FSTR L17;
 dSTR L18;
 BOOL L19;
 BOOL L201_;
 FSTR L22;
 dSTR L23;
 extern STR S_frame5;
 BOOL L24;
 BOOL L251_;
 FSTR L26;
 dSTR L27;
 extern STR name32;
 FSTR L28;
 dSTR L29;
 mang_self = self;
 mang_ob = ((OB) s);
 ret_val1 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
 plus_self = ret_val1;
 plus_sarg = ((STR) &S_frame_alloc);
 ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
 res = ret_val2;
 L1 = FSETSI722431140(ATTR(self,emitted_iter_all), s);
 L21_=!(L1); 
 if (L21_) {
  SATTR(self,emitted_iter_all,FSETSI671175003(ATTR(self,emitted_iter_all), s));
  plus_self1 = ATTR(self,unbox_c);
  plus_s = ((dSTR) ((STR) &void12));
  L3 = (plus_s==((dSTR) NULL));
  L41_=!(L3); 
  if (L41_) {
   L5 = ATTR(plus_self1,ntext);
   L6 = plus_s;
   SATTR(plus_self1,ntext,FSTR_p1752847026(L5, (*dSTR_strrSTR[TAG(L6)])(L6)));
  }
  ret_val3 = plus_self1;
  plus_self2 = ret_val3;
  plus_s1 = ((dSTR) res);
  L7 = (plus_s1==((dSTR) NULL));
  L81_=!(L7); 
  if (L81_) {
   L9 = ATTR(plus_self2,ntext);
   L10 = plus_s1;
   SATTR(plus_self2,ntext,FSTR_p1752847026(L9, (*dSTR_strrSTR[TAG(L10)])(L10)));
  }
  ret_val4 = plus_self2;
  plus_self3 = ret_val4;
  plus_s2 = ((dSTR) ((STR) &name172));
  L11 = (plus_s2==((dSTR) NULL));
  L121_=!(L11); 
  if (L121_) {
   L13 = ATTR(plus_self3,ntext);
   L14 = plus_s2;
   SATTR(plus_self3,ntext,FSTR_p1752847026(L13, (*dSTR_strrSTR[TAG(L14)])(L14)));
  }
  ret_val5 = plus_self3;
  plus_self4 = ret_val5;
  plus_s3 = ((dSTR) ((STR) &returnOBALLOC));
  L15 = (plus_s3==((dSTR) NULL));
  L161_=!(L15); 
  if (L161_) {
   L17 = ATTR(plus_self4,ntext);
   L18 = plus_s3;
   SATTR(plus_self4,ntext,FSTR_p1752847026(L17, (*dSTR_strrSTR[TAG(L18)])(L18)));
  }
  ret_val6 = plus_self4;
  plus_self5 = ret_val6;
  mang_self1 = self;
  mang_ob1 = ((OB) s);
  ret_val7 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
  plus_s4 = ((dSTR) ret_val7);
  L19 = (plus_s4==((dSTR) NULL));
  L201_=!(L19); 
  if (L201_) {
   L22 = ATTR(plus_self5,ntext);
   L23 = plus_s4;
   SATTR(plus_self5,ntext,FSTR_p1752847026(L22, (*dSTR_strrSTR[TAG(L23)])(L23)));
  }
  ret_val8 = plus_self5;
  plus_self6 = ret_val8;
  plus_s5 = ((dSTR) ((STR) &S_frame5));
  L24 = (plus_s5==((dSTR) NULL));
  L251_=!(L24); 
  if (L251_) {
   L26 = ATTR(plus_self6,ntext);
   L27 = plus_s5;
   SATTR(plus_self6,ntext,FSTR_p1752847026(L26, (*dSTR_strrSTR[TAG(L27)])(L27)));
  }
  ret_val9 = plus_self6;
  plus_self7 = ret_val9;
  plus_s6 = ((dSTR) ((STR) &name32));
  L28 = ATTR(plus_self7,ntext);
  L29 = plus_s6;
  SATTR(plus_self7,ntext,FSTR_p1752847026(L28, (*dSTR_strrSTR[TAG(L29)])(L29)));
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e56867199(CGEN self, SIG s, SIG abs1) {
 STR ret_val;
 STR call11 = ((STR) NULL);
 STR decl = ((STR) NULL);
 BOOL w = BOOL_zero;
 ARRAYARG L11;
 ARRAYARG L21;
 STR res;
 STR pog;
 ARRAYARG L31;
 ARG a;
 dTP set;
 ARRAYARG L41;
 ARG absa;
 dTP abset;
 INT L51 = INT_zero;
 INT idx = INT_zero;
 STR arg_str;
 ARRAYSTR arg_list;
 INT L61 = INT_zero;
 INT L71 = INT_zero;
 INT i = INT_zero;
 INT L81 = INT_zero;
 INT L91 = INT_zero;
 INT i1 = INT_zero;
 ARG is_eq_self;
 ARG is_eq_a;
 BOOL ret_val1 = BOOL_zero;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val2;
 CODE_FILE uses_unbox_self;
 SIG uses_unbox_s;
 SIG uses_unbox_abs;
 TUPSIGSIG create_self = TUPSIGSIG_zero;
 SIG create_at1;
 SIG create_at2;
 TUPSIGSIG ret_val3 = TUPSIGSIG_zero;
 SIG is_iter_self;
 BOOL ret_val4 = BOOL_zero;
 IDENT is_iter_self1 = IDENT_zero;
 BOOL ret_val5 = BOOL_zero;
 CODE_FILE uses_unbox_self1;
 SIG uses_unbox_s1;
 SIG uses_unbox_abs1;
 TUPSIGSIG create_self1 = TUPSIGSIG_zero;
 SIG create_at11;
 SIG create_at21;
 TUPSIGSIG ret_val6 = TUPSIGSIG_zero;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val7;
 STR plus_self;
 STR plus_sarg;
 STR ret_val8;
 CODE_FILE plus_self1;
 dSTR plus_s;
 STR plus_self2;
 STR plus_sarg1;
 STR ret_val9;
 CGEN comment_self;
 CODE_FILE comment_f;
 STR comment_com;
 CODE_FILE plus_self3;
 dSTR plus_s1;
 CODE_FILE ret_val10;
 CODE_FILE plus_self4;
 dSTR plus_s2;
 CODE_FILE ret_val11;
 CODE_FILE plus_self5;
 dSTR plus_s3;
 CODE_FILE plus_self6;
 dSTR plus_s4;
 CODE_FILE plus_self7;
 dSTR plus_s5;
 CGEN mang_self2;
 OB mang_ob2;
 STR ret_val12;
 CODE_FILE plus_self8;
 dSTR plus_s6;
 CODE_FILE ret_val13;
 CODE_FILE plus_self9;
 dSTR plus_s7;
 SIG is_iter_self2;
 BOOL ret_val14 = BOOL_zero;
 IDENT is_iter_self3 = IDENT_zero;
 BOOL ret_val15 = BOOL_zero;
 CODE_FILE plus_self10;
 dSTR plus_s8;
 CODE_FILE ret_val16;
 CODE_FILE plus_self11;
 dSTR plus_s9;
 CODE_FILE ret_val17;
 CGEN mang_self3;
 OB mang_ob3;
 STR ret_val18;
 CODE_FILE plus_self12;
 dSTR plus_s10;
 CODE_FILE ret_val19;
 CODE_FILE plus_self13;
 dSTR plus_s11;
 CODE_FILE ret_val20;
 CODE_FILE plus_self14;
 dSTR plus_s12;
 CODE_FILE ret_val21;
 CODE_FILE plus_self15;
 dSTR plus_s13;
 CODE_FILE plus_self16;
 dSTR plus_s14;
 CODE_FILE ret_val22;
 CODE_FILE plus_self17;
 dSTR plus_s15;
 CODE_FILE ret_val23;
 CGEN mang_self4;
 OB mang_ob4;
 STR ret_val24;
 CODE_FILE plus_self18;
 dSTR plus_s16;
 CODE_FILE ret_val25;
 CODE_FILE plus_self19;
 dSTR plus_s17;
 CODE_FILE plus_self20;
 dSTR plus_s18;
 CODE_FILE ret_val26;
 CODE_FILE plus_self21;
 dSTR plus_s19;
 CODE_FILE ret_val27;
 CGEN mang_self5;
 OB mang_ob5;
 STR ret_val28;
 CODE_FILE plus_self22;
 dSTR plus_s20;
 CODE_FILE ret_val29;
 CODE_FILE plus_self23;
 dSTR plus_s21;
 INT L101 = INT_zero;
 CGEN boxed_arg_type_s;
 ARG boxed_arg_type_s1;
 STR ret_val30;
 STR res1;
 CGEN mang_self6;
 OB mang_ob6;
 STR ret_val31;
 STR plus_self24;
 STR plus_sarg2;
 STR ret_val32;
 INT str_self = INT_zero;
 STR ret_val33;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val34;
 FSTR str_self1;
 STR ret_val35;
 CODE_FILE plus_self25;
 dSTR plus_s22;
 CGEN arg_type_str_sel;
 ARG arg_type_str_e;
 STR ret_val36;
 STR res2;
 CGEN mang_self7;
 OB mang_ob7;
 STR ret_val37;
 STR plus_self26;
 STR plus_sarg3;
 STR ret_val38;
 INT str_self2 = INT_zero;
 STR ret_val39;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val40;
 FSTR str_self3;
 STR ret_val41;
 CODE_FILE plus_self27;
 dSTR plus_s23;
 CODE_FILE plus_self28;
 dSTR plus_s24;
 CODE_FILE ret_val42;
 CODE_FILE plus_self29;
 dSTR plus_s25;
 FSTR create_self2;
 FSTR ret_val43;
 SIG is_iter_self4;
 BOOL ret_val44 = BOOL_zero;
 IDENT is_iter_self5 = IDENT_zero;
 BOOL ret_val45 = BOOL_zero;
 CGEN mang_self8;
 OB mang_ob8;
 STR ret_val46;
 STR plus_self30;
 STR plus_sarg4;
 STR ret_val47;
 CGEN mang_self9;
 OB mang_ob9;
 STR ret_val48;
 STR plus_self31;
 STR plus_sarg5;
 STR ret_val49;
 STR plus_self32;
 STR plus_sarg6;
 STR ret_val50;
 CGEN mang_self10;
 OB mang_ob10;
 STR ret_val51;
 STR plus_self33;
 CHAR plus_arg = CHAR_zero;
 STR ret_val52;
 STR s1;
 CHAR str_self4 = CHAR_zero;
 STR ret_val53;
 STR create_self3;
 CHAR create_c = CHAR_zero;
 STR ret_val54;
 STR r;
 CGEN mang_self11;
 OB mang_ob11;
 STR ret_val55;
 STR plus_self34;
 STR plus_sarg7;
 STR ret_val56;
 STR plus_self35;
 STR plus_sarg8;
 STR ret_val57;
 STR plus_self36;
 STR plus_sarg9;
 STR ret_val58;
 STR plus_self37;
 STR plus_sarg10;
 STR ret_val59;
 STR plus_self38;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val60;
 STR s2;
 CHAR str_self5 = CHAR_zero;
 STR ret_val61;
 STR create_self4;
 CHAR create_c1 = CHAR_zero;
 STR ret_val62;
 STR r1;
 STR plus_self39;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val63;
 STR s3;
 CHAR str_self6 = CHAR_zero;
 STR ret_val64;
 STR create_self5;
 CHAR create_c2 = CHAR_zero;
 STR ret_val65;
 STR r2;
 CGEN cast_self;
 dTP cast_dest_tp;
 dTP cast_src_tp;
 STR cast_expr;
 BOOL cast_only_boxing = BOOL_zero;
 STR ret_val66;
 STR plus_self40;
 STR plus_sarg11;
 STR ret_val67;
 CGEN mang_self12;
 OB mang_ob12;
 STR ret_val68;
 STR plus_self41;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val69;
 STR s4;
 CHAR str_self7 = CHAR_zero;
 STR ret_val70;
 STR create_self6;
 CHAR create_c3 = CHAR_zero;
 STR ret_val71;
 STR r3;
 STR plus_self42;
 STR plus_sarg12;
 STR ret_val72;
 STR plus_self43;
 STR plus_sarg13;
 STR ret_val73;
 SIG num_args_self;
 INT ret_val74 = INT_zero;
 ARRAYSTR create_self7;
 INT create_n = INT_zero;
 ARRAYSTR ret_val75;
 CGEN cast_self1;
 dTP cast_dest_tp1;
 dTP cast_src_tp1;
 STR cast_expr1;
 BOOL cast_only_boxing1 = BOOL_zero;
 STR ret_val76;
 SIG num_args_self1;
 INT ret_val77 = INT_zero;
 STR plus_self44;
 INT plus_arg4 = INT_zero;
 STR ret_val78;
 STR s5;
 INT str_self8 = INT_zero;
 STR ret_val79;
 FSTR buf3 = ((FSTR) NULL);
 FSTR clear_self2;
 INT str_in_self2 = INT_zero;
 FSTR str_in_s2;
 FSTR ret_val80;
 FSTR str_self9;
 STR ret_val81;
 CGEN cast_self2;
 dTP cast_dest_tp2;
 dTP cast_src_tp2;
 STR cast_expr2;
 BOOL cast_only_boxing2 = BOOL_zero;
 STR ret_val82;
 CGEN mang_self13;
 OB mang_ob13;
 STR ret_val83;
 STR plus_self45;
 CHAR plus_arg5 = CHAR_zero;
 STR ret_val84;
 STR s6;
 CHAR str_self10 = CHAR_zero;
 STR ret_val85;
 STR create_self8;
 CHAR create_c4 = CHAR_zero;
 STR ret_val86;
 STR r4;
 CGEN cast_self3;
 dTP cast_dest_tp3;
 dTP cast_src_tp3;
 STR cast_expr3;
 BOOL cast_only_boxing3 = BOOL_zero;
 STR ret_val87;
 STR plus_self46;
 STR plus_sarg14;
 STR ret_val88;
 CGEN mang_self14;
 OB mang_ob14;
 STR ret_val89;
 STR plus_self47;
 CHAR plus_arg6 = CHAR_zero;
 STR ret_val90;
 STR s7;
 CHAR str_self11 = CHAR_zero;
 STR ret_val91;
 STR create_self9;
 CHAR create_c5 = CHAR_zero;
 STR ret_val92;
 STR r5;
 CGEN mang_self15;
 OB mang_ob15;
 STR ret_val93;
 STR plus_self48;
 STR plus_sarg15;
 STR ret_val94;
 SIG num_args_self2;
 INT ret_val95 = INT_zero;
 STR plus_self49;
 STR plus_sarg16;
 STR ret_val96;
 STR plus_self50;
 INT plus_arg7 = INT_zero;
 STR ret_val97;
 STR s8;
 INT str_self12 = INT_zero;
 STR ret_val98;
 FSTR buf4 = ((FSTR) NULL);
 FSTR clear_self3;
 INT str_in_self3 = INT_zero;
 FSTR str_in_s3;
 FSTR ret_val99;
 FSTR str_self13;
 STR ret_val100;
 CGEN cast_arg_self;
 dTP cast_arg_dest_tp;
 dTP cast_arg_src_tp;
 STR cast_arg_expr;
 BOOL cast_arg_only_bo = BOOL_zero;
 dMODE cast_arg_mode;
 STR ret_val101;
 STR plus_self51;
 STR plus_sarg17;
 STR ret_val102;
 STR plus_self52;
 STR plus_sarg18;
 STR ret_val103;
 CGEN arg_type_str_sel1;
 ARG arg_type_str_e1;
 STR ret_val104;
 STR res3;
 CGEN mang_self16;
 OB mang_ob16;
 STR ret_val105;
 STR plus_self53;
 STR plus_sarg19;
 STR ret_val106;
 STR plus_self54;
 STR plus_sarg20;
 STR ret_val107;
 STR plus_self55;
 STR plus_sarg21;
 STR ret_val108;
 STR plus_self56;
 STR plus_sarg22;
 STR ret_val109;
 STR plus_self57;
 CHAR plus_arg8 = CHAR_zero;
 STR ret_val110;
 STR s9;
 CHAR str_self14 = CHAR_zero;
 STR ret_val1111;
 STR create_self10;
 CHAR create_c6 = CHAR_zero;
 STR ret_val112;
 STR r6;
 STR plus_self58;
 CHAR plus_arg9 = CHAR_zero;
 STR ret_val113;
 STR s10;
 CHAR str_self15 = CHAR_zero;
 STR ret_val114;
 STR create_self11;
 CHAR create_c7 = CHAR_zero;
 STR ret_val115;
 STR r7;
 CGEN cast_self4;
 dTP cast_dest_tp4;
 dTP cast_src_tp4;
 STR cast_expr4;
 BOOL cast_only_boxing4 = BOOL_zero;
 STR ret_val116;
 STR plus_self59;
 STR plus_sarg23;
 STR ret_val117;
 CGEN mang_self17;
 OB mang_ob17;
 STR ret_val118;
 STR plus_self60;
 CHAR plus_arg10 = CHAR_zero;
 STR ret_val119;
 STR s11;
 CHAR str_self16 = CHAR_zero;
 STR ret_val120;
 STR create_self12;
 CHAR create_c8 = CHAR_zero;
 STR ret_val121;
 STR r8;
 STR plus_self61;
 STR plus_sarg24;
 STR ret_val122;
 STR plus_self62;
 STR plus_sarg25;
 STR ret_val123;
 CODE_FILE plus_self63;
 dSTR plus_s26;
 CODE_FILE ret_val124;
 CODE_FILE plus_self64;
 dSTR plus_s27;
 CODE_FILE ret_val125;
 CODE_FILE plus_self65;
 dSTR plus_s28;
 CODE_FILE ret_val126;
 CODE_FILE plus_self66;
 dSTR plus_s29;
 CODE_FILE ret_val127;
 CODE_FILE plus_self67;
 dSTR plus_s30;
 CODE_FILE ret_val128;
 CODE_FILE plus_self68;
 dSTR plus_s31;
 CODE_FILE plus_self69;
 dSTR plus_s32;
 CODE_FILE ret_val129;
 CODE_FILE plus_self70;
 dSTR plus_s33;
 CODE_FILE ret_val130;
 CODE_FILE plus_self71;
 dSTR plus_s34;
 CODE_FILE ret_val131;
 CODE_FILE plus_self72;
 dSTR plus_s35;
 CODE_FILE ret_val132;
 CODE_FILE plus_self73;
 dSTR plus_s36;
 CODE_FILE ret_val133;
 CODE_FILE plus_self74;
 dSTR plus_s37;
 BOOL L12;
 BOOL L131_;
 BOOL L14;
 dTP L15;
 dTP L16;
 INT L171_br;
INT i_L171_=0;
 ARG aL171_;
 INT L181_br;
INT i_L181_=0;
 ARG aL181_;
 ARG L19;
 ARG L20;
 BOOL L22;
 dTP L23;
 dMODE L24;
 BOOL L25;
 BOOL L261_;
 BOOL L27;
 BOOL L281_;
 TUPSIGSIG L29;
 TUPSIGSIG L30;
 BOOL L32;
 BOOL L33;
 BOOL L341_;
 BOOL L35;
 BOOL L36;
 BOOL L371_;
 STR L38;
 INT L39;
 INT L401_;
 INT L42;
 CHAR L431_;
 CHAR L44;
 BOOL L451_;
 TUPSIGSIG L46;
 TUPSIGSIG L47;
 extern STR S_unbox;
 dSTR L48;
 OB L49;
 FSTR L50;
 dSTR L52;
 extern STR Wrappertounbox;
 extern STR name78;
 BOOL L53;
 BOOL L541_;
 FSTR L55;
 dSTR L56;
 BOOL L57;
 BOOL L581_;
 FSTR L59;
 dSTR L60;
 extern STR name79;
 FSTR L62;
 dSTR L63;
 dSTR L64;
 OB L65;
 FSTR L66;
 dSTR L67;
 extern STR void6;
 FSTR L68;
 dSTR L69;
 BOOL L70;
 BOOL L721_;
 FSTR L73;
 dSTR L74;
 dSTR L75;
 OB L76;
 FSTR L77;
 dSTR L78;
 extern STR name1;
 extern STR struct1621582358;
 BOOL L79;
 BOOL L80;
 BOOL L821_;
 STR L83;
 INT L84;
 INT L851_;
 INT L86;
 CHAR L871_;
 CHAR L88;
 BOOL L891_;
 BOOL L90;
 BOOL L921_;
 FSTR L93;
 dSTR L94;
 dSTR L95;
 OB L96;
 BOOL L97;
 BOOL L981_;
 FSTR L99;
 dSTR L100;
 BOOL L102;
 BOOL L1031_;
 FSTR L104;
 dSTR L105;
 extern STR S_frameframe;
 BOOL L106;
 BOOL L1071_;
 FSTR L108;
 dSTR L109;
 BOOL L110;
 BOOL L1111_;
 FSTR L112;
 dSTR L113;
 extern STR name164;
 FSTR L114;
 dSTR L115;
 BOOL L116;
 dTP L117;
 BOOL L118;
 dTP L119;
 dTP L120;
 BOOL L121;
 BOOL L1221_;
 FSTR L123;
 dSTR L124;
 extern STR name24;
 BOOL L125;
 BOOL L1261_;
 FSTR L127;
 dSTR L128;
 BOOL L129;
 BOOL L1301_;
 FSTR L132;
 dSTR L133;
 extern STR S_boxedself;
 FSTR L134;
 dSTR L135;
 BOOL L136;
 BOOL L1371_;
 FSTR L138;
 dSTR L139;
 extern STR name24;
 BOOL L140;
 BOOL L1411_;
 FSTR L142;
 dSTR L143;
 BOOL L144;
 BOOL L1451_;
 FSTR L146;
 dSTR L147;
 extern STR self8;
 FSTR L148;
 dSTR L149;
 INT L1501_br;
 ARG aL1501_;
 INT L1511_br;
 ARG aL1511_;
 INT L1521_;
 ARG L153;
 ARG L154;
 INT L155;
 BOOL L156;
 dTP L157;
 BOOL L158;
 dTP L159;
 dTP L160;
 extern STR name29;
 extern STR S_boxed2;
 BOOL L161;
 dMODE L162;
 dMODE L163;
 extern STR name58;
 extern STR arg4;
 BOOL L164;
 BOOL L1651_;
 FSTR L166;
 dSTR L167;
 extern STR name29;
 BOOL L168;
 dMODE L169;
 dMODE L170;
 extern STR name58;
 extern STR arg4;
 BOOL L172;
 BOOL L1731_;
 FSTR L174;
 dSTR L175;
 INT L1761_;
 BOOL L177;
 BOOL L1781_;
 FSTR L179;
 dSTR L180;
 extern STR name164;
 FSTR L182;
 dSTR L183;
 FSTR L184;
 INT L185;
 OB L186;
 INT L187;
 BOOL L188;
 BOOL L189;
 BOOL L1901_;
 STR L191;
 INT L192;
 INT L1931_;
 INT L194;
 CHAR L1951_;
 CHAR L196;
 BOOL L1971_;
 extern STR name75;
 extern STR S_frameframe1;
 STR L198;
 INT L199;
 OB L200;
 INT L201;
 extern STR S_frame4;
 extern STR pFF1;
 extern STR struct1392482001;
 STR L203;
 INT L204;
 OB L205;
 INT L206;
 STR L208;
 INT L209;
 OB L210;
 INT L211;
 BOOL L213;
 BOOL L2141_;
 extern STR return4;
 STR L215;
 INT L216;
 OB L217;
 INT L218;
 extern STR void6;
 extern STR arg21;
 ARRAYARG L220;
 INT L2211_;
 INT L222;
 INT L2231_;
 ARRAYSTR L224;
 INT L225;
 OB L226;
 INT L227;
 extern STR self1;
 STR L228;
 ARRAYARG L231;
 INT L2321_;
 INT L2301_,L2301_m;
 INT L233;
 ARRAYARG L234;
 INT L2351_;
 INT L236;
 ARG L2371_;
 ARG L238;
 ARRAYARG L239;
 INT L2401_;
 INT L241;
 ARG L2421_;
 ARG L243;
 BOOL L244;
 BOOL L2451_;
 STR L246;
 STR L248;
 INT L249;
 OB L250;
 INT L251;
 extern STR self1;
 STR L253;
 INT L254;
 OB L255;
 INT L256;
 ARRAYARG L259;
 INT L2601_;
 INT L2581_,L2581_m;
 INT L262;
 extern STR name29;
 ARRAYARG L263;
 INT L2641_;
 INT L265;
 ARG L2661_;
 ARG L267;
 ARRAYARG L268;
 INT L2691_;
 INT L270;
 ARG L2711_;
 ARG L272;
 BOOL L273;
 BOOL L2741_;
 ARRAYARG L275;
 INT L2761_;
 INT L277;
 ARG L2781_;
 ARG L279;
 extern STR name29;
 ARRAYARG L280;
 INT L2821_;
 INT L283;
 ARG L2841_;
 BOOL L285;
 dMODE L286;
 dMODE L287;
 extern STR name58;
 extern STR pFF1;
 extern STR struct1392482001;
 STR L288;
 INT L289;
 OB L290;
 INT L291;
 STR L293;
 INT L294;
 OB L295;
 INT L296;
 BOOL L298;
 BOOL L2991_;
 extern STR return4;
 STR L300;
 INT L301;
 OB L302;
 INT L303;
 extern STR void6;
 extern STR name1;
 dSTR L305;
 OB L306;
 BOOL L307;
 BOOL L3081_;
 FSTR L309;
 dSTR L310;
 BOOL L311;
 BOOL L3121_;
 FSTR L313;
 dSTR L314;
 extern STR name38;
 BOOL L315;
 BOOL L3161_;
 FSTR L317;
 dSTR L318;
 BOOL L319;
 BOOL L3201_;
 FSTR L321;
 dSTR L322;
 extern STR name13;
 BOOL L323;
 BOOL L3241_;
 FSTR L325;
 dSTR L326;
 extern STR name32;
 FSTR L327;
 dSTR L328;
 dSTR L329;
 OB L330;
 BOOL L331;
 BOOL L3321_;
 FSTR L333;
 dSTR L334;
 BOOL L335;
 BOOL L3361_;
 FSTR L337;
 dSTR L338;
 extern STR name38;
 BOOL L339;
 BOOL L3401_;
 FSTR L342;
 dSTR L343;
 BOOL L344;
 BOOL L3451_;
 FSTR L346;
 dSTR L347;
 extern STR name13;
 BOOL L348;
 BOOL L3491_;
 FSTR L350;
 dSTR L351;
 extern STR name32;
 FSTR L352;
 dSTR L353;
 L12 = SIG_is418491101(s);
 L131_=!(L12); 
 if (L131_) {
  L15 = ATTR(abs1,tp);
  if ((*dTP_is242312711[TAG(L15)])(L15, ((OB) ATTR(s,tp)))) {
   L16 = ATTR(abs1,ret);
   L14 = (*dTP_is242312711[TAG(L16)])(L16, ((OB) ATTR(s,ret)));
  } else {
   L14 = FALSE;
  }
  w = L14;
  {
   BOOL f_L171_ = TRUE;
   BOOL f_L181_ = TRUE;
   L11 = ATTRs(s,args,((ARRAYARG) NULL));
   L21 = ATTRs(abs1,args,((ARRAYARG) NULL));
   L171_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
   i_L171_=0;
   L181_br=L21==NULL?0:ASIZE((ARRAYARG)L21); 
   i_L181_=0;
   while (1) {
    if (w) {
    }
    else {
     goto after_loop;
    }
    if(i_L171_>=L171_br)  goto after_loop; 
    aL171_=ARR((ARRAYARG)L11,i_L171_); i_L171_++;
    is_eq_self = aL171_;
    if(i_L181_>=L181_br)  goto after_loop; 
    aL181_=ARR((ARRAYARG)L21,i_L181_); i_L181_++;
    is_eq_a = aL181_;
    L23 = ATTR(is_eq_self,tp);
    if ((*dTP_is242312711[TAG(L23)])(L23, ((OB) ATTR(is_eq_a,tp)))) {
     L24 = ATTR(is_eq_self,mode1);
     L22 = (*dMODE_814247358[TAG(L24)])(L24, ATTR(is_eq_a,mode1));
    } else {
     L22 = FALSE;
    }
    ret_val1 = L22;
    L25 = ret_val1;
    L261_=!(L25); 
    if (L261_) {
     w = FALSE;
    }
   }
  }
  after_loop: ;
  if (w) {
   mang_self = self;
   mang_ob = ((OB) s);
   ret_val2 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
   ret_val = ret_val2;
   return ret_val;
  }
 }
 res = FMAPSI1519487311(ATTR(self,emitted_dispatch), s);
 L27 = (res==((STR) NULL));
 L281_=!(L27); 
 if (L281_) {
  ret_val = res;
  return ret_val;
 }
 SATTR(self,code_c,ATTR(self,unbox_c));
 uses_unbox_self = ATTR(self,unbox_c);
 uses_unbox_s = s;
 uses_unbox_abs = abs1;
 create_self = TUPSIGSIG_zero;
 create_at1 = uses_unbox_s;
 create_at2 = uses_unbox_abs;
 L29 = create_self;
 L29.t1 = create_at1;
 L30 = L29;
 L30.t2 = create_at2;
 ret_val3 = L30;
 SATTR(uses_unbox_self,unboxes,FSETTU2063384524(ATTR(uses_unbox_self,unboxes), ret_val3));
 CODE_F1291349628(ATTR(self,unbox_c), abs1);
 L33 = SIG_is418491101(s);
 L341_=!(L33); 
 if (L341_) {
  L32 = TRUE;
 } else {
  is_iter_self = s;
  is_iter_self1 = ATTR(is_iter_self,name1);
  L36 = (is_iter_self1.str==((STR) NULL));
  L371_=!(L36); 
  if (L371_) {
   L38 = is_iter_self1.str;
   L39 = STR_sizerINT(is_iter_self1.str);
   L401_=INTMINUS(L39,1); 
   L42 = L401_;
   L431_=ARR((STR)L38,L42); 
   L44 = L431_;
   L451_=L44=='!'; 
   L35 = L451_;
  } else {
   L35 = FALSE;
  }
  ret_val5 = L35;
  ret_val4 = ret_val5;
  L32 = ret_val4;
 }
 if (L32) {
  CODE_F1291349628(ATTR(self,dispatch_c), s);
  CODE_F1291349628(ATTR(self,unbox_c), s);
 }
 uses_unbox_self1 = ATTR(self,dispatch_c);
 uses_unbox_s1 = s;
 uses_unbox_abs1 = abs1;
 create_self1 = TUPSIGSIG_zero;
 create_at11 = uses_unbox_s1;
 create_at21 = uses_unbox_abs1;
 L46 = create_self1;
 L46.t1 = create_at11;
 L47 = L46;
 L47.t2 = create_at21;
 ret_val6 = L47;
 SATTR(uses_unbox_self1,unboxes,FSETTU2063384524(ATTR(uses_unbox_self1,unboxes), ret_val6));
 mang_self1 = self;
 mang_ob1 = ((OB) s);
 ret_val7 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
 plus_self = ret_val7;
 plus_sarg = ((STR) &S_unbox);
 ret_val8 = STR_ap2004550586(plus_self, plus_sarg);
 res = ret_val8;
 plus_self1 = ATTR(self,unbox_c);
 L49=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L49==NULL) FATAL("Unable to allocate more memory");
 memset(L49,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L49)->header.tag=CHAR_tag;
 L48 = (dSTR)((CHAR_boxed) L49);
 ((CHAR_boxed) L48)->immutable_part = '\n';
 plus_s = L48;
 L50 = ATTR(plus_self1,ntext);
 L52 = plus_s;
 SATTR(plus_self1,ntext,FSTR_p1752847026(L50, (*dSTR_strrSTR[TAG(L52)])(L52)));
 comment_self = self;
 comment_f = ATTR(self,unbox_c);
 plus_self2 = ((STR) &Wrappertounbox);
 plus_sarg1 = ATTR(s,str);
 ret_val9 = STR_ap2004550586(plus_self2, plus_sarg1);
 comment_com = ret_val9;
 if (ATTR(comment_self,pretty)) {
  plus_self3 = comment_f;
  plus_s1 = ((dSTR) ((STR) &name78));
  L53 = (plus_s1==((dSTR) NULL));
  L541_=!(L53); 
  if (L541_) {
   L55 = ATTR(plus_self3,ntext);
   L56 = plus_s1;
   SATTR(plus_self3,ntext,FSTR_p1752847026(L55, (*dSTR_strrSTR[TAG(L56)])(L56)));
  }
  ret_val10 = plus_self3;
  plus_self4 = ret_val10;
  plus_s2 = ((dSTR) comment_com);
  L57 = (plus_s2==((dSTR) NULL));
  L581_=!(L57); 
  if (L581_) {
   L59 = ATTR(plus_self4,ntext);
   L60 = plus_s2;
   SATTR(plus_self4,ntext,FSTR_p1752847026(L59, (*dSTR_strrSTR[TAG(L60)])(L60)));
  }
  ret_val11 = plus_self4;
  plus_self5 = ret_val11;
  plus_s3 = ((dSTR) ((STR) &name79));
  L62 = ATTR(plus_self5,ntext);
  L63 = plus_s3;
  SATTR(plus_self5,ntext,FSTR_p1752847026(L62, (*dSTR_strrSTR[TAG(L63)])(L63)));
 }
 plus_self6 = ATTR(self,unbox_c);
 L65=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L65==NULL) FATAL("Unable to allocate more memory");
 memset(L65,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L65)->header.tag=CHAR_tag;
 L64 = (dSTR)((CHAR_boxed) L65);
 ((CHAR_boxed) L64)->immutable_part = '\n';
 plus_s4 = L64;
 L66 = ATTR(plus_self6,ntext);
 L67 = plus_s4;
 SATTR(plus_self6,ntext,FSTR_p1752847026(L66, (*dSTR_strrSTR[TAG(L67)])(L67)));
 if ((ATTR(s,ret)==((dTP) NULL))) {
  plus_self7 = ATTR(self,unbox_c);
  plus_s5 = ((dSTR) ((STR) &void6));
  L68 = ATTR(plus_self7,ntext);
  L69 = plus_s5;
  SATTR(plus_self7,ntext,FSTR_p1752847026(L68, (*dSTR_strrSTR[TAG(L69)])(L69)));
 }
 else {
  plus_self8 = ATTR(self,unbox_c);
  mang_self2 = self;
  mang_ob2 = ((OB) ATTR(abs1,ret));
  ret_val12 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, ((OB) NULL));
  plus_s6 = ((dSTR) ret_val12);
  L70 = (plus_s6==((dSTR) NULL));
  L721_=!(L70); 
  if (L721_) {
   L73 = ATTR(plus_self8,ntext);
   L74 = plus_s6;
   SATTR(plus_self8,ntext,FSTR_p1752847026(L73, (*dSTR_strrSTR[TAG(L74)])(L74)));
  }
  ret_val13 = plus_self8;
  plus_self9 = ret_val13;
  L76=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
  if (L76==NULL) FATAL("Unable to allocate more memory");
  memset(L76,0,sizeof(struct CHAR_boxed_struct));
  ((OB)L76)->header.tag=CHAR_tag;
  L75 = (dSTR)((CHAR_boxed) L76);
  ((CHAR_boxed) L75)->immutable_part = ' ';
  plus_s7 = L75;
  L77 = ATTR(plus_self9,ntext);
  L78 = plus_s7;
  SATTR(plus_self9,ntext,FSTR_p1752847026(L77, (*dSTR_strrSTR[TAG(L78)])(L78)));
 }
 pog = ((STR) &name1);
 if (ATTR(self,func_tables)) {
  pog = ((STR) &struct1621582358);
 }
 is_iter_self2 = s;
 is_iter_self3 = ATTR(is_iter_self2,name1);
 L80 = (is_iter_self3.str==((STR) NULL));
 L821_=!(L80); 
 if (L821_) {
  L83 = is_iter_self3.str;
  L84 = STR_sizerINT(is_iter_self3.str);
  L851_=INTMINUS(L84,1); 
  L86 = L851_;
  L871_=ARR((STR)L83,L86); 
  L88 = L871_;
  L891_=L88=='!'; 
  L79 = L891_;
 } else {
  L79 = FALSE;
 }
 ret_val15 = L79;
 ret_val14 = ret_val15;
 if (ret_val14) {
  plus_self10 = ATTR(self,unbox_c);
  plus_s8 = ((dSTR) res);
  L90 = (plus_s8==((dSTR) NULL));
  L921_=!(L90); 
  if (L921_) {
   L93 = ATTR(plus_self10,ntext);
   L94 = plus_s8;
   SATTR(plus_self10,ntext,FSTR_p1752847026(L93, (*dSTR_strrSTR[TAG(L94)])(L94)));
  }
  ret_val16 = plus_self10;
  plus_self11 = ret_val16;
  L96=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
  if (L96==NULL) FATAL("Unable to allocate more memory");
  memset(L96,0,sizeof(struct CHAR_boxed_struct));
  ((OB)L96)->header.tag=CHAR_tag;
  L95 = (dSTR)((CHAR_boxed) L96);
  ((CHAR_boxed) L95)->immutable_part = '(';
  plus_s9 = L95;
  L97 = (plus_s9==((dSTR) NULL));
  L981_=!(L97); 
  if (L981_) {
   L99 = ATTR(plus_self11,ntext);
   L100 = plus_s9;
   SATTR(plus_self11,ntext,FSTR_p1752847026(L99, (*dSTR_strrSTR[TAG(L100)])(L100)));
  }
  ret_val17 = plus_self11;
  plus_self12 = ret_val17;
  mang_self3 = self;
  mang_ob3 = ((OB) abs1);
  ret_val18 = MANGLE119219986(ATTR(mang_self3,mangler), mang_ob3, ((OB) NULL));
  plus_s10 = ((dSTR) ret_val18);
  L102 = (plus_s10==((dSTR) NULL));
  L1031_=!(L102); 
  if (L1031_) {
   L104 = ATTR(plus_self12,ntext);
   L105 = plus_s10;
   SATTR(plus_self12,ntext,FSTR_p1752847026(L104, (*dSTR_strrSTR[TAG(L105)])(L105)));
  }
  ret_val19 = plus_self12;
  plus_self13 = ret_val19;
  plus_s11 = ((dSTR) ((STR) &S_frameframe));
  L106 = (plus_s11==((dSTR) NULL));
  L1071_=!(L106); 
  if (L1071_) {
   L108 = ATTR(plus_self13,ntext);
   L109 = plus_s11;
   SATTR(plus_self13,ntext,FSTR_p1752847026(L108, (*dSTR_strrSTR[TAG(L109)])(L109)));
  }
  ret_val20 = plus_self13;
  plus_self14 = ret_val20;
  plus_s12 = ((dSTR) pog);
  L110 = (plus_s12==((dSTR) NULL));
  L1111_=!(L110); 
  if (L1111_) {
   L112 = ATTR(plus_self14,ntext);
   L113 = plus_s12;
   SATTR(plus_self14,ntext,FSTR_p1752847026(L112, (*dSTR_strrSTR[TAG(L113)])(L113)));
  }
  ret_val21 = plus_self14;
  plus_self15 = ret_val21;
  plus_s13 = ((dSTR) ((STR) &name164));
  L114 = ATTR(plus_self15,ntext);
  L115 = plus_s13;
  SATTR(plus_self15,ntext,FSTR_p1752847026(L114, (*dSTR_strrSTR[TAG(L115)])(L115)));
 }
 else {
  L117 = ATTR(abs1,tp);
  if ((*dTP_is1811059018[TAG(L117)])(L117)) {
   L119 = ATTR(s,tp);
   if ((*dTP_is1334578382[TAG(L119)])(L119)) {
    L118 = TRUE;
   } else {
    L120 = ATTR(s,tp);
    L118 = (*dTP_is242312711[TAG(L120)])(L120, ((OB) TP_BUI389447236));
   }
   L116 = L118;
  } else {
   L116 = FALSE;
  }
  if (L116) {
   plus_self16 = ATTR(self,unbox_c);
   plus_s14 = ((dSTR) res);
   L121 = (plus_s14==((dSTR) NULL));
   L1221_=!(L121); 
   if (L1221_) {
    L123 = ATTR(plus_self16,ntext);
    L124 = plus_s14;
    SATTR(plus_self16,ntext,FSTR_p1752847026(L123, (*dSTR_strrSTR[TAG(L124)])(L124)));
   }
   ret_val22 = plus_self16;
   plus_self17 = ret_val22;
   plus_s15 = ((dSTR) ((STR) &name24));
   L125 = (plus_s15==((dSTR) NULL));
   L1261_=!(L125); 
   if (L1261_) {
    L127 = ATTR(plus_self17,ntext);
    L128 = plus_s15;
    SATTR(plus_self17,ntext,FSTR_p1752847026(L127, (*dSTR_strrSTR[TAG(L128)])(L128)));
   }
   ret_val23 = plus_self17;
   plus_self18 = ret_val23;
   mang_self4 = self;
   mang_ob4 = ((OB) ATTR(s,tp));
   ret_val24 = MANGLE119219986(ATTR(mang_self4,mangler), mang_ob4, ((OB) NULL));
   plus_s16 = ((dSTR) ret_val24);
   L129 = (plus_s16==((dSTR) NULL));
   L1301_=!(L129); 
   if (L1301_) {
    L132 = ATTR(plus_self18,ntext);
    L133 = plus_s16;
    SATTR(plus_self18,ntext,FSTR_p1752847026(L132, (*dSTR_strrSTR[TAG(L133)])(L133)));
   }
   ret_val25 = plus_self18;
   plus_self19 = ret_val25;
   plus_s17 = ((dSTR) ((STR) &S_boxedself));
   L134 = ATTR(plus_self19,ntext);
   L135 = plus_s17;
   SATTR(plus_self19,ntext,FSTR_p1752847026(L134, (*dSTR_strrSTR[TAG(L135)])(L135)));
  }
  else {
   plus_self20 = ATTR(self,unbox_c);
   plus_s18 = ((dSTR) res);
   L136 = (plus_s18==((dSTR) NULL));
   L1371_=!(L136); 
   if (L1371_) {
    L138 = ATTR(plus_self20,ntext);
    L139 = plus_s18;
    SATTR(plus_self20,ntext,FSTR_p1752847026(L138, (*dSTR_strrSTR[TAG(L139)])(L139)));
   }
   ret_val26 = plus_self20;
   plus_self21 = ret_val26;
   plus_s19 = ((dSTR) ((STR) &name24));
   L140 = (plus_s19==((dSTR) NULL));
   L1411_=!(L140); 
   if (L1411_) {
    L142 = ATTR(plus_self21,ntext);
    L143 = plus_s19;
    SATTR(plus_self21,ntext,FSTR_p1752847026(L142, (*dSTR_strrSTR[TAG(L143)])(L143)));
   }
   ret_val27 = plus_self21;
   plus_self22 = ret_val27;
   mang_self5 = self;
   mang_ob5 = ((OB) ATTR(s,tp));
   ret_val28 = MANGLE119219986(ATTR(mang_self5,mangler), mang_ob5, ((OB) NULL));
   plus_s20 = ((dSTR) ret_val28);
   L144 = (plus_s20==((dSTR) NULL));
   L1451_=!(L144); 
   if (L1451_) {
    L146 = ATTR(plus_self22,ntext);
    L147 = plus_s20;
    SATTR(plus_self22,ntext,FSTR_p1752847026(L146, (*dSTR_strrSTR[TAG(L147)])(L147)));
   }
   ret_val29 = plus_self22;
   plus_self23 = ret_val29;
   plus_s21 = ((dSTR) ((STR) &self8));
   L148 = ATTR(plus_self23,ntext);
   L149 = plus_s21;
   SATTR(plus_self23,ntext,FSTR_p1752847026(L148, (*dSTR_strrSTR[TAG(L149)])(L149)));
  }
  {
   BOOL f_L1501_ = TRUE;
   BOOL f_L1511_ = TRUE;
   BOOL f_L1521_ = TRUE;
   /* loop index variable */
   L101 = 0;
   L31 = ATTR(s,args);
   L41 = ATTRs(abs1,args,((ARRAYARG) NULL));
   L51 = 1;
   L1501_br=L31==NULL?0:ASIZE((ARRAYARG)L31); 
   L1511_br=L41==NULL?0:ASIZE((ARRAYARG)L41); 
   L1521_=L51-1; 
   while (1) {
    if(L101>=L1501_br)  goto after_loop1; 
    aL1501_=ARR((ARRAYARG)L31,L101); 
    a = aL1501_;
    set = ATTR(a,tp);
    if(L101>=L1511_br)  goto after_loop1; 
    aL1511_=ARR((ARRAYARG)L41,L101); 
    absa = aL1511_;
    abset = ATTR(absa,tp);
    L1521_++; 
    idx = L1521_;
    L157 = abset;
    if ((*dTP_is1811059018[TAG(L157)])(L157)) {
     L159 = set;
     if ((*dTP_is1334578382[TAG(L159)])(L159)) {
      L158 = TRUE;
     } else {
      L160 = set;
      L158 = (*dTP_is242312711[TAG(L160)])(L160, ((OB) TP_BUI389447236));
     }
     L156 = L158;
    } else {
     L156 = FALSE;
    }
    if (L156) {
     plus_self25 = ATTR(self,unbox_c);
     boxed_arg_type_s = self;
     boxed_arg_type_s1 = a;
     mang_self6 = boxed_arg_type_s;
     mang_ob6 = ((OB) ATTR(boxed_arg_type_s1,tp));
     ret_val31 = MANGLE119219986(ATTR(mang_self6,mangler), mang_ob6, ((OB) NULL));
     plus_self24 = ret_val31;
     plus_sarg2 = ((STR) &S_boxed2);
     ret_val32 = STR_ap2004550586(plus_self24, plus_sarg2);
     res1 = ret_val32;
     L162 = ATTR(boxed_arg_type_s1,mode1);
     if ((*dMODE_814247358[TAG(L162)])(L162, ((dMODE) MODES_inout_mode))) {
      L161 = TRUE;
     } else {
      L163 = ATTR(boxed_arg_type_s1,mode1);
      L161 = (*dMODE_814247358[TAG(L163)])(L163, ((dMODE) MODES_out_mode));
     }
     if (L161) {
      res1 = STR_ap2004550586(res1, ((STR) &name58));
     }
     ret_val30 = res1;
     str_self = idx;
     clear_self = buf1;
     L164 = (clear_self==((FSTR) NULL));
     L1651_=!(L164); 
     if (L1651_) {
      SATTR(clear_self,loc,0);
     }
     str_in_self = str_self;
     str_in_s = buf1;
     ret_val34 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
     buf1 = ret_val34;
     str_self1 = buf1;
     ret_val35 = STR_fr1097270334(((STR) NULL), str_self1);
     ret_val33 = ret_val35;
     plus_s22 = ((dSTR) STR_ap1417971546(((STR) &name29), ret_val30, ((STR) &arg4), ret_val33));
     L166 = ATTR(plus_self25,ntext);
     L167 = plus_s22;
     SATTR(plus_self25,ntext,FSTR_p1752847026(L166, (*dSTR_strrSTR[TAG(L167)])(L167)));
    }
    else {
     plus_self27 = ATTR(self,unbox_c);
     arg_type_str_sel = self;
     arg_type_str_e = absa;
     mang_self7 = arg_type_str_sel;
     mang_ob7 = ((OB) ATTR(arg_type_str_e,tp));
     ret_val37 = MANGLE119219986(ATTR(mang_self7,mangler), mang_ob7, ((OB) NULL));
     res2 = ret_val37;
     L169 = ATTR(arg_type_str_e,mode1);
     if ((*dMODE_814247358[TAG(L169)])(L169, ((dMODE) MODES_inout_mode))) {
      L168 = TRUE;
     } else {
      L170 = ATTR(arg_type_str_e,mode1);
      L168 = (*dMODE_814247358[TAG(L170)])(L170, ((dMODE) MODES_out_mode));
     }
     if (L168) {
      plus_self26 = res2;
      plus_sarg3 = ((STR) &name58);
      ret_val38 = STR_ap2004550586(plus_self26, plus_sarg3);
      res2 = ret_val38;
     }
     ret_val36 = res2;
     str_self2 = idx;
     clear_self1 = buf2;
     L172 = (clear_self1==((FSTR) NULL));
     L1731_=!(L172); 
     if (L1731_) {
      SATTR(clear_self1,loc,0);
     }
     str_in_self1 = str_self2;
     str_in_s1 = buf2;
     ret_val40 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
     buf2 = ret_val40;
     str_self3 = buf2;
     ret_val41 = STR_fr1097270334(((STR) NULL), str_self3);
     ret_val39 = ret_val41;
     plus_s23 = ((dSTR) STR_ap1417971546(((STR) &name29), ret_val36, ((STR) &arg4), ret_val39));
     L174 = ATTR(plus_self27,ntext);
     L175 = plus_s23;
     SATTR(plus_self27,ntext,FSTR_p1752847026(L174, (*dSTR_strrSTR[TAG(L175)])(L175)));
    }
    L1761_=INTPLUS(L101,1); 
    L101 = L1761_;
   }
  }
  after_loop1: ;
  plus_self28 = ATTR(self,unbox_c);
  plus_s24 = ((dSTR) pog);
  L177 = (plus_s24==((dSTR) NULL));
  L1781_=!(L177); 
  if (L1781_) {
   L179 = ATTR(plus_self28,ntext);
   L180 = plus_s24;
   SATTR(plus_self28,ntext,FSTR_p1752847026(L179, (*dSTR_strrSTR[TAG(L180)])(L180)));
  }
  ret_val42 = plus_self28;
  plus_self29 = ret_val42;
  plus_s25 = ((dSTR) ((STR) &name164));
  L182 = ATTR(plus_self29,ntext);
  L183 = plus_s25;
  SATTR(plus_self29,ntext,FSTR_p1752847026(L182, (*dSTR_strrSTR[TAG(L183)])(L183)));
 }
 create_self2 = ((FSTR) NULL);
 L185 = 16;
 L187=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L185)*sizeof(CHAR);
 L186=ZALLOC_LEAF_BIG(L187);
 if (L186==NULL) FATAL("Unable to allocate more memory");
 memset(L186,0,L187);
 ((OB)L186)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L186)->header.destroyed=0;
#endif

 L184 = ((FSTR) L186);
 L184->asize = L185;
 ret_val43 = L184;
 SATTR(self,routine_code,ret_val43);
 is_iter_self4 = s;
 is_iter_self5 = ATTR(is_iter_self4,name1);
 L189 = (is_iter_self5.str==((STR) NULL));
 L1901_=!(L189); 
 if (L1901_) {
  L191 = is_iter_self5.str;
  L192 = STR_sizerINT(is_iter_self5.str);
  L1931_=INTMINUS(L192,1); 
  L194 = L1931_;
  L1951_=ARR((STR)L191,L194); 
  L196 = L1951_;
  L1971_=L196=='!'; 
  L188 = L1971_;
 } else {
  L188 = FALSE;
 }
 ret_val45 = L188;
 ret_val44 = ret_val45;
 if (ret_val44) {
  mang_self8 = self;
  mang_ob8 = ((OB) s);
  ret_val46 = MANGLE119219986(ATTR(mang_self8,mangler), mang_ob8, ((OB) NULL));
  plus_self30 = ret_val46;
  plus_sarg4 = ((STR) &name75);
  ret_val47 = STR_ap2004550586(plus_self30, plus_sarg4);
  plus_self31 = ret_val47;
  mang_self9 = self;
  mang_ob9 = ((OB) s);
  ret_val48 = MANGLE119219986(ATTR(mang_self9,mangler), mang_ob9, ((OB) NULL));
  plus_sarg5 = ret_val48;
  ret_val49 = STR_ap2004550586(plus_self31, plus_sarg5);
  plus_self32 = ret_val49;
  plus_sarg6 = ((STR) &S_frameframe1);
  ret_val50 = STR_ap2004550586(plus_self32, plus_sarg6);
  call11 = ret_val50;
  mang_self10 = self;
  mang_ob10 = ((OB) s);
  ret_val51 = MANGLE119219986(ATTR(mang_self10,mangler), mang_ob10, ((OB) NULL));
  plus_self33 = ret_val51;
  plus_arg = '(';
  str_self4 = plus_arg;
  create_self3 = ((STR) NULL);
  create_c = str_self4;
  L199 = 1;
  L201=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L199)*sizeof(CHAR);
  L200=ZALLOC_LEAF_BIG(L201);
  if (L200==NULL) FATAL("Unable to allocate more memory");
  memset(L200,0,L201);
  ((OB)L200)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L200)->header.destroyed=0;
#endif

  L198 = ((STR) L200);
  L198->asize = L199;
  r = L198;
  SARR((STR)r,0,create_c); 
  ;
  ret_val54 = r;
  ret_val53 = ret_val54;
  s1 = ret_val53;
  ret_val52 = STR_ap1077157958(plus_self33, s1, TRUE);
  plus_self34 = ret_val52;
  mang_self11 = self;
  mang_ob11 = ((OB) s);
  ret_val55 = MANGLE119219986(ATTR(mang_self11,mangler), mang_ob11, ((OB) NULL));
  plus_sarg7 = ret_val55;
  ret_val56 = STR_ap2004550586(plus_self34, plus_sarg7);
  plus_self35 = ret_val56;
  plus_sarg8 = ((STR) &S_frame4);
  ret_val57 = STR_ap2004550586(plus_self35, plus_sarg8);
  decl = ret_val57;
  if (ATTR(self,func_tables)) {
   plus_self36 = call11;
   plus_sarg9 = ((STR) &pFF1);
   ret_val58 = STR_ap2004550586(plus_self36, plus_sarg9);
   call11 = ret_val58;
   plus_self37 = decl;
   plus_sarg10 = ((STR) &struct1392482001);
   ret_val59 = STR_ap2004550586(plus_self37, plus_sarg10);
   decl = ret_val59;
  }
  plus_self38 = call11;
  plus_arg1 = ')';
  str_self5 = plus_arg1;
  create_self4 = ((STR) NULL);
  create_c1 = str_self5;
  L204 = 1;
  L206=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L204)*sizeof(CHAR);
  L205=ZALLOC_LEAF_BIG(L206);
  if (L205==NULL) FATAL("Unable to allocate more memory");
  memset(L205,0,L206);
  ((OB)L205)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L205)->header.destroyed=0;
#endif

  L203 = ((STR) L205);
  L203->asize = L204;
  r1 = L203;
  SARR((STR)r1,0,create_c1); 
  ;
  ret_val62 = r1;
  ret_val61 = ret_val62;
  s2 = ret_val61;
  ret_val60 = STR_ap1077157958(plus_self38, s2, TRUE);
  call11 = ret_val60;
  plus_self39 = decl;
  plus_arg2 = ')';
  str_self6 = plus_arg2;
  create_self5 = ((STR) NULL);
  create_c2 = str_self6;
  L209 = 1;
  L211=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L209)*sizeof(CHAR);
  L210=ZALLOC_LEAF_BIG(L211);
  if (L210==NULL) FATAL("Unable to allocate more memory");
  memset(L210,0,L211);
  ((OB)L210)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L210)->header.destroyed=0;
#endif

  L208 = ((STR) L210);
  L208->asize = L209;
  r2 = L208;
  SARR((STR)r2,0,create_c2); 
  ;
  ret_val65 = r2;
  ret_val64 = ret_val65;
  s3 = ret_val64;
  ret_val63 = STR_ap1077157958(plus_self39, s3, TRUE);
  decl = ret_val63;
  L213 = (ATTR(s,ret)==((dTP) NULL));
  L2141_=!(L213); 
  if (L2141_) {
   plus_self40 = ((STR) &return4);
   cast_self = self;
   cast_dest_tp = ATTR(abs1,ret);
   cast_src_tp = ATTR(s,ret);
   cast_expr = call11;
   cast_only_boxing = FALSE;
   ret_val66 = CGEN_d1915593494(cast_self, cast_dest_tp, cast_src_tp, cast_expr, cast_only_boxing, ((dMODE) NULL));
   plus_sarg11 = ret_val66;
   ret_val67 = STR_ap2004550586(plus_self40, plus_sarg11);
   call11 = ret_val67;
   mang_self12 = self;
   mang_ob12 = ((OB) ATTR(s,ret));
   ret_val68 = MANGLE119219986(ATTR(mang_self12,mangler), mang_ob12, ((OB) NULL));
   plus_self41 = ret_val68;
   plus_arg3 = ' ';
   str_self7 = plus_arg3;
   create_self6 = ((STR) NULL);
   create_c3 = str_self7;
   L216 = 1;
   L218=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L216)*sizeof(CHAR);
   L217=ZALLOC_LEAF_BIG(L218);
   if (L217==NULL) FATAL("Unable to allocate more memory");
   memset(L217,0,L218);
   ((OB)L217)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L217)->header.destroyed=0;
#endif

   L215 = ((STR) L217);
   L215->asize = L216;
   r3 = L215;
   SARR((STR)r3,0,create_c3); 
   ;
   ret_val71 = r3;
   ret_val70 = ret_val71;
   s4 = ret_val70;
   ret_val69 = STR_ap1077157958(plus_self41, s4, TRUE);
   plus_self42 = ret_val69;
   plus_sarg12 = decl;
   ret_val72 = STR_ap2004550586(plus_self42, plus_sarg12);
   decl = ret_val72;
  }
  else {
   plus_self43 = ((STR) &void6);
   plus_sarg13 = decl;
   ret_val73 = STR_ap2004550586(plus_self43, plus_sarg13);
   decl = ret_val73;
  }
 }
 else {
  arg_str = ((STR) &arg21);
  if (SIG_is418491101(s)) {
   create_self7 = ((ARRAYSTR) NULL);
   num_args_self = s;
   L220 = ATTR(num_args_self,args);
   L2211_=(L220)==NULL?0:ASIZE((ARRAYARG)L220); 
   ret_val74 = L2211_;
   L222 = ret_val74;
   L2231_=INTPLUS(L222,1); 
   create_n = L2231_;
   L225 = create_n;
   L227=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L225)*sizeof(STR);
   L226=ZALLOC_BIG(L227);
   if (L226==NULL) FATAL("Unable to allocate more memory");
   ((OB)L226)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

     ((OB)L226)->header.destroyed=0;
#endif

   L224 = ((ARRAYSTR) L226);
   L224->asize = L225;
   ret_val75 = L224;
   arg_list = ret_val75;
   cast_self1 = self;
   cast_dest_tp1 = ATTR(s,tp);
   cast_src_tp1 = ATTR(abs1,tp);
   cast_expr1 = ((STR) &self1);
   cast_only_boxing1 = FALSE;
   ret_val76 = CGEN_d1915593494(cast_self1, cast_dest_tp1, cast_src_tp1, cast_expr1, cast_only_boxing1, ((dMODE) NULL));
   L228 = ret_val76;
   SARR((ARRAYSTR)arg_list,0,(STR)L228); 
   ;
   {
    BOOL f_L2301_ = TRUE;
    L61 = 1;
    num_args_self1 = s;
    L231 = ATTR(num_args_self1,args);
    L2321_=(L231)==NULL?0:ASIZE((ARRAYARG)L231); 
    ret_val77 = L2321_;
    L71 = ret_val77;
    L2301_=L61-1;L2301_m=L71; 
    while (1) {
     if(L2301_++>=L2301_m)  goto after_loop2; 
     i = L2301_;
     cast_self2 = self;
     L234 = ATTR(s,args);
     L2351_=INTMINUS(i,1); 
     L236 = L2351_;
     L2371_=(ARG)ARR((ARRAYARG)L234,L236); 
     L238=L2371_;
     cast_dest_tp2 = ATTR(L238,tp);
     L239 = ATTR(abs1,args);
     L2401_=INTMINUS(i,1); 
     L241 = L2401_;
     L2421_=(ARG)ARR((ARRAYARG)L239,L241); 
     L243=L2421_;
     cast_src_tp2 = ATTR(L243,tp);
     plus_self44 = arg_str;
     plus_arg4 = i;
     str_self8 = plus_arg4;
     clear_self2 = buf3;
     L244 = (clear_self2==((FSTR) NULL));
     L2451_=!(L244); 
     if (L2451_) {
      SATTR(clear_self2,loc,0);
     }
     str_in_self2 = str_self8;
     str_in_s2 = buf3;
     ret_val80 = INT_st367594495(str_in_self2, str_in_s2, 0, 10, ' ');
     buf3 = ret_val80;
     str_self9 = buf3;
     ret_val81 = STR_fr1097270334(((STR) NULL), str_self9);
     ret_val79 = ret_val81;
     s5 = ret_val79;
     ret_val78 = STR_ap1077157958(plus_self44, s5, TRUE);
     cast_expr2 = ret_val78;
     cast_only_boxing2 = TRUE;
     ret_val82 = CGEN_d1915593494(cast_self2, cast_dest_tp2, cast_src_tp2, cast_expr2, cast_only_boxing2, ((dMODE) NULL));
     L246 = ret_val82;
     SARR((ARRAYSTR)arg_list,i,(STR)L246); 
     ;
    }
   }
   after_loop2: ;
   call11 = CGEN_p1740273317(self, s, ((dAM_CALL_EXPR) NULL), arg_list);
  }
  else {
   mang_self13 = self;
   mang_ob13 = ((OB) s);
   ret_val83 = MANGLE119219986(ATTR(mang_self13,mangler), mang_ob13, ((OB) NULL));
   plus_self45 = ret_val83;
   plus_arg5 = '(';
   str_self10 = plus_arg5;
   create_self8 = ((STR) NULL);
   create_c4 = str_self10;
   L249 = 1;
   L251=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L249)*sizeof(CHAR);
   L250=ZALLOC_LEAF_BIG(L251);
   if (L250==NULL) FATAL("Unable to allocate more memory");
   memset(L250,0,L251);
   ((OB)L250)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L250)->header.destroyed=0;
#endif

   L248 = ((STR) L250);
   L248->asize = L249;
   r4 = L248;
   SARR((STR)r4,0,create_c4); 
   ;
   ret_val86 = r4;
   ret_val85 = ret_val86;
   s6 = ret_val85;
   ret_val84 = STR_ap1077157958(plus_self45, s6, TRUE);
   plus_self46 = ret_val84;
   cast_self3 = self;
   cast_dest_tp3 = ATTR(s,tp);
   cast_src_tp3 = ATTR(abs1,tp);
   cast_expr3 = ((STR) &self1);
   cast_only_boxing3 = FALSE;
   ret_val87 = CGEN_d1915593494(cast_self3, cast_dest_tp3, cast_src_tp3, cast_expr3, cast_only_boxing3, ((dMODE) NULL));
   plus_sarg14 = ret_val87;
   ret_val88 = STR_ap2004550586(plus_self46, plus_sarg14);
   call11 = ret_val88;
   mang_self14 = self;
   mang_ob14 = ((OB) s);
   ret_val89 = MANGLE119219986(ATTR(mang_self14,mangler), mang_ob14, ((OB) NULL));
   plus_self47 = ret_val89;
   plus_arg6 = '(';
   str_self11 = plus_arg6;
   create_self9 = ((STR) NULL);
   create_c5 = str_self11;
   L254 = 1;
   L256=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L254)*sizeof(CHAR);
   L255=ZALLOC_LEAF_BIG(L256);
   if (L255==NULL) FATAL("Unable to allocate more memory");
   memset(L255,0,L256);
   ((OB)L255)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L255)->header.destroyed=0;
#endif

   L253 = ((STR) L255);
   L253->asize = L254;
   r5 = L253;
   SARR((STR)r5,0,create_c5); 
   ;
   ret_val92 = r5;
   ret_val91 = ret_val92;
   s7 = ret_val91;
   ret_val90 = STR_ap1077157958(plus_self47, s7, TRUE);
   plus_self48 = ret_val90;
   mang_self15 = self;
   mang_ob15 = ((OB) ATTR(s,tp));
   ret_val93 = MANGLE119219986(ATTR(mang_self15,mangler), mang_ob15, ((OB) NULL));
   plus_sarg15 = ret_val93;
   ret_val94 = STR_ap2004550586(plus_self48, plus_sarg15);
   decl = ret_val94;
   {
    BOOL f_L2581_ = TRUE;
    L81 = 1;
    num_args_self2 = s;
    L259 = ATTR(num_args_self2,args);
    L2601_=(L259)==NULL?0:ASIZE((ARRAYARG)L259); 
    ret_val95 = L2601_;
    L91 = ret_val95;
    L2581_=L81-1;L2581_m=L91; 
    while (1) {
     if(L2581_++>=L2581_m)  goto after_loop3; 
     i1 = L2581_;
     plus_self49 = call11;
     plus_sarg16 = ((STR) &name29);
     ret_val96 = STR_ap2004550586(plus_self49, plus_sarg16);
     plus_self51 = ret_val96;
     cast_arg_self = self;
     L263 = ATTR(s,args);
     L2641_=INTMINUS(i1,1); 
     L265 = L2641_;
     L2661_=(ARG)ARR((ARRAYARG)L263,L265); 
     L267=L2661_;
     cast_arg_dest_tp = ATTR(L267,tp);
     L268 = ATTR(abs1,args);
     L2691_=INTMINUS(i1,1); 
     L270 = L2691_;
     L2711_=(ARG)ARR((ARRAYARG)L268,L270); 
     L272=L2711_;
     cast_arg_src_tp = ATTR(L272,tp);
     plus_self50 = arg_str;
     plus_arg7 = i1;
     str_self12 = plus_arg7;
     clear_self3 = buf4;
     L273 = (clear_self3==((FSTR) NULL));
     L2741_=!(L273); 
     if (L2741_) {
      SATTR(clear_self3,loc,0);
     }
     str_in_self3 = str_self12;
     str_in_s3 = buf4;
     ret_val99 = INT_st367594495(str_in_self3, str_in_s3, 0, 10, ' ');
     buf4 = ret_val99;
     str_self13 = buf4;
     ret_val100 = STR_fr1097270334(((STR) NULL), str_self13);
     ret_val98 = ret_val100;
     s8 = ret_val98;
     ret_val97 = STR_ap1077157958(plus_self50, s8, TRUE);
     cast_arg_expr = ret_val97;
     cast_arg_only_bo = FALSE;
     L275 = ATTR(s,args);
     L2761_=INTMINUS(i1,1); 
     L277 = L2761_;
     L2781_=(ARG)ARR((ARRAYARG)L275,L277); 
     L279=L2781_;
     cast_arg_mode = ATTR(L279,mode1);
     ret_val101 = CGEN_d1915593494(cast_arg_self, cast_arg_dest_tp, cast_arg_src_tp, cast_arg_expr, cast_arg_only_bo, cast_arg_mode);
     plus_sarg17 = ret_val101;
     ret_val102 = STR_ap2004550586(plus_self51, plus_sarg17);
     call11 = ret_val102;
     plus_self52 = decl;
     plus_sarg18 = ((STR) &name29);
     ret_val103 = STR_ap2004550586(plus_self52, plus_sarg18);
     plus_self54 = ret_val103;
     arg_type_str_sel1 = self;
     L280 = ATTR(s,args);
     L2821_=INTMINUS(i1,1); 
     L283 = L2821_;
     L2841_=(ARG)ARR((ARRAYARG)L280,L283); 
     arg_type_str_e1 = L2841_;
     mang_self16 = arg_type_str_sel1;
     mang_ob16 = ((OB) ATTR(arg_type_str_e1,tp));
     ret_val105 = MANGLE119219986(ATTR(mang_self16,mangler), mang_ob16, ((OB) NULL));
     res3 = ret_val105;
     L286 = ATTR(arg_type_str_e1,mode1);
     if ((*dMODE_814247358[TAG(L286)])(L286, ((dMODE) MODES_inout_mode))) {
      L285 = TRUE;
     } else {
      L287 = ATTR(arg_type_str_e1,mode1);
      L285 = (*dMODE_814247358[TAG(L287)])(L287, ((dMODE) MODES_out_mode));
     }
     if (L285) {
      plus_self53 = res3;
      plus_sarg19 = ((STR) &name58);
      ret_val106 = STR_ap2004550586(plus_self53, plus_sarg19);
      res3 = ret_val106;
     }
     ret_val104 = res3;
     plus_sarg20 = ret_val104;
     ret_val107 = STR_ap2004550586(plus_self54, plus_sarg20);
     decl = ret_val107;
    }
   }
   after_loop3: ;
   if (ATTR(self,func_tables)) {
    plus_self55 = call11;
    plus_sarg21 = ((STR) &pFF1);
    ret_val108 = STR_ap2004550586(plus_self55, plus_sarg21);
    call11 = ret_val108;
    plus_self56 = decl;
    plus_sarg22 = ((STR) &struct1392482001);
    ret_val109 = STR_ap2004550586(plus_self56, plus_sarg22);
    decl = ret_val109;
   }
   plus_self57 = call11;
   plus_arg8 = ')';
   str_self14 = plus_arg8;
   create_self10 = ((STR) NULL);
   create_c6 = str_self14;
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
   r6 = L288;
   SARR((STR)r6,0,create_c6); 
   ;
   ret_val112 = r6;
   ret_val1111 = ret_val112;
   s9 = ret_val1111;
   ret_val110 = STR_ap1077157958(plus_self57, s9, TRUE);
   call11 = ret_val110;
   plus_self58 = decl;
   plus_arg9 = ')';
   str_self15 = plus_arg9;
   create_self11 = ((STR) NULL);
   create_c7 = str_self15;
   L294 = 1;
   L296=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L294)*sizeof(CHAR);
   L295=ZALLOC_LEAF_BIG(L296);
   if (L295==NULL) FATAL("Unable to allocate more memory");
   memset(L295,0,L296);
   ((OB)L295)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L295)->header.destroyed=0;
#endif

   L293 = ((STR) L295);
   L293->asize = L294;
   r7 = L293;
   SARR((STR)r7,0,create_c7); 
   ;
   ret_val115 = r7;
   ret_val114 = ret_val115;
   s10 = ret_val114;
   ret_val113 = STR_ap1077157958(plus_self58, s10, TRUE);
   decl = ret_val113;
  }
  L298 = (ATTR(s,ret)==((dTP) NULL));
  L2991_=!(L298); 
  if (L2991_) {
   plus_self59 = ((STR) &return4);
   cast_self4 = self;
   cast_dest_tp4 = ATTR(abs1,ret);
   cast_src_tp4 = ATTR(s,ret);
   cast_expr4 = call11;
   cast_only_boxing4 = FALSE;
   ret_val116 = CGEN_d1915593494(cast_self4, cast_dest_tp4, cast_src_tp4, cast_expr4, cast_only_boxing4, ((dMODE) NULL));
   plus_sarg23 = ret_val116;
   ret_val117 = STR_ap2004550586(plus_self59, plus_sarg23);
   call11 = ret_val117;
   mang_self17 = self;
   mang_ob17 = ((OB) ATTR(s,ret));
   ret_val118 = MANGLE119219986(ATTR(mang_self17,mangler), mang_ob17, ((OB) NULL));
   plus_self60 = ret_val118;
   plus_arg10 = ' ';
   str_self16 = plus_arg10;
   create_self12 = ((STR) NULL);
   create_c8 = str_self16;
   L301 = 1;
   L303=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L301)*sizeof(CHAR);
   L302=ZALLOC_LEAF_BIG(L303);
   if (L302==NULL) FATAL("Unable to allocate more memory");
   memset(L302,0,L303);
   ((OB)L302)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L302)->header.destroyed=0;
#endif

   L300 = ((STR) L302);
   L300->asize = L301;
   r8 = L300;
   SARR((STR)r8,0,create_c8); 
   ;
   ret_val121 = r8;
   ret_val120 = ret_val121;
   s11 = ret_val120;
   ret_val119 = STR_ap1077157958(plus_self60, s11, TRUE);
   plus_self61 = ret_val119;
   plus_sarg24 = decl;
   ret_val122 = STR_ap2004550586(plus_self61, plus_sarg24);
   decl = ret_val122;
  }
  else {
   plus_self62 = ((STR) &void6);
   plus_sarg25 = decl;
   ret_val123 = STR_ap2004550586(plus_self62, plus_sarg25);
   decl = ret_val123;
   if ((call11==((STR) NULL))) {
    call11 = ((STR) &name1);
   }
  }
 }
 if (SIG_is418491101(s)) {
  plus_self63 = ATTR(self,unbox_c);
  L306=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
  if (L306==NULL) FATAL("Unable to allocate more memory");
  memset(L306,0,sizeof(struct CHAR_boxed_struct));
  ((OB)L306)->header.tag=CHAR_tag;
  L305 = (dSTR)((CHAR_boxed) L306);
  ((CHAR_boxed) L305)->immutable_part = ' ';
  plus_s26 = L305;
  L307 = (plus_s26==((dSTR) NULL));
  L3081_=!(L307); 
  if (L3081_) {
   L309 = ATTR(plus_self63,ntext);
   L310 = plus_s26;
   SATTR(plus_self63,ntext,FSTR_p1752847026(L309, (*dSTR_strrSTR[TAG(L310)])(L310)));
  }
  ret_val124 = plus_self63;
  plus_self64 = ret_val124;
  plus_s27 = ((dSTR) ATTR(self,routine_code));
  L311 = (plus_s27==((dSTR) NULL));
  L3121_=!(L311); 
  if (L3121_) {
   L313 = ATTR(plus_self64,ntext);
   L314 = plus_s27;
   SATTR(plus_self64,ntext,FSTR_p1752847026(L313, (*dSTR_strrSTR[TAG(L314)])(L314)));
  }
  ret_val125 = plus_self64;
  plus_self65 = ret_val125;
  plus_s28 = ((dSTR) ((STR) &name38));
  L315 = (plus_s28==((dSTR) NULL));
  L3161_=!(L315); 
  if (L3161_) {
   L317 = ATTR(plus_self65,ntext);
   L318 = plus_s28;
   SATTR(plus_self65,ntext,FSTR_p1752847026(L317, (*dSTR_strrSTR[TAG(L318)])(L318)));
  }
  ret_val126 = plus_self65;
  plus_self66 = ret_val126;
  plus_s29 = ((dSTR) call11);
  L319 = (plus_s29==((dSTR) NULL));
  L3201_=!(L319); 
  if (L3201_) {
   L321 = ATTR(plus_self66,ntext);
   L322 = plus_s29;
   SATTR(plus_self66,ntext,FSTR_p1752847026(L321, (*dSTR_strrSTR[TAG(L322)])(L322)));
  }
  ret_val127 = plus_self66;
  plus_self67 = ret_val127;
  plus_s30 = ((dSTR) ((STR) &name13));
  L323 = (plus_s30==((dSTR) NULL));
  L3241_=!(L323); 
  if (L3241_) {
   L325 = ATTR(plus_self67,ntext);
   L326 = plus_s30;
   SATTR(plus_self67,ntext,FSTR_p1752847026(L325, (*dSTR_strrSTR[TAG(L326)])(L326)));
  }
  ret_val128 = plus_self67;
  plus_self68 = ret_val128;
  plus_s31 = ((dSTR) ((STR) &name32));
  L327 = ATTR(plus_self68,ntext);
  L328 = plus_s31;
  SATTR(plus_self68,ntext,FSTR_p1752847026(L327, (*dSTR_strrSTR[TAG(L328)])(L328)));
 }
 else {
  plus_self69 = ATTR(self,unbox_c);
  L330=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
  if (L330==NULL) FATAL("Unable to allocate more memory");
  memset(L330,0,sizeof(struct CHAR_boxed_struct));
  ((OB)L330)->header.tag=CHAR_tag;
  L329 = (dSTR)((CHAR_boxed) L330);
  ((CHAR_boxed) L329)->immutable_part = ' ';
  plus_s32 = L329;
  L331 = (plus_s32==((dSTR) NULL));
  L3321_=!(L331); 
  if (L3321_) {
   L333 = ATTR(plus_self69,ntext);
   L334 = plus_s32;
   SATTR(plus_self69,ntext,FSTR_p1752847026(L333, (*dSTR_strrSTR[TAG(L334)])(L334)));
  }
  ret_val129 = plus_self69;
  plus_self70 = ret_val129;
  plus_s33 = ((dSTR) ATTR(self,routine_code));
  L335 = (plus_s33==((dSTR) NULL));
  L3361_=!(L335); 
  if (L3361_) {
   L337 = ATTR(plus_self70,ntext);
   L338 = plus_s33;
   SATTR(plus_self70,ntext,FSTR_p1752847026(L337, (*dSTR_strrSTR[TAG(L338)])(L338)));
  }
  ret_val130 = plus_self70;
  plus_self71 = ret_val130;
  plus_s34 = ((dSTR) ((STR) &name38));
  L339 = (plus_s34==((dSTR) NULL));
  L3401_=!(L339); 
  if (L3401_) {
   L342 = ATTR(plus_self71,ntext);
   L343 = plus_s34;
   SATTR(plus_self71,ntext,FSTR_p1752847026(L342, (*dSTR_strrSTR[TAG(L343)])(L343)));
  }
  ret_val131 = plus_self71;
  plus_self72 = ret_val131;
  plus_s35 = ((dSTR) call11);
  L344 = (plus_s35==((dSTR) NULL));
  L3451_=!(L344); 
  if (L3451_) {
   L346 = ATTR(plus_self72,ntext);
   L347 = plus_s35;
   SATTR(plus_self72,ntext,FSTR_p1752847026(L346, (*dSTR_strrSTR[TAG(L347)])(L347)));
  }
  ret_val132 = plus_self72;
  plus_self73 = ret_val132;
  plus_s36 = ((dSTR) ((STR) &name13));
  L348 = (plus_s36==((dSTR) NULL));
  L3491_=!(L348); 
  if (L3491_) {
   L350 = ATTR(plus_self73,ntext);
   L351 = plus_s36;
   SATTR(plus_self73,ntext,FSTR_p1752847026(L350, (*dSTR_strrSTR[TAG(L351)])(L351)));
  }
  ret_val133 = plus_self73;
  plus_self74 = ret_val133;
  plus_s37 = ((dSTR) ((STR) &name32));
  L352 = ATTR(plus_self74,ntext);
  L353 = plus_s37;
  SATTR(plus_self74,ntext,FSTR_p1752847026(L352, (*dSTR_strrSTR[TAG(L353)])(L353)));
 }
 SATTR(self,emitted_dispatch,FMAPSI1897406528(ATTR(self,emitted_dispatch), s, res));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_f1593827635(CGEN self, SIG sig1) {
 STR ret_val;
 STR res = ((STR) NULL);
 BOOL has_return = BOOL_zero;
 dTP L11;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val1;
 CGEN bind_fortran_nam;
 STR bind_fortran_nam1;
 STR ret_val2;
 STR res1 = ((STR) NULL);
 BIND_FORTRAN bind_name_self;
 STR bind_name_name;
 STR ret_val3;
 STR res2 = ((STR) NULL);
 BIND_FORTRAN append_underscor;
 STR append_underscor1;
 STR ret_val4;
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val6;
 dTP L2;
 dTP L3;
 dTP L4;
 dTP L5;
 extern STR name10;
 extern STR void6;
 extern STR S_;
 STR L6;
 extern STR name24;
 STR L7;
 extern STR name76;
 if ((ATTR(sig1,ret)==((dTP) NULL))) {
  has_return = FALSE;
 }
 else {
  L11 = ATTR(sig1,ret);
  L2 = L11;
  if ((*dTP_is242312711[TAG(L2)])(L2, ((OB) TP_BUI633637838))) {
   has_return = FALSE;
  }
  else {
   L3 = L11;
   if ((*dTP_is242312711[TAG(L3)])(L3, ((OB) TP_BUI1032249067))) {
    has_return = FALSE;
   }
   else {
    L4 = L11;
    if ((*dTP_is242312711[TAG(L4)])(L4, ((OB) TP_BUI1805763264))) {
     has_return = FALSE;
    }
    else {
     L5 = L11;
     if ((*dTP_is242312711[TAG(L5)])(L5, ((OB) TP_BUI2088674906))) {
      has_return = FALSE;
     }
     else {
      has_return = TRUE;
     }
    }
   }
  }
 }
 if (has_return) {
  mang_self = self;
  mang_ob = ((OB) ATTR(sig1,ret));
  ret_val1 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
  res = STR_ap1693864410(res, ret_val1, ((STR) &name10));
 }
 else {
  res = STR_ap2004550586(res, ((STR) &void6));
 }
 plus_self1 = res;
 bind_fortran_nam = self;
 bind_fortran_nam1 = ATTR(sig1,name1).str;
 res1 = STR_ap1693864410(ATTR(bind_fortran_nam,fortran_prepend_), bind_fortran_nam1, ATTR(bind_fortran_nam,fortran_append_u));
 if (ATTR(bind_fortran_nam,fortran_bind_fun)) {
  bind_name_self = ((BIND_FORTRAN) NULL);
  bind_name_name = res1;
  append_underscor = bind_name_self;
  append_underscor1 = bind_name_name;
  plus_self = append_underscor1;
  plus_sarg = ((STR) &S_);
  ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
  ret_val4 = ret_val5;
  res2 = ret_val4;
  ret_val3 = res2;
  res1 = ret_val3;
 }
 ret_val2 = res1;
 plus_sarg1 = ret_val2;
 ret_val6 = STR_ap2004550586(plus_self1, plus_sarg1);
 res = ret_val6;
 L6 = ATTR(self,func_name_suffix);
 L7 = CGEN_g1520354813(self, sig1);
 res = STR_ap44172742(res, L6, ((STR) &name24), L7, ((STR) &name76));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_g1520354813(CGEN self, SIG sig1) {
 STR ret_val;
 STR res = ((STR) NULL);
 INT num_character_ar = INT_zero;
 dTP L11;
 ARRAYARG L21;
 dTP tp;
 STR L31;
 INT L41 = INT_zero;
 INT L51 = INT_zero;
 INT i = INT_zero;
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
 INT L61 = INT_zero;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val7;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val8;
 BOOL L7;
 BOOL L81_;
 dTP L9;
 ARRAYARG L10;
 INT L121_;
 INT L13;
 BOOL L141_;
 extern STR name3;
 dTP L15;
 ARRAYARG L16;
 INT L171_;
 INT L18;
 BOOL L191_;
 extern STR name3;
 dTP L20;
 extern STR F_LENGTH2;
 ARRAYARG L22;
 INT L231_;
 INT L24;
 BOOL L251_;
 extern STR name3;
 dTP L26;
 extern STR F_LENGTH2;
 ARRAYARG L27;
 INT L281_;
 INT L29;
 BOOL L301_;
 extern STR name3;
 extern STR name3;
 INT L321_br;
 ARG aL321_;
 ARG L33;
 ARG L34;
 STR L35;
 BOOL L36;
 dTP L37;
 dTP L38;
 INT L391_;
 INT L401_;
 INT L421_,L421_m;
 INT L43;
 extern STR F_LENGTH2;
 L7 = (ATTR(sig1,ret)==((dTP) NULL));
 L81_=!(L7); 
 if (L81_) {
  L11 = ATTR(sig1,ret);
  L9 = L11;
  if ((*dTP_is242312711[TAG(L9)])(L9, ((OB) TP_BUI633637838))) {
   res = CGEN_f2084214268(self, ATTR(sig1,ret));
   L10 = ATTR(sig1,args);
   L121_=(L10)==NULL?0:ASIZE((ARRAYARG)L10); 
   L13 = L121_;
   L141_=(0)<(L13); 
   if (L141_) {
    plus_self = res;
    plus_sarg = ((STR) &name3);
    ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
    res = ret_val1;
   }
  }
  else {
   L15 = L11;
   if ((*dTP_is242312711[TAG(L15)])(L15, ((OB) TP_BUI1032249067))) {
    res = CGEN_f2084214268(self, ATTR(sig1,ret));
    L16 = ATTR(sig1,args);
    L171_=(L16)==NULL?0:ASIZE((ARRAYARG)L16); 
    L18 = L171_;
    L191_=(0)<(L18); 
    if (L191_) {
     plus_self1 = res;
     plus_sarg1 = ((STR) &name3);
     ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
     res = ret_val2;
    }
   }
   else {
    L20 = L11;
    if ((*dTP_is242312711[TAG(L20)])(L20, ((OB) TP_BUI1805763264))) {
     plus_self2 = CGEN_f2084214268(self, ATTR(sig1,ret));
     plus_sarg2 = ((STR) &F_LENGTH2);
     ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
     res = ret_val3;
     L22 = ATTR(sig1,args);
     L231_=(L22)==NULL?0:ASIZE((ARRAYARG)L22); 
     L24 = L231_;
     L251_=(0)<(L24); 
     if (L251_) {
      plus_self3 = res;
      plus_sarg3 = ((STR) &name3);
      ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
      res = ret_val4;
     }
    }
    else {
     L26 = L11;
     if ((*dTP_is242312711[TAG(L26)])(L26, ((OB) TP_BUI2088674906))) {
      plus_self4 = CGEN_f2084214268(self, ATTR(sig1,ret));
      plus_sarg4 = ((STR) &F_LENGTH2);
      ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
      res = ret_val5;
      L27 = ATTR(sig1,args);
      L281_=(L27)==NULL?0:ASIZE((ARRAYARG)L27); 
      L29 = L281_;
      L301_=(0)<(L29); 
      if (L301_) {
       plus_self5 = res;
       plus_sarg5 = ((STR) &name3);
       ret_val6 = STR_ap2004550586(plus_self5, plus_sarg5);
       res = ret_val6;
      }
     }
    }
   }
  }
 }
 {
  BOOL f_L321_ = TRUE;
  struct STR_se405450305_frame_struct other1_0;
STR_se405450305_frame noname1 = &other1_0;
  /* loop index variable */
  L61 = 0;
  L21 = ATTR(sig1,args);
  L31 = ((STR) &name3);
  L321_br=L21==NULL?0:ASIZE((ARRAYARG)L21); 
  noname1->self = L31;
  noname1->state = 0;
  while (1) {
   if(L61>=L321_br)  goto after_loop; 
   aL321_=ARR((ARRAYARG)L21,L61); 
   L34=aL321_;
   tp = ATTR(L34,tp);
   plus_self6 = res;
   noname1->arg1 = ((dSTR) CGEN_f2084214268(self, tp));
   L35 = STR_se405450305(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   plus_sarg6 = L35;
   ret_val7 = STR_ap2004550586(plus_self6, plus_sarg6);
   res = ret_val7;
   L37 = tp;
   if ((*dTP_is242312711[TAG(L37)])(L37, ((OB) TP_BUI1805763264))) {
    L36 = TRUE;
   } else {
    L38 = tp;
    L36 = (*dTP_is242312711[TAG(L38)])(L38, ((OB) TP_BUI2088674906));
   }
   if (L36) {
    L391_=INTPLUS(num_character_ar,1); 
    num_character_ar = L391_;
   }
   L401_=INTPLUS(L61,1); 
   L61 = L401_;
  }
 }
 after_loop: ;
 {
  BOOL f_L421_ = TRUE;
  L41 = 1;
  L51 = num_character_ar;
  L421_=L41-1;L421_m=L51; 
  while (1) {
   if(L421_++>=L421_m)  goto after_loop1; 
   i = L421_;
   plus_self7 = res;
   plus_sarg7 = ((STR) &F_LENGTH2);
   ret_val8 = STR_ap2004550586(plus_self7, plus_sarg7);
   res = ret_val8;
  }
 }
 after_loop1: ;
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e1130765144(CGEN self, AM_ROUT_DEF f) {
 FLISTdTP des1;
 INT mintag = INT_zero;
 INT maxtag = INT_zero;
 STR cst = ((STR) NULL);
 FSETdTP gh = ((FSETdTP) NULL);
 dTP fst;
 FSETdTP L11;
 dTP t;
 INT tag1 = INT_zero;
 STR decl;
 STR tbl;
 ARRAYARG L21;
 ARG e;
 INT L31 = INT_zero;
 INT L41 = INT_zero;
 INT i = INT_zero;
 BOOL exists = BOOL_zero;
 FLISTdTP L51;
 dTP e1;
 INT tag2 = INT_zero;
 SIG real_sig;
 STR functocall = ((STR) NULL);
 FLISTdTP create_self;
 FLISTdTP ret_val;
 TP_GRAPH_ABS_DES des_of_self;
 TP_CLASS des_of_tp;
 FSETdTP ret_val1;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val2;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val4;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val5;
 CGEN mang_self2;
 OB mang_ob2;
 STR ret_val6;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val7;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val8;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val9;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val10;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val11;
 CGEN mang_self3;
 OB mang_ob3;
 STR ret_val12;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val13;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val14;
 CGEN mang_self4;
 OB mang_ob4;
 STR ret_val15;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val16;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val17;
 CGEN mang_self5;
 OB mang_ob5;
 STR ret_val18;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val19;
 STR plus_self12;
 STR plus_sarg12;
 STR ret_val20;
 CGEN mang_self6;
 OB mang_ob6;
 STR ret_val21;
 STR plus_self13;
 STR plus_sarg13;
 STR ret_val22;
 STR plus_self14;
 STR plus_sarg14;
 STR ret_val23;
 CGEN mang_self7;
 OB mang_ob7;
 STR ret_val24;
 STR plus_self15;
 STR plus_sarg15;
 STR ret_val25;
 INT L61 = INT_zero;
 STR plus_self16;
 STR plus_sarg16;
 STR ret_val26;
 CGEN arg_type_str_sel;
 ARG arg_type_str_e;
 STR ret_val27;
 STR res;
 CGEN mang_self8;
 OB mang_ob8;
 STR ret_val28;
 STR plus_self17;
 STR plus_sarg17;
 STR ret_val29;
 STR plus_self18;
 STR plus_sarg18;
 STR ret_val30;
 STR plus_self19;
 STR plus_sarg19;
 STR ret_val31;
 CGEN arg_type_str_sel1;
 ARG arg_type_str_e1;
 STR ret_val32;
 STR res1;
 CGEN mang_self9;
 OB mang_ob9;
 STR ret_val33;
 STR plus_self20;
 STR plus_sarg20;
 STR ret_val34;
 STR plus_self21;
 STR plus_sarg21;
 STR ret_val35;
 STR plus_self22;
 STR plus_sarg22;
 STR ret_val36;
 CGEN arg_type_str_sel2;
 ARG arg_type_str_e2;
 STR ret_val37;
 STR res2;
 CGEN mang_self10;
 OB mang_ob10;
 STR ret_val38;
 STR plus_self23;
 STR plus_sarg23;
 STR ret_val39;
 STR plus_self24;
 STR plus_sarg24;
 STR ret_val40;
 STR plus_self25;
 STR plus_sarg25;
 STR ret_val41;
 STR plus_self26;
 STR plus_sarg26;
 STR ret_val42;
 STR plus_self27;
 STR plus_sarg27;
 STR ret_val43;
 STR plus_self28;
 STR plus_sarg28;
 STR ret_val44;
 STR plus_self29;
 STR plus_sarg29;
 STR ret_val45;
 CGEN mang_self11;
 OB mang_ob11;
 STR ret_val46;
 STR plus_self30;
 STR plus_sarg30;
 STR ret_val47;
 STR plus_self31;
 STR plus_sarg31;
 STR ret_val48;
 STR plus_self32;
 INT plus_arg = INT_zero;
 STR ret_val49;
 STR s;
 INT str_self = INT_zero;
 STR ret_val50;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val51;
 FSTR str_self1;
 STR ret_val52;
 STR plus_self33;
 STR plus_sarg32;
 STR ret_val53;
 CODE_FILE plus_self34;
 dSTR plus_s;
 CODE_FILE ret_val54;
 CODE_FILE plus_self35;
 dSTR plus_s1;
 STR plus_self36;
 STR plus_sarg33;
 STR ret_val55;
 CGEN make_sure_emitte;
 SIG make_sure_emitte1;
 AM_ROUT_DEF ard;
 CODE_FILE plus_self37;
 dSTR plus_s2;
 CODE_FILE ret_val56;
 CODE_FILE plus_self38;
 dSTR plus_s3;
 CODE_FILE ret_val57;
 CODE_FILE plus_self39;
 dSTR plus_s4;
 CODE_FILE plus_self40;
 dSTR plus_s5;
 CGEN comment_self;
 CODE_FILE comment_f;
 STR comment_com;
 CODE_FILE plus_self41;
 dSTR plus_s6;
 CODE_FILE ret_val58;
 CODE_FILE plus_self42;
 dSTR plus_s7;
 CODE_FILE ret_val59;
 CODE_FILE plus_self43;
 dSTR plus_s8;
 CODE_FILE plus_self44;
 dSTR plus_s9;
 CODE_FILE plus_self45;
 dSTR plus_s10;
 CODE_FILE ret_val60;
 CODE_FILE plus_self46;
 dSTR plus_s11;
 CODE_FILE ret_val61;
 CODE_FILE plus_self47;
 dSTR plus_s12;
 CODE_FILE ret_val62;
 CODE_FILE plus_self48;
 dSTR plus_s13;
 CODE_FILE plus_self49;
 dSTR plus_s14;
 CODE_FILE plus_self50;
 dSTR plus_s15;
 CODE_FILE plus_self51;
 dSTR plus_s16;
 CODE_FILE plus_self52;
 dSTR plus_s17;
 CODE_FILE ret_val63;
 CODE_FILE plus_self53;
 dSTR plus_s18;
 CODE_FILE ret_val64;
 CODE_FILE plus_self54;
 dSTR plus_s19;
 INT L71_=SINT_MAX;
 INT L81_=SINT_MIN;
 dTP L9;
 dTP L10;
 BOOL L12;
 BOOL L131_;
 BOOL L141_;
 INT L151_;
 INT L161_;
 extern STR RETURNED_CONST1;
 extern STR RETURNED_CONST1;
 extern STR RETURNED_CONST2;
 BOOL L17;
 BOOL L181_;
 extern STR void1;
 extern STR void1;
 extern STR void1;
 extern STR name165;
 extern STR S_tbl;
 extern STR name166;
 extern STR name167;
 extern STR name168;
 BOOL L19;
 BOOL L201_;
 INT L221_br;
 ARG aL221_;
 ARG L23;
 extern STR name29;
 BOOL L24;
 dMODE L25;
 dMODE L26;
 extern STR name58;
 extern STR name29;
 BOOL L27;
 dMODE L28;
 dMODE L29;
 extern STR name58;
 extern STR name29;
 BOOL L30;
 dMODE L32;
 dMODE L33;
 extern STR name58;
 INT L341_;
 extern STR struct139248200;
 extern STR struct139248200;
 extern STR struct139248200;
 extern STR name26;
 extern STR name169;
 extern STR S_tbl1;
 INT L351_;
 BOOL L36;
 BOOL L371_;
 extern STR name127;
 BOOL L38;
 BOOL L391_;
 FSTR L40;
 dSTR L42;
 extern STR name170;
 FSTR L43;
 dSTR L44;
 extern STR name87;
 INT L451_,L451_m;
 INT L46;
 dTP L47;
 dTP L48;
 IFC L49;
 BOOL L50;
 BOOL L52;
 BOOL L53;
 BOOL L541_;
 BOOL L551_;
 BOOL L561_;
 BOOL L57;
 BOOL L581_;
 BOOL L59;
 BOOL L601_;
 dSTR L62;
 OB L63;
 BOOL L64;
 BOOL L651_;
 FSTR L66;
 dSTR L67;
 BOOL L68;
 BOOL L691_;
 FSTR L70;
 dSTR L72;
 FSTR L73;
 dSTR L74;
 BOOL L751_;
 BOOL L76;
 BOOL L771_;
 extern STR name3;
 FSTR L78;
 dSTR L79;
 extern STR name78;
 BOOL L80;
 BOOL L821_;
 FSTR L83;
 dSTR L84;
 BOOL L85;
 BOOL L861_;
 FSTR L87;
 dSTR L88;
 extern STR name79;
 FSTR L89;
 dSTR L90;
 dSTR L91;
 OB L92;
 FSTR L93;
 dSTR L94;
 BOOL L951_;
 extern STR name54;
 BOOL L96;
 BOOL L971_;
 FSTR L98;
 dSTR L99;
 dSTR L100;
 OB L101;
 BOOL L102;
 BOOL L1031_;
 FSTR L104;
 dSTR L105;
 extern STR name79;
 BOOL L106;
 BOOL L1071_;
 FSTR L108;
 dSTR L109;
 extern STR NULL7;
 FSTR L110;
 dSTR L111;
 BOOL L1121_;
 BOOL L113;
 BOOL L1141_;
 extern STR name3;
 FSTR L115;
 dSTR L116;
 dSTR L117;
 OB L118;
 FSTR L119;
 dSTR L120;
 BOOL L1211_;
 extern STR NULLNo1888211653;
 FSTR L122;
 dSTR L123;
 extern STR name171;
 BOOL L124;
 BOOL L1251_;
 FSTR L126;
 dSTR L127;
 BOOL L128;
 BOOL L1291_;
 FSTR L130;
 dSTR L132;
 extern STR name9;
 FSTR L133;
 dSTR L134;
 CODE_F1291349628(ATTR(self,dispatch_c), ATTR(f,sig1));
 create_self = ((FLISTdTP) NULL);
 ret_val = ((FLISTdTP) NULL);
 des1 = ret_val;
 
 mintag = L71_;
 
 maxtag = L81_;
 fst = ATTR(ATTR(f,sig1),tp);
 switch (TAG(fst)) {
  case TP_CLASS_tag:
   des_of_self = ATTR(ATTR(self,prog),tp_graph_abs_des);
   des_of_tp = ((TP_CLASS) fst);
   ret_val1 = FMAPTP861983338(ATTR(des_of_self,tbl), des_of_tp);
   gh = ret_val1; break;
  default: ;
   FATAL("No applicable type in typecase\nin CGEN::emit_dispatch_func_table(AM_ROUT_DEF)\n./Back/cgen.sa:2680:14");
 }
 {
  struct FSETdTP_eltbrdTP_frame_struct other1_0;
FSETdTP_eltbrdTP_frame noname1 = &other1_0;
  L11 = gh;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETdTP_eltbrdTP(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   t = L9;
   L10 = t;
   L12 = (*dTP_is1553335918[TAG(L10)])(L10);
   L131_=!(L12); 
   if (L131_) {
    des1 = FLISTd1953924847(des1, t);
    tag1 = FMAPdT165518335(ATTR(self,tags), t);
    L141_=(tag1)==(0); 
    if (L141_) {
    }
    else {
     L151_=(maxtag>tag1)?maxtag:tag1; 
     maxtag = L151_;
     L161_=(mintag<tag1)?mintag:tag1; 
     mintag = L161_;
    }
   }
  }
 }
 after_loop: ;
 decl = ((STR) &RETURNED_CONST1);
 tbl = ((STR) &RETURNED_CONST1);
 cst = ((STR) &RETURNED_CONST2);
 L17 = (ATTR(ATTR(f,sig1),ret)==((dTP) NULL));
 L181_=!(L17); 
 if (L181_) {
  plus_self = decl;
  mang_self = self;
  mang_ob = ((OB) ATTR(ATTR(f,sig1),ret));
  ret_val2 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
  plus_sarg = ret_val2;
  ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
  decl = ret_val3;
  plus_self1 = tbl;
  mang_self1 = self;
  mang_ob1 = ((OB) ATTR(ATTR(f,sig1),ret));
  ret_val4 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
  plus_sarg1 = ret_val4;
  ret_val5 = STR_ap2004550586(plus_self1, plus_sarg1);
  tbl = ret_val5;
  plus_self2 = cst;
  mang_self2 = self;
  mang_ob2 = ((OB) ATTR(ATTR(f,sig1),ret));
  ret_val6 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, ((OB) NULL));
  plus_sarg2 = ret_val6;
  ret_val7 = STR_ap2004550586(plus_self2, plus_sarg2);
  cst = ret_val7;
 }
 else {
  plus_self3 = decl;
  plus_sarg3 = ((STR) &void1);
  ret_val8 = STR_ap2004550586(plus_self3, plus_sarg3);
  decl = ret_val8;
  plus_self4 = tbl;
  plus_sarg4 = ((STR) &void1);
  ret_val9 = STR_ap2004550586(plus_self4, plus_sarg4);
  tbl = ret_val9;
  plus_self5 = cst;
  plus_sarg5 = ((STR) &void1);
  ret_val10 = STR_ap2004550586(plus_self5, plus_sarg5);
  cst = ret_val10;
 }
 plus_self6 = decl;
 plus_sarg6 = ((STR) &name165);
 ret_val11 = STR_ap2004550586(plus_self6, plus_sarg6);
 plus_self7 = ret_val11;
 mang_self3 = self;
 mang_ob3 = ((OB) ATTR(f,sig1));
 ret_val12 = MANGLE119219986(ATTR(mang_self3,mangler), mang_ob3, ((OB) NULL));
 plus_sarg7 = ret_val12;
 ret_val13 = STR_ap2004550586(plus_self7, plus_sarg7);
 plus_self8 = ret_val13;
 plus_sarg8 = ((STR) &S_tbl);
 ret_val14 = STR_ap2004550586(plus_self8, plus_sarg8);
 plus_self9 = ret_val14;
 mang_self4 = self;
 mang_ob4 = ((OB) ATTR(ATTR(f,sig1),tp));
 ret_val15 = MANGLE119219986(ATTR(mang_self4,mangler), mang_ob4, ((OB) NULL));
 plus_sarg9 = ret_val15;
 ret_val16 = STR_ap2004550586(plus_self9, plus_sarg9);
 decl = ret_val16;
 plus_self10 = tbl;
 plus_sarg10 = ((STR) &name166);
 ret_val17 = STR_ap2004550586(plus_self10, plus_sarg10);
 plus_self11 = ret_val17;
 mang_self5 = self;
 mang_ob5 = ((OB) ATTR(f,sig1));
 ret_val18 = MANGLE119219986(ATTR(mang_self5,mangler), mang_ob5, ((OB) NULL));
 plus_sarg11 = ret_val18;
 ret_val19 = STR_ap2004550586(plus_self11, plus_sarg11);
 plus_self12 = ret_val19;
 plus_sarg12 = ((STR) &name167);
 ret_val20 = STR_ap2004550586(plus_self12, plus_sarg12);
 plus_self13 = ret_val20;
 mang_self6 = self;
 mang_ob6 = ((OB) ATTR(ATTR(f,sig1),tp));
 ret_val21 = MANGLE119219986(ATTR(mang_self6,mangler), mang_ob6, ((OB) NULL));
 plus_sarg13 = ret_val21;
 ret_val22 = STR_ap2004550586(plus_self13, plus_sarg13);
 tbl = ret_val22;
 plus_self14 = cst;
 plus_sarg14 = ((STR) &name168);
 ret_val23 = STR_ap2004550586(plus_self14, plus_sarg14);
 plus_self15 = ret_val23;
 mang_self7 = self;
 mang_ob7 = ((OB) ATTR(ATTR(f,sig1),tp));
 ret_val24 = MANGLE119219986(ATTR(mang_self7,mangler), mang_ob7, ((OB) NULL));
 plus_sarg15 = ret_val24;
 ret_val25 = STR_ap2004550586(plus_self15, plus_sarg15);
 cst = ret_val25;
 L19 = (ATTR(ATTR(f,sig1),args)==((ARRAYARG) NULL));
 L201_=!(L19); 
 if (L201_) {
  {
   BOOL f_L221_ = TRUE;
   /* loop index variable */
   L61 = 0;
   L21 = ATTR(ATTR(f,sig1),args);
   L221_br=L21==NULL?0:ASIZE((ARRAYARG)L21); 
   while (1) {
    if(L61>=L221_br)  goto after_loop1; 
    aL221_=ARR((ARRAYARG)L21,L61); 
    e = aL221_;
    plus_self16 = tbl;
    plus_sarg16 = ((STR) &name29);
    ret_val26 = STR_ap2004550586(plus_self16, plus_sarg16);
    plus_self18 = ret_val26;
    arg_type_str_sel = self;
    arg_type_str_e = e;
    mang_self8 = arg_type_str_sel;
    mang_ob8 = ((OB) ATTR(arg_type_str_e,tp));
    ret_val28 = MANGLE119219986(ATTR(mang_self8,mangler), mang_ob8, ((OB) NULL));
    res = ret_val28;
    L25 = ATTR(arg_type_str_e,mode1);
    if ((*dMODE_814247358[TAG(L25)])(L25, ((dMODE) MODES_inout_mode))) {
     L24 = TRUE;
    } else {
     L26 = ATTR(arg_type_str_e,mode1);
     L24 = (*dMODE_814247358[TAG(L26)])(L26, ((dMODE) MODES_out_mode));
    }
    if (L24) {
     plus_self17 = res;
     plus_sarg17 = ((STR) &name58);
     ret_val29 = STR_ap2004550586(plus_self17, plus_sarg17);
     res = ret_val29;
    }
    ret_val27 = res;
    plus_sarg18 = ret_val27;
    ret_val30 = STR_ap2004550586(plus_self18, plus_sarg18);
    tbl = ret_val30;
    plus_self19 = decl;
    plus_sarg19 = ((STR) &name29);
    ret_val31 = STR_ap2004550586(plus_self19, plus_sarg19);
    plus_self21 = ret_val31;
    arg_type_str_sel1 = self;
    arg_type_str_e1 = e;
    mang_self9 = arg_type_str_sel1;
    mang_ob9 = ((OB) ATTR(arg_type_str_e1,tp));
    ret_val33 = MANGLE119219986(ATTR(mang_self9,mangler), mang_ob9, ((OB) NULL));
    res1 = ret_val33;
    L28 = ATTR(arg_type_str_e1,mode1);
    if ((*dMODE_814247358[TAG(L28)])(L28, ((dMODE) MODES_inout_mode))) {
     L27 = TRUE;
    } else {
     L29 = ATTR(arg_type_str_e1,mode1);
     L27 = (*dMODE_814247358[TAG(L29)])(L29, ((dMODE) MODES_out_mode));
    }
    if (L27) {
     plus_self20 = res1;
     plus_sarg20 = ((STR) &name58);
     ret_val34 = STR_ap2004550586(plus_self20, plus_sarg20);
     res1 = ret_val34;
    }
    ret_val32 = res1;
    plus_sarg21 = ret_val32;
    ret_val35 = STR_ap2004550586(plus_self21, plus_sarg21);
    decl = ret_val35;
    plus_self22 = cst;
    plus_sarg22 = ((STR) &name29);
    ret_val36 = STR_ap2004550586(plus_self22, plus_sarg22);
    plus_self24 = ret_val36;
    arg_type_str_sel2 = self;
    arg_type_str_e2 = e;
    mang_self10 = arg_type_str_sel2;
    mang_ob10 = ((OB) ATTR(arg_type_str_e2,tp));
    ret_val38 = MANGLE119219986(ATTR(mang_self10,mangler), mang_ob10, ((OB) NULL));
    res2 = ret_val38;
    L32 = ATTR(arg_type_str_e2,mode1);
    if ((*dMODE_814247358[TAG(L32)])(L32, ((dMODE) MODES_inout_mode))) {
     L30 = TRUE;
    } else {
     L33 = ATTR(arg_type_str_e2,mode1);
     L30 = (*dMODE_814247358[TAG(L33)])(L33, ((dMODE) MODES_out_mode));
    }
    if (L30) {
     plus_self23 = res2;
     plus_sarg23 = ((STR) &name58);
     ret_val39 = STR_ap2004550586(plus_self23, plus_sarg23);
     res2 = ret_val39;
    }
    ret_val37 = res2;
    plus_sarg24 = ret_val37;
    ret_val40 = STR_ap2004550586(plus_self24, plus_sarg24);
    cst = ret_val40;
    L341_=INTPLUS(L61,1); 
    L61 = L341_;
   }
  }
  after_loop1: ;
 }
 if (ATTR(self,func_tables)) {
  plus_self25 = decl;
  plus_sarg25 = ((STR) &struct139248200);
  ret_val41 = STR_ap2004550586(plus_self25, plus_sarg25);
  decl = ret_val41;
  plus_self26 = tbl;
  plus_sarg26 = ((STR) &struct139248200);
  ret_val42 = STR_ap2004550586(plus_self26, plus_sarg26);
  tbl = ret_val42;
  plus_self27 = cst;
  plus_sarg27 = ((STR) &struct139248200);
  ret_val43 = STR_ap2004550586(plus_self27, plus_sarg27);
  cst = ret_val43;
 }
 plus_self28 = decl;
 plus_sarg28 = ((STR) &name26);
 ret_val44 = STR_ap2004550586(plus_self28, plus_sarg28);
 decl = ret_val44;
 plus_self29 = tbl;
 plus_sarg29 = ((STR) &name169);
 ret_val45 = STR_ap2004550586(plus_self29, plus_sarg29);
 plus_self30 = ret_val45;
 mang_self11 = self;
 mang_ob11 = ((OB) ATTR(f,sig1));
 ret_val46 = MANGLE119219986(ATTR(mang_self11,mangler), mang_ob11, ((OB) NULL));
 plus_sarg30 = ret_val46;
 ret_val47 = STR_ap2004550586(plus_self30, plus_sarg30);
 plus_self31 = ret_val47;
 plus_sarg31 = ((STR) &S_tbl1);
 ret_val48 = STR_ap2004550586(plus_self31, plus_sarg31);
 plus_self32 = ret_val48;
 L351_=INTMINUS(0,mintag); 
 plus_arg = L351_;
 str_self = plus_arg;
 clear_self = buf1;
 L36 = (clear_self==((FSTR) NULL));
 L371_=!(L36); 
 if (L371_) {
  SATTR(clear_self,loc,0);
 }
 str_in_self = str_self;
 str_in_s = buf1;
 ret_val51 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
 buf1 = ret_val51;
 str_self1 = buf1;
 ret_val52 = STR_fr1097270334(((STR) NULL), str_self1);
 ret_val50 = ret_val52;
 s = ret_val50;
 ret_val49 = STR_ap1077157958(plus_self32, s, TRUE);
 plus_self33 = ret_val49;
 plus_sarg32 = ((STR) &name127);
 ret_val53 = STR_ap2004550586(plus_self33, plus_sarg32);
 tbl = ret_val53;
 plus_self34 = ATTR(self,dispatch_c);
 plus_s = ((dSTR) decl);
 L38 = (plus_s==((dSTR) NULL));
 L391_=!(L38); 
 if (L391_) {
  L40 = ATTR(plus_self34,ntext);
  L42 = plus_s;
  SATTR(plus_self34,ntext,FSTR_p1752847026(L40, (*dSTR_strrSTR[TAG(L42)])(L42)));
 }
 ret_val54 = plus_self34;
 plus_self35 = ret_val54;
 plus_s1 = ((dSTR) ((STR) &name170));
 L43 = ATTR(plus_self35,ntext);
 L44 = plus_s1;
 SATTR(plus_self35,ntext,FSTR_p1752847026(L43, (*dSTR_strrSTR[TAG(L44)])(L44)));
 plus_self36 = cst;
 plus_sarg33 = ((STR) &name87);
 ret_val55 = STR_ap2004550586(plus_self36, plus_sarg33);
 cst = ret_val55;
 {
  BOOL f_L451_ = TRUE;
  L31 = mintag;
  L41 = maxtag;
  L451_=L31-1;L451_m=L41; 
  while (1) {
   if(L451_++>=L451_m)  goto after_loop2; 
   i = L451_;
   exists = FALSE;
   {
    struct FLISTd1740602786_frame_struct other4_0;
FLISTd1740602786_frame noname2 = &other4_0;
    L51 = des1;
    noname2->self = L51;
    noname2->state = 0;
    while (1) {
     L47 = FLISTd1740602786(noname2);
     if (noname2->state == -1) {
      goto after_loop3;
     }
     e1 = L47;
     tag2 = FMAPdT165518335(ATTR(self,tags), e1);
     L48 = e1;
     L49 = (*dTP_ifcrIFC[TAG(L48)])(L48);
     real_sig = IFC_si2085565042(L49, ATTR(f,sig1));
     L53 = (real_sig==((SIG) NULL));
     L541_=!(L53); 
     if (L541_) {
      L551_=(tag2)==(i); 
      L52 = L551_;
     } else {
      L52 = FALSE;
     }
     if (L52) {
      L561_=(tag2)==(0); 
      L57 = L561_;
      L581_=!(L57); 
      L50 = L581_;
     } else {
      L50 = FALSE;
     }
     if (L50) {
      exists = TRUE;
      make_sure_emitte = self;
      make_sure_emitte1 = real_sig;
      ard = FMAPSI2016001247(ATTR(make_sure_emitte,not_emitted), make_sure_emitte1);
      L59 = (ard==((AM_ROUT_DEF) NULL));
      L601_=!(L59); 
      if (L601_) {
       SATTR(make_sure_emitte,leftovers,FSETAM2044524049(ATTR(make_sure_emitte,leftovers), ard));
       SATTR(make_sure_emitte,not_emitted,FMAPSI1625125906(ATTR(make_sure_emitte,not_emitted), make_sure_emitte1));
      }
      functocall = CGEN_e56867199(self, real_sig, ATTR(f,sig1));
      plus_self37 = ATTR(self,dispatch_c);
      L63=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
      if (L63==NULL) FATAL("Unable to allocate more memory");
      memset(L63,0,sizeof(struct CHAR_boxed_struct));
      ((OB)L63)->header.tag=CHAR_tag;
      L62 = (dSTR)((CHAR_boxed) L63);
      ((CHAR_boxed) L62)->immutable_part = ' ';
      plus_s2 = L62;
      L64 = (plus_s2==((dSTR) NULL));
      L651_=!(L64); 
      if (L651_) {
       L66 = ATTR(plus_self37,ntext);
       L67 = plus_s2;
       SATTR(plus_self37,ntext,FSTR_p1752847026(L66, (*dSTR_strrSTR[TAG(L67)])(L67)));
      }
      ret_val56 = plus_self37;
      plus_self38 = ret_val56;
      plus_s3 = ((dSTR) cst);
      L68 = (plus_s3==((dSTR) NULL));
      L691_=!(L68); 
      if (L691_) {
       L70 = ATTR(plus_self38,ntext);
       L72 = plus_s3;
       SATTR(plus_self38,ntext,FSTR_p1752847026(L70, (*dSTR_strrSTR[TAG(L72)])(L72)));
      }
      ret_val57 = plus_self38;
      plus_self39 = ret_val57;
      plus_s4 = ((dSTR) functocall);
      L73 = ATTR(plus_self39,ntext);
      L74 = plus_s4;
      SATTR(plus_self39,ntext,FSTR_p1752847026(L73, (*dSTR_strrSTR[TAG(L74)])(L74)));
      L751_=(i)==(maxtag); 
      L76 = L751_;
      L771_=!(L76); 
      if (L771_) {
       plus_self40 = ATTR(self,dispatch_c);
       plus_s5 = ((dSTR) ((STR) &name3));
       L78 = ATTR(plus_self40,ntext);
       L79 = plus_s5;
       SATTR(plus_self40,ntext,FSTR_p1752847026(L78, (*dSTR_strrSTR[TAG(L79)])(L79)));
      }
      comment_self = self;
      comment_f = ATTR(self,dispatch_c);
      comment_com = ATTR(real_sig,str);
      if (ATTR(comment_self,pretty)) {
       plus_self41 = comment_f;
       plus_s6 = ((dSTR) ((STR) &name78));
       L80 = (plus_s6==((dSTR) NULL));
       L821_=!(L80); 
       if (L821_) {
        L83 = ATTR(plus_self41,ntext);
        L84 = plus_s6;
        SATTR(plus_self41,ntext,FSTR_p1752847026(L83, (*dSTR_strrSTR[TAG(L84)])(L84)));
       }
       ret_val58 = plus_self41;
       plus_self42 = ret_val58;
       plus_s7 = ((dSTR) comment_com);
       L85 = (plus_s7==((dSTR) NULL));
       L861_=!(L85); 
       if (L861_) {
        L87 = ATTR(plus_self42,ntext);
        L88 = plus_s7;
        SATTR(plus_self42,ntext,FSTR_p1752847026(L87, (*dSTR_strrSTR[TAG(L88)])(L88)));
       }
       ret_val59 = plus_self42;
       plus_self43 = ret_val59;
       plus_s8 = ((dSTR) ((STR) &name79));
       L89 = ATTR(plus_self43,ntext);
       L90 = plus_s8;
       SATTR(plus_self43,ntext,FSTR_p1752847026(L89, (*dSTR_strrSTR[TAG(L90)])(L90)));
      }
      plus_self44 = ATTR(self,dispatch_c);
      L92=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
      if (L92==NULL) FATAL("Unable to allocate more memory");
      memset(L92,0,sizeof(struct CHAR_boxed_struct));
      ((OB)L92)->header.tag=CHAR_tag;
      L91 = (dSTR)((CHAR_boxed) L92);
      ((CHAR_boxed) L91)->immutable_part = '\n';
      plus_s9 = L91;
      L93 = ATTR(plus_self44,ntext);
      L94 = plus_s9;
      SATTR(plus_self44,ntext,FSTR_p1752847026(L93, (*dSTR_strrSTR[TAG(L94)])(L94)));
     }
    }
   }
   after_loop3: ;
   L951_=!(exists); 
   if (L951_) {
    plus_self45 = ATTR(self,dispatch_c);
    plus_s10 = ((dSTR) ((STR) &name54));
    L96 = (plus_s10==((dSTR) NULL));
    L971_=!(L96); 
    if (L971_) {
     L98 = ATTR(plus_self45,ntext);
     L99 = plus_s10;
     SATTR(plus_self45,ntext,FSTR_p1752847026(L98, (*dSTR_strrSTR[TAG(L99)])(L99)));
    }
    ret_val60 = plus_self45;
    plus_self46 = ret_val60;
    L101=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
    if (L101==NULL) FATAL("Unable to allocate more memory");
    memset(L101,0,sizeof(struct INT_boxed_struct));
    ((OB)L101)->header.tag=INT_tag;
    L100 = (dSTR)((INT_boxed) L101);
    ((INT_boxed) L100)->immutable_part = i;
    plus_s11 = L100;
    L102 = (plus_s11==((dSTR) NULL));
    L1031_=!(L102); 
    if (L1031_) {
     L104 = ATTR(plus_self46,ntext);
     L105 = plus_s11;
     SATTR(plus_self46,ntext,FSTR_p1752847026(L104, (*dSTR_strrSTR[TAG(L105)])(L105)));
    }
    ret_val61 = plus_self46;
    plus_self47 = ret_val61;
    plus_s12 = ((dSTR) ((STR) &name79));
    L106 = (plus_s12==((dSTR) NULL));
    L1071_=!(L106); 
    if (L1071_) {
     L108 = ATTR(plus_self47,ntext);
     L109 = plus_s12;
     SATTR(plus_self47,ntext,FSTR_p1752847026(L108, (*dSTR_strrSTR[TAG(L109)])(L109)));
    }
    ret_val62 = plus_self47;
    plus_self48 = ret_val62;
    plus_s13 = ((dSTR) ((STR) &NULL7));
    L110 = ATTR(plus_self48,ntext);
    L111 = plus_s13;
    SATTR(plus_self48,ntext,FSTR_p1752847026(L110, (*dSTR_strrSTR[TAG(L111)])(L111)));
    L1121_=(i)==(maxtag); 
    L113 = L1121_;
    L1141_=!(L113); 
    if (L1141_) {
     plus_self49 = ATTR(self,dispatch_c);
     plus_s14 = ((dSTR) ((STR) &name3));
     L115 = ATTR(plus_self49,ntext);
     L116 = plus_s14;
     SATTR(plus_self49,ntext,FSTR_p1752847026(L115, (*dSTR_strrSTR[TAG(L116)])(L116)));
    }
    plus_self50 = ATTR(self,dispatch_c);
    L118=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
    if (L118==NULL) FATAL("Unable to allocate more memory");
    memset(L118,0,sizeof(struct CHAR_boxed_struct));
    ((OB)L118)->header.tag=CHAR_tag;
    L117 = (dSTR)((CHAR_boxed) L118);
    ((CHAR_boxed) L117)->immutable_part = '\n';
    plus_s15 = L117;
    L119 = ATTR(plus_self50,ntext);
    L120 = plus_s15;
    SATTR(plus_self50,ntext,FSTR_p1752847026(L119, (*dSTR_strrSTR[TAG(L120)])(L120)));
   }
  }
 }
 after_loop2: ;
 L1211_=(maxtag)<(mintag); 
 if (L1211_) {
  plus_self51 = ATTR(self,dispatch_c);
  plus_s16 = ((dSTR) ((STR) &NULLNo1888211653));
  L122 = ATTR(plus_self51,ntext);
  L123 = plus_s16;
  SATTR(plus_self51,ntext,FSTR_p1752847026(L122, (*dSTR_strrSTR[TAG(L123)])(L123)));
 }
 plus_self52 = ATTR(self,dispatch_c);
 plus_s17 = ((dSTR) ((STR) &name171));
 L124 = (plus_s17==((dSTR) NULL));
 L1251_=!(L124); 
 if (L1251_) {
  L126 = ATTR(plus_self52,ntext);
  L127 = plus_s17;
  SATTR(plus_self52,ntext,FSTR_p1752847026(L126, (*dSTR_strrSTR[TAG(L127)])(L127)));
 }
 ret_val63 = plus_self52;
 plus_self53 = ret_val63;
 plus_s18 = ((dSTR) tbl);
 L128 = (plus_s18==((dSTR) NULL));
 L1291_=!(L128); 
 if (L1291_) {
  L130 = ATTR(plus_self53,ntext);
  L132 = plus_s18;
  SATTR(plus_self53,ntext,FSTR_p1752847026(L130, (*dSTR_strrSTR[TAG(L132)])(L132)));
 }
 ret_val64 = plus_self53;
 plus_self54 = ret_val64;
 plus_s19 = ((dSTR) ((STR) &name9));
 L133 = ATTR(plus_self54,ntext);
 L134 = plus_s19;
 SATTR(plus_self54,ntext,FSTR_p1752847026(L133, (*dSTR_strrSTR[TAG(L134)])(L134)));
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e1935776612(CGEN self, AM_ROUT_DEF f) {
 FLISTdTP des1;
 INT mintag = INT_zero;
 INT maxtag = INT_zero;
 STR cst = ((STR) NULL);
 FSETdTP gh = ((FSETdTP) NULL);
 dTP fst;
 FSETdTP L11;
 dTP t;
 INT tag1 = INT_zero;
 STR decl;
 STR tbl;
 STR alloc_cast;
 INT L21 = INT_zero;
 INT L31 = INT_zero;
 INT i = INT_zero;
 BOOL exists = BOOL_zero;
 FLISTdTP L41;
 dTP e;
 INT tag2 = INT_zero;
 SIG real_sig;
 STR functocall = ((STR) NULL);
 STR funcalloc;
 FLISTdTP create_self;
 FLISTdTP ret_val;
 TP_GRAPH_ABS_DES des_of_self;
 TP_CLASS des_of_tp;
 FSETdTP ret_val1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 CGEN comment_self;
 CODE_FILE comment_f;
 STR comment_com;
 CODE_FILE plus_self1;
 dSTR plus_s;
 CODE_FILE ret_val3;
 CODE_FILE plus_self2;
 dSTR plus_s1;
 CODE_FILE ret_val4;
 CODE_FILE plus_self3;
 dSTR plus_s2;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val5;
 STR plus_self4;
 STR plus_sarg1;
 STR ret_val6;
 STR plus_self5;
 STR plus_sarg2;
 STR ret_val7;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val8;
 STR plus_self6;
 STR plus_sarg3;
 STR ret_val9;
 STR plus_self7;
 STR plus_sarg4;
 STR ret_val10;
 CGEN mang_self2;
 OB mang_ob2;
 STR ret_val11;
 STR plus_self8;
 STR plus_sarg5;
 STR ret_val12;
 STR plus_self9;
 STR plus_sarg6;
 STR ret_val13;
 STR plus_self10;
 STR plus_sarg7;
 STR ret_val14;
 CGEN mang_self3;
 OB mang_ob3;
 STR ret_val15;
 STR plus_self11;
 STR plus_sarg8;
 STR ret_val16;
 CGEN mang_self4;
 OB mang_ob4;
 STR ret_val17;
 STR plus_self12;
 STR plus_sarg9;
 STR ret_val18;
 STR plus_self13;
 STR plus_sarg10;
 STR ret_val19;
 STR plus_self14;
 STR plus_sarg11;
 STR ret_val20;
 CGEN mang_self5;
 OB mang_ob5;
 STR ret_val21;
 STR plus_self15;
 STR plus_sarg12;
 STR ret_val22;
 STR plus_self16;
 STR plus_sarg13;
 STR ret_val23;
 CODE_FILE plus_self17;
 dSTR plus_s3;
 CODE_FILE ret_val24;
 CODE_FILE plus_self18;
 dSTR plus_s4;
 CODE_FILE ret_val25;
 CODE_FILE plus_self19;
 dSTR plus_s5;
 STR plus_self20;
 STR plus_sarg14;
 STR ret_val26;
 STR plus_self21;
 STR plus_sarg15;
 STR ret_val27;
 STR plus_self22;
 STR plus_sarg16;
 STR ret_val28;
 CGEN mang_self6;
 OB mang_ob6;
 STR ret_val29;
 STR plus_self23;
 STR plus_sarg17;
 STR ret_val30;
 STR plus_self24;
 STR plus_sarg18;
 STR ret_val31;
 STR plus_self25;
 INT plus_arg = INT_zero;
 STR ret_val32;
 STR s;
 INT str_self = INT_zero;
 STR ret_val33;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val34;
 FSTR str_self1;
 STR ret_val35;
 STR plus_self26;
 STR plus_sarg19;
 STR ret_val36;
 CGEN make_sure_emitte;
 SIG make_sure_emitte1;
 AM_ROUT_DEF ard;
 CODE_FILE uses_iter_self;
 SIG uses_iter_s;
 CODE_FILE uses_iter_self1;
 SIG uses_iter_s1;
 CODE_FILE plus_self27;
 dSTR plus_s6;
 CODE_FILE ret_val37;
 CODE_FILE plus_self28;
 dSTR plus_s7;
 CODE_FILE ret_val38;
 CODE_FILE plus_self29;
 dSTR plus_s8;
 CODE_FILE ret_val39;
 CODE_FILE plus_self30;
 dSTR plus_s9;
 CODE_FILE ret_val40;
 CODE_FILE plus_self31;
 dSTR plus_s10;
 CODE_FILE ret_val41;
 CODE_FILE plus_self32;
 dSTR plus_s11;
 CODE_FILE ret_val42;
 CODE_FILE plus_self33;
 dSTR plus_s12;
 CODE_FILE plus_self34;
 dSTR plus_s13;
 CGEN comment_self1;
 CODE_FILE comment_f1;
 STR comment_com1;
 CODE_FILE plus_self35;
 dSTR plus_s14;
 CODE_FILE ret_val43;
 CODE_FILE plus_self36;
 dSTR plus_s15;
 CODE_FILE ret_val44;
 CODE_FILE plus_self37;
 dSTR plus_s16;
 CODE_FILE plus_self38;
 dSTR plus_s17;
 CODE_FILE plus_self39;
 dSTR plus_s18;
 CODE_FILE plus_self40;
 dSTR plus_s19;
 CODE_FILE plus_self41;
 dSTR plus_s20;
 CODE_FILE plus_self42;
 dSTR plus_s21;
 CODE_FILE plus_self43;
 dSTR plus_s22;
 CODE_FILE ret_val45;
 CODE_FILE plus_self44;
 dSTR plus_s23;
 CODE_FILE ret_val46;
 CODE_FILE plus_self45;
 dSTR plus_s24;
 INT L51_=SINT_MAX;
 INT L61_=SINT_MIN;
 dTP L7;
 dTP L8;
 BOOL L9;
 BOOL L101_;
 BOOL L121_;
 BOOL L13;
 BOOL L141_;
 INT L151_;
 INT L161_;
 extern STR Iterat1041410143;
 extern STR name78;
 BOOL L17;
 BOOL L181_;
 FSTR L19;
 dSTR L20;
 BOOL L22;
 BOOL L231_;
 FSTR L24;
 dSTR L25;
 extern STR name79;
 FSTR L26;
 dSTR L27;
 extern STR RETURNED_CONST1;
 extern STR S_entry;
 extern STR RETURNED_CONST1;
 extern STR S_entry;
 extern STR RETURNED_CONST2;
 extern STR S_tbl2;
 extern STR name58;
 BOOL L28;
 BOOL L291_;
 extern STR void1;
 extern STR name168;
 extern STR S_frame4;
 extern STR name9;
 BOOL L30;
 BOOL L321_;
 FSTR L33;
 dSTR L34;
 BOOL L35;
 BOOL L361_;
 FSTR L37;
 dSTR L38;
 extern STR name170;
 FSTR L39;
 dSTR L40;
 extern STR struct1392482001;
 extern STR name87;
 extern STR name33;
 extern STR S_tbl1;
 INT L421_;
 BOOL L43;
 BOOL L441_;
 extern STR name127;
 extern STR constvoid;
 INT L451_,L451_m;
 INT L46;
 dTP L47;
 dTP L48;
 IFC L49;
 BOOL L50;
 BOOL L52;
 BOOL L53;
 BOOL L541_;
 BOOL L551_;
 BOOL L561_;
 BOOL L57;
 BOOL L581_;
 BOOL L59;
 BOOL L601_;
 extern STR name173;
 BOOL L62;
 BOOL L631_;
 FSTR L64;
 dSTR L65;
 BOOL L66;
 BOOL L671_;
 FSTR L68;
 dSTR L69;
 BOOL L70;
 BOOL L711_;
 FSTR L72;
 dSTR L73;
 extern STR name29;
 BOOL L74;
 BOOL L751_;
 FSTR L76;
 dSTR L77;
 BOOL L78;
 BOOL L791_;
 FSTR L80;
 dSTR L81;
 BOOL L82;
 BOOL L831_;
 FSTR L84;
 dSTR L85;
 extern STR name4;
 FSTR L86;
 dSTR L87;
 BOOL L881_;
 BOOL L89;
 BOOL L901_;
 extern STR name3;
 FSTR L91;
 dSTR L92;
 extern STR name78;
 BOOL L93;
 BOOL L941_;
 FSTR L95;
 dSTR L96;
 BOOL L97;
 BOOL L981_;
 FSTR L99;
 dSTR L100;
 extern STR name79;
 FSTR L102;
 dSTR L103;
 dSTR L104;
 OB L105;
 FSTR L106;
 dSTR L107;
 BOOL L1081_;
 extern STR NULLNULL1;
 FSTR L109;
 dSTR L110;
 BOOL L1111_;
 BOOL L112;
 BOOL L1131_;
 extern STR name3;
 FSTR L114;
 dSTR L115;
 dSTR L116;
 OB L117;
 FSTR L118;
 dSTR L119;
 BOOL L1201_;
 extern STR NULLNU651700710;
 FSTR L122;
 dSTR L123;
 extern STR name171;
 BOOL L124;
 BOOL L1251_;
 FSTR L126;
 dSTR L127;
 BOOL L128;
 BOOL L1291_;
 FSTR L130;
 dSTR L131;
 extern STR name9;
 FSTR L132;
 dSTR L133;
 CODE_F1291349628(ATTR(self,dispatch_c), ATTR(f,sig1));
 create_self = ((FLISTdTP) NULL);
 ret_val = ((FLISTdTP) NULL);
 des1 = ret_val;
 
 mintag = L51_;
 
 maxtag = L61_;
 fst = ATTR(ATTR(f,sig1),tp);
 switch (TAG(fst)) {
  case TP_CLASS_tag:
   des_of_self = ATTR(ATTR(self,prog),tp_graph_abs_des);
   des_of_tp = ((TP_CLASS) fst);
   ret_val1 = FMAPTP861983338(ATTR(des_of_self,tbl), des_of_tp);
   gh = ret_val1; break;
  default: ;
   FATAL("No applicable type in typecase\nin CGEN::emit_dispatch_iter_table(AM_ROUT_DEF)\n./Back/cgen.sa:2791:14");
 }
 {
  struct FSETdTP_eltbrdTP_frame_struct other1_0;
FSETdTP_eltbrdTP_frame noname1 = &other1_0;
  L11 = gh;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L7 = FSETdTP_eltbrdTP(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   t = L7;
   L8 = t;
   L9 = (*dTP_is1553335918[TAG(L8)])(L8);
   L101_=!(L9); 
   if (L101_) {
    des1 = FLISTd1953924847(des1, t);
    tag1 = FMAPdT165518335(ATTR(self,tags), t);
    L121_=(tag1)==(0); 
    L13 = L121_;
    L141_=!(L13); 
    if (L141_) {
     L151_=(maxtag>tag1)?maxtag:tag1; 
     maxtag = L151_;
     L161_=(mintag<tag1)?mintag:tag1; 
     mintag = L161_;
    }
   }
  }
 }
 after_loop: ;
 comment_self = self;
 comment_f = ATTR(self,dispatch_c);
 plus_self = ((STR) &Iterat1041410143);
 plus_sarg = ATTR(ATTR(f,sig1),str);
 ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
 comment_com = ret_val2;
 if (ATTR(comment_self,pretty)) {
  plus_self1 = comment_f;
  plus_s = ((dSTR) ((STR) &name78));
  L17 = (plus_s==((dSTR) NULL));
  L181_=!(L17); 
  if (L181_) {
   L19 = ATTR(plus_self1,ntext);
   L20 = plus_s;
   SATTR(plus_self1,ntext,FSTR_p1752847026(L19, (*dSTR_strrSTR[TAG(L20)])(L20)));
  }
  ret_val3 = plus_self1;
  plus_self2 = ret_val3;
  plus_s1 = ((dSTR) comment_com);
  L22 = (plus_s1==((dSTR) NULL));
  L231_=!(L22); 
  if (L231_) {
   L24 = ATTR(plus_self2,ntext);
   L25 = plus_s1;
   SATTR(plus_self2,ntext,FSTR_p1752847026(L24, (*dSTR_strrSTR[TAG(L25)])(L25)));
  }
  ret_val4 = plus_self2;
  plus_self3 = ret_val4;
  plus_s2 = ((dSTR) ((STR) &name79));
  L26 = ATTR(plus_self3,ntext);
  L27 = plus_s2;
  SATTR(plus_self3,ntext,FSTR_p1752847026(L26, (*dSTR_strrSTR[TAG(L27)])(L27)));
 }
 plus_self4 = ((STR) &RETURNED_CONST1);
 mang_self = self;
 mang_ob = ((OB) ATTR(f,sig1));
 ret_val5 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
 plus_sarg1 = ret_val5;
 ret_val6 = STR_ap2004550586(plus_self4, plus_sarg1);
 plus_self5 = ret_val6;
 plus_sarg2 = ((STR) &S_entry);
 ret_val7 = STR_ap2004550586(plus_self5, plus_sarg2);
 decl = ret_val7;
 plus_self6 = ((STR) &RETURNED_CONST1);
 mang_self1 = self;
 mang_ob1 = ((OB) ATTR(f,sig1));
 ret_val8 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
 plus_sarg3 = ret_val8;
 ret_val9 = STR_ap2004550586(plus_self6, plus_sarg3);
 plus_self7 = ret_val9;
 plus_sarg4 = ((STR) &S_entry);
 ret_val10 = STR_ap2004550586(plus_self7, plus_sarg4);
 tbl = ret_val10;
 cst = ((STR) &RETURNED_CONST2);
 plus_self8 = decl;
 mang_self2 = self;
 mang_ob2 = ((OB) ATTR(f,sig1));
 ret_val11 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, ((OB) NULL));
 plus_sarg5 = ret_val11;
 ret_val12 = STR_ap2004550586(plus_self8, plus_sarg5);
 plus_self9 = ret_val12;
 plus_sarg6 = ((STR) &S_tbl2);
 ret_val13 = STR_ap2004550586(plus_self9, plus_sarg6);
 decl = ret_val13;
 plus_self10 = tbl;
 plus_sarg7 = ((STR) &name58);
 ret_val14 = STR_ap2004550586(plus_self10, plus_sarg7);
 plus_self11 = ret_val14;
 mang_self3 = self;
 mang_ob3 = ((OB) ATTR(f,sig1));
 ret_val15 = MANGLE119219986(ATTR(mang_self3,mangler), mang_ob3, ((OB) NULL));
 plus_sarg8 = ret_val15;
 ret_val16 = STR_ap2004550586(plus_self11, plus_sarg8);
 tbl = ret_val16;
 L28 = (ATTR(ATTR(f,sig1),ret)==((dTP) NULL));
 L291_=!(L28); 
 if (L291_) {
  plus_self12 = cst;
  mang_self4 = self;
  mang_ob4 = ((OB) ATTR(ATTR(f,sig1),ret));
  ret_val17 = MANGLE119219986(ATTR(mang_self4,mangler), mang_ob4, ((OB) NULL));
  plus_sarg9 = ret_val17;
  ret_val18 = STR_ap2004550586(plus_self12, plus_sarg9);
  cst = ret_val18;
 }
 else {
  plus_self13 = cst;
  plus_sarg10 = ((STR) &void1);
  ret_val19 = STR_ap2004550586(plus_self13, plus_sarg10);
  cst = ret_val19;
 }
 plus_self14 = cst;
 plus_sarg11 = ((STR) &name168);
 ret_val20 = STR_ap2004550586(plus_self14, plus_sarg11);
 plus_self15 = ret_val20;
 mang_self5 = self;
 mang_ob5 = ((OB) ATTR(f,sig1));
 ret_val21 = MANGLE119219986(ATTR(mang_self5,mangler), mang_ob5, ((OB) NULL));
 plus_sarg12 = ret_val21;
 ret_val22 = STR_ap2004550586(plus_self15, plus_sarg12);
 plus_self16 = ret_val22;
 plus_sarg13 = ((STR) &S_frame4);
 ret_val23 = STR_ap2004550586(plus_self16, plus_sarg13);
 cst = ret_val23;
 plus_self17 = ATTR(self,dispatch_c);
 plus_s3 = ((dSTR) ((STR) &name9));
 L30 = (plus_s3==((dSTR) NULL));
 L321_=!(L30); 
 if (L321_) {
  L33 = ATTR(plus_self17,ntext);
  L34 = plus_s3;
  SATTR(plus_self17,ntext,FSTR_p1752847026(L33, (*dSTR_strrSTR[TAG(L34)])(L34)));
 }
 ret_val24 = plus_self17;
 plus_self18 = ret_val24;
 plus_s4 = ((dSTR) decl);
 L35 = (plus_s4==((dSTR) NULL));
 L361_=!(L35); 
 if (L361_) {
  L37 = ATTR(plus_self18,ntext);
  L38 = plus_s4;
  SATTR(plus_self18,ntext,FSTR_p1752847026(L37, (*dSTR_strrSTR[TAG(L38)])(L38)));
 }
 ret_val25 = plus_self18;
 plus_self19 = ret_val25;
 plus_s5 = ((dSTR) ((STR) &name170));
 L39 = ATTR(plus_self19,ntext);
 L40 = plus_s5;
 SATTR(plus_self19,ntext,FSTR_p1752847026(L39, (*dSTR_strrSTR[TAG(L40)])(L40)));
 if (ATTR(self,func_tables)) {
  plus_self20 = cst;
  plus_sarg14 = ((STR) &struct1392482001);
  ret_val26 = STR_ap2004550586(plus_self20, plus_sarg14);
  cst = ret_val26;
 }
 plus_self21 = cst;
 plus_sarg15 = ((STR) &name87);
 ret_val27 = STR_ap2004550586(plus_self21, plus_sarg15);
 cst = ret_val27;
 plus_self22 = tbl;
 plus_sarg16 = ((STR) &name33);
 ret_val28 = STR_ap2004550586(plus_self22, plus_sarg16);
 plus_self23 = ret_val28;
 mang_self6 = self;
 mang_ob6 = ((OB) ATTR(f,sig1));
 ret_val29 = MANGLE119219986(ATTR(mang_self6,mangler), mang_ob6, ((OB) NULL));
 plus_sarg17 = ret_val29;
 ret_val30 = STR_ap2004550586(plus_self23, plus_sarg17);
 plus_self24 = ret_val30;
 plus_sarg18 = ((STR) &S_tbl1);
 ret_val31 = STR_ap2004550586(plus_self24, plus_sarg18);
 plus_self25 = ret_val31;
 L421_=INTMINUS(0,mintag); 
 plus_arg = L421_;
 str_self = plus_arg;
 clear_self = buf1;
 L43 = (clear_self==((FSTR) NULL));
 L441_=!(L43); 
 if (L441_) {
  SATTR(clear_self,loc,0);
 }
 str_in_self = str_self;
 str_in_s = buf1;
 ret_val34 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
 buf1 = ret_val34;
 str_self1 = buf1;
 ret_val35 = STR_fr1097270334(((STR) NULL), str_self1);
 ret_val33 = ret_val35;
 s = ret_val33;
 ret_val32 = STR_ap1077157958(plus_self25, s, TRUE);
 plus_self26 = ret_val32;
 plus_sarg19 = ((STR) &name127);
 ret_val36 = STR_ap2004550586(plus_self26, plus_sarg19);
 tbl = ret_val36;
 alloc_cast = ((STR) &constvoid);
 {
  BOOL f_L451_ = TRUE;
  L21 = mintag;
  L31 = maxtag;
  L451_=L21-1;L451_m=L31; 
  while (1) {
   if(L451_++>=L451_m)  goto after_loop1; 
   i = L451_;
   exists = FALSE;
   {
    struct FLISTd1740602786_frame_struct other3_0;
FLISTd1740602786_frame noname2 = &other3_0;
    L41 = des1;
    noname2->self = L41;
    noname2->state = 0;
    while (1) {
     L47 = FLISTd1740602786(noname2);
     if (noname2->state == -1) {
      goto after_loop2;
     }
     e = L47;
     tag2 = FMAPdT165518335(ATTR(self,tags), e);
     L48 = e;
     L49 = (*dTP_ifcrIFC[TAG(L48)])(L48);
     real_sig = IFC_si2085565042(L49, ATTR(f,sig1));
     L53 = INT_IS_VOID(tag2);
     L541_=!(L53); 
     if (L541_) {
      L551_=(tag2)==(i); 
      L52 = L551_;
     } else {
      L52 = FALSE;
     }
     if (L52) {
      L561_=(tag2)==(0); 
      L57 = L561_;
      L581_=!(L57); 
      L50 = L581_;
     } else {
      L50 = FALSE;
     }
     if (L50) {
      exists = TRUE;
      make_sure_emitte = self;
      make_sure_emitte1 = real_sig;
      ard = FMAPSI2016001247(ATTR(make_sure_emitte,not_emitted), make_sure_emitte1);
      L59 = (ard==((AM_ROUT_DEF) NULL));
      L601_=!(L59); 
      if (L601_) {
       SATTR(make_sure_emitte,leftovers,FSETAM2044524049(ATTR(make_sure_emitte,leftovers), ard));
       SATTR(make_sure_emitte,not_emitted,FMAPSI1625125906(ATTR(make_sure_emitte,not_emitted), make_sure_emitte1));
      }
      uses_iter_self = ATTR(self,dispatch_c);
      uses_iter_s = real_sig;
      SATTR(uses_iter_self,iters,FSETSI671175003(ATTR(uses_iter_self,iters), uses_iter_s));
      uses_iter_self1 = ATTR(self,unbox_c);
      uses_iter_s1 = real_sig;
      SATTR(uses_iter_self1,iters,FSETSI671175003(ATTR(uses_iter_self1,iters), uses_iter_s1));
      functocall = CGEN_e56867199(self, real_sig, ATTR(f,sig1));
      funcalloc = CGEN_e1296302677(self, real_sig, ATTR(f,sig1));
      plus_self27 = ATTR(self,dispatch_c);
      plus_s6 = ((dSTR) ((STR) &name173));
      L62 = (plus_s6==((dSTR) NULL));
      L631_=!(L62); 
      if (L631_) {
       L64 = ATTR(plus_self27,ntext);
       L65 = plus_s6;
       SATTR(plus_self27,ntext,FSTR_p1752847026(L64, (*dSTR_strrSTR[TAG(L65)])(L65)));
      }
      ret_val37 = plus_self27;
      plus_self28 = ret_val37;
      plus_s7 = ((dSTR) cst);
      L66 = (plus_s7==((dSTR) NULL));
      L671_=!(L66); 
      if (L671_) {
       L68 = ATTR(plus_self28,ntext);
       L69 = plus_s7;
       SATTR(plus_self28,ntext,FSTR_p1752847026(L68, (*dSTR_strrSTR[TAG(L69)])(L69)));
      }
      ret_val38 = plus_self28;
      plus_self29 = ret_val38;
      plus_s8 = ((dSTR) functocall);
      L70 = (plus_s8==((dSTR) NULL));
      L711_=!(L70); 
      if (L711_) {
       L72 = ATTR(plus_self29,ntext);
       L73 = plus_s8;
       SATTR(plus_self29,ntext,FSTR_p1752847026(L72, (*dSTR_strrSTR[TAG(L73)])(L73)));
      }
      ret_val39 = plus_self29;
      plus_self30 = ret_val39;
      plus_s9 = ((dSTR) ((STR) &name29));
      L74 = (plus_s9==((dSTR) NULL));
      L751_=!(L74); 
      if (L751_) {
       L76 = ATTR(plus_self30,ntext);
       L77 = plus_s9;
       SATTR(plus_self30,ntext,FSTR_p1752847026(L76, (*dSTR_strrSTR[TAG(L77)])(L77)));
      }
      ret_val40 = plus_self30;
      plus_self31 = ret_val40;
      plus_s10 = ((dSTR) alloc_cast);
      L78 = (plus_s10==((dSTR) NULL));
      L791_=!(L78); 
      if (L791_) {
       L80 = ATTR(plus_self31,ntext);
       L81 = plus_s10;
       SATTR(plus_self31,ntext,FSTR_p1752847026(L80, (*dSTR_strrSTR[TAG(L81)])(L81)));
      }
      ret_val41 = plus_self31;
      plus_self32 = ret_val41;
      plus_s11 = ((dSTR) funcalloc);
      L82 = (plus_s11==((dSTR) NULL));
      L831_=!(L82); 
      if (L831_) {
       L84 = ATTR(plus_self32,ntext);
       L85 = plus_s11;
       SATTR(plus_self32,ntext,FSTR_p1752847026(L84, (*dSTR_strrSTR[TAG(L85)])(L85)));
      }
      ret_val42 = plus_self32;
      plus_self33 = ret_val42;
      plus_s12 = ((dSTR) ((STR) &name4));
      L86 = ATTR(plus_self33,ntext);
      L87 = plus_s12;
      SATTR(plus_self33,ntext,FSTR_p1752847026(L86, (*dSTR_strrSTR[TAG(L87)])(L87)));
      L881_=(i)==(maxtag); 
      L89 = L881_;
      L901_=!(L89); 
      if (L901_) {
       plus_self34 = ATTR(self,dispatch_c);
       plus_s13 = ((dSTR) ((STR) &name3));
       L91 = ATTR(plus_self34,ntext);
       L92 = plus_s13;
       SATTR(plus_self34,ntext,FSTR_p1752847026(L91, (*dSTR_strrSTR[TAG(L92)])(L92)));
      }
      comment_self1 = self;
      comment_f1 = ATTR(self,dispatch_c);
      comment_com1 = ATTR(real_sig,str);
      if (ATTR(comment_self1,pretty)) {
       plus_self35 = comment_f1;
       plus_s14 = ((dSTR) ((STR) &name78));
       L93 = (plus_s14==((dSTR) NULL));
       L941_=!(L93); 
       if (L941_) {
        L95 = ATTR(plus_self35,ntext);
        L96 = plus_s14;
        SATTR(plus_self35,ntext,FSTR_p1752847026(L95, (*dSTR_strrSTR[TAG(L96)])(L96)));
       }
       ret_val43 = plus_self35;
       plus_self36 = ret_val43;
       plus_s15 = ((dSTR) comment_com1);
       L97 = (plus_s15==((dSTR) NULL));
       L981_=!(L97); 
       if (L981_) {
        L99 = ATTR(plus_self36,ntext);
        L100 = plus_s15;
        SATTR(plus_self36,ntext,FSTR_p1752847026(L99, (*dSTR_strrSTR[TAG(L100)])(L100)));
       }
       ret_val44 = plus_self36;
       plus_self37 = ret_val44;
       plus_s16 = ((dSTR) ((STR) &name79));
       L102 = ATTR(plus_self37,ntext);
       L103 = plus_s16;
       SATTR(plus_self37,ntext,FSTR_p1752847026(L102, (*dSTR_strrSTR[TAG(L103)])(L103)));
      }
      plus_self38 = ATTR(self,dispatch_c);
      L105=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
      if (L105==NULL) FATAL("Unable to allocate more memory");
      memset(L105,0,sizeof(struct CHAR_boxed_struct));
      ((OB)L105)->header.tag=CHAR_tag;
      L104 = (dSTR)((CHAR_boxed) L105);
      ((CHAR_boxed) L104)->immutable_part = '\n';
      plus_s17 = L104;
      L106 = ATTR(plus_self38,ntext);
      L107 = plus_s17;
      SATTR(plus_self38,ntext,FSTR_p1752847026(L106, (*dSTR_strrSTR[TAG(L107)])(L107)));
     }
    }
   }
   after_loop2: ;
   L1081_=!(exists); 
   if (L1081_) {
    plus_self39 = ATTR(self,dispatch_c);
    plus_s18 = ((dSTR) ((STR) &NULLNULL1));
    L109 = ATTR(plus_self39,ntext);
    L110 = plus_s18;
    SATTR(plus_self39,ntext,FSTR_p1752847026(L109, (*dSTR_strrSTR[TAG(L110)])(L110)));
    L1111_=(i)==(maxtag); 
    L112 = L1111_;
    L1131_=!(L112); 
    if (L1131_) {
     plus_self40 = ATTR(self,dispatch_c);
     plus_s19 = ((dSTR) ((STR) &name3));
     L114 = ATTR(plus_self40,ntext);
     L115 = plus_s19;
     SATTR(plus_self40,ntext,FSTR_p1752847026(L114, (*dSTR_strrSTR[TAG(L115)])(L115)));
    }
    plus_self41 = ATTR(self,dispatch_c);
    L117=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
    if (L117==NULL) FATAL("Unable to allocate more memory");
    memset(L117,0,sizeof(struct CHAR_boxed_struct));
    ((OB)L117)->header.tag=CHAR_tag;
    L116 = (dSTR)((CHAR_boxed) L117);
    ((CHAR_boxed) L116)->immutable_part = '\n';
    plus_s20 = L116;
    L118 = ATTR(plus_self41,ntext);
    L119 = plus_s20;
    SATTR(plus_self41,ntext,FSTR_p1752847026(L118, (*dSTR_strrSTR[TAG(L119)])(L119)));
   }
  }
 }
 after_loop1: ;
 L1201_=(maxtag)<(mintag); 
 if (L1201_) {
  plus_self42 = ATTR(self,dispatch_c);
  plus_s21 = ((dSTR) ((STR) &NULLNU651700710));
  L122 = ATTR(plus_self42,ntext);
  L123 = plus_s21;
  SATTR(plus_self42,ntext,FSTR_p1752847026(L122, (*dSTR_strrSTR[TAG(L123)])(L123)));
 }
 plus_self43 = ATTR(self,dispatch_c);
 plus_s22 = ((dSTR) ((STR) &name171));
 L124 = (plus_s22==((dSTR) NULL));
 L1251_=!(L124); 
 if (L1251_) {
  L126 = ATTR(plus_self43,ntext);
  L127 = plus_s22;
  SATTR(plus_self43,ntext,FSTR_p1752847026(L126, (*dSTR_strrSTR[TAG(L127)])(L127)));
 }
 ret_val45 = plus_self43;
 plus_self44 = ret_val45;
 plus_s23 = ((dSTR) tbl);
 L128 = (plus_s23==((dSTR) NULL));
 L1291_=!(L128); 
 if (L1291_) {
  L130 = ATTR(plus_self44,ntext);
  L131 = plus_s23;
  SATTR(plus_self44,ntext,FSTR_p1752847026(L130, (*dSTR_strrSTR[TAG(L131)])(L131)));
 }
 ret_val46 = plus_self44;
 plus_self45 = ret_val46;
 plus_s24 = ((dSTR) ((STR) &name9));
 L132 = ATTR(plus_self45,ntext);
 L133 = plus_s24;
 SATTR(plus_self45,ntext,FSTR_p1752847026(L132, (*dSTR_strrSTR[TAG(L133)])(L133)));
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_finalize(CGEN self) {
 BOOL tmp1 = BOOL_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 INT ct = INT_zero;
 INT L31 = INT_zero;
 INT L41 = INT_zero;
 INT i1 = INT_zero;
 INT ct1 = INT_zero;
 STR allflags;
 STR libs;
 FLISTSTR L51;
 STR flag1;
 STR obfiles;
 STR syscom;
 STR other1;
 STR s;
 FSETSTR L61;
 STR s1;
 FSETSTR L71;
 TP_CLASS_TBL L81;
 TP_CLASS cl;
 FSETSTR files11;
 FSETSTR L91;
 STR file1;
 STR s2;
 FLISTSTR L101;
 STR exec_name = ((STR) NULL);
 INT index1 = INT_zero;
 INT suffix_length = INT_zero;
 INT L121 = INT_zero;
 INT L131 = INT_zero;
 INT i2 = INT_zero;
 FSTR comp_opt;
 FLISTSTR L141;
 BFILE make_include_fil;
 FSTR link_opt;
 STR s3;
 FSETSTR L151;
 STR o11;
 INT dummy1 = INT_zero;
 CGEN make_sure_emitte;
 SIG make_sure_emitte1;
 AM_ROUT_DEF ard;
 CODE_FILE plus_self;
 dSTR plus_s;
 FLISTA1893461511 is_empty_self;
 BOOL ret_val = BOOL_zero;
 FLISTA1893461511 is_empty_self1;
 BOOL ret_val1 = BOOL_zero;
 CODE_FILE plus_self1;
 dSTR plus_s1;
 OUT create_self;
 OUT ret_val2;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val3;
 FILE1 stdout_self;
 FILE1 ret_val4;
 FILE1 r;
 OUT plus_self3;
 dSTR plus_s3;
 FILE1 stdout_self1;
 FILE1 ret_val5;
 FILE1 r1;
 OUT create_self1;
 OUT ret_val6;
 OUT plus_self4;
 STR plus_s4;
 OUT ret_val7;
 FILE1 stdout_self2;
 FILE1 ret_val8;
 FILE1 r2;
 OUT plus_self5;
 dSTR plus_s5;
 FILE1 stdout_self3;
 FILE1 ret_val9;
 FILE1 r3;
 OUT create_self2;
 OUT ret_val10;
 OUT plus_self6;
 STR plus_s6;
 OUT ret_val11;
 FILE1 stdout_self4;
 FILE1 ret_val12;
 FILE1 r4;
 OUT plus_self7;
 dSTR plus_s7;
 FILE1 stdout_self5;
 FILE1 ret_val13;
 FILE1 r5;
 OUT create_self3;
 OUT ret_val14;
 OUT plus_self8;
 STR plus_s8;
 OUT ret_val15;
 FILE1 stdout_self6;
 FILE1 ret_val16;
 FILE1 r6;
 OUT plus_self9;
 dSTR plus_s9;
 FILE1 stdout_self7;
 FILE1 ret_val17;
 FILE1 r7;
 OUT create_self4;
 OUT ret_val18;
 OUT plus_self10;
 STR plus_s10;
 OUT ret_val19;
 FILE1 stdout_self8;
 FILE1 ret_val20;
 FILE1 r8;
 OUT plus_self11;
 dSTR plus_s11;
 FILE1 stdout_self9;
 FILE1 ret_val21;
 FILE1 r9;
 OUT create_self5;
 OUT ret_val22;
 OUT plus_self12;
 STR plus_s12;
 OUT ret_val23;
 FILE1 stdout_self10;
 FILE1 ret_val24;
 FILE1 r10;
 OUT plus_self13;
 dSTR plus_s13;
 FILE1 stdout_self11;
 FILE1 ret_val25;
 FILE1 r11;
 OUT create_self6;
 OUT ret_val26;
 OUT plus_self14;
 STR plus_s14;
 OUT ret_val27;
 FILE1 stdout_self12;
 FILE1 ret_val28;
 FILE1 r12;
 OUT plus_self15;
 dSTR plus_s15;
 FILE1 stdout_self13;
 FILE1 ret_val29;
 FILE1 r13;
 OUT create_self7;
 OUT ret_val30;
 OUT plus_self16;
 STR plus_s16;
 OUT ret_val31;
 FILE1 stdout_self14;
 FILE1 ret_val32;
 FILE1 r14;
 OUT plus_self17;
 dSTR plus_s17;
 FILE1 stdout_self15;
 FILE1 ret_val33;
 FILE1 r15;
 OUT create_self8;
 OUT ret_val34;
 OUT plus_self18;
 STR plus_s18;
 OUT ret_val35;
 FILE1 stdout_self16;
 FILE1 ret_val36;
 FILE1 r16;
 OUT plus_self19;
 dSTR plus_s19;
 FILE1 stdout_self17;
 FILE1 ret_val37;
 FILE1 r17;
 OUT create_self9;
 OUT ret_val38;
 OUT plus_self20;
 STR plus_s20;
 OUT ret_val39;
 FILE1 stdout_self18;
 FILE1 ret_val40;
 FILE1 r18;
 OUT plus_self21;
 dSTR plus_s21;
 FILE1 stdout_self19;
 FILE1 ret_val41;
 FILE1 r19;
 OUT create_self10;
 OUT ret_val42;
 OUT plus_self22;
 STR plus_s22;
 OUT ret_val43;
 FILE1 stdout_self20;
 FILE1 ret_val44;
 FILE1 r20;
 OUT plus_self23;
 dSTR plus_s23;
 FILE1 stdout_self21;
 FILE1 ret_val45;
 FILE1 r21;
 OUT create_self11;
 OUT ret_val46;
 OUT plus_self24;
 STR plus_s24;
 OUT ret_val47;
 FILE1 stdout_self22;
 FILE1 ret_val48;
 FILE1 r22;
 OUT plus_self25;
 dSTR plus_s25;
 FILE1 stdout_self23;
 FILE1 ret_val49;
 FILE1 r23;
 OUT create_self12;
 OUT ret_val50;
 OUT plus_self26;
 STR plus_s26;
 OUT ret_val51;
 FILE1 stdout_self24;
 FILE1 ret_val52;
 FILE1 r24;
 OUT plus_self27;
 dSTR plus_s27;
 FILE1 stdout_self25;
 FILE1 ret_val53;
 FILE1 r25;
 OUT create_self13;
 OUT ret_val54;
 OUT plus_self28;
 STR plus_s28;
 OUT ret_val55;
 FILE1 stdout_self26;
 FILE1 ret_val56;
 FILE1 r26;
 OUT plus_self29;
 dSTR plus_s29;
 FILE1 stdout_self27;
 FILE1 ret_val57;
 FILE1 r27;
 OUT create_self14;
 OUT ret_val58;
 OUT plus_self30;
 STR plus_s30;
 OUT ret_val59;
 FILE1 stdout_self28;
 FILE1 ret_val60;
 FILE1 r28;
 OUT plus_self31;
 dSTR plus_s31;
 FILE1 stdout_self29;
 FILE1 ret_val61;
 FILE1 r29;
 OUT create_self15;
 OUT ret_val62;
 OUT plus_self32;
 STR plus_s32;
 OUT ret_val63;
 FILE1 stdout_self30;
 FILE1 ret_val64;
 FILE1 r30;
 OUT plus_self33;
 dSTR plus_s33;
 FILE1 stdout_self31;
 FILE1 ret_val65;
 FILE1 r31;
 OUT create_self16;
 OUT ret_val66;
 OUT plus_self34;
 STR plus_s34;
 OUT ret_val67;
 FILE1 stdout_self32;
 FILE1 ret_val68;
 FILE1 r32;
 OUT plus_self35;
 dSTR plus_s35;
 OUT ret_val69;
 FILE1 stdout_self33;
 FILE1 ret_val70;
 FILE1 r33;
 OUT plus_self36;
 STR plus_s36;
 FILE1 stdout_self34;
 FILE1 ret_val71;
 FILE1 r34;
 OUT create_self17;
 OUT ret_val72;
 OUT plus_self37;
 STR plus_s37;
 FILE1 stdout_self35;
 FILE1 ret_val73;
 FILE1 r35;
 OUT create_self18;
 OUT ret_val74;
 OUT plus_self38;
 STR plus_s38;
 FILE1 stdout_self36;
 FILE1 ret_val75;
 FILE1 r36;
 OUT create_self19;
 OUT ret_val76;
 OUT plus_self39;
 dSTR plus_s39;
 OUT ret_val77;
 FILE1 stdout_self37;
 FILE1 ret_val78;
 FILE1 r37;
 OUT plus_self40;
 dSTR plus_s40;
 OUT ret_val79;
 FILE1 stdout_self38;
 FILE1 ret_val80;
 FILE1 r38;
 OUT plus_self41;
 dSTR plus_s41;
 OUT ret_val81;
 FILE1 stdout_self39;
 FILE1 ret_val82;
 FILE1 r39;
 OUT plus_self42;
 dSTR plus_s42;
 OUT ret_val83;
 FILE1 stdout_self40;
 FILE1 ret_val84;
 FILE1 r40;
 OUT plus_self43;
 dSTR plus_s43;
 FILE1 stdout_self41;
 FILE1 ret_val85;
 FILE1 r41;
 OUT create_self20;
 OUT ret_val86;
 OUT plus_self44;
 STR plus_s44;
 FILE1 stdout_self42;
 FILE1 ret_val87;
 FILE1 r42;
 OUT create_self21;
 OUT ret_val88;
 OUT plus_self45;
 dSTR plus_s45;
 OUT ret_val89;
 FILE1 stdout_self43;
 FILE1 ret_val90;
 FILE1 r43;
 OUT plus_self46;
 dSTR plus_s46;
 OUT ret_val91;
 FILE1 stdout_self44;
 FILE1 ret_val92;
 FILE1 r44;
 OUT plus_self47;
 dSTR plus_s47;
 OUT ret_val93;
 FILE1 stdout_self45;
 FILE1 ret_val94;
 FILE1 r45;
 OUT plus_self48;
 dSTR plus_s48;
 OUT ret_val95;
 FILE1 stdout_self46;
 FILE1 ret_val96;
 FILE1 r46;
 OUT plus_self49;
 dSTR plus_s49;
 FILE1 stdout_self47;
 FILE1 ret_val97;
 FILE1 r47;
 STR plus_self50;
 STR plus_sarg;
 STR ret_val98;
 STR plus_self51;
 CHAR plus_arg = CHAR_zero;
 STR ret_val99;
 STR s4;
 CHAR str_self = CHAR_zero;
 STR ret_val100;
 STR create_self22;
 CHAR create_c = CHAR_zero;
 STR ret_val101;
 STR r48;
 STR plus_self52;
 STR plus_sarg1;
 STR ret_val102;
 STR plus_self53;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val103;
 STR s5;
 CHAR str_self1 = CHAR_zero;
 STR ret_val104;
 STR create_self23;
 CHAR create_c1 = CHAR_zero;
 STR ret_val105;
 STR r49;
 STR plus_self54;
 STR plus_sarg2;
 STR ret_val106;
 STR plus_self55;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val107;
 STR s6;
 CHAR str_self2 = CHAR_zero;
 STR ret_val108;
 STR create_self24;
 CHAR create_c2 = CHAR_zero;
 STR ret_val109;
 STR r50;
 STR plus_self56;
 STR plus_sarg3;
 STR ret_val110;
 STR plus_self57;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val1111;
 STR s7;
 CHAR str_self3 = CHAR_zero;
 STR ret_val112;
 STR create_self25;
 CHAR create_c3 = CHAR_zero;
 STR ret_val113;
 STR r51;
 STR plus_self58;
 STR plus_sarg4;
 STR ret_val114;
 STR plus_self59;
 CHAR plus_arg4 = CHAR_zero;
 STR ret_val115;
 STR s8;
 CHAR str_self4 = CHAR_zero;
 STR ret_val116;
 STR create_self26;
 CHAR create_c4 = CHAR_zero;
 STR ret_val117;
 STR r52;
 STR plus_self60;
 STR plus_sarg5;
 STR ret_val118;
 STR plus_self61;
 CHAR plus_arg5 = CHAR_zero;
 STR ret_val119;
 STR s9;
 CHAR str_self5 = CHAR_zero;
 STR ret_val120;
 STR create_self27;
 CHAR create_c5 = CHAR_zero;
 STR ret_val121;
 STR r53;
 STR plus_self62;
 STR plus_sarg6;
 STR ret_val122;
 STR plus_self63;
 CHAR plus_arg6 = CHAR_zero;
 STR ret_val123;
 STR s10;
 CHAR str_self6 = CHAR_zero;
 STR ret_val124;
 STR create_self28;
 CHAR create_c6 = CHAR_zero;
 STR ret_val125;
 STR r54;
 STR plus_self64;
 STR plus_sarg7;
 STR ret_val126;
 STR plus_self65;
 CHAR plus_arg7 = CHAR_zero;
 STR ret_val127;
 STR s11;
 CHAR str_self7 = CHAR_zero;
 STR ret_val128;
 STR create_self29;
 CHAR create_c7 = CHAR_zero;
 STR ret_val129;
 STR r55;
 STR plus_self66;
 STR plus_sarg8;
 STR ret_val130;
 STR plus_self67;
 CHAR plus_arg8 = CHAR_zero;
 STR ret_val131;
 STR s12;
 CHAR str_self8 = CHAR_zero;
 STR ret_val132;
 STR create_self30;
 CHAR create_c8 = CHAR_zero;
 STR ret_val133;
 STR r56;
 STR plus_self68;
 STR plus_sarg9;
 STR ret_val134;
 STR plus_self69;
 CHAR plus_arg9 = CHAR_zero;
 STR ret_val135;
 STR s13;
 CHAR str_self9 = CHAR_zero;
 STR ret_val136;
 STR create_self31;
 CHAR create_c9 = CHAR_zero;
 STR ret_val137;
 STR r57;
 STR plus_self70;
 STR plus_sarg10;
 STR ret_val138;
 PROG psather_self;
 BOOL ret_val139 = BOOL_zero;
 STR plus_self71;
 STR plus_sarg11;
 STR ret_val140;
 STR plus_self72;
 STR plus_sarg12;
 STR ret_val141;
 STR plus_self73;
 STR plus_sarg13;
 STR ret_val142;
 STR plus_self74;
 STR plus_sarg14;
 STR ret_val143;
 STR plus_self75;
 STR plus_sarg15;
 STR ret_val144;
 STR replace_suffix_s;
 STR replace_suffix_o;
 STR replace_suffix_r;
 STR ret_val145;
 STR plus_self76;
 STR plus_sarg16;
 STR ret_val146;
 STR plus_self77;
 CHAR plus_arg10 = CHAR_zero;
 STR ret_val147;
 STR s14;
 CHAR str_self10 = CHAR_zero;
 STR ret_val148;
 STR create_self32;
 CHAR create_c10 = CHAR_zero;
 STR ret_val149;
 STR r58;
 STR plus_self78;
 STR plus_sarg17;
 STR ret_val150;
 STR plus_self79;
 CHAR plus_arg11 = CHAR_zero;
 STR ret_val151;
 STR s15;
 CHAR str_self11 = CHAR_zero;
 STR ret_val152;
 STR create_self33;
 CHAR create_c11 = CHAR_zero;
 STR ret_val153;
 STR r59;
 STR plus_self80;
 STR plus_sarg18;
 STR ret_val154;
 STR plus_self81;
 STR plus_sarg19;
 STR ret_val155;
 STR plus_self82;
 CHAR plus_arg12 = CHAR_zero;
 STR ret_val156;
 STR s16;
 CHAR str_self12 = CHAR_zero;
 STR ret_val157;
 STR create_self34;
 CHAR create_c12 = CHAR_zero;
 STR ret_val158;
 STR r60;
 STR plus_self83;
 STR plus_sarg20;
 STR ret_val159;
 STR plus_self84;
 STR plus_sarg21;
 STR ret_val160;
 OUT create_self35;
 OUT ret_val161;
 OUT plus_self85;
 STR plus_s50;
 OUT ret_val162;
 FILE1 stdout_self48;
 FILE1 ret_val163;
 FILE1 r61;
 OUT plus_self86;
 STR plus_s51;
 OUT ret_val164;
 FILE1 stdout_self49;
 FILE1 ret_val165;
 FILE1 r62;
 OUT plus_self87;
 STR plus_s52;
 OUT ret_val166;
 FILE1 stdout_self50;
 FILE1 ret_val167;
 FILE1 r63;
 OUT plus_self88;
 dSTR plus_s53;
 OUT ret_val168;
 FILE1 stdout_self51;
 FILE1 ret_val169;
 FILE1 r64;
 OUT plus_self89;
 STR plus_s54;
 FILE1 stdout_self52;
 FILE1 ret_val170;
 FILE1 r65;
 STR plus_self90;
 CHAR plus_arg13 = CHAR_zero;
 STR ret_val171;
 STR s17;
 CHAR str_self13 = CHAR_zero;
 STR ret_val172;
 STR create_self36;
 CHAR create_c13 = CHAR_zero;
 STR ret_val173;
 STR r66;
 STR plus_self91;
 STR plus_sarg22;
 STR ret_val174;
 STR plus_self92;
 CHAR plus_arg14 = CHAR_zero;
 STR ret_val175;
 STR s18;
 CHAR str_self14 = CHAR_zero;
 STR ret_val176;
 STR create_self37;
 CHAR create_c14 = CHAR_zero;
 STR ret_val177;
 STR r67;
 STR plus_self93;
 STR plus_sarg23;
 STR ret_val178;
 STR plus_self94;
 STR plus_sarg24;
 STR ret_val179;
 STR plus_self95;
 STR plus_sarg25;
 STR ret_val180;
 STR plus_self96;
 CHAR plus_arg15 = CHAR_zero;
 STR ret_val181;
 STR s19;
 CHAR str_self15 = CHAR_zero;
 STR ret_val182;
 STR create_self38;
 CHAR create_c15 = CHAR_zero;
 STR ret_val183;
 STR r68;
 STR plus_self97;
 STR plus_sarg26;
 STR ret_val184;
 STR plus_self98;
 STR plus_sarg27;
 STR ret_val185;
 STR plus_self99;
 STR plus_sarg28;
 STR ret_val186;
 STR plus_self100;
 STR plus_sarg29;
 STR ret_val187;
 STR plus_self101;
 STR plus_sarg30;
 STR ret_val188;
 FSTR create_self39;
 FSTR ret_val189;
 STR plus_self102;
 STR plus_sarg31;
 STR ret_val190;
 BFILE open_for_read_se;
 STR open_for_read_nm;
 BFILE ret_val191;
 BFILE r69;
 BFILE error_self;
 BOOL ret_val192 = BOOL_zero;
 BFILE close_self;
 FSTR plus_self103;
 CHAR plus_c = CHAR_zero;
 FSTR ret_val193;
 FSTR plus_self104;
 CHAR plus_c1 = CHAR_zero;
 FSTR ret_val194;
 FSTR plus_self105;
 CHAR plus_c2 = CHAR_zero;
 FSTR ret_val195;
 CODE_FILE plus_self106;
 dSTR plus_s55;
 FSTR create_self40;
 FSTR ret_val196;
 FSTR plus_self107;
 CHAR plus_c3 = CHAR_zero;
 FSTR ret_val197;
 FSTR plus_self108;
 CHAR plus_c4 = CHAR_zero;
 FSTR ret_val198;
 FSTR plus_self109;
 CHAR plus_c5 = CHAR_zero;
 FSTR ret_val199;
 FSTR plus_self110;
 CHAR plus_c6 = CHAR_zero;
 FSTR ret_val200;
 CODE_FILE plus_self111;
 dSTR plus_s56;
 CODE_FILE plus_self112;
 dSTR plus_s57;
 CODE_FILE plus_self113;
 dSTR plus_s58;
 CODE_FILE plus_self114;
 dSTR plus_s59;
 CODE_FILE ret_val201;
 CODE_FILE plus_self115;
 dSTR plus_s60;
 CODE_FILE ret_val202;
 CODE_FILE plus_self116;
 dSTR plus_s61;
 CODE_FILE plus_self117;
 dSTR plus_s62;
 CODE_FILE ret_val203;
 CODE_FILE plus_self118;
 dSTR plus_s63;
 CODE_FILE ret_val204;
 CODE_FILE plus_self119;
 dSTR plus_s64;
 CODE_FILE plus_self120;
 dSTR plus_s65;
 STR replace_suffix_s1;
 STR replace_suffix_o1;
 STR replace_suffix_r1;
 STR ret_val205;
 STR plus_self121;
 STR plus_sarg32;
 STR ret_val206;
 STR plus_self122;
 STR plus_sarg33;
 STR ret_val207;
 CODE_FILE plus_self123;
 dSTR plus_s66;
 CODE_FILE ret_val208;
 CODE_FILE plus_self124;
 dSTR plus_s67;
 CODE_FILE ret_val209;
 CODE_FILE plus_self125;
 dSTR plus_s68;
 CODE_FILE ret_val210;
 CODE_FILE plus_self126;
 dSTR plus_s69;
 CODE_FILE ret_val211;
 CODE_FILE plus_self127;
 dSTR plus_s70;
 CODE_FILE ret_val212;
 CODE_FILE plus_self128;
 dSTR plus_s71;
 CODE_FILE ret_val213;
 CODE_FILE plus_self129;
 dSTR plus_s72;
 STR plus_self130;
 STR plus_sarg34;
 STR ret_val214;
 STR plus_self131;
 STR plus_sarg35;
 STR ret_val215;
 STR plus_self132;
 STR plus_sarg36;
 STR ret_val216;
 STR plus_self133;
 STR plus_sarg37;
 STR ret_val217;
 STR plus_self134;
 CHAR plus_arg16 = CHAR_zero;
 STR ret_val218;
 STR s20;
 CHAR str_self16 = CHAR_zero;
 STR ret_val219;
 STR create_self41;
 CHAR create_c16 = CHAR_zero;
 STR ret_val220;
 STR r70;
 STR plus_self135;
 STR plus_sarg38;
 STR ret_val221;
 STR plus_self136;
 CHAR plus_arg17 = CHAR_zero;
 STR ret_val222;
 STR s21;
 CHAR str_self17 = CHAR_zero;
 STR ret_val223;
 STR create_self42;
 CHAR create_c17 = CHAR_zero;
 STR ret_val224;
 STR r71;
 STR plus_self137;
 STR plus_sarg39;
 STR ret_val225;
 OUT create_self43;
 OUT ret_val226;
 OUT plus_self138;
 STR plus_s73;
 FILE1 stdout_self53;
 FILE1 ret_val227;
 FILE1 r72;
 OUT create_self44;
 OUT ret_val228;
 OUT plus_self139;
 dSTR plus_s74;
 OUT ret_val229;
 FILE1 stdout_self54;
 FILE1 ret_val230;
 FILE1 r73;
 OUT plus_self140;
 STR plus_s75;
 OUT ret_val231;
 FILE1 stdout_self55;
 FILE1 ret_val232;
 FILE1 r74;
 OUT plus_self141;
 dSTR plus_s76;
 FILE1 stdout_self56;
 FILE1 ret_val233;
 FILE1 r75;
 UNIX system_self;
 STR system_s;
 INT ret_val234 = INT_zero;
 CGEN barf_self;
 STR barf_msg;
 PROG barf_self1;
 STR barf_msg1;
 PROG barf_at_self;
 STR barf_at_msg;
 dPROG_ERR barf_at_at;
 STR plus_self142;
 STR plus_sarg40;
 STR ret_val235;
 UNIX exit_self;
 INT exit_code = INT_zero;
 STR plus_self143;
 STR plus_sarg41;
 STR ret_val236;
 UNIX system_self1;
 STR system_s1;
 INT ret_val237 = INT_zero;
 BOOL L16;
 BOOL L171_;
 BOOL L18;
 extern STR defineFFpFF;
 FSTR L19;
 dSTR L20;
 BOOL L22;
 BOOL L23;
 INT L24;
 BOOL L251_;
 INT L26;
 BOOL L271_;
 INT L28;
 BOOL L291_;
 BOOL L30;
 extern STR undefFF;
 FSTR L32;
 dSTR L33;
 extern STR Abstractcalls;
 FILE1 L34;
 OB L35;
 dSTR L37;
 OB L38;
 FILE1 L39;
 OB L40;
 extern STR Concretecalls;
 FILE1 L43;
 OB L44;
 dSTR L46;
 OB L47;
 FILE1 L48;
 OB L49;
 extern STR Inline469134742;
 FILE1 L52;
 OB L53;
 dSTR L55;
 OB L56;
 FILE1 L57;
 OB L58;
 extern STR Inline1184875661;
 FILE1 L60;
 OB L62;
 dSTR L64;
 OB L65;
 FILE1 L66;
 OB L67;
 extern STR Marked1264672611;
 FILE1 L69;
 OB L70;
 dSTR L73;
 OB L74;
 FILE1 L75;
 OB L76;
 extern STR Inline1346595549;
 FILE1 L78;
 OB L79;
 dSTR L82;
 OB L83;
 FILE1 L84;
 OB L85;
 extern STR Marked887950191;
 FILE1 L87;
 OB L88;
 dSTR L90;
 OB L92;
 FILE1 L93;
 OB L94;
 extern STR Inline778953810;
 FILE1 L96;
 OB L97;
 dSTR L99;
 OB L100;
 FILE1 L102;
 OB L103;
 extern STR Marked2085347308;
 FILE1 L105;
 OB L106;
 dSTR L108;
 OB L109;
 FILE1 L110;
 OB L111;
 extern STR Inline311198520;
 FILE1 L113;
 OB L114;
 dSTR L116;
 OB L117;
 FILE1 L118;
 OB L119;
 extern STR MarkedINTrouts;
 FILE1 L122;
 OB L123;
 dSTR L125;
 OB L126;
 FILE1 L127;
 OB L128;
 extern STR FoldedINTcalls;
 FILE1 L130;
 OB L132;
 dSTR L134;
 OB L135;
 FILE1 L136;
 OB L137;
 extern STR Markedshortrouts;
 FILE1 L139;
 OB L140;
 dSTR L143;
 OB L144;
 FILE1 L145;
 OB L146;
 extern STR Inline1829321774;
 FILE1 L148;
 OB L149;
 dSTR L152;
 OB L153;
 FILE1 L154;
 OB L155;
 extern STR Markedflatiters;
 FILE1 L157;
 OB L158;
 dSTR L160;
 OB L161;
 FILE1 L162;
 OB L163;
 extern STR Inline453141559;
 FILE1 L165;
 OB L166;
 dSTR L168;
 OB L169;
 FILE1 L170;
 OB L172;
 extern STR Inline1146142148;
 FILE1 L174;
 OB L175;
 dSTR L177;
 OB L178;
 FILE1 L179;
 OB L180;
 extern STR name163;
 FILE1 L182;
 OB L183;
 BOOL L185;
 BOOL L1861_;
 extern STR Thefol1627053373;
 FILE1 L187;
 OB L188;
 extern STR ItersItersloops;
 FILE1 L190;
 OB L191;
 INT L1931_,L1931_m;
 INT L194;
 ARRAYINT L195;
 INT L1961_;
 BOOL L1971_;
 dSTR L198;
 OB L199;
 FILE1 L200;
 OB L201;
 dSTR L203;
 OB L204;
 FILE1 L205;
 OB L206;
 dSTR L208;
 OB L209;
 FILE1 L210;
 OB L211;
 dSTR L213;
 OB L214;
 FILE1 L215;
 OB L216;
 dSTR L218;
 OB L219;
 FILE1 L220;
 OB L221;
 extern STR HotsItersloops;
 FILE1 L223;
 OB L224;
 INT L2261_,L2261_m;
 INT L227;
 ARRAYINT L228;
 INT L2291_;
 BOOL L2301_;
 dSTR L231;
 OB L232;
 FILE1 L233;
 OB L234;
 dSTR L236;
 OB L237;
 FILE1 L238;
 OB L239;
 dSTR L241;
 OB L242;
 FILE1 L243;
 OB L244;
 dSTR L246;
 OB L247;
 FILE1 L248;
 OB L249;
 dSTR L252;
 OB L253;
 FILE1 L254;
 OB L255;
 extern STR CC_OPTIONS;
 extern STR name1;
 extern STR I;
 STR L257;
 INT L258;
 OB L259;
 INT L260;
 extern STR CC_DEBUG_FLAG;
 STR L262;
 INT L263;
 OB L264;
 INT L265;
 extern STR CC_DEBUG_FLAG;
 STR L267;
 INT L268;
 OB L269;
 INT L270;
 extern STR CC_OPTIMIZE_FLAG;
 STR L273;
 INT L274;
 OB L275;
 INT L276;
 extern STR CC_OPTIMIZE_FLAG;
 STR L278;
 INT L279;
 OB L280;
 INT L281;
 extern STR CC_PROLIX_FLAG;
 STR L283;
 INT L284;
 OB L285;
 INT L286;
 extern STR CC_PROLIX_FLAG;
 STR L288;
 BOOL L289;
 INT L290;
 BOOL L2921_;
 STR L293;
 extern STR l111;
 STR L294;
 INT L295;
 OB L296;
 INT L297;
 STR L299;
 INT L300;
 OB L301;
 INT L302;
 STR L304;
 INT L305;
 OB L306;
 INT L307;
 extern STR GC_LINK;
 STR L309;
 INT L310;
 OB L311;
 INT L312;
 extern STR LINK_OPTIONS;
 BOOL L314;
 extern STR DPSATHER_CHK;
 extern STR DPSATH545432352;
 extern STR DPSATHER_TRACE;
 extern STR dCCdCFLAGS;
 extern STR EXEC_OPTION;
 extern STR dCSdOBJ;
 extern STR name1;
 STR L315;
 extern STR C_EXT;
 extern STR OBJECT_EXT;
 INT L316;
 INT L317;
 INT L3181_;
 STR L319;
 INT L320;
 OB L321;
 INT L322;
 INT L324;
 INT L3251_;
 STR L326;
 CHAR L3271_;
 CHAR L328;
 BOOL L3291_;
 STR L330;
 INT L331;
 OB L332;
 INT L333;
 CHAR L3351_;
 CHAR L336;
 BOOL L3371_;
 STR L338;
 INT L339;
 OB L340;
 INT L341;
 extern STR name46;
 TP_CLASS L343;
 FMAPSTRFSETSTR L344;
 BOOL L345;
 BOOL L3461_;
 STR L347;
 extern STR Including1;
 FILE1 L348;
 OB L349;
 FILE1 L351;
 OB L352;
 extern STR name189;
 FILE1 L354;
 OB L355;
 FILE1 L357;
 OB L358;
 extern STR wasreached;
 FILE1 L360;
 OB L362;
 CHAR L3641_;
 CHAR L365;
 BOOL L3661_;
 STR L367;
 INT L368;
 OB L369;
 INT L370;
 CHAR L3721_;
 CHAR L373;
 BOOL L3741_;
 STR L375;
 INT L376;
 OB L377;
 INT L378;
 extern STR name190;
 STR L380;
 CHAR L3811_;
 CHAR L382;
 BOOL L3831_;
 STR L384;
 INT L385;
 OB L386;
 INT L387;
 extern STR name190;
 extern STR dOTHER;
 extern STR dLIBS;
 INT L390;
 INT L3911_;
 INT L3891_,L3891_m;
 INT L392;
 STR L393;
 CHAR L3941_;
 CHAR L395;
 BOOL L3961_;
 INT L3971_;
 FSTR L398;
 INT L399;
 OB L400;
 INT L401;
 STR L402;
 extern STR Maketargetinc;
 BFILE L403;
 OB L404;
 extern STR rb;
 STR L405;
 STR L406;
 BOOL L407;
 BOOL L408;
 BOOL L4091_;
 BOOL L410;
 BOOL L411;
 BOOL L4121_;
 FSTR L413;
 extern STR name163;
 extern STR SHOME;
 FSTR L414;
 extern STR CFLAGS;
 FSTR L415;
 extern STR CC;
 FSTR L416;
 extern STR C_COMPILER;
 extern STR HDRsatherhtagsh;
 FSTR L417;
 dSTR L418;
 FSTR L419;
 INT L420;
 OB L422;
 INT L423;
 FSTR L424;
 extern STR LIBS;
 FSTR L425;
 extern STR CS1;
 FSTR L426;
 FSTR L427;
 extern STR EXEC_SUFFIX;
 extern STR OBJ;
 FSTR L428;
 extern STR OTHER;
 FSTR L429;
 FSTR L430;
 dSTR L431;
 FSTR L432;
 dSTR L433;
 extern STR dCSLIN1806998169;
 FSTR L434;
 dSTR L435;
 extern STR name37;
 BOOL L436;
 BOOL L4371_;
 FSTR L438;
 dSTR L439;
 BOOL L440;
 BOOL L4411_;
 FSTR L442;
 dSTR L443;
 extern STR name163;
 FSTR L444;
 dSTR L445;
 extern STR syscom1;
 BOOL L446;
 BOOL L4471_;
 FSTR L448;
 dSTR L449;
 BOOL L450;
 BOOL L4521_;
 FSTR L453;
 dSTR L454;
 dSTR L455;
 OB L456;
 FSTR L457;
 dSTR L458;
 extern STR dOBJCO818039632;
 FSTR L459;
 dSTR L460;
 STR L461;
 extern STR C_EXT;
 extern STR OBJECT_EXT;
 INT L462;
 INT L463;
 INT L4641_;
 INT L465;
 INT L4661_;
 CHAR L4671_;
 CHAR L468;
 BOOL L4691_;
 BOOL L470;
 BOOL L4711_;
 extern STR name46;
 extern STR name9;
 BOOL L472;
 BOOL L4731_;
 FSTR L474;
 dSTR L475;
 BOOL L476;
 BOOL L4771_;
 FSTR L478;
 dSTR L479;
 extern STR name20;
 BOOL L480;
 BOOL L4811_;
 FSTR L482;
 dSTR L483;
 BOOL L484;
 BOOL L4851_;
 FSTR L486;
 dSTR L487;
 extern STR COMPIL44583114;
 BOOL L488;
 BOOL L4891_;
 FSTR L490;
 dSTR L491;
 BOOL L492;
 BOOL L4931_;
 FSTR L494;
 dSTR L495;
 extern STR name9;
 FSTR L496;
 dSTR L497;
 extern STR cd;
 extern STR SHELL_SEP;
 extern STR name10;
 extern STR MAKE_COMMAND;
 STR L498;
 INT L499;
 OB L500;
 INT L502;
 extern STR MAKE_V183621898;
 STR L504;
 INT L505;
 OB L506;
 INT L507;
 extern STR MAKE_V183621898;
 BOOL L509;
 extern STR Skippingmake;
 FILE1 L510;
 OB L511;
 dSTR L513;
 OB L514;
 FILE1 L515;
 OB L516;
 FILE1 L518;
 OB L519;
 dSTR L521;
 OB L522;
 FILE1 L523;
 OB L524;
 STR L526;
 INT L527;
 BOOL L5281_;
 BOOL L529;
 BOOL L5301_;
 extern STR Makefailed;
 extern STR Intern28965746;
 BOOL L531;
 BOOL L532;
 BOOL L5331_;
 BOOL L534;
 BOOL L5351_;
 extern STR rmfr;
 STR L536;
 BOOL L537;
 BOOL L538;
 BOOL L5391_;
 BOOL L540;
 BOOL L5421_;
 if (ATTR(self,only_check)) {
  return;
 }
 make_sure_emitte = self;
 make_sure_emitte1 = ATTR(self,main_sig);
 ard = FMAPSI2016001247(ATTR(make_sure_emitte,not_emitted), make_sure_emitte1);
 L16 = (ard==((AM_ROUT_DEF) NULL));
 L171_=!(L16); 
 if (L171_) {
  SATTR(make_sure_emitte,leftovers,FSETAM2044524049(ATTR(make_sure_emitte,leftovers), ard));
  SATTR(make_sure_emitte,not_emitted,FMAPSI1625125906(ATTR(make_sure_emitte,not_emitted), make_sure_emitte1));
 }
 CGEN_d498181638(self);
 CGEN_m875729145(self);
 if (ATTR(self,print_ob)) {
  L18 = TRUE;
 } else {
  L18 = ATTR(self,print_ob_g);
 }
 if (L18) {
  plus_self = ATTR(self,system_c);
  plus_s = ((dSTR) ((STR) &defineFFpFF));
  L19 = ATTR(plus_self,ntext);
  L20 = plus_s;
  SATTR(plus_self,ntext,FSTR_p1752847026(L19, (*dSTR_strrSTR[TAG(L20)])(L20)));
 }
 while (1) {
  CGEN_g1629839129(self);
  CGEN_g637688959(self);
  CGEN_g333195909(self);
  CGEN_g1348388497(self);
  L24 = FSETAM1999114804(ATTR(self,leftovers));
  L251_=(L24)==(0); 
  if (L251_) {
   is_empty_self = ATTR(self,bnd_rout_creates);
   L26 = FLISTA919540447(is_empty_self);
   L271_=(L26)==(0); 
   ret_val = L271_;
   L23 = ret_val;
  } else {
   L23 = FALSE;
  }
  if (L23) {
   is_empty_self1 = ATTR(self,bnd_iter_creates);
   L28 = FLISTA919540447(is_empty_self1);
   L291_=(L28)==(0); 
   ret_val1 = L291_;
   L22 = ret_val1;
  } else {
   L22 = FALSE;
  }
  if (L22) {
   goto after_loop;
  }
 }
 after_loop: ;
 if (ATTR(self,print_ob)) {
  L30 = TRUE;
 } else {
  L30 = ATTR(self,print_ob_g);
 }
 if (L30) {
  plus_self1 = ATTR(self,system_c);
  plus_s1 = ((dSTR) ((STR) &undefFF));
  L32 = ATTR(plus_self1,ntext);
  L33 = plus_s1;
  SATTR(plus_self1,ntext,FSTR_p1752847026(L32, (*dSTR_strrSTR[TAG(L33)])(L33)));
 }
 CGEN_g1874773404(self);
 tmp1 = CGEN_a952489530(self);
 CGEN_g1356172775(self);
 if (ATTR(ATTR(self,prog),prolix)) {
  create_self = ((OUT) NULL);
  ret_val2 = create_self;
  plus_self2 = ret_val2;
  plus_s2 = ((STR) &Abstractcalls);
  stdout_self = ((FILE1) NULL);
  L35=ZALLOC(sizeof(struct FILE1_struct));
  if (L35==NULL) FATAL("Unable to allocate more memory");
  ((OB)L35)->header.tag=FILE1_tag;
  L34 = ((FILE1) L35);
  r = L34;
  
  SATTR(r,fp,stdout);
  ret_val4 = r;
  FILE_plus_STR(ret_val4, plus_s2);
  ret_val3 = plus_self2;
  plus_self3 = ret_val3;
  L38=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L38==NULL) FATAL("Unable to allocate more memory");
  memset(L38,0,sizeof(struct INT_boxed_struct));
  ((OB)L38)->header.tag=INT_tag;
  L37 = (dSTR)((INT_boxed) L38);
  ((INT_boxed) L37)->immutable_part = ATTR(self,abstract_calls);
  plus_s3 = L37;
  stdout_self1 = ((FILE1) NULL);
  L40=ZALLOC(sizeof(struct FILE1_struct));
  if (L40==NULL) FATAL("Unable to allocate more memory");
  ((OB)L40)->header.tag=FILE1_tag;
  L39 = ((FILE1) L40);
  r1 = L39;
  
  SATTR(r1,fp,stdout);
  ret_val5 = r1;
  FILE_plus_dSTR(ret_val5, plus_s3);
  create_self1 = ((OUT) NULL);
  ret_val6 = create_self1;
  plus_self4 = ret_val6;
  plus_s4 = ((STR) &Concretecalls);
  stdout_self2 = ((FILE1) NULL);
  L44=ZALLOC(sizeof(struct FILE1_struct));
  if (L44==NULL) FATAL("Unable to allocate more memory");
  ((OB)L44)->header.tag=FILE1_tag;
  L43 = ((FILE1) L44);
  r2 = L43;
  
  SATTR(r2,fp,stdout);
  ret_val8 = r2;
  FILE_plus_STR(ret_val8, plus_s4);
  ret_val7 = plus_self4;
  plus_self5 = ret_val7;
  L47=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L47==NULL) FATAL("Unable to allocate more memory");
  memset(L47,0,sizeof(struct INT_boxed_struct));
  ((OB)L47)->header.tag=INT_tag;
  L46 = (dSTR)((INT_boxed) L47);
  ((INT_boxed) L46)->immutable_part = ATTR(self,concrete_calls);
  plus_s5 = L46;
  stdout_self3 = ((FILE1) NULL);
  L49=ZALLOC(sizeof(struct FILE1_struct));
  if (L49==NULL) FATAL("Unable to allocate more memory");
  ((OB)L49)->header.tag=FILE1_tag;
  L48 = ((FILE1) L49);
  r3 = L48;
  
  SATTR(r3,fp,stdout);
  ret_val9 = r3;
  FILE_plus_dSTR(ret_val9, plus_s5);
  create_self2 = ((OUT) NULL);
  ret_val10 = create_self2;
  plus_self6 = ret_val10;
  plus_s6 = ((STR) &Inline469134742);
  stdout_self4 = ((FILE1) NULL);
  L53=ZALLOC(sizeof(struct FILE1_struct));
  if (L53==NULL) FATAL("Unable to allocate more memory");
  ((OB)L53)->header.tag=FILE1_tag;
  L52 = ((FILE1) L53);
  r4 = L52;
  
  SATTR(r4,fp,stdout);
  ret_val12 = r4;
  FILE_plus_STR(ret_val12, plus_s6);
  ret_val11 = plus_self6;
  plus_self7 = ret_val11;
  L56=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L56==NULL) FATAL("Unable to allocate more memory");
  memset(L56,0,sizeof(struct INT_boxed_struct));
  ((OB)L56)->header.tag=INT_tag;
  L55 = (dSTR)((INT_boxed) L56);
  ((INT_boxed) L55)->immutable_part = ATTR(self,inlined_value_ob);
  plus_s7 = L55;
  stdout_self5 = ((FILE1) NULL);
  L58=ZALLOC(sizeof(struct FILE1_struct));
  if (L58==NULL) FATAL("Unable to allocate more memory");
  ((OB)L58)->header.tag=FILE1_tag;
  L57 = ((FILE1) L58);
  r5 = L57;
  
  SATTR(r5,fp,stdout);
  ret_val13 = r5;
  FILE_plus_dSTR(ret_val13, plus_s7);
  create_self3 = ((OUT) NULL);
  ret_val14 = create_self3;
  plus_self8 = ret_val14;
  plus_s8 = ((STR) &Inline1184875661);
  stdout_self6 = ((FILE1) NULL);
  L62=ZALLOC(sizeof(struct FILE1_struct));
  if (L62==NULL) FATAL("Unable to allocate more memory");
  ((OB)L62)->header.tag=FILE1_tag;
  L60 = ((FILE1) L62);
  r6 = L60;
  
  SATTR(r6,fp,stdout);
  ret_val16 = r6;
  FILE_plus_STR(ret_val16, plus_s8);
  ret_val15 = plus_self8;
  plus_self9 = ret_val15;
  L65=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L65==NULL) FATAL("Unable to allocate more memory");
  memset(L65,0,sizeof(struct INT_boxed_struct));
  ((OB)L65)->header.tag=INT_tag;
  L64 = (dSTR)((INT_boxed) L65);
  ((INT_boxed) L64)->immutable_part = ATTR(self,inlined_pointer_);
  plus_s9 = L64;
  stdout_self7 = ((FILE1) NULL);
  L67=ZALLOC(sizeof(struct FILE1_struct));
  if (L67==NULL) FATAL("Unable to allocate more memory");
  ((OB)L67)->header.tag=FILE1_tag;
  L66 = ((FILE1) L67);
  r7 = L66;
  
  SATTR(r7,fp,stdout);
  ret_val17 = r7;
  FILE_plus_dSTR(ret_val17, plus_s9);
  create_self4 = ((OUT) NULL);
  ret_val18 = create_self4;
  plus_self10 = ret_val18;
  plus_s10 = ((STR) &Marked1264672611);
  stdout_self8 = ((FILE1) NULL);
  L70=ZALLOC(sizeof(struct FILE1_struct));
  if (L70==NULL) FATAL("Unable to allocate more memory");
  ((OB)L70)->header.tag=FILE1_tag;
  L69 = ((FILE1) L70);
  r8 = L69;
  
  SATTR(r8,fp,stdout);
  ret_val20 = r8;
  FILE_plus_STR(ret_val20, plus_s10);
  ret_val19 = plus_self10;
  plus_self11 = ret_val19;
  L74=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L74==NULL) FATAL("Unable to allocate more memory");
  memset(L74,0,sizeof(struct INT_boxed_struct));
  ((OB)L74)->header.tag=INT_tag;
  L73 = (dSTR)((INT_boxed) L74);
  ((INT_boxed) L73)->immutable_part = INLINE598259372;
  plus_s11 = L73;
  stdout_self9 = ((FILE1) NULL);
  L76=ZALLOC(sizeof(struct FILE1_struct));
  if (L76==NULL) FATAL("Unable to allocate more memory");
  ((OB)L76)->header.tag=FILE1_tag;
  L75 = ((FILE1) L76);
  r9 = L75;
  
  SATTR(r9,fp,stdout);
  ret_val21 = r9;
  FILE_plus_dSTR(ret_val21, plus_s11);
  create_self5 = ((OUT) NULL);
  ret_val22 = create_self5;
  plus_self12 = ret_val22;
  plus_s12 = ((STR) &Inline1346595549);
  stdout_self10 = ((FILE1) NULL);
  L79=ZALLOC(sizeof(struct FILE1_struct));
  if (L79==NULL) FATAL("Unable to allocate more memory");
  ((OB)L79)->header.tag=FILE1_tag;
  L78 = ((FILE1) L79);
  r10 = L78;
  
  SATTR(r10,fp,stdout);
  ret_val24 = r10;
  FILE_plus_STR(ret_val24, plus_s12);
  ret_val23 = plus_self12;
  plus_self13 = ret_val23;
  L83=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L83==NULL) FATAL("Unable to allocate more memory");
  memset(L83,0,sizeof(struct INT_boxed_struct));
  ((OB)L83)->header.tag=INT_tag;
  L82 = (dSTR)((INT_boxed) L83);
  ((INT_boxed) L82)->immutable_part = INLINE957944682;
  plus_s13 = L82;
  stdout_self11 = ((FILE1) NULL);
  L85=ZALLOC(sizeof(struct FILE1_struct));
  if (L85==NULL) FATAL("Unable to allocate more memory");
  ((OB)L85)->header.tag=FILE1_tag;
  L84 = ((FILE1) L85);
  r11 = L84;
  
  SATTR(r11,fp,stdout);
  ret_val25 = r11;
  FILE_plus_dSTR(ret_val25, plus_s13);
  create_self6 = ((OUT) NULL);
  ret_val26 = create_self6;
  plus_self14 = ret_val26;
  plus_s14 = ((STR) &Marked887950191);
  stdout_self12 = ((FILE1) NULL);
  L88=ZALLOC(sizeof(struct FILE1_struct));
  if (L88==NULL) FATAL("Unable to allocate more memory");
  ((OB)L88)->header.tag=FILE1_tag;
  L87 = ((FILE1) L88);
  r12 = L87;
  
  SATTR(r12,fp,stdout);
  ret_val28 = r12;
  FILE_plus_STR(ret_val28, plus_s14);
  ret_val27 = plus_self14;
  plus_self15 = ret_val27;
  L92=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L92==NULL) FATAL("Unable to allocate more memory");
  memset(L92,0,sizeof(struct INT_boxed_struct));
  ((OB)L92)->header.tag=INT_tag;
  L90 = (dSTR)((INT_boxed) L92);
  ((INT_boxed) L90)->immutable_part = INLINE2014794524;
  plus_s15 = L90;
  stdout_self13 = ((FILE1) NULL);
  L94=ZALLOC(sizeof(struct FILE1_struct));
  if (L94==NULL) FATAL("Unable to allocate more memory");
  ((OB)L94)->header.tag=FILE1_tag;
  L93 = ((FILE1) L94);
  r13 = L93;
  
  SATTR(r13,fp,stdout);
  ret_val29 = r13;
  FILE_plus_dSTR(ret_val29, plus_s15);
  create_self7 = ((OUT) NULL);
  ret_val30 = create_self7;
  plus_self16 = ret_val30;
  plus_s16 = ((STR) &Inline778953810);
  stdout_self14 = ((FILE1) NULL);
  L97=ZALLOC(sizeof(struct FILE1_struct));
  if (L97==NULL) FATAL("Unable to allocate more memory");
  ((OB)L97)->header.tag=FILE1_tag;
  L96 = ((FILE1) L97);
  r14 = L96;
  
  SATTR(r14,fp,stdout);
  ret_val32 = r14;
  FILE_plus_STR(ret_val32, plus_s16);
  ret_val31 = plus_self16;
  plus_self17 = ret_val31;
  L100=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L100==NULL) FATAL("Unable to allocate more memory");
  memset(L100,0,sizeof(struct INT_boxed_struct));
  ((OB)L100)->header.tag=INT_tag;
  L99 = (dSTR)((INT_boxed) L100);
  ((INT_boxed) L99)->immutable_part = INLINE1258550073;
  plus_s17 = L99;
  stdout_self15 = ((FILE1) NULL);
  L103=ZALLOC(sizeof(struct FILE1_struct));
  if (L103==NULL) FATAL("Unable to allocate more memory");
  ((OB)L103)->header.tag=FILE1_tag;
  L102 = ((FILE1) L103);
  r15 = L102;
  
  SATTR(r15,fp,stdout);
  ret_val33 = r15;
  FILE_plus_dSTR(ret_val33, plus_s17);
  create_self8 = ((OUT) NULL);
  ret_val34 = create_self8;
  plus_self18 = ret_val34;
  plus_s18 = ((STR) &Marked2085347308);
  stdout_self16 = ((FILE1) NULL);
  L106=ZALLOC(sizeof(struct FILE1_struct));
  if (L106==NULL) FATAL("Unable to allocate more memory");
  ((OB)L106)->header.tag=FILE1_tag;
  L105 = ((FILE1) L106);
  r16 = L105;
  
  SATTR(r16,fp,stdout);
  ret_val36 = r16;
  FILE_plus_STR(ret_val36, plus_s18);
  ret_val35 = plus_self18;
  plus_self19 = ret_val35;
  L109=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L109==NULL) FATAL("Unable to allocate more memory");
  memset(L109,0,sizeof(struct INT_boxed_struct));
  ((OB)L109)->header.tag=INT_tag;
  L108 = (dSTR)((INT_boxed) L109);
  ((INT_boxed) L108)->immutable_part = INLINE520918393;
  plus_s19 = L108;
  stdout_self17 = ((FILE1) NULL);
  L111=ZALLOC(sizeof(struct FILE1_struct));
  if (L111==NULL) FATAL("Unable to allocate more memory");
  ((OB)L111)->header.tag=FILE1_tag;
  L110 = ((FILE1) L111);
  r17 = L110;
  
  SATTR(r17,fp,stdout);
  ret_val37 = r17;
  FILE_plus_dSTR(ret_val37, plus_s19);
  create_self9 = ((OUT) NULL);
  ret_val38 = create_self9;
  plus_self20 = ret_val38;
  plus_s20 = ((STR) &Inline311198520);
  stdout_self18 = ((FILE1) NULL);
  L114=ZALLOC(sizeof(struct FILE1_struct));
  if (L114==NULL) FATAL("Unable to allocate more memory");
  ((OB)L114)->header.tag=FILE1_tag;
  L113 = ((FILE1) L114);
  r18 = L113;
  
  SATTR(r18,fp,stdout);
  ret_val40 = r18;
  FILE_plus_STR(ret_val40, plus_s20);
  ret_val39 = plus_self20;
  plus_self21 = ret_val39;
  L117=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L117==NULL) FATAL("Unable to allocate more memory");
  memset(L117,0,sizeof(struct INT_boxed_struct));
  ((OB)L117)->header.tag=INT_tag;
  L116 = (dSTR)((INT_boxed) L117);
  ((INT_boxed) L116)->immutable_part = INLINE1306635001;
  plus_s21 = L116;
  stdout_self19 = ((FILE1) NULL);
  L119=ZALLOC(sizeof(struct FILE1_struct));
  if (L119==NULL) FATAL("Unable to allocate more memory");
  ((OB)L119)->header.tag=FILE1_tag;
  L118 = ((FILE1) L119);
  r19 = L118;
  
  SATTR(r19,fp,stdout);
  ret_val41 = r19;
  FILE_plus_dSTR(ret_val41, plus_s21);
  create_self10 = ((OUT) NULL);
  ret_val42 = create_self10;
  plus_self22 = ret_val42;
  plus_s22 = ((STR) &MarkedINTrouts);
  stdout_self20 = ((FILE1) NULL);
  L123=ZALLOC(sizeof(struct FILE1_struct));
  if (L123==NULL) FATAL("Unable to allocate more memory");
  ((OB)L123)->header.tag=FILE1_tag;
  L122 = ((FILE1) L123);
  r20 = L122;
  
  SATTR(r20,fp,stdout);
  ret_val44 = r20;
  FILE_plus_STR(ret_val44, plus_s22);
  ret_val43 = plus_self22;
  plus_self23 = ret_val43;
  L126=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L126==NULL) FATAL("Unable to allocate more memory");
  memset(L126,0,sizeof(struct INT_boxed_struct));
  ((OB)L126)->header.tag=INT_tag;
  L125 = (dSTR)((INT_boxed) L126);
  ((INT_boxed) L125)->immutable_part = INLINE1173790597;
  plus_s23 = L125;
  stdout_self21 = ((FILE1) NULL);
  L128=ZALLOC(sizeof(struct FILE1_struct));
  if (L128==NULL) FATAL("Unable to allocate more memory");
  ((OB)L128)->header.tag=FILE1_tag;
  L127 = ((FILE1) L128);
  r21 = L127;
  
  SATTR(r21,fp,stdout);
  ret_val45 = r21;
  FILE_plus_dSTR(ret_val45, plus_s23);
  create_self11 = ((OUT) NULL);
  ret_val46 = create_self11;
  plus_self24 = ret_val46;
  plus_s24 = ((STR) &FoldedINTcalls);
  stdout_self22 = ((FILE1) NULL);
  L132=ZALLOC(sizeof(struct FILE1_struct));
  if (L132==NULL) FATAL("Unable to allocate more memory");
  ((OB)L132)->header.tag=FILE1_tag;
  L130 = ((FILE1) L132);
  r22 = L130;
  
  SATTR(r22,fp,stdout);
  ret_val48 = r22;
  FILE_plus_STR(ret_val48, plus_s24);
  ret_val47 = plus_self24;
  plus_self25 = ret_val47;
  L135=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L135==NULL) FATAL("Unable to allocate more memory");
  memset(L135,0,sizeof(struct INT_boxed_struct));
  ((OB)L135)->header.tag=INT_tag;
  L134 = (dSTR)((INT_boxed) L135);
  ((INT_boxed) L134)->immutable_part = INLINE1950426034;
  plus_s25 = L134;
  stdout_self23 = ((FILE1) NULL);
  L137=ZALLOC(sizeof(struct FILE1_struct));
  if (L137==NULL) FATAL("Unable to allocate more memory");
  ((OB)L137)->header.tag=FILE1_tag;
  L136 = ((FILE1) L137);
  r23 = L136;
  
  SATTR(r23,fp,stdout);
  ret_val49 = r23;
  FILE_plus_dSTR(ret_val49, plus_s25);
  create_self12 = ((OUT) NULL);
  ret_val50 = create_self12;
  plus_self26 = ret_val50;
  plus_s26 = ((STR) &Markedshortrouts);
  stdout_self24 = ((FILE1) NULL);
  L140=ZALLOC(sizeof(struct FILE1_struct));
  if (L140==NULL) FATAL("Unable to allocate more memory");
  ((OB)L140)->header.tag=FILE1_tag;
  L139 = ((FILE1) L140);
  r24 = L139;
  
  SATTR(r24,fp,stdout);
  ret_val52 = r24;
  FILE_plus_STR(ret_val52, plus_s26);
  ret_val51 = plus_self26;
  plus_self27 = ret_val51;
  L144=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L144==NULL) FATAL("Unable to allocate more memory");
  memset(L144,0,sizeof(struct INT_boxed_struct));
  ((OB)L144)->header.tag=INT_tag;
  L143 = (dSTR)((INT_boxed) L144);
  ((INT_boxed) L143)->immutable_part = INLINE1516217386;
  plus_s27 = L143;
  stdout_self25 = ((FILE1) NULL);
  L146=ZALLOC(sizeof(struct FILE1_struct));
  if (L146==NULL) FATAL("Unable to allocate more memory");
  ((OB)L146)->header.tag=FILE1_tag;
  L145 = ((FILE1) L146);
  r25 = L145;
  
  SATTR(r25,fp,stdout);
  ret_val53 = r25;
  FILE_plus_dSTR(ret_val53, plus_s27);
  create_self13 = ((OUT) NULL);
  ret_val54 = create_self13;
  plus_self28 = ret_val54;
  plus_s28 = ((STR) &Inline1829321774);
  stdout_self26 = ((FILE1) NULL);
  L149=ZALLOC(sizeof(struct FILE1_struct));
  if (L149==NULL) FATAL("Unable to allocate more memory");
  ((OB)L149)->header.tag=FILE1_tag;
  L148 = ((FILE1) L149);
  r26 = L148;
  
  SATTR(r26,fp,stdout);
  ret_val56 = r26;
  FILE_plus_STR(ret_val56, plus_s28);
  ret_val55 = plus_self28;
  plus_self29 = ret_val55;
  L153=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L153==NULL) FATAL("Unable to allocate more memory");
  memset(L153,0,sizeof(struct INT_boxed_struct));
  ((OB)L153)->header.tag=INT_tag;
  L152 = (dSTR)((INT_boxed) L153);
  ((INT_boxed) L152)->immutable_part = INLINE1232309177;
  plus_s29 = L152;
  stdout_self27 = ((FILE1) NULL);
  L155=ZALLOC(sizeof(struct FILE1_struct));
  if (L155==NULL) FATAL("Unable to allocate more memory");
  ((OB)L155)->header.tag=FILE1_tag;
  L154 = ((FILE1) L155);
  r27 = L154;
  
  SATTR(r27,fp,stdout);
  ret_val57 = r27;
  FILE_plus_dSTR(ret_val57, plus_s29);
  create_self14 = ((OUT) NULL);
  ret_val58 = create_self14;
  plus_self30 = ret_val58;
  plus_s30 = ((STR) &Markedflatiters);
  stdout_self28 = ((FILE1) NULL);
  L158=ZALLOC(sizeof(struct FILE1_struct));
  if (L158==NULL) FATAL("Unable to allocate more memory");
  ((OB)L158)->header.tag=FILE1_tag;
  L157 = ((FILE1) L158);
  r28 = L157;
  
  SATTR(r28,fp,stdout);
  ret_val60 = r28;
  FILE_plus_STR(ret_val60, plus_s30);
  ret_val59 = plus_self30;
  plus_self31 = ret_val59;
  L161=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L161==NULL) FATAL("Unable to allocate more memory");
  memset(L161,0,sizeof(struct INT_boxed_struct));
  ((OB)L161)->header.tag=INT_tag;
  L160 = (dSTR)((INT_boxed) L161);
  ((INT_boxed) L160)->immutable_part = INLINE1401026874;
  plus_s31 = L160;
  stdout_self29 = ((FILE1) NULL);
  L163=ZALLOC(sizeof(struct FILE1_struct));
  if (L163==NULL) FATAL("Unable to allocate more memory");
  ((OB)L163)->header.tag=FILE1_tag;
  L162 = ((FILE1) L163);
  r29 = L162;
  
  SATTR(r29,fp,stdout);
  ret_val61 = r29;
  FILE_plus_dSTR(ret_val61, plus_s31);
  create_self15 = ((OUT) NULL);
  ret_val62 = create_self15;
  plus_self32 = ret_val62;
  plus_s32 = ((STR) &Inline453141559);
  stdout_self30 = ((FILE1) NULL);
  L166=ZALLOC(sizeof(struct FILE1_struct));
  if (L166==NULL) FATAL("Unable to allocate more memory");
  ((OB)L166)->header.tag=FILE1_tag;
  L165 = ((FILE1) L166);
  r30 = L165;
  
  SATTR(r30,fp,stdout);
  ret_val64 = r30;
  FILE_plus_STR(ret_val64, plus_s32);
  ret_val63 = plus_self32;
  plus_self33 = ret_val63;
  L169=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L169==NULL) FATAL("Unable to allocate more memory");
  memset(L169,0,sizeof(struct INT_boxed_struct));
  ((OB)L169)->header.tag=INT_tag;
  L168 = (dSTR)((INT_boxed) L169);
  ((INT_boxed) L168)->immutable_part = INLINE1301544351;
  plus_s33 = L168;
  stdout_self31 = ((FILE1) NULL);
  L172=ZALLOC(sizeof(struct FILE1_struct));
  if (L172==NULL) FATAL("Unable to allocate more memory");
  ((OB)L172)->header.tag=FILE1_tag;
  L170 = ((FILE1) L172);
  r31 = L170;
  
  SATTR(r31,fp,stdout);
  ret_val65 = r31;
  FILE_plus_dSTR(ret_val65, plus_s33);
  create_self16 = ((OUT) NULL);
  ret_val66 = create_self16;
  plus_self34 = ret_val66;
  plus_s34 = ((STR) &Inline1146142148);
  stdout_self32 = ((FILE1) NULL);
  L175=ZALLOC(sizeof(struct FILE1_struct));
  if (L175==NULL) FATAL("Unable to allocate more memory");
  ((OB)L175)->header.tag=FILE1_tag;
  L174 = ((FILE1) L175);
  r32 = L174;
  
  SATTR(r32,fp,stdout);
  ret_val68 = r32;
  FILE_plus_STR(ret_val68, plus_s34);
  ret_val67 = plus_self34;
  plus_self35 = ret_val67;
  L178=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L178==NULL) FATAL("Unable to allocate more memory");
  memset(L178,0,sizeof(struct INT_boxed_struct));
  ((OB)L178)->header.tag=INT_tag;
  L177 = (dSTR)((INT_boxed) L178);
  ((INT_boxed) L177)->immutable_part = ATTR(self,inlined_iter_cou);
  plus_s35 = L177;
  stdout_self33 = ((FILE1) NULL);
  L180=ZALLOC(sizeof(struct FILE1_struct));
  if (L180==NULL) FATAL("Unable to allocate more memory");
  ((OB)L180)->header.tag=FILE1_tag;
  L179 = ((FILE1) L180);
  r33 = L179;
  
  SATTR(r33,fp,stdout);
  ret_val70 = r33;
  FILE_plus_dSTR(ret_val70, plus_s35);
  ret_val69 = plus_self35;
  plus_self36 = ret_val69;
  plus_s36 = ((STR) &name163);
  stdout_self34 = ((FILE1) NULL);
  L183=ZALLOC(sizeof(struct FILE1_struct));
  if (L183==NULL) FATAL("Unable to allocate more memory");
  ((OB)L183)->header.tag=FILE1_tag;
  L182 = ((FILE1) L183);
  r34 = L182;
  
  SATTR(r34,fp,stdout);
  ret_val71 = r34;
  FILE_plus_STR(ret_val71, plus_s36);
  L185 = (ATTR(ATTR(self,prog),itercounts)==((ARRAYINT) NULL));
  L1861_=!(L185); 
  if (L1861_) {
   create_self17 = ((OUT) NULL);
   ret_val72 = create_self17;
   plus_self37 = ret_val72;
   plus_s37 = ((STR) &Thefol1627053373);
   stdout_self35 = ((FILE1) NULL);
   L188=ZALLOC(sizeof(struct FILE1_struct));
   if (L188==NULL) FATAL("Unable to allocate more memory");
   ((OB)L188)->header.tag=FILE1_tag;
   L187 = ((FILE1) L188);
   r35 = L187;
   
   SATTR(r35,fp,stdout);
   ret_val73 = r35;
   FILE_plus_STR(ret_val73, plus_s37);
   create_self18 = ((OUT) NULL);
   ret_val74 = create_self18;
   plus_self38 = ret_val74;
   plus_s38 = ((STR) &ItersItersloops);
   stdout_self36 = ((FILE1) NULL);
   L191=ZALLOC(sizeof(struct FILE1_struct));
   if (L191==NULL) FATAL("Unable to allocate more memory");
   ((OB)L191)->header.tag=FILE1_tag;
   L190 = ((FILE1) L191);
   r36 = L190;
   
   SATTR(r36,fp,stdout);
   ret_val75 = r36;
   FILE_plus_STR(ret_val75, plus_s38);
   {
    BOOL f_L1931_ = TRUE;
    L11 = 0;
    L21 = 9;
    L1931_=L11-1;L1931_m=L21; 
    while (1) {
     if(L1931_++>=L1931_m)  goto after_loop1; 
     i = L1931_;
     L195 = ATTR(ATTR(self,prog),itercounts);
     L1961_=ARR((ARRAYINT)L195,i); 
     ct = L1961_;
     L1971_=(0)<(ct); 
     if (L1971_) {
     }
     else {
      goto after_loop1;
     }
     create_self19 = ((OUT) NULL);
     ret_val76 = create_self19;
     plus_self39 = ret_val76;
     L199=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L199==NULL) FATAL("Unable to allocate more memory");
     memset(L199,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L199)->header.tag=CHAR_tag;
     L198 = (dSTR)((CHAR_boxed) L199);
     ((CHAR_boxed) L198)->immutable_part = '\t';
     plus_s39 = L198;
     stdout_self37 = ((FILE1) NULL);
     L201=ZALLOC(sizeof(struct FILE1_struct));
     if (L201==NULL) FATAL("Unable to allocate more memory");
     ((OB)L201)->header.tag=FILE1_tag;
     L200 = ((FILE1) L201);
     r37 = L200;
     
     SATTR(r37,fp,stdout);
     ret_val78 = r37;
     FILE_plus_dSTR(ret_val78, plus_s39);
     ret_val77 = plus_self39;
     plus_self40 = ret_val77;
     L204=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
     if (L204==NULL) FATAL("Unable to allocate more memory");
     memset(L204,0,sizeof(struct INT_boxed_struct));
     ((OB)L204)->header.tag=INT_tag;
     L203 = (dSTR)((INT_boxed) L204);
     ((INT_boxed) L203)->immutable_part = i;
     plus_s40 = L203;
     stdout_self38 = ((FILE1) NULL);
     L206=ZALLOC(sizeof(struct FILE1_struct));
     if (L206==NULL) FATAL("Unable to allocate more memory");
     ((OB)L206)->header.tag=FILE1_tag;
     L205 = ((FILE1) L206);
     r38 = L205;
     
     SATTR(r38,fp,stdout);
     ret_val80 = r38;
     FILE_plus_dSTR(ret_val80, plus_s40);
     ret_val79 = plus_self40;
     plus_self41 = ret_val79;
     L209=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L209==NULL) FATAL("Unable to allocate more memory");
     memset(L209,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L209)->header.tag=CHAR_tag;
     L208 = (dSTR)((CHAR_boxed) L209);
     ((CHAR_boxed) L208)->immutable_part = '\t';
     plus_s41 = L208;
     stdout_self39 = ((FILE1) NULL);
     L211=ZALLOC(sizeof(struct FILE1_struct));
     if (L211==NULL) FATAL("Unable to allocate more memory");
     ((OB)L211)->header.tag=FILE1_tag;
     L210 = ((FILE1) L211);
     r39 = L210;
     
     SATTR(r39,fp,stdout);
     ret_val82 = r39;
     FILE_plus_dSTR(ret_val82, plus_s41);
     ret_val81 = plus_self41;
     plus_self42 = ret_val81;
     L214=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
     if (L214==NULL) FATAL("Unable to allocate more memory");
     memset(L214,0,sizeof(struct INT_boxed_struct));
     ((OB)L214)->header.tag=INT_tag;
     L213 = (dSTR)((INT_boxed) L214);
     ((INT_boxed) L213)->immutable_part = ct;
     plus_s42 = L213;
     stdout_self40 = ((FILE1) NULL);
     L216=ZALLOC(sizeof(struct FILE1_struct));
     if (L216==NULL) FATAL("Unable to allocate more memory");
     ((OB)L216)->header.tag=FILE1_tag;
     L215 = ((FILE1) L216);
     r40 = L215;
     
     SATTR(r40,fp,stdout);
     ret_val84 = r40;
     FILE_plus_dSTR(ret_val84, plus_s42);
     ret_val83 = plus_self42;
     plus_self43 = ret_val83;
     L219=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L219==NULL) FATAL("Unable to allocate more memory");
     memset(L219,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L219)->header.tag=CHAR_tag;
     L218 = (dSTR)((CHAR_boxed) L219);
     ((CHAR_boxed) L218)->immutable_part = '\n';
     plus_s43 = L218;
     stdout_self41 = ((FILE1) NULL);
     L221=ZALLOC(sizeof(struct FILE1_struct));
     if (L221==NULL) FATAL("Unable to allocate more memory");
     ((OB)L221)->header.tag=FILE1_tag;
     L220 = ((FILE1) L221);
     r41 = L220;
     
     SATTR(r41,fp,stdout);
     ret_val85 = r41;
     FILE_plus_dSTR(ret_val85, plus_s43);
    }
   }
   after_loop1: ;
   create_self20 = ((OUT) NULL);
   ret_val86 = create_self20;
   plus_self44 = ret_val86;
   plus_s44 = ((STR) &HotsItersloops);
   stdout_self42 = ((FILE1) NULL);
   L224=ZALLOC(sizeof(struct FILE1_struct));
   if (L224==NULL) FATAL("Unable to allocate more memory");
   ((OB)L224)->header.tag=FILE1_tag;
   L223 = ((FILE1) L224);
   r42 = L223;
   
   SATTR(r42,fp,stdout);
   ret_val87 = r42;
   FILE_plus_STR(ret_val87, plus_s44);
   {
    BOOL f_L2261_ = TRUE;
    L31 = 0;
    L41 = 9;
    L2261_=L31-1;L2261_m=L41; 
    while (1) {
     if(L2261_++>=L2261_m)  goto after_loop2; 
     i1 = L2261_;
     L228 = ATTR(ATTR(self,prog),hotcounts);
     L2291_=ARR((ARRAYINT)L228,i1); 
     ct1 = L2291_;
     L2301_=(0)<(ct1); 
     if (L2301_) {
     }
     else {
      goto after_loop2;
     }
     create_self21 = ((OUT) NULL);
     ret_val88 = create_self21;
     plus_self45 = ret_val88;
     L232=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L232==NULL) FATAL("Unable to allocate more memory");
     memset(L232,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L232)->header.tag=CHAR_tag;
     L231 = (dSTR)((CHAR_boxed) L232);
     ((CHAR_boxed) L231)->immutable_part = '\t';
     plus_s45 = L231;
     stdout_self43 = ((FILE1) NULL);
     L234=ZALLOC(sizeof(struct FILE1_struct));
     if (L234==NULL) FATAL("Unable to allocate more memory");
     ((OB)L234)->header.tag=FILE1_tag;
     L233 = ((FILE1) L234);
     r43 = L233;
     
     SATTR(r43,fp,stdout);
     ret_val90 = r43;
     FILE_plus_dSTR(ret_val90, plus_s45);
     ret_val89 = plus_self45;
     plus_self46 = ret_val89;
     L237=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
     if (L237==NULL) FATAL("Unable to allocate more memory");
     memset(L237,0,sizeof(struct INT_boxed_struct));
     ((OB)L237)->header.tag=INT_tag;
     L236 = (dSTR)((INT_boxed) L237);
     ((INT_boxed) L236)->immutable_part = i1;
     plus_s46 = L236;
     stdout_self44 = ((FILE1) NULL);
     L239=ZALLOC(sizeof(struct FILE1_struct));
     if (L239==NULL) FATAL("Unable to allocate more memory");
     ((OB)L239)->header.tag=FILE1_tag;
     L238 = ((FILE1) L239);
     r44 = L238;
     
     SATTR(r44,fp,stdout);
     ret_val92 = r44;
     FILE_plus_dSTR(ret_val92, plus_s46);
     ret_val91 = plus_self46;
     plus_self47 = ret_val91;
     L242=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L242==NULL) FATAL("Unable to allocate more memory");
     memset(L242,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L242)->header.tag=CHAR_tag;
     L241 = (dSTR)((CHAR_boxed) L242);
     ((CHAR_boxed) L241)->immutable_part = '\t';
     plus_s47 = L241;
     stdout_self45 = ((FILE1) NULL);
     L244=ZALLOC(sizeof(struct FILE1_struct));
     if (L244==NULL) FATAL("Unable to allocate more memory");
     ((OB)L244)->header.tag=FILE1_tag;
     L243 = ((FILE1) L244);
     r45 = L243;
     
     SATTR(r45,fp,stdout);
     ret_val94 = r45;
     FILE_plus_dSTR(ret_val94, plus_s47);
     ret_val93 = plus_self47;
     plus_self48 = ret_val93;
     L247=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
     if (L247==NULL) FATAL("Unable to allocate more memory");
     memset(L247,0,sizeof(struct INT_boxed_struct));
     ((OB)L247)->header.tag=INT_tag;
     L246 = (dSTR)((INT_boxed) L247);
     ((INT_boxed) L246)->immutable_part = ct1;
     plus_s48 = L246;
     stdout_self46 = ((FILE1) NULL);
     L249=ZALLOC(sizeof(struct FILE1_struct));
     if (L249==NULL) FATAL("Unable to allocate more memory");
     ((OB)L249)->header.tag=FILE1_tag;
     L248 = ((FILE1) L249);
     r46 = L248;
     
     SATTR(r46,fp,stdout);
     ret_val96 = r46;
     FILE_plus_dSTR(ret_val96, plus_s48);
     ret_val95 = plus_self48;
     plus_self49 = ret_val95;
     L253=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L253==NULL) FATAL("Unable to allocate more memory");
     memset(L253,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L253)->header.tag=CHAR_tag;
     L252 = (dSTR)((CHAR_boxed) L253);
     ((CHAR_boxed) L252)->immutable_part = '\n';
     plus_s49 = L252;
     stdout_self47 = ((FILE1) NULL);
     L255=ZALLOC(sizeof(struct FILE1_struct));
     if (L255==NULL) FATAL("Unable to allocate more memory");
     ((OB)L255)->header.tag=FILE1_tag;
     L254 = ((FILE1) L255);
     r47 = L254;
     
     SATTR(r47,fp,stdout);
     ret_val97 = r47;
     FILE_plus_dSTR(ret_val97, plus_s49);
    }
   }
   after_loop2: ;
  }
 }
 allflags = CONFIG1397479570(ATTR(ATTR(self,prog),config), ((STR) &CC_OPTIONS), 0);
 libs = ((STR) &name1);
 plus_self50 = allflags;
 plus_sarg = ((STR) &I);
 ret_val98 = STR_ap2004550586(plus_self50, plus_sarg);
 allflags = ret_val98;
 if (ATTR(self,debug1)) {
  plus_self51 = allflags;
  plus_arg = ' ';
  str_self = plus_arg;
  create_self22 = ((STR) NULL);
  create_c = str_self;
  L258 = 1;
  L260=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L258)*sizeof(CHAR);
  L259=ZALLOC_LEAF_BIG(L260);
  if (L259==NULL) FATAL("Unable to allocate more memory");
  memset(L259,0,L260);
  ((OB)L259)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L259)->header.destroyed=0;
#endif

  L257 = ((STR) L259);
  L257->asize = L258;
  r48 = L257;
  SARR((STR)r48,0,create_c); 
  ;
  ret_val101 = r48;
  ret_val100 = ret_val101;
  s4 = ret_val100;
  ret_val99 = STR_ap1077157958(plus_self51, s4, TRUE);
  plus_self52 = ret_val99;
  plus_sarg1 = CONFIG1397479570(ATTR(ATTR(self,prog),config), ((STR) &CC_DEBUG_FLAG), 1);
  ret_val102 = STR_ap2004550586(plus_self52, plus_sarg1);
  allflags = ret_val102;
 }
 else {
  plus_self53 = allflags;
  plus_arg1 = ' ';
  str_self1 = plus_arg1;
  create_self23 = ((STR) NULL);
  create_c1 = str_self1;
  L263 = 1;
  L265=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L263)*sizeof(CHAR);
  L264=ZALLOC_LEAF_BIG(L265);
  if (L264==NULL) FATAL("Unable to allocate more memory");
  memset(L264,0,L265);
  ((OB)L264)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L264)->header.destroyed=0;
#endif

  L262 = ((STR) L264);
  L262->asize = L263;
  r49 = L262;
  SARR((STR)r49,0,create_c1); 
  ;
  ret_val105 = r49;
  ret_val104 = ret_val105;
  s5 = ret_val104;
  ret_val103 = STR_ap1077157958(plus_self53, s5, TRUE);
  plus_self54 = ret_val103;
  plus_sarg2 = CONFIG1397479570(ATTR(ATTR(self,prog),config), ((STR) &CC_DEBUG_FLAG), 0);
  ret_val106 = STR_ap2004550586(plus_self54, plus_sarg2);
  allflags = ret_val106;
 }
 if (ATTR(self,optimize)) {
  plus_self55 = allflags;
  plus_arg2 = ' ';
  str_self2 = plus_arg2;
  create_self24 = ((STR) NULL);
  create_c2 = str_self2;
  L268 = 1;
  L270=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L268)*sizeof(CHAR);
  L269=ZALLOC_LEAF_BIG(L270);
  if (L269==NULL) FATAL("Unable to allocate more memory");
  memset(L269,0,L270);
  ((OB)L269)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L269)->header.destroyed=0;
#endif

  L267 = ((STR) L269);
  L267->asize = L268;
  r50 = L267;
  SARR((STR)r50,0,create_c2); 
  ;
  ret_val109 = r50;
  ret_val108 = ret_val109;
  s6 = ret_val108;
  ret_val107 = STR_ap1077157958(plus_self55, s6, TRUE);
  plus_self56 = ret_val107;
  plus_sarg3 = CONFIG1397479570(ATTR(ATTR(self,prog),config), ((STR) &CC_OPTIMIZE_FLAG), 1);
  ret_val110 = STR_ap2004550586(plus_self56, plus_sarg3);
  allflags = ret_val110;
 }
 else {
  plus_self57 = allflags;
  plus_arg3 = ' ';
  str_self3 = plus_arg3;
  create_self25 = ((STR) NULL);
  create_c3 = str_self3;
  L274 = 1;
  L276=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L274)*sizeof(CHAR);
  L275=ZALLOC_LEAF_BIG(L276);
  if (L275==NULL) FATAL("Unable to allocate more memory");
  memset(L275,0,L276);
  ((OB)L275)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L275)->header.destroyed=0;
#endif

  L273 = ((STR) L275);
  L273->asize = L274;
  r51 = L273;
  SARR((STR)r51,0,create_c3); 
  ;
  ret_val113 = r51;
  ret_val112 = ret_val113;
  s7 = ret_val112;
  ret_val1111 = STR_ap1077157958(plus_self57, s7, TRUE);
  plus_self58 = ret_val1111;
  plus_sarg4 = CONFIG1397479570(ATTR(ATTR(self,prog),config), ((STR) &CC_OPTIMIZE_FLAG), 0);
  ret_val114 = STR_ap2004550586(plus_self58, plus_sarg4);
  allflags = ret_val114;
 }
 if (ATTR(ATTR(self,prog),prolix)) {
  plus_self59 = allflags;
  plus_arg4 = ' ';
  str_self4 = plus_arg4;
  create_self26 = ((STR) NULL);
  create_c4 = str_self4;
  L279 = 1;
  L281=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L279)*sizeof(CHAR);
  L280=ZALLOC_LEAF_BIG(L281);
  if (L280==NULL) FATAL("Unable to allocate more memory");
  memset(L280,0,L281);
  ((OB)L280)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L280)->header.destroyed=0;
#endif

  L278 = ((STR) L280);
  L278->asize = L279;
  r52 = L278;
  SARR((STR)r52,0,create_c4); 
  ;
  ret_val117 = r52;
  ret_val116 = ret_val117;
  s8 = ret_val116;
  ret_val115 = STR_ap1077157958(plus_self59, s8, TRUE);
  plus_self60 = ret_val115;
  plus_sarg5 = CONFIG1397479570(ATTR(ATTR(self,prog),config), ((STR) &CC_PROLIX_FLAG), 0);
  ret_val118 = STR_ap2004550586(plus_self60, plus_sarg5);
  allflags = ret_val118;
 }
 else {
  plus_self61 = allflags;
  plus_arg5 = ' ';
  str_self5 = plus_arg5;
  create_self27 = ((STR) NULL);
  create_c5 = str_self5;
  L284 = 1;
  L286=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L284)*sizeof(CHAR);
  L285=ZALLOC_LEAF_BIG(L286);
  if (L285==NULL) FATAL("Unable to allocate more memory");
  memset(L285,0,L286);
  ((OB)L285)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L285)->header.destroyed=0;
#endif

  L283 = ((STR) L285);
  L283->asize = L284;
  r53 = L283;
  SARR((STR)r53,0,create_c5); 
  ;
  ret_val121 = r53;
  ret_val120 = ret_val121;
  s9 = ret_val120;
  ret_val119 = STR_ap1077157958(plus_self61, s9, TRUE);
  plus_self62 = ret_val119;
  plus_sarg6 = CONFIG1397479570(ATTR(ATTR(self,prog),config), ((STR) &CC_PROLIX_FLAG), 1);
  ret_val122 = STR_ap2004550586(plus_self62, plus_sarg6);
  allflags = ret_val122;
 }
 {
  struct FLISTS1171631134_frame_struct other5_0;
FLISTS1171631134_frame noname1 = &other5_0;
  L51 = ATTR(self,c_flags);
  noname1->self = L51;
  noname1->state = 0;
  while (1) {
   L288 = FLISTS1171631134(noname1);
   if (noname1->state == -1) {
    goto after_loop3;
   }
   flag1 = L288;
   L290 = STR_sizerINT(flag1);
   L2921_=(2)<(L290); 
   if (L2921_) {
    L293 = STR_head_INTrSTR(flag1, 2);
    L289 = STR_is111530248(L293, ((STR) &l111));
   } else {
    L289 = FALSE;
   }
   if (L289) {
    plus_self63 = libs;
    plus_arg6 = ' ';
    str_self6 = plus_arg6;
    create_self28 = ((STR) NULL);
    create_c6 = str_self6;
    L295 = 1;
    L297=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L295)*sizeof(CHAR);
    L296=ZALLOC_LEAF_BIG(L297);
    if (L296==NULL) FATAL("Unable to allocate more memory");
    memset(L296,0,L297);
    ((OB)L296)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L296)->header.destroyed=0;
#endif

    L294 = ((STR) L296);
    L294->asize = L295;
    r54 = L294;
    SARR((STR)r54,0,create_c6); 
    ;
    ret_val125 = r54;
    ret_val124 = ret_val125;
    s10 = ret_val124;
    ret_val123 = STR_ap1077157958(plus_self63, s10, TRUE);
    plus_self64 = ret_val123;
    plus_sarg7 = flag1;
    ret_val126 = STR_ap2004550586(plus_self64, plus_sarg7);
    libs = ret_val126;
   }
   else {
    plus_self65 = allflags;
    plus_arg7 = ' ';
    str_self7 = plus_arg7;
    create_self29 = ((STR) NULL);
    create_c7 = str_self7;
    L300 = 1;
    L302=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L300)*sizeof(CHAR);
    L301=ZALLOC_LEAF_BIG(L302);
    if (L301==NULL) FATAL("Unable to allocate more memory");
    memset(L301,0,L302);
    ((OB)L301)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L301)->header.destroyed=0;
#endif

    L299 = ((STR) L301);
    L299->asize = L300;
    r55 = L299;
    SARR((STR)r55,0,create_c7); 
    ;
    ret_val129 = r55;
    ret_val128 = ret_val129;
    s11 = ret_val128;
    ret_val127 = STR_ap1077157958(plus_self65, s11, TRUE);
    plus_self66 = ret_val127;
    plus_sarg8 = flag1;
    ret_val130 = STR_ap2004550586(plus_self66, plus_sarg8);
    allflags = ret_val130;
   }
  }
 }
 after_loop3: ;
 plus_self67 = libs;
 plus_arg8 = ' ';
 str_self8 = plus_arg8;
 create_self30 = ((STR) NULL);
 create_c8 = str_self8;
 L305 = 1;
 L307=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L305)*sizeof(CHAR);
 L306=ZALLOC_LEAF_BIG(L307);
 if (L306==NULL) FATAL("Unable to allocate more memory");
 memset(L306,0,L307);
 ((OB)L306)->header.tag=STR_tag;
#ifdef DESTROY_CHK

   ((OB)L306)->header.destroyed=0;
#endif

 L304 = ((STR) L306);
 L304->asize = L305;
 r56 = L304;
 SARR((STR)r56,0,create_c8); 
 ;
 ret_val133 = r56;
 ret_val132 = ret_val133;
 s12 = ret_val132;
 ret_val131 = STR_ap1077157958(plus_self67, s12, TRUE);
 plus_self68 = ret_val131;
 plus_sarg9 = CONFIG1397479570(ATTR(ATTR(self,prog),config), ((STR) &GC_LINK), 0);
 ret_val134 = STR_ap2004550586(plus_self68, plus_sarg9);
 plus_self69 = ret_val134;
 plus_arg9 = ' ';
 str_self9 = plus_arg9;
 create_self31 = ((STR) NULL);
 create_c9 = str_self9;
 L310 = 1;
 L312=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L310)*sizeof(CHAR);
 L311=ZALLOC_LEAF_BIG(L312);
 if (L311==NULL) FATAL("Unable to allocate more memory");
 memset(L311,0,L312);
 ((OB)L311)->header.tag=STR_tag;
#ifdef DESTROY_CHK

   ((OB)L311)->header.destroyed=0;
#endif

 L309 = ((STR) L311);
 L309->asize = L310;
 r57 = L309;
 SARR((STR)r57,0,create_c9); 
 ;
 ret_val137 = r57;
 ret_val136 = ret_val137;
 s13 = ret_val136;
 ret_val135 = STR_ap1077157958(plus_self69, s13, TRUE);
 plus_self70 = ret_val135;
 plus_sarg10 = CONFIG1397479570(ATTR(ATTR(self,prog),config), ((STR) &LINK_OPTIONS), 0);
 ret_val138 = STR_ap2004550586(plus_self70, plus_sarg10);
 libs = ret_val138;
 psather_self = ATTR(self,prog);
 if (ATTR(psather_self,threads)) {
  L314 = TRUE;
 } else {
  L314 = ATTR(psather_self,distributed);
 }
 ret_val139 = L314;
 if (ret_val139) {
  if (ATTR(ATTR(self,options1),psather_chk)) {
   plus_self71 = allflags;
   plus_sarg11 = ((STR) &DPSATHER_CHK);
   ret_val140 = STR_ap2004550586(plus_self71, plus_sarg11);
   allflags = ret_val140;
  }
  if (ATTR(ATTR(self,options1),psather_stats)) {
   plus_self72 = allflags;
   plus_sarg12 = ((STR) &DPSATH545432352);
   ret_val141 = STR_ap2004550586(plus_self72, plus_sarg12);
   allflags = ret_val141;
  }
  if (ATTR(ATTR(self,options1),psather_trace)) {
   plus_self73 = allflags;
   plus_sarg13 = ((STR) &DPSATHER_TRACE);
   ret_val142 = STR_ap2004550586(plus_self73, plus_sarg13);
   allflags = ret_val142;
  }
 }
 obfiles = CODE_F1366222275(((CODE_FILE) NULL));
 syscom = ((STR) &dCCdCFLAGS);
 plus_self74 = syscom;
 plus_sarg14 = CONFIG1397479570(ATTR(ATTR(self,prog),config), ((STR) &EXEC_OPTION), 0);
 ret_val143 = STR_ap2004550586(plus_self74, plus_sarg14);
 plus_self75 = ret_val143;
 plus_sarg15 = ((STR) &dCSdOBJ);
 ret_val144 = STR_ap2004550586(plus_self75, plus_sarg15);
 syscom = ret_val144;
 other1 = ((STR) &name1);
 {
  struct FSETSTR_eltbrSTR_frame_struct other6_0;
FSETSTR_eltbrSTR_frame noname2 = &other6_0;
  L61 = ATTR(self,c_files);
  noname2->self = L61;
  noname2->state = 0;
  while (1) {
   L315 = FSETSTR_eltbrSTR(noname2);
   if (noname2->state == -1) {
    goto after_loop4;
   }
   replace_suffix_s = L315;
   replace_suffix_o = CONFIG1397479570(ATTR(ATTR(self,prog),config), ((STR) &C_EXT), 0);
   replace_suffix_r = CONFIG1397479570(ATTR(ATTR(self,prog),config), ((STR) &OBJECT_EXT), 0);
   L316 = STR_lengthrINT(replace_suffix_s);
   L317 = STR_sizerINT(replace_suffix_o);
   L3181_=INTMINUS(L316,L317); 
   plus_self76 = STR_head_INTrSTR(replace_suffix_s, L3181_);
   plus_sarg16 = replace_suffix_r;
   ret_val146 = STR_ap2004550586(plus_self76, plus_sarg16);
   ret_val145 = ret_val146;
   s = ret_val145;
   plus_self77 = obfiles;
   plus_arg10 = ' ';
   str_self10 = plus_arg10;
   create_self32 = ((STR) NULL);
   create_c10 = str_self10;
   L320 = 1;
   L322=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L320)*sizeof(CHAR);
   L321=ZALLOC_LEAF_BIG(L322);
   if (L321==NULL) FATAL("Unable to allocate more memory");
   memset(L321,0,L322);
   ((OB)L321)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L321)->header.destroyed=0;
#endif

   L319 = ((STR) L321);
   L319->asize = L320;
   r58 = L319;
   SARR((STR)r58,0,create_c10); 
   ;
   ret_val149 = r58;
   ret_val148 = ret_val149;
   s14 = ret_val148;
   ret_val147 = STR_ap1077157958(plus_self77, s14, TRUE);
   plus_self78 = ret_val147;
   L324 = STR_se1633361857(s, '/');
   L3251_=INTPLUS(L324,1); 
   plus_sarg17 = STR_su1627796404(s, L3251_);
   ret_val150 = STR_ap2004550586(plus_self78, plus_sarg17);
   obfiles = ret_val150;
  }
 }
 after_loop4: ;
 {
  struct FSETSTR_eltbrSTR_frame_struct other7_0;
FSETSTR_eltbrSTR_frame noname3 = &other7_0;
  L71 = ATTR(self,object_files);
  noname3->self = L71;
  noname3->state = 0;
  while (1) {
   L326 = FSETSTR_eltbrSTR(noname3);
   if (noname3->state == -1) {
    goto after_loop5;
   }
   s1 = L326;
   L3271_=ARR((STR)s1,0); 
   L328 = L3271_;
   L3291_=L328=='/'; 
   if (L3291_) {
    plus_self79 = other1;
    plus_arg11 = ' ';
    str_self11 = plus_arg11;
    create_self33 = ((STR) NULL);
    create_c11 = str_self11;
    L331 = 1;
    L333=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L331)*sizeof(CHAR);
    L332=ZALLOC_LEAF_BIG(L333);
    if (L332==NULL) FATAL("Unable to allocate more memory");
    memset(L332,0,L333);
    ((OB)L332)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L332)->header.destroyed=0;
#endif

    L330 = ((STR) L332);
    L330->asize = L331;
    r59 = L330;
    SARR((STR)r59,0,create_c11); 
    ;
    ret_val153 = r59;
    ret_val152 = ret_val153;
    s15 = ret_val152;
    ret_val151 = STR_ap1077157958(plus_self79, s15, TRUE);
    plus_self80 = ret_val151;
    plus_sarg18 = s1;
    ret_val154 = STR_ap2004550586(plus_self80, plus_sarg18);
    other1 = ret_val154;
   }
   else {
    L3351_=ARR((STR)s1,0); 
    L336 = L3351_;
    L3371_=L336=='-'; 
    if (L3371_) {
     plus_self81 = other1;
     plus_sarg19 = s1;
     ret_val155 = STR_ap2004550586(plus_self81, plus_sarg19);
     other1 = ret_val155;
    }
    else {
     plus_self82 = other1;
     plus_arg12 = ' ';
     str_self12 = plus_arg12;
     create_self34 = ((STR) NULL);
     create_c12 = str_self12;
     L339 = 1;
     L341=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L339)*sizeof(CHAR);
     L340=ZALLOC_LEAF_BIG(L341);
     if (L340==NULL) FATAL("Unable to allocate more memory");
     memset(L340,0,L341);
     ((OB)L340)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L340)->header.destroyed=0;
#endif

     L338 = ((STR) L340);
     L338->asize = L339;
     r60 = L338;
     SARR((STR)r60,0,create_c12); 
     ;
     ret_val158 = r60;
     ret_val157 = ret_val158;
     s16 = ret_val157;
     ret_val156 = STR_ap1077157958(plus_self82, s16, TRUE);
     plus_self83 = ret_val156;
     plus_sarg20 = ((STR) &name46);
     ret_val159 = STR_ap2004550586(plus_self83, plus_sarg20);
     plus_self84 = ret_val159;
     plus_sarg21 = s1;
     ret_val160 = STR_ap2004550586(plus_self84, plus_sarg21);
     other1 = ret_val160;
    }
   }
  }
 }
 after_loop5: ;
 {
  struct TP_CLA115798516_frame_struct other8_0;
TP_CLA115798516_frame noname4 = &other8_0;
  L81 = ATTR(ATTR(ATTR(self,prog),tp_tbl),class_tbl);
  noname4->self = L81;
  noname4->state = 0;
  while (1) {
   L343 = TP_CLA115798516(noname4);
   if (noname4->state == -1) {
    goto after_loop6;
   }
   cl = L343;
   L344 = ATTR(self,externals);
   files11 = FMAPST521088757(L344, TP_CLASS_strrSTR(cl));
   L345 = (files11==((FSETSTR) NULL));
   L3461_=!(L345); 
   if (L3461_) {
    {
     struct FSETSTR_eltbrSTR_frame_struct other9_0;
FSETSTR_eltbrSTR_frame noname5 = &other9_0;
     L91 = files11;
     noname5->self = L91;
     noname5->state = 0;
     while (1) {
      L347 = FSETSTR_eltbrSTR(noname5);
      if (noname5->state == -1) {
       goto after_loop7;
      }
      file1 = L347;
      if (ATTR(ATTR(self,prog),prolix)) {
       create_self35 = ((OUT) NULL);
       ret_val161 = create_self35;
       plus_self85 = ret_val161;
       plus_s50 = ((STR) &Including1);
       stdout_self48 = ((FILE1) NULL);
       L349=ZALLOC(sizeof(struct FILE1_struct));
       if (L349==NULL) FATAL("Unable to allocate more memory");
       ((OB)L349)->header.tag=FILE1_tag;
       L348 = ((FILE1) L349);
       r61 = L348;
       
       SATTR(r61,fp,stdout);
       ret_val163 = r61;
       FILE_plus_STR(ret_val163, plus_s50);
       ret_val162 = plus_self85;
       plus_self86 = ret_val162;
       plus_s51 = file1;
       stdout_self49 = ((FILE1) NULL);
       L352=ZALLOC(sizeof(struct FILE1_struct));
       if (L352==NULL) FATAL("Unable to allocate more memory");
       ((OB)L352)->header.tag=FILE1_tag;
       L351 = ((FILE1) L352);
       r62 = L351;
       
       SATTR(r62,fp,stdout);
       ret_val165 = r62;
       FILE_plus_STR(ret_val165, plus_s51);
       ret_val164 = plus_self86;
       plus_self87 = ret_val164;
       plus_s52 = ((STR) &name189);
       stdout_self50 = ((FILE1) NULL);
       L355=ZALLOC(sizeof(struct FILE1_struct));
       if (L355==NULL) FATAL("Unable to allocate more memory");
       ((OB)L355)->header.tag=FILE1_tag;
       L354 = ((FILE1) L355);
       r63 = L354;
       
       SATTR(r63,fp,stdout);
       ret_val167 = r63;
       FILE_plus_STR(ret_val167, plus_s52);
       ret_val166 = plus_self87;
       plus_self88 = ret_val166;
       plus_s53 = ((dSTR) cl);
       stdout_self51 = ((FILE1) NULL);
       L358=ZALLOC(sizeof(struct FILE1_struct));
       if (L358==NULL) FATAL("Unable to allocate more memory");
       ((OB)L358)->header.tag=FILE1_tag;
       L357 = ((FILE1) L358);
       r64 = L357;
       
       SATTR(r64,fp,stdout);
       ret_val169 = r64;
       FILE_plus_dSTR(ret_val169, plus_s53);
       ret_val168 = plus_self88;
       plus_self89 = ret_val168;
       plus_s54 = ((STR) &wasreached);
       stdout_self52 = ((FILE1) NULL);
       L362=ZALLOC(sizeof(struct FILE1_struct));
       if (L362==NULL) FATAL("Unable to allocate more memory");
       ((OB)L362)->header.tag=FILE1_tag;
       L360 = ((FILE1) L362);
       r65 = L360;
       
       SATTR(r65,fp,stdout);
       ret_val170 = r65;
       FILE_plus_STR(ret_val170, plus_s54);
      }
      L3641_=ARR((STR)file1,0); 
      L365 = L3641_;
      L3661_=L365=='/'; 
      if (L3661_) {
       plus_self90 = other1;
       plus_arg13 = ' ';
       str_self13 = plus_arg13;
       create_self36 = ((STR) NULL);
       create_c13 = str_self13;
       L368 = 1;
       L370=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L368)*sizeof(CHAR);
       L369=ZALLOC_LEAF_BIG(L370);
       if (L369==NULL) FATAL("Unable to allocate more memory");
       memset(L369,0,L370);
       ((OB)L369)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L369)->header.destroyed=0;
#endif

       L367 = ((STR) L369);
       L367->asize = L368;
       r66 = L367;
       SARR((STR)r66,0,create_c13); 
       ;
       ret_val173 = r66;
       ret_val172 = ret_val173;
       s17 = ret_val172;
       ret_val171 = STR_ap1077157958(plus_self90, s17, TRUE);
       plus_self91 = ret_val171;
       plus_sarg22 = file1;
       ret_val174 = STR_ap2004550586(plus_self91, plus_sarg22);
       other1 = ret_val174;
      }
      else {
       L3721_=ARR((STR)file1,0); 
       L373 = L3721_;
       L3741_=L373=='-'; 
       if (L3741_) {
        plus_self92 = other1;
        plus_arg14 = ' ';
        str_self14 = plus_arg14;
        create_self37 = ((STR) NULL);
        create_c14 = str_self14;
        L376 = 1;
        L378=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L376)*sizeof(CHAR);
        L377=ZALLOC_LEAF_BIG(L378);
        if (L377==NULL) FATAL("Unable to allocate more memory");
        memset(L377,0,L378);
        ((OB)L377)->header.tag=STR_tag;
#ifdef DESTROY_CHK

          ((OB)L377)->header.destroyed=0;
#endif

        L375 = ((STR) L377);
        L375->asize = L376;
        r67 = L375;
        SARR((STR)r67,0,create_c14); 
        ;
        ret_val177 = r67;
        ret_val176 = ret_val177;
        s18 = ret_val176;
        ret_val175 = STR_ap1077157958(plus_self92, s18, TRUE);
        plus_self93 = ret_val175;
        plus_sarg23 = file1;
        ret_val178 = STR_ap2004550586(plus_self93, plus_sarg23);
        other1 = ret_val178;
       }
       else {
        plus_self94 = other1;
        plus_sarg24 = ((STR) &name190);
        ret_val179 = STR_ap2004550586(plus_self94, plus_sarg24);
        plus_self95 = ret_val179;
        plus_sarg25 = file1;
        ret_val180 = STR_ap2004550586(plus_self95, plus_sarg25);
        other1 = ret_val180;
       }
      }
     }
    }
    after_loop7: ;
   }
  }
 }
 after_loop6: ;
 {
  struct FLISTS1171631134_frame_struct other10_0;
FLISTS1171631134_frame noname6 = &other10_0;
  L101 = ATTR(self,archive_files);
  noname6->self = L101;
  noname6->state = 0;
  while (1) {
   L380 = FLISTS1171631134(noname6);
   if (noname6->state == -1) {
    goto after_loop8;
   }
   s2 = L380;
   L3811_=ARR((STR)s2,0); 
   L382 = L3811_;
   L3831_=L382=='/'; 
   if (L3831_) {
    plus_self96 = other1;
    plus_arg15 = ' ';
    str_self15 = plus_arg15;
    create_self38 = ((STR) NULL);
    create_c15 = str_self15;
    L385 = 1;
    L387=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L385)*sizeof(CHAR);
    L386=ZALLOC_LEAF_BIG(L387);
    if (L386==NULL) FATAL("Unable to allocate more memory");
    memset(L386,0,L387);
    ((OB)L386)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L386)->header.destroyed=0;
#endif

    L384 = ((STR) L386);
    L384->asize = L385;
    r68 = L384;
    SARR((STR)r68,0,create_c15); 
    ;
    ret_val183 = r68;
    ret_val182 = ret_val183;
    s19 = ret_val182;
    ret_val181 = STR_ap1077157958(plus_self96, s19, TRUE);
    plus_self97 = ret_val181;
    plus_sarg26 = s2;
    ret_val184 = STR_ap2004550586(plus_self97, plus_sarg26);
    other1 = ret_val184;
   }
   else {
    plus_self98 = other1;
    plus_sarg27 = ((STR) &name190);
    ret_val185 = STR_ap2004550586(plus_self98, plus_sarg27);
    plus_self99 = ret_val185;
    plus_sarg28 = s2;
    ret_val186 = STR_ap2004550586(plus_self99, plus_sarg28);
    other1 = ret_val186;
   }
  }
 }
 after_loop8: ;
 plus_self100 = syscom;
 plus_sarg29 = ((STR) &dOTHER);
 ret_val187 = STR_ap2004550586(plus_self100, plus_sarg29);
 syscom = ret_val187;
 plus_self101 = syscom;
 plus_sarg30 = ((STR) &dLIBS);
 ret_val188 = STR_ap2004550586(plus_self101, plus_sarg30);
 syscom = ret_val188;
 suffix_length = 0;
 {
  BOOL f_L3891_ = TRUE;
  L390 = STR_sizerINT(ATTR(self,executable1));
  L3911_=INTMINUS(L390,1); 
  L121 = L3911_;
  L131 = 0;
  L3891_=L121+1;L3891_m=L131; 
  while (1) {
   if(L3891_--<=L3891_m)  goto after_loop9; 
   i2 = L3891_;
   L393 = ATTR(self,executable1);
   L3941_=ARR((STR)L393,i2); 
   L395 = L3941_;
   L3961_=L395=='/'; 
   if (L3961_) {
    goto after_loop9;
   }
   L3971_=INTPLUS(suffix_length,1); 
   suffix_length = L3971_;
  }
 }
 after_loop9: ;
 exec_name = STR_tail_INTrSTR(ATTR(self,executable1), suffix_length);
 create_self39 = ((FSTR) NULL);
 L399 = 16;
 L401=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L399)*sizeof(CHAR);
 L400=ZALLOC_LEAF_BIG(L401);
 if (L400==NULL) FATAL("Unable to allocate more memory");
 memset(L400,0,L401);
 ((OB)L400)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L400)->header.destroyed=0;
#endif

 L398 = ((FSTR) L400);
 L398->asize = L399;
 ret_val189 = L398;
 comp_opt = ret_val189;
 {
  struct FLISTS1171631134_frame_struct other12_0;
FLISTS1171631134_frame noname7 = &other12_0;
  L141 = ATTR(ATTR(self,prog),platforms);
  noname7->self = L141;
  noname7->state = 0;
  while (1) {
   open_for_read_se = ((BFILE) NULL);
   L402 = FLISTS1171631134(noname7);
   if (noname7->state == -1) {
    goto after_loop10;
   }
   plus_self102 = L402;
   plus_sarg31 = ((STR) &Maketargetinc);
   ret_val190 = STR_ap2004550586(plus_self102, plus_sarg31);
   open_for_read_nm = ret_val190;
   L404=ZALLOC(sizeof(struct BFILE_struct));
   if (L404==NULL) FATAL("Unable to allocate more memory");
   ((OB)L404)->header.tag=BFILE_tag;
   L403 = ((BFILE) L404);
   r69 = L403;
   L405 = open_for_read_nm;
   L406 = ((STR) &rb);
   SATTR(r69,fp,fopen(((L405==NULL)?NULL:L405->arr_part), ((L406==NULL)?NULL:L406->arr_part)));
   ret_val191 = r69;
   make_include_fil = ret_val191;
   L408 = (make_include_fil==((BFILE) NULL));
   L4091_=!(L408); 
   if (L4091_) {
    error_self = make_include_fil;
    if ((ATTR(error_self,fp)==((EXT_OB) NULL))) {
     L410 = TRUE;
    } else {
     L410 = ferror(ATTR(error_self,fp));
    }
    ret_val192 = L410;
    L411 = ret_val192;
    L4121_=!(L411); 
    L407 = L4121_;
   } else {
    L407 = FALSE;
   }
   if (L407) {
    L413 = FSTR_p1410513982(comp_opt, BFILE_fstrrFSTR(make_include_fil));
    comp_opt = FSTR_p1752847026(L413, ((STR) &name163));
    close_self = make_include_fil;
    fclose(ATTR(close_self,fp));
   }
  }
 }
 after_loop10: ;
 L414 = FSTR_p1752847026(comp_opt, ((STR) &SHOME));
 plus_self103 = FSTR_p1752847026(L414, ATTR(self,comp_home));
 plus_c = '\n';
 ret_val193 = FSTR_p399773021(plus_self103, plus_c);
 L415 = FSTR_p1752847026(ret_val193, ((STR) &CFLAGS));
 plus_self104 = FSTR_p1752847026(L415, allflags);
 plus_c1 = '\n';
 ret_val194 = FSTR_p399773021(plus_self104, plus_c1);
 L416 = FSTR_p1752847026(ret_val194, ((STR) &CC));
 plus_self105 = FSTR_p1752847026(L416, CONFIG1397479570(ATTR(ATTR(self,prog),config), ((STR) &C_COMPILER), 0));
 plus_c2 = '\n';
 ret_val195 = FSTR_p399773021(plus_self105, plus_c2);
 comp_opt = FSTR_p1752847026(ret_val195, ((STR) &HDRsatherhtagsh));
 plus_self106 = ATTR(self,comp_options);
 plus_s55 = ((dSTR) comp_opt);
 L417 = ATTR(plus_self106,ntext);
 L418 = plus_s55;
 SATTR(plus_self106,ntext,FSTR_p1752847026(L417, (*dSTR_strrSTR[TAG(L418)])(L418)));
 create_self40 = ((FSTR) NULL);
 L420 = 16;
 L423=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L420)*sizeof(CHAR);
 L422=ZALLOC_LEAF_BIG(L423);
 if (L422==NULL) FATAL("Unable to allocate more memory");
 memset(L422,0,L423);
 ((OB)L422)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L422)->header.destroyed=0;
#endif

 L419 = ((FSTR) L422);
 L419->asize = L420;
 ret_val196 = L419;
 L424 = FSTR_p1410513982(ret_val196, comp_opt);
 L425 = FSTR_p1752847026(L424, ((STR) &LIBS));
 plus_self107 = FSTR_p1752847026(L425, libs);
 plus_c3 = '\n';
 ret_val197 = FSTR_p399773021(plus_self107, plus_c3);
 L426 = FSTR_p1752847026(ret_val197, ((STR) &CS1));
 L427 = FSTR_p1752847026(L426, exec_name);
 plus_self108 = FSTR_p1752847026(L427, CONFIG1397479570(ATTR(ATTR(self,prog),config), ((STR) &EXEC_SUFFIX), 0));
 plus_c4 = '\n';
 ret_val198 = FSTR_p399773021(plus_self108, plus_c4);
 L428 = FSTR_p1752847026(ret_val198, ((STR) &OBJ));
 plus_self109 = FSTR_p1752847026(L428, obfiles);
 plus_c5 = '\n';
 ret_val199 = FSTR_p399773021(plus_self109, plus_c5);
 L429 = FSTR_p1752847026(ret_val199, ((STR) &OTHER));
 plus_self110 = FSTR_p1752847026(L429, other1);
 plus_c6 = '\n';
 ret_val200 = FSTR_p399773021(plus_self110, plus_c6);
 link_opt = ret_val200;
 plus_self111 = ATTR(self,link_options);
 plus_s56 = ((dSTR) link_opt);
 L430 = ATTR(plus_self111,ntext);
 L431 = plus_s56;
 SATTR(plus_self111,ntext,FSTR_p1752847026(L430, (*dSTR_strrSTR[TAG(L431)])(L431)));
 plus_self112 = ATTR(self,makefile);
 plus_s57 = ((dSTR) link_opt);
 L432 = ATTR(plus_self112,ntext);
 L433 = plus_s57;
 SATTR(plus_self112,ntext,FSTR_p1752847026(L432, (*dSTR_strrSTR[TAG(L433)])(L433)));
 plus_self113 = ATTR(self,makefile);
 plus_s58 = ((dSTR) ((STR) &dCSLIN1806998169));
 L434 = ATTR(plus_self113,ntext);
 L435 = plus_s58;
 SATTR(plus_self113,ntext,FSTR_p1752847026(L434, (*dSTR_strrSTR[TAG(L435)])(L435)));
 plus_self114 = ATTR(self,makefile);
 plus_s59 = ((dSTR) ((STR) &name37));
 L436 = (plus_s59==((dSTR) NULL));
 L4371_=!(L436); 
 if (L4371_) {
  L438 = ATTR(plus_self114,ntext);
  L439 = plus_s59;
  SATTR(plus_self114,ntext,FSTR_p1752847026(L438, (*dSTR_strrSTR[TAG(L439)])(L439)));
 }
 ret_val201 = plus_self114;
 plus_self115 = ret_val201;
 plus_s60 = ((dSTR) syscom);
 L440 = (plus_s60==((dSTR) NULL));
 L4411_=!(L440); 
 if (L4411_) {
  L442 = ATTR(plus_self115,ntext);
  L443 = plus_s60;
  SATTR(plus_self115,ntext,FSTR_p1752847026(L442, (*dSTR_strrSTR[TAG(L443)])(L443)));
 }
 ret_val202 = plus_self115;
 plus_self116 = ret_val202;
 plus_s61 = ((dSTR) ((STR) &name163));
 L444 = ATTR(plus_self116,ntext);
 L445 = plus_s61;
 SATTR(plus_self116,ntext,FSTR_p1752847026(L444, (*dSTR_strrSTR[TAG(L445)])(L445)));
 plus_self117 = ATTR(self,link_options);
 plus_s62 = ((dSTR) ((STR) &syscom1));
 L446 = (plus_s62==((dSTR) NULL));
 L4471_=!(L446); 
 if (L4471_) {
  L448 = ATTR(plus_self117,ntext);
  L449 = plus_s62;
  SATTR(plus_self117,ntext,FSTR_p1752847026(L448, (*dSTR_strrSTR[TAG(L449)])(L449)));
 }
 ret_val203 = plus_self117;
 plus_self118 = ret_val203;
 plus_s63 = ((dSTR) syscom);
 L450 = (plus_s63==((dSTR) NULL));
 L4521_=!(L450); 
 if (L4521_) {
  L453 = ATTR(plus_self118,ntext);
  L454 = plus_s63;
  SATTR(plus_self118,ntext,FSTR_p1752847026(L453, (*dSTR_strrSTR[TAG(L454)])(L454)));
 }
 ret_val204 = plus_self118;
 plus_self119 = ret_val204;
 L456=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L456==NULL) FATAL("Unable to allocate more memory");
 memset(L456,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L456)->header.tag=CHAR_tag;
 L455 = (dSTR)((CHAR_boxed) L456);
 ((CHAR_boxed) L455)->immutable_part = '\n';
 plus_s64 = L455;
 L457 = ATTR(plus_self119,ntext);
 L458 = plus_s64;
 SATTR(plus_self119,ntext,FSTR_p1752847026(L457, (*dSTR_strrSTR[TAG(L458)])(L458)));
 plus_self120 = ATTR(self,makefile);
 plus_s65 = ((dSTR) ((STR) &dOBJCO818039632));
 L459 = ATTR(plus_self120,ntext);
 L460 = plus_s65;
 SATTR(plus_self120,ntext,FSTR_p1752847026(L459, (*dSTR_strrSTR[TAG(L460)])(L460)));
 {
  struct FSETSTR_eltbrSTR_frame_struct other13_0;
FSETSTR_eltbrSTR_frame noname8 = &other13_0;
  L151 = ATTR(self,c_files);
  noname8->self = L151;
  noname8->state = 0;
  while (1) {
   L461 = FSETSTR_eltbrSTR(noname8);
   if (noname8->state == -1) {
    goto after_loop11;
   }
   s3 = L461;
   replace_suffix_s1 = s3;
   replace_suffix_o1 = CONFIG1397479570(ATTR(ATTR(self,prog),config), ((STR) &C_EXT), 0);
   replace_suffix_r1 = CONFIG1397479570(ATTR(ATTR(self,prog),config), ((STR) &OBJECT_EXT), 0);
   L462 = STR_lengthrINT(replace_suffix_s1);
   L463 = STR_sizerINT(replace_suffix_o1);
   L4641_=INTMINUS(L462,L463); 
   plus_self121 = STR_head_INTrSTR(replace_suffix_s1, L4641_);
   plus_sarg32 = replace_suffix_r1;
   ret_val206 = STR_ap2004550586(plus_self121, plus_sarg32);
   ret_val205 = ret_val206;
   o11 = ret_val205;
   L465 = STR_se1633361857(o11, '/');
   L4661_=INTPLUS(L465,1); 
   o11 = STR_su1627796404(o11, L4661_);
   L4671_=ARR((STR)s3,0); 
   L468 = L4671_;
   L4691_=L468=='/'; 
   L470 = L4691_;
   L4711_=!(L470); 
   if (L4711_) {
    plus_self122 = ((STR) &name46);
    plus_sarg33 = s3;
    ret_val207 = STR_ap2004550586(plus_self122, plus_sarg33);
    s3 = ret_val207;
   }
   plus_self123 = ATTR(self,makefile);
   plus_s66 = ((dSTR) ((STR) &name9));
   L472 = (plus_s66==((dSTR) NULL));
   L4731_=!(L472); 
   if (L4731_) {
    L474 = ATTR(plus_self123,ntext);
    L475 = plus_s66;
    SATTR(plus_self123,ntext,FSTR_p1752847026(L474, (*dSTR_strrSTR[TAG(L475)])(L475)));
   }
   ret_val208 = plus_self123;
   plus_self124 = ret_val208;
   plus_s67 = ((dSTR) o11);
   L476 = (plus_s67==((dSTR) NULL));
   L4771_=!(L476); 
   if (L4771_) {
    L478 = ATTR(plus_self124,ntext);
    L479 = plus_s67;
    SATTR(plus_self124,ntext,FSTR_p1752847026(L478, (*dSTR_strrSTR[TAG(L479)])(L479)));
   }
   ret_val209 = plus_self124;
   plus_self125 = ret_val209;
   plus_s68 = ((dSTR) ((STR) &name20));
   L480 = (plus_s68==((dSTR) NULL));
   L4811_=!(L480); 
   if (L4811_) {
    L482 = ATTR(plus_self125,ntext);
    L483 = plus_s68;
    SATTR(plus_self125,ntext,FSTR_p1752847026(L482, (*dSTR_strrSTR[TAG(L483)])(L483)));
   }
   ret_val210 = plus_self125;
   plus_self126 = ret_val210;
   plus_s69 = ((dSTR) s3);
   L484 = (plus_s69==((dSTR) NULL));
   L4851_=!(L484); 
   if (L4851_) {
    L486 = ATTR(plus_self126,ntext);
    L487 = plus_s69;
    SATTR(plus_self126,ntext,FSTR_p1752847026(L486, (*dSTR_strrSTR[TAG(L487)])(L487)));
   }
   ret_val211 = plus_self126;
   plus_self127 = ret_val211;
   plus_s70 = ((dSTR) ((STR) &COMPIL44583114));
   L488 = (plus_s70==((dSTR) NULL));
   L4891_=!(L488); 
   if (L4891_) {
    L490 = ATTR(plus_self127,ntext);
    L491 = plus_s70;
    SATTR(plus_self127,ntext,FSTR_p1752847026(L490, (*dSTR_strrSTR[TAG(L491)])(L491)));
   }
   ret_val212 = plus_self127;
   plus_self128 = ret_val212;
   plus_s71 = ((dSTR) s3);
   L492 = (plus_s71==((dSTR) NULL));
   L4931_=!(L492); 
   if (L4931_) {
    L494 = ATTR(plus_self128,ntext);
    L495 = plus_s71;
    SATTR(plus_self128,ntext,FSTR_p1752847026(L494, (*dSTR_strrSTR[TAG(L495)])(L495)));
   }
   ret_val213 = plus_self128;
   plus_self129 = ret_val213;
   plus_s72 = ((dSTR) ((STR) &name9));
   L496 = ATTR(plus_self129,ntext);
   L497 = plus_s72;
   SATTR(plus_self129,ntext,FSTR_p1752847026(L496, (*dSTR_strrSTR[TAG(L497)])(L497)));
  }
 }
 after_loop11: ;
 CODE_F2105979088(((CODE_FILE) NULL));
 plus_self130 = ((STR) &cd);
 plus_sarg34 = ATTR(self,code_dir);
 ret_val214 = STR_ap2004550586(plus_self130, plus_sarg34);
 plus_self131 = ret_val214;
 plus_sarg35 = CONFIG1397479570(ATTR(ATTR(self,prog),config), ((STR) &SHELL_SEP), 0);
 ret_val215 = STR_ap2004550586(plus_self131, plus_sarg35);
 plus_self132 = ret_val215;
 plus_sarg36 = ((STR) &name10);
 ret_val216 = STR_ap2004550586(plus_self132, plus_sarg36);
 plus_self133 = ret_val216;
 plus_sarg37 = CONFIG1397479570(ATTR(ATTR(self,prog),config), ((STR) &MAKE_COMMAND), 0);
 ret_val217 = STR_ap2004550586(plus_self133, plus_sarg37);
 syscom = ret_val217;
 if (ATTR(ATTR(self,prog),verbose)) {
  plus_self134 = syscom;
  plus_arg16 = ' ';
  str_self16 = plus_arg16;
  create_self41 = ((STR) NULL);
  create_c16 = str_self16;
  L499 = 1;
  L502=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L499)*sizeof(CHAR);
  L500=ZALLOC_LEAF_BIG(L502);
  if (L500==NULL) FATAL("Unable to allocate more memory");
  memset(L500,0,L502);
  ((OB)L500)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L500)->header.destroyed=0;
#endif

  L498 = ((STR) L500);
  L498->asize = L499;
  r70 = L498;
  SARR((STR)r70,0,create_c16); 
  ;
  ret_val220 = r70;
  ret_val219 = ret_val220;
  s20 = ret_val219;
  ret_val218 = STR_ap1077157958(plus_self134, s20, TRUE);
  plus_self135 = ret_val218;
  plus_sarg38 = CONFIG1397479570(ATTR(ATTR(self,prog),config), ((STR) &MAKE_V183621898), 1);
  ret_val221 = STR_ap2004550586(plus_self135, plus_sarg38);
  syscom = ret_val221;
 }
 else {
  plus_self136 = syscom;
  plus_arg17 = ' ';
  str_self17 = plus_arg17;
  create_self42 = ((STR) NULL);
  create_c17 = str_self17;
  L505 = 1;
  L507=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L505)*sizeof(CHAR);
  L506=ZALLOC_LEAF_BIG(L507);
  if (L506==NULL) FATAL("Unable to allocate more memory");
  memset(L506,0,L507);
  ((OB)L506)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L506)->header.destroyed=0;
#endif

  L504 = ((STR) L506);
  L504->asize = L505;
  r71 = L504;
  SARR((STR)r71,0,create_c17); 
  ;
  ret_val224 = r71;
  ret_val223 = ret_val224;
  s21 = ret_val223;
  ret_val222 = STR_ap1077157958(plus_self136, s21, TRUE);
  plus_self137 = ret_val222;
  plus_sarg39 = CONFIG1397479570(ATTR(ATTR(self,prog),config), ((STR) &MAKE_V183621898), 0);
  ret_val225 = STR_ap2004550586(plus_self137, plus_sarg39);
  syscom = ret_val225;
 }
 if (ATTR(self,only_C)) {
  L509 = TRUE;
 } else {
  L509 = ATTR(ATTR(self,prog),benchmark);
 }
 if (L509) {
  if (ATTR(ATTR(self,prog),prolix)) {
   create_self43 = ((OUT) NULL);
   ret_val226 = create_self43;
   plus_self138 = ret_val226;
   plus_s73 = ((STR) &Skippingmake);
   stdout_self53 = ((FILE1) NULL);
   L511=ZALLOC(sizeof(struct FILE1_struct));
   if (L511==NULL) FATAL("Unable to allocate more memory");
   ((OB)L511)->header.tag=FILE1_tag;
   L510 = ((FILE1) L511);
   r72 = L510;
   
   SATTR(r72,fp,stdout);
   ret_val227 = r72;
   FILE_plus_STR(ret_val227, plus_s73);
  }
 }
 else {
  if (ATTR(ATTR(self,prog),prolix)) {
   create_self44 = ((OUT) NULL);
   ret_val228 = create_self44;
   plus_self139 = ret_val228;
   L514=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
   if (L514==NULL) FATAL("Unable to allocate more memory");
   memset(L514,0,sizeof(struct CHAR_boxed_struct));
   ((OB)L514)->header.tag=CHAR_tag;
   L513 = (dSTR)((CHAR_boxed) L514);
   ((CHAR_boxed) L513)->immutable_part = '\n';
   plus_s74 = L513;
   stdout_self54 = ((FILE1) NULL);
   L516=ZALLOC(sizeof(struct FILE1_struct));
   if (L516==NULL) FATAL("Unable to allocate more memory");
   ((OB)L516)->header.tag=FILE1_tag;
   L515 = ((FILE1) L516);
   r73 = L515;
   
   SATTR(r73,fp,stdout);
   ret_val230 = r73;
   FILE_plus_dSTR(ret_val230, plus_s74);
   ret_val229 = plus_self139;
   plus_self140 = ret_val229;
   plus_s75 = syscom;
   stdout_self55 = ((FILE1) NULL);
   L519=ZALLOC(sizeof(struct FILE1_struct));
   if (L519==NULL) FATAL("Unable to allocate more memory");
   ((OB)L519)->header.tag=FILE1_tag;
   L518 = ((FILE1) L519);
   r74 = L518;
   
   SATTR(r74,fp,stdout);
   ret_val232 = r74;
   FILE_plus_STR(ret_val232, plus_s75);
   ret_val231 = plus_self140;
   plus_self141 = ret_val231;
   L522=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
   if (L522==NULL) FATAL("Unable to allocate more memory");
   memset(L522,0,sizeof(struct CHAR_boxed_struct));
   ((OB)L522)->header.tag=CHAR_tag;
   L521 = (dSTR)((CHAR_boxed) L522);
   ((CHAR_boxed) L521)->immutable_part = '\n';
   plus_s76 = L521;
   stdout_self56 = ((FILE1) NULL);
   L524=ZALLOC(sizeof(struct FILE1_struct));
   if (L524==NULL) FATAL("Unable to allocate more memory");
   ((OB)L524)->header.tag=FILE1_tag;
   L523 = ((FILE1) L524);
   r75 = L523;
   
   SATTR(r75,fp,stdout);
   ret_val233 = r75;
   FILE_plus_dSTR(ret_val233, plus_s76);
  }
  system_self = ((UNIX) NULL);
  system_s = syscom;
  L526 = system_s;
  ret_val234 = system(((L526==NULL)?NULL:L526->arr_part));
  L527 = ret_val234;
  L5281_=(L527)==(0); 
  L529 = L5281_;
  L5301_=!(L529); 
  if (L5301_) {
   barf_self = self;
   barf_msg = ((STR) &Makefailed);
   barf_self1 = ATTR(barf_self,prog);
   barf_msg1 = barf_msg;
   barf_at_self = barf_self1;
   barf_at_msg = barf_msg1;
   barf_at_at = ((dPROG_ERR) NULL);
   PROG_e176405615(barf_at_self, barf_at_at);
   plus_self142 = ((STR) &Intern28965746);
   plus_sarg40 = barf_at_msg;
   ret_val235 = STR_ap2004550586(plus_self142, plus_sarg40);
   PROG_err_STR(barf_at_self, ret_val235);
   exit_self = ((UNIX) NULL);
   exit_code = 1;
   exit(exit_code);
  }
 }
 L532 = ATTR(self,gen_c);
 L5331_=!(L532); 
 if (L5331_) {
  L534 = ATTR(ATTR(self,prog),benchmark);
  L5351_=!(L534); 
  L531 = L5351_;
 } else {
  L531 = FALSE;
 }
 if (L531) {
  system_self1 = ((UNIX) NULL);
  plus_self143 = ((STR) &rmfr);
  plus_sarg41 = ATTR(self,code_dir);
  ret_val236 = STR_ap2004550586(plus_self143, plus_sarg41);
  system_s1 = ret_val236;
  L536 = system_s1;
  ret_val237 = system(((L536==NULL)?NULL:L536->arr_part));
  dummy1 = ret_val237;
 }
 L538 = ATTR(self,no_destroy);
 L5391_=!(L538); 
 if (L5391_) {
  L540 = ATTR(self,no_destroy_lock);
  L5421_=!(L540); 
  L537 = L5421_;
 } else {
  L537 = FALSE;
 }
 if (L537) {
  SYSDESTROY(self); 
  ;
 }
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_g1348388497(CGEN self) {
 FLISTAM_ROUT_DEF L11;
 AM_ROUT_DEF abs_rout;
 FSETdTP gh = ((FSETdTP) NULL);
 dTP fst;
 FSETdTP L21;
 dTP e;
 SIG real_sig;
 FLISTAM_ROUT_DEF L31;
 AM_ROUT_DEF abs_iter;
 FSETdTP gh1 = ((FSETdTP) NULL);
 dTP fst1;
 FSETdTP L41;
 dTP e1;
 SIG real_sig1;
 TP_GRAPH_ABS_DES des_of_self;
 TP_CLASS des_of_tp;
 FSETdTP ret_val;
 CGEN make_sure_emitte;
 SIG make_sure_emitte1;
 AM_ROUT_DEF ard;
 TP_GRAPH_ABS_DES des_of_self1;
 TP_CLASS des_of_tp1;
 FSETdTP ret_val1;
 CGEN make_sure_emitte2;
 SIG make_sure_emitte3;
 AM_ROUT_DEF ard1;
 AM_ROUT_DEF L5;
 dTP L6;
 dTP L7;
 BOOL L8;
 BOOL L91_;
 INT L10;
 BOOL L121_;
 BOOL L13;
 BOOL L141_;
 dTP L15;
 IFC L16;
 BOOL L17;
 BOOL L181_;
 BOOL L19;
 BOOL L201_;
 AM_ROUT_DEF L22;
 dTP L23;
 dTP L24;
 BOOL L25;
 BOOL L261_;
 INT L27;
 BOOL L281_;
 BOOL L29;
 BOOL L301_;
 dTP L32;
 IFC L33;
 BOOL L34;
 BOOL L351_;
 BOOL L36;
 BOOL L371_;
 while (1) {
  {
   struct FLISTA2121868826_frame_struct other2_0;
FLISTA2121868826_frame noname1 = &other2_0;
   L11 = ATTR(self,abstract_routs);
   noname1->self = L11;
   noname1->state = 0;
   while (1) {
    L5 = FLISTA2121868826(noname1);
    if (noname1->state == -1) {
     goto after_loop1;
    }
    abs_rout = L5;
    fst = ATTR(ATTR(abs_rout,sig1),tp);
    switch (TAG(fst)) {
     case TP_CLASS_tag:
      des_of_self = ATTR(ATTR(self,prog),tp_graph_abs_des);
      des_of_tp = ((TP_CLASS) fst);
      ret_val = FMAPTP861983338(ATTR(des_of_self,tbl), des_of_tp);
      gh = ret_val; break;
     default: ;
      FATAL("No applicable type in typecase\nin CGEN::generate_dispatch_rout_and_iters\n./Back/cgen.sa:2454:20");
    }
    {
     struct FSETdTP_eltbrdTP_frame_struct other3_0;
FSETdTP_eltbrdTP_frame noname2 = &other3_0;
     L21 = gh;
     noname2->self = L21;
     noname2->state = 0;
     while (1) {
      L6 = FSETdTP_eltbrdTP(noname2);
      if (noname2->state == -1) {
       goto after_loop2;
      }
      e = L6;
      L7 = e;
      L8 = (*dTP_is1553335918[TAG(L7)])(L7);
      L91_=!(L8); 
      if (L91_) {
       L10 = FMAPdT165518335(ATTR(self,tags), e);
       L121_=(L10)==(0); 
       L13 = L121_;
       L141_=!(L13); 
       if (L141_) {
        L15 = e;
        L16 = (*dTP_ifcrIFC[TAG(L15)])(L15);
        real_sig = IFC_si2085565042(L16, ATTR(abs_rout,sig1));
        L17 = (real_sig==((SIG) NULL));
        L181_=!(L17); 
        if (L181_) {
         make_sure_emitte = self;
         make_sure_emitte1 = real_sig;
         ard = FMAPSI2016001247(ATTR(make_sure_emitte,not_emitted), make_sure_emitte1);
         L19 = (ard==((AM_ROUT_DEF) NULL));
         L201_=!(L19); 
         if (L201_) {
          SATTR(make_sure_emitte,leftovers,FSETAM2044524049(ATTR(make_sure_emitte,leftovers), ard));
          SATTR(make_sure_emitte,not_emitted,FMAPSI1625125906(ATTR(make_sure_emitte,not_emitted), make_sure_emitte1));
         }
        }
       }
      }
     }
    }
    after_loop2: ;
   }
  }
  after_loop1: ;
  {
   struct FLISTA2121868826_frame_struct other4_0;
FLISTA2121868826_frame noname3 = &other4_0;
   L31 = ATTR(self,abstract_iters);
   noname3->self = L31;
   noname3->state = 0;
   while (1) {
    L22 = FLISTA2121868826(noname3);
    if (noname3->state == -1) {
     goto after_loop3;
    }
    abs_iter = L22;
    fst1 = ATTR(ATTR(abs_iter,sig1),tp);
    switch (TAG(fst1)) {
     case TP_CLASS_tag:
      des_of_self1 = ATTR(ATTR(self,prog),tp_graph_abs_des);
      des_of_tp1 = ((TP_CLASS) fst1);
      ret_val1 = FMAPTP861983338(ATTR(des_of_self1,tbl), des_of_tp1);
      gh1 = ret_val1; break;
     default: ;
      FATAL("No applicable type in typecase\nin CGEN::generate_dispatch_rout_and_iters\n./Back/cgen.sa:2475:20");
    }
    {
     struct FSETdTP_eltbrdTP_frame_struct other5_0;
FSETdTP_eltbrdTP_frame noname4 = &other5_0;
     L41 = gh1;
     noname4->self = L41;
     noname4->state = 0;
     while (1) {
      L23 = FSETdTP_eltbrdTP(noname4);
      if (noname4->state == -1) {
       goto after_loop4;
      }
      e1 = L23;
      L24 = e1;
      L25 = (*dTP_is1553335918[TAG(L24)])(L24);
      L261_=!(L25); 
      if (L261_) {
       L27 = FMAPdT165518335(ATTR(self,tags), e1);
       L281_=(L27)==(0); 
       L29 = L281_;
       L301_=!(L29); 
       if (L301_) {
        L32 = e1;
        L33 = (*dTP_ifcrIFC[TAG(L32)])(L32);
        real_sig1 = IFC_si2085565042(L33, ATTR(abs_iter,sig1));
        L34 = (real_sig1==((SIG) NULL));
        L351_=!(L34); 
        if (L351_) {
         make_sure_emitte2 = self;
         make_sure_emitte3 = real_sig1;
         ard1 = FMAPSI2016001247(ATTR(make_sure_emitte2,not_emitted), make_sure_emitte3);
         L36 = (ard1==((AM_ROUT_DEF) NULL));
         L371_=!(L36); 
         if (L371_) {
          SATTR(make_sure_emitte2,leftovers,FSETAM2044524049(ATTR(make_sure_emitte2,leftovers), ard1));
          SATTR(make_sure_emitte2,not_emitted,FMAPSI1625125906(ATTR(make_sure_emitte2,not_emitted), make_sure_emitte3));
         }
        }
       }
      }
     }
    }
    after_loop4: ;
   }
  }
  after_loop3: ;
  if (CGEN_a952489530(self)) {
  }
  else {
   goto after_loop;
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_g1356172775(CGEN self) {
 STR s;
 FSTR print_dec = ((FSTR) NULL);
 ARRAYSTR ts;
 ARRAYdTP tags_keys;
 FMAPdTPINT L11;
 ARRAYdTP L21;
 FMAPdTPINT L31;
 TUPdTPINT p = TUPdTPINT_zero;
 ARRAYSTR L41;
 ARRAYSTR L51;
 ARRAYdTP L61;
 dTP p_key;
 AM_STR_CONST dummy1;
 ARRAYSTR L71;
 ARRAYSTR L81;
 dTP tp;
 ARRAYdTP L91;
 STR tpstr;
 CGEN tag_for_self;
 dTP tag_for_tp;
 STR ret_val;
 STR res;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val1;
 CGEN forbid_self;
 STR forbid_s;
 MANGLE forbid_self1;
 STR forbid_s1;
 CGEN tag_for_self1;
 dTP tag_for_tp1;
 STR ret_val2;
 STR res1;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val3;
 CGEN forbid_self2;
 STR forbid_s2;
 MANGLE forbid_self3;
 STR forbid_s3;
 CGEN tag_for_self2;
 dTP tag_for_tp2;
 STR ret_val4;
 STR res2;
 CGEN mang_self2;
 OB mang_ob2;
 STR ret_val5;
 CGEN forbid_self4;
 STR forbid_s4;
 MANGLE forbid_self5;
 STR forbid_s5;
 CGEN tag_for_self3;
 dTP tag_for_tp3;
 STR ret_val6;
 STR res3;
 CGEN mang_self3;
 OB mang_ob3;
 STR ret_val7;
 CGEN forbid_self6;
 STR forbid_s6;
 MANGLE forbid_self7;
 STR forbid_s7;
 CGEN tag_for_self4;
 dTP tag_for_tp4;
 STR ret_val8;
 STR res4;
 CGEN mang_self4;
 OB mang_ob4;
 STR ret_val9;
 CGEN forbid_self8;
 STR forbid_s8;
 MANGLE forbid_self9;
 STR forbid_s9;
 CGEN tag_for_self5;
 dTP tag_for_tp5;
 STR ret_val10;
 STR res5;
 CGEN mang_self5;
 OB mang_ob5;
 STR ret_val11;
 CGEN forbid_self10;
 STR forbid_s10;
 MANGLE forbid_self11;
 STR forbid_s11;
 FSTR str_self;
 STR ret_val12;
 CODE_FILE plus_self;
 dSTR plus_s;
 CODE_FILE plus_self1;
 dSTR plus_s1;
 FSTR create_self;
 FSTR ret_val13;
 CGEN ndefer_self;
 STR ndefer_s;
 ARRAYSTR create_self1;
 INT create_n = INT_zero;
 ARRAYSTR ret_val14;
 ARRAYdTP create_self2;
 INT create_n1 = INT_zero;
 ARRAYdTP ret_val15;
 INT L101 = INT_zero;
 ARRAYdTP sort_self;
 CODE_FILE plus_self2;
 dSTR plus_s2;
 CODE_FILE ret_val16;
 CGEN mang_self6;
 OB mang_ob6;
 STR ret_val17;
 CODE_FILE plus_self3;
 dSTR plus_s3;
 CODE_FILE ret_val18;
 CODE_FILE plus_self4;
 dSTR plus_s4;
 CODE_FILE ret_val19;
 CODE_FILE plus_self5;
 dSTR plus_s5;
 CGEN mang_self7;
 OB mang_ob7;
 STR ret_val20;
 STR plus_self6;
 STR plus_sarg;
 STR ret_val21;
 STR plus_self7;
 STR plus_sarg1;
 STR ret_val22;
 STR plus_self8;
 INT plus_arg = INT_zero;
 STR ret_val23;
 STR s1;
 INT str_self1 = INT_zero;
 STR ret_val24;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val25;
 FSTR str_self2;
 STR ret_val26;
 ARRAYSTR sort_self1;
 INT L121 = INT_zero;
 CODE_FILE plus_self9;
 dSTR plus_s6;
 INT L131 = INT_zero;
 AM_STR_CONST create_self3;
 AM_STR_CONST ret_val27;
 CGEN mang_self8;
 OB mang_ob8;
 STR ret_val28;
 STR plus_self10;
 STR plus_sarg2;
 STR ret_val29;
 STR plus_self11;
 STR plus_sarg3;
 STR ret_val30;
 STR plus_self12;
 STR plus_sarg4;
 STR ret_val31;
 STR plus_self13;
 STR plus_sarg5;
 STR ret_val32;
 ARRAYSTR sort_self2;
 INT L141 = INT_zero;
 CGEN ndefer_self1;
 STR ndefer_s1;
 CGEN ndefer_self2;
 STR ndefer_s2;
 PROG psather_self;
 BOOL ret_val33 = BOOL_zero;
 CGEN ndefer_self3;
 STR ndefer_s3;
 CGEN ndefer_self4;
 STR ndefer_s4;
 CGEN ndefer_self5;
 STR ndefer_s5;
 CODE_FILE plus_self14;
 dSTR plus_s7;
 CODE_FILE ret_val34;
 CODE_FILE plus_self15;
 dSTR plus_s8;
 CODE_FILE plus_self16;
 dSTR plus_s9;
 CODE_FILE plus_self17;
 dSTR plus_s10;
 FSTR create_self4;
 FSTR ret_val35;
 CGEN ndefer_self6;
 STR ndefer_s6;
 CGEN ndefer_self7;
 STR ndefer_s7;
 CGEN ndefer_self8;
 STR ndefer_s8;
 CGEN ndefer_self9;
 STR ndefer_s9;
 CGEN ndefer_self10;
 STR ndefer_s10;
 CGEN ndefer_self11;
 STR ndefer_s11;
 CGEN ndefer_self12;
 STR ndefer_s12;
 CGEN ndefer_self13;
 STR ndefer_s13;
 INT L151 = INT_zero;
 CGEN mang_self9;
 OB mang_ob9;
 STR ret_val36;
 CODE_FILE uses_tp_self;
 dTP uses_tp_t;
 STR plus_self18;
 STR plus_sarg6;
 STR ret_val37;
 STR plus_self19;
 STR plus_sarg7;
 STR ret_val38;
 CGEN ndefer_self14;
 STR ndefer_s14;
 STR plus_self20;
 STR plus_sarg8;
 STR ret_val39;
 STR plus_self21;
 STR plus_sarg9;
 STR ret_val40;
 STR plus_self22;
 STR plus_sarg10;
 STR ret_val41;
 STR plus_self23;
 STR plus_sarg11;
 STR ret_val42;
 CGEN ndefer_self15;
 STR ndefer_s15;
 STR plus_self24;
 STR plus_sarg12;
 STR ret_val43;
 STR plus_self25;
 STR plus_sarg13;
 STR ret_val44;
 CGEN ndefer_self16;
 STR ndefer_s16;
 STR plus_self26;
 STR plus_sarg14;
 STR ret_val45;
 STR plus_self27;
 STR plus_sarg15;
 STR ret_val46;
 CGEN ndefer_self17;
 STR ndefer_s17;
 STR plus_self28;
 STR plus_sarg16;
 STR ret_val47;
 STR plus_self29;
 STR plus_sarg17;
 STR ret_val48;
 CGEN ndefer_self18;
 STR ndefer_s18;
 STR plus_self30;
 STR plus_sarg18;
 STR ret_val49;
 STR plus_self31;
 STR plus_sarg19;
 STR ret_val50;
 STR plus_self32;
 STR plus_sarg20;
 STR ret_val51;
 STR plus_self33;
 STR plus_sarg21;
 STR ret_val52;
 STR plus_self34;
 STR plus_sarg22;
 STR ret_val53;
 STR plus_self35;
 STR plus_sarg23;
 STR ret_val54;
 CGEN ndefer_self19;
 STR ndefer_s19;
 STR plus_self36;
 STR plus_sarg24;
 STR ret_val55;
 STR plus_self37;
 STR plus_sarg25;
 STR ret_val56;
 STR plus_self38;
 STR plus_sarg26;
 STR ret_val57;
 STR plus_self39;
 STR plus_sarg27;
 STR ret_val58;
 CGEN ndefer_self20;
 STR ndefer_s20;
 STR plus_self40;
 STR plus_sarg28;
 STR ret_val59;
 STR plus_self41;
 STR plus_sarg29;
 STR ret_val60;
 CGEN ndefer_self21;
 STR ndefer_s21;
 STR plus_self42;
 STR plus_sarg30;
 STR ret_val61;
 STR plus_self43;
 STR plus_sarg31;
 STR ret_val62;
 CGEN ndefer_self22;
 STR ndefer_s22;
 STR plus_self44;
 STR plus_sarg32;
 STR ret_val63;
 STR plus_self45;
 STR plus_sarg33;
 STR ret_val64;
 STR plus_self46;
 STR plus_sarg34;
 STR ret_val65;
 STR plus_self47;
 STR plus_sarg35;
 STR ret_val66;
 CGEN ndefer_self23;
 STR ndefer_s23;
 STR plus_self48;
 STR plus_sarg36;
 STR ret_val67;
 STR plus_self49;
 STR plus_sarg37;
 STR ret_val68;
 STR plus_self50;
 STR plus_sarg38;
 STR ret_val69;
 STR plus_self51;
 STR plus_sarg39;
 STR ret_val70;
 CGEN ndefer_self24;
 STR ndefer_s24;
 STR plus_self52;
 STR plus_sarg40;
 STR ret_val71;
 STR plus_self53;
 STR plus_sarg41;
 STR ret_val72;
 CGEN ndefer_self25;
 STR ndefer_s25;
 CGEN ndefer_self26;
 STR ndefer_s26;
 CGEN ndefer_self27;
 STR ndefer_s27;
 CODE_FILE plus_self54;
 dSTR plus_s11;
 CODE_FILE ret_val73;
 CODE_FILE plus_self55;
 dSTR plus_s12;
 extern STR S_tag;
 extern STR S_tag;
 extern STR S_tag;
 extern STR S_tag;
 extern STR S_tag;
 extern STR S_tag;
 FSTR L16;
 dSTR L17;
 extern STR STRgen663879124;
 FSTR L18;
 dSTR L19;
 FSTR L20;
 INT L22;
 OB L23;
 INT L24;
 extern STR name9;
 extern STR switchi;
 ARRAYSTR L25;
 INT L26;
 OB L27;
 INT L28;
 ARRAYdTP L29;
 INT L30;
 OB L32;
 INT L33;
 INT L341_br;
 dTP L35;
 dTP L36;
 INT L371_;
 BOOL L38;
 BOOL L391_;
 INT L401_;
 INT L42;
 INT L431_;
 INT L441_br;
INT i_L441_=0;
 TUPdTPINT L45;
 BOOL L46;
 BOOL L47;
 dTP L48;
 dTP L49;
 BOOL L50;
 BOOL L521_;
 dTP L53;
 IMPL L54;
 BOOL L55;
 BOOL L561_;
 extern STR define1;
 BOOL L57;
 BOOL L581_;
 FSTR L59;
 dSTR L60;
 BOOL L62;
 BOOL L631_;
 FSTR L64;
 dSTR L65;
 extern STR S_ASIZE1;
 BOOL L66;
 BOOL L671_;
 FSTR L68;
 dSTR L69;
 dTP L70;
 dSTR L72;
 OB L73;
 FSTR L74;
 dSTR L75;
 extern STR define1;
 extern STR S_tag1;
 BOOL L76;
 BOOL L771_;
 STR L78;
 BOOL L79;
 BOOL L801_;
 INT L821_;
 INT L83;
 INT L841_;
 INT L851_br;
 STR aL851_;
 STR L86;
 FSTR L87;
 dSTR L88;
 INT L891_;
 INT L901_br;
 dTP aL901_;
 INT L921_br;
 dTP L93;
 AM_STR_CONST L94;
 OB L95;
 dTP L96;
 extern STR case4;
 extern STR S_tagreturn;
 extern STR name16;
 STR L97;
 INT L981_;
 BOOL L99;
 BOOL L1001_;
 INT L1021_;
 INT L103;
 INT L1041_;
 INT L1051_br;
 STR aL1051_;
 STR L106;
 INT L1071_;
 extern STR defaul1261471675;
 BOOL L108;
 extern STR PSATHER_ABORT2;
 extern STR abort2;
 extern STR name139;
 BOOL L109;
 BOOL L1101_;
 FSTR L111;
 dSTR L112;
 extern STR name161;
 FSTR L113;
 dSTR L114;
 extern STR BOOLge1425058140;
 FSTR L115;
 dSTR L116;
 extern STR INTt1t2BOOLr1;
 FSTR L117;
 dSTR L118;
 FSTR L119;
 INT L120;
 OB L122;
 INT L123;
 extern STR name9;
 extern STR ifo1o2returnTRUE;
 extern STR ifFVOI1051551677;
 extern STR ifFVOI1458418902;
 extern STR t1F_TA1146024679;
 extern STR ifo1NU2130326399;
 extern STR t1o1he313626905;
 extern STR ift1bt496219238;
 extern STR switcht1;
 INT L1241_br;
 dTP aL1241_;
 dTP L125;
 dTP L126;
 dTP L127;
 extern STR case4;
 extern STR S_tag2;
 extern STR name180;
 extern STR v1v2F_1821257885;
 extern STR S_boxe1015469271;
 extern STR F_VA_RATTR_NAv2;
 extern STR S_boxe2059544060;
 extern STR returnr;
 extern STR v12;
 extern STR v22;
 extern STR name102;
 extern STR case4;
 extern STR S_tag2;
 extern STR name180;
 extern STR v1v2F_V_RATTR_LP;
 extern STR v13;
 extern STR S_boxe1015469271;
 extern STR F_V_RATTR_LP2;
 extern STR v23;
 extern STR S_boxe2059544060;
 extern STR returnr;
 extern STR v12;
 extern STR v22;
 extern STR name102;
 extern STR case4;
 extern STR S_tag2;
 extern STR name180;
 extern STR v14;
 extern STR S_boxe10154692711;
 extern STR name181;
 extern STR v24;
 extern STR S_boxe20595440601;
 extern STR returnr;
 extern STR v12;
 extern STR v22;
 extern STR name102;
 INT L1281_;
 extern STR defaul1217511725;
 extern STR name182;
 BOOL L129;
 BOOL L1301_;
 FSTR L132;
 dSTR L133;
 extern STR name161;
 FSTR L134;
 dSTR L135;
 tag_for_self = self;
 tag_for_tp = ((dTP) TP_BUILTIN_int);
 SATTR(tag_for_self,needs_tag,FSETdT1016814448(ATTR(tag_for_self,needs_tag), tag_for_tp));
 mang_self = tag_for_self;
 mang_ob = ((OB) tag_for_tp);
 ret_val1 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
 res = STR_ap2004550586(ret_val1, ((STR) &S_tag));
 forbid_self = tag_for_self;
 forbid_s = res;
 forbid_self1 = ATTR(forbid_self,mangler);
 forbid_s1 = forbid_s;
 SATTR(forbid_self1,forbidden,FSETST1404644833(ATTR(forbid_self1,forbidden), forbid_s1));
 ret_val = res;
 s = ret_val;
 tag_for_self1 = self;
 tag_for_tp1 = ((dTP) TP_BUILTIN_char);
 SATTR(tag_for_self1,needs_tag,FSETdT1016814448(ATTR(tag_for_self1,needs_tag), tag_for_tp1));
 mang_self1 = tag_for_self1;
 mang_ob1 = ((OB) tag_for_tp1);
 ret_val3 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
 res1 = STR_ap2004550586(ret_val3, ((STR) &S_tag));
 forbid_self2 = tag_for_self1;
 forbid_s2 = res1;
 forbid_self3 = ATTR(forbid_self2,mangler);
 forbid_s3 = forbid_s2;
 SATTR(forbid_self3,forbidden,FSETST1404644833(ATTR(forbid_self3,forbidden), forbid_s3));
 ret_val2 = res1;
 s = ret_val2;
 tag_for_self2 = self;
 tag_for_tp2 = ((dTP) TP_BUILTIN_bool);
 SATTR(tag_for_self2,needs_tag,FSETdT1016814448(ATTR(tag_for_self2,needs_tag), tag_for_tp2));
 mang_self2 = tag_for_self2;
 mang_ob2 = ((OB) tag_for_tp2);
 ret_val5 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, ((OB) NULL));
 res2 = STR_ap2004550586(ret_val5, ((STR) &S_tag));
 forbid_self4 = tag_for_self2;
 forbid_s4 = res2;
 forbid_self5 = ATTR(forbid_self4,mangler);
 forbid_s5 = forbid_s4;
 SATTR(forbid_self5,forbidden,FSETST1404644833(ATTR(forbid_self5,forbidden), forbid_s5));
 ret_val4 = res2;
 s = ret_val4;
 tag_for_self3 = self;
 tag_for_tp3 = ((dTP) TP_BUILTIN_flt);
 SATTR(tag_for_self3,needs_tag,FSETdT1016814448(ATTR(tag_for_self3,needs_tag), tag_for_tp3));
 mang_self3 = tag_for_self3;
 mang_ob3 = ((OB) tag_for_tp3);
 ret_val7 = MANGLE119219986(ATTR(mang_self3,mangler), mang_ob3, ((OB) NULL));
 res3 = STR_ap2004550586(ret_val7, ((STR) &S_tag));
 forbid_self6 = tag_for_self3;
 forbid_s6 = res3;
 forbid_self7 = ATTR(forbid_self6,mangler);
 forbid_s7 = forbid_s6;
 SATTR(forbid_self7,forbidden,FSETST1404644833(ATTR(forbid_self7,forbidden), forbid_s7));
 ret_val6 = res3;
 s = ret_val6;
 tag_for_self4 = self;
 tag_for_tp4 = ((dTP) TP_BUILTIN_fltd);
 SATTR(tag_for_self4,needs_tag,FSETdT1016814448(ATTR(tag_for_self4,needs_tag), tag_for_tp4));
 mang_self4 = tag_for_self4;
 mang_ob4 = ((OB) tag_for_tp4);
 ret_val9 = MANGLE119219986(ATTR(mang_self4,mangler), mang_ob4, ((OB) NULL));
 res4 = STR_ap2004550586(ret_val9, ((STR) &S_tag));
 forbid_self8 = tag_for_self4;
 forbid_s8 = res4;
 forbid_self9 = ATTR(forbid_self8,mangler);
 forbid_s9 = forbid_s8;
 SATTR(forbid_self9,forbidden,FSETST1404644833(ATTR(forbid_self9,forbidden), forbid_s9));
 ret_val8 = res4;
 s = ret_val8;
 tag_for_self5 = self;
 tag_for_tp5 = ((dTP) TP_BUILTIN_str);
 SATTR(tag_for_self5,needs_tag,FSETdT1016814448(ATTR(tag_for_self5,needs_tag), tag_for_tp5));
 mang_self5 = tag_for_self5;
 mang_ob5 = ((OB) tag_for_tp5);
 ret_val11 = MANGLE119219986(ATTR(mang_self5,mangler), mang_ob5, ((OB) NULL));
 res5 = STR_ap2004550586(ret_val11, ((STR) &S_tag));
 forbid_self10 = tag_for_self5;
 forbid_s10 = res5;
 forbid_self11 = ATTR(forbid_self10,mangler);
 forbid_s11 = forbid_s10;
 SATTR(forbid_self11,forbidden,FSETST1404644833(ATTR(forbid_self11,forbidden), forbid_s11));
 ret_val10 = res5;
 s = ret_val10;
 if (ATTR(self,type_tables)) {
  plus_self = ATTR(self,tags_h);
  str_self = PRINT_1557717939(((PRINT_OB) NULL), ATTR(self,prog), self);
  ret_val12 = STR_fr1097270334(((STR) NULL), str_self);
  plus_s = ((dSTR) ret_val12);
  L16 = ATTR(plus_self,ntext);
  L17 = plus_s;
  SATTR(plus_self,ntext,FSTR_p1752847026(L16, (*dSTR_strrSTR[TAG(L17)])(L17)));
 }
 SATTR(self,code_c,ATTR(self,system_c));
 plus_self1 = ATTR(self,system_c);
 plus_s1 = ((dSTR) ((STR) &STRgen663879124));
 L18 = ATTR(plus_self1,ntext);
 L19 = plus_s1;
 SATTR(plus_self1,ntext,FSTR_p1752847026(L18, (*dSTR_strrSTR[TAG(L19)])(L19)));
 create_self = ((FSTR) NULL);
 L22 = 16;
 L24=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L22)*sizeof(CHAR);
 L23=ZALLOC_LEAF_BIG(L24);
 if (L23==NULL) FATAL("Unable to allocate more memory");
 memset(L23,0,L24);
 ((OB)L23)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L23)->header.destroyed=0;
#endif

 L20 = ((FSTR) L23);
 L20->asize = L22;
 ret_val13 = L20;
 SATTR(self,routine_code,FSTR_p1752847026(ret_val13, ((STR) &name9)));
 ndefer_self = self;
 ndefer_s = ((STR) &switchi);
 CGEN_d695831906(ndefer_self);
 SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
 create_self1 = ((ARRAYSTR) NULL);
 create_n = FMAPdT1077886323(ATTR(self,tags));
 L26 = create_n;
 L28=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L26)*sizeof(STR);
 L27=ZALLOC_BIG(L28);
 if (L27==NULL) FATAL("Unable to allocate more memory");
 ((OB)L27)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L27)->header.destroyed=0;
#endif

 L25 = ((ARRAYSTR) L27);
 L25->asize = L26;
 ret_val14 = L25;
 ts = ret_val14;
 create_self2 = ((ARRAYdTP) NULL);
 create_n1 = FMAPdT1077886323(ATTR(self,tags));
 L30 = create_n1;
 L33=(sizeof(struct ARRAYdTP_struct)-sizeof(dTP))+(L30)*sizeof(dTP);
 L32=ZALLOC_BIG(L33);
 if (L32==NULL) FATAL("Unable to allocate more memory");
 ((OB)L32)->header.tag=ARRAYdTP_tag;
#ifdef DESTROY_CHK

   ((OB)L32)->header.destroyed=0;
#endif

 L29 = ((ARRAYdTP) L32);
 L29->asize = L30;
 ret_val15 = L29;
 tags_keys = ret_val15;
 {
  struct FMAPdT705360156_frame_struct other1_0;
FMAPdT705360156_frame noname1 = &other1_0;
  BOOL f_L341_ = TRUE;
  L11 = ATTR(self,tags);
  /* loop index variable */
  L101 = 0;
  L21 = tags_keys;
  noname1->self = L11;
  noname1->state = 0;
  L341_br=L21==NULL?0:ASIZE((ARRAYdTP)L21); 
  while (1) {
   if(L101>=L341_br)  goto after_loop; 
   L35 = FMAPdT705360156(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   L36 = L35;
   SARR((ARRAYdTP)L21,L101,(dTP)L36); 
   ;
   L371_=INTPLUS(L101,1); 
   L101 = L371_;
  }
 }
 after_loop: ;
 sort_self = tags_keys;
 L38 = (sort_self==((ARRAYdTP) NULL));
 L391_=!(L38); 
 if (L391_) {
  L401_=ASIZE((ARRAYdTP)sort_self); 
  L42 = L401_;
  L431_=INTMINUS(L42,1); 
  ARRAYd1239612935(sort_self, 0, L431_);
 }
 {
  struct FMAPdT1857608820_frame_struct other2_0;
FMAPdT1857608820_frame noname2 = &other2_0;
  BOOL f_L441_ = TRUE;
  L31 = ATTR(self,tags);
  L41 = ts;
  noname2->self = L31;
  noname2->state = 0;
  L441_br=L41==NULL?0:ASIZE((ARRAYSTR)L41); 
  i_L441_=0;
  while (1) {
   L45 = FMAPdT1857608820(noname2);
   if (noname2->state == -1) {
    goto after_loop1;
   }
   p = L45;
   L48 = p.t1;
   if ((*dTP_is1334578382[TAG(L48)])(L48)) {
    L49 = p.t1;
    L50 = ((*dTP_implrIMPL[TAG(L49)])(L49)==((IMPL) NULL));
    L521_=!(L50); 
    L47 = L521_;
   } else {
    L47 = FALSE;
   }
   if (L47) {
    L53 = p.t1;
    L54=(*dTP_implrIMPL[TAG(L53)])(L53);
    L55 = (ATTR(L54,arr)==((TP_CLASS) NULL));
    L561_=!(L55); 
    L46 = L561_;
   } else {
    L46 = FALSE;
   }
   if (L46) {
    plus_self2 = ATTR(self,tags_h);
    plus_s2 = ((dSTR) ((STR) &define1));
    L57 = (plus_s2==((dSTR) NULL));
    L581_=!(L57); 
    if (L581_) {
     L59 = ATTR(plus_self2,ntext);
     L60 = plus_s2;
     SATTR(plus_self2,ntext,FSTR_p1752847026(L59, (*dSTR_strrSTR[TAG(L60)])(L60)));
    }
    ret_val16 = plus_self2;
    plus_self3 = ret_val16;
    mang_self6 = self;
    mang_ob6 = ((OB) p.t1);
    ret_val17 = MANGLE119219986(ATTR(mang_self6,mangler), mang_ob6, ((OB) NULL));
    plus_s3 = ((dSTR) ret_val17);
    L62 = (plus_s3==((dSTR) NULL));
    L631_=!(L62); 
    if (L631_) {
     L64 = ATTR(plus_self3,ntext);
     L65 = plus_s3;
     SATTR(plus_self3,ntext,FSTR_p1752847026(L64, (*dSTR_strrSTR[TAG(L65)])(L65)));
    }
    ret_val18 = plus_self3;
    plus_self4 = ret_val18;
    plus_s4 = ((dSTR) ((STR) &S_ASIZE1));
    L66 = (plus_s4==((dSTR) NULL));
    L671_=!(L66); 
    if (L671_) {
     L68 = ATTR(plus_self4,ntext);
     L69 = plus_s4;
     SATTR(plus_self4,ntext,FSTR_p1752847026(L68, (*dSTR_strrSTR[TAG(L69)])(L69)));
    }
    ret_val19 = plus_self4;
    plus_self5 = ret_val19;
    L70 = p.t1;
    L73=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
    if (L73==NULL) FATAL("Unable to allocate more memory");
    memset(L73,0,sizeof(struct INT_boxed_struct));
    ((OB)L73)->header.tag=INT_tag;
    L72 = (dSTR)((INT_boxed) L73);
    ((INT_boxed) L72)->immutable_part = IMPL_a1897060099((*dTP_implrIMPL[TAG(L70)])(L70));
    plus_s5 = L72;
    L74 = ATTR(plus_self5,ntext);
    L75 = plus_s5;
    SATTR(plus_self5,ntext,FSTR_p1752847026(L74, (*dSTR_strrSTR[TAG(L75)])(L75)));
   }
   if(i_L441_>=L441_br)  goto after_loop1; 
   plus_self6 = ((STR) &define1);
   mang_self7 = self;
   mang_ob7 = ((OB) p.t1);
   ret_val20 = MANGLE119219986(ATTR(mang_self7,mangler), mang_ob7, ((OB) NULL));
   plus_sarg = ret_val20;
   ret_val21 = STR_ap2004550586(plus_self6, plus_sarg);
   plus_self7 = ret_val21;
   plus_sarg1 = ((STR) &S_tag1);
   ret_val22 = STR_ap2004550586(plus_self7, plus_sarg1);
   plus_self8 = ret_val22;
   plus_arg = p.t2;
   str_self1 = plus_arg;
   clear_self = buf1;
   L76 = (clear_self==((FSTR) NULL));
   L771_=!(L76); 
   if (L771_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self1;
   str_in_s = buf1;
   ret_val25 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val25;
   str_self2 = buf1;
   ret_val26 = STR_fr1097270334(((STR) NULL), str_self2);
   ret_val24 = ret_val26;
   s1 = ret_val24;
   ret_val23 = STR_ap1077157958(plus_self8, s1, TRUE);
   L78 = ret_val23;
   SARR((ARRAYSTR)L41,i_L441_,(STR)L78); i_L441_++;
   ;
  }
 }
 after_loop1: ;
 sort_self1 = ts;
 L79 = (sort_self1==((ARRAYSTR) NULL));
 L801_=!(L79); 
 if (L801_) {
  L821_=ASIZE((ARRAYSTR)sort_self1); 
  L83 = L821_;
  L841_=INTMINUS(L83,1); 
  ARRAYS1450857938(sort_self1, 0, L841_);
 }
 {
  BOOL f_L851_ = TRUE;
  /* loop index variable */
  L121 = 0;
  L51 = ts;
  L851_br=L51==NULL?0:ASIZE((ARRAYSTR)L51); 
  while (1) {
   plus_self9 = ATTR(self,tags_h);
   if(L121>=L851_br)  goto after_loop2; 
   aL851_=ARR((ARRAYSTR)L51,L121); 
   plus_s6 = ((dSTR) aL851_);
   L87 = ATTR(plus_self9,ntext);
   L88 = plus_s6;
   SATTR(plus_self9,ntext,FSTR_p1752847026(L87, (*dSTR_strrSTR[TAG(L88)])(L88)));
   L891_=INTPLUS(L121,1); 
   L121 = L891_;
  }
 }
 after_loop2: ;
 {
  BOOL f_L901_ = TRUE;
  BOOL f_L921_ = TRUE;
  /* loop index variable */
  L131 = 0;
  L61 = tags_keys;
  L71 = ts;
  L901_br=L61==NULL?0:ASIZE((ARRAYdTP)L61); 
  L921_br=L71==NULL?0:ASIZE((ARRAYSTR)L71); 
  while (1) {
   if(L131>=L901_br)  goto after_loop3; 
   aL901_=ARR((ARRAYdTP)L61,L131); 
   p_key = aL901_;
   create_self3 = ((AM_STR_CONST) NULL);
   L95=ZALLOC(sizeof(struct AM_STR_CONST_struct));
   if (L95==NULL) FATAL("Unable to allocate more memory");
   ((OB)L95)->header.tag=AM_STR_CONST_tag;
   L94 = ((AM_STR_CONST) L95);
   ret_val27 = L94;
   dummy1 = ret_val27;
   L96 = p_key;
   SATTR(dummy1,bval,(*dTP_strrSTR[TAG(L96)])(L96));
   if(L131>=L921_br)  goto after_loop3; 
   plus_self10 = ((STR) &case4);
   mang_self8 = self;
   mang_ob8 = ((OB) p_key);
   ret_val28 = MANGLE119219986(ATTR(mang_self8,mangler), mang_ob8, ((OB) NULL));
   plus_sarg2 = ret_val28;
   ret_val29 = STR_ap2004550586(plus_self10, plus_sarg2);
   plus_self11 = ret_val29;
   plus_sarg3 = ((STR) &S_tagreturn);
   ret_val30 = STR_ap2004550586(plus_self11, plus_sarg3);
   plus_self12 = ret_val30;
   plus_sarg4 = CGEN_e1633118543(self, dummy1);
   ret_val31 = STR_ap2004550586(plus_self12, plus_sarg4);
   plus_self13 = ret_val31;
   plus_sarg5 = ((STR) &name16);
   ret_val32 = STR_ap2004550586(plus_self13, plus_sarg5);
   L97 = ret_val32;
   SARR((ARRAYSTR)L71,L131,(STR)L97); 
   ;
   L981_=INTPLUS(L131,1); 
   L131 = L981_;
  }
 }
 after_loop3: ;
 sort_self2 = ts;
 L99 = (sort_self2==((ARRAYSTR) NULL));
 L1001_=!(L99); 
 if (L1001_) {
  L1021_=ASIZE((ARRAYSTR)sort_self2); 
  L103 = L1021_;
  L1041_=INTMINUS(L103,1); 
  ARRAYS1450857938(sort_self2, 0, L1041_);
 }
 {
  BOOL f_L1051_ = TRUE;
  /* loop index variable */
  L141 = 0;
  L81 = ts;
  L1051_br=L81==NULL?0:ASIZE((ARRAYSTR)L81); 
  while (1) {
   ndefer_self1 = self;
   if(L141>=L1051_br)  goto after_loop4; 
   aL1051_=ARR((ARRAYSTR)L81,L141); 
   ndefer_s1 = aL1051_;
   CGEN_d695831906(ndefer_self1);
   SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
   L1071_=INTPLUS(L141,1); 
   L141 = L1071_;
  }
 }
 after_loop4: ;
 ndefer_self2 = self;
 ndefer_s2 = ((STR) &defaul1261471675);
 CGEN_d695831906(ndefer_self2);
 SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
 psather_self = ATTR(self,prog);
 if (ATTR(psather_self,threads)) {
  L108 = TRUE;
 } else {
  L108 = ATTR(psather_self,distributed);
 }
 ret_val33 = L108;
 if (ret_val33) {
  ndefer_self3 = self;
  ndefer_s3 = ((STR) &PSATHER_ABORT2);
  CGEN_d695831906(ndefer_self3);
  SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
 }
 ndefer_self4 = self;
 ndefer_s4 = ((STR) &abort2);
 CGEN_d695831906(ndefer_self4);
 SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
 ndefer_self5 = self;
 ndefer_s5 = ((STR) &name139);
 CGEN_d695831906(ndefer_self5);
 SATTR(ndefer_self5,routine_code,FSTR_p1752847026(ATTR(ndefer_self5,routine_code), ndefer_s5));
 plus_self14 = ATTR(self,system_c);
 plus_s7 = ((dSTR) ATTR(self,routine_code));
 L109 = (plus_s7==((dSTR) NULL));
 L1101_=!(L109); 
 if (L1101_) {
  L111 = ATTR(plus_self14,ntext);
  L112 = plus_s7;
  SATTR(plus_self14,ntext,FSTR_p1752847026(L111, (*dSTR_strrSTR[TAG(L112)])(L112)));
 }
 ret_val34 = plus_self14;
 plus_self15 = ret_val34;
 plus_s8 = ((dSTR) ((STR) &name161));
 L113 = ATTR(plus_self15,ntext);
 L114 = plus_s8;
 SATTR(plus_self15,ntext,FSTR_p1752847026(L113, (*dSTR_strrSTR[TAG(L114)])(L114)));
 SATTR(self,code_c,ATTR(self,system_c));
 plus_self16 = ATTR(self,system_c);
 plus_s9 = ((dSTR) ((STR) &BOOLge1425058140));
 L115 = ATTR(plus_self16,ntext);
 L116 = plus_s9;
 SATTR(plus_self16,ntext,FSTR_p1752847026(L115, (*dSTR_strrSTR[TAG(L116)])(L116)));
 plus_self17 = ATTR(self,system_c);
 plus_s10 = ((dSTR) ((STR) &INTt1t2BOOLr1));
 L117 = ATTR(plus_self17,ntext);
 L118 = plus_s10;
 SATTR(plus_self17,ntext,FSTR_p1752847026(L117, (*dSTR_strrSTR[TAG(L118)])(L118)));
 create_self4 = ((FSTR) NULL);
 L120 = 16;
 L123=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L120)*sizeof(CHAR);
 L122=ZALLOC_LEAF_BIG(L123);
 if (L122==NULL) FATAL("Unable to allocate more memory");
 memset(L122,0,L123);
 ((OB)L122)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L122)->header.destroyed=0;
#endif

 L119 = ((FSTR) L122);
 L119->asize = L120;
 ret_val35 = L119;
 SATTR(self,routine_code,FSTR_p1752847026(ret_val35, ((STR) &name9)));
 ndefer_self6 = self;
 ndefer_s6 = ((STR) &ifo1o2returnTRUE);
 CGEN_d695831906(ndefer_self6);
 SATTR(ndefer_self6,routine_code,FSTR_p1752847026(ATTR(ndefer_self6,routine_code), ndefer_s6));
 if (ATTR(ATTR(self,prog),distributed)) {
  ndefer_self7 = self;
  ndefer_s7 = ((STR) &ifFVOI1051551677);
  CGEN_d695831906(ndefer_self7);
  SATTR(ndefer_self7,routine_code,FSTR_p1752847026(ATTR(ndefer_self7,routine_code), ndefer_s7));
  ndefer_self8 = self;
  ndefer_s8 = ((STR) &ifFVOI1458418902);
  CGEN_d695831906(ndefer_self8);
  SATTR(ndefer_self8,routine_code,FSTR_p1752847026(ATTR(ndefer_self8,routine_code), ndefer_s8));
  ndefer_self9 = self;
  ndefer_s9 = ((STR) &t1F_TA1146024679);
  CGEN_d695831906(ndefer_self9);
  SATTR(ndefer_self9,routine_code,FSTR_p1752847026(ATTR(ndefer_self9,routine_code), ndefer_s9));
 }
 else {
  ndefer_self10 = self;
  ndefer_s10 = ((STR) &ifo1NU2130326399);
  CGEN_d695831906(ndefer_self10);
  SATTR(ndefer_self10,routine_code,FSTR_p1752847026(ATTR(ndefer_self10,routine_code), ndefer_s10));
  ndefer_self11 = self;
  ndefer_s11 = ((STR) &t1o1he313626905);
  CGEN_d695831906(ndefer_self11);
  SATTR(ndefer_self11,routine_code,FSTR_p1752847026(ATTR(ndefer_self11,routine_code), ndefer_s11));
 }
 ndefer_self12 = self;
 ndefer_s12 = ((STR) &ift1bt496219238);
 CGEN_d695831906(ndefer_self12);
 SATTR(ndefer_self12,routine_code,FSTR_p1752847026(ATTR(ndefer_self12,routine_code), ndefer_s12));
 ndefer_self13 = self;
 ndefer_s13 = ((STR) &switcht1);
 CGEN_d695831906(ndefer_self13);
 SATTR(ndefer_self13,routine_code,FSTR_p1752847026(ATTR(ndefer_self13,routine_code), ndefer_s13));
 {
  BOOL f_L1241_ = TRUE;
  /* loop index variable */
  L151 = 0;
  L91 = tags_keys;
  L1241_br=L91==NULL?0:ASIZE((ARRAYdTP)L91); 
  while (1) {
   if(L151>=L1241_br)  goto after_loop5; 
   aL1241_=ARR((ARRAYdTP)L91,L151); 
   tp = aL1241_;
   mang_self9 = self;
   mang_ob9 = ((OB) tp);
   ret_val36 = MANGLE119219986(ATTR(mang_self9,mangler), mang_ob9, ((OB) NULL));
   tpstr = ret_val36;
   uses_tp_self = ATTR(self,system_c);
   uses_tp_t = tp;
   CODE_F525364269(uses_tp_self, LAYOUT79932797(((LAYOUT_TBL) NULL), uses_tp_t));
   L126 = tp;
   if ((*dTP_is1334578382[TAG(L126)])(L126)) {
    if (ATTR(ATTR(self,prog),distributed)) {
     L127 = tp;
     if ((*dTP_is418055720[TAG(L127)])(L127)) {
      ndefer_self14 = self;
      plus_self18 = ((STR) &case4);
      plus_sarg6 = tpstr;
      ret_val37 = STR_ap2004550586(plus_self18, plus_sarg6);
      plus_self19 = ret_val37;
      plus_sarg7 = ((STR) &S_tag2);
      ret_val38 = STR_ap2004550586(plus_self19, plus_sarg7);
      ndefer_s14 = ret_val38;
      CGEN_d695831906(ndefer_self14);
      SATTR(ndefer_self14,routine_code,FSTR_p1752847026(ATTR(ndefer_self14,routine_code), ndefer_s14));
      ndefer_self15 = self;
      plus_self20 = ((STR) &name180);
      plus_sarg8 = tpstr;
      ret_val39 = STR_ap2004550586(plus_self20, plus_sarg8);
      plus_self21 = ret_val39;
      plus_sarg9 = ((STR) &v1v2F_1821257885);
      ret_val40 = STR_ap2004550586(plus_self21, plus_sarg9);
      plus_self22 = ret_val40;
      plus_sarg10 = tpstr;
      ret_val41 = STR_ap2004550586(plus_self22, plus_sarg10);
      plus_self23 = ret_val41;
      plus_sarg11 = ((STR) &S_boxe1015469271);
      ret_val42 = STR_ap2004550586(plus_self23, plus_sarg11);
      ndefer_s15 = ret_val42;
      CGEN_d695831906(ndefer_self15);
      SATTR(ndefer_self15,routine_code,FSTR_p1752847026(ATTR(ndefer_self15,routine_code), ndefer_s15));
      ndefer_self16 = self;
      plus_self24 = ((STR) &F_VA_RATTR_NAv2);
      plus_sarg12 = tpstr;
      ret_val43 = STR_ap2004550586(plus_self24, plus_sarg12);
      plus_self25 = ret_val43;
      plus_sarg13 = ((STR) &S_boxe2059544060);
      ret_val44 = STR_ap2004550586(plus_self25, plus_sarg13);
      ndefer_s16 = ret_val44;
      CGEN_d695831906(ndefer_self16);
      SATTR(ndefer_self16,routine_code,FSTR_p1752847026(ATTR(ndefer_self16,routine_code), ndefer_s16));
      ndefer_self17 = self;
      plus_self26 = ((STR) &returnr);
      plus_sarg14 = CGEN_v205291382(self, tp, ((STR) &v12), ((STR) &v22));
      ret_val45 = STR_ap2004550586(plus_self26, plus_sarg14);
      plus_self27 = ret_val45;
      plus_sarg15 = ((STR) &name102);
      ret_val46 = STR_ap2004550586(plus_self27, plus_sarg15);
      ndefer_s17 = ret_val46;
      CGEN_d695831906(ndefer_self17);
      SATTR(ndefer_self17,routine_code,FSTR_p1752847026(ATTR(ndefer_self17,routine_code), ndefer_s17));
     }
     else {
      ndefer_self18 = self;
      plus_self28 = ((STR) &case4);
      plus_sarg16 = tpstr;
      ret_val47 = STR_ap2004550586(plus_self28, plus_sarg16);
      plus_self29 = ret_val47;
      plus_sarg17 = ((STR) &S_tag2);
      ret_val48 = STR_ap2004550586(plus_self29, plus_sarg17);
      ndefer_s18 = ret_val48;
      CGEN_d695831906(ndefer_self18);
      SATTR(ndefer_self18,routine_code,FSTR_p1752847026(ATTR(ndefer_self18,routine_code), ndefer_s18));
      ndefer_self19 = self;
      plus_self30 = ((STR) &name180);
      plus_sarg18 = tpstr;
      ret_val49 = STR_ap2004550586(plus_self30, plus_sarg18);
      plus_self31 = ret_val49;
      plus_sarg19 = ((STR) &v1v2F_V_RATTR_LP);
      ret_val50 = STR_ap2004550586(plus_self31, plus_sarg19);
      plus_self32 = ret_val50;
      plus_sarg20 = tpstr;
      ret_val51 = STR_ap2004550586(plus_self32, plus_sarg20);
      plus_self33 = ret_val51;
      plus_sarg21 = ((STR) &v13);
      ret_val52 = STR_ap2004550586(plus_self33, plus_sarg21);
      plus_self34 = ret_val52;
      plus_sarg22 = tpstr;
      ret_val53 = STR_ap2004550586(plus_self34, plus_sarg22);
      plus_self35 = ret_val53;
      plus_sarg23 = ((STR) &S_boxe1015469271);
      ret_val54 = STR_ap2004550586(plus_self35, plus_sarg23);
      ndefer_s19 = ret_val54;
      CGEN_d695831906(ndefer_self19);
      SATTR(ndefer_self19,routine_code,FSTR_p1752847026(ATTR(ndefer_self19,routine_code), ndefer_s19));
      ndefer_self20 = self;
      plus_self36 = ((STR) &F_V_RATTR_LP2);
      plus_sarg24 = tpstr;
      ret_val55 = STR_ap2004550586(plus_self36, plus_sarg24);
      plus_self37 = ret_val55;
      plus_sarg25 = ((STR) &v23);
      ret_val56 = STR_ap2004550586(plus_self37, plus_sarg25);
      plus_self38 = ret_val56;
      plus_sarg26 = tpstr;
      ret_val57 = STR_ap2004550586(plus_self38, plus_sarg26);
      plus_self39 = ret_val57;
      plus_sarg27 = ((STR) &S_boxe2059544060);
      ret_val58 = STR_ap2004550586(plus_self39, plus_sarg27);
      ndefer_s20 = ret_val58;
      CGEN_d695831906(ndefer_self20);
      SATTR(ndefer_self20,routine_code,FSTR_p1752847026(ATTR(ndefer_self20,routine_code), ndefer_s20));
      ndefer_self21 = self;
      plus_self40 = ((STR) &returnr);
      plus_sarg28 = CGEN_v205291382(self, tp, ((STR) &v12), ((STR) &v22));
      ret_val59 = STR_ap2004550586(plus_self40, plus_sarg28);
      plus_self41 = ret_val59;
      plus_sarg29 = ((STR) &name102);
      ret_val60 = STR_ap2004550586(plus_self41, plus_sarg29);
      ndefer_s21 = ret_val60;
      CGEN_d695831906(ndefer_self21);
      SATTR(ndefer_self21,routine_code,FSTR_p1752847026(ATTR(ndefer_self21,routine_code), ndefer_s21));
     }
    }
    else {
     ndefer_self22 = self;
     plus_self42 = ((STR) &case4);
     plus_sarg30 = tpstr;
     ret_val61 = STR_ap2004550586(plus_self42, plus_sarg30);
     plus_self43 = ret_val61;
     plus_sarg31 = ((STR) &S_tag2);
     ret_val62 = STR_ap2004550586(plus_self43, plus_sarg31);
     ndefer_s22 = ret_val62;
     CGEN_d695831906(ndefer_self22);
     SATTR(ndefer_self22,routine_code,FSTR_p1752847026(ATTR(ndefer_self22,routine_code), ndefer_s22));
     ndefer_self23 = self;
     plus_self44 = ((STR) &name180);
     plus_sarg32 = tpstr;
     ret_val63 = STR_ap2004550586(plus_self44, plus_sarg32);
     plus_self45 = ret_val63;
     plus_sarg33 = ((STR) &v14);
     ret_val64 = STR_ap2004550586(plus_self45, plus_sarg33);
     plus_self46 = ret_val64;
     plus_sarg34 = tpstr;
     ret_val65 = STR_ap2004550586(plus_self46, plus_sarg34);
     plus_self47 = ret_val65;
     plus_sarg35 = ((STR) &S_boxe10154692711);
     ret_val66 = STR_ap2004550586(plus_self47, plus_sarg35);
     ndefer_s23 = ret_val66;
     CGEN_d695831906(ndefer_self23);
     SATTR(ndefer_self23,routine_code,FSTR_p1752847026(ATTR(ndefer_self23,routine_code), ndefer_s23));
     ndefer_self24 = self;
     plus_self48 = ((STR) &name181);
     plus_sarg36 = tpstr;
     ret_val67 = STR_ap2004550586(plus_self48, plus_sarg36);
     plus_self49 = ret_val67;
     plus_sarg37 = ((STR) &v24);
     ret_val68 = STR_ap2004550586(plus_self49, plus_sarg37);
     plus_self50 = ret_val68;
     plus_sarg38 = tpstr;
     ret_val69 = STR_ap2004550586(plus_self50, plus_sarg38);
     plus_self51 = ret_val69;
     plus_sarg39 = ((STR) &S_boxe20595440601);
     ret_val70 = STR_ap2004550586(plus_self51, plus_sarg39);
     ndefer_s24 = ret_val70;
     CGEN_d695831906(ndefer_self24);
     SATTR(ndefer_self24,routine_code,FSTR_p1752847026(ATTR(ndefer_self24,routine_code), ndefer_s24));
     ndefer_self25 = self;
     plus_self52 = ((STR) &returnr);
     plus_sarg40 = CGEN_v205291382(self, tp, ((STR) &v12), ((STR) &v22));
     ret_val71 = STR_ap2004550586(plus_self52, plus_sarg40);
     plus_self53 = ret_val71;
     plus_sarg41 = ((STR) &name102);
     ret_val72 = STR_ap2004550586(plus_self53, plus_sarg41);
     ndefer_s25 = ret_val72;
     CGEN_d695831906(ndefer_self25);
     SATTR(ndefer_self25,routine_code,FSTR_p1752847026(ATTR(ndefer_self25,routine_code), ndefer_s25));
    }
   }
   L1281_=INTPLUS(L151,1); 
   L151 = L1281_;
  }
 }
 after_loop5: ;
 ndefer_self26 = self;
 ndefer_s26 = ((STR) &defaul1217511725);
 CGEN_d695831906(ndefer_self26);
 SATTR(ndefer_self26,routine_code,FSTR_p1752847026(ATTR(ndefer_self26,routine_code), ndefer_s26));
 ndefer_self27 = self;
 ndefer_s27 = ((STR) &name182);
 CGEN_d695831906(ndefer_self27);
 SATTR(ndefer_self27,routine_code,FSTR_p1752847026(ATTR(ndefer_self27,routine_code), ndefer_s27));
 plus_self54 = ATTR(self,system_c);
 plus_s11 = ((dSTR) ATTR(self,routine_code));
 L129 = (plus_s11==((dSTR) NULL));
 L1301_=!(L129); 
 if (L1301_) {
  L132 = ATTR(plus_self54,ntext);
  L133 = plus_s11;
  SATTR(plus_self54,ntext,FSTR_p1752847026(L132, (*dSTR_strrSTR[TAG(L133)])(L133)));
 }
 ret_val73 = plus_self54;
 plus_self55 = ret_val73;
 plus_s12 = ((dSTR) ((STR) &name161));
 L134 = ATTR(plus_self55,ntext);
 L135 = plus_s12;
 SATTR(plus_self55,ntext,FSTR_p1752847026(L134, (*dSTR_strrSTR[TAG(L135)])(L135)));
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_g1874773404(CGEN self) {
 FLISTAM_ROUT_DEF L11;
 AM_ROUT_DEF abs_rout;
 FLISTAM_ROUT_DEF L21;
 AM_ROUT_DEF abs_iter;
 AM_ROUT_DEF L3;
 AM_ROUT_DEF L4;
 {
  struct FLISTA2121868826_frame_struct other1_0;
FLISTA2121868826_frame noname1 = &other1_0;
  L11 = ATTR(self,abstract_routs);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = FLISTA2121868826(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   abs_rout = L3;
   CGEN_e1130765144(self, abs_rout);
  }
 }
 after_loop: ;
 {
  struct FLISTA2121868826_frame_struct other2_0;
FLISTA2121868826_frame noname2 = &other2_0;
  L21 = ATTR(self,abstract_iters);
  noname2->self = L21;
  noname2->state = 0;
  while (1) {
   L4 = FLISTA2121868826(noname2);
   if (noname2->state == -1) {
    goto after_loop1;
   }
   abs_iter = L4;
   CGEN_e1935776612(self, abs_iter);
  }
 }
 after_loop1: ;
}
