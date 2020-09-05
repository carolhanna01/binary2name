#include "sather.h"

/* Layouts */

typedef struct dAS_CLASS_ELT_struct {
 OB_HEADER header;
 } *dAS_CLASS_ELT;

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

typedef struct dSTR_struct {
 OB_HEADER header;
 } *dSTR;

typedef struct dTP_struct {
 OB_HEADER header;
 } *dTP;

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

typedef struct ARRAYATTRSORT_struct {/* layout for ARRAY{ATTRSORT} */
 OB_HEADER header;
 INT asize;
 struct ATTRSORT_struct *arr_part[1];
 } *ARRAYATTRSORT;

typedef struct ATTRSORT_struct {/* layout for ATTRSORT */
 OB_HEADER header;
 struct ELT_struct *elt1;
 STR name1;
 } *ATTRSORT;

typedef struct BFILE_struct {/* layout for BFILE */
 OB_HEADER header;
 EXT_OB fp;
 } *BFILE;

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

typedef struct RAT_struct {/* layout for RAT */
 struct INTI1_struct *u;
 struct INTI1_struct *v;
 } RAT;
static RAT RAT_zero;

typedef struct RAT_boxed_struct {
 OB_HEADER header;
 RAT immutable_part;
 } *RAT_boxed;

typedef struct SCANNER_struct {/* layout for SCANNER */
 OB_HEADER header;
 struct FSTR_struct *buf1;
 struct IDENT_struct lex_value;
 INT value_type;
 struct PROG_struct *prog;
 struct RAT_struct num_value;
 CHAR char_value;
 CHAR last_char;
 CHAR next;
 BOOL backed_up;
 BOOL pSather;
 BOOL zones;
 } *SCANNER;

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

typedef struct TOKEN_struct {/* layout for TOKEN */
 INT val1;
 } TOKEN;
static TOKEN TOKEN_zero;

typedef struct TOKEN_boxed_struct {
 OB_HEADER header;
 TOKEN immutable_part;
 } *TOKEN_boxed;

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

typedef struct TUPSIGINT_struct {/* layout for TUP{SIG,INT} */
 INT t2;
 struct SIG_struct *t1;
 } TUPSIGINT;
static TUPSIGINT TUPSIGINT_zero;

typedef struct TUPSIGINT_boxed_struct {
 OB_HEADER header;
 TUPSIGINT immutable_part;
 } *TUPSIGINT_boxed;

typedef struct UNIX_struct {/* layout for UNIX */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *UNIX;

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

typedef struct AM_ROU1569581452_frame_struct {
 INT state;
 AM_ROUT_DEF self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ROU1569581452_frame;

typedef struct ARRAYA1660316962_frame_struct {
 INT state;
 ARRAYATTRSORT self;/* Formal argument: self */
 ATTRSORT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA1660316962_frame;

typedef struct ARRAYA107911980_frame_struct {
 INT state;
 ARRAYATTRSORT self;/* Formal argument: self */
 ATTRSORT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA107911980_frame;

typedef struct ARRAYTUPSIGINT_struct {/* layout for ARRAY{TUP{SIG,INT}} */
 OB_HEADER header;
 INT asize;
 struct TUPSIGINT_struct arr_part[1];
 } *ARRAYTUPSIGINT;

typedef struct ARRAYT1593820953_frame_struct {
 INT state;
 ARRAYTUPSIGINT self;/* Formal argument: self */
 TUPSIGINT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT1593820953_frame;

typedef struct ARRAYT1024744539_frame_struct {
 INT state;
 ARRAYTUPSIGINT self;/* Formal argument: self */
 TUPSIGINT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT1024744539_frame;

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

typedef struct CONVERT_struct {/* layout for CONVERT */
 OB_HEADER header;
 struct BFILE_struct *file1;
 INT end_pos;
 INT start_pos;
 BOOL need_to_convert;
 } *CONVERT;

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

typedef struct FLISTINT_struct {/* layout for FLIST{INT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 INT arr_part[1];
 } *FLISTINT;

typedef struct FLISTSFILE_ID_struct {/* layout for FLIST{SFILE_ID} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct SFILE_ID_struct arr_part[1];
 } *FLISTSFILE_ID;

typedef struct FLISTSIG_struct {/* layout for FLIST{SIG} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct SIG_struct *arr_part[1];
 } *FLISTSIG;

typedef struct FLISTS1074219534_frame_struct {
 INT state;
 FLISTSIG self;/* Formal argument: self */
 SIG ret_val1;
 INT i;
 INT sz;
 FLISTSIG aget_self;
 INT aget_ind;
 SIG ret_val;
 } *FLISTS1074219534_frame;

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

typedef struct FMAPdT1072907830_frame_struct {
 INT state;
 FMAPdTPINT self;/* Formal argument: self */
 INT ret_val1;
 FMAPdTPINT L61;
 TUPdTPINT e;
 INT L31;
 FMAPdTPINT is_key_nil_self;
 dTP is_key_nil_e;
 BOOL ret_val;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPdT1072907830_frame;

typedef struct FSETdTP_struct {/* layout for FSET{$TP} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct dTP_struct *arr_part[1];
 } *FSETdTP;

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

typedef struct PARSER_struct {/* layout for PARSER */
 OB_HEADER header;
 struct CONVERT_struct *convert;
 struct FLISTSTR_struct *entered;
 struct PROG_struct *prog;
 struct SCANNER_struct *scanner;
 struct TOKEN_struct next;
 BOOL version_1_0;
 } *PARSER;

typedef struct bound1_ob_struct {
 OB_HEADER header;
 BOOL (*funcptr)(struct bound1_ob_struct *, TUPSIGINT, TUPSIGINT);
 PRINT_OB bound_arg0;
 } *bound1_ob;

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

typedef struct ROUTTU467247893_struct {
 OB_HEADER header;
 BOOL (*funcptr)(void *, TUPSIGINT, TUPSIGINT);
} *ROUTTU467247893;

#include "tags.h"

/* Globals */

extern FLISTINT SFILE_ID_lines;
extern FLISTSIG PRINT_OB_funcs;
extern FLISTSTR SFILE_ID_files;
extern FSTR SFILE_ID_source;
extern INOUT_MODE MODES_inout_mode;
extern INT PARSER1701976116;
extern INT PARSER_eof_tok;
extern INT PARSER_semi_tok;
extern INT PARSER_type_tok;
extern INT PRINT_1541629256;
extern INT SFILE_ID_line;
extern INT SFILE_ID_pos;
extern MANGLE PRINT_OB_mangler;
extern OUT_MODE MODES_out_mode;
extern TP_CLASS TP_BUI1813155122;

/* Function declarations */


extern RETURNED_CONST AS_CLASS_DEF (**dPARSE1581792481)(dPARSE, IDENT, INT);

extern RETURNED_CONST BOOL (**dMODE_814247358)(dMODE, dMODE);

extern RETURNED_CONST BOOL (**dTP_is124163553)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1334578382)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1811059018)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1999456142)(dTP, dTP);

extern RETURNED_CONST BOOL (**dTP_is735947028)(dTP);

extern RETURNED_CONST IMPL (**dTP_implrIMPL)(dTP);

extern RETURNED_CONST SFILE_ID (**dAS_CL474111786)(dAS_CLASS_ELT);

extern RETURNED_CONST SFILE_ID (**dPROG_848621491)(dPROG_ERR);

extern RETURNED_CONST STR (**dSTR_strrSTR)(dSTR);

extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);
AM_LOCAL_EXPR FLISTA2119642552(FLISTA2119642552_frame);
ARRAYATTRSORT ARRAYA1114043836(ARRAYATTRSORT, INT);
AS_CLASS_DEF PARSER1968454579(PARSER);
AS_CLASS_DEF PARSER2058766590(PARSER);
AS_CLASS_DEF PARSER525553873(PARSER);
ATTRSORT ARRAYA1660316962(ARRAYA1660316962_frame);
BOOL CGEN_a952489530(CGEN);
BOOL ELT_is380037919(ELT);
BOOL PARSER1883080161(PARSER, TOKEN);
BOOL PARSER2128430730(PARSER, INT);
BOOL PRINT_1038801195(PRINT_OB, TUPSIGINT, TUPSIGINT);
BOOL PROG_e596344618(PROG, SFILE_ID);
BOOL STR_is111530248(STR, STR);
BOOL STR_is171745744(STR, STR);
CODE_FILE CODE_F1432464034(CODE_FILE, STR);
ELT ELT_TBL_eltbrELT(ELT_TBL_eltbrELT_frame);
FLISTSFILE_ID FLISTS1155895163(FLISTSFILE_ID, SFILE_ID);
FLISTSTR FLISTS1425610062(FLISTSTR, STR);
FSETSTR FSETST1404644833(FSETSTR, STR);
FSETdTP FSETdT1016814448(FSETdTP, dTP);
FSTR FSTR_c1307336863(FSTR, STR);
FSTR FSTR_c1998740379(FSTR, INT);
FSTR FSTR_p1410513982(FSTR, FSTR);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
FSTR PRINT_1557717939(PRINT_OB, PROG, CGEN);
INT AM_ROU1569581452(AM_ROU1569581452_frame);
INT ELT_TBL_sizerINT(ELT_TBL);
INT FLISTI1321626366(FLISTINT);
INT FLISTS228922656(FLISTSIG);
INT FLISTS434496825(FLISTSFILE_ID);
INT FMAPdT1072907830(FMAPdT1072907830_frame);
INT IMPL_a1897060099(IMPL);
INT INT_up418511718(INT_up418511718_frame);
INT INT_upbrINT(INT_upbrINT_frame);
INT PRINT_1524431106(PRINT_OB, CGEN, SFILE_ID);
INT SFILE_1102877860(SFILE_ID);
INT SFILE_2070755978(SFILE_ID);
INT STR_sizerINT(STR);
SFILE_ID SFILE_1594766561(SFILE_ID);
SIG FLISTS1074219534(FLISTS1074219534_frame);
STR FLISTS1171631134(FLISTS1171631134_frame);
STR FLISTSTR_poprSTR(FLISTSTR);
STR FLISTSTR_toprSTR(FLISTSTR);
STR MANGLE119219986(MANGLE, OB, OB);
STR SFILE_ID_strrSTR(SFILE_ID);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap2004550586(STR, STR);
STR STR_fr1097270334(STR, FSTR);
STR TOKEN_strrSTR(TOKEN);
TUPSIGINT ARRAYT1593820953(ARRAYT1593820953_frame);
TUPdTPINT FMAPdT1857608820(FMAPdT1857608820_frame);
dLAYOUT LAYOUT79932797(LAYOUT_TBL, dTP);
void ARRAYA107911980(ARRAYA107911980_frame);
void ARRAYA487581188(ARRAYATTRSORT, INT, INT);
void ARRAYT1024744539(ARRAYT1024744539_frame);
void ARRAYT215015885(ARRAYTUPSIGINT, ROUTTU467247893);
void AS_CLA1288691224(AS_CLASS_DEF, AS_CLASS_DEF);
void BFILE_plus_STR(BFILE, STR);
void CODE_F525364269(CODE_FILE, dLAYOUT);
void CODE_F65146505(CODE_FILE);
void CONVER840793676(CONVERT, CHAR);
void CONVER911997526(CONVERT);
void FILE_plus_STR(FILE1, STR);
void FILE_plus_dSTR(FILE1, dSTR);
void PRINT_116660897(PRINT_OB, CODE_FILE, dTP, dTP, STR);
void PRINT_463825405(PRINT_OB, PROG, CGEN);
void PRINT_481601799(PRINT_OB, PROG, CGEN, CODE_FILE);
void PRINT_721582855(PRINT_OB, PROG, CGEN, CODE_FILE, AM_ROUT_DEF, FLISTA725283029);
void PRINT_791786249(PRINT_OB, PROG, CGEN, INT, INT);
void PROG_e176405615(PROG, dPROG_ERR);
void PROG_err_STR(PROG, STR);
void PROG_message_STR(PROG, STR);
void PROG_warning_STR(PROG, STR);

/* Bound rout stubs decls */

BOOL bound1(bound1_ob, TUPSIGINT, TUPSIGINT);

/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

AS_CLASS_DEF PARSER2058766590(PARSER self) {
 AS_CLASS_DEF ret_val;
 AS_CLASS_DEF res = ((AS_CLASS_DEF) NULL);
 PARSER enter_self;
 STR enter_s;
 CONVERT set_start_self;
 INT set_start_i = INT_zero;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val1 = BOOL_zero;
 TOKEN is_eq_self1 = TOKEN_zero;
 INT is_eq_y1 = INT_zero;
 BOOL ret_val2 = BOOL_zero;
 TOKEN is_eq_self2 = TOKEN_zero;
 INT is_eq_y2 = INT_zero;
 BOOL ret_val3 = BOOL_zero;
 PARSER error_self;
 STR error_msg;
 PARSER source_loc_self;
 SFILE_ID ret_val4 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val6;
 STR plus_self2;
 CHAR plus_arg = CHAR_zero;
 STR ret_val7;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val8;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val9;
 STR r;
 PARSER err_self;
 STR err_s;
 TOKEN is_eq_self3 = TOKEN_zero;
 INT is_eq_y3 = INT_zero;
 BOOL ret_val10 = BOOL_zero;
 PARSER error_self1;
 STR error_msg1;
 PARSER source_loc_self1;
 SFILE_ID ret_val11 = SFILE_ID_zero;
 PROG set_eloc_self1;
 SFILE_ID set_eloc_l1 = SFILE_ID_zero;
 STR plus_self3;
 STR plus_sarg2;
 STR ret_val12;
 STR plus_self4;
 STR plus_sarg3;
 STR ret_val13;
 STR plus_self5;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val14;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val15;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val16;
 STR r1;
 PARSER err_self1;
 STR err_s1;
 CONVERT convert_type_def;
 CONVERT set_start_self1;
 BFILE flush_self;
 PARSER exp_error_self;
 STR exp_error_msg;
 STR plus_self6;
 STR plus_sarg4;
 STR ret_val17;
 STR plus_self7;
 STR plus_sarg5;
 STR ret_val18;
 PARSER error_self2;
 STR error_msg2;
 PARSER source_loc_self2;
 SFILE_ID ret_val19 = SFILE_ID_zero;
 PROG set_eloc_self2;
 SFILE_ID set_eloc_l2 = SFILE_ID_zero;
 STR plus_self8;
 STR plus_sarg6;
 STR ret_val20;
 STR plus_self9;
 STR plus_sarg7;
 STR ret_val21;
 STR plus_self10;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val22;
 STR s2;
 CHAR str_self2 = CHAR_zero;
 STR ret_val23;
 STR create_self2;
 CHAR create_c2 = CHAR_zero;
 STR ret_val24;
 STR r2;
 PARSER err_self2;
 STR err_s2;
 TOKEN is_eq_self4 = TOKEN_zero;
 INT is_eq_y4 = INT_zero;
 BOOL ret_val25 = BOOL_zero;
 PARSER exp_error_self1;
 STR exp_error_msg1;
 STR plus_self11;
 STR plus_sarg8;
 STR ret_val26;
 STR plus_self12;
 STR plus_sarg9;
 STR ret_val27;
 PARSER error_self3;
 STR error_msg3;
 PARSER source_loc_self3;
 SFILE_ID ret_val28 = SFILE_ID_zero;
 PROG set_eloc_self3;
 SFILE_ID set_eloc_l3 = SFILE_ID_zero;
 STR plus_self13;
 STR plus_sarg10;
 STR ret_val29;
 STR plus_self14;
 STR plus_sarg11;
 STR ret_val30;
 STR plus_self15;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val31;
 STR s3;
 CHAR str_self3 = CHAR_zero;
 STR ret_val32;
 STR create_self3;
 CHAR create_c3 = CHAR_zero;
 STR ret_val33;
 STR r3;
 PARSER err_self3;
 STR err_s3;
 CONVERT set_end_self;
 PARSER close_file_self;
 SCANNER close_file_self1;
 SFILE_ID close_file_self2 = SFILE_ID_zero;
 FLISTINT aset_self;
 INT aset_ind = INT_zero;
 INT aset_val = INT_zero;
 PARSER exit_self;
 STR s4;
 extern STR sourcefile;
 BOOL L1;
 INT L2;
 BOOL L31_;
 INT L4;
 BOOL L51_;
 BOOL L6;
 INT L7;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 extern STR keywor710852211;
 extern STR in1;
 STR L11;
 INT L12;
 OB L13;
 INT L14;
 BOOL L16;
 INT L17;
 BOOL L181_;
 extern STR keywor1399248125;
 extern STR in1;
 STR L19;
 INT L20;
 OB L21;
 INT L22;
 extern STR abstractclass;
 INT L24;
 INT L251_;
 extern STR semicolon;
 extern STR expectedbutfound;
 extern STR in1;
 STR L26;
 INT L27;
 OB L28;
 INT L29;
 INT L32;
 BOOL L331_;
 BOOL L34;
 BOOL L351_;
 extern STR endoffile;
 extern STR expectedbutfound;
 extern STR in1;
 STR L36;
 INT L37;
 OB L38;
 INT L39;
 INT L41;
 INT L421_;
 BOOL L43;
 BOOL L441_;
 INT L45;
 INT L461_;
 enter_self = self;
 enter_s = ((STR) &sourcefile);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 set_start_self = ATTR(self,convert);
 set_start_i = 0;
 if (ATTR(set_start_self,need_to_convert)) {
  SATTR(set_start_self,start_pos,0);
 }
 while (1) {
  if (PARSER1883080161(self, ATTR(self,next))) {
   is_eq_self = ATTR(self,next);
   is_eq_y = PARSER_type_tok;
   L2 = is_eq_self.val1;
   L31_=(L2)==(is_eq_y); 
   ret_val1 = L31_;
   if (ret_val1) {
    L1 = TRUE;
   } else {
    is_eq_self1 = ATTR(self,next);
    is_eq_y1 = PARSER1701976116;
    L4 = is_eq_self1.val1;
    L51_=(L4)==(is_eq_y1); 
    ret_val2 = L51_;
    L1 = ret_val2;
   }
   if (L1) {
    is_eq_self2 = ATTR(self,next);
    is_eq_y2 = PARSER_type_tok;
    L7 = is_eq_self2.val1;
    L81_=(L7)==(is_eq_y2); 
    ret_val3 = L81_;
    if (ret_val3) {
     L9 = ATTR(self,version_1_0);
     L101_=!(L9); 
     L6 = L101_;
    } else {
     L6 = FALSE;
    }
    if (L6) {
     error_self = self;
     error_msg = ((STR) &keywor710852211);
     set_eloc_self = ATTR(error_self,prog);
     source_loc_self = error_self;
     ret_val4 = SFILE_1594766561(SFILE_ID_zero);
     set_eloc_l = ret_val4;
     SATTR(set_eloc_self,eloc,set_eloc_l);
     err_self = error_self;
     plus_self = error_msg;
     plus_sarg = ((STR) &in1);
     ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
     plus_self1 = ret_val5;
     plus_sarg1 = FLISTSTR_toprSTR(ATTR(error_self,entered));
     ret_val6 = STR_ap2004550586(plus_self1, plus_sarg1);
     plus_self2 = ret_val6;
     plus_arg = ')';
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

     L11 = ((STR) L13);
     L11->asize = L12;
     r = L11;
     SARR((STR)r,0,create_c); 
     ;
     ret_val9 = r;
     ret_val8 = ret_val9;
     s = ret_val8;
     ret_val7 = STR_ap1077157958(plus_self2, s, TRUE);
     err_s = ret_val7;
     PROG_err_STR(ATTR(err_self,prog), err_s);
    }
    is_eq_self3 = ATTR(self,next);
    is_eq_y3 = PARSER1701976116;
    L17 = is_eq_self3.val1;
    L181_=(L17)==(is_eq_y3); 
    ret_val10 = L181_;
    if (ret_val10) {
     L16 = ATTR(self,version_1_0);
    } else {
     L16 = FALSE;
    }
    if (L16) {
     error_self1 = self;
     error_msg1 = ((STR) &keywor1399248125);
     set_eloc_self1 = ATTR(error_self1,prog);
     source_loc_self1 = error_self1;
     ret_val11 = SFILE_1594766561(SFILE_ID_zero);
     set_eloc_l1 = ret_val11;
     SATTR(set_eloc_self1,eloc,set_eloc_l1);
     err_self1 = error_self1;
     plus_self3 = error_msg1;
     plus_sarg2 = ((STR) &in1);
     ret_val12 = STR_ap2004550586(plus_self3, plus_sarg2);
     plus_self4 = ret_val12;
     plus_sarg3 = FLISTSTR_toprSTR(ATTR(error_self1,entered));
     ret_val13 = STR_ap2004550586(plus_self4, plus_sarg3);
     plus_self5 = ret_val13;
     plus_arg1 = ')';
     str_self1 = plus_arg1;
     create_self1 = ((STR) NULL);
     create_c1 = str_self1;
     L20 = 1;
     L22=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L20)*sizeof(CHAR);
     L21=ZALLOC_LEAF_BIG(L22);
     if (L21==NULL) FATAL("Unable to allocate more memory");
     memset(L21,0,L22);
     ((OB)L21)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L21)->header.destroyed=0;
#endif

     L19 = ((STR) L21);
     L19->asize = L20;
     r1 = L19;
     SARR((STR)r1,0,create_c1); 
     ;
     ret_val16 = r1;
     ret_val15 = ret_val16;
     s1 = ret_val15;
     ret_val14 = STR_ap1077157958(plus_self5, s1, TRUE);
     err_s1 = ret_val14;
     PROG_err_STR(ATTR(err_self1,prog), err_s1);
    }
    convert_type_def = ATTR(self,convert);
    if (ATTR(convert_type_def,need_to_convert)) {
     CONVER840793676(convert_type_def, 't');
     BFILE_plus_STR(ATTR(convert_type_def,file1), ((STR) &abstractclass));
     set_start_self1 = convert_type_def;
     if (ATTR(set_start_self1,need_to_convert)) {
      L24 = SFILE_ID_pos;
      L251_=INTMINUS(L24,1); 
      SATTR(set_start_self1,start_pos,L251_);
     }
     flush_self = ATTR(convert_type_def,file1);
     fflush(ATTR(flush_self,fp));
    }
    if ((res==((AS_CLASS_DEF) NULL))) {
     res = PARSER1968454579(self);
    }
    else {
     AS_CLA1288691224(res, PARSER1968454579(self));
    }
   }
   else {
    if ((res==((AS_CLASS_DEF) NULL))) {
     res = PARSER525553873(self);
    }
    else {
     AS_CLA1288691224(res, PARSER525553873(self));
    }
   }
  }
  if (PARSER2128430730(self, PARSER_semi_tok)) {
  }
  else {
   if (PARSER1883080161(self, ATTR(self,next))) {
    exp_error_self = self;
    exp_error_msg = ((STR) &semicolon);
    error_self2 = exp_error_self;
    plus_self6 = exp_error_msg;
    plus_sarg4 = ((STR) &expectedbutfound);
    ret_val17 = STR_ap2004550586(plus_self6, plus_sarg4);
    plus_self7 = ret_val17;
    plus_sarg5 = TOKEN_strrSTR(ATTR(exp_error_self,next));
    ret_val18 = STR_ap2004550586(plus_self7, plus_sarg5);
    error_msg2 = ret_val18;
    set_eloc_self2 = ATTR(error_self2,prog);
    source_loc_self2 = error_self2;
    ret_val19 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l2 = ret_val19;
    SATTR(set_eloc_self2,eloc,set_eloc_l2);
    err_self2 = error_self2;
    plus_self8 = error_msg2;
    plus_sarg6 = ((STR) &in1);
    ret_val20 = STR_ap2004550586(plus_self8, plus_sarg6);
    plus_self9 = ret_val20;
    plus_sarg7 = FLISTSTR_toprSTR(ATTR(error_self2,entered));
    ret_val21 = STR_ap2004550586(plus_self9, plus_sarg7);
    plus_self10 = ret_val21;
    plus_arg2 = ')';
    str_self2 = plus_arg2;
    create_self2 = ((STR) NULL);
    create_c2 = str_self2;
    L27 = 1;
    L29=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L27)*sizeof(CHAR);
    L28=ZALLOC_LEAF_BIG(L29);
    if (L28==NULL) FATAL("Unable to allocate more memory");
    memset(L28,0,L29);
    ((OB)L28)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L28)->header.destroyed=0;
#endif

    L26 = ((STR) L28);
    L26->asize = L27;
    r2 = L26;
    SARR((STR)r2,0,create_c2); 
    ;
    ret_val24 = r2;
    ret_val23 = ret_val24;
    s2 = ret_val23;
    ret_val22 = STR_ap1077157958(plus_self10, s2, TRUE);
    err_s2 = ret_val22;
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
   }
   else {
    is_eq_self4 = ATTR(self,next);
    is_eq_y4 = PARSER_eof_tok;
    L32 = is_eq_self4.val1;
    L331_=(L32)==(is_eq_y4); 
    ret_val25 = L331_;
    L34 = ret_val25;
    L351_=!(L34); 
    if (L351_) {
     exp_error_self1 = self;
     exp_error_msg1 = ((STR) &endoffile);
     error_self3 = exp_error_self1;
     plus_self11 = exp_error_msg1;
     plus_sarg8 = ((STR) &expectedbutfound);
     ret_val26 = STR_ap2004550586(plus_self11, plus_sarg8);
     plus_self12 = ret_val26;
     plus_sarg9 = TOKEN_strrSTR(ATTR(exp_error_self1,next));
     ret_val27 = STR_ap2004550586(plus_self12, plus_sarg9);
     error_msg3 = ret_val27;
     set_eloc_self3 = ATTR(error_self3,prog);
     source_loc_self3 = error_self3;
     ret_val28 = SFILE_1594766561(SFILE_ID_zero);
     set_eloc_l3 = ret_val28;
     SATTR(set_eloc_self3,eloc,set_eloc_l3);
     err_self3 = error_self3;
     plus_self13 = error_msg3;
     plus_sarg10 = ((STR) &in1);
     ret_val29 = STR_ap2004550586(plus_self13, plus_sarg10);
     plus_self14 = ret_val29;
     plus_sarg11 = FLISTSTR_toprSTR(ATTR(error_self3,entered));
     ret_val30 = STR_ap2004550586(plus_self14, plus_sarg11);
     plus_self15 = ret_val30;
     plus_arg3 = ')';
     str_self3 = plus_arg3;
     create_self3 = ((STR) NULL);
     create_c3 = str_self3;
     L37 = 1;
     L39=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L37)*sizeof(CHAR);
     L38=ZALLOC_LEAF_BIG(L39);
     if (L38==NULL) FATAL("Unable to allocate more memory");
     memset(L38,0,L39);
     ((OB)L38)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L38)->header.destroyed=0;
#endif

     L36 = ((STR) L38);
     L36->asize = L37;
     r3 = L36;
     SARR((STR)r3,0,create_c3); 
     ;
     ret_val33 = r3;
     ret_val32 = ret_val33;
     s3 = ret_val32;
     ret_val31 = STR_ap1077157958(plus_self15, s3, TRUE);
     err_s3 = ret_val31;
     PROG_err_STR(ATTR(err_self3,prog), err_s3);
    }
    goto after_loop;
   }
  }
 }
 after_loop: ;
 set_end_self = ATTR(self,convert);
 if (ATTR(set_end_self,need_to_convert)) {
  L41 = SFILE_ID_pos;
  L421_=INTMINUS(L41,1); 
  SATTR(set_end_self,end_pos,L421_);
  CONVER911997526(set_end_self);
 }
 close_file_self = self;
 close_file_self1 = ATTR(close_file_self,scanner);
 close_file_self2 = SFILE_ID_zero;
 L43 = (SFILE_ID_source==((FSTR) NULL));
 L441_=!(L43); 
 if (L441_) {
  aset_self = SFILE_ID_lines;
  L45 = FLISTI1321626366(SFILE_ID_lines);
  L461_=INTMINUS(L45,1); 
  aset_ind = L461_;
  aset_val = SFILE_ID_line;
  SARR((FLISTINT)aset_self,aset_ind,aset_val); 
  ;
  SFILE_ID_source = ((FSTR) NULL);
 }
 exit_self = self;
 s4 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL PRINT_1038801195(PRINT_OB self, TUPSIGINT t1, TUPSIGINT t2) {
 BOOL ret_val = BOOL_zero;
 ret_val = STR_is171745744(ATTR(t1.t1,str), ATTR(t2.t1,str));
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL PROG_e596344618(PROG self, SFILE_ID l) {
 BOOL ret_val = BOOL_zero;
 INT i = INT_zero;
 FLISTSFILE_ID aget_self;
 INT aget_ind = INT_zero;
 SFILE_ID ret_val1 = SFILE_ID_zero;
 SFILE_ID is_eq_self = SFILE_ID_zero;
 SFILE_ID is_eq_y = SFILE_ID_zero;
 BOOL ret_val2 = BOOL_zero;
 INT L1;
 BOOL L21_;
 SFILE_ID L31_;
 INT L4;
 INT L5;
 BOOL L61_;
 INT L71_;
 i = 0;
 if ((ATTR(self,err_list)==((FLISTSFILE_ID) NULL))) {
  SATTR(self,err_list,FLISTS1155895163(ATTR(self,err_list), l));
  ret_val = FALSE;
  return ret_val;
 }
 while (1) {
  L1 = FLISTS434496825(ATTR(self,err_list));
  L21_=(i)<(L1); 
  if (L21_) {
  }
  else {
   goto after_loop;
  }
  is_eq_self = l;
  aget_self = ATTR(self,err_list);
  aget_ind = i;
  L31_=ARR((FLISTSFILE_ID)aget_self,aget_ind); 
  ret_val1 = L31_;
  is_eq_y = ret_val1;
  L4 = is_eq_self.loc;
  L5 = is_eq_y.loc;
  L61_=(L4)==(L5); 
  ret_val2 = L61_;
  if (ret_val2) {
   ret_val = TRUE;
   return ret_val;
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 SATTR(self,err_list,FLISTS1155895163(ATTR(self,err_list), l));
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSTR PRINT_1557717939(PRINT_OB self, PROG prog, CGEN cgen) {
 FSTR ret_val;
 FSTR dec;
 INT file_count = INT_zero;
 INT fc = INT_zero;
 CODE_FILE p;
 TP_CLASS_TBL clt;
 INT smallest_tag = INT_zero;
 INT largest_tag = INT_zero;
 FMAPdTPINT L11;
 INT n = INT_zero;
 INT last_largest = INT_zero;
 INT last_smallest = INT_zero;
 INT tg = INT_zero;
 FMAPdTPINT L21;
 INT n11 = INT_zero;
 dTP cl;
 FMAPdTPINT L31;
 TUPdTPINT pr = TUPdTPINT_zero;
 FSTR fname;
 ARRAYATTRSORT as = ((ARRAYATTRSORT) NULL);
 INT attrs = INT_zero;
 ELT_TBL L41;
 STR n2;
 ELT_TBL L51;
 ELT c;
 ARRAYATTRSORT L61;
 STR tps = ((STR) NULL);
 BOOL first1 = BOOL_zero;
 INT i = INT_zero;
 ARRAYATTRSORT L71;
 ELT e;
 dTP arr;
 INT str_self = INT_zero;
 STR ret_val1;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val2;
 FSTR str_self1;
 STR ret_val3;
 STR plus_self;
 STR plus_sarg;
 STR ret_val4;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val5;
 STR plus_self2;
 INT plus_arg = INT_zero;
 STR ret_val6;
 STR s;
 INT str_self2 = INT_zero;
 STR ret_val7;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val8;
 FSTR str_self3;
 STR ret_val9;
 STR plus_self3;
 STR plus_sarg2;
 STR ret_val10;
 PROG barf_self;
 STR barf_msg;
 PROG barf_at_self;
 STR barf_at_msg;
 dPROG_ERR barf_at_at;
 STR plus_self4;
 STR plus_sarg3;
 STR ret_val11;
 UNIX exit_self;
 INT exit_code = INT_zero;
 INT str_self4 = INT_zero;
 STR ret_val12;
 FSTR buf3 = ((FSTR) NULL);
 FSTR clear_self2;
 INT str_in_self2 = INT_zero;
 FSTR str_in_s2;
 FSTR ret_val13;
 FSTR str_self5;
 STR ret_val14;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val15;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val16;
 CODE_FILE uses_tp_self;
 dTP uses_tp_t;
 PRINT_OB mkfname_self;
 dTP mkfname_cl;
 FSTR ret_val17;
 PRINT_OB mang_self;
 OB mang_ob;
 STR ret_val18;
 STR fstr_self;
 FSTR ret_val19;
 ARRAYATTRSORT create_self;
 INT create_n = INT_zero;
 ARRAYATTRSORT ret_val20;
 ELT name_self;
 IDENT ret_val21 = IDENT_zero;
 ATTRSORT create_self1;
 STR create_n1;
 ELT create_e;
 ATTRSORT ret_val22;
 ATTRSORT r;
 ARRAYATTRSORT sort_self;
 CODE_FILE plus_self7;
 dSTR plus_s;
 CODE_FILE ret_val23;
 CODE_FILE plus_self8;
 dSTR plus_s1;
 CODE_FILE ret_val24;
 CODE_FILE plus_self9;
 dSTR plus_s2;
 CODE_FILE ret_val25;
 CODE_FILE plus_self10;
 dSTR plus_s3;
 CODE_FILE ret_val26;
 CODE_FILE plus_self11;
 dSTR plus_s4;
 CODE_FILE ret_val27;
 CODE_FILE plus_self12;
 dSTR plus_s5;
 CODE_FILE ret_val28;
 CODE_FILE plus_self13;
 dSTR plus_s6;
 CODE_FILE ret_val29;
 PRINT_OB mang_self1;
 OB mang_ob1;
 STR ret_val30;
 STR plus_self14;
 STR plus_sarg6;
 STR ret_val31;
 STR plus_self15;
 STR plus_sarg7;
 STR ret_val32;
 PRINT_OB mang_self2;
 OB mang_ob2;
 STR ret_val33;
 STR plus_self16;
 STR plus_sarg8;
 STR ret_val34;
 STR plus_self17;
 STR plus_sarg9;
 STR ret_val35;
 CODE_FILE plus_self18;
 dSTR plus_s7;
 CODE_FILE ret_val36;
 CODE_FILE plus_self19;
 dSTR plus_s8;
 CODE_FILE ret_val37;
 CODE_FILE plus_self20;
 dSTR plus_s9;
 CODE_FILE ret_val38;
 CODE_FILE plus_self21;
 dSTR plus_s10;
 CODE_FILE ret_val39;
 CODE_FILE plus_self22;
 dSTR plus_s11;
 CODE_FILE ret_val40;
 CODE_FILE plus_self23;
 dSTR plus_s12;
 CODE_FILE ret_val41;
 CODE_FILE plus_self24;
 dSTR plus_s13;
 CODE_FILE ret_val42;
 CODE_FILE plus_self25;
 dSTR plus_s14;
 CODE_FILE ret_val43;
 CODE_FILE plus_self26;
 dSTR plus_s15;
 CODE_FILE ret_val44;
 CODE_FILE plus_self27;
 dSTR plus_s16;
 CODE_FILE ret_val45;
 CODE_FILE plus_self28;
 dSTR plus_s17;
 CODE_FILE ret_val46;
 CODE_FILE plus_self29;
 dSTR plus_s18;
 CODE_FILE ret_val47;
 CODE_FILE plus_self30;
 dSTR plus_s19;
 CODE_FILE ret_val48;
 CODE_FILE plus_self31;
 dSTR plus_s20;
 CODE_FILE ret_val49;
 CODE_FILE plus_self32;
 dSTR plus_s21;
 CODE_FILE ret_val50;
 PRINT_OB mang_self3;
 OB mang_ob3;
 STR ret_val51;
 CODE_FILE plus_self33;
 dSTR plus_s22;
 CODE_FILE ret_val52;
 CODE_FILE plus_self34;
 dSTR plus_s23;
 CODE_FILE ret_val53;
 CODE_FILE plus_self35;
 dSTR plus_s24;
 CODE_FILE ret_val54;
 CODE_FILE plus_self36;
 dSTR plus_s25;
 CODE_FILE ret_val55;
 CODE_FILE plus_self37;
 dSTR plus_s26;
 CODE_FILE ret_val56;
 CODE_FILE plus_self38;
 dSTR plus_s27;
 CODE_FILE ret_val57;
 PRINT_OB mang_self4;
 OB mang_ob4;
 STR ret_val58;
 CODE_FILE plus_self39;
 dSTR plus_s28;
 CODE_FILE ret_val59;
 CODE_FILE plus_self40;
 dSTR plus_s29;
 CODE_FILE ret_val60;
 PRINT_OB mang_self5;
 OB mang_ob5;
 STR ret_val61;
 CODE_FILE plus_self41;
 dSTR plus_s30;
 CODE_FILE ret_val62;
 CODE_FILE plus_self42;
 dSTR plus_s31;
 CODE_FILE ret_val63;
 CODE_FILE plus_self43;
 dSTR plus_s32;
 CODE_FILE plus_self44;
 dSTR plus_s33;
 CODE_FILE ret_val64;
 CODE_FILE plus_self45;
 dSTR plus_s34;
 CODE_FILE ret_val65;
 PRINT_OB mang_self6;
 OB mang_ob6;
 STR ret_val66;
 CODE_FILE plus_self46;
 dSTR plus_s35;
 CODE_FILE ret_val67;
 CODE_FILE plus_self47;
 dSTR plus_s36;
 CODE_FILE ret_val68;
 CODE_FILE plus_self48;
 dSTR plus_s37;
 CODE_FILE ret_val69;
 PRINT_OB mang_self7;
 OB mang_ob7;
 STR ret_val70;
 CODE_FILE plus_self49;
 dSTR plus_s38;
 CODE_FILE ret_val71;
 CODE_FILE plus_self50;
 dSTR plus_s39;
 CODE_FILE ret_val72;
 CODE_FILE plus_self51;
 dSTR plus_s40;
 CODE_FILE ret_val73;
 CODE_FILE plus_self52;
 dSTR plus_s41;
 CODE_FILE plus_self53;
 dSTR plus_s42;
 CODE_FILE ret_val74;
 CODE_FILE plus_self54;
 dSTR plus_s43;
 CODE_FILE plus_self55;
 dSTR plus_s44;
 CODE_FILE ret_val75;
 CODE_FILE plus_self56;
 dSTR plus_s45;
 CODE_FILE ret_val76;
 CODE_FILE plus_self57;
 dSTR plus_s46;
 CODE_FILE ret_val77;
 CODE_FILE plus_self58;
 dSTR plus_s47;
 CODE_FILE ret_val78;
 CODE_FILE plus_self59;
 dSTR plus_s48;
 CODE_FILE ret_val79;
 CODE_FILE plus_self60;
 dSTR plus_s49;
 CODE_FILE ret_val80;
 CODE_FILE plus_self61;
 dSTR plus_s50;
 CODE_FILE ret_val81;
 CODE_FILE plus_self62;
 dSTR plus_s51;
 CODE_FILE ret_val82;
 CODE_FILE plus_self63;
 dSTR plus_s52;
 CODE_FILE ret_val83;
 CODE_FILE plus_self64;
 dSTR plus_s53;
 CODE_FILE ret_val84;
 CODE_FILE plus_self65;
 dSTR plus_s54;
 CODE_FILE ret_val85;
 CODE_FILE plus_self66;
 dSTR plus_s55;
 CODE_FILE ret_val86;
 CODE_FILE plus_self67;
 dSTR plus_s56;
 INT L81 = INT_zero;
 CODE_FILE plus_self68;
 dSTR plus_s57;
 CODE_FILE plus_self69;
 dSTR plus_s58;
 CODE_FILE ret_val87;
 ELT name_self1;
 IDENT ret_val88 = IDENT_zero;
 CODE_FILE plus_self70;
 dSTR plus_s59;
 CODE_FILE ret_val89;
 CODE_FILE plus_self71;
 dSTR plus_s60;
 CODE_FILE ret_val90;
 ELT name_self2;
 IDENT ret_val91 = IDENT_zero;
 PRINT_OB mang_self8;
 OB mang_ob8;
 OB mang_ns;
 STR ret_val92;
 CODE_FILE plus_self72;
 dSTR plus_s61;
 CODE_FILE ret_val93;
 CODE_FILE plus_self73;
 dSTR plus_s62;
 CODE_FILE ret_val94;
 ELT ret_self;
 dTP ret_val95;
 CGEN tag_for_self;
 dTP tag_for_tp;
 STR ret_val96;
 STR res;
 CGEN mang_self9;
 OB mang_ob9;
 STR ret_val97;
 CGEN forbid_self;
 STR forbid_s;
 MANGLE forbid_self1;
 STR forbid_s1;
 CODE_FILE plus_self74;
 dSTR plus_s63;
 CODE_FILE ret_val98;
 CODE_FILE plus_self75;
 dSTR plus_s64;
 CODE_FILE ret_val99;
 PRINT_OB mang_self10;
 OB mang_ob10;
 STR ret_val100;
 CODE_FILE plus_self76;
 dSTR plus_s65;
 CODE_FILE plus_self77;
 dSTR plus_s66;
 CODE_FILE ret_val101;
 CODE_FILE plus_self78;
 dSTR plus_s67;
 CODE_FILE ret_val102;
 ELT name_self3;
 IDENT ret_val103 = IDENT_zero;
 PRINT_OB mang_self11;
 OB mang_ob11;
 OB mang_ns1;
 STR ret_val104;
 CODE_FILE plus_self79;
 dSTR plus_s68;
 CODE_FILE ret_val105;
 CODE_FILE plus_self80;
 dSTR plus_s69;
 CODE_FILE ret_val106;
 CODE_FILE plus_self81;
 dSTR plus_s70;
 CODE_FILE ret_val107;
 CODE_FILE plus_self82;
 dSTR plus_s71;
 CODE_FILE ret_val108;
 CODE_FILE plus_self83;
 dSTR plus_s72;
 CODE_FILE plus_self84;
 dSTR plus_s73;
 CODE_FILE ret_val109;
 PRINT_OB mang_self12;
 OB mang_ob12;
 STR ret_val110;
 CODE_FILE plus_self85;
 dSTR plus_s74;
 CODE_FILE ret_val1111;
 CODE_FILE plus_self86;
 dSTR plus_s75;
 CODE_FILE ret_val112;
 CODE_FILE plus_self87;
 dSTR plus_s76;
 CODE_FILE ret_val113;
 PRINT_OB mang_self13;
 OB mang_ob13;
 STR ret_val114;
 CODE_FILE plus_self88;
 dSTR plus_s77;
 CODE_FILE ret_val115;
 CODE_FILE plus_self89;
 dSTR plus_s78;
 CODE_FILE ret_val116;
 CODE_FILE plus_self90;
 dSTR plus_s79;
 CODE_FILE ret_val117;
 CODE_FILE plus_self91;
 dSTR plus_s80;
 CODE_FILE ret_val118;
 CGEN tag_for_self1;
 dTP tag_for_tp1;
 STR ret_val119;
 STR res1;
 CGEN mang_self14;
 OB mang_ob14;
 STR ret_val120;
 CGEN forbid_self2;
 STR forbid_s2;
 MANGLE forbid_self3;
 STR forbid_s3;
 CODE_FILE plus_self92;
 dSTR plus_s81;
 CODE_FILE ret_val121;
 CODE_FILE plus_self93;
 dSTR plus_s82;
 CODE_FILE ret_val122;
 PRINT_OB mang_self15;
 OB mang_ob15;
 STR ret_val123;
 CODE_FILE plus_self94;
 dSTR plus_s83;
 CODE_FILE ret_val124;
 CODE_FILE plus_self95;
 dSTR plus_s84;
 CODE_FILE ret_val125;
 CODE_FILE plus_self96;
 dSTR plus_s85;
 CODE_FILE ret_val126;
 CODE_FILE plus_self97;
 dSTR plus_s86;
 CODE_FILE ret_val127;
 CODE_FILE plus_self98;
 dSTR plus_s87;
 CODE_FILE ret_val128;
 FSTR plus_self99;
 INT plus_i = INT_zero;
 FSTR ret_val129;
 INT str_in_self3 = INT_zero;
 FSTR str_in_s3;
 FSTR ret_val130;
 extern STR Print;
 BOOL L9;
 BOOL L101_;
 extern STR c1;
 INT L12;
 BOOL L131_;
 BOOL L141_;
 BOOL L151_;
 BOOL L161_;
 INT L171_;
 BOOL L181_;
 BOOL L191_;
 INT L201_;
 BOOL L221_;
 BOOL L23;
 BOOL L241_;
 BOOL L251_;
 BOOL L26;
 BOOL L271_;
 INT L28;
 BOOL L291_;
 BOOL L301_;
 BOOL L321_;
 INT L331_;
 BOOL L341_;
 INT L351_;
 BOOL L361_;
 BOOL L37;
 BOOL L381_;
 TUPdTPINT L39;
 INT L40;
 BOOL L421_;
 extern STR theres458619408;
 BOOL L43;
 BOOL L441_;
 extern STR name9;
 extern STR Intern28965746;
 BOOL L451_;
 INT L461_;
 extern STR Print;
 BOOL L47;
 BOOL L481_;
 extern STR c1;
 INT L491_;
 BOOL L50;
 BOOL L521_;
 BOOL L53;
 dTP L54;
 BOOL L55;
 BOOL L561_;
 dTP L57;
 BOOL L58;
 BOOL L591_;
 dTP L60;
 IMPL L62;
 ARRAYATTRSORT L63;
 INT L64;
 OB L65;
 INT L66;
 INT L671_br;
INT i_L671_=0;
 ELT L68;
 dTP L69;
 IMPL L70;
 IDENT L72;
 ELT L73;
 dTP L74;
 IMPL L75;
 ATTRSORT L76;
 OB L77;
 ATTRSORT L78;
 INT L791_;
 BOOL L80;
 BOOL L821_;
 INT L831_;
 INT L84;
 INT L851_;
 extern STR typein400820632;
 BOOL L86;
 BOOL L871_;
 FSTR L88;
 dSTR L89;
 dTP L90;
 BOOL L91;
 BOOL L921_;
 FSTR L93;
 dSTR L94;
 extern STR name55;
 BOOL L95;
 BOOL L961_;
 FSTR L97;
 dSTR L98;
 extern STR staticint;
 BOOL L99;
 BOOL L1001_;
 FSTR L102;
 dSTR L103;
 BOOL L104;
 BOOL L1051_;
 FSTR L106;
 dSTR L107;
 extern STR S_ref_offsets;
 BOOL L108;
 BOOL L1091_;
 FSTR L110;
 dSTR L111;
 dTP L112;
 extern STR LOCK_H1013548551;
 BOOL L113;
 BOOL L1141_;
 FSTR L115;
 dSTR L116;
 BOOL L117;
 BOOL L118;
 dTP L119;
 BOOL L120;
 BOOL L1211_;
 dTP L122;
 BOOL L123;
 BOOL L1241_;
 dTP L125;
 BOOL L126;
 BOOL L1271_;
 dTP L128;
 extern STR int2;
 extern STR S07;
 extern STR int2;
 extern STR S08;
 extern STR S13;
 BOOL L129;
 BOOL L1301_;
 FSTR L132;
 dSTR L133;
 extern STR struct2095330783;
 BOOL L134;
 BOOL L1351_;
 FSTR L136;
 dSTR L137;
 extern STR unsign1349438557;
 BOOL L138;
 BOOL L1391_;
 FSTR L140;
 dSTR L142;
 BOOL L143;
 dTP L144;
 BOOL L145;
 BOOL L1461_;
 dTP L147;
 IMPL L148;
 BOOL L149;
 BOOL L1501_;
 INT L1521_;
 BOOL L1531_;
 extern STR struct682057613;
 BOOL L154;
 BOOL L1551_;
 FSTR L156;
 dSTR L157;
 dSTR L158;
 OB L159;
 BOOL L160;
 BOOL L1621_;
 FSTR L163;
 dSTR L164;
 extern STR name86;
 BOOL L165;
 BOOL L1661_;
 FSTR L167;
 dSTR L168;
 extern STR name140;
 BOOL L169;
 BOOL L1701_;
 FSTR L172;
 dSTR L173;
 BOOL L174;
 BOOL L1751_;
 FSTR L176;
 dSTR L177;
 extern STR S_type_table2;
 BOOL L178;
 BOOL L1791_;
 FSTR L180;
 dSTR L182;
 extern STR name60;
 BOOL L183;
 BOOL L1841_;
 FSTR L185;
 dSTR L186;
 BOOL L187;
 BOOL L1881_;
 FSTR L189;
 dSTR L190;
 extern STR S_ref_offsets1;
 BOOL L192;
 BOOL L1931_;
 FSTR L194;
 dSTR L195;
 extern STR name141;
 BOOL L196;
 BOOL L1971_;
 FSTR L198;
 dSTR L199;
 dTP L200;
 BOOL L202;
 BOOL L2031_;
 FSTR L204;
 dSTR L205;
 extern STR name176;
 BOOL L206;
 BOOL L2071_;
 FSTR L208;
 dSTR L209;
 dTP L210;
 BOOL L211;
 BOOL L2121_;
 BOOL L213;
 BOOL L2141_;
 FSTR L215;
 dSTR L216;
 extern STR name177;
 BOOL L217;
 BOOL L2181_;
 FSTR L219;
 dSTR L220;
 dPARSE L222;
 IDENT L223;
 ARRAYdTP L224;
 INT L2251_;
 dPARSE L226;
 AS_CLASS_DEF L227;
 dSTR L228;
 OB L229;
 BOOL L230;
 BOOL L2311_;
 FSTR L232;
 dSTR L233;
 extern STR name12;
 BOOL L234;
 BOOL L2351_;
 FSTR L236;
 dSTR L237;
 extern STR S09;
 BOOL L238;
 BOOL L2391_;
 FSTR L240;
 dSTR L242;
 dTP L243;
 extern STR sizeof2;
 BOOL L244;
 BOOL L2451_;
 FSTR L246;
 dSTR L247;
 BOOL L248;
 BOOL L2491_;
 FSTR L250;
 dSTR L252;
 extern STR int3;
 BOOL L253;
 BOOL L2541_;
 FSTR L255;
 dSTR L256;
 BOOL L257;
 BOOL L2581_;
 FSTR L259;
 dSTR L260;
 extern STR S_boxe758056869;
 BOOL L261;
 BOOL L2621_;
 FSTR L263;
 dSTR L264;
 dTP L265;
 STR L266;
 extern STR EXT_OB1;
 extern STR S00siz916295349;
 FSTR L267;
 dSTR L268;
 dTP L269;
 extern STR S00ext1705215522;
 BOOL L270;
 BOOL L2721_;
 FSTR L273;
 dSTR L274;
 BOOL L275;
 dTP L276;
 BOOL L277;
 BOOL L2781_;
 dTP L279;
 IMPL L280;
 BOOL L281;
 BOOL L2821_;
 extern STR int4;
 BOOL L283;
 BOOL L2841_;
 FSTR L285;
 dSTR L286;
 BOOL L287;
 BOOL L2881_;
 FSTR L289;
 dSTR L290;
 extern STR S0arr_1958137105;
 BOOL L292;
 BOOL L2931_;
 FSTR L294;
 dSTR L295;
 extern STR sizeof3;
 BOOL L296;
 BOOL L2971_;
 FSTR L298;
 dSTR L299;
 BOOL L300;
 BOOL L3021_;
 FSTR L303;
 dSTR L304;
 extern STR S00;
 BOOL L305;
 BOOL L3061_;
 FSTR L307;
 dSTR L308;
 BOOL L309;
 dTP L310;
 BOOL L311;
 BOOL L3121_;
 dTP L313;
 IMPL L314;
 BOOL L315;
 BOOL L3161_;
 extern STR name60;
 BOOL L317;
 BOOL L3181_;
 FSTR L319;
 dSTR L320;
 INT L3221_;
 dSTR L323;
 OB L324;
 FSTR L325;
 dSTR L326;
 extern STR name60;
 BOOL L327;
 BOOL L3281_;
 FSTR L329;
 dSTR L330;
 dSTR L332;
 OB L333;
 FSTR L334;
 dSTR L335;
 dTP L336;
 extern STR S14;
 BOOL L337;
 BOOL L3381_;
 FSTR L339;
 dSTR L340;
 extern STR S010;
 BOOL L342;
 BOOL L3431_;
 FSTR L344;
 dSTR L345;
 BOOL L346;
 BOOL L347;
 BOOL L348;
 dTP L349;
 BOOL L350;
 BOOL L3521_;
 dTP L353;
 BOOL L354;
 BOOL L3551_;
 dTP L356;
 BOOL L357;
 BOOL L3581_;
 dTP L359;
 BOOL L360;
 BOOL L3621_;
 extern STR S15;
 BOOL L363;
 BOOL L3641_;
 FSTR L365;
 dSTR L366;
 extern STR S05;
 BOOL L367;
 BOOL L3681_;
 FSTR L369;
 dSTR L370;
 dTP L371;
 extern STR S15;
 BOOL L372;
 BOOL L3731_;
 FSTR L374;
 dSTR L375;
 extern STR S05;
 BOOL L376;
 BOOL L3771_;
 FSTR L378;
 dSTR L379;
 dTP L380;
 extern STR S15;
 BOOL L382;
 BOOL L3831_;
 FSTR L384;
 dSTR L385;
 extern STR S05;
 BOOL L386;
 BOOL L3871_;
 FSTR L388;
 dSTR L389;
 dTP L390;
 extern STR S15;
 BOOL L391;
 BOOL L3921_;
 FSTR L393;
 dSTR L394;
 extern STR S05;
 BOOL L395;
 BOOL L3961_;
 FSTR L397;
 dSTR L398;
 BOOL L399;
 dTP L400;
 BOOL L401;
 BOOL L4021_;
 dTP L403;
 IMPL L404;
 BOOL L405;
 BOOL L4061_;
 extern STR S15;
 BOOL L407;
 BOOL L4081_;
 FSTR L409;
 dSTR L410;
 extern STR S05;
 BOOL L411;
 BOOL L4121_;
 FSTR L413;
 dSTR L414;
 BOOL L415;
 BOOL L416;
 dTP L417;
 BOOL L418;
 BOOL L4191_;
 dTP L420;
 BOOL L422;
 BOOL L4231_;
 BOOL L4241_;
 extern STR name178;
 FSTR L425;
 dSTR L426;
 INT L4271_br;
 ATTRSORT aL4271_;
 ATTRSORT L428;
 ATTRSORT L429;
 extern STR name12;
 FSTR L430;
 dSTR L431;
 extern STR name179;
 BOOL L432;
 BOOL L4331_;
 FSTR L434;
 dSTR L435;
 IDENT L436;
 BOOL L437;
 BOOL L4381_;
 FSTR L439;
 dSTR L440;
 extern STR name142;
 BOOL L442;
 BOOL L4431_;
 FSTR L444;
 dSTR L445;
 OB L446;
 OB L447;
 BOOL L448;
 BOOL L4491_;
 FSTR L450;
 dSTR L452;
 extern STR name146;
 BOOL L453;
 BOOL L4541_;
 FSTR L455;
 dSTR L456;
 extern STR S_tag;
 BOOL L457;
 BOOL L4581_;
 FSTR L459;
 dSTR L460;
 extern STR int5;
 BOOL L462;
 BOOL L4631_;
 FSTR L464;
 dSTR L465;
 FSTR L466;
 dSTR L467;
 dTP L468;
 extern STR name58;
 BOOL L469;
 BOOL L4701_;
 FSTR L471;
 dSTR L472;
 extern STR S08;
 BOOL L473;
 BOOL L4741_;
 FSTR L475;
 dSTR L476;
 OB L477;
 OB L478;
 BOOL L479;
 BOOL L4801_;
 FSTR L482;
 dSTR L483;
 extern STR name3;
 BOOL L484;
 BOOL L4851_;
 FSTR L486;
 dSTR L487;
 dAS_CLASS_ELT L488;
 dSTR L489;
 OB L490;
 BOOL L492;
 BOOL L4931_;
 FSTR L494;
 dSTR L495;
 extern STR name139;
 BOOL L496;
 BOOL L4971_;
 FSTR L498;
 dSTR L499;
 INT L5001_;
 dTP L501;
 IMPL L502;
 BOOL L503;
 BOOL L5041_;
 extern STR name12;
 FSTR L505;
 dSTR L506;
 extern STR asizea1439666166;
 BOOL L507;
 BOOL L5081_;
 FSTR L509;
 dSTR L510;
 dTP L511;
 BOOL L512;
 BOOL L5131_;
 FSTR L514;
 dSTR L515;
 extern STR S_ASIZE;
 BOOL L516;
 BOOL L5171_;
 FSTR L518;
 dSTR L519;
 extern STR int6;
 BOOL L520;
 BOOL L5221_;
 FSTR L523;
 dSTR L524;
 BOOL L525;
 BOOL L5261_;
 FSTR L527;
 dSTR L528;
 extern STR S0asize;
 BOOL L529;
 BOOL L5301_;
 FSTR L531;
 dSTR L532;
 extern STR S011;
 BOOL L533;
 BOOL L5341_;
 FSTR L535;
 dSTR L536;
 dTP L537;
 IMPL L538;
 ARRAYdTP L539;
 dTP L5401_;
 extern STR arr_part5;
 BOOL L541;
 BOOL L5421_;
 FSTR L543;
 dSTR L544;
 extern STR S_tag;
 BOOL L545;
 BOOL L5461_;
 FSTR L547;
 dSTR L548;
 extern STR int7;
 BOOL L549;
 BOOL L5501_;
 FSTR L551;
 dSTR L552;
 BOOL L553;
 BOOL L5541_;
 FSTR L555;
 dSTR L556;
 dTP L557;
 extern STR name58;
 BOOL L558;
 BOOL L5591_;
 FSTR L560;
 dSTR L562;
 extern STR S0arr_part0;
 BOOL L563;
 BOOL L5641_;
 FSTR L565;
 dSTR L566;
 extern STR name147;
 BOOL L567;
 BOOL L5681_;
 FSTR L569;
 dSTR L570;
 extern STR name148;
 BOOL L571;
 BOOL L5721_;
 FSTR L573;
 dSTR L574;
 extern STR define2029181961;
 extern STR name9;
 dec = FSTR_c1998740379(((FSTR) NULL), 1000);
 PRINT_OB_mangler = ATTR(cgen,mangler);
 file_count = 0;
 plus_self = ((STR) &Print);
 str_self = file_count;
 clear_self = buf1;
 L9 = (clear_self==((FSTR) NULL));
 L101_=!(L9); 
 if (L101_) {
  SATTR(clear_self,loc,0);
 }
 str_in_self = str_self;
 str_in_s = buf1;
 ret_val2 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
 buf1 = ret_val2;
 str_self1 = buf1;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self1);
 ret_val1 = ret_val3;
 plus_sarg = ret_val1;
 ret_val4 = STR_ap2004550586(plus_self, plus_sarg);
 plus_self1 = ret_val4;
 plus_sarg1 = ((STR) &c1);
 ret_val5 = STR_ap2004550586(plus_self1, plus_sarg1);
 p = CODE_F1432464034(((CODE_FILE) NULL), ret_val5);
 SATTR(p,is_c_code,TRUE);
 clt = ATTR(ATTR(prog,tp_tbl),class_tbl);
 smallest_tag = 0;
 largest_tag = 0;
 {
  struct FMAPdT1072907830_frame_struct other1_0;
FMAPdT1072907830_frame noname1 = &other1_0;
  L11 = ATTR(cgen,tags);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L12 = FMAPdT1072907830(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   n = L12;
   L131_=(n)<(smallest_tag); 
   if (L131_) {
    smallest_tag = n;
   }
   L141_=(largest_tag)<(n); 
   if (L141_) {
    largest_tag = n;
   }
  }
 }
 after_loop: ;
 last_largest = 0;
 last_smallest = 0;
 tg = 0;
 while (1) {
  L151_=(0)<(tg); 
  if (L151_) {
   L161_=(tg)<(largest_tag); 
   if (L161_) {
    L171_=INTPLUS(tg,1); 
    tg = L171_;
   }
   else {
    tg = 0;
   }
  }
  else {
   L181_=(tg)<(0); 
   if (L181_) {
    L191_=(smallest_tag)<(tg); 
    if (L191_) {
     L201_=INTMINUS(tg,1); 
     tg = L201_;
    }
    else {
     tg = 0;
    }
   }
  }
  L221_=(tg)==(0); 
  if (L221_) {
   L241_=(last_smallest)==(smallest_tag); 
   if (L241_) {
    L251_=(last_largest)==(largest_tag); 
    L23 = L251_;
   } else {
    L23 = FALSE;
   }
   if (L23) {
    L26 = CGEN_a952489530(cgen);
    L271_=!(L26); 
    if (L271_) {
     goto after_loop1;
    }
    last_smallest = smallest_tag;
    last_largest = largest_tag;
    {
     struct FMAPdT1072907830_frame_struct other3_0;
FMAPdT1072907830_frame noname2 = &other3_0;
     L21 = ATTR(cgen,tags);
     noname2->self = L21;
     noname2->state = 0;
     while (1) {
      L28 = FMAPdT1072907830(noname2);
      if (noname2->state == -1) {
       goto after_loop2;
      }
      n11 = L28;
      L291_=(n11)<(smallest_tag); 
      if (L291_) {
       smallest_tag = n11;
      }
      L301_=(largest_tag)<(n11); 
      if (L301_) {
       largest_tag = n11;
      }
     }
    }
    after_loop2: ;
   }
   L321_=(last_largest)<(largest_tag); 
   if (L321_) {
    L331_=INTPLUS(last_largest,1); 
    tg = L331_;
    last_largest = largest_tag;
   }
   else {
    L341_=(smallest_tag)<(last_smallest); 
    if (L341_) {
     L351_=INTMINUS(last_smallest,1); 
     tg = L351_;
     last_smallest = smallest_tag;
    }
    else {
     goto after_loop1;
    }
   }
  }
  cl = ((dTP) NULL);
  L361_=(tg)==(0); 
  L37 = L361_;
  L381_=!(L37); 
  if (L381_) {
   {
    struct FMAPdT1857608820_frame_struct other4_0;
FMAPdT1857608820_frame noname3 = &other4_0;
    L31 = ATTR(cgen,tags);
    noname3->self = L31;
    noname3->state = 0;
    while (1) {
     L39 = FMAPdT1857608820(noname3);
     if (noname3->state == -1) {
      goto after_loop3;
     }
     pr = L39;
     L40 = pr.t2;
     L421_=(L40)==(tg); 
     if (L421_) {
      cl = pr.t1;
      goto after_loop3;
     }
    }
   }
   after_loop3: ;
   if ((cl==((dTP) NULL))) {
    barf_self = prog;
    plus_self2 = ((STR) &theres458619408);
    plus_arg = tg;
    str_self2 = plus_arg;
    clear_self1 = buf2;
    L43 = (clear_self1==((FSTR) NULL));
    L441_=!(L43); 
    if (L441_) {
     SATTR(clear_self1,loc,0);
    }
    str_in_self1 = str_self2;
    str_in_s1 = buf2;
    ret_val8 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
    buf2 = ret_val8;
    str_self3 = buf2;
    ret_val9 = STR_fr1097270334(((STR) NULL), str_self3);
    ret_val7 = ret_val9;
    s = ret_val7;
    ret_val6 = STR_ap1077157958(plus_self2, s, TRUE);
    plus_self3 = ret_val6;
    plus_sarg2 = ((STR) &name9);
    ret_val10 = STR_ap2004550586(plus_self3, plus_sarg2);
    barf_msg = ret_val10;
    barf_at_self = barf_self;
    barf_at_msg = barf_msg;
    barf_at_at = ((dPROG_ERR) NULL);
    PROG_e176405615(barf_at_self, barf_at_at);
    plus_self4 = ((STR) &Intern28965746);
    plus_sarg3 = barf_at_msg;
    ret_val11 = STR_ap2004550586(plus_self4, plus_sarg3);
    PROG_err_STR(barf_at_self, ret_val11);
    exit_self = ((UNIX) NULL);
    exit_code = 1;
    exit(exit_code);
   }
   L451_=(fc)==(30); 
   if (L451_) {
    L461_=INTPLUS(file_count,1); 
    file_count = L461_;
    plus_self5 = ((STR) &Print);
    str_self4 = file_count;
    clear_self2 = buf3;
    L47 = (clear_self2==((FSTR) NULL));
    L481_=!(L47); 
    if (L481_) {
     SATTR(clear_self2,loc,0);
    }
    str_in_self2 = str_self4;
    str_in_s2 = buf3;
    ret_val13 = INT_st367594495(str_in_self2, str_in_s2, 0, 10, ' ');
    buf3 = ret_val13;
    str_self5 = buf3;
    ret_val14 = STR_fr1097270334(((STR) NULL), str_self5);
    ret_val12 = ret_val14;
    plus_sarg4 = ret_val12;
    ret_val15 = STR_ap2004550586(plus_self5, plus_sarg4);
    plus_self6 = ret_val15;
    plus_sarg5 = ((STR) &c1);
    ret_val16 = STR_ap2004550586(plus_self6, plus_sarg5);
    p = CODE_F1432464034(((CODE_FILE) NULL), ret_val16);
    SATTR(p,is_c_code,TRUE);
    fc = 0;
   }
   L491_=INTPLUS(fc,1); 
   fc = L491_;
   L50 = (cl==((dTP) NULL));
   L521_=!(L50); 
   if (L521_) {
    uses_tp_self = p;
    uses_tp_t = cl;
    CODE_F525364269(uses_tp_self, LAYOUT79932797(((LAYOUT_TBL) NULL), uses_tp_t));
    mkfname_self = self;
    mkfname_cl = cl;
    mang_self = mkfname_self;
    mang_ob = ((OB) mkfname_cl);
    ret_val18 = MANGLE119219986(PRINT_OB_mangler, mang_ob, ((OB) NULL));
    fstr_self = ret_val18;
    ret_val19 = FSTR_c1307336863(((FSTR) NULL), fstr_self);
    ret_val17 = ret_val19;
    fname = ret_val17;
    attrs = 0;
    L54 = cl;
    L55 = (*dTP_is1811059018[TAG(L54)])(L54);
    L561_=!(L55); 
    if (L561_) {
     L57 = cl;
     L58 = ((*dTP_implrIMPL[TAG(L57)])(L57)==((IMPL) NULL));
     L591_=!(L58); 
     L53 = L591_;
    } else {
     L53 = FALSE;
    }
    if (L53) {
     create_self = ((ARRAYATTRSORT) NULL);
     L60 = cl;
     L62=(*dTP_implrIMPL[TAG(L60)])(L60);
     create_n = ELT_TBL_sizerINT(ATTR(L62,elts));
     L64 = create_n;
     L66=(sizeof(struct ARRAYATTRSORT_struct)-sizeof(ATTRSORT))+(L64)*sizeof(ATTRSORT);
     L65=ZALLOC_BIG(L66);
     if (L65==NULL) FATAL("Unable to allocate more memory");
     ((OB)L65)->header.tag=ARRAYATTRSORT_tag;
#ifdef DESTROY_CHK

       ((OB)L65)->header.destroyed=0;
#endif

     L63 = ((ARRAYATTRSORT) L65);
     L63->asize = L64;
     ret_val20 = L63;
     as = ret_val20;
     {
      struct ELT_TBL_eltbrELT_frame_struct other5_0;
ELT_TBL_eltbrELT_frame noname4 = &other5_0;
      struct ELT_TBL_eltbrELT_frame_struct other5_1;
ELT_TBL_eltbrELT_frame noname5 = &other5_1;
      BOOL f_L671_ = TRUE;
      L61 = as;
      noname4->state = 0;
      noname5->state = 0;
      L671_br=L61==NULL?0:ASIZE((ARRAYATTRSORT)L61); 
      i_L671_=0;
      while (1) {
       if (noname4->state == 0) {
        L69 = cl;
        L70=(*dTP_implrIMPL[TAG(L69)])(L69);
        L41 = ATTR(L70,elts);
        noname4->self = L41;
       }
       L68 = ELT_TBL_eltbrELT(noname4);
       if (noname4->state == -1) {
        goto after_loop4;
       }
       name_self = L68;
       ret_val21 = ATTR(ATTR(name_self,sig1),name1);
       L72=ret_val21;
       n2 = L72.str;
       if (noname5->state == 0) {
        L74 = cl;
        L75=(*dTP_implrIMPL[TAG(L74)])(L74);
        L51 = ATTR(L75,elts);
        noname5->self = L51;
       }
       L73 = ELT_TBL_eltbrELT(noname5);
       if (noname5->state == -1) {
        goto after_loop4;
       }
       c = L73;
       if (ELT_is380037919(c)) {
        if(i_L671_>=L671_br)  goto after_loop4; 
        create_self1 = ((ATTRSORT) NULL);
        create_n1 = n2;
        create_e = c;
        L77=ZALLOC(sizeof(struct ATTRSORT_struct));
        if (L77==NULL) FATAL("Unable to allocate more memory");
        ((OB)L77)->header.tag=ATTRSORT_tag;
        L76 = ((ATTRSORT) L77);
        r = L76;
        SATTR(r,name1,create_n1);
        SATTR(r,elt1,create_e);
        ret_val22 = r;
        L78 = ret_val22;
        SARR((ARRAYATTRSORT)L61,i_L671_,(ATTRSORT)L78); i_L671_++;
        ;
        L791_=INTPLUS(attrs,1); 
        attrs = L791_;
       }
      }
     }
     after_loop4: ;
     as = ARRAYA1114043836(as, attrs);
     sort_self = as;
     L80 = (sort_self==((ARRAYATTRSORT) NULL));
     L821_=!(L80); 
     if (L821_) {
      L831_=ASIZE((ARRAYATTRSORT)sort_self); 
      L84 = L831_;
      L851_=INTMINUS(L84,1); 
      ARRAYA487581188(sort_self, 0, L851_);
     }
    }
    plus_self7 = p;
    plus_s = ((dSTR) ((STR) &typein400820632));
    L86 = (plus_s==((dSTR) NULL));
    L871_=!(L86); 
    if (L871_) {
     L88 = ATTR(plus_self7,ntext);
     L89 = plus_s;
     SATTR(plus_self7,ntext,FSTR_p1752847026(L88, (*dSTR_strrSTR[TAG(L89)])(L89)));
    }
    ret_val23 = plus_self7;
    plus_self8 = ret_val23;
    L90 = cl;
    plus_s1 = ((dSTR) (*dTP_strrSTR[TAG(L90)])(L90));
    L91 = (plus_s1==((dSTR) NULL));
    L921_=!(L91); 
    if (L921_) {
     L93 = ATTR(plus_self8,ntext);
     L94 = plus_s1;
     SATTR(plus_self8,ntext,FSTR_p1752847026(L93, (*dSTR_strrSTR[TAG(L94)])(L94)));
    }
    ret_val24 = plus_self8;
    plus_self9 = ret_val24;
    plus_s2 = ((dSTR) ((STR) &name55));
    L95 = (plus_s2==((dSTR) NULL));
    L961_=!(L95); 
    if (L961_) {
     L97 = ATTR(plus_self9,ntext);
     L98 = plus_s2;
     SATTR(plus_self9,ntext,FSTR_p1752847026(L97, (*dSTR_strrSTR[TAG(L98)])(L98)));
    }
    ret_val25 = plus_self9;
    p = ret_val25;
    plus_self10 = p;
    plus_s3 = ((dSTR) ((STR) &staticint));
    L99 = (plus_s3==((dSTR) NULL));
    L1001_=!(L99); 
    if (L1001_) {
     L102 = ATTR(plus_self10,ntext);
     L103 = plus_s3;
     SATTR(plus_self10,ntext,FSTR_p1752847026(L102, (*dSTR_strrSTR[TAG(L103)])(L103)));
    }
    ret_val26 = plus_self10;
    plus_self11 = ret_val26;
    plus_s4 = ((dSTR) fname);
    L104 = (plus_s4==((dSTR) NULL));
    L1051_=!(L104); 
    if (L1051_) {
     L106 = ATTR(plus_self11,ntext);
     L107 = plus_s4;
     SATTR(plus_self11,ntext,FSTR_p1752847026(L106, (*dSTR_strrSTR[TAG(L107)])(L107)));
    }
    ret_val27 = plus_self11;
    plus_self12 = ret_val27;
    plus_s5 = ((dSTR) ((STR) &S_ref_offsets));
    L108 = (plus_s5==((dSTR) NULL));
    L1091_=!(L108); 
    if (L1091_) {
     L110 = ATTR(plus_self12,ntext);
     L111 = plus_s5;
     SATTR(plus_self12,ntext,FSTR_p1752847026(L110, (*dSTR_strrSTR[TAG(L111)])(L111)));
    }
    ret_val28 = plus_self12;
    p = ret_val28;
    L112 = cl;
    if ((*dTP_is1999456142[TAG(L112)])(L112, ((dTP) TP_BUI1813155122))) {
     plus_self13 = p;
     plus_s6 = ((dSTR) ((STR) &LOCK_H1013548551));
     L113 = (plus_s6==((dSTR) NULL));
     L1141_=!(L113); 
     if (L1141_) {
      L115 = ATTR(plus_self13,ntext);
      L116 = plus_s6;
      SATTR(plus_self13,ntext,FSTR_p1752847026(L115, (*dSTR_strrSTR[TAG(L116)])(L116)));
     }
     ret_val29 = plus_self13;
     p = ret_val29;
    }
    L119 = cl;
    L120 = (*dTP_is1811059018[TAG(L119)])(L119);
    L1211_=!(L120); 
    if (L1211_) {
     L122 = cl;
     L123 = (*dTP_is735947028[TAG(L122)])(L122);
     L1241_=!(L123); 
     L118 = L1241_;
    } else {
     L118 = FALSE;
    }
    if (L118) {
     L125 = cl;
     L126 = (*dTP_is124163553[TAG(L125)])(L125);
     L1271_=!(L126); 
     L117 = L1271_;
    } else {
     L117 = FALSE;
    }
    if (L117) {
     L128 = cl;
     if ((*dTP_is1334578382[TAG(L128)])(L128)) {
      plus_self14 = ((STR) &int2);
      mang_self1 = self;
      mang_ob1 = ((OB) cl);
      ret_val30 = MANGLE119219986(PRINT_OB_mangler, mang_ob1, ((OB) NULL));
      plus_sarg6 = ret_val30;
      ret_val31 = STR_ap2004550586(plus_self14, plus_sarg6);
      plus_self15 = ret_val31;
      plus_sarg7 = ((STR) &S07);
      ret_val32 = STR_ap2004550586(plus_self15, plus_sarg7);
      tps = ret_val32;
     }
     else {
      plus_self16 = ((STR) &int2);
      mang_self2 = self;
      mang_ob2 = ((OB) cl);
      ret_val33 = MANGLE119219986(PRINT_OB_mangler, mang_ob2, ((OB) NULL));
      plus_sarg8 = ret_val33;
      ret_val34 = STR_ap2004550586(plus_self16, plus_sarg8);
      plus_self17 = ret_val34;
      plus_sarg9 = ((STR) &S08);
      ret_val35 = STR_ap2004550586(plus_self17, plus_sarg9);
      tps = ret_val35;
     }
     PRINT_116660897(self, p, cl, cl, tps);
    }
    plus_self18 = p;
    plus_s7 = ((dSTR) ((STR) &S13));
    L129 = (plus_s7==((dSTR) NULL));
    L1301_=!(L129); 
    if (L1301_) {
     L132 = ATTR(plus_self18,ntext);
     L133 = plus_s7;
     SATTR(plus_self18,ntext,FSTR_p1752847026(L132, (*dSTR_strrSTR[TAG(L133)])(L133)));
    }
    ret_val36 = plus_self18;
    p = ret_val36;
    plus_self19 = p;
    plus_s8 = ((dSTR) ((STR) &struct2095330783));
    L134 = (plus_s8==((dSTR) NULL));
    L1351_=!(L134); 
    if (L1351_) {
     L136 = ATTR(plus_self19,ntext);
     L137 = plus_s8;
     SATTR(plus_self19,ntext,FSTR_p1752847026(L136, (*dSTR_strrSTR[TAG(L137)])(L137)));
    }
    ret_val37 = plus_self19;
    plus_self20 = ret_val37;
    plus_s9 = ((dSTR) ((STR) &unsign1349438557));
    L138 = (plus_s9==((dSTR) NULL));
    L1391_=!(L138); 
    if (L1391_) {
     L140 = ATTR(plus_self20,ntext);
     L142 = plus_s9;
     SATTR(plus_self20,ntext,FSTR_p1752847026(L140, (*dSTR_strrSTR[TAG(L142)])(L142)));
    }
    ret_val38 = plus_self20;
    p = ret_val38;
    L144 = cl;
    L145 = ((*dTP_implrIMPL[TAG(L144)])(L144)==((IMPL) NULL));
    L1461_=!(L145); 
    if (L1461_) {
     L147 = cl;
     L148=(*dTP_implrIMPL[TAG(L147)])(L147);
     L149 = (ATTR(L148,arr)==((TP_CLASS) NULL));
     L1501_=!(L149); 
     L143 = L1501_;
    } else {
     L143 = FALSE;
    }
    if (L143) {
     L1521_=INTPLUS(attrs,2); 
     attrs = L1521_;
    }
    L1531_=(0)<(attrs); 
    if (L1531_) {
     plus_self21 = p;
     plus_s10 = ((dSTR) ((STR) &struct682057613));
     L154 = (plus_s10==((dSTR) NULL));
     L1551_=!(L154); 
     if (L1551_) {
      L156 = ATTR(plus_self21,ntext);
      L157 = plus_s10;
      SATTR(plus_self21,ntext,FSTR_p1752847026(L156, (*dSTR_strrSTR[TAG(L157)])(L157)));
     }
     ret_val39 = plus_self21;
     plus_self22 = ret_val39;
     L159=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
     if (L159==NULL) FATAL("Unable to allocate more memory");
     memset(L159,0,sizeof(struct INT_boxed_struct));
     ((OB)L159)->header.tag=INT_tag;
     L158 = (dSTR)((INT_boxed) L159);
     ((INT_boxed) L158)->immutable_part = attrs;
     plus_s11 = L158;
     L160 = (plus_s11==((dSTR) NULL));
     L1621_=!(L160); 
     if (L1621_) {
      L163 = ATTR(plus_self22,ntext);
      L164 = plus_s11;
      SATTR(plus_self22,ntext,FSTR_p1752847026(L163, (*dSTR_strrSTR[TAG(L164)])(L164)));
     }
     ret_val40 = plus_self22;
     plus_self23 = ret_val40;
     plus_s12 = ((dSTR) ((STR) &name86));
     L165 = (plus_s12==((dSTR) NULL));
     L1661_=!(L165); 
     if (L1661_) {
      L167 = ATTR(plus_self23,ntext);
      L168 = plus_s12;
      SATTR(plus_self23,ntext,FSTR_p1752847026(L167, (*dSTR_strrSTR[TAG(L168)])(L168)));
     }
     ret_val41 = plus_self23;
     p = ret_val41;
    }
    plus_self24 = p;
    plus_s13 = ((dSTR) ((STR) &name140));
    L169 = (plus_s13==((dSTR) NULL));
    L1701_=!(L169); 
    if (L1701_) {
     L172 = ATTR(plus_self24,ntext);
     L173 = plus_s13;
     SATTR(plus_self24,ntext,FSTR_p1752847026(L172, (*dSTR_strrSTR[TAG(L173)])(L173)));
    }
    ret_val42 = plus_self24;
    plus_self25 = ret_val42;
    plus_s14 = ((dSTR) fname);
    L174 = (plus_s14==((dSTR) NULL));
    L1751_=!(L174); 
    if (L1751_) {
     L176 = ATTR(plus_self25,ntext);
     L177 = plus_s14;
     SATTR(plus_self25,ntext,FSTR_p1752847026(L176, (*dSTR_strrSTR[TAG(L177)])(L177)));
    }
    ret_val43 = plus_self25;
    plus_self26 = ret_val43;
    plus_s15 = ((dSTR) ((STR) &S_type_table2));
    L178 = (plus_s15==((dSTR) NULL));
    L1791_=!(L178); 
    if (L1791_) {
     L180 = ATTR(plus_self26,ntext);
     L182 = plus_s15;
     SATTR(plus_self26,ntext,FSTR_p1752847026(L180, (*dSTR_strrSTR[TAG(L182)])(L182)));
    }
    ret_val44 = plus_self26;
    p = ret_val44;
    plus_self27 = p;
    plus_s16 = ((dSTR) ((STR) &name60));
    L183 = (plus_s16==((dSTR) NULL));
    L1841_=!(L183); 
    if (L1841_) {
     L185 = ATTR(plus_self27,ntext);
     L186 = plus_s16;
     SATTR(plus_self27,ntext,FSTR_p1752847026(L185, (*dSTR_strrSTR[TAG(L186)])(L186)));
    }
    ret_val45 = plus_self27;
    plus_self28 = ret_val45;
    plus_s17 = ((dSTR) fname);
    L187 = (plus_s17==((dSTR) NULL));
    L1881_=!(L187); 
    if (L1881_) {
     L189 = ATTR(plus_self28,ntext);
     L190 = plus_s17;
     SATTR(plus_self28,ntext,FSTR_p1752847026(L189, (*dSTR_strrSTR[TAG(L190)])(L190)));
    }
    ret_val46 = plus_self28;
    plus_self29 = ret_val46;
    plus_s18 = ((dSTR) ((STR) &S_ref_offsets1));
    L192 = (plus_s18==((dSTR) NULL));
    L1931_=!(L192); 
    if (L1931_) {
     L194 = ATTR(plus_self29,ntext);
     L195 = plus_s18;
     SATTR(plus_self29,ntext,FSTR_p1752847026(L194, (*dSTR_strrSTR[TAG(L195)])(L195)));
    }
    ret_val47 = plus_self29;
    plus_self30 = ret_val47;
    plus_s19 = ((dSTR) ((STR) &name141));
    L196 = (plus_s19==((dSTR) NULL));
    L1971_=!(L196); 
    if (L1971_) {
     L198 = ATTR(plus_self30,ntext);
     L199 = plus_s19;
     SATTR(plus_self30,ntext,FSTR_p1752847026(L198, (*dSTR_strrSTR[TAG(L199)])(L199)));
    }
    ret_val48 = plus_self30;
    plus_self31 = ret_val48;
    L200 = cl;
    plus_s20 = ((dSTR) (*dTP_strrSTR[TAG(L200)])(L200));
    L202 = (plus_s20==((dSTR) NULL));
    L2031_=!(L202); 
    if (L2031_) {
     L204 = ATTR(plus_self31,ntext);
     L205 = plus_s20;
     SATTR(plus_self31,ntext,FSTR_p1752847026(L204, (*dSTR_strrSTR[TAG(L205)])(L205)));
    }
    ret_val49 = plus_self31;
    plus_self32 = ret_val49;
    plus_s21 = ((dSTR) ((STR) &name176));
    L206 = (plus_s21==((dSTR) NULL));
    L2071_=!(L206); 
    if (L2071_) {
     L208 = ATTR(plus_self32,ntext);
     L209 = plus_s21;
     SATTR(plus_self32,ntext,FSTR_p1752847026(L208, (*dSTR_strrSTR[TAG(L209)])(L209)));
    }
    ret_val50 = plus_self32;
    p = ret_val50;
    L210 = cl;
    L211 = (*dTP_is124163553[TAG(L210)])(L210);
    L2121_=!(L211); 
    if (L2121_) {
     plus_self33 = p;
     mang_self3 = self;
     mang_ob3 = ((OB) cl);
     ret_val51 = MANGLE119219986(PRINT_OB_mangler, mang_ob3, ((OB) NULL));
     plus_s22 = ((dSTR) ret_val51);
     L213 = (plus_s22==((dSTR) NULL));
     L2141_=!(L213); 
     if (L2141_) {
      L215 = ATTR(plus_self33,ntext);
      L216 = plus_s22;
      SATTR(plus_self33,ntext,FSTR_p1752847026(L215, (*dSTR_strrSTR[TAG(L216)])(L216)));
     }
     ret_val52 = plus_self33;
     p = ret_val52;
    }
    if (cl==NULL) {
     goto other467;
    } else
    switch (TAG(cl)) {
     case TP_CLASS_tag:
      plus_self34 = p;
      plus_s23 = ((dSTR) ((STR) &name177));
      L217 = (plus_s23==((dSTR) NULL));
      L2181_=!(L217); 
      if (L2181_) {
       L219 = ATTR(plus_self34,ntext);
       L220 = plus_s23;
       SATTR(plus_self34,ntext,FSTR_p1752847026(L219, (*dSTR_strrSTR[TAG(L220)])(L220)));
      }
      ret_val53 = plus_self34;
      plus_self35 = ret_val53;
      L222 = ATTR(prog,parse);
      L223 = ATTR(((TP_CLASS) cl),name1);
      L224 = ATTR(((TP_CLASS) cl),params);
      L2251_=(L224)==NULL?0:ASIZE((ARRAYdTP)L224); 
      L226 = L222;
      L227=(*dPARSE1581792481[TAG(L226)])(L226, L223, L2251_);
      L229=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
      if (L229==NULL) FATAL("Unable to allocate more memory");
      memset(L229,0,sizeof(struct INT_boxed_struct));
      ((OB)L229)->header.tag=INT_tag;
      L228 = (dSTR)((INT_boxed) L229);
      ((INT_boxed) L228)->immutable_part = PRINT_1524431106(self, cgen, ATTR(L227,source1));
      plus_s24 = L228;
      L230 = (plus_s24==((dSTR) NULL));
      L2311_=!(L230); 
      if (L2311_) {
       L232 = ATTR(plus_self35,ntext);
       L233 = plus_s24;
       SATTR(plus_self35,ntext,FSTR_p1752847026(L232, (*dSTR_strrSTR[TAG(L233)])(L233)));
      }
      ret_val54 = plus_self35;
      plus_self36 = ret_val54;
      plus_s25 = ((dSTR) ((STR) &name12));
      L234 = (plus_s25==((dSTR) NULL));
      L2351_=!(L234); 
      if (L2351_) {
       L236 = ATTR(plus_self36,ntext);
       L237 = plus_s25;
       SATTR(plus_self36,ntext,FSTR_p1752847026(L236, (*dSTR_strrSTR[TAG(L237)])(L237)));
      }
      ret_val55 = plus_self36;
      p = ret_val55; break;
     default: ;
     other467: ;
      plus_self37 = p;
      plus_s26 = ((dSTR) ((STR) &S09));
      L238 = (plus_s26==((dSTR) NULL));
      L2391_=!(L238); 
      if (L2391_) {
       L240 = ATTR(plus_self37,ntext);
       L242 = plus_s26;
       SATTR(plus_self37,ntext,FSTR_p1752847026(L240, (*dSTR_strrSTR[TAG(L242)])(L242)));
      }
      ret_val56 = plus_self37;
      p = ret_val56;
    }
    L243 = cl;
    if ((*dTP_is1334578382[TAG(L243)])(L243)) {
     plus_self38 = p;
     plus_s27 = ((dSTR) ((STR) &sizeof2));
     L244 = (plus_s27==((dSTR) NULL));
     L2451_=!(L244); 
     if (L2451_) {
      L246 = ATTR(plus_self38,ntext);
      L247 = plus_s27;
      SATTR(plus_self38,ntext,FSTR_p1752847026(L246, (*dSTR_strrSTR[TAG(L247)])(L247)));
     }
     ret_val57 = plus_self38;
     plus_self39 = ret_val57;
     mang_self4 = self;
     mang_ob4 = ((OB) cl);
     ret_val58 = MANGLE119219986(PRINT_OB_mangler, mang_ob4, ((OB) NULL));
     plus_s28 = ((dSTR) ret_val58);
     L248 = (plus_s28==((dSTR) NULL));
     L2491_=!(L248); 
     if (L2491_) {
      L250 = ATTR(plus_self39,ntext);
      L252 = plus_s28;
      SATTR(plus_self39,ntext,FSTR_p1752847026(L250, (*dSTR_strrSTR[TAG(L252)])(L252)));
     }
     ret_val59 = plus_self39;
     plus_self40 = ret_val59;
     plus_s29 = ((dSTR) ((STR) &int3));
     L253 = (plus_s29==((dSTR) NULL));
     L2541_=!(L253); 
     if (L2541_) {
      L255 = ATTR(plus_self40,ntext);
      L256 = plus_s29;
      SATTR(plus_self40,ntext,FSTR_p1752847026(L255, (*dSTR_strrSTR[TAG(L256)])(L256)));
     }
     ret_val60 = plus_self40;
     plus_self41 = ret_val60;
     mang_self5 = self;
     mang_ob5 = ((OB) cl);
     ret_val61 = MANGLE119219986(PRINT_OB_mangler, mang_ob5, ((OB) NULL));
     plus_s30 = ((dSTR) ret_val61);
     L257 = (plus_s30==((dSTR) NULL));
     L2581_=!(L257); 
     if (L2581_) {
      L259 = ATTR(plus_self41,ntext);
      L260 = plus_s30;
      SATTR(plus_self41,ntext,FSTR_p1752847026(L259, (*dSTR_strrSTR[TAG(L260)])(L260)));
     }
     ret_val62 = plus_self41;
     plus_self42 = ret_val62;
     plus_s31 = ((dSTR) ((STR) &S_boxe758056869));
     L261 = (plus_s31==((dSTR) NULL));
     L2621_=!(L261); 
     if (L2621_) {
      L263 = ATTR(plus_self42,ntext);
      L264 = plus_s31;
      SATTR(plus_self42,ntext,FSTR_p1752847026(L263, (*dSTR_strrSTR[TAG(L264)])(L264)));
     }
     ret_val63 = plus_self42;
     p = ret_val63;
    }
    else {
     L265 = cl;
     L266 = (*dTP_strrSTR[TAG(L265)])(L265);
     if (STR_is111530248(L266, ((STR) &EXT_OB1))) {
      plus_self43 = p;
      plus_s32 = ((dSTR) ((STR) &S00siz916295349));
      L267 = ATTR(plus_self43,ntext);
      L268 = plus_s32;
      SATTR(plus_self43,ntext,FSTR_p1752847026(L267, (*dSTR_strrSTR[TAG(L268)])(L268)));
     }
     else {
      L269 = cl;
      if ((*dTP_is124163553[TAG(L269)])(L269)) {
       plus_self44 = p;
       plus_s33 = ((dSTR) ((STR) &S00ext1705215522));
       L270 = (plus_s33==((dSTR) NULL));
       L2721_=!(L270); 
       if (L2721_) {
        L273 = ATTR(plus_self44,ntext);
        L274 = plus_s33;
        SATTR(plus_self44,ntext,FSTR_p1752847026(L273, (*dSTR_strrSTR[TAG(L274)])(L274)));
       }
       ret_val64 = plus_self44;
       p = ret_val64;
      }
      else {
       L276 = cl;
       L277 = ((*dTP_implrIMPL[TAG(L276)])(L276)==((IMPL) NULL));
       L2781_=!(L277); 
       if (L2781_) {
        L279 = cl;
        L280=(*dTP_implrIMPL[TAG(L279)])(L279);
        L281 = (ATTR(L280,arr)==((TP_CLASS) NULL));
        L2821_=!(L281); 
        L275 = L2821_;
       } else {
        L275 = FALSE;
       }
       if (L275) {
        plus_self45 = p;
        plus_s34 = ((dSTR) ((STR) &int4));
        L283 = (plus_s34==((dSTR) NULL));
        L2841_=!(L283); 
        if (L2841_) {
         L285 = ATTR(plus_self45,ntext);
         L286 = plus_s34;
         SATTR(plus_self45,ntext,FSTR_p1752847026(L285, (*dSTR_strrSTR[TAG(L286)])(L286)));
        }
        ret_val65 = plus_self45;
        plus_self46 = ret_val65;
        mang_self6 = self;
        mang_ob6 = ((OB) cl);
        ret_val66 = MANGLE119219986(PRINT_OB_mangler, mang_ob6, ((OB) NULL));
        plus_s35 = ((dSTR) ret_val66);
        L287 = (plus_s35==((dSTR) NULL));
        L2881_=!(L287); 
        if (L2881_) {
         L289 = ATTR(plus_self46,ntext);
         L290 = plus_s35;
         SATTR(plus_self46,ntext,FSTR_p1752847026(L289, (*dSTR_strrSTR[TAG(L290)])(L290)));
        }
        ret_val67 = plus_self46;
        plus_self47 = ret_val67;
        plus_s36 = ((dSTR) ((STR) &S0arr_1958137105));
        L292 = (plus_s36==((dSTR) NULL));
        L2931_=!(L292); 
        if (L2931_) {
         L294 = ATTR(plus_self47,ntext);
         L295 = plus_s36;
         SATTR(plus_self47,ntext,FSTR_p1752847026(L294, (*dSTR_strrSTR[TAG(L295)])(L295)));
        }
        ret_val68 = plus_self47;
        p = ret_val68;
       }
       else {
        plus_self48 = p;
        plus_s37 = ((dSTR) ((STR) &sizeof3));
        L296 = (plus_s37==((dSTR) NULL));
        L2971_=!(L296); 
        if (L2971_) {
         L298 = ATTR(plus_self48,ntext);
         L299 = plus_s37;
         SATTR(plus_self48,ntext,FSTR_p1752847026(L298, (*dSTR_strrSTR[TAG(L299)])(L299)));
        }
        ret_val69 = plus_self48;
        plus_self49 = ret_val69;
        mang_self7 = self;
        mang_ob7 = ((OB) cl);
        ret_val70 = MANGLE119219986(PRINT_OB_mangler, mang_ob7, ((OB) NULL));
        plus_s38 = ((dSTR) ret_val70);
        L300 = (plus_s38==((dSTR) NULL));
        L3021_=!(L300); 
        if (L3021_) {
         L303 = ATTR(plus_self49,ntext);
         L304 = plus_s38;
         SATTR(plus_self49,ntext,FSTR_p1752847026(L303, (*dSTR_strrSTR[TAG(L304)])(L304)));
        }
        ret_val71 = plus_self49;
        plus_self50 = ret_val71;
        plus_s39 = ((dSTR) ((STR) &S00));
        L305 = (plus_s39==((dSTR) NULL));
        L3061_=!(L305); 
        if (L3061_) {
         L307 = ATTR(plus_self50,ntext);
         L308 = plus_s39;
         SATTR(plus_self50,ntext,FSTR_p1752847026(L307, (*dSTR_strrSTR[TAG(L308)])(L308)));
        }
        ret_val72 = plus_self50;
        p = ret_val72;
       }
      }
     }
    }
    L310 = cl;
    L311 = ((*dTP_implrIMPL[TAG(L310)])(L310)==((IMPL) NULL));
    L3121_=!(L311); 
    if (L3121_) {
     L313 = cl;
     L314=(*dTP_implrIMPL[TAG(L313)])(L313);
     L315 = (ATTR(L314,arr)==((TP_CLASS) NULL));
     L3161_=!(L315); 
     L309 = L3161_;
    } else {
     L309 = FALSE;
    }
    if (L309) {
     plus_self51 = p;
     plus_s40 = ((dSTR) ((STR) &name60));
     L317 = (plus_s40==((dSTR) NULL));
     L3181_=!(L317); 
     if (L3181_) {
      L319 = ATTR(plus_self51,ntext);
      L320 = plus_s40;
      SATTR(plus_self51,ntext,FSTR_p1752847026(L319, (*dSTR_strrSTR[TAG(L320)])(L320)));
     }
     ret_val73 = plus_self51;
     plus_self52 = ret_val73;
     L3221_=INTMINUS(attrs,2); 
     L324=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
     if (L324==NULL) FATAL("Unable to allocate more memory");
     memset(L324,0,sizeof(struct INT_boxed_struct));
     ((OB)L324)->header.tag=INT_tag;
     L323 = (dSTR)((INT_boxed) L324);
     ((INT_boxed) L323)->immutable_part = L3221_;
     plus_s41 = L323;
     L325 = ATTR(plus_self52,ntext);
     L326 = plus_s41;
     SATTR(plus_self52,ntext,FSTR_p1752847026(L325, (*dSTR_strrSTR[TAG(L326)])(L326)));
    }
    else {
     plus_self53 = p;
     plus_s42 = ((dSTR) ((STR) &name60));
     L327 = (plus_s42==((dSTR) NULL));
     L3281_=!(L327); 
     if (L3281_) {
      L329 = ATTR(plus_self53,ntext);
      L330 = plus_s42;
      SATTR(plus_self53,ntext,FSTR_p1752847026(L329, (*dSTR_strrSTR[TAG(L330)])(L330)));
     }
     ret_val74 = plus_self53;
     plus_self54 = ret_val74;
     L333=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
     if (L333==NULL) FATAL("Unable to allocate more memory");
     memset(L333,0,sizeof(struct INT_boxed_struct));
     ((OB)L333)->header.tag=INT_tag;
     L332 = (dSTR)((INT_boxed) L333);
     ((INT_boxed) L332)->immutable_part = attrs;
     plus_s43 = L332;
     L334 = ATTR(plus_self54,ntext);
     L335 = plus_s43;
     SATTR(plus_self54,ntext,FSTR_p1752847026(L334, (*dSTR_strrSTR[TAG(L335)])(L335)));
    }
    L336 = cl;
    if ((*dTP_is1334578382[TAG(L336)])(L336)) {
     plus_self55 = p;
     plus_s44 = ((dSTR) ((STR) &S14));
     L337 = (plus_s44==((dSTR) NULL));
     L3381_=!(L337); 
     if (L3381_) {
      L339 = ATTR(plus_self55,ntext);
      L340 = plus_s44;
      SATTR(plus_self55,ntext,FSTR_p1752847026(L339, (*dSTR_strrSTR[TAG(L340)])(L340)));
     }
     ret_val75 = plus_self55;
     p = ret_val75;
    }
    else {
     plus_self56 = p;
     plus_s45 = ((dSTR) ((STR) &S010));
     L342 = (plus_s45==((dSTR) NULL));
     L3431_=!(L342); 
     if (L3431_) {
      L344 = ATTR(plus_self56,ntext);
      L345 = plus_s45;
      SATTR(plus_self56,ntext,FSTR_p1752847026(L344, (*dSTR_strrSTR[TAG(L345)])(L345)));
     }
     ret_val76 = plus_self56;
     p = ret_val76;
    }
    L349 = cl;
    L350 = (*dTP_is1334578382[TAG(L349)])(L349);
    L3521_=!(L350); 
    if (L3521_) {
     L353 = cl;
     L354 = (*dTP_is1811059018[TAG(L353)])(L353);
     L3551_=!(L354); 
     L348 = L3551_;
    } else {
     L348 = FALSE;
    }
    if (L348) {
     L356 = cl;
     L357 = (*dTP_is735947028[TAG(L356)])(L356);
     L3581_=!(L357); 
     L347 = L3581_;
    } else {
     L347 = FALSE;
    }
    if (L347) {
     L359 = cl;
     L360 = (*dTP_is124163553[TAG(L359)])(L359);
     L3621_=!(L360); 
     L346 = L3621_;
    } else {
     L346 = FALSE;
    }
    if (L346) {
     plus_self57 = p;
     plus_s46 = ((dSTR) ((STR) &S15));
     L363 = (plus_s46==((dSTR) NULL));
     L3641_=!(L363); 
     if (L3641_) {
      L365 = ATTR(plus_self57,ntext);
      L366 = plus_s46;
      SATTR(plus_self57,ntext,FSTR_p1752847026(L365, (*dSTR_strrSTR[TAG(L366)])(L366)));
     }
     ret_val77 = plus_self57;
     p = ret_val77;
    }
    else {
     plus_self58 = p;
     plus_s47 = ((dSTR) ((STR) &S05));
     L367 = (plus_s47==((dSTR) NULL));
     L3681_=!(L367); 
     if (L3681_) {
      L369 = ATTR(plus_self58,ntext);
      L370 = plus_s47;
      SATTR(plus_self58,ntext,FSTR_p1752847026(L369, (*dSTR_strrSTR[TAG(L370)])(L370)));
     }
     ret_val78 = plus_self58;
     p = ret_val78;
    }
    L371 = cl;
    if ((*dTP_is1811059018[TAG(L371)])(L371)) {
     plus_self59 = p;
     plus_s48 = ((dSTR) ((STR) &S15));
     L372 = (plus_s48==((dSTR) NULL));
     L3731_=!(L372); 
     if (L3731_) {
      L374 = ATTR(plus_self59,ntext);
      L375 = plus_s48;
      SATTR(plus_self59,ntext,FSTR_p1752847026(L374, (*dSTR_strrSTR[TAG(L375)])(L375)));
     }
     ret_val79 = plus_self59;
     p = ret_val79;
    }
    else {
     plus_self60 = p;
     plus_s49 = ((dSTR) ((STR) &S05));
     L376 = (plus_s49==((dSTR) NULL));
     L3771_=!(L376); 
     if (L3771_) {
      L378 = ATTR(plus_self60,ntext);
      L379 = plus_s49;
      SATTR(plus_self60,ntext,FSTR_p1752847026(L378, (*dSTR_strrSTR[TAG(L379)])(L379)));
     }
     ret_val80 = plus_self60;
     p = ret_val80;
    }
    L380 = cl;
    if ((*dTP_is735947028[TAG(L380)])(L380)) {
     plus_self61 = p;
     plus_s50 = ((dSTR) ((STR) &S15));
     L382 = (plus_s50==((dSTR) NULL));
     L3831_=!(L382); 
     if (L3831_) {
      L384 = ATTR(plus_self61,ntext);
      L385 = plus_s50;
      SATTR(plus_self61,ntext,FSTR_p1752847026(L384, (*dSTR_strrSTR[TAG(L385)])(L385)));
     }
     ret_val81 = plus_self61;
     p = ret_val81;
    }
    else {
     plus_self62 = p;
     plus_s51 = ((dSTR) ((STR) &S05));
     L386 = (plus_s51==((dSTR) NULL));
     L3871_=!(L386); 
     if (L3871_) {
      L388 = ATTR(plus_self62,ntext);
      L389 = plus_s51;
      SATTR(plus_self62,ntext,FSTR_p1752847026(L388, (*dSTR_strrSTR[TAG(L389)])(L389)));
     }
     ret_val82 = plus_self62;
     p = ret_val82;
    }
    L390 = cl;
    if ((*dTP_is124163553[TAG(L390)])(L390)) {
     plus_self63 = p;
     plus_s52 = ((dSTR) ((STR) &S15));
     L391 = (plus_s52==((dSTR) NULL));
     L3921_=!(L391); 
     if (L3921_) {
      L393 = ATTR(plus_self63,ntext);
      L394 = plus_s52;
      SATTR(plus_self63,ntext,FSTR_p1752847026(L393, (*dSTR_strrSTR[TAG(L394)])(L394)));
     }
     ret_val83 = plus_self63;
     p = ret_val83;
    }
    else {
     plus_self64 = p;
     plus_s53 = ((dSTR) ((STR) &S05));
     L395 = (plus_s53==((dSTR) NULL));
     L3961_=!(L395); 
     if (L3961_) {
      L397 = ATTR(plus_self64,ntext);
      L398 = plus_s53;
      SATTR(plus_self64,ntext,FSTR_p1752847026(L397, (*dSTR_strrSTR[TAG(L398)])(L398)));
     }
     ret_val84 = plus_self64;
     p = ret_val84;
    }
    L400 = cl;
    L401 = ((*dTP_implrIMPL[TAG(L400)])(L400)==((IMPL) NULL));
    L4021_=!(L401); 
    if (L4021_) {
     L403 = cl;
     L404=(*dTP_implrIMPL[TAG(L403)])(L403);
     L405 = (ATTR(L404,arr)==((TP_CLASS) NULL));
     L4061_=!(L405); 
     L399 = L4061_;
    } else {
     L399 = FALSE;
    }
    if (L399) {
     plus_self65 = p;
     plus_s54 = ((dSTR) ((STR) &S15));
     L407 = (plus_s54==((dSTR) NULL));
     L4081_=!(L407); 
     if (L4081_) {
      L409 = ATTR(plus_self65,ntext);
      L410 = plus_s54;
      SATTR(plus_self65,ntext,FSTR_p1752847026(L409, (*dSTR_strrSTR[TAG(L410)])(L410)));
     }
     ret_val85 = plus_self65;
     p = ret_val85;
    }
    else {
     plus_self66 = p;
     plus_s55 = ((dSTR) ((STR) &S05));
     L411 = (plus_s55==((dSTR) NULL));
     L4121_=!(L411); 
     if (L4121_) {
      L413 = ATTR(plus_self66,ntext);
      L414 = plus_s55;
      SATTR(plus_self66,ntext,FSTR_p1752847026(L413, (*dSTR_strrSTR[TAG(L414)])(L414)));
     }
     ret_val86 = plus_self66;
     p = ret_val86;
    }
    L417 = cl;
    L418 = (*dTP_is1811059018[TAG(L417)])(L417);
    L4191_=!(L418); 
    if (L4191_) {
     L420 = cl;
     L422 = ((*dTP_implrIMPL[TAG(L420)])(L420)==((IMPL) NULL));
     L4231_=!(L422); 
     L416 = L4231_;
    } else {
     L416 = FALSE;
    }
    if (L416) {
     L4241_=(0)<(attrs); 
     L415 = L4241_;
    } else {
     L415 = FALSE;
    }
    if (L415) {
     plus_self67 = p;
     plus_s56 = ((dSTR) ((STR) &name178));
     L425 = ATTR(plus_self67,ntext);
     L426 = plus_s56;
     SATTR(plus_self67,ntext,FSTR_p1752847026(L425, (*dSTR_strrSTR[TAG(L426)])(L426)));
     first1 = TRUE;
     i = 1;
     {
      BOOL f_L4271_ = TRUE;
      /* loop index variable */
      L81 = 0;
      L71 = as;
      L4271_br=L71==NULL?0:ASIZE((ARRAYATTRSORT)L71); 
      while (1) {
       if(L81>=L4271_br)  goto after_loop5; 
       aL4271_=ARR((ARRAYATTRSORT)L71,L81); 
       L429=aL4271_;
       e = ATTR(L429,elt1);
       if (ELT_is380037919(e)) {
        if (first1) {
         first1 = FALSE;
        }
        else {
         plus_self68 = p;
         plus_s57 = ((dSTR) ((STR) &name12));
         L430 = ATTR(plus_self68,ntext);
         L431 = plus_s57;
         SATTR(plus_self68,ntext,FSTR_p1752847026(L430, (*dSTR_strrSTR[TAG(L431)])(L431)));
        }
        plus_self69 = p;
        plus_s58 = ((dSTR) ((STR) &name179));
        L432 = (plus_s58==((dSTR) NULL));
        L4331_=!(L432); 
        if (L4331_) {
         L434 = ATTR(plus_self69,ntext);
         L435 = plus_s58;
         SATTR(plus_self69,ntext,FSTR_p1752847026(L434, (*dSTR_strrSTR[TAG(L435)])(L435)));
        }
        ret_val87 = plus_self69;
        plus_self70 = ret_val87;
        name_self1 = e;
        ret_val88 = ATTR(ATTR(name_self1,sig1),name1);
        L436=ret_val88;
        plus_s59 = ((dSTR) L436.str);
        L437 = (plus_s59==((dSTR) NULL));
        L4381_=!(L437); 
        if (L4381_) {
         L439 = ATTR(plus_self70,ntext);
         L440 = plus_s59;
         SATTR(plus_self70,ntext,FSTR_p1752847026(L439, (*dSTR_strrSTR[TAG(L440)])(L440)));
        }
        ret_val89 = plus_self70;
        plus_self71 = ret_val89;
        plus_s60 = ((dSTR) ((STR) &name142));
        L442 = (plus_s60==((dSTR) NULL));
        L4431_=!(L442); 
        if (L4431_) {
         L444 = ATTR(plus_self71,ntext);
         L445 = plus_s60;
         SATTR(plus_self71,ntext,FSTR_p1752847026(L444, (*dSTR_strrSTR[TAG(L445)])(L445)));
        }
        ret_val90 = plus_self71;
        plus_self72 = ret_val90;
        mang_self8 = self;
        name_self2 = e;
        ret_val91 = ATTR(ATTR(name_self2,sig1),name1);
        L447=ZALLOC(sizeof(struct IDENT_boxed_struct));
        if (L447==NULL) FATAL("Unable to allocate more memory");
        ((OB)L447)->header.tag=IDENT_tag;
        L446 = (OB)((IDENT_boxed) L447);
        ((IDENT_boxed) L446)->immutable_part = ret_val91;
        mang_ob8 = L446;
        mang_ns = ((OB) cl);
        ret_val92 = MANGLE119219986(PRINT_OB_mangler, mang_ob8, mang_ns);
        plus_s61 = ((dSTR) ret_val92);
        L448 = (plus_s61==((dSTR) NULL));
        L4491_=!(L448); 
        if (L4491_) {
         L450 = ATTR(plus_self72,ntext);
         L452 = plus_s61;
         SATTR(plus_self72,ntext,FSTR_p1752847026(L450, (*dSTR_strrSTR[TAG(L452)])(L452)));
        }
        ret_val93 = plus_self72;
        plus_self73 = ret_val93;
        plus_s62 = ((dSTR) ((STR) &name146));
        L453 = (plus_s62==((dSTR) NULL));
        L4541_=!(L453); 
        if (L4541_) {
         L455 = ATTR(plus_self73,ntext);
         L456 = plus_s62;
         SATTR(plus_self73,ntext,FSTR_p1752847026(L455, (*dSTR_strrSTR[TAG(L456)])(L456)));
        }
        ret_val94 = plus_self73;
        plus_self74 = ret_val94;
        tag_for_self = cgen;
        ret_self = e;
        ret_val95 = ATTR(ATTR(ret_self,sig1),ret);
        tag_for_tp = ret_val95;
        SATTR(tag_for_self,needs_tag,FSETdT1016814448(ATTR(tag_for_self,needs_tag), tag_for_tp));
        mang_self9 = tag_for_self;
        mang_ob9 = ((OB) tag_for_tp);
        ret_val97 = MANGLE119219986(ATTR(mang_self9,mangler), mang_ob9, ((OB) NULL));
        res = STR_ap2004550586(ret_val97, ((STR) &S_tag));
        forbid_self = tag_for_self;
        forbid_s = res;
        forbid_self1 = ATTR(forbid_self,mangler);
        forbid_s1 = forbid_s;
        SATTR(forbid_self1,forbidden,FSETST1404644833(ATTR(forbid_self1,forbidden), forbid_s1));
        ret_val96 = res;
        plus_s63 = ((dSTR) ret_val96);
        L457 = (plus_s63==((dSTR) NULL));
        L4581_=!(L457); 
        if (L4581_) {
         L459 = ATTR(plus_self74,ntext);
         L460 = plus_s63;
         SATTR(plus_self74,ntext,FSTR_p1752847026(L459, (*dSTR_strrSTR[TAG(L460)])(L460)));
        }
        ret_val98 = plus_self74;
        plus_self75 = ret_val98;
        plus_s64 = ((dSTR) ((STR) &int5));
        L462 = (plus_s64==((dSTR) NULL));
        L4631_=!(L462); 
        if (L4631_) {
         L464 = ATTR(plus_self75,ntext);
         L465 = plus_s64;
         SATTR(plus_self75,ntext,FSTR_p1752847026(L464, (*dSTR_strrSTR[TAG(L465)])(L465)));
        }
        ret_val99 = plus_self75;
        plus_self76 = ret_val99;
        mang_self10 = self;
        mang_ob10 = ((OB) cl);
        ret_val100 = MANGLE119219986(PRINT_OB_mangler, mang_ob10, ((OB) NULL));
        plus_s65 = ((dSTR) ret_val100);
        L466 = ATTR(plus_self76,ntext);
        L467 = plus_s65;
        SATTR(plus_self76,ntext,FSTR_p1752847026(L466, (*dSTR_strrSTR[TAG(L467)])(L467)));
        L468 = cl;
        if ((*dTP_is1334578382[TAG(L468)])(L468)) {
         plus_self77 = p;
         plus_s66 = ((dSTR) ((STR) &name58));
         L469 = (plus_s66==((dSTR) NULL));
         L4701_=!(L469); 
         if (L4701_) {
          L471 = ATTR(plus_self77,ntext);
          L472 = plus_s66;
          SATTR(plus_self77,ntext,FSTR_p1752847026(L471, (*dSTR_strrSTR[TAG(L472)])(L472)));
         }
         ret_val101 = plus_self77;
         p = ret_val101;
        }
        plus_self78 = p;
        plus_s67 = ((dSTR) ((STR) &S08));
        L473 = (plus_s67==((dSTR) NULL));
        L4741_=!(L473); 
        if (L4741_) {
         L475 = ATTR(plus_self78,ntext);
         L476 = plus_s67;
         SATTR(plus_self78,ntext,FSTR_p1752847026(L475, (*dSTR_strrSTR[TAG(L476)])(L476)));
        }
        ret_val102 = plus_self78;
        plus_self79 = ret_val102;
        mang_self11 = self;
        name_self3 = e;
        ret_val103 = ATTR(ATTR(name_self3,sig1),name1);
        L478=ZALLOC(sizeof(struct IDENT_boxed_struct));
        if (L478==NULL) FATAL("Unable to allocate more memory");
        ((OB)L478)->header.tag=IDENT_tag;
        L477 = (OB)((IDENT_boxed) L478);
        ((IDENT_boxed) L477)->immutable_part = ret_val103;
        mang_ob11 = L477;
        mang_ns1 = ((OB) cl);
        ret_val104 = MANGLE119219986(PRINT_OB_mangler, mang_ob11, mang_ns1);
        plus_s68 = ((dSTR) ret_val104);
        L479 = (plus_s68==((dSTR) NULL));
        L4801_=!(L479); 
        if (L4801_) {
         L482 = ATTR(plus_self79,ntext);
         L483 = plus_s68;
         SATTR(plus_self79,ntext,FSTR_p1752847026(L482, (*dSTR_strrSTR[TAG(L483)])(L483)));
        }
        ret_val105 = plus_self79;
        p = ret_val105;
        plus_self80 = p;
        plus_s69 = ((dSTR) ((STR) &name3));
        L484 = (plus_s69==((dSTR) NULL));
        L4851_=!(L484); 
        if (L4851_) {
         L486 = ATTR(plus_self80,ntext);
         L487 = plus_s69;
         SATTR(plus_self80,ntext,FSTR_p1752847026(L486, (*dSTR_strrSTR[TAG(L487)])(L487)));
        }
        ret_val106 = plus_self80;
        plus_self81 = ret_val106;
        L488 = ATTR(e,as);
        L490=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
        if (L490==NULL) FATAL("Unable to allocate more memory");
        memset(L490,0,sizeof(struct INT_boxed_struct));
        ((OB)L490)->header.tag=INT_tag;
        L489 = (dSTR)((INT_boxed) L490);
        ((INT_boxed) L489)->immutable_part = PRINT_1524431106(self, cgen, (*dAS_CL474111786[TAG(L488)])(L488));
        plus_s70 = L489;
        L492 = (plus_s70==((dSTR) NULL));
        L4931_=!(L492); 
        if (L4931_) {
         L494 = ATTR(plus_self81,ntext);
         L495 = plus_s70;
         SATTR(plus_self81,ntext,FSTR_p1752847026(L494, (*dSTR_strrSTR[TAG(L495)])(L495)));
        }
        ret_val107 = plus_self81;
        plus_self82 = ret_val107;
        plus_s71 = ((dSTR) ((STR) &name139));
        L496 = (plus_s71==((dSTR) NULL));
        L4971_=!(L496); 
        if (L4971_) {
         L498 = ATTR(plus_self82,ntext);
         L499 = plus_s71;
         SATTR(plus_self82,ntext,FSTR_p1752847026(L498, (*dSTR_strrSTR[TAG(L499)])(L499)));
        }
        ret_val108 = plus_self82;
        p = ret_val108;
       }
       L5001_=INTPLUS(L81,1); 
       L81 = L5001_;
      }
     }
     after_loop5: ;
     L501 = cl;
     L502=(*dTP_implrIMPL[TAG(L501)])(L501);
     L503 = (ATTR(L502,arr)==((TP_CLASS) NULL));
     L5041_=!(L503); 
     if (L5041_) {
      if (first1) {
       first1 = FALSE;
      }
      else {
       plus_self83 = p;
       plus_s72 = ((dSTR) ((STR) &name12));
       L505 = ATTR(plus_self83,ntext);
       L506 = plus_s72;
       SATTR(plus_self83,ntext,FSTR_p1752847026(L505, (*dSTR_strrSTR[TAG(L506)])(L506)));
      }
      plus_self84 = p;
      plus_s73 = ((dSTR) ((STR) &asizea1439666166));
      L507 = (plus_s73==((dSTR) NULL));
      L5081_=!(L507); 
      if (L5081_) {
       L509 = ATTR(plus_self84,ntext);
       L510 = plus_s73;
       SATTR(plus_self84,ntext,FSTR_p1752847026(L509, (*dSTR_strrSTR[TAG(L510)])(L510)));
      }
      ret_val109 = plus_self84;
      p = ret_val109;
      L511 = cl;
      if ((*dTP_is1334578382[TAG(L511)])(L511)) {
       plus_self85 = p;
       mang_self12 = self;
       mang_ob12 = ((OB) cl);
       ret_val110 = MANGLE119219986(PRINT_OB_mangler, mang_ob12, ((OB) NULL));
       plus_s74 = ((dSTR) ret_val110);
       L512 = (plus_s74==((dSTR) NULL));
       L5131_=!(L512); 
       if (L5131_) {
        L514 = ATTR(plus_self85,ntext);
        L515 = plus_s74;
        SATTR(plus_self85,ntext,FSTR_p1752847026(L514, (*dSTR_strrSTR[TAG(L515)])(L515)));
       }
       ret_val1111 = plus_self85;
       plus_self86 = ret_val1111;
       plus_s75 = ((dSTR) ((STR) &S_ASIZE));
       L516 = (plus_s75==((dSTR) NULL));
       L5171_=!(L516); 
       if (L5171_) {
        L518 = ATTR(plus_self86,ntext);
        L519 = plus_s75;
        SATTR(plus_self86,ntext,FSTR_p1752847026(L518, (*dSTR_strrSTR[TAG(L519)])(L519)));
       }
       ret_val112 = plus_self86;
       p = ret_val112;
      }
      else {
       plus_self87 = p;
       plus_s76 = ((dSTR) ((STR) &int6));
       L520 = (plus_s76==((dSTR) NULL));
       L5221_=!(L520); 
       if (L5221_) {
        L523 = ATTR(plus_self87,ntext);
        L524 = plus_s76;
        SATTR(plus_self87,ntext,FSTR_p1752847026(L523, (*dSTR_strrSTR[TAG(L524)])(L524)));
       }
       ret_val113 = plus_self87;
       plus_self88 = ret_val113;
       mang_self13 = self;
       mang_ob13 = ((OB) cl);
       ret_val114 = MANGLE119219986(PRINT_OB_mangler, mang_ob13, ((OB) NULL));
       plus_s77 = ((dSTR) ret_val114);
       L525 = (plus_s77==((dSTR) NULL));
       L5261_=!(L525); 
       if (L5261_) {
        L527 = ATTR(plus_self88,ntext);
        L528 = plus_s77;
        SATTR(plus_self88,ntext,FSTR_p1752847026(L527, (*dSTR_strrSTR[TAG(L528)])(L528)));
       }
       ret_val115 = plus_self88;
       plus_self89 = ret_val115;
       plus_s78 = ((dSTR) ((STR) &S0asize));
       L529 = (plus_s78==((dSTR) NULL));
       L5301_=!(L529); 
       if (L5301_) {
        L531 = ATTR(plus_self89,ntext);
        L532 = plus_s78;
        SATTR(plus_self89,ntext,FSTR_p1752847026(L531, (*dSTR_strrSTR[TAG(L532)])(L532)));
       }
       ret_val116 = plus_self89;
       p = ret_val116;
      }
      plus_self90 = p;
      plus_s79 = ((dSTR) ((STR) &S011));
      L533 = (plus_s79==((dSTR) NULL));
      L5341_=!(L533); 
      if (L5341_) {
       L535 = ATTR(plus_self90,ntext);
       L536 = plus_s79;
       SATTR(plus_self90,ntext,FSTR_p1752847026(L535, (*dSTR_strrSTR[TAG(L536)])(L536)));
      }
      ret_val117 = plus_self90;
      p = ret_val117;
      L537 = cl;
      L538=(*dTP_implrIMPL[TAG(L537)])(L537);
      L539 = ATTR(ATTR(L538,arr),params);
      L5401_=(dTP)ARR((ARRAYdTP)L539,0); 
      arr = L5401_;
      plus_self91 = p;
      plus_s80 = ((dSTR) ((STR) &arr_part5));
      L541 = (plus_s80==((dSTR) NULL));
      L5421_=!(L541); 
      if (L5421_) {
       L543 = ATTR(plus_self91,ntext);
       L544 = plus_s80;
       SATTR(plus_self91,ntext,FSTR_p1752847026(L543, (*dSTR_strrSTR[TAG(L544)])(L544)));
      }
      ret_val118 = plus_self91;
      plus_self92 = ret_val118;
      tag_for_self1 = cgen;
      tag_for_tp1 = arr;
      SATTR(tag_for_self1,needs_tag,FSETdT1016814448(ATTR(tag_for_self1,needs_tag), tag_for_tp1));
      mang_self14 = tag_for_self1;
      mang_ob14 = ((OB) tag_for_tp1);
      ret_val120 = MANGLE119219986(ATTR(mang_self14,mangler), mang_ob14, ((OB) NULL));
      res1 = STR_ap2004550586(ret_val120, ((STR) &S_tag));
      forbid_self2 = tag_for_self1;
      forbid_s2 = res1;
      forbid_self3 = ATTR(forbid_self2,mangler);
      forbid_s3 = forbid_s2;
      SATTR(forbid_self3,forbidden,FSETST1404644833(ATTR(forbid_self3,forbidden), forbid_s3));
      ret_val119 = res1;
      plus_s81 = ((dSTR) ret_val119);
      L545 = (plus_s81==((dSTR) NULL));
      L5461_=!(L545); 
      if (L5461_) {
       L547 = ATTR(plus_self92,ntext);
       L548 = plus_s81;
       SATTR(plus_self92,ntext,FSTR_p1752847026(L547, (*dSTR_strrSTR[TAG(L548)])(L548)));
      }
      ret_val121 = plus_self92;
      plus_self93 = ret_val121;
      plus_s82 = ((dSTR) ((STR) &int7));
      L549 = (plus_s82==((dSTR) NULL));
      L5501_=!(L549); 
      if (L5501_) {
       L551 = ATTR(plus_self93,ntext);
       L552 = plus_s82;
       SATTR(plus_self93,ntext,FSTR_p1752847026(L551, (*dSTR_strrSTR[TAG(L552)])(L552)));
      }
      ret_val122 = plus_self93;
      plus_self94 = ret_val122;
      mang_self15 = self;
      mang_ob15 = ((OB) cl);
      ret_val123 = MANGLE119219986(PRINT_OB_mangler, mang_ob15, ((OB) NULL));
      plus_s83 = ((dSTR) ret_val123);
      L553 = (plus_s83==((dSTR) NULL));
      L5541_=!(L553); 
      if (L5541_) {
       L555 = ATTR(plus_self94,ntext);
       L556 = plus_s83;
       SATTR(plus_self94,ntext,FSTR_p1752847026(L555, (*dSTR_strrSTR[TAG(L556)])(L556)));
      }
      ret_val124 = plus_self94;
      p = ret_val124;
      L557 = cl;
      if ((*dTP_is1334578382[TAG(L557)])(L557)) {
       plus_self95 = p;
       plus_s84 = ((dSTR) ((STR) &name58));
       L558 = (plus_s84==((dSTR) NULL));
       L5591_=!(L558); 
       if (L5591_) {
        L560 = ATTR(plus_self95,ntext);
        L562 = plus_s84;
        SATTR(plus_self95,ntext,FSTR_p1752847026(L560, (*dSTR_strrSTR[TAG(L562)])(L562)));
       }
       ret_val125 = plus_self95;
       p = ret_val125;
      }
      plus_self96 = p;
      plus_s85 = ((dSTR) ((STR) &S0arr_part0));
      L563 = (plus_s85==((dSTR) NULL));
      L5641_=!(L563); 
      if (L5641_) {
       L565 = ATTR(plus_self96,ntext);
       L566 = plus_s85;
       SATTR(plus_self96,ntext,FSTR_p1752847026(L565, (*dSTR_strrSTR[TAG(L566)])(L566)));
      }
      ret_val126 = plus_self96;
      p = ret_val126;
     }
     plus_self97 = p;
     plus_s86 = ((dSTR) ((STR) &name147));
     L567 = (plus_s86==((dSTR) NULL));
     L5681_=!(L567); 
     if (L5681_) {
      L569 = ATTR(plus_self97,ntext);
      L570 = plus_s86;
      SATTR(plus_self97,ntext,FSTR_p1752847026(L569, (*dSTR_strrSTR[TAG(L570)])(L570)));
     }
     ret_val127 = plus_self97;
     p = ret_val127;
    }
    plus_self98 = p;
    plus_s87 = ((dSTR) ((STR) &name148));
    L571 = (plus_s87==((dSTR) NULL));
    L5721_=!(L571); 
    if (L5721_) {
     L573 = ATTR(plus_self98,ntext);
     L574 = plus_s87;
     SATTR(plus_self98,ntext,FSTR_p1752847026(L573, (*dSTR_strrSTR[TAG(L574)])(L574)));
    }
    ret_val128 = plus_self98;
    p = ret_val128;
    CODE_F65146505(p);
   }
  }
 }
 after_loop1: ;
 plus_self99 = FSTR_p1752847026(dec, ((STR) &define2029181961));
 plus_i = PRINT_1541629256;
 str_in_self3 = plus_i;
 str_in_s3 = plus_self99;
 ret_val130 = INT_st367594495(str_in_self3, str_in_s3, 0, 10, ' ');
 ret_val129 = ret_val130;
 dec = FSTR_p1752847026(ret_val129, ((STR) &name9));
 PRINT_791786249(self, prog, cgen, smallest_tag, largest_tag);
 PRINT_481601799(self, prog, cgen, p);
 if (ATTR(cgen,func_tables)) {
  PRINT_463825405(self, prog, cgen);
 }
 ret_val = dec;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT PRINT_1524431106(PRINT_OB self, CGEN cgen, SFILE_ID s) {
 INT ret_val = INT_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L4;
 INT L51_;
 if (ATTR(cgen,func_tables)) {
  L1 = SFILE_1102877860(s);
  L21_=INTTIMES(L1,1048576); 
  L3 = L21_;
  L4 = SFILE_2070755978(s);
  L51_=INTPLUS(L3,L4); 
  ret_val = L51_;
  return ret_val;
 }
 else {
  ret_val = 0;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

void PRINT_116660897(PRINT_OB self, CODE_FILE p, dTP cl, dTP el, STR prefix1) {
 ELT_TBL L11;
 ELT e;
 dTP arr;
 INT L21 = INT_zero;
 INT L31 = INT_zero;
 INT i = INT_zero;
 INT L41 = INT_zero;
 INT L51 = INT_zero;
 INT i1 = INT_zero;
 ELT ret_self;
 dTP ret_val;
 ELT ret_self1;
 dTP ret_val1;
 ELT ret_self2;
 dTP ret_val2;
 CODE_FILE plus_self;
 dSTR plus_s;
 CODE_FILE ret_val3;
 ELT name_self;
 IDENT ret_val4 = IDENT_zero;
 PRINT_OB mang_self;
 OB mang_ob;
 OB mang_ns;
 STR ret_val5;
 CODE_FILE plus_self1;
 dSTR plus_s1;
 CODE_FILE ret_val6;
 CODE_FILE plus_self2;
 dSTR plus_s2;
 ELT ret_self3;
 dTP ret_val7;
 ELT ret_self4;
 dTP ret_val8;
 ELT name_self1;
 IDENT ret_val9 = IDENT_zero;
 PRINT_OB mang_self1;
 OB mang_ob1;
 OB mang_ns1;
 STR ret_val10;
 STR plus_self3;
 STR plus_sarg;
 STR ret_val11;
 STR plus_self4;
 STR plus_sarg1;
 STR ret_val12;
 CODE_FILE plus_self5;
 dSTR plus_s3;
 CODE_FILE ret_val13;
 CODE_FILE plus_self6;
 dSTR plus_s4;
 CODE_FILE ret_val14;
 CODE_FILE plus_self7;
 dSTR plus_s5;
 CODE_FILE ret_val15;
 CODE_FILE plus_self8;
 dSTR plus_s6;
 CODE_FILE ret_val16;
 CODE_FILE plus_self9;
 dSTR plus_s7;
 STR plus_self10;
 STR plus_sarg2;
 STR ret_val17;
 STR plus_self11;
 INT plus_arg = INT_zero;
 STR ret_val18;
 STR s;
 INT str_self = INT_zero;
 STR ret_val19;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val20;
 FSTR str_self1;
 STR ret_val21;
 STR plus_self12;
 STR plus_sarg3;
 STR ret_val22;
 ELT L6;
 dTP L7;
 IMPL L8;
 BOOL L9;
 BOOL L10;
 dTP L12;
 BOOL L13;
 BOOL L141_;
 dTP L15;
 BOOL L16;
 BOOL L171_;
 dTP L18;
 BOOL L19;
 BOOL L201_;
 BOOL L22;
 BOOL L231_;
 FSTR L24;
 dSTR L25;
 OB L26;
 OB L27;
 BOOL L28;
 BOOL L291_;
 FSTR L30;
 dSTR L32;
 extern STR name3;
 FSTR L33;
 dSTR L34;
 dTP L35;
 OB L36;
 OB L37;
 extern STR name18;
 BOOL L38;
 BOOL L39;
 dTP L40;
 dTP L42;
 BOOL L43;
 BOOL L441_;
 dTP L45;
 IMPL L46;
 BOOL L47;
 BOOL L481_;
 dTP L49;
 IMPL L50;
 ARRAYdTP L52;
 dTP L531_;
 BOOL L54;
 BOOL L55;
 dTP L56;
 BOOL L57;
 BOOL L581_;
 dTP L59;
 BOOL L60;
 BOOL L611_;
 dTP L62;
 BOOL L63;
 BOOL L641_;
 INT L66;
 dTP L67;
 INT L68;
 INT L691_;
 INT L651_,L651_m;
 BOOL L70;
 BOOL L711_;
 FSTR L72;
 dSTR L73;
 extern STR arr_part4;
 BOOL L74;
 BOOL L751_;
 FSTR L76;
 dSTR L77;
 dSTR L78;
 OB L79;
 BOOL L80;
 BOOL L811_;
 FSTR L82;
 dSTR L83;
 extern STR name7;
 BOOL L84;
 BOOL L851_;
 FSTR L86;
 dSTR L87;
 extern STR name3;
 FSTR L88;
 dSTR L89;
 dTP L90;
 INT L92;
 dTP L93;
 INT L94;
 INT L951_;
 INT L911_,L911_m;
 extern STR arr_part4;
 BOOL L96;
 BOOL L971_;
 extern STR name174;
 {
  struct ELT_TBL_eltbrELT_frame_struct other1_0;
ELT_TBL_eltbrELT_frame noname1 = &other1_0;
  noname1->state = 0;
  while (1) {
   if (noname1->state == 0) {
    L7 = el;
    L8=(*dTP_implrIMPL[TAG(L7)])(L7);
    L11 = ATTR(L8,elts);
    noname1->self = L11;
   }
   L6 = ELT_TBL_eltbrELT(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L6;
   if (ELT_is380037919(e)) {
    ret_self = e;
    ret_val = ATTR(ATTR(ret_self,sig1),ret);
    L12 = ret_val;
    L13 = (*dTP_is1334578382[TAG(L12)])(L12);
    L141_=!(L13); 
    if (L141_) {
     ret_self1 = e;
     ret_val1 = ATTR(ATTR(ret_self1,sig1),ret);
     L15 = ret_val1;
     L16 = (*dTP_is735947028[TAG(L15)])(L15);
     L171_=!(L16); 
     L10 = L171_;
    } else {
     L10 = FALSE;
    }
    if (L10) {
     ret_self2 = e;
     ret_val2 = ATTR(ATTR(ret_self2,sig1),ret);
     L18 = ret_val2;
     L19 = (*dTP_is124163553[TAG(L18)])(L18);
     L201_=!(L19); 
     L9 = L201_;
    } else {
     L9 = FALSE;
    }
    if (L9) {
     plus_self = p;
     plus_s = ((dSTR) prefix1);
     L22 = (plus_s==((dSTR) NULL));
     L231_=!(L22); 
     if (L231_) {
      L24 = ATTR(plus_self,ntext);
      L25 = plus_s;
      SATTR(plus_self,ntext,FSTR_p1752847026(L24, (*dSTR_strrSTR[TAG(L25)])(L25)));
     }
     ret_val3 = plus_self;
     plus_self1 = ret_val3;
     mang_self = self;
     name_self = e;
     ret_val4 = ATTR(ATTR(name_self,sig1),name1);
     L27=ZALLOC(sizeof(struct IDENT_boxed_struct));
     if (L27==NULL) FATAL("Unable to allocate more memory");
     ((OB)L27)->header.tag=IDENT_tag;
     L26 = (OB)((IDENT_boxed) L27);
     ((IDENT_boxed) L26)->immutable_part = ret_val4;
     mang_ob = L26;
     mang_ns = ((OB) el);
     ret_val5 = MANGLE119219986(PRINT_OB_mangler, mang_ob, mang_ns);
     plus_s1 = ((dSTR) ret_val5);
     L28 = (plus_s1==((dSTR) NULL));
     L291_=!(L28); 
     if (L291_) {
      L30 = ATTR(plus_self1,ntext);
      L32 = plus_s1;
      SATTR(plus_self1,ntext,FSTR_p1752847026(L30, (*dSTR_strrSTR[TAG(L32)])(L32)));
     }
     ret_val6 = plus_self1;
     plus_self2 = ret_val6;
     plus_s2 = ((dSTR) ((STR) &name3));
     L33 = ATTR(plus_self2,ntext);
     L34 = plus_s2;
     SATTR(plus_self2,ntext,FSTR_p1752847026(L33, (*dSTR_strrSTR[TAG(L34)])(L34)));
    }
    else {
     ret_self3 = e;
     ret_val7 = ATTR(ATTR(ret_self3,sig1),ret);
     L35 = ret_val7;
     if ((*dTP_is1334578382[TAG(L35)])(L35)) {
      ret_self4 = e;
      ret_val8 = ATTR(ATTR(ret_self4,sig1),ret);
      plus_self3 = prefix1;
      mang_self1 = self;
      name_self1 = e;
      ret_val9 = ATTR(ATTR(name_self1,sig1),name1);
      L37=ZALLOC(sizeof(struct IDENT_boxed_struct));
      if (L37==NULL) FATAL("Unable to allocate more memory");
      ((OB)L37)->header.tag=IDENT_tag;
      L36 = (OB)((IDENT_boxed) L37);
      ((IDENT_boxed) L36)->immutable_part = ret_val9;
      mang_ob1 = L36;
      mang_ns1 = ((OB) el);
      ret_val10 = MANGLE119219986(PRINT_OB_mangler, mang_ob1, mang_ns1);
      plus_sarg = ret_val10;
      ret_val11 = STR_ap2004550586(plus_self3, plus_sarg);
      plus_self4 = ret_val11;
      plus_sarg1 = ((STR) &name18);
      ret_val12 = STR_ap2004550586(plus_self4, plus_sarg1);
      PRINT_116660897(self, p, cl, ret_val8, ret_val12);
     }
    }
   }
  }
 }
 after_loop: ;
 L40 = el;
 if ((*dTP_is1334578382[TAG(L40)])(L40)) {
  L42 = el;
  L43 = ((*dTP_implrIMPL[TAG(L42)])(L42)==((IMPL) NULL));
  L441_=!(L43); 
  L39 = L441_;
 } else {
  L39 = FALSE;
 }
 if (L39) {
  L45 = el;
  L46=(*dTP_implrIMPL[TAG(L45)])(L45);
  L47 = (ATTR(L46,arr)==((TP_CLASS) NULL));
  L481_=!(L47); 
  L38 = L481_;
 } else {
  L38 = FALSE;
 }
 if (L38) {
  L49 = el;
  L50=(*dTP_implrIMPL[TAG(L49)])(L49);
  L52 = ATTR(ATTR(L50,arr),params);
  L531_=(dTP)ARR((ARRAYdTP)L52,0); 
  arr = L531_;
  L56 = arr;
  L57 = (*dTP_is1334578382[TAG(L56)])(L56);
  L581_=!(L57); 
  if (L581_) {
   L59 = arr;
   L60 = (*dTP_is735947028[TAG(L59)])(L59);
   L611_=!(L60); 
   L55 = L611_;
  } else {
   L55 = FALSE;
  }
  if (L55) {
   L62 = arr;
   L63 = (*dTP_is124163553[TAG(L62)])(L62);
   L641_=!(L63); 
   L54 = L641_;
  } else {
   L54 = FALSE;
  }
  if (L54) {
   {
    BOOL f_L651_ = TRUE;
    while (1) {
     if (f_L651_) {
      f_L651_ = FALSE;
      L21 = 0;
      L67 = el;
      L68 = IMPL_a1897060099((*dTP_implrIMPL[TAG(L67)])(L67));
      L691_=INTMINUS(L68,1); 
      L31 = L691_;
      L651_=L21-1;L651_m=L31; 
     }
     if(L651_++>=L651_m)  goto after_loop1; 
     i = L651_;
     plus_self5 = p;
     plus_s3 = ((dSTR) prefix1);
     L70 = (plus_s3==((dSTR) NULL));
     L711_=!(L70); 
     if (L711_) {
      L72 = ATTR(plus_self5,ntext);
      L73 = plus_s3;
      SATTR(plus_self5,ntext,FSTR_p1752847026(L72, (*dSTR_strrSTR[TAG(L73)])(L73)));
     }
     ret_val13 = plus_self5;
     plus_self6 = ret_val13;
     plus_s4 = ((dSTR) ((STR) &arr_part4));
     L74 = (plus_s4==((dSTR) NULL));
     L751_=!(L74); 
     if (L751_) {
      L76 = ATTR(plus_self6,ntext);
      L77 = plus_s4;
      SATTR(plus_self6,ntext,FSTR_p1752847026(L76, (*dSTR_strrSTR[TAG(L77)])(L77)));
     }
     ret_val14 = plus_self6;
     plus_self7 = ret_val14;
     L79=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
     if (L79==NULL) FATAL("Unable to allocate more memory");
     memset(L79,0,sizeof(struct INT_boxed_struct));
     ((OB)L79)->header.tag=INT_tag;
     L78 = (dSTR)((INT_boxed) L79);
     ((INT_boxed) L78)->immutable_part = i;
     plus_s5 = L78;
     L80 = (plus_s5==((dSTR) NULL));
     L811_=!(L80); 
     if (L811_) {
      L82 = ATTR(plus_self7,ntext);
      L83 = plus_s5;
      SATTR(plus_self7,ntext,FSTR_p1752847026(L82, (*dSTR_strrSTR[TAG(L83)])(L83)));
     }
     ret_val15 = plus_self7;
     plus_self8 = ret_val15;
     plus_s6 = ((dSTR) ((STR) &name7));
     L84 = (plus_s6==((dSTR) NULL));
     L851_=!(L84); 
     if (L851_) {
      L86 = ATTR(plus_self8,ntext);
      L87 = plus_s6;
      SATTR(plus_self8,ntext,FSTR_p1752847026(L86, (*dSTR_strrSTR[TAG(L87)])(L87)));
     }
     ret_val16 = plus_self8;
     plus_self9 = ret_val16;
     plus_s7 = ((dSTR) ((STR) &name3));
     L88 = ATTR(plus_self9,ntext);
     L89 = plus_s7;
     SATTR(plus_self9,ntext,FSTR_p1752847026(L88, (*dSTR_strrSTR[TAG(L89)])(L89)));
    }
   }
   after_loop1: ;
  }
  else {
   L90 = arr;
   if ((*dTP_is1334578382[TAG(L90)])(L90)) {
    {
     BOOL f_L911_ = TRUE;
     while (1) {
      if (f_L911_) {
       f_L911_ = FALSE;
       L41 = 0;
       L93 = el;
       L94 = IMPL_a1897060099((*dTP_implrIMPL[TAG(L93)])(L93));
       L951_=INTMINUS(L94,1); 
       L51 = L951_;
       L911_=L41-1;L911_m=L51; 
      }
      if(L911_++>=L911_m)  goto after_loop2; 
      i1 = L911_;
      plus_self10 = prefix1;
      plus_sarg2 = ((STR) &arr_part4);
      ret_val17 = STR_ap2004550586(plus_self10, plus_sarg2);
      plus_self11 = ret_val17;
      plus_arg = i1;
      str_self = plus_arg;
      clear_self = buf1;
      L96 = (clear_self==((FSTR) NULL));
      L971_=!(L96); 
      if (L971_) {
       SATTR(clear_self,loc,0);
      }
      str_in_self = str_self;
      str_in_s = buf1;
      ret_val20 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
      buf1 = ret_val20;
      str_self1 = buf1;
      ret_val21 = STR_fr1097270334(((STR) NULL), str_self1);
      ret_val19 = ret_val21;
      s = ret_val19;
      ret_val18 = STR_ap1077157958(plus_self11, s, TRUE);
      plus_self12 = ret_val18;
      plus_sarg3 = ((STR) &name174);
      ret_val22 = STR_ap2004550586(plus_self12, plus_sarg3);
      PRINT_116660897(self, p, cl, arr, ret_val22);
     }
    }
    after_loop2: ;
   }
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

void PRINT_463825405(PRINT_OB self, PROG prog, CGEN cgen) {
 CODE_FILE c;
 ARRAYTUPSIGINT cs;
 FLISTSIG L11;
 INT L21 = INT_zero;
 ARRAYTUPSIGINT L31;
 ARRAYTUPSIGINT L41;
 INT L51 = INT_zero;
 ARRAYTUPSIGINT L61;
 ARRAYTUPSIGINT L71;
 ARRAYTUPSIGINT create_self;
 INT create_n = INT_zero;
 ARRAYTUPSIGINT ret_val;
 TUPSIGINT create_self1 = TUPSIGINT_zero;
 SIG create_at1;
 INT create_at2 = INT_zero;
 TUPSIGINT ret_val1 = TUPSIGINT_zero;
 INT L81 = INT_zero;
 INT L91 = INT_zero;
 STR plus_self;
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
 STR plus_self1;
 STR plus_sarg;
 STR ret_val6;
 STR plus_self2;
 INT plus_arg1 = INT_zero;
 STR ret_val7;
 STR s1;
 INT str_self2 = INT_zero;
 STR ret_val8;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val9;
 FSTR str_self3;
 STR ret_val10;
 STR plus_self3;
 STR plus_sarg1;
 STR ret_val11;
 CODE_FILE plus_self4;
 dSTR plus_s;
 CODE_FILE plus_self5;
 dSTR plus_s1;
 INT L101 = INT_zero;
 PRINT_OB mang_self;
 OB mang_ob;
 STR ret_val12;
 STR plus_self6;
 STR plus_sarg2;
 STR ret_val13;
 STR plus_self7;
 STR plus_sarg3;
 STR ret_val14;
 CODE_FILE plus_self8;
 dSTR plus_s2;
 CODE_FILE plus_self9;
 dSTR plus_s3;
 CODE_FILE plus_self10;
 dSTR plus_s4;
 INT L121 = INT_zero;
 PRINT_OB mang_self1;
 OB mang_ob1;
 STR ret_val15;
 STR plus_self11;
 STR plus_sarg4;
 STR ret_val16;
 STR plus_self12;
 STR plus_sarg5;
 STR ret_val17;
 CODE_FILE plus_self13;
 dSTR plus_s5;
 CODE_FILE plus_self14;
 dSTR plus_s6;
 extern STR sather_funcc;
 ARRAYTUPSIGINT L13;
 INT L14;
 OB L15;
 INT L16;
 INT L171_;
 INT L181_br;
 SIG L19;
 INT L20;
 TUPSIGINT L22;
 TUPSIGINT L23;
 TUPSIGINT L24;
 INT L251_;
 bound1_ob L26;
 INT L271_br;
 TUPSIGINT aL271_;
 INT L281_;
 extern STR defineF_tag_;
 TUPSIGINT L29;
 TUPSIGINT L30;
 BOOL L32;
 BOOL L331_;
 extern STR name10;
 INT L34;
 BOOL L35;
 BOOL L361_;
 extern STR name9;
 FSTR L37;
 dSTR L38;
 INT L391_;
 extern STR define1568744415;
 FSTR L40;
 dSTR L42;
 INT L431_br;
 TUPSIGINT aL431_;
 extern STR ES;
 TUPSIGINT L44;
 TUPSIGINT L45;
 extern STR S_iframe1;
 FSTR L46;
 dSTR L47;
 INT L481_;
 extern STR name9;
 FSTR L49;
 dSTR L50;
 extern STR struct1935008194;
 FSTR L52;
 dSTR L53;
 INT L541_br;
 TUPSIGINT aL541_;
 extern STR name175;
 TUPSIGINT L55;
 TUPSIGINT L56;
 extern STR S_iframe2;
 FSTR L57;
 dSTR L58;
 INT L591_;
 extern STR NULL9;
 FSTR L60;
 dSTR L62;
 c = CODE_F1432464034(((CODE_FILE) NULL), ((STR) &sather_funcc));
 SATTR(c,is_c_code,TRUE);
 SATTR(c,do_not_merge,TRUE);
 create_self = ((ARRAYTUPSIGINT) NULL);
 create_n = FLISTS228922656(PRINT_OB_funcs);
 L14 = create_n;
 L16=(sizeof(struct ARRAYTUPSIGINT_struct)-sizeof(TUPSIGINT))+(L14)*sizeof(TUPSIGINT);
 L15=ZALLOC_BIG(L16);
 if (L15==NULL) FATAL("Unable to allocate more memory");
 ((OB)L15)->header.tag=ARRAYTUPSIGINT_tag;
#ifdef DESTROY_CHK

   ((OB)L15)->header.destroyed=0;
#endif

 L13 = ((ARRAYTUPSIGINT) L15);
 L13->asize = L14;
 ret_val = L13;
 cs = ret_val;
 {
  struct FLISTS1074219534_frame_struct other1_0;
FLISTS1074219534_frame noname1 = &other1_0;
  BOOL f_L171_ = TRUE;
  BOOL f_L181_ = TRUE;
  L11 = PRINT_OB_funcs;
  L21 = 0;
  /* loop index variable */
  L81 = 0;
  L31 = cs;
  noname1->self = L11;
  noname1->state = 0;
  L171_=L21-1; 
  L181_br=L31==NULL?0:ASIZE((ARRAYTUPSIGINT)L31); 
  while (1) {
   if(L81>=L181_br)  goto after_loop; 
   create_self1 = TUPSIGINT_zero;
   L19 = FLISTS1074219534(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   create_at1 = L19;
   L171_++; 
   create_at2 = L171_;
   L22 = create_self1;
   L22.t1 = create_at1;
   L23 = L22;
   L23.t2 = create_at2;
   ret_val1 = L23;
   L24 = ret_val1;
   SARR((ARRAYTUPSIGINT)L31,L81,L24); 
   ;
   L251_=INTPLUS(L81,1); 
   L81 = L251_;
  }
 }
 after_loop: ;
 L26 = (bound1_ob) ZALLOC(sizeof(struct bound1_ob_struct));
 L26->header.tag = ROUTTU467247893_tag;
 L26->funcptr = bound1;
 L26->bound_arg0 = self;
 ARRAYT215015885(cs, (ROUTTU467247893) L26);
 {
  BOOL f_L271_ = TRUE;
  BOOL f_L281_ = TRUE;
  /* loop index variable */
  L91 = 0;
  L41 = cs;
  L51 = 0;
  L271_br=L41==NULL?0:ASIZE((ARRAYTUPSIGINT)L41); 
  L281_=L51-1; 
  while (1) {
   plus_self4 = ATTR(cgen,tags_h);
   plus_self = ((STR) &defineF_tag_);
   if(L91>=L271_br)  goto after_loop1; 
   aL271_=ARR((ARRAYTUPSIGINT)L41,L91); 
   L30=aL271_;
   plus_arg = L30.t2;
   str_self = plus_arg;
   clear_self = buf1;
   L32 = (clear_self==((FSTR) NULL));
   L331_=!(L32); 
   if (L331_) {
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
   ret_val2 = STR_ap1077157958(plus_self, s, TRUE);
   plus_self1 = ret_val2;
   plus_sarg = ((STR) &name10);
   ret_val6 = STR_ap2004550586(plus_self1, plus_sarg);
   plus_self2 = ret_val6;
   L281_++; 
   plus_arg1 = L281_;
   str_self2 = plus_arg1;
   clear_self1 = buf2;
   L35 = (clear_self1==((FSTR) NULL));
   L361_=!(L35); 
   if (L361_) {
    SATTR(clear_self1,loc,0);
   }
   str_in_self1 = str_self2;
   str_in_s1 = buf2;
   ret_val9 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
   buf2 = ret_val9;
   str_self3 = buf2;
   ret_val10 = STR_fr1097270334(((STR) NULL), str_self3);
   ret_val8 = ret_val10;
   s1 = ret_val8;
   ret_val7 = STR_ap1077157958(plus_self2, s1, TRUE);
   plus_self3 = ret_val7;
   plus_sarg1 = ((STR) &name9);
   ret_val11 = STR_ap2004550586(plus_self3, plus_sarg1);
   plus_s = ((dSTR) ret_val11);
   L37 = ATTR(plus_self4,ntext);
   L38 = plus_s;
   SATTR(plus_self4,ntext,FSTR_p1752847026(L37, (*dSTR_strrSTR[TAG(L38)])(L38)));
   L391_=INTPLUS(L91,1); 
   L91 = L391_;
  }
 }
 after_loop1: ;
 plus_self5 = c;
 plus_s1 = ((dSTR) ((STR) &define1568744415));
 L40 = ATTR(plus_self5,ntext);
 L42 = plus_s1;
 SATTR(plus_self5,ntext,FSTR_p1752847026(L40, (*dSTR_strrSTR[TAG(L42)])(L42)));
 {
  BOOL f_L431_ = TRUE;
  /* loop index variable */
  L101 = 0;
  L61 = cs;
  L431_br=L61==NULL?0:ASIZE((ARRAYTUPSIGINT)L61); 
  while (1) {
   plus_self8 = c;
   plus_self6 = ((STR) &ES);
   mang_self = self;
   if(L101>=L431_br)  goto after_loop2; 
   aL431_=ARR((ARRAYTUPSIGINT)L61,L101); 
   L45=aL431_;
   mang_ob = ((OB) L45.t1);
   ret_val12 = MANGLE119219986(PRINT_OB_mangler, mang_ob, ((OB) NULL));
   plus_sarg2 = ret_val12;
   ret_val13 = STR_ap2004550586(plus_self6, plus_sarg2);
   plus_self7 = ret_val13;
   plus_sarg3 = ((STR) &S_iframe1);
   ret_val14 = STR_ap2004550586(plus_self7, plus_sarg3);
   plus_s2 = ((dSTR) ret_val14);
   L46 = ATTR(plus_self8,ntext);
   L47 = plus_s2;
   SATTR(plus_self8,ntext,FSTR_p1752847026(L46, (*dSTR_strrSTR[TAG(L47)])(L47)));
   L481_=INTPLUS(L101,1); 
   L101 = L481_;
  }
 }
 after_loop2: ;
 plus_self9 = c;
 plus_s3 = ((dSTR) ((STR) &name9));
 L49 = ATTR(plus_self9,ntext);
 L50 = plus_s3;
 SATTR(plus_self9,ntext,FSTR_p1752847026(L49, (*dSTR_strrSTR[TAG(L50)])(L50)));
 plus_self10 = c;
 plus_s4 = ((dSTR) ((STR) &struct1935008194));
 L52 = ATTR(plus_self10,ntext);
 L53 = plus_s4;
 SATTR(plus_self10,ntext,FSTR_p1752847026(L52, (*dSTR_strrSTR[TAG(L53)])(L53)));
 {
  BOOL f_L541_ = TRUE;
  /* loop index variable */
  L121 = 0;
  L71 = cs;
  L541_br=L71==NULL?0:ASIZE((ARRAYTUPSIGINT)L71); 
  while (1) {
   plus_self13 = c;
   plus_self11 = ((STR) &name175);
   mang_self1 = self;
   if(L121>=L541_br)  goto after_loop3; 
   aL541_=ARR((ARRAYTUPSIGINT)L71,L121); 
   L56=aL541_;
   mang_ob1 = ((OB) L56.t1);
   ret_val15 = MANGLE119219986(PRINT_OB_mangler, mang_ob1, ((OB) NULL));
   plus_sarg4 = ret_val15;
   ret_val16 = STR_ap2004550586(plus_self11, plus_sarg4);
   plus_self12 = ret_val16;
   plus_sarg5 = ((STR) &S_iframe2);
   ret_val17 = STR_ap2004550586(plus_self12, plus_sarg5);
   plus_s5 = ((dSTR) ret_val17);
   L57 = ATTR(plus_self13,ntext);
   L58 = plus_s5;
   SATTR(plus_self13,ntext,FSTR_p1752847026(L57, (*dSTR_strrSTR[TAG(L58)])(L58)));
   L591_=INTPLUS(L121,1); 
   L121 = L591_;
  }
 }
 after_loop3: ;
 plus_self14 = c;
 plus_s6 = ((dSTR) ((STR) &NULL9));
 L60 = ATTR(plus_self14,ntext);
 L62 = plus_s6;
 SATTR(plus_self14,ntext,FSTR_p1752847026(L60, (*dSTR_strrSTR[TAG(L62)])(L62)));
}


#undef IS_ITER
#define IS_ITER 0

void PRINT_481601799(PRINT_OB self, PROG prog, CGEN cgen, CODE_FILE p) {
 FLISTSTR L11;
 CODE_FILE plus_self;
 dSTR plus_s;
 STR plus_self1;
 STR plus_sarg;
 STR ret_val;
 STR plus_self2;
 STR plus_sarg1;
 STR ret_val1;
 CODE_FILE plus_self3;
 dSTR plus_s1;
 CODE_FILE plus_self4;
 dSTR plus_s2;
 extern STR charsource_files;
 FSTR L2;
 dSTR L3;
 extern STR name71;
 STR L4;
 extern STR name143;
 FSTR L5;
 dSTR L6;
 extern STR name171;
 FSTR L7;
 dSTR L8;
 plus_self = p;
 plus_s = ((dSTR) ((STR) &charsource_files));
 L2 = ATTR(plus_self,ntext);
 L3 = plus_s;
 SATTR(plus_self,ntext,FSTR_p1752847026(L2, (*dSTR_strrSTR[TAG(L3)])(L3)));
 {
  struct FLISTS1171631134_frame_struct other1_0;
FLISTS1171631134_frame noname1 = &other1_0;
  L11 = SFILE_ID_files;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   plus_self3 = p;
   plus_self1 = ((STR) &name71);
   L4 = FLISTS1171631134(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   plus_sarg = L4;
   ret_val = STR_ap2004550586(plus_self1, plus_sarg);
   plus_self2 = ret_val;
   plus_sarg1 = ((STR) &name143);
   ret_val1 = STR_ap2004550586(plus_self2, plus_sarg1);
   plus_s1 = ((dSTR) ret_val1);
   L5 = ATTR(plus_self3,ntext);
   L6 = plus_s1;
   SATTR(plus_self3,ntext,FSTR_p1752847026(L5, (*dSTR_strrSTR[TAG(L6)])(L6)));
  }
 }
 after_loop: ;
 plus_self4 = p;
 plus_s2 = ((dSTR) ((STR) &name171));
 L7 = ATTR(plus_self4,ntext);
 L8 = plus_s2;
 SATTR(plus_self4,ntext,FSTR_p1752847026(L7, (*dSTR_strrSTR[TAG(L8)])(L8)));
}


#undef IS_ITER
#define IS_ITER 0

void PRINT_721582855(PRINT_OB self, PROG prog, CGEN cgen, CODE_FILE p, AM_ROUT_DEF o11, FLISTA725283029 iout) {
 INT n = INT_zero;
 INT args = INT_zero;
 INT locals11 = INT_zero;
 FLISTA725283029 L11;
 AM_LOCAL_EXPR l;
 BOOL first1 = BOOL_zero;
 AM_ROUT_DEF L21;
 INT i = INT_zero;
 AM_LOCAL_EXPR s;
 FLISTA725283029 L31;
 FLISTA725283029 L41;
 AM_LOCAL_EXPR l1;
 AM_LOCAL_EXPR is_eq_self;
 OB is_eq_a;
 BOOL ret_val = BOOL_zero;
 CODE_FILE plus_self;
 dSTR plus_s;
 CODE_FILE plus_self1;
 dSTR plus_s1;
 CODE_FILE ret_val1;
 CODE_FILE plus_self2;
 dSTR plus_s2;
 CODE_FILE ret_val2;
 CODE_FILE plus_self3;
 dSTR plus_s3;
 CODE_FILE plus_self4;
 dSTR plus_s4;
 CODE_FILE ret_val3;
 PRINT_OB mang_self;
 OB mang_ob;
 STR ret_val4;
 CODE_FILE plus_self5;
 dSTR plus_s5;
 CODE_FILE ret_val5;
 CODE_FILE plus_self6;
 dSTR plus_s6;
 CODE_FILE plus_self7;
 dSTR plus_s7;
 CODE_FILE ret_val6;
 PRINT_OB mang_self1;
 OB mang_ob1;
 STR ret_val7;
 CODE_FILE plus_self8;
 dSTR plus_s8;
 CODE_FILE ret_val8;
 CODE_FILE plus_self9;
 dSTR plus_s9;
 CODE_FILE plus_self10;
 dSTR plus_s10;
 CODE_FILE ret_val9;
 CODE_FILE plus_self11;
 dSTR plus_s11;
 CODE_FILE ret_val10;
 CODE_FILE plus_self12;
 dSTR plus_s12;
 CODE_FILE plus_self13;
 dSTR plus_s13;
 CODE_FILE ret_val11;
 CODE_FILE plus_self14;
 dSTR plus_s14;
 CODE_FILE ret_val12;
 CODE_FILE plus_self15;
 dSTR plus_s15;
 CODE_FILE ret_val13;
 PRINT_OB mang_self2;
 OB mang_ob2;
 STR ret_val14;
 CODE_FILE plus_self16;
 dSTR plus_s16;
 CODE_FILE ret_val15;
 CODE_FILE plus_self17;
 dSTR plus_s17;
 CODE_FILE plus_self18;
 dSTR plus_s18;
 CODE_FILE ret_val16;
 CODE_FILE plus_self19;
 dSTR plus_s19;
 CODE_FILE ret_val17;
 CODE_FILE plus_self20;
 dSTR plus_s20;
 CODE_FILE ret_val18;
 CODE_FILE plus_self21;
 dSTR plus_s21;
 CODE_FILE plus_self22;
 dSTR plus_s22;
 INT L51 = INT_zero;
 CODE_FILE plus_self23;
 dSTR plus_s23;
 SIG is_iter_self;
 BOOL ret_val19 = BOOL_zero;
 IDENT is_iter_self1 = IDENT_zero;
 BOOL ret_val20 = BOOL_zero;
 CODE_FILE plus_self24;
 dSTR plus_s24;
 CODE_FILE ret_val21;
 AM_FORMAL_ARG name_self;
 IDENT ret_val22 = IDENT_zero;
 CODE_FILE plus_self25;
 dSTR plus_s25;
 CODE_FILE ret_val23;
 CODE_FILE plus_self26;
 dSTR plus_s26;
 CODE_FILE ret_val24;
 PRINT_OB mang_self3;
 OB mang_ob3;
 OB mang_ns;
 STR ret_val25;
 CODE_FILE plus_self27;
 dSTR plus_s27;
 CODE_FILE ret_val26;
 CODE_FILE plus_self28;
 dSTR plus_s28;
 AM_FORMAL_ARG tp_self;
 dTP ret_val27;
 AM_LOCAL_EXPR tp_self1;
 dTP ret_val28;
 CGEN tag_for_self;
 dTP tag_for_tp;
 STR ret_val29;
 STR res;
 CGEN mang_self4;
 OB mang_ob4;
 STR ret_val30;
 CGEN forbid_self;
 STR forbid_s;
 MANGLE forbid_self1;
 STR forbid_s1;
 CODE_FILE plus_self29;
 dSTR plus_s29;
 CODE_FILE ret_val31;
 CODE_FILE plus_self30;
 dSTR plus_s30;
 CODE_FILE ret_val32;
 CODE_FILE plus_self31;
 dSTR plus_s31;
 CODE_FILE ret_val33;
 CODE_FILE plus_self32;
 dSTR plus_s32;
 AM_LOCAL_EXPR is_eq_self1;
 OB is_eq_a1;
 BOOL ret_val34 = BOOL_zero;
 CODE_FILE plus_self33;
 dSTR plus_s33;
 CODE_FILE plus_self34;
 dSTR plus_s34;
 CODE_FILE ret_val35;
 CODE_FILE plus_self35;
 dSTR plus_s35;
 CODE_FILE ret_val36;
 CODE_FILE plus_self36;
 dSTR plus_s36;
 CODE_FILE ret_val37;
 PRINT_OB mang_self5;
 OB mang_ob5;
 OB mang_ns1;
 STR ret_val38;
 CODE_FILE plus_self37;
 dSTR plus_s37;
 CODE_FILE ret_val39;
 CODE_FILE plus_self38;
 dSTR plus_s38;
 AM_LOCAL_EXPR tp_self2;
 dTP ret_val40;
 CGEN tag_for_self1;
 dTP tag_for_tp1;
 STR ret_val41;
 STR res1;
 CGEN mang_self6;
 OB mang_ob6;
 STR ret_val42;
 CGEN forbid_self2;
 STR forbid_s2;
 MANGLE forbid_self3;
 STR forbid_s3;
 CODE_FILE plus_self39;
 dSTR plus_s39;
 CODE_FILE ret_val43;
 CODE_FILE plus_self40;
 dSTR plus_s40;
 CODE_FILE ret_val44;
 CODE_FILE plus_self41;
 dSTR plus_s41;
 CODE_FILE ret_val45;
 CODE_FILE plus_self42;
 dSTR plus_s42;
 CODE_FILE plus_self43;
 dSTR plus_s43;
 CODE_FILE plus_self44;
 dSTR plus_s44;
 INT L61_;
 AM_LOCAL_EXPR L7;
 BOOL L8;
 BOOL L9;
 BOOL L101_;
 BOOL L12;
 BOOL L131_;
 INT L141_;
 extern STR struct328711801;
 FSTR L15;
 dSTR L16;
 INT L171_;
 INT L18;
 BOOL L191_;
 extern STR struct682057613;
 BOOL L20;
 BOOL L221_;
 FSTR L23;
 dSTR L24;
 INT L251_;
 dSTR L26;
 OB L27;
 BOOL L28;
 BOOL L291_;
 FSTR L30;
 dSTR L32;
 extern STR name86;
 FSTR L33;
 dSTR L34;
 extern STR name140;
 BOOL L35;
 BOOL L361_;
 FSTR L37;
 dSTR L38;
 BOOL L39;
 BOOL L401_;
 FSTR L42;
 dSTR L43;
 extern STR S_iframe;
 FSTR L44;
 dSTR L45;
 extern STR void10;
 BOOL L46;
 BOOL L471_;
 FSTR L48;
 dSTR L49;
 BOOL L50;
 BOOL L521_;
 FSTR L53;
 dSTR L54;
 extern STR name12;
 FSTR L55;
 dSTR L56;
 extern STR name60;
 BOOL L57;
 BOOL L581_;
 FSTR L59;
 dSTR L60;
 dSTR L62;
 OB L63;
 BOOL L64;
 BOOL L651_;
 FSTR L66;
 dSTR L67;
 extern STR name12;
 FSTR L68;
 dSTR L69;
 extern STR name141;
 BOOL L70;
 BOOL L711_;
 FSTR L72;
 dSTR L73;
 BOOL L74;
 BOOL L751_;
 FSTR L76;
 dSTR L77;
 extern STR name142;
 BOOL L78;
 BOOL L791_;
 FSTR L80;
 dSTR L81;
 BOOL L82;
 BOOL L831_;
 FSTR L84;
 dSTR L85;
 extern STR name143;
 FSTR L86;
 dSTR L87;
 extern STR name60;
 BOOL L88;
 BOOL L891_;
 FSTR L90;
 dSTR L91;
 dSTR L92;
 OB L93;
 BOOL L94;
 BOOL L951_;
 FSTR L96;
 dSTR L97;
 extern STR name29;
 BOOL L98;
 BOOL L991_;
 FSTR L100;
 dSTR L102;
 dSTR L103;
 OB L104;
 FSTR L105;
 dSTR L106;
 INT L1071_;
 INT L108;
 BOOL L1091_;
 extern STR name144;
 FSTR L110;
 dSTR L111;
 INT L1121_br;
 INT rL1121_;
 INT L113;
 INT L1141_;
 BOOL L1151_;
 extern STR name3;
 FSTR L116;
 dSTR L117;
 AM_FORMAL_ARG L1181_;
 AM_FORMAL_ARG L119;
 BOOL L120;
 BOOL L121;
 BOOL L122;
 BOOL L1231_;
 STR L124;
 INT L125;
 INT L1261_;
 INT L127;
 CHAR L1281_;
 CHAR L129;
 BOOL L1301_;
 BOOL L132;
 AM_FORMAL_ARG L1331_;
 AM_FORMAL_ARG L134;
 dMODE L135;
 AM_FORMAL_ARG L1361_;
 AM_FORMAL_ARG L137;
 dMODE L138;
 AM_LOCAL_EXPR L139;
 extern STR name145;
 BOOL L140;
 BOOL L1421_;
 FSTR L143;
 dSTR L144;
 AM_FORMAL_ARG L1451_;
 IDENT L146;
 BOOL L147;
 BOOL L1481_;
 FSTR L149;
 dSTR L150;
 extern STR name142;
 BOOL L151;
 BOOL L1521_;
 FSTR L153;
 dSTR L154;
 BOOL L155;
 BOOL L1561_;
 FSTR L157;
 dSTR L158;
 extern STR name146;
 FSTR L159;
 dSTR L160;
 AM_FORMAL_ARG L1611_;
 extern STR S_tag;
 BOOL L162;
 BOOL L1631_;
 FSTR L164;
 dSTR L165;
 extern STR S0;
 BOOL L166;
 BOOL L1671_;
 FSTR L168;
 dSTR L169;
 dSTR L170;
 OB L172;
 BOOL L173;
 BOOL L1741_;
 FSTR L175;
 dSTR L176;
 extern STR name139;
 FSTR L177;
 dSTR L178;
 INT L1791_;
 AM_LOCAL_EXPR L180;
 BOOL L181;
 BOOL L182;
 BOOL L1831_;
 BOOL L184;
 BOOL L1851_;
 INT L1861_;
 BOOL L1871_;
 extern STR name3;
 FSTR L188;
 dSTR L189;
 extern STR name145;
 BOOL L190;
 BOOL L1921_;
 FSTR L193;
 dSTR L194;
 BOOL L195;
 BOOL L1961_;
 FSTR L197;
 dSTR L198;
 extern STR name142;
 BOOL L199;
 BOOL L2001_;
 FSTR L201;
 dSTR L202;
 BOOL L203;
 BOOL L2041_;
 FSTR L205;
 dSTR L206;
 extern STR name146;
 FSTR L207;
 dSTR L208;
 extern STR S_tag;
 BOOL L209;
 BOOL L2101_;
 FSTR L211;
 dSTR L212;
 extern STR S0;
 BOOL L213;
 BOOL L2141_;
 FSTR L215;
 dSTR L216;
 dSTR L217;
 OB L218;
 BOOL L219;
 BOOL L2201_;
 FSTR L222;
 dSTR L223;
 extern STR name139;
 FSTR L224;
 dSTR L225;
 extern STR name147;
 FSTR L226;
 dSTR L227;
 extern STR name148;
 FSTR L228;
 dSTR L229;
 INT L230;
 BOOL L2311_;
 PRINT_OB_mangler = ATTR(cgen,mangler);
 n = 0;
 L61_=(o11)==NULL?0:ASIZE((AM_ROUT_DEF)o11); 
 args = L61_;
 locals11 = 0;
 {
  struct FLISTA2119642552_frame_struct other1_0;
FLISTA2119642552_frame noname1 = &other1_0;
  L11 = ATTR(o11,locals1);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L7 = FLISTA2119642552(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   l = L7;
   is_eq_self = l;
   is_eq_a = ((OB) ATTR(o11,rres));
   ret_val = SYSOBEQ(((OB) is_eq_self),is_eq_a);
   L9 = ret_val;
   L101_=!(L9); 
   if (L101_) {
    L12 = (ATTR(l,name1).str==(STR)0);
    L131_=!(L12); 
    L8 = L131_;
   } else {
    L8 = FALSE;
   }
   if (L8) {
    L141_=INTPLUS(locals11,1); 
    locals11 = L141_;
   }
  }
 }
 after_loop: ;
 plus_self = p;
 plus_s = ((dSTR) ((STR) &struct328711801));
 L15 = ATTR(plus_self,ntext);
 L16 = plus_s;
 SATTR(plus_self,ntext,FSTR_p1752847026(L15, (*dSTR_strrSTR[TAG(L16)])(L16)));
 L171_=INTPLUS(args,locals11); 
 L18 = L171_;
 L191_=(0)<(L18); 
 if (L191_) {
  plus_self1 = p;
  plus_s1 = ((dSTR) ((STR) &struct682057613));
  L20 = (plus_s1==((dSTR) NULL));
  L221_=!(L20); 
  if (L221_) {
   L23 = ATTR(plus_self1,ntext);
   L24 = plus_s1;
   SATTR(plus_self1,ntext,FSTR_p1752847026(L23, (*dSTR_strrSTR[TAG(L24)])(L24)));
  }
  ret_val1 = plus_self1;
  plus_self2 = ret_val1;
  L251_=INTPLUS(locals11,args); 
  L27=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L27==NULL) FATAL("Unable to allocate more memory");
  memset(L27,0,sizeof(struct INT_boxed_struct));
  ((OB)L27)->header.tag=INT_tag;
  L26 = (dSTR)((INT_boxed) L27);
  ((INT_boxed) L26)->immutable_part = L251_;
  plus_s2 = L26;
  L28 = (plus_s2==((dSTR) NULL));
  L291_=!(L28); 
  if (L291_) {
   L30 = ATTR(plus_self2,ntext);
   L32 = plus_s2;
   SATTR(plus_self2,ntext,FSTR_p1752847026(L30, (*dSTR_strrSTR[TAG(L32)])(L32)));
  }
  ret_val2 = plus_self2;
  plus_self3 = ret_val2;
  plus_s3 = ((dSTR) ((STR) &name86));
  L33 = ATTR(plus_self3,ntext);
  L34 = plus_s3;
  SATTR(plus_self3,ntext,FSTR_p1752847026(L33, (*dSTR_strrSTR[TAG(L34)])(L34)));
 }
 plus_self4 = p;
 plus_s4 = ((dSTR) ((STR) &name140));
 L35 = (plus_s4==((dSTR) NULL));
 L361_=!(L35); 
 if (L361_) {
  L37 = ATTR(plus_self4,ntext);
  L38 = plus_s4;
  SATTR(plus_self4,ntext,FSTR_p1752847026(L37, (*dSTR_strrSTR[TAG(L38)])(L38)));
 }
 ret_val3 = plus_self4;
 plus_self5 = ret_val3;
 mang_self = self;
 mang_ob = ((OB) ATTR(o11,sig1));
 ret_val4 = MANGLE119219986(PRINT_OB_mangler, mang_ob, ((OB) NULL));
 plus_s5 = ((dSTR) ret_val4);
 L39 = (plus_s5==((dSTR) NULL));
 L401_=!(L39); 
 if (L401_) {
  L42 = ATTR(plus_self5,ntext);
  L43 = plus_s5;
  SATTR(plus_self5,ntext,FSTR_p1752847026(L42, (*dSTR_strrSTR[TAG(L43)])(L43)));
 }
 ret_val5 = plus_self5;
 plus_self6 = ret_val5;
 plus_s6 = ((dSTR) ((STR) &S_iframe));
 L44 = ATTR(plus_self6,ntext);
 L45 = plus_s6;
 SATTR(plus_self6,ntext,FSTR_p1752847026(L44, (*dSTR_strrSTR[TAG(L45)])(L45)));
 plus_self7 = p;
 plus_s7 = ((dSTR) ((STR) &void10));
 L46 = (plus_s7==((dSTR) NULL));
 L471_=!(L46); 
 if (L471_) {
  L48 = ATTR(plus_self7,ntext);
  L49 = plus_s7;
  SATTR(plus_self7,ntext,FSTR_p1752847026(L48, (*dSTR_strrSTR[TAG(L49)])(L49)));
 }
 ret_val6 = plus_self7;
 plus_self8 = ret_val6;
 mang_self1 = self;
 mang_ob1 = ((OB) ATTR(o11,sig1));
 ret_val7 = MANGLE119219986(PRINT_OB_mangler, mang_ob1, ((OB) NULL));
 plus_s8 = ((dSTR) ret_val7);
 L50 = (plus_s8==((dSTR) NULL));
 L521_=!(L50); 
 if (L521_) {
  L53 = ATTR(plus_self8,ntext);
  L54 = plus_s8;
  SATTR(plus_self8,ntext,FSTR_p1752847026(L53, (*dSTR_strrSTR[TAG(L54)])(L54)));
 }
 ret_val8 = plus_self8;
 plus_self9 = ret_val8;
 plus_s9 = ((dSTR) ((STR) &name12));
 L55 = ATTR(plus_self9,ntext);
 L56 = plus_s9;
 SATTR(plus_self9,ntext,FSTR_p1752847026(L55, (*dSTR_strrSTR[TAG(L56)])(L56)));
 plus_self10 = p;
 plus_s10 = ((dSTR) ((STR) &name60));
 L57 = (plus_s10==((dSTR) NULL));
 L581_=!(L57); 
 if (L581_) {
  L59 = ATTR(plus_self10,ntext);
  L60 = plus_s10;
  SATTR(plus_self10,ntext,FSTR_p1752847026(L59, (*dSTR_strrSTR[TAG(L60)])(L60)));
 }
 ret_val9 = plus_self10;
 plus_self11 = ret_val9;
 L63=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
 if (L63==NULL) FATAL("Unable to allocate more memory");
 memset(L63,0,sizeof(struct INT_boxed_struct));
 ((OB)L63)->header.tag=INT_tag;
 L62 = (dSTR)((INT_boxed) L63);
 ((INT_boxed) L62)->immutable_part = PRINT_1524431106(self, cgen, ATTR(o11,source1));
 plus_s11 = L62;
 L64 = (plus_s11==((dSTR) NULL));
 L651_=!(L64); 
 if (L651_) {
  L66 = ATTR(plus_self11,ntext);
  L67 = plus_s11;
  SATTR(plus_self11,ntext,FSTR_p1752847026(L66, (*dSTR_strrSTR[TAG(L67)])(L67)));
 }
 ret_val10 = plus_self11;
 plus_self12 = ret_val10;
 plus_s12 = ((dSTR) ((STR) &name12));
 L68 = ATTR(plus_self12,ntext);
 L69 = plus_s12;
 SATTR(plus_self12,ntext,FSTR_p1752847026(L68, (*dSTR_strrSTR[TAG(L69)])(L69)));
 plus_self13 = p;
 plus_s13 = ((dSTR) ((STR) &name141));
 L70 = (plus_s13==((dSTR) NULL));
 L711_=!(L70); 
 if (L711_) {
  L72 = ATTR(plus_self13,ntext);
  L73 = plus_s13;
  SATTR(plus_self13,ntext,FSTR_p1752847026(L72, (*dSTR_strrSTR[TAG(L73)])(L73)));
 }
 ret_val11 = plus_self13;
 plus_self14 = ret_val11;
 plus_s14 = ((dSTR) ATTR(ATTR(o11,sig1),str));
 L74 = (plus_s14==((dSTR) NULL));
 L751_=!(L74); 
 if (L751_) {
  L76 = ATTR(plus_self14,ntext);
  L77 = plus_s14;
  SATTR(plus_self14,ntext,FSTR_p1752847026(L76, (*dSTR_strrSTR[TAG(L77)])(L77)));
 }
 ret_val12 = plus_self14;
 plus_self15 = ret_val12;
 plus_s15 = ((dSTR) ((STR) &name142));
 L78 = (plus_s15==((dSTR) NULL));
 L791_=!(L78); 
 if (L791_) {
  L80 = ATTR(plus_self15,ntext);
  L81 = plus_s15;
  SATTR(plus_self15,ntext,FSTR_p1752847026(L80, (*dSTR_strrSTR[TAG(L81)])(L81)));
 }
 ret_val13 = plus_self15;
 plus_self16 = ret_val13;
 mang_self2 = self;
 mang_ob2 = ((OB) ATTR(o11,sig1));
 ret_val14 = MANGLE119219986(PRINT_OB_mangler, mang_ob2, ((OB) NULL));
 plus_s16 = ((dSTR) ret_val14);
 L82 = (plus_s16==((dSTR) NULL));
 L831_=!(L82); 
 if (L831_) {
  L84 = ATTR(plus_self16,ntext);
  L85 = plus_s16;
  SATTR(plus_self16,ntext,FSTR_p1752847026(L84, (*dSTR_strrSTR[TAG(L85)])(L85)));
 }
 ret_val15 = plus_self16;
 plus_self17 = ret_val15;
 plus_s17 = ((dSTR) ((STR) &name143));
 L86 = ATTR(plus_self17,ntext);
 L87 = plus_s17;
 SATTR(plus_self17,ntext,FSTR_p1752847026(L86, (*dSTR_strrSTR[TAG(L87)])(L87)));
 plus_self18 = p;
 plus_s18 = ((dSTR) ((STR) &name60));
 L88 = (plus_s18==((dSTR) NULL));
 L891_=!(L88); 
 if (L891_) {
  L90 = ATTR(plus_self18,ntext);
  L91 = plus_s18;
  SATTR(plus_self18,ntext,FSTR_p1752847026(L90, (*dSTR_strrSTR[TAG(L91)])(L91)));
 }
 ret_val16 = plus_self18;
 plus_self19 = ret_val16;
 L93=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
 if (L93==NULL) FATAL("Unable to allocate more memory");
 memset(L93,0,sizeof(struct INT_boxed_struct));
 ((OB)L93)->header.tag=INT_tag;
 L92 = (dSTR)((INT_boxed) L93);
 ((INT_boxed) L92)->immutable_part = args;
 plus_s19 = L92;
 L94 = (plus_s19==((dSTR) NULL));
 L951_=!(L94); 
 if (L951_) {
  L96 = ATTR(plus_self19,ntext);
  L97 = plus_s19;
  SATTR(plus_self19,ntext,FSTR_p1752847026(L96, (*dSTR_strrSTR[TAG(L97)])(L97)));
 }
 ret_val17 = plus_self19;
 plus_self20 = ret_val17;
 plus_s20 = ((dSTR) ((STR) &name29));
 L98 = (plus_s20==((dSTR) NULL));
 L991_=!(L98); 
 if (L991_) {
  L100 = ATTR(plus_self20,ntext);
  L102 = plus_s20;
  SATTR(plus_self20,ntext,FSTR_p1752847026(L100, (*dSTR_strrSTR[TAG(L102)])(L102)));
 }
 ret_val18 = plus_self20;
 plus_self21 = ret_val18;
 L104=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
 if (L104==NULL) FATAL("Unable to allocate more memory");
 memset(L104,0,sizeof(struct INT_boxed_struct));
 ((OB)L104)->header.tag=INT_tag;
 L103 = (dSTR)((INT_boxed) L104);
 ((INT_boxed) L103)->immutable_part = locals11;
 plus_s21 = L103;
 L105 = ATTR(plus_self21,ntext);
 L106 = plus_s21;
 SATTR(plus_self21,ntext,FSTR_p1752847026(L105, (*dSTR_strrSTR[TAG(L106)])(L106)));
 L1071_=INTPLUS(args,locals11); 
 L108 = L1071_;
 L1091_=(0)<(L108); 
 if (L1091_) {
  plus_self22 = p;
  plus_s22 = ((dSTR) ((STR) &name144));
  L110 = ATTR(plus_self22,ntext);
  L111 = plus_s22;
  SATTR(plus_self22,ntext,FSTR_p1752847026(L110, (*dSTR_strrSTR[TAG(L111)])(L111)));
  first1 = TRUE;
  {
   BOOL f_L1121_ = TRUE;
   struct FLISTA2119642552_frame_struct other2_0;
FLISTA2119642552_frame noname2 = &other2_0;
   /* loop index variable */
   L51 = 0;
   L21 = o11;
   L31 = iout;
   L1121_br=L21==NULL?0:ASIZE((AM_ROUT_DEF)L21); 
   noname2->self = L31;
   noname2->state = 0;
   while (1) {
    if(L51>=L1121_br)  goto after_loop1; 
    rL1121_=L51; 
    i = rL1121_;
    L1141_=INTPLUS(n,1); 
    n = L1141_;
    L1151_=!(first1); 
    if (L1151_) {
     plus_self23 = p;
     plus_s23 = ((dSTR) ((STR) &name3));
     L116 = ATTR(plus_self23,ntext);
     L117 = plus_s23;
     SATTR(plus_self23,ntext,FSTR_p1752847026(L116, (*dSTR_strrSTR[TAG(L117)])(L117)));
    }
    first1 = FALSE;
    L1181_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)o11,i); 
    L119=L1181_;
    s = ATTR(L119,expr);
    is_iter_self = ATTR(o11,sig1);
    is_iter_self1 = ATTR(is_iter_self,name1);
    L122 = (is_iter_self1.str==((STR) NULL));
    L1231_=!(L122); 
    if (L1231_) {
     L124 = is_iter_self1.str;
     L125 = STR_sizerINT(is_iter_self1.str);
     L1261_=INTMINUS(L125,1); 
     L127 = L1261_;
     L1281_=ARR((STR)L124,L127); 
     L129 = L1281_;
     L1301_=L129=='!'; 
     L121 = L1301_;
    } else {
     L121 = FALSE;
    }
    ret_val20 = L121;
    ret_val19 = ret_val20;
    if (ret_val19) {
     L1331_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)o11,i); 
     L134=L1331_;
     L135 = ATTR(L134,mode1);
     if ((*dMODE_814247358[TAG(L135)])(L135, ((dMODE) MODES_inout_mode))) {
      L132 = TRUE;
     } else {
      L1361_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)o11,i); 
      L137=L1361_;
      L138 = ATTR(L137,mode1);
      L132 = (*dMODE_814247358[TAG(L138)])(L138, ((dMODE) MODES_out_mode));
     }
     L120 = L132;
    } else {
     L120 = FALSE;
    }
    if (L120) {
     L139 = FLISTA2119642552(noname2);
     if (noname2->state == -1) {
      goto after_loop1;
     }
     s = L139;
    }
    plus_self24 = p;
    plus_s24 = ((dSTR) ((STR) &name145));
    L140 = (plus_s24==((dSTR) NULL));
    L1421_=!(L140); 
    if (L1421_) {
     L143 = ATTR(plus_self24,ntext);
     L144 = plus_s24;
     SATTR(plus_self24,ntext,FSTR_p1752847026(L143, (*dSTR_strrSTR[TAG(L144)])(L144)));
    }
    ret_val21 = plus_self24;
    plus_self25 = ret_val21;
    L1451_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)o11,i); 
    name_self = L1451_;
    ret_val22 = ATTR(ATTR(name_self,expr),name1);
    L146=ret_val22;
    plus_s25 = ((dSTR) L146.str);
    L147 = (plus_s25==((dSTR) NULL));
    L1481_=!(L147); 
    if (L1481_) {
     L149 = ATTR(plus_self25,ntext);
     L150 = plus_s25;
     SATTR(plus_self25,ntext,FSTR_p1752847026(L149, (*dSTR_strrSTR[TAG(L150)])(L150)));
    }
    ret_val23 = plus_self25;
    plus_self26 = ret_val23;
    plus_s26 = ((dSTR) ((STR) &name142));
    L151 = (plus_s26==((dSTR) NULL));
    L1521_=!(L151); 
    if (L1521_) {
     L153 = ATTR(plus_self26,ntext);
     L154 = plus_s26;
     SATTR(plus_self26,ntext,FSTR_p1752847026(L153, (*dSTR_strrSTR[TAG(L154)])(L154)));
    }
    ret_val24 = plus_self26;
    plus_self27 = ret_val24;
    mang_self3 = self;
    mang_ob3 = ((OB) s);
    mang_ns = ((OB) ATTR(o11,sig1));
    ret_val25 = MANGLE119219986(PRINT_OB_mangler, mang_ob3, mang_ns);
    plus_s27 = ((dSTR) ret_val25);
    L155 = (plus_s27==((dSTR) NULL));
    L1561_=!(L155); 
    if (L1561_) {
     L157 = ATTR(plus_self27,ntext);
     L158 = plus_s27;
     SATTR(plus_self27,ntext,FSTR_p1752847026(L157, (*dSTR_strrSTR[TAG(L158)])(L158)));
    }
    ret_val26 = plus_self27;
    plus_self28 = ret_val26;
    plus_s28 = ((dSTR) ((STR) &name146));
    L159 = ATTR(plus_self28,ntext);
    L160 = plus_s28;
    SATTR(plus_self28,ntext,FSTR_p1752847026(L159, (*dSTR_strrSTR[TAG(L160)])(L160)));
    plus_self29 = p;
    tag_for_self = cgen;
    L1611_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)o11,i); 
    tp_self = L1611_;
    tp_self1 = ATTR(tp_self,expr);
    ret_val28 = ATTR(tp_self1,tp_at);
    ret_val27 = ret_val28;
    tag_for_tp = ret_val27;
    SATTR(tag_for_self,needs_tag,FSETdT1016814448(ATTR(tag_for_self,needs_tag), tag_for_tp));
    mang_self4 = tag_for_self;
    mang_ob4 = ((OB) tag_for_tp);
    ret_val30 = MANGLE119219986(ATTR(mang_self4,mangler), mang_ob4, ((OB) NULL));
    res = STR_ap2004550586(ret_val30, ((STR) &S_tag));
    forbid_self = tag_for_self;
    forbid_s = res;
    forbid_self1 = ATTR(forbid_self,mangler);
    forbid_s1 = forbid_s;
    SATTR(forbid_self1,forbidden,FSETST1404644833(ATTR(forbid_self1,forbidden), forbid_s1));
    ret_val29 = res;
    plus_s29 = ((dSTR) ret_val29);
    L162 = (plus_s29==((dSTR) NULL));
    L1631_=!(L162); 
    if (L1631_) {
     L164 = ATTR(plus_self29,ntext);
     L165 = plus_s29;
     SATTR(plus_self29,ntext,FSTR_p1752847026(L164, (*dSTR_strrSTR[TAG(L165)])(L165)));
    }
    ret_val31 = plus_self29;
    plus_self30 = ret_val31;
    plus_s30 = ((dSTR) ((STR) &S0));
    L166 = (plus_s30==((dSTR) NULL));
    L1671_=!(L166); 
    if (L1671_) {
     L168 = ATTR(plus_self30,ntext);
     L169 = plus_s30;
     SATTR(plus_self30,ntext,FSTR_p1752847026(L168, (*dSTR_strrSTR[TAG(L169)])(L169)));
    }
    ret_val32 = plus_self30;
    plus_self31 = ret_val32;
    L172=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
    if (L172==NULL) FATAL("Unable to allocate more memory");
    memset(L172,0,sizeof(struct INT_boxed_struct));
    ((OB)L172)->header.tag=INT_tag;
    L170 = (dSTR)((INT_boxed) L172);
    ((INT_boxed) L170)->immutable_part = PRINT_1524431106(self, cgen, ATTR(o11,source1));
    plus_s31 = L170;
    L173 = (plus_s31==((dSTR) NULL));
    L1741_=!(L173); 
    if (L1741_) {
     L175 = ATTR(plus_self31,ntext);
     L176 = plus_s31;
     SATTR(plus_self31,ntext,FSTR_p1752847026(L175, (*dSTR_strrSTR[TAG(L176)])(L176)));
    }
    ret_val33 = plus_self31;
    plus_self32 = ret_val33;
    plus_s32 = ((dSTR) ((STR) &name139));
    L177 = ATTR(plus_self32,ntext);
    L178 = plus_s32;
    SATTR(plus_self32,ntext,FSTR_p1752847026(L177, (*dSTR_strrSTR[TAG(L178)])(L178)));
    L1791_=INTPLUS(L51,1); 
    L51 = L1791_;
   }
  }
  after_loop1: ;
  {
   struct FLISTA2119642552_frame_struct other3_0;
FLISTA2119642552_frame noname3 = &other3_0;
   L41 = ATTR(o11,locals1);
   noname3->self = L41;
   noname3->state = 0;
   while (1) {
    L180 = FLISTA2119642552(noname3);
    if (noname3->state == -1) {
     goto after_loop2;
    }
    l1 = L180;
    is_eq_self1 = l1;
    is_eq_a1 = ((OB) ATTR(o11,rres));
    ret_val34 = SYSOBEQ(((OB) is_eq_self1),is_eq_a1);
    L182 = ret_val34;
    L1831_=!(L182); 
    if (L1831_) {
     L184 = (ATTR(l1,name1).str==(STR)0);
     L1851_=!(L184); 
     L181 = L1851_;
    } else {
     L181 = FALSE;
    }
    if (L181) {
     L1861_=INTPLUS(n,1); 
     n = L1861_;
     L1871_=!(first1); 
     if (L1871_) {
      plus_self33 = p;
      plus_s33 = ((dSTR) ((STR) &name3));
      L188 = ATTR(plus_self33,ntext);
      L189 = plus_s33;
      SATTR(plus_self33,ntext,FSTR_p1752847026(L188, (*dSTR_strrSTR[TAG(L189)])(L189)));
     }
     first1 = FALSE;
     plus_self34 = p;
     plus_s34 = ((dSTR) ((STR) &name145));
     L190 = (plus_s34==((dSTR) NULL));
     L1921_=!(L190); 
     if (L1921_) {
      L193 = ATTR(plus_self34,ntext);
      L194 = plus_s34;
      SATTR(plus_self34,ntext,FSTR_p1752847026(L193, (*dSTR_strrSTR[TAG(L194)])(L194)));
     }
     ret_val35 = plus_self34;
     plus_self35 = ret_val35;
     plus_s35 = ((dSTR) ATTR(l1,name1).str);
     L195 = (plus_s35==((dSTR) NULL));
     L1961_=!(L195); 
     if (L1961_) {
      L197 = ATTR(plus_self35,ntext);
      L198 = plus_s35;
      SATTR(plus_self35,ntext,FSTR_p1752847026(L197, (*dSTR_strrSTR[TAG(L198)])(L198)));
     }
     ret_val36 = plus_self35;
     plus_self36 = ret_val36;
     plus_s36 = ((dSTR) ((STR) &name142));
     L199 = (plus_s36==((dSTR) NULL));
     L2001_=!(L199); 
     if (L2001_) {
      L201 = ATTR(plus_self36,ntext);
      L202 = plus_s36;
      SATTR(plus_self36,ntext,FSTR_p1752847026(L201, (*dSTR_strrSTR[TAG(L202)])(L202)));
     }
     ret_val37 = plus_self36;
     plus_self37 = ret_val37;
     mang_self5 = self;
     mang_ob5 = ((OB) l1);
     mang_ns1 = ((OB) ATTR(o11,sig1));
     ret_val38 = MANGLE119219986(PRINT_OB_mangler, mang_ob5, mang_ns1);
     plus_s37 = ((dSTR) ret_val38);
     L203 = (plus_s37==((dSTR) NULL));
     L2041_=!(L203); 
     if (L2041_) {
      L205 = ATTR(plus_self37,ntext);
      L206 = plus_s37;
      SATTR(plus_self37,ntext,FSTR_p1752847026(L205, (*dSTR_strrSTR[TAG(L206)])(L206)));
     }
     ret_val39 = plus_self37;
     plus_self38 = ret_val39;
     plus_s38 = ((dSTR) ((STR) &name146));
     L207 = ATTR(plus_self38,ntext);
     L208 = plus_s38;
     SATTR(plus_self38,ntext,FSTR_p1752847026(L207, (*dSTR_strrSTR[TAG(L208)])(L208)));
     plus_self39 = p;
     tag_for_self1 = cgen;
     tp_self2 = l1;
     ret_val40 = ATTR(tp_self2,tp_at);
     tag_for_tp1 = ret_val40;
     SATTR(tag_for_self1,needs_tag,FSETdT1016814448(ATTR(tag_for_self1,needs_tag), tag_for_tp1));
     mang_self6 = tag_for_self1;
     mang_ob6 = ((OB) tag_for_tp1);
     ret_val42 = MANGLE119219986(ATTR(mang_self6,mangler), mang_ob6, ((OB) NULL));
     res1 = STR_ap2004550586(ret_val42, ((STR) &S_tag));
     forbid_self2 = tag_for_self1;
     forbid_s2 = res1;
     forbid_self3 = ATTR(forbid_self2,mangler);
     forbid_s3 = forbid_s2;
     SATTR(forbid_self3,forbidden,FSETST1404644833(ATTR(forbid_self3,forbidden), forbid_s3));
     ret_val41 = res1;
     plus_s39 = ((dSTR) ret_val41);
     L209 = (plus_s39==((dSTR) NULL));
     L2101_=!(L209); 
     if (L2101_) {
      L211 = ATTR(plus_self39,ntext);
      L212 = plus_s39;
      SATTR(plus_self39,ntext,FSTR_p1752847026(L211, (*dSTR_strrSTR[TAG(L212)])(L212)));
     }
     ret_val43 = plus_self39;
     plus_self40 = ret_val43;
     plus_s40 = ((dSTR) ((STR) &S0));
     L213 = (plus_s40==((dSTR) NULL));
     L2141_=!(L213); 
     if (L2141_) {
      L215 = ATTR(plus_self40,ntext);
      L216 = plus_s40;
      SATTR(plus_self40,ntext,FSTR_p1752847026(L215, (*dSTR_strrSTR[TAG(L216)])(L216)));
     }
     ret_val44 = plus_self40;
     plus_self41 = ret_val44;
     L218=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
     if (L218==NULL) FATAL("Unable to allocate more memory");
     memset(L218,0,sizeof(struct INT_boxed_struct));
     ((OB)L218)->header.tag=INT_tag;
     L217 = (dSTR)((INT_boxed) L218);
     ((INT_boxed) L217)->immutable_part = PRINT_1524431106(self, cgen, ATTR(l1,source1));
     plus_s41 = L217;
     L219 = (plus_s41==((dSTR) NULL));
     L2201_=!(L219); 
     if (L2201_) {
      L222 = ATTR(plus_self41,ntext);
      L223 = plus_s41;
      SATTR(plus_self41,ntext,FSTR_p1752847026(L222, (*dSTR_strrSTR[TAG(L223)])(L223)));
     }
     ret_val45 = plus_self41;
     plus_self42 = ret_val45;
     plus_s42 = ((dSTR) ((STR) &name139));
     L224 = ATTR(plus_self42,ntext);
     L225 = plus_s42;
     SATTR(plus_self42,ntext,FSTR_p1752847026(L224, (*dSTR_strrSTR[TAG(L225)])(L225)));
    }
   }
  }
  after_loop2: ;
  plus_self43 = p;
  plus_s43 = ((dSTR) ((STR) &name147));
  L226 = ATTR(plus_self43,ntext);
  L227 = plus_s43;
  SATTR(plus_self43,ntext,FSTR_p1752847026(L226, (*dSTR_strrSTR[TAG(L227)])(L227)));
 }
 plus_self44 = p;
 plus_s44 = ((dSTR) ((STR) &name148));
 L228 = ATTR(plus_self44,ntext);
 L229 = plus_s44;
 SATTR(plus_self44,ntext,FSTR_p1752847026(L228, (*dSTR_strrSTR[TAG(L229)])(L229)));
 L230 = PRINT_1541629256;
 L2311_=(L230)<(n); 
 if (L2311_) {
  PRINT_1541629256 = n;
 }
}


#undef IS_ITER
#define IS_ITER 0

void PRINT_791786249(PRINT_OB self, PROG prog, CGEN cgen, INT sm, INT lg) {
 CODE_FILE c;
 FSTR decl;
 FSTR p;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT tg = INT_zero;
 dTP cl = ((dTP) NULL);
 FMAPdTPINT L31;
 TUPdTPINT pr = TUPdTPINT_zero;
 FSTR fname;
 STR plus_self;
 INT plus_arg = INT_zero;
 STR ret_val;
 STR s;
 INT str_self = INT_zero;
 STR ret_val1;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val2;
 FSTR str_self1;
 STR ret_val3;
 STR plus_self1;
 STR plus_sarg;
 STR ret_val4;
 PROG barf_self;
 STR barf_msg;
 PROG barf_at_self;
 STR barf_at_msg;
 dPROG_ERR barf_at_at;
 STR plus_self2;
 STR plus_sarg1;
 STR ret_val5;
 UNIX exit_self;
 INT exit_code = INT_zero;
 PRINT_OB mkfname_self;
 dTP mkfname_cl;
 FSTR ret_val6;
 PRINT_OB mang_self;
 OB mang_ob;
 STR ret_val7;
 STR fstr_self;
 FSTR ret_val8;
 FSTR plus_self3;
 INT plus_i = INT_zero;
 FSTR ret_val9;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val10;
 CODE_FILE plus_self4;
 dSTR plus_s;
 CODE_FILE ret_val11;
 CODE_FILE plus_self5;
 dSTR plus_s1;
 CODE_FILE ret_val12;
 CODE_FILE plus_self6;
 dSTR plus_s2;
 extern STR sather_typec;
 extern STR static1525151245;
 extern STR define2130232346;
 INT L41_,L41_m;
 INT L5;
 BOOL L61_;
 extern STR NULLno1802593478;
 TUPdTPINT L7;
 INT L8;
 BOOL L91_;
 extern STR theres458619408;
 BOOL L10;
 BOOL L121_;
 extern STR name9;
 extern STR Intern28965746;
 extern STR ESS;
 FSTR L13;
 FSTR L14;
 extern STR S_type_table;
 extern STR name175;
 FSTR L15;
 FSTR L16;
 extern STR S_type_table1;
 extern STR NULL8;
 extern STR struct254901963;
 INT L171_;
 extern STR name13;
 BOOL L18;
 BOOL L191_;
 FSTR L20;
 dSTR L22;
 extern STR name9;
 BOOL L23;
 BOOL L241_;
 FSTR L25;
 dSTR L26;
 FSTR L27;
 dSTR L28;
 c = CODE_F1432464034(((CODE_FILE) NULL), ((STR) &sather_typec));
 SATTR(c,is_c_code,TRUE);
 SATTR(c,do_not_merge,TRUE);
 decl = FSTR_c1998740379(((FSTR) NULL), 1000);
 p = FSTR_c1998740379(((FSTR) NULL), 10000);
 p = FSTR_p1752847026(p, ((STR) &static1525151245));
 decl = FSTR_p1752847026(decl, ((STR) &define2130232346));
 {
  BOOL f_L41_ = TRUE;
  L11 = sm;
  L21 = lg;
  L41_=L11-1;L41_m=L21; 
  while (1) {
   if(L41_++>=L41_m)  goto after_loop; 
   tg = L41_;
   L61_=(tg)==(0); 
   if (L61_) {
    p = FSTR_p1752847026(p, ((STR) &NULLno1802593478));
   }
   else {
    {
     struct FMAPdT1857608820_frame_struct other2_0;
FMAPdT1857608820_frame noname1 = &other2_0;
     L31 = ATTR(cgen,tags);
     noname1->self = L31;
     noname1->state = 0;
     while (1) {
      L7 = FMAPdT1857608820(noname1);
      if (noname1->state == -1) {
       goto after_loop1;
      }
      pr = L7;
      L8 = pr.t2;
      L91_=(L8)==(tg); 
      if (L91_) {
       cl = pr.t1;
       goto after_loop1;
      }
     }
    }
    after_loop1: ;
    if ((cl==((dTP) NULL))) {
     barf_self = prog;
     plus_self = ((STR) &theres458619408);
     plus_arg = tg;
     str_self = plus_arg;
     clear_self = buf1;
     L10 = (clear_self==((FSTR) NULL));
     L121_=!(L10); 
     if (L121_) {
      SATTR(clear_self,loc,0);
     }
     str_in_self = str_self;
     str_in_s = buf1;
     ret_val2 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
     buf1 = ret_val2;
     str_self1 = buf1;
     ret_val3 = STR_fr1097270334(((STR) NULL), str_self1);
     ret_val1 = ret_val3;
     s = ret_val1;
     ret_val = STR_ap1077157958(plus_self, s, TRUE);
     plus_self1 = ret_val;
     plus_sarg = ((STR) &name9);
     ret_val4 = STR_ap2004550586(plus_self1, plus_sarg);
     barf_msg = ret_val4;
     barf_at_self = barf_self;
     barf_at_msg = barf_msg;
     barf_at_at = ((dPROG_ERR) NULL);
     PROG_e176405615(barf_at_self, barf_at_at);
     plus_self2 = ((STR) &Intern28965746);
     plus_sarg1 = barf_at_msg;
     ret_val5 = STR_ap2004550586(plus_self2, plus_sarg1);
     PROG_err_STR(barf_at_self, ret_val5);
     exit_self = ((UNIX) NULL);
     exit_code = 1;
     exit(exit_code);
    }
    mkfname_self = self;
    mkfname_cl = cl;
    mang_self = mkfname_self;
    mang_ob = ((OB) mkfname_cl);
    ret_val7 = MANGLE119219986(PRINT_OB_mangler, mang_ob, ((OB) NULL));
    fstr_self = ret_val7;
    ret_val8 = FSTR_c1307336863(((FSTR) NULL), fstr_self);
    ret_val6 = ret_val8;
    fname = ret_val6;
    L13 = FSTR_p1752847026(decl, ((STR) &ESS));
    L14 = FSTR_p1410513982(L13, fname);
    decl = FSTR_p1752847026(L14, ((STR) &S_type_table));
    L15 = FSTR_p1752847026(p, ((STR) &name175));
    L16 = FSTR_p1410513982(L15, fname);
    p = FSTR_p1752847026(L16, ((STR) &S_type_table1));
   }
  }
 }
 after_loop: ;
 p = FSTR_p1752847026(p, ((STR) &NULL8));
 plus_self3 = FSTR_p1752847026(p, ((STR) &struct254901963));
 L171_=INTMINUS(0,sm); 
 plus_i = L171_;
 str_in_self1 = plus_i;
 str_in_s1 = plus_self3;
 ret_val10 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
 ret_val9 = ret_val10;
 p = FSTR_p1752847026(ret_val9, ((STR) &name13));
 plus_self4 = c;
 plus_s = ((dSTR) decl);
 L18 = (plus_s==((dSTR) NULL));
 L191_=!(L18); 
 if (L191_) {
  L20 = ATTR(plus_self4,ntext);
  L22 = plus_s;
  SATTR(plus_self4,ntext,FSTR_p1752847026(L20, (*dSTR_strrSTR[TAG(L22)])(L22)));
 }
 ret_val11 = plus_self4;
 plus_self5 = ret_val11;
 plus_s1 = ((dSTR) ((STR) &name9));
 L23 = (plus_s1==((dSTR) NULL));
 L241_=!(L23); 
 if (L241_) {
  L25 = ATTR(plus_self5,ntext);
  L26 = plus_s1;
  SATTR(plus_self5,ntext,FSTR_p1752847026(L25, (*dSTR_strrSTR[TAG(L26)])(L26)));
 }
 ret_val12 = plus_self5;
 plus_self6 = ret_val12;
 plus_s2 = ((dSTR) p);
 L27 = ATTR(plus_self6,ntext);
 L28 = plus_s2;
 SATTR(plus_self6,ntext,FSTR_p1752847026(L27, (*dSTR_strrSTR[TAG(L28)])(L28)));
}


#undef IS_ITER
#define IS_ITER 0

void PROG_e176405615(PROG self, dPROG_ERR t) {
 dPROG_ERR L1;
 if ((t==((dPROG_ERR) NULL))) {
  SATTR(self,eloc,SFILE_ID_zero);
  return;
 }
 L1 = t;
 SATTR(self,eloc,(*dPROG_848621491[TAG(L1)])(L1));
}


#undef IS_ITER
#define IS_ITER 0

void PROG_err_STR(PROG self, STR s) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 dSTR plus_s;
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
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 OUT create_self2;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val7;
 FILE1 stdout_self3;
 FILE1 ret_val8;
 FILE1 r3;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val9;
 FILE1 r4;
 BOOL L1;
 BOOL L2;
 BOOL L31_;
 dSTR L4;
 OB L5;
 FILE1 L6;
 OB L7;
 BOOL L9;
 BOOL L101_;
 BOOL L11;
 BOOL L121_;
 FILE1 L13;
 OB L14;
 extern STR name20;
 FILE1 L16;
 OB L17;
 FILE1 L19;
 OB L20;
 extern STR name9;
 FILE1 L22;
 OB L23;
 if (ATTR(self,verbose)) {
  L2 = ATTR(self,err_seen);
  L31_=!(L2); 
  L1 = L31_;
 } else {
  L1 = FALSE;
 }
 if (L1) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  L5=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
  if (L5==NULL) FATAL("Unable to allocate more memory");
  memset(L5,0,sizeof(struct CHAR_boxed_struct));
  ((OB)L5)->header.tag=CHAR_tag;
  L4 = (dSTR)((CHAR_boxed) L5);
  ((CHAR_boxed) L4)->immutable_part = '\n';
  plus_s = L4;
  stdout_self = ((FILE1) NULL);
  L7=ZALLOC(sizeof(struct FILE1_struct));
  if (L7==NULL) FATAL("Unable to allocate more memory");
  ((OB)L7)->header.tag=FILE1_tag;
  L6 = ((FILE1) L7);
  r = L6;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_dSTR(ret_val1, plus_s);
 }
 SATTR(self,err_seen,TRUE);
 L9 = INT_IS_VOID(ATTR(self,eloc).loc);
 L101_=!(L9); 
 if (L101_) {
  if (PROG_e596344618(self, ATTR(self,eloc))) {
   return;
  }
 }
 L11 = INT_IS_VOID(ATTR(self,eloc).loc);
 L121_=!(L11); 
 if (L121_) {
  create_self1 = ((OUT) NULL);
  ret_val2 = create_self1;
  plus_self1 = ret_val2;
  plus_s1 = SFILE_ID_strrSTR(ATTR(self,eloc));
  stdout_self1 = ((FILE1) NULL);
  L14=ZALLOC(sizeof(struct FILE1_struct));
  if (L14==NULL) FATAL("Unable to allocate more memory");
  ((OB)L14)->header.tag=FILE1_tag;
  L13 = ((FILE1) L14);
  r1 = L13;
  
  SATTR(r1,fp,stdout);
  ret_val4 = r1;
  FILE_plus_STR(ret_val4, plus_s1);
  ret_val3 = plus_self1;
  plus_self2 = ret_val3;
  plus_s2 = ((STR) &name20);
  stdout_self2 = ((FILE1) NULL);
  L17=ZALLOC(sizeof(struct FILE1_struct));
  if (L17==NULL) FATAL("Unable to allocate more memory");
  ((OB)L17)->header.tag=FILE1_tag;
  L16 = ((FILE1) L17);
  r2 = L16;
  
  SATTR(r2,fp,stdout);
  ret_val5 = r2;
  FILE_plus_STR(ret_val5, plus_s2);
 }
 create_self2 = ((OUT) NULL);
 ret_val6 = create_self2;
 plus_self3 = ret_val6;
 plus_s3 = s;
 stdout_self3 = ((FILE1) NULL);
 L20=ZALLOC(sizeof(struct FILE1_struct));
 if (L20==NULL) FATAL("Unable to allocate more memory");
 ((OB)L20)->header.tag=FILE1_tag;
 L19 = ((FILE1) L20);
 r3 = L19;
 
 SATTR(r3,fp,stdout);
 ret_val8 = r3;
 FILE_plus_STR(ret_val8, plus_s3);
 ret_val7 = plus_self3;
 plus_self4 = ret_val7;
 plus_s4 = ((STR) &name9);
 stdout_self4 = ((FILE1) NULL);
 L23=ZALLOC(sizeof(struct FILE1_struct));
 if (L23==NULL) FATAL("Unable to allocate more memory");
 ((OB)L23)->header.tag=FILE1_tag;
 L22 = ((FILE1) L23);
 r4 = L22;
 
 SATTR(r4,fp,stdout);
 ret_val9 = r4;
 FILE_plus_STR(ret_val9, plus_s4);
}


#undef IS_ITER
#define IS_ITER 0

void PROG_message_STR(PROG self, STR s) {
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
 OUT create_self1;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val5;
 FILE1 stdout_self2;
 FILE1 ret_val6;
 FILE1 r2;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val7;
 FILE1 stdout_self3;
 FILE1 ret_val8;
 FILE1 r3;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val9;
 FILE1 r4;
 BOOL L1;
 BOOL L21_;
 FILE1 L3;
 OB L4;
 extern STR name20;
 FILE1 L6;
 OB L7;
 extern STR MESSAGE;
 FILE1 L9;
 OB L10;
 FILE1 L12;
 OB L13;
 extern STR name9;
 FILE1 L15;
 OB L16;
 L1 = INT_IS_VOID(ATTR(self,eloc).loc);
 L21_=!(L1); 
 if (L21_) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = SFILE_ID_strrSTR(ATTR(self,eloc));
  stdout_self = ((FILE1) NULL);
  L4=ZALLOC(sizeof(struct FILE1_struct));
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=FILE1_tag;
  L3 = ((FILE1) L4);
  r = L3;
  
  SATTR(r,fp,stdout);
  ret_val2 = r;
  FILE_plus_STR(ret_val2, plus_s);
  ret_val1 = plus_self;
  plus_self1 = ret_val1;
  plus_s1 = ((STR) &name20);
  stdout_self1 = ((FILE1) NULL);
  L7=ZALLOC(sizeof(struct FILE1_struct));
  if (L7==NULL) FATAL("Unable to allocate more memory");
  ((OB)L7)->header.tag=FILE1_tag;
  L6 = ((FILE1) L7);
  r1 = L6;
  
  SATTR(r1,fp,stdout);
  ret_val3 = r1;
  FILE_plus_STR(ret_val3, plus_s1);
 }
 create_self1 = ((OUT) NULL);
 ret_val4 = create_self1;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &MESSAGE);
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
 plus_s3 = s;
 stdout_self3 = ((FILE1) NULL);
 L13=ZALLOC(sizeof(struct FILE1_struct));
 if (L13==NULL) FATAL("Unable to allocate more memory");
 ((OB)L13)->header.tag=FILE1_tag;
 L12 = ((FILE1) L13);
 r3 = L12;
 
 SATTR(r3,fp,stdout);
 ret_val8 = r3;
 FILE_plus_STR(ret_val8, plus_s3);
 ret_val7 = plus_self3;
 plus_self4 = ret_val7;
 plus_s4 = ((STR) &name9);
 stdout_self4 = ((FILE1) NULL);
 L16=ZALLOC(sizeof(struct FILE1_struct));
 if (L16==NULL) FATAL("Unable to allocate more memory");
 ((OB)L16)->header.tag=FILE1_tag;
 L15 = ((FILE1) L16);
 r4 = L15;
 
 SATTR(r4,fp,stdout);
 ret_val9 = r4;
 FILE_plus_STR(ret_val9, plus_s4);
}


#undef IS_ITER
#define IS_ITER 0

void PROG_warning_STR(PROG self, STR s) {
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
 OUT create_self1;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val5;
 FILE1 stdout_self2;
 FILE1 ret_val6;
 FILE1 r2;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val7;
 FILE1 stdout_self3;
 FILE1 ret_val8;
 FILE1 r3;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val9;
 FILE1 r4;
 BOOL L1;
 BOOL L21_;
 FILE1 L3;
 OB L4;
 extern STR name20;
 FILE1 L6;
 OB L7;
 extern STR WARNING1;
 FILE1 L9;
 OB L10;
 FILE1 L12;
 OB L13;
 extern STR name9;
 FILE1 L15;
 OB L16;
 L1 = INT_IS_VOID(ATTR(self,eloc).loc);
 L21_=!(L1); 
 if (L21_) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = SFILE_ID_strrSTR(ATTR(self,eloc));
  stdout_self = ((FILE1) NULL);
  L4=ZALLOC(sizeof(struct FILE1_struct));
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=FILE1_tag;
  L3 = ((FILE1) L4);
  r = L3;
  
  SATTR(r,fp,stdout);
  ret_val2 = r;
  FILE_plus_STR(ret_val2, plus_s);
  ret_val1 = plus_self;
  plus_self1 = ret_val1;
  plus_s1 = ((STR) &name20);
  stdout_self1 = ((FILE1) NULL);
  L7=ZALLOC(sizeof(struct FILE1_struct));
  if (L7==NULL) FATAL("Unable to allocate more memory");
  ((OB)L7)->header.tag=FILE1_tag;
  L6 = ((FILE1) L7);
  r1 = L6;
  
  SATTR(r1,fp,stdout);
  ret_val3 = r1;
  FILE_plus_STR(ret_val3, plus_s1);
 }
 create_self1 = ((OUT) NULL);
 ret_val4 = create_self1;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &WARNING1);
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
 plus_s3 = s;
 stdout_self3 = ((FILE1) NULL);
 L13=ZALLOC(sizeof(struct FILE1_struct));
 if (L13==NULL) FATAL("Unable to allocate more memory");
 ((OB)L13)->header.tag=FILE1_tag;
 L12 = ((FILE1) L13);
 r3 = L12;
 
 SATTR(r3,fp,stdout);
 ret_val8 = r3;
 FILE_plus_STR(ret_val8, plus_s3);
 ret_val7 = plus_self3;
 plus_self4 = ret_val7;
 plus_s4 = ((STR) &name9);
 stdout_self4 = ((FILE1) NULL);
 L16=ZALLOC(sizeof(struct FILE1_struct));
 if (L16==NULL) FATAL("Unable to allocate more memory");
 ((OB)L16)->header.tag=FILE1_tag;
 L15 = ((FILE1) L16);
 r4 = L15;
 
 SATTR(r4,fp,stdout);
 ret_val9 = r4;
 FILE_plus_STR(ret_val9, plus_s4);
}
