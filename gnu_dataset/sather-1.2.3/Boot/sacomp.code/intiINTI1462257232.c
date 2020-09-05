#include "sather.h"

/* Layouts */

typedef struct dLAYOUT_struct {
 OB_HEADER header;
 } *dLAYOUT;

typedef struct dMODE_struct {
 OB_HEADER header;
 } *dMODE;

typedef struct dTP_struct {
 OB_HEADER header;
 } *dTP;

typedef struct ABSTRA151498155_struct {/* layout for ABSTRACT_FRAME_LAYOUT */
 OB_HEADER header;
 struct PROG_struct *prog;
 struct SIG_struct *sig1;
 STR str;
 } *ABSTRA151498155;

typedef struct ABSTRACT_LAYOUT_struct {/* layout for ABSTRACT_LAYOUT */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct FLISTdLAYOUT_struct *dependencies;
 struct PROG_struct *prog;
 STR str;
 } *ABSTRACT_LAYOUT;

typedef struct ARG_LAYOUT_struct {/* layout for ARG_LAYOUT */
 OB_HEADER header;
 struct SIG_struct *sig1;
 STR str;
 } *ARG_LAYOUT;

typedef struct ARRAYARG_struct {/* layout for ARRAY{ARG} */
 OB_HEADER header;
 INT asize;
 struct ARG_struct *arr_part[1];
 } *ARRAYARG;

typedef struct BOUND_1007407331_struct {/* layout for BOUND_ITER_FRAME_LAYOUT */
 OB_HEADER header;
 struct AM_BND1124877163_struct *e;
 struct PROG_struct *prog;
 STR str;
 } *BOUND_1007407331;

typedef struct BOUND_2140373124_struct {/* layout for BOUND_ITER_TYPE_LAYOUT */
 OB_HEADER header;
 STR str;
 struct TP_ITER_struct *tp;
 } *BOUND_2140373124;

typedef struct BOUND_1677815802_struct {/* layout for BOUND_OBJECT_LAYOUT */
 OB_HEADER header;
 struct AM_BND1124877163_struct *e;
 STR str;
 } *BOUND_1677815802;

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

typedef struct ELT_NILSIG_struct {/* layout for ELT_NIL{SIG} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILSIG;

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

typedef struct IFC_struct {/* layout for IFC */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct SIG_TBL_struct *sigs;
 } *IFC;

typedef struct INOUT_MODE_struct {/* layout for INOUT_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *INOUT_MODE;

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

typedef struct SIG_TBL_struct {/* layout for SIG_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct SIG_struct *arr_part[1];
 } *SIG_TBL;

typedef struct SIG_TBL_eltbrSIG_frame_struct {
 INT state;
 SIG_TBL self;/* Formal argument: self */
 SIG ret_val2;
 SIG_TBL L61;
 SIG r;
 INT L31;
 SIG_TBL is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val;
 ELT_NILSIG is_elt_nil_self1;
 SIG is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *SIG_TBL_eltbrSIG_frame;

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

typedef struct TUPdTPdLAYOUT_struct {/* layout for TUP{$TP,$LAYOUT} */
 struct dLAYOUT_struct *t2;
 struct dTP_struct *t1;
 } TUPdTPdLAYOUT;
static TUPdTPdLAYOUT TUPdTPdLAYOUT_zero;

typedef struct TUPdTPdLAYOUT_boxed_struct {
 OB_HEADER header;
 TUPdTPdLAYOUT immutable_part;
 } *TUPdTPdLAYOUT_boxed;

typedef struct TUPAM_304458649_struct {/* layout for TUP{AM_BND_CREATE_EXPR,BOUND_ITER_FRAME_LAYOUT} */
 struct AM_BND1124877163_struct *t1;
 struct BOUND_1007407331_struct *t2;
 } TUPAM_304458649;
static TUPAM_304458649 TUPAM_304458649_zero;

typedef struct TUPAM_304458649_boxed_struct {
 OB_HEADER header;
 TUPAM_304458649 immutable_part;
 } *TUPAM_304458649_boxed;

typedef struct TUPSIG1754078736_struct {/* layout for TUP{SIG,ABSTRACT_FRAME_LAYOUT} */
 struct ABSTRA151498155_struct *t2;
 struct SIG_struct *t1;
 } TUPSIG1754078736;
static TUPSIG1754078736 TUPSIG1754078736_zero;

typedef struct TUPSIG1754078736_boxed_struct {
 OB_HEADER header;
 TUPSIG1754078736 immutable_part;
 } *TUPSIG1754078736_boxed;

typedef struct TUPSIG2023533247_struct {/* layout for TUP{SIG,FRAME_LAYOUT} */
 struct FRAME_LAYOUT_struct *t2;
 struct SIG_struct *t1;
 } TUPSIG2023533247;
static TUPSIG2023533247 TUPSIG2023533247_zero;

typedef struct TUPSIG2023533247_boxed_struct {
 OB_HEADER header;
 TUPSIG2023533247 immutable_part;
 } *TUPSIG2023533247_boxed;

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

typedef struct ARRAYA1077419174_frame_struct {
 INT state;
 ARRAYARG self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA1077419174_frame;

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

typedef struct ARRAYI180735351_frame_struct {
 INT state;
 ARRAYINT self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYI180735351_frame;

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

typedef struct FLISTINT_struct {/* layout for FLIST{INT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 INT arr_part[1];
 } *FLISTINT;

typedef struct FMAPdTPdLAYOUT_struct {/* layout for FMAP{$TP,$LAYOUT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdTPdLAYOUT_struct arr_part[1];
 } *FMAPdTPdLAYOUT;

typedef struct FMAPAM339652544_struct {/* layout for FMAP{AM_BND_CREATE_EXPR,BOUND_ITER_FRAME_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPAM_304458649_struct arr_part[1];
 } *FMAPAM339652544;

typedef struct FMAPSI2103621588_struct {/* layout for FMAP{SIG,ABSTRACT_FRAME_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSIG1754078736_struct arr_part[1];
 } *FMAPSI2103621588;

typedef struct FMAPSI1121373188_struct {/* layout for FMAP{SIG,FRAME_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSIG2023533247_struct arr_part[1];
 } *FMAPSI1121373188;

typedef struct FSETAB1569698808_struct {/* layout for FSET{ABSTRACT_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct ABSTRACT_LAYOUT_struct *arr_part[1];
 } *FSETAB1569698808;

typedef struct FSETBO1548397318_struct {/* layout for FSET{BOUND_ITER_TYPE_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct BOUND_2140373124_struct *arr_part[1];
 } *FSETBO1548397318;

typedef struct FSETBO1147931784_struct {/* layout for FSET{BOUND_TYPE_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct BOUND_809390774_struct *arr_part[1];
 } *FSETBO1147931784;

typedef struct FSETBU1446477505_struct {/* layout for FSET{BUILTIN_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct BUILTIN_LAYOUT_struct *arr_part[1];
 } *FSETBU1446477505;

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

extern FMAPAM339652544 BOUND_982289559;
extern FMAPSI1121373188 FRAME_1643038396;
extern FMAPSI2103621588 ABSTRA854284101;
extern FMAPdTPdLAYOUT LAYOUT1191461457;
extern FSETAB1569698808 ABSTRA8148400;
extern FSETBO1147931784 BOUND_2075179152;
extern FSETBO1548397318 BOUND_210358778;
extern FSETBU1446477505 BUILTI898649695;
extern FSTR INTI_buf;
extern INOUT_MODE MODES_inout_mode;
extern INT INTI_B;
extern INT INTI_D;
extern INT INTI_log10D;
extern INT INTI_log2B;
extern INT INT_asize;
extern OUT_MODE MODES_out_mode;

/* Function declarations */


extern RETURNED_CONST BOOL (**dMODE_814247358)(dMODE, dMODE);

extern RETURNED_CONST BOOL (**dTP_is124163553)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1811059018)(dTP);

extern RETURNED_CONST IFC (**dTP_ifcrIFC)(dTP);

extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);
ABSTRA151498155 ABSTRA1493033801(ABSTRA151498155, SIG, PROG);
ABSTRA151498155 FMAPSI146872941(FMAPSI2103621588, SIG);
ABSTRACT_LAYOUT ABSTRA762331980(ABSTRACT_LAYOUT, dTP, PROG);
ARG ARRAYA827580689(ARRAYA827580689_frame);
BOOL ARRAYB1390856792(ARRAYB1390856792_frame);
BOOL INTI_i1060921131(INTI1, OB);
BOUND_1007407331 BOUND_11324444(BOUND_1007407331, AM_BND1124877163, PROG);
BOUND_1007407331 FMAPAM1635877202(FMAPAM339652544, AM_BND1124877163);
BOUND_2140373124 BOUND_166620816(BOUND_2140373124, dTP);
BOUND_809390774 BOUND_848335287(BOUND_809390774, dTP);
BUILTIN_LAYOUT BUILTI1036809386(BUILTIN_LAYOUT, dTP);
FLISTINT FLISTI1369678776(FLISTINT, INT);
FLISTdLAYOUT ABSTRA1868518641(ABSTRACT_LAYOUT);
FLISTdLAYOUT ABSTRA1904387652(ABSTRA151498155);
FLISTdLAYOUT ARG_LA836569915(ARG_LAYOUT);
FLISTdLAYOUT BOUND_1150883246(BOUND_1007407331);
FLISTdLAYOUT BOUND_1157561903(BOUND_2140373124);
FLISTdLAYOUT BOUND_1585387107(BOUND_1677815802);
FLISTdLAYOUT BOUND_220914967(BOUND_809390774);
FLISTdLAYOUT BUILTI1794609038(BUILTIN_LAYOUT);
FLISTdLAYOUT FLISTd1429133189(FLISTdLAYOUT, dLAYOUT);
FMAPAM339652544 FMAPAM136290044(FMAPAM339652544, AM_BND1124877163, BOUND_1007407331);
FMAPSI2103621588 FMAPSI1221142349(FMAPSI2103621588, SIG, ABSTRA151498155);
FMAPdTPdLAYOUT FMAPdT1248247834(FMAPdTPdLAYOUT, dTP, dLAYOUT);
FRAME_LAYOUT FMAPSI547426166(FMAPSI1121373188, SIG);
FSETAB1569698808 FSETAB1823289229(FSETAB1569698808, ABSTRACT_LAYOUT);
FSETBO1147931784 FSETBO113908103(FSETBO1147931784, BOUND_809390774);
FSETBO1548397318 FSETBO2112177303(FSETBO1548397318, BOUND_2140373124);
FSETBU1446477505 FSETBU2034709890(FSETBU1446477505, BUILTIN_LAYOUT);
FSETSTR FSETST1404644833(FSETSTR, STR);
FSTR FSTR_p399773021(FSTR, CHAR);
FSTR INTI_s96356965(INTI1, FSTR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
IDENT IDENT_1150413730(IDENT, STR);
INT ARRAYA1077419174(ARRAYA1077419174_frame);
INT ARRAYI180735351(ARRAYI180735351_frame);
INT ARRAYI69049459(ARRAYI69049459_frame);
INT FLISTI1321626366(FLISTINT);
INT FLISTINT_poprINT(FLISTINT);
INT INTI_c675035669(INTI1, INTI1);
INT INTI_hashrINT(INTI1);
INT INTI_intrINT(INTI1);
INT INTI_u1057260348(INTI1, INTI1, INTI1);
INT INTI_u1380826102(INTI1, INTI1, INT);
INT INT_hashrINT(INT);
INT INT_hi740923800(INT);
INT INT_timesbrINT(INT_timesbrINT_frame);
INT INT_upbrINT(INT_upbrINT_frame);
INT STR_sizerINT(STR);
INTI1 INTI_absrINTI(INTI1);
INTI1 INTI_c1588968505(INTI1, INT);
INTI1 INTI_copyrINTI(INTI1);
INTI1 INTI_d359767808(INTI1, INTI1);
INTI1 INTI_g1390757385(INTI1, INTI1, INTI1, INTI1);
INTI1 INTI_g1917565373(INTI1, INTI1);
INTI1 INTI_g960784342(INTI1, INTI1, INTI1, INTI1);
INTI1 INTI_m436971372(INTI1, INTI1);
INTI1 INTI_m582155739(INTI1, INTI1);
INTI1 INTI_negaterINTI(INTI1);
INTI1 INTI_p1518979099(INTI1, INT);
INTI1 INTI_p1769163857(INTI1, INTI1);
INTI1 INTI_t1692605406(INTI1, INTI1);
INTI1 INTI_u1193628812(INTI1, INTI1, INTI1);
INTI1 INTI_u1281648686(INTI1, INTI1, INTI1);
INTI1 INTI_u1836437467(INTI1, INTI1, INTI1);
INTI1 INTI_u2116735672(INTI1, INTI1, INTI1);
INTI1 INTI_u450560926(INTI1, INTI1, INT, INT);
SIG SIG_TBL_eltbrSIG(SIG_TBL_eltbrSIG_frame);
STR ABSTRA12614027(ABSTRA151498155, CGEN);
STR ABSTRA1464296195(ABSTRACT_LAYOUT);
STR ABSTRA1984535400(ABSTRA151498155);
STR ABSTRA353314648(ABSTRACT_LAYOUT, CGEN);
STR ABSTRA607493838(ABSTRA151498155, ARG, STR);
STR ARG_LA635972259(ARG_LAYOUT);
STR ARG_LA807737867(ARG_LAYOUT, CGEN);
STR ARG_strrSTR(ARG);
STR BOUND_1019937534(BOUND_1007407331, ARG, STR);
STR BOUND_1245789017(BOUND_2140373124);
STR BOUND_138602513(BOUND_1677815802, ARG, STR);
STR BOUND_1401874693(BOUND_1007407331, CGEN);
STR BOUND_1537967790(BOUND_809390774, CGEN);
STR BOUND_1707602440(BOUND_1677815802, CGEN);
STR BOUND_1856529942(BOUND_1007407331);
STR BOUND_1967670005(BOUND_809390774, ARG, STR);
STR BOUND_365396347(BOUND_2140373124, CGEN);
STR BOUND_457865042(BOUND_2140373124, ARG, STR);
STR BOUND_619031357(BOUND_809390774);
STR BOUND_787102169(BOUND_1677815802);
STR BUILTI1940169806(BUILTIN_LAYOUT);
STR BUILTI35892888(BUILTIN_LAYOUT, CGEN);
STR INTI_strrSTR(INTI1);
STR MANGLE119219986(MANGLE, OB, OB);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap1417971546(STR, STR, STR, STR);
STR STR_ap2004550586(STR, STR);
STR STR_fr1097270334(STR, FSTR);
STR TP_ITER_strrSTR(TP_ITER);
STR TP_ROUT_strrSTR(TP_ROUT);
dLAYOUT FMAPdT7482853(FMAPdTPdLAYOUT, dTP);
dLAYOUT LAYOUT79932797(LAYOUT_TBL, dTP);
void ABSTRA1640219182(ABSTRACT_LAYOUT);
void FILE_plus_STR(FILE1, STR);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

ABSTRA151498155 ABSTRA1493033801(ABSTRA151498155 self, SIG sig1, PROG p) {
 ABSTRA151498155 ret_val;
 ABSTRA151498155 res;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 ABSTRA151498155 L1;
 OB L2;
 extern STR abstractframe;
 res = FMAPSI146872941(ABSTRA854284101, sig1);
 if ((res==((ABSTRA151498155) NULL))) {
  L2=ZALLOC(sizeof(struct ABSTRA151498155_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=ABSTRA151498155_tag;
  L1 = ((ABSTRA151498155) L2);
  res = L1;
  SATTR(res,sig1,sig1);
  SATTR(res,prog,p);
  plus_self = ATTR(ATTR(res,sig1),str);
  plus_sarg = ((STR) &abstractframe);
  ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
  SATTR(res,str,ret_val1);
  ABSTRA854284101 = FMAPSI1221142349(ABSTRA854284101, sig1, res);
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

ABSTRACT_LAYOUT ABSTRA762331980(ABSTRACT_LAYOUT self, dTP tp, PROG p) {
 ABSTRACT_LAYOUT ret_val;
 dLAYOUT r;
 ABSTRACT_LAYOUT res = ((ABSTRACT_LAYOUT) NULL);
 ABSTRACT_LAYOUT makestr_self;
 STR ret_val1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 BOOL L1;
 BOOL L21_;
 ABSTRACT_LAYOUT L3;
 OB L4;
 dTP L5;
 extern STR Abstract;
 r = FMAPdT7482853(LAYOUT1191461457, tp);
 L1 = (r==((dLAYOUT) NULL));
 L21_=!(L1); 
 if (L21_) {
  switch (TAG(r)) {
   case ABSTRACT_LAYOUT_tag:
    res = ((ABSTRACT_LAYOUT) r); break;
   default: ;
    FATAL("No applicable type in typecase\nin ABSTRACT_LAYOUT::create($TP,PROG):SAME\n./Back/layout.sa:238:17");
  }
 }
 else {
  L4=ZALLOC(sizeof(struct ABSTRACT_LAYOUT_struct));
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=ABSTRACT_LAYOUT_tag;
  L3 = ((ABSTRACT_LAYOUT) L4);
  res = L3;
  SATTR(res,tp,tp);
  SATTR(res,prog,p);
  makestr_self = res;
  L5 = ATTR(makestr_self,tp);
  plus_self = (*dTP_strrSTR[TAG(L5)])(L5);
  plus_sarg = ((STR) &Abstract);
  ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
  ret_val1 = ret_val2;
  SATTR(res,str,ret_val1);
  ABSTRA8148400 = FSETAB1823289229(ABSTRA8148400, res);
  LAYOUT1191461457 = FMAPdT1248247834(LAYOUT1191461457, tp, ((dLAYOUT) res));
  ABSTRA1640219182(res);
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL INTI_i1060921131(INTI1 self, OB arg) {
 BOOL ret_val = BOOL_zero;
 INTI1 is_eq_self;
 INTI1 is_eq_y;
 BOOL ret_val1 = BOOL_zero;
 BOOL L1;
 INT L2;
 BOOL L31_;
 if (arg==NULL) {
  goto other169;
 } else
 switch (TAG(arg)) {
  case INTI1_tag:
   is_eq_self = self;
   is_eq_y = ((INTI1) arg);
   if ((is_eq_self==is_eq_y)) {
    L1 = TRUE;
   } else {
    L2 = INTI_c675035669(is_eq_self, is_eq_y);
    L31_=(L2)==(0); 
    L1 = L31_;
   }
   ret_val1 = L1;
   ret_val = ret_val1;
   return ret_val; break;
  default: ;
  other169: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOUND_1007407331 BOUND_11324444(BOUND_1007407331 self, AM_BND1124877163 e, PROG p) {
 BOUND_1007407331 ret_val;
 BOUND_1007407331 res;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 BOUND_1007407331 L1;
 OB L2;
 extern STR bounditerobject;
 res = FMAPAM1635877202(BOUND_982289559, e);
 if ((res==((BOUND_1007407331) NULL))) {
  L2=ZALLOC(sizeof(struct BOUND_1007407331_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=BOUND_1007407331_tag;
  L1 = ((BOUND_1007407331) L2);
  res = L1;
  SATTR(res,e,e);
  SATTR(res,prog,p);
  plus_self = ATTR(ATTR(ATTR(res,e),fun),str);
  plus_sarg = ((STR) &bounditerobject);
  ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
  SATTR(res,str,ret_val1);
  BOUND_982289559 = FMAPAM136290044(BOUND_982289559, e, res);
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOUND_2140373124 BOUND_166620816(BOUND_2140373124 self, dTP arg_tp) {
 BOUND_2140373124 ret_val;
 BOUND_2140373124 res = ((BOUND_2140373124) NULL);
 dLAYOUT ares;
 LAYOUT_TBL add_self;
 dTP add_t;
 dLAYOUT add_l;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 BOOL L1;
 BOOL L21_;
 BOUND_2140373124 L3;
 OB L4;
 extern STR bounditertype;
 ares = FMAPdT7482853(LAYOUT1191461457, arg_tp);
 L1 = (ares==((dLAYOUT) NULL));
 L21_=!(L1); 
 if (L21_) {
  switch (TAG(ares)) {
   case BOUND_2140373124_tag:
    res = ((BOUND_2140373124) ares); break;
   default: ;
    FATAL("No applicable type in typecase\nin BOUND_ITER_TYPE_LAYOUT::create($TP):BOUND_ITER_TYPE_LAYOUT\n./Back/layout.sa:786:17");
  }
 }
 else {
  L4=ZALLOC(sizeof(struct BOUND_2140373124_struct));
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=BOUND_2140373124_tag;
  L3 = ((BOUND_2140373124) L4);
  res = L3;
  switch (TAG(arg_tp)) {
   case TP_ITER_tag:
    SATTR(res,tp,((TP_ITER) arg_tp)); break;
   default: ;
    FATAL("No applicable type in typecase\nin BOUND_ITER_TYPE_LAYOUT::create($TP):BOUND_ITER_TYPE_LAYOUT\n./Back/layout.sa:790:17");
  }
  add_self = ((LAYOUT_TBL) NULL);
  add_t = arg_tp;
  add_l = ((dLAYOUT) res);
  LAYOUT1191461457 = FMAPdT1248247834(LAYOUT1191461457, add_t, add_l);
  BOUND_210358778 = FSETBO2112177303(BOUND_210358778, res);
  plus_self = TP_ITER_strrSTR(ATTR(res,tp));
  plus_sarg = ((STR) &bounditertype);
  ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
  SATTR(res,str,ret_val1);
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOUND_809390774 BOUND_848335287(BOUND_809390774 self, dTP arg_tp) {
 BOUND_809390774 ret_val;
 BOUND_809390774 res = ((BOUND_809390774) NULL);
 dLAYOUT ares;
 LAYOUT_TBL add_self;
 dTP add_t;
 dLAYOUT add_l;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 BOOL L1;
 BOOL L21_;
 BOUND_809390774 L3;
 OB L4;
 extern STR boundtype;
 ares = FMAPdT7482853(LAYOUT1191461457, arg_tp);
 L1 = (ares==((dLAYOUT) NULL));
 L21_=!(L1); 
 if (L21_) {
  switch (TAG(ares)) {
   case BOUND_809390774_tag:
    res = ((BOUND_809390774) ares); break;
   default: ;
    FATAL("No applicable type in typecase\nin BOUND_TYPE_LAYOUT::create($TP):BOUND_TYPE_LAYOUT\n./Back/layout.sa:987:17");
  }
 }
 else {
  L4=ZALLOC(sizeof(struct BOUND_809390774_struct));
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=BOUND_809390774_tag;
  L3 = ((BOUND_809390774) L4);
  res = L3;
  switch (TAG(arg_tp)) {
   case TP_ROUT_tag:
    SATTR(res,tp,((TP_ROUT) arg_tp)); break;
   default: ;
    FATAL("No applicable type in typecase\nin BOUND_TYPE_LAYOUT::create($TP):BOUND_TYPE_LAYOUT\n./Back/layout.sa:991:17");
  }
  add_self = ((LAYOUT_TBL) NULL);
  add_t = arg_tp;
  add_l = ((dLAYOUT) res);
  LAYOUT1191461457 = FMAPdT1248247834(LAYOUT1191461457, add_t, add_l);
  BOUND_2075179152 = FSETBO113908103(BOUND_2075179152, res);
  plus_self = TP_ROUT_strrSTR(ATTR(res,tp));
  plus_sarg = ((STR) &boundtype);
  ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
  SATTR(res,str,ret_val1);
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BUILTIN_LAYOUT BUILTI1036809386(BUILTIN_LAYOUT self, dTP tp) {
 BUILTIN_LAYOUT ret_val;
 dLAYOUT r;
 BUILTIN_LAYOUT res = ((BUILTIN_LAYOUT) NULL);
 BUILTIN_LAYOUT makestr_self;
 STR ret_val1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 BOOL L1;
 BOOL L21_;
 BUILTIN_LAYOUT L3;
 OB L4;
 dTP L5;
 extern STR Builtin;
 r = FMAPdT7482853(LAYOUT1191461457, tp);
 L1 = (r==((dLAYOUT) NULL));
 L21_=!(L1); 
 if (L21_) {
  switch (TAG(r)) {
   case BUILTIN_LAYOUT_tag:
    res = ((BUILTIN_LAYOUT) r); break;
   default: ;
    FATAL("No applicable type in typecase\nin BUILTIN_LAYOUT::create($TP):SAME\n./Back/layout.sa:83:17");
  }
 }
 else {
  L4=ZALLOC(sizeof(struct BUILTIN_LAYOUT_struct));
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=BUILTIN_LAYOUT_tag;
  L3 = ((BUILTIN_LAYOUT) L4);
  res = L3;
  SATTR(res,tp,tp);
  makestr_self = res;
  L5 = ATTR(makestr_self,tp);
  plus_self = (*dTP_strrSTR[TAG(L5)])(L5);
  plus_sarg = ((STR) &Builtin);
  ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
  ret_val1 = ret_val2;
  SATTR(res,str,ret_val1);
  BUILTI898649695 = FSETBU2034709890(BUILTI898649695, res);
  LAYOUT1191461457 = FMAPdT1248247834(LAYOUT1191461457, tp, ((dLAYOUT) res));
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTdLAYOUT ABSTRA1868518641(ABSTRACT_LAYOUT self) {
 return ATTR(self,dependencies);
}


#undef IS_ITER
#define IS_ITER 0

FLISTdLAYOUT ABSTRA1904387652(ABSTRA151498155 self) {
 FLISTdLAYOUT ret_val;
 FLISTdLAYOUT res;
 ARRAYARG L11;
 FLISTdLAYOUT create_self;
 FLISTdLAYOUT ret_val1;
 INT L21 = INT_zero;
 INT L31_br;
 ARG aL31_;
 ARG L4;
 ARG L5;
 INT L61_;
 BOOL L7;
 BOOL L81_;
 create_self = ((FLISTdLAYOUT) NULL);
 ret_val1 = ((FLISTdLAYOUT) NULL);
 res = ret_val1;
 {
  BOOL f_L31_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = ATTR(ATTR(self,sig1),args);
  L31_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
  while (1) {
   if(L21>=L31_br)  goto after_loop; 
   aL31_=ARR((ARRAYARG)L11,L21); 
   L5=aL31_;
   res = FLISTd1429133189(res, LAYOUT79932797(((LAYOUT_TBL) NULL), ATTR(L5,tp)));
   L61_=INTPLUS(L21,1); 
   L21 = L61_;
  }
 }
 after_loop: ;
 L7 = (ATTR(ATTR(self,sig1),ret)==((dTP) NULL));
 L81_=!(L7); 
 if (L81_) {
  res = FLISTd1429133189(res, LAYOUT79932797(((LAYOUT_TBL) NULL), ATTR(ATTR(self,sig1),ret)));
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTdLAYOUT ARG_LA836569915(ARG_LAYOUT self) {
 FLISTdLAYOUT ret_val;
 FLISTdLAYOUT res;
 ARRAYARG L11;
 FLISTdLAYOUT create_self;
 FLISTdLAYOUT ret_val1;
 INT L21 = INT_zero;
 INT L31_br;
 ARG aL31_;
 ARG L4;
 ARG L5;
 INT L61_;
 create_self = ((FLISTdLAYOUT) NULL);
 ret_val1 = ((FLISTdLAYOUT) NULL);
 res = ret_val1;
 res = FLISTd1429133189(res, LAYOUT79932797(((LAYOUT_TBL) NULL), ATTR(ATTR(self,sig1),tp)));
 {
  BOOL f_L31_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = ATTR(ATTR(self,sig1),args);
  L31_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
  while (1) {
   if(L21>=L31_br)  goto after_loop; 
   aL31_=ARR((ARRAYARG)L11,L21); 
   L5=aL31_;
   res = FLISTd1429133189(res, LAYOUT79932797(((LAYOUT_TBL) NULL), ATTR(L5,tp)));
   L61_=INTPLUS(L21,1); 
   L21 = L61_;
  }
 }
 after_loop: ;
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTdLAYOUT BOUND_1150883246(BOUND_1007407331 self) {
 FLISTdLAYOUT ret_val;
 FLISTdLAYOUT res;
 dLAYOUT fl = ((dLAYOUT) NULL);
 IDENT ident1 = IDENT_zero;
 BOOL binding_iter_clo = BOOL_zero;
 ARRAYINT L11;
 INT i = INT_zero;
 ARRAYINT L21;
 INT i1 = INT_zero;
 FLISTdLAYOUT create_self;
 FLISTdLAYOUT ret_val1;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
 OUT create_self1;
 OUT ret_val3;
 OUT plus_self;
 STR plus_s;
 OUT ret_val4;
 FILE1 stdout_self;
 FILE1 ret_val5;
 FILE1 r;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val6;
 FILE1 stdout_self1;
 FILE1 ret_val7;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val8;
 FILE1 r2;
 INT L31 = INT_zero;
 INT L41 = INT_zero;
 extern STR callb;
 dTP L5;
 extern STR Errori170665162;
 FILE1 L6;
 OB L7;
 FILE1 L9;
 OB L10;
 extern STR name9;
 FILE1 L13;
 OB L14;
 BOOL L16;
 BOOL L171_;
 INT L181_br;
 INT aL181_;
 INT L19;
 BOOL L201_;
 ARRAYARG L22;
 INT L231_;
 INT L24;
 ARG L251_;
 ARG L26;
 INT L271_;
 INT L281_br;
 INT rL281_;
 INT L29;
 ARRAYINT L30;
 INT L321_;
 INT L33;
 BOOL L341_;
 ARRAYARG L35;
 ARRAYINT L36;
 INT L371_;
 INT L38;
 INT L391_;
 INT L40;
 ARG L421_;
 ARG L43;
 INT L441_;
 create_self = ((FLISTdLAYOUT) NULL);
 ret_val1 = ((FLISTdLAYOUT) NULL);
 res = ret_val1;
 ident1 = IDENT_1150413730(IDENT_zero, ((STR) &callb));
 binding_iter_clo = FALSE;
 L5 = ATTR(ATTR(ATTR(self,e),fun),tp);
 if ((*dTP_is1811059018[TAG(L5)])(L5)) {
  fl = ((dLAYOUT) FMAPSI146872941(ABSTRA854284101, ATTR(ATTR(self,e),fun)));
 }
 else {
  is_eq_self = ATTR(ATTR(ATTR(self,e),fun),name1);
  is_eq_i = ident1;
  ret_val2 = (is_eq_self.str==is_eq_i.str);
  if (ret_val2) {
   binding_iter_clo = TRUE;
  }
  else {
   fl = ((dLAYOUT) FMAPSI547426166(FRAME_1643038396, ATTR(ATTR(self,e),fun)));
  }
 }
 if (binding_iter_clo) {
 }
 else {
  if ((fl==((dLAYOUT) NULL))) {
   create_self1 = ((OUT) NULL);
   ret_val3 = create_self1;
   plus_self = ret_val3;
   plus_s = ((STR) &Errori170665162);
   stdout_self = ((FILE1) NULL);
   L7=ZALLOC(sizeof(struct FILE1_struct));
   if (L7==NULL) FATAL("Unable to allocate more memory");
   ((OB)L7)->header.tag=FILE1_tag;
   L6 = ((FILE1) L7);
   r = L6;
   
   SATTR(r,fp,stdout);
   ret_val5 = r;
   FILE_plus_STR(ret_val5, plus_s);
   ret_val4 = plus_self;
   plus_self1 = ret_val4;
   plus_s1 = ATTR(ATTR(ATTR(self,e),fun),str);
   stdout_self1 = ((FILE1) NULL);
   L10=ZALLOC(sizeof(struct FILE1_struct));
   if (L10==NULL) FATAL("Unable to allocate more memory");
   ((OB)L10)->header.tag=FILE1_tag;
   L9 = ((FILE1) L10);
   r1 = L9;
   
   SATTR(r1,fp,stdout);
   ret_val7 = r1;
   FILE_plus_STR(ret_val7, plus_s1);
   ret_val6 = plus_self1;
   plus_self2 = ret_val6;
   plus_s2 = ((STR) &name9);
   stdout_self2 = ((FILE1) NULL);
   L14=ZALLOC(sizeof(struct FILE1_struct));
   if (L14==NULL) FATAL("Unable to allocate more memory");
   ((OB)L14)->header.tag=FILE1_tag;
   L13 = ((FILE1) L14);
   r2 = L13;
   
   SATTR(r2,fp,stdout);
   ret_val8 = r2;
   FILE_plus_STR(ret_val8, plus_s2);
  }
  else {
   res = FLISTd1429133189(res, fl);
  }
 }
 L16 = (ATTR(ATTR(ATTR(self,e),fun),ret)==((dTP) NULL));
 L171_=!(L16); 
 if (L171_) {
  res = FLISTd1429133189(res, LAYOUT79932797(((LAYOUT_TBL) NULL), ATTR(ATTR(ATTR(self,e),fun),ret)));
 }
 {
  BOOL f_L181_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = ATTR(ATTR(self,e),unbnd_args);
  L181_br=L11==NULL?0:ASIZE((ARRAYINT)L11); 
  while (1) {
   if(L31>=L181_br)  goto after_loop; 
   aL181_=ARR((ARRAYINT)L11,L31); 
   i = aL181_;
   L201_=(i)==(0); 
   if (L201_) {
    res = FLISTd1429133189(res, LAYOUT79932797(((LAYOUT_TBL) NULL), ATTR(ATTR(ATTR(self,e),fun),tp)));
   }
   else {
    L22 = ATTR(ATTR(ATTR(self,e),fun),args);
    L231_=INTMINUS(i,1); 
    L24 = L231_;
    L251_=(ARG)ARR((ARRAYARG)L22,L24); 
    L26=L251_;
    res = FLISTd1429133189(res, LAYOUT79932797(((LAYOUT_TBL) NULL), ATTR(L26,tp)));
   }
   L271_=INTPLUS(L31,1); 
   L31 = L271_;
  }
 }
 after_loop: ;
 {
  BOOL f_L281_ = TRUE;
  /* loop index variable */
  L41 = 0;
  L21 = ATTR(ATTR(self,e),bnd_args);
  L281_br=L21==NULL?0:ASIZE((ARRAYINT)L21); 
  while (1) {
   if(L41>=L281_br)  goto after_loop1; 
   rL281_=L41; 
   i1 = rL281_;
   L30 = ATTR(ATTR(self,e),bnd_args);
   L321_=ARR((ARRAYINT)L30,i1); 
   L33 = L321_;
   L341_=(L33)==(0); 
   if (L341_) {
    res = FLISTd1429133189(res, LAYOUT79932797(((LAYOUT_TBL) NULL), ATTR(ATTR(ATTR(self,e),fun),tp)));
   }
   else {
    L35 = ATTR(ATTR(ATTR(self,e),fun),args);
    L36 = ATTR(ATTR(self,e),bnd_args);
    L371_=ARR((ARRAYINT)L36,i1); 
    L38 = L371_;
    L391_=INTMINUS(L38,1); 
    L40 = L391_;
    L421_=(ARG)ARR((ARRAYARG)L35,L40); 
    L43=L421_;
    res = FLISTd1429133189(res, LAYOUT79932797(((LAYOUT_TBL) NULL), ATTR(L43,tp)));
   }
   L441_=INTPLUS(L41,1); 
   L41 = L441_;
  }
 }
 after_loop1: ;
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTdLAYOUT BOUND_1157561903(BOUND_2140373124 self) {
 FLISTdLAYOUT ret_val;
 FLISTdLAYOUT res;
 ARRAYARG L11;
 FLISTdLAYOUT create_self;
 FLISTdLAYOUT ret_val1;
 INT L21 = INT_zero;
 INT L31_br;
 ARG aL31_;
 ARG L4;
 ARG L5;
 INT L61_;
 BOOL L7;
 BOOL L81_;
 create_self = ((FLISTdLAYOUT) NULL);
 ret_val1 = ((FLISTdLAYOUT) NULL);
 res = ret_val1;
 {
  BOOL f_L31_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = ATTR(ATTR(self,tp),args);
  L31_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
  while (1) {
   if(L21>=L31_br)  goto after_loop; 
   aL31_=ARR((ARRAYARG)L11,L21); 
   L5=aL31_;
   res = FLISTd1429133189(res, LAYOUT79932797(((LAYOUT_TBL) NULL), ATTR(L5,tp)));
   L61_=INTPLUS(L21,1); 
   L21 = L61_;
  }
 }
 after_loop: ;
 L7 = (ATTR(ATTR(self,tp),ret)==((dTP) NULL));
 L81_=!(L7); 
 if (L81_) {
  res = FLISTd1429133189(res, LAYOUT79932797(((LAYOUT_TBL) NULL), ATTR(ATTR(self,tp),ret)));
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTdLAYOUT BOUND_1585387107(BOUND_1677815802 self) {
 FLISTdLAYOUT ret_val;
 FLISTdLAYOUT res;
 ARRAYINT L11;
 INT i = INT_zero;
 ARRAYINT L21;
 INT i1 = INT_zero;
 FLISTdLAYOUT create_self;
 FLISTdLAYOUT ret_val1;
 INT L31 = INT_zero;
 INT L41 = INT_zero;
 BOOL L5;
 BOOL L61_;
 INT L71_br;
 INT aL71_;
 INT L8;
 BOOL L91_;
 dTP L10;
 BOOL L12;
 BOOL L131_;
 ARRAYARG L14;
 INT L151_;
 INT L16;
 ARG L171_;
 ARG L18;
 INT L191_;
 INT L201_br;
 INT rL201_;
 INT L22;
 ARRAYINT L23;
 INT L241_;
 INT L25;
 BOOL L261_;
 dTP L27;
 BOOL L28;
 BOOL L291_;
 ARRAYARG L30;
 ARRAYINT L32;
 INT L331_;
 INT L34;
 INT L351_;
 INT L36;
 ARG L371_;
 ARG L38;
 INT L391_;
 create_self = ((FLISTdLAYOUT) NULL);
 ret_val1 = ((FLISTdLAYOUT) NULL);
 res = ret_val1;
 L5 = (ATTR(ATTR(ATTR(self,e),fun),ret)==((dTP) NULL));
 L61_=!(L5); 
 if (L61_) {
  res = FLISTd1429133189(res, LAYOUT79932797(((LAYOUT_TBL) NULL), ATTR(ATTR(ATTR(self,e),fun),ret)));
 }
 {
  BOOL f_L71_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = ATTR(ATTR(self,e),unbnd_args);
  L71_br=L11==NULL?0:ASIZE((ARRAYINT)L11); 
  while (1) {
   if(L31>=L71_br)  goto after_loop; 
   aL71_=ARR((ARRAYINT)L11,L31); 
   i = aL71_;
   L91_=(i)==(0); 
   if (L91_) {
    L10 = ATTR(ATTR(ATTR(self,e),fun),tp);
    L12 = (*dTP_is124163553[TAG(L10)])(L10);
    L131_=!(L12); 
    if (L131_) {
     res = FLISTd1429133189(res, LAYOUT79932797(((LAYOUT_TBL) NULL), ATTR(ATTR(ATTR(self,e),fun),tp)));
    }
   }
   else {
    L14 = ATTR(ATTR(ATTR(self,e),fun),args);
    L151_=INTMINUS(i,1); 
    L16 = L151_;
    L171_=(ARG)ARR((ARRAYARG)L14,L16); 
    L18=L171_;
    res = FLISTd1429133189(res, LAYOUT79932797(((LAYOUT_TBL) NULL), ATTR(L18,tp)));
   }
   L191_=INTPLUS(L31,1); 
   L31 = L191_;
  }
 }
 after_loop: ;
 {
  BOOL f_L201_ = TRUE;
  /* loop index variable */
  L41 = 0;
  L21 = ATTR(ATTR(self,e),bnd_args);
  L201_br=L21==NULL?0:ASIZE((ARRAYINT)L21); 
  while (1) {
   if(L41>=L201_br)  goto after_loop1; 
   rL201_=L41; 
   i1 = rL201_;
   L23 = ATTR(ATTR(self,e),bnd_args);
   L241_=ARR((ARRAYINT)L23,i1); 
   L25 = L241_;
   L261_=(L25)==(0); 
   if (L261_) {
    L27 = ATTR(ATTR(ATTR(self,e),fun),tp);
    L28 = (*dTP_is124163553[TAG(L27)])(L27);
    L291_=!(L28); 
    if (L291_) {
     res = FLISTd1429133189(res, LAYOUT79932797(((LAYOUT_TBL) NULL), ATTR(ATTR(ATTR(self,e),fun),tp)));
    }
   }
   else {
    L30 = ATTR(ATTR(ATTR(self,e),fun),args);
    L32 = ATTR(ATTR(self,e),bnd_args);
    L331_=ARR((ARRAYINT)L32,i1); 
    L34 = L331_;
    L351_=INTMINUS(L34,1); 
    L36 = L351_;
    L371_=(ARG)ARR((ARRAYARG)L30,L36); 
    L38=L371_;
    res = FLISTd1429133189(res, LAYOUT79932797(((LAYOUT_TBL) NULL), ATTR(L38,tp)));
   }
   L391_=INTPLUS(L41,1); 
   L41 = L391_;
  }
 }
 after_loop1: ;
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTdLAYOUT BOUND_220914967(BOUND_809390774 self) {
 FLISTdLAYOUT ret_val;
 FLISTdLAYOUT res;
 ARRAYARG L11;
 FLISTdLAYOUT create_self;
 FLISTdLAYOUT ret_val1;
 INT L21 = INT_zero;
 INT L31_br;
 ARG aL31_;
 ARG L4;
 ARG L5;
 INT L61_;
 BOOL L7;
 BOOL L81_;
 create_self = ((FLISTdLAYOUT) NULL);
 ret_val1 = ((FLISTdLAYOUT) NULL);
 res = ret_val1;
 {
  BOOL f_L31_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = ATTR(ATTR(self,tp),args);
  L31_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
  while (1) {
   if(L21>=L31_br)  goto after_loop; 
   aL31_=ARR((ARRAYARG)L11,L21); 
   L5=aL31_;
   res = FLISTd1429133189(res, LAYOUT79932797(((LAYOUT_TBL) NULL), ATTR(L5,tp)));
   L61_=INTPLUS(L21,1); 
   L21 = L61_;
  }
 }
 after_loop: ;
 L7 = (ATTR(ATTR(self,tp),ret)==((dTP) NULL));
 L81_=!(L7); 
 if (L81_) {
  res = FLISTd1429133189(res, LAYOUT79932797(((LAYOUT_TBL) NULL), ATTR(ATTR(self,tp),ret)));
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTdLAYOUT BUILTI1794609038(BUILTIN_LAYOUT self) {
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

FSTR INTI_s96356965(INTI1 self, FSTR s) {
 FSTR ret_val;
 INTI1 x;
 FLISTINT d = ((FLISTINT) NULL);
 FSTR plus_self;
 CHAR plus_c = CHAR_zero;
 FSTR ret_val1;
 FSTR plus_self1;
 CHAR plus_c1 = CHAR_zero;
 FSTR ret_val2;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val3;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val4;
 INT L1;
 BOOL L21_;
 INT L3;
 BOOL L41_;
 INT L5;
 INT L61_;
 INT L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 BOOL L111_;
 INT L12;
 BOOL L131_;
 INT L14;
 L1 = ATTR(self,len1);
 L21_=(L1)==(0); 
 if (L21_) {
  plus_self = s;
  plus_c = '0';
  ret_val1 = FSTR_p399773021(plus_self, plus_c);
  ret_val = ret_val1;
  return ret_val;
 }
 else {
  L3 = ATTR(self,len1);
  L41_=(L3)<(0); 
  if (L41_) {
   plus_self1 = s;
   plus_c1 = '-';
   ret_val2 = FSTR_p399773021(plus_self1, plus_c1);
   s = ret_val2;
  }
  L5 = ATTR(self,len1);
  L61_=(L5<0)?-L5:L5; 
  L7 = L61_;
  L81_=(L7)==(1); 
  if (L81_) {
   L91_=ARR((INTI1)self,0); 
   str_in_self = L91_;
   str_in_s = s;
   ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   ret_val = ret_val3;
   return ret_val;
  }
  else {
   x = INTI_copyrINTI(self);
   while (1) {
    d = FLISTI1369678776(d, INTI_u1380826102(self, x, INTI_D));
    L10 = ATTR(x,len1);
    L111_=(L10)==(0); 
    if (L111_) {
     goto after_loop;
    }
   }
   after_loop: ;
   str_in_self1 = FLISTINT_poprINT(d);
   str_in_s1 = s;
   ret_val4 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
   s = ret_val4;
   while (1) {
    L12 = FLISTI1321626366(d);
    L131_=(0)<(L12); 
    if (L131_) {
    }
    else {
     goto after_loop1;
    }
    L14 = FLISTINT_poprINT(d);
    s = INT_st367594495(L14, s, INTI_log10D, 10, '0');
   }
   after_loop1: ;
   ret_val = s;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

INT INTI_c675035669(INTI1 self, INTI1 y) {
 INT ret_val = INT_zero;
 INT L1;
 BOOL L21_;
 INT L3;
 INT L41_;
 INT L5;
 BOOL L61_;
 BOOL L7;
 INT L8;
 BOOL L91_;
 BOOL L10;
 BOOL L111_;
 BOOL L12;
 BOOL L131_;
 INT L14;
 BOOL L151_;
 L1 = ATTR(self,len1);
 L21_=(L1)==(0); 
 if (L21_) {
  L3 = ATTR(y,len1);
  L41_=INTMINUS(0,L3); 
  ret_val = L41_;
  return ret_val;
 }
 else {
  L5 = ATTR(self,len1);
  L61_=(L5)<(0); 
  L7 = L61_;
  L8 = ATTR(y,len1);
  L91_=(L8)<(0); 
  L10 = L91_;
  L111_=L7==L10; 
  L12 = L111_;
  L131_=!(L12); 
  if (L131_) {
   ret_val = ATTR(self,len1);
   return ret_val;
  }
  else {
   L14 = ATTR(self,len1);
   L151_=(L14)<(0); 
   if (L151_) {
    ret_val = INTI_u1057260348(self, y, self);
    return ret_val;
   }
   else {
    ret_val = INTI_u1057260348(self, self, y);
    return ret_val;
   }
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

INT INTI_hashrINT(INTI1 self) {
 INT ret_val = INT_zero;
 INT hval = INT_zero;
 INT L11 = INT_zero;
 INT i = INT_zero;
 INT L21 = INT_zero;
 INT lrotate_self = INT_zero;
 INT lrotate_i = INT_zero;
 INT ret_val1 = INT_zero;
 INT L31_;
 INT L4;
 BOOL L51_;
 BOOL L6;
 BOOL L71_;
 INT L91_;
 INT aL81_;
 INT L10;
 INT L121_;
 INT L13;
 INT L141_;
 INT L151_;
 INT L16;
 INT L17;
 INT L181_;
 INT L19;
 INT L201_;
 INT L22;
 INT L231_;
 INT L241_;
 L31_=ASIZE((INTI1)self); 
 L4 = L31_;
 L51_=(0)<(L4); 
 L6 = L51_;
 L71_=!(L6); 
 if (L71_) {
  ret_val = 289201;
  return ret_val;
 }
 else {
  hval = 0;
  {
   BOOL f_L81_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L91_=ASIZE((INTI1)self); 
   L11 = L91_;
   while (1) {
    if(L21>=L11)  goto after_loop; 
    aL81_=L21; 
    i = aL81_;
    L121_=ARR((INTI1)self,i); 
    L13 = INT_hashrINT(L121_);
    L141_=hval^L13; 
    lrotate_self = L141_;
    lrotate_i = 3;
    L151_=((unsigned)lrotate_self)<<((unsigned)lrotate_i); 
    L16 = L151_;
    L17 = INT_asize;
    L181_=INTMINUS(L17,lrotate_i); 
    L19 = L181_;
    L201_=((unsigned)lrotate_self)>>((unsigned)L19); 
    L22 = L201_;
    L231_=L16|L22; 
    ret_val1 = L231_;
    hval = ret_val1;
    L241_=INTPLUS(L21,1); 
    L21 = L241_;
   }
  }
  after_loop: ;
  ret_val = hval;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT INTI_intrINT(INTI1 self) {
 INT ret_val = INT_zero;
 INT i = INT_zero;
 INT z = INT_zero;
 INT L1;
 INT L21_;
 BOOL L31_;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 INT L9;
 INT L101_;
 INT L11;
 BOOL L121_;
 INT L131_;
 L1 = ATTR(self,len1);
 L21_=(L1<0)?-L1:L1; 
 i = L21_;
 z = 0;
 while (1) {
  L31_=(0)<(i); 
  if (L31_) {
  }
  else {
   goto after_loop;
  }
  L41_=INTMINUS(i,1); 
  i = L41_;
  L5 = INTI_B;
  L61_=INTTIMES(z,L5); 
  L7 = L61_;
  L81_=ARR((INTI1)self,i); 
  L9 = L81_;
  L101_=INTPLUS(L7,L9); 
  z = L101_;
 }
 after_loop: ;
 L11 = ATTR(self,len1);
 L121_=(L11)<(0); 
 if (L121_) {
  L131_=INTMINUS(0,z); 
  z = L131_;
 }
 ret_val = z;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT INTI_u1057260348(INTI1 self, INTI1 x, INTI1 y) {
 INT ret_val = INT_zero;
 INT i = INT_zero;
 INT j = INT_zero;
 INT z = INT_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 BOOL L5;
 BOOL L61_;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 INT L101_;
 BOOL L11;
 BOOL L121_;
 BOOL L13;
 BOOL L141_;
 INT L151_;
 INT L16;
 INT L171_;
 INT L18;
 BOOL L191_;
 INT L201_;
 INT L221_;
 INT L23;
 INT L241_;
 INT L25;
 INT L261_;
 INT L271_;
 L1 = ATTR(x,len1);
 L21_=(L1<0)?-L1:L1; 
 i = L21_;
 L3 = ATTR(y,len1);
 L41_=(L3<0)?-L3:L3; 
 j = L41_;
 L61_=(i)==(j); 
 if (L61_) {
  L71_=(i)==(0); 
  L8 = L71_;
  L91_=!(L8); 
  L5 = L91_;
 } else {
  L5 = FALSE;
 }
 if (L5) {
  L101_=INTMINUS(i,1); 
  i = L101_;
  while (1) {
   L121_=(i)==(0); 
   L13 = L121_;
   L141_=!(L13); 
   if (L141_) {
    L151_=ARR((INTI1)x,i); 
    L16 = L151_;
    L171_=ARR((INTI1)y,i); 
    L18 = L171_;
    L191_=(L16)==(L18); 
    L11 = L191_;
   } else {
    L11 = FALSE;
   }
   if (L11) {
   }
   else {
    goto after_loop;
   }
   L201_=INTMINUS(i,1); 
   i = L201_;
  }
  after_loop: ;
  L221_=ARR((INTI1)x,i); 
  L23 = L221_;
  L241_=ARR((INTI1)y,i); 
  L25 = L241_;
  L261_=INTMINUS(L23,L25); 
  z = L261_;
 }
 else {
  L271_=INTMINUS(i,j); 
  z = L271_;
 }
 ret_val = z;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT INTI_u1380826102(INTI1 self, INTI1 x, INT d) {
 INT ret_val = INT_zero;
 INT xl = INT_zero;
 INT i = INT_zero;
 INT c = INT_zero;
 INT L1;
 INT L21_;
 BOOL L31_;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 INT L9;
 INT L101_;
 INT L111_;
 INT L12;
 INT L141_;
 INT L151_;
 INT L16;
 INT L171_;
 INT L18;
 BOOL L191_;
 INT L201_;
 L1 = ATTR(x,len1);
 L21_=(L1<0)?-L1:L1; 
 xl = L21_;
 i = xl;
 c = 0;
 while (1) {
  L31_=(0)<(i); 
  if (L31_) {
  }
  else {
   goto after_loop;
  }
  L41_=INTMINUS(i,1); 
  i = L41_;
  L5 = INTI_B;
  L61_=INTTIMES(c,L5); 
  L7 = L61_;
  L81_=ARR((INTI1)x,i); 
  L9 = L81_;
  L101_=INTPLUS(L7,L9); 
  c = L101_;
  L111_=INTDIV(c,d); 
  L12 = L111_;
  SARR((INTI1)x,i,L12); 
  ;
  L141_=INTMOD(c,d); 
  c = L141_;
 }
 after_loop: ;
 L151_=INTMINUS(xl,1); 
 L16 = L151_;
 L171_=ARR((INTI1)x,L16); 
 L18 = L171_;
 L191_=(L18)==(0); 
 if (L191_) {
  L201_=INTMINUS(xl,1); 
  SATTR(x,len1,L201_);
 }
 ret_val = c;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INTI1 INTI_absrINTI(INTI1 self) {
 INTI1 ret_val;
 INTI1 z;
 INT L1;
 BOOL L21_;
 INT L3;
 INT L41_;
 L1 = ATTR(self,len1);
 L21_=(L1)<(0); 
 if (L21_) {
  z = INTI_copyrINTI(self);
  L3 = ATTR(self,len1);
  L41_=INTMINUS(0,L3); 
  SATTR(z,len1,L41_);
  ret_val = z;
  return ret_val;
 }
 else {
  ret_val = self;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INTI1 INTI_c1588968505(INTI1 self, INT x) {
 INTI1 ret_val;
 INTI1 z = ((INTI1) NULL);
 INT a = INT_zero;
 INT i = INT_zero;
 INT is_nil_self = INT_zero;
 BOOL ret_val1 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 BOOL L61_;
 INTI1 L7;
 INT L8;
 INT L91_;
 INT L101_;
 INTI1 L11;
 INT L12;
 INT L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 BOOL L201_;
 BOOL L22;
 BOOL L231_;
 INT L24;
 INT L251_;
 INT L26;
 INT L28;
 INT L291_;
 INT L301_;
 BOOL L311_;
 INT L321_;
 is_nil_self = x;
 L1 = INT_asize;
 L21_=INTMINUS(L1,1); 
 L3 = L21_;
 L41_=((unsigned)1)<<((unsigned)L3); 
 L5 = L41_;
 L61_=(is_nil_self)==(L5); 
 ret_val1 = L61_;
 if (ret_val1) {
  L7 = INTI_c1588968505(((INTI1) NULL), 2);
  L8 = INT_asize;
  L91_=INTMINUS(L8,1); 
  z = INTI_negaterINTI(INTI_p1518979099(L7, L91_));
 }
 else {
  L101_=(x<0)?-x:x; 
  a = L101_;
  i = 0;
  L13 = INT_hi740923800(a);
  L14 = INTI_log2B;
  L151_=INTDIV(L13,L14); 
  L16 = L151_;
  L171_=INTPLUS(L16,1); 
  L12 = L171_;
  L19=(sizeof(struct INTI1_struct)+1-sizeof(INT))+(L12)*sizeof(INT);
  L18=ZALLOC_LEAF_BIG(L19);
  if (L18==NULL) FATAL("Unable to allocate more memory");
  memset(L18,0,L19);
  ((OB)L18)->header.tag=INTI1_tag;
#ifdef DESTROY_CHK

    ((OB)L18)->header.destroyed=0;
#endif

  L11 = ((INTI1) L18);
  L11->asize = L12;
  z = L11;
  while (1) {
   L201_=(a)==(0); 
   L22 = L201_;
   L231_=!(L22); 
   if (L231_) {
   }
   else {
    goto after_loop;
   }
   L24 = INTI_B;
   L251_=INTMOD(a,L24); 
   L26 = L251_;
   SARR((INTI1)z,i,L26); 
   ;
   L28 = INTI_B;
   L291_=INTDIV(a,L28); 
   a = L291_;
   L301_=INTPLUS(i,1); 
   i = L301_;
  }
  after_loop: ;
  L311_=(x)<(0); 
  if (L311_) {
   L321_=INTMINUS(0,i); 
   SATTR(z,len1,L321_);
  }
  else {
   SATTR(z,len1,i);
  }
 }
 ret_val = z;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INTI1 INTI_copyrINTI(INTI1 self) {
 INTI1 ret_val;
 INT i = INT_zero;
 INTI1 z;
 INT L1;
 INT L21_;
 INTI1 L3;
 INT L4;
 INT L51_;
 OB L6;
 INT L7;
 BOOL L81_;
 INT L91_;
 INT L101_;
 INT L11;
 L1 = ATTR(self,len1);
 L21_=(L1<0)?-L1:L1; 
 i = L21_;
 L51_=INTPLUS(i,1); 
 L4 = L51_;
 L7=(sizeof(struct INTI1_struct)+1-sizeof(INT))+(L4)*sizeof(INT);
 L6=ZALLOC_LEAF_BIG(L7);
 if (L6==NULL) FATAL("Unable to allocate more memory");
 memset(L6,0,L7);
 ((OB)L6)->header.tag=INTI1_tag;
#ifdef DESTROY_CHK

   ((OB)L6)->header.destroyed=0;
#endif

 L3 = ((INTI1) L6);
 L3->asize = L4;
 z = L3;
 SATTR(z,len1,ATTR(self,len1));
 while (1) {
  L81_=(0)<(i); 
  if (L81_) {
  }
  else {
   goto after_loop;
  }
  L91_=INTMINUS(i,1); 
  i = L91_;
  L101_=ARR((INTI1)self,i); 
  L11 = L101_;
  SARR((INTI1)z,i,L11); 
  ;
 }
 after_loop: ;
 ret_val = z;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INTI1 INTI_d359767808(INTI1 self, INTI1 y) {
 INTI1 ret_val;
 INTI1 z = ((INTI1) NULL);
 INTI1 qr;
 INT L1;
 INT L21_;
 INT L3;
 INT L4;
 INT L51_;
 INT L6;
 BOOL L71_;
 BOOL L8;
 INT L9;
 BOOL L101_;
 INTI1 L11;
 INT L12;
 BOOL L131_;
 BOOL L14;
 BOOL L151_;
 INT L16;
 BOOL L171_;
 BOOL L18;
 INT L19;
 BOOL L201_;
 BOOL L22;
 BOOL L231_;
 BOOL L24;
 BOOL L251_;
 INT L26;
 INT L271_;
 L1 = ATTR(self,len1);
 L21_=(L1<0)?-L1:L1; 
 L3 = L21_;
 L4 = ATTR(y,len1);
 L51_=(L4<0)?-L4:L4; 
 L6 = L51_;
 L71_=(L3)<(L6); 
 if (L71_) {
  z = INTI_c1588968505(((INTI1) NULL), 0);
 }
 else {
  qr = INTI_u1836437467(self, self, y);
  z = INTI_g1390757385(self, self, y, qr);
  L9 = ATTR(self,len1);
  L101_=(L9)<(0); 
  if (L101_) {
   L11=INTI_g960784342(self, self, y, qr);
   L12 = ATTR(L11,len1);
   L131_=(L12)==(0); 
   L14 = L131_;
   L151_=!(L14); 
   L8 = L151_;
  } else {
   L8 = FALSE;
  }
  if (L8) {
   z = INTI_u450560926(self, z, 1, 1);
  }
  L16 = ATTR(self,len1);
  L171_=(L16)<(0); 
  L18 = L171_;
  L19 = ATTR(y,len1);
  L201_=(L19)<(0); 
  L22 = L201_;
  L231_=L18==L22; 
  L24 = L231_;
  L251_=!(L24); 
  if (L251_) {
   L26 = ATTR(z,len1);
   L271_=INTMINUS(0,L26); 
   SATTR(z,len1,L271_);
  }
 }
 ret_val = z;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INTI1 INTI_g1390757385(INTI1 self, INTI1 x, INTI1 y, INTI1 q) {
 INTI1 ret_val;
 INT i = INT_zero;
 INT yl = INT_zero;
 INTI1 z;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 BOOL L5;
 BOOL L61_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 BOOL L111_;
 INT L121_;
 INTI1 L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_;
 INT L22;
 INT L231_;
 BOOL L241_;
 BOOL L25;
 BOOL L261_;
 INT L271_;
 INT L28;
 INT L291_;
 INT L30;
 INT L321_;
 L1 = ATTR(x,len1);
 L21_=(L1<0)?-L1:L1; 
 i = L21_;
 L3 = ATTR(y,len1);
 L41_=(L3<0)?-L3:L3; 
 yl = L41_;
 while (1) {
  L61_=(i)<(yl); 
  L7 = L61_;
  L81_=!(L7); 
  if (L81_) {
   L91_=ARR((INTI1)q,i); 
   L10 = L91_;
   L111_=(L10)==(0); 
   L5 = L111_;
  } else {
   L5 = FALSE;
  }
  if (L5) {
  }
  else {
   goto after_loop;
  }
  L121_=INTMINUS(i,1); 
  i = L121_;
 }
 after_loop: ;
 L151_=INTMINUS(i,yl); 
 L16 = L151_;
 L171_=INTPLUS(L16,1); 
 L14 = L171_;
 L19=(sizeof(struct INTI1_struct)+1-sizeof(INT))+(L14)*sizeof(INT);
 L18=ZALLOC_LEAF_BIG(L19);
 if (L18==NULL) FATAL("Unable to allocate more memory");
 memset(L18,0,L19);
 ((OB)L18)->header.tag=INTI1_tag;
#ifdef DESTROY_CHK

   ((OB)L18)->header.destroyed=0;
#endif

 L13 = ((INTI1) L18);
 L13->asize = L14;
 z = L13;
 L201_=INTMINUS(i,yl); 
 L22 = L201_;
 L231_=INTPLUS(L22,1); 
 SATTR(z,len1,L231_);
 while (1) {
  L241_=(i)<(yl); 
  L25 = L241_;
  L261_=!(L25); 
  if (L261_) {
  }
  else {
   goto after_loop1;
  }
  L271_=INTMINUS(i,yl); 
  L28 = L271_;
  L291_=ARR((INTI1)q,i); 
  L30 = L291_;
  SARR((INTI1)z,L28,L30); 
  ;
  L321_=INTMINUS(i,1); 
  i = L321_;
 }
 after_loop1: ;
 ret_val = z;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INTI1 INTI_g1917565373(INTI1 self, INTI1 i) {
 INTI1 ret_val;
 INTI1 c;
 INTI1 d;
 INTI1 r;
 INT L1;
 BOOL L21_;
 c = INTI_absrINTI(self);
 d = INTI_absrINTI(i);
 while (1) {
  L1 = ATTR(d,len1);
  L21_=(L1)==(0); 
  if (L21_) {
   goto after_loop;
  }
  r = INTI_m582155739(c, d);
  c = d;
  d = r;
 }
 after_loop: ;
 ret_val = c;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INTI1 INTI_g960784342(INTI1 self, INTI1 x, INTI1 y, INTI1 q) {
 INTI1 ret_val;
 INT i = INT_zero;
 INTI1 z;
 INT L1;
 INT L21_;
 INT L3;
 INT L4;
 INT L51_;
 INT L6;
 INT L71_;
 INT L8;
 INT L91_;
 BOOL L10;
 BOOL L111_;
 BOOL L12;
 BOOL L131_;
 INT L141_;
 INT L15;
 BOOL L161_;
 INT L171_;
 INTI1 L18;
 INT L19;
 INT L201_;
 OB L22;
 INT L23;
 INT L241_;
 BOOL L251_;
 BOOL L26;
 BOOL L271_;
 INT L281_;
 INT L29;
 INT L311_;
 L1 = ATTR(x,len1);
 L21_=(L1<0)?-L1:L1; 
 L3 = L21_;
 L4 = ATTR(y,len1);
 L51_=(L4<0)?-L4:L4; 
 L6 = L51_;
 L71_=(L3<L6)?L3:L6; 
 L8 = L71_;
 L91_=INTMINUS(L8,1); 
 i = L91_;
 while (1) {
  L111_=(i)<(0); 
  L12 = L111_;
  L131_=!(L12); 
  if (L131_) {
   L141_=ARR((INTI1)q,i); 
   L15 = L141_;
   L161_=(L15)==(0); 
   L10 = L161_;
  } else {
   L10 = FALSE;
  }
  if (L10) {
  }
  else {
   goto after_loop;
  }
  L171_=INTMINUS(i,1); 
  i = L171_;
 }
 after_loop: ;
 L201_=INTPLUS(i,1); 
 L19 = L201_;
 L23=(sizeof(struct INTI1_struct)+1-sizeof(INT))+(L19)*sizeof(INT);
 L22=ZALLOC_LEAF_BIG(L23);
 if (L22==NULL) FATAL("Unable to allocate more memory");
 memset(L22,0,L23);
 ((OB)L22)->header.tag=INTI1_tag;
#ifdef DESTROY_CHK

   ((OB)L22)->header.destroyed=0;
#endif

 L18 = ((INTI1) L22);
 L18->asize = L19;
 z = L18;
 L241_=INTPLUS(i,1); 
 SATTR(z,len1,L241_);
 while (1) {
  L251_=(i)<(0); 
  L26 = L251_;
  L271_=!(L26); 
  if (L271_) {
  }
  else {
   goto after_loop1;
  }
  L281_=ARR((INTI1)q,i); 
  L29 = L281_;
  SARR((INTI1)z,i,L29); 
  ;
  L311_=INTMINUS(i,1); 
  i = L311_;
 }
 after_loop1: ;
 ret_val = z;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INTI1 INTI_m436971372(INTI1 self, INTI1 y) {
 INTI1 ret_val;
 INTI1 z = ((INTI1) NULL);
 INT L1;
 BOOL L21_;
 BOOL L3;
 INT L4;
 BOOL L51_;
 BOOL L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 INT L10;
 BOOL L111_;
 INT L12;
 INT L131_;
 INT L14;
 BOOL L151_;
 INT L16;
 INT L171_;
 L1 = ATTR(self,len1);
 L21_=(L1)<(0); 
 L3 = L21_;
 L4 = ATTR(y,len1);
 L51_=(L4)<(0); 
 L6 = L51_;
 L71_=L3==L6; 
 L8 = L71_;
 L91_=!(L8); 
 if (L91_) {
  z = INTI_u2116735672(self, self, y);
 }
 else {
  L10 = INTI_u1057260348(self, self, y);
  L111_=(L10)<(0); 
  if (L111_) {
   z = INTI_u1193628812(self, y, self);
   L12 = ATTR(z,len1);
   L131_=INTMINUS(0,L12); 
   SATTR(z,len1,L131_);
  }
  else {
   z = INTI_u1193628812(self, self, y);
  }
 }
 L14 = ATTR(self,len1);
 L151_=(L14)<(0); 
 if (L151_) {
  L16 = ATTR(z,len1);
  L171_=INTMINUS(0,L16); 
  SATTR(z,len1,L171_);
 }
 ret_val = z;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INTI1 INTI_m582155739(INTI1 self, INTI1 y) {
 INTI1 ret_val;
 INTI1 z = ((INTI1) NULL);
 INT L1;
 INT L21_;
 INT L3;
 INT L4;
 INT L51_;
 INT L6;
 BOOL L71_;
 BOOL L8;
 INT L9;
 BOOL L101_;
 INT L11;
 BOOL L121_;
 BOOL L13;
 BOOL L141_;
 L1 = ATTR(self,len1);
 L21_=(L1<0)?-L1:L1; 
 L3 = L21_;
 L4 = ATTR(y,len1);
 L51_=(L4<0)?-L4:L4; 
 L6 = L51_;
 L71_=(L3)<(L6); 
 if (L71_) {
  z = self;
 }
 else {
  z = INTI_g960784342(self, self, y, INTI_u1836437467(self, self, y));
  L9 = ATTR(self,len1);
  L101_=(L9)<(0); 
  if (L101_) {
   L11 = ATTR(z,len1);
   L121_=(L11)==(0); 
   L13 = L121_;
   L141_=!(L13); 
   L8 = L141_;
  } else {
   L8 = FALSE;
  }
  if (L8) {
   z = INTI_u1193628812(self, y, z);
  }
 }
 ret_val = z;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INTI1 INTI_negaterINTI(INTI1 self) {
 INTI1 ret_val;
 INTI1 z;
 INT L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 INT L5;
 INT L61_;
 L1 = ATTR(self,len1);
 L21_=(L1)==(0); 
 L3 = L21_;
 L41_=!(L3); 
 if (L41_) {
  z = INTI_copyrINTI(self);
  L5 = ATTR(self,len1);
  L61_=INTMINUS(0,L5); 
  SATTR(z,len1,L61_);
  ret_val = z;
  return ret_val;
 }
 else {
  ret_val = self;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INTI1 INTI_p1518979099(INTI1 self, INT i) {
 INTI1 ret_val;
 INTI1 x;
 INTI1 z;
 INTI1 square_self;
 INTI1 ret_val1;
 BOOL L11_;
 BOOL L21_;
 INT L31_;
 x = self;
 z = INTI_c1588968505(((INTI1) NULL), 1);
 while (1) {
  L11_=(0)<(i); 
  if (L11_) {
  }
  else {
   goto after_loop;
  }
  L21_=(i&1)!=0; 
  if (L21_) {
   z = INTI_t1692605406(z, x);
  }
  square_self = x;
  ret_val1 = INTI_t1692605406(square_self, square_self);
  x = ret_val1;
  L31_=INTDIV(i,2); 
  i = L31_;
 }
 after_loop: ;
 ret_val = z;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INTI1 INTI_p1769163857(INTI1 self, INTI1 y) {
 INTI1 ret_val;
 INTI1 z = ((INTI1) NULL);
 INT L1;
 BOOL L21_;
 BOOL L3;
 INT L4;
 BOOL L51_;
 BOOL L6;
 BOOL L71_;
 INT L8;
 BOOL L91_;
 INT L10;
 INT L111_;
 INT L12;
 BOOL L131_;
 INT L14;
 INT L151_;
 L1 = ATTR(self,len1);
 L21_=(L1)<(0); 
 L3 = L21_;
 L4 = ATTR(y,len1);
 L51_=(L4)<(0); 
 L6 = L51_;
 L71_=L3==L6; 
 if (L71_) {
  z = INTI_u2116735672(self, self, y);
 }
 else {
  L8 = INTI_u1057260348(self, self, y);
  L91_=(L8)<(0); 
  if (L91_) {
   z = INTI_u1193628812(self, y, self);
   L10 = ATTR(z,len1);
   L111_=INTMINUS(0,L10); 
   SATTR(z,len1,L111_);
  }
  else {
   z = INTI_u1193628812(self, self, y);
  }
 }
 L12 = ATTR(self,len1);
 L131_=(L12)<(0); 
 if (L131_) {
  L14 = ATTR(z,len1);
  L151_=INTMINUS(0,L14); 
  SATTR(z,len1,L151_);
 }
 ret_val = z;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INTI1 INTI_t1692605406(INTI1 self, INTI1 y) {
 INTI1 ret_val;
 INTI1 z = ((INTI1) NULL);
 BOOL L1;
 INT L2;
 BOOL L31_;
 INT L4;
 BOOL L51_;
 BOOL L6;
 INT L7;
 INT L81_;
 INT L9;
 BOOL L101_;
 INT L11;
 INT L121_;
 INT L13;
 BOOL L141_;
 INT L151_;
 INT L16;
 INT L171_;
 INT L18;
 INT L191_;
 INT L20;
 INT L211_;
 INT L22;
 INT L23;
 INT L241_;
 INT L25;
 BOOL L261_;
 INT L27;
 BOOL L281_;
 BOOL L29;
 INT L30;
 BOOL L321_;
 BOOL L33;
 BOOL L341_;
 BOOL L35;
 BOOL L361_;
 INT L37;
 INT L381_;
 L2 = ATTR(self,len1);
 L31_=(L2)==(0); 
 if (L31_) {
  L1 = TRUE;
 } else {
  L4 = ATTR(y,len1);
  L51_=(L4)==(0); 
  L1 = L51_;
 }
 if (L1) {
  z = INTI_c1588968505(((INTI1) NULL), 0);
 }
 else {
  L7 = ATTR(self,len1);
  L81_=(L7<0)?-L7:L7; 
  L9 = L81_;
  L101_=(L9)==(1); 
  if (L101_) {
   L11 = ATTR(y,len1);
   L121_=(L11<0)?-L11:L11; 
   L13 = L121_;
   L141_=(L13)==(1); 
   L6 = L141_;
  } else {
   L6 = FALSE;
  }
  if (L6) {
   L151_=ARR((INTI1)self,0); 
   L16 = L151_;
   L171_=ARR((INTI1)y,0); 
   L18 = L171_;
   L191_=INTTIMES(L16,L18); 
   z = INTI_c1588968505(((INTI1) NULL), L191_);
  }
  else {
   L20 = ATTR(self,len1);
   L211_=(L20<0)?-L20:L20; 
   L22 = L211_;
   L23 = ATTR(y,len1);
   L241_=(L23<0)?-L23:L23; 
   L25 = L241_;
   L261_=(L22)<(L25); 
   if (L261_) {
    z = INTI_u1281648686(self, self, y);
   }
   else {
    z = INTI_u1281648686(self, y, self);
   }
  }
 }
 L27 = ATTR(self,len1);
 L281_=(L27)<(0); 
 L29 = L281_;
 L30 = ATTR(y,len1);
 L321_=(L30)<(0); 
 L33 = L321_;
 L341_=L29==L33; 
 L35 = L341_;
 L361_=!(L35); 
 if (L361_) {
  L37 = ATTR(z,len1);
  L381_=INTMINUS(0,L37); 
  SATTR(z,len1,L381_);
 }
 ret_val = z;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INTI1 INTI_u1193628812(INTI1 self, INTI1 x, INTI1 y) {
 INTI1 ret_val;
 INT xl = INT_zero;
 INT yl = INT_zero;
 INT i = INT_zero;
 INT c = INT_zero;
 INTI1 z = ((INTI1) NULL);
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INTI1 L5;
 INT L6;
 OB L7;
 INT L8;
 BOOL L91_;
 INT L101_;
 INT L11;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 INT L161_;
 INT L17;
 INT L181_;
 INT L19;
 INT L22;
 INT L231_;
 INT L241_;
 BOOL L251_;
 INT L261_;
 INT L27;
 INT L281_;
 INT L29;
 INT L301_;
 INT L31;
 INT L33;
 INT L341_;
 INT L351_;
 BOOL L36;
 BOOL L371_;
 INT L381_;
 INT L39;
 INT L401_;
 INT L42;
 BOOL L431_;
 INT L441_;
 L1 = ATTR(x,len1);
 L21_=(L1<0)?-L1:L1; 
 xl = L21_;
 L3 = ATTR(y,len1);
 L41_=(L3<0)?-L3:L3; 
 yl = L41_;
 i = 0;
 c = 0;
 L6 = xl;
 L8=(sizeof(struct INTI1_struct)+1-sizeof(INT))+(L6)*sizeof(INT);
 L7=ZALLOC_LEAF_BIG(L8);
 if (L7==NULL) FATAL("Unable to allocate more memory");
 memset(L7,0,L8);
 ((OB)L7)->header.tag=INTI1_tag;
#ifdef DESTROY_CHK

   ((OB)L7)->header.destroyed=0;
#endif

 L5 = ((INTI1) L7);
 L5->asize = L6;
 z = L5;
 while (1) {
  L91_=(i)<(yl); 
  if (L91_) {
  }
  else {
   goto after_loop;
  }
  L101_=ARR((INTI1)x,i); 
  L11 = L101_;
  L121_=INTPLUS(c,L11); 
  L13 = L121_;
  L141_=ARR((INTI1)y,i); 
  L15 = L141_;
  L161_=INTMINUS(L13,L15); 
  c = L161_;
  L17 = INTI_B;
  L181_=INTMOD(c,L17); 
  L19 = L181_;
  SARR((INTI1)z,i,L19); 
  ;
  L22 = INTI_B;
  L231_=INTDIV(c,L22); 
  c = L231_;
  L241_=INTPLUS(i,1); 
  i = L241_;
 }
 after_loop: ;
 while (1) {
  L251_=(i)<(xl); 
  if (L251_) {
  }
  else {
   goto after_loop1;
  }
  L261_=ARR((INTI1)x,i); 
  L27 = L261_;
  L281_=INTPLUS(c,L27); 
  c = L281_;
  L29 = INTI_B;
  L301_=INTMOD(c,L29); 
  L31 = L301_;
  SARR((INTI1)z,i,L31); 
  ;
  L33 = INTI_B;
  L341_=INTDIV(c,L33); 
  c = L341_;
  L351_=INTPLUS(i,1); 
  i = L351_;
 }
 after_loop1: ;
 while (1) {
  L371_=(0)<(i); 
  if (L371_) {
   L381_=INTMINUS(i,1); 
   L39 = L381_;
   L401_=ARR((INTI1)z,L39); 
   L42 = L401_;
   L431_=(L42)==(0); 
   L36 = L431_;
  } else {
   L36 = FALSE;
  }
  if (L36) {
  }
  else {
   goto after_loop2;
  }
  L441_=INTMINUS(i,1); 
  i = L441_;
 }
 after_loop2: ;
 SATTR(z,len1,i);
 ret_val = z;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INTI1 INTI_u1281648686(INTI1 self, INTI1 x, INTI1 y) {
 INTI1 ret_val;
 INT xl = INT_zero;
 INT yl = INT_zero;
 INT i = INT_zero;
 INT j = INT_zero;
 INT k = INT_zero;
 INT d = INT_zero;
 INT c = INT_zero;
 INTI1 z;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L51_;
 INTI1 L6;
 INT L7;
 OB L8;
 INT L9;
 BOOL L101_;
 INT L111_;
 BOOL L131_;
 INT L141_;
 BOOL L151_;
 BOOL L16;
 BOOL L171_;
 BOOL L181_;
 INT L191_;
 INT L20;
 INT L221_;
 INT L23;
 INT L241_;
 INT L25;
 INT L261_;
 INT L27;
 INT L281_;
 INT L29;
 INT L301_;
 INT L31;
 INT L33;
 INT L341_;
 INT L351_;
 INT L361_;
 BOOL L371_;
 BOOL L38;
 BOOL L391_;
 INT L421_;
 INT L431_;
 L1 = ATTR(x,len1);
 L21_=(L1<0)?-L1:L1; 
 xl = L21_;
 L3 = ATTR(y,len1);
 L41_=(L3<0)?-L3:L3; 
 yl = L41_;
 L51_=INTPLUS(xl,yl); 
 i = L51_;
 L7 = i;
 L9=(sizeof(struct INTI1_struct)+1-sizeof(INT))+(L7)*sizeof(INT);
 L8=ZALLOC_LEAF_BIG(L9);
 if (L8==NULL) FATAL("Unable to allocate more memory");
 memset(L8,0,L9);
 ((OB)L8)->header.tag=INTI1_tag;
#ifdef DESTROY_CHK

   ((OB)L8)->header.destroyed=0;
#endif

 L6 = ((INTI1) L8);
 L6->asize = L7;
 z = L6;
 while (1) {
  L101_=(0)<(i); 
  if (L101_) {
  }
  else {
   goto after_loop;
  }
  L111_=INTMINUS(i,1); 
  i = L111_;
  SARR((INTI1)z,i,0); 
  ;
 }
 after_loop: ;
 while (1) {
  L131_=(i)<(xl); 
  if (L131_) {
  }
  else {
   goto after_loop1;
  }
  L141_=ARR((INTI1)x,i); 
  d = L141_;
  L151_=(d)==(0); 
  L16 = L151_;
  L171_=!(L16); 
  if (L171_) {
   j = 0;
   k = i;
   c = 0;
   while (1) {
    L181_=(j)<(yl); 
    if (L181_) {
    }
    else {
     goto after_loop2;
    }
    L191_=ARR((INTI1)z,k); 
    L20 = L191_;
    L221_=INTPLUS(c,L20); 
    L23 = L221_;
    L241_=ARR((INTI1)y,j); 
    L25 = L241_;
    L261_=INTTIMES(d,L25); 
    L27 = L261_;
    L281_=INTPLUS(L23,L27); 
    c = L281_;
    L29 = INTI_B;
    L301_=INTMOD(c,L29); 
    L31 = L301_;
    SARR((INTI1)z,k,L31); 
    ;
    L33 = INTI_B;
    L341_=INTDIV(c,L33); 
    c = L341_;
    L351_=INTPLUS(j,1); 
    j = L351_;
    L361_=INTPLUS(k,1); 
    k = L361_;
   }
   after_loop2: ;
   L371_=(c)==(0); 
   L38 = L371_;
   L391_=!(L38); 
   if (L391_) {
    SARR((INTI1)z,k,c); 
    ;
    L421_=INTPLUS(k,1); 
    k = L421_;
   }
  }
  L431_=INTPLUS(i,1); 
  i = L431_;
 }
 after_loop1: ;
 SATTR(z,len1,k);
 ret_val = z;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INTI1 INTI_u1836437467(INTI1 self, INTI1 x, INTI1 y) {
 INTI1 ret_val;
 INT xl = INT_zero;
 INT yl = INT_zero;
 INT i = INT_zero;
 INT j = INT_zero;
 INT k = INT_zero;
 INT c = INT_zero;
 INT d = INT_zero;
 INT q = INT_zero;
 INT y11 = INT_zero;
 INT y2 = INT_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 BOOL L51_;
 INT L61_;
 INT L71_;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 INT L11;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 INT L161_;
 INT L171_;
 INT L18;
 INT L191_;
 INT L20;
 INT L231_;
 INT L241_;
 BOOL L261_;
 BOOL L27;
 BOOL L281_;
 INT L30;
 INT L311_;
 INT L32;
 INT L331_;
 INT L34;
 INT L351_;
 INT L36;
 INT L371_;
 INT L38;
 INT L391_;
 INT L40;
 INT L421_;
 BOOL L431_;
 BOOL L44;
 BOOL L451_;
 BOOL L461_;
 INT L471_;
 INT L48;
 INT L491_;
 INT L50;
 INT L521_;
 INT L53;
 INT L541_;
 INT L55;
 INT L57;
 INT L581_;
 INT L591_;
 BOOL L621_;
 INT L631_;
 INT L64;
 INT L651_;
 INT L66;
 INT L671_;
 INT L68;
 INT L691_;
 INT L70;
 INT L73;
 INT L741_;
 INT L751_;
 INT L761_;
 INT L77;
 INT L781_;
 INT L791_;
 INT L80;
 INT L821_;
 BOOL L831_;
 BOOL L84;
 BOOL L851_;
 INT L861_;
 INT L87;
 BOOL L881_;
 BOOL L89;
 BOOL L901_;
 INT L911_;
 INT L92;
 INT L93;
 INT L941_;
 INT L95;
 INT L961_;
 INT L97;
 INT L981_;
 INT L99;
 INT L1001_;
 INT L102;
 INT L1031_;
 INT L104;
 INT L1051_;
 INT L1061_;
 INT L107;
 INT L108;
 INT L1091_;
 INT L110;
 INT L1111_;
 INT L112;
 INT L1131_;
 INT L114;
 INT L1151_;
 INT L116;
 INT L1171_;
 INT L118;
 INT L1191_;
 INT L120;
 INT L122;
 INT L1231_;
 INT L124;
 INT L1251_;
 INT L126;
 INT L1271_;
 INT L128;
 INT L1291_;
 INT L130;
 INT L1311_;
 INT L132;
 BOOL L1331_;
 INT L1341_;
 INT L1351_;
 BOOL L1361_;
 INT L1371_;
 INT L138;
 INT L1391_;
 INT L140;
 INT L1421_;
 INT L143;
 INT L1441_;
 INT L145;
 INT L1461_;
 INT L147;
 INT L1481_;
 INT L149;
 INT L151;
 INT L1521_;
 INT L1531_;
 INT L1541_;
 INT L1551_;
 INT L156;
 INT L1571_;
 INT L158;
 BOOL L1591_;
 BOOL L160;
 BOOL L1621_;
 INT L1631_;
 BOOL L1641_;
 INT L1651_;
 INT L166;
 INT L1671_;
 INT L168;
 INT L1691_;
 INT L170;
 INT L1721_;
 INT L173;
 INT L1741_;
 INT L175;
 INT L177;
 INT L1781_;
 INT L1791_;
 INT L1801_;
 INT L1811_;
 INT L182;
 INT L1851_;
 BOOL L1861_;
 BOOL L187;
 BOOL L1881_;
 BOOL L1891_;
 INT L1901_;
 INT L192;
 INT L1931_;
 INT L194;
 INT L1951_;
 INT L196;
 INT L1971_;
 INT L1981_;
 INT L199;
 INT L2011_;
 L1 = ATTR(x,len1);
 L21_=(L1<0)?-L1:L1; 
 xl = L21_;
 L3 = ATTR(y,len1);
 L41_=(L3<0)?-L3:L3; 
 yl = L41_;
 x = INTI_copyrINTI(x);
 L51_=(yl)==(1); 
 if (L51_) {
  L61_=INTMINUS(xl,1); 
  i = L61_;
  c = 0;
  L71_=ARR((INTI1)y,0); 
  d = L71_;
  while (1) {
   L81_=(i)<(0); 
   L9 = L81_;
   L101_=!(L9); 
   if (L101_) {
   }
   else {
    goto after_loop;
   }
   L11 = INTI_B;
   L121_=INTTIMES(c,L11); 
   L13 = L121_;
   L141_=ARR((INTI1)x,i); 
   L15 = L141_;
   L161_=INTPLUS(L13,L15); 
   c = L161_;
   L171_=INTPLUS(i,1); 
   L18 = L171_;
   L191_=INTDIV(c,d); 
   L20 = L191_;
   SARR((INTI1)x,L18,L20); 
   ;
   L231_=INTMOD(c,d); 
   c = L231_;
   L241_=INTMINUS(i,1); 
   i = L241_;
  }
  after_loop: ;
  SARR((INTI1)x,0,c); 
  ;
 }
 else {
  L261_=(xl)<(yl); 
  L27 = L261_;
  L281_=!(L27); 
  if (L281_) {
   SARR((INTI1)x,xl,0); 
   ;
   L30 = INTI_B;
   L311_=INTDIV(L30,2); 
   L32 = L311_;
   L331_=INTMINUS(L32,1); 
   L34 = L331_;
   L351_=INTMINUS(yl,1); 
   L36 = L351_;
   L371_=ARR((INTI1)y,L36); 
   L38 = L371_;
   L391_=INTDIV(L34,L38); 
   L40 = L391_;
   L421_=INTPLUS(L40,1); 
   d = L421_;
   L431_=(d)==(1); 
   L44 = L431_;
   L451_=!(L44); 
   if (L451_) {
    y = INTI_copyrINTI(y);
    i = 0;
    c = 0;
    while (1) {
     L461_=(i)<(xl); 
     if (L461_) {
     }
     else {
      goto after_loop1;
     }
     L471_=ARR((INTI1)x,i); 
     L48 = L471_;
     L491_=INTTIMES(d,L48); 
     L50 = L491_;
     L521_=INTPLUS(c,L50); 
     c = L521_;
     L53 = INTI_B;
     L541_=INTMOD(c,L53); 
     L55 = L541_;
     SARR((INTI1)x,i,L55); 
     ;
     L57 = INTI_B;
     L581_=INTDIV(c,L57); 
     c = L581_;
     L591_=INTPLUS(i,1); 
     i = L591_;
    }
    after_loop1: ;
    SARR((INTI1)x,i,c); 
    ;
    i = 0;
    c = 0;
    while (1) {
     L621_=(i)<(yl); 
     if (L621_) {
     }
     else {
      goto after_loop2;
     }
     L631_=ARR((INTI1)y,i); 
     L64 = L631_;
     L651_=INTTIMES(d,L64); 
     L66 = L651_;
     L671_=INTPLUS(c,L66); 
     c = L671_;
     L68 = INTI_B;
     L691_=INTMOD(c,L68); 
     L70 = L691_;
     SARR((INTI1)y,i,L70); 
     ;
     L73 = INTI_B;
     L741_=INTDIV(c,L73); 
     c = L741_;
     L751_=INTPLUS(i,1); 
     i = L751_;
    }
    after_loop2: ;
   }
   L761_=INTMINUS(yl,1); 
   L77 = L761_;
   L781_=ARR((INTI1)y,L77); 
   y11 = L781_;
   L791_=INTMINUS(yl,2); 
   L80 = L791_;
   L821_=ARR((INTI1)y,L80); 
   y2 = L821_;
   i = xl;
   while (1) {
    L831_=(i)<(yl); 
    L84 = L831_;
    L851_=!(L84); 
    if (L851_) {
    }
    else {
     goto after_loop3;
    }
    L861_=ARR((INTI1)x,i); 
    L87 = L861_;
    L881_=(L87)==(y11); 
    L89 = L881_;
    L901_=!(L89); 
    if (L901_) {
     L911_=ARR((INTI1)x,i); 
     L92 = L911_;
     L93 = INTI_B;
     L941_=INTTIMES(L92,L93); 
     L95 = L941_;
     L961_=INTMINUS(i,1); 
     L97 = L961_;
     L981_=ARR((INTI1)x,L97); 
     L99 = L981_;
     L1001_=INTPLUS(L95,L99); 
     L102 = L1001_;
     L1031_=INTDIV(L102,y11); 
     q = L1031_;
    }
    else {
     L104 = INTI_B;
     L1051_=INTMINUS(L104,1); 
     q = L1051_;
    }
    while (1) {
     L1061_=ARR((INTI1)x,i); 
     L107 = L1061_;
     L108 = INTI_B;
     L1091_=INTTIMES(L107,L108); 
     L110 = L1091_;
     L1111_=INTMINUS(i,1); 
     L112 = L1111_;
     L1131_=ARR((INTI1)x,L112); 
     L114 = L1131_;
     L1151_=INTPLUS(L110,L114); 
     L116 = L1151_;
     L1171_=INTTIMES(y11,q); 
     L118 = L1171_;
     L1191_=INTMINUS(L116,L118); 
     L120 = L1191_;
     L122 = INTI_B;
     L1231_=INTTIMES(L120,L122); 
     L124 = L1231_;
     L1251_=INTMINUS(i,2); 
     L126 = L1251_;
     L1271_=ARR((INTI1)x,L126); 
     L128 = L1271_;
     L1291_=INTPLUS(L124,L128); 
     L130 = L1291_;
     L1311_=INTTIMES(y2,q); 
     L132 = L1311_;
     L1331_=(L130)<(L132); 
     if (L1331_) {
     }
     else {
      goto after_loop4;
     }
     L1341_=INTMINUS(q,1); 
     q = L1341_;
    }
    after_loop4: ;
    L1351_=INTMINUS(i,yl); 
    j = L1351_;
    k = 0;
    c = 0;
    while (1) {
     L1361_=(k)<(yl); 
     if (L1361_) {
     }
     else {
      goto after_loop5;
     }
     L1371_=ARR((INTI1)x,j); 
     L138 = L1371_;
     L1391_=INTPLUS(c,L138); 
     L140 = L1391_;
     L1421_=ARR((INTI1)y,k); 
     L143 = L1421_;
     L1441_=INTTIMES(q,L143); 
     L145 = L1441_;
     L1461_=INTMINUS(L140,L145); 
     c = L1461_;
     L147 = INTI_B;
     L1481_=INTMOD(c,L147); 
     L149 = L1481_;
     SARR((INTI1)x,j,L149); 
     ;
     L151 = INTI_B;
     L1521_=INTDIV(c,L151); 
     c = L1521_;
     L1531_=INTPLUS(j,1); 
     j = L1531_;
     L1541_=INTPLUS(k,1); 
     k = L1541_;
    }
    after_loop5: ;
    L1551_=ARR((INTI1)x,i); 
    L156 = L1551_;
    L1571_=INTPLUS(c,L156); 
    L158 = L1571_;
    L1591_=(L158)==(0); 
    L160 = L1591_;
    L1621_=!(L160); 
    if (L1621_) {
     L1631_=INTMINUS(i,yl); 
     j = L1631_;
     k = 0;
     c = 0;
     while (1) {
      L1641_=(k)<(yl); 
      if (L1641_) {
      }
      else {
       goto after_loop6;
      }
      L1651_=ARR((INTI1)x,j); 
      L166 = L1651_;
      L1671_=INTPLUS(c,L166); 
      L168 = L1671_;
      L1691_=ARR((INTI1)y,k); 
      L170 = L1691_;
      L1721_=INTPLUS(L168,L170); 
      c = L1721_;
      L173 = INTI_B;
      L1741_=INTMOD(c,L173); 
      L175 = L1741_;
      SARR((INTI1)x,j,L175); 
      ;
      L177 = INTI_B;
      L1781_=INTDIV(c,L177); 
      c = L1781_;
      L1791_=INTPLUS(j,1); 
      j = L1791_;
      L1801_=INTPLUS(k,1); 
      k = L1801_;
     }
     after_loop6: ;
     L1811_=INTMINUS(q,1); 
     L182 = L1811_;
     SARR((INTI1)x,i,L182); 
     ;
    }
    else {
     SARR((INTI1)x,i,q); 
     ;
    }
    L1851_=INTMINUS(i,1); 
    i = L1851_;
   }
   after_loop3: ;
   L1861_=(d)==(1); 
   L187 = L1861_;
   L1881_=!(L187); 
   if (L1881_) {
    i = yl;
    c = 0;
    while (1) {
     L1891_=(0)<(i); 
     if (L1891_) {
     }
     else {
      goto after_loop7;
     }
     L1901_=INTMINUS(i,1); 
     i = L1901_;
     L192 = INTI_B;
     L1931_=INTTIMES(c,L192); 
     L194 = L1931_;
     L1951_=ARR((INTI1)x,i); 
     L196 = L1951_;
     L1971_=INTPLUS(L194,L196); 
     c = L1971_;
     L1981_=INTDIV(c,d); 
     L199 = L1981_;
     SARR((INTI1)x,i,L199); 
     ;
     L2011_=INTMOD(c,d); 
     c = L2011_;
    }
    after_loop7: ;
   }
  }
 }
 ret_val = x;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INTI1 INTI_u2116735672(INTI1 self, INTI1 x, INTI1 y) {
 INTI1 ret_val;
 INT xl = INT_zero;
 INT yl = INT_zero;
 INT l = INT_zero;
 INT i = INT_zero;
 INT c = INT_zero;
 INTI1 z = ((INTI1) NULL);
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L51_;
 INTI1 L6;
 INT L7;
 INT L81_;
 INT L9;
 INT L101_;
 OB L11;
 INT L12;
 BOOL L131_;
 INT L141_;
 INT L15;
 INT L161_;
 INT L17;
 INT L181_;
 INT L19;
 INT L201_;
 INT L22;
 INT L231_;
 INT L24;
 INT L26;
 INT L271_;
 INT L281_;
 BOOL L291_;
 INT L301_;
 INT L31;
 INT L321_;
 INT L33;
 INT L341_;
 INT L35;
 INT L37;
 INT L381_;
 INT L391_;
 BOOL L401_;
 INT L421_;
 INT L43;
 INT L441_;
 INT L45;
 INT L461_;
 INT L47;
 INT L49;
 INT L501_;
 INT L521_;
 BOOL L531_;
 BOOL L54;
 BOOL L551_;
 INT L571_;
 L1 = ATTR(x,len1);
 L21_=(L1<0)?-L1:L1; 
 xl = L21_;
 L3 = ATTR(y,len1);
 L41_=(L3<0)?-L3:L3; 
 yl = L41_;
 L51_=(xl<yl)?xl:yl; 
 l = L51_;
 i = 0;
 c = 0;
 L81_=(xl>yl)?xl:yl; 
 L9 = L81_;
 L101_=INTPLUS(L9,1); 
 L7 = L101_;
 L12=(sizeof(struct INTI1_struct)+1-sizeof(INT))+(L7)*sizeof(INT);
 L11=ZALLOC_LEAF_BIG(L12);
 if (L11==NULL) FATAL("Unable to allocate more memory");
 memset(L11,0,L12);
 ((OB)L11)->header.tag=INTI1_tag;
#ifdef DESTROY_CHK

   ((OB)L11)->header.destroyed=0;
#endif

 L6 = ((INTI1) L11);
 L6->asize = L7;
 z = L6;
 while (1) {
  L131_=(i)<(l); 
  if (L131_) {
  }
  else {
   goto after_loop;
  }
  L141_=ARR((INTI1)x,i); 
  L15 = L141_;
  L161_=INTPLUS(c,L15); 
  L17 = L161_;
  L181_=ARR((INTI1)y,i); 
  L19 = L181_;
  L201_=INTPLUS(L17,L19); 
  c = L201_;
  L22 = INTI_B;
  L231_=INTMOD(c,L22); 
  L24 = L231_;
  SARR((INTI1)z,i,L24); 
  ;
  L26 = INTI_B;
  L271_=INTDIV(c,L26); 
  c = L271_;
  L281_=INTPLUS(i,1); 
  i = L281_;
 }
 after_loop: ;
 while (1) {
  L291_=(i)<(xl); 
  if (L291_) {
  }
  else {
   goto after_loop1;
  }
  L301_=ARR((INTI1)x,i); 
  L31 = L301_;
  L321_=INTPLUS(c,L31); 
  c = L321_;
  L33 = INTI_B;
  L341_=INTMOD(c,L33); 
  L35 = L341_;
  SARR((INTI1)z,i,L35); 
  ;
  L37 = INTI_B;
  L381_=INTDIV(c,L37); 
  c = L381_;
  L391_=INTPLUS(i,1); 
  i = L391_;
 }
 after_loop1: ;
 while (1) {
  L401_=(i)<(yl); 
  if (L401_) {
  }
  else {
   goto after_loop2;
  }
  L421_=ARR((INTI1)y,i); 
  L43 = L421_;
  L441_=INTPLUS(c,L43); 
  c = L441_;
  L45 = INTI_B;
  L461_=INTMOD(c,L45); 
  L47 = L461_;
  SARR((INTI1)z,i,L47); 
  ;
  L49 = INTI_B;
  L501_=INTDIV(c,L49); 
  c = L501_;
  L521_=INTPLUS(i,1); 
  i = L521_;
 }
 after_loop2: ;
 L531_=(c)==(0); 
 L54 = L531_;
 L551_=!(L54); 
 if (L551_) {
  SARR((INTI1)z,i,c); 
  ;
  L571_=INTPLUS(i,1); 
  i = L571_;
 }
 SATTR(z,len1,i);
 ret_val = z;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INTI1 INTI_u450560926(INTI1 self, INTI1 x, INT y, INT c) {
 INTI1 ret_val;
 INT xl = INT_zero;
 INT i = INT_zero;
 INTI1 z;
 INT L1;
 INT L21_;
 INTI1 L3;
 INT L4;
 INT L51_;
 OB L6;
 INT L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 INT L131_;
 INT L14;
 INT L151_;
 INT L16;
 INT L18;
 INT L191_;
 INT L201_;
 BOOL L221_;
 BOOL L23;
 BOOL L241_;
 INT L261_;
 L1 = ATTR(x,len1);
 L21_=(L1<0)?-L1:L1; 
 xl = L21_;
 i = 0;
 L51_=INTPLUS(xl,1); 
 L4 = L51_;
 L7=(sizeof(struct INTI1_struct)+1-sizeof(INT))+(L4)*sizeof(INT);
 L6=ZALLOC_LEAF_BIG(L7);
 if (L6==NULL) FATAL("Unable to allocate more memory");
 memset(L6,0,L7);
 ((OB)L6)->header.tag=INTI1_tag;
#ifdef DESTROY_CHK

   ((OB)L6)->header.destroyed=0;
#endif

 L3 = ((INTI1) L6);
 L3->asize = L4;
 z = L3;
 while (1) {
  L81_=(i)<(xl); 
  if (L81_) {
  }
  else {
   goto after_loop;
  }
  L91_=ARR((INTI1)x,i); 
  L10 = L91_;
  L111_=INTTIMES(L10,y); 
  L12 = L111_;
  L131_=INTPLUS(c,L12); 
  c = L131_;
  L14 = INTI_B;
  L151_=INTMOD(c,L14); 
  L16 = L151_;
  SARR((INTI1)z,i,L16); 
  ;
  L18 = INTI_B;
  L191_=INTDIV(c,L18); 
  c = L191_;
  L201_=INTPLUS(i,1); 
  i = L201_;
 }
 after_loop: ;
 L221_=(c)==(0); 
 L23 = L221_;
 L241_=!(L23); 
 if (L241_) {
  SARR((INTI1)z,i,c); 
  ;
  L261_=INTPLUS(i,1); 
  i = L261_;
 }
 SATTR(z,len1,i);
 ret_val = z;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ABSTRA12614027(ABSTRA151498155 self, CGEN c) {
 STR ret_val;
 MANGLE m;
 dTP the_tp;
 STR res = ((STR) NULL);
 ARRAYARG L11;
 ARG fi;
 INT L21 = INT_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 MANGLE forbid_self;
 STR forbid_s;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val3;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val4;
 MANGLE forbid_self1;
 STR forbid_s1;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val5;
 STR plus_self5;
 CHAR plus_arg = CHAR_zero;
 STR ret_val6;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val7;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val8;
 STR r;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val9;
 STR plus_self7;
 STR plus_sarg6;
 STR ret_val10;
 INT L31 = INT_zero;
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
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val14;
 STR s2;
 CHAR str_self2 = CHAR_zero;
 STR ret_val15;
 STR create_self2;
 CHAR create_c2 = CHAR_zero;
 STR ret_val16;
 STR r2;
 STR plus_self10;
 STR plus_sarg7;
 STR ret_val17;
 STR plus_self11;
 STR plus_sarg8;
 STR ret_val18;
 STR plus_self12;
 INT plus_arg3 = INT_zero;
 STR ret_val19;
 STR s3;
 INT str_self3 = INT_zero;
 STR ret_val20;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val21;
 FSTR str_self4;
 STR ret_val22;
 STR plus_self13;
 CHAR plus_arg4 = CHAR_zero;
 STR ret_val23;
 STR s4;
 CHAR str_self5 = CHAR_zero;
 STR ret_val24;
 STR create_self3;
 CHAR create_c3 = CHAR_zero;
 STR ret_val25;
 STR r3;
 STR plus_self14;
 STR plus_sarg9;
 STR ret_val26;
 STR plus_self15;
 STR plus_sarg10;
 STR ret_val27;
 STR plus_self16;
 STR plus_sarg11;
 STR ret_val28;
 STR plus_self17;
 STR plus_sarg12;
 STR ret_val29;
 STR plus_self18;
 STR plus_sarg13;
 STR ret_val30;
 STR plus_self19;
 STR plus_sarg14;
 STR ret_val31;
 STR plus_self20;
 STR plus_sarg15;
 STR ret_val32;
 STR plus_self21;
 STR plus_sarg16;
 STR ret_val33;
 STR plus_self22;
 STR plus_sarg17;
 STR ret_val34;
 STR plus_self23;
 STR plus_sarg18;
 STR ret_val35;
 STR plus_self24;
 STR plus_sarg19;
 STR ret_val36;
 STR plus_self25;
 STR plus_sarg20;
 STR ret_val37;
 STR plus_self26;
 STR plus_sarg21;
 STR ret_val38;
 STR plus_self27;
 STR plus_sarg22;
 STR ret_val39;
 STR plus_self28;
 STR plus_sarg23;
 STR ret_val40;
 STR plus_self29;
 STR plus_sarg24;
 STR ret_val41;
 STR plus_self30;
 STR plus_sarg25;
 STR ret_val42;
 STR plus_self31;
 STR plus_sarg26;
 STR ret_val43;
 STR plus_self32;
 STR plus_sarg27;
 STR ret_val44;
 STR plus_self33;
 STR plus_sarg28;
 STR ret_val45;
 STR plus_self34;
 STR plus_sarg29;
 STR ret_val46;
 STR plus_self35;
 STR plus_sarg30;
 STR ret_val47;
 STR plus_self36;
 STR plus_sarg31;
 STR ret_val48;
 STR plus_self37;
 STR plus_sarg32;
 STR ret_val49;
 extern STR S_entry1;
 extern STR typedefstruct;
 extern STR S_frame_struct1;
 extern STR S_frame_struct2;
 extern STR INTstate;
 STR L4;
 INT L5;
 OB L6;
 INT L7;
 extern STR self9;
 INT L91_br;
 ARG aL91_;
 INT L101_;
 ARG L12;
 STR L13;
 INT L14;
 OB L15;
 INT L16;
 STR L18;
 INT L19;
 OB L20;
 INT L22;
 extern STR arg21;
 INT L24;
 BOOL L25;
 BOOL L261_;
 STR L27;
 INT L28;
 OB L29;
 INT L30;
 extern STR Formalargument;
 extern STR name55;
 INT L331_;
 extern STR name183;
 extern STR S_frame6;
 extern STR Dispat1213124981;
 extern STR name55;
 extern STR typedefstruct;
 extern STR S_entry2;
 extern STR const3;
 BOOL L34;
 BOOL L351_;
 extern STR void1;
 extern STR iter4;
 extern STR S_frameframe;
 extern STR struct139248200;
 extern STR name127;
 extern STR constv18267182;
 extern STR name184;
 extern STR S_entry3;
 m = ATTR(c,mangler);
 the_tp = ATTR(ATTR(self,sig1),tp);
 forbid_self = m;
 plus_self = MANGLE119219986(m, ((OB) ATTR(self,sig1)), ((OB) NULL));
 plus_sarg = ((STR) &S_entry1);
 ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
 forbid_s = ret_val1;
 SATTR(forbid_self,forbidden,FSETST1404644833(ATTR(forbid_self,forbidden), forbid_s));
 plus_self1 = ((STR) &typedefstruct);
 plus_sarg1 = MANGLE119219986(m, ((OB) ATTR(self,sig1)), ((OB) NULL));
 ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
 plus_self2 = ret_val2;
 plus_sarg2 = ((STR) &S_frame_struct1);
 ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
 res = ret_val3;
 forbid_self1 = m;
 plus_self3 = MANGLE119219986(m, ((OB) ATTR(self,sig1)), ((OB) NULL));
 plus_sarg3 = ((STR) &S_frame_struct2);
 ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
 forbid_s1 = ret_val4;
 SATTR(forbid_self1,forbidden,FSETST1404644833(ATTR(forbid_self1,forbidden), forbid_s1));
 plus_self4 = res;
 plus_sarg4 = ((STR) &INTstate);
 ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
 res = ret_val5;
 plus_self5 = res;
 plus_arg = ' ';
 str_self = plus_arg;
 create_self = ((STR) NULL);
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
 ret_val8 = r;
 ret_val7 = ret_val8;
 s = ret_val7;
 ret_val6 = STR_ap1077157958(plus_self5, s, TRUE);
 plus_self6 = ret_val6;
 plus_sarg5 = MANGLE119219986(m, ((OB) ATTR(ATTR(self,sig1),tp)), ((OB) NULL));
 ret_val9 = STR_ap2004550586(plus_self6, plus_sarg5);
 plus_self7 = ret_val9;
 plus_sarg6 = ((STR) &self9);
 ret_val10 = STR_ap2004550586(plus_self7, plus_sarg6);
 res = ret_val10;
 {
  BOOL f_L91_ = TRUE;
  BOOL f_L101_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = ATTR(ATTR(self,sig1),args);
  L21 = 1;
  L91_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
  L101_=L21-1; 
  while (1) {
   if(L31>=L91_br)  goto after_loop; 
   aL91_=ARR((ARRAYARG)L11,L31); 
   fi = aL91_;
   plus_self8 = res;
   plus_arg1 = ' ';
   str_self1 = plus_arg1;
   create_self1 = ((STR) NULL);
   create_c1 = str_self1;
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
   r1 = L13;
   SARR((STR)r1,0,create_c1); 
   ;
   ret_val13 = r1;
   ret_val12 = ret_val13;
   s1 = ret_val12;
   ret_val11 = STR_ap1077157958(plus_self8, s1, TRUE);
   plus_self10 = ret_val11;
   plus_self9 = MANGLE119219986(m, ((OB) ATTR(fi,tp)), ((OB) NULL));
   plus_arg2 = ' ';
   str_self2 = plus_arg2;
   create_self2 = ((STR) NULL);
   create_c2 = str_self2;
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
   r2 = L18;
   SARR((STR)r2,0,create_c2); 
   ;
   ret_val16 = r2;
   ret_val15 = ret_val16;
   s2 = ret_val15;
   ret_val14 = STR_ap1077157958(plus_self9, s2, TRUE);
   plus_sarg7 = ABSTRA607493838(self, fi, ret_val14);
   ret_val17 = STR_ap2004550586(plus_self10, plus_sarg7);
   plus_self11 = ret_val17;
   plus_sarg8 = ((STR) &arg21);
   ret_val18 = STR_ap2004550586(plus_self11, plus_sarg8);
   plus_self12 = ret_val18;
   L101_++; 
   plus_arg3 = L101_;
   str_self3 = plus_arg3;
   clear_self = buf1;
   L25 = (clear_self==((FSTR) NULL));
   L261_=!(L25); 
   if (L261_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self3;
   str_in_s = buf1;
   ret_val21 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val21;
   str_self4 = buf1;
   ret_val22 = STR_fr1097270334(((STR) NULL), str_self4);
   ret_val20 = ret_val22;
   s3 = ret_val20;
   ret_val19 = STR_ap1077157958(plus_self12, s3, TRUE);
   plus_self13 = ret_val19;
   plus_arg4 = ';';
   str_self5 = plus_arg4;
   create_self3 = ((STR) NULL);
   create_c3 = str_self5;
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
   r3 = L27;
   SARR((STR)r3,0,create_c3); 
   ;
   ret_val25 = r3;
   ret_val24 = ret_val25;
   s4 = ret_val24;
   ret_val23 = STR_ap1077157958(plus_self13, s4, TRUE);
   res = ret_val23;
   plus_self14 = res;
   plus_sarg9 = ((STR) &Formalargument);
   ret_val26 = STR_ap2004550586(plus_self14, plus_sarg9);
   plus_self15 = ret_val26;
   plus_sarg10 = ARG_strrSTR(fi);
   ret_val27 = STR_ap2004550586(plus_self15, plus_sarg10);
   plus_self16 = ret_val27;
   plus_sarg11 = ((STR) &name55);
   ret_val28 = STR_ap2004550586(plus_self16, plus_sarg11);
   res = ret_val28;
   L331_=INTPLUS(L31,1); 
   L31 = L331_;
  }
 }
 after_loop: ;
 plus_self17 = res;
 plus_sarg12 = ((STR) &name183);
 ret_val29 = STR_ap2004550586(plus_self17, plus_sarg12);
 plus_self18 = ret_val29;
 plus_sarg13 = MANGLE119219986(m, ((OB) ATTR(self,sig1)), ((OB) NULL));
 ret_val30 = STR_ap2004550586(plus_self18, plus_sarg13);
 plus_self19 = ret_val30;
 plus_sarg14 = ((STR) &S_frame6);
 ret_val31 = STR_ap2004550586(plus_self19, plus_sarg14);
 res = ret_val31;
 plus_self20 = res;
 plus_sarg15 = ((STR) &Dispat1213124981);
 ret_val32 = STR_ap2004550586(plus_self20, plus_sarg15);
 plus_self21 = ret_val32;
 plus_sarg16 = ATTR(ATTR(self,sig1),str);
 ret_val33 = STR_ap2004550586(plus_self21, plus_sarg16);
 plus_self22 = ret_val33;
 plus_sarg17 = ((STR) &name55);
 ret_val34 = STR_ap2004550586(plus_self22, plus_sarg17);
 res = ret_val34;
 plus_self23 = res;
 plus_sarg18 = ((STR) &typedefstruct);
 ret_val35 = STR_ap2004550586(plus_self23, plus_sarg18);
 plus_self24 = ret_val35;
 plus_sarg19 = MANGLE119219986(m, ((OB) ATTR(self,sig1)), ((OB) NULL));
 ret_val36 = STR_ap2004550586(plus_self24, plus_sarg19);
 plus_self25 = ret_val36;
 plus_sarg20 = ((STR) &S_entry2);
 ret_val37 = STR_ap2004550586(plus_self25, plus_sarg20);
 plus_self26 = ret_val37;
 plus_sarg21 = ((STR) &const3);
 ret_val38 = STR_ap2004550586(plus_self26, plus_sarg21);
 res = ret_val38;
 L34 = (ATTR(ATTR(self,sig1),ret)==((dTP) NULL));
 L351_=!(L34); 
 if (L351_) {
  plus_self27 = res;
  plus_sarg22 = MANGLE119219986(m, ((OB) ATTR(ATTR(self,sig1),ret)), ((OB) NULL));
  ret_val39 = STR_ap2004550586(plus_self27, plus_sarg22);
  res = ret_val39;
 }
 else {
  plus_self28 = res;
  plus_sarg23 = ((STR) &void1);
  ret_val40 = STR_ap2004550586(plus_self28, plus_sarg23);
  res = ret_val40;
 }
 plus_self29 = res;
 plus_sarg24 = ((STR) &iter4);
 ret_val41 = STR_ap2004550586(plus_self29, plus_sarg24);
 plus_self30 = ret_val41;
 plus_sarg25 = MANGLE119219986(m, ((OB) ATTR(self,sig1)), ((OB) NULL));
 ret_val42 = STR_ap2004550586(plus_self30, plus_sarg25);
 plus_self31 = ret_val42;
 plus_sarg26 = ((STR) &S_frameframe);
 ret_val43 = STR_ap2004550586(plus_self31, plus_sarg26);
 res = ret_val43;
 if (ATTR(c,func_tables)) {
  plus_self32 = res;
  plus_sarg27 = ((STR) &struct139248200);
  ret_val44 = STR_ap2004550586(plus_self32, plus_sarg27);
  res = ret_val44;
 }
 plus_self33 = res;
 plus_sarg28 = ((STR) &name127);
 ret_val45 = STR_ap2004550586(plus_self33, plus_sarg28);
 res = ret_val45;
 plus_self34 = res;
 plus_sarg29 = ((STR) &constv18267182);
 ret_val46 = STR_ap2004550586(plus_self34, plus_sarg29);
 plus_self35 = ret_val46;
 plus_sarg30 = ((STR) &name184);
 ret_val47 = STR_ap2004550586(plus_self35, plus_sarg30);
 plus_self36 = ret_val47;
 plus_sarg31 = MANGLE119219986(m, ((OB) ATTR(self,sig1)), ((OB) NULL));
 ret_val48 = STR_ap2004550586(plus_self36, plus_sarg31);
 plus_self37 = ret_val48;
 plus_sarg32 = ((STR) &S_entry3);
 ret_val49 = STR_ap2004550586(plus_self37, plus_sarg32);
 res = ret_val49;
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ABSTRA1464296195(ABSTRACT_LAYOUT self) {
 return ATTR(self,str);
}


#undef IS_ITER
#define IS_ITER 0

STR ABSTRA1984535400(ABSTRA151498155 self) {
 return ATTR(self,str);
}


#undef IS_ITER
#define IS_ITER 0

STR ABSTRA353314648(ABSTRACT_LAYOUT self, CGEN c) {
 STR ret_val;
 MANGLE m;
 STR decl_str;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 MANGLE forbid_self;
 STR forbid_s;
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
 extern STR S_struct2;
 extern STR typedefstruct;
 extern STR S_struct3;
 extern STR OB_HEADERheader;
 extern STR name183;
 extern STR name185;
 m = ATTR(c,mangler);
 forbid_self = m;
 plus_self = MANGLE119219986(m, ((OB) ATTR(self,tp)), ((OB) NULL));
 plus_sarg = ((STR) &S_struct2);
 ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
 forbid_s = ret_val1;
 SATTR(forbid_self,forbidden,FSETST1404644833(ATTR(forbid_self,forbidden), forbid_s));
 plus_self1 = ((STR) &typedefstruct);
 plus_sarg1 = MANGLE119219986(m, ((OB) ATTR(self,tp)), ((OB) NULL));
 ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
 plus_self2 = ret_val2;
 plus_sarg2 = ((STR) &S_struct3);
 ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
 plus_self3 = ret_val3;
 plus_sarg3 = ((STR) &OB_HEADERheader);
 ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
 plus_self4 = ret_val4;
 plus_sarg4 = ((STR) &name183);
 ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
 plus_self5 = ret_val5;
 plus_sarg5 = MANGLE119219986(m, ((OB) ATTR(self,tp)), ((OB) NULL));
 ret_val6 = STR_ap2004550586(plus_self5, plus_sarg5);
 plus_self6 = ret_val6;
 plus_sarg6 = ((STR) &name185);
 ret_val7 = STR_ap2004550586(plus_self6, plus_sarg6);
 decl_str = ret_val7;
 ret_val = decl_str;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ABSTRA607493838(ABSTRA151498155 self, ARG e, STR s) {
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

STR ARG_LA635972259(ARG_LAYOUT self) {
 return ATTR(self,str);
}


#undef IS_ITER
#define IS_ITER 0

STR ARG_LA807737867(ARG_LAYOUT self, CGEN c) {
 STR ret_val;
 MANGLE m;
 STR res;
 ARRAYARG L11;
 INT L21 = INT_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 STR plus_self2;
 CHAR plus_arg = CHAR_zero;
 STR ret_val3;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val4;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val5;
 STR r;
 STR plus_self3;
 STR plus_sarg2;
 STR ret_val6;
 STR plus_self4;
 STR plus_sarg3;
 STR ret_val7;
 STR plus_self5;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val8;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val9;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val10;
 STR r1;
 INT L31 = INT_zero;
 STR plus_self6;
 STR plus_sarg4;
 STR ret_val11;
 STR plus_self7;
 STR plus_sarg5;
 STR ret_val12;
 STR plus_self8;
 INT plus_arg2 = INT_zero;
 STR ret_val13;
 STR s2;
 INT str_self2 = INT_zero;
 STR ret_val14;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val15;
 FSTR str_self3;
 STR ret_val16;
 STR plus_self9;
 STR plus_sarg6;
 STR ret_val17;
 STR plus_self10;
 STR plus_sarg7;
 STR ret_val18;
 STR plus_self11;
 STR plus_sarg8;
 STR ret_val19;
 STR plus_self12;
 STR plus_sarg9;
 STR ret_val20;
 extern STR typedefstruct;
 extern STR S_arg_1067136973;
 STR L4;
 INT L5;
 OB L6;
 INT L7;
 extern STR self9;
 INT L101_;
 STR L12;
 INT L13;
 OB L14;
 INT L15;
 ARG L17;
 INT L91_br;
 ARG aL91_;
 ARG L18;
 extern STR arg4;
 INT L19;
 BOOL L20;
 BOOL L221_;
 extern STR name13;
 INT L231_;
 extern STR name186;
 extern STR S_arg_frame;
 m = ATTR(c,mangler);
 plus_self = ((STR) &typedefstruct);
 plus_sarg = MANGLE119219986(m, ((OB) ATTR(self,sig1)), ((OB) ATTR(self,sig1)));
 ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
 plus_self1 = ret_val1;
 plus_sarg1 = ((STR) &S_arg_1067136973);
 ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
 res = ret_val2;
 plus_self2 = res;
 plus_arg = ' ';
 str_self = plus_arg;
 create_self = ((STR) NULL);
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
 ret_val3 = STR_ap1077157958(plus_self2, s, TRUE);
 plus_self3 = ret_val3;
 plus_sarg2 = MANGLE119219986(m, ((OB) ATTR(ATTR(self,sig1),tp)), ((OB) ATTR(self,sig1)));
 ret_val6 = STR_ap2004550586(plus_self3, plus_sarg2);
 plus_self4 = ret_val6;
 plus_sarg3 = ((STR) &self9);
 ret_val7 = STR_ap2004550586(plus_self4, plus_sarg3);
 res = ret_val7;
 {
  BOOL f_L91_ = TRUE;
  BOOL f_L101_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L21 = 1;
  L101_=L21-1; 
  while (1) {
   plus_self5 = res;
   plus_arg1 = ' ';
   str_self1 = plus_arg1;
   create_self1 = ((STR) NULL);
   create_c1 = str_self1;
   L13 = 1;
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
   r1 = L12;
   SARR((STR)r1,0,create_c1); 
   ;
   ret_val10 = r1;
   ret_val9 = ret_val10;
   s1 = ret_val9;
   ret_val8 = STR_ap1077157958(plus_self5, s1, TRUE);
   plus_self6 = ret_val8;
   if (f_L91_) {
    f_L91_ = FALSE;
    L11 = ATTR(ATTR(self,sig1),args);
    L91_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
   }
   if(L31>=L91_br)  goto after_loop; 
   aL91_=ARR((ARRAYARG)L11,L31); 
   L18=aL91_;
   plus_sarg4 = MANGLE119219986(m, ((OB) ATTR(L18,tp)), ((OB) ATTR(self,sig1)));
   ret_val11 = STR_ap2004550586(plus_self6, plus_sarg4);
   plus_self7 = ret_val11;
   plus_sarg5 = ((STR) &arg4);
   ret_val12 = STR_ap2004550586(plus_self7, plus_sarg5);
   plus_self8 = ret_val12;
   L101_++; 
   plus_arg2 = L101_;
   str_self2 = plus_arg2;
   clear_self = buf1;
   L20 = (clear_self==((FSTR) NULL));
   L221_=!(L20); 
   if (L221_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self2;
   str_in_s = buf1;
   ret_val15 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val15;
   str_self3 = buf1;
   ret_val16 = STR_fr1097270334(((STR) NULL), str_self3);
   ret_val14 = ret_val16;
   s2 = ret_val14;
   ret_val13 = STR_ap1077157958(plus_self8, s2, TRUE);
   plus_self9 = ret_val13;
   plus_sarg6 = ((STR) &name13);
   ret_val17 = STR_ap2004550586(plus_self9, plus_sarg6);
   res = ret_val17;
   L231_=INTPLUS(L31,1); 
   L31 = L231_;
  }
 }
 after_loop: ;
 plus_self10 = res;
 plus_sarg7 = ((STR) &name186);
 ret_val18 = STR_ap2004550586(plus_self10, plus_sarg7);
 plus_self11 = ret_val18;
 plus_sarg8 = MANGLE119219986(m, ((OB) ATTR(self,sig1)), ((OB) ATTR(self,sig1)));
 ret_val19 = STR_ap2004550586(plus_self11, plus_sarg8);
 plus_self12 = ret_val19;
 plus_sarg9 = ((STR) &S_arg_frame);
 ret_val20 = STR_ap2004550586(plus_self12, plus_sarg9);
 res = ret_val20;
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR BOUND_1019937534(BOUND_1007407331 self, ARG e, STR s) {
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

STR BOUND_1245789017(BOUND_2140373124 self) {
 return ATTR(self,str);
}


#undef IS_ITER
#define IS_ITER 0

STR BOUND_138602513(BOUND_1677815802 self, ARG e, STR s) {
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

STR BOUND_1401874693(BOUND_1007407331 self, CGEN c) {
 STR ret_val;
 MANGLE m;
 STR name111;
 STR res;
 BOOL hot = BOOL_zero;
 ARRAYINT L11;
 INT a = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 ARG arg;
 IDENT ident1 = IDENT_zero;
 ARRAYINT L31;
 INT i1 = INT_zero;
 ARG bnd_a;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 MANGLE forbid_self;
 STR forbid_s;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 MANGLE forbid_self1;
 STR forbid_s1;
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
 INT L41 = INT_zero;
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
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val15;
 STR s2;
 CHAR str_self2 = CHAR_zero;
 STR ret_val16;
 STR create_self2;
 CHAR create_c2 = CHAR_zero;
 STR ret_val17;
 STR r2;
 STR plus_self11;
 STR plus_sarg8;
 STR ret_val18;
 STR plus_self12;
 STR plus_sarg9;
 STR ret_val19;
 STR plus_self13;
 INT plus_arg3 = INT_zero;
 STR ret_val20;
 STR s3;
 INT str_self3 = INT_zero;
 STR ret_val21;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val22;
 FSTR str_self4;
 STR ret_val23;
 STR plus_self14;
 STR plus_sarg10;
 STR ret_val24;
 STR plus_self15;
 INT plus_arg4 = INT_zero;
 STR ret_val25;
 STR s4;
 INT str_self5 = INT_zero;
 STR ret_val26;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val27;
 FSTR str_self6;
 STR ret_val28;
 STR plus_self16;
 STR plus_sarg11;
 STR ret_val29;
 STR plus_self17;
 CHAR plus_arg5 = CHAR_zero;
 STR ret_val30;
 STR s5;
 CHAR str_self7 = CHAR_zero;
 STR ret_val31;
 STR create_self3;
 CHAR create_c3 = CHAR_zero;
 STR ret_val32;
 STR r3;
 STR plus_self18;
 STR plus_sarg12;
 STR ret_val33;
 STR plus_self19;
 STR plus_sarg13;
 STR ret_val34;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val35 = BOOL_zero;
 STR plus_self20;
 STR plus_sarg14;
 STR ret_val36;
 STR plus_self21;
 STR plus_sarg15;
 STR ret_val37;
 STR plus_self22;
 STR plus_sarg16;
 STR ret_val38;
 STR plus_self23;
 STR plus_sarg17;
 STR ret_val39;
 STR plus_self24;
 STR plus_sarg18;
 STR ret_val40;
 STR plus_self25;
 STR plus_sarg19;
 STR ret_val41;
 STR plus_self26;
 CHAR plus_arg6 = CHAR_zero;
 STR ret_val42;
 STR s6;
 CHAR str_self8 = CHAR_zero;
 STR ret_val43;
 STR create_self4;
 CHAR create_c4 = CHAR_zero;
 STR ret_val44;
 STR r4;
 STR plus_self27;
 STR plus_sarg20;
 STR ret_val45;
 STR plus_self28;
 STR plus_sarg21;
 STR ret_val46;
 STR plus_self29;
 STR plus_sarg22;
 STR ret_val47;
 STR plus_self30;
 STR plus_sarg23;
 STR ret_val48;
 STR plus_self31;
 STR plus_sarg24;
 STR ret_val49;
 STR plus_self32;
 STR plus_sarg25;
 STR ret_val50;
 STR plus_self33;
 STR plus_sarg26;
 STR ret_val51;
 STR plus_self34;
 STR plus_sarg27;
 STR ret_val52;
 INT L51 = INT_zero;
 STR plus_self35;
 CHAR plus_arg7 = CHAR_zero;
 STR ret_val53;
 STR s7;
 CHAR str_self9 = CHAR_zero;
 STR ret_val54;
 STR create_self5;
 CHAR create_c5 = CHAR_zero;
 STR ret_val55;
 STR r5;
 STR plus_self36;
 STR plus_sarg28;
 STR ret_val56;
 STR plus_self37;
 STR plus_sarg29;
 STR ret_val57;
 STR plus_self38;
 INT plus_arg8 = INT_zero;
 STR ret_val58;
 STR s8;
 INT str_self10 = INT_zero;
 STR ret_val59;
 FSTR buf3 = ((FSTR) NULL);
 FSTR clear_self2;
 INT str_in_self2 = INT_zero;
 FSTR str_in_s2;
 FSTR ret_val60;
 FSTR str_self11;
 STR ret_val61;
 STR plus_self39;
 STR plus_sarg30;
 STR ret_val62;
 STR plus_self40;
 CHAR plus_arg9 = CHAR_zero;
 STR ret_val63;
 STR s9;
 CHAR str_self12 = CHAR_zero;
 STR ret_val64;
 STR create_self6;
 CHAR create_c6 = CHAR_zero;
 STR ret_val65;
 STR r6;
 STR plus_self41;
 STR plus_sarg31;
 STR ret_val66;
 STR plus_self42;
 STR plus_sarg32;
 STR ret_val67;
 STR plus_self43;
 INT plus_arg10 = INT_zero;
 STR ret_val68;
 STR s10;
 INT str_self13 = INT_zero;
 STR ret_val69;
 FSTR buf4 = ((FSTR) NULL);
 FSTR clear_self3;
 INT str_in_self3 = INT_zero;
 FSTR str_in_s3;
 FSTR ret_val70;
 FSTR str_self14;
 STR ret_val71;
 STR plus_self44;
 STR plus_sarg33;
 STR ret_val72;
 PROG psather_self;
 BOOL ret_val73 = BOOL_zero;
 STR plus_self45;
 STR plus_sarg34;
 STR ret_val74;
 STR plus_self46;
 STR plus_sarg35;
 STR ret_val75;
 STR plus_self47;
 STR plus_sarg36;
 STR ret_val76;
 STR plus_self48;
 STR plus_sarg37;
 STR ret_val77;
 extern STR S_iter_frame;
 extern STR S_iter2041871633;
 extern STR typedefstruct;
 extern STR S_iter_ob_struct2;
 extern STR OB_HEADERheader;
 extern STR INTstate1;
 INT L61_br;
 INT aL61_;
 INT L71_;
 INT L8;
 INT L9;
 BOOL L101_;
 STR L12;
 INT L13;
 OB L14;
 INT L15;
 STR L17;
 INT L18;
 OB L19;
 INT L20;
 ARRAYARG L23;
 INT L241_;
 INT L25;
 ARG L261_;
 BOOL L27;
 BOOL L281_;
 ARRAYBOOL L29;
 INT L301_;
 INT L32;
 BOOL L331_;
 extern STR name10;
 STR L34;
 INT L35;
 OB L36;
 INT L37;
 extern STR hotarg1;
 BOOL L39;
 BOOL L401_;
 extern STR oncearg1;
 BOOL L42;
 BOOL L431_;
 extern STR name13;
 INT L441_;
 BOOL L45;
 BOOL L461_;
 STR L47;
 INT L48;
 OB L49;
 INT L50;
 extern STR ret_val411;
 extern STR callb;
 extern STR name10;
 extern STR iter_frame;
 extern STR name10;
 extern STR S_fram362955677;
 BOOL L53;
 BOOL L541_;
 STR L55;
 INT L56;
 OB L57;
 INT L58;
 extern STR void13;
 extern STR callstruct;
 extern STR S_iter_ob_struct3;
 extern STR struct139248200;
 extern STR name127;
 extern STR size_tsize;
 INT L601_br;
 INT rL601_;
 INT L62;
 ARRAYINT L63;
 INT L641_;
 INT L65;
 BOOL L661_;
 dTP L67;
 BOOL L68;
 BOOL L691_;
 STR L70;
 INT L72;
 OB L73;
 INT L74;
 extern STR bound_arg2;
 BOOL L76;
 BOOL L771_;
 extern STR name13;
 ARRAYARG L78;
 ARRAYINT L79;
 INT L801_;
 INT L81;
 INT L821_;
 INT L83;
 ARG L841_;
 STR L85;
 INT L86;
 OB L87;
 INT L88;
 extern STR bound_arg2;
 BOOL L90;
 BOOL L911_;
 extern STR name13;
 INT L921_;
 BOOL L93;
 extern STR voidex;
 extern STR name186;
 extern STR S_iter_ob3;
 m = ATTR(c,mangler);
 name111 = MANGLE119219986(m, ((OB) ATTR(self,e)), ((OB) NULL));
 forbid_self = m;
 plus_self = name111;
 plus_sarg = ((STR) &S_iter_frame);
 ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
 forbid_s = ret_val1;
 SATTR(forbid_self,forbidden,FSETST1404644833(ATTR(forbid_self,forbidden), forbid_s));
 forbid_self1 = m;
 plus_self1 = name111;
 plus_sarg1 = ((STR) &S_iter2041871633);
 ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
 forbid_s1 = ret_val2;
 SATTR(forbid_self1,forbidden,FSETST1404644833(ATTR(forbid_self1,forbidden), forbid_s1));
 plus_self2 = ((STR) &typedefstruct);
 plus_sarg2 = name111;
 ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
 plus_self3 = ret_val3;
 plus_sarg3 = ((STR) &S_iter_ob_struct2);
 ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
 res = ret_val4;
 plus_self4 = res;
 plus_sarg4 = ((STR) &OB_HEADERheader);
 ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
 res = ret_val5;
 plus_self5 = res;
 plus_sarg5 = ((STR) &INTstate1);
 ret_val6 = STR_ap2004550586(plus_self5, plus_sarg5);
 res = ret_val6;
 hot = FALSE;
 {
  BOOL f_L61_ = TRUE;
  BOOL f_L71_ = TRUE;
  /* loop index variable */
  L41 = 0;
  L11 = ATTR(ATTR(self,e),unbnd_args);
  L21 = 0;
  L61_br=L11==NULL?0:ASIZE((ARRAYINT)L11); 
  L71_=L21-1; 
  while (1) {
   if(L41>=L61_br)  goto after_loop; 
   aL61_=ARR((ARRAYINT)L11,L41); 
   a = aL61_;
   L71_++; 
   i = L71_;
   L101_=(a)==(0); 
   if (L101_) {
    plus_self6 = res;
    plus_arg = ' ';
    str_self = plus_arg;
    create_self = ((STR) NULL);
    create_c = str_self;
    L13 = 1;
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
    SARR((STR)r,0,create_c); 
    ;
    ret_val9 = r;
    ret_val8 = ret_val9;
    s = ret_val8;
    ret_val7 = STR_ap1077157958(plus_self6, s, TRUE);
    plus_self7 = ret_val7;
    plus_sarg6 = MANGLE119219986(m, ((OB) ATTR(ATTR(ATTR(self,e),fun),tp)), ((OB) NULL));
    ret_val10 = STR_ap2004550586(plus_self7, plus_sarg6);
    plus_self8 = ret_val10;
    plus_arg1 = ' ';
    str_self1 = plus_arg1;
    create_self1 = ((STR) NULL);
    create_c1 = str_self1;
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
    r1 = L17;
    SARR((STR)r1,0,create_c1); 
    ;
    ret_val13 = r1;
    ret_val12 = ret_val13;
    s1 = ret_val12;
    ret_val11 = STR_ap1077157958(plus_self8, s1, TRUE);
    res = ret_val11;
   }
   else {
    L23 = ATTR(ATTR(ATTR(self,e),fun),args);
    L241_=INTMINUS(a,1); 
    L25 = L241_;
    L261_=(ARG)ARR((ARRAYARG)L23,L25); 
    arg = L261_;
    L27 = (ATTR(ATTR(ATTR(self,e),fun),hot)==((ARRAYBOOL) NULL));
    L281_=!(L27); 
    if (L281_) {
     L29 = ATTR(ATTR(ATTR(self,e),fun),hot);
     L301_=INTMINUS(a,1); 
     L32 = L301_;
     L331_=ARR((ARRAYBOOL)L29,L32); 
     hot = L331_;
    }
    plus_self9 = res;
    plus_sarg7 = ((STR) &name10);
    ret_val14 = STR_ap2004550586(plus_self9, plus_sarg7);
    plus_self11 = ret_val14;
    plus_self10 = MANGLE119219986(m, ((OB) ATTR(arg,tp)), ((OB) NULL));
    plus_arg2 = ' ';
    str_self2 = plus_arg2;
    create_self2 = ((STR) NULL);
    create_c2 = str_self2;
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
    r2 = L34;
    SARR((STR)r2,0,create_c2); 
    ;
    ret_val17 = r2;
    ret_val16 = ret_val17;
    s2 = ret_val16;
    ret_val15 = STR_ap1077157958(plus_self10, s2, TRUE);
    plus_sarg8 = BOUND_1019937534(self, arg, ret_val15);
    ret_val18 = STR_ap2004550586(plus_self11, plus_sarg8);
    res = ret_val18;
   }
   if (hot) {
    plus_self12 = res;
    plus_sarg9 = ((STR) &hotarg1);
    ret_val19 = STR_ap2004550586(plus_self12, plus_sarg9);
    plus_self13 = ret_val19;
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
    ret_val22 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
    buf1 = ret_val22;
    str_self4 = buf1;
    ret_val23 = STR_fr1097270334(((STR) NULL), str_self4);
    ret_val21 = ret_val23;
    s3 = ret_val21;
    ret_val20 = STR_ap1077157958(plus_self13, s3, TRUE);
    res = ret_val20;
   }
   else {
    plus_self14 = res;
    plus_sarg10 = ((STR) &oncearg1);
    ret_val24 = STR_ap2004550586(plus_self14, plus_sarg10);
    plus_self15 = ret_val24;
    plus_arg4 = i;
    str_self5 = plus_arg4;
    clear_self1 = buf2;
    L42 = (clear_self1==((FSTR) NULL));
    L431_=!(L42); 
    if (L431_) {
     SATTR(clear_self1,loc,0);
    }
    str_in_self1 = str_self5;
    str_in_s1 = buf2;
    ret_val27 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
    buf2 = ret_val27;
    str_self6 = buf2;
    ret_val28 = STR_fr1097270334(((STR) NULL), str_self6);
    ret_val26 = ret_val28;
    s4 = ret_val26;
    ret_val25 = STR_ap1077157958(plus_self15, s4, TRUE);
    res = ret_val25;
   }
   plus_self16 = res;
   plus_sarg11 = ((STR) &name13);
   ret_val29 = STR_ap2004550586(plus_self16, plus_sarg11);
   res = ret_val29;
   L441_=INTPLUS(L41,1); 
   L41 = L441_;
  }
 }
 after_loop: ;
 L45 = (ATTR(ATTR(ATTR(self,e),fun),ret)==((dTP) NULL));
 L461_=!(L45); 
 if (L461_) {
  plus_self17 = res;
  plus_arg5 = ' ';
  str_self7 = plus_arg5;
  create_self3 = ((STR) NULL);
  create_c3 = str_self7;
  L48 = 1;
  L50=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L48)*sizeof(CHAR);
  L49=ZALLOC_LEAF_BIG(L50);
  if (L49==NULL) FATAL("Unable to allocate more memory");
  memset(L49,0,L50);
  ((OB)L49)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L49)->header.destroyed=0;
#endif

  L47 = ((STR) L49);
  L47->asize = L48;
  r3 = L47;
  SARR((STR)r3,0,create_c3); 
  ;
  ret_val32 = r3;
  ret_val31 = ret_val32;
  s5 = ret_val31;
  ret_val30 = STR_ap1077157958(plus_self17, s5, TRUE);
  plus_self18 = ret_val30;
  plus_sarg12 = MANGLE119219986(m, ((OB) ATTR(ATTR(ATTR(self,e),fun),ret)), ((OB) NULL));
  ret_val33 = STR_ap2004550586(plus_self18, plus_sarg12);
  res = ret_val33;
  plus_self19 = res;
  plus_sarg13 = ((STR) &ret_val411);
  ret_val34 = STR_ap2004550586(plus_self19, plus_sarg13);
  res = ret_val34;
 }
 ident1 = IDENT_1150413730(IDENT_zero, ((STR) &callb));
 is_eq_self = ATTR(ATTR(ATTR(self,e),fun),name1);
 is_eq_i = ident1;
 ret_val35 = (is_eq_self.str==is_eq_i.str);
 if (ret_val35) {
  plus_self20 = res;
  plus_sarg14 = ((STR) &name10);
  ret_val36 = STR_ap2004550586(plus_self20, plus_sarg14);
  plus_self21 = ret_val36;
  plus_sarg15 = MANGLE119219986(m, ((OB) ATTR(ATTR(ATTR(self,e),fun),tp)), ((OB) NULL));
  ret_val37 = STR_ap2004550586(plus_self21, plus_sarg15);
  plus_self22 = ret_val37;
  plus_sarg16 = ((STR) &iter_frame);
  ret_val38 = STR_ap2004550586(plus_self22, plus_sarg16);
  res = ret_val38;
 }
 else {
  plus_self23 = res;
  plus_sarg17 = ((STR) &name10);
  ret_val39 = STR_ap2004550586(plus_self23, plus_sarg17);
  plus_self24 = ret_val39;
  plus_sarg18 = MANGLE119219986(m, ((OB) ATTR(ATTR(self,e),fun)), ((OB) NULL));
  ret_val40 = STR_ap2004550586(plus_self24, plus_sarg18);
  plus_self25 = ret_val40;
  plus_sarg19 = ((STR) &S_fram362955677);
  ret_val41 = STR_ap2004550586(plus_self25, plus_sarg19);
  res = ret_val41;
 }
 L53 = (ATTR(ATTR(ATTR(self,e),fun),ret)==((dTP) NULL));
 L541_=!(L53); 
 if (L541_) {
  plus_self26 = res;
  plus_arg6 = ' ';
  str_self8 = plus_arg6;
  create_self4 = ((STR) NULL);
  create_c4 = str_self8;
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
  r4 = L55;
  SARR((STR)r4,0,create_c4); 
  ;
  ret_val44 = r4;
  ret_val43 = ret_val44;
  s6 = ret_val43;
  ret_val42 = STR_ap1077157958(plus_self26, s6, TRUE);
  plus_self27 = ret_val42;
  plus_sarg20 = MANGLE119219986(m, ((OB) ATTR(ATTR(ATTR(self,e),fun),ret)), ((OB) NULL));
  ret_val45 = STR_ap2004550586(plus_self27, plus_sarg20);
  res = ret_val45;
 }
 else {
  plus_self28 = res;
  plus_sarg21 = ((STR) &void13);
  ret_val46 = STR_ap2004550586(plus_self28, plus_sarg21);
  res = ret_val46;
 }
 plus_self29 = res;
 plus_sarg22 = ((STR) &callstruct);
 ret_val47 = STR_ap2004550586(plus_self29, plus_sarg22);
 plus_self30 = ret_val47;
 plus_sarg23 = name111;
 ret_val48 = STR_ap2004550586(plus_self30, plus_sarg23);
 plus_self31 = ret_val48;
 plus_sarg24 = ((STR) &S_iter_ob_struct3);
 ret_val49 = STR_ap2004550586(plus_self31, plus_sarg24);
 res = ret_val49;
 if (ATTR(c,func_tables)) {
  plus_self32 = res;
  plus_sarg25 = ((STR) &struct139248200);
  ret_val50 = STR_ap2004550586(plus_self32, plus_sarg25);
  res = ret_val50;
 }
 plus_self33 = res;
 plus_sarg26 = ((STR) &name127);
 ret_val51 = STR_ap2004550586(plus_self33, plus_sarg26);
 res = ret_val51;
 plus_self34 = res;
 plus_sarg27 = ((STR) &size_tsize);
 ret_val52 = STR_ap2004550586(plus_self34, plus_sarg27);
 res = ret_val52;
 {
  BOOL f_L601_ = TRUE;
  /* loop index variable */
  L51 = 0;
  L31 = ATTR(ATTR(self,e),bnd_args);
  L601_br=L31==NULL?0:ASIZE((ARRAYINT)L31); 
  while (1) {
   if(L51>=L601_br)  goto after_loop1; 
   rL601_=L51; 
   i1 = rL601_;
   L63 = ATTR(ATTR(self,e),bnd_args);
   L641_=ARR((ARRAYINT)L63,i1); 
   L65 = L641_;
   L661_=(L65)==(0); 
   if (L661_) {
    L67 = ATTR(ATTR(ATTR(self,e),fun),tp);
    L68 = (*dTP_is124163553[TAG(L67)])(L67);
    L691_=!(L68); 
    if (L691_) {
     plus_self35 = res;
     plus_arg7 = ' ';
     str_self9 = plus_arg7;
     create_self5 = ((STR) NULL);
     create_c5 = str_self9;
     L72 = 1;
     L74=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L72)*sizeof(CHAR);
     L73=ZALLOC_LEAF_BIG(L74);
     if (L73==NULL) FATAL("Unable to allocate more memory");
     memset(L73,0,L74);
     ((OB)L73)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L73)->header.destroyed=0;
#endif

     L70 = ((STR) L73);
     L70->asize = L72;
     r5 = L70;
     SARR((STR)r5,0,create_c5); 
     ;
     ret_val55 = r5;
     ret_val54 = ret_val55;
     s7 = ret_val54;
     ret_val53 = STR_ap1077157958(plus_self35, s7, TRUE);
     plus_self36 = ret_val53;
     plus_sarg28 = MANGLE119219986(m, ((OB) ATTR(ATTR(ATTR(self,e),fun),tp)), ((OB) NULL));
     ret_val56 = STR_ap2004550586(plus_self36, plus_sarg28);
     res = ret_val56;
     plus_self37 = res;
     plus_sarg29 = ((STR) &bound_arg2);
     ret_val57 = STR_ap2004550586(plus_self37, plus_sarg29);
     plus_self38 = ret_val57;
     plus_arg8 = i1;
     str_self10 = plus_arg8;
     clear_self2 = buf3;
     L76 = (clear_self2==((FSTR) NULL));
     L771_=!(L76); 
     if (L771_) {
      SATTR(clear_self2,loc,0);
     }
     str_in_self2 = str_self10;
     str_in_s2 = buf3;
     ret_val60 = INT_st367594495(str_in_self2, str_in_s2, 0, 10, ' ');
     buf3 = ret_val60;
     str_self11 = buf3;
     ret_val61 = STR_fr1097270334(((STR) NULL), str_self11);
     ret_val59 = ret_val61;
     s8 = ret_val59;
     ret_val58 = STR_ap1077157958(plus_self38, s8, TRUE);
     plus_self39 = ret_val58;
     plus_sarg30 = ((STR) &name13);
     ret_val62 = STR_ap2004550586(plus_self39, plus_sarg30);
     res = ret_val62;
    }
   }
   else {
    L78 = ATTR(ATTR(ATTR(self,e),fun),args);
    L79 = ATTR(ATTR(self,e),bnd_args);
    L801_=ARR((ARRAYINT)L79,i1); 
    L81 = L801_;
    L821_=INTMINUS(L81,1); 
    L83 = L821_;
    L841_=(ARG)ARR((ARRAYARG)L78,L83); 
    bnd_a = L841_;
    plus_self40 = res;
    plus_arg9 = ' ';
    str_self12 = plus_arg9;
    create_self6 = ((STR) NULL);
    create_c6 = str_self12;
    L86 = 1;
    L88=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L86)*sizeof(CHAR);
    L87=ZALLOC_LEAF_BIG(L88);
    if (L87==NULL) FATAL("Unable to allocate more memory");
    memset(L87,0,L88);
    ((OB)L87)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L87)->header.destroyed=0;
#endif

    L85 = ((STR) L87);
    L85->asize = L86;
    r6 = L85;
    SARR((STR)r6,0,create_c6); 
    ;
    ret_val65 = r6;
    ret_val64 = ret_val65;
    s9 = ret_val64;
    ret_val63 = STR_ap1077157958(plus_self40, s9, TRUE);
    plus_self41 = ret_val63;
    plus_sarg31 = BOUND_1019937534(self, bnd_a, MANGLE119219986(m, ((OB) ATTR(bnd_a,tp)), ((OB) NULL)));
    ret_val66 = STR_ap2004550586(plus_self41, plus_sarg31);
    res = ret_val66;
    plus_self42 = res;
    plus_sarg32 = ((STR) &bound_arg2);
    ret_val67 = STR_ap2004550586(plus_self42, plus_sarg32);
    plus_self43 = ret_val67;
    plus_arg10 = i1;
    str_self13 = plus_arg10;
    clear_self3 = buf4;
    L90 = (clear_self3==((FSTR) NULL));
    L911_=!(L90); 
    if (L911_) {
     SATTR(clear_self3,loc,0);
    }
    str_in_self3 = str_self13;
    str_in_s3 = buf4;
    ret_val70 = INT_st367594495(str_in_self3, str_in_s3, 0, 10, ' ');
    buf4 = ret_val70;
    str_self14 = buf4;
    ret_val71 = STR_fr1097270334(((STR) NULL), str_self14);
    ret_val69 = ret_val71;
    s10 = ret_val69;
    ret_val68 = STR_ap1077157958(plus_self43, s10, TRUE);
    plus_self44 = ret_val68;
    plus_sarg33 = ((STR) &name13);
    ret_val72 = STR_ap2004550586(plus_self44, plus_sarg33);
    res = ret_val72;
   }
   L921_=INTPLUS(L51,1); 
   L51 = L921_;
  }
 }
 after_loop1: ;
 psather_self = ATTR(c,prog);
 if (ATTR(psather_self,threads)) {
  L93 = TRUE;
 } else {
  L93 = ATTR(psather_self,distributed);
 }
 ret_val73 = L93;
 if (ret_val73) {
  plus_self45 = res;
  plus_sarg34 = ((STR) &voidex);
  ret_val74 = STR_ap2004550586(plus_self45, plus_sarg34);
  res = ret_val74;
 }
 plus_self46 = res;
 plus_sarg35 = ((STR) &name186);
 ret_val75 = STR_ap2004550586(plus_self46, plus_sarg35);
 plus_self47 = ret_val75;
 plus_sarg36 = name111;
 ret_val76 = STR_ap2004550586(plus_self47, plus_sarg36);
 plus_self48 = ret_val76;
 plus_sarg37 = ((STR) &S_iter_ob3);
 ret_val77 = STR_ap2004550586(plus_self48, plus_sarg37);
 res = ret_val77;
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR BOUND_1537967790(BOUND_809390774 self, CGEN c) {
 STR ret_val;
 STR res = ((STR) NULL);
 MANGLE m;
 STR name111;
 ARRAYARG L11;
 ARG a;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 MANGLE forbid_self;
 STR forbid_s;
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
 CHAR plus_arg = CHAR_zero;
 STR ret_val5;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val6;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val7;
 STR r;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val8;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val9;
 STR plus_self7;
 STR plus_sarg6;
 STR ret_val10;
 INT L21 = INT_zero;
 STR plus_self8;
 STR plus_sarg7;
 STR ret_val11;
 STR plus_self9;
 STR plus_sarg8;
 STR ret_val12;
 STR plus_self10;
 STR plus_sarg9;
 STR ret_val13;
 STR plus_self11;
 STR plus_sarg10;
 STR ret_val14;
 STR plus_self12;
 STR plus_sarg11;
 STR ret_val15;
 STR plus_self13;
 STR plus_sarg12;
 STR ret_val16;
 extern STR typedef1;
 extern STR F_ROUT5;
 extern STR name13;
 extern STR S_struct2;
 extern STR typedefstruct;
 extern STR S_struct3;
 extern STR OB_HEADERheader;
 BOOL L3;
 BOOL L41_;
 STR L5;
 INT L6;
 OB L7;
 INT L8;
 extern STR void13;
 extern STR funcptrvoid;
 INT L101_br;
 ARG aL101_;
 ARG L12;
 extern STR name29;
 INT L131_;
 extern STR struct139248200;
 extern STR name187;
 extern STR name185;
 m = ATTR(c,mangler);
 name111 = MANGLE119219986(m, ((OB) ATTR(self,tp)), ((OB) NULL));
 if (ATTR(ATTR(self,tp),is_fortran)) {
  res = STR_ap1417971546(((STR) &typedef1), ((STR) &F_ROUT5), name111, ((STR) &name13));
  ret_val = res;
  return ret_val;
 }
 else {
  forbid_self = m;
  plus_self = name111;
  plus_sarg = ((STR) &S_struct2);
  ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
  forbid_s = ret_val1;
  SATTR(forbid_self,forbidden,FSETST1404644833(ATTR(forbid_self,forbidden), forbid_s));
  plus_self1 = ((STR) &typedefstruct);
  plus_sarg1 = name111;
  ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
  plus_self2 = ret_val2;
  plus_sarg2 = ((STR) &S_struct3);
  ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
  res = ret_val3;
  plus_self3 = res;
  plus_sarg3 = ((STR) &OB_HEADERheader);
  ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
  res = ret_val4;
  L3 = (ATTR(ATTR(self,tp),ret)==((dTP) NULL));
  L41_=!(L3); 
  if (L41_) {
   plus_self4 = res;
   plus_arg = ' ';
   str_self = plus_arg;
   create_self = ((STR) NULL);
   create_c = str_self;
   L6 = 1;
   L8=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L6)*sizeof(CHAR);
   L7=ZALLOC_LEAF_BIG(L8);
   if (L7==NULL) FATAL("Unable to allocate more memory");
   memset(L7,0,L8);
   ((OB)L7)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L7)->header.destroyed=0;
#endif

   L5 = ((STR) L7);
   L5->asize = L6;
   r = L5;
   SARR((STR)r,0,create_c); 
   ;
   ret_val7 = r;
   ret_val6 = ret_val7;
   s = ret_val6;
   ret_val5 = STR_ap1077157958(plus_self4, s, TRUE);
   plus_self5 = ret_val5;
   plus_sarg4 = MANGLE119219986(m, ((OB) ATTR(ATTR(self,tp),ret)), ((OB) NULL));
   ret_val8 = STR_ap2004550586(plus_self5, plus_sarg4);
   res = ret_val8;
  }
  else {
   plus_self6 = res;
   plus_sarg5 = ((STR) &void13);
   ret_val9 = STR_ap2004550586(plus_self6, plus_sarg5);
   res = ret_val9;
  }
  plus_self7 = res;
  plus_sarg6 = ((STR) &funcptrvoid);
  ret_val10 = STR_ap2004550586(plus_self7, plus_sarg6);
  res = ret_val10;
  {
   BOOL f_L101_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = ATTR(ATTR(self,tp),args);
   L101_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
   while (1) {
    if(L21>=L101_br)  goto after_loop; 
    aL101_=ARR((ARRAYARG)L11,L21); 
    a = aL101_;
    plus_self8 = res;
    plus_sarg7 = ((STR) &name29);
    ret_val11 = STR_ap2004550586(plus_self8, plus_sarg7);
    plus_self9 = ret_val11;
    plus_sarg8 = BOUND_1967670005(self, a, MANGLE119219986(m, ((OB) ATTR(a,tp)), ((OB) NULL)));
    ret_val12 = STR_ap2004550586(plus_self9, plus_sarg8);
    res = ret_val12;
    L131_=INTPLUS(L21,1); 
    L21 = L131_;
   }
  }
  after_loop: ;
  if (ATTR(c,func_tables)) {
   plus_self10 = res;
   plus_sarg9 = ((STR) &struct139248200);
   ret_val13 = STR_ap2004550586(plus_self10, plus_sarg9);
   res = ret_val13;
  }
  plus_self11 = res;
  plus_sarg10 = ((STR) &name187);
  ret_val14 = STR_ap2004550586(plus_self11, plus_sarg10);
  plus_self12 = ret_val14;
  plus_sarg11 = name111;
  ret_val15 = STR_ap2004550586(plus_self12, plus_sarg11);
  plus_self13 = ret_val15;
  plus_sarg12 = ((STR) &name185);
  ret_val16 = STR_ap2004550586(plus_self13, plus_sarg12);
  res = ret_val16;
  ret_val = res;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR BOUND_1707602440(BOUND_1677815802 self, CGEN c) {
 STR ret_val;
 MANGLE m;
 STR name111;
 STR res;
 ARRAYINT L11;
 INT i = INT_zero;
 ARRAYINT L21;
 INT i1 = INT_zero;
 ARG bnd_a;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 MANGLE forbid_self;
 STR forbid_s;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 MANGLE forbid_self1;
 STR forbid_s1;
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
 INT L31 = INT_zero;
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
 INT L41 = INT_zero;
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
 INT plus_arg3 = INT_zero;
 STR ret_val34;
 STR s3;
 INT str_self3 = INT_zero;
 STR ret_val35;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val36;
 FSTR str_self4;
 STR ret_val37;
 STR plus_self28;
 STR plus_sarg24;
 STR ret_val38;
 STR plus_self29;
 CHAR plus_arg4 = CHAR_zero;
 STR ret_val39;
 STR s4;
 CHAR str_self5 = CHAR_zero;
 STR ret_val40;
 STR create_self3;
 CHAR create_c3 = CHAR_zero;
 STR ret_val41;
 STR r3;
 STR plus_self30;
 STR plus_sarg25;
 STR ret_val42;
 STR plus_self31;
 STR plus_sarg26;
 STR ret_val43;
 STR plus_self32;
 INT plus_arg5 = INT_zero;
 STR ret_val44;
 STR s5;
 INT str_self6 = INT_zero;
 STR ret_val45;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val46;
 FSTR str_self7;
 STR ret_val47;
 STR plus_self33;
 STR plus_sarg27;
 STR ret_val48;
 STR plus_self34;
 STR plus_sarg28;
 STR ret_val49;
 STR plus_self35;
 STR plus_sarg29;
 STR ret_val50;
 STR plus_self36;
 STR plus_sarg30;
 STR ret_val51;
 extern STR S_ob1;
 extern STR S_ob_struct1;
 extern STR typedefstruct;
 extern STR S_ob_struct2;
 extern STR OB_HEADERheader;
 extern STR struct3;
 extern STR S_ob_structlocal;
 BOOL L5;
 BOOL L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 STR L10;
 INT L12;
 OB L13;
 INT L14;
 STR L16;
 INT L17;
 OB L18;
 INT L19;
 extern STR void14;
 extern STR funcptrstruct;
 extern STR S_ob_struct3;
 INT L221_br;
 INT aL221_;
 INT L23;
 BOOL L241_;
 dTP L25;
 BOOL L26;
 BOOL L271_;
 extern STR name29;
 extern STR name29;
 ARRAYARG L28;
 INT L291_;
 INT L30;
 ARG L321_;
 ARG L33;
 ARRAYARG L34;
 INT L351_;
 INT L36;
 ARG L371_;
 ARG L38;
 INT L391_;
 extern STR struct139248200;
 extern STR name127;
 BOOL L40;
 BOOL L42;
 BOOL L431_;
 extern STR name10;
 extern STR ret_arg2;
 INT L441_br;
 INT rL441_;
 INT L45;
 ARRAYINT L46;
 INT L471_;
 INT L48;
 BOOL L491_;
 dTP L50;
 BOOL L51;
 BOOL L521_;
 STR L53;
 INT L54;
 OB L55;
 INT L56;
 extern STR bound_arg2;
 BOOL L58;
 BOOL L591_;
 extern STR name13;
 ARRAYARG L60;
 ARRAYINT L61;
 INT L621_;
 INT L63;
 INT L641_;
 INT L65;
 ARG L661_;
 STR L67;
 INT L68;
 OB L69;
 INT L70;
 extern STR bound_arg2;
 BOOL L73;
 BOOL L741_;
 extern STR name13;
 INT L751_;
 extern STR name183;
 extern STR S_ob2;
 m = ATTR(c,mangler);
 name111 = MANGLE119219986(m, ((OB) ATTR(self,e)), ((OB) NULL));
 forbid_self = m;
 plus_self = name111;
 plus_sarg = ((STR) &S_ob1);
 ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
 forbid_s = ret_val1;
 SATTR(forbid_self,forbidden,FSETST1404644833(ATTR(forbid_self,forbidden), forbid_s));
 forbid_self1 = m;
 plus_self1 = name111;
 plus_sarg1 = ((STR) &S_ob_struct1);
 ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
 forbid_s1 = ret_val2;
 SATTR(forbid_self1,forbidden,FSETST1404644833(ATTR(forbid_self1,forbidden), forbid_s1));
 plus_self2 = ((STR) &typedefstruct);
 plus_sarg2 = name111;
 ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
 plus_self3 = ret_val3;
 plus_sarg3 = ((STR) &S_ob_struct2);
 ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
 res = ret_val4;
 plus_self4 = res;
 plus_sarg4 = ((STR) &OB_HEADERheader);
 ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
 res = ret_val5;
 if (ATTR(ATTR(self,e),is_remote)) {
  plus_self5 = res;
  plus_sarg5 = ((STR) &struct3);
  ret_val6 = STR_ap2004550586(plus_self5, plus_sarg5);
  plus_self6 = ret_val6;
  plus_sarg6 = name111;
  ret_val7 = STR_ap2004550586(plus_self6, plus_sarg6);
  plus_self7 = ret_val7;
  plus_sarg7 = ((STR) &S_ob_structlocal);
  ret_val8 = STR_ap2004550586(plus_self7, plus_sarg7);
  res = ret_val8;
 }
 L6 = ATTR(ATTR(self,e),is_remote);
 L71_=!(L6); 
 if (L71_) {
  L8 = (ATTR(ATTR(ATTR(self,e),fun),ret)==((dTP) NULL));
  L91_=!(L8); 
  L5 = L91_;
 } else {
  L5 = FALSE;
 }
 if (L5) {
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
  plus_sarg8 = MANGLE119219986(m, ((OB) ATTR(ATTR(ATTR(self,e),fun),ret)), ((OB) NULL));
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
  res = ret_val13;
 }
 else {
  plus_self11 = res;
  plus_sarg9 = ((STR) &void14);
  ret_val16 = STR_ap2004550586(plus_self11, plus_sarg9);
  res = ret_val16;
 }
 plus_self12 = res;
 plus_sarg10 = ((STR) &funcptrstruct);
 ret_val17 = STR_ap2004550586(plus_self12, plus_sarg10);
 plus_self13 = ret_val17;
 plus_sarg11 = name111;
 ret_val18 = STR_ap2004550586(plus_self13, plus_sarg11);
 plus_self14 = ret_val18;
 plus_sarg12 = ((STR) &S_ob_struct3);
 ret_val19 = STR_ap2004550586(plus_self14, plus_sarg12);
 res = ret_val19;
 {
  BOOL f_L221_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = ATTR(ATTR(self,e),unbnd_args);
  L221_br=L11==NULL?0:ASIZE((ARRAYINT)L11); 
  while (1) {
   if(L31>=L221_br)  goto after_loop; 
   aL221_=ARR((ARRAYINT)L11,L31); 
   i = aL221_;
   L241_=(i)==(0); 
   if (L241_) {
    L25 = ATTR(ATTR(ATTR(self,e),fun),tp);
    L26 = (*dTP_is124163553[TAG(L25)])(L25);
    L271_=!(L26); 
    if (L271_) {
     plus_self15 = res;
     plus_sarg13 = ((STR) &name29);
     ret_val20 = STR_ap2004550586(plus_self15, plus_sarg13);
     plus_self16 = ret_val20;
     plus_sarg14 = MANGLE119219986(m, ((OB) ATTR(ATTR(ATTR(self,e),fun),tp)), ((OB) NULL));
     ret_val21 = STR_ap2004550586(plus_self16, plus_sarg14);
     res = ret_val21;
    }
   }
   else {
    plus_self17 = res;
    plus_sarg15 = ((STR) &name29);
    ret_val22 = STR_ap2004550586(plus_self17, plus_sarg15);
    plus_self18 = ret_val22;
    L28 = ATTR(ATTR(ATTR(self,e),fun),args);
    L291_=INTMINUS(i,1); 
    L30 = L291_;
    L321_=(ARG)ARR((ARRAYARG)L28,L30); 
    L33 = L321_;
    L34 = ATTR(ATTR(ATTR(self,e),fun),args);
    L351_=INTMINUS(i,1); 
    L36 = L351_;
    L371_=(ARG)ARR((ARRAYARG)L34,L36); 
    L38=L371_;
    plus_sarg16 = BOUND_138602513(self, L33, MANGLE119219986(m, ((OB) ATTR(L38,tp)), ((OB) NULL)));
    ret_val23 = STR_ap2004550586(plus_self18, plus_sarg16);
    res = ret_val23;
   }
   L391_=INTPLUS(L31,1); 
   L31 = L391_;
  }
 }
 after_loop: ;
 if (ATTR(c,func_tables)) {
  plus_self19 = res;
  plus_sarg17 = ((STR) &struct139248200);
  ret_val24 = STR_ap2004550586(plus_self19, plus_sarg17);
  res = ret_val24;
 }
 plus_self20 = res;
 plus_sarg18 = ((STR) &name127);
 ret_val25 = STR_ap2004550586(plus_self20, plus_sarg18);
 res = ret_val25;
 if (ATTR(ATTR(self,e),is_remote)) {
  L42 = (ATTR(ATTR(ATTR(self,e),fun),ret)==((dTP) NULL));
  L431_=!(L42); 
  L40 = L431_;
 } else {
  L40 = FALSE;
 }
 if (L40) {
  plus_self21 = res;
  plus_sarg19 = ((STR) &name10);
  ret_val26 = STR_ap2004550586(plus_self21, plus_sarg19);
  plus_self22 = ret_val26;
  plus_sarg20 = MANGLE119219986(m, ((OB) ATTR(ATTR(ATTR(self,e),fun),ret)), ((OB) NULL));
  ret_val27 = STR_ap2004550586(plus_self22, plus_sarg20);
  plus_self23 = ret_val27;
  plus_sarg21 = ((STR) &ret_arg2);
  ret_val28 = STR_ap2004550586(plus_self23, plus_sarg21);
  res = ret_val28;
 }
 {
  BOOL f_L441_ = TRUE;
  /* loop index variable */
  L41 = 0;
  L21 = ATTR(ATTR(self,e),bnd_args);
  L441_br=L21==NULL?0:ASIZE((ARRAYINT)L21); 
  while (1) {
   if(L41>=L441_br)  goto after_loop1; 
   rL441_=L41; 
   i1 = rL441_;
   L46 = ATTR(ATTR(self,e),bnd_args);
   L471_=ARR((ARRAYINT)L46,i1); 
   L48 = L471_;
   L491_=(L48)==(0); 
   if (L491_) {
    L50 = ATTR(ATTR(ATTR(self,e),fun),tp);
    L51 = (*dTP_is124163553[TAG(L50)])(L50);
    L521_=!(L51); 
    if (L521_) {
     plus_self24 = res;
     plus_arg2 = ' ';
     str_self2 = plus_arg2;
     create_self2 = ((STR) NULL);
     create_c2 = str_self2;
     L54 = 1;
     L56=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L54)*sizeof(CHAR);
     L55=ZALLOC_LEAF_BIG(L56);
     if (L55==NULL) FATAL("Unable to allocate more memory");
     memset(L55,0,L56);
     ((OB)L55)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L55)->header.destroyed=0;
#endif

     L53 = ((STR) L55);
     L53->asize = L54;
     r2 = L53;
     SARR((STR)r2,0,create_c2); 
     ;
     ret_val31 = r2;
     ret_val30 = ret_val31;
     s2 = ret_val30;
     ret_val29 = STR_ap1077157958(plus_self24, s2, TRUE);
     plus_self25 = ret_val29;
     plus_sarg22 = MANGLE119219986(m, ((OB) ATTR(ATTR(ATTR(self,e),fun),tp)), ((OB) NULL));
     ret_val32 = STR_ap2004550586(plus_self25, plus_sarg22);
     res = ret_val32;
     plus_self26 = res;
     plus_sarg23 = ((STR) &bound_arg2);
     ret_val33 = STR_ap2004550586(plus_self26, plus_sarg23);
     plus_self27 = ret_val33;
     plus_arg3 = i1;
     str_self3 = plus_arg3;
     clear_self = buf1;
     L58 = (clear_self==((FSTR) NULL));
     L591_=!(L58); 
     if (L591_) {
      SATTR(clear_self,loc,0);
     }
     str_in_self = str_self3;
     str_in_s = buf1;
     ret_val36 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
     buf1 = ret_val36;
     str_self4 = buf1;
     ret_val37 = STR_fr1097270334(((STR) NULL), str_self4);
     ret_val35 = ret_val37;
     s3 = ret_val35;
     ret_val34 = STR_ap1077157958(plus_self27, s3, TRUE);
     plus_self28 = ret_val34;
     plus_sarg24 = ((STR) &name13);
     ret_val38 = STR_ap2004550586(plus_self28, plus_sarg24);
     res = ret_val38;
    }
   }
   else {
    L60 = ATTR(ATTR(ATTR(self,e),fun),args);
    L61 = ATTR(ATTR(self,e),bnd_args);
    L621_=ARR((ARRAYINT)L61,i1); 
    L63 = L621_;
    L641_=INTMINUS(L63,1); 
    L65 = L641_;
    L661_=(ARG)ARR((ARRAYARG)L60,L65); 
    bnd_a = L661_;
    plus_self29 = res;
    plus_arg4 = ' ';
    str_self5 = plus_arg4;
    create_self3 = ((STR) NULL);
    create_c3 = str_self5;
    L68 = 1;
    L70=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L68)*sizeof(CHAR);
    L69=ZALLOC_LEAF_BIG(L70);
    if (L69==NULL) FATAL("Unable to allocate more memory");
    memset(L69,0,L70);
    ((OB)L69)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L69)->header.destroyed=0;
#endif

    L67 = ((STR) L69);
    L67->asize = L68;
    r3 = L67;
    SARR((STR)r3,0,create_c3); 
    ;
    ret_val41 = r3;
    ret_val40 = ret_val41;
    s4 = ret_val40;
    ret_val39 = STR_ap1077157958(plus_self29, s4, TRUE);
    plus_self30 = ret_val39;
    plus_sarg25 = MANGLE119219986(m, ((OB) ATTR(bnd_a,tp)), ((OB) NULL));
    ret_val42 = STR_ap2004550586(plus_self30, plus_sarg25);
    res = ret_val42;
    plus_self31 = res;
    plus_sarg26 = ((STR) &bound_arg2);
    ret_val43 = STR_ap2004550586(plus_self31, plus_sarg26);
    plus_self32 = ret_val43;
    plus_arg5 = i1;
    str_self6 = plus_arg5;
    clear_self1 = buf2;
    L73 = (clear_self1==((FSTR) NULL));
    L741_=!(L73); 
    if (L741_) {
     SATTR(clear_self1,loc,0);
    }
    str_in_self1 = str_self6;
    str_in_s1 = buf2;
    ret_val46 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
    buf2 = ret_val46;
    str_self7 = buf2;
    ret_val47 = STR_fr1097270334(((STR) NULL), str_self7);
    ret_val45 = ret_val47;
    s5 = ret_val45;
    ret_val44 = STR_ap1077157958(plus_self32, s5, TRUE);
    plus_self33 = ret_val44;
    plus_sarg27 = ((STR) &name13);
    ret_val48 = STR_ap2004550586(plus_self33, plus_sarg27);
    res = ret_val48;
   }
   L751_=INTPLUS(L41,1); 
   L41 = L751_;
  }
 }
 after_loop1: ;
 plus_self34 = res;
 plus_sarg28 = ((STR) &name183);
 ret_val49 = STR_ap2004550586(plus_self34, plus_sarg28);
 plus_self35 = ret_val49;
 plus_sarg29 = name111;
 ret_val50 = STR_ap2004550586(plus_self35, plus_sarg29);
 plus_self36 = ret_val50;
 plus_sarg30 = ((STR) &S_ob2);
 ret_val51 = STR_ap2004550586(plus_self36, plus_sarg30);
 res = ret_val51;
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR BOUND_1856529942(BOUND_1007407331 self) {
 return ATTR(self,str);
}


#undef IS_ITER
#define IS_ITER 0

STR BOUND_1967670005(BOUND_809390774 self, ARG e, STR s) {
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

STR BOUND_365396347(BOUND_2140373124 self, CGEN c) {
 STR ret_val;
 MANGLE m;
 STR name111;
 STR res;
 BOOL is_hot = BOOL_zero;
 ARRAYARG L11;
 ARG a;
 ARRAYBOOL L21;
 ARRAYARG L31;
 INT i = INT_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 MANGLE forbid_self;
 STR forbid_s;
 MANGLE forbid_self1;
 STR forbid_s1;
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
 INT L41 = INT_zero;
 STR plus_self5;
 CHAR plus_arg = CHAR_zero;
 STR ret_val6;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val7;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val8;
 STR r;
 STR plus_self6;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val9;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val10;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val11;
 STR r1;
 STR plus_self7;
 STR plus_sarg5;
 STR ret_val12;
 STR plus_self8;
 STR plus_sarg6;
 STR ret_val13;
 STR plus_self9;
 STR plus_sarg7;
 STR ret_val14;
 STR plus_self10;
 STR plus_sarg8;
 STR ret_val15;
 STR plus_self11;
 INT plus_arg2 = INT_zero;
 STR ret_val16;
 STR s2;
 INT str_self2 = INT_zero;
 STR ret_val17;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val18;
 FSTR str_self3;
 STR ret_val19;
 STR plus_self12;
 STR plus_sarg9;
 STR ret_val20;
 STR plus_self13;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val21;
 STR s3;
 CHAR str_self4 = CHAR_zero;
 STR ret_val22;
 STR create_self2;
 CHAR create_c2 = CHAR_zero;
 STR ret_val23;
 STR r2;
 STR plus_self14;
 STR plus_sarg10;
 STR ret_val24;
 STR plus_self15;
 STR plus_sarg11;
 STR ret_val25;
 STR plus_self16;
 STR plus_sarg12;
 STR ret_val26;
 STR plus_self17;
 CHAR plus_arg4 = CHAR_zero;
 STR ret_val27;
 STR s4;
 CHAR str_self5 = CHAR_zero;
 STR ret_val28;
 STR create_self3;
 CHAR create_c3 = CHAR_zero;
 STR ret_val29;
 STR r3;
 STR plus_self18;
 STR plus_sarg13;
 STR ret_val30;
 STR plus_self19;
 STR plus_sarg14;
 STR ret_val31;
 STR plus_self20;
 STR plus_sarg15;
 STR ret_val32;
 STR plus_self21;
 STR plus_sarg16;
 STR ret_val33;
 STR plus_self22;
 STR plus_sarg17;
 STR ret_val34;
 STR plus_self23;
 STR plus_sarg18;
 STR ret_val35;
 STR plus_self24;
 STR plus_sarg19;
 STR ret_val36;
 STR plus_self25;
 STR plus_sarg20;
 STR ret_val37;
 STR plus_self26;
 STR plus_sarg21;
 STR ret_val38;
 STR plus_self27;
 STR plus_sarg22;
 STR ret_val39;
 STR plus_self28;
 STR plus_sarg23;
 STR ret_val40;
 extern STR S_struct2;
 extern STR typedefstruct;
 extern STR S_struct3;
 extern STR OB_HEADERheader;
 extern STR INTstate1;
 INT L51_br;
 ARG aL51_;
 ARG L8;
 BOOL L9;
 BOOL L101_;
 BOOL L12;
 INT L61_br;
INT i_L61_=0;
 BOOL aL61_;
 INT L13;
 INT L71_br;
INT i_L71_=0;
 INT rL71_;
 STR L14;
 INT L15;
 OB L16;
 INT L17;
 STR L19;
 INT L20;
 OB L22;
 INT L23;
 extern STR hot11;
 extern STR once;
 extern STR arg21;
 BOOL L25;
 BOOL L261_;
 extern STR name13;
 INT L271_;
 BOOL L28;
 BOOL L291_;
 STR L30;
 INT L32;
 OB L33;
 INT L34;
 extern STR ret_val411;
 extern STR iter_f1092307779;
 BOOL L36;
 BOOL L371_;
 STR L38;
 INT L39;
 OB L40;
 INT L42;
 extern STR void13;
 extern STR callstruct;
 extern STR S_struct4;
 extern STR struct139248200;
 extern STR name127;
 extern STR size_tsize;
 extern STR name186;
 extern STR name185;
 m = ATTR(c,mangler);
 name111 = MANGLE119219986(m, ((OB) ATTR(self,tp)), ((OB) NULL));
 forbid_self = m;
 plus_self = name111;
 plus_sarg = ((STR) &S_struct2);
 ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
 forbid_s = ret_val1;
 SATTR(forbid_self,forbidden,FSETST1404644833(ATTR(forbid_self,forbidden), forbid_s));
 forbid_self1 = m;
 forbid_s1 = name111;
 SATTR(forbid_self1,forbidden,FSETST1404644833(ATTR(forbid_self1,forbidden), forbid_s1));
 plus_self1 = ((STR) &typedefstruct);
 plus_sarg1 = name111;
 ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
 plus_self2 = ret_val2;
 plus_sarg2 = ((STR) &S_struct3);
 ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
 res = ret_val3;
 plus_self3 = res;
 plus_sarg3 = ((STR) &OB_HEADERheader);
 ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
 res = ret_val4;
 plus_self4 = res;
 plus_sarg4 = ((STR) &INTstate1);
 ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
 res = ret_val5;
 {
  BOOL f_L51_ = TRUE;
  BOOL f_L61_ = TRUE;
  BOOL f_L71_ = TRUE;
  /* loop index variable */
  L41 = 0;
  L11 = ATTR(ATTR(self,tp),args);
  L51_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
  while (1) {
   if(L41>=L51_br)  goto after_loop; 
   aL51_=ARR((ARRAYARG)L11,L41); 
   a = aL51_;
   L9 = (ATTR(ATTR(self,tp),hot)==((ARRAYBOOL) NULL));
   L101_=!(L9); 
   if (L101_) {
    if (f_L61_) {
     f_L61_ = FALSE;
     L21 = ATTR(ATTR(self,tp),hot);
     L61_br=L21==NULL?0:ASIZE((ARRAYBOOL)L21); 
     i_L61_=0;
    }
    if(i_L61_>=L61_br)  goto after_loop; 
    aL61_=ARR((ARRAYBOOL)L21,i_L61_); i_L61_++;
    is_hot = aL61_;
   }
   else {
    is_hot = FALSE;
   }
   if (f_L71_) {
    f_L71_ = FALSE;
    L31 = ATTR(ATTR(self,tp),args);
    L71_br=L31==NULL?0:ASIZE((ARRAYARG)L31); 
    i_L71_=0;
   }
   if(i_L71_>=L71_br)  goto after_loop; 
   rL71_=i_L71_; i_L71_++;
   i = rL71_;
   plus_self5 = res;
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
   ret_val8 = r;
   ret_val7 = ret_val8;
   s = ret_val7;
   ret_val6 = STR_ap1077157958(plus_self5, s, TRUE);
   plus_self7 = ret_val6;
   plus_self6 = MANGLE119219986(m, ((OB) ATTR(a,tp)), ((OB) NULL));
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
   ret_val11 = r1;
   ret_val10 = ret_val11;
   s1 = ret_val10;
   ret_val9 = STR_ap1077157958(plus_self6, s1, TRUE);
   plus_sarg5 = BOUND_457865042(self, a, ret_val9);
   ret_val12 = STR_ap2004550586(plus_self7, plus_sarg5);
   res = ret_val12;
   if (is_hot) {
    plus_self8 = res;
    plus_sarg6 = ((STR) &hot11);
    ret_val13 = STR_ap2004550586(plus_self8, plus_sarg6);
    res = ret_val13;
   }
   else {
    plus_self9 = res;
    plus_sarg7 = ((STR) &once);
    ret_val14 = STR_ap2004550586(plus_self9, plus_sarg7);
    res = ret_val14;
   }
   plus_self10 = res;
   plus_sarg8 = ((STR) &arg21);
   ret_val15 = STR_ap2004550586(plus_self10, plus_sarg8);
   plus_self11 = ret_val15;
   plus_arg2 = i;
   str_self2 = plus_arg2;
   clear_self = buf1;
   L25 = (clear_self==((FSTR) NULL));
   L261_=!(L25); 
   if (L261_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self2;
   str_in_s = buf1;
   ret_val18 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val18;
   str_self3 = buf1;
   ret_val19 = STR_fr1097270334(((STR) NULL), str_self3);
   ret_val17 = ret_val19;
   s2 = ret_val17;
   ret_val16 = STR_ap1077157958(plus_self11, s2, TRUE);
   plus_self12 = ret_val16;
   plus_sarg9 = ((STR) &name13);
   ret_val20 = STR_ap2004550586(plus_self12, plus_sarg9);
   res = ret_val20;
   L271_=INTPLUS(L41,1); 
   L41 = L271_;
  }
 }
 after_loop: ;
 L28 = (ATTR(ATTR(self,tp),ret)==((dTP) NULL));
 L291_=!(L28); 
 if (L291_) {
  plus_self13 = res;
  plus_arg3 = ' ';
  str_self4 = plus_arg3;
  create_self2 = ((STR) NULL);
  create_c2 = str_self4;
  L32 = 1;
  L34=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L32)*sizeof(CHAR);
  L33=ZALLOC_LEAF_BIG(L34);
  if (L33==NULL) FATAL("Unable to allocate more memory");
  memset(L33,0,L34);
  ((OB)L33)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L33)->header.destroyed=0;
#endif

  L30 = ((STR) L33);
  L30->asize = L32;
  r2 = L30;
  SARR((STR)r2,0,create_c2); 
  ;
  ret_val23 = r2;
  ret_val22 = ret_val23;
  s3 = ret_val22;
  ret_val21 = STR_ap1077157958(plus_self13, s3, TRUE);
  plus_self14 = ret_val21;
  plus_sarg10 = MANGLE119219986(m, ((OB) ATTR(ATTR(self,tp),ret)), ((OB) NULL));
  ret_val24 = STR_ap2004550586(plus_self14, plus_sarg10);
  res = ret_val24;
  plus_self15 = res;
  plus_sarg11 = ((STR) &ret_val411);
  ret_val25 = STR_ap2004550586(plus_self15, plus_sarg11);
  res = ret_val25;
 }
 plus_self16 = res;
 plus_sarg12 = ((STR) &iter_f1092307779);
 ret_val26 = STR_ap2004550586(plus_self16, plus_sarg12);
 res = ret_val26;
 L36 = (ATTR(ATTR(self,tp),ret)==((dTP) NULL));
 L371_=!(L36); 
 if (L371_) {
  plus_self17 = res;
  plus_arg4 = ' ';
  str_self5 = plus_arg4;
  create_self3 = ((STR) NULL);
  create_c3 = str_self5;
  L39 = 1;
  L42=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L39)*sizeof(CHAR);
  L40=ZALLOC_LEAF_BIG(L42);
  if (L40==NULL) FATAL("Unable to allocate more memory");
  memset(L40,0,L42);
  ((OB)L40)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L40)->header.destroyed=0;
#endif

  L38 = ((STR) L40);
  L38->asize = L39;
  r3 = L38;
  SARR((STR)r3,0,create_c3); 
  ;
  ret_val29 = r3;
  ret_val28 = ret_val29;
  s4 = ret_val28;
  ret_val27 = STR_ap1077157958(plus_self17, s4, TRUE);
  plus_self18 = ret_val27;
  plus_sarg13 = MANGLE119219986(m, ((OB) ATTR(ATTR(self,tp),ret)), ((OB) NULL));
  ret_val30 = STR_ap2004550586(plus_self18, plus_sarg13);
  res = ret_val30;
 }
 else {
  plus_self19 = res;
  plus_sarg14 = ((STR) &void13);
  ret_val31 = STR_ap2004550586(plus_self19, plus_sarg14);
  res = ret_val31;
 }
 plus_self20 = res;
 plus_sarg15 = ((STR) &callstruct);
 ret_val32 = STR_ap2004550586(plus_self20, plus_sarg15);
 plus_self21 = ret_val32;
 plus_sarg16 = name111;
 ret_val33 = STR_ap2004550586(plus_self21, plus_sarg16);
 plus_self22 = ret_val33;
 plus_sarg17 = ((STR) &S_struct4);
 ret_val34 = STR_ap2004550586(plus_self22, plus_sarg17);
 res = ret_val34;
 if (ATTR(c,func_tables)) {
  plus_self23 = res;
  plus_sarg18 = ((STR) &struct139248200);
  ret_val35 = STR_ap2004550586(plus_self23, plus_sarg18);
  res = ret_val35;
 }
 plus_self24 = res;
 plus_sarg19 = ((STR) &name127);
 ret_val36 = STR_ap2004550586(plus_self24, plus_sarg19);
 res = ret_val36;
 plus_self25 = res;
 plus_sarg20 = ((STR) &size_tsize);
 ret_val37 = STR_ap2004550586(plus_self25, plus_sarg20);
 res = ret_val37;
 plus_self26 = res;
 plus_sarg21 = ((STR) &name186);
 ret_val38 = STR_ap2004550586(plus_self26, plus_sarg21);
 plus_self27 = ret_val38;
 plus_sarg22 = name111;
 ret_val39 = STR_ap2004550586(plus_self27, plus_sarg22);
 plus_self28 = ret_val39;
 plus_sarg23 = ((STR) &name185);
 ret_val40 = STR_ap2004550586(plus_self28, plus_sarg23);
 res = ret_val40;
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR BOUND_457865042(BOUND_2140373124 self, ARG e, STR s) {
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

STR BOUND_619031357(BOUND_809390774 self) {
 return ATTR(self,str);
}


#undef IS_ITER
#define IS_ITER 0

STR BOUND_787102169(BOUND_1677815802 self) {
 return ATTR(self,str);
}


#undef IS_ITER
#define IS_ITER 0

STR BUILTI1940169806(BUILTIN_LAYOUT self) {
 return ATTR(self,str);
}


#undef IS_ITER
#define IS_ITER 0

STR BUILTI35892888(BUILTIN_LAYOUT self, CGEN c) {
 STR ret_val;
 extern STR name1;
 ret_val = ((STR) &name1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR INTI_strrSTR(INTI1 self) {
 STR ret_val;
 FSTR clear_self;
 FSTR str_self;
 STR ret_val1;
 BOOL L1;
 BOOL L21_;
 clear_self = INTI_buf;
 L1 = (clear_self==((FSTR) NULL));
 L21_=!(L1); 
 if (L21_) {
  SATTR(clear_self,loc,0);
 }
 INTI_buf = INTI_s96356965(self, INTI_buf);
 str_self = INTI_buf;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void ABSTRA1640219182(ABSTRACT_LAYOUT self) {
 SIG_TBL L11;
 SIG sig1;
 FLISTdLAYOUT create_self;
 FLISTdLAYOUT ret_val;
 SIG is_iter_self;
 BOOL ret_val1 = BOOL_zero;
 IDENT is_iter_self1 = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
 SIG L2;
 dTP L3;
 IFC L4;
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
 create_self = ((FLISTdLAYOUT) NULL);
 ret_val = ((FLISTdLAYOUT) NULL);
 SATTR(self,dependencies,ret_val);
 {
  struct SIG_TBL_eltbrSIG_frame_struct other1_0;
SIG_TBL_eltbrSIG_frame noname1 = &other1_0;
  noname1->state = 0;
  while (1) {
   if (noname1->state == 0) {
    L3 = ATTR(self,tp);
    L4=(*dTP_ifcrIFC[TAG(L3)])(L3);
    L11 = ATTR(L4,sigs);
    noname1->self = L11;
   }
   L2 = SIG_TBL_eltbrSIG(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   sig1 = L2;
   is_iter_self = sig1;
   is_iter_self1 = ATTR(is_iter_self,name1);
   L6 = (is_iter_self1.str==((STR) NULL));
   L71_=!(L6); 
   if (L71_) {
    L8 = is_iter_self1.str;
    L9 = STR_sizerINT(is_iter_self1.str);
    L101_=INTMINUS(L9,1); 
    L12 = L101_;
    L131_=ARR((STR)L8,L12); 
    L14 = L131_;
    L151_=L14=='!'; 
    L5 = L151_;
   } else {
    L5 = FALSE;
   }
   ret_val2 = L5;
   ret_val1 = ret_val2;
   if (ret_val1) {
   }
  }
 }
 after_loop: ;
}
