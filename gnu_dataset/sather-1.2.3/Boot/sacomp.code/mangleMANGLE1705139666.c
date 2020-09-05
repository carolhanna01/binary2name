#include "sather.h"

/* Layouts */

typedef struct dBACK_END_struct {
 OB_HEADER header;
 } *dBACK_END;

typedef struct dPROG_ERR_struct {
 OB_HEADER header;
 } *dPROG_ERR;

typedef struct dSTR_struct {
 OB_HEADER header;
 } *dSTR;

typedef struct dTP_struct {
 OB_HEADER header;
 } *dTP;

typedef struct ARRAYARG_struct {/* layout for ARRAY{ARG} */
 OB_HEADER header;
 INT asize;
 struct ARG_struct *arr_part[1];
 } *ARRAYARG;

typedef struct ELT_HASH_struct {/* layout for ELT_HASH */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_HASH;

typedef struct ELT_NILdOB_struct {/* layout for ELT_NIL{$OB} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILdOB;

typedef struct ERR1_struct {/* layout for ERR */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ERR1;

typedef struct FAST_IDENTIFIER_struct {/* layout for FAST_IDENTIFIER */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *FAST_IDENTIFIER;

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

typedef struct STR_eltbrCHAR_frame_struct {
 INT state;
 STR self;/* Formal argument: self */
 CHAR ret_val;
 INT i;
 INT sz;
 } *STR_eltbrCHAR_frame;

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

typedef struct FMAPdOBNAMESPACE_struct {/* layout for FMAP{$OB,NAMESPACE} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdOBNAMESPACE_struct arr_part[1];
 } *FMAPdOBNAMESPACE;

typedef struct FMAPSTRINT_struct {/* layout for FMAP{STR,INT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRINT_struct arr_part[1];
 } *FMAPSTRINT;

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

typedef struct NAMEMA1836164318_frame_struct {
 INT state;
 NAMEMAP self;/* Formal argument: self */
 TUPdOBSTR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *NAMEMA1836164318_frame;

typedef struct NAMEMA1267087904_frame_struct {
 INT state;
 NAMEMAP self;/* Formal argument: self */
 TUPdOBSTR arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *NAMEMA1267087904_frame;

typedef struct NAMEMA726632978_frame_struct {
 INT state;
 NAMEMAP self;/* Formal argument: self */
 TUPdOBSTR ret_val1;
 NAMEMAP L61;
 TUPdOBSTR r;
 INT L31;
 NAMEMAP is_key_nil_self;
 OB is_key_nil_e;
 BOOL ret_val;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *NAMEMA726632978_frame;

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

extern IDENT IDENT_857412936;
extern INT CGEN_m1427112264;
extern INT NAMEMA2064441237;

/* Function declarations */


extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);
ARG ARRAYA827580689(ARRAYA827580689_frame);
BOOL ELT_NI455285430(ELT_NILdOB, OB);
BOOL FSETST1025458804(FSETSTR, STR);
BOOL NAMEMA291126687(NAMEMAP, OB, OB);
BOOL STR_is111530248(STR, STR);
BOOL TP_CLA727826289(TP_CLASS);
CHAR STR_eltbrCHAR(STR_eltbrCHAR_frame);
FMAPdOBNAMESPACE FMAPdO1199466663(FMAPdOBNAMESPACE, OB, NAMESPACE);
FSETSTR FSETST1404644833(FSETSTR, STR);
FSTR FSTR_p1410513982(FSTR, FSTR);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR FSTR_p399773021(FSTR, CHAR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
IMPL TP_CLA1109727233(TP_CLASS);
INT ELT_HA1612720024(ELT_HASH, OB);
INT FSTR_sizerINT(FSTR);
INT STR_hashrINT(STR);
INT STR_lengthrINT(STR);
INT STR_sizerINT(STR);
MANGLE MANGLE2136606480(MANGLE, PROG);
NAMEMAP NAMEMA1469296016(NAMEMAP, INT);
NAMEMAP NAMEMA2020020771(NAMEMAP, OB, STR);
NAMEMAP NAMEMA54431949(NAMEMAP);
NAMESPACE FMAPdO790518483(FMAPdOBNAMESPACE, OB);
OB ELT_NI509824640(ELT_NILdOB);
STR ARG_strrSTR(ARG);
STR FAST_I535598365(FAST_IDENTIFIER);
STR IMPL_s709672763(IMPL, IDENT);
STR INT_oc2110318348(INT);
STR MANGLE119219986(MANGLE, OB, OB);
STR MANGLE127610753(MANGLE, OB, OB);
STR MANGLE1995580116(MANGLE, STR);
STR MANGLE868076060(MANGLE, CHAR);
STR NAMEMA1183587160(NAMEMAP, OB);
STR NAMESP180237388(NAMESPACE, STR);
STR NAMESP8552102(NAMESPACE, STR);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap1693864410(STR, STR, STR);
STR STR_ap2004550586(STR, STR);
STR STR_fr1097270334(STR, FSTR);
STR STR_head_INTrSTR(STR, INT);
STR STR_su203383946(STR, INT, INT);
STR SYSTEM675309847(SYSTEM675309847_frame);
STR TP_CLASS_strrSTR(TP_CLASS);
STR TP_ITER_strrSTR(TP_ITER);
STR TP_ROUT_strrSTR(TP_ROUT);
SYSTEM_LEX SYSTEM2034253983(SYSTEM_LEX, STR);
TUPdOBSTR NAMEMA1836164318(NAMEMA1836164318_frame);
TUPdOBSTR NAMEMA726632978(NAMEMA726632978_frame);
void FILE_plus_dSTR(FILE1, dSTR);
void MANGLE1827205184(MANGLE, OB, STR, OB);
void NAMEMA1267087904(NAMEMA1267087904_frame);
void PROG_e176405615(PROG, dPROG_ERR);
void PROG_err_STR(PROG, STR);
void PROG_warning_STR(PROG, STR);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

BOOL NAMEMA291126687(NAMEMAP self, OB k1, OB k2) {
 BOOL ret_val = BOOL_zero;
 if (k1==NULL) {
  goto other219;
 } else
 switch (TAG(k1)) {
  case STR_tag:
   if (k2==NULL) {
    goto other220;
   } else
   switch (TAG(k2)) {
    case STR_tag:
     ret_val = STR_is111530248(((STR) k1), ((STR) k2));
     return ret_val; break;
    default: ;
    other220: ;
     ret_val = FALSE;
     return ret_val;
   } break;
  default: ;
  other219: ;
   ret_val = SYSOBEQ(k1,k2);
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

MANGLE MANGLE2136606480(MANGLE self, PROG p) {
 MANGLE ret_val;
 MANGLE res;
 dBACK_END back1;
 SYSTEM_LEX l;
 SYSTEM_LEX L11;
 FMAPdOBNAMESPACE create_self;
 FMAPdOBNAMESPACE ret_val1;
 FSETSTR create_self1;
 FSETSTR ret_val2;
 FSETSTR create_self2;
 FSETSTR ret_val3;
 MANGLE space_self;
 OB space_ns;
 NAMESPACE ret_val4;
 NAMESPACE r;
 NAMESPACE create_self3;
 NAMESPACE ret_val5;
 NAMESPACE r1;
 NAMEMAP create_self4;
 NAMEMAP ret_val6;
 FSETSTR create_self5;
 FSETSTR ret_val7;
 FMAPSTRINT create_self6;
 FMAPSTRINT ret_val8;
 STR plus_self;
 STR plus_sarg;
 STR ret_val9;
 MANGLE forbid_self;
 STR forbid_s;
 MANGLE forbid_self1;
 STR forbid_s1;
 MANGLE L2;
 OB L3;
 BOOL L4;
 BOOL L51_;
 NAMESPACE L6;
 OB L7;
 extern STR SystemFORBID;
 STR L8;
 extern STR self1;
 L3=ZALLOC(sizeof(struct MANGLE_struct));
 if (L3==NULL) FATAL("Unable to allocate more memory");
 ((OB)L3)->header.tag=MANGLE_tag;
 L2 = ((MANGLE) L3);
 res = L2;
 SATTR(res,prog,p);
 create_self = ((FMAPdOBNAMESPACE) NULL);
 ret_val1 = ((FMAPdOBNAMESPACE) NULL);
 SATTR(res,namespaces,ret_val1);
 create_self1 = ((FSETSTR) NULL);
 ret_val2 = ((FSETSTR) NULL);
 SATTR(res,forbidden,ret_val2);
 create_self2 = ((FSETSTR) NULL);
 ret_val3 = ((FSETSTR) NULL);
 SATTR(res,used_by_local,ret_val3);
 back1 = ATTR(p,back_end);
 switch (TAG(back1)) {
  case CGEN_tag:
   SATTR(res,max_identifier_l,CGEN_m1427112264);
   L4 = ATTR(((CGEN) back1),pretty);
   L51_=!(L4); 
   SATTR(res,append_hash_to_i,L51_); break;
  default: ;
   FATAL("No applicable type in typecase\nin MANGLE::create(PROG):SAME\n./Back/mangle.sa:154:14");
 }
 space_self = res;
 space_ns = ((OB) NULL);
 if ((space_ns==((OB) NULL))) {
  space_ns = ((OB) space_self);
 }
 r = FMAPdO790518483(ATTR(space_self,namespaces), space_ns);
 if ((r==((NAMESPACE) NULL))) {
  create_self3 = ((NAMESPACE) NULL);
  L7=ZALLOC(sizeof(struct NAMESPACE_struct));
  if (L7==NULL) FATAL("Unable to allocate more memory");
  ((OB)L7)->header.tag=NAMESPACE_tag;
  L6 = ((NAMESPACE) L7);
  r1 = L6;
  create_self4 = ((NAMEMAP) NULL);
  ret_val6 = ((NAMEMAP) NULL);
  SATTR(r1,map,ret_val6);
  create_self5 = ((FSETSTR) NULL);
  ret_val7 = ((FSETSTR) NULL);
  SATTR(r1,set,ret_val7);
  SATTR(r1,counter,0);
  create_self6 = ((FMAPSTRINT) NULL);
  ret_val8 = ((FMAPSTRINT) NULL);
  SATTR(r1,uniques,ret_val8);
  ret_val5 = r1;
  r = ret_val5;
  SATTR(space_self,namespaces,FMAPdO1199466663(ATTR(space_self,namespaces), space_ns, r));
 }
 ret_val4 = r;
 SATTR(res,global_space,ret_val4);
 plus_self = ATTR(p,home);
 plus_sarg = ((STR) &SystemFORBID);
 ret_val9 = STR_ap2004550586(plus_self, plus_sarg);
 l = SYSTEM2034253983(((SYSTEM_LEX) NULL), ret_val9);
 {
  struct SYSTEM675309847_frame_struct other1_0;
SYSTEM675309847_frame noname1 = &other1_0;
  L11 = l;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   forbid_self = res;
   L8 = SYSTEM675309847(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   forbid_s = L8;
   SATTR(forbid_self,forbidden,FSETST1404644833(ATTR(forbid_self,forbidden), forbid_s));
  }
 }
 after_loop: ;
 forbid_self1 = res;
 forbid_s1 = ((STR) &self1);
 SATTR(forbid_self1,forbidden,FSETST1404644833(ATTR(forbid_self1,forbidden), forbid_s1));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

NAMEMAP NAMEMA1469296016(NAMEMAP self, INT n) {
 NAMEMAP ret_val;
 NAMEMAP r;
 NAMEMAP L11;
 NAMEMAP key_nil_self;
 OB ret_val1;
 NAMEMAP key_nil_self1;
 OB ret_val2;
 TUPdOBSTR create_self = TUPdOBSTR_zero;
 OB create_at1;
 STR create_at2;
 TUPdOBSTR ret_val3 = TUPdOBSTR_zero;
 INT L21 = INT_zero;
 NAMEMAP L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TUPdOBSTR L10;
 TUPdOBSTR L12;
 TUPdOBSTR L13;
 INT L141_;
 L4 = n;
 L6=(sizeof(struct NAMEMAP_struct)-sizeof(TUPdOBSTR))+(L4)*sizeof(TUPdOBSTR);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=NAMEMAP_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((NAMEMAP) L5);
 L3->asize = L4;
 r = L3;
 key_nil_self = self;
 ret_val1 = ELT_NI509824640(((ELT_NILdOB) NULL));
 L7 = (ret_val1==((OB) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((NAMEMAP)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    create_self = TUPdOBSTR_zero;
    key_nil_self1 = self;
    ret_val2 = ELT_NI509824640(((ELT_NILdOB) NULL));
    create_at1 = ret_val2;
    create_at2 = ((STR) NULL);
    L10 = create_self;
    L10.t1 = create_at1;
    L12 = L10;
    L12.t2 = create_at2;
    ret_val3 = L12;
    L13 = ret_val3;
    SARR((NAMEMAP)L11,L21,L13); 
    ;
    L141_=INTPLUS(L21,1); 
    L21 = L141_;
   }
  }
  after_loop: ;
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

NAMEMAP NAMEMA2020020771(NAMEMAP self, OB k, STR t) {
 NAMEMAP ret_val;
 NAMEMAP r;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 INT asm1 = INT_zero;
 OB tk;
 OB tk1;
 NAMEMAP should_grow_self;
 BOOL ret_val1 = BOOL_zero;
 NAMEMAP key_hash_self;
 OB key_hash_e;
 INT ret_val2 = INT_zero;
 NAMEMAP is_key_nil_self;
 OB is_key_nil_e;
 BOOL ret_val3 = BOOL_zero;
 TUPdOBSTR create_self = TUPdOBSTR_zero;
 OB create_at1;
 STR create_at2;
 TUPdOBSTR ret_val4 = TUPdOBSTR_zero;
 NAMEMAP is_key_nil_self1;
 OB is_key_nil_e1;
 BOOL ret_val5 = BOOL_zero;
 TUPdOBSTR create_self1 = TUPdOBSTR_zero;
 OB create_at11;
 STR create_at21;
 TUPdOBSTR ret_val6 = TUPdOBSTR_zero;
 TUPdOBSTR create_self2 = TUPdOBSTR_zero;
 OB create_at12;
 STR create_at22;
 TUPdOBSTR ret_val7 = TUPdOBSTR_zero;
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
 INT L141_;
 INT L15;
 INT L161_;
 INT L171_;
 INT L18;
 INT L191_;
 TUPdOBSTR L201_;
 TUPdOBSTR L21;
 TUPdOBSTR L22;
 TUPdOBSTR L23;
 TUPdOBSTR L24;
 INT L261_;
 BOOL L271_;
 TUPdOBSTR L281_;
 TUPdOBSTR L29;
 TUPdOBSTR L30;
 TUPdOBSTR L31;
 TUPdOBSTR L32;
 INT L341_;
 TUPdOBSTR L35;
 TUPdOBSTR L36;
 TUPdOBSTR L37;
 INT L39;
 INT L401_;
 r = self;
 if ((r==((NAMEMAP) NULL))) {
  r = NAMEMA1469296016(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((NAMEMAP)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = NAMEMA2064441237;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val1 = L91_;
  if (ret_val1) {
   r = NAMEMA54431949(self);
  }
 }
 key_hash_self = r;
 key_hash_e = k;
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L10 = ret_val2;
 L121_=ASIZE((NAMEMAP)r); 
 L13 = L121_;
 L141_=INTMINUS(L13,2); 
 L15 = L141_;
 L161_=L10&L15; 
 orig_h = L161_;
 h = orig_h;
 L171_=ASIZE((NAMEMAP)r); 
 L18 = L171_;
 L191_=INTMINUS(L18,1); 
 asm1 = L191_;
 while (1) {
  L201_=ARR((NAMEMAP)r,h); 
  L21=L201_;
  tk = L21.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  ret_val3 = ELT_NI455285430(((ELT_NILdOB) NULL), is_key_nil_e);
  if (ret_val3) {
   goto after_loop;
  }
  if (NAMEMA291126687(self, tk, k)) {
   create_self = TUPdOBSTR_zero;
   create_at1 = k;
   create_at2 = t;
   L22 = create_self;
   L22.t1 = create_at1;
   L23 = L22;
   L23.t2 = create_at2;
   ret_val4 = L23;
   L24 = ret_val4;
   SARR((NAMEMAP)r,h,L24); 
   ;
   ret_val = r;
   return ret_val;
  }
  L261_=INTPLUS(h,1); 
  h = L261_;
 }
 after_loop: ;
 L271_=(h)==(asm1); 
 if (L271_) {
  h = 0;
  while (1) {
   L281_=ARR((NAMEMAP)r,h); 
   L29=L281_;
   tk1 = L29.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   ret_val5 = ELT_NI455285430(((ELT_NILdOB) NULL), is_key_nil_e1);
   if (ret_val5) {
    goto after_loop1;
   }
   if (NAMEMA291126687(self, tk1, k)) {
    create_self1 = TUPdOBSTR_zero;
    create_at11 = k;
    create_at21 = t;
    L30 = create_self1;
    L30.t1 = create_at11;
    L31 = L30;
    L31.t2 = create_at21;
    ret_val6 = L31;
    L32 = ret_val6;
    SARR((NAMEMAP)r,h,L32); 
    ;
    ret_val = r;
    return ret_val;
   }
   L341_=INTPLUS(h,1); 
   h = L341_;
  }
  after_loop1: ;
 }
 create_self2 = TUPdOBSTR_zero;
 create_at12 = k;
 create_at22 = t;
 L35 = create_self2;
 L35.t1 = create_at12;
 L36 = L35;
 L36.t2 = create_at22;
 ret_val7 = L36;
 L37 = ret_val7;
 SARR((NAMEMAP)r,h,L37); 
 ;
 L39 = ATTR(r,hsize);
 L401_=INTPLUS(L39,1); 
 SATTR(r,hsize,L401_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

NAMEMAP NAMEMA54431949(NAMEMAP self) {
 NAMEMAP ret_val;
 INT ns = INT_zero;
 NAMEMAP r;
 NAMEMAP L11;
 NAMEMAP insert_pair_self;
 TUPdOBSTR insert_pair_p = TUPdOBSTR_zero;
 NAMEMAP ret_val1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPdOBSTR L9;
 L21_=ASIZE((NAMEMAP)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 ns = L81_;
 r = NAMEMA1469296016(self, ns);
 {
  struct NAMEMA726632978_frame_struct other1_0;
NAMEMA726632978_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   insert_pair_self = r;
   L9 = NAMEMA726632978(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   insert_pair_p = L9;
   ret_val1 = NAMEMA2020020771(insert_pair_self, insert_pair_p.t1, insert_pair_p.t2);
   r = ret_val1;
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

STR MANGLE119219986(MANGLE self, OB ob, OB ns) {
 STR ret_val;
 STR cname;
 IMPL C_name_val_self;
 STR ret_val1;
 MANGLE fast_mangle_self;
 OB fast_mangle_ob;
 OB fast_mangle_ns;
 STR ret_val2;
 NAMESPACE sp;
 STR res;
 MANGLE space_self;
 OB space_ns;
 NAMESPACE ret_val3;
 NAMESPACE r;
 NAMESPACE create_self;
 NAMESPACE ret_val4;
 NAMESPACE r1;
 NAMEMAP create_self1;
 NAMEMAP ret_val5;
 FSETSTR create_self2;
 FSETSTR ret_val6;
 FMAPSTRINT create_self3;
 FMAPSTRINT ret_val7;
 NAMESPACE get_self;
 OB get_ob;
 STR ret_val8;
 NAMESPACE insert_self;
 OB insert_ob;
 STR insert_nm;
 MANGLE fast_mangle_self1;
 OB fast_mangle_ob1;
 OB fast_mangle_ns1;
 STR ret_val9;
 NAMESPACE sp1;
 STR res1;
 MANGLE space_self1;
 OB space_ns1;
 NAMESPACE ret_val10;
 NAMESPACE r2;
 NAMESPACE create_self4;
 NAMESPACE ret_val11;
 NAMESPACE r3;
 NAMEMAP create_self5;
 NAMEMAP ret_val12;
 FSETSTR create_self6;
 FSETSTR ret_val13;
 FMAPSTRINT create_self7;
 FMAPSTRINT ret_val14;
 NAMESPACE get_self1;
 OB get_ob1;
 STR ret_val15;
 NAMESPACE insert_self1;
 OB insert_ob1;
 STR insert_nm1;
 BOOL L1;
 BOOL L21_;
 NAMESPACE L3;
 OB L4;
 NAMESPACE L5;
 OB L6;
 if (ATTR(ATTR(self,prog),temp_option_fast)) {
  if (ob==NULL) {
   goto other355;
  } else
  switch (TAG(ob)) {
   case TP_CLASS_tag:
    if (TP_CLA727826289(((TP_CLASS) ob))) {
     C_name_val_self = TP_CLA1109727233(((TP_CLASS) ob));
     ret_val1 = IMPL_s709672763(C_name_val_self, IDENT_857412936);
     cname = ret_val1;
     L1 = (cname==((STR) NULL));
     L21_=!(L1); 
     if (L21_) {
      MANGLE1827205184(self, ob, cname, ns);
      ret_val = cname;
      return ret_val;
     }
    }
    fast_mangle_self = self;
    fast_mangle_ob = ob;
    fast_mangle_ns = ns;
    space_self = fast_mangle_self;
    space_ns = fast_mangle_ns;
    if ((space_ns==((OB) NULL))) {
     space_ns = ((OB) space_self);
    }
    r = FMAPdO790518483(ATTR(space_self,namespaces), space_ns);
    if ((r==((NAMESPACE) NULL))) {
     create_self = ((NAMESPACE) NULL);
     L4=ZALLOC(sizeof(struct NAMESPACE_struct));
     if (L4==NULL) FATAL("Unable to allocate more memory");
     ((OB)L4)->header.tag=NAMESPACE_tag;
     L3 = ((NAMESPACE) L4);
     r1 = L3;
     create_self1 = ((NAMEMAP) NULL);
     ret_val5 = ((NAMEMAP) NULL);
     SATTR(r1,map,ret_val5);
     create_self2 = ((FSETSTR) NULL);
     ret_val6 = ((FSETSTR) NULL);
     SATTR(r1,set,ret_val6);
     SATTR(r1,counter,0);
     create_self3 = ((FMAPSTRINT) NULL);
     ret_val7 = ((FMAPSTRINT) NULL);
     SATTR(r1,uniques,ret_val7);
     ret_val4 = r1;
     r = ret_val4;
     SATTR(space_self,namespaces,FMAPdO1199466663(ATTR(space_self,namespaces), space_ns, r));
    }
    ret_val3 = r;
    sp = ret_val3;
    get_self = sp;
    get_ob = fast_mangle_ob;
    ret_val8 = NAMEMA1183587160(ATTR(get_self,map), get_ob);
    res = ret_val8;
    if ((res==((STR) NULL))) {
     res = FAST_I535598365(((FAST_IDENTIFIER) NULL));
     insert_self = sp;
     insert_ob = fast_mangle_ob;
     insert_nm = res;
     SATTR(insert_self,map,NAMEMA2020020771(ATTR(insert_self,map), insert_ob, insert_nm));
     SATTR(insert_self,set,FSETST1404644833(ATTR(insert_self,set), insert_nm));
    }
    ret_val2 = res;
    ret_val = ret_val2;
    return ret_val; break;
   case IDENT_tag:
    ret_val = MANGLE127610753(self, ob, ns);
    return ret_val; break;
   default: ;
   other355: ;
    fast_mangle_self1 = self;
    fast_mangle_ob1 = ob;
    fast_mangle_ns1 = ns;
    space_self1 = fast_mangle_self1;
    space_ns1 = fast_mangle_ns1;
    if ((space_ns1==((OB) NULL))) {
     space_ns1 = ((OB) space_self1);
    }
    r2 = FMAPdO790518483(ATTR(space_self1,namespaces), space_ns1);
    if ((r2==((NAMESPACE) NULL))) {
     create_self4 = ((NAMESPACE) NULL);
     L6=ZALLOC(sizeof(struct NAMESPACE_struct));
     if (L6==NULL) FATAL("Unable to allocate more memory");
     ((OB)L6)->header.tag=NAMESPACE_tag;
     L5 = ((NAMESPACE) L6);
     r3 = L5;
     create_self5 = ((NAMEMAP) NULL);
     ret_val12 = ((NAMEMAP) NULL);
     SATTR(r3,map,ret_val12);
     create_self6 = ((FSETSTR) NULL);
     ret_val13 = ((FSETSTR) NULL);
     SATTR(r3,set,ret_val13);
     SATTR(r3,counter,0);
     create_self7 = ((FMAPSTRINT) NULL);
     ret_val14 = ((FMAPSTRINT) NULL);
     SATTR(r3,uniques,ret_val14);
     ret_val11 = r3;
     r2 = ret_val11;
     SATTR(space_self1,namespaces,FMAPdO1199466663(ATTR(space_self1,namespaces), space_ns1, r2));
    }
    ret_val10 = r2;
    sp1 = ret_val10;
    get_self1 = sp1;
    get_ob1 = fast_mangle_ob1;
    ret_val15 = NAMEMA1183587160(ATTR(get_self1,map), get_ob1);
    res1 = ret_val15;
    if ((res1==((STR) NULL))) {
     res1 = FAST_I535598365(((FAST_IDENTIFIER) NULL));
     insert_self1 = sp1;
     insert_ob1 = fast_mangle_ob1;
     insert_nm1 = res1;
     SATTR(insert_self1,map,NAMEMA2020020771(ATTR(insert_self1,map), insert_ob1, insert_nm1));
     SATTR(insert_self1,set,FSETST1404644833(ATTR(insert_self1,set), insert_nm1));
    }
    ret_val9 = res1;
    ret_val = ret_val9;
    return ret_val;
  }
 }
 else {
  ret_val = MANGLE127610753(self, ob, ns);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR MANGLE127610753(MANGLE self, OB ob, OB ns) {
 STR ret_val;
 NAMESPACE sp;
 STR res;
 STR s = ((STR) NULL);
 BOOL is_local = BOOL_zero;
 BOOL is_attribute = BOOL_zero;
 ARRAYARG L11;
 STR cname;
 FSTR tmp1;
 STR L21;
 CHAR c = CHAR_zero;
 CHAR L31 = CHAR_zero;
 CHAR L41 = CHAR_zero;
 INT orig_res_len = INT_zero;
 INT max_usable_ident = INT_zero;
 STR reshash;
 INT maxprehash = INT_zero;
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
 NAMESPACE get_self;
 OB get_ob;
 STR ret_val6;
 INT L51 = INT_zero;
 IMPL C_name_val_self;
 STR ret_val7;
 STR plus_self;
 STR plus_sarg;
 STR ret_val8;
 STR plus_self1;
 CHAR plus_arg = CHAR_zero;
 STR ret_val9;
 STR s1;
 CHAR str_self = CHAR_zero;
 STR ret_val10;
 STR create_self4;
 CHAR create_c = CHAR_zero;
 STR ret_val11;
 STR r2;
 STR plus_self2;
 STR plus_sarg1;
 STR ret_val12;
 ERR1 create_self5;
 ERR1 ret_val13;
 ERR1 plus_self3;
 dSTR plus_s;
 ERR1 ret_val14;
 FILE1 stderr_self;
 FILE1 ret_val15;
 FILE1 r3;
 ERR1 plus_self4;
 dSTR plus_s1;
 FILE1 stderr_self1;
 FILE1 ret_val16;
 FILE1 r4;
 FSTR create_self6;
 FSTR ret_val17;
 FSTR plus_self5;
 CHAR plus_c = CHAR_zero;
 FSTR ret_val18;
 FSTR plus_self6;
 CHAR plus_c1 = CHAR_zero;
 FSTR ret_val19;
 FSTR plus_self7;
 CHAR plus_c2 = CHAR_zero;
 FSTR ret_val20;
 FSTR plus_self8;
 CHAR plus_c3 = CHAR_zero;
 FSTR ret_val21;
 FSTR plus_self9;
 CHAR plus_c4 = CHAR_zero;
 FSTR ret_val22;
 FSTR plus_self10;
 CHAR plus_c5 = CHAR_zero;
 FSTR ret_val23;
 FSTR plus_self11;
 CHAR plus_c6 = CHAR_zero;
 FSTR ret_val24;
 FSTR plus_self12;
 CHAR plus_c7 = CHAR_zero;
 FSTR ret_val25;
 FSTR plus_self13;
 CHAR plus_c8 = CHAR_zero;
 FSTR ret_val26;
 FSTR plus_self14;
 CHAR plus_c9 = CHAR_zero;
 FSTR ret_val27;
 FSTR plus_self15;
 CHAR plus_c10 = CHAR_zero;
 FSTR ret_val28;
 FSTR plus_self16;
 CHAR plus_c11 = CHAR_zero;
 FSTR ret_val29;
 FSTR plus_self17;
 CHAR plus_c12 = CHAR_zero;
 FSTR ret_val30;
 FSTR plus_self18;
 CHAR plus_c13 = CHAR_zero;
 FSTR ret_val31;
 FSTR plus_self19;
 CHAR plus_c14 = CHAR_zero;
 FSTR ret_val32;
 FSTR plus_self20;
 CHAR plus_c15 = CHAR_zero;
 FSTR ret_val33;
 FSTR plus_self21;
 CHAR plus_c16 = CHAR_zero;
 FSTR ret_val34;
 FSTR plus_self22;
 CHAR plus_c17 = CHAR_zero;
 FSTR ret_val35;
 FSTR plus_self23;
 CHAR plus_c18 = CHAR_zero;
 FSTR ret_val36;
 FSTR plus_self24;
 CHAR plus_c19 = CHAR_zero;
 FSTR ret_val37;
 FSTR plus_self25;
 CHAR plus_c20 = CHAR_zero;
 FSTR ret_val38;
 FSTR plus_self26;
 CHAR plus_c21 = CHAR_zero;
 FSTR ret_val39;
 FSTR plus_self27;
 CHAR plus_c22 = CHAR_zero;
 FSTR ret_val40;
 FSTR plus_self28;
 CHAR plus_c23 = CHAR_zero;
 FSTR ret_val41;
 FSTR plus_self29;
 CHAR plus_c24 = CHAR_zero;
 FSTR ret_val42;
 FSTR plus_self30;
 CHAR plus_c25 = CHAR_zero;
 FSTR ret_val43;
 FSTR plus_self31;
 CHAR plus_c26 = CHAR_zero;
 FSTR ret_val44;
 FSTR plus_self32;
 CHAR plus_c27 = CHAR_zero;
 FSTR ret_val45;
 FSTR plus_self33;
 CHAR plus_c28 = CHAR_zero;
 FSTR ret_val46;
 FSTR plus_self34;
 CHAR plus_c29 = CHAR_zero;
 FSTR ret_val47;
 FSTR plus_self35;
 CHAR plus_c30 = CHAR_zero;
 FSTR ret_val48;
 FSTR plus_self36;
 CHAR plus_c31 = CHAR_zero;
 FSTR ret_val49;
 FSTR plus_self37;
 CHAR plus_c32 = CHAR_zero;
 FSTR ret_val50;
 FSTR plus_self38;
 CHAR plus_c33 = CHAR_zero;
 FSTR ret_val51;
 FSTR plus_self39;
 CHAR plus_c34 = CHAR_zero;
 FSTR ret_val52;
 FSTR plus_self40;
 CHAR plus_c35 = CHAR_zero;
 FSTR ret_val53;
 FSTR plus_self41;
 CHAR plus_c36 = CHAR_zero;
 FSTR ret_val54;
 FSTR plus_self42;
 CHAR plus_c37 = CHAR_zero;
 FSTR ret_val55;
 FSTR plus_self43;
 CHAR plus_c38 = CHAR_zero;
 FSTR ret_val56;
 FSTR plus_self44;
 CHAR plus_c39 = CHAR_zero;
 FSTR ret_val57;
 FSTR plus_self45;
 CHAR plus_c40 = CHAR_zero;
 FSTR ret_val58;
 FSTR plus_self46;
 CHAR plus_c41 = CHAR_zero;
 FSTR ret_val59;
 FSTR plus_self47;
 CHAR plus_c42 = CHAR_zero;
 FSTR ret_val60;
 FSTR plus_self48;
 CHAR plus_c43 = CHAR_zero;
 FSTR ret_val61;
 FSTR plus_self49;
 CHAR plus_c44 = CHAR_zero;
 FSTR ret_val62;
 FSTR plus_self50;
 CHAR plus_c45 = CHAR_zero;
 FSTR ret_val63;
 FSTR plus_self51;
 CHAR plus_c46 = CHAR_zero;
 FSTR ret_val64;
 FSTR plus_self52;
 CHAR plus_c47 = CHAR_zero;
 FSTR ret_val65;
 FSTR plus_self53;
 CHAR plus_c48 = CHAR_zero;
 FSTR ret_val66;
 FSTR plus_self54;
 CHAR plus_c49 = CHAR_zero;
 FSTR ret_val67;
 FSTR plus_self55;
 CHAR plus_c50 = CHAR_zero;
 FSTR ret_val68;
 FSTR plus_self56;
 CHAR plus_c51 = CHAR_zero;
 FSTR ret_val69;
 FSTR plus_self57;
 CHAR plus_c52 = CHAR_zero;
 FSTR ret_val70;
 FSTR plus_self58;
 CHAR plus_c53 = CHAR_zero;
 FSTR ret_val71;
 FSTR plus_self59;
 CHAR plus_c54 = CHAR_zero;
 FSTR ret_val72;
 FSTR plus_self60;
 CHAR plus_c55 = CHAR_zero;
 FSTR ret_val73;
 FSTR plus_self61;
 CHAR plus_c56 = CHAR_zero;
 FSTR ret_val74;
 FSTR plus_self62;
 CHAR plus_c57 = CHAR_zero;
 FSTR ret_val75;
 FSTR plus_self63;
 CHAR plus_c58 = CHAR_zero;
 FSTR ret_val76;
 FSTR plus_self64;
 CHAR plus_c59 = CHAR_zero;
 FSTR ret_val77;
 FSTR plus_self65;
 CHAR plus_c60 = CHAR_zero;
 FSTR ret_val78;
 FSTR plus_self66;
 CHAR plus_c61 = CHAR_zero;
 FSTR ret_val79;
 FSTR plus_self67;
 CHAR plus_c62 = CHAR_zero;
 FSTR ret_val80;
 FSTR plus_self68;
 CHAR plus_c63 = CHAR_zero;
 FSTR ret_val81;
 FSTR plus_self69;
 CHAR plus_c64 = CHAR_zero;
 FSTR ret_val82;
 FSTR length_self;
 INT ret_val83 = INT_zero;
 FSTR aget_self;
 INT aget_ind = INT_zero;
 CHAR ret_val84 = CHAR_zero;
 FSTR create_self7;
 FSTR ret_val85;
 FSTR create_self8;
 FSTR ret_val86;
 FSTR create_self9;
 FSTR ret_val87;
 FSTR create_self10;
 FSTR ret_val88;
 FSTR create_self11;
 FSTR ret_val89;
 FSTR create_self12;
 FSTR ret_val90;
 FSTR create_self13;
 FSTR ret_val91;
 FSTR create_self14;
 FSTR ret_val92;
 FSTR create_self15;
 FSTR ret_val93;
 FSTR create_self16;
 FSTR ret_val94;
 FSTR create_self17;
 FSTR ret_val95;
 FSTR str_self1;
 STR ret_val96;
 INT str_self2 = INT_zero;
 STR ret_val97;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val98;
 FSTR str_self3;
 STR ret_val99;
 STR plus_self70;
 STR plus_sarg2;
 STR ret_val100;
 NAMESPACE is_taken_self;
 STR is_taken_s;
 BOOL ret_val101 = BOOL_zero;
 NAMESPACE is_taken_self1;
 STR is_taken_s1;
 BOOL ret_val102 = BOOL_zero;
 NAMESPACE insert_self;
 OB insert_ob;
 STR insert_nm;
 ERR1 create_self18;
 ERR1 ret_val103;
 ERR1 plus_self71;
 dSTR plus_s2;
 ERR1 ret_val104;
 FILE1 stderr_self2;
 FILE1 ret_val105;
 FILE1 r5;
 ERR1 plus_self72;
 dSTR plus_s3;
 ERR1 ret_val106;
 FILE1 stderr_self3;
 FILE1 ret_val107;
 FILE1 r6;
 ERR1 plus_self73;
 dSTR plus_s4;
 FILE1 stderr_self4;
 FILE1 ret_val108;
 FILE1 r7;
 NAMESPACE L6;
 OB L7;
 dTP L8;
 STR L9;
 extern STR S_;
 BOOL L10;
 BOOL L121_;
 INT L131_br;
 ARG aL131_;
 extern STR S_;
 ARG L14;
 INT L151_;
 BOOL L16;
 BOOL L171_;
 extern STR r211;
 dTP L18;
 extern STR L;
 BOOL L19;
 BOOL L201_;
 extern STR L;
 BOOL L22;
 BOOL L231_;
 extern STR bound;
 extern STR S_str;
 IDENT L24;
 dTP L25;
 STR L26;
 INT L27;
 OB L28;
 INT L29;
 extern STR after_loop;
 extern STR pS_simple_par_ob;
 extern STR pS_fork_handler;
 extern STR pS_at_handler;
 extern STR noname1;
 extern STR Staringwith;
 FILE1 L32;
 OB L33;
 FILE1 L35;
 OB L36;
 FSTR L38;
 INT L39;
 OB L40;
 INT L42;
 CHAR L43;
 INT L44;
 BOOL L451_;
 extern STR name74;
 CHAR L461_;
 FSTR L47;
 INT L48;
 OB L49;
 INT L50;
 extern STR S1;
 FSTR L52;
 FSTR L53;
 INT L54;
 OB L55;
 INT L56;
 extern STR S1;
 FSTR L57;
 FSTR L58;
 INT L59;
 OB L60;
 INT L61;
 extern STR S1;
 FSTR L62;
 FSTR L63;
 INT L64;
 OB L65;
 INT L66;
 extern STR S1;
 FSTR L67;
 FSTR L68;
 INT L69;
 OB L70;
 INT L71;
 extern STR S1;
 FSTR L72;
 FSTR L73;
 INT L74;
 OB L75;
 INT L76;
 extern STR S1;
 FSTR L77;
 FSTR L78;
 INT L79;
 OB L80;
 INT L81;
 extern STR S1;
 FSTR L82;
 FSTR L83;
 INT L84;
 OB L85;
 INT L86;
 extern STR S1;
 FSTR L87;
 FSTR L88;
 INT L89;
 OB L90;
 INT L91;
 extern STR S1;
 FSTR L92;
 FSTR L93;
 INT L94;
 OB L95;
 INT L96;
 extern STR S1;
 FSTR L97;
 FSTR L98;
 INT L99;
 OB L100;
 INT L101;
 extern STR S1;
 FSTR L102;
 INT L103;
 INT L1041_;
 BOOL L105;
 BOOL L106;
 BOOL L107;
 BOOL L1081_;
 BOOL L1091_;
 BOOL L1101_;
 INT L111;
 INT L1121_;
 BOOL L113;
 BOOL L1141_;
 INT L1151_;
 BOOL L1161_;
 BOOL L1171_;
 BOOL L118;
 BOOL L119;
 BOOL L120;
 BOOL L122;
 BOOL L123;
 BOOL L124;
 BOOL L1251_;
 BOOL L1261_;
 INT L127;
 INT L1281_;
 extern STR to1;
 FILE1 L129;
 OB L130;
 FILE1 L133;
 OB L134;
 extern STR name9;
 FILE1 L136;
 OB L137;
 space_self = self;
 space_ns = ns;
 if ((space_ns==((OB) NULL))) {
  space_ns = ((OB) space_self);
 }
 r = FMAPdO790518483(ATTR(space_self,namespaces), space_ns);
 if ((r==((NAMESPACE) NULL))) {
  create_self = ((NAMESPACE) NULL);
  L7=ZALLOC(sizeof(struct NAMESPACE_struct));
  if (L7==NULL) FATAL("Unable to allocate more memory");
  ((OB)L7)->header.tag=NAMESPACE_tag;
  L6 = ((NAMESPACE) L7);
  r1 = L6;
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
 sp = ret_val1;
 get_self = sp;
 get_ob = ob;
 ret_val6 = NAMEMA1183587160(ATTR(get_self,map), get_ob);
 res = ret_val6;
 if ((res==((STR) NULL))) {
  is_local = FALSE;
  is_attribute = FALSE;
  if (ob==NULL) {
   goto other354;
  } else
  switch (TAG(ob)) {
   case SIG_tag:
    L8 = ATTR(((SIG) ob),tp);
    L9 = (*dTP_strrSTR[TAG(L8)])(L8);
    s = STR_ap1693864410(L9, ((STR) &S_), ATTR(((SIG) ob),name1).str);
    L10 = (ATTR(((SIG) ob),args)==((ARRAYARG) NULL));
    L121_=!(L10); 
    if (L121_) {
     {
      BOOL f_L131_ = TRUE;
      /* loop index variable */
      L51 = 0;
      L11 = ATTR(((SIG) ob),args);
      L131_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
      while (1) {
       if(L51>=L131_br)  goto after_loop; 
       aL131_=ARR((ARRAYARG)L11,L51); 
       s = STR_ap1693864410(s, ((STR) &S_), ARG_strrSTR(aL131_));
       L151_=INTPLUS(L51,1); 
       L51 = L151_;
      }
     }
     after_loop: ;
    }
    L16 = (ATTR(((SIG) ob),ret)==((dTP) NULL));
    L171_=!(L16); 
    if (L171_) {
     L18 = ATTR(((SIG) ob),ret);
     s = STR_ap1693864410(s, ((STR) &r211), (*dTP_strrSTR[TAG(L18)])(L18));
    } break;
   case INT_tag:
    s = NAMESP180237388(sp, ((STR) &L));
    is_local = TRUE; break;
   case AM_LOCAL_EXPR_tag:
    L19 = (ATTR(((AM_LOCAL_EXPR) ob),name1).str==(STR)0);
    L201_=!(L19); 
    if (L201_) {
     s = ATTR(((AM_LOCAL_EXPR) ob),name1).str;
    }
    else {
     s = NAMESP180237388(sp, ((STR) &L));
    }
    is_local = TRUE; break;
   case TP_CLASS_tag:
    if (TP_CLA727826289(((TP_CLASS) ob))) {
     C_name_val_self = TP_CLA1109727233(((TP_CLASS) ob));
     ret_val7 = IMPL_s709672763(C_name_val_self, IDENT_857412936);
     cname = ret_val7;
     L22 = (cname==((STR) NULL));
     L231_=!(L22); 
     if (L231_) {
      MANGLE1827205184(self, ob, cname, ns);
      ret_val = cname;
      return ret_val;
     }
    }
    s = TP_CLASS_strrSTR(((TP_CLASS) ob)); break;
   case TP_ROUT_tag:
    s = TP_ROUT_strrSTR(((TP_ROUT) ob)); break;
   case TP_ITER_tag:
    s = TP_ITER_strrSTR(((TP_ITER) ob)); break;
   case AM_BND1124877163_tag:
    s = ((STR) &bound); break;
   case STR_tag:
    plus_self = ((STR) ob);
    plus_sarg = ((STR) &S_str);
    ret_val8 = STR_ap2004550586(plus_self, plus_sarg);
    s = ret_val8; break;
   case IDENT_tag:
    L24 = ((IDENT_boxed) ob)->immutable_part;
    s = L24.str;
    is_attribute = TRUE; break;
   case AM_SHARED_EXPR_tag:
    L25 = ATTR(((AM_SHARED_EXPR) ob),class_tp);
    plus_self1 = (*dTP_strrSTR[TAG(L25)])(L25);
    plus_arg = '_';
    str_self = plus_arg;
    create_self4 = ((STR) NULL);
    create_c = str_self;
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
    SARR((STR)r2,0,create_c); 
    ;
    ret_val11 = r2;
    ret_val10 = ret_val11;
    s1 = ret_val10;
    ret_val9 = STR_ap1077157958(plus_self1, s1, TRUE);
    plus_self2 = ret_val9;
    plus_sarg1 = ATTR(((AM_SHARED_EXPR) ob),name1).str;
    ret_val12 = STR_ap2004550586(plus_self2, plus_sarg1);
    s = ret_val12; break;
   case AM_LOOP_STMT_tag:
    s = ((STR) &after_loop); break;
   case AM_PAR_STMT_tag:
    s = ((STR) &pS_simple_par_ob); break;
   case AM_FORK_STMT_tag:
    s = ((STR) &pS_fork_handler); break;
   case AM_AT_EXPR_tag:
    s = ((STR) &pS_at_handler); break;
   case AM_STR_CONST_tag:
    s = ATTR(((AM_STR_CONST) ob),bval); break;
   default: ;
   other354: ;
  }
  if ((s==((STR) NULL))) {
   res = NAMESP8552102(sp, ((STR) &noname1));
  }
  else {
   if (ATTR(ATTR(self,prog),debug_mangle)) {
    create_self5 = ((ERR1) NULL);
    ret_val13 = create_self5;
    plus_self3 = ret_val13;
    plus_s = ((dSTR) ((STR) &Staringwith));
    stderr_self = ((FILE1) NULL);
    L33=ZALLOC(sizeof(struct FILE1_struct));
    if (L33==NULL) FATAL("Unable to allocate more memory");
    ((OB)L33)->header.tag=FILE1_tag;
    L32 = ((FILE1) L33);
    r3 = L32;
    
    SATTR(r3,fp,stderr);
    ret_val15 = r3;
    FILE_plus_dSTR(ret_val15, plus_s);
    ret_val14 = plus_self3;
    plus_self4 = ret_val14;
    plus_s1 = ((dSTR) s);
    stderr_self1 = ((FILE1) NULL);
    L36=ZALLOC(sizeof(struct FILE1_struct));
    if (L36==NULL) FATAL("Unable to allocate more memory");
    ((OB)L36)->header.tag=FILE1_tag;
    L35 = ((FILE1) L36);
    r4 = L35;
    
    SATTR(r4,fp,stderr);
    ret_val16 = r4;
    FILE_plus_dSTR(ret_val16, plus_s1);
   }
   create_self6 = ((FSTR) NULL);
   L39 = 16;
   L42=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L39)*sizeof(CHAR);
   L40=ZALLOC_LEAF_BIG(L42);
   if (L40==NULL) FATAL("Unable to allocate more memory");
   memset(L40,0,L42);
   ((OB)L40)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

     ((OB)L40)->header.destroyed=0;
#endif

   L38 = ((FSTR) L40);
   L38->asize = L39;
   ret_val17 = L38;
   tmp1 = ret_val17;
   {
    struct STR_eltbrCHAR_frame_struct other2_0;
STR_eltbrCHAR_frame noname1 = &other2_0;
    L21 = s;
    noname1->self = L21;
    noname1->state = 0;
    while (1) {
     L43 = STR_eltbrCHAR(noname1);
     if (noname1->state == -1) {
      goto after_loop1;
     }
     c = L43;
     L31 = c;
     switch (L31) {
      case 'a': 
       plus_self5 = tmp1;
       plus_c = c;
       ret_val18 = FSTR_p399773021(plus_self5, plus_c);
       tmp1 = ret_val18;
       break;
      case 'b': 
       plus_self6 = tmp1;
       plus_c1 = c;
       ret_val19 = FSTR_p399773021(plus_self6, plus_c1);
       tmp1 = ret_val19;
       break;
      case 'c': 
       plus_self7 = tmp1;
       plus_c2 = c;
       ret_val20 = FSTR_p399773021(plus_self7, plus_c2);
       tmp1 = ret_val20;
       break;
      case 'd': 
       plus_self8 = tmp1;
       plus_c3 = c;
       ret_val21 = FSTR_p399773021(plus_self8, plus_c3);
       tmp1 = ret_val21;
       break;
      case 'e': 
       plus_self9 = tmp1;
       plus_c4 = c;
       ret_val22 = FSTR_p399773021(plus_self9, plus_c4);
       tmp1 = ret_val22;
       break;
      case 'f': 
       plus_self10 = tmp1;
       plus_c5 = c;
       ret_val23 = FSTR_p399773021(plus_self10, plus_c5);
       tmp1 = ret_val23;
       break;
      case 'g': 
       plus_self11 = tmp1;
       plus_c6 = c;
       ret_val24 = FSTR_p399773021(plus_self11, plus_c6);
       tmp1 = ret_val24;
       break;
      case 'h': 
       plus_self12 = tmp1;
       plus_c7 = c;
       ret_val25 = FSTR_p399773021(plus_self12, plus_c7);
       tmp1 = ret_val25;
       break;
      case 'i': 
       plus_self13 = tmp1;
       plus_c8 = c;
       ret_val26 = FSTR_p399773021(plus_self13, plus_c8);
       tmp1 = ret_val26;
       break;
      case 'j': 
       plus_self14 = tmp1;
       plus_c9 = c;
       ret_val27 = FSTR_p399773021(plus_self14, plus_c9);
       tmp1 = ret_val27;
       break;
      case 'k': 
       plus_self15 = tmp1;
       plus_c10 = c;
       ret_val28 = FSTR_p399773021(plus_self15, plus_c10);
       tmp1 = ret_val28;
       break;
      case 'l': 
       plus_self16 = tmp1;
       plus_c11 = c;
       ret_val29 = FSTR_p399773021(plus_self16, plus_c11);
       tmp1 = ret_val29;
       break;
      case 'm': 
       plus_self17 = tmp1;
       plus_c12 = c;
       ret_val30 = FSTR_p399773021(plus_self17, plus_c12);
       tmp1 = ret_val30;
       break;
      case 'n': 
       plus_self18 = tmp1;
       plus_c13 = c;
       ret_val31 = FSTR_p399773021(plus_self18, plus_c13);
       tmp1 = ret_val31;
       break;
      case 'o': 
       plus_self19 = tmp1;
       plus_c14 = c;
       ret_val32 = FSTR_p399773021(plus_self19, plus_c14);
       tmp1 = ret_val32;
       break;
      case 'p': 
       plus_self20 = tmp1;
       plus_c15 = c;
       ret_val33 = FSTR_p399773021(plus_self20, plus_c15);
       tmp1 = ret_val33;
       break;
      case 'q': 
       plus_self21 = tmp1;
       plus_c16 = c;
       ret_val34 = FSTR_p399773021(plus_self21, plus_c16);
       tmp1 = ret_val34;
       break;
      case 'r': 
       plus_self22 = tmp1;
       plus_c17 = c;
       ret_val35 = FSTR_p399773021(plus_self22, plus_c17);
       tmp1 = ret_val35;
       break;
      case 's': 
       plus_self23 = tmp1;
       plus_c18 = c;
       ret_val36 = FSTR_p399773021(plus_self23, plus_c18);
       tmp1 = ret_val36;
       break;
      case 't': 
       plus_self24 = tmp1;
       plus_c19 = c;
       ret_val37 = FSTR_p399773021(plus_self24, plus_c19);
       tmp1 = ret_val37;
       break;
      case 'u': 
       plus_self25 = tmp1;
       plus_c20 = c;
       ret_val38 = FSTR_p399773021(plus_self25, plus_c20);
       tmp1 = ret_val38;
       break;
      case 'v': 
       plus_self26 = tmp1;
       plus_c21 = c;
       ret_val39 = FSTR_p399773021(plus_self26, plus_c21);
       tmp1 = ret_val39;
       break;
      case 'w': 
       plus_self27 = tmp1;
       plus_c22 = c;
       ret_val40 = FSTR_p399773021(plus_self27, plus_c22);
       tmp1 = ret_val40;
       break;
      case 'x': 
       plus_self28 = tmp1;
       plus_c23 = c;
       ret_val41 = FSTR_p399773021(plus_self28, plus_c23);
       tmp1 = ret_val41;
       break;
      case 'y': 
       plus_self29 = tmp1;
       plus_c24 = c;
       ret_val42 = FSTR_p399773021(plus_self29, plus_c24);
       tmp1 = ret_val42;
       break;
      case 'z': 
       plus_self30 = tmp1;
       plus_c25 = c;
       ret_val43 = FSTR_p399773021(plus_self30, plus_c25);
       tmp1 = ret_val43;
       break;
      case '0': 
       plus_self31 = tmp1;
       plus_c26 = c;
       ret_val44 = FSTR_p399773021(plus_self31, plus_c26);
       tmp1 = ret_val44;
       break;
      case '1': 
       plus_self32 = tmp1;
       plus_c27 = c;
       ret_val45 = FSTR_p399773021(plus_self32, plus_c27);
       tmp1 = ret_val45;
       break;
      case '2': 
       plus_self33 = tmp1;
       plus_c28 = c;
       ret_val46 = FSTR_p399773021(plus_self33, plus_c28);
       tmp1 = ret_val46;
       break;
      case '3': 
       plus_self34 = tmp1;
       plus_c29 = c;
       ret_val47 = FSTR_p399773021(plus_self34, plus_c29);
       tmp1 = ret_val47;
       break;
      case '4': 
       plus_self35 = tmp1;
       plus_c30 = c;
       ret_val48 = FSTR_p399773021(plus_self35, plus_c30);
       tmp1 = ret_val48;
       break;
      case '5': 
       plus_self36 = tmp1;
       plus_c31 = c;
       ret_val49 = FSTR_p399773021(plus_self36, plus_c31);
       tmp1 = ret_val49;
       break;
      case '6': 
       plus_self37 = tmp1;
       plus_c32 = c;
       ret_val50 = FSTR_p399773021(plus_self37, plus_c32);
       tmp1 = ret_val50;
       break;
      case '7': 
       plus_self38 = tmp1;
       plus_c33 = c;
       ret_val51 = FSTR_p399773021(plus_self38, plus_c33);
       tmp1 = ret_val51;
       break;
      case '8': 
       plus_self39 = tmp1;
       plus_c34 = c;
       ret_val52 = FSTR_p399773021(plus_self39, plus_c34);
       tmp1 = ret_val52;
       break;
      case '9': 
       plus_self40 = tmp1;
       plus_c35 = c;
       ret_val53 = FSTR_p399773021(plus_self40, plus_c35);
       tmp1 = ret_val53;
       break;
      case '_': 
       plus_self41 = tmp1;
       plus_c36 = c;
       ret_val54 = FSTR_p399773021(plus_self41, plus_c36);
       tmp1 = ret_val54;
       break;
      case 'A': 
       plus_self42 = tmp1;
       plus_c37 = c;
       ret_val55 = FSTR_p399773021(plus_self42, plus_c37);
       tmp1 = ret_val55;
       break;
      case 'B': 
       plus_self43 = tmp1;
       plus_c38 = c;
       ret_val56 = FSTR_p399773021(plus_self43, plus_c38);
       tmp1 = ret_val56;
       break;
      case 'C': 
       plus_self44 = tmp1;
       plus_c39 = c;
       ret_val57 = FSTR_p399773021(plus_self44, plus_c39);
       tmp1 = ret_val57;
       break;
      case 'D': 
       plus_self45 = tmp1;
       plus_c40 = c;
       ret_val58 = FSTR_p399773021(plus_self45, plus_c40);
       tmp1 = ret_val58;
       break;
      case 'E': 
       plus_self46 = tmp1;
       plus_c41 = c;
       ret_val59 = FSTR_p399773021(plus_self46, plus_c41);
       tmp1 = ret_val59;
       break;
      case 'F': 
       plus_self47 = tmp1;
       plus_c42 = c;
       ret_val60 = FSTR_p399773021(plus_self47, plus_c42);
       tmp1 = ret_val60;
       break;
      case 'G': 
       plus_self48 = tmp1;
       plus_c43 = c;
       ret_val61 = FSTR_p399773021(plus_self48, plus_c43);
       tmp1 = ret_val61;
       break;
      case 'H': 
       plus_self49 = tmp1;
       plus_c44 = c;
       ret_val62 = FSTR_p399773021(plus_self49, plus_c44);
       tmp1 = ret_val62;
       break;
      case 'I': 
       plus_self50 = tmp1;
       plus_c45 = c;
       ret_val63 = FSTR_p399773021(plus_self50, plus_c45);
       tmp1 = ret_val63;
       break;
      case 'J': 
       plus_self51 = tmp1;
       plus_c46 = c;
       ret_val64 = FSTR_p399773021(plus_self51, plus_c46);
       tmp1 = ret_val64;
       break;
      case 'K': 
       plus_self52 = tmp1;
       plus_c47 = c;
       ret_val65 = FSTR_p399773021(plus_self52, plus_c47);
       tmp1 = ret_val65;
       break;
      case 'L': 
       plus_self53 = tmp1;
       plus_c48 = c;
       ret_val66 = FSTR_p399773021(plus_self53, plus_c48);
       tmp1 = ret_val66;
       break;
      case 'M': 
       plus_self54 = tmp1;
       plus_c49 = c;
       ret_val67 = FSTR_p399773021(plus_self54, plus_c49);
       tmp1 = ret_val67;
       break;
      case 'N': 
       plus_self55 = tmp1;
       plus_c50 = c;
       ret_val68 = FSTR_p399773021(plus_self55, plus_c50);
       tmp1 = ret_val68;
       break;
      case 'O': 
       plus_self56 = tmp1;
       plus_c51 = c;
       ret_val69 = FSTR_p399773021(plus_self56, plus_c51);
       tmp1 = ret_val69;
       break;
      case 'P': 
       plus_self57 = tmp1;
       plus_c52 = c;
       ret_val70 = FSTR_p399773021(plus_self57, plus_c52);
       tmp1 = ret_val70;
       break;
      case 'Q': 
       plus_self58 = tmp1;
       plus_c53 = c;
       ret_val71 = FSTR_p399773021(plus_self58, plus_c53);
       tmp1 = ret_val71;
       break;
      case 'R': 
       plus_self59 = tmp1;
       plus_c54 = c;
       ret_val72 = FSTR_p399773021(plus_self59, plus_c54);
       tmp1 = ret_val72;
       break;
      case 'S': 
       plus_self60 = tmp1;
       plus_c55 = c;
       ret_val73 = FSTR_p399773021(plus_self60, plus_c55);
       tmp1 = ret_val73;
       break;
      case 'T': 
       plus_self61 = tmp1;
       plus_c56 = c;
       ret_val74 = FSTR_p399773021(plus_self61, plus_c56);
       tmp1 = ret_val74;
       break;
      case 'U': 
       plus_self62 = tmp1;
       plus_c57 = c;
       ret_val75 = FSTR_p399773021(plus_self62, plus_c57);
       tmp1 = ret_val75;
       break;
      case 'V': 
       plus_self63 = tmp1;
       plus_c58 = c;
       ret_val76 = FSTR_p399773021(plus_self63, plus_c58);
       tmp1 = ret_val76;
       break;
      case 'W': 
       plus_self64 = tmp1;
       plus_c59 = c;
       ret_val77 = FSTR_p399773021(plus_self64, plus_c59);
       tmp1 = ret_val77;
       break;
      case 'X': 
       plus_self65 = tmp1;
       plus_c60 = c;
       ret_val78 = FSTR_p399773021(plus_self65, plus_c60);
       tmp1 = ret_val78;
       break;
      case 'Y': 
       plus_self66 = tmp1;
       plus_c61 = c;
       ret_val79 = FSTR_p399773021(plus_self66, plus_c61);
       tmp1 = ret_val79;
       break;
      case 'Z': 
       plus_self67 = tmp1;
       plus_c62 = c;
       ret_val80 = FSTR_p399773021(plus_self67, plus_c62);
       tmp1 = ret_val80;
       break;
      case '$': 
       plus_self68 = tmp1;
       plus_c63 = 'd';
       ret_val81 = FSTR_p399773021(plus_self68, plus_c63);
       tmp1 = ret_val81;
       break;
      case '!': 
       plus_self69 = tmp1;
       plus_c64 = 'b';
       ret_val82 = FSTR_p399773021(plus_self69, plus_c64);
       tmp1 = ret_val82;
       break;
      default: ;
     }
    }
   }
   after_loop1: ;
   length_self = tmp1;
   ret_val83 = FSTR_sizerINT(length_self);
   L44 = ret_val83;
   L451_=(L44)==(0); 
   if (L451_) {
    tmp1 = FSTR_p1752847026(tmp1, ((STR) &name74));
   }
   aget_self = tmp1;
   aget_ind = 0;
   L461_=ARR((FSTR)aget_self,aget_ind); 
   ret_val84 = L461_;
   L41 = ret_val84;
   switch (L41) {
    case '0': 
     create_self7 = ((FSTR) NULL);
     L48 = 16;
     L50=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L48)*sizeof(CHAR);
     L49=ZALLOC_LEAF_BIG(L50);
     if (L49==NULL) FATAL("Unable to allocate more memory");
     memset(L49,0,L50);
     ((OB)L49)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

       ((OB)L49)->header.destroyed=0;
#endif

     L47 = ((FSTR) L49);
     L47->asize = L48;
     ret_val85 = L47;
     L52 = FSTR_p1752847026(ret_val85, ((STR) &S1));
     tmp1 = FSTR_p1410513982(L52, tmp1);
     break;
    case '1': 
     create_self8 = ((FSTR) NULL);
     L54 = 16;
     L56=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L54)*sizeof(CHAR);
     L55=ZALLOC_LEAF_BIG(L56);
     if (L55==NULL) FATAL("Unable to allocate more memory");
     memset(L55,0,L56);
     ((OB)L55)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

       ((OB)L55)->header.destroyed=0;
#endif

     L53 = ((FSTR) L55);
     L53->asize = L54;
     ret_val86 = L53;
     L57 = FSTR_p1752847026(ret_val86, ((STR) &S1));
     tmp1 = FSTR_p1410513982(L57, tmp1);
     break;
    case '2': 
     create_self9 = ((FSTR) NULL);
     L59 = 16;
     L61=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L59)*sizeof(CHAR);
     L60=ZALLOC_LEAF_BIG(L61);
     if (L60==NULL) FATAL("Unable to allocate more memory");
     memset(L60,0,L61);
     ((OB)L60)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

       ((OB)L60)->header.destroyed=0;
#endif

     L58 = ((FSTR) L60);
     L58->asize = L59;
     ret_val87 = L58;
     L62 = FSTR_p1752847026(ret_val87, ((STR) &S1));
     tmp1 = FSTR_p1410513982(L62, tmp1);
     break;
    case '3': 
     create_self10 = ((FSTR) NULL);
     L64 = 16;
     L66=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L64)*sizeof(CHAR);
     L65=ZALLOC_LEAF_BIG(L66);
     if (L65==NULL) FATAL("Unable to allocate more memory");
     memset(L65,0,L66);
     ((OB)L65)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

       ((OB)L65)->header.destroyed=0;
#endif

     L63 = ((FSTR) L65);
     L63->asize = L64;
     ret_val88 = L63;
     L67 = FSTR_p1752847026(ret_val88, ((STR) &S1));
     tmp1 = FSTR_p1410513982(L67, tmp1);
     break;
    case '4': 
     create_self11 = ((FSTR) NULL);
     L69 = 16;
     L71=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L69)*sizeof(CHAR);
     L70=ZALLOC_LEAF_BIG(L71);
     if (L70==NULL) FATAL("Unable to allocate more memory");
     memset(L70,0,L71);
     ((OB)L70)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

       ((OB)L70)->header.destroyed=0;
#endif

     L68 = ((FSTR) L70);
     L68->asize = L69;
     ret_val89 = L68;
     L72 = FSTR_p1752847026(ret_val89, ((STR) &S1));
     tmp1 = FSTR_p1410513982(L72, tmp1);
     break;
    case '5': 
     create_self12 = ((FSTR) NULL);
     L74 = 16;
     L76=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L74)*sizeof(CHAR);
     L75=ZALLOC_LEAF_BIG(L76);
     if (L75==NULL) FATAL("Unable to allocate more memory");
     memset(L75,0,L76);
     ((OB)L75)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

       ((OB)L75)->header.destroyed=0;
#endif

     L73 = ((FSTR) L75);
     L73->asize = L74;
     ret_val90 = L73;
     L77 = FSTR_p1752847026(ret_val90, ((STR) &S1));
     tmp1 = FSTR_p1410513982(L77, tmp1);
     break;
    case '6': 
     create_self13 = ((FSTR) NULL);
     L79 = 16;
     L81=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L79)*sizeof(CHAR);
     L80=ZALLOC_LEAF_BIG(L81);
     if (L80==NULL) FATAL("Unable to allocate more memory");
     memset(L80,0,L81);
     ((OB)L80)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

       ((OB)L80)->header.destroyed=0;
#endif

     L78 = ((FSTR) L80);
     L78->asize = L79;
     ret_val91 = L78;
     L82 = FSTR_p1752847026(ret_val91, ((STR) &S1));
     tmp1 = FSTR_p1410513982(L82, tmp1);
     break;
    case '7': 
     create_self14 = ((FSTR) NULL);
     L84 = 16;
     L86=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L84)*sizeof(CHAR);
     L85=ZALLOC_LEAF_BIG(L86);
     if (L85==NULL) FATAL("Unable to allocate more memory");
     memset(L85,0,L86);
     ((OB)L85)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

       ((OB)L85)->header.destroyed=0;
#endif

     L83 = ((FSTR) L85);
     L83->asize = L84;
     ret_val92 = L83;
     L87 = FSTR_p1752847026(ret_val92, ((STR) &S1));
     tmp1 = FSTR_p1410513982(L87, tmp1);
     break;
    case '8': 
     create_self15 = ((FSTR) NULL);
     L89 = 16;
     L91=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L89)*sizeof(CHAR);
     L90=ZALLOC_LEAF_BIG(L91);
     if (L90==NULL) FATAL("Unable to allocate more memory");
     memset(L90,0,L91);
     ((OB)L90)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

       ((OB)L90)->header.destroyed=0;
#endif

     L88 = ((FSTR) L90);
     L88->asize = L89;
     ret_val93 = L88;
     L92 = FSTR_p1752847026(ret_val93, ((STR) &S1));
     tmp1 = FSTR_p1410513982(L92, tmp1);
     break;
    case '9': 
     create_self16 = ((FSTR) NULL);
     L94 = 16;
     L96=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L94)*sizeof(CHAR);
     L95=ZALLOC_LEAF_BIG(L96);
     if (L95==NULL) FATAL("Unable to allocate more memory");
     memset(L95,0,L96);
     ((OB)L95)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

       ((OB)L95)->header.destroyed=0;
#endif

     L93 = ((FSTR) L95);
     L93->asize = L94;
     ret_val94 = L93;
     L97 = FSTR_p1752847026(ret_val94, ((STR) &S1));
     tmp1 = FSTR_p1410513982(L97, tmp1);
     break;
    case '_': 
     create_self17 = ((FSTR) NULL);
     L99 = 16;
     L101=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L99)*sizeof(CHAR);
     L100=ZALLOC_LEAF_BIG(L101);
     if (L100==NULL) FATAL("Unable to allocate more memory");
     memset(L100,0,L101);
     ((OB)L100)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

       ((OB)L100)->header.destroyed=0;
#endif

     L98 = ((FSTR) L100);
     L98->asize = L99;
     ret_val95 = L98;
     L102 = FSTR_p1752847026(ret_val95, ((STR) &S1));
     tmp1 = FSTR_p1410513982(L102, tmp1);
     break;
    default: ;
   }
   str_self1 = tmp1;
   ret_val96 = STR_fr1097270334(((STR) NULL), str_self1);
   res = ret_val96;
   orig_res_len = STR_sizerINT(res);
   L103 = ATTR(self,max_identifier_l);
   L1041_=INTMINUS(L103,8); 
   max_usable_ident = L1041_;
   L1081_=!(is_attribute); 
   if (L1081_) {
    L1091_=!(is_local); 
    L107 = L1091_;
   } else {
    L107 = FALSE;
   }
   if (L107) {
    L106 = ATTR(self,append_hash_to_i);
   } else {
    L106 = FALSE;
   }
   if (L106) {
    L1101_=(max_usable_ident)<(orig_res_len); 
    L105 = L1101_;
   } else {
    L105 = FALSE;
   }
   if (L105) {
    L111 = STR_hashrINT(res);
    L1121_=(L111<0)?-L111:L111; 
    str_self2 = L1121_;
    clear_self = buf1;
    L113 = (clear_self==((FSTR) NULL));
    L1141_=!(L113); 
    if (L1141_) {
     SATTR(clear_self,loc,0);
    }
    str_in_self = str_self2;
    str_in_s = buf1;
    ret_val98 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
    buf1 = ret_val98;
    str_self3 = buf1;
    ret_val99 = STR_fr1097270334(((STR) NULL), str_self3);
    ret_val97 = ret_val99;
    reshash = ret_val97;
    L1151_=INTMINUS(max_usable_ident,10); 
    maxprehash = L1151_;
    L1161_=(maxprehash)<(orig_res_len); 
    if (L1161_) {
     res = STR_head_INTrSTR(res, maxprehash);
    }
    plus_self70 = res;
    plus_sarg2 = reshash;
    ret_val100 = STR_ap2004550586(plus_self70, plus_sarg2);
    res = ret_val100;
   }
   else {
    L1171_=(max_usable_ident)<(orig_res_len); 
    if (L1171_) {
     res = STR_head_INTrSTR(res, max_usable_ident);
    }
   }
   while (1) {
    is_taken_self = sp;
    is_taken_s = res;
    ret_val101 = FSETST1025458804(ATTR(is_taken_self,set), is_taken_s);
    if (ret_val101) {
     L120 = TRUE;
    } else {
     L120 = FSETST1025458804(ATTR(self,forbidden), res);
    }
    if (L120) {
     L119 = TRUE;
    } else {
     if (is_local) {
      is_taken_self1 = ATTR(self,global_space);
      is_taken_s1 = res;
      ret_val102 = FSETST1025458804(ATTR(is_taken_self1,set), is_taken_s1);
      L122 = ret_val102;
     } else {
      L122 = FALSE;
     }
     L119 = L122;
    }
    if (L119) {
     L118 = TRUE;
    } else {
     L1251_=!(is_local); 
     if (L1251_) {
      L1261_=!(is_attribute); 
      L124 = L1261_;
     } else {
      L124 = FALSE;
     }
     if (L124) {
      L123 = FSETST1025458804(ATTR(self,used_by_local), res);
     } else {
      L123 = FALSE;
     }
     L118 = L123;
    }
    if (L118) {
    }
    else {
     goto after_loop2;
    }
    L127 = STR_lengthrINT(res);
    L1281_=(max_usable_ident<L127)?max_usable_ident:L127; 
    res = NAMESP8552102(sp, STR_head_INTrSTR(res, L1281_));
   }
   after_loop2: ;
  }
  insert_self = sp;
  insert_ob = ob;
  insert_nm = res;
  SATTR(insert_self,map,NAMEMA2020020771(ATTR(insert_self,map), insert_ob, insert_nm));
  SATTR(insert_self,set,FSETST1404644833(ATTR(insert_self,set), insert_nm));
  if (is_local) {
   SATTR(self,used_by_local,FSETST1404644833(ATTR(self,used_by_local), res));
  }
 }
 if (ATTR(ATTR(self,prog),debug_mangle)) {
  create_self18 = ((ERR1) NULL);
  ret_val103 = create_self18;
  plus_self71 = ret_val103;
  plus_s2 = ((dSTR) ((STR) &to1));
  stderr_self2 = ((FILE1) NULL);
  L130=ZALLOC(sizeof(struct FILE1_struct));
  if (L130==NULL) FATAL("Unable to allocate more memory");
  ((OB)L130)->header.tag=FILE1_tag;
  L129 = ((FILE1) L130);
  r5 = L129;
  
  SATTR(r5,fp,stderr);
  ret_val105 = r5;
  FILE_plus_dSTR(ret_val105, plus_s2);
  ret_val104 = plus_self71;
  plus_self72 = ret_val104;
  plus_s3 = ((dSTR) res);
  stderr_self3 = ((FILE1) NULL);
  L134=ZALLOC(sizeof(struct FILE1_struct));
  if (L134==NULL) FATAL("Unable to allocate more memory");
  ((OB)L134)->header.tag=FILE1_tag;
  L133 = ((FILE1) L134);
  r6 = L133;
  
  SATTR(r6,fp,stderr);
  ret_val107 = r6;
  FILE_plus_dSTR(ret_val107, plus_s3);
  ret_val106 = plus_self72;
  plus_self73 = ret_val106;
  plus_s4 = ((dSTR) ((STR) &name9));
  stderr_self4 = ((FILE1) NULL);
  L137=ZALLOC(sizeof(struct FILE1_struct));
  if (L137==NULL) FATAL("Unable to allocate more memory");
  ((OB)L137)->header.tag=FILE1_tag;
  L136 = ((FILE1) L137);
  r7 = L136;
  
  SATTR(r7,fp,stderr);
  ret_val108 = r7;
  FILE_plus_dSTR(ret_val108, plus_s4);
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR MANGLE1995580116(MANGLE self, STR arg) {
 STR ret_val;
 FSTR res;
 STR L11;
 FSTR create_self;
 FSTR ret_val1;
 FSTR str_self;
 STR ret_val2;
 FSTR L2;
 INT L3;
 OB L4;
 INT L5;
 CHAR L6;
 create_self = ((FSTR) NULL);
 L3 = 16;
 L5=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L3)*sizeof(CHAR);
 L4=ZALLOC_LEAF_BIG(L5);
 if (L4==NULL) FATAL("Unable to allocate more memory");
 memset(L4,0,L5);
 ((OB)L4)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L4)->header.destroyed=0;
#endif

 L2 = ((FSTR) L4);
 L2->asize = L3;
 ret_val1 = L2;
 res = ret_val1;
 {
  struct STR_eltbrCHAR_frame_struct other1_0;
STR_eltbrCHAR_frame noname1 = &other1_0;
  L11 = arg;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L6 = STR_eltbrCHAR(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   res = FSTR_p1752847026(res, MANGLE868076060(self, L6));
  }
 }
 after_loop: ;
 str_self = res;
 ret_val2 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val2;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR MANGLE868076060(MANGLE self, CHAR c) {
 STR ret_val;
 STR res;
 CHAR L11 = CHAR_zero;
 STR oc;
 STR plus_self;
 CHAR plus_arg = CHAR_zero;
 STR ret_val1;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val2;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val3;
 STR r;
 STR plus_self1;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val4;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val5;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val6;
 STR r1;
 STR plus_self2;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val7;
 STR s2;
 CHAR str_self2 = CHAR_zero;
 STR ret_val8;
 STR create_self2;
 CHAR create_c2 = CHAR_zero;
 STR ret_val9;
 STR r2;
 STR plus_self3;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val10;
 STR s3;
 CHAR str_self3 = CHAR_zero;
 STR ret_val11;
 STR create_self3;
 CHAR create_c3 = CHAR_zero;
 STR ret_val12;
 STR r3;
 STR plus_self4;
 CHAR plus_arg4 = CHAR_zero;
 STR ret_val13;
 STR s4;
 CHAR str_self4 = CHAR_zero;
 STR ret_val14;
 STR create_self4;
 CHAR create_c4 = CHAR_zero;
 STR ret_val15;
 STR r4;
 STR plus_self5;
 CHAR plus_arg5 = CHAR_zero;
 STR ret_val16;
 STR s5;
 CHAR str_self5 = CHAR_zero;
 STR ret_val17;
 STR create_self5;
 CHAR create_c5 = CHAR_zero;
 STR ret_val18;
 STR r5;
 STR plus_self6;
 CHAR plus_arg6 = CHAR_zero;
 STR ret_val19;
 STR s6;
 CHAR str_self6 = CHAR_zero;
 STR ret_val20;
 STR create_self6;
 CHAR create_c6 = CHAR_zero;
 STR ret_val21;
 STR r6;
 STR plus_self7;
 CHAR plus_arg7 = CHAR_zero;
 STR ret_val22;
 STR s7;
 CHAR str_self7 = CHAR_zero;
 STR ret_val23;
 STR create_self7;
 CHAR create_c7 = CHAR_zero;
 STR ret_val24;
 STR r7;
 STR plus_self8;
 CHAR plus_arg8 = CHAR_zero;
 STR ret_val25;
 STR s8;
 CHAR str_self8 = CHAR_zero;
 STR ret_val26;
 STR create_self8;
 CHAR create_c8 = CHAR_zero;
 STR ret_val27;
 STR r8;
 STR plus_self9;
 CHAR plus_arg9 = CHAR_zero;
 STR ret_val28;
 STR s9;
 CHAR str_self9 = CHAR_zero;
 STR ret_val29;
 STR create_self9;
 CHAR create_c9 = CHAR_zero;
 STR ret_val30;
 STR r9;
 STR plus_self10;
 CHAR plus_arg10 = CHAR_zero;
 STR ret_val31;
 STR s10;
 CHAR str_self10 = CHAR_zero;
 STR ret_val32;
 STR create_self10;
 CHAR create_c10 = CHAR_zero;
 STR ret_val33;
 STR r10;
 STR plus_self11;
 CHAR plus_arg11 = CHAR_zero;
 STR ret_val34;
 STR s11;
 CHAR str_self11 = CHAR_zero;
 STR ret_val35;
 STR create_self11;
 CHAR create_c11 = CHAR_zero;
 STR ret_val36;
 STR r11;
 STR plus_self12;
 CHAR plus_arg12 = CHAR_zero;
 STR ret_val37;
 STR s12;
 CHAR str_self12 = CHAR_zero;
 STR ret_val38;
 STR create_self12;
 CHAR create_c12 = CHAR_zero;
 STR ret_val39;
 STR r12;
 STR plus_self13;
 CHAR plus_arg13 = CHAR_zero;
 STR ret_val40;
 STR s13;
 CHAR str_self13 = CHAR_zero;
 STR ret_val41;
 STR create_self13;
 CHAR create_c13 = CHAR_zero;
 STR ret_val42;
 STR r13;
 STR plus_self14;
 CHAR plus_arg14 = CHAR_zero;
 STR ret_val43;
 STR s14;
 CHAR str_self14 = CHAR_zero;
 STR ret_val44;
 STR create_self14;
 CHAR create_c14 = CHAR_zero;
 STR ret_val45;
 STR r14;
 STR plus_self15;
 CHAR plus_arg15 = CHAR_zero;
 STR ret_val46;
 STR s15;
 CHAR str_self15 = CHAR_zero;
 STR ret_val47;
 STR create_self15;
 CHAR create_c15 = CHAR_zero;
 STR ret_val48;
 STR r15;
 STR plus_self16;
 CHAR plus_arg16 = CHAR_zero;
 STR ret_val49;
 STR s16;
 CHAR str_self16 = CHAR_zero;
 STR ret_val50;
 STR create_self16;
 CHAR create_c16 = CHAR_zero;
 STR ret_val51;
 STR r16;
 STR plus_self17;
 CHAR plus_arg17 = CHAR_zero;
 STR ret_val52;
 STR s17;
 CHAR str_self17 = CHAR_zero;
 STR ret_val53;
 STR create_self17;
 CHAR create_c17 = CHAR_zero;
 STR ret_val54;
 STR r17;
 STR plus_self18;
 CHAR plus_arg18 = CHAR_zero;
 STR ret_val55;
 STR s18;
 CHAR str_self18 = CHAR_zero;
 STR ret_val56;
 STR create_self18;
 CHAR create_c18 = CHAR_zero;
 STR ret_val57;
 STR r18;
 STR plus_self19;
 CHAR plus_arg19 = CHAR_zero;
 STR ret_val58;
 STR s19;
 CHAR str_self19 = CHAR_zero;
 STR ret_val59;
 STR create_self19;
 CHAR create_c19 = CHAR_zero;
 STR ret_val60;
 STR r19;
 STR plus_self20;
 CHAR plus_arg20 = CHAR_zero;
 STR ret_val61;
 STR s20;
 CHAR str_self20 = CHAR_zero;
 STR ret_val62;
 STR create_self20;
 CHAR create_c20 = CHAR_zero;
 STR ret_val63;
 STR r20;
 STR plus_self21;
 CHAR plus_arg21 = CHAR_zero;
 STR ret_val64;
 STR s21;
 CHAR str_self21 = CHAR_zero;
 STR ret_val65;
 STR create_self21;
 CHAR create_c21 = CHAR_zero;
 STR ret_val66;
 STR r21;
 STR plus_self22;
 CHAR plus_arg22 = CHAR_zero;
 STR ret_val67;
 STR s22;
 CHAR str_self22 = CHAR_zero;
 STR ret_val68;
 STR create_self22;
 CHAR create_c22 = CHAR_zero;
 STR ret_val69;
 STR r22;
 STR plus_self23;
 CHAR plus_arg23 = CHAR_zero;
 STR ret_val70;
 STR s23;
 CHAR str_self23 = CHAR_zero;
 STR ret_val71;
 STR create_self23;
 CHAR create_c23 = CHAR_zero;
 STR ret_val72;
 STR r23;
 STR plus_self24;
 CHAR plus_arg24 = CHAR_zero;
 STR ret_val73;
 STR s24;
 CHAR str_self24 = CHAR_zero;
 STR ret_val74;
 STR create_self24;
 CHAR create_c24 = CHAR_zero;
 STR ret_val75;
 STR r24;
 STR plus_self25;
 CHAR plus_arg25 = CHAR_zero;
 STR ret_val76;
 STR s25;
 CHAR str_self25 = CHAR_zero;
 STR ret_val77;
 STR create_self25;
 CHAR create_c25 = CHAR_zero;
 STR ret_val78;
 STR r25;
 STR plus_self26;
 CHAR plus_arg26 = CHAR_zero;
 STR ret_val79;
 STR s26;
 CHAR str_self26 = CHAR_zero;
 STR ret_val80;
 STR create_self26;
 CHAR create_c26 = CHAR_zero;
 STR ret_val81;
 STR r26;
 STR plus_self27;
 CHAR plus_arg27 = CHAR_zero;
 STR ret_val82;
 STR s27;
 CHAR str_self27 = CHAR_zero;
 STR ret_val83;
 STR create_self27;
 CHAR create_c27 = CHAR_zero;
 STR ret_val84;
 STR r27;
 STR plus_self28;
 CHAR plus_arg28 = CHAR_zero;
 STR ret_val85;
 STR s28;
 CHAR str_self28 = CHAR_zero;
 STR ret_val86;
 STR create_self28;
 CHAR create_c28 = CHAR_zero;
 STR ret_val87;
 STR r28;
 STR plus_self29;
 CHAR plus_arg29 = CHAR_zero;
 STR ret_val88;
 STR s29;
 CHAR str_self29 = CHAR_zero;
 STR ret_val89;
 STR create_self29;
 CHAR create_c29 = CHAR_zero;
 STR ret_val90;
 STR r29;
 STR plus_self30;
 CHAR plus_arg30 = CHAR_zero;
 STR ret_val91;
 STR s30;
 CHAR str_self30 = CHAR_zero;
 STR ret_val92;
 STR create_self30;
 CHAR create_c30 = CHAR_zero;
 STR ret_val93;
 STR r30;
 STR plus_self31;
 CHAR plus_arg31 = CHAR_zero;
 STR ret_val94;
 STR s31;
 CHAR str_self31 = CHAR_zero;
 STR ret_val95;
 STR create_self31;
 CHAR create_c31 = CHAR_zero;
 STR ret_val96;
 STR r31;
 STR plus_self32;
 CHAR plus_arg32 = CHAR_zero;
 STR ret_val97;
 STR s32;
 CHAR str_self32 = CHAR_zero;
 STR ret_val98;
 STR create_self32;
 CHAR create_c32 = CHAR_zero;
 STR ret_val99;
 STR r32;
 STR plus_self33;
 CHAR plus_arg33 = CHAR_zero;
 STR ret_val100;
 STR s33;
 CHAR str_self33 = CHAR_zero;
 STR ret_val101;
 STR create_self33;
 CHAR create_c33 = CHAR_zero;
 STR ret_val102;
 STR r33;
 STR plus_self34;
 CHAR plus_arg34 = CHAR_zero;
 STR ret_val103;
 STR s34;
 CHAR str_self34 = CHAR_zero;
 STR ret_val104;
 STR create_self34;
 CHAR create_c34 = CHAR_zero;
 STR ret_val105;
 STR r34;
 STR plus_self35;
 CHAR plus_arg35 = CHAR_zero;
 STR ret_val106;
 STR s35;
 CHAR str_self35 = CHAR_zero;
 STR ret_val107;
 STR create_self35;
 CHAR create_c35 = CHAR_zero;
 STR ret_val108;
 STR r35;
 STR plus_self36;
 CHAR plus_arg36 = CHAR_zero;
 STR ret_val109;
 STR s36;
 CHAR str_self36 = CHAR_zero;
 STR ret_val110;
 STR create_self36;
 CHAR create_c36 = CHAR_zero;
 STR ret_val1111;
 STR r36;
 STR plus_self37;
 CHAR plus_arg37 = CHAR_zero;
 STR ret_val112;
 STR s37;
 CHAR str_self37 = CHAR_zero;
 STR ret_val113;
 STR create_self37;
 CHAR create_c37 = CHAR_zero;
 STR ret_val114;
 STR r37;
 STR plus_self38;
 CHAR plus_arg38 = CHAR_zero;
 STR ret_val115;
 STR s38;
 CHAR str_self38 = CHAR_zero;
 STR ret_val116;
 STR create_self38;
 CHAR create_c38 = CHAR_zero;
 STR ret_val117;
 STR r38;
 STR plus_self39;
 CHAR plus_arg39 = CHAR_zero;
 STR ret_val118;
 STR s39;
 CHAR str_self39 = CHAR_zero;
 STR ret_val119;
 STR create_self39;
 CHAR create_c39 = CHAR_zero;
 STR ret_val120;
 STR r39;
 STR plus_self40;
 CHAR plus_arg40 = CHAR_zero;
 STR ret_val121;
 STR s40;
 CHAR str_self40 = CHAR_zero;
 STR ret_val122;
 STR create_self40;
 CHAR create_c40 = CHAR_zero;
 STR ret_val123;
 STR r40;
 STR plus_self41;
 CHAR plus_arg41 = CHAR_zero;
 STR ret_val124;
 STR s41;
 CHAR str_self41 = CHAR_zero;
 STR ret_val125;
 STR create_self41;
 CHAR create_c41 = CHAR_zero;
 STR ret_val126;
 STR r41;
 STR plus_self42;
 CHAR plus_arg42 = CHAR_zero;
 STR ret_val127;
 STR s42;
 CHAR str_self42 = CHAR_zero;
 STR ret_val128;
 STR create_self42;
 CHAR create_c42 = CHAR_zero;
 STR ret_val129;
 STR r42;
 STR plus_self43;
 CHAR plus_arg43 = CHAR_zero;
 STR ret_val130;
 STR s43;
 CHAR str_self43 = CHAR_zero;
 STR ret_val131;
 STR create_self43;
 CHAR create_c43 = CHAR_zero;
 STR ret_val132;
 STR r43;
 STR plus_self44;
 CHAR plus_arg44 = CHAR_zero;
 STR ret_val133;
 STR s44;
 CHAR str_self44 = CHAR_zero;
 STR ret_val134;
 STR create_self44;
 CHAR create_c44 = CHAR_zero;
 STR ret_val135;
 STR r44;
 STR plus_self45;
 CHAR plus_arg45 = CHAR_zero;
 STR ret_val136;
 STR s45;
 CHAR str_self45 = CHAR_zero;
 STR ret_val137;
 STR create_self45;
 CHAR create_c45 = CHAR_zero;
 STR ret_val138;
 STR r45;
 STR plus_self46;
 CHAR plus_arg46 = CHAR_zero;
 STR ret_val139;
 STR s46;
 CHAR str_self46 = CHAR_zero;
 STR ret_val140;
 STR create_self46;
 CHAR create_c46 = CHAR_zero;
 STR ret_val141;
 STR r46;
 STR plus_self47;
 CHAR plus_arg47 = CHAR_zero;
 STR ret_val142;
 STR s47;
 CHAR str_self47 = CHAR_zero;
 STR ret_val143;
 STR create_self47;
 CHAR create_c47 = CHAR_zero;
 STR ret_val144;
 STR r47;
 STR plus_self48;
 CHAR plus_arg48 = CHAR_zero;
 STR ret_val145;
 STR s48;
 CHAR str_self48 = CHAR_zero;
 STR ret_val146;
 STR create_self48;
 CHAR create_c48 = CHAR_zero;
 STR ret_val147;
 STR r48;
 STR plus_self49;
 CHAR plus_arg49 = CHAR_zero;
 STR ret_val148;
 STR s49;
 CHAR str_self49 = CHAR_zero;
 STR ret_val149;
 STR create_self49;
 CHAR create_c49 = CHAR_zero;
 STR ret_val150;
 STR r49;
 STR plus_self50;
 CHAR plus_arg50 = CHAR_zero;
 STR ret_val151;
 STR s50;
 CHAR str_self50 = CHAR_zero;
 STR ret_val152;
 STR create_self50;
 CHAR create_c50 = CHAR_zero;
 STR ret_val153;
 STR r50;
 STR plus_self51;
 CHAR plus_arg51 = CHAR_zero;
 STR ret_val154;
 STR s51;
 CHAR str_self51 = CHAR_zero;
 STR ret_val155;
 STR create_self51;
 CHAR create_c51 = CHAR_zero;
 STR ret_val156;
 STR r51;
 STR plus_self52;
 CHAR plus_arg52 = CHAR_zero;
 STR ret_val157;
 STR s52;
 CHAR str_self52 = CHAR_zero;
 STR ret_val158;
 STR create_self52;
 CHAR create_c52 = CHAR_zero;
 STR ret_val159;
 STR r52;
 STR plus_self53;
 CHAR plus_arg53 = CHAR_zero;
 STR ret_val160;
 STR s53;
 CHAR str_self53 = CHAR_zero;
 STR ret_val161;
 STR create_self53;
 CHAR create_c53 = CHAR_zero;
 STR ret_val162;
 STR r53;
 STR plus_self54;
 CHAR plus_arg54 = CHAR_zero;
 STR ret_val163;
 STR s54;
 CHAR str_self54 = CHAR_zero;
 STR ret_val164;
 STR create_self54;
 CHAR create_c54 = CHAR_zero;
 STR ret_val165;
 STR r54;
 STR plus_self55;
 CHAR plus_arg55 = CHAR_zero;
 STR ret_val166;
 STR s55;
 CHAR str_self55 = CHAR_zero;
 STR ret_val167;
 STR create_self55;
 CHAR create_c55 = CHAR_zero;
 STR ret_val168;
 STR r55;
 STR plus_self56;
 CHAR plus_arg56 = CHAR_zero;
 STR ret_val169;
 STR s56;
 CHAR str_self56 = CHAR_zero;
 STR ret_val170;
 STR create_self56;
 CHAR create_c56 = CHAR_zero;
 STR ret_val171;
 STR r56;
 STR plus_self57;
 CHAR plus_arg57 = CHAR_zero;
 STR ret_val172;
 STR s57;
 CHAR str_self57 = CHAR_zero;
 STR ret_val173;
 STR create_self57;
 CHAR create_c57 = CHAR_zero;
 STR ret_val174;
 STR r57;
 STR plus_self58;
 CHAR plus_arg58 = CHAR_zero;
 STR ret_val175;
 STR s58;
 CHAR str_self58 = CHAR_zero;
 STR ret_val176;
 STR create_self58;
 CHAR create_c58 = CHAR_zero;
 STR ret_val177;
 STR r58;
 STR plus_self59;
 CHAR plus_arg59 = CHAR_zero;
 STR ret_val178;
 STR s59;
 CHAR str_self59 = CHAR_zero;
 STR ret_val179;
 STR create_self59;
 CHAR create_c59 = CHAR_zero;
 STR ret_val180;
 STR r59;
 STR plus_self60;
 CHAR plus_arg60 = CHAR_zero;
 STR ret_val181;
 STR s60;
 CHAR str_self60 = CHAR_zero;
 STR ret_val182;
 STR create_self60;
 CHAR create_c60 = CHAR_zero;
 STR ret_val183;
 STR r60;
 STR plus_self61;
 CHAR plus_arg61 = CHAR_zero;
 STR ret_val184;
 STR s61;
 CHAR str_self61 = CHAR_zero;
 STR ret_val185;
 STR create_self61;
 CHAR create_c61 = CHAR_zero;
 STR ret_val186;
 STR r61;
 STR plus_self62;
 CHAR plus_arg62 = CHAR_zero;
 STR ret_val187;
 STR s62;
 CHAR str_self62 = CHAR_zero;
 STR ret_val188;
 STR create_self62;
 CHAR create_c62 = CHAR_zero;
 STR ret_val189;
 STR r62;
 STR plus_self63;
 CHAR plus_arg63 = CHAR_zero;
 STR ret_val190;
 STR s63;
 CHAR str_self63 = CHAR_zero;
 STR ret_val191;
 STR create_self63;
 CHAR create_c63 = CHAR_zero;
 STR ret_val192;
 STR r63;
 STR plus_self64;
 CHAR plus_arg64 = CHAR_zero;
 STR ret_val193;
 STR s64;
 CHAR str_self64 = CHAR_zero;
 STR ret_val194;
 STR create_self64;
 CHAR create_c64 = CHAR_zero;
 STR ret_val195;
 STR r64;
 STR plus_self65;
 CHAR plus_arg65 = CHAR_zero;
 STR ret_val196;
 STR s65;
 CHAR str_self65 = CHAR_zero;
 STR ret_val197;
 STR create_self65;
 CHAR create_c65 = CHAR_zero;
 STR ret_val198;
 STR r65;
 STR plus_self66;
 CHAR plus_arg66 = CHAR_zero;
 STR ret_val199;
 STR s66;
 CHAR str_self66 = CHAR_zero;
 STR ret_val200;
 STR create_self66;
 CHAR create_c66 = CHAR_zero;
 STR ret_val201;
 STR r66;
 STR plus_self67;
 CHAR plus_arg67 = CHAR_zero;
 STR ret_val202;
 STR s67;
 CHAR str_self67 = CHAR_zero;
 STR ret_val203;
 STR create_self67;
 CHAR create_c67 = CHAR_zero;
 STR ret_val204;
 STR r67;
 STR plus_self68;
 CHAR plus_arg68 = CHAR_zero;
 STR ret_val205;
 STR s68;
 CHAR str_self68 = CHAR_zero;
 STR ret_val206;
 STR create_self68;
 CHAR create_c68 = CHAR_zero;
 STR ret_val207;
 STR r68;
 STR plus_self69;
 CHAR plus_arg69 = CHAR_zero;
 STR ret_val208;
 STR s69;
 CHAR str_self69 = CHAR_zero;
 STR ret_val209;
 STR create_self69;
 CHAR create_c69 = CHAR_zero;
 STR ret_val210;
 STR r69;
 STR plus_self70;
 CHAR plus_arg70 = CHAR_zero;
 STR ret_val211;
 STR s70;
 CHAR str_self70 = CHAR_zero;
 STR ret_val212;
 STR create_self70;
 CHAR create_c70 = CHAR_zero;
 STR ret_val213;
 STR r70;
 STR plus_self71;
 CHAR plus_arg71 = CHAR_zero;
 STR ret_val214;
 STR s71;
 CHAR str_self71 = CHAR_zero;
 STR ret_val215;
 STR create_self71;
 CHAR create_c71 = CHAR_zero;
 STR ret_val216;
 STR r71;
 STR plus_self72;
 CHAR plus_arg72 = CHAR_zero;
 STR ret_val217;
 STR s72;
 CHAR str_self72 = CHAR_zero;
 STR ret_val218;
 STR create_self72;
 CHAR create_c72 = CHAR_zero;
 STR ret_val219;
 STR r72;
 STR plus_self73;
 CHAR plus_arg73 = CHAR_zero;
 STR ret_val220;
 STR s73;
 CHAR str_self73 = CHAR_zero;
 STR ret_val221;
 STR create_self73;
 CHAR create_c73 = CHAR_zero;
 STR ret_val222;
 STR r73;
 STR plus_self74;
 CHAR plus_arg74 = CHAR_zero;
 STR ret_val223;
 STR s74;
 CHAR str_self74 = CHAR_zero;
 STR ret_val224;
 STR create_self74;
 CHAR create_c74 = CHAR_zero;
 STR ret_val225;
 STR r74;
 STR plus_self75;
 CHAR plus_arg75 = CHAR_zero;
 STR ret_val226;
 STR s75;
 CHAR str_self75 = CHAR_zero;
 STR ret_val227;
 STR create_self75;
 CHAR create_c75 = CHAR_zero;
 STR ret_val228;
 STR r75;
 STR plus_self76;
 CHAR plus_arg76 = CHAR_zero;
 STR ret_val229;
 STR s76;
 CHAR str_self76 = CHAR_zero;
 STR ret_val230;
 STR create_self76;
 CHAR create_c76 = CHAR_zero;
 STR ret_val231;
 STR r76;
 STR plus_self77;
 CHAR plus_arg77 = CHAR_zero;
 STR ret_val232;
 STR s77;
 CHAR str_self77 = CHAR_zero;
 STR ret_val233;
 STR create_self77;
 CHAR create_c77 = CHAR_zero;
 STR ret_val234;
 STR r77;
 STR plus_self78;
 CHAR plus_arg78 = CHAR_zero;
 STR ret_val235;
 STR s78;
 CHAR str_self78 = CHAR_zero;
 STR ret_val236;
 STR create_self78;
 CHAR create_c78 = CHAR_zero;
 STR ret_val237;
 STR r78;
 STR plus_self79;
 CHAR plus_arg79 = CHAR_zero;
 STR ret_val238;
 STR s79;
 CHAR str_self79 = CHAR_zero;
 STR ret_val239;
 STR create_self79;
 CHAR create_c79 = CHAR_zero;
 STR ret_val240;
 STR r79;
 STR plus_self80;
 CHAR plus_arg80 = CHAR_zero;
 STR ret_val241;
 STR s80;
 CHAR str_self80 = CHAR_zero;
 STR ret_val242;
 STR create_self80;
 CHAR create_c80 = CHAR_zero;
 STR ret_val243;
 STR r80;
 STR plus_self81;
 CHAR plus_arg81 = CHAR_zero;
 STR ret_val244;
 STR s81;
 CHAR str_self81 = CHAR_zero;
 STR ret_val245;
 STR create_self81;
 CHAR create_c81 = CHAR_zero;
 STR ret_val246;
 STR r81;
 STR plus_self82;
 CHAR plus_arg82 = CHAR_zero;
 STR ret_val247;
 STR s82;
 CHAR str_self82 = CHAR_zero;
 STR ret_val248;
 STR create_self82;
 CHAR create_c82 = CHAR_zero;
 STR ret_val249;
 STR r82;
 STR plus_self83;
 CHAR plus_arg83 = CHAR_zero;
 STR ret_val250;
 STR s83;
 CHAR str_self83 = CHAR_zero;
 STR ret_val251;
 STR create_self83;
 CHAR create_c83 = CHAR_zero;
 STR ret_val252;
 STR r83;
 STR plus_self84;
 CHAR plus_arg84 = CHAR_zero;
 STR ret_val253;
 STR s84;
 CHAR str_self84 = CHAR_zero;
 STR ret_val254;
 STR create_self84;
 CHAR create_c84 = CHAR_zero;
 STR ret_val255;
 STR r84;
 STR plus_self85;
 CHAR plus_arg85 = CHAR_zero;
 STR ret_val256;
 STR s85;
 CHAR str_self85 = CHAR_zero;
 STR ret_val257;
 STR create_self85;
 CHAR create_c85 = CHAR_zero;
 STR ret_val258;
 STR r85;
 STR plus_self86;
 CHAR plus_arg86 = CHAR_zero;
 STR ret_val259;
 STR s86;
 CHAR str_self86 = CHAR_zero;
 STR ret_val260;
 STR create_self86;
 CHAR create_c86 = CHAR_zero;
 STR ret_val261;
 STR r86;
 STR plus_self87;
 CHAR plus_arg87 = CHAR_zero;
 STR ret_val262;
 STR s87;
 CHAR str_self87 = CHAR_zero;
 STR ret_val263;
 STR create_self87;
 CHAR create_c87 = CHAR_zero;
 STR ret_val264;
 STR r87;
 STR plus_self88;
 CHAR plus_arg88 = CHAR_zero;
 STR ret_val265;
 STR s88;
 CHAR str_self88 = CHAR_zero;
 STR ret_val266;
 STR create_self88;
 CHAR create_c88 = CHAR_zero;
 STR ret_val267;
 STR r88;
 STR plus_self89;
 CHAR plus_arg89 = CHAR_zero;
 STR ret_val268;
 STR s89;
 CHAR str_self89 = CHAR_zero;
 STR ret_val269;
 STR create_self89;
 CHAR create_c89 = CHAR_zero;
 STR ret_val270;
 STR r89;
 STR plus_self90;
 CHAR plus_arg90 = CHAR_zero;
 STR ret_val271;
 STR s90;
 CHAR str_self90 = CHAR_zero;
 STR ret_val272;
 STR create_self90;
 CHAR create_c90 = CHAR_zero;
 STR ret_val273;
 STR r90;
 STR plus_self91;
 CHAR plus_arg91 = CHAR_zero;
 STR ret_val274;
 STR s91;
 CHAR str_self91 = CHAR_zero;
 STR ret_val275;
 STR create_self91;
 CHAR create_c91 = CHAR_zero;
 STR ret_val276;
 STR r91;
 STR plus_self92;
 STR plus_sarg;
 STR ret_val277;
 STR plus_self93;
 STR plus_sarg1;
 STR ret_val278;
 STR plus_self94;
 STR plus_sarg2;
 STR ret_val279;
 STR plus_self95;
 STR plus_sarg3;
 STR ret_val280;
 STR plus_self96;
 STR plus_sarg4;
 STR ret_val281;
 STR plus_self97;
 STR plus_sarg5;
 STR ret_val282;
 STR plus_self98;
 STR plus_sarg6;
 STR ret_val283;
 STR plus_self99;
 STR plus_sarg7;
 STR ret_val284;
 STR plus_self100;
 STR plus_sarg8;
 STR ret_val285;
 STR plus_self101;
 CHAR plus_arg92 = CHAR_zero;
 STR ret_val286;
 STR s92;
 CHAR str_self92 = CHAR_zero;
 STR ret_val287;
 STR create_self92;
 CHAR create_c92 = CHAR_zero;
 STR ret_val288;
 STR r92;
 STR plus_self102;
 STR plus_sarg9;
 STR ret_val289;
 extern STR name1;
 STR L2;
 INT L3;
 OB L4;
 INT L5;
 STR L7;
 INT L8;
 OB L9;
 INT L10;
 STR L13;
 INT L14;
 OB L15;
 INT L16;
 STR L18;
 INT L19;
 OB L20;
 INT L21;
 STR L23;
 INT L24;
 OB L25;
 INT L26;
 STR L28;
 INT L29;
 OB L30;
 INT L31;
 STR L33;
 INT L34;
 OB L35;
 INT L36;
 STR L38;
 INT L39;
 OB L40;
 INT L41;
 STR L43;
 INT L44;
 OB L45;
 INT L46;
 STR L48;
 INT L49;
 OB L50;
 INT L51;
 STR L53;
 INT L54;
 OB L55;
 INT L56;
 STR L58;
 INT L59;
 OB L60;
 INT L62;
 STR L64;
 INT L65;
 OB L66;
 INT L67;
 STR L69;
 INT L70;
 OB L71;
 INT L72;
 STR L74;
 INT L75;
 OB L76;
 INT L77;
 STR L79;
 INT L80;
 OB L81;
 INT L82;
 STR L84;
 INT L85;
 OB L86;
 INT L87;
 STR L89;
 INT L90;
 OB L91;
 INT L92;
 STR L94;
 INT L95;
 OB L96;
 INT L97;
 STR L99;
 INT L100;
 OB L101;
 INT L102;
 STR L104;
 INT L105;
 OB L106;
 INT L107;
 STR L109;
 INT L110;
 OB L111;
 INT L112;
 STR L114;
 INT L115;
 OB L116;
 INT L117;
 STR L119;
 INT L120;
 OB L122;
 INT L123;
 STR L125;
 INT L126;
 OB L127;
 INT L128;
 STR L130;
 INT L131;
 OB L132;
 INT L133;
 STR L135;
 INT L136;
 OB L137;
 INT L138;
 STR L140;
 INT L141;
 OB L142;
 INT L143;
 STR L145;
 INT L146;
 OB L147;
 INT L148;
 STR L150;
 INT L151;
 OB L152;
 INT L153;
 STR L155;
 INT L156;
 OB L157;
 INT L158;
 STR L160;
 INT L161;
 OB L162;
 INT L163;
 STR L165;
 INT L166;
 OB L167;
 INT L168;
 STR L170;
 INT L172;
 OB L173;
 INT L174;
 STR L176;
 INT L177;
 OB L178;
 INT L179;
 STR L181;
 INT L182;
 OB L183;
 INT L184;
 STR L186;
 INT L187;
 OB L188;
 INT L189;
 STR L191;
 INT L192;
 OB L193;
 INT L194;
 STR L196;
 INT L197;
 OB L198;
 INT L199;
 STR L201;
 INT L202;
 OB L203;
 INT L204;
 STR L206;
 INT L207;
 OB L208;
 INT L209;
 STR L211;
 INT L212;
 OB L213;
 INT L214;
 STR L216;
 INT L217;
 OB L218;
 INT L219;
 STR L222;
 INT L223;
 OB L224;
 INT L225;
 STR L227;
 INT L228;
 OB L229;
 INT L230;
 STR L232;
 INT L233;
 OB L234;
 INT L235;
 STR L237;
 INT L238;
 OB L239;
 INT L240;
 STR L242;
 INT L243;
 OB L244;
 INT L245;
 STR L247;
 INT L248;
 OB L249;
 INT L250;
 STR L252;
 INT L253;
 OB L254;
 INT L255;
 STR L257;
 INT L258;
 OB L259;
 INT L260;
 STR L262;
 INT L263;
 OB L264;
 INT L265;
 STR L267;
 INT L268;
 OB L269;
 INT L270;
 STR L273;
 INT L274;
 OB L275;
 INT L276;
 STR L278;
 INT L279;
 OB L280;
 INT L281;
 STR L283;
 INT L284;
 OB L285;
 INT L286;
 STR L288;
 INT L289;
 OB L290;
 INT L291;
 STR L293;
 INT L294;
 OB L295;
 INT L296;
 STR L298;
 INT L299;
 OB L300;
 INT L301;
 STR L303;
 INT L304;
 OB L305;
 INT L306;
 STR L308;
 INT L309;
 OB L310;
 INT L311;
 STR L313;
 INT L314;
 OB L315;
 INT L316;
 STR L318;
 INT L319;
 OB L320;
 INT L322;
 STR L324;
 INT L325;
 OB L326;
 INT L327;
 STR L329;
 INT L330;
 OB L331;
 INT L332;
 STR L334;
 INT L335;
 OB L336;
 INT L337;
 STR L339;
 INT L340;
 OB L341;
 INT L342;
 STR L344;
 INT L345;
 OB L346;
 INT L347;
 STR L349;
 INT L350;
 OB L351;
 INT L352;
 STR L354;
 INT L355;
 OB L356;
 INT L357;
 STR L359;
 INT L360;
 OB L361;
 INT L362;
 STR L364;
 INT L365;
 OB L366;
 INT L367;
 STR L369;
 INT L370;
 OB L372;
 INT L373;
 STR L375;
 INT L376;
 OB L377;
 INT L378;
 STR L380;
 INT L381;
 OB L382;
 INT L383;
 STR L385;
 INT L386;
 OB L387;
 INT L388;
 STR L390;
 INT L391;
 OB L392;
 INT L393;
 STR L395;
 INT L396;
 OB L397;
 INT L398;
 STR L400;
 INT L401;
 OB L402;
 INT L403;
 STR L405;
 INT L406;
 OB L407;
 INT L408;
 STR L410;
 INT L411;
 OB L412;
 INT L413;
 STR L415;
 INT L416;
 OB L417;
 INT L418;
 STR L420;
 INT L422;
 OB L423;
 INT L424;
 STR L426;
 INT L427;
 OB L428;
 INT L429;
 STR L431;
 INT L432;
 OB L433;
 INT L434;
 STR L436;
 INT L437;
 OB L438;
 INT L439;
 STR L441;
 INT L442;
 OB L443;
 INT L444;
 STR L446;
 INT L447;
 OB L448;
 INT L449;
 STR L451;
 INT L452;
 OB L453;
 INT L454;
 STR L456;
 INT L457;
 OB L458;
 INT L459;
 STR L461;
 INT L462;
 OB L463;
 INT L464;
 STR L466;
 INT L467;
 OB L468;
 INT L469;
 extern STR b21;
 extern STR f4;
 extern STR n21;
 extern STR r311;
 extern STR t31;
 extern STR v211;
 extern STR name114;
 extern STR name115;
 extern STR name116;
 INT L4721_;
 INT L473;
 INT L4741_;
 STR L475;
 INT L476;
 OB L477;
 INT L478;
 res = ((STR) &name1);
 L11 = c;
 switch (L11) {
  case 'a': 
   plus_self = res;
   plus_arg = c;
   str_self = plus_arg;
   create_self = ((STR) NULL);
   create_c = str_self;
   L3 = 1;
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
   SARR((STR)r,0,create_c); 
   ;
   ret_val3 = r;
   ret_val2 = ret_val3;
   s = ret_val2;
   ret_val1 = STR_ap1077157958(plus_self, s, TRUE);
   res = ret_val1;
   break;
  case 'b': 
   plus_self1 = res;
   plus_arg1 = c;
   str_self1 = plus_arg1;
   create_self1 = ((STR) NULL);
   create_c1 = str_self1;
   L8 = 1;
   L10=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L8)*sizeof(CHAR);
   L9=ZALLOC_LEAF_BIG(L10);
   if (L9==NULL) FATAL("Unable to allocate more memory");
   memset(L9,0,L10);
   ((OB)L9)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L9)->header.destroyed=0;
#endif

   L7 = ((STR) L9);
   L7->asize = L8;
   r1 = L7;
   SARR((STR)r1,0,create_c1); 
   ;
   ret_val6 = r1;
   ret_val5 = ret_val6;
   s1 = ret_val5;
   ret_val4 = STR_ap1077157958(plus_self1, s1, TRUE);
   res = ret_val4;
   break;
  case 'c': 
   plus_self2 = res;
   plus_arg2 = c;
   str_self2 = plus_arg2;
   create_self2 = ((STR) NULL);
   create_c2 = str_self2;
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
   r2 = L13;
   SARR((STR)r2,0,create_c2); 
   ;
   ret_val9 = r2;
   ret_val8 = ret_val9;
   s2 = ret_val8;
   ret_val7 = STR_ap1077157958(plus_self2, s2, TRUE);
   res = ret_val7;
   break;
  case 'd': 
   plus_self3 = res;
   plus_arg3 = c;
   str_self3 = plus_arg3;
   create_self3 = ((STR) NULL);
   create_c3 = str_self3;
   L19 = 1;
   L21=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L19)*sizeof(CHAR);
   L20=ZALLOC_LEAF_BIG(L21);
   if (L20==NULL) FATAL("Unable to allocate more memory");
   memset(L20,0,L21);
   ((OB)L20)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L20)->header.destroyed=0;
#endif

   L18 = ((STR) L20);
   L18->asize = L19;
   r3 = L18;
   SARR((STR)r3,0,create_c3); 
   ;
   ret_val12 = r3;
   ret_val11 = ret_val12;
   s3 = ret_val11;
   ret_val10 = STR_ap1077157958(plus_self3, s3, TRUE);
   res = ret_val10;
   break;
  case 'e': 
   plus_self4 = res;
   plus_arg4 = c;
   str_self4 = plus_arg4;
   create_self4 = ((STR) NULL);
   create_c4 = str_self4;
   L24 = 1;
   L26=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L24)*sizeof(CHAR);
   L25=ZALLOC_LEAF_BIG(L26);
   if (L25==NULL) FATAL("Unable to allocate more memory");
   memset(L25,0,L26);
   ((OB)L25)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L25)->header.destroyed=0;
#endif

   L23 = ((STR) L25);
   L23->asize = L24;
   r4 = L23;
   SARR((STR)r4,0,create_c4); 
   ;
   ret_val15 = r4;
   ret_val14 = ret_val15;
   s4 = ret_val14;
   ret_val13 = STR_ap1077157958(plus_self4, s4, TRUE);
   res = ret_val13;
   break;
  case 'f': 
   plus_self5 = res;
   plus_arg5 = c;
   str_self5 = plus_arg5;
   create_self5 = ((STR) NULL);
   create_c5 = str_self5;
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
   r5 = L28;
   SARR((STR)r5,0,create_c5); 
   ;
   ret_val18 = r5;
   ret_val17 = ret_val18;
   s5 = ret_val17;
   ret_val16 = STR_ap1077157958(plus_self5, s5, TRUE);
   res = ret_val16;
   break;
  case 'g': 
   plus_self6 = res;
   plus_arg6 = c;
   str_self6 = plus_arg6;
   create_self6 = ((STR) NULL);
   create_c6 = str_self6;
   L34 = 1;
   L36=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L34)*sizeof(CHAR);
   L35=ZALLOC_LEAF_BIG(L36);
   if (L35==NULL) FATAL("Unable to allocate more memory");
   memset(L35,0,L36);
   ((OB)L35)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L35)->header.destroyed=0;
#endif

   L33 = ((STR) L35);
   L33->asize = L34;
   r6 = L33;
   SARR((STR)r6,0,create_c6); 
   ;
   ret_val21 = r6;
   ret_val20 = ret_val21;
   s6 = ret_val20;
   ret_val19 = STR_ap1077157958(plus_self6, s6, TRUE);
   res = ret_val19;
   break;
  case 'h': 
   plus_self7 = res;
   plus_arg7 = c;
   str_self7 = plus_arg7;
   create_self7 = ((STR) NULL);
   create_c7 = str_self7;
   L39 = 1;
   L41=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L39)*sizeof(CHAR);
   L40=ZALLOC_LEAF_BIG(L41);
   if (L40==NULL) FATAL("Unable to allocate more memory");
   memset(L40,0,L41);
   ((OB)L40)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L40)->header.destroyed=0;
#endif

   L38 = ((STR) L40);
   L38->asize = L39;
   r7 = L38;
   SARR((STR)r7,0,create_c7); 
   ;
   ret_val24 = r7;
   ret_val23 = ret_val24;
   s7 = ret_val23;
   ret_val22 = STR_ap1077157958(plus_self7, s7, TRUE);
   res = ret_val22;
   break;
  case 'i': 
   plus_self8 = res;
   plus_arg8 = c;
   str_self8 = plus_arg8;
   create_self8 = ((STR) NULL);
   create_c8 = str_self8;
   L44 = 1;
   L46=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L44)*sizeof(CHAR);
   L45=ZALLOC_LEAF_BIG(L46);
   if (L45==NULL) FATAL("Unable to allocate more memory");
   memset(L45,0,L46);
   ((OB)L45)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L45)->header.destroyed=0;
#endif

   L43 = ((STR) L45);
   L43->asize = L44;
   r8 = L43;
   SARR((STR)r8,0,create_c8); 
   ;
   ret_val27 = r8;
   ret_val26 = ret_val27;
   s8 = ret_val26;
   ret_val25 = STR_ap1077157958(plus_self8, s8, TRUE);
   res = ret_val25;
   break;
  case 'j': 
   plus_self9 = res;
   plus_arg9 = c;
   str_self9 = plus_arg9;
   create_self9 = ((STR) NULL);
   create_c9 = str_self9;
   L49 = 1;
   L51=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L49)*sizeof(CHAR);
   L50=ZALLOC_LEAF_BIG(L51);
   if (L50==NULL) FATAL("Unable to allocate more memory");
   memset(L50,0,L51);
   ((OB)L50)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L50)->header.destroyed=0;
#endif

   L48 = ((STR) L50);
   L48->asize = L49;
   r9 = L48;
   SARR((STR)r9,0,create_c9); 
   ;
   ret_val30 = r9;
   ret_val29 = ret_val30;
   s9 = ret_val29;
   ret_val28 = STR_ap1077157958(plus_self9, s9, TRUE);
   res = ret_val28;
   break;
  case 'k': 
   plus_self10 = res;
   plus_arg10 = c;
   str_self10 = plus_arg10;
   create_self10 = ((STR) NULL);
   create_c10 = str_self10;
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
   r10 = L53;
   SARR((STR)r10,0,create_c10); 
   ;
   ret_val33 = r10;
   ret_val32 = ret_val33;
   s10 = ret_val32;
   ret_val31 = STR_ap1077157958(plus_self10, s10, TRUE);
   res = ret_val31;
   break;
  case 'l': 
   plus_self11 = res;
   plus_arg11 = c;
   str_self11 = plus_arg11;
   create_self11 = ((STR) NULL);
   create_c11 = str_self11;
   L59 = 1;
   L62=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L59)*sizeof(CHAR);
   L60=ZALLOC_LEAF_BIG(L62);
   if (L60==NULL) FATAL("Unable to allocate more memory");
   memset(L60,0,L62);
   ((OB)L60)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L60)->header.destroyed=0;
#endif

   L58 = ((STR) L60);
   L58->asize = L59;
   r11 = L58;
   SARR((STR)r11,0,create_c11); 
   ;
   ret_val36 = r11;
   ret_val35 = ret_val36;
   s11 = ret_val35;
   ret_val34 = STR_ap1077157958(plus_self11, s11, TRUE);
   res = ret_val34;
   break;
  case 'm': 
   plus_self12 = res;
   plus_arg12 = c;
   str_self12 = plus_arg12;
   create_self12 = ((STR) NULL);
   create_c12 = str_self12;
   L65 = 1;
   L67=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L65)*sizeof(CHAR);
   L66=ZALLOC_LEAF_BIG(L67);
   if (L66==NULL) FATAL("Unable to allocate more memory");
   memset(L66,0,L67);
   ((OB)L66)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L66)->header.destroyed=0;
#endif

   L64 = ((STR) L66);
   L64->asize = L65;
   r12 = L64;
   SARR((STR)r12,0,create_c12); 
   ;
   ret_val39 = r12;
   ret_val38 = ret_val39;
   s12 = ret_val38;
   ret_val37 = STR_ap1077157958(plus_self12, s12, TRUE);
   res = ret_val37;
   break;
  case 'n': 
   plus_self13 = res;
   plus_arg13 = c;
   str_self13 = plus_arg13;
   create_self13 = ((STR) NULL);
   create_c13 = str_self13;
   L70 = 1;
   L72=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L70)*sizeof(CHAR);
   L71=ZALLOC_LEAF_BIG(L72);
   if (L71==NULL) FATAL("Unable to allocate more memory");
   memset(L71,0,L72);
   ((OB)L71)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L71)->header.destroyed=0;
#endif

   L69 = ((STR) L71);
   L69->asize = L70;
   r13 = L69;
   SARR((STR)r13,0,create_c13); 
   ;
   ret_val42 = r13;
   ret_val41 = ret_val42;
   s13 = ret_val41;
   ret_val40 = STR_ap1077157958(plus_self13, s13, TRUE);
   res = ret_val40;
   break;
  case 'o': 
   plus_self14 = res;
   plus_arg14 = c;
   str_self14 = plus_arg14;
   create_self14 = ((STR) NULL);
   create_c14 = str_self14;
   L75 = 1;
   L77=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L75)*sizeof(CHAR);
   L76=ZALLOC_LEAF_BIG(L77);
   if (L76==NULL) FATAL("Unable to allocate more memory");
   memset(L76,0,L77);
   ((OB)L76)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L76)->header.destroyed=0;
#endif

   L74 = ((STR) L76);
   L74->asize = L75;
   r14 = L74;
   SARR((STR)r14,0,create_c14); 
   ;
   ret_val45 = r14;
   ret_val44 = ret_val45;
   s14 = ret_val44;
   ret_val43 = STR_ap1077157958(plus_self14, s14, TRUE);
   res = ret_val43;
   break;
  case 'p': 
   plus_self15 = res;
   plus_arg15 = c;
   str_self15 = plus_arg15;
   create_self15 = ((STR) NULL);
   create_c15 = str_self15;
   L80 = 1;
   L82=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L80)*sizeof(CHAR);
   L81=ZALLOC_LEAF_BIG(L82);
   if (L81==NULL) FATAL("Unable to allocate more memory");
   memset(L81,0,L82);
   ((OB)L81)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L81)->header.destroyed=0;
#endif

   L79 = ((STR) L81);
   L79->asize = L80;
   r15 = L79;
   SARR((STR)r15,0,create_c15); 
   ;
   ret_val48 = r15;
   ret_val47 = ret_val48;
   s15 = ret_val47;
   ret_val46 = STR_ap1077157958(plus_self15, s15, TRUE);
   res = ret_val46;
   break;
  case 'q': 
   plus_self16 = res;
   plus_arg16 = c;
   str_self16 = plus_arg16;
   create_self16 = ((STR) NULL);
   create_c16 = str_self16;
   L85 = 1;
   L87=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L85)*sizeof(CHAR);
   L86=ZALLOC_LEAF_BIG(L87);
   if (L86==NULL) FATAL("Unable to allocate more memory");
   memset(L86,0,L87);
   ((OB)L86)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L86)->header.destroyed=0;
#endif

   L84 = ((STR) L86);
   L84->asize = L85;
   r16 = L84;
   SARR((STR)r16,0,create_c16); 
   ;
   ret_val51 = r16;
   ret_val50 = ret_val51;
   s16 = ret_val50;
   ret_val49 = STR_ap1077157958(plus_self16, s16, TRUE);
   res = ret_val49;
   break;
  case 'r': 
   plus_self17 = res;
   plus_arg17 = c;
   str_self17 = plus_arg17;
   create_self17 = ((STR) NULL);
   create_c17 = str_self17;
   L90 = 1;
   L92=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L90)*sizeof(CHAR);
   L91=ZALLOC_LEAF_BIG(L92);
   if (L91==NULL) FATAL("Unable to allocate more memory");
   memset(L91,0,L92);
   ((OB)L91)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L91)->header.destroyed=0;
#endif

   L89 = ((STR) L91);
   L89->asize = L90;
   r17 = L89;
   SARR((STR)r17,0,create_c17); 
   ;
   ret_val54 = r17;
   ret_val53 = ret_val54;
   s17 = ret_val53;
   ret_val52 = STR_ap1077157958(plus_self17, s17, TRUE);
   res = ret_val52;
   break;
  case 's': 
   plus_self18 = res;
   plus_arg18 = c;
   str_self18 = plus_arg18;
   create_self18 = ((STR) NULL);
   create_c18 = str_self18;
   L95 = 1;
   L97=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L95)*sizeof(CHAR);
   L96=ZALLOC_LEAF_BIG(L97);
   if (L96==NULL) FATAL("Unable to allocate more memory");
   memset(L96,0,L97);
   ((OB)L96)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L96)->header.destroyed=0;
#endif

   L94 = ((STR) L96);
   L94->asize = L95;
   r18 = L94;
   SARR((STR)r18,0,create_c18); 
   ;
   ret_val57 = r18;
   ret_val56 = ret_val57;
   s18 = ret_val56;
   ret_val55 = STR_ap1077157958(plus_self18, s18, TRUE);
   res = ret_val55;
   break;
  case 't': 
   plus_self19 = res;
   plus_arg19 = c;
   str_self19 = plus_arg19;
   create_self19 = ((STR) NULL);
   create_c19 = str_self19;
   L100 = 1;
   L102=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L100)*sizeof(CHAR);
   L101=ZALLOC_LEAF_BIG(L102);
   if (L101==NULL) FATAL("Unable to allocate more memory");
   memset(L101,0,L102);
   ((OB)L101)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L101)->header.destroyed=0;
#endif

   L99 = ((STR) L101);
   L99->asize = L100;
   r19 = L99;
   SARR((STR)r19,0,create_c19); 
   ;
   ret_val60 = r19;
   ret_val59 = ret_val60;
   s19 = ret_val59;
   ret_val58 = STR_ap1077157958(plus_self19, s19, TRUE);
   res = ret_val58;
   break;
  case 'u': 
   plus_self20 = res;
   plus_arg20 = c;
   str_self20 = plus_arg20;
   create_self20 = ((STR) NULL);
   create_c20 = str_self20;
   L105 = 1;
   L107=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L105)*sizeof(CHAR);
   L106=ZALLOC_LEAF_BIG(L107);
   if (L106==NULL) FATAL("Unable to allocate more memory");
   memset(L106,0,L107);
   ((OB)L106)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L106)->header.destroyed=0;
#endif

   L104 = ((STR) L106);
   L104->asize = L105;
   r20 = L104;
   SARR((STR)r20,0,create_c20); 
   ;
   ret_val63 = r20;
   ret_val62 = ret_val63;
   s20 = ret_val62;
   ret_val61 = STR_ap1077157958(plus_self20, s20, TRUE);
   res = ret_val61;
   break;
  case 'v': 
   plus_self21 = res;
   plus_arg21 = c;
   str_self21 = plus_arg21;
   create_self21 = ((STR) NULL);
   create_c21 = str_self21;
   L110 = 1;
   L112=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L110)*sizeof(CHAR);
   L111=ZALLOC_LEAF_BIG(L112);
   if (L111==NULL) FATAL("Unable to allocate more memory");
   memset(L111,0,L112);
   ((OB)L111)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L111)->header.destroyed=0;
#endif

   L109 = ((STR) L111);
   L109->asize = L110;
   r21 = L109;
   SARR((STR)r21,0,create_c21); 
   ;
   ret_val66 = r21;
   ret_val65 = ret_val66;
   s21 = ret_val65;
   ret_val64 = STR_ap1077157958(plus_self21, s21, TRUE);
   res = ret_val64;
   break;
  case 'w': 
   plus_self22 = res;
   plus_arg22 = c;
   str_self22 = plus_arg22;
   create_self22 = ((STR) NULL);
   create_c22 = str_self22;
   L115 = 1;
   L117=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L115)*sizeof(CHAR);
   L116=ZALLOC_LEAF_BIG(L117);
   if (L116==NULL) FATAL("Unable to allocate more memory");
   memset(L116,0,L117);
   ((OB)L116)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L116)->header.destroyed=0;
#endif

   L114 = ((STR) L116);
   L114->asize = L115;
   r22 = L114;
   SARR((STR)r22,0,create_c22); 
   ;
   ret_val69 = r22;
   ret_val68 = ret_val69;
   s22 = ret_val68;
   ret_val67 = STR_ap1077157958(plus_self22, s22, TRUE);
   res = ret_val67;
   break;
  case 'x': 
   plus_self23 = res;
   plus_arg23 = c;
   str_self23 = plus_arg23;
   create_self23 = ((STR) NULL);
   create_c23 = str_self23;
   L120 = 1;
   L123=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L120)*sizeof(CHAR);
   L122=ZALLOC_LEAF_BIG(L123);
   if (L122==NULL) FATAL("Unable to allocate more memory");
   memset(L122,0,L123);
   ((OB)L122)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L122)->header.destroyed=0;
#endif

   L119 = ((STR) L122);
   L119->asize = L120;
   r23 = L119;
   SARR((STR)r23,0,create_c23); 
   ;
   ret_val72 = r23;
   ret_val71 = ret_val72;
   s23 = ret_val71;
   ret_val70 = STR_ap1077157958(plus_self23, s23, TRUE);
   res = ret_val70;
   break;
  case 'y': 
   plus_self24 = res;
   plus_arg24 = c;
   str_self24 = plus_arg24;
   create_self24 = ((STR) NULL);
   create_c24 = str_self24;
   L126 = 1;
   L128=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L126)*sizeof(CHAR);
   L127=ZALLOC_LEAF_BIG(L128);
   if (L127==NULL) FATAL("Unable to allocate more memory");
   memset(L127,0,L128);
   ((OB)L127)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L127)->header.destroyed=0;
#endif

   L125 = ((STR) L127);
   L125->asize = L126;
   r24 = L125;
   SARR((STR)r24,0,create_c24); 
   ;
   ret_val75 = r24;
   ret_val74 = ret_val75;
   s24 = ret_val74;
   ret_val73 = STR_ap1077157958(plus_self24, s24, TRUE);
   res = ret_val73;
   break;
  case 'z': 
   plus_self25 = res;
   plus_arg25 = c;
   str_self25 = plus_arg25;
   create_self25 = ((STR) NULL);
   create_c25 = str_self25;
   L131 = 1;
   L133=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L131)*sizeof(CHAR);
   L132=ZALLOC_LEAF_BIG(L133);
   if (L132==NULL) FATAL("Unable to allocate more memory");
   memset(L132,0,L133);
   ((OB)L132)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L132)->header.destroyed=0;
#endif

   L130 = ((STR) L132);
   L130->asize = L131;
   r25 = L130;
   SARR((STR)r25,0,create_c25); 
   ;
   ret_val78 = r25;
   ret_val77 = ret_val78;
   s25 = ret_val77;
   ret_val76 = STR_ap1077157958(plus_self25, s25, TRUE);
   res = ret_val76;
   break;
  case '0': 
   plus_self26 = res;
   plus_arg26 = c;
   str_self26 = plus_arg26;
   create_self26 = ((STR) NULL);
   create_c26 = str_self26;
   L136 = 1;
   L138=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L136)*sizeof(CHAR);
   L137=ZALLOC_LEAF_BIG(L138);
   if (L137==NULL) FATAL("Unable to allocate more memory");
   memset(L137,0,L138);
   ((OB)L137)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L137)->header.destroyed=0;
#endif

   L135 = ((STR) L137);
   L135->asize = L136;
   r26 = L135;
   SARR((STR)r26,0,create_c26); 
   ;
   ret_val81 = r26;
   ret_val80 = ret_val81;
   s26 = ret_val80;
   ret_val79 = STR_ap1077157958(plus_self26, s26, TRUE);
   res = ret_val79;
   break;
  case '1': 
   plus_self27 = res;
   plus_arg27 = c;
   str_self27 = plus_arg27;
   create_self27 = ((STR) NULL);
   create_c27 = str_self27;
   L141 = 1;
   L143=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L141)*sizeof(CHAR);
   L142=ZALLOC_LEAF_BIG(L143);
   if (L142==NULL) FATAL("Unable to allocate more memory");
   memset(L142,0,L143);
   ((OB)L142)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L142)->header.destroyed=0;
#endif

   L140 = ((STR) L142);
   L140->asize = L141;
   r27 = L140;
   SARR((STR)r27,0,create_c27); 
   ;
   ret_val84 = r27;
   ret_val83 = ret_val84;
   s27 = ret_val83;
   ret_val82 = STR_ap1077157958(plus_self27, s27, TRUE);
   res = ret_val82;
   break;
  case '2': 
   plus_self28 = res;
   plus_arg28 = c;
   str_self28 = plus_arg28;
   create_self28 = ((STR) NULL);
   create_c28 = str_self28;
   L146 = 1;
   L148=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L146)*sizeof(CHAR);
   L147=ZALLOC_LEAF_BIG(L148);
   if (L147==NULL) FATAL("Unable to allocate more memory");
   memset(L147,0,L148);
   ((OB)L147)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L147)->header.destroyed=0;
#endif

   L145 = ((STR) L147);
   L145->asize = L146;
   r28 = L145;
   SARR((STR)r28,0,create_c28); 
   ;
   ret_val87 = r28;
   ret_val86 = ret_val87;
   s28 = ret_val86;
   ret_val85 = STR_ap1077157958(plus_self28, s28, TRUE);
   res = ret_val85;
   break;
  case '3': 
   plus_self29 = res;
   plus_arg29 = c;
   str_self29 = plus_arg29;
   create_self29 = ((STR) NULL);
   create_c29 = str_self29;
   L151 = 1;
   L153=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L151)*sizeof(CHAR);
   L152=ZALLOC_LEAF_BIG(L153);
   if (L152==NULL) FATAL("Unable to allocate more memory");
   memset(L152,0,L153);
   ((OB)L152)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L152)->header.destroyed=0;
#endif

   L150 = ((STR) L152);
   L150->asize = L151;
   r29 = L150;
   SARR((STR)r29,0,create_c29); 
   ;
   ret_val90 = r29;
   ret_val89 = ret_val90;
   s29 = ret_val89;
   ret_val88 = STR_ap1077157958(plus_self29, s29, TRUE);
   res = ret_val88;
   break;
  case '4': 
   plus_self30 = res;
   plus_arg30 = c;
   str_self30 = plus_arg30;
   create_self30 = ((STR) NULL);
   create_c30 = str_self30;
   L156 = 1;
   L158=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L156)*sizeof(CHAR);
   L157=ZALLOC_LEAF_BIG(L158);
   if (L157==NULL) FATAL("Unable to allocate more memory");
   memset(L157,0,L158);
   ((OB)L157)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L157)->header.destroyed=0;
#endif

   L155 = ((STR) L157);
   L155->asize = L156;
   r30 = L155;
   SARR((STR)r30,0,create_c30); 
   ;
   ret_val93 = r30;
   ret_val92 = ret_val93;
   s30 = ret_val92;
   ret_val91 = STR_ap1077157958(plus_self30, s30, TRUE);
   res = ret_val91;
   break;
  case '5': 
   plus_self31 = res;
   plus_arg31 = c;
   str_self31 = plus_arg31;
   create_self31 = ((STR) NULL);
   create_c31 = str_self31;
   L161 = 1;
   L163=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L161)*sizeof(CHAR);
   L162=ZALLOC_LEAF_BIG(L163);
   if (L162==NULL) FATAL("Unable to allocate more memory");
   memset(L162,0,L163);
   ((OB)L162)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L162)->header.destroyed=0;
#endif

   L160 = ((STR) L162);
   L160->asize = L161;
   r31 = L160;
   SARR((STR)r31,0,create_c31); 
   ;
   ret_val96 = r31;
   ret_val95 = ret_val96;
   s31 = ret_val95;
   ret_val94 = STR_ap1077157958(plus_self31, s31, TRUE);
   res = ret_val94;
   break;
  case '6': 
   plus_self32 = res;
   plus_arg32 = c;
   str_self32 = plus_arg32;
   create_self32 = ((STR) NULL);
   create_c32 = str_self32;
   L166 = 1;
   L168=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L166)*sizeof(CHAR);
   L167=ZALLOC_LEAF_BIG(L168);
   if (L167==NULL) FATAL("Unable to allocate more memory");
   memset(L167,0,L168);
   ((OB)L167)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L167)->header.destroyed=0;
#endif

   L165 = ((STR) L167);
   L165->asize = L166;
   r32 = L165;
   SARR((STR)r32,0,create_c32); 
   ;
   ret_val99 = r32;
   ret_val98 = ret_val99;
   s32 = ret_val98;
   ret_val97 = STR_ap1077157958(plus_self32, s32, TRUE);
   res = ret_val97;
   break;
  case '7': 
   plus_self33 = res;
   plus_arg33 = c;
   str_self33 = plus_arg33;
   create_self33 = ((STR) NULL);
   create_c33 = str_self33;
   L172 = 1;
   L174=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L172)*sizeof(CHAR);
   L173=ZALLOC_LEAF_BIG(L174);
   if (L173==NULL) FATAL("Unable to allocate more memory");
   memset(L173,0,L174);
   ((OB)L173)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L173)->header.destroyed=0;
#endif

   L170 = ((STR) L173);
   L170->asize = L172;
   r33 = L170;
   SARR((STR)r33,0,create_c33); 
   ;
   ret_val102 = r33;
   ret_val101 = ret_val102;
   s33 = ret_val101;
   ret_val100 = STR_ap1077157958(plus_self33, s33, TRUE);
   res = ret_val100;
   break;
  case '8': 
   plus_self34 = res;
   plus_arg34 = c;
   str_self34 = plus_arg34;
   create_self34 = ((STR) NULL);
   create_c34 = str_self34;
   L177 = 1;
   L179=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L177)*sizeof(CHAR);
   L178=ZALLOC_LEAF_BIG(L179);
   if (L178==NULL) FATAL("Unable to allocate more memory");
   memset(L178,0,L179);
   ((OB)L178)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L178)->header.destroyed=0;
#endif

   L176 = ((STR) L178);
   L176->asize = L177;
   r34 = L176;
   SARR((STR)r34,0,create_c34); 
   ;
   ret_val105 = r34;
   ret_val104 = ret_val105;
   s34 = ret_val104;
   ret_val103 = STR_ap1077157958(plus_self34, s34, TRUE);
   res = ret_val103;
   break;
  case '9': 
   plus_self35 = res;
   plus_arg35 = c;
   str_self35 = plus_arg35;
   create_self35 = ((STR) NULL);
   create_c35 = str_self35;
   L182 = 1;
   L184=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L182)*sizeof(CHAR);
   L183=ZALLOC_LEAF_BIG(L184);
   if (L183==NULL) FATAL("Unable to allocate more memory");
   memset(L183,0,L184);
   ((OB)L183)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L183)->header.destroyed=0;
#endif

   L181 = ((STR) L183);
   L181->asize = L182;
   r35 = L181;
   SARR((STR)r35,0,create_c35); 
   ;
   ret_val108 = r35;
   ret_val107 = ret_val108;
   s35 = ret_val107;
   ret_val106 = STR_ap1077157958(plus_self35, s35, TRUE);
   res = ret_val106;
   break;
  case 'A': 
   plus_self36 = res;
   plus_arg36 = c;
   str_self36 = plus_arg36;
   create_self36 = ((STR) NULL);
   create_c36 = str_self36;
   L187 = 1;
   L189=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L187)*sizeof(CHAR);
   L188=ZALLOC_LEAF_BIG(L189);
   if (L188==NULL) FATAL("Unable to allocate more memory");
   memset(L188,0,L189);
   ((OB)L188)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L188)->header.destroyed=0;
#endif

   L186 = ((STR) L188);
   L186->asize = L187;
   r36 = L186;
   SARR((STR)r36,0,create_c36); 
   ;
   ret_val1111 = r36;
   ret_val110 = ret_val1111;
   s36 = ret_val110;
   ret_val109 = STR_ap1077157958(plus_self36, s36, TRUE);
   res = ret_val109;
   break;
  case 'B': 
   plus_self37 = res;
   plus_arg37 = c;
   str_self37 = plus_arg37;
   create_self37 = ((STR) NULL);
   create_c37 = str_self37;
   L192 = 1;
   L194=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L192)*sizeof(CHAR);
   L193=ZALLOC_LEAF_BIG(L194);
   if (L193==NULL) FATAL("Unable to allocate more memory");
   memset(L193,0,L194);
   ((OB)L193)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L193)->header.destroyed=0;
#endif

   L191 = ((STR) L193);
   L191->asize = L192;
   r37 = L191;
   SARR((STR)r37,0,create_c37); 
   ;
   ret_val114 = r37;
   ret_val113 = ret_val114;
   s37 = ret_val113;
   ret_val112 = STR_ap1077157958(plus_self37, s37, TRUE);
   res = ret_val112;
   break;
  case 'C': 
   plus_self38 = res;
   plus_arg38 = c;
   str_self38 = plus_arg38;
   create_self38 = ((STR) NULL);
   create_c38 = str_self38;
   L197 = 1;
   L199=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L197)*sizeof(CHAR);
   L198=ZALLOC_LEAF_BIG(L199);
   if (L198==NULL) FATAL("Unable to allocate more memory");
   memset(L198,0,L199);
   ((OB)L198)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L198)->header.destroyed=0;
#endif

   L196 = ((STR) L198);
   L196->asize = L197;
   r38 = L196;
   SARR((STR)r38,0,create_c38); 
   ;
   ret_val117 = r38;
   ret_val116 = ret_val117;
   s38 = ret_val116;
   ret_val115 = STR_ap1077157958(plus_self38, s38, TRUE);
   res = ret_val115;
   break;
  case 'D': 
   plus_self39 = res;
   plus_arg39 = c;
   str_self39 = plus_arg39;
   create_self39 = ((STR) NULL);
   create_c39 = str_self39;
   L202 = 1;
   L204=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L202)*sizeof(CHAR);
   L203=ZALLOC_LEAF_BIG(L204);
   if (L203==NULL) FATAL("Unable to allocate more memory");
   memset(L203,0,L204);
   ((OB)L203)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L203)->header.destroyed=0;
#endif

   L201 = ((STR) L203);
   L201->asize = L202;
   r39 = L201;
   SARR((STR)r39,0,create_c39); 
   ;
   ret_val120 = r39;
   ret_val119 = ret_val120;
   s39 = ret_val119;
   ret_val118 = STR_ap1077157958(plus_self39, s39, TRUE);
   res = ret_val118;
   break;
  case 'E': 
   plus_self40 = res;
   plus_arg40 = c;
   str_self40 = plus_arg40;
   create_self40 = ((STR) NULL);
   create_c40 = str_self40;
   L207 = 1;
   L209=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L207)*sizeof(CHAR);
   L208=ZALLOC_LEAF_BIG(L209);
   if (L208==NULL) FATAL("Unable to allocate more memory");
   memset(L208,0,L209);
   ((OB)L208)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L208)->header.destroyed=0;
#endif

   L206 = ((STR) L208);
   L206->asize = L207;
   r40 = L206;
   SARR((STR)r40,0,create_c40); 
   ;
   ret_val123 = r40;
   ret_val122 = ret_val123;
   s40 = ret_val122;
   ret_val121 = STR_ap1077157958(plus_self40, s40, TRUE);
   res = ret_val121;
   break;
  case 'F': 
   plus_self41 = res;
   plus_arg41 = c;
   str_self41 = plus_arg41;
   create_self41 = ((STR) NULL);
   create_c41 = str_self41;
   L212 = 1;
   L214=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L212)*sizeof(CHAR);
   L213=ZALLOC_LEAF_BIG(L214);
   if (L213==NULL) FATAL("Unable to allocate more memory");
   memset(L213,0,L214);
   ((OB)L213)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L213)->header.destroyed=0;
#endif

   L211 = ((STR) L213);
   L211->asize = L212;
   r41 = L211;
   SARR((STR)r41,0,create_c41); 
   ;
   ret_val126 = r41;
   ret_val125 = ret_val126;
   s41 = ret_val125;
   ret_val124 = STR_ap1077157958(plus_self41, s41, TRUE);
   res = ret_val124;
   break;
  case 'G': 
   plus_self42 = res;
   plus_arg42 = c;
   str_self42 = plus_arg42;
   create_self42 = ((STR) NULL);
   create_c42 = str_self42;
   L217 = 1;
   L219=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L217)*sizeof(CHAR);
   L218=ZALLOC_LEAF_BIG(L219);
   if (L218==NULL) FATAL("Unable to allocate more memory");
   memset(L218,0,L219);
   ((OB)L218)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L218)->header.destroyed=0;
#endif

   L216 = ((STR) L218);
   L216->asize = L217;
   r42 = L216;
   SARR((STR)r42,0,create_c42); 
   ;
   ret_val129 = r42;
   ret_val128 = ret_val129;
   s42 = ret_val128;
   ret_val127 = STR_ap1077157958(plus_self42, s42, TRUE);
   res = ret_val127;
   break;
  case 'H': 
   plus_self43 = res;
   plus_arg43 = c;
   str_self43 = plus_arg43;
   create_self43 = ((STR) NULL);
   create_c43 = str_self43;
   L223 = 1;
   L225=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L223)*sizeof(CHAR);
   L224=ZALLOC_LEAF_BIG(L225);
   if (L224==NULL) FATAL("Unable to allocate more memory");
   memset(L224,0,L225);
   ((OB)L224)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L224)->header.destroyed=0;
#endif

   L222 = ((STR) L224);
   L222->asize = L223;
   r43 = L222;
   SARR((STR)r43,0,create_c43); 
   ;
   ret_val132 = r43;
   ret_val131 = ret_val132;
   s43 = ret_val131;
   ret_val130 = STR_ap1077157958(plus_self43, s43, TRUE);
   res = ret_val130;
   break;
  case 'I': 
   plus_self44 = res;
   plus_arg44 = c;
   str_self44 = plus_arg44;
   create_self44 = ((STR) NULL);
   create_c44 = str_self44;
   L228 = 1;
   L230=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L228)*sizeof(CHAR);
   L229=ZALLOC_LEAF_BIG(L230);
   if (L229==NULL) FATAL("Unable to allocate more memory");
   memset(L229,0,L230);
   ((OB)L229)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L229)->header.destroyed=0;
#endif

   L227 = ((STR) L229);
   L227->asize = L228;
   r44 = L227;
   SARR((STR)r44,0,create_c44); 
   ;
   ret_val135 = r44;
   ret_val134 = ret_val135;
   s44 = ret_val134;
   ret_val133 = STR_ap1077157958(plus_self44, s44, TRUE);
   res = ret_val133;
   break;
  case 'J': 
   plus_self45 = res;
   plus_arg45 = c;
   str_self45 = plus_arg45;
   create_self45 = ((STR) NULL);
   create_c45 = str_self45;
   L233 = 1;
   L235=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L233)*sizeof(CHAR);
   L234=ZALLOC_LEAF_BIG(L235);
   if (L234==NULL) FATAL("Unable to allocate more memory");
   memset(L234,0,L235);
   ((OB)L234)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L234)->header.destroyed=0;
#endif

   L232 = ((STR) L234);
   L232->asize = L233;
   r45 = L232;
   SARR((STR)r45,0,create_c45); 
   ;
   ret_val138 = r45;
   ret_val137 = ret_val138;
   s45 = ret_val137;
   ret_val136 = STR_ap1077157958(plus_self45, s45, TRUE);
   res = ret_val136;
   break;
  case 'K': 
   plus_self46 = res;
   plus_arg46 = c;
   str_self46 = plus_arg46;
   create_self46 = ((STR) NULL);
   create_c46 = str_self46;
   L238 = 1;
   L240=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L238)*sizeof(CHAR);
   L239=ZALLOC_LEAF_BIG(L240);
   if (L239==NULL) FATAL("Unable to allocate more memory");
   memset(L239,0,L240);
   ((OB)L239)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L239)->header.destroyed=0;
#endif

   L237 = ((STR) L239);
   L237->asize = L238;
   r46 = L237;
   SARR((STR)r46,0,create_c46); 
   ;
   ret_val141 = r46;
   ret_val140 = ret_val141;
   s46 = ret_val140;
   ret_val139 = STR_ap1077157958(plus_self46, s46, TRUE);
   res = ret_val139;
   break;
  case 'L': 
   plus_self47 = res;
   plus_arg47 = c;
   str_self47 = plus_arg47;
   create_self47 = ((STR) NULL);
   create_c47 = str_self47;
   L243 = 1;
   L245=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L243)*sizeof(CHAR);
   L244=ZALLOC_LEAF_BIG(L245);
   if (L244==NULL) FATAL("Unable to allocate more memory");
   memset(L244,0,L245);
   ((OB)L244)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L244)->header.destroyed=0;
#endif

   L242 = ((STR) L244);
   L242->asize = L243;
   r47 = L242;
   SARR((STR)r47,0,create_c47); 
   ;
   ret_val144 = r47;
   ret_val143 = ret_val144;
   s47 = ret_val143;
   ret_val142 = STR_ap1077157958(plus_self47, s47, TRUE);
   res = ret_val142;
   break;
  case 'M': 
   plus_self48 = res;
   plus_arg48 = c;
   str_self48 = plus_arg48;
   create_self48 = ((STR) NULL);
   create_c48 = str_self48;
   L248 = 1;
   L250=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L248)*sizeof(CHAR);
   L249=ZALLOC_LEAF_BIG(L250);
   if (L249==NULL) FATAL("Unable to allocate more memory");
   memset(L249,0,L250);
   ((OB)L249)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L249)->header.destroyed=0;
#endif

   L247 = ((STR) L249);
   L247->asize = L248;
   r48 = L247;
   SARR((STR)r48,0,create_c48); 
   ;
   ret_val147 = r48;
   ret_val146 = ret_val147;
   s48 = ret_val146;
   ret_val145 = STR_ap1077157958(plus_self48, s48, TRUE);
   res = ret_val145;
   break;
  case 'N': 
   plus_self49 = res;
   plus_arg49 = c;
   str_self49 = plus_arg49;
   create_self49 = ((STR) NULL);
   create_c49 = str_self49;
   L253 = 1;
   L255=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L253)*sizeof(CHAR);
   L254=ZALLOC_LEAF_BIG(L255);
   if (L254==NULL) FATAL("Unable to allocate more memory");
   memset(L254,0,L255);
   ((OB)L254)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L254)->header.destroyed=0;
#endif

   L252 = ((STR) L254);
   L252->asize = L253;
   r49 = L252;
   SARR((STR)r49,0,create_c49); 
   ;
   ret_val150 = r49;
   ret_val149 = ret_val150;
   s49 = ret_val149;
   ret_val148 = STR_ap1077157958(plus_self49, s49, TRUE);
   res = ret_val148;
   break;
  case 'O': 
   plus_self50 = res;
   plus_arg50 = c;
   str_self50 = plus_arg50;
   create_self50 = ((STR) NULL);
   create_c50 = str_self50;
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
   r50 = L257;
   SARR((STR)r50,0,create_c50); 
   ;
   ret_val153 = r50;
   ret_val152 = ret_val153;
   s50 = ret_val152;
   ret_val151 = STR_ap1077157958(plus_self50, s50, TRUE);
   res = ret_val151;
   break;
  case 'P': 
   plus_self51 = res;
   plus_arg51 = c;
   str_self51 = plus_arg51;
   create_self51 = ((STR) NULL);
   create_c51 = str_self51;
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
   r51 = L262;
   SARR((STR)r51,0,create_c51); 
   ;
   ret_val156 = r51;
   ret_val155 = ret_val156;
   s51 = ret_val155;
   ret_val154 = STR_ap1077157958(plus_self51, s51, TRUE);
   res = ret_val154;
   break;
  case 'Q': 
   plus_self52 = res;
   plus_arg52 = c;
   str_self52 = plus_arg52;
   create_self52 = ((STR) NULL);
   create_c52 = str_self52;
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
   r52 = L267;
   SARR((STR)r52,0,create_c52); 
   ;
   ret_val159 = r52;
   ret_val158 = ret_val159;
   s52 = ret_val158;
   ret_val157 = STR_ap1077157958(plus_self52, s52, TRUE);
   res = ret_val157;
   break;
  case 'R': 
   plus_self53 = res;
   plus_arg53 = c;
   str_self53 = plus_arg53;
   create_self53 = ((STR) NULL);
   create_c53 = str_self53;
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
   r53 = L273;
   SARR((STR)r53,0,create_c53); 
   ;
   ret_val162 = r53;
   ret_val161 = ret_val162;
   s53 = ret_val161;
   ret_val160 = STR_ap1077157958(plus_self53, s53, TRUE);
   res = ret_val160;
   break;
  case 'S': 
   plus_self54 = res;
   plus_arg54 = c;
   str_self54 = plus_arg54;
   create_self54 = ((STR) NULL);
   create_c54 = str_self54;
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
   r54 = L278;
   SARR((STR)r54,0,create_c54); 
   ;
   ret_val165 = r54;
   ret_val164 = ret_val165;
   s54 = ret_val164;
   ret_val163 = STR_ap1077157958(plus_self54, s54, TRUE);
   res = ret_val163;
   break;
  case 'T': 
   plus_self55 = res;
   plus_arg55 = c;
   str_self55 = plus_arg55;
   create_self55 = ((STR) NULL);
   create_c55 = str_self55;
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
   r55 = L283;
   SARR((STR)r55,0,create_c55); 
   ;
   ret_val168 = r55;
   ret_val167 = ret_val168;
   s55 = ret_val167;
   ret_val166 = STR_ap1077157958(plus_self55, s55, TRUE);
   res = ret_val166;
   break;
  case 'U': 
   plus_self56 = res;
   plus_arg56 = c;
   str_self56 = plus_arg56;
   create_self56 = ((STR) NULL);
   create_c56 = str_self56;
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
   r56 = L288;
   SARR((STR)r56,0,create_c56); 
   ;
   ret_val171 = r56;
   ret_val170 = ret_val171;
   s56 = ret_val170;
   ret_val169 = STR_ap1077157958(plus_self56, s56, TRUE);
   res = ret_val169;
   break;
  case 'V': 
   plus_self57 = res;
   plus_arg57 = c;
   str_self57 = plus_arg57;
   create_self57 = ((STR) NULL);
   create_c57 = str_self57;
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
   r57 = L293;
   SARR((STR)r57,0,create_c57); 
   ;
   ret_val174 = r57;
   ret_val173 = ret_val174;
   s57 = ret_val173;
   ret_val172 = STR_ap1077157958(plus_self57, s57, TRUE);
   res = ret_val172;
   break;
  case 'W': 
   plus_self58 = res;
   plus_arg58 = c;
   str_self58 = plus_arg58;
   create_self58 = ((STR) NULL);
   create_c58 = str_self58;
   L299 = 1;
   L301=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L299)*sizeof(CHAR);
   L300=ZALLOC_LEAF_BIG(L301);
   if (L300==NULL) FATAL("Unable to allocate more memory");
   memset(L300,0,L301);
   ((OB)L300)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L300)->header.destroyed=0;
#endif

   L298 = ((STR) L300);
   L298->asize = L299;
   r58 = L298;
   SARR((STR)r58,0,create_c58); 
   ;
   ret_val177 = r58;
   ret_val176 = ret_val177;
   s58 = ret_val176;
   ret_val175 = STR_ap1077157958(plus_self58, s58, TRUE);
   res = ret_val175;
   break;
  case 'X': 
   plus_self59 = res;
   plus_arg59 = c;
   str_self59 = plus_arg59;
   create_self59 = ((STR) NULL);
   create_c59 = str_self59;
   L304 = 1;
   L306=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L304)*sizeof(CHAR);
   L305=ZALLOC_LEAF_BIG(L306);
   if (L305==NULL) FATAL("Unable to allocate more memory");
   memset(L305,0,L306);
   ((OB)L305)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L305)->header.destroyed=0;
#endif

   L303 = ((STR) L305);
   L303->asize = L304;
   r59 = L303;
   SARR((STR)r59,0,create_c59); 
   ;
   ret_val180 = r59;
   ret_val179 = ret_val180;
   s59 = ret_val179;
   ret_val178 = STR_ap1077157958(plus_self59, s59, TRUE);
   res = ret_val178;
   break;
  case 'Y': 
   plus_self60 = res;
   plus_arg60 = c;
   str_self60 = plus_arg60;
   create_self60 = ((STR) NULL);
   create_c60 = str_self60;
   L309 = 1;
   L311=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L309)*sizeof(CHAR);
   L310=ZALLOC_LEAF_BIG(L311);
   if (L310==NULL) FATAL("Unable to allocate more memory");
   memset(L310,0,L311);
   ((OB)L310)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L310)->header.destroyed=0;
#endif

   L308 = ((STR) L310);
   L308->asize = L309;
   r60 = L308;
   SARR((STR)r60,0,create_c60); 
   ;
   ret_val183 = r60;
   ret_val182 = ret_val183;
   s60 = ret_val182;
   ret_val181 = STR_ap1077157958(plus_self60, s60, TRUE);
   res = ret_val181;
   break;
  case 'Z': 
   plus_self61 = res;
   plus_arg61 = c;
   str_self61 = plus_arg61;
   create_self61 = ((STR) NULL);
   create_c61 = str_self61;
   L314 = 1;
   L316=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L314)*sizeof(CHAR);
   L315=ZALLOC_LEAF_BIG(L316);
   if (L315==NULL) FATAL("Unable to allocate more memory");
   memset(L315,0,L316);
   ((OB)L315)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L315)->header.destroyed=0;
#endif

   L313 = ((STR) L315);
   L313->asize = L314;
   r61 = L313;
   SARR((STR)r61,0,create_c61); 
   ;
   ret_val186 = r61;
   ret_val185 = ret_val186;
   s61 = ret_val185;
   ret_val184 = STR_ap1077157958(plus_self61, s61, TRUE);
   res = ret_val184;
   break;
  case '!': 
   plus_self62 = res;
   plus_arg62 = c;
   str_self62 = plus_arg62;
   create_self62 = ((STR) NULL);
   create_c62 = str_self62;
   L319 = 1;
   L322=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L319)*sizeof(CHAR);
   L320=ZALLOC_LEAF_BIG(L322);
   if (L320==NULL) FATAL("Unable to allocate more memory");
   memset(L320,0,L322);
   ((OB)L320)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L320)->header.destroyed=0;
#endif

   L318 = ((STR) L320);
   L318->asize = L319;
   r62 = L318;
   SARR((STR)r62,0,create_c62); 
   ;
   ret_val189 = r62;
   ret_val188 = ret_val189;
   s62 = ret_val188;
   ret_val187 = STR_ap1077157958(plus_self62, s62, TRUE);
   res = ret_val187;
   break;
  case '@': 
   plus_self63 = res;
   plus_arg63 = c;
   str_self63 = plus_arg63;
   create_self63 = ((STR) NULL);
   create_c63 = str_self63;
   L325 = 1;
   L327=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L325)*sizeof(CHAR);
   L326=ZALLOC_LEAF_BIG(L327);
   if (L326==NULL) FATAL("Unable to allocate more memory");
   memset(L326,0,L327);
   ((OB)L326)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L326)->header.destroyed=0;
#endif

   L324 = ((STR) L326);
   L324->asize = L325;
   r63 = L324;
   SARR((STR)r63,0,create_c63); 
   ;
   ret_val192 = r63;
   ret_val191 = ret_val192;
   s63 = ret_val191;
   ret_val190 = STR_ap1077157958(plus_self63, s63, TRUE);
   res = ret_val190;
   break;
  case '#': 
   plus_self64 = res;
   plus_arg64 = c;
   str_self64 = plus_arg64;
   create_self64 = ((STR) NULL);
   create_c64 = str_self64;
   L330 = 1;
   L332=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L330)*sizeof(CHAR);
   L331=ZALLOC_LEAF_BIG(L332);
   if (L331==NULL) FATAL("Unable to allocate more memory");
   memset(L331,0,L332);
   ((OB)L331)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L331)->header.destroyed=0;
#endif

   L329 = ((STR) L331);
   L329->asize = L330;
   r64 = L329;
   SARR((STR)r64,0,create_c64); 
   ;
   ret_val195 = r64;
   ret_val194 = ret_val195;
   s64 = ret_val194;
   ret_val193 = STR_ap1077157958(plus_self64, s64, TRUE);
   res = ret_val193;
   break;
  case '$': 
   plus_self65 = res;
   plus_arg65 = c;
   str_self65 = plus_arg65;
   create_self65 = ((STR) NULL);
   create_c65 = str_self65;
   L335 = 1;
   L337=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L335)*sizeof(CHAR);
   L336=ZALLOC_LEAF_BIG(L337);
   if (L336==NULL) FATAL("Unable to allocate more memory");
   memset(L336,0,L337);
   ((OB)L336)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L336)->header.destroyed=0;
#endif

   L334 = ((STR) L336);
   L334->asize = L335;
   r65 = L334;
   SARR((STR)r65,0,create_c65); 
   ;
   ret_val198 = r65;
   ret_val197 = ret_val198;
   s65 = ret_val197;
   ret_val196 = STR_ap1077157958(plus_self65, s65, TRUE);
   res = ret_val196;
   break;
  case '%': 
   plus_self66 = res;
   plus_arg66 = c;
   str_self66 = plus_arg66;
   create_self66 = ((STR) NULL);
   create_c66 = str_self66;
   L340 = 1;
   L342=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L340)*sizeof(CHAR);
   L341=ZALLOC_LEAF_BIG(L342);
   if (L341==NULL) FATAL("Unable to allocate more memory");
   memset(L341,0,L342);
   ((OB)L341)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L341)->header.destroyed=0;
#endif

   L339 = ((STR) L341);
   L339->asize = L340;
   r66 = L339;
   SARR((STR)r66,0,create_c66); 
   ;
   ret_val201 = r66;
   ret_val200 = ret_val201;
   s66 = ret_val200;
   ret_val199 = STR_ap1077157958(plus_self66, s66, TRUE);
   res = ret_val199;
   break;
  case '^': 
   plus_self67 = res;
   plus_arg67 = c;
   str_self67 = plus_arg67;
   create_self67 = ((STR) NULL);
   create_c67 = str_self67;
   L345 = 1;
   L347=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L345)*sizeof(CHAR);
   L346=ZALLOC_LEAF_BIG(L347);
   if (L346==NULL) FATAL("Unable to allocate more memory");
   memset(L346,0,L347);
   ((OB)L346)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L346)->header.destroyed=0;
#endif

   L344 = ((STR) L346);
   L344->asize = L345;
   r67 = L344;
   SARR((STR)r67,0,create_c67); 
   ;
   ret_val204 = r67;
   ret_val203 = ret_val204;
   s67 = ret_val203;
   ret_val202 = STR_ap1077157958(plus_self67, s67, TRUE);
   res = ret_val202;
   break;
  case '&': 
   plus_self68 = res;
   plus_arg68 = c;
   str_self68 = plus_arg68;
   create_self68 = ((STR) NULL);
   create_c68 = str_self68;
   L350 = 1;
   L352=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L350)*sizeof(CHAR);
   L351=ZALLOC_LEAF_BIG(L352);
   if (L351==NULL) FATAL("Unable to allocate more memory");
   memset(L351,0,L352);
   ((OB)L351)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L351)->header.destroyed=0;
#endif

   L349 = ((STR) L351);
   L349->asize = L350;
   r68 = L349;
   SARR((STR)r68,0,create_c68); 
   ;
   ret_val207 = r68;
   ret_val206 = ret_val207;
   s68 = ret_val206;
   ret_val205 = STR_ap1077157958(plus_self68, s68, TRUE);
   res = ret_val205;
   break;
  case '*': 
   plus_self69 = res;
   plus_arg69 = c;
   str_self69 = plus_arg69;
   create_self69 = ((STR) NULL);
   create_c69 = str_self69;
   L355 = 1;
   L357=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L355)*sizeof(CHAR);
   L356=ZALLOC_LEAF_BIG(L357);
   if (L356==NULL) FATAL("Unable to allocate more memory");
   memset(L356,0,L357);
   ((OB)L356)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L356)->header.destroyed=0;
#endif

   L354 = ((STR) L356);
   L354->asize = L355;
   r69 = L354;
   SARR((STR)r69,0,create_c69); 
   ;
   ret_val210 = r69;
   ret_val209 = ret_val210;
   s69 = ret_val209;
   ret_val208 = STR_ap1077157958(plus_self69, s69, TRUE);
   res = ret_val208;
   break;
  case '(': 
   plus_self70 = res;
   plus_arg70 = c;
   str_self70 = plus_arg70;
   create_self70 = ((STR) NULL);
   create_c70 = str_self70;
   L360 = 1;
   L362=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L360)*sizeof(CHAR);
   L361=ZALLOC_LEAF_BIG(L362);
   if (L361==NULL) FATAL("Unable to allocate more memory");
   memset(L361,0,L362);
   ((OB)L361)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L361)->header.destroyed=0;
#endif

   L359 = ((STR) L361);
   L359->asize = L360;
   r70 = L359;
   SARR((STR)r70,0,create_c70); 
   ;
   ret_val213 = r70;
   ret_val212 = ret_val213;
   s70 = ret_val212;
   ret_val211 = STR_ap1077157958(plus_self70, s70, TRUE);
   res = ret_val211;
   break;
  case ')': 
   plus_self71 = res;
   plus_arg71 = c;
   str_self71 = plus_arg71;
   create_self71 = ((STR) NULL);
   create_c71 = str_self71;
   L365 = 1;
   L367=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L365)*sizeof(CHAR);
   L366=ZALLOC_LEAF_BIG(L367);
   if (L366==NULL) FATAL("Unable to allocate more memory");
   memset(L366,0,L367);
   ((OB)L366)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L366)->header.destroyed=0;
#endif

   L364 = ((STR) L366);
   L364->asize = L365;
   r71 = L364;
   SARR((STR)r71,0,create_c71); 
   ;
   ret_val216 = r71;
   ret_val215 = ret_val216;
   s71 = ret_val215;
   ret_val214 = STR_ap1077157958(plus_self71, s71, TRUE);
   res = ret_val214;
   break;
  case '-': 
   plus_self72 = res;
   plus_arg72 = c;
   str_self72 = plus_arg72;
   create_self72 = ((STR) NULL);
   create_c72 = str_self72;
   L370 = 1;
   L373=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L370)*sizeof(CHAR);
   L372=ZALLOC_LEAF_BIG(L373);
   if (L372==NULL) FATAL("Unable to allocate more memory");
   memset(L372,0,L373);
   ((OB)L372)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L372)->header.destroyed=0;
#endif

   L369 = ((STR) L372);
   L369->asize = L370;
   r72 = L369;
   SARR((STR)r72,0,create_c72); 
   ;
   ret_val219 = r72;
   ret_val218 = ret_val219;
   s72 = ret_val218;
   ret_val217 = STR_ap1077157958(plus_self72, s72, TRUE);
   res = ret_val217;
   break;
  case '=': 
   plus_self73 = res;
   plus_arg73 = c;
   str_self73 = plus_arg73;
   create_self73 = ((STR) NULL);
   create_c73 = str_self73;
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
   r73 = L375;
   SARR((STR)r73,0,create_c73); 
   ;
   ret_val222 = r73;
   ret_val221 = ret_val222;
   s73 = ret_val221;
   ret_val220 = STR_ap1077157958(plus_self73, s73, TRUE);
   res = ret_val220;
   break;
  case '+': 
   plus_self74 = res;
   plus_arg74 = c;
   str_self74 = plus_arg74;
   create_self74 = ((STR) NULL);
   create_c74 = str_self74;
   L381 = 1;
   L383=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L381)*sizeof(CHAR);
   L382=ZALLOC_LEAF_BIG(L383);
   if (L382==NULL) FATAL("Unable to allocate more memory");
   memset(L382,0,L383);
   ((OB)L382)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L382)->header.destroyed=0;
#endif

   L380 = ((STR) L382);
   L380->asize = L381;
   r74 = L380;
   SARR((STR)r74,0,create_c74); 
   ;
   ret_val225 = r74;
   ret_val224 = ret_val225;
   s74 = ret_val224;
   ret_val223 = STR_ap1077157958(plus_self74, s74, TRUE);
   res = ret_val223;
   break;
  case '|': 
   plus_self75 = res;
   plus_arg75 = c;
   str_self75 = plus_arg75;
   create_self75 = ((STR) NULL);
   create_c75 = str_self75;
   L386 = 1;
   L388=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L386)*sizeof(CHAR);
   L387=ZALLOC_LEAF_BIG(L388);
   if (L387==NULL) FATAL("Unable to allocate more memory");
   memset(L387,0,L388);
   ((OB)L387)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L387)->header.destroyed=0;
#endif

   L385 = ((STR) L387);
   L385->asize = L386;
   r75 = L385;
   SARR((STR)r75,0,create_c75); 
   ;
   ret_val228 = r75;
   ret_val227 = ret_val228;
   s75 = ret_val227;
   ret_val226 = STR_ap1077157958(plus_self75, s75, TRUE);
   res = ret_val226;
   break;
  case ':': 
   plus_self76 = res;
   plus_arg76 = c;
   str_self76 = plus_arg76;
   create_self76 = ((STR) NULL);
   create_c76 = str_self76;
   L391 = 1;
   L393=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L391)*sizeof(CHAR);
   L392=ZALLOC_LEAF_BIG(L393);
   if (L392==NULL) FATAL("Unable to allocate more memory");
   memset(L392,0,L393);
   ((OB)L392)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L392)->header.destroyed=0;
#endif

   L390 = ((STR) L392);
   L390->asize = L391;
   r76 = L390;
   SARR((STR)r76,0,create_c76); 
   ;
   ret_val231 = r76;
   ret_val230 = ret_val231;
   s76 = ret_val230;
   ret_val229 = STR_ap1077157958(plus_self76, s76, TRUE);
   res = ret_val229;
   break;
  case ';': 
   plus_self77 = res;
   plus_arg77 = c;
   str_self77 = plus_arg77;
   create_self77 = ((STR) NULL);
   create_c77 = str_self77;
   L396 = 1;
   L398=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L396)*sizeof(CHAR);
   L397=ZALLOC_LEAF_BIG(L398);
   if (L397==NULL) FATAL("Unable to allocate more memory");
   memset(L397,0,L398);
   ((OB)L397)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L397)->header.destroyed=0;
#endif

   L395 = ((STR) L397);
   L395->asize = L396;
   r77 = L395;
   SARR((STR)r77,0,create_c77); 
   ;
   ret_val234 = r77;
   ret_val233 = ret_val234;
   s77 = ret_val233;
   ret_val232 = STR_ap1077157958(plus_self77, s77, TRUE);
   res = ret_val232;
   break;
  case '`': 
   plus_self78 = res;
   plus_arg78 = c;
   str_self78 = plus_arg78;
   create_self78 = ((STR) NULL);
   create_c78 = str_self78;
   L401 = 1;
   L403=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L401)*sizeof(CHAR);
   L402=ZALLOC_LEAF_BIG(L403);
   if (L402==NULL) FATAL("Unable to allocate more memory");
   memset(L402,0,L403);
   ((OB)L402)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L402)->header.destroyed=0;
#endif

   L400 = ((STR) L402);
   L400->asize = L401;
   r78 = L400;
   SARR((STR)r78,0,create_c78); 
   ;
   ret_val237 = r78;
   ret_val236 = ret_val237;
   s78 = ret_val236;
   ret_val235 = STR_ap1077157958(plus_self78, s78, TRUE);
   res = ret_val235;
   break;
  case '~': 
   plus_self79 = res;
   plus_arg79 = c;
   str_self79 = plus_arg79;
   create_self79 = ((STR) NULL);
   create_c79 = str_self79;
   L406 = 1;
   L408=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L406)*sizeof(CHAR);
   L407=ZALLOC_LEAF_BIG(L408);
   if (L407==NULL) FATAL("Unable to allocate more memory");
   memset(L407,0,L408);
   ((OB)L407)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L407)->header.destroyed=0;
#endif

   L405 = ((STR) L407);
   L405->asize = L406;
   r79 = L405;
   SARR((STR)r79,0,create_c79); 
   ;
   ret_val240 = r79;
   ret_val239 = ret_val240;
   s79 = ret_val239;
   ret_val238 = STR_ap1077157958(plus_self79, s79, TRUE);
   res = ret_val238;
   break;
  case '_': 
   plus_self80 = res;
   plus_arg80 = c;
   str_self80 = plus_arg80;
   create_self80 = ((STR) NULL);
   create_c80 = str_self80;
   L411 = 1;
   L413=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L411)*sizeof(CHAR);
   L412=ZALLOC_LEAF_BIG(L413);
   if (L412==NULL) FATAL("Unable to allocate more memory");
   memset(L412,0,L413);
   ((OB)L412)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L412)->header.destroyed=0;
#endif

   L410 = ((STR) L412);
   L410->asize = L411;
   r80 = L410;
   SARR((STR)r80,0,create_c80); 
   ;
   ret_val243 = r80;
   ret_val242 = ret_val243;
   s80 = ret_val242;
   ret_val241 = STR_ap1077157958(plus_self80, s80, TRUE);
   res = ret_val241;
   break;
  case ' ': 
   plus_self81 = res;
   plus_arg81 = c;
   str_self81 = plus_arg81;
   create_self81 = ((STR) NULL);
   create_c81 = str_self81;
   L416 = 1;
   L418=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L416)*sizeof(CHAR);
   L417=ZALLOC_LEAF_BIG(L418);
   if (L417==NULL) FATAL("Unable to allocate more memory");
   memset(L417,0,L418);
   ((OB)L417)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L417)->header.destroyed=0;
#endif

   L415 = ((STR) L417);
   L415->asize = L416;
   r81 = L415;
   SARR((STR)r81,0,create_c81); 
   ;
   ret_val246 = r81;
   ret_val245 = ret_val246;
   s81 = ret_val245;
   ret_val244 = STR_ap1077157958(plus_self81, s81, TRUE);
   res = ret_val244;
   break;
  case ',': 
   plus_self82 = res;
   plus_arg82 = c;
   str_self82 = plus_arg82;
   create_self82 = ((STR) NULL);
   create_c82 = str_self82;
   L422 = 1;
   L424=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L422)*sizeof(CHAR);
   L423=ZALLOC_LEAF_BIG(L424);
   if (L423==NULL) FATAL("Unable to allocate more memory");
   memset(L423,0,L424);
   ((OB)L423)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L423)->header.destroyed=0;
#endif

   L420 = ((STR) L423);
   L420->asize = L422;
   r82 = L420;
   SARR((STR)r82,0,create_c82); 
   ;
   ret_val249 = r82;
   ret_val248 = ret_val249;
   s82 = ret_val248;
   ret_val247 = STR_ap1077157958(plus_self82, s82, TRUE);
   res = ret_val247;
   break;
  case '.': 
   plus_self83 = res;
   plus_arg83 = c;
   str_self83 = plus_arg83;
   create_self83 = ((STR) NULL);
   create_c83 = str_self83;
   L427 = 1;
   L429=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L427)*sizeof(CHAR);
   L428=ZALLOC_LEAF_BIG(L429);
   if (L428==NULL) FATAL("Unable to allocate more memory");
   memset(L428,0,L429);
   ((OB)L428)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L428)->header.destroyed=0;
#endif

   L426 = ((STR) L428);
   L426->asize = L427;
   r83 = L426;
   SARR((STR)r83,0,create_c83); 
   ;
   ret_val252 = r83;
   ret_val251 = ret_val252;
   s83 = ret_val251;
   ret_val250 = STR_ap1077157958(plus_self83, s83, TRUE);
   res = ret_val250;
   break;
  case '<': 
   plus_self84 = res;
   plus_arg84 = c;
   str_self84 = plus_arg84;
   create_self84 = ((STR) NULL);
   create_c84 = str_self84;
   L432 = 1;
   L434=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L432)*sizeof(CHAR);
   L433=ZALLOC_LEAF_BIG(L434);
   if (L433==NULL) FATAL("Unable to allocate more memory");
   memset(L433,0,L434);
   ((OB)L433)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L433)->header.destroyed=0;
#endif

   L431 = ((STR) L433);
   L431->asize = L432;
   r84 = L431;
   SARR((STR)r84,0,create_c84); 
   ;
   ret_val255 = r84;
   ret_val254 = ret_val255;
   s84 = ret_val254;
   ret_val253 = STR_ap1077157958(plus_self84, s84, TRUE);
   res = ret_val253;
   break;
  case '>': 
   plus_self85 = res;
   plus_arg85 = c;
   str_self85 = plus_arg85;
   create_self85 = ((STR) NULL);
   create_c85 = str_self85;
   L437 = 1;
   L439=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L437)*sizeof(CHAR);
   L438=ZALLOC_LEAF_BIG(L439);
   if (L438==NULL) FATAL("Unable to allocate more memory");
   memset(L438,0,L439);
   ((OB)L438)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L438)->header.destroyed=0;
#endif

   L436 = ((STR) L438);
   L436->asize = L437;
   r85 = L436;
   SARR((STR)r85,0,create_c85); 
   ;
   ret_val258 = r85;
   ret_val257 = ret_val258;
   s85 = ret_val257;
   ret_val256 = STR_ap1077157958(plus_self85, s85, TRUE);
   res = ret_val256;
   break;
  case '/': 
   plus_self86 = res;
   plus_arg86 = c;
   str_self86 = plus_arg86;
   create_self86 = ((STR) NULL);
   create_c86 = str_self86;
   L442 = 1;
   L444=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L442)*sizeof(CHAR);
   L443=ZALLOC_LEAF_BIG(L444);
   if (L443==NULL) FATAL("Unable to allocate more memory");
   memset(L443,0,L444);
   ((OB)L443)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L443)->header.destroyed=0;
#endif

   L441 = ((STR) L443);
   L441->asize = L442;
   r86 = L441;
   SARR((STR)r86,0,create_c86); 
   ;
   ret_val261 = r86;
   ret_val260 = ret_val261;
   s86 = ret_val260;
   ret_val259 = STR_ap1077157958(plus_self86, s86, TRUE);
   res = ret_val259;
   break;
  case '?': 
   plus_self87 = res;
   plus_arg87 = c;
   str_self87 = plus_arg87;
   create_self87 = ((STR) NULL);
   create_c87 = str_self87;
   L447 = 1;
   L449=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L447)*sizeof(CHAR);
   L448=ZALLOC_LEAF_BIG(L449);
   if (L448==NULL) FATAL("Unable to allocate more memory");
   memset(L448,0,L449);
   ((OB)L448)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L448)->header.destroyed=0;
#endif

   L446 = ((STR) L448);
   L446->asize = L447;
   r87 = L446;
   SARR((STR)r87,0,create_c87); 
   ;
   ret_val264 = r87;
   ret_val263 = ret_val264;
   s87 = ret_val263;
   ret_val262 = STR_ap1077157958(plus_self87, s87, TRUE);
   res = ret_val262;
   break;
  case '[': 
   plus_self88 = res;
   plus_arg88 = c;
   str_self88 = plus_arg88;
   create_self88 = ((STR) NULL);
   create_c88 = str_self88;
   L452 = 1;
   L454=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L452)*sizeof(CHAR);
   L453=ZALLOC_LEAF_BIG(L454);
   if (L453==NULL) FATAL("Unable to allocate more memory");
   memset(L453,0,L454);
   ((OB)L453)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L453)->header.destroyed=0;
#endif

   L451 = ((STR) L453);
   L451->asize = L452;
   r88 = L451;
   SARR((STR)r88,0,create_c88); 
   ;
   ret_val267 = r88;
   ret_val266 = ret_val267;
   s88 = ret_val266;
   ret_val265 = STR_ap1077157958(plus_self88, s88, TRUE);
   res = ret_val265;
   break;
  case ']': 
   plus_self89 = res;
   plus_arg89 = c;
   str_self89 = plus_arg89;
   create_self89 = ((STR) NULL);
   create_c89 = str_self89;
   L457 = 1;
   L459=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L457)*sizeof(CHAR);
   L458=ZALLOC_LEAF_BIG(L459);
   if (L458==NULL) FATAL("Unable to allocate more memory");
   memset(L458,0,L459);
   ((OB)L458)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L458)->header.destroyed=0;
#endif

   L456 = ((STR) L458);
   L456->asize = L457;
   r89 = L456;
   SARR((STR)r89,0,create_c89); 
   ;
   ret_val270 = r89;
   ret_val269 = ret_val270;
   s89 = ret_val269;
   ret_val268 = STR_ap1077157958(plus_self89, s89, TRUE);
   res = ret_val268;
   break;
  case '{': 
   plus_self90 = res;
   plus_arg90 = c;
   str_self90 = plus_arg90;
   create_self90 = ((STR) NULL);
   create_c90 = str_self90;
   L462 = 1;
   L464=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L462)*sizeof(CHAR);
   L463=ZALLOC_LEAF_BIG(L464);
   if (L463==NULL) FATAL("Unable to allocate more memory");
   memset(L463,0,L464);
   ((OB)L463)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L463)->header.destroyed=0;
#endif

   L461 = ((STR) L463);
   L461->asize = L462;
   r90 = L461;
   SARR((STR)r90,0,create_c90); 
   ;
   ret_val273 = r90;
   ret_val272 = ret_val273;
   s90 = ret_val272;
   ret_val271 = STR_ap1077157958(plus_self90, s90, TRUE);
   res = ret_val271;
   break;
  case '}': 
   plus_self91 = res;
   plus_arg91 = c;
   str_self91 = plus_arg91;
   create_self91 = ((STR) NULL);
   create_c91 = str_self91;
   L467 = 1;
   L469=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L467)*sizeof(CHAR);
   L468=ZALLOC_LEAF_BIG(L469);
   if (L468==NULL) FATAL("Unable to allocate more memory");
   memset(L468,0,L469);
   ((OB)L468)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L468)->header.destroyed=0;
#endif

   L466 = ((STR) L468);
   L466->asize = L467;
   r91 = L466;
   SARR((STR)r91,0,create_c91); 
   ;
   ret_val276 = r91;
   ret_val275 = ret_val276;
   s91 = ret_val275;
   ret_val274 = STR_ap1077157958(plus_self91, s91, TRUE);
   res = ret_val274;
   break;
  case '\b': 
   plus_self92 = res;
   plus_sarg = ((STR) &b21);
   ret_val277 = STR_ap2004550586(plus_self92, plus_sarg);
   res = ret_val277;
   break;
  case '\f': 
   plus_self93 = res;
   plus_sarg1 = ((STR) &f4);
   ret_val278 = STR_ap2004550586(plus_self93, plus_sarg1);
   res = ret_val278;
   break;
  case '\n': 
   plus_self94 = res;
   plus_sarg2 = ((STR) &n21);
   ret_val279 = STR_ap2004550586(plus_self94, plus_sarg2);
   res = ret_val279;
   break;
  case '\r': 
   plus_self95 = res;
   plus_sarg3 = ((STR) &r311);
   ret_val280 = STR_ap2004550586(plus_self95, plus_sarg3);
   res = ret_val280;
   break;
  case '\t': 
   plus_self96 = res;
   plus_sarg4 = ((STR) &t31);
   ret_val281 = STR_ap2004550586(plus_self96, plus_sarg4);
   res = ret_val281;
   break;
  case '\v': 
   plus_self97 = res;
   plus_sarg5 = ((STR) &v211);
   ret_val282 = STR_ap2004550586(plus_self97, plus_sarg5);
   res = ret_val282;
   break;
  case '\\': 
   plus_self98 = res;
   plus_sarg6 = ((STR) &name114);
   ret_val283 = STR_ap2004550586(plus_self98, plus_sarg6);
   res = ret_val283;
   break;
  case '\'': 
   plus_self99 = res;
   plus_sarg7 = ((STR) &name115);
   ret_val284 = STR_ap2004550586(plus_self99, plus_sarg7);
   res = ret_val284;
   break;
  case '\"': 
   plus_self100 = res;
   plus_sarg8 = ((STR) &name116);
   ret_val285 = STR_ap2004550586(plus_self100, plus_sarg8);
   res = ret_val285;
   break;
  default: ;
   L4721_=(INT)((unsigned char)c); 
   oc = INT_oc2110318348(L4721_);
   L473 = STR_lengthrINT(oc);
   L4741_=INTMINUS(L473,2); 
   oc = STR_su203383946(oc, 2, L4741_);
   plus_self101 = res;
   plus_arg92 = '\\';
   str_self92 = plus_arg92;
   create_self92 = ((STR) NULL);
   create_c92 = str_self92;
   L476 = 1;
   L478=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L476)*sizeof(CHAR);
   L477=ZALLOC_LEAF_BIG(L478);
   if (L477==NULL) FATAL("Unable to allocate more memory");
   memset(L477,0,L478);
   ((OB)L477)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L477)->header.destroyed=0;
#endif

   L475 = ((STR) L477);
   L475->asize = L476;
   r92 = L475;
   SARR((STR)r92,0,create_c92); 
   ;
   ret_val288 = r92;
   ret_val287 = ret_val288;
   s92 = ret_val287;
   ret_val286 = STR_ap1077157958(plus_self101, s92, TRUE);
   plus_self102 = ret_val286;
   plus_sarg9 = oc;
   ret_val289 = STR_ap2004550586(plus_self102, plus_sarg9);
   res = ret_val289;
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR NAMEMA1183587160(NAMEMAP self, OB k) {
 STR ret_val;
 INT h = INT_zero;
 OB tk;
 OB tk1;
 NAMEMAP key_hash_self;
 OB key_hash_e;
 INT ret_val1 = INT_zero;
 NAMEMAP is_key_nil_self;
 OB is_key_nil_e;
 BOOL ret_val2 = BOOL_zero;
 NAMEMAP is_key_nil_self1;
 OB is_key_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TUPdOBSTR L71_;
 TUPdOBSTR L8;
 TUPdOBSTR L91_;
 TUPdOBSTR L10;
 INT L111_;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 BOOL L161_;
 TUPdOBSTR L171_;
 TUPdOBSTR L18;
 TUPdOBSTR L191_;
 TUPdOBSTR L20;
 INT L221_;
 if ((self==((NAMEMAP) NULL))) {
  ret_val = ((STR) NULL);
  return ret_val;
 }
 key_hash_self = self;
 key_hash_e = k;
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((NAMEMAP)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=ARR((NAMEMAP)self,h); 
  L8=L71_;
  tk = L8.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  ret_val2 = ELT_NI455285430(((ELT_NILdOB) NULL), is_key_nil_e);
  if (ret_val2) {
   goto after_loop;
  }
  else {
   if (NAMEMA291126687(self, tk, k)) {
    L91_=ARR((NAMEMAP)self,h); 
    L10=L91_;
    ret_val = L10.t2;
    return ret_val;
   }
  }
  L111_=INTPLUS(h,1); 
  h = L111_;
 }
 after_loop: ;
 L121_=ASIZE((NAMEMAP)self); 
 L13 = L121_;
 L141_=INTMINUS(L13,1); 
 L15 = L141_;
 L161_=(h)==(L15); 
 if (L161_) {
  h = 0;
  while (1) {
   L171_=ARR((NAMEMAP)self,h); 
   L18=L171_;
   tk1 = L18.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   ret_val3 = ELT_NI455285430(((ELT_NILdOB) NULL), is_key_nil_e1);
   if (ret_val3) {
    goto after_loop1;
   }
   else {
    if (NAMEMA291126687(self, tk1, k)) {
     L191_=ARR((NAMEMAP)self,h); 
     L20=L191_;
     ret_val = L20.t2;
     return ret_val;
    }
   }
   L221_=INTPLUS(h,1); 
   h = L221_;
  }
  after_loop1: ;
 }
 ret_val = ((STR) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void MANGLE1827205184(MANGLE self, OB ob, STR s, OB ns) {
 NAMESPACE sp;
 STR x;
 MANGLE space_self;
 OB space_ns;
 NAMESPACE ret_val;
 NAMESPACE r;
 NAMESPACE create_self;
 NAMESPACE ret_val1;
 NAMESPACE r1;
 NAMEMAP create_self1;
 NAMEMAP ret_val2;
 FSETSTR create_self2;
 FSETSTR ret_val3;
 FMAPSTRINT create_self3;
 FMAPSTRINT ret_val4;
 NAMESPACE get_self;
 OB get_ob;
 STR ret_val5;
 NAMESPACE is_taken_self;
 STR is_taken_s;
 BOOL ret_val6 = BOOL_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val7;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val8;
 MANGLE warning_self;
 STR warning_s;
 NAMESPACE insert_self;
 OB insert_ob;
 STR insert_nm;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val9;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val10;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val11;
 MANGLE barf_self;
 STR barf_msg;
 PROG barf_self1;
 STR barf_msg1;
 PROG barf_at_self;
 STR barf_at_msg;
 dPROG_ERR barf_at_at;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val12;
 UNIX exit_self;
 INT exit_code = INT_zero;
 NAMESPACE L1;
 OB L2;
 extern STR Name;
 extern STR alread1884374476;
 BOOL L3;
 BOOL L41_;
 extern STR Name;
 extern STR couldn226966787;
 extern STR Intern28965746;
 space_self = self;
 space_ns = ns;
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
  ret_val2 = ((NAMEMAP) NULL);
  SATTR(r1,map,ret_val2);
  create_self2 = ((FSETSTR) NULL);
  ret_val3 = ((FSETSTR) NULL);
  SATTR(r1,set,ret_val3);
  SATTR(r1,counter,0);
  create_self3 = ((FMAPSTRINT) NULL);
  ret_val4 = ((FMAPSTRINT) NULL);
  SATTR(r1,uniques,ret_val4);
  ret_val1 = r1;
  r = ret_val1;
  SATTR(space_self,namespaces,FMAPdO1199466663(ATTR(space_self,namespaces), space_ns, r));
 }
 ret_val = r;
 sp = ret_val;
 get_self = sp;
 get_ob = ob;
 ret_val5 = NAMEMA1183587160(ATTR(get_self,map), get_ob);
 x = ret_val5;
 if ((x==((STR) NULL))) {
  is_taken_self = sp;
  is_taken_s = s;
  ret_val6 = FSETST1025458804(ATTR(is_taken_self,set), is_taken_s);
  if (ret_val6) {
   warning_self = self;
   plus_self = ((STR) &Name);
   plus_sarg = s;
   ret_val7 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val7;
   plus_sarg1 = ((STR) &alread1884374476);
   ret_val8 = STR_ap2004550586(plus_self1, plus_sarg1);
   warning_s = ret_val8;
   PROG_warning_STR(ATTR(warning_self,prog), warning_s);
  }
  insert_self = sp;
  insert_ob = ob;
  insert_nm = s;
  SATTR(insert_self,map,NAMEMA2020020771(ATTR(insert_self,map), insert_ob, insert_nm));
  SATTR(insert_self,set,FSETST1404644833(ATTR(insert_self,set), insert_nm));
 }
 else {
  L3 = STR_is111530248(x, s);
  L41_=!(L3); 
  if (L41_) {
   barf_self = self;
   plus_self2 = ((STR) &Name);
   plus_sarg2 = s;
   ret_val9 = STR_ap2004550586(plus_self2, plus_sarg2);
   plus_self3 = ret_val9;
   plus_sarg3 = ((STR) &couldn226966787);
   ret_val10 = STR_ap2004550586(plus_self3, plus_sarg3);
   plus_self4 = ret_val10;
   plus_sarg4 = x;
   ret_val11 = STR_ap2004550586(plus_self4, plus_sarg4);
   barf_msg = ret_val11;
   barf_self1 = ATTR(barf_self,prog);
   barf_msg1 = barf_msg;
   barf_at_self = barf_self1;
   barf_at_msg = barf_msg1;
   barf_at_at = ((dPROG_ERR) NULL);
   PROG_e176405615(barf_at_self, barf_at_at);
   plus_self5 = ((STR) &Intern28965746);
   plus_sarg5 = barf_at_msg;
   ret_val12 = STR_ap2004550586(plus_self5, plus_sarg5);
   PROG_err_STR(barf_at_self, ret_val12);
   exit_self = ((UNIX) NULL);
   exit_code = 1;
   exit(exit_code);
  }
 }
}


#undef IS_ITER
#define IS_ITER 1

TUPdOBSTR NAMEMA1836164318(NAMEMA1836164318_frame frame) {
 TUPdOBSTR dummy = TUPdOBSTR_zero;
 TUPdOBSTR aI_u_I;
 TUPdOBSTR L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((NAMEMAP)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((NAMEMAP)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPdOBSTR NAMEMA726632978(NAMEMA726632978_frame frame) {
 TUPdOBSTR dummy = TUPdOBSTR_zero;
 BOOL L1;
 BOOL L21_;
 TUPdOBSTR L5;
 TUPdOBSTR aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((NAMEMAP) NULL));
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
     frame->L41_br=ASIZE((NAMEMAP)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((NAMEMAP)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->r.t1;
    frame->ret_val = ELT_NI455285430(((ELT_NILdOB) NULL), frame->is_key_nil_e);
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

void NAMEMA1267087904(NAMEMA1267087904_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((NAMEMAP)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((NAMEMAP)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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
