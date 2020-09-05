#include "sather.h"

/* Layouts */

typedef struct dAM_EXPR_struct {
 OB_HEADER header;
 } *dAM_EXPR;

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

typedef struct CSE_DOUBLE_EXPRS_struct {/* layout for CSE_DOUBLE_EXPRS */
 OB_HEADER header;
 struct FLISTdAM_EXPR_struct *expr;
 struct FLISTAM_CURSOR_struct *pos;
 struct FLISTA725283029_struct *locals1;
 } *CSE_DOUBLE_EXPRS;

typedef struct ELT_EQ702804563_struct {/* layout for ELT_EQ{AM_ROUT_DEF} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ702804563;

typedef struct ELT_EQ1304198768_struct {/* layout for ELT_EQ{AM_SHARED_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ1304198768;

typedef struct ELT_EQ365890604_struct {/* layout for ELT_EQ{AS_FEAT_MOD} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ365890604;

typedef struct ELT_EQ1712035353_struct {/* layout for ELT_EQ{BOUND_ITER_TYPE_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ1712035353;

typedef struct ELT_EQ1334130421_struct {/* layout for ELT_EQ{BOUND_TYPE_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ1334130421;

typedef struct ELT_EQ1400400210_struct {/* layout for ELT_EQ{BUILTIN_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ1400400210;

typedef struct ELT_EQ1657948444_struct {/* layout for ELT_EQ{CLASS_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ1657948444;

typedef struct ELT_EQ1507878401_struct {/* layout for ELT_EQ{CSE_DOUBLE_EXPRS} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ1507878401;

typedef struct ELT_EQ482950930_struct {/* layout for ELT_EQ{EXTERNAL_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ482950930;

typedef struct ELT_HASH_struct {/* layout for ELT_HASH */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_HASH;

typedef struct ELT_NI602850927_struct {/* layout for ELT_NIL{AM_ROUT_DEF} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI602850927;

typedef struct ELT_NI1377997731_struct {/* layout for ELT_NIL{AM_SHARED_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI1377997731;

typedef struct ELT_NI465844240_struct {/* layout for ELT_NIL{AS_FEAT_MOD} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI465844240;

typedef struct ELT_NI338362486_struct {/* layout for ELT_NIL{BOUND_ITER_TYPE_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI338362486;

typedef struct ELT_NI1119971763_struct {/* layout for ELT_NIL{BOUND_TYPE_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI1119971763;

typedef struct ELT_NI1281796289_struct {/* layout for ELT_NIL{BUILTIN_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI1281796289;

typedef struct ELT_NI737899749_struct {/* layout for ELT_NIL{CLASS_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI737899749;

typedef struct ELT_NI881793976_struct {/* layout for ELT_NIL{CSE_DOUBLE_EXPRS} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI881793976;

typedef struct ELT_NI94923122_struct {/* layout for ELT_NIL{EXTERNAL_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI94923122;

typedef struct EXTERNAL_LAYOUT_struct {/* layout for EXTERNAL_LAYOUT */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct AM_OB_DEF_struct *l;
 struct PROG_struct *prog;
 STR str;
 } *EXTERNAL_LAYOUT;

typedef struct IDENT_struct {/* layout for IDENT */
 STR str;
 } IDENT;
static IDENT IDENT_zero;

typedef struct IDENT_boxed_struct {
 OB_HEADER header;
 IDENT immutable_part;
 } *IDENT_boxed;

typedef struct SFILE_ID_struct {/* layout for SFILE_ID */
 INT loc;
 } SFILE_ID;
static SFILE_ID SFILE_ID_zero;

typedef struct SFILE_ID_boxed_struct {
 OB_HEADER header;
 SFILE_ID immutable_part;
 } *SFILE_ID_boxed;

typedef struct SYS_struct {/* layout for SYS */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *SYS;

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

typedef struct AS_FEAT_MOD_struct {/* layout for AS_FEAT_MOD */
 OB_HEADER header;
 struct AS_FEAT_MOD_struct *next;
 struct IDENT_struct name1;
 struct IDENT_struct new_name;
 struct SFILE_ID_struct source1;
 BOOL is_private;
 BOOL is_readonly;
 } *AS_FEAT_MOD;

typedef struct FLISTdAM_EXPR_struct {/* layout for FLIST{$AM_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dAM_EXPR_struct *arr_part[1];
 } *FLISTdAM_EXPR;

typedef struct FSETAM_ROUT_DEF_struct {/* layout for FSET{AM_ROUT_DEF} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct AM_ROUT_DEF_struct *arr_part[1];
 } *FSETAM_ROUT_DEF;

typedef struct FSETAM1263255605_frame_struct {
 INT state;
 FSETAM_ROUT_DEF self;/* Formal argument: self */
 AM_ROUT_DEF ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETAM1263255605_frame;

typedef struct FSETAM541410851_frame_struct {
 INT state;
 FSETAM_ROUT_DEF self;/* Formal argument: self */
 AM_ROUT_DEF arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETAM541410851_frame;

typedef struct FSETAM1323524382_frame_struct {
 INT state;
 FSETAM_ROUT_DEF self;/* Formal argument: self */
 AM_ROUT_DEF ret_val2;
 FSETAM_ROUT_DEF L61;
 AM_ROUT_DEF r;
 INT i;
 INT sz;
 INT L31;
 FSETAM_ROUT_DEF is_elt_nil_self;
 AM_ROUT_DEF is_elt_nil_e;
 BOOL ret_val;
 ELT_NI602850927 is_elt_nil_self1;
 AM_ROUT_DEF is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETAM1323524382_frame;

typedef struct FSETAM1542678947_struct {/* layout for FSET{AM_SHARED_EXPR} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct AM_SHARED_EXPR_struct *arr_part[1];
 } *FSETAM1542678947;

typedef struct FSETAM674227038_frame_struct {
 INT state;
 FSETAM1542678947 self;/* Formal argument: self */
 AM_SHARED_EXPR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETAM674227038_frame;

typedef struct FSETAM289897564_frame_struct {
 INT state;
 FSETAM1542678947 self;/* Formal argument: self */
 AM_SHARED_EXPR arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETAM289897564_frame;

typedef struct FSETAM12483681_frame_struct {
 INT state;
 FSETAM1542678947 self;/* Formal argument: self */
 AM_SHARED_EXPR ret_val2;
 FSETAM1542678947 L61;
 AM_SHARED_EXPR r;
 INT i;
 INT sz;
 INT L31;
 FSETAM1542678947 is_elt_nil_self;
 AM_SHARED_EXPR is_elt_nil_e;
 BOOL ret_val;
 ELT_NI1377997731 is_elt_nil_self1;
 AM_SHARED_EXPR is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETAM12483681_frame;

typedef struct FSETAS_FEAT_MOD_struct {/* layout for FSET{AS_FEAT_MOD} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct AS_FEAT_MOD_struct *arr_part[1];
 } *FSETAS_FEAT_MOD;

typedef struct FSETAS18373515_frame_struct {
 INT state;
 FSETAS_FEAT_MOD self;/* Formal argument: self */
 AS_FEAT_MOD ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETAS18373515_frame;

typedef struct FSETAS703471239_frame_struct {
 INT state;
 FSETAS_FEAT_MOD self;/* Formal argument: self */
 AS_FEAT_MOD arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETAS703471239_frame;

typedef struct FSETAS1592299594_frame_struct {
 INT state;
 FSETAS_FEAT_MOD self;/* Formal argument: self */
 AS_FEAT_MOD ret_val2;
 FSETAS_FEAT_MOD L61;
 AS_FEAT_MOD r;
 INT i;
 INT sz;
 INT L31;
 FSETAS_FEAT_MOD is_elt_nil_self;
 AS_FEAT_MOD is_elt_nil_e;
 BOOL ret_val;
 ELT_NI465844240 is_elt_nil_self1;
 AS_FEAT_MOD is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETAS1592299594_frame;

typedef struct FSETBO1548397318_struct {/* layout for FSET{BOUND_ITER_TYPE_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct BOUND_2140373124_struct *arr_part[1];
 } *FSETBO1548397318;

typedef struct FSETBO1041122758_frame_struct {
 INT state;
 FSETBO1548397318 self;/* Formal argument: self */
 BOUND_2140373124 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETBO1041122758_frame;

typedef struct FSETBO1600177472_frame_struct {
 INT state;
 FSETBO1548397318 self;/* Formal argument: self */
 BOUND_2140373124 arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETBO1600177472_frame;

typedef struct FSETBO677183779_frame_struct {
 INT state;
 FSETBO1548397318 self;/* Formal argument: self */
 BOUND_2140373124 ret_val2;
 FSETBO1548397318 L61;
 BOUND_2140373124 r;
 INT i;
 INT sz;
 INT L31;
 FSETBO1548397318 is_elt_nil_self;
 BOUND_2140373124 is_elt_nil_e;
 BOOL ret_val;
 ELT_NI338362486 is_elt_nil_self1;
 BOUND_2140373124 is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETBO677183779_frame;

typedef struct FSETBO1147931784_struct {/* layout for FSET{BOUND_TYPE_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct BOUND_809390774_struct *arr_part[1];
 } *FSETBO1147931784;

typedef struct FSETBO1129496037_frame_struct {
 INT state;
 FSETBO1147931784 self;/* Formal argument: self */
 BOUND_809390774 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETBO1129496037_frame;

typedef struct FSETBO189494685_frame_struct {
 INT state;
 FSETBO1147931784 self;/* Formal argument: self */
 BOUND_809390774 arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETBO189494685_frame;

typedef struct FSETBO10998148_frame_struct {
 INT state;
 FSETBO1147931784 self;/* Formal argument: self */
 BOUND_809390774 ret_val2;
 FSETBO1147931784 L61;
 BOUND_809390774 r;
 INT i;
 INT sz;
 INT L31;
 FSETBO1147931784 is_elt_nil_self;
 BOUND_809390774 is_elt_nil_e;
 BOOL ret_val;
 ELT_NI1119971763 is_elt_nil_self1;
 BOUND_809390774 is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETBO10998148_frame;

typedef struct FSETBU1446477505_struct {/* layout for FSET{BUILTIN_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct BUILTIN_LAYOUT_struct *arr_part[1];
 } *FSETBU1446477505;

typedef struct FSETBU838451994_frame_struct {
 INT state;
 FSETBU1446477505 self;/* Formal argument: self */
 BUILTIN_LAYOUT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETBU838451994_frame;

typedef struct FSETBU1802576596_frame_struct {
 INT state;
 FSETBU1446477505 self;/* Formal argument: self */
 BUILTIN_LAYOUT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETBU1802576596_frame;

typedef struct FSETBU202043707_frame_struct {
 INT state;
 FSETBU1446477505 self;/* Formal argument: self */
 BUILTIN_LAYOUT ret_val2;
 FSETBU1446477505 L61;
 BUILTIN_LAYOUT r;
 INT i;
 INT sz;
 INT L31;
 FSETBU1446477505 is_elt_nil_self;
 BUILTIN_LAYOUT is_elt_nil_e;
 BOOL ret_val;
 ELT_NI1281796289 is_elt_nil_self1;
 BUILTIN_LAYOUT is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETBU202043707_frame;

typedef struct FSETCLASS_LAYOUT_struct {/* layout for FSET{CLASS_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct CLASS_LAYOUT_struct *arr_part[1];
 } *FSETCLASS_LAYOUT;

typedef struct FSETCL108778432_frame_struct {
 INT state;
 FSETCLASS_LAYOUT self;/* Formal argument: self */
 CLASS_LAYOUT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETCL108778432_frame;

typedef struct FSETCL938926870_frame_struct {
 INT state;
 FSETCLASS_LAYOUT self;/* Formal argument: self */
 CLASS_LAYOUT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETCL938926870_frame;

typedef struct FSETCL1233705521_frame_struct {
 INT state;
 FSETCLASS_LAYOUT self;/* Formal argument: self */
 CLASS_LAYOUT ret_val2;
 FSETCLASS_LAYOUT L61;
 CLASS_LAYOUT r;
 INT i;
 INT sz;
 INT L31;
 FSETCLASS_LAYOUT is_elt_nil_self;
 CLASS_LAYOUT is_elt_nil_e;
 BOOL ret_val;
 ELT_NI737899749 is_elt_nil_self1;
 CLASS_LAYOUT is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETCL1233705521_frame;

typedef struct FSETCS1561418284_struct {/* layout for FSET{CSE_DOUBLE_EXPRS} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct CSE_DOUBLE_EXPRS_struct *arr_part[1];
 } *FSETCS1561418284;

typedef struct FSETCS1710945472_frame_struct {
 INT state;
 FSETCS1561418284 self;/* Formal argument: self */
 CSE_DOUBLE_EXPRS ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETCS1710945472_frame;

typedef struct FSETCS1554315126_frame_struct {
 INT state;
 FSETCS1561418284 self;/* Formal argument: self */
 CSE_DOUBLE_EXPRS arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETCS1554315126_frame;

typedef struct FSETCS1170733135_frame_struct {
 INT state;
 FSETCS1561418284 self;/* Formal argument: self */
 CSE_DOUBLE_EXPRS ret_val2;
 FSETCS1561418284 L61;
 CSE_DOUBLE_EXPRS r;
 INT i;
 INT sz;
 INT L31;
 FSETCS1561418284 is_elt_nil_self;
 CSE_DOUBLE_EXPRS is_elt_nil_e;
 BOOL ret_val;
 ELT_NI881793976 is_elt_nil_self1;
 CSE_DOUBLE_EXPRS is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETCS1170733135_frame;

typedef struct FSETEX1260947371_struct {/* layout for FSET{EXTERNAL_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct EXTERNAL_LAYOUT_struct *arr_part[1];
 } *FSETEX1260947371;

typedef struct FSETEX1787855975_frame_struct {
 INT state;
 FSETEX1260947371 self;/* Formal argument: self */
 EXTERNAL_LAYOUT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETEX1787855975_frame;

typedef struct FSETEX1368613067_frame_struct {
 INT state;
 FSETEX1260947371 self;/* Formal argument: self */
 EXTERNAL_LAYOUT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETEX1368613067_frame;

typedef struct FSETEX1633103446_frame_struct {
 INT state;
 FSETEX1260947371 self;/* Formal argument: self */
 EXTERNAL_LAYOUT ret_val2;
 FSETEX1260947371 L61;
 EXTERNAL_LAYOUT r;
 INT i;
 INT sz;
 INT L31;
 FSETEX1260947371 is_elt_nil_self;
 EXTERNAL_LAYOUT is_elt_nil_e;
 BOOL ret_val;
 ELT_NI94923122 is_elt_nil_self1;
 EXTERNAL_LAYOUT is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETEX1633103446_frame;

#include "tags.h"

/* Globals */

extern BOOL FSETAM1208812133;
extern BOOL FSETAM1935294117;
extern BOOL FSETAM501048668;
extern BOOL FSETAM649812378;
extern BOOL FSETAS1812528589;
extern BOOL FSETAS46095922;
extern BOOL FSETBO1572491060;
extern BOOL FSETBO2074935273;
extern BOOL FSETBO216310762;
extern BOOL FSETBO286133451;
extern BOOL FSETBU1017592727;
extern BOOL FSETBU841031784;
extern BOOL FSETCL2058817423;
extern BOOL FSETCL377525362;
extern BOOL FSETCS1600255829;
extern BOOL FSETCS836086956;
extern BOOL FSETEX1520806408;
extern BOOL FSETEX915536377;
extern INT FSETAM154189325;
extern INT FSETAM1929715922;
extern INT FSETAM295466615;
extern INT FSETAM744778340;
extern INT FSETAS1287498417;
extern INT FSETAS1969137460;
extern INT FSETBO1040487207;
extern INT FSETBO1172349319;
extern INT FSETBO157482060;
extern INT FSETBO717550434;
extern INT FSETBU1565274135;
extern INT FSETBU864102134;
extern INT FSETCL497069731;
extern INT FSETCL786981258;
extern INT FSETCS1883401206;
extern INT FSETCS2145101147;
extern INT FSETEX1632590957;
extern INT FSETEX2023113290;

/* Function declarations */


extern RETURNED_CONST BOOL (**dAM_EX2004971809)(dAM_EXPR, OB);
AM_ROUT_DEF FSETAM1263255605(FSETAM1263255605_frame);
AM_ROUT_DEF FSETAM1323524382(FSETAM1323524382_frame);
AM_ROUT_DEF FSETAM1484639533(FSETAM_ROUT_DEF);
AM_SHARED_EXPR FSETAM12483681(FSETAM12483681_frame);
AM_SHARED_EXPR FSETAM674227038(FSETAM674227038_frame);
AS_FEAT_MOD FSETAS1592299594(FSETAS1592299594_frame);
AS_FEAT_MOD FSETAS18373515(FSETAS18373515_frame);
BOOL AM_SHA623484008(AM_SHARED_EXPR, OB);
BOOL FSETAM1940973993(FSETAM_ROUT_DEF, AM_ROUT_DEF);
BOOL FSETAM195283444(FSETAM_ROUT_DEF, AM_ROUT_DEF);
BOOL FSETAM442373018(FSETAM_ROUT_DEF, AM_ROUT_DEF);
BOOL FSETAM631702418(FSETAM_ROUT_DEF);
BOOL FSETAS1197816120(FSETAS_FEAT_MOD, AS_FEAT_MOD);
BOOL FSETAS242044491(FSETAS_FEAT_MOD, AS_FEAT_MOD);
BOOL FSETAS456491640(FSETAS_FEAT_MOD, AS_FEAT_MOD);
BOUND_2140373124 FSETBO1041122758(FSETBO1041122758_frame);
BOUND_2140373124 FSETBO677183779(FSETBO677183779_frame);
BOUND_809390774 FSETBO10998148(FSETBO10998148_frame);
BOUND_809390774 FSETBO1129496037(FSETBO1129496037_frame);
BUILTIN_LAYOUT FSETBU202043707(FSETBU202043707_frame);
BUILTIN_LAYOUT FSETBU838451994(FSETBU838451994_frame);
CLASS_LAYOUT FSETCL108778432(FSETCL108778432_frame);
CLASS_LAYOUT FSETCL1233705521(FSETCL1233705521_frame);
CSE_DOUBLE_EXPRS FSETCS1072099132(FSETCS1561418284, CSE_DOUBLE_EXPRS);
CSE_DOUBLE_EXPRS FSETCS1170733135(FSETCS1170733135_frame);
CSE_DOUBLE_EXPRS FSETCS1710945472(FSETCS1710945472_frame);
CSE_DOUBLE_EXPRS FSETCS1814959(FSETCS1561418284, CSE_DOUBLE_EXPRS);
CSE_DOUBLE_EXPRS FSETCS924714586(FSETCS1561418284, CSE_DOUBLE_EXPRS);
EXTERNAL_LAYOUT FSETEX1633103446(FSETEX1633103446_frame);
EXTERNAL_LAYOUT FSETEX1787855975(FSETEX1787855975_frame);
FSETAM1542678947 FSETAM1056526605(FSETAM1542678947);
FSETAM1542678947 FSETAM1090993303(FSETAM1542678947, INT);
FSETAM1542678947 FSETAM1287842946(FSETAM1542678947, FSETAM1542678947, AM_SHARED_EXPR);
FSETAM1542678947 FSETAM1298377859(FSETAM1542678947, FSETAM1542678947);
FSETAM1542678947 FSETAM1609335141(FSETAM1542678947);
FSETAM1542678947 FSETAM1748814338(FSETAM1542678947, FSETAM1542678947, AM_SHARED_EXPR);
FSETAM1542678947 FSETAM35504302(FSETAM1542678947);
FSETAM1542678947 FSETAM381445024(FSETAM1542678947, AM_SHARED_EXPR);
FSETAM_ROUT_DEF FSETAM124285384(FSETAM_ROUT_DEF, INT);
FSETAM_ROUT_DEF FSETAM1281071096(FSETAM_ROUT_DEF, AM_ROUT_DEF);
FSETAM_ROUT_DEF FSETAM1353877611(FSETAM_ROUT_DEF, AM_ROUT_DEF);
FSETAM_ROUT_DEF FSETAM1457953461(FSETAM_ROUT_DEF);
FSETAM_ROUT_DEF FSETAM1582329164(FSETAM_ROUT_DEF, FSETAM_ROUT_DEF, AM_ROUT_DEF);
FSETAM_ROUT_DEF FSETAM1947131874(FSETAM_ROUT_DEF, AM_ROUT_DEF);
FSETAM_ROUT_DEF FSETAM2044524049(FSETAM_ROUT_DEF, AM_ROUT_DEF);
FSETAM_ROUT_DEF FSETAM2120233524(FSETAM_ROUT_DEF);
FSETAM_ROUT_DEF FSETAM814128556(FSETAM_ROUT_DEF);
FSETAM_ROUT_DEF FSETAM94979636(FSETAM_ROUT_DEF, FSETAM_ROUT_DEF, AM_ROUT_DEF);
FSETAS_FEAT_MOD FSETAS1457709924(FSETAS_FEAT_MOD, INT);
FSETAS_FEAT_MOD FSETAS1731630631(FSETAS_FEAT_MOD);
FSETAS_FEAT_MOD FSETAS1911338080(FSETAS_FEAT_MOD, FSETAS_FEAT_MOD, AS_FEAT_MOD);
FSETAS_FEAT_MOD FSETAS559779810(FSETAS_FEAT_MOD, AS_FEAT_MOD);
FSETAS_FEAT_MOD FSETAS706320416(FSETAS_FEAT_MOD, FSETAS_FEAT_MOD, AS_FEAT_MOD);
FSETAS_FEAT_MOD FSETAS841309232(FSETAS_FEAT_MOD);
FSETBO1147931784 FSETBO1130450260(FSETBO1147931784, FSETBO1147931784, BOUND_809390774);
FSETBO1147931784 FSETBO113908103(FSETBO1147931784, BOUND_809390774);
FSETBO1147931784 FSETBO1624907678(FSETBO1147931784);
FSETBO1147931784 FSETBO1674616923(FSETBO1147931784);
FSETBO1147931784 FSETBO273868630(FSETBO1147931784, INT);
FSETBO1147931784 FSETBO503820844(FSETBO1147931784, FSETBO1147931784, BOUND_809390774);
FSETBO1548397318 FSETBO1059930149(FSETBO1548397318);
FSETBO1548397318 FSETBO1686201418(FSETBO1548397318, FSETBO1548397318, BOUND_2140373124);
FSETBO1548397318 FSETBO1907478089(FSETBO1548397318, INT);
FSETBO1548397318 FSETBO2112177303(FSETBO1548397318, BOUND_2140373124);
FSETBO1548397318 FSETBO264880646(FSETBO1548397318);
FSETBO1548397318 FSETBO888993482(FSETBO1548397318, FSETBO1548397318, BOUND_2140373124);
FSETBU1446477505 FSETBU1738179145(FSETBU1446477505);
FSETBU1446477505 FSETBU2034709890(FSETBU1446477505, BUILTIN_LAYOUT);
FSETBU1446477505 FSETBU409340763(FSETBU1446477505, INT);
FSETBU1446477505 FSETBU436837902(FSETBU1446477505, FSETBU1446477505, BUILTIN_LAYOUT);
FSETBU1446477505 FSETBU897809294(FSETBU1446477505, FSETBU1446477505, BUILTIN_LAYOUT);
FSETBU1446477505 FSETBU923961098(FSETBU1446477505);
FSETCLASS_LAYOUT FSETCL11745129(FSETCLASS_LAYOUT, INT);
FSETCLASS_LAYOUT FSETCL120396958(FSETCLASS_LAYOUT, FSETCLASS_LAYOUT, CLASS_LAYOUT);
FSETCLASS_LAYOUT FSETCL1622914132(FSETCLASS_LAYOUT);
FSETCLASS_LAYOUT FSETCL1676723742(FSETCLASS_LAYOUT, FSETCLASS_LAYOUT, CLASS_LAYOUT);
FSETCLASS_LAYOUT FSETCL686728372(FSETCLASS_LAYOUT, CLASS_LAYOUT);
FSETCLASS_LAYOUT FSETCL719945875(FSETCLASS_LAYOUT);
FSETCS1561418284 FSETCS1162845217(FSETCS1561418284);
FSETCS1561418284 FSETCS1557128156(FSETCS1561418284);
FSETCS1561418284 FSETCS1791208015(FSETCS1561418284, CSE_DOUBLE_EXPRS);
FSETCS1561418284 FSETCS197028574(FSETCS1561418284, FSETCS1561418284, CSE_DOUBLE_EXPRS);
FSETCS1561418284 FSETCS945551710(FSETCS1561418284, FSETCS1561418284, CSE_DOUBLE_EXPRS);
FSETCS1561418284 FSETCS971548571(FSETCS1561418284, INT);
FSETEX1260947371 FSETEX1004991460(FSETEX1260947371);
FSETEX1260947371 FSETEX1141803248(FSETEX1260947371, INT);
FSETEX1260947371 FSETEX1154744664(FSETEX1260947371, FSETEX1260947371, EXTERNAL_LAYOUT);
FSETEX1260947371 FSETEX1571333349(FSETEX1260947371);
FSETEX1260947371 FSETEX1746916700(FSETEX1260947371, EXTERNAL_LAYOUT);
FSETEX1260947371 FSETEX1861592536(FSETEX1260947371, FSETEX1260947371, EXTERNAL_LAYOUT);
INT ELT_HA1612720024(ELT_HASH, OB);
INT FSETAM1999114804(FSETAM_ROUT_DEF);
INT FSETAM2105800198(FSETAM1542678947);
INT FSETAS77357633(FSETAS_FEAT_MOD);
void FSETAM289897564(FSETAM289897564_frame);
void FSETAM541410851(FSETAM541410851_frame);
void FSETAS703471239(FSETAS703471239_frame);
void FSETBO1600177472(FSETBO1600177472_frame);
void FSETBO189494685(FSETBO189494685_frame);
void FSETBU1802576596(FSETBU1802576596_frame);
void FSETCL938926870(FSETCL938926870_frame);
void FSETCS1554315126(FSETCS1554315126_frame);
void FSETEX1368613067(FSETEX1368613067_frame);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */




#undef IS_ITER
#define IS_ITER 0

AM_ROUT_DEF FSETAM1484639533(FSETAM_ROUT_DEF self) {
 AM_ROUT_DEF ret_val;
 FSETAM_ROUT_DEF L11;
 AM_ROUT_DEF r;
 INT L21 = INT_zero;
 FSETAM_ROUT_DEF is_elt_nil_self;
 AM_ROUT_DEF is_elt_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NI602850927 is_elt_nil_self1;
 AM_ROUT_DEF is_elt_nil_e1;
 BOOL ret_val2 = BOOL_zero;
 FSETAM_ROUT_DEF elt_nil_self;
 AM_ROUT_DEF ret_val3;
 ELT_NI602850927 elt_nil_self1;
 AM_ROUT_DEF ret_val4;
 AM_ROUT_DEF e = ((AM_ROUT_DEF) NULL);
 BOOL L3;
 BOOL L41_;
 INT L51_br;
 AM_ROUT_DEF aL51_;
 AM_ROUT_DEF L6;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 BOOL L121_;
 AM_ROUT_DEF L131_;
 L3 = (self==((FSETAM_ROUT_DEF) NULL));
 L41_=!(L3); 
 if (L41_) {
  if (ATTR(self,use_map)) {
   {
    BOOL f_L51_ = TRUE;
    /* loop index variable */
    L21 = 0;
    L11 = self;
    L51_br=ASIZE((FSETAM_ROUT_DEF)L11); 
    while (1) {
     if(L21>=L51_br)  goto after_loop; 
     aL51_=ARR((FSETAM_ROUT_DEF)L11,L21); 
     r = aL51_;
     is_elt_nil_self = self;
     is_elt_nil_e = r;
     is_elt_nil_self1 = ((ELT_NI602850927) NULL);
     is_elt_nil_e1 = is_elt_nil_e;
     ret_val2 = (is_elt_nil_e1==((AM_ROUT_DEF) NULL));
     ret_val1 = ret_val2;
     L7 = ret_val1;
     L81_=!(L7); 
     if (L81_) {
      ret_val = r;
      return ret_val;
     }
     L91_=INTPLUS(L21,1); 
     L21 = L91_;
    }
   }
   after_loop: ;
  }
  else {
   L10 = ATTR(self,hsize);
   L121_=(0)<(L10); 
   if (L121_) {
    L131_=(AM_ROUT_DEF)ARR((FSETAM_ROUT_DEF)self,0); 
    ret_val = L131_;
    return ret_val;
   }
  }
 }
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NI602850927) NULL);
 ret_val4 = ((AM_ROUT_DEF) NULL);
 ret_val3 = ret_val4;
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL FSETAM1940973993(FSETAM_ROUT_DEF self, AM_ROUT_DEF e) {
 BOOL ret_val = BOOL_zero;
 INT h = INT_zero;
 AM_ROUT_DEF te;
 AM_ROUT_DEF te1;
 FSETAM_ROUT_DEF elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETAM_ROUT_DEF is_elt_nil_self;
 AM_ROUT_DEF is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI602850927 is_elt_nil_self1;
 AM_ROUT_DEF is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 FSETAM_ROUT_DEF elt_eq_self;
 AM_ROUT_DEF elt_eq_e1;
 AM_ROUT_DEF elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ702804563 elt_eq_self1;
 AM_ROUT_DEF elt_eq_e11;
 AM_ROUT_DEF elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 FSETAM_ROUT_DEF is_elt_nil_self2;
 AM_ROUT_DEF is_elt_nil_e2;
 BOOL ret_val6 = BOOL_zero;
 ELT_NI602850927 is_elt_nil_self3;
 AM_ROUT_DEF is_elt_nil_e3;
 BOOL ret_val7 = BOOL_zero;
 FSETAM_ROUT_DEF elt_eq_self2;
 AM_ROUT_DEF elt_eq_e12;
 AM_ROUT_DEF elt_eq_e22;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQ702804563 elt_eq_self3;
 AM_ROUT_DEF elt_eq_e13;
 AM_ROUT_DEF elt_eq_e23;
 BOOL ret_val9 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 AM_ROUT_DEF L71_;
 INT L81_;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 BOOL L131_;
 AM_ROUT_DEF L141_;
 INT L151_;
 elt_hash_self = self;
 elt_hash_e = ((OB) e);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FSETAM_ROUT_DEF)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=(AM_ROUT_DEF)ARR((FSETAM_ROUT_DEF)self,h); 
  te = L71_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NI602850927) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1==((AM_ROUT_DEF) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQ702804563) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   ret_val5 = (elt_eq_e11==elt_eq_e21);
   ret_val4 = ret_val5;
   if (ret_val4) {
    ret_val = TRUE;
    return ret_val;
   }
  }
  L81_=INTPLUS(h,1); 
  h = L81_;
 }
 after_loop: ;
 L91_=ASIZE((FSETAM_ROUT_DEF)self); 
 L10 = L91_;
 L111_=INTMINUS(L10,1); 
 L12 = L111_;
 L131_=(h)==(L12); 
 if (L131_) {
  h = 0;
  while (1) {
   L141_=(AM_ROUT_DEF)ARR((FSETAM_ROUT_DEF)self,h); 
   te1 = L141_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NI602850927) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val7 = (is_elt_nil_e3==((AM_ROUT_DEF) NULL));
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQ702804563) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    ret_val9 = (elt_eq_e13==elt_eq_e23);
    ret_val8 = ret_val9;
    if (ret_val8) {
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

BOOL FSETAM195283444(FSETAM_ROUT_DEF self, AM_ROUT_DEF e) {
 BOOL ret_val = BOOL_zero;
 if ((self==((FSETAM_ROUT_DEF) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 if (ATTR(self,use_map)) {
  ret_val = FSETAM1940973993(self, e);
  return ret_val;
 }
 else {
  ret_val = FSETAM442373018(self, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL FSETAM442373018(FSETAM_ROUT_DEF self, AM_ROUT_DEF e) {
 BOOL ret_val = BOOL_zero;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETAM_ROUT_DEF elt_eq_self;
 AM_ROUT_DEF elt_eq_e1;
 AM_ROUT_DEF elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ702804563 elt_eq_self1;
 AM_ROUT_DEF elt_eq_e11;
 AM_ROUT_DEF elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 BOOL L11_;
 AM_ROUT_DEF L21_;
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
  L21_=(AM_ROUT_DEF)ARR((FSETAM_ROUT_DEF)self,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQ702804563) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  ret_val2 = (elt_eq_e11==elt_eq_e21);
  ret_val1 = ret_val2;
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

BOOL FSETAM631702418(FSETAM_ROUT_DEF self) {
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
 L21_=ASIZE((FSETAM_ROUT_DEF)self); 
 L3 = L21_;
 L41_=(L3)<(33); 
 L5 = L41_;
 L61_=!(L5); 
 if (L61_) {
  L7 = ATTR(self,hsize);
  L81_=ASIZE((FSETAM_ROUT_DEF)self); 
  L9 = L81_;
  L101_=INTMINUS(L9,1); 
  L11 = L101_;
  L12 = FSETAM295466615;
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

BOOL FSETAS1197816120(FSETAS_FEAT_MOD self, AS_FEAT_MOD e) {
 BOOL ret_val = BOOL_zero;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETAS_FEAT_MOD elt_eq_self;
 AS_FEAT_MOD elt_eq_e1;
 AS_FEAT_MOD elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ365890604 elt_eq_self1;
 AS_FEAT_MOD elt_eq_e11;
 AS_FEAT_MOD elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 BOOL L11_;
 AS_FEAT_MOD L21_;
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
  L21_=(AS_FEAT_MOD)ARR((FSETAS_FEAT_MOD)self,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQ365890604) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  ret_val2 = (elt_eq_e11==elt_eq_e21);
  ret_val1 = ret_val2;
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

BOOL FSETAS242044491(FSETAS_FEAT_MOD self, AS_FEAT_MOD e) {
 BOOL ret_val = BOOL_zero;
 INT h = INT_zero;
 AS_FEAT_MOD te;
 AS_FEAT_MOD te1;
 FSETAS_FEAT_MOD elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETAS_FEAT_MOD is_elt_nil_self;
 AS_FEAT_MOD is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI465844240 is_elt_nil_self1;
 AS_FEAT_MOD is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 FSETAS_FEAT_MOD elt_eq_self;
 AS_FEAT_MOD elt_eq_e1;
 AS_FEAT_MOD elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ365890604 elt_eq_self1;
 AS_FEAT_MOD elt_eq_e11;
 AS_FEAT_MOD elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 FSETAS_FEAT_MOD is_elt_nil_self2;
 AS_FEAT_MOD is_elt_nil_e2;
 BOOL ret_val6 = BOOL_zero;
 ELT_NI465844240 is_elt_nil_self3;
 AS_FEAT_MOD is_elt_nil_e3;
 BOOL ret_val7 = BOOL_zero;
 FSETAS_FEAT_MOD elt_eq_self2;
 AS_FEAT_MOD elt_eq_e12;
 AS_FEAT_MOD elt_eq_e22;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQ365890604 elt_eq_self3;
 AS_FEAT_MOD elt_eq_e13;
 AS_FEAT_MOD elt_eq_e23;
 BOOL ret_val9 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 AS_FEAT_MOD L71_;
 INT L81_;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 BOOL L131_;
 AS_FEAT_MOD L141_;
 INT L151_;
 elt_hash_self = self;
 elt_hash_e = ((OB) e);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FSETAS_FEAT_MOD)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=(AS_FEAT_MOD)ARR((FSETAS_FEAT_MOD)self,h); 
  te = L71_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NI465844240) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1==((AS_FEAT_MOD) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQ365890604) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   ret_val5 = (elt_eq_e11==elt_eq_e21);
   ret_val4 = ret_val5;
   if (ret_val4) {
    ret_val = TRUE;
    return ret_val;
   }
  }
  L81_=INTPLUS(h,1); 
  h = L81_;
 }
 after_loop: ;
 L91_=ASIZE((FSETAS_FEAT_MOD)self); 
 L10 = L91_;
 L111_=INTMINUS(L10,1); 
 L12 = L111_;
 L131_=(h)==(L12); 
 if (L131_) {
  h = 0;
  while (1) {
   L141_=(AS_FEAT_MOD)ARR((FSETAS_FEAT_MOD)self,h); 
   te1 = L141_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NI465844240) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val7 = (is_elt_nil_e3==((AS_FEAT_MOD) NULL));
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQ365890604) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    ret_val9 = (elt_eq_e13==elt_eq_e23);
    ret_val8 = ret_val9;
    if (ret_val8) {
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

BOOL FSETAS456491640(FSETAS_FEAT_MOD self, AS_FEAT_MOD e) {
 BOOL ret_val = BOOL_zero;
 if ((self==((FSETAS_FEAT_MOD) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 if (ATTR(self,use_map)) {
  ret_val = FSETAS242044491(self, e);
  return ret_val;
 }
 else {
  ret_val = FSETAS1197816120(self, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

CSE_DOUBLE_EXPRS FSETCS1072099132(FSETCS1561418284 self, CSE_DOUBLE_EXPRS e) {
 CSE_DOUBLE_EXPRS ret_val;
 INT h = INT_zero;
 CSE_DOUBLE_EXPRS te;
 CSE_DOUBLE_EXPRS te1;
 FSETCS1561418284 elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETCS1561418284 is_elt_nil_self;
 CSE_DOUBLE_EXPRS is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI881793976 is_elt_nil_self1;
 CSE_DOUBLE_EXPRS is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 FSETCS1561418284 elt_eq_self;
 CSE_DOUBLE_EXPRS elt_eq_e1;
 CSE_DOUBLE_EXPRS elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ1507878401 elt_eq_self1;
 CSE_DOUBLE_EXPRS elt_eq_e11;
 CSE_DOUBLE_EXPRS elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 CSE_DOUBLE_EXPRS is_eq_self;
 CSE_DOUBLE_EXPRS is_eq_e;
 BOOL ret_val6 = BOOL_zero;
 FLISTdAM_EXPR aget_self;
 INT aget_ind = INT_zero;
 dAM_EXPR ret_val7;
 FLISTdAM_EXPR aget_self1;
 INT aget_ind1 = INT_zero;
 dAM_EXPR ret_val8;
 FSETCS1561418284 is_elt_nil_self2;
 CSE_DOUBLE_EXPRS is_elt_nil_e2;
 BOOL ret_val9 = BOOL_zero;
 ELT_NI881793976 is_elt_nil_self3;
 CSE_DOUBLE_EXPRS is_elt_nil_e3;
 BOOL ret_val10 = BOOL_zero;
 FSETCS1561418284 elt_eq_self2;
 CSE_DOUBLE_EXPRS elt_eq_e12;
 CSE_DOUBLE_EXPRS elt_eq_e22;
 BOOL ret_val11 = BOOL_zero;
 ELT_EQ1507878401 elt_eq_self3;
 CSE_DOUBLE_EXPRS elt_eq_e13;
 CSE_DOUBLE_EXPRS elt_eq_e23;
 BOOL ret_val12 = BOOL_zero;
 CSE_DOUBLE_EXPRS is_eq_self1;
 CSE_DOUBLE_EXPRS is_eq_e1;
 BOOL ret_val13 = BOOL_zero;
 FLISTdAM_EXPR aget_self2;
 INT aget_ind2 = INT_zero;
 dAM_EXPR ret_val14;
 FLISTdAM_EXPR aget_self3;
 INT aget_ind3 = INT_zero;
 dAM_EXPR ret_val15;
 FSETCS1561418284 elt_nil_self;
 CSE_DOUBLE_EXPRS ret_val16;
 ELT_NI881793976 elt_nil_self1;
 CSE_DOUBLE_EXPRS ret_val17;
 CSE_DOUBLE_EXPRS e1 = ((CSE_DOUBLE_EXPRS) NULL);
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 CSE_DOUBLE_EXPRS L71_;
 dAM_EXPR L81_;
 dAM_EXPR L91_;
 dAM_EXPR L10;
 INT L111_;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 BOOL L161_;
 CSE_DOUBLE_EXPRS L171_;
 dAM_EXPR L181_;
 dAM_EXPR L191_;
 dAM_EXPR L20;
 INT L221_;
 elt_hash_self = self;
 elt_hash_e = ((OB) e);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FSETCS1561418284)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=(CSE_DOUBLE_EXPRS)ARR((FSETCS1561418284)self,h); 
  te = L71_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NI881793976) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1==((CSE_DOUBLE_EXPRS) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQ1507878401) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   is_eq_self = elt_eq_e11;
   is_eq_e = elt_eq_e21;
   aget_self = ATTR(is_eq_self,expr);
   aget_ind = 0;
   L81_=(dAM_EXPR)ARR((FLISTdAM_EXPR)aget_self,aget_ind); 
   ret_val7 = L81_;
   aget_self1 = ATTR(is_eq_e,expr);
   aget_ind1 = 0;
   L91_=(dAM_EXPR)ARR((FLISTdAM_EXPR)aget_self1,aget_ind1); 
   ret_val8 = L91_;
   L10 = ret_val7;
   ret_val6 = (*dAM_EX2004971809[TAG(L10)])(L10, ((OB) ret_val8));
   ret_val5 = ret_val6;
   ret_val4 = ret_val5;
   if (ret_val4) {
    ret_val = te;
    return ret_val;
   }
  }
  L111_=INTPLUS(h,1); 
  h = L111_;
 }
 after_loop: ;
 L121_=ASIZE((FSETCS1561418284)self); 
 L13 = L121_;
 L141_=INTMINUS(L13,1); 
 L15 = L141_;
 L161_=(h)==(L15); 
 if (L161_) {
  h = 0;
  while (1) {
   L171_=(CSE_DOUBLE_EXPRS)ARR((FSETCS1561418284)self,h); 
   te1 = L171_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NI881793976) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val10 = (is_elt_nil_e3==((CSE_DOUBLE_EXPRS) NULL));
   ret_val9 = ret_val10;
   if (ret_val9) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQ1507878401) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    is_eq_self1 = elt_eq_e13;
    is_eq_e1 = elt_eq_e23;
    aget_self2 = ATTR(is_eq_self1,expr);
    aget_ind2 = 0;
    L181_=(dAM_EXPR)ARR((FLISTdAM_EXPR)aget_self2,aget_ind2); 
    ret_val14 = L181_;
    aget_self3 = ATTR(is_eq_e1,expr);
    aget_ind3 = 0;
    L191_=(dAM_EXPR)ARR((FLISTdAM_EXPR)aget_self3,aget_ind3); 
    ret_val15 = L191_;
    L20 = ret_val14;
    ret_val13 = (*dAM_EX2004971809[TAG(L20)])(L20, ((OB) ret_val15));
    ret_val12 = ret_val13;
    ret_val11 = ret_val12;
    if (ret_val11) {
     ret_val = te1;
     return ret_val;
    }
   }
   L221_=INTPLUS(h,1); 
   h = L221_;
  }
  after_loop1: ;
 }
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NI881793976) NULL);
 ret_val17 = ((CSE_DOUBLE_EXPRS) NULL);
 ret_val16 = ret_val17;
 ret_val = ret_val16;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

CSE_DOUBLE_EXPRS FSETCS1814959(FSETCS1561418284 self, CSE_DOUBLE_EXPRS e) {
 CSE_DOUBLE_EXPRS ret_val;
 FSETCS1561418284 elt_nil_self;
 CSE_DOUBLE_EXPRS ret_val1;
 ELT_NI881793976 elt_nil_self1;
 CSE_DOUBLE_EXPRS ret_val2;
 CSE_DOUBLE_EXPRS e1 = ((CSE_DOUBLE_EXPRS) NULL);
 if ((self==((FSETCS1561418284) NULL))) {
  elt_nil_self = self;
  elt_nil_self1 = ((ELT_NI881793976) NULL);
  ret_val2 = ((CSE_DOUBLE_EXPRS) NULL);
  ret_val1 = ret_val2;
  ret_val = ret_val1;
  return ret_val;
 }
 if (ATTR(self,use_map)) {
  ret_val = FSETCS1072099132(self, e);
  return ret_val;
 }
 else {
  ret_val = FSETCS924714586(self, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

CSE_DOUBLE_EXPRS FSETCS924714586(FSETCS1561418284 self, CSE_DOUBLE_EXPRS e) {
 CSE_DOUBLE_EXPRS ret_val;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETCS1561418284 elt_eq_self;
 CSE_DOUBLE_EXPRS elt_eq_e1;
 CSE_DOUBLE_EXPRS elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ1507878401 elt_eq_self1;
 CSE_DOUBLE_EXPRS elt_eq_e11;
 CSE_DOUBLE_EXPRS elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 CSE_DOUBLE_EXPRS is_eq_self;
 CSE_DOUBLE_EXPRS is_eq_e;
 BOOL ret_val3 = BOOL_zero;
 FLISTdAM_EXPR aget_self;
 INT aget_ind = INT_zero;
 dAM_EXPR ret_val4;
 FLISTdAM_EXPR aget_self1;
 INT aget_ind1 = INT_zero;
 dAM_EXPR ret_val5;
 FSETCS1561418284 elt_nil_self;
 CSE_DOUBLE_EXPRS ret_val6;
 ELT_NI881793976 elt_nil_self1;
 CSE_DOUBLE_EXPRS ret_val7;
 CSE_DOUBLE_EXPRS e1 = ((CSE_DOUBLE_EXPRS) NULL);
 BOOL L11_;
 CSE_DOUBLE_EXPRS L21_;
 dAM_EXPR L31_;
 dAM_EXPR L41_;
 dAM_EXPR L5;
 CSE_DOUBLE_EXPRS L61_;
 INT L71_;
 i = 0;
 sz = ATTR(self,hsize);
 while (1) {
  L11_=(i)==(sz); 
  if (L11_) {
   goto after_loop;
  }
  elt_eq_self = self;
  elt_eq_e1 = e;
  L21_=(CSE_DOUBLE_EXPRS)ARR((FSETCS1561418284)self,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQ1507878401) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  is_eq_self = elt_eq_e11;
  is_eq_e = elt_eq_e21;
  aget_self = ATTR(is_eq_self,expr);
  aget_ind = 0;
  L31_=(dAM_EXPR)ARR((FLISTdAM_EXPR)aget_self,aget_ind); 
  ret_val4 = L31_;
  aget_self1 = ATTR(is_eq_e,expr);
  aget_ind1 = 0;
  L41_=(dAM_EXPR)ARR((FLISTdAM_EXPR)aget_self1,aget_ind1); 
  ret_val5 = L41_;
  L5 = ret_val4;
  ret_val3 = (*dAM_EX2004971809[TAG(L5)])(L5, ((OB) ret_val5));
  ret_val2 = ret_val3;
  ret_val1 = ret_val2;
  if (ret_val1) {
   L61_=(CSE_DOUBLE_EXPRS)ARR((FSETCS1561418284)self,i); 
   ret_val = L61_;
   return ret_val;
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NI881793976) NULL);
 ret_val7 = ((CSE_DOUBLE_EXPRS) NULL);
 ret_val6 = ret_val7;
 ret_val = ret_val6;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETAM1542678947 FSETAM1056526605(FSETAM1542678947 self) {
 FSETAM1542678947 ret_val;
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
  L11_=ASIZE((FSETAM1542678947)self); 
  L2 = L11_;
  L3 = ATTR(self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FSETAM154189325;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  if (L91_) {
   ret_val = FSETAM35504302(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
 else {
  L10 = ATTR(self,hsize);
  L121_=ASIZE((FSETAM1542678947)self); 
  L13 = L121_;
  L141_=(L10)<(L13); 
  L15 = L141_;
  L161_=!(L15); 
  if (L161_) {
   ret_val = FSETAM35504302(self);
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

FSETAM1542678947 FSETAM1090993303(FSETAM1542678947 self, INT n) {
 FSETAM1542678947 ret_val;
 FSETAM1542678947 r;
 FSETAM1542678947 L11;
 FSETAM1542678947 elt_nil_self;
 AM_SHARED_EXPR ret_val1;
 ELT_NI1377997731 elt_nil_self1;
 AM_SHARED_EXPR ret_val2;
 AM_SHARED_EXPR e = ((AM_SHARED_EXPR) NULL);
 FSETAM1542678947 elt_nil_self2;
 AM_SHARED_EXPR ret_val3;
 ELT_NI1377997731 elt_nil_self3;
 AM_SHARED_EXPR ret_val4;
 AM_SHARED_EXPR e1 = ((AM_SHARED_EXPR) NULL);
 INT L21 = INT_zero;
 FSETAM1542678947 L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 AM_SHARED_EXPR L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct FSETAM1542678947_struct)-sizeof(AM_SHARED_EXPR))+(L4)*sizeof(AM_SHARED_EXPR);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FSETAM1542678947_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FSETAM1542678947) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NI1377997731) NULL);
 ret_val2 = ((AM_SHARED_EXPR) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((AM_SHARED_EXPR) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FSETAM1542678947)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NI1377997731) NULL);
    ret_val4 = ((AM_SHARED_EXPR) NULL);
    ret_val3 = ret_val4;
    L10 = ret_val3;
    SARR((FSETAM1542678947)L11,L21,(AM_SHARED_EXPR)L10); 
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

FSETAM1542678947 FSETAM1287842946(FSETAM1542678947 self, FSETAM1542678947 r, AM_SHARED_EXPR e) {
 FSETAM1542678947 ret_val;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETAM1542678947 elt_eq_self;
 AM_SHARED_EXPR elt_eq_e1;
 AM_SHARED_EXPR elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ1304198768 elt_eq_self1;
 AM_SHARED_EXPR elt_eq_e11;
 AM_SHARED_EXPR elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 BOOL L11_;
 AM_SHARED_EXPR L21_;
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
  L21_=(AM_SHARED_EXPR)ARR((FSETAM1542678947)r,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQ1304198768) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  ret_val2 = AM_SHA623484008(elt_eq_e11, ((OB) elt_eq_e21));
  ret_val1 = ret_val2;
  if (ret_val1) {
   SARR((FSETAM1542678947)r,i,(AM_SHARED_EXPR)e); 
   ;
   ret_val = r;
   return ret_val;
  }
  L41_=INTPLUS(i,1); 
  i = L41_;
 }
 after_loop: ;
 L5 = ATTR(r,hsize);
 SARR((FSETAM1542678947)r,L5,(AM_SHARED_EXPR)e); 
 ;
 L7 = ATTR(r,hsize);
 L81_=INTPLUS(L7,1); 
 SATTR(r,hsize,L81_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETAM1542678947 FSETAM1298377859(FSETAM1542678947 self, FSETAM1542678947 s) {
 FSETAM1542678947 ret_val;
 FSETAM1542678947 r;
 FSETAM1542678947 L11;
 AM_SHARED_EXPR L2;
 r = self;
 {
  struct FSETAM12483681_frame_struct other1_0;
FSETAM12483681_frame noname1 = &other1_0;
  L11 = s;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FSETAM12483681(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETAM381445024(r, L2);
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETAM1542678947 FSETAM1609335141(FSETAM1542678947 self) {
 FSETAM1542678947 ret_val;
 FSETAM1542678947 r = ((FSETAM1542678947) NULL);
 FSETAM1542678947 L11;
 AM_SHARED_EXPR L2;
 {
  struct FSETAM12483681_frame_struct other1_0;
FSETAM12483681_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FSETAM12483681(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETAM381445024(r, L2);
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETAM1542678947 FSETAM1748814338(FSETAM1542678947 self, FSETAM1542678947 r, AM_SHARED_EXPR e) {
 FSETAM1542678947 ret_val;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 AM_SHARED_EXPR te;
 AM_SHARED_EXPR te1;
 FSETAM1542678947 elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETAM1542678947 is_elt_nil_self;
 AM_SHARED_EXPR is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI1377997731 is_elt_nil_self1;
 AM_SHARED_EXPR is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 FSETAM1542678947 elt_eq_self;
 AM_SHARED_EXPR elt_eq_e1;
 AM_SHARED_EXPR elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ1304198768 elt_eq_self1;
 AM_SHARED_EXPR elt_eq_e11;
 AM_SHARED_EXPR elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 FSETAM1542678947 is_elt_nil_self2;
 AM_SHARED_EXPR is_elt_nil_e2;
 BOOL ret_val6 = BOOL_zero;
 ELT_NI1377997731 is_elt_nil_self3;
 AM_SHARED_EXPR is_elt_nil_e3;
 BOOL ret_val7 = BOOL_zero;
 FSETAM1542678947 elt_eq_self2;
 AM_SHARED_EXPR elt_eq_e12;
 AM_SHARED_EXPR elt_eq_e22;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQ1304198768 elt_eq_self3;
 AM_SHARED_EXPR elt_eq_e13;
 AM_SHARED_EXPR elt_eq_e23;
 BOOL ret_val9 = BOOL_zero;
 INT L11_;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 AM_SHARED_EXPR L61_;
 INT L81_;
 INT L91_;
 INT L10;
 BOOL L121_;
 AM_SHARED_EXPR L131_;
 INT L151_;
 INT L17;
 INT L181_;
 L11_=ASIZE((FSETAM1542678947)r); 
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
  L61_=(AM_SHARED_EXPR)ARR((FSETAM1542678947)r,h); 
  te = L61_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NI1377997731) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1==((AM_SHARED_EXPR) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQ1304198768) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   ret_val5 = AM_SHA623484008(elt_eq_e11, ((OB) elt_eq_e21));
   ret_val4 = ret_val5;
   if (ret_val4) {
    SARR((FSETAM1542678947)r,h,(AM_SHARED_EXPR)e); 
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
   L131_=(AM_SHARED_EXPR)ARR((FSETAM1542678947)r,h); 
   te1 = L131_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NI1377997731) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val7 = (is_elt_nil_e3==((AM_SHARED_EXPR) NULL));
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQ1304198768) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    ret_val9 = AM_SHA623484008(elt_eq_e13, ((OB) elt_eq_e23));
    ret_val8 = ret_val9;
    if (ret_val8) {
     SARR((FSETAM1542678947)r,h,(AM_SHARED_EXPR)e); 
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
 SARR((FSETAM1542678947)r,h,(AM_SHARED_EXPR)e); 
 ;
 L17 = ATTR(r,hsize);
 L181_=INTPLUS(L17,1); 
 SATTR(r,hsize,L181_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETAM1542678947 FSETAM35504302(FSETAM1542678947 self) {
 FSETAM1542678947 ret_val;
 FSETAM1542678947 r;
 FSETAM1542678947 L11;
 FSETAM1542678947 switch_structure;
 BOOL switch_structure1 = BOOL_zero;
 BOOL switch_structure2 = BOOL_zero;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 AM_SHARED_EXPR L9;
 L21_=ASIZE((FSETAM1542678947)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = FSETAM1090993303(self, L81_);
 switch_structure = r;
 switch_structure1 = ATTR(self,use_map);
 switch_structure2 = TRUE;
 if (FSETAM649812378) {
  SATTR(switch_structure,use_map,switch_structure2);
 }
 else {
  SATTR(switch_structure,use_map,switch_structure1);
 }
 {
  struct FSETAM12483681_frame_struct other1_0;
FSETAM12483681_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETAM12483681(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETAM381445024(r, L9);
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

FSETAM1542678947 FSETAM381445024(FSETAM1542678947 self, AM_SHARED_EXPR e) {
 FSETAM1542678947 ret_val;
 FSETAM1542678947 r;
 FSETAM1542678947 set_initial_stru;
 r = self;
 if ((r==((FSETAM1542678947) NULL))) {
  r = FSETAM1090993303(self, FSETAM744778340);
  set_initial_stru = r;
  SATTR(set_initial_stru,use_map,FSETAM1208812133);
 }
 else {
  r = FSETAM1056526605(self);
 }
 if (ATTR(r,use_map)) {
  ret_val = FSETAM1748814338(self, r, e);
  return ret_val;
 }
 else {
  ret_val = FSETAM1287842946(self, r, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETAM_ROUT_DEF FSETAM124285384(FSETAM_ROUT_DEF self, INT n) {
 FSETAM_ROUT_DEF ret_val;
 FSETAM_ROUT_DEF r;
 FSETAM_ROUT_DEF L11;
 FSETAM_ROUT_DEF elt_nil_self;
 AM_ROUT_DEF ret_val1;
 ELT_NI602850927 elt_nil_self1;
 AM_ROUT_DEF ret_val2;
 AM_ROUT_DEF e = ((AM_ROUT_DEF) NULL);
 FSETAM_ROUT_DEF elt_nil_self2;
 AM_ROUT_DEF ret_val3;
 ELT_NI602850927 elt_nil_self3;
 AM_ROUT_DEF ret_val4;
 AM_ROUT_DEF e1 = ((AM_ROUT_DEF) NULL);
 INT L21 = INT_zero;
 FSETAM_ROUT_DEF L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 AM_ROUT_DEF L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct FSETAM_ROUT_DEF_struct)-sizeof(AM_ROUT_DEF))+(L4)*sizeof(AM_ROUT_DEF);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FSETAM_ROUT_DEF_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FSETAM_ROUT_DEF) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NI602850927) NULL);
 ret_val2 = ((AM_ROUT_DEF) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((AM_ROUT_DEF) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FSETAM_ROUT_DEF)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NI602850927) NULL);
    ret_val4 = ((AM_ROUT_DEF) NULL);
    ret_val3 = ret_val4;
    L10 = ret_val3;
    SARR((FSETAM_ROUT_DEF)L11,L21,(AM_ROUT_DEF)L10); 
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

FSETAM_ROUT_DEF FSETAM1281071096(FSETAM_ROUT_DEF self, AM_ROUT_DEF e) {
 FSETAM_ROUT_DEF ret_val;
 INT delete_elt_ind = INT_zero;
 INT hash_table_size = INT_zero;
 INT i = INT_zero;
 INT empty_loc = INT_zero;
 INT second_to_last_i = INT_zero;
 INT next = INT_zero;
 FSETAM_ROUT_DEF elt_eq_self;
 AM_ROUT_DEF elt_eq_e1;
 AM_ROUT_DEF elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ702804563 elt_eq_self1;
 AM_ROUT_DEF elt_eq_e11;
 AM_ROUT_DEF elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 BOOL L11_;
 BOOL L2;
 BOOL L31_;
 AM_ROUT_DEF L41_;
 INT L51_;
 BOOL L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 INT L10;
 BOOL L121_;
 INT L13;
 INT L141_;
 BOOL L151_;
 INT L161_;
 AM_ROUT_DEF L171_;
 AM_ROUT_DEF L18;
 INT L20;
 INT L211_;
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
  L41_=(AM_ROUT_DEF)ARR((FSETAM_ROUT_DEF)self,i); 
  elt_eq_e2 = L41_;
  elt_eq_self1 = ((ELT_EQ702804563) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  ret_val2 = (elt_eq_e11==elt_eq_e21);
  ret_val1 = ret_val2;
  if (ret_val1) {
   delete_elt_ind = i;
   goto after_loop;
  }
  L51_=INTPLUS(i,1); 
  i = L51_;
 }
 after_loop: ;
 L71_=(delete_elt_ind)<(0); 
 L8 = L71_;
 L91_=!(L8); 
 if (L91_) {
  L10 = ATTR(self,hsize);
  L121_=(delete_elt_ind)<(L10); 
  L6 = L121_;
 } else {
  L6 = FALSE;
 }
 if (L6) {
  empty_loc = delete_elt_ind;
  L13 = ATTR(self,hsize);
  L141_=INTMINUS(L13,2); 
  second_to_last_i = L141_;
  while (1) {
   L151_=(second_to_last_i)<(empty_loc); 
   if (L151_) {
    goto after_loop1;
   }
   L161_=INTPLUS(empty_loc,1); 
   next = L161_;
   L171_=(AM_ROUT_DEF)ARR((FSETAM_ROUT_DEF)self,next); 
   L18 = L171_;
   SARR((FSETAM_ROUT_DEF)self,empty_loc,(AM_ROUT_DEF)L18); 
   ;
   empty_loc = next;
  }
  after_loop1: ;
  L20 = ATTR(self,hsize);
  L211_=INTMINUS(L20,1); 
  SATTR(self,hsize,L211_);
 }
 ret_val = self;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETAM_ROUT_DEF FSETAM1353877611(FSETAM_ROUT_DEF self, AM_ROUT_DEF e) {
 FSETAM_ROUT_DEF ret_val;
 if ((self==((FSETAM_ROUT_DEF) NULL))) {
  ret_val = ((FSETAM_ROUT_DEF) NULL);
  return ret_val;
 }
 if (ATTR(self,use_map)) {
  ret_val = FSETAM1947131874(self, e);
  return ret_val;
 }
 else {
  ret_val = FSETAM1281071096(self, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETAM_ROUT_DEF FSETAM1457953461(FSETAM_ROUT_DEF self) {
 FSETAM_ROUT_DEF ret_val;
 FSETAM_ROUT_DEF r;
 FSETAM_ROUT_DEF L11;
 FSETAM_ROUT_DEF switch_structure;
 BOOL switch_structure1 = BOOL_zero;
 BOOL switch_structure2 = BOOL_zero;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 AM_ROUT_DEF L9;
 L21_=ASIZE((FSETAM_ROUT_DEF)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = FSETAM124285384(self, L81_);
 switch_structure = r;
 switch_structure1 = ATTR(self,use_map);
 switch_structure2 = TRUE;
 if (FSETAM1935294117) {
  SATTR(switch_structure,use_map,switch_structure2);
 }
 else {
  SATTR(switch_structure,use_map,switch_structure1);
 }
 {
  struct FSETAM1323524382_frame_struct other1_0;
FSETAM1323524382_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETAM1323524382(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETAM2044524049(r, L9);
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

FSETAM_ROUT_DEF FSETAM1582329164(FSETAM_ROUT_DEF self, FSETAM_ROUT_DEF r, AM_ROUT_DEF e) {
 FSETAM_ROUT_DEF ret_val;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 AM_ROUT_DEF te;
 AM_ROUT_DEF te1;
 FSETAM_ROUT_DEF elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETAM_ROUT_DEF is_elt_nil_self;
 AM_ROUT_DEF is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI602850927 is_elt_nil_self1;
 AM_ROUT_DEF is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 FSETAM_ROUT_DEF elt_eq_self;
 AM_ROUT_DEF elt_eq_e1;
 AM_ROUT_DEF elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ702804563 elt_eq_self1;
 AM_ROUT_DEF elt_eq_e11;
 AM_ROUT_DEF elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 FSETAM_ROUT_DEF is_elt_nil_self2;
 AM_ROUT_DEF is_elt_nil_e2;
 BOOL ret_val6 = BOOL_zero;
 ELT_NI602850927 is_elt_nil_self3;
 AM_ROUT_DEF is_elt_nil_e3;
 BOOL ret_val7 = BOOL_zero;
 FSETAM_ROUT_DEF elt_eq_self2;
 AM_ROUT_DEF elt_eq_e12;
 AM_ROUT_DEF elt_eq_e22;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQ702804563 elt_eq_self3;
 AM_ROUT_DEF elt_eq_e13;
 AM_ROUT_DEF elt_eq_e23;
 BOOL ret_val9 = BOOL_zero;
 INT L11_;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 AM_ROUT_DEF L61_;
 INT L81_;
 INT L91_;
 INT L10;
 BOOL L121_;
 AM_ROUT_DEF L131_;
 INT L151_;
 INT L17;
 INT L181_;
 L11_=ASIZE((FSETAM_ROUT_DEF)r); 
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
  L61_=(AM_ROUT_DEF)ARR((FSETAM_ROUT_DEF)r,h); 
  te = L61_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NI602850927) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1==((AM_ROUT_DEF) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQ702804563) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   ret_val5 = (elt_eq_e11==elt_eq_e21);
   ret_val4 = ret_val5;
   if (ret_val4) {
    SARR((FSETAM_ROUT_DEF)r,h,(AM_ROUT_DEF)e); 
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
   L131_=(AM_ROUT_DEF)ARR((FSETAM_ROUT_DEF)r,h); 
   te1 = L131_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NI602850927) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val7 = (is_elt_nil_e3==((AM_ROUT_DEF) NULL));
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQ702804563) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    ret_val9 = (elt_eq_e13==elt_eq_e23);
    ret_val8 = ret_val9;
    if (ret_val8) {
     SARR((FSETAM_ROUT_DEF)r,h,(AM_ROUT_DEF)e); 
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
 SARR((FSETAM_ROUT_DEF)r,h,(AM_ROUT_DEF)e); 
 ;
 L17 = ATTR(r,hsize);
 L181_=INTPLUS(L17,1); 
 SATTR(r,hsize,L181_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETAM_ROUT_DEF FSETAM1947131874(FSETAM_ROUT_DEF self, AM_ROUT_DEF e) {
 FSETAM_ROUT_DEF ret_val;
 INT h = INT_zero;
 AM_ROUT_DEF te;
 INT i = INT_zero;
 AM_ROUT_DEF te1;
 INT hsh = INT_zero;
 FSETAM_ROUT_DEF elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETAM_ROUT_DEF is_elt_nil_self;
 AM_ROUT_DEF is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI602850927 is_elt_nil_self1;
 AM_ROUT_DEF is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 FSETAM_ROUT_DEF elt_eq_self;
 AM_ROUT_DEF elt_eq_e1;
 AM_ROUT_DEF elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ702804563 elt_eq_self1;
 AM_ROUT_DEF elt_eq_e11;
 AM_ROUT_DEF elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 FSETAM_ROUT_DEF elt_nil_self;
 AM_ROUT_DEF ret_val6;
 ELT_NI602850927 elt_nil_self1;
 AM_ROUT_DEF ret_val7;
 AM_ROUT_DEF e1 = ((AM_ROUT_DEF) NULL);
 FSETAM_ROUT_DEF is_elt_nil_self2;
 AM_ROUT_DEF is_elt_nil_e2;
 BOOL ret_val8 = BOOL_zero;
 ELT_NI602850927 is_elt_nil_self3;
 AM_ROUT_DEF is_elt_nil_e3;
 BOOL ret_val9 = BOOL_zero;
 FSETAM_ROUT_DEF elt_hash_self1;
 OB elt_hash_e1;
 INT ret_val10 = INT_zero;
 FSETAM_ROUT_DEF elt_nil_self2;
 AM_ROUT_DEF ret_val11;
 ELT_NI602850927 elt_nil_self3;
 AM_ROUT_DEF ret_val12;
 AM_ROUT_DEF e2 = ((AM_ROUT_DEF) NULL);
 FSETAM_ROUT_DEF elt_nil_self4;
 AM_ROUT_DEF ret_val13;
 ELT_NI602850927 elt_nil_self5;
 AM_ROUT_DEF ret_val14;
 AM_ROUT_DEF e3 = ((AM_ROUT_DEF) NULL);
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 AM_ROUT_DEF L71_;
 INT L81_;
 INT L9;
 INT L101_;
 INT L11;
 BOOL L121_;
 INT L131_;
 AM_ROUT_DEF L14;
 INT L16;
 INT L171_;
 INT L181_;
 INT L19;
 INT L201_;
 INT L22;
 BOOL L231_;
 INT L241_;
 AM_ROUT_DEF L251_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 INT L30;
 INT L311_;
 BOOL L321_;
 BOOL L33;
 BOOL L341_;
 BOOL L35;
 BOOL L361_;
 BOOL L371_;
 BOOL L38;
 BOOL L391_;
 AM_ROUT_DEF L401_;
 AM_ROUT_DEF L42;
 AM_ROUT_DEF L44;
 BOOL L46;
 BOOL L471_;
 BOOL L48;
 BOOL L491_;
 BOOL L501_;
 AM_ROUT_DEF L511_;
 AM_ROUT_DEF L52;
 AM_ROUT_DEF L54;
 elt_hash_self = self;
 elt_hash_e = ((OB) e);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FSETAM_ROUT_DEF)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=(AM_ROUT_DEF)ARR((FSETAM_ROUT_DEF)self,h); 
  te = L71_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NI602850927) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1==((AM_ROUT_DEF) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   ret_val = self;
   return ret_val;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQ702804563) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   ret_val5 = (elt_eq_e11==elt_eq_e21);
   ret_val4 = ret_val5;
   if (ret_val4) {
    goto after_loop;
   }
  }
  L81_=ASIZE((FSETAM_ROUT_DEF)self); 
  L9 = L81_;
  L101_=INTMINUS(L9,2); 
  L11 = L101_;
  L121_=(h)==(L11); 
  if (L121_) {
   h = 0;
  }
  else {
   L131_=INTPLUS(h,1); 
   h = L131_;
  }
 }
 after_loop: ;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NI602850927) NULL);
 ret_val7 = ((AM_ROUT_DEF) NULL);
 ret_val6 = ret_val7;
 L14 = ret_val6;
 SARR((FSETAM_ROUT_DEF)self,h,(AM_ROUT_DEF)L14); 
 ;
 L16 = ATTR(self,hsize);
 L171_=INTMINUS(L16,1); 
 SATTR(self,hsize,L171_);
 i = h;
 while (1) {
  L181_=ASIZE((FSETAM_ROUT_DEF)self); 
  L19 = L181_;
  L201_=INTMINUS(L19,2); 
  L22 = L201_;
  L231_=(i)==(L22); 
  if (L231_) {
   i = 0;
  }
  else {
   L241_=INTPLUS(i,1); 
   i = L241_;
  }
  L251_=(AM_ROUT_DEF)ARR((FSETAM_ROUT_DEF)self,i); 
  te1 = L251_;
  is_elt_nil_self2 = self;
  is_elt_nil_e2 = te1;
  is_elt_nil_self3 = ((ELT_NI602850927) NULL);
  is_elt_nil_e3 = is_elt_nil_e2;
  ret_val9 = (is_elt_nil_e3==((AM_ROUT_DEF) NULL));
  ret_val8 = ret_val9;
  if (ret_val8) {
   goto after_loop1;
  }
  elt_hash_self1 = self;
  elt_hash_e1 = ((OB) te1);
  ret_val10 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e1);
  L26 = ret_val10;
  L271_=ASIZE((FSETAM_ROUT_DEF)self); 
  L28 = L271_;
  L291_=INTMINUS(L28,2); 
  L30 = L291_;
  L311_=L26&L30; 
  hsh = L311_;
  L321_=(i)<(hsh); 
  L33 = L321_;
  L341_=!(L33); 
  if (L341_) {
   L361_=(h)<(i); 
   if (L361_) {
    L371_=(h)<(hsh); 
    L38 = L371_;
    L391_=!(L38); 
    L35 = L391_;
   } else {
    L35 = FALSE;
   }
   if (L35) {
    L401_=(AM_ROUT_DEF)ARR((FSETAM_ROUT_DEF)self,i); 
    L42 = L401_;
    SARR((FSETAM_ROUT_DEF)self,h,(AM_ROUT_DEF)L42); 
    ;
    h = i;
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NI602850927) NULL);
    ret_val12 = ((AM_ROUT_DEF) NULL);
    ret_val11 = ret_val12;
    L44 = ret_val11;
    SARR((FSETAM_ROUT_DEF)self,i,(AM_ROUT_DEF)L44); 
    ;
   }
  }
  else {
   L471_=(h)<(hsh); 
   L48 = L471_;
   L491_=!(L48); 
   if (L491_) {
    L46 = TRUE;
   } else {
    L501_=(h)<(i); 
    L46 = L501_;
   }
   if (L46) {
    L511_=(AM_ROUT_DEF)ARR((FSETAM_ROUT_DEF)self,i); 
    L52 = L511_;
    SARR((FSETAM_ROUT_DEF)self,h,(AM_ROUT_DEF)L52); 
    ;
    h = i;
    elt_nil_self4 = self;
    elt_nil_self5 = ((ELT_NI602850927) NULL);
    ret_val14 = ((AM_ROUT_DEF) NULL);
    ret_val13 = ret_val14;
    L54 = ret_val13;
    SARR((FSETAM_ROUT_DEF)self,i,(AM_ROUT_DEF)L54); 
    ;
   }
  }
 }
 after_loop1: ;
 if (FSETAM631702418(self)) {
  ret_val = FSETAM814128556(self);
  return ret_val;
 }
 else {
  ret_val = self;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETAM_ROUT_DEF FSETAM2044524049(FSETAM_ROUT_DEF self, AM_ROUT_DEF e) {
 FSETAM_ROUT_DEF ret_val;
 FSETAM_ROUT_DEF r;
 FSETAM_ROUT_DEF set_initial_stru;
 r = self;
 if ((r==((FSETAM_ROUT_DEF) NULL))) {
  r = FSETAM124285384(self, FSETAM1929715922);
  set_initial_stru = r;
  SATTR(set_initial_stru,use_map,FSETAM501048668);
 }
 else {
  r = FSETAM2120233524(self);
 }
 if (ATTR(r,use_map)) {
  ret_val = FSETAM1582329164(self, r, e);
  return ret_val;
 }
 else {
  ret_val = FSETAM94979636(self, r, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETAM_ROUT_DEF FSETAM2120233524(FSETAM_ROUT_DEF self) {
 FSETAM_ROUT_DEF ret_val;
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
  L11_=ASIZE((FSETAM_ROUT_DEF)self); 
  L2 = L11_;
  L3 = ATTR(self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FSETAM295466615;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  if (L91_) {
   ret_val = FSETAM1457953461(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
 else {
  L10 = ATTR(self,hsize);
  L121_=ASIZE((FSETAM_ROUT_DEF)self); 
  L13 = L121_;
  L141_=(L10)<(L13); 
  L15 = L141_;
  L161_=!(L15); 
  if (L161_) {
   ret_val = FSETAM1457953461(self);
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

FSETAM_ROUT_DEF FSETAM814128556(FSETAM_ROUT_DEF self) {
 FSETAM_ROUT_DEF ret_val;
 FSETAM_ROUT_DEF r;
 FSETAM_ROUT_DEF L11;
 FSETAM_ROUT_DEF switch_structure;
 BOOL switch_structure1 = BOOL_zero;
 BOOL switch_structure2 = BOOL_zero;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 AM_ROUT_DEF L9;
 L21_=ASIZE((FSETAM_ROUT_DEF)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTDIV(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = FSETAM124285384(self, L81_);
 switch_structure = r;
 switch_structure1 = ATTR(self,use_map);
 switch_structure2 = TRUE;
 if (FSETAM1935294117) {
  SATTR(switch_structure,use_map,switch_structure2);
 }
 else {
  SATTR(switch_structure,use_map,switch_structure1);
 }
 {
  struct FSETAM1323524382_frame_struct other1_0;
FSETAM1323524382_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETAM1323524382(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETAM2044524049(r, L9);
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

FSETAM_ROUT_DEF FSETAM94979636(FSETAM_ROUT_DEF self, FSETAM_ROUT_DEF r, AM_ROUT_DEF e) {
 FSETAM_ROUT_DEF ret_val;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETAM_ROUT_DEF elt_eq_self;
 AM_ROUT_DEF elt_eq_e1;
 AM_ROUT_DEF elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ702804563 elt_eq_self1;
 AM_ROUT_DEF elt_eq_e11;
 AM_ROUT_DEF elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 BOOL L11_;
 AM_ROUT_DEF L21_;
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
  L21_=(AM_ROUT_DEF)ARR((FSETAM_ROUT_DEF)r,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQ702804563) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  ret_val2 = (elt_eq_e11==elt_eq_e21);
  ret_val1 = ret_val2;
  if (ret_val1) {
   SARR((FSETAM_ROUT_DEF)r,i,(AM_ROUT_DEF)e); 
   ;
   ret_val = r;
   return ret_val;
  }
  L41_=INTPLUS(i,1); 
  i = L41_;
 }
 after_loop: ;
 L5 = ATTR(r,hsize);
 SARR((FSETAM_ROUT_DEF)r,L5,(AM_ROUT_DEF)e); 
 ;
 L7 = ATTR(r,hsize);
 L81_=INTPLUS(L7,1); 
 SATTR(r,hsize,L81_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETAS_FEAT_MOD FSETAS1457709924(FSETAS_FEAT_MOD self, INT n) {
 FSETAS_FEAT_MOD ret_val;
 FSETAS_FEAT_MOD r;
 FSETAS_FEAT_MOD L11;
 FSETAS_FEAT_MOD elt_nil_self;
 AS_FEAT_MOD ret_val1;
 ELT_NI465844240 elt_nil_self1;
 AS_FEAT_MOD ret_val2;
 AS_FEAT_MOD e = ((AS_FEAT_MOD) NULL);
 FSETAS_FEAT_MOD elt_nil_self2;
 AS_FEAT_MOD ret_val3;
 ELT_NI465844240 elt_nil_self3;
 AS_FEAT_MOD ret_val4;
 AS_FEAT_MOD e1 = ((AS_FEAT_MOD) NULL);
 INT L21 = INT_zero;
 FSETAS_FEAT_MOD L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 AS_FEAT_MOD L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct FSETAS_FEAT_MOD_struct)-sizeof(AS_FEAT_MOD))+(L4)*sizeof(AS_FEAT_MOD);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FSETAS_FEAT_MOD_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FSETAS_FEAT_MOD) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NI465844240) NULL);
 ret_val2 = ((AS_FEAT_MOD) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((AS_FEAT_MOD) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FSETAS_FEAT_MOD)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NI465844240) NULL);
    ret_val4 = ((AS_FEAT_MOD) NULL);
    ret_val3 = ret_val4;
    L10 = ret_val3;
    SARR((FSETAS_FEAT_MOD)L11,L21,(AS_FEAT_MOD)L10); 
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

FSETAS_FEAT_MOD FSETAS1731630631(FSETAS_FEAT_MOD self) {
 FSETAS_FEAT_MOD ret_val;
 FSETAS_FEAT_MOD r;
 FSETAS_FEAT_MOD L11;
 FSETAS_FEAT_MOD switch_structure;
 BOOL switch_structure1 = BOOL_zero;
 BOOL switch_structure2 = BOOL_zero;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 AS_FEAT_MOD L9;
 L21_=ASIZE((FSETAS_FEAT_MOD)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = FSETAS1457709924(self, L81_);
 switch_structure = r;
 switch_structure1 = ATTR(self,use_map);
 switch_structure2 = TRUE;
 if (FSETAS46095922) {
  SATTR(switch_structure,use_map,switch_structure2);
 }
 else {
  SATTR(switch_structure,use_map,switch_structure1);
 }
 {
  struct FSETAS1592299594_frame_struct other1_0;
FSETAS1592299594_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETAS1592299594(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETAS559779810(r, L9);
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

FSETAS_FEAT_MOD FSETAS1911338080(FSETAS_FEAT_MOD self, FSETAS_FEAT_MOD r, AS_FEAT_MOD e) {
 FSETAS_FEAT_MOD ret_val;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETAS_FEAT_MOD elt_eq_self;
 AS_FEAT_MOD elt_eq_e1;
 AS_FEAT_MOD elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ365890604 elt_eq_self1;
 AS_FEAT_MOD elt_eq_e11;
 AS_FEAT_MOD elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 BOOL L11_;
 AS_FEAT_MOD L21_;
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
  L21_=(AS_FEAT_MOD)ARR((FSETAS_FEAT_MOD)r,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQ365890604) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  ret_val2 = (elt_eq_e11==elt_eq_e21);
  ret_val1 = ret_val2;
  if (ret_val1) {
   SARR((FSETAS_FEAT_MOD)r,i,(AS_FEAT_MOD)e); 
   ;
   ret_val = r;
   return ret_val;
  }
  L41_=INTPLUS(i,1); 
  i = L41_;
 }
 after_loop: ;
 L5 = ATTR(r,hsize);
 SARR((FSETAS_FEAT_MOD)r,L5,(AS_FEAT_MOD)e); 
 ;
 L7 = ATTR(r,hsize);
 L81_=INTPLUS(L7,1); 
 SATTR(r,hsize,L81_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETAS_FEAT_MOD FSETAS559779810(FSETAS_FEAT_MOD self, AS_FEAT_MOD e) {
 FSETAS_FEAT_MOD ret_val;
 FSETAS_FEAT_MOD r;
 FSETAS_FEAT_MOD set_initial_stru;
 r = self;
 if ((r==((FSETAS_FEAT_MOD) NULL))) {
  r = FSETAS1457709924(self, FSETAS1287498417);
  set_initial_stru = r;
  SATTR(set_initial_stru,use_map,FSETAS1812528589);
 }
 else {
  r = FSETAS841309232(self);
 }
 if (ATTR(r,use_map)) {
  ret_val = FSETAS706320416(self, r, e);
  return ret_val;
 }
 else {
  ret_val = FSETAS1911338080(self, r, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETAS_FEAT_MOD FSETAS706320416(FSETAS_FEAT_MOD self, FSETAS_FEAT_MOD r, AS_FEAT_MOD e) {
 FSETAS_FEAT_MOD ret_val;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 AS_FEAT_MOD te;
 AS_FEAT_MOD te1;
 FSETAS_FEAT_MOD elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETAS_FEAT_MOD is_elt_nil_self;
 AS_FEAT_MOD is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI465844240 is_elt_nil_self1;
 AS_FEAT_MOD is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 FSETAS_FEAT_MOD elt_eq_self;
 AS_FEAT_MOD elt_eq_e1;
 AS_FEAT_MOD elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ365890604 elt_eq_self1;
 AS_FEAT_MOD elt_eq_e11;
 AS_FEAT_MOD elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 FSETAS_FEAT_MOD is_elt_nil_self2;
 AS_FEAT_MOD is_elt_nil_e2;
 BOOL ret_val6 = BOOL_zero;
 ELT_NI465844240 is_elt_nil_self3;
 AS_FEAT_MOD is_elt_nil_e3;
 BOOL ret_val7 = BOOL_zero;
 FSETAS_FEAT_MOD elt_eq_self2;
 AS_FEAT_MOD elt_eq_e12;
 AS_FEAT_MOD elt_eq_e22;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQ365890604 elt_eq_self3;
 AS_FEAT_MOD elt_eq_e13;
 AS_FEAT_MOD elt_eq_e23;
 BOOL ret_val9 = BOOL_zero;
 INT L11_;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 AS_FEAT_MOD L61_;
 INT L81_;
 INT L91_;
 INT L10;
 BOOL L121_;
 AS_FEAT_MOD L131_;
 INT L151_;
 INT L17;
 INT L181_;
 L11_=ASIZE((FSETAS_FEAT_MOD)r); 
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
  L61_=(AS_FEAT_MOD)ARR((FSETAS_FEAT_MOD)r,h); 
  te = L61_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NI465844240) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1==((AS_FEAT_MOD) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQ365890604) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   ret_val5 = (elt_eq_e11==elt_eq_e21);
   ret_val4 = ret_val5;
   if (ret_val4) {
    SARR((FSETAS_FEAT_MOD)r,h,(AS_FEAT_MOD)e); 
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
   L131_=(AS_FEAT_MOD)ARR((FSETAS_FEAT_MOD)r,h); 
   te1 = L131_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NI465844240) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val7 = (is_elt_nil_e3==((AS_FEAT_MOD) NULL));
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQ365890604) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    ret_val9 = (elt_eq_e13==elt_eq_e23);
    ret_val8 = ret_val9;
    if (ret_val8) {
     SARR((FSETAS_FEAT_MOD)r,h,(AS_FEAT_MOD)e); 
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
 SARR((FSETAS_FEAT_MOD)r,h,(AS_FEAT_MOD)e); 
 ;
 L17 = ATTR(r,hsize);
 L181_=INTPLUS(L17,1); 
 SATTR(r,hsize,L181_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETAS_FEAT_MOD FSETAS841309232(FSETAS_FEAT_MOD self) {
 FSETAS_FEAT_MOD ret_val;
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
  L11_=ASIZE((FSETAS_FEAT_MOD)self); 
  L2 = L11_;
  L3 = ATTR(self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FSETAS1969137460;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  if (L91_) {
   ret_val = FSETAS1731630631(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
 else {
  L10 = ATTR(self,hsize);
  L121_=ASIZE((FSETAS_FEAT_MOD)self); 
  L13 = L121_;
  L141_=(L10)<(L13); 
  L15 = L141_;
  L161_=!(L15); 
  if (L161_) {
   ret_val = FSETAS1731630631(self);
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

FSETBO1147931784 FSETBO1130450260(FSETBO1147931784 self, FSETBO1147931784 r, BOUND_809390774 e) {
 FSETBO1147931784 ret_val;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETBO1147931784 elt_eq_self;
 BOUND_809390774 elt_eq_e1;
 BOUND_809390774 elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ1334130421 elt_eq_self1;
 BOUND_809390774 elt_eq_e11;
 BOUND_809390774 elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 BOOL L11_;
 BOUND_809390774 L21_;
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
  L21_=(BOUND_809390774)ARR((FSETBO1147931784)r,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQ1334130421) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  ret_val2 = (elt_eq_e11==elt_eq_e21);
  ret_val1 = ret_val2;
  if (ret_val1) {
   SARR((FSETBO1147931784)r,i,(BOUND_809390774)e); 
   ;
   ret_val = r;
   return ret_val;
  }
  L41_=INTPLUS(i,1); 
  i = L41_;
 }
 after_loop: ;
 L5 = ATTR(r,hsize);
 SARR((FSETBO1147931784)r,L5,(BOUND_809390774)e); 
 ;
 L7 = ATTR(r,hsize);
 L81_=INTPLUS(L7,1); 
 SATTR(r,hsize,L81_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETBO1147931784 FSETBO113908103(FSETBO1147931784 self, BOUND_809390774 e) {
 FSETBO1147931784 ret_val;
 FSETBO1147931784 r;
 FSETBO1147931784 set_initial_stru;
 r = self;
 if ((r==((FSETBO1147931784) NULL))) {
  r = FSETBO273868630(self, FSETBO157482060);
  set_initial_stru = r;
  SATTR(set_initial_stru,use_map,FSETBO1572491060);
 }
 else {
  r = FSETBO1624907678(self);
 }
 if (ATTR(r,use_map)) {
  ret_val = FSETBO503820844(self, r, e);
  return ret_val;
 }
 else {
  ret_val = FSETBO1130450260(self, r, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETBO1147931784 FSETBO1624907678(FSETBO1147931784 self) {
 FSETBO1147931784 ret_val;
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
  L11_=ASIZE((FSETBO1147931784)self); 
  L2 = L11_;
  L3 = ATTR(self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FSETBO1040487207;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  if (L91_) {
   ret_val = FSETBO1674616923(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
 else {
  L10 = ATTR(self,hsize);
  L121_=ASIZE((FSETBO1147931784)self); 
  L13 = L121_;
  L141_=(L10)<(L13); 
  L15 = L141_;
  L161_=!(L15); 
  if (L161_) {
   ret_val = FSETBO1674616923(self);
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

FSETBO1147931784 FSETBO1674616923(FSETBO1147931784 self) {
 FSETBO1147931784 ret_val;
 FSETBO1147931784 r;
 FSETBO1147931784 L11;
 FSETBO1147931784 switch_structure;
 BOOL switch_structure1 = BOOL_zero;
 BOOL switch_structure2 = BOOL_zero;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 BOUND_809390774 L9;
 L21_=ASIZE((FSETBO1147931784)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = FSETBO273868630(self, L81_);
 switch_structure = r;
 switch_structure1 = ATTR(self,use_map);
 switch_structure2 = TRUE;
 if (FSETBO286133451) {
  SATTR(switch_structure,use_map,switch_structure2);
 }
 else {
  SATTR(switch_structure,use_map,switch_structure1);
 }
 {
  struct FSETBO10998148_frame_struct other1_0;
FSETBO10998148_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETBO10998148(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETBO113908103(r, L9);
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

FSETBO1147931784 FSETBO273868630(FSETBO1147931784 self, INT n) {
 FSETBO1147931784 ret_val;
 FSETBO1147931784 r;
 FSETBO1147931784 L11;
 FSETBO1147931784 elt_nil_self;
 BOUND_809390774 ret_val1;
 ELT_NI1119971763 elt_nil_self1;
 BOUND_809390774 ret_val2;
 BOUND_809390774 e = ((BOUND_809390774) NULL);
 FSETBO1147931784 elt_nil_self2;
 BOUND_809390774 ret_val3;
 ELT_NI1119971763 elt_nil_self3;
 BOUND_809390774 ret_val4;
 BOUND_809390774 e1 = ((BOUND_809390774) NULL);
 INT L21 = INT_zero;
 FSETBO1147931784 L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 BOUND_809390774 L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct FSETBO1147931784_struct)-sizeof(BOUND_809390774))+(L4)*sizeof(BOUND_809390774);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FSETBO1147931784_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FSETBO1147931784) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NI1119971763) NULL);
 ret_val2 = ((BOUND_809390774) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((BOUND_809390774) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FSETBO1147931784)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NI1119971763) NULL);
    ret_val4 = ((BOUND_809390774) NULL);
    ret_val3 = ret_val4;
    L10 = ret_val3;
    SARR((FSETBO1147931784)L11,L21,(BOUND_809390774)L10); 
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

FSETBO1147931784 FSETBO503820844(FSETBO1147931784 self, FSETBO1147931784 r, BOUND_809390774 e) {
 FSETBO1147931784 ret_val;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 BOUND_809390774 te;
 BOUND_809390774 te1;
 FSETBO1147931784 elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETBO1147931784 is_elt_nil_self;
 BOUND_809390774 is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI1119971763 is_elt_nil_self1;
 BOUND_809390774 is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 FSETBO1147931784 elt_eq_self;
 BOUND_809390774 elt_eq_e1;
 BOUND_809390774 elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ1334130421 elt_eq_self1;
 BOUND_809390774 elt_eq_e11;
 BOUND_809390774 elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 FSETBO1147931784 is_elt_nil_self2;
 BOUND_809390774 is_elt_nil_e2;
 BOOL ret_val6 = BOOL_zero;
 ELT_NI1119971763 is_elt_nil_self3;
 BOUND_809390774 is_elt_nil_e3;
 BOOL ret_val7 = BOOL_zero;
 FSETBO1147931784 elt_eq_self2;
 BOUND_809390774 elt_eq_e12;
 BOUND_809390774 elt_eq_e22;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQ1334130421 elt_eq_self3;
 BOUND_809390774 elt_eq_e13;
 BOUND_809390774 elt_eq_e23;
 BOOL ret_val9 = BOOL_zero;
 INT L11_;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 BOUND_809390774 L61_;
 INT L81_;
 INT L91_;
 INT L10;
 BOOL L121_;
 BOUND_809390774 L131_;
 INT L151_;
 INT L17;
 INT L181_;
 L11_=ASIZE((FSETBO1147931784)r); 
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
  L61_=(BOUND_809390774)ARR((FSETBO1147931784)r,h); 
  te = L61_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NI1119971763) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1==((BOUND_809390774) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQ1334130421) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   ret_val5 = (elt_eq_e11==elt_eq_e21);
   ret_val4 = ret_val5;
   if (ret_val4) {
    SARR((FSETBO1147931784)r,h,(BOUND_809390774)e); 
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
   L131_=(BOUND_809390774)ARR((FSETBO1147931784)r,h); 
   te1 = L131_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NI1119971763) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val7 = (is_elt_nil_e3==((BOUND_809390774) NULL));
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQ1334130421) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    ret_val9 = (elt_eq_e13==elt_eq_e23);
    ret_val8 = ret_val9;
    if (ret_val8) {
     SARR((FSETBO1147931784)r,h,(BOUND_809390774)e); 
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
 SARR((FSETBO1147931784)r,h,(BOUND_809390774)e); 
 ;
 L17 = ATTR(r,hsize);
 L181_=INTPLUS(L17,1); 
 SATTR(r,hsize,L181_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETBO1548397318 FSETBO1059930149(FSETBO1548397318 self) {
 FSETBO1548397318 ret_val;
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
  L11_=ASIZE((FSETBO1548397318)self); 
  L2 = L11_;
  L3 = ATTR(self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FSETBO717550434;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  if (L91_) {
   ret_val = FSETBO264880646(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
 else {
  L10 = ATTR(self,hsize);
  L121_=ASIZE((FSETBO1548397318)self); 
  L13 = L121_;
  L141_=(L10)<(L13); 
  L15 = L141_;
  L161_=!(L15); 
  if (L161_) {
   ret_val = FSETBO264880646(self);
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

FSETBO1548397318 FSETBO1686201418(FSETBO1548397318 self, FSETBO1548397318 r, BOUND_2140373124 e) {
 FSETBO1548397318 ret_val;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 BOUND_2140373124 te;
 BOUND_2140373124 te1;
 FSETBO1548397318 elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETBO1548397318 is_elt_nil_self;
 BOUND_2140373124 is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI338362486 is_elt_nil_self1;
 BOUND_2140373124 is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 FSETBO1548397318 elt_eq_self;
 BOUND_2140373124 elt_eq_e1;
 BOUND_2140373124 elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ1712035353 elt_eq_self1;
 BOUND_2140373124 elt_eq_e11;
 BOUND_2140373124 elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 FSETBO1548397318 is_elt_nil_self2;
 BOUND_2140373124 is_elt_nil_e2;
 BOOL ret_val6 = BOOL_zero;
 ELT_NI338362486 is_elt_nil_self3;
 BOUND_2140373124 is_elt_nil_e3;
 BOOL ret_val7 = BOOL_zero;
 FSETBO1548397318 elt_eq_self2;
 BOUND_2140373124 elt_eq_e12;
 BOUND_2140373124 elt_eq_e22;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQ1712035353 elt_eq_self3;
 BOUND_2140373124 elt_eq_e13;
 BOUND_2140373124 elt_eq_e23;
 BOOL ret_val9 = BOOL_zero;
 INT L11_;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 BOUND_2140373124 L61_;
 INT L81_;
 INT L91_;
 INT L10;
 BOOL L121_;
 BOUND_2140373124 L131_;
 INT L151_;
 INT L17;
 INT L181_;
 L11_=ASIZE((FSETBO1548397318)r); 
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
  L61_=(BOUND_2140373124)ARR((FSETBO1548397318)r,h); 
  te = L61_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NI338362486) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1==((BOUND_2140373124) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQ1712035353) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   ret_val5 = (elt_eq_e11==elt_eq_e21);
   ret_val4 = ret_val5;
   if (ret_val4) {
    SARR((FSETBO1548397318)r,h,(BOUND_2140373124)e); 
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
   L131_=(BOUND_2140373124)ARR((FSETBO1548397318)r,h); 
   te1 = L131_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NI338362486) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val7 = (is_elt_nil_e3==((BOUND_2140373124) NULL));
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQ1712035353) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    ret_val9 = (elt_eq_e13==elt_eq_e23);
    ret_val8 = ret_val9;
    if (ret_val8) {
     SARR((FSETBO1548397318)r,h,(BOUND_2140373124)e); 
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
 SARR((FSETBO1548397318)r,h,(BOUND_2140373124)e); 
 ;
 L17 = ATTR(r,hsize);
 L181_=INTPLUS(L17,1); 
 SATTR(r,hsize,L181_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETBO1548397318 FSETBO1907478089(FSETBO1548397318 self, INT n) {
 FSETBO1548397318 ret_val;
 FSETBO1548397318 r;
 FSETBO1548397318 L11;
 FSETBO1548397318 elt_nil_self;
 BOUND_2140373124 ret_val1;
 ELT_NI338362486 elt_nil_self1;
 BOUND_2140373124 ret_val2;
 BOUND_2140373124 e = ((BOUND_2140373124) NULL);
 FSETBO1548397318 elt_nil_self2;
 BOUND_2140373124 ret_val3;
 ELT_NI338362486 elt_nil_self3;
 BOUND_2140373124 ret_val4;
 BOUND_2140373124 e1 = ((BOUND_2140373124) NULL);
 INT L21 = INT_zero;
 FSETBO1548397318 L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 BOUND_2140373124 L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct FSETBO1548397318_struct)-sizeof(BOUND_2140373124))+(L4)*sizeof(BOUND_2140373124);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FSETBO1548397318_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FSETBO1548397318) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NI338362486) NULL);
 ret_val2 = ((BOUND_2140373124) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((BOUND_2140373124) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FSETBO1548397318)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NI338362486) NULL);
    ret_val4 = ((BOUND_2140373124) NULL);
    ret_val3 = ret_val4;
    L10 = ret_val3;
    SARR((FSETBO1548397318)L11,L21,(BOUND_2140373124)L10); 
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

FSETBO1548397318 FSETBO2112177303(FSETBO1548397318 self, BOUND_2140373124 e) {
 FSETBO1548397318 ret_val;
 FSETBO1548397318 r;
 FSETBO1548397318 set_initial_stru;
 r = self;
 if ((r==((FSETBO1548397318) NULL))) {
  r = FSETBO1907478089(self, FSETBO1172349319);
  set_initial_stru = r;
  SATTR(set_initial_stru,use_map,FSETBO216310762);
 }
 else {
  r = FSETBO1059930149(self);
 }
 if (ATTR(r,use_map)) {
  ret_val = FSETBO1686201418(self, r, e);
  return ret_val;
 }
 else {
  ret_val = FSETBO888993482(self, r, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETBO1548397318 FSETBO264880646(FSETBO1548397318 self) {
 FSETBO1548397318 ret_val;
 FSETBO1548397318 r;
 FSETBO1548397318 L11;
 FSETBO1548397318 switch_structure;
 BOOL switch_structure1 = BOOL_zero;
 BOOL switch_structure2 = BOOL_zero;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 BOUND_2140373124 L9;
 L21_=ASIZE((FSETBO1548397318)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = FSETBO1907478089(self, L81_);
 switch_structure = r;
 switch_structure1 = ATTR(self,use_map);
 switch_structure2 = TRUE;
 if (FSETBO2074935273) {
  SATTR(switch_structure,use_map,switch_structure2);
 }
 else {
  SATTR(switch_structure,use_map,switch_structure1);
 }
 {
  struct FSETBO677183779_frame_struct other1_0;
FSETBO677183779_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETBO677183779(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETBO2112177303(r, L9);
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

FSETBO1548397318 FSETBO888993482(FSETBO1548397318 self, FSETBO1548397318 r, BOUND_2140373124 e) {
 FSETBO1548397318 ret_val;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETBO1548397318 elt_eq_self;
 BOUND_2140373124 elt_eq_e1;
 BOUND_2140373124 elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ1712035353 elt_eq_self1;
 BOUND_2140373124 elt_eq_e11;
 BOUND_2140373124 elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 BOOL L11_;
 BOUND_2140373124 L21_;
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
  L21_=(BOUND_2140373124)ARR((FSETBO1548397318)r,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQ1712035353) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  ret_val2 = (elt_eq_e11==elt_eq_e21);
  ret_val1 = ret_val2;
  if (ret_val1) {
   SARR((FSETBO1548397318)r,i,(BOUND_2140373124)e); 
   ;
   ret_val = r;
   return ret_val;
  }
  L41_=INTPLUS(i,1); 
  i = L41_;
 }
 after_loop: ;
 L5 = ATTR(r,hsize);
 SARR((FSETBO1548397318)r,L5,(BOUND_2140373124)e); 
 ;
 L7 = ATTR(r,hsize);
 L81_=INTPLUS(L7,1); 
 SATTR(r,hsize,L81_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETBU1446477505 FSETBU1738179145(FSETBU1446477505 self) {
 FSETBU1446477505 ret_val;
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
  L11_=ASIZE((FSETBU1446477505)self); 
  L2 = L11_;
  L3 = ATTR(self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FSETBU1565274135;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  if (L91_) {
   ret_val = FSETBU923961098(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
 else {
  L10 = ATTR(self,hsize);
  L121_=ASIZE((FSETBU1446477505)self); 
  L13 = L121_;
  L141_=(L10)<(L13); 
  L15 = L141_;
  L161_=!(L15); 
  if (L161_) {
   ret_val = FSETBU923961098(self);
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

FSETBU1446477505 FSETBU2034709890(FSETBU1446477505 self, BUILTIN_LAYOUT e) {
 FSETBU1446477505 ret_val;
 FSETBU1446477505 r;
 FSETBU1446477505 set_initial_stru;
 r = self;
 if ((r==((FSETBU1446477505) NULL))) {
  r = FSETBU409340763(self, FSETBU864102134);
  set_initial_stru = r;
  SATTR(set_initial_stru,use_map,FSETBU1017592727);
 }
 else {
  r = FSETBU1738179145(self);
 }
 if (ATTR(r,use_map)) {
  ret_val = FSETBU436837902(self, r, e);
  return ret_val;
 }
 else {
  ret_val = FSETBU897809294(self, r, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETBU1446477505 FSETBU409340763(FSETBU1446477505 self, INT n) {
 FSETBU1446477505 ret_val;
 FSETBU1446477505 r;
 FSETBU1446477505 L11;
 FSETBU1446477505 elt_nil_self;
 BUILTIN_LAYOUT ret_val1;
 ELT_NI1281796289 elt_nil_self1;
 BUILTIN_LAYOUT ret_val2;
 BUILTIN_LAYOUT e = ((BUILTIN_LAYOUT) NULL);
 FSETBU1446477505 elt_nil_self2;
 BUILTIN_LAYOUT ret_val3;
 ELT_NI1281796289 elt_nil_self3;
 BUILTIN_LAYOUT ret_val4;
 BUILTIN_LAYOUT e1 = ((BUILTIN_LAYOUT) NULL);
 INT L21 = INT_zero;
 FSETBU1446477505 L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 BUILTIN_LAYOUT L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct FSETBU1446477505_struct)-sizeof(BUILTIN_LAYOUT))+(L4)*sizeof(BUILTIN_LAYOUT);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FSETBU1446477505_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FSETBU1446477505) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NI1281796289) NULL);
 ret_val2 = ((BUILTIN_LAYOUT) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((BUILTIN_LAYOUT) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FSETBU1446477505)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NI1281796289) NULL);
    ret_val4 = ((BUILTIN_LAYOUT) NULL);
    ret_val3 = ret_val4;
    L10 = ret_val3;
    SARR((FSETBU1446477505)L11,L21,(BUILTIN_LAYOUT)L10); 
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

FSETBU1446477505 FSETBU436837902(FSETBU1446477505 self, FSETBU1446477505 r, BUILTIN_LAYOUT e) {
 FSETBU1446477505 ret_val;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 BUILTIN_LAYOUT te;
 BUILTIN_LAYOUT te1;
 FSETBU1446477505 elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETBU1446477505 is_elt_nil_self;
 BUILTIN_LAYOUT is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI1281796289 is_elt_nil_self1;
 BUILTIN_LAYOUT is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 FSETBU1446477505 elt_eq_self;
 BUILTIN_LAYOUT elt_eq_e1;
 BUILTIN_LAYOUT elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ1400400210 elt_eq_self1;
 BUILTIN_LAYOUT elt_eq_e11;
 BUILTIN_LAYOUT elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 FSETBU1446477505 is_elt_nil_self2;
 BUILTIN_LAYOUT is_elt_nil_e2;
 BOOL ret_val6 = BOOL_zero;
 ELT_NI1281796289 is_elt_nil_self3;
 BUILTIN_LAYOUT is_elt_nil_e3;
 BOOL ret_val7 = BOOL_zero;
 FSETBU1446477505 elt_eq_self2;
 BUILTIN_LAYOUT elt_eq_e12;
 BUILTIN_LAYOUT elt_eq_e22;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQ1400400210 elt_eq_self3;
 BUILTIN_LAYOUT elt_eq_e13;
 BUILTIN_LAYOUT elt_eq_e23;
 BOOL ret_val9 = BOOL_zero;
 INT L11_;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 BUILTIN_LAYOUT L61_;
 INT L81_;
 INT L91_;
 INT L10;
 BOOL L121_;
 BUILTIN_LAYOUT L131_;
 INT L151_;
 INT L17;
 INT L181_;
 L11_=ASIZE((FSETBU1446477505)r); 
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
  L61_=(BUILTIN_LAYOUT)ARR((FSETBU1446477505)r,h); 
  te = L61_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NI1281796289) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1==((BUILTIN_LAYOUT) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQ1400400210) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   ret_val5 = (elt_eq_e11==elt_eq_e21);
   ret_val4 = ret_val5;
   if (ret_val4) {
    SARR((FSETBU1446477505)r,h,(BUILTIN_LAYOUT)e); 
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
   L131_=(BUILTIN_LAYOUT)ARR((FSETBU1446477505)r,h); 
   te1 = L131_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NI1281796289) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val7 = (is_elt_nil_e3==((BUILTIN_LAYOUT) NULL));
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQ1400400210) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    ret_val9 = (elt_eq_e13==elt_eq_e23);
    ret_val8 = ret_val9;
    if (ret_val8) {
     SARR((FSETBU1446477505)r,h,(BUILTIN_LAYOUT)e); 
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
 SARR((FSETBU1446477505)r,h,(BUILTIN_LAYOUT)e); 
 ;
 L17 = ATTR(r,hsize);
 L181_=INTPLUS(L17,1); 
 SATTR(r,hsize,L181_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETBU1446477505 FSETBU897809294(FSETBU1446477505 self, FSETBU1446477505 r, BUILTIN_LAYOUT e) {
 FSETBU1446477505 ret_val;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETBU1446477505 elt_eq_self;
 BUILTIN_LAYOUT elt_eq_e1;
 BUILTIN_LAYOUT elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ1400400210 elt_eq_self1;
 BUILTIN_LAYOUT elt_eq_e11;
 BUILTIN_LAYOUT elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 BOOL L11_;
 BUILTIN_LAYOUT L21_;
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
  L21_=(BUILTIN_LAYOUT)ARR((FSETBU1446477505)r,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQ1400400210) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  ret_val2 = (elt_eq_e11==elt_eq_e21);
  ret_val1 = ret_val2;
  if (ret_val1) {
   SARR((FSETBU1446477505)r,i,(BUILTIN_LAYOUT)e); 
   ;
   ret_val = r;
   return ret_val;
  }
  L41_=INTPLUS(i,1); 
  i = L41_;
 }
 after_loop: ;
 L5 = ATTR(r,hsize);
 SARR((FSETBU1446477505)r,L5,(BUILTIN_LAYOUT)e); 
 ;
 L7 = ATTR(r,hsize);
 L81_=INTPLUS(L7,1); 
 SATTR(r,hsize,L81_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETBU1446477505 FSETBU923961098(FSETBU1446477505 self) {
 FSETBU1446477505 ret_val;
 FSETBU1446477505 r;
 FSETBU1446477505 L11;
 FSETBU1446477505 switch_structure;
 BOOL switch_structure1 = BOOL_zero;
 BOOL switch_structure2 = BOOL_zero;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 BUILTIN_LAYOUT L9;
 L21_=ASIZE((FSETBU1446477505)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = FSETBU409340763(self, L81_);
 switch_structure = r;
 switch_structure1 = ATTR(self,use_map);
 switch_structure2 = TRUE;
 if (FSETBU841031784) {
  SATTR(switch_structure,use_map,switch_structure2);
 }
 else {
  SATTR(switch_structure,use_map,switch_structure1);
 }
 {
  struct FSETBU202043707_frame_struct other1_0;
FSETBU202043707_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETBU202043707(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETBU2034709890(r, L9);
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

FSETCLASS_LAYOUT FSETCL11745129(FSETCLASS_LAYOUT self, INT n) {
 FSETCLASS_LAYOUT ret_val;
 FSETCLASS_LAYOUT r;
 FSETCLASS_LAYOUT L11;
 FSETCLASS_LAYOUT elt_nil_self;
 CLASS_LAYOUT ret_val1;
 ELT_NI737899749 elt_nil_self1;
 CLASS_LAYOUT ret_val2;
 CLASS_LAYOUT e = ((CLASS_LAYOUT) NULL);
 FSETCLASS_LAYOUT elt_nil_self2;
 CLASS_LAYOUT ret_val3;
 ELT_NI737899749 elt_nil_self3;
 CLASS_LAYOUT ret_val4;
 CLASS_LAYOUT e1 = ((CLASS_LAYOUT) NULL);
 INT L21 = INT_zero;
 FSETCLASS_LAYOUT L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 CLASS_LAYOUT L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct FSETCLASS_LAYOUT_struct)-sizeof(CLASS_LAYOUT))+(L4)*sizeof(CLASS_LAYOUT);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FSETCLASS_LAYOUT_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FSETCLASS_LAYOUT) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NI737899749) NULL);
 ret_val2 = ((CLASS_LAYOUT) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((CLASS_LAYOUT) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FSETCLASS_LAYOUT)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NI737899749) NULL);
    ret_val4 = ((CLASS_LAYOUT) NULL);
    ret_val3 = ret_val4;
    L10 = ret_val3;
    SARR((FSETCLASS_LAYOUT)L11,L21,(CLASS_LAYOUT)L10); 
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

FSETCLASS_LAYOUT FSETCL120396958(FSETCLASS_LAYOUT self, FSETCLASS_LAYOUT r, CLASS_LAYOUT e) {
 FSETCLASS_LAYOUT ret_val;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETCLASS_LAYOUT elt_eq_self;
 CLASS_LAYOUT elt_eq_e1;
 CLASS_LAYOUT elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ1657948444 elt_eq_self1;
 CLASS_LAYOUT elt_eq_e11;
 CLASS_LAYOUT elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 BOOL L11_;
 CLASS_LAYOUT L21_;
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
  L21_=(CLASS_LAYOUT)ARR((FSETCLASS_LAYOUT)r,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQ1657948444) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  ret_val2 = (elt_eq_e11==elt_eq_e21);
  ret_val1 = ret_val2;
  if (ret_val1) {
   SARR((FSETCLASS_LAYOUT)r,i,(CLASS_LAYOUT)e); 
   ;
   ret_val = r;
   return ret_val;
  }
  L41_=INTPLUS(i,1); 
  i = L41_;
 }
 after_loop: ;
 L5 = ATTR(r,hsize);
 SARR((FSETCLASS_LAYOUT)r,L5,(CLASS_LAYOUT)e); 
 ;
 L7 = ATTR(r,hsize);
 L81_=INTPLUS(L7,1); 
 SATTR(r,hsize,L81_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETCLASS_LAYOUT FSETCL1622914132(FSETCLASS_LAYOUT self) {
 FSETCLASS_LAYOUT ret_val;
 FSETCLASS_LAYOUT r;
 FSETCLASS_LAYOUT L11;
 FSETCLASS_LAYOUT switch_structure;
 BOOL switch_structure1 = BOOL_zero;
 BOOL switch_structure2 = BOOL_zero;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 CLASS_LAYOUT L9;
 L21_=ASIZE((FSETCLASS_LAYOUT)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = FSETCL11745129(self, L81_);
 switch_structure = r;
 switch_structure1 = ATTR(self,use_map);
 switch_structure2 = TRUE;
 if (FSETCL377525362) {
  SATTR(switch_structure,use_map,switch_structure2);
 }
 else {
  SATTR(switch_structure,use_map,switch_structure1);
 }
 {
  struct FSETCL1233705521_frame_struct other1_0;
FSETCL1233705521_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETCL1233705521(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETCL686728372(r, L9);
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

FSETCLASS_LAYOUT FSETCL1676723742(FSETCLASS_LAYOUT self, FSETCLASS_LAYOUT r, CLASS_LAYOUT e) {
 FSETCLASS_LAYOUT ret_val;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 CLASS_LAYOUT te;
 CLASS_LAYOUT te1;
 FSETCLASS_LAYOUT elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETCLASS_LAYOUT is_elt_nil_self;
 CLASS_LAYOUT is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI737899749 is_elt_nil_self1;
 CLASS_LAYOUT is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 FSETCLASS_LAYOUT elt_eq_self;
 CLASS_LAYOUT elt_eq_e1;
 CLASS_LAYOUT elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ1657948444 elt_eq_self1;
 CLASS_LAYOUT elt_eq_e11;
 CLASS_LAYOUT elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 FSETCLASS_LAYOUT is_elt_nil_self2;
 CLASS_LAYOUT is_elt_nil_e2;
 BOOL ret_val6 = BOOL_zero;
 ELT_NI737899749 is_elt_nil_self3;
 CLASS_LAYOUT is_elt_nil_e3;
 BOOL ret_val7 = BOOL_zero;
 FSETCLASS_LAYOUT elt_eq_self2;
 CLASS_LAYOUT elt_eq_e12;
 CLASS_LAYOUT elt_eq_e22;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQ1657948444 elt_eq_self3;
 CLASS_LAYOUT elt_eq_e13;
 CLASS_LAYOUT elt_eq_e23;
 BOOL ret_val9 = BOOL_zero;
 INT L11_;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 CLASS_LAYOUT L61_;
 INT L81_;
 INT L91_;
 INT L10;
 BOOL L121_;
 CLASS_LAYOUT L131_;
 INT L151_;
 INT L17;
 INT L181_;
 L11_=ASIZE((FSETCLASS_LAYOUT)r); 
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
  L61_=(CLASS_LAYOUT)ARR((FSETCLASS_LAYOUT)r,h); 
  te = L61_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NI737899749) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1==((CLASS_LAYOUT) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQ1657948444) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   ret_val5 = (elt_eq_e11==elt_eq_e21);
   ret_val4 = ret_val5;
   if (ret_val4) {
    SARR((FSETCLASS_LAYOUT)r,h,(CLASS_LAYOUT)e); 
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
   L131_=(CLASS_LAYOUT)ARR((FSETCLASS_LAYOUT)r,h); 
   te1 = L131_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NI737899749) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val7 = (is_elt_nil_e3==((CLASS_LAYOUT) NULL));
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQ1657948444) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    ret_val9 = (elt_eq_e13==elt_eq_e23);
    ret_val8 = ret_val9;
    if (ret_val8) {
     SARR((FSETCLASS_LAYOUT)r,h,(CLASS_LAYOUT)e); 
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
 SARR((FSETCLASS_LAYOUT)r,h,(CLASS_LAYOUT)e); 
 ;
 L17 = ATTR(r,hsize);
 L181_=INTPLUS(L17,1); 
 SATTR(r,hsize,L181_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETCLASS_LAYOUT FSETCL686728372(FSETCLASS_LAYOUT self, CLASS_LAYOUT e) {
 FSETCLASS_LAYOUT ret_val;
 FSETCLASS_LAYOUT r;
 FSETCLASS_LAYOUT set_initial_stru;
 r = self;
 if ((r==((FSETCLASS_LAYOUT) NULL))) {
  r = FSETCL11745129(self, FSETCL786981258);
  set_initial_stru = r;
  SATTR(set_initial_stru,use_map,FSETCL2058817423);
 }
 else {
  r = FSETCL719945875(self);
 }
 if (ATTR(r,use_map)) {
  ret_val = FSETCL1676723742(self, r, e);
  return ret_val;
 }
 else {
  ret_val = FSETCL120396958(self, r, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETCLASS_LAYOUT FSETCL719945875(FSETCLASS_LAYOUT self) {
 FSETCLASS_LAYOUT ret_val;
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
  L11_=ASIZE((FSETCLASS_LAYOUT)self); 
  L2 = L11_;
  L3 = ATTR(self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FSETCL497069731;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  if (L91_) {
   ret_val = FSETCL1622914132(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
 else {
  L10 = ATTR(self,hsize);
  L121_=ASIZE((FSETCLASS_LAYOUT)self); 
  L13 = L121_;
  L141_=(L10)<(L13); 
  L15 = L141_;
  L161_=!(L15); 
  if (L161_) {
   ret_val = FSETCL1622914132(self);
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

FSETCS1561418284 FSETCS1162845217(FSETCS1561418284 self) {
 FSETCS1561418284 ret_val;
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
  L11_=ASIZE((FSETCS1561418284)self); 
  L2 = L11_;
  L3 = ATTR(self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FSETCS1883401206;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  if (L91_) {
   ret_val = FSETCS1557128156(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
 else {
  L10 = ATTR(self,hsize);
  L121_=ASIZE((FSETCS1561418284)self); 
  L13 = L121_;
  L141_=(L10)<(L13); 
  L15 = L141_;
  L161_=!(L15); 
  if (L161_) {
   ret_val = FSETCS1557128156(self);
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

FSETCS1561418284 FSETCS1557128156(FSETCS1561418284 self) {
 FSETCS1561418284 ret_val;
 FSETCS1561418284 r;
 FSETCS1561418284 L11;
 FSETCS1561418284 switch_structure;
 BOOL switch_structure1 = BOOL_zero;
 BOOL switch_structure2 = BOOL_zero;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 CSE_DOUBLE_EXPRS L9;
 L21_=ASIZE((FSETCS1561418284)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = FSETCS971548571(self, L81_);
 switch_structure = r;
 switch_structure1 = ATTR(self,use_map);
 switch_structure2 = TRUE;
 if (FSETCS1600255829) {
  SATTR(switch_structure,use_map,switch_structure2);
 }
 else {
  SATTR(switch_structure,use_map,switch_structure1);
 }
 {
  struct FSETCS1170733135_frame_struct other1_0;
FSETCS1170733135_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETCS1170733135(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETCS1791208015(r, L9);
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

FSETCS1561418284 FSETCS1791208015(FSETCS1561418284 self, CSE_DOUBLE_EXPRS e) {
 FSETCS1561418284 ret_val;
 FSETCS1561418284 r;
 FSETCS1561418284 set_initial_stru;
 r = self;
 if ((r==((FSETCS1561418284) NULL))) {
  r = FSETCS971548571(self, FSETCS2145101147);
  set_initial_stru = r;
  SATTR(set_initial_stru,use_map,FSETCS836086956);
 }
 else {
  r = FSETCS1162845217(self);
 }
 if (ATTR(r,use_map)) {
  ret_val = FSETCS197028574(self, r, e);
  return ret_val;
 }
 else {
  ret_val = FSETCS945551710(self, r, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETCS1561418284 FSETCS197028574(FSETCS1561418284 self, FSETCS1561418284 r, CSE_DOUBLE_EXPRS e) {
 FSETCS1561418284 ret_val;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 CSE_DOUBLE_EXPRS te;
 CSE_DOUBLE_EXPRS te1;
 FSETCS1561418284 elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETCS1561418284 is_elt_nil_self;
 CSE_DOUBLE_EXPRS is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI881793976 is_elt_nil_self1;
 CSE_DOUBLE_EXPRS is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 FSETCS1561418284 elt_eq_self;
 CSE_DOUBLE_EXPRS elt_eq_e1;
 CSE_DOUBLE_EXPRS elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ1507878401 elt_eq_self1;
 CSE_DOUBLE_EXPRS elt_eq_e11;
 CSE_DOUBLE_EXPRS elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 CSE_DOUBLE_EXPRS is_eq_self;
 CSE_DOUBLE_EXPRS is_eq_e;
 BOOL ret_val6 = BOOL_zero;
 FLISTdAM_EXPR aget_self;
 INT aget_ind = INT_zero;
 dAM_EXPR ret_val7;
 FLISTdAM_EXPR aget_self1;
 INT aget_ind1 = INT_zero;
 dAM_EXPR ret_val8;
 FSETCS1561418284 is_elt_nil_self2;
 CSE_DOUBLE_EXPRS is_elt_nil_e2;
 BOOL ret_val9 = BOOL_zero;
 ELT_NI881793976 is_elt_nil_self3;
 CSE_DOUBLE_EXPRS is_elt_nil_e3;
 BOOL ret_val10 = BOOL_zero;
 FSETCS1561418284 elt_eq_self2;
 CSE_DOUBLE_EXPRS elt_eq_e12;
 CSE_DOUBLE_EXPRS elt_eq_e22;
 BOOL ret_val11 = BOOL_zero;
 ELT_EQ1507878401 elt_eq_self3;
 CSE_DOUBLE_EXPRS elt_eq_e13;
 CSE_DOUBLE_EXPRS elt_eq_e23;
 BOOL ret_val12 = BOOL_zero;
 CSE_DOUBLE_EXPRS is_eq_self1;
 CSE_DOUBLE_EXPRS is_eq_e1;
 BOOL ret_val13 = BOOL_zero;
 FLISTdAM_EXPR aget_self2;
 INT aget_ind2 = INT_zero;
 dAM_EXPR ret_val14;
 FLISTdAM_EXPR aget_self3;
 INT aget_ind3 = INT_zero;
 dAM_EXPR ret_val15;
 INT L11_;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 CSE_DOUBLE_EXPRS L61_;
 dAM_EXPR L71_;
 dAM_EXPR L81_;
 dAM_EXPR L9;
 INT L121_;
 INT L131_;
 INT L14;
 BOOL L151_;
 CSE_DOUBLE_EXPRS L161_;
 dAM_EXPR L171_;
 dAM_EXPR L181_;
 dAM_EXPR L19;
 INT L211_;
 INT L23;
 INT L241_;
 L11_=ASIZE((FSETCS1561418284)r); 
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
  L61_=(CSE_DOUBLE_EXPRS)ARR((FSETCS1561418284)r,h); 
  te = L61_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NI881793976) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1==((CSE_DOUBLE_EXPRS) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQ1507878401) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   is_eq_self = elt_eq_e11;
   is_eq_e = elt_eq_e21;
   aget_self = ATTR(is_eq_self,expr);
   aget_ind = 0;
   L71_=(dAM_EXPR)ARR((FLISTdAM_EXPR)aget_self,aget_ind); 
   ret_val7 = L71_;
   aget_self1 = ATTR(is_eq_e,expr);
   aget_ind1 = 0;
   L81_=(dAM_EXPR)ARR((FLISTdAM_EXPR)aget_self1,aget_ind1); 
   ret_val8 = L81_;
   L9 = ret_val7;
   ret_val6 = (*dAM_EX2004971809[TAG(L9)])(L9, ((OB) ret_val8));
   ret_val5 = ret_val6;
   ret_val4 = ret_val5;
   if (ret_val4) {
    SARR((FSETCS1561418284)r,h,(CSE_DOUBLE_EXPRS)e); 
    ;
    ret_val = r;
    return ret_val;
   }
  }
  L121_=INTPLUS(h,1); 
  h = L121_;
 }
 after_loop: ;
 L131_=INTMINUS(asz,1); 
 L14 = L131_;
 L151_=(h)==(L14); 
 if (L151_) {
  h = 0;
  while (1) {
   L161_=(CSE_DOUBLE_EXPRS)ARR((FSETCS1561418284)r,h); 
   te1 = L161_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NI881793976) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val10 = (is_elt_nil_e3==((CSE_DOUBLE_EXPRS) NULL));
   ret_val9 = ret_val10;
   if (ret_val9) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQ1507878401) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    is_eq_self1 = elt_eq_e13;
    is_eq_e1 = elt_eq_e23;
    aget_self2 = ATTR(is_eq_self1,expr);
    aget_ind2 = 0;
    L171_=(dAM_EXPR)ARR((FLISTdAM_EXPR)aget_self2,aget_ind2); 
    ret_val14 = L171_;
    aget_self3 = ATTR(is_eq_e1,expr);
    aget_ind3 = 0;
    L181_=(dAM_EXPR)ARR((FLISTdAM_EXPR)aget_self3,aget_ind3); 
    ret_val15 = L181_;
    L19 = ret_val14;
    ret_val13 = (*dAM_EX2004971809[TAG(L19)])(L19, ((OB) ret_val15));
    ret_val12 = ret_val13;
    ret_val11 = ret_val12;
    if (ret_val11) {
     SARR((FSETCS1561418284)r,h,(CSE_DOUBLE_EXPRS)e); 
     ;
     ret_val = r;
     return ret_val;
    }
   }
   L211_=INTPLUS(h,1); 
   h = L211_;
  }
  after_loop1: ;
 }
 SARR((FSETCS1561418284)r,h,(CSE_DOUBLE_EXPRS)e); 
 ;
 L23 = ATTR(r,hsize);
 L241_=INTPLUS(L23,1); 
 SATTR(r,hsize,L241_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETCS1561418284 FSETCS945551710(FSETCS1561418284 self, FSETCS1561418284 r, CSE_DOUBLE_EXPRS e) {
 FSETCS1561418284 ret_val;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETCS1561418284 elt_eq_self;
 CSE_DOUBLE_EXPRS elt_eq_e1;
 CSE_DOUBLE_EXPRS elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ1507878401 elt_eq_self1;
 CSE_DOUBLE_EXPRS elt_eq_e11;
 CSE_DOUBLE_EXPRS elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 CSE_DOUBLE_EXPRS is_eq_self;
 CSE_DOUBLE_EXPRS is_eq_e;
 BOOL ret_val3 = BOOL_zero;
 FLISTdAM_EXPR aget_self;
 INT aget_ind = INT_zero;
 dAM_EXPR ret_val4;
 FLISTdAM_EXPR aget_self1;
 INT aget_ind1 = INT_zero;
 dAM_EXPR ret_val5;
 BOOL L11_;
 CSE_DOUBLE_EXPRS L21_;
 dAM_EXPR L31_;
 dAM_EXPR L41_;
 dAM_EXPR L5;
 INT L71_;
 INT L8;
 INT L10;
 INT L121_;
 i = 0;
 sz = ATTR(r,hsize);
 while (1) {
  L11_=(i)==(sz); 
  if (L11_) {
   goto after_loop;
  }
  elt_eq_self = self;
  elt_eq_e1 = e;
  L21_=(CSE_DOUBLE_EXPRS)ARR((FSETCS1561418284)r,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQ1507878401) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  is_eq_self = elt_eq_e11;
  is_eq_e = elt_eq_e21;
  aget_self = ATTR(is_eq_self,expr);
  aget_ind = 0;
  L31_=(dAM_EXPR)ARR((FLISTdAM_EXPR)aget_self,aget_ind); 
  ret_val4 = L31_;
  aget_self1 = ATTR(is_eq_e,expr);
  aget_ind1 = 0;
  L41_=(dAM_EXPR)ARR((FLISTdAM_EXPR)aget_self1,aget_ind1); 
  ret_val5 = L41_;
  L5 = ret_val4;
  ret_val3 = (*dAM_EX2004971809[TAG(L5)])(L5, ((OB) ret_val5));
  ret_val2 = ret_val3;
  ret_val1 = ret_val2;
  if (ret_val1) {
   SARR((FSETCS1561418284)r,i,(CSE_DOUBLE_EXPRS)e); 
   ;
   ret_val = r;
   return ret_val;
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 L8 = ATTR(r,hsize);
 SARR((FSETCS1561418284)r,L8,(CSE_DOUBLE_EXPRS)e); 
 ;
 L10 = ATTR(r,hsize);
 L121_=INTPLUS(L10,1); 
 SATTR(r,hsize,L121_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETCS1561418284 FSETCS971548571(FSETCS1561418284 self, INT n) {
 FSETCS1561418284 ret_val;
 FSETCS1561418284 r;
 FSETCS1561418284 L11;
 FSETCS1561418284 elt_nil_self;
 CSE_DOUBLE_EXPRS ret_val1;
 ELT_NI881793976 elt_nil_self1;
 CSE_DOUBLE_EXPRS ret_val2;
 CSE_DOUBLE_EXPRS e = ((CSE_DOUBLE_EXPRS) NULL);
 FSETCS1561418284 elt_nil_self2;
 CSE_DOUBLE_EXPRS ret_val3;
 ELT_NI881793976 elt_nil_self3;
 CSE_DOUBLE_EXPRS ret_val4;
 CSE_DOUBLE_EXPRS e1 = ((CSE_DOUBLE_EXPRS) NULL);
 INT L21 = INT_zero;
 FSETCS1561418284 L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 CSE_DOUBLE_EXPRS L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct FSETCS1561418284_struct)-sizeof(CSE_DOUBLE_EXPRS))+(L4)*sizeof(CSE_DOUBLE_EXPRS);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FSETCS1561418284_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FSETCS1561418284) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NI881793976) NULL);
 ret_val2 = ((CSE_DOUBLE_EXPRS) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((CSE_DOUBLE_EXPRS) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FSETCS1561418284)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NI881793976) NULL);
    ret_val4 = ((CSE_DOUBLE_EXPRS) NULL);
    ret_val3 = ret_val4;
    L10 = ret_val3;
    SARR((FSETCS1561418284)L11,L21,(CSE_DOUBLE_EXPRS)L10); 
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

FSETEX1260947371 FSETEX1004991460(FSETEX1260947371 self) {
 FSETEX1260947371 ret_val;
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
  L11_=ASIZE((FSETEX1260947371)self); 
  L2 = L11_;
  L3 = ATTR(self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FSETEX2023113290;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  if (L91_) {
   ret_val = FSETEX1571333349(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
 else {
  L10 = ATTR(self,hsize);
  L121_=ASIZE((FSETEX1260947371)self); 
  L13 = L121_;
  L141_=(L10)<(L13); 
  L15 = L141_;
  L161_=!(L15); 
  if (L161_) {
   ret_val = FSETEX1571333349(self);
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

FSETEX1260947371 FSETEX1141803248(FSETEX1260947371 self, INT n) {
 FSETEX1260947371 ret_val;
 FSETEX1260947371 r;
 FSETEX1260947371 L11;
 FSETEX1260947371 elt_nil_self;
 EXTERNAL_LAYOUT ret_val1;
 ELT_NI94923122 elt_nil_self1;
 EXTERNAL_LAYOUT ret_val2;
 EXTERNAL_LAYOUT e = ((EXTERNAL_LAYOUT) NULL);
 FSETEX1260947371 elt_nil_self2;
 EXTERNAL_LAYOUT ret_val3;
 ELT_NI94923122 elt_nil_self3;
 EXTERNAL_LAYOUT ret_val4;
 EXTERNAL_LAYOUT e1 = ((EXTERNAL_LAYOUT) NULL);
 INT L21 = INT_zero;
 FSETEX1260947371 L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 EXTERNAL_LAYOUT L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct FSETEX1260947371_struct)-sizeof(EXTERNAL_LAYOUT))+(L4)*sizeof(EXTERNAL_LAYOUT);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FSETEX1260947371_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FSETEX1260947371) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NI94923122) NULL);
 ret_val2 = ((EXTERNAL_LAYOUT) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((EXTERNAL_LAYOUT) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FSETEX1260947371)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NI94923122) NULL);
    ret_val4 = ((EXTERNAL_LAYOUT) NULL);
    ret_val3 = ret_val4;
    L10 = ret_val3;
    SARR((FSETEX1260947371)L11,L21,(EXTERNAL_LAYOUT)L10); 
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

FSETEX1260947371 FSETEX1154744664(FSETEX1260947371 self, FSETEX1260947371 r, EXTERNAL_LAYOUT e) {
 FSETEX1260947371 ret_val;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETEX1260947371 elt_eq_self;
 EXTERNAL_LAYOUT elt_eq_e1;
 EXTERNAL_LAYOUT elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ482950930 elt_eq_self1;
 EXTERNAL_LAYOUT elt_eq_e11;
 EXTERNAL_LAYOUT elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 BOOL L11_;
 EXTERNAL_LAYOUT L21_;
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
  L21_=(EXTERNAL_LAYOUT)ARR((FSETEX1260947371)r,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQ482950930) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  ret_val2 = (elt_eq_e11==elt_eq_e21);
  ret_val1 = ret_val2;
  if (ret_val1) {
   SARR((FSETEX1260947371)r,i,(EXTERNAL_LAYOUT)e); 
   ;
   ret_val = r;
   return ret_val;
  }
  L41_=INTPLUS(i,1); 
  i = L41_;
 }
 after_loop: ;
 L5 = ATTR(r,hsize);
 SARR((FSETEX1260947371)r,L5,(EXTERNAL_LAYOUT)e); 
 ;
 L7 = ATTR(r,hsize);
 L81_=INTPLUS(L7,1); 
 SATTR(r,hsize,L81_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETEX1260947371 FSETEX1571333349(FSETEX1260947371 self) {
 FSETEX1260947371 ret_val;
 FSETEX1260947371 r;
 FSETEX1260947371 L11;
 FSETEX1260947371 switch_structure;
 BOOL switch_structure1 = BOOL_zero;
 BOOL switch_structure2 = BOOL_zero;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 EXTERNAL_LAYOUT L9;
 L21_=ASIZE((FSETEX1260947371)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = FSETEX1141803248(self, L81_);
 switch_structure = r;
 switch_structure1 = ATTR(self,use_map);
 switch_structure2 = TRUE;
 if (FSETEX1520806408) {
  SATTR(switch_structure,use_map,switch_structure2);
 }
 else {
  SATTR(switch_structure,use_map,switch_structure1);
 }
 {
  struct FSETEX1633103446_frame_struct other1_0;
FSETEX1633103446_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETEX1633103446(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETEX1746916700(r, L9);
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

FSETEX1260947371 FSETEX1746916700(FSETEX1260947371 self, EXTERNAL_LAYOUT e) {
 FSETEX1260947371 ret_val;
 FSETEX1260947371 r;
 FSETEX1260947371 set_initial_stru;
 r = self;
 if ((r==((FSETEX1260947371) NULL))) {
  r = FSETEX1141803248(self, FSETEX1632590957);
  set_initial_stru = r;
  SATTR(set_initial_stru,use_map,FSETEX915536377);
 }
 else {
  r = FSETEX1004991460(self);
 }
 if (ATTR(r,use_map)) {
  ret_val = FSETEX1861592536(self, r, e);
  return ret_val;
 }
 else {
  ret_val = FSETEX1154744664(self, r, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETEX1260947371 FSETEX1861592536(FSETEX1260947371 self, FSETEX1260947371 r, EXTERNAL_LAYOUT e) {
 FSETEX1260947371 ret_val;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 EXTERNAL_LAYOUT te;
 EXTERNAL_LAYOUT te1;
 FSETEX1260947371 elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETEX1260947371 is_elt_nil_self;
 EXTERNAL_LAYOUT is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI94923122 is_elt_nil_self1;
 EXTERNAL_LAYOUT is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 FSETEX1260947371 elt_eq_self;
 EXTERNAL_LAYOUT elt_eq_e1;
 EXTERNAL_LAYOUT elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ482950930 elt_eq_self1;
 EXTERNAL_LAYOUT elt_eq_e11;
 EXTERNAL_LAYOUT elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 FSETEX1260947371 is_elt_nil_self2;
 EXTERNAL_LAYOUT is_elt_nil_e2;
 BOOL ret_val6 = BOOL_zero;
 ELT_NI94923122 is_elt_nil_self3;
 EXTERNAL_LAYOUT is_elt_nil_e3;
 BOOL ret_val7 = BOOL_zero;
 FSETEX1260947371 elt_eq_self2;
 EXTERNAL_LAYOUT elt_eq_e12;
 EXTERNAL_LAYOUT elt_eq_e22;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQ482950930 elt_eq_self3;
 EXTERNAL_LAYOUT elt_eq_e13;
 EXTERNAL_LAYOUT elt_eq_e23;
 BOOL ret_val9 = BOOL_zero;
 INT L11_;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 EXTERNAL_LAYOUT L61_;
 INT L81_;
 INT L91_;
 INT L10;
 BOOL L121_;
 EXTERNAL_LAYOUT L131_;
 INT L151_;
 INT L17;
 INT L181_;
 L11_=ASIZE((FSETEX1260947371)r); 
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
  L61_=(EXTERNAL_LAYOUT)ARR((FSETEX1260947371)r,h); 
  te = L61_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NI94923122) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1==((EXTERNAL_LAYOUT) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQ482950930) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   ret_val5 = (elt_eq_e11==elt_eq_e21);
   ret_val4 = ret_val5;
   if (ret_val4) {
    SARR((FSETEX1260947371)r,h,(EXTERNAL_LAYOUT)e); 
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
   L131_=(EXTERNAL_LAYOUT)ARR((FSETEX1260947371)r,h); 
   te1 = L131_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NI94923122) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val7 = (is_elt_nil_e3==((EXTERNAL_LAYOUT) NULL));
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQ482950930) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    ret_val9 = (elt_eq_e13==elt_eq_e23);
    ret_val8 = ret_val9;
    if (ret_val8) {
     SARR((FSETEX1260947371)r,h,(EXTERNAL_LAYOUT)e); 
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
 SARR((FSETEX1260947371)r,h,(EXTERNAL_LAYOUT)e); 
 ;
 L17 = ATTR(r,hsize);
 L181_=INTPLUS(L17,1); 
 SATTR(r,hsize,L181_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT FSETAM1999114804(FSETAM_ROUT_DEF self) {
 INT ret_val = INT_zero;
 if ((self==((FSETAM_ROUT_DEF) NULL))) {
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

INT FSETAM2105800198(FSETAM1542678947 self) {
 INT ret_val = INT_zero;
 if ((self==((FSETAM1542678947) NULL))) {
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

INT FSETAS77357633(FSETAS_FEAT_MOD self) {
 INT ret_val = INT_zero;
 if ((self==((FSETAS_FEAT_MOD) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  ret_val = ATTR(self,hsize);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 1

AM_ROUT_DEF FSETAM1263255605(FSETAM1263255605_frame frame) {
 AM_ROUT_DEF dummy = ((AM_ROUT_DEF) NULL);
 AM_ROUT_DEF aI_u_I;
 AM_ROUT_DEF L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETAM_ROUT_DEF)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FSETAM_ROUT_DEF)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

AM_ROUT_DEF FSETAM1323524382(FSETAM1323524382_frame frame) {
 AM_ROUT_DEF dummy = ((AM_ROUT_DEF) NULL);
 BOOL L1;
 BOOL L21_;
 AM_ROUT_DEF L5;
 AM_ROUT_DEF aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 BOOL L111_;
 AM_ROUT_DEF L121_;
 INT L131_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  }
 state0:;
 L1 = (frame->self==((FSETAM_ROUT_DEF) NULL));
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
      frame->L41_br=ASIZE((FSETAM_ROUT_DEF)frame->L61); 
     }
     if(frame->L31>=frame->L41_br)  goto after_loop; 
     aL41_=ARR((FSETAM_ROUT_DEF)frame->L61,frame->L31); 
     frame->r = aL41_;
     frame->is_elt_nil_self = frame->self;
     frame->is_elt_nil_e = frame->r;
     frame->is_elt_nil_self1 = ((ELT_NI602850927) NULL);
     frame->is_elt_nil_e1 = frame->is_elt_nil_e;
     frame->ret_val1 = (frame->is_elt_nil_e1==((AM_ROUT_DEF) NULL));
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
    L121_=(AM_ROUT_DEF)ARR((FSETAM_ROUT_DEF)frame->self,frame->i); 
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

AM_SHARED_EXPR FSETAM12483681(FSETAM12483681_frame frame) {
 AM_SHARED_EXPR dummy = ((AM_SHARED_EXPR) NULL);
 BOOL L1;
 BOOL L21_;
 AM_SHARED_EXPR L5;
 AM_SHARED_EXPR aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 BOOL L111_;
 AM_SHARED_EXPR L121_;
 INT L131_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  }
 state0:;
 L1 = (frame->self==((FSETAM1542678947) NULL));
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
      frame->L41_br=ASIZE((FSETAM1542678947)frame->L61); 
     }
     if(frame->L31>=frame->L41_br)  goto after_loop; 
     aL41_=ARR((FSETAM1542678947)frame->L61,frame->L31); 
     frame->r = aL41_;
     frame->is_elt_nil_self = frame->self;
     frame->is_elt_nil_e = frame->r;
     frame->is_elt_nil_self1 = ((ELT_NI1377997731) NULL);
     frame->is_elt_nil_e1 = frame->is_elt_nil_e;
     frame->ret_val1 = (frame->is_elt_nil_e1==((AM_SHARED_EXPR) NULL));
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
    L121_=(AM_SHARED_EXPR)ARR((FSETAM1542678947)frame->self,frame->i); 
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

AM_SHARED_EXPR FSETAM674227038(FSETAM674227038_frame frame) {
 AM_SHARED_EXPR dummy = ((AM_SHARED_EXPR) NULL);
 AM_SHARED_EXPR aI_u_I;
 AM_SHARED_EXPR L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETAM1542678947)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FSETAM1542678947)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

AS_FEAT_MOD FSETAS1592299594(FSETAS1592299594_frame frame) {
 AS_FEAT_MOD dummy = ((AS_FEAT_MOD) NULL);
 BOOL L1;
 BOOL L21_;
 AS_FEAT_MOD L5;
 AS_FEAT_MOD aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 BOOL L111_;
 AS_FEAT_MOD L121_;
 INT L131_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  }
 state0:;
 L1 = (frame->self==((FSETAS_FEAT_MOD) NULL));
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
      frame->L41_br=ASIZE((FSETAS_FEAT_MOD)frame->L61); 
     }
     if(frame->L31>=frame->L41_br)  goto after_loop; 
     aL41_=ARR((FSETAS_FEAT_MOD)frame->L61,frame->L31); 
     frame->r = aL41_;
     frame->is_elt_nil_self = frame->self;
     frame->is_elt_nil_e = frame->r;
     frame->is_elt_nil_self1 = ((ELT_NI465844240) NULL);
     frame->is_elt_nil_e1 = frame->is_elt_nil_e;
     frame->ret_val1 = (frame->is_elt_nil_e1==((AS_FEAT_MOD) NULL));
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
    L121_=(AS_FEAT_MOD)ARR((FSETAS_FEAT_MOD)frame->self,frame->i); 
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

AS_FEAT_MOD FSETAS18373515(FSETAS18373515_frame frame) {
 AS_FEAT_MOD dummy = ((AS_FEAT_MOD) NULL);
 AS_FEAT_MOD aI_u_I;
 AS_FEAT_MOD L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETAS_FEAT_MOD)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FSETAS_FEAT_MOD)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

BOUND_2140373124 FSETBO1041122758(FSETBO1041122758_frame frame) {
 BOUND_2140373124 dummy = ((BOUND_2140373124) NULL);
 BOUND_2140373124 aI_u_I;
 BOUND_2140373124 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETBO1548397318)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FSETBO1548397318)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

BOUND_2140373124 FSETBO677183779(FSETBO677183779_frame frame) {
 BOUND_2140373124 dummy = ((BOUND_2140373124) NULL);
 BOOL L1;
 BOOL L21_;
 BOUND_2140373124 L5;
 BOUND_2140373124 aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 BOOL L111_;
 BOUND_2140373124 L121_;
 INT L131_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  }
 state0:;
 L1 = (frame->self==((FSETBO1548397318) NULL));
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
      frame->L41_br=ASIZE((FSETBO1548397318)frame->L61); 
     }
     if(frame->L31>=frame->L41_br)  goto after_loop; 
     aL41_=ARR((FSETBO1548397318)frame->L61,frame->L31); 
     frame->r = aL41_;
     frame->is_elt_nil_self = frame->self;
     frame->is_elt_nil_e = frame->r;
     frame->is_elt_nil_self1 = ((ELT_NI338362486) NULL);
     frame->is_elt_nil_e1 = frame->is_elt_nil_e;
     frame->ret_val1 = (frame->is_elt_nil_e1==((BOUND_2140373124) NULL));
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
    L121_=(BOUND_2140373124)ARR((FSETBO1548397318)frame->self,frame->i); 
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

BOUND_809390774 FSETBO10998148(FSETBO10998148_frame frame) {
 BOUND_809390774 dummy = ((BOUND_809390774) NULL);
 BOOL L1;
 BOOL L21_;
 BOUND_809390774 L5;
 BOUND_809390774 aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 BOOL L111_;
 BOUND_809390774 L121_;
 INT L131_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  }
 state0:;
 L1 = (frame->self==((FSETBO1147931784) NULL));
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
      frame->L41_br=ASIZE((FSETBO1147931784)frame->L61); 
     }
     if(frame->L31>=frame->L41_br)  goto after_loop; 
     aL41_=ARR((FSETBO1147931784)frame->L61,frame->L31); 
     frame->r = aL41_;
     frame->is_elt_nil_self = frame->self;
     frame->is_elt_nil_e = frame->r;
     frame->is_elt_nil_self1 = ((ELT_NI1119971763) NULL);
     frame->is_elt_nil_e1 = frame->is_elt_nil_e;
     frame->ret_val1 = (frame->is_elt_nil_e1==((BOUND_809390774) NULL));
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
    L121_=(BOUND_809390774)ARR((FSETBO1147931784)frame->self,frame->i); 
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

BOUND_809390774 FSETBO1129496037(FSETBO1129496037_frame frame) {
 BOUND_809390774 dummy = ((BOUND_809390774) NULL);
 BOUND_809390774 aI_u_I;
 BOUND_809390774 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETBO1147931784)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FSETBO1147931784)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

BUILTIN_LAYOUT FSETBU202043707(FSETBU202043707_frame frame) {
 BUILTIN_LAYOUT dummy = ((BUILTIN_LAYOUT) NULL);
 BOOL L1;
 BOOL L21_;
 BUILTIN_LAYOUT L5;
 BUILTIN_LAYOUT aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 BOOL L111_;
 BUILTIN_LAYOUT L121_;
 INT L131_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  }
 state0:;
 L1 = (frame->self==((FSETBU1446477505) NULL));
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
      frame->L41_br=ASIZE((FSETBU1446477505)frame->L61); 
     }
     if(frame->L31>=frame->L41_br)  goto after_loop; 
     aL41_=ARR((FSETBU1446477505)frame->L61,frame->L31); 
     frame->r = aL41_;
     frame->is_elt_nil_self = frame->self;
     frame->is_elt_nil_e = frame->r;
     frame->is_elt_nil_self1 = ((ELT_NI1281796289) NULL);
     frame->is_elt_nil_e1 = frame->is_elt_nil_e;
     frame->ret_val1 = (frame->is_elt_nil_e1==((BUILTIN_LAYOUT) NULL));
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
    L121_=(BUILTIN_LAYOUT)ARR((FSETBU1446477505)frame->self,frame->i); 
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

BUILTIN_LAYOUT FSETBU838451994(FSETBU838451994_frame frame) {
 BUILTIN_LAYOUT dummy = ((BUILTIN_LAYOUT) NULL);
 BUILTIN_LAYOUT aI_u_I;
 BUILTIN_LAYOUT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETBU1446477505)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FSETBU1446477505)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

CLASS_LAYOUT FSETCL108778432(FSETCL108778432_frame frame) {
 CLASS_LAYOUT dummy = ((CLASS_LAYOUT) NULL);
 CLASS_LAYOUT aI_u_I;
 CLASS_LAYOUT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETCLASS_LAYOUT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FSETCLASS_LAYOUT)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

CLASS_LAYOUT FSETCL1233705521(FSETCL1233705521_frame frame) {
 CLASS_LAYOUT dummy = ((CLASS_LAYOUT) NULL);
 BOOL L1;
 BOOL L21_;
 CLASS_LAYOUT L5;
 CLASS_LAYOUT aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 BOOL L111_;
 CLASS_LAYOUT L121_;
 INT L131_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  }
 state0:;
 L1 = (frame->self==((FSETCLASS_LAYOUT) NULL));
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
      frame->L41_br=ASIZE((FSETCLASS_LAYOUT)frame->L61); 
     }
     if(frame->L31>=frame->L41_br)  goto after_loop; 
     aL41_=ARR((FSETCLASS_LAYOUT)frame->L61,frame->L31); 
     frame->r = aL41_;
     frame->is_elt_nil_self = frame->self;
     frame->is_elt_nil_e = frame->r;
     frame->is_elt_nil_self1 = ((ELT_NI737899749) NULL);
     frame->is_elt_nil_e1 = frame->is_elt_nil_e;
     frame->ret_val1 = (frame->is_elt_nil_e1==((CLASS_LAYOUT) NULL));
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
    L121_=(CLASS_LAYOUT)ARR((FSETCLASS_LAYOUT)frame->self,frame->i); 
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

CSE_DOUBLE_EXPRS FSETCS1170733135(FSETCS1170733135_frame frame) {
 CSE_DOUBLE_EXPRS dummy = ((CSE_DOUBLE_EXPRS) NULL);
 BOOL L1;
 BOOL L21_;
 CSE_DOUBLE_EXPRS L5;
 CSE_DOUBLE_EXPRS aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 BOOL L111_;
 CSE_DOUBLE_EXPRS L121_;
 INT L131_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  }
 state0:;
 L1 = (frame->self==((FSETCS1561418284) NULL));
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
      frame->L41_br=ASIZE((FSETCS1561418284)frame->L61); 
     }
     if(frame->L31>=frame->L41_br)  goto after_loop; 
     aL41_=ARR((FSETCS1561418284)frame->L61,frame->L31); 
     frame->r = aL41_;
     frame->is_elt_nil_self = frame->self;
     frame->is_elt_nil_e = frame->r;
     frame->is_elt_nil_self1 = ((ELT_NI881793976) NULL);
     frame->is_elt_nil_e1 = frame->is_elt_nil_e;
     frame->ret_val1 = (frame->is_elt_nil_e1==((CSE_DOUBLE_EXPRS) NULL));
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
    L121_=(CSE_DOUBLE_EXPRS)ARR((FSETCS1561418284)frame->self,frame->i); 
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

CSE_DOUBLE_EXPRS FSETCS1710945472(FSETCS1710945472_frame frame) {
 CSE_DOUBLE_EXPRS dummy = ((CSE_DOUBLE_EXPRS) NULL);
 CSE_DOUBLE_EXPRS aI_u_I;
 CSE_DOUBLE_EXPRS L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETCS1561418284)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FSETCS1561418284)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

EXTERNAL_LAYOUT FSETEX1633103446(FSETEX1633103446_frame frame) {
 EXTERNAL_LAYOUT dummy = ((EXTERNAL_LAYOUT) NULL);
 BOOL L1;
 BOOL L21_;
 EXTERNAL_LAYOUT L5;
 EXTERNAL_LAYOUT aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 BOOL L111_;
 EXTERNAL_LAYOUT L121_;
 INT L131_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  }
 state0:;
 L1 = (frame->self==((FSETEX1260947371) NULL));
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
      frame->L41_br=ASIZE((FSETEX1260947371)frame->L61); 
     }
     if(frame->L31>=frame->L41_br)  goto after_loop; 
     aL41_=ARR((FSETEX1260947371)frame->L61,frame->L31); 
     frame->r = aL41_;
     frame->is_elt_nil_self = frame->self;
     frame->is_elt_nil_e = frame->r;
     frame->is_elt_nil_self1 = ((ELT_NI94923122) NULL);
     frame->is_elt_nil_e1 = frame->is_elt_nil_e;
     frame->ret_val1 = (frame->is_elt_nil_e1==((EXTERNAL_LAYOUT) NULL));
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
    L121_=(EXTERNAL_LAYOUT)ARR((FSETEX1260947371)frame->self,frame->i); 
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

EXTERNAL_LAYOUT FSETEX1787855975(FSETEX1787855975_frame frame) {
 EXTERNAL_LAYOUT dummy = ((EXTERNAL_LAYOUT) NULL);
 EXTERNAL_LAYOUT aI_u_I;
 EXTERNAL_LAYOUT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETEX1260947371)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FSETEX1260947371)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

void FSETAM289897564(FSETAM289897564_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETAM1542678947)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FSETAM1542678947)frame->self,frame->i_I_u_I,(AM_SHARED_EXPR)frame->arg1); frame->i_I_u_I++;
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

void FSETAM541410851(FSETAM541410851_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETAM_ROUT_DEF)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FSETAM_ROUT_DEF)frame->self,frame->i_I_u_I,(AM_ROUT_DEF)frame->arg1); frame->i_I_u_I++;
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

void FSETAS703471239(FSETAS703471239_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETAS_FEAT_MOD)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FSETAS_FEAT_MOD)frame->self,frame->i_I_u_I,(AS_FEAT_MOD)frame->arg1); frame->i_I_u_I++;
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

void FSETBO1600177472(FSETBO1600177472_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETBO1548397318)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FSETBO1548397318)frame->self,frame->i_I_u_I,(BOUND_2140373124)frame->arg1); frame->i_I_u_I++;
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

void FSETBO189494685(FSETBO189494685_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETBO1147931784)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FSETBO1147931784)frame->self,frame->i_I_u_I,(BOUND_809390774)frame->arg1); frame->i_I_u_I++;
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

void FSETBU1802576596(FSETBU1802576596_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETBU1446477505)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FSETBU1446477505)frame->self,frame->i_I_u_I,(BUILTIN_LAYOUT)frame->arg1); frame->i_I_u_I++;
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

void FSETCL938926870(FSETCL938926870_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETCLASS_LAYOUT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FSETCLASS_LAYOUT)frame->self,frame->i_I_u_I,(CLASS_LAYOUT)frame->arg1); frame->i_I_u_I++;
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

void FSETCS1554315126(FSETCS1554315126_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETCS1561418284)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FSETCS1561418284)frame->self,frame->i_I_u_I,(CSE_DOUBLE_EXPRS)frame->arg1); frame->i_I_u_I++;
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

void FSETEX1368613067(FSETEX1368613067_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETEX1260947371)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FSETEX1260947371)frame->self,frame->i_I_u_I,(EXTERNAL_LAYOUT)frame->arg1); frame->i_I_u_I++;
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
