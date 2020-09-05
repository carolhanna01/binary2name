#include "sather.h"

/* Layouts */

typedef struct dSTR_struct {
 OB_HEADER header;
 } *dSTR;

typedef struct ARRAYSTR_struct {/* layout for ARRAY{STR} */
 OB_HEADER header;
 INT asize;
 STR arr_part[1];
 } *ARRAYSTR;

typedef struct BFILE_struct {/* layout for BFILE */
 OB_HEADER header;
 EXT_OB fp;
 } *BFILE;

typedef struct ELT_NILSTR_struct {/* layout for ELT_NIL{STR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILSTR;

typedef struct ERR1_struct {/* layout for ERR */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ERR1;

typedef struct FILE1_struct {/* layout for FILE */
 OB_HEADER header;
 EXT_OB fp;
 } *FILE1;

typedef struct INT_do676118316_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT arg1;/* Formal argument: i */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_I,I_u_Im; /* used by builtin iter */
 } *INT_do676118316_frame;

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

typedef struct STR_el1843619312_frame_struct {
 INT state;
 STR self;/* Formal argument: self */
 INT arg1;/* Formal argument: beg */
 CHAR ret_val;
 INT i;
 INT sz;
 } *STR_el1843619312_frame;

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

typedef struct ARRAYS1161048019_frame_struct {
 INT state;
 ARRAYSTR self;/* Formal argument: self */
 STR arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYS1161048019_frame;

typedef struct CODE_OPTIONS_struct {/* layout for CODE_OPTIONS */
 OB_HEADER header;
 struct ARRAYSTR_struct *arg_locations;
 struct ARRAYSTR_struct *args;
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
 INT next;
 struct PROG_struct *prog;
 BOOL all1;
 BOOL arith_all;
 BOOL assert_all;
 BOOL bounds_all;
 BOOL destroy_chk;
 BOOL got_libs;
 BOOL invariant_all;
 BOOL post_all;
 BOOL pre_all;
 BOOL psather_chk;
 BOOL psather_stats;
 BOOL psather_trace;
 BOOL return_all;
 BOOL stats;
 BOOL void_all;
 BOOL when_all;
 } *CODE_OPTIONS;

typedef struct FLISTSTR_struct {/* layout for FLIST{STR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 STR arr_part[1];
 } *FLISTSTR;

typedef struct FMAPSTRSTR_struct {/* layout for FMAP{STR,STR} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRSTR_struct arr_part[1];
 } *FMAPSTRSTR;

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


/* Function declarations */


extern RETURNED_CONST STR (**dSTR_strrSTR)(dSTR);
ARRAYSTR ARRAYS417083844(ARRAYSTR, ARRAYSTR);
ARRAYSTR FLISTS533235694(FLISTSTR);
BOOL CHAR_i629101715(CHAR);
BOOL CHAR_i84735023(CHAR);
BOOL CODE_O1472228953(CODE_OPTIONS, STR);
BOOL CODE_O1581463621(CODE_OPTIONS, STR);
BOOL FSETST1025458804(FSETSTR, STR);
BOOL STR_is111530248(STR, STR);
CHAR STR_el1843619312(STR_el1843619312_frame);
FLISTSTR CODE_O992729481(CODE_OPTIONS, STR);
FLISTSTR FLISTS1425610062(FLISTSTR, STR);
FMAPSTRSTR FMAPST859022540(FMAPSTRSTR, STR, STR);
FSETSTR FSETST1404644833(FSETSTR, STR);
FSETSTR FSETST1884046860(FSETSTR);
FSETSTR FSETST866022026(FSETSTR, FSETSTR);
FSETSTR FSETST897681893(FSETSTR, FSETSTR);
FSTR BFILE_fstrrFSTR(BFILE);
FSTR FSTR_p399773021(FSTR, CHAR);
FSTR FSTR_s1920457899(FSTR, INT, INT);
INT FLISTS326330076(FLISTSTR);
INT FSTR_sizerINT(FSTR);
INT INT_do676118316(INT_do676118316_frame);
INT STR_lengthrINT(STR);
INT STR_se1725540125(STR, CHAR);
INT STR_sizerINT(STR);
STR CODE_O1099966235(CODE_OPTIONS);
STR CODE_O1483684745(CODE_OPTIONS, STR, dSTR, INT*);
STR CODE_O1955258734(CODE_OPTIONS, STR);
STR CODE_O218236968(CODE_OPTIONS, STR, dSTR);
STR CODE_O346288391(CODE_OPTIONS);
STR CODE_O689829754(CODE_OPTIONS, STR);
STR CODE_O740953890(CODE_OPTIONS, STR, STR, STR);
STR CODE_O862164136(CODE_OPTIONS);
STR FSETSTR_eltbrSTR(FSETSTR_eltbrSTR_frame);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap2004550586(STR, STR);
STR STR_as1755591216(STR);
STR STR_fr1097270334(STR, FSTR);
STR STR_head_INTrSTR(STR, INT);
STR STR_tail_INTrSTR(STR, INT);
STR UNIX_g108606936(UNIX, STR);
void ARRAYS1161048019(ARRAYS1161048019_frame);
void CODE_O1006348781(CODE_OPTIONS, STR);
void CODE_O1040697618(CODE_OPTIONS);
void CODE_O1614295032(CODE_OPTIONS, ARRAYSTR);
void CODE_O1656361547(CODE_OPTIONS, STR);
void CODE_O1709756185(CODE_OPTIONS, FLISTSTR, STR);
void CODE_O1762472157(CODE_OPTIONS);
void CODE_O1862448983(CODE_OPTIONS, STR, STR);
void CODE_O549188150(CODE_OPTIONS);
void FILE_plus_STR(FILE1, STR);
void FILE_plus_dSTR(FILE1, dSTR);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

BOOL CODE_O1472228953(CODE_OPTIONS self, STR fn) {
 BOOL ret_val = BOOL_zero;
 CHAR L11_;
 CHAR L2;
 BOOL L31_;
 CHAR L41_;
 CHAR L5;
 BOOL L61_;
 INT L7;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 L11_=ARR((STR)fn,0); 
 L2 = L11_;
 L31_=L2=='-'; 
 if (L31_) {
  ret_val = FALSE;
  return ret_val;
 }
 L41_=ARR((STR)fn,0); 
 L5 = L41_;
 L61_=L5=='/'; 
 if (L61_) {
  ret_val = TRUE;
  return ret_val;
 }
 L7 = STR_se1725540125(fn, '.');
 L81_=(L7)==(-1); 
 L9 = L81_;
 L101_=!(L9); 
 ret_val = L101_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL CODE_O1581463621(CODE_OPTIONS self, STR nm) {
 BOOL ret_val = BOOL_zero;
 STR L11;
 INT L21 = INT_zero;
 CHAR c = CHAR_zero;
 CHAR L31 = CHAR_zero;
 BOOL L4;
 CHAR L51_;
 CHAR L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 CHAR L101_;
 BOOL L12;
 BOOL L131_;
 CHAR L14;
 L51_=ARR((STR)nm,0); 
 L6 = L51_;
 L71_=L6=='$'; 
 L8 = L71_;
 L91_=!(L8); 
 if (L91_) {
  L101_=ARR((STR)nm,0); 
  L12 = CHAR_i629101715(L101_);
  L131_=!(L12); 
  L4 = L131_;
 } else {
  L4 = FALSE;
 }
 if (L4) {
  ret_val = FALSE;
  return ret_val;
 }
 {
  struct STR_el1843619312_frame_struct other1_0;
STR_el1843619312_frame noname1 = &other1_0;
  L11 = nm;
  L21 = 1;
  noname1->self = L11;
  noname1->arg1 = L21;
  noname1->state = 0;
  while (1) {
   L14 = STR_el1843619312(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   c = L14;
   L31 = c;
   switch (L31) {
    case 'A': 
     break;
    case 'B': 
     break;
    case 'C': 
     break;
    case 'D': 
     break;
    case 'E': 
     break;
    case 'F': 
     break;
    case 'G': 
     break;
    case 'H': 
     break;
    case 'I': 
     break;
    case 'J': 
     break;
    case 'K': 
     break;
    case 'L': 
     break;
    case 'M': 
     break;
    case 'N': 
     break;
    case 'O': 
     break;
    case 'P': 
     break;
    case 'Q': 
     break;
    case 'R': 
     break;
    case 'S': 
     break;
    case 'T': 
     break;
    case 'U': 
     break;
    case 'V': 
     break;
    case 'W': 
     break;
    case 'X': 
     break;
    case 'Y': 
     break;
    case 'Z': 
     break;
    case '0': 
     break;
    case '1': 
     break;
    case '2': 
     break;
    case '3': 
     break;
    case '4': 
     break;
    case '5': 
     break;
    case '6': 
     break;
    case '7': 
     break;
    case '8': 
     break;
    case '9': 
     break;
    case '_': 
     break;
    default: ;
     ret_val = FALSE;
     return ret_val;
   }
  }
 }
 after_loop: ;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTSTR CODE_O992729481(CODE_OPTIONS self, STR name111) {
 FLISTSTR ret_val;
 STR wd;
 FLISTSTR cl;
 BFILE f;
 FSTR fs;
 STR tok;
 INT pos = INT_zero;
 CHAR c = CHAR_zero;
 INT start1 = INT_zero;
 FLISTSTR create_self;
 FLISTSTR ret_val1;
 BFILE open_for_read_se;
 STR open_for_read_nm;
 BFILE ret_val2;
 BFILE r;
 BFILE error_self;
 BOOL ret_val3 = BOOL_zero;
 ERR1 create_self1;
 ERR1 ret_val4;
 ERR1 plus_self;
 dSTR plus_s;
 ERR1 ret_val5;
 FILE1 stderr_self;
 FILE1 ret_val6;
 FILE1 r1;
 ERR1 plus_self1;
 dSTR plus_s1;
 ERR1 ret_val7;
 FILE1 stderr_self1;
 FILE1 ret_val8;
 FILE1 r2;
 ERR1 plus_self2;
 dSTR plus_s2;
 FILE1 stderr_self2;
 FILE1 ret_val9;
 FILE1 r3;
 FSTR plus_self3;
 CHAR plus_c = CHAR_zero;
 FSTR ret_val10;
 BFILE close_self;
 FSTR aget_self;
 INT aget_ind = INT_zero;
 CHAR ret_val11 = CHAR_zero;
 FSTR aget_self1;
 INT aget_ind1 = INT_zero;
 CHAR ret_val12 = CHAR_zero;
 FSTR aget_self2;
 INT aget_ind2 = INT_zero;
 CHAR ret_val13 = CHAR_zero;
 FSTR aget_self3;
 INT aget_ind3 = INT_zero;
 CHAR ret_val14 = CHAR_zero;
 STR plus_self4;
 CHAR plus_arg = CHAR_zero;
 STR ret_val15;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val16;
 STR create_self2;
 CHAR create_c = CHAR_zero;
 STR ret_val17;
 STR r4;
 FSTR aget_self4;
 INT aget_ind4 = INT_zero;
 CHAR ret_val18 = CHAR_zero;
 ERR1 create_self3;
 ERR1 ret_val19;
 ERR1 plus_self5;
 dSTR plus_s3;
 ERR1 ret_val20;
 FILE1 stderr_self3;
 FILE1 ret_val21;
 FILE1 r5;
 ERR1 plus_self6;
 dSTR plus_s4;
 ERR1 ret_val22;
 FILE1 stderr_self4;
 FILE1 ret_val23;
 FILE1 r6;
 ERR1 plus_self7;
 dSTR plus_s5;
 FILE1 stderr_self5;
 FILE1 ret_val24;
 FILE1 r7;
 FSTR aget_self5;
 INT aget_ind5 = INT_zero;
 CHAR ret_val25 = CHAR_zero;
 FSTR aget_self6;
 INT aget_ind6 = INT_zero;
 CHAR ret_val26 = CHAR_zero;
 STR plus_self8;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val27;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val28;
 STR create_self4;
 CHAR create_c1 = CHAR_zero;
 STR ret_val29;
 STR r8;
 ERR1 create_self5;
 ERR1 ret_val30;
 ERR1 plus_self9;
 dSTR plus_s6;
 ERR1 ret_val31;
 FILE1 stderr_self6;
 FILE1 ret_val32;
 FILE1 r9;
 ERR1 plus_self10;
 dSTR plus_s7;
 ERR1 ret_val33;
 FILE1 stderr_self7;
 FILE1 ret_val34;
 FILE1 r10;
 ERR1 plus_self11;
 dSTR plus_s8;
 FILE1 stderr_self8;
 FILE1 ret_val35;
 FILE1 r11;
 FSTR aget_self7;
 INT aget_ind7 = INT_zero;
 CHAR ret_val36 = CHAR_zero;
 FSTR str_self2;
 STR ret_val37;
 FSTR aget_self8;
 INT aget_ind8 = INT_zero;
 CHAR ret_val38 = CHAR_zero;
 FSTR aget_self9;
 INT aget_ind9 = INT_zero;
 CHAR ret_val39 = CHAR_zero;
 STR plus_self12;
 STR plus_sarg;
 STR ret_val40;
 FSTR aget_self10;
 INT aget_ind10 = INT_zero;
 CHAR ret_val41 = CHAR_zero;
 FSTR aget_self11;
 INT aget_ind11 = INT_zero;
 CHAR ret_val42 = CHAR_zero;
 STR plus_self13;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val43;
 STR s2;
 CHAR str_self3 = CHAR_zero;
 STR ret_val44;
 STR create_self6;
 CHAR create_c2 = CHAR_zero;
 STR ret_val45;
 STR r12;
 STR plus_self14;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val46;
 STR s3;
 CHAR str_self4 = CHAR_zero;
 STR ret_val47;
 STR create_self7;
 CHAR create_c3 = CHAR_zero;
 STR ret_val48;
 STR r13;
 STR plus_self15;
 CHAR plus_arg4 = CHAR_zero;
 STR ret_val49;
 STR s4;
 CHAR str_self5 = CHAR_zero;
 STR ret_val50;
 STR create_self8;
 CHAR create_c4 = CHAR_zero;
 STR ret_val51;
 STR r14;
 STR plus_self16;
 CHAR plus_arg5 = CHAR_zero;
 STR ret_val52;
 STR s5;
 CHAR str_self6 = CHAR_zero;
 STR ret_val53;
 STR create_self9;
 CHAR create_c5 = CHAR_zero;
 STR ret_val54;
 STR r15;
 STR plus_self17;
 STR plus_sarg1;
 STR ret_val55;
 BFILE L1;
 OB L2;
 extern STR rb;
 STR L3;
 STR L4;
 BOOL L5;
 extern STR Couldntopenfile;
 FILE1 L6;
 OB L7;
 FILE1 L9;
 OB L10;
 dSTR L12;
 OB L13;
 FILE1 L14;
 OB L15;
 extern STR name1;
 INT L17;
 BOOL L181_;
 BOOL L19;
 BOOL L201_;
 CHAR L211_;
 BOOL L221_;
 INT L231_;
 CHAR L241_;
 CHAR L25;
 BOOL L261_;
 INT L271_;
 BOOL L28;
 BOOL L29;
 INT L30;
 BOOL L311_;
 BOOL L32;
 BOOL L331_;
 CHAR L341_;
 CHAR L35;
 BOOL L361_;
 CHAR L371_;
 CHAR L38;
 BOOL L391_;
 STR L40;
 INT L41;
 OB L42;
 INT L43;
 BOOL L451_;
 INT L461_;
 CHAR L471_;
 CHAR L48;
 BOOL L491_;
 INT L501_;
 INT L51;
 INT L521_;
 INT L53;
 BOOL L541_;
 BOOL L55;
 BOOL L561_;
 extern STR Unterm820051874;
 FILE1 L57;
 OB L58;
 FILE1 L60;
 OB L61;
 dSTR L63;
 OB L64;
 FILE1 L65;
 OB L66;
 BOOL L68;
 CHAR L691_;
 CHAR L70;
 BOOL L711_;
 INT L721_;
 CHAR L731_;
 CHAR L74;
 BOOL L751_;
 INT L761_;
 STR L77;
 INT L78;
 OB L79;
 INT L80;
 BOOL L831_;
 INT L841_;
 INT L85;
 INT L861_;
 INT L87;
 BOOL L881_;
 BOOL L89;
 BOOL L901_;
 extern STR Unterm919914180;
 FILE1 L91;
 OB L92;
 FILE1 L94;
 OB L95;
 dSTR L97;
 OB L98;
 FILE1 L99;
 OB L100;
 CHAR L1021_;
 CHAR L103;
 BOOL L1041_;
 INT L1051_;
 INT L106;
 INT L1071_;
 extern STR name1;
 INT L1081_;
 BOOL L1091_;
 BOOL L110;
 INT L1121_;
 CHAR L1131_;
 CHAR L114;
 BOOL L1151_;
 INT L1161_;
 CHAR L1171_;
 CHAR L118;
 BOOL L1191_;
 INT* L120;
 STR L121;
 BOOL L122;
 BOOL L1231_;
 BOOL L124;
 CHAR L1251_;
 BOOL L126;
 BOOL L1271_;
 INT L128;
 BOOL L1291_;
 CHAR L1301_;
 STR L131;
 INT L132;
 OB L133;
 INT L134;
 INT L1361_;
 extern STR name1;
 STR L137;
 INT L138;
 OB L139;
 INT L140;
 BOOL L142;
 BOOL L1431_;
 STR L144;
 INT L145;
 OB L146;
 INT L147;
 extern STR name1;
 BOOL L149;
 BOOL L1501_;
 BOOL L151;
 CHAR L1521_;
 CHAR L153;
 BOOL L1541_;
 BOOL L155;
 BOOL L1561_;
 STR L157;
 INT L158;
 OB L159;
 INT L160;
 extern STR name1;
 INT L1631_;
 wd = CODE_O1955258734(self, name111);
 create_self = ((FLISTSTR) NULL);
 ret_val1 = ((FLISTSTR) NULL);
 cl = ret_val1;
 open_for_read_se = ((BFILE) NULL);
 open_for_read_nm = name111;
 L2=ZALLOC(sizeof(struct BFILE_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=BFILE_tag;
 L1 = ((BFILE) L2);
 r = L1;
 L3 = open_for_read_nm;
 L4 = ((STR) &rb);
 SATTR(r,fp,fopen(((L3==NULL)?NULL:L3->arr_part), ((L4==NULL)?NULL:L4->arr_part)));
 ret_val2 = r;
 f = ret_val2;
 error_self = f;
 if ((ATTR(error_self,fp)==((EXT_OB) NULL))) {
  L5 = TRUE;
 } else {
  L5 = ferror(ATTR(error_self,fp));
 }
 ret_val3 = L5;
 if (ret_val3) {
  create_self1 = ((ERR1) NULL);
  ret_val4 = create_self1;
  plus_self = ret_val4;
  plus_s = ((dSTR) ((STR) &Couldntopenfile));
  stderr_self = ((FILE1) NULL);
  L7=ZALLOC(sizeof(struct FILE1_struct));
  if (L7==NULL) FATAL("Unable to allocate more memory");
  ((OB)L7)->header.tag=FILE1_tag;
  L6 = ((FILE1) L7);
  r1 = L6;
  
  SATTR(r1,fp,stderr);
  ret_val6 = r1;
  FILE_plus_dSTR(ret_val6, plus_s);
  ret_val5 = plus_self;
  plus_self1 = ret_val5;
  plus_s1 = ((dSTR) name111);
  stderr_self1 = ((FILE1) NULL);
  L10=ZALLOC(sizeof(struct FILE1_struct));
  if (L10==NULL) FATAL("Unable to allocate more memory");
  ((OB)L10)->header.tag=FILE1_tag;
  L9 = ((FILE1) L10);
  r2 = L9;
  
  SATTR(r2,fp,stderr);
  ret_val8 = r2;
  FILE_plus_dSTR(ret_val8, plus_s1);
  ret_val7 = plus_self1;
  plus_self2 = ret_val7;
  L13=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
  if (L13==NULL) FATAL("Unable to allocate more memory");
  memset(L13,0,sizeof(struct CHAR_boxed_struct));
  ((OB)L13)->header.tag=CHAR_tag;
  L12 = (dSTR)((CHAR_boxed) L13);
  ((CHAR_boxed) L12)->immutable_part = '\n';
  plus_s2 = L12;
  stderr_self2 = ((FILE1) NULL);
  L15=ZALLOC(sizeof(struct FILE1_struct));
  if (L15==NULL) FATAL("Unable to allocate more memory");
  ((OB)L15)->header.tag=FILE1_tag;
  L14 = ((FILE1) L15);
  r3 = L14;
  
  SATTR(r3,fp,stderr);
  ret_val9 = r3;
  FILE_plus_dSTR(ret_val9, plus_s2);
  CODE_O1762472157(self);
 }
 plus_self3 = BFILE_fstrrFSTR(f);
 plus_c = ' ';
 ret_val10 = FSTR_p399773021(plus_self3, plus_c);
 fs = ret_val10;
 close_self = f;
 fclose(ATTR(close_self,fp));
 tok = ((STR) &name1);
 pos = 0;
 while (1) {
  L17 = FSTR_sizerINT(fs);
  L181_=(pos)<(L17); 
  L19 = L181_;
  L201_=!(L19); 
  if (L201_) {
   goto after_loop;
  }
  aget_self = fs;
  aget_ind = pos;
  L211_=ARR((FSTR)aget_self,aget_ind); 
  ret_val11 = L211_;
  c = ret_val11;
  L221_=c=='-'; 
  if (L221_) {
   aget_self1 = fs;
   L231_=INTPLUS(pos,1); 
   aget_ind1 = L231_;
   L241_=ARR((FSTR)aget_self1,aget_ind1); 
   ret_val12 = L241_;
   L25 = ret_val12;
   L261_=L25=='-'; 
   if (L261_) {
    while (1) {
     L271_=INTPLUS(pos,1); 
     pos = L271_;
     L30 = FSTR_sizerINT(fs);
     L311_=(pos)<(L30); 
     L32 = L311_;
     L331_=!(L32); 
     if (L331_) {
      L29 = TRUE;
     } else {
      aget_self2 = fs;
      aget_ind2 = pos;
      L341_=ARR((FSTR)aget_self2,aget_ind2); 
      ret_val13 = L341_;
      L35 = ret_val13;
      L361_=L35=='\n'; 
      L29 = L361_;
     }
     if (L29) {
      L28 = TRUE;
     } else {
      aget_self3 = fs;
      aget_ind3 = pos;
      L371_=ARR((FSTR)aget_self3,aget_ind3); 
      ret_val14 = L371_;
      L38 = ret_val14;
      L391_=L38=='\r'; 
      L28 = L391_;
     }
     if (L28) {
      goto after_loop1;
     }
    }
    after_loop1: ;
   }
   else {
    plus_self4 = tok;
    plus_arg = '-';
    str_self = plus_arg;
    create_self2 = ((STR) NULL);
    create_c = str_self;
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
    r4 = L40;
    SARR((STR)r4,0,create_c); 
    ;
    ret_val17 = r4;
    ret_val16 = ret_val17;
    s = ret_val16;
    ret_val15 = STR_ap1077157958(plus_self4, s, TRUE);
    tok = ret_val15;
   }
  }
  else {
   L451_=c=='('; 
   if (L451_) {
    aget_self4 = fs;
    L461_=INTPLUS(pos,1); 
    aget_ind4 = L461_;
    L471_=ARR((FSTR)aget_self4,aget_ind4); 
    ret_val18 = L471_;
    L48 = ret_val18;
    L491_=L48=='*'; 
    if (L491_) {
     while (1) {
      L501_=INTPLUS(pos,1); 
      pos = L501_;
      L51 = FSTR_sizerINT(fs);
      L521_=INTMINUS(L51,1); 
      L53 = L521_;
      L541_=(pos)<(L53); 
      L55 = L541_;
      L561_=!(L55); 
      if (L561_) {
       create_self3 = ((ERR1) NULL);
       ret_val19 = create_self3;
       plus_self5 = ret_val19;
       plus_s3 = ((dSTR) ((STR) &Unterm820051874));
       stderr_self3 = ((FILE1) NULL);
       L58=ZALLOC(sizeof(struct FILE1_struct));
       if (L58==NULL) FATAL("Unable to allocate more memory");
       ((OB)L58)->header.tag=FILE1_tag;
       L57 = ((FILE1) L58);
       r5 = L57;
       
       SATTR(r5,fp,stderr);
       ret_val21 = r5;
       FILE_plus_dSTR(ret_val21, plus_s3);
       ret_val20 = plus_self5;
       plus_self6 = ret_val20;
       plus_s4 = ((dSTR) name111);
       stderr_self4 = ((FILE1) NULL);
       L61=ZALLOC(sizeof(struct FILE1_struct));
       if (L61==NULL) FATAL("Unable to allocate more memory");
       ((OB)L61)->header.tag=FILE1_tag;
       L60 = ((FILE1) L61);
       r6 = L60;
       
       SATTR(r6,fp,stderr);
       ret_val23 = r6;
       FILE_plus_dSTR(ret_val23, plus_s4);
       ret_val22 = plus_self6;
       plus_self7 = ret_val22;
       L64=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
       if (L64==NULL) FATAL("Unable to allocate more memory");
       memset(L64,0,sizeof(struct CHAR_boxed_struct));
       ((OB)L64)->header.tag=CHAR_tag;
       L63 = (dSTR)((CHAR_boxed) L64);
       ((CHAR_boxed) L63)->immutable_part = '\n';
       plus_s5 = L63;
       stderr_self5 = ((FILE1) NULL);
       L66=ZALLOC(sizeof(struct FILE1_struct));
       if (L66==NULL) FATAL("Unable to allocate more memory");
       ((OB)L66)->header.tag=FILE1_tag;
       L65 = ((FILE1) L66);
       r7 = L65;
       
       SATTR(r7,fp,stderr);
       ret_val24 = r7;
       FILE_plus_dSTR(ret_val24, plus_s5);
       goto after_loop2;
      }
      aget_self5 = fs;
      aget_ind5 = pos;
      L691_=ARR((FSTR)aget_self5,aget_ind5); 
      ret_val25 = L691_;
      L70 = ret_val25;
      L711_=L70=='*'; 
      if (L711_) {
       aget_self6 = fs;
       L721_=INTPLUS(pos,1); 
       aget_ind6 = L721_;
       L731_=ARR((FSTR)aget_self6,aget_ind6); 
       ret_val26 = L731_;
       L74 = ret_val26;
       L751_=L74==')'; 
       L68 = L751_;
      } else {
       L68 = FALSE;
      }
      if (L68) {
       goto after_loop2;
      }
     }
     after_loop2: ;
     L761_=INTPLUS(pos,1); 
     pos = L761_;
    }
    else {
     plus_self8 = tok;
     plus_arg1 = '(';
     str_self1 = plus_arg1;
     create_self4 = ((STR) NULL);
     create_c1 = str_self1;
     L78 = 1;
     L80=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L78)*sizeof(CHAR);
     L79=ZALLOC_LEAF_BIG(L80);
     if (L79==NULL) FATAL("Unable to allocate more memory");
     memset(L79,0,L80);
     ((OB)L79)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L79)->header.destroyed=0;
#endif

     L77 = ((STR) L79);
     L77->asize = L78;
     r8 = L77;
     SARR((STR)r8,0,create_c1); 
     ;
     ret_val29 = r8;
     ret_val28 = ret_val29;
     s1 = ret_val28;
     ret_val27 = STR_ap1077157958(plus_self8, s1, TRUE);
     tok = ret_val27;
    }
   }
   else {
    L831_=c=='\"'; 
    if (L831_) {
     start1 = pos;
     while (1) {
      L841_=INTPLUS(pos,1); 
      pos = L841_;
      L85 = FSTR_sizerINT(fs);
      L861_=INTMINUS(L85,1); 
      L87 = L861_;
      L881_=(pos)<(L87); 
      L89 = L881_;
      L901_=!(L89); 
      if (L901_) {
       create_self5 = ((ERR1) NULL);
       ret_val30 = create_self5;
       plus_self9 = ret_val30;
       plus_s6 = ((dSTR) ((STR) &Unterm919914180));
       stderr_self6 = ((FILE1) NULL);
       L92=ZALLOC(sizeof(struct FILE1_struct));
       if (L92==NULL) FATAL("Unable to allocate more memory");
       ((OB)L92)->header.tag=FILE1_tag;
       L91 = ((FILE1) L92);
       r9 = L91;
       
       SATTR(r9,fp,stderr);
       ret_val32 = r9;
       FILE_plus_dSTR(ret_val32, plus_s6);
       ret_val31 = plus_self9;
       plus_self10 = ret_val31;
       plus_s7 = ((dSTR) name111);
       stderr_self7 = ((FILE1) NULL);
       L95=ZALLOC(sizeof(struct FILE1_struct));
       if (L95==NULL) FATAL("Unable to allocate more memory");
       ((OB)L95)->header.tag=FILE1_tag;
       L94 = ((FILE1) L95);
       r10 = L94;
       
       SATTR(r10,fp,stderr);
       ret_val34 = r10;
       FILE_plus_dSTR(ret_val34, plus_s7);
       ret_val33 = plus_self10;
       plus_self11 = ret_val33;
       L98=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
       if (L98==NULL) FATAL("Unable to allocate more memory");
       memset(L98,0,sizeof(struct CHAR_boxed_struct));
       ((OB)L98)->header.tag=CHAR_tag;
       L97 = (dSTR)((CHAR_boxed) L98);
       ((CHAR_boxed) L97)->immutable_part = '\n';
       plus_s8 = L97;
       stderr_self8 = ((FILE1) NULL);
       L100=ZALLOC(sizeof(struct FILE1_struct));
       if (L100==NULL) FATAL("Unable to allocate more memory");
       ((OB)L100)->header.tag=FILE1_tag;
       L99 = ((FILE1) L100);
       r11 = L99;
       
       SATTR(r11,fp,stderr);
       ret_val35 = r11;
       FILE_plus_dSTR(ret_val35, plus_s8);
       goto after_loop3;
      }
      aget_self7 = fs;
      aget_ind7 = pos;
      L1021_=ARR((FSTR)aget_self7,aget_ind7); 
      ret_val36 = L1021_;
      L103 = ret_val36;
      L1041_=L103=='\"'; 
      if (L1041_) {
       goto after_loop3;
      }
     }
     after_loop3: ;
     L1051_=INTMINUS(pos,start1); 
     L106 = L1051_;
     L1071_=INTPLUS(L106,1); 
     str_self2 = FSTR_s1920457899(fs, start1, L1071_);
     ret_val37 = STR_fr1097270334(((STR) NULL), str_self2);
     tok = ret_val37;
     tok = STR_as1755591216(tok);
     tok = CODE_O218236968(self, name111, ((dSTR) tok));
     cl = FLISTS1425610062(cl, tok);
     tok = ((STR) &name1);
     L1081_=INTPLUS(pos,1); 
     pos = L1081_;
    }
    else {
     L1091_=c=='$'; 
     if (L1091_) {
      aget_self8 = fs;
      L1121_=INTPLUS(pos,1); 
      aget_ind8 = L1121_;
      L1131_=ARR((FSTR)aget_self8,aget_ind8); 
      ret_val38 = L1131_;
      L114 = ret_val38;
      L1151_=L114=='('; 
      if (L1151_) {
       L110 = TRUE;
      } else {
       aget_self9 = fs;
       L1161_=INTPLUS(pos,1); 
       aget_ind9 = L1161_;
       L1171_=ARR((FSTR)aget_self9,aget_ind9); 
       ret_val39 = L1171_;
       L118 = ret_val39;
       L1191_=L118=='{'; 
       L110 = L1191_;
      }
      if (L110) {
       plus_self12 = tok;
       L120 = &pos;
       L121 = CODE_O1483684745(self, name111, ((dSTR) fs), L120);
       plus_sarg = L121;
       ret_val40 = STR_ap2004550586(plus_self12, plus_sarg);
       tok = ret_val40;
       L122 = (tok==((STR) NULL));
       L1231_=!(L122); 
       if (L1231_) {
        while (1) {
         aget_self10 = fs;
         aget_ind10 = pos;
         L1251_=ARR((FSTR)aget_self10,aget_ind10); 
         ret_val41 = L1251_;
         L126 = CHAR_i84735023(ret_val41);
         L1271_=!(L126); 
         if (L1271_) {
          L128 = FSTR_sizerINT(fs);
          L1291_=(pos)<(L128); 
          L124 = L1291_;
         } else {
          L124 = FALSE;
         }
         if (L124) {
         }
         else {
          goto after_loop4;
         }
         plus_self13 = tok;
         aget_self11 = fs;
         aget_ind11 = pos;
         L1301_=ARR((FSTR)aget_self11,aget_ind11); 
         ret_val42 = L1301_;
         plus_arg2 = ret_val42;
         str_self3 = plus_arg2;
         create_self6 = ((STR) NULL);
         create_c2 = str_self3;
         L132 = 1;
         L134=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L132)*sizeof(CHAR);
         L133=ZALLOC_LEAF_BIG(L134);
         if (L133==NULL) FATAL("Unable to allocate more memory");
         memset(L133,0,L134);
         ((OB)L133)->header.tag=STR_tag;
#ifdef DESTROY_CHK

           ((OB)L133)->header.destroyed=0;
#endif

         L131 = ((STR) L133);
         L131->asize = L132;
         r12 = L131;
         SARR((STR)r12,0,create_c2); 
         ;
         ret_val45 = r12;
         ret_val44 = ret_val45;
         s2 = ret_val44;
         ret_val43 = STR_ap1077157958(plus_self13, s2, TRUE);
         tok = ret_val43;
         L1361_=INTPLUS(pos,1); 
         pos = L1361_;
        }
        after_loop4: ;
        cl = FLISTS1425610062(cl, tok);
       }
       tok = ((STR) &name1);
      }
      else {
       plus_self14 = tok;
       plus_arg3 = '$';
       str_self4 = plus_arg3;
       create_self7 = ((STR) NULL);
       create_c3 = str_self4;
       L138 = 1;
       L140=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L138)*sizeof(CHAR);
       L139=ZALLOC_LEAF_BIG(L140);
       if (L139==NULL) FATAL("Unable to allocate more memory");
       memset(L139,0,L140);
       ((OB)L139)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L139)->header.destroyed=0;
#endif

       L137 = ((STR) L139);
       L137->asize = L138;
       r13 = L137;
       SARR((STR)r13,0,create_c3); 
       ;
       ret_val48 = r13;
       ret_val47 = ret_val48;
       s3 = ret_val47;
       ret_val46 = STR_ap1077157958(plus_self14, s3, TRUE);
       tok = ret_val46;
      }
     }
     else {
      L142 = CHAR_i84735023(c);
      L1431_=!(L142); 
      if (L1431_) {
       plus_self15 = tok;
       plus_arg4 = c;
       str_self5 = plus_arg4;
       create_self8 = ((STR) NULL);
       create_c4 = str_self5;
       L145 = 1;
       L147=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L145)*sizeof(CHAR);
       L146=ZALLOC_LEAF_BIG(L147);
       if (L146==NULL) FATAL("Unable to allocate more memory");
       memset(L146,0,L147);
       ((OB)L146)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L146)->header.destroyed=0;
#endif

       L144 = ((STR) L146);
       L144->asize = L145;
       r14 = L144;
       SARR((STR)r14,0,create_c4); 
       ;
       ret_val51 = r14;
       ret_val50 = ret_val51;
       s4 = ret_val50;
       ret_val49 = STR_ap1077157958(plus_self15, s4, TRUE);
       tok = ret_val49;
      }
      else {
       L149 = STR_is111530248(tok, ((STR) &name1));
       L1501_=!(L149); 
       if (L1501_) {
        if (CODE_O1472228953(self, tok)) {
         L1521_=ARR((STR)tok,0); 
         L153 = L1521_;
         L1541_=L153=='/'; 
         L155 = L1541_;
         L1561_=!(L155); 
         L151 = L1561_;
        } else {
         L151 = FALSE;
        }
        if (L151) {
         plus_self16 = wd;
         plus_arg5 = '/';
         str_self6 = plus_arg5;
         create_self9 = ((STR) NULL);
         create_c5 = str_self6;
         L158 = 1;
         L160=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L158)*sizeof(CHAR);
         L159=ZALLOC_LEAF_BIG(L160);
         if (L159==NULL) FATAL("Unable to allocate more memory");
         memset(L159,0,L160);
         ((OB)L159)->header.tag=STR_tag;
#ifdef DESTROY_CHK

           ((OB)L159)->header.destroyed=0;
#endif

         L157 = ((STR) L159);
         L157->asize = L158;
         r15 = L157;
         SARR((STR)r15,0,create_c5); 
         ;
         ret_val54 = r15;
         ret_val53 = ret_val54;
         s5 = ret_val53;
         ret_val52 = STR_ap1077157958(plus_self16, s5, TRUE);
         plus_self17 = ret_val52;
         plus_sarg1 = tok;
         ret_val55 = STR_ap2004550586(plus_self17, plus_sarg1);
         tok = ret_val55;
        }
        if (CODE_O1472228953(self, tok)) {
         SATTR(self,module_inclusion,FMAPST859022540(ATTR(self,module_inclusion), tok, name111));
        }
        cl = FLISTS1425610062(cl, tok);
        tok = ((STR) &name1);
       }
      }
     }
    }
   }
  }
  L1631_=INTPLUS(pos,1); 
  pos = L1631_;
 }
 after_loop: ;
 ret_val = cl;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CODE_O1099966235(CODE_OPTIONS self) {
 STR ret_val;
 STR r = ((STR) NULL);
 STR s = ((STR) NULL);
 FSETSTR L11;
 FSETSTR L21;
 FSETSTR L31;
 FSETSTR L41;
 FSETSTR L51;
 FSETSTR L61;
 FSETSTR L71;
 FSETSTR L81;
 FSETSTR L91;
 FSETSTR L101;
 FSETSTR L121;
 FSETSTR L131;
 FSETSTR L141;
 FSETSTR L151;
 FSETSTR L161;
 FSETSTR L171;
 FSETSTR L181;
 FSETSTR L191;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 CHAR plus_arg = CHAR_zero;
 STR ret_val2;
 STR s1;
 CHAR str_self = CHAR_zero;
 STR ret_val3;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val4;
 STR r1;
 STR plus_self2;
 STR plus_sarg1;
 STR ret_val5;
 STR plus_self3;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val6;
 STR s2;
 CHAR str_self1 = CHAR_zero;
 STR ret_val7;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val8;
 STR r2;
 STR plus_self4;
 STR plus_sarg2;
 STR ret_val9;
 STR plus_self5;
 STR plus_sarg3;
 STR ret_val10;
 STR plus_self6;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val11;
 STR s3;
 CHAR str_self2 = CHAR_zero;
 STR ret_val12;
 STR create_self2;
 CHAR create_c2 = CHAR_zero;
 STR ret_val13;
 STR r3;
 STR plus_self7;
 STR plus_sarg4;
 STR ret_val14;
 STR plus_self8;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val15;
 STR s4;
 CHAR str_self3 = CHAR_zero;
 STR ret_val16;
 STR create_self3;
 CHAR create_c3 = CHAR_zero;
 STR ret_val17;
 STR r4;
 STR plus_self9;
 STR plus_sarg5;
 STR ret_val18;
 STR plus_self10;
 STR plus_sarg6;
 STR ret_val19;
 STR plus_self11;
 CHAR plus_arg4 = CHAR_zero;
 STR ret_val20;
 STR s5;
 CHAR str_self4 = CHAR_zero;
 STR ret_val21;
 STR create_self4;
 CHAR create_c4 = CHAR_zero;
 STR ret_val22;
 STR r5;
 STR plus_self12;
 STR plus_sarg7;
 STR ret_val23;
 STR plus_self13;
 CHAR plus_arg5 = CHAR_zero;
 STR ret_val24;
 STR s6;
 CHAR str_self5 = CHAR_zero;
 STR ret_val25;
 STR create_self5;
 CHAR create_c5 = CHAR_zero;
 STR ret_val26;
 STR r6;
 STR plus_self14;
 STR plus_sarg8;
 STR ret_val27;
 STR plus_self15;
 STR plus_sarg9;
 STR ret_val28;
 STR plus_self16;
 CHAR plus_arg6 = CHAR_zero;
 STR ret_val29;
 STR s7;
 CHAR str_self6 = CHAR_zero;
 STR ret_val30;
 STR create_self6;
 CHAR create_c6 = CHAR_zero;
 STR ret_val31;
 STR r7;
 STR plus_self17;
 STR plus_sarg10;
 STR ret_val32;
 STR plus_self18;
 CHAR plus_arg7 = CHAR_zero;
 STR ret_val33;
 STR s8;
 CHAR str_self7 = CHAR_zero;
 STR ret_val34;
 STR create_self7;
 CHAR create_c7 = CHAR_zero;
 STR ret_val35;
 STR r8;
 STR plus_self19;
 STR plus_sarg11;
 STR ret_val36;
 STR plus_self20;
 STR plus_sarg12;
 STR ret_val37;
 STR plus_self21;
 CHAR plus_arg8 = CHAR_zero;
 STR ret_val38;
 STR s9;
 CHAR str_self8 = CHAR_zero;
 STR ret_val39;
 STR create_self8;
 CHAR create_c8 = CHAR_zero;
 STR ret_val40;
 STR r9;
 STR plus_self22;
 STR plus_sarg13;
 STR ret_val41;
 STR plus_self23;
 CHAR plus_arg9 = CHAR_zero;
 STR ret_val42;
 STR s10;
 CHAR str_self9 = CHAR_zero;
 STR ret_val43;
 STR create_self9;
 CHAR create_c9 = CHAR_zero;
 STR ret_val44;
 STR r10;
 STR plus_self24;
 STR plus_sarg14;
 STR ret_val45;
 STR plus_self25;
 STR plus_sarg15;
 STR ret_val46;
 STR plus_self26;
 CHAR plus_arg10 = CHAR_zero;
 STR ret_val47;
 STR s11;
 CHAR str_self10 = CHAR_zero;
 STR ret_val48;
 STR create_self10;
 CHAR create_c10 = CHAR_zero;
 STR ret_val49;
 STR r11;
 STR plus_self27;
 STR plus_sarg16;
 STR ret_val50;
 STR plus_self28;
 CHAR plus_arg11 = CHAR_zero;
 STR ret_val51;
 STR s12;
 CHAR str_self11 = CHAR_zero;
 STR ret_val52;
 STR create_self11;
 CHAR create_c11 = CHAR_zero;
 STR ret_val53;
 STR r12;
 STR plus_self29;
 STR plus_sarg17;
 STR ret_val54;
 STR plus_self30;
 STR plus_sarg18;
 STR ret_val55;
 STR plus_self31;
 CHAR plus_arg12 = CHAR_zero;
 STR ret_val56;
 STR s13;
 CHAR str_self12 = CHAR_zero;
 STR ret_val57;
 STR create_self12;
 CHAR create_c12 = CHAR_zero;
 STR ret_val58;
 STR r13;
 STR plus_self32;
 STR plus_sarg19;
 STR ret_val59;
 STR plus_self33;
 CHAR plus_arg13 = CHAR_zero;
 STR ret_val60;
 STR s14;
 CHAR str_self13 = CHAR_zero;
 STR ret_val61;
 STR create_self13;
 CHAR create_c13 = CHAR_zero;
 STR ret_val62;
 STR r14;
 STR plus_self34;
 STR plus_sarg20;
 STR ret_val63;
 STR plus_self35;
 STR plus_sarg21;
 STR ret_val64;
 STR plus_self36;
 CHAR plus_arg14 = CHAR_zero;
 STR ret_val65;
 STR s15;
 CHAR str_self14 = CHAR_zero;
 STR ret_val66;
 STR create_self14;
 CHAR create_c14 = CHAR_zero;
 STR ret_val67;
 STR r15;
 STR plus_self37;
 STR plus_sarg22;
 STR ret_val68;
 STR plus_self38;
 CHAR plus_arg15 = CHAR_zero;
 STR ret_val69;
 STR s16;
 CHAR str_self15 = CHAR_zero;
 STR ret_val70;
 STR create_self15;
 CHAR create_c15 = CHAR_zero;
 STR ret_val71;
 STR r16;
 STR plus_self39;
 STR plus_sarg23;
 STR ret_val72;
 STR plus_self40;
 STR plus_sarg24;
 STR ret_val73;
 STR plus_self41;
 CHAR plus_arg16 = CHAR_zero;
 STR ret_val74;
 STR s17;
 CHAR str_self16 = CHAR_zero;
 STR ret_val75;
 STR create_self16;
 CHAR create_c16 = CHAR_zero;
 STR ret_val76;
 STR r17;
 STR plus_self42;
 STR plus_sarg25;
 STR ret_val77;
 STR plus_self43;
 CHAR plus_arg17 = CHAR_zero;
 STR ret_val78;
 STR s18;
 CHAR str_self17 = CHAR_zero;
 STR ret_val79;
 STR create_self17;
 CHAR create_c17 = CHAR_zero;
 STR ret_val80;
 STR r18;
 STR plus_self44;
 STR plus_sarg26;
 STR ret_val81;
 extern STR name1;
 extern STR All;
 STR L20;
 INT L22;
 OB L23;
 INT L24;
 STR L26;
 STR L27;
 INT L28;
 OB L29;
 INT L30;
 STR L33;
 extern STR Preconditions;
 extern STR name1;
 extern STR All;
 STR L34;
 INT L35;
 OB L36;
 INT L37;
 STR L39;
 STR L40;
 INT L42;
 OB L43;
 INT L44;
 STR L46;
 extern STR Postconditions;
 extern STR name1;
 extern STR All;
 STR L47;
 INT L48;
 OB L49;
 INT L50;
 STR L53;
 STR L54;
 INT L55;
 OB L56;
 INT L57;
 STR L59;
 extern STR Invariants;
 extern STR name1;
 extern STR All;
 STR L60;
 INT L62;
 OB L63;
 INT L64;
 STR L66;
 STR L67;
 INT L68;
 OB L69;
 INT L70;
 STR L73;
 extern STR Asserts;
 extern STR name1;
 extern STR All;
 STR L74;
 INT L75;
 OB L76;
 INT L77;
 STR L79;
 STR L80;
 INT L82;
 OB L83;
 INT L84;
 STR L86;
 extern STR Voidchecks;
 extern STR name1;
 extern STR All;
 STR L87;
 INT L88;
 OB L89;
 INT L90;
 STR L93;
 STR L94;
 INT L95;
 OB L96;
 INT L97;
 STR L99;
 extern STR Whenclause;
 extern STR name1;
 extern STR All;
 STR L100;
 INT L102;
 OB L103;
 INT L104;
 STR L106;
 STR L107;
 INT L108;
 OB L109;
 INT L110;
 STR L112;
 extern STR Bounds;
 extern STR On;
 extern STR Off;
 extern STR Destroy;
 extern STR On;
 extern STR Off;
 extern STR pSatherchecks;
 extern STR On;
 extern STR Off;
 extern STR Statis1655725615;
 extern STR On;
 extern STR Off;
 extern STR pSatherstats;
 extern STR On;
 extern STR Off;
 extern STR pSathertrace;
 extern STR name1;
 extern STR All;
 STR L113;
 INT L114;
 OB L115;
 INT L116;
 STR L118;
 STR L119;
 INT L120;
 OB L122;
 INT L123;
 STR L125;
 extern STR Arith;
 extern STR name1;
 extern STR All;
 STR L126;
 INT L127;
 OB L128;
 INT L129;
 STR L132;
 STR L133;
 INT L134;
 OB L135;
 INT L136;
 STR L138;
 extern STR Return;
 s = CODE_O346288391(self);
 r = ((STR) &name1);
 if (ATTR(self,pre_all)) {
  plus_self = r;
  plus_sarg = ((STR) &All);
  ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
  r = ret_val1;
 }
 else {
  {
   struct FSETSTR_eltbrSTR_frame_struct other1_0;
FSETSTR_eltbrSTR_frame noname1 = &other1_0;
   noname1->state = 0;
   while (1) {
    plus_self1 = r;
    plus_arg = '+';
    str_self = plus_arg;
    create_self = ((STR) NULL);
    create_c = str_self;
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
    SARR((STR)r1,0,create_c); 
    ;
    ret_val4 = r1;
    ret_val3 = ret_val4;
    s1 = ret_val3;
    ret_val2 = STR_ap1077157958(plus_self1, s1, TRUE);
    plus_self2 = ret_val2;
    if (noname1->state == 0) {
     L11 = ATTR(self,pre_in);
     noname1->self = L11;
    }
    L26 = FSETSTR_eltbrSTR(noname1);
    if (noname1->state == -1) {
     goto after_loop;
    }
    plus_sarg1 = L26;
    ret_val5 = STR_ap2004550586(plus_self2, plus_sarg1);
    r = ret_val5;
   }
  }
  after_loop: ;
 }
 {
  struct FSETSTR_eltbrSTR_frame_struct other2_0;
FSETSTR_eltbrSTR_frame noname2 = &other2_0;
  noname2->state = 0;
  while (1) {
   plus_self3 = r;
   plus_arg1 = '-';
   str_self1 = plus_arg1;
   create_self1 = ((STR) NULL);
   create_c1 = str_self1;
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
   SARR((STR)r2,0,create_c1); 
   ;
   ret_val8 = r2;
   ret_val7 = ret_val8;
   s2 = ret_val7;
   ret_val6 = STR_ap1077157958(plus_self3, s2, TRUE);
   plus_self4 = ret_val6;
   if (noname2->state == 0) {
    L21 = ATTR(self,pre_out);
    noname2->self = L21;
   }
   L33 = FSETSTR_eltbrSTR(noname2);
   if (noname2->state == -1) {
    goto after_loop1;
   }
   plus_sarg2 = L33;
   ret_val9 = STR_ap2004550586(plus_self4, plus_sarg2);
   r = ret_val9;
  }
 }
 after_loop1: ;
 s = CODE_O740953890(self, ((STR) &Preconditions), s, r);
 r = ((STR) &name1);
 if (ATTR(self,post_all)) {
  plus_self5 = r;
  plus_sarg3 = ((STR) &All);
  ret_val10 = STR_ap2004550586(plus_self5, plus_sarg3);
  r = ret_val10;
 }
 else {
  {
   struct FSETSTR_eltbrSTR_frame_struct other3_0;
FSETSTR_eltbrSTR_frame noname3 = &other3_0;
   noname3->state = 0;
   while (1) {
    plus_self6 = r;
    plus_arg2 = '+';
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
    r3 = L34;
    SARR((STR)r3,0,create_c2); 
    ;
    ret_val13 = r3;
    ret_val12 = ret_val13;
    s3 = ret_val12;
    ret_val11 = STR_ap1077157958(plus_self6, s3, TRUE);
    plus_self7 = ret_val11;
    if (noname3->state == 0) {
     L31 = ATTR(self,post_in);
     noname3->self = L31;
    }
    L39 = FSETSTR_eltbrSTR(noname3);
    if (noname3->state == -1) {
     goto after_loop2;
    }
    plus_sarg4 = L39;
    ret_val14 = STR_ap2004550586(plus_self7, plus_sarg4);
    r = ret_val14;
   }
  }
  after_loop2: ;
 }
 {
  struct FSETSTR_eltbrSTR_frame_struct other4_0;
FSETSTR_eltbrSTR_frame noname4 = &other4_0;
  noname4->state = 0;
  while (1) {
   plus_self8 = r;
   plus_arg3 = '-';
   str_self3 = plus_arg3;
   create_self3 = ((STR) NULL);
   create_c3 = str_self3;
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
   SARR((STR)r4,0,create_c3); 
   ;
   ret_val17 = r4;
   ret_val16 = ret_val17;
   s4 = ret_val16;
   ret_val15 = STR_ap1077157958(plus_self8, s4, TRUE);
   plus_self9 = ret_val15;
   if (noname4->state == 0) {
    L41 = ATTR(self,post_out);
    noname4->self = L41;
   }
   L46 = FSETSTR_eltbrSTR(noname4);
   if (noname4->state == -1) {
    goto after_loop3;
   }
   plus_sarg5 = L46;
   ret_val18 = STR_ap2004550586(plus_self9, plus_sarg5);
   r = ret_val18;
  }
 }
 after_loop3: ;
 s = CODE_O740953890(self, ((STR) &Postconditions), s, r);
 r = ((STR) &name1);
 if (ATTR(self,invariant_all)) {
  plus_self10 = r;
  plus_sarg6 = ((STR) &All);
  ret_val19 = STR_ap2004550586(plus_self10, plus_sarg6);
  r = ret_val19;
 }
 else {
  {
   struct FSETSTR_eltbrSTR_frame_struct other5_0;
FSETSTR_eltbrSTR_frame noname5 = &other5_0;
   noname5->state = 0;
   while (1) {
    plus_self11 = r;
    plus_arg4 = '+';
    str_self4 = plus_arg4;
    create_self4 = ((STR) NULL);
    create_c4 = str_self4;
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
    r5 = L47;
    SARR((STR)r5,0,create_c4); 
    ;
    ret_val22 = r5;
    ret_val21 = ret_val22;
    s5 = ret_val21;
    ret_val20 = STR_ap1077157958(plus_self11, s5, TRUE);
    plus_self12 = ret_val20;
    if (noname5->state == 0) {
     L51 = ATTR(self,invariant_in);
     noname5->self = L51;
    }
    L53 = FSETSTR_eltbrSTR(noname5);
    if (noname5->state == -1) {
     goto after_loop4;
    }
    plus_sarg7 = L53;
    ret_val23 = STR_ap2004550586(plus_self12, plus_sarg7);
    r = ret_val23;
   }
  }
  after_loop4: ;
 }
 {
  struct FSETSTR_eltbrSTR_frame_struct other6_0;
FSETSTR_eltbrSTR_frame noname6 = &other6_0;
  noname6->state = 0;
  while (1) {
   plus_self13 = r;
   plus_arg5 = '-';
   str_self5 = plus_arg5;
   create_self5 = ((STR) NULL);
   create_c5 = str_self5;
   L55 = 1;
   L57=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L55)*sizeof(CHAR);
   L56=ZALLOC_LEAF_BIG(L57);
   if (L56==NULL) FATAL("Unable to allocate more memory");
   memset(L56,0,L57);
   ((OB)L56)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L56)->header.destroyed=0;
#endif

   L54 = ((STR) L56);
   L54->asize = L55;
   r6 = L54;
   SARR((STR)r6,0,create_c5); 
   ;
   ret_val26 = r6;
   ret_val25 = ret_val26;
   s6 = ret_val25;
   ret_val24 = STR_ap1077157958(plus_self13, s6, TRUE);
   plus_self14 = ret_val24;
   if (noname6->state == 0) {
    L61 = ATTR(self,invariant_out);
    noname6->self = L61;
   }
   L59 = FSETSTR_eltbrSTR(noname6);
   if (noname6->state == -1) {
    goto after_loop5;
   }
   plus_sarg8 = L59;
   ret_val27 = STR_ap2004550586(plus_self14, plus_sarg8);
   r = ret_val27;
  }
 }
 after_loop5: ;
 s = CODE_O740953890(self, ((STR) &Invariants), s, r);
 r = ((STR) &name1);
 if (ATTR(self,assert_all)) {
  plus_self15 = r;
  plus_sarg9 = ((STR) &All);
  ret_val28 = STR_ap2004550586(plus_self15, plus_sarg9);
  r = ret_val28;
 }
 else {
  {
   struct FSETSTR_eltbrSTR_frame_struct other7_0;
FSETSTR_eltbrSTR_frame noname7 = &other7_0;
   noname7->state = 0;
   while (1) {
    plus_self16 = r;
    plus_arg6 = '+';
    str_self6 = plus_arg6;
    create_self6 = ((STR) NULL);
    create_c6 = str_self6;
    L62 = 1;
    L64=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L62)*sizeof(CHAR);
    L63=ZALLOC_LEAF_BIG(L64);
    if (L63==NULL) FATAL("Unable to allocate more memory");
    memset(L63,0,L64);
    ((OB)L63)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L63)->header.destroyed=0;
#endif

    L60 = ((STR) L63);
    L60->asize = L62;
    r7 = L60;
    SARR((STR)r7,0,create_c6); 
    ;
    ret_val31 = r7;
    ret_val30 = ret_val31;
    s7 = ret_val30;
    ret_val29 = STR_ap1077157958(plus_self16, s7, TRUE);
    plus_self17 = ret_val29;
    if (noname7->state == 0) {
     L71 = ATTR(self,assert_in);
     noname7->self = L71;
    }
    L66 = FSETSTR_eltbrSTR(noname7);
    if (noname7->state == -1) {
     goto after_loop6;
    }
    plus_sarg10 = L66;
    ret_val32 = STR_ap2004550586(plus_self17, plus_sarg10);
    r = ret_val32;
   }
  }
  after_loop6: ;
 }
 {
  struct FSETSTR_eltbrSTR_frame_struct other8_0;
FSETSTR_eltbrSTR_frame noname8 = &other8_0;
  noname8->state = 0;
  while (1) {
   plus_self18 = r;
   plus_arg7 = '-';
   str_self7 = plus_arg7;
   create_self7 = ((STR) NULL);
   create_c7 = str_self7;
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
   r8 = L67;
   SARR((STR)r8,0,create_c7); 
   ;
   ret_val35 = r8;
   ret_val34 = ret_val35;
   s8 = ret_val34;
   ret_val33 = STR_ap1077157958(plus_self18, s8, TRUE);
   plus_self19 = ret_val33;
   if (noname8->state == 0) {
    L81 = ATTR(self,assert_out);
    noname8->self = L81;
   }
   L73 = FSETSTR_eltbrSTR(noname8);
   if (noname8->state == -1) {
    goto after_loop7;
   }
   plus_sarg11 = L73;
   ret_val36 = STR_ap2004550586(plus_self19, plus_sarg11);
   r = ret_val36;
  }
 }
 after_loop7: ;
 s = CODE_O740953890(self, ((STR) &Asserts), s, r);
 r = ((STR) &name1);
 if (ATTR(self,void_all)) {
  plus_self20 = r;
  plus_sarg12 = ((STR) &All);
  ret_val37 = STR_ap2004550586(plus_self20, plus_sarg12);
  r = ret_val37;
 }
 else {
  {
   struct FSETSTR_eltbrSTR_frame_struct other9_0;
FSETSTR_eltbrSTR_frame noname9 = &other9_0;
   noname9->state = 0;
   while (1) {
    plus_self21 = r;
    plus_arg8 = '+';
    str_self8 = plus_arg8;
    create_self8 = ((STR) NULL);
    create_c8 = str_self8;
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
    r9 = L74;
    SARR((STR)r9,0,create_c8); 
    ;
    ret_val40 = r9;
    ret_val39 = ret_val40;
    s9 = ret_val39;
    ret_val38 = STR_ap1077157958(plus_self21, s9, TRUE);
    plus_self22 = ret_val38;
    if (noname9->state == 0) {
     L91 = ATTR(self,void_in);
     noname9->self = L91;
    }
    L79 = FSETSTR_eltbrSTR(noname9);
    if (noname9->state == -1) {
     goto after_loop8;
    }
    plus_sarg13 = L79;
    ret_val41 = STR_ap2004550586(plus_self22, plus_sarg13);
    r = ret_val41;
   }
  }
  after_loop8: ;
 }
 {
  struct FSETSTR_eltbrSTR_frame_struct other10_0;
FSETSTR_eltbrSTR_frame noname10 = &other10_0;
  noname10->state = 0;
  while (1) {
   plus_self23 = r;
   plus_arg9 = '-';
   str_self9 = plus_arg9;
   create_self9 = ((STR) NULL);
   create_c9 = str_self9;
   L82 = 1;
   L84=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L82)*sizeof(CHAR);
   L83=ZALLOC_LEAF_BIG(L84);
   if (L83==NULL) FATAL("Unable to allocate more memory");
   memset(L83,0,L84);
   ((OB)L83)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L83)->header.destroyed=0;
#endif

   L80 = ((STR) L83);
   L80->asize = L82;
   r10 = L80;
   SARR((STR)r10,0,create_c9); 
   ;
   ret_val44 = r10;
   ret_val43 = ret_val44;
   s10 = ret_val43;
   ret_val42 = STR_ap1077157958(plus_self23, s10, TRUE);
   plus_self24 = ret_val42;
   if (noname10->state == 0) {
    L101 = ATTR(self,void_out);
    noname10->self = L101;
   }
   L86 = FSETSTR_eltbrSTR(noname10);
   if (noname10->state == -1) {
    goto after_loop9;
   }
   plus_sarg14 = L86;
   ret_val45 = STR_ap2004550586(plus_self24, plus_sarg14);
   r = ret_val45;
  }
 }
 after_loop9: ;
 s = CODE_O740953890(self, ((STR) &Voidchecks), s, r);
 r = ((STR) &name1);
 if (ATTR(self,when_all)) {
  plus_self25 = r;
  plus_sarg15 = ((STR) &All);
  ret_val46 = STR_ap2004550586(plus_self25, plus_sarg15);
  r = ret_val46;
 }
 else {
  {
   struct FSETSTR_eltbrSTR_frame_struct other11_0;
FSETSTR_eltbrSTR_frame noname11 = &other11_0;
   noname11->state = 0;
   while (1) {
    plus_self26 = r;
    plus_arg10 = '+';
    str_self10 = plus_arg10;
    create_self10 = ((STR) NULL);
    create_c10 = str_self10;
    L88 = 1;
    L90=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L88)*sizeof(CHAR);
    L89=ZALLOC_LEAF_BIG(L90);
    if (L89==NULL) FATAL("Unable to allocate more memory");
    memset(L89,0,L90);
    ((OB)L89)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L89)->header.destroyed=0;
#endif

    L87 = ((STR) L89);
    L87->asize = L88;
    r11 = L87;
    SARR((STR)r11,0,create_c10); 
    ;
    ret_val49 = r11;
    ret_val48 = ret_val49;
    s11 = ret_val48;
    ret_val47 = STR_ap1077157958(plus_self26, s11, TRUE);
    plus_self27 = ret_val47;
    if (noname11->state == 0) {
     L121 = ATTR(self,when_in);
     noname11->self = L121;
    }
    L93 = FSETSTR_eltbrSTR(noname11);
    if (noname11->state == -1) {
     goto after_loop10;
    }
    plus_sarg16 = L93;
    ret_val50 = STR_ap2004550586(plus_self27, plus_sarg16);
    r = ret_val50;
   }
  }
  after_loop10: ;
 }
 {
  struct FSETSTR_eltbrSTR_frame_struct other12_0;
FSETSTR_eltbrSTR_frame noname12 = &other12_0;
  noname12->state = 0;
  while (1) {
   plus_self28 = r;
   plus_arg11 = '-';
   str_self11 = plus_arg11;
   create_self11 = ((STR) NULL);
   create_c11 = str_self11;
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
   r12 = L94;
   SARR((STR)r12,0,create_c11); 
   ;
   ret_val53 = r12;
   ret_val52 = ret_val53;
   s12 = ret_val52;
   ret_val51 = STR_ap1077157958(plus_self28, s12, TRUE);
   plus_self29 = ret_val51;
   if (noname12->state == 0) {
    L131 = ATTR(self,when_out);
    noname12->self = L131;
   }
   L99 = FSETSTR_eltbrSTR(noname12);
   if (noname12->state == -1) {
    goto after_loop11;
   }
   plus_sarg17 = L99;
   ret_val54 = STR_ap2004550586(plus_self29, plus_sarg17);
   r = ret_val54;
  }
 }
 after_loop11: ;
 s = CODE_O740953890(self, ((STR) &Whenclause), s, r);
 r = ((STR) &name1);
 if (ATTR(self,bounds_all)) {
  plus_self30 = r;
  plus_sarg18 = ((STR) &All);
  ret_val55 = STR_ap2004550586(plus_self30, plus_sarg18);
  r = ret_val55;
 }
 else {
  {
   struct FSETSTR_eltbrSTR_frame_struct other13_0;
FSETSTR_eltbrSTR_frame noname13 = &other13_0;
   noname13->state = 0;
   while (1) {
    plus_self31 = r;
    plus_arg12 = '+';
    str_self12 = plus_arg12;
    create_self12 = ((STR) NULL);
    create_c12 = str_self12;
    L102 = 1;
    L104=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L102)*sizeof(CHAR);
    L103=ZALLOC_LEAF_BIG(L104);
    if (L103==NULL) FATAL("Unable to allocate more memory");
    memset(L103,0,L104);
    ((OB)L103)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L103)->header.destroyed=0;
#endif

    L100 = ((STR) L103);
    L100->asize = L102;
    r13 = L100;
    SARR((STR)r13,0,create_c12); 
    ;
    ret_val58 = r13;
    ret_val57 = ret_val58;
    s13 = ret_val57;
    ret_val56 = STR_ap1077157958(plus_self31, s13, TRUE);
    plus_self32 = ret_val56;
    if (noname13->state == 0) {
     L141 = ATTR(self,bounds_in);
     noname13->self = L141;
    }
    L106 = FSETSTR_eltbrSTR(noname13);
    if (noname13->state == -1) {
     goto after_loop12;
    }
    plus_sarg19 = L106;
    ret_val59 = STR_ap2004550586(plus_self32, plus_sarg19);
    r = ret_val59;
   }
  }
  after_loop12: ;
 }
 {
  struct FSETSTR_eltbrSTR_frame_struct other14_0;
FSETSTR_eltbrSTR_frame noname14 = &other14_0;
  noname14->state = 0;
  while (1) {
   plus_self33 = r;
   plus_arg13 = '-';
   str_self13 = plus_arg13;
   create_self13 = ((STR) NULL);
   create_c13 = str_self13;
   L108 = 1;
   L110=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L108)*sizeof(CHAR);
   L109=ZALLOC_LEAF_BIG(L110);
   if (L109==NULL) FATAL("Unable to allocate more memory");
   memset(L109,0,L110);
   ((OB)L109)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L109)->header.destroyed=0;
#endif

   L107 = ((STR) L109);
   L107->asize = L108;
   r14 = L107;
   SARR((STR)r14,0,create_c13); 
   ;
   ret_val62 = r14;
   ret_val61 = ret_val62;
   s14 = ret_val61;
   ret_val60 = STR_ap1077157958(plus_self33, s14, TRUE);
   plus_self34 = ret_val60;
   if (noname14->state == 0) {
    L151 = ATTR(self,bounds_out);
    noname14->self = L151;
   }
   L112 = FSETSTR_eltbrSTR(noname14);
   if (noname14->state == -1) {
    goto after_loop13;
   }
   plus_sarg20 = L112;
   ret_val63 = STR_ap2004550586(plus_self34, plus_sarg20);
   r = ret_val63;
  }
 }
 after_loop13: ;
 s = CODE_O740953890(self, ((STR) &Bounds), s, r);
 if (ATTR(self,destroy_chk)) {
  r = ((STR) &On);
 }
 else {
  r = ((STR) &Off);
 }
 s = CODE_O740953890(self, ((STR) &Destroy), s, r);
 if (ATTR(self,psather_chk)) {
  r = ((STR) &On);
 }
 else {
  r = ((STR) &Off);
 }
 s = CODE_O740953890(self, ((STR) &pSatherchecks), s, r);
 if (ATTR(self,stats)) {
  r = ((STR) &On);
 }
 else {
  r = ((STR) &Off);
 }
 s = CODE_O740953890(self, ((STR) &Statis1655725615), s, r);
 if (ATTR(self,psather_stats)) {
  r = ((STR) &On);
 }
 else {
  r = ((STR) &Off);
 }
 s = CODE_O740953890(self, ((STR) &pSatherstats), s, r);
 if (ATTR(self,psather_trace)) {
  r = ((STR) &On);
 }
 else {
  r = ((STR) &Off);
 }
 s = CODE_O740953890(self, ((STR) &pSathertrace), s, r);
 r = ((STR) &name1);
 if (ATTR(self,arith_all)) {
  plus_self35 = r;
  plus_sarg21 = ((STR) &All);
  ret_val64 = STR_ap2004550586(plus_self35, plus_sarg21);
  r = ret_val64;
 }
 else {
  {
   struct FSETSTR_eltbrSTR_frame_struct other15_0;
FSETSTR_eltbrSTR_frame noname15 = &other15_0;
   noname15->state = 0;
   while (1) {
    plus_self36 = r;
    plus_arg14 = '+';
    str_self14 = plus_arg14;
    create_self14 = ((STR) NULL);
    create_c14 = str_self14;
    L114 = 1;
    L116=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L114)*sizeof(CHAR);
    L115=ZALLOC_LEAF_BIG(L116);
    if (L115==NULL) FATAL("Unable to allocate more memory");
    memset(L115,0,L116);
    ((OB)L115)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L115)->header.destroyed=0;
#endif

    L113 = ((STR) L115);
    L113->asize = L114;
    r15 = L113;
    SARR((STR)r15,0,create_c14); 
    ;
    ret_val67 = r15;
    ret_val66 = ret_val67;
    s15 = ret_val66;
    ret_val65 = STR_ap1077157958(plus_self36, s15, TRUE);
    plus_self37 = ret_val65;
    if (noname15->state == 0) {
     L161 = ATTR(self,arith_in);
     noname15->self = L161;
    }
    L118 = FSETSTR_eltbrSTR(noname15);
    if (noname15->state == -1) {
     goto after_loop14;
    }
    plus_sarg22 = L118;
    ret_val68 = STR_ap2004550586(plus_self37, plus_sarg22);
    r = ret_val68;
   }
  }
  after_loop14: ;
 }
 {
  struct FSETSTR_eltbrSTR_frame_struct other16_0;
FSETSTR_eltbrSTR_frame noname16 = &other16_0;
  noname16->state = 0;
  while (1) {
   plus_self38 = r;
   plus_arg15 = '-';
   str_self15 = plus_arg15;
   create_self15 = ((STR) NULL);
   create_c15 = str_self15;
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
   r16 = L119;
   SARR((STR)r16,0,create_c15); 
   ;
   ret_val71 = r16;
   ret_val70 = ret_val71;
   s16 = ret_val70;
   ret_val69 = STR_ap1077157958(plus_self38, s16, TRUE);
   plus_self39 = ret_val69;
   if (noname16->state == 0) {
    L171 = ATTR(self,arith_out);
    noname16->self = L171;
   }
   L125 = FSETSTR_eltbrSTR(noname16);
   if (noname16->state == -1) {
    goto after_loop15;
   }
   plus_sarg23 = L125;
   ret_val72 = STR_ap2004550586(plus_self39, plus_sarg23);
   r = ret_val72;
  }
 }
 after_loop15: ;
 s = CODE_O740953890(self, ((STR) &Arith), s, r);
 r = ((STR) &name1);
 if (ATTR(self,return_all)) {
  plus_self40 = r;
  plus_sarg24 = ((STR) &All);
  ret_val73 = STR_ap2004550586(plus_self40, plus_sarg24);
  r = ret_val73;
 }
 else {
  {
   struct FSETSTR_eltbrSTR_frame_struct other17_0;
FSETSTR_eltbrSTR_frame noname17 = &other17_0;
   noname17->state = 0;
   while (1) {
    plus_self41 = r;
    plus_arg16 = '+';
    str_self16 = plus_arg16;
    create_self16 = ((STR) NULL);
    create_c16 = str_self16;
    L127 = 1;
    L129=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L127)*sizeof(CHAR);
    L128=ZALLOC_LEAF_BIG(L129);
    if (L128==NULL) FATAL("Unable to allocate more memory");
    memset(L128,0,L129);
    ((OB)L128)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L128)->header.destroyed=0;
#endif

    L126 = ((STR) L128);
    L126->asize = L127;
    r17 = L126;
    SARR((STR)r17,0,create_c16); 
    ;
    ret_val76 = r17;
    ret_val75 = ret_val76;
    s17 = ret_val75;
    ret_val74 = STR_ap1077157958(plus_self41, s17, TRUE);
    plus_self42 = ret_val74;
    if (noname17->state == 0) {
     L181 = ATTR(self,return_in);
     noname17->self = L181;
    }
    L132 = FSETSTR_eltbrSTR(noname17);
    if (noname17->state == -1) {
     goto after_loop16;
    }
    plus_sarg25 = L132;
    ret_val77 = STR_ap2004550586(plus_self42, plus_sarg25);
    r = ret_val77;
   }
  }
  after_loop16: ;
 }
 {
  struct FSETSTR_eltbrSTR_frame_struct other18_0;
FSETSTR_eltbrSTR_frame noname18 = &other18_0;
  noname18->state = 0;
  while (1) {
   plus_self43 = r;
   plus_arg17 = '-';
   str_self17 = plus_arg17;
   create_self17 = ((STR) NULL);
   create_c17 = str_self17;
   L134 = 1;
   L136=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L134)*sizeof(CHAR);
   L135=ZALLOC_LEAF_BIG(L136);
   if (L135==NULL) FATAL("Unable to allocate more memory");
   memset(L135,0,L136);
   ((OB)L135)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L135)->header.destroyed=0;
#endif

   L133 = ((STR) L135);
   L133->asize = L134;
   r18 = L133;
   SARR((STR)r18,0,create_c17); 
   ;
   ret_val80 = r18;
   ret_val79 = ret_val80;
   s18 = ret_val79;
   ret_val78 = STR_ap1077157958(plus_self43, s18, TRUE);
   plus_self44 = ret_val78;
   if (noname18->state == 0) {
    L191 = ATTR(self,return_out);
    noname18->self = L191;
   }
   L138 = FSETSTR_eltbrSTR(noname18);
   if (noname18->state == -1) {
    goto after_loop17;
   }
   plus_sarg26 = L138;
   ret_val81 = STR_ap2004550586(plus_self44, plus_sarg26);
   r = ret_val81;
  }
 }
 after_loop17: ;
 s = CODE_O740953890(self, ((STR) &Return), s, r);
 ret_val = s;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CODE_O1483684745(CODE_OPTIONS self, STR fname, dSTR arg_str, INT* pos) {
 INT L1 = *pos;/*Local for arg*/
 STR ret_val;
 STR s;
 STR e = ((STR) NULL);
 CHAR op = CHAR_zero;
 BOOL skip1 = BOOL_zero;
 CHAR cp = CHAR_zero;
 STR tenv;
 ERR1 create_self;
 ERR1 ret_val1;
 ERR1 plus_self;
 dSTR plus_s;
 ERR1 ret_val2;
 FILE1 stderr_self;
 FILE1 ret_val3;
 FILE1 r;
 ERR1 plus_self1;
 dSTR plus_s1;
 ERR1 ret_val4;
 FILE1 stderr_self1;
 FILE1 ret_val5;
 FILE1 r1;
 ERR1 plus_self2;
 dSTR plus_s2;
 ERR1 ret_val6;
 FILE1 stderr_self2;
 FILE1 ret_val7;
 FILE1 r2;
 ERR1 plus_self3;
 dSTR plus_s3;
 ERR1 ret_val8;
 FILE1 stderr_self3;
 FILE1 ret_val9;
 FILE1 r3;
 ERR1 plus_self4;
 dSTR plus_s4;
 ERR1 ret_val10;
 FILE1 stderr_self4;
 FILE1 ret_val11;
 FILE1 r4;
 ERR1 plus_self5;
 dSTR plus_s5;
 ERR1 ret_val12;
 FILE1 stderr_self5;
 FILE1 ret_val13;
 FILE1 r5;
 ERR1 plus_self6;
 dSTR plus_s6;
 ERR1 ret_val14;
 FILE1 stderr_self6;
 FILE1 ret_val15;
 FILE1 r6;
 ERR1 plus_self7;
 dSTR plus_s7;
 ERR1 ret_val16;
 FILE1 stderr_self7;
 FILE1 ret_val17;
 FILE1 r7;
 ERR1 plus_self8;
 dSTR plus_s8;
 FILE1 stderr_self8;
 FILE1 ret_val18;
 FILE1 r8;
 STR plus_self9;
 CHAR plus_arg = CHAR_zero;
 STR ret_val19;
 STR s1;
 CHAR str_self = CHAR_zero;
 STR ret_val20;
 STR create_self1;
 CHAR create_c = CHAR_zero;
 STR ret_val21;
 STR r9;
 STR plus_self10;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val22;
 STR s2;
 CHAR str_self1 = CHAR_zero;
 STR ret_val23;
 STR create_self2;
 CHAR create_c1 = CHAR_zero;
 STR ret_val24;
 STR r10;
 STR plus_self11;
 STR plus_sarg;
 STR ret_val25;
 STR plus_self12;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val26;
 STR s3;
 CHAR str_self2 = CHAR_zero;
 STR ret_val27;
 STR create_self3;
 CHAR create_c2 = CHAR_zero;
 STR ret_val28;
 STR r11;
 dSTR L2;
 INT L31_;
 INT L4;
 CHAR L51_;
 BOOL L6;
 BOOL L71_;
 BOOL L81_;
 BOOL L91_;
 extern STR name1;
 INT L101_;
 BOOL L11;
 INT L12;
 BOOL L131_;
 CHAR L141_;
 CHAR L15;
 BOOL L161_;
 BOOL L17;
 BOOL L181_;
 BOOL L19;
 BOOL L20;
 BOOL L21;
 CHAR L221_;
 CHAR L23;
 BOOL L241_;
 CHAR L251_;
 CHAR L26;
 BOOL L271_;
 CHAR L281_;
 CHAR L29;
 BOOL L301_;
 CHAR L321_;
 CHAR L33;
 BOOL L341_;
 extern STR Unterm889448459;
 FILE1 L35;
 OB L36;
 extern STR name8;
 FILE1 L38;
 OB L39;
 FILE1 L41;
 OB L42;
 extern STR in;
 FILE1 L44;
 OB L45;
 FILE1 L47;
 OB L48;
 extern STR assume;
 FILE1 L50;
 OB L52;
 dSTR L54;
 OB L55;
 FILE1 L56;
 OB L57;
 extern STR name19;
 FILE1 L59;
 OB L60;
 extern STR skipthistoken;
 FILE1 L62;
 OB L63;
 CHAR L651_;
 STR L66;
 INT L67;
 OB L68;
 INT L69;
 INT L721_;
 BOOL L731_;
 INT L74;
 BOOL L751_;
 INT L761_;
 extern STR d1;
 STR L77;
 INT L78;
 OB L79;
 INT L80;
 STR L83;
 INT L84;
 OB L85;
 INT L86;
 L2 = arg_str;
 s = (*dSTR_strrSTR[TAG(L2)])(L2);
 L31_=INTPLUS(L1,1); 
 L4 = L31_;
 L51_=ARR((STR)s,L4); 
 op = L51_;
 L71_=op=='{'; 
 if (L71_) {
  L6 = TRUE;
 } else {
  L81_=op=='('; 
  L6 = L81_;
 }
 if (L6) {
  skip1 = FALSE;
  cp = '}';
  L91_=op=='('; 
  if (L91_) {
   cp = ')';
  }
  tenv = ((STR) &name1);
  L101_=INTPLUS(L1,2); 
  L1 = L101_;
  while (1) {
   L12 = STR_sizerINT(s);
   L131_=(L1)<(L12); 
   if (L131_) {
    L141_=ARR((STR)s,L1); 
    L15 = L141_;
    L161_=L15==cp; 
    L17 = L161_;
    L181_=!(L17); 
    L11 = L181_;
   } else {
    L11 = FALSE;
   }
   if (L11) {
   }
   else {
    goto after_loop;
   }
   L221_=ARR((STR)s,L1); 
   L23 = L221_;
   L241_=L23==')'; 
   if (L241_) {
    L21 = TRUE;
   } else {
    L251_=ARR((STR)s,L1); 
    L26 = L251_;
    L271_=L26=='}'; 
    L21 = L271_;
   }
   if (L21) {
    L20 = TRUE;
   } else {
    L281_=ARR((STR)s,L1); 
    L29 = L281_;
    L301_=L29=='\n'; 
    L20 = L301_;
   }
   if (L20) {
    L19 = TRUE;
   } else {
    L321_=ARR((STR)s,L1); 
    L33 = L321_;
    L341_=L33=='\r'; 
    L19 = L341_;
   }
   if (L19) {
    create_self = ((ERR1) NULL);
    ret_val1 = create_self;
    plus_self = ret_val1;
    plus_s = ((dSTR) ((STR) &Unterm889448459));
    stderr_self = ((FILE1) NULL);
    L36=ZALLOC(sizeof(struct FILE1_struct));
    if (L36==NULL) FATAL("Unable to allocate more memory");
    ((OB)L36)->header.tag=FILE1_tag;
    L35 = ((FILE1) L36);
    r = L35;
    
    SATTR(r,fp,stderr);
    ret_val3 = r;
    FILE_plus_dSTR(ret_val3, plus_s);
    ret_val2 = plus_self;
    plus_self1 = ret_val2;
    plus_s1 = ((dSTR) ((STR) &name8));
    stderr_self1 = ((FILE1) NULL);
    L39=ZALLOC(sizeof(struct FILE1_struct));
    if (L39==NULL) FATAL("Unable to allocate more memory");
    ((OB)L39)->header.tag=FILE1_tag;
    L38 = ((FILE1) L39);
    r1 = L38;
    
    SATTR(r1,fp,stderr);
    ret_val5 = r1;
    FILE_plus_dSTR(ret_val5, plus_s1);
    ret_val4 = plus_self1;
    plus_self2 = ret_val4;
    plus_s2 = ((dSTR) tenv);
    stderr_self2 = ((FILE1) NULL);
    L42=ZALLOC(sizeof(struct FILE1_struct));
    if (L42==NULL) FATAL("Unable to allocate more memory");
    ((OB)L42)->header.tag=FILE1_tag;
    L41 = ((FILE1) L42);
    r2 = L41;
    
    SATTR(r2,fp,stderr);
    ret_val7 = r2;
    FILE_plus_dSTR(ret_val7, plus_s2);
    ret_val6 = plus_self2;
    plus_self3 = ret_val6;
    plus_s3 = ((dSTR) ((STR) &in));
    stderr_self3 = ((FILE1) NULL);
    L45=ZALLOC(sizeof(struct FILE1_struct));
    if (L45==NULL) FATAL("Unable to allocate more memory");
    ((OB)L45)->header.tag=FILE1_tag;
    L44 = ((FILE1) L45);
    r3 = L44;
    
    SATTR(r3,fp,stderr);
    ret_val9 = r3;
    FILE_plus_dSTR(ret_val9, plus_s3);
    ret_val8 = plus_self3;
    plus_self4 = ret_val8;
    plus_s4 = ((dSTR) fname);
    stderr_self4 = ((FILE1) NULL);
    L48=ZALLOC(sizeof(struct FILE1_struct));
    if (L48==NULL) FATAL("Unable to allocate more memory");
    ((OB)L48)->header.tag=FILE1_tag;
    L47 = ((FILE1) L48);
    r4 = L47;
    
    SATTR(r4,fp,stderr);
    ret_val11 = r4;
    FILE_plus_dSTR(ret_val11, plus_s4);
    ret_val10 = plus_self4;
    plus_self5 = ret_val10;
    plus_s5 = ((dSTR) ((STR) &assume));
    stderr_self5 = ((FILE1) NULL);
    L52=ZALLOC(sizeof(struct FILE1_struct));
    if (L52==NULL) FATAL("Unable to allocate more memory");
    ((OB)L52)->header.tag=FILE1_tag;
    L50 = ((FILE1) L52);
    r5 = L50;
    
    SATTR(r5,fp,stderr);
    ret_val13 = r5;
    FILE_plus_dSTR(ret_val13, plus_s5);
    ret_val12 = plus_self5;
    plus_self6 = ret_val12;
    L55=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
    if (L55==NULL) FATAL("Unable to allocate more memory");
    memset(L55,0,sizeof(struct CHAR_boxed_struct));
    ((OB)L55)->header.tag=CHAR_tag;
    L54 = (dSTR)((CHAR_boxed) L55);
    ((CHAR_boxed) L54)->immutable_part = cp;
    plus_s6 = L54;
    stderr_self6 = ((FILE1) NULL);
    L57=ZALLOC(sizeof(struct FILE1_struct));
    if (L57==NULL) FATAL("Unable to allocate more memory");
    ((OB)L57)->header.tag=FILE1_tag;
    L56 = ((FILE1) L57);
    r6 = L56;
    
    SATTR(r6,fp,stderr);
    ret_val15 = r6;
    FILE_plus_dSTR(ret_val15, plus_s6);
    ret_val14 = plus_self6;
    plus_self7 = ret_val14;
    plus_s7 = ((dSTR) ((STR) &name19));
    stderr_self7 = ((FILE1) NULL);
    L60=ZALLOC(sizeof(struct FILE1_struct));
    if (L60==NULL) FATAL("Unable to allocate more memory");
    ((OB)L60)->header.tag=FILE1_tag;
    L59 = ((FILE1) L60);
    r7 = L59;
    
    SATTR(r7,fp,stderr);
    ret_val17 = r7;
    FILE_plus_dSTR(ret_val17, plus_s7);
    ret_val16 = plus_self7;
    plus_self8 = ret_val16;
    plus_s8 = ((dSTR) ((STR) &skipthistoken));
    stderr_self8 = ((FILE1) NULL);
    L63=ZALLOC(sizeof(struct FILE1_struct));
    if (L63==NULL) FATAL("Unable to allocate more memory");
    ((OB)L63)->header.tag=FILE1_tag;
    L62 = ((FILE1) L63);
    r8 = L62;
    
    SATTR(r8,fp,stderr);
    ret_val18 = r8;
    FILE_plus_dSTR(ret_val18, plus_s8);
    skip1 = TRUE;
    goto after_loop;
   }
   plus_self9 = tenv;
   L651_=ARR((STR)s,L1); 
   plus_arg = L651_;
   str_self = plus_arg;
   create_self1 = ((STR) NULL);
   create_c = str_self;
   L67 = 1;
   L69=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L67)*sizeof(CHAR);
   L68=ZALLOC_LEAF_BIG(L69);
   if (L68==NULL) FATAL("Unable to allocate more memory");
   memset(L68,0,L69);
   ((OB)L68)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L68)->header.destroyed=0;
#endif

   L66 = ((STR) L68);
   L66->asize = L67;
   r9 = L66;
   SARR((STR)r9,0,create_c); 
   ;
   ret_val21 = r9;
   ret_val20 = ret_val21;
   s1 = ret_val20;
   ret_val19 = STR_ap1077157958(plus_self9, s1, TRUE);
   tenv = ret_val19;
   L721_=INTPLUS(L1,1); 
   L1 = L721_;
  }
  after_loop: ;
  L731_=!(skip1); 
  if (L731_) {
   L74 = STR_sizerINT(s);
   L751_=(L1)<(L74); 
   if (L751_) {
    L761_=INTPLUS(L1,1); 
    L1 = L761_;
   }
   e = UNIX_g108606936(((UNIX) NULL), tenv);
   if ((e==((STR) NULL))) {
    plus_self10 = ((STR) &d1);
    plus_arg1 = op;
    str_self1 = plus_arg1;
    create_self2 = ((STR) NULL);
    create_c1 = str_self1;
    L78 = 1;
    L80=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L78)*sizeof(CHAR);
    L79=ZALLOC_LEAF_BIG(L80);
    if (L79==NULL) FATAL("Unable to allocate more memory");
    memset(L79,0,L80);
    ((OB)L79)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L79)->header.destroyed=0;
#endif

    L77 = ((STR) L79);
    L77->asize = L78;
    r10 = L77;
    SARR((STR)r10,0,create_c1); 
    ;
    ret_val24 = r10;
    ret_val23 = ret_val24;
    s2 = ret_val23;
    ret_val22 = STR_ap1077157958(plus_self10, s2, TRUE);
    plus_self11 = ret_val22;
    plus_sarg = tenv;
    ret_val25 = STR_ap2004550586(plus_self11, plus_sarg);
    plus_self12 = ret_val25;
    plus_arg2 = cp;
    str_self2 = plus_arg2;
    create_self3 = ((STR) NULL);
    create_c2 = str_self2;
    L84 = 1;
    L86=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L84)*sizeof(CHAR);
    L85=ZALLOC_LEAF_BIG(L86);
    if (L85==NULL) FATAL("Unable to allocate more memory");
    memset(L85,0,L86);
    ((OB)L85)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L85)->header.destroyed=0;
#endif

    L83 = ((STR) L85);
    L83->asize = L84;
    r11 = L83;
    SARR((STR)r11,0,create_c2); 
    ;
    ret_val28 = r11;
    ret_val27 = ret_val28;
    s3 = ret_val27;
    ret_val26 = STR_ap1077157958(plus_self12, s3, TRUE);
    e = ret_val26;
   }
  }
 }
 ret_val = e;
 *pos = L1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CODE_O1955258734(CODE_OPTIONS self, STR nm) {
 STR ret_val;
 INT pos = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 STR r;
 INT L4;
 INT L51_;
 INT L31_,L31_m;
 INT L6;
 CHAR L71_;
 CHAR L8;
 BOOL L91_;
 extern STR name1;
 extern STR name18;
 {
  BOOL f_L31_ = TRUE;
  L4 = STR_sizerINT(nm);
  L51_=INTMINUS(L4,1); 
  L11 = L51_;
  L21 = 0;
  L31_=L11+1;L31_m=L21; 
  while (1) {
   if(L31_--<=L31_m)  goto after_loop; 
   pos = L31_;
   L71_=ARR((STR)nm,pos); 
   L8 = L71_;
   L91_=L8=='/'; 
   if (L91_) {
    goto after_loop;
   }
  }
 }
 after_loop: ;
 r = STR_head_INTrSTR(nm, pos);
 if (STR_is111530248(r, ((STR) &name1))) {
  r = ((STR) &name18);
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CODE_O218236968(CODE_OPTIONS self, STR fname, dSTR arg_str) {
 STR ret_val;
 STR s;
 INT i = INT_zero;
 STR res = ((STR) NULL);
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 CHAR plus_arg = CHAR_zero;
 STR ret_val2;
 STR s1;
 CHAR str_self = CHAR_zero;
 STR ret_val3;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val4;
 STR r;
 dSTR L1;
 INT L2;
 BOOL L31_;
 CHAR L41_;
 CHAR L5;
 BOOL L61_;
 INT* L7;
 STR L8;
 CHAR L91_;
 STR L10;
 INT L11;
 OB L12;
 INT L13;
 INT L151_;
 L1 = arg_str;
 s = (*dSTR_strrSTR[TAG(L1)])(L1);
 i = 0;
 while (1) {
  L2 = STR_sizerINT(s);
  L31_=(i)<(L2); 
  if (L31_) {
  }
  else {
   goto after_loop;
  }
  L41_=ARR((STR)s,i); 
  L5 = L41_;
  L61_=L5=='$'; 
  if (L61_) {
   plus_self = res;
   L7 = &i;
   L8 = CODE_O1483684745(self, fname, ((dSTR) s), L7);
   plus_sarg = L8;
   ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
   res = ret_val1;
  }
  else {
   plus_self1 = res;
   L91_=ARR((STR)s,i); 
   plus_arg = L91_;
   str_self = plus_arg;
   create_self = ((STR) NULL);
   create_c = str_self;
   L11 = 1;
   L13=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L11)*sizeof(CHAR);
   L12=ZALLOC_LEAF_BIG(L13);
   if (L12==NULL) FATAL("Unable to allocate more memory");
   memset(L12,0,L13);
   ((OB)L12)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L12)->header.destroyed=0;
#endif

   L10 = ((STR) L12);
   L10->asize = L11;
   r = L10;
   SARR((STR)r,0,create_c); 
   ;
   ret_val4 = r;
   ret_val3 = ret_val4;
   s1 = ret_val3;
   ret_val2 = STR_ap1077157958(plus_self1, s1, TRUE);
   res = ret_val2;
   L151_=INTPLUS(i,1); 
   i = L151_;
  }
 }
 after_loop: ;
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CODE_O346288391(CODE_OPTIONS self) {
 STR ret_val;
 STR r = ((STR) NULL);
 STR s = ((STR) NULL);
 FSETSTR L11;
 STR plus_self;
 CHAR plus_arg = CHAR_zero;
 STR ret_val1;
 STR s1;
 CHAR str_self = CHAR_zero;
 STR ret_val2;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val3;
 STR r1;
 STR plus_self1;
 STR plus_sarg;
 STR ret_val4;
 STR plus_self2;
 STR plus_sarg1;
 STR ret_val5;
 STR plus_self3;
 STR plus_sarg2;
 STR ret_val6;
 STR plus_self4;
 STR plus_sarg3;
 STR ret_val7;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val8;
 extern STR name1;
 STR L2;
 INT L3;
 OB L4;
 INT L5;
 STR L7;
 extern STR Satherfiles;
 extern STR name1;
 extern STR Mainclass;
 extern STR Homedirectory;
 extern STR Prolix;
 extern STR Verbose;
 r = ((STR) &name1);
 {
  struct FSETSTR_eltbrSTR_frame_struct other1_0;
FSETSTR_eltbrSTR_frame noname1 = &other1_0;
  noname1->state = 0;
  while (1) {
   plus_self = r;
   plus_arg = ' ';
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
   r1 = L2;
   SARR((STR)r1,0,create_c); 
   ;
   ret_val3 = r1;
   ret_val2 = ret_val3;
   s1 = ret_val2;
   ret_val1 = STR_ap1077157958(plus_self, s1, TRUE);
   plus_self1 = ret_val1;
   if (noname1->state == 0) {
    L11 = ATTR(ATTR(self,prog),sather_files);
    noname1->self = L11;
   }
   L7 = FSETSTR_eltbrSTR(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   plus_sarg = L7;
   ret_val4 = STR_ap2004550586(plus_self1, plus_sarg);
   r = ret_val4;
  }
 }
 after_loop: ;
 s = CODE_O740953890(self, ((STR) &Satherfiles), ((STR) &name1), r);
 s = CODE_O740953890(self, ((STR) &Mainclass), s, ATTR(ATTR(self,prog),main_class));
 plus_self2 = s;
 plus_sarg1 = ((STR) &Homedirectory);
 ret_val5 = STR_ap2004550586(plus_self2, plus_sarg1);
 plus_self3 = ret_val5;
 plus_sarg2 = ATTR(ATTR(self,prog),home);
 ret_val6 = STR_ap2004550586(plus_self3, plus_sarg2);
 s = ret_val6;
 if (ATTR(ATTR(self,prog),prolix)) {
  plus_self4 = s;
  plus_sarg3 = ((STR) &Prolix);
  ret_val7 = STR_ap2004550586(plus_self4, plus_sarg3);
  s = ret_val7;
 }
 if (ATTR(ATTR(self,prog),verbose)) {
  plus_self5 = s;
  plus_sarg4 = ((STR) &Verbose);
  ret_val8 = STR_ap2004550586(plus_self5, plus_sarg4);
  s = ret_val8;
 }
 ret_val = s;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CODE_O689829754(CODE_OPTIONS self, STR a) {
 STR ret_val;
 INT pos = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT L4;
 INT L51_;
 INT L31_,L31_m;
 INT L6;
 CHAR L71_;
 CHAR L8;
 BOOL L91_;
 INT L10;
 INT L121_;
 {
  BOOL f_L31_ = TRUE;
  L4 = STR_lengthrINT(a);
  L51_=INTMINUS(L4,1); 
  L11 = L51_;
  L21 = 0;
  L31_=L11+1;L31_m=L21; 
  while (1) {
   if(L31_--<=L31_m)  goto after_loop; 
   pos = L31_;
   L71_=ARR((STR)a,pos); 
   L8 = L71_;
   L91_=L8=='.'; 
   if (L91_) {
    goto after_loop;
   }
  }
 }
 after_loop: ;
 L10 = STR_lengthrINT(a);
 L121_=INTMINUS(L10,pos); 
 ret_val = STR_tail_INTrSTR(a, L121_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CODE_O740953890(CODE_OPTIONS self, STR name111, STR s1, STR s2) {
 STR ret_val;
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
 STR plus_sarg;
 STR ret_val4;
 STR plus_self2;
 STR plus_sarg1;
 STR ret_val5;
 STR plus_self3;
 STR plus_sarg2;
 STR ret_val6;
 extern STR name1;
 BOOL L1;
 BOOL L21_;
 STR L3;
 INT L4;
 OB L5;
 INT L6;
 extern STR name20;
 L1 = STR_is111530248(s2, ((STR) &name1));
 L21_=!(L1); 
 if (L21_) {
  plus_self = s1;
  plus_arg = '\n';
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
  ret_val3 = r;
  ret_val2 = ret_val3;
  s = ret_val2;
  ret_val1 = STR_ap1077157958(plus_self, s, TRUE);
  plus_self1 = ret_val1;
  plus_sarg = name111;
  ret_val4 = STR_ap2004550586(plus_self1, plus_sarg);
  plus_self2 = ret_val4;
  plus_sarg1 = ((STR) &name20);
  ret_val5 = STR_ap2004550586(plus_self2, plus_sarg1);
  plus_self3 = ret_val5;
  plus_sarg2 = s2;
  ret_val6 = STR_ap2004550586(plus_self3, plus_sarg2);
  ret_val = ret_val6;
  return ret_val;
 }
 else {
  ret_val = s1;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR CODE_O862164136(CODE_OPTIONS self) {
 STR ret_val;
 STR res;
 CODE_OPTIONS more_args_self;
 BOOL ret_val1 = BOOL_zero;
 OUT create_self;
 OUT ret_val2;
 OUT plus_self;
 STR plus_s;
 OUT ret_val3;
 FILE1 stdout_self;
 FILE1 ret_val4;
 FILE1 r;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val5;
 FILE1 stdout_self1;
 FILE1 ret_val6;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val7;
 FILE1 r2;
 INT L1;
 ARRAYSTR L2;
 INT L31_;
 INT L4;
 BOOL L51_;
 ARRAYSTR L6;
 INT L7;
 STR L81_;
 INT L9;
 INT L101_;
 extern STR Arg;
 FILE1 L11;
 OB L12;
 FILE1 L14;
 OB L15;
 extern STR name9;
 FILE1 L17;
 OB L18;
 extern STR name1;
 more_args_self = self;
 L1 = ATTR(more_args_self,next);
 L2 = ATTR(more_args_self,args);
 L31_=(L2)==NULL?0:ASIZE((ARRAYSTR)L2); 
 L4 = L31_;
 L51_=(L1)<(L4); 
 ret_val1 = L51_;
 if (ret_val1) {
  L6 = ATTR(self,args);
  L7 = ATTR(self,next);
  L81_=(STR)ARR((ARRAYSTR)L6,L7); 
  res = L81_;
  L9 = ATTR(self,next);
  L101_=INTPLUS(L9,1); 
  SATTR(self,next,L101_);
  if (ATTR(ATTR(self,prog),prolix)) {
   create_self = ((OUT) NULL);
   ret_val2 = create_self;
   plus_self = ret_val2;
   plus_s = ((STR) &Arg);
   stdout_self = ((FILE1) NULL);
   L12=ZALLOC(sizeof(struct FILE1_struct));
   if (L12==NULL) FATAL("Unable to allocate more memory");
   ((OB)L12)->header.tag=FILE1_tag;
   L11 = ((FILE1) L12);
   r = L11;
   
   SATTR(r,fp,stdout);
   ret_val4 = r;
   FILE_plus_STR(ret_val4, plus_s);
   ret_val3 = plus_self;
   plus_self1 = ret_val3;
   plus_s1 = res;
   stdout_self1 = ((FILE1) NULL);
   L15=ZALLOC(sizeof(struct FILE1_struct));
   if (L15==NULL) FATAL("Unable to allocate more memory");
   ((OB)L15)->header.tag=FILE1_tag;
   L14 = ((FILE1) L15);
   r1 = L14;
   
   SATTR(r1,fp,stdout);
   ret_val6 = r1;
   FILE_plus_STR(ret_val6, plus_s1);
   ret_val5 = plus_self1;
   plus_self2 = ret_val5;
   plus_s2 = ((STR) &name9);
   stdout_self2 = ((FILE1) NULL);
   L18=ZALLOC(sizeof(struct FILE1_struct));
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FILE1_tag;
   L17 = ((FILE1) L18);
   r2 = L17;
   
   SATTR(r2,fp,stdout);
   ret_val7 = r2;
   FILE_plus_STR(ret_val7, plus_s2);
  }
  ret_val = res;
  return ret_val;
 }
 else {
  CODE_O1762472157(self);
  ret_val = ((STR) &name1);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

void CODE_O1006348781(CODE_OPTIONS self, STR s) {
 STR L11;
 STR cfs;
 STR platform_module;
 BFILE f;
 FSETSTR create_self;
 FSETSTR ret_val;
 FSETSTR create_self1;
 FSETSTR ret_val1;
 FSETSTR create_self2;
 FSETSTR ret_val2;
 FSETSTR create_self3;
 FSETSTR ret_val3;
 FSETSTR create_self4;
 FSETSTR ret_val4;
 FSETSTR create_self5;
 FSETSTR ret_val5;
 FSETSTR create_self6;
 FSETSTR ret_val6;
 FSETSTR create_self7;
 FSETSTR ret_val7;
 FSETSTR create_self8;
 FSETSTR ret_val8;
 FSETSTR union_self;
 FSETSTR union_s;
 FSETSTR ret_val9;
 FSETSTR union_self1;
 FSETSTR union_s1;
 FSETSTR ret_val10;
 FSETSTR union_self2;
 FSETSTR union_s2;
 FSETSTR ret_val11;
 FSETSTR union_self3;
 FSETSTR union_s3;
 FSETSTR ret_val12;
 FSETSTR union_self4;
 FSETSTR union_s4;
 FSETSTR ret_val13;
 FSETSTR union_self5;
 FSETSTR union_s5;
 FSETSTR ret_val14;
 FSETSTR union_self6;
 FSETSTR union_s6;
 FSETSTR ret_val15;
 FSETSTR union_self7;
 FSETSTR union_s7;
 FSETSTR ret_val16;
 FSETSTR union_self8;
 FSETSTR union_s8;
 FSETSTR ret_val17;
 FSETSTR create_self9;
 FSETSTR ret_val18;
 FSETSTR create_self10;
 FSETSTR ret_val19;
 FSETSTR create_self11;
 FSETSTR ret_val20;
 FSETSTR create_self12;
 FSETSTR ret_val21;
 FSETSTR create_self13;
 FSETSTR ret_val22;
 FSETSTR create_self14;
 FSETSTR ret_val23;
 FSETSTR create_self15;
 FSETSTR ret_val24;
 FSETSTR create_self16;
 FSETSTR ret_val25;
 FSETSTR create_self17;
 FSETSTR ret_val26;
 FSETSTR union_self9;
 FSETSTR union_s9;
 FSETSTR ret_val27;
 FSETSTR union_self10;
 FSETSTR union_s10;
 FSETSTR ret_val28;
 FSETSTR union_self11;
 FSETSTR union_s11;
 FSETSTR ret_val29;
 FSETSTR union_self12;
 FSETSTR union_s12;
 FSETSTR ret_val30;
 FSETSTR union_self13;
 FSETSTR union_s13;
 FSETSTR ret_val31;
 FSETSTR union_self14;
 FSETSTR union_s14;
 FSETSTR ret_val32;
 FSETSTR union_self15;
 FSETSTR union_s15;
 FSETSTR ret_val33;
 FSETSTR union_self16;
 FSETSTR union_s16;
 FSETSTR ret_val34;
 FSETSTR union_self17;
 FSETSTR union_s17;
 FSETSTR ret_val35;
 FSETSTR create_self18;
 FSETSTR ret_val36;
 FSETSTR union_self18;
 FSETSTR union_s18;
 FSETSTR ret_val37;
 FSETSTR create_self19;
 FSETSTR ret_val38;
 FSETSTR union_self19;
 FSETSTR union_s19;
 FSETSTR ret_val39;
 FSETSTR create_self20;
 FSETSTR ret_val40;
 FSETSTR union_self20;
 FSETSTR union_s20;
 FSETSTR ret_val41;
 FSETSTR create_self21;
 FSETSTR ret_val42;
 FSETSTR union_self21;
 FSETSTR union_s21;
 FSETSTR ret_val43;
 FSETSTR create_self22;
 FSETSTR ret_val44;
 FSETSTR union_self22;
 FSETSTR union_s22;
 FSETSTR ret_val45;
 FSETSTR create_self23;
 FSETSTR ret_val46;
 FSETSTR union_self23;
 FSETSTR union_s23;
 FSETSTR ret_val47;
 FSETSTR create_self24;
 FSETSTR ret_val48;
 FSETSTR union_self24;
 FSETSTR union_s24;
 FSETSTR ret_val49;
 FSETSTR create_self25;
 FSETSTR ret_val50;
 FSETSTR union_self25;
 FSETSTR union_s25;
 FSETSTR ret_val51;
 FSETSTR create_self26;
 FSETSTR ret_val52;
 FSETSTR union_self26;
 FSETSTR union_s26;
 FSETSTR ret_val53;
 FSETSTR create_self27;
 FSETSTR ret_val54;
 FSETSTR create_self28;
 FSETSTR ret_val55;
 FSETSTR create_self29;
 FSETSTR ret_val56;
 FSETSTR create_self30;
 FSETSTR ret_val57;
 FSETSTR create_self31;
 FSETSTR ret_val58;
 FSETSTR create_self32;
 FSETSTR ret_val59;
 FSETSTR create_self33;
 FSETSTR ret_val60;
 FSETSTR create_self34;
 FSETSTR ret_val61;
 FSETSTR create_self35;
 FSETSTR ret_val62;
 FSETSTR union_self27;
 FSETSTR union_s27;
 FSETSTR ret_val63;
 FSETSTR union_self28;
 FSETSTR union_s28;
 FSETSTR ret_val64;
 FSETSTR union_self29;
 FSETSTR union_s29;
 FSETSTR ret_val65;
 FSETSTR union_self30;
 FSETSTR union_s30;
 FSETSTR ret_val66;
 FSETSTR union_self31;
 FSETSTR union_s31;
 FSETSTR ret_val67;
 FSETSTR union_self32;
 FSETSTR union_s32;
 FSETSTR ret_val68;
 FSETSTR union_self33;
 FSETSTR union_s33;
 FSETSTR ret_val69;
 FSETSTR union_self34;
 FSETSTR union_s34;
 FSETSTR ret_val70;
 FSETSTR union_self35;
 FSETSTR union_s35;
 FSETSTR ret_val71;
 FSETSTR create_self36;
 FSETSTR ret_val72;
 FSETSTR create_self37;
 FSETSTR ret_val73;
 FSETSTR create_self38;
 FSETSTR ret_val74;
 FSETSTR create_self39;
 FSETSTR ret_val75;
 FSETSTR create_self40;
 FSETSTR ret_val76;
 FSETSTR create_self41;
 FSETSTR ret_val77;
 FSETSTR create_self42;
 FSETSTR ret_val78;
 FSETSTR create_self43;
 FSETSTR ret_val79;
 FSETSTR create_self44;
 FSETSTR ret_val80;
 FSETSTR union_self36;
 FSETSTR union_s36;
 FSETSTR ret_val81;
 FSETSTR union_self37;
 FSETSTR union_s37;
 FSETSTR ret_val82;
 FSETSTR union_self38;
 FSETSTR union_s38;
 FSETSTR ret_val83;
 FSETSTR union_self39;
 FSETSTR union_s39;
 FSETSTR ret_val84;
 FSETSTR union_self40;
 FSETSTR union_s40;
 FSETSTR ret_val85;
 FSETSTR union_self41;
 FSETSTR union_s41;
 FSETSTR ret_val86;
 FSETSTR union_self42;
 FSETSTR union_s42;
 FSETSTR ret_val87;
 FSETSTR union_self43;
 FSETSTR union_s43;
 FSETSTR ret_val88;
 FSETSTR union_self44;
 FSETSTR union_s44;
 FSETSTR ret_val89;
 FSETSTR create_self45;
 FSETSTR ret_val90;
 FSETSTR union_self45;
 FSETSTR union_s45;
 FSETSTR ret_val91;
 FSETSTR create_self46;
 FSETSTR ret_val92;
 FSETSTR union_self46;
 FSETSTR union_s46;
 FSETSTR ret_val93;
 FSETSTR create_self47;
 FSETSTR ret_val94;
 FSETSTR union_self47;
 FSETSTR union_s47;
 FSETSTR ret_val95;
 FSETSTR create_self48;
 FSETSTR ret_val96;
 FSETSTR union_self48;
 FSETSTR union_s48;
 FSETSTR ret_val97;
 FSETSTR create_self49;
 FSETSTR ret_val98;
 FSETSTR union_self49;
 FSETSTR union_s49;
 FSETSTR ret_val99;
 FSETSTR create_self50;
 FSETSTR ret_val100;
 FSETSTR union_self50;
 FSETSTR union_s50;
 FSETSTR ret_val101;
 FSETSTR create_self51;
 FSETSTR ret_val102;
 FSETSTR union_self51;
 FSETSTR union_s51;
 FSETSTR ret_val103;
 FSETSTR create_self52;
 FSETSTR ret_val104;
 FSETSTR union_self52;
 FSETSTR union_s52;
 FSETSTR ret_val105;
 FSETSTR create_self53;
 FSETSTR ret_val106;
 FSETSTR union_self53;
 FSETSTR union_s53;
 FSETSTR ret_val107;
 STR plus_self;
 STR plus_sarg;
 STR ret_val108;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val109;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val110;
 BFILE open_for_read_se;
 STR open_for_read_nm;
 BFILE ret_val1111;
 BFILE r;
 BFILE error_self;
 BOOL ret_val112 = BOOL_zero;
 ERR1 create_self54;
 ERR1 ret_val113;
 ERR1 plus_self3;
 dSTR plus_s;
 ERR1 ret_val114;
 FILE1 stderr_self;
 FILE1 ret_val115;
 FILE1 r1;
 ERR1 plus_self4;
 dSTR plus_s1;
 ERR1 ret_val116;
 FILE1 stderr_self1;
 FILE1 ret_val117;
 FILE1 r2;
 ERR1 plus_self5;
 dSTR plus_s2;
 ERR1 ret_val118;
 FILE1 stderr_self2;
 FILE1 ret_val119;
 FILE1 r3;
 ERR1 plus_self6;
 dSTR plus_s3;
 ERR1 ret_val120;
 FILE1 stderr_self3;
 FILE1 ret_val121;
 FILE1 r4;
 ERR1 plus_self7;
 dSTR plus_s4;
 FILE1 stderr_self4;
 FILE1 ret_val122;
 FILE1 r5;
 UNIX exit_self;
 INT exit_code = INT_zero;
 STR plus_self8;
 STR plus_sarg3;
 STR ret_val123;
 extern STR chk_all;
 extern STR chk;
 FSETSTR L2;
 FSETSTR L3;
 FSETSTR L4;
 FSETSTR L5;
 FSETSTR L6;
 FSETSTR L7;
 FSETSTR L8;
 FSETSTR L9;
 FSETSTR L10;
 extern STR chk;
 extern STR chk;
 FSETSTR L12;
 FSETSTR L13;
 FSETSTR L14;
 FSETSTR L15;
 FSETSTR L16;
 FSETSTR L17;
 FSETSTR L18;
 FSETSTR L19;
 FSETSTR L20;
 extern STR chk_arith;
 FSETSTR L21;
 extern STR chk_bounds;
 FSETSTR L22;
 extern STR chk_void;
 FSETSTR L23;
 extern STR chk_pre;
 FSETSTR L24;
 extern STR chk_post;
 FSETSTR L25;
 extern STR chk_assert;
 FSETSTR L26;
 extern STR chk_invariant;
 FSETSTR L27;
 extern STR chk_when;
 FSETSTR L28;
 extern STR chk_destroy;
 extern STR chk_psather;
 extern STR stats;
 extern STR psather_stats;
 extern STR psather_trace;
 extern STR show_am;
 extern STR return1;
 FSETSTR L29;
 extern STR chk_no_all;
 extern STR chk_no;
 FSETSTR L30;
 FSETSTR L31;
 FSETSTR L32;
 FSETSTR L33;
 FSETSTR L34;
 FSETSTR L35;
 FSETSTR L36;
 FSETSTR L37;
 FSETSTR L38;
 extern STR chk_no;
 extern STR chk_no;
 FSETSTR L39;
 FSETSTR L40;
 FSETSTR L41;
 FSETSTR L42;
 FSETSTR L43;
 FSETSTR L44;
 FSETSTR L45;
 FSETSTR L46;
 FSETSTR L47;
 extern STR chk_no_arith;
 FSETSTR L48;
 extern STR chk_no_bounds;
 FSETSTR L49;
 extern STR chk_no_void;
 FSETSTR L50;
 extern STR chk_no_pre;
 FSETSTR L51;
 extern STR chk_no_post;
 FSETSTR L52;
 extern STR chk_no_assert;
 FSETSTR L53;
 extern STR chk_no_invariant;
 FSETSTR L54;
 extern STR chk_no_destroy;
 extern STR chk_no_psather;
 extern STR no_stats;
 extern STR no_psather_stats;
 extern STR no_psather_trace;
 extern STR chk_no_return;
 FSETSTR L55;
 extern STR chk_no_when;
 FSETSTR L56;
 extern STR not_a_42245793;
 INT L57;
 INT L581_;
 CHAR L591_;
 CHAR L60;
 BOOL L611_;
 extern STR SystemPlatforms;
 INT L62;
 INT L631_;
 extern STR Platformmodule;
 BFILE L64;
 OB L65;
 extern STR rb;
 STR L66;
 STR L67;
 BOOL L68;
 extern STR Unreco482634771;
 FILE1 L69;
 OB L70;
 FILE1 L72;
 OB L73;
 extern STR orplatform;
 FILE1 L75;
 OB L76;
 INT L78;
 INT L791_;
 FILE1 L80;
 OB L81;
 extern STR missing1;
 FILE1 L83;
 OB L84;
 INT L86;
 INT L871_;
 FLISTSTR L88;
 extern STR Platform;
 L11 = s;
 if (STR_is111530248(L11, ((STR) &chk_all))) {
  if (STR_is111530248(s, ((STR) &chk))) {
   SATTR(self,all1,TRUE);
  }
  else {
   CODE_O1040697618(self);
  }
  SATTR(ATTR(self,prog),arith_checks,TRUE);
  SATTR(ATTR(self,prog),pre_checks,TRUE);
  SATTR(ATTR(self,prog),post_checks,TRUE);
  SATTR(ATTR(self,prog),assert_checks,TRUE);
  SATTR(ATTR(self,prog),void_checks,TRUE);
  if (ATTR(self,all1)) {
   SATTR(self,arith_all,TRUE);
   create_self = ((FSETSTR) NULL);
   ret_val = ((FSETSTR) NULL);
   SATTR(self,arith_out,ret_val);
   SATTR(self,bounds_all,TRUE);
   create_self1 = ((FSETSTR) NULL);
   ret_val1 = ((FSETSTR) NULL);
   SATTR(self,bounds_out,ret_val1);
   SATTR(self,void_all,TRUE);
   create_self2 = ((FSETSTR) NULL);
   ret_val2 = ((FSETSTR) NULL);
   SATTR(self,void_out,ret_val2);
   SATTR(self,pre_all,TRUE);
   create_self3 = ((FSETSTR) NULL);
   ret_val3 = ((FSETSTR) NULL);
   SATTR(self,pre_out,ret_val3);
   SATTR(self,post_all,TRUE);
   create_self4 = ((FSETSTR) NULL);
   ret_val4 = ((FSETSTR) NULL);
   SATTR(self,post_out,ret_val4);
   SATTR(self,assert_all,TRUE);
   create_self5 = ((FSETSTR) NULL);
   ret_val5 = ((FSETSTR) NULL);
   SATTR(self,assert_out,ret_val5);
   SATTR(self,invariant_all,TRUE);
   create_self6 = ((FSETSTR) NULL);
   ret_val6 = ((FSETSTR) NULL);
   SATTR(self,invariant_out,ret_val6);
   SATTR(self,when_all,TRUE);
   create_self7 = ((FSETSTR) NULL);
   ret_val7 = ((FSETSTR) NULL);
   SATTR(self,when_out,ret_val7);
   SATTR(self,return_all,TRUE);
   create_self8 = ((FSETSTR) NULL);
   ret_val8 = ((FSETSTR) NULL);
   SATTR(self,return_out,ret_val8);
   SATTR(self,destroy_chk,TRUE);
   SATTR(self,psather_chk,TRUE);
  }
  else {
   union_self = ATTR(self,arith_in);
   union_s = ATTR(self,classes1);
   L2 = FSETST1884046860(union_self);
   ret_val9 = FSETST866022026(L2, union_s);
   SATTR(self,arith_in,ret_val9);
   union_self1 = ATTR(self,bounds_in);
   union_s1 = ATTR(self,classes1);
   L3 = FSETST1884046860(union_self1);
   ret_val10 = FSETST866022026(L3, union_s1);
   SATTR(self,bounds_in,ret_val10);
   union_self2 = ATTR(self,void_in);
   union_s2 = ATTR(self,classes1);
   L4 = FSETST1884046860(union_self2);
   ret_val11 = FSETST866022026(L4, union_s2);
   SATTR(self,void_in,ret_val11);
   union_self3 = ATTR(self,pre_in);
   union_s3 = ATTR(self,classes1);
   L5 = FSETST1884046860(union_self3);
   ret_val12 = FSETST866022026(L5, union_s3);
   SATTR(self,pre_in,ret_val12);
   union_self4 = ATTR(self,post_in);
   union_s4 = ATTR(self,classes1);
   L6 = FSETST1884046860(union_self4);
   ret_val13 = FSETST866022026(L6, union_s4);
   SATTR(self,post_in,ret_val13);
   union_self5 = ATTR(self,assert_in);
   union_s5 = ATTR(self,classes1);
   L7 = FSETST1884046860(union_self5);
   ret_val14 = FSETST866022026(L7, union_s5);
   SATTR(self,assert_in,ret_val14);
   union_self6 = ATTR(self,invariant_in);
   union_s6 = ATTR(self,classes1);
   L8 = FSETST1884046860(union_self6);
   ret_val15 = FSETST866022026(L8, union_s6);
   SATTR(self,invariant_in,ret_val15);
   union_self7 = ATTR(self,when_in);
   union_s7 = ATTR(self,classes1);
   L9 = FSETST1884046860(union_self7);
   ret_val16 = FSETST866022026(L9, union_s7);
   SATTR(self,when_in,ret_val16);
   union_self8 = ATTR(self,return_in);
   union_s8 = ATTR(self,classes1);
   L10 = FSETST1884046860(union_self8);
   ret_val17 = FSETST866022026(L10, union_s8);
   SATTR(self,return_in,ret_val17);
   SATTR(self,arith_out,FSETST897681893(ATTR(self,arith_out), ATTR(self,classes1)));
   SATTR(self,bounds_out,FSETST897681893(ATTR(self,bounds_out), ATTR(self,classes1)));
   SATTR(self,void_out,FSETST897681893(ATTR(self,void_out), ATTR(self,classes1)));
   SATTR(self,pre_out,FSETST897681893(ATTR(self,pre_out), ATTR(self,classes1)));
   SATTR(self,post_out,FSETST897681893(ATTR(self,post_out), ATTR(self,classes1)));
   SATTR(self,assert_out,FSETST897681893(ATTR(self,assert_out), ATTR(self,classes1)));
   SATTR(self,invariant_out,FSETST897681893(ATTR(self,invariant_out), ATTR(self,classes1)));
   SATTR(self,when_out,FSETST897681893(ATTR(self,when_out), ATTR(self,classes1)));
   SATTR(self,return_out,FSETST897681893(ATTR(self,return_out), ATTR(self,classes1)));
  }
 }
 else {
  if (STR_is111530248(L11, ((STR) &chk))) {
   if (STR_is111530248(s, ((STR) &chk))) {
    SATTR(self,all1,TRUE);
   }
   else {
    CODE_O1040697618(self);
   }
   SATTR(ATTR(self,prog),arith_checks,TRUE);
   SATTR(ATTR(self,prog),pre_checks,TRUE);
   SATTR(ATTR(self,prog),post_checks,TRUE);
   SATTR(ATTR(self,prog),assert_checks,TRUE);
   SATTR(ATTR(self,prog),void_checks,TRUE);
   if (ATTR(self,all1)) {
    SATTR(self,arith_all,TRUE);
    create_self9 = ((FSETSTR) NULL);
    ret_val18 = ((FSETSTR) NULL);
    SATTR(self,arith_out,ret_val18);
    SATTR(self,bounds_all,TRUE);
    create_self10 = ((FSETSTR) NULL);
    ret_val19 = ((FSETSTR) NULL);
    SATTR(self,bounds_out,ret_val19);
    SATTR(self,void_all,TRUE);
    create_self11 = ((FSETSTR) NULL);
    ret_val20 = ((FSETSTR) NULL);
    SATTR(self,void_out,ret_val20);
    SATTR(self,pre_all,TRUE);
    create_self12 = ((FSETSTR) NULL);
    ret_val21 = ((FSETSTR) NULL);
    SATTR(self,pre_out,ret_val21);
    SATTR(self,post_all,TRUE);
    create_self13 = ((FSETSTR) NULL);
    ret_val22 = ((FSETSTR) NULL);
    SATTR(self,post_out,ret_val22);
    SATTR(self,assert_all,TRUE);
    create_self14 = ((FSETSTR) NULL);
    ret_val23 = ((FSETSTR) NULL);
    SATTR(self,assert_out,ret_val23);
    SATTR(self,invariant_all,TRUE);
    create_self15 = ((FSETSTR) NULL);
    ret_val24 = ((FSETSTR) NULL);
    SATTR(self,invariant_out,ret_val24);
    SATTR(self,when_all,TRUE);
    create_self16 = ((FSETSTR) NULL);
    ret_val25 = ((FSETSTR) NULL);
    SATTR(self,when_out,ret_val25);
    SATTR(self,return_all,TRUE);
    create_self17 = ((FSETSTR) NULL);
    ret_val26 = ((FSETSTR) NULL);
    SATTR(self,return_out,ret_val26);
    SATTR(self,destroy_chk,TRUE);
    SATTR(self,psather_chk,TRUE);
   }
   else {
    union_self9 = ATTR(self,arith_in);
    union_s9 = ATTR(self,classes1);
    L12 = FSETST1884046860(union_self9);
    ret_val27 = FSETST866022026(L12, union_s9);
    SATTR(self,arith_in,ret_val27);
    union_self10 = ATTR(self,bounds_in);
    union_s10 = ATTR(self,classes1);
    L13 = FSETST1884046860(union_self10);
    ret_val28 = FSETST866022026(L13, union_s10);
    SATTR(self,bounds_in,ret_val28);
    union_self11 = ATTR(self,void_in);
    union_s11 = ATTR(self,classes1);
    L14 = FSETST1884046860(union_self11);
    ret_val29 = FSETST866022026(L14, union_s11);
    SATTR(self,void_in,ret_val29);
    union_self12 = ATTR(self,pre_in);
    union_s12 = ATTR(self,classes1);
    L15 = FSETST1884046860(union_self12);
    ret_val30 = FSETST866022026(L15, union_s12);
    SATTR(self,pre_in,ret_val30);
    union_self13 = ATTR(self,post_in);
    union_s13 = ATTR(self,classes1);
    L16 = FSETST1884046860(union_self13);
    ret_val31 = FSETST866022026(L16, union_s13);
    SATTR(self,post_in,ret_val31);
    union_self14 = ATTR(self,assert_in);
    union_s14 = ATTR(self,classes1);
    L17 = FSETST1884046860(union_self14);
    ret_val32 = FSETST866022026(L17, union_s14);
    SATTR(self,assert_in,ret_val32);
    union_self15 = ATTR(self,invariant_in);
    union_s15 = ATTR(self,classes1);
    L18 = FSETST1884046860(union_self15);
    ret_val33 = FSETST866022026(L18, union_s15);
    SATTR(self,invariant_in,ret_val33);
    union_self16 = ATTR(self,when_in);
    union_s16 = ATTR(self,classes1);
    L19 = FSETST1884046860(union_self16);
    ret_val34 = FSETST866022026(L19, union_s16);
    SATTR(self,when_in,ret_val34);
    union_self17 = ATTR(self,return_in);
    union_s17 = ATTR(self,classes1);
    L20 = FSETST1884046860(union_self17);
    ret_val35 = FSETST866022026(L20, union_s17);
    SATTR(self,return_in,ret_val35);
    SATTR(self,arith_out,FSETST897681893(ATTR(self,arith_out), ATTR(self,classes1)));
    SATTR(self,bounds_out,FSETST897681893(ATTR(self,bounds_out), ATTR(self,classes1)));
    SATTR(self,void_out,FSETST897681893(ATTR(self,void_out), ATTR(self,classes1)));
    SATTR(self,pre_out,FSETST897681893(ATTR(self,pre_out), ATTR(self,classes1)));
    SATTR(self,post_out,FSETST897681893(ATTR(self,post_out), ATTR(self,classes1)));
    SATTR(self,assert_out,FSETST897681893(ATTR(self,assert_out), ATTR(self,classes1)));
    SATTR(self,invariant_out,FSETST897681893(ATTR(self,invariant_out), ATTR(self,classes1)));
    SATTR(self,when_out,FSETST897681893(ATTR(self,when_out), ATTR(self,classes1)));
    SATTR(self,return_out,FSETST897681893(ATTR(self,return_out), ATTR(self,classes1)));
   }
  }
  else {
   if (STR_is111530248(L11, ((STR) &chk_arith))) {
    CODE_O1040697618(self);
    SATTR(ATTR(self,prog),arith_checks,TRUE);
    if (ATTR(self,all1)) {
     SATTR(self,arith_all,TRUE);
     create_self18 = ((FSETSTR) NULL);
     ret_val36 = ((FSETSTR) NULL);
     SATTR(self,arith_out,ret_val36);
    }
    else {
     union_self18 = ATTR(self,arith_in);
     union_s18 = ATTR(self,classes1);
     L21 = FSETST1884046860(union_self18);
     ret_val37 = FSETST866022026(L21, union_s18);
     SATTR(self,arith_in,ret_val37);
     SATTR(self,arith_out,FSETST897681893(ATTR(self,arith_out), ATTR(self,classes1)));
    }
   }
   else {
    if (STR_is111530248(L11, ((STR) &chk_bounds))) {
     CODE_O1040697618(self);
     if (ATTR(self,all1)) {
      SATTR(self,bounds_all,TRUE);
      create_self19 = ((FSETSTR) NULL);
      ret_val38 = ((FSETSTR) NULL);
      SATTR(self,bounds_out,ret_val38);
     }
     else {
      union_self19 = ATTR(self,bounds_in);
      union_s19 = ATTR(self,classes1);
      L22 = FSETST1884046860(union_self19);
      ret_val39 = FSETST866022026(L22, union_s19);
      SATTR(self,bounds_in,ret_val39);
      SATTR(self,bounds_out,FSETST897681893(ATTR(self,bounds_out), ATTR(self,classes1)));
     }
    }
    else {
     if (STR_is111530248(L11, ((STR) &chk_void))) {
      CODE_O1040697618(self);
      SATTR(ATTR(self,prog),void_checks,TRUE);
      if (ATTR(self,all1)) {
       SATTR(self,void_all,TRUE);
       create_self20 = ((FSETSTR) NULL);
       ret_val40 = ((FSETSTR) NULL);
       SATTR(self,void_out,ret_val40);
      }
      else {
       union_self20 = ATTR(self,void_in);
       union_s20 = ATTR(self,classes1);
       L23 = FSETST1884046860(union_self20);
       ret_val41 = FSETST866022026(L23, union_s20);
       SATTR(self,void_in,ret_val41);
       SATTR(self,void_out,FSETST897681893(ATTR(self,void_out), ATTR(self,classes1)));
      }
     }
     else {
      if (STR_is111530248(L11, ((STR) &chk_pre))) {
       SATTR(ATTR(self,prog),pre_checks,TRUE);
       CODE_O1040697618(self);
       if (ATTR(self,all1)) {
        SATTR(self,pre_all,TRUE);
        create_self21 = ((FSETSTR) NULL);
        ret_val42 = ((FSETSTR) NULL);
        SATTR(self,pre_out,ret_val42);
       }
       else {
        union_self21 = ATTR(self,pre_in);
        union_s21 = ATTR(self,classes1);
        L24 = FSETST1884046860(union_self21);
        ret_val43 = FSETST866022026(L24, union_s21);
        SATTR(self,pre_in,ret_val43);
        SATTR(self,pre_out,FSETST897681893(ATTR(self,pre_out), ATTR(self,classes1)));
       }
      }
      else {
       if (STR_is111530248(L11, ((STR) &chk_post))) {
        SATTR(ATTR(self,prog),post_checks,TRUE);
        CODE_O1040697618(self);
        if (ATTR(self,all1)) {
         SATTR(self,post_all,TRUE);
         create_self22 = ((FSETSTR) NULL);
         ret_val44 = ((FSETSTR) NULL);
         SATTR(self,post_out,ret_val44);
        }
        else {
         union_self22 = ATTR(self,post_in);
         union_s22 = ATTR(self,classes1);
         L25 = FSETST1884046860(union_self22);
         ret_val45 = FSETST866022026(L25, union_s22);
         SATTR(self,post_in,ret_val45);
         SATTR(self,post_out,FSETST897681893(ATTR(self,post_out), ATTR(self,classes1)));
        }
       }
       else {
        if (STR_is111530248(L11, ((STR) &chk_assert))) {
         SATTR(ATTR(self,prog),assert_checks,TRUE);
         CODE_O1040697618(self);
         if (ATTR(self,all1)) {
          SATTR(self,assert_all,TRUE);
          create_self23 = ((FSETSTR) NULL);
          ret_val46 = ((FSETSTR) NULL);
          SATTR(self,assert_out,ret_val46);
         }
         else {
          union_self23 = ATTR(self,assert_in);
          union_s23 = ATTR(self,classes1);
          L26 = FSETST1884046860(union_self23);
          ret_val47 = FSETST866022026(L26, union_s23);
          SATTR(self,assert_in,ret_val47);
          SATTR(self,assert_out,FSETST897681893(ATTR(self,assert_out), ATTR(self,classes1)));
         }
        }
        else {
         if (STR_is111530248(L11, ((STR) &chk_invariant))) {
          CODE_O1040697618(self);
          if (ATTR(self,all1)) {
           SATTR(self,invariant_all,TRUE);
           create_self24 = ((FSETSTR) NULL);
           ret_val48 = ((FSETSTR) NULL);
           SATTR(self,invariant_out,ret_val48);
          }
          else {
           union_self24 = ATTR(self,invariant_in);
           union_s24 = ATTR(self,classes1);
           L27 = FSETST1884046860(union_self24);
           ret_val49 = FSETST866022026(L27, union_s24);
           SATTR(self,invariant_in,ret_val49);
           SATTR(self,invariant_out,FSETST897681893(ATTR(self,invariant_out), ATTR(self,classes1)));
          }
         }
         else {
          if (STR_is111530248(L11, ((STR) &chk_when))) {
           CODE_O1040697618(self);
           if (ATTR(self,all1)) {
            SATTR(self,when_all,TRUE);
            create_self25 = ((FSETSTR) NULL);
            ret_val50 = ((FSETSTR) NULL);
            SATTR(self,when_out,ret_val50);
           }
           else {
            union_self25 = ATTR(self,when_in);
            union_s25 = ATTR(self,classes1);
            L28 = FSETST1884046860(union_self25);
            ret_val51 = FSETST866022026(L28, union_s25);
            SATTR(self,when_in,ret_val51);
            SATTR(self,when_out,FSETST897681893(ATTR(self,when_out), ATTR(self,classes1)));
           }
          }
          else {
           if (STR_is111530248(L11, ((STR) &chk_destroy))) {
            SATTR(self,destroy_chk,TRUE);
           }
           else {
            if (STR_is111530248(L11, ((STR) &chk_psather))) {
             SATTR(self,psather_chk,TRUE);
            }
            else {
             if (STR_is111530248(L11, ((STR) &stats))) {
              SATTR(self,stats,TRUE);
             }
             else {
              if (STR_is111530248(L11, ((STR) &psather_stats))) {
               SATTR(self,psather_stats,TRUE);
              }
              else {
               if (STR_is111530248(L11, ((STR) &psather_trace))) {
                SATTR(self,psather_trace,TRUE);
               }
               else {
                if (STR_is111530248(L11, ((STR) &show_am))) {
                 SATTR(ATTR(self,prog),show_am,TRUE);
                }
                else {
                 if (STR_is111530248(L11, ((STR) &return1))) {
                  CODE_O1040697618(self);
                  if (ATTR(self,all1)) {
                   SATTR(self,return_all,TRUE);
                   create_self26 = ((FSETSTR) NULL);
                   ret_val52 = ((FSETSTR) NULL);
                   SATTR(self,return_out,ret_val52);
                  }
                  else {
                   union_self26 = ATTR(self,return_in);
                   union_s26 = ATTR(self,classes1);
                   L29 = FSETST1884046860(union_self26);
                   ret_val53 = FSETST866022026(L29, union_s26);
                   SATTR(self,return_in,ret_val53);
                   SATTR(self,return_out,FSETST897681893(ATTR(self,return_out), ATTR(self,classes1)));
                  }
                 }
                 else {
                  if (STR_is111530248(L11, ((STR) &chk_no_all))) {
                   if (STR_is111530248(s, ((STR) &chk_no))) {
                    SATTR(self,all1,TRUE);
                   }
                   else {
                    CODE_O1040697618(self);
                   }
                   if (ATTR(self,all1)) {
                    SATTR(ATTR(self,prog),void_checks,FALSE);
                    SATTR(ATTR(self,prog),pre_checks,FALSE);
                    SATTR(ATTR(self,prog),post_checks,FALSE);
                    SATTR(ATTR(self,prog),assert_checks,FALSE);
                    SATTR(ATTR(self,prog),arith_checks,FALSE);
                    create_self27 = ((FSETSTR) NULL);
                    ret_val54 = ((FSETSTR) NULL);
                    SATTR(self,arith_in,ret_val54);
                    SATTR(self,arith_all,FALSE);
                    create_self28 = ((FSETSTR) NULL);
                    ret_val55 = ((FSETSTR) NULL);
                    SATTR(self,bounds_in,ret_val55);
                    SATTR(self,bounds_all,FALSE);
                    create_self29 = ((FSETSTR) NULL);
                    ret_val56 = ((FSETSTR) NULL);
                    SATTR(self,void_in,ret_val56);
                    SATTR(self,void_all,FALSE);
                    create_self30 = ((FSETSTR) NULL);
                    ret_val57 = ((FSETSTR) NULL);
                    SATTR(self,pre_in,ret_val57);
                    SATTR(self,pre_all,FALSE);
                    create_self31 = ((FSETSTR) NULL);
                    ret_val58 = ((FSETSTR) NULL);
                    SATTR(self,post_in,ret_val58);
                    SATTR(self,post_all,FALSE);
                    create_self32 = ((FSETSTR) NULL);
                    ret_val59 = ((FSETSTR) NULL);
                    SATTR(self,assert_in,ret_val59);
                    SATTR(self,assert_all,FALSE);
                    create_self33 = ((FSETSTR) NULL);
                    ret_val60 = ((FSETSTR) NULL);
                    SATTR(self,invariant_in,ret_val60);
                    SATTR(self,invariant_all,FALSE);
                    create_self34 = ((FSETSTR) NULL);
                    ret_val61 = ((FSETSTR) NULL);
                    SATTR(self,when_in,ret_val61);
                    SATTR(self,when_all,FALSE);
                    SATTR(self,destroy_chk,FALSE);
                    create_self35 = ((FSETSTR) NULL);
                    ret_val62 = ((FSETSTR) NULL);
                    SATTR(self,return_in,ret_val62);
                    SATTR(self,return_all,FALSE);
                    SATTR(self,psather_chk,FALSE);
                   }
                   else {
                    union_self27 = ATTR(self,arith_out);
                    union_s27 = ATTR(self,classes1);
                    L30 = FSETST1884046860(union_self27);
                    ret_val63 = FSETST866022026(L30, union_s27);
                    SATTR(self,arith_out,ret_val63);
                    union_self28 = ATTR(self,bounds_out);
                    union_s28 = ATTR(self,classes1);
                    L31 = FSETST1884046860(union_self28);
                    ret_val64 = FSETST866022026(L31, union_s28);
                    SATTR(self,bounds_out,ret_val64);
                    union_self29 = ATTR(self,void_out);
                    union_s29 = ATTR(self,classes1);
                    L32 = FSETST1884046860(union_self29);
                    ret_val65 = FSETST866022026(L32, union_s29);
                    SATTR(self,void_out,ret_val65);
                    union_self30 = ATTR(self,pre_out);
                    union_s30 = ATTR(self,classes1);
                    L33 = FSETST1884046860(union_self30);
                    ret_val66 = FSETST866022026(L33, union_s30);
                    SATTR(self,pre_out,ret_val66);
                    union_self31 = ATTR(self,post_out);
                    union_s31 = ATTR(self,classes1);
                    L34 = FSETST1884046860(union_self31);
                    ret_val67 = FSETST866022026(L34, union_s31);
                    SATTR(self,post_out,ret_val67);
                    union_self32 = ATTR(self,assert_out);
                    union_s32 = ATTR(self,classes1);
                    L35 = FSETST1884046860(union_self32);
                    ret_val68 = FSETST866022026(L35, union_s32);
                    SATTR(self,assert_out,ret_val68);
                    union_self33 = ATTR(self,invariant_out);
                    union_s33 = ATTR(self,classes1);
                    L36 = FSETST1884046860(union_self33);
                    ret_val69 = FSETST866022026(L36, union_s33);
                    SATTR(self,invariant_out,ret_val69);
                    union_self34 = ATTR(self,when_out);
                    union_s34 = ATTR(self,classes1);
                    L37 = FSETST1884046860(union_self34);
                    ret_val70 = FSETST866022026(L37, union_s34);
                    SATTR(self,when_out,ret_val70);
                    union_self35 = ATTR(self,return_out);
                    union_s35 = ATTR(self,classes1);
                    L38 = FSETST1884046860(union_self35);
                    ret_val71 = FSETST866022026(L38, union_s35);
                    SATTR(self,return_out,ret_val71);
                   }
                  }
                  else {
                   if (STR_is111530248(L11, ((STR) &chk_no))) {
                    if (STR_is111530248(s, ((STR) &chk_no))) {
                     SATTR(self,all1,TRUE);
                    }
                    else {
                     CODE_O1040697618(self);
                    }
                    if (ATTR(self,all1)) {
                     SATTR(ATTR(self,prog),void_checks,FALSE);
                     SATTR(ATTR(self,prog),pre_checks,FALSE);
                     SATTR(ATTR(self,prog),post_checks,FALSE);
                     SATTR(ATTR(self,prog),assert_checks,FALSE);
                     SATTR(ATTR(self,prog),arith_checks,FALSE);
                     create_self36 = ((FSETSTR) NULL);
                     ret_val72 = ((FSETSTR) NULL);
                     SATTR(self,arith_in,ret_val72);
                     SATTR(self,arith_all,FALSE);
                     create_self37 = ((FSETSTR) NULL);
                     ret_val73 = ((FSETSTR) NULL);
                     SATTR(self,bounds_in,ret_val73);
                     SATTR(self,bounds_all,FALSE);
                     create_self38 = ((FSETSTR) NULL);
                     ret_val74 = ((FSETSTR) NULL);
                     SATTR(self,void_in,ret_val74);
                     SATTR(self,void_all,FALSE);
                     create_self39 = ((FSETSTR) NULL);
                     ret_val75 = ((FSETSTR) NULL);
                     SATTR(self,pre_in,ret_val75);
                     SATTR(self,pre_all,FALSE);
                     create_self40 = ((FSETSTR) NULL);
                     ret_val76 = ((FSETSTR) NULL);
                     SATTR(self,post_in,ret_val76);
                     SATTR(self,post_all,FALSE);
                     create_self41 = ((FSETSTR) NULL);
                     ret_val77 = ((FSETSTR) NULL);
                     SATTR(self,assert_in,ret_val77);
                     SATTR(self,assert_all,FALSE);
                     create_self42 = ((FSETSTR) NULL);
                     ret_val78 = ((FSETSTR) NULL);
                     SATTR(self,invariant_in,ret_val78);
                     SATTR(self,invariant_all,FALSE);
                     create_self43 = ((FSETSTR) NULL);
                     ret_val79 = ((FSETSTR) NULL);
                     SATTR(self,when_in,ret_val79);
                     SATTR(self,when_all,FALSE);
                     SATTR(self,destroy_chk,FALSE);
                     create_self44 = ((FSETSTR) NULL);
                     ret_val80 = ((FSETSTR) NULL);
                     SATTR(self,return_in,ret_val80);
                     SATTR(self,return_all,FALSE);
                     SATTR(self,psather_chk,FALSE);
                    }
                    else {
                     union_self36 = ATTR(self,arith_out);
                     union_s36 = ATTR(self,classes1);
                     L39 = FSETST1884046860(union_self36);
                     ret_val81 = FSETST866022026(L39, union_s36);
                     SATTR(self,arith_out,ret_val81);
                     union_self37 = ATTR(self,bounds_out);
                     union_s37 = ATTR(self,classes1);
                     L40 = FSETST1884046860(union_self37);
                     ret_val82 = FSETST866022026(L40, union_s37);
                     SATTR(self,bounds_out,ret_val82);
                     union_self38 = ATTR(self,void_out);
                     union_s38 = ATTR(self,classes1);
                     L41 = FSETST1884046860(union_self38);
                     ret_val83 = FSETST866022026(L41, union_s38);
                     SATTR(self,void_out,ret_val83);
                     union_self39 = ATTR(self,pre_out);
                     union_s39 = ATTR(self,classes1);
                     L42 = FSETST1884046860(union_self39);
                     ret_val84 = FSETST866022026(L42, union_s39);
                     SATTR(self,pre_out,ret_val84);
                     union_self40 = ATTR(self,post_out);
                     union_s40 = ATTR(self,classes1);
                     L43 = FSETST1884046860(union_self40);
                     ret_val85 = FSETST866022026(L43, union_s40);
                     SATTR(self,post_out,ret_val85);
                     union_self41 = ATTR(self,assert_out);
                     union_s41 = ATTR(self,classes1);
                     L44 = FSETST1884046860(union_self41);
                     ret_val86 = FSETST866022026(L44, union_s41);
                     SATTR(self,assert_out,ret_val86);
                     union_self42 = ATTR(self,invariant_out);
                     union_s42 = ATTR(self,classes1);
                     L45 = FSETST1884046860(union_self42);
                     ret_val87 = FSETST866022026(L45, union_s42);
                     SATTR(self,invariant_out,ret_val87);
                     union_self43 = ATTR(self,when_out);
                     union_s43 = ATTR(self,classes1);
                     L46 = FSETST1884046860(union_self43);
                     ret_val88 = FSETST866022026(L46, union_s43);
                     SATTR(self,when_out,ret_val88);
                     union_self44 = ATTR(self,return_out);
                     union_s44 = ATTR(self,classes1);
                     L47 = FSETST1884046860(union_self44);
                     ret_val89 = FSETST866022026(L47, union_s44);
                     SATTR(self,return_out,ret_val89);
                    }
                   }
                   else {
                    if (STR_is111530248(L11, ((STR) &chk_no_arith))) {
                     CODE_O1040697618(self);
                     if (ATTR(self,all1)) {
                      SATTR(ATTR(self,prog),arith_checks,FALSE);
                      create_self45 = ((FSETSTR) NULL);
                      ret_val90 = ((FSETSTR) NULL);
                      SATTR(self,arith_in,ret_val90);
                      SATTR(self,arith_all,FALSE);
                     }
                     else {
                      union_self45 = ATTR(self,arith_out);
                      union_s45 = ATTR(self,classes1);
                      L48 = FSETST1884046860(union_self45);
                      ret_val91 = FSETST866022026(L48, union_s45);
                      SATTR(self,arith_out,ret_val91);
                     }
                    }
                    else {
                     if (STR_is111530248(L11, ((STR) &chk_no_bounds))) {
                      CODE_O1040697618(self);
                      if (ATTR(self,all1)) {
                       create_self46 = ((FSETSTR) NULL);
                       ret_val92 = ((FSETSTR) NULL);
                       SATTR(self,bounds_in,ret_val92);
                       SATTR(self,bounds_all,FALSE);
                      }
                      else {
                       union_self46 = ATTR(self,bounds_out);
                       union_s46 = ATTR(self,classes1);
                       L49 = FSETST1884046860(union_self46);
                       ret_val93 = FSETST866022026(L49, union_s46);
                       SATTR(self,bounds_out,ret_val93);
                      }
                     }
                     else {
                      if (STR_is111530248(L11, ((STR) &chk_no_void))) {
                       CODE_O1040697618(self);
                       if (ATTR(self,all1)) {
                        SATTR(ATTR(self,prog),void_checks,FALSE);
                        create_self47 = ((FSETSTR) NULL);
                        ret_val94 = ((FSETSTR) NULL);
                        SATTR(self,void_in,ret_val94);
                        SATTR(self,void_all,FALSE);
                       }
                       else {
                        union_self47 = ATTR(self,void_out);
                        union_s47 = ATTR(self,classes1);
                        L50 = FSETST1884046860(union_self47);
                        ret_val95 = FSETST866022026(L50, union_s47);
                        SATTR(self,void_out,ret_val95);
                       }
                      }
                      else {
                       if (STR_is111530248(L11, ((STR) &chk_no_pre))) {
                        CODE_O1040697618(self);
                        if (ATTR(self,all1)) {
                         SATTR(ATTR(self,prog),pre_checks,FALSE);
                         create_self48 = ((FSETSTR) NULL);
                         ret_val96 = ((FSETSTR) NULL);
                         SATTR(self,pre_in,ret_val96);
                         SATTR(self,pre_all,FALSE);
                        }
                        else {
                         union_self48 = ATTR(self,pre_out);
                         union_s48 = ATTR(self,classes1);
                         L51 = FSETST1884046860(union_self48);
                         ret_val97 = FSETST866022026(L51, union_s48);
                         SATTR(self,pre_out,ret_val97);
                        }
                       }
                       else {
                        if (STR_is111530248(L11, ((STR) &chk_no_post))) {
                         CODE_O1040697618(self);
                         if (ATTR(self,all1)) {
                          SATTR(ATTR(self,prog),post_checks,FALSE);
                          create_self49 = ((FSETSTR) NULL);
                          ret_val98 = ((FSETSTR) NULL);
                          SATTR(self,post_in,ret_val98);
                          SATTR(self,post_all,FALSE);
                         }
                         else {
                          union_self49 = ATTR(self,post_out);
                          union_s49 = ATTR(self,classes1);
                          L52 = FSETST1884046860(union_self49);
                          ret_val99 = FSETST866022026(L52, union_s49);
                          SATTR(self,post_out,ret_val99);
                         }
                        }
                        else {
                         if (STR_is111530248(L11, ((STR) &chk_no_assert))) {
                          CODE_O1040697618(self);
                          if (ATTR(self,all1)) {
                           SATTR(ATTR(self,prog),assert_checks,FALSE);
                           create_self50 = ((FSETSTR) NULL);
                           ret_val100 = ((FSETSTR) NULL);
                           SATTR(self,assert_in,ret_val100);
                           SATTR(self,assert_all,FALSE);
                          }
                          else {
                           union_self50 = ATTR(self,assert_out);
                           union_s50 = ATTR(self,classes1);
                           L53 = FSETST1884046860(union_self50);
                           ret_val101 = FSETST866022026(L53, union_s50);
                           SATTR(self,assert_out,ret_val101);
                          }
                         }
                         else {
                          if (STR_is111530248(L11, ((STR) &chk_no_invariant))) {
                           CODE_O1040697618(self);
                           if (ATTR(self,all1)) {
                            create_self51 = ((FSETSTR) NULL);
                            ret_val102 = ((FSETSTR) NULL);
                            SATTR(self,invariant_in,ret_val102);
                            SATTR(self,invariant_all,FALSE);
                           }
                           else {
                            union_self51 = ATTR(self,invariant_out);
                            union_s51 = ATTR(self,classes1);
                            L54 = FSETST1884046860(union_self51);
                            ret_val103 = FSETST866022026(L54, union_s51);
                            SATTR(self,invariant_out,ret_val103);
                           }
                          }
                          else {
                           if (STR_is111530248(L11, ((STR) &chk_no_destroy))) {
                            SATTR(self,destroy_chk,FALSE);
                           }
                           else {
                            if (STR_is111530248(L11, ((STR) &chk_no_psather))) {
                             SATTR(self,psather_chk,FALSE);
                            }
                            else {
                             if (STR_is111530248(L11, ((STR) &no_stats))) {
                              SATTR(self,stats,FALSE);
                             }
                             else {
                              if (STR_is111530248(L11, ((STR) &no_psather_stats))) {
                               SATTR(self,psather_stats,FALSE);
                              }
                              else {
                               if (STR_is111530248(L11, ((STR) &no_psather_trace))) {
                                SATTR(self,psather_trace,FALSE);
                               }
                               else {
                                if (STR_is111530248(L11, ((STR) &chk_no_return))) {
                                 CODE_O1040697618(self);
                                 if (ATTR(self,all1)) {
                                  create_self52 = ((FSETSTR) NULL);
                                  ret_val104 = ((FSETSTR) NULL);
                                  SATTR(self,return_in,ret_val104);
                                  SATTR(self,return_all,FALSE);
                                 }
                                 else {
                                  union_self52 = ATTR(self,return_out);
                                  union_s52 = ATTR(self,classes1);
                                  L55 = FSETST1884046860(union_self52);
                                  ret_val105 = FSETST866022026(L55, union_s52);
                                  SATTR(self,return_out,ret_val105);
                                 }
                                }
                                else {
                                 if (STR_is111530248(L11, ((STR) &chk_no_when))) {
                                  CODE_O1040697618(self);
                                  if (ATTR(self,all1)) {
                                   create_self53 = ((FSETSTR) NULL);
                                   ret_val106 = ((FSETSTR) NULL);
                                   SATTR(self,when_in,ret_val106);
                                   SATTR(self,when_all,FALSE);
                                  }
                                  else {
                                   union_self53 = ATTR(self,when_out);
                                   union_s53 = ATTR(self,classes1);
                                   L56 = FSETST1884046860(union_self53);
                                   ret_val107 = FSETST866022026(L56, union_s53);
                                   SATTR(self,when_out,ret_val107);
                                  }
                                 }
                                 else {
                                  if (STR_is111530248(L11, ((STR) &not_a_42245793))) {
                                   L57 = ATTR(ATTR(self,prog),genuine_platform);
                                   L581_=INTMINUS(L57,1); 
                                   SATTR(ATTR(self,prog),genuine_platform,L581_);
                                  }
                                  else {
                                   L591_=ARR((STR)s,0); 
                                   L60 = L591_;
                                   L611_=L60=='-'; 
                                   if (L611_) {
                                    plus_self = ATTR(ATTR(self,prog),home);
                                    plus_sarg = ((STR) &SystemPlatforms);
                                    ret_val108 = STR_ap2004550586(plus_self, plus_sarg);
                                    plus_self1 = ret_val108;
                                    L62 = STR_lengthrINT(s);
                                    L631_=INTMINUS(L62,1); 
                                    plus_sarg1 = STR_tail_INTrSTR(s, L631_);
                                    ret_val109 = STR_ap2004550586(plus_self1, plus_sarg1);
                                    cfs = ret_val109;
                                    plus_self2 = cfs;
                                    plus_sarg2 = ((STR) &Platformmodule);
                                    ret_val110 = STR_ap2004550586(plus_self2, plus_sarg2);
                                    platform_module = ret_val110;
                                    open_for_read_se = ((BFILE) NULL);
                                    open_for_read_nm = platform_module;
                                    L65=ZALLOC(sizeof(struct BFILE_struct));
                                    if (L65==NULL) FATAL("Unable to allocate more memory");
                                    ((OB)L65)->header.tag=BFILE_tag;
                                    L64 = ((BFILE) L65);
                                    r = L64;
                                    L66 = open_for_read_nm;
                                    L67 = ((STR) &rb);
                                    SATTR(r,fp,fopen(((L66==NULL)?NULL:L66->arr_part), ((L67==NULL)?NULL:L67->arr_part)));
                                    ret_val1111 = r;
                                    f = ret_val1111;
                                    error_self = f;
                                    if ((ATTR(error_self,fp)==((EXT_OB) NULL))) {
                                     L68 = TRUE;
                                    } else {
                                     L68 = ferror(ATTR(error_self,fp));
                                    }
                                    ret_val112 = L68;
                                    if (ret_val112) {
                                     create_self54 = ((ERR1) NULL);
                                     ret_val113 = create_self54;
                                     plus_self3 = ret_val113;
                                     plus_s = ((dSTR) ((STR) &Unreco482634771));
                                     stderr_self = ((FILE1) NULL);
                                     L70=ZALLOC(sizeof(struct FILE1_struct));
                                     if (L70==NULL) FATAL("Unable to allocate more memory");
                                     ((OB)L70)->header.tag=FILE1_tag;
                                     L69 = ((FILE1) L70);
                                     r1 = L69;
                                     
                                     SATTR(r1,fp,stderr);
                                     ret_val115 = r1;
                                     FILE_plus_dSTR(ret_val115, plus_s);
                                     ret_val114 = plus_self3;
                                     plus_self4 = ret_val114;
                                     plus_s1 = ((dSTR) s);
                                     stderr_self1 = ((FILE1) NULL);
                                     L73=ZALLOC(sizeof(struct FILE1_struct));
                                     if (L73==NULL) FATAL("Unable to allocate more memory");
                                     ((OB)L73)->header.tag=FILE1_tag;
                                     L72 = ((FILE1) L73);
                                     r2 = L72;
                                     
                                     SATTR(r2,fp,stderr);
                                     ret_val117 = r2;
                                     FILE_plus_dSTR(ret_val117, plus_s1);
                                     ret_val116 = plus_self4;
                                     plus_self5 = ret_val116;
                                     plus_s2 = ((dSTR) ((STR) &orplatform));
                                     stderr_self2 = ((FILE1) NULL);
                                     L76=ZALLOC(sizeof(struct FILE1_struct));
                                     if (L76==NULL) FATAL("Unable to allocate more memory");
                                     ((OB)L76)->header.tag=FILE1_tag;
                                     L75 = ((FILE1) L76);
                                     r3 = L75;
                                     
                                     SATTR(r3,fp,stderr);
                                     ret_val119 = r3;
                                     FILE_plus_dSTR(ret_val119, plus_s2);
                                     ret_val118 = plus_self5;
                                     plus_self6 = ret_val118;
                                     L78 = STR_lengthrINT(s);
                                     L791_=INTMINUS(L78,1); 
                                     plus_s3 = ((dSTR) STR_tail_INTrSTR(s, L791_));
                                     stderr_self3 = ((FILE1) NULL);
                                     L81=ZALLOC(sizeof(struct FILE1_struct));
                                     if (L81==NULL) FATAL("Unable to allocate more memory");
                                     ((OB)L81)->header.tag=FILE1_tag;
                                     L80 = ((FILE1) L81);
                                     r4 = L80;
                                     
                                     SATTR(r4,fp,stderr);
                                     ret_val121 = r4;
                                     FILE_plus_dSTR(ret_val121, plus_s3);
                                     ret_val120 = plus_self6;
                                     plus_self7 = ret_val120;
                                     plus_s4 = ((dSTR) ((STR) &missing1));
                                     stderr_self4 = ((FILE1) NULL);
                                     L84=ZALLOC(sizeof(struct FILE1_struct));
                                     if (L84==NULL) FATAL("Unable to allocate more memory");
                                     ((OB)L84)->header.tag=FILE1_tag;
                                     L83 = ((FILE1) L84);
                                     r5 = L83;
                                     
                                     SATTR(r5,fp,stderr);
                                     ret_val122 = r5;
                                     FILE_plus_dSTR(ret_val122, plus_s4);
                                     exit_self = ((UNIX) NULL);
                                     exit_code = 1;
                                     exit(exit_code);
                                    }
                                    else {
                                     SATTR(ATTR(self,prog),platforms,FLISTS1425610062(ATTR(ATTR(self,prog),platforms), cfs));
                                     L86 = ATTR(ATTR(self,prog),genuine_platform);
                                     L871_=INTPLUS(L86,1); 
                                     SATTR(ATTR(self,prog),genuine_platform,L871_);
                                     L88 = CODE_O992729481(self, platform_module);
                                     plus_self8 = ((STR) &Platform);
                                     plus_sarg3 = s;
                                     ret_val123 = STR_ap2004550586(plus_self8, plus_sarg3);
                                     CODE_O1709756185(self, L88, ret_val123);
                                    }
                                   }
                                   else {
                                    CODE_O1656361547(self, s);
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
     }
    }
   }
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

void CODE_O1040697618(CODE_OPTIONS self) {
 STR arg;
 FSETSTR create_self;
 FSETSTR ret_val;
 CODE_OPTIONS more_args_self;
 BOOL ret_val1 = BOOL_zero;
 CODE_OPTIONS rewind_self;
 INT L1;
 ARRAYSTR L2;
 INT L31_;
 INT L4;
 BOOL L51_;
 extern STR all1;
 INT L6;
 INT L71_;
 create_self = ((FSETSTR) NULL);
 ret_val = ((FSETSTR) NULL);
 SATTR(self,classes1,ret_val);
 while (1) {
  more_args_self = self;
  L1 = ATTR(more_args_self,next);
  L2 = ATTR(more_args_self,args);
  L31_=(L2)==NULL?0:ASIZE((ARRAYSTR)L2); 
  L4 = L31_;
  L51_=(L1)<(L4); 
  ret_val1 = L51_;
  if (ret_val1) {
  }
  else {
   goto after_loop;
  }
  arg = CODE_O862164136(self);
  SATTR(self,all1,STR_is111530248(arg, ((STR) &all1)));
  if (ATTR(self,all1)) {
   return;
  }
  else {
   if (CODE_O1581463621(self, arg)) {
    SATTR(self,classes1,FSETST1404644833(ATTR(self,classes1), arg));
   }
   else {
    rewind_self = self;
    L6 = ATTR(rewind_self,next);
    L71_=INTMINUS(L6,1); 
    SATTR(rewind_self,next,L71_);
    return;
   }
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void CODE_O1614295032(CODE_OPTIONS self, ARRAYSTR a) {
 ARRAYSTR L11;
 STR s;
 STR L21;
 ARRAYSTR create_self;
 INT create_n = INT_zero;
 ARRAYSTR ret_val;
 INT L31 = INT_zero;
 CODE_OPTIONS more_args_self;
 BOOL ret_val1 = BOOL_zero;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self;
 STR plus_s;
 OUT ret_val3;
 FILE1 stdout_self;
 FILE1 ret_val4;
 FILE1 r;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val5;
 FILE1 stdout_self1;
 FILE1 ret_val6;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val7;
 FILE1 r2;
 CODE_OPTIONS barf_self;
 STR barf_msg;
 ERR1 create_self2;
 ERR1 ret_val8;
 ERR1 plus_self3;
 dSTR plus_s3;
 ERR1 ret_val9;
 FILE1 stderr_self;
 FILE1 ret_val10;
 FILE1 r3;
 ERR1 plus_self4;
 dSTR plus_s4;
 FILE1 stderr_self1;
 FILE1 ret_val11;
 FILE1 r4;
 UNIX exit_self;
 INT exit_code = INT_zero;
 CODE_OPTIONS barf_self1;
 STR barf_msg1;
 ERR1 create_self3;
 ERR1 ret_val12;
 ERR1 plus_self5;
 dSTR plus_s5;
 ERR1 ret_val13;
 FILE1 stderr_self2;
 FILE1 ret_val14;
 FILE1 r5;
 ERR1 plus_self6;
 dSTR plus_s6;
 FILE1 stderr_self3;
 FILE1 ret_val15;
 FILE1 r6;
 UNIX exit_self1;
 INT exit_code1 = INT_zero;
 INT L41_;
 ARRAYSTR L5;
 INT L6;
 OB L7;
 INT L8;
 INT L91_br;
 extern STR commandline;
 INT L101_;
 INT L12;
 ARRAYSTR L13;
 INT L141_;
 INT L15;
 BOOL L161_;
 extern STR Processingoption;
 FILE1 L17;
 OB L18;
 FILE1 L20;
 OB L22;
 extern STR name9;
 FILE1 L24;
 OB L25;
 extern STR main2;
 extern STR home;
 extern STR homeis1034977208;
 FILE1 L27;
 OB L28;
 dSTR L30;
 OB L32;
 FILE1 L33;
 OB L34;
 extern STR verbose;
 extern STR prolix;
 extern STR prolix706342420;
 extern STR O_debug;
 extern STR O_debug_func;
 FLISTSTR L36;
 extern STR O_verbose;
 extern STR end2;
 extern STR Nohome1873319144;
 FILE1 L37;
 OB L38;
 dSTR L40;
 OB L42;
 FILE1 L43;
 OB L44;
 SATTR(self,args,a);
 create_self = ((ARRAYSTR) NULL);
 L41_=(a)==NULL?0:ASIZE((ARRAYSTR)a); 
 create_n = L41_;
 L6 = create_n;
 L8=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L6)*sizeof(STR);
 L7=ZALLOC_BIG(L8);
 if (L7==NULL) FATAL("Unable to allocate more memory");
 ((OB)L7)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L7)->header.destroyed=0;
#endif

 L5 = ((ARRAYSTR) L7);
 L5->asize = L6;
 ret_val = L5;
 SATTR(self,arg_locations,ret_val);
 {
  BOOL f_L91_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = ATTR(self,arg_locations);
  L91_br=L11==NULL?0:ASIZE((ARRAYSTR)L11); 
  while (1) {
   if(L31>=L91_br)  goto after_loop; 
   SARR((ARRAYSTR)L11,L31,(STR)((STR) &commandline)); 
   ;
   L101_=INTPLUS(L31,1); 
   L31 = L101_;
  }
 }
 after_loop: ;
 SATTR(self,next,1);
 CODE_O549188150(self);
 while (1) {
  more_args_self = self;
  L12 = ATTR(more_args_self,next);
  L13 = ATTR(more_args_self,args);
  L141_=(L13)==NULL?0:ASIZE((ARRAYSTR)L13); 
  L15 = L141_;
  L161_=(L12)<(L15); 
  ret_val1 = L161_;
  if (ret_val1) {
  }
  else {
   goto after_loop1;
  }
  s = CODE_O862164136(self);
  if (ATTR(ATTR(self,prog),prolix)) {
   create_self1 = ((OUT) NULL);
   ret_val2 = create_self1;
   plus_self = ret_val2;
   plus_s = ((STR) &Processingoption);
   stdout_self = ((FILE1) NULL);
   L18=ZALLOC(sizeof(struct FILE1_struct));
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FILE1_tag;
   L17 = ((FILE1) L18);
   r = L17;
   
   SATTR(r,fp,stdout);
   ret_val4 = r;
   FILE_plus_STR(ret_val4, plus_s);
   ret_val3 = plus_self;
   plus_self1 = ret_val3;
   plus_s1 = s;
   stdout_self1 = ((FILE1) NULL);
   L22=ZALLOC(sizeof(struct FILE1_struct));
   if (L22==NULL) FATAL("Unable to allocate more memory");
   ((OB)L22)->header.tag=FILE1_tag;
   L20 = ((FILE1) L22);
   r1 = L20;
   
   SATTR(r1,fp,stdout);
   ret_val6 = r1;
   FILE_plus_STR(ret_val6, plus_s1);
   ret_val5 = plus_self1;
   plus_self2 = ret_val5;
   plus_s2 = ((STR) &name9);
   stdout_self2 = ((FILE1) NULL);
   L25=ZALLOC(sizeof(struct FILE1_struct));
   if (L25==NULL) FATAL("Unable to allocate more memory");
   ((OB)L25)->header.tag=FILE1_tag;
   L24 = ((FILE1) L25);
   r2 = L24;
   
   SATTR(r2,fp,stdout);
   ret_val7 = r2;
   FILE_plus_STR(ret_val7, plus_s2);
  }
  L21 = s;
  if (STR_is111530248(L21, ((STR) &main2))) {
   SATTR(ATTR(self,prog),main_class,CODE_O862164136(self));
  }
  else {
   if (STR_is111530248(L21, ((STR) &home))) {
    barf_self = self;
    barf_msg = ((STR) &homeis1034977208);
    create_self2 = ((ERR1) NULL);
    ret_val8 = create_self2;
    plus_self3 = ret_val8;
    plus_s3 = ((dSTR) barf_msg);
    stderr_self = ((FILE1) NULL);
    L28=ZALLOC(sizeof(struct FILE1_struct));
    if (L28==NULL) FATAL("Unable to allocate more memory");
    ((OB)L28)->header.tag=FILE1_tag;
    L27 = ((FILE1) L28);
    r3 = L27;
    
    SATTR(r3,fp,stderr);
    ret_val10 = r3;
    FILE_plus_dSTR(ret_val10, plus_s3);
    ret_val9 = plus_self3;
    plus_self4 = ret_val9;
    L32=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
    if (L32==NULL) FATAL("Unable to allocate more memory");
    memset(L32,0,sizeof(struct CHAR_boxed_struct));
    ((OB)L32)->header.tag=CHAR_tag;
    L30 = (dSTR)((CHAR_boxed) L32);
    ((CHAR_boxed) L30)->immutable_part = '\n';
    plus_s4 = L30;
    stderr_self1 = ((FILE1) NULL);
    L34=ZALLOC(sizeof(struct FILE1_struct));
    if (L34==NULL) FATAL("Unable to allocate more memory");
    ((OB)L34)->header.tag=FILE1_tag;
    L33 = ((FILE1) L34);
    r4 = L33;
    
    SATTR(r4,fp,stderr);
    ret_val11 = r4;
    FILE_plus_dSTR(ret_val11, plus_s4);
    exit_self = ((UNIX) NULL);
    exit_code = 1;
    exit(exit_code);
   }
   else {
    if (STR_is111530248(L21, ((STR) &verbose))) {
     SATTR(ATTR(self,prog),verbose,TRUE);
    }
    else {
     if (STR_is111530248(L21, ((STR) &prolix))) {
      SATTR(ATTR(self,prog),verbose,TRUE);
      SATTR(ATTR(self,prog),prolix,TRUE);
     }
     else {
      if (STR_is111530248(L21, ((STR) &prolix706342420))) {
       SATTR(ATTR(self,prog),show_types,TRUE);
       SATTR(ATTR(self,prog),prolix,TRUE);
      }
      else {
       if (STR_is111530248(L21, ((STR) &O_debug))) {
        SATTR(ATTR(self,prog),opt_debug,TRUE);
        SATTR(ATTR(self,prog),opt_verbose,TRUE);
       }
       else {
        if (STR_is111530248(L21, ((STR) &O_debug_func))) {
         L36 = ATTR(ATTR(self,prog),opt_debug_func);
         SATTR(ATTR(self,prog),opt_debug_func,FLISTS1425610062(L36, CODE_O862164136(self)));
         SATTR(ATTR(self,prog),opt_verbose,TRUE);
        }
        else {
         if (STR_is111530248(L21, ((STR) &O_verbose))) {
          SATTR(ATTR(self,prog),opt_verbose,TRUE);
         }
         else {
          if (STR_is111530248(L21, ((STR) &end2))) {
          }
          else {
           CODE_O1006348781(self, s);
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
 after_loop1: ;
 if ((ATTR(ATTR(self,prog),home)==((STR) NULL))) {
  barf_self1 = self;
  barf_msg1 = ((STR) &Nohome1873319144);
  create_self3 = ((ERR1) NULL);
  ret_val12 = create_self3;
  plus_self5 = ret_val12;
  plus_s5 = ((dSTR) barf_msg1);
  stderr_self2 = ((FILE1) NULL);
  L38=ZALLOC(sizeof(struct FILE1_struct));
  if (L38==NULL) FATAL("Unable to allocate more memory");
  ((OB)L38)->header.tag=FILE1_tag;
  L37 = ((FILE1) L38);
  r5 = L37;
  
  SATTR(r5,fp,stderr);
  ret_val14 = r5;
  FILE_plus_dSTR(ret_val14, plus_s5);
  ret_val13 = plus_self5;
  plus_self6 = ret_val13;
  L42=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
  if (L42==NULL) FATAL("Unable to allocate more memory");
  memset(L42,0,sizeof(struct CHAR_boxed_struct));
  ((OB)L42)->header.tag=CHAR_tag;
  L40 = (dSTR)((CHAR_boxed) L42);
  ((CHAR_boxed) L40)->immutable_part = '\n';
  plus_s6 = L40;
  stderr_self3 = ((FILE1) NULL);
  L44=ZALLOC(sizeof(struct FILE1_struct));
  if (L44==NULL) FATAL("Unable to allocate more memory");
  ((OB)L44)->header.tag=FILE1_tag;
  L43 = ((FILE1) L44);
  r6 = L43;
  
  SATTR(r6,fp,stderr);
  ret_val15 = r6;
  FILE_plus_dSTR(ret_val15, plus_s6);
  exit_self1 = ((UNIX) NULL);
  exit_code1 = 1;
  exit(exit_code1);
 }
}


#undef IS_ITER
#define IS_ITER 0

void CODE_O1656361547(CODE_OPTIONS self, STR s) {
 CODE_OPTIONS rewind_self;
 CODE_OPTIONS rewind_self1;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 if (CODE_O1472228953(self, s)) {
  rewind_self = self;
  L1 = ATTR(rewind_self,next);
  L21_=INTMINUS(L1,1); 
  SATTR(rewind_self,next,L21_);
  CODE_O549188150(self);
 }
 else {
  rewind_self1 = self;
  L3 = ATTR(rewind_self1,next);
  L41_=INTMINUS(L3,1); 
  SATTR(rewind_self1,next,L41_);
  CODE_O1762472157(self);
  return;
 }
}


#undef IS_ITER
#define IS_ITER 0

void CODE_O1709756185(CODE_OPTIONS self, FLISTSTR cl, STR filename) {
 ARRAYSTR tail = ((ARRAYSTR) NULL);
 ARRAYSTR tail_names = ((ARRAYSTR) NULL);
 ARRAYSTR L11;
 CODE_OPTIONS more_args_self;
 BOOL ret_val = BOOL_zero;
 ARRAYSTR subarr_self;
 INT subarr_beg = INT_zero;
 INT subarr_num = INT_zero;
 ARRAYSTR ret_val1;
 ARRAYSTR r;
 ARRAYSTR create_self;
 INT create_n = INT_zero;
 ARRAYSTR ret_val2;
 ARRAYSTR create_self1;
 INT create_n1 = INT_zero;
 ARRAYSTR ret_val3;
 ARRAYSTR create_self2;
 INT create_n2 = INT_zero;
 ARRAYSTR ret_val4;
 ARRAYSTR create_self3;
 INT create_n3 = INT_zero;
 ARRAYSTR ret_val5;
 INT L21 = INT_zero;
 INT L3;
 ARRAYSTR L4;
 INT L51_;
 INT L6;
 BOOL L71_;
 ARRAYSTR L8;
 INT L91_;
 INT L10;
 INT L12;
 INT L131_;
 ARRAYSTR L14;
 INT L15;
 OB L16;
 INT L17;
 ARRAYSTR L19;
 INT L201_;
 INT L22;
 INT L23;
 INT L241_;
 ARRAYSTR L25;
 INT L26;
 OB L27;
 INT L28;
 ARRAYSTR L29;
 INT L30;
 OB L31;
 INT L32;
 ARRAYSTR L33;
 INT L34;
 OB L35;
 INT L36;
 ARRAYSTR L37;
 ARRAYSTR L38;
 INT L39;
 OB L40;
 INT L41;
 INT L421_br;
 INT L431_;
 more_args_self = self;
 L3 = ATTR(more_args_self,next);
 L4 = ATTR(more_args_self,args);
 L51_=(L4)==NULL?0:ASIZE((ARRAYSTR)L4); 
 L6 = L51_;
 L71_=(L3)<(L6); 
 ret_val = L71_;
 if (ret_val) {
  subarr_self = ATTR(self,args);
  subarr_beg = ATTR(self,next);
  L8 = ATTR(self,args);
  L91_=(L8)==NULL?0:ASIZE((ARRAYSTR)L8); 
  L10 = L91_;
  L12 = ATTR(self,next);
  L131_=INTMINUS(L10,L12); 
  subarr_num = L131_;
  L15 = subarr_num;
  L17=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L15)*sizeof(STR);
  L16=ZALLOC_BIG(L17);
  if (L16==NULL) FATAL("Unable to allocate more memory");
  ((OB)L16)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L16)->header.destroyed=0;
#endif

  L14 = ((ARRAYSTR) L16);
  L14->asize = L15;
  r = L14;
  AREFACOPYBNS(r,0,subarr_num,subarr_beg,subarr_self); 
  ;
  ret_val1 = r;
  tail = ret_val1;
  create_self = ((ARRAYSTR) NULL);
  L19 = ATTR(self,args);
  L201_=(L19)==NULL?0:ASIZE((ARRAYSTR)L19); 
  L22 = L201_;
  L23 = ATTR(self,next);
  L241_=INTMINUS(L22,L23); 
  create_n = L241_;
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
  ret_val2 = L25;
  tail_names = ret_val2;
 }
 else {
  create_self1 = ((ARRAYSTR) NULL);
  create_n1 = 0;
  L30 = create_n1;
  L32=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L30)*sizeof(STR);
  L31=ZALLOC_BIG(L32);
  if (L31==NULL) FATAL("Unable to allocate more memory");
  ((OB)L31)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L31)->header.destroyed=0;
#endif

  L29 = ((ARRAYSTR) L31);
  L29->asize = L30;
  ret_val3 = L29;
  tail = ret_val3;
  create_self2 = ((ARRAYSTR) NULL);
  create_n2 = 0;
  L34 = create_n2;
  L36=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L34)*sizeof(STR);
  L35=ZALLOC_BIG(L36);
  if (L35==NULL) FATAL("Unable to allocate more memory");
  ((OB)L35)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L35)->header.destroyed=0;
#endif

  L33 = ((ARRAYSTR) L35);
  L33->asize = L34;
  ret_val4 = L33;
  tail_names = ret_val4;
 }
 L37 = FLISTS533235694(cl);
 SATTR(self,args,ARRAYS417083844(L37, tail));
 create_self3 = ((ARRAYSTR) NULL);
 create_n3 = FLISTS326330076(cl);
 L39 = create_n3;
 L41=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L39)*sizeof(STR);
 L40=ZALLOC_BIG(L41);
 if (L40==NULL) FATAL("Unable to allocate more memory");
 ((OB)L40)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L40)->header.destroyed=0;
#endif

 L38 = ((ARRAYSTR) L40);
 L38->asize = L39;
 ret_val5 = L38;
 SATTR(self,arg_locations,ret_val5);
 {
  BOOL f_L421_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = ATTR(self,arg_locations);
  L421_br=L11==NULL?0:ASIZE((ARRAYSTR)L11); 
  while (1) {
   if(L21>=L421_br)  goto after_loop; 
   SARR((ARRAYSTR)L11,L21,(STR)filename); 
   ;
   L431_=INTPLUS(L21,1); 
   L21 = L431_;
  }
 }
 after_loop: ;
 SATTR(self,arg_locations,ARRAYS417083844(ATTR(self,arg_locations), tail_names));
 SATTR(self,next,0);
}


#undef IS_ITER
#define IS_ITER 0

void CODE_O1762472157(CODE_OPTIONS self) {
 ERR1 create_self;
 ERR1 ret_val;
 ERR1 plus_self;
 dSTR plus_s;
 ERR1 ret_val1;
 FILE1 stderr_self;
 FILE1 ret_val2;
 FILE1 r;
 ERR1 plus_self1;
 dSTR plus_s1;
 FILE1 stderr_self1;
 FILE1 ret_val3;
 FILE1 r1;
 ERR1 create_self1;
 ERR1 ret_val4;
 ERR1 plus_self2;
 dSTR plus_s2;
 ERR1 ret_val5;
 FILE1 stderr_self2;
 FILE1 ret_val6;
 FILE1 r2;
 ERR1 plus_self3;
 dSTR plus_s3;
 ERR1 ret_val7;
 FILE1 stderr_self3;
 FILE1 ret_val8;
 FILE1 r3;
 ERR1 plus_self4;
 dSTR plus_s4;
 FILE1 stderr_self4;
 FILE1 ret_val9;
 FILE1 r4;
 ERR1 create_self2;
 ERR1 ret_val10;
 ERR1 plus_self5;
 dSTR plus_s5;
 FILE1 stderr_self5;
 FILE1 ret_val11;
 FILE1 r5;
 UNIX exit_self;
 INT exit_code = INT_zero;
 extern STR Comman1382430749;
 FILE1 L1;
 OB L2;
 ARRAYSTR L4;
 INT L5;
 ARRAYSTR L6;
 INT L71_;
 INT L8;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 STR L131_;
 FILE1 L14;
 OB L15;
 extern STR in;
 FILE1 L17;
 OB L18;
 ARRAYSTR L20;
 INT L21;
 ARRAYSTR L22;
 INT L231_;
 INT L24;
 INT L251_;
 INT L26;
 INT L271_;
 INT L28;
 STR L291_;
 FILE1 L30;
 OB L32;
 dSTR L34;
 OB L35;
 FILE1 L36;
 OB L37;
 extern STR Seemanpage;
 FILE1 L39;
 OB L40;
 create_self = ((ERR1) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((dSTR) ((STR) &Comman1382430749));
 stderr_self = ((FILE1) NULL);
 L2=ZALLOC(sizeof(struct FILE1_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=FILE1_tag;
 L1 = ((FILE1) L2);
 r = L1;
 
 SATTR(r,fp,stderr);
 ret_val2 = r;
 FILE_plus_dSTR(ret_val2, plus_s);
 ret_val1 = plus_self;
 plus_self1 = ret_val1;
 L4 = ATTR(self,args);
 L5 = ATTR(self,next);
 L6 = ATTR(self,args);
 L71_=(L6)==NULL?0:ASIZE((ARRAYSTR)L6); 
 L8 = L71_;
 L91_=INTMINUS(L8,1); 
 L10 = L91_;
 L111_=(L5<L10)?L5:L10; 
 L12 = L111_;
 L131_=(STR)ARR((ARRAYSTR)L4,L12); 
 plus_s1 = ((dSTR) L131_);
 stderr_self1 = ((FILE1) NULL);
 L15=ZALLOC(sizeof(struct FILE1_struct));
 if (L15==NULL) FATAL("Unable to allocate more memory");
 ((OB)L15)->header.tag=FILE1_tag;
 L14 = ((FILE1) L15);
 r1 = L14;
 
 SATTR(r1,fp,stderr);
 ret_val3 = r1;
 FILE_plus_dSTR(ret_val3, plus_s1);
 create_self1 = ((ERR1) NULL);
 ret_val4 = create_self1;
 plus_self2 = ret_val4;
 plus_s2 = ((dSTR) ((STR) &in));
 stderr_self2 = ((FILE1) NULL);
 L18=ZALLOC(sizeof(struct FILE1_struct));
 if (L18==NULL) FATAL("Unable to allocate more memory");
 ((OB)L18)->header.tag=FILE1_tag;
 L17 = ((FILE1) L18);
 r2 = L17;
 
 SATTR(r2,fp,stderr);
 ret_val6 = r2;
 FILE_plus_dSTR(ret_val6, plus_s2);
 ret_val5 = plus_self2;
 plus_self3 = ret_val5;
 L20 = ATTR(self,arg_locations);
 L21 = ATTR(self,next);
 L22 = ATTR(self,args);
 L231_=(L22)==NULL?0:ASIZE((ARRAYSTR)L22); 
 L24 = L231_;
 L251_=INTMINUS(L24,1); 
 L26 = L251_;
 L271_=(L21<L26)?L21:L26; 
 L28 = L271_;
 L291_=(STR)ARR((ARRAYSTR)L20,L28); 
 plus_s3 = ((dSTR) L291_);
 stderr_self3 = ((FILE1) NULL);
 L32=ZALLOC(sizeof(struct FILE1_struct));
 if (L32==NULL) FATAL("Unable to allocate more memory");
 ((OB)L32)->header.tag=FILE1_tag;
 L30 = ((FILE1) L32);
 r3 = L30;
 
 SATTR(r3,fp,stderr);
 ret_val8 = r3;
 FILE_plus_dSTR(ret_val8, plus_s3);
 ret_val7 = plus_self3;
 plus_self4 = ret_val7;
 L35=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L35==NULL) FATAL("Unable to allocate more memory");
 memset(L35,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L35)->header.tag=CHAR_tag;
 L34 = (dSTR)((CHAR_boxed) L35);
 ((CHAR_boxed) L34)->immutable_part = '\n';
 plus_s4 = L34;
 stderr_self4 = ((FILE1) NULL);
 L37=ZALLOC(sizeof(struct FILE1_struct));
 if (L37==NULL) FATAL("Unable to allocate more memory");
 ((OB)L37)->header.tag=FILE1_tag;
 L36 = ((FILE1) L37);
 r4 = L36;
 
 SATTR(r4,fp,stderr);
 ret_val9 = r4;
 FILE_plus_dSTR(ret_val9, plus_s4);
 create_self2 = ((ERR1) NULL);
 ret_val10 = create_self2;
 plus_self5 = ret_val10;
 plus_s5 = ((dSTR) ((STR) &Seemanpage));
 stderr_self5 = ((FILE1) NULL);
 L40=ZALLOC(sizeof(struct FILE1_struct));
 if (L40==NULL) FATAL("Unable to allocate more memory");
 ((OB)L40)->header.tag=FILE1_tag;
 L39 = ((FILE1) L40);
 r5 = L39;
 
 SATTR(r5,fp,stderr);
 ret_val11 = r5;
 FILE_plus_dSTR(ret_val11, plus_s5);
 exit_self = ((UNIX) NULL);
 exit_code = 1;
 exit(exit_code);
}


#undef IS_ITER
#define IS_ITER 0

void CODE_O1862448983(CODE_OPTIONS self, STR arg, STR suf) {
 STR L11;
 ERR1 create_self;
 ERR1 ret_val;
 ERR1 plus_self;
 dSTR plus_s;
 ERR1 ret_val1;
 FILE1 stderr_self;
 FILE1 ret_val2;
 FILE1 r;
 ERR1 plus_self1;
 dSTR plus_s1;
 ERR1 ret_val3;
 FILE1 stderr_self1;
 FILE1 ret_val4;
 FILE1 r1;
 ERR1 plus_self2;
 dSTR plus_s2;
 FILE1 stderr_self2;
 FILE1 ret_val5;
 FILE1 r2;
 extern STR c1;
 extern STR o;
 extern STR a1;
 extern STR obj;
 extern STR lib1;
 extern STR Didntu1702411602;
 FILE1 L2;
 OB L3;
 FILE1 L5;
 OB L6;
 dSTR L8;
 OB L9;
 FILE1 L10;
 OB L12;
 L11 = suf;
 if (STR_is111530248(L11, ((STR) &c1))) {
  return;
 }
 else {
  if (STR_is111530248(L11, ((STR) &o))) {
   return;
  }
  else {
   if (STR_is111530248(L11, ((STR) &a1))) {
    return;
   }
   else {
    if (STR_is111530248(L11, ((STR) &obj))) {
     return;
    }
    else {
     if (STR_is111530248(L11, ((STR) &lib1))) {
      return;
     }
     else {
      create_self = ((ERR1) NULL);
      ret_val = create_self;
      plus_self = ret_val;
      plus_s = ((dSTR) ((STR) &Didntu1702411602));
      stderr_self = ((FILE1) NULL);
      L3=ZALLOC(sizeof(struct FILE1_struct));
      if (L3==NULL) FATAL("Unable to allocate more memory");
      ((OB)L3)->header.tag=FILE1_tag;
      L2 = ((FILE1) L3);
      r = L2;
      
      SATTR(r,fp,stderr);
      ret_val2 = r;
      FILE_plus_dSTR(ret_val2, plus_s);
      ret_val1 = plus_self;
      plus_self1 = ret_val1;
      plus_s1 = ((dSTR) arg);
      stderr_self1 = ((FILE1) NULL);
      L6=ZALLOC(sizeof(struct FILE1_struct));
      if (L6==NULL) FATAL("Unable to allocate more memory");
      ((OB)L6)->header.tag=FILE1_tag;
      L5 = ((FILE1) L6);
      r1 = L5;
      
      SATTR(r1,fp,stderr);
      ret_val4 = r1;
      FILE_plus_dSTR(ret_val4, plus_s1);
      ret_val3 = plus_self1;
      plus_self2 = ret_val3;
      L9=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
      if (L9==NULL) FATAL("Unable to allocate more memory");
      memset(L9,0,sizeof(struct CHAR_boxed_struct));
      ((OB)L9)->header.tag=CHAR_tag;
      L8 = (dSTR)((CHAR_boxed) L9);
      ((CHAR_boxed) L8)->immutable_part = '\n';
      plus_s2 = L8;
      stderr_self2 = ((FILE1) NULL);
      L12=ZALLOC(sizeof(struct FILE1_struct));
      if (L12==NULL) FATAL("Unable to allocate more memory");
      ((OB)L12)->header.tag=FILE1_tag;
      L10 = ((FILE1) L12);
      r2 = L10;
      
      SATTR(r2,fp,stderr);
      ret_val5 = r2;
      FILE_plus_dSTR(ret_val5, plus_s2);
      CODE_O1762472157(self);
     }
    }
   }
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

void CODE_O549188150(CODE_OPTIONS self) {
 STR arg;
 STR suf;
 STR path11;
 CODE_OPTIONS more_args_self;
 BOOL ret_val = BOOL_zero;
 CODE_OPTIONS rewind_self;
 BFILE resolve_path_sel;
 STR resolve_path_fn;
 STR ret_val1;
 INT L1;
 ARRAYSTR L2;
 INT L31_;
 INT L4;
 BOOL L51_;
 BOOL L6;
 BOOL L71_;
 INT L8;
 INT L91_;
 extern STR sa;
 BOOL L10;
 BOOL L111_;
 extern STR module;
 FLISTSTR L12;
 while (1) {
  more_args_self = self;
  L1 = ATTR(more_args_self,next);
  L2 = ATTR(more_args_self,args);
  L31_=(L2)==NULL?0:ASIZE((ARRAYSTR)L2); 
  L4 = L31_;
  L51_=(L1)<(L4); 
  ret_val = L51_;
  if (ret_val) {
  }
  else {
   goto after_loop;
  }
  arg = CODE_O862164136(self);
  L6 = CODE_O1472228953(self, arg);
  L71_=!(L6); 
  if (L71_) {
   rewind_self = self;
   L8 = ATTR(rewind_self,next);
   L91_=INTMINUS(L8,1); 
   SATTR(rewind_self,next,L91_);
   goto after_loop;
  }
  suf = CODE_O689829754(self, arg);
  if (STR_is111530248(suf, ((STR) &sa))) {
   resolve_path_sel = ((BFILE) NULL);
   resolve_path_fn = arg;
   ret_val1 = resolve_path_fn;
   path11 = ret_val1;
   L10 = FSETST1025458804(ATTR(self,paths), path11);
   L111_=!(L10); 
   if (L111_) {
    SATTR(ATTR(self,prog),sather_files,FSETST1404644833(ATTR(ATTR(self,prog),sather_files), arg));
    SATTR(self,paths,FSETST1404644833(ATTR(self,paths), path11));
   }
  }
  else {
   if (STR_is111530248(suf, ((STR) &module))) {
    L12 = CODE_O992729481(self, arg);
    CODE_O1709756185(self, L12, arg);
   }
   else {
    CODE_O1862448983(self, arg, suf);
    return;
   }
  }
 }
 after_loop: ;
}
