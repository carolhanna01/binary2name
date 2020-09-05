#include "sather.h"

/* Layouts */

typedef struct A_STAC1244966098_struct {/* layout for A_STACK{AM_CURSOR_POS} */
 OB_HEADER header;
 struct FLISTA886630705_struct *s;
 } *A_STAC1244966098;

typedef struct INT_do676118316_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT arg1;/* Formal argument: i */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_I,I_u_Im; /* used by builtin iter */
 } *INT_do676118316_frame;

typedef struct SFILE_ID_struct {/* layout for SFILE_ID */
 INT loc;
 } SFILE_ID;
static SFILE_ID SFILE_ID_zero;

typedef struct SFILE_ID_boxed_struct {
 OB_HEADER header;
 SFILE_ID immutable_part;
 } *SFILE_ID_boxed;

typedef struct AM_CURSOR_POS_struct {/* layout for AM_CURSOR_POS */
 OB_HEADER header;
 struct dAM_struct *stmt;
 INT branch;
 BOOL mark;
 } *AM_CURSOR_POS;

typedef struct AM_OB_DEF_struct {/* layout for AM_OB_DEF */
 OB_HEADER header;
 struct dTP_struct *arr;
 struct dTP_struct *tp;
 struct ARRAYIDENT_struct *sorted_at;
 struct FMAPIDENTdTP_struct *at;
 INT asize;
 struct SFILE_ID_struct source1;
 } *AM_OB_DEF;

typedef struct FLISTA886630705_struct {/* layout for FLIST{AM_CURSOR_POS} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_CURSOR_POS_struct *arr_part[1];
 } *FLISTA886630705;

typedef struct A_STAC408111917_frame_struct {
 INT state;
 A_STAC1244966098 self;/* Formal argument: self */
 AM_CURSOR_POS ret_val1;
 INT L31;
 INT L61;
 FLISTA886630705 aget_self;
 INT aget_ind;
 AM_CURSOR_POS ret_val;
 BOOL f_L11_; /* used by builtin iter */
 INT L11_,L11_m; /* used by builtin iter */
 } *A_STAC408111917_frame;

#include "tags.h"

/* Globals */


/* Function declarations */

AM_CURSOR_POS A_STAC408111917(A_STAC408111917_frame);
INT FLISTA587993904(FLISTA886630705);
INT INT_do676118316(INT_do676118316_frame);
SFILE_ID AM_OB_1633498909(AM_OB_DEF);
STR A_STAC1193125327(A_STAC1244966098);
STR FLISTA1598488884(FLISTA886630705);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */




#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_OB_1633498909(AM_OB_DEF self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

STR A_STAC1193125327(A_STAC1244966098 self) {
 STR ret_val;
 ret_val = FLISTA1598488884(ATTR(self,s));
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 1

AM_CURSOR_POS A_STAC408111917(A_STAC408111917_frame frame) {
 AM_CURSOR_POS dummy = ((AM_CURSOR_POS) NULL);
 INT L2;
 INT L4;
 INT L51_;
 AM_CURSOR_POS L71_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->f_L11_ = TRUE;
  while (1) {
   frame->state = 1;
   frame->aget_self = ATTR(frame->self,s);
   if (frame->f_L11_) {
    frame->f_L11_ = FALSE;
    L4 = FLISTA587993904(ATTR(frame->self,s));
    L51_=INTMINUS(L4,1); 
    frame->L31 = L51_;
    frame->L61 = 0;
    frame->L11_=frame->L31+1;frame->L11_m=frame->L61; 
   }
   if(frame->L11_--<=frame->L11_m)  goto after_loop; 
   frame->aget_ind = frame->L11_;
   L71_=(AM_CURSOR_POS)ARR((FLISTA886630705)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L71_;
   return frame->ret_val;
   state1:;
  }
 }
 after_loop: ;
 frame->state = -1;
 return dummy;
}
