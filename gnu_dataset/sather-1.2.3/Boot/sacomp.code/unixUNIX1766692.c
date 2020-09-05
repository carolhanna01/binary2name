#include "sather.h"

/* Layouts */

typedef struct UNIX_struct {/* layout for UNIX */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *UNIX;

#include "tags.h"

/* Globals */


/* Function declarations */

INT STR_sizerINT(STR);
STR STR_cr1492743419(STR, EXT_OB);
STR UNIX_g108606936(UNIX, STR);
STR UNIX_s517080793(UNIX);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */




#undef IS_ITER
#define IS_ITER 0

STR UNIX_g108606936(UNIX self, STR var) {
 STR ret_val;
 EXT_OB r;
 STR L1;
 L1 = var;
 r = getenv(((L1==NULL)?NULL:L1->arr_part));
 if ((r==((EXT_OB) NULL))) {
  ret_val = ((STR) NULL);
  return ret_val;
 }
 else {
  ret_val = STR_cr1492743419(((STR) NULL), r);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR UNIX_s517080793(UNIX self) {
 STR ret_val;
 STR r;
 extern STR SATHER_HOME;
 extern STR usrlibsather;
 INT L1;
 INT L21_;
 INT L3;
 CHAR L41_;
 CHAR L5;
 BOOL L61_;
 extern STR Enviro2124171808;
 r = UNIX_g108606936(self, ((STR) &SATHER_HOME));
 if ((r==((STR) NULL))) {
  r = ((STR) &usrlibsather);
 }
 L1 = STR_sizerINT(r);
 L21_=INTMINUS(L1,1); 
 L3 = L21_;
 L41_=ARR((STR)r,L3); 
 L5 = L41_;
 L61_=L5=='/'; 
 if (L61_) {
  RAISE(((OB) ((STR) &Enviro2124171808)));
 }
 ret_val = r;
 return ret_val;
}
