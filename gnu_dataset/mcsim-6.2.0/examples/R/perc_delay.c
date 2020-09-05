/* perc_delay.c for R deSolve package
   ___________________________________________________

   Model File:  perc_delay.model

   Date:  Tue Jan 24 10:20:19 2017

   Created by:  "mod v5.6.6"
    -- a model preprocessor by Don Maszle
   ___________________________________________________

   Copyright (c) 1993-2017 Free Software Foundation, Inc.

   Model calculations for compartmental model:

   6 States:
     Q_fat = 0.0,
     Q_wp = 0.0,
     Q_pp = 0.0,
     Q_liv = 0.0,
     Q_exh = 0.0,
     Q_met = 0.0,

   7 Outputs:
    "C_liv",
    "old_Q_liv",
    "C_alv",
    "C_exh",
    "C_ven",
    "Pct_metabolized",
    "C_exh_ug",

   2 Inputs:
     C_inh (forcing function)
     Q_ing (forcing function)

   37 Parameters:
     PPM_per_mg_per_l = 72.0 / 0.488,
     mg_per_l_per_PPM = 1/PPM_per_mg_per_l,
     tau = 1,
     InhMag = 0.0,
     Period = 0.0,
     Exposure = 0.0,
     IngDose = 0.0,
     LeanBodyWt = 55,
     Pct_M_fat = .16,
     Pct_LM_liv = .03,
     Pct_LM_wp = .17,
     Pct_LM_pp = .70,
     Pct_Flow_fat = .09,
     Pct_Flow_liv = .34,
     Pct_Flow_wp = .50,
     Pct_Flow_pp = .07,
     PC_fat = 144,
     PC_liv = 4.6,
     PC_wp = 8.7,
     PC_pp = 1.4,
     PC_art = 12.0,
     Flow_pul = 8.0,
     Vent_Perf = 1.14,
     sc_Vmax = .0026,
     Km = 1.0,
     BodyWt = LeanBodyWt / (1 - Pct_M_fat),
     V_fat = Pct_M_fat  * BodyWt/0.92,
     V_liv = Pct_LM_liv * LeanBodyWt,
     V_wp = Pct_LM_wp  * LeanBodyWt,
     V_pp = 0.9 * LeanBodyWt - V_liv - V_wp,
     Flow_fat = 0,
     Flow_liv = 0,
     Flow_wp = 0,
     Flow_pp = 0,
     Flow_tot = 0,
     Flow_alv = 0,
     Vmax = 0,
*/

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>

/* Model variables: States */
#define ID_Q_fat 0x00000
#define ID_Q_wp 0x00001
#define ID_Q_pp 0x00002
#define ID_Q_liv 0x00003
#define ID_Q_exh 0x00004
#define ID_Q_met 0x00005

/* Model variables: Outputs */
#define ID_C_liv 0x00000
#define ID_old_Q_liv 0x00001
#define ID_C_alv 0x00002
#define ID_C_exh 0x00003
#define ID_C_ven 0x00004
#define ID_Pct_metabolized 0x00005
#define ID_C_exh_ug 0x00006

/* Parameters */
static double parms[37];

#define PPM_per_mg_per_l parms[0]
#define mg_per_l_per_PPM parms[1]
#define tau parms[2]
#define InhMag parms[3]
#define Period parms[4]
#define Exposure parms[5]
#define IngDose parms[6]
#define LeanBodyWt parms[7]
#define Pct_M_fat parms[8]
#define Pct_LM_liv parms[9]
#define Pct_LM_wp parms[10]
#define Pct_LM_pp parms[11]
#define Pct_Flow_fat parms[12]
#define Pct_Flow_liv parms[13]
#define Pct_Flow_wp parms[14]
#define Pct_Flow_pp parms[15]
#define PC_fat parms[16]
#define PC_liv parms[17]
#define PC_wp parms[18]
#define PC_pp parms[19]
#define PC_art parms[20]
#define Flow_pul parms[21]
#define Vent_Perf parms[22]
#define sc_Vmax parms[23]
#define Km parms[24]
#define BodyWt parms[25]
#define V_fat parms[26]
#define V_liv parms[27]
#define V_wp parms[28]
#define V_pp parms[29]
#define Flow_fat parms[30]
#define Flow_liv parms[31]
#define Flow_wp parms[32]
#define Flow_pp parms[33]
#define Flow_tot parms[34]
#define Flow_alv parms[35]
#define Vmax parms[36]

/* Forcing (Input) functions */
static double forc[2];

#define C_inh forc[0]
#define Q_ing forc[1]

/* Function definitions for delay differential equations */

int Nout=1;
int nr[1]={0};
double ytau[1] = {0.0};

static double yini[6] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0}; /*Array of initial state variables*/

void lagvalue(double T, int *nr, int N, double *ytau) {
  static void(*fun)(double, int*, int, double*) = NULL;
  if (fun == NULL)
    fun = (void(*)(double, int*, int, double*))R_GetCCallable("deSolve", "lagvalue");
  return fun(T, nr, N, ytau);
}

double CalcDelay(int hvar, double dTime, double delay) {
  double T = dTime-delay;
  if (dTime > delay){
    nr[0] = hvar;
    lagvalue( T, nr, Nout, ytau );
}
  else{
    ytau[0] = yini[hvar];
}
  return(ytau[0]);
}

/*----- Initializers */
void initmod (void (* odeparms)(int *, double *))
{
  int N=37;
  odeparms(&N, parms);
}

void initforc (void (* odeforcs)(int *, double *))
{
  int N=2;
  odeforcs(&N, forc);
}


/* Calling R code will ensure that input y has same
   dimension as yini */
void initState (double *y)
{
  int i;

  for (i = 0; i < sizeof(yini) / sizeof(yini[0]); i++)
  {
    yini[i] = y[i];
  }
}

void getParms (double *inParms, double *out, int *nout) {
/*----- Model scaling */

  int i;

  for (i = 0; i < *nout; i++) {
    parms[i] = inParms[i];
  }


  Flow_alv = Flow_pul * 0.7 ;

  Flow_tot = Flow_alv / Vent_Perf ;

  Flow_fat = Pct_Flow_fat * Flow_tot ;
  Flow_liv = Pct_Flow_liv * Flow_tot ;
  Flow_pp = Pct_Flow_pp * Flow_tot ;
  Flow_wp = Flow_tot - Flow_fat - Flow_liv - Flow_pp ;

  Vmax = sc_Vmax * exp ( 0.7 * log ( LeanBodyWt ) ) ;

  for (i = 0; i < *nout; i++) {
    out[i] = parms[i];
  }
  }
/*----- Dynamics section */

void derivs (int *neq, double *pdTime, double *y, double *ydot, double *yout, int *ip)
{
  /* local */ double Cout_fat;
  /* local */ double Cout_wp;
  /* local */ double Cout_pp;
  /* local */ double Cout_liv;
  /* local */ double dQ_ven;
  /* local */ double C_art;
  /* local */ double dQmet_liv;

  Cout_fat = y[ID_Q_fat] / ( V_fat * PC_fat ) ;

  Cout_wp = y[ID_Q_wp] / ( V_wp * PC_wp ) ;

  Cout_pp = y[ID_Q_pp] / ( V_pp * PC_pp ) ;

  Cout_liv = y[ID_Q_liv] / ( V_liv * PC_liv ) ;

  dQ_ven = Flow_fat * Cout_fat + Flow_wp * Cout_wp + Flow_pp * Cout_pp + Flow_liv * Cout_liv ;

  yout[ID_C_ven] = dQ_ven / Flow_tot ;

  C_art = ( Flow_alv * C_inh / PPM_per_mg_per_l + dQ_ven ) / ( Flow_tot + Flow_alv / PC_art ) ;

  yout[ID_C_alv] = C_art / PC_art ;

  yout[ID_C_exh] = 0.7 * yout[ID_C_alv] + 0.3 * C_inh / PPM_per_mg_per_l ;

  yout[ID_old_Q_liv] = CalcDelay ( ID_Q_liv, (*pdTime) , tau ) ;

  dQmet_liv = Vmax * yout[ID_old_Q_liv] / ( Km + y[ID_Q_liv] ) ;

  ydot[ID_Q_exh] = Flow_alv * yout[ID_C_alv] ;

  ydot[ID_Q_fat] = Flow_fat * ( C_art - Cout_fat ) ;

  ydot[ID_Q_wp] = Flow_wp * ( C_art - Cout_wp ) ;

  ydot[ID_Q_pp] = Flow_pp * ( C_art - Cout_pp ) ;

  ydot[ID_Q_liv] = Q_ing + Flow_liv * ( C_art - Cout_liv ) - dQmet_liv ;

  ydot[ID_Q_met] = dQmet_liv ;

  yout[ID_C_liv] = y[ID_Q_liv] / V_liv ;

  yout[ID_Pct_metabolized] = ( InhMag ? 100 * y[ID_Q_met] / ( 1440 * Flow_alv * InhMag * mg_per_l_per_PPM ) : 0 ) ;

  yout[ID_C_exh_ug] = yout[ID_C_exh] * 1000 ;

} /* derivs */


/*----- Jacobian calculations: */
void jac (int *neq, double *t, double *y, int *ml, int *mu, double *pd, int *nrowpd, double *yout, int *ip)
{

} /* jac */


/*----- Events calculations: */
void event (int *n, double *t, double *y)
{

} /* event */

/*----- Roots calculations: */
void root (int *neq, double *t, double *y, int *ng, double *gout, double *out, int *ip)
{

} /* root */

