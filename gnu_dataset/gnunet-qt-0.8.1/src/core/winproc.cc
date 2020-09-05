/*
     This file is part of gnunet-qt.
     (C) 2009 Nils Durner (and other contributing authors)

     gnunet-qt is free software; you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published
     by the Free Software Foundation; either version 2, or (at your
     option) any later version.

     gnunet-qt is distributed in the hope that it will be useful, but
     WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
     General Public License for more details.

     You should have received a copy of the GNU General Public License
     along with GNUnet; see the file COPYING.  If not, write to the
     Free Software Foundation, Inc., 59 Temple Place - Suite 330,
     Boston, MA 02111-1307, USA.
*/

/**
 * @file src/core/winproc.cc
 * @brief Windows specific functions
 * @author Nils Durner
 */
#include "config.h"

#ifdef WINDOWS
  #include <windows.h>

  /* maps Windows LCIDs to locale strings */
  typedef struct
  {
    LCID lcid;
    char locale[6];
  } GLcid2Locale;

  static const GLcid2Locale Lcid2Locale[] =
  {
    { 0x0436,"af_ZA"},  { 0x041c,"sq_AL"},  { 0x045e,"am_ET"},  { 0x1401,"ar_DZ"},
    { 0x3c01,"ar_BH"},  { 0x0c01,"ar_EG"},  { 0x0801,"ar_IQ"},  { 0x2c01,"ar_JO"},
    { 0x3401,"ar_KW"},  { 0x3001,"ar_LB"},  { 0x1001,"ar_LY"},  { 0x1801,"ar_MA"},
    { 0x2001,"ar_OM"},  { 0x4001,"ar_QA"},  { 0x0401,"ar_SA"},  { 0x2801,"ar_SY"},
    { 0x1c01,"ar_TN"},  { 0x3801,"ar_AE"},  { 0x2401,"ar_YE"},  { 0x042b,"hy_AM"},
    { 0x044d,"as_IN"},  { 0x046d,"ba_RU"},  { 0x042d,"eu_ES"},  { 0x0423,"be_BY"},
    { 0x0445,"bn_IN"},  { 0x047e,"br_FR"},  { 0x0402,"bg_BG"},  { 0x0403,"ca_ES"},
    { 0x0c04,"zh_HK"},  { 0x1404,"zh_MO"},  { 0x0804,"zh_CN"},  { 0x1004,"zh_SG"},
    { 0x0404,"zh_TW"},  { 0x101a,"hr_BA"},  { 0x041a,"hr_HR"},  { 0x0405,"cs_CZ"},
    { 0x0406,"da_DK"},  { 0x0465,"dv_MV"},  { 0x0813,"nl_BE"},  { 0x0413,"nl_NL"},
    { 0x0c09,"en_AU"},  { 0x2809,"en_BZ"},  { 0x1009,"en_CA"},  { 0x4009,"en_IN"},
    { 0x1809,"en_IE"},  { 0x2009,"en_JM"},  { 0x4409,"en_MY"},  { 0x1409,"en_NZ"},
    { 0x3409,"en_PH"},  { 0x4809,"en_SG"},  { 0x1c09,"en_ZA"},  { 0x2c09,"en_TT"},
    { 0x0809,"en_GB"},  { 0x0409,"en_US"},  { 0x3009,"en_ZW"},  { 0x0425,"et_EE"},
    { 0x0438,"fo_FO"},  { 0x040b,"fi_FI"},  { 0x080c,"fr_BE"},  { 0x0c0c,"fr_CA"},
    { 0x040c,"fr_FR"},  { 0x140c,"fr_LU"},  { 0x180c,"fr_MC"},  { 0x100c,"fr_CH"},
    { 0x0462,"fy_NL"},  { 0x0456,"gl_ES"},  { 0x0437,"ka_GE"},  { 0x0c07,"de_AT"},
    { 0x0407,"de_DE"},  { 0x1407,"de_LI"},  { 0x1007,"de_LU"},  { 0x0807,"de_CH"},
    { 0x0408,"el_GR"},  { 0x046f,"kl_GL"},  { 0x0447,"gu_IN"},  { 0x040d,"he_IL"},
    { 0x0439,"hi_IN"},  { 0x040e,"hu_HU"},  { 0x040f,"is_IS"},  { 0x0470,"ig_NG"},
    { 0x0421,"id_ID"},  { 0x083c,"ga_IE"},  { 0x0410,"it_IT"},  { 0x0810,"it_CH"},
    { 0x0411,"ja_JP"},  { 0x044b,"kn_IN"},  { 0x043f,"kk_KZ"},  { 0x0453,"kh_KH"},
    { 0x0487,"rw_RW"},  { 0x0412,"ko_KR"},  { 0x0440,"ky_KG"},  { 0x0454,"lo_LA"},
    { 0x0426,"lv_LV"},  { 0x0427,"lt_LT"},  { 0x046e,"lb_LU"},  { 0x042f,"mk_MK"},
    { 0x083e,"ms_BN"},  { 0x043e,"ms_MY"},  { 0x044c,"ml_IN"},  { 0x043a,"mt_MT"},
    { 0x0481,"mi_NZ"},  { 0x044e,"mr_IN"},  { 0x0461,"ne_NP"},  { 0x0414,"nb_NO"},
    { 0x0814,"nn_NO"},  { 0x0482,"oc_FR"},  { 0x0448,"or_IN"},  { 0x0463,"ps_AF"},
    { 0x0429,"fa_IR"},  { 0x0415,"pl_PL"},  { 0x0416,"pt_BR"},  { 0x0816,"pt_PT"},
    { 0x0446,"pa_IN"},  { 0x0418,"ro_RO"},  { 0x0417,"rm_CH"},  { 0x0419,"ru_RU"},
    { 0x0c3b,"se_FI"},  { 0x043b,"se_NO"},  { 0x083b,"se_SE"},  { 0x044f,"sa_IN"},
    { 0x046c,"ns_ZA"},  { 0x0432,"tn_ZA"},  { 0x045b,"si_LK"},  { 0x041b,"sk_SK"},
    { 0x0424,"sl_SI"},  { 0x2c0a,"es_AR"},  { 0x400a,"es_BO"},  { 0x340a,"es_CL"},
    { 0x240a,"es_CO"},  { 0x140a,"es_CR"},  { 0x1c0a,"es_DO"},  { 0x300a,"es_EC"},
    { 0x440a,"es_SV"},  { 0x100a,"es_GT"},  { 0x480a,"es_HN"},  { 0x080a,"es_MX"},
    { 0x4c0a,"es_NI"},  { 0x180a,"es_PA"},  { 0x3c0a,"es_PY"},  { 0x280a,"es_PE"},
    { 0x500a,"es_PR"},  { 0x0c0a,"es_ES"},  { 0x540a,"es_US"},  { 0x380a,"es_UY"},
    { 0x200a,"es_VE"},  { 0x0441,"sw_KE"},  { 0x081d,"sv_FI"},  { 0x041d,"sv_SE"},
    { 0x0449,"ta_IN"},  { 0x0444,"tt_RU"},  { 0x044a,"te_IN"},  { 0x041e,"th_TH"},
    { 0x0851,"bo_BT"},  { 0x0451,"bo_CN"},  { 0x041f,"tr_TR"},  { 0x0442,"tk_TM"},
    { 0x0480,"ug_CN"},  { 0x0422,"uk_UA"},  { 0x0820,"tr_IN"},  { 0x0420,"ur_PK"},
    { 0x042a,"vi_VN"},  { 0x0452,"cy_GB"},  { 0x0488,"wo_SN"},  { 0x0434,"xh_ZA"},
    { 0x0478,"ii_CN"},  { 0x046a,"yo_NG"},  { 0x0435,"zu_ZA"},  { 0x0000,"C"}
  };

  const char *win_locale()
  {
    GLcid2Locale *entry = (GLcid2Locale *) Lcid2Locale;
    LCID lcid = GetThreadLocale() & 0x7FFF;

    while(entry->lcid)
    {
      if (entry->lcid == lcid)
        return entry->locale;

      entry++;
    }

    return entry->locale;
  }

extern int gn_main(int argc, char * const *argv);

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
  int argc, ret;
  char **argv, *idx, *line, *start, mod[4097];
  int quoted, esc;

  argc = 0;
  quoted = esc = 0;
  line = strdup(lpCmdLine);
  argv = (char **) malloc(sizeof(char *) * 2);

  GetModuleFileNameA(NULL, mod, 4096);
  argv[0] = mod;
  argv[1] = NULL;
  start = line;

  for (idx = line; *idx != 0; idx++)
  {
    if (*idx == '"' && (idx == line || *(idx-1) != '\\'))
      quoted = !quoted;

    if (!quoted && (*idx == '\t' || *idx == ' '))
    {
      argc++;
      argv = (char **) realloc(argv, (argc + 2) * sizeof(char *));
      argv[argc] = start;
      argv[argc + 1] = NULL;
      *idx = 0;
      start = idx + 1;
    }
  }

  ret = gn_main(argc + 1, (char * const *) argv);
  free(argv);
  free(line);

  return ret;
}

int win_isWMClose(const MSG *msg)
{
  return msg->message == WM_CLOSE;
}

void win_freeConsole()
{
  FreeConsole();
}

#endif
