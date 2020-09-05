/*
   GNU Maverik - a system for managing display and interaction in 
              Virtual Environment applications.
   Copyright (C) 2008  Advanced Interfaces Group

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.

   The authors can be contacted via:
   www   - http://aig.cs.man.ac.uk
   email - maverik@aig.cs.man.ac.uk
   mail  - Advanced Interfaces Group, Room 2.94, Kilburn Building, 
        University of Manchester, Manchester, M13 9PL, UK
*/


#include "mavlib_kernel.h"
#include <string.h>
#if !defined(WIN32) || defined(__CYGWIN__)
char *strdup (const char *s1);
#endif
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#if defined(__CYGWIN__) || (!defined(WIN32) && !defined(macintosh))
#include <unistd.h>
#ifdef MAV_LINUX
#ifndef MAV_FREEBSD
int gethostname(char *name, size_t len);
#endif
#endif
#include <dlfcn.h>
#endif

MAV_vector MAV_NULL_VECTOR={0,0,0};
MAV_vector MAV_X_VECTOR={1,0,0};
MAV_vector MAV_Y_VECTOR={0,1,0};
MAV_vector MAV_Z_VECTOR={0,0,1};
MAV_matrix MAV_ID_MATRIX={{{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}}};
MAV_quaternion MAV_ID_QUATERNION={1,0,0,0};

MAV_window *mav_win_all;
MAV_palette *mav_palette_default;
MAV_class *mav_class_all;
MAV_callback *mav_callback_delete;
MAV_SMSCallback *mav_SMSCallback_delete;
MAV_SMSCallback *mav_SMSCallback_objectRmv;
MAV_list *mav_module_list;
MAV_list *mav_sms_list;
MAV_list *mav_palette_list;
MAV_list *mav_object_list;
MAV_viewParams mav_vp_default;
int mavlib_usedWin[MAV_MAX_WIN];
float mav_fps;
float mav_fps_avg;
int mav_mallocCount=0;
char mav_hostName[200];
int mav_opt_output= MAV_UNDEFINED;
int mav_opt_objectTables= MAV_TRUE;
int mav_opt_WMPlacement= MAV_UNDEFINED;
int mav_opt_singleBuf= MAV_UNDEFINED;
int mav_opt_quadBuf= MAV_FALSE;
int mav_opt_accumBuf= MAV_FALSE;
int mav_opt_stencilBuf= MAV_FALSE;
int mav_opt_destAlpha= MAV_FALSE;
int mav_opt_multiSample= MAV_DONTCARE;
int mav_this_version= MAV_THIS_VERSION;
void *mavlib_dlh= NULL;
int mav_opt_bindTextures= MAV_UNDEFINED;
int mav_opt_shareContexts= MAV_UNDEFINED;
int mav_opt_syncSwap= MAV_UNDEFINED;
int mav_opt_defaultInit= MAV_TRUE;

/* For passing options to modules. Need global vars to cope cleanly with */
/* modules initialised from the application. */
int mav_argc= 0;
char **mav_argv= NULL;
FILE *mav_userconf= NULL;


/* Routines to identify and initalise the kernel */

char *mav_kernelID(void)
{
  return "GNU Maverik v6.2";
}

#if defined(WIN32) || defined(macintosh)
void mavlib_execInit(MAV_moduleInitFn fn)
{
  if (fn) fn();
}
#else
void mavlib_execInit(char *buf)
{
  MAV_moduleInitFn fn;

  fn= (MAV_moduleInitFn) dlsym(mavlib_dlh, buf);
  if (fn) 
  {
    fn();
  }
  else
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: Can not find function %s, exiting\n", buf);
    exit(1);
  }
}
#endif

#if defined(WIN32) || defined(macintosh)
#ifdef __cplusplus
extern "C" {
#endif
int mav_gfxModuleInit(void);
int mav_callbacksModuleInit(void);
int mav_SMSModuleInit(void);
int mav_windowsModuleInit(void);
int mav_navigationModuleInit(void);
int mav_objectsModuleInit(void);
#ifdef __cplusplus
}
#endif
#endif


void mavlib_kernelCmdLineParse(int argc, char *argv[])
{
  int cur_arg;
  int cur_char;
  char *arg_name;

  /* start at 1 to skip prog name */
  for (cur_arg=1; cur_arg < argc; cur_arg++)
  {

    /* skip args already parsed by another module */
    if (argv[cur_arg])
    {

      arg_name= mav_malloc(sizeof(char)*strlen(argv[cur_arg])+1);
      strcpy(arg_name, argv[cur_arg]);

      /* lowercase arg_name */
      for (cur_char=0; arg_name[cur_char]; cur_char++)
      {  
	arg_name[cur_char]= tolower(arg_name[cur_char]);
      }

      if (strcmp(arg_name, "-verbose") == 0)
      {
	mav_opt_output= MAV_VERBOSE;
	argv[cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-silent") == 0)
      {
	mav_opt_output= MAV_SILENT;
	argv[cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-fixedrnd") == 0)
      {
	mav_opt_fixedRnd= MAV_TRUE;
	argv[cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-nofixedrnd") == 0)
      {
	mav_opt_fixedRnd= MAV_FALSE;
	argv[cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-wmplacement") == 0)
      {
	mav_opt_WMPlacement= MAV_TRUE;
	argv[cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-nowmplacement") == 0)
      {
	mav_opt_WMPlacement= MAV_FALSE;
	argv[cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-singlebuf") == 0)
      {
	mav_opt_singleBuf= MAV_TRUE;
	argv[cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-nosinglebuf") == 0)
      {
	mav_opt_singleBuf= MAV_FALSE;
	argv[cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-multisample") == 0)
      {
	mav_opt_multiSample= MAV_TRUE;
	argv[cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-nomultisample") == 0)
      {
	mav_opt_multiSample= MAV_FALSE;
	argv[cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-sharecontexts") == 0)
      {
	mav_opt_shareContexts= MAV_TRUE;
	argv[cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-nosharecontexts") == 0)
      {
	mav_opt_shareContexts= MAV_FALSE;
	argv[cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-bindtextures") == 0)
      {
	mav_opt_bindTextures= MAV_TRUE;
	argv[cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-nobindtextures") == 0)
      {
	mav_opt_bindTextures= MAV_FALSE;
	argv[cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-flush") == 0)
      {
	mav_opt_flush= MAV_TRUE;
	argv[cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-noflush") == 0)
      {
	mav_opt_flush= MAV_FALSE;
	argv[cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-finish") == 0)
      {
	mav_opt_finish= MAV_TRUE;
	argv[cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-nofinish") == 0)
      {
	mav_opt_finish= MAV_FALSE;
	argv[cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-syncswap") == 0)
      {
	mav_opt_syncSwap= MAV_TRUE;
	argv[cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-nosyncswap") == 0)
      {
	mav_opt_syncSwap= MAV_FALSE;
	argv[cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-mavhelp") == 0)
      {
	printf("Maverik command line options:\n"
	       "  -verbose\t\t\t\tVerbose output on cmd line\n"
	       "  -silent\t\t\t\tNo textual output\n"
	       "  -[no]fixedRnd\t\t\t\tUse the built-in set of 'random' numbers\n"
	       "  -[no]WMPlacement\t\t\tAllow window manager to place window\n"
	       "  -[no]singleBuf\t\t\tSingle buffered rendering\n"
	       "  -[no]multiSample\t\t\tMultisampled rendering\n"
	       "  -[no]shareContexts\t\t\tMultiple windows share contexts\n"
	       "  -[no]bindTextures\t\t\tUse bound textures\n"
	       "  -[no]flush\t\t\t\tPerfrom a glFlush before swapping buffers\n"
	       "  -[no]finish\t\t\t\tPerfrom a glFinish before swapping buffers\n"
	       "  -[no]syncSwap\t\t\t\tSynchronize swapping buffers across multiple windows\n");

	/* suppress output that would otherwise disrupt the options listing */
	mav_opt_output= MAV_SILENT;
      }

      mav_free(arg_name);
    }
  }
}


void mavlib_kernelEnvVarsParse(void)
{
  char *ev;
  
  if ((ev= getenv("MAV_VERBOSE")))
  {
    mav_opt_output= (strcmp(ev, "0") == 0) ? MAV_SILENT : MAV_VERBOSE;
  }

  if ((ev= getenv("MAV_FIXEDRND")))
  {
    mav_opt_fixedRnd= (strcmp(ev, "0") == 0) ? MAV_FALSE : MAV_TRUE;
  }

  if ((ev= getenv("MAV_WMPLACEMENT")))
  {
    mav_opt_WMPlacement= (strcmp(ev, "0") == 0) ? MAV_FALSE : MAV_TRUE;
  }

  if ((ev= getenv("MAV_SINGLEBUF")))
  {
    mav_opt_singleBuf= (strcmp(ev, "0") == 0) ? MAV_FALSE : MAV_TRUE;
  }

  if ((ev= getenv("MAV_MULTISAMPLE")))
  {
    mav_opt_multiSample= (strcmp(ev, "0") == 0) ? MAV_FALSE : MAV_TRUE;
  }

  if ((ev= getenv("MAV_SHARECONTEXTS")))
  {
    mav_opt_shareContexts= (strcmp(ev, "0") == 0) ? MAV_FALSE : MAV_TRUE;
  }

  if ((ev= getenv("MAV_BINDTEXTURES")))
  {
    mav_opt_bindTextures= (strcmp(ev, "0") == 0) ? MAV_FALSE : MAV_TRUE;
  }

  if ((ev= getenv("MAV_FLUSH")))
  {
    mav_opt_flush= (strcmp(ev, "0") == 0) ? MAV_FALSE : MAV_TRUE;
  }

  if ((ev= getenv("MAV_FINISH")))
  {
    mav_opt_finish= (strcmp(ev, "0") == 0) ? MAV_FALSE : MAV_TRUE;
  }

  if ((ev= getenv("MAV_SYNCSWAP")))
  {
    mav_opt_syncSwap= (strcmp(ev, "0") == 0) ? MAV_FALSE : MAV_TRUE;
  }
}


void mavlib_kernelConfigFileParse(FILE *cf_file)
{
  char line[200], opt[100], val[100];
  int cur_char;

  fseek(cf_file, 0, SEEK_SET);

  while (fgets(line, 200, cf_file))
  {
    if (sscanf(line, "%s %s", opt, val) != 2)
    {
      if (line[strlen(line)-1] == '\n') line[strlen(line)-1]= '\0';
      fprintf(stderr, "Warning: \'%s\' is not an option/value pair in config file, ignoring\n", line);
    }
    else
    {
      /* lowercase opt name */
      for (cur_char=0; opt[cur_char]; cur_char++)
      {  
	opt[cur_char]= tolower(opt[cur_char]);
      }

      /* config file options mustn't override application settings */

      if (strcmp(opt, "verbose") == 0 && mav_opt_output == MAV_UNDEFINED)
      {
	mav_opt_output= (strcmp(val, "0") == 0) ? MAV_SILENT : MAV_VERBOSE;
      }

      else if (strcmp(opt, "fixedrnd") == 0 && mav_opt_fixedRnd == MAV_UNDEFINED)
      {
	mav_opt_fixedRnd= (strcmp(val, "0") == 0) ? MAV_FALSE : MAV_TRUE;
      }

      else if (strcmp(opt, "wmplacement") == 0 && mav_opt_WMPlacement == MAV_UNDEFINED)
      {
	mav_opt_WMPlacement= (strcmp(val, "0") == 0) ? MAV_FALSE : MAV_TRUE;
      }

      else if (strcmp(opt, "singlebuf") == 0 && mav_opt_singleBuf == MAV_UNDEFINED)
      {
	mav_opt_singleBuf= (strcmp(val, "0") == 0) ? MAV_FALSE : MAV_TRUE;
      }

      else if (strcmp(opt, "multisample") == 0 && mav_opt_multiSample == MAV_DONTCARE)
      {
	mav_opt_multiSample= (strcmp(val, "0") == 0) ? MAV_FALSE : MAV_TRUE;
      }

      else if (strcmp(opt, "sharecontexts") == 0 && mav_opt_shareContexts == MAV_UNDEFINED)
      {
	mav_opt_shareContexts= (strcmp(val, "0") == 0) ? MAV_FALSE : MAV_TRUE;
      }

      else if (strcmp(opt, "bindtextures") == 0 && mav_opt_bindTextures == MAV_UNDEFINED)
      {
	mav_opt_bindTextures= (strcmp(val, "0") == 0) ? MAV_FALSE : MAV_TRUE;
      }

      else if (strcmp(opt, "flush") == 0 && mav_opt_flush == MAV_UNDEFINED)
      {
	mav_opt_flush= (strcmp(val, "0") == 0) ? MAV_FALSE : MAV_TRUE;
      }

      else if (strcmp(opt, "finish") == 0 && mav_opt_finish == MAV_UNDEFINED)
      {
	mav_opt_finish= (strcmp(val, "0") == 0) ? MAV_FALSE : MAV_TRUE;
      }

      else if (strcmp(opt, "syncswap") == 0 && mav_opt_syncSwap == MAV_UNDEFINED)
      {
	mav_opt_syncSwap= (strcmp(val, "0") == 0) ? MAV_FALSE : MAV_TRUE;
      }
    }
  }
}


void mavlib_kernelOptionsDefaultSet(void)
{
  /* set any unset options */

  if (mav_opt_output == MAV_UNDEFINED) mav_opt_output= MAV_VERBOSE;
  if (mav_opt_fixedRnd == MAV_UNDEFINED) mav_opt_fixedRnd= MAV_FALSE;
  if (mav_opt_singleBuf == MAV_UNDEFINED) mav_opt_singleBuf= MAV_FALSE;
  if (mav_opt_WMPlacement == MAV_UNDEFINED) mav_opt_WMPlacement= MAV_FALSE;
  if (mav_opt_shareContexts == MAV_UNDEFINED) mav_opt_shareContexts= MAV_TRUE;
  if (mav_opt_bindTextures == MAV_UNDEFINED) mav_opt_bindTextures= MAV_TRUE;
  if (mav_opt_flush == MAV_UNDEFINED) mav_opt_flush= MAV_FALSE;
  if (mav_opt_finish == MAV_UNDEFINED) mav_opt_finish= MAV_FALSE;
  if (mav_opt_syncSwap == MAV_UNDEFINED) mav_opt_syncSwap= MAV_FALSE;
  /* leave multiSample as DONTCARE, if set as such */
}


void mav_initialiseNoArgs(void)
{
  mav_initialise(NULL, NULL);
}


void mav_initialise(int *argc_p, char *argv[])
{
  int i;
  char homepath[200];

#if defined(WIN32) || defined(macintosh)
  int defaultInit=1;
  char confname[]= "maverik.ini";
  MAV_moduleInitFn defaultMod[]={ *mav_gfxModuleInit,
                                  *mav_callbacksModuleInit,
                                  *mav_SMSModuleInit,
                                  *mav_windowsModuleInit,
                                  *mav_navigationModuleInit,
                                  *mav_objectsModuleInit, 0};
#else
  FILE *f;
  int defaultInit=0;
  char buf[100];
  char confname[]= "/home/rosch/.maverikrc";
  char *defaultMod[]={"mav_gfxModuleInit",
                      "mav_callbacksModuleInit",
                      "mav_SMSModuleInit",
                      "mav_windowsModuleInit",
                      "mav_navigationModuleInit",
                      "mav_objectsModuleInit", 0};
#endif

  /* Precedence for setting options is as follows (lowest to highest) */
  /* - mav defaults */
  /* - system config file */
  /* - user config file */
  /* - application specified - these are set before this function is called, so*/
  /*   config file options mustn't overwrite app specified ones.               */
  /* - env vars - higher than app specified so you can set a variable before   */
  /*   running your program and it will remain in effect for each run. Don't   */
  /*   set these in .profile/.bashrc etc. - use .maverikrc instead.            */
  /* - cmd line arguments */

  /* Procedure for handling cmd line args: each module sets to NULL anything     */
  /* argv that it parses. Once all modules initialised, parsed args are stripped */
  /* out so that application only sees it's args. */

  if (argc_p) mav_argc= *argc_p;
  mav_argv= argv;

  sprintf(homepath, "%s", confname);
  mav_userconf= fopen(homepath, "r");

  if (!mav_userconf && getenv("HOME"))
  {
    sprintf(homepath, "%s/%s", getenv("HOME"), confname);
    mav_userconf= fopen(homepath, "r");
  }

  if (mav_userconf) mavlib_kernelConfigFileParse(mav_userconf);
  mavlib_kernelEnvVarsParse();
  if (mav_argc) mavlib_kernelCmdLineParse(mav_argc, mav_argv);
  mavlib_kernelOptionsDefaultSet();
  
  /* identify the kernel version */
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "%s - Copyright (C) 1999-2002 Advanced Interfaces Group\n", mav_kernelID());

  /* create kernel defined callbacks to delete an object and SMS and remove an object from an SMS */
  mav_callback_delete= mav_callbackNew();
  mav_SMSCallback_delete= mav_SMSCallbackNew();
  mav_SMSCallback_objectRmv= mav_SMSCallbackNew();

  /* create the global window handle */
  mav_win_all= (MAV_window *) mav_malloc(sizeof(MAV_window));
  mav_win_all->id= 0;

  /* create the global class handle */
  mav_class_all= mav_classNew();

  /* create a list of all windows, SMS's, palettes and objects  */
  mav_win_list= mav_listNew();
  mav_sms_list= mav_listNew();
  mav_palette_list= mav_listNew();
  mav_object_list= mav_listNew();

  /* create a list frame functions */
  mavlib_frame0_list= mav_listNew();
  mavlib_frame1_list= mav_listNew();
  mavlib_frame2_list= mav_listNew();
  mavlib_frame3_list= mav_listNew();
  mavlib_frame4_list= mav_listNew();

  /* create a list of device functions */
  mavlib_devicePoll_list= mav_listNew();
  mavlib_deviceCalc_list= mav_listNew();
  mavlib_deviceEvent_list= mav_listNew();

  /* create a list of modules */
  mav_module_list= mav_listNew();

  /* default view params */
  mav_vp_default.eye.x=0;
  mav_vp_default.eye.y=0;
  mav_vp_default.eye.z=10;
  mav_vp_default.view.x=0;
  mav_vp_default.view.y=0;
  mav_vp_default.view.z=-1;
  mav_vp_default.up.x=0;
  mav_vp_default.up.y=1;
  mav_vp_default.up.z=0;
  mav_vp_default.fixed_up= mav_vp_default.up;
  mav_vp_default.mod= NULL;

  /* window used tables */
  for (i=1; i<MAV_MAX_WIN; i++) mavlib_usedWin[i]=0;

  /* initialise object lookup tables */
  if (mav_opt_objectTables) 
  {
    mavlib_setUpObjectTables();
  }
  else
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: Not using object look-up tables.\n");
  }

  /* get hostname */
#ifdef macintosh
  strcpy(mav_hostName, "Macintosh");
#elif defined(WIN32) && !defined(__CYGWIN__)
  strcpy(mav_hostName, "Windows"); /* WIN32 */
#else
  gethostname(mav_hostName, 200);
#endif

  /* multisample hint  */
  if (mav_opt_multiSample==MAV_DONTCARE) {
    if (!strcmp(mav_hostName, "bigmachine"))
    {
      mav_opt_multiSample= MAV_TRUE;
    }
    else
    {
      mav_opt_multiSample= MAV_FALSE;
    }
  }

  /* find the correct .MavModules file or use default */
#if !defined(WIN32) && !defined(macintosh)
  f= fopen(".MavModules", "r");
  if (!f) {
    if (getenv("MAV_HOME"))
    {
      sprintf(homepath, "%s/.MavModules", getenv("MAV_HOME"));
      f= fopen(homepath, "r");
      if (!f) defaultInit=1;
    }
    else
    {
      defaultInit=1;
    }
  }

  /* initalise each module defined */
#ifdef MAV_SUNOS4
  mavlib_dlh= dlopen(0, 1);
#else
  mavlib_dlh= dlopen(0, RTLD_NOW);
#endif  
  
  if (!mavlib_dlh) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: failed to dlopen the executable, exiting\n");
    exit(1);
  }
#endif  

  if (defaultInit) 
  {
    if (mav_opt_defaultInit) {
      i=0;
      while (defaultMod[i]) {
	mavlib_execInit(defaultMod[i]);
	i++;
      }
    }
  }
  else
  {
#if !defined(WIN32) && !defined(macintosh)
    do {
      if (fscanf(f, "%s", buf)>0) mavlib_execInit(buf);
    } while (!feof(f));
    
    fclose(f);
#endif
  }

  if (argc_p)
  {
    int cur_arg, cur_char;
    char *arg_name;
    int to, from;

    /* if -mavhelp cmd line option used, exit */

    for (cur_arg=1; cur_arg < mav_argc; cur_arg++)
    {
      if (argv[cur_arg])
      {
	arg_name= strdup(argv[cur_arg]);
	/* lowercase arg_name */
	for (cur_char=0; arg_name[cur_char]; cur_char++)
	  arg_name[cur_char]= tolower(arg_name[cur_char]);

	if (strcmp(arg_name, "-mavhelp") == 0) exit(EXIT_SUCCESS);
      }
    }

    /* tidy up argc and argv */
    for (to=1, from=1; from<(*argc_p); from++)
    {
      if (argv[from])
      {
	argv[to]= argv[from];
	to++;
      }
    }

    *argc_p= to;
  }

  if (mav_userconf) fclose(mav_userconf);
}



/* Wrappers to system malloc and free */

void *mav_malloc(int msize)
{
  void *result=(void *) malloc(msize);
  
  if (!result) {
    fprintf(stderr, "Error: malloc call failed. Requested size %i\n", msize);
    exit(1);
  } 

  mav_mallocCount++;

  return result;
}

void *mav_calloc(int nelem, int msize)
{
  void *result=(void *) calloc(nelem, msize);
  
  if (!result) {
    fprintf(stderr, "Error: calloc call failed. Requested elements %i, size %i\n", nelem, msize);
    exit(1);
  } 

  mav_mallocCount++;

  return result;
}

void mav_free(void *mem_ptr) 
{
  mav_mallocCount--;

  free(mem_ptr);
}



/* strdup is not defined on MacOS or on Win32 under Metrowerks compiler */

#if defined(macintosh) || defined(__MWERKS__)
char *strdup(const char *str) 
{
  char *newstr;
  if (!str) return 0;
  newstr= (char *) mav_malloc(strlen(str)+1);
  strcpy(newstr, str);
  return newstr;
}
#endif



/* Routine to perform micro second resolution sleep */

#ifdef MAV_IRIX5
#include <limits.h>
#include <unistd.h>
int sginap(long ticks);
#endif

#ifdef MAV_IRIX6
#include <limits.h>
#include <unistd.h>
long sginap(long ticks);
#endif

#ifdef MAV_LINUX
#include <unistd.h>
#ifndef MAV_FREEBSD
void usleep(unsigned long usec);
#endif
#endif

#ifdef WIN32
#include <time.h>
#endif

void mav_sleep(float len)
{
#ifdef MAV_IRIX5
  sginap((long) (len*CLK_TCK));
#else
#ifdef MAV_IRIX6
  sginap((long) (len*CLK_TCK));
#else
#ifdef WIN32
  clock_t goal= clock()+(len*CLOCKS_PER_SEC);
  while (goal>clock());
#else 
#ifdef macintosh
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: mav_sleep not implemented on Macs, ignoring\n");
#else
  usleep(len*1.0E+6);
#endif
#endif
#endif
#endif
}



/* Routine to add new modules and print a list of them */

void mav_moduleNew(MAV_moduleIDFn fn)
{
  mav_listItemAdd(mav_module_list, (void *) fn);
}

void mav_moduleDump(void)
{
  MAV_moduleIDFn fn;

  fprintf(stderr, "%s\n", mav_kernelID());
  mav_listPointerReset(mav_module_list);

  while (mav_listItemNext(mav_module_list, (void **) &fn)) fprintf(stderr, "Module: %s\n", (*fn)());
}



/* Wrappers to kernel defined callbacks */

void mav_callbackDeleteSet(MAV_window *w, MAV_class *c, MAV_callbackDeleteFn fn)
{
  mav_callbackSet(mav_callback_delete, w, c, (MAV_callbackFn) fn);
}

int mav_callbackDeleteExec(MAV_window *w, MAV_object *obj)
{
  return (mav_callbackExec(mav_callback_delete, w, obj, NULL, NULL));
}

void mav_SMSCallbackDeleteSet(MAV_SMSClass *c, MAV_SMSCallbackDeleteFn fn)
{
  mav_SMSCallbackSet(mav_SMSCallback_delete, c, (MAV_SMSCallbackFn) fn);
}

int mav_SMSCallbackDeleteExec(MAV_SMS *s, int o)
{
  return (mav_SMSCallbackExec(mav_SMSCallback_delete, s, (void *) &o, NULL, NULL, NULL));
}

void mav_SMSCallbackObjectRmvSet(MAV_SMSClass *c, MAV_SMSCallbackObjectRmvFn fn)
{
  mav_SMSCallbackSet(mav_SMSCallback_objectRmv, c, (MAV_SMSCallbackFn) fn);
}

int mav_SMSCallbackObjectRmvExec(MAV_SMS *s, MAV_object *obj)
{
  return (mav_SMSCallbackExec(mav_SMSCallback_objectRmv, s, (void *) obj, NULL, NULL, NULL));
}

int mav_SMSObjectRmv(MAV_SMS *s, MAV_object *o)
{
  return mav_SMSCallbackObjectRmvExec(s,o);
}
