/*
 * EDMA: Entorno de Desarrollo Modular y Abierto
 * Object Oriented and Componetware Framework
 * Copyright (C) 1998, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2010
 *    David Martínez Oliveira
 *
 * This file is part of EDMA.
 *
 * EDMA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * EDMA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with EDMA.  If not, see <http://www.gnu.org/licenses/>.
 */

/*
 * Entorno de Desarrollo Modular y Abierto
 * Versión 0.3r1
 * (c) David Martínez Oliveira
 * 
 * Módulo de inicialización para LINUX
 * ------------------
 * 24/11/1995
 *   Intentamos añadir gestión de traps para evitar salidas abruptas
 * -----------------------------------------
 * Febraury, 7th, 2001
 * Code cleanup and comment translation
 * ----------------------------------------------------------------
 * October, 19th, 2001
 * We have remove _init entry point due to problems linking libedma.so
 * with autotools 
 * ---------------------------------------------------------------------
 * November, 18th, 2001
 * Now, EDMAInit cheks if TMPDIR exists. If not, it  create it
 * ----------------------------------------------------------------
 * March, 2nd,2002
 * Code cleanup
 * --------------------------------------------------------------------------
 * May, 10th, 2003
 * Modification to support changes to internal class structures
 */
 
#include "portable.h"
#include "shmem.h"
#include "ini1.h"
#include "const.h"
#include "systyp.h"
#include "sys31.h"
#include "ttypes.h"
#include "misc.h"
#include "emi.h"
#include "vglobal.h"
#include "ini1.h"
#include "obj.h"

#include <signal.h>

/* Includes for stat. Is this GNU/Linux specific?*/
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>

/* Prototype for strsignal to remove compile warnning on linux*/
/* This file is system dependent*/
extern char *strsignal(int); 


void HandleEx(int);
EUint32 TheEnd(void);

/*EUint32 _init()*/
/* We remove _init function and now applications must explicitly
** call EDMAInit in order to bring the system up.
** 
** We can't make libtool to user the -nostartflags when linking the
** shared library
*/

/* EDMAInit
 *   Main GNU/EDMA initialisation function
 *   THis function will, in general, be specific for each supported system
 *   Must be called before any other function 
 */
EUint32 
EDMAInit()
{
  mode_t   my_umask;
  HMEM	   hGVar = 0;
  struct stat buf;

  /* Set default signal handlers for properly shutdown the system*/
  signal (SIGHUP   , HandleEx);
  signal (SIGINT   , HandleEx);
  signal (SIGQUIT  , HandleEx);
  signal (SIGILL   , HandleEx);
  signal (SIGABRT  , HandleEx);
  signal (SIGFPE   , HandleEx);
  signal (SIGSEGV  , HandleEx);
  signal (SIGPIPE  , HandleEx);
 
  /**********************************************************************/
  /* We must to check if /tmp/EDMAtmp exists, and else create it
   * Maybe this should be done during installation process and so
   * we don't need to check it each time a process loads EDMA.
   *
   * Other solution is to manage the GVar allocation independiently, that is
   * directly without using SAlloc, so we allways can check if other process
   * has created the directory
   */
  if ((stat (TMPDIR, &buf)) == -1) 
    {
      if (errno == ENOENT) 
	{ /* If doen't exist create it*/
	  
	  fprintf (stderr, "GNU EDMA Startup...");
	  fprintf (stderr, "%s temporally file doesn't exist. "
		   "Trying to create it\n", TMPDIR);
	  
	  my_umask = umask (0000);
	  if ((mkdir (TMPDIR, 0777)) == -1) 
	    {
	      fprintf (stderr, "Can't create %s directory\n", TMPDIR);
	      perror ("EDMAInit:");
	      fprintf (stderr, "Aborting...\n");
	      edma_sfree (hGVar, GVar);
	      exit(1);	
	    }
	  /* Restore old umask */
	  umask (my_umask);
	} 
      else 
	{ /* If there is some other problem*/
	  fprintf (stderr, "Can't stat %s\n", TMPDIR);
	  fprintf (stderr, "%s directory must exists. "
		   "Try create it manually\n", TMPDIR);
	  perror ("EDMAInit:");
	  fprintf (stderr, "Aborting...\n");
	  edma_sfree (hGVar, GVar);
	  exit (1);
	}
    }
  /* End TMPDIR creating */
  /***********************************************************************/

  /* We create or attach gloibal shared variables*/
  hGVar = edma_salloc (sizeof (SYS_GLOBAL_VAR), "MisVars1");
   
  GVar = (SYS_GLOBAL_VAR*) edma_sget (hGVar);
  edma_printf ("\nGNU/EDMA %d.%d.%d build [%s][%s][%s] starting up...",
	       VERSION_MA, VERSION_MI, VERSION_BUG, __DATE__, VERSION_STATUS, 
	       EVERSION);

  if (GVar->Running)
    edma_log ("Running: %d | nClasses: %d | nMaxClasses: %d | SystemPath: %s\n",
	      GVar->Running, GVar->nClases, GVar->nMaxClases, GVar->SystemPath);
  if (GVar->Running == 0)	
    { /* Is it first time execution? */
      GVar->Running++;
      GVar->AppIdGen = 0;
    } 
  else
    GVar->Running++;
  
  GVar->AppIdGen++;
  AppId = GVar->AppIdGen;
  sprintf (AppName, "%ld", AppId);
  if (GVar->DebugLevel == 0)
    GVar->DebugLevel = 69;

  GVar->hMySelf = hGVar;

  /* EDMA no-arch inicialization */
  EDMAIni ();

  return 0;
}

/* EDMAFree
 *    Main GNU/EDMA shutdown function
 */

EUint32 
EDMAEnd()
{
  HMEM      h;
  ESint32   i;
  /* Ignore signals on shutdown*/
  /* Set default signal handlers for properly shutdown the system*/
  signal (SIGHUP   , SIG_IGN);
  signal (SIGINT   , SIG_IGN);
  signal (SIGQUIT  , SIG_IGN);
  signal (SIGILL   , SIG_IGN);
  signal (SIGABRT  , SIG_IGN);
  signal (SIGFPE   , SIG_IGN);
  signal (SIGSEGV  , SIG_IGN);
  signal (SIGPIPE  , SIG_IGN);

  edma_printf ("-------------------[EDMAEnd]--------------------------------");
  /* Test for objects*/
  for (i = 0; i < nMaxObj; i++)
    {
      if (gObj[i] != NULL)
	{
	  edma_printf ("[WARNNING][%s] Object %d of class %s don't destroyed",
		       __FUNCTION__,
		       i, gClass[gObj[i]->IdClass]->ClassName);
	}
      
    }

  GVar->Running--;
  if (GVar->Running < 0) 
    {
      edma_printf ("System Error: Process reference below 0");
      GVar->Running = 0;
    }
  edma_log ("Process %d leaves GNU EDMA. %d process remaining", 
	       AppId, GVar->Running);
  

  EDMAFinish ();
  

  if (GVar->Running == 0) 
    {
      GVar->AppIdGen = 0;
      edma_printf ("No more process in GNU EDMA. Freeing All Resources...");

      EDMAFree ();
      /* TODO: Really free shared memory blocks. Deattach all the references
       * and then destroy it.
       *
       * It seems that only the process that creates the shared memory block
       * can really destroy it, so in a final GNU EDMA installation a root
       * process should be the first initiated one.
       *
       * we must to find out how all this stuff works
       * UPDATE: Now using emerald daemon to manage shared memory.
       *         No need to explicitly delete shared memory from apps.
       */
      h = GVar->hMySelf;
      
      edma_sunget (GVar);
      edma_sfree (h, GVar);
      edma_printf ("%s", "Cleanup done!!");
    }

  return 0;
}


/* HandleEx
 *   Generic Signal catcher to try ordered shutdown of the system of
 *   critical errors
 */
void 
HandleEx (int s)
{
  ETHREAD eth;

  eth = edma_thread_self();
  printf ("\n----------------------------------------------------------------");
  printf ("\n[GNUEDMA Exception Handler] Signal : %s [%d]. %ld running "
	  "threads -> Cleaning Up!!",
	  strsignal(s), s, num_threads);
  if (num_threads != 1)
    return;
  printf ("\n");
  EDMAEnd ();
  exit (1);
}
