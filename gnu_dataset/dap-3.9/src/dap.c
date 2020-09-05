/* Controls processes for editing, preprocessing,
 * compiling, and running Dap programs.  
 */

/*  Copyright (C) 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
 *
 *  This file is part of Dap.
 *
 *  Dap is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  Dap is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Dap.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <signal.h>
#include <string.h>


#define ED "/usr/bin/emacs"

#define DAPPP "/usr/local/bin/dappp"
#define DAPRUNS "/usr/local/bin/dapruns"
#define INCDIR "/usr/local/include"
#define LIBDIR "/usr/local/lib"

char *editor;
char *edopts;

char *dappp;
char *dapruns;
char *incdir;
char *libdir;

void edrun(int argc, char **argv);
char *argcpy(char arg[], int extra);
void suffix(char name[], char suff[]);
int parseopts(char *opts, char **arg);

char *ecopy(char *e)
{
  char *copy;

  if (e)
    {
      if (!(copy = malloc(strlen(e) + 1)))
	{
	  perror("dap");
	  exit(1);
	}
      strcpy(copy, e);
      return copy;
    }
  return NULL;
}

int main(int argc, char **argv)
{
  int runstat;	/* return status of execution of program */

  fputs("\nDap, Copyright (C) 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.\n",
	stderr);
  fputs("Dap comes with ABSOLUTELY NO WARRANTY;\n", stderr);
  fputs("for details see the GNU Public License.\n", stderr);
  fputs("This is free software, and you are welcome to\n", stderr);
  fputs("redistribute it under certain conditions; see\n", stderr);
  fputs("the GNU Public License for details.\n\n", stderr);

  if (argc < 2)
    {
      fputs("dap: no files to process\n", stderr);
      exit(1);
    }
  if (argc <= 1 || (!strcmp(argv[1], "--help") || !strcmp(argv[1], "-h")))
    {
      fputs("Usage:\ndap [-k] [-d] FILE1.c [ FILE2.c ... ] [-a ARG1 ...]\n", stderr);
      fputs("dap [--keep] [--debug] FILE1.c [ FILE2.c ... ] [--args] ARG1 ...]\n", stderr);
      fputs("dap [-k] [-d] FILE1.sbs [ FILE2.c ... ]\n", stderr);
      fputs("dap [--keep] [--debug] FILE1.sbs [ FILE2.c ... ]\n", stderr);
      fputs("\nReport bugs to <bug-dap@gnu.org>\n", stderr);
      exit(1);
    }
  else if (argc == 2 && argv[1][0] == '-')
    {
      if (!strcmp(argv[1], "--version") || !strcmp(argv[1], "-v"))
	{
	  fputs("Dap 3.6\n", stderr);
	  exit(1);
	}
      else
	{
	  fprintf(stderr, "dap: bad option: %s\n", argv[0]);
	  exit(1);
	}
    }
  if (!(editor = ecopy(getenv("DAPEDITOR"))))
    editor = ED;
  if (!(edopts = ecopy(getenv("DAPEDOPTS"))))
    edopts = NULL;
  if (!(dappp = ecopy(getenv("DAPPP"))))
    dappp = DAPPP;
  if (!(dapruns = ecopy(getenv("DAPRUNS"))))
    dapruns = DAPRUNS;
  if (!(incdir = ecopy(getenv("INCDIR"))))
    incdir = INCDIR;
  if (!(libdir = ecopy(getenv("LIBDIR"))))
    libdir = LIBDIR;
  edrun(argc, argv);
}

/* source file types */
#define C 0
#define SBS 1

int srctype(char *name)
{
  int n;

  n = strlen(name);
  if (n > 2 && !strcmp(name + n - 2, ".c"))
    return C;
  else if (n > 4 && !strcmp(name + n - 4, ".sbs"))
    return SBS;
  else
    {
      fprintf(stderr, "dap: name must end in .c or .sbs: %s\n", name);
      exit(1);
    }
}

/* run editor */
void edrun(int argc, char **argv)
{
  char **arg;	/* copy of arguments, modified */
  int av;		/* index to argv */
  int a;		/* index to arguments */
  char *argstr;	/* original argument list, as string */
  int argstrlen;	/* length of argument list, as string */
  int argstart;      /* start of arguments following options */

  if (!(arg = (char **) malloc(sizeof(char *) *
			       (argc + 5 + parseopts(edopts, NULL)))))
    {	/* + 5 for .log, .lst, .c, --eval, (shell-command "dapruns ... &") */
      perror("dap");
      exit(1);
    }
  a = 0;
  for (argstart = 1; argstart < argc && argv[argstart][0] == '-'; argstart++)
    ;
  arg[a++] = argcpy(argv[argstart], 0);
  a += parseopts(edopts, arg + a);
  arg[a] = argcpy(argv[argstart], 4);
  suffix(arg[a++], ".log");
  arg[a] = argcpy(argv[argstart], 4);
  suffix(arg[a++], ".lst");
  switch (srctype(argv[argstart]))
    {
    case C:
      arg[a] = argcpy(argv[argstart], 2);
      suffix(arg[a++], ".c");
      break;
    case SBS:
      arg[a] = argcpy(argv[argstart], 4);
      suffix(arg[a++], ".sbs");
      break;
    }
  /* now put arguments back into a string
   */
  for (argstrlen = strlen("(shell-command \" &\")") + strlen(dapruns) + 1, av = 1;
       av < argc; av++)
    argstrlen += strlen(argv[av] + 1);
  argstr = malloc(argstrlen);
  sprintf(argstr, "(shell-command \"%s ", dapruns);
  for (av = 1; av < argc; av++)
    {
      strcat(argstr, argv[av]);
      strcat(argstr, " ");
    }
  strcat(argstr, "&\")");
  arg[a++] = "--eval";
  arg[a++] = argstr;
  /* Only use arguments preceding "-a"; arguments following "-a"
   * are arguments to program  
   */
  for (av = argstart + 1;
       av < argc && strcmp(argv[av], "-a") && strcmp(argv[av], "--args"); av++)
    arg[a++] = argv[av];
  arg[a] = NULL;
  execv(editor, arg);
  perror(editor);
  exit(1);
}

/* replace trailing ".c" with suff */
void suffix(char name[], char suff[])
{
  int n;

  n = strlen(name);
  if (n > 2 && !strcmp(name + n - 2, ".c"))
    {
      name[n - 2] = '\0';
      strcat(name, suff);
    }
  else if (n > 4 && !strcmp(name + n - 4, ".sbs"))
    {
      name[n - 4] = '\0';
      strcat(name, suff);
    }
  else
    {
      fprintf(stderr, "dap: name must end in .c or .sbs: %s\n", name);
      exit(1);
    }
}

/* allocate copy of argument, with extra characters if requested,
 * and copy argument; return address of copy
 */
char *argcpy(char arg[], int extra)
{
  char *cpy;

  if (!(cpy = malloc(strlen(arg) + extra + 1)))
    {
      perror("dap");
      exit(1);
    }
  strcpy(cpy, arg);
  return cpy;
}

/* If arg non NULL, copy arguments, report number found,
 * else just report number found.  
 */
int parseopts(char *opts, char **arg)
{
  static char *optcpy = NULL;	/* copy of opts */
  static int optlen = 0;
  int i;		/* index to opts */
  int a;		/* arg count */

  if (!opts)
    return 0;
  if (strlen(opts) > optlen)
    {
      if (optcpy)
	free(optcpy);
      optlen = strlen(opts);
      if (!(optcpy = malloc(optlen + 1)))
	{
	  perror("dap");
	  exit(1);
	}
    }
  strcpy(optcpy, opts);
  for (i = 0; optcpy[i] == ' '; i++)
    ;
  for (a = 0; optcpy[i]; a++)
    {
      if (arg)
	arg[a] = optcpy + i;
      while (optcpy[i] && optcpy[i] != ' ')
	i++;
      if (optcpy[i])
	{
	  if (arg)
	    optcpy[i] = '\0';
	  for (i++; optcpy[i] == ' '; i++)
	    ;
	}
    }
  return a;
}
