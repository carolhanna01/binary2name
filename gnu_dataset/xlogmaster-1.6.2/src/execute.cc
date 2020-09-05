/*

  execute.cc

  program execution subroutines for xlogmaster.cc
  Copyright (C) 1998,1999 Georg C. F. Greve
  
This file is part of GNU xlogmaster.

GNU xlogmaster is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

GNU xlogmaster is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see http://www.gnu.org/licenses/.

  
  Contact: 
           mailto:xlogmaster-bugs@gnu.org
           http://www.gnu.org/software/xlogmaster/
  Secondary sources:
           http://www.fusebox.hanse.de/xlogmaster/
	 
*/

/*{{{  Header  */

#include "../config.h"
#include "sysinc.H"
#include "logclass.H"
#include "execute.H"
#include "extern.H"

/*}}}*/

using namespace std;

/*{{{  execute_program  */
void 
execute_program(gint i, gint f, gchar* line)
{
  gchar* execline = entry[i]->filter[f]->execline;
  gchar* name = (gchar*) entry[i]->buttontext;
  gchar* filename = (gchar*) entry[i]->filename;
  gchar* helptext = (gchar*) entry[i]->help;
  gchar* modeline = NULL;

  switch ( entry[i]->mode )
    {
    case TAIL_FILE:
      modeline = "TAIL";
      break;
    case CAT_FILE:
      modeline = "CAT";
      break;
    case RUN_FILE:
      modeline = "RUN";
      break;
    default:
      modeline = "ERROR";
      break;
    };

  if ( execline == NULL ) return;

  pid_t pid = fork();
  if ( pid > 0 )
    {
      /* PARENT, fork succeeded */
      
      reference_child(-1); /* we don't care about the end of this one as
			      long as it does get terminated... */
      return;
    }
  else  if ( pid == -1 )
    {
      /* PARENT, fork failed */
      
      fork_error();
      return;
      
    }

  /*
    +++ NOW WE ARE A CHILD PROCESS +++
  */
  
  
  char** argv = split_execute_line( execline );
  
  
  /*
    and now see whether there are variables to subsitute
    this is done in this order because it should pose the
    lowest threat to having a variable in a replaced string that
    triggers another replacement later... 
  */
  gint x;
  
  /* replace all %M */
  x = 0;
  while ( argv[x] != NULL )
    {
      argv[x] = substitute(argv[x], "%M", modeline);
      x++;
    }
  my_setenv( "XLM_MODE", modeline); // and set environment variable
  
  /* replace all %F */
  x = 0;
  while ( argv[x] != NULL )
    {
      argv[x] = substitute(argv[x], "%F", filename);
      x++;
    }
  my_setenv( "XLM_FILENAME", filename); // and set environment variable
  
  /* replace all %N */
  x = 0;
  while ( argv[x] != NULL )
    {
      argv[x] = substitute(argv[x], "%N", name);
      x++;
    }
  my_setenv( "XLM_NAME", name); // and set environment variable
  
  /* replace all %H */
  x = 0;
  while ( argv[x] != NULL )
    {
      argv[x] = substitute(argv[x], "%H", helptext);
      x++;
    }
  x = my_setenv( "XLM_HELP", helptext); // and set environment variable
  
  /* replace all %L */
  x = 0;
  while ( argv[x] != NULL )
    {
      argv[x] = substitute(argv[x], "%L", line);
      x++;
    }
  my_setenv( "XLM_LINE", line); // and set environment variable
  

  /*
    Now we assemble the whole thing again into one string...

    first it will be counted then we allocate a string and
    then we put everything into it...
  */
  x = 0;
  gint length = 0;
  while ( argv[x] != NULL )
    {
      length += strlen( argv[x] );
      x++;
    }

  gchar* argument = new gchar[ length + x + 3 ];
  argument[0] = 0;

  x = 0;
  while ( argv[x] != NULL )
    {
      strcat( argument , argv[x] );
      strcat( argument , " " );
      x++;
    }
  
  x = 0;
  while ( argv[x] != NULL )
    {
      delete argv[x];
      x++;
    }
  delete argv;
  
  /* create final argv array for running program */
  argv = new gchar*[4];  
  argv[0] = "/bin/sh";
  argv[1] = "-c";
  argv[2] = argument;
  argv[3] = NULL;
  
  /* now we got the argv[] array set up, so it's time to
     start the program... */
  execvp(argv[0], argv);

  /* bail out unceremoniously :-) */
  _exit(0);
}
/*}}}*/

/*{{{  execution subroutines  */

/*
  this routine creates an argv array from a string.

  the parameter is a string that gives the command
  to start with the parameters.

  the return value is a NULL terminated argv array
  of strings.
  
*/
gchar**
split_execute_line(gchar* execline)
{
  gchar** argv = NULL;
  gint argc = 0;
  
  /*
    split the long line up in it's argv parts...
  */
  
  gint a = 0;
  gint length = strlen(execline);  
  gchar* tempstring = new gchar[strlen(execline)+10];
  while ( a < length )
    {
      gint b = 0;
      
      while ( a < length && 
	      execline[a] != ' ' &&
	      execline[a] != '´' &&
	      execline[a] != '`' &&
	      execline[a] != '"' &&
	      execline[a] != '\'' )
	{
	  tempstring[b++] = execline[a++];
	}
      tempstring[b] = 0;

      if ( a < length && 
	   b == 0 && 
	   execline[a] != ' ' )
	{
	  tempstring[b++] = execline[a++];
	  tempstring[b] = 0;
	}
    
      if ( strlen(tempstring) > 0 )
	{
	  
	  if ( argc == 0 )
	    {
	      argv = new char*[2];
	      argv[0] = new char[strlen(tempstring)+2];
	      argv[1] = NULL;
	    } else {
	      char** temp = new char*[argc+2];
	      gint x = 0;
	      while ( argv[x] != NULL ){
		temp[x] = argv[x];
		x++;
	      }
	      temp[x] = temp[x+1] = NULL;
	      delete argv;
	      argv = temp;
	      argv[argc] = new char[strlen(tempstring)+2];
	    }
	  
	  strcpy(argv[argc], tempstring);
	  argc++;
	  
	}
      
      while ( a < length &&
	      execline[a] == ' ' ) a++;
      
    }
  delete tempstring;
  
  return argv;
}

/*
  sets environment variable (first parameter) to value (second parameter).

  Basically a rewrite of the setenv function on some systems because not
  all of them support it.
*/
gint
my_setenv(const gchar* variable, const gchar* value){
  gint x = -1;
#if HAVE_PUTENV
  gchar* envstring = new gchar[strlen(value) + strlen(variable) + 2];
  strcpy( envstring, variable);
  strcat( envstring, "=");
  strcat( envstring, value );
  x = putenv( envstring );
#endif /* HAVE_PUTENV */
  return x;
}

/* 
   replaces all occurences of pattern in target with 
   replacement and gives back pointer to new string 
*/ 
gchar* substitute(gchar* target, gchar* pattern, gchar* replacement){
  gint x,y;
  gint substitutions;
  gint patternlength = strlen(pattern);

  /* see whether there are any replacements to be made */
  substitutions = 0;
  x = 0;
  while ( target[x] != 0 ){

    y = 0;
    while ( target[x+y] == pattern[y] 
	    && target[x+y] != 0 
	    && pattern[y] != 0 ) y++;

    if ( y == patternlength ) substitutions++;
    
    x++;
  }

  /* if no replacements are to be made return the original string */
  if ( substitutions == 0 ) return target;

  /* otherwise allocate enough memory for the new string plus 
     a little spare for safety */
  gchar* string = new gchar[ substitutions * strlen(replacement)
			   + strlen(target) + 10 ];

  /* now do replacement */
  gint a = 0;
  gint b;
  x = 0;
  while ( target[x] != 0 ){
    y = 0;
    while ( target[x+y] == pattern[y] 
	    && target[x+y] != 0 
	    && pattern[y] != 0 ) y++;
    
    if ( y == patternlength ){
      b = 0;
      
      while ( replacement[b] != 0 )
	string[a++] = replacement[b++];
      
      x += patternlength;
    } else string[a++] = target[x++];
  }
  string[a] = 0;

  delete target;
  return string;
}
/*}}}*/



/*{{{  reference_child  */
void 
reference_child(gint pid)
{
  pid_reference++;
  if ( cleanup_tag == 0 )
    cleanup_tag = gtk_timeout_add (100,
				   *cleanup_interrupt,
				   (gpointer*) NULL);
  
  /* do we need to track the termination of this one ? */
  if ( pid != -1 )
    pid_list = g_list_append(pid_list, (gpointer) pid );
}
/*}}}*/
/*{{{  check_terminate  */
gint 
check_terminate(gint pid)
{
  gint index = g_list_index(pid_list, (gpointer) pid);
  if ( index == -1 ) return TRUE; // there is no such pid in the list
  else return FALSE; // it is still in there - the process is still running
}
/*}}}*/
/*{{{  cleanup_interrupt  */
gint 
cleanup_interrupt(gpointer)
{

  gint result = waitpid(-1, NULL, WNOHANG);

  while ( result > 0 )
    {
 
      pid_reference--;
  
      if ( pid_reference < 0 )
	{
	  cerr << "OOoops in execute.cc::cleanup_interrupt, pid_reference negative..." << endl;
	  pid_reference = 0;
	}
      
      gint index = g_list_index(pid_list, (gpointer) result);

      if ( index != -1 ) pid_list = g_list_remove( pid_list, (gpointer) result );
      
      if ( pid_reference == 0 )
	{
	  gtk_timeout_remove(cleanup_tag);
	  cleanup_tag = 0;
	}

      result = waitpid(-1, NULL, WNOHANG);
    }

  return TRUE;
}
/*}}}*/

