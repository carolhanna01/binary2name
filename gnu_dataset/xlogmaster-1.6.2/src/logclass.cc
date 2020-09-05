/*

  logclass.cc

  class definitions for xlogmaster.cc
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
#include "extern.H"

/*}}}*/

using namespace std;

/*
  Log Class Routines:
 */
/*{{{  Constructor & Destructor  */
Log::Log()
{
  /*
    Initialize primary data elements
  */
  filename = NULL;
  help = NULL;
  buttontext = NULL;
  mode = EMPTY;
  interval = -1;
  delay = 6;
  filter = NULL;
  filterclass = NO_FILTER;
  plugin = NULL;

  /*
    now initialize the secondary elements
  */
  buffer = NULL;
  rbuffer = NULL;
  chunk = (glong) RDBLCK * INTERVAL;
  length = 0;
  got = 0;
  button = NULL;
  style = NULL;
  pixels[0] = 0;
  pixels[1] = 0;

  /*
    and the temporaries...
  */
  tag = 0;
  alert_tag = 0;
  fd = 0;
  active = FALSE;
  fade = 0;
  last_fade = 0;
  fadestep = 0;
  pid = 0;
  output_filter_pid = 0;
  fd_into_filter = 0;
  pipe_buf = 0;
  fd_from_filter = 0;

  input_buffer = output_buffer = NULL;
  input_buffer_size = output_buffer_size = 0;
  input_buffer_used = output_buffer_used = 0;

  pipe_lock = FALSE;
  pmi_tag = 0;

  up_to_date = TRUE;
  replace = TRUE;

  /*
    done... now the element should be nice and sober
  */

  return;
};
Log::~Log()
{
  
  /*
    Use the general "clean everything" routine on this element...
  */
  this->purge();
  
  return;
};
/*}}}*/

/*{{{  purge()  */
/*
  This routine brings the element back to the completely
  fresh state - no arrays, no values, no data...
*/
void 
Log::purge()
{

  /*
    we start with clearing the temporaries
  */
  up_to_date = TRUE;
  active = FALSE;
  if ( pmi_tag != 0 )
    {
      gtk_timeout_remove( pmi_tag );
      pmi_tag = 0;
    }
  if ( tag != 0 )
    {
      gtk_timeout_remove( tag );
      tag = 0;
    }
  if ( alert_tag != 0 )
    {
      gtk_timeout_remove( alert_tag );
      alert_tag = 0;
    }
  if ( fd != 0 )
    {
      close( fd );
      fd = 0;
    }
  fade = 0;
  last_fade = 0;
  fadestep = 0;
  if ( pid != 0 )
    {
      kill( pid, SIGTERM );
      pid = 0;
    }
  if ( output_filter_pid != 0 )
    {
      kill( output_filter_pid, SIGTERM );
      output_filter_pid = 0;
    }
  if ( fd_into_filter != 0 )
    {
      close( fd_into_filter );
      fd_into_filter = 0;
    }
  pipe_buf = 0;
  if ( fd_from_filter != 0 )
    {
      close( fd_from_filter );
      fd_from_filter = 0;
    }
  replace = TRUE;

  if ( input_buffer != NULL )
    {
      delete input_buffer;
      input_buffer = NULL;
    }  
  if ( output_buffer != NULL )
    {
      delete output_buffer;
      output_buffer = NULL;
    }
  input_buffer_size = output_buffer_size = 0;
  input_buffer_used = output_buffer_used = 0;
  pipe_lock = FALSE;

  /*
    now the secondaries
  */
  pixels[0] = 0;
  pixels[1] = 0;
  chunk = (glong) RDBLCK * INTERVAL;
  length = 0;
  got = 0;
  if ( button != NULL )
    {
      gtk_widget_destroy( button );
      button = NULL;
    }
  style = NULL;
  /* now free the buffers... */
  this->free_buffers();
  

  /*
    and finally the primaries
  */

  if ( filename != NULL )
    {
      delete filename;
      filename = NULL;
    }
  if ( help != NULL ) 
    {
      delete help;
      help = NULL;
    }  
  if ( buttontext != NULL )
    {
      delete buttontext;
      buttontext = NULL;
    }
  if ( plugin != NULL )
    {
      delete plugin;
      plugin = NULL;
    }
  mode = EMPTY;
  interval = -1;
  delay = 6;
  filterclass = NO_FILTER;
  if ( filter != NULL )
    {
      gint x = 0;
      while ( filter[x] != NULL )
	{
	  delete filter[x];
	  filter[x] = NULL;
	  x++;
	}
      filter = NULL;
    }
  
}
/*}}}*/
/*{{{  init  */
/*
  This routine initializes the primary data structures,
  Filter things are just being reset...
*/
void
Log::init(const gchar* fn, const gchar* hl, const gchar* bt, const gchar* pl, gint md, gint iv, gint dl)
{
  
  /*
    set it it initial state
  */
  this->purge();
  
  /* filename */
  filename = new gchar[ strlen(fn)+2 ];
  strcpy(filename, fn);
  
  /* helptext */
  if ( hl == NULL )
    {
      help = new gchar[ strlen(fn)+2 ];
      strcpy(help, fn);
    }
  else
    {
      help = new gchar[ strlen(hl)+2 ];
      strcpy(help, hl);
    }
  
  /* buttontext */
  buttontext = new gchar[ strlen(bt)+2 ];
  strcpy(buttontext, bt);

  /* plugin */
  if ( pl == NULL )
    {
      plugin = NULL;
    }
  else
    {
      plugin = new gchar[ strlen(pl)+2 ];
      strcpy( plugin, pl );
      gint valid_chars = 0;
      gint x = 0;
      while ( plugin[x] != 0 )
	{
	  if ( plugin[x] != ' ' &&
	       plugin[x] != '\r' &&
	       plugin[x] != '\n' &&
	       plugin[x] != '-' )
	    valid_chars++;
	  x++;
	}
      if ( valid_chars <= 0 )
	{
	  delete plugin;
	  plugin = NULL;
	}
    }

  /* mode */
  mode = md;
  
  /* interval */
  interval = iv;
  
  /* delay */
  delay = dl;

  /* 
     chunk gives the blocksize of the read for the watchdog
     must be maxtext for anything else than TAIL, of course.
  */
  chunk = RDBLCK * interval;
  if ( ( mode == CAT_FILE ) || ( chunk > maxtext ) )
    {
      chunk = maxtext;
    }
  
  /*
    if the size is bigger than SSIZE_MAX we need to limit
    it to SSIZE_MAX !
  */
  if ( chunk > SSIZE_MAX )
    {
      chunk = SSIZE_MAX;
    }
  

}
/*}}}*/
/*{{{  allocate_buffers  */
/*
  This routine allocates the space that is needed for proper
  functionality
*/
void
Log::allocate_buffers()
{
  
  if ( filterclass & CLASS1_FILTER )
    {
      if ( mode == TAIL_FILE || mode == RUN_FILE )
	{
	  buffer = new gchar[maxtext];
	}

      rbuffer = new gchar[chunk];
      
      length = 0;
      got = 0;
      
    }
  else
    {
      
      /* if we don't need the buffers, free them */
      this->free_buffers();
      
    }

}
/*}}}*/
/*{{{  free_buffers  */
/*
  This routine frees the buffers again
*/
void
Log::free_buffers()
{
  
  if ( buffer != NULL )
    {
      delete buffer;
      buffer = NULL;
    }

  if ( rbuffer != NULL )
    {
      delete rbuffer;
      rbuffer = NULL;
    }

  length = 0;
  got = 0;

}
/*}}}*/
/*{{{  add_filter  */
/*
  This routine adds a filter to the array and maintains it
  NULL terminated, creating one if it is empty and so on...
*/
void
Log::add_filter(const gchar* str, gint md, const gchar* el)
{
  /* count amount of elements */
  gint amount = 0;
  while ( filter != NULL && filter[amount] != NULL )
    {
      amount++;
    }

  /* copy over list */
  Filter** old = filter;
  filter = (Filter**) new void*[amount+2];
  amount = 0;
  while ( old != NULL && old[amount] != NULL )
    {
      filter[amount] = old[amount];
      amount++;
    }

  /* delete old array */
  if ( old != NULL ) delete old;
  old = NULL;

  /* create new & NULL terminate list */
  filter[amount+1] = NULL;
  filter[amount] = new Filter();

  /* initialize new one */
  filter[amount]->init( str, md | COMPILE_REGEX );
  if ( el != NULL )
    {
      filter[amount]->execline = new gchar[strlen(el) + 2];
      strcpy(filter[amount]->execline, el);
    }
  else
    {
      filter[amount]->execline = NULL;
    }

  /* sort the new list */
  sort_filters( filter );

  /* and now set the filter class bits accordingly */
  filterclass |= determine_filter_class( md );

  return;
}
/*}}}*/



/*  
  Filter Class Routines
*/

/*{{{  Constructor & Destructor  */
Filter::Filter()
{
 
  string = NULL;
  regex_pattern = NULL;
  execline = NULL;
  mode = NO_CHANGE;
 
  /* status is zero (no problem) by default */
  regex_status = 0;
  
  return;

};
Filter::~Filter()
{
  
  if ( string != NULL )
    {
      delete string;
    }
  string = NULL;
  
  if ( regex_pattern != NULL )
    {
      regfree(regex_pattern);
      delete regex_pattern;
      regex_pattern = NULL;
    }
  
  if ( execline != NULL )
    {
      delete execline;
    }
  execline = NULL;

  mode = NO_CHANGE;
  
  return;

};
/*}}}*/

/*{{{  init()  */
void
Filter::init(const char* str, gint new_mode)
{

  /* we do not want to preserve the calculation command,
     so we will mask everything but the modes needed during
     runtime out */
  mode = new_mode & RUNTIME_MODES_MASK; 
  
  string = new gchar[ strlen(str) + 2 ];
  strcpy( string, str );

  if ( regex_pattern != NULL )
    {

      regfree(regex_pattern);
      delete regex_pattern;
      regex_pattern = NULL;
      regex_status = 0;

    }
  
  if ( ! (new_mode & COMPILE_REGEX) )
    return;
  
  /*
    only if COMPILE_REGEX is given we will actually try to compile
    the REGEX !!!
    This prevents multiple error messages reinitialisation of the program 
    for the same problem because often filter elements are only being
    used as temporary structures.
  */
  
  regex_pattern = new regex_t;
  gint compile_flags = ( REG_EXTENDED | REG_NOSUB | REG_NEWLINE );

  /*
    set the translation table according to whether the
    user wants case sensitivity
  */
  if ( !( mode & CASE_SENSITIVE ) )
    {
      compile_flags |= REG_ICASE;
    }
  
  /*
    compile expression
  */
  gint result = regcomp( regex_pattern, string, compile_flags );

  if ( result != 0 )
    {
      
      gint need = regerror( result, regex_pattern, NULL, 0);
      gchar* error = new gchar[need+2];
      regerror( result, regex_pattern, error, need+1 );

      /* there has been an error during compilation 
	 output error messages to user: */
      cerr << PACKAGE << ": " << "error compiling REGEX ";
      cerr << "' "<< string << " '" << endl;
      cerr << "  regcomp returned: " << error << endl;
      
      /* and create a popup dialog box */
      gchar* opener 
	= "\n\t     Error while trying to compile REGEX     \t\n\n\t      ";
      gchar* closer 
	= "      \t\n\n\t   calling regcomp returned:     \t\n\n";
      gchar* text 
	= new gchar[strlen(error) + 
		   strlen(opener) + 
		   strlen(closer) + 
		   strlen(string) + 10 ];
      strcpy ( text, opener );
      strcat ( text, string );
      strcat ( text, closer );
      strcat ( text, error );
      strcat ( text, "\n\t" );
      
      GList* buttons = g_list_alloc();
      GList* functions = g_list_alloc();
      buttons = g_list_append(buttons,
			      (gpointer) "\t   DISMISS   \t");
      functions = g_list_append(functions, NULL );

      popup_dialog("Invalid REGEX", text, buttons, functions);

      /* delete the strings again... */
      delete text;
      delete error;
      
      /* and the pattern since we can't use it anyways */
      delete regex_pattern;
      regex_pattern = NULL;
    }

  return;
}
/*}}}*/

/*{{{  purge()  */
void
Filter::purge()
{

  if ( string != NULL )
    {
      delete string;
    }
  string = NULL;

  if ( regex_pattern != NULL )
    {

      regfree(regex_pattern);
      delete regex_pattern;
      regex_pattern = NULL;
      regex_status = 0;

    }

  if ( execline != NULL )
    {

      delete execline;

    }
  execline = NULL;

  mode = NO_CHANGE;
  
}
/*}}}*/
