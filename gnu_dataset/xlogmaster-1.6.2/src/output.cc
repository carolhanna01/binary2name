/*

  output.cc

  output filter/plugin subroutines for xlogmaster.cc
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
#include "output.H"
#include "extern.H"

/*}}}*/

using namespace std;

/*{{{  pipe_magic  */
/*

  This function takes a buffer and pipes it into the plugin,
  then it checks for output from the plugin and replaces the data
  in the buffer with it. The amount of retrieved data is found
  in the return value.

  return value = amount of data read from plugin
  gint i = number of entry
  transfer_buffer = buffer to use
  transfer_chars = amount of bytes to be written into the plugin
  buffer_size = size of the used buffer

 */
glong
pipe_magic(gint i, gchar* transfer_buffer, glong transfer_chars, glong buffer_size)
{
  /* lock out interrupt */
  entry[i]->pipe_lock = TRUE;

  /* Anything to put into our buffer ? */
  if ( transfer_chars > 0 )
    {
      
      gint x = entry[i]->input_buffer_used;

      if ( entry[i]->input_buffer_used + transfer_chars 
	   >= entry[i]->input_buffer_size )
	{
	  
	  /* determine size of new buffer */
	  entry[i]->input_buffer_size = 
	    entry[i]->input_buffer_used + 
	    transfer_chars + (glong) ( entry[i]->pipe_buf / 2 );
	  
	  if ( entry[i]->input_buffer_size < entry[i]->pipe_buf )
	    entry[i]->input_buffer_size = entry[i]->pipe_buf;
	  
	  /* create new buffer */
	  gchar* new_buffer = new gchar[ entry[i]->input_buffer_size ];
	  
	  /* copy over old data into the pipe */
	  for ( gint k = 0 ; k < entry[i]->input_buffer_used ; k++ )
	    new_buffer[k] = entry[i]->input_buffer[k];
	  
	  /* delete old buffer, activate new one */
	  if ( entry[i]->input_buffer != NULL )
	    delete entry[i]->input_buffer;
	  entry[i]->input_buffer = new_buffer;
	  
	}
      
      /* append new data into the pipe */
      for ( gint y = 0 ; y < transfer_chars ; y++ )
	entry[i]->input_buffer[x++] = transfer_buffer[y];
      
      /* the length is the combined amount of bytes */
      entry[i]->input_buffer_used += transfer_chars;
      
    }

  /* now check for data to return */
  glong transferred_chars = 0;
  if ( entry[i]->output_buffer_used > 0 )
    {

      glong n = 0;
      while ( n < entry[i]->output_buffer_used &&
	      n < buffer_size )
	{
	  
	  transfer_buffer[n] = entry[i]->output_buffer[n];
	  n++;

	}
      transferred_chars = n;

      /* subtract the amount of transferred bytes from the amount
	 in the output buffer */
      entry[i]->output_buffer_used =
	entry[i]->output_buffer_used - transferred_chars;

      /* should we shrink the buffer a little bit ? */
      if ( entry[i]->output_buffer_size - entry[i]->output_buffer_used
	   > (glong) ( entry[i]->pipe_buf + entry[i]->pipe_buf + 50 ) )
	{

	  /* determine size for new buffer */
	  entry[i]->output_buffer_size = (glong) 
	    (entry[i]->output_buffer_used 
	     + entry[i]->pipe_buf 
	     + entry[i]->pipe_buf + 10 );
	  
	  /* allocate new buffer */
	  gchar* new_buffer = new gchar[entry[i]->output_buffer_size];
	  
	  /* copy over data */
	  for ( gint o = 0 ; o < entry[i]->output_buffer_used ; o++ )
	    new_buffer[o] = entry[i]->output_buffer[o];

	  /* delete old one */
	  if ( entry[i]->output_buffer != NULL )
	    delete entry[i]->output_buffer;

	  /* activate new buffer */
	  entry[i]->output_buffer = new_buffer;

	}

    }

  entry[i]->pipe_lock = FALSE;

  /* if interrupt isn't running: start it ! */
  if ( entry[i]->pmi_tag == 0 )
    {
      /* add the interrupt routine, it gets run 4 times as often
	 as the usual "data lookup" routines to make sure everything
	 is okay */
      entry[i]->pmi_tag = gtk_timeout_add ( entry[i]->interval*25,
					    *pipe_magic_interrupt,
					    (gpointer*) i );

      if ( entry[i]->output_buffer_used == 0 &&
	   entry[i]->input_buffer_used == 0 )
	{

	  entry[i]->up_to_date = TRUE;

	}
    }

  return transferred_chars;
  
}
/*}}}*/

/*{{{  pipe_magic_interrupt  */
/*
  interrupt routine does the real transfer between the buffers and the
  plugin
*/
gint 
pipe_magic_interrupt(gpointer data)
{
  gint i = (gint) data;
  
  /* are we locked out ? */
  if ( entry[i]->pipe_lock == TRUE )
    return TRUE;

  /* is space for output less than entry[i]->pipe_buf ? */
  if ( entry[i]->output_buffer_size - 
       entry[i]->output_buffer_used <= entry[i]->pipe_buf )
    {
      
      /* recalculate required size */
      entry[i]->output_buffer_size = entry[i]->output_buffer_used
	+ entry[i]->pipe_buf + entry[i]->pipe_buf + 40;

      /* allocate new buffer */
      gchar* new_buffer = new gchar[entry[i]->output_buffer_size];

      /* copy over old data */
      for ( gint x = 0 ; x < entry[i]->output_buffer_used ; x++ )
	new_buffer[x] = entry[i]->output_buffer[x];

      /* delete old array */
      if ( entry[i]->output_buffer != NULL )
	delete entry[i]->output_buffer;

      /* activate new array */
      entry[i]->output_buffer = new_buffer;

    }

  /* read data from plugin if any */
  glong got = 0;
  if ( entry[i]->fd_from_filter != 0 )
    got = read ( entry[i]->fd_from_filter ,
		 &entry[i]->output_buffer[entry[i]->output_buffer_used],
		 entry[i]->pipe_buf );
  
  /* and add read data to data counter */
  if ( got >= 0 ) entry[i]->output_buffer_used += got;
  
  /* make sure plugin is really running */
  assure_plugin_running(i);

  
  /* do we have data to be sent into the plugin ? */
  if ( entry[i]->input_buffer_used > 0 )
    {

      /* how much should we try to write ? */
      glong put = entry[i]->input_buffer_used;
      if ( put > entry[i]->pipe_buf )
	put = entry[i]->pipe_buf;


      /* try to write to pipe */
      put = write ( entry[i]->fd_into_filter ,
		    entry[i]->input_buffer ,
		    put );

      /* if we wrote something we need to update the
	 buffer accordingly */
      if ( put > 0 )
	{

	  /* subtract amount of written bytes */
	  entry[i]->input_buffer_used =
	    entry[i]->input_buffer_used - put;	  
	  
	  gchar* source = &entry[i]->input_buffer[put];
	  gchar* target = entry[i]->input_buffer;
	  gchar* new_buffer = NULL;
	  
	  if ( entry[i]->input_buffer_size 
	       - entry[i]->input_buffer_used >
	       ( glong ) ( entry[i]->pipe_buf + entry[i]->pipe_buf ) )
	    {

	      /* determine new size */
	      entry[i]->input_buffer_size = entry[i]->input_buffer_used
		+ entry[i]->pipe_buf;
	      
	      /* allocate new buffer */
	      new_buffer = new gchar[ entry[i]->input_buffer_size ];

	      /* target is new buffer */
	      target = new_buffer;
	      
	    }
	  
	  /* copy over data */
	  for ( gint y = 0 ; y < entry[i]->input_buffer_used ; y++ )
	    target[y] = source[y];
	  
	  /* if we allocated a new buffer we delete the old one
	     and activate it */
	  if ( new_buffer != NULL )
	    {
	      
	      if ( entry[i]->input_buffer != NULL )
		delete entry[i]->input_buffer;
	      
	      entry[i]->input_buffer = new_buffer;
	      
	    }
	  
	}
      
    }
  
  if ( entry[i]->output_buffer_used == 0 &&
       entry[i]->input_buffer_used == 0 )
    {

      gtk_timeout_remove( entry[i]->pmi_tag );
      entry[i]->pmi_tag = 0;
            
    }

  return TRUE;
}
/*}}}*/

/*{{{  assure_plugin_running  */
/*
  This function takes the number of the entry as the
  argument and it's function is to make sure that the
  filter/plugin is running properly. If needed it will
  kill the old process and start a new one. It also
  controls the pipe-setup.
*/
void
assure_plugin_running(gint i)
{

  /* Check for:
     terminated program
     pid = 0 (not started yet)
     any pipe = 0 (pipes not initialized properly)
  */
  if ( ! ( check_terminate( entry[i]->output_filter_pid ) == TRUE ||
	   entry[i]->output_filter_pid == 0 ||
	   entry[i]->fd_into_filter == 0 ||
	   entry[i]->fd_into_filter == 0 ) )
    {
      /* If the plugin is running, pipes are set up and it did
	 not terminate, we got nothing to do here ! */
      
      return;

    }
    
  /*
    Program is not running properly ... we need to start it
  */
  
  /* close all open pipes... */
  if ( entry[i]->fd_into_filter != 0 )
    {
      close(  entry[i]->fd_into_filter );
      entry[i]->fd_into_filter = 0;
    }
  
  if ( entry[i]->fd_from_filter != 0 )
    {
      close(  entry[i]->fd_from_filter );
      entry[i]->fd_from_filter = 0;
    }
  
  /* if program is still running at this point there has obviously 
     been a problem with the pipes. We kill it and restart. */
  if ( entry[i]->output_filter_pid != 0 &&
       check_terminate( entry[i]->output_filter_pid ) == FALSE )
    {	  
      kill( entry[i]->output_filter_pid, SIGKILL );
      entry[i]->output_filter_pid = 0;
    }
  
  
  /*
    O.k. - everything should be clear to be set up again now
  */
  
  /* prepare pipes */
  gint pipe_io[2][2];      
  pipe( pipe_io[0] ); /* set up input pipe */
  pipe( pipe_io[1] ); /* set up output pipe */
  
  pid_t pid = fork();
  
  if ( pid == 0 )
    {
      
      /*
	+++ NOW WE ARE A CHILD PROCESS +++
      */
      
      /* we will start it via "/bin/sh -c" */
      char** argv = new gchar*[5];
      argv[0] = "/bin/sh";
      argv[1] = "-c";
      argv[2] = new gchar[ strlen(entry[i]->plugin) + 2 ];
      strcpy( argv[2] , entry[i]->plugin );
      argv[3] = NULL;
      
      /* redirect stdin(0), stdout (1) and stderr(2) */
      dup2( pipe_io[0][0], 0 );
      dup2( pipe_io[1][1], 1 );
      dup2( pipe_io[1][1], 2 );
      
      
      /* set up PATH environment variable so the plugin directory
	 will always have priority over everything else */
      gchar* system_path = NULL;
      system_path = (gchar*) getenv("PATH");
      gchar* xlm_path;
      xlm_path = (gchar*) new char [ strlen( lib_dir ) + 
				   strlen( system_path ) + 40 ];
      strcpy(xlm_path, lib_dir);
      strcat(xlm_path, "/output-plugins:");
      strcat(xlm_path, system_path);
      my_setenv( "PATH", xlm_path);
      delete xlm_path;
      
      /* now it's time to start the program... */
      execvp(argv[0], argv);
      
      /* ooops. if we get here the execution failed ! */
      switch ( errno )
	{
	case EACCES:
	  cerr << "execution of plugin failed, ";
	  cerr << "insufficient permission" << endl;
	  break;
	case EPERM:
	  cerr << "execution of plugin failed, ";
	  cerr << "suid on nosuid-mounted filesystem" << endl;
	  break;
	default:
	  cerr << "execution of plugin failed" << endl;
	  break;
	}
      
      /* bail out directly... */
      _exit(0);
    }
  
  /*
    +++ THIS IS THE PARENT PROCESS +++
  */

  if ( pid == -1 )
    {
      
      /* reaching this point means that we couldn't initialize
	 the output filter/plugin because we didn't succeed in
	 forking. Pretty bad - we will bail out of the program
	 completely for now */
      
      fork_error();
      _exit(0);
    }
  
  /* we want to keep track of it's status */
  reference_child(pid);
  entry[i]->output_filter_pid = pid;
  
  /* remember the pipes.... */
  entry[i]->fd_into_filter = pipe_io[0][1];
  entry[i]->fd_from_filter = pipe_io[1][0];
  
  /* and check for the maximum number of bytes that may
     be written into it */
  entry[i]->pipe_buf = fpathconf( entry[i]->fd_into_filter,
				  _PC_PIPE_BUF );
  
  /* ...and set them to non-blocking I/O */
  fcntl(entry[i]->fd_into_filter, F_SETFL, O_NONBLOCK);
  fcntl(entry[i]->fd_from_filter, F_SETFL, O_NONBLOCK);
  
  /* close unused ones */
  close( pipe_io[0][0] );
  close( pipe_io[1][1] );
  
}



/*}}}*/

/*{{{  deactivate_plugin  */
/*
  This routine does a clean deactivation of the plugin part.

  The option is the number of the entry
  
*/
void
deactivate_plugin(gint i)
{

  /* if the pipe_magic_interrupt exists,
     make it unexist ! */
  if ( entry[i]->pmi_tag != 0 )
    {
      gtk_timeout_remove(entry[i]->pmi_tag);
      entry[i]->pmi_tag = 0;
    }

  /* if the process is still running, send a 
     SIGTERM... if it doesn't react it will
     receive a SIGKILL at one point */
  if ( entry[i]->output_filter_pid != 0 )
    kill( entry[i]->output_filter_pid, SIGTERM ); 
	  
  /* close all open pipes... */
  if ( entry[i]->fd_into_filter != 0 )
    {
      close(  entry[i]->fd_into_filter );
      entry[i]->fd_into_filter = 0;
    }
  
  if ( entry[i]->fd_from_filter != 0 )
    {
      close(  entry[i]->fd_from_filter );
      entry[i]->fd_from_filter = 0;
    }

  /* now delete the buffers that have been allocated */
  if ( entry[i]->input_buffer != NULL )
    {
      delete entry[i]->input_buffer;
      entry[i]->input_buffer = NULL;
    }
  if ( entry[i]->output_buffer != NULL )
    {
      delete entry[i]->output_buffer;
      entry[i]->output_buffer = NULL;
    }
  
  /* set the other variables accordingly */
  entry[i]->input_buffer_size =
    entry[i]->input_buffer_used =
    entry[i]->output_buffer_size =
    entry[i]->output_buffer_used = 0;
  
  /* reset the lock just in case */
  entry[i]->pipe_lock = FALSE;
  
}
/*}}}*/
