/*

  watchdog.cc

  background watching subroutines for xlogmaster.cc
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
#include "watchdog.H"
#include "extern.H"

/*}}}*/

using namespace std;


/*{{{  data retrieval subroutines  */
/* 
   This routine tries to close the file and open it again.
   The idea is to catch logfile rotations that would leave
   us with a useless filedescriptor otherwise.

   parameter:
   - entry number

   return value: 
   TRUE for success
   FALSE for problem
*/
gint 
reopen_logfile(gint i )
{
  close(entry[i]->fd);
  entry[i]->fd = open(entry[i]->filename, O_RDONLY);
 
  if ( entry[active]->fd > -1 )
    {
      // now we need to go to the beginning of the file:
      lseek(entry[i]->fd, 0, SEEK_SET);
      // everything clear... return
      return TRUE;
    }
  
  /* we got a file opening error */
  entry[i]->got = -1;
  entry[i]->fd = -1;

  return FALSE;
}
/*
  This function takes a filedescriptor as the argument
  and returns
  TRUE: if the reading position is within the file's
  boundaries
  FALSE: if the reading position is outside the file's 
  boundaries

  on problems the return value is FALSE
*/
gint
position_within_boundaries(gint i, gint fd)
{
  gint err = stat(entry[i]->filename, &stat_buffer);
    
  /*
    on error tell us what went wrong
  */
  if ( err == -1 )
    {
      switch ( errno )
	{
	case EBADF:
	  cerr << "(xlogmaster) position_within_boundaries: filedes is bad." << endl;
	  break;
	case ENOENT:
	  cerr << "(xlogmaster) position_within_boundaries: File does not exist." << endl;
	  break;
	case EFAULT:
	  cerr << "(xlogmaster) position_within_boundaries: Bad address." << endl;
	  break;
	case EACCES:
	  cerr << "(xlogmaster) position_within_boundaries: Permission denied." << endl;
	  break;
	case ENOMEM:
	  cerr << "(xlogmaster) position_within_boundaries: Out of memory (i.e. kernel memory)." << endl;
	  break;
	case ENAMETOOLONG:
	  cerr << "(xlogmaster) position_within_boundaries: File name too long." << endl;
	  break;
	}
      return FALSE;
    }

  /*
    retrieve position
  */
  off_t position = lseek( fd, 0, SEEK_CUR );

  /*
    check for error
  */
  if ( position == -1 )
    {
      cerr << "(xlogmaster) position_within_boundaries: lseek returned error... " << endl;
      return FALSE;
    }
  
  if ( stat_buffer.st_size < position )
    {
      return FALSE;
    }
  
  return TRUE;
}
/*}}}*/

/*{{{  execution subroutines  */
/*
  This routine starts the data gathering program for one
  entry (parameter !).

  If it is successful it only returns to the parent returning
  a "TRUE". If it failed there will not be a child and the
  parent will get a "FALSE" returned.

 */
gint
run_program(gint i )
{
  gint pipe_io[2][2];
  
  pipe( pipe_io[0] ); /* set up input pipe */
  pipe( pipe_io[1] ); /* set up output pipe */
  
  entry[i]->pid = fork();
  
  if ( entry[i]->pid == 0 )
    {
      
      /*
	+++ NOW WE ARE A CHILD PROCESS +++
      */
      
      /* redirect stdin(0), stdout (1) and stderr(2) */
      dup2( pipe_io[0][0], 0 );
      dup2( pipe_io[1][1], 1 );
      dup2( pipe_io[1][1], 2 );
      
      /* set up argv[] array... */
      gchar** argv = split_execute_line( entry[i]->filename );
      
      /* now it's time to start the program... */
      execvp(argv[0], argv);
      
      cerr << endl << endl << "\tEXECUTION ERROR ! " << endl << endl;
      cerr << "\tcould not run ";
      cerr <<   "'" << argv[0] << "'" << endl << endl;
      cerr << "\tplease check name, permissions and your" << endl;
      cerr << "\tPATH environment variable." << endl;
      
      /* bail out quick 'n dirty... :-) */
      _exit(0);
      
      /*
	+++ END OF CHILD PROCESS +++
      */
    }
  
  if ( entry[i]->pid == -1 )
    {
      /* an error occured */

      /* close pipes ... we don't need them */
      close( pipe_io[0][0] );
      close( pipe_io[0][1] );
      close( pipe_io[1][0] );
      close( pipe_io[1][1] );
      entry[i]->pid = 0;
      entry[i]->fd = -1;
      return FALSE;

    }
  else
    {
      /* everything was okay */
  
      reference_child( entry[i]->pid );
      
      /* close unused pipes */
      close( pipe_io[0][0] );
      close( pipe_io[1][1] );
      close( pipe_io[0][1] );
      /* memorize the pipe we read from */
      entry[i]->fd = pipe_io[1][0]; 
      /* and set it to non-blocking IO */
      fcntl(entry[i]->fd, F_SETFL, O_NONBLOCK); 
      
    }

  return TRUE;
}
/*}}}*/

/*{{{  Text subroutines....  */

/*
  this routine outputs text for the TAIL mode.

  the scrollbar keeps same relative position to the end
  and all output is being appended to the text window.

  For overflow at the end bytes are being freed at the 
  beginning of the buffer.

  Parameters:
  - buffer address with the text
  - length of the buffer
  
*/
void
output_tail_mode(gchar* buffer, glong amount)
{

  /* leave it alone if we don't have anything to append */
  if ( amount == 0 ) return;  

  /* Freeze for these actions: */
  gtk_text_freeze (GTK_TEXT(textwindow));
  
  gfloat old_value = ( GTK_TEXT(textwindow)->vadj->upper - 
		       GTK_TEXT(textwindow)->vadj->page_size -
		       GTK_TEXT(textwindow)->vadj->value );
  
  append_text(buffer, amount);
  
  /* And now unfreeze again */
  gtk_text_thaw(GTK_TEXT(textwindow));
  
  /* Now put scrollbar on same position relative to end */
  gtk_adjustment_set_value( GTK_TEXT(textwindow)->vadj,
			    GTK_TEXT(textwindow)->vadj->upper - 
			    GTK_TEXT(textwindow)->vadj->page_size -
			    old_value );
  
}

/*
  this routine outputs text for the RUN mode.
  
  the scrollbar is not set in any way because otherwise this mode
  would either jump around a lot (and be almost impossible to read)
  or crash when it tries to keep it's position relative to the
  beginning...
  
  Parameters:
  - buffer address with the text
  - length of the buffer
  - flag whether to append or replace the given
  buffer (is it a new process or the output of an old one ?)
  
*/
void
output_run_mode(gchar* buffer, glong amount, gint replace_flag)
{

  /* leave it alone if we don't have anything to replace it
     with ! */
  if ( amount == 0 ) return;

  if ( replace_flag == TRUE )
    {

      /* Freeze for these actions: */
      gtk_text_freeze (GTK_TEXT(textwindow));

      replace_text(buffer, amount);

      /* And now unfreeze again */
      gtk_text_thaw(GTK_TEXT(textwindow));

      gtk_adjustment_set_value( GTK_TEXT(textwindow)->vadj,
				GTK_TEXT(textwindow)->vadj->upper - 
				GTK_TEXT(textwindow)->vadj->page_size );
      
    }
  else
    {
      
      /* Freeze for these actions: */
      gtk_text_freeze (GTK_TEXT(textwindow));
      
      gfloat old_value = ( GTK_TEXT(textwindow)->vadj->upper - 
			   GTK_TEXT(textwindow)->vadj->page_size -
			   GTK_TEXT(textwindow)->vadj->value );
      
      
      append_text(buffer, amount);
      
      /* And now unfreeze again */
      gtk_text_thaw(GTK_TEXT(textwindow));
      
      /* Now put scrollbar on same position relative to end */
      gtk_adjustment_set_value( GTK_TEXT(textwindow)->vadj,
				GTK_TEXT(textwindow)->vadj->upper - 
				GTK_TEXT(textwindow)->vadj->page_size -
				old_value );
      
    }
  
}

/*
  this routine outputs text for the CAT mode.
  
  the scrollbar keeps the same position relative to the end
  and all the old text is being *replaced* with the
  new text.

  Parameters:
  - buffer address with the text
  - length of the buffer
*/
void
output_cat_mode(gchar* buffer, glong amount)
{

  /* leave it alone if we don't have anything to replace it
     with ! */  
  if ( amount == 0 ) return;
  
  /* Freeze for these actions: */
  gtk_text_freeze (GTK_TEXT(textwindow));
  
  gfloat old_value = ( GTK_TEXT(textwindow)->vadj->upper - 
		       GTK_TEXT(textwindow)->vadj->page_size -
		       GTK_TEXT(textwindow)->vadj->value );
  
  replace_text(buffer, amount);
  
  /* And now unfreeze again */
  gtk_text_thaw(GTK_TEXT(textwindow));
  
  /* Now put scrollbar on same position relative to end */
  gtk_adjustment_set_value( GTK_TEXT(textwindow)->vadj,
			    GTK_TEXT(textwindow)->vadj->upper - 
			    GTK_TEXT(textwindow)->vadj->page_size -
			    old_value );
  
}

/*
  this routine appends characters to the text display.

  The parameters are:
  - buffer address
  - amount of characters
  
*/
void 
append_text(gchar* buffer, glong amount)
{
  /*
    More than maxtext bytes ? Then delete some...
  */
  long text_length = gtk_text_get_length(GTK_TEXT(textwindow));
  
  if ( amount + text_length > maxtext )
    {
      
      gtk_text_set_point (GTK_TEXT(textwindow),0);

      gtk_text_forward_delete (GTK_TEXT(textwindow), 
			       (text_length+amount) - maxtext );
 
    }
  
  gtk_text_set_point (GTK_TEXT(textwindow),
		      gtk_text_get_length(GTK_TEXT(textwindow)));
  
  /*
    Now append the data...
  */  
  output_text(textwindow, buffer, amount);
  
}

/*
  this routine replaces the text in the display
  by the text in the given buffer.
  
  Parameters:
  - buffer address
  - amount of characters
  
  if you give a NULL buffer or zero bytes of
  appendable text, the text display will only
  be cleared.
*/
void 
replace_text(gchar* buffer, glong amount)
{

  /* Clear window: */
  gtk_text_set_point ( GTK_TEXT(textwindow), 0 );

  guint text_length = gtk_text_get_length(GTK_TEXT(textwindow));
  if ( text_length > 0 )
    gtk_text_forward_delete (GTK_TEXT(textwindow), text_length);
  
  /* now output the text */
  if ( amount > 0 && buffer != NULL )
    output_text( textwindow, buffer, amount );

}
/*}}}*/

/*{{{  read_from_filedescriptor (main PLUGIN routine)  */
/*

  This function will read data from a filedescriptor.

  In case of a plugin it will do the "pipe magic" thing
  that swaps the input against the output from the
  pipe.

  Return value is the amount of bytes written

*/
glong
read_from_filedescriptor(gint i, off_t amount)
{
  glong got;
  
  /* determine how much we are going to read */
  if ( amount < 0 )
    {
      if ( entry[i]->filterclass & CLASS1_FILTER )
	amount = entry[i]->chunk;
      else
	amount = maxtext;
    }


  if ( entry[i]->filterclass & CLASS1_FILTER )
    {
      
      got = read( entry[i]->fd,
		  entry[i]->rbuffer, 
		  amount );  
      
      /* if we have a plugin we need to do the pipe magic ! */
      if ( got >= 0 &&
	   entry[i]->plugin != NULL )
	{

	  got = pipe_magic( i , 
			    entry[i]->rbuffer , 
			    got ,
			    amount );

	}

    }
  else
    {
      
      got = read( entry[i]->fd,
		  read_buffer, 
		  amount );
    
      /* if we have a plugin we need to do the pipe magic ! */
      if ( got >= 0 &&
	   entry[i]->plugin != NULL )
	{
	  
	  got = pipe_magic( i , 
			    read_buffer , 
			    got ,
			    amount );
	  
	}
      
    }
  
  return got;
  
}
/*}}}*/

/*
  
  Background Logfile handling 

*/

/*{{{  Watchdog (background log watching) on/off  */
/*
  Startup routine for background log watching
  neeeds some more niceness and should probably
  be split up eventually...
*/
void 
start_watchdog()
{
  gint i;
  for ( i = 0 ; i < syslogs ; i++ )
    {
     
      /* this routine is smart enough to be called here */
      entry[i]->allocate_buffers();

      if ( entry[i]->filterclass & CLASS1_FILTER )
	{
	 	  
	  entry[i]->fd = open(entry[i]->filename, O_RDONLY);
	  
	  if ( entry[i]->mode == TAIL_FILE )
	    {
	      /*
		SETUP for TAIL mode
	      */
	      
	      fstat( entry[i]->fd,  &status );
	    
	      /*
		We need to check whether the file is shorter than maxtext
		bytes because some systems (like FreeBSD) allow a negative
		index for files...
	      */
	      if ( maxtext < status.st_size )
		{

		  /* 
		     if the file is bigger than maxtext we should
		     read maxtext bytes 
		  */
		  lseek(entry[i]->fd, -maxtext, SEEK_END);
		  entry[i]->length = read(entry[i]->fd, 
					  entry[i]->buffer, 
					  maxtext);

		  /* if we have a plugin we need to do the pipe magic ! */
		  if ( entry[i]->length >= 0 &&
		       entry[i]->plugin != NULL )
		    {
		      
		      entry[i]->length = pipe_magic( i , 
						     entry[i]->buffer , 
						     entry[i]->length ,
						     maxtext );
		      
		      entry[i]->up_to_date = FALSE;

		    }
		  
		}
	      else
		{

		  /*
		    otherwise read everything there is
		  */
		  lseek(entry[i]->fd, 0, SEEK_SET);
		  entry[i]->length = read(entry[i]->fd, 
					  entry[i]->buffer, 
					  status.st_size );
		  
		  /* if we have a plugin we need to do the pipe magic ! */
		  if ( entry[i]->length >= 0 &&
		       entry[i]->plugin != NULL )
		    {
		      
		      entry[i]->length = pipe_magic( i , 
						     entry[i]->buffer , 
						     entry[i]->length ,
						     maxtext );
		      
		      entry[i]->up_to_date = FALSE;
		      
		    }
		  
		}
	      
	      /*
		everything is prepared, now we start
		the interrupt function
	      */
	      entry[i]->tag = gtk_timeout_add (entry[i]->interval*100,
						  *watchdog_tail_interrupt,
						  (gpointer*) i);
	      
	      entry[i]->active = TRUE;
	    
	    }
	  else if ( entry[i]->mode == RUN_FILE )
	    {
	      /*
		SETUP for RUN mode
	      */

	      if ( entry[i]->pid != 0 &&
		   check_terminate(entry[i]->pid) == FALSE )
		{
		  /* 
		     there is still an old process hanging around... this is not
		     good - so we'll send it a kill -9 to tell it to go away.
		  */
		  kill( entry[i]->pid, SIGKILL );
		}
	      
	      /*
		everything is prepared, now we start
		the interrupt function
	      */
	      entry[i]->tag = gtk_timeout_add (entry[i]->interval*100,
						  *watchdog_run_interrupt,
						  (gpointer*) i);
	      
	      /* now we restart the program: */
	      if ( run_program(i) == FALSE )
		{
		  watchdog_run_error(i);
		  return;
		}

	      entry[i]->active = TRUE;
	      
	    }
	  else if ( entry[i]->mode == CAT_FILE )
	    {
	      /*
		SETUP for CAT mode
	      */	      
	      
	      entry[i]->length = read(entry[i]->fd, 
				      entry[i]->rbuffer, 
				      maxtext);
	      
	      close(entry[i]->fd);

	      /* if we have a plugin we need to do the pipe magic ! */
	      if ( entry[i]->length >= 0 &&
		   entry[i]->plugin != NULL )
		{
		  
		  entry[i]->length = pipe_magic( i , 
						 entry[i]->rbuffer , 
						 entry[i]->length ,
						 maxtext );
		  
		}
	      
	      /*
		everything is prepared, now we start
		the interrupt function
	      */
	      entry[i]->tag = gtk_timeout_add (entry[i]->interval*100,
						  *watchdog_cat_interrupt,
						  (gpointer*) i);
	      
	      entry[i]->active = TRUE;
	      
	    }
	  else
	    {
	      cerr << PACKAGE << ": illegal mode in entry" << i << endl;
	      cerr << "  while starting watchdog" << endl;
	      _exit(0);
	    }
	  	  
	}
      else
	{

	  entry[i]->active = FALSE;
	  
	}
      
    }
}
/*
  This routine terminates all background log watching
*/
void 
stop_watchdog()
{
  gint i;
  for ( i = 0 ; i < syslogs ; i++ )
    if ( entry[i]->filterclass & CLASS1_FILTER )
      {
	entry[i]->active = FALSE;

	/* remove interrupt */
	gtk_timeout_remove(entry[i]->tag);
	entry[i]->tag = 0;

	/* close open file descriptors */
	if ( entry[i]->mode == TAIL_FILE || entry[i]->mode == RUN_FILE )
	  close(entry[i]->fd);
	
	/* 
	   if it is a run mode element, we tell the process to
	   terminate if it hasn't done so already
	*/
	if ( entry[i]->mode == RUN_FILE &&
	     entry[i]->pid != 0 &&
	     check_terminate(entry[i]->pid) == FALSE ) 
	  kill( entry[i]->pid, SIGTERM );
	
	/* does it have a plugin ? Make sure it's dead !! */
	if ( entry[i]->plugin != NULL )
	  deactivate_plugin( i );
	
	/* free buffers again */
	entry[i]->free_buffers();
      }
}
/*}}}*/

/*{{{  watchdog tail interrupt  */
/*
  This routine does the background monitoring for
  TAIL mode entries
*/
gint 
watchdog_tail_interrupt(gpointer data)
{
  gint i = (gint) data;
  if ( entry[i]->active == FALSE ) return TRUE;
  
  if ( entry[i]->fd == -1 )
    {
      watchdog_file_error(i);
      return TRUE;
    }

  long got = read_from_filedescriptor(i);
  
  /*
    If the file produced an error we try to reopen it
    
    should the reopen fail the fd will be set to -1 which
    will result in a file error next time...
  */
  if ( got == -1 && errno != EAGAIN )
    {
      /*
	evaluate the errno and take according actions...
      */
      switch ( errno )
	{
	case EINTR:
	  /*
	    we just got interrupted by some signal... whatever
	    that may have been. We'll just retry the next time.
	  */
	  cerr << 
	    "(xlogmaster) tail interrupt: INTERRUPTED BY SIGNAL" 
	       << endl;
	  break;
	case EIO:
	  /*
	    We have an I/O error. This might be due to disk
	    problems or something else that happened. Let's
	    just try to continue for now.
	  */
	  cerr << 
	    "(xlogmaster) tail interrupt: I/O ERROR" 
	       << endl;
	  break;
	case EBADF:
	  /*
	    The FD has gone bad for some reason... since we
	    have no clue what happened trying to repoen it
	    appears to be the action of choice.
	  */
	  reopen_logfile(i);
	  cerr << 
	    "(xlogmaster) tail interrupt: I/O ERROR" 
	       << endl;

	  break;
	case EISDIR:
	  cerr << "tail file error: FD is directory" << endl;
	  break;
	case EINVAL:
	  cerr << "tail file error: object unsuitable for reading" << endl;
	  break;
	case EFAULT:
	  cerr << "tail file error: buffer outside address space" << endl;
	  break;
	}
  
      return TRUE;
    }
  
  /*
    check for logfile turnover
  */
  if ( position_within_boundaries( i, entry[i]->fd ) == FALSE )
    {
      cerr << "Xlogmaster: detected logfile turnover for ";
      cerr << entry[i]->buttontext << " !" << endl;
      
      reopen_logfile(i);
      return TRUE;
    }


  while ( got > 0 )
    {
      long overlap;
      
      /*
	handling of data in memory... updating buffers and pointers
      */
      overlap= ( got + entry[i]->length ) - maxtext;
      if ( overlap < 0 ) overlap = 0;
      if ( overlap > 0 )
	{
	  for ( long y = overlap ; y < entry[i]->length ; y++ )
	    {
	      entry[i]->buffer[ y - overlap] = entry[i]->buffer[ y ];
	    }
	}
      
      long target = entry[i]->length - overlap;
      for ( long z = 0 ; z < got ; z++ )
	{
	  entry[i]->buffer[ z + target ] = entry[i]->rbuffer[z];
	}   
      entry[i]->length += got;
      if ( entry[i]->length > maxtext ) entry[i]->length = maxtext;
      
      /*
	check for Class1 filters if available
      */ 
      if ( ( entry[i]->filter != NULL ) &&
	   ( entry[i]->filterclass & CLASS1_FILTER ) &&
	   ( entry[i]->up_to_date == TRUE ) )
	{
	  process_class1_filters(i, got);
	}
      
      /*
	Is the log active ? Then display it ...
      */
      if ( i == active )
	{
	  output_tail_mode( entry[i]->rbuffer, got);
	}
      
      got = read_from_filedescriptor(i);
    }

  return TRUE;
}
/*}}}*/

/*{{{  watchdog run interrupt  */
/*
  This routine does the background monitoring for
  RUN mode entries
*/
gint 
watchdog_run_interrupt(gpointer data)
{
  int i = (int) data;
  if ( entry[i]->active == FALSE ) return TRUE;

  if ( entry[i]->fd == -1 )
    {
      watchdog_file_error(i);
      return TRUE;
    }

  if ( entry[i]->replace == TRUE )
    {
      entry[i]->length = 0;
    }

  long got = read_from_filedescriptor(i);

  if ( got > 0 )
    {
      long overlap;
  
      /*
	handling of data in memory... updating buffers and pointers
      */
      overlap= ( got + entry[i]->length ) - maxtext;
      if ( overlap < 0 ) overlap = 0;
      if ( overlap > 0 )
	{
	  for ( long y = overlap ; y < entry[i]->length ; y++ )
	    {
	      entry[i]->buffer[ y - overlap] = entry[i]->buffer[ y ];
	    }
	}
      
      long target = entry[i]->length - overlap;
      for ( long z = 0 ; z < got ; z++ )
	{
	  entry[i]->buffer[ z + target ] = entry[i]->rbuffer[z];
	}   
      entry[i]->length += got;
      if ( entry[i]->length > maxtext ) entry[i]->length = maxtext;  
      
      /*
	check for Class1 filters if available
      */ 
      if ( ( entry[i]->filter != NULL ) &&
	   ( entry[i]->filterclass & CLASS1_FILTER ))
	{
	  process_class1_filters(i, got);
	}
      
      /*
	Is the log active ? Then display it ...
      */
      if ( i == active )
	{
	  output_run_mode( entry[i]->rbuffer, got, entry[i]->replace );
	}
      
      entry[i]->replace = FALSE;

      return TRUE;
    }
  
  /*
    The run mode is more complicated, if there was no
    data to be read we need to check for termination & 
    possibly restart...
  */
  
  /* did the program terminate ? */
  if (  check_terminate(entry[i]->pid) == TRUE )
    {
      
      close( entry[i]->fd );
      entry[i]->fd = 0;
      entry[i]->pid = 0;
      
      /*
	do we want to restart immediately or do we have a delay ?
      */    
      if ( entry[i]->delay > 0 )
	{
	  /* we have a delay */
	  /* remove old interrupt */
	  gtk_timeout_remove(entry[i]->tag);
	  
	  /* and set restarting one */
	  entry[i]->tag = gtk_timeout_add (entry[i]->delay*100,
					   *watchdog_run_restart_interrupt,
					   (gpointer*) i);
	  
	  return TRUE;
	}
      
      /* we restart immediately */  
      if ( run_program(i) == FALSE )
	{
	  watchdog_run_error(i);
	  return FALSE;
	}
      
      /* next time we need to replace the contents */
      entry[i]->replace = TRUE;
      
    }

  return TRUE;
}
gint
watchdog_run_restart_interrupt(gpointer data)
{ 
  gint i = (gint) data;
  
  /* first remove this interrupt */
  gtk_timeout_remove(entry[i]->tag);
  
  /* then set other interrupt to read the output again */
  entry[i]->tag = gtk_timeout_add (entry[i]->interval*100,
				      *watchdog_run_interrupt,
				      (gpointer*) i);
  
  /* now we restart the program: */
  if ( run_program(i) == FALSE )
    {
      watchdog_run_error(i);
      return FALSE;
    }
  
  /* next time we need to replace it again */
  entry[i]->replace = TRUE;
  
  return TRUE;
}
/*}}}*/

/*{{{  watchdog cat interrupt  */
/*
  This routine does the background monitoring for
  CAT mode entries
*/
gint 
watchdog_cat_interrupt(gpointer data)
{
  int i = (int) data;
  if ( entry[i]->active == FALSE ) return TRUE;
  
  /* we need to open it every time anew for CAT */
  entry[i]->fd = open(entry[i]->filename, O_RDONLY);    
  
  if ( entry[i]->fd == -1 )
    {
      watchdog_file_error(i);
      return TRUE;
    }
  
  entry[i]->length = read_from_filedescriptor(i);
  
  close( entry[i]->fd );
  
  /*
    check for Class1 filters if available
  */ 
  if ( ( entry[i]->filter != NULL ) &&
       ( entry[i]->filterclass & CLASS1_FILTER ))
    {
      process_class1_filters( i, entry[i]->length );
    }
  
  /*
    Is the log active ? Then display it ...
  */
  if ( i == active )
    {
      output_cat_mode( entry[i]->rbuffer,  entry[i]->length);
    }
  
  return TRUE;
}
/*}}}*/

/*{{{  Class 1 filter subroutine  */
/*
  This routine checks for Class 1 filters that need to be triggered
  and takes the according action (if neccessary).
 
  The parameters are:
  - entry number
  - amount of bytes that have been read into the entry[i]->rbuffer
  
  there is no return value.
*/
void
process_class1_filters(gint i, glong got)
{
  glong start = 0;
  glong stop = 0;
    
  while ( start < got )
    {
	
      stop = match_all( entry[i]->filter, 
			CLASS1_FILTER, 
			entry[i]->rbuffer, 
			start, 
			got);
	
      gint action = 0;
      gint x = 0;
      while ( entry[i]->filter[x] != NULL )
	{
	    
	  if ( entry[i]->filter[x]->regex_status == 0 )
	    {
	      action |= entry[i]->filter[x]->mode;
	    }
	  
	  x++;
	}
      
      /*
	mask out things that are not CLASS1_FILTERS,
	afterwards actions has the bit for every applicable
	CLASS1_FILTER set.
      */
      action &= CLASS1_MASK;
      
      /*
	if action is not 0 we need to do something about
	that line, so here is our routine for that:
      */
      
      if ( action != 0 )
	{
	  gchar* line = NULL;
	  
	  /*
	    These filters need the line that triggered the match
	  */
	  if ( action & NOTICE ||
	       action & EXECUTE )
	    {
	      line = new gchar[(stop-start)+5];
	      
	      gint a = 0;
	      gint b = start;
	      while ( b < stop )
		{
		  
		  if ( entry[i]->rbuffer[b] != '\r' &&
		       entry[i]->rbuffer[b] != '\n' )
		    {
		      line[a++] = entry[i]->rbuffer[b++];
		    }
		  else
		    {
		      b++;
		    }
		  
		}
	      line[a] = 0;
	      
	    }
	  
	  if ( action & NOTICE )
	    {
	      notice_alert(entry[i]->buttontext , line);
	      play(NOTICE);
	    }
	  
	  if ( action & ALERT )
	    {
	      trigger_alert(i);
	      play(ALERT);
	    }

	  if ( action & UNICONIFY )
	    {
	      /* This maps the window, which also de-iconifies it according to ICCCM. */
	      gdk_window_show (GTK_WIDGET (window)->window);
	      gdk_window_raise (GTK_WIDGET (window)->window);
	      play(UNICONIFY);
	    }

	  if ( action & EXECUTE )
	    {
	      x = 0;
	      while (  entry[i]->filter[x] != NULL )
		{
		  
		  if ( entry[i]->filter[x]->regex_status >= 0 
		       && entry[i]->filter[x]->mode & EXECUTE )
		    {
		      execute_program(i, x , line);
		    }
		  
		  x++;
		}
	    }
	  
	  /* 
	     now the line can be freed again
	  */
	  if ( line != NULL ) delete line;
	}
      
      start = stop + 1;
    }
  
}
/*}}}*/

/*{{{  error subroutines for watchdog  */
/*
  This routine handles file access errors during watchdog startup
  and interrupts.

  The parameter is the number of the entry that caused the problems
 
  - If the number equals te active logfile, deactivate is called
  - the watchdog interrupt for this entry is being removed
  - the buffers are freed
  and a file error dialog box is being popped up
*/
void 
watchdog_file_error(gint i)
{
  entry[i]->got = -1;      
  if ( i == active ) deactivate();
  if ( entry[i]->tag != 0 ) gtk_timeout_remove(entry[i]->tag);
  entry[i]->tag = 0;
  if ( entry[i]->buffer != NULL ) delete entry[i]->buffer;
  entry[i]->buffer = NULL;
  if ( entry[i]->rbuffer != NULL ) delete entry[i]->rbuffer;
  entry[i]->rbuffer = NULL;
  entry[i]->length = 0;
  entry[i]->active = FALSE;

  /*
    Hide the button for the entry that failed...
  */
  gtk_widget_hide( entry[i]->button );

  file_error(entry[i]->filename);
}
/*
  This routine is basically identical with the watchdog_file_error
  routine, it is only for the "RUN" mode and will create a different
  popup.
*/ 
void 
watchdog_run_error(gint i)
{
  entry[i]->got = -1;      
  if ( i == active ) deactivate();
  if ( entry[i]->tag != 0 ) gtk_timeout_remove(entry[i]->tag);
  entry[i]->tag = 0;
  if ( entry[i]->buffer != NULL ) delete entry[i]->buffer;
  entry[i]->buffer = NULL;
  if ( entry[i]->rbuffer != NULL ) delete entry[i]->rbuffer;
  entry[i]->rbuffer = NULL;
  entry[i]->length = 0;
  entry[i]->active = FALSE;

  /*
    Hide the button for the entry that failed...
  */
  gtk_widget_hide( entry[i]->button );
  
  execute_error(entry[i]->filename);
}
/*}}}*/


/*
  
  Logfile handling
  
*/

/*{{{  activate()  */
void 
activate()
{
  if ( display_logs == DISABLED ) return;
  /* speedup fadedown for activated logfile */
  entry[active]->fadestep = -1 * ( fade_base / 3 );
  
  /*
    Set title to button text:
  */
  gtk_window_set_title (GTK_WINDOW (window), entry[active]->buttontext );  
  
  /*
    If entry is not in alert mode right now, color it's
    button in PRELIGHT color !
  */  
  entry[active]->style = gtk_style_copy(stdstyle);
  if ( entry[active]->alert_tag == 0 )
    {
      entry[active]->style->bg[GTK_STATE_NORMAL].red = prelight.red;
      entry[active]->style->bg[GTK_STATE_NORMAL].green = prelight.green;
      entry[active]->style->bg[GTK_STATE_NORMAL].blue = prelight.blue;
    }  
  gtk_widget_set_style(entry[active]->button, entry[active]->style);
  gtk_style_unref(entry[active]->style);
  

  /*{{{  Activate entry with Class 1 fiters 
    (monitoring is done in the bacground !) */
  
  if ( entry[active]->filterclass & CLASS1_FILTER )
    {
      
      if ( entry[active]->active == FALSE )
	{
	  file_error( entry[active]->filename );
	  return;
	}

      /* Freeze for output: */
      gtk_text_freeze ( GTK_TEXT(textwindow) );

      /*
	and now replace the text in the display
      */
      if ( entry[active]->mode == TAIL_FILE ||
	   entry[active]->mode == RUN_FILE )
	{
	  output_text( textwindow, entry[active]->buffer, 
		       entry[active]->length);
	}
      else if ( entry[active]->mode == CAT_FILE )
	{
	  /*
	    the data for CAT always stays in the read buffer...
	  */
	  output_text( textwindow, entry[active]->rbuffer, 
		       entry[active]->length);
	}
      
      /* And now unfreeze again */
      gtk_text_thaw(GTK_TEXT(textwindow));
      
      gtk_adjustment_set_value( GTK_TEXT(textwindow)->vadj,
				GTK_TEXT(textwindow)->vadj->upper - 
				GTK_TEXT(textwindow)->vadj->page_size );
      
      /*
	now we are done....
      */
      return;
    }
  
  /*
    There is no background log watching for this entry, so we need to start
    reading...
  */

  if ( entry[active]->mode == TAIL_FILE )
    {

/*{{{  Startup of "tail" mode  */
      entry[active]->fd = open(entry[active]->filename, O_RDONLY);
      if ( entry[active]->fd == -1 )
	{
	  file_error(entry[active]->filename);
	  deactivate();

	  /*
	    Hide the button for the entry that failed...
	  */
	  gtk_widget_hide( entry[active]->button );
	  
	  return;
	}
      
      glong got;
      fstat( entry[active]->fd,  &status );
      /*
	We need to check whether the file is shorter than maxtext
	bytes because some systems (like FreeBSD) allow a negative
	index for files...
      */

      if ( maxtext < status.st_size )
	{
	  /* if the file is bigger than maxtext we should
	     read maxtext bytes 
	  */
	  lseek(entry[active]->fd, -maxtext, SEEK_END);
	  got = read_from_filedescriptor(active, maxtext);
	}
      else
	{
	  /*
	    otherwise read everything there is
	  */
	  lseek(entry[active]->fd, 0, SEEK_SET);
	  got = read_from_filedescriptor(active, status.st_size);
	}

      if ( got > 0 )
	{
	  
	  /* Freeze for output: */
	  gtk_text_freeze (GTK_TEXT(textwindow));
	  
	  output_text(textwindow, read_buffer, got);
	  
	  /* And now unfreeze again */
	  gtk_text_thaw(GTK_TEXT(textwindow));

	  gtk_adjustment_set_value(GTK_TEXT(textwindow)->vadj,
				   GTK_TEXT(textwindow)->vadj->upper - 
				   GTK_TEXT(textwindow)->vadj->page_size);

	}
      else
	{
	  gtk_adjustment_set_value(GTK_TEXT(textwindow)->vadj, 0);
	}
      
      entry[active]->tag = gtk_timeout_add (entry[active]->interval*100,
					    *tail_interrupt,
					    (gpointer*) active);
      
      entry[active]->active = TRUE;
      
/*}}}*/

    } 
  else if ( entry[active]->mode == CAT_FILE )
    {

/*{{{  Startup of "cat" mode  */
      entry[active]->fd = open(entry[active]->filename, O_RDONLY);
      if ( entry[active]->fd == -1 )
	{
	  file_error(entry[active]->filename);
	  deactivate();

	  /*
	    Hide the button for the entry that failed...
	  */
	  gtk_widget_hide( entry[active]->button );
	  
	  return;
	}
      
      long got = read_from_filedescriptor(active);
      
      close(entry[active]->fd);
      
      /* Freeze for output: */
      gtk_text_freeze (GTK_TEXT(textwindow));
      
      output_text(textwindow, read_buffer, got);
      
      /* And now unfreeze again */
      gtk_text_thaw(GTK_TEXT(textwindow));
      
      gtk_adjustment_set_value(GTK_TEXT(textwindow)->vadj,
			       GTK_TEXT(textwindow)->vadj->lower);
      
      entry[active]->tag = gtk_timeout_add (entry[active]->interval*100,
					    *cat_interrupt,
					    (gpointer*) active);
      
      entry[active]->active = TRUE;  
 
/*}}}*/

    }
  else if ( entry[active]->mode == RUN_FILE )
    {
            
/*{{{  Startup of "run" mode  */
      
      if ( entry[active]->pid != 0 &&
	   check_terminate(entry[active]->pid) == FALSE )
	{
	  /* 
	     there is still an old process hanging around... this is not
	     good - so we'll send it a kill -9 to tell it to DIE, DIE, DIE !!!! :-) 
	  */
	  kill( entry[active]->pid, SIGKILL );
	}
      
      if ( run_program(active) == FALSE )
	{
	  entry[active]->active = FALSE;

	  /*
	    Hide the button for the entry that failed...
	  */
	  gtk_widget_hide( entry[active]->button );
	  
	  return;
	}
      
      entry[active]->tag = gtk_timeout_add (entry[active]->interval*100,
					    *run_interrupt,
					    (gpointer*) active);
      
      entry[active]->replace = TRUE;
      
      entry[active]->active = TRUE;
      
/*}}}*/

    }

  return;
}
/*}}}*/

/*{{{  deactivate()  */
void 
deactivate()
{
  if ( ! ( entry[active]->filterclass & CLASS1_FILTER ) )
    {
      
      /*
	We need to disable interrupt-routines...
      */
      entry[active]->active = FALSE;
      
      if ( entry[active]->tag != 0 )
	{
	  gtk_timeout_remove(entry[active]->tag);
	  entry[active]->tag=0;
	}
      
      if ( entry[active]->mode == TAIL_FILE || 
	   entry[active]->mode == RUN_FILE )
	{
	  close( entry[active]->fd );
	  entry[active]->fd = 0;
	}

      if ( entry[active]->mode == RUN_FILE &&
	   entry[active]->pid != 0 &&
	   check_terminate(entry[active]->pid) == FALSE )
	{	  
	  kill( entry[active]->pid, SIGTERM ); 
	}

      /* if there is a plugin: make sure it's gone ! */
      if ( entry[active]->plugin != NULL )
	deactivate_plugin( active );
      
    }

  /*
    We always want to clear the textdisplay:
  */
  gtk_text_freeze (GTK_TEXT(textwindow));
  gtk_text_set_point (GTK_TEXT(textwindow),0);
  gtk_text_forward_delete (GTK_TEXT(textwindow),
			   gtk_text_get_length(GTK_TEXT(textwindow)));
  gtk_text_thaw(GTK_TEXT(textwindow));
  gtk_adjustment_set_value(GTK_TEXT(textwindow)->vadj, 0);
  
  
  /*
    If entry is not in alert mode right now, color it's
    button in NORMAL color !
  */  
  if ( entry[active]->alert_tag == 0 )
    {
      entry[active]->style = gtk_style_copy(stdstyle);
      gtk_widget_set_style(entry[active]->button, entry[active]->style);
      gtk_style_unref(entry[active]->style);
    }
}
/*}}}*/


/* 
   
   Foreground Logfile handling
   
*/

/*{{{  tail_interrupt  */
gint 
tail_interrupt(gpointer data)
{ 
  gint i = (gint) data;
  if ( entry[i]->active == FALSE ) return TRUE;
  
  /*
    If the entry has an defective file descriptor, give
    out an error message and deactivate it !
  */
  if ( entry[i]->fd == -1 )
    {

      deactivate();

      /*
	Hide the button for the entry that failed...
      */
      gtk_widget_hide( entry[i]->button );
      
      file_error(entry[i]->filename);
      return FALSE;

    }

  glong got = read_from_filedescriptor(i);
  
  /*
    If the file produced an error we try to reopen it
    
    should the reopen fail the fd will be set to -1 which
    will result in a file error next time...
  */
  if ( got == -1 && errno != EAGAIN )
    {
      
      /*
	evaluate the errno and take according actions...
      */
      switch ( errno )
	{
	case EINTR:
	  /*
	    we just got interrupted by some signal... whatever
	    that may have been. We'll just retry the next time.
	  */
	  cerr << 
	    "(xlogmaster) tail interrupt: INTERRUPTED BY SIGNAL" 
	       << endl;
	  break;
	case EIO:
	  /*
	    We have an I/O error. This might be due to disk
	    problems or something else that happened. Let's
	    just try to continue for now.
	  */
	  cerr << 
	    "(xlogmaster) tail interrupt: I/O ERROR" 
	       << endl;
	  break;
	case EBADF:
	  /*
	    The FD has gone bad for some reason... since we
	    have no clue what happened trying to repoen it
	    appears to be the action of choice.
	  */
	  reopen_logfile(i);
	  cerr << 
	    "(xlogmaster) tail interrupt: I/O ERROR" 
	       << endl;
	  
	  break;
	case EISDIR:
	  cerr << "tail file error: FD is directory" << endl;
	  break;
	case EINVAL:
	  cerr << "tail file error: object unsuitable for reading" << endl;
	  break;
	case EFAULT:
	  cerr << "tail file error: buffer outside address space" << endl;
	  break;
	}
      
      return TRUE;
    }
  
  /*
    check for logfile turnover
  */
  if ( position_within_boundaries( i, entry[i]->fd ) == FALSE )
    {
      cerr << "Xlogmaster: detected logfile turnover for ";
      cerr << entry[i]->buttontext << " !" << endl;
      
      reopen_logfile(i);
      return TRUE;
    }

  while ( got > 0 )
    {
      
      output_tail_mode(read_buffer, got);
      
      got = read_from_filedescriptor(i);
      
    }
  
  return TRUE;
}
/*}}}*/

/*{{{  cat_interrupt  */
gint 
cat_interrupt(gpointer data)
{
  gint i = (gint) data;
  if ( entry[i]->active == FALSE ) return TRUE;
  
  entry[i]->fd = open(entry[i]->filename, O_RDONLY);
  if ( entry[i]->fd == -1 )
    {
      deactivate();

      /*
	Hide the button for the entry that failed...
      */
      gtk_widget_hide( entry[i]->button );

      file_error(entry[i]->filename);
      return FALSE;
    }
  long got = read_from_filedescriptor(i);
  close( entry[i]->fd );
  
  output_cat_mode(read_buffer, got);

  return TRUE;
}
/*}}}*/

/*{{{  run_interrupt  */
gint 
run_interrupt(gpointer data)
{ 
  gint i = (gint) data;
  if ( entry[i]->active == FALSE ) return TRUE;

  long got = read_from_filedescriptor(i);
  
  if ( got > 0 )
    {
      
      output_run_mode(read_buffer, got, entry[i]->replace);
      
      entry[i]->replace = FALSE;

      return TRUE;
    }
  
  if (  check_terminate(entry[i]->pid) == FALSE )
    {
      return TRUE;
    }
  
  /*
    do we want to restart immediately or do we have a delay ?
  */
  
  if ( entry[i]->delay > 0 )
    {

      /* we need to wait. first remove this interrupt */
      gtk_timeout_remove(entry[i]->tag);
      
      /* then set restarting interrupt */
      entry[i]->tag = gtk_timeout_add (entry[i]->delay*100,
				       *run_restart_interrupt,
				       (gpointer*) i);
      
      /* and return */
      return TRUE;
    }

  /*
    the process has died... we need to close the pipe
    and set the pid to zero.
  */
  close( entry[i]->fd );
  entry[i]->fd = 0;
  entry[i]->pid = 0;
  
  /* we restart directly: */
  if ( run_program(i) == FALSE )
    {
      deactivate();
      return FALSE;
    }
  
  /* next time we need to replace again */
  entry[i]->replace = TRUE;
  
  return TRUE;
}
gint 
run_restart_interrupt(gpointer data)
{ 
  gint i = (gint) data;
 
  /* first remove this interrupt */
  gtk_timeout_remove(entry[i]->tag);

  /* check whether there is still more coming in that
     we should display before restarting */
  long got = read_from_filedescriptor(i);
  if ( got > 0 )
    {
      output_run_mode(read_buffer, got, entry[i]->replace);
    }
  
  /*
    the process has died long ago... we need to close the pipe
    and set the pid to zero.
  */
  close( entry[i]->fd );
  entry[i]->fd = 0;
  entry[i]->pid = 0;
  
  /* then set other interrupt to read the output again */
  entry[i]->tag = gtk_timeout_add (entry[i]->interval*100,
				   *run_interrupt,
				   (gpointer*) i);
  
  /* now we restart the program: */
  if ( run_program(i) == FALSE )
    {
      deactivate();
      
      /*
	Hide the button for the entry that failed...
      */
      gtk_widget_hide( entry[i]->button );
      
      return FALSE;
    }
  
  /* next time we need to replace it again */
  entry[i]->replace = TRUE;
  
  return TRUE;
}
/*}}}*/

