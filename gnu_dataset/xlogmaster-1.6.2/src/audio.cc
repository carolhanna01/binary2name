/*

  audio.cc

  audio subroutines for xlogmaster.cc
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
#include "audio.H"
#include "extern.H"

/*}}}*/

/*{{{  play()  */
void 
play(gint which)
{
  if ( sound == FALSE ) 
    return;
  
  /* determine what to play: */
  gchar* audiofile = NULL;
  switch ( which )
    {
    case ALERT:
      audiofile = alert_sound;
      break;
    case NOTICE:
      audiofile = notice_sound;
      break;
    case UNICONIFY:
      audiofile = uniconify_sound;
      break;
    }
  
  /* return if we don't need to play anything */
  if ( audiofile == NULL ) 
    return;

  /* now fork */
  pid_t pid = fork();

  if ( pid > 0 )
    {
      /* PARENT, forking o.k. */
      
      reference_child(-1); /* we don't care about the end of this one as
			      long as it does get terminated... */
      return;
    } 
  else if ( pid == -1 )
    {
      /* PARENT, forking failed */

      fork_error();
      return;
    }

  /*
    +++ NOW WE ARE A CHILD PROCESS +++
  */

  char* buffer = new char[AUDIO_BSIZE];
  int fd = open(audiofile, O_RDONLY);
  int out = open(sound_device, O_WRONLY );
  
  if ( fd == -1 || out == -1 )
    {
      close(fd);
      close(out);
      delete buffer;
      _exit(0);
    }
 
  long got = read( fd, buffer, AUDIO_BSIZE);

  while ( got > 0 )
    {
      write(out, buffer, got);
      got = read( fd, buffer, AUDIO_BSIZE);
    }

  close(fd);
  close(out);

  delete buffer;
  _exit(0);
}
/*}}}*/
