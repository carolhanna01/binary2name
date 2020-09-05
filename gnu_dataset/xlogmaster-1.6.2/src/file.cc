/*

  file.cc

  file access subroutines for xlogmaster.cc
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
#include "file.H"
#include "extern.H"

/*}}}*/

using namespace std; 	/* jsg - to fix c*** errs using gcc */

/*{{{  Read Configuration file  */
/*
  This routine reads a configuration file & returns
  a fully qualified, completely initialized NULL
  terminated array of Log class objects with filters
  and everything else.
  
  The parameter is the absolute filename of the configuration
  file to be read - the return value is the pointer to
  the structure that has been filled up.
*/
Log**
read_configuration_file(gchar* filename)
{
  Log** array = NULL;

  gint fd = open(filename, O_RDONLY);
  if ( fd == -1 ) return array;
  fstat( fd,  &status );
  int length = (int) status.st_size;
  char* buffer = new char[length+2];

  gint got = (gint) read(fd, buffer, length);
  close(fd);
  if ( got != length )
    {
      delete buffer;
      return array;
    }
  buffer[length] = 0; // Make it zero terminated
  
  /*
    And now start browsing the file
  */

  create_regex_patterns();

  gint x = 0;
  gchar* line = buffer;
  gint a;

  while ( x < length )
    {

      while ( x < length &&
	      buffer[x] != '\r' &&
	      buffer[x] != '\n' ) x++;
      
      buffer[x] = 0;
      a = 0;
      gint result = REG_NOMATCH;

      while ( mode_regex[a] != NULL &&
	      result == REG_NOMATCH )
	{
	  
	  result = regexec( *(mode_regex[a]),
			    line,
			    0,
			    NULL,
			    0);
	  
	  if ( result == REG_NOMATCH ) a++;
	}
      
      if ( result == 0 )
	{
	  /*
	    Line is a mode line
	  */
	  
	  gint new_mode = mode_flags[a];
	  
	  guint index = searchfor('{', line, 0);
	  if ( index >= strlen(line) )
	    {
	      free_regex_patterns();	    
	      delete buffer;
	      return array;
	    }
	  index++;
	
	  /*
	    retrieve filename
	  */
	  gchar* new_filename = getstring(',', line, index);
	  if ( new_filename == NULL )
	    {
	      free_regex_patterns();
	      delete buffer;
	      return array;
	    }
	  index += strlen(new_filename)+1;
	  new_filename = clean_string(new_filename);

	  /*
	    retrieve time interval & delay information of value
	    interval[/delay]
	  */
	  gint new_interval = 0;
	  gint new_delay = 6;
	  gchar* new_int_string = getstring(',', line, index);
	  if ( new_int_string == NULL )
	    {
	      delete new_filename;
	      free_regex_patterns();
	      delete buffer;
	      return array;
	    }
	  index += strlen(new_int_string)+1;
	  new_int_string = clean_string(new_int_string);
	  guint time_index = searchfor('/', new_int_string, 0);
	  if ( time_index < strlen(new_int_string) )
	    {
	      /* separate values */
	      new_int_string[time_index] = 0;
	      time_index++;
	      
	      /* read delay value */
	      gchar* delay_string = (gchar*) (&(new_int_string[time_index]));
/*	This does not work anymore with the new classes - jsg
	      istringstream(delay_string, strlen(delay_string)) >> new_delay;
*/
	      istringstream(delay_string) >> new_delay;
	    }
	  /* now read interval value */
/*	This does not work anymore with the new classes - jsg
	  istringstream(new_int_string, strlen(new_int_string)) >> new_interval;
*/
	  istringstream(new_int_string) >> new_interval;
	  if ( new_interval == 0 ||
	       strlen(new_int_string) == 0 ) new_interval = INTERVAL;
	  delete new_int_string;

	  /*
	    retrieve buttontext
	  */
	  gchar* new_buttontext = getstring(',', line, index);
	  if ( new_buttontext == NULL )
	    {
	      delete new_filename;
	      free_regex_patterns();
	      delete buffer;
	      return array;
	    }
	  index += strlen(new_buttontext)+1;
	  new_buttontext = clean_string(new_buttontext);
	  
	  /*
	    retrieve helptext
	  */
	  gchar* new_help = getstring('}', line, index);
	  if ( new_help == NULL )
	    {
	      delete new_buttontext;
	      delete new_filename;
	      free_regex_patterns();
	      delete buffer;
	      return array;
	    }
	  index += strlen(new_help)+1;
	  new_help = clean_string(new_help);
	  if ( strlen(new_help) == 0 )
	    {
	      delete new_help;
	      new_help = new char[strlen(new_filename)+1];
	      strcpy(new_help, new_filename);
	    }

	  /*
	    and now add the entry...
	  */
	  array = add_log_entry( array,
				 new_mode,
				 new_filename,
				 new_help,
				 new_buttontext,
				 NULL,
				 new_interval,
				 new_delay );

	  /*
	    and now kill the temporaries again
	  */
	  delete new_filename;
	  delete new_help;
	  delete new_buttontext;

	}
      else if ( regexec( plugin_regex,
			 line,
			 0,
			 NULL,
			 0 ) == 0 )
	{

	  /*
	    Line is a PLUGIN line
	  */
	  
	  gchar* new_filter_line = new gchar[strlen(line)];

	  gint x = 0;
	  gint start = (gint) searchfor('{', line, 0) + 1;
	  gint stop = strlen(line);
	  while ( stop >= 0 &&
		  line[stop] != '}' ) stop--;
	  
	  while ( start < stop )
	    {
	      new_filter_line[x] = line[start];
	      x++;
	      start++;
	    }
	  new_filter_line[x] = 0;

	  Log* element = NULL;
	  x = 0;
	  
	  while ( array != NULL && array[x]!= NULL )
	    {

	      element = array[x];
	      x++;

	    }
	  
	  if ( element != NULL )
	    {
	      
	      if ( element->plugin != NULL )
		delete element->plugin;
	      
	      element->plugin = new_filter_line;
	      
	    }
	  
	}
      else
	{
	  
	  /*
	    Line is no MODE or PLUGIN line, so we need to check whether
	    it is a filter line...
	  */

	  a = 0;
	  gint new_filter_mode = 0;

	  while ( filter_regex[a] != NULL )
	    {
	      
	      result = regexec( *(filter_regex[a]),
				line,
				0,
				NULL,
				0 );

	      /*
		if we have a match add the according flag to
		the filter flags
	      */
	      
	      if ( result == 0 )
		{
		  new_filter_mode |= filter_flags[a];
		}
	      
	      a++;
	    }

	  if ( new_filter_mode != 0 )
	    {
	      /*
		This line contains a valid filter entry, we just need
		the string(s) now...
	      */

	      /*
		The string will never be longer than our line
		and since this is only a very temporary array
		the waste of space shouldn't bother us too much.
	      */
	      gchar* new_filter_string = new gchar[strlen(line)];
	      gchar* new_filter_execline = NULL;

	      gint index = strlen(line);
	      while ( index >= 0 &&
		      line[index] != '}' ) index--;

	      gint end;
	      gint counter;
	      gint x;

	      /*
		Now we have the position of the last `}`
		in the line which is the delimiter of the
		REGEX or EXECUTE string, so first we need
		to check for the EXECUTE flag and fetch the
		string if present.

		these routines expect EXECUTE strings to
		contain no '{' !!!
	      */

	      if ( new_filter_mode & EXECUTE )
		{
		  new_filter_execline = new gchar[strlen(line)];
		  end = index;
		  
		  while ( index >= 0 &&
			  line[index] != '{' ) index--;
		  
		  counter = index+1;
		  x = 0;
		  
		  while ( counter < end )
		    new_filter_execline[x++] = line[counter++];

		  /*
		    zero termination is crucial
		  */
		  new_filter_execline[x] = 0;
		}

	      /*
		and now retrieve the REGEX in the same way,
		it is the expression that is standing between
		the '}' at the end of the line (after the EXECUTE
		line has been taken out if neccessary) and the
		first '{' from the beginning of the line.
	      */

	      while ( index >= 0 &&
		      line[index] != '}' ) index--;
	      
	      end = index;
	      counter = searchfor('{', line, 0) + 1;

	      x = 0;

	      while ( counter < end )
		new_filter_string[x++] = line[counter++];
	      
	      /*
		zero termination is crucial
	      */
	      new_filter_string[x] = 0;
	      
	      /*
		And now add the filter entry to the current
		log entry
	      */
	      add_filter_to_last( array,
				  new_filter_string,
				  new_filter_mode,
				  new_filter_execline );
	      
	      delete new_filter_string;
	      if ( new_filter_execline != NULL )
		{
		  delete new_filter_execline;
		}
	      
	    } 
	}
      
      /*
	Now step to the beginning of the next line
      */
      
      while ( x < length && 
	      ( buffer[x] == 0 ||
		buffer[x] == '\r' ||
		buffer[x] == '\n'  )) x++;
      
      line = &buffer[x];
      
    }
  
  free_regex_patterns();
  delete buffer;  

  return array;
}
/*}}}*/

/*{{{  Matching subroutines  */

// legalize filter mode, only one of the CLASS0 filters is allowed
gint 
legalize(gint fmode, gint def)
{
  gint activated = 0;
  gint x = 0;
  while ( class0_filters[x] != -1 )
    {
      if ( fmode & class0_filters[x] ) activated++;
      x++;
    }

  if ( activated > 1 )
    {
      /* leave class1 and special mode things untouched 
	 and delete all class0 */
      fmode &= ( CLASS1_MASK |  SPECIAL_MODE_MASK ); 
      fmode |= def; // set default
    }
  
  return fmode;
}
// tries to find "string" in "buffer" at position "x"
int 
match(char* string, char* buffer, int x)
{
  int i = 0;
  while( string[i] != 0 )
    if ( string[i++] != buffer[x++] )
      return FALSE;
  
  return TRUE;
}
// searches for char 'c' in buffer starting at x and returning position
int 
searchfor(char c, char* buffer, int x)
{
  while( buffer[x] != c && buffer[x] != 0 ) x++;
  return x;
}
// tries to create string from x to position of 'delim'
// returns NULL if out of bounds...
char* 
getstring(char delim, char* buffer, int x)
{
  int y = searchfor( delim, buffer, x);
  if ( buffer[y] == 0 ) return NULL;
  char* string = new char[y-x+1];
  int i = 0;
  while ( i < y-x ){
    string[i] = buffer[x+i];
    i++;
  }
  string[y-x] = 0;
  return string;
}
char* 
clean_string(char* source)
{
  if ( source == NULL ) return NULL;
  char* target = new char [strlen(source)+1];
  // eliminate leading and trailing spaces + all CR/LF's
  int a = 0;  // source
  int b = 0;  // target
  while ( source[a] == ' ' || source[a] == 10 || source[a] == 13 ) a++;
  while ( source[a] != 0 ){
    if ( source[a] != 10 && source[a] != 13 ) target[b++] = source[a++]; 
    else a++;
  }
 target[b--] = 0;
  while ( target[b] == ' ' || target[b] == 10 || target[b] == 13 ) target[b--] = 0;
  char* result = new char[strlen(target)+1];
  strcpy(result, target);
  delete source;
  delete target;

  return result;
}
/*}}}*/

/*{{{  REGEX subroutines  */
/*
  This routine does nothing else than initializing the
  patterns for the matching process
*/
void
create_regex_patterns()
{
  gint x = 0;

  while ( all_string[x] != NULL )
    {
      
      (*(all_regex[x])) = new regex_t;
      
      regcomp( (*(all_regex[x])),
	       all_string[x],
	       REG_NOSUB | REG_NEWLINE );
      x++;
    }
  
}
/*
  This routine frees the allocted patterns again...
*/
void
free_regex_patterns()
{
  gint x = 0;
  
  while ( all_string[x] != NULL )
    {
      
      regfree(*(all_regex[x]));
      delete (*(all_regex[x]));
      (*(all_regex[x])) = NULL;
      
      x++;
    }
  
}

/*}}}*/

/*{{{  Write Configuration  */
/*
  This routine writes an Xlogmaster style configuration file
  into the file fname (only parameter).
*/
int 
write_configuration(char* fname)
{
  ofstream file;
  file.open(fname, ios::out);
  if ( file.fail() ) return FALSE;
  
  for ( int i = 0 ; i < syslogs ; i++ )
    {
      if ( entry[i]->mode == TAIL_FILE ) file << "TAIL{";
      else if ( entry[i]->mode == RUN_FILE ) file << "RUN{";
      else if ( entry[i]->mode == CAT_FILE ) file << "CAT{";
      file << entry[i]->filename << ",";
      file << entry[i]->interval;
      if ( entry[i]->mode == RUN_FILE )
	file << "/" << entry[i]->delay;
      file << ",";
      file << entry[i]->buttontext << ",";
      file << entry[i]->help << "}" << endl;
      
      if ( entry[i]->plugin != NULL )
	{
	  file << "PLUGIN{";
	  file << entry[i]->plugin << "}" << endl;
	}
      
      if ( entry[i]->filter != NULL )
	{
	  int a = 0;
	  while ( entry[i]->filter[a] != NULL )
	    {

	      int fe = 0;
	      if ( entry[i]->filter[a]->mode & RAISE )
		{
		  file << "RAISE";
		  fe++;
		}
	
	      if ( entry[i]->filter[a]->mode & LOWER )
		{
		  if ( fe > 0 ) file << ",";
		  file << "LOWER";
		  fe++;
		}

	      if ( entry[i]->filter[a]->mode & HIDE )
		{
		  if ( fe > 0 ) file << ",";
		  file << "HIDE";
		  fe++;
		}
	
	      if ( entry[i]->filter[a]->mode & ALERT )
		{
		  if ( fe > 0 ) file << ",";
		  file << "ALERT";
		  fe++;
		}
	
	      if ( entry[i]->filter[a]->mode & NOTICE )
		{
		  if ( fe > 0 ) file << ",";
		  file << "NOTICE";
		  fe++;
		}
	
	      if ( entry[i]->filter[a]->mode & UNICONIFY )
		{
		  if ( fe > 0 ) file << ",";
		  file << "UNICONIFY";
		  fe++;
		}

	      if ( entry[i]->filter[a]->mode & EXECUTE )
		{
		  if ( fe > 0 ) file << ",";
		  file << "EXECUTE";
		  fe++;
		}

	      if ( entry[i]->filter[a]->mode & INVERT )
		{
		  if ( fe > 0 ) file << ",";
		  file << "INVERT";
		  fe++;
		}

	      if ( entry[i]->filter[a]->mode & CASE_SENSITIVE )
		{
		  if ( fe > 0 ) file << ",";
		  file << "CASE_SENSITIVE";
		  fe++;
		}
	
	      file << "{" << entry[i]->filter[a]->string << "}";
	      
	      if ( entry[i]->filter[a]->mode & EXECUTE )
		file << "{" << entry[i]->filter[a]->execline << "}";
	
	      file << endl;
	
	      a++;
	      
	    }
	}
    }
  
  file.close();
  return TRUE;
}
/*}}}*/

