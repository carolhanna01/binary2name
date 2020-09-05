/* GNATS password conversion tool.
   Copyright (C) 2001, 2002, 2007 Free Software Foundation, Inc.

This file is part of GNU GNATS.

GNU GNATS is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU GNATS is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU GNATS; see the file COPYING. If not, see
<http://www.gnu.org/licenses/>.
*/

#include "gnats.h"
#ifdef HAVE_LIBCRYPT
#ifdef HAVE_CRYPT_H		/* some systems declare `crypt' in unistd.h */
#include <crypt.h>
#endif
#endif

enum _Crypt_Type { NONE, PLAIN, CRYPT, MD5 };
typedef enum _Crypt_Type Crypt_Type;


#define PROGRAM_NAME "gnats-pwconv"
const char *program_name = PROGRAM_NAME;
/* static const int MAXLINE = 1024; */
static const char* const PROGRAM_VERSION = "1.0";

static const char* USAGE[] =
  {
    "Usage: ", PROGRAM_NAME,
#ifdef HAVE_LIBCRYPT
    " {-c | -m | -p} INFILE [OUTFILE]\n",
#else
    " -p INFILE [OUTFILE]\n",
#endif
    "Convert GNATS v3 gnatsd*.access passwords to version 4 format.\n",
#ifdef HAVE_LIBCRYPT
    "Supports plaintext, DES crypt and MD5 output.\n",
#else
    "Supports only unencrypted plaintext (compiled without `libcrypt').\n",
#endif
    "\n",
#ifdef HAVE_LIBCRYPT    
"  -c  --crypt        Use crypt() encryption of passwords.\n\
  -m  --md5          Use MD5 encryption.\n",
#endif
"  -p  --plaintext    Do not encrypt the passwords (plaintext).\n\
  -h  --help\n\
  -V  --version\n",
    NULL
  };

/* Print perror MESSAGE, processed through printf with a single argument ARG.
 */
static void
perrorf (const char *message, char *arg)
{
  char *formatted;
  if (asprintf (&formatted, message, arg) < 0)
    {
      perror ("System error");
      exit (1);
    }
  perror (formatted);
  free (formatted);
}


/* Split LINE into its fields.
   Return true iff the operation was successful.
   Note: LINE is modified during the operation.
*/
static bool
parse_entry (char *line,
	     char **username, char **password, char **level, char **dbs)
{
  int i;
  int length = strlen (line);
  char *separator;
  char **fields[4];
  fields[0] = username; fields[1] = password;
  fields[2] = level; fields[3] = dbs;

  if (length && line[length-1] == '\n')
    {
      line[length-1] = '\0';
    }
    
  *dbs = (char *)"";
  for (i = 0;
       (i < 4) && (separator = (char *)strchr (line, ':')) != NULL;
       i++)
    {
      *(fields[i]) = line;
      *separator = '\0';
      line = separator + 1;
    }

  return i >= 3;
}


/* Encrypt CLEARPWSTRING by the encryption type CRYPTTYPE and store the result
   into NEWPWENTRY.  Return 0 on success, 1 if MD5 encryption is requested and
   it is not supported by the system. */
static int
encrypt_ (char *clearpwstring, Crypt_Type crypttype, char **newpwentry)
{
  int result;
#ifdef HAVE_LIBCRYPT
  unsigned long seed = random();
  char salt[12];
  char rawsalt[9];
  const char *SEEDCHARS =
    "./0123456789ABCDEFGHIJKLMNOPQRST"
    "UVWXYZabcdefghijklmnopqrstuvwxyz";
  const int SEEDCHARS_LEN = strlen (SEEDCHARS);
  int i;
  
  for (i = 0; i < 8; i++)
    {
      rawsalt[i] = SEEDCHARS[(seed/(i+1)) % SEEDCHARS_LEN];
    }
  rawsalt[i] = '\0';
#endif
  
  switch (crypttype)
    {
    case PLAIN:
      result = asprintf (newpwentry, "$0$%s", clearpwstring);
      break;
      
#ifdef HAVE_LIBCRYPT      
    case CRYPT:      
      strncpy (salt, rawsalt, 2);
      salt[2] = '\0';
      result = asprintf (newpwentry, "%s", crypt (clearpwstring, salt));
      break;
      
    case MD5:
      sprintf (salt, "$1$%s", rawsalt);
      result = asprintf (newpwentry, "%s", crypt (clearpwstring, salt));
      break;
#endif
      
    default:
      fprintf (stderr, "Program error\n");
      exit (1);
    }

  if (result < 0)
    {
      fprintf (stderr, "Memory allocation error\n");
      exit (1);
    }
  
  /* Are we on a system that supports MD5? */
  return (crypttype == MD5 && strncmp ("$1$", *newpwentry, 3)) ? 1 : 0;
}


/* Convert password data in the file INFILE to the file OUTFILE.
   OUTFILE can be NULL, in which case the data is output to stdout.
   CRYPTTYPE is passed through to `encrypt_'.
   Return 0 on success, anything else otherwise. */
static int
process_file (char *infile, char *outfile, int crypttype)
{
  FILE *input;
  FILE *output;
  char *line;
  int i;
  
  if ((input = fopen (infile, "r")) == NULL)
    {
      perrorf ("Can't open the file `%s' for input", infile);
      return 1;
    }
  if (outfile == NULL)
    {
      output = stdout;
    }
  else if ((output = fopen (outfile, "w")) == NULL)
    {
      perrorf ("Can't open the file `%s' for output", outfile);
      return 1;
    }
  
  for (i = 1; (line = read_line (input, NULL)) != NULL; i++)
    {
      if (strncmp (line, "#", 1) == 0
	  || (strspn (line, " \t\r\n") == strlen (line)))
	{
	  fprintf (output, "%s", line);
	}
      else
	{
	  char *username, *password, *level, *dbs;
	  char *newpwentry;
	  if (parse_entry (line, &username, &password, &level, &dbs))
	    {
	      if (encrypt_ (password, crypttype, &newpwentry) == 1)
		{
		  fprintf (stderr, "Error: "
			   "MD5 encryption not supported on this system.\n");
		  free (line);
		  free (newpwentry);
		  return 1;
		}
	      fprintf (output, "%s:%s:%s:%s\n",
		       username, newpwentry, level, dbs);
	      free (newpwentry);
	    }
	  else
	    {
	      fprintf (stderr,
		       "Error: Unable to decode line %d of the file `%s'.\n",
		       i, infile);
	      free (line);
	      return 1;
	    }
	  free (line);
	}
    }
  
  fclose(input);
  fclose(output);
  
  return 0;
}


int
main (int argc, char **argv)
{
#ifdef HAVE_LIBCRYPT
  static const char* const OPTSTRING = "pcmVh";
#else
  static const char* const OPTSTRING = "pVh";
#endif
  int optc = 0;
  int opt_index = 0;
  Crypt_Type crypttype = NONE;
  char *infile = NULL;
  char *outfile = NULL;
  struct option LONG_OPTIONS[] =
    {
      {"plaintext", no_argument, NULL, 'p'},
#ifdef HAVE_LIBCRYPT
      {"crypt",     no_argument, NULL, 'c'},
      {"md5",       no_argument, NULL, 'm'},
#endif
      {"version",   no_argument, NULL, 'V'},
      {"help",      no_argument, NULL, 'h'}, 
      {0, 0, 0, 0}
    };
  
  while ((optc = getopt_long (argc, argv, OPTSTRING, LONG_OPTIONS, &opt_index))
	 != -1)
    {
      switch (optc)
	{
	case 'p':
	  if (crypttype != NONE)
	    usage (USAGE, 1);
	  crypttype = PLAIN;
	  break;

	case 'c':
	  if (crypttype != NONE)
	    usage (USAGE, 1);
	  crypttype = CRYPT;
	  break;

	case 'm':
	  if (crypttype != NONE)
	    usage (USAGE, 1);
	  crypttype = MD5;
	  break;

	case 'V':
	  printf ("%s %s\n", PROGRAM_NAME, PROGRAM_VERSION);
	  exit (0);
	  break;

	case 'h':
	  usage (USAGE, 0);
	  break;
      
	default:
	  usage (USAGE, 1);
	}
    }

  if ((crypttype == NONE) || (optind >= argc))
    usage (USAGE, 1);
  
  infile = argv[optind++];
  if (optind < argc)
    {
      outfile = argv[optind];
    }
  
  srandom (time (NULL));
  return process_file (infile, outfile, crypttype);
}
