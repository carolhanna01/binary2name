/* Handle getting a message from email into GNATS.
   Copyright (C) 2001, 07 Free Software Foundation
   Copyright (C) 1993, 1994, 1995 (FIXME: Brendan Kehoe or Cygnus/Red Hat)
   Contributed by Brendan Kehoe (brendan@cygnus.com).
   Further work by Milan Zamazal (pdm@zamazal.org) and Dirk Bergstrom
   (dirk@juniper.net).

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
#include "gnats-dirs.h"

/* Put sys/wait.h (for waitpid, WEXITSTATUS) after gnats.h to avoid signal.h
   syntax error about stack_t when compiling with gcc -ansi on solaris 10.  */
#include <sys/wait.h> /* FIXME */ 
#include <sys/param.h> /* FIXME: gets MAXBSIZE */

#ifndef MAXBSIZE /* FIXME - from sys/param.h */
#define MAXBSIZE 8192
#endif

/* The name this program was run with. */
const char *program_name;

/* Iff TRUE, queue the incoming message. */
bool queue_msg = FALSE;

/* Max size for messages.  */
int max_size = 0;

/* Iff TRUE, run pr-edit on each message in the queue. */
bool run_queue = FALSE;

/* If TRUE, emit debugging information. */
bool flag_debug = FALSE;

/* Which file (defaults to NULL, aka stdin) to use to read as the message to
   queue for GNATS. */
char *queue_file = NULL;

struct option long_options[] =
{
  {"database", 1, NULL, 'd'},
  {"global-databases-file", 1, NULL, 'g'},
  {"file", 1, NULL, 'f'},
  {"debug", 0, NULL, 'D'},
  {"queue", 0, NULL, 'q'},
  {"run", 0, NULL, 'r'},
  {"max-size", 1, NULL, 'm'},
  {"help", 0, NULL, 'h'},
  {"version", 0, NULL, 'V'},
  {NULL, 0, NULL, 0}
};

#define PROGRAM_NAME "queue-pr"

static const char *const USAGE[] = {
  "Usage: " PROGRAM_NAME " [OPTION]...\n\
Handle messages submitted via e-mail.\n",
"\n\
  -q, --queue               queue the given message\n\
  -r, --run                 process messages in the queue\n\
  -f, --file=FILENAME       read the message from FILENAME instead of stdin\n\
  -m, --max-size=KBYTES     don't process messages larger than KBYTES",
"\n\
  -d, --database=DATABASE   use DATABASE instead of the default database\n\
  -g, --global-databases-file=PATH   PATH to databases file\n\
  -D, --debug               enable some debugging\n\
  -h, --help                display this help and exit\n\
  -V, --version             display version information and exit\n\
",
"\n\
Usually you want to specify exactly one of the options `--queue' and `--run'.\
\n",
  NULL};


/* Run pr-edit on FILENAME and return its exit status. */
static int
fork_gnats (DatabaseInfo database, const char *filename)
{
  /* A safe environment for execle().  */
  static const char *safe_env[] = { "", "PATH=/bin:/usr/bin", NULL };
  char *ptr;

  int pid; /* pid_t */
  int status;

  const char *gaddr = gnatsAdminMailAddr (database);

  asprintf (&ptr, "USER=%s", gaddr);
  safe_env[0] = ptr;

  errno = 0;
  pid = fork();
  if (pid < 0)
    {
      punt (database, 1, "could not fork pr-edit: %s\n", strerror (errno));
    }
  else if (pid == 0)
    {
      char *pr_edit_bin;
      int fd;

      asprintf (&pr_edit_bin, "%s/pr-edit", binDir (database));

      if (! debugMode (database))
	{
	  /* Redirect stderr to /dev/null, since `at' will give you info about
	     the job you've queued.  If opening /dev/null fails, just ignore
	     it and go on.  */
	  fd = open ("/dev/null", O_RDWR);
	  if (fd > 0)
	    {
	      close (2);
	      fcntl (fd, F_DUPFD, 2);
	    }
	}

      errno = 0;
      if (flag_debug)
	{
	  if (execle (pr_edit_bin, "pr-edit", 
		      "--submit",
		      "-f", filename,
		      "--debug",
		      "--database", databaseName (database),
		      NULL, safe_env) < 0)
	    {
	      punt (database, 1, 
		    "execle of gnats failed: %s\n",  strerror (errno));
	    }
	}
      else
	{
	  if (execle (pr_edit_bin, "pr-edit",
		      "--submit",
		      "-f", filename,
		      "--database", databaseName (database),
		      NULL, safe_env) < 0)
	    {
	      punt (database, 1,
		    "execle of gnats failed: %s\n", strerror (errno));
	    }
	}

      exit (-1); /* XXX */
    }

  waitpid (pid, &status, 0); /* FIXME: not portable */
  free (ptr);

#ifndef WEXITSTATUS
#define WEXITSTATUS(x) (((union wait*)&(x))->w_retcode)
#endif
  return WEXITSTATUS (status);
}

/* Run a gnats process on each message in QUEUE_DIR.  */
static void
run_gnats (DatabaseInfo database, const char *queue_dir)
{
  DIR *d;
  struct dirent *next;
  int i;
  int nfiles = 0;
  int maxfiles = 10;
  struct file {
    char *name;
  } *files = (struct file *) xmalloc (sizeof (struct file) * maxfiles);

  if (chdir (queue_dir) < 0)
    {
      punt (database, 1, "can't open queue directory: %s", queue_dir);
    }

  errno = 0;
  d = opendir (".");
  if (! d)
    {
      log_msg (LOG_INFO, 1, "can't open: ", queue_dir);
      return;
    }

  while ((next = readdir (d)))
    if (next->d_name[0] != '.')
      {
	if (strcmp (next->d_name, "core") == 0)
	  {
	    log_msg (LOG_INFO, 0, "core file in queue directory");
	    continue;
	  }

	/* skip files larger than max_size */
	if (max_size)
	  {
	    struct stat buf;
	    if (stat ((char*) next->d_name, &buf) == 0)
	      {
		if ((int) buf.st_size > max_size)
		  {
		    char *max;
		    char *name2 = xmalloc (strlen ((char*) next->d_name) + 2);
		    strcpy (name2, ".");
		    strcat (name2, (char*) next->d_name);
		    rename ((char*) next->d_name, name2);
		    asprintf (&max, "%d", max_size / 1024);
		    punt (database, 0,
			  ("file `%s' larger than max-size of %sK.\n"
			   "renamed to `%s' pending human intervention.\n"),
			  (char*) next->d_name, max, name2);
		    log_msg (LOG_INFO, 1, "file larger than max-size:", name2);
		    free (name2);
		    free (max);
		    continue;
		  }
	      }
	  }		    

	if (nfiles == maxfiles)
	  {
	    maxfiles *= 2;
	    files = (struct file *) xrealloc ((char *) files,
					      sizeof (struct file) * maxfiles);
	  }
	files[nfiles++].name = (char *) xstrdup (next->d_name);
      }

  closedir (d);

  /* Run a gnats process for each file in the directory.  */
  for (i = 0; i < nfiles; i++)
    {
      int child_status;
      
      if (flag_debug)
	fprintf (stderr, "%s: running %s\n", program_name, files[i].name);

      child_status = fork_gnats (database, files[i].name);

      /* If gnats ran okay, then we can unlink the queue file.  Otherwise,
	 keep it around so we can give it another try whenever the problems
	 have been taken care of.
	 Exit status of 1: Simple failure, try again
	                2: Unexpected failure, probably case-specific
			3: Unexpected failure, probably general  */
      if (child_status == 0)
	{
	  if (unlink (files[i].name))
	    log_msg (LOG_INFO, 1, "cannot remove: ", files[i].name);
	}
      else if (child_status == 2)
	{
	  struct stat buf;
	  if (stat ((char*) files[i].name, &buf) != 0 &&
	      errno == ENOENT)
	    {
	      /* the file is gone, perhaps another queue-pr took care of it? */
	      punt (database, 0,
		    "file `%s' was gone when pr-edit tried to process it.\n",
		    files[i].name);
	    }
	  else
	    {
	      /* something else bad has happened with this file,
		 move it aside, and alert the admin */
	      char *name2 = xmalloc (strlen (files[i].name) + 2);
	      strcpy (name2, ".");
	      strcat (name2, files[i].name);
	      rename (files[i].name, name2);
	      punt (database, 0,
		    "renamed `%s' to `%s' pending human intervention.\n",
		    files[i].name, name2);
	      free (name2);
	    }
	  if (is_gnats_locked (database))
	    {
	      client_unlock_gnats ();
	    }
	}
      else if (child_status == 3)
	{
	  punt (database, 0,
		"pr-edit complained of a (probably) general problem.\n");
	}
    }
}

/* Drop the message in QUEUE_FILE (or, failing that, stdin) into the
   QUEUE_DIR.  */
static void
drop_msg (DatabaseInfo database, const char *queue_dir)
{
  int fd[2];
  const char *tmpdir;
  char *bug_file;
  int r; /* XXX ssize_t */
  char *buf = (char *) xmalloc (MAXBSIZE);
  char *base, *new_name;

  if (queue_file)
    {
      fd[0] = open (queue_file, O_RDONLY);
      if (fd[0] < 0)
	punt (database, 1, "can't open queue file %s for reading: %s\n",
	      queue_file, strerror (errno));
    }
  else
    fd[0] = 0;

  tmpdir = temporary_directory ();
  asprintf (&bug_file, "%s/gnatsXXXXXX", tmpdir);
  
  fd[1] = open_temporary_file (bug_file, 0664);
  if (fd[1] < 0)
    {
      punt (database, 1, "can't open queue file %s for writing: %s\n",
	    bug_file, strerror (errno));
    }
  
  while ((r = read (fd[0], buf, MAXBSIZE)) > 0)
    {
      if (write (fd[1], buf, r) < 0)
	{
	  if (fd[0])
	    close (fd[0]);
	  close (fd[1]);
	  punt (database, 1, "error in writing %s: %s\n",
		bug_file, strerror (errno));
	}
    }
  
  if (r < 0)
    {
      if (queue_file)
	punt (database, 1, "error reading queue file %s: %s\n",
	      queue_file, strerror (errno));
    }
  
  if (fd[0])
    {
      close (fd[0]);
    }
  close (fd[1]);

  errno = 0;
  base = basename (bug_file);
  asprintf (&new_name, "%s/%s", queue_dir, base);
  /* FIXME: Is there any guarantee here that NEW_NAME doesn't already exist? */
  if (rename (bug_file, new_name) < 0)
    {
      if (errno != EXDEV)
	{
	  punt (database, 1, "could not rename %s into the queue dir: %s\n",
		bug_file, strerror (errno));
	}
      
      {
	char *tmp_name;
	asprintf (&tmp_name, "%s/.%s", queue_dir, base);
	copy_file (bug_file, tmp_name);
	if (rename (tmp_name, new_name) < 0)
	  {
	    punt (database, 1, "could not rename %s to %s: %s\n",
		  tmp_name, new_name, strerror (errno));
	  }
      }

      if (unlink (bug_file))
	{
	  punt (database, 1, "cannot remove `%s'", bug_file);
	}
    }
}


int
main (int argc, char **argv)
{
  int optc;
  char *database_name = NULL;
  ErrorDesc err;
  char *queue_dir;
  DatabaseInfo database = NULL;
  unsigned long max_size_scale;
  unsigned long max_size_range;

  program_name = basename (argv[0]);

  /* FIXME: handle signals */

  while ((optc = getopt_long (argc, argv, "d:f:g:VmqrDh",
			      long_options, (int *) 0)) != EOF)
    {
      switch (optc)
	{
	case 'd':
	  if (optarg[0] == '\0')
	    {
	      punt (database, 1, "directory must be non-null\n");
	    }
	  database_name = optarg;
	  break;

	case 'f':
	  if (optarg[0] == '\0')
	    {
	      punt (database, 1, "filename must be non-null\n");
	    }
	  queue_file = optarg;
	  break;

	case 'g':
	  if (optarg[0] == '\0')
	    {
	      punt(database, 1, "filename must be non-null\n");
       	    }
	  global_db_list_file = optarg;
	  break;


	case 'r':
	  if (queue_msg)
	    {
	      punt (database, 1,
		    "-q and -r can't be specified at the same time.\n");
	    }
	  run_queue = TRUE;
	  break;

	case 'm':
	  max_size_range = LONG_MAX / 1024;
	  max_size_scale = strtol (optarg, NULL, 10);
	  if (max_size_scale <= 1 || 
	      max_size_scale > max_size_range || 
	      errno == ERANGE)
	    {
	      punt (database, 1,
		    "max-size must be an integer between 1 and %li\n",
		    max_size_range);
	    }
	  max_size = max_size * 1024;
	  break;

	case 'q':
	  if (run_queue)
	    {
	      punt (database, 1, 
		    "-q and -r can't be specified at the same time.\n");
	    }
	  queue_msg = TRUE;
	  break;

	case 'D':
	  flag_debug = TRUE;
	  break;

	case 'h':
	  usage (USAGE, 0);
	  break;

	case 'V':
	  fprintf (stderr, "queue-pr version %s.\n", version_string);
	  exit (0);
	  break;

	default:
	  usage (USAGE, 1);
	  break;
	}
    }

  umask (022);

  database = init_gnats (program_name, database_name, &err);

  if (! databaseValid (database))
    {
      client_print_errors (database, err);
      fprintf (stderr, "%s: invalid value for GNATS database\n",
	       program_name);
      exit (1);
    }

  asprintf (&queue_dir, "%s/gnats-queue", databaseDir (database));

  if (queue_msg)
    {
      drop_msg (database, queue_dir);
    }
  if (run_queue)
    {
      run_gnats (database, queue_dir);
    }

  free (queue_dir);

  exit (0);
}
