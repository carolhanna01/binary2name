/* shmm.c -- Help you to manage easily all your shared memory.

   Copyright (C) 2008 Jeannie BOFFEL.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  */

/*
 *
 * shmm for Shared Memory Manager.
 * The aim is to deal with standard
 * IPC even with a shell script !
 * Warning ! Use only with string !!
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <locale.h>
#include <ctype.h>

#define _GNU_SOURCE
#include <getopt.h>

#define PACKAGE "shmm"
#define PACKAGE_BUGREPORT "Jeannie BOFFEL <jboffel@gmail.com>"
#define VERSION "1.0"

/* Internationalization.  */
#include "gettext.h"
#define _(str) gettext (str)
#define N_(str) gettext_noop (str)

/* Check for errors on write.  */
#include "closeout.h"

static void print_help (void);
static void print_version (void);

int cmdmem (int , int , char , int );
int writemem (int , int , int , char *, int );
int readmem (int , int , int , int );
int updatemem (int , int , int , int , int , int );
int checkPermFromId (int , char );
size_t getSizeFromId (int );

const char *program_name;

char *path_file;

int binary_mode = 0, shmid = 0, file_mode = 0;

#  if __BYTE_ORDER == __BIG_ENDIAN
struct permissions
{
  int unused:23;

  char uidr:1;
  char uidw:1;
  char uidx:1;

  char gidr:1;
  char gidw:1;
  char gidx:1;

  char oidr:1;
  char oidw:1;
  char oidx:1;
};
#  endif
#  if __BYTE_ORDER == __LITTLE_ENDIAN
struct permissions
{
  char oidx:1;
  char oidw:1;
  char oidr:1;

  char gidx:1;
  char gidw:1;
  char gidr:1;

  char uidx:1;
  char uidw:1;
  char uidr:1;

  int unused:23;
};
#  endif

static const struct option long_options[] = {
  {"shm-access", required_argument, NULL, 's'},
  {"id", required_argument, NULL, 'i'},
  {"read", no_argument, NULL, 'r'},
  {"write", optional_argument, NULL, 'w'},
  {"offset", required_argument, NULL, 'o'},
  {"command", required_argument, NULL, 'c'},
  {"update", required_argument, NULL, 'u'},
  {"binary", no_argument, NULL, 'b'},
  {"file", required_argument, NULL, 'f'},
  {"version", no_argument, NULL, 'v'},
  {"help", no_argument, NULL, 'h'},
  {NULL, 0, NULL, 0}
};

int
main (int argc, char **argv)
{

  char mode, cmd, c, *token, *string;
  int key = 0;
  int offset = 0;
  int size = 0;
  int perm = 0;
  int newperm = 0;
  int newuid = 0;
  int newgid = 0;
  int we_need_s = 0;

  int option_index = 0;

  int return_code = 0;

  program_name = argv[0];

  setlocale (LC_ALL, "");

#if ENABLE_NLS
  /* Set the text message domain.  */
  bindtextdomain (PACKAGE, LOCALEDIR);
  textdomain (PACKAGE);
#endif

  atexit (close_stdout);

  if (argc < 2)
  {
    printf (_("Usage of shmm : %s (-s shm_key:access_perm:size | -i shmid) ( ( (-r [-o offset]) | (-wstring [-o offset]) ( [-b] | [-f] ) ) | (-c l|u) | (-u uid:gid:perm) )\nWhere :\n-r is for read only\n-w for write only\n-c for command (man shmctl), l=lock u=unlock\n-s for access infos :\n  shm_key for shared memory key\n  access_perm for access permission to the shared memory (ex: 0600)\n  size for the size of the shared memory\n-o for offset for where to start.\n-b is to print binary content of a shared memory\n-i can be used instead of -s to give an existing shmid directly!\n-f is to read to or write from a file raw memory data\n-u permit to update uid, gid and permissions of a shared memory segment\n"), program_name);
    exit (EXIT_FAILURE);
  }

  while (1)
  {

    c = getopt_long (argc, argv, "u:f:i:s:o:c:rbhvw::", long_options, &option_index);

      if (c == -1)
        break;

    switch (c)
    {
      case 'u' :
        if (sscanf (optarg, "%i:%i:%i", &newuid, &newgid, &newperm) != 3)
        {
          fprintf (stderr, _("Bad values for: update infos : %s !\n"), optarg);
          exit (EXIT_FAILURE);
        }
        mode = 'u';
        break;
      case 'f' :
        file_mode = 1;
        path_file = optarg;
        break;
      case 'i' :
        if (sscanf (optarg, "%i", &shmid) != 1)
        {
          fprintf (stderr, _("Bad values for: shmid : %s !\n"), optarg);
          exit (EXIT_FAILURE);
        }
        break;
      case 's' :
        if (sscanf (optarg, "%li:%i:%i", &key, &perm, &size) != 3)
        {
          fprintf (stderr, _("Bad values for: access infos : %s !\n"), optarg);
          exit (EXIT_FAILURE);
        }
        strtok (optarg, ":");
        token = strtok (NULL, ":");
        if (!(token[0] == '0' && token[1] >= '0' && token[1] <= '7' && token[2] >= '0' && token[2] <= '7' && token[3] >= '0' && token[3] <= '7') || strlen (token) != 4)
        {
          fprintf (stderr, _("Bad values for: permission : %s ! (should be 0xxx like 0640)\n"), token);
          exit (EXIT_FAILURE);
        }
        we_need_s = 1;
        break;
      case 'r' :
        mode = 'r';
        break;
      case 'w' :
        mode = 'w';
        string = optarg;
        break;
      case 'o' :
        if (sscanf (optarg, "%i", &offset) !=1 )
        {
          fprintf (stderr, _("Bad value for: offset !\n"));
          exit (EXIT_FAILURE);
        }
        break;
      case 'c' :
        mode = 'c';
        if (sscanf (optarg, "%c", &cmd) !=1 )
        {
          fprintf (stderr, _("Bad value for: command !\n"));
          exit (EXIT_FAILURE);
        }
        break;
      case 'b' :
        binary_mode = 1;
        break;
      case 'h' :
        print_help ();
        exit (EXIT_SUCCESS);
      case 'v' :
        print_version ();
        exit (EXIT_SUCCESS);
      case '?' : exit (EXIT_FAILURE);
      default : fprintf (stderr, _("Strange arguments: %c\n"), c);
    }
  }

  if (we_need_s == 0 && shmid == 0)
  {
    fprintf (stderr, _("You must specify --shm-access or --id at least !\n"));
    exit (EXIT_FAILURE);
  }

  switch (mode)
  {
    case 'r' :
      return_code = readmem (perm, key, offset, size);
      break;
    case 'w' :
      return_code = writemem (perm, key, offset, string, size);
      break;
    case 'c' :
      return_code = cmdmem (perm, key, cmd, size);
      break;
    case 'u' :
      return_code = updatemem(key, size, perm, newperm, newuid, newgid);
      break;
    default : 
      fprintf (stderr, _("Unknown mode: Did you set -r or -w or -c ?\n"), mode);
      exit (EXIT_FAILURE);
  }

  if (return_code != 0)
  {
    perror (NULL);
    exit (EXIT_FAILURE);
  }

  exit (EXIT_SUCCESS);

}

int
readmem (int perm, int key, int offset, int size)
{

  int id, i;
  size_t realsize = 0;
  char *mem;
  FILE *writeto;

  if (shmid == 0)
  {
    if ((id = shmget (key, size, perm)) == -1)
    {
      fprintf (stderr, _("Fatal error: Can't open shared memory.\n"));
      return -1;
    }
    if (checkPermFromId (id, 'r') == -1)
    {
      fprintf (stderr, _("Fatal error: You can't access this shared memory\n"));
      return -1;
    }
  }
  else
  {
    id = shmid;
    if (checkPermFromId (id, 'r') == -1)
    {
      fprintf (stderr, _("Fatal error: You can't access this shared memory\n"));
      return -1;
    }
  }

  if ((realsize = getSizeFromId (id)) == -1)
  {
    fprintf (stderr, _("Fatal error: When accessing shared memory informations\n"));
    return -1; 
  }

  if (shmid != 0)
    size = realsize;

  if (size > realsize)
  {
    fprintf (stderr, _("Warning: Size given in --shm-access is greater than the realsize\n"));
    if (binary_mode)
    {
      fprintf (stderr, _("Warning: Binary mode used so we trunk size to realsize\n"));
      size = realsize;
    }
  }
  else if (size < realsize)
  {
    fprintf (stderr, _("Warning: Size given in --shm-access is lower than the realsize\n"));
  }

  if ((mem = shmat (id, NULL, SHM_RDONLY)) == (void *)-1)
  {
    fprintf (stderr, _("Fatal error: We can't attach shared memory.\n"));
    return -1;
  }

  if (!file_mode)
  {
    if (binary_mode)
    {
      for (i = offset; i < size; i++)
      {
        if (isprint ((unsigned char)*(mem+i)))
          printf ("%c ", (unsigned char)*(mem+i));
        else
          printf ("0x%0.2x ", (unsigned char)*(mem+i));
      }
      printf ("\n");
    }
    else
    {
      printf ("%s\n", mem+offset);
    }
  }
  else
  {
    if ((writeto = fopen (path_file, "a")) == NULL)
    {
      return -1;
    }
    if (fwrite ((const char *)mem+offset, size, 1, writeto) != 1)
    {
      fprintf (stderr, _("Warning: we couldn't write all shared memory in file %s\n"), path_file);
    }
    fclose (writeto);
  }

  if (shmdt (mem) == -1)
  {
    fprintf (stderr, _("Impossible to dettach shared memory !\n"));
    return -1;
  }

  return 0;
}

int
writemem (int perm, int key, int offset, char *data, int size)
{

  int id, length = 0;
  size_t realsize = 0;
  char *mem;
  int i;
  FILE *readfrom;
  struct stat binfile;

  if (data != NULL && !file_mode)
  {
    length = strlen (data);
  }
  else if (file_mode)
  {
    if (stat (path_file, (struct stat *)&binfile) == -1)
    {
      return errno;
    }
    length = binfile.st_size;

    if ((readfrom = fopen (path_file, "r")) == NULL)
    {
      return errno;
    }
  }

  if (shmid != 0)
  {
    id = shmid;
    if (checkPermFromId (id, 'w') == -1)
    {
      fprintf (stderr, _("Fatal error: You can't access this shared memory\n"));
      return errno;
    }
    if ((size = getSizeFromId (id)) == -1)
    {
      fprintf (stderr, _("Fatal error: When accessing shared memory informations\n"));
      return errno; 
    }
  }

  if (!binary_mode)
  {
    if ((size - 1) - offset < length)
    {
      fprintf (stderr, _("Impossible to write data in memory: Not enough space, %d:%d:%d.\n"), size - 1, offset, length);
      errno = ENOMEM;
      return errno;
    }
  }
  else
  {
    if (size - offset < length)
    {
      fprintf (stderr, _("Impossible to write data in memory: Not enough space, %d:%d:%d.\n"), size, offset, length);
      errno = ENOMEM;
      return errno;
    }
  }

  if (shmid == 0)
  {
    if ((id = shmget (key, size, perm|IPC_CREAT|IPC_EXCL)) == -1)
    {
      if ((id = shmget (key, size, perm)) == -1)
      {
        fprintf (stderr, _("Fatal error: Can't open or create shared memory !\n"));
        return errno;
      }
      if (checkPermFromId (id, 'w') == -1)
      {
        fprintf (stderr, _("Fatal error: You can't access this shared memory\n"));
        return errno;
      }
      if ((realsize = getSizeFromId (id)) == -1)
      {
        fprintf (stderr, _("Fatal error: When accessing shared memory informations\n"));
        return errno;
      }
      if (size < realsize)
      {
        fprintf (stderr, _("Warning: Size given in --shm-access is lower than the one of the existing shared memory\n"));
      }
      else if (size > realsize)
      {
        fprintf (stderr, _("Fatal error: Size of existing shared memory is lower than the one given in --shm-access informations !\n"));
        errno = ENOMEM;
        return errno;
      }
      if ((mem = shmat (id, NULL, 0)) == (void *)-1)
      {
        fprintf (stderr, _("Fatal error: We can't attach shared memory.\n"));
        return errno;	
      }
    }
    else
    {
      if (checkPermFromId (id, 'w') == -1)
      {
        fprintf (stderr, _("Fatal error: You can't access this shared memory\n"));
        return errno;
      }
      if ((mem = shmat (id, NULL, 0)) == (void *)-1)
      {
        fprintf (stderr, _("Fatal error: We can't attach shared memory.\n"));
        return errno;	
      }
      if (!binary_mode)
        for (i = 0; i < size; i++)
          mem[i]='\0';
    }
  }
  else
  {
    if (*(mem = shmat (id, NULL, 0)) == -1)
    {
      fprintf (stderr, _("Fatal error: We can't attach shared memory.\n"));
      return errno;
    }
  }

  if (file_mode)
  {
    if (fread (mem+offset, length, 1, readfrom) != 1)
    {
      if (ferror (readfrom) != 0)
      {
        fclose (readfrom);
        fprintf (stderr, _("Fatal error: writing in memory failed, try again.\n"));
        errno = EXFULL;
        return -1;
      }
    }
    fclose (readfrom);
  }
  else
  {
    for (i = 0; i < length; i++)
      mem[offset+i] = data[i];
  }

  if (shmdt (mem) == -1)
  {
    fprintf (stderr, _("Impossible to dettach shared memory !\n"));
    return errno;
  }

  return 0;
}

int
cmdmem (int perm, int key, char cmd, int size)
{

  int id;

  if (shmid != 0)
  {
    id = shmid;
  }
  else
  {
    if ((id = shmget (key, size, perm)) == -1)
    {
      fprintf (stderr, _("Fatal error: Can't open shared memory !\n"));
      return errno;
    }
  }

  if (checkPermFromId (id, 'c') == -1)
  {
    fprintf (stderr, _("Fatal error: You can't access this shared memory\n"));
    return errno;
  }

  switch (cmd)
  {
    case 'l' :
      if (shmctl (id, SHM_LOCK, NULL) == -1)
      {
        fprintf (stderr, _("Error while executing cmd: %c\n"), cmd);
        return errno;
      }
      break;
    case 'u' :
      if (shmctl (id, SHM_UNLOCK, NULL) == -1)
      {
        fprintf (stderr, _("Error while executing cmd: %c\n"), cmd);
        return errno;
      }
      break;
    default:
      fprintf (stderr, _("Unknown command: %c !\n"), cmd);
      errno = EINVAL;
      return 1;
  }
  return 0;
}

int
updatemem (int key, int size, int perm, int newperm, int newuid, int newgid)
{

  int id;
  struct shmid_ds shminfos;
  struct ipc_perm updatedinfos;

  updatedinfos.uid = newuid;
  updatedinfos.gid = newgid;
  updatedinfos.mode = newperm;

  shminfos.shm_perm = updatedinfos;

  if (shmid != 0)
  {
    id = shmid;
  }
  else
  {
    if ((id = shmget (key, size, perm)) == -1)
    {
      fprintf (stderr, _("Fatal error: Can't open shared memory !\n"));
      return errno;
    }
  }

  if (checkPermFromId (id, 'c') == -1)
  {
    fprintf (stderr, _("Fatal error: You can't access this shared memory\n"));
    return errno;
  }

    if (shmctl (id, IPC_SET, (struct shmid_ds *)&shminfos) == -1)
    {
      fprintf (stderr, _("Error while updating data\n"));
      return errno;
    }

  return 0;
}

int
checkPermFromId (int id, char mode)
{

  uid_t uid, cuid, myuid;
  gid_t gid, cgid, mygid;
  unsigned short perm = 0;
  struct shmid_ds informations;
  struct permissions easyperm; 

  /*If perm was 0000 during creation, only root can access it.*/
  if (shmctl (id, IPC_STAT, (struct shmid_ds *)&informations) == -1)
  {
    return -1;
  }

  uid = informations.shm_perm.uid;
  gid = informations.shm_perm.gid;
  cuid = informations.shm_perm.cuid;
  cgid = informations.shm_perm.cgid;
  perm = informations.shm_perm.mode;

  memcpy ((void *)&easyperm, (const void *)&perm, sizeof (int));

  myuid = geteuid ();
  mygid = getegid ();

  /* Root or creator get all access everytime... */
  if (myuid == 0 || myuid == cuid)
    return 0;

  switch (mode)
  {
    case 'r' :
      if (myuid == uid)
        if (easyperm.uidr)
          return 0;
      if (mygid == gid || mygid == cgid)
        if (easyperm.gidr)
          return 0;
      if (easyperm.oidr)
        return 0;
      break;
    case 'w' :
      if (myuid == uid)
        if (easyperm.uidw)
          return 0;
      if (mygid == gid || mygid == cgid)
        if (easyperm.gidw)
          return 0;
      if (easyperm.oidw)
        return 0;
      break;
    case 'c' :
      if (myuid == uid)
        return 0;
      break;
    default :
      fprintf (stderr, _("Abnormal mode !\n"));
      errno=EINVAL;
      return -1;
  }
  errno = EACCES;
  return -1;
}

size_t
getSizeFromId (int id)
{

  struct shmid_ds informations;

  if (shmctl (id, IPC_STAT, (struct shmid_ds *)&informations) == -1)
  {
    return -1;
  }

  return informations.shm_segsz;

}

/* Print help info.  This long message is split into
   several pieces to help translators be able to align different
   blocks and identify the various pieces.  */

static void
print_help (void)
{
  /* TRANSLATORS: --help output 1 (synopsis)
     no-wrap */
        printf (_("\
Usage: %s [OPTION]...\n"), program_name);

  /* TRANSLATORS: --help output 2 (brief description)
     no-wrap */
  fputs (_("\
Help you to manage easily all your shared memories.\n"), stdout);

  puts ("");
  /* TRANSLATORS: --help output 3: options 1/2
     no-wrap */
  fputs (_("\
  -h, --help            display this help and exit\n\
  -v, --version         display version information and exit\n"), stdout);

  puts ("");
  /* TRANSLATORS: --help output 4: options 2/2
     no-wrap */
  fputs (_("\
  -s, --shm-access=TEXT use to give access info as key:permission:size\n\
                        (permission as 0xxx like 0666)\n\
  -r, --read            use to read shared memory\n\
  -w, --write=TEXT      use TEXT to write data in shared memory until size-1\n\
                        except if -b is set. TEXT is an optional argument\n\
                        Create the shared memory if it not exists\n\
  -c, --command=CHAR    use CHAR to 'l'ock or 'u'nlock your shared memory\n\
  -u, --update=TEXT     use update to set new user id, group id, permission\n\
                        with TEXT as : uid:gid:perm (perm as 0xxx like 0666)\n\
  -o, --offset=NUM      use NUM to start after 0 with read or write options\n\
  -b, --binary          use to print hex and/or char (when printable) dump of\n\
                        the memory\n\
  -i, --id              use to give existing id instead of key and size\n\
  -f, --file=TEXT       use to read to or write raw memory from file depending\n\
                        on -r (data are appended to existing file) or -w\n"), stdout);

  printf (_("\n"));
  /* TRANSLATORS: --help output 5 (end)
     TRANSLATORS: the placeholder indicates the bug-reporting address
     for this application.  Please add _another line_ with the
     address for translation bugs.
     no-wrap */
  printf (_("\
Report bugs to %s.\n"), PACKAGE_BUGREPORT);
}

/* Print version and copyright information.  */

static void
print_version (void)
{
  printf (_("\
shmm (GNU %s) %s\n"), PACKAGE, VERSION);
  /* xgettext: no-wrap */
  puts ("");

  /* It is important to separate the year from the rest of the message,
     as done here, to avoid having to retranslate the message when a new
     year comes around.  */
  printf (_("\
Copyright (C) %s Jeannie BOFFEL.\n\
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>\n\
This is free software: you are free to change and redistribute it.\n\
There is NO WARRANTY, to the extent permitted by law.\n"),
              "2008");
}
