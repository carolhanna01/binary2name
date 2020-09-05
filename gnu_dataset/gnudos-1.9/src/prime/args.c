/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2013, 2014, 2015 (c)
 * 
 *    file: args.c
 *    This file is part of Prime.
 *
 *    Prime is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    (at your option) any later version.
 *
 *    Prime is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with Prime.  If not, see <http://www.gnu.org/licenses/>.
 */    
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <asm/types.h>
#include <limits.h>
#include <string.h>
#include <pwd.h>
#include <time.h>
#include <sys/stat.h>
#include "defs.h"

struct passwd *pass;//will be used to find the home dir
time_t tm;

char *prime_ver = "1.3";

extern int NEW_GNU_DOS_LEVEL;
extern int write_config_file_defaults();

char *log_file_name;//string holding the name of the log file
FILE *log_file;
char *config_file_name;//string holding the name of the config file
FILE *config_file;
int GNU_DOS_LEVEL;

void parse_args(int argc, char **argv);

void parse_args(int argc, char **argv)
{
	/////////////////////////////////////////////
	//if no command line args, just open the
	//log file and carry on with program startup
	/////////////////////////////////////////////
	if(argc == 1)
	{
	  if((pass = getpwuid(geteuid())) == NULL)
	  {
	    printf("Error: couldn't open home directory to write log file.\n");
	    printf("If you want to start Prime with no log, type '%s"
	      "--no-log'\nAborting.\n", argv[0]);
	    exit(1);
	  }
	  log_file_name = (char *) malloc(strlen(pass->pw_dir)+12);
	  if(!log_file_name) { printf("Insufficient memory\n"); exit(1); }
	  strcpy(log_file_name, pass->pw_dir);
	  strcat(log_file_name, "/");
	  strcat(log_file_name, "prime.log\0");
	  if(!(log_file = fopen(log_file_name, "w"))) 
	  {
	    printf("Fatal error: couldn't open logfile 'prime.log'.\n"
	      "If you want to start Prime with no log, type '%s --no-log'\n"
	      "Aborting.\n", argv[0]);
	    exit(1);
	  }
	  return;
	}

	///////////////////////////////////////
	//parse command line arguments
	///////////////////////////////////////
	int c;
	//char *file_name = NULL;
	static int EXPORT_FLAG = 0;
	static int LOG_FILE_SET = 0;
	char *EXPORT_DIR = NULL, *EXPORT_FILE = NULL;
	static struct option long_options[] =
	{
	 {"no-log",       no_argument,            0,  'n'},
	 {"reset-config", no_argument,            0,  'r'},
	 {"help",         no_argument,            0,  'h'},
	 {"level",  required_argument,            0,  'l'},
	 {"log",    required_argument, &LOG_FILE_SET, 'g'},
	 {"export", required_argument, &EXPORT_FLAG,  'e'},
	 {"version",      no_argument,            0,  'v'},
	 {0, 0, 0, 0}
	};

	while(1)
	{
		int option_index=0;
		c = getopt_long(argc, argv, "vnhl:g:e:r", long_options, &option_index);
		if(c==-1) break;	//end of options

		switch(c)
		{
		case 0:
			break;
		case 'r':	//reset config file
		  if(!write_config_file_defaults())
		  {
		    printf("Error writing default config file: .prime.conf.\n");
		    exit(1);
		  }
		  break;
		case 'n':	//no log
		  //******Start Prime with no log information
		  log_file = fopen("/dev/null", "w");
		  LOG_FILE_SET = 1;
		  break;
		case 'e':	//export directory tree
		  EXPORT_DIR = (char *)malloc(strlen(optarg));
		  if(!EXPORT_DIR) { printf("Insufficient memory\n"); exit(1); }
		  strcpy(EXPORT_DIR, optarg);
		  //set the flag for later
		  EXPORT_FLAG = 1;
		  break;
		case 'l':	//set GNU_DOS level
		  if(0) ;
		  int i = atoi(optarg);
		  if(i < 1 || i > 6)
		  {
			  printf("Unrecognised level. See 'man prime' or "
				 "'info prime' for information about possible"
				 " levels.\n");
			  exit(1); break;
		  }
		  NEW_GNU_DOS_LEVEL = i;
		  break;
		case 'v':	//show version & exit
			printf("%s\n", prime_ver);
			exit(0); break;
		case 'h':	//show help & exit
		  printf("Prime for GNU/Linux, Version %s\n"
		    "By Mohammed Isam, 2013, 2014, 2015, 2016\n"
		    "Prime is a GNU software, part of the GnuDOS package\n"
		    "Prime is a file manager for the GNU/Linux console/xterm.\n"
		    "It was developed with ease of use and simplicity in mind.\n"
		    "\nUsage: %s [options] [dir-name]\n"
		    "\nOptions:\n"
		    "  [-e, --export] dirname filename:\n"
		    "  \texport directory tree of 'dirname' into 'filename'\n"
		    "  [-g,--log] log-filename:\n"
		    "  \tlog-filename is the name of logfile to use, instead of"
		    " '.prime.log'\n"
		    "  [-h, --help]:\n"
		    "  \tshow this help\n"
		    "  [-l,--level] GNU DOS level:\n"
		    "  \tthe GNU DOS level of experience to be used\n"
		    "  [-n, --no-log]:\n"
		    "  \tstart Prime with no log information\n"
		    "  [-r, --reset-config]:\n"
		    "  \treset the configuration file to it's defaults\n"
		    "  dir-name: is the name of directory to load into Prime at "
		    "startup\n\n",
		    prime_ver, argv[0]);
		  exit(0); break;
		case 'g':	//set log filename
		  //******Open specified file as log file
		  log_file_name = (char *) malloc(strlen(optarg));
		  if(!log_file_name) { printf("Insufficient memory\n"); exit(1); }
		  strcpy(log_file_name, optarg);
		  if(!(log_file = fopen(log_file_name, "w"))) 
		  {
		    printf("Error: couldn't open logfile '%s'.\n", log_file_name);
		    printf("Would you like to use default file 'prime.log'? [Y/n]:");
		    char c = getchar();
		    if(c == 'Y' || c == 'y' || c == '\n') 
		    {
		      if((pass = getpwuid(geteuid())) == NULL)
		      {
			printf("Error: couldn't open home directory to write log file.\n");
			printf("If you want to start Prime with no log, type '%s"
			       "--no-log'\nAborting.\n", argv[0]);
			exit(1);
		      }
		      log_file_name = (char *) malloc(strlen(pass->pw_dir)+12);
		      if(!log_file_name) { printf("Insufficient memory\n"); exit(1); }
		      strcpy(log_file_name, pass->pw_dir);
		      strcat(log_file_name, "/");
		      strcat(log_file_name, "prime.log");
		      if(!(log_file = fopen(log_file_name, "w"))) 
		      {
			printf("Fatal error: couldn't open logfile 'prime.log'. Aborting.\n");
			exit(1);
		      }
		    } 
		    else 
		    {
		      printf("Couldn't open logfile. If you want to start Prime with no "
			     "log, type '%s --no-log'\n", argv[0]);
		      exit(1);
		    }
		  }
		  LOG_FILE_SET = 1;
		  break;
		case '?':
			break;
		default:
			abort();
		}
	}
	///////////////////////////////////////
	//check for missing argument
	///////////////////////////////////////
	if(optind >= argc && EXPORT_FLAG)
	{
		printf("Error: Missing argument: filename\n"
		       "Try %s -h\n", argv[0]);
		exit(1);
	}
	///////////////////////////////////////
	//parse the remaining arguments
	///////////////////////////////////////
	while(optind < argc)
	{
	  if(EXPORT_FLAG)
	  {
		EXPORT_FILE = (char *)malloc(strlen(argv[optind]));
		if(!EXPORT_FILE) { printf("Insufficient memory\n"); exit(1); }
		strcpy(EXPORT_FILE, argv[optind]);
		exportTreeFromCommandLine(EXPORT_DIR, EXPORT_FILE);
		EXPORT_FLAG = 0;
		exit(0);
		optind++;
		continue;
	  }
	  ///////////////////////////////////////////////////
	  ///////////////////////////////////////////////////
	  ///////////////////////////////////////////////////
	  //******Start with directory dir
	  //check for '~'in dir name
	  char *tmp = (char *) malloc(MAX_DIR_NAME_LEN);
	  if(!tmp) { printf("Insufficient memory\n"); exit(1); }
	  strcpy(tmp, argv[optind]);
	  if(strchr(tmp, '~')) 
	  {
	    char *tmp2 = (char *) malloc(MAX_DIR_NAME_LEN);
	    if(!tmp2) { printf("Insufficient memory\n"); exit(1); }
	    strcpy(tmp2, argv[optind]+((strchr(tmp, '~')+1)-tmp));
	    if((pass = getpwuid(geteuid())) == NULL)
	    {
	      printf("Error: couldn't open home directory to write log file.\n");
	      printf("If you want to start Prime with no log, type '%s"
		     "--no-log'\nAborting.\n", argv[0]);
	      exit(1);
	    }
	    strcpy(tmp, pass->pw_dir);
	    strcat(tmp, "/");
	    strcat(tmp, tmp2);
	    strcat(tmp, "\0");
	    free(tmp2);
	  }
	  struct stat st;
	  if(stat(tmp, &st) == -1) 
	  {
	    printf("Directory '%s' doesn't exist.\n", tmp);
	    printf("Starting Prime from current working directory.\n");
	    printf("Press any key..");
	    char c = getchar();
	  } 
	  else
	  {
	    int z = chdir(tmp);
	    if(z == -1) { printf("Error changing directory\n"); exit(1); }
	  }
	  //continue to the rest of program startup....
	  free(tmp);
	  if(!LOG_FILE_SET)
	  {
	    //Open log file
	    if((pass = getpwuid(geteuid())) == NULL)
	    {
	      printf("Error: couldn't open home directory to write log file.\n");
	      printf("If you want to start Prime with no log, type '%s"
		     "--no-log'\nAborting.\n", argv[0]);
	      exit(1);
	    }
	    log_file_name = (char *) malloc(strlen(pass->pw_dir)+12);
	    if(!log_file_name) { printf("Insufficient memory\n"); exit(1); }
	    strcpy(log_file_name, pass->pw_dir);
	    strcat(log_file_name, "/");
	    strcat(log_file_name, "prime.log");
	    if(!(log_file = fopen(log_file_name, "w"))) 
	    {
	      printf("Fatal error: couldn't open logfile 'prime.log'.\n"
		     "If you want to start Prime with no log, type '%s --no-log'\n"
		     "Aborting.\n", argv[0]);
	      exit(1);
	    }
	  }
	  optind++;
	}
	///////////////////////////////////////
	///////////////////////////////////////
	if(!LOG_FILE_SET)
	{
	    //Open log file
	    if((pass = getpwuid(geteuid())) == NULL)
	    {
	      printf("Error: couldn't open home directory to write log file.\n");
	      printf("If you want to start Prime with no log, type '%s"
		     "--no-log'\nAborting.\n", argv[0]);
	      exit(1);
	    }
	    log_file_name = (char *) malloc(strlen(pass->pw_dir)+12);
	    if(!log_file_name) { printf("Insufficient memory\n"); exit(1); }
	    strcpy(log_file_name, pass->pw_dir);
	    strcat(log_file_name, "/");
	    strcat(log_file_name, "prime.log");
	    if(!(log_file = fopen(log_file_name, "w"))) 
	    {
	      printf("Fatal error: couldn't open logfile 'prime.log'.\n"
		     "If you want to start Prime with no log, type '%s --no-log'\n"
		     "Aborting.\n", argv[0]);
	      exit(1);
	    }
	}
}
