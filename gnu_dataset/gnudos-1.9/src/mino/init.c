/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014, 2015, 2016 (c)
 * 
 *    file: init.c
 *    This file is part of mino.
 *
 *    mino is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    (at your option) any later version.
 *
 *    mino is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with mino.  If not, see <http://www.gnu.org/licenses/>.
 */    
#include "defs.h"
#include "kbd.h"
#include "edit.h"
#include "options.h"
#include <pwd.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <getopt.h>
#include <asm/types.h>

extern int fcloseall (void);    /* stdio.h */

int NEW_GNU_DOS_LEVEL;
struct passwd *pass;//will be used to find the home dir
char *STARTUP_FILE_NAME;
char *mino_ver = "1.3";

void exit_gracefully(int exit_code) 
{
  if(FILE_STATE == MODIFIED) 
  {
      int i = msgBox("File has been changed. Save changes?", YES|NO, INFO);
      if(i == YES) 
      {//if #2
	if(NEW_FILE) 
	{//if #3
	  if(!openSaveFile(SAVE, YES)) 
	  {
	    msgBox("Failed to save file.", OK, ERROR);
	  } else { FILE_STATE = SAVED; }
	} 
	else 
	{
	  if(!openSaveFile(SAVE, NO)) 
	  {
	    msgBox("Failed to save file.", OK, ERROR);
	  } else { FILE_STATE = SAVED; }
	}//end if #3
      } 
      else if(i == CANCEL) 
      { 
	FILE_STATE = MODIFIED;
      } else { FILE_STATE = IDLE; }
    if(FILE_STATE == SAVED || FILE_STATE == IDLE)
      goto good_to_go;
    //i = msgBox("Are you sure you want to exit?", YES | NO, CONFIRM);
  } else goto good_to_go;
  refreshView();
  return;

good_to_go:
  //write_config_file();
  fcloseall();
  //remove backup file, if any
  //char *backup_file_name = (char *) malloc(strlen(open_file_name)+2);
  //strcpy(backup_file_name, open_file_name);
  //strcat(backup_file_name, "~\0");
  //remove(backup_file_name);
  //free(backup_file_name);
  setScreenColors(WHITE, BGBLACK);
  clearScreen(); 
  restoreTerminal(); 
  setScreenColors(WHITE, BGBLACK);
  fprintf(stdout, "\x1b[24m");
  fflush(stdout);
  exit(exit_code); 
}

void parseLineArgs(int argc, char **argv) 
{
  GNU_DOS_LEVEL = 1;
  NEW_FILE = 1;
  FILE_STATE = NEW;
  open_file_at_startup = 0;
  AUTO_HIGHLIGHTING = 0;
  NEW_GNU_DOS_LEVEL = 0;
  //struct passwd *pass;//will be used to find the home dir
  
	///////////////////////////////////////
	//parse command line arguments
	///////////////////////////////////////
	int c;
	//char *file_name = NULL;
	//static int EXPORT_FLAG = 0;
	//static int LOG_FILE_SET = 0;
	//char *EXPORT_DIR, *EXPORT_FILE;
	static struct option long_options[] =
	{
	 {"reset-config", no_argument,            0,  'r'},
	 {"help",         no_argument,            0,  'h'},
	 {"level",  required_argument,            0,  'l'},
	 {"version",      no_argument,            0,  'v'},
	 {0, 0, 0, 0}
	};

	while(1)
	{
		int option_index=0;
		c = getopt_long(argc, argv, "rhl:v", long_options, &option_index);
		if(c==-1) break;	//end of options

		switch(c)
		{
		case 0:
			break;
		case 'r':	//reset config file
		  if(!(pass = getpwuid(geteuid()))) 
		  {
		    printf("Error: couldn't open home directory to write "
			   "configuration file.\n");
		    printf("Aborting.\n");
		    exit(1);
		  }
		  config_file_name = (char *) malloc(strlen(pass->pw_dir)+12);
		  if(!config_file_name) { fprintf(stderr, "Insufficient memory\n"); exit(1); }
		  strcpy(config_file_name, pass->pw_dir);
		  strcat(config_file_name, "/");
		  strcat(config_file_name, ".mino.conf");
		  if(!(config_file = fopen(config_file_name, "w"))) 
		  {
		    printf("Error: couldn't write to configuration file in "
		           "home directory.\n");
		    printf("Aborting.\n");
		    exit(1);
		  }
		  printf("Resetting program configuration..\n");
		  //write default values to the configuration file
		  fprintf(config_file, "#Configuration file for mino program\n");
		  fprintf(config_file, "#Please do not modify this file by hand\n");
		  fprintf(config_file, "#Use the Options menu in mino to change"
				       " program configuration.\n\n");
		  //fprintf(config_file, "#Maximum number of dirs that can be handled in dialog boxes\n");
		  //fprintf(config_file, "MAXDIRS = 255\n");
		  //fprintf(config_file, "#Maximum number of files that can be handled in dialog boxes\n");
		  //fprintf(config_file, "MAXFILES = 255\n");
		  //fprintf(config_file, "#Maximum number of permitted lines in each document\n");
		  //fprintf(config_file, "MAXLINES = 100\n\n");
		  fprintf(config_file, "#Line Wrapping\n");
		  fprintf(config_file, "#If TRUE, lines cannot be longer than"
				       " the screen width\n");
		  fprintf(config_file, "#If FALSE, lines cannot be upto "
				       "MAX_CHARS_PER_LINE characters\n");
		  fprintf(config_file, "#Note this value is omitted if "
				       "WRAP_LINES is TRUE\n");
		  fprintf(config_file, "MAX_CHARS_PER_LINE = 100\n");
		  fprintf(config_file, "WRAP_LINES = TRUE\n\n");
		  fprintf(config_file, "#Number of spaces to insert when user"
				       " presses TAB key\n");
		  fprintf(config_file, "TAB_CHARS = 8\n\n");
		  fprintf(config_file, "#Default title for newly opened "
				       "documents, no quotes\n");
		  fprintf(config_file, "DEFAULT_TITLE = untitled\n\n");
		  fprintf(config_file, "#Start mino with CAPS and INSERT set "
				       "to OFF/ON\n");
		  fprintf(config_file, "CAPS = OFF\n");
		  fprintf(config_file, "INSERT = OFF\n\n");
		  fprintf(config_file, "#Display colors\n");
		  fprintf(config_file, "FG_COLOR_WIN = 37\n");
		  fprintf(config_file, "FG_COLOR_HLT = 34\n");
		  fprintf(config_file, "FG_COLOR_MBAR = 34\n");
		  fprintf(config_file, "FG_COLOR_SBAR = 34\n");
		  fprintf(config_file, "FG_COLOR_HBUT = 32\n");
		  fprintf(config_file, "FG_COLOR_BUT = 37\n");
		  fprintf(config_file, "BG_COLOR_WIN = 44\n");
		  fprintf(config_file, "BG_COLOR_HLT = 47\n");
		  fprintf(config_file, "BG_COLOR_MBAR = 47\n");
		  fprintf(config_file, "BG_COLOR_SBAR = 47\n");
		  fprintf(config_file, "BG_COLOR_HBUT = 41\n");
		  fprintf(config_file, "BG_COLOR_BUT = 41\n");
		  fprintf(config_file, "#Show README on startup\n");
		  fprintf(config_file, "SHOW_README\n");
		  fprintf(config_file, "#GnuDOS Level\n");
		  fprintf(config_file, "GNU_DOS_LEVEL = 1\n");
		  fprintf(config_file, "#Auto-indentation\n");
		  fprintf(config_file, "AUTO_INDENT = 1\n");
		  fclose(config_file);
		  printf("Finished writing default values to ~/.mino.conf\n");
		  exit(0);
		  break;
		case 'h':	//show program help
		  printf("\nCommand line help for mino under GNU/Linux\n");
		  printf("Copyright (C) Mohammed Isam 2014, 2015\n");
		  printf("This is a GNU software, part of the GnuDOS package\n\n");
		  printf("Usage: %s [filename][options]\n\n", argv[0]);
		  printf("Options:\n");
		  printf("\tfilename: File to be opened by mino (optional)\n");
		  printf("\t--reset-config, -r\tReset program configuration. "
			 "Writes default\n");
		  printf("\t                  \tvalues to .mino.conf file\n");
		  printf("\t--help, -h        \tShow this command line help\n");
		  printf("\t--version, -v     \tShow program version\n");
		  printf("\t--levelX, -lX     \tSet the experience level, where X is 1-6\n");
		  printf("For more information, see 'info mino' or the README"
			 " file in mino help menu\n");
		  exit(0);
		  break;
		case 'v':	//show program version
		  printf("Mino version %s\n", mino_ver);
		  exit(0);
		  break;
		case 'l':	//set GNU_DOS level
		  if(0) ;
		  int i = atoi(optarg);
		  if(i < 1 || i > 6)
		  {
			  printf("Unrecognised level. See 'man mino' or "
				 "'info mino' for information about possible"
				 " levels.\n");
			  exit(1); break;
		  }
		  NEW_GNU_DOS_LEVEL = i;
		  break;
		case '?':
			break;
		default:
			abort();
		}
	}
	///////////////////////////////////////
	//parse the remaining arguments
	///////////////////////////////////////
	while(optind < argc)
	{
	  NEW_FILE = 0; FILE_STATE = OPENED;
	  open_file_at_startup = 1;
	  STARTUP_FILE_NAME = (char *)malloc(strlen(argv[optind]));
	  if(!STARTUP_FILE_NAME) { fprintf(stderr, "Insufficient memory\n"); exit(1); }
	  strcpy(STARTUP_FILE_NAME, argv[optind]);
	  break;
	}
}//end parseLineArgs()

void init_error() 
{
      printf("Error: couldn't open home directory and read configuration file.\n");
      printf("Will use built-in defaults. This means if you have set any\n");
      printf("preferences (for example, display colors), they will be of\n");
      printf("no effect.\n");
      printf("You can reset your config file to program defaults by invoking:\n");
      printf("mino --reset-config\n\nPress any key to continue..\n");
      int ch = getchar();
      init_built_in_defaults();
}

/***************************************
 * init(): 
 * Procedure containing code that init-
 * ializes various data structures.
 * **************************************/
void init() 
{
  int i;
  AUTO_INDENT = 1;
  for(i=0; i<100; i++) AUTO_INDENT_STR[i] = '\0';
  /////////////////////////////////////
  
    //pass = malloc(sizeof(struct passwd));
    //struct passwd *pass;//will be used to find the home dir
    if(!(pass = getpwuid(geteuid()))) 
    {
      init_error();
      return;
    }
    config_file_name = (char *) malloc(strlen(pass->pw_dir)+11);
    if(!config_file_name) { fprintf(stderr, "Insufficient memory\n"); exit(1); }
    strcpy(config_file_name, pass->pw_dir);
    strcat(config_file_name, "/");
    strcat(config_file_name, ".mino.conf");
    if(!(config_file = fopen(config_file_name, "r"))) 
    {
      //init_error();
      init_built_in_defaults();
      return;
    }

  MAX_FILE_NAME_LEN = 100;
  getScreenSize();
  //1. Main Menu//
  menu[0] = "&File";
  menu[1] = "&Edit";
  menu[2] = "&Options";
  menu[3] = "&Help";
  //2. File Menu//
  fileMenu[0] = "New..         ^N";
  fileMenu[1] = "Open file     ^O";
  fileMenu[2] = "Save file     ^S";
  fileMenu[3] = "Save as..       ";
  fileMenu[4] = "Print         ^P";
  fileMenu[5] = "Exit          ^Q";
  //3. Edit Menu//
  editMenu[0] = "Cut                ^X";
  editMenu[1] = "Copy               ^C";
  editMenu[2] = "Paste              ^V";
  editMenu[3] = "Select all         ^A";
  editMenu[4] = "Undo               ^Z";
  editMenu[5] = "Redo               ^Y";
  editMenu[6] = "Delete Line        ^D";
  editMenu[7] = "Find..             ^F";
  editMenu[8] = "Replace..          ^R";
  editMenu[9] = "Toggle select mode ^E";
  //4. Options Menu//
  optionsMenu[0] = "Change colors  ";
  optionsMenu[1] = "Tab spaces     ";
  optionsMenu[2] = "Autoindent     ";
  optionsMenu[3] = "Reset config   ";
  //5. Help Menu//
  helpMenu[0] = "View README    ";
  helpMenu[1] = "GNU Keybindings";
  helpMenu[2] = "Quick reference";
  helpMenu[3] = "About Mino..   ";

  //initiate color arrays
  COLOR_STR[0] = "BLACK";
  COLOR_STR[1] = "RED";
  COLOR_STR[2] = "GREEN";
  COLOR_STR[3] = "BROWN";
  COLOR_STR[4] = "BLUE";
  COLOR_STR[5] = "MAGENTA";
  COLOR_STR[6] = "CYAN";
  COLOR_STR[7] = "WHITE";
  FG_COLOR_ARRAY[0] = 30;
  FG_COLOR_ARRAY[1] = 31;
  FG_COLOR_ARRAY[2] = 32;
  FG_COLOR_ARRAY[3] = 33;
  FG_COLOR_ARRAY[4] = 34;
  FG_COLOR_ARRAY[5] = 35;
  FG_COLOR_ARRAY[6] = 36;
  FG_COLOR_ARRAY[7] = 37;
  BG_COLOR_ARRAY[0] = 40;
  BG_COLOR_ARRAY[1] = 41;
  BG_COLOR_ARRAY[2] = 42;
  BG_COLOR_ARRAY[3] = 43;
  BG_COLOR_ARRAY[4] = 44;
  BG_COLOR_ARRAY[5] = 45;
  BG_COLOR_ARRAY[6] = 46;
  BG_COLOR_ARRAY[7] = 47;
 SHOW_README = 0;

 char buf[100];
 //read configuration file
 while(fgets(buf, sizeof(buf), config_file)) 
 {
   if(buf[0] == '#' || buf[0] == '\n') continue;
/*   if(strstr(buf, "MAXDIRS")) {
     MAXDIRS = atoi((strchr(buf, "=")+2));
   } else if (strstr(buf, "MAXFILES")) {
     MAXFILES = atoi((strchr(buf, "=")+2));
   } else if (strstr(buf, "MAXLINES")) {
     MAX_LINES = atoi((strchr(buf, "=")+2));
   } else*/
   if (strstr(buf, "MAX_CHARS_PER_LINE")) 
   {
     MAX_CHARS_PER_LINE = atoi((strchr(buf, '=')+2));
   }
   else if (strstr(buf, "GNU_DOS_LEVEL")) 
   {
     GNU_DOS_LEVEL = atoi((strchr(buf, '=')+2));
   }
   else if (strstr(buf, "TAB_CHARS")) 
   {
     TAB_CHARS = atoi(strchr(buf, '=')+2);
     if(TAB_CHARS < 1 || TAB_CHARS > SCREEN_W-3) TAB_CHARS = 8;
   }
   else if (strstr(buf, "DEFAULT_TITLE")) 
   {
     DEFAULT_TITLE = (char *)malloc(MAX_FILE_NAME_LEN);
     if(!DEFAULT_TITLE) { fprintf(stderr, "Insufficient memory\n"); exit(1); }
     strcpy(DEFAULT_TITLE, strchr(buf, '=')+2);
     DEFAULT_TITLE[strlen(DEFAULT_TITLE)-1] = '\0';
   }
   else if (strstr(buf, "WRAP_LINES")) 
   {
     if(strstr(buf, "TRUE")) WRAP_LINES = 1;
     else WRAP_LINES = 0;
   }
   else if (strstr(buf, "CAPS")) 
   {
     if(strstr(buf, "OFF")) CAPS = 0;
     else CAPS = 1;
   }
   else if (strstr(buf, "INSERT")) 
   {
     if(strstr(buf, "OFF")) INSERT = 0;
     else INSERT = 1;
   }
   else if (strstr(buf, "FG_COLOR_WIN")) 
   {
     FG_COLOR[COLOR_WINDOW] = atoi(strchr(buf, '=')+2);
   }
   else if (strstr(buf, "FG_COLOR_HLT")) 
   {
     FG_COLOR[COLOR_HIGHLIGHT_TEXT] = atoi(strchr(buf, '=')+2);
   }
   else if (strstr(buf, "FG_COLOR_MBAR")) 
   {
     FG_COLOR[COLOR_MENU_BAR] = atoi(strchr(buf, '=')+2);
   }
   else if (strstr(buf, "FG_COLOR_SBAR")) 
   {
     FG_COLOR[COLOR_STATUS_BAR] = atoi(strchr(buf, '=')+2);
   }
   else if (strstr(buf, "FG_COLOR_HBUT")) 
   {
     FG_COLOR[COLOR_HBUTTONS] = atoi(strchr(buf, '=')+2);
   }
   else if (strstr(buf, "FG_COLOR_BUT")) 
   {
     FG_COLOR[COLOR_BUTTONS] = atoi(strchr(buf, '=')+2);
   }
   else if (strstr(buf, "BG_COLOR_WIN")) 
   {
     BG_COLOR[COLOR_WINDOW] = atoi(strchr(buf, '=')+2);
   }
   else if (strstr(buf, "BG_COLOR_HLT")) 
   {
     BG_COLOR[COLOR_HIGHLIGHT_TEXT] = atoi(strchr(buf, '=')+2);
   }
   else if (strstr(buf, "BG_COLOR_MBAR")) 
   {
     BG_COLOR[COLOR_MENU_BAR] = atoi(strchr(buf, '=')+2);
   }
   else if (strstr(buf, "BG_COLOR_SBAR")) 
   {
     BG_COLOR[COLOR_STATUS_BAR] = atoi(strchr(buf, '=')+2);
   }
   else if (strstr(buf, "BG_COLOR_HBUT")) 
   {
     BG_COLOR[COLOR_HBUTTONS] = atoi(strchr(buf, '=')+2);
   }
   else if (strstr(buf, "BG_COLOR_BUT")) 
   {
     BG_COLOR[COLOR_BUTTONS] = atoi(strchr(buf, '=')+2);
   }
   else if (strstr(buf, "SHOW_README")) 
   {
     SHOW_README = 1;
   }
   else if (strstr(buf, "AUTO_INDENT")) 
   {
     AUTO_INDENT = atoi(strchr(buf, '=')+2);
  }//end if
 }//end while
 
 fclose(config_file);
 free(config_file_name);

 
 if(AUTO_INDENT) optionsMenu[2] = strdup("Autoindent    *");
 else optionsMenu[2] = strdup("Autoindent     ");
 //else optionsMenu[2][14] = ' ';
 
 old_window_color = BG_COLOR[COLOR_WINDOW];
 if(NEW_GNU_DOS_LEVEL)
   GNU_DOS_LEVEL = NEW_GNU_DOS_LEVEL;
 if(GNU_DOS_LEVEL > 5 || GNU_DOS_LEVEL < 1) GNU_DOS_LEVEL = 1;
 
 //set defaults for Dir view//
 numVisDirs = SCREEN_H-9;
 firstVisDir = 0;
 selectedDir = -1;
 totalDirs = 0;

 MAX_DIR_NAME_LEN = (SCREEN_W/2)-8;
 MAX_MSG_BOX_W = SCREEN_W - 4;
 MAX_MSG_BOX_H = SCREEN_H - 4;
 
 for(i = 0; i < MAXDIRS; i++) 
 {
   dirs[i] = (char *) malloc(MAX_DIR_NAME_LEN);
   if(!dirs[i]) { fprintf(stderr, "Insufficient memory\n"); exit(1); }
 }
 for(i = 0; i < MAXFILES; i++) 
 {
   files[i] = (char *) malloc(MAX_DIR_NAME_LEN);
   if(!files[i]) { fprintf(stderr, "Insufficient memory\n"); exit(1); }
 }
 
 if(WRAP_LINES) MAX_CHARS_PER_LINE = SCREEN_W-2;
 
 if(!DEFAULT_TITLE) 
 { 
   DEFAULT_TITLE = (char *) malloc(MAX_FILE_NAME_LEN*4);
   if(!DEFAULT_TITLE) { fprintf(stderr, "Insufficient memory\n"); exit(1); }
   strcpy(DEFAULT_TITLE, "untitled");
 }
 if(!documentTitle) documentTitle = (char *) malloc(MAX_FILE_NAME_LEN*4);
 if(!documentTitle) { fprintf(stderr, "Insufficient memory\n"); exit(1); }
 open_file_name = (char *) malloc(MAX_FILE_NAME_LEN*4);
 if(!open_file_name) { fprintf(stderr, "Insufficient memory\n"); exit(1); }
 if(NEW_FILE) 
 {
   strcpy(documentTitle, DEFAULT_TITLE);
   for(i = 0; i < MAX_LINES; i++) 
   {
    lines[i] = (char *) malloc((MAX_CHARS_PER_LINE+TAB_CHARS)*4);
    if(!lines[i]) { fprintf(stderr, "Insufficient memory\n"); exit(1); }
    strcpy(lines[i], "\0");
    totalCharsInLine[i] = 0;
    LINE_IS_LINKED[i] = 0;
   }//end for
   totalLines = 1;
   total_tabs = 0;
 } 
 else
 {
   if(!open_file_at_startup) strcpy(documentTitle, open_file_name);
 }
 
 selectedLine = 0;
 selectedChar = 0;
 selectedCharCarry = 0;
 totalVisLines = SCREEN_H-4;
 firstVisLine = 0;
 //WRAP_LINES = 1;//TRUE -- restrict line width to (screen width-2)
 //TAB_CHARS = 8;	//define 8 spaces in a tab
 SELECTING = 0;
 CLIPBOARD_IS_EMPTY = 1;
 last_undo_action = -1;
 for(i = 0; i < MAX_UNDO_ACTIONS; i++) 
 {
   undo_text[i] = 0; //(char *) malloc(MAX_CHARS_PER_LINE*10);
   //if(!undo_text[i]) { fprintf(stderr, "Insufficient memory\n"); exit(1); }
   undo_text_replace[i] = 0; //(char *) malloc(MAX_CHARS_PER_LINE*10);
   //if(!undo_text_replace[i]) { fprintf(stderr, "Insufficient memory\n"); exit(1); }
 }
 
 if(!initTerminal()) 
 {
   printf("Fatal error: Couldn't initialize terminal.\nAborting.\n");
   exit(1);
 }
 catchSignals();
}

/***************************************
 * init_built_in_defaults(): 
 * Procedure containing code that init-
 * ializes various data structures to
 * default values, used in case there 
 * was an error opening the config
 * file from home directory.
 * **************************************/
void init_built_in_defaults() 
{
  getScreenSize();
  //1. Main Menu//
  menu[0] = "&File";
  menu[1] = "&Edit";
  menu[2] = "&Options";
  menu[3] = "&Help";
  //2. File Menu//
  fileMenu[0] = "New..         ^N";
  fileMenu[1] = "Open file     ^O";
  fileMenu[2] = "Save file     ^S";
  fileMenu[3] = "Save as..       ";
  fileMenu[4] = "Print         ^P";
  fileMenu[5] = "Exit          ^Q";
  //3. Edit Menu//
  editMenu[0] = "Cut                ^X";
  editMenu[1] = "Copy               ^C";
  editMenu[2] = "Paste              ^V";
  editMenu[3] = "Select all         ^A";
  editMenu[4] = "Undo               ^Z";
  editMenu[5] = "Redo               ^Y";
  editMenu[6] = "Delete Line        ^D";
  editMenu[7] = "Find..             ^F";
  editMenu[8] = "Replace..          ^R";
  editMenu[9] = "Toggle select mode ^E";
  //4. Options Menu//
  optionsMenu[0] = "Change colors  ";
  optionsMenu[1] = "Tab spaces     ";
  optionsMenu[2] = "Autoindent    *";
  optionsMenu[3] = "Reset config   ";
  
  //5. Help Menu//
  helpMenu[0] = "View README    ";
  helpMenu[1] = "GNU Keybindings";
  helpMenu[2] = "Quick reference";
  helpMenu[3] = "About Mino..   ";
  //initialize color arrays
  FG_COLOR_ARRAY[0] = 30;
  FG_COLOR_ARRAY[1] = 31;
  FG_COLOR_ARRAY[2] = 32;
  FG_COLOR_ARRAY[3] = 33;
  FG_COLOR_ARRAY[4] = 34;
  FG_COLOR_ARRAY[5] = 35;
  FG_COLOR_ARRAY[6] = 36;
  FG_COLOR_ARRAY[7] = 37;
  BG_COLOR_ARRAY[0] = 40;
  BG_COLOR_ARRAY[1] = 41;
  BG_COLOR_ARRAY[2] = 42;
  BG_COLOR_ARRAY[3] = 43;
  BG_COLOR_ARRAY[4] = 44;
  BG_COLOR_ARRAY[5] = 45;
  BG_COLOR_ARRAY[6] = 46;
  BG_COLOR_ARRAY[7] = 47;
  FG_COLOR[COLOR_WINDOW] = 37;
  FG_COLOR[COLOR_HIGHLIGHT_TEXT] = 34;
  FG_COLOR[COLOR_MENU_BAR] = 34;
  FG_COLOR[COLOR_STATUS_BAR] = 34;
  FG_COLOR[COLOR_BUTTONS] = 37;
  FG_COLOR[COLOR_HBUTTONS] = 32;
  BG_COLOR[COLOR_WINDOW] = 44;
  BG_COLOR[COLOR_HIGHLIGHT_TEXT] = 47;
  BG_COLOR[COLOR_MENU_BAR] = 47;
  BG_COLOR[COLOR_STATUS_BAR] = 47;
  BG_COLOR[COLOR_BUTTONS] = 41;
  BG_COLOR[COLOR_HBUTTONS] = 41;
 old_window_color = BG_COLOR[COLOR_WINDOW];

 AUTO_INDENT = 1;
 GNU_DOS_LEVEL = 1;
 int i;
 for(i=0; i<100; i++) AUTO_INDENT_STR[i] = '\0';
 //set defaults for Dir view//
 numVisDirs = SCREEN_H-9;
 firstVisDir = 0;
 selectedDir = -1;
 totalDirs = 0;
 
 MAX_FILE_NAME_LEN = 100;
 MAX_DIR_NAME_LEN = (SCREEN_W/2)-8;
 MAX_MSG_BOX_W = SCREEN_W - 4;
 MAX_MSG_BOX_H = SCREEN_H - 4;
 for(i = 0; i < MAXDIRS; i++) 
 {
   dirs[i] = (char *) malloc(MAX_DIR_NAME_LEN);
   if(!dirs[i]) { fprintf(stderr, "Insufficient memory\n"); exit(1); }
 }
 for(i = 0; i < MAXFILES; i++) 
 {
   files[i] = (char *) malloc(MAX_DIR_NAME_LEN);
   if(!files[i]) { fprintf(stderr, "Insufficient memory\n"); exit(1); }
 }
 MAX_CHARS_PER_LINE = SCREEN_W-2;
 selectedLine = 0;
 selectedChar = 0;
 selectedCharCarry = 0;
 totalLines = 1;
 totalVisLines = SCREEN_H-4;
 firstVisLine = 0;
 WRAP_LINES = 1;//TRUE -- restric line width to (screen width-2)
 TAB_CHARS = 8;	//define 8 spaces in a tab

 if(!documentTitle) documentTitle = (char *) malloc(MAX_FILE_NAME_LEN*4);
 if(!documentTitle) { fprintf(stderr, "Insufficient memory\n"); exit(1); }
 if(!DEFAULT_TITLE) DEFAULT_TITLE = (char *) malloc(MAX_FILE_NAME_LEN*4);
 if(!DEFAULT_TITLE) { fprintf(stderr, "Insufficient memory\n"); exit(1); }
 strcpy(DEFAULT_TITLE, "untitled");
 open_file_name = (char *) malloc(MAX_FILE_NAME_LEN*4);
 if(!open_file_name) { fprintf(stderr, "Insufficient memory\n"); exit(1); }
 if(NEW_FILE) 
 {
   strcpy(documentTitle, DEFAULT_TITLE);
   for(i = 0; i < MAX_LINES; i++) 
   {
    lines[i] = (char *) malloc((MAX_CHARS_PER_LINE+TAB_CHARS)*4);
    if(!lines[i]) { fprintf(stderr, "Insufficient memory\n"); exit(1); }
    strcpy(lines[i], "\0");
    totalCharsInLine[i] = 0;
    LINE_IS_LINKED[i] = 0;
   }//end for
   totalLines = 1;
   total_tabs = 0;
 } 
 else 
 {
   if(!open_file_at_startup) strcpy(documentTitle, open_file_name);
 }
 
 //MAXDIRS = 255;
 //MAXFILES = 255;
 //MAX_LINES = 100;
 CAPS = 0;
 INSERT = 0;
 SELECTING = 0;
 CLIPBOARD_IS_EMPTY = 1;
 last_undo_action = -1;
 RECORDING_UNDO_ACTION = 0;
 for(i = 0; i < MAX_UNDO_ACTIONS; i++) 
 {
   undo_text[i] = 0;//(char *) malloc(MAX_CHARS_PER_LINE*10);
   //if(!undo_text[i]) { fprintf(stderr, "Insufficient memory\n"); exit(1); }
   undo_text_replace[i] = 0;//(char *) malloc(MAX_CHARS_PER_LINE*10);
   //if(!undo_text_replace[i]) { fprintf(stderr, "Insufficient memory\n"); exit(1); }
 }

 if(!initTerminal()) 
 {
   printf("Fatal error: Couldn't initialize terminal.\nAborting.\n");
   exit(1);
 }
 catchSignals();
 //AUTO_HIGHLIGHTING = 0;
 //loadKeywords();
 SHOW_README = 1;
  write_config_file();
 //int ch = getchar();
}

void showREADMEOnStartup() 
{
  int x = SCREEN_H/2-3;
  int y = SCREEN_W/2-20;
  int w = y+40;
  int h = x+5;
  
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  drawBox(x, y, h, w, " Welcome to mino ", YES);
  printf("\e[%d;%dH", x+1, y+2);
  printf("Welcome to mino!");
  printf("\e[%d;%dH", x+2, y+2);
  printf("README file will be shown to help you");
  printf("\e[%d;%dH", x+3, y+2);
  printf("start using mino.");
  setScreenColors(FG_COLOR[COLOR_HBUTTONS], BG_COLOR[COLOR_HBUTTONS]);
  printf("\e[%d;%dH", x+4, y+2);
  printf("  OK  ");
  setScreenColors(FG_COLOR[COLOR_BUTTONS], BG_COLOR[COLOR_BUTTONS]);
  printf("\e[%d;%dH", x+4, y+10);
  printf(" Don't show README again ");
  printf("\e[%d;%dH", x+4, y+2);
  fflush(stdout);
  int sel = 0;
  char *c = (char *)malloc(5);
  while((c = getKey())) 
  {
    switch(c[0]) 
    {
      case(RIGHT_KEY):
      case(LEFT_KEY):
      case(TAB_KEY):
	if(sel == 0) 
	{
	  setScreenColors(FG_COLOR[COLOR_BUTTONS], BG_COLOR[COLOR_BUTTONS]);
	  printf("\e[%d;%dH", x+4, y+2);
	  printf("  OK  ");
	  setScreenColors(FG_COLOR[COLOR_HBUTTONS], BG_COLOR[COLOR_HBUTTONS]);
	  printf("\e[%d;%dH", x+4, y+10);
	  printf(" Don't show README again ");
	  printf("\e[%d;%dH", x+4, y+10);
	  sel = 1;
	} 
	else 
	{
	  setScreenColors(FG_COLOR[COLOR_HBUTTONS], BG_COLOR[COLOR_HBUTTONS]);
	  printf("\e[%d;%dH", x+4, y+2);
	  printf("  OK  ");
	  setScreenColors(FG_COLOR[COLOR_BUTTONS], BG_COLOR[COLOR_BUTTONS]);
	  printf("\e[%d;%dH", x+4, y+10);
	  printf(" Don't show README again ");
	  printf("\e[%d;%dH", x+4, y+2);
	  sel = 0;
	} break;
      case(ENTER_KEY):
      case(SPACE_KEY):
	if(sel == 0) SHOW_README = 1;
	else SHOW_README = 0;
	showReadMe();
	write_config_file();
	refreshView();
	return;
    }//end switch
    fflush(stdout);
  }//end while
}
