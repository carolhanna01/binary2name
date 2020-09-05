/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2013, 2014, 2015, 2016 (c)
 * 
 *    file: main.c
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
#include "../corelib/dialogs.h"
#include "defs.h"
#include "edit.h"
#include "options.h"
//#include "print.h"
#include "find.h"
#include "cutcopy.h"
#include <stdio.h>
#include <string.h>
#include <sys/ioctl.h>
#include <dirent.h>
#include <sys/stat.h>
#include <unistd.h>
#include <poll.h>
#include <signal.h>
#include <termios.h>
#include <sys/ioctl.h>
#include <stdlib.h>
#include <pwd.h>
#include <time.h>

extern int fcloseall (void);    /* stdio.h */
extern char * strcasestr (const char *HAYSTACK, const char *NEEDLE);    /* string.h */

extern void showPropertiesDialog();

char *tmp, *tmp2;
char *input_str;

struct passwd *pass;//will be used to find the home dir
time_t tm;
static sig_atomic_t end = 0;

static int one(const struct dirent *unused);

int read_config_file();
int write_config_file_defaults();
int NEW_GNU_DOS_LEVEL = 0;

//extern int PRINTING;
//extern void showPrintDialogBox();
//extern void freePrinters();
//extern int findInstalledPrinters();

extern void parse_args(int argc, char **argv);

void exit_gracefully()
{
  fprintf(log_file, "Restoring terminal [ OK ]..\n");
  restoreTerminal(); 
  fprintf(log_file, "Closing files and Exiting Prime [ OK ]..\n");
  setScreenColors(WHITE, BGDEFAULT);
  clearScreen(); 
  showCursor();
  fflush(stdout);
  time_t tm = time(NULL);
  fprintf(log_file, "System time and date: %s", ctime(&tm));
  write_config_file();
  fcloseall();
  //freePrinters();
  setScreenColors(WHITE, BGDEFAULT);
  clearScreen(); 
  fprintf(stdout, "\x1b[0m");	/* reset settings */
  exit(0);
}

/***************************************
 * sighandler(): 
 * Procedure that sets trap to the system
 * signals that are issued when user 
 * presses CTRL-C or CTRL-Z...
 * *************************************/
void sighandler(int signo)
{
    fprintf(log_file, "SIGNAL %d received\n", signo);
    if(signo == 2) 
    {	//CTRL-C pressed
      copyMarked();
      //redraw main window but don't clear the area
      drawBox(1, 1, SCREEN_H, SCREEN_W, " Prime  for GNU/Linux ", NO);
    } 
    else if(signo == SIGTSTP) 
    { //CTRL-Z pressed
      int i = msgBox("Are you sure you want to exit?", YES | NO, CONFIRM);
      if(i == YES) 
      { 	//exit gracefully
	exit_gracefully();
      } 
      else
      {
	refreshWindows();
      }//end inner if
    } 
    else 
    {
      end = 1;
    }//end outer if
}//end sighandler

struct termios oldtio, curtio;
struct sigaction sa;

void print_dir_highlight(int selectedDir)
{
  int j;
  fprintf(stdout, "\x1b[%d;%dH%s", selectedDir+4, 3, dirHighLight);
  if(strlen(dirs[firstVisDir+selectedDir]) >= MAX_DIR_NAME_LEN)
  {
    fprintf(stdout, "\x1b[%d;%dH%c", selectedDir+4, 3,
		dirStar[firstVisDir+selectedDir]);
    for(j = 0; j < MAX_DIR_NAME_LEN-3; j++)
      fprintf(stdout, "%c", dirs[firstVisDir+selectedDir][j]);
    fprintf(stdout, "..");
  }
  else
  {
    fprintf(stdout, "\x1b[%d;%dH%c%s", selectedDir+4, 3, 
		dirStar[firstVisDir+selectedDir], 
		dirs[firstVisDir+selectedDir]);
  }//end if
}

void print_file_highlight(int selectedFile)
{
  int j;
  fprintf(stdout, "\x1b[%d;%dH%s", selectedFile+4, (SCREEN_W/2)+1, fileHighLight);
  if(strlen(files[firstVisFile+selectedFile]) >= MAX_DIR_NAME_LEN) 
  {
    fprintf(stdout, "\x1b[%d;%dH%c", selectedFile+4, (SCREEN_W/2)+1, 
	    fileStar[firstVisFile+selectedFile]);
    for(j = 0; j < MAX_DIR_NAME_LEN-3; j++) 
      fprintf(stdout, "%c", files[firstVisFile+selectedFile][j]);
    fprintf(stdout, "..");
  } 
  else 
  { 
    fprintf(stdout, "\x1b[%d;%dH%c%s", selectedFile+4, (SCREEN_W/2)+1, 
	    fileStar[firstVisFile+selectedFile], 
	    files[firstVisFile+selectedFile]);
  }//end if
}

/***************************************
 * main(): 
 * Main program loop.
 * *************************************/
int main(int argc, char **argv) 
{
 parse_args(argc, argv);
 
 cwd = getcwd(NULL, 0);
 tm = time(NULL);
 if(!log_file)
 {
   printf("Couldn't open logfile. If you want to start Prime with no "
	  "log, type '%s --no-log'\n", argv[0]);
   exit(1);
 }
 fprintf(log_file, "Starting Prime in: %s\n", cwd);
 fprintf(log_file, "System time and date: %s", ctime(&tm));
 fprintf(log_file, "Log_file: %s\n", log_file_name);
 fprintf(log_file, "Program started: in the main function..\n");
 fprintf(log_file, "Initializing variables [ OK ]..\n");
 init();			//some initialization code
 
 fprintf(log_file, "Reading configuration file [ OK ]..\n");
 read_config_file();
 
 clearScreen();			//clear the screen
 setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
 //draw main window
 drawBox(1, 1, SCREEN_H, SCREEN_W, " Prime for GNU/Linux ", YES);
 drawMenuBar(2, 2, SCREEN_W-2); //draw main menu bar

 //draw right sub-window
 drawBox(3, (int)(SCREEN_W/2), SCREEN_H-5, SCREEN_W-1, " File view ", YES);
 //draw bottom sub-window
 drawBox(SCREEN_H-4, 2, SCREEN_H-1, SCREEN_W-1, " Quick functions ", YES);
 //draw left sub-window
 drawBox(3, 2, SCREEN_H-5, (int)(SCREEN_W/2)-1, " Directory view ", YES);

 //redraw main window but don't clear the area
 drawBox(1, 1, SCREEN_H, SCREEN_W, " Prime for GNU/Linux ", NO);
 fprintf(log_file, "Setting windows and main menu [ OK ]..\n");

 //CHECK FOR PRINTERS ON SYSTEM
 /*fprintf(log_file, "Checking for installed printers");
 if(findInstalledPrinters() != 0) {
  fprintf(log_file, " [ERROR]: Couldn't find printers..\n");
 } else fprintf(log_file, " [ OK ]..\n");
 */
 refreshBottomView();
 fprintf(log_file, "Scanning current working directory [ OK ]..\n");
 scanDir(".");

 int ch;
 while(!end) 
 {	//infinite program loop//
  fprintf(log_file, "Main loop: Waiting for user input..\n");
  ch = getKey();
  switch(ch) 
  {
   case(DEL_KEY):
     if(GNU_DOS_LEVEL > 3) break;
do_del:
     if(numStarred == 0) 
     {
       if(activeWindow == DIR_WIN) 
       {
	 int k = msgBox("Are you sure you want to delete\n"
			"this directory and all its "
			"subdirectories?", YES|NO, CONFIRM);
	 if(k == YES) 
	 {
	   deleteThisDir(dirs[firstVisDir+selectedDir], 0);
	 }
       } 
       else 
       { 
	 int j = msgBox("Are you sure you want to delete this file?", 
			YES|NO, CONFIRM);
	 if(j == YES) remove(files[firstVisFile+selectedFile]);
       }//end inner if/else
     } 
     else 
     {//end outer if
       deleteMarked();
     }//end outer else
     setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
     scanDir(cwd);
     break;
   case(UP_KEY):
     if(GNU_DOS_LEVEL > 1) break;
do_up:
     if(activeWindow == DIR_WIN) 
     {
       selectedDir--; 
       if(selectedDir < 0) 
       {
	 selectedDir = 0;
	 if(firstVisDir > 0) { firstVisDir--; refreshDirView(); }
       } 
       else 
       {
	//int j;
	setScreenColors(FILE_DIR_COLOR[dirType[firstVisDir+selectedDir+1]-'a'], BG_COLOR[COLOR_WINDOW]);
	print_dir_highlight(selectedDir+1);
	setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	print_dir_highlight(selectedDir);
       }//end if
       fprintf(stdout, "\e[3;3H");	//move cursor to point to window
       fflush(stdout);
     } 
     else 
     {
       //int j;
       if(strcmp(files[0], "(Empty folder)") == 0) break;//this dir is empty!!
       selectedFile--;
       if(selectedFile < 0) 
       {
	 selectedFile = 0;
	 if(firstVisFile > 0) { firstVisFile--; refreshFileView(); }
       } 
       else 
       {
	setScreenColors(FILE_DIR_COLOR[fileType[firstVisFile+selectedFile+1]-'a'], BG_COLOR[COLOR_WINDOW]);
	print_file_highlight(selectedFile+1);
	setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	print_file_highlight(selectedFile);
       }//end if
       fprintf(stdout, "\e[3;%dH",(SCREEN_W/2)+2);//cursor to point to window
       fflush(stdout);
     } 
     break;
   case(DOWN_KEY):
     if(GNU_DOS_LEVEL > 1) break;
do_down:
     if(activeWindow == DIR_WIN) 
     {
       selectedDir++;
       //if totalDirs is less than numVisDirs
       if(selectedDir >= totalDirs) selectedDir--;
       if(selectedDir >= numVisDirs) 
       {
	 selectedDir = numVisDirs-1;
	 if((firstVisDir+numVisDirs) < totalDirs) 
	 { 
	   firstVisDir++; refreshDirView(); 
	}
       } 
       else 
       {
	//int j;
	setScreenColors(FILE_DIR_COLOR[dirType[firstVisDir+selectedDir-1]-'a'], BG_COLOR[COLOR_WINDOW]);
	print_dir_highlight(selectedDir-1);
	setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	print_dir_highlight(selectedDir);
       }//end if
       fprintf(stdout, "\e[3;3H");	//move cursor to point to window
       fflush(stdout);
     } 
     else 
     {
       if(strcmp(files[0], "(Empty folder)") == 0) break;//this dir is empty!!
       selectedFile++;
       //if totalFiles is less than numVisFiles
       if(selectedFile >= totalFiles) selectedFile--;
       if(selectedFile >= numVisFiles) {
	 selectedFile = numVisFiles-1;
	 if((firstVisFile+numVisFiles) < totalFiles) 
	 { 
	   firstVisFile++; refreshFileView(); 
	}
       } 
       else 
       {
	//int j;
	setScreenColors(FILE_DIR_COLOR[fileType[firstVisFile+selectedFile-1]-'a'], BG_COLOR[COLOR_WINDOW]);
	print_file_highlight(selectedFile-1);
	setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	print_file_highlight(selectedFile);
       }//end if
       fprintf(stdout, "\e[3;%dH",(SCREEN_W/2)+2);//cursor to point to window
       fflush(stdout);
     } 
     break;
   case(TAB_KEY):	//toggle active window with TAB press
     if(activeWindow == DIR_WIN) 
	activeWindow = FILE_WIN;
     else 
	activeWindow = DIR_WIN;
     refreshFileView();
     refreshDirView();
     break;
   case(ENTER_KEY):
     if(ALT) 
     {
       if(activeWindow == FILE_WIN && strcmp(files[0], "(Empty folder)") == 0) break;
       showPropertiesDialog();
       setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
       scanDir(cwd);
       break;
     }
     else
     {
      if(activeWindow == DIR_WIN) 
      {	//navigate to the selected directory
        scanDir(dirs[firstVisDir+selectedDir]);
      }
      else
      {
       if(strcmp(files[0], "(Empty folder)"))
       {
           showPropertiesDialog();
           setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
           scanDir(cwd);
       }
      }
      break;
     }
   case(SPACE_KEY):		//toggle select/unselect file or directory
     toggleSelected();
     break;
   case('s'):
     if(CTRL) 
     {
      if(GNU_DOS_LEVEL <= 4) break;
      findFile();
      hideCursor();
      setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
      scanDir(cwd);
     } 
     else goto insert_char;
     break;
   case('f'):
     if(ALT) 
     {
      //if(GNU_DOS_LEVEL > 1) break;
      //fprintf(log_file, "Opening file menu..\n");
      showFileMenu(YES);
      //fprintf(log_file, "Closed file menu, Waiting for user input..\n");
      break;
     } 
     else if(CTRL) 
     {
      if(GNU_DOS_LEVEL > 4) break;
      findFile();
      setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
      scanDir(cwd);
     } 
     else goto insert_char;
     break;
   case('e'):
     if(CTRL) 
     {
       if(GNU_DOS_LEVEL > 2) goto do_end;
      exportTree(YES);
      setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
      scanDir(cwd);
     }
     else if(ALT) 
     {
      //fprintf(log_file, "Opening edit menu..\n");
      showEditMenu(YES);
      //fprintf(log_file, "Closed edit menu, Waiting for user input..\n");
      break;
     } 
     else goto insert_char;
     break;
   case('h'):
     if(ALT) 
     {
      //fprintf(log_file, "Opening help menu..\n");
      showHelpMenu(YES);
      //fprintf(log_file, "Closed help menu, Waiting for user input..\n");
     } 
     else goto insert_char;
     break;
   case('x'):
     if(CTRL) 
     {
      if(GNU_DOS_LEVEL > 4)
      {
	int k=(int)SCREEN_H/2;
	int l=(int)SCREEN_W/2;
	drawBox(k-1, l-23, k+1, l+23, NULL, YES);
	setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	fprintf(stdout, "\e[%d;%dH", k, l-22);
	fprintf(stdout, "[C-c] Quit  [C-f] Open location [C-g] Cancel");
	int loop = 1;
	while(loop)
	{
	  ch = getKey();
	  if(ch == 'c' && CTRL) goto do_exit;
	  else if(ch == 'f' && CTRL) goto do_open_location;
	  else if(ch == 'g' && CTRL)
	  {
	    refreshWindows();
	    loop = 0;
	    break;
	  }
	}//end while
      }
      else cutMarked();			//found in edit.c
     } 
     else goto insert_char;
     break;
   case('v'):
     if(CTRL) 
     {
       if(GNU_DOS_LEVEL > 3) break;
       pasteMarked();
     } 
     else goto insert_char;
     break;
   case('p'):
     if(CTRL) 
     {
      if(GNU_DOS_LEVEL > 1) goto do_up;
      msgBox("Oops! This function is currently not implemented.", OK, INFO);
      //fprintf(log_file, "Showing Print dialog box..\n");
      //showPrintDialogBox();
      refreshWindows();
     }
     else goto insert_char;
     break;
   case('a'):
     if(CTRL) 
     {
      if(GNU_DOS_LEVEL > 2) goto do_home;//GNU key binding
      markAll();
      if(activeWindow == DIR_WIN)
	fprintf(log_file, "Selecting All directories..\n");
      else fprintf(log_file, "Selecting All files..\n");
      refreshAll();
     } 
     else goto insert_char;
     break;
   case('w'):
     if(CTRL) 
     {
      if(GNU_DOS_LEVEL < 4)
      {
	clearSelection();
	fprintf(log_file, "Clearing all selections [ OK ]..\n");
      }
      else
      {
        cutMarked();			//found in edit.c
      }
     } 
     else goto insert_char;
     break;
   case('o'):
    if(CTRL) 
    {
     if(GNU_DOS_LEVEL > 4) break;
do_open_location:
     file_open_location();
    } 
    else if(ALT)
    {//open options menu
      //fprintf(log_file, "Opening options menu..\n");
      showOptionsMenu(YES);
      //fprintf(log_file, "Closed options menu, Waiting for user input..\n");
    }
    else goto insert_char;
    break;
   case(HOME_KEY):
     if(GNU_DOS_LEVEL > 1) break;
do_home:
     if(activeWindow == DIR_WIN) 
     {
       firstVisDir = 0;
       selectedDir = 0;
       refreshDirView();
     } 
     else if(activeWindow == FILE_WIN) 
     {
       firstVisFile = 0;
       selectedFile = 0;
       refreshFileView();
       fflush(stdout);
     } break;
   case(END_KEY):
     if(GNU_DOS_LEVEL > 1) break;
do_end:
     if(activeWindow == DIR_WIN) 
     {
       if(totalDirs <= numVisDirs) 
       {
	 selectedDir = totalDirs-1;
       }
       else 
       {
	 firstVisDir = totalDirs-numVisDirs;
	 selectedDir = numVisDirs-1;
       }//end inner if
       refreshDirView();
     } 
     else 
     {//end outer if
       if(totalFiles <= numVisFiles) 
       {
	 selectedFile = totalFiles-1;
       }
       else 
       {
	 firstVisFile = totalFiles-numVisFiles;
	 selectedFile = numVisFiles-1;
       }//end inner if
       refreshFileView();
       fflush(stdout);
     }//end outer else
     break;
   case('g'):
    if(CTRL) 
    {
     if(GNU_DOS_LEVEL < 3) break;
     goto do_exit;
    } 
    else goto insert_char;
    break;
   case('d'):
    if(CTRL) 
    {
       if(GNU_DOS_LEVEL > 3) goto do_del;
    } 
    else goto insert_char;
    break;
   case('q'):
    if(CTRL) 
    {
     if(GNU_DOS_LEVEL > 4) break;
do_exit:
      if(1) { ; }
      int i = msgBox("Are you sure you want to exit?", YES | NO, CONFIRM);
      if(i == YES) 
      { 	//exit gracefully
	exit_gracefully();
      } 
      else 
      {
	refreshWindows();
      }//end inner if
    } 
    else goto insert_char;
    break;
   case('c'):
     if(CTRL) 
     {
      copyMarked();
     } 
     else goto insert_char;
     break;
   case('/'):
     if(CTRL && GNU_DOS_LEVEL > 4) goto do_undo;
     break;
   case('z'):
     if(CTRL) 
     {
     if(GNU_DOS_LEVEL > 4) break;
do_undo:
      unMarkAll();
      if(activeWindow == DIR_WIN)
	fprintf(log_file, "Deselecting All directories..\n");
      else fprintf(log_file, "Deselecting All files..\n");
     } 
     else goto insert_char;
     break;
   case('n'):
   if(CTRL) 
   {
    if(GNU_DOS_LEVEL > 1) goto do_down;
    inputBox("Enter directory name to create:", "New Directory");
    char *res2 = (char *) malloc(MAX_DIR_NAME_LEN);
    if(!res2) { msgBox("Insufficient memory", OK, ERROR); break; }
    
    strcpy(res2, input);
    if(res2 != NULL)
    {
      struct stat st;
      if(stat(res2, &st) == -1) 
      {
	mkdir(res2, 0775);//check if it doesn't exist, create it
	fprintf(log_file, "Creating directory %s..\n", res2);
      }
      else 
      {
	msgBox("Directory already exists!", OK, ERROR);
	fprintf(log_file, "Directory %s already exists..\n", res2);
      }
      scanDir(cwd);
      refreshFileView();
      refreshDirView();
    } 
    free(res2);
   } 
   else goto insert_char;
   break;
   ////////////////////////////////////////////////////
   default:
     //browse to the first entry starting with the entered character
insert_char:
     if(ch >= 33 && ch <= 126) 
     {//if1
       int i, x = -1;
	  if(activeWindow == DIR_WIN) 
       {//if2
	 //search from this point to the end
	 for(i = firstVisDir+selectedDir+1; i < totalDirs; i++)
	   if(dirs[i][0] == ch || dirs[i][0] == ch-32) { x=i; break; }
	 //if the previous loop didn't find anything, try again
	 //starting from the top to the current point
	 if(i >= totalDirs)
	   for(i = 0; i <= firstVisDir+selectedDir; i++)
	      if(dirs[i][0] == ch || dirs[i][0] == ch-32) { x=i; break; }
	 //check to see if we found any result
	 if(x >= 0) 
	 {
	   selectedDir = x-firstVisDir;
	   if(totalDirs <= numVisDirs) { refreshDirView(); continue; }
	   if(selectedDir < 0) {firstVisDir += selectedDir; selectedDir = 0;}
	   if(selectedDir >= numVisDirs) 
	   { 
	     firstVisDir += selectedDir-numVisDirs+1;
	     selectedDir = numVisDirs-1;
	   }
	   if(totalDirs-firstVisDir < numVisDirs) 
	   { 
	     selectedDir = firstVisDir;
	     firstVisDir = totalDirs-numVisDirs; 
	     selectedDir -= firstVisDir; 
	  } 
	   refreshDirView();
	 }
	//////////////////////////////////////////////////
       } 
       else if(activeWindow == FILE_WIN) 
       {//if2
	 //search from this point to the end
	 for(i = firstVisFile+selectedFile+1; i < totalFiles; i++)
	   if(files[i][0] == ch || files[i][0] == ch-32) { x=i; break; }
	 //if the previous loop didn't find anything, try again
	 //starting from the top to the current point
	 if(i >= totalFiles)
	   for(i = 0; i <= firstVisFile+selectedFile; i++)
	      if(files[i][0] == ch || files[i][0] == ch-32) { x=i; break; }
	 //check to see if we found any result
	 if(x >= 0) 
	 {
	   selectedFile = x-firstVisFile;
	   if(totalFiles <= numVisFiles) { refreshFileView(); continue; }
	   if(selectedFile < 0) {firstVisFile+=selectedFile; selectedFile = 0;}
	   if(selectedFile >= numVisFiles) 
	   { 
	     firstVisFile += selectedFile-numVisFiles+1;
	     selectedFile = numVisFiles-1;
	   } 
	   if(totalFiles-firstVisFile < numVisFiles) 
	   { 
	     selectedFile = firstVisFile;
	     firstVisFile = totalFiles-numVisFiles; 
	     selectedFile -= firstVisFile;
	  } 
	   refreshFileView();
	 }
       }//end if2
     }//end if1
     break;
  }//end switch
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
 }//end while

 exit_gracefully();
 return 0;
}

void toggleSelected() 
{
  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
  switch(activeWindow) 
  {
    case(DIR_WIN):
      if((strcmp(dirs[firstVisDir+selectedDir], ".") == 0)//ignore '.' and '..'
	 || (strcmp(dirs[firstVisDir+selectedDir], "..") == 0)) break;

      if(dirStar[firstVisDir+selectedDir] == '*') 
      {	//if selected, un-select
	dirStar[firstVisDir+selectedDir] = ' '; numStarred--;
      } 
      else 
      {						//otherwise, select it
	if(dirStar[firstVisDir+selectedDir] == '^') 
	{
	  numCut--;	//if marked for cut, remove cut
	  removeCutDir(firstVisDir+selectedDir);
	} 
	else if (dirStar[firstVisDir+selectedDir] == '#') 
	{
	  numCopy--;	//if marked for copy, remove copy
	  removeCopyDir(firstVisDir+selectedDir);
	} //end inner if
	dirStar[firstVisDir+selectedDir] = '*'; numStarred++;
      }//end outer if				//and then print it!!
      fprintf(stdout, "\e[%d;%dH", selectedDir+4, 3);
      fprintf(stdout, "%c", dirStar[firstVisDir+selectedDir]);
      setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
      refreshBottomView();
      fprintf(stdout, "\e[3;3H");	//move cursor to point to window
      break;
    case(FILE_WIN):
      if(strcmp(files[firstVisFile+selectedFile], 
		"(Empty folder)") == 0) break;//this means folder is empty!!
      if(fileStar[firstVisFile+selectedFile] == '*') 
      {//if selected, unselect
	fileStar[firstVisFile+selectedFile] = ' '; numStarred--;
      } 
      else 
      {			//otherwise, select it
	if(fileStar[firstVisFile+selectedFile] == '^') 
	{
	  numCut--; //if marked for cut, remove cut
	  removeCutFile(firstVisFile+selectedFile);
	} 
	else if(fileStar[firstVisFile+selectedFile] == '#') 
	{
	  numCopy--; //if marked for cut, remove cut
	  removeCopyFile(firstVisFile+selectedFile);
	} //end inner if
	fileStar[firstVisFile+selectedFile] = '*'; numStarred++;
      }//end outer if				//and then print it!!
      fprintf(stdout, "\e[%d;%dH", selectedFile+4, (SCREEN_W/2)+1);
      fprintf(stdout, "%c", fileStar[firstVisFile+selectedFile]);
      setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
      refreshBottomView();
      fprintf(stdout, "\e[3;%dH",(SCREEN_W/2)+2);//cursor to point to window
      break;
  }
}

#define SHOW_ERROR_AND_RETURN(err)	\
({					\
  msgBox(err, OK, ERROR);		\
  refreshFileView();			\
  refreshDirView();			\
  return;				\
})

void scanDir(char *dir) 
{
  int dcount, fcount;
  //int nf = 0;
  int n, j;
  struct dirent **eps;
  struct stat statbuf;

  n = scandir(dir, &eps, one, alphasort);
  j = chdir(dir);
  if(j == -1) SHOW_ERROR_AND_RETURN("Error changing directory");
  //{ msgBox("Error changing directory", OK, ERROR); return; }
  
  cwd = getcwd(NULL, 0);
  dcount = fcount = 0;
  
  if(n >= 0) 
  {
    int cnt;
    fprintf(log_file, "Scanning directory '%s'\n", cwd);
    for(cnt = 0; cnt < n; ++cnt) 
    {
      j = lstat(eps[cnt]->d_name,&statbuf);
      if(j == -1) SHOW_ERROR_AND_RETURN(strerror(errno));
      //{ msgBox(strerror(errno), OK, ERROR); return; }
      
      if(S_ISDIR(statbuf.st_mode)) 
      {
	if(dcount >= MAXDIRS) break;
	dirs[dcount++] = eps[cnt]->d_name;
	if(strcmp(eps[cnt]->d_name, "..") == 0
	   || strcmp(eps[cnt]->d_name, ".") == 0) j = 0;
	else j = checkCutOrCopyDir(dcount-1);
	if(j == 1) dirStar[dcount-1] = '^';
	else if(j == 2) dirStar[dcount-1] = '#';
	else dirStar[dcount-1] = ' ';
	//check if it is a hidden dir or not
	if(dirs[dcount-1][0] == '.')
	  dirType[dcount-1] = 'h';
	else dirType[dcount-1] = 'd';
      } 
      else 
      {
	if(fcount >= MAXFILES) break;
	files[fcount++] = eps[cnt]->d_name;
	if(strcmp(dir, "..") == 0) j = 0;
	else j = checkCutOrCopyFile(fcount-1);
	if(j == 1) fileStar[fcount-1] = '^';
	else if(j == 2) fileStar[fcount-1] = '#';
	else fileStar[fcount-1] = ' ';
	//check to see the file type
	if(S_ISLNK(statbuf.st_mode))	//is it a link?
	  fileType[fcount-1] = 'l';
	else
	{
	  if(files[fcount-1][0] == '.')	//is it hidden?
	    fileType[fcount-1] = 'h';
	  else if(strcasestr(files[fcount-1], ".tar")
	       || strcasestr(files[fcount-1], ".gz")
	       || strcasestr(files[fcount-1], ".xz")
	       || strcasestr(files[fcount-1], ".Z")
	       || strcasestr(files[fcount-1], ".rar")
	       || strcasestr(files[fcount-1], ".zip")
	       || strcasestr(files[fcount-1], ".bz2")
	       || strcasestr(files[fcount-1], ".7z")
	       || strcasestr(files[fcount-1], ".lzma")
	       || strcasestr(files[fcount-1], ".lha")
	       || strcasestr(files[fcount-1], ".jar"))//is archive?
	    fileType[fcount-1] = 'a';
	  else if(strcasestr(files[fcount-1], ".bmp")
	       || strcasestr(files[fcount-1], ".png")
	       || strcasestr(files[fcount-1], ".jpg")
	       || strcasestr(files[fcount-1], ".jpeg")
	       || strcasestr(files[fcount-1], ".pcx")
	       || strcasestr(files[fcount-1], ".ico")
	       || strcasestr(files[fcount-1], ".gif")
	       || strcasestr(files[fcount-1], ".tiff"))//is picture?
	    fileType[fcount-1] = 'p';
	  else if((statbuf.st_mode & S_IXUSR)
	       || (statbuf.st_mode & S_IXGRP)
	       || (statbuf.st_mode & S_IXOTH)) //is executable?
	    fileType[fcount-1] = 'x';
	  else	//just a regular file
	    fileType[fcount-1] = 'r';
	}//end file type check
      }
    }
  } 
  else 
  {
    fprintf(log_file, "Error opening dir: %s\n", cwd);
    dcount = msgBox("Cannot open directory.\nProbably bad path specified "
		    "or access denied.", OK, ERROR);
    refreshWindows();
    return;
  }
  
  totalFiles = fcount;
  if(totalFiles < numVisFiles)
    files[totalFiles] = NULL;	//so the refreshFileView() will work properly
  firstVisFile = 0;
  selectedFile = 0;
    
  totalDirs = dcount;
  if(totalDirs < numVisDirs) 
    dirs[totalDirs] = NULL;	//so the refreshDirView() will work properly
  firstVisDir = 0;
  selectedDir = 0;
    
  numStarred = 0;
  refreshAll();
  //fprintf(log_file, "Finished scanning directory '%s'..\n", cwd);
}

static int one(const struct dirent *unused) 
{
  return 1;
}


void refreshAll()
{
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  drawBox(1,1, SCREEN_H, SCREEN_W," Prime for GNU/Linux ",NO);
  drawMenuBar(2, 2, SCREEN_W-2);
  refreshFileView();
  refreshDirView();
  refreshBottomView();
}

void refreshWindows()
{
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  refreshFileView();
  refreshDirView();
}

/***************************************
 * refreshDirView(): 
 * Procedure to refresh the left window
 * showing directory tree.
 * **************************************/
void refreshDirView() 
{
 if(activeWindow == DIR_WIN) setScreenColors(GREEN, BG_COLOR[COLOR_WINDOW]);
 //draw left sub-window
 drawBox(3, 2, SCREEN_H-5, (int)(SCREEN_W/2)-1, " Directory view ", YES);
 int i, j;
 if(totalDirs == 0) 
 {
   i = msgBox("No directory entries found!.", OK, ERROR);
   refreshFileView();
   refreshDirView();// FIXME: this will make an infinite loop.. solution??
   return;
 }
 for(i = 0; i < numVisDirs; i++) 
 {
   if(dirs[firstVisDir+i] == NULL) break; 
   if(i == selectedDir && activeWindow == DIR_WIN) 
   {
      setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
      fprintf(stdout, "\x1b[%d;%dH%s", i+4, 3, dirHighLight);
      if(strlen(dirs[firstVisDir+i]) >= MAX_DIR_NAME_LEN) {
	fprintf(stdout, "\x1b[%d;%dH%c", i+4, 3, dirStar[firstVisDir+i]);
	for(j = 0; j < MAX_DIR_NAME_LEN-3; j++) 
	  fprintf(stdout, "%c", dirs[firstVisDir+i][j]);
	fprintf(stdout, "..");
      } 
      else 
      {
	fprintf(stdout, "\x1b[%d;%dH%c%s", i+4, 3, 
		dirStar[firstVisDir+i], dirs[firstVisDir+i]);
      }//end if
    } 
    else 
    {
      setScreenColors(FILE_DIR_COLOR[dirType[firstVisDir+i]-'a'], BG_COLOR[COLOR_WINDOW]);
      if(strlen(dirs[firstVisDir+i]) >= MAX_DIR_NAME_LEN) 
      {
	fprintf(stdout, "\x1b[%d;%dH%c", i+4, 3, dirStar[firstVisDir+i]);
	for(j = 0; j < MAX_DIR_NAME_LEN-3; j++) 
	  fprintf(stdout, "%c", dirs[firstVisDir+i][j]);
	fprintf(stdout, "..");
      } 
      else 
      {
	fprintf(stdout, "\x1b[%d;%dH%c%s", i+4, 3, 
		dirStar[firstVisDir+i], dirs[firstVisDir+i]);
      }//end if
    }//end if
 }//end for
 setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
 //redraw main window but don't clear the area
 drawBox(1, 1, SCREEN_H, SCREEN_W, " Prime  for GNU/Linux ", NO);
 
 //move cursor to point to window
 if(activeWindow == DIR_WIN) fprintf(stdout, "\e[3;3H");
 //move cursor to point to window
 if(activeWindow == FILE_WIN) fprintf(stdout, "\e[3;%dH",(SCREEN_W/2)+2);
}


/***************************************
 * refreshFileView(): 
 * Procedure to refresh the right window
 * showing file entries.
 * **************************************/
void refreshFileView() 
{
 if(activeWindow == FILE_WIN) setScreenColors(GREEN, BG_COLOR[COLOR_WINDOW]);
 //draw right sub-window
 drawBox(3, (int)(SCREEN_W/2), SCREEN_H-5, SCREEN_W-1, " File view ", YES);
 int i, j;
 if(totalFiles == 0) 
 {
   files[0] = "(Empty folder)";
   files[1] = NULL;
   totalFiles = 1;
 }
 for(i = 0; i < numVisFiles; i++) 
 {
    if(files[firstVisFile+i] == NULL) break; 
    if(i == selectedFile && activeWindow == FILE_WIN) 
    {
      setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
      fprintf(stdout, "\x1b[%d;%dH%s", i+4, (SCREEN_W/2)+1, fileHighLight);
      if(strlen(files[firstVisFile+i]) >= MAX_FILE_NAME_LEN)
      {
	fprintf(stdout, "\x1b[%d;%dH%c", i+4, (SCREEN_W/2)+1, 
		fileStar[firstVisFile+i]);
	for(j = 0; j < MAX_FILE_NAME_LEN-3; j++) 
	  fprintf(stdout, "%c", files[firstVisFile+i][j]);
	fprintf(stdout, "..");
      } 
      else 
      {
	fprintf(stdout, "\x1b[%d;%dH%c%s", i+4, (SCREEN_W/2)+1, 
		      fileStar[firstVisFile+i], files[firstVisFile+i]);
      }//end if
    } 
    else 
    {
	setScreenColors(FILE_DIR_COLOR[fileType[firstVisFile+i]-'a'], BG_COLOR[COLOR_WINDOW]);
	if(strlen(files[firstVisFile+i]) >= MAX_FILE_NAME_LEN) 
	{
	  fprintf(stdout, "\x1b[%d;%dH%c", i+4, (SCREEN_W/2)+1, 
		  fileStar[firstVisFile+i]);
	  for(j = 0; j < MAX_FILE_NAME_LEN-3; j++) 
	    fprintf(stdout, "%c", files[firstVisFile+i][j]);
	  fprintf(stdout, "..");
	} 
	else 
	{
          fprintf(stdout, "\x1b[%d;%dH%c%s", i+4, (SCREEN_W/2)+1, 
		      fileStar[firstVisFile+i], files[firstVisFile+i]);
	}//end if
    }//end if
 }//end for
 setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  
 if(activeWindow == DIR_WIN) 
   fprintf(stdout, "\e[3;3H");	//move cursor to point to window
 if(activeWindow == FILE_WIN) 
   fprintf(stdout, "\e[3;%dH",(SCREEN_W/2)+2);//move cursor to point to window
}

/***************************************
 * refreshBottomView(): 
 * Procedure to refresh the bottom window
 * showing CWD and statistics.
 * *************************************/
void refreshBottomView() 
{
  //draw bottom sub-window
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  drawBox(SCREEN_H-4, 2, SCREEN_H-1, SCREEN_W-1, " Quick reference ", YES);
  fprintf(stdout, "\e[%d;%dH", SCREEN_H-2, 4);
  if(cwd != NULL) 
  {	//check the current working directory var is not NULL pointer
    if(strlen(cwd) > SCREEN_W-7) 
    {	//check if cwd length is more than display width
      int i;
      fprintf(stdout, "CWD: ");
      for(i = 0; i < SCREEN_W-12; i++) 
	putchar(cwd[i]);	//show just enought chars of the string
      fprintf(stdout, "..");			//and seal it with '..'
    } 
    else 
    { 				//the length is less than display width
      fprintf(stdout, "CWD: %s", cwd); 	//so spit it all out
    }
  }
  fprintf(stdout, "\e[%d;%dH", SCREEN_H-3, 4);
  if(numStarred > 0) fprintf(stdout, "Marked (%d) ", numStarred);
  if(numCut > 0) fprintf(stdout, "Cut (%d) ", numCut);
  if(numCopy > 0) fprintf(stdout, "Copy (%d) ", numCopy);
  //redraw main window but don't clear the area
  drawBox(1, 1, SCREEN_H, SCREEN_W, " Prime  for GNU/Linux ", NO);

  if(activeWindow == DIR_WIN) 
    fprintf(stdout, "\e[3;3H");	//move cursor to point to window
  if(activeWindow == FILE_WIN) 
    fprintf(stdout, "\e[3;%dH",(SCREEN_W/2)+2);//cursor to point to window
}

/***************************************
 * drawMenuBar(): 
 * Procedure to draw the main menu bar.
 * **************************************/
void drawMenuBar(int x, int y, int w) 
{
  printf("\x1b[0m");
  setScreenColors(FG_COLOR[COLOR_MENU_BAR], BG_COLOR[COLOR_MENU_BAR]);
  fprintf(stdout, "\x1b[%d;%dH", x, y);		//reposition the cursor
  int i,j, lastChar=y;
  for(i = 0; i < w; i++) fputc(' ', stdout);	//Draw empty menu bar
  fprintf(stdout, "\x1b[%d;%dH", x, y);		//reposition the cursor

  for(i = 0; i < totalMainMenus; i++) 
  {
    j=0; lastChar++;
    fprintf(stdout, " ");
    while(menu[i][j] != '\0') 
    {
      if(menu[i][j] == '&') 
      {	//turn on underline feature to print the shortcut key
	//fprintf(stdout, "\x1b[4m");
	fprintf(stdout, "%c", menu[i][j+1]);
	//fprintf(stdout, "\x1b[24m");//then turn it off
      }
      else
	fprintf(stdout, "%c", menu[i][j+1]);	//print normal chars (other than the
      lastChar++; j++;					//shortcut key)
    }
    fprintf(stdout, " ");
  }
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
}

/***************************************
 * init(): 
 * Procedure containing code that init-
 * ializes various data structures.
 * **************************************/
void init()
{
  getScreenSize();
  if(!SCREEN_H || !SCREEN_W)
  {
    printf("Error: Failed to get terminal size\n");
    exit(1);
  }
  //define colors that will be used to colorize the view
  FILE_DIR_COLOR['d'-'a'] = FG_COLOR[COLOR_WINDOW];	//color for directories
  FILE_DIR_COLOR['x'-'a'] = GREEN;	//color for exec files
  FILE_DIR_COLOR['r'-'a'] = FG_COLOR[COLOR_WINDOW];	//color for regular files
  FILE_DIR_COLOR['l'-'a'] = CYAN;	//color for links
  FILE_DIR_COLOR['a'-'a'] = RED;	//color for archives
  FILE_DIR_COLOR['h'-'a'] = BROWN;	//color for hidden files
  FILE_DIR_COLOR['p'-'a'] = MAGENTA;	//color for picture files

  //1. Main Menu//
  menu[0] = "&File";
  menu[1] = "&Edit";
  menu[2] = "&Options";
  menu[3] = "&Help";
  //2. File Menu//
  fileMenu[0] = "New directory ^N";
  fileMenu[1] = "Open location ^O";
  fileMenu[2] = "Export tree   ^E";
  fileMenu[3] = "Print         ^P";
  fileMenu[4] = "Exit          ^Q";
  //3. Edit Menu//
  editMenu[0] = "Cut        ^X";
  editMenu[1] = "Copy       ^C";
  editMenu[2] = "Paste      ^V";
  editMenu[3] = "Mark all   ^A";
  editMenu[4] = "Unmark all ^Z";
  editMenu[5] = "Clear sel. ^W";
  editMenu[6] = "Find..     ^F";
  //4. Options Menu//
  optionsMenu[0] = "Properties     ";
  optionsMenu[1] = "Change colors  ";
  optionsMenu[2] = "Reset config   ";
  //5. Help Menu//
  helpMenu[0] = "View README    ";
  helpMenu[1] = "GNU keybindings";
  helpMenu[2] = "Quick reference";
  helpMenu[3] = "About Prime..  ";

 int i;
  //set defaults for Dir view//
 numVisDirs = SCREEN_H-9;
 firstVisDir = 0;
 selectedDir = -1;
 totalDirs = 0;
 //reserve enough space in memory for highlight bar
 dirHighLight = malloc(SCREEN_W/2);
 if(!dirHighLight) goto memory_error;
 memset(dirHighLight, ' ', (SCREEN_W/2)-4);	//fill the bar with spaces
 dirHighLight[(SCREEN_W/2)-4] = '\0';
  //set defaults for File view//
 numVisFiles = SCREEN_H-9;
 firstVisFile = 0;
 selectedFile = -1;
 totalFiles = 0;
 fileHighLight = malloc(SCREEN_W/2);
 if(!fileHighLight) goto memory_error;
 memset(fileHighLight, ' ', (SCREEN_W/2)-2);
 fileHighLight[(SCREEN_W/2)-2] = '\0';
 //strcat(fileHighLight, "\0");
 
 activeWindow = DIR_WIN;
 MAX_DIR_NAME_LEN = (SCREEN_W/2)-4;
 MAX_FILE_NAME_LEN = (SCREEN_W/2)-2; 
 MAX_MSG_BOX_W = SCREEN_W - 4;
 MAX_MSG_BOX_H = SCREEN_H - 4;
 for(i = 0; i < MAXDIRS; i++) 
 {
   dirStar[i] = ' ';
   dirs[i] = (char *) malloc(MAX_DIR_NAME_LEN);
   if(!dirs[i]) goto memory_error;
   dirType[i] = 'd';
 }
 for(i = 0; i < MAXFILES; i++) 
 {
   fileStar[i] = ' ';
   fileType[i] = 'r';
   files[i] = (char *) malloc(MAX_FILE_NAME_LEN);
   if(!files[i]) goto memory_error;
 }
  
 numStarred = 0;
 numCut = 0;
 numCopy = 0;
 numCutFiles = 0;
 numCopyFiles = 0;
 numCutDirs = 0;
 numCopyDirs = 0;
 for(i = 0; i < MAX_CUT_COPY; i++) 
 {
   cutFiles[i] = malloc(MAX_FILE_NAME_LEN);
   copyFiles[i] = malloc(MAX_FILE_NAME_LEN);
   cutDirs[i] = malloc(MAX_DIR_NAME_LEN);
   copyDirs[i] = malloc(MAX_DIR_NAME_LEN);
   if(!cutFiles[i] || !copyFiles[i] || !cutDirs[i] || !copyDirs[i])
     goto memory_error;
 }

 //numPrinters = -1;
 //selectedPrinter = -1;
 //PRINTING = 0;

 startStr = (char *) malloc(MAX_DIR_NAME_LEN); 
 if(!startStr) goto memory_error;
 middleStr = (char *) malloc(MAX_DIR_NAME_LEN);
 if(!middleStr) goto memory_error;
 endStr = (char *) malloc(MAX_DIR_NAME_LEN); 
 if(!endStr) goto memory_error;

 FG_COLOR[COLOR_WINDOW]         = 37;
 FG_COLOR[COLOR_HIGHLIGHT_TEXT] = 34;
 FG_COLOR[COLOR_MENU_BAR]       = 34;
 FG_COLOR[COLOR_STATUS_BAR]     = 34;
 FG_COLOR[COLOR_BUTTONS]        = 37;
 FG_COLOR[COLOR_HBUTTONS]       = 32;
 BG_COLOR[COLOR_WINDOW]         = 49;
 BG_COLOR[COLOR_HIGHLIGHT_TEXT] = 47;
 BG_COLOR[COLOR_MENU_BAR]       = 47;
 BG_COLOR[COLOR_STATUS_BAR]     = 47;
 BG_COLOR[COLOR_BUTTONS]        = 41;
 BG_COLOR[COLOR_HBUTTONS]       = 41;
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
 BG_COLOR_ARRAY[0] = 49;
 BG_COLOR_ARRAY[1] = 41;
 BG_COLOR_ARRAY[2] = 42;
 BG_COLOR_ARRAY[3] = 43;
 BG_COLOR_ARRAY[4] = 44;
 BG_COLOR_ARRAY[5] = 45;
 BG_COLOR_ARRAY[6] = 46;
 BG_COLOR_ARRAY[7] = 47;
 
 tmp = (char *) malloc(MAX_DIR_NAME_LEN+MAX_FILE_NAME_LEN+1);
 if(!tmp) goto memory_error;
 tmp2 = (char *) malloc(MAX_DIR_NAME_LEN+MAX_FILE_NAME_LEN+1);
 if(!tmp2) goto memory_error;
 input_str = (char *) malloc(MAX_FILE_NAME_LEN);
 if(!input_str) goto memory_error;

 initTerminal();
 return;
 
memory_error:
 fprintf(stderr, "Insufficient memory\n");
 exit(1);
}


int read_config_file()
{
      GNU_DOS_LEVEL = 1;
      if(!(pass = getpwuid(geteuid()))) 
      {
	printf("Error: couldn't open home directory to read "
	       "configuration file.\n");
	printf("Aborting.\n");
	exit(1);
      }
      config_file_name = (char *) malloc(strlen(pass->pw_dir)+12);
      if(!config_file_name) { printf("Insufficient memory\n"); exit(1); }
      strcpy(config_file_name, pass->pw_dir);
      strcat(config_file_name, "/");
      strcat(config_file_name, ".prime.conf");
      if(!(config_file = fopen(config_file_name, "r"))) 
      {
	printf("Error: couldn't read configuration file in home directory.\n");
	printf("Resetting configuration file.\n");
	//exit(1);
	write_config_file_defaults();
	config_file = fopen(config_file_name, "r");
      }

      char buf[100];
      //read configuration file
      while(fgets(buf, sizeof(buf), config_file)) 
      {
	if(buf[0] == '#' || buf[0] == '\n') continue;
	else if (strstr(buf, "GNU_DOS_LEVEL")) 
	{
	  GNU_DOS_LEVEL = atoi((strchr(buf, '=')+2));
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
      }//end while
      fclose(config_file);
      if(NEW_GNU_DOS_LEVEL)
	GNU_DOS_LEVEL = NEW_GNU_DOS_LEVEL;
      if(GNU_DOS_LEVEL > 6 || GNU_DOS_LEVEL < 1) GNU_DOS_LEVEL = 1;
      return 1;
}

int write_config_file_defaults()
{
      if(!(pass = getpwuid(geteuid()))) 
      {
	//printf("Error: couldn't open home directory to write configuration file.\n");
	//printf("Aborting.\n");
	//exit(1);
	return 0;
      }
      config_file_name = (char *) malloc(strlen(pass->pw_dir)+12);
      if(!config_file_name) { printf("Insufficient memory\n"); exit(1); }
      strcpy(config_file_name, pass->pw_dir);
      strcat(config_file_name, "/");
      strcat(config_file_name, ".prime.conf");
      if(!(config_file = fopen(config_file_name, "w"))) 
      {
	//printf("Error: couldn't write to configuration file in home directory.\n");
	//printf("Aborting.\n");
	//exit(1);
	return 0;
      }
      //printf("Resetting program configuration..\n");
      //write default values to the configuration file
      fprintf(config_file, "#Configuration file for the prime program\n");
      fprintf(config_file, "#Please do not modify this file by hand\n\n");
      fprintf(config_file, "#Display colors\n");
      fprintf(config_file, "FG_COLOR_WIN = 37\n");
      fprintf(config_file, "FG_COLOR_HLT = 34\n");
      fprintf(config_file, "FG_COLOR_MBAR = 34\n");
      fprintf(config_file, "FG_COLOR_SBAR = 34\n");
      fprintf(config_file, "FG_COLOR_HBUT = 32\n");
      fprintf(config_file, "FG_COLOR_BUT = 37\n");
      fprintf(config_file, "BG_COLOR_WIN = 49\n");
      fprintf(config_file, "BG_COLOR_HLT = 47\n");
      fprintf(config_file, "BG_COLOR_MBAR = 47\n");
      fprintf(config_file, "BG_COLOR_SBAR = 47\n");
      fprintf(config_file, "BG_COLOR_HBUT = 41\n");
      fprintf(config_file, "BG_COLOR_BUT = 41\n\n");
      fprintf(config_file, "#GnuDOS Level\n");
      fprintf(config_file, "GNU_DOS_LEVEL = %d\n", GNU_DOS_LEVEL);
      fclose(config_file);
      return 1;
}
