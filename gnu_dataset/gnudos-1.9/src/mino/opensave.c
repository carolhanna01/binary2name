/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014, 2015 (c)
 * 
 *    file: opensave.c
 *    This file is part of mino (Mino).
 *
 *    mino (Mino) is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    (at your option) any later version.
 *
 *    mino (Mino) is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with mino (Mino).  If not, see <http://www.gnu.org/licenses/>.
 */    
#include "defs.h"
#include "kbd.h"
#include "options.h"
#include "edit.h"
#include <pwd.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <dirent.h>
#include <sys/stat.h>
#include <fcntl.h>

/* main.c */
extern char *tmp;
extern char *tmp2;

void checkFileExtension();

static int one(const struct dirent *unused) 
{
  return 1;
}

struct passwd *pass;//will be used to find the home dir

//char *cwd;
void findFullFileName(char *f);
char *findFullDirName(char *d);

/*****************************************
 * findFullFileName():
 * Some file names are too long, so we
 * cut them down and put ".." at the end.
 * When we want to open the actual file,
 * we need to find the complete name.
 * This function does that.
 * ***************************************/
void findFullFileName(char *f) 
{
  int n;
  struct dirent **eps;
  struct stat st;
  //remove the ".." at the end
  f[strlen(f)-2] = '\0';
  
  n = scandir(".", &eps, one, alphasort);
  if(n >= 0) 
  {
    int cnt;
    for(cnt = 0; cnt < n; ++cnt) 
    {
      int x = lstat(eps[cnt]->d_name,&st);
      if(x == -1) { msgBox(strerror(errno), OK, ERROR); return; }
      if(S_ISDIR(st.st_mode)) 
      {
      } 
      else 
      {
	if(strstr(eps[cnt]->d_name, f)) 
	{
		strcpy(open_file_name, cwd);
		strcat(open_file_name, "/");
		strcat(open_file_name, eps[cnt]->d_name);
	  return;
	}
      } //end else
    } //end for
 } //end outer if
}

/*****************************************
 * findFullDirName():
 * Some dir names are too long, so we
 * cut them down and put ".." at the end.
 * When we want to open the actual dir,
 * we need to find the complete name.
 * This function does that and returns
 * the full dir name as char pointer.
 * ***************************************/
char *findFullDirName(char *d) 
{
  int n;
  struct dirent **eps;
  struct stat st;
  //remove the ".." at the end
  d[strlen(d)-2] = '\0';

  n = scandir(".", &eps, one, alphasort);
  if(n >= 0) 
  {
    int cnt;
    for(cnt = 0; cnt < n; ++cnt) 
    {
      int x = lstat(eps[cnt]->d_name,&st);
      if(x == -1) { msgBox(strerror(errno), OK, ERROR); return NULL; }
      if(S_ISDIR(st.st_mode)) 
      {
	if(strstr(eps[cnt]->d_name, d))
	  return eps[cnt]->d_name;
      }
    } //end for
  }
  return NULL;
}

#define WRAP_DIR_OUTPUT			\
({					\
  if(pos >= totalDirs)			\
  {					\
    fprintf(stdout, "\x1b[%d;%dH%s", (selectedDir >= numVisDirs) ? 	\
    selectedDir-numVisDirs+3:selectedDir+3,				\
    (selectedDir >= numVisDirs) ? x+MAX_DIR_NAME_LEN+1:x+1,		\
    files[pos-totalDirs]);						\
  }									\
  else 					\
  {					\
    fprintf(stdout, "\x1b[%d;%dH[%s]", (selectedDir >= numVisDirs) ? 	\
    selectedDir-numVisDirs+3:selectedDir+3,				\
    (selectedDir >= numVisDirs) ? x+MAX_DIR_NAME_LEN+1:x+1,		\
    dirs[pos]);								\
  }					\
})

/*****************************************
 * openSaveFile():
 * This function shows a dialog box that
 * contains the directory listing of the
 * current dir. It takes over user input
 * to enable navigation through the dir
 * tree and selecting a file to open/save.
 * It then calls _openFile() or _saveFile()
 * to actually open/save the file.
 * ***************************************/
int openSaveFile(OPEN_SAVE openSave, int showDialog) 
{
  x = 2; y = 2;
  h = SCREEN_H-1; w = SCREEN_W-1;
  
  char inputName[w-y-9];//the input file name entered in the field
  int selChar;	//the selected char in input field
  int sel;	//0=dir tree, 1=input field
  
  switch(showDialog) 
  {
    case(YES):
      //try to open the document directory, if failed open home directory,
      //if also failed, just open the current working directory.
      if(documentTitle && strrchr(documentTitle, '/')) 
      {
	  char *tmp;
	  tmp = (char *) malloc(strlen(documentTitle));
	  if(!tmp) { msgBox("Insufficient memory", OK, ERROR); return 0; }
	  strcpy(tmp, documentTitle);
	  tmp[strrchr(documentTitle, '/')-documentTitle] = '\0';
	  scanDir(tmp);
	  free(tmp);
      } 
      else if((pass = getpwuid(geteuid()))) 
      {
	if(!scanDir(pass->pw_dir)) return 0;
      } 
      else 
      {
	if(!scanDir(".")) return 0;
      }
      sel = (openSave == SAVE) ? 1 : 0;
      if(openSave == OPEN) strcpy(inputName, "");
      else 
      {
	//if there is a document title, save the title without
	//the pathname, if any..
	if(documentTitle) strcpy(inputName, strrchr(documentTitle, '/') ?
					    strrchr(documentTitle, '/')+1 :
					    documentTitle);
	else strcpy(inputName, DEFAULT_TITLE);
      }
      selChar = strlen(inputName);
  
      numVisDirs = h-x-3;
      firstVisDir = 0;
      selectedDir = 0;
      refreshDirView();
      //set the input field
	setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	fprintf(stdout, "\e[%d;%dH", h-2, y+1);
	printf("File: ");
	setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	printf("%s%*s", inputName, (int)(w-y-strlen(inputName)-9), " ");
	fprintf(stdout, "\e[%d;%dH", h-2, (int)(y+strlen(inputName)+7));
	//reset cursor if it is an OPEN file dialog
	if(openSave == OPEN) 
	{//if #1
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  int pos = firstVisDir+selectedDir;
	  WRAP_DIR_OUTPUT;
	}//end if #1
      fflush(stdout);
      /***************************************************/
      //take control over user input
      /***************************************************/
      while(1) 
      {
	char *ch = (char *)malloc(5);
	ch = getKey();
	switch(ch[0]) 
	{
	  case('a'):
	    if(GNU_DOS_LEVEL > 2 && CTRL) goto do_home;
	    goto do_enter_char; break;
	  case(HOME_KEY):
	    if(GNU_DOS_LEVEL > 2) break;
do_home:
	    if(sel == 1) 
	    {
	      selChar = 0;
	      fprintf(stdout, "\e[%d;%dH", h-2, y+selChar+7);
	      fflush(stdout);
	    } break;
	  case('e'):
	    if(GNU_DOS_LEVEL > 2 && CTRL) goto do_end;
	    goto do_enter_char; break;
	  case(END_KEY):
	    if(GNU_DOS_LEVEL > 2) break;
do_end:
	    if(sel == 1) 
	    {
	      selChar = strlen(inputName);
	      if(sel > w-y-9) sel--;
	      fprintf(stdout, "\e[%d;%dH", h-2, y+selChar+7);
	      fflush(stdout);
	    } break;
	  case('d'):
	    if(GNU_DOS_LEVEL > 3 && CTRL) goto do_del;
	    goto do_enter_char; break;
	  case(DEL_KEY):
	    if(GNU_DOS_LEVEL > 3) break;
do_del:
	    if(sel == 1) 
	    {
	      if(selChar == strlen(inputName)) break;
	      int i;
	      for(i = selChar; i < strlen(inputName)-1; i++)
		inputName[i] = inputName[i+1];
	      inputName[i] = '\0';
	      setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	      fprintf(stdout, "\e[%d;%dH", h-2, y+7);
	      printf("%*s", w-y-9, " ");
	      fprintf(stdout, "\e[%d;%dH", h-2, y+7);
	      printf("%s", inputName);
	      fprintf(stdout, "\e[%d;%dH", h-2, y+selChar+7);
	      fflush(stdout);
	    } break;
	  /***************************************************/
	  /***************************************************/
	  /***************************************************/
	  case(BACKSPACE_KEY):
	    if(sel == 1) 
	    {
	      if(selChar == 0) break;
	      int i;
	      if(selChar == strlen(inputName)) 
	      {
		i = strlen(inputName)-1;
	      } 
	      else 
	      {
		for(i = selChar; i <= strlen(inputName); i++)
		  inputName[i-1] = inputName[i];
	      }
	      inputName[i] = '\0';
	      selChar--;
	      setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	      fprintf(stdout, "\e[%d;%dH", h-2, y+7);
	      printf("%*s", w-y-9, " ");
	      fprintf(stdout, "\e[%d;%dH", h-2, y+7);
	      printf("%s", inputName);
	      fprintf(stdout, "\e[%d;%dH", h-2, y+selChar+7);
	      fflush(stdout);
	    } break;
	  /***************************************************/
	  /***************************************************/
	  /***************************************************/
	  case(TAB_KEY):
	      sel = (sel == 0) ? 1 : 0;
	      int pos = firstVisDir+selectedDir;
	      if(sel == 1) 
	      {//if #1
		setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
		WRAP_DIR_OUTPUT;
		fprintf(stdout, "\e[%d;%dH", h-2, y+selChar+7);
	      } 
	      else 
	      {
		setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
		WRAP_DIR_OUTPUT;
	      }//if #1
	    //} 
	    break;
	  /***************************************************/
	  /***************************************************/
	  /***************************************************/
	  case('b'):
	    if(GNU_DOS_LEVEL > 1 && CTRL) goto do_left;
	    goto do_enter_char; break;
	  case(LEFT_KEY):
	    if(GNU_DOS_LEVEL > 1) break;
do_left:
	    if(sel == 1) 
	    {
	      if(selChar == 0) break;
	      selChar--;
	      fprintf(stdout, "\e[%d;%dH", h-2, y+selChar+7);
	      break;
	    }
	    
	    if(selectedDir >= numVisDirs) 
	    {
		int pos = firstVisDir+selectedDir;
		setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
		WRAP_DIR_OUTPUT;
		selectedDir -= numVisDirs;
		pos -= numVisDirs;
		setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
		if(pos >= totalDirs) 
		{//if #6
		  fprintf(stdout, "\x1b[%d;%dH%s", (selectedDir >= numVisDirs) ? 
			    selectedDir-numVisDirs+3:selectedDir+3,
			    (selectedDir >= numVisDirs) ? x+MAX_DIR_NAME_LEN+1:x+1,
			    files[pos-totalDirs]);
		  if(strstr(files[pos], "..")) 
		  {
		    findFullFileName(files[pos-totalDirs]);
		    strcpy(inputName, open_file_name);
		  } else strcpy(inputName, files[pos-totalDirs]);
		} 
		else 
		{
		  fprintf(stdout, "\x1b[%d;%dH[%s]", (selectedDir >= numVisDirs) ? 
			    selectedDir-numVisDirs+3:selectedDir+3,
			    (selectedDir >= numVisDirs) ? x+MAX_DIR_NAME_LEN+1:x+1,
			    dirs[pos]);
		}//end if #6
	    } 
	    else 
	    {
	      if(firstVisDir == 0) break;
	      selectedDir += numVisDirs;
	      firstVisDir -= numVisDirs;
	      refreshDirView();
	    } 
	      setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	      fprintf(stdout, "\e[%d;%dH", h-2, y+1);
	      printf("File: ");
	      setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	      printf("%s%*s", inputName, (int)(w-y-strlen(inputName)-9), " ");
	      if(sel == 1) fprintf(stdout, "\e[%d;%dH", h-2, y+selChar+7);
	    fflush(stdout);
	    break;
	  /***************************************************/
	  /***************************************************/
	  /***************************************************/
	  case('f'):
	    if(GNU_DOS_LEVEL > 1 && CTRL) goto do_right;
	    goto do_enter_char; break;
	  case(RIGHT_KEY):
	    if(GNU_DOS_LEVEL > 1) break;
do_right:
	    if(sel == 1) 
	    {
	      if(selChar == strlen(inputName)) break;
	      if(selChar >= w-y-9) break;
	      selChar++;
	      fprintf(stdout, "\e[%d;%dH", h-2, y+selChar+7);
	      break;
	    }
	    
	    if(selectedDir < numVisDirs) 
	    {
		int pos = firstVisDir+selectedDir;
		setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
		WRAP_DIR_OUTPUT;
		selectedDir += numVisDirs;
		pos += numVisDirs;
		
		if(pos >= (totalDirs+totalFiles))
		{
		  int h = (totalDirs+totalFiles)-pos-1;
		  pos += h;  //h is negative, so add it in order to subtract it!!
		  selectedDir += h;
		}
		
		setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
		if(pos >= totalDirs) 
		{//if #6
		  fprintf(stdout, "\x1b[%d;%dH%s", (selectedDir >= numVisDirs) ? 
			    selectedDir-numVisDirs+3:selectedDir+3,
			    (selectedDir >= numVisDirs) ? x+MAX_DIR_NAME_LEN+1:x+1,
			    files[pos-totalDirs]);
		  if(strstr(files[pos], "..")) 
		  {
		    findFullFileName(files[pos-totalDirs]);
		    strcpy(inputName, open_file_name);
		  } else strcpy(inputName, files[pos-totalDirs]);
		} 
		else 
		{
		  fprintf(stdout, "\x1b[%d;%dH[%s]", (selectedDir >= numVisDirs) ? 
			    selectedDir-numVisDirs+3:selectedDir+3,
			    (selectedDir >= numVisDirs) ? x+MAX_DIR_NAME_LEN+1:x+1,
			    dirs[pos]);
		}//end if #6
	    } 
	    else 
	    {
	      if((firstVisDir+(numVisDirs*2)) >= (totalDirs+totalFiles)) break;
	      selectedDir -= numVisDirs;
	      firstVisDir += numVisDirs;
	      refreshDirView();
	    } 
	      setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	      fprintf(stdout, "\e[%d;%dH", h-2, y+1);
	      printf("File: ");
	      setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	      printf("%s%*s", inputName, (int)(w-y-strlen(inputName)-9), " ");
	      if(sel == 1) fprintf(stdout, "\e[%d;%dH", h-2, y+selChar+7);
	    fflush(stdout);
	    break;
	  /***************************************************/
	  /***************************************************/
	  /***************************************************/
	  case('g'):
	    if(GNU_DOS_LEVEL > 2 && CTRL) goto do_esc;
	    goto do_enter_char; break;
	  case(ESC_KEY):
	    if(GNU_DOS_LEVEL > 2) break;
do_esc:
	    refreshView();
	    return 0;
	    break;
	  /***************************************************/
	  /***************************************************/
	  /***************************************************/
	  case(ENTER_KEY):
	    if(sel == 1) 
	    {
	      if(openSave == SAVE) 
	      {
		strcpy(open_file_name, cwd);
		strcat(open_file_name, "/");
		strcat(open_file_name, inputName);
		//*****check if the file already exists
		if(fopen(open_file_name, "r")) 
		{
		  int i = msgBox("File already exists. Overwrite?", YES|NO, CONFIRM);
		  if(i == NO) 
		  { 
		    refreshDirView(); 
		    setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	     	    fprintf(stdout, "\e[%d;%dH", h-2, y+1);
		    printf("File: ");
		    setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
		    printf("%s%*s", inputName, (int)(w-y-strlen(inputName)-9), " ");
		    if(sel == 1) fprintf(stdout, "\e[%d;%dH", h-2, y+selChar+7);
		    fflush(stdout);
		    break; 
		  }
		}/////////////////////////
		if(!_saveFile()) return 0;
		FILE_STATE = SAVED;
		NEW_FILE = 0;
		return 1;
	      }
	      else 
	      {
		strcpy(open_file_name, cwd);
		strcat(open_file_name, "/");
		strcat(open_file_name, inputName);
		if(!_openFile()) return 0;
		FILE_STATE = OPENED;
		NEW_FILE = 0;
		return 1;
	      }
	    }
	    
	    if((firstVisDir+selectedDir) < totalDirs) 
	    {//if #1
	      //selected a directory.. navigate to it
	     if(strcmp(dirs[firstVisDir+selectedDir], "..") == 0) 
	     {//if #2
	       char *tmp2;
	       int i = (strrchr(cwd, '/')-cwd);
	       tmp2 = (char *) malloc(i+2);
	       if(!tmp2) { msgBox("Insufficient memory", OK, ERROR); return 0; }
	       strncpy(tmp2, cwd, i);
	       tmp2[i] = '/';
	       tmp2[i+1] = '\0';
	       scanDir(tmp2);
	       free(tmp2);
	     } 
	     else 
	     {
	      char *tmp;
	      if(strstr(dirs[firstVisDir+selectedDir], "..")) 
	      {//if #3
		char *tmp2;
		tmp2 = findFullDirName(dirs[firstVisDir+selectedDir]);
		tmp = (char *) malloc(strlen(cwd)+strlen(tmp2+2));
		if(!tmp) { msgBox("Insufficient memory", OK, ERROR); return 0; }
		strcpy(tmp, cwd);
		strcat(tmp, "/");
		strcat(tmp, tmp2);
		strcat(tmp, "\0");
		scanDir(tmp);
		free(tmp);
	      } 
	      else 
	      {
		tmp = (char *) malloc(strlen(cwd)+
		      strlen(dirs[firstVisDir+selectedDir])+2);
		if(!tmp) { msgBox("Insufficient memory", OK, ERROR); return 0; }
		strcpy(tmp, cwd);
		strcat(tmp, "/");
		strcat(tmp, dirs[firstVisDir+selectedDir]);
		strcat(tmp, "\0");
		scanDir(tmp);
		free(tmp);
	      }//end if #3
	     }//end if #2
	     firstVisDir = 0;
	     selectedDir = 0;
	     refreshDirView();
	     setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	     fprintf(stdout, "\e[%d;%dH", h-2, y+1);
	     printf("File: ");
	     setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	     printf("%s%*s", inputName, (int)(w-y-strlen(inputName)-9), " ");
	     if(sel == 1) fprintf(stdout, "\e[%d;%dH", h-2, y+selChar+7);
	     fflush(stdout);
	    } 
	    else 
	    {
	      //selected a file.. open/save it
	      if(openSave == OPEN) 
	      {
		if(strstr(files[firstVisDir+selectedDir], ".."))
		  findFullFileName(files[firstVisDir+selectedDir-totalDirs]);
		else 
		{
		  //strcpy(open_file_name, files[firstVisDir+selectedDir-totalDirs]);
		  strcpy(open_file_name, cwd);
		  strcat(open_file_name, "/");
		  strcat(open_file_name, inputName);
		}
		if(!_openFile()) return 0;
		FILE_STATE = OPENED;
		NEW_FILE = 0;
		return 1;
	      } 
	      else 
	      {	//save file
		strcpy(open_file_name, cwd);
		strcat(open_file_name, "/");
		strcat(open_file_name, inputName);
		//*****check if the file already exists
		if(fopen(open_file_name, "r")) 
		{
		  int i = msgBox("File already exists. Overwrite?", YES|NO, CONFIRM);
		  if(i == NO) 
		  { 
		    refreshDirView(); 
		    setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	     	    fprintf(stdout, "\e[%d;%dH", h-2, y+1);
		    printf("File: ");
		    setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
		    printf("%s%*s", inputName, (int)(w-y-strlen(inputName)-9), " ");
		    if(sel == 1) fprintf(stdout, "\e[%d;%dH", h-2, y+selChar+7);
		    fflush(stdout);
		    break;
		  }
		}/////////////////////////
		if(!_saveFile()) return 0;
		FILE_STATE = SAVED;
		NEW_FILE = 0;
		return 1;
	      }
	    }//end if #1
	    break;
	  /***************************************************/
	  /***************************************************/
	  /***************************************************/
	  //Navigate up through dirs and files
	  case('p'):
	    if(GNU_DOS_LEVEL > 1 && CTRL) goto do_up;
	    goto do_enter_char; break;
	  case(UP_KEY):
	    if(GNU_DOS_LEVEL > 1) break;
do_up:
	    if(sel == 1) break;
	    if(selectedDir == 0) 
	    {//if #1
	      if(firstVisDir == 0) break;
	      firstVisDir -= numVisDirs;
	      selectedDir = numVisDirs-1;
	      refreshDirView();
	    } 
	    else 
	    {
	      int pos = firstVisDir+selectedDir;
		setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
		WRAP_DIR_OUTPUT;
		selectedDir--; pos--;
		setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
		WRAP_DIR_OUTPUT;
		if(pos >= totalDirs) 
		{//if #6
		  fprintf(stdout, "\x1b[%d;%dH%s", (selectedDir >= numVisDirs) ? 
			    selectedDir-numVisDirs+3:selectedDir+3,
			    (selectedDir >= numVisDirs) ? x+MAX_DIR_NAME_LEN+1:x+1,
			    files[pos-totalDirs]);
		  if(strstr(files[pos], "..")) 
		  {
		    findFullFileName(files[pos-totalDirs]);
		    if(strlen(open_file_name) > w-y-9)
		      strncpy(inputName, open_file_name, w-y-9);
		    else strcpy(inputName, open_file_name);
		  } else strcpy(inputName, files[pos-totalDirs]);
		} 
		else 
		{
		  fprintf(stdout, "\x1b[%d;%dH[%s]", (selectedDir >= numVisDirs) ? 
			    selectedDir-numVisDirs+3:selectedDir+3,
			    (selectedDir >= numVisDirs) ? x+MAX_DIR_NAME_LEN+1:x+1,
			    dirs[pos]);
		}//end if #6
	      //}//end if #2
	    }//end if #1
		fprintf(stdout, "\e[%d;%dH", h-2, y+7);
		setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
		printf("%s%*s", inputName, (int)(w-y-strlen(inputName)-9), " ");
		if(sel == 1) fprintf(stdout, "\e[%d;%dH", h-2, y+selChar+7);
	    //}
	    fflush(stdout);
	    break;
	  /***************************************************/
	  /***************************************************/
	  /***************************************************/
	  //Navigate down through dirs and files
	  case('n'):
	    if(GNU_DOS_LEVEL > 1 && CTRL) goto do_down;
	    goto do_enter_char; break;
	  case(DOWN_KEY):
	    if(GNU_DOS_LEVEL > 1) break;
do_down:
	    if(sel == 1) break;
	    if(selectedDir == (numVisDirs*2)-1) 
	    {//if #1
	      if((firstVisDir+(numVisDirs*2)) < (totalDirs+totalFiles))
	      {
		firstVisDir += numVisDirs;
		selectedDir = numVisDirs;
		refreshDirView();
	      //if SAVE dialog, redraw the input field
	      if(openSave == SAVE) 
	      {
		setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
		fprintf(stdout, "\e[%d;%dH", h-2, y+1);
		printf("File: ");
		setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
		printf("%s%*s", inputName, (int)(w-y-strlen(inputName)-9), " ");
		fprintf(stdout, "\e[%d;%dH", h-2, y+selChar+7);
	      }
		fflush(stdout);
		break;
	      }
	    } 
	    else 
	    {
		int pos = firstVisDir+selectedDir;
		if(pos >= (totalDirs+totalFiles-1)) break;
		setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
		WRAP_DIR_OUTPUT;
		selectedDir++; pos++;
		setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
		if(pos >= totalDirs) 
		{//if #3
		  fprintf(stdout, "\x1b[%d;%dH%s", (selectedDir >= numVisDirs) ? 
			    selectedDir-numVisDirs+3:selectedDir+3,
			    (selectedDir >= numVisDirs) ? x+MAX_DIR_NAME_LEN+1:x+1,
			    files[pos-totalDirs]);
		  if(strstr(files[pos], "..")) 
		  {
		    findFullFileName(files[pos-totalDirs]);
		    if(strlen(open_file_name) > w-y-9)
		      strncpy(inputName, open_file_name, w-y-9);
		    else strcpy(inputName, open_file_name);
		  } else strcpy(inputName, files[pos-totalDirs]);
		} 
		else 
		{
		  fprintf(stdout, "\x1b[%d;%dH[%s]", (selectedDir >= numVisDirs) ? 
			    selectedDir-numVisDirs+3:selectedDir+3,
			    (selectedDir >= numVisDirs) ? x+MAX_DIR_NAME_LEN+1:x+1,
			    dirs[pos]);
		}//end if #3
	    }//end if #1
		fprintf(stdout, "\e[%d;%dH", h-2, y+7);
		setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
		printf("%s%*s", inputName, (int)(w-y-strlen(inputName)-9), " ");
		if(sel == 1) fprintf(stdout, "\e[%d;%dH", h-2, y+selChar+7);
	    fflush(stdout);
	    break;
	  default:
do_enter_char:
	    if(sel == 1) 
	    {
		if(strlen(inputName) >= w-y-9) break;
		int i;
		inputName[strlen(inputName)+1] = '\0';
		for(i = strlen(inputName); i > selChar; i--)
		  inputName[i] = inputName[i-1];
		inputName[selChar++] = (CAPS || SHIFT) ? ch[0]-32 : ch[0];
		fprintf(stdout, "\e[%d;%dH", h-2, y+7);
		setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
		printf("%s%*s", inputName, (int)(w-y-strlen(inputName)-9), " ");
		fprintf(stdout, "\e[%d;%dH", h-2, y+selChar+7);
	    }
	    break;
	}//end switch
      }//end while
      break;
    case(NO):
      if(openSave == OPEN) 
      {//if #1
	if(!_openFile()) 
	{
	  return 0;
	} 
	FILE_STATE = OPENED;
	NEW_FILE = 0;
	return 1;
      } 
      else 
      {
	if(!_saveFile()) 
	{
	  return 0;
	} 
	FILE_STATE = SAVED;
	NEW_FILE = 0;
	return 1;
      }//end if #1
      break;
  }//end switch
  return 0;
}//end openSaveFile()


/***************************************************************
 * This function saves the file specified in open_file_name.
 * It is called indirectly through the function openSaveFile().
 * *************************************************************/
int _saveFile() 
{
  //if(!open_file)
    if(!(open_file = fopen(open_file_name, "w+"))) 
    {
      return 0;
    }
    
  int i, j;
  for(i = 0; i < totalLines; i++) 
  {
    for(j = 0; j < strlen(lines[i]); j+=4)
    {
      fputc(lines[i][j], open_file);
      if ((lines[i][j] & mask[0]) == mask[0]) fputc(lines[i][j+1], open_file);
      if ((lines[i][j] & mask[1]) == mask[1]) fputc(lines[i][j+2], open_file);
      if ((lines[i][j] & mask[2]) == mask[2]) fputc(lines[i][j+3], open_file);
    }
    //fputs(lines[i], open_file);
    if(!LINE_IS_LINKED[i] && lines[i][j-4] != '\n') fputc('\n', open_file);
  }
  FILE_STATE = SAVED;
  if(!documentTitle) documentTitle = (char *) malloc(MAX_FILE_NAME_LEN*4);
  if(!documentTitle) { msgBox("Insufficient memory", OK, ERROR); return 0; }
  if(strcmp(documentTitle, open_file_name) != 0)
  {
    if(strrchr(open_file_name, '/'))
      strcpy(documentTitle, strrchr(open_file_name, '/')+1);
    else
      strcpy(documentTitle, open_file_name);
  }
  fflush(open_file); 

  checkFileExtension();
  return 1;
}

/***************************************************************
 * This function opens the file specified in open_file_name.
 * It is called indirectly through the function openSaveFile().
 * *************************************************************/
int _openFile() 
{
  if(!(open_file = fopen(open_file_name, "r+"))) 
  {
    return 0;
  }
  //make a backup copy of the opened file, so if things went
  //wrong, we don't lose our original file!!
  char *backup_file_name = (char *) malloc(strlen(open_file_name)+2);
  if(!backup_file_name) { msgBox("Insufficient memory", OK, ERROR); return 0; }
  strcpy(backup_file_name, open_file_name);
  strcat(backup_file_name, "~\0");
  FILE *backup_file = fopen(backup_file_name, "w");
  
  total_tabs = 0;
  int j = 0;
  int i = 0;
  totalLines = 0;

  for(i = 0; i < MAX_LINES; i++) 
  {
    if(!lines[i]) lines[i] = (char *) 
	  malloc((MAX_CHARS_PER_LINE+TAB_CHARS)*4);
    if(!lines[i]) { msgBox("Insufficient memory", OK, ERROR); return 0; }
    strcpy(lines[i], "\0");
  }//end for
  i = 0;
  int k = 0;
  while(!feof(open_file))
  {
    if(totalLines >= MAX_LINES) break;
    j = fgetc(open_file);
    fputc(j, backup_file);
    if(k == MAX_CHARS_PER_LINE)
    {
      LINE_IS_LINKED[i] = 1;
      i++; k = 0;
      totalLines++;
    }
    lines[i][k] = j;
    if(j == '\n')
    {
      LINE_IS_LINKED[i] = 0;
      i++; k = 0;
      totalLines++;
    }
    else
    {
      k++;
    }
  }
  fflush(backup_file);
  
  char *t = (char *)malloc(MAX_CHARS_PER_LINE*4);
  if(!t) { msgBox("Insufficient memory", OK, ERROR); return 0; }
  for(i = 0; i < totalLines; i++) 
  {
    /////////////////////////////////////////////////////////
    //we will expand all chars in line into 4-byte length..
    /////////////////////////////////////////////////////////
    int x, z;
    char c;
    int carry = 0;
    int len;
    len = strlen(lines[i]);
    for(x = 0; x < strlen(lines[i]); x++) 
    {
      if(x+carry >= MAX_CHARS_PER_LINE*4)
      {
	if(!LINE_IS_LINKED[i])
	{
	  move_lines_down(totalLines, i+1);
	  lines[i+1][0] = '\0';
	  LINE_IS_LINKED[i] = 1;
	}
	strcpy(t, lines[i]+x);
	lines[i][x] = '\0';
	strcpy(tmp2, lines[i+1]);
	strcpy(lines[i+1], t);
	strcat(lines[i+1], tmp2);
      }

      z = 1;
      if ((lines[i][x] & mask[0]) == mask[0]) z++;
      if ((lines[i][x] & mask[1]) == mask[1]) z++;
      if ((lines[i][x] & mask[2]) == mask[2]) z++;
      strcpy(t, lines[i]+x+z);
      c = lines[i][x];
      if(c == '\0')
      {
	break;
      }
      int w;
      for(w = z; w < 4; w++) lines[i][x+w] = ' ';
      lines[i][x+w] = '\0';
      strcat(lines[i], t);
      
      if(c == '\t')
      {
	j = TAB_CHARS -(((x/4)+carry)%TAB_CHARS);
	j = 0 ? TAB_CHARS : j;
	carry += (j-1)*4;
	len += carry;
      }
      x += (w-1);
    }
    //int len2;
    //len2 = strlen(lines[i]);
    calcTotalCharsInLine(i);
  }
  free(t);
  
  if(!documentTitle) documentTitle = (char *) malloc(MAX_FILE_NAME_LEN*4);
  if(!documentTitle) { msgBox("Insufficient memory", OK, ERROR); return 0; }
  if(strrchr(open_file_name, '/'))
    strcpy(documentTitle, strrchr(open_file_name, '/')+1);
  else
    strcpy(documentTitle, open_file_name);
  firstVisLine = 0;
  selectedLine = 0;
  selectedChar = 0;

  if(backup_file) fclose(backup_file);
  free(backup_file_name);
  
  checkFileExtension();
  return 1;
}

void checkFileExtension() 
{
  //search for the last dot in the filename
  char *i = strrchr(open_file_name, '.');
  if(i != NULL)	//was there a dot?
  {
    //check what was the file extension:
    //is it a C source/header file?
    if(strcmp(i, ".c") == 0 || strcmp(i, ".h") == 0)
    {
      AUTO_HIGHLIGHTING = 1; BG_COLOR[COLOR_WINDOW] = COLOR_HWINDOW;
      HIGHLIGHT_MODE = C_MODE;
    } 
    //is it a C++ source/header file?
    else if(strcmp(i, ".C") == 0 || strcmp(i, ".cpp") == 0
       || strcmp(i, ".CPP") == 0 || strcmp(i, ".cc") == 0
       || strcmp(i, ".c++") == 0 || strcmp(i, ".cxx") == 0
       || strcmp(i, ".hpp") == 0 || strcmp(i, ".cp") == 0)
    {
      AUTO_HIGHLIGHTING = 1; BG_COLOR[COLOR_WINDOW] = COLOR_HWINDOW;
      HIGHLIGHT_MODE = CPP_MODE;
    } 
    //is it a shell script file?
    else if(strcmp(i, ".sh") == 0)
    {
      AUTO_HIGHLIGHTING = 1; BG_COLOR[COLOR_WINDOW] = COLOR_HWINDOW;
      HIGHLIGHT_MODE = SHELL_MODE;
    } 
    //is it a PERL script file?
    else if(strcmp(i, ".pl") == 0 || strcmp(i, ".PL") == 0)
    {
      AUTO_HIGHLIGHTING = 1; BG_COLOR[COLOR_WINDOW] = COLOR_HWINDOW;
      HIGHLIGHT_MODE = PERL_MODE;
    }
    //is it a texi file?
    else if(strcmp(i, ".texi") == 0 || strcmp(i, ".txi") == 0
	    || strcmp(i, ".texinfo") == 0)
    {
      AUTO_HIGHLIGHTING = 1; BG_COLOR[COLOR_WINDOW] = COLOR_HWINDOW;
      HIGHLIGHT_MODE = TEXI_MODE;
    }
    //is it an assembly file?
    else if(strcmp(i, ".asm") == 0 || strcmp(i, ".ASM") == 0)
    {
      AUTO_HIGHLIGHTING = 1; BG_COLOR[COLOR_WINDOW] = COLOR_HWINDOW;
      HIGHLIGHT_MODE = ASM_MODE;
    }
    //is it python source file?
    else if(strcmp(i, ".py") == 0 || strcmp(i, ".PY") == 0)
    {
      AUTO_HIGHLIGHTING = 1; BG_COLOR[COLOR_WINDOW] = COLOR_HWINDOW;
      HIGHLIGHT_MODE = PYTHON_MODE;
    }
    //is it JavaScript file?
    else if(strcmp(i, ".js") == 0 || strcmp(i, ".JS") == 0)
    {
      AUTO_HIGHLIGHTING = 1; BG_COLOR[COLOR_WINDOW] = COLOR_HWINDOW;
      HIGHLIGHT_MODE = JAVASCRIPT_MODE;
    }
    //is it an HTML webpage?
    else if(strcmp(i, ".html") == 0 || strcmp(i, ".htm") == 0
	    || strcmp(i, ".HTML") == 0 || strcmp(i, ".HTM") == 0)
    {
      AUTO_HIGHLIGHTING = 1; BG_COLOR[COLOR_WINDOW] = COLOR_HWINDOW;
      HIGHLIGHT_MODE = HTML_MODE;
    }
    //is it a BASIC file?
    else if(strcmp(i, ".bas") == 0 || strcmp(i, ".BAS") == 0)
    {
      AUTO_HIGHLIGHTING = 1; BG_COLOR[COLOR_WINDOW] = COLOR_HWINDOW;
      HIGHLIGHT_MODE = BASIC_MODE;
    }
    //is it a Pascal file?
    else if(strcmp(i, ".pas") == 0 || strcmp(i, ".PAS") == 0)
    {
      AUTO_HIGHLIGHTING = 1; BG_COLOR[COLOR_WINDOW] = COLOR_HWINDOW;
      HIGHLIGHT_MODE = PASCAL_MODE;
    }
    //is it a Fortran 77 file?
    else if(strcmp(i, ".f") == 0 || strcmp(i, ".for") == 0
	    || strcmp(i, ".F") == 0 || strcmp(i, ".FOR") == 0)
    {
      AUTO_HIGHLIGHTING = 1; BG_COLOR[COLOR_WINDOW] = COLOR_HWINDOW;
      HIGHLIGHT_MODE = F77_MODE;
    }
    //NONE of the above...
    else 
    { 
      AUTO_HIGHLIGHTING = 0; BG_COLOR[COLOR_WINDOW] = old_window_color;
      HIGHLIGHT_MODE = NO_MODE;
    }//end if
  }
  /////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////
  //No dot in the file name, check file content
  /////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////
  else
  {
    //is it a shell script file?
    if((strstr(lines[0], "/bin/csh")) || (strstr(lines[0], "/bin/ksh")) ||
       (strstr(lines[0], "/bin/sh")) || (strstr(lines[0], "/bin/bash")))
    {
      AUTO_HIGHLIGHTING = 1; BG_COLOR[COLOR_WINDOW] = COLOR_HWINDOW;
      HIGHLIGHT_MODE = SHELL_MODE;
    } 
    //is it a PERL script file?
    else if(strstr(lines[0], "/bin/perl"))
    {
      AUTO_HIGHLIGHTING = 1; BG_COLOR[COLOR_WINDOW] = COLOR_HWINDOW;
      HIGHLIGHT_MODE = PERL_MODE;
    }
    //NONE of the above...
    else 
    { 
      AUTO_HIGHLIGHTING = 0; BG_COLOR[COLOR_WINDOW] = old_window_color;
      HIGHLIGHT_MODE = NO_MODE;
    }//end if
  }
  if(AUTO_HIGHLIGHTING) loadKeywords(HIGHLIGHT_MODE);
}
