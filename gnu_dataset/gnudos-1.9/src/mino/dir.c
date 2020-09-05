/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014 (c)
 * 
 *    file: dir.c
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
#include "options.h"
#include <sys/ioctl.h>	//included for terminal size query
#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

#define DIR_RETURN_ERROR()		\
{					\
    msgBox(strerror(errno), OK, ERROR);	\
    return 0;				\
}

static int one(const struct dirent *unused) 
{
  return 1;
}

int scanDir(char *dir) 
{
  int dcount = 0;
  int fcount = 0;
  int n;
  struct dirent **eps;
  struct stat st;

  int x = chdir(dir);
  if(x == -1) DIR_RETURN_ERROR();
  
  cwd = getcwd(NULL, 0);
  if(cwd == NULL) DIR_RETURN_ERROR();
  
  x = lstat(dir, &st);
  if(x == -1) DIR_RETURN_ERROR();
 
 if(S_ISDIR(st.st_mode)) 
 {
  n = scandir(dir, &eps, one, alphasort);
  if(n >= 0) 
  {
    int cnt;
    for(cnt = 0; cnt < n; ++cnt) 
    {
      x = lstat(eps[cnt]->d_name,&st);
      if(x == -1) DIR_RETURN_ERROR();
      
      if(S_ISDIR(st.st_mode)) 
      {
	if(strcmp(eps[cnt]->d_name, ".") == 0) //ignore "."
	  continue;
	if(strlen(eps[cnt]->d_name) > MAX_DIR_NAME_LEN) 
	{
	  int i;
	  for(i = 0; i < MAX_DIR_NAME_LEN-2; i++)
	    dirs[dcount][i] = eps[cnt]->d_name[i];
	  dirs[dcount][i] = '.';
	  dirs[dcount][i+1] = '.';
	  dirs[dcount++][i+2] = '\0';
	} else  { strcpy(dirs[dcount++], eps[cnt]->d_name); }
      } 
      else 
      {
	if(strlen(eps[cnt]->d_name) > MAX_DIR_NAME_LEN) 
	{
	  int i;
	  for(i = 0; i < MAX_DIR_NAME_LEN-2; i++)
	    files[fcount][i] = eps[cnt]->d_name[i];
	  files[fcount][i] = '.';
	  files[fcount][i+1] = '.';
	  files[fcount++][i+2] = '\0';
	} else { strcpy(files[fcount++], eps[cnt]->d_name); }
      } //end else
    } //end for
  } 
  else 
  {	//end inner if
    char *tmp = (char *) malloc(strlen(dir)+21);
    if(!tmp) { msgBox("Insufficient memory", OK, ERROR); return 0; }
    sprintf(tmp, "Error opening dir:\n%s", dir);
    strcat(tmp, "\0");
    msgBox(tmp, OK, ERROR);
    free(tmp);
    return 0;
  } //end else
  
 } //end outer if
 //chdir("..");
 totalDirs = dcount;
 totalFiles = fcount;
 return 1;
}

/***************************************
 * refreshDirView(): 
 * Procedure to refresh the left window
 * showing directory tree.
 * **************************************/
void refreshDirView() 
{
 if(strlen(cwd) > w-y-5) 
 {
   char *tmp;
   tmp = (char *) malloc(w-y+1);
   if(!tmp) { msgBox("Insufficient memory", OK, ERROR); return; }
   
   int i, k, j = w-y-5;
   for(i = strlen(cwd), k = j; k > 1; i--, k--) tmp[k] = cwd[i];
   tmp[0] = '.';
   tmp[1] = '.';
   drawBox(x, y, h, w, tmp, YES);
   free(tmp);
 } else drawBox(x, y, h, w, cwd, YES);
 
 //show control message at the bottom
 fprintf(stdout, "\e[%d;%dH", h-1, y+1);
 setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
 if(GNU_DOS_LEVEL > 2)
    printf("[ENTER]: Open dir/file [C-p C-n C-f C-b]: Navigate [C-g]: Cancel");
 else if(GNU_DOS_LEVEL > 1)
    printf("[ENTER]: Open dir/file [C-p C-n C-f C-b]: Navigate [ESC]: Cancel");
 else
    printf("[ENTER]: Open dir/file  [ARROWS]: Navigate  [ESC]: Cancel");
 fprintf(stdout, "\e[%d;%dH", h-2, y+1);
 printf("File: ");
 
 int i = firstVisDir;
 int j = 0; int k = 0;
 int curX, curY;
 
 while(i < totalDirs) 
 {
      if((i-firstVisDir) == selectedDir) 
      {
	setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	fprintf(stdout, "\x1b[%d;%dH[%s]", (j >= numVisDirs) ? j-numVisDirs+3:j+3,
				       (j >= numVisDirs) ? x+MAX_DIR_NAME_LEN+1:x+1,
				       dirs[i]);
	curX = (j >= numVisDirs) ? j-numVisDirs+3 : j+3;
	curY = (j >= numVisDirs) ? x+MAX_DIR_NAME_LEN+strlen(dirs[i])
				  : x+strlen(dirs[i]); curY += 3;
      } 
      else 
      {
	setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	fprintf(stdout, "\x1b[%d;%dH[%s]", (j >= numVisDirs) ? j-numVisDirs+3:j+3,
				       (j >= numVisDirs) ? x+MAX_DIR_NAME_LEN+1:x+1,
				       dirs[i]);
      }
      if(j >= (numVisDirs*2)-1) break;
      i++; j++; k++;
 }
 //if there is more room, show the files
 if((i >= totalDirs) && (j < numVisDirs*2)) 
 {
    if(firstVisDir > totalDirs) j = firstVisDir-totalDirs;
    else j = 0;
    
    while(j < totalFiles) 
    {
      if((i-firstVisDir) == selectedDir) 
      {
	setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	fprintf(stdout, "\x1b[%d;%dH%s", (k >= numVisDirs) ? k-numVisDirs+3:k+3,
				       (k >= numVisDirs) ? x+MAX_DIR_NAME_LEN+1:x+1,
				       files[j]);
	curX = (j >= numVisDirs) ? j-numVisDirs+3 : j+3;
	curY = (j >= numVisDirs) ? x+MAX_DIR_NAME_LEN+strlen(files[j])
				  : x+strlen(files[j]); curY += 3;
      } 
      else 
      {
	setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	fprintf(stdout, "\x1b[%d;%dH%s", (k >= numVisDirs) ? k-numVisDirs+3:k+3,
				       (k >= numVisDirs) ? x+MAX_DIR_NAME_LEN+1:x+1,
				       files[j]);
      }
      if(j >= totalFiles) break;
      if(k >= (numVisDirs*2)-1) break;
      i++; j++; k++;
    }//end while
 }//end if 

 //reposition the cursor
 fprintf(stdout, "\e[%d;%dH", curX, curY);
 fflush(stdout);
}
