/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2013, 2014, 2015 (c)
 * 
 *    file: file.c
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

#include "defs.h"
#include <string.h>
#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>
#include <pwd.h>

extern char *tmp, *tmp2;

static int one(const struct dirent *unused) 
{
  return 1;
}

struct stat statbuf;
FILE *file;

/***********************************************
 * This function exports the tree of directory
 * passed as argv[1] into file passed as argv[2]
 * from the command line.
 * *********************************************/
void exportTreeFromCommandLine(char *d, char *f) 
{
  if(!(file = fopen(f, "w"))) 
  {
    printf("Error opening export file '%s'.\nAborting.\n", f);
    exit(1);
  }
  
  //cwd = getcwd(NULL, 0);

  printf("Reading directory tree. Please wait..\n");
  fprintf(file, "\nDirectory tree of '%s/':", d);
  scanThisDir(d, 0, 0);
  
  printf("Finished writing directory tree of '%s' to '%s'.\n", d, f);
  fclose(file);
  return;
}

/***********************************************
 * This function exports the tree under the
 * current working directory into an external
 * file as input by the user.
 * *********************************************/
void exportTree(int showFileNameDialogBox) 
{
  if(showFileNameDialogBox == YES) 
  {	//show dialog box
    char *f = inputBox("The directory tree of the current dir will be "
		       "exported.\nEnter export file name:", "Export");
    if(f == NULL) return;
  }
  
  //check for '~'
  strcpy(tmp, input);
  if(strchr(tmp, '~')) 
  {
    strcpy(tmp2, input+((strchr(input, '~')+1)-input));
    struct passwd *pass;//will be used to find the home dir
    if((pass = getpwuid(geteuid())))
	  ;//printf("\nError: couldn't open home directory.\n");
    strcpy(tmp, pass->pw_dir);
    strcat(tmp, "/");
    strcat(tmp, tmp2);
    strcat(tmp, "\0");
  }
  if(!(file = fopen(tmp, "w"))) 
  {
    msgBox("Error opening export file. Aborting.", OK, ERROR);
    fprintf(log_file, "Error opening file '%s' to export tree of '%s'..\n", 
	    tmp, cwd);
    refreshWindows();
    return;
  }
  
  fprintf(file, "\nDirectory tree of '%s/':", cwd);
  char *t = (char *) malloc(strlen(cwd));
  if(!t) { msgBox("Insufficient memory", OK, ERROR); return; }
  strcpy(t, cwd);
  scanThisDir(t, 0, 1);
  free(t);
  
  fprintf(log_file, "Exported tree of '%s' to file '%s'..\n", cwd, tmp);
  fclose(file);

  int x = chdir(cwd);
  if(x == -1) { msgBox("Error changing directory", OK, ERROR); return; }
  //free(tmp);
  return;
}

/***********************************************
 * This function scans the directory passed to
 * it as tmp[] for exporting. Not to be called
 * directly. Instead, called from exportTree().
 * *********************************************/
void scanThisDir(char tmp[], int level, int showProgress) 
{
  static int nf = 0;
  static int nd = 0;	//static: total num of files and dirs
  int n;
  struct dirent **eps;	//structure used in exportTree() function
  
  //variables used in displaying progress message to the user
  static char exportingString[] = "Exporting %d of %d";
  //
  
  n = scandir(tmp, &eps, one, alphasort);
  if(n >= 0) 
  {
    //fprintf(file, "\nDirectory tree of '%s':\n", tmp);
    int cnt;
    if(showProgress) 
      fprintf(log_file, "Exporting dir of: %s, level: %d, children (n):%d\n",
	      tmp, level, n);
    for(cnt = 0; cnt < n; ++cnt) 
    {  
      if(level == 0 && showProgress) 
      {
	drawBox((SCREEN_H/2)-2, (SCREEN_W/2)-12,
		(SCREEN_H/2)+1, (SCREEN_W/2)+12, NULL, YES);
	fprintf(stdout, "\e[%d;%dH", (SCREEN_H/2)-1, (SCREEN_W/2)-11);
	printf(exportingString, cnt+1, n);
      }//end if -- display progress message to user
      
      int z = lstat(eps[cnt]->d_name,&statbuf);
      if(z == -1) { msgBox(strerror(errno), OK, ERROR); return; }
      
      if(S_ISDIR(statbuf.st_mode)) 
      {
	if(strcmp(eps[cnt]->d_name, ".") == 0 
	   || strcmp(eps[cnt]->d_name, "..") == 0)	//ignore "." & ".."
	  continue;
	fprintf(file, "\n%*s|---- %s", level, " ", eps[cnt]->d_name);
	//if(showProgress) fprintf(log_file, "Exporting dir: %s, cnt:%d\n", eps[cnt]->d_name, cnt);
	
	char *tmp2 = (char *)malloc(strlen(tmp)+strlen(eps[cnt]->d_name)+2);
	if(!tmp2) { msgBox("Insufficient memory", OK, ERROR); return; }
	strcpy(tmp2, tmp);
	strcat(tmp2, "/");
	strcat(tmp2, eps[cnt]->d_name);
	strcat(tmp2, "\0");
	
	nd++;
	int i;
	i = chdir(tmp2);
	if(i == -1) { msgBox("Error changing directory", OK, ERROR); return; }
	//scanThisDir(eps[cnt]->d_name, level+4);
	scanThisDir(tmp2, level+4, showProgress);
	i = chdir("..");
	free(tmp2);
	if(i == -1) { msgBox("Error changing directory", OK, ERROR); return; }
	continue;
      } 
      else 
      {
	fprintf(file, "\n%*s|-[f] %s", level, " ", eps[cnt]->d_name);
	//if(showProgress) fprintf(log_file, "Exporting file: %s, level: %d, cnt:%d\n", eps[cnt]->d_name, level, cnt);
	nf++;
      }
    }
  }
  else 
  {
    if(showProgress) 
      fprintf(log_file, "Error opening dir: %s, level: %d\n", tmp, level);
    fprintf(file, " (Couldn't open the directory)");
  }
 //}

 if(level == 0) 
 {
    fprintf(file, "\n---------------------------------\n");
    fprintf(file, "Total dirs: %d, Total files: %d\n", nd, nf);
  }
}
