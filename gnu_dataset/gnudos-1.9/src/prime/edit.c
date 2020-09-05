/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2013, 2014, 2015 (c)
 * 
 *    file: edit.c
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
#include "edit.h"
#include "cutcopy.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <dirent.h>
#include <sys/stat.h>
#include <errno.h>
#include <unistd.h>

int CONFIRM_CUT_COPY;
int ABORT_COPY;

static int one(const struct dirent *unused) 
{
  return 1;
}

struct stat statbuf;

mode_t get_mode(__mode_t st_mode)
{
  mode_t mode = 00;
  if(st_mode & S_IRUSR) { mode += 0400; }
  if(st_mode & S_IWUSR) { mode += 0200; }
  if(st_mode & S_IXUSR) { mode += 0100; }
  if(st_mode & S_IRGRP) { mode +=  040; }
  if(st_mode & S_IWGRP) { mode +=  020; }
  if(st_mode & S_IXGRP) { mode +=  010; }
  if(st_mode & S_IROTH) { mode +=   04; }
  if(st_mode & S_IWOTH) { mode +=   02; }
  if(st_mode & S_IXOTH) { mode +=   01; }
  return mode;
}

void clearSelection() 
{
  int a = activeWindow;
  activeWindow = DIR_WIN; unMarkAll();
  activeWindow = FILE_WIN; unMarkAll();
  activeWindow = a;

  numCopy = 0;
  numCut = 0;
  numStarred = 0;
}

/********************************************
 * Marks all items in the active window with
 * a '*' mark.
 * ******************************************/
void markAll() 
{
  int i, j = 0;
  if(activeWindow == DIR_WIN) 
  {
    for(i = 0; i < totalDirs; i++) 
    {
      if((strcmp(dirs[i], ".") == 0) 	//ignore '.' and '..'
	 || (strcmp(dirs[i], "..") == 0)) continue;
      if(dirStar[i] == '*') continue;
      if(dirStar[i] == '^') { removeCutDir(i); numCut--; }
      if(dirStar[i] == '#') { removeCopyDir(i); numCopy--; }
      dirStar[i] = '*';
      j++;
    } i--;
    refreshDirView();
  } 
  else 
  {
    for(i = 0; i < totalFiles; i++) 
    {
      if(strcmp(files[i], "(Empty folder)") == 0) break;//means dir is empty!!
      if(fileStar[i] == '*') continue;
      if(fileStar[i] == '^') { removeCutFile(i); numCut--; }
      if(fileStar[i] == '#') { removeCopyFile(i); numCopy--; }
      fileStar[i] = '*';
      j++;
    }	//end for
    refreshFileView();
  }	//end if
  numStarred += j;
  refreshBottomView();
}

/********************************************
 * Removes all marks on items in the active 
 * window.
 * ******************************************/
void unMarkAll() 
{
  int i, j = 0;
  if(activeWindow == DIR_WIN) 
  {
    for(i = 0; i < totalDirs; i++) 
    {
      if(dirStar[i] == ' ') continue;
      if(dirStar[i] == '^') { removeCutDir(i); numCut--; }
      if(dirStar[i] == '#') { removeCopyDir(i); numCopy--; }
      dirStar[i] = ' ';
      j++;
    } i++;
    refreshDirView();
  } 
  else 
  {
    for(i = 0; i < totalFiles; i++) 
    {
      if(strcmp(files[i], "(Empty folder)") == 0) break;//means dir is empty!!
      if(fileStar[i] == ' ') continue;
      if(fileStar[i] == '^') { removeCutFile(i); numCut--; }
      if(fileStar[i] == '#') { removeCopyFile(i); numCopy--; }
      fileStar[i] = ' ';
      j++;
    }
    refreshFileView();
  }
  numStarred -= j;
  refreshBottomView();
}

/********************************************
 * Marks one item for cut in the active 
 * window with a '^' mark.
 * ******************************************/
void cutOne() 
{
       if(activeWindow == DIR_WIN) 
       { 
	 if((strcmp(dirs[firstVisDir+selectedDir], ".") == 0)//ignore . & ..
	    || (strcmp(dirs[firstVisDir+selectedDir], "..") == 0)) return;
	 if(dirStar[firstVisDir+selectedDir] == '^') return;
	 if(dirStar[firstVisDir+selectedDir] == '*') numStarred--;
	 if(dirStar[firstVisDir+selectedDir] == '#') 
	 {
	   removeCopyDir(firstVisDir+selectedDir); 
	   numCopy--; 
	 }
	 dirStar[firstVisDir+selectedDir] = '^'; 
	 saveCutDir(firstVisDir+selectedDir);
	 numCut++;
      } 
      else 
      { 
	if(strcmp(files[firstVisFile+selectedFile], "(Empty folder)") == 0) 
	  return;//this means folder is empty!!
	if(fileStar[firstVisFile+selectedFile] == '^') return;
	if(fileStar[firstVisFile+selectedFile] == '*') numStarred--;
	if(fileStar[firstVisFile+selectedFile] == '#') 
	{
	  removeCopyFile(firstVisFile+selectedFile); 
	  numCopy--;
	}
	fileStar[firstVisFile+selectedFile] = '^'; 
	saveCutFile(firstVisFile+selectedFile);
	numCut++; 
      }
       refreshFileView();
       refreshDirView();
       refreshBottomView();
}

/********************************************
 * Sets marked items for cut in the active 
 * window with a '^' mark.
 * ******************************************/
void cutMarked() 
{
    if(numStarred == 0) 
    {		//if no starred items, just mark the selected item
	cutOne();
	return;
    } 
    else 
    {			//mark the starred items by changing '*' to '^'
       int i = 0;
       while(i < totalDirs) 
       { 
	 if(dirStar[i] == '*') 
	 { 
	   dirStar[i] = '^'; numCut++;
	   saveCutDir(i);
	}
	 i++;
       } //end while
       i = 0;
       while(i < totalFiles) 
       { 
	 if(fileStar[i] == '*') 
	 { 
	   fileStar[i] = '^'; numCut++; 
	   saveCutFile(i);
	} //end while
	 i++;
       }
       numStarred = 0;
       refreshFileView();
       refreshDirView();
       refreshBottomView();
     }
}

/********************************************
 * Marks one item for copy in the active 
 * window with a '#' mark.
 * ******************************************/
void copyOne() 
{
       if(activeWindow == DIR_WIN) 
       { 
	 if((strcmp(dirs[firstVisDir+selectedDir], ".") == 0)//ignore . & ..
	    || (strcmp(dirs[firstVisDir+selectedDir], "..") == 0)) return;
	 if(dirStar[firstVisDir+selectedDir] == '#') return;
	 if(dirStar[firstVisDir+selectedDir] == '*') numStarred--;
	 if(dirStar[firstVisDir+selectedDir] == '^') 
	 {
	   removeCutDir(firstVisDir+selectedDir); 
	   numCut--;
	 }
	 dirStar[firstVisDir+selectedDir] = '#'; 
	 saveCopyDir(firstVisDir+selectedDir); 
	 numCopy++;
      } 
      else 
      { 
	if(strcmp(files[firstVisFile+selectedFile], "(Empty folder)") == 0) 
	  return;//this means folder is empty!!
	if(fileStar[firstVisFile+selectedFile] == '#') return;
	if(fileStar[firstVisFile+selectedFile] == '*') numStarred--;
	if(fileStar[firstVisFile+selectedFile] == '^') 
	{
	  removeCutFile(firstVisFile+selectedFile);
	  numCut--;
	}
	fileStar[firstVisFile+selectedFile] = '#'; 
	saveCopyFile(firstVisFile+selectedFile);
	numCopy++; 
      }
       refreshFileView();
       refreshDirView();
       refreshBottomView();
}


/********************************************
 * Sets marked items for copy in the active 
 * window with a '^' mark.
 * ******************************************/
void copyMarked() 
{
    if(numStarred == 0) 
    {		//if no starred items, just mark the selected item
	//redraw main window but don't clear the area
        drawBox(1, 1, SCREEN_H, SCREEN_W, " Prime (Prime) for GNU/Linux ", NO);
	copyOne();
	return;
    } 
    else 
    {				//mark the starred items by changing '*' to '^'
       int i = 0;
       while(i < totalDirs) 
       { 
	 if(dirStar[i] == '*') 
	 { 
	   dirStar[i] = '#'; numCopy++; 
	   saveCopyDir(i);
	} //end if
	 i++;
       } //end while
       i = 0;
       while(i < totalFiles) 
       { 
	 if(fileStar[i] == '*') 
	 { 
	   fileStar[i] = '#'; numCopy++; 
	   saveCopyFile(i);
	} //end if
	 i++;
       } //end while
       numStarred = 0;
       refreshFileView();
       refreshDirView();
       refreshBottomView();
       //redraw main window but don't clear the area
       drawBox(1, 1, SCREEN_H, SCREEN_W, " Prime (Prime) for GNU/Linux ", NO);
     }
}

/********************************************
 * this function pastes all the files/dirs
 * marked for cut/copy.. it puts them in the
 * cwd path.
 * ******************************************/
void pasteMarked() 
{
  if(numCut == 0 && numCopy == 0) 
  {
    msgBox("Nothing marked for Cut/Copy.", OK, INFO);
    setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
    return;
  }

  CONFIRM_CUT_COPY = 0;
  ABORT_COPY = 0;
  int i;
  /********************************************
   * First copy files marked for copying
   * ******************************************/  
  for(i = 0; i < numCopyFiles; i++) 
  {
    if(ABORT_COPY) goto finish;
    copyThisFile(copyFiles[i]);
  } //END FOR
  numCopyFiles = 0;

  /********************************************
   * Then move files marked for moving
   * ******************************************/  
  for(i = 0; i < numCutFiles; i++) 
  {
    if(ABORT_COPY) goto finish;
    copyThisFile(cutFiles[i]);
    if(remove(cutFiles[i])) 
    {	//success returns 0, otherwise err value
      msgBox("Unable to remove file!.", OK, ERROR);
      fprintf(log_file, "Unable to remove file '%s'..\n", cutFiles[i]);
    }
  } //END FOR
  numCutFiles = 0;

  /********************************************
   * Now copy dirs marked for copying
   * ******************************************/  
  for(i = 0; i < numCopyDirs; i++) 
  {
    if(ABORT_COPY) goto finish;
    copyThisDir(copyDirs[i], 0);
  }  
  numCopyDirs = 0;
  
  /********************************************
   * And move dirs marked for moving
   * ******************************************/  
  for(i = 0; i < numCutDirs; i++) 
  {
    if(ABORT_COPY) goto finish;
    moveThisDir(cutDirs[i], 0);
  }  
  numCutDirs = 0;
  
  numCopy = 0; numCut = 0;

finish:
  CONFIRM_CUT_COPY = 0;
  ABORT_COPY = 0;
  
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  scanDir(cwd);
} //END OF FUNCTION

/****************************************
 * copies the file sent as fileName into
 * the CWD with the same fileName.
 * **************************************/
void copyThisFile(char *fileName) 
{
  char *str;
  FILE *inFile;
  FILE *outFile;
  char buf[4096];
  int read_size;

  if(!(inFile = fopen(fileName, "rb"))) 
  {//check the file for reading
	fprintf(log_file, "Error opening file for read: %s\n", fileName);
	char *s = (char *) malloc(strlen(fileName)+36);
	if(!s) { msgBox("Insufficient memory", OK, ERROR); return; }
	strcpy(s, "Error opening read file:\n");
	strcat(s, fileName);
	strcat(s, "\nAborting.");
	strcat(s, "\0");
	msgBox(s, OK, ERROR); 
	//fprintf(log_file, "Error opening file '%s'..\n", fileName);
	free(s); return;
  } //end if
  //find the last '/' in the path, add one for the start of the filename
  str = strrchr(fileName, '/'); str++; 
  if((outFile = fopen(str, "rb"))) 
  { //check if out file exists
      int j;
      fclose(outFile);
      if(!CONFIRM_CUT_COPY) 
      {
	char *s = (char *) malloc(strlen(fileName)+34);
	if(!s) { msgBox("Insufficient memory", OK, ERROR); return; }
	strcpy(s, "File:\n");
	strcat(s, fileName);
	strcat(s, "\nalready exists. Overwrite?");
	strcat(s, "\0");
	fprintf(log_file, "File '%s' already exists..\n", fileName);
	j = msgBox(s, YES|NO|ALL, CONFIRM);
	free(s);
	if(j == NO) return;
	if(j == ABORT) { ABORT_COPY = 1; return; }
	if(j == ALL) CONFIRM_CUT_COPY = 1;
      }
  } //end if
  if(!(outFile = fopen(str, "wb"))) 
  { //open out file for writing
      fprintf(log_file, "Error opening file for write: %s\n", str);
	char *s = (char *) malloc(strlen(fileName)+37);
	if(!s) { msgBox("Insufficient memory", OK, ERROR); return; }
	strcpy(s, "Error opening write file:\n");
	strcat(s, fileName);
	strcat(s, "\nAborting.");
	strcat(s, "\0");
	msgBox(s, OK, ERROR); 
	//fprintf(log_file, "Error opening file '%s'..\n", fileName);
	free(s); return;
  } //end if
    
  //start copying infile to outfile in chunks//
  while((read_size = fread(buf, sizeof(char), sizeof(buf), inFile))) 
  {
      fwrite(buf, sizeof(char), read_size, outFile);
  } //END WHILE
    
  fclose(inFile);
  fclose(outFile);
  /* Set the file mode */
  struct stat statbuf;
  mode_t mode = 00;
  int z = lstat(fileName, &statbuf);
  if(z == -1) { msgBox(strerror(errno), OK, ERROR); return; }
  mode = get_mode(statbuf.st_mode);
  if(chmod(str, mode) != 0) 
  {
    msgBox(strerror(errno), OK, ERROR);
  }
}

/**********************************************
 * copies the dir sent as tmp[] recursively
 * making new subdirs in the CWD if necessary.
 * ********************************************/
void copyThisDir(char tmp[], int level) 
{
  char *str;

  int n;
  struct dirent **eps;

  //find the last '/' in the path, add one for the start of the dirname
  str = strrchr(tmp, '/'); str++; 
  //show progress to the user
  drawBox((SCREEN_H/2)-2, (SCREEN_W/2)-30,
	  (SCREEN_H/2)+2, (SCREEN_W/2)+30, " Copying ", YES);
  printf("\e[%d;%dH", (SCREEN_H/2)-1, (SCREEN_W/2)-29);
  if(strlen(str) > 58)
    for(n = 0; n < 58; n++) putchar(str[n]);
  else printf("%s", str);
  fflush(stdout);
  
  //check if it doesn't exist, create it
  if(stat(str, &statbuf) == -1) mkdir(str, 0700);
  else 
  {
    if(!CONFIRM_CUT_COPY) 
    {
      char *s = (char *) malloc(strlen(str)+39);
      if(!s) { msgBox("Insufficient memory", OK, ERROR); return; }
      strcpy(s, "Directory:\n");
      strcat(s, str);
      strcat(s, "\nalready exists. Overwrite?");
      strcat(s, "\0");
      n = msgBox(s, YES|NO|ALL, CONFIRM);
      fprintf(log_file, "copyThisDir(): Dir '%s' already exists.\n", str);
      free(s);
      if(n == NO) return;
      if(n == ABORT) { ABORT_COPY = 1; return; }
      if(n == ALL) CONFIRM_CUT_COPY = 1;
    }
  }
  int z = chdir(str);
  if(z == -1) { msgBox("Error changing directory", OK, ERROR); return; }
  
  n = scandir(tmp, &eps, one, alphasort);
  if(n >= 0) 
  {
    int cnt;
    for(cnt = 0; cnt < n; ++cnt) 
    {
      if(strcmp(eps[cnt]->d_name, ".") == 0 
	 || strcmp(eps[cnt]->d_name, "..") == 0)	//ignore "." & ".."
	continue;
      char *cc = (char *) malloc(strlen(tmp)+strlen(eps[cnt]->d_name)+2);
      if(!cc) { msgBox("Insufficient memory", OK, ERROR); return; }
      strcpy(cc, tmp);
      strcat(cc, "/");
      strcat(cc, eps[cnt]->d_name);
      strcat(cc, "\0");
      lstat(cc,&statbuf);
      if(S_ISDIR(statbuf.st_mode)) 
      {
	copyThisDir(cc, level+1);
      } 
      else 
      {
	copyThisFile(cc);
      } //end else
      free(cc);
    } //end for
  }	//end inner if 
  else 
  {
    fprintf(log_file, "Error opening dir: %s\n", tmp);
  } //end else
  
  /* Set the Directory mode */
  struct stat statbuf;
  mode_t mode = 00;
  z = lstat(tmp, &statbuf);
  if(z == -1) { msgBox(strerror(errno), OK, ERROR); return; }
  mode = get_mode(statbuf.st_mode);
  z = chdir("..");
  if(z == -1) { msgBox("Error changing directory", OK, ERROR); return; }
  if(chmod(str, mode) != 0) 
  {
	    msgBox(strerror(errno), OK, ERROR);
  }
}

/**********************************************
 * moves the dir sent as tmp[] recursively
 * making new subdirs in the CWD if necessary.
 * ********************************************/
void moveThisDir(char tmp[], int level) 
{
  char *str;

  int n;
  struct dirent **eps;
  struct stat st;

  //find the last '/' in the path, add one for the start of the dirname
  str = strrchr(tmp, '/'); str++; 
  //show progress to the user
  drawBox((SCREEN_H/2)-2, (SCREEN_W/2)-30,
	  (SCREEN_H/2)+2, (SCREEN_W/2)+30, " Moving ", YES);
  printf("\e[%d;%dH", (SCREEN_H/2)-1, (SCREEN_W/2)-29);
  if(strlen(str) > 58)
    for(n = 0; n < 58; n++) putchar(str[n]);
  else printf("%s", str);
  fflush(stdout);
  
  //check if it doesn't exist, create it
  if(stat(str, &st) == -1) mkdir(str, 0700);
  else 
  {
    if(!CONFIRM_CUT_COPY) 
    {
      char *s = (char *) malloc(strlen(str)+39);
      if(!s) { msgBox("Insufficient memory", OK, ERROR); return; }
      strcpy(s, "Directory:\n");
      strcat(s, str);
      strcat(s, "\nalready exists. Overwrite?");
      strcat(s, "\0");
      n = msgBox(s, YES|NO|ALL, CONFIRM);
      fprintf(log_file, "moveThisDir(): Dir '%s' already exists.\n", str);
      free(s);
      if(n == NO) return;
      if(n == ABORT) { ABORT_COPY = 1; return; }
      if(n == ALL) CONFIRM_CUT_COPY = 1;
    }
  }
  int z = chdir(str);
  if(z == -1) { msgBox("Error changing directory", OK, ERROR); return; }
  
  n = scandir(tmp, &eps, one, alphasort);
  if(n >= 0) 
  {
    int cnt;
    for(cnt = 0; cnt < n; ++cnt) 
    {
      if(strcmp(eps[cnt]->d_name, ".") == 0 
	 || strcmp(eps[cnt]->d_name, "..") == 0)	//ignore "." & ".."
	continue;
      char *cc = (char *) malloc(strlen(tmp)+strlen(eps[cnt]->d_name)+2);
      if(!cc) { msgBox("Insufficient memory", OK, ERROR); return; }
      strcpy(cc, tmp);
      strcat(cc, "/");
      strcat(cc, eps[cnt]->d_name);
      strcat(cc, "\0");
      lstat(cc,&statbuf);
      if(S_ISDIR(statbuf.st_mode)) 
      {
	moveThisDir(cc, level+1);
      } 
      else 
      {
	copyThisFile(cc);
	remove(cc);
      } //end else
      free(cc);
    } //end for
  }	//end inner if 
  else 
  {
    fprintf(log_file, "Error opening dir: %s\n", tmp);
  } //end else
  
  /* Set the Directory mode */
  struct stat statbuf;
  mode_t mode = 00;
  z = lstat(tmp, &statbuf);
  if(z == -1) { msgBox(strerror(errno), OK, ERROR); return; }
  mode = get_mode(statbuf.st_mode);
  if(chmod(str, mode) != 0) 
  {
	    msgBox(strerror(errno), OK, ERROR);
  }

  z = chdir("..");
  if(z == -1) { msgBox("Error changing directory", OK, ERROR); return; }
  rmdir(tmp);
}

/**********************************************
 * deletes the dir sent as tmp[] and all its
 * child dirs and contained files recursively.
 * ********************************************/
void deleteThisDir(char tmp[], int level) 
{
  char *str = strdup(tmp);

  //int nf = 0;
  int n;
  struct dirent **eps;
  //struct stat st;

  //show progress to the user
  drawBox((SCREEN_H/2)-2, (SCREEN_W/2)-30,
	  (SCREEN_H/2)+2, (SCREEN_W/2)+30, " Deleting ", YES);
  printf("\e[%d;%dH", (SCREEN_H/2)-1, (SCREEN_W/2)-29);
  if(strlen(str) > 58)
    for(n = 0; n < 58; n++) putchar(str[n]);
  else printf("%s", str);
  fflush(stdout);
  
  //chdir(tmp);
  n = scandir(str, &eps, one, alphasort);
  if(n >= 0) 
  {
    int cnt;
    for(cnt = 0; cnt < n; ++cnt) 
    {
      if(strcmp(eps[cnt]->d_name, ".") == 0 
	 || strcmp(eps[cnt]->d_name, "..") == 0)	//ignore "." & ".."
	continue;
      char *cc = (char *) malloc(strlen(str)+strlen(eps[cnt]->d_name)+2);
      if(!cc) { msgBox("Insufficient memory", OK, ERROR); return; }
      strcpy(cc, str);
      strcat(cc, "/");
      strcat(cc, eps[cnt]->d_name);
      strcat(cc, "\0");
      lstat(cc,&statbuf);
      if(S_ISDIR(statbuf.st_mode)) 
      {
	int z;
	z = chdir(str);
	if(z == -1) { msgBox("Error changing directory", OK, ERROR); return; }
	deleteThisDir(eps[cnt]->d_name, level+1);
	z = chdir("..");
	if(z == -1) { msgBox("Error changing directory", OK, ERROR); return; }
      } 
      else 
      {
	remove(cc);
      } //end else
      free(cc);
    } //end for
  } 
  else 
  {	//end inner if
    fprintf(log_file, "Error opening dir: %s\n", tmp);
  } //end else
  
  rmdir(str);
  free(str);
}

void deleteThisFile(char *fileName) 
{
  if(strcmp(fileName, "(Empty folder)") == 0)
    return;//this means folder is empty!!
}

/********************************************
 * this function deletes all the files/dirs
 * marked with a star.. if no items marked,
 * it deletes the highlighted file/dir.
 * ******************************************/
/********************************************
 * Removes marked items in the active window.
 * ******************************************/
void deleteMarked() 
{
    int i;
    i = msgBox("Delete all selected files/dirs?", YES|NO, CONFIRM);
    if(i == NO) return;
    for(i = 0; i < totalFiles; i++) 
    {
      if(fileStar[i] == '*')
      {
	remove(files[i]);
      }//end if
    }//end for
    for(i = 0; i < totalDirs; i++) 
    {
      if(dirStar[i] == '*') 
      {
	deleteThisDir(dirs[i], 0);
      }//end if
    }//end for
    //free(s4);
  numStarred = 0;
}//end deleteMarked()
