/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2013, 2014, 2015 (c)
 * 
 *    file: cutcopy.c
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
#include "cutcopy.h"
#include <string.h>
#include <stdlib.h>

/************************************************
 * this function checks if a directory is set
 * to be cut or copied. Pass it the number of
 * the directory 'i' in the global dirs[] array.
 * RETURNS:
 * 	1 if the dir is set to be cut
 * 	2 if the dir is set to be copied
 * 	0 if neither
 * **********************************************/
int checkCutOrCopyDir(int i)
{
  if(numCutDirs == 0 && numCopyDirs == 0) return 0;
  int j;
  char *str = (char *) malloc(strlen(dirs[i])+strlen(cwd)+2);
  if(!str) { msgBox("Insufficient memory", OK, ERROR); return 0; }
  strcpy(str, cwd);
  strcat(str, "/");
  strcat(str, dirs[i]);
  strcat(str, "\0");
  for(j = 0; j < numCutDirs; j++) 
  {
    if(strcmp(cutDirs[j], str) == 0) 
    { 
      free(str); return 1; 
    }
  } //end for
  for(j = 0; j < numCopyDirs; j++) 
  {
    if(strcmp(copyDirs[j], str) == 0) 
    { 
      free(str); return 2; 
    }
  } //end for
  //not found.. return FALSE//
  free(str); 
  return 0;
}

/************************************************
 * this function checks if a file is set
 * to be cut or copied. Pass it the number of
 * the file 'i' in the global files[] array.
 * RETURNS:
 * 	1 if the file is set to be cut
 * 	2 if the file is set to be copied
 * 	0 if neither
 * **********************************************/
int checkCutOrCopyFile(int i) 
{
  if(numCutFiles == 0 && numCopyFiles == 0) return 0;
  int j;
  char *str = (char *) malloc(strlen(files[i])+strlen(cwd)+2);
  if(!str) { msgBox("Insufficient memory", OK, ERROR); return 0; }
  strcpy(str, cwd);
  strcat(str, "/");
  strcat(str, files[i]);
  strcat(str, "\0");
  for(j = 0; j < numCutFiles; j++) 
  {
    if(strcmp(cutFiles[j], str) == 0) 
    { 
      free(str); return 1; 
    }
  } //end for
  for(j = 0; j < numCopyFiles; j++) 
  {
    if(strcmp(copyFiles[j], str) == 0) 
    { 
      free(str); return 2; 
    }
  } //end for
  //not found.. return FALSE//
  free(str); 
  return 0;
}

/*****************************************************
 * this function removes a file name from the 
 * array of files set to be cut 'cutFiles[]'. Pass 
 * it the file number 'i' in the global files[] array.
 * ***************************************************/
void removeCutFile(int i) 
{
  if(numCutFiles == 0) return;
  int j, k;
  char *str = (char *) malloc(strlen(files[i])+strlen(cwd)+2);
  if(!str) { msgBox("Insufficient memory", OK, ERROR); return; }
  strcpy(str, cwd);
  strcat(str, "/");
  strcat(str, files[i]);
  strcat(str, "\0");
  for(j = 0; j < numCutFiles; j++) 
  {
    if(strcmp(str, cutFiles[j]) == 0) 
    {	//found a match
      for(k = j; k < numCutFiles-1; k++) 
      {
	cutFiles[k] = cutFiles[k+1];
      } //end inner for
      numCutFiles--;
      free(str);
      return;
    }	//end if
  }	//end outer for
  free(str);
}	//end removeCutFile()

/*****************************************************
 * this function removes a file name from the 
 * array of files set to be copied 'copyFiles[]'. Pass 
 * it the file number 'i' in the global files[] array.
 * ***************************************************/
void removeCopyFile(int i) 
{
  if(numCopyFiles == 0) return;
  int j, k;
  char *str = (char *) malloc(strlen(files[i])+strlen(cwd)+2);
  if(!str) { msgBox("Insufficient memory", OK, ERROR); return; }
  strcpy(str, cwd);
  strcat(str, "/");
  strcat(str, files[i]);
  strcat(str, "\0");
  for(j = 0; j < numCopyFiles; j++) 
  {
    if(strcmp(str, copyFiles[j]) == 0) 
    {	//found a match
      for(k = j; k < numCopyFiles-1; k++) 
      {
	strcpy(copyFiles[k], copyFiles[k+1]);
      } //end inner for
      numCopyFiles--;
      free(str);
      return;
    }	//end if
  }	//end outer for
  free(str);
}	//end removeCopyFile()

/*****************************************************
 * this function removes a dir name from the 
 * array of dirs set to be cut 'cutDirs[]'. Pass 
 * it the dir number 'i' in the global dirs[] array.
 * ***************************************************/
void removeCutDir(int i) 
{
  if(numCutDirs == 0) return;
  int j, k;
  char *str = (char *) malloc(strlen(dirs[i])+strlen(cwd)+2);
  if(!str) { msgBox("Insufficient memory", OK, ERROR); return; }
  strcpy(str, cwd);
  strcat(str, "/");
  strcat(str, dirs[i]);
  strcat(str, "\0");
  for(j = 0; j < numCutDirs; j++) 
  {
    if(strcmp(str, cutDirs[j]) == 0) 
    {	//found a match
      for(k = j; k < numCutDirs-1; k++) 
      {
	cutDirs[k] = cutDirs[k+1];
      } //end inner for
      numCutDirs--;
      free(str);
      return;
    }	//end if
  }	//end outer for
  free(str);
}	//end removeCutDir()

/*****************************************************
 * this function removes a dir name from the 
 * array of dirs set to be copied 'copyDirs[]'. Pass 
 * it the dir number 'i' in the global dirs[] array.
 * ***************************************************/
void removeCopyDir(int i) 
{
  if(numCopyDirs == 0) return;
  int j, k;
  char *str = (char *) malloc(strlen(dirs[i])+strlen(cwd)+2);
  if(!str) { msgBox("Insufficient memory", OK, ERROR); return; }
  strcpy(str, cwd);
  strcat(str, "/");
  strcat(str, dirs[i]);
  strcat(str, "\0");
  for(j = 0; j < numCopyDirs; j++) 
  {
    if(strcmp(str, copyDirs[j]) == 0) 
    {	//found a match
      for(k = j; k < numCopyDirs-1; k++) 
      {
	copyDirs[k] = copyDirs[k+1];
      } //end inner for
      numCopyDirs--;
      free(str);
      return;
    }	//end if
  }	//end outer for
  free(str);
}	//end removeCopyDir()

/*****************************************************
 * this function adds a file to the array of files set 
 * to be cut 'cutFiles[]'. Pass it the file number 'i' 
 * in the global files[] array.
 * ***************************************************/
void saveCutFile(int i) 
{
  if(numCutFiles > MAX_CUT_COPY) 
  {
    msgBox("Unable to perform operation.\nClipboard is full.", OK, ERROR);
    fprintf(log_file, "Unable to cut file '%s'.. Clipboard is full\n", files[i]);
    return;
  }
  strcpy(cutFiles[numCutFiles], cwd);
  strcat(cutFiles[numCutFiles], "/");
  strcat(cutFiles[numCutFiles], files[i]);
  strcat(cutFiles[numCutFiles++], "\0");
}

/*****************************************************
 * this function adds a file to the array of files set 
 * to be copied 'copyFiles[]'. Pass it the file number 
 * 'i' in the global files[] array.
 * ***************************************************/
void saveCopyFile(int i) 
{
  if(numCopyFiles > MAX_CUT_COPY) 
  {
    msgBox("Unable to perform operation.\nClipboard is full.", OK, ERROR);
    fprintf(log_file, "Unable to copy file '%s'.. Clipboard is full\n", files[i]);
    return;
  }
  //free(copyFiles[numCopyFiles]);
  //copyFiles[numCopyFiles] = (char *) malloc(strlen(cwd)+strlen(files[i])+1);
  strcpy(copyFiles[numCopyFiles], cwd);
  strcat(copyFiles[numCopyFiles], "/");
  strcat(copyFiles[numCopyFiles], files[i]);
  strcat(copyFiles[numCopyFiles++], "\0");
}

/*****************************************************
 * this function adds a dir to the array of dirs set 
 * to be cut 'cutDirs[]'. Pass it the dir number 'i' 
 * in the global dirs[] array.
 * ***************************************************/
void saveCutDir(int i) 
{
  if(numCutDirs > MAX_CUT_COPY) 
  {
    msgBox("Unable to perform operation.\nClipboard is full.", OK, ERROR);
    fprintf(log_file, "Unable to cut dir '%s'.. Clipboard is full\n", files[i]);
    return;
  }
  strcpy(cutDirs[numCutDirs], cwd);
  strcat(cutDirs[numCutDirs], "/");
  strcat(cutDirs[numCutDirs], dirs[i]);
  strcat(cutDirs[numCutDirs++], "\0");
}

/*****************************************************
 * this function adds a dir to the array of dirs set 
 * to be copied 'copyDirs[]'. Pass it the dir number 'i' 
 * in the global dirs[] array.
 * ***************************************************/
void saveCopyDir(int i) 
{
  if(numCopyDirs > MAX_CUT_COPY) 
  {
    msgBox("Unable to perform operation.\nClipboard is full.", OK, ERROR);
    fprintf(log_file, "Unable to copy dir '%s'.. Clipboard is full\n", files[i]);
    return;
  }
  strcpy(copyDirs[numCopyDirs], cwd);
  strcat(copyDirs[numCopyDirs], "/");
  strcat(copyDirs[numCopyDirs], dirs[i]);
  strcat(copyDirs[numCopyDirs++], "\0");
}
