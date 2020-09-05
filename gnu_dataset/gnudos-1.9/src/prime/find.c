/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2013, 2014, 2015 (c)
 * 
 *    file: find.c
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
#include "find.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pwd.h>

extern char *tmp, *tmp2;
FILE *searchResultsFile;

static int one(const struct dirent *unused) 
{
  return 1;
}

void draw_findFile_buttons(int bx, int by, int selection)
{
  if(selection == 4)
    setScreenColors(FG_COLOR[COLOR_HBUTTONS], BG_COLOR[COLOR_HBUTTONS]);
  else setScreenColors(FG_COLOR[COLOR_BUTTONS], BG_COLOR[COLOR_BUTTONS]);
  fprintf(stdout, "\x1b[%d;%dH", bx, by);
  fprintf(stdout, "   OK   ");
  by += 12;
  if(selection == 5)
    setScreenColors(FG_COLOR[COLOR_HBUTTONS], BG_COLOR[COLOR_HBUTTONS]);
  else setScreenColors(FG_COLOR[COLOR_BUTTONS], BG_COLOR[COLOR_BUTTONS]);
  fprintf(stdout, "\x1b[%d;%dH", bx, by);
  fprintf(stdout, " CANCEL ");
  //adjust cursor to point at selected button
  if(selection == 4)
    fprintf(stdout, "\x1b[%d;%dH", bx, by-9);
  else if(selection == 5)
    fprintf(stdout, "\x1b[%d;%dH", bx, by+1);
  fflush(stdout);
}

/*************************************************
 * this function displays a dialog box for the
 * user to enter file name, where to search, and
 * OK/CANCEL buttons. it holds input from the user
 * to navigate the dialog box fields, and then 
 * calls scanThisDir() function to search.
 * ***********************************************/
void findFile() 
{
  int x = 5;
  int w = 46;
  int h = 12+x;
  int y = (SCREEN_W-w)/2;
  w += y;
  int i, sel = 0;	//the selected control in the window
  //sel: 	0=first input field	1=first option
  //		2=second option		3=third option
  //		4=second input field	5=OK	6=CANCEL
  int osel = 0;		//the option marked with [X] in front of it
  //osel: 0=first option, 1=second, 2=third
  
  for(i = 0; i < MAX_INPUT1_LEN; i++) findFileName[i] = '\0';
  for(i = 0; i < MAX_INPUT2_LEN; i++) findInDir[i] = '\0';
  input1_len = 0; input2_len = 0;
  highlight1 = 0; highlight2 = 0;
  
  if(!(searchResultsFile = fopen("/tmp/prime_find", "w+"))) 
  {
    msgBox("Error opening temporary file. Aborting.", OK, ERROR);
    setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
    fprintf(log_file, "Failed to open '/tmp/prime_find' file..\n");
    return;
  }

  //Set the user interface//
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  drawBox(x, y, h, w, " Find File ", YES);
  fprintf(stdout, "\e[%d;%dH", x+2, y+2);
  fprintf(stdout, "Enter file name to search for:");
  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
  fprintf(stdout, "\e[%d;%dH", x+3, y+2);
  printf("%*s", MAX_INPUT1_LEN, " ");
  fprintf(stdout, "\e[%d;%dH", x+9, y+22);
  printf("%*s", MAX_INPUT2_LEN, " ");
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  fprintf(stdout, "\e[%d;%dH", x+6, y+2);
  fprintf(stdout, "Search in:");
  fprintf(stdout, "\e[%d;%dH", x+7, y+2);
  fprintf(stdout, "[X] Only current working directory");
  fprintf(stdout, "\e[%d;%dH", x+8, y+2);
  fprintf(stdout, "[ ] All the filesystem (slower)");
  fprintf(stdout, "\e[%d;%dH", x+9, y+2);
  fprintf(stdout, "[ ] Under this path:");
  //Draw the buttons//
  showCursor();
  int bx, by;
  bx = h-1;
  by = y + ((w-y-16)/2) - 2;
  draw_findFile_buttons(bx, by, sel);
  fprintf(stdout, "\e[%d;%dH", x+3, y+2);//adjust cursor to be at input field

  //wait for user response//
 int ch;
 while(1)
 {	//infinite program loop//
    ch = getKey();
    switch(ch) 
    {
      case(ESC_KEY): 
	setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	return;
      case(SPACE_KEY):
	if(sel == 0 || sel == 4) 
	{	//if pressed space in input field, insert the space
	  goto enterNewChar;
	  break;
	}//if pressed space on a button or option, fall through to ENTER below
      case(ENTER_KEY):
	if(sel == 1) 
	{	//select this option if not already selected
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  if(osel != 0) fprintf(stdout, "\e[%d;%dH ", x+7+osel, y+3);
	  fprintf(stdout, "\e[%d;%dHX", x+7, y+3);
	  fprintf(stdout, "\e[%d;%dH", x+7, y+3);
	  osel = 0;
	}
	else if(sel == 2) 
	{	//select this option if not already selected
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  if(osel != 1) fprintf(stdout, "\e[%d;%dH ", x+7+osel, y+3);
	  fprintf(stdout, "\e[%d;%dHX", x+8, y+3);
	  fprintf(stdout, "\e[%d;%dH", x+8, y+3);
	  osel = 1;
	}
	else if(sel == 3) 
	{	//select this option if not already selected
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  if(osel != 2) fprintf(stdout, "\e[%d;%dH ", x+7+osel, y+3);
	  fprintf(stdout, "\e[%d;%dHX", x+9, y+3);
	  fprintf(stdout, "\e[%d;%dH", x+9, y+3);
	  osel = 2;
	}
	else if(sel == 0 || sel == 5) 
	{	//pressed ENTER on OK button or on the first input field
	  if(input1_len <= 0) return;	//if no input entered, just return
	  findFileName[input1_len] = '\0';
	  /////////////////////////////////////////////
	  //chop search file name into three strings
	  /////////////////////////////////////////////
	  if(strchr(findFileName, '*')) 
	  {
	    i = strchr(findFileName, '*')-findFileName;
	    if(i == 0) 
	    {
	      strcpy(startStr, "*");
	      strcpy(middleStr, "\0");
	      strcpy(endStr, findFileName+1);
	    } 
	    else if(i == strlen(findFileName)-1) 
	    {
	      strncpy(startStr, findFileName, strlen(findFileName)-1);
	      strcpy(middleStr, "\0");
	      strcpy(endStr, "*");
	    } 
	    else 
	    {
	      strncpy(startStr, findFileName, i);
	      strcpy(middleStr, "*");
	      strcpy(endStr, findFileName+strlen(findFileName)-i);
	    }
	  /////////////////////////////////////////////
	  } 
	  else 
	  {
	    strcpy(startStr, findFileName);
	    strcpy(middleStr, "\0");
	    strcpy(endStr, "\0");
	  }
	  /////////////////////////////////////////////
	  /////////////////////////////////////////////
	  if(osel == 0) 
	  {	//search in current working directory
	    char *t = (char *) malloc(strlen(cwd));//[strlen(cwd)];
	    if(!t) { msgBox("Insufficient memory", OK, ERROR); return; }
	    
	    fprintf(log_file, "Searching for '%s' under '%s'..\n", 
		    findFileName, cwd);
	    strcpy(t, cwd);
	    scanDirForFile(t, 0);
	    free(t);
	    remove("/tmp/prime_find");
	    return;
	  } 
	  else if (osel == 1) 
	  {	//search starting from '/'
	    fprintf(log_file, "Searching for '%s' under Root..\n", findFileName);
	    scanDirForFile("/", 0);
	    return;
	  } 
	  else if (osel == 2) 
	  {	//search in selected path
	    if(input2_len == 0)
	      msgBox("Invalid path.", OK, ERROR);
	    else 
	    {
	      strcpy(tmp, findInDir);
	      if(strchr(tmp, '~')) 
	      {
		strcpy(tmp2, findInDir+((strchr(tmp, '~')+1)-tmp));
		struct passwd *pass;//will be used to find the home dir
		if((pass = getpwuid(geteuid())))
		  ;
		strcpy(tmp, pass->pw_dir);
		strcat(tmp, "/");
		strcat(tmp, tmp2);
		strcat(tmp, "\0");
	      }
	      fprintf(log_file, "Searching for '%s' under '%s'..\n", 
		      findFileName, tmp);
	      scanDirForFile(tmp, 0);
	    }
	  }
	  remove("/tmp/prime_find");
	  return;				//otherwise return the input
	}
	if(sel == 6) return;		//return NULL also if selected CANCEL
	break;
      case(RIGHT_KEY):
	if(sel == 0) 
	{	//first input field
	  if(highlight1 >= input1_len) break;//already at last char
	  if(highlight1 == MAX_INPUT1_LEN-1) break;//already at last char
	  highlight1++;
	  //adjust cursor to point at input field
	  fprintf(stdout, "\x1b[%d;%dH", x+3, y+highlight1+2);
	} 
	else if(sel == 4) 
	{	//second input field
	  if(highlight2 >= input2_len) break;//already at last char
	  if(highlight2 == MAX_INPUT2_LEN-1) break;//already at last char
	  highlight2++;
	  //adjust cursor to point at input field
	  fprintf(stdout, "\x1b[%d;%dH", x+9, y+highlight2+22);
	} 
	break;
      case(LEFT_KEY):
	if(sel == 0)
	{	//first input field
	  if(highlight1 <= 0) break;//already at first char
	  highlight1--;
	  //adjust cursor to point at input field
	  fprintf(stdout, "\x1b[%d;%dH", x+3, y+highlight1+2);
	} 
	else if(sel == 4)
	{	//second input field
	  if(highlight2 <= 0) break;//already at first char
	  highlight2--;
	  //adjust cursor to point at input field
	  fprintf(stdout, "\x1b[%d;%dH", x+9, y+highlight2+22);
	} 
	break;
      case(TAB_KEY):
	  bx = h-1;
	  by = y + ((w-y-16)/2) - 2;
	  if(sel == 0) 
	  {
	    fprintf(stdout, "\e[%d;%dH", x+7, y+3);
	    sel = 1;
	  } 
	  else if(sel == 1) 
	  {
	    fprintf(stdout, "\e[%d;%dH", x+8, y+3);
	    sel = 2;
	  } 
	  else if(sel == 2) 
	  {
	    fprintf(stdout, "\e[%d;%dH", x+9, y+3);
	    sel = 3;
	  } 
	  else if(sel == 3) 
	  {
	    fprintf(stdout, "\e[%d;%dH", x+9, y+highlight2+22);
	    sel = 4;
	  } 
	  else if(sel == 4) 
	  {
	    draw_findFile_buttons(bx, by, sel);
	    sel = 5;
	  } 
	  else if(sel == 5) 
	  {
	    draw_findFile_buttons(bx, by, sel);
	    sel = 6;
	  } 
	  else 
	  {
	    draw_findFile_buttons(bx, by, sel);
	    sel = 0;
	    //adjust cursor to point at input field
	    fprintf(stdout, "\x1b[%d;%dH", x+3, y+highlight1+2);
	  }
	setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	break;
      case(BACKSPACE_KEY):
	if(sel == 0)
	{
	  if(input1_len == 0) break;
	  if(highlight1 == 0) break;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], 
			  BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  for(i = --highlight1; i < input1_len; i++) 
	    findFileName[i] = findFileName[i+1];
	  findFileName[i] = '\0';
	  input1_len--;
	  //adjust cursor to point at input field
	  fprintf(stdout, "\x1b[%d;%dH", x+3, y+highlight1+2);
	  for(i = highlight1; i < input1_len; i++) putchar(findFileName[i]);
	  putchar(' ');
	  //adjust cursor to point at input field
	  fprintf(stdout, "\x1b[%d;%dH", x+3, y+highlight1+2);
	} 
	else if(sel == 4)
	{
	  if(input2_len == 0) break;
	  if(highlight2 == 0) break;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], 
			  BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  for(i = --highlight2; i < input2_len; i++) 
	    findInDir[i] = findInDir[i+1];
	  findInDir[i] = '\0';
	  input2_len--;
	  //adjust cursor to point at input field
	  fprintf(stdout, "\x1b[%d;%dH", x+9, y+highlight2+22);
	  for(i = highlight2; i < input2_len; i++) putchar(findInDir[i]);
	  putchar(' ');
	  //adjust cursor to point at input field
	  fprintf(stdout, "\x1b[%d;%dH", x+9, y+highlight2+22);
	}
      case(DEL_KEY):
	if(sel == 0)
	{
	  if(input1_len == 0) break;
	  if(highlight1 == 0) break;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], 
			  BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  for(i = highlight1; i < input1_len-1; i++) 
	    findFileName[i] = findFileName[i+1];
	  findFileName[i] = '\0';
	  input1_len--;
	  //adjust cursor to point at input field
	  fprintf(stdout, "\x1b[%d;%dH", x+3, y+highlight1+2);
	  for(i = highlight1; i < input1_len; i++) putchar(findFileName[i]);
	  putchar(' ');
	  //adjust cursor to point at input field
	  fprintf(stdout, "\x1b[%d;%dH", x+3, y+highlight1+2);
	} 
	else if(sel == 4)
	{
	  if(input2_len == 0) break;
	  if(highlight2 == 0) break;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], 
			  BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  for(i = highlight2; i < input2_len-1; i++) 
	    findInDir[i] = findInDir[i+1];
	  findInDir[i] = '\0';
	  input2_len--;
	  //adjust cursor to point at input field
	  fprintf(stdout, "\x1b[%d;%dH", x+9, y+highlight2+22);
	  for(i = highlight2; i < input2_len; i++) putchar(findInDir[i]);
	  putchar(' ');
	  //adjust cursor to point at input field
	  fprintf(stdout, "\x1b[%d;%dH", x+9, y+highlight2+22);
	}
      default:
	  if((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
	     || (ch >= 32 && ch<= 64) || (ch >=123 && ch <= 126)) 
	  {	//if it is alphanumeric
enterNewChar:
	    if(sel == 0) 
	    {
	      if(strlen(findFileName) >= MAX_INPUT1_LEN) break;
	      setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], 
			  BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	      //inserting in the middle of a string means we need to shift all
	      //chars one position to the right before inserting the new char
	      //at the highlighted position.
	      if(findFileName[highlight1] != '\0') 
	      {
		for(i = input1_len; i > highlight1; i--) 
		  findFileName[i] = findFileName[i-1];
	      }
	      findFileName[highlight1] = ch;
	      input1_len++;
	      putchar(findFileName[highlight1++]);
	      if(input1_len > highlight1) 
	      {	//there are some chars to the right side
		//adjust cursor to point at input field
		for(i = highlight1; i < input1_len; i++) 
		  putchar(findFileName[i]);
	      }
	      if(highlight1 >= MAX_INPUT1_LEN) highlight1 = MAX_INPUT1_LEN-1;
	      //adjust cursor to point at input field
	      fprintf(stdout, "\x1b[%d;%dH", x+3, y+highlight1+2);
	    } 
	    else if(sel == 4) 
	    {	//the second input field
	      if(osel != 2)
	      {
		setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
		fprintf(stdout, "\e[%d;%dH ", x+7+osel, y+3);
		fprintf(stdout, "\e[%d;%dHX", x+9, y+3);
		fprintf(stdout, "\e[%d;%dH", x+9, y+3);
		osel = 2;
		fprintf(stdout, "\x1b[%d;%dH", x+9, y+highlight2+22);
	      }
	      if(strlen(findInDir) >= MAX_INPUT2_LEN) break;
	      setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], 
			  BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	    //inserting in the middle of a string means we need to shift all
	    //chars one position to the right before inserting the new char
	    //at the highlighted position.
	      if(findInDir[highlight2] != '\0') 
	      {
		for(i = input2_len; i > highlight2; i--) 
		  findInDir[i] = findInDir[i-1];
	      }
	      findInDir[highlight2] = ch;
	      input2_len++;
	      putchar(findInDir[highlight2++]);
	      if(input2_len > highlight2) 
	      {	//there are some chars to the right side
		//adjust cursor to point at input field
		for(i = highlight2; i < input2_len; i++) putchar(findInDir[i]);
	      }
	      if(highlight2 >= MAX_INPUT2_LEN) highlight2 = MAX_INPUT2_LEN-1;
	      //adjust cursor to point at input field
	      fprintf(stdout, "\x1b[%d;%dH", x+9, y+highlight2+22);
	    }
	  }
    }
 }
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  return;
}

/********************************************
 * this function searches for the requested
 * file in the directory passed as tmp[] and
 * it's subdirectories. NOT to be called
 * directly, instead, call from findFile().
 * PARAMETERS:
 * 	tmp[]: name of directory to search in
 * 	level: current depth, used in recursive
 * 		function calling
 * ******************************************/
void scanDirForFile(char tmp[], int level) 
{
  static int nf;
  int n, i, z;
  struct dirent **eps;

  if(level == 0) 
  {
    nf = 0;
    //display progress message to user
    drawBox((SCREEN_H/2)-2, 5, (SCREEN_H/2)+2, SCREEN_W-5, NULL, YES);
    fprintf(stdout, "\e[%d;%dH", (SCREEN_H/2)-1, 6);
    printf("Scanning:");
  }
  
  z = chdir(tmp);
  if(z == -1)
  {
    /* msgBox("Error changing directory", OK, ERROR); */ return;
  }
  n = scandir(tmp, &eps, one, alphasort);
  if(n >= 0) 
  {
    int cnt;
    //fprintf(log_file, "directory of: %s, children:%d\n", tmp, n);
    for(cnt = 0; cnt < n; ++cnt) 
    {
      z = lstat(eps[cnt]->d_name,&statbuf);
      if(z == -1)
      {
	/* msgBox(strerror(errno), OK, ERROR); */ return;
      }
      if(S_ISDIR(statbuf.st_mode)) 
      {
	if(strcmp(eps[cnt]->d_name, ".") == 0 
	   || strcmp(eps[cnt]->d_name, "..") == 0)	//ignore "." & ".."
	  continue;

	//display progress message to user
	fprintf(stdout, "\e[%d;%dH", SCREEN_H/2, 6);
	if(strlen(eps[cnt]->d_name) > SCREEN_W-11)
	  printf("..%s", 
		 eps[cnt]->d_name+(strlen(eps[cnt]->d_name)-SCREEN_W-13));
	else 
	  printf("%s%*s", 
		 eps[cnt]->d_name, (int)(SCREEN_W-12-strlen(eps[cnt]->d_name)), " ");

	
	char *tmp2 = (char *)malloc(strlen(tmp)+strlen(eps[cnt]->d_name)+2);
	if(!tmp2) { msgBox("Insufficient memory", OK, ERROR); return; }
	strcpy(tmp2, tmp);
	if(tmp2[strlen(tmp2)-1] != '/') strcat(tmp2, "/");
	strcat(tmp2, eps[cnt]->d_name);
	strcat(tmp2, "\0");
	
	scanDirForFile(tmp2, level+1);
	free(tmp2);
	
	continue;
      } 
      else 
      {
	if(strrchr(eps[cnt]->d_name, '/'))
	  i = compareFileName(strrchr(eps[cnt]->d_name, '/')+1);
	else i = compareFileName(eps[cnt]->d_name);
	if(i) 
	{
	  //make sure the match is in the filename, not the dir name
	  //do this by comparing the index of the match with the
	  //last '/' in the name.
	  if(i < (int)(strrchr(eps[cnt]->d_name, '/')-eps[cnt]->d_name)) continue;
	  //if the file name is too long, truncate it
	  if((strlen(tmp)+strlen(eps[cnt]->d_name)) > SCREEN_W-19) 
	  {
	    int j = SCREEN_W-strlen(eps[cnt]->d_name)-22;
	    for(i = 0; i < j; i++)
	      fputc(tmp[i], searchResultsFile);
	    fputs("../", searchResultsFile);
	    fputs(eps[cnt]->d_name, searchResultsFile);
	    fputc('\n', searchResultsFile);
	  } 
	  else 
	  {
	    fprintf(searchResultsFile, "%s/%s\n", tmp, eps[cnt]->d_name);
	  }//end inner if
	  nf++;
	}//end outer if
      }//end if
    }//end for
  }
  else 
  {
    //fprintf(log_file, "Error opening dir: %s\n", tmp);
  }//end outer if

  printf("%s\n", tmp);
  if(level == 0) 
  {
    if(nf == 0) 
    {
      msgBox("Search failed. No matches were found.", OK, INFO);
      //fprintf(log_file, "Search failed. No matches were found.\n");
    } 
    else 
    {
      //fprintf(log_file, "Search finished. %d files were found\n", nf);
      fprintf(searchResultsFile, "%06d", nf);
      fflush(searchResultsFile);
      showSearchResults();
      fclose(searchResultsFile);
    }
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  } 
  else
  {
    z = chdir("..");
    if(z == -1)
    {
      /* msgBox("Error changing directory", OK, ERROR); */
      return;
    }
  }
}

int compareFileName(char *name) 
{
    int i = 0;
    
    if(strlen(startStr))
      if(strcmp(startStr, "*") != 0)
      {
	if(((strstr(name, startStr))-name) == 0) 
	{ 
	  i = 1;
	} 
	else return 0;
      }
    if(strlen(endStr))
      if(strcmp(endStr, "*") != 0) 
      {
	if(((strstr(name, endStr)-name)+strlen(endStr) == strlen(name))) 
	{ 
	  i = 1;
	} 
	else return 0;
      }
    if(strlen(middleStr))
      if(strcmp(middleStr, "*") != 0)
      {
	if(((strstr(name, middleStr))-name) > 0) 
	{ 
	  i = 1;
	} 
	else i = 0;
      }
    return i;
}

/*********************************************
 * this function shows a window with the
 * results of file search as its contents.
 * NOT to be called directly. Instead, called
 * from scanDirForFile() after it finishes.
 * *******************************************/
void showSearchResults() 
{
  int lineStart[500];
  int i, j, k, l;
  int buf_size = 4096;
  char buf[buf_size];
  int buf_len = 0;
  int moreLines = 0;
  int firstVisLine = 0;
  char totalResultsC[6];
  int totalResults;
  int page_number = 0;
  int firstPageEntry = 0;
  int page_start[100];
  page_start[0] = 0;
  int page_firstVisFile[100];
  page_firstVisFile[0] = 0;
  
  fseek(searchResultsFile, -6, SEEK_END);//read int at end of file
  i = fread(totalResultsC, sizeof(char), 6, searchResultsFile);
  totalResults = atoi(totalResultsC);
  
  drawBox(5, 5, SCREEN_H-5, SCREEN_W-5, " Search results: ", YES);
  fseek(searchResultsFile, 0, SEEK_SET);//reset to beginning of file
  buf_len = fread(buf, sizeof(char), sizeof(buf), searchResultsFile);
  if(!buf_len) return;
  buf_len -= 6;
  
  i = 0; //counter for the output chars
  j = 6; //counter to keep track of the total number of lines
  k = 6; //counter of char number in the line
  l = 1; //counter to tell the number of the file over the total no.
  lineStart[0] = 0;
  int len = 0;
  char t[20];
  fprintf(stdout, "\e[%d;%dH", j, k);
  sprintf(t, "(%d/%d) ", l, totalResults);
  fprintf(stdout, "%s", t);
  len = strlen(t);
  while(i < buf_len) 
  {
    if(j > SCREEN_H-6) { moreLines = 1; break; }
    if(len > SCREEN_W-12)
    {
      k = 6; j++; lineStart[l] = i; 
      fprintf(stdout, "\e[%d;%dH   ", j, k);
      len = 3;
      continue; 
    }
    if(buf[i] == '\n') 
    {
      k = 6; j++; lineStart[l] = ++i; l++;
      if(l > totalResults) break;
      if(j > SCREEN_H-6) { moreLines = 1; break; }
      sprintf(t, "(%d/%d) ", l, totalResults);
      fprintf(stdout, "\e[%d;%dH", j, k);
      fprintf(stdout, "%s", t);
      len = strlen(t);
      continue;
    }
    putchar(buf[i++]);
    k++;
    len++;
  }//end while
  char c;
  while((c = getKey())) 
  {
    switch(c) 
    {
      case(SPACE_KEY):
      case(ENTER_KEY):
      case(ESC_KEY):
	return;
	break;
      case(UP_KEY):
	if(firstVisLine > 0) { firstVisLine--; goto Refresh; }
	//check to see if there are more results up there in the file
	if(page_number > 0)
	{
	  //reset to beginning of next page
	  fseek(searchResultsFile, 
		page_start[page_number-1], 
		SEEK_SET);
	  buf_len = fread(buf, sizeof(char), sizeof(buf), searchResultsFile);
	  if(!buf_len) return;
	  buf_len -= 6;
	  firstPageEntry = page_firstVisFile[page_number];
	  firstVisLine = page_firstVisFile[page_number]-1;
	  page_number--;
	  i = 0; j = 0;
	  lineStart[0] = 0;
	  while(i < buf_len)
	  {
	    if(buf[i] == '\n')
	    { lineStart[++j] = i; }
	    i++;
	  }
	  goto Refresh;
	}
	break;
      case(DOWN_KEY):
	if(moreLines) { firstVisLine++; goto Refresh; }
	//check to see if there are more results in the file
	if(firstVisLine+(SCREEN_H-12) < totalResults-1)
	{
	  //reset to beginning of next page
	  fseek(searchResultsFile, 
		page_start[page_number]+lineStart[firstVisLine+1], 
		SEEK_SET);
	  buf_len = fread(buf, sizeof(char), sizeof(buf), searchResultsFile);
	  if(!buf_len) return;
	  buf_len -= 6;
	  page_number++;
	  page_start[page_number] = 
	      page_start[page_number-1]+lineStart[firstVisLine+1];
	  firstPageEntry = firstVisLine+1;
	  page_firstVisFile[page_number] = page_firstVisFile[page_number-1]+firstPageEntry-1;
	  firstVisLine = 0;
	  lineStart[0] = 0;
	  goto Refresh;
	}
	break;
Refresh:
    i = lineStart[firstVisLine]; j = 6; k = 6; moreLines=0; l=firstVisLine+1;
    fprintf(stdout, "\e[%d;%dH", j, k);
    sprintf(t, "(%d/%d) ", l+page_firstVisFile[page_number], totalResults);
    fprintf(stdout, "%s", t);
    len = strlen(t);
    while(i < buf_len) 
    {
      if(j > SCREEN_H-6) { moreLines = 1; break; }
      if(len > SCREEN_W-12)
      { 
	k = 6; j++; lineStart[l] = i; 
	fprintf(stdout, "\e[%d;%dH   ", j, k);
	len = 3;
	continue; 
      }
      if(buf[i] == '\n') 
      { 
	printf("%*s", SCREEN_W-len-12, " ");//finish line with spaces
	k = 6; j++; lineStart[l] = ++i; l++;
	if(l+page_firstVisFile[page_number] > totalResults) break;
	if(j > SCREEN_H-6) { moreLines = 1; break; }
	fprintf(stdout, "\e[%d;%dH", j, k);
	sprintf(t, "(%d/%d) ", l+page_firstVisFile[page_number], totalResults);
	fprintf(stdout, "%s", t);
	len = strlen(t);
	continue;
      }
      putchar(buf[i++]);
      k++;
      len++;
     }//end while
    }//end switch

  }//end while
}//end showSearchResults()
