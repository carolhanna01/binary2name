/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2013, 2014, 2015 (c)
 * 
 *    file: defs.h
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

/**********************************************
  This file contains definitions for constants 
  and functions used by the Prime program.
 **********************************************/
#include <stdio.h>

#ifndef __Prime_H
#define __Prime_H

#include "../corelib/dialogs.h"
#include <errno.h>

#define MAXDIRS		255
#define	MAXFILES	255
int MAX_DIR_NAME_LEN;
int MAX_FILE_NAME_LEN;
int MAX_MSG_BOX_W;
int MAX_MSG_BOX_H;
#define MAX_INPUT_MSG_LEN 100

//Function prototypes//
void drawMenuBar(int x, int y, int w);
void init();
void scanDir(char *dir);
void refreshAll();
void refreshWindows();
void refreshDirView();
void refreshFileView();
void refreshBottomView();
void showFileMenu(int visible);
void showEditMenu(int visible);
void showOptionsMenu(int visible);
void showHelpMenu(int visible);
void showPropertiesDialog();
char *file_open_location();
void toggleSelected();
void showReadMe();	//called from help menu to show README file
void showKeybindings();	//called from help menu to show KEYBINDINGS file
void exit_gracefully();

//Menu items//
typedef struct menu_item_s 
{
  char *mtext;
  char *shortCutKeys;
} menu_item_struct;

//Main menu items//
typedef struct menu_s 
{
  char *text;
  int children;
  //struct menu_item_s childMenu[];
  char *child[];
} mainMenu;

//struct menu_s menu[3];
char *menu[4];
#define totalMainMenus	4	//total items in main menu bar
#define fTotal		5	//total items in file menu
#define eTotal		7	//total items in edit menu
#define oTotal		3	//total items in options menu
#define hTotal		4	//total items in help menu
char *fileMenu[6];
char *editMenu[8];
char *optionsMenu[3];
char *helpMenu[5];

char *dirs[MAXDIRS];
char *files[MAXFILES];
int numVisDirs, firstVisDir, selectedDir, totalDirs;
int numVisFiles, firstVisFile, selectedFile, totalFiles;
char *dirHighLight;	//the hightlight bar to delineate the selected dir
char *fileHighLight;	//the hightlight bar to delineate the selected file
char dirStar[MAXDIRS];	//we need a byte-length declaration, so use char
char fileStar[MAXFILES];//both of these indicate which dir/file is starred 
			//(selected) in the view
			//value 42 is '*' and value 32 is ' '
char dirType[MAXDIRS];	//The type of dir: either hidden or regular dir
char fileType[MAXFILES];//The type of file
int activeWindow;	//int value indicating which window is active

//values used in the activeWindow variable//
#define DIR_WIN		1
#define FILE_WIN	2
#define FILE_MENU	3
#define EDIT_MENU	4
#define HELP_MENU	5

//variables used in editing of dirs and files//
int numStarred;		//number of items starred (marked)
int numCut;		//number of items set to be cut
int numCopy;		//number of items selected for copy
//optional -- ask user for file name to export to
void exportTree(int showFileNameDialogBox); 
void exportTreeFromCommandLine(char *d, char *f);

//function called by exportTree() function
void scanThisDir(char tmp[], int level, int showProgress);

char *cwd;	//the current working directory
/* defined in args.c */
extern char *log_file_name;//string holding the name of the log file
extern FILE *log_file;

extern char *config_file_name;//string holding the name of the config file
extern FILE *config_file;
extern int GNU_DOS_LEVEL;

//Array to store different colors that will be given
//to files of different types.. The array is indexed by
//a character indicating the type of file/dir.. Currently,
//the chars are:
//'d' for directories
//'x' for executable files
//'r' for regular files
//'l' for links
//'a' for archives
//'h' for hidden files
//'p' for picture files
char FILE_DIR_COLOR[26];

/* defined in args.c */
extern char *prime_ver;
#endif