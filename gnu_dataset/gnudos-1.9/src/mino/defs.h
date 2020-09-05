/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014, 2015 (c)
 * 
 *    file: defs.h
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

#include <stdio.h>
#include <errno.h>

#ifndef __Mino_PRO_H
#define __Mino_PRO_H

#ifndef __BOOL_DEFINED_
#define __BOOL_DEFINED_
typedef int bool;
#endif

int SCREEN_W;
int SCREEN_H;

//Definitions for box-drawing characters//
#define ULC	0x6C//201	//Upper Left Corner of Double-box
#define LLC	0x6D//200	//Lower Left Corner of Double-box
#define URC	0x6B//187	//Upper Right Corner of Double-box
#define LRC	0x6A//188	//Lower Right Corner of Double-box
#define HB	0x71//205	//Horizontal bar of Double-box
#define VB	0x78//186	//Vertical bar of Double-box
#define RFT	0x74//204	//Right-facing T of Double-box
#define LFT	0x75//185	//Left-facing T of Double-box
#define DFT	0x77//203	//Down-facing T of Double-box
#define UFT	0x76//202	//Up-facing T of Double-box
#define MC	0x7E//206	//Middle-Cross of Double-box

//Definitions for colors//
#define BLACK      30      //set black foreground
#define RED        31      //set red foreground
#define GREEN      32      //set green foreground
#define BROWN      33      //set brown foreground
#define BLUE       34      //set blue foreground
#define MAGENTA    35      //set magenta foreground
#define CYAN       36      //set cyan foreground
#define WHITE      37      //set white foreground
#define BGBLACK    40      //set black background
#define BGRED      41      //set red background
#define BGGREEN    42      //set green background
#define BGBROWN    43      //set brown background
#define BGBLUE     44      //set blue background
#define BGMAGENTA  45      //set magenta background
#define BGCYAN     46      //set cyan background
#define BGWHITE    47      //set white background
#define BGDEFAULT  49      //set default background color

//Definitions for keyboard key scancodes//
//--> use ones defined in kbd.h
/*#define UP_KEY		1
#define DOWN_KEY	2
#define LEFT_KEY	3
#define RIGHT_KEY	4
#define HOME_KEY	5
#define END_KEY		6
#define ENTER_KEY	7
#define TAB_KEY		8
#define BACKSPACE_KEY	9
#define ESC_KEY		10
#define SPACE_KEY	32
#define DEL_KEY		12
#define ALT_F_KEY	13
#define ALT_E_KEY	14
#define ALT_H_KEY	15
#define CTRL_X_KEY	16
#define CTRL_V_KEY	17
#define CTRL_O_KEY	18
#define CTRL_P_KEY	19
#define CTRL_F_KEY	20
#define CTRL_A_KEY	21
*/

//buttons used in message boxes//
#define OK	1	//00000001
#define YES	2	//00000010
#define CANCEL	4	//00000100
#define NO	8	//00001000
//button combinations-->  00000101	OK/CANCEL = 5
			//00001010	YES/NO = 10
//types of message displayed in message boxes//
typedef enum msgT { INFO, ERROR, CONFIRM } msgType;

#define MAXDIRS		255
#define	MAXFILES	255
//int MAXDIRS;
//int MAXFILES;
int MAX_DIR_NAME_LEN;
int MAX_FILE_NAME_LEN;
int MAX_MSG_BOX_W;
int MAX_MSG_BOX_H;
#define MAX_INPUT_MSG_LEN 100

//Function prototypes//
//clearArea is a flag to indicate whether to clear the window with spaces
void drawBox(int x1, int y1, int x2, int y2, char *title, int clearArea);   
void drawMenuBar(int x, int y, int w);
void setScreenColors(int FG, int BG);
void getScreenSize();
void clearScreen();
void init();
void init_built_in_defaults();
void init_error();
void exit_gracefully(int exit_code);
void parseLineArgs(int argc, char **argv);
int scanDir(char *dir);

int msgBox(char *msg, int Buttons, msgType tmsg);
char* inputBox(char *msg, char *title);
char input[(MAX_INPUT_MSG_LEN*4)+1];//input string returned by inputBox() function

void refreshDirView();
void refreshView();
void refreshSelectedLine();
void refreshSelectedLineInColor(int pos);
void refreshBottomView();
void showFileMenu(int visible);
void showEditMenu(int visible);
void showOptionsMenu(int visible);
void showHelpMenu(int visible);
void showReadMe();	//called from help menu to show README file
void showKeybindings();	//called from help menu to show KEYBINDINGS file

//openSave determines if the function is to open or save a file
//showDialog: if YES, show open dialogbox, if NO, don't
//The function returns 1 if successful, 0 if failed to open/save file
typedef enum opensaveE { OPEN, SAVE } OPEN_SAVE;
int openSaveFile(OPEN_SAVE openSave, int showDialog);

int _openFile();//called by above function to actually do the work of opening file
int _saveFile();//called by above function to actually do the work of saving file
bool NEW_FILE;	//boolean telling if the current file is a new or opened one

//Menu items//
char *menu[4];
#define totalMainMenus	4	//total items in main menu bar
#define fTotal		6	//total items in file menu
#define eTotal		10	//total items in mino menu
#define oTotal		4	//total items in options menu
#define hTotal		4	//total items in help menu
char *fileMenu[6];
char *editMenu[10];
char *optionsMenu[4];
char *helpMenu[4];

int x, y, w, h;
char *dirs[MAXDIRS];
char *files[MAXFILES];
int numVisDirs, firstVisDir, selectedDir, totalDirs;
int numVisFiles, firstVisFile, selectedFile, totalFiles;

//values used in the activeWindow variable//
#define DIR_WIN		1
#define FILE_WIN	2
#define FILE_MENU	3
#define Mino_MENU	4
#define HELP_MENU	5

void catchSignals();

char *cwd;	//the current working directory
char *config_file_name;//string holding the name of the config file
char *open_file_name;
FILE *config_file;
FILE *open_file;
bool open_file_at_startup;

#define MAX_LINES	5000
char *lines[MAX_LINES];
int MAX_CHARS_PER_LINE;
int firstVisLine;
int totalVisLines;
int totalLines;
int totalCharsInLine[MAX_LINES];
bool LINE_IS_LINKED[MAX_LINES];
int selectedLine;
int selectedChar;
int selectedCharCarry;
bool WRAP_LINES;//if TRUE (1), lines are wrapped, ie will not go on beyond screen limits
int TAB_CHARS;	//number of spaces in a TAB
char *documentTitle;//[MAX_FILE_NAME_LEN];	//document title is the file name
char *DEFAULT_TITLE;
void insertChar(char *ch);
void insertTab();
void insertEnter();
void deletePrevChar();
void deleteNextChar();
void deletePrevWord();
void deleteNextWord();
void deleteLine();
//enumeration determining the state of the open file
enum fstate { MODIFIED, NEW, SAVED, OPENED, IDLE } FILE_STATE;
//whether to show README file on startup
bool SHOW_README;
void showREADMEOnStartup();

void sighandler(int signo);

//print unicode char at line 'pos' with index of 'index'
//into the screen
void putuchar(int pos, int index);
void putunichar(char *ch);
int GNU_DOS_LEVEL;
int makestr(char *newstr, char *str);
void move_lines_up(int first, int last);
void move_lines_down(int first, int last);
void calcCharCarry(int pos);
bool is_whitespace(char c);

#endif
