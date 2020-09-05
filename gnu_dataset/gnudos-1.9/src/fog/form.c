/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014 (c)
 * 
 *    file: form.c
 *    This file is part of fog.
 *
 *    fog is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    (at your option) any later version.
 *
 *    fog is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with fog.  If not, see <http://www.gnu.org/licenses/>.
 */    
#include "fog.h"
#include "form.h"

void drawForm() 
{
  int designW = SCREEN_W-16;
  int designH = SCREEN_H-3;
  static int startX = 0;
  static int startY = 0;
  int i;
  char *tmp = (char *) malloc(MAX_FORM_TITLE_LEN);
  strcpy(tmp, "(");
  strcat(tmp, FORM_TITLE);
  strcat(tmp, "): Design\0");
  if(FORM_HEIGHT <= designH && FORM_WIDTH <= designW) 
  {
    //drawBox(3, 1, SCREEN_H-1, SCREEN_W-16, "", YES);
    //drawBox(3, 1, FORM_HEIGHT+4, FORM_WIDTH+2, FORM_TITLE, YES);
    drawBox(3, 1, FORM_HEIGHT+4, FORM_WIDTH+2, tmp, YES);
  }
  else
  {
    drawBox(3, 1, SCREEN_H-1, SCREEN_W-16, tmp, YES);
    if(startY == 0)
    {
      if(//adding_tool &&
	 (tool_pos[selected_form_tool].y+
			 strlen(form_tool_text[selected_form_tool]))
					> designW-2)
	startY++;
    }
    else
    {
      if(tool_pos[selected_form_tool].y < startY)
	startY--;
      else if(//adding_tool &&
	      (tool_pos[selected_form_tool].y+
	      strlen(form_tool_text[selected_form_tool]))
					> designW-2)
	startY++;
    }//end inner if
    if(startX == 0)
    {
      if(//adding_tool && 
	tool_pos[selected_form_tool].x >= designH-2)
	startX++;
    }
    else
    {
      if(tool_pos[selected_form_tool].x < startX)
	startX--;
      else if(//adding_tool &&
	      startX+tool_pos[selected_form_tool].x >= designH-2)
	startX++;
    }//end inner if
  }
  free(tmp);
  /////////////////////////////////////////////
  //draw the tools
  /////////////////////////////////////////////
  if(FORM_HEIGHT <= designH && FORM_WIDTH <= designW)
  {
    for(i = 0; i < total_form_tools; i++) 
    {
      if(i == selected_form_tool)
        setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
      else 
        setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
      printf("\e[%d;%dH", tool_pos[i].x+3, tool_pos[i].y+1);
      printf("%s", form_tool_text[i]);
    }
    printf("\e[%d;%dH", tool_pos[selected_form_tool].x+3, 
		      tool_pos[selected_form_tool].y+1
		      +(int)strlen(form_tool_text[selected_form_tool]));
  }
  else
  {
    for(i = 0; i < total_form_tools; i++) 
    {
      if(tool_pos[i].x < startX || tool_pos[i].x > startX+designH)
	continue;
      if(tool_pos[i].y+strlen(form_tool_text[selected_form_tool]) <
	  startY || tool_pos[i].y > startY+designW)
	continue;
      if(i == selected_form_tool)
        setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
      else 
        setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
      printf("\e[%d;%dH", tool_pos[i].x+3-startX, (tool_pos[i].y+2-startY > 1) ? tool_pos[i].y+2-startY : 2);
      printf("%s", form_tool_text[i]+((startY>tool_pos[i].y) ?
				      (startY-tool_pos[i].y):0));
    }
    printf("\e[%d;%dH", tool_pos[selected_form_tool].x+3-startX, 
		      tool_pos[selected_form_tool].y+2-startY
		      +(int)strlen(form_tool_text[selected_form_tool]));
  }
  fflush(stdout);
}

void getInputFromFormWin() 
{
  int c;
  int toolX = 1;
  int toolY = 1;
  if(adding_tool) {//if1
    //check if there is room for a new tool
    if(total_form_tools >= MAX_FORM_TOOLS) 
    {
      msgBox("Form has maximum number of tools. Can't add more.", OK, INFO);
      drawForm();
      return;
    }
    char *tmp = (char *) malloc(FORM_WIDTH);
    char *tmp2 = (char *) malloc(5);
    //make the tool text according to its type
    if(strcmp(tool[selected_tool], "Bullet item") == 0) 
    {
      strcpy(tmp, BULLET);
      strcat(tmp, tool[selected_tool]);
    } 
    else if(strcmp(tool[selected_tool], "Option item") == 0) 
    {
      strcpy(tmp, "[ ] ");
      strcat(tmp, tool[selected_tool]);
    } 
    else if(strcmp(tool[selected_tool], "Button") == 0) 
    {
      strcpy(tmp, "  ");
      strcat(tmp, tool[selected_tool]);
    } 
    else 
    {
      strcpy(tmp, tool[selected_tool]);
    }
    //strcpy(tmp, tool[selected_tool]);
    sprintf(tmp2, "%d", ++tool_count[selected_tool]);
    strcat(tmp, tmp2);
    if(strcmp(tool[selected_tool], "Button") == 0)
      strcat(tmp, "  ");
    strcat(tmp, "\0");
    printf("\e[%d;%dH", toolX+3, toolY+1);
    printf("%s", tmp);
    fflush(stdout);
    //get user input
    while(1) 
    {
      c = getKey();
      switch(c) {
	case(UP_KEY):
	  if(toolX <= 1) break;
	  drawForm();
	  toolX--;
	  printf("\e[%d;%dH", toolX+3, toolY+1);
	  printf("%s", tmp);
	  fflush(stdout);
	  break;
	case(DOWN_KEY):
	  if(toolX >= FORM_HEIGHT-1) break;
	  drawForm();
	  toolX++;
	  printf("\e[%d;%dH", toolX+3, toolY+1);
	  printf("%s", tmp);
	  fflush(stdout);
	  break;
	case(LEFT_KEY):
	  if(toolY <= 1) break;
	  drawForm();
	  toolY--;
	  printf("\e[%d;%dH", toolX+3, toolY+1);
	  printf("%s", tmp);
	  fflush(stdout);
	  break;
	case(RIGHT_KEY):
	  if(toolY >= FORM_WIDTH-strlen(tmp)) break;
	  drawForm();
	  toolY++;
	  printf("\e[%d;%dH", toolX+3, toolY+1);
	  printf("%s", tmp);
	  fflush(stdout);
	  break;
	case(ESC_KEY):
	  adding_tool = 0;
	  tool_count[selected_tool]--;
	  //selected_tool = -1;
	  free(tmp); free(tmp2);
	  return; break;
	case(ENTER_KEY):
	  strcpy(form_tool[total_form_tools], tool[selected_tool]);
	  strcpy(form_tool_text[total_form_tools], tmp);
	  tool_pos[total_form_tools].x = toolX;
	  tool_pos[total_form_tools].y = toolY;
	  //tool_count[selected_tool]++;
	  selected_form_tool = total_form_tools;
	  adding_tool = 0;
	  total_form_tools++;
	  free(tmp); free(tmp2);
	  drawForm();
	  drawStatusBar();
	  FILE_STATE = MODIFIED;
	  return; break;
      }//end switch
    }//end while
  ///////////////////////////////////////////////////////
  ///////////////////////////////////////////////////////
  ///////////////////////////////////////////////////////
  } 
  else 
  {
    //not adding a new tool...
    //get user input
    while(1) 
    {
      c = getKey();
      switch(c)
      {
	case(UP_KEY):
	  if(total_form_tools <= 0) break;
	  if(tool_pos[selected_form_tool].x <= 1) break;
	  tool_pos[selected_form_tool].x--;
	  drawForm();
	  break;
	case(DOWN_KEY):
	  if(total_form_tools <= 0) break;
	  if(tool_pos[selected_form_tool].x >= FORM_HEIGHT) break;
	  tool_pos[selected_form_tool].x++;
	  drawForm();
	  break;
	case(LEFT_KEY):
	  if(total_form_tools <= 0) break;
	  if(tool_pos[selected_form_tool].y <= 1) break;
	  tool_pos[selected_form_tool].y--;
	  drawForm();
	  break;
	case(RIGHT_KEY):
	  if(total_form_tools <= 0) break;
	  if(tool_pos[selected_form_tool].y >= 
	    FORM_WIDTH-strlen(form_tool_text[selected_form_tool])) break;
	  tool_pos[selected_form_tool].y++;
	  drawForm();
	  break;
	case(TAB_KEY):
	  if(total_form_tools <= 0) break;
	  if(++selected_form_tool >= total_form_tools) selected_form_tool = 0;
	  drawForm();
	  break;
	case('t'):
	  activeWindow = TOOLBOX_WIN;
	  drawForm();
	  drawToolBox();
	  drawStatusBar();
	  return; break;
	case(ENTER_KEY):
	  if(total_form_tools <= 0) break;
	  char *msg = (char *) 
		    malloc(strlen(form_tool_text[selected_form_tool])+64);
	  strcpy(msg, "Old tool text is:\n");
	  strcat(msg, form_tool_text[selected_form_tool]);
	  strcat(msg, "\nEnter new text (CANCEL to retain old text):\0");
	  inputBoxI(msg, form_tool_text[selected_form_tool], 
		    "Change tool text");
	  free(msg);
	  if(strlen(input))
	  {
	    strcpy(form_tool_text[selected_form_tool], input);
	    FILE_STATE = MODIFIED;
	  }
	  drawForm();
	  drawStatusBar();
	  break;
	case(DEL_KEY):
	  if(total_form_tools <= 0) break;
	  int i;
	  for(i = selected_form_tool; i < total_form_tools-1; i++) 
	  {
	    strcpy(form_tool_text[i], form_tool_text[i+1]);
	    strcpy(form_tool[i], form_tool[i+1]);
	    tool_pos[i].x = tool_pos[i+1].x;
	    tool_pos[i].y = tool_pos[i+1].y;
	  } total_form_tools--;
	  if(selected_form_tool >= total_form_tools) 
	    selected_form_tool = total_form_tools-1;
	  FILE_STATE = MODIFIED;
	  drawForm();
	  drawStatusBar();
	  break;
	case('s'):
	  if(CTRL) saveForm();
	  break;
	case('o'):
	  if(CTRL) { openForm(); return; }
	  if(ALT) showMenu(2);
	  break;
	case('n'):
	  if(CTRL) { newForm(); return; }
	  break;
	case('q'):
	  if(CTRL) { endme = 1; return; }
	  break;
	case('w'):
	  if(CTRL) makeProject();
	  break;
	case('f'): if(ALT) showMenu(0); break;
	case('e'): if(ALT) showMenu(1); break;
	case('h'): if(ALT) showMenu(3); break;
	case(ESC_KEY):
	  restoreTerminal(); exit(0); break;
      }//end switch
    }//end while
  }//end if1
}

int saveForm()
{
  FILE *file;
  if(NEW_FILE) 
  {
    inputBoxI("Enter file name to save the form:", 
	     form_file_name, " Save Form As ");
    if(strlen(input))
      strcpy(form_file_name, input);
    else { drawForm(); drawToolBox(); drawStatusBar(); return 0; }
    
    if((file = fopen(form_file_name, "r")))
    {
      int i = msgBox("File already exists. Overwrite?", YES|NO, CONFIRM);
      fclose(file);
      if(i == NO) { drawForm(); drawToolBox(); drawStatusBar(); return 0; };
    }
  }

  if(!(file = fopen(form_file_name, "w"))) 
  {
    msgBox("Error creating form file.", OK, ERROR);
    drawForm(); drawToolBox(); drawStatusBar();
    return 0;
  }
  
  fprintf(file, "#!/bin/fog\n");
  fprintf(file, "#Fog form design file\n");
  fprintf(file, "#Please do not edit by hand!\n\n");
  fprintf(file, "[FORM]\n");
  fprintf(file, "Width=%d\n", FORM_WIDTH);
  fprintf(file, "Height=%d\n", FORM_HEIGHT);
  fprintf(file, "Title=%s\n", FORM_TITLE);
  fprintf(file, "Tools=%d\n", total_form_tools);
  int i;
  for(i = 0; i < total_tools; i++) 
  {
    fprintf(file, "n=%d\n", tool_count[i]);
  }
  //fprintf(file, "\n");
  for(i = 0; i < total_form_tools; i++) 
  {
    fprintf(file, "\n[%s]\n", form_tool[i]);
    fprintf(file, "Text=%s\n", form_tool_text[i]);
    fprintf(file, "x=%d\n", tool_pos[i].x);
    fprintf(file, "y=%d\n", tool_pos[i].y);
  }
  fclose(file);
  NEW_FILE = 0;
  FILE_STATE = SAVED;
  drawForm();
  drawToolBox();
  drawStatusBar();
  return 1;
}

void newForm() 
{
  if(FILE_STATE == MODIFIED)
  {
    if(!saveForm()) return;
  }
  selected_tool = 0;
  adding_tool = 0;
  selected_form_tool = 0;
  activeWindow = TOOLBOX_WIN;
  
  FORM_WIDTH = SCREEN_W-17;
  FORM_HEIGHT = SCREEN_H-5;
  strcpy(FORM_TITLE, "Sample Form");
  strcpy(form_file_name, "SampleForm.fog");
  NEW_FILE = 1;
  FILE_STATE = NEW;
  total_form_tools = 0;
  int i;
  for(i = 0; i < total_tools; i++)
    tool_count[i] = 0;
  drawForm();
  drawToolBox();
  drawStatusBar();
  return;
}

void openForm() 
{
  if(FILE_STATE == MODIFIED)
  {
    if(!saveForm()) return;
  }
  FILE *file;
  inputBox("Enter file name to open:", " Open Form ");
  if(!strlen(input)) { drawForm(); drawToolBox(); drawStatusBar(); return; }
    
  if(!(file = fopen(input, "r"))) 
  {
    msgBox("File does not exist.", OK, ERROR);
    drawForm(); drawToolBox(); drawStatusBar();
    return;
  }
  
  char buf[1024];
  int i = -1;
  int j = 0;
  int k = -1;  
  while(fgets(buf, sizeof(buf), file)) 
  {
    i++;
    if(i == 0)
      if(strstr(buf, "/bin/fog") == NULL) 
      {
	msgBox("File is not a valid fog file.", OK, ERROR);
	drawForm();
	drawToolBox();
	drawStatusBar();
	return;
      }
    if(buf[0] == '#') continue;	//this is a commented line
    if(strstr(buf, "Width")) 
    {
      FORM_WIDTH = atoi(strstr(buf, "=")+1);
    } 
    else if(strstr(buf, "Height")) 
    {
      FORM_HEIGHT = atoi(strstr(buf, "=")+1);
    } 
    else if(strstr(buf, "Title")) 
    {
      strcpy(FORM_TITLE, strstr(buf, "=")+1);
      if(FORM_TITLE[strlen(FORM_TITLE)-1] == '\n')
	FORM_TITLE[strlen(FORM_TITLE)-1] = '\0';
    } 
    else if(strstr(buf, "Tools")) 
    {
      total_form_tools = atoi(strstr(buf, "=")+1);
    }
    else if(strstr(buf, "n=")) 
    {
      tool_count[j] = atoi(strstr(buf, "=")+1); j++;
    } 
    else if(strstr(buf, "[")) 
    {
      if(strstr(buf, "[FORM]")) continue;
      k++;
      strncpy(form_tool[k], strstr(buf, "[")+1, strlen(strstr(buf, "[")+1)-2);
    } 
    else if(strstr(buf, "Text")) 
    {
      strcpy(form_tool_text[k], strstr(buf, "=")+1);
      if(form_tool_text[k][strlen(form_tool_text[k])-1] == '\n')
	form_tool_text[k][strlen(form_tool_text[k])-1] = '\0';
    } 
    else if(strstr(buf, "x")) 
    {
      tool_pos[k].x = atoi(strstr(buf, "=")+1);
    } 
    else if(strstr(buf, "y")) 
    {
      tool_pos[k].y = atoi(strstr(buf, "=")+1);
    }
  }//end while

  selected_tool = 0;
  adding_tool = 0;
  if(total_form_tools) selected_form_tool = 0;
  else selected_form_tool = -1;
  activeWindow = FORM_WIN;
  
  strcpy(form_file_name, input);
  NEW_FILE = 1;
  FILE_STATE = OPENED;
  drawForm();
  drawToolBox();
  drawStatusBar();
  return;
}


void deleteFormTool() 
{
	  if(total_form_tools <= 0) return;
	  int i;
	  for(i = selected_form_tool; i < total_form_tools-1; i++) 
	  {
	    strcpy(form_tool_text[i], form_tool_text[i+1]);
	    strcpy(form_tool[i], form_tool[i+1]);
	    tool_pos[i].x = tool_pos[i+1].x;
	    tool_pos[i].y = tool_pos[i+1].y;
	  } total_form_tools--;
	  if(selected_form_tool >= total_form_tools) 
	    selected_form_tool = total_form_tools-1;
	  FILE_STATE = MODIFIED;
}

void editFormToolText() 
{
	  if(total_form_tools <= 0) return;
	  char *msg = (char *) 
		      malloc(strlen(form_tool_text[selected_form_tool])+64);
	  strcpy(msg, "Old tool text is:\n");
	  strcat(msg, form_tool_text[selected_form_tool]);
	  strcat(msg, "\nEnter new text (CANCEL to retain old text):\0");
	  inputBoxI(msg, form_tool_text[selected_form_tool], 
		    "Change tool text");
	  free(msg);
	  if(strlen(input))
	    strcpy(form_tool_text[selected_form_tool], input);
	  FILE_STATE = MODIFIED;
}

void changeFormTitle() 
{
	  char *msg = (char *) malloc(strlen(FORM_TITLE)+64);
	  strcpy(msg, "Old form title is:\n");
	  strcat(msg, FORM_TITLE);
	  strcat(msg, "\nEnter new title (CANCEL to retain old title):\0");
	  inputBoxI(msg, FORM_TITLE, "Change form title");
	  free(msg);
	  if(strlen(input))
	    strcpy(FORM_TITLE, input);
	  FILE_STATE = MODIFIED;
}

void changeFormWidth() 
{
	  char *msg = (char *) malloc(70);
	  char *fw = (char *) malloc(5);
	  sprintf(msg, "Old form width is: %d\n", FORM_WIDTH);
	  strcat(msg, "Enter new width (CANCEL to retain old width):\0");
	  sprintf(fw, "%d", FORM_WIDTH);
	  inputBoxI(msg, fw, "Change form width");
	  free(msg);
	  free(fw);
	  if(strlen(input))
	    FORM_WIDTH = atoi(input);
	  FILE_STATE = MODIFIED;
	  drawBox(2, 1, SCREEN_H-1, SCREEN_W, " FOG: The console Form Designer ", YES);
	  drawToolBox();
}

void changeFormHeight() 
{
	  char *msg = (char *) malloc(70);
	  char *fw = (char *) malloc(5);
	  sprintf(msg, "Old form height is: %d\n", FORM_HEIGHT);
	  strcat(msg, "Enter new height (CANCEL to retain old height):\0");
	  sprintf(fw, "%d", FORM_HEIGHT);
	  inputBoxI(msg, fw, "Change form height");
	  free(msg);
	  free(fw);
	  if(strlen(input))
	    FORM_HEIGHT = atoi(input);
	  FILE_STATE = MODIFIED;
	  drawBox(2, 1, SCREEN_H-1, SCREEN_W, " FOG: The console Form Designer ", YES);
	  drawToolBox();
}