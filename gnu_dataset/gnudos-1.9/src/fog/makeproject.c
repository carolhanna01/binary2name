/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014 (c)
 * 
 *    file: makeproject.c
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

/**************************************************
 * This function creates the project output files:
 * (1) main.c: contains the main function and
 *     function calls to the form design functions
 * (2) fog_header.h: contains function definitions
 *     and other declarations
 * (3) form_design.c: contains function defitions to
 *     draw the form
 * ************************************************/
void makeProject() 
{
  if(strrchr(form_file_name, '/')) 
  {
    int j = strrchr(form_file_name, '/')-form_file_name;
    char *dir = (char *) malloc(MAX_FORM_FILE_NAME_LEN);
    strncpy(dir, form_file_name, j);
    strcat(dir, "\0");
    j = chdir(dir);
    free(dir);
  }
  
  FILE *file;
  int i, overwrite_all = 0;;
  if((file = fopen("main.c", "r")))
  {
    int i = msgBox("File:\nmain.c\nalready exists. Overwrite?", YES|NO|ALL,
		   CONFIRM);
    fclose(file);
    if(i == NO) goto write_header_file;
    if(i == ALL) overwrite_all = 1;
  }
  if((file = fopen("main.c", "w"))) 
  {
    fprintf(file, "/*main.c: Source file containing main() function\n");
    fprintf(file, " *        accompanying %s.\n *\n", form_file_name);
    fprintf(file, " *Created by GNU/Linux Fog Version 1.1\n");
    fprintf(file, " *By Mohammed Isam, 2014\n");
    fprintf(file, 
      " *\n *Will need to be linked against libgnudos in order to compile\n");
    fprintf(file, 
      " *e.g.\n *  gcc -o myprog main.c form_design.c -lgnudos\n */\n\n");
    fprintf(file, "#include \"fog_header.h\"\n\n");
    fprintf(file, "void sighandler(int signo)\n");
    fprintf(file, "{\n\t\n");
    fprintf(file, "}\n\n");
    fprintf(file, "int main()\n");
    fprintf(file, "{\n\tinit_form();\n");
    fprintf(file, "\trefresh_form();\n");
    fprintf(file, "\tinput_loop();\n");
    fprintf(file, "\tclose_form();\n");
    fprintf(file, "\texit(0);\n");
    fprintf(file, "}\n");
    for(i = 0; i < total_form_tools; i++) 
    {
      if(strcmp(form_tool[i], "Button") != 0) continue;
      fprintf(file, "\nvoid buttonClicked(int button)\n");
      fprintf(file, " {\n");
      fprintf(file, "\tswitch(button)\n");
      fprintf(file, "\t{\n");
      for(i = 0; i < total_form_tools; i++) 
      {
	if(strcmp(form_tool[i], "Button") != 0) continue;
	fprintf(file, "\tcase(%d):  //%s\n", i, form_tool_text[i]);
	fprintf(file, "\t  msgBox(\"Button clicked.\", OK, INFO);\n");
	fprintf(file, "\t  refresh_form();\n");
	fprintf(file, "\t  break;\n");
      }
      fprintf(file, "\t}\n");
      break;
    }
    fprintf(file, "}\n");
    fflush(file);
    fclose(file);
  } 
  else 
  {
    msgBox("Error creating file: main.c", OK, ERROR);
  }

write_header_file:
  if(!overwrite_all)
    if((file = fopen("fog_header.h", "r")))
    {
      int i = msgBox("File:\nfog_header.h\nalready exists. Overwrite?", 
		     YES|NO|ALL, CONFIRM);
      fclose(file);
      if(i == NO) goto write_design_file;
      if(i == ALL) overwrite_all = 1;
    }
  if((file = fopen("fog_header.h", "w"))) 
  {
    fprintf(file, 
	    "/*fog_header.h: Header file containing function prototypes\n");
    fprintf(file, " *              and declarations accompanying %s.\n *\n",
	    form_file_name);
    fprintf(file, " *Created by GNU/Linux Fog Version 1.1\n");
    fprintf(file, " *By Mohammed Isam, 2014\n */\n\n");
    fprintf(file, "#ifndef __FOG_HEADER_H\n");
    fprintf(file, "#define __FOG_HEADER_H\n\n");
    fprintf(file, "#include \"console/dialogs.h\"\n");
    fprintf(file, "#include <string.h>\n");
    fprintf(file, "#include <stdio.h>\n");
    fprintf(file, "\nvoid init_form();\n");
    fprintf(file, "void refresh_form();\n");
    fprintf(file, "void input_loop();\n");
    fprintf(file, "void close_form();\n");
    fprintf(file, "\n");
    fprintf(file, "int FORM_WIDTH;\n");
    fprintf(file, "int FORM_HEIGHT;\n");
    fprintf(file, "int x, y;\n");
    fprintf(file, "char *FORM_TITLE;\n");
    //fprintf(file, "int TOOLS;\n\n");
    fprintf(file, "typedef struct\n");
    fprintf(file, "{\n");
    fprintf(file, "  int x;\n");
    fprintf(file, "  int y;\n");
    fprintf(file, "} tpos;\n");
    fprintf(file, "\n");
    fprintf(file, "#define total_form_tools %d\n", total_form_tools);
    //fprintf(file, "#define MAX_FORM_TOOLS	30\n");
    fprintf(file, "char *form_tool[total_form_tools];\n");
    fprintf(file, "char *form_tool_text[total_form_tools];\n");
    fprintf(file, "tpos tool_pos[total_form_tools];\n");
    //fprintf(file, "#define total_tools 5\n");
    //fprintf(file, "int tool_count[total_tools];\n");
    fprintf(file, "int selected_tool;\n");
    fprintf(file, "int selected_option;\n");
    for(i = 0; i < total_form_tools; i++) 
    {
      if(strcmp(form_tool[i], "Button") != 0) continue;
      fprintf(file, "void buttonClicked(int button);\n");
      break;
    }
    for(i = 0; i < total_form_tools; i++) 
    {
      if(strcmp(form_tool[i], "Input field") != 0) continue;
      fprintf(file, "char *input[%d];\n", total_form_tools);
      fprintf(file, "#define MAX_INPUT_LEN\t255\n");
      //fprintf(file, "void editInputField(int index);\n");
      break;
    }
    fprintf(file, "#endif\n");
    fflush(file);
    fclose(file);
  } 
  else 
  {
    msgBox("Error creating file: fog_header.h", OK, ERROR);
  }

write_design_file:
  if(!overwrite_all)
    if((file = fopen("form_design.c", "r")))
    {
      int i = msgBox("File:\nform_design.c\nalready exists. Overwrite?", 
		     YES|NO|ALL, CONFIRM);
      fclose(file);
      if(i == NO) goto finished;
      if(i == ALL) overwrite_all = 1;
    }
  if((file = fopen("form_design.c", "w"))) 
  {
    fprintf(file, 
	    "/*form_design.c: Source file containing function definitions\n");
    fprintf(file, " *               and form design accompanying %s.\n *\n",
	    form_file_name);
    fprintf(file, " *Created by GNU/Linux Fog Version 1.1\n");
    fprintf(file, " *By Mohammed Isam, 2014\n */\n\n");
    fprintf(file, "#include \"fog_header.h\"\n");
    fprintf(file, "\n");
    fprintf(file, "void init_form() {\n");
    fprintf(file, "\tFORM_WIDTH = %d;\n", FORM_WIDTH);
    fprintf(file, "\tFORM_HEIGHT = %d;\n", FORM_HEIGHT);
    fprintf(file, "\tFORM_TITLE = (char *) malloc(%d);\n", (int)strlen(FORM_TITLE));
    fprintf(file, "\tstrcpy(FORM_TITLE, \"%s\");\n", FORM_TITLE);
    //fprintf(file, "\ttotal_form_tools = %d;\n\n", total_form_tools);
    fprintf(file, "\tFG_COLOR[COLOR_WINDOW] = %d;\n", FG_COLOR[COLOR_WINDOW]);
    fprintf(file, "\tFG_COLOR[COLOR_HIGHLIGHT_TEXT] = %d;\n", 
	    FG_COLOR[COLOR_HIGHLIGHT_TEXT]);
    fprintf(file, "\tFG_COLOR[COLOR_BUTTONS] = %d;\n", 
	    FG_COLOR[COLOR_BUTTONS]);
    fprintf(file, "\tFG_COLOR[COLOR_HBUTTONS] = %d;\n", 
	    FG_COLOR[COLOR_HBUTTONS]);
    fprintf(file, "\tBG_COLOR[COLOR_WINDOW] = %d;\n", BG_COLOR[COLOR_WINDOW]);
    fprintf(file, "\tBG_COLOR[COLOR_HIGHLIGHT_TEXT] = %d;\n", 
	    BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
    fprintf(file, "\tBG_COLOR[COLOR_BUTTONS] = %d;\n", 
	    BG_COLOR[COLOR_BUTTONS]);
    fprintf(file, "\tBG_COLOR[COLOR_HBUTTONS] = %d;\n\n", 
	    BG_COLOR[COLOR_HBUTTONS]);
    int opt = -1;
    for(i = 0; i < total_form_tools; i++) 
    {
      fprintf(file, "\tform_tool[%d] = (char *) malloc(%d);\n", i, 
	      (int)strlen(form_tool[i]));
      fprintf(file, "\tstrcpy(form_tool[%d], \"%s\");\n", i, form_tool[i]);
      fprintf(file, "\tform_tool_text[%d] = (char *) malloc(%d);\n", 
	      i, (int)strlen(form_tool_text[i]));
      fprintf(file, "\tstrcpy(form_tool_text[%d], \"%s\");\n", 
	      i, form_tool_text[i]);
      fprintf(file, "\ttool_pos[%d].x = %d;\n", i, tool_pos[i].x);
      fprintf(file, "\ttool_pos[%d].y = %d;\n", i, tool_pos[i].y);
      if(strcmp(form_tool[i], "Option item") == 0)
	if(opt < 0) {opt = i; fprintf(file, "\tselected_option = %d;\n", i);}
    }
    for(i = 0; i < total_form_tools; i++) 
    {
      if(strcmp(form_tool[i], "Input field") != 0) continue;
      fprintf(file, "input[%d] = (char *) malloc(MAX_INPUT_LEN);\n", i);
      fprintf(file, "strcpy(input[%d], \"\\0\");\n", i);
    }
    fprintf(file, "\tselected_tool = 0;\n\n");
    fprintf(file, "\tinitTerminal();\n");
    fprintf(file, "\tgetScreenSize();\n");
    fprintf(file, "\tif(FORM_WIDTH > SCREEN_W) FORM_WIDTH = SCREEN_W;\n");
    fprintf(file, "\tif(FORM_HEIGHT > SCREEN_H) FORM_HEIGHT = SCREEN_H;\n");
    fprintf(file, "\tx = 1; y = 1;\n");
    fprintf(file, "\tif(FORM_WIDTH < SCREEN_W)\n");
    fprintf(file, "\t{\n");
    fprintf(file, "\t\ty = (SCREEN_W-FORM_WIDTH)/2;\n");
    fprintf(file, "\t\tint i;\n");
    fprintf(file, "\t\tfor(i = 0; i < total_form_tools; i++)\n");
    fprintf(file, "\t\t  tool_pos[i].y += y;\n");
    fprintf(file, "\t}\n");
    fprintf(file, "\tif(FORM_HEIGHT < SCREEN_H)\n");
    fprintf(file, "\t{\n");
    fprintf(file, "\t\tx = (SCREEN_H-FORM_HEIGHT)/2;\n");
    fprintf(file, "\t\tint i;\n");
    fprintf(file, "\t\tfor(i = 0; i < total_form_tools; i++)\n");
    fprintf(file, "\t\t  tool_pos[i].x += x;\n");
    fprintf(file, "\t}\n");
    fprintf(file, "\tclearScreen();\n");
    fprintf(file, "}\n");
    fprintf(file, "\n");
    fprintf(file, "void refresh_form()\n");
    fprintf(file, "{\n");
    fprintf(file, 
	  "\tdrawBox(x, y, x+FORM_HEIGHT, y+FORM_WIDTH, FORM_TITLE, YES);\n");
    fprintf(file, "\tint i;\n");
    fprintf(file, "\tfor(i = 0; i < total_form_tools; i++)\n");
    fprintf(file, "\t{\n");
    fprintf(file, "\t\tif(i == selected_tool)\n");
    fprintf(file, "\t\t\tif(strcmp(form_tool[i], \"Button\") == 0)\n");
    fprintf(file, "\t\t\t  setScreenColors(FG_COLOR[COLOR_HBUTTONS], "
				"BG_COLOR[COLOR_HBUTTONS]);\n");
    fprintf(file, "\t\t\telse\n");
    fprintf(file, "\t\t\t  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], "
				"BG_COLOR[COLOR_HIGHLIGHT_TEXT]);\n");
    fprintf(file, "\t\telse\n");
    fprintf(file, "\t\t\tif(strcmp(form_tool[i], \"Button\") == 0)\n");
    fprintf(file, "\t\t\t  setScreenColors(FG_COLOR[COLOR_BUTTONS], "
				"BG_COLOR[COLOR_BUTTONS]);\n");
    fprintf(file, "\t\t\telse\n");
    fprintf(file, "\t\t\t  setScreenColors(FG_COLOR[COLOR_WINDOW], "
				"BG_COLOR[COLOR_WINDOW]);\n");
    fprintf(file, "\t\tlocate(tool_pos[i].x, tool_pos[i].y);\n");
    fprintf(file, "\t\t//input fields are highlighted anyway//\n");
    fprintf(file, "\t\tif(strcmp(form_tool[i], \"Input field\") == 0)\n");
    fprintf(file, "\t\t{\n");
    fprintf(file, "\t\t\t  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], "
				"BG_COLOR[COLOR_HIGHLIGHT_TEXT]);\n");
    fprintf(file, "\t\t\t  printf(\"%%s\", input[selected_tool]);\n");
    fprintf(file, "\t\t\t  printf(\"%%*s\", "
			"FORM_WIDTH-tool_pos[selected_tool]."
			"x-strlen(input[selected_tool]), \" \");\n");
    fprintf(file, "\t\t\t  locate(tool_pos[selected_tool].x+selectedChar, "
			"tool_pos[selected_tool].y);\n");
    fprintf(file, "\t\t} else {\n");
    fprintf(file, "\t\t  printf(\"%%s\", form_tool_text[i]);\n");
    fprintf(file, "\t\t  if(i == selected_option)\n");
    fprintf(file, "\t\t  {\n");
    fprintf(file, "\t\t    locate(tool_pos[i].x, tool_pos[i].y+1);\n");
    fprintf(file, "\t\t    printf(\"X\");\n");
    fprintf(file, "\t\t  }\n");
    fprintf(file, "\t\t  locate(tool_pos[selected_tool].x, "
			"tool_pos[selected_tool].y);\n");
    fprintf(file, "\t\t}\n");
    fprintf(file, "\t}\n");
    fprintf(file, "\tfflush(stdout);\n");
    fprintf(file, "}\n");
    fprintf(file, "\n");
    fprintf(file, "void input_loop()\n");
    fprintf(file, "{\n");
    fprintf(file, "\tint c;\n");
    fprintf(file, "\tint i;\n");
    fprintf(file, "\tstatic int selectedChar = 0;\n");
    fprintf(file, "\twhile(1)\n");
    fprintf(file, "\t{\n");
    fprintf(file, "\t\tc = getKey();\n");
    fprintf(file, "\t\tswitch(c)\n");
    fprintf(file, "\t\t{\n");
    fprintf(file, "\t\tcase(LEFT_KEY):\n");
    fprintf(file, "\t\t  if(strcmp(form_tool[selected_tool], "
			"\"Input field\") == 0)\n");
    fprintf(file, "\t\t  {\n");
    fprintf(file, "\t\t    if(selectedChar > 0) selectedChar--;\n");
    fprintf(file, "\t\t    locate(tool_pos[selected_tool].x+selectedChar, "
			"tool_pos[selected_tool].y);\n");
    fprintf(file, "\t\t    break;\n");
    fprintf(file, "\t\t  }\n");
    fprintf(file, "\t\tcase(UP_KEY):\n");
    fprintf(file, "\t\t  selected_tool--;\n");
    fprintf(file, "\t\t  if(selected_tool < 0) selected_tool = "
			"total_form_tools-1;\n");
    fprintf(file, "\t\t  while(strcmp(form_tool[selected_tool], "
			"\"Message field\") == 0)\n");
    fprintf(file, "\t\t    { selected_tool--; if(selected_tool == 0) "
			"break; }\n");
    fprintf(file, "\t\t  selectedChar = 0;\n");
    fprintf(file, "\t\t  refresh_form();\n");
    fprintf(file, "\t\t  break;\n");
    fprintf(file, "\t\tcase(RIGHT_KEY):\n");
    fprintf(file, "\t\t  if(strcmp(form_tool[selected_tool], "
			"\"Input field\") == 0)\n");
    fprintf(file, "\t\t  {\n");
    fprintf(file, "\t\t    if(selectedChar < strlen(input[selected_tool]) "
			"selectedChar++;\n");
    fprintf(file, "\t\t    locate(tool_pos[selected_tool].x+selectedChar, "
			"tool_pos[selected_tool].y);\n");
    fprintf(file, "\t\t    break;\n");
    fprintf(file, "\t\t  }\n");
    fprintf(file, "\t\tcase(TAB_KEY):\n");
    fprintf(file, "\t\tcase(DOWN_KEY):\n");
    fprintf(file, "\t\t  selected_tool++;\n");
    fprintf(file, "\t\t  if(selected_tool > total_form_tools-1) "
			"selected_tool = 0;\n");
    fprintf(file, "\t\t  while(strcmp(form_tool[selected_tool], "
			"\"Message field\") == 0)\n");
    fprintf(file, "\t\t    { selected_tool++; if(selected_tool "
			">= total_form_tools-1) break; }\n");
    fprintf(file, "\t\t  selectedChar = 0;\n");
    fprintf(file, "\t\t  refresh_form();\n");
    fprintf(file, "\t\t  break;\n");
    fprintf(file, "\t\tcase(ESC_KEY):\n");
    fprintf(file, "\t\t  return; break;\n");
    //fprintf(file, "\t\tcase(SPACE_KEY):\n");
    fprintf(file, "\t\tcase(ENTER_KEY):\n");
    fprintf(file, "\t\t  if(strcmp(form_tool[selected_tool], "
			"\"Button\") == 0)\n");
    fprintf(file, "\t\t  { buttonClicked(selected_tool); break; }\n");
    fprintf(file, "\t\t  if(strcmp(form_tool[selected_tool], "
			"\"Option item\") == 0)\n");
    fprintf(file, "\t\t    if(selected_tool != selected_option) {\n");
    fprintf(file, "\t\t      setScreenColors(FG_COLOR[COLOR_WINDOW], "
			"BG_COLOR[COLOR_WINDOW]);\n");
    fprintf(file, "\t\t      locate(tool_pos[selected_option].x, "
			"tool_pos[selected_option].y);\n");
    fprintf(file, "\t\t      printf(\"%%s\", "
			"form_tool_text[selected_option]);\n");
    fprintf(file, "\t\t      selected_option = selected_tool;\n");
    fprintf(file, "\t\t      setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], "
			"BG_COLOR[COLOR_HIGHLIGHT_TEXT]);\n");
    fprintf(file, "\t\t      locate(tool_pos[selected_option].x, "
			"tool_pos[selected_option].y);\n");
    fprintf(file, "\t\t      printf(\"%%s\", "
			"form_tool_text[selected_option]);\n");
    fprintf(file, "\t\t      locate(tool_pos[selected_option].x, "
			"tool_pos[selected_option].y+1);\n");
    fprintf(file, "\t\t      printf(\"X\");\n");
    fprintf(file, "\t\t      fflush(stdout);\n");
    fprintf(file, "\t\t    }\n");
    fprintf(file, "\t\t  break;\n");
    fprintf(file, "\t\tcase(DEL_KEY):\n");
    fprintf(file, "\t\t  if(strcmp(form_tool[selected_tool], "
			"\"Input field\") == 0)\n");
    fprintf(file, "\t\t  {\n");
    fprintf(file, "\t\t    if(selectedChar == strlen(input[selected_tool]) "
			"break;\n");
    fprintf(file, "\t\t    for(i = selectedChar; i "
		"< strlen(input[selected_tool]); i++) input[selected_tool][i] "
		"= input[selected_tool][i+1];\n");
    fprintf(file, "\t\t    locate(tool_pos[selected_tool].x, "
		"tool_pos[selected_tool].y);\n");
    fprintf(file, "\t\t    setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], "
		"BG_COLOR[COLOR_HIGHLIGHT_TEXT]);\n");
    fprintf(file, "\t\t    printf(\"%%s\", input[selected_tool]);\n");
    fprintf(file, "\t\t    printf(\"%%*s\", "
		"FORM_WIDTH-tool_pos[selected_tool]."
		"x-strlen(input[selected_tool]), \" \");\n");
    fprintf(file, "\t\t    locate(tool_pos[selected_tool].x+selectedChar, "
		"tool_pos[selected_tool].y);\n");
    fprintf(file, "\t\t    break;\n");
    fprintf(file, "\t\t  }\n");
    fprintf(file, "\t\tcase(BACKSPACE_KEY):\n");
    fprintf(file, "\t\t  if(strcmp(form_tool[selected_tool], \"Input field\") "
		"== 0)\n\t\t  {\n");
    fprintf(file, "\t\t    if(selectedChar == 0) break;\n");
    fprintf(file, "\t\t    selectedChar--;\n");
    fprintf(file, "\t\t    for(i = selectedChar; i "
		"< strlen(input[selected_tool]); i++) input[selected_tool][i] "
		"= input[selected_tool][i+1];\n");
    fprintf(file, "\t\t    locate(tool_pos[selected_tool].x, "
		"tool_pos[selected_tool].y);\n");
    fprintf(file, "\t\t    setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], "
		"BG_COLOR[COLOR_HIGHLIGHT_TEXT]);\n");
    fprintf(file, "\t\t    printf(\"%%s\", input[selected_tool]);\n");
    fprintf(file, "\t\t    printf(\"%%*s\", "
		"FORM_WIDTH-tool_pos[selected_tool]."
		"x-strlen(input[selected_tool]), \" \");\n");
    fprintf(file, "\t\t    locate(tool_pos[selected_tool].x+selectedChar, "
		"tool_pos[selected_tool].y);\n");
    fprintf(file, "\t\t    break;\n");
    fprintf(file, "\t\t  }\n");
    fprintf(file, "\t\tdefault:\n");
    fprintf(file, "\t\t  if(strcmp(form_tool[selected_tool], \"Input field\") "
		"== 0)\n");
    fprintf(file, "\t\t    if(c >= 32 && c <= 126)\n");
    fprintf(file, "\t\t    {\n");
    fprintf(file, "\t\t      if(strlen(input[selected_tool]) "
		">= FORM_WIDTH-tool_pos[selected_tool].x-2) break;\n");
    fprintf(file, "\t\t      if(selectedChar == strlen(input[selected_tool])) "
		"\n\t\t      {\n");
    fprintf(file, "\t\t        input[selected_tool][selectedChar] = c;\n");
    fprintf(file, "\t\t        input[selected_tool][selectedChar+1] = '\\0'"
		";\n");
    fprintf(file, "\t\t        putchar(c);\n");
    fprintf(file, "\t\t        selectedChar++;\n");
    fprintf(file, "\t\t      }\n");
    fprintf(file, "\t\t      else\n");
    fprintf(file, "\t\t      {\n");
    fprintf(file, "\t\t        char *tmp = (char *) malloc(MAX_INPUT_LEN);\n");
    fprintf(file, "\t\t        strcpy(tmp, input[selected_tool]+selectedChar);"
		"\n");
    fprintf(file, "\t\t        input[selected_tool][selectedChar] = c;\n");
    fprintf(file, "\t\t        input[selected_tool][selectedChar+1] = '\\0';"
		"\n");
    fprintf(file, "\t\t        strcat(input[selected_tool], tmp);\n");
    fprintf(file, "\t\t        selectedChar++;\n");
    fprintf(file, "\t\t        locate(tool_pos[selected_tool].x, "
		"tool_pos[selected_tool].y);\n");
    fprintf(file, "\t\t        setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT],"
		" BG_COLOR[COLOR_HIGHLIGHT_TEXT]);\n");
    fprintf(file, "\t\t        printf(\"%%s\", input[selected_tool]);\n");
    fprintf(file, "\t\t        printf(\"%%*s\", "
		"FORM_WIDTH-tool_pos[selected_tool]."
		"x-strlen(input[selected_tool]), \" \");\n");
    fprintf(file, "\t\t        locate(tool_pos[selected_tool].x+selectedChar, "
		"tool_pos[selected_tool].y);\n");
    fprintf(file, "\t\t        free(tmp);\n");
    fprintf(file, "\t\t      }\n");
    fprintf(file, "\t\t    }\n");
    fprintf(file, "\t\t  break;\n");
    fprintf(file, "\t\t}//end switch\n");
    fprintf(file, "\t}//end while\n");
    fprintf(file, "}\n");
    fprintf(file, "\n");
    fprintf(file, "void close_form()\n{\n");
    fprintf(file, "\trestoreTerminal();\n");
    fprintf(file, "\tsetScreenColors(WHITE, BGBLACK);\n");
    fprintf(file, "\tclearScreen();\n");
    fprintf(file, "\treturn;\n");
    /*fprintf(file, "}\n\n");
    fprintf(file, "void editInputField(int index) {\n");
    for(i = 0; i < total_form_tools; i++) {
      if(strcmp(form_tool[i], "Input field") != 0) continue;
      fprintf(file, "input[%d] = (char *) malloc(MAX_INPUT_LEN);\n", i);
      fprintf(file, "strcpy(input[%d], \"\0\");\n", i);
    }*/
    fprintf(file, "}\n");
    fflush(file);
    fclose(file);
  } 
  else 
  {
    msgBox("Error creating file: form_design.c", OK, ERROR);
  }

finished:
  msgBox("Finished writing project files.", OK, INFO);
  drawForm();
  drawToolBox();
  drawStatusBar();
  return;
}
