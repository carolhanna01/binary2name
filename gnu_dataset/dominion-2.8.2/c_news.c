/* c_news.c : Curses routines for the news subsystem of Dominion */
/*
 * Copyright (C) 1990 Free Software Foundation, Inc.
 * Written by the dominion project.
 *
 * This file is part of dominion.
 *
 * dominion is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 1, or (at your option)
 * any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <stdio.h>
#include <ctype.h>
#include <unistd.h>
#include <string.h>

#include "dominion.h"
#include "news.h"

extern Suser user;		/* The current user. Used for names. */
extern Sworld world;		/* The current world. Used for the thon. num.*/

extern Sgroup *group_find();	/* In news.c. Searches a database for a 
				 specified group. */

void group_insert(Sgroup **first, Sgroup *temp);

extern char *current_dir,libdir[];

int first_unread(Sarticle **art_arr, Sgroup *g_choice)
{
  int loop,ret;

  ret=g_choice->last;
  if (g_choice->first <= g_choice->last)
    for(loop=g_choice->first;((loop<=g_choice->last)&&(ret==g_choice->last))
	;loop++)
      if (!(art_arr[loop-g_choice->first]->read))
	ret=loop;
  return(ret);
} /* first_unread */


void write_newsrc(Sarticle **art_arr, Sgroup *g_choice)
{
  FILE *nr_fp,*tmp_fp;
  char nr_fn[100],tmp_fn[100];
  char input[300], cmd[100], group_name[100];
  int art_pnt;

  if ((g_choice->first)>(g_choice->last))
    return;
  sprintf(nr_fn,"%s/.newsrc.%d",NEWS_DIR,user.id);
  sprintf(tmp_fn,"%s/.tmprc.%d",NEWS_DIR,user.id);
  nr_fp=fopen(nr_fn,"r");
  tmp_fp=fopen(tmp_fn,"w");
  if (tmp_fp==NULL)
    {
      fprintf(stderr,"Could not open temp. file to save read article list\n");
      fflush(stderr);
      fclose(nr_fp);
      fclose(tmp_fp);
    }
  else
    {
      if (nr_fp!=NULL)
/*	while((fscanf(nr_fp,"%[^\n]",input))!=EOF)*/
	while ((fgets(input,300,nr_fp))!=NULL)
	  {
	    sscanf(input,"%s",group_name);
	    if (strcmp(group_name,g_choice->name))
	      fprintf(tmp_fp,"%s",input);
	  }
      fprintf(tmp_fp,"%s ",g_choice->name);
      art_pnt=g_choice->first;
      while(art_pnt<=g_choice->last)
	{
	  if (!(art_arr[art_pnt-g_choice->first]->read))
	    art_pnt++;
	  else
	    if ((art_pnt<g_choice->last)&&
		(art_arr[art_pnt+1-g_choice->first]->read))
	      {
		fprintf(tmp_fp,"%d-",art_pnt);
		while((art_pnt<g_choice->last)&&
		      (art_arr[art_pnt+1-g_choice->first]->read))
		  art_pnt++;
		fprintf(tmp_fp,"%d ",art_pnt);
		art_pnt++;
	      }
	    else
	      {
		fprintf(tmp_fp,"%d ",art_pnt);
		art_pnt++;
	      }
	} /* while art_pnt <= gchoice->last */
      fprintf(tmp_fp,"\n");
      fclose(tmp_fp);
      fclose(nr_fp);
      sprintf(cmd,"rm -f %s",nr_fn);
      system(cmd);
      sprintf(cmd,"cp %s %s",tmp_fn,nr_fn);
      system(cmd);
      sprintf(cmd,"rm -f %s",tmp_fn);
      system(cmd);
    } /* else */
} /* write_newsrc */

void read_newsrc(Sarticle **art_arr, Sgroup *g_choice)
{
  FILE *nr_fp;
  char nr_fn[100],input[300],group_name[100],*in_ptr;
  int art_num, first_art, last_art;

  sprintf(nr_fn,"%s/.newsrc.%d",NEWS_DIR,user.id);
  nr_fp=fopen(nr_fn,"r");
  if (nr_fp!=NULL)
    {
      while ((fgets(input,300,nr_fp))!=NULL)
	{
	  sscanf(input,"%s",group_name);
	  if (!strcmp(group_name,g_choice->name))
	    {
	      in_ptr=strchr(input,(int)' ');
	      if (in_ptr!=NULL)
		in_ptr++;
	      while (*in_ptr!='\0')
		{
		  sscanf(in_ptr,"%d",&first_art);
		  while (isdigit(*in_ptr))
		    in_ptr++;
		  if (*in_ptr=='-')
		    {
		      sscanf(in_ptr+1,"%d",&last_art);
		      in_ptr=strchr(in_ptr,(int)(' '));
		    }
		  else
		    {
		      last_art=first_art;
		      in_ptr++;
		    }
		  for (art_num=first_art;art_num<=last_art;art_num++)
		    if ((art_num<=g_choice->last)&&(art_num>=g_choice->first))
		      (art_arr[art_num-g_choice->first]->read)=1;
		} /* while *in_ptr */
	    } /* if !strcmp */
	} /* while */
    } /* if nr_fp */
  fclose(nr_fp);
} /* read_newsrc */

void art_menu_bottom(int menu_lines)
{
  mvaddstr(menu_lines + 2, 5,
	   "Position arrow with [j]/[2] (down) and [k]/[8] (up)");
  mvaddstr((menu_lines + 3), 5,
	   "RETURN to Read, Q to Quit    (Use SHIFT to jump by page)");
  mvaddstr(menu_lines + 4, 1,
	   "(*'s indicate previously read - [r]/[u] to mark as read/unread)");
}


/* This function displays a portion (or the whole) of the menu of articles
   that can be read. The menu is displayed according to where the cursor is
   and where it's pointing to. (and of course the size of the screen) */

void show_art_menu(Sarticle **art_arr, int menu_lines, int cur_point,
		   int cur_pos, int first_art, int last_art)
/* art_arr:     an array of pointers to "article structures". See news.h */
/* menu_lines:  how many lines can be displayed on the screen */
/* cur_point:   which article the cursor is pointing to */
/* cur_pos:     where on the screen the cursor is */
/* first_art:   the first article of all the articles that can be read */
/* last_art:    the last article.... */
{
  int line,art_no; /* line: what line on the display we're showing.
		      art_no: What article this is we're showing. */
  char fmt[50],out[200]; /* fmt: How to format the output strings (sprintf).
			    out: The output string for the menu. */

  sprintf(fmt,"%%4d> %%-9.9s  %%-%d.%ds %%-%d.%ds",NAMELEN,
	  NAMELEN,(COLS-23-NAMELEN),(COLS-23-NAMELEN)); 
  /* fmt is how the menu lines look. It's used to handle variable width screens
     4 characters for the article number, a >, 16 characters for the thon,
     NAMELEN characters for the sender, and the rest for the subject. */
  for(line=1;line <= menu_lines;line++)
    {
      art_no=(cur_point-cur_pos)+line; /* Took a while to figure out. I 
					think it's right though. */
      if (art_no <= last_art)	/* Don't display articles past the end.  */
	{
	  sprintf(out,fmt,art_no,art_arr[art_no-first_art]->date,
		  art_arr[art_no-first_art]->sender,
		  art_arr[art_no-first_art]->subject); /* Print to the string*/
	  mvaddstr(line,4,out);	/* Show the string. */
	  if ((art_arr[art_no-first_art]->read)==1)
	    mvaddstr(line,5,"*");
	} /* if (art_no <= last_art */
    } /* for line=1... */
} /* show_art_menu */


/* This function is used for reading news articles. The user has selected a
   group to read (pointed to by g_choice). This displays a menu of the articles
   and allows the user to display articles from the menu.
 */
void news_arts_menu(Sgroup *g_choice, int turn)
    /* g_choice:       This is the group to read from */
    /* turn:           What thon is this? For positioning the cursor. */
{
  char news_dir[100], g_dir[100], a_fn[100];
    /* news_dir: the main news directory. g_dir: this group's directory.
       a_fn: The file name of an article.
     */
  char text[100],type[10];
    /* , date, from: The header of the articles. 
     type what type line of the header? */
  char this_thon[20]; /* What thon it is in string format. */
  FILE *a_fp; /* The file pointer to an article file. */
  int loop;   /* To loop through the articles. */
  char c;     /* The key the user presses. */
  Sarticle **art_arr,*a_temp;	/* art_arr: An array of pointers to
				            'article' structures.
				   a_temp: A work pointer. */
  int menu_lines,menu_center,cur_pos,cur_point,done;
  int show_next = 0;	/* should we see the next article immediately? */
    /* menu_lines: How many lines can be shown.
       menu_center: What line is the center line.
       cur_pos: Where on the screen is the cursor.
       cur_point: What article is it pointing to.
       done: Can we leave news? */
  if((g_choice->first)<=(g_choice->last))
    art_arr=(Sarticle **)malloc(sizeof(Sarticle *)*(g_choice->last-
						      g_choice->first+1));
       /* Allocate space for the array. */
  else
    art_arr=NULL;
  sprintf(this_thon,"Thon %d",turn);  
  sprintf(news_dir,"%s",NEWS_DIR);
  sprintf(g_dir,"%s/%s",news_dir,g_choice->name);
  clear();
  for(loop=g_choice->first;loop<=g_choice->last;loop++)
    {
      a_temp=(Sarticle *)malloc(sizeof(Sarticle));
      a_temp->read=0;
      sprintf(a_fn,"%s/%d",g_dir,loop);
      a_fp=fopen(a_fn,"r");
      if (a_fp!=NULL) {
	strcpy(a_temp->subject,"");
	while (!strcmp(a_temp->subject,"")) {
	  fscanf(a_fp,"%s",type);
	  if (!strcmp(type,"Date:")) {
	    fscanf(a_fp,"%[^\n]",text);
	    strcpy(a_temp->date,text);
	  }
	  if (!strcmp(type,"From:")) {
	    fscanf(a_fp,"%[^\n]",text);
	    strcpy(a_temp->sender,text);
	  }
	  if (!strcmp(type,"Author:")) {
	    fscanf(a_fp,"%[^\n]",text);
	  }
	  if (strcmp(type,"Subj:") == 0 || strcmp(type, "Subject:") == 0) {
	    fscanf(a_fp,"%[^\n]",text);
	    strcpy(a_temp->subject,text);
	  }
	}
	a_temp->art_num=loop;
      } else {
	sprintf(a_temp->date, "%s", "");
	sprintf(a_temp->sender, "  --Article Missing--");
	sprintf(a_temp->subject, "%s", "");
	a_temp->art_num=loop;
      } /* else */
      art_arr[loop-(g_choice->first)]=a_temp;
      if (a_fp != NULL) { fclose(a_fp); } 
    }

  read_newsrc(art_arr,g_choice);
  
  menu_lines=(LINES-6);
  menu_lines=(int)((menu_lines-1)/2);
  menu_lines*=2;
  menu_lines+=1;
  menu_center=(int)((menu_lines+1)/2);
  cur_point=first_unread(art_arr,g_choice);
  cur_pos=cur_point-(g_choice->first-1);
  while (cur_pos>menu_lines)
    cur_pos-=(menu_center);
    
  
  show_art_menu(art_arr,menu_lines,cur_point,cur_pos,g_choice->first,
		g_choice->last);
  art_menu_bottom(menu_lines);
  mvaddstr(cur_pos,1,"===>");
  refresh();
  done=0;
  while (!done)
    {
      switch(c=to_getch()) {
      case 'k':
      case '8':
	if (cur_point > g_choice->first)
	  {
	    mvaddstr(cur_pos,1,"    ");
	    cur_point--;
	    if (cur_pos==1)
	      {
		cur_pos=menu_center;
		clear();
		show_art_menu(art_arr,menu_lines,cur_point,cur_pos,
			      g_choice->first,g_choice->last);
		art_menu_bottom(menu_lines);
	      }
	    else
	      cur_pos--;
	    mvaddstr(cur_pos,1,"===>");
	    refresh();
	  } /* if cur_point > ... */
	break; 
      case 'K':
      case '*':
	if (cur_point > g_choice->first)
	  {
	    mvaddstr(cur_pos,1,"    ");
	    if (cur_pos > menu_center)
	      {
		cur_point-=(cur_pos-menu_center);
		cur_pos=menu_center;
	      }
	    else
	      {
		cur_point-=cur_pos;
		if (cur_point<g_choice->first)
		  {
		    cur_pos=1;
		    cur_point=g_choice->first;
		  }
		else
		  {
		    cur_pos=menu_center;
		    clear();
		    show_art_menu(art_arr,menu_lines,cur_point,cur_pos,
				  g_choice->first, g_choice->last);
		    art_menu_bottom(menu_lines);
		  } /* else */
	      } /* else */
	    mvaddstr(cur_pos,1,"===>");
	    refresh();
	  } /* if cur_point ... */
	break;
      case 'j':
      case '2':
      if (cur_point < g_choice->last)
	{
	  mvaddstr(cur_pos,1,"    ");
	  cur_point++;
	  if (cur_pos==menu_lines)
	    {
	      cur_pos=menu_center;
	      clear();
	      show_art_menu(art_arr,menu_lines,cur_point,cur_pos,
			    g_choice->first, g_choice->last);
	      art_menu_bottom(menu_lines);
	    }
	  else
	    cur_pos++;
	  mvaddstr(cur_pos,1,"===>");
	  refresh();
	} /*  if cur_point... */
      break;
      case 'J':
      case '@':
	if (cur_point < g_choice->last)
	  {
	    mvaddstr(cur_pos,1,"    ");
	    if (cur_pos<menu_center)
	      {
		cur_point+=(menu_center-cur_pos);
		cur_pos=menu_center;
	      }
	    else
	      {
		cur_point+=((menu_lines-cur_pos)+1);
		if(cur_point>g_choice->last)
		  {
		    cur_point=g_choice->last;
		    cur_pos=cur_point-(g_choice->first-1);
		    while(cur_pos>menu_lines)
		      cur_pos-=menu_center;
		  }
		else
		  {
		    cur_pos=menu_center;
		    clear();
		    show_art_menu(art_arr,menu_lines,cur_point,cur_pos,
				  g_choice->first, g_choice->last);
		    art_menu_bottom(menu_lines);
		  } /* else*/
	      } /* else */
	    mvaddstr(cur_pos,1,"===>");
	    refresh();
	  } /* if cur_point... */
	break;
      case 'r':
	if (art_arr!=NULL)
	  {
	    art_arr[cur_point-g_choice->first]->read=1;
	    mvaddstr(cur_pos,5,"*");
	    mvaddstr(cur_pos,5,"");
	  }
	break;
      case 'u':
	if (art_arr!=NULL)
	  {
	    art_arr[cur_point-g_choice->first]->read=0;
	    mvaddstr(cur_pos,5," ");
	    mvaddstr(cur_pos,5,"");
	  }
      	break;
      case '\n':
      case '\r':
      case ' ':
	show_next = 0;
	start_help_win();
	do {
	    /* show the article with more */
/*	  sprintf(cmd,"more %s/%d",g_dir,cur_point);
	  sprintf(cmd,"%s/%d",g_dir,cur_point);
*/
	  sprintf(a_fn, "%s/%d", g_dir, cur_point);
/*	  cleanup(); */
/*	  system(cmd); */
/*	  show_file(a_fn); */
/*	  init_screen(); */
/*	  printf("\nPress RETURN to continue or [n] for the next article\n");
	  fflush(stdout);
*/
	    /* run the pager on the file.  pager() returns 'n'
	       if the user types n at the end of an article
	     */
	  if ((c = pager(a_fn)) != 'n'){ /* run the pager on this file */
	    statline("RETURN or [q] for article menu; [n] for next article"
		     , "");
	    c = to_getch();
	  }

	  if (art_arr!=NULL)
	    art_arr[cur_point-g_choice->first]->read=1;

	  if (c == 'n') {
	    show_next = 1;
	  } else {
	    show_next = 0;
	  }
	  if(cur_point<g_choice->last) {
	    cur_point++;
	    cur_pos++;
	    if (cur_pos>=menu_lines)
 	      cur_pos-=menu_center;
	  } /* if cur_point < */
	} while (show_next);
	end_help_win();
	clear();
	show_art_menu(art_arr,menu_lines,cur_point,cur_pos,
		      g_choice->first,g_choice->last);
	art_menu_bottom(menu_lines);
	mvaddstr(cur_pos,1,"===>");
	refresh();
	break;
      case 'Q':
      case 'q':
	done=1;
	break;
      } /* switch */
    }   /* while */
  /* Write out "newsrc" data for this group */
  write_newsrc(art_arr,g_choice);
  /* Clean up memory */
  for (loop=g_choice->first;loop<=g_choice->last;loop++)
    free(art_arr[loop-(g_choice->first)]);
  free(art_arr);

  clear();
} /* news_arts_menu */

Sgroup *news_groups_menu(int human, int reading)
    /* human:               a flag to say if a human is posting. */
    /* reading:             a flag to say if we're only reading. */
{
  char news_dir[100],g_fn[100];
  FILE *g_fp;
  char g_name_in[100],post_in;
  int first_in,last_in,g_index,total_groups,cur_pos,done,loop;
  char s_in[100],c;
  Sgroup *g_first, *g_temp, *g_ret;

  g_first=NULL;
  g_temp=NULL;
  total_groups=0;

  sprintf(news_dir,"%s",NEWS_DIR);
  sprintf(g_fn,"%s/%s",news_dir,NGDB_FILE);
  g_fp=fopen(g_fn,"r");
  if (g_fp!=NULL)
/*    while ((fscanf(g_fp,"%[^:]: %d %d %c",g_name_in,&first_in,&last_in,
		   &post_in))>0) */
    while ((fgets(s_in,100,g_fp))!=NULL)
      {
	s_in[strlen(s_in)-1] = '\0';
	sscanf(s_in,"%s %d %d %c",g_name_in,&first_in,&last_in,&post_in);
	g_temp = (Sgroup *) malloc(sizeof(Sgroup));
	sprintf((g_temp)->name,"%s",g_name_in);
	(g_temp)->first=first_in;
	(g_temp)->last=last_in;
	(g_temp)->postable=post_in;
	(g_temp)->next=NULL;
	if ((human==0)||(g_temp->postable=='1')) {
	  if (!reading||((g_temp->first)<=(g_temp->last))) {
	    group_insert(&g_first, g_temp);
	    total_groups++;
	  }
	} else {
	  free(g_temp);
	}
      }
  if (g_fp != NULL) { fclose(g_fp); } 
  clear();
  if (g_first==NULL)
    {
      fprintf(stderr,"Problem getting group database\n");
      return(NULL);
    }
  statline("choose the group you want (RETURN to exit)","news_group_choices");
  statline2("*'s indicate articles exist","");
  g_index=0;
  for ((g_temp)=(g_first);(g_temp)!=NULL;(g_temp)=(g_temp)->next)
    {
      if ((g_temp)->last >= (g_temp)->first)
	mvaddstr((g_index)+2,6,"*");
      mvaddstr((g_index++)+2,7,(g_temp)->name);
    }
  mvaddstr(LINES-4,5,"Position arrow with [j]/[2] (down) and [k]/[8] (up)");
  mvaddstr(LINES-3,5,"RETURN to Read, Q to Quit");
  cur_pos=1;
  mvaddstr(cur_pos+1,1,"===>");
  refresh();
  done=0;
  g_temp=NULL;
/*  if (total_groups==1)
    {
      done=1;
      g_temp=g_first;
    } */
  while (!done)
    {
      switch(c=to_getch()) {
      case 'k':
      case '8':
	if (cur_pos>1)
	  {
	    mvaddstr(cur_pos+1,1,"    ");
	    cur_pos--;
	    mvaddstr(cur_pos+1,1,"===>");
	    refresh();
	  }
	break;
      case 'j':
      case '2':
	if (cur_pos<total_groups)
	  {
	    mvaddstr(cur_pos+1,1,"    ");
	    cur_pos++;
	    mvaddstr(cur_pos+1,1,"===>");
	    refresh();
	  }
	break;
      case '\n':
      case '\r':
      case ' ':
	g_temp=g_first;
	if (cur_pos>1)
	  for(loop=2;loop<=cur_pos;loop++)
	    g_temp=g_temp->next;
	done=1;
	break;
      case 'q':
      case 'Q':
	done = 1;
	break;
      } /* switch */
    } /* while */
  
/*  ret = wget_name(stdscr, g_name_in);
  if (ret > 0)
    {
      g_temp=group_find(g_first,g_name_in);
      if (g_temp==NULL)
	{
	  statline("Bad Group Name - Space to Return","Bad Group Name");
	  get_space();
	}
    }
  clear();*/
/* Clean up allocated space */
  g_ret=g_temp;
  for(g_temp=g_first;g_temp!=NULL;g_temp=g_first)
    {
      if (g_temp!=g_ret)
	{
	  g_first=g_temp->next;
	  free(g_temp);
	}
      else
	{
	  g_first=g_temp->next;
	  g_ret->next=NULL;
	}
    }
  return(g_ret);
} /* news_groups_menu */

void news()
{
  int not_done;
  char tmp_fname[PATHLEN];
  char date[80];
  char from[80];
  char subj[80];
  Sgroup *g_temp;
  statline("do you want to (r)ead news or (p)ost news","news");
  switch (to_getch()) {
  case 'r':
    g_temp=news_groups_menu(0,1); /* 0 = not human(n/a), 1 = reading */
/*    if (g_temp!=NULL)
      news_arts_menu(g_temp,world.turn);*/
    while(g_temp!=NULL)
      {
	news_arts_menu(g_temp,world.turn);
	g_temp=news_groups_menu(0,1); /* 0 = not human(n/a), 1=reading */
      }
    clear(); refresh();
    break;
  case 'p':
    sprintf(date,"Thon %d",world.turn);
    sprintf(from,"%s of %s",user.np->leader,user.np->name);
    clear();
    g_temp=news_groups_menu(1,0); /* 1=human, 0=posting(!reading) */
    clear();
    if(g_temp!=NULL) {
      int fd;
      strcpy(tmp_fname, "/usr/tmp/domXXXXXX");
      if ((fd = mkstemp(tmp_fname)) == -1) {
        fprintf(stderr,"Error getting temp file name\n");
        fflush(stderr);
        return;
      }
      close(fd);
      statline("Enter Subject","Posting News");
      mvaddstr(2,1,"Subject: ");
      wget_name(stdscr, subj);
/*	echo();
	nocbreak();
	refresh();
	scanw("%[^\n]",subj);
	noecho();
	cbreak();
	refresh();
*/
      cleanup();
      chdir("/usr/tmp");
      edit(tmp_fname);
      chdir(current_dir);
      chdir(libdir);
      {
        refresh();
/*	  init_screen(); */
/*	  initscr();
	  savetty();
	  nonl();
	  cbreak();
	  noecho();
	  clear();
*/
      }
      mvaddstr(2,0,"Choices: S)end news or A)bort posting ");
      refresh();
      not_done=1;
      while(not_done) {
        switch(to_getch()) {
        case 'S':
        case 's':
          mvaddstr(3,9,"Posting News...");
          refresh();
          post_news_file(tmp_fname, g_temp->name, subj, user.id);
          mvaddstr(3,24,"done.");
          refresh();
          not_done=0;
          unlink(tmp_fname);
          break;
        case 'A':
          mvaddstr(3,23,"OK. Aborting...");
          refresh();
          unlink(tmp_fname);
          not_done=0;
          break;
        } /* switch */
      } /* while */
      {
/*	  cleanup(); */
        erase();
        touchwin(stdscr);
        refresh();
        
/*	  init_screen(); */
      }
    } /* if g_temp!=NULL */
    break;
  } /* switch */
  user.just_moved = 1;
} /* news */
