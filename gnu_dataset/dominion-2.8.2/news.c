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
#ifdef AMIGA
#  include <exec/types.h>
#  define sleep(x) Delay(x*50)
#else
#  include <sys/types.h>
#endif
#include <time.h>
#include <unistd.h>
#include <string.h>

#include "dominion.h"
#include "news.h"

extern Sworld world;
extern Suser user;

char *news_post()
{
    FILE *file;
    static char buffer[1024];
    char *ptr;

    if ((file = fopen(NEWS_POST_FILE, "r"))) {
	fgets(buffer, sizeof(buffer), file);
	fclose(file);
	ptr = buffer + strlen(buffer) - 1;
	if (*ptr == '\n')
	    *ptr = '\0';
	return buffer;
    }
    else
	return NULL;
}

int valid_group(name,posting)
char *name;
int posting;
{
  char news_dir[100],g_fn[100],in[100];
  char post_in;
  FILE *g_fp;
  int ret = 0;
  char s_in[100];

  sprintf(news_dir,"%s",NEWS_DIR);
  sprintf(g_fn,"%s/%s",news_dir,NGDB_FILE);
  
  if ((g_fp=fopen(g_fn,"r")) == NULL) {
    return 0;
  }
  while((fgets(s_in,100,g_fp))!=NULL) {
    s_in[strlen(s_in)-1] = '\0';
    sscanf(s_in,"%s %*d %*d %c",in,&post_in);
    if(!posting && !strcmp(in,name)) {
      ret=1;
    } else {
      if((posting)&&(!strcmp(in,name))&&(post_in=='1')) {
	ret=1;
      }
    }
  }
  fclose(g_fp);
  return(ret);
} /* valid_group */

Sgroup *group_find(Sgroup *first, char *name)
{
  Sgroup *loop, *ret;
  ret=NULL;
  for (loop=first;loop!=NULL;loop=loop->next)
    if (!strcmp(name,loop->name))
      ret=loop;
  return(ret);
} /* group_find */
      
/* This checks if someone else is adding an article (ie it's locked) */

int check_news_lock()
{
  FILE *lock_fp;
  char lock_fn[100];
  char news_dir[100];
  int ret;

  sprintf(news_dir,"%s",NEWS_DIR);
  sprintf(lock_fn,"%s/%s.lock",news_dir,NGDB_FILE);
  if ((lock_fp=fopen(lock_fn,"r"))!=NULL) {
    ret=0;
    fclose(lock_fp);
  } else {
    ret=1;
  }
  return(ret);
}   /* check_news_lock */

void lock_news(msg)
     char *msg;
{
  FILE *lock_fp;
  char news_dir[100],lock_fn[100];

  sprintf(news_dir,"%s",NEWS_DIR);
  sprintf(lock_fn,"%s/%s.lock",news_dir,NGDB_FILE);
  if ((lock_fp=fopen(lock_fn,"w"))!=NULL) {
    fprintf(lock_fp,"locked - %s\n",msg);
    fclose(lock_fp);
  }
}  /* lock_news */

void unlock_news()
{
  char news_dir[100],lock_fn[100];
  sprintf(news_dir,"%s",NEWS_DIR);
  sprintf(lock_fn,"%s/%s.lock",news_dir,NGDB_FILE);
  unlink(lock_fn);
}  /* unlock_news */

void group_insert(Sgroup **first, Sgroup *temp)
{
  Sgroup *loop;
  
  if (*first==NULL)
    {
      *first=temp;
    }
  else
    {
      for (loop=(*first);loop->next!=NULL;loop=loop->next);
      loop->next=temp;
    }
} /* group_insert */

void post_news_file(char *news_file, char *group_name, char *subject, int id)
{
  FILE *g_fp;
  char news_dir[100],g_fn[100],g_dir[100],a_fn[100];
  Sgroup *g_first,*g_temp;
  char g_name_in[100],post_in;
  int first_in,last_in;
  int news_locked;
  char cmd[1000];
  char s_in[100];
  FILE *a_fp;
  char *forward;

  g_first=NULL;

  sprintf(news_dir,"%s",NEWS_DIR);
  sprintf(g_fn,"%s/%s",news_dir,NGDB_FILE);
  sprintf(g_dir,"%s/%s",news_dir,group_name);
  if (!(news_locked=check_news_lock()))
    {
      sleep(2);
      news_locked=check_news_lock();
    }
  if (news_locked==1)
    {
      lock_news(group_name);
      if ((g_fp=fopen(g_fn,"r")) == NULL) {
	fprintf(stderr,"\nCould not open group file '%s' for reading.\n",g_fn);
	fprintf(stderr,"The gamemaster should check this out.\n");
	fprintf(stderr," (Your post has been aborted... Sorry)\n");
	sleep(2);
      } else {
	while ((fgets(s_in,100,g_fp))!=NULL) {
	  s_in[strlen(s_in)-1] = '\0';
	  sscanf(s_in,"%s %d %d %c",g_name_in,&first_in,&last_in,&post_in);
	  g_temp=(Sgroup *)malloc(sizeof(Sgroup));
	  strcpy(g_temp->name,g_name_in);
	  g_temp->first=first_in;
	  g_temp->last=last_in;
	  g_temp->postable=post_in;
	  g_temp->next=NULL;
	  group_insert(&g_first,g_temp);
	}
	fclose(g_fp);
      }
      g_temp=group_find(g_first,group_name);
      if (g_temp==NULL) {
	fprintf(stderr,"Bad group name: %s\n",group_name);
	fprintf(stderr,"Post aborted\n");
	sleep(1);
      } else {
	sprintf(a_fn,"%s/%d",g_dir,g_temp->last+1);
	a_fp=fopen(a_fn,"w");
	if (a_fp==NULL) {
	  fprintf(stderr,"Error Opening New Article File Pointer\n");
	  fprintf(stderr,"Aborting news post!\n");
	  sleep(2);
	}
	else
	  {
	    fprintf(a_fp,"Date: Thon %d\n",world.turn);
	    if (id==0) {
	      fprintf(a_fp,"From: Update\n");
	    } else {
	      fprintf(a_fp,"From: %s\n",world.nations[id].name);
	      fprintf(a_fp,"Author: %s of %s\n",world.nations[id].leader,
		      world.nations[id].name);
	    }
	    fprintf(a_fp,"Subject: %s\n\n",subject);
	    insert_mail(news_file, a_fp);
	    if (a_fp != NULL) { fclose(a_fp); }
	    (g_temp->last)++;
	  }
	if ( (forward = news_post()) != NULL) {
	  sprintf(cmd, "cat %s | %s '%s'", a_fn, MAILER, forward);
	  system(cmd);
	}
        post_news_to_users(a_fn);
	if ((g_fp = fopen(g_fn,"w")) != NULL) {
	  for (g_temp=g_first;g_temp!=NULL;) {
	    fprintf(g_fp,"%s %d %d %c\n",g_temp->name,g_temp->first,
		    g_temp->last,g_temp->postable);
	    g_first=g_temp->next;
	    free(g_temp);
	    g_temp=g_first;
	  }
	  fclose(g_fp);
	}
      } /* if (g_temp==NULL) .. else */
      unlock_news();
    } /* News_locked==1 */
  else
    {
      fprintf(stderr,"News is being locked by someone.\n");
      fprintf(stderr,"Posting is being aborted...\n");
      sleep(2);
    }
  unlink(news_file);
} /* Post_news_file */

/* Forward the news to the users who want news forwarded */
void post_news_to_users(char *filename)
{
  int i;
  char cmd[1000], *forward;

  for (i = 0; i < world.n_nations; i++) {
    if ((forward = world.nations[i].opts->news_forward)) {
      sprintf(cmd, "cat %s | %s '%s'", filename, MAILER, forward);
      system(cmd);
    }
  }
}

