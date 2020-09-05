/*
   GNU Maverik - a system for managing display and interaction in 
              Virtual Environment applications.
   Copyright (C) 2008  Advanced Interfaces Group

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.

   The authors can be contacted via:
   www   - http://aig.cs.man.ac.uk
   email - maverik@aig.cs.man.ac.uk
   mail  - Advanced Interfaces Group, Room 2.94, Kilburn Building, 
        University of Manchester, Manchester, M13 9PL, UK
*/


#include "mavlib_kernel.h"

int mavlib_callbackNum=0;
int mavlib_SMSCallbackNum=0;



/* Routine to define a new callback class */

MAV_callback *mav_callbackNew(void)
{
  MAV_callback *c= (MAV_callback *) mav_malloc(sizeof(MAV_callback));

  c->num= mavlib_callbackNum;
  mavlib_callbackNum++;

  return c;
}



/* Routine to set a callback class */

void mav_callbackSet(MAV_callback *b, MAV_window *w, MAV_class *c, MAV_callbackFn fn)
{
  c->fn[b->num][w->id]= fn;
}



/* Routine to execute a callback class */

int mav_callbackExec(MAV_callback *b, MAV_window *w, MAV_object *o, void *d1, void *d2)
{
  int rv=MAV_FALSE;
  
  /* First check win all, class all */
  if (mav_class_all->fn[b->num][mav_win_all->id]) {
    rv= (*(mav_class_all->fn[b->num][mav_win_all->id])) (o,d1,d2);
  } 
  /* Next is win w, class all */ 
  else if (mav_class_all->fn[b->num][w->id]) {
    rv= (*(mav_class_all->fn[b->num][w->id])) (o,d1,d2);
  }
  /* Then check win all, class of obj */
  else if (o->the_class->fn[b->num][mav_win_all->id]) {
    rv= (*(o->the_class->fn[b->num][mav_win_all->id])) (o,d1,d2);
  }
  /* finally is win w, class of obj */
  else if (o->the_class->fn[b->num][w->id]) {
    rv= (*(o->the_class->fn[b->num][w->id])) (o,d1,d2);
  }
  
  return rv;
}



/* Routine to query if a callback class is defined */

MAV_callbackFn mav_callbackQuery(MAV_callback *b, MAV_window *w, MAV_object *o)
{
  MAV_callbackFn rv= 0;

  /* First check win all, class all */
  if (mav_class_all->fn[b->num][mav_win_all->id]) {
    rv= mav_class_all->fn[b->num][mav_win_all->id];
  } 
  /* Then check win all, class of obj */
  else if (o->the_class->fn[b->num][mav_win_all->id]) {
    rv= o->the_class->fn[b->num][mav_win_all->id];
  }
  /* Next is win w, class all */ 
  else if (mav_class_all->fn[b->num][w->id]) {
    rv= mav_class_all->fn[b->num][w->id];
  }
  /* finally is win w, class of obj */
  else if (o->the_class->fn[b->num][w->id]) {
    rv= o->the_class->fn[b->num][w->id];
  }

  return rv;
}




/* As above but for SMS callback classes */

MAV_SMSCallback *mav_SMSCallbackNew(void)
{
  MAV_SMSCallback *c= (MAV_SMSCallback *) mav_malloc(sizeof(MAV_SMSCallback));

  c->num= mavlib_SMSCallbackNum;
  mavlib_SMSCallbackNum++;

  return c;
}

void mav_SMSCallbackSet(MAV_SMSCallback *b, MAV_SMSClass *c, MAV_SMSCallbackFn fn)
{
  c->fn[b->num]= fn;
}

int mav_SMSCallbackExec(MAV_SMSCallback *b, MAV_SMS *s, void *d1, void *d2, void *d3, void *d4)
{
  int rv=MAV_FALSE;
  
  if (s->the_class->fn[b->num]) {
    rv= (*(s->the_class->fn[b->num])) (s, d1, d2, d3, d4);
  }
  
  return rv;
}

MAV_SMSCallbackFn mav_SMSCallbackQuery(MAV_SMSCallback *b, MAV_SMS *s)
{
  MAV_SMSCallbackFn rv= 0;

  if (s->the_class->fn[b->num]) rv= s->the_class->fn[b->num];

  return rv;
}
