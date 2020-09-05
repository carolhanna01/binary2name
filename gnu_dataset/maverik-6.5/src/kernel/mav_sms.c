/*
    GNU Maverik - a system for managing display and interaction in 
               Virtual Environment applications.
    Copyright (C) 2008  <name of author>

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
#include <stdlib.h>



/* Routines to get the two parts of an SMS */

void *mav_SMSDataGet(MAV_SMS *sms)
{
  return sms->the_data;
}

MAV_SMSClass *mav_SMSClassGet(MAV_SMS *sms)
{
  return sms->the_class;
}



/* Routine to create an SMS */

MAV_SMS *mav_SMSNew(MAV_SMSClass *the_class, void *the_data)
{  
  MAV_SMS *newsms= (MAV_SMS *) mav_malloc(sizeof(MAV_SMS));
  int i;

  newsms->the_class= the_class;
  newsms->the_data= the_data;

  /* By default, SMS's are selectable in mav_win_all but not each individual window */
  newsms->selectable[mav_win_all->id]= MAV_TRUE;  
  for (i=1; i<MAV_MAX_WIN; i++) newsms->selectable[i]= MAV_FALSE;

  /* keep a list of all SMS's */
  mav_listItemAdd(mav_sms_list, (void *) newsms);

  return newsms;
}



/* Routine to delete an SMS */

void mav_SMSDelete(MAV_SMS *s, int o)
{
  /* call delete callback */
  mav_SMSCallbackDeleteExec(s, o);

  /* remove from list of all SMS's */
  mav_listItemRmv(mav_sms_list, (void *) s);
  
  /* free the memory */
  mav_free(s);
}



/* Routine to set SMS's selectability. */

void mav_SMSSelectabilitySet(MAV_SMS *s, MAV_window *w, int v)
{
  s->selectable[w->id]= v;
}
