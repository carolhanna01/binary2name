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
#include <stdlib.h>
#include <stdio.h>

MAV_list **mavlib_table_list;


int mav_opt_objectTableSize= MAVLIB_OBJECT_TABLE_SIZE;
int mavlib_objectTableSize;

/* Routines to get the two parts of a object */

void *mav_objectDataGet(MAV_object *o)
{
  return o->the_data;
}

MAV_class *mav_objectClassGet(MAV_object *o)
{
  return o->the_class;
}


int mavlib_isPrime(int num)
{
  int tester= 2;

  while (1) {
    if (num%tester==0) return 0;
    if (tester*tester>num) return 1;

    tester ++;
  }
}


void mavlib_setUpObjectTables (void) 
{
  /* set up object look-up tables that contain all lists and SMS's that */
  /* contain each object */

  int count;

  /* get table size */
  mavlib_objectTableSize= mav_opt_objectTableSize;

  if (mavlib_objectTableSize < 1) mavlib_objectTableSize= 1;

  while (!mavlib_isPrime (mavlib_objectTableSize))
    mavlib_objectTableSize --;

  if (mav_opt_output==MAV_VERBOSE && mavlib_objectTableSize!=MAVLIB_OBJECT_TABLE_SIZE) fprintf (stderr, "Object table size: %d\n", mavlib_objectTableSize);

  /* get memory for table */
  mavlib_table_list= (MAV_list **) mav_malloc(mavlib_objectTableSize*sizeof(MAV_list *));

  /* reset entries */
  for (count= 0; count < mavlib_objectTableSize; count ++)
    mavlib_table_list[count]= mav_listNew ();
}



MAVLIB_objectTableEntry *mavlib_objectGetEntryFromObject (MAV_object *the_obj) 
{
/* get the MAVLIB_objectTableEntry for 'the_obj' --- if it doesn't have one */
/* then create one for it */
  int obj_index;
  void *current;
  MAVLIB_objectTableEntry *the_entry=NULL;
  int not_found= MAV_TRUE;

  /* get index to table array */
  obj_index= (abs((int)(the_obj->the_data)))%(mavlib_objectTableSize);

  /* get list info */
  mav_listPointerReset (mavlib_table_list[obj_index]);

  /* search for this object's entry */
  while ((not_found)&&(mav_listItemNext (mavlib_table_list[obj_index], &current))) {
    the_entry= (MAVLIB_objectTableEntry *) current;

    if (the_entry->the_obj == the_obj)
      /* found it */
      not_found= MAV_FALSE;
  }

  if (not_found) {
    /* create a new entry */
    the_entry= (MAVLIB_objectTableEntry *) mav_malloc (sizeof(MAVLIB_objectTableEntry));
    the_entry->the_obj= the_obj;
    the_entry->SMS_list= mav_listNew ();

    /* add entry to table */
    mav_listItemAdd (mavlib_table_list[obj_index], the_entry);
  }

  return the_entry;
}



MAVLIB_objectTableEntry *mavlib_objectGetEntryFromData (void *the_data) 
{
  /*  get the MAVLIB_objectTableEntry for 'the_data' or return NULL if it */
  /* doesn't have one */
  int obj_index;
  void *current;
  MAVLIB_objectTableEntry *the_entry=NULL;
  int not_found= MAV_TRUE;

  /* get index */
  obj_index= (abs((int)the_data))%(mavlib_objectTableSize);

  mav_listPointerReset (mavlib_table_list[obj_index]);
  
  while ((not_found)&&(mav_listItemNext (mavlib_table_list[obj_index], &current))) {
    the_entry= (MAVLIB_objectTableEntry *) current;

    if (the_entry->the_obj->the_data == the_data)
      /* found it */
      not_found= MAV_FALSE;
  }

  if (not_found)
    return NULL;
  else
    return the_entry;
}



/* Routine to returns the MAV_object that contains 'the_data' as
   its data thingy bit or 'NULL' if no entry is found */

MAV_object *mav_objectDataWith (void *the_data) 
{
  MAVLIB_objectTableEntry *obj_entry;

  if (mavlib_table_list) {
    obj_entry= mavlib_objectGetEntryFromData (the_data);

    /* check it exists */
    if (obj_entry != NULL)
      return obj_entry->the_obj;
    else
      return NULL;
  }
  else
    return NULL;
}



/* Routine to create a new object */

MAV_object *mav_objectNew(MAV_class *the_class, void *the_data)
{
  MAV_object *obj= (MAV_object *) mav_malloc(sizeof(MAV_object));

  /* set class and data */
  obj->the_class= the_class;
  obj->the_data= the_data;

  if (mav_opt_objectTables) {
    /* set object look-up table entry */
    mavlib_objectGetEntryFromObject(obj);

    /* add to list of all objects */
    mav_listItemAdd(mav_object_list, obj);
  }

  return obj;
}



void mavlib_objectRemoveEntry (MAVLIB_objectTableEntry *the_entry) 
{
  /* delete an entry from the table */
  int obj_index;

  /* get this entry's object's index (hmmm) */
  obj_index= (abs((int)(the_entry->the_obj->the_data)))%(mavlib_objectTableSize);

  /* remove entry from the table */
  mav_listItemRmv (mavlib_table_list[obj_index], (void *) the_entry);
}



void mavlib_objectTableDelete (MAV_object *the_obj) 
{
  /* remove 'the_obj' from all lists and SMS's listed in its object table */
  /* entry */
  MAVLIB_objectTableEntry *obj_entry;
  void *current;
  MAV_SMS *the_sms;

  /* get entry */
  obj_entry= mavlib_objectGetEntryFromData (the_obj->the_data);

  if (obj_entry != NULL) {
    /* now we can turn off object tables (otherwise the lists will */
    /* be updated whilst we are processing them) */
    mav_opt_objectTables= MAV_FALSE;

    /* loop through SMSs */
    mav_listPointerReset (obj_entry->SMS_list);

    while (mav_listItemNext (obj_entry->SMS_list, &current)) {
      the_sms= (MAV_SMS *) current;

      /* remove 'the_obj' from this SMS */
      mav_SMSCallbackObjectRmvExec(the_sms, the_obj);
    }
    
    /* free list list memory */
    mav_listDelete (obj_entry->SMS_list);

    /* turn table lists back on */
    mav_opt_objectTables= MAV_TRUE;

    /* remove entry from the table */
    mavlib_objectRemoveEntry (obj_entry);

    /* delete the entry */
    mav_free(obj_entry);
  }
}



/* Routine to delete an object */

void mav_objectDelete(MAV_object *mo) 
{
  if (mav_opt_objectTables) {
    /* update object tables */
    mavlib_objectTableDelete(mo);

    /* remove from list of all objects */
    mav_listItemRmv(mav_object_list, mo);
  }

  /* do application delete function */
  mav_callbackDeleteExec(mav_win_all, mo);

  /* free memory */
  mav_free(mo);
}



/* Routines to add and remove an SMS to 'the_obj's table entry */

void mav_objectTablesSMSAdd (MAV_object *the_obj, MAV_SMS *the_sms) 
{
  MAVLIB_objectTableEntry *obj_entry;

  /* get the table entry */
  obj_entry= mavlib_objectGetEntryFromObject (the_obj);

  /* add the SMS */
  mav_listItemAdd (obj_entry->SMS_list, (void *) the_sms);
}

void mav_objectTablesSMSRmv (MAV_object *the_obj, MAV_SMS *the_sms) 
{
  MAVLIB_objectTableEntry *obj_entry;

  /* get the table entry */
  obj_entry= mavlib_objectGetEntryFromData (the_obj->the_data);

  /* remove the list */
  if (obj_entry != NULL)
    mav_listItemRmv (obj_entry->SMS_list, (void *) the_sms);
}

MAV_list *mav_objectSMSsGet(MAV_object *the_obj)
{
  MAVLIB_objectTableEntry *obj_entry;
  MAV_list *rv= NULL;

  /* get the table entry */
  obj_entry= mavlib_objectGetEntryFromData (the_obj->the_data);

  /* find the list of SMSs which this object is in */
  if (obj_entry) rv=obj_entry->SMS_list;

  return rv;
}
