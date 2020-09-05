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

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "maverik.h"
#include "city_macros.h"
#include "city_types.h"

#ifdef COMPOSITES

extern int num_polys;
extern MAV_composite composites[NUM_COMP];
extern MAV_list *list_of_objects;

void
Add_Composite
(MAV_cityCell *c, int type, MAV_matrix *transform)
{
  MAV_composite *comp;

  comp= mav_malloc(sizeof(MAV_composite));

  *comp= composites[type];
  comp->matrix= *transform;

  mav_listItemAdd (c->composites, (void *)mav_objectNew(mav_class_composite, comp));
}

void
Add_Feature
(MAV_cityCell *c, int type, MAV_matrix *transform)
{
  MAV_composite *comp;

  comp= mav_malloc(sizeof(MAV_composite));

  *comp= composites[type];
  comp->matrix= *transform;

  mav_listItemAdd (c->features, (void *)mav_objectNew(mav_class_composite, comp));
}

MAV_object*
Add_Composite_To_List
(MAV_SMS *list, int type, MAV_matrix *transform)
{
  MAV_composite *comp;
  MAV_object *obj;

  comp= mav_malloc(sizeof(MAV_composite));

  *comp= composites[type];
  comp->matrix= *transform;

  obj= mav_objectNew(mav_class_composite, comp);
  mav_SMSObjectAdd (list, obj);

  return obj;
}

#endif

