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


#include "maverik.h"

/* Structure thats used as cylinders userdef variable */

typedef struct {
  char name[100];
} MyStruct;

/* Object SMS and selected object */

extern MAV_SMS *sms;
extern MAV_object *selected;

/* Selected and unselected surface params */

extern MAV_surfaceParams *selectedSp;
extern MAV_surfaceParams *unselectedSp;

/* Lookabout view parameter mode */

void lookabout(MAV_window *w);

/* Callback prototypes */

int select_obj(MAV_object *obj, MAV_mouseEvent *event);
int unselect_obj(MAV_object *obj, MAV_mouseEvent *event);

int keyb(MAV_object *obj, MAV_keyboardEvent *event);

int inc_box(MAV_object *obj, MAV_mouseEvent *event);
int dec_box(MAV_object *obj, MAV_mouseEvent *event);

int inc_pyr(MAV_object *obj, MAV_mouseEvent *event);
int dec_pyr(MAV_object *obj, MAV_mouseEvent *event);

int inc_cyl(MAV_object *obj, MAV_mouseEvent *event);
int dec_cyl(MAV_object *obj, MAV_mouseEvent *event);

int inc_cone(MAV_object *obj, MAV_mouseEvent *event);
int dec_cone(MAV_object *obj, MAV_mouseEvent *event);

int inc_hsph(MAV_object *obj, MAV_mouseEvent *event);
int dec_hsph(MAV_object *obj, MAV_mouseEvent *event);

int inc_ct(MAV_object *obj, MAV_mouseEvent *event);
int dec_ct(MAV_object *obj, MAV_mouseEvent *event);

int inc_rt(MAV_object *obj, MAV_mouseEvent *event);
int dec_rt(MAV_object *obj, MAV_mouseEvent *event);

/* function prototypes */

void defprims(void);
void spinner(void *ignored);
void picker(void *ignored);
void boxer(void *ignored);
