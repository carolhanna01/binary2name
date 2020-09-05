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
#include "forms.h"

/* Define a box */
void defBox(MAV_box *b)
{
  b->size.x= 1.0; /* Specify its size */
  b->size.y= 2.0;
  b->size.z= 3.0;
  b->matrix= MAV_ID_MATRIX; /* Position and orientation */
  b->sp= mav_sp_default;    /* Surface parameters, i.e. colour */
}

/* Render a frame */
void drawFrame(MAV_SMS *sms)
{
  /* Check for and act on any events */
  mav_eventsCheck();
    
  /* Request start of a new frame */
  mav_frameBegin();
    
  /* Display the SMS in all windows */
  mav_SMSDisplay(mav_win_all, sms);

  /* Request end of the frame */
  mav_frameEnd();
}


FL_FORM *form;
FL_OBJECT *xsz, *ysz, *zsz, *but;
MAV_box box;

/* xforms callbacks - set appropriate box size to slider value */
void cbx(FL_OBJECT *o, long d)
{
  box.size.x= fl_get_slider_value(xsz);
}

void cby(FL_OBJECT *o, long d)
{
  box.size.y= fl_get_slider_value(ysz);
}

void cbz(FL_OBJECT *o, long d)
{
  box.size.z= fl_get_slider_value(zsz);
}

void cbb(FL_OBJECT *o, long d)
{
  exit(1);
}

/* Create form and set callbacks */
void createForm(void)
{
  form = fl_bgn_form(FL_UP_BOX, 200,200);

  xsz = fl_add_valslider(FL_HOR_NICE_SLIDER, 10, 10, 180, 30,"X size");
  ysz = fl_add_valslider(FL_HOR_NICE_SLIDER, 10, 60, 180, 30,"Y size");
  zsz = fl_add_valslider(FL_HOR_NICE_SLIDER, 10, 110, 180, 30,"Z size");
  but = fl_add_button(FL_NORMAL_BUTTON, 60, 160, 80, 30,"Exit");

  fl_set_slider_bounds(xsz, 0.1, 5.0);
  fl_set_slider_bounds(ysz, 0.1, 5.0);
  fl_set_slider_bounds(zsz, 0.1, 5.0);

  fl_set_object_callback(xsz, cbx, 0);
  fl_set_object_callback(ysz, cby, 0);
  fl_set_object_callback(zsz, cbz, 0);
  fl_set_object_callback(but, cbb, 0);

  fl_end_form();
}

/* Maverik callback */
void sldUpd(void)
{
  fl_set_slider_value(xsz, box.size.x);
  fl_set_slider_value(ysz, box.size.y);
  fl_set_slider_value(zsz, box.size.z);
}  

int mouseEvent(MAV_object *o, MAV_mouseEvent *me)
{
  if (me->movement==MAV_PRESSED && box.size.x<5.0) {
    box.size.x+=0.1; /* Increase box's X size */
    sldUpd(); /* Update slider value */
  }

  return 1;
}



int main(int argc, char *argv[])
{
  MAV_object *obj;
  MAV_SMS *sms;

  /* Initialise the Maverik system */
  mav_initialise(&argc, argv);

  /* Initialise xforms */
  fl_initialize(&argc, argv, "xform demo", 0, 0);

  /* Create and place xforms */
  createForm();
  fl_show_form(form, FL_PLACE_MOUSE, FL_FULLBORDER,"xform demo");

  /* Define a box object */
  defBox(&box);

  /* Register the box as a Maverik object */
  obj= mav_objectNew(mav_class_box, &box);

  /* Create a SMS */
  sms= mav_SMSObjListNew();

  /* Add object to SMS */
  mav_SMSObjectAdd(sms, obj);

  /* Use default mouse navigation */
  mav_navigationMouse(mav_win_all, mav_navigationMouseDefault);

  /* Set mouse event callback */
  mav_callbackMouseSet(MAV_MIDDLE_BUTTON, mav_win_all, mav_class_box, mouseEvent);

  /* Update slider values */
  sldUpd();

  /* Check xforms and render loop */
  while (1) {fl_check_forms(); drawFrame(sms);}
}
