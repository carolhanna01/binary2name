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
#include <gtk/gtk.h>
#include <stdio.h>
#include <stdlib.h>



MAV_box box;
GtkObject *xadj, *yadj, *zadj;

gint gtkExit(GtkWidget *widget, GdkEventAny *event)
{
  exit(0);
  return TRUE;
}

void xchg(GtkAdjustment *adj)
{
  box.size.x= adj->value;
}

void ychg(GtkAdjustment *adj)
{
  box.size.y= adj->value;
}

void zchg(GtkAdjustment *adj)
{
  box.size.z= adj->value;
}

void gui(GtkWidget *mavarea, char *nm)
{
  GtkWidget *window, *qbut, *vbox, *xslider, *yslider, *zslider;

  /* Main widget container - a vbox */
  vbox= gtk_vbox_new(FALSE, 10);

  /* Make 3 sliders */
  xadj= gtk_adjustment_new(1.0, 0.1, 5.0, 0.1, 1.0, 0);
  gtk_signal_connect(xadj, "value_changed", GTK_SIGNAL_FUNC(xchg), NULL);
  xslider= gtk_hscale_new(GTK_ADJUSTMENT(xadj));

  yadj= gtk_adjustment_new(1.0, 0.1, 5.0, 0.1, 1.0, 0);
  gtk_signal_connect(yadj, "value_changed", GTK_SIGNAL_FUNC(ychg), NULL);
  yslider= gtk_hscale_new(GTK_ADJUSTMENT(yadj));

  zadj= gtk_adjustment_new(1.0, 0.1, 5.0, 0.1, 1.0, 0);
  gtk_signal_connect(zadj, "value_changed", GTK_SIGNAL_FUNC(zchg), NULL);
  zslider= gtk_hscale_new(GTK_ADJUSTMENT(zadj));

  /* Pack sliders into the vbox */
  gtk_box_pack_start(GTK_BOX(vbox), xslider, FALSE, TRUE, 0);
  gtk_box_pack_start(GTK_BOX(vbox), yslider, FALSE, TRUE, 0);
  gtk_box_pack_start(GTK_BOX(vbox), zslider, FALSE, TRUE, 0);

  /* Pack Maverik window into the vbox */
  gtk_box_pack_start(GTK_BOX(vbox), mavarea, FALSE, TRUE, 0);

  /* Make a quit button */
  qbut= gtk_button_new_with_label("Quit");
  gtk_signal_connect(GTK_OBJECT(qbut), "clicked", GTK_SIGNAL_FUNC(gtkExit), NULL);

  /* Pack button into the vbox */
  gtk_box_pack_start(GTK_BOX(vbox), qbut, FALSE, TRUE, 0);

  /* Main window */
  window= gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(window), nm);
  gtk_signal_connect(GTK_OBJECT(window), "delete_event", GTK_SIGNAL_FUNC(gtkExit), NULL);
  gtk_container_set_border_width(GTK_CONTAINER(window), 10);

  /* Add the vbox to the top-level window */
  gtk_container_add(GTK_CONTAINER(window), vbox);
  
  /* Show everything */
  gtk_widget_show(xslider);
  gtk_widget_show(yslider);
  gtk_widget_show(zslider);
  gtk_widget_show(mavarea);
  gtk_widget_show(qbut);
  gtk_widget_show(vbox);
  gtk_widget_show(window);
}



/* Mouse interaction callback */
int hitBox(MAV_object *o, MAV_mouseEvent *me)
{
  if (me->movement==MAV_PRESSED) {
    
    /* Increase size of box */
    box.size.x++;

    /* Update slider */
    gtk_adjustment_set_value(GTK_ADJUSTMENT(xadj), box.size.x);
  }
  
  return 1;
}



/* Define a box */
void defBox(MAV_box *b)
{
  b->size.x= 1.0; /* Specify its size */
  b->size.y= 2.0;
  b->size.z= 3.0;
  b->matrix= MAV_ID_MATRIX; /* Position and orientation */
  b->sp= mav_surfaceParamsNew(MAV_MATERIAL, 0, 1, 0); 

  /* Update sliders */
  gtk_adjustment_set_value(GTK_ADJUSTMENT(xadj), b->size.x);
  gtk_adjustment_set_value(GTK_ADJUSTMENT(yadj), b->size.y);
  gtk_adjustment_set_value(GTK_ADJUSTMENT(zadj), b->size.z);
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

int main(int argc, char *argv[])
{
  MAV_object *obj;
  MAV_SMS *sms;

  /* Define the function to use to create the GUI */
  mav_opt_disp= (char *) gui;

  /* Initialise the Maverik system */
  mav_initialise(&argc, argv);

  /* Define a box object */
  defBox(&box);

  /* Use default mouse navigation */
  mav_navigationMouse(mav_win_all, mav_navigationMouseDefault);

  /* Register the box as a Maverik object */
  obj= mav_objectNew(mav_class_box, &box);

  /* Create a SMS */
  sms= mav_SMSObjListNew();

  /* Add object to SMS */
  mav_SMSObjectAdd(sms, obj);

  /* Mouse interaction callback */
  mav_callbackMouseSet(MAV_MIDDLE_BUTTON, mav_win_all, mav_class_box, hitBox);

  /* Rendering loop */
  while (1) drawFrame(sms);
}
