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
#include <qslider.h>
#include <qapplication.h>
#include <qvbox.h>
#include "qt_moc.h"
#include <stdlib.h>

MAV_box box;
QSlider *xs, *ys, *zs;

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

/* Define a box */
void defBox(MAV_box *b)
{
  b->size.x= 1.0; /* Specify its size */
  b->size.y= 2.0;
  b->size.z= 3.0;
  b->matrix= MAV_ID_MATRIX; /* Position and orientation */
  b->sp= mav_surfaceParamsNew(MAV_MATERIAL, 0, 1, 0); 

  // Set slider values
  xs->setValue((int) (b->size.x*10));
  ys->setValue((int) (b->size.y*10));
  zs->setValue((int) (b->size.z*10));
}

Sld::Sld() : QObject()
{
}

void Sld::xChanged(int val)
{
  box.size.x= val/10.0;
}

void Sld::yChanged(int val)
{
  box.size.y= val/10.0;
}

void Sld::zChanged(int val)
{
  box.size.z= val/10.0;
}

void Sld::closed(void)
{
  exit(1);
}

int hitBox(MAV_object *o, MAV_mouseEvent *me)
{
  if (me->movement==MAV_PRESSED) {
    
    /* Increase size of box */
    box.size.x++;

    /* Update slider */
    xs->setValue((int) (box.size.x*10));
  }
  
  return 1;
}

int main(int argc, char *argv[])
{
  MAV_SMS *sms;
  MAV_object *obj;

  // Initiliase Qt
  QApplication app(argc, argv, "slider");

  // Create a class to receive GUI events
  Sld *sld= new Sld();
 
  // Main widget container - a vbox
  QVBox *vb= new QVBox();
  vb->resize(500, 500);

  // Make 3 sliders - each is a child of the vbox
  xs= new QSlider(0, 50, 1, 1, Qt::Horizontal, vb, "x slider");
  xs->setTickmarks(QSlider::Above);
  QObject::connect(xs, SIGNAL(valueChanged(int)), sld, SLOT(xChanged(int)));

  ys= new QSlider(0, 50, 1, 1, Qt::Horizontal, vb, "y slider");
  ys->setTickmarks(QSlider::Above);
  QObject::connect(ys, SIGNAL(valueChanged(int)), sld, SLOT(yChanged(int)));

  zs= new QSlider(0, 50, 1, 1, Qt::Horizontal, vb, "z slider");
  zs->setTickmarks(QSlider::Above);
  QObject::connect(zs, SIGNAL(valueChanged(int)), sld, SLOT(zChanged(int)));

  // Make sure we can exit
  app.setMainWidget(vb);
  QObject::connect(&app, SIGNAL(lastWindowClosed()), sld, SLOT(closed()));

  // Show the vbox to make it visible
  vb->show();

  /* Define the parent widget of the Maverik window */
  mav_opt_disp= (char *) vb;

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

  return 1;
}
