/*
    GNU Maverik - a system for managing display and interaction in 
               Virtual Environment applications.
    Copyright (C) 1999-2002 Advanced Interfaces Group
				  2008  <name of author>

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
#include "mav_vrad.h"
#include <stdlib.h>

MAV_vrad vrad;

int keyb(MAV_object *o, MAV_keyboardEvent *ke)
{
  if (ke->movement==MAV_PRESSED) {
    switch (ke->key) {
    case '=': /* brighter */
      mav_vradGamma(&vrad, 0.9);
      break;

    case '-': /* dimmer */
      mav_vradGamma(&vrad, 1.1);
      break;
    }
  }

  return 1;
}


int main(int argc, char *argv[])
{
  MAV_SMS *sms;
  
  mav_opt_objectTables=MAV_FALSE;
  mav_initialise(&argc, argv);
  mav_vradModuleInit();

  if (!mav_vradRead("aiglab", &vrad, MAV_ID_MATRIX)) exit(1);
  mav_vradGamma(&vrad, 0.7);

  sms= mav_SMSNew(mav_SMSClass_objList, mav_objListNew());
  mav_SMSCallbackObjectAddExec(sms, mav_objectNew(mav_class_vrad, &vrad));

  mav_windowBackfaceCullSet(mav_win_all, MAV_TRUE);

  mav_navigationMouse(mav_win_all, mav_navigationMouseDefault);
  mav_navigationMouseDefaultParams(mav_win_all, MAV_MIDDLE_BUTTON, mav_navigateYawFixedUp, 0.5, -0.001, mav_navigatePitch, 0.5, 0.001);

  mav_callbackKeyboardSet(mav_win_all, mav_class_world, keyb);

  mav_vp_default.eye.x= -0.687075;
  mav_vp_default.eye.y=  1.220000;
  mav_vp_default.eye.z= -0.127375;

  while (1) {
    mav_eventsCheck();
    mav_frameBegin();
    mav_SMSDisplay(mav_win_all, sms);
    mav_frameEnd();
  }
}
