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
#include "mav_sr.h"
#include <stdio.h>

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

/* Speech recognition event callback */
int srEvent(MAV_object *o, MAV_SREvent *se)
{
  printf("I think you said %s for vocab %s\n", se->word, se->vocab);
  
  return 1;
}

int main(int argc, char *argv[])
{
  MAV_box box;
  MAV_object *obj;
  MAV_SMS *sms;

  /* Initialise the Maverik system */
  mav_initialise(&argc, argv);

  /* Initialise the speech recognition module */
  mav_SRModuleInit();

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

  /* Define a speech recognition event callback */
  mav_callbackSRSet(mav_win_all, mav_class_world, srEvent);

  /* Define the words we'll recognize */
  mav_SRVocabDefine("voc1");
  mav_SRVocabWordAdd("voc1", "one");
  mav_SRVocabWordAdd("voc1", "two");
  mav_SRVocabWordAdd("voc1", "five");
  mav_SRVocabEnable("voc1");

  /* Turn the microphone on */
  mav_SRMicOn();
  
  /* Rendering loop */
  while (1) drawFrame(sms);
}
