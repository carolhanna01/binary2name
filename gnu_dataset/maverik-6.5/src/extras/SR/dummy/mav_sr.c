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

MAV_callback *mav_callback_SR;



/* Wrapper routines to set and execute the speech recognition callback */

void mav_callbackSRSet(MAV_window *w, MAV_class *c, MAV_callbackSRFn fn)
{
  mav_callbackSet(mav_callback_SR, w, c, (MAV_callbackFn) fn);
}

int mav_callbackSRExec(MAV_window *w, MAV_object *o, MAV_SREvent *se)
{
  return (mav_callbackExec(mav_callback_SR, w, o, (void *) se, NULL));
}



/* Vocabulary control */

int mav_SRVocabDefine(char *voc)
{
  return 1;
}

int mav_SRVocabEnable(char *voc)
{
  return 1;
}

int mav_SRVocabDisable(char *voc)
{
  return 1;
}

int mav_SRVocabWordAdd(char *voc, char *word)
{
  return 1;
}

int mav_SRVocabWordRmv(char *voc, char *word)
{
  return 1;
}

int mav_SRVocabFileAdd(char *voc, char *filename)
{
  return 1;
}



/* Microphone control */

int mav_SRMicOn(void)
{
  return 1;
}

int mav_SRMicOff(void)
{
  return 1;
}



/* Routines to initialise the module */

char *mav_SRModuleID(void)
{
  return "SR (dummy)";
}

int mav_SRModuleInit(void)
{
  /* Add the new module */
  mav_moduleNew(mav_SRModuleID);

  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: code not compiled with SR option, ignoring\n");

  /* Define new callback for speech recognition event */
  mav_callback_SR= mav_callbackNew();

  return 1;
}
