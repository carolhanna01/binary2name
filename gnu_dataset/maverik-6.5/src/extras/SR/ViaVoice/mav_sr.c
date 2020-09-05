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
#include <smapi.h>
#include <stdio.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>           

MAV_callback *mav_callback_SR;
int mavlib_SRMic=0;
int mavlib_VVSck;
int (*mavlib_VVFn)();
void *mavlib_VVData;
int mavlib_VVrv;



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
  SM_MSG rpy;
  int rv=0, rc;

  rc= SmDefineVocab(voc, 0, NULL, &rpy);

  if (rc==SM_RC_OK)
  {
    fprintf(stderr, "SR: Vocab '%s' defined\n", voc);
    rv=1;
  }
  else
  {
    fprintf(stderr, "SR: Failed to define vocab '%s', rc=%i\n", voc, rc);
  }
	
  return rv;
}

int mav_SRVocabEnable(char *voc)
{
  SM_MSG rpy;
  int rv=0, rc;

  rc= SmEnableVocab(voc, &rpy);

  if (rc==SM_RC_OK)
  {
    fprintf(stderr, "SR: Vocab '%s' enabled\n", voc);
    rv=1;
  }
  else
  {
    fprintf(stderr, "SR: Failed to enable vocab '%s', rc=%i\n", voc, rc);
  }		

  return rv;
}

int mav_SRVocabDisable(char *voc)
{
  SM_MSG rpy;
  int rv=0, rc;

  rc= SmDisableVocab(voc, &rpy);

  if (rc==SM_RC_OK)
  {
    fprintf(stderr, "SR: Vocab '%s' disabled\n", voc);
    rv=1;
  }
  else
  {
    fprintf(stderr, "SR: Failed to disable vocab '%s', rc=%i\n", voc, rc);
  }

  return 1;
}

int mav_SRVocabWordAdd(char *voc, char *word)
{
  SM_VOCWORD **vword;
  SM_MSG rpy;
  SM_VOCWORD *missing;
  unsigned long nmissing;
  int rv=0, rc;

  /* Add word */
  vword= mav_malloc(sizeof(SM_VOCWORD *));
  vword[0]= mav_malloc(sizeof(SM_VOCWORD));
  vword[0]->flags= 0;
  vword[0]->spelling_size= strlen(word)+1;
  vword[0]->spelling= word;

  rc= SmAddToVocab(voc, 1, vword, &rpy);

  mav_free(vword[0]);
  mav_free(vword);

  if (rc==SM_RC_OK)
  {
    /* Check word was successfully added */
    rc= SmGetVocWords(rpy, &nmissing, &missing);

    if (rc==SM_RC_OK)
    {
      switch (nmissing) {
      case 0:
	fprintf(stderr, "SR: Added word '%s' to vocab '%s'\n", word, voc);
	rv=1;
	break;

      case 1:
	fprintf(stderr, "SR: Failed to add word '%s' to vocab '%s' - its an unknown word\n", word, voc);
	break;
      
      default:
	fprintf(stderr, "SR: Unknown number of missing words!!!\n");
	break;
      }
    }
    else
    {
      fprintf(stderr, "SR: Failed to check word '%s' when adding to vocab '%s', rc=%i\n", word, voc, rc);
    }
  }
  else
  {
    fprintf(stderr, "SR: Failed to add word '%s' to vocab '%s', rc=%i\n", word, voc, rc);
  }

  return rv;
}

int mav_SRVocabWordRmv(char *voc, char *word)
{
  SM_VOCWORD **vword;
  SM_MSG rpy;
  int rv=0, rc;

  /* Remove word */
  vword= mav_malloc(sizeof(SM_VOCWORD *));
  vword[0]= mav_malloc(sizeof(SM_VOCWORD));
  vword[0]->flags= 0;
  vword[0]->spelling_size= strlen(word)+1;
  vword[0]->spelling= word;

  rc= SmRemoveFromVocab(voc, 1, vword, &rpy);

  mav_free(vword[0]);
  mav_free(vword);

  if (rc==SM_RC_OK)
  {
    fprintf(stderr, "SR: Removed word '%s' from vocab '%s'\n", word, voc);
    rv=1;
  }
  else
  {
    fprintf(stderr, "SR: Failed to remove word '%s' from vocab '%s', rc=%i\n", word, voc, rc);    
  }

  return rv;
}

int mav_SRVocabFileAdd(char *voc, char *filename)
{
  FILE *f;
  char word[200];
  int rv=1;

  f= fopen(filename, "r");

  if (f)
  {
    while (!feof(f)) {
      if (fscanf(f, "%s", word)==1) rv&=mav_SRVocabWordAdd(voc, word);
    }
    fclose(f);
  }
  else
  {
    fprintf(stderr, "SR: Failed to open file '%s'\n", filename);    
    rv=0;
  }

  return rv;
}



/* Microphone control */

int mav_SRMicOn(void)
{
  SM_MSG rpy;
  int rv=0, rc;

  rc= SmMicOn(&rpy);

  if (rc==SM_RC_OK || rc==SM_RC_MIC_ALREADY_ON)
  {
    fprintf(stderr, "SR: Mic on\n");
    mavlib_SRMic=1;
    rv=1;

    /* Start capturing the audio and processing it */
    SmRecognizeNextWord(SmAsynchronous);
  }
  else
  {
    fprintf(stderr, "SR: Failed to turn mic on, rc=%i\n", rc);
    mavlib_SRMic=0;
  }

  return rv;
}

int mav_SRMicOff(void)
{
  SM_MSG rpy;
  int rv=0, rc;

  rc= SmMicOff(&rpy);

  if (rc==SM_RC_OK || rc==SM_RC_MIC_ALREADY_OFF)
  {
    fprintf(stderr, "SR: Mic off\n");
    mavlib_SRMic=0;
    rv=1;
  }
  else
  {
    fprintf(stderr, "SR: Failed to turn mic off, rc=%i\n", rc);
    mavlib_SRMic=1;
  }					     

  return rv;
}



/* ViaVoice callbacks */

SmHandler mavlib_VVMicOnCB(SM_MSG rpy, void *client, void *call_data)
{
  fprintf(stderr, "SR: Mic has been turned on by some other process\n");
  mavlib_SRMic=1;

  /* Start capturing the audio and processing it */
  SmRecognizeNextWord(SmAsynchronous);

  return SM_RC_OK;
}

SmHandler mavlib_VVMicOffCB(SM_MSG rpy, void *client, void *call_data)
{
  fprintf(stderr, "SR: Mic has been turned off by some other process\n");
  mavlib_SRMic=0;

  return SM_RC_OK;
}

SmHandler mavlib_VVFocusCB(SM_MSG rpy, void *client, void *call_data)
{
  return SM_RC_OK;
}

SmHandler mavlib_VVUtteranceCB(SM_MSG rpy, void *client, void *call_data)
{
  return SM_RC_OK;
}

SmHandler mavlib_VVNextWordCB(SM_MSG rpy, void *client, void *call_data)
{
  return SM_RC_OK;
}

SmHandler mavlib_VVWordCB(SM_MSG rpy, void *client, void *call_data)
{
  unsigned long num_firm;
  SM_WORD *firm;

  /* Get word */
  SmGetFirmWords(rpy, &num_firm, &firm);

  if (num_firm==1)
  {
    if (*firm[0].spelling && *firm[0].vocab)
    {
      /* Make up event data structure */
      MAV_SREvent sr;

      /* Speech bit */
      sr.word= firm[0].spelling;
      sr.vocab= firm[0].vocab;
      sr.score= 0;

      /* Window and mouse bit */
      sr.win= mav_win_mouse;
      sr.x= mav_mouse_x;
      sr.y= mav_mouse_y;
      sr.root_x= mav_mouse_root_x;
      sr.root_y= mav_mouse_root_y;

      /* See what we hit */
      sr.line= mav_lineFrom2DPoint(sr.win, sr.x, sr.y);
      sr.intersects= mav_SMSIntersectLineAll(sr.win, sr.line, &sr.objint, &sr.obj);

      /* Check if any callbacks are defined for the world object */
      if (mav_callbackQuery(mav_callback_SR, sr.win, mav_object_world))
      {
	mavlib_VVrv= mav_callbackSRExec(sr.win, mav_object_world, &sr);
      }
      else
      {
	/* If we intersected, check for any class callbacks before specific class ones */
	if (sr.intersects) 
	{
	  if (mav_callbackQuery(mav_callback_SR, sr.win, mav_object_any))
	  {
	    mavlib_VVrv= mav_callbackSRExec(sr.win, mav_object_any, &sr);
	  }
	  else
	  {
	    if (mav_callbackQuery(mav_callback_SR, sr.win, sr.obj))
	    {
	      mavlib_VVrv= mav_callbackSRExec(sr.win, sr.obj, &sr);
	    }
	  }
	}
	else
	{
	  /*  If no intersection check for none class */
	  if (mav_callbackQuery(mav_callback_SR, sr.win, mav_object_none))
	  {
	    mavlib_VVrv= mav_callbackSRExec(sr.win, mav_object_none, &sr);
	  }
	}
      }
    }
  }
  else
  {
    fprintf(stderr, "SR: more than one firm word!!!\n");
  }

  /* Go again */
  SmRecognizeNextWord(SmAsynchronous);

  return SM_RC_OK;
}



/* Function to poll for events from ViaVoice */

int mavlib_VVPoll(void)
{
  fd_set fd;
  struct timeval tv;

  /* See if there is anything to read from ViaVoice */

  FD_ZERO(&fd);
  FD_SET(mavlib_VVSck, &fd);

  tv.tv_sec=0;
  tv.tv_usec=0;

  mavlib_VVrv=0;

  if (select(200, &fd, NULL, NULL, &tv)) {
    (mavlib_VVFn)(mavlib_VVData);
  }

  return mavlib_VVrv;
}



/* Connection/disconnection handler function */

int mavlib_VVNotifier(int sck, int (*fn)(), void *recv_data, void *client_data)
{
  if (fn) {
    /* Store the data needed to communicate with ViaVoice */
    mavlib_VVSck= sck;
    mavlib_VVFn= fn;
    mavlib_VVData= recv_data;

    /* Add a new device */
    mav_deviceNew(NULL, NULL, mavlib_VVPoll);
  }

  return SM_RC_OK;
}



/* Routine to toggle microphone status */

void mavlib_srcf11(MAV_window *w)
{
  if (mavlib_SRMic) 
  {
    mav_SRMicOff();
  }
  else
  {
    mav_SRMicOn();
  }
}



/* Routines to initialise the module */

char *mav_SRModuleID(void)
{
  return "SR (ViaVoice)";
}

int mav_SRModuleInit(void)
{
  SmArg smArgs[10];
  SM_MSG rpy;
  int rv=0, rc, smc;

  /* Add the new module */
  mav_moduleNew(mav_SRModuleID);

  /* Define new callback for speech recognition event */
  mav_callback_SR= mav_callbackNew();

  /* Setup the ViaVoice options */
  smc=0;
  SmSetArg(smArgs[smc], SmNapplicationName, "MaverikApp"); smc++;
  SmSetArg(smArgs[smc], SmNrecognize, 1); smc++;
  SmSetArg(smArgs[smc], SmNuserId, SM_USE_CURRENT); smc++;
  SmSetArg(smArgs[smc], SmNtask, SM_USE_CURRENT); smc++;
  SmSetArg(smArgs[smc], SmNenrollId, SM_USE_CURRENT); smc++;
  SmSetArg(smArgs[smc], SmNexternalNotifier, mavlib_VVNotifier); smc++;

  /* Open a connections */
  rc= SmOpen(smc, smArgs);

  if (rc==SM_RC_OK)
  {
    /* Connect */
    rc= SmConnect(smc, smArgs, &rpy);

    if (rc==SM_RC_OK)
    {
      /* Define callbacks */
      SmAddCallback(SmNmicOnCallback, mavlib_VVMicOnCB, NULL);      
      SmAddCallback(SmNmicOffCallback, mavlib_VVMicOffCB, NULL);      
      SmAddCallback(SmNfocusGrantedCallback, mavlib_VVFocusCB, NULL);      
      SmAddCallback(SmNutteranceCompletedCallback, mavlib_VVUtteranceCB, NULL);      
      SmAddCallback(SmNrecognizeNextWordCallback, mavlib_VVNextWordCB, NULL);      
      SmAddCallback(SmNrecognizedWordCallback, mavlib_VVWordCB, NULL);      

      /* Make Ctrl-F11 toggle mic on/off (this key combo also used by TR) */
      if (mav_ctrlF[11] && mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: Ctrl-F11 key press already reserved, overwriting\n"); 
      mav_ctrlF[11]= mavlib_srcf11;
      mav_ctrlF_desc[11]= "Ctrl-F11 toggle microphone on/off";      

      rv=1;
    }
    else
    {
      if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: failed to connect with ViaVoice, rc=%i\n", rc);
    }
  }
  else
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: failed to open connection with ViaVoice, rc=%i\n", rc);
  }

  return rv;
}
