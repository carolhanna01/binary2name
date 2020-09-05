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

#ifndef _MAV_SR_INCLUDE
#define _MAV_SR_INCLUDE

#ifdef MAVAPI
#undef MAVAPI
#endif
#if defined(WIN32) && !defined(__CYGWIN__) 
#ifdef LIBMAV_SR_EXPORTS
#define MAVAPI __declspec(dllexport) 
#else
#define MAVAPI __declspec(dllimport) 
#endif
#else
#define MAVAPI
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* Event and callback */

typedef struct {
  MAV_window *win;
  int x;
  int y;
  int root_x;
  int root_y;
  MAV_line line;
  int intersects;
  MAV_object *obj;
  MAV_objectIntersection objint;
  char *word;
  char *vocab;
  int score;
} MAV_SREvent;

MAVAPI extern MAV_callback *mav_callback_SR;
typedef int (*MAV_callbackSRFn)(MAV_object *, MAV_SREvent *);
MAVAPI void mav_callbackSRSet(MAV_window *w, MAV_class *c, MAV_callbackSRFn fn);
MAVAPI int  mav_callbackSRExec(MAV_window *w, MAV_object *o, MAV_SREvent *ke);



/* Vocabulary control */

MAVAPI int mav_SRVocabDefine(char *voc);
MAVAPI int mav_SRVocabEnable(char *voc);
MAVAPI int mav_SRVocabDisable(char *voc);
MAVAPI int mav_SRVocabWordAdd(char *voc, char *word);
MAVAPI int mav_SRVocabWordRmv(char *voc, char *word);
MAVAPI int mav_SRVocabFileAdd(char *voc, char *filename);



/* Microphone control */

MAVAPI int mav_SRMicOn(void);
MAVAPI int mav_SRMicOff(void);



/* Module initialise */

MAVAPI char *mav_SRModuleID(void);
MAVAPI int mav_SRModuleInit(void);

#ifdef __cplusplus
}
#endif
#endif
