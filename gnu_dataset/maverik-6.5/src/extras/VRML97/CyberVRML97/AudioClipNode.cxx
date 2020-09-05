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

/******************************************************************
*
*	VRML library for C++
*
*	Copyright (C) Satoshi Konno 1996-1997
*
*	File:	AudioClipNode.cpp
*
******************************************************************/

#ifdef WIN32
#include <windows.h>
#endif

#include "AudioClipNode.h"

////////////////////////////////////////////////
//	PlayAudioClip
////////////////////////////////////////////////

static void PlayAudioClip(AudioClipNode *ac)
{
#ifdef SUPPORT_SOUND
	if (0 < ac->getNUrls()) {
		char *filename = ac->getUrl(0);
		if (filename) {
#ifdef WIN32
			PlaySound(filename, NULL, SND_FILENAME | SND_ASYNC);
#endif
		}
	}
#endif
}

////////////////////////////////////////////////
//	StopAudioClip
////////////////////////////////////////////////

static void StopAudioClip(AudioClipNode *ac)
{
}

////////////////////////////////////////////////
//	AudioClipNode::initialize
////////////////////////////////////////////////

void AudioClipNode::initialize() 
{
	setIsActive(false);
	StopAudioClip(this);
	setCurrentTime(-1.0);
}

////////////////////////////////////////////////
//	AudioClipNode::uninitialize
////////////////////////////////////////////////

void AudioClipNode::uninitialize() 
{
	StopAudioClip(this);
}

////////////////////////////////////////////////
//	AudioClipNode::update
////////////////////////////////////////////////

void AudioClipNode::update() 
{
	double currentTime = getCurrentTime();

	if (currentTime < 0.0)
		currentTime = GetCurrentSystemTime();

	double startTime = getStartTime();
	double stopTime = getStopTime();

	bool bActive	= isActive();
	bool bLoop		= isLoop();

	if (bActive == false) {
		if (currentTime <= startTime) {
			if (bLoop == true && stopTime <= startTime)
				bActive = true;
			else if	(bLoop == true && startTime < stopTime)
				bActive = true;
			else if (bLoop == false && startTime < stopTime)
				bActive = true;

			if (bActive == true) {
				setIsActive(true);
				sendEvent(getIsActiveField());
				PlayAudioClip(this);
				setIsActive(false);
			}
		}
	}

	currentTime = GetCurrentSystemTime();
	setCurrentTime(currentTime);

/*	
	if (bActive == true) {
		if (bLoop == true && startTime < stopTime) {
			if (stopTime < currentTime)
				bActive = false;
		}
		else if (bLoop == false && stopTime <= startTime) {
			if (startTime + cycleInterval < currentTime)
				bActive = false;
		}

		if (bActive == false) {
			setIsActive(false);
			sendEvent(getIsActiveField());
		}
	}
*/
}

