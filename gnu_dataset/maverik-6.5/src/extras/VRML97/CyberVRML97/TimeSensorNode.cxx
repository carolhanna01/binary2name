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
*	File:	AudioClip.cpp
*
******************************************************************/

#include "TimeSensorNode.h"

////////////////////////////////////////////////
//	AudioClip::update
////////////////////////////////////////////////

void TimeSensorNode::update() 
{
	static double currentTime = 0;

	double startTime = getStartTime();
	double stopTime = getStopTime();
	double cycleInterval = getCycleInterval();

	bool bActive	= isActive();
	bool bEnable	= isEnabled();
	bool bLoop		= isLoop();

	if (currentTime == 0)
		currentTime = GetCurrentSystemTime();

	// isActive 
	if (bEnable == false && bActive == true) {
		setIsActive(false);
		sendEvent(getIsActiveField());
		return;
	}

	if (bActive == false && bEnable == true) {
		if (startTime <= currentTime) {
			if (bLoop == true && stopTime <= startTime)
				bActive = true;
			else if (bLoop == false && stopTime <= startTime)
				bActive = true;
			else if (currentTime <= stopTime) {
				if (bLoop == true && startTime < stopTime)
					bActive = true;
				else if	(bLoop == false && startTime < (startTime + cycleInterval) && (startTime + cycleInterval) <= stopTime)
					bActive = true;
				else if (bLoop == false && startTime < stopTime && stopTime < (startTime + cycleInterval))
					bActive = true;
			}
		}
		if (bActive) {
			setIsActive(true);
			sendEvent(getIsActiveField());
			setCycleTime(currentTime);
			sendEvent(getCycleTimeField());
		}
	}

	currentTime = GetCurrentSystemTime();
	
	if (bActive == true && bEnable == true) {
		if (bLoop == true && startTime < stopTime) {
			if (stopTime < currentTime)
				bActive = false;
		}
		else if (bLoop == false && stopTime <= startTime) {
			if (startTime + cycleInterval < currentTime)
				bActive = false;
		}
		else if (bLoop == false && startTime < (startTime + cycleInterval) && (startTime + cycleInterval) <= stopTime) {
			if (startTime + cycleInterval < currentTime)
				bActive = false;
		}
		else if (bLoop == false && startTime < stopTime && stopTime < (startTime + cycleInterval)) {
			if (stopTime < currentTime)
				bActive = false;
		}

		if (bActive == false) {
			setIsActive(false);
			sendEvent(getIsActiveField());
		}
	}

	if (bEnable == false || isActive() == false)
		return;

	// fraction_changed 
	double	fraction = fmod(currentTime - startTime, cycleInterval);
	if (fraction == 0.0 && startTime < currentTime)
		fraction = 1.0;
	else
		fraction /= cycleInterval;
	setFractionChanged((float)fraction);
	sendEvent(getFractionChangedField());

	// cycleTime
	double	cycleTime		= getCycleTime();
	double	cycleEndTime	= cycleTime + cycleInterval;
	while (cycleEndTime < currentTime) {
		setCycleTime(cycleEndTime);
		cycleEndTime += cycleInterval;
		setCycleTime(currentTime);
		sendEvent(getCycleTimeField());
	}

	// time
	setTime(currentTime);
	sendEvent(getTimeField());
}

