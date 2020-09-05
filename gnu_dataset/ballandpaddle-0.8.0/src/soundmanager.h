/*
 *  Ball And Paddle
 *
 *  Copyright (C) 2007, 2008 by Eric Hutchins
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 3 of the License, or
 *  (at your option) any later version.
 *  
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 **/

#ifndef __SOUNDMANAGER_H__
#define __SOUNDMANAGER_H__

#include <vector>
#include <string>
#include <iostream>
using namespace std;

#include "SDL.h"
#include "SDL_mixer.h"

#include "globals.h"
#include "settingsmanager.h"

class SoundManager
{
  vector < Mix_Chunk * >sounds;
  vector < string > soundnames;
  string levelset_filename;
public:
  SoundManager ();
  ~SoundManager ();
  void load_sounds (string levelset_filename);
  void addSound (string name, string filename);
  void playSound (string name);
  void loopSound (string name);
  void stopSound ();
  void freeSounds ();
};

#endif
