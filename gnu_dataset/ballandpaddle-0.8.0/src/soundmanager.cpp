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

#include "soundmanager.h"

extern SettingsManager settings_manager;

SoundManager::SoundManager ()
{
}

SoundManager::~SoundManager ()
{
}

void
SoundManager::load_sounds (string levelset_filename)
{
  this->levelset_filename = levelset_filename;
  addSound ("blockbreak",
			    LEVELPATH + "/" + levelset_filename +
			    "/blockbreak.wav");
  addSound ("ballhitwall",
			    LEVELPATH + "/" + levelset_filename +
			    "/ballhitwall.wav");
  addSound ("laser",
			    LEVELPATH + "/" + levelset_filename +
			    "/laser.wav");
  addSound ("bonustick",
			    LEVELPATH + "/" + levelset_filename +
			    "/bonustick.wav");
}

void
SoundManager::addSound (string name, string filename)
{
  Mix_Chunk *temp = Mix_LoadWAV (filename.c_str ());
  if (temp == NULL)
    {
      cout << "Couldn't load sound file: " << filename << endl;
      return;
    }
  sounds.push_back (temp);
  soundnames.push_back (name);
}

void
SoundManager::playSound (string name)
{
  if (!settings_manager.get_sound_enabled ()) return;
  for (int i = 0; i < sounds.size (); i++)
    {
      if (soundnames[i].compare (name) == 0)
	{
	  Mix_PlayChannel (-1, sounds[i], 0);
	}
    }
}

void
SoundManager::loopSound (string name)
{
  if (!settings_manager.get_sound_enabled ()) return;
  for (int i = 0; i < sounds.size (); i++)
    {
      if (soundnames[i].compare (name) == 0)
	{
	  if (Mix_PlayChannel (-1, sounds[i], -1) == -1)
	    cout << "Error playing sound \"" << name << "\"" << endl;
	}
    }
}

void
SoundManager::freeSounds ()
{
  for (int i = 0; i < sounds.size (); i++)
    Mix_FreeChunk (sounds[i]);
}

void
SoundManager::stopSound ()
{
  Mix_HaltChannel (-1);
}

