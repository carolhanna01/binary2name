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

#include "settingsmanager.h"

SettingsManager::SettingsManager ()
{
}

SettingsManager::~SettingsManager ()
{
}

void
SettingsManager::set_fullscreen (bool fullscreen)
{
  this->fullscreen = fullscreen;
}

void
SettingsManager::set_sound_enabled (bool sound_enabled)
{
  this->sound_enabled = sound_enabled;
}

void
SettingsManager::save_settings ()
{
  ofstream file;
#ifndef SINGLEDIR
  string location = string (getenv ("HOME")) + "/.ballandpaddle/config";
#endif
#ifdef SINGLEDIR
  string location = "config";
#endif
  file.open (location.c_str ());
  if (!file.fail ())
    {
      file << fullscreen << endl;
      file << sound_enabled << endl;
      file.close ();
    }
  else
    {
      cout << "couldn't open file: " << location << endl;
    }
}

void
SettingsManager::load_settings ()
{
  ifstream file;
#ifndef SINGLEDIR
  string location = string (getenv ("HOME")) + "/.ballandpaddle/config";
#endif
#ifdef SINGLEDIR
  string location = "config";
#endif
  file.open (location.c_str (), ifstream::in);
  if (!file.fail ())
    {
      file >> fullscreen;
      file >> sound_enabled;
      file.close ();
    }
  else
    {
#ifndef SINGLEDIR
      location = string (getenv ("HOME")) + "/.ballandpaddle";
      mkdir (location.c_str (), 0777);
#endif
//      m_strSkinName = "Blue";
//      m_strSkinFilename = "skin.png";
      sound_enabled = true;
    }
}

