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

#ifndef SETTINGS_MANAGER_H
#define SETTINGS_MANAGER_H

#include <fstream>
#include <iostream>
using namespace std;

#include <sys/stat.h>

class SettingsManager
{
  bool fullscreen;
  bool sound_enabled;
public:
  SettingsManager ();
  ~SettingsManager ();
  void set_sound_enabled (bool sound_enabled);
  void set_fullscreen (bool fullscreen);
  bool get_fullscreen () { return fullscreen; }
  bool get_sound_enabled () { return sound_enabled; }
  void save_settings ();
  void load_settings ();
};

#endif

