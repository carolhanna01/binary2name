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

#ifndef __OPTIONSMENU_H__
#define __OPTIONSMENU_H__

#include <vector>
#include <iostream>
using namespace std;

#include "SDL.h"
#include "SDL_image.h"

#include "globals.h"
#include "gamestate.h"
#include "settingsmanager.h"

class OptionsMenu:public GameState
{
  int cursor;
    vector < string > menuItems;
  SDL_Surface *imgBackground;
  int spacing;

  void handleItem ();

  static OptionsMenu m_instance;

public:
  static OptionsMenu *instance ()
  {
    return &m_instance;
  }
  void init ();
  void cleanup ();
  void pause ();
  void resume ();
  void handleEvents ();
  void update ();
  void draw (SDL_Surface *dest);
  bool getTransitioning ()
  {
    return false;
  }
protected:
  OptionsMenu ()
  {
  }
};

#endif
