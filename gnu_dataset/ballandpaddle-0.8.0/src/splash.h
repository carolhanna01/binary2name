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

#ifndef SPLASH_H
#define SPLASH_H

#include <vector>
#include <iostream>
using namespace std;

#include "SDL.h"
#include "SDL_image.h"

#include "gamestate.h"
#include "globals.h"

class Splash:public GameState
{

private:
  static Splash m_instance;
  SDL_Surface *image;
  int time;
  long last_time;
public:
  static Splash *instance ()
  {
    return &m_instance;
  }
  void init ();
  void cleanup ();
  void pause ();
  void resume ();
  void handleEvents ();
  void draw (SDL_Surface *dest);
  void update ();
  void start ();
  bool done ()
  {
    return SDL_GetTicks () - last_time >= time;
  }
  bool getTransitioning ()
  {
    return false;
  }
protected:
  Splash ()
  {
  };
};

#endif
