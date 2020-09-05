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

#ifndef __POWERUP_H__
#define __POWERUP_H__

#include <iostream>
using namespace std;

#include "SDL.h"
#include "SDL_image.h"

#include "poweruptype.h"
#include "globals.h"

class PowerUp
{
  double x;
  double y;
  SDL_Surface *imgPowerUps;
  PowerUpType * type;
public:
    PowerUp (double x, double y, PowerUpType * type, SDL_Surface * imgPowerUps);
   ~PowerUp ();
  void draw (SDL_Surface * dest);
  void update (int time);
  double getX ()
  {
    return x;
  }
  double getY ()
  {
    return y;
  }
  int getWidth ()
  {
    return type->get_width ();
  }
  int getHeight ()
  {
    return type->get_height ();
  }
  string get_script ()
  {
    return type->get_script ();
  }
  PowerUpType * getType ()
  {
    return type;
  }
};

#endif
