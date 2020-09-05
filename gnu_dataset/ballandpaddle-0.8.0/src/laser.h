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

#ifndef __LASER_H__
#define __LASER_H__

#include <vector>
#include <map>
using namespace std;

#include <libguile.h>

#include "SDL.h"

#include "blocktype.h"
#include "block.h"
#include "powerup.h"
#include "utilities.h"
#include "soundmanager.h"

class Laser
{
  double x;
  double y;
  int width;
  int height;
  int field_width;
  int field_height;
  SDL_Surface *imgLaser;
  bool alive;
public:
  static Laser *current;
    Laser (int x, int y, int width, int height, 
           int field_width, int field_height, SDL_Surface * imgLaser);
   ~Laser ();
  void draw (SDL_Surface * dest);
  void update (int time, map < string, BlockType * > block_palette, 
               vector < Block * >blocks,
	       vector < PowerUp * >&powerUps, int &score);
  bool isAlive ()
  {
    return alive;
  }
  double getY ()
  {
    return y;
  }
  int getHeight ()
  {
    return height;
  }
};

#endif
