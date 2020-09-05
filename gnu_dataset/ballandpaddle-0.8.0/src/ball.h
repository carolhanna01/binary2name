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

#ifndef __BALL_H__
#define __BALL_H__

#include <cmath>
#include <iostream>
#include <map>
using namespace std;

#include <libguile.h>

#include "SDL.h"
#include "SDL_image.h"

#include "globals.h"
#include "block.h"
#include "blocktype.h"
#include "paddle.h"
#include "powerup.h"
#include "soundmanager.h"
#include "utilities.h"

class Ball
{
  SDL_Surface *imgBall;
  double x;
  double y;
  int width;
  int height;
  int field_width;
  int field_height;
  double speed;
  double direction;
  bool attached;
  Paddle *attacher;
  double attachXOff;
  double attachYOff;
  void mirrorXDir ();
  void mirrorYDir ();
  void normalizeDirection ();
  void bounceTR ();
  void bounceTL ();
  void bounceBR ();
  void bounceBL ();
public:
    Ball (int x, int y, double direction, int size, 
          int field_width, int field_height, SDL_Surface * imgBall);
   ~Ball ();
  static Ball *current;
  void draw (SDL_Surface * dest, bool powerBalls);
  void update (int time, map < string, BlockType * > block_palette, 
               vector < Block * >blocks, Paddle * paddle,
	       vector < PowerUp * >&powerUps, bool powerBalls, int &score);
  void setAttached (Paddle * paddle);
  void setSpeed (double speed);
  void set_size (int size);
  double getX ()
  {
    return x;
  }
  double getY ()
  {
    return y;
  }
  double getDirection ()
  {
    return direction;
  }
  double getSpeed ()
  {
    return speed;
  }
  void release ();
  bool isAttached ()
  {
    return attached;
  }
};

#endif
