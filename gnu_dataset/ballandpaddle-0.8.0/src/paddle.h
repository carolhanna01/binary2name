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

#ifndef __PADDLE_H__
#define __PADDLE_H__

#include "SDL.h"
#include "SDL_image.h"

#include "globals.h"

class Paddle
{
  double x;
  double y;
  int width;
  int height;
  int field_width;
  int field_height;
  int direction;
  bool glue;
  bool laser;
  SDL_Surface *imgPaddle;
public:
    Paddle (SDL_Surface * imgPaddle, int field_width, int field_height);
   ~Paddle ();
  void draw (SDL_Surface * dest);
  void setDirection (int direction);
  void update (int time);
  void setLaser ();
  void setGlue ();
  void setX (int x);
  bool isGlue ()
  {
    return glue;
  }
  bool isLaser ()
  {
    return laser;
  }
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
    return width;
  }
  int getHeight ()
  {
    return height;
  }
  int getDirection ()
  {
    return direction;
  }
  void stretch ();
  void shrink ();
};

#endif
