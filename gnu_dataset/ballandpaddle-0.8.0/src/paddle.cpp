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

#include "paddle.h"

Paddle::Paddle (SDL_Surface * imgPaddle, int field_width, int field_height)
{
  this->imgPaddle = imgPaddle;

  x = 160;
  y = 390;

  width = 80;
  height = 10;

  this->field_width = field_width;
  this->field_height = field_height;

  laser = false;
  glue = false;

  direction = PADDLE_NONE;
}

Paddle::~Paddle ()
{
  SDL_FreeSurface (imgPaddle);
}

void
Paddle::setDirection (int direction)
{
  this->direction = direction;
}

void
Paddle::draw (SDL_Surface * dest)
{
  SDL_Rect s = { 0, 0, width, height };
  if (width == 80)
    s.y += 10;
  else if (width == 160)
    s.y += 20;
  if (isLaser ())
    s.y += 30;
  else if (isGlue ())
    s.y += 60;
  SDL_Rect d = { (int) x, (int) y, 0, 0 };
  SDL_BlitSurface (imgPaddle, &s, dest, &d);
}

void
Paddle::update (int time)
{
  switch (direction)
    {
    case PADDLE_NONE:
      break;
    case PADDLE_LEFT:
      x -= time * 0.4;
      break;
    case PADDLE_RIGHT:
      x += time * 0.4;
      break;
    }
  if (x < 0)
    x = 0;
  if (x + width > field_width)
    x = field_width - width;
}

void
Paddle::stretch ()
{
  if (width < 160)
    {
      width *= 2;
      x = x + width / 4 - width / 2;
    }
}

void
Paddle::shrink ()
{
  if (width > 40)
    {
      width /= 2;
      x = x + width - width / 2;
    }
}

void
Paddle::setLaser ()
{
  glue = false;
  laser = true;
}

void
Paddle::setGlue ()
{
  glue = true;
  laser = false;
}

void
Paddle::setX (int x)
{
  this->x = x;
}
