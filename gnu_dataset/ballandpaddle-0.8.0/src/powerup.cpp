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

#include "powerup.h"

PowerUp::PowerUp (double x, double y, PowerUpType * type, SDL_Surface *imgPowerUps)
{
  this->x = x;
  this->y = y;
  this->type = type;
  this->imgPowerUps = imgPowerUps;
/*
  if (type == POWERUP_RANDOM)
    {
      double d = (double) rand () / ((double) RAND_MAX + (double) 1);
      if (d < 0.2)
	{
	  this->type = 0;
	}
      else if (d < 0.4)
	{
	  this->type = 1;
	}
      else if (d < 0.48)
	{
	  this->type = 2;
	}
      else if (d < 0.56)
	{
	  this->type = 3;
	}
      else if (d < 0.64)
	{
	  this->type = 4;
	}
      else if (d < 0.7)
	{
	  this->type = 5;
	}
      else if (d < 0.75)
	{
	  this->type = 6;
	}
      else if (d < 0.88)
	{
	  this->type = 7;
	}
      else
	{
	  this->type = 8;
	}
    }
*/
}

PowerUp::~PowerUp ()
{
}

void
PowerUp::draw (SDL_Surface * dest)
{
  SDL_Rect s = { type->get_sx (), type->get_sy (), 
                 type->get_width (), type->get_height () };
  SDL_Rect d = { (int) x, (int) y, 0, 0 };
  SDL_BlitSurface (imgPowerUps, &s, dest, &d);
}

void
PowerUp::update (int time)
{
  y += time * 0.2;
}
