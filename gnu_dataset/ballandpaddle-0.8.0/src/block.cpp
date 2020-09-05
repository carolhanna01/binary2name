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

#include "block.h"

Block *Block::current = NULL;

Block::Block (int x, int y, BlockType * type,
	      SDL_Surface * imgBlocks)
{
  this->x = x;
  this->y = y;
  this->type = type;
  this->imgBlocks = imgBlocks;
  maxStrength = 1;
  state = 0;
  strength = 1;
}

Block::~Block ()
{
}

void
Block::draw (SDL_Surface * dest)
{
  if (state < 0)
    return;
/*
  SDL_Rect s = { (type % 8) * 32, (type / 8) * 128 + state * 16, 32, 16 };
*/
  SDL_Rect s = { type->get_sx (), 
                 type->get_sy (), 
                 type->get_width (),
                 type->get_height () };
  SDL_Rect d = { x, y, 0, 0 };
  SDL_BlitSurface (imgBlocks, &s, dest, &d);
}

void
Block::update (int time)
{
}

void
Block::setMaxStrength (int maxStrength)
{
  this->maxStrength = maxStrength;
}

void
Block::setStrength (int strength)
{
  this->strength = strength;
}

void
Block::setState (int state)
{
  this->state = state;
}

// returns true when the block is destroyed
bool
Block::weaken ()
{
  if (type->get_invincible ())
    return false;
  if (state == -1)
    return false;
  strength--;
  if (strength == 0)
    {
      state = -1;
      return true;
    }
  int cracks = maxStrength - strength;
  switch (maxStrength)
    {
    case 2:
      state = 2;
      break;
    case 3:
      if (strength == 2)
	{
	  srand (SDL_GetTicks ());
	  state = ((rand () > RAND_MAX / 2) ? (4) : (5));
	}
      else
	state = 6;
      break;
    }
  return false;
}

void
Block::set_type (BlockType *type)
{
  this->type = type;
}

