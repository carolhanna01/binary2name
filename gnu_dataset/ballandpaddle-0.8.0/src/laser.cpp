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

#include "laser.h"

extern SoundManager *soundManager;

Laser *Laser::current = NULL;

Laser::Laser (int x, int y, int width, int height, 
              int field_width, int field_height, SDL_Surface * imgLaser)
{
  this->x = x;
  this->y = y;
  this->width = width;
  this->height = height;
  this->field_width = field_width;
  this->field_height = field_height;
  this->imgLaser = imgLaser;
  alive = true;
}

Laser::~Laser ()
{
}

void
Laser::draw (SDL_Surface * dest)
{
  if (!alive)
    return;
  SDL_Rect d = { (int) x, (int) y, 0, 0 };
  SDL_BlitSurface (imgLaser, NULL, dest, &d);
}

void
Laser::update (int time, map < string, BlockType * > block_palette, 
               vector < Block * >blocks,
	       vector < PowerUp * >&powerUps, int &score)
{
  y -= time * 0.4;
  if (!alive)
    return;
  int left = (int) x;
  int right = (int) (x + width - 1);
  int top = (int) y;
  int bottom = (int) (y + height - 1);

  vector < Block * >hitBlocks;

  if (top > 0 && left > 0 && top < field_height && left < field_width)
    {
      Block *temp_block = block_at(block_palette, blocks, left,top);
      if (temp_block != NULL)
        {
          bool isThere = false;
          for (int i = 0; i < hitBlocks.size (); i++)
            {
              if (temp_block == hitBlocks[i])
                isThere = true;
            }
          if (!isThere)
            hitBlocks.push_back (temp_block);
        }
    }
  if (top > 0 && right < field_width && top < field_height && right > 0)
    {
      Block *temp_block = block_at(block_palette, blocks, right,top);
      if (temp_block != NULL)
        {
          bool isThere = false;
          for (int i = 0; i < hitBlocks.size (); i++)
            {
              if (temp_block == hitBlocks[i])
                isThere = true;
            }
          if (!isThere)
            hitBlocks.push_back (temp_block);
        }
    }
  if (bottom < field_height && left > 0 && bottom > 0 && left < field_width)
    {
      Block *temp_block = block_at(block_palette, blocks, left,bottom);
      if (temp_block != NULL)
        {
          bool isThere = false;
          for (int i = 0; i < hitBlocks.size (); i++)
            {
              if (temp_block == hitBlocks[i])
                isThere = true;
            }
          if (!isThere)
            hitBlocks.push_back (temp_block);
        }
    }
  if (bottom < field_height && right < field_width && bottom > 0 && right > 0)
    {
      Block *temp_block = block_at(block_palette, blocks, right,bottom);
      if (temp_block != NULL)
        {
          bool isThere = false;
          for (int i = 0; i < hitBlocks.size (); i++)
            {
              if (temp_block == hitBlocks[i])
                isThere = true;
            }
          if (!isThere)
            hitBlocks.push_back (temp_block);
        }
    }

  for (int i = 0; i < hitBlocks.size (); i++)
    {
      alive = false;
      Laser::current = this;
      Block::current = hitBlocks[i];
      scm_c_eval_string (hitBlocks[i]->get_script ().c_str ());
    }
}
