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

#include "utilities.h"

Block *
block_at (map < string, BlockType * > block_palette, vector < Block * >blocks, 
                int x, int y)
{
  for (int i = 0; i < blocks.size(); i++)
    {
      if (blocks[i]->get_x () <= x
          && blocks[i]->get_x () 
             + blocks[i]->get_type ()->get_width () > x
          && blocks[i]->get_y () <= y
          && blocks[i]->get_y () 
             + blocks[i]->get_type ()->get_height () > y)
        {
          return blocks[i];
        }
    }
  return NULL;
}

Block *
strong_block_at (map < string, BlockType * >block_palette, vector < Block * >blocks, int x, int y)
{
  for (int i = 0; i < blocks.size(); i++)
    {
      if (blocks[i]->get_x () <= x
          && blocks[i]->get_x () 
             + blocks[i]->get_type ()->get_width () > x
          && blocks[i]->get_y () <= y
          && blocks[i]->get_y () 
             + blocks[i]->get_type ()->get_height () > y
          && blocks[i]->get_type ()->get_invincible ())
        {
          return blocks[i];
        }
    }
  return NULL;
}

