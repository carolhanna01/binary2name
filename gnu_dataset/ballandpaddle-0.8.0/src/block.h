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

#ifndef __BLOCK_H__
#define __BLOCK_H__

#include <cstdlib>

#include "SDL.h"

#include "blocktype.h"

class Block
{
  int x;
  int y;
  int state;
  int strength;
  int maxStrength;
  SDL_Surface *imgBlocks;
  BlockType * type;
public:
    Block (int x, int y, BlockType * type, SDL_Surface * imgBlocks);
   ~Block ();
  static Block *current;
  void draw (SDL_Surface * dest);
  void update (int time);
  void setMaxStrength (int maxStrength);
  void setStrength (int strength);
  void setState (int state);
  string get_script () { return type->get_script (); }
  void set_type (BlockType *type);
  int get_x ()
  {
    return x;
  }
  int get_y ()
  {
    return y;
  }
  BlockType * get_type ()
  {
    return type;
  }
  int get_max_strength ()
  {
    return maxStrength;
  }
  int get_strength ()
  {
    return strength;
  }
  bool weaken ();
  bool get_invincible () { return type->get_invincible (); }
  void kill ()
  {
    state = -1;
  }
  bool is_alive ()
  {
    return state >= 0;
  }
};

#endif
