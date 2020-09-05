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

#ifndef __GAMESTATE_H__
#define __GAMESTATE_H__

#include "SDL.h"

class GameState
{
public:
  virtual bool getTransitioning () = 0;
  virtual void init () = 0;
  virtual void cleanup () = 0;
  virtual void pause () = 0;
  virtual void resume () = 0;
  virtual void handleEvents () = 0;
  virtual void draw (SDL_Surface *screen) = 0;
  virtual void update () = 0;
  virtual GameState * get_pushstate ()
  {
    return pushstate;
  }
  virtual GameState * get_changestate ()
  {
    return changestate;
  }
  virtual bool get_popstate ()
  {
    return popstate;
  }
  virtual bool get_quit ()
  {
    return quit;
  }
protected:
    GameState ()
  {
  };
  bool quit;
  bool popstate;
  GameState *changestate;
  GameState *pushstate;
};

#endif
