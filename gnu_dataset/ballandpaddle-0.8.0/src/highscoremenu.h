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

#ifndef __HIGHSCOREMENU_H__
#define __HIGHSCOREMENU_H__

#include <vector>
#include <iostream>
#include <sstream>
#include <fstream>
using namespace std;

#include "SDL.h"
#include "SDL_image.h"

#include "globals.h"
#include "gamestate.h"
#include "fontmanager.h"

class HighScoreMenu:public GameState
{
  int cursor;
    vector < string > menuItems;
  SDL_Surface *imgBackground;
  int spacing;

  void handleItem ();

    vector < string > levelsetNames;
    vector < string > levelsetFilenames;
    vector < int >scores;
    vector < string > names;
  int score_index;
  bool entering_highscore;
  bool m_shift;
  int curr_levelset_index;
  int levelset_count;

  static HighScoreMenu m_instance;

public:
  static HighScoreMenu *instance ()
  {
    return &m_instance;
  }
  void init ();
  void cleanup ();
  void pause ();
  void resume ();
  void handleEvents ();
  void update ();
  void draw (SDL_Surface *dest);
  void loadHighScores ();
  void loadHighScores (string filename);
  void saveHighScores ();
  void set_score_index (int score_index);
  void set_levelset_filename (string filename);
  void set_entering_highscore (bool entering_high_score);
  bool getTransitioning ()
  {
    return false;
  }
protected:
  HighScoreMenu ()
  {
  }
};

#endif
