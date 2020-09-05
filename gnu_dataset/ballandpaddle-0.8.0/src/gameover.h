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

#ifndef GAMEOVER_H
#define GAMEOVER_H

#include <vector>
#include <iostream>
#include <sstream>
using namespace std;

#include "SDL.h"
#include "SDL_image.h"

#include "gamestate.h"
#include "globals.h"
#include "fontmanager.h"
#include "soundmanager.h"
#include "settingsmanager.h"
#include "highscoremenu.h"

class GameOver:public GameState
{

private:
  long last_time;
  static GameOver m_instance;
  SDL_Surface *imgBall;
  SDL_Surface *imgSkin;
  long time;
  long currentTime;
  int m_ball_count;
  int m_balls_left;
  int ball_size;
  string levelsetFilename;
  int score;
    vector < int >scores;
    vector < string > names;
  void endGame (bool won);
  int ball_score;
  int ball_time;
  int ball_padding;
  int initial_time;
  int balls_x;
  int balls_y;
  int score_label_x;
  int score_label_y;
  int score_x;
  int score_y;
  int score_font_size;
  int score_label_font_size;
  SDL_Color score_label_font_color;
  SDL_Color score_font_color;
public:
  static GameOver *instance ()
  {
    return &m_instance;
  }
  void init ();
  void cleanup ();
  void pause ();
  void resume ();
  void handleEvents ();
  void draw (SDL_Surface *dest);
  void update ();
  void start ();
  bool done ()
  {
    return currentTime >= time;
  }
  void setBallCount (int ball_count);
  void setLevelsetFilename (string levelsetFilename);
  void loadHighScores ();
  void saveHighScores ();
  bool getTransitioning ()
  {
    return false;
  }
  void set_score (int score);
  void set_ball_size (int size);
  void set_ball_score (int ball_score);
  void set_ball_time (int ball_time);
  void set_ball_padding (int ball_padding);
  void set_initial_time (int initial_time);
  void set_balls_x (int balls_x);
  void set_balls_y (int balls_y);
  void set_score_label_x (int score_label_x);
  void set_score_label_y (int score_label_y);
  void set_score_label_font_size (int size);
  void set_score_font_size (int size);
  void set_score_label_font_color (int r, int g, int b);
  void set_score_font_color (int r, int g, int b);
  void set_score_x (int score_x);
  void set_score_y (int score_y);
protected:
  GameOver ()
  {
  };
};

#endif
