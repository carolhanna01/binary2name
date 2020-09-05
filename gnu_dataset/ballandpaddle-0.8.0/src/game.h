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

#ifndef __GAME_H__
#define __GAME_H__

#include <vector>
#include <map>
#include <fstream>
#include <sstream>
using namespace std;

#include <libguile.h>

#include "SDL.h"
#include "SDL_image.h"

#include "gamestate.h"
#include "globals.h"
#include "ball.h"
#include "block.h"
#include "paddle.h"
#include "powerup.h"
#include "laser.h"
#include "highscoremenu.h"
#include "mainmenu.h"
#include "gameover.h"
#include "fontmanager.h"
#include "soundmanager.h"
#include "settingsmanager.h"

class Game:public GameState
{

private:
  long last_time;
  long time;
  SDL_Surface * imgBlocks;
  SDL_Surface *imgBall;
  SDL_Surface *imgPowerUps;
  SDL_Surface *imgBackground;
  SDL_Surface *imgLaser;
  SDL_Surface *imgPaddle;
  SDL_Surface *imgSkin;
  SDL_Surface *imgBonus;
    vector < Ball * >balls;
    vector < Block * >blocks;
    map <string, BlockType * >block_palette;
  Paddle *paddle;
    vector < PowerUp * >powerUps;
    map <string, PowerUpType * >powerup_palette;
    vector < Laser * >lasers;
  int timeTilReady;
  bool powerBalls;
  int level;
  int oldScore;
  int bonusScore;
  int score;
  bool reverse;
  bool m_paused;
  bool m_gameover;
  bool m_transitioning;
  bool tickingBonusTime;
  int bonus_time;
  int extraBallCount;
  int ball_size;
  int powerup_width;
  int powerup_height;
  string levelsetName;
  string levelsetFilename;
    vector < int >scores;
    vector < string > names;
  void drawBox (SDL_Surface * dest, int x, int y, int width, int height,
		Uint32 inside, Uint32 outside);

  bool m_started;

  bool initialized_guile;

  int field_width;
  int field_height;
  int field_x;
  int field_y;

  int ball_padding;
  int balls_x;
  int balls_y;

  string score_label_font_face;
  int score_label_font_size;
  SDL_Color score_label_font_color;

  int score_label_x;
  int score_label_y;

  string score_font_face;
  int score_font_size;
  SDL_Color score_font_color;

  int score_x;
  int score_y;

  string level_label_font_face;
  int level_label_font_size;
  SDL_Color level_label_font_color;

  int level_label_x;
  int level_label_y;

  string level_font_face;
  int level_font_size;
  SDL_Color level_font_color;

  int level_x;
  int level_y;

  int bonus_timer_x;
  int bonus_timer_y;
  int bonus_timer_width;
  int bonus_timer_height;
  int bonus_timer_source_x;
  int bonus_timer_source_y;
  int bonus_timer_source_width;
  int bonus_timer_source_height;
  int bonus_timer_start_x;
  int bonus_timer_start_y;
  int bonus_timer_start_width;
  int bonus_timer_start_height;
  int bonus_timer_end_x;
  int bonus_timer_end_y;
  int bonus_timer_end_width;
  int bonus_timer_end_height;

  string gameover_score_font_face;
  int gameover_score_font_size;
  SDL_Color gameover_score_font_color;
  string gameover_score_label_font_face;
  SDL_Color gameover_score_label_font_color;
  int gameover_score_label_font_size;
  int gameover_score_label_x;
  int gameover_score_label_y;
  int gameover_score_x;
  int gameover_score_y;
  int gameover_ball_score;
  int gameover_ball_time;
  int gameover_initial_time;
  int gameover_ball_padding;
  int gameover_balls_x;
  int gameover_balls_y;

  static Game m_instance;

public:
  static Game *instance ()
  {
    return &m_instance;
  }
  void game_over ();
  void init ();
  bool guile_init ();
  void cleanup ();
  void pause ();
  void resume ();
  void handleEvents ();
  void update ();
  void draw (SDL_Surface *dest);
  bool loadLevel (int level);
  void loadImages ();
  void handlePowerUp (int type);
  bool getTransitioning ()
  {
    return m_transitioning;
  }
  map < string, BlockType * > * get_block_palette () { return &block_palette; }
  map < string, PowerUpType * > * get_powerup_palette () { return &powerup_palette; }
  vector < Block * > * get_blocks () { return &blocks; }
  vector < Ball * > * get_balls () { return &balls; }
  vector < PowerUp * > * get_powerups () { return &powerUps; }
  void add_powerup (int x, int y, string type);
  void add_powerup_type (string name, int width, int height, int sx, int sy, 
                         string script);
  void add_block (int x, int y, string type);
  void add_block_type (string name, int width, int height, int sx, int sy, 
                       bool invincible, string script);
  void set_levelset_filename (string levelset_filename);
  void set_levelset_name (string levelset_name);
  string get_levelset_filename () { return levelsetFilename; }
  string get_levelset_name () { return levelsetName; }
  void set_ball_size (int size);
  void set_powerup_width (int powerup_width);
  void set_powerup_height (int powerup_height);
  void set_bonus_time (int bonus_time);
  void set_skin (string filename);
  void set_background (string filename);
  void set_level (int level);
  void set_field_width (int width);
  void set_field_height (int height);
  void set_field_x (int x);
  void set_field_y (int y);
  void set_bonus_timer_rect (int x, int y, int width, int height);
  void set_bonus_timer_source (int x, int y, int width, int height);
  void set_bonus_timer_start (int x, int y, int width, int height);
  void set_bonus_timer_end (int x, int y, int width, int height);
  void add_score (int amount);
  int get_ball_size () { return ball_size; }
  int get_powerup_width () { return powerup_width; }
  int get_powerup_height () { return powerup_height; }
  void set_gameover_initial_time (int gameover_initial_time);
  void set_gameover_ball_time (int gameover_ball_time);
  void set_gameover_ball_score (int gameover_ball_score);
  void set_gameover_ball_padding (int gameover_ball_padding);
  void set_gameover_balls_x (int gameover_balls_x);
  void set_gameover_balls_y (int gameover_balls_y);
  void set_gameover_score_x (int gameover_score_x);
  void set_gameover_score_y (int gameover_score_y);
  void set_gameover_score_label_x (int gameover_score_label_x);
  void set_gameover_score_label_y (int gameover_score_label_y);
  void set_ball_padding (int ball_padding);
  void set_balls_x (int balls_x);
  void set_balls_y (int balls_y);
  void set_score_x (int score_x);
  void set_score_y (int score_y);
  void set_score_label_x (int score_label_x);
  void set_score_label_y (int score_label_y);
  void set_level_x (int level_x);
  void set_level_y (int level_y);
  void set_level_label_x (int level_label_x);
  void set_level_label_y (int level_label_y);
  void set_level_font_face (string face);
  void set_level_font_size (int size);
  void set_level_font_color (int r, int g, int b);
  void set_score_font_face (string face);
  void set_score_font_size (int size);
  void set_score_font_color (int r, int g, int b);
  void set_level_label_font_face (string face);
  void set_level_label_font_size (int size);
  void set_level_label_font_color (int r, int g, int b);
  void set_score_label_font_face (string face);
  void set_score_label_font_size (int size);
  void set_score_label_font_color (int r, int g, int b);
  void set_gameover_score_label_font_face (string face);
  void set_gameover_score_label_font_size (int size);
  void set_gameover_score_label_font_color (int r, int g, int b);
  void set_gameover_score_font_face (string face);
  void set_gameover_score_font_size (int size);
  void set_gameover_score_font_color (int r, int g, int b);
};

#endif
