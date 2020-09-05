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

#ifndef __GAMESTATEMANAGER_H__
#define __GAMESTATEMANAGER_H__


#include <string>
#include <vector>
#include <iostream>
#include <fstream>
using namespace std;

#include <sys/stat.h>

#include "SDL.h"
#include "SDL_image.h"
#include "SDL_mixer.h"

#include "settingsmanager.h"
#include "gamestate.h"
#include "globals.h"
#include "game.h"

class GameState;

class GameStateManager
{
  int maxFPS;
  int m_time;
  long m_lastTime;
  bool m_fullscreen;
  bool m_playSounds;
  bool m_running;
  bool m_enteringHighScore;
  int m_startLevel;
  string m_strSkinName;
  string m_strSkinFilename;
  string m_strLevelsetName;
  string m_strLevelsetFilename;
  int m_scoreIndex;
  int m_score;
  vector < GameState * >states;
public:
  void changeState (GameState * state);
  void pushState (GameState * state);
  void popState ();
  void init ();
  void initVideo ();
  void toggleFullscreen ();
  void toggleSounds ()
  {
    m_playSounds = !m_playSounds;
  }
  void cleanup ();
  void handleEvents ();
  void update ();
  void draw ();
  void quit ();

  bool running ()
  {
    return m_running;
  }

  SDL_Surface *screen;

  SDL_Surface *imgSkin;

  int getTime ()
  {
    return m_time;
  }

  bool isFullscreen ()
  {
    return m_fullscreen;
  }
  bool playsSounds ()
  {
    return m_playSounds;
  }

  string getSkinFilename ()
  {
    return m_strSkinFilename;
  }
  void setSkin (string skinName, string skinFilename);

  void loadSettings ();
  void saveSettings ();
  void loadSkin ();
  void setLevelset (string levelsetName, string levelsetFilename);
  void setStartLevel (int startLevel);
  string getLevelsetName ()
  {
    return m_strLevelsetName;
  }
  string getLevelsetFilename ()
  {
    return m_strLevelsetFilename;
  }
  int getStartLevel ()
  {
    return m_startLevel;
  }
  bool isEnteringHighScore ()
  {
    return m_enteringHighScore;
  }
  void setEnteringHighScore (bool enteringHighScore);
  void setScoreIndex (int scoreIndex);
  void setScore (int score);
  int getScoreIndex ()
  {
    return m_scoreIndex;
  }
  int getScore ()
  {
    return m_score;
  }
  void loadImages ();
  void level_add_block (int x, int y, int type);
};

#endif
