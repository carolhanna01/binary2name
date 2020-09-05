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

#include "gamestatemanager.h"

extern bool fullscreen;

extern SettingsManager settings_manager;

void
GameStateManager::init ()
{

  maxFPS = 100;

  if (SDL_Init (SDL_INIT_VIDEO | SDL_INIT_AUDIO) < 0)
    {
      printf ("Unable to initialize SDL: %s\n", SDL_GetError ());
      exit (1);
    }


  settings_manager.load_settings ();

  initVideo ();

  loadSkin ();

  m_running = true;

  m_lastTime = SDL_GetTicks ();

  srand (SDL_GetTicks ());
  for (int i = 0; i < 5; i++)
    srand (rand () + rand ());
}

void
GameStateManager::loadImages ()
{
  Game::instance ()->loadImages();
}

void
GameStateManager::initVideo ()
{
  if (settings_manager.get_fullscreen ())
    {
      screen = SDL_SetVideoMode (640, 480, 0, SDL_DOUBLEBUF | SDL_FULLSCREEN);
      if (screen == NULL)
	{
	  printf ("Unable to set 640x480 video: %s\n", SDL_GetError ());
	  settings_manager.set_fullscreen (false);
	  initVideo ();
	}
    }
  else
    {
      screen = SDL_SetVideoMode (640, 480, 0, SDL_DOUBLEBUF);
      if (screen == NULL)
	{
	  printf ("Unable to set video mode: %s\n", SDL_GetError ());
	  SDL_Quit ();
	  return;
	}
    }
}

void
GameStateManager::toggleFullscreen ()
{
  m_fullscreen = !m_fullscreen;
  initVideo ();
}

void
GameStateManager::setSkin (string skinName, string skinFilename)
{
  m_strSkinName = skinName;
  m_strSkinFilename = skinFilename;
}

void
GameStateManager::setLevelset (string levelsetName, string levelsetFilename)
{
  m_strLevelsetName = levelsetName;
  m_strLevelsetFilename = levelsetFilename;
}

void
GameStateManager::setStartLevel (int startLevel)
{
  m_startLevel = startLevel;
}

void
GameStateManager::setEnteringHighScore (bool enteringHighScore)
{
  m_enteringHighScore = enteringHighScore;
}

void
GameStateManager::setScoreIndex (int scoreIndex)
{
  m_scoreIndex = scoreIndex;
}

void
GameStateManager::setScore (int score)
{
  m_score = score;
}

void
GameStateManager::loadSkin ()
{
  SDL_FreeSurface (imgSkin);
  string location = IMAGEPATH + "/" + m_strSkinFilename;
  SDL_Surface *temp = IMG_Load (location.c_str ());
  if (temp != NULL)
    imgSkin = SDL_DisplayFormat (temp);
  SDL_FreeSurface (temp);
  if (imgSkin != NULL)
    SDL_SetColorKey (imgSkin, SDL_SRCCOLORKEY | SDL_RLEACCEL, 0x00ff00ff);
}

void
GameStateManager::loadSettings ()
{
}

void
GameStateManager::saveSettings ()
{
}

void
GameStateManager::cleanup ()
{
  Mix_HaltChannel (-1);
  Mix_CloseAudio ();
  SDL_Quit ();
}

void
GameStateManager::pushState (GameState * state)
{
  if (!states.empty ())
    {
      states.back ()->pause ();
    }
  states.push_back (state);
  states.back ()->init ();
}

void
GameStateManager::changeState (GameState * state)
{
  if (!states.empty ())
    {
      states.back ()->cleanup ();
      states.pop_back ();
    }

  states.push_back (state);
  states.back ()->init ();
}

void
GameStateManager::popState ()
{
  states.back ()->cleanup ();
  states.pop_back ();
  if (!states.empty ())
    {
      states.back ()->resume ();
    }
}

void
GameStateManager::handleEvents ()
{
  if (states.back ()->get_quit ()) return;

  states.back ()->handleEvents ();

  if (states.back ()->get_quit ())
  {
    m_running = false;
    return;
  }

  GameState *game_state = states.back ()->get_changestate ();

  if (game_state != NULL)
    changeState (game_state);

  game_state = states.back ()->get_pushstate ();

  if (game_state != NULL)
    pushState (game_state);

  if (states.back ()->get_popstate ())
    popState ();
}

void
GameStateManager::update ()
{
  if (states.back ()->get_quit ()) return;
  if (SDL_GetTicks () - m_lastTime < 1000 / maxFPS)
    {
      SDL_Delay (m_lastTime + 1000 / maxFPS - SDL_GetTicks ());
    }
  long now = SDL_GetTicks ();
  m_time = now - m_lastTime;
  m_lastTime = now;
  states.back ()->update ();
  if (states.back ()->get_quit ())
  {
    m_running = false;
    return;
  }
  GameState *game_state = states.back ()->get_changestate ();
  if (game_state != NULL)
    changeState (game_state);
  game_state = states.back ()->get_pushstate ();
  if (game_state != NULL)
    pushState (game_state);
  if (states.back ()->get_popstate ())
    popState ();
}

void
GameStateManager::draw ()
{
  if (states.back ()->get_quit ()) return;
  if (!states.back ()->getTransitioning ())
    {
      states.back ()->draw (screen);
      SDL_Flip (screen);
    }
}

void
GameStateManager::quit ()
{
  m_running = false;
}

void
GameStateManager::level_add_block (int x, int y, int type)
{
}

