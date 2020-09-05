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

#include "gameover.h"
#include "mainmenu.h"

GameOver
  GameOver::m_instance;

extern FontManager *font_manager;
extern SoundManager *soundManager;
extern SettingsManager settings_manager;

void
GameOver::setLevelsetFilename (string levelsetFilename)
{
  this->levelsetFilename = levelsetFilename;
}

void
GameOver::init ()
{
  quit = false;
  popstate = false;
  changestate = NULL;
  pushstate = NULL;
  string location = LEVELPATH + "/" + levelsetFilename + "/ball.png";
  SDL_Surface *temp = IMG_Load (location.c_str ());
  if (temp != NULL)
    imgBall = SDL_DisplayFormat (temp);
  SDL_FreeSurface (temp);
  SDL_SetColorKey (imgBall, SDL_SRCCOLORKEY | SDL_RLEACCEL, 0x00ff00ff);

  imgSkin = NULL;
  location = LEVELPATH + "/" + levelsetFilename + "/gameover.png";
  temp = IMG_Load (location.c_str ());
  if (temp != NULL) imgSkin = SDL_DisplayFormat (temp);
  SDL_FreeSurface (temp);

  score_font_size = 24;
  score_label_font_size = 24;
  score_label_font_color.r = 255;
  score_label_font_color.g = 255;
  score_label_font_color.b = 255;
  score_font_color.r = 255;
  score_font_color.g = 255;
  score_font_color.b = 255;

  start ();
}

void
GameOver::setBallCount (int ball_count)
{
  m_balls_left = m_ball_count = ball_count;
  time = initial_time + ball_count * ball_time;
}

void
GameOver::cleanup ()
{
  SDL_FreeSurface (imgBall);
}

void
GameOver::pause ()
{
}

void
GameOver::resume ()
{
}

void
GameOver::loadHighScores ()
{
  scores = vector <int> ();
  names = vector <string> ();
  ifstream file;
#ifndef SINGLEDIR
  string location =
    string (getenv ("HOME")) + "/.ballandpaddle/" + levelsetFilename +
    "_scores";
#endif
#ifdef SINGLEDIR
  string location = levelsetFilename + "_scores";
#endif
  file.open (location.c_str (), ifstream::in);
  if (!file.fail ())
    {
      for (int i = 0; i < 5; i++)
	{
	  int buffer;
	  file >> buffer;
	  scores.push_back (buffer);
	}
      char buffer[50];
      // get garbage data on end of line (should be just a newline character)
      file.getline (buffer, 50);
      for (int i = 0; i < 5; i++)
	{
	  char strName[20];
	  file.getline (strName, 20);
	  names.push_back (string (strName));
	}
      file.close ();
    }
  else
    {
      for (int i = 0; i < 5; i++)
	scores.push_back (0);
      for (int i = 0; i < 5; i++)
	names.push_back ("0");
    }
}

void
GameOver::saveHighScores ()
{
  if (scores.size () < 5)
    {
      int size = scores.size ();
      for (int i = 0; i < 5 - size; i++)
	scores.push_back (0);
    }
  ofstream file;
#ifndef SINGLEDIR
  string location =
    string (getenv ("HOME")) + "/.ballandpaddle/" + levelsetFilename +
    "_scores";
#endif
#ifdef SINGLEDIR
  string location = levelsetFilename + "_scores";
#endif
  file.open (location.c_str ());
  for (int i = 0; i < 5; i++)
    {
      file << scores[i] << endl;
    }
  for (int i = 0; i < 5; i++)
    {
      file << names[i] << endl;
    }
  file.close ();
}

void
GameOver::endGame (bool won)
{
  loadHighScores ();
  scores.push_back (score);
  names.push_back ("");
  int scoreIndex = names.size () - 1;
  for (int i = 0; i < scores.size (); i++)
    {
      int highestIndex = i;
      for (int j = i; j < scores.size (); j++)
	if (scores[j] >= scores[highestIndex])
	  highestIndex = j;
      int temp = scores[i];
      string strTemp = names[i];
      scores[i] = scores[highestIndex];
      scores[highestIndex] = temp;
      names[i] = names[highestIndex];
      names[highestIndex] = strTemp;
      if (i == scoreIndex)
	scoreIndex = highestIndex;
      else if (highestIndex == scoreIndex)
	scoreIndex = i;
    }
  saveHighScores ();

  if (settings_manager.get_fullscreen ())
    SDL_ShowCursor (true);

  // go to high score menu
  changestate = HighScoreMenu::instance ();
  HighScoreMenu::instance ()->init ();
  HighScoreMenu::instance ()->set_levelset_filename (levelsetFilename);
  HighScoreMenu::instance ()->loadHighScores (levelsetFilename);

  // if it's in the top 5, setup name input
  if (scoreIndex < 5)
    {
      HighScoreMenu::instance ()->set_entering_highscore (true);
      HighScoreMenu::instance ()->set_score_index (scoreIndex);
    }
}

void
GameOver::start ()
{
  last_time = SDL_GetTicks ();
  currentTime = 0;
}

void
GameOver::handleEvents ()
{
  SDL_Event event;

  if (SDL_PollEvent (&event))
    {
      switch (event.type)
	{
	case SDL_QUIT:
          quit = true;
	  break;
	case SDL_KEYDOWN:
	  break;
	}
    }
}

void
GameOver::draw (SDL_Surface *dest)
{
  SDL_FillRect (dest, NULL,
		SDL_MapRGB (dest->format, 224, 224, 224));
  if (imgSkin != NULL)
    SDL_BlitSurface (imgSkin, NULL, dest, NULL);
  for (int i = 0; i < m_balls_left; i++)
    {
      SDL_Rect s = { 0, 0, ball_size, ball_size};
      SDL_Rect d = { balls_x + (ball_size + ball_padding) * i, balls_y, 0, 0 };
      SDL_BlitSurface (imgBall, &s, dest, &d);
    }
  stringstream ssScore (stringstream::in | stringstream::out);
  ssScore << score;
  string strScore;
  int scoreBuffer = 5 - ssScore.str ().length ();
  if (scoreBuffer < 0)
    scoreBuffer = 0;
  for (int i = 0; i < scoreBuffer; i++)
    strScore += ' ';
  strScore += ssScore.str ();
  font_manager->drawString ("FreeSans", score_font_size, "Score: ", score_label_x, score_label_y, dest, score_label_font_color);
  font_manager->drawString ("FreeSans", score_label_font_size, strScore.c_str (), score_x, score_y, dest, score_font_color);
}

void
GameOver::update ()
{
  long elapsed = SDL_GetTicks () - last_time;
  last_time = SDL_GetTicks ();
  currentTime += elapsed;
  int prev_balls_left = m_balls_left;
  if (currentTime < initial_time) return;
  m_balls_left = m_ball_count - (currentTime - initial_time) / ball_time;
  if (m_balls_left < 0)
    m_balls_left = 0;
  if (m_balls_left != prev_balls_left)
    {
      score += ball_score;
      soundManager->playSound ("bonustick");
    }
  if (currentTime >= time)
    endGame (true);
}

void
GameOver::set_score (int score)
{
  this->score = score;
}

void
GameOver::set_ball_size (int size)
{
  ball_size = size;
}

void
GameOver::set_ball_padding (int ball_padding)
{
  this->ball_padding = ball_padding;
}

void
GameOver::set_initial_time (int initial_time)
{
  this->initial_time = initial_time;
}

void
GameOver::set_ball_time (int ball_time)
{
  this->ball_time = ball_time;
}

void
GameOver::set_ball_score (int ball_score)
{
  this->ball_score = ball_score;
}

void
GameOver::set_balls_x (int balls_x)
{
  this->balls_x = balls_x;
}

void
GameOver::set_balls_y (int balls_y)
{
  this->balls_y = balls_y;
}

void
GameOver::set_score_label_x (int score_label_x)
{
  this->score_label_x = score_label_x;
}

void
GameOver::set_score_label_y (int score_label_y)
{
  this->score_label_y = score_label_y;
}

void
GameOver::set_score_x (int score_x)
{
  this->score_x = score_x;
}

void
GameOver::set_score_y (int score_y)
{
  this->score_y = score_y;
}

void
GameOver::set_score_font_size (int size)
{
  score_font_size = size;
}

void
GameOver::set_score_label_font_size (int size)
{
  score_label_font_size = size;
}

void
GameOver::set_score_font_color (int r, int g, int b)
{
  score_font_color.r = r;
  score_font_color.g = g;
  score_font_color.b = b;
}

void
GameOver::set_score_label_font_color (int r, int g, int b)
{
  score_label_font_color.r = r;
  score_label_font_color.g = g;
  score_label_font_color.b = b;
}

