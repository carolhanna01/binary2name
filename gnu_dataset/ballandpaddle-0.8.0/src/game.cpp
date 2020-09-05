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

#include "game.h"

Game Game::m_instance;

extern FontManager *
  font_manager;
extern SoundManager *
  soundManager;
extern SettingsManager
  settings_manager;
/*
extern void
level_add_block (int x, int y, int type);
*/

void
Game::init ()
{
  quit = false;
  popstate = false;
  pushstate = NULL;
  changestate = NULL;
  paddle = NULL;

  score = 0;

  m_paused = false;

  m_gameover = false;
  m_transitioning = false;

  extraBallCount = 5;

  level = 1;

  powerBalls = false;

  m_started = false;

  imgSkin = NULL;

  balls_x = 15;
  balls_y = 427;
  ball_padding = 5;

  level_label_font_face = "FreeSans";
  level_label_font_size = 24;
  level_label_font_color.r = 255;
  level_label_font_color.g = 255;
  level_label_font_color.b = 255;
  level_font_face = "FreeSans";
  level_font_size = 24;
  level_font_color.r = 255;
  level_font_color.g = 255;
  level_font_color.b = 255;
  score_label_font_face = "FreeSans";
  score_label_font_size = 24;
  score_label_font_color.r = 255;
  score_label_font_color.g = 255;
  score_label_font_color.b = 255;
  score_font_face = "FreeSans";
  score_font_size = 24;
  score_font_color.r = 255;
  score_font_color.g = 255;
  score_font_color.b = 255;
  gameover_initial_time = 1000;
  gameover_ball_time = 500;
  gameover_ball_score = 500;
  gameover_balls_x = 5;
  gameover_balls_y = 5;
  gameover_ball_padding = 5;
  gameover_score_label_x = 534;
  gameover_score_label_y = 10;
  gameover_score_x = 546;
  gameover_score_y = 39;
  gameover_score_font_face = "FreeSans";
  gameover_score_font_size = 24;
  gameover_score_font_color.b = 255;
  gameover_score_font_color.r = 255;
  gameover_score_font_color.g = 255;
  gameover_score_label_font_face = "FreeSans";
  gameover_score_label_font_size = 24;
  gameover_score_label_font_color.r = 255;
  gameover_score_label_font_color.g = 255;
  gameover_score_label_font_color.b = 255;
  score_label_x = 534;
  score_label_y = 10;
  score_x = 546;
  score_y = 39;
  level_label_x = 534;
  level_label_y = 72;
  level_x = 546;
  level_y = 101;

  last_time = SDL_GetTicks ();

  block_palette = map <string, BlockType *> ();

  powerup_palette = map <string, PowerUpType *> ();

  initialized_guile = false;
}

bool
Game::guile_init ()
{
  stringstream ss (stringstream::in | stringstream::out);
  ss << LEVELPATH << "/" << levelsetFilename;
  ss << "/initialize.scm";
  ifstream
  file (ss.str ().c_str ());
  if (!file.is_open ())
    {
      file.close ();
      return false;
    }
  file.close ();
  scm_c_primitive_load (ss.str ().c_str ());
  font_manager->initialize_font (gameover_score_font_face, gameover_score_font_size);
  font_manager->initialize_font (gameover_score_label_font_face, gameover_score_label_font_size);
  font_manager->initialize_font (level_label_font_face, level_label_font_size);
  font_manager->initialize_font (level_font_face, level_font_size);
  font_manager->initialize_font (score_label_font_face, score_label_font_size);
  font_manager->initialize_font (score_font_face, score_font_size);
  initialized_guile = true;
  return true;
}

void
Game::cleanup ()
{
  soundManager->stopSound ();
  SDL_FreeSurface (imgBlocks);
  SDL_FreeSurface (imgBall);
  SDL_FreeSurface (imgLaser);
  SDL_FreeSurface (imgPowerUps);
}

void
Game::pause ()
{
  m_paused = true;
}

void
Game::resume ()
{
  m_paused = false;
}

void
Game::drawBox (SDL_Surface * dest, int x, int y, int width, int height,
	       Uint32 inside, Uint32 outside)
{
  SDL_Rect d = { x + 1, y + 1, width - 2, height - 2 };
  SDL_FillRect (dest, &d, inside);
  d.x = x;
  d.y = y;
  d.w = 1;
  d.h = height;
  SDL_FillRect (dest, &d, outside);
  d.x = x + width - 1;
  SDL_FillRect (dest, &d, outside);
  d.x = x;
  d.w = width;
  d.h = 1;
  SDL_FillRect (dest, &d, outside);
  d.y = y + height - 1;
  SDL_FillRect (dest, &d, outside);
}

void
Game::loadImages ()
{
  string location = LEVELPATH + "/" + levelsetFilename + "/bonus.png";
  SDL_Surface *temp = IMG_Load (location.c_str ());
  if (temp != NULL)
    imgBonus = SDL_DisplayFormat (temp);
  SDL_FreeSurface (temp);
  SDL_SetColorKey (imgBonus, SDL_SRCCOLORKEY | SDL_RLEACCEL, 0x00ff00ff);

  location = LEVELPATH + "/" + levelsetFilename + "/ball.png";
  temp = IMG_Load (location.c_str ());
  if (temp != NULL)
    imgBall = SDL_DisplayFormat (temp);
  SDL_FreeSurface (temp);
  SDL_SetColorKey (imgBall, SDL_SRCCOLORKEY | SDL_RLEACCEL, 0x00ff00ff);

  location = LEVELPATH + "/" + levelsetFilename + "/paddle.png";
  temp = IMG_Load (location.c_str ());
  if (temp != NULL)
    imgPaddle = SDL_DisplayFormat (temp);
  SDL_FreeSurface (temp);
  SDL_SetColorKey (imgPaddle, SDL_SRCCOLORKEY | SDL_RLEACCEL, 0x00ff00ff);

  location = LEVELPATH + "/" + levelsetFilename + "/laser.png";
  temp = IMG_Load (location.c_str ());
  if (temp != NULL)
    imgLaser = SDL_DisplayFormat (temp);
  SDL_FreeSurface (temp);
  SDL_SetColorKey (imgLaser, SDL_SRCCOLORKEY | SDL_RLEACCEL, 0x00ff00ff);

  location = LEVELPATH + "/" + levelsetFilename + "/blocks.png";
  temp = IMG_Load (location.c_str ());
  if (temp != NULL)
    imgBlocks = SDL_DisplayFormat (temp);
  SDL_FreeSurface (temp);

  location = LEVELPATH + "/" + levelsetFilename + "/powerups.png";
  temp = IMG_Load (location.c_str ());
  if (temp != NULL)
    imgPowerUps = SDL_DisplayFormat (temp);
  SDL_FreeSurface (temp);
  SDL_SetColorKey (imgPowerUps, SDL_SRCCOLORKEY | SDL_RLEACCEL, 0x00ff00ff);
}

bool Game::loadLevel (int level)
{
  blocks = vector < Block * >();
  stringstream
  ss (stringstream::in | stringstream::out);
  ss.str ("");
  ss << LEVELPATH << "/" << levelsetFilename;
  ss << "/level";
  ss << level;
  ss << ".scm";
  ifstream
  file (ss.str ().c_str ());
  file.open (ss.str ().c_str ());
  if (!file.is_open ())
    {
      file.close ();
      return false;
    }
  file.close ();
  scm_c_primitive_load (ss.str ().c_str ());

  balls = vector < Ball * >();

  double
    direction =
    PI / 4 + PI / 2 * ((double) rand () / ((double) (RAND_MAX) + (double) 1));
  balls.push_back (new Ball (251, 250, direction, ball_size, 
                             field_width, field_height, imgBall));

  powerBalls = false;
  reverse = false;

  lasers = vector < Laser * >();

  if (paddle != NULL)
    delete
      paddle;
  paddle = new Paddle (imgPaddle, field_width, field_height);

  powerUps = vector < PowerUp * >();

  timeTilReady = 2700;

  tickingBonusTime = false;

  return true;
}

void
Game::draw (SDL_Surface * dest)
{
  SDL_FillRect (dest, NULL, SDL_MapRGB (dest->format, 255, 255, 255));

  if (imgSkin != NULL)
    SDL_BlitSurface (imgSkin, NULL, dest, NULL);

  SDL_Surface *buffer =
    SDL_CreateRGBSurface (SDL_HWSURFACE, field_width, field_height, 32, 0, 0, 0, 0);

  SDL_BlitSurface (imgBackground, NULL, buffer, NULL);

  for (int i = 0; i < lasers.size (); i++)
    lasers[i]->draw (buffer);

  if (paddle != NULL)
    paddle->draw (buffer);

  for (int i = 0; i < balls.size (); i++)
    balls[i]->draw (buffer, powerBalls);

  for (int i = 0; i < powerUps.size (); i++)
    powerUps[i]->draw (buffer);

  for (int i = 0; i < blocks.size (); i++)
    blocks[i]->draw (buffer);

  SDL_Color readycolor = {255, 255, 255};

  if (timeTilReady > 0)
    {
      drawBox (buffer, 120, 200, 250, 40,
	       SDL_MapRGB (buffer->format, 64, 64, 64),
	       SDL_MapRGB (buffer->format, 0, 0, 0));
      font_manager->drawString ("FreeSans", 16, "Get Ready", 150, 205, buffer, readycolor);
      if (timeTilReady < 1800)
	font_manager->drawString ("FreeSans", 16, ".", 294, 200, buffer, readycolor);
      if (timeTilReady < 1200)
	font_manager->drawString ("FreeSans", 16, ".", 310, 200, buffer, readycolor);
      if (timeTilReady < 600)
	font_manager->drawString ("FreeSans", 16, ".", 326, 200, buffer, readycolor);
    }

  if (m_paused)
    {
      drawBox (buffer, 180, 200, 130, 40,
	       SDL_MapRGB (buffer->format, 64, 64, 64),
	       SDL_MapRGB (buffer->format, 0, 0, 0));
      font_manager->drawString ("FreeSans", 16, "Paused", 190, 205, buffer, readycolor);
    }

  SDL_Rect d = { field_x, field_y, 0, 0 };
  SDL_BlitSurface (buffer, NULL, dest, &d);
  SDL_FreeSurface (buffer);

  // draw row of extra balls
  SDL_Rect ballRect = { balls_x, balls_y, 0, 0 };
  SDL_Rect s = { 0, 0, ball_size, ball_size };
  for (int i = 0; i < extraBallCount; i++)
    {
      ballRect.x = balls_x + i * (ball_size + ball_padding);
      SDL_BlitSurface (imgBall, &s, dest, &ballRect);
    }

  // draw the row of bonus time
  SDL_Surface *bonus_surface 
     = SDL_CreateRGBSurface (SDL_HWSURFACE, 
                             (bonus_timer_width - bonus_timer_start_width - bonus_timer_end_width) * bonus_time / 100000, 
                             bonus_timer_height, 32, 0, 0, 0, 0);
  SDL_Rect bonus_timer_source_rect = {bonus_timer_source_x,
                                      bonus_timer_source_y, 
                                      bonus_timer_source_width,
                                      bonus_timer_source_height};
  SDL_Rect bonus_rect = {0, 0, 0, 0};
  for (int i = 0; i < bonus_surface->w; i += bonus_timer_source_width)
    {
      bonus_rect.x = i;
      SDL_BlitSurface (imgBonus, &bonus_timer_source_rect, 
                       bonus_surface, &bonus_rect);
    }
  SDL_Rect bonus_dest_rect = {bonus_timer_x + bonus_timer_start_width, bonus_timer_y, 0, 0};
  SDL_BlitSurface (bonus_surface, NULL, dest, &bonus_dest_rect);
  bonus_rect.x = bonus_timer_x;
  bonus_rect.y = bonus_timer_y;
  // draw the first end (start)
  SDL_Rect bonus_timer_start_rect = {bonus_timer_start_x,
                                     bonus_timer_start_y,
                                     bonus_timer_start_width,
                                     bonus_timer_start_height};
  SDL_BlitSurface (imgBonus, &bonus_timer_start_rect, 
                   dest, &bonus_rect);
  bonus_rect.x = bonus_timer_x + bonus_timer_start_width + bonus_surface->w;
  // draw the last end (end)
  SDL_Rect bonus_timer_end_rect = {bonus_timer_end_x,
                                   bonus_timer_end_y,
                                   bonus_timer_end_width,
                                   bonus_timer_end_height};
  SDL_BlitSurface (imgBonus, &bonus_timer_end_rect, 
                   dest, &bonus_rect);

  // draw the level number
  stringstream ssLevel (stringstream::in | stringstream::out);
  ssLevel << level;
  string strLevel;
  int levelBuffer = 5 - ssLevel.str ().length ();
  if (levelBuffer < 0)
    levelBuffer = 0;
  for (int i = 0; i < levelBuffer; i++)
    strLevel += ' ';
  strLevel += ssLevel.str ();
  font_manager->drawString ("FreeSans", level_label_font_size, "Level: ", level_label_x, level_label_y, dest, level_label_font_color);
  font_manager->drawString ("FreeSans", level_font_size, strLevel.c_str (), level_x, level_y, dest, level_font_color);

  // draw the score
  stringstream ssScore (stringstream::in | stringstream::out);
  ssScore << score;
  string strScore;
  int scoreBuffer = 5 - ssScore.str ().length ();
  if (scoreBuffer < 0)
    scoreBuffer = 0;
  for (int i = 0; i < scoreBuffer; i++)
    strScore += ' ';
  strScore += ssScore.str ();
  font_manager->drawString ("FreeSans", score_label_font_size, "Score: ", score_label_x, score_label_y, dest, score_label_font_color);
  font_manager->drawString ("FreeSans", score_font_size, strScore.c_str (), score_x, score_y, dest, score_font_color);
}

void
Game::game_over ()
{
  m_gameover = true;
  m_transitioning = true;
  GameOver *gameover = GameOver::instance ();
  gameover->setLevelsetFilename (levelsetFilename);
  gameover->init ();
  gameover->set_ball_score (gameover_ball_score);
  gameover->set_ball_time (gameover_ball_time);
  gameover->set_ball_padding (gameover_ball_padding);
  gameover->set_initial_time (gameover_initial_time);
  gameover->set_balls_x (gameover_balls_x);
  gameover->set_balls_y (gameover_balls_y);
  gameover->set_score_label_x (gameover_score_label_x);
  gameover->set_score_label_y (gameover_score_label_y);
  gameover->set_score_label_font_size (gameover_score_label_font_size);
  gameover->set_score_font_size (gameover_score_font_size);
  gameover->set_score_label_font_color (gameover_score_label_font_color.r,
                                        gameover_score_label_font_color.g,
                                        gameover_score_label_font_color.b);
  gameover->set_score_font_color (gameover_score_font_color.r, 
                                  gameover_score_font_color.g,
                                  gameover_score_font_color.b);
  gameover->set_score_x (gameover_score_x);
  gameover->set_score_y (gameover_score_y);
  gameover->set_score (score);
  gameover->setBallCount (extraBallCount);
  gameover->set_ball_size (ball_size);
  changestate = gameover;
}

void
Game::update ()
{
  if (!m_started)
    {
      loadImages ();

      if (!initialized_guile)
        {
          if (!guile_init ())
            {
              game_over ();
              return;
            }
        }

      if (settings_manager.get_fullscreen ())
        {
	  SDL_WarpMouse (field_x + field_width / 2, 0);
        }

      // load the level
      if (!loadLevel (level))
	{
          game_over ();
	  return;
	}
      m_started = true;
    }

  time = SDL_GetTicks () - last_time;
  last_time = SDL_GetTicks ();

  // if ticking away bonus after level is over
  if (tickingBonusTime)
    {
      int prevScore = score;
      bonus_time -= time * 8;
      bonusScore += time;
      if (bonus_time < 0)
	{
	  // stop the ticking
	  soundManager->stopSound ();
	  // subtract back the overlapped time lost to the score
	  bonusScore += bonus_time / 8;
          // reset bonus time so that it draws correctly
          bonus_time = 0;
	  // move to next level
	  level++;
	  // reset mouse and level
	  m_started = false;
	  // reset the ticking bonus meter
	  tickingBonusTime = false;
	}
      score = oldScore + bonusScore / 4;
      if (score != 0 &&
	  (score / EXTRA_BALL_SCORE) > (prevScore / EXTRA_BALL_SCORE))
	extraBallCount++;
      return;
    }

  // do nothing if paused
  if (m_paused)
    return;

  // update the paddle
  paddle->update (time);

  // if waiting for ready, tick away
  if (timeTilReady > 0)
    {
      timeTilReady -= time;
      return;
    }

  // seed the random number generator for anything random during this frame
  srand (SDL_GetTicks ());

  // update the bonus time meter
  if (bonus_time > 0)
    bonus_time -= time;

  // update all balls
  for (int i = 0; i < balls.size (); i++)
    {
      int prevScore = score;
      balls[i]->update (time, block_palette, blocks, paddle, powerUps,
			powerBalls, score);
      if (score != prevScore
	  && (score / EXTRA_BALL_SCORE) > (prevScore / EXTRA_BALL_SCORE))
	extraBallCount++;
      if (balls[i]->getY () >= field_height)
	{
	  delete balls[i];
	  balls.erase (balls.begin () + i);
	  i--;
	}
    }
  // update all lasers
  for (int i = 0; i < lasers.size (); i++)
    {
      int prevScore = score;
      lasers[i]->update (time, block_palette, blocks, powerUps, score);
      if (score != prevScore &&
	  (score / EXTRA_BALL_SCORE) > (prevScore / EXTRA_BALL_SCORE))
	extraBallCount++;
      if (lasers[i]->getY () < -lasers[i]->getHeight ())
	{
	  delete lasers[i];
	  lasers.erase (lasers.begin () + i);
	  i--;
	}
    }
  // update all powerups
  for (int i = 0; i < powerUps.size (); i++)
    {
      powerUps[i]->update (time);
      if (powerUps[i]->getY () >= field_height)
	{
	  delete powerUps[i];
	  powerUps.erase (powerUps.begin () + i);
	  i--;
	  continue;
	}
      if ((int) powerUps[i]->getX () <
	  (int) (paddle->getX () + paddle->getWidth ()) &&
	  (int) (powerUps[i]->getX () + powerUps[i]->getWidth ()) >=
	  (int) paddle->getX () &&
	  (int) powerUps[i]->getY () <
	  (int) (paddle->getY () + paddle->getHeight ()) &&
	  (int) (powerUps[i]->getY () + powerUps[i]->getHeight ()) >=
	  (int) paddle->getY ())
	{
          scm_c_eval_string (powerUps[i]->get_script ().c_str ());
	  delete powerUps[i];
	  powerUps.erase (powerUps.begin () + i);
	  i--;
	  continue;
	}
    }
  // if there are no balls on the field, lose a life or game over
  if (balls.size () == 0)
    {
      extraBallCount--;
      if (extraBallCount >= 0)
	{
	  timeTilReady = 2700;

	  balls = vector < Ball * >();

	  double direction =
	    PI / 4 +
	    PI / 2 * ((double) rand () / ((double) (RAND_MAX) + (double) 1));
	  balls.push_back (new Ball (251, 250, direction, ball_size, 
                                     field_width, field_height, imgBall));

	  powerBalls = false;

	  powerUps = vector < PowerUp * >();
	}
      else
	{
          extraBallCount = 0;
          game_over ();
	  return;
	}
    }

  for (vector < Block * >::iterator i = blocks.begin ();
       i != blocks.end (); i++)
    {
      if ((*i)->get_strength () < 1)
	{
	  blocks.erase (i);
	  i--;
	}
    }

  bool won = true;
  for (int i = 0; i < blocks.size (); i++)
    {
      if (!blocks[i]->get_invincible ()) won = false;
    }

  if (won)
    {
      oldScore = score;
      bonusScore = 0;
      tickingBonusTime = true;
      // make sure all channels aren't being used by other sounds
      soundManager->stopSound ();
      // start ticking
      soundManager->loopSound ("bonustick");
    }
}

void
Game::handleEvents ()
{
  if (!m_started)
    return;

  SDL_Event event;
  GameOver *gameover;
  if (m_paused || tickingBonusTime)
    {
      while (SDL_PollEvent (&event))
	{
	  switch (event.type)
	    {
	    case SDL_QUIT:
	      quit = true;
	      break;
	    case SDL_KEYDOWN:
	      switch (event.key.keysym.sym)
		{
		case SDLK_ESCAPE:
                  game_over ();
		  break;
		case SDLK_p:
		  // p to pause
		  m_paused = !m_paused;
		  // if unpausing make sure the mouse is still in the same place (to avoid cheating)
		  if (!m_paused)
		    SDL_WarpMouse ((int)
				   (paddle->getX () +
				    paddle->getWidth () / 2 + 12), 0);
		  break;
		}
	      break;
	    }
	}
    }

  // if paused, don't do anything else
  if (m_paused || tickingBonusTime)
    return;

  int paddledirection = paddle->getDirection ();

  switch (paddledirection)
    {
    case PADDLE_LEFT:
      paddledirection = -1;
      break;
    case PADDLE_NONE:
      paddledirection = 0;
      break;
    case PADDLE_RIGHT:
      paddledirection = 1;
      break;
    }

  int paddleX;
  while (SDL_PollEvent (&event))
    {
      GameOver *gameover;
      switch (event.type)
	{
	case SDL_QUIT:
	  quit = true;
	  break;
	case SDL_KEYDOWN:
	  switch (event.key.keysym.sym)
	    {
	    case SDLK_ESCAPE:
              game_over ();
	      break;
	    case SDLK_p:
	      // p to pause
	      m_paused = !m_paused;
	      // if unpausing make sure the mouse is still in the same place (to avoid cheating)
	      if (!m_paused)
		SDL_WarpMouse ((int)
			       (paddle->getX () + paddle->getWidth () / 2 +
				12), 0);
	      break;
	    case SDLK_LEFT:
	      if (!reverse)
		paddledirection--;
	      else
		paddledirection++;
	      break;
	    case SDLK_RIGHT:
	      if (!reverse)
		paddledirection++;
	      else
		paddledirection--;
	      break;
	    case SDLK_SPACE:
	      if (timeTilReady > 0)
		break;
	      if (paddle->isLaser ())
		lasers.
		  push_back (new
			     Laser ((int) paddle->getX () +
				    paddle->getWidth () / 2 - 4,
				    (int) paddle->getY (), 8, 30, 
                                    field_width, field_height, imgLaser));
	      else if (paddle->isGlue ())
		{
		  for (int i = 0; i < balls.size (); i++)
		    {
		      if (balls[i]->isAttached ())
			balls[i]->release ();
		    }
		}
	      break;
	    }
	  break;
	case SDL_KEYUP:
	  switch (event.key.keysym.sym)
	    {
	    case SDLK_LEFT:
	      if (!reverse)
		paddledirection++;
	      else
		paddledirection--;
	      break;
	    case SDLK_RIGHT:
	      if (!reverse)
		paddledirection--;
	      else
		paddledirection++;
	      break;
	    }
	  break;
	case SDL_MOUSEMOTION:
	  if (!reverse)
	    paddleX = event.motion.x - field_x - paddle->getWidth () / 2;
	  else
	    paddleX = field_width - (event.motion.x - field_x) - paddle->getWidth () / 2;
	  if (paddleX < 0)
	    paddleX = 0;
	  if (paddleX > field_width - paddle->getWidth ())
	    paddleX = field_width - paddle->getWidth ();
	  paddle->setX (paddleX);

	  if (settings_manager.get_fullscreen ())
	    {
	      if (event.motion.x < field_x + paddle->getWidth () / 2)
		{
		  paddleX = 0;
		  SDL_WarpMouse (paddleX + field_x + paddle->getWidth () / 2,
				 event.motion.y);
		}
	      if (event.motion.x > field_width + field_x - paddle->getWidth () / 2)
		{
		  paddleX = field_width - paddle->getWidth ();
		  SDL_WarpMouse (paddleX + field_x + paddle->getWidth () / 2,
				 event.motion.y);
		}
	    }
	  break;
	case SDL_MOUSEBUTTONDOWN:
	  if (timeTilReady > 0)
	    break;
	  if (event.button.button == SDL_BUTTON_LEFT)
	    {
	      if (paddle->isLaser ())
		{
		  lasers.
		    push_back (new
			       Laser ((int) paddle->getX () +
				      paddle->getWidth () / 2 - 4,
				      (int) paddle->getY (), 8, 30,
				      field_width, field_height, imgLaser));
		  soundManager->playSound ("laser");
		}
	      else if (paddle->isGlue ())
		{
		  for (int i = 0; i < balls.size (); i++)
		    {
		      if (balls[i]->isAttached ())
			balls[i]->release ();
		    }
		}
	    }
	}
    }

  switch (paddledirection)
    {
    case -1:
      paddle->setDirection (PADDLE_LEFT);
      break;
    case 0:
      paddle->setDirection (PADDLE_NONE);
      break;
    case 1:
      paddle->setDirection (PADDLE_RIGHT);
      break;
    }

  // update position of balls to be aligned new paddle position
  if (paddle->isGlue ())
    {
      for (int i = 0; i < balls.size (); i++)
	{
	  if (balls[i]->isAttached ())
	    balls[i]->update (0, block_palette, blocks, paddle, powerUps,
			      false, score);
	}
    }
}

void
Game::handlePowerUp (int type)
{
  int n;
  double speed;
  int mouseX;
  switch (type)
    {
    case 0:
      paddle->stretch ();
      break;
    case 1:
      paddle->shrink ();
      break;
    case 2:
      n = balls.size ();
      if (balls.size () >= 28)
	break;
      for (int i = 0; i < n; i++)
	{
	  double direction = balls[i]->getDirection ();
	  double x = balls[i]->getX ();
	  double y = balls[i]->getY ();

	  balls.push_back (new Ball ((int) x,
				     (int) y, direction - PI / 12, ball_size, 
                                     field_width, field_height, imgBall));
	  if (balls[i]->isAttached ())
	    balls.back ()->setAttached (paddle);
	  balls.back ()->setSpeed (balls[i]->getSpeed ());

	  balls.push_back (new Ball ((int) x,
				     (int) y, direction + PI / 12, ball_size, 
                                     field_width, field_height, imgBall));
	  if (balls[i]->isAttached ())
	    balls.back ()->setAttached (paddle);
	  balls.back ()->setSpeed (balls[i]->getSpeed ());
	}
      break;
    case 3:
      paddle->setLaser ();
      for (int i = 0; i < balls.size (); i++)
	if (balls[i]->isAttached ())
	  balls[i]->release ();
      break;
    case 4:
      paddle->setGlue ();
      break;
    case 5:
      powerBalls = true;
      break;
    case 6:
      reverse = !reverse;
      mouseX =
	field_width + field_x - paddle->getWidth () / 2 - (mouseX - 12 -
					 paddle->getWidth () / 2);
      SDL_WarpMouse (mouseX, 0);
      break;
    case 7:
      speed = balls[0]->getSpeed ();
      if (speed < 0.25)
	for (int i = 0; i < balls.size (); i++)
	  balls[i]->setSpeed (0.25);
      else if (speed < 0.30)
	for (int i = 0; i < balls.size (); i++)
	  balls[i]->setSpeed (0.30);
      break;
    case 8:
      speed = balls[0]->getSpeed ();
      if (speed > 0.25)
	for (int i = 0; i < balls.size (); i++)
	  balls[i]->setSpeed (0.20);
      if (speed > 0.20)
	for (int i = 0; i < balls.size (); i++)
	  balls[i]->setSpeed (0.20);
      break;
    }
}

void
Game::add_powerup (int x, int y, string type)
{
  PowerUp *powerup = new PowerUp (x, y, powerup_palette[type], imgPowerUps);
  powerUps.push_back (powerup);
}

void
Game::add_powerup_type (string name, int width, int height, int sx, int sy, 
                        string script)
{
  PowerUpType *poweruptype = new PowerUpType (width, height, sx, sy, script);
  powerup_palette[name] =  poweruptype;
}

void
Game::add_block (int x, int y, string type)
{
  Block *block = new Block (x, y, block_palette[type], imgBlocks);
  blocks.push_back (block);
}

void
Game::add_block_type (string name, int width, int height, int sx, int sy, 
                      bool invincible, string script)
{
  BlockType *blocktype = new BlockType (width, height, sx, sy, invincible,
					script);
  block_palette[name] =  blocktype;
}

void
Game::set_levelset_filename (string levelset_filename)
{
  levelsetFilename = levelset_filename;
}

void
Game::set_levelset_name (string levelset_name)
{
  levelsetName = levelset_name;
}

void
Game::set_ball_size (int ball_size)
{
  this->ball_size = ball_size;
}

void
Game::set_bonus_time (int bonus_time)
{
  this->bonus_time = bonus_time;
}

void
Game::set_background (string filename)
{
  stringstream ss (stringstream::in | stringstream::out);
  ss << LEVELPATH << "/" << levelsetFilename;
  ss << "/" << filename;
  SDL_Surface *temp = IMG_Load (ss.str ().c_str ());
  if (temp != NULL)
    this->imgBackground = SDL_DisplayFormat (temp);
}

void
Game::set_skin (string filename)
{
  stringstream ss (stringstream::in | stringstream::out);
  ss << LEVELPATH << "/" << levelsetFilename;
  ss << "/" << filename;
  SDL_Surface *temp = IMG_Load (ss.str ().c_str ());
  if (temp != NULL)
    this->imgSkin = SDL_DisplayFormat (temp);
}

void
Game::set_level (int level)
{
  this->level = level;
}

void
Game::set_field_width (int field_width)
{
  this->field_width = field_width;
}

void
Game::set_field_height (int field_height)
{
  this->field_height = field_height;
}

void
Game::set_field_x (int field_x)
{
  this->field_x = field_x;
}

void
Game::set_field_y (int field_y)
{
  this->field_y = field_y;
}

void
Game::set_powerup_width (int powerup_width)
{
  this->powerup_width = powerup_width;
}

void
Game::set_powerup_height (int powerup_height)
{
  this->powerup_height = powerup_height;
}

void
Game::add_score (int amount)
{
  score += amount;
}

void
Game::set_bonus_timer_rect (int x, int y, int width, int height)
{
  bonus_timer_x = x;
  bonus_timer_y = y;
  bonus_timer_width = width;
  bonus_timer_height = height;
}

void
Game::set_bonus_timer_source (int x, int y, int width, int height)
{
  bonus_timer_source_x = x;
  bonus_timer_source_y = y;
  bonus_timer_source_width = width;
  bonus_timer_source_height = height;
}

void
Game::set_bonus_timer_start (int x, int y, int width, int height)
{
  bonus_timer_start_x = x;
  bonus_timer_start_y = y;
  bonus_timer_start_width = width;
  bonus_timer_start_height = height;
}

void
Game::set_bonus_timer_end (int x, int y, int width, int height)
{
  bonus_timer_end_x = x;
  bonus_timer_end_y = y;
  bonus_timer_end_width = width;
  bonus_timer_end_height = height;
}

void
Game::set_gameover_initial_time (int gameover_initial_time)
{
  this->gameover_initial_time = gameover_initial_time;
}

void
Game::set_gameover_ball_time (int gameover_ball_time)
{
  this->gameover_ball_time = gameover_ball_time;
}

void
Game::set_gameover_ball_score (int gameover_ball_score)
{
  this->gameover_ball_score = gameover_ball_score;
}

void
Game::set_gameover_ball_padding (int gameover_ball_padding)
{
  this->gameover_ball_padding = gameover_ball_padding;
}

void
Game::set_gameover_balls_x (int gameover_balls_x)
{
  this->gameover_balls_x = gameover_balls_x;
}

void
Game::set_gameover_balls_y (int gameover_balls_y)
{
  this->gameover_balls_y = gameover_balls_y;
}

void
Game::set_ball_padding (int ball_padding)
{
  this->ball_padding = ball_padding;
}

void
Game::set_balls_x (int balls_x)
{
  this->balls_x = balls_x;
}

void
Game::set_balls_y (int balls_y)
{
  this->balls_y = balls_y;
}

void
Game::set_gameover_score_x (int gameover_score_x)
{
  this->gameover_score_x = gameover_score_x;
}

void
Game::set_gameover_score_y (int gameover_score_y)
{
  this->gameover_score_y = gameover_score_y;
}

void
Game::set_gameover_score_label_x (int gameover_score_label_x)
{
  this->gameover_score_label_x = gameover_score_label_x;
}

void
Game::set_gameover_score_label_y (int gameover_score_label_y)
{
  this->gameover_score_label_y = gameover_score_label_y;
}

void
Game::set_score_x (int score_x)
{
  this->score_x = score_x;
}

void
Game::set_score_y (int score_y)
{
  this->score_y = score_y;
}

void
Game::set_score_label_x (int score_label_x)
{
  this->score_label_x = score_label_x;
}

void
Game::set_score_label_y (int score_label_y)
{
  this->score_label_y = score_label_y;
}

void
Game::set_level_x (int level_x)
{
  this->level_x = level_x;
}

void
Game::set_level_y (int level_y)
{
  this->level_y = level_y;
}

void
Game::set_level_label_x (int level_label_x)
{
  this->level_label_x = level_label_x;
}

void
Game::set_level_label_y (int level_label_y)
{
  this->level_label_y = level_label_y;
}

void
Game::set_level_font_face (string face)
{
  level_font_face = face;
}

void
Game::set_level_font_size (int size)
{
  level_font_size = size;
}

void
Game::set_level_font_color (int r, int g, int b)
{
  level_font_color.r = r;
  level_font_color.g = g;
  level_font_color.b = b;
}

void
Game::set_score_font_face (string face)
{
  score_font_face = face;
}

void
Game::set_score_font_size (int size)
{
  score_font_size = size;
}

void
Game::set_score_font_color (int r, int g, int b)
{
  score_font_color.r = r;
  score_font_color.g = g;
  score_font_color.b = b;
}

void
Game::set_level_label_font_face (string face)
{
  level_label_font_face = face;
}

void
Game::set_level_label_font_size (int size)
{
  level_label_font_size = size;
}

void
Game::set_level_label_font_color (int r, int g, int b)
{
  level_label_font_color.r = r;
  level_label_font_color.g = g;
  level_label_font_color.b = b;
}

void
Game::set_score_label_font_face (string face)
{
  score_label_font_face = face;
}

void
Game::set_score_label_font_size (int size)
{
  score_label_font_size = size;
}

void
Game::set_score_label_font_color (int r, int g, int b)
{
  score_label_font_color.r = r;
  score_label_font_color.g = g;
  score_label_font_color.b = b;
}

void
Game::set_gameover_score_label_font_face (string face)
{
  gameover_score_label_font_face = face;
}

void
Game::set_gameover_score_label_font_size (int size)
{
  gameover_score_label_font_size = size;
}

void
Game::set_gameover_score_label_font_color (int r, int g, int b)
{
  gameover_score_label_font_color.r = r;
  gameover_score_label_font_color.g = g;
  gameover_score_label_font_color.b = b;
}

void
Game::set_gameover_score_font_face (string face)
{
  gameover_score_font_face = face;
}

void
Game::set_gameover_score_font_size (int size)
{
  gameover_score_font_size = size;
}

void
Game::set_gameover_score_font_color (int r, int g, int b)
{
  gameover_score_font_color.r = r;
  gameover_score_font_color.g = g;
  gameover_score_font_color.b = b;
}

