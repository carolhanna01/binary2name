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

#include "extension.h"

extern SoundManager *soundManager;

SCM
play_sound (SCM name)
{
  soundManager->playSound (scm_to_locale_string (name));
}

SCM
level_add_sound (SCM name)
{
  string levelset_filename = Game::instance ()->get_levelset_filename ();
  soundManager->addSound (scm_to_locale_string (name),
			    LEVELPATH + "/" + levelset_filename +
			    "/" + scm_to_locale_string (name) + ".wav");
}

SCM
random_powerup (SCM chance)
{
  if (rand () > RAND_MAX * 0.01 * scm_to_int (chance)) return scm_from_int (0);
  map < string, PowerUpType * > * powerup_palette = 
    Game::instance ()->get_powerup_palette ();
  int size = powerup_palette->size ();
  if (size == 0) return scm_from_int (0);
  int a = rand () / (RAND_MAX / size);
  map<string,PowerUpType *>::iterator i = powerup_palette->begin ();
  for (int j = 0; j < a; j++)
    {
      i++;
    }
  Game::instance ()->add_powerup (Block::current->get_x (), 
                                  Block::current->get_y (), i->first);
}

SCM
current_ball_remove ()
{
  vector < Ball * > * balls = Game::instance ()->get_balls ();
  for (vector < Ball * >::iterator i = balls->begin ();
       i != balls->end (); i++)
    {
      if ((*i) == Ball::current)
	{
	  balls->erase (i);
	  i--;
	}
    }
}

SCM
current_block_remove ()
{
  vector < Block * > * blocks = Game::instance ()->get_blocks ();
  for (vector < Block * >::iterator i = blocks->begin ();
       i != blocks->end (); i++)
    {
      if ((*i) == Block::current)
	{
	  blocks->erase (i);
	  i--;
	}
    }
}

SCM
score_add (SCM amount)
{
  Game::instance ()->add_score (scm_to_int (amount));
}

SCM
current_block_set_type (SCM type)
{
  map < string, BlockType * > * block_palette = 
    Game::instance ()->get_block_palette ();
  Block::current->set_type ((*block_palette)[scm_to_locale_string (type)]);
}

SCM
current_ball_set_speed (SCM speed)
{
  Ball::current->setSpeed (scm_to_double (speed));
}

SCM
level_add_powerup_type (SCM name, SCM width, SCM height, SCM sx, SCM sy, 
                        SCM script)
{
  Game *game = Game::instance ();
  if (game != NULL)
    {
      game->add_powerup_type (scm_to_locale_string (name),
                              scm_to_int (width), scm_to_int (height), 
                              scm_to_int (sx), scm_to_int (sy), 
                              scm_to_locale_string (script));
    }
}

SCM
level_add_powerup (SCM x, SCM y, SCM type)
{
  Game *game = Game::instance ();
  if (game != NULL)
    {
      game->add_powerup (scm_to_int (x), scm_to_int (y), 
                         scm_to_locale_string (type));
    }
}

SCM
level_add_block_type (SCM name, SCM width, SCM height, SCM sx, SCM sy, 
                      SCM invincible, SCM script)
{
  Game *game = Game::instance ();
  if (game != NULL)
    {
      game->add_block_type (scm_to_locale_string (name),
                            scm_to_int (width), scm_to_int (height), 
                            scm_to_int (sx), scm_to_int (sy), 
                            scm_to_bool (invincible), 
                            scm_to_locale_string (script));
    }
}

SCM
level_add_block (SCM x, SCM y, SCM type)
{
  Game *game = Game::instance ();
  if (game != NULL)
    {
      game->add_block (scm_to_int (x), scm_to_int (y), 
                       scm_to_locale_string (type));
    }
}

SCM
set_skin (SCM filename)
{
  Game *game = Game::instance ();
  if (game != NULL)
    {
      game->set_skin (scm_to_locale_string (filename));
    }
}

SCM
set_background (SCM filename)
{
  Game *game = Game::instance ();
  if (game != NULL)
    {
      game->set_background (scm_to_locale_string (filename));
    }
}

SCM
set_field_x (SCM field_x)
{
  Game *game = Game::instance ();
  if (game != NULL)
    {
      game->set_field_x (scm_to_int (field_x));
    }
}

SCM
set_field_y (SCM field_y)
{
  Game *game = Game::instance ();
  if (game != NULL)
    {
      game->set_field_y (scm_to_int (field_y));
    }
}

SCM
set_field_width (SCM field_width)
{
  Game *game = Game::instance ();
  if (game != NULL)
    {
      game->set_field_width (scm_to_int (field_width));
    }
}

SCM
set_field_height (SCM field_height)
{
  Game *game = Game::instance ();
  if (game != NULL)
    {
      game->set_field_height (scm_to_int (field_height));
    }
}

SCM
set_powerup_width (SCM size)
{
  Game *game = Game::instance ();
  if (game != NULL)
    {
      game->set_powerup_width (scm_to_int (size));
    }
}

SCM
set_powerup_height (SCM size)
{
  Game *game = Game::instance ();
  if (game != NULL)
    {
      game->set_powerup_height (scm_to_int (size));
    }
}

SCM
set_ball_size (SCM size)
{
  Game *game = Game::instance ();
  if (game != NULL)
    {
      game->set_ball_size (scm_to_int (size));
    }
}

SCM
set_bonus_time (SCM bonus_time)
{
  Game *game = Game::instance ();
  if (game != NULL)
    {
      game->set_bonus_time (scm_to_int (bonus_time));
    }
}

SCM
get_block_type_width (SCM type)
{
  Game *game = Game::instance ();
  if (game != NULL)
    {
      map < string, BlockType * > * block_palette = game->get_block_palette ();
      return scm_from_int ((*block_palette)[scm_to_locale_string(type)]->get_width ());
    }
}

SCM
get_block_type_height (SCM type)
{
  Game *game = Game::instance ();
  if (game != NULL)
    {
      map < string, BlockType * > * block_palette = game->get_block_palette ();
      return scm_from_int ((*block_palette)[scm_to_locale_string(type)]->get_height ());
    }
}

SCM
effect_stretch_paddle ()
{
  Game::instance ()->handlePowerUp (0);
}

SCM
effect_shrink_paddle ()
{
  Game::instance ()->handlePowerUp (1);
}

SCM
effect_divide_balls ()
{
  Game::instance ()->handlePowerUp (2);
}

SCM
effect_laser_power ()
{
  Game::instance ()->handlePowerUp (3);
}

SCM
effect_glue_power ()
{
  Game::instance ()->handlePowerUp (4);
}

SCM
effect_power_balls ()
{
  Game::instance ()->handlePowerUp (5);
}

SCM
effect_reverse_controls ()
{
  Game::instance ()->handlePowerUp (6);
}

SCM
effect_speedup_balls ()
{
  Game::instance ()->handlePowerUp (7);
}

SCM
effect_slowdown_balls ()
{
  Game::instance ()->handlePowerUp (8);
}

SCM
current_block_x ()
{
  return scm_from_int (Block::current->get_x ());
}

SCM
current_block_y ()
{
  return scm_from_int (Block::current->get_y ());
}

SCM
current_block_width ()
{
  return scm_from_int (Block::current->get_type ()->get_width ());
}

SCM
current_block_height ()
{
  return scm_from_int (Block::current->get_type ()->get_height ());
}
/*
SCM
set_bonus_x (SCM x)
{
  Game::instance ()->set_bonus_x (scm_to_int (x));
}

SCM
set_bonus_y (SCM y)
{
  Game::instance ()->set_bonus_y (scm_to_int (y));
}

SCM
set_bonus_graphic_width (SCM width)
{
  Game::instance ()->set_bonus_graphic_width (scm_to_int (width));
}

SCM
set_bonus_graphic_x (SCM graphic_x)
{
  Game::instance ()->set_bonus_graphic_x (scm_to_int (graphic_x));
}

SCM
set_bonus_graphic_y (SCM graphic_y)
{
  Game::instance ()->set_bonus_graphic_y (scm_to_int (graphic_y));
}

SCM
set_bonus_width (SCM width)
{
  Game::instance ()->set_bonus_width (scm_to_int (width));
}

SCM
set_bonus_height (SCM height)
{
  Game::instance ()->set_bonus_height (scm_to_int (height));
}

SCM
set_bonus_start_graphic_x (SCM graphic_x)
{
  Game::instance ()->set_bonus_start_graphic_x (scm_to_int (graphic_x));
}

SCM
set_bonus_start_graphic_width(SCM graphic_width)
{
  Game::instance ()->set_bonus_start_graphic_width (scm_to_int (graphic_width));
}

SCM
set_bonus_end_graphic_x (SCM graphic_x)
{
  Game::instance ()->set_bonus_end_graphic_x (scm_to_int (graphic_x));
}

SCM
set_bonus_end_graphic_width (SCM graphic_width)
{
  Game::instance ()->set_bonus_end_graphic_width (scm_to_int (graphic_width));
}
*/

SCM
set_bonus_timer_rect (SCM x, SCM y, SCM width, SCM height)
{
  Game::instance ()->set_bonus_timer_rect (scm_to_int (x),
                                           scm_to_int (y),
                                           scm_to_int (width),
                                           scm_to_int (height));
}

SCM
set_bonus_timer_source (SCM x, SCM y, SCM width, SCM height)
{
  Game::instance ()->set_bonus_timer_source (scm_to_int (x),
                                             scm_to_int (y),
                                             scm_to_int (width),
                                             scm_to_int (height));
}

SCM
set_bonus_timer_start (SCM x, SCM y, SCM width, SCM height)
{
  Game::instance ()->set_bonus_timer_start (scm_to_int (x),
                                            scm_to_int (y),
                                            scm_to_int (width),
                                            scm_to_int (height));
}

SCM
set_bonus_timer_end (SCM x, SCM y, SCM width, SCM height)
{
  Game::instance ()->set_bonus_timer_end (scm_to_int (x),
                                          scm_to_int (y),
                                          scm_to_int (width),
                                          scm_to_int (height));
}

SCM
set_gameover_initial_time (SCM initial_time)
{
  Game::instance ()->set_gameover_initial_time (scm_to_int (initial_time));
}

SCM
set_gameover_ball_time (SCM ball_time)
{
  Game::instance ()->set_gameover_ball_time (scm_to_int (ball_time));
}

SCM
set_gameover_ball_score (SCM ball_score)
{
  Game::instance ()->set_gameover_ball_score (scm_to_int (ball_score));
}

SCM
set_gameover_ball_padding (SCM ball_padding)
{
  Game::instance ()->set_gameover_ball_padding (scm_to_int (ball_padding));
}

SCM
set_gameover_balls_x (SCM balls_x)
{
  Game::instance ()->set_gameover_balls_x (scm_to_int (balls_x));
}

SCM
set_gameover_balls_y (SCM balls_y)
{
  Game::instance ()->set_gameover_balls_y (scm_to_int (balls_y));
}

SCM
set_ball_padding (SCM ball_padding)
{
  Game::instance ()->set_ball_padding (scm_to_int (ball_padding));
}

SCM
set_balls_x (SCM balls_x)
{
  Game::instance ()->set_balls_x (scm_to_int (balls_x));
}

SCM
set_balls_y (SCM balls_y)
{
  Game::instance ()->set_balls_y (scm_to_int (balls_y));
}

SCM
set_gameover_score_pos (SCM gameover_score_x, SCM gameover_score_y)
{
  Game::instance ()->set_gameover_score_x (scm_to_int (gameover_score_x));
  Game::instance ()->set_gameover_score_y (scm_to_int (gameover_score_y));
}

SCM
set_gameover_score_label_pos (SCM gameover_score_label_x, SCM gameover_score_label_y)
{
  Game::instance ()->set_gameover_score_label_x (scm_to_int (gameover_score_label_x));
  Game::instance ()->set_gameover_score_label_y (scm_to_int (gameover_score_label_y));
}

SCM
set_score_pos (SCM score_x, SCM score_y)
{
  Game::instance ()->set_score_x (scm_to_int (score_x));
  Game::instance ()->set_score_y (scm_to_int (score_y));
}

SCM
set_score_label_pos (SCM score_label_x, SCM score_label_y)
{
  Game::instance ()->set_score_label_x (scm_to_int (score_label_x));
  Game::instance ()->set_score_label_y (scm_to_int (score_label_y));
}

SCM
set_level_pos (SCM level_x, SCM level_y)
{
  Game::instance ()->set_level_x (scm_to_int (level_x));
  Game::instance ()->set_level_y (scm_to_int (level_y));
}

SCM
set_level_label_pos (SCM level_label_x, SCM level_label_y)
{
  Game::instance ()->set_level_label_x (scm_to_int (level_label_x));
  Game::instance ()->set_level_label_y (scm_to_int (level_label_y));
}

SCM
set_level_font_face (SCM face)
{
  Game::instance ()->set_level_font_face (scm_to_locale_string (face));
}

SCM
set_level_font_size (SCM size)
{
  Game::instance ()->set_level_font_size (scm_to_int (size));
}

SCM
set_level_font_color (SCM r, SCM g, SCM b)
{
  Game::instance ()->set_level_font_color (scm_to_int (r), scm_to_int (g), scm_to_int (b));
}

SCM
set_score_font_face (SCM face)
{
  Game::instance ()->set_score_font_face (scm_to_locale_string (face));
}

SCM
set_score_font_size (SCM size)
{
  Game::instance ()->set_score_font_size (scm_to_int (size));
}

SCM
set_score_font_color (SCM r, SCM g, SCM b)
{
  Game::instance ()->set_score_font_color (scm_to_int (r), scm_to_int (g), scm_to_int (b));
}

SCM
set_level_label_font_face (SCM face)
{
  Game::instance ()->set_level_label_font_face (scm_to_locale_string (face));
}

SCM
set_level_label_font_size (SCM size)
{
  Game::instance ()->set_level_label_font_size (scm_to_int (size));
}

SCM
set_level_label_font_color (SCM r, SCM g, SCM b)
{
  Game::instance ()->set_level_label_font_color (scm_to_int (r), scm_to_int (g), scm_to_int (b));
}

SCM
set_score_label_font_face (SCM face)
{
  Game::instance ()->set_score_label_font_face (scm_to_locale_string (face));
}

SCM
set_score_label_font_size (SCM size)
{
  Game::instance ()->set_score_label_font_size (scm_to_int (size));
}

SCM
set_score_label_font_color (SCM r, SCM g, SCM b)
{
  Game::instance ()->set_score_label_font_color (scm_to_int (r), scm_to_int (g), scm_to_int (b));
}

SCM
set_gameover_score_font_color (SCM r, SCM g, SCM b)
{
  Game::instance ()->set_gameover_score_font_color (scm_to_int (r), scm_to_int (g), scm_to_int (b));
}

SCM
set_gameover_score_label_font_color (SCM r, SCM g, SCM b)
{
  Game::instance ()->set_gameover_score_label_font_color (scm_to_int (r), scm_to_int (g), scm_to_int (b));
  Game::instance ()->set_score_label_font_color (scm_to_int (r), scm_to_int (g), scm_to_int (b));
}

SCM
set_gameover_score_font_size (SCM size)
{
  Game::instance ()->set_gameover_score_font_size (scm_to_int (size));
}

SCM
set_gameover_score_label_font_size (SCM size)
{
  Game::instance ()->set_gameover_score_label_font_size (scm_to_int (size));
}

SCM
set_gameover_score_font_face (SCM face)
{
  Game::instance ()->set_score_font_face (scm_to_locale_string (face));
}

SCM
set_gameover_score_label_font_face (SCM face)
{
  Game::instance ()->set_score_label_font_face (scm_to_locale_string (face));
}

