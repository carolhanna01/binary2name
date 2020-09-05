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

#ifndef EXTENSION_H
#define EXTENSION_H

#include <vector>
#include <string>
using namespace std;

#include <libguile.h>

#include "game.h"

SCM
play_sound (SCM name);

SCM
level_add_sound (SCM name);

SCM
random_powerup (SCM chance);

SCM
current_ball_remove ();

SCM
current_block_remove ();

SCM
score_add (SCM amount);

SCM
current_block_set_type (SCM type);

SCM
current_ball_set_speed (SCM speed);

SCM
level_add_powerup_type (SCM name, SCM width, SCM height, SCM sx, SCM sy, 
                        SCM script);

SCM
level_add_powerup (SCM x, SCM y, SCM type);

SCM
level_add_block_type (SCM name, SCM width, SCM height, SCM sx, SCM sy, 
                      SCM invincible, SCM script);

SCM
level_add_block (SCM x, SCM y, SCM type);

SCM
set_skin (SCM filename);

SCM
set_background (SCM filename);

SCM
set_ball_size (SCM size);

SCM
set_powerup_width (SCM size);

SCM
set_powerup_height (SCM size);

SCM
set_bonus_time (SCM bonus_time);

SCM
set_field_x (SCM field_x);

SCM
set_field_y (SCM field_y);

SCM
set_field_width (SCM field_width);

SCM
set_field_height (SCM field_height);

SCM
get_block_type_width (SCM type);

SCM
get_block_type_height (SCM type);

SCM
effect_stretch_paddle ();

SCM
effect_shrink_paddle ();

SCM
effect_divide_balls ();

SCM
effect_laser_power ();

SCM
effect_glue_power ();

SCM
effect_power_balls ();

SCM
effect_reverse_controls ();

SCM
effect_speedup_balls ();

SCM
effect_slowdown_balls ();

SCM
current_block_width ();

SCM
current_block_height ();

SCM
current_block_x ();

SCM
current_block_y ();

SCM
set_bonus_timer_rect (SCM x, SCM y, SCM width, SCM height);

SCM
set_bonus_timer_source (SCM x, SCM y, SCM width, SCM height);

SCM
set_bonus_timer_start (SCM x, SCM y, SCM width, SCM height);

SCM
set_bonus_timer_end (SCM x, SCM y, SCM width, SCM height);

SCM
set_gameover_initial_time (SCM initial_time);

SCM
set_gameover_ball_time (SCM ball_time);

SCM
set_gameover_ball_score (SCM ball_score);

SCM
set_gameover_ball_padding (SCM ball_padding);

SCM
set_gameover_balls_x (SCM balls_x);

SCM
set_gameover_balls_y (SCM balls_y);

SCM
set_gameover_score_font_color (SCM r, SCM g, SCM b);

SCM
set_gameover_score_label_font_color (SCM r, SCM g, SCM b);

SCM
set_gameover_score_font_size (SCM size);

SCM
set_gameover_score_label_font_size (SCM size);

SCM
set_gameover_score_font_face (SCM face);

SCM
set_gameover_score_label_font_face (SCM face);

SCM
set_gameover_score_pos (SCM gameover_score_x, SCM gameover_score_y);

SCM
set_gameover_score_label_pos (SCM gameover_score_label_x, SCM gameover_score_label_y);

SCM
set_score_pos (SCM score_x, SCM score_y);

SCM
set_score_label_pos (SCM score_label_x, SCM score_label_y);

SCM
set_level_pos (SCM level_x, SCM level_y);

SCM
set_level_label_pos (SCM level_label_x, SCM level_label_y);

SCM
set_ball_padding (SCM ball_padding);

SCM
set_balls_x (SCM balls_x);

SCM
set_balls_y (SCM balls_y);

SCM
set_level_font_face (SCM face);

SCM
set_level_font_size (SCM size);

SCM
set_level_font_color (SCM r, SCM g, SCM b);

SCM
set_score_font_face (SCM face);

SCM
set_score_font_size (SCM size);

SCM
set_score_font_color (SCM r, SCM g, SCM b);

SCM
set_level_label_font_face (SCM face);

SCM
set_level_label_font_size (SCM size);

SCM
set_level_label_font_color (SCM r, SCM g, SCM b);

SCM
set_score_label_font_face (SCM face);

SCM
set_score_label_font_size (SCM size);

SCM
set_score_label_font_color (SCM r, SCM g, SCM b);

#endif

