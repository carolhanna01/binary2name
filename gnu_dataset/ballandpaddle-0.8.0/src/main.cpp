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

#include <string>
#include <vector>
#include <iostream>
#include <sstream>
using namespace std;

#include <sys/stat.h>

#include "SDL.h"
#include "SDL_image.h"

#include "config.h"
#include "globals.h"
#include "splash.h"
#include "gamestatemanager.h"
#include "soundmanager.h"
#include "fontmanager.h"
#include "settingsmanager.h"
#include "extension.h"

bool fullscreen = false;

GameStateManager gameStateManager;

SoundManager *soundManager;

FontManager *font_manager;

SettingsManager settings_manager;

void
initVideo ()
{
  gameStateManager.initVideo ();
}

void
initAudio ()
{
  if (Mix_OpenAudio (MIX_DEFAULT_FREQUENCY, MIX_DEFAULT_FORMAT, 2, 512) ==
      -1)
    printf ("Failed to open audio\n");
  Mix_AllocateChannels (100);

  soundManager = new SoundManager ();
}

void
inner_main (void *data, int argc, char *args[])
{
  scm_c_define_gsubr ("current-block-width", 0, 0, 0, 
                      (SCM (*)() ) current_block_width);
  scm_c_define_gsubr ("current-block-height", 0, 0, 0, 
                      (SCM (*)() ) current_block_height);
  scm_c_define_gsubr ("current-block-x", 0, 0, 0, 
                      (SCM (*)() ) current_block_x);
  scm_c_define_gsubr ("current-block-y", 0, 0, 0, 
                      (SCM (*)() ) current_block_y);
  scm_c_define_gsubr ("effect-slowdown-balls", 0, 0, 0, 
                      (SCM (*)() ) effect_slowdown_balls);
  scm_c_define_gsubr ("effect-speedup-balls", 0, 0, 0, 
                      (SCM (*)() ) effect_speedup_balls);
  scm_c_define_gsubr ("effect-reverse-controls", 0, 0, 0, 
                      (SCM (*)() ) effect_reverse_controls);
  scm_c_define_gsubr ("effect-power-balls", 0, 0, 0, 
                      (SCM (*)() ) effect_power_balls);
  scm_c_define_gsubr ("effect-glue-power", 0, 0, 0, 
                      (SCM (*)() ) effect_glue_power);
  scm_c_define_gsubr ("effect-laser-power", 0, 0, 0, 
                      (SCM (*)() ) effect_laser_power);
  scm_c_define_gsubr ("effect-divide-balls", 0, 0, 0, 
                      (SCM (*)() ) effect_divide_balls);
  scm_c_define_gsubr ("effect-stretch-paddle", 0, 0, 0, 
                      (SCM (*)() ) effect_stretch_paddle);
  scm_c_define_gsubr ("effect-shrink-paddle", 0, 0, 0, 
                      (SCM (*)() ) effect_shrink_paddle);
  scm_c_define_gsubr ("play-sound", 1, 0, 0, (SCM (*)() ) play_sound);
  scm_c_define_gsubr ("level-add-sound", 1, 0, 0, (SCM (*)() ) level_add_sound);
  scm_c_define_gsubr ("level-add-powerup-type", 6, 0, 0, 
                      (SCM (*)() ) level_add_powerup_type);
  scm_c_define_gsubr ("level-add-block-type", 7, 0, 0, 
                      (SCM (*)() ) level_add_block_type);
  scm_c_define_gsubr ("level-add-block", 3, 0, 0, (SCM (*)() ) level_add_block);
  scm_c_define_gsubr ("level-add-powerup", 3, 0, 0, (SCM (*)() ) level_add_powerup);
  scm_c_define_gsubr ("set-ball-size", 1, 0, 0, (SCM (*)() ) set_ball_size);
  scm_c_define_gsubr ("set-gameover-score-pos", 2, 0, 0, (SCM (*)() ) set_gameover_score_pos);
  scm_c_define_gsubr ("set-gameover-score-font-size", 1, 0, 0, (SCM (*)() ) set_gameover_score_font_size);
  scm_c_define_gsubr ("set-gameover-score-font-color", 3, 0, 0, (SCM (*)() ) set_gameover_score_font_color);
  scm_c_define_gsubr ("set-gameover-score-label-pos", 2, 0, 0, (SCM (*)() ) set_gameover_score_label_pos);
  scm_c_define_gsubr ("set-gameover-score-label-font-size", 1, 0, 0, (SCM (*)() ) set_gameover_score_label_font_size);
  scm_c_define_gsubr ("set-gameover-score-label-font-color", 3, 0, 0, (SCM (*)() ) set_gameover_score_label_font_color);
  scm_c_define_gsubr ("set-score-pos", 2, 0, 0, (SCM (*)() ) set_score_pos);
  scm_c_define_gsubr ("set-score-font-size", 1, 0, 0, (SCM (*)() ) set_score_font_size);
  scm_c_define_gsubr ("set-score-font-color", 3, 0, 0, (SCM (*)() ) set_score_font_color);
  scm_c_define_gsubr ("set-score-label-pos", 2, 0, 0, (SCM (*)() ) set_score_label_pos);
  scm_c_define_gsubr ("set-score-label-font-size", 1, 0, 0, (SCM (*)() ) set_score_label_font_size);
  scm_c_define_gsubr ("set-score-label-font-color", 3, 0, 0, (SCM (*)() ) set_score_label_font_color);
  scm_c_define_gsubr ("set-level-pos", 2, 0, 0, (SCM (*)() ) set_level_pos);
  scm_c_define_gsubr ("set-level-font-size", 1, 0, 0, (SCM (*)() ) set_level_font_size);
  scm_c_define_gsubr ("set-level-font-color", 3, 0, 0, (SCM (*)() ) set_level_font_color);
  scm_c_define_gsubr ("set-level-label-pos", 2, 0, 0, (SCM (*)() ) set_level_label_pos);
  scm_c_define_gsubr ("set-level-label-font-size", 1, 0, 0, (SCM (*)() ) set_level_label_font_size);
  scm_c_define_gsubr ("set-level-label-font-color", 3, 0, 0, (SCM (*)() ) set_level_label_font_color);
  scm_c_define_gsubr ("set-gameover-ball-time", 1, 0, 0, (SCM (*)() ) set_gameover_ball_time);
  scm_c_define_gsubr ("set-gameover-initial-time", 1, 0, 0, (SCM (*)() ) set_gameover_initial_time);
  scm_c_define_gsubr ("set-gameover-ball-padding", 1, 0, 0, (SCM (*)() ) set_gameover_ball_padding);
  scm_c_define_gsubr ("set-gameover-balls-x", 1, 0, 0, (SCM (*)() ) set_gameover_balls_x);
  scm_c_define_gsubr ("set-gameover-balls-y", 1, 0, 0, (SCM (*)() ) set_gameover_balls_y);
  scm_c_define_gsubr ("set-ball-padding", 1, 0, 0, (SCM (*)() ) set_ball_padding);
  scm_c_define_gsubr ("set-balls-x", 1, 0, 0, (SCM (*)() ) set_balls_x);
  scm_c_define_gsubr ("set-balls-y", 1, 0, 0, (SCM (*)() ) set_balls_y);
  scm_c_define_gsubr ("set-gameover-ball-score", 1, 0, 0, (SCM (*)() ) set_gameover_ball_score);
  scm_c_define_gsubr ("set-powerup-width", 1, 0, 0, 
                      (SCM (*)() ) set_powerup_width);
  scm_c_define_gsubr ("set-powerup-height", 1, 0, 0, 
                      (SCM (*)() ) set_powerup_height);
  scm_c_define_gsubr ("set-field-width", 1, 0, 0, (SCM (*)() ) set_field_width);
  scm_c_define_gsubr ("set-field-height", 1, 0, 0, 
                      (SCM (*)() ) set_field_height);
  scm_c_define_gsubr ("set-field-x", 1, 0, 0, (SCM (*)() ) set_field_x);
  scm_c_define_gsubr ("set-field-y", 1, 0, 0, (SCM (*)() ) set_field_y);
  scm_c_define_gsubr ("set-bonus-timer-rect", 4, 0, 0, (SCM (*)() ) set_bonus_timer_rect);
  scm_c_define_gsubr ("set-bonus-timer-source", 4, 0, 0, (SCM (*)() ) set_bonus_timer_source);
  scm_c_define_gsubr ("set-bonus-timer-start", 4, 0, 0, (SCM (*)() ) set_bonus_timer_start);
  scm_c_define_gsubr ("set-bonus-timer-end", 4, 0, 0, (SCM (*)() ) set_bonus_timer_end);
  scm_c_define_gsubr ("set-bonus-time", 1, 0, 0, (SCM (*)() ) set_bonus_time);
  scm_c_define_gsubr ("set-skin", 1, 0, 0, (SCM (*)() ) set_skin);
  scm_c_define_gsubr ("current-ball-set-speed", 1, 0, 0, 
                      (SCM (*)() ) current_ball_set_speed);
  scm_c_define_gsubr ("current-block-set-type", 1, 0, 0, 
                      (SCM (*)() ) current_block_set_type);
  scm_c_define_gsubr ("set-background", 1, 0, 0, (SCM (*)() ) set_background);
  scm_c_define_gsubr ("get-block-type-width", 1, 0, 0, 
                      (SCM (*)() ) get_block_type_width);
  scm_c_define_gsubr ("get-block-type-height", 1, 0, 0, 
                      (SCM (*)() ) get_block_type_height);
  scm_c_define_gsubr ("score-add", 1, 0, 0, (SCM (*)() ) score_add);
  scm_c_define_gsubr ("current-block-remove", 0, 0, 0, 
                      (SCM (*)() ) current_block_remove);
  scm_c_define_gsubr ("current-ball-remove", 0, 0, 0, 
                      (SCM (*)() ) current_ball_remove);
  scm_c_define_gsubr ("random-powerup", 1, 0, 0, (SCM (*)() ) random_powerup);

  setlocale (LC_ALL, "");
  bindtextdomain (PACKAGE, LOCALEDIR);
  textdomain (PACKAGE);

  bool direct = false;

  string levelsetName;
  string levelsetFilename;

  int level = 1;

  stringstream version_ss;
  version_ss << "ballandpaddle (GNU Ball and Paddle) " << VERSION << endl;
  version_ss << "Copyright (C) 2007, 2008 Eric Hutchins" << endl;
  version_ss << "This is free software.  You may redistribute copies of it under the terms of" << endl;
  version_ss << "the GNU General Public License <http://www.gnu.org/licenses/gpl.html>." << endl;
  version_ss << "There is NO WARRANTY, to the extent permitted by law." << endl;
  version_ss << endl;

  stringstream help_ss;
  help_ss << "Usage:" << endl;
  help_ss << "  ballandpaddle [ --levelsetname LEVELSETNAME --levelsetfilename LEVELSETFILENAME --level LEVEL]" << endl;
  help_ss << endl;
  help_ss << "  --level \t\tuse LEVEL as the level and go straight to the game" << endl;
  help_ss << "  --levelsetname \tuse LEVELSETNAME as the levelset name and go straight" << endl;
  help_ss << "   \t\t\t  to the game" << endl;
  help_ss << "  --levelsetfilename \tuse LEVELSETFILENAME as the levelset filename and go" << endl;
  help_ss << "   \t\t\t  straight to the game" << endl;
  help_ss << "  --help \t\tdisplay this help and exit" << endl;
  help_ss << "  --version \t\toutput version information and exit" << endl;

  bool levelsetname_set = false;
  bool levelsetfilename_set = false;
  bool level_set = false;

  string try_help = "Try `ballandpaddle --help' for more information.";

  for (int i = 0; i < argc; i++)
    {
      if (string (args[i]).compare ("--levelsetname") == 0)
        {
          if (argc <= i + 1)
            {
              cout << "ballandpaddle: option requires argument -- levelsetname" << endl;
              cout << try_help << endl;
              exit(0);
            }
          levelsetName = args[i + 1];
          levelsetname_set = true;
          direct = true;
        }
      if (string (args[i]).compare ("--levelsetfilename") == 0)
        {
          if (argc <= i + 1)
            {
              cout << "ballandpaddle: option requires argument -- levelsetfilename" << endl;
              cout << try_help << endl;
              exit(0);
            }
          levelsetFilename = args[i + 1];
          levelsetfilename_set = true;
          direct = true;
        }
      if (string (args[i]).compare ("--level") == 0)
        {
          if (argc <= i + 1)
            {
              cout << "ballandpaddle: option requires argument -- level" << endl;
              cout << try_help << endl;
              exit(0);
            }
          stringstream ss (stringstream::in | stringstream::out);
          ss << args[i + 1];
          ss >> level;
          level_set = true;
          direct = true;
        }
      if (string (args[i]).compare ("--version") == 0)
        {
          cout << version_ss.str();
          exit(0);
        }
      if (string (args[i]).compare ("--help") == 0)
        {
          cout << help_ss.str();
          exit(0);
        }
    }

  if (level_set + levelsetfilename_set + levelsetname_set > 0)
    {
      if (!level_set)
        {
          cout << "ballandpaddle: missing level" << endl;
          cout << try_help << endl;
          exit(0);
        }
      if (!levelsetfilename_set)
        {
          cout << "ballandpaddle: missing levelsetfilename" << endl;
          cout << try_help << endl;
          exit(0);
        }
      if (!levelsetname_set)
        {
          cout << "ballandpaddle: missing levelsetname" << endl;
          cout << try_help << endl;
          exit(0);
        }
    }

  initAudio ();

  font_manager = new FontManager ();
  font_manager->initialize_font ("FreeSans", 16);
  font_manager->initialize_font ("FreeSans", 24);

  gameStateManager.init ();

  if (direct)
    {
      gameStateManager.changeState (Game::instance ());
      if (fullscreen)
        SDL_ShowCursor (false);
      Game::instance ()->set_levelset_name (levelsetName);
      Game::instance ()->set_levelset_filename (levelsetFilename);
      Game::instance ()->set_level (level);
      gameStateManager.loadImages ();
    }
  else
    {
      gameStateManager.changeState (Splash::instance ());
    }

  while (gameStateManager.running ())
    {
      gameStateManager.draw ();
      gameStateManager.handleEvents ();
      gameStateManager.update ();
    }

  gameStateManager.cleanup ();
}

int
main (int argc, char **argv)
{
  scm_boot_guile(argc, argv, inner_main, NULL);
}

