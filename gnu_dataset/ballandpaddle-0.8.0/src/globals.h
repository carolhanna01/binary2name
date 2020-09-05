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

#include <vector>
#include <string>
using namespace std;

#define FONT_INACTIVE 0
#define FONT_ACTIVE 1
#define FONT_WHITE 2
#define FONT_SMALL_WHITE 3

#define EXTRA_BALL_SCORE 500

#define HIT_LEFT 0
#define HIT_UP 1
#define HIT_RIGHT 2
#define HIT_DOWN 3

#define PADDLE_NONE 0
#define PADDLE_LEFT 1
#define PADDLE_RIGHT 2

#define PI 3.14159

#define POWERUP_RANDOM -1

#define OPTIONS_FULLSCREEN 0
#define OPTIONS_SOUNDS 1
#define OPTIONS_BACK 2

#ifndef PREFIX
#define SINGLEDIR
#define PREFIX "."
#define IMAGEPATH string(PREFIX)+"/images"
#define SOUNDPATH string(PREFIX)+"/sounds"
#define LEVELPATH string(PREFIX)+"/levels"
#endif

#ifndef IMAGEPATH
#define IMAGEPATH string(PREFIX)+"/share/ballandpaddle/images"
#endif

#ifndef SOUNDPATH
#define SOUNDPATH string(PREFIX)+"/share/ballandpaddle/sounds"
#endif

#ifndef LEVELPATH
#define LEVELPATH string(PREFIX)+"/share/ballandpaddle/levels"
#endif

