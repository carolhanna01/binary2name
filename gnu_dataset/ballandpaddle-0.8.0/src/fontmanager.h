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

#ifndef FONT_MANAGER_H
#define FONT_MANAGER_H

#include <string>
#include <sstream>
#include <map>
#include <iostream>
using namespace std;

#include "SDL.h"
#include "SDL_ttf.h"

#include "globals.h"

class FontManager
{
  map <string, TTF_Font *> fonts;
  TTF_Font *font16;
  TTF_Font *font24;
public:
  FontManager ();
  ~FontManager ();
  void initialize_font (string face, int size);
  void drawString (string face, int size, string text, int x, int y, SDL_Surface * dest, SDL_Color color);
  void drawString_right (string face, int size, string text, int x, int y, SDL_Surface * dest, SDL_Color color);
  void drawStringCentered (string face, int size, string text, int x, int y,
			   SDL_Surface * dest, SDL_Color color);
  int get_width (string text);
  int get_height (string text);
};

#endif
