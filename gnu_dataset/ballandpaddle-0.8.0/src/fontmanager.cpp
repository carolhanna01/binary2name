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

#include "fontmanager.h"

FontManager::FontManager ()
{
  TTF_Init ();
  font16 = TTF_OpenFont (string(string(PREFIX) 
                       + "/share/ballandpaddle/FreeSans.ttf").c_str(), 16);
  font24 = TTF_OpenFont (string(string(PREFIX) 
                       + "/share/ballandpaddle/FreeSans.ttf").c_str(), 24);
}

FontManager::~FontManager ()
{
}

void
FontManager::initialize_font (string face, int size)
{
  stringstream ss;
  ss << face << size;
  if (fonts.find (ss.str ()) == fonts.end ())
    {
      fonts[ss.str ()] = TTF_OpenFont (string(string(PREFIX) 
                           + "/share/ballandpaddle/" + face + ".ttf").c_str(), size);
//      cout "Setting up font \"" << ss.str () << "\"" << endl;
    }
}

void
FontManager::drawString_right (string face, int size, string text, int x, int y, 
                               SDL_Surface * dest, SDL_Color color)
{
  stringstream ss;
  ss << face << size;
  if (fonts.find (ss.str ()) == fonts.end ()) return;
  SDL_Surface *label = NULL;
  label = TTF_RenderText_Blended (fonts[ss.str ()], text.c_str(), color);
  SDL_Rect dRect = {x - label->w, y, 0, 0};
  SDL_BlitSurface (label, NULL, dest, &dRect);
  SDL_FreeSurface (label);
}

void
FontManager::drawString (string face, int size, string text, int x, int y, 
                         SDL_Surface * dest, SDL_Color color)
{
  stringstream ss;
  ss << face << size;
  if (fonts.find (ss.str ()) == fonts.end ()) return;
  SDL_Surface *label = NULL;
  label = TTF_RenderText_Blended (fonts[ss.str ()], text.c_str(), color);
  SDL_Rect dRect = {x, y, 0, 0};
  SDL_BlitSurface (label, NULL, dest, &dRect);
  SDL_FreeSurface (label);
}

void
FontManager::drawStringCentered (string face, int size, string text, int x, int y,
			         SDL_Surface * dest, SDL_Color color)
{
  stringstream ss;
  ss << face << size;
  if (fonts.find (ss.str ()) == fonts.end ()) return;
  SDL_Surface *label = NULL;
  label = TTF_RenderText_Blended (fonts[ss.str ()], text.c_str(), color);
  SDL_Rect dRect = {x - label->w / 2, y, 0, 0};
  SDL_BlitSurface (label, NULL, dest, &dRect);
  SDL_FreeSurface (label);
}

int
FontManager::get_width (string text)
{
  if (text.length () == 0) return 0;
  SDL_Color color = {0,0,0};
  SDL_Surface *label = TTF_RenderText_Blended (this->font24, text.c_str(), color);
  return label->w;
}

int
FontManager::get_height (string text)
{
  if (text.length () == 0) return 0;
  SDL_Color color = {0,0,0};
  SDL_Surface *label = TTF_RenderText_Blended (this->font24, text.c_str(), color);
  return label->h;
}

