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

#include "helpscreen.h"
#include "mainmenu.h"

HelpScreen
  HelpScreen::m_instance;

void
HelpScreen::init ()
{
  quit = false;
  popstate = false;
  changestate = NULL;
  pushstate = NULL;
  string location = IMAGEPATH + "/helpscreen.png";
  SDL_Surface *temp = IMG_Load (location.c_str ());
  if (temp != NULL)
    image = SDL_DisplayFormat (temp);
  SDL_FreeSurface (temp);
}

void
HelpScreen::cleanup ()
{
  SDL_FreeSurface (image);
}

void
HelpScreen::pause ()
{
}

void
HelpScreen::resume ()
{
}

void
HelpScreen::handleEvents ()
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
	  changestate = MainMenu::instance ();
	  break;
	}
    }
}

void
HelpScreen::draw (SDL_Surface *dest)
{
  SDL_BlitSurface (image, NULL, dest, NULL);
}

void
HelpScreen::update ()
{
}
