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

#include "mainmenu.h"
#include "optionsmenu.h"
#include "helpscreen.h"
#include "highscoremenu.h"
#include "levelsetmenu.h"

#include <libintl.h>

extern FontManager *font_manager;
extern SoundManager *soundManager;

MainMenu
  MainMenu::m_instance;

void
MainMenu::init ()
{
  quit = false;
  pushstate = NULL;
  changestate = NULL;
  popstate = false;
  menuItems.push_back (gettext ("Start"));
  menuItems.push_back (gettext ("Help"));
  menuItems.push_back (gettext ("Options"));
  menuItems.push_back (gettext ("High Score"));
  menuItems.push_back (gettext ("Exit"));
  spacing = 40;
  string location = IMAGEPATH + "/menuback.png";
  SDL_Surface *temp = IMG_Load (location.c_str ());
  if (temp != NULL)
    imgBackground = SDL_DisplayFormat (temp);
  SDL_FreeSurface (temp);
  cursor = 0;
}

void
MainMenu::cleanup ()
{
  while (!menuItems.empty ())
    menuItems.pop_back ();
  SDL_FreeSurface (imgBackground);
}

void
MainMenu::pause ()
{
}

void
MainMenu::resume ()
{
}

void
MainMenu::draw (SDL_Surface *dest)
{
  SDL_BlitSurface (imgBackground, NULL, dest, NULL);
  SDL_Rect r = { 160, 230, 320, 230 };
  SDL_FillRect (dest, &r,
		SDL_MapRGB (dest->format, 0, 0, 0));
  r.w -= 2;
  r.h -= 2;
  r.x++;
  r.y++;
  SDL_FillRect (dest, &r,
		SDL_MapRGB (dest->format, 96, 96, 96));

  for (int i = 0; i < menuItems.size (); i++)
    {
      if (cursor == i)
        {
          SDL_Color color = {160, 160, 0};
          font_manager->drawStringCentered ("FreeSans", 24, menuItems[i], 320,
                                            240 + i * spacing, dest, color);
        }
      else
        {
          SDL_Color color = {0, 0, 0};
          font_manager->drawStringCentered ("FreeSans", 24, menuItems[i],
                                            320, 240 + i * spacing,
                                            dest, color);
        }
    }
}

void
MainMenu::handleEvents ()
{

  int mouseX;
  int mouseY;

  SDL_Event event;

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
	    case SDLK_RETURN:
	      handleItem ();
	      break;
	    case SDLK_UP:
	      cursor--;
	      if (cursor < 0)
		cursor = menuItems.size () - 1;
	      soundManager->playSound ("hit");
	      break;
	    case SDLK_DOWN:
	      cursor++;
	      if (cursor >= menuItems.size ())
		cursor = 0;
	      soundManager->playSound ("hit");
	      break;
	    }
	  break;
	case SDL_MOUSEMOTION:
	  mouseX = event.motion.x;
	  mouseY = event.motion.y;

	  int x1;
	  int x2;
	  for (int i = 0; i < menuItems.size (); i++)
	    {
              int text_width = font_manager->get_width (menuItems[i]);
	      x1 = 320 - text_width / 2;
	      x2 = x1 + text_width;
	      if (mouseX >= x1 && mouseX <= x2 && mouseY >= 250 + i * spacing
		  && mouseY <=
		  250 + font_manager->get_height (menuItems[i]) +
		  i * spacing)
		cursor = i;
	    }
	  break;
	case SDL_MOUSEBUTTONDOWN:
	  handleItem ();
	  break;
	}
    }

}

void
MainMenu::update ()
{
}

void
MainMenu::handleItem ()
{
  switch (cursor)
    {
    case 0:
      changestate = LevelSetMenu::instance ();
      break;
    case 1:
      changestate = HelpScreen::instance ();
      break;
    case 2:
      changestate = OptionsMenu::instance ();
      break;
    case 3:
      changestate = HighScoreMenu::instance ();
      break;
    case 4:
      quit = true; 
      break;
    }
}
