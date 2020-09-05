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

#include "optionsmenu.h"
#include "mainmenu.h"

OptionsMenu
  OptionsMenu::m_instance;

extern FontManager *font_manager;

extern SettingsManager settings_manager;

extern void initVideo ();
extern bool fullscreen;

void
OptionsMenu::init ()
{
  quit = false;
  popstate = false;
  pushstate = NULL;
  changestate = NULL;
  menuItems.push_back ("Fullscreen [ ]");
  menuItems.push_back ("Play Sounds [ ]");
  menuItems.push_back ("Back");
  spacing = 40;
  string location = IMAGEPATH + "/menuback.png";
  SDL_Surface *temp = IMG_Load (location.c_str ());
  if (temp != NULL)
    imgBackground = SDL_DisplayFormat (temp);
  SDL_FreeSurface (temp);
  cursor = 0;
}

void
OptionsMenu::cleanup ()
{
  while (!menuItems.empty ())
    menuItems.pop_back ();
  SDL_FreeSurface (imgBackground);
}

void
OptionsMenu::pause ()
{
}

void
OptionsMenu::resume ()
{
}

void
OptionsMenu::draw (SDL_Surface *dest)
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
                                            250 + i * spacing, dest, color);
        }
      else
        {
          SDL_Color color = {0, 0, 0};
          font_manager->drawStringCentered ("FreeSans", 24, menuItems[i],
                                            320, 250 + i * spacing,
                                            dest, color);
        }
    }
}

void
OptionsMenu::handleEvents ()
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
	      break;
	    case SDLK_DOWN:
	      cursor++;
	      if (cursor >= menuItems.size ())
		cursor = 0;
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
OptionsMenu::update ()
{
  if (settings_manager.get_fullscreen ())
    {
      if (menuItems[0].compare ("Fullscreen [X]") != 0)
	menuItems[0] = "Fullscreen [X]";
    }
  else
    {
      if (menuItems[0].compare ("Fullscreen [ ]") != 0)
	menuItems[0] = "Fullscreen [ ]";
    }
  if (settings_manager.get_sound_enabled ())
    {
      if (menuItems[1].compare ("Play Sounds [X]") != 0)
	menuItems[1] = "Play Sounds [X]";
    }
  else
    {
      if (menuItems[1].compare ("Play Sounds [ ]") != 0)
	menuItems[1] = "Play Sounds [ ]";
    }
}

void
OptionsMenu::handleItem ()
{
  switch (cursor)
    {
    case OPTIONS_FULLSCREEN:
      settings_manager.set_fullscreen (!settings_manager.get_fullscreen ());
      settings_manager.save_settings ();
      initVideo ();
      break;
    case OPTIONS_SOUNDS:
      settings_manager.
        set_sound_enabled (!settings_manager.get_sound_enabled ());
      settings_manager.save_settings ();
      break;
    case OPTIONS_BACK:
      changestate = MainMenu::instance ();
      break;
    }
}

