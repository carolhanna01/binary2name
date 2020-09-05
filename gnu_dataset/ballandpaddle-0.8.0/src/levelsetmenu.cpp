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

#include "levelsetmenu.h"
#include "mainmenu.h"
#include "game.h"

LevelSetMenu
  LevelSetMenu::m_instance;

extern FontManager *font_manager;
extern SettingsManager settings_manager;
extern SoundManager* soundManager;

void
LevelSetMenu::init ()
{
  quit = false;
  popstate = false;
  changestate = NULL;
  pushstate = NULL;
  string location = IMAGEPATH + "/menuback.png";
  SDL_Surface *temp = IMG_Load (location.c_str ());
  if (temp != NULL)
    imgBackground = SDL_DisplayFormat (temp);
  SDL_FreeSurface (temp);
  cursor = 0;
#ifndef SINGLEDIR
  location = string (PREFIX) + "/share/ballandpaddle/levelsets";
#endif
#ifdef SINGLEDIR
  location = string (PREFIX) + "/levelsets";
#endif
  ifstream file;
  file.open (location.c_str (), ifstream::in);
  int levelsetCount;
  file >> levelsetCount;
  // buffer to consume rest of line
  char buffer[256];
  // consume the rest of the line with the number of skins
  file.getline (buffer, 256);
  for (int i = 0; i < levelsetCount; i++)
    {
      char strLevelsetName[256];
      file.getline (strLevelsetName, 256);
      levelsetNames.push_back (string (strLevelsetName));
      char strLevelsetFilename[256];
      file.getline (strLevelsetFilename, 256);
      levelsetFilenames.push_back (string (strLevelsetFilename));
    }
  file.close ();
  for (int i = 0; i < levelsetNames.size (); i++)
    menuItems.push_back (levelsetNames[i]);
  menuItems.push_back ("Back");
  spacing = 40;
}

void
LevelSetMenu::cleanup ()
{
  while (!menuItems.empty ())
    menuItems.pop_back ();
  while (!levelsetNames.empty ())
    levelsetNames.pop_back ();
  while (!levelsetFilenames.empty ())
    levelsetFilenames.pop_back ();
  SDL_FreeSurface (imgBackground);
}

void
LevelSetMenu::pause ()
{
}

void
LevelSetMenu::resume ()
{
}

void
LevelSetMenu::draw (SDL_Surface *dest)
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
LevelSetMenu::handleEvents ()
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
LevelSetMenu::update ()
{
}

void
LevelSetMenu::handleItem ()
{
  for (int i = 0; i < levelsetNames.size (); i++)
    {
      if (cursor == i)
	{
	  if (settings_manager.get_fullscreen ())
	    SDL_ShowCursor (false);
	  Game::instance ()->set_levelset_name (levelsetNames[i]);
	  Game::instance ()->set_levelset_filename (levelsetFilenames[i]);
	  Game::instance ()->loadImages ();
	  changestate = Game::instance ();
	}
    }
  if (cursor == menuItems.size () - 1)
    changestate = MainMenu::instance ();
}
