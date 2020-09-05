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

#include "highscoremenu.h"
#include "mainmenu.h"

HighScoreMenu
  HighScoreMenu::m_instance;

extern FontManager *font_manager;

void
HighScoreMenu::init ()
{
  quit = false;
  popstate = false;
  pushstate = NULL;
  changestate = NULL;
  m_shift = false;
  menuItems.push_back ("Back");
  spacing = 40;
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
  file >> levelset_count;
  // buffer to consume rest of line
  char buffer[256];
  // consume the rest of the line with the number of levelsets
  file.getline (buffer, 256);
  for (int i = 0; i < levelset_count; i++)
    {
      char strLevelsetName[256];
      file.getline (strLevelsetName, 256);
      levelsetNames.push_back (string (strLevelsetName));
      char strLevelsetFilename[256];
      file.getline (strLevelsetFilename, 256);
      levelsetFilenames.push_back (string (strLevelsetFilename));
    }
  file.close ();
  loadHighScores ();
}

void
HighScoreMenu::cleanup ()
{
  while (!menuItems.empty ())
    menuItems.pop_back ();
  SDL_FreeSurface (imgBackground);
}

void
HighScoreMenu::loadHighScores (string filename)
{
  for (int i = 0; i < levelset_count; i++)
    {
      if (levelsetFilenames[i].compare (filename) == 0)
	{
	  curr_levelset_index = i;
	  break;
	}
    }
  loadHighScores ();
}

void
HighScoreMenu::loadHighScores ()
{
  if (curr_levelset_index > levelsetNames.size () || curr_levelset_index < 0)
    curr_levelset_index = 0;
  scores = vector < int >();
  names = vector < string > ();
  ifstream file;
#ifndef SINGLEDIR
  string location =
    string (getenv ("HOME")) + "/.ballandpaddle/" +
    levelsetFilenames[curr_levelset_index] + "_scores";
#endif
#ifdef SINGLEDIR
  string location = levelsetFilenames[curr_levelset_index] + "_scores";
#endif
  file.open (location.c_str (), ifstream::in);
  if (!file.fail ())
    {
      for (int i = 0; i < 5; i++)
	{
	  int buffer;
	  file >> buffer;
	  scores.push_back (buffer);
	}
      char buffer[50];
      // get garbage data on end of line (should be just a newline character)
      file.getline (buffer, 50);
      for (int i = 0; i < 5; i++)
	{
	  char strName[20];
	  file.getline (strName, 20);
	  names.push_back (string (strName));
	}
      file.close ();
    }
  else
    {
      for (int i = 0; i < 10; i++)
	scores.push_back (0);
      for (int i = 0; i < 10; i++)
	names.push_back ("0");
    }
}

void
HighScoreMenu::saveHighScores ()
{
  if (curr_levelset_index > levelsetNames.size () || curr_levelset_index < 0)
    curr_levelset_index = 0;
  if (scores.size () < 5)
    {
      int size = scores.size ();
      for (int i = 0; i < 5 - size; i++)
	scores.push_back (0);
    }
  ofstream file;
#ifndef SINGLEDIR
  string location =
    string (getenv ("HOME")) + "/.ballandpaddle/" +
    levelsetFilenames[curr_levelset_index] + "_scores";
#endif
#ifdef SINGLEDIR
  string location = levelsetFilenames[curr_levelset_index] + "_scores";
#endif
  file.open (location.c_str ());
  for (int i = 0; i < 5; i++)
    {
      file << scores[i] << endl;
    }
  for (int i = 0; i < 5; i++)
    {
      file << names[i] << endl;
    }
  file.close ();
}

void
HighScoreMenu::pause ()
{
}

void
HighScoreMenu::resume ()
{
}

void
HighScoreMenu::draw (SDL_Surface *dest)
{
  if (curr_levelset_index > levelsetNames.size () || curr_levelset_index < 0)
    curr_levelset_index = 0;
  SDL_BlitSurface (imgBackground, NULL, dest, NULL);
  SDL_Rect r = { 100, 180, 440, 40 };
  SDL_FillRect (dest, &r,
		SDL_MapRGB (dest->format, 64, 64, 64));
  r.w -= 2;
  r.h -= 2;
  r.x++;
  r.y++;
  SDL_FillRect (dest, &r,
		SDL_MapRGB (dest->format, 96, 96, 96));
  SDL_Color maincolor = {160, 160, 0};
  font_manager->drawString ("FreeSans", 24, "<", 100, 180, dest, maincolor);
  font_manager->drawString ("FreeSans", 24, ">", 524, 180, dest, maincolor);
  font_manager->drawStringCentered ("FreeSans", 24,
					levelsetNames[curr_levelset_index],
					320, 180, dest, maincolor);
  SDL_Rect r2 = { 160, 220, 320, 250 };
  SDL_FillRect (dest, &r2,
		SDL_MapRGB (dest->format, 0, 0, 0));
  r2.w -= 2;
  r2.h -= 2;
  r2.x++;
  r2.y++;
  SDL_FillRect (dest, &r2,
		SDL_MapRGB (dest->format, 96, 96, 96));
  for (int i = 0; i < 5; i++)
    {
      if (entering_highscore && i == score_index)
        {
          SDL_Color color = {160, 160, 0};
          font_manager->drawString ("FreeSans", 24, names[i], 164,
				      240 + i * 40, dest, color);
        }
      else
        {
          SDL_Color color = {0, 0, 0};
          font_manager->drawString ("FreeSans", 24, names[i], 164,
                                    240 + i * 40, dest, color);
        }
      stringstream ss (stringstream::in | stringstream::out);
      ss << scores[i];
      string line = ss.str ();
      if (entering_highscore && i == score_index)
        {
          SDL_Color color = {160, 160, 0};
          font_manager->drawString_right ("FreeSans", 24, line, 472, 240 + i * 40, dest, color);
        }
      else
        {
          SDL_Color color = {0, 0, 0};
          font_manager->drawString_right ("FreeSans", 24, line, 472, 240 + i * 40, dest, color);
        }
    }
  if (entering_highscore)
    {
      if (SDL_GetTicks () % 1000 < 500)
	{
	  r.x = 165 + font_manager->get_width (names[score_index]);
	  r.y = 244 + score_index * 40;
	  r.w = 1;
	  r.h = 29;
	  SDL_FillRect (dest, &r,
			SDL_MapRGB (dest->format, 0, 0,
				    0));
	}
    }

  for (int i = 0; i < menuItems.size (); i++)
    {
      if (cursor == i)
        {
          SDL_Color color = {160, 160, 0};
          font_manager->drawStringCentered ("FreeSans", 24, menuItems[i], 320,
                                            430 + i * spacing, dest, color);
        }
      else
        {
          SDL_Color color = {0, 0, 0};
          font_manager->drawStringCentered ("FreeSans", 24, menuItems[i],
                                            320, 430 + i * spacing,
                                            dest, color);
        }
    }
}

void
HighScoreMenu::handleEvents ()
{
  if (curr_levelset_index > levelsetNames.size () || curr_levelset_index < 0)
    curr_levelset_index = 0;

  int mouseX;
  int mouseY;

  SDL_Event event;

  if (entering_highscore)
    {
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
		case SDLK_LSHIFT:
		  m_shift = true;
		  break;
		case SDLK_RSHIFT:
		  m_shift = true;
		  break;
		case SDLK_RETURN:
                  entering_highscore = false;
		  saveHighScores ();
		  break;
		case SDLK_a:
		  names[score_index] += ((m_shift) ? ('A') : ('a'));
		  break;
		case SDLK_b:
		  names[score_index] += ((m_shift) ? ('B') : ('b'));
		  break;
		case SDLK_c:
		  names[score_index] += ((m_shift) ? ('C') : ('c'));
		  break;
		case SDLK_d:
		  names[score_index] += ((m_shift) ? ('D') : ('d'));
		  break;
		case SDLK_e:
		  names[score_index] += ((m_shift) ? ('E') : ('e'));
		  break;
		case SDLK_f:
		  names[score_index] += ((m_shift) ? ('F') : ('f'));
		  break;
		case SDLK_g:
		  names[score_index] += ((m_shift) ? ('G') : ('g'));
		  break;
		case SDLK_h:
		  names[score_index] += ((m_shift) ? ('H') : ('h'));
		  break;
		case SDLK_i:
		  names[score_index] += ((m_shift) ? ('I') : ('i'));
		  break;
		case SDLK_j:
		  names[score_index] += ((m_shift) ? ('J') : ('j'));
		  break;
		case SDLK_k:
		  names[score_index] += ((m_shift) ? ('K') : ('k'));
		  break;
		case SDLK_l:
		  names[score_index] += ((m_shift) ? ('L') : ('l'));
		  break;
		case SDLK_m:
		  names[score_index] += ((m_shift) ? ('M') : ('m'));
		  break;
		case SDLK_n:
		  names[score_index] += ((m_shift) ? ('N') : ('n'));
		  break;
		case SDLK_o:
		  names[score_index] += ((m_shift) ? ('O') : ('o'));
		  break;
		case SDLK_p:
		  names[score_index] += ((m_shift) ? ('P') : ('p'));
		  break;
		case SDLK_q:
		  names[score_index] += ((m_shift) ? ('Q') : ('q'));
		  break;
		case SDLK_r:
		  names[score_index] += ((m_shift) ? ('R') : ('r'));
		  break;
		case SDLK_s:
		  names[score_index] += ((m_shift) ? ('S') : ('s'));
		  break;
		case SDLK_t:
		  names[score_index] += ((m_shift) ? ('T') : ('t'));
		  break;
		case SDLK_u:
		  names[score_index] += ((m_shift) ? ('U') : ('u'));
		  break;
		case SDLK_v:
		  names[score_index] += ((m_shift) ? ('V') : ('v'));
		  break;
		case SDLK_w:
		  names[score_index] += ((m_shift) ? ('W') : ('w'));
		  break;
		case SDLK_x:
		  names[score_index] += ((m_shift) ? ('X') : ('x'));
		  break;
		case SDLK_y:
		  names[score_index] += ((m_shift) ? ('Y') : ('y'));
		  break;
		case SDLK_z:
		  names[score_index] += ((m_shift) ? ('Z') : ('z'));
		  break;
		case SDLK_SPACE:
		  names[score_index] += ' ';
		  break;
		case SDLK_BACKSPACE:
		  names[score_index] =
		    names[score_index].substr (0,
					      names[score_index].length () -
					      1);
		  break;
		}
	      break;
	    case SDL_KEYUP:
	      switch (event.key.keysym.sym)
		{
		case SDLK_LSHIFT:
		  m_shift = false;
		  break;
		case SDLK_RSHIFT:
		  m_shift = false;
		  break;
		}
	    }
	}
      return;
    }

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
/*
	      cursor--;
	      if (cursor < 0)
		cursor = menuItems.size () - 1;
*/
	      break;
	    case SDLK_DOWN:
/*
	      cursor++;
	      if (cursor >= menuItems.size ())
		cursor = 0;
*/
	      break;
	    case SDLK_LEFT:
	      curr_levelset_index--;
	      if (curr_levelset_index < 0)
		curr_levelset_index = 0;
	      else
		loadHighScores ();
	      break;
	    case SDLK_RIGHT:
	      curr_levelset_index++;
	      if (curr_levelset_index >= levelset_count)
		curr_levelset_index = levelset_count - 1;
	      else
		loadHighScores ();
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
HighScoreMenu::update ()
{
}

void
HighScoreMenu::handleItem ()
{
  switch (cursor)
    {
    case 0:
      changestate = MainMenu::instance ();
      break;
    }
}

void
HighScoreMenu::set_score_index (int score_index)
{
  this->score_index = score_index;
}

void
HighScoreMenu::set_entering_highscore (bool entering_highscore)
{
  this->entering_highscore= entering_highscore;
}

void
HighScoreMenu::set_levelset_filename (string filename)
{
  for (int i = 0; i < levelset_count; i++)
    {
      if (levelsetFilenames[i].compare (filename) == 0) curr_levelset_index = i;
    }
}

