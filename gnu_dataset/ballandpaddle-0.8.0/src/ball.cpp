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

#include "ball.h"

extern SoundManager *soundManager;

Ball *Ball::current = NULL;

Ball::Ball (int x, int y, double direction, int size, 
            int field_width, int field_height, SDL_Surface * imgBall)
{
  this->x = x;
  this->y = y;
  this->direction = direction;
  this->imgBall = imgBall;
  this->field_width = field_width;
  this->field_height = field_height;
  width = height = size;
  speed = 0.25;
  attached = false;
  attacher = NULL;
}

Ball::~Ball ()
{
}

void
Ball::draw (SDL_Surface * dest, bool powerBalls)
{
  SDL_Rect s = { (powerBalls ? width : 0), 0, width, height };
  SDL_Rect d = { (int) x, (int) y, 0, 0 };
  SDL_BlitSurface (imgBall, &s, dest, &d);
}

void
Ball::normalizeDirection ()
{
  while (direction < 0)
    direction += 2 * PI;
  while (direction > 2 * PI)
    direction -= 2 * PI;
}

void
Ball::mirrorXDir ()
{
  if (direction >= 0 && direction < PI / 2)
    {
      direction = (double) (PI / 2 + (PI / 2 - direction));
    }
  else if (direction >= PI / 2 && direction < PI)
    {
      direction = (double) (PI / 2 - (direction - PI / 2));
    }
  else if (direction >= PI && direction < 3 * PI / 2)
    {
      direction = (double) (3 * PI / 2 + (3 * PI / 2 - direction));
    }
  else if (direction >= 3 * PI / 2 && direction < 2 * PI)
    {
      direction = (double) (3 * PI / 2 - (direction - 3 * PI / 2));
    }
}

void
Ball::mirrorYDir ()
{
  if (direction >= 0 && direction < PI / 2)
    {
      direction = (double) (2 * PI - direction);
    }
  else if (direction >= PI / 2 && direction < PI)
    {
      direction = (double) (3 * PI / 2 - (direction - PI / 2));
    }
  else if (direction >= PI && direction < 3 * PI / 2)
    {
      direction = (double) (PI / 2 + (3 * PI / 2 - direction));
    }
  else if (direction >= 3 * PI / 2 && direction < 2 * PI)
    {
      direction = (double) (PI / 2 - (direction - 3 * PI / 2));
    }
}

void
Ball::bounceTL ()
{
  direction =
    PI + 3 * PI / 16 +
    PI / 8 * ((double) rand () / ((double) (RAND_MAX) + (double) 1));
}

void
Ball::bounceTR ()
{
  direction =
    PI * 1.5 + 3 * PI / 16 +
    PI / 8 * ((double) rand () / ((double) (RAND_MAX) + (double) 1));
}

void
Ball::bounceBR ()
{
  direction =
    3 * PI / 16 +
    PI / 8 * ((double) rand () / ((double) (RAND_MAX) + (double) 1));
}

void
Ball::bounceBL ()
{
  direction =
    PI / 2 + 3 * PI / 16 +
    PI / 8 * ((double) rand () / ((double) (RAND_MAX) + (double) 1));
}

void
Ball::setSpeed (double speed)
{
  this->speed = speed;
}

void
Ball::set_size (int size)
{
  this->width = size;
  this->height = size;
}

void
Ball::setAttached (Paddle * paddle)
{
  attached = true;
  attacher = paddle;
  attachXOff =
    (x + width / 2 -
     (paddle->getX () + paddle->getWidth () / 2)) / paddle->getWidth ();
  attachYOff =
    (y + height / 2 -
     (paddle->getY () + paddle->getHeight () / 2)) / paddle->getHeight ();
}

void
Ball::release ()
{
  attached = false;
  direction =
    PI + PI / 4 + PI / 2 * (x - attacher->getX ()) / (attacher->getWidth () -
						      width);
  y = field_height - 20;
  if (x < 0)
    x = 0;
  if (x > field_width)
    x = field_width;
  attacher = NULL;
}

void
Ball::update (int time, map < string, BlockType * > block_palette, 
              vector < Block * >blocks, Paddle * paddle, 
              vector < PowerUp * >&powerUps, bool powerBalls, 
              int &score)
{
  if (attached)
    {
      x =
	paddle->getX () + paddle->getWidth () / 2 +
	attachXOff * paddle->getWidth () - width / 2;
      y =
	paddle->getY () + paddle->getHeight () / 2 +
	attachYOff * paddle->getHeight () - height / 2;
      return;
    }
  x += cos (direction) * speed * time;
  y += sin (direction) * speed * time;

  // if it's off the bottom of the play area, nothing more needs to happen
  if (y >= field_height)
    return;

  int left = (int) x;
  int right = (int) (x + width - 1);
  int top = (int) y;
  int bottom = (int) (y + height - 1);
  bool TLHit = false;
  bool TRHit = false;
  bool BLHit = false;
  bool BRHit = false;
  bool paddleTL = false;
  bool paddleTR = false;
  bool paddleBL = false;
  bool paddleBR = false;
  // only bounce off the blocks if the balls aren't powerballs unless it's an invincible block
  if (!powerBalls)
    {
      if (top > 0 && left > 0 && top < field_height && left < field_width)
        {
          if(block_at(block_palette, blocks, left, top) != NULL)
            TLHit = true;
          else TLHit = false;
        }
      if (top > 0 && right < field_width && top < field_height && right > 0)
        {
          if(block_at(block_palette, blocks, right, top) != NULL)
            TRHit = true;
          else TRHit = false;
        }
      if (bottom < field_height && left > 0 && bottom > 0 && left < field_width)
        {
          if(block_at(block_palette, blocks, left, bottom) != NULL)
            BLHit = true;
          else BLHit = false;
        }
      if (bottom < field_height && right < field_width && bottom > 0 && right > 0)
        {
          if(block_at(block_palette, blocks, right, bottom) != NULL)
            BRHit = true;
          else BRHit = false;
        }
    }
  else
    {
      if (top > 0 && left > 0 && top < field_height && left < field_width)
        {
          if(strong_block_at(block_palette, blocks, left, top) != NULL)
            TLHit = true;
          else TLHit = false;
        }
      if (top > 0 && right < field_width && top < field_height && right > 0)
        {
          if(strong_block_at(block_palette, blocks, right, top) != NULL)
            TRHit = true;
          else TRHit = false;
        }
      if (bottom < field_height && left > 0 && bottom > 0 && left < field_width)
        {
          if(strong_block_at(block_palette, blocks, left, bottom) != NULL)
            BLHit = true;
          else BLHit = false;
        }
      if (bottom < field_height && right < field_width && bottom > 0 && right > 0)
        {
          if(strong_block_at(block_palette, blocks, right, bottom) != NULL)
            BRHit = true;
          else BRHit = false;
        }
    }

  if (y < 0)
    {
      TLHit = true;
      TRHit = true;
      y = 0;
      soundManager->playSound ("ballhitwall");
    }
  if (x < 0)
    {
      TLHit = true;
      BLHit = true;
      x = 0;
      soundManager->playSound ("ballhitwall");
    }
  if (x + width >= field_width)
    {
      TRHit = true;
      BRHit = true;
      x = field_width - width;
      soundManager->playSound ("ballhitwall");
    }
  int pl = (int) paddle->getX ();
  int pr = (int) (paddle->getX () + paddle->getWidth () - 1);
  int pt = (int) paddle->getY ();
  int pb = (int) (paddle->getY () + paddle->getHeight () - 1);
  if (top >= pt && top <= pb && left >= pl && left <= pr)
    {
      TLHit = true;
      paddleTL = true;
    }
  if (top >= pt && top <= pb && right >= pl && right <= pr)
    {
      if (!powerBalls)
	TRHit = true;
      paddleTR = true;
    }
  if (bottom >= pt && bottom <= pb && left >= pl && left <= pr)
    {
      BLHit = true;
      paddleBL = true;
    }
  if (bottom >= pt && bottom <= pb && right >= pl && right <= pr)
    {
      BRHit = true;
      paddleBR = true;
    }

  if (paddleTL || paddleTR || paddleBL || paddleBR)
    soundManager->playSound ("ballhitwall");

  int sum = TLHit + TRHit + BLHit + BRHit;
  switch (sum)
    {
    case 0:
      break;
    case 1:
      if (TLHit)
	bounceBR ();
      else if (TRHit)
	bounceBL ();
      else if (BRHit)
	bounceTL ();
      else if (BLHit)
	bounceTR ();
      break;
    case 2:
      if (BLHit && BRHit)
	mirrorYDir ();
      else if (TRHit && BRHit)
	mirrorXDir ();
      else if (TLHit && TRHit)
	mirrorYDir ();
      else if (TLHit && BLHit)
	mirrorXDir ();
      else if (TLHit && BRHit)
	{
	  if (cos (direction) < 0)
	    bounceTR ();
	  else
	    bounceBL ();
	}
      else if (TRHit && BLHit)
	{
	  if (cos (direction) < 0)
	    bounceBR ();
	  else
	    bounceTL ();
	}
      break;
    case 3:
      if (!BRHit)
	bounceBR ();
      else if (!BLHit)
	bounceBL ();
      else if (!TLHit)
	bounceTL ();
      else if (!TRHit)
	bounceTR ();
      break;
    case 4:
      break;
    }

  vector < Block * >hitBlocks;

  if (top > 0 && left > 0 && top < field_height && left < field_width)
    {
      Block *temp_block = block_at(block_palette, blocks, left,top);
      if (temp_block != NULL)
        {
          bool isThere = false;
          for (int i = 0; i < hitBlocks.size (); i++)
            {
              if (temp_block == hitBlocks[i])
                isThere = true;
            }
          if (!isThere)
            hitBlocks.push_back (temp_block);
        }
    }
  if (top > 0 && right < field_width && top < field_height && right > 0)
    {
      Block *temp_block = block_at(block_palette, blocks, right,top);
      if (temp_block != NULL)
        {
          bool isThere = false;
          for (int i = 0; i < hitBlocks.size (); i++)
            {
              if (temp_block == hitBlocks[i])
                isThere = true;
            }
          if (!isThere)
            hitBlocks.push_back (temp_block);
        }
    }
  if (bottom < field_height && left > 0 && bottom > 0 && left < field_width)
    {
      Block *temp_block = block_at(block_palette, blocks, left,bottom);
      if (temp_block != NULL)
        {
          bool isThere = false;
          for (int i = 0; i < hitBlocks.size (); i++)
            {
              if (temp_block == hitBlocks[i])
                isThere = true;
            }
          if (!isThere)
            hitBlocks.push_back (temp_block);
        }
    }
  if (bottom < field_height && right < field_width && bottom > 0 && right > 0)
    {
      Block *temp_block = block_at(block_palette, blocks, right,bottom);
      if (temp_block != NULL)
        {
          bool isThere = false;
          for (int i = 0; i < hitBlocks.size (); i++)
            {
              if (temp_block == hitBlocks[i])
                isThere = true;
            }
          if (!isThere)
            hitBlocks.push_back (temp_block);
        }
    }

  for (int i = 0; i < hitBlocks.size (); i++)
    {
      Ball::current = this;
      Block::current = hitBlocks[i];
      scm_c_eval_string (hitBlocks[i]->get_script ().c_str ());
    }

  // if the paddle is glue, don't bounce, just attach
  if (paddle->isGlue ())
    {
      if (left < paddle->getX () + paddle->getWidth () &&
	  right >= paddle->getX () &&
	  top < paddle->getY () + paddle->getHeight () &&
	  bottom >= paddle->getY ())
	{
	  attached = true;
	  attacher = paddle;
	  attachXOff =
	    (x + width / 2 -
	     (paddle->getX () +
	      paddle->getWidth () / 2)) / paddle->getWidth ();
	  attachYOff =
	    (y + height / 2 -
	     (paddle->getY () +
	      paddle->getHeight () / 2)) / paddle->getHeight ();
	}
    }
  else
    {
      // if it bounces off paddle, shoot in the direction aimed at
      if (paddleBL && paddleBR)
	{
	  double xoff =
	    (x - paddle->getX () - paddle->getWidth () / 2 + width / 2) /
	    (paddle->getWidth () - width);
	  direction = PI + (PI - acos (xoff));
	  y = field_height - paddle->getHeight () - height;;
	}
    }

  // make sure ball is still in play area
  if (x < 0)
    x = 0;
  if (x > field_width - width)
    x = field_width - width;
  if (y < 0)
    y = 0;

  normalizeDirection ();
}
