/*
    GNU Maverik - a system for managing display and interaction in 
               Virtual Environment applications.
    Copyright (C) 2008  Advanced Interfaces Group

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

    The authors can be contacted via:
    www   - http://aig.cs.man.ac.uk
    email - maverik@aig.cs.man.ac.uk
    mail  - Advanced Interfaces Group, Room 2.94, Kilburn Building, 
         University of Manchester, Manchester, M13 9PL, UK
*/


#include "maverik.h"
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/time.h>
#include <stdlib.h>


#define PAWN   0
#define ROOK   1
#define KNIGHT 2
#define BISHOP 3
#define QUEEN  4
#define KING   5
#define WHITE  0
#define BLACK  1

int fd[4];
int board[64];
MAV_object *piece[2][6];
int whosgo=1;
int moving=0;
int movefrom=0;
int moveto=0;
MAV_vector movedir;



/* Routine to draw the board */

void drawboard(void)
{
  MAV_matrix *m;
  MAV_vector p;
  MAV_surfaceParams sp;
  int i, x, y, col=1;

  for (i=0; i<64; i++) {

    /* Calculate the colour of the square */
    if (i%8) col=!col;
      
    sp.mode= MAV_COLOUR;
    if (moving && (i==moveto || i==movefrom)) 
    {
      sp.colour= MAV_COLOUR_RED;
    } 
    else if (col)
    {
      sp.colour= MAV_COLOUR_BLACK;
    }
    else
    {
      sp.colour= MAV_COLOUR_WHITE;
    }
    mav_surfaceParamsUse(&sp);

    /* Draw the square */
    x=i%8;
    y=i/8;

    p.x=x-0.5;
    p.y=y-0.5;
    p.z=0;
    mav_gfxPolygonBegin();
    mav_gfxVertex(p);
    
    p.x+=1.0;
    mav_gfxVertex(p);
    
    p.y+=1.0;
    mav_gfxVertex(p);

    p.x-=1.0;
    mav_gfxVertex(p);
    mav_gfxPolygonEnd();

    /* Does the square contain a piece */
    if (board[i]!=-1) {
      /* calculate which piece and its colour */
      int pc= board[i]&7;
      int co= (board[i]>>4)&1;

      /* get a pointer to the matrix of that piece */
      mav_callbackGetMatrixExec(mav_win_current, piece[co][pc], &m);

      /* set matrix to correct position */
      if (moving && i==movefrom) 
      {
	*m= mav_matrixSet(0,90,0, x+movedir.x*moving, y+movedir.y*moving, 0);
      }
      else
      {
	*m= mav_matrixSet(0,90,0, x, y, 0);
      }
      
      /* render the piece */
      mav_callbackDrawExec(mav_win_current, piece[co][pc], NULL);
    }
  }

  /* Display status of game */
  if (moving) 
  {
    mav_stringDisplay(mav_win_all, "moving", MAV_COLOUR_RED, 0, -0.9, 0.9);
  }
  else if (whosgo) 
  {
    mav_stringDisplay(mav_win_all, "player thinking...", MAV_COLOUR_RED, 0, -0.9, 0.9);
  }
  else
  {
    mav_stringDisplay(mav_win_all, "computer thinking...", MAV_COLOUR_RED, 0, -0.9, 0.9);
  }

  /* Pieces are moved over 5 frames */
  if (moving) {
    moving++;

    if (moving==6) {
    /* movement over, update board */
      moving=0;
      board[moveto]= board[movefrom];
      board[movefrom]= -1;
    }
  }
}



/* Routine to convert chess style position (e.g "h3") into array index */

int sq(char *s)
{
  return ((s[0]-'a')+(s[1]-'1')*8);
}



/* Routine to make the move give in string s */

void movePiece(char *s)
{
  if (strstr(s, "o-o")) 
  {
    if (whosgo) 
    {
      /* player castling */
      board[sq("g1")]= board[sq("e1")];
      board[sq("e1")]= -1;

      board[sq("f1")]= board[sq("h1")]; 
      board[sq("h1")]= -1;
    }
    else
    {
      /* computer castling */
      board[sq("g8")]= board[sq("e8")];
      board[sq("e8")]= -1;

      board[sq("f8")]= board[sq("h8")]; 
      board[sq("h8")]= -1;
    }
  }
  else
  {
    /* calculate the movement of the piece */
    moving= 1;
    movefrom= sq(&s[0]);
    moveto= sq(&s[2]);
    movedir.x= (moveto%8)-(movefrom%8);
    movedir.y= (moveto/8)-(movefrom/8);
    movedir.z= 0.0;
    movedir= mav_vectorScalar(movedir, 0.2); /* complete movement in 5 steps */
  }

  /* toggle whos go */
  whosgo=!whosgo;
}



/* Routine to check for events from GNU chess */

int checkGNUchess(void)
{
  char buf[100];
  fd_set fdset;
  struct timeval td;
  int i;

  for (i=0; i<100; i++) buf[i]=0;
  
  /* Check if any data is waiting to be read */

  FD_ZERO(&fdset);
  FD_SET(fd[0], &fdset);
  
  td.tv_sec=0;
  td.tv_usec=0;

  if (select(100, &fdset, NULL, NULL, &td) && !moving) {

  /* read what GNU chess has to say */
    read(fd[0], buf, 100);

  /* computers move? */
    if (strstr(buf, "is:")) movePiece(strstr(buf, "is:")+4);

  /* players move? */
    if (strstr(buf, "Hint:")) {
      write(fd[3], strstr(buf, "Hint:")+6, 5);
      movePiece(strstr(buf, "Hint:")+6);
    }
  }

  return 1;
}



/* Keyboard event callback routine */

int keyb(MAV_object *o, MAV_keyboardEvent *ke)
{
  if (ke->movement==MAV_PRESSED) {
    
    switch (ke->key) {
    case 'q': /* quit */
      write(fd[3], "exit\n", 5); /* quit gnuchess */
      exit(1);
    default:  /* player makes a move (does this by sending "hint" to gnuchess) */
      if (whosgo && !moving) 
      {
	write(fd[3], "hint\n", 5);
      }
      else
      {
	printf("wait your turn\n");
      }
    }
  }

  return 1;
}



int main(int argc, char *argv[]) 
{
  MAV_viewParams vp;
  MAV_composite comp[2][6];
  int i, j;

/* Start GNU chess */

  pipe(fd);
  pipe(&fd[2]);

  if (fork()==0) {
    dup2(fd[1], 1);
    dup2(fd[2], 0);
    if (execvp("gnuchessx", NULL)) {
      fprintf(stderr, "Can't start gunchess\n");
      exit(1);
    }
  }

/* Initialise the Maverik system */

  mav_initialise(&argc, argv);

/* Add a new "device" to check for events from GNU chess */

  mav_deviceNew(NULL, NULL, checkGNUchess);

/* Initialise the board */

  for (i=0; i<64; i++) board[i]= -1;
  board[sq("a1")]= (WHITE<<4) | ROOK;
  board[sq("b1")]= (WHITE<<4) | KNIGHT;
  board[sq("c1")]= (WHITE<<4) | BISHOP;
  board[sq("d1")]= (WHITE<<4) | QUEEN;
  board[sq("e1")]= (WHITE<<4) | KING;
  board[sq("f1")]= (WHITE<<4) | BISHOP;
  board[sq("g1")]= (WHITE<<4) | KNIGHT;
  board[sq("h1")]= (WHITE<<4) | ROOK;
  board[sq("a2")]= (WHITE<<4) | PAWN;
  board[sq("b2")]= (WHITE<<4) | PAWN;
  board[sq("c2")]= (WHITE<<4) | PAWN;
  board[sq("d2")]= (WHITE<<4) | PAWN;
  board[sq("e2")]= (WHITE<<4) | PAWN;
  board[sq("f2")]= (WHITE<<4) | PAWN;
  board[sq("g2")]= (WHITE<<4) | PAWN;
  board[sq("h2")]= (WHITE<<4) | PAWN;

  board[sq("a7")]= (BLACK<<4) | PAWN;
  board[sq("b7")]= (BLACK<<4) | PAWN;
  board[sq("c7")]= (BLACK<<4) | PAWN;
  board[sq("d7")]= (BLACK<<4) | PAWN;
  board[sq("e7")]= (BLACK<<4) | PAWN;
  board[sq("f7")]= (BLACK<<4) | PAWN;
  board[sq("g7")]= (BLACK<<4) | PAWN;
  board[sq("h7")]= (BLACK<<4) | PAWN;
  board[sq("a8")]= (BLACK<<4) | ROOK;
  board[sq("b8")]= (BLACK<<4) | KNIGHT;
  board[sq("c8")]= (BLACK<<4) | BISHOP;
  board[sq("d8")]= (BLACK<<4) | QUEEN;
  board[sq("e8")]= (BLACK<<4) | KING;
  board[sq("f8")]= (BLACK<<4) | BISHOP;
  board[sq("g8")]= (BLACK<<4) | KNIGHT;
  board[sq("h8")]= (BLACK<<4) | ROOK;

/* Read in AC3D description of pieces  */

  mav_compositeReadAC3D("pawn.wh.ac", &comp[WHITE][PAWN], MAV_ID_MATRIX);
  mav_compositeReadAC3D("rook.wh.ac", &comp[WHITE][ROOK], MAV_ID_MATRIX);
  mav_compositeReadAC3D("knight.wh.ac", &comp[WHITE][KNIGHT], MAV_ID_MATRIX);
  mav_compositeReadAC3D("bishop.wh.ac", &comp[WHITE][BISHOP], MAV_ID_MATRIX);
  mav_compositeReadAC3D("queen.wh.ac", &comp[WHITE][QUEEN], MAV_ID_MATRIX);
  mav_compositeReadAC3D("king.wh.ac", &comp[WHITE][KING], MAV_ID_MATRIX);

  mav_compositeReadAC3D("pawn.bl.ac", &comp[BLACK][PAWN], MAV_ID_MATRIX);
  mav_compositeReadAC3D("rook.bl.ac", &comp[BLACK][ROOK], MAV_ID_MATRIX);
  mav_compositeReadAC3D("knight.bl.ac", &comp[BLACK][KNIGHT], MAV_ID_MATRIX);
  mav_compositeReadAC3D("bishop.bl.ac", &comp[BLACK][BISHOP], MAV_ID_MATRIX);
  mav_compositeReadAC3D("queen.bl.ac", &comp[BLACK][QUEEN], MAV_ID_MATRIX);
  mav_compositeReadAC3D("king.bl.ac", &comp[BLACK][KING], MAV_ID_MATRIX);

/* Register them as Maverik objects */

  for (i=0; i<2; i++) {
    for (j=0; j<6; j++) {
      piece[i][j]= mav_objectNew(mav_class_composite, &comp[i][j]);
    }
  }

/* Set perspective, background colour and backface culling */
  
  mav_windowPerspectiveSet(mav_win_all, 0.1, 1000, 70.0, 1.25); 
  mav_windowBackgroundColourSet(mav_win_all, 0.0, 0.5, 1.0);
  mav_windowBackfaceCullSet(mav_win_all, MAV_TRUE);

/* Set up mouse navigators */

#define LS 0.002
#define AS 0.0002

  mav_navigationMouse(mav_win_all, mav_navigationMouseDefault);
  mav_navigationMouseDefaultParams(mav_win_all, MAV_LEFT_BUTTON, mav_navigateYawFixedUp, LS, -AS,
			      mav_navigateForwardsFixedUp, LS, AS);
  mav_navigationMouseDefaultParams(mav_win_all, MAV_MIDDLE_BUTTON, mav_navigateYawFixedUp, LS, -AS,
			      mav_navigatePitch, LS, AS);
  mav_navigationMouseDefaultParams(mav_win_all, MAV_RIGHT_BUTTON, mav_navigateRightFixedUp, LS, AS,
			      mav_navigateUpFixedUp, LS, AS);

/* Define keyboard interaction callback */

  mav_callbackKeyboardSet(mav_win_all, mav_class_world, keyb);

/* Create a lighting model and light source */

  mav_paletteLightingModelSet(mav_palette_default, 0.4, 0.4, 0.4, 1.0, 1);
  mav_paletteLightSet(mav_palette_default, 1,  0.0, 0.0, 0.0, 0.0,  1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0);
  mav_paletteLightPos(mav_palette_default, 1,  mav_vectorSet(150,50,0));

/* Define the viewing parameters */

  vp.eye.x= 3.5; /* eye position */
  vp.eye.y= -5.0;
  vp.eye.z= 2.25;

  vp.view.x= 0;  /* view direction */
  vp.view.y= 1;
  vp.view.z= 0;

  vp.up.x= 0;  /* view up and World up */
  vp.up.y= 0;
  vp.up.z= 1;
  vp.fixed_up= vp.up;
  vp.mod= NULL;
  
/* Bind the viewing parameters to the window */

  mav_windowViewParamsSet(mav_win_all, &vp);

/* Main loop */

  while (1) {

/* Check for, and act on, any events */

    mav_eventsCheck();

/* Do what needs to be done at the start of a frame */

    mav_frameBegin();

/* Draw the board */

    drawboard();

/* Do what needs to be done at the end of a frame */

    mav_frameEnd();
  }
}


