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

/* Virtual City Builder (VCB) by John Bowers.

   VCB creates files which describe virtual
   cities generated using a variety of aggregation
   algorithms. Currently implemented is a very
   flexible version of Hillier and Hanson's
   model of aggregation. This aggregates open
   cell and closed cell doublets so that open
   cells have a full face-wise join with each other.
   The probability distributions which determine
   how the closed cells join their associated
   open cell can be set in various ways. For example,
   VCB can be set so that a closed cell will
   tend to aggregate onto another closed cell or
   so that closed cells will avoid aggregating in
   places where they would have more than one neighbour,
   amongst many other possibilities. This enables
   a variety of settlement types to be generated
   within the same overall framework.

   The files created by this program can be converted
   to DIVE files using the VCBtoDIVE converter program.

   Note: VCB always generates a file called "CellsDataFile"
   and this is the name that VCBtoDIVE must use. VCB will
   overwrite any older file called "CellsDataFile" without
   warning so be careful to rename files you want to save
   or move them to a different folder (or change the code!).

   Diffusion Limitted Agrregation and Dielectric Breakdown
   algorithms will soon be implemented too. */

#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include "maverik.h"
#include "city_macros.h"
#include "city_types.h"

int space[SPACE_SIZE][SPACE_SIZE];
int cand[MAX_CAND][2];
int cells[RUN_LENGTH][4];
int numCand= 0;
int numCell= 0;
int neighList [4][2]= { { 0, 1 },
			      { 1, 0 },
			      { 0, -1 },
			      { -1, 0 } };
int weightTable[4]= { ZERO_NEIGH_WEIGHT, ONE_NEIGH_WEIGHT,
			TWO_NEIGH_WEIGHT, THREE_NEIGH_WEIGHT };
/*
This array contains the weightings used to decide whether to build a closed
cell in a given location, depending on the number of neighbours that cell has. For
example, {20, 40, 40, 0} would mean that there could be no closed cell at a locati
on surrounded by 3 closed cells and cells with 1 or 2 closed neighbours are equally
attractive building locations with 0 half as attractive. It is not necessary for the
elements in the list to sum to 100 though that might be convenient. It is necessar
y however to select numbers so that building onto the seed can take place.
i.e. ONE_NEIGH_WEIGHT <> 0.
*/

char symbolList []= ". +";

#if 0
static void
saveSpaceToFile (void)
{
  int i,j;
  int c;
  int found;

  fprintf(stdout,"size %d number %d\n", SPACE_SIZE, numCell);
  for (i = 1; i <= numCell; i++)
    fprintf(stdout,"cell %d open %d %d closed %d %d\n", i,
	    cells[i][0], cells[i][1], cells[i][2], cells[i][3]);

  for (j= SPACE_SIZE-1; j >= 0; j--)
    {
      for (i= 0; i< SPACE_SIZE; i++)
	{
	  found= 0;
	  for (c= 1; c<= numCell; c++)
	    {
	      if (cells[c][2] == i && cells[c][3] == j)
		{
		  fprintf(stdout, "X");
		  found= 1;
		}
	    }
	  if (!found)
	    fprintf(stdout, "O");
	}
      fprintf(stdout, "\n");
    }
}
#endif

/*
This function steps through the candidates list until it finds
an entry to matching posX, posy. It then deletes this entry
and moves all the remainder 'up' one place.
If it finds no match, it has no effect on the candidates list.
This function assumes that there are no duplicates on the
candidates list. It is quite possibly better implemented
than by the code below which nests a couple of loops and a
conditional.
*/
static void
deleteFromCand
(int posX, int posY)
{
  int i, j;
  for (i = 0; i < numCand; i++)
    {
      if ((cand[i][0] == posX) && (cand[i][1] == posY))
	{
	  for (j = i; j < numCand; j++)
	    {
	      cand[j][0] = cand[j+1][0];
	      cand[j][1] = cand[j+1][1];
	    }
	  /* The next two lines may be superfluous.
	     cand[numCand-1][0] = -1;
	     cand[numCand-1][1] = -1; */
	  numCand--;
	  break;
	}
    }
}


/*
This function determines the free spaces which neighbour posX, posY
and adds them to the candidates list if they are not already on it.
If a free space is already on the candidates list, the call of
'deleteFromCand' will delete its earlier occurrence with the new
occurrence being appended to the  end of the list. The unconditional
call of 'deleteFromCand' should be safe as this function has no effect
on the candidates list if no match is found.
*/
static void
addToCandNeighsOf
(int posX, int posY)
{
  int i, neighX, neighY;
  for (i = 0; i < 4; i++)
    {
      neighX = posX + neighList[i][0];
      neighY = posY + neighList[i][1];
      if ((neighX > -1) && (neighX < SPACE_SIZE) && (neighY > -1)
	  && (neighY < SPACE_SIZE) && (space[neighX][neighY] == 0))
	/* Only add a neighbour to the list if it is within range and the space is unoccupied. */
	{
	  deleteFromCand(neighX,neighY);
	  cand[numCand][0] = neighX;
	  cand[numCand][1] = neighY;
	  numCand++;
	  if (numCand > MAX_CAND+1)
	    {
	      fprintf(stdout,"Candidate list is out of memory\n");
	      exit(0);
	    }
	}
    }
}


static int
noVertexOnlyJoin
(int posX, int posY)
{
  if ((space[posX][posY+1] > 0) || (space[posX+1][posY] > 0) ||
      (space[posX][posY-1] > 0) || (space[posX][posY-1] > 0))
    return(1);
  /* Return 1 (true) if there's any full facewise join with a closed cell. */
  else
    {
      if ((space[posX-1][posY-1] > 0) || (space[posX+1][posY-1] > 0) ||
	  (space[posX-1][posY+1] > 0) || (space[posX+1][posY+1] > 0))
	return(0);
      /* Return 0 (false) if there's any vertext join. */
      else
	return(1);
    }
}

/*
  This function selects a location for a closed cell
  given that its associated open cell is posX, posY.
  It calculates the weight associated with each possible location
  for the closed cell and then makes a weighted probability selection
  if the total of the weights is not zero.
  It returns the weight total and (if this is not zero) it updates
  the space matrix and calls a function to update the candidates list.
  */
static int
selectClosed
(int posX, int posY)
{
  int i, j, k, neighX, neighY, numNeighNeigh, s, closedX, closedY;
  int accum = 0;
  int weight [4] = {25, 25, 25, 25};
  int weightTotal = 0;
  
  for (i = 0; i < 4; i++)
    {
      neighX = posX + neighList[i][0];
      neighY = posY + neighList[i][1];
      if ((neighX > -1) && (neighX < SPACE_SIZE) &&
	  (neighY > -1) && (neighY < SPACE_SIZE) && (space[neighX][neighY] == 0)
	  && (noVertexOnlyJoin(neighX, neighY) == 1))
	{
	  /*
	    If a neighbour to potential open cell is within range, the space is unoccupied
	    and there is no vertext only join with any existing closed cell,
	    then count up how many neighbours that neighbour has and put the weight
	    associated with this number (given by consulting the weight table) into the
	    weight array in a slot corresponding to the neighbours vector displacement
	    (N = 0, E = 1 etc) from the open cell. Also accumulate the weight total.
	    */
	  numNeighNeigh = 0;
	  for (j = 0; j < 4; j++)
	    {
	      if (space[neighX + neighList[j][0]][neighY += neighList[j][1]] > 0)
		numNeighNeigh++;
	    }
	  weight[i] = weightTable[numNeighNeigh];
	  weightTotal = weightTotal + weight[i];
	}
      else
	/*
	  If a neighbour to a potential open cell is either out of range or already
	  occupied, put 0 in its corresponding weight. This will ensure that it doesn't
	  get selected.
	  */
	{
	  weight[i] = 0;
	}
    }
  
  if (weightTotal > 0)
    /*
      A neighbour to the open cell is selected with a probability
      weighted by the weights which have been calculated above by
      examining the numbers
      of closed cells around each neighbour.
      */
    {
      s = (int)(mav_random()*weightTotal);
      for (k = 0; k < 4; k++)
	{
	  accum = accum + weight[k];
	  if (s < accum) break;
	}
      /*
	k = 0;
	do
	{
	accum = accum + weight[k++];
	}
	while (s >= accum);
	*/
      closedX = posX + neighList[k][0];
      closedY = posY + neighList[k][1];
      space[closedX][closedY] = ++numCell;
      cells[numCell][2] = closedX;
      cells[numCell][3] = closedY;
      /*
	Number of cells in array incremented and entered into space array
	to signify the order in which this closed cell appeared in this run.
	The coordinates of the closed cell are also written to the cells array.
	*/
      deleteFromCand(closedX,closedY);
    }
  return (weightTotal);
  /* Note 0 returned if no cell can be selected. */
}

/*
This function randomly selects a location for a possible
open cell from the candidates list, only updating the space array
with a negative number signifying the open cell if a neighbouring
closed cell has been found. If no closed cell can be found
for a selected open cell, the function loops to try and find another
open cell. In this way, open cells only appear when closed cells
can be found and the 'doublet' stays intact.
A selected location is deleted from the candidates list if an open
cell is placed there or if - once selected - no closed cell can be
found to adjoin. This gets rid of 'encircled' candidates.
New candidates are only added once a doublet has been put in place.
*/
static void
selectOpen (void)
{
  int s, openX, openY;

  do {
    if (numCand == 0) return;
    else s = (int)(numCand*mav_random());

    openX = cand[s][0];
    openY = cand[s][1];
    deleteFromCand(openX,openY);
  } while (selectClosed(openX,openY) == 0);

  space[openX][openY] = -numCell;
  cells[numCell][0] = openX;
  cells[numCell][1] = openY;

  deleteFromCand(openX,openY);
  addToCandNeighsOf(openX,openY);
}

#if 0
static void
Generate_River (void)
{
  float rnd= mav_random();
  int x, y;
  int dir;
  int wid;
  int okay;
  int i;

  wid= (int)(5*mav_random());

  if (rnd < 0.25)
    {
      x= 0;
      y= (int)(SPACE_SIZE*mav_random());
      if (y+wid-1 >= SPACE_SIZE) y= SPACE_SIZE-wid;
      dir= 1; /* right */
    }
  else if (rnd < 0.5)
    {
      x= (int)(SPACE_SIZE*mav_random());
      y= 0;
      if (x+wid-1 >= SPACE_SIZE) x= SPACE_SIZE-wid;
      dir= 2; /* down */
    }
  else if (rnd < 0.75)
    {
      x= SPACE_SIZE-1;
      y= (int)(SPACE_SIZE*mav_random());
      if (y+wid-1 >= SPACE_SIZE) y= SPACE_SIZE-wid;
      dir= 3; /* left */
    }
  else
    {
      x= (int)(SPACE_SIZE*mav_random());
      y= SPACE_SIZE-1;
      if (x+wid-1 >= SPACE_SIZE) x= SPACE_SIZE-wid;
      dir= 4; /* up */
    }

  okay= 1;
  while (okay)
    {
      /* add river segment */
      switch (dir)
	{
	case 1:
	case 3:
	  fprintf(stdout, "%d,%d to %d,%d\n", x,y, x,y+wid-1);
	  for (i= y; i< y+wid; i++)
	    space[x][i]= -9999; /* river */
	  break;
	case 2:
	case 4:
	  fprintf(stdout, "%d,%d to %d,%d\n", x,y, x+wid-1,y);
	  for (i= x; i< x+wid; i++)
	    space[i][y]= -9999; /* river */
	  break;
	}

      /* move x,y */
      switch (dir)
	{
	case 1 :
	  x++;
	  if (x == SPACE_SIZE)
	    okay= 0;

	  rnd= mav_random();
	  if (rnd < 0.333)
	    {
	      y--;
	      if (y == -1) y= 0;
	    }
	  else if (rnd > 0.666)
	    {
	      y++;
	      if (y == SPACE_SIZE) y= SPACE_SIZE-1;
	    }
	  break;
	case 2 :
	  y++;
	  if (y == SPACE_SIZE)
	    okay= 0;

	  rnd= mav_random();
	  if (rnd < 0.333)
	    {
	      x--;
	      if (x == -1) x= 0;
	    }
	  else if (rnd > 0.666)
	    {
	      x++;
	      if (x == SPACE_SIZE) x= SPACE_SIZE-1;
	    }
	  break;
	case 3 :
	  x--;
	  if (x == -1)
	    okay= 0;

	  rnd= mav_random();
	  if (rnd < 0.333)
	    {
	      y--;
	      if (y == -1) y= 0;
	    }
	  else if (rnd > 0.666)
	    {
	      y++;
	      if (y == SPACE_SIZE) y= SPACE_SIZE-1;
	    }
	  break;
	case 4 :
	  y--;
	  if (y == -1)
	    okay= 0;

	  rnd= mav_random();
	  if (rnd < 0.333)
	    {
	      x--;
	      if (x == -1) x= 0;
	    }
	  else if (rnd > 0.666)
	    {
	      x++;
	      if (x == SPACE_SIZE) x= SPACE_SIZE-1;
	    }
	  break;
	}
    }
}
#endif

/*
This function puts a seed cell in the middle of the space
and initialises the candidates list.
*/
static void
initialise
(int num_seeds)
{
  int x, y;
  int i;

  /* Seeds the random number generator. */
  /*  srand(0);*/

  for (x= 0; x < SPACE_SIZE; x++)
    {
      for (y= 0; y < SPACE_SIZE; y++)
	/* 0 used to fill empty space. */
	space[x][y]= 0;
    }

  /*
    Negative number used to signify an open cell in the space array.
    -1 denotes first open cell selected in this run.
    */
  for (x= 0; x < MAX_CAND; x++)
    {
      for (y= 0; y < 2; y++)
	/* -1 used to fill empty space. 0 cannot be used as 0 is a=20
	   coordinate value. */
	cand[x][y]= -1;
    }

  numCand= 0;
  for (i= 0; i< num_seeds; i++)
    {
      x= (int)(SPACE_SIZE*mav_random());
      y= (int)(SPACE_SIZE*mav_random());
      space[x][y]= -1;
      cells[i+1][0]= x;
      cells[i+1][1]= y;

      cand[i][0]= x;
      cand[i][1]= y+1;
      cand[i+1][0]= x+1;
      cand[i+1][1]= y;
      cand[i+2][0]= x;
      cand[i+2][1]= y-1;
      cand[i+3][0]= x-1;
      cand[i+3][1]= y;
      numCand += 4;
      selectClosed(x, y);
    }
}


#if 0
static void
displaySpace
(int iter)
{
  int x, y, loc;

  fprintf(stdout,"\nIteration number %d\n", iter);
  for (y = 0; y < SPACE_SIZE; y++)
    {
      fprintf(stdout,"\n");
      for (x = 0; x < SPACE_SIZE; x++)
	loc = space[x][y];
      if (loc == 0)
	fprintf(stdout,"%c", symbolList[0]);
      else if (loc > 0)
	fprintf(stdout,"%c", symbolList[1]);
      else
	fprintf(stdout,"%c", symbolList[2]);
    }
  fprintf(stdout,"\n");
  /*
  for (y = 0; y < SPACE_SIZE; y++)
    {
      fprintf(stdout,"\n");
      for (x = 0; x < SPACE_SIZE; x++)
	fprintf(stdout,"%+1d",space[x][y]);
    }
    */
  fprintf(stdout,"\n");
}
#endif

int
vcb_main
(int num_seeds, int num_cells)
{
  int i;

  initialise (num_seeds);

  for (i= 0; num_cells > 0 && (numCand > 0) && (i< RUN_LENGTH); i++)
    {
      selectOpen();
      num_cells--;
    }


  fprintf(stdout,"Building completed with %d cells filled.\n", numCell);
  return (numCell);
}
