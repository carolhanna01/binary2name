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


/* Define colors and color mapping of original LC */

/* Prototypes */

void GetLetterColor (int, int, float *);
void BuildColorTable ();
int MapColorCode (int ColorCode, int CityCode);

/* Defines to pass to the conversion routine to define which city we
   are working on at the moment. */
   
#define MANHATTAN_COLOR_CODE			0
#define AMSTERDAM_COLOR_CODE			1
#define KARLSRUHE_COLOR_CODE			2

/* These are the maximum number that we should expect in the data files. */

#define MANHATTAN_MAX_COLOR_INDEX	10
#define AMSTERDAM_MAX_COLOR_INDEX	51
#define KARLSRUHE_MAX_COLOR_INDEX	49

/* Maximum number of random shades for each color and brightness difference 
   between shades */

#define NUM_SHADES			5
#define SHADE_DIFFERENCE		0.1

/* Fixed Colors in the Table */

#define CANAL_COLOR			11*NUM_SHADES
#define GRASS_COLOR			12*NUM_SHADES
#define PAVEMENT_COLOR			13*NUM_SHADES
