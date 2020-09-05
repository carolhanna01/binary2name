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

/* Nothing to to with build.c */

#define UNKNOWN 	0
#define CANAL 		1
#define PAVEMENT 	2
#define GRASS 		3
#define STREET0		10
#define STREET1		11
#define STREET2		12
#define STREET3		13
#define STREET4		14
#define STREET5		15

/* Maximum radial extent of city */
static float city_mre[3];

/* City center coordinates */
static float centx[3],centy[3];

/* Largest maximum radial extent */
static float global_city_mre;

