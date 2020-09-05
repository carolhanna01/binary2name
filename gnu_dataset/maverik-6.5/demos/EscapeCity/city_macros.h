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

#ifndef CITY_MACROS_H
#define CITY_MACROS_H

/*
#define HMD
#define DISPLAY_LISTS
#define STEREO
*/
#define COMPOSITES
#define TEXTURES

#ifdef HMD
#define STEREO
#endif

#define BLOCK_WIDTH 16.0
#define PAVE_WIDTH 4.0
#define PAVE_HEIGHT 0.15
#define MARK_LENGTH 2.0
#define MARK_WIDTH 0.15
#define JUNC_LENGTH 1.0
#define JUNC_WIDTH 0.15
#ifdef MAV_GL
#define DELTA_HEIGHT 0.05
#else
#define DELTA_HEIGHT 0.01
#endif

#define MAX_BUILDING_LEN 2

#define MAX_CANDIDATE_EDGES 10000

/* The maximum number of candidate cells where aggregation can take
   place limitted only by available memory. */
#define MAX_CAND 10000

/* This is one less than the number of cells which will be generated. */
#define RUN_LENGTH 1000

/* The size of the grid on which aggregation takes place. */
#define SPACE_SIZE 25

/* Weighting the aggregation process. */
#define ZERO_NEIGH_WEIGHT 70
#define ONE_NEIGH_WEIGHT 10
#define TWO_NEIGH_WEIGHT 10
#define THREE_NEIGH_WEIGHT 10

#define IN_FRONT 0
#define BEHIND 1

#define MAX_HEIGHT_DIFF 2.0

#define MAX_BILLBOARDS 1000
#define MAX_OCCLUDERS 10000

#define MAT_GRASS 99
#ifdef TEXTURES
#define MAT_ASPHALT -1
#else
#define MAT_ASPHALT 98
#endif
#define MAT_PAVEMENT 97
#define MAT_MARK 96
#define MAT_CONCRETE 95
#define MAT_GLASS 94
#define MAT_STRUTS 93
#define MAT_DIRT 92
#define MAT_WHITE 91
#define MAT_WHITE2 90
#define MAT_WHITE3 89
#define MAT_METAL 88
#define MAT_DARK_BLUE 87
#ifndef TEXTURES
#define MAT_NOTEX 86
#define MAT_NOTEX2 85
#define MAT_NOTEX3 84
#else
#define MAT_NOTEX 91
#endif
#define MAT_TROUSERS1 83
#define MAT_TROUSERS2 82
#define MAT_TROUSERS3 81
#define MAT_JUMPER1 80
#define MAT_JUMPER2 79
#define MAT_JUMPER3 78

#define MAV_COLOUR_YELLOW 1

#ifdef TEXTURES
#define TEX_ASPHALT 1
#else
#define TEX_ASPHALT -1
#endif
#define TEX_BRICKS 2
#define TEX_BRICKS2 3
#define TEX_DIRT 4
#define TEX_GRASS 5
#define TEX_MANC 6
#define TEX_SKY 7
#define TEX_TREE1 8
#define TEX_TREE2 9
#define TEX_TREE3 10
#define TEX_PAVEMENT 11
#ifdef TEXTURES
#define TEX_BUILDING_SIDE1 12
#define TEX_BUILDING_SIDE2 13
#define TEX_BUILDING_SIDE3 14
#define TEX_BUILDING_SIDE4 15
#define TEX_BUILDING_SIDE5 16
#define TEX_BUILDING_SIDE6 17
#define TEX_BUILDING_SIDE7 18
#define TEX_BUILDING_TOP 19
#define TEX_WINDOW 20
#define TEX_WINDOW_TOP 21
#define TEX_WINDOW_ROOF 22
#define TEX_WINDOW2 23
#define TEX_JUMPER1 24
#define TEX_JUMPER2 25
#define TEX_TROUSERS1 26
#define TEX_TROUSERS2 27
#else
#define TEX_BUILDING_SIDE1 -1
#define TEX_BUILDING_SIDE2 -1
#define TEX_BUILDING_SIDE3 -1
#define TEX_BUILDING_SIDE4 -1
#define TEX_BUILDING_SIDE5 -1
#define TEX_BUILDING_SIDE6 -1
#define TEX_BUILDING_SIDE7 -1
#define TEX_BUILDING_TOP -1
#define TEX_WINDOW -1
#define TEX_WINDOW_TOP -1
#define TEX_WINDOW_ROOF -1
#define TEX_WINDOW2 -1
#endif

#define NUM_COMP 15
#define COMP_TRAFFICLIGHT 0
#define COMP_STREETLIGHT 1
#define COMP_FENCE 2
#define COMP_FOUNTAIN 3
#define COMP_RED_FOUNTAIN 4
#define COMP_DOOR 5
#define COMP_EASTER 6
#define COMP_GAZEBO 7
#define COMP_STHENGE 8
#define COMP_LEGOMAN 9
#define COMP_RECOGNIZER 10
#define COMP_FISH 11
#define COMP_STANDINGMAN 12
#define COMP_WOMAN 13
#define COMP_CAROSEL 14

#endif
