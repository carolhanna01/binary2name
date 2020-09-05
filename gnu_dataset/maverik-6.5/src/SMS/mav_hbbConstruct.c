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


#include "mavlib_sms.h"
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#define MAV_HBB_THRESHOLD 100000000

typedef struct {
  int num_x;
  int num_y;
  int num_z;
  MAV_list **cells;
  MAV_BB box;
  MAV_vector voxel_size;
} MAV_HBBClusterGrid;


void mavlib_HBBCalculateSurfaceArea (MAV_BB *bb, float *sa) {
  /* return the surface area of a bounding box */

  *sa= 2.0 * (bb->max.x - bb->min.x) * (bb->max.z - bb->min.z) +
	2.0 * (bb->max.y - bb->min.y) * (bb->max.z - bb->min.z) +
	2.0 * (bb->max.y - bb->min.y) * (bb->max.x - bb->min.x);
}

float mavlib_HBBCalculateSurfaceAreaIncrease (MAV_HBBCluster *node, MAV_BB *bb, float *new_sa) {
  /* calculate the increase in surface area of 'node' when 'bb' is added
     to it as a child */

  MAV_BB new_bb;
  float sa;

  /* calculate new bound */
  new_bb.min.x= (node->box.min.x < bb->min.x) ? node->box.min.x : bb->min.x;
  new_bb.min.y= (node->box.min.y < bb->min.y) ? node->box.min.y : bb->min.y;
  new_bb.min.z= (node->box.min.z < bb->min.z) ? node->box.min.z : bb->min.z;

  new_bb.max.x= (node->box.max.x > bb->max.x) ? node->box.max.x : bb->max.x;
  new_bb.max.y= (node->box.max.y > bb->max.y) ? node->box.max.y : bb->max.y;
  new_bb.max.z= (node->box.max.z > bb->max.z) ? node->box.max.z : bb->max.z;

  mavlib_HBBCalculateSurfaceArea (&new_bb, &sa);
  *new_sa= sa;

  return (sa - node->surface_area);
}

float mavlib_HBBParentCost (MAV_HBBCluster *node, MAV_BB *bb) {
  /* calculate the insertion cost for the branch of the hierarchy
     running from the parent of the insertion point back up to the
     root */

  float area_diff;
  float sa;

  if (node == NULL) return (0.0);

  /* calculate increase in surface area */
  area_diff= mavlib_HBBCalculateSurfaceAreaIncrease (node, bb, &sa);

  /* if bounding box has changed, sum cost and add cost of parents */
  if (area_diff > 0.0)
    return (node->num_children * area_diff + mavlib_HBBParentCost (node->parent, bb));
  else
    return (0.0);
}

float mavlib_HBBCalculateBranchNodeCost (MAV_HBBCluster *node, MAV_BB *bb) {
  /* calculate the cost of inserting an object as another child
     of an internal node */

  float sa;

  mavlib_HBBCalculateSurfaceAreaIncrease (node, bb, &sa);

  /* sum cost, and add to parent cost */
  return ((node->num_children + 1.0) * (sa - node->surface_area) +
        sa + mavlib_HBBParentCost (node->parent, bb));
}

float mavlib_HBBCalculateBranchLeafCost (MAV_HBBCluster *node, MAV_BB *bb) {
  /* calculate the cost of inserting an object as a leaf */

  float sa;

  mavlib_HBBCalculateSurfaceAreaIncrease (node, bb, &sa);

  /* sum cost, and add to parent cost */
  return (2.0 * sa + mavlib_HBBParentCost (node->parent, bb));
}

void mavlib_HBBRecalculateNodeBound (MAV_HBBCluster *node) {
  /* recalculates the bounding volume for a node and its parent nodes */
MAV_HBBChild *ch;
MAV_BB old_bb;

  old_bb= node->box;

  /* calculate bounding volume of node */
  ch= node->children;
  while (ch != NULL) {
    if (ch->child->box.min.x < node->box.min.x)
	node->box.min.x= ch->child->box.min.x;
    if (ch->child->box.min.y < node->box.min.y)
	node->box.min.y= ch->child->box.min.y;
    if (ch->child->box.min.z < node->box.min.z)
	node->box.min.z= ch->child->box.min.z;

    if (ch->child->box.max.x > node->box.max.x)
	node->box.max.x= ch->child->box.max.x;
    if (ch->child->box.max.y > node->box.max.y)
	node->box.max.y= ch->child->box.max.y;
    if (ch->child->box.max.z > node->box.max.z)
	node->box.max.z= ch->child->box.max.z;

    ch= ch->next;
  }

  /* store its new surface area */
  mavlib_HBBCalculateSurfaceArea (&(node->box), &(node->surface_area));

  /* if the size of the bound has changed, update pos/size and visit parent */
  if ((node->box.min.x != old_bb.min.x) ||
      (node->box.min.y != old_bb.min.y) ||
      (node->box.min.z != old_bb.min.z) ||
      (node->box.max.x != old_bb.max.x) ||
      (node->box.max.y != old_bb.max.y) ||
      (node->box.max.z != old_bb.max.z)) {
    node->pos= mav_vectorAdd (mav_vectorScalar (node->box.min, 0.5),
		mav_vectorScalar (node->box.max, 0.5));
    node->size= mav_vectorDotProduct (
		mav_vectorSub (node->box.max, node->box.min),
		mav_vectorSub (node->box.max, node->box.min));
    if (node->parent != NULL)
      mavlib_HBBRecalculateNodeBound (node->parent);
  }
}

void mavlib_HBBInsertObjectAsNode (MAV_HBBCluster *node, MAV_object *obj, MAV_BB *bb) {
  /* insert object as new child of an internal node */

  MAV_HBBChild *new_child;
  MAV_HBBCluster *new_cluster;

  new_child= (MAV_HBBChild *) mav_malloc (sizeof (MAV_HBBChild));
  new_cluster= (MAV_HBBCluster *) mav_malloc (sizeof (MAV_HBBCluster));

  mavlib_HBBCalculateSurfaceArea (bb, &(new_cluster->surface_area));
  new_cluster->box= *bb;
  new_cluster->pos= mav_vectorAdd (mav_vectorScalar (bb->min, 0.5),
		mav_vectorScalar (bb->max, 0.5));
  new_cluster->size= mav_vectorDotProduct (mav_vectorSub (bb->max, bb->min),
		mav_vectorSub (bb->max, bb->min));
  new_cluster->obj= obj;
  new_cluster->num_children= 0;
  new_cluster->children= NULL;
  new_cluster->parent= node;

  new_child->child= new_cluster;
  new_child->next= node->children;
  node->children= new_child;
  node->num_children ++;

  /* make sure all parent bounding volumes are correct */
  mavlib_HBBRecalculateNodeBound (node);
}

void mavlib_HBBInsertObjectAsLeaf (MAV_HBBCluster *node, MAV_object *obj, MAV_BB *bb) {
  /* insert object as a leaf - the old leaf or node and the object are made
     children of a new parent node that is placed at the same place
     as the old leaf or node */

  MAV_HBBChild *child_left;
  MAV_HBBCluster *cluster_left;
  MAV_HBBChild *child_right;
  MAV_HBBCluster *cluster_right;
  MAV_HBBChild *ch;

  child_left= (MAV_HBBChild *) mav_malloc (sizeof (MAV_HBBChild));
  child_right= (MAV_HBBChild *) mav_malloc (sizeof (MAV_HBBChild));
  cluster_left= (MAV_HBBCluster *) mav_malloc (sizeof (MAV_HBBCluster));
  cluster_right= (MAV_HBBCluster *) mav_malloc (sizeof (MAV_HBBCluster));

  mavlib_HBBCalculateSurfaceArea (bb, &(cluster_right->surface_area));
  cluster_right->box= *bb;
  cluster_right->pos= mav_vectorAdd (mav_vectorScalar (bb->min, 0.5),
		mav_vectorScalar (bb->max, 0.5));
  cluster_right->size= mav_vectorDotProduct (mav_vectorSub (bb->max, bb->min),
		mav_vectorSub (bb->max, bb->min));
  cluster_right->obj= obj;
  cluster_right->num_children= 0;
  cluster_right->children= NULL;
  cluster_right->parent= node;

  cluster_left->box= node->box;
  cluster_left->pos= node->pos;
  cluster_left->size= node->size;
  cluster_left->obj= node->obj;
  /* left cluster has all children of original node */
  cluster_left->num_children= node->num_children;
  cluster_left->children= node->children;
  cluster_left->parent= node;

  /* need to update children to point at correct parent */
  ch= cluster_left->children;
  while (ch != NULL) {
    ch->child->parent= cluster_left;
    ch= ch->next;
  }

  child_right->child= cluster_right;
  child_right->next= NULL;
  child_left->child= cluster_left;
  child_left->next= child_right;

  node->obj= NULL;
  node->children= child_left;
  node->num_children= 2;

  /* make sure all parent bounding volumes are correct */
  mavlib_HBBRecalculateNodeBound (node);
}

void mavlib_HBBSearchHierarchyPatch (MAV_HBBCluster *node, MAV_HBBCluster **optimum, float *smallest, MAV_BB *bb) {
  /* search the hierarchy to find the optimal place to insert
     a bounding box */

  MAV_HBBChild *ch;
  float min;
  float cost;
  float ainc;
  float sa;

  /* if current search node isn't a leaf */
  if (node->children != NULL) {
  /* calculate the total cost for the branch */
    cost= mavlib_HBBCalculateBranchNodeCost (node, bb);

	  /* store if smallest */
    if (cost <= *smallest) {
      *smallest= cost;
      *optimum= node;
    }

  /* search children to find smallest increase in area caused */
    ch= node->children;
    min= 10000000000.0;
    while (ch != NULL) {
      ainc= mavlib_HBBCalculateSurfaceAreaIncrease (ch->child, bb, &sa);
      if (ainc < min) min= ainc;
      ch= ch->next;
    }

  /* and continue searching down the best cost paths */
    ch= node->children;
    while (ch != NULL) {
      ainc= mavlib_HBBCalculateSurfaceAreaIncrease (ch->child, bb, &sa);
      if (ainc == min)
        mavlib_HBBSearchHierarchyPatch (ch->child, optimum, smallest, bb);
      ch= ch->next;
    }
  } else {
  /* current node is a leaf node */
    cost= mavlib_HBBCalculateBranchLeafCost (node, bb);

  /* store if cost is smallest */
    if (cost <= *smallest) {
      *smallest= cost;
      *optimum= node;
    }
  }
}

int mav_HBBObjectAdd (MAV_SMS *sms, MAV_object *obj) {
  /* add an object to the SMS */

  MAV_HBB *hbb= (MAV_HBB *) mav_SMSDataGet (sms);
  float smallest_inc;
  MAV_HBBCluster *optimal_place;
  MAV_BB bb;

  mav_callbackBBExec (mav_win_current, obj, &bb);

  if (hbb->root == NULL) {
  /* first object */
    hbb->root= (MAV_HBBCluster *) mav_malloc (sizeof (MAV_HBBCluster));
    hbb->root->box= bb;
    hbb->root->pos= mav_vectorAdd (mav_vectorScalar (bb.min, 0.5),
		mav_vectorScalar (bb.max, 0.5));
    hbb->root->size= mav_vectorDotProduct (mav_vectorSub (bb.max, bb.min),
		mav_vectorSub (bb.max, bb.min));
    mavlib_HBBCalculateSurfaceArea (&bb, &(hbb->root->surface_area));
    hbb->root->num_children= 0;
    hbb->root->obj= obj;
    hbb->root->children= NULL;
    hbb->root->parent= NULL;
  } else {
  /* find best place for insertion */
    smallest_inc= 1000000000000.0;
    optimal_place= hbb->root;
    mavlib_HBBSearchHierarchyPatch (hbb->root, &optimal_place, &smallest_inc, &bb);

  /* add object into hierarchy */
    if ((optimal_place->children == NULL)||(smallest_inc > MAV_HBB_THRESHOLD))
      mavlib_HBBInsertObjectAsLeaf (optimal_place, obj, &bb);
    else
      mavlib_HBBInsertObjectAsNode (optimal_place, obj, &bb);
  }

  if (mav_opt_objectTables) mav_objectTablesSMSAdd (obj, sms);

  hbb->size++;

  return 1;
}

void mavlib_HBBCalculateBoundFit (MAV_BB *bb, int level, float *fit, MAV_BB *all_bb) {
  /* generates a metric describing how well a bounding box fits
     the voxel size of a particular level in the cluster-grid.
     The metric is taken as the difference between the length of
     the longest side of the bounding volume and the voxel size */

  /* all_bb is the bounding box of all objects currently in the HBB and
     all objects waiting to be inserted */

  float max, side;
  MAV_vector voxel_size;

  voxel_size.x= (all_bb->max.x - all_bb->min.x)/ pow (2.0, (float) level);
  voxel_size.y= (all_bb->max.y - all_bb->min.y)/ pow (2.0, (float) level);
  voxel_size.z= (all_bb->max.z - all_bb->min.z)/ pow (2.0, (float) level);

  max= bb->max.x - bb->min.x;
  *fit= fabs (voxel_size.x - max);

  side= bb->max.y - bb->min.y;
  if (side > max) {
    max= side;
    *fit= fabs (voxel_size.y - max);
  }

  side= bb->max.z - bb->min.z;
  if (side > max)
    *fit= fabs (voxel_size.z - max);
}

void mavlib_HBBAddObjectToVoxelList (MAV_object *obj, MAV_HBBClusterGrid *grid, int i, int j, int k) {
  /* adds an object to the list of objects stored in voxel i,j,k in
     gridcell level grid */

  int cell_num;

  cell_num= k * grid->num_x * grid->num_y + j * grid->num_x + i;

  /* create a new list if this is the first object in this voxel */
  if (grid->cells[cell_num] == NULL) 
    grid->cells[cell_num]= mav_listNew (); 

  /* add the object to the voxel list */
  mav_listItemAdd (grid->cells[cell_num], (void *) obj);
}

void mav_HBBConstructFromSMS (MAV_SMS *hierarchy, MAV_SMS *from_sms) {
  /* construct a HBB in 'hierarchy' using the objects in 'from_sms' */
  int i,j,k, n, nprims, num, grid_depth, best_level, cell_addr;
  float fit, best_fit;
  MAV_BB all_bb, current_bb, object_bound;
  MAV_vector centre, obj_cent;
  MAV_HBBClusterGrid *cluster_grid;
  MAV_object *obj, *this_obj;

  /* set some scene info */
  mav_SMSCallbackSizeExec(from_sms, &nprims);
  if (mav_opt_output==MAV_VERBOSE) fprintf (stderr,
	"Generating a HBB using %d objects.\n", nprims);

  if (nprims==0) return;

  /* use first object to set initial root node bb */
  mav_SMSCallbackPointerResetExec (from_sms);
  mav_SMSCallbackObjectNextExec (from_sms, &this_obj);
  mav_callbackBBExec (mav_win_current, this_obj, &all_bb);

  /* calculate global bb */
  while (mav_SMSCallbackObjectNextExec (from_sms, &this_obj)) {
  /* get current class and bounding box */
    mav_callbackBBExec (mav_win_current, this_obj, &current_bb);
  /* compare with global bb */
    if (current_bb.min.x < all_bb.min.x) all_bb.min.x= current_bb.min.x;
    if (current_bb.min.y < all_bb.min.y) all_bb.min.y= current_bb.min.y;
    if (current_bb.min.z < all_bb.min.z) all_bb.min.z= current_bb.min.z;

    if (current_bb.max.x > all_bb.max.x) all_bb.max.x= current_bb.max.x;
    if (current_bb.max.y > all_bb.max.y) all_bb.max.y= current_bb.max.y;
    if (current_bb.max.z > all_bb.max.z) all_bb.max.z= current_bb.max.z;
  }

  /* compute depth of gridcells */
  num= (int) (ceil (pow (nprims, 0.333333333)));
  grid_depth= (int) (log (num) / log (2.0)) + 1;

  if (mav_opt_output==MAV_VERBOSE) fprintf (stderr,
	"Cluster gridcell depth= %d.\n", grid_depth - 1);

  /* construct all grids */
  cluster_grid= (MAV_HBBClusterGrid *) mav_malloc (grid_depth *
	sizeof (MAV_HBBClusterGrid));
  for (n= 0; n < grid_depth; n++) {
  /* calculate and set gridcell x,y,z size */
    num= (int) (pow (2.0, (double) n));
    cluster_grid[n].num_x= num;
    cluster_grid[n].num_y= num;
    cluster_grid[n].num_z= num;
  /* set voxel size (in world units) */
    cluster_grid[n].voxel_size.x= (all_bb.max.x - all_bb.min.x) / num;
    cluster_grid[n].voxel_size.y= (all_bb.max.y - all_bb.min.y) / num;
    cluster_grid[n].voxel_size.z= (all_bb.max.z - all_bb.min.z) / num;
  /* reset the list of voxels for this gridcell */
    cluster_grid[n].cells= (MAV_list **) mav_malloc (num*num*num *
		sizeof (MAV_list *));

    for (k= 0; k < num; k ++)
      for (j= 0 ; j < num; j ++)
        for (i= 0; i < num; i ++)
          cluster_grid[n].cells[k*num*num+j*num+i]= NULL;
  }

  /* insert objects into appropriate levels in hierarchy */
  /* each object inserted once only */
  if (mav_opt_output==MAV_VERBOSE) fprintf (stderr,
	"start: insert objects into HBB.\n");

  mav_SMSCallbackPointerResetExec (from_sms);
  while (mav_SMSCallbackObjectNextExec (from_sms, &obj)) {
    mav_callbackBBExec (mav_win_all, obj, &object_bound);
  /* determine level of best fit */
    mavlib_HBBCalculateBoundFit (&object_bound, 0, &best_fit, &all_bb);
    best_level= 0;
    for (n= 1; n < grid_depth; n ++) {
      mavlib_HBBCalculateBoundFit (&object_bound, n, &fit, &all_bb);
      if (fit < best_fit) {
        best_level= n;
        best_fit= fit;
      }
    }

  /* calculate voxel coords of centre of object */
    centre.x= 0.5 * (object_bound.min.x + object_bound.max.x) - all_bb.min.x;
    centre.y= 0.5 * (object_bound.min.y + object_bound.max.y) - all_bb.min.y;
    centre.z= 0.5 * (object_bound.min.z + object_bound.max.z) - all_bb.min.z;

    obj_cent.x= (int) (centre.x / cluster_grid[best_level].voxel_size.x);
    obj_cent.y= (int) (centre.y / cluster_grid[best_level].voxel_size.y);
    obj_cent.z= (int) (centre.z / cluster_grid[best_level].voxel_size.z);

  /* clip to edge of gridcell */
    if (obj_cent.x < 0) obj_cent.x= 0;
    if (obj_cent.y < 0) obj_cent.y= 0;
    if (obj_cent.z < 0) obj_cent.z= 0;

    if (obj_cent.x >= cluster_grid[best_level].num_x)
      obj_cent.x= cluster_grid[best_level].num_x - 1;
    if (obj_cent.y >= cluster_grid[best_level].num_y)
      obj_cent.y= cluster_grid[best_level].num_y - 1;
    if (obj_cent.z >= cluster_grid[best_level].num_z)
      obj_cent.z= cluster_grid[best_level].num_z - 1;

  /* add the object to the voxel */
    mavlib_HBBAddObjectToVoxelList (obj, &(cluster_grid[best_level]),
	obj_cent.x, obj_cent.y, obj_cent.z);
  }

  if (mav_opt_output==MAV_VERBOSE) fprintf (stderr,
	"gridcells completed.\nbuilding hierarchy ...\n");

  /* now traverse each grid inserting objects into SMS */
  for (n= 0; n< grid_depth; n++) {
    for (j= 0; j < cluster_grid[n].num_y; j++)
      for (k= 0; k < cluster_grid[n].num_z; k++)
        for (i= 0; i < cluster_grid[n].num_x; i++) {
      /* calculate array address of this cell */
          cell_addr= k * cluster_grid[n].num_x * cluster_grid[n].num_y +
		j * cluster_grid[n].num_x + i;
  /* if this cell contains any objects ... */
          if (cluster_grid[n].cells[cell_addr] != NULL) {
  /* go through this list */
            mav_listPointerReset (cluster_grid[n].cells[cell_addr]);
            while (mav_listItemNext (cluster_grid[n].cells[cell_addr],
			(void **) &this_obj)) {
  /* get current object */
	      mav_callbackBBExec (mav_win_all, this_obj, &object_bound);
	      mav_HBBObjectAdd (hierarchy, this_obj);
	    } /* while */
	  } /* if */
        } /* for (j k i) */
  } /* for (n) */

  /* free gridcells */
  for (n= grid_depth - 1; n >= 0; n --) {
    for (k= 0; k < cluster_grid[n].num_z; k ++)
      for (j= 0; j < cluster_grid[n].num_y; j ++)
        for (i= 0; i < cluster_grid[n].num_x; i ++) {
          cell_addr= k * cluster_grid[n].num_x * cluster_grid[n].num_y +
		j * cluster_grid[n].num_x + i;
  /* free lists of objects */
          if (cluster_grid[n].cells[cell_addr] != NULL)
	  mav_listEmpty (cluster_grid[n].cells[cell_addr]);
        }

  /* free voxel lists */
    mav_free (cluster_grid[n].cells);
  }

  /* free gridcells */
  mav_free (cluster_grid);

  if (mav_opt_output==MAV_VERBOSE)
	fprintf (stderr, "finished construction\n");
}


