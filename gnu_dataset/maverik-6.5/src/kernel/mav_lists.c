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


#include "mavlib_kernel.h"
#include <stdlib.h>
#include <stdio.h>



/* Routine to create a new list */

MAV_list *mav_listNew(void) 
{
  MAV_list *rv= (MAV_list *) mav_malloc(sizeof(MAV_list));
  
  rv->length= 0;
  rv->head= NULL;
  rv->tail= NULL;
  rv->current= (MAV_listPointer *) mav_malloc(sizeof(MAV_listPointer));
  rv->current->item= NULL;
  rv->current->next= NULL;

  return rv;
}



/* Routine to add a (void) item to a list */

void mav_listItemAdd(MAV_list *l, void *d) 
{
  mav_listItemsAdd(l, d, NULL);
}

void mav_listItemsAdd(MAV_list *l, void *d1, void *d2) 
{
  MAV_listItem *ls= (MAV_listItem *) mav_malloc(sizeof(MAV_listItem));

  /* Fill in the data */
  ls->data1= d1;
  ls->data2= d2;

  /* This item goes at the end of the list */
  ls->next= NULL;
  ls->prev= l->tail;

  /* Update head and tail of list */
  if (!l->head) {
    l->head= ls;
    l->current->item= l->head;
  }
  if (l->tail) l->tail->next= ls;
  l->tail= ls;

  /* Inc length */
  l->length++;
}



/* Routine to remove a (void) item from a list */

void mav_listItemRmv(MAV_list *l, void *d) 
{
  mav_listItemsRmv(l, d, NULL);
}

void mav_listItemsRmv(MAV_list *l, void *d1, void *d2) 
{
  MAV_listItem *li= l->head;
  MAV_listPointer *lp= l->current;

  /* Step through linked list looking for match */
  while (li) {
    MAV_listItem *n= li->next;

    if (li->data1==d1 && li->data2==d2) {

      /* Relink list */
      if (li->prev) li->prev->next= li->next;
      if (li->next) li->next->prev= li->prev;
      
      /* Update the head or tail of list is this is the item being deleted */
      if (li==l->head) l->head= li->next;
      if (li==l->tail) l->tail= li->prev;

      /* Check for references to this item in the list pointer stack */
      while (lp) {
	if (lp->item==li) lp->item= li->next;
	lp= lp->next;
      }

      /* Remove item */
      mav_free(li);
      l->length--;

      /* Allow mutiple same entries (maybe useful) */
      return;
    }

    li= n;
  }
}



/* Routine to fetch the next item in a list */

int mav_listItemNext(MAV_list *l, void **d) 
{
  void *ig;
  return mav_listItemsNext(l, d, &ig);
}

int mav_listItemsNext(MAV_list *l, void **d1, void **d2) 
{
  int rv;

  if (l->current->item)
  {
    /* Get the data */
    *d1= l->current->item->data1;
    *d2= l->current->item->data2;

    /* Move to next item */
    l->current->item= l->current->item->next;

    rv=1;
  }
  else
  {
    rv=0;
  }

  return rv;
}



/* Routine to return size of a list */

int mav_listSize(MAV_list *l) 
{
  return l->length;
}



/* Routine to dump the contents of a list */

void mav_listPrint(char *s, MAV_list *l) 
{
  MAV_listItem *li= l->head;
  MAV_listPointer *lp= l->current;

  printf("%s", s);
  printf("list contains %i items:\n ", l->length);

  while (li) {
    printf("%p ", li);
    li= li->next;
  }
  printf("\nlist pointer stack:\n ");
  
  while (lp) {
    printf("%p ", lp->item);
    lp= lp->next;
  }

  printf("\n");
}



/* Routine to empty a list */

void mav_listEmpty(MAV_list *l)
{
  MAV_listItem *li= l->head;
  MAV_listPointer *lp= l->current;

  /* Step through linked list deleting entries */
  while (li) {
    MAV_listItem *n= li->next;
    mav_free(li);
    li= n;
  }

  /* Step through list pointers deleting entries */
  while (lp) {
    MAV_listPointer *n= lp->next;
    mav_free(lp);
    lp= n;
  }

  /* Reset pointers */
  l->length= 0;
  l->head= NULL;
  l->tail= NULL;
  l->current= (MAV_listPointer *) mav_malloc(sizeof(MAV_listPointer));
  l->current->item= NULL;
  l->current->next= NULL;
}



/* Routine to delete a list */

void mav_listDelete(MAV_list *l)
{
  /* Empty the list */
  mav_listEmpty(l);

  /* Free up list pointer */
  mav_free(l->current);

  /* Free the list itself */
  mav_free(l);
}



/* Routine to return if a list contains an item */

int mav_listItemContains(MAV_list *l, void *d) 
{
  return mav_listItemsContains(l, d, NULL);
}

int mav_listItemsContains(MAV_list *l, void *d1, void *d2) 
{
  MAV_listItem *li= l->head;

  /* Step through linked list looking for match */
  while (li) {
    if (li->data1==d1 && li->data2==d2) return 1;
    li= li->next;
  }

  return 0;
}



/* Routine to reset a list pointer */

void mav_listPointerReset(MAV_list *l) 
{
  l->current->item= l->head;
}



/* Routine to push a list pointer */

void mav_listPointerPush(MAV_list *l)
{
  MAV_listPointer *lp= (MAV_listPointer *) mav_malloc(sizeof(MAV_listPointer));

  /* Keep same position */
  lp->item= l->current->item;
  
  /* Linked list of list pointers */
  lp->next= l->current;
  l->current= lp;
}



/* Routine to pop a list pointer */

void mav_listPointerPop(MAV_list *l)
{
  if (l->current->next)
  {
    MAV_listPointer *n= l->current->next;
    mav_free(l->current);
    l->current= n;
  }
  else
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: list pointer stack empty\n");   
  }
}
