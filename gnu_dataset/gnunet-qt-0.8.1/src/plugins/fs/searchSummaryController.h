/*
     This file is part of gnunet-qt.
     (C) 2007 Nils Durner (and other contributing authors)

     gnunet-qt is free software; you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published
     by the Free Software Foundation; either version 2, or (at your
     option) any later version.

     gnunet-qt is distributed in the hope that it will be useful, but
     WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
     General Public License for more details.

     You should have received a copy of the GNU General Public License
     along with gnunet-qt; see the file COPYING.  If not, write to the
     Free Software Foundation, Inc., 59 Temple Place - Suite 330,
     Boston, MA 02111-1307, USA.
*/

/**
 * @file src/plugins/fs/searchSummaryController.h
 * @brief Controller for search summary
 * @author Nils Durner
 */

#ifndef SEARCHSUMMARYCONTROLLER_H_
#define SEARCHSUMMARYCONTROLLER_H_

#include <QObject>

#include "fs.h"
#include "searchSummaryModel.h"

class GFSSearchSummaryController : public QObject
{
  Q_OBJECT
  
public:
	GFSSearchSummaryController(class GFSPlugin *fs);
	virtual ~GFSSearchSummaryController();
  
  void searchStarted(const struct GNUNET_FSUI_SearchList *handle, const struct GNUNET_ECRS_URI *uri);
  void searchResult(const struct GNUNET_FSUI_SearchList *handle);
  void searchStopped(const struct GNUNET_FSUI_SearchList *handle);
  void searchState(const struct GNUNET_FSUI_SearchList *handle, GNUNET_FSUI_EventType event);
  
protected:
  GFSPlugin *fs;
  class GFSSearchSummaryModel *model;
};

#endif /*SEARCHSUMMARYCONTROLLER_H_*/
