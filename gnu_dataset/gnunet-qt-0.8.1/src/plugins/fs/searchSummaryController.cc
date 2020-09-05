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
 * @file src/plugins/fs/searchSummaryController.cc
 * @brief Controller for search summary
 * @author Nils Durner
 */

#include "searchSummaryController.h"

GFSSearchSummaryController::GFSSearchSummaryController(class GFSPlugin *fs)
{
  model = new GFSSearchSummaryModel(fs->config(), fs->errorContext());
  this->fs = fs;
  fs->searchSummaryView()->setModel(model);
}

GFSSearchSummaryController::~GFSSearchSummaryController()
{
  delete model;
}

void GFSSearchSummaryController::searchStarted(const struct GNUNET_FSUI_SearchList *handle,
  const struct GNUNET_ECRS_URI *uri)
{
  model->setSearch(handle, 0, uri);
  model->setStatus(handle, tr("active"), false);
}

void GFSSearchSummaryController::searchResult(const struct GNUNET_FSUI_SearchList *handle)
{
  model->incSearch(handle);
  model->setStatus(handle, tr("active"), false);
}

void GFSSearchSummaryController::searchStopped(const struct GNUNET_FSUI_SearchList *handle)
{
  model->removeSearch(handle);
}

void GFSSearchSummaryController::searchState(const struct GNUNET_FSUI_SearchList *handle, GNUNET_FSUI_EventType event)
{
  bool done;
  
  switch(event)
  {
    case GNUNET_FSUI_search_started:
    case GNUNET_FSUI_search_result:
    case GNUNET_FSUI_search_resumed:
      done = false;
      break;
    default:
      done = true;
  }
  
  model->setStatus(handle, fs->fsuiState(event), done);
}

/** end of searchSummaryController.cc **/
