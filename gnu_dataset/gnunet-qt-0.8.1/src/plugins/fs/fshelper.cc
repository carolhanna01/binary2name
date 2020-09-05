/*
     This file is part of gnunet-qt.
     (C) 2006 Nils Durner (and other contributing authors)

     gnunet-qt is free software; you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published
     by the Free Software Foundation; either version 2, or (at your
     option) any later version.

     gnunet-qt is distributed in the hope that it will be useful, but
     WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
     General Public License for more details.

     You should have received a copy of the GNU General Public License
     along with GNUnet; see the file COPYING.  If not, write to the
     Free Software Foundation, Inc., 59 Temple Place - Suite 330,
     Boston, MA 02111-1307, USA.
*/

/**
 * @file src/plugins/fs/fshelper.cc
 * @brief Helper functions for FS
 * @author Nils Durner
 */

#include "fshelper.h"

/**
 * @brief Get the name of a given keyword/metadata type
 * @param type the keyword/metadata type
 * @returns the name
 */
QString metaTypeName(EXTRACTOR_KeywordType type)
{
    if (type == EXTRACTOR_THUMBNAIL_DATA)
      return QObject::tr("Preview");
    else if (type == EXTRACTOR_UNKNOWN)
      return QObject::tr("Unclassified");
    else if (type == EXTRACTOR_getHighestKeywordTypeNumber() + 1)
      return QObject::tr("Metadata");
    else
    {
      GString strCol;
      
      strCol = EXTRACTOR_getKeywordTypeAsString((EXTRACTOR_KeywordType) type);
      strCol.proper();
      
      return strCol;
    }  
}

/* end of fshelper.cc */
