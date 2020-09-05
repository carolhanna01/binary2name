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
 * @file src/plugins/fs/fshelper.h
 * @brief Helper functions for FS
 * @author Nils Durner
 */

#ifndef FSHELPER_H_
#define FSHELPER_H_

#include <QMultiHash>
#include <extractor.h>
#include "gnunet_qt_common.h"

#define SEARCH_URI_COLUMN ((int) EXTRACTOR_getHighestKeywordTypeNumber() + 1)
#define SEARCH_META_COLUMN ((int) EXTRACTOR_getHighestKeywordTypeNumber() + 2)
#define SEARCH_RANK_COLUMN ((int) EXTRACTOR_getHighestKeywordTypeNumber() + 3)
#define SEARCH_MAX_COLUMN SEARCH_RANK_COLUMN

/* Maps view index 0 ("Filename") to model index 1 (EXTRACTOR_FILENAME).
 * We can't use unified indexes because row decorations (branches, +/-)
 * are always painted to view colum 0. That would be EXTRACTOR_UNKNOWN,
 * which is usually hidden. Also map THUMBNAIL <=> FILESIZE because
 * the thumbnail should be the last and there's obviously a bug in
 * QHeaderView::moveColumn() and/or ::setHidden()
 */
#define MODEL_IDX(i) (((EXTRACTOR_KeywordType) i == EXTRACTOR_UNKNOWN) ? \
  (EXTRACTOR_KeywordType) 1 : (i == EXTRACTOR_FILENAME) ? \
  (EXTRACTOR_KeywordType) 0 : (i == EXTRACTOR_FILE_SIZE) ? \
  EXTRACTOR_THUMBNAIL_DATA : (i == EXTRACTOR_THUMBNAIL_DATA) ? \
  EXTRACTOR_FILE_SIZE : (EXTRACTOR_KeywordType) i)

typedef struct
{
  int availability_rank;
  unsigned int availability_certainty;
  unsigned int applicability_rank;
  long long rank_sort;
  unsigned int keywords;
} GRanking;

Q_DECLARE_METATYPE(GRanking)

/**
 * @brief Get the name of a given keyword/metadata type
 * @param type the keyword/metadata type
 * @returns the name
 */
QString metaTypeName(EXTRACTOR_KeywordType type);

typedef QMultiHash<EXTRACTOR_KeywordType, QByteArray> GFSMetaData;

#endif /*FSHELPER_H_*/

/* end of fshelper.h */
