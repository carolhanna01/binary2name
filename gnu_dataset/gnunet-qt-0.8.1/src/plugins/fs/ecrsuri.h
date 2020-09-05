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
 * @file src/plugins/fs/ecrsuri.h
 * @brief Wrapper for GNUNET_ECRS_URI
 * @author Nils Durner
 */

#ifndef ECRSURI_H_
#define ECRSURI_H_

#include "gnunet_qt_common.h"
#include <GNUnet/gnunet_ecrs_lib.h>

class GFSEcrsUri
{
public:
  GFSEcrsUri();
  GFSEcrsUri(QString serialized);
  GFSEcrsUri(const GNUNET_ECRS_URI *uri);
  GFSEcrsUri(const GFSEcrsUri &src);
  ~GFSEcrsUri();

  const GNUNET_ECRS_URI *uri();
  QString toString();
  QString toDisplayString(struct GNUNET_GC_Configuration *cfg,
      struct GNUNET_GE_Context *ectx);
  QString serialized();

  bool operator==(const GFSEcrsUri &rvalue);
  GFSEcrsUri &operator=(const GFSEcrsUri &src);
  GFSEcrsUri &operator=(const GNUNET_ECRS_URI *src);
  bool operator<(const class GFSEcrsUri &rvalue) const;
protected:
  GNUNET_ECRS_URI *ecrsUri;
};

Q_DECLARE_METATYPE(GFSEcrsUri)

#endif /*ECRSURI_H_*/

/* end of ecrsuri.h */
