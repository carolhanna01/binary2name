/*
     This file is part of gnunet-qt.
     (C) 2006, 2007 Nils Durner (and other contributing authors)

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
 * @file src/plugins/fs/ecrsMetaData.h
 * @brief Encapsulates GNUNET_MetaData
 * @author Nils Durner
 */

#ifndef ECRSMETADATA_H_
#define ECRSMETADATA_H_

#include <QByteArray>
#include <GNUnet/gnunet_ecrs_lib.h>

class GFSEcrsMetaData
{
public:
	GFSEcrsMetaData();
  GFSEcrsMetaData(QByteArray serialized);
  GFSEcrsMetaData(const class GFSEcrsMetaData &src);
  GFSEcrsMetaData(const struct GNUNET_MetaData *src);
  virtual ~GFSEcrsMetaData();
  
  GFSEcrsMetaData &operator=(const class GFSEcrsMetaData &src);
  GFSEcrsMetaData &operator=(const struct GNUNET_MetaData *src);
  
  QByteArray serialized();
  
  struct GNUNET_MetaData *meta();
  
protected:
  struct GNUNET_MetaData *metaData;

  void copyFrom(const class GFSEcrsMetaData &src);
  void copyFrom(const struct GNUNET_MetaData *src);
};

#endif /*ECRSMETADATA_H_*/
