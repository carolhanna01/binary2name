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
 * @file src/plugins/fs/ecrsMetaData.cc
 * @brief Encapsulates GNUNET_MetaData
 * @author Nils Durner
 */

#include "gnunet_qt_common.h"
#include "ecrsMetaData.h"

GFSEcrsMetaData::GFSEcrsMetaData()
{
  metaData = NULL;
}

GFSEcrsMetaData::GFSEcrsMetaData(QByteArray serialized)
{
  metaData = GNUNET_meta_data_deserialize(NULL, serialized.data(), serialized.size());
}

GFSEcrsMetaData::~GFSEcrsMetaData()
{
  if (metaData)
    GNUNET_meta_data_destroy(metaData);
}

GFSEcrsMetaData::GFSEcrsMetaData(const GFSEcrsMetaData &src)
{
  copyFrom(src);
}

GFSEcrsMetaData::GFSEcrsMetaData(const struct GNUNET_MetaData *src)
{
  copyFrom(src);
}

GFSEcrsMetaData &GFSEcrsMetaData::operator=(const class GFSEcrsMetaData &src)
{
  copyFrom(src);
  return *this;
}

GFSEcrsMetaData &GFSEcrsMetaData::operator=(const struct GNUNET_MetaData *src)
{
  copyFrom(src);
  return *this;
}
  
void GFSEcrsMetaData::copyFrom(const class GFSEcrsMetaData &src)
{
  metaData = GNUNET_meta_data_duplicate(src.metaData);
}

void GFSEcrsMetaData::copyFrom(const struct GNUNET_MetaData *src)
{
  metaData = GNUNET_meta_data_duplicate(src);
}

struct GNUNET_MetaData *GFSEcrsMetaData::meta()
{
  return metaData;
}

QByteArray GFSEcrsMetaData::serialized()
{
  int size;
  
  size = GNUNET_meta_data_get_serialized_size(metaData, GNUNET_SERIALIZE_FULL);
  if (size != GNUNET_SYSERR)
  {
    QByteArray ret;
    char *data;

    data = new char[size];
    GNUNETQT_ASSERT(GNUNET_meta_data_serialize(NULL, metaData, data, size,
      GNUNET_SERIALIZE_FULL) != GNUNET_SYSERR);
    
    ret = QByteArray(data, size);
    delete [] data;
    
    return ret;
  }
  else
    return QByteArray();
}

/* end of ecrsMetaData.cc */
