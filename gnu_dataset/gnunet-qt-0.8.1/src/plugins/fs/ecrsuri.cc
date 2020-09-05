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

#include "ecrsuri.h"
#include "gnunet_qt_common.h"

#include <GNUnet/gnunet_namespace_lib.h>

GFSEcrsUri::GFSEcrsUri()
{
  ecrsUri = NULL;
}

GFSEcrsUri::GFSEcrsUri(QString serialized)
{
  ecrsUri = GNUNET_ECRS_string_to_uri(NULL, qPrintable(serialized));
}

GFSEcrsUri::GFSEcrsUri(const GFSEcrsUri &src)
{
  if (src.ecrsUri)
    ecrsUri = GNUNET_ECRS_uri_duplicate(src.ecrsUri);
  else
    ecrsUri = NULL;
}

GFSEcrsUri::~GFSEcrsUri()
{
  if (ecrsUri)
    GNUNET_ECRS_uri_destroy(ecrsUri);
}

const GNUNET_ECRS_URI *GFSEcrsUri::uri()
{
  return ecrsUri;
}

bool GFSEcrsUri::operator==(const GFSEcrsUri &rvalue)
{
  return ecrsUri && rvalue.ecrsUri && GNUNET_ECRS_uri_test_equal(ecrsUri, rvalue.ecrsUri);
}

GFSEcrsUri &GFSEcrsUri::operator=(const GFSEcrsUri &src)
{
  if (ecrsUri)
    GNUNET_ECRS_uri_destroy(ecrsUri);

  if (src.ecrsUri)
    ecrsUri = GNUNET_ECRS_uri_duplicate(src.ecrsUri);
  else
    ecrsUri = NULL;

  return *this;
}

bool GFSEcrsUri::operator<(const class GFSEcrsUri &rvalue) const
{
  char *s1, *s2;
  bool ret;

  s1 = GNUNET_ECRS_uri_to_string(ecrsUri);
  s2 = GNUNET_ECRS_uri_to_string(rvalue.ecrsUri);

  ret = (strcmp(s1, s2) < 0);

  GNUNET_free(s1);
  GNUNET_free(s2);

  return ret;
}

QString GFSEcrsUri::toString()
{
  char *dhead, *desc;
  QString strRet;

  if (!ecrsUri)
    return QString();

  desc = GNUNET_ECRS_uri_to_string(ecrsUri);
  if (!desc)
    return QString();

  GNUNETQT_ASSERT(strlen(desc) >= strlen(GNUNET_ECRS_URI_PREFIX));

  dhead = desc + strlen(GNUNET_ECRS_URI_PREFIX);
  if (strncmp(dhead, GNUNET_ECRS_SEARCH_INFIX, strlen(GNUNET_ECRS_SEARCH_INFIX)) == 0)
    strRet = dhead + strlen(GNUNET_ECRS_SEARCH_INFIX);
  else if (strncmp(dhead, GNUNET_ECRS_SUBSPACE_INFIX, strlen(GNUNET_ECRS_SUBSPACE_INFIX)) == 0)
    strRet = dhead + strlen(GNUNET_ECRS_SUBSPACE_INFIX);
  else if (strncmp(dhead, GNUNET_ECRS_FILE_INFIX, strlen(GNUNET_ECRS_FILE_INFIX)) == 0)
    strRet = dhead + strlen(GNUNET_ECRS_FILE_INFIX);

  GNUNET_free(desc);

  return strRet;
}

QString GFSEcrsUri::toDisplayString(struct GNUNET_GC_Configuration *cfg,
    struct GNUNET_GE_Context *ectx)
{
  if (GNUNET_ECRS_uri_test_ksk(ecrsUri))
    return GNUNET_ECRS_ksk_uri_to_human_readable_string(ecrsUri);
  else
    return GNUNET_NS_sks_uri_to_human_readable_string(ectx, cfg, ecrsUri);

  GNUNET_GE_BREAK (ectx, 0);

  return "";
}

QString GFSEcrsUri::serialized()
{
  char *str;
  QString ret;

  if (!ecrsUri)
    return QString();

  str = GNUNET_ECRS_uri_to_string(ecrsUri);

  if (str)
  {
    ret = QString::fromLocal8Bit(str);
    GNUNET_free(str);
  }

  return ret;
}

GFSEcrsUri::GFSEcrsUri(const GNUNET_ECRS_URI *uri)
{
  if (uri)
    ecrsUri = GNUNET_ECRS_uri_duplicate(uri);
  else
    ecrsUri = NULL;
}

GFSEcrsUri &GFSEcrsUri::operator=(const GNUNET_ECRS_URI *uri)
{
  if (ecrsUri)
    GNUNET_ECRS_uri_destroy(ecrsUri);

  if (uri)
    ecrsUri = GNUNET_ECRS_uri_duplicate(uri);
  else
    ecrsUri = NULL;

  return *this;
}

/* end of ecrsuri.cc */
