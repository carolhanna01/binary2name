//
// Part of the ht://Dig package   <http://www.htdig.org/>
// Copyright (c) 1999, 2000, 2001 The ht://Dig Group
// For copyright details, see the file COPYING in your distribution
// or the GNU General Public License version 2 or later
// <http://www.gnu.org/copyleft/gpl.html>
//
// $Id: WordMatch.cc,v 1.2 2001/06/29 14:14:08 loic Exp $
//
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#include <WordMatch.h>

String WordMatch::Get() const
{
  String tmp;
  match.Get(tmp);
  if(!info.empty())
    tmp << "(" << info << ")";
  return tmp;
}
