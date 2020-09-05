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
 * @file src/common/eventDispatcher.h
 * @brief Non-deadlocking dispatcher for event
 * @author Nils Durner
 */

#ifndef EVENTDISPATCHER_H_
#define EVENTDISPATCHER_H_

class GEventDispatcher
{
public:
  static bool sendEvent(QObject *receiver, QEvent *event);
  static void postEvent(QObject *receiver, QEvent *event);
};

#endif /*EVENTDISPATCHER_H_*/
