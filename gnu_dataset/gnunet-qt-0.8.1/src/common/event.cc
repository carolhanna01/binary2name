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
 * @file src/common/event.cc
 * @brief Event to communicate with objects in other threads
 * @author Nils Durner
 */

#include "event.h"

GEvent::GEvent(QEvent::Type type, void *param, void **ret, QSemaphore *sem) : QEvent(type)
{
  this->param = param;
  this->ret = ret;
  this->sem = sem;
  ownerThread = QThread::currentThread();
}

GEvent::~GEvent()
{
}

void GEvent::setReturn(void *ret)
{
  if (this->ret)
    *(this->ret) = ret;
  
  if (sem)
  {
    // unblock waiting thread
    sem->release(1);
  }
}

void *GEvent::getParam()
{
  return param;
}

/* end of event.cc */
