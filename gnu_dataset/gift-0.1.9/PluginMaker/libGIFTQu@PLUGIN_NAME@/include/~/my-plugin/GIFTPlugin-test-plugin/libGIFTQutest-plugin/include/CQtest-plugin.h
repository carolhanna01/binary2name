/* -*- mode: c++ -*- 
*/
/* 

    GIFT, a flexible content based image retrieval system.
    Copyright (C) 1998, 1999, 2000, 2001, 2002, CUI University of Geneva

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*/
// -*- mode: c++ -*-
#ifndef _CQtest-plugin
#define _CQtest-plugin


/* -*- mode: c++ -*- 
*/
/**
 *
 * CQtest-plugin.h
 * An empty CQuery subclass
 *
 *
 * modification history:
 *
 * WM 20000205 creation
 *
 *
 *
 * compiler defines used:
 * _CQtest-plugin.h avoids double inclusion
 *
 */
#include <memory>
#include <map>
#include <list>
#include "libMRML/include/CQuery.h"
#include "libMRML/include/CSelfDestroyPointer.h"
#include "libMRML/include/CIDRelevanceLevelPairList.h"
#include "libMRML/include/CRelevanceLevelList.h"

//#include "CWeightingFunctionPointerList.h"
//#include "CWeightingFunctionPointerHash.h"

#include "libMRML/include/CAlgorithm.h"
#include "libMRML/include/CAccessor.h"
#include "libMRML/include/CAccessorAdminCollection.h"
#include "libMRML/include/CAccessorAdmin.h"


class CScoreBoard;
class CAccessor;


/** 

    This is a simple test-plugin based browser.
    

    @Author: Wolfgang Müller

 */
class CQtest-plugin:public CQuery{
protected:
  /** sets mCurrentPosition to 0*/
  void init();

 public:
  /**
   *
   * default constructor
   * 
   */
  CQtest-plugin();
  /**
   *
   * we need to unregister the accessors used
   * 
   */
  ~CQtest-plugin();
  /**
   */
  CQtest-plugin(CAccessorAdminCollection& inAccessorAdminCollection,
		  CAlgorithm&          inAlgorithm);
    
  /**
   * Processing a query
   *
   *@param inQuery an CXMLElement containing a query-step element
   */
  virtual CXMLElement* query(const CXMLElement& inQuery);

  /** This function is useless in the context of this plugin.
      Please see CQuery and its documentation for more instructions 

      The reason why this function is here, is to bail out on misconfiguration,
      and to keep the compiler happy.

      @see CQuery
  */
  virtual CIDRelevanceLevelPairList* fastQuery(const CXMLElement& ,
					       int ,
					       double);


  /**
   *
   * set the Algorithm. DEPRECATED
   *
   */
  virtual bool setAlgorithm(CAlgorithm& inAlgorithm);

}; /* end of class */

#endif

