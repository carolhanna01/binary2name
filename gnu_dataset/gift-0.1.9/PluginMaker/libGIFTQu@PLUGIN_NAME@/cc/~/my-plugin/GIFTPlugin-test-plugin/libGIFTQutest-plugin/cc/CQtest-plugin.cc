// -*- mode: c++ -*- 
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
#include "libMRML/include/mrml_const.h" // for parsing
#include "libGIFTQutest-plugin/include/CQtest-plugin.h"    // the header for this class
#include "libGIFTActest-plugin/include/CActest-plugin.h"   // the header for its accessor
#include "libMRML/include/CXMLElementBuilder.h" //building XML elements from a string

// for pipes
#include <errno.h>  // error number (man errno)
#include <string.h> // error string
#include <cstdio>   // c standard i/o (enspricht stdio.h)
#include <assert.h> // 

/** 

    There is no special initialisation to do,
    so this function is empty

 */

void CQtest-plugin::init(){
};

/**
 *
 * default constructor SHOULD NOT BE CALLED
 * 
 */
CQtest-plugin::CQtest-plugin(){
  assert(0);
};
/**
 * constructor
 * see CQuery
 */
CQtest-plugin::CQtest-plugin(CAccessorAdminCollection& inAccessorAdminCollection,
			 CAlgorithm&          inAlgorithm):
  CQuery(inAccessorAdminCollection,
	 inAlgorithm){
  {

    // mproxy has been filled in a reasonable way 
    // by CQuery::CQuery
    mAccessor=mAccessorAdmin->openAccessor("test-plugin");

    init();

    assert(mAccessor);
  }
};
    
/**
 *
 * destructor: at present empty
 * 
 */
CQtest-plugin::~CQtest-plugin(){

  cout << "destroying this "
       << __FILE__
       << __LINE__
       << flush
       << endl;

  //i thought i will need this, but at present I do not have this impression
  //it does not hurt, so we leave it in
};

/**  */
bool CQtest-plugin::setAlgorithm(CAlgorithm & inAlgorithm){
  if(mAlgorithm && mAlgorithm->getCollectionID()==inAlgorithm.getCollectionID()){
    
    return true;
    
  }else{
    //close the old collection, if exsisting
    if(mAccessorAdmin)
      mAccessorAdmin->closeAccessor("test-plugin");
    //
    mAccessorAdmin=&mAccessorAdminCollection->getProxy(inAlgorithm.getCollectionID());
    mAccessor=mAccessorAdmin->openAccessor("test-plugin");
    
    assert(mAccessor);
    //
    return (CQuery::setAlgorithm(inAlgorithm) && mAccessor);
  }
};

/**
 *
 *
 */
CIDRelevanceLevelPairList* CQtest-plugin::fastQuery(const CXMLElement& ,
					      int ,
					      double){
  
  assert(!"test-plugin: METHOD fastQuery not implemented");
}

/**
 *
 *
 */
CXMLElement* CQtest-plugin::query(const CXMLElement& inQuery){

  CXMLElement* lReturnValue=new CXMLElement(mrml_const::query_result);
  return  lReturnValue;
};

