// Copyright (C) 2014 Arthur I. Schwarz
//
// This file is part of the C++ SLIP library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/* 
 * File:   SlipDescriptor.h
 * Author: A. Schwarz
 *
 * Created on April 8, 2014, 11:49 AM
 */

#ifndef SLIPDESCRIPTION_H
#define	SLIPDESCRIPTION_H
        
# include <string>
# include "SlipCell.h"

namespace slip {
   /**
    * @brief Supports list and data maintenance.
    * <p>Container for data used to pass values within the parser and between
    *    the parser and other classes. The container provides required data
    *    to enable construction of lists, in the parser, and to support
    *    Descriptor List forward references and resolution.</p>
    * <p>There are three types of information passed:</p>
    * <ol>
    *    <li><b>ANONYMOUS</b> lists: Lists created curing processing
    *       which have not been given a name (yet) or which will be
    *       inserted into a list are anonymous. Anonymous lists must
    *       be deleted during syntax error recovery and after insertion
    *       into a list as a sublist.</li>
    *       <p>For error recovery it is assumed if the list is not yet
    *          named then without deletion there is a chance that the
    *          list space will be irrecoverable.</p>
    *       <p>After insertion as a sublist if an anonymous list is not
    *          deleted then there will be two 'owners', the containing
    *          list and the parser. If the list is not deleted, leaving
    *          the parser as on owner, then when the parser is exited
    *          the list becomes irrecoverable on deletion.</p>
    *    <li><b>NAMED</b> lists: Named lists are not deleted during parsing.
    *       They can only be deleted during post-processing. During 
    *       processing, the registry 'owns' the list. During post-processing
    *       the registry removes this ownership by deletion.</li>
    *    <li><b>DATA</b>: Data is always deleted during syntax error
    *       recovery. Data is sometimes deleted during semantic error
    *       recovery. Data is not deleted after it is inserted into a
    *       list. At any given time, unlike lists, there can be onbly
    *       one data 'owner'. When the data is inserted into a list, the
    *       list becomes the owner. Before insertion, the parser owns
    *       the data and is responsible for graceful deletion.</li>
    * </ol>
    * 
    * @sa SlipHashEntry
    */
   class SlipDescription {                     // Support %destructor for decriptionList
   private:
   const static string sType[];
   public:

   /**
    * @brief Enumerate different SlipCells passed in a Description Object.
    * <p><b>Overview</b></p><hr>
    * <p>A description object serves to identify the cell types in order
    *    to allow proper cell deletion during normal and error processing.</p>
    * <p><b>ENUM</b><hr>
    * <p>The cell types and their processing characteristics are:</p>
    * <ul>
    *    <li><b>ANONYMOUS</b>: An anonymous list. An anonymous list can be
    *        deleted during error processing without affecting recovery or
    *        operations. An anonymous list must be deleted during normal
    *        processing when it is linked into a list.</li>
    *    <li><b>NAMED</b>: A named list expanded during processing. A
    *        named list can only be deleted during post-processing following
    *        parsing. A named list can not be deleted during error recovery
    *        or following insertion into another list.</li>
    *    <li><b>DATA</b>: A data object (<i>SlipDatum</i> object). A data
    *        object can be deleted during error recovery without affecting
    *        processing. A data object can not be deleted after being 
    *        inserted into a list.</li>
    * </ul>
    * <p><b>DATA</b><hr>
    * <ul>
    *    <li>ptr Pointer to a list header (<i>SlipHeader</i>) or datum 
    *        (<i>SlipDatum</i>) object. If the data type is <b>ANONYMOUS</b>
    *        or <b>NAMED</b>, then the pointer is to a list. If the data type
    *        is <b>DATA</b> then the pointer is to data.</li>
    *    <li>nestedPtr Pointer to a Descriptor List object. Points to a
    *        list nested Descriptor List. If the list does not have a 
    *        Descriptor List or if the <b>ptr</b> field points to data, then
    *        the field is <b>NULL</b>.</li>
    *    <li>name Name of a <i>NamedList</i> or a the name of the User Data
    *        class represented by a <i>SlipDatum</i> object.</li>
    *    <li>type Type of <i>SlipDescriptor</i>, <b>ANONYMOUS</b>, <b>NAMED</b>,
    *        or <b>DATA</b>.</li>
    * </ul>
    * <p><b>Constructors/Destructors</b></p><hr>
    * <ul>
    *    <li>SlipDescription Creates the following <i>SlipDescriptor</i> 
    *        object:</li>
    *    <ul>
    *       <li>unnamed</li>
    *       <li>named</li>
    *       <li>duplicate object</li>
    *    </ul>
    *    <li>~SlipDescription Deleted a nested Descriptor List (nestPtr) if it
    *         exists and the name. Does not delete the current list(if one
    *         exists) of a <i>SlipDatum</i> object.</li>
    * </ul>
    * <p><b>Methods</b></p><hr>
    * <ul>
    *    <li>deleteAll Deletes all the data (list, datum, nested Descriptor
    *        list, and name) and returns the <i>SlipDescriptor</i> object to
    *        the heap.</li>
    *    <li>deleteData Deletes the <i>SlipDatum</i> object, an <b>ANONYMOUS</b>
    *        list, nested Descriptor List, the name and returns the 
    *        <i>SlipDescriptor</i> object to the heap.</li>
    *    <li>deleteList Deletes an <b>ANONYMOUS</b> list, a nested Descriptor 
    *        List, the name and returns the <i>SlipoDescriptor</i> object to 
    *        the heap.</li>
    *    <li>toString Pretty-prints the fields and outputs the "toString" 
    *        values of the data pointed to by the <b>ptr</b> field (list or
    *        datum).</li>
    * </ul>
    */
   enum DataTypes { ANONYMOUS                  //!< anonymous list
                  , NAMED                      //!< named list
                  , DATA                       //!< SlipDatum
                  };
   private:
      SlipCell*        ptr;                    //!< pointer to a list
      string*          name;                   //!< name of list or user definition
      SlipDescription* nestedPtr;              //!< name of nested Description List
      DataTypes        type;                   //!< ANONYMOUS, EXPAND, DATA
   public:
      const bool       leakDebugFlag;          //!< memory leak debug
      SlipDescription(SlipCell* ptr, DataTypes type = ANONYMOUS, int debugFlag= 0);
      SlipDescription(SlipCell* ptr, string* name, DataTypes type = ANONYMOUS, int debugFlag = 0);
      SlipDescription(SlipDescription* desc, int debugFlag = 0);
      ~SlipDescription();
      SlipDescription& operator=(SlipCell*    X)     { ptr  = X; return *this;       }
      SlipDescription& operator=(string*      X)     { name = X; return *this;       }
      SlipDescription& operator=(SlipDescription* X) { nestedPtr = X; return *this;  }
      SlipDescription& operator=(DataTypes        X) { type      = X; return *this;  }
      SlipCell*        getPtr()  { return ptr;       }
      string*          getName() { return name;      }
      SlipDescription* getDesc() { return nestedPtr; }
      DataTypes        getType() { return type;      }
      SlipDescription* putDesc(SlipDescription* desc) { nestedPtr = desc; return this; }
      void             resetPtr(){ ptr = NULL;       }
      void deleteAll();
      void deleteData();
      void deleteList();
      string toString();
      
   }; // class SlipDescription
   
}; // namespace slip

#endif	/* SLIPDESCRIPTION_H */

