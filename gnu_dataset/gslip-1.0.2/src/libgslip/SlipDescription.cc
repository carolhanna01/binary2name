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
 * File:   SlipDescription.cpp
 * Author: skidmarks
 *
 * Created on April 8, 2014, 11:49 AM
 */

# include <iomanip>
# include <iostream>
# include "SlipDatum.h"
# include "SlipDescription.h"
# include "SlipHeader.h"
# include "SlipRead.h"

#ifdef DEBUG_IO
   #undef DEBUG
   #define DEBUG(flag, stmt) if (flag) { stmt }
#else
   #define DEBUG(flag, stmt)
#endif

namespace slip {

const string SlipDescription::sType[] = { "ANONYMOUS", " NAMED", "DATA" };

   /**
    * @brief SlipDescription Constructor.
    * <p>Create an object with no name and no Descriptor List. </p>
    * @param[in] ptr (SlipCell*) pointer to a <i>SlipHeader</i>
    * @param[in] type (DataTypes) type of descriptor created
    * @param[in] debugFlag (int) debug flags
    */
   SlipDescription::SlipDescription(SlipCell* ptr, DataTypes type, int debugFlag)
               : ptr(ptr)
               , type(type)
               , name(NULL)
               , leakDebugFlag(debugFlag & SlipRead::LEAK)
               , nestedPtr(NULL) {
      DEBUG(leakDebugFlag,
      cout << left << setw(27) << setfill(' ') << "SlipDescription::create " << hex << setw(sizeof(void*)) 
           << setfill('0') << (void*)this << ' ' << ptr->toString() << endl;)
   }; // SlipDescription::SlipDescription(SlipCell* ptr, DataTypes type)

   /**
    * @brief SlipDescription Constructor.
    * <p>Create an object with a name but without a nested Descriptor List.</p>
    * @param[in] ptr (SlipCell*) pointer to a <i>SlipHeader</i>
    * @param[in] name (string*) named list name
    * @param[in] type (DataTypes) type of descriptor created
    * @param[in] debugFlag (int) debug flags
    */
   SlipDescription::SlipDescription(SlipCell* ptr, string* name, DataTypes type, int debugFlag)
               : ptr(ptr)
               , leakDebugFlag(debugFlag & SlipRead::LEAK)
               , type(type)
               , name(new string(*name))
               , nestedPtr(NULL) {
      DEBUG(leakDebugFlag,
      cout << left << setw(27) << setfill(' ') << "SlipDescription::create " << hex << setw(sizeof(void*)) 
           << setfill('0') << (void*)this << ' ' << *name << endl;)
   }; // SlipDescription::SlipDescription(SlipCell* ptr, string* name, DataTypes type, int debugFlag)

   /**
    * @brief Construct a copy of a SlipDescriptor object.
    * @param desc (SlipDescription*) construction template
    * @param[in] debugFlag (int) debug flags
    */
   SlipDescription::SlipDescription(SlipDescription* desc, int debugFlag)
               : ptr(desc->ptr)
               , leakDebugFlag(debugFlag & SlipRead::LEAK)
               , type(desc->type)
               , name(new string(*(desc->name)))
               , nestedPtr(NULL) {
      DEBUG(leakDebugFlag,
      cout << left << setw(27) << setfill(' ') << "SlipDescription::create " << hex << setw(sizeof(void*)) 
           << setfill('0') << (void*)this << ' ' << *(desc->name) << endl;)
   }; // SlipDescription::SlipDescription(SlipDescription* desc, int debugFlag)

   /**
    * @brief Destructor deletes the name and Descriptor List.
    * <p>The object name is deleted. The object pointer, to either
    *    the User Data input parser of to a list (SlipHeader object)
    *    are not deleted. Deletion of references are selective and
    *    done in one of the delete... methods.</p>
    */
   SlipDescription::~SlipDescription() {
      static string null = string("NONAME");
      DEBUG(leakDebugFlag, {
      cout << left << setw(27) << setfill(' ') << "SlipDescription::delete " 
           << setfill('0') << (void*)this << ' ' << this->toString() << endl;})
      if (nestedPtr) delete nestedPtr;
      if ((type != ANONYMOUS) && (this->name != NULL)) {
         delete this->name;
         this->name = NULL;
      }
      nestedPtr = NULL;
   }; // SlipDescription::~SlipDescription()

   /**
    * @brief Delete the current object and all reference pointers and te current object.
    * <p>There are two types of reference pointers, one to the User Data
    *    input parser and one to a list (SlipHeader object). Delete both.</p>
    * <p>Deleting the current object deletes the name and nested Descriptor List.</p>
    */
   void SlipDescription::deleteAll() {
      if ((type == DATA) && ptr) delete (SlipDatum*)ptr;
      else ((SlipHeader*)ptr)->deleteList();
      delete this;
   }; // void SlipDescription::deleteAll()

   /**
    * @brief Delete a a data object or an ANONYMOUS list and the current object.
    * <p>The pointer to the User Defined input parser and the current object
    *    are deleted. The input parser is not deleted, only the pointer to
    *    the object.</p>
    * <p>Deleting the current object deletes the name and nested Descriptor List.</p>
    */
   void SlipDescription::deleteData() {
      if ((type == DATA) && ptr) delete (SlipDatum*)ptr;
      else if (type == ANONYMOUS) ((SlipHeader*)ptr)->deleteList();
      delete this;
   }; // void SlipDescription::deleteData()

   /**
    * @brief Delete an ANONYMOUS list and the current object.
    * <p>An <b>ANONYMOUS</b> list is an unnamed list. This list is transient
    *    and is deleted in a production rule in the parser.</p>
    * <p>The list is a normal SlipHeader object and deletion is proscribed
    *    when there are multiple uses of the same object. In the input
    *    parser, deletion of an anonymous object is made after it is
    *    referenced as a sublist in some list. Therefor, actual deletion
    *    of the list data does not occur, but the reference count
    *    indicating that there are multiple references is decremented
    *    by one (1).</p>
    * <p>Deleting the current object deletes the name and nested Descriptor List.</p>
    */
   void SlipDescription::deleteList() {
      if ((type == ANONYMOUS) && ptr) ((SlipHeader*)ptr)->deleteList();
      delete this;
   }; // void SlipDescription::deleteList()

   /**
    * @brief Pretty-print the instance data.
    * @return pretty-printed string representing the data
    */
   string SlipDescription::toString() {
      stringstream pretty;
      if (name) pretty << right << setw(10) << *name << " ";
      else      pretty << right << setw(10) << sType[type] << ' ';
      if (nestedPtr)  {
         string dName;
         if (nestedPtr->name) dName = "<" + *(nestedPtr->name) + ">";
         else                 dName = "<NONAME>";
         pretty << right << setw(9) << dName << " ";
      } else             pretty << setw(10) << "NONAME ";
      if (ptr) {
         if (type == DATA) pretty << ((SlipDatum*)ptr)->toString();
         else              pretty << ((SlipHeader*)ptr)->writeToString();
       } else     pretty << "NULL";
      return pretty.str();
   }; // string SlipDescription::toString()

}; // namespace slip