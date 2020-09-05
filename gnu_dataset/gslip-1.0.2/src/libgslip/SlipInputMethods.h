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
 * @file   SlipInputMethods.h
 * @author Arthur Schwarz
 * @date July 19, 2014, 12:42 PM
 * 
 * @brief A collected set of methods to support parsing
 */

#ifndef SLIPINPUTMETHODS_H
#define	SLIPINPUTMETHODS_H

# include <string>
# include "SlipDescription.h"
# include "SlipRegister.h"

namespace slip {
   class SlipInputMethods {
   private:
      SlipRegister* reg;
      int debugFlag;
      int errorCount;
      bool inputDebugFlag;
   private:
      SlipInputMethods(const SlipInputMethods& orig);
   public:
      SlipInputMethods(SlipRegister* reg, int debugFlag);
      virtual ~SlipInputMethods();
      bool checkDescriptorElement(SlipErr::eError errorName, SlipDescription* item1, SlipDescription* item2);
      bool copyDList(SlipDescription* from, SlipDescription*  to);
      static void debugOut(const string title, SlipDescription* desc);
      int  getDebugFlag()  { return debugFlag; }
      int  getErrorCount() { return errorCount; }
      bool getInputFlag()  { return inputDebugFlag; }
      void incErrorCount() { errorCount++; }
      void setParserDebugON(int debugFlag);
      void setParserDebugOFF();

    /*********************************************************
     *                      BNF Actions                      *
    **********************************************************/

        /*************************************************
         *                listDefinition                 *
        **************************************************/
      void 
      declarationItem(string title, string* name, SlipDescription* list);
      SlipDescription* 
      descriptionList(string title, SlipDescription* desc, SlipDescription* list);
      SlipDescription* 
      descriptionNULLList(string title, SlipDescription* desc);
      SlipDescription* 
      markDescription(string title, UCHAR mark, SlipDescription* desc);
      SlipDescription* 
      markDescriptionList(string title, UCHAR mark, SlipDescription* desc, SlipDescription* list);
      SlipDescription* 
      markList(string title, UCHAR mark, SlipDescription* list);
      SlipDescription* 
      listDefinition(string title, SlipDescription* list);
      SlipDescription* 
      mark(string title, UCHAR mark);
      SlipDescription* 
      nullList(string title);

        /*************************************************
         *                  description                  *
        **************************************************/
      SlipDescription* 
      markDescription(string title, UCHAR mark);
      SlipDescription* 
      nullDescription(string title);
      SlipDescription* 
      descriptionDescriptionList(string title, SlipDescription* desc, SlipDescription* list);
      SlipDescription* 
      description(string title, SlipDescription* desc);

        /*************************************************
         *                descriptionList                *
        **************************************************/
      SlipDescription* 
      markDescriptionItemList(string title, UCHAR mark, SlipDescription* desc);

        /*************************************************
         *              descriptionItemlist              *
        **************************************************/
      SlipDescription* 
      descriptionItemlist(string title, SlipDescription* list, SlipDescription* key, SlipDescription* value);
      SlipDescription* 
      descriptionItem(string title, SlipDescription* key, SlipDescription* value);

        /*************************************************
         *                   # INCLUDE                   *
        **************************************************/
      void 
      includeFile(string title, SlipDescription* path);

        /*************************************************
         *                 userDataList                  *
        **************************************************/
      void 
      userDataList(string title, string* name);

        /*************************************************
         *                 listItemList                  *
        **************************************************/
      SlipDescription* 
      listItemList(string title, SlipDescription* list, SlipDescription* item);
      SlipDescription* 
      listItem(string title, SlipDescription* item);

        /*************************************************
         *                   listItem                    *
        **************************************************/
      SlipDescription* 
      listItemListDefinition(string title, SlipDescription* list);

        /*************************************************
         *                   namedList                   *
        **************************************************/
      SlipDescription* 
      namedList(string title, string* name);
      
        /*************************************************
         *                   userData                    *
        **************************************************/
      SlipDescription* 
      userData(string title, string* str, SlipDescription* list);

        /*************************************************
         *                General Actions                *
        **************************************************/
      SlipDescription* 
      simpleReturn(string title, SlipDescription* list);
   };
}; // namespace Slip

#endif	/* SLIPINPUTMETHODS_H */

