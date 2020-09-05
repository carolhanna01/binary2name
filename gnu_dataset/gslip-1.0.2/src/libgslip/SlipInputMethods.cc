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
 * @file   SlipInputMethods.cpp
 * @author Arthur Schwarz
 * @date July 19, 2014, 12:42 PM
 */

#ifdef DEBUG_IO
   #undef DEBUG
   #define DEBUG(flag, stmt) if (flag) { stmt }
#else
   #define DEBUG(flag, stmt)
#endif  

#include "SlipInputMethods.h"
# include <iomanip>
# include <iostream>
# include <sstream>
# include <stdint.h>
# include <string>
# include "lex.Slip.h"
# include "Slip.h"
# include "SlipDatum.h"
# include "SlipDef.h"
# include "SlipDescription.h"
# include "SlipErr.h"
# include "SlipHeader.h"
# include "SlipRead.h"
# include "SlipRegister.h"
# include "SlipRegister.h"
# include "SlipSequencer.h"
# include "SlipSublist.h"

using namespace slip;

SlipInputMethods::SlipInputMethods(SlipRegister* reg, int debugFlag)
                        : reg(reg)
                        , inputDebugFlag((bool)(debugFlag & SlipRead::INPUT))
                        , debugFlag(debugFlag)
{
}

SlipInputMethods::SlipInputMethods(const SlipInputMethods& orig) {
}

SlipInputMethods::~SlipInputMethods() {
}

    /*********************************************************
     *                      BNF Actions                      *
    **********************************************************/

        /*************************************************
         *                listDefinition                 *
        **************************************************/

   /**
    * @brief Action for <i>declarationItem: name listDefinition ';'</i>
    * @param[in] title (string) title to be used for debug output
    * @param[in] name (string) 'name' in production RHS
    * @param[in] list (SlipDescription*) descriptor for a list in the RHS
    */
   void SlipInputMethods::declarationItem(string title, string* name, SlipDescription* list) {
      if (list == NULL) {
         errorCount++;
         postError(__FILE__, __LINE__, SlipErr::E4012, "parser::parser", string(*name), "");
         list = new SlipDescription(new SlipHeader(), SlipDescription::NAMED, debugFlag);
      }
      *list = new string(*name);
      *list = SlipDescription::NAMED;
      if (!reg->post(list)) {
         errorCount++;
         postError(__FILE__, __LINE__, SlipErr::E4011, "parser::parser", string(*name), "");
      }
      DEBUG(inputDebugFlag, debugOut(title, list););
      delete name;
      delete list;
}; // void SlipInputMethods::declarationItem(string title, string name, SlipDescription* desc)


   /**
    * @brief Action for <i>listDefinition: '(' description list ')'</i>
    * @param[in] title (string) title to be used for debug output
    * @param[in] desc (SlipDescription*) list descriptor '( <> )'
    * @param[in] list (SlipDescription*) list '( )'
    * @return (SlipDescription*) packet containing list
    */
   SlipDescription* SlipInputMethods::descriptionList(string title, SlipDescription* desc, SlipDescription* list) {
      if (list == NULL) {
         errorCount++;
         postError(__FILE__, __LINE__, SlipErr::E4010, "parser::parser", "", "");
         list = new SlipDescription(new SlipHeader(), SlipDescription::ANONYMOUS, debugFlag);
      }
      ((SlipHeader*)(list->getPtr()))->create_dList();
      if (desc != NULL) list->putDesc(desc);
      DEBUG(inputDebugFlag, debugOut(title, desc);)
      DEBUG(inputDebugFlag, debugOut(title, list);)
      return list;
   }; // SlipDescription* SlipInputMethods::descriptionList(string title, SlipDescription* desc, SlipDescription* list) 
   
   /**
    * @brief Action for <i>listDefinition: '(' description ')'</i>
    * @param[in] title (string) title to be used for debug output
    * @param[in] desc (SlipDescription*) list descriptor '( <> )'
    * @return (SlipDescription*) packet containing list
    */
   SlipDescription* SlipInputMethods::descriptionNULLList(string title, SlipDescription* desc) {
      SlipDescription* list = new SlipDescription(new SlipHeader(), SlipDescription::ANONYMOUS, debugFlag);
      ((SlipHeader*)(list->getPtr()))->create_dList();
      if (desc != NULL) list->putDesc(desc);
      DEBUG(inputDebugFlag, debugOut(title, list);)
      return list;
   }; // SlipDescription* SlipInputMethods::descriptionNULLList(string title, SlipDescription* desc)

   /**
    * @brief Action for <i>listDefinition: '(' mark description ')'</i>
    * @param[in] title (string) title to be used for debug output
    * @param[in] mark (UCHAR) list mark
    * @param[in] desc (SlipDescription*) list descriptor '( <> )'
    * @return (SlipDescription*) packet containing list
    */
   SlipDescription* SlipInputMethods::markDescription(string title, UCHAR mark, SlipDescription* desc) {
      SlipHeader* head = new SlipHeader();
      SlipDescription* list = new SlipDescription(head, SlipDescription::ANONYMOUS, debugFlag);
      ((SlipHeader*)(list->getPtr()))->create_dList();
      head->putMark(mark);
      if (desc != NULL)  *(list->getDesc()) = desc;
      DEBUG(inputDebugFlag, debugOut(title, list);)
      return list;
   }; // SlipDescription* SlipInputMethods::markDescription(string title, UCHAR mark, SlipDescription* desc)

   /**
    * @brief Action for <i>listDefinition: '(' mark description list ')'</i>
    * @param[in] title (string) title to be used for debug output
    * @param[in] mark (UCHAR) list mark
    * @param[in] desc (SlipDescription*) list descriptor '( <> )'
    * @param[in] list (SlipDescription*) list '( )'
    * @return (SlipDescription*) packet containing list
    */
   SlipDescription* SlipInputMethods::markDescriptionList(string title, UCHAR mark, SlipDescription* desc, SlipDescription* list)
   {
      if (list == NULL) {
         errorCount++;
         postError(__FILE__, __LINE__, SlipErr::E4010, "parser::parser", "", "");
         list = new SlipDescription(new SlipHeader(), SlipDescription::ANONYMOUS, debugFlag);
      } 
      ((SlipHeader*)(list->getPtr()))->create_dList();
      ((SlipHeader*)(list->getPtr()))->putMark(mark);
      if (desc != NULL) *(list->getDesc()) = desc;
      DEBUG(inputDebugFlag, debugOut(title, desc);)
      DEBUG(inputDebugFlag, debugOut(title, list);)
      return list;
   }; // SlipDescription* SlipInputMethods::markDescriptionList(string title, UCHAR mark, SlipDescription* desc, SlipDescription* list)
   
   /**
    * @brief Action for <i>listDefinition: '(' mark list ')'</i>
    * @param[in] title (string) title to be used for debug output
    * @param[in] mark (UCHAR) list mark
    * @param[in] list (SlipDescription*) list '( )'
    * @return (SlipDescription*) packet containing list
    */
   SlipDescription* SlipInputMethods::markList(string title, UCHAR mark, SlipDescription* list) {
      if (list == NULL) {
         errorCount++;
         postError(__FILE__, __LINE__, SlipErr::E4010, "parser::parser", "", "");
         list = new SlipDescription(new SlipHeader(), SlipDescription::ANONYMOUS, debugFlag);
      }
      if (list->getType() != SlipDescription::NAMED)
         ((SlipHeader*)(list->getPtr()))->putMark(mark);
      DEBUG(inputDebugFlag, debugOut(title, list);)
      return list;
   }; // SlipDescription* SlipInputMethods::markList(string title, UCHAR mark, SlipDescription* list)
   
   /**
    * @brief Action for <i>listDefinition: '(' list ')'</i>
    * @param[in] title (string) title to be used for debug output
    * @param[in] list (SlipDescription*) list '( )'
    * @return (SlipDescription*) packet containing list
    */
   SlipDescription* SlipInputMethods::listDefinition(string title, SlipDescription* list) {
      if (list == NULL) {
         errorCount++;
         postError(__FILE__, __LINE__, SlipErr::E4010, "parser::parser", "", "");
         list = new SlipDescription(new SlipHeader(), SlipDescription::ANONYMOUS, debugFlag);
      }
      DEBUG(inputDebugFlag, debugOut(title, list);)
      return list;
   }; // SlipDescription* SlipInputMethods::listDefinition(string title, SlipDescription* list)
   
   /**
    * @brief Action for <i>listDefinition: '(' mark ')'</i>
    * @param[in] title (string) title to be used for debug output
    * @param[in] mark (UCHAR) list mark
    * @return (SlipDescription*) packet containing list
    */
   SlipDescription* SlipInputMethods::mark(string title, UCHAR mark) {
      SlipHeader* head = new SlipHeader();
      head->putMark(mark);
      SlipDescription* list = new SlipDescription(head, SlipDescription::ANONYMOUS, debugFlag);
      DEBUG(inputDebugFlag, debugOut(title, list);)
      return list;
   }; // SlipDescription* SlipInputMethods::mark(string title, UCHAR mark)
   
   /**
    * @brief Action for <i>listDefinition: '('  ')'</i>
    * @param[in] title (string) title to be used for debug output
    * @return (SlipDescription*) packet containing list
    */
   SlipDescription* SlipInputMethods::nullList(string title) {
      SlipDescription* list = new SlipDescription(new SlipHeader(), SlipDescription::ANONYMOUS, debugFlag);
      DEBUG(inputDebugFlag, debugOut(title, list);)
      return list;
   }; // SlipDescription* SlipInputMethods::nullList(string title)

        /*************************************************
         *                  description                  *
        **************************************************/

   /**
    * @brief Action for <i>description: '<' description '>''</i>
    * @param[in] title (string) title to be used for debug output
    * @param[in] desc (SlipDescription*) list descriptor '( <> )'
    * @return (SlipDescription*) packet containing list
    */
   SlipDescription* SlipInputMethods::description(string title, SlipDescription* desc) {
      SlipHeader* head = new SlipHeader(); 
      SlipDescription* list = new SlipDescription(head, SlipDescription::ANONYMOUS, debugFlag);
      if (desc != NULL) {
         head->create_dList();
         copyDList(desc, list);
         desc->deleteList();
      }
      DEBUG(inputDebugFlag, debugOut(title, list);)
      return list;
   }; // SlipDescription* SlipInputMethods::description(string title, SlipDescription* list)

   /**
    * @brief Action for <i>description: '<' description descriptionList '>''</i>
    * @param[in] title (string) title to be used for debug output
    * @param[in] desc (SlipDescription*) list descriptor '( <> )'
    * @param[in] list (SlipDescription*) list '( )'
    * @return (SlipDescription*) packet containing list
    */
   SlipDescription* SlipInputMethods::descriptionDescriptionList(string title, SlipDescription* desc, SlipDescription* list) {
      if (list == NULL) 
         list = new SlipDescription(new SlipHeader(), SlipDescription::ANONYMOUS, debugFlag);
      if (desc != NULL) {
         ((SlipHeader*)(list->getPtr()))->create_dList();
         if (desc->getName()) *(list->getDesc()) =desc;
         copyDList(desc, list);
         desc->deleteList();
      }
      DEBUG(inputDebugFlag, debugOut(title, desc);)
      DEBUG(inputDebugFlag, debugOut(title, list);)
      return list;
   }; //  SlipDescription* SlipInputMethods::descriptionDescriptionList(string title, SlipDescription* list)

   /**
    * @brief Action for <i>description: '<' mark '>''</i>
    * @param[in] title (string) title to be used for debug output
    * @param[in] mark (UCHAR) description list mark
    * @return (SlipDescription*) packet containing list
    */
   SlipDescription* SlipInputMethods::markDescription(string title, UCHAR mark) {
      SlipHeader* head = new SlipHeader(); 
      head->putMark(mark);
      SlipDescription* list = new SlipDescription(head, SlipDescription::ANONYMOUS, debugFlag);
      DEBUG(inputDebugFlag, debugOut(title, list);)
      return list;
   }; // SlipDescription* SlipInputMethods::markDescription(string title, UCHAR mark
   
   /**
    * @brief Action for <i>description: '<'  '>''</i>
    * @param[in] title (string) title to be used for debug output
    * @return (SlipDescription*) packet containing list
    */
   SlipDescription* SlipInputMethods::nullDescription(string title) {
      SlipHeader* head = new SlipHeader(); 
      SlipDescription* list = new SlipDescription(head, SlipDescription::ANONYMOUS, debugFlag);
      DEBUG(inputDebugFlag, debugOut(title, list);)
      return list;
   }; // SlipDescription* SlipInputMethods::nullDescription(string title)

        /*************************************************
         *                descriptionList                *
        **************************************************/

   /**
    * @brief Action for <i>descriptionList: '&lt;' mark descriptionItemlist '&gt;'</i>
    * @param[in] title (string) title to be used for debug output
    * @param[in] mark (UCHAR) description list mark
    * @param[in] desc (SlipDescription*) list descriptor 
    * @return (SlipDescription*) packet containing list
    */
   SlipDescription* SlipInputMethods::markDescriptionItemList(string title, UCHAR mark, SlipDescription* desc) {
      if (desc != NULL) ((SlipHeader*)(desc->getPtr()))->putMark(mark);
      DEBUG(inputDebugFlag, debugOut(title, desc);)
      return desc;
   }; // SlipDescription* SlipInputMethods::markDescriptionItemList(string title, UCHAR mark, SlipDescription* desc) {

        /*************************************************
         *              descriptionItemlist              *
        **************************************************/

   /**
    * @brief Action for <i>descriptionItemlist: descriptionItemlist descriptionElement descriptionElement</i>
    * @param[in] title (string) title to be used for debug output
    * @param[in] list (SlipDescription*) list or SlipDatum object in descriptor list 
    * @param[in] key (SlipDescription*) key of &lt;key, value&gt; tuple 
    * @param[in] value (SlipDescription*) value of &lt;key, value&gt; tuple 
    * @return (SlipDescription*) packet containing list
    */
   SlipDescription* SlipInputMethods::descriptionItemlist(string title, SlipDescription* list, SlipDescription* key, SlipDescription* value) {
      if (list == NULL) 
         list = new SlipDescription(new SlipHeader(), SlipDescription::ANONYMOUS, debugFlag);

      bool errorFlag =  checkDescriptorElement(SlipErr::E4008, key, value)
                     || checkDescriptorElement(SlipErr::E4009, value, key);

      if (errorFlag) { 
         errorCount++;
      } else if ((key != NULL) && (value != NULL)) {
            ((SlipHeader*)list->getPtr())->enqueue(*(key->getPtr()));
            ((SlipHeader*)list->getPtr())->enqueue(*(value->getPtr()));
      }
      if (key != NULL) key->deleteList();
      if (value != NULL) value->deleteList();
      DEBUG(inputDebugFlag, debugOut(title, list);)
      return list;
   }; // 

   /**
    * @brief Action for <i>descriptionItemlist: descriptionElement descriptionElement</i>
    * @param[in] title (string) title to be used for debug output
    * @param[in] key (SlipDescription*) key of &lt;key, value&gt; tuple 
    * @param[in] value (SlipDescription*) value of &lt;key, value&gt; tuple 
    * @return (SlipDescription*) packet containing list
    */
   SlipDescription* SlipInputMethods::descriptionItem(string title, SlipDescription* key, SlipDescription* value) {
      SlipDescription* tuple = new SlipDescription(new SlipHeader(), SlipDescription::ANONYMOUS, debugFlag);
      bool errorFlag =  checkDescriptorElement(SlipErr::E4008, key, value)
                     || checkDescriptorElement(SlipErr::E4009, value, key);
      if (errorFlag) { 
         errorCount++;
      } else if ((key != NULL) && (value != NULL)) {
            ((SlipHeader*)tuple->getPtr())->enqueue(*(key->getPtr()));
            ((SlipHeader*)tuple->getPtr())->enqueue(*(value->getPtr()));
      }
      if (key != NULL) key->deleteList();
      if (value != NULL) value->deleteList();
      DEBUG(inputDebugFlag, debugOut(title, tuple);)
      return tuple;
   }; // SlipDescription* SlipInputMethods::descriptionItem(string title, SlipDescription* key, SlipDescription* value)

        /*************************************************
         *                   # INCLUDE                   *
        **************************************************/
   
   /**
    * @brief Open an include file
    * @param[in] title (string) title to be used for debug output
    * @param[in] path (SlipDescription*) path to include file
    */
   void SlipInputMethods::includeFile(string title, SlipDescription* path) {
      if (path == NULL) {
         cout << "INCLUDE because of previous error." << endl;
      } else {
         DEBUG(inputDebugFlag, debugOut(title, path);)
         SlipDatum* datum = (SlipDatum*)(path->getPtr());
         string filename(datum->toString());
         if (!saveLexState(filename)) {
            errorCount++;
            postError(__FILE__, __LINE__, SlipErr::E4007, "parser::parser", "", filename);   
         }
         path->deleteData();
      }
   }; // void SlipInputMethods::include(string title, SlipDatum path)

        /*************************************************
         *                 userDataList                  *
        **************************************************/
   void SlipInputMethods::userDataList(string title, string* name) {
      if (reg->getParse(*name) == NULL) {
        errorCount++;
        postError(__FILE__, __LINE__, SlipErr::E4005, "parser::parser", "", *name);   
     } else 
        delete name;
   }; // void SlipInputMethods::userDataList(string title, string* name)

        /*************************************************
         *                 listItemList                  *
        **************************************************/

   /**
    * @brief Action for <i>listItemList: listItemList listItem</i>
    * @param[in] title (string) title to be used for debug output
    * @param[in] list (SlipDescription*) list 
    * @param[in] item (SlipDescription*) item to be appended to list 
    * @return (SlipDescription*) packet containing list
    */
   SlipDescription* SlipInputMethods::listItemList(string title, SlipDescription* list, SlipDescription* item) {
      if (list == NULL)
         list = new SlipDescription(new SlipHeader(), SlipDescription::ANONYMOUS, debugFlag);
      if (item != NULL) {
         ((SlipHeader*)(list->getPtr()))->enqueue(*(item->getPtr()));
         item->deleteList(); 
      }
      DEBUG(inputDebugFlag, debugOut(title, list);)
      return list;
   }; // SlipDescription* SlipInputMethods::listItemList(string title, SlipDescription* list, SlipDescription* item)

   /**
    * @brief Action for <i>listItemList: listItemList listItem</i>
    * @param[in] title (string) title to be used for debug output
    * @param[in] item (SlipDescription*) first item in list 
    * @return (SlipDescription*) packet containing list
    */
   SlipDescription* SlipInputMethods::listItem(string title, SlipDescription* item) {
      SlipDescription* list = new SlipDescription(new SlipHeader(), SlipDescription::ANONYMOUS, debugFlag);
      if (item != NULL) {
         ((SlipHeader*)(list->getPtr()))->enqueue(*(item->getPtr())); 
         item->deleteList();
      }
      DEBUG(inputDebugFlag, debugOut(title, list);)
      return list;
   }; // SlipDescription* SlipInputMethods::listItem(string title, SlipDescription* item)

        /*************************************************
         *                   listItem                    *
        **************************************************/

   /**
    * @brief Action for <i>listItemList: listItemList listItem</i>
    * @param[in] title (string) title to be used for debug output
    * @param[in] list (SlipDescription*) list 
    * @return (SlipDescription*) packet containing list
    */
   SlipDescription* SlipInputMethods::listItemListDefinition(string title, SlipDescription* list) {
      if (list->getDesc() != NULL) {
         copyDList(list->getDesc(), list);
      }
      DEBUG(inputDebugFlag, debugOut(title, list);)
      return list;
   }; // SlipDescription* SlipInputMethods::listItemListDefinition(string title, SlipDescription* list)

        /*************************************************
         *                   namedList                   *
        **************************************************/

   /**
    * @brief Action for <i>{ name }</i>
    * @param[in] title (string) title to be used for debug output
    * @param[in] name (SlipDescription*) list 
    * @return (SlipDescription*) packet containing list
    */
   SlipDescription* SlipInputMethods::namedList(string title, string* name) {
      SlipDescription* list = new SlipDescription(reg->getSublist(*name), name, SlipDescription::NAMED, debugFlag);
      if (list->getPtr() == NULL) {
         if (reg->post(*name))
            *list = reg->getSublist(*name);
      }
      DEBUG(inputDebugFlag, debugOut(title, list);)
      delete name;
      return list;
   }; // SlipDescription* SlipInputMethods::namedList(string title, string* name)
      
        /*************************************************
         *                   userData                    *
        **************************************************/

/**
 * @brief Process user Data input object definitions.
 * 
 * <p>Parser processing recognizes a User Data class name and data; the data
 *    is given as a list. Processing retrieves the registered User Data 
 *    parser and transmits the input list to the User Data parser for User
 *    specific processing. The User Data parser is required to delete the
 *    input list if required. From the perspective of the language parser,
 *    once the User Data parser is sent the parsed list, it is the User
 *    Data parser's responsibility to handle </p>
 *
 * <p>There are two types of lists (from the language parser's perspective):</p>
 * <ul>
 *    <li>A list delimited by parenthesis - a normal list.</li>
 *        Format: ( ... )
 *    <li>Areference to a named list.</li>
 *        Format: {name}
 * </ul>
 * <p>For both these cases, there can be no unresolved forward references. If
 *    an unresolved forward reference is contained in the list then the forward
 *    reference will be 'resolved' as an empty list and the object data will not
 *    be correct.</p>
 * </p>For example, the following input file:
 * <tt><pre>
 *    list1 ( SP {list2} {list2} );
 *    list2 ( 1 2 3 );
 *    {list1}
 * </pre></tt>
 * <p>Will cause the User Data parser to received the list '( )' because at the
 *    time that {list2} is referenced in list1, list2 is undefined. If we assume
 *    that the User Data parser returns '0' for an empty list and '1' for a
 *    non-empty list, then the returned list will be:</p>
 * <tt><pre>
 *    ( 0 1 2 3 )
 * </pre></tt>
 * <p>If we provide the forward reference passed to the User Data is resolved,
 *    as in:</p>
 * <tt><pre>
 *    list2 ( 1 2 3 );
 *    list1 ( SP {list2} {list2} );
 *    {list1}
 * </pre></tt>
 * <p>We would get:</p>
 * <tt><pre>
 *    ( 1 1 2 3 )
 * </pre></tt>
 * <p>We can not guarantee that there will be no forward references for object
 *    creation in the User Data parser for output files generated by SLIP. If
 *    you expect that as part of the output generation process a forward 
 *    reference will be created, you must hand check and correct the generated
 *    file.</p>
 * 
 * @param[in] title (string) BNF context in parser
 * @param[in] str (string*) SlipDescriptor packet with User Data name
 * @param[in] list (SlipDescriptor*) SlipDescriptor packet with object data
*/
   SlipDescription* SlipInputMethods::userData(string title, string* str, SlipDescription* list) {
      SlipDescription* retVal = NULL;
      Parse fnc = reg->getParse(*str);
      if (fnc == NULL) {
         DEBUG(inputDebugFlag, cout << left << setw(27) << setfill(' ') << title << endl;)
         errorCount++;
         errorCount++;
         if (list != NULL) {
            postError(__FILE__, __LINE__, SlipErr::E4005, "parser::parser", "", *str);  
            list->deleteList();
         } else 
            postError(__FILE__, __LINE__, SlipErr::E4005, "parser::parser");  
         delete str;
         return retVal;
      } else {
         if (list->getDesc()) {
            if (!((SlipHeader*)(list->getPtr()))->isDList()) 
               ((SlipHeader*)(list->getPtr()))->create_dList();
            copyDList(list->getDesc(), list);
         }
         retVal = new SlipDescription(&(fnc(*((SlipHeader*)(list->getPtr())))), str, SlipDescription::DATA, debugFlag);
         DEBUG(inputDebugFlag, cout << left << setw(27) << setfill(' ') << title << retVal->toString() << endl;)
      }
      delete str;
      list->resetPtr();
      list->deleteList();
      return retVal;
   }; // SlipDescription* SlipInputMethods::userData(string* str, SlipDescription* list, string title)

        /*************************************************
         *                General Actions                *
        **************************************************/

   /**
    * @brief Return the input list and output it when debug is on.
    * @param[in] title (string) title to be used for debug output
    * @param[in] list (SlipDescription*) list '( )'
    * @return (SlipDescription*) packet containing list
    */
   SlipDescription* SlipInputMethods::simpleReturn(string title, SlipDescription* list) {
      DEBUG(inputDebugFlag, debugOut(title, list);)
      return list;
   }; // SlipDescription* SlipInputMethods::simpleReturn(string title, SlipDescription* list)

    /*********************************************************
     *                   General Functions                   *
    **********************************************************/

/**
 * @brief Check a descriptorElement for nullness.
 * <p>If item1 is <b>NULL</b> post a diagnostic message, return an error indication 
 *    and delete item2 if it is not <b>NULL</b>.
 * <p>This method has an unfortunate side effect of deleting item2 before it is
 *    processed by the caller and could lead to a catastrophic use of a deleted
 *    object. This is avoided by poor quality programming. The sequence of events
 *    in the caller allows item2 to be deleted without a catastrophic failure but
 *    this is definitely not th way to run a business.</p>
 * <p>If item2 is <b>NULL</b>, there is an internal check to see if item2 contains
 *    a pointer to a SlipDatum object, and if so, this object is deleted.</p>
 */
bool SlipInputMethods::checkDescriptorElement(SlipErr::eError errorName, SlipDescription* item1, SlipDescription* item2) {
   bool retval = false;
   if (item1 == NULL) {
      retval = true;
      postError(__FILE__, __LINE__, errorName, "parser::checkDescriptorElement", "", "");
      if (item2 != NULL) {
         item2->deleteData();
      }
   }
   return retval;
}; // bool SlipInputMethods::checkDescriptorElement(SlipErr::eError errorName, SlipDescription* desc)

/**
 * @brief Register and resolve Description List references.
 * <p>Either register a forward reference to a named list in a Description List
 *    or attempt to resolve a forward reference to a named list.</p>
 * <p>If the input indicates that this a named list definition then the definition 
 *    is registered and any outstanding forward references resolved. All future 
 *    references are resolved immediately (since the definition is known)</p>
 * <p>If this is a reference to a named list then the reference is stored for
 *    future resolution, or if the named list definition is available, it is
 *    resolved immediately.</p>
 * <p>In this context, resolution implies that the referenced named list is
 *    copied from the source to the destination Description List.</p>
 * <p>If the source is an anonymous list then it is immediately copied
 *    into the destination Descriptor List field.</p>
 * <p><b>Algorithm</b></p>
 * <p>If either the source (from) or the destination (to) list is <b>NULL</b>
 *    then this is an internal error message is posted, otherwise:</p>
 * <table>
 *    <caption>Description List Related Processing</caption>
 *    <tr>
 *       <th>to\\from</th>
 *       <th>ANONYMOUS</th>
 *       <th>NAMED LIST</th>
 *    </tr>
 *    <tr>
 *       <th>ANONYMOUS</th>
 *       <td>Copy from =&gt; to</td>
 *       <td>Copy from =&gt; to if available otherwise forward reference</td>
 *    </tr>
 *    <tr>
 *       <th>NAMED LIST</th>
 *       <td>Illegal. Redefining a named list not valid.</td>
 *       <td>Illegal. Redefining a named list not valid.</td>
 *    </tr>
 * </table>
 * <p>It should be noted that a named list is also called an "Expanded Name".</p>
 * 
 * @param[in] from (Description*)  Description List (source)
 * @param[inout] to (Description*) Containing list (destination)
 * 
 */
bool SlipInputMethods::copyDList(SlipDescription* from, SlipDescription*  to) {
   int count = errorCount;
   DEBUG(inputDebugFlag, cout << left << setw(27) << setfill(' ') << "parser::copyDList from " << from->toString() << endl;)
   DEBUG(inputDebugFlag, cout << left << setw(27) << setfill(' ') << "parser::copyDList to      " << to->toString() << endl;)
   if (to == NULL) {
      errorCount++;
      postError(__FILE__, __LINE__, SlipErr::E4010, "parser::copyDList", "", "");
   } else if (from != NULL) {
      int sw = (to->getType() == SlipDescription::NAMED)? 1: 0;
      sw  |= (from->getType() == SlipDescription::NAMED)? 2: 0;
      switch (sw) {
      case 0x0: {                 // <from, to> <ANONYMOUS, ANONYMOUS>
         SlipHeader* header = (SlipHeader*)(to->getPtr());
         SlipHeader* dList  = (SlipHeader*)(from->getPtr());
         SlipHeader& hdList = header->getDList();
         hdList = *dList;
         DEBUG(inputDebugFlag, cout << left << setw(27) << setfill(' ') << "parser::copyDList" << to->toString() << endl;)
         break; }
      case 0x1:                  // <from, to> <ANONYMOUS, NAMED    >
         errorCount++;
         postError(__FILE__, __LINE__, SlipErr::E4017, "parser::copyDList", "", "");
         break;
      case 0x2:                  // <from, to> <NAMED,    ANONYMOUS>
         reg->copyDList(from, to);
         DEBUG(inputDebugFlag, cout << left << setw(27) << "parser::copyDList" << from->toString() << endl;)
         break;
      case 0x3:                  // <from, to> <NAMED,    NAMED >
         errorCount++;
         postError(__FILE__, __LINE__, SlipErr::E4016, "parser::copyDList", "", "");
         break;
      }
   }
   return (count == errorCount);
}; // bool SlipInputMethods::copyDList((SlipRegister* reg, SlipDescription* from, SlipDescription*  to)

/**
 * @brief Output formatted SlipDescriptor data
 * @param[in] desc (SlipDescription*) packet to be formatted and output
 * @param[in] title (string) title
 */
void SlipInputMethods::debugOut(const string title, SlipDescription* desc) {
   cout << left << setw(27) << setfill(' ') << title;
   if (desc) cout << desc->toString();
   cout << endl; 
}; // void SlipInputMethods::DebugOut(const string title, SlipDescriptor* desc) {

void SlipInputMethods::setParserDebugON(int debugFlag) { 
   SlipInputMethods::debugFlag = debugFlag;
   inputDebugFlag = (bool)(debugFlag & SlipRead::INPUT)? true: false; 
}; // void SlipInputMethods::setParserDebugON(int debugFlag)

void SlipInputMethods::setParserDebugOFF() { inputDebugFlag = false; }
