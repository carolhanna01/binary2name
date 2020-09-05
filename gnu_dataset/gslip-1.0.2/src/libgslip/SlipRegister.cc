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
 * @file    SlipRegister.cpp
 * @author  A. Schwarz
 * @date    December 31, 2013
 *
 */

# include <string>
# include <iomanip>
# include <iostream>
# include <sstream>
# include "SlipAsciiEntry.h"
# include "SlipBinaryEntry.h"
# include "SlipDatum.h"
# include "SlipDef.h"
# include "SlipDescription.h"
# include "SlipErr.h"
# include "SlipHashEntry.h"
# include "SlipHeader.h"
# include "SlipRegister.h"
#include "Slip.Bison.hpp"
#include "SlipRead.h"

using namespace std;

namespace slip {

#undef DEBUG
#ifdef DEBUG_IO
   #define DEBUG(flag, stmt) if (flag) stmt
#else
   #define DEBUG(flag, stmt)
#endif

const static string sFlag[] = {"false ", "true  "};

       /*************************************************
        *           Constructors & Destructors          *
        ************************************************/

   /**
    * @brief The copy constructor is not allowed.
    * <p>The copy constructor is a private method and not available to mortals.</p>
    * @param[in] X (SlipRegister&) an original version of a SlipRegister object
    */
   SlipRegister::SlipRegister(const SlipRegister& X) {
   }; // SlipRegister::SlipRegister(const SlipRegister&)

   /**
    * @brief Create a SlipRegister object.
    * <p>A <i>SlipRegister</i> object is created for each instance of an
    *    input reader (<i>SlipRead</i>) or writer (<i>SlipWrite</i>.) The
    *    object has three fields, a pointer to a <i>SlipHash</i> object
    *    the input debug mask (<b>debugFlag</b>), and flags for debug
    *    hashing, <b>hashDebugFlag</b>, and and input debugging,
    *    <b>inputDebugFlag</b>.</p>
    * <p>The hash table is created with <b>null</b> pointers for each entry.</p>
    * @param[in] debugFlag (int) debug flag mask
    */
   SlipRegister::SlipRegister(int debugFlag)
                                :  debugFlag(debugFlag)              // all flags import
                                ,  hashDebugFlag((bool)(debugFlag & SlipRead::INPUT))
                                ,  inputDebugFlag((bool)(debugFlag & SlipRead::INPUT))
                                ,  hashTable(new SlipHash(debugFlag)) { }

   /**
    * @brief delete the hash table.
    */
   SlipRegister::~SlipRegister() {
      delete hashTable;
   }; // SlipRegister::~SlipRegister()

       /*************************************************
        *                    Methods                    *
        ************************************************/

   /**
    * @brief Return the hash table entry at the indexed position.
    * @param ndx (int) index into hash table
    * @return hash table entry or <b>null</b>
    */
   SlipHashEntry* SlipRegister::operator[](int ndx) const {
      return &(*hashTable)[ndx];
   }; // SlipHashEntry& SlipRegister::operator[](int ndx) const

   /**
    * @brief Determines whether a state transition is valid and sets the entry type.
    * <p>Verifies that the requested transition is correct and transitions
    *    to the next valid state. Unconditionally sets the entry
    *    (<i>SliphashEntry</i> type.</p>
    * <p>The transition table indicates the input entry type and the next entry
    *    type.</p>
    * <p>The state machine is given as follows:</p>
    * <br />
    * <center><table>
    * <caption><b>Transition&rarr;Action Table</b></caption>
    *    <tr>
    *       <th></th>
    *       <th colspan=7><b>Exit Type&rarr;Action</b></th>
    *    </tr>
    *    <tr>
    *       <th>type</th>
    *       <th><b>ILLEGAL</b></th>
    *       <th><b>USERDATA</b></th>
    *       <th><b>REFERENCE</b></th>
    *       <th><b>DEFINED</b></th>
    *       <th><b>R_AND_D</b></th>
    *    </tr>
    *    <tr>
    *       <th><b>ILLEGAL</b></th>
    *       <td>ILLEGAL&rarr;error</td>
    *       <td>USERDATA&rarr;error</td>
    *       <td>REFERENCE&rarr;error</td>
    *       <td>DEFINED&rarr;error</td>
    *       <td>R_AND_D&rarr;error</td>
    *    </tr>
    *    <tr>
    *       <th><b>USERDATA</b></th>
    *       <td>ILLEGAL&rarr;error</td>
    *       <td>USERDATA&rarr;&epsilon;</td>
    *       <td>REFERENCE&rarr;error</td>
    *       <td>DEFINED&rarr;error</td>
    *       <td>R_AND_D&rarr;error</td>
    *    </tr>
    *    <tr>
    *       <th><b>UNKNOWN</b></th>
    *       <td>ILLEGAL&rarr;error</td>
    *       <td>USERDATA&rarr;error</td>
    *       <td>REFERENCE&rarr;error</td>
    *       <td>DEFINED&rarr;error</td>
    *       <td>R_AND_D&rarr;error</td>
    *    </tr>
    *    <tr>
    *       <th><b>ANONYMOUS</b></th>
    *       <td>ILLEGAL&rarr;error</td>
    *       <td>USERDATA&rarr;error</td>
    *       <td>REFERENCE&rarr;error</td>
    *       <td>DEFINED&rarr;error</td>
    *       <td>R_AND_D&rarr;error</td>
    *    </tr>
    *    <tr>
    *       <th><b>REFERENCE</b></th>
    *       <td>ILLEGAL&rarr;error</td>
    *       <td>USERDATA&rarr;error</td>
    *       <td>REFERENCE&rarr;&epsilon;</td>
    *       <td>R_AND_D&rarr;&epsilon;</td>
    *       <td>R_AND_D&rarr;&epsilon;</td>
    *    </tr>
    *    <tr>
    *       <th><b>DEFINED</b></th>
    *       <td>ILLEGAL&rarr;error</td>
    *       <td>USERDATA&rarr;error</td>
    *       <td>R_AND_D&rarr;&epsilon;</td>
    *       <td>DEFINED&rarr;&epsilon;</td>
    *       <td>R_AND_D&rarr;&epsilon;</td>
    *    </tr>
    *    <tr>
    *       <th><b>R_AND_D</b></th>
    *       <td>ILLEGAL&rarr;error</td>
    *       <td>USERDATA&rarr;error</td>
    *       <td>REFERENCE&rarr;error</td>
    *       <td>DEFINED&rarr;error</td>
    *       <td>R_AND_D&rarr;error</td>
    *    </tr>
    * </table>
    * </center>
    * <table border="0" width="400">
    *    <tr>
    *       <td><b>&epsilon;</b></td>
    *       <td>Do nothing. There is no indicated action, ignore.</td>
    *    </tr>
    *    <tr>
    *       <td><b>error</b></td>
    *       <td>An internal error has occurred. There is an illegal transaction
    *           from one state to another.</td>
    *    </tr>
    *    <tr>
    *       <td><b>ILLEGAL</b></td>
    *       <td>There should be no input of type ILLEGAL. Any such input is
    *           illegal, the exit type is illegal, and a message will be
    *           issued.</td>
    *    </tr>
    *    <tr>
    *       <td><b>USERDATA</b></td>
    *       <td>A USERDATA type represents a <i>SlipDatum</i> user data
    *           object. This entry type is invariant, the input and output
    *           entry type will be <b>UNDEFINED</b>.</td>
    *    </tr>
    *    <tr>
    *       <td><b>REFERENCED</b></td>
    *       <td>The list has been referenced but not defined.</td>
    *    </tr>
    *    <tr>
    *       <td><b>DEFINED</b></td>
    *       <td>The list has been defined but not referenced.</td>
    *    </tr>
    *    <tr>
    *       <td><b>R_AND_D</b></td>
    *       <td>The list has been defined and registered.</td>
    *    </tr>
    * </table>
    *
    * @see <i>SlipHashEntry</i>
    * <p><b>ERRORS</b></p>
    *
    * @param entry (SlipHashEntry*) existing registration entry
    * @param type (SlipHashEntry::Type) transition type
    * @return <b>true</b> transition successful
    */
   bool SlipRegister::fsm(SlipHashEntry& entry, SlipHashEntry::Type type) const {
      /**
       * @enum
       * @brief Avoid using fully qualified reference  to SlipHashEntry::Type.
       * <p>Type is reassigned to an anonymous enum in local space. This allows
       *    direct use of the enumerations without qualification.</p>
       */
      enum { ILLEGAL    = SlipHashEntry::ILLEGAL    //!< illegal value
           , USERDATA   = SlipHashEntry::USERDATA   //!< User Data entry
                                                 /**********************************/
                                                 /*      List specific types       */
                                                 /**********************************/
           , REFERENCED = SlipHashEntry::REFERENCED //!< List referenced but not defined
           , DEFINED    = SlipHashEntry::DEFINED    //!< List defined
           , R_AND_D    = SlipHashEntry::R_AND_D    //!< List Referenced and Defined
           , SIZE      = SlipHashEntry::SIZE        //!< Number of enumerations
      };

      /**
       * @brief Transition Table showing state transitions is the FSM.
       * <p>A Finite State Machine (FSM) consists of two components. One
       *    provides information as to what state to transition to and the
       *    other provides information on what action to perform  when
       *    the transition occurs (this is a Mealy Machine implementation
       *    of an FSM).</p>
       * <p>The transitions are given as a mapping between the current state,
       *    the current input symbol, and the next state. That is: </p>
       * <tt><pre>
       *    &delta;(input, current) &rarr; next state
       *      |      |          |
       *      |      |          o next state
       *      |      |
       *      |      o current state
       *      |
       *      o input state
       * </pre></tt>
       * <p>Where the input, current, and output states are made in reference to
       *    the SlipHashEntry::Type values. that is, the input and output and
       *    next states are SlipHashEntry::Type enumerations.</p>
       * <p>This can be combined with an arc transition into the following
       *    representation.</p>
       * <tt><pre>
       *    &delta;(input, current) &rarr; &lt;next state, action&gt;
       *      |      |           |           |
       *      |      |           |           o program action to perform
       *      |      |           o next state
       *      |      |
       *      |      o current state
       *      |
       *      o input state
       * </pre></tt>
       * <p>The input entry (SlipHashEntry) contains the state machine current
       *    state. The input argument type provides the event triggering a
       *    state change, and the next state is stored back into the entry.</P>
       * <p>The table inputs are given in the table rows, the columns represent
       *    states, and the cells ([row][column] represent the next state, the
       *    transition.</p>
       */
      const static int transition[][SIZE] = {
      /*input         ILLEGAL  USERDATA  REFERENCED  DEFINED  R_AND_D*/
      /*ILLEGAL  */ { ILLEGAL, USERDATA, REFERENCED, DEFINED, R_AND_D }
      /*USERDATA */,{ ILLEGAL, USERDATA, REFERENCED, DEFINED, R_AND_D }
      /*REFERENCE*/,{ ILLEGAL, USERDATA, REFERENCED, R_AND_D, R_AND_D }
      /*DEFINED  */,{ ILLEGAL, USERDATA, R_AND_D,    DEFINED, R_AND_D }
      /*R_AND_D  */,{ ILLEGAL, USERDATA, REFERENCED, DEFINED, R_AND_D }
      }; // transition[][SIZE]

      /**
       * @enum Action
       * @brief State actions in the Finite State Machine (FSM)
       * <p>The enumerations define the actions to perform. At each transition
       *    in the FSM, an actions is performed.</p>
       */
      enum Action { nada                                    //!< do nothing
                  , error                                   //!< post an error message
                  , post                                    //!< no longer used
      };

      /**
       * @brief Action Table showing action to perform on FSM transitions.
       * <p>A Finite State Machine (FSM) consists of two components. One
       *    provides information as to what state to transition to and the
       *    other provides information on what action to perform  when
       *    the transition occurs (this is a Mealy Machine implementation
       *    of an FSM).</p>
       * <p>The actions are given as a mapping between the current state,
       *    the current input symbol, and the next state. That is: </p>
       * <tt><pre>
       *    &delta;(input, current) &rarr; action
       *      |      |          |
       *      |      |          o programming action
       *      |      |
       *      |      o current state
       *      |
       *      o input state
       * </pre></tt>
       * <p>Where the input and current states are made in reference to
       *    the SlipHashEntry::Type values. that is, the input and output and
       *    next states are SlipHashEntry::Type enumerations.</p>
       * <p>This can be combined with an arc transition into the following
       *    representation.</p>
       * <tt><pre>
       *    &delta;(input, current) &rarr; &lt;next state, action&gt;
       *      |      |           |           |
       *      |      |           |           o program action to perform
       *      |      |           o next state
       *      |      |
       *      |      o current state
       *      |
       *      o input state
       * </pre></tt>
       * <p>The input entry (SlipHashEntry) contains the state machine current
       *    state. The input argument type provides the event triggering a
       *    state change, and the action is performed as a result.</P>
       * <p>The table inputs are given in the table rows, the columns represent
       *    states, and the cells ([row][column] represent the action.</p>
       */
      const static Action action[][SIZE] = {
      /*input         ILLEGAL  USERDATA  REFERENCED  DEFINED  R_AND_D*/
      /*ILLEGAL  */ { error,   error,    error,      error,   error   }
      /*USERDATA */,{ error,   nada,     error,      error,   error   }
      /*REFERENCE*/,{ error,   error,    nada,       nada,    nada    }
      /*DEFINED  */,{ error,   error,    nada,       error,   error   }
      /*R_AND_D  */,{ error,   error,    error,      error,   error   }
      }; // Action action[][SIZE]

      const static string sState[] = {"nada ", "post ", "error"};
      bool errorFlag = false;                                //!< <b>true</b> error
//      DEBUG(inputDebugFlag,                                       // Perform checks when debugging
//      if ((type < ILLEGAL) || (type >= SIZE)) {
//         errorFlag = true;
//         cout << "SlipRegister::post INTERNAL ERROR: input SlipHashEntry::Type illegal 0x"
//              << left << setfill('0') << setw(4) << type << endl;
//      } else)
      {
         SlipHashEntry::Type oldType = entry.getType();
         int state = transition[type][oldType];
//         if (debugFlag) {
//            cout << left << setw(27) << setfill(' ')  << "SlipRegister::fsm"
//                 << "del("   << SlipHashEntry::toString(type)
//                 << ", "     << SlipHashEntry::toString(oldType)
//                 << ") -> <" << SlipHashEntry::toString((SlipHashEntry::Type)state)
//                 << ", "     << sState[action[type][oldType]] << ">" << endl;
//            }
         switch(action[type][oldType]) {
         case nada:                                         // do nothing
            break;
         case post:                                         // register the entry
//            entry.setHash();
            break;
         case error:                                        // whoops
            errorFlag = true;
            DEBUG(inputDebugFlag,
               cout << "SlipRegister::post INTERNAL ERROR: illegal transition" << endl;
            )
            errorFlag = true;
            break;
         }
         entry.setType((SlipHashEntry::Type)state);
      }

      return !errorFlag;
   }; //    bool SlipRegister::fsm(SlipAsciiEntry& entry, SlipHashEntry::Type type)

   /**
    * @brief Copy a named list to the Descriptor List field of a list.
    * <p>A named list is copied to a Descriptor List field of a list If
    *    the named list has not been expanded, then a forward reference
    *    is created.</p>
    * <p>The algorithm is:</p>
    * <ol>
    *    <li>If the destination list has a Descriptor List then the copy request
    *        is ignored and an error indication is returned.</li>
    *    <li>If the source list is either found in the hash table or inserted
    *        into the hash table then:</li>
    *    <ul>
    *       <li>Insert a reference to the destination Descriptor List list
    *           header into the source named list. When the named list is
    *           resolved, the forward reference is resolved.</li>
    *       <li>If the destination is an named list, insert it into the hash
    *           table.</li>
    *    </ul>
    * </ol>
    * @param[in] from (<i>SlipDescription*</i>) source for copy
    * @param[in] to (<i>SlipDescription*</i>) destination of copy
    * @return <b>true></b> successful copy
    */
   bool SlipRegister::copyDList(SlipDescription* from, SlipDescription* to) {
      bool retval = false;
      const string* fromName = from->getName();
      SlipHeader* head = (SlipHeader*)(to->getPtr());
      if ((head != NULL) && (!(head->isDList())) ) {
         if (fromName != NULL) {
            SlipHash::ReturnTuple tuple = insert(*fromName);
            if ((tuple.condition == SlipHash::ReturnTuple::FOUND) ||
                (tuple.condition == SlipHash::ReturnTuple::INSERTED)) {
               retval = (tuple.entry)->copyDList(to);
               if (!(to->getType() == SlipDescription::ANONYMOUS)) {
                  SlipHash::ReturnTuple tuple = insert(*(to->getName()));
               }
            }
         } else cout << endl;
      } else cout << endl;

      return retval;
   }; // bool SlipRegister::copyDList(const SlipDescription* from, SlipDescription* to)

   /**
    * @brief Insert a named list into the hash table.
    * <p>This is only used during input processing.</p>
    * <p>This is a reference to a named list not a defintion of a
    *    named list. If the name does not exist then insert the name and
    *    insert a placeholder for the list. If the name does exist or
    *    the name can not be inserted, then deleted the list placeholder.</p>
    * <p>If a placeholder is inserted then all future references will insert
    *    the placeholder into a list as a <i>SlipSublist</i> object. When
    *    the named list is defined, the definition will be copied into the
    *    placeholder.</p>
    * <p><b>ERRORS</b></p>
    * <ol>
    *    <li><b>E4001</b> The name already exists in the hash
    *        table. Duplicate names are illegal.</li>
    *    <li><b>E4003</b> Hash table is unable to allocate memory.</li>
    * </ol>
    * @sa SlipRegister::post(const string& name)
    * @sa SlipRegister::insert(const string& name)
    * @param[in] name (string&) name of named list
    * @return tuple returned as a result on the insert
    */
   SlipHash::ReturnTuple SlipRegister::insert(const string& name) {
      SlipAsciiEntry entry(name, new SlipHeader(), SlipHashEntry::REFERENCED, debugFlag);
      SlipHash::ReturnTuple tuple = hashTable->insert(entry);
      if (tuple.condition != SlipHash::ReturnTuple::INSERTED) {
         ((SlipHeader*)entry.getBinaryValue())->deleteList();
         if (tuple.condition == SlipHash::ReturnTuple::FOUND) {
            if ( (!fsm(*tuple.entry, SlipHashEntry::REFERENCED)) ) {
               tuple.condition = SlipHash::ReturnTuple::ILLEGAL;
               postError(__FILE__, __LINE__, SlipErr::E4001, "SlipRegister::insert(" + name + ")", "", "");
            }
         } else {
            tuple.condition = SlipHash::ReturnTuple::ILLEGAL;
            postError(__FILE__, __LINE__, SlipErr::E4003, "SlipRegister::insert(" + name + ")", "", "");
         }
      }
      return tuple;
   }; // SlipHash::ReturnTuple SlipRegister::insert(string& name)

  /**
   * @brief Return a pointer to the User Data parse function.
   * <p>The hash table is searched for the input name. If the name is
   *    found and the item is a User Data parse function then a pointer to
   *    the parse function is returned, otherwise <b>null</b> is returned.</p>
   * <p>hashTable is used as the hash table..</p>
   * @note the <b>null</b> return can either be because the input name
   *       has not been found in the hash table or the input name has
   *       been found but does not refer to a User Data entry.
   * <p><b>ERRORS</b></p>
   * <ol>
   *    <li><b>E4002</b> Illegal type. Must be a User Data object.</li>
   * </ol>
   * @param[in] name (string) search string
   * @return (Parse) pointer to the parse function or <b>null</b>
   */
   Parse  SlipRegister::getParse(const string& name) const {
      Parse retval = NULL;
      SlipAsciiEntry entry(name, SlipHashEntry::REFERENCED, debugFlag);
      SlipHash::ReturnTuple tuple = hashTable->search(entry);
      if (tuple.condition == SlipHash::ReturnTuple::FOUND) {
         if (fsm(*tuple.entry, SlipHashEntry::USERDATA))
            retval = (Parse)(tuple.entry)->getBinaryValue();
      }
      DEBUG(inputDebugFlag,
      cout << left << setw(27) << setfill(' ') << "SlipRegister::getParse"
           << name << " 0x" << setw(sizeof(void*)) << setfill('0')
           << hex << (void*)retval << endl;)
      return retval;
   }; // Parse SlipRegister::getParse(const string name)

  /**
   *  @brief Return a pointer to the sublist list header.
   *  <p>Search the hash table using the name as a key. If the name is
   *     found and is for a named list then return a pointer to the header,
   *     otherwise return <b>null</b>.</p>
   * @param[in] name (string) search string
   * @return (SlipHeader*) pointer to a list header or <b>null</b>
   */
   SlipHeader* SlipRegister::getSublist(const string& name) const {
      SlipHeader* retval = NULL;
      SlipAsciiEntry entry(name, SlipHashEntry::REFERENCED, debugFlag);
      SlipHash::ReturnTuple tuple = hashTable->search(entry);
      if (tuple.condition == SlipHash::ReturnTuple::FOUND) {
         if (fsm(*tuple.entry, SlipHashEntry::REFERENCED))
            retval = (SlipHeader*)(tuple.entry)->getBinaryValue();
      }
      DEBUG(inputDebugFlag, {
         cout << left << setw(36) << setfill(' ') << setfill(' ') << "SlipRegister::getSublist"
                      << right << setw(10) << name << ' ';
                         if (retval) cout << retval->writeToString() << endl;
                         else        cout << "NULL" << endl;})
      return retval;
   }; // SlipHeader* SlipRegister::getSublist(const string name)

  /**
   * @brief Return the stored name for a list using a list pointer as a key.
   * <p>Return the ascii value for a list entry into the hash table.
   *    If the header address is found and the item is a sublist list
   *    header then a the list name is returned, otherwise <b>""</b>
   *    is returned.</p>
   * @param[in] head (SlipHeader&) search key
   * @return (Parse) pointer to the parse function or <b>null</b>
   */
   string SlipRegister::getSublist(SlipHeader& head) const {
      string name("");
      SlipBinaryEntry entry(&head);
      SlipHash::ReturnTuple tuple = hashTable->search(entry);
      if (tuple.condition == SlipHash::ReturnTuple::FOUND) {
         if (fsm(*tuple.entry, SlipHashEntry::REFERENCED))
            name = (tuple.entry)->getAsciiValue();
      }
      DEBUG(inputDebugFlag,
         cout << left << setw(36) << setfill(' ') << "SlipRegister::getSublist"
              << right << setw(10) << name << " "
              << head.writeToString() << endl;)
      return name;                            // returning a reference to an local variable
   }; // bool SlipRegister::getSublist(SlipHeader& head)

   /**
    * @brief Register User Data.
    * <p>The User Data name and parser method are extracted from the
    *    <i>SlipDatum</i> object containing the User Data pointer,
    *    and the name and object are inserted into the hash table.</p>
    * <p>If the post is successful then <b>true</b> is returned. If the
    *    post is unsuccessful <b>false</b> is returned.</p>
    * <p>If the name does not follow naming conventions it will be inserted
    *    into the hash table but it will not be possible to use the name in
    *    the parser. The effect is that the name will be registered but the
    *    User Data parser function will not be retrievable.</p>
    * <p><b>ERRORS</b></p>
    * <ol>
    *    <li><b>E4001</b> The name already exists in the hash
    *        table. Duplicate names are illegal.</li>
    *    <li><b>E4002</b> Illegal type. Must be a User Data object.</li>
    *    <li><b>E4003</b> Hash table is unable to allocate memory.</li>
    *    <li><b>E4004</b> User Data parse method can not be null.</li>
    * </ol>
    * @param[in] userData (SlipDatum&) reference to a User Data object
    * @return (bool) <b>true</b> post successful <b>false</b> otherwise
    */
   bool SlipRegister::post(const SlipDatum& userData) {
      bool retval = false;
      if (!userData.isPtr()) {
         postError(__FILE__, __LINE__, SlipErr::E4002, "SlipRegister::post", "", "", userData);
      } else {
         Parse parser = const_cast<SlipDatum&>(userData).getParse();
         if (parser == NULL) {
            postError(__FILE__, __LINE__, SlipErr::E4004, "SlipRegister::post", "", "", userData);
         } else {
            SlipAsciiEntry entry(userData, parser, SlipHashEntry::USERDATA, debugFlag);
            SlipHash::ReturnTuple tuple = hashTable->insert(entry);
            retval = true;
            if (tuple.condition != SlipHash::ReturnTuple::INSERTED) {
               if (tuple.condition == SlipHash::ReturnTuple::FOUND) {
                  retval = false;
                  postError(__FILE__, __LINE__, SlipErr::E4001, "SlipRegister::post", "", "", userData);
               } else if (tuple.condition == SlipHash::ReturnTuple::FULL) {
                  retval = false;
                  postError(__FILE__, __LINE__, SlipErr::E4003, "SlipRegister::post", "", "", userData);
               }
            }
         }
      }
      DEBUG(inputDebugFlag,
         cout << left << setw(27) << setfill(' ') << "SlipRegister::post"
              << userData.getName() << userData.toString() << " " << endl);
      return retval;
   }; // bool SlipRegister::post(const SlipDatum& userData)

   /**
    * @brief Register a named list and its definition.
    * <p>This methods is only used during output (write) of a list.</p>
    * <p>Attempt to insert a pointer to a list into the hash table. If
    *    the store is successful then the search key will be the address
    *    of the list and the search value will be the name of the list,
    *    &lt;address, name&gt;. Lack of success means that there is a
    *    duplicate entry for the same key in the hash table or that the
    *    heap memory is exhausted.</p>
    * <p><b>ERRORS</b></p>
    * <ol>
    *    <li><b>E4001</b> The name already exists in the hash
    *        table. Duplicate names are illegal.</li>
    *    <li><b>E4002</b> Illegal type. Must be a User Data object.</li>
    *    <li><b>E4003</b> Hash table is unable to allocate memory.</li>
    * </ol>
    * @param head (SlipHeader&) reference of list to be inserted
    * @param name (stringO name of the list in ascii
    * @return <b>true</b> list successfully stored
    */
   bool SlipRegister::post(SlipHeader& head, string& name) {
      bool retval = false;
      if (head.isPtr() || head.isString()) {
         postError(__FILE__, __LINE__, SlipErr::E4002, "SlipRegister::post", "", "", head);
      } else {                       // attempt to insert a new entry
         retval = true;
         SlipBinaryEntry entry(name, &head);
         entry.setType(SlipHashEntry::R_AND_D);
         SlipHash::ReturnTuple tuple = hashTable->insert(entry);
         if (tuple.condition != SlipHash::ReturnTuple::INSERTED) {
            if (tuple.condition == SlipHash::ReturnTuple::FOUND) {
               if ((tuple.entry)->getType() == SlipHashEntry::USERDATA) {
                  retval = false;
                  postError(__FILE__, __LINE__, SlipErr::E4003, "SlipRegister::post", "", "", head);
               } else {
                  retval = false;
                  postError(__FILE__, __LINE__, SlipErr::E4001, "SlipRegister::post", "", "", head);
               }
            }
         }
      }
      DEBUG(inputDebugFlag,
         cout << left  << setw(45) << setfill(' ') << "SlipRegister::post"
              << right << setw(10) << name << head.writeToString() << endl;)
      return retval;
   }; // bool SlipRegister::post(const string name, SlipHeader& head)

   /**
    * @brief Register a forward reference to a sublist.
    * <p>Only used during input processing.</p>
    * <p>The list input file contains forward references to a named list.
    *    Register the name. If the hash table already has an entry for
    *    this name, then the forward reference is ignored.</p>
    * <p>If the post is successful then <b>true</b> is returned. If the
    *    post is unsuccessful <b>false</b> is returned.</p>
    * <p>If the name does not follow naming conventions it will be registered
    *    in the hash table but it will not be possible to use the name in
    *    the parser. The effect is that the name will be registered but user
    *    data will not be recovered from the input list.</p>
    * @param[in] name (string) name of sublist
    */
  bool SlipRegister::post(const string& name) {
      bool retval = true;
      SlipHash::ReturnTuple tuple = insert(name);
      retval = (tuple.condition != SlipHash::ReturnTuple::ILLEGAL);
      DEBUG(inputDebugFlag,
         cout << left  << setw(42) << setfill(' ') << "SlipRegister::post" << tuple.toString() << endl;)
      return retval;
   }; // bool SlipRegister::post(const string name)

   /**
    * @brief Registers the named list and its definition.
    * <p>Used only during input processing.</p>
    * <p>If the named list has not been referenced, then this is the first
    *    posting. The list registered becomes the list names value. Searches
    *    for this name  will return the recorded list.</p>
    * <p>If a forward reference exists then the definition is copied into
    *    the list associated with the forward reference. All existing forward
    *    references use the associated (empty) list as a sublist. Copying
    *    the definition replaces a <b>null</b> sublist with its value in all
    *    references.</p>
    * <p>Subsequent attempts to redefine the list will cause an error.
    *    An existing definition can not be overwritten. Either you
    *    reference the list many times and define the list once, or
    *    you use someone else's software.</p>
    * <p>The list state will transition to R_AND_D if the list has
    *    previously been defined, or DEFINED if this is the first
    *    time the list has been accessed.</p>
    * <p><b>ERRORS</b></p>
    * <ol>
    *    <li><b>E4001</b> The name already exists in the hash
    *        table. Duplicate names are illegal.</li>
    *    <li><b>E4002</b> Illegal type. Must be a User Data object.</li>
    *    <li><b>E4003</b> Hash table is unable to allocate memory.</li>
    * </ol>
    * @param[in] definition (lipDescription*) description of named list list
    * @return (bool) <b>true</b> post successful <b>false</b> otherwise
    */
   bool SlipRegister::post(SlipDescription* definition) {
      DEBUG(inputDebugFlag,
      cout << left << setw(27) << setfill(' ') << "SlipRegister::post" << definition->toString() << endl;)

      bool retval = true;
      SlipHeader* head = (SlipHeader*)(definition->getPtr());
      if (!head->isHeader()) {
         retval = false;
         postError(__FILE__, __LINE__, SlipErr::E4002, "SlipRegister::post", "", "", *head);
      } else {                       // attempt to insert a new entry
         SlipAsciiEntry entry(definition, SlipHashEntry::DEFINED, debugFlag);
         SlipHash::ReturnTuple tuple = hashTable->insert(entry);
         if (tuple.condition == SlipHash::ReturnTuple::FOUND) {
            if (fsm(*tuple.entry, SlipHashEntry::DEFINED)) {
               DEBUG(inputDebugFlag,
               cout << left << setw(27) << setfill(' ') << "SlipRegister::post"
                    << right << setw(10) << *definition->getName() << setw(9) << ' '
                    << head->writeToString() << endl;)
               tuple.entry->putPtr(definition->getPtr());
               tuple.entry->resolveForwardReferences(this, definition);
            } else if ((tuple.entry)->getType() == SlipHashEntry::USERDATA) {
               retval = false;
               postError(__FILE__, __LINE__, SlipErr::E4001, "SlipRegister::post", "", "", *head);
            } else {
               retval = false;
               postError(__FILE__, __LINE__, SlipErr::E4015, "SlipRegister::post", "", "", *head);
            }
         } else if (tuple.condition == SlipHash::ReturnTuple::INSERTED) {
            DEBUG(inputDebugFlag,
            cout << left  << setw(36) << setfill(' ') << "SlipRegister::post"
                 << right << setw(10) << *definition->getName()  << ' ' << head->writeToString() << endl;)
            tuple.entry->resolveForwardReferences(this, definition);
         } else
            retval = false;
      }
      DEBUG(inputDebugFlag,
         cout << left << setw(27) << setfill(' ') << "SlipRegister::post"  << definition->toString() << endl;)
      return retval;
   }; // bool SlipRegister::post(SlipDescription* definition)

   /**
    * @brief Search the Hash Table using the input ASCII key.
    * <p>The input key is hashed until either an entry with the same key is
    *    found or a NULL entry is found.</p>
    * @param[in] name (string&) hash key
    * @return tuple containing results of search
    */
   SlipHash::ReturnTuple SlipRegister::search(const string& name) const {
      SlipAsciiEntry entry(name,  SlipHashEntry::REFERENCED, debugFlag);
      return hashTable->search(entry);
   }; // int SlipRegister::search(const string& name) const

   /**
    * @brief Search the Hash Table using the input binary key.
    * <p>The input key is hashed until either an entry with the same key is
    *    found or a NULL entry is found.</p>
    * @param[in] head (SlipHeader&) hash key
    * @return <b>true</b> sublist found
    */
   bool SlipRegister::search(SlipHeader& head) const {
      SlipBinaryEntry entry(&head);
      bool retval = (hashTable->search(entry).condition == SlipHash::ReturnTuple::FOUND);
      DEBUG(inputDebugFlag, cout << sFlag[retval] << "search(" << head.toString() << ")" << endl);
      return retval;
   }; // SlipRegister::search(SlipHeader& head) const

   /**
    * @brief Turn debugging off.
    */
   void SlipRegister::setDebugOFF(int debugFlag) {
      inputDebugFlag = (bool)(debugFlag & SlipRead::INPUT);
      hashDebugFlag  = (bool)(debugFlag & SlipRead::HASH);
      hashTable->setDebugOFF(debugFlag);
      this->debugFlag &= ~debugFlag;
   }; // void SlipRegister::setDebugOFF(

   /**
    * @brief Turn debugging on.
    */
   void SlipRegister::setDebugON(int debugFlag) {
      this->debugFlag = debugFlag;
      inputDebugFlag = (bool)(debugFlag & SlipRead::INPUT);
      hashDebugFlag  = (bool)(debugFlag & SlipRead::HASH);
      hashTable->setDebugON(debugFlag);
      this->debugFlag |= debugFlag;
   }; // void SlipRegister::setDebugON(

   /**
    * @brief Return the number of non-null entries in the hash table.
    * @return number  of non-null entries in the hash table
    */
   int SlipRegister::size() {
      return hashTable->size();
   }; // int SlipRegister::size()

   /**
    * @brief Output all non-NULL hash table entries.
    */
   void SlipRegister::write() const {
      hashTable->write();
   }; // SlipRegister::write() const

   /**
    * @brief Output the entire hash table.
    */
   void SlipRegister::writeFull() const {
      hashTable->writeFull();
   }; // void SlipRegister::writeFull() const


} // namespace slip
