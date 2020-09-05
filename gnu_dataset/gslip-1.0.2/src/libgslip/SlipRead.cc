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

/**
 * @file    SlipRead.cpp
 * @author  A. Schwarz
 * @date    January 13, 2014, 2012
 *
 * @brief Read and parse an input list file.
 *
 */

# include <istream>
# include <cstdarg>
# include "lex.Slip.h"
# include "Slip.Bison.hpp"
# include "SlipDatum.h"
# include "SlipHeader.h"
# include "SlipInputMethods.h"
# include "SlipRead.h"
# include "SlipRegister.h"

namespace slip {

#undef DEBUG
#ifdef DEBUG_IO
   #define DEBUG(flag, stmt) if (flag) stmt
#else
   #define DEBUG(flag, stmt)
#endif

   /*********************************************************
    *               Constructors & Destructors              *
   **********************************************************/

   /**
    * @brief Constructor.
    * <p>Creates the register for recording User Data parse methods and
    *    for sublist header references.</p>
    * @param[in] debugFlag (int) debug mask
    */
   SlipRead::SlipRead(int debugFlag) : reg(new SlipRegister(debugFlag))
                                     , debugFlag(debugFlag)
                                     , errorReturn(0)
                                     , inputDebugFlag((bool)(debugFlag & SlipRead::INPUT))
                                     , leakDebugFlag((bool)(debugFlag & SlipRead::LEAK))
                                     , lexerDebugFlag((bool)(debugFlag & SlipRead::LEXER))
                                     , parserDebugFlag((bool)(debugFlag & SlipRead::PARSER)) {
   }; // SlipRead::SlipRead(int debugFlag)

   /**
    * @brief Constructor.
    * <p>Creates the register for recording User Data parse methods and
    *    for sublist header references and populates the register with
    *    a single User Data parse method.</p>
    * @param[in] userData (SlipDatum&) User Data object
    * @param[in] debugFlag (int) debug mask
    */
   SlipRead::SlipRead(SlipDatum& userData, int debugFlag)
                                           : reg(new SlipRegister(debugFlag))
                                           , debugFlag(debugFlag)
                                           , errorReturn(0)
                                           , inputDebugFlag((bool)(debugFlag & SlipRead::INPUT))
                                           , leakDebugFlag((bool)(debugFlag & SlipRead::LEAK))
                                           , lexerDebugFlag((bool)(debugFlag & SlipRead::LEXER))
                                           , parserDebugFlag((bool)(debugFlag & SlipRead::PARSER)) {
      registerUserData(userData);
   }; // SlipRead::SlipRead(SlipDatum& userData, int universalDeubg)

   /**
    * @brief Constructor.
    * <p>Creates the register for recording User Data parse methods and
    *    for sublist header references and populates the register with
    *    a multiple User Data parse methods.</p>
    * @param[in] size (int) number of User Data objects
    * @param[in] userData (SlipDatum* []) list of pointers to User Data objects
    * @param[in] debugFlag (int) debug mask
    */
   SlipRead::SlipRead(const int size, SlipDatum const userData[], int debugFlag)
                                           : reg(new SlipRegister(debugFlag))
                                           , debugFlag(debugFlag)
                                           , errorReturn(0)
                                           , inputDebugFlag((bool)(debugFlag & SlipRead::INPUT))
                                           , leakDebugFlag((bool)(debugFlag & SlipRead::LEAK))
                                           , lexerDebugFlag((bool)(debugFlag & SlipRead::LEXER))
                                           , parserDebugFlag((bool)(debugFlag & SlipRead::PARSER)) {
      registerUserData(size, userData);
   }; // SlipRead::SlipRead(const int size, SlipDatum const userData[], int debugFlag)

   /**
    * @brief Constructor.
    * <p>Creates the register for recording User Data parse methods and
    *    for sublist header references and populates the register with
    *    a multiple User Data parse methods.</p>
    * @param[in] size (int) number of User Data objects
    * @param[in] userData (SlipDatum* []) list of pointers to User Data objects
    * @param[in] debugFlag (int) debug mask
    */
   SlipRead::SlipRead(const int size, SlipDatum  * const userData[], int debugFlag)
                                           : reg(new SlipRegister(debugFlag))
                                           , debugFlag(debugFlag)
                                           , errorReturn(0)
                                           , inputDebugFlag((bool)(debugFlag & SlipRead::INPUT))
                                           , leakDebugFlag((bool)(debugFlag & SlipRead::LEAK))
                                           , lexerDebugFlag((bool)(debugFlag & SlipRead::LEXER))
                                           , parserDebugFlag((bool)(debugFlag & SlipRead::PARSER)) {
      registerUserData(size, userData);
   }; // SlipRead::SlipRead(const int size, SlipDatum  * const userData[], int debugFlag)

   /**
    * @brief Copy constructor.
    * <p>This is not available for use.</p>
    * @param orig (SlipRead&) reference to SlipRead object
    */
   SlipRead::SlipRead(const SlipRead& orig) {
   }; // SlipRead::SlipRead(const SlipRead& orig)

   /**
    * @brief Destructor (deconstructor?).
    * <p>Deletes the register.</p>
    */
   SlipRead::~SlipRead() {
      restoreLexState();
      delete reg;
   }; // SlipRead::~SlipRead()

   /*********************************************************
    *                        Methods                        *
   **********************************************************/

   /**
    * @brief Return stored read status.
    * <p>The reason for read termination is stored in a class variable and
    *    can be returned with this method.</p>
    * <center><table border="0">
    * <tr>
    *    <td><b>'0'</b></td>
    *    <td>Normal termination. List file successfully input.</td>
    * </tr>
    * <tr>
    *    <td><b>'1'</b></td>
    *    <td>Abnoral termination. Error in reading list file.</td>
    * </tr>
    * <tr>
    *    <td><b>'2'</b></td>
    *    <td>Abnormal termination. Could not read list file.</td>
    * </tr>
    * </table></center>
    * @return reason for termination of read
    */
   int SlipRead::getError() {
      return errorReturn;
   }; // SlipRead::getError())

   /**
    * @brief Read the list file.
    * <p>The list file is parsed and the return value saved in a local
    *    variable. If the parse was unsuccessful, an empty list is
    *    returned.</p>
    * @return list (SlipHeader*) pointer.
    */
   SlipHeader& SlipRead::parse() {
      SlipHeader* head;
      SlipInputMethods* methods = new SlipInputMethods(reg, debugFlag);
      parser parser(&head, methods);
      DEBUG(parserDebugFlag, parser.set_debug_level(1);)
      DEBUG(lexerDebugFlag, ::setLexerDebugON();)
      errorReturn = parser.parse();
      errorReturn += methods->getErrorCount();
      if (head != NULL) {
         for(; head->getRefCount() ;) head->deleteList();
      }
//      if (errorReturn != 0) {
//         if (head != NULL) head->deleteList();
//      }
      if (head == NULL) head = new SlipHeader();
      delete methods;
      return *head;
   }; // SlipHeader& SlipRead::parse()

   /**
    * @brief Initialize the parser and return its value.
    * <p>Parser operation is initialized and the list file is opened.
    *    If either operation fails, an empty list is returned and an
    *    error value is saved in a class variable.</p>
    * @param filename (string) file name and path to be opened.
    * @return list pointer
    */
   SlipHeader& SlipRead::read(string filename) {
      if (saveLexState(filename)) {
         return parse();
      } else {
         errorReturn = 2;
         return *new SlipHeader();
      }
   }; // SlipHeader& SlipRead::read()

   /**
    * @brief Register the name and parse method for a User Data class.
    * <p>The User Data name and parse method are retrieved from the
    *    input class instance and stored into the registration class
    *    used by the input list file parser.</p>
    * <p>Registration can fail if space is exhausted for the registration
    *    tables or if an existing entry with the same User Data name has
    *    a different parse function.</p>
    * @param userData (SlipDatum&) User Data class instance
    * @return <b>true</b> registration successful
    */
   bool SlipRead::registerUserData(const SlipDatum& userData) {
      return reg->post(userData);
   }; // bool SlipRead::registerUserData(SlipDatum& userData)

   /**
    * @brief Register an array of pointers to User Data instances.
    * <p>Multiple calls to registerUserData(const SlipDatum&).</p>
    * @param size (int) number of items in the array
    * @param userData (SlipDatum* []) array of pointers
    * @return <b>true</b> all input items were successfully registered
    */
   bool SlipRead::registerUserData(const int size, SlipDatum  * const userData[]) {
      bool flag = true;
      for(int i = 0; i < size; i++)
         flag &= registerUserData(*userData[i]);
      return flag;
   }; // bool SlipRead::registerUserData(int size, SlipDatum* const userData[])


   /**
    * @brief Register an array of User Data instances.
    * <p>Multiple calls to registerUserData(const SlipDatum&).</p>
    * @param size (int) number of items in the array
    * @param userData (SlipDatum []) array of pointers
    * @return <b>true</b> all input items were successfully registered
    */
   bool SlipRead::registerUserData(const int size, SlipDatum const userData[]) {
      bool flag = true;
      for(int i = 0; i < size; i++)
         flag &= registerUserData(userData[i]);
      return flag;
   }; // bool SlipRead::registerUserData(int size, SlipDatum const userData[])

   /**
    * @brief Turn off debugging.
    * <p>Selectively turn debugging off. If no input parameter is specified then all
    *    debug flags are turned off.</p>
    * @param[in] debugFlag debug mask for debug off
    */
   void SlipRead::setDebugOFF(int debugFlag) {
      debugFlag = (debugFlag)? debugFlag: ALL;
      inputDebugFlag =  (bool)(debugFlag & SlipRead::INPUT)? false: inputDebugFlag;
      leakDebugFlag  =  (bool)(debugFlag & SlipRead::LEAK)? false: leakDebugFlag;
      lexerDebugFlag =  (bool)(debugFlag & SlipRead::LEXER)? false: lexerDebugFlag;
      parserDebugFlag = (bool)(debugFlag & SlipRead::PARSER)? false: parserDebugFlag;
      reg->setDebugOFF(debugFlag);
   }; // void SlipRead::setDebugOFF()

   /**
    * @brief Turn on debugging.
    */
   void SlipRead::setDebugON(int debugFlag) {
      this-> debugFlag = (debugFlag)? debugFlag: ALL;
      inputDebugFlag =  (bool)(debugFlag & SlipRead::INPUT);
      leakDebugFlag  =  (bool)(debugFlag & SlipRead::LEAK);
      lexerDebugFlag =  (bool)(debugFlag & SlipRead::LEXER);
      parserDebugFlag = (bool)(debugFlag & SlipRead::PARSER);
      reg->setDebugON(debugFlag);
   }; // void SlipRead::setDebugON()

}; // namespace slip