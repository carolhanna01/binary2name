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
 * @file    SlipErr.cpp
 * @author  A. Schwarz
 * @date    June 04, 2012
 *
 */
# include <string>
# include <sstream>
# include <iostream>
# include "SlipDatum.h"
# include "SlipErr.h"
# include "SlipException.h"

using namespace std;

namespace slip {
   
/**
 * @fn slip::defaultCallback
 * 
 * @brief Default call back function.
 * 
 * <p>The callback default returns <b>true</b> forcing slipException to be thrown for
 *    any diagnostic message.</p>
 * 
 * @param[in] filename (string) filename where error detected
 * @param[in] lineno (int) line number where error detected
 * @param[in] err (SlipErr::err) error object
 * @param[in] message (string) frmatted diagnostic message
 * @return <b>true</b>
 */
bool defaultCallback(string filename, int lineno, SlipErr::Error err, string message) { 
            cout << message << flush;
            return true; 
}; // bool defaultCallback(string filename, int lineno, SlipErr::Error err, string message)

/**
 * @var slip::callBack
 * 
 * @brief Container for the user call back function.
 * 
 * <p>Initially the container is defaulted to the default callback function. The
 *    default callback function areturns <b>true.</b>. Each time that the user
 *    specifies a new application callback the pointer is overwritten. There is
 *    no retention of previous values.</p>
 */   
static errorCallback callBack = defaultCallback;         //!< pointer to callbace function

/**
 * @var slip::SlipErr::slipMessage
 * 
 * @brief array of objects containing diagnostic messages.
 * 
 * <p>All diagnostic error objects are in this array. SLIP issues messages 
 *    using a reference (enum eError) into this array. Output message 
 *    composition uses the stored text message.</p>
 */
SlipErr::Error SlipErr::slipMessage[SlipErr::ELAST] = {
      SlipErr::Error(SlipErr::E0100, eException, "E0100", "Feature Not Implemented ")
    , SlipErr::Error(SlipErr::E1001, eException, "E1001", "Invalid unary '+' Operation for. ")
    , SlipErr::Error(SlipErr::E1002, eException, "E1002", "Invalid unary '-' Operation for ")
    , SlipErr::Error(SlipErr::E1003, eException, "E1003", "Invalid '++' Operation for. ")
    , SlipErr::Error(SlipErr::E1004, eException, "E1004", "Invalid '--' Operation for.")
    , SlipErr::Error(SlipErr::E1005, eException, "E1005", "Invalid '+' Operation for ")
    , SlipErr::Error(SlipErr::E1006, eException, "E1006", "Invalid '-' Operation for ")
    , SlipErr::Error(SlipErr::E1007, eException, "E1007", "Invalid '*' Operation for ")
    , SlipErr::Error(SlipErr::E1008, eException, "E1008", "Invalid '/' Operation for ")
    , SlipErr::Error(SlipErr::E1009, eException, "E1009", "Invalid '+=' Operation for ")
    , SlipErr::Error(SlipErr::E1010, eException, "E1010", "Invalid '-=' Operation for ")
    , SlipErr::Error(SlipErr::E1011, eException, "E1011", "Invalid '*=' Operation for ")
    , SlipErr::Error(SlipErr::E1012, eException, "E1012", "Invalid '/=' Operation for ")
    , SlipErr::Error(SlipErr::E1013, eException, "E1013", "Invalid '%=' Operation for ")
    , SlipErr::Error(SlipErr::E1014, eException, "E1014", "Invalid '<<=' Operation for ")
    , SlipErr::Error(SlipErr::E1015, eException, "E1015", "Invalid '>>=' Operation for ")
    , SlipErr::Error(SlipErr::E1016, eException, "E1016", "Invalid '&=' Operation for ")
    , SlipErr::Error(SlipErr::E1017, eException, "E1017", "Invalid '^=' Operation for ")
    , SlipErr::Error(SlipErr::E1018, eException, "E1018", "Invalid '= ' Operation for ")
    , SlipErr::Error(SlipErr::E1019, eException, "E1019", "Invalid '|=' Operation for ")
    , SlipErr::Error(SlipErr::E1020, eException, "E1020", "Invalid '<<' Operation for ")
    , SlipErr::Error(SlipErr::E1021, eException, "E1021", "Invalid '>>' Operation for ")
    , SlipErr::Error(SlipErr::E1022, eException, "E1022", "Invalid '^' Operation for ")
    , SlipErr::Error(SlipErr::E1023, eException, "E1023", "Invalid '|' Operation for ")
    , SlipErr::Error(SlipErr::E1024, eException, "E1024", "Invalid '&' Operation for ")
    , SlipErr::Error(SlipErr::E1025, eException, "E1025", "Invalid '!' Operation for ")
    , SlipErr::Error(SlipErr::E1026, eException, "E1026", "Invalid '%' Operation for ")
    , SlipErr::Error(SlipErr::E1027, eException, "E1027", "Invalid cast to char from ")
    , SlipErr::Error(SlipErr::E1028, eException, "E1028", "Invalid '!=' Operation for ")
    , SlipErr::Error(SlipErr::E1029, eException, "E1029", "Invalid '<' Operation for ")
    , SlipErr::Error(SlipErr::E1030, eException, "E1030", "Invalid '<=' Operation for ")
    , SlipErr::Error(SlipErr::E1031, eException, "E1031", "Invalid '==' Operation. ")
    , SlipErr::Error(SlipErr::E1032, eException, "E1032", "Invalid '>=' Operation for ")
    , SlipErr::Error(SlipErr::E1033, eException, "E1033", "Invalid '>' Operation for ")
    , SlipErr::Error(SlipErr::E1034, eException, "E1034", "Invalid Operation for Data Cell Type. ")
    , SlipErr::Error(SlipErr::E1035, eException, "E1035", "Cast to bool not legal for  ")
    , SlipErr::Error(SlipErr::E1036, eException, "E1036", "Cast to unsigned char not legal for ")
    , SlipErr::Error(SlipErr::E1037, eException, "E1037", "Cast to char not legal for ")
    , SlipErr::Error(SlipErr::E1038, eException, "E1038", "Cast to unsigned long not legal for ")
    , SlipErr::Error(SlipErr::E1039, eException, "E1039", "Cast to long not legal for ")
    , SlipErr::Error(SlipErr::E1040, eException, "E1040", "Cast to double not legal for ")
    , SlipErr::Error(SlipErr::E1041, eException, "E1041", "Cast to pointer not legal for ")
    , SlipErr::Error(SlipErr::E1042, eException, "E1043", "Cast to string not legal for ")
    , SlipErr::Error(SlipErr::E1043, eException, "E1043", "Invalid unary '~' Operation for ")

    , SlipErr::Error(SlipErr::E2001, eException, "E2001", "Break operation restricted to Reader not allowed for ")
    , SlipErr::Error(SlipErr::E2002, eException, "E2002", "Copy operation restricted to List Copy not allowed for ")
    , SlipErr::Error(SlipErr::E2003, eException, "E2003", "Flush operation restricted to List not allowed for ")
    , SlipErr::Error(SlipErr::E2004, eException, "E2004", "InsLeft operation not allowed for TEMPORARY. ")
    , SlipErr::Error(SlipErr::E2005, eException, "E2005", "InsRight operation not allowed for TEMPORARY. ")
    , SlipErr::Error(SlipErr::E2006, eException, "E2006", "Cell can not be a local temporary. Create with 'new'. ")
    , SlipErr::Error(SlipErr::E2007, eException, "E2007", "Move list to datum right not allowed for TEMPORARY. ")
    , SlipErr::Error(SlipErr::E2008, eException, "E2008", "dequeue of empty list not allowed.  ")
    , SlipErr::Error(SlipErr::E2009, eException, "E2009", "pop of empty list not allowed. ")
    , SlipErr::Error(SlipErr::E2010, eException, "E2010", "Can not use header cell. ")
    , SlipErr::Error(SlipErr::E2011, eException, "E2011", "INTERNAL ERROR: The SlipSublist construct can not be passed a SlipDatum Cell. ")
    , SlipErr::Error(SlipErr::E2012, eException, "E2012", "Can not changeTop/Bot() of an empty list for ")
    , SlipErr::Error(SlipErr::E2013, eException, "E2013", "Cell must be in a list. Cell is not in a list.  ")
    , SlipErr::Error(SlipErr::E2014, eException, "E2014", "Can not replace the contents of a header cell. ")
    , SlipErr::Error(SlipErr::E2015, eException, "E2015", "Header not allowed in replace method. ")
    , SlipErr::Error(SlipErr::E2016, eException, "E2016", "Sublist not allowed. ")
    , SlipErr::Error(SlipErr::E2017, eException, "E2017", "Temporary not allowed in move operation. ")
    , SlipErr::Error(SlipErr::E2018, eException, "E2018", "Source of move operation not in a list. ")
    , SlipErr::Error(SlipErr::E2019, eException, "E2019", "advanceLEL operation restricted to Reader not allowed for ")
    , SlipErr::Error(SlipErr::E2020, eException, "E2020", "advanceLER operation restricted to Reader not allowed for ")
    , SlipErr::Error(SlipErr::E2021, eException, "E2021", "advanceLNL operation restricted to Reader not allowed for ")
    , SlipErr::Error(SlipErr::E2022, eException, "E2022", "advanceLNR operation restricted to Reader not allowed for ")
    , SlipErr::Error(SlipErr::E2023, eException, "E2023", "advanceLWL operation restricted to Reader not allowed for ")
    , SlipErr::Error(SlipErr::E2024, eException, "E2024", "advanceLWR operation restricted to Reader not allowed for ")
    , SlipErr::Error(SlipErr::E2025, eException, "E2025", "advanceSEL operation restricted to Reader not allowed for ")
    , SlipErr::Error(SlipErr::E2026, eException, "E2026", "advanceSER operation restricted to Reader not allowed for ")
    , SlipErr::Error(SlipErr::E2027, eException, "E2027", "advanceSNL operation restricted to Reader not allowed for ")
    , SlipErr::Error(SlipErr::E2028, eException, "E2028", "advanceSNR operation restricted to Reader not allowed for ")
    , SlipErr::Error(SlipErr::E2029, eException, "E2029", "advanceSWL operation restricted to Reader not allowed for ")
    , SlipErr::Error(SlipErr::E2030, eException, "E2030", "advanceSWR operation restricted to Reader not allowed for ")
    , SlipErr::Error(SlipErr::E2031, eException, "E2031", "Slip Reader must be created with a SlipHeader list cell.  ")
    , SlipErr::Error(SlipErr::E2032, eException, "E2032", "Slip Reader can not be on the stack but must be taken from AVLS for ")
    , SlipErr::Error(SlipErr::E2033, eException, "E2033", "INTERNAL ERROR new SlipReader cell from AVLS is marked UNDEFINED for ")
    , SlipErr::Error(SlipErr::E2034, eException, "E2034", "Attempt to delete a SlipSublist cell that does not have a SlipHeader. ")
    , SlipErr::Error(SlipErr::E2035, eException, "E2035", "SlipSublist cell is a TEMPORARY.  ")
    , SlipErr::Error(SlipErr::E2036, eException, "E2036", "Can not replace the value in a TEMPORARY. Use '='.  ")
    , SlipErr::Error(SlipErr::E2037, eException, "E2037", "Cell must not be on a list. Unlink cell from list. ")
    , SlipErr::Error(SlipErr::E2038, eException, "E2038", "Header required. ")
    , SlipErr::Error(SlipErr::E2039, eException, "E2039", "Can not use 'replace' for source cell if it is in a list. Use 'move'. ")
    , SlipErr::Error(SlipErr::E2040, eException, "E2040", "Key of <key, value> pair present in Descriptor List. ")
    , SlipErr::Error(SlipErr::E2041, eException, "E2041", "Header exists. ")


        //-------------------------------------------------------------
        // Execution Errors
        //-------------------------------------------------------------
    , SlipErr::Error(SlipErr::E3001, eException, "E3001", "NULSTL can not split on a Header. Must split on a Cell. ")
    , SlipErr::Error(SlipErr::E3002, eException, "E3002", "NULSTR can not split on a Header. Must split on a Cell. ")
    , SlipErr::Error(SlipErr::E3003, eException, "E3003", "LSSCPY operation restricted to a List not allowed for ")
    , SlipErr::Error(SlipErr::E3004, eException, "E3004", "Unable to return BOT, List empty for ")
    , SlipErr::Error(SlipErr::E3005, eException, "E3005", "Unable to return TOP, List empty for ")
    , SlipErr::Error(SlipErr::E3006, eException, "E3006", "Unable to return POPBOT, List empty for ")
    , SlipErr::Error(SlipErr::E3007, eException, "E3007", "Unable to return POPTOP, List empty for ")
    , SlipErr::Error(SlipErr::E3008, eException, "E3008", "Unable to get `avsl.alloc` SLIP Cells from Heap.")
    , SlipErr::Error(SlipErr::E3009, eException, "E3009", "Unknown Exception.")
    , SlipErr::Error(SlipErr::E3010, eException, "E3010", "Attempt to delete a range of cells where at least one cell is a temp. ")
    , SlipErr::Error(SlipErr::E3011, eException, "E3011", "Illegal to create a Header cell on the stack. Must use new. ")
    , SlipErr::Error(SlipErr::E3012, eException, "E3012", "Invalid to create a Sublist cell on the stack. Must use new. ")
    , SlipErr::Error(SlipErr::E3013, eException, "E3013", "Invalid to create a Datum cell on the stack. Must use new. ")
    , SlipErr::Error(SlipErr::E3014, eException, "E3014", "INTERNAL ERROR: Sublist not initialized correctly. ")
    , SlipErr::Error(SlipErr::E3015, eException, "E3015", "Assignment illegal. Right hand side not a SlipHeader. ")
    , SlipErr::Error(SlipErr::E3016, eException, "E3016", "Can not assign a SlipSublist to a TEMPORARY.  ")
    , SlipErr::Error(SlipErr::E3017, eException, "E3017", "SlipDatum must be assigned to a SlipDatum. ")
    , SlipErr::Error(SlipErr::E3018, eException, "E3018", "Unable to delete a Slip Cell already deleted. ")
    , SlipErr::Error(SlipErr::E3019, eException, "E3019", "Cell has been deleted. Reference to cell is stale. ")
    , SlipErr::Error(SlipErr::E3020, eException, "E3020", "Unknown or illegal input argument. ")
    , SlipErr::Error(SlipErr::E3021, eException, "E3021", "Attempt to return a cell to the AVSL not from the AVSL. ")
    , SlipErr::Error(SlipErr::E3022, eException, "E3022", "Can not insert a non-AVSL cell into a list. ")
    , SlipErr::Error(SlipErr::E3023, eException, "E3023", "SlipPointer object has been deleted. ")
    , SlipErr::Error(SlipErr::E3024, eException, "E3024", "Delete header is illegal. header.deleteList() must be used. ")
    , SlipErr::Error(SlipErr::E3025, eException, "E3025", "Descriptor List has incorrect number of entries. ")
    , SlipErr::Error(SlipErr::E3026, eException, "E3025", "Attempt to use deleted SlipHeader. ")

          //-------------------------------------------------------------
          // Input/Output errors
          //-------------------------------------------------------------
    , SlipErr::Error(SlipErr::E4001, eWarning,   "E4001", "Duplicate name in hash table. ")
    , SlipErr::Error(SlipErr::E4002, eWarning,   "E4002", "Illegal type. Must be a User Data object. ")
    , SlipErr::Error(SlipErr::E4003, eWarning,   "E4003", "Hash table is unable to allocate memory. ")
    , SlipErr::Error(SlipErr::E4004, eWarning,   "E4004", "User Data parse method can not be null. ")
    , SlipErr::Error(SlipErr::E4005, eWarning,   "E4005", "User Data Function not found. getParse returned NULL. ")
    , SlipErr::Error(SlipErr::E4006, eWarning,   "E4006", "Can not expand sublist. Name not found. ")
    , SlipErr::Error(SlipErr::E4007, eWarning,   "E4007", "Unable to open include file. ")
    , SlipErr::Error(SlipErr::E4008, eWarning,   "E4008", "Key is NULL in Descriptor List. ")
    , SlipErr::Error(SlipErr::E4009, eWarning,   "E4009", "Value is NULL in Descriptor List. ")
    , SlipErr::Error(SlipErr::E4010, eWarning,   "E4010", "Missing list in sublist definition. ")
    , SlipErr::Error(SlipErr::E4011, eWarning,   "E4011", "Unable to register Sublist name. ")
    , SlipErr::Error(SlipErr::E4012, eWarning,   "E4012", "List not found for sublist definition. ")
    , SlipErr::Error(SlipErr::E4013, eWarning,   "E4013", "Unable to output string. Too large. ")
    , SlipErr::Error(SlipErr::E4014, eWarning,   "E4014", "Unable to open output list file. ")
    , SlipErr::Error(SlipErr::E4015, eWarning,   "E4015", "Two list definitions found. ")
    , SlipErr::Error(SlipErr::E4016, eWarning,   "E4016", "Can not attach a named list to a named list. ")
    , SlipErr::Error(SlipErr::E4017, eWarning,   "E4017", "Can not attach a anonymous list to a named list. ")
    , SlipErr::Error(SlipErr::E4018, eWarning,   "E4018", "Named List in Descriptor List can not be found. ")
    , SlipErr::Error(SlipErr::E4019, eWarning,   "E4019", "Descriptor list not empty in name list.. ")
    }; //

   /**
    * @fn errorType SlipErr::setErrorState(errorType state, string err)
    * 
    * @brief Sets the diagnostic message state.
    * 
    * <p>Searches the message table for an object with the same name as supplied
    *    by the application. Diagnostic names consist of 'E' followed by 4
    *    decimal digits. The initial character is not case sensitive.</p>
    * <p>If the name is found in the table, then the state is changed to the
    *    value supplied by the application and the previous state is returned.</p>
    * <p>There is no effort to optimize the search or the table organization. It
    *    is expected that the method will be called infrequently enough to make
    *    such optimizations unnecessary. </p>
    * <p>Some interesting notes:</p>
    * <ul>
    *    <li>If the leading 'E' is missing, then one is inserted. That is, if
    *        '1111' is input as a message name where 'E1111' is meant, the internl
    *        format of the name is converted to 'E1111'.</li>
    *    <li>A leading lower case 'e' is converted to an upper case 'E'.</li>
    *    <li>If the message name is not found then eIgnore is returned.</li>
    *    <li>If the new state is not legal then eIgnore is returned.</li>
    * </ul>
    * <p>Legal values for the input state are:</p>
    * | state name | value |                default action               |
    * | :--------- | :---: | :------------------------------------------ |
    * | eIgnore    |   0   | No output, no throw.                        |
    * | eWarning   |   1   | Output a message, no throw.                 |
    * | eException |   2   | Output a message and throw a slipExceptino. |
    * --------------------------------------------------------------------
    * <p>The default action can be changed by an application supplied callback
    *    funciton.</p>
    * 
    * @see lip::SlipErr::slipMessage
    * 
    * @param[in] state (int) new message state
    * @param[in] err (string) diagnostic message name
    * @return previous diagnostic message state.
    */
   errorType SlipErr::setErrorState(errorType state, string err) {
      string errMsg;
      if ((state < eIgnore) || (state > eException)) return eIgnore;
      errMsg =  (err[0] != 'E' || err[0] != 'e')? 'E' + err: err;
      errMsg[0] = 'E';
      for(int i = 0; i < ELAST; i++) {
         if (slipMessage[i].getErrorNumber() == errMsg) {
            errorType tmp = slipMessage[i].getState();
            slipMessage[i].setState(state);
            return tmp;
         }
      }
      return eIgnore;
   }; // errorType SlipErr::setErrorState(errorType state, string err)

   /**
    * @fn slip::void postError(string file, int lineno, SlipErr::eError error, string name, string prefix, string suffix, const SlipCellBase& cell1, const SlipCellBase& cell2)
    * 
    * @brief Post an error message.
    * 
    * <p>An output error message is posted depending on the message state and
    *    the effect of any application callback function</p>
    * <ul type="none">
    *    <li>If the default callback function is used, then:</li>
    *    <ul type="none">
    *       <li>A message is output if the state is eWarning or eException.</li>
    *       <li>A slipException is thrown if the state is eException.</li>
    *       <li>The error is ignored if the state is eIgnore.</li>
    *    </ul>
    *    <li>If a callback function returns <b>true</b> then a slipException
    *        is thrown It is the responsibility of the callback function to
    *        output a diagnostic message.</li>
    *    <li>If a callback function returns <b>false</b> then a slipException
    *        is not thrown It is the responsibility of the callback function to
    *        output a diagnostic message.</li>
    * </ul>
    * <p>The format of the basic diagnostic message is:</p>
    * <br>
    * <pre><tt>
    *  file: line: in function error: prefix message suffix
    *  |     |        |        |      |      |       |
    *  |     |        |        |      |      |       o - message suffix string
    *  |     |        |        |      |      |
    *  |     |        |        |      |      o - base message
    *  |     |        |        |      |
    *  |     |        |        |      o - message prefix string
    *  |     |        |        |
    *  |     |        |        o - error number (message name)
    *  |     |        |
    *  |     |        o - function name where error message found or posted
    *  |     |
    *  |     o - line number in file where message found or posted
    *  |
    *  o - filename where mesage found or posted
    * </tt></pre>
    * <p>Supplemental message information consists of a formatted dump of any
    *    input SlipCells. There can be 0, 1, or 2 cells to be dumped depending
    *    on the postError function being used.</p>
    * 
    * @param[in] file (string) filename where message post is made
    * @param[in] lineno (int) line number in file where message post is made
    * @param[in] error (eError) diagnostic message object
    * @param[in] name (string) method name where error found
    * @param[in] prefix (string) string to precede base message
    * @param[in] suffix (string) string to follow base message
    * @param[in] cell1 (SlipCell) SLIP cell to be formatted into message
    * @param[in] cell2 (SlipCell) SLIP cell to be formatted into message
    */
   void postError( string file                     //!< filename issuing error
                 , int lineno                      //!< line number in the file
                 , SlipErr::eError error           //!< diagnostic message object
                 , string name                     //!< method name generating message
                 , string prefix                   //!< message prefix
                 , string suffix                   //!< messge suffix
                 , const SlipCellBase& cell1       //!< cell to be dumped
                 , const SlipCellBase& cell2) {    //!< cell to be dumped
      SlipErr::Error base = SlipErr::slipMessage[error];
      if (base.isIgnore()) return;
      stringstream message;
      message << file << ':' << lineno << " in " << name << ": "
              << base.getErrorNumber() << ' '
              << prefix << ' ' << base.getMessage() << ' ' << suffix << endl
              << "      " << cell1.dump() << endl
              << "      " << cell2.dump() << endl;
      if (callBack(file, lineno, base, message.str())) 
         throw SlipException(base, message.str(), &cell1, &cell2); 
   }; // void postError(string file, int lineno, SlipErr::eError error, string name, string prefix, string suffix, const SlipCellBase& cell1, const SlipCellBase& cell2)

   /**
    * @fn slip::void postError(string file, int lineno, SlipErr::eError error, string name, string prefix, string suffix, const SlipCellBase& cell1)
    * 
    * @brief Post an error message.
    * 
    * <p>An output error message is posted depending on the message state and
    *    the effect of any application callback function</p>
    * <ul type="none">
    *    <li>If the default callback function is used, then:</li>
    *    <ul type="none">
    *       <li>A message is output if the state is eWarning or eException.</li>
    *       <li>A slipException is thrown if the state is eException.</li>
    *       <li>The error is ignored if the state is eIgnore.</li>
    *    </ul>
    *    <li>If a callback function returns <b>true</b> then a slipException
    *        is thrown It is the responsibility of the callback function to
    *        output a diagnostic message.</li>
    *    <li>If a callback function returns <b>false</b> then a slipException
    *        is not thrown It is the responsibility of the callback function to
    *        output a diagnostic message.</li>
    * </ul>
    * <p>The format of the basic diagnostic message is:</p>
    * <br>
    * <pre><tt>
    *  file: line: in function: error prefix message suffix
    *  |     |        |         |      |      |       |
    *  |     |        |         |      |      |       o - message suffix string
    *  |     |        |         |      |      |
    *  |     |        |         |      |      o - base message
    *  |     |        |         |      |
    *  |     |        |         |      o - message prefix string
    *  |     |        |         |
    *  |     |        |         o - error number (message name)
    *  |     |        |
    *  |     |        o - function name where error message found or posted
    *  |     |
    *  |     o - line number in file where message found or posted
    *  |
    *  o - filename where mesage found or posted
    * </tt></pre>
    * <p>Supplemental message information consists of a formatted dump of any
    *    input SlipCells. There can be 0, 1, or 2 cells to be dumped depending
    *    on the postError function being used.</p>
    * 
    * @param[in] file (string) filename where message post is made
    * @param[in] lineno (int) line number in file where message post is made
    * @param[in] error (eError) diagnostic message object
    * @param[in] name (string) method name where error found
    * @param[in] prefix (string) string to precede base message
    * @param[in] suffix (string) string to follow base message
    * @param[in] cell1 (SlipCell) SLIP cell to be formatted into message
    */
   void postError(string file, int lineno, SlipErr::eError error, string name, string prefix, string suffix, const SlipCellBase& cell1) {
      SlipErr::Error base = SlipErr::slipMessage[error];
      if (base.isIgnore()) return;
      stringstream message;
      message << file << ':' << lineno << " in " << name << ": "
              << base.getErrorNumber() << ' '
              << prefix << ' ' << base.getMessage() << ' ' << suffix << endl
              << "      " << cell1.dump() << endl;
      if (callBack) {
         if (callBack(file, lineno, base, message.str())) 
            throw SlipException(base, message.str(), &cell1, NULL); 
      } else {
         cout << message.str() << flush;
         if (base.isException())
            throw SlipException(base, message.str(), &cell1, NULL); 
      }
   }; // void postError(string file, int lineno, SlipErr::eError error, string name, string prefix, string suffix, const SlipCellBase& cell1)

   /**
    * @fn slip::void postError(string file, int lineno, SlipErr::eError error, string name, string prefix, string suffix)
    * 
    * @brief Post an error message.
    * 
    * <p>An output error message is posted depending on the message state and
    *    the effect of any application callback function</p>
    * <ul type="none">
    *    <li>If the default callback function is used, then:</li>
    *    <ul type="none">
    *       <li>A message is output if the state is eWarning or eException.</li>
    *       <li>A slipException is thrown if the state is eException.</li>
    *       <li>The error is ignored if the state is eIgnore.</li>
    *    </ul>
    *    <li>If a callback function returns <b>true</b> then a slipException
    *        is thrown It is the responsibility of the callback function to
    *        output a diagnostic message.</li>
    *    <li>If a callback function returns <b>false</b> then a slipException
    *        is not thrown It is the responsibility of the callback function to
    *        output a diagnostic message.</li>
    * </ul>
    * <p>The format of the basic diagnostic message is:</p>
    * <br>
    * <pre><tt>
    *  file: line: in function: error prefix message suffix
    *  |     |        |         |      |      |       |
    *  |     |        |         |      |      |       o - message suffix string
    *  |     |        |         |      |      |
    *  |     |        |         |      |      o - base message
    *  |     |        |         |      |
    *  |     |        |         |      o - message prefix string
    *  |     |        |         |
    *  |     |        |         o - error number (message name)
    *  |     |        |
    *  |     |        o - function name where error message found or posted
    *  |     |
    *  |     o - line number in file where message found or posted
    *  |
    *  o - filename where mesage found or posted
    * </tt></pre>
    * <p>Supplemental message information consists of a formatted dump of any
    *    input SlipCells. There can be 0, 1, or 2 cells to be dumped depending
    *    on the postError function being used.</p>
    * 
    * @param[in] file (string) filename where message post is made
    * @param[in] lineno (int) line number in file where message post is made
    * @param[in] error (eError) diagnostic message object
    * @param[in] name (string) method name where error found
    * @param[in] prefix (string) string to precede base message
    * @param[in] suffix (string) string to follow base message
    */
   void postError(string file, int lineno, SlipErr::eError error, string name, string prefix, string suffix) {
      SlipErr::Error base = SlipErr::slipMessage[error];
      if (base.isIgnore()) return;
      stringstream message;
      message << file << ':' << lineno << " in " << name << ": "
              << base.getErrorNumber() << ' '
              << prefix << ' ' << base.getMessage() << ' '<< suffix << endl;
      if (callBack) {
         if (callBack(file, lineno, base, message.str())) 
            throw SlipException(base, message.str(), NULL, NULL);
      } else {
         cout << message.str() << flush;
         if (base.isException())
            throw SlipException(base, message.str(), NULL, NULL);
      }
   }; // void postError(string file, int lineno, SlipErr::eError error, string name, string prefix, string suffix)

   /**
    * @fn slip::void postError(string file, int lineno, SlipErr::eError error, string name)
    * 
    * @brief Post an error message.
    * 
    * <p>An output error message is posted depending on the message state and
    *    the effect of any application callback function</p>
    * <ul type="none">
    *    <li>If the default callback function is used, then:</li>
    *    <ul type="none">
    *       <li>A message is output if the state is eWarning or eException.</li>
    *       <li>A slipException is thrown if the state is eException.</li>
    *       <li>The error is ignored if the state is eIgnore.</li>
    *    </ul>
    *    <li>If a callback function returns <b>true</b> then a slipException
    *        is thrown It is the responsibility of the callback function to
    *        output a diagnostic message.</li>
    *    <li>If a callback function returns <b>false</b> then a slipException
    *        is not thrown It is the responsibility of the callback function to
    *        output a diagnostic message.</li>
    * </ul>
    * <p>The format of the basic diagnostic message is:</p>
    * <br>
    * <pre><tt>
    *  file: line: in function: error message
    *  |     |        |         |     | 
    *  |     |        |         |     |
    *  |     |        |         |     |
    *  |     |        |         |     o - base message
    *  |     |        |         |
    *  |     |        |         o - error number (message name)
    *  |     |        |
    *  |     |        o - function name where error message found or posted
    *  |     |
    *  |     o - line number in file where message found or posted
    *  |
    *  o - filename where mesage found or posted
    * </tt></pre>
    * <p>Supplemental message information consists of a formatted dump of any
    *    input SlipCells. There can be 0, 1, or 2 cells to be dumped depending
    *    on the postError function being used.</p>
    * 
    * @param[in] file (string) filename where message post is made
    * @param[in] lineno (int) line number in file where message post is made
    * @param[in] error (eError) diagnostic message object
    * @param[in] name (string) method name where error found
    * @param[in] prefix (string) string to precede base message
    * @param[in] suffix (string) string to follow base message
    */
   void postError(string file, int lineno, SlipErr::eError error, string name) {
      SlipErr::Error base = SlipErr::slipMessage[error];
      if (base.isIgnore()) return;
      stringstream message;
      message << file << ':' << lineno << " in " << name << ": "
              << base.getErrorNumber() << endl;
      if (callBack) {
         if (callBack(file, lineno, base, message.str())) 
            throw SlipException(base, message.str(), NULL, NULL); 
      } else {
         cout << message.str() << flush;
         if (base.isException())
            throw SlipException(base, message.str(), NULL, NULL);
      }
   }; // void postError(string file, int lineno, SlipErr::eError error, string name)
   
   /**
    * @brief Setup the user callback for diagnostic messages.
    * 
    * <p>The user can provide a callback function for messages. Every time a
    *    diagnostic message is output the user callback function is invoked.
    *    The callback function is responsible for fixup and message annotation.
    *    If the callback function returns <i>true</i> then the executable 
    *    program aborts, otherwise the executable program continues.</p>
    * <p>ONly one user callback function can be active. Each time that this 
    *    function is invoked, the previous callback function is returned
    *    and replaced.</p>
    * 
    * @param callBack (errorCallBack) user diagnostic message handler.
    * @return the value of the current callback function.
    */
   errorCallback userCallBack(errorCallback callBack) {                          // Set user defined error callback function.s
      errorCallback tmp = callBack;
      slip::callBack = callBack;
      return tmp;
   }; // errorCallback userCallBack(errorCallback callBack)


}; // namcespace slip
