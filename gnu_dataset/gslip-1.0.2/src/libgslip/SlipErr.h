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
 * @file    SlipErr.h
 * @author  A. Schwarz
 * @date    October 28, 2011
 *
 * @brief Error handling container.
 */

#ifndef SLIPERR_H
#define	SLIPERR_H

# include <string>
# include "SlipCellBase.h"
# include "SlipDef.h"

using namespace std;

namespace slip {

//-----------------------------------------------------------------------------
// Base Class for SLIP errors.
//-----------------------------------------------------------------------------
/**
 * @class slip::SlipErr  SlipErr.h "<slip/SlipErr.h>"
 * 
 * @brief Container class for diagnostic messages.
 * 
 * <p>Diagnostic messages, their enumerated types, and functions for access to
 * and posting of messages are provided. This is a container file. All accesses
 * are provided through functions.</p>
 * <p>Associated with each message is an enumerated type. The messages text is
 * stored into an array, each message in the array is associated with an
 * enumerated name.</p>
 * <p>Messages are in one of three states:</p>
 * <ul>
 *    <li><b>ignore</b> The message is ignored. Diagnostic output is suppressed
 *        and return to the calling routing is immediate. It is the responsibility
 *        of the invoking function to perform runtime fixup.</li>
 *    <li><b>warning</b> A diagnostic message is output and return to the caller
 *        is immediate. It is th responsibility of the caller to perform runtime
 *        fixup.</li>
 *    <li><b>erro</b> A diagnostic message is output and an exception is thrown.</li>
 * </ul>
 * <p>The default state of all messages is <b>exception</b>, output a message and
 *    issue an exception.</p>
 * <p></p>
 * <p>A callback ability is provided applications. The application provided 
 *    callback function will be invoked every time a diagnostic message is issued.
 *    The callback function is responsible for processing the message. It can 
 *    output the message, fixup the error, throw an exception or order the SLIP
 *    processing system diagnostic software to throw an exception. If the 
 *    application attempts to establish multiple callback functions, onlhy the 
 *    last one is retain, ie., only one callback function can be active.</p>
 * <p>The callback prototype is:</p>
 * <br><tt>bool func(string filename, int lineno, SlipErr::Error err, string message)</tt>
 * <p>The error object, given by <tt>SlipErr::Error err</tt> provides access to
 *    the diagnostic message object properties. These properties ar3e:<p>
 * <ul>
 *    <li><b>state</b> The message 'state'. How to treat the message.</li>
 *    <li><b>index</b> the enumerated value of the message. the position of the
 *        message in the table of messages.</li>
 *    <li><b>message</b> The 'raw' message text. Message test without the runtime
 *        annotations of file name, line number, amd dump of cell contents.</li>
 *    <li><b>error number</b> The string representing the message 'name'.</li>
 * </ul>
 * <p>There is a capability for the application to alter the state of the 
 *    diagnostic message. However, changing the state from error to warning or 
 *    ignore may make the runtime system unstable. The application is responsible
 *    for fixup. The underlying system fixup (primarily) either returns a benign
 *    value or does nothing. In either case, the system is assumed to be unstable.</p>
 * 
 * <b>Theory Of Operation</b>
 * <p>There are three parts to the class:</p>
 * <ul>
 *    <li><<b>An enumeration</b> One entry for each diagnostic message.</li>
 *    <li><<b>Array of structures</b> Static array for each diagnostic message.</li>
 *    <li><<b>Set state method</b> To change the state of any message.</li>
 * </ul>
 * <p>In operation the enumeration and array are invariant and the set state
 *    method allows modification of behavior of any one diagnostic message. The SLIP
 *    system passes the enumerated value of a message to a post function. The post
 *    function uses the enumeration to select a message object and compose a message.
 *    the application is not expected to have an 'interest' is generating a SLIP
 *    message and no specific facility is provided for the application - although
 *    the application can use the available functions.</p>
 * <p>Changing a message state is done through a circuitous route. Direct access
 *    to SlipErr is hidden and instead an interface function is provided in slip.h.
 *    This function calls the file global function which in its turn calls the class
 *    method. The application is not required to include the SlipErr header file. </p>
 * 
 * @see slip.h
 * 
 */

   class SlipErr {
   public:
      
    //---------------------------------------------------------------------
    // Slip local Diagnostic Messages
    //---------------------------------------------------------------------
    //                                                    base  Message text
      enum eError    { E0100                       //!<  Feature Not Implemented
                     , E1001                       //!<  Invalid unary '+' Operation for
                     , E1002                       //!<  Invalid unary '-' Operation for
                     , E1003                       //!<  Invalid '++' Operation for
                     , E1004                       //!<  Invalid '--' Operation for
                     , E1005                       //!<  Invalid '+' Operation for
                     , E1006                       //!<  Invalid '-' Operation for
                     , E1007                       //!<  Invalid '*' Operation for
                     , E1008                       //!<  Invalid '/' Operation for
                     , E1009                       //!<  Invalid '+=' Operation for
                     , E1010                       //!<  Invalid '-=' Operation for
                     , E1011                       //!<  Invalid '*=' Operation for
                     , E1012                       //!<  Invalid '/=' Operation for
                     , E1013                       //!<  Invalid '%=' Operation for
                     , E1014                       //!<  Invalid '<<=' Operation for
                     , E1015                       //!<  Invalid '>>=' Operation for
                     , E1016                       //!<  Invalid '&=' Operation for
                     , E1017                       //!<  Invalid '^=' Operation for
                     , E1018                       //!<  Invalid '= ' Operation for
                     , E1019                       //!<  Invalid '|=' Operation for
                     , E1020                       //!<  Invalid '<<' Operation for
                     , E1021                       //!<  Invalid '>>' Operation for
                     , E1022                       //!<  Invalid '^' Operation for
                     , E1023                       //!<  Invalid '|' Operation for
                     , E1024                       //!<  Invalid '&' Operation for
                     , E1025                       //!<  Invalid '!' Operation for
                     , E1026                       //!<  Invalid '%' Operation for
                     , E1027                       //!<  Invalid cast to char from
                     , E1028                       //!<  Invalid '!=' Operation for
                     , E1029                       //!<  Invalid '<' Operation for
                     , E1030                       //!<  Invalid '<=' Operation for
                     , E1031                       //!<  Invalid '==' Operation for
                     , E1032                       //!<  Invalid '>=' Operation for
                     , E1033                       //!<  Invalid '>' Operation for
                     , E1034                       //!<  Invalid Operation for Data Cell Type
                     , E1035                       //!<  Cast to bool not legal for 
                     , E1036                       //!<  Cast to unsigned char not legal for 
                     , E1037                       //!<  Cast to char not legal for 
                     , E1038                       //!<  Cast to unsigned long not legal for 
                     , E1039                       //!<  Cast to long not legal for 
                     , E1040                       //!<  Cast to double not legal for 
                     , E1041                       //!<  Cast to pointer not legal for 
                     , E1042                       //!<  Cast to string not legal for 
                     , E1043                       //!<  Invalid unary '~' Operation for

                     , E2001                       //!<  Break operation restricted to Reader not
                     , E2002                       //!<  Copy operation restricted to List Copy not
                     , E2003                       //!<  Flush operation restricted to List not
                     , E2004                       //!<  InsLeft operation not allowed for TEMPORARY
                     , E2005                       //!<  InsRight operation not allowed for TEMPORARY
                     , E2006                       //!<  Cell can not be a local temporary. Create with 'new'
                     , E2007                       //!<  Move list to datum right not allowed for TEMPORARY
                     , E2008                       //!<  dequeue of empty list not allowed
                     , E2009                       //!<  pop of empty list not allowed
                     , E2010                       //!<  Can not use header cell 
                     , E2011                       //!<  SlipSublist constructor must be passed a SlipHeader cell
                     , E2012                       //!<  Can not changeTop/Bot() of an empty list
                     , E2013                       //!<  Cell must be in a list. Cell is not in a list. 
                     , E2014                       //!<  Can not replace the contents of a header cell
                     , E2015                       //!<  Header not allowed in replace method
                     , E2016                       //!<  Sublist not allowed 
                     , E2017                       //!<  Temporary not allowed in move operation
                     , E2018                       //!<  Source of move operation not in a list

          //-------------------------------------------------------------
          // Reader Functions
          //-------------------------------------------------------------
                     , E2019                       //!<  advanceLEL operation restricted to Reader not allowed for
                     , E2020                       //!<  advanceLER operation restricted to Reader not allowed for
                     , E2021                       //!<  advanceLNL operation restricted to Reader not allowed for
                     , E2022                       //!<  advanceLNR operation restricted to Reader not allowed for
                     , E2023                       //!<  advanceLWL operation restricted to Reader not allowed for
                     , E2024                       //!<  advanceLWR operation restricted to Reader not allowed for
                     , E2025                       //!<  advanceSEL operation restricted to Reader not allowed for
                     , E2026                       //!<  advanceSER operation restricted to Reader not allowed for
                     , E2027                       //!<  advanceSNL operation restricted to Reader not allowed for
                     , E2028                       //!<  advanceSNR operation restricted to Reader not allowed for
                     , E2029                       //!<  advanceSWL operation restricted to Reader not allowed for
                     , E2030                       //!<  advanceSWR operation restricted to Reader not allowed for
                     , E2031                       //!<  Slip Reader must be created with a SlipHeader list cell
                     , E2032                       //!<  Slip Reader can not be on the stack but must be taken from AVLS for
                     , E2033                       //!<  INTERNAL ERROR new SlipReader cell from AVLS is marked UNDEFINED for
                     , E2034                       //!<  Attempt to delete a SlipSublist cell that does not have a SlipHeader
                     , E2035                       //!<  SlipSublist cell is a TEMPORARY 
                     , E2036                       //!<  Can not replace the value in a TEMPORARY. Use '='.  
                     , E2037                       //!<  Cell must not be on a list. Unlink cell from list.  
                     , E2038                       //!<  Header required  
                     , E2039                       //!<  Can not use 'replace' for source cell if it is in a list. Use 'move'. 
                     , E2040                       //!<  Key of <key, value> pair present in Descriptor List
                     , E2041                       //!<  Header exists

          //-------------------------------------------------------------
          // Execution Errors
          //-------------------------------------------------------------
                     , E3001                       //!<  NULSTL can not split on a Header. Must split on a Cell.
                     , E3002                       //!<  NULSTR can not split on a Header. Must split on a Cell.
                     , E3003                       //!<  LSSCPY operation restricted to a List not allowed by Slip Type
                     , E3004                       //!<  Unable to return BOT, List empty for Slip Type
                     , E3005                       //!<  Unable to return TOP, List empty for Slip Type
                     , E3006                       //!<  Unable to return POPBOT, List empty for Slip Type
                     , E3007                       //!<  Unable to return POPTOP, List empty for Slip Type
                     , E3008                       //!<  Unable to get `avsl.alloc` SLIP Cells from Heap
                     , E3009                       //!<  Unknown exception
                     , E3010                       //!<  Attempt to recover a range of cells where at least one cell is a temp
                     , E3011                       //!<  Illegal to create a Header cell on the stack. Must use new
                     , E3012                       //!<  Invalid to create a SUBLIST cell on the stack. Must use new
                     , E3013                       //!<  Invalid to create a Datum cell on the stack. Must use new
                     , E3014                       //!<  INTERNAL ERROR: Sublist not initialized correctly
                     , E3015                       //!<  Assignment illegal. Right hand side not a SlipHeader
                     , E3016                       //!<  Can not assign a SlipSublist to a TEMPORARY. 
                     , E3017                       //!<  SlipDatum must be assigned to a SlipDatum
                     , E3018                       //!<  Unable to delete a Slip Cell already deleted
                     , E3019                       //!<  Slip Cell has been deleted and can not be used
                     , E3020                       //!<  Unknown or illegal input argument 
                     , E3021                       //!<  Attempt to return a cell to the AVSL not from the AVSL. 
                     , E3022                       //!<  Can not insert a non-AVSL cell into a list. 
                     , E3023                       //!<  SlipPointer object has been deleted.
                     , E3024                       //!<  Delete header is illegal. header.deleteList() must be used
                     , E3025                       //!<  Descriptor List has incorrect number of entries
                     , E3026                       //!<  Attempt to use deleted SlipHeader

          //-------------------------------------------------------------
          // Input/Output errors
          //-------------------------------------------------------------
                     , E4001                       //!<  Duplicate name in hash table.
                     , E4002                       //!<  Illegal type. Must be a User Data object.
                     , E4003                       //!<  Hash table is unable to allocate memory.
                     , E4004                       //!<  User Data parse method can not be null.
                     , E4005                       //!<  User Data Function not found. getParse returned NULL..
                     , E4006                       //!<  Can not expand sublist. Name not found.
                     , E4007                       //!<  Unable to open include file.
                     , E4008                       //!<  Key is NULL in Descriptor List.
                     , E4009                       //!<  Value is NULL in Descriptor List.
                     , E4010                       //!<  Missing list in sublist definition.
                     , E4011                       //!<  Unable to register Sublist name.
                     , E4012                       //!<  List not found for sublist definition.
                     , E4013                       //!<  Unable to output string. Too large.
                     , E4014                       //!<  Unable to open output list file.
                     , E4015                       //!<  Two list definitions found.
                     , E4016                       //!<  Can not attach a named list to a named list.
                     , E4017                       //!<  Can not attach a anonymous list to a named list.
                     , E4018                       //!<  Named List in Descriptor List can not be found.
                     , E4019                       //!<  Descriptor list not empty in name list..
                     , ELAST                       //!<  marker for number of enums in list
                     }; // enum eError

      /**
       * @class SlipErr::Error SlipErr.h "<slip/SlipErr.h>"
       * @brief Error Message Structure
       *
       * <p>Message encapsulation for all diagnostic messages.</p>
       * <p>Diagnostic messages are stored as objects in the list of messages,
       *    <i>SlipErr::slipMessage</i>. The structure provides services needed 
       *    to support error handling:</p>
       * <table border='0'>
       *    <tr>
       *       <td><b>message</b></td>
       *       <td>The text of the diagnostic message.</td>
       *    </tr>
       *    <tr>
       *       <td><b>errorNumber</b></td>
       *       <td>An ASCII string represented the message 'name'. Message
       *           names begin with 'E' and have 4 digits, such as "E3024"..</td>
       *    </tr>
       *    <tr>
       *       <td><b>state</b></td>
       *       <td>Defines the default treatment of each message</td>
       *       <table border='0'>
       *          <td><b>ignore</b></td>
       *          <td>Do not output a message and do not throw and exception.</td>
       *          <td><b>warning</b></td>
       *          <td>Issue a diagnostic message and do not throw an exception.</td>
       *          <td><b>exception</b></td>
       *          <td>Issue a diagnostic message and throw a SlipException.</td>
       *       </table>
       *    </tr>
       *    <tr>
       *       <td><b>index</b></td>
       *       <td>Index position in the SlipErr::slipMessage table.</td>
       *    </tr>
       * </table>
       * <p><b>METHODS</b></p>
       * <ul>
       *    <li><b>getState</b> Return the current message state.</li>
       *    <li><b>getErrorNumber</b> Return the string representing the message name.</li>
       *    <li><b>getIndex</b> Return the table index.</li>
       *    <li><b>getMessage</b> Return the message ASCII text.</li>
       *    <li><b>isIgnore</b> <b>true</b> if the current state is ignore.</li>
       *    <li><b>isException</b> <b>true</b> if the current state is exception.</li></li>
       *    <li><b>setState</b> Rest the current state.</li>
       * </ul>
      */
      class Error {
      private:
         const string   message;                   //!< error message
         const string   errorNumber;               //!< error number for message
         const eError   index;                     //!< enumerated value (message position)
               errorType state;                    //!< state to take (ignore, warning, exception
      public:
         Error(eError index, errorType state, string errorNumber, string message)
               : index(index)                       //!< index of message in the error message array
               , state(state)                       //!< state (eIgnore, eWarning, eException)
               , errorNumber(errorNumber)           //!< text string for error number
               , message(message)                   //!< text of message
          { }
          Error() : index(ELAST), message(""), errorNumber(""), state(eIgnore) { }
          string    getErrorNumber() { return errorNumber; }
          int       getIndex()       { return (int)index; }
          string    getMessage()     { return message; }
          errorType getState()       { return state; }
          bool      isException()    { return state == eException; }
          bool      isIgnore()       { return state == eIgnore; }
          bool      isWarning()      { return state == eWarning; }
          void      setState(errorType state) { this->state = state; }
      }; // struct Error

      static errorType setErrorState(errorType state, string err);
      static Error slipMessage[ELAST];                      //!< array of diagnistic message objects

   }; // class SlipErr
   
   /**
    * @typedef slip::errorCallback
    * 
    * @brief Prototype for user error message callback program.
    * 
    * <p>User supplied callback function is called when a runtime error
    *    is detected. The user error function is passed the filename and 
    *    line number where the message occured, and the diagnostic text.</p>
    * <p>The boolean return value determines what the error processing software
    *    will do. If the return value is <i>true</i>, then the diagnostic
    *    routine will abort execution. If the return value is <i>false</i>
    *    then the diagnostic routine will return to the user and not abort
    *    execution. If a return to the user is requested, the state of SLIP
    *    will be uncertain and it is possible that a cascade of future errors
    *    will occur.</p>
    * @param[in] filename (string) filename where an error has been detected.
    * @param[in] lineno (int) line number where an error has been detected.
    * @param[in] message (string) diagnostic message text.
    * @return <i>true</i>, abort execution, <i>false</i> do not abort execution.
    */
   typedef bool (*errorCallback)(string filename, int lineno, SlipErr::Error err, string message);
     
   void postError(string file, int lineno, SlipErr::eError error, string name, string prefix, string suffix, const SlipCellBase& cell1, const SlipCellBase& cell2);
   void postError(string file, int lineno, SlipErr::eError error, string name, string prefix, string suffix, const SlipCellBase& cell1);
   void postError(string file, int lineno, SlipErr::eError error, string name, string prefix, string suffix);
   void postError(string file, int lineno, SlipErr::eError error, string name);
   errorCallback userCallBack(errorCallback callBack);                          // Set user defined error callback function.s

} // namespace slip

#endif	/* SLIPERR_H */

