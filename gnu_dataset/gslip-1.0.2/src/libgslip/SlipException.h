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
 * @file    SlipException.h
 * @author  A. Schwarz
 * @date    June 1, 2012
 * 
 * @brief Container class for SLIP exceptions
 */

#ifndef _SLIPEXCEPTIONS_H
#define	_SLIPEXCEPTIONS_H

//using namespace std;
   
# include <exception>
# include <stdlib.h>
# include "SlipCellBase.h"
# include "SlipErr.h"

namespace slip {
   /**
    * @class SlipException
    * 
    * @brief This is a container class for SLIP exceptions data.
    * 
    * <p>SLIP exceptions are thrown in SlipErr. The data passed as a result
    *    of the exception is carried in a new (new SlipException) object.
    *    This class has methods to return the value of the retained data
    *    to the application. There are no means provided to modify the data.</p>
    * <p><b>FIELDS</b></p>
    * <ul>
    *    <li><b>base</b> The SlipErr:Error object containing the unformatted 
    *        diagnostic message. The object has methods useful to determining 
    *        message properties. </li>
    *    <li><b>message</b>The formatted diagnostic message. The message is
    *        formatted in SlipErr postError functions.</li>
    *    <li><b>cell1</b> If the SlipErr postError function was passed a SlipCell
    *        then a pointer that SlipCell is retained the the exception object.</li>
    *    <li><b>cell2</b> If the SlipErr postError function was passed a SlipCell
    *        then a pointer that SlipCell is retained the the exception object.</li>
    * </ul>
    * <p><b>METHODS</b></p>
    * <ul>
    *    <li><b>getBase</b> Returns the diagnostic message object.</li>
    *    <li><b>getCell1</b> Returns the value of the cell1 field.</li>
    *    <li><b>getCell2</b> Returns the value of the cell2 field.</li>
    *    <li><b>getMessage</b> Returns the formatted message.</li>
    *    <li><b>watch</b> Returns a C string (char*) representation of 
    *        message.</li>
    * </ul>
    * 
    * @see SlipErr
    */
   /**
    * @fn SlipException::getBase()
    * @brief Returns the SlipErr::Error base message object.
    * @return base (SlipErr::Error) unformatted diagnostic message
    * 
    * @fn SlipException::getCell1()
    * @brief Returns a pointer to the first SlipCell in the formatted message.
    * 
    * <p>If the formatted diagnostic message has a dump of two SlipCells then
    *    this is a pointer to the second one, otherwise <b><null/b></p>
    * @return SlipCell* pointer to a SlipCell
    * 
    * @fn SlipException::getCell2()
    * @brief Returns a pointer to the first SlipCell in the formatted message.
    * 
    * <p>If the formatted diagnostic message has a dump of a SlipCell then
    *    this is its pointer, otherwise <b><null/b></p>
    * @return SlipCell* pointer to a SlipCell
    * 
    * @fn SlipException::getMessage()
    * @brief Returns the formatted message.
    * @return message (string) formatted message
    * 
    * @fn SlipException::watch()
    * @brief Returns a C string version of the formatted message
    * @return C string (char*) version of the formatted message
    * 
    */
   class SlipException : public exception {
   private:
      void log();    
      
   protected:
      const SlipCellBase* cell1;                     //!< pointer to a SlipCell or <b>null</b>
      const SlipCellBase* cell2;                     //!< pointer to a SlipCell or <b>null</b>
      const SlipErr::Error base;                     //!< message object
      const string  message;                         //!< formatted message
      
   protected:
      SlipException() { };
   public:
      
      SlipException( const SlipErr::Error& base      //!< message object
                   , string message                  //!< formatted message
                   , const SlipCellBase* cell1       //!< pointer to a SlipCell or <b>null</b>
                   , const  SlipCellBase* cell2)     //!< pointer to a SlipCell or <b>null</b>
                   : base(base)
                   , message(message)
                   , cell1(cell1)
                   , cell2(cell2) {  
         log();
      }; // SlipException(SlipErr::Error message)
      
     virtual const char* what() const throw() {
        return message.c_str();
     }; // virtual const char* what() const throw();
      
     virtual ~SlipException()  throw() { }
      
      const SlipErr::Error getBase()    { return base;   }
      const SlipCellBase*  getCell1()   { return cell1;  }
      const SlipCellBase*  getCell2()   { return cell2;  }
      const string&        getMessage() { return message;}
   }; // class SlipException


}; // namespace slip
#endif	/* SLIPEXCEPTIONS_H */

