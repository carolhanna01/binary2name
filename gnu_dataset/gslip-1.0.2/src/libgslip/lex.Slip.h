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
 * @file    lex.Slip.h
 * @author  A. Schwarz
 * @date November 17, 2013
 *
 * @brief Scanner exporable interface
 *<p></p>
 */

 
#ifndef LEX_SLIP_H
#define	LEX_SLIP_H

# include <string>
# include "Slip.Bison.hpp"

using namespace std;
using namespace slip;

/**
 * @brief Cleanup scanner by deleting allocated storage.
 */
extern void deleteLexState();

/**
 * @brief Get the file name of the current input file.
 * @return (string) file name
 */
extern string getFileName();

/**
 * @brief Check if there are any more lexer states.
 * @return <b>true</b> more lexer states exist
 */
extern bool isEmpty();

/**
 * @brief Are we processing the initial file.
 * @return <b>true</b> this is the initial input list file
 */
extern bool isTop();

 /**
  * @brief External reference to lexer state restore interface.
  * <p>Each time an include file is exited, the current state must
  *    be restored. </p>
  */
extern bool restoreLexState();

 /**
  * @brief External reference to lexer state save interface.
  * <p>Each time an include file is seen, the current state must
  *    be saved.</p>
  * @return <b>true</b> save successful
  */
extern bool saveLexState(string fileName);

/**
 * @brief Turn debugging on.
*/
extern void setLexerDebugON();

/**
 * @brief Turn debugging off.
*/
extern void setLexerDebugOFF();

/**
 *@brief Scanner Name.
 */
extern int Sliplex( parser::semantic_type* yylval
                  , parser::location_type* yylloc);

#endif	/* LEX_SLIP_H */

