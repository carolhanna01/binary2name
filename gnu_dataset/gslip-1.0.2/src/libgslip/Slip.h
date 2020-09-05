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
 * @file    Slip.h
 * @author  A. Schwarz
 * @date    December 13, 2012
 * 
 * @brief Non-member binary operator implementation and gateway functions.
 * <p><b>Binary Functions</b></p><hr>
 * <p>Let <b>X</b> be a SlipDatum instance, <b>v</b> be an atomic object
 *    (bool, CHAR, UCHAR, LONG, ULONG, DOUBLE, STRING, or PTR), and 
 *    <b>op</b> be a binary operation, then <em>X op v</em> is executed
 *    by SlipDatum and <em>v op X</em> is executed by Slip. The non-member
 *    functions allow treatment of operations without regard to the order
 *    of the operands.</p>
 * <ul>
 *    <li>Where the operations are commutative, <em>X op v</em> yields the
 *      same value as <em>v op X</em>, execution of the operation is
 *      performed by commuting <em>v op X</em> into <em>X op v</em></li>.
 *    <li>Integer promotion (bool and CHAR promoted to LONG, and UCHAR
 *       promoted to ULONG) is performed when the results of the computation
 *       are the same under promotion.</li>
 *    <li>Logical inequalities are transformed to a <em>X op v</em> format
 *        before execution.</li>
 * </ul>
 * <p>This is summed up in the following table.</p>
 * <tt><center>
 *               | initial format |    commute     | promote |
 *               | :------------- | :------------- | :-----: |
 *               | v + X          |  X + v         | &radic;       |
 *               | v - X          |  v - X         | &radic;       |
 *               | v * X          |  X * v         | &radic;       |
 *               | v / X          |  v / X         | &radic;       |
 *               | v % X          |  v % X         | &radic;       |
 *               | v &lt;&lt; X         |  v &lt;&lt; X       | &radic;       |
 *               | v &gt;&gt; X         |  v &gt;&gt; X        | &radic;       |
 *               | v &amp; X          |  X &amp; v         | &radic;       |
 *               | v \| X          |  X \| v         | &radic;       |
 *               | v ^ X          |  X ^ v         | &radic;       |
 *               | v != X         |  X != v        | &radic;       |
 *               | v &lt; X          |  X &gt; v         | &radic;       |
 *               | v &lt;= X          |  X &gt;= v         | &radic;       |
 *               | v == X         |  X == v        | &radic;       |
 *               | v &gt;= X          |  X &lt;= v         | &radic;       |
 *               | v &gt; X          |  X &lt;< v         | &radic;       |
 *               | v += X         |  v += (type)X  |         |
 *               | v -= X         |  v += (type)X  |         |
 *               | v *= X         |  v += (type)X  |         |
 *               | v /= X         |  v += (type)X  |         |
 *               | v %= X         |  v += (type)X  |         |
 *               | v &lt;&lt;= X        |  v &lt;&lt;= (type)X |         |
 *               | v &gt;&gt;= X        |  v &gt;&gt;= (type)X |         |
 *               | v &amp;= X         |  v &amp;= (type)X  |         |
 *               | v \|= X         |  v \|= (type)X  |         |
 *               | v ^= X         |  v ^= (type)X  |         |
 * </center></tt>
 * <p>where</p>
 * <ul type="none">
 *    <li><b>initial format</b> is the format executed by a <em>Slip</em> 
 *        function.</li>
 *    <li><b>commute</b> is the commutative format. When the initial format
 *        and the commutative format are the same, no conversion is done
 *        before processing.</li>
 *    <li><b>promote</b> if this column is checked then integer promotion
 *        is done.</li>
 *    <li><b>type</b> <em>X</em> is converted to the type of <em>v</em>
 *        before the assignment statement.</li>
 * </ul>
 * <p>Actual execution depends on the operation field of <em>X</em>. That
 *    is <em>v op X</em> or <em>X op v</em> is executed by executing
 *    (X.operation)->op(v).</p>
 * <p>A more complete description of the operations and mechanisms is in
 *    <em>SlipDatum</em>.</p>
 * <p><b>Gateway Functions</b></p><hr>
 * <p>The functions provide gateway access the SlipCellBase methods which
 *    provide diagnostic and miscellaneous routines to the application. The
 *    functions are a shorthand to these methods.</p>
 * 
 * |   Function Name   | Description                                    |
 * |:------------------|:-----------------------------------------------|
 * | avslHistory       | true turns history on                          |
 * | getSlipState      | return the internal Slip State                 |
 * | printAVSL         | Output the AVSL junk                           |
 * | printClassSizes   | Output the size of each class                  |
 * | printFragmentList | format and output the fragment list            |
 * | printMemory       | Output entire memory contents                  |
 * | printState        | Output the AVSL state                          |
 * | setErrorState     | Set the error message state                    |
 * | sysInfo           | Output the SLIP system info                    |
 * | slipInit          | Initialize SLIP system                         |
 * | slipInit          | Initialize SLIP system and allocation resources|
 * 
 * @sa slip::SlipDatum
 * @sa slip::SlipCellBase
 *
 */

#ifndef SLIP_H
#define	SLIP_H

# include "SlipCell.h"
# include "SlipDatum.h"
# include "SlipDef.h"
# include "SlipErr.h"
# include "SlipException.h"
# include "SlipHeader.h"
# include "SlipReader.h"
# include "SlipSequencer.h"

namespace slip {
   
   /**
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
   
          /*************************************************
           *                Binary operators               *
          *************************************************/
   SlipDatum  operator+(const DOUBLE X, const SlipDatum& Y);                    // Y + V
   SlipDatum  operator-(const DOUBLE X, const SlipDatum& Y);                    // Y - V
   SlipDatum  operator*(const DOUBLE X, const SlipDatum& Y);                    // Y * V
   SlipDatum  operator/(const DOUBLE X, const SlipDatum& Y);                    // Y / V

   SlipDatum  operator+(const bool   X, const SlipDatum& Y);                    // Y + V
   SlipDatum  operator-(const bool   X, const SlipDatum& Y);                    // Y - V
   SlipDatum  operator*(const bool   X, const SlipDatum& Y);                    // Y * V
   SlipDatum  operator/(const bool   X, const SlipDatum& Y);                    // Y / V
   LONG       operator%(const bool   X, const SlipDatum& Y);                    // Y % V

   SlipDatum  operator+(const UCHAR  X, const SlipDatum& Y);                    // Y + V
   SlipDatum  operator-(const UCHAR  X, const SlipDatum& Y);                    // Y - V
   SlipDatum  operator*(const UCHAR  X, const SlipDatum& Y);                    // Y * V
   SlipDatum  operator/(const UCHAR  X, const SlipDatum& Y);                    // Y / V
   LONG       operator%(const UCHAR  X, const SlipDatum& Y);                    // Y % V

   SlipDatum  operator+(const CHAR   X, const SlipDatum& Y);                    // Y + V
   SlipDatum  operator-(const CHAR   X, const SlipDatum& Y);                    // Y - V
   SlipDatum  operator*(const CHAR   X, const SlipDatum& Y);                    // Y * V
   SlipDatum  operator/(const CHAR   X, const SlipDatum& Y);                    // Y / V
   LONG       operator%(const CHAR   X, const SlipDatum& Y);                    // Y % V

   SlipDatum  operator+(const ULONG  X, const SlipDatum& Y);                    // Y + V
   SlipDatum  operator-(const ULONG  X, const SlipDatum& Y);                    // Y - V
   SlipDatum  operator*(const ULONG  X, const SlipDatum& Y);                    // Y * V
   SlipDatum  operator/(const ULONG  X, const SlipDatum& Y);                    // Y / V
   LONG       operator%(const ULONG  X, const SlipDatum& Y);                    // Y % V

   SlipDatum  operator+(const LONG   X, const SlipDatum& Y);                    // Y + V
   SlipDatum  operator-(const LONG   X, const SlipDatum& Y);                    // Y - V
   SlipDatum  operator*(const LONG   X, const SlipDatum& Y);                    // Y * V
   SlipDatum  operator/(const LONG   X, const SlipDatum& Y);                    // Y / V
   LONG       operator%(const LONG   X, const SlipDatum& Y);                    // Y % V

       /*************************************************
        *                Bit operators                  *
       *************************************************/

   LONG       operator<<(const bool   X, const SlipDatum& Y);                   // Y << V
   LONG       operator>>(const bool   X, const SlipDatum& Y);                   // Y >> V
   LONG       operator&(const bool    X, const SlipDatum& Y);                   // Y & V
   LONG       operator|(const bool    X, const SlipDatum& Y);                   // Y | V
   LONG       operator^(const bool    X, const SlipDatum& Y);                   // Y ^ V

   LONG       operator<<(const UCHAR  X, const SlipDatum& Y);                   // Y << V
   LONG       operator>>(const UCHAR  X, const SlipDatum& Y);                   // Y >> V
   LONG       operator&(const UCHAR   X, const SlipDatum& Y);                   // Y & V
   LONG       operator|(const UCHAR   X, const SlipDatum& Y);                   // Y | V
   LONG       operator^(const UCHAR   X, const SlipDatum& Y);                   // Y ^ V

   LONG       operator<<(const CHAR   X, const SlipDatum& Y);                   // Y << V
   LONG       operator>>(const CHAR   X, const SlipDatum& Y);                   // Y >> V
   LONG       operator&(const CHAR    X, const SlipDatum& Y);                   // Y & V
   LONG       operator|(const CHAR    X, const SlipDatum& Y);                   // Y | V
   LONG       operator^(const CHAR    X, const SlipDatum& Y);                   // Y ^ V

   LONG       operator<<(const ULONG  X, const SlipDatum& Y);                   // Y << V
   LONG       operator>>(const ULONG  X, const SlipDatum& Y);                   // Y >> V
   LONG       operator&(const ULONG   X, const SlipDatum& Y);                   // Y & V
   LONG       operator|(const ULONG   X, const SlipDatum& Y);                   // Y | V
   LONG       operator^(const ULONG   X, const SlipDatum& Y);                   // Y ^ V

   LONG       operator<<(const LONG   X, const SlipDatum& Y);                   // Y << V
   LONG       operator>>(const LONG   X, const SlipDatum& Y);                   // Y >> V
   LONG       operator&(const LONG    X, const SlipDatum& Y);                   // Y & V
   LONG       operator|(const LONG    X, const SlipDatum& Y);                   // Y | V
   LONG       operator^(const LONG    X, const SlipDatum& Y);                   // Y ^ V

       /*************************************************
        *       Arithmetic Assignment operators         *
       *************************************************/

   DOUBLE     operator+=(DOUBLE& X, const SlipDatum& Y);                        // Y += V
   DOUBLE     operator-=(DOUBLE& X, const SlipDatum& Y);                        // Y -= V
   DOUBLE     operator*=(DOUBLE& X, const SlipDatum& Y);                        // Y *= V
   DOUBLE     operator/=(DOUBLE& X, const SlipDatum& Y);                        // Y /= V

   bool       operator+=(bool&   X, const SlipDatum& Y);                        // Y += V
   bool       operator-=(bool&   X, const SlipDatum& Y);                        // Y -= V

   UCHAR      operator+=(UCHAR&  X, const SlipDatum& Y);                        // Y += V
   UCHAR      operator-=(UCHAR&  X, const SlipDatum& Y);                        // Y -= V
   UCHAR      operator*=(UCHAR&  X, const SlipDatum& Y);                        // Y *= V
   UCHAR      operator/=(UCHAR&  X, const SlipDatum& Y);                        // Y /= V
   UCHAR      operator%=(UCHAR&  X, const SlipDatum& Y);                        // Y %= V
   UCHAR      operator<<=(UCHAR& X, const SlipDatum& Y);                        // Y <<= V
   UCHAR      operator>>=(UCHAR& X, const SlipDatum& Y);                        // Y >>= V
   UCHAR      operator&=(UCHAR&  X, const SlipDatum& Y);                        // Y &= V
   UCHAR      operator|=(UCHAR&  X, const SlipDatum& Y);                        // Y |= V
   UCHAR      operator^=(UCHAR&  X, const SlipDatum& Y);                        // Y ^= V

   CHAR       operator+=(CHAR&   X, const SlipDatum& Y);                        // Y += V
   CHAR       operator-=(CHAR&   X, const SlipDatum& Y);                        // Y -= V
   CHAR       operator*=(CHAR&   X, const SlipDatum& Y);                        // Y *= V
   CHAR       operator/=(CHAR&   X, const SlipDatum& Y);                        // Y /= V
   CHAR       operator%=(CHAR&   X, const SlipDatum& Y);                        // Y %= V
   CHAR       operator<<=(CHAR&  X, const SlipDatum& Y);                        // Y <<= V
   CHAR       operator>>=(CHAR&  X, const SlipDatum& Y);                        // Y >>= V
   CHAR       operator&=(CHAR&   X, const SlipDatum& Y);                        // Y &= V
   CHAR       operator|=(CHAR&   X, const SlipDatum& Y);                        // Y |= V
   CHAR       operator^=(CHAR&   X, const SlipDatum& Y);                        // Y ^= V

  ULONG       operator+=(ULONG&  X, const SlipDatum& Y);                        // Y += V
  ULONG       operator-=(ULONG&  X, const SlipDatum& Y);                        // Y -= V
  ULONG       operator*=(ULONG&  X, const SlipDatum& Y);                        // Y *= V
  ULONG       operator/=(ULONG&  X, const SlipDatum& Y);                        // Y /= V
  ULONG       operator%=(ULONG&  X, const SlipDatum& Y);                        // Y %= V
  ULONG       operator<<=(ULONG& X, const SlipDatum& Y);                        // Y <<= V
  ULONG       operator>>=(ULONG& X, const SlipDatum& Y);                        // Y >>= V
  ULONG       operator&=(ULONG&  X, const SlipDatum& Y);                        // Y &= V
  ULONG       operator|=(ULONG&  X, const SlipDatum& Y);                        // Y |= V
  ULONG       operator^=(ULONG&  X, const SlipDatum& Y);                        // Y ^= V

   LONG       operator+=( LONG&   X, const SlipDatum& Y);                       // Y += V
   LONG       operator-=( LONG&   X, const SlipDatum& Y);                       // Y -= V
   LONG       operator*=( LONG&   X, const SlipDatum& Y);                       // Y *= V
   LONG       operator/=( LONG&   X, const SlipDatum& Y);                       // Y /= V
   LONG       operator%=( LONG&   X, const SlipDatum& Y);                       // Y %= V
   LONG       operator<<=( LONG&  X, const SlipDatum& Y);                       // Y <<= V
   LONG       operator>>=( LONG&  X, const SlipDatum& Y);                       // Y >>= V
   LONG       operator&=( LONG&   X, const SlipDatum& Y);                       // Y &= V
   LONG       operator|=( LONG&   X, const SlipDatum& Y);                       // Y |= V
   LONG       operator^=( LONG&   X, const SlipDatum& Y);                       // Y ^= V

       /*************************************************
        *              Logical operators                *
       *************************************************/

    bool      operator!=(const bool   X, const SlipDatum& Y);                   // Y != X
    bool      operator<(const bool    X, const SlipDatum& Y);                   // Y <  X
    bool      operator<=(const bool   X, const SlipDatum& Y);                   // Y <= X
    bool      operator==(const bool   X, const SlipDatum& Y);                   // Y == X
    bool      operator>=(const bool   X, const SlipDatum& Y);                   // Y >= X
    bool      operator>(const bool    X, const SlipDatum& Y);                   // Y ?  X

    bool      operator!=(const DOUBLE X, const SlipDatum& Y);                   // Y != V
    bool      operator<(const DOUBLE  X, const SlipDatum& Y);                   // Y <  V
    bool      operator<=(const DOUBLE X, const SlipDatum& Y);                   // Y <= V
    bool      operator==(const DOUBLE X, const SlipDatum& Y);                   // Y == V
    bool      operator>=(const DOUBLE X, const SlipDatum& Y);                   // Y >= V
    bool      operator>(const DOUBLE  X, const SlipDatum& Y);                   // Y >  V

    bool      operator!=(const UCHAR  X, const SlipDatum& Y);                   // Y != V
    bool      operator<(const UCHAR   X, const SlipDatum& Y);                   // Y <  V
    bool      operator<=(const UCHAR  X, const SlipDatum& Y);                   // Y <= V
    bool      operator==(const UCHAR  X, const SlipDatum& Y);                   // Y == V
    bool      operator>=(const UCHAR  X, const SlipDatum& Y);                   // Y >= V
    bool      operator>(const UCHAR   X, const SlipDatum& Y);                   // Y > V

    bool      operator!=(const CHAR    X, const SlipDatum& Y);                  // Y !=  V
    bool      operator<(const CHAR    X, const SlipDatum& Y);                   // Y <  V
    bool      operator<=(const CHAR   X, const SlipDatum& Y);                   // Y <= V
    bool      operator==(const CHAR   X, const SlipDatum& Y);                   // Y == V
    bool      operator>=(const CHAR   X, const SlipDatum& Y);                   // Y >= V
    bool      operator>(const CHAR    X, const SlipDatum& Y);                   // Y >  V

    bool      operator!=(const ULONG  X, const SlipDatum& Y);                   // Y != V
    bool      operator<(const ULONG   X, const SlipDatum& Y);                   // Y <  V
    bool      operator<=(const ULONG  X, const SlipDatum& Y);                   // Y <= V
    bool      operator==(const ULONG  X, const SlipDatum& Y);                   // Y == V
    bool      operator>=(const ULONG  X, const SlipDatum& Y);                   // Y >= V
    bool      operator>(const ULONG   X, const SlipDatum& Y);                   // Y > V

    bool      operator!=(const LONG   X, const SlipDatum& Y);                   // Y != V
    bool      operator<(const LONG    X, const SlipDatum& Y);                   // Y <  V
    bool      operator<=(const LONG   X, const SlipDatum& Y);                   // Y <= V
    bool      operator==(const LONG   X, const SlipDatum& Y);                   // Y == V
    bool      operator>=(const LONG   X, const SlipDatum& Y);                   // Y >= V
    bool      operator>(const LONG    X, const SlipDatum& Y);                   // Y > V

    bool      operator!=(const PTR    X, const SlipDatum& Y);                   // Y != V
    bool      operator==(const PTR    X, const SlipDatum& Y);                   // Y == V

    bool      operator!=(const STRING  X, const SlipDatum& Y);                  // Y != V
    bool      operator==(const STRING  X, const SlipDatum& Y);                  // Y == V


       extern  void    avslHistory(bool onOFF);                                 // true turns history on
       extern SlipState getSlipState();                                         // return the internal Slip State
       extern  void    printAVSL(string str = "");                              // Output the AVSL junk
       extern  void    printClassSizes();                                       // Output the size of each class
       extern  void    printFragmentList(string str = "");                      // format and output the fragment list
       extern  void    printMemory(string str = "");                            // Output entire memory contents
       extern  void    printState(string str = "");                             // Output the AVSL state
       extern  errorType  setErrorState(errorType state, string err);           // Set error message state
       extern  void    sysInfo(ostream& out);                                   // Output the SLIP system info
       extern  void    slipInit();                                              // Initialize SLIP system
       extern  void    slipInit(ULONG alloc, ULONG lta);                        // Initialize SLIP system and allocation resources
       extern  errorCallback setCallBack(errorCallback callBack);               // Set user defined error callback function.s

}; // namespace slip
#endif	/* SLIP_H */

