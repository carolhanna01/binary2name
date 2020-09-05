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
 * @file    SlipGlobal.cpp
 * @author  A. Schwarz
 * @date    August 17, 2012
 *
 */
# include "SlipGlobal.h"
# include "SlipBoolOp.h"
# include "SlipCharOp.h"
# include "SlipDoubleOp.h"
# include "SlipHeaderOp.h"
# include "SlipLongOp.h"
# include "SlipPointer.h"
# include "SlipPtrOp.h"
# include "SlipReaderOp.h"
# include "SlipStringOp.h"
# include "SlipSublistOp.h"
# include "SlipUndefinedOp.h"
# include "SlipUnsignedCharOp.h"
# include "SlipUnsignedLongOp.h"

namespace slip {

   /**
    * class myPtr
    * @brief This class supports defining an illegal link.
    * <p>If a SlipSequence link value is illegal then
    *    the constructed "undefined" SlipDatum object is
    *    inserted as the link. This only occurs if it is
    *    found that a replacement link is found faulty and
    *    the application elects to continue after a diagnostic
    *    message is issued. If there is no continue after
    *    failure, then the diagnostic message will cause an
    *    abort.</p>
    * <p>It is not expected that the overriden methods will ever
    *    be executed.</p>
    * <p>The overridden methods do nothing useful. </p>
    * <ul>
    *    <li><b>copy()</b>: return a pointer to itself (this).</li>
    *    <li><b>dump()</b>: return "Sequencer"</li>
    *    <li><b>remove()</b>: do nothing.</li>
    *    <li><b>toString()</b>: return "Sequencer"</li>
    * </ul>
    *
    */
   /**
    * @fn virtual SlipPointer* MyPtr::copy()
    * @brief The 'this' pointer is returned.
    * @return pointer to the current instantiation
    */
   /**
    * @fn virtual string MyPtr::dump()
    * @brief "Sequencer" is returned.
    * @return "Sequencer"
    */
   /**
    * @fn virtual void   MyPtr::remove()
    * @brief Do nothing.
    */
   /**
    * @fn virtual string MyPtr::toString();
    * @brief "Sequencer" is returned.
    * @return "Sequencer
    *
    */

   SlipDatum myData(1);

   class MyPtr: public SlipPointer {
   public:
      MyPtr() : SlipPointer(new string("ILLEGAL")) { }
      virtual SlipPointer* copy()     { return this; };
      virtual string dump()     const { return "ILLEGAL"; };
      virtual Parse  getParse() { return &parse; };
      static  SlipDatum& parse(SlipHeader& head) { return myData; };            // input parser
      virtual void   remove()   const { };
      virtual string toString() const { return "ILLEGAL";};
      virtual string write() const { return "()"; };
   };

   /**
    * @brief An illegally constructed SlipDatum object.
    * <p>Any operations using this object, including inserting it onto a list
    *    or deletion, should yield an error.</p>
    */
   static const SlipDatum constIllegalDatum(new MyPtr(), undefinedOP);
   const void* illegalDatum = (void*) &constIllegalDatum;

   const void* boolOP         = (void*) new SlipBoolOp();
   const void* charOP         = (void*) new SlipCharOp();
   const void* doubleOP       = (void*) new SlipDoubleOp();
   const void* headOP         = (void*) new SlipHeaderOp();
   const void* longOP         = (void*) new SlipLongOp();
   const void* ptrOP          = (void*) new SlipPtrOp();
   const void* readerOP       = (void*) new SlipReaderOp();
   const void* stringOP       = (void*) new SlipStringOp();
   const void* sublistOP      = (void*) new SlipSublistOp();
   const void* undefinedOP    = (void*) new SlipUndefinedOp();
   const void* ucharOP        = (void*) new SlipUnsignedCharOp();
   const void* ulongOP        = (void*) new SlipUnsignedLongOp();


}
