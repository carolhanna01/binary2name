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
 * @file    SlipDef.h
 * @author  A. Schwarz
 * @date    October 28, 2011
 *
 */

#ifndef SLIPDEF_H
#define	SLIPDEF_H

# include <string>
# include <stdlib.h>
# include <stdint.h>

namespace slip {
    
class SlipPointer;                                                              //!< Forward reference
class SlipCellBase;                                                             //!< Forward reference
       
#ifdef CHAR
    # undef CHAR
#endif
#ifdef UCHAR
    # undef UCHAR
#endif
#ifdef SHORT
    # undef SHORT
#endif
#ifdef USHORT
    # undef USHORT
#endif
#ifdef INT
    # undef INT
#endif
#ifdef UINT
    # undef UINT
#endif
#ifdef LONG
    # undef LONG
#endif
#ifdef ULONG
    # undef ULONG
#endif
#ifdef FLOAT
    # undef FLOAT
#endif
#ifdef DOUBLE
    # undef DOUBLE
#endif
#ifdef PTR
    # undef PTR
#endif

    //---------------------------------------------------------------------
    // Slip Typedef
    //---------------------------------------------------------------------

    typedef unsigned char   UCHAR;                                              //  8-bits
    typedef signed   char   CHAR;                                               //  8-bits
    typedef uint16_t        USHORT;                                             // 16-bits
    typedef int16_t         SHORT;                                              // 16-bits
    typedef unsigned int    UINT;                                               // 16-bits
    typedef signed   int    INT;                                                // 16-bits
    typedef uint32_t        ULONG;                                              // 32-bits
    typedef int32_t         LONG;                                               // 32-bits
    typedef float           FLOAT;                                              // 32-bits
    typedef double          DOUBLE;                                             // 64-bits
    typedef SlipPointer *   PTR;                                                // pointer to a SlipPointer object
    typedef SlipPointer *   STRING;                                             // pointer to a SlipString  object
    
    class SlipDatum;
    class SlipHeader;
    typedef SlipDatum& (*Parse)(SlipHeader& head);                              // pointer to User Data parse method

   /**
    * @brief The error states supported.
    * <p>Each error message has an associated error state. The state
    *    determines whether the error is to be output, and if output,
    *    whether an exception is to be thrown.</p>
    * <p></p>
    * <center>
    * | error type | output | exception |
    * | :--------- | :----- | :-------= |
    * | eIgnore    | No     | No        |
    * | eWarning   | Yes    | No        |
    * | eException | Yes    | Yes       |
    * -----------------------------------
    * </center>
    */
  enum errorType {
       eIgnore                                                                  //!< ignore the error - no output
     , eWarning                                                                 //!< issue a warning message
     , eException                                                               //!< issue a warning message & throw an exception
  };

    //---------------------------------------------------------------------
    // Slip operation types
    //---------------------------------------------------------------------
   enum ClassType { eUNDEFINED =  0                                             //!< undefined object
                  , eBOOL      =  1                                             //!< bool      object
                  , eCHAR      =  2                                             //!< char      object
                  , eDOUBLE    =  3                                             //!< double    object
                  , eHEADER    =  4                                             //!< header    object
                  , eLONG      =  5                                             //!< long      object
                  , ePTR       =  6                                             //!< pointer   object
                  , eREADER    =  7                                             //!< reader    object
                  , eSTRING    =  8                                             //!< string    object
                  , eSUBLIST   =  9                                             //!< sublist   object
                  , eUCHAR     = 10                                             //!< unsigned char object
                  , eULONG     = 11                                             //!< unsigned long object
                  , eSENTINEL                                                   // non-class sentinel
                  };

    //------------------------------------------------------------------
    // Cell DATA field Definition(s)
    //------------------------------------------------------------------
   
    /**
     * <p> allData represents the full 64-bits of data in the cell data packet.
     *     This representation allows the data to be reset and changed as a 
     *     full 64-bit quantity.
     */
    struct sAllData {
       ULONG       data1;                                                       //!< 1st 32 bits of data
       ULONG       data2;                                                       //!< 2nd 32 bits of data
    }; // struct sAllData

    /**
     * <p>Each allocation from the heap to the AVSL is called a "Fragment".
     *    Each fragment reserves the first cell in the fragment for storage
     *    of personal data, the smallest and largest addresses of the 
     *    fragment. This information is used to validate that deleted SLIP 
     *    cells being returned to the AVSL are from the AVSL.
     */
    struct sFragmentData {
       SlipCellBase* lowWater;                                                   //!< smallest address in the fragment
       SlipCellBase* highWater;                                                  //!< larget address in the fragment
    }; // struct sFragmentData
    

    /**
     * <p>The Header Data field replaces the SLIP cell DATUM field when the SLIP
     *    cell is a Header. The Header data field consists of the following fields:
     *    </p>
     * <ul type = "dot">
     * <li> <b>DescriptionList:DESCRLIST</b> A pointer to a SLIP Header cell 
     *      containing a list of 2-tuples, a key and value, paired for key 
     *      searches, The Description list is unordered and duplicate keys 
     *      can be allowed.The value is any of a SLIP Data cell or a SLIP 
     *      Sublist cell referencing a SLIP list. If the DescriptionList is 
     *      <b>NULL</b> then a DesriptionList has not been assigned. If the
     *      DescriptionList points to a Header cell then a DescriptionList 
     *      has been assigned but the list may or may not be empty.</li>
     * <li> <b>RefCnt:REFCOUNT</b> The number of lists referencing this list. 
     *      If the ReferenceCount is assigned the value '1' on creation then the
     *      list must be manually deleted (garbage collection is manual).</li>
     * <li> <b>Mrk:LSTMRK</b> User defined identifier (mark). This is an
     *      uncontrolled numeric field reserved for user use.</li>
     * </ul>
     */
    struct sHeadData {                           // SLIP Header Datum Field
        void*      descriptionList;              //!< Association list Pointer
        USHORT     refCnt;                       //!< List Usage Count
        USHORT     mrk;                          //!< List in use mark
    }; // struct sHeadData

    /**
     * The Reader Data field replaces the SLIP cell DATUM field when the SLIP
     * cell is a Reader. The Reader data field consists of the following fields:
     * <ul type = "dot">
     *   <li> <b>LOFRDR:ListTop</b> Pointer to the SLIP Header cell of the current
     *        list being traversed.</li>
     *   <li> <b>LCNTR:Depth</b> The number of lists preceding this one. If this
     *        is the topmost list being searched, then this number is '0'. For each
     *        sublist traversed the count is incremented by one.</li>
     * </ul>
     */
    struct sReadData {                          // SLIP Reader Datum field
        void*     listTop;                      //!< Pointer to current list header
        USHORT    depth;                        //!< Reader list depth
    }; // struct sReadData

    /**
     * The Sublist Data field replaces the SLIP cell DATUM field when the SLIP
     * cell is a Sublist. The Sublist data field consists of the following fields:
     * <ul type = "dot">
     * <li> <b>_:Header</b> A pointer to a SLIP Header cell</li>
     * </ul>
     */
    struct sSublist {                            //!< SLIP Sublist Datum field
        void*     Header;                        //!< Pointer to sublist header
    }; // struct sSublist

    /**
     * <p>The Data cell Data field replaces the SLIP cell DATUM field when the SLIP
     *    cell is a Data cell. The Datum field contains user used and user defined
     *    data. The data field contains data types which can participate in numeric
     *    computation and ones that can not. The participatory types are boolean,
     *    long, and double. The non-participatory types are pointer and string. The
     *    implementation allows a C++ boolean to be stored in the field. Booleans
     *    are treated as '0' for false in computations and '1' for true in computations.
     *   Then the numeric types are used in a boolean expression, a '0' indicates
     *    false and anything else is true. </p>
     * <p>Administrative fragment data is maintained as the first SLIP cell in
     *    any space allocated to the AVSL from the heap. The administrative 
     *    data contains the smallest and largest address of the heap space.</p>
     * <p>Signed and unsigned numeric types are treated as separate data types.
     *    Internally all numeric types are converted to CHAR, UNSIGNED CHAR, LONG,
     *    UNSIGNED LONG, or DOUBLE.</p>
     * <ul type = "dot">
     * <li> <b>DATUM:Datum</b> SLIP cell data field.</li>
     * </ul>
     * <p>Aggregated data. The SLIP Data field is a union of all possible types
     *    that can be contained in the field. The union is used for sizing and
     *    definition but does not play a role in the implementation as such.
     * </p>
     */
    union Data {                                                                // Slip data contents
        sReadData Reader;                                                       //!< Reader Data Definition
        sHeadData Head;                                                         //!< Header Data Definition
        sSublist  Sublist;                                                      //!< Sublist Data Definition
        sAllData  allData;                                                      //!< wrapper for the union
        sFragmentData fragment;                                                 //!< administrative data
        bool      Bool;                                                         //!< compiler defined
        CHAR      Chr;                                                          //!<  8-bits
        UCHAR     UChr;                                                         //!<  8-bits
        LONG      Long;                                                         //!< 32-bits
        ULONG     ULong;                                                        //!< 32-bits
        DOUBLE    Double;                                                       //!< 64-bits
        PTR       Ptr;                                                          //!< Pointer to a class wrapper
        STRING    String;                                                       //!< pointer to a string class wrapper
        
        // TO BE IMPLEMENTED - arrays  contained within the DATUM field
//        CHAR      ChrAr[sizeof(sHeadData)/sizeof(CHAR)];        //
//        LONG      LngAr[sizeof(sHeadData)/sizeof(LONG)];        //
//        INT       IntAr[sizeof(sHeadData)/sizeof(int)];         //
//        SHORT     ShtAr[sizeof(sHeadData)/sizeof(short)];       //
    }; // union Data
    
    struct SlipState {
        ULONG         total;                                                    //!< Total number of SLIP Cells
        ULONG         avail;                                                    //!< Number of AVSL SLIP Cells
        ULONG         alloc;                                                     //!< Initial cells to allocation
        ULONG         delta;                                                     //!< Incremental cells to allocation
        SlipState(): total(0), avail(0), alloc(0), delta(0) { } 
        SlipState(ULONG total, ULONG avail, ULONG alloc, ULONG delta)
                 : total(total), avail(avail), alloc(alloc), delta(delta) { }
    }; // struct SlipState

    /*********************************************************
    *                      Constants                        *
   **********************************************************/
         /**
          * @brief named constants used throughout SLIP.
          * <p>Compilation for a 64-bit computer uses 64-bit addresses, and
          *    compilation for a 32-bit computer uses 32-bit addresses. The
          *    absolute values of TEMPORARY and UNDEFDATA are adjusted
          *    accordingly.</p>
          * <ul>
          *    <li>TEMPORARY: indicates that a SLIP object is on the runtime
          *        stack. When the scope containing the object is exited, the
          *        object will be deleted by the C++ kernel. A TEMPORARY
          *        object can not be put into a list or used in any persistent
          *        data structure.</li>
          *    <li>UNDEFDATA: indicates that the object is undefined, neither
          *        fish nor fowl. Until the object is transformed into one
          *        of the SLIP classes it is banished.</li>
          * </ul>
          */
    
#if __SIZEOF_POINTER__ == 4
    static const SlipCellBase * const TEMPORARY = (SlipCellBase*)0xFFFFFFFF;
    static const SlipCellBase * const UNDEFDATA = (SlipCellBase*)0xDeadBeef;
#elif __SIZEOF_POINTER__ == 8
    static const SlipCellBase * const TEMPORARY = (SlipCellBase*)0xFFFFFFFFFFFFFFFF;
    static const SlipCellBase * const UNDEFDATA = (SlipCellBase*)0xDeadBeefDeadBeef;
#endif
    
} // namespace slip

#endif	/* SLIPDEF_H */

