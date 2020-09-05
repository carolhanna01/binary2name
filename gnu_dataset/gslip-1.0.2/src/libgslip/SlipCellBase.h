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
 * @file    SlipCellBase.h
 * @author  A. Schwarz
 * @date November 16, 2011
 *
 * @brief File contains the SlipCellBase declarations
 */

#ifndef _SLIPCELLBASE_H
#define	_SLIPCELLBASE_H

# include <stdio.h>
# include <string>
# include <sstream>
# include "SlipDef.h"

using namespace std;

namespace slip {

/**
 * @class slip::SlipCellBase
 * @brief Base class for all Slip Cells.
 * <p>The enumerated Slip cells are:</p>
 * <ul>
 *    <li><b>SlipReader</b> a list iterator with memory.</li>
 *    <li><b>SlipDatum</b> a data carrier.</li>
 *    <li><b>SlipHeader</b> a list header.</li>
 *    <li><b>SlipSublist</b> a reference to a list from within a list.</li>
 * </ul>
 * <p>SlipCellBase: Internal composition of a generic SLIP cell. All fields are
 *    represented. The SLIP cell is a composite of all individual cell types.
 *    The cell size is the same for all SLIP cell types. Fields which are not
 *    used in an instance of a SLIP cell type have the unused field values
 *    initialized to <b>NULL</b>. Additionally, at cell creation all fields
 *    are initialized to <b>NULL</b>.
 * </p>
 * <p>The generic representation of a Slip cell is:</p>
 *  <table>
 *    <caption>SlipCellBase structure</caption>
 *    <tr>
 *       <td></td>
 *       <td></td>
 *       <td></td>
 *       <td></td>
 *       <td></td>
 *       <td></td>
 *       <td></td>
 *       <td></td>
 *       <td></td>
 *       <td></td>
 *       <td></td>
 *       <td></td>
 *       <td></td>
 *       <td></td>
 *       <td></td>
 *       <td></td>
 *    </tr>
 *    <tr>
 *       <td colspan="8">leftLink</td>
 *       <td colspan="8">rightLink</td>
 *    </tr>
 *    <tr>
 *       <td colspan="8">operator</td>
 *       <td colspan="8">Datum</td>
 *    </tr>
 *    <tr>
 *       <td colspan="8">Datum</td>
 *    </tr>
 * </table>
 * <br />
 * <p>Where the field definitions are:</p>
 * <ul>
 *    <li><b>leftLink</b></li>
 *    <ul>
 *       <li>Pointer to the preceding list cell when in a list.</li>
 *       <li>Null when not in a list.</li>
 *       <li>TEMPORARY (0xFFFFFFFF) when on the stack.</li>
 *       <li>UNDEFDATA (0xDeadBeef) when in the available space list (AVSL).</li>
 *    </ul>
 *    <li><b>rightLink</b></li>
 *    <ul>
 *       <li>Pointer to the next list cell when in a list.</li>
 *       <li>Null when not in a list.</li>
 *       <li>TEMPORARY (0xFFFFFFFF) when on the stack.</li>
 *       <li>Pointer to the next list cell when in the available space list 
 *           (AVSL).</li>
 *    </ul>
 *    <li><b>operator</b> pointer to invariant properties of the Slip cell type.</li>
 *    <li><b>Datum</b> defined by the derived object.</li>
 * </ul>
 * <p>The requirement that all SLIP cell types be the same size allows garbage
 *    collection to be used without memory holes. Memory reorganization is not
 *    necessary because uneven sized memory allocations do not exist. If total
 *    memory size is pre-allocated and constant then SLIP becomes suitable for
 *    use in an embedded system environment (memory utilization for SLIP is an
 *    internal matter and does not use the C++ heap).
 * </p>
 * <p>The original SLIP implementation has been altered in the following ways:
 * </p>
 * <ul>
 * <li> The SLIP Cell structure has been modified. Data type indications are not
 *      carried in the data structure but are returned programmatically. Reader,
 *      Header, and Data cells are primarily retained but operations for cells
 *      which can be accessed as part of a list (Data, Header, and Sublist) have
 *      operations carried in a new class field, 'operation'.</li>
 * <li> Each SLIP Cell owns its own operations. Either the SLIP class has
 *      operations intrinsic to the class type (Reader, Header, Sublist, Data) or
 *      the operations for the class are contained in the operations field, or
 *      both.</li>
 * <li> A distinction is drawn between cell types which can be found through
 *      normal list traversal (Header, Sublist, and Data) and Readers. Readers
 *      are always distinct and known by the program and valid operations are
 *      wholly contained in SlipReader. </li>
 * <p>  Headers, Sublists, and Data classes (SlipHeader, SlipSublist, and
 *      SlipDatum) all appear on lists and it is possible to attempt to execute
 *      an illegal operation for an object retrieved from a list. All operations,
 *      legal and illegal, which are not intrinsic to the class are contained in
 *      the operations field of SlipCellBase.
 * </p>
 * <p>  An attempt to execute an illegal operation for a cell type will cause
 *      a diagnostic message to be output.
 * </p>
 * <li> Cell types which can be on a list inherit from SlipCell. SlipReader,
 *      which can not be on a list, inherits from SlipDatum.</li>
 * <li> Errors are detected an returned for illegal operations.</li>
 * <li> Datum types are expanded for SlipDatum cells (they include characters,
 *      strings and pointers) with their associated operations.</li>
 * <li> Datum casting functions are provided for SlipDatum cells.</li>
 * <li> The datum field size in a SLIP Cell is a union of all valid datum types.
 *      Hence the size is the largest size of all valid types. The actual size
 *      may vary according to compiler and hardware (32-, 64-bit hardware).</li>
 * <li> Creation and / or modification of a SLIP Cell will change the valid
 *      operations depending on the datum created or modified.</li>
 * </ul>
 * <p>The SlipDatum fields and their definitions are given below. The definitions
 *    consists of three parts; the SLIP documentation name of the field, the
 *    implementation name of the field, and the field description. The general
 *    syntax is: <i><b>SLIPName:ImplementationName</b>: Description</i>,
 * </p>
 * <ul type = "dot">
 * <li> <b>:operation</b>: Pointer to the class containing the functions
 *      which can be legally used with this cell. This is an implementation
 *      feature not part of the original SLIP implementation.</li>
 * <li> <b>ID:id</b>: The cell identifier. This is one of the enum eType
 *      choices. C++ typing is used to assure proper alignment with the
 *      compiler generating slack bytes.</li>
 * <li> <b>LINKL:leftLink</b>: Pointer to the preceding SLIP cell in a list
 *      (except for READER cells).</li>
 * <li> <b>LINKR:rightLink</b>: Pointer to the next SLIP cell in a list
 *      (except for READER cells).</li>
 * <li> <b>DATUM:Datum</b>: SLIP cell specific data. An anonymous C++ union
 *      is used to capture all the types used. The union assures us a fixed
 *      size SLIP cell.</li>
 * </ul>
 * <p>
 * @note A SLIP Data cell with a type of STRING does not garbage collect
 *    the string. The SLIP Data cell is garbage collected.
 * <p>Allocates space and provides all space management functions and provides
 *    protected functions to support access and modification of a slip object.
 * </p>
 */
/**
 * @fn virtual string SlipCellBase::toString() const = 0
 * @brief Return a pretty-printed string for the object.
 * <p>Each Slip object derived from SlipCellBase is required to return a
 *    pretty string of the object. The string must be suitable for output
 *    and where possible, must be suitable for a user to read it in
 *    comfort.</p>
 * <p>The string representation for the derived classes are:</p>
 * <ul>
 *    <li><b>SlipDatum</b> the datum value:</li>
 *    <ul>
 *       <li>bool <b>true/false</b></li>
 *       <li>CHAR a single character. Non-printable characters are output
 *           without change, normal output becomes a blank.</li>
 *       <li>UCHAR same as CHAR.</li>
 *       <li>LONG decimal value.</li>
 *       <li>ULONG decimal value.</li>
 *       <li>DOUBLE double precision number.</li>
 *       <li>PTR the user must provide a "toString" method.</li>
 *       <li>STRING the string not surrounded in quotes.</li>
 *    </ul>
 *    <li><b>SlipHeader</b> hexadecimal dump of the SlipHeader object.</li>
 *    <li><b>SlipReader</b> pretty-printed dump of the SlipReader object.</li>
 *    <li><b>SlipSublist</b> hexadecimal dump of the SlipSublist object.</li>
 * </ul>
 * @return pretty string
 */
   class SlipCellBase {
      friend class SlipHashEntry;
      friend class SlipOp;
      friend class SlipSequencer;
      friend class SlipWrite;
   private:
       void*         operation;                                                 //!< Pointer to invariant operations class
       SlipCellBase* leftLink;                                                  //!< Pointer to preceding cell
       SlipCellBase* rightLink;                                                 //!< Pointer to following cell
       Data          datum;                                                     //!< SLIP cell data field

   /*********************************************************
    *                SLIP Private Functions                 *
   **********************************************************/
      static void getAVSLSpace(SlipCellBase*& lowWater, SlipCellBase*& highWater);
      static void insertFragmentLeft(SlipCellBase* cell, SlipCellBase* X);      // Insert X to current cell left
      static void insertFragmentRight(SlipCellBase* cell, SlipCellBase* X);     // Insert X to current cell right
      static void newFragment(SlipCellBase* lowWater, SlipCellBase* highWater);

   protected:

          /*************************************************
           *           Constructors & Destructors          *
           ************************************************/
       SlipCellBase() {
         if ((leftLink == rightLink) && (rightLink == UNDEFDATA)) {
            leftLink = rightLink = NULL;
         } else
            leftLink = rightLink = (SlipCellBase*)TEMPORARY;
       }

   /*********************************************************
    *               SLIP Protected Functions                *
   **********************************************************/

       void*                setOperator(void* const OP) {
         void* tmp = this->operation;
         this->operation = OP;
         return tmp;
       }
       static void*         setOperator(SlipCellBase* X, void* const OP)           { return X->setOperator(OP); }
       SlipCellBase*        setLeftLink(SlipCellBase* leftLink) {
         SlipCellBase* tmp = this->leftLink;
         this->leftLink = leftLink;
         return tmp;
       }
       SlipCellBase*        setRightLink(SlipCellBase* rightLink) {
         SlipCellBase* tmp = this->rightLink;
         this->rightLink = rightLink;
         return tmp;
       }
       static SlipCellBase* setLeftLink(SlipCellBase* X, SlipCellBase* leftLink)   { return X->setLeftLink(leftLink); }
       static SlipCellBase* setRightLink(SlipCellBase* X, SlipCellBase* rightLink) { return X->setRightLink(rightLink); }

       /**********************************************************
        *      Accessor functions for Slip Cell Data field       *
        **********************************************************/
       static sAllData*     getData(SlipCellBase const * const X)                  { return X->getData(); }

             /**********************************************
              *     Header Cell Data Access Functions      *
              **********************************************/
       void**               getHeadDescList() const { return &const_cast<SlipCellBase*>(this)->datum.Head.descriptionList; }
       USHORT*              getHeadRefCnt()   const { return &const_cast<SlipCellBase*>(this)->datum.Head.refCnt;          }
       USHORT*              getHeadMrk()      const { return &const_cast<SlipCellBase*>(this)->datum.Head.mrk;             }

       static void**        getHeadDescList(const SlipCellBase& X) { return X.getHeadDescList(); }
       static USHORT*       getHeadRefCnt(const SlipCellBase& X)   { return X.getHeadRefCnt();   }
       static USHORT*       getHeadMrk(const SlipCellBase& X)      { return X.getHeadMrk();      }

             /**********************************************
              *     Sublist Cell Data Access Functions      *
              **********************************************/
       void**               getSublistHeader() const { return &const_cast<SlipCellBase*>(this)->datum.Sublist.Header; }

       static void**        getSublistHeader(const SlipCellBase& X) { return X.getSublistHeader(); }

             /**********************************************
              *     Reader Cell Data Access Functions      *
              **********************************************/
       void**               getReaderTop()   const { return &const_cast<SlipCellBase*>(this)->datum.Reader.listTop; }
       USHORT*              getReaderDepth() const { return &const_cast<SlipCellBase*>(this)->datum.Reader.depth;   }

       static void**        getReaderTop(SlipCellBase& X)   { return X.getReaderTop();  }
       static USHORT*       getReaderDepth(SlipCellBase& X) { return X.getReaderDepth();}


             /**********************************************
              *   Slip Datum Cell data Access Functions    *
              **********************************************/
       bool*                getBool()  const       { return &const_cast<SlipCellBase*>(this)->datum.Bool;    }
       UCHAR*               getUChar() const       { return &const_cast<SlipCellBase*>(this)->datum.UChr;    }
       CHAR*                getChar()  const       { return &const_cast<SlipCellBase*>(this)->datum.Chr;     }
       ULONG*               getULong() const       { return &const_cast<SlipCellBase*>(this)->datum.ULong;   }
       LONG*                getLong()  const       { return &const_cast<SlipCellBase*>(this)->datum.Long;    }
       DOUBLE*              getDouble()const       { return &const_cast<SlipCellBase*>(this)->datum.Double;  }
       PTR*                 getPtr()   const       { return &const_cast<SlipCellBase*>(this)->datum.Ptr;     }
       PTR*                 getString()const       { return &const_cast<SlipCellBase*>(this)->datum.Ptr;     }
       sAllData*            getData()  const       { return &const_cast<SlipCellBase*>(this)->datum.allData; }
       void                 resetData()            { datum.allData.data1 = 0; datum.allData.data2 = 0;       }
       void                 setData(const SlipCellBase& X)   { datum.allData.data1 = X.datum.allData.data1;
                                                               datum.allData.data2 = X.datum.allData.data2;  }

       static bool*         getBool(const SlipCellBase& X)   { return X.getBool();   }
       static UCHAR*        getUChar(const SlipCellBase& X)  { return X.getUChar();  }
       static CHAR*         getChar(const SlipCellBase& X)   { return X.getChar();   }
       static ULONG*        getULong(const SlipCellBase& X)  { return X.getULong();  }
       static LONG*         getLong(const SlipCellBase& X)   { return X.getLong();   }
       static DOUBLE*       getDouble(const SlipCellBase& X) { return X.getDouble(); }
       static PTR*          getPtr(const SlipCellBase& X)    { return X.getPtr();    }
       static STRING*       getString(const SlipCellBase& X) { return X.getString(); }
       static sAllData*     getData(const SlipCellBase& X)   { return X.getData();   }
       static void          resetData(SlipCellBase& X)       { X.resetData();        }
       static void          setData(SlipCellBase& X, const SlipCellBase& Y) { X.setData(Y); }

       /**********************************************************
        *            Accessor functions for Slip Cell            *
        **********************************************************/
       void**               getOperator()               const  { return &const_cast<SlipCellBase*>(this)->operation;  }
       static void**        getOperator(const SlipCellBase* X) { return &(const_cast<SlipCellBase*>(X)->operation);   }
       static SlipCellBase* getLeftLink(const SlipCellBase& X) { return X.leftLink; }
       static SlipCellBase* getRightLink(const SlipCellBase& X) { return X.rightLink; }

       /**********************************************************
        *            Miscellaneous Slip Cell functions           *
        **********************************************************/

       static bool          isTemp(const SlipCellBase* cell)   { return cell->isTemp(); }

       static void          addAVSLCells( SlipCellBase* start, SlipCellBase* stop);

       static string        slipDump(string name, SlipCellBase& X);
       string               slipDump(string name)              { return slipDump(name, *this); }

               /*****************************************
                *             Miscellaneous             *
               ******************************************/
       static sAllData getDatumCopy(const void* X);                             // Return valid datum
       static void   remove(const SlipCellBase& X);                             // Perform Data defined datum delete
   public:

       /*********************************************************
        *                   Public Functions                    *
       **********************************************************/
       void * operator new(size_t size);                                        // Get a new SLIP cell from the AVSL
       void   operator delete(void* ptr) { }                                    // Class destructor puts cells on AVSL
       virtual ~SlipCellBase()  { }                                             // destructor
       SlipCellBase*   getLeftLink()  const   { return leftLink; }              // Return pointer to previous cell
       SlipCellBase*   getRightLink() const   { return rightLink; }             // Return pointer to next cell
       static void     deleteSlip();                                            // Reinitialize SLIP to its pristine state
       static void     sysInfo(ostream& out);                                   // Output the SLIP system info
       static void     slipInit();                                              // Initialize SLIP system
       static void     slipInit(ULONG alloc, ULONG lta);                        // Initialize SLIP system and allocation resources

       /*********************************************************
        *                SLIP Public Functions                  *
       **********************************************************/
       bool            isDeleted() const   { return (leftLink == UNDEFDATA); }  // Cell in AVSL?
       bool            isUnlinked() const  { return ((leftLink == NULL) || (rightLink == NULL)); } // Cell not on list 
       bool            isTemp() const      { return ((leftLink == rightLink) && (leftLink == (SlipCellBase*)TEMPORARY)); }
       static bool     isAVSL(const SlipCellBase* X);                           // Cell is in the AVSL

               /*****************************************
                *            Basic Primitive            *
               ******************************************/
       const ClassType getClassType() const;                                    // return the internal class type
       const string    getName()      const;                                    // string representing SlipDatumCell
       const bool      isData()       const;                                    // true if cell is data
       const bool      isDiscrete()   const;                                    // true if cell is a discrete number
       const bool      isHeader()     const;                                    // true if cell is a header
       const bool      isName()       const { return isSublist(); }             // true for SlipSublist
       const bool      isNumber()     const;                                    // true if cell is numeric data
       const bool      isPtr()        const;                                    // true if pointer
       const bool      isReal()       const;                                    // true if cell is a real data type
       const bool      isString()     const;                                    // true if string
       const bool      isSublist()    const;                                    // true if cell is sublist

               /*****************************************
                *             Miscellaneous             *
               ******************************************/
       static SlipState getSlipState();                                         // return the internal Slip State
       static  void    avslHistory(bool onOFF);                                 // true turns history on
               string  dumpLink() const;                                        // dump the left/right link values
       static  string  dumpLink(SlipCellBase const * const X);                  // dump the left/right link values
       static  void    printAVSL(string str = "");                              // Output the AVSL junk
       static  void    printClassSizes();                                       // Output the size of each class
       static  void    printFragmentList(string str = "");                      // format and output the fragment list
       static  void    printMemory(string str = "");                            // Output entire memory contents
       static  void    printState(string str = "");                             // Output the AVSL state
       virtual string  dump() const;                                            // return everything about the SLIP Cell
       virtual string  toString() const       = 0;                              // return SlipCell Value

   }; // class SlipCellBase

}; // namespace slip

#endif	/* SLILPCELLBASE_H */
