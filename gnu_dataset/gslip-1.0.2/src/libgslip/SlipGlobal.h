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
 * @file    SlipGlobal.h
 * @author  A. Schwarz
 * @date    July 19, 2009
 *
 * @brief Slip Global Data
 *
 * <p><b>Description</b></p>
 * <p>Contains definitions for each of the operator classes. Each operator has
 *    a predefined set of valid operations (see SlipOp.h).
 * </p>
 * <p>Each SLIP cell is capable of valid in any of the defined operators and this
 *    involvement is amorphous, it can change in time depending on the contexts of
 *    the SLIP cell. The definitions provided are the set of operations to be used
 *    for a particular SLIP cell. the user is responsible for casting an individual
 *    SLIP cell to the correct operational environment. This is a convenience and
 *    not a requirement.
 * </p>
 */

#ifndef _GLOBAL_H
#define	_GLOBAL_H

namespace slip {

   enum userConstants {
      MAXREFCNT      = 32767                   //! Maximum number of references to a list
    , REFMARKMASK    = 0X7FFF                  //! Mask for header reference mark
   };
   
   /**************************************
   *          SLIP Cell SubTypes         *
   **************************************/
   enum eSubType {
                 SUBUNDEFINED   = 0            //! Undefined Cell Subtype
               , SUBLIST_HEADER = 1            //! List HeaderCcell Subtype
               , SUBDESCRIPTION = 2            //! List Description Cell Subtype
                         
               , SUBbool        = 1            //! SlipDatum Bool Subtype
               , SUBCHAR        = 2            //! SlipDatum CHAR Subtype
               , SUBUCHAR       = 3            //! SlipDatum UCHAR Subtype
               , SUBLONG        = 4            //! SlipDatum LONG Subtype
               , SUBULONG       = 5            //! SlipDatum ULONG Subtype
               , SUBDOUBLE      = 6            //! SlipDatum DOUBLE Subtype
                         
               , SUBSTRING      = 1            //! SlipDatum STRING Subtype
               , SUBPOINTER     = 2            //! SlipDatum PTR Subtype
                         
   }; // enum eSubType 
   
   /**************************************
   *            SLIP Cell ID             *
   **************************************/
   enum eID {
              IDUNDEFINED   = 0                //! Undefined SlipCell
            , IDSUBLIST     = 1                //! SlipSublist Cell
            , IDHEADER      = 2                //! SlipHeader Cell
            , IDREADER      = 3                //! SlipReader Cell
            , IDSEQUENCER   = 4                //! SlipSequencer
            , IDNUMERIC     = 5                //! SlipDatum numeric data type
            , IDNON_NUMERIC = 6                //! SlipDatum non-numeric data type
   }; // enum eID
   
    enum eType {
              undefined       =  0x00          //!< Illegal type - defines first enumeration value
            , SUBLIST         =  0x01          //!< Sublist Cell (Name Cell)
            , HEADER          =  0x02          //!< Header Type
            , LIST_HEADER     =  0x12          //!< Header Data Cell
            , DESCRIPTION     =  0x22          //!< Description list Header Cell
            , READER          =  0x03          //!< Reader
            , SEQUENCER       =  0x04          //!< Sequencer
                                               //!< Numeric Types
            , NUMERIC         =  0x05          //!< Numeric Data Cell
            , tbool           =  0x15          //!< Data    Cell:bool
            , tCHR            =  0x25          //!< Data    Cell:char
            , tUCHR           =  0x35          //!< Data    cell unsigned chcar
            , tLONG           =  0x45          //!< Data    Cell:long
            , tULONG          =  0x55          //!< Data    Cell:unsigned long
            , tDOUBLE         =  0x65          //!< Data    Cell:double
                                               //!< Non-Numeric Type
            , NON_NUMERIC     =  0x06          //!< Non-Numeric Data Cell
            , tSTR            =  0x16          //!< Data    Cell:string
            , tPTR            =  0x26          //!< Data    Cell:user definedpointer
            , guard           =  tPTR + 1      //!< Illegal type - defines last enumeration value
         }; // enum eType
    

extern const void* boolOP;                     //= new SlipBoolOp();
extern const void* charOP;                     //= new SlipCharOp();
extern const void* doubleOP;                   //= new SlipDoubleOp();
extern const void* headOP;                     //= new SlipHeaderOp();
extern const void* longOP;                     //= new SlipLongOp();
extern const void* ptrOP;                      //= new SlipPtrOp();
extern const void* readerOP;                   //= new SlipReaderOp();
extern const void* stringOP;                   //= new SlipStringOp();
extern const void* sublistOP;                  //= new SlipSublistOp();
extern const void* undefinedOP;                //= new SlipUndefinedOp();
extern const void* ucharOP;                    //= new SlipUnsignedCharOp();
extern const void* ulongOP;                    //= new SlipUnsignedLongOp();
extern const void* illegalDatum;

}; // namespace slip
#endif	/* _GLOBAL_H */

