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
 * @file    SlipCellBase.cpp
 * @author  A. Schwarz
 * @date    May 27, 2012
 *
 * @brief Contains the SlipCellBase definitions and static support methods.
 */

# include <iostream>
# include <iomanip>
# include <exception>
# include "SlipCellBase.h"
# include "SlipCell.h"
# include "SlipHeader.h"
# include "SlipSublist.h"
# include "SlipDatum.h"
# include "SlipReader.h"
# include "SlipGlobal.h"
# include "SlipException.h"
# include "SlipErr.h"
# include "SlipOp.h"
# include "SlipBoolOp.h"
# include "SlipCharOp.h"
# include "SlipDoubleOp.h"
# include "SlipHeaderOp.h"
# include "SlipLongOp.h"
# include "SlipPtrOp.h"
# include "SlipReaderOp.h"
# include "SlipStringOp.h"
# include "SlipSublistOp.h"
# include "SlipUndefinedOp.h"
# include "SlipUnsignedCharOp.h"
# include "SlipUnsignedLongOp.h"

using namespace std;

namespace slip {
   static bool avslHistoryFlag = false;                                         //!< history flag <em>true</em> when AVSL output wanted
   static bool firstTimeOnlyTime = false;                                       //!< Singleton flag for SLIP setup
   static ULONG const ALLOC = 25;                                               //!< Default initial and delta allocationof AVSL  cells
   static ULONG const MINALLOC = 25;                                            //!< Minimum cells to allocate on AVSL

   static const ULONG SlipCellBaseSize = sizeof(SlipCellBase);                  //!< Byte size of SlipCellBase
   static const ULONG SlipCellSize     = sizeof(SlipCell);                      //!< Byte size of SlipCell
   static const ULONG SlipDatumSize    = sizeof(SlipDatum);                     //!< Byte size of SlipDatum
   static const ULONG SlipHeaderSize   = sizeof(SlipHeader);                    //!< Byte size of SlipHeader
   static const ULONG SlipSublistSize  = sizeof(SlipSublist);                   //!< Byte size of SlipReader
   static const ULONG SlipReaderSize   = sizeof(SlipReaderCell);                //!< Byte size of Cell size used in AVSL allocation
   static const ULONG cellSize         = (((max(SlipCellBaseSize,               //!< Cell size rounded up to a 32-bit boundary
                                            max(SlipCellSize,
                                            max(SlipDatumSize,
                                            max(SlipHeaderSize,
                                            max(SlipSublistSize,
                                                SlipReaderSize))))))+ 3) / 4) * 4;

   /**
    * @brief Administrative structure and methods for Available Space List.
    * <p>The state of the Available Space List. The state is defined as all
    *    required operational variables and their values, and the allocation
    *    heap size (in SlipCellBase cells). The state is updated each time
    *    that a SLIP Cell is allocated or deleted, and each time that a request
    *    for allocation of a new AVLS fragment from heap space is made.</p>
    * <p>Each cell on the AVSL is counted as a single cell for purposes of
    *    counting.</p>
    * <p>When a sublist cell is allocated in response to a user request, the
    *    header list reference count is decremented. If the count is non-zero
    *    the referenced sublist head is not put on the AVSL (other references
    *    to the list exist). When no more references exist (the reference count
    *    is zero after decrementing) then the list is deallocated and all cells
    *    in the list, including the header, are placed in the AVSL. Any sublists
    *    in the deallocated list are not evaluated until the sublist cell is
    *    allocated. This definition is recursive.</p>
    * <p>This can be summarized by:</p>
    * <ul>
    *  <li>The reference count of the referenced list is decremented.</li>
    *  <li>If the reference count is zero, list header and the list contents
    *   `  are placed onto the AVSL. </li>
    *  <li>The AVSL count is incremented for each cell placed onto the AVSL.</li>
    *  <li>Investigation of sublist cells is deferred until an allocation
    *      request is made.</li>
    *  <li>Each cell placed on the AVSL increases the AVSL count by one.
    * </ul>
    * <p>The structure maintains certain administrative data in addition to
    *    tracking space utilization.</p>
    * <ul>
    *   <li>AVSL Management is controlled with the following variables. </li>
    *   <ul>
    *     <li><em>top</em> Pointer to the first cell in the AVSL. If the AVSL
    *         is empty then the value is <em>null</em>.</li>
    *     <li><em>bot</em> Pointer to the last cell in the AVSL. If the AVSL
    *         is empty then the value is <em>null</em>.</li>
    *   </ul>
    *   <p>The AVSL is a singly linked list. The left link (of a cell) is set
    *      to <em>UNDEFINED</em> and the right link points to the next cell or
    *      is <em>null</em> if it is the last cell in the AVSL.</p>
    *   <li>AVSL Reporting is controlled with:</li>
    *   <ul>
    *      <li><em>total</em> The total number of cells allocated from the heap.</li>
    *      <li><em>avail</em> The number of free cells in the AVSL. This count
    *          is inaccurate because of deferred evaluation of sublists cells.</li>
    *   </ul>
    *   <li><em>AVSLList</em> Pointer to the AVSL fragment list. Each successful
    *       heap allocation request causes the creation of a cell containing
    *       the highest byte address and the lowest byte address satisfying the
    *       request. The created cell is always the first cell allocated from
    *       the heap, so that if <em>delta</em> cells are request then
    *       <em>delta - 1</em> are available.</li>
    *   <li><em>highWater</em> High water mark: the largest address allocated from the heap.</li>
    *   <li><em>lowWater</em> Low water mark: the smallest address allocated from the heap.</li>
    *   <li>User Allocation Request Sizes</li>
    *   <ul>
    *      <li><em>alloc</em> Initial allocation size: The initial allocation of cells from the
    *          heap. <em>NOTE:</em> this is the number of cells not the number
    *          of bytes.</li>
    *      <li><em>delta</em> Delta allocation size: The number of cells to allocate from the
    *          heap when the initial allocation is exhausted. The delta allocation
    *          will be used each time the AVSL receives an allocation request
    *          and the AVSL is empty. If the delta allocation is zero and the
    *          initial allocation is exhausted an exception will be generated.</li>
    *          NOTE:this is the number of cells not the number of bytes.
    *   </ul>
    * </ul>
    */
   struct AVSLState {
      SlipHeader*   AVSLList;                                //!< List of sorted AVSL fragments
      SlipCellBase* top;                                     //!< Pointer to the first AVSL cell
      SlipCellBase* bot;                                     //!< Pointer to the last  AVSL cell
      SlipCellBase* highWater;                               //!< Highest AVSL address
      SlipCellBase* lowWater;                                //!< Lowest AVSL address
      ULONG         total;                                   //!< Total number of SLIP Cells
      ULONG         avail;                                   //!< Number of AVSL SLIP Cells
      ULONG         alloc;                                   //!< Initial cells to allocate from the heap
      ULONG         delta;                                   //!< Incremental cells to allocate fromthe heap
      ULONG         counter;                                 //!< Transaction counter
   private:

      /**
       * @brief Initialize the AVSL.
       * <p>This method is executed only to initialize the AVSL for processing.</p>
       * <p>Allocation of space is deferred until the initial space request by
       *    the user. The initial state sets up AVSL administration variables to
       *    require that the initial space allocation causes a heap demand for
       *    space.</p>
       * <p>The method ensures that the minimum allocation requirements
       *    for the initial and delta minumum heap data requests are set
       *    correctly. Errors in input are silently corrected. No check is made
       *    to ensure system maximums can satisfy the initial request.</p>
       * @param[in] alloc initial cells to allocated from the heap
       * @param[in] delta subsequent cells to allocate from the heap
       */
      void init(ULONG alloc, ULONG delta) {
         this->AVSLList = NULL;
         this->alloc = max(alloc, MINALLOC);
         this->delta = max(delta, MINALLOC);
         this->top = this->bot = NULL;
         this->total = 0;
         this->avail  = 0;
         this->highWater = (SlipCellBase*)0x00000000;
         this->lowWater  = (SlipCellBase*)0xFFFFFFFF;
         this->counter   = 0;
      }
   public:
      /**
       * @brief Default constructor for Available Space List (AVSL) management.
       * <p>Allocation is handled by @sa init(ULONG alloc, ULONG delta),
       *    The default constructor uses the SLIP minimum allocation sizes.</p>
       */
      AVSLState() {                                          //! avslState constructor
         init(MINALLOC, MINALLOC);
      }; // avslState

      /**
       * @brief Constructor for Available Space List (AVSL) management.
       * <p>Allocation is handled by @sa init(ULONG alloc, ULONG delta),
       *    The constructor uses user supplied allocation request sizes for the
       *    initial and additional heap demands.</p>
       * @param[in] alloc (ULONG) Initial cells to allocate from the heap
       * @param[in] total (ULONG) Incremental cells to allocate fromthe heap
       */
      AVSLState(ULONG alloc, ULONG total) {
         init(alloc, total);
      }; // AVLSState(ULONG alloc, ULONG total)
      
      /**
       * @brief Reset the AVSL State to its initial state.
       * 
       * <p>After reseting SLIP will no longer be operational. The user must
       * call the <i>slipInit</i> method to resume SLIP operations.</p>
       */
      void resetAVSLState() {
         this->AVSLList = NULL;
         this->alloc = 10000;
         this->delta = 10000;
         this->top = this->bot = NULL;
         this->total = 0;
         this->avail  = 0;
         this->highWater = (SlipCellBase*)0x00000000;
         this->lowWater  = (SlipCellBase*)0xFFFFFFFF;
         this->counter   = 0;
      }; // void resetAVSLState()
      /**
       * @brief Allows dynamic setting of the heap allocation sizes.
       * <p>After initial allocation the heap allocation requirements can be
       *    modified with this method.</p>
       * @param[in] alloc (ULONG) Initial cells to allocate from the heap
       * @param[in] delta (ULONG) Incremental cells to allocate from the heap
       */
      void setAlloc(ULONG alloc, ULONG delta) {
         this->alloc = max(MINALLOC, alloc);
         this->delta = max(MINALLOC, delta);
      }; // void setAlloc(ULONG alloc, ULONG total)

      /**
       * @brief Return a pretty-printed string for the AVSL state.
       * @return string pretty-printed string of the AVSL internal state
       */
      string toString() const {                            //! Pretty-print current state
          ULONG used = total - avail;
          stringstream ss(stringstream::in | stringstream::out);
          ss << "AVSL State: Total Cells   " << total << '\n'
             << "            Free  Cells   " << avail << '\n'
             << "            Used  Cells   " << used  << '\n'
             << "            Delta Alloc   " << alloc << '\n'
             << "            Init  Alloc   " << delta << '\n'
             << "            Cell  Size    " << ::cellSize << " bytes\n"
             << "            Transaction counter "         << counter << '\n'
             << "            High  Address 0x" << setw(0) << setfill('0') << hex << highWater << '\n'
             << "            Low   Address 0x" << setw(0) << setfill('0') << hex << lowWater
             << endl;
          return ss.str();
      }; // string toString() const
   }; // struct avslState

   /**
    * @brief SLIP AVSL state object.
    * <p>The AVSL State contains the allocation requirements for SLIP. Part of
    *    the state is the retention of the initial space allocation requirements
    *    and the incremental space allocation requirements. During instantiation
    *    of the AVSL State, default values are given for the allocation
    *    requirements.</p>
    * <p>Creating the AVSL state object does not allocate AVSL free cells. AVSL
    *    free cells are created at the first user demand for space.</p>
    * <p>The AVSL state object is created on the stack (not the heap). It is
    *    removed when the program terminates.</p>
    */
   static AVSLState avsl(10000, 10000);                                         //!< AVSL State with default allocation values

          /*************************************************
           *           Constructors & Destructors          *
           ************************************************/
   /**
    * @fn SlipCellBase::SlipCellBase()
    * @brief Construct a SLIP Cell.
    * <p>This constructor is accessed whenever a Slip Cell is created. It is
    *    responsible for distinguishing between Slip Cells created on the stack
    *    (temporaries) and Slip Cells taken from the AVSL (persistent). Temporaries
    *    have restrictions on use, they can not be put into a list or the AVSL.
    *    Persistent cells have no restriction on usage.</p>
    * <p>Distinguishing between temporary cells and persistent cells occurs because
    *    of an 'agreement' concerning cells removed from the AVSL. AVSL cells have
    *    the leftLink and the rightLink set to a known value, <em>UNDEFINED</em>.
    *    All Slip Cells which do not the leftLink and rightLink set to
    *    <em>UNDEFINED</em> are considered stack allocated temporaries. This is
    *    not guaranteed to work properly in all cases. It is possible that a stack
    *    based cell could have the preset values. This is deemed unlikely.</p>
    * <p>On exit the Slip Cell will have the following values for the leftLink and
    *    the rightLink:</p>
    * <ul>
    *    <li><em>null</em> for AVSL acquired persistent cells.</li>
    *    <li><em>TEMPORARY</em> for stack allocated temporary cells.</li>
    * </ul>
    * <p>Slip always creates stack allocated temporary cells to hold intermediate
    *    values in a computation. The user can create temporary cells for SlipDatum
    *    cells at the users convenience. Temporary cells are destroyed when the
    *    enclosing block unwinds (function return or block exit).</p>
    * <p>If a user created persistent cell is not put into a list, then the user
    *    must delete the cell. The SLIP kernel will delete the cell when the list
    *    is deleted (<em>deleteList()</em>) or when the user removes that cell from
    *    the list and deletes it.</p>
    * <p>Legal and illegal creation myths are:</p>
    * <tt>
    * |                 data declaration                 | link value | Description       |
    * | :----------------------------------------------- | :--------- | :---------------- |
    * | SlipSublist  sub;                                | illegal    | Compiler detected |
    * | SlipSublist  sub(Header);                        | illegal    | Compiler detected |
    * | SlipSublist *sub = new SlipSublist();            | null       |                   |
    * | SlipHeader   head;                               | illegal    | Runtime detected  |
    * | SlipHeader   head();                             | illegal    | Runtime detected  |
    * | SlipHeader  *head() = new SlipHeader();          | null       |                   |
    * | SlipDatum    datum;                              | TEMPORARY  |                   |
    * | SlipDatum    datum((type)value);                 | TEMPORARY  |                   |
    * ! SlipDatum*   datum = new SlipDatum();            | null       |                   |
    * | SlipDatum*   datum = new SlipDatum((type)value); | null       |                   |
    *
    * </tt>
    * @return SlipCell marked as persistent or temporary
    */

   /*********************************************************
    *               SLIP Protected Functions                *
   **********************************************************/

   /**
    *
    * @brief Add SLIP Cells to the AVSL.
    *
    * <p>Only cells originally removed from the AVSL can be returned to the AVSL.
    *    All cells being returned are checked to see that:</p>
    * <ul>
    *    <li>They are not on the runtime stack.</li>
    *    <li>They are not from the heap.</li>
    *    <li>They originated as a cell in the AVSL.</li>
    * </ul>
    * <p>Slip Cells are restricted to the following classes:</p>
    * <ul>
    *    <li>SlipSublist, A list reference to a list.</li>
    *    <li>SlipHeader. A list header. The header is considered as part of a list
    *        and not independent from the list.</li>
    *    <li>SlipDatum. Value containing slip cells.</li>
    *    <li>SlipReaderCell. Reader iterator memoize container.</li>
    * </ul>
    * <p>There are no other Slip cells. If it is not one of the above, it is not
    *    a Slip cell and no matter how hard you try, you will not be able to add
    *    it to the AVSL.</p>
    * <p>It is the responsibility of a cell destructor to call addAVSLCells. The
    *    destructor is not called by the base classes SlipCellBase or SlipCell. For
    *    SlipDatum and SlipSublist, only one cell is returned to the AVSL. For
    *    SlipHeader, the entire list is returned, for flush, and the entire list and
    *    the header cell is returned for deleteList(). For SlipReaderCell, one or more
    *    cells are returned.</p>
    * <p>In summary:</p>
    * <tt>
    * |   delete operation  | AVSL recovered cells                      |
    * | :------------------ | :---------------------------------------- |
    * | delete datum        | A single cell                             |
    * | delete sublist      | A single cell                             |
    * | header.flush()      | All cells in the list                     |
    * | header.deleteList() | The header cell and all cells in the list |
    * | delete reader       | All SlipReaderCells memoized
    * </tt>
    * <p>The start pointer references the initial cell to be returned, and the
    *    stop pointer references to the last cell to be returned. If they are
    *    the same then only one cell is returned. If they are different then the
    *    input must represent a singly, right linked, list whose last member is
    *    the stop pointer. Anything else will irramedially damage the integrity of
    *    the AVSL.
    * </p>
    * <p>In operation, the start cell will follow the last cell in the AVSL and the
    *    stop cell becomes the new list bottom. The AVSL state will be changed:
    * </p>
    * <ul>
    *    <li><b>avsl.top</b> points to <b>start</b> if the AVSL was empty, otherwise is
    *        is unchanged.</li>
    *    <li><b>avsl.bot</b> points to <b>stop</b>.</li>
    *    <li><b>avsl.avail</b> is incremented for each cell added to the AVSL. Sublists
    *        are not evaluated, therefore, if a sublist references a list that
    *        would have been returned to the AVSL, the list cells will not be
    *        recovered and avsl.avail will not be incremented.</li>
    * </ul>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E3010</b> Attempt to recover a range of cells where at least one cell is a temp</li>
    *    <li><b>E3018</b> Unable to delete a Slip Cell already deleted</li>
    * </ul>
    *
    * @param[in] start: Pointer to the first cell to be returned to the AVSL
    * @param[in] stop:  Pointer to the last  cell to be returned to the AVSL
    */
   void SlipCellBase::addAVSLCells( SlipCellBase* start                         // pointer to first cell
                                   , SlipCellBase* stop) {                       // pointer to last cell
     if (isTemp(start) || isTemp(stop)) {
        postError(__FILE__, __LINE__, SlipErr::E3010, "addAVSLCells", "", "", *start, *stop);
     }
     stop->rightLink = NULL;
     for(SlipCellBase* link = start; link != NULL; link = getRightLink(*link)) {
         avsl.counter++;
         if (avslHistoryFlag) {
           SlipOp* op = *(SlipOp**)(link->getOperator());
           cout << right << setw(8) << avsl.counter << setw(9) << avsl.avail
                << " delete " << op ->dump((SlipCell&)*link) << endl;
         }
         if (link->isDeleted()) {
           postError(__FILE__, __LINE__, SlipErr::E3018, "addAVSLCells", "", "", *link);
           (link->leftLink)->rightLink = link->rightLink;
        } else {
           avsl.avail++;
           link->leftLink = (SlipCellBase*)UNDEFDATA;
        }
     }
     avsl.top   = (avsl.top == NULL)? start: avsl.top;                          // if AVSL empty before add, change top
     avsl.bot   = (avsl.bot == NULL)? NULL: avsl.bot->rightLink = start;        // if AVSL not empty link bottom
     avsl.bot = stop;
     stop->leftLink = (SlipCellBase*)UNDEFDATA;
   }; // SlipCellBase::addAVSLCells( size_t size, SlipCellBase* start, SlipCellBase* stop)

   /**
    * @brief Turn logging of cell posting/removal from the AVSL.
    * <p>Turns output logging of AVSL events on/off. there is an incrementing
    *    counter for AVSL events. This counter is incremented every time an
    *    AVSL action occurs. It is output along with the history.</p>
    *
    * @param[in] onOFF <b>true</b> turns logging on
    */
   void SlipCellBase::avslHistory(bool onOFF) {                                 // true turns history on
      avslHistoryFlag = onOFF;
   }; // void SlipCellBase::avslHistory(bool onOFF)

   /**
    * @brief Return a string with the pretty-printed left and right link.
    * @return string: pretty-printed left and right SLIP Cell link
    */
   string  SlipCellBase::dumpLink() const { return dumpLink(this); }            // dump the left/right link values

   /**
    * @brief Return a string with the pretty-printed left and right link.
    * @return string: pretty-printed left and right SLIP Cell link
    */
  string   SlipCellBase::dumpLink(SlipCellBase const * const X) {
     stringstream pretty;
     static const string null = "NULL      ";
     pretty << hex << setfill('0') << setw(8) << X << "::";
     if (X->isTemp()) {
        pretty << "<TEMPORARY TEMPORARY> ";
     } else {
        pretty << "<";
        if (getLeftLink(*X) == NULL)  pretty << null;
        else                    pretty << hex << getLeftLink(*X);
        if (getRightLink(*X) == NULL) pretty << ' ' << null;
        else                    pretty << " " << hex << getRightLink(*X);
        pretty << ">";
     }
     return pretty.str();
  }; //SlipCellBase::dumpLink(SlipCellBase const * const X)

  /**
   *
   * @brief Allocates and initializes heap space for the AVSL list.
   * <p>A demand for a Slip cell on an empty AVSL causes a cal to this method
   *    to retrieve additional space from the heap. The retrieved space is
   *    formated into Slip cells and linked into the AVSL list..</p>
   * <p>A formatted Slip cell placed on the AVSL list consists of:</p>
   * <ul>
   *  <li><b>leftLink</b> set to <em>UNDEFDATA</em>.</li>
   *  <li><b>rightLink</b> points to the next cell. The last link is <em>null</em>.</li>
   *  <li><b>operator</b> points to <em>SlipUndefinedOp</em></li>
   *  <li><b>datum</b> contains <em>(long)0</em>, <em>(long)0</em>.</li>
   * </ul>
   * <p>On successful execution the AVSL state is changed to: </p>
   * <ul>
   *  <li><b>avsl.top</b> pointer to the first formatted cell in the AVSL list.</li>
   *  <li><b>avsl.bot</b> pointer to the last formatted cell in the AVSL list</li>.</li>
   *  <li><b>avsl.avail</b> number of cells retrieved.</li>
   * </ul>
   *
   * <p><b>ERRORS</b></p>
   * <ul>
   *    <li><b>E3008</b> Unable to get # Slip Cells from the heap</li>
   *    <li><b>E3009</b> Unknown exception</li>
   * </ul>
   *
   * @param[in,out] lowWater the lowest (smallest) address of the allocation
   * @param[in,out] highWater the highest (largest) address of the allocation
   */
   void SlipCellBase::getAVSLSpace(SlipCellBase*& lowWater, SlipCellBase*& highWater) {
       if (avsl.alloc == 0) {
          cout << "E3008: Unable to get " << avsl.alloc
               << " Slip Cells from the heap " << avsl.toString() << endl;
          if (!firstTimeOnlyTime) {
             cout << "       Hint. This error occurred before initialization and may be because a SlipCell was\n"
                  << "       created in file or global scope." << endl;
          }
          exit(1);
       }
       firstTimeOnlyTime = true;
       try {
           ULONG i = 0;
           SlipCellBase* array = (SlipCellBase*) new char[cellSize * avsl.alloc]();
           lowWater  = (SlipCellBase*)array;
           highWater = (SlipCellBase*)array + avsl.alloc - 1;

           /**
            * Continue space setup
            */
           avsl.top    = &array[0];
           avsl.total += avsl.alloc;
           avsl.avail += avsl.alloc;
           for (; i < avsl.alloc - 1; i++ ) {
               array[i].setOperator((void*)undefinedOP);
               array[i].rightLink = &array[i + 1];
               array[i].leftLink  = (SlipCellBase*)UNDEFDATA;
           }
           array[i].setOperator((void*)undefinedOP);
           array[i].rightLink = NULL;
           array[i].leftLink  = (SlipCellBase*)UNDEFDATA;
           avsl.bot = &array[i];
       } catch (bad_alloc& ba) {
          cout << "E3008: Unable to get " << avsl.alloc
               << "Slip Cells from the heap " << avsl.toString() << endl;
           throw(ba);
       } catch (...) {
          cout << (SlipErr::slipMessage[SlipErr::E3009]).getMessage() << endl;
          exit(1);
       };
   }; // void SlipCellBase::getAVSLSpace()

   /**
    * @fn SlipCellBase::getData(SlipCellBase const * const X)
    * @brief Return a pointer to the current datum field.
    * @return Return a pointer to the current datum field
    */

   /**
    * @fn SlipCellBase::getDatumCopy(const void* X)
    * @brief Return a copy of the data field in X.
    * <p>A reference is made to the static cell characteristics object.
    *    Although the static cell characteristics for each cell are different,
    *    the location of the object pointer in each cell is the same. As a
    *    convenience, the input cell is assumed to be a SlipDatum cell to
    *    allow retrieval of this pointer.</p>
    * <p>The meaning of copy depends on the type of X.</p>
    * <p>To eliminate cycles and cross linkages in the header files, the input
    *    argument is a void pointer rather than a pointer to the actual object
    *    type.</p>
    * @param[in] X a pointer to a Slip cell object
    * @return Return a copy of the data field in X
    */
   sAllData SlipCellBase::getDatumCopy(const void* X) {                          // Return valid datum
     SlipDatum& cell = *(SlipDatum*)X;
     return ((SlipOp*)*(cell.getOperator()))->getDatumCopy(cell);
   }; // sAllData SlipCellBase::getDatumCopy(const SlipDatum& X)

   /**
    * @brief inserts the second cell to the left of the first cell.
    * <p>This is an internal utility method. No checks are made on the validity
    *    of the request. It is assumed that the insertion is always successful
    *    and that both input cells are from the AVSL.</p>
    * @param[in,out] cell source cell
    * @param[in,out] X object cell inserted to the left of the source cell
    */
   void SlipCellBase::insertFragmentLeft(SlipCellBase* cell, SlipCellBase* X) { // Insert X to current cell left
      X->setRightLink(cell);
      X->setLeftLink(cell->getLeftLink());
      setRightLink(cell->getLeftLink(), X);
      cell->setLeftLink(X);
   }; // void SlipCellBase::insertFragmentLeft(SlipCellBase* cell, SlipCellBase* X)

   /**
    * @brief inserts the second cell to the right of the first cell.
    * <p>This is an internal utility method. No checks are made on the validity
    *    of the request. It is assumed that the insertion is always successful
    *    and that both input cells are from the AVSL.</p>
    * @param[in,out] cell source cell
    * @param[in,out] X object cell inserted to the right of the source cell
    */
   void SlipCellBase::insertFragmentRight(SlipCellBase* cell, SlipCellBase* X) { // Insert X to current cell right
      X->setLeftLink(cell);
      X->setRightLink(cell->getRightLink());
      setLeftLink(cell->getRightLink(), X);
      cell->setRightLink(X);
   }; // void SlipCellBase::insertFragmentRight(SlipCellBase* cell, SlipCellBase* X)

   /**
    * @brief returns <b>true</b> if the input cell is from the AVSL.
    * <p>The input cell is checked to see if it is from any of the space
    *    fragments retrieved from the heap. If the input cell is not from the
    *    heap, the method returns <b>false</b>.</p>
    * @param[in] X Slip cell being checked
    * @return <b>true</b> cell is from the AVSL <b>false</b> otherwise
    */
   bool SlipCellBase::isAVSL(const SlipCellBase* X)  {                          // Cell is in AVSL
      bool flag = false;
      SlipCellBase* adr = const_cast<SlipCellBase*>(X);
      if ((adr >= avsl.lowWater) && (adr <= avsl.highWater)) {  // quick check
         SlipCellBase* link = avsl.AVSLList->getRightLink();
         while((adr > link->datum.fragment.highWater) && !link->isHeader())
            link = link->getRightLink();
         flag = (!link->isHeader() && (adr <= link->datum.fragment.highWater));
      }
      return flag;
   }; // bool SlipCellBase::isAVSL(const SlipCellBase* X)

   /**
    * @brief insert a new AVSL fragment into the sorted AVSL fragment list.
    * <p>Insert a new fragment into the space fragment list and setup the high
    *    water/low water address marks.</p>
    * <p>The search loop looks at a list of AVSL memory fragments and places
    *    the new fragment in sorted order within the list. Loop invariants are:
    * </p>
    * <ol>
    *    <li>There is at least one space fragment on the list</li>
    *    <li>Each fragment is strictly ordered with the low water mark
    *        &lt; the high water mark.</li>
    *    <li>Each fragment is strictly ordered with respect to the following,
    *        preceding fragment. That is:
    *        <p><tt>high water[i-1] &lt; low water[i] &lt; high water[i] &lt; low water[i+1]</tt><p>
    *    </li>
    * </ol>
    * <p>If two fragments are conjoined (one fragment immediately following or
    *    preceding the other) then the old fragment list element is expanded.</p>
    * <p>The AVSL state is altered:</p>
    * <ul>
    *    <li><b>avsl.highWater</b> contains the highest byte address in the fragment list</li>
    *    <li><b>avsl.lowWater</b> contains the lowest byte address in the fragment list</li>
    * </ul>
    *
    * @param[in,out] lowWater the lowest (smallest) address of the allocation
    * @param[in,out] highWater the highest (largest) address of the allocation
    */
   void SlipCellBase::newFragment( SlipCellBase* lowWater                       // Create a new AVSL fragment history
                                 , SlipCellBase* highWater) {
      SlipDatum* datum = new SlipDatum((ULONG)0);
      datum->datum.fragment.lowWater = lowWater;
      datum->datum.fragment.highWater = highWater;
      if (highWater < avsl.lowWater) {                                          // the current allocation precedes the lowest addressed fragment
         if ((highWater + 1) != avsl.lowWater) {                                // attempt to expand an existing fragment
            insertFragmentRight(avsl.AVSLList, datum);
         } else
            delete datum;
         avsl.lowWater = lowWater;
      } else if (lowWater > avsl.highWater) {                                   // the current allocation follows the lowest addressed fragment
         if ((lowWater - 1) != avsl.highWater) {                                // attempt to expand and existing fragement
            insertFragmentLeft(avsl.AVSLList, datum);
         } else
            delete datum;
         avsl.highWater = highWater;
      } else {                                                                  // the current allocation is between the highes and lowest address
         SlipCellBase* link = avsl.AVSLList->rightLink;
         while((!link->isHeader()) && lowWater < link->datum.fragment.lowWater);         // do a sequential search
         if ((lowWater - 1)== link->datum.fragment.highWater) {
            link->datum.fragment.highWater = highWater;
            delete datum;
         } else if ((highWater + 1) == (link->getRightLink())->datum.fragment.lowWater) {
            (link->getRightLink())->datum.fragment.lowWater = lowWater;
            delete datum;
         } else {
            insertFragmentRight(link, datum);
         }
      }
   }; // void SlipCellBase::newFragment(ULONG lowWater, ULONG highWater)

   /**
    *
    * @brief allocate and return a pointer to an AVSL Slip cell.
    * <p>Remove a cell from the AVSL, set the cell links (right and left),
    *    to <em>UNDEFDATA</em>, the static type characteristics pointer,
    *    <em>operator</em>, to point to <em>SlipUndefinedOp</em>, and clear
    *    the datum value.</p>
    * <p>If the AVSL is empty, retrieve a allocation from the heap and insert
    *    a fragment entry into the fragment list, and then retrieve and
    *    initialize a Slip cell.</p>
    * <p>Ignore any Slip cell whose address is the same as <em>UNDEFDATA</em>.
    *    This creates a hole in the AVSL which is one cell deep.</p>
    * <p>If the retrieved cell was originally a SlipSublist cell then delete
    *    the referenced list (reference.deleteList()). The deletion method will
    *    reduce the reference count, and if zero, will inser the list header
    *    and all list cells into the AVSL. If the reference count is not zero,
    *    then deleteList() will do nothing.</p>
    * <p>Sublist deletion is a deferred action. The time of recovery of a
    *    deleted list is deferred until the time that the referencing sublist
    *    is removed from the AVSL. This deferral should cause no operational
    *    issues during normal use.</p>
    * <p>Update the AVSL state as required:</p>
    * <ul>
    *    <li><b>avsl.avail</b> is decremented.</li>
    *    <li><b>avsl.top</b> is adjusted to point to the next available cell.</li>
    * </ul>
    * <p>In summary:</p>
    * <ol>
    *    <li>If the AVSL is empty, a new list of cells retrieved from the heap.</li>
    *    <li>If the top AVSL cell is a Slip header, datum or reader cell, it is used.</li>
    *    <li>If the top AVSL cell is a sublist cell the referenced header is
    *        deleted (<em>header->deleteList()</em>)</li>
    *    <li>A return cell is initialized.</li>
    * </ol>
    * <p><b>ERRORS</b></p>
    * <ul>
    *    <li><b>E2010</b> Can not use header cell Header in sublist is null</li>
    *    <li><b>E3018</b> Can not use header cell Header in sublist is not a header</li>
    * </ul>
    *
    * @param[in] size ignored
    * @return a SLIP Cell
    */
   void * SlipCellBase::operator new(size_t size) {    // Create a new SLIP cell
      SlipCellBase* link = NULL;
      do  {
        if (avsl.top == NULL) {
           SlipCellBase* lowWater;
           SlipCellBase* highWater;
           SlipCellBase::getAVSLSpace(lowWater, highWater);
           newFragment(lowWater, highWater);
        }
        link = (SlipCellBase*)avsl.top;
        avsl.top = avsl.top->rightLink;
        avsl.avail--;
      } while(link == TEMPORARY );

      avsl.bot = (avsl.top == NULL)? NULL: avsl.bot;                            // set both top & bot if AVSL empty

      SlipOp* op = *(SlipOp**)link->getOperator();

      avsl.counter++;
      if (avslHistoryFlag)
         cout << right << setw(8) << avsl.counter << setw(9) << avsl.avail
              << " new    " << op->dump((SlipDatum&)*link) << endl;
      if (op->isSublist()) {
         SlipHeader* header = *(SlipHeader**)getSublistHeader(*link);
         if (header == NULL) {
            postError(__FILE__, __LINE__, SlipErr::E2010, "operator new", "", "Header in sublist is null", *link);
         } else if (!header->isHeader()) {
            postError(__FILE__, __LINE__, SlipErr::E2010, "operator new", "", "Header in sublist is not a header.", *link, *header);
         } else {
           header->deleteList();
         }
      }

          /* Zero Cell */
      remove(*link);                                                            // Delete SlipPointer datum
      link->setOperator((void*)undefinedOP);
      link->rightLink = link->leftLink = (SlipCellBase*)UNDEFDATA;
      link->resetData();
      return link;
   }; // void * SlipCellBase::operator new(size_t size)

   /**
    * @fn SlipCellBase::operator delete(void* ptr)
    * @brief Bypasses the standard delete - do nothing.
    * <p>The class destructors for SlipSublist, SlipDatum, SlipHeader, and
    *    SlipReader are responsible to determining whether a slip cell is to be
    *    returned to the AVSL. The destructors perform 'deletion' by calling
    *    addAVSLCells when a cell (or cells) is to be deleted.</p>
    * <p></p>
    * @param[in] ptr: pointer to cell to delete
    */

   /**
    * @brief pretty-print the AVSL state and AVSL list.
    * <p>The AVSL state variables are output followed by a dump of each cell
    *    in the AVSL list. The AVSL list cell dump includes an ordinal number
    *    representing the cell position in the AVSL list, with the first
    *    position being output as one ('1').</p>
    * <p>Output of the AVSL list is in order of appearance within the list.
    *    Each cell is analyzed before output and diagnostics messages are
    *    issued as required. Identifying which fragment contains the cell
    *    is not done.</p>
    * <p><b>WARNINGS</b></p>
    * <ul>
    *    <li>"****** Left link is not 0xdeadbeef"</li>
    *    <li>"****** Sublist header pointer is NULL"</li>
    *    <li>"****** Sublist header pointer does not point to a header"</li>
    *    <li>"****** Header reference count > 0. Header is still active"</li>
    *    <li>"****** Number of cells in AVSL does not agree with free space count"
    * "         count "
    * "         avail ";</li>
    * </ul>
    * @param[in] str user caption printed as a heading
    */
   void SlipCellBase::printAVSL(string str) {                                   // pretty-print the AVSL state and list
      SlipCellBase* link = avsl.top;
      ULONG count = 0;
      if (str != "") cout << str << endl;
      printState();
      cout << '\n';
      while((link != NULL) && (count < avsl.avail + 2)) {
         count++;
         SlipOp* op  = *(SlipOp**)link->getOperator();
         string dump = op->dump(*(SlipDatum*)link);
         cout << setfill(' ') << dec
              << setw(5) << count << "  " << dump << endl;
         if (link->getLeftLink() != UNDEFDATA) {
            cout << "****** Left link is not 0x"
                 << setfill(' ') << setw(8) << hex
                 << UNDEFDATA << endl;
         }
         if (link->isSublist()) {
            SlipHeader* header = *(SlipHeader**)getSublistHeader(*link);
            if (header == NULL) {
              cout << "****** Sublist header pointer is NULL" << endl;
            } else if (!header->isHeader()) {
              cout << "****** Sublist header pointer does not point to a header" << endl;
            }
         }
         if (link->isHeader()) {
            if (*link->getHeadRefCnt() > 0) {
              cout << "****** Header reference count > 0. Header is still active" << endl;
            }
         }
         if ((count % 5) == 0) cout << endl;
         link = link->getRightLink();
      }
      if (count != avsl.avail) {
        cout << "****** Number of cells in AVSL does not agree with free space count" << endl;
        cout << "         count " << count << endl;
        cout << "         avail " << avsl.avail << endl;
      }
      cout << endl << endl;
   }; // void SlipCellBase::printAVSL()

   /**
    * @brief Prints the size (in bytes) of each class inheriting SlipCellBase.
    */
   void SlipCellBase:: printClassSizes() {                                      // Output the size of each class
      cout << "Class sizes in bytes" << '\n'
           << "  SlipCellBase Size  " << SlipCellBaseSize << '\n'
           << "  SlipCell Size      " << SlipCellSize     << '\n'
           << "  SlipDatum Size     " << SlipDatumSize    << '\n'
           << "  SlipDatum Size     " << SlipHeaderSize   << '\n'
           << "  SlipReader Size    " << SlipReaderSize   << '\n'
           << "  maximum class Size " << cellSize << " rounded up to a long boundary"
           << endl << endl;
   }; // void SlipCellBase::printClassSizes()

   /**
    * @brief Prints the contents of each fragment in the fragment list.
    */
   void SlipCellBase::printFragmentList(string str) {                           // Format and output the fragment list
      int i = 0;
      SlipCellBase* link = avsl.AVSLList->rightLink;
      /*
       * Fragment ### 0x######## 0x########  ####### ######
       */
      cout << str <<'\n'
           << "AVSL start 0x" << setw(8) << hex << avsl.top
           << "  stop 0x" << setw(8) << hex << avsl.bot << '\n'
           << "Fragment   # low Water  high Water     size    cells" << endl;
      for(; !link->isHeader(); link = link->rightLink, i++) {
         ULONG lowWater = link->datum.allData.data1;
         ULONG highWater= link->datum.allData.data2;
         ULONG size     = highWater - lowWater + 1;
         cout << "Fragment " << right << setw(3) << i
              << " 0x" << setw(8) << setfill('0') << hex << lowWater
              << " 0x" << setw(8) << setfill('0') << hex << highWater
              << "  "  << setw(7) << setfill(' ') << setbase(10) << right << size
              << "  "  << setw(7) << setfill(' ') << setbase(10) << right << (size/cellSize)
              << endl;
      }
      cout << " Total Number of Fragments " << i << endl << endl;
   }; // void SlipCellBase::printFragmentList()

   /**
    * @brief Print the entire memory contents
    * <p>The Hail Mary Pass: Print all memory in each fragment by fragment. Do
    *    not separate cells in the AVSL from other cells. The link values and
    *    their interpretation is:
    * <ul>
    *    <li><b>leftLink</b> = 0xDeadBeef The cell is in the AVSL. Only one
    *         cell can have a rightLink of NULL (0x0). This is the last cell
    *         in the AVSL. All other cells point to the next cell.</li>
    *    <li><b>leftLink</b> = 0xFFFFFFFF and <b>rightLinkt</b> = 0xFFFFFFFF.
    *        Cell is a temporary cell created on the runtime stadk. This is
    *        an error if seen in this listing. No cell in any fragment can
    *        have the rightLink or the leftLink = 0xFFFFFFFF.</li>
    *    <li><b>leftLink</b> = NULL (0x0)> and the <b>rightLink</b> = NULL
    *        (0x0). This is an unlinked cell not assigned to any list. Until
    *        the cell is put onto a list or returned to the AVSL, the links
    *        will be null.
    *        @note if only the <b>rightLink</b> = NULL then this indicates
    *        that this cell is the last cell in the AVSL.</li>
    *    <li><b>header cell</b> a header cell can never be a temporary. If
    *        the <b>leftLink</b> is 0xDeadBeef then the header cell is in
    *        the AVSL. If the <b>rightLink</b> is NULL then it is the last
    *        cell in the AVSL and the <b>leftLink</b> must be 0xDeadBeef.</li>
    *    <li><b>sublist cell</b> must have a valid header cell and the header
    *        cell can not be in the AVSL. This condition is invariant and
    *        matters nought whether the <b>sublist cell</b> is in the aVSL
    *        or is 'live'. If the <b>leftLink</b> is 0xDeadBeef then the
    *        <b>rightLink</b> can be NULL if the cell is the last one in
    *        the AVSL.</li>
    *    <li><b>reader cell</b> can never be temporary. If the <b>leftLink</b>
    *        is 0xDeadBeef then the <<b>rightLink</b> can be NULL if this is
    *        the last cell in the AVSL.</li>
    * </ul>
    * @param str (string) user caption
    */
   void SlipCellBase::printMemory(string str) {                                 // Dump all ememory
      SlipCellBase* link = avsl.AVSLList->rightLink;                             //!< Fragment List Pointer
      int i = 0;                                                                 //!< Fragment number
      cout << str <<'\n'
           << "AVSL start 0x" << setw(8) << hex << avsl.top
           << "      stop 0x" << setw(8) << hex << avsl.bot << '\n' << endl;
      for(; !link->isHeader(); link = link->rightLink, i++) {
         SlipCellBase* lowWater = link->datum.fragment.lowWater;
         SlipCellBase* highWater= link->datum.fragment.highWater;
         ULONG size     = highWater - lowWater + 1;
         cout << "Fragment " << right << setw(3) << i
              << " 0x" << setw(8) << setfill('0') << hex << lowWater
              << " 0x" << setw(8) << setfill('0') << hex << highWater
              << "  "  << setw(7) << setfill(' ') << setbase(10) << right << size
              << "  "  << setw(7) << setfill(' ') << setbase(10) << right << (size/cellSize)
              << endl;
        for (SlipCell* adr = (SlipCell*)lowWater;                               // output all cells in the fragment
             adr <= (SlipCellBase*)highWater;
             adr++) {
            SlipOp* op = *(SlipOp**)link->getOperator();
            cout << "  " << op->dump(*adr) << endl;
        }
      }

   }; // void SlipCellBase::printMemory(string str = "")

   /**
    * @brief Prints the AVSL state.
    * @param[in] str caption output with the content
    */
   void SlipCellBase::printState(string str) {                                  // Output the AVSL state
      if (str != "") cout << str << endl;
      cout << avsl.toString();
   }; // void SlipCellBase::printState()

   /**
    * @brief deletes a pointer data object.
    * <p>String and user supplied data (pointer) are deleted separately from
    *    the [SlipDatum] cell. The cell is maintained by the SLIP kernel. The
    *    referenced pointer data [Slip::Pointer] is controlled by the user. The
    *    user must specify under what conditions the object pointed to by the
    *    data pointer will be deleted.</p>
    * <p>At the time of creation of a SlipDatum(STRING) cell, the user provides
    *    deletion information by explicitly or by default referencing either
    *    [SlipStringConst] or [SlipStringNonConst]. At the time of creation of a
    *    user supplied object pointer, the user is required to supply a pointer
    *    to a class conforming inheriting from [Slip::Pointer]. In both these
    *    cases, when the [SlipDatum] cell is deleted the SLIP kernel makes an
    *    explicit call to the provided object deletion method conforming to the
    *    virtual remove method in [Slip::Pointer].</p>
    * <p>The casting of the input SlipCellBase to a SlipDatum will work for all
    *    classes. Reference to an invariant class characteristic is made through
    *    the operator pointer contained in the Slip cell. The invariant methods
    *    depend on the input being a SlipDatum object.</p>
    *
    * @param[in] X reference to a Slip cell
    */
   void SlipCellBase::remove(const SlipCellBase& X) {                           // Perform Data defined datum delete
      SlipOp* op = (SlipOp*)(X.operation);
      SlipDatum& Y = (SlipDatum&)X;
      op->remove(Y);
   }; // void SlipCellBase::remove(const SlipCellBase* X)

   /**
    * @fn SlipCellBase::setLeftLink(SlipCellBase* leftLink)
    * @brief Sets the left link of the current Slip cell.
    *
    * @param[in] leftLink pointer to an object of type SlipCellBase
    * @return the original leftLink pointer
    */
  
  /**
   * @fn SlipCellBase::setLeftLink(SlipCellBase* X, SlipCellBase* leftLink)
   * @brief Sets the left link of the input Slip cell to the input left link.
   *
   * @param[in,out] X pointer to the cell being changed
   * @param[in] leftLink pointer to an object of type SlipCellBase
   * @return the original leftLink pointer
   */

  /**
   * @fn SlipCellBase::setOperator(SlipCellBase* X, void* const OP)
   * @brief Replace the operation field of the input cell.
   * <p>A SLIP core function can set the operation field in a cell to a new one.
   *    The operation field defines legitimate arithmetic, logical, and bit
   *    operations, and Slip cell type invariants.</p>
   * <p>The operations associated with the Datum field in a cell are changed every
   *    time that the cell datum field is changed or the Slip cell type is changed.
   *    The operations are specific to the data type. </p>
   * <p>The SlipOP class is an abstract class at the root of all operations. It
   *    contains pure abstract methods for each operation.</p>
   * @see SlipOP
   * @param[in,out] X Slip cell
   * @param[in] OP: object to be used as the operation object
   * @return original cell operator pointer.
   */

  /**
   * @fn SlipCellBase::setOperator(void* const OP)
   * @brief Replace the operation field and return the cell's original value.
   * <p>A SLIP core function can set the operation field in a cell to a new one.
   *    The operation field defines legitimate arithmetic, logical, and bit
   *    operations, and Slip cell type invariants.</p>
   * <p>The operations associated with the Datum field in a cell are changed every
   *    time that the cell datum field is changed or the Slip cell type is changed.
   *    The operations are specific to the data type. </p>
   * <p>The SlipOP class is an abstract class at the root of all operations. It
   *    contains pure abstract methods for each operation.</p>
   * @see SlipOP
   * @param[in] OP: object to be used as the operation object
   * @return original cell operator pointer.
   */

   /**
    * @fn SlipCellBase::setRightLink(SlipCellBase* rightLink)
    * @brief Sets the right link field of the current Slip cell.
    *
    * @param[in] rightLink pointer to an object of type SlipCellBase
    * @return the original rightLink pointer
    */

   /**
    * @fn SlipCellBase::setRightLink(SlipCellBase* X, SlipCellBase* rightLink)
    * Sets the right link field of the input Slip cell.
    *
    * @param[in] X (SlipCellBase*) cell to be changed
    * @param[in] rightLink pointer to an object of type SlipCellBase
    * @return the original rightLink pointer
    */

  /**
   * @fn SlipCellBase::slipDump(string name)
   * @brief Uglify a Slip cell left/right link and operator pointer.
   * <p>Format the left/right link and operator pointer as hex numbers.</p>
   * @param[in] name dump caption
   * @return string representing the uglified cell
   */

  /**
   * @brief Uglify a Slip cell left/right link and operator pointer.
   * <p>Format the left/right link and operator pointer as hex numbers.</p>
   * @param[in] X reference to cell to be uglified
   * @param[in] name dump caption
   * @return string representing the uglified cell
   */
  string SlipCellBase::slipDump(string name, SlipCellBase& X) {
     stringstream pretty;
     pretty << name << " "
            << dumpLink(&X) << " ["
            << hex << setfill('0') << setw(8)
            << *getOperator(&X) << "] ";
     return pretty.str();
  }; // string slipDump(string name, SlipCellBase& X)
  
  /**
   * @brief Restore SLIP to its initial state.
   * 
   *  <p>SLIP must be initialized with <i>SlipCellBase::slipInit()</i> if 
   *     continued operations are required. This method deletes all fragments
   *     and initializes all internal variables. When the method is complete,
   *     there will be no memory allocated for SLIP cells and the SLIP API
   *     will not be operational.</p>
   */
   void SlipCellBase::deleteSlip() {                                            // Reinitialize SLIP to its pristine state
      avslHistoryFlag = false;                                                  // history flag <em>true</em> when AVSL output wanted
      firstTimeOnlyTime = false;                                                // Singleton flag for SLIP setup
           /**********************************/
           /*      Delete All Fragments      */
           /**********************************/
      SlipCellBase* link = avsl.AVSLList->getRightLink();
      while(!link->isHeader()) {
         SlipCellBase* adr = link;
         link = link->getRightLink();
         delete adr;
      }
      delete link;
      avsl.resetAVSLState();
   }; // void SlipCellBase::deleteSlip()

  /**
   * @brief One time, only time Slip initialization using defaults.
   * <p>Sets the inial allocation to 10,000 Slip cells and incremental
   *    allocations to 10,000 Slip cells.</p>
   */
  void   SlipCellBase::slipInit() {                                        // Initialize SLIP system
     slipInit(MINALLOC, MINALLOC);
  }; // void   SlipCellBase::slipInit()

   /**
    * @brief One time, only time Slip initialization using user provided values.
    * <p>All error handling is silent.</p>
    * <p>Sets the one time, only time flag, initializes the initial and delta
    *    heap allocation sizes. Gets the initial heap allocation and initializes
    *    the AVSL.</p>
    * <p>Initializes the header for the fragment table and inserts the first
    *    fragment to represent the initial allocation.</p>
    *
    * @param[in] alloc number of cells in the initial allocation
    * @param[in] delta number of cells in successive allocations
    */
   void   SlipCellBase::slipInit(ULONG alloc, ULONG delta) {                // Initialize SLIP system and allocation resources
      if (firstTimeOnlyTime) return;
      firstTimeOnlyTime = true;
      avsl.setAlloc(alloc, delta);
      getAVSLSpace(avsl.lowWater, avsl.highWater);
      /*
       * Initialize space fragment list and insert first fragment
       */
      avsl.AVSLList = new SlipHeader();
      SlipDatum* link = new SlipDatum((ULONG)0);
      link->datum.fragment.lowWater  = avsl.lowWater;
      link->datum.fragment.highWater = avsl.highWater;
      insertFragmentRight(avsl.AVSLList, link);
   }; // void   SlipCellBase::slipInit(ULONG alloc, ULONG delta)

   /**
    * @brief Output the system info
    * <p>All things known about the system are output</p>
    * <ul>
    *    <li>Size of bools, chars, ints, etc.</li>
    *    <li>Size of data within a SLIP cell.</li>
    *    <li>Size of SLIP cells.</li>
    *    <li>AVSL properties.</li>
    * </ul>
    */
   void SlipCellBase::sysInfo(ostream& out) {                                   // Output the SLIP system info
      out << "Size of Atomic Types in bytes" << '\n'
          << "  bool      "  << setw(6) << setfill(' ') << sizeof(bool)   << '\n'
          << "  CHAR      "  << setw(6) << setfill(' ') << sizeof(CHAR)   << '\n'
          << "  UCHAR     "  << setw(6) << setfill(' ') << sizeof(UCHAR)  << '\n'
          << "  LONG      "  << setw(6) << setfill(' ') << sizeof(LONG)   << '\n'
          << "  ULONG     "  << setw(6) << setfill(' ') << sizeof(ULONG)  << '\n'
          << "  DOUBLE    "  << setw(6) << setfill(' ') << sizeof(DOUBLE) << '\n'
          << "  PTR       "  << setw(6) << setfill(' ') << sizeof(PTR)    << '\n'
          << "  void*     "  << setw(6) << setfill(' ') << sizeof(void *) << '\n' << '\n';

      out << "Slip Data Sizes in bytes" << '\n'
          << "  ReadData       " << sizeof(sReadData)    << '\n'
          << "  Sublist        " << sizeof(sSublist)     << '\n'
          << "  Header         " << sizeof(sHeadData)    << '\n'
          << "  Reader         " << sizeof(sReadData)    << '\n'
          << "  AllData        " << sizeof(sAllData)     << '\n'
          << "  Datum          " << sizeof(Data)         << '\n'
          << "  FragmentData   " << sizeof(sFragmentData) << endl << endl;

      out << "Class sizes in bytes" << '\n'
          << "  SlipCellBase   " << SlipCellBaseSize << '\n'
          << "  SlipCell       " << SlipCellSize     << '\n'
          << "  SlipDatum      " << SlipDatumSize    << '\n'
          << "  SlipDatum      " << SlipHeaderSize   << '\n'
          << "  SlipReader     " << SlipReaderSize   << '\n'
          << "  maximum class  " << cellSize << " rounded up to a long boundary"
          << endl << endl;

      out << avsl.toString() << endl;

   }; // void SlipCellBase::sysInfo(ostream& out)


             /**********************************************
              *     Header Cell Data Access Functions      *
              **********************************************/
   /**
    * @fn SlipCellBase::getHeadDescList() const
    * @brief Return a pointer to the field containing the SlipHeader description list pointer.
    * @return pointer to the field containing the SlipHeader description list pointer
    */

   /**
    * @fn USHORT * SlipCellBase::getHeadRefCnt() const
    * @brief Return a pointer to the field containing the SlipHeader reference count.
    * @return pointer to the field containing the SlipHeader reference count
    */

   /**
    * @fn USHORT * SlipCellBase::getHeadMrk() const 
    * @brief Return a pointer to the field containing the SlipHeader user mark.
    * @return pointer to the field containing the SlipHeader user mark
    */

   /**
    * @fn SlipCellBase::getHeadDescList(const SlipCellBase& X)
    * @brief Return a pointer to the field containing the SlipHeader description list pointer.
    * @param[in] X SlipHeader object reference
    * @return pointer to the field containing the SlipHeader description list pointer
    */

   /**
    * @fn SlipCellBase::getHeadRefCnt(const SlipCellBase& X)
    * @brief Return a pointer to the field containing the SlipHeader reference count.
    * @param[in] X SlipHeader object reference
    * @return pointer to the field containing the SlipHeader reference count
    */

   /**
    * @fn SlipCellBase::getHeadMrk(const SlipCellBase& X)
    * @brief Return a pointer to the field containing the SlipHeader mark.
    * @param[in] X reference to a SlipHeader object
    * @return pointer to the field containing the SlipHeader mark
    */

             /**********************************************
              *     Sublist Cell Data Access Functions      *
              **********************************************/
   /**
    * @fn SlipCellBase::getSublistHeader() const
    * @brief Return a pointer to the SlipSublist field containing the SlipHeader pointer.
    * @return pointer to the SlipSublist field containing the SlipHeader pointer
    */

   /**
    * @fn SlipCellBase::getSublistHeader(const SlipCellBase& X)
    * @brief Return a pointer to the SlipSublist field containing the SlipHeader pointer.
    * @param[in] X SlipHeader object reference
    * @return pointer to the SlipSublist field containing the SlipHeader pointer
    */

             /**********************************************
              *     Reader Cell Data Access Functions      *
              **********************************************/
   /**
    * @fn void ** SlipCellBase::getReaderTop() const
    * @brief Return a pointer to the current list header field of a SlipReader object.
    * @return pointer to the current list header field in a SlipReader object
    */

   /**
    * @fn SlipCellBase::getReaderDepth() const
    * @brief Return a pointer to the current depth field of a SlipReader object.
    * @return pointer to the current depth field of a SlipReader object
    */

   /**
    * @fn SlipCellBase::getReaderTop(SlipCellBase& X)
    * @brief Return a pointer to the current list header field of a SlipReader object.
    * @param[in] X SlipReader object reference
    * @return a pointer to the current list header field of a SlipReader object
    */

   /**
    * @fn SlipCellBase::getReaderDepth(SlipCellBase& X)
    * @brief Return a pointer to the current depth field of a SlipReader object.
    * @param[in] X (SlipReader&) object reference
    * @return pointer to the current depth field of a SlipReader object
    */


             /**********************************************
              *   Slip Datum Cell data Access Functions    *
              **********************************************/
   /**
    * @fn SlipCellBase::getBool()  const
    * @brief Return a pointer to the SlipDatum cell bool field.
    * @return pointer to the SlipDatum cell bool field
    */

   /**
    * @fn SlipCellBase::getUChar() const
    * @brief Return a pointer to the SlipDatum cell UCHAR field.
    * @return pointer to the SlipDatum cell UCHAR field
    */

   /**
    * @fn SlipCellBase::getChar()  const
    * @brief Return a pointer to the SlipDatum CHAR field.
    * @return pointer to the SlipDatum CHAR field
    */

   /**
    * @fn SlipCellBase::getULong() const
    * @brief Return a pointer to the SlipDatum cell ULONG field.
    * @return pointer to the SlipDatum cell ULONG field
    */

   /**
    * @fn SlipCellBase::getLong()  const
    * @brief Return a pointer to the SlipDatum cell LONG field.
    * @return pointer to the SlipDatum cell LONG field
    */

   /**
    * @fn SlipCellBase::getDouble()const
    * @brief Return a pointer to the SlipDatum cell DOUBLE field.
    * @return pointer to the SlipDatum cell DOUBLE field
    */

   /**
    * @fn SlipCellBase::getPtr()   const
    * @brief Return a pointer to the SlipDatum cell PTR field.
    * @return pointer to the SlipDatum cell PTR field
    */

   /**
    * @fn SlipCellBase::getString()const
    * @brief Return a pointer to the SlipDatum cell STRING field.
    * @return pointer to the SlipDatum cell STRING field
    */

   /**
    * @fn SlipCellBase::getData()  const
    * @brief Return a pointer to the SlipDatum cell datum field.
    * <p>The SlipDatum cell datum field is the union of all SlipDatum values. It
    *    is the union of:</p>
    * <ul>
    *    <li><b>bool</b> boolean value</li>
    *    <li><b>CHAR</b> 8-bit character</li>
    *    <li><b>UCHAR</b> 8-bit unsigned character</li>
    *    <li><b>LONG</b> long value</li>
    *    <li><b>ULONG</b> unsigned long value</li>
    *    <li><b>DOUBLE</b> 64-bit double value</li>
    *    <li><b>STRING</b> pointer to either a SlipStringConst or SlipStringNonConst object</li>
    *    <li><b>PTR</b> pointer to a SlipPointer object</li>
    * </ul>
    * <p>The return field pointer points to the field within a SlipDatum object
    *    which contains this union.</p>
    * @return pointer to the SlipDatum cell datum field
    */

   /**
    * @fn SlipCellBase::resetData()
    * @brief Reset the SlipCell datum field to zero.
    */

   /**
    * @fn SlipCellBase::setData(const SlipCellBase& X)
    * @brief Assign data from the input SlipCellBase datum field to the current cell.
    * @param[in] X SlipCellBase reference data source
    */

   /**
    * @fn SlipCellBase::getBool(const SlipCellBase& X)
    * @brief Return a pointer to the SlipDatum cell bool field.
    * @param[in] X reference to a SlipDatum bool field
    * @return pointer to the SlipDatum cell bool field
    */

   /**
    * @fn SlipCellBase::getUChar(const SlipCellBase& X)
    * @brief Return a pointer to the SlipDatum cell UCHAR field.
    * Return a pointer to the SlipDatum cell UCHAR field.
    * @param[in] X reference to a SlipDatum UCHAR field
    * @return pointer to the SlipDatum cell UCHAR field
    */

   /**
    * @fn SlipCellBase::getChar(const SlipCellBase& X)
    * @brief Return a pointer to the SlipDatum cell CHAR field.
    * Return a pointer to the SlipDatum cell CHAR field.
    * @param[in] X reference to a SlipDatum CHAR field
    * @return pointer to the SlipDatum cell CHAR field
    */

   /**
    * @fn SlipCellBase::getULong(const SlipCellBase& X)
    * @brief Return a pointer to the SlipDatum cell ULONG field.
    * Return a pointer to the SlipDatum cell ULONG field.
    * @param[in] X reference to a SlipDatum ULONG field
    * @return pointer to the SlipDatum cell ULONG field
    */

   /**
    * @fn SlipCellBase::getLong(const SlipCellBase& X)
    * @brief Return a pointer to the SlipDatum cell LONG field.
    * Return a pointer to the SlipDatum cell LONG field.
    * @param[in] X reference to a SlipDatum LONG field
    * @return pointer to the SlipDatum cell LONG field
    */

   /**
    * @fn SlipCellBase::getDouble(const SlipCellBase& X)
    * @brief Return a pointer to the SlipDatum cell DOUBLE field.
    * Return a pointer to the SlipDatum cell DOUBLE field.
    * @param[in] X reference to a SlipDatum DOUBLE field
    * @return pointer to the SlipDatum cell DOUBLE field
    */

   /**
    * @fn SlipCellBase::getPtr(const SlipCellBase& X)
    * @brief Return a pointer to the SlipDatum cell PTR field.
    * Return a pointer to the SlipDatum cell PTR field.
    * <p>The field value points to a SlipPointer object.</p>
    * @param[in] X reference to a SlipDatum PTR field
    * @return pointer to the SlipDatum cell PTR field
    */

   /**
    * @fn SlipCellBase::getString(const SlipCellBase& X)
    * @brief Return a pointer to the SlipDatum cell STRING field.
    * <p>The field value points to a SlipStringConst or a SlipStringNonConst
    *    object.</p>
    * @param[in] X reference to a SlipDatum STRING field
    * @return pointer to the SlipDatum cell STRING field
    */

   /**
    * @fn SlipCellBase::getData(const SlipCellBase& X)
    * @brief Return a pointer to the SlipDatum cell datum field.
    * <p>The SlipDatum cell datum field is the union of all SlipDatum values. It
    *    is the union of:</p>
    * <ul>
    *    <li><b>bool</b> boolean value</li>
    *    <li><b>CHAR</b> 8-bit character</li>
    *    <li><b>UCHAR</b> 8-bit unsigned character</li>
    *    <li><b>LONG</b> long value</li>
    *    <li><b>ULONG</b> unsigned long value</li>
    *    <li><b>DOUBLE</b> 64-bit double value</li>
    *    <li><b>STRING</b> pointer to either a SlipStringConst or SlipStringNonConst object</li>
    *    <li><b>PTR</b> pointer to a SlipPointer object</li>
    * </ul>
    * <p>The return field pointer points to the field within a SlipDatum object
    *    which contains this union.</p>
    * @param[in] X reference to a SlipDatum cell object
    * @return pointer to the SlipDatum cell UCHAR field
    */

   /**
    * @fn SlipCellBase::resetData(SlipCellBase& X)
    * Reset the SlipDatum field value to zero
    * @param[in] X reference to a SlipDatum cell object
    */

  /**
   * @fn SlipCellBase::setData(SlipCellBase& X, const SlipCellBase& Y)
   * Replace the datum field of a Slip cell object.
   * <p>The entire field is replaced (assigned to).</p>
   * @param[out] X destination of Slip cell assignment
   * @param[in] Y source Slip cell assignment
   */

       /**********************************************************
        *            Accessor functions for Slip Cell            *
        **********************************************************/

   /**
    * @fn SlipCellBase::getOperator() const
    * @brief Return the SlipOp object pointer of a SlipCellBase object.
    * <p>The SlipOp object contains the invariant values, methods, and operations
    *    associated with the SlipCellBase cell. This applies to all derived
    *    classes of the SlipCellBase base class.</p>
    * @return SlipOp object pointer of a SlipCellBase object
    */

   /**
    * @fn SlipCellBase::getOperator(const SlipCellBase* X)
    * @brief Return the SlipOp object pointer of the input SlipCellBase object.
    * <p>The SlipOp object contains the invariant values, methods, and operations
    *    associated with the SlipCellBase cell. This applies to all derived
    *    classes of the SlipCellBase base class.</p>
    * @param[in] X reference to a SlipCellBase object
    * @return SlipOp object pointer of a SlipCellBase object
    */

   /**
    * @fn SlipCellBase::getLeftLink(const SlipCellBase& X)
    * @brief Return the value of the input SlipCellBase leftLink field.
    * @param[in] X reference to a SlipCellBase object
    * @return value of the input SlipCellBase leftLink field
    */

   /**
    * @fn SlipCellBase::getRightLink(const SlipCellBase& X)
    * @brief Return the value of the input SlipCellBase rightLink field.
    * @param[in] X reference to a SlipCellBase object
    * @return value of the input SlipCellBase rightLink field
    */

       /**********************************************************
        *                      Miscellaneous                     *
        **********************************************************/

   /**
    * @fn SlipCellBase::getClassType()const
    * @brief Return the object class type.
    * <p>Valid cell types are:</p>
    * | Cell  Type |   data type   |
    * | :--------- | :------------ |
    * | <b>eUNDEFINED</b>   | UNDEFINED     |
    * | <b>eBOOL</b>        | bool          |
    * | <b>eCHAR</b>        | CHAR          |
    * | <b>eUCHAR</b>       | UCHAR         |
    * | <b>eLONG</b>        | LONG          |
    * | <b>eULONG</b>       | ULONG         |
    * | <b>eDOUBLE</b>      | DOUBLE        |
    * | <b>eSUBLIST</b>     | SUBLIST       |
    * | <b>eSTRING</b>      | STRING        |
    * | <b>ePTR</b>         | PTR           |
    * | <b>eREADER</b>      | READER        |
    * | <b>eHEADER</b>      | HEADER        |
    *
    * @return class type of the object
    */
   const ClassType SlipCellBase::getClassType()const { return ((SlipOp*)operation)->getClassType();} // return the internal class type

   /**
    * @fn SlipCellBase::getLeftLink() const
    * @brief Return the cell leftLink.
    * @return Return the cell leftLink
    */
   /**
    * @fn SlipCellBase::getName()     const
    * @brief Return the cell canonical name.
    * <p>The Canonical Names are:</p>
    * |   Slip cell   | datum type |    Name   |
    * | :------------ | :--------- | :-------- |
    * | SlipHeader    |            | HEADER    |
    * | SlipSublist   |            | SUBLIST   |
    * | SlipReader    |            | READER    |
    * | SlipDatum     | bool       | bool      |
    * |               | CHAR       | CHAR      |
    * |               | UCHAR      | UCHAR     |
    * |               | LONG       | LONG      |
    * |               | ULONG      | ULONG     |
    * |               | DOUBLE     | DOUBLE    |
    * |               | PTR        | PTR       |
    * |               | STRING     | STRING    |
    * | SlipUndefined | UNDEFINED  | UNDEFINED |
    * @return (string) return the cell canonical name
    */
 const string     SlipCellBase::getName() const { return ((SlipOp*)operation)->getName((SlipDatum*)this);}
   /**
    * @fn SlipCellBase::getRightLink() const
    * @brief Return the rightLink.
    * @return Return the rightLink
    */

   /**
    * @fn SlipCellBase::getSlipState()
    * @brief Return the current Slip state structure.
    * <p>The current Slip state is contained in an object of type AVSLState. The
    *    following field values are returned:</p>
    * | Field names | Description
    * | :---------- | :---------------------------------- |
    * | avail       | Total AVSL free cells               |
    * | total       | Total AVSL cells (free \& used)     |
    * | alloc       | Initial heap allocation (cells)     |
    * | delta       | Incremental heap allocation (cells) |
    * @return Return the current Slip state structure     |
    */
   SlipState       SlipCellBase::getSlipState()      { return SlipState(avsl.total, avsl.avail, avsl.alloc, avsl.delta);};

   /**
    * @fn SlipCellBase::isData()      const
    * @brief Return <b>true</b> if the Slip cell is a SlipDatum cell.
    * @return <b>true</b> if the Slip cell is a SlipDatum cell
    */
   const bool      SlipCellBase::isData() const { return ((SlipOp*)operation)->isData();}       // true if cell is data

   /**
    * @fn SlipCellBase::isDeleted()   const
    * @brief Return <b>true</b> if the Slip cell is in the AVSL.
    * <p>All cells that are posted to the AVSL have their left links set to
    *    <b>UNDEFINED</b>, a manifest constant whose value is 0xdeadbeef for
    *    32-bit compiles and 0xdeadbeefdeadbeef for 64-bit compiles. The
    *    left link of the current Slip cell is checked.</p>
    * <p>It is considered remarkably poor form to retain a reference to a
    *    deleted Slip cell.</p>
    * @return <b>true</b> if the Slip cell is in the AVSL
    */

   /**
    * @fn SlipCellBase::isDiscrete()  const
    * @brief Return <b>true</b> if the SlipDatum represents a discrete data type.
    * <p>Discrete data objects can participate in arithmetic, bit, boolean, and
    *    logical operations.</p>
    * <p>The discrete data types are:</p>
    * <ul>
    *    <li><b>bool</b></li>
    *    <li><b>CHAR</b></li>
    *    <li><b>LONG</b></li>
    *    <li><b>UCHAR</b></li>
    *    <li><b>ULONG</b></li>
    * </ul>
    * <p>The non-discrete data types are:</p>
    * <ul>
    *    <li><b>DOUBLE</b></li>
    *    <li><b>HEADER</b></li>
    *    <li><b>PTR</b></li>
    *    <li><b>STRING</b></li>
    *    <li><b>READER</b></li>
    *    <li><b>SUBLIST</b></li>
    *    <li><b>UNDEFINED</b></li>
    * </ul>
    * @note boolean objects have the value <b>true</b>/<b>1</b> or <b>false</b>
    *       <b>0</b> as appropriate.
    * @return <b>true</b> if the SlipDatum represents a discrete data type
    */
   const bool      SlipCellBase::isDiscrete() const { return ((SlipOp*)operation)->isDiscrete();}   // true if cell is a discrete number

   /**
    * @fn  SlipCellBase::isHeader()    const
    * @brief Return <b>true</b> for a SlipHeader object.
    * @return Return <b>true</b> for a SlipHeader object
    */
   const bool      SlipCellBase::isHeader()    const { return ((SlipOp*)operation)->isHeader();}    // true if cell is a header

   /**
    * @fn SlipCellBase::isName()      const
    * @brief Return <b>true</b> for a SlipSublist object.
    * <p>The use of "Name" is a relic of the original SLIP definition manual.</p>
    * @return <b>true</b> for a SlipSublist object.
    */

   /**
    * @fn SlipCellBase::isNumber()    const
    * @brief Return <b>true</b> if this is a SlipDatum numeric type.
    * <p>The numeric data types are:</p>
    * <ul>
    *    <li><b>bool</b></li>
    *    <li><b>CHAR</b></li>
    *    <li><b>DOUBLE</b></li>
    *    <li><b>LONG</b></li>
    *    <li><b>UCHAR</b></li>
    *    <li><b>ULONG</b></li>
    * </ul>
    * <p>The non-numeric data types are:</p>
    * <ul>

    *    <li><b>HEADER</b></li>
    *    <li><b>PTR</b></li>
    *    <li><b>STRING</b></li>
    *    <li><b>READER</b></li>
    *    <li><b>SUBLIST</b></li>
    *    <li><b>UNDEFINED</b></li>
    * </ul>
    * @return <b>true</b> if this is a SlipDatum numeric type
    */
   const bool      SlipCellBase::isNumber()    const { return ((SlipOp*)operation)->isNumber();}    // true if cell is numeric data

   /**
    * @fn SlipCellBase::isPtr()       const
    * @brief Return <b>true</b> for a SlipDatum (PTR) data cell.
    * <p>This method will return <b>false</b> for a SlipDatum (STRING) object.</p>
    * @return <b>true</b> for a SlipDatum (PTR) data cell
    */
   const bool      SlipCellBase::isPtr()       const { return ((SlipOp*)operation)->isPtr();}       // true if cell is a pointer
   
   /**
    * @fn SlipCellBase::isReal()      const
    * @brief Return <b>true</b> for a SlipDatum (DOUBLE) object.
    * @return <b>true</b> for a SlipDatum (DOUBLE) object
    */
   const bool      SlipCellBase::isReal()      const { return ((SlipOp*)operation)->isReal();}      // true if cell is a real

   /**
    * @fn SlipCellBase::isString()    const
    * @brief Return <b>true</b> for a SlipDatum (STRING) data object.
    * @return Return <b>true</b> for a SlipDatum (STRING) data object
    */
   const bool      SlipCellBase::isString()    const { return ((SlipOp*)operation)->isString();}    // true if cell is a string

   /**
    * @fn SlipCellBase::isSublist()
    * @brief Return <b>true</b> for a SlipSublist object.
    * @return <b>true</b> for a SlipSublist object
    */
   const bool      SlipCellBase::isSublist()   const { return ((SlipOp*)operation)->isSublist();}   // true if cell is sublist

   /**
    * @fn SlipCellBase::isTemp()      const
    * @brief Return <b>true</b> for an object created on the runtime stack.
    * @return <b>true</b> for an object created on the runtime stack
    */

   /**
    * @fn SlipCellBase::isTemp(const SlipCellBase* cell)
    * @brief Return <b>true</b> for an object created on the runtime stack.
    * @param[in] cell object to be checked
    * @return <b>true</b> for an object created on the runtime stack
    */

   /**
    * @fn SlipCellBase::isUnlinked()  const
    * @brief Return <b>true</b> if the Slip object is not in a list.
    * <p>A <b>false</b> will be returned if:</p>
    * <ul>
    *    <li>An object is in the AVSL (a list of available cells).</li>
    *    <li>An object is created on the runtime stack.</li>
    * </ul>
    * @return Return <b>true</b> if the Slip object is not in a list
    */

   /**
    * @brief Uglify and print the object.
    * <p>The linkLeft, linkRight, and operator fields are output in hex. The
    *    datum field is output according to the Slip object type.</p>
    * @return Uglify and print the object
    */
   string   SlipCellBase::dump() const {
      try {
         return ((SlipOp*)*getOperator())->dump((SlipDatum&)*this);
      } catch(...) {
         stringstream pretty;
         pretty << "[sublist  ] 0x" << hex << setw(8) << setfill('0');
         pretty << this << "::";
         return pretty.str();
      }
   }; // string   SlipCellBase::dump() const

} // namespace slip
