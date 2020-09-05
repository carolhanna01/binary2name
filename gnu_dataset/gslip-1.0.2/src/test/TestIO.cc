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

# include <cstdio>
# include <cstdio>
# include <ctime>
# include <fstream>
# include <iostream>
# include <sstream>
# include <unistd.h>

# include "../libgslip/SlipDatum.h"
# include "../libgslip/SlipHeader.h"
# include "../libgslip/SlipPointer.h"
# include "../libgslip/SlipRead.h"
# include "../libgslip/SlipSequencer.h"

# include "TestGlobal.h"
# include "TestIO.h"
# include "TestUtilities.h"

using namespace slip;
using namespace std;

string filename;
string filenameBase;
string udFilename = "udTest0.log";

/**
 * @brief Compare two lists for semantic identity.
 * <p>Two lists are compared for exact identity.
 *    For both the Descriptor list (Descriptor
 *    chain) and the lists, all entries must be
 *    the same in type and value, excepting only
 *    that the sublist references can be to two
 *    different list headers.</p>
 * <p><b>true</b> is returned if the list are
 *    topologically identical with the same contents.</p>
 * @param list1 (SlipHeader*) comparand
 * @param list2 (SlipHeader*) comparand
 * @return <b>true</b> lists are equal
 */
bool compareLists(SlipHeader& list1, SlipHeader& list2) {
   stringstream pretty;
   SlipSequencer seq1(list1);
   SlipSequencer seq2(list2);

   if (list1.getMark() != list2.getMark()) {
      pretty << "The list marks are different " << list1.getMark() << " != " << list2.getMark();
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
      return false;
   }

   if (list1.isDList() != list2.isDList()) {
      if (list1.isDList()) {
         pretty << "list1 has a Description List but list2 does not";
      } else {
         pretty << "list1 does not have a Description list but list2 does";
      }
      writeDiagnostics(__FILE__, __LINE__, pretty.str());
      return false;
   }

   if (list1.isDList()) {
      if (!compareLists(list1.getDList(), list2.getDList()))
         return false;
   }
   for (seq1.advanceLWR(), seq2.advanceLWR();
       !seq1.isHeader() && !seq2.isHeader();
        seq1.advanceLWR(), seq2.advanceLWR()) {
      if (seq1.getClassType() != seq2.getClassType()) {
         pretty << "list1 " << seq1.toString() << " list2 " << seq2.toString();
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
         writeDiagnostics(__FILE__, "list1 and list2 datum types are different ");
         return false;
      }
      if (seq1.isSublist()) {
         SlipHeader& head1 = ((SlipSublist&)(seq1.currentCell())).getHeader();
         SlipHeader& head2 = ((SlipSublist&)(seq2.currentCell())).getHeader();
         if (!compareLists(head1, head2))
            return false;
      } else if (!seq1.isPtr()) {
         if (seq1.currentCell() != seq2.currentCell()) {
           pretty << "list1 " << seq1.toString() << " list2 " << seq2.toString();
           writeDiagnostics(__FILE__, __LINE__, pretty.str());
           writeDiagnostics(__FILE__, "list1 and list2 cell contents are different ");
           return false;
        }
      }
   }
   return seq1.isHeader() && seq2.isHeader();
}; // bool compareLists(SlipHeader* list1, SlipHeader* list2)

/**
 * @brief Debug header (during debug testing).
 * @param[in] i (int) index of current list header
 * @param[in] head (SlipHeader*) list
 */
void debugList(int i, SlipHeader* head) {
   stringstream pretty;
   pretty << "---------- " << "header[" << i  << "] ----------";
   writeDiagnostics(__FILE__, __LINE__, pretty.str());
   head->writeQuick(logFile);
   logFile;
   head->write(logFile);
   writeDiagnostics(__FILE__,  "\n");
}; // void debugList(string name, SlipHeader* head)

bool openFile(ofstream& out, string filename) {
   bool flag = true;
   out.clear(ios::badbit | ios::failbit | ios::eofbit);
   out.open(filename.c_str(), ios::trunc);
   if (!out.is_open()) {
      flag = false;
      postError(__FILE__, __LINE__, SlipErr::E4014, "TestIO::write::testInclude", filename, "");
   }
   return flag;
}; // bool openFile())
/**
 * @brief Test I/O of Descriptor Lists.
 * @return <b>true</b> if test succeed
 */
bool testDescriptorListIO() {
   stringstream pretty;
   bool flag = true;
   SlipHeader* header[] = { new SlipHeader()             // "( < > )"
                          , new SlipHeader()             // "( < < > > )"
                          , new SlipHeader()             // "( < > 1 2 )"
                          , new SlipHeader()             // "( < < > 1 2 > )"
                          , new SlipHeader()             // "(< < 1 2 > 3 4 > )"
                          , new SlipHeader()             // "(< < 1 2 > 3 4 > 5 6 )"
                          , new SlipHeader()             // "( <7 8> (< <1 2 > 3 4> 5 6) )"
                          };

   header[0]->create_dList();                            // ( <> ))

   header[1]->create_dList();                            // ( <> ))
   (header[1]->getDList()).create_dList();               // ( < <> > ))

   header[2]->enqueue((LONG)1).enqueue((LONG)2);         // ( 1 2 )
   header[2]->create_dList();                            // ( <> 1 2 )

   header[3]->create_dList().appendDList(*header[2]);    // ( < <> 1 2 > )

   header[4]->create_dList().enqueue((LONG)3).enqueue((LONG)4).create_dList().enqueue((LONG)1).enqueue((LONG)2);

   header[5]->enqueue((LONG)5).enqueue((LONG)6).appendDList(*header[4]);

   header[6]->enqueue(*header[5]).create_dList().enqueue((LONG)7).enqueue((LONG)8);

   const int size = sizeof(header)/sizeof(header[0]);
   
   for(int i = 1; i < size; i++) {
      header[i]->write(filename);
      SlipRead* reader = new SlipRead();
      SlipHeader& input = reader->read(filename);
      if (!compareLists(*header[i], input)) {
         flag = false;
         pretty << "error on header[" << i << "]";
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
         pretty << "output["    << i << "] " << header[i]->writeToString();
         writeDiagnostics(__FILE__, pretty.str());
         pretty << "input     " << input.writeToString();
         writeDiagnostics(__FILE__, pretty.str());
      }
      delete reader;
      input.deleteList();
   }

   for (int i = 0; i < size; i++) header[i]->deleteList();

   if (!flag) testResultOutput("test Descriptor List IO", 0.0, flag);
   return flag;
}; // bool testDescriptorListIO()

/**
 * @brief Test include directive.
 * @return <b>true</b> if test successful
 */
bool testInclude() {
   stringstream pretty;
   bool flag = true;
   SlipHeader* header = new SlipHeader();
   string listFileData = "# include \"" + filenameBase + "\"\n {list1}";
   string includeFileData = "list1 ( 1 2 );";
   ofstream out;

   header->enqueue((LONG)1).enqueue((LONG)2);

   if ((flag = openFile(out, filenameBase))) {
      out << includeFileData;
      out.close();
   }

   if (flag && (flag = openFile(out, filename))) {
      out << listFileData;
      out.close();
      SlipRead* reader = new SlipRead();
      SlipHeader& input = reader->read(filename);
      if (!compareLists(*header, input)) {
         flag = false;
         writeDiagnostics(__FILE__, __LINE__, "error on header");
         pretty << "output    " << header->writeToString();
         writeDiagnostics(__FILE__, pretty.str());
         pretty << "input     " << input.writeToString();
         writeDiagnostics(__FILE__, pretty.str());
      }
      delete reader;
      input.deleteList();
      remove(filename.c_str());
      remove(filenameBase.c_str());
   }

   if (!flag) testResultOutput("Test Include", 0.0, flag);
   return flag;
}; // bool testInclude()

/**
 * @brief Test 'normal' list I/O.
 * @return <b>true</b> if test successful
 */
bool testListIO() {
   stringstream pretty;
   bool flag = true;
   SlipHeader* header[] = { new SlipHeader()             // "( )"
                          , new SlipHeader()             // "( 1 2 3 "This is a String" )"
                          , new SlipHeader()             // "( 5 6 (1 2 3 "This is a String") )"
                          , new SlipHeader()             // "( 7 (5 6 (1 2 3 "This is a String")) 8 )"
                          , new SlipHeader()             // ""
                          , new SlipHeader()             // "(() () ())"
                          , new SlipHeader()             // "(() () ())"
                          };

   header[1]->enqueue((LONG) 1).enqueue((LONG) 2).enqueue((LONG) 3).enqueue((string)"This is a String");
   header[2]->enqueue((LONG) 5).enqueue((LONG) 6).enqueue(*header[1]);
   header[3]->enqueue((LONG) 7).enqueue(*header[2]).enqueue((LONG) 8);
   header[4]->enqueue(*header[3]).enqueue(*header[2]).enqueue(*header[1]).enqueue((ULONG) 9);
   header[5]->enqueue(*header[0]).enqueue(*header[0]).enqueue(*header[0]);
   header[6]->enqueue(*new SlipHeader()).enqueue(*new SlipHeader()).enqueue(*new SlipHeader());
   SlipSequencer seq(*header[6]);

   for (seq.advanceLWR(); !seq.isHeader(); seq.advanceLWR())
      ((SlipSublist&)seq.currentCell()).getHeader().deleteList();

   const int size = sizeof(header)/sizeof(header[0]);

   for(int i = 0; i < size; i++) {
      if ((flag = header[i]->write(filename))) {
         SlipRead* reader = new SlipRead();
         SlipHeader& input = reader->read(filename);
         if (!compareLists(*header[i], input)) {
            flag = false;
            pretty << "error on header[" << i << "]";
            writeDiagnostics(__FILE__, __LINE__, pretty.str());
            pretty << "output["    << i << "] " << header[i]->writeToString();
            writeDiagnostics(__FILE__, pretty.str());
            pretty << "input     " << input.writeToString();
            writeDiagnostics(__FILE__, pretty.str());
         }
         delete reader;
         input.deleteList();
         remove(filename.c_str());
      }
   }

   for (int i = 0; i < size; i++) header[i]->deleteList();

   if (!flag) testResultOutput("Test List I/O", 0.0, flag);
   return flag;
}; // bool testListIO()

/**
 * @brief Test 'normal' lists with user Reference Marks.
 * @return <b>true</b> if test successful
 */
bool testMarkListIO() {
   stringstream pretty;
   bool flag = true;
   SlipHeader* header[] = { new SlipHeader()             // "( )"
                          , new SlipHeader()             // "( 1 2 3 "This is a String" )"
                          , new SlipHeader()             // "( 5 6 (1 2 3 "This is a String") )"
                          , new SlipHeader()             // "( 7 (5 6 (1 2 3 "This is a String")) 8 )"
                          , new SlipHeader()             // ""
                          , new SlipHeader()             // "(() () ())"
                          , new SlipHeader()             // "(() () ())"
                          };

   header[1]->enqueue((LONG) 1).enqueue((LONG) 2).enqueue((LONG) 3).enqueue((string)"This is a String");
   header[2]->enqueue((LONG) 5).enqueue((LONG) 6).enqueue(*header[1]);
   header[3]->enqueue((LONG) 7).enqueue(*header[2]).enqueue((LONG) 8);
   header[4]->enqueue(*header[3]).enqueue(*header[2]).enqueue(*header[1]).enqueue((ULONG) 9);
   header[5]->enqueue(*header[0]).enqueue(*header[0]).enqueue(*header[0]);
   header[6]->enqueue(*new SlipHeader()).enqueue(*new SlipHeader()).enqueue(*new SlipHeader());

   SlipSequencer seq(*header[6]);

   for (seq.advanceLNR(); !seq.isHeader(); seq.advanceLNR())
      ((SlipSublist&)seq.currentCell()).getHeader().deleteList();

   const int size = sizeof(header)/sizeof(header[0]);

   for (int i = 0; i < size; i++) header[i]->putMark(i + 10);
   for(int i = 0; i < size; i++) {

      header[i]->write(filename);
      SlipRead* reader = new SlipRead();
      SlipHeader& input = reader->read(filename);
      if (!compareLists(*header[i], input)) {
         flag = false;
         pretty << "error on header[" << i << "]";
         writeDiagnostics(__FILE__, __LINE__, pretty.str());
         pretty << "output["    << i << "] " << header[i]->writeToString();
         writeDiagnostics(__FILE__, pretty.str());
         pretty << "input     " << input.writeToString();
         writeDiagnostics(__FILE__, pretty.str());
      }
      delete reader;
      input.deleteList();
   }

   for (int i = 0; i < size; i++) header[i]->deleteList();

   if (!flag) testResultOutput("Test Mark List I/O", 0.0, flag);
   return flag;
}; // bool tesMarktListIO()

/**
 * @brief Test I/O of User Data SlipDatum objects
 * @return <b>true</b> if test successful
 */
int ctr = 0;

bool testUserDataIO() {
   stringstream pretty;
   bool flag = true;
   class SP : public SlipPointer {
      SlipHeader* list;
      static
      string* udName() { ctr++; stringstream pretty;
                         pretty << "SP" << setw(2) << setfill('0') << ctr;
                         return new string(pretty.str()); }
   public:
      SP(SlipHeader* list) : SlipPointer(udName()), list(list) {}
      virtual ~SP() { list->deleteList(); }
      virtual SP*      copy( )        {return new SP(new SlipHeader(list)); }
      virtual Parse    getParse()     { return &parse; };
      static  SlipDatum& parse(SlipHeader& head) { return *new SlipDatum(new SP(&head)); };
      virtual void     remove() const { delete this; }
      static  void     reset()        { ctr = 0; }
      virtual string   dump() const   { return list->dump(); }
      virtual string   toString() const { return list->writeToString(); }
      virtual string   write() const    { return toString(); }
   }; // class SP : SlipPointer

   SlipHeader* header[] = { new SlipHeader()             //  0  "( < > )"
                          , new SlipHeader()             //  1  "( < < > > )"
                          , new SlipHeader()             //  2  "( < > 1 2 )"
                          , new SlipHeader()             //  3  "( < < > 1 2 > )"
                          , new SlipHeader()             //  4  "(< < 1 2 > 3 4 > )"
                          , new SlipHeader()             //  5  "(< < 1 2 > 3 4 > 5 6 )"
                          , new SlipHeader()             //  6  "( <7 8> (< <1 2 > 3 4> 5 6) )"
                          , new SlipHeader()             //  7  "( )"
                          , new SlipHeader()             //  8  "( 1 2 3 "This is a String" )"
                          , new SlipHeader()             //  9  "( 5 6 (1 2 3 "This is a String") )"
                          , new SlipHeader()             // 10  "( 7 (5 6 (1 2 3 "This is a String")) 8 )"
                          , new SlipHeader()             // 11  ""
                          , new SlipHeader()             // 12  "(() () ())"
                          , new SlipHeader()             // 13  "(() () ())"
                          };

   header[0]->create_dList();                            // ( <> ))

   header[1]->create_dList();                            // ( <> ))
   (header[1]->getDList()).create_dList();               // ( < <> > ))

   header[2]->enqueue((LONG)1).enqueue((LONG)2);         // ( 1 2 )
   header[2]->create_dList();                            // ( <> 1 2 )

   header[3]->create_dList().appendDList(*header[2]);    // ( < <> 1 2 > )

   header[4]->create_dList().enqueue((LONG)3).enqueue((LONG)4).create_dList().enqueue((LONG)1).enqueue((LONG)2);

   header[5]->enqueue((LONG)5).enqueue((LONG)6).appendDList(*header[4]);

   header[6]->enqueue(*header[5]).create_dList().enqueue((LONG)7).enqueue((LONG)8);

   header[ 8]->enqueue((LONG) 1).enqueue((LONG) 2).enqueue((LONG) 3).enqueue((string)"This is a String");
   header[ 9]->enqueue((LONG) 5).enqueue((LONG) 6).enqueue(*header[8]);
   header[10]->enqueue((LONG) 7).enqueue(*header[9]).enqueue((LONG) 8);
   header[11]->enqueue(*header[9]).enqueue(*header[8]).enqueue(*header[7]).enqueue((ULONG) 9);
   header[12]->enqueue(*header[7]).enqueue(*header[7]).enqueue(*header[7]);
   header[13]->enqueue(*new SlipHeader()).enqueue(*new SlipHeader()).enqueue(*new SlipHeader());

   SlipHeader* udList = new SlipHeader();

   const int size = sizeof(header)/sizeof(header[0]);

   for (int i = 0; i < size; i++) udList->enqueue(*new SlipDatum(new SP(header[i])));
   SP::reset();
   udList->write(filename);
   SlipRead* reader = new SlipRead();
   SlipReader* iter = new SlipReader(*udList);
   while(!iter->advanceSWR().isHeader()) {
      if (iter->isPtr())
         reader->registerUserData((SlipDatum&)iter->currentCell());
   }
   SlipHeader& input = reader->read(filename);
   delete reader;
   if (!compareLists(*udList, input)) {
      flag = false;
      writeDiagnostics(__FILE__, __LINE__, "error on udList");
      pretty << "output " << udList->writeToString();
      writeDiagnostics(__FILE__, pretty.str());
      pretty << "input  " << input.writeToString();
      writeDiagnostics(__FILE__, pretty.str());
   }
   input.deleteList();

   udList->deleteList();
   for (int i = 0; i < size; i++) header[i]->deleteList();

   if (!flag) testResultOutput("Test User Data I/O", 0.0, flag);
   return flag;
}; // testUserDataIO()1

/**
 * @brief Test driver
 * @return <b>true</b> if all subtests are successful
 */
bool testIO() {
   high_resolution_clock::time_point start =  high_resolution_clock::now();
   bool flag = true;

   time_t rawtime;
   struct tm * ptm;
   time ( &rawtime );
   ptm = gmtime ( &rawtime );
   stringstream pretty;
   pretty << "TestIO_" << ptm->tm_yday
                       << ptm->tm_hour
                       << ptm->tm_min
                       << ptm->tm_sec;
   filenameBase = pretty.str();
   pretty << ".log";

   filename = pretty.str();
   flag = (testDescriptorListIO())?  flag: false;
   flag = (testListIO())?            flag: false;
   flag = (testMarkListIO())?        flag: false;
   flag = (testUserDataIO())?        flag: false;
   flag = (testInclude())?           flag: false;

   remove(filename.c_str());
   
   high_resolution_clock::time_point end =  high_resolution_clock::now();
   duration<double> seconds = duration_cast<duration<double>>(end - start);
   testResultOutput("Test Input/Output", seconds.count(), flag);
   return flag;
}; // bool testIO()
