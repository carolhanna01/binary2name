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
 * @file   SlipSequencer.h
 * @author A. Schwarz
 * @date   August 14, 2012
 *
 * @brief User interface to the non-structured list iterator.
 *
 */

#ifndef SLIPSEQUENCER_H
#define	SLIPSEQUENCER_H

# include "SlipHeader.h"
# include "SlipCell.h"
# include "SlipCellBase.h"
# include "SlipDatum.h"
# include "SlipGlobal.h"
# include "SlipSublist.h"
#include "SlipReader.h"

namespace slip {
/**
 * @class slip::SlipSequencer
 * 
 * @brief User interface to the non-structured list iterator.
 * 
 * <p><b>Overview</b></p><hr>
 * <p>Linear iteration is the process of visiting cells in a single list.
 *    Structured iteration is the processes of visiting cells in a list
 *    and sublist(s).The sequencer interface provides a means to do
 *    linear and structured iteration over a list.</p>
 * <p>The sequencer interface provides linear and structured left and right 
 *    (previous and next) iteration over an acyclic graph with the means
 *    to determine the cell type to terminate the iteration. Iteration
 *    is called <b>advance</b> and there are three attributes that can 
 *    be used in an iteration:</p>
 * <ul>
 *    <li><b>Element (E)</b> advance until a <em>SlipDatum</em> cell is found.</li>
 *    <li><b>Name (N)</b> advance until a <em>SlipSublist</em> cell is found.</li>
 *    <li><b>Word (*W)</b> advance to the next cell.</li>
 * </ul>
 * <p>If the search element (E or W) is not found, then the iteration will stop
 *    at the current list header (SlipHeader) for linear (L) iterations
 *    and at the bottommost list header for structured (S) iterations.</p>
 * <p>The advance can be <b>Left (L)</b> or <b>Right (R)</b>, or 
 *    <b>Linear (L)</b> or <b>Structured (S)</b>. This can be composed
 *    into the following advance methods:</p>
 * <tt><center>
 * |   advance  |  iterate  |  search | direction |
 * | :--------- | :-------- | :------ | :-------- |
 * | advanceLER | linear    | element | right     |
 * | advanceLEL | linear    | element | left      |
 * | advanceLNR | linear    | name    | right     |
 * | advanceLNL | linear    | name    | left      |
 * | advanceLWR | linear    | word    | right     |
 * | advanceLWL | linear    | word    | left      |
 * | advanceSER | structure | element | right     |
 * | advanceSEL | structure | element | left      |
 * | advanceSNR | structure | name    | right     |
 * | advanceSNL | structure | name    | left      |
 * | advanceSWR | structure | word    | right     |
 * | advanceSWL | structure | word    | left      |
 * </center></tt>
 * <p>The legends have the following meaning:</p>
 * <ul type="none">
 *    <li><b>advance</b> name of the method.</li>
 *    <li><b>iteration</b> linear iteration will not ascend from or descend
 *        to a list, structured iteration will ascend from or descend to a
 *        list.</li>
 *    <li><b>search</b> stop iterating when an element, name, or word is
 *        found. Iteration will always terminate on a list head, 
 *        <b>SlipHeader</b>, cell if the search item is not found. For
 *        linear iterators a search will stop when the list head of the 
 *        current list is found. For structured iterators a search will
 *        stop at the bottommost list head on the first sublist seen in
 *        each indentured list.</li>
 *    <li><b>direction</b> the direction of the iteration starting at the
 *        current cell. The iteration will proceed to prior cells for 
 *        <b>Left</b> and to succeeding next cells for <b>Right</b>.
 *        Sublist entry/exit depends on the iteration type, <b>Linear</b> 
 *        <b>Structured</b>.</li>
 * </ul>
 * <p>An example of the use of these functions is given:</p>
 * <tt><pre>
 *   <b>list = (a b (c (d) e ) f)</b>
 * </pre></tt>
 * <p>Using the notation D(value) to represent a SlipDatum value, and H() to
 *    represent a SlipHeader defined list, and N(H()) to represent a SlipSublist
 *    reference, N(), to a list header, H() where H represents the 'name' of
 *    a list, we transform the above list to the following linear relation:</p>
 * <tt><pre>
 *   <b>list = L1( D(a) D(b) N(L2(D(c) N(L3(D(d))) D(e)) D(f))</b>
 * </pre></tt>
 * <p>The spatial relation representing this list is:</p>
 * <tt><pre>
 *   <b>L1( D(a) D(b) N() D(f) )word
 *                 |
 *                 |
 *                 o L2( D(c) N() D(e) )
 *                            |
 *                            |
 *                            o L3( D(d) )
 * </b>
 * </pre></tt>
 * <p>The following examples show the effect of each of the readers over the
 *    list. The sublist references will be returned as N(H) representing
 *    a sublist, N, and the list it references, H. In our example there are
 *    two sublists, N(L2) and N(L3) representing the sublist cells referencing
 *    lists L2 and L3 respectively.</p>
 * <p>All sequencers start at L1.</p>
 * <ul>
 *    <li><b>SlipSequencer R = new SlipSequencer(L1);</b> &rarr; <tt>&lt;L1 L1&gt;</tt></li>
 *    <ul type="none">
 *       <li><b>R.advanceLER();</b> &rarr; <tt>&lt;D(a)&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceLER();</b> &rarr; <tt>&lt;D(b)&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceLER();</b> &rarr; <tt>&lt;D(f)&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceLER();</b> &rarr; <tt>&lt;L1&nbsp;&nbsp;&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>delete R;</b></li>
 *    </ul>
 *    <li><b>SlipSequencer R = new SlipSequencer(L1);</b> &rarr; <tt>&lt;L1&gt;</tt></li>
 *    <ul type="none">
 *       <li><b>R.advanceLEL();</b> &rarr; <tt>&lt;D(f)&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceLEL();</b> &rarr; <tt>&lt;D(b)&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceLEL();</b> &rarr; <tt>&lt;D(a)&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceLEL();</b> &rarr; <tt>&lt;L1&nbsp;&nbsp;&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>delete R;</b></li>
 *    </ul>
 *    <li><b>SlipSequencer R = new SlipSequencer(L1);</b> &rarr; <tt>&lt;L1&gt;</tt></li>
 *    <ul type="none">
 *       <li><b>R.advanceLNR();</b> &rarr; <tt>&lt;N(L2)&gt;</tt></li>
 *       <li><b>R.advanceLNR();</b> &rarr; <tt>&lt;L1&nbsp;&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>delete R;</b></li>
 *    </ul>
 *    <li><b>SlipSequencer R = new SlipSequencer(L1);</b> &rarr; <tt>&lt;L1&gt;</tt></li>
 *    <ul type="none">
 *       <li><b>R.advanceLNL();</b> &rarr; <tt>&lt;N(L2)&gt;</tt></li>
 *       <li><b>R.advanceLNL();</b> &rarr; <tt>&lt;L1&nbsp;&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>delete R;</b></li>
 *    </ul>
 *    <li><b>SlipSequencer R = new SlipSequencer(L1);</b> &rarr; <tt>&lt;L1&nbsp;&nbsp;&nbsp;&gt;</tt></li>
 *    <ul type="none">
 *       <li><b>R.advanceLWR();</b> &rarr; <tt>&lt;D(a)&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceLWR();</b> &rarr; <tt>&lt;D(b)&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceLWR();</b> &rarr; <tt>&lt;N(L2)&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceLWR();</b> &rarr; <tt>&lt;D(f)&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceLWR();</b> &rarr; <tt>&lt;L1&nbsp;&nbsp;&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>delete R;</b></li>
 *    </ul>
 *    <li><b>SlipSequencer R = new SlipSequencer(L1);</b> &rarr; <tt>&lt;L1&gt;</tt></li>
 *    <ul type="none">
 *       <li><b>R.advanceLWL();</b> &rarr; <tt>&lt;D(f)&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceLWL();</b> &rarr; <tt>&lt;N(L2)&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceLWL();</b> &rarr; <tt>&lt;D(b)&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceLWL();</b> &rarr; <tt>&lt;D(a)&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceLWL();</b> &rarr; <tt>&lt;L1&nbsp;&nbsp;&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>delete R;</b></li>
 *    </ul>
 *    <li><b>SlipSequencer R = new SlipSequencer(L1);</b> &rarr; <tt>&lt;L1&gt;</tt></li>
 *    <ul type="none">
 *       <li><b>R.advanceSER();</b> &rarr; <tt>&lt;D(a)&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceSER();</b> &rarr; <tt>&lt;D(b)&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceSER();</b> &rarr; <tt>&lt;D(c)&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceSER();</b> &rarr; <tt>&lt;D(d)&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceSER();</b> &rarr; <tt>&lt;L1&nbsp;&nbsp;&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>delete R;</b></li>
 *    </ul>
 *    <li><b>SlipSequencer R = new SlipSequencer(L1);</b> &rarr; <tt>&lt;L1&gt;</tt></li>
 *    <ul type="none">
 *       <li><b>R.advanceSEL();</b> &rarr; <tt>&lt;D(f)&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceSEL();</b> &rarr; <tt>&lt;D(e)&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceSEL();</b> &rarr; <tt>&lt;D(d)&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceSEL();</b> &rarr; <tt>&lt;L3&nbsp;&nbsp;&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>delete R;</b></li>
 *    </ul>
 *    <li><b>SlipSequencer R = new SlipSequencer(L1);</b> &rarr; <tt>&lt;L1&nbsp;&nbsp;&nbsp;&gt;</tt></li>
 *    <ul type="none">
 *       <li><b>R.advanceSNR();</b> &rarr; <tt>&lt;N(L2)&gt;</tt></li>
 *       <li><b>R.advanceSNR();</b> &rarr; <tt>&lt;N(L3)&gt;</tt></li>
 *       <li><b>R.advanceSNR();</b> &rarr; <tt>&lt;L2&nbsp;&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>delete R;</b></li>
 *    </ul>
 *    <li><b>SlipSequencer R = new SlipSequencer(L1);</b> &rarr; <tt>&lt;L1&gt;</tt></li>
 *    <ul type="none">
 *       <li><b>R.advanceSNL();</b> &rarr; <tt>&lt;N(L2)&gt;</tt></li>
 *       <li><b>R.advanceSNL();</b> &rarr; <tt>&lt;N(L3)&gt;</tt></li>
 *       <li><b>R.advanceSNL();</b> &rarr; <tt>&lt;L2&nbsp;&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>delete R;</b></li>
 *    </ul>
 *    <li><b>SlipSequencer R = new SlipSequencer(L1);</b> &rarr; <tt>&lt;L1&gt;</tt></li>
 *    <ul type="none">
 *       <li><b>R.advanceSWR();</b> &rarr; <tt>&lt;D(a)&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceSWR();</b> &rarr; <tt>&lt;D(b)&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceSWR();</b> &rarr; <tt>&lt;N(L2)&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceSWR();</b> &rarr; <tt>&lt;D(c)&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceSWR();</b> &rarr; <tt>&lt;N(L3)&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceSWR();</b> &rarr; <tt>&lt;D(d)&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceSWR();</b> &rarr; <tt>&lt;L3&nbsp;&nbsp;&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>delete R;</b></li>
 *    </ul>
 *    <li><b>SlipSequencer R = new SlipSequencer(L1);</b> &rarr; <tt>&lt;L1&gt;</tt></li>
 *    <ul type="none">
 *       <li><b>R.advanceSWL();</b> &rarr; <tt>&lt;D(f)&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceSWL();</b> &rarr; <tt>&lt;N(L2)&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceSWL();</b> &rarr; <tt>&lt;D(e)&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceSWL();</b> &rarr; <tt>&lt;N(L3)&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceSWL();</b> &rarr; <tt>&lt;D(d)&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceSWL();</b> &rarr; <tt>&lt;L3&nbsp;&nbsp;&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>delete R;</b></li>
 *    </ul>
 *    <li><b>SlipSequencer R = new SlipSequencer(L1);</b> &rarr; <tt>&lt;L1&gt;</tt></li>
 *    <ul type="none">
 *       <li><b>R.advanceSNR();</b> &rarr; <tt>&lt;N(L2)&gt;</tt></li>
 *       <li><b>R.advanceSER();</b> &rarr; <tt>&lt;D(c)&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceLWL();</b> &rarr; <tt>&lt;L2&nbsp;&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>R.advanceLNR();</b> &rarr; <tt>&lt;N(L3)&gt;</tt></li>
 *       <li><b>R.advanceLNR();</b> &rarr; <tt>&lt;L2&nbsp;&nbsp;&nbsp;&gt;</tt></li>
 *       <li><b>delete R;</b></li>
 *    </ul>
 * </ul>
 * <p>Where &lt;&gt; represents &lt;current cell&gt;.</p>
 * <p>There are several additional methods which support sequencer operations.
 *    The user can reset the current cell reference, <em>reset()</em> and 
 *    retrieve a reference to the current cell, <em>currentCell()</em>. There 
 *    are other operations and methods which are conveniences to support the 
 *    user but which do not contribute to sequencer functionality. These are 
 *    all documented in the following sections.</p>
 * 
 * <p><b>Constructors/Destructors</b></p><hr>
 * <p>A null constructor is not available to the user. The user must
 *    provide a list header directly or indirectly through a reference
 *    to a sublist cell to construct the sequencer. If construction is done 
 *    on the stack, the system will automatically delete the sequencer 
 *    on scope exit, otherwise the user must explicitely delete the sequencer./p>
 * <ul>
 *    <li><b>SlipSequencer()</b> unavailable to the user.</li>
 *    <li><b>SlipSequencer(header)</b> construct an instance of a sequencer.</li>
 *    <li><b>SlipSequencer(sublist)</b> construct an instance of the sequencer
 *        iterator using (<em>SlipSublist</em>&) sublist.</li>
 *    <li><b>~SlipSequencer()</b> delete the Sequencer.</li>
 * </ul>
 * 
 * <p><b>Iteration</b></p><hr>
 * <p>The linear and structural iteration methods have been described above.
 *    They are listed here:</p>
 * <ul>
 *    <li><b>advanceLER()</b> advance linearly to the right and stop at a 
 *        <em>SlipDatum</em> cell.</li>
 *    <li><b>advanceLEL()</b> advance linearly to the left and stop at a 
 *        <em>SlipDatum</em> cell.</li>
 *    <li><b>advanceLNR()</b> advance linearly to the right and stop at a 
 *        <em>SlipSublist</em> cell.</li>
 *    <li><b>advanceLNL()</b> advance linearly to the left and stop at a 
 *        <em>SlipSublist</em> cell.</li>
 *    <li><b>advanceLWR()</b> advance linearly to the right and stop at a 
 *        <em>SlipDatum</em>, <em>SlipSublist</em>, or <em>SlipHeader</em> 
 *        cell.</li>
 *    <li><b>advanceLWL()</b> advance linearly to the left and stop at a 
 *        <em>SlipDatum</em>, <em>SlipSublist</em>, or <em>SlipHeader</em> 
 *        cell.</li>
 *    <li><b>advanceSER()</b> advance structurally to the right and stop at a 
 *        <em>SlipDatum</em> cell.</li>
 *    <li><b>advanceSEL()</b> advance structurally to the left and stop at a 
 *        <em>SlipDatum</em> cell.</li>
 *    <li><b>advanceSNR()</b> advance structurally to the right and stop at a 
 *        <em>SlipSublist</em> cell.</li>
 *    <li><b>advanceSNL()</b> advance structurally to the left and stop at a 
 *        <em>SlipSublist</em> cell.</li>
 *    <li><b>advanceSWR()</b> advance structurally to the right and stop at a 
 *        <em>SlipDatum</em>, <em>SlipSublist</em>, or <em>SlipHeader</em> 
 *        cell.</li>
 *    <li><b>advanceSWL()</b> advance structurally to the left and stop at a 
 *        <em>SlipDatum</em>, <em>SlipSublist</em>, or <em>SlipHeader</em> 
 *        cell.</li>
 * </ul>
 * 
 * <p><b>Iteration Miscellaneous</b></p>
 * <p>Several methods support the use of the sequencer. They do not iterate over
 *    a list but support the user.</p>
 * <ul>
 *    <li><b>currentCell()</b> return a reference to the current sequencer 
 *        current cell.</li>
 *    <li><b>reset()</b> reset the current cell to a <b>SlipCell</b>.</li>
 * </ul>
 * 
 * <p><b>Convenience Methods</b></p>
 * <p>These methods provide increased usability. They provide access to
 *    existing methods without requiring that the user extract a cell 
 *    reference (R.currentCell()), and they maintain sequencer consistency 
 *    when the operation invalidates the <em>currentCell()</em>.</p>
 * <p>Consistency assures the user that the operation and the sequencer are
 *    in synchrony. Operations done outside of the sequencer may place the
 *    sequencer in an inconsistent state. Operations done with the sequencer
 *    ensure that the sequencer is in a consistent state before and after
 *    the operation.</p>
 * 
 * <p><b>Cell Convenience Methods</b></p><hr>
 * <p>Cell methods maintain the consistency of the sequencer before and after
 *    execution. The methods are a short-hand version of SLIP operations using 
 *    the current cell as a default in the operation.</p>
 * <ul>
 *    <li><b>deleteCell()</b> delete the current cell and perform a 
 *        <em>advanceLWL()</em>.</li>
 *    <li><b>insLeft()</b> insert the input cell to the left of the 
 *        current cell.</li>
 *    <li><b>insRight()</b> insert the input cell to the right of the 
 *        current cell.</li></li>
 *    <li><b>moveLeft()</b> move the input cell to the left of the
 *        current cell.</li>
 *    <li><b>moveRight()</b> move the input cell to the right of the
 *        current cell.</li>
 *    <li><b>replace()</b> replace the current cell with the input value
 *        and set the current cell to the replacement cell.</li>
 *    <li><b>unlink()</b> unlinks the current cell from a list and advances
 *        the sequencer one word to the left.</li>
 * </ul>
 * 
 * <p><b>Operators</b></p><hr>
 * <p></p>
 * <ul>
 *    <li><b>operator=()</b> Assignment can be to the sequencer or to a cell.</li>
 *    <ul>
 *        <li>List assignment replaces the sequencer current cell with a 
 *            pointer to the SlipHeader object.</li>
 *        <li>Sequencer assignment replaces the current cell with a the RHS current
 *            cell reference.</li>
 *        <li>Sequencer assignment replaces the current cell with a the RHS current
 *            cell reference.</li>
 *        <li>Cell assignment assigns like-to-like. A RHS <em>SlipDatum</em> 
 *            cell can be assigned to a LHS <em>SlipDatum</em> cell and a RHS 
 *            <em>SlipSublist</em> cell can be assigned to a LHS 
 *            <em>SlipSublist</em> cell. Unlike ordinary cell assignment
 *            (see slip::SlipDatum) the assignment of a <em>SlipHeader</em>
 *            reference to a <em>SlipSublist</em> cell can not be done.</li>
 *    </ul>
 *    <li><b>operator==()</b> compares the current cell to the RHS. 
 *        If a <em>SlipSequencer</em> is given as the RHS then the comparison is 
 *        <b>true</b> if both the current cells are equal.></li>
 *    <li><b>operator!=()</b> compares the LHS to the RHS <em>slip::SlipCell</em>. 
 *        If a <em>SlipSequencer</em> is given as the RHS then the comparison is 
 *        <b>true</b> if both the current cells are unequal.></li>
 * </ul>
 * 
 * <p><b>Miscellaneous</b></p><hr>
 * <p></p>
 * <ul>
 *    <li><b>getClassType()</b> return the class type of the current cell.</li>
 *    <li><b>getName()</b> return a string representing the <em>SlipDatum</em>
 *        type of the current cell.</li>
 *    <li><b>isData()</b> <b>true</b> if the current cell is data.</li>
 *    <li><b>isHeader()</b> <b>true</b> if the current cell is a 
 *        <em>SlipHeader</em>/li>
 *    <li><b>isName()</b> <b>true</b> if the current cell is a 
 *        <em>SlipSublist</em>.</li>
 *    <li><b>isNumber()</b> <b>true</b> if the current cell is a 
 *        <em>SlipDatum</em> number.</li>
 *    <li><b>isPtr()</b> <b>true</b> if the current cell is a 
 *        <em>SlipDatum</em> pointer.</li>
 *    <li><b>isSublist()</b> <b>true</b> if the current cell is a 
 *        <em>SlipSublist</em>.</li>
 *    <li><b>toString()</b> pretty-print the current cell.</li>
 *    <li><b>dump()</b> uglify the current cell.</li>
 * </ul>
 * <p><b>NOTES</b></p><hr>
 * <ol>
 *    <li>Deletion of a list containing a cell referenced by a sequencer
 *        will cause the sequencer to go out of synchrony when the list
 *        cells are returned to the AVSL.</li>
 *    <li>Deletion of a cell referenced by a sequencer will cause the
 *        sequencer to go out of synchrony.
 *    </li>
 * </ol>
 * 
 */
   
# ifdef SLIPASGN
#   undef SLIPASGN
# endif
# define SLIPASGN(X)     {  *link = (X); return *this; }
   
# ifdef SLIPEQUAL
#   undef SLIPEQUAL
# endif
# define SLIPEQUAL(X)    {  return ((SlipDatum&)currentCell()) == (X);}
   
# ifdef SLIPNOEQUAL
#   undef SLIPNOEQUAL
# endif
# define SLIPNOEQUAL(X)  {  return ((SlipDatum&)currentCell()) != (X);}
   
# ifdef SLIPADVANCE
#   undef SLIPADVANCE
# endif
# define SLIPADVANCE(X)  { reader = reader->X(); return *this; }  

# ifdef SLIPINSLEFT
#   undef SLIPINSLEFT
# endif
# define SLIPINSLEFT(X)  { return link->insLeft((X)); }
   
# ifdef SLIPINSRIGHT
#   undef SLIPINSRIGHT
# endif
# define SLIPINSRIGHT(X) { return link->insRight((X)); }
   
# ifdef SLIPREPLACE
#   undef SLIPREPLACE
# endif
# define SLIPREPLACE(X)  { return *(link = &(link->replace((X)))); }
   
   class SlipReader;

   class SlipSequencer  {
    /*********************************************************
     *                   Private Functions                   *
    **********************************************************/
    private:
       SlipCell* link;                                                          //!< link to current cell

       SlipSequencer();

    /*********************************************************
     *                  Protected Functions                  *
    **********************************************************/
    protected:

   /*********************************************************
    *                   Public Functions                    *
   **********************************************************/
    public:
       SlipSequencer(SlipHeader& header);
       SlipSequencer(SlipSublist& sublist);
       SlipSequencer(const SlipCell& orig);
       ~SlipSequencer();
            /*****************************************
             *           Sequencer Iteration         *
            ******************************************/
      SlipSequencer&     advanceLEL();                                          // advance linear element left,  true if element found
      SlipSequencer&     advanceLER();                                          // advance linear element right, true if element found
      SlipSequencer&     advanceLNL();                                          // advance linear name left,     true if element found
      SlipSequencer&     advanceLNR();                                          // advance linear name right,    true if element found
      SlipSequencer&     advanceLWL();                                          // advance linear word right,    true if element found
      SlipSequencer&     advanceLWR();                                          // advance linear word right,    true if element found
      SlipSequencer&     advanceSEL();                                          // advance structural element left,  true if element found
      SlipSequencer&     advanceSER();                                          // advance structural element right, true if element found
      SlipSequencer&     advanceSNL();                                          // advance structural name left,     true if element found
      SlipSequencer&     advanceSNR();                                          // advance structural name right,    true if element found
      SlipSequencer&     advanceSWL();                                          // advance structural word right,    true if element found
      SlipSequencer&     advanceSWR();                                          // advance structural word right,    true if element found

            /*****************************************
             *         Sequencer Assignment          *
            ******************************************/
      SlipSequencer&      operator=(const SlipHeader& X);                        // Y = V
   
            /*****************************************
             *            Cell Assignment            *
            ******************************************/
       SlipSequencer&  operator=(const SlipDatum& X)     SLIPASGN(X)           // Y = V
       SlipSequencer&  operator=(const SlipReader& X);                          
       SlipSequencer&  operator=(const SlipSequencer& X) SLIPASGN((X.currentCell()))  // Y  = V
       SlipSequencer&  operator=(const SlipCell&   X)    SLIPASGN(X)           // Y = V
       SlipSequencer&  operator=(const bool    X)        SLIPASGN(X)           // Y = V
       SlipSequencer&  operator=(const CHAR    X)        SLIPASGN(X)           // Y = V
       SlipSequencer&  operator=(const DOUBLE  X)        SLIPASGN(X)           // Y = V
       SlipSequencer&  operator=(const LONG    X)        SLIPASGN(X)           // Y = V
       SlipSequencer&  operator=(const UCHAR   X)        SLIPASGN(X)           // Y = V
       SlipSequencer&  operator=(const ULONG   X)        SLIPASGN(X)           // Y = V
       SlipSequencer&  operator=(const PTR     X)        SLIPASGN(X)           // Y = V
       SlipSequencer&  operator=(const string& X)        SLIPASGN(X)           // Y = V
       SlipSequencer&  operator=(const string* X)        SLIPASGN(*X)          // Y = V
      
            /*****************************************
             *             Cell Equality             *
            ******************************************/
       bool         operator==(const SlipReader&  X);
       bool         operator==(const SlipSequencer&  X) SLIPEQUAL(X.currentCell()) // Y == V
       bool         operator==(const SlipCell& X)       SLIPEQUAL(X)           // Y == V
       bool         operator==(const bool    X)         SLIPEQUAL(X)           // Y == V
       bool         operator==(const CHAR    X)         SLIPEQUAL(X)           // Y == V
       bool         operator==(const DOUBLE  X)         SLIPEQUAL(X)           // Y == V
       bool         operator==(const LONG    X)         SLIPEQUAL(X)           // Y == V
       bool         operator==(const UCHAR   X)         SLIPEQUAL(X)           // Y == V
       bool         operator==(const ULONG   X)         SLIPEQUAL(X)           // Y == V
       bool         operator==(const PTR     X)         SLIPEQUAL(X)           // Y == V
       bool         operator==(const string& X)         SLIPEQUAL(X)           // Y == V
       bool         operator==(const string* X)         SLIPEQUAL(*X)          // Y == V

       bool         operator!=(const SlipReader&  X);                          // Y != V
       bool         operator!=(const SlipSequencer& X);                        // Y != V
       bool         operator!=(const SlipCell& X);                             // Y != V
       bool         operator!=(const bool    X)        SLIPNOEQUAL(X)          // Y != V
       bool         operator!=(const CHAR    X)        SLIPNOEQUAL(X)          // Y != V
       bool         operator!=(const DOUBLE  X)        SLIPNOEQUAL(X)          // Y != V
       bool         operator!=(const LONG    X)        SLIPNOEQUAL(X)          // Y != V
       bool         operator!=(const UCHAR   X)        SLIPNOEQUAL(X)          // Y != V
       bool         operator!=(const ULONG   X)        SLIPNOEQUAL(X)          // Y != V
       bool         operator!=(const PTR     X)        SLIPNOEQUAL(X)          // Y != V
       bool         operator!=(const string& X)        SLIPNOEQUAL(X)          // Y != V
       bool         operator!=(const string* X)        SLIPNOEQUAL(*X)         // Y != V

            /*****************************************
             *           Manipulating Cells          *
            ******************************************/
                                                                               // List Cells must be unlinked
      SlipCell&          insLeft(SlipReader&  X);                              // Insert X to current cell left
      SlipCell&          insLeft(SlipSequencer& X);                            // Insert X to current cell left
      SlipCell&          insLeft(SlipCell&  X)         SLIPINSLEFT(X)          // Insert X to current cell left
      SlipCell&          insLeft(bool       X)         SLIPINSLEFT(X)          // Insert X to current cell left
      SlipCell&          insLeft(DOUBLE     X)         SLIPINSLEFT(X)          // Insert X to current cell left
      SlipCell&          insLeft(ULONG      X)         SLIPINSLEFT(X)          // Insert X to current cell left
      SlipCell&          insLeft(LONG       X)         SLIPINSLEFT(X)          // Insert X to current cell left
      SlipCell&          insLeft(UCHAR      X)         SLIPINSLEFT(X)          // Insert X to current cell left
      SlipCell&          insLeft(CHAR       X)         SLIPINSLEFT(X)          // Insert X to current cell left
      SlipCell&          insLeft(const PTR  X, const void* operation = ptrOP)  // Insert X to current cell left
                                                       SLIPINSLEFT((X, operation))
      SlipCell&          insLeft(const string& X, bool constFlag = false)      // Insert X to current cell left
                                                       SLIPINSLEFT((X, constFlag))
      SlipCell&          insLeft(const string* X, bool constFlag = false)      // Insert X to current cell left
                                                       SLIPINSLEFT((*X, constFlag))
                                                                               // List Cells must be unlinked
      SlipCell&          insRight(SlipReader&  X);                             // Insert X to current cell left
      SlipCell&          insRight(SlipSequencer& X);                           // Insert X to current cell left
      SlipCell&          insRight(SlipCell&  X)        SLIPINSRIGHT(X)         // Insert X to current cell left
      SlipCell&          insRight(bool       X)        SLIPINSRIGHT(X)         // Insert X to current cell left
      SlipCell&          insRight(DOUBLE     X)        SLIPINSRIGHT(X)         // Insert X to current cell left
      SlipCell&          insRight(ULONG      X)        SLIPINSRIGHT(X)         // Insert X to current cell left
      SlipCell&          insRight(LONG       X)        SLIPINSRIGHT(X)         // Insert X to current cell left
      SlipCell&          insRight(UCHAR      X)        SLIPINSRIGHT(X)         // Insert X to current cell left
      SlipCell&          insRight(CHAR       X)        SLIPINSRIGHT(X)         // Insert X to current cell left
      SlipCell&          insRight(const PTR  X, const void* operation = ptrOP) // Insert X to current cell right
                                                       SLIPINSRIGHT((X, operation))
      SlipCell&          insRight(const string&    X, bool constFlag = false)  // Insert X to current cell right
                                                       SLIPINSRIGHT((X, constFlag))
      SlipCell&          insRight(const string*    X, bool constFlag = false)  // Insert X to current cell right
                                                       SLIPINSRIGHT((*X, constFlag))

      SlipCell&          moveLeft(SlipCell& X) { currentCell().moveLeft(X); return currentCell(); }  // Move the current cell to the left of X
      SlipCell&          moveLeft(SlipReader& X);                               // Move the current cell to the left of X
      SlipCell&          moveLeft(SlipSequencer& X);                            // Move the current cell to the left of X
      SlipCell&          moveRight(SlipCell& X) { currentCell().moveRight(X); return currentCell(); }  // Move the current cell to the right of X
      SlipCell&          moveRight(SlipReader& X);                              // Move the current cell to the right of X
      SlipCell&          moveRight(SlipSequencer& X);                           // Move the current cell to the right of X

      SlipCell&          moveListLeft(SlipCell& X) { currentCell().moveListLeft(X); return currentCell(); } // Move the current cell to the left of X
      SlipCell&          moveListLeft(SlipReader& X);                           // Move the current cell to the left of X
      SlipCell&          moveListLeft(SlipSequencer& X);                        // Move the current cell to the left of X
      SlipCell&          moveListRight(SlipCell& X) { currentCell().moveListRight(X); return currentCell(); } // Move the current cell to the right of X
      SlipCell&          moveListRight(SlipReader& X);                          // Move the current cell to the right of X
      SlipCell&          moveListRight(SlipSequencer& X);                       // Move the current cell to the right of X

      SlipCell&          replace(const SlipReader& X)     SLIPREPLACE(X.currentCell()) // Replace a cell on a list with a new cell
      SlipCell&          replace(const SlipSequencer& X)  SLIPREPLACE(X.currentCell()) // Replace a cell on a list with a new cell
      SlipCell&          replace(const SlipCell& X)    SLIPREPLACE(X)          // Replace a cell on a list with a new cell
      SlipCell&          replace(bool X)               SLIPREPLACE(X)          // Replace a cell on a list with a new cell
      SlipCell&          replace(CHAR X)               SLIPREPLACE(X)          // Replace a cell on a list with a new cell
      SlipCell&          replace(UCHAR X)              SLIPREPLACE(X)          // Replace a cell on a list with a new cell
      SlipCell&          replace(LONG X)               SLIPREPLACE(X)          // Replace a cell on a list with a new cell
      SlipCell&          replace(ULONG X)              SLIPREPLACE(X)          // Replace a cell on a list with a new cell
      SlipCell&          replace(DOUBLE X)             SLIPREPLACE(X)          // Replace a cell on a list with a new cell
      SlipCell&          replace(const PTR X, const void* operation = ptrOP)   // Replace a cell on a list with a new cell
                                                       SLIPREPLACE((X, operation))
      SlipCell&          replace(const string& X, bool constFlag = false)      // Replace a cell on a list with a new cell
                                                       SLIPREPLACE((X, constFlag))
      SlipCell&          replace(const string* X, bool constFlag = false)      // Replace a cell on a list with a new cell
                                                       SLIPREPLACE((X, constFlag))

      SlipCell&          unlink();                                              // Unlink cell from list.
      
            /*****************************************
             *   Miscellaneous Sequencer Methods     *
            ******************************************/
       SlipSequencer&     reset(SlipCell& X);
       SlipSequencer&     reset(SlipHeader& X);
       SlipSequencer&     reset(SlipReader& X);
       SlipSequencer&     reset(SlipSequencer& X);
       SlipCell&          currentCell() const  { return *link;  }              // Return current SLIP cell being referenced
       string             toString() { return link->toString(); }              // Pretty-print the cell
       string             dump()     { return link->SlipCellBase::dump(); }    // uglify the cell
 
            /*****************************************
             *             Miscellaneous             *
            ******************************************/
       SlipSequencer&     deleteCell();                                          // delete the current cell
       ClassType    getClassType() const { return currentCell().getClassType();}// get class type
       SlipCell*    getLeftLink()  const { return (SlipCell*)currentCell().getLeftLink(); } // Return pointer to previous cell
       string       getName()      const { return currentCell().getName();   }  // return ascii name of cell
       SlipCell*    getRightLink() const { return (SlipCell*)currentCell().getRightLink();} // Return pointer to next cell
       bool         isData()       const { return currentCell().isData();    }  // true if cell is data
       bool         isDiscrete()   const { return currentCell().isDiscrete();}  // true if cell contains a discrete number
       bool         isHeader()     const { return currentCell().isHeader();  }  // true if cell is a header
       bool         isName()       const { return currentCell().isName();    }  // true if cell is sublist
       bool         isNumber()     const { return currentCell().isNumber();  }  // true if cell is numeric data
       bool         isPtr()        const { return currentCell().isPtr();     }  // true if pointer
       bool         isReal()       const { return currentCell().isReal();    }  // true if cell contains a real number
       bool         isString()     const { return currentCell().isString();  }  // true if cell contains a string
       bool         isSublist()    const { return currentCell().isSublist(); }  // true if cell is sublist

   }; // class SlipSequencer


# undef SLIPASGN
# undef SLIPEQUAL
# undef SLIPNOEQUAL
# undef SLIPADVANCE
# undef SLIPINSLEFT
# undef SLIPINSRIGHT
# undef SLIPREPLACE

}; // namespace slip

#endif	/* SLIPREADER_H */

