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
 * @file   SlipReader.h
 * @author A. Schwarz
 * @date   July 18, 2012
 *
 * @brief User interface to the structured list iterator.
 *
 * 
 */

#ifndef SLIPREADER_H
#define	SLIPREADER_H

# include "SlipGlobal.h"
# include "SlipHeader.h"
# include "SlipReaderCell.h"
# include "SlipSublist.h"

namespace slip {
/**
 * @class slip::SlipReader
 * 
 * @brief User interface to the structured (memoized) list iterator.
 * 
 * <p><b>Overview</b></p>
 * <p>Linear iteration is the process of visiting cells in a single list.
 *    Structured iteration is the processes of visiting cells in a list
 *    and sublist(s). Memoize is the memory of the sublists entered and the 
 *    ability to use this memory to 'return' to the containing list referencing
 *    the iterated sublist. The reader interface provides a means to do
 *    linear and structured iteration over a list with a memory to allow
 *    'return' from iteration over a sublist to it's containing list.</p>
 * <p>The reader interface provides linear and structured left and right 
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
 *    and at the topmost list header for structured (S) iterations.</p>
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
 *        stop at the topmost list head after a depth first search is
 *        made of all sublists starting at the search start.</li>
 *    <li><b>direction</b> the direction of the iteration starting at the
 *        current cell. The iteration will proceed to succeeding cells for 
 *        <b>Left</b> and to succeeding next cells for <b>Right</b>.
 *        Sublist entry/exit depends on the iteration type, <b>Linear</b> 
 *        <b>Structured</b>.</li>
 * </ul>
 * <p>As the structured reader descends into sublists a chain of back linked
 *    SlipReaderCells is formed. This chain allows ascent from a sublist to
 *    its containing list and forms a memory of the sublists traversed up to
 *    the current list. The chain is managed by the underlying system and is
 *    inaccessible to the user.</p>
 * <p>The current reader represents the last sublist iterated. The current
 *    reader has a number of user accessible fields. They are:</p>
 * <ul>
 *    <li><b>current list</b> a reference to the list header (SlipHeader) 
 *        being iterated.</li>
 *    <li><b>current cell</b> a reference to the cell in the list that
 *        the reader at. This data can be a data cell (SlipDatum), a
 *        sublist cell (SlipSublist, or the list header (SlipHeader).</li>
 *    <li><b>depth</b> the number of sublists traversed. The topmost
 *        sublist has a depth of zero (0). This depth is incremented
 *        by one (1) for each sublist in the reader chain.</li>
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
 *   L1( D(a) D(b) N() D(f) )
 *                 |
 *                 |
 *                 o L2( D(c) N() D(e) )
 *                            |
 *                            |
 *                            o L3( D(d) )
 * </pre></tt>
 * <p>The following examples show the effect of each of the readers over the
 *    list. The sublist references will be returned as N(H) representing
 *    a sublist, N, and the list it references, H. In our example there are
 *    two sublists, N(L2) and N(L3) representing the sublist cells referencing
 *    lists L2 and L3 respectively.</p>
 * <p>All readers start at L1.</p>
 * <ul>
 *    <li><b>SlipReader R = new SlipReader(L1);</b> &rarr; <tt>&lt;L1 L1 0&gt;</tt></li>
 *    <ul type="none">
 *       <li><b>R.advanceLER();</b> &rarr; <tt>&lt;L1 D(a)&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>R.advanceLER();</b> &rarr; <tt>&lt;L1 D(b)&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>R.advanceLER();</b> &rarr; <tt>&lt;L1 D(f)&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>R.advanceLER();</b> &rarr; <tt>&lt;L1 L1&nbsp;&nbsp;&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>delete R;</b></li>
 *    </ul>
 *    <li><b>SlipReader R = new SlipReader(L1);</b> &rarr; <tt>&lt;L1 L1 0&gt;</tt></li>
 *    <ul type="none">
 *       <li><b>R.advanceLEL();</b> &rarr; <tt>&lt;L1 D(f)&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>R.advanceLEL();</b> &rarr; <tt>&lt;L1 D(b)&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>R.advanceLEL();</b> &rarr; <tt>&lt;L1 D(a)&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>R.advanceLEL();</b> &rarr; <tt>&lt;L1 L1&nbsp;&nbsp;&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>delete R;</b></li>
 *    </ul>
 *    <li><b>SlipReader R = new SlipReader(L1);</b> &rarr; <tt>&lt;L1 L1 0&gt;</tt></li>
 *    <ul type="none">
 *       <li><b>R.advanceLNR();</b> &rarr; <tt>&lt;L1 N(L2) 0&gt;</tt></li>
 *       <li><b>R.advanceLNR();</b> &rarr; <tt>&lt;L1 L1&nbsp;&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>delete R;</b></li>
 *    </ul>
 *    <li><b>SlipReader R = new SlipReader(L1);</b> &rarr; <tt>&lt;L1 L1 0&gt;</tt></li>
 *    <ul type="none">
 *       <li><b>R.advanceLNL();</b> &rarr; <tt>&lt;L1 N(L2) 0&gt;</tt></li>
 *       <li><b>R.advanceLNL();</b> &rarr; <tt>&lt;L1 L1&nbsp;&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>delete R;</b></li>
 *    </ul>
 *    <li><b>SlipReader R = new SlipReader(L1);</b> &rarr; <tt>&lt;L1 L1&nbsp;&nbsp;&nbsp; 0&gt;</tt></li>
 *    <ul type="none">
 *       <li><b>R.advanceLWR();</b> &rarr; <tt>&lt;L1 D(a)&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>R.advanceLWR();</b> &rarr; <tt>&lt;L1 D(b)&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>R.advanceLWR();</b> &rarr; <tt>&lt;L1 N(L2)&nbsp; 0&gt;</tt></li>
 *       <li><b>R.advanceLWR();</b> &rarr; <tt>&lt;L1 D(f)&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>R.advanceLWR();</b> &rarr; <tt>&lt;L1 L1&nbsp;&nbsp;&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>delete R;</b></li>
 *    </ul>
 *    <li><b>SlipReader R = new SlipReader(L1);</b> &rarr; <tt>&lt;L1 L1 0&gt;</tt></li>
 *    <ul type="none">
 *       <li><b>R.advanceLWL();</b> &rarr; <tt>&lt;L1 D(f)&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>R.advanceLWL();</b> &rarr; <tt>&lt;L1 N(L2)&nbsp; 0&gt;</tt></li>
 *       <li><b>R.advanceLWL();</b> &rarr; <tt>&lt;L1 D(b)&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>R.advanceLWL();</b> &rarr; <tt>&lt;L1 D(a)&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>R.advanceLWL();</b> &rarr; <tt>&lt;L1 L1&nbsp;&nbsp;&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>delete R;</b></li>
 *    </ul>
 *    <li><b>SlipReader R = new SlipReader(L1);</b> &rarr; <tt>&lt;L1 L1 0&gt;</tt></li>
 *    <ul type="none">
 *       <li><b>R.advanceSER();</b> &rarr; <tt>&lt;L1 D(a)&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>R.advanceSER();</b> &rarr; <tt>&lt;L1 D(b)&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>R.advanceSER();</b> &rarr; <tt>&lt;L2 D(c)&nbsp;&nbsp; 1&gt;</tt></li>
 *       <li><b>R.advanceSER();</b> &rarr; <tt>&lt;L3 D(d)&nbsp;&nbsp; 2&gt;</tt></li>
 *       <li><b>R.advanceSER();</b> &rarr; <tt>&lt;L2 D(e)&nbsp;&nbsp; 1&gt;</tt></li>
 *       <li><b>R.advanceSER();</b> &rarr; <tt>&lt;L1 D(f)&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>R.advanceSER();</b> &rarr; <tt>&lt;L1 L1&nbsp;&nbsp;&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>delete R;</b></li>
 *    </ul>
 *    <li><b>SlipReader R = new SlipReader(L1);</b> &rarr; <tt>&lt;L1 L1 0&gt;</tt></li>
 *    <ul type="none">
 *       <li><b>R.advanceSEL();</b> &rarr; <tt>&lt;L1 D(f)&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>R.advanceSEL();</b> &rarr; <tt>&lt;L2 D(e)&nbsp;&nbsp; 1&gt;</tt></li>
 *       <li><b>R.advanceSEL();</b> &rarr; <tt>&lt;L3 D(d)&nbsp;&nbsp; 2&gt;</tt></li>
 *       <li><b>R.advanceSEL();</b> &rarr; <tt>&lt;L2 D(c)&nbsp;&nbsp; 1&gt;</tt></li>
 *       <li><b>R.advanceSEL();</b> &rarr; <tt>&lt;L1 D(b)&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>R.advanceSEL();</b> &rarr; <tt>&lt;L1 D(a)&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>R.advanceSEL();</b> &rarr; <tt>&lt;L1 L1&nbsp;&nbsp;&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>delete R;</b></li>
 *    </ul>
 *    <li><b>SlipReader R = new SlipReader(L1);</b> &rarr; <tt>&lt;L1 L1&nbsp;&nbsp;&nbsp; 0&gt;</tt></li>
 *    <ul type="none">
 *       <li><b>R.advanceSNR();</b> &rarr; <tt>&lt;L1 N(L2) 0&gt;</tt></li>
 *       <li><b>R.advanceSNR();</b> &rarr; <tt>&lt;L2 N(L3) 1&gt;</tt></li>
 *       <li><b>R.advanceSNR();</b> &rarr; <tt>&lt;L1 L1&nbsp;&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>delete R;</b></li>
 *    </ul>
 *    <li><b>SlipReader R = new SlipReader(L1);</b> &rarr; <tt>&lt;L1 L1 0&gt;</tt></li>
 *    <ul type="none">
 *       <li><b>R.advanceSNL();</b> &rarr; <tt>&lt;L1 N(L2) 0&gt;</tt></li>
 *       <li><b>R.advanceSNL();</b> &rarr; <tt>&lt;L2 N(L3) 1&gt;</tt></li>
 *       <li><b>R.advanceSNL();</b> &rarr; <tt>&lt;L1 L1&nbsp;&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>delete R;</b></li>
 *    </ul>
 *    <li><b>SlipReader R = new SlipReader(L1);</b> &rarr; <tt>&lt;L1 L1 0&gt;</tt></li>
 *    <ul type="none">
 *       <li><b>R.advanceSWR();</b> &rarr; <tt>&lt;L1 D(a)&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>R.advanceSWR();</b> &rarr; <tt>&lt;L1 D(b)&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>R.advanceSWR();</b> &rarr; <tt>&lt;L1 N(L2)&nbsp; 0&gt;</tt></li>
 *       <li><b>R.advanceSWR();</b> &rarr; <tt>&lt;L2 D(c)&nbsp;&nbsp; 1&gt;</tt></li>
 *       <li><b>R.advanceSWR();</b> &rarr; <tt>&lt;L2 N(L3)&nbsp; 1&gt;</tt></li>
 *       <li><b>R.advanceSWR();</b> &rarr; <tt>&lt;L3 D(d)&nbsp;&nbsp; 2&gt;</tt></li>
 *       <li><b>R.advanceSWR();</b> &rarr; <tt>&lt;L2 D(e)&nbsp;&nbsp; 1&gt;</tt></li>
 *       <li><b>R.advanceSWR();</b> &rarr; <tt>&lt;L1 D(f)&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>R.advanceSWR();</b> &rarr; <tt>&lt;L1 L1&nbsp;&nbsp;&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>delete R;</b></li>
 *    </ul>
 *    <li><b>SlipReader R = new SlipReader(L1);</b> &rarr; <tt>&lt;L1 L1 0&gt;</tt></li>
 *    <ul type="none">
 *       <li><b>R.advanceSWL();</b> &rarr; <tt>&lt;L1 D(f)&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>R.advanceSWL();</b> &rarr; <tt>&lt;L1 N(L2)&nbsp; 0&gt;</tt></li>
 *       <li><b>R.advanceSWL();</b> &rarr; <tt>&lt;L2 D(e)&nbsp;&nbsp; 1&gt;</tt></li>
 *       <li><b>R.advanceSWL();</b> &rarr; <tt>&lt;L2 N(L3)&nbsp; 1&gt;</tt></li>
 *       <li><b>R.advanceSWL();</b> &rarr; <tt>&lt;L3 D(d)&nbsp;&nbsp; 2&gt;</tt></li>
 *       <li><b>R.advanceSWL();</b> &rarr; <tt>&lt;L2 D(c)&nbsp;&nbsp; 1&gt;</tt></li>
 *       <li><b>R.advanceSWL();</b> &rarr; <tt>&lt;L1 D(b)&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>R.advanceSWL();</b> &rarr; <tt>&lt;L1 D(a)&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>R.advanceSWL();</b> &rarr; <tt>&lt;L1 L1&nbsp;&nbsp;&nbsp;&nbsp; 0&gt;</tt></li>
 *       <li><b>delete R;</b></li>
 *    </ul>
 *    <li><b>SlipReader R = new SlipReader(L1);</b> &rarr; <tt>&lt;L1 L1 0&gt;</tt></li>
 *    <ul type="none">
 *       <li><b>R.advanceSNR();</b> &rarr; <tt>&lt;L1 N(L2) 0&gt;</tt></li>
 *       <li><b>R.advanceSER();</b> &rarr; <tt>&lt;L2 D(c)&nbsp; 1&gt;</tt></li>
 *       <li><b>R.advanceLWL();</b> &rarr; <tt>&lt;L2 L2&nbsp;&nbsp;&nbsp; 1&gt;</tt></li>
 *       <li><b>R.advanceLNR();</b> &rarr; <tt>&lt;L2 N(L3) 1&gt;</tt></li>
 *       <li><b>R.advanceLNR();</b> &rarr; <tt>&lt;L2 L2&nbsp;&nbsp;&nbsp; 1&gt;</tt></li>
 *       <li><b>delete R;</b></li>
 *    </ul>
 * </ul>
 * <p>Where the 3-tuple &lt;&gt; represents &lt;current list, current cell, 
 *    depth&gt;.</p>
 * <p>There are several additional methods which support reader operations.
 *    The user can reset the current cell reference to the header of the 
 *    current list, <em>reset()</em>, return to the sublist cell in the 
 *    topmost list, <em>resetTop()</em>, and return to the sublist cell
 *    referencing the current list, <em>upLevel()</em>, and retrieve a
 *    reference to the current list <em>currentList()</em>, and current 
 *    cell, <em>currentCell()</em>, and get the <em>listDepth()</em>. There 
 *    are other operations and methods which are conveniences to support the 
 *    user but which do not contribute to reader functionality. These are all 
 *    documented in the following sections.</p>
 * 
 * <p><b>Constructors/Destructors</b></p>
 * <p>A null constructor is not available to the user. The user must
 *    provide a list header directly or indirectly through a reference
 *    to a sublist cell to construct the reader. If construction
 *    is done on the stack, the system will automatically delete the
 *    reader on scope exit, otherwise the user must explicitely
 *    delete the reader. On deletion, the reader chain is deleted./p>
 * <ul>
 *    <li><b>SlipReader()</b> unavailable to the user.</li>
 *    <li><b>SlipReader(header)</b> construct an instance of the reader
 *        iterator using (<em>SlipHeader</em>&) header as the topmost list.</li>
 *    <li><b>SlipReader(sublist)</b> construct an instance of the reader
 *        iterator using (<em>SlipSublist</em>&) sublist.<em>getHeader()</em>
 *        as the topmost list.</li>
 *    <li><b>~SlipReader()</b> delete the reader chain.</li>
 * </ul>
 * 
 * <p><b>Iteration</b></p>
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
 * <p>Several methods support the use of the reader. They do not iterate over
 *    a list but support the user in using the reader.</p>
 * <p>The current reader will always reference the list or sublist being
 *    iterated. If the current reader is iterating a sublist, then all the 
 *    preceeding readers on the reader chain will have a current
 *    cell referencing the <em>SlipSublist</em> cell which caused entry to
 *    the current sublist. If the current reader is at the topmost list then
 *    commands which cause a change in the list being traversed will be ignored.</p>
 * <ul>
 *    <li><b>currentCell()</b> return a reference to the current reader current
 *        cell.</li>
 *    <li><b>currentList()</b> return a reference to the current reader current
 *        list.</li>
 *    <li><b>listDepth()</b> return the current reader depth.</li>
 *    <li><b>reset()</b> set the current reader current cell to the current 
 *        list.</li>
 *    <li><b>resetTop()</b> delete all readers on the reader chain up to but
 *        not including the topmost list. The current cell will be the 
 *        <em>SlipSublist</em> cell used in the initial list descension.If the
 *        reader is at the topmost list, this method does nothing.</li>
 *    <li><b>upLevel()</b> delete the current reader. This will restore the
 *        reader chain to that which existed when it descended to the current
 *        list. If this is the topmost list, do nothing.</li>
 * </ul>
 * 
 * <p><b>Convenience Methods</b></p>
 * <p>These methods provide increased usability. They provide access to
 *    existing methods without requiring that the user extract a list
 *    reference (R.currentList()) or a cell reference (R.currentCell()), 
 *    and they maintain reader consistency when the operation invalidates
 *    either the <em>currentCell()</em> or <em>currentList()</em>.</p>
 * <p>Consistency assures the user that the operation and the reader are
 *    in synchrony. Operations done outside of the reader may place the
 *    reader in an inconsistent state. Operations done with the reader
 *    ensure that the reader is in a consistent state before and after
 *    the operation.</p>
 * 
 * <p><b>Header Convenience Methods</b></p>
 * <p></p>
 * <ul>
 *    <li><b>deleteList()</b> the current list is deleted and an <em>upLevel()</em>
 *        to the previous level is performed. If the current list is the topmost
 *        list, then the associated <em>SlipReaderCell</em> is deleted and the
 *        reader, <em>SlipReader</em> will be in an invalid state. All further
 *        operations with this reader will have dreadful consequences.</li>
 *    <li><b>dequeue()</b> remove and return the bottom cell in the current
 *        list. If the bottom cell is the current cell,  perform an 
 *        <em>advanceLWL()</em></li>
 *    <li><b>enqueue()</b> place a new <em>SlipDatum</em> or 
 *        <em>SlipSublist</em> cell on the bottom of the current list.</li>
 *    <li><b>flush()</b> delete all cells in the current list and set the
 *        current cell to the current list.</li>
 *    <li><b>getBot()</b> return the bottom cell in the current list.</li>
 *    <li><b>getTop()</b> return the top cell in the current list.</li>
 *    <li><b>moveListLeft()</b> move the current list to the left of the
 *        input cell. This flushes the current list. Set the current cell
 *        to the current list.</li>
 *    <li><b>moveListRight()</b> move the current list to the right of the
 *        input cell. This flushes the current list. Set the current cell
 *        to the current list.</li>
 *    <li><b>pop()</b> remove and return the list top. If the list top
 *        is the current cell then do a <em>advanceLWL()</em></li>
 *    <li><b>push()</b> insert the input cell to the top of the list.</li>
 *    <li><b>replaceBot()</b> replace the current list bottom with the
 *        input value. If the current list bottom is the current cell, then
 *        set the current cell reference to the new cell.</li>
 *    <li><b>replaceTop()</b> replace the current list top with the
 *        input value. If the current list top is the current cell, then
 *        set the current cell reference to the new cell.</li>
 *    <li><b>splitLeft()</b> create and return a new <em>SlipHeader</em>
 *        containing all cells from the current list top to and including
 *        the current cell. Set the current cell to the current list,
 *        <em>reset()</em>.</li>
 *    <li><b>splitRight()</b> create and return a new <em>SlipHeader</em>
 *        containing all cells from the current cell to and including the 
 *        current list bottom. Set the current cell to the current list,
 *        <em>reset()</em>.</li>
 * </ul>
 * 
 * <p><b>Descriptor List Convenience Methods</b></p>
 * <p>All Descriptor List methods are supported. The current list is used
 *    as the default list in the operations.</p>
 * <ul>
 *    <li><b>contains()</b> return <b>true</b> if a value is found.</li>
 *    <li><b>containsKey()</b> return <b>true</b> if a key is found.</li>
 *    <li><b>create_dList()</b> create an empty Description List.</li>
 *    <li><b>deleteAttribute()</b> Delete a &lt;key value &gt; pair.</li>
 *    <li><b>delete_dList()</b> delete a Description List.</li>
 *    <li><b>flush_dList()</b> empty all &lt;key value&gt; pairs from the 
 *        Description List.</li>
 *    <li><b>get()</b> given a key return it's associated attribute.</li>
 *    <li><b>isDList()</b> <b>true</b> if there is a Descriptor List.</li>
 *    <li><b>keys()</b> return an array of keys</li>
 *    <li><b>printDList()</b> return a list of keys.</b></li>
 *    <li><b>dumpDList()</b> dump (uglify) all &lt;key value&gt; pairs in the
 *        Description List.</li>
 *    <li><b>put()</b> add a &lt;key value&gt; to the Description list.</li>
 *    <li><b>size_dList()</b> number of &lt;key value&gt; pairs in the 
 *        Description List.</li>
 * </ul>
 * 
 * <p><b>Cell Convenience Methods</b></p>
 * <p>Cell methods maintain the consistency of the reader before and after
 *    execution. The methods are a short-hand version of SLIP operations 
 *    using the current cell as a default in the operation.</p>
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
 *        the reader one word to the left.</li>
 * </ul>
 * 
 * <p><b>Operators</b></p>
 * <p></p>
 * <ul>
 *    <li><b>operator=()</b> Assignment can be to the reader or to a cell in
 *        the current list.</li>
 *    <ul>
 *        <li>Reader assignment assigns the current cell of the current reader
 *            to the current cell of the input reader.</li>
 *        <li>Sequencer assignment assigns the current cell of the current reader
 *            to the current cell of the input sequencer.</li>
 *        <li>Cell assignment assigns like-to-like. A RHS <em>SlipDatum</em> 
 *            cell can be assigned to a LHS <em>SlipDatum</em> cell and a RHS 
 *            <em>SlipSublist</em> cell can be assigned to a LHS 
 *            <em>SlipSublist</em> cell. Unlike ordinary cell assignment
 *            (see slip::SlipDatum) the assignment of a <em>SlipHeader</em>
 *            reference to a <em>SlipSublist</em> cell can not be done.</li>
 *    </ul>
 *    <li><b>operator==()</b> compares the LHS to the RHS for a 
 *        <em>slip::SlipCell</em>. If a <em>SlipReader</em> is given as the
 *        RHS then the comparison is <b>true</b> if the current cells are equal.
 *        </li>
 *    <li><b>operator!=()</b> compares the LHS to the RHS for a 
 *        <em>slip::SlipCell</em>. If a <em>SlipReader</em> is given as the
 *        RHS then the comparison is <b>true</b> if either the current cell 
 *        or the current list are unequal.</li>
 * </ul>
 * 
 * <p><b>Miscellaneous</b></p>
 * <p></p>
 * <ul>
 *    <li><b>dump()</b> uglify the current cell.</li>
 *    <li><b>dumpList()</b> uglify the current list.</li>
 *    <li><b>getClassType()</b> return the class type of the current cell.</li>
 *    <li><b>getName()</b> return a string representing the <em>SlipDatum</em>
 *        type of the current cell.</li>
 *    <li><b>getMark()</b> return the current list mark.</li>
 *    <li><b>getRefCount()</b> return the current list reference count.</li>
 *    <li><b>isData()</b> <b>true</b> if the current cell is data.</li>
 *    <li><b>isDList()</b> <b>true</b> if the current list has a descriptor
 *        list.</li>
 *    <li><b>isEmpty()</b> <b>true</b> if the current list is empty.</li>
 *    <li><b>isEqual()</b> <b>true</b> if the input list and the current list
 *        are structurally identical.</li>
 *    <li><b>isHeader()</b> <b>true</b> if the current cell is a 
 *        <em>SlipHeader</em></li>
 *    <li><b>isName()</b> <b>true</b> if the current cell is a 
 *        <em>SlipSublist</em>.</li>
 *    <li><b>isNumber()</b> <b>true</b> if the current cell is a 
 *        <em>SlipDatum</em> number.</li>
 *    <li><b>isPtr()</b> <b>true</b> if the current cell is a 
 *        <em>SlipDatum</em> pointer.</li>
 *    <li><b>isSublist()</b> <b>true</b> if the current cell is a 
 *        <em>SlipSublist</em>.</li>
 *    <li><b>putMark()</b> place a new mark into the current list.</li>
 *    <li><b>size()</b> return the number of cells in the current list.</li>
 *    <li><b>toString()</b> pretty-print the current cell.</li>
 *    <li><b>write()</b> pretty-print the current list</li>
 *    <li><b>writeQuick</b> output a list and do not preserve list reuse in
 *        sublists. The generated list can be input with the following proviso's:</li>
 *       <ul>
 *          <li>Sublists can not be reused. On input each sublist becomes a new 
 *              list.</li>
 *          <li>User Data classes are not declared. The user is responsible for
 *              registering User Data classes before input but there is non 
 *              checking to see if the classes have been declared.</li>
 *       </ul>
 *    <li><b>writeToString</b> direct the output of writeQuick to a string.</li>
 * </ul>
 * <p><b>NOTES</b></p>
 * <ol>
 *    <li>Modifications to a list are not synchronized with readers. A
 *        change in a list not using a reader can fatally affect subsequent
 *        reader use. 
 *        <p>Some examples:</p>
 *        <tt><pre>
 *        SlipHeader H = new SlipHeader();   // H = (D(a) D(b) N(L2))
 *        SlipReader R = new SlipReader(H);
 *        R.advanceSNR().advanceSWR();       // R entered L2
 *        delete N;                          // delete the sublist reference
 *        </pre></tt>
 *        <br/>
 *        R is now out of synch. R contains a reference in a list (L2)
 *        which may no longer exist and the reader chain now references
 *        a sublist cell (N) which is no longer part of the list H. Any
 *        operation using R may cause a fatal error, at best, or an
 *        undiscovered reference within a list other than H at worst.
 *        <p>In general, any change to a sublist cell or deletion of a 
 *           list on a reader chain or deletion of the current cell can 
 *           be expected to be a problem. Changes outside the reader, other 
 *           than those noted, can be expected to work correctly.</p>
 *    </li>
 *    <li>Multiple readers of the same list or sublist are supported. The
 *        readers are not synchronized. A change in a list using one 
 *        reader will not be reflected in any other reader and can have a 
 *        fatal impact in execution in the same way as an uncoordinated
 *        change made externally to a reader.</li>
 *    <li>Where a method can be interpreted as applying to either the 
 *        current list, <em>currentList()</em>, or the current cell, 
 *        <em>currentCell()</em>, the current cell will be assumed. 
 *        That is, if R is a reader then R.method() will preferentially
 *        apply to R.currentCell() if method can be interpreted as to be
 *        applicable to either R.currentCell() or R.CurrentList().</li>
 * </ol>
 * 
 */
   
# ifdef SLIPASGN
#   undef SLIPASGN
# endif
# define SLIPASGN(X) {  currentCell() = (X); return *this; }
   
# ifdef SLIPEQUAL
#   undef SLIPEQUAL
# endif
# define SLIPEQUAL(X) {  return ((SlipDatum&)currentCell()) == (X); }
   
# ifdef SLIPNOEQUAL
#   undef SLIPNOEQUAL
# endif
# define SLIPNOEQUAL(X) { return ((SlipDatum&)currentCell()) != (X); }
   
# ifdef SLIPADVANCE
#   undef SLIPADVANCE
# endif
# define SLIPADVANCE(X) { reader = reader->X(); return *this; }  

                                                                                // Cells must be unlinked
# ifdef SLIPINSLEFT
#   undef SLIPINSLEFT
# endif
# define SLIPINSLEFT(X) { currentCell().insLeft((X)); return currentCell(); }
   
                                                                                // Cells must be unlinked
# ifdef SLIPINSRIGHT
#   undef SLIPINSRIGHT
# endif
# define SLIPINSRIGHT(X) { currentCell().insRight((X)); return currentCell(); }
   
# ifdef SLIPREPLACE
#   undef SLIPREPLACE
# endif
# define SLIPREPLACE(X) { reader->replace((X)); return currentCell(); }

# ifdef SLIPENQUEUE
#   undef SLIPENQUEUE
# endif
# define SLIPENQUEUE(X) { currentList().enqueue((X)); return *this; }
 
# ifdef SLIPPUSH
#   undef SLIPPUSH
# endif
# define SLIPPUSH(X) { currentList().push((X)); return *this; }
     
# ifdef SLIPREPLACEY
#   undef SLIPREPLACEY
# endif
# define SLIPREPLACEY(Y, X) { reader->Y((X)); return *this; }

   class SlipSequencer;
   
   class SlipReader {
    /*********************************************************
     *                   Private Functions                   *
    **********************************************************/
    private:
       SlipReaderCell* reader;                                                  //!< link to reader cell object

       SlipReader();                                                           //!< Null constructor

    /*********************************************************
     *                  Protected Functions                  *
    **********************************************************/
    protected:
       SlipReaderCell* getLink(const SlipReader& X)      { return X.reader; }   // get the SlipReaderCell pointer

   /*********************************************************
    *                   Public Functions                    *
   **********************************************************/
    public:
       SlipReader(const SlipHeader& header)   { reader = new SlipReaderCell(header); }
       SlipReader(const SlipSublist& sublist) { reader = new SlipReaderCell(sublist.getHeader()); }
       ~SlipReader()                          { delete reader; reader = NULL;        }

            /*****************************************
             *           Reader Assignment           *
            ******************************************/
       SlipReader&  operator=(const SlipHeader& X)    {  *reader = X; return *this;      }  // Y = V

            /*****************************************
             *        Current Cell Assignment        *
            ******************************************/
       SlipReader&  operator=(const SlipDatum& X)     SLIPASGN(X)                            // Y = V
       SlipReader&  operator=(const SlipReader& X)    SLIPASGN(X.currentCell())              // Y = V

       SlipReader&  operator=(const SlipSequencer& X);                                       // Y = V
       SlipReader&  operator=(const bool    X)        SLIPASGN(X)                            // Y = V
       SlipReader&  operator=(const CHAR    X)        SLIPASGN(X)                            // Y = V
       SlipReader&  operator=(const DOUBLE  X)        SLIPASGN(X)                            // Y = V
       SlipReader&  operator=(const LONG    X)        SLIPASGN(X)                            // Y = V
       SlipReader&  operator=(const UCHAR   X)        SLIPASGN(X)                            // Y = V
       SlipReader&  operator=(const ULONG   X)        SLIPASGN(X)                            // Y = V
       SlipReader&  operator=(const PTR     X)        SLIPASGN(X)                            // Y = V
       SlipReader&  operator=(const string& X)        SLIPASGN(X)                            // Y = V
       SlipReader&  operator=(const string* X)        SLIPASGN(X)                            // Y = V
      
            /*****************************************
             *         Current Cell Equality         *
            ******************************************/
       
       bool         operator==(const SlipSequencer&  X);
       bool         operator==(const SlipReader&  X)   {  return (currentCell() == X.currentCell()); }  // Y == V
       bool         operator==(const SlipCell& X)      {  return currentCell() == X; }               // Y == V
       bool         operator==(const bool    X)        SLIPEQUAL(X)                                  // Y == V
       bool         operator==(const CHAR    X)        SLIPEQUAL(X)                                  // Y == V
       bool         operator==(const DOUBLE  X)        SLIPEQUAL(X)                                  // Y == V
       bool         operator==(const LONG    X)        SLIPEQUAL(X)                                  // Y == V
       bool         operator==(const UCHAR   X)        SLIPEQUAL(X)                                  // Y == V
       bool         operator==(const ULONG   X)        SLIPEQUAL(X)                                  // Y == V
       bool         operator==(const PTR     X)        SLIPEQUAL(X)                                  // Y == V
       bool         operator==(const string& X)        SLIPEQUAL(X)                                  // Y == V
       bool         operator==(const string* X)        SLIPEQUAL(*X)                                 // Y == V
      
            /*****************************************
             *        Current Cell Inequality        *
            ******************************************/

       bool         operator!=(const SlipSequencer&  X);
       bool         operator!=(const SlipReader&  X)   {  return (currentCell() != X.currentCell()); }  // Y != V
       bool         operator!=(const SlipCell& X)      {  return currentCell() != X; }                  // Y != V
       bool         operator!=(const bool    X)        SLIPNOEQUAL(X)                                   // Y != V
       bool         operator!=(const CHAR    X)        SLIPNOEQUAL(X)                                   // Y != V
       bool         operator!=(const DOUBLE  X)        SLIPNOEQUAL(X)                                   // Y != V
       bool         operator!=(const LONG    X)        SLIPNOEQUAL(X)                                   // Y != V
       bool         operator!=(const UCHAR   X)        SLIPNOEQUAL(X)                                   // Y != V
       bool         operator!=(const ULONG   X)        SLIPNOEQUAL(X)                                   // Y != V
       bool         operator!=(const PTR     X)        SLIPNOEQUAL(X)                                   // Y != V
       bool         operator!=(const string& X)        SLIPNOEQUAL(X)                                   // Y != V
       bool         operator!=(const string* X)        SLIPNOEQUAL(*X)                                  // Y != V
       
            /*****************************************
             *            Reader Iteration           *
            ******************************************/
       
       SlipReader&  advanceLEL() SLIPADVANCE(advanceLEL)                        // advance linear element left,  true if element found
       SlipReader&  advanceLER() SLIPADVANCE(advanceLER)                        // advance linear element right, true if element found
       SlipReader&  advanceLNL() SLIPADVANCE(advanceLNL)                        // advance linear name left,     true if element found
       SlipReader&  advanceLNR() SLIPADVANCE(advanceLNR)                        // advance linear name right,    true if element found
       SlipReader&  advanceLWL() SLIPADVANCE(advanceLWL)                        // advance linear word right,    true if element found
       SlipReader&  advanceLWR() SLIPADVANCE(advanceLWR)                        // advance linear word right,    true if element found
       SlipReader&  advanceSEL() SLIPADVANCE(advanceSEL)                        // advance structural element left,  true if element found
       SlipReader&  advanceSER() SLIPADVANCE(advanceSER)                        // advance structural element right, true if element found
       SlipReader&  advanceSNL() SLIPADVANCE(advanceSNL)                        // advance structural name left,     true if element found
       SlipReader&  advanceSNR() SLIPADVANCE(advanceSNR)                        // advance structural name right,    true if element found
       SlipReader&  advanceSWL() SLIPADVANCE(advanceSWL)                        // advance structural word right,    true if element found
       SlipReader&  advanceSWR() SLIPADVANCE(advanceSWR)                        // advance structural word right,    true if element found

            /*****************************************
             *     Miscellaneous Reader Methods      *
            ******************************************/
       SlipCell&    currentCell() const     { return reader->currentCell();}    // Return current SLIP cell being referenced
       USHORT       listDepth()   const     { return reader->listDepth();  }    // Current list depth being traversed
       SlipHeader&  currentList() const     { return reader->currentList();}    // Return current SLIP list header being traversed
       SlipHeader&  reset() { reader = reader->reset(); return reader->currentList();} // Break reader to current list header
       SlipReader&  resetTop() { reader = reader->resetTop(); return *this; }   // Return to start of list descent sublist (return to top)
       SlipReader&  upLevel()  { reader = reader->upLevel();  return *this; }   // Return to previous level

               /*****************************************
                *            Descriptor Lists           *
               ******************************************/
       bool         contains(SlipCell& value) { return currentList().contains(value); }   // returns true if the value is in the list
       bool         containsKey(SlipCell& key){ return currentList().containsKey(key);}   // returns true if the key is in the list
       SlipHeader&  create_dList()     { return currentList().create_dList(); } // create/change a descriptor list for this header
       void         deleteAttribute(SlipCell& key) { currentList().deleteAttribute(key); }// delete attribute and value from list
       SlipHeader&  delete_dList()     { return currentList().delete_dList(); } // delete the descriptor list
       SlipHeader&  flush_dList()      { return currentList().flush_dList();  } // delete contents of a descriptor list
       SlipCell&    get(SlipCell& key) { return currentList().get(key);       } // return value of an attribute
       SlipCell*    keys()             { return currentList().keys();         } // returns an array of keys in the descriptor list
       SlipCell&    put(SlipCell& key, SlipCell& value) { return currentList().put(key, value); }// add attribute to description list
       unsigned     size_dList()       {  return currentList().size_dList();  } // number of <key, value> pairs

            /*****************************************
             *           Manipulating Cells          *
            ******************************************/
       SlipReader&  deleteCell()          { reader->deleteCell(); return *this; } // delete the current cell
       
       SlipCell&    insLeft(SlipReader& X);                                     // Insert X to current cell left
       SlipCell&    insLeft(SlipSequencer& X);                                  // Insert X to current cell left
       SlipCell&    insLeft(SlipCell&  X)   SLIPINSLEFT(X)                      // Insert X to current cell left
       SlipCell&    insLeft(bool       X)   SLIPINSLEFT(X)                      // Insert X to current cell left
       SlipCell&    insLeft(DOUBLE     X)   SLIPINSLEFT(X)                      // Insert X to current cell left
       SlipCell&    insLeft(ULONG      X)   SLIPINSLEFT(X)                      // Insert X to current cell left
       SlipCell&    insLeft(LONG       X)   SLIPINSLEFT(X)                      // Insert X to current cell left
       SlipCell&    insLeft(UCHAR      X)   SLIPINSLEFT(X)                      // Insert X to current cell left
       SlipCell&    insLeft(CHAR       X)   SLIPINSLEFT(X)                      // Insert X to current cell left
       SlipCell&    insLeft(const PTR  X, const void* operation = ptrOP)        // Insert X to current cell leftX, operation
                                            SLIPINSLEFT((X, operation))
       SlipCell&    insLeft(const string& X, bool constFlag = false)            // Insert X to current cell left
                                            SLIPINSLEFT((X, constFlag))
       SlipCell&    insLeft(const string* X, bool constFlag = false)            // Insert X to current cell left
                                            SLIPINSLEFT((*X, constFlag))
                                                                                // List Cells must be unlinked
       SlipCell&    insRight(SlipReader& X)  SLIPINSRIGHT(X.currentCell())      // Insert X to current cell left
       SlipCell&    insRight(SlipSequencer& X);                                 // Insert X to current cell left
       SlipCell&    insRight(SlipCell& X)    SLIPINSRIGHT(X)                    // Insert X to current cell left
       SlipCell&    insRight(bool      X)    SLIPINSRIGHT(X)                    // Insert X to current cell left
       SlipCell&    insRight(DOUBLE    X)    SLIPINSRIGHT(X)                    // Insert X to current cell left
       SlipCell&    insRight(ULONG     X)    SLIPINSRIGHT(X)                    // Insert X to current cell left
       SlipCell&    insRight(LONG      X)    SLIPINSRIGHT(X)                    // Insert X to current cell left
       SlipCell&    insRight(UCHAR     X)    SLIPINSRIGHT(X)                    // Insert X to current cell left
       SlipCell&    insRight(CHAR      X)    SLIPINSRIGHT(X)                    // Insert X to current cell left
       SlipCell&    insRight(const PTR X, const void* operation = ptrOP)        // Insert X to current cell left
                                             SLIPINSRIGHT((X, operation))
       SlipCell&    insRight(const string& X, bool constFlag = false)            // Insert X to current cell left
                                             SLIPINSRIGHT((X, constFlag))
       SlipCell&    insRight(const string* X, bool constFlag = false)            // Insert X to current cell left
                                             SLIPINSRIGHT((*X, constFlag))
       
       SlipCell&    moveLeft(SlipCell& X)                                       // Move the current cell to the left of X
                                          { currentCell().moveLeft(X); return currentCell(); }
       SlipCell&    moveLeft(SlipReader& X);                                    // Move the current cell to the left of X
       SlipCell&    moveLeft(SlipSequencer& X);                                 // Move the current cell to the left of X
       SlipCell&    moveRight(SlipCell& X)                                      // Move the current cell to the left of X
                                          { currentCell().moveRight(X); return currentCell(); }
       SlipCell&    moveRight(SlipReader& X);                                   // Move the current cell to the left of X
       SlipCell&    moveRight(SlipSequencer& X);                                // Move the current cell to the right of X

       SlipCell&    replace(const SlipReader&   X) SLIPREPLACE(X.currentCell()) // Replace a cell on a list with a new cell
       SlipCell&    replace(const SlipSequencer& X);                            // Replace a cell on a list with a new cell
       SlipCell&    replace(const SlipCell& X)  SLIPREPLACE(X)                  // Replace a cell on a list with a new cell
       SlipCell&    replace(bool X)             SLIPREPLACE(X)                  // Replace a cell on a list with a new cell
       SlipCell&    replace(CHAR X)             SLIPREPLACE(X)                  // Replace a cell on a list with a new cell
       SlipCell&    replace(UCHAR X)            SLIPREPLACE(X)                  // Replace a cell on a list with a new cell
       SlipCell&    replace(LONG X)             SLIPREPLACE(X)                  // Replace a cell on a list with a new cell
       SlipCell&    replace(ULONG X)            SLIPREPLACE(X)                  // Replace a cell on a list with a new cell
       SlipCell&    replace(DOUBLE X)           SLIPREPLACE(X)                  // Replace a cell on a list with a new cell
       SlipCell&    replace(const PTR X, const void* operation = ptrOP)         // Replace a cell on a list with a new cell
                                                SLIPREPLACE((X, operation))
       SlipCell&    replace(const string& X, bool constFlag = false)            // Replace a cell on a list with a new cell
                                                SLIPREPLACE((X, constFlag))
       SlipCell&    replace(const string* X, bool constFlag = false)            // Replace a cell on a list with a new cell
                                                SLIPREPLACE((*X, constFlag))

       SlipCell&    unlink();                                                   // Unlink the current cell from a list
       
               /*****************************************
                *          Manipulating Lists           *
               ******************************************/
       
       SlipReader&  deleteList();                                               // delete the current list
       SlipCell&    dequeue();                                                  // Remove and return list bottom

       SlipReader&  enqueue(SlipCell& X) SLIPENQUEUE(X)                         // Insert X on bottom of list
       SlipReader&  enqueue(bool X)      SLIPENQUEUE(X)                         // Insert X on top of list
       SlipReader&  enqueue(UCHAR X)     SLIPENQUEUE(X)                         // Insert X on top of list
       SlipReader&  enqueue(CHAR X)      SLIPENQUEUE(X)                         // Insert X on top of list
       SlipReader&  enqueue(ULONG X)     SLIPENQUEUE(X)                         // Insert X on top of list
       SlipReader&  enqueue(LONG X)      SLIPENQUEUE(X)                         // Insert X on top of list
       SlipReader&  enqueue(DOUBLE X)    SLIPENQUEUE(X)                         // Insert X on top of list
       SlipReader&  enqueue(const PTR X, const void* operation = ptrOP)         // Insert X on top of list
                                         SLIPENQUEUE((X, operation))
       SlipReader&  enqueue(const string& X, bool constFlag = false)            // Insert X on top of list
                                         SLIPENQUEUE((X, constFlag))
       SlipReader&  enqueue(const string* X, bool constFlag = false)            // Insert X on top of list
                                         SLIPENQUEUE((*X, constFlag))
       SlipReader&  flush()                                                     // Flush list of contents
                                         { (SlipHeader&)(reset().flush()); return *this; }
       
       SlipCell&    getBot()             { return currentList().getBot(); }     // Return pointer to list bottom
       SlipCell&    getTop()             { return currentList().getTop(); }     // Return pointer to list top

       SlipReader&  moveListLeft(SlipCell& X);                                  // Move list contents to left of X
       SlipReader&  moveListRight(SlipCell& X);                                 // Move list contents to right of X
       SlipCell&    pop();                                                      // Remove and return list top

       SlipReader&  push(SlipCell& X)    SLIPPUSH(X)                            // Insert X on top of list
       SlipReader&  push(bool X)         SLIPPUSH(X)                            // Insert X on top of list
       SlipReader&  push(UCHAR X)        SLIPPUSH(X)                            // Insert X on top of list
       SlipReader&  push(CHAR X)         SLIPPUSH(X)                            // Insert X on top of list
       SlipReader&  push(ULONG X)        SLIPPUSH(X)                            // Insert X on top of list
       SlipReader&  push(LONG X)         SLIPPUSH(X)                            // Insert X on top of list
       SlipReader&  push(DOUBLE X)       SLIPPUSH(X)                            // Insert X on top of list
       SlipReader&  push(const PTR X, const void* operation = ptrOP)            // Insert V on bottom of list
                                         SLIPPUSH((X, operation))
       SlipReader&  push(const string& X, bool constFlag = false)               // Insert V on bottom of list
                                         SLIPPUSH((X, constFlag))
       SlipReader&  push(const string* X, bool constFlag = false)               // Insert V on bottom of list
                                         SLIPPUSH((*X, constFlag))
               
       SlipReader&  replaceTop(SlipHeader& X) SLIPREPLACEY(replaceTop, X)       // Substitute list TOP value
       SlipReader&  replaceTop(SlipCell& X)   SLIPREPLACEY(replaceTop, X)       // Substitute list TOP value
       SlipReader&  replaceTop(SlipReader& X) SLIPREPLACEY(replaceTop, (X.currentCell()))       // Substitute list TOP value
       SlipReader&  replaceTop(SlipSequencer& X);                               // Substitute list TOP value
       SlipReader&  replaceBot(SlipHeader& X) SLIPREPLACEY(replaceBot, X)       // Substitute list TOP value
       SlipReader&  replaceBot(SlipCell& X)   SLIPREPLACEY(replaceBot, X)       // Substitute list TOP value
       SlipReader&  replaceBot(SlipReader& X) SLIPREPLACEY(replaceBot, (X.currentCell())) // Substitute list TOP value
       SlipReader&  replaceBot(SlipSequencer& X);                                      // Substitute cell BOT value

       SlipReader&  replaceTop(bool X)        SLIPREPLACEY(replaceTop, X)       // Substitute list TOP value
       SlipReader&  replaceTop(UCHAR X)       SLIPREPLACEY(replaceTop, X)       // Substitute list TOP value
       SlipReader&  replaceTop(CHAR X)        SLIPREPLACEY(replaceTop, X)       // Substitute list TOP value
       SlipReader&  replaceTop(ULONG X)       SLIPREPLACEY(replaceTop, X)       // Substitute list TOP value
       SlipReader&  replaceTop(LONG X)        SLIPREPLACEY(replaceTop, X)       // Substitute list TOP value
       SlipReader&  replaceTop(DOUBLE X)      SLIPREPLACEY(replaceTop, X)       // Substitute list TOP value
       SlipReader&  replaceTop(const PTR X, const void* operation = ptrOP)      // Substitute list TOP value
                                              SLIPREPLACEY(replaceTop, (X, operation))
       SlipReader&  replaceTop(const string& X, bool constFlag = false)         // Substitute list TOP value
                                              SLIPREPLACEY(replaceTop, (X, constFlag))
       SlipReader&  replaceTop(const string* X, bool constFlag = false)        // Substitute list TOP value
                                              SLIPREPLACEY(replaceTop, (*X, constFlag))

       SlipReader&  replaceBot(bool X)        SLIPREPLACEY(replaceBot, X)       // Substitute list TOP value
       SlipReader&  replaceBot(UCHAR X)       SLIPREPLACEY(replaceBot, X)       // Substitute list TOP value
       SlipReader&  replaceBot(CHAR X)        SLIPREPLACEY(replaceBot, X)       // Substitute list TOP value
       SlipReader&  replaceBot(ULONG X)       SLIPREPLACEY(replaceBot, X)       // Substitute list TOP value
       SlipReader&  replaceBot(LONG X)        SLIPREPLACEY(replaceBot, X)       // Substitute list TOP value
       SlipReader&  replaceBot(DOUBLE X)      SLIPREPLACEY(replaceBot, X)       // Substitute list TOP value
       SlipReader&  replaceBot(const PTR X, const void* operation = ptrOP)      // Substitute list TOP value
                                              SLIPREPLACEY(replaceBot, (X, operation))
       SlipReader&  replaceBot(const string& X, bool constFlag = false)         // Substitute list TOP value
                                              SLIPREPLACEY(replaceBot, (X, constFlag))
       SlipReader&  replaceBot(const string* X, bool constFlag = false)        // Substitute list TOP value
                                              SLIPREPLACEY(replaceBot, (*X, constFlag))

       SlipHeader&  splitLeft();                                                // Split cells to left of X & X to a new list
       SlipHeader&  splitRight();                                               // Split cells to right of X & X to a new list

            /*****************************************
             *             Miscellaneous             *
            ******************************************/
       ClassType    getClassType() const { return currentCell().getClassType();}// get class type
       SlipCell*    getLeftLink()  const { return (SlipCell*)currentCell().getLeftLink(); } // Return pointer to previous cell
       string       getName()     const  { return currentCell().getName();   }  // return ascii name of cell
       USHORT       getMark()     const  { return currentList().getMark();   }  // value of list mark
       USHORT       getRefCount() const  { return currentList().getRefCount(); }// get the list reference count
       SlipCell*    getRightLink() const { return (SlipCell*)currentCell().getRightLink();} // Return pointer to next cell
       bool         isData()      const  { return currentCell().isData();    }  // true if cell is data
       bool         isDiscrete()  const  { return currentCell().isDiscrete();}  // true if cell contains a discrete number
       bool         isDList()     const  { return currentList().isDList();   }  // true if there is a descriptor list
       bool         isEmpty()     const  { return currentList().isEmpty();   }  // true if the current list is empty
       bool         isEqual(const SlipHeader& X) const                          // true the the lists are structurally and data identical
                                         { return currentList().isEqual(X);  }
       bool         isHeader()    const  { return currentCell().isHeader();  }  // true if cell is a header
       bool         isName()      const  { return currentCell().isName();    }  // true if cell is sublist
       bool         isNumber()    const  { return currentCell().isNumber();  }  // true if cell is numeric data
       bool         isPtr()       const  { return currentCell().isPtr();     }  // true if pointer
       bool         isReal()      const  { return currentCell().isReal();    }  // true if cell contains a real number
       bool         isString()    const  { return currentCell().isString();  }  // true if cell contains a string
       bool         isSublist()   const  { return currentCell().isSublist(); }  // true if cell is sublist
       USHORT       putMark(const USHORT X){ return currentList().putMark(X);}  // Mark the list
       unsigned     size()        const  { return currentList().size();      }  // Number of cells in list
       bool         write()              { return currentList().write();     }  // Output the current list
       bool         write(ostream& out)  { return currentList().write(out);  }  // Output the current list
       bool         write(string& filename) { return currentList().write(filename); }// Output the current list
       void         writeQuick()         { return currentList().writeQuick();}  // Output the current list - quickly
       void         writeQuick(ostream& out){ return currentList().writeQuick(out); }// Output the current list - quickly
       void         writeQuick(string& filename)                                // Output the current list - quickly
                                         { return currentList().writeQuick(filename); }
       string       writeToString()      { return currentList().writeToString(); } // Return an ASCII formated list as a string
       string       toString();                                                 // pretty print the reader
       string       dump()              { return (this->reader)->dump();     }  // uglify the reader
       void         dumpList()          { currentList().dumpList();          }
   }; // class SlipReader
}; // namespace slip

# undef SLIPASGN
# undef SLIPEQUAL
# undef SLIPNOEQUAL
# undef SLIPADVANCE
# undef SLIPINSLEFT
# undef SLIPINSRIGHT
# undef SLIPREPLACE
# undef SLIPENQUEUE
# undef SLIPPUSH
# undef SLIPREPLACEY

#endif