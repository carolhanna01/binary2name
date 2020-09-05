/* -*- c++ -*- */
/*
 * Copyright 2005 Free Software Foundation, Inc.
 * 
 * This file is part of GNU Radio
 * 
 * GNU Radio is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * GNU Radio is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with GNU Radio; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street,
 * Boston, MA 02110-1301, USA.
 */

#ifndef INCLUDED_GR_SKIPHEAD_H
#define INCLUDED_GR_SKIPHEAD_H

#include <gr_sync_block.h>
#include <stddef.h>      // size_t

/*!
 * \brief skips the first N items, from then on copies items to the output
 * \ingroup block
 *
 * Useful for building test cases and sources which have metadata or junk at the start
 */

class gr_skiphead : public gr_sync_block
{
  friend gr_block_sptr gr_make_skiphead (size_t sizeof_stream_item, int nitems);
  gr_skiphead (size_t sizeof_stream_item, int nitems);

  int  d_nitems;
  int  d_nskipped_items;

 public:
  int work (int noutput_items,
     gr_vector_const_void_star &input_items,
     gr_vector_void_star &output_items);
};

gr_block_sptr
gr_make_skiphead (size_t sizeof_stream_item, int nitems);


#endif /* INCLUDED_GR_SKIPHEAD_H */
