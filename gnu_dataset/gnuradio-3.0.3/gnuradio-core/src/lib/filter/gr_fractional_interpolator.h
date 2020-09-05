/* -*- c++ -*- */
/*
 * Copyright 2004 Free Software Foundation, Inc.
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

#ifndef INCLUDED_GR_FRACTIONAL_INTERPOLATOR_H
#define	INCLUDED_GR_FRACTIONAL_INTERPOLATOR_H

#include <gr_block.h>

class gri_mmse_fir_interpolator;

class gr_fractional_interpolator;
typedef boost::shared_ptr<gr_fractional_interpolator> gr_fractional_interpolator_sptr;

// public constructor
gr_fractional_interpolator_sptr gr_make_fractional_interpolator (float phase_shift, float interp_ratio);

/*!
 * \brief Interpolating mmse filter with float input, float output
 * \ingroup filter
 */
class gr_fractional_interpolator : public gr_block
{
 public:
  ~gr_fractional_interpolator ();
  void forecast(int noutput_items, gr_vector_int &ninput_items_required);
  int general_work (int noutput_items,
		    gr_vector_int &ninput_items,
		    gr_vector_const_void_star &input_items,
		    gr_vector_void_star &output_items);

protected:
  gr_fractional_interpolator (float phase_shift, float interp_ratio);

 private:
  float 			d_mu;
  float 			d_mu_inc;
  gri_mmse_fir_interpolator 	*d_interp;

  friend gr_fractional_interpolator_sptr
    gr_make_fractional_interpolator (float phase_shift, float interp_ratio);
};

#endif
