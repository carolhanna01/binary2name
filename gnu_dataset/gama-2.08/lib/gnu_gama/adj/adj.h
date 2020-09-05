/*
  GNU Gama -- adjustment of geodetic networks
  Copyright (C) 2002, 2018  Ales Cepek <cepek@gnu.org>

  This file is part of the GNU Gama C++ library.

  This library is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 3 of the License, or
  (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with GNU Gama.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <matvec/covmat.h>
#include <gnu_gama/adj/adj_input_data.h>

#include <iostream>

#ifndef gama_local_Adj_adjustment_class_h
#define gama_local_Adj_adjustment_class_h


namespace GNU_gama {


  class AdjInputData;

  /** \brief General adjustment class for GNU Gama project.
    */

  class Adj
  {
  public:

    /** Adjustment algorithms implemented in Adj class */
    enum algorithm
      {
        /** Sparse matrix solution of Cholesky decomposition minimizing
            local bandwidth.
         */
        envelope,
        gso,       /*!< Gram-Schmidt ortogonalization of design matrix */
        svd,       /*!< Singular Value decomposition of project matrix */
        cholesky   /*!< Cholesky decomposition of normal equations     */
      };

    Adj ()
    {
      init(nullptr);
    }
    virtual ~Adj();

    Adj(const Adj&) = delete;
    Adj& operator=(const Adj&) = delete;
    Adj(const Adj&&) = delete;
    Adj& operator=(const Adj&&) = delete;

    int n_obs() const { return n_obs_; }   /*!< number of observations */
    int n_par() const { return n_par_; }   /*!< number of parameters   */

    /**  sets pointer to input data object */
    void set(const AdjInputData* inp) { init(inp); }

    /** numerical algorithm                 */
    void set_algorithm(Adj::algorithm);
    /** returns current numerical algorithm */
    Adj::algorithm get_algorithm() const { return algorithm_; }

    int    defect() const { return least_squares->defect(); }
    double rtr   () const { return rtr_; }     /*!< weighted sum of squares */
    const Vec<>& x();                          /*!< adjusted parameters     */
    const Vec<>& r();                          /*!< adjusted residuals      */

    /** weight coeficients of adjusted parameters    */
    double q_xx(int i, int j) { return least_squares->q_xx(i,j); }
    /** weight coefficients of adjusted observations */
    double q_bb(int i, int j);

    /* ######################################################################
     * Functions choldec() and forwardSubstitution were identical in Adj and
     * LocalNetwork classes. They were declared static in Adj class and
     * commented out in LocalNetwork in version 2.08.
     * ################################################################### */
    static void choldec (CovMat<>& chol);
    static void forwardSubstitution(const CovMat<>& chol, Vec<>& v);

  private:

    using Exc = GNU_gama::Exception::matvec;
    using AdjBase = GNU_gama::AdjBase<double, int, Exc>;
    using AdjBaseFull = GNU_gama::AdjBaseFull<double, int, Exc>;
    using AdjBaseSparse = GNU_gama::AdjBaseSparse<double, int, Exc, AdjInputData>;

    const AdjInputData* data {nullptr};
    AdjBase* least_squares {nullptr};

    bool      solved {false};
    algorithm algorithm_ {envelope};
    int       n_obs_{0}, n_par_{0};
    double    rtr_ {0};
    int       minx_dim {0};
    int*      minx {nullptr};
    Mat <>    A_dot;
    Vec <>    b_dot;
    Vec <>    x_;
    Vec <>    r_;

    void init(const AdjInputData*);
    void init_least_squares();
  };

}  // namespace GNU_gama

#endif
