/*
  GNU Gama -- adjustment of geodetic networks
  Copyright (C) 2000  Ales Cepek <cepek@fsv.cvut.cz>
  2011  Vaclav Petras <wenzeslaus@gmail.com>
  2013, 2014, 2018, 2019  Ales Cepek <cepek@gnu.org>

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

/** \file observation.h
 * \brief local observation classes header file
 *
 * \author Ales Cepek
 * \author Vaclav Petras (acyclic visitor pattern)
 */

#ifndef gama_local_Bod_Mer_Mereni_H
#define gama_local_Bod_Mer_Mereni_H

#include <gnu_gama/local/float.h>
#include <gnu_gama/local/pointid.h>
#include <gnu_gama/local/exception.h>
#include <gnu_gama/local/language.h>
#include <gnu_gama/visitor.h>
#include <gnu_gama/simplified.h>

#include <iostream>
#include <vector>
#include <utility>

#include <gnu_gama/obsdata.h>
#include <gnu_gama/local/matvec.h>

namespace GNU_gama { namespace local {

  class PointData;
  class Observation;


  using ObservationList = std::list<Observation*>;


  /** \brief Local observation base class
   *
   * \note VisitableObservation class can be used for automatic implementation
   *       of accept method.
   * \note Every derived class should be added to class AllObservationsVisitor.
   *
   * \sa VisitableObservation, AllObservationsVisitor
   */
  class Observation
    {
    public:

      using CovarianceMatrix = GNU_gama::local::CovMat;

      Observation(const PointID& s, const PointID& c, double m)
        :
        cluster(nullptr), from_(s), to_(c), value_(m), active_(true),
        from_dh_(0), to_dh_(0)
        {
          if (s == c) throw GNU_gama::local::Exception(T_GaMa_from_equals_to);
        }
      virtual ~Observation();
      Observation(const Observation&) = default;
      Observation& operator=(const Observation&) = default;
      Observation(Observation&&) = default;
      Observation& operator=(Observation&&) = default;


      /** \brief Cloning method
       *
       * Every derived class has to implement this method.
       */
      virtual Observation* clone() const = 0;

      /** \brief This is accept method from acyclic visitor pattern.
       *
       * Every derived class has to implement this method.
       *
       * \sa VisitableObservation
       */
      virtual void accept(BaseVisitor* visitor) = 0;

      const GNU_gama::Cluster<Observation>* ptr_cluster() const
        {
          return cluster;
        }
      GNU_gama::Cluster<Observation>* ptr_cluster()
        {
          return cluster;
        }
      void set_cluster(GNU_gama::Cluster<Observation>* c)
        {
          cluster = c;
        }

      const PointID& from() const { return from_;    }
      const PointID& to()   const { return to_;      }
      double value()        const { return value_;   }
      double stdDev()       const ;

      /** \brief Checks whether observation is active.
       *
       * Passive (non-active) observations are usually not taken into account
       * during computations.
       *
       * \sa set_active(), set_passive()
       */
      bool active() const { return active_; }

      /** \brief Sets observation state to active. */
      void set_active() { active_ = true; }

      /** \brief Sets observation state to passive (non-active). */
      void set_passive() { active_ = false; }

      bool check_std_dev() const;

      static  bool gons;   // added in 1.7.09

      // instrument / reflector height

      double  from_dh() const { return from_dh_; }
      double  to_dh  () const { return to_dh_;   }

      void    set_value  (double v) { value_   = v; }
      void    set_from_dh(double h) { from_dh_ = h; }
      void    set_to_dh  (double h) { to_dh_   = h; }

      int     dimension() const { return 1; }

      virtual bool angular() const { return false; }

      /** \brief XML attribute "extern" is not processed by gama-local. */
      std::string get_extern() const { return extern_; }
      void set_extern(std::string s) { extern_ = GNU_gama::simplified(std::move(s)); }

    protected:

      /** \brief Constructs only partially initialized object.
       *
       * It is necessary to call init().
       */
      Observation()
          : cluster(nullptr), from_(""), to_(""), value_(0), active_(true),
            from_dh_(0), to_dh_(0)
      {}

      /** \brief Finishes initialization after Observation(). */
      void init(const PointID& from, const PointID& to, double value)
      {
          from_ = from;
          to_ = to;
          value_ = value;
      }

      void norm_rad_val()
      {
          while (value_ >= 2*M_PI) value_ -= 2*M_PI;
          while (value_ <   0    ) value_ += 2*M_PI;
      }

      GNU_gama::Cluster<Observation>* cluster {nullptr};
      int      cluster_index {0};
      friend   class GNU_gama::Cluster<Observation>;

    private:

      PointID from_;
      PointID to_;
      double        value_{0};        // observed value
      mutable bool  active_{false};   // set false for unused observation
      double        from_dh_{0};      // height of instrument
      double        to_dh_{0};        // height of reflector
      std::string   extern_{};
    };


  class Distance : public Accept<Distance, Observation>
    {
    public:
      Distance(const PointID& s, const PointID& c, double d)
        {
          if (d <= 0)
            throw GNU_gama::local::Exception(T_POBS_zero_or_negative_distance);
          init(s, c, d);
        }
      ~Distance() final;
      Distance(const Distance&) = default;
      Distance& operator=(const Distance&) = default;
      Distance(Distance&&) = default;
      Distance& operator=(Distance&&) = default;

      Distance* clone() const final { return new Distance(*this); }
    };


  class Direction : public Accept<Direction, Observation>
    {
    public:
      Direction(const PointID& s, const PointID& c,  double d)
        {
          init(s, c, d);
          norm_rad_val();
        }
      ~Direction() final;
      Direction(const Direction&) = default;
      Direction& operator=(const Direction&) = default;
      Direction(Direction&&) = default;
      Direction& operator=(Direction&&) = default;

      Direction* clone() const final { return new Direction(*this); }

      bool angular() const final { return true; }

      double orientation() const;
      void   set_orientation(double p);
      bool   test_orientation() const;
      void   delete_orientation();
      void   index_orientation(int n);
      int    index_orientation() const;
    };


  class Angle : public Accept<Angle, Observation>
    {
    private:
      PointID fs_;
      double  fs_dh_;
    public:
      Angle(const PointID& s, const PointID& b,  const PointID& f, double d)
          : fs_(f), fs_dh_(0)
        {
          /* was: if (s == c2 || c == c2) ...; from 1.3.31 we allow
           * left and right targets to be identical, surely this is
           * not an realistic case but it's useful in g2d_point.h:67
           * and should not cause a trouble elsewhere */
          if (s == f)
            throw GNU_gama::local::Exception(T_GaMa_from_equals_to);
          init(s, b, d);
          norm_rad_val();
        }
      ~Angle() final;
      Angle(const Angle&) = default;
      Angle& operator=(const Angle&) = default;
      Angle(Angle&&) = default;
      Angle& operator=(Angle&&) = default;

      Angle* clone() const final { return new Angle(*this); }

      bool angular() const final { return true; }

      const PointID& bs() const { return to(); }     // backsight station
      const PointID& fs() const { return fs_;  }     // foresight station

      double bs_dh() const       { return to_dh(); }
      void   set_bs_dh(double h) { set_to_dh(h);   }
      double fs_dh() const       { return fs_dh_;  }
      void   set_fs_dh(double h) { fs_dh_ = h;     }
    };


  // height differences

  class H_Diff : public Accept<H_Diff, Observation>
    {
    private:
      double dist_;
    public:
      H_Diff(const PointID& s, const PointID& c, double dh, double d=0)
        : dist_(d)
        {
          if (d < 0)   // zero distance is legal in H_Diff
            throw GNU_gama::local::Exception(T_POBS_zero_or_negative_distance);
          init(s, c, dh);
        }
      ~H_Diff() final;
      H_Diff(const H_Diff&) = default;
      H_Diff& operator=(const H_Diff&) = default;
      H_Diff(H_Diff&&) = default;
      H_Diff& operator=(H_Diff&&) = default;

      H_Diff* clone() const final { return new H_Diff(*this); }

      void   set_dist(double d)
        {
          if (d < 0)    // zero distance is legal in H_Diff
            throw GNU_gama::local::Exception(T_POBS_zero_or_negative_distance);
          dist_ = d;
        }
      double dist() const { return dist_; }
    };


  // coordinate differences (vectors)  dx, dy, dz

  class Xdiff : public Accept<Xdiff, Observation>
    {
    public:
      Xdiff(const PointID& from, const PointID& to, double dx)
      {
          init(from, to, dx);
      }
      ~Xdiff() final;
      Xdiff(const Xdiff&) = default;
      Xdiff& operator=(const Xdiff&) = default;
      Xdiff(Xdiff&&) = default;
      Xdiff& operator=(Xdiff&&) = default;

      Xdiff* clone() const final { return new Xdiff(*this); }
    };

  class Ydiff : public Accept<Ydiff, Observation>
    {
    public:
      Ydiff(const PointID& from, const PointID& to, double dy)
      {
          init(from, to, dy);
      }
      ~Ydiff() final;
      Ydiff(const Ydiff&) = default;
      Ydiff& operator=(const Ydiff&) = default;
      Ydiff(Ydiff&&) = default;
      Ydiff& operator=(Ydiff&&) = default;

      Ydiff* clone() const final { return new Ydiff(*this); }
    };

  class Zdiff : public Accept<Zdiff, Observation>
    {
    public:
      Zdiff(const PointID& from, const PointID& to, double dz)
      {
          init(from, to, dz);
      }
      ~Zdiff() final;
      Zdiff(const Zdiff&) = default;
      Zdiff& operator=(const Zdiff&) = default;
      Zdiff(Zdiff&&) = default;
      Zdiff& operator=(Zdiff&&) = default;

      Zdiff* clone() const final { return new Zdiff(*this); }
    };


  // coordinate observations (observed coordinates)  x, y, z

  class X : public Accept<X, Observation>
    {
    public:
      X(const PointID& point, double x)
      {
          init(point, "", x);
      }

      ~X() final;
      X(const X&) = default;
      X& operator=(const X&) = default;
      X(X&&) = default;
      X& operator=(X&&) = default;

      X* clone() const final { return new X(*this); }
    };

  class Y : public Accept<Y, Observation>
    {
    public:
      Y(const PointID& point, double y)
      {
          init(point, "", y);
      }

      ~Y() final;
      Y(const Y&) = default;
      Y& operator=(const Y&) = default;
      Y(Y&&) = default;
      Y& operator=(Y&&) = default;

      Y* clone() const final{ return new Y(*this); }
    };

  class Z : public Accept<Z, Observation>
    {
    public:
      Z(const PointID& point, double z)
      {
          init(point, "", z);
      }

      ~Z() final;
      Z(const Z&) = default;
      Z& operator=(const Z&) = default;
      Z(Z&&) = default;
      Z& operator=(Z&&) = default;

      Z* clone() const final { return new Z(*this); }
    };


  // slope observations (slope distances and zenith angles)

  class S_Distance : public Accept<S_Distance, Observation>
    {
    public:
      S_Distance(const PointID& s, const PointID& c, double d)
        {
          if (d <= 0)
            throw GNU_gama::local::Exception(T_POBS_zero_or_negative_distance);
          init(s, c, d);
        }

      ~S_Distance() final;
      S_Distance(const S_Distance&) = default;
      S_Distance& operator=(const S_Distance&) = default;
      S_Distance(S_Distance&&) = default;
      S_Distance& operator=(S_Distance&&) = default;

      S_Distance* clone() const final { return new S_Distance(*this); }
    };


  class Z_Angle : public Accept<Z_Angle, Observation>
    {
    public:
      Z_Angle(const PointID& s, const PointID& c, double d)
        {
          if (d <= 0)
            throw GNU_gama::local::Exception(T_POBS_zero_or_negative_distance);
          init(s, c, d);
        }

      ~Z_Angle() final;
      Z_Angle(const Z_Angle&) = default;
      Z_Angle& operator=(const Z_Angle&) = default;
      Z_Angle(Z_Angle&&) = default;
      Z_Angle& operator=(Z_Angle&&) = default;

      Z_Angle* clone() const final { return new Z_Angle(*this); }

      bool angular() const final { return true; }
    };


  class Azimuth : public Accept<Azimuth, Observation>
    {
    public:
      Azimuth(const PointID& s, const PointID& c,  double d)
        {
          init(s, c, d);
          norm_rad_val();
        }
      ~Azimuth() final;
      Azimuth(const Azimuth&) = default;
      Azimuth& operator=(const Azimuth&) = default;
      Azimuth(Azimuth&&) = default;
      Azimuth& operator=(Azimuth&&) = default;

      Azimuth* clone() const final { return new Azimuth(*this); }

      bool angular() const final { return true; }
    };


  /** \brief Base class for visitors which visit all observations
   *
   * AllObservationsVisitor is a interface for visitors which visit
   * all observations.  It ensures implementing all visit methods.
   *
   * Acyclic visitor pattern does not require the ability to visit all
   * classes from hierarchy.  However, most observation visitors visit
   * all observations.
   *
   * AllObservationsVisitor should be used as a base class when
   * visitor is expected to deal with all kinds of local observations
   * (classes derived from Observation).  When a new local observation
   * type is created, it should be added to AllObservationsVisitor to
   * ensure that compiler will warn about visitors that want to visit
   * all observations but they don't.
   *
   * Example of using AllObservationsVisitor:
   * \code
   * class SomeVisitor : public GNU_gama::local::AllObservationsVisitor
   * {
   *     void visit(Direction* d)
   *     {
   *         // ...
   *     }
   *     // ...
   * };
   * \endcode
   *
   * List of functions to be implemented:
   * \code
   * void visit(Distance* obs)
   * void visit(Direction* obs)
   * void visit(Angle* obs)
   * void visit(H_Diff* obs)
   * void visit(S_Distance* obs)
   * void visit(Z_Angle* obs)
   * void visit(X* obs)
   * void visit(Y* obs)
   * void visit(Z* obs)
   * void visit(Xdiff* obs)
   * void visit(Ydiff* obs)
   * void visit(Zdiff* obs)
   * void visit(Azimuth* obs)
   * \endcode
   *
   * \sa BaseVisitor, Visitor
   */
  class AllObservationsVisitor :
          public BaseVisitor,
          public Visitor<Direction>,
          public Visitor<Distance>,
          public Visitor<Angle>,
          public Visitor<H_Diff>,
          public Visitor<S_Distance>,
          public Visitor<Z_Angle>,
          public Visitor<X>,
          public Visitor<Y>,
          public Visitor<Z>,
          public Visitor<Xdiff>,
          public Visitor<Ydiff>,
          public Visitor<Zdiff>,
          public Visitor<Azimuth>
  {
  };


  /** \brief Helper class for printing observational data.
   */

  class LocalNetwork;

  class DisplayObservationVisitor final : public AllObservationsVisitor
  {
  public:

    DisplayObservationVisitor(LocalNetwork* ln);

    std::string xml_name;
    std::string str_val;
    std::string str_stdev;
    std::string str_from;
    std::string str_to;
    std::string str_bs;
    std::string str_fs;

    void visit(Distance* obs) final;
    void visit(Direction* obs) final;
    void visit(Angle* obs) final;
    void visit(H_Diff* obs) final;
    void visit(S_Distance* obs) final;
    void visit(Z_Angle* obs) final;
    void visit(X* obs) final;
    void visit(Y* obs) final;
    void visit(Z* obs) final;
    void visit(Xdiff* obs) final;
    void visit(Ydiff* obs) final;
    void visit(Zdiff* obs) final;
    void visit(Azimuth* obs) final;

  private:

    LocalNetwork* lnet;
    const double  scale;
    void clear();
  };


}}   // namespace GNU_gama::local

#endif
