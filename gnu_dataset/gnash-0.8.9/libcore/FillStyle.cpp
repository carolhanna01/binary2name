// FillStyle.cpp:  Graphical region filling styles, for Gnash.
// 
//   Copyright (C) 2007, 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
// 
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
//

#include "FillStyle.h"

#include <ostream> 
#include <boost/variant.hpp>

#include "smart_ptr.h"
#include "CachedBitmap.h"
#include "movie_definition.h"
#include "SWF.h"
#include "GnashNumeric.h"
#include "RunResources.h"
#include "GnashImage.h"

namespace gnash {

namespace {

/// Create a lerped version of two other FillStyles.
//
/// The two fill styles must have exactly the same types. Callers are 
/// responsible for ensuring this.
class SetLerp : public boost::static_visitor<>
{
public:
    SetLerp(const FillStyle::Fill& a, const FillStyle::Fill& b, double ratio)
        :
        _a(a),
        _b(b),
        _ratio(ratio)
    {
    }

    template<typename T> void operator()(T& f) const {
        const T& a = boost::get<T>(_a);
        const T& b = boost::get<T>(_b);
        f.setLerp(a, b, _ratio);
    }

private:
    const FillStyle::Fill& _a;
    const FillStyle::Fill& _b;
    const double _ratio;

};

}

SWFMatrix
gradientMatrix(GradientFill::Type t, const SWFMatrix& m)
{
    SWFMatrix base;
    switch (t) {
        case GradientFill::LINEAR:
            base.set_translation(128, 0);
            base.set_scale(1.0 / 128, 1.0 / 128);
            break;
        case GradientFill::RADIAL:
            base.set_scale(1.0 / 512, 1.0 / 512);
            break;
    }
    base.concatenate(m);
    return base;
}

GradientFill::GradientFill(Type t, const SWFMatrix& m,
        const GradientRecords& recs)
    :
    spreadMode(PAD),
    interpolation(RGB),
    _focalPoint(0.0),
    _gradients(recs),
    _type(t),
    _matrix(gradientMatrix(t, m))
{
    assert(recs.empty() || recs.size() > 1);
}
    
void
GradientFill::setFocalPoint(double d)
{
    _focalPoint = clamp<float>(d, -1, 1); 
}

BitmapFill::BitmapFill(Type t, const CachedBitmap* bi, const SWFMatrix& m,
        SmoothingPolicy pol)
    :
    _type(t),
    _smoothingPolicy(pol),
    _matrix(m),
    _bitmapInfo(bi),
    _md(0),
    _id(0)
{
}
    
BitmapFill::BitmapFill(SWF::FillType t, movie_definition* md,
        boost::uint16_t id, const SWFMatrix& m)
    :
    _type(),
    _smoothingPolicy(),
    _matrix(m),
    _bitmapInfo(0),
    _md(md),
    _id(id)
{
    assert(md);

    _smoothingPolicy = md->get_version() >= 8 ? 
        BitmapFill::SMOOTHING_ON : BitmapFill::SMOOTHING_UNSPECIFIED;

    switch (t) {
        case SWF::FILL_TILED_BITMAP_HARD:
            _type = BitmapFill::TILED;
            _smoothingPolicy = BitmapFill::SMOOTHING_OFF;
            break;

        case SWF::FILL_TILED_BITMAP:
            _type = BitmapFill::TILED;
            break;

        case SWF::FILL_CLIPPED_BITMAP_HARD:
            _type = BitmapFill::CLIPPED;
            _smoothingPolicy = BitmapFill::SMOOTHING_OFF;
            break;

        case SWF::FILL_CLIPPED_BITMAP:
            _type = BitmapFill::CLIPPED;
            break;

        default:
            std::abort();
    }
}

BitmapFill::BitmapFill(const BitmapFill& other)
    :
    _type(other._type),
    _smoothingPolicy(other._smoothingPolicy),
    _matrix(other._matrix),
    _bitmapInfo(other._bitmapInfo),
    _md(other._md),
    _id(other._id)
{
}

BitmapFill::~BitmapFill()
{
}
    
BitmapFill&
BitmapFill::operator=(const BitmapFill& other)
{
    _type = other._type;
    _smoothingPolicy = other._smoothingPolicy;
    _matrix = other._matrix;
    _bitmapInfo = other._bitmapInfo;
    _md = other._md;
    _id = other._id;
    return *this;
}

const CachedBitmap*
BitmapFill::bitmap() const
{
    if (_bitmapInfo) {
        return  _bitmapInfo->disposed() ? 0 : _bitmapInfo.get();
    }
    if (!_md) return 0;
    _bitmapInfo = _md->getBitmap(_id);

    // May still be 0!
    return _bitmapInfo.get();
}
    
void
GradientFill::setLerp(const GradientFill& a, const GradientFill& b,
        double ratio)
{
    assert(type() == a.type());
    assert(_gradients.size() == a.recordCount());
    assert(_gradients.size() == b.recordCount());

    for (size_t i = 0, e = _gradients.size(); i < e; ++i) {
        const GradientRecord& ra = a.record(i);
        const GradientRecord& rb = b.record(i);
        _gradients[i].ratio = frnd(lerp<float>(ra.ratio, rb.ratio, ratio));
        _gradients[i].color.set_lerp(ra.color, rb.color, ratio);
    }
    _matrix.set_lerp(a.matrix(), b.matrix(), ratio);
}
    
void
BitmapFill::setLerp(const BitmapFill& a, const BitmapFill& b, double ratio)
{
    _matrix.set_lerp(a.matrix(), b.matrix(), ratio);
}

// Sets this style to a blend of a and b.  t = [0,1]
void
setLerp(FillStyle& f, const FillStyle& a, const FillStyle& b, double t)
{
    assert(t >= 0 && t <= 1);
    f.fill = a.fill;
    boost::apply_visitor(SetLerp(a.fill, b.fill, t), f.fill);
}


std::ostream&
operator<<(std::ostream& os, const BitmapFill::SmoothingPolicy& p)
{
    switch (p) {
        case BitmapFill::SMOOTHING_UNSPECIFIED:
            os << "unspecified";
            break;
        case BitmapFill::SMOOTHING_ON:
            os << "on";
            break;
        case BitmapFill::SMOOTHING_OFF:
            os << "off";
            break;
        default:
            // cast to int required to avoid infinite recursion
            os << "unknown " << +p;
            break;
    }
    return os;
}

} // namespace gnash


// Local Variables:
// mode: C++
// End:
