
/* GNU fdisk, (gnufdisk-device) a library to manage a device
 * Copyright (C) 2011 Free Software Foundation
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA. */

#include <stdlib.h>

#include <gnufdisk-debug.h>
#include <gnufdisk-exception.h>
#include <gnufdisk-device.h>

#define GEOMETRY (getenv("GNUFDISK_GEOMETRY") != NULL)

struct gnufdisk_geometry {
  struct gnufdisk_device* device;
  gnufdisk_integer start;
  gnufdisk_integer end;
};

static void delete_string(void* _p)
{
  GNUFDISK_LOG((GEOMETRY, "delete struct gnufdisk_string* %p", _p));
  gnufdisk_string_delete(_p);
}

static void
check_geometry (struct gnufdisk_geometry **_g)
{
  GNUFDISK_RETRY rp0;
  union gnufdisk_device_exception_data data;

  GNUFDISK_RETRY_SET (rp0);

  if (gnufdisk_check_memory (*_g, sizeof (struct gnufdisk_geometry), 0) != 0)
    {
      data.egeometrypointer = _g;

      GNUFDISK_THROW (GNUFDISK_EXCEPTION_ALL, &rp0,
		      GNUFDISK_DEVICE_EGEOMETRYPOINTER, &data,
		      "invalid struct gnufdisk_geometry* %p", *_g);
    }
  else if (gnufdisk_check_memory ((*_g)->device, 1, 1) != 0)
    {
      data.edevicepointer = &(*_g)->device;

      GNUFDISK_THROW (GNUFDISK_EXCEPTION_ALL, &rp0, 
                      GNUFDISK_DEVICE_EDEVICEPOINTER, &data,
                      "invalid struct gnufdisk_device* %p", (*_g)->device);
    }
}

static void
free_pointer (void *_p)
{
  GNUFDISK_LOG ((GEOMETRY, "free ponter %p", _p));
  free(_p);
}

struct gnufdisk_geometry *
gnufdisk_geometry_new (struct gnufdisk_device *_dev)
{
  struct gnufdisk_geometry *g;
  GNUFDISK_RETRY rp0;

  GNUFDISK_RETRY_SET (rp0);

  if (gnufdisk_check_memory(_dev, 1, 1) != 0)
    {
      union gnufdisk_device_exception_data data;

      data.edevicepointer = &_dev;

      GNUFDISK_THROW (GNUFDISK_EXCEPTION_ALL, &rp0,
		      GNUFDISK_DEVICE_EDEVICEPOINTER, &data,
		      "invalid struct gnufdisk_device* %p", _dev);
    }

  if((g = malloc(sizeof (struct gnufdisk_geometry))) == NULL)
    GNUFDISK_THROW(0, NULL, GNUFDISK_DEVICE_ENOMEM, NULL, "cannot allocate memory");

  if(gnufdisk_exception_register_unwind_handler (&free_pointer, g) != 0)
    GNUFDISK_WARNING("gnufdisk_exception_register_unwind_handler failed. Missing GNUFDISK_TRY?");

  memset(g, 0, sizeof(struct gnufdisk_geometry));

  gnufdisk_device_ref (_dev);
  g->device = _dev;

  if(gnufdisk_exception_unregister_unwind_handler(&free_pointer, g) != 0)
    GNUFDISK_WARNING("gnufdisk_exception_unregister_unwind_handler failed. Missing GNUFDISK_TRY?");

  return g;
}

struct gnufdisk_geometry* 
gnufdisk_geometry_duplicate(struct gnufdisk_geometry* _g)
{
  struct gnufdisk_geometry* ret;

  check_geometry(&_g);

  ret = gnufdisk_geometry_new(_g->device);

  ret->start = _g->start;
  ret->end = _g->end;

  return ret;
}

static gnufdisk_integer
align_sector (struct gnufdisk_device *_dev, gnufdisk_integer _s)
{
  /* FIXME align according to device constraints */
  return _s;
}

struct gnufdisk_geometry *
gnufdisk_geometry_set (struct gnufdisk_geometry *_g,
		       gnufdisk_integer _s, gnufdisk_integer _l)
{
  GNUFDISK_RETRY rp0;

  check_geometry (&_g);

  GNUFDISK_RETRY_SET (rp0);

  if (_l <= 0) /* FIXME: check device size overlap ? */
    {
      union gnufdisk_device_exception_data data;

      data.egeometrylength = &_l;

      GNUFDISK_THROW (GNUFDISK_EXCEPTION_ALL, &rp0,
		      GNUFDISK_DEVICE_EGEOMETRYLENGTH, &data,
		      "invalid length: %lld", _l);
    }

  _g->start = align_sector (_g->device, _s);
  _g->end = align_sector (_g->device, _s + _l - 1);

  return _g;
}

struct gnufdisk_geometry *
gnufdisk_geometry_set_chs (struct gnufdisk_geometry *_g,
			   struct gnufdisk_chs _s, 
			   struct gnufdisk_chs _l)
{
  GNUFDISK_RETRY rp0;
  gnufdisk_integer spt;
  gnufdisk_integer hpc;
  struct gnufdisk_string* s;
  gnufdisk_integer start;
  gnufdisk_integer length;

  check_geometry (&_g);

  GNUFDISK_RETRY_SET (rp0);
  
  s = gnufdisk_string_new(NULL);

  if(gnufdisk_exception_register_unwind_handler(&delete_string, s) != 0)
    GNUFDISK_WARNING("gnufdisk_exception_register_unwind_handler failed. Missing GNUFDISK_TRY?");

  gnufdisk_string_set(s, "SECTORS-PER-TRACK");
  gnufdisk_device_get_parameter (_g->device, s, &spt, sizeof (spt));

  gnufdisk_string_set(s, "HEADS-PER-CYLINDER");
  gnufdisk_device_get_parameter (_g->device, s, &hpc, sizeof (hpc));
 
  start = ((_s.cylinder * hpc) + _s.head) * spt + _s.sector - 1;
  length = ((_l.cylinder * hpc) + _l.head) * spt + _l.sector - 1;

  if (length <= 0) /* FIXME: check device size overlap ? */
    {
      union gnufdisk_device_exception_data data;

      data.egeometrylength = &length;

      GNUFDISK_THROW (GNUFDISK_EXCEPTION_ALL, &rp0,
		      GNUFDISK_DEVICE_EGEOMETRYLENGTH, &data,
		      "invalid length: %lld", _l);
    }

  _g->start = align_sector (_g->device, start);
  _g->end = align_sector (_g->device, start + length - 1);

  if(gnufdisk_exception_unregister_unwind_handler(&delete_string, s) != 0)
    GNUFDISK_WARNING("gnufdisk_exception_unregister_unwind_handler failed. Missing GNUFDISK_TRY?");

  gnufdisk_string_delete(s);

  return _g;
}

gnufdisk_integer
gnufdisk_geometry_start (struct gnufdisk_geometry *_g)
{
  check_geometry (&_g);

  return _g->start;
}

struct gnufdisk_chs
gnufdisk_geometry_start_chs (struct gnufdisk_geometry *_g)
{
  struct gnufdisk_chs ret;
  gnufdisk_integer spt;
  gnufdisk_integer hpc;
  struct gnufdisk_string* s;

  check_geometry (&_g);
  
  s = gnufdisk_string_new(NULL);

  if(gnufdisk_exception_register_unwind_handler(&delete_string, s) != 0)
    GNUFDISK_WARNING("gnufdisk_exception_register_unwind_handler failed. Missing GNUFDISK_TRY?");

  gnufdisk_string_set(s, "SECTORS-PER-TRACK");
  gnufdisk_device_get_parameter (_g->device, s, &spt, sizeof (spt));

  gnufdisk_string_set(s, "HEADS-PER-CYLINDER");
  gnufdisk_device_get_parameter (_g->device, s, &hpc, sizeof (hpc));

  ret.cylinder = _g->start / (spt * hpc);
  ret.head = (_g->start / spt) % hpc;
  ret.sector = (_g->start % spt) + 1;

  if(gnufdisk_exception_unregister_unwind_handler(&delete_string, s) != 0)
    GNUFDISK_WARNING("gnufdisk_exception_unregister_unwind_handler failed. Missing GNUFDISK_TRY?");

  gnufdisk_string_delete(s);

  return ret;
}

gnufdisk_integer
gnufdisk_geometry_end (struct gnufdisk_geometry *_g)
{
  check_geometry (&_g);

  return _g->end;
}

struct gnufdisk_chs
gnufdisk_geometry_end_chs (struct gnufdisk_geometry *_g)
{
  struct gnufdisk_chs ret;
  gnufdisk_integer spt;
  gnufdisk_integer hpc;
  struct gnufdisk_string* s;

  check_geometry (&_g);

  s = gnufdisk_string_new(NULL);
  if(gnufdisk_exception_register_unwind_handler(&delete_string, s) != 0)
    GNUFDISK_WARNING("gnufdisk_exception_register_unwind_handler failed. Missing GNUFDISK_TRY?");

  gnufdisk_string_set(s, "SECTORS-PER-TRACK");
  gnufdisk_device_get_parameter (_g->device, s, &spt, sizeof (spt));

  gnufdisk_string_set(s, "HEADS-PER-CYLINDER");
  gnufdisk_device_get_parameter (_g->device, s, &hpc, sizeof (hpc));

  ret.cylinder = _g->end / (spt * hpc);
  ret.head = (_g->end / spt) % hpc;
  ret.sector = (_g->end % spt) + 1;

  if(gnufdisk_exception_unregister_unwind_handler(&delete_string, s) != 0)
    GNUFDISK_WARNING("gnufdisk_exception_unregister_unwind_handler failed. Missing GNUFDISK_TRY?");

  gnufdisk_string_delete(s);

  return ret;
}

gnufdisk_integer
gnufdisk_geometry_length (struct gnufdisk_geometry *_g)
{
  check_geometry (&_g);

  return (_g->end - _g->start) + 1;
}

struct gnufdisk_chs
gnufdisk_geometry_length_chs (struct gnufdisk_geometry *_g)
{
  struct gnufdisk_chs ret;
  gnufdisk_integer length;
  gnufdisk_integer spt;
  gnufdisk_integer hpc;
  struct gnufdisk_string* s;

  check_geometry (&_g);

  s = gnufdisk_string_new(NULL);
  
  if(gnufdisk_exception_register_unwind_handler(&delete_string, s) != 0)
    GNUFDISK_WARNING("gnufdisk_exception_register_unwind_handler failed. Missing GNUFDISK_TRY?");

  length = (_g->end - _g->start) + 1;

  gnufdisk_string_set(s, "SECTORS-PER-TRACK");
  gnufdisk_device_get_parameter (_g->device, s, &spt, sizeof (spt));

  gnufdisk_string_set(s, "HEADS-PER-CYLINDER");
  gnufdisk_device_get_parameter (_g->device, s, &hpc, sizeof (hpc));

  ret.cylinder = length / (spt * hpc);
  ret.head = (length / spt) % hpc;
  ret.sector = (length % spt) + 1;

  if(gnufdisk_exception_unregister_unwind_handler(&delete_string, s) != 0)
    GNUFDISK_WARNING("gnufdisk_exception_unregister_unwind_handler failed. Missing GNUFDISK_TRY?");

  gnufdisk_string_delete(s);
  return ret;
}

void
gnufdisk_geometry_delete (struct gnufdisk_geometry *_g)
{
  check_geometry (&_g);

  if (_g->device)
    gnufdisk_device_delete (_g->device);

  free (_g);
}

