/* GNU Fidsk (gnufdisk-devicemanager), a library to manage devices.
 *
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
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <assert.h>

#include <gnufdisk-common.h>
#include <gnufdisk-debug.h>
#include <gnufdisk-exception.h>
#include <gnufdisk-device.h>

#include <gnufdisk-userinterface.h>
#include <gnufdisk-devicemanager.h>

#define DEVICEMANAGER (getenv("GNUFDISK_DEVICEMANAGER") != NULL)

struct gnufdisk_devicemanager
{
  struct gnufdisk_userinterface *userinterface;
  int nref;
};

static int new_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

struct gnufdisk_devicemanager* gnufdisk_devicemanager_new(struct gnufdisk_userinterface* _ui)
{
  struct gnufdisk_devicemanager* ret;

  ret = NULL;

  GNUFDISK_TRY(&new_throw_handler, NULL)
    {
      if(gnufdisk_check_memory(_ui, 1, 1) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_userinterface* %p", _ui);

      if((ret = malloc(sizeof(struct gnufdisk_devicemanager))) == NULL)
        GNUFDISK_THROW(0, NULL, ENOMEM, NULL, "cannot allocate memory");

      if(gnufdisk_exception_register_unwind_handler(&free, ret) != 0)
        GNUFDISK_WARNING("gnufdisk_exception_register_unwind_handler failed.");

      memset(ret, 0, sizeof(struct gnufdisk_devicemanager));

      gnufdisk_userinterface_ref(_ui);
      ret->userinterface = _ui;
      ret->nref = 1;

      if(gnufdisk_exception_unregister_unwind_handler(&free, ret) != 0)
        GNUFDISK_WARNING("gnufdisk_exception_unregister_unwind_handler failed.");
    } 
  GNUFDISK_CATCH_DEFAULT
    {
      ret = NULL;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int ref_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void *_edata)
{
  return -1;
}

int gnufdisk_devicemanager_ref (struct gnufdisk_devicemanager *_dm)
{
  int ret;

  ret = 0;

  GNUFDISK_TRY(&ref_throw_handler, NULL)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      _dm->nref++;

      ret = 0;
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int delete_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void *_edata)
{
  return -1;
}

int gnufdisk_devicemanager_delete (struct gnufdisk_devicemanager *_dm)
{
  int ret;

  ret = 0;

  GNUFDISK_TRY(&delete_throw_handler, NULL)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      if(_dm->nref < 2)
	{
	  if(_dm->userinterface)
	    {
	      gnufdisk_userinterface_delete(_dm->userinterface);
	      _dm->userinterface = NULL;
	    }
	  
	  free(_dm);
	}
      else
	_dm->nref--;

      ret = 0;
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int geometry_new_throw_handler(void * _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

struct gnufdisk_geometry* 
gnufdisk_devicemanager_geometry_new(struct gnufdisk_devicemanager* _dm,
                                    struct gnufdisk_device* _dev)
{
  struct gnufdisk_geometry* ret;

  ret = NULL;

  GNUFDISK_TRY(&geometry_new_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_geometrymanager* %p", _dm);

      ret = gnufdisk_geometry_new(_dev);
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = NULL;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int geometry_duplicate_throw_handler(void * _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

struct gnufdisk_geometry* 
gnufdisk_devicemanager_geometry_duplicate(struct gnufdisk_devicemanager* _dm,
                                          struct gnufdisk_geometry* _geom)
{
  struct gnufdisk_geometry* ret;

  ret = NULL;

  GNUFDISK_TRY(&geometry_duplicate_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_geometrymanager* %p", _dm);

      ret = gnufdisk_geometry_duplicate(_geom);
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = NULL;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int geometry_delete_throw_handler(void * _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

int 
gnufdisk_devicemanager_geometry_delete(struct gnufdisk_devicemanager* _dm,
                                       struct gnufdisk_geometry* _geom)
{
  int ret;

  ret = 0;

  GNUFDISK_TRY(&geometry_delete_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_geometrymanager* %p", _dm);

      gnufdisk_geometry_delete(_geom);

      ret = 0;
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int geometry_set_throw_handler(void * _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

struct gnufdisk_geometry*
gnufdisk_devicemanager_geometry_set(struct gnufdisk_devicemanager* _dm,
                                    struct gnufdisk_geometry* _geom,
                                    gnufdisk_integer _start,
                                    gnufdisk_integer _length)
{
  struct gnufdisk_geometry* ret;

  ret = NULL;

  GNUFDISK_TRY(&geometry_set_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_geometrymanager* %p", _dm);

      ret = gnufdisk_geometry_set(_geom, _start, _length);
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = NULL;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int geometry_set_chs_throw_handler(void * _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

struct gnufdisk_geometry*
gnufdisk_devicemanager_geometry_set_chs(struct gnufdisk_devicemanager* _dm,
                                        struct gnufdisk_geometry* _geom,
                                        struct gnufdisk_chs _start,
                                        struct gnufdisk_chs _length)
{
  struct gnufdisk_geometry* ret;

  ret = NULL;

  GNUFDISK_TRY(&geometry_set_chs_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_geometrymanager* %p", _dm);

      ret = gnufdisk_geometry_set_chs(_geom, _start, _length);
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = NULL;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int geometry_start_throw_handler(void * _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

gnufdisk_integer 
gnufdisk_devicemanager_geometry_start(struct gnufdisk_devicemanager* _dm,
                                      struct gnufdisk_geometry* _geom)
{
  gnufdisk_integer ret;

  ret = 0;

  GNUFDISK_TRY(&geometry_start_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_geometrymanager* %p", _dm);

      ret = gnufdisk_geometry_start(_geom);
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int geometry_start_chs_throw_handler(void * _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

struct gnufdisk_chs 
gnufdisk_devicemanager_geometry_start_chs(struct gnufdisk_devicemanager* _dm,
                                      struct gnufdisk_geometry* _geom)
{
  struct gnufdisk_chs ret;

  memset(&ret, 0, sizeof(struct gnufdisk_chs));

  GNUFDISK_TRY(&geometry_start_chs_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_geometrymanager* %p", _dm);

      ret = gnufdisk_geometry_start_chs(_geom);
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret.cylinder = -1;
      ret.head = -1;
      ret.sector = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int geometry_end_throw_handler(void * _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

gnufdisk_integer 
gnufdisk_devicemanager_geometry_end(struct gnufdisk_devicemanager* _dm,
                                      struct gnufdisk_geometry* _geom)
{
  gnufdisk_integer ret;

  ret = 0;

  GNUFDISK_TRY(&geometry_end_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_geometrymanager* %p", _dm);

      ret = gnufdisk_geometry_end(_geom);
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int geometry_end_chs_throw_handler(void * _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

struct gnufdisk_chs 
gnufdisk_devicemanager_geometry_end_chs(struct gnufdisk_devicemanager* _dm,
                                      struct gnufdisk_geometry* _geom)
{
  struct gnufdisk_chs ret;

  memset(&ret, 0, sizeof(struct gnufdisk_chs));

  GNUFDISK_TRY(&geometry_end_chs_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_geometrymanager* %p", _dm);

      ret = gnufdisk_geometry_end_chs(_geom);
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret.cylinder = -1;
      ret.head = -1;
      ret.sector = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int geometry_length_throw_handler(void * _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

gnufdisk_integer 
gnufdisk_devicemanager_geometry_length(struct gnufdisk_devicemanager* _dm,
                                      struct gnufdisk_geometry* _geom)
{
  gnufdisk_integer ret;

  ret = 0;

  GNUFDISK_TRY(&geometry_length_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_geometrymanager* %p", _dm);

      ret = gnufdisk_geometry_length(_geom);
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int geometry_length_chs_throw_handler(void * _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

struct gnufdisk_chs 
gnufdisk_devicemanager_geometry_length_chs(struct gnufdisk_devicemanager* _dm,
                                      struct gnufdisk_geometry* _geom)
{
  struct gnufdisk_chs ret;

  memset(&ret, 0, sizeof(struct gnufdisk_chs));

  GNUFDISK_TRY(&geometry_length_chs_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_geometrymanager* %p", _dm);

      ret = gnufdisk_geometry_length_chs(_geom);
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret.cylinder = -1;
      ret.head = -1;
      ret.sector = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int device_new_throw_handler(void * _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

struct gnufdisk_device* 
gnufdisk_devicemanager_device_new(struct gnufdisk_devicemanager* _dm,
                                  struct gnufdisk_string* _module,
                                  struct gnufdisk_string* _module_options)
{
  struct gnufdisk_device* ret;

  ret = NULL;

  GNUFDISK_TRY(&device_new_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      ret = gnufdisk_device_new(_module, _module_options);
    }
  GNUFDISK_CATCH_DEFAULT
    {
      GNUFDISK_LOG((DEVICEMANAGER, 
                    "caught an exception from %s:%d: %s", 
                    exception_info.file,
                    exception_info.line,
                    exception_info.message));
      ret = NULL;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int device_open_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void *_edata)
{
  return -1;
}

int gnufdisk_devicemanager_device_open(struct gnufdisk_devicemanager* _dm, 
                                       struct gnufdisk_device* _dev,
                                       struct gnufdisk_string* _path)
{
  int ret;

  ret = 0;

  GNUFDISK_TRY(&device_open_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      gnufdisk_device_open(_dev, _path);

      ret = 0;
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int device_disklabel_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

struct gnufdisk_disklabel* 
gnufdisk_devicemanager_device_disklabel(struct gnufdisk_devicemanager* _dm,
                                        struct gnufdisk_device* _dev)
{
  struct gnufdisk_disklabel* ret;

  ret = NULL;

  GNUFDISK_TRY(&device_disklabel_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      ret = gnufdisk_device_disklabel(_dev);
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = NULL;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int device_create_disklabel_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

struct gnufdisk_disklabel* 
gnufdisk_devicemanager_device_create_disklabel(struct gnufdisk_devicemanager* _dm,
                                               struct gnufdisk_device* _dev,
                                               struct gnufdisk_string* _system)
{
  struct gnufdisk_disklabel* ret;

  ret = NULL;

  GNUFDISK_TRY(&device_create_disklabel_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      ret = gnufdisk_device_create_disklabel(_dev, _system);
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = NULL;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int device_set_parameter_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

int gnufdisk_devicemanager_device_set_parameter(struct gnufdisk_devicemanager* _dm,
                                                struct gnufdisk_device* _dev,
                                                struct gnufdisk_string* _param,
                                                const void* _data,
                                                size_t _size)
{
  int ret;

  ret = 0;

  GNUFDISK_TRY(&device_set_parameter_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      gnufdisk_device_set_parameter(_dev, _param, _data, _size);

      ret = 0;
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int device_get_parameter_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

int gnufdisk_devicemanager_device_get_parameter(struct gnufdisk_devicemanager* _dm,
                                                struct gnufdisk_device* _dev,
                                                struct gnufdisk_string* _param,
                                                void* _dest,
                                                size_t _size)
{
  int ret;

  ret = 0;

  GNUFDISK_TRY(&device_get_parameter_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      gnufdisk_device_get_parameter(_dev, _param, _dest, _size);

      ret = 0;
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int device_commit_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

int gnufdisk_devicemanager_device_commit(struct gnufdisk_devicemanager* _dm,
                                         struct gnufdisk_device* _dev)
{
  int ret;

  ret = 0;

  GNUFDISK_TRY(&device_commit_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      gnufdisk_device_commit(_dev);

      ret = 0;
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int device_close_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

int gnufdisk_devicemanager_device_close(struct gnufdisk_devicemanager* _dm, 
                                        struct gnufdisk_device* _dev)
{
  int ret;

  ret = 0;

  GNUFDISK_TRY(&device_close_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      gnufdisk_device_close(_dev);

      ret = 0;
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int device_delete_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

int gnufdisk_devicemanager_device_delete(struct gnufdisk_devicemanager* _dm, 
                                         struct gnufdisk_device* _dev)
{
  int ret;

  ret = 0;

  GNUFDISK_TRY(&device_delete_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      gnufdisk_device_delete(_dev);

      ret = 0;
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int disklabel_raw_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

int gnufdisk_devicemanager_disklabel_raw(struct gnufdisk_devicemanager* _dm,
                                         struct gnufdisk_disklabel* _disk,
                                         void** _dest,
                                         size_t* _size)
{
  int ret;

  ret = 0;

  GNUFDISK_TRY(&disklabel_raw_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      gnufdisk_disklabel_raw(_disk, _dest, _size);

      ret = 0;
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int disklabel_system_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

struct gnufdisk_string* 
gnufdisk_devicemanager_disklabel_system(struct gnufdisk_devicemanager* _dm,
                                        struct gnufdisk_disklabel* _disk)
{
  struct gnufdisk_string* ret;

  GNUFDISK_TRY(&disklabel_system_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      ret = gnufdisk_disklabel_system(_disk);
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = NULL;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int disklabel_partition_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

struct gnufdisk_partition* 
gnufdisk_devicemanager_disklabel_partition(struct gnufdisk_devicemanager* _dm,
                                           struct gnufdisk_disklabel* _disk,
                                           size_t _number)
{
  struct gnufdisk_partition* ret;

  ret = NULL;

  GNUFDISK_TRY(&disklabel_partition_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      ret = gnufdisk_disklabel_partition(_disk, _number);
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = NULL;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int disklabel_create_partition_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

struct gnufdisk_partition* 
gnufdisk_devicemanager_disklabel_create_partition(struct gnufdisk_devicemanager* _dm,
                                                  struct gnufdisk_disklabel* _disk,
                                                  struct gnufdisk_geometry* _start,
                                                  struct gnufdisk_geometry* _end,
                                                  struct gnufdisk_string* _type)
{
  struct gnufdisk_partition* ret;

  ret = NULL;

  GNUFDISK_TRY(&disklabel_create_partition_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      ret = gnufdisk_disklabel_create_partition(_disk, _start, _end, _type);
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = NULL;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int disklabel_remove_partition_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void *_edata)
{
  return -1;
}

int gnufdisk_devicemanager_disklabel_remove_partition(struct gnufdisk_devicemanager* _dm,
                                                      struct gnufdisk_disklabel* _disk,
                                                      size_t _number)
{
  int ret;

  ret = 0;

  GNUFDISK_TRY(&disklabel_remove_partition_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      gnufdisk_disklabel_remove_partition(_disk, _number);

      ret = 0;
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int disklabel_set_parameter_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

int gnufdisk_devicemanager_disklabel_set_parameter(struct gnufdisk_devicemanager* _dm,
                                                   struct gnufdisk_disklabel* _disk,
                                                   struct gnufdisk_string* _param,
                                                   const void* _data,
                                                   size_t _size)
{
  int ret;

  ret = 0;

  GNUFDISK_TRY(&disklabel_set_parameter_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      gnufdisk_disklabel_set_parameter(_disk, _param, _data, _size);

      ret = 0;
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int disklabel_get_parameter_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

int gnufdisk_devicemanager_disklabel_get_parameter(struct gnufdisk_devicemanager* _dm,
                                                   struct gnufdisk_disklabel* _disk,
                                                   struct gnufdisk_string* _param,
                                                   void* _dest,
                                                   size_t _size)
{
  int ret;

  ret = 0;

  GNUFDISK_TRY(&disklabel_get_parameter_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      gnufdisk_disklabel_get_parameter(_disk, _param, _dest, _size);

      ret = 0;
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int disklabel_delete_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

int gnufdisk_devicemanager_disklabel_delete(struct gnufdisk_devicemanager* _dm,
					    struct gnufdisk_disklabel* _disk)
{
  int ret;

  ret = 0;

  GNUFDISK_TRY(&disklabel_delete_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      gnufdisk_disklabel_delete(_disk);

      ret = 0;
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int partition_set_parameter_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

int gnufdisk_devicemanager_partition_set_parameter(struct gnufdisk_devicemanager* _dm,
                                                   struct gnufdisk_partition* _part,
                                                   struct gnufdisk_string* _param,
                                                   const void* _data,
                                                   size_t _size)
{
  int ret;

  ret = 0;

  GNUFDISK_TRY(&partition_set_parameter_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      gnufdisk_partition_set_parameter(_part, _param, _data, _size);

      ret = 0;
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}


static int partition_get_parameter_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

int gnufdisk_devicemanager_partition_get_parameter(struct gnufdisk_devicemanager* _dm,
                                                   struct gnufdisk_partition* _part,
                                                   struct gnufdisk_string* _param,
                                                   void* _dest,
                                                   size_t _size)
{
  int ret;

  ret = 0;

  GNUFDISK_TRY(&partition_get_parameter_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      gnufdisk_partition_get_parameter(_part, _param, _dest, _size);

      ret = 0;
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int partition_type_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

struct gnufdisk_string*
gnufdisk_devicemanager_partition_type(struct gnufdisk_devicemanager* _dm,
                                      struct gnufdisk_partition* _part)
{
  struct gnufdisk_string* ret;
  
  ret = NULL;

  GNUFDISK_TRY(&partition_type_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      ret = gnufdisk_partition_type(_part);
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = NULL;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int partition_start_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

gnufdisk_integer
gnufdisk_devicemanager_partition_start(struct gnufdisk_devicemanager* _dm,
                                          struct gnufdisk_partition* _part)
{
  gnufdisk_integer ret;
  
  ret = 0;

  GNUFDISK_TRY(&partition_start_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      ret = gnufdisk_partition_start(_part);
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int partition_length_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

gnufdisk_integer
gnufdisk_devicemanager_partition_length(struct gnufdisk_devicemanager* _dm,
                                          struct gnufdisk_partition* _part)
{
  gnufdisk_integer ret;
  
  ret = 0;

  GNUFDISK_TRY(&partition_length_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      ret = gnufdisk_partition_length(_part);
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int partition_number_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

int gnufdisk_devicemanager_partition_number(struct gnufdisk_devicemanager* _dm,
                                            struct gnufdisk_partition* _part)
{
  int ret;

  ret = 0;

  GNUFDISK_TRY(&partition_number_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      ret = gnufdisk_partition_number(_part);
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}


static int partition_move_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

int gnufdisk_devicemanager_partition_move(struct gnufdisk_devicemanager* _dm,
                                          struct gnufdisk_partition* _part,
                                          struct gnufdisk_geometry* _range)
{
  int ret;

  ret = 0;

  GNUFDISK_TRY(&partition_move_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      gnufdisk_partition_move(_part, _range);

      ret = 0;
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int partition_resize_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

int gnufdisk_devicemanager_partition_resize(struct gnufdisk_devicemanager* _dm,
                                            struct gnufdisk_partition* _part,
                                            struct gnufdisk_geometry* _range)
{
  int ret;

  ret = 0;

  GNUFDISK_TRY(&partition_resize_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      gnufdisk_partition_resize(_part, _range);

      ret = 0;
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int partition_read_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

int gnufdisk_devicemanager_partition_read(struct gnufdisk_devicemanager* _dm,
                                          struct gnufdisk_partition* _part,
                                          gnufdisk_integer _start,
                                          void* _buf,
                                          size_t _size)
{
  int ret;

  ret = 0;

  GNUFDISK_TRY(&partition_read_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      ret = gnufdisk_partition_read(_part, _start, _buf, _size);
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int partition_write_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

int gnufdisk_devicemanager_partition_write(struct gnufdisk_devicemanager* _dm,
                                          struct gnufdisk_partition* _part,
                                          gnufdisk_integer _start,
                                          const void* _buf,
                                          size_t _size)
{
  int ret;

  ret = 0;

  GNUFDISK_TRY(&partition_write_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      ret = gnufdisk_partition_write(_part, _start, _buf, _size);
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

static int partition_delete_throw_handler(void* _data, struct gnufdisk_exception_info* _info, void* _edata)
{
  return -1;
}

int gnufdisk_devicemanager_partition_delete(struct gnufdisk_devicemanager* _dm,
                                            struct gnufdisk_partition* _part)
{
  int ret;

  ret = 0;

  GNUFDISK_TRY(&partition_delete_throw_handler, _dm)
    {
      if(gnufdisk_check_memory(_dm, sizeof(struct gnufdisk_devicemanager), 0) != 0)
        GNUFDISK_THROW(0, NULL, EFAULT, NULL, "invalid struct gnufdisk_devicemanager* %p", _dm);

      gnufdisk_partition_delete(_part);

      ret = 0;
    }
  GNUFDISK_CATCH_DEFAULT
    {
      ret = -1;
    }
  GNUFDISK_EXCEPTION_END;

  return ret;
}

