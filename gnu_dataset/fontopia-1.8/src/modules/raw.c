/* 
 *    Copyright 2015, 2016, 2017, 2018 (c) Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 * 
 *    file: raw.c
 *    This file is part of fontopia.
 *
 *    fontopia is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    (at your option) any later version.
 *
 *    fontopia is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with fontopia.  If not, see <http://www.gnu.org/licenses/>.
 */    

#include "raw.h"
#include "../defs.h"
#include "../view.h"

struct font_s *raw_create_empty_font()
{
	struct font_s *font = (struct font_s *)NULL;
	font = (struct font_s *)malloc(sizeof(struct font_s));
	if(!font) goto memory_error;
	memset((void *)font, 0, sizeof(struct font_s));
	
	font->length = 256;
	font->has_unicode_table = 0;
	font->height   = 8;
	font->width    = 8;
	font->charsize = font->height;
	font->version = get_version("RAW");
	font->file_hdr = (void *)NULL;
	font->header_size = 0;
	font->utf_version = 0;
	font->data_size = font->height*font->length;
	font->data = (unsigned char *)malloc(font->data_size);
	if(!font->data) goto memory_error;
	memset((void *)font->data, 0, font->data_size);
	font->state = NEW;
	calc_max_zoom(font);

	font->module = get_module_by_name("raw");
	/* something REALLY WRONG happended here */
	if(!font->module) goto undefined_error;

	reset_all_cursors();
	return font;

undefined_error:
	status_error("Error creating new font");
	goto end;
memory_error:
	status_error("Insufficient memory");
end:
	kill_font(font);
	return (struct font_s *)NULL;
}


struct font_s *raw_load_font_file(char *file_name)
{
	FILE *font_file = (FILE *)NULL;
	char *file_data = (char *)NULL;
	struct font_s *font = (struct font_s *)NULL;
	if(!file_name) return (struct font_s *)NULL;
	if(!(font_file = fopen(file_name, "rb")))
	{
		status_error("Error opening file");
		return (struct font_s *)NULL;
	}
	
	long i;
	i = fseek(font_file, 0, SEEK_END);
	long file_size = ftell(font_file);
	if(!file_size)
	{
		status_error("Error: empty file!");
		fclose(font_file);
		return (struct font_s *)NULL;
	}
	if(file_size != 2048 && file_size != 4096)
	{
		status_error("Error: Invalid file size");
		fclose(font_file);
		return (struct font_s *)NULL;
	}
	
	rewind(font_file);
	file_data = (char *)malloc(file_size);
	if(!file_data) goto memory_error;
	i = fread(file_data, 1, file_size, font_file);
	if(i != file_size) goto file_read_error;
	fclose(font_file);
	font = raw_load_font((unsigned char *)file_data, file_size);
	free(file_data);
	return font;
	
file_read_error:
	status_error("Error reading font file");
	goto end;
memory_error:
	status_error("Not enough memory to load font file");
end:
	if(file_data) free(file_data);
	fclose(font_file);
	font_file = (FILE *)NULL;
	return (struct font_s *)NULL;
}


struct font_s *raw_load_font(unsigned char *file_data, long file_size)
{
	struct font_s *font = (struct font_s *)NULL;
	font = (struct font_s *)malloc(sizeof(struct font_s));
	if(!font) goto memory_error;
	memset((void *)font, 0, sizeof(struct font_s));
	
	font->length = 256;
	font->has_unicode_table = 0;
	font->height   = (file_size == 2048) ? 8 : 16;
	font->width    = 8;
	font->charsize = font->height;
	font->version = get_version("RAW"); //VER_RAW;
	font->file_hdr = (void *)NULL;
	font->header_size = 0;
	font->utf_version = 0;
	//font->raw_data = file_data;
	//font->raw_data_size = file_size;
	font->data = (unsigned char *)malloc(file_size);
	if(!font->data) goto memory_error;
	memcpy((void *)font->data, (void *)file_data, file_size);
	font->data_size = file_size;
	font->state = OPENED;
	calc_max_zoom(font);

	font->module = get_module_by_name("raw");
	/* something REALLY WRONG happended here */
	if(!font->module) goto undefined_error;

	reset_all_cursors();
	return font;

undefined_error:
	status_error("Error loading font file");
	goto end;
memory_error:
	status_error("Insufficient memory");
	goto end;
//file_read_error:
	//status_error("Error reading file");
end:
	kill_font(font);
	return (struct font_s *)NULL;
}


int raw_write_to_file(FILE *file, struct font_s *font)
{
  if(!file || !font) return 1;
  int res;
  res = fwrite(font->data, 1, font->data_size, file);
  if(res != font->data_size) return 1;
  return 0;
}


void raw_handle_hw_change(struct font_s *font, char *newdata, long new_datasize)
{
  long old_datasize = font->data_size;
  /* shrinking data - easy one, no new memory allocation */
  if(new_datasize < old_datasize)
  {
    memcpy((void *)font->data, (void *)newdata, new_datasize);
    font->data_size = new_datasize;
  }
  /* expanding data - we need memory reallocation */
  else
  {
    void *new_rawdata = (void *)malloc(new_datasize);
    /* FIXME: Handle this error more decently */
    if(!new_rawdata) return;
    memcpy((void *)new_rawdata, (void *)newdata, new_datasize);
    free(font->data);
    font->data = new_rawdata;
    font->data_size = new_datasize;
  }
}

/*
void raw_export_unitab(struct font_s *font, FILE *f)
{
  status_error("Raw font has no unicode table");
  return;
}
*/

int raw_create_unitab(struct font_s *font)
{
  status_error("Raw font has no unicode table");
  return 0;
}

void raw_kill_unitab(struct font_s *font)
{
  free_unicode_table(font);
  font->unicode_info_size = 0;
  if(font->unicode_info) free(font->unicode_info);
  font->unicode_info = 0;
  font->has_unicode_table = 0;
}


void raw_handle_unicode_table_change(struct font_s *font, char old_has_unicode_table)
{
  /***********************/
  /* remove unicode info */
  /***********************/
  if(!font->has_unicode_table)
  {
    raw_kill_unitab(font);
  }
  else
  {
    status_error("Raw fonts have no Unicode tables");
  }
}


void raw_handle_version_change(struct font_s *font, char old_version)
{
  font->header_size = 0;
  if(font->file_hdr) free(font->file_hdr);
  font->file_hdr = 0;
  raw_kill_unitab(font);
}


void raw_convert_to_psf(struct font_s *font)
{
  if(font->version == VER_PSF1)
  {
    struct psf1_header hdr;
    hdr.magic[0] = PSF1_MAGIC0;
    hdr.magic[1] = PSF1_MAGIC1;
    hdr.mode = 0;
    hdr.charsize = font->charsize;
    /* shift font structure */
    long sz = sizeof(struct psf1_header);
    unsigned char *new_hdr = (unsigned char *)malloc(sz);
    if(!new_hdr) { status_error("Insufficient memory"); return; }
    memcpy((void *)new_hdr, (void *)&hdr, sizeof(struct psf1_header));
    font->file_hdr = new_hdr;
    font->header_size = sz;
  }
  else if(font->version == VER_PSF2)
  {
    struct psf2_header hdr2;
    hdr2.magic[0] = PSF2_MAGIC0;
    hdr2.magic[1] = PSF2_MAGIC1;
    hdr2.magic[2] = PSF2_MAGIC2;
    hdr2.magic[3] = PSF2_MAGIC3;
    hdr2.version = 0;
    hdr2.length = font->length;
    hdr2.charsize = font->charsize;
    hdr2.height = font->height;
    hdr2.width = font->width;
    hdr2.headersize = sizeof(struct psf2_header);
    hdr2.flags = 0;
    /* shift font structure */
    long sz = sizeof(struct psf2_header);
    unsigned char *new_hdr = (unsigned char *)malloc(sz);
    if(!new_hdr) { status_error("Insufficient memory"); return; }
    memcpy((void *)new_hdr, (void *)&hdr2, sizeof(struct psf2_header));
    font->file_hdr = new_hdr;
    font->header_size = sz;
  }
  force_font_dirty(font);
}

/*
void raw_shrink_glyphs(struct font_s *font, int old_length)
{
  psf_shrink_glyphs(font, old_length);
}

void raw_expand_glyphs(struct font_s *font, int old_length, int option)
{
  psf_expand_glyphs(font, old_length, option);
}
*/

/********************************
 * ******************************
 * ******************************/
struct module_s raw_module;

void raw_init_module()
{
  strcpy(raw_module.mod_name, "raw");
  raw_module.max_width = 8;
  raw_module.max_height = 16;
  raw_module.max_length = 256;
  raw_module.create_empty_font = raw_create_empty_font;
  raw_module.write_to_file = raw_write_to_file;
  raw_module.load_font = raw_load_font;
  raw_module.load_font_file = raw_load_font_file;
  raw_module.handle_hw_change = raw_handle_hw_change;
  raw_module.shrink_glyphs = NULL;//raw_shrink_glyphs;
  raw_module.expand_glyphs = NULL;//raw_expand_glyphs;
  raw_module.update_font_hdr = NULL;
  raw_module.handle_version_change = raw_handle_version_change;
  raw_module.handle_unicode_table_change = raw_handle_unicode_table_change;
  raw_module.export_unitab = NULL; //raw_export_unitab;
  raw_module.create_unitab = raw_create_unitab;
  raw_module.kill_unitab = raw_kill_unitab;
  raw_module.convert_to_psf = raw_convert_to_psf;
  raw_module.make_utf16_unitab = NULL;
  register_module(&raw_module);
  //add_file_extension("fnt", "raw");
  //add_file_extension("", "raw");
}
