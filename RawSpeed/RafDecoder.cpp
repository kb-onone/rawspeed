#include "StdAfx.h"
#include "RafDecoder.h"

/*
    RawSpeed - RAW file decoder.

    Copyright (C) 2009-2014 Klaus Post
    Copyright (C) 2014-2015 Pedro Côrte-Real

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

    http://www.klauspost.com
*/
namespace RawSpeed {

char xtrans_abs[6][6] {

   { 0, 2, 1, 2, 0, 1 },
   { 1, 1, 0, 1, 1, 2 },
   { 1, 1, 2, 1, 1, 0 },
   { 2, 0, 1, 0, 2, 1 },
   { 1, 1, 2, 1, 1, 0 },
   { 1, 1, 0, 1, 1, 2 },


/*
    2, 1, 1, 0, 1, 1,
    0, 1, 1, 2, 1, 1,
    1, 2, 0, 1, 0, 2,
    0, 1, 1, 2, 1, 1,
    2, 1, 1, 0, 1, 1,
    1, 0, 2, 1, 2, 0,
*/
    /*
    1, 0, 2, 1, 2, 0,
    2, 1, 1, 0, 1, 1,
    0, 1, 1, 2, 1, 1,
    1, 2, 0, 1, 0, 2,
    0, 1, 1, 2, 1, 1,
    2, 1, 1, 0, 1, 1
    */

    /*
    2, 0, 1, 0, 2, 1,
    1, 1, 0, 1, 1, 2,
    1, 1, 2, 1, 1, 0,
    0, 2, 1, 2, 0, 1,
    1, 1, 2, 1, 1, 0,
    1, 1, 0, 1, 1, 2,

    */
};


RafDecoder::RafDecoder(TiffIFD *rootIFD, FileMap* file) :
    RawDecoder(file), mRootIFD(rootIFD) {
  decoderVersion = 1;
  alt_layout = FALSE;
}
RafDecoder::~RafDecoder(void) {
  if (mRootIFD)
    delete mRootIFD;
  mRootIFD = NULL;
}

RawImage RafDecoder::decodeRawInternal() {
  vector<TiffIFD*> data = mRootIFD->getIFDsWithTag(FUJI_STRIPOFFSETS);

  if (data.empty())
    ThrowRDE("Fuji decoder: Unable to locate raw IFD");

  TiffIFD* raw = data[0];
  mFile = raw->getFileMap();
  uint32 height = 0;
  uint32 width = 0;



  if (raw->hasEntry(FUJI_RAWIMAGEFULLHEIGHT)) {
    height = raw->getEntry(FUJI_RAWIMAGEFULLHEIGHT)->getInt();
    width = raw->getEntry(FUJI_RAWIMAGEFULLWIDTH)->getInt();
  } else if (raw->hasEntry(IMAGEWIDTH)) {
    TiffEntry *e = raw->getEntry(IMAGEWIDTH);
    if (e->count < 2)
      ThrowRDE("Fuji decoder: Size array too small");
    height = e->getShort(0);
    width = e->getShort(1);
  } 
  if (raw->hasEntry(FUJI_LAYOUT)) {
    TiffEntry *e = raw->getEntry(FUJI_LAYOUT);
    if (e->count < 2)
      ThrowRDE("Fuji decoder: Layout array too small");
    const uchar8 *layout = e->getData();
    alt_layout = !(layout[0] >> 7);


  }

  if (width <= 0 ||  height <= 0)
    ThrowRDE("RAF decoder: Unable to locate image size");

  TiffEntry *offsets = raw->getEntry(FUJI_STRIPOFFSETS);
  TiffEntry *counts = raw->getEntry(FUJI_STRIPBYTECOUNTS);

  if (offsets->count != 1 || counts->count != 1)
    ThrowRDE("RAF Decoder: Multiple Strips found: %u %u", offsets->count, counts->count);

  int off = offsets->getInt();
  int count = counts->getInt();

  int bps = 16;
  if (raw->hasEntry(FUJI_BITSPERSAMPLE))
    bps = raw->getEntry(FUJI_BITSPERSAMPLE)->getInt();

  // x-trans sensors report 14bpp, but data isn't packed so read as 16bpp

  bool bpsOverride = hints.find("bps_override") != hints.end();

  if (bps == 14 && ! bpsOverride )
      bps = 16;

  // Some fuji SuperCCD cameras include a second raw image next to the first one
  // that is identical but darker to the first. The two combined can produce
  // a higher dynamic range image. Right now we're ignoring it.
  bool double_width = hints.find("double_width_unpacked") != hints.end();

  mRaw->dim = iPoint2D(width*(double_width ? 2 : 1), height);
  mRaw->createData();
  ByteStream input(mFile, off);
  iPoint2D pos(0, 0);

  ByteStream inputCheck(mFile, off);

  if ( parse_xtrans_header(&inputCheck) /* count*8/(width*height) < 10 */ ) {

      if ( parse_xtrans_header(&input) ) {

          vector<TiffIFD*> vec  = mRootIFD->getIFDsWithTag((TiffTag)0x131);

          TiffEntry *layout = 0;

          for(int i = 0; i < vec.size(); i++) {

              if ( vec[i]->hasEntry((TiffTag)0x131) ) {
                  TiffEntry *entry = vec[i]->getEntry((TiffTag)0x131);

//                  printf("Count %d Type %d\n", entry->count, entry->type);

                  if ( entry->count == 36 && entry->type == 7 ) {
                      layout = entry;
                      break;
                  }

              }

          }

 //         fflush(stdout);

          char *tab = 0;

          if ( layout ) {

                const uchar8 *data = layout->getData();

                tab = (char *) layout->getData();

//                printf("Count %d Type %d\n", layout->count, layout->type);

                for (int c = 0; c < 36; c++) {
                    xtrans_abs[0][35-c] = data[c];
//                    printf("XTRANS_ABS %d\n", xtrans_abs[0][35-c]);
                }
/*
                for (int y = 0; y < 6; y++) {
                for (int x = 0; x < 6; x++)
                    printf("%d,", xtrans_abs[Y][X]);
                printf("\n");
                }

                fflush(stdout);
*/
          } else {


                char fixedTab[] = {    1,0,2,1,2,0,
                                  2,1,1,0,1,1,
                                  0,1,1,2,1,1,
                                  1,2,0,1,0,2,
                                  0,1,1,2,1,1,
                                  2,1,1,0,1,1 };


                tab = &fixedTab[0];

          }

          for (int c = 0; c < 36; c++)
              xtrans_abs[0][35-c] = tab[c];

            xtrans_compressed_load_raw(&input);

        } else
              ThrowRDE("Don't know how to decode compressed images of this format");

  } else if (double_width) {
    Decode16BitRawUnpacked(input, width*2, height);
  } else if (mRootIFD->endian == big) {
    Decode16BitRawBEunpacked(input, width, height);
  } else {
    if (hints.find("jpeg32_bitorder") != hints.end())
      readUncompressedRaw(input, mRaw->dim, pos, width*bps/8, bps, BitOrder_Jpeg32);
    else
      readUncompressedRaw(input, mRaw->dim, pos, width*bps/8, bps, BitOrder_Plain);
  }

  return mRaw;
}


void RafDecoder::decodeThreaded(RawDecoderThread * t) {
}

void RafDecoder::checkSupportInternal(CameraMetaData *meta) {
  vector<TiffIFD*> data = mRootIFD->getIFDsWithTag(MODEL);
  if (data.empty())
    ThrowRDE("RAF Support check: Model name found");

  string make = data[0]->getEntry(MAKE)->getString();
  string model = data[0]->getEntry(MODEL)->getString();


  string mode = getMode();

  if (!this->checkCameraSupported(meta, make, model, mode))
     ThrowRDE("RAFDecoder: Unknown camera. Will not guess.");
}

string RafDecoder::getMode()
{

    ostringstream mode;

    vector<TiffIFD*> data = mRootIFD->getIFDsWithTag(FUJI_STRIPOFFSETS);

    if (data.empty())
      ThrowRDE("Fuji decoder: Unable to locate raw IFD");

    TiffIFD* raw = data[0];
    mFile = raw->getFileMap();
    uint32 height = 0;
    uint32 width = 0;

    if (raw->hasEntry(FUJI_RAWIMAGEFULLHEIGHT)) {
      height = raw->getEntry(FUJI_RAWIMAGEFULLHEIGHT)->getInt();
      width = raw->getEntry(FUJI_RAWIMAGEFULLWIDTH)->getInt();
    } else if (raw->hasEntry(IMAGEWIDTH)) {
      TiffEntry *e = raw->getEntry(IMAGEWIDTH);
      if (e->count < 2)
        ThrowRDE("Fuji decoder: Size array too small");
      height = e->getShort(0);
      width = e->getShort(1);
    }
    if (raw->hasEntry(FUJI_LAYOUT)) {
      TiffEntry *e = raw->getEntry(FUJI_LAYOUT);
      if (e->count < 2)
        ThrowRDE("Fuji decoder: Layout array too small");
      const uchar8 *layout = e->getData();
      alt_layout = !(layout[0] >> 7);


    }

    if (width <= 0 ||  height <= 0)
      ThrowRDE("RAF decoder: Unable to locate image size");

    TiffEntry *offsets = raw->getEntry(FUJI_STRIPOFFSETS);
    TiffEntry *counts = raw->getEntry(FUJI_STRIPBYTECOUNTS);

    if (offsets->count != 1 || counts->count != 1)
      ThrowRDE("RAF Decoder: Multiple Strips found: %u %u", offsets->count, counts->count);

    int off = offsets->getInt();
    int count = counts->getInt();

    ByteStream input(mFile, off);

    if ( parse_xtrans_header(&input) /* count*8/(width*height) < 10 */ )
        mode << "compressed";
    else
        mode << "";

    return mode.str();
}


void RafDecoder::decodeMetaDataInternal(CameraMetaData *meta) {
  vector<TiffIFD*> data = mRootIFD->getIFDsWithTag(MODEL);

  if (data.empty())
    ThrowRDE("RAF Meta Decoder: Model name not found");
  if (!data[0]->hasEntry(MAKE))
    ThrowRDE("RAF Support: Make name not found");

  string make = data[0]->getEntry(MAKE)->getString();
  string model = data[0]->getEntry(MODEL)->getString();

  int iso = 0;
  if (mRootIFD->hasEntryRecursive(ISOSPEEDRATINGS))
    iso = mRootIFD->getEntryRecursive(ISOSPEEDRATINGS)->getInt();
  mRaw->metadata.isoSpeed = iso;



  string mode = getMode();

  // This is where we'd normally call setMetaData but since we may still need
  // to rotate the image for SuperCCD cameras we do everything ourselves
  TrimSpaces(make);
  TrimSpaces(model);
  Camera *cam = meta->getCamera(make, model, mode);
  if (!cam)
    ThrowRDE("RAF Meta Decoder: Couldn't find camera");

  iPoint2D new_size(mRaw->dim);
  iPoint2D crop_offset = iPoint2D(0,0);
  if (applyCrop) {
    new_size = cam->cropSize;
    crop_offset = cam->cropPos;
    bool double_width = hints.find("double_width_unpacked") != hints.end();
    // If crop size is negative, use relative cropping
    if (new_size.x <= 0)
      new_size.x = mRaw->dim.x / (double_width ? 2 : 1) - cam->cropPos.x + new_size.x;
    else
      new_size.x /= (double_width ? 2 : 1);

    if (new_size.y <= 0)
      new_size.y = mRaw->dim.y - cam->cropPos.y + new_size.y;
  }

  bool rotate = hints.find("fuji_rotate") != hints.end();
  rotate = rotate & fujiRotate;

  // Rotate 45 degrees - could be multithreaded.
  if (rotate && !this->uncorrectedRawValues) {
    // Calculate the 45 degree rotated size;
    uint32 rotatedsize;
    uint32 rotationPos;
    if (alt_layout) {
      rotatedsize = new_size.y+new_size.x/2;
      rotationPos = new_size.x/2 - 1;
    }
    else {
      rotatedsize = new_size.x+new_size.y/2;
      rotationPos = new_size.x - 1;
    }

    iPoint2D final_size(rotatedsize, rotatedsize-1);
    RawImage rotated = RawImage::create(final_size, TYPE_USHORT16, 1);
    rotated->clearArea(iRectangle2D(iPoint2D(0,0), rotated->dim));
    rotated->metadata = mRaw->metadata;
    rotated->metadata.fujiRotationPos = rotationPos;

    int dest_pitch = (int)rotated->pitch / 2;
    ushort16 *dst = (ushort16*)rotated->getData(0,0);

    for (int y = 0; y < new_size.y; y++) {
      ushort16 *src = (ushort16*)mRaw->getData(crop_offset.x, crop_offset.y + y);
      for (int x = 0; x < new_size.x; x++) {
        int h, w;
        if (alt_layout) { // Swapped x and y
          h = rotatedsize - (new_size.y + 1 - y + (x >> 1));
          w = ((x+1) >> 1) + y;
        } else {
          h = new_size.x - 1 - x + (y >> 1);
          w = ((y+1) >> 1) + x;
        }
        if (h < rotated->dim.y && w < rotated->dim.x)
          dst[w + h * dest_pitch] = src[x];
        else
          ThrowRDE("RAF Decoder: Trying to write out of bounds");
      }
    }
    mRaw = rotated;
  } else if (applyCrop) {
    mRaw->subFrame(iRectangle2D(crop_offset, new_size));
  }

  const CameraSensorInfo *sensor = cam->getSensorInfo(iso);
  mRaw->blackLevel = sensor->mBlackLevel;

  // at least the (bayer sensor) X100 comes with a tag like this:
  if (mRootIFD->hasEntryRecursive(FUJI_RGGBLEVELSBLACK))
  {
    TiffEntry *sep_black = mRootIFD->getEntryRecursive(FUJI_RGGBLEVELSBLACK);
    if (sep_black->count == 4)
    {
      for(int k=0;k<4;k++)
        mRaw->blackLevelSeparate[k] = sep_black->getInt(k);
    }
  }
  mRaw->whitePoint = sensor->mWhiteLevel;
  mRaw->blackAreas = cam->blackAreas;
  mRaw->cfa = cam->cfa;
  mRaw->metadata.canonical_make = cam->canonical_make;
  mRaw->metadata.canonical_model = cam->canonical_model;
  mRaw->metadata.canonical_alias = cam->canonical_alias;
  mRaw->metadata.canonical_id = cam->canonical_id;
  mRaw->metadata.make = make;
  mRaw->metadata.model = model;

  if (mRootIFD->hasEntryRecursive(FUJI_WB_GRBLEVELS)) {
    TiffEntry *wb = mRootIFD->getEntryRecursive(FUJI_WB_GRBLEVELS);
    if (wb->count == 3) {
      mRaw->metadata.wbCoeffs[0] = wb->getFloat(1);
      mRaw->metadata.wbCoeffs[1] = wb->getFloat(0);
      mRaw->metadata.wbCoeffs[2] = wb->getFloat(2);
    }
  } else if (mRootIFD->hasEntryRecursive(FUJIOLDWB)) {
    TiffEntry *wb = mRootIFD->getEntryRecursive(FUJIOLDWB);
    if (wb->count == 8) {
      mRaw->metadata.wbCoeffs[0] = wb->getFloat(1);
      mRaw->metadata.wbCoeffs[1] = wb->getFloat(0);
      mRaw->metadata.wbCoeffs[2] = wb->getFloat(3);
    }
  }
}


/* -*- C++ -*-
 * File: libraw_xtrans_compressed.cpp
 * Copyright (C) 2016 Alexey Danilchenko
 *
 * Adopted to LibRaw by Alex Tutubalin, lexa@lexa.ru
 * LibRaw Fujifilm/compressed decoder
LibRaw is free software; you can redistribute it and/or modify
it under the terms of the one of three licenses as you choose:
1. GNU LESSER GENERAL PUBLIC LICENSE version 2.1
   (See file LICENSE.LGPL provided in LibRaw distribution archive for details).
2. COMMON DEVELOPMENT AND DISTRIBUTION LICENSE (CDDL) Version 1.0
   (See file LICENSE.CDDL provided in LibRaw distribution archive for details).
 */

// This file is #included in libraw_cxx

#ifdef _abs
#undef _abs
#undef _min
#endif
#define _abs(x) (((int)(x) ^ ((int)(x) >> 31)) - ((int)(x) >> 31))
#define _min(a,b) ((a) < (b) ? (a) : (b))

#define FC(row,col) \
    (0x94949494 >> ((((row) << 1 & 14) + ((col) & 1)) << 1) & 3)


static void fuji_extend_generic(ushort *linebuf[_ltotal], int line_width, int start, int end)
{
  for (int i = start; i <= end; i++)
  {
    linebuf[i][0] = linebuf[i - 1][1];
    linebuf[i][line_width + 1] = linebuf[i - 1][line_width];
  }
}

static void fuji_extend_red(ushort *linebuf[_ltotal], int line_width)
{
  fuji_extend_generic(linebuf, line_width, _R2, _R4);
}

static void fuji_extend_green(ushort *linebuf[_ltotal], int line_width)
{
  fuji_extend_generic(linebuf, line_width, _G2, _G7);
}

static void fuji_extend_blue(ushort *linebuf[_ltotal], int line_width)
{
  fuji_extend_generic(linebuf, line_width, _B2, _B4);
}


static unsigned sgetn(int n, uchar *s)
{
    unsigned result = 0;
    while (n-- > 0)
        result = (result<<8) | (*s++);
    return result;
}

void RafDecoder::init_xtrans(struct xtrans_params* info)
{
    int cur_val, i;
    char *qt;

    if (internal_data.fuji_block_width % 3)
        return; // Error

    info->q_table = (char *) malloc(32768);
//    merror(info->q_table, "init_xtrans()");

    if (internal_data.fuji_raw_type == 16)
//      info->line_width = (libraw_internal_data.unpacker_data.fuji_block_width * 2) / 3;
        info->line_width = (internal_data.fuji_block_width*2)/3;
    else
      info->line_width = internal_data.fuji_block_width >> 1;


    info->q_point[0] = 0;
    info->q_point[1] = 0x12;
    info->q_point[2] = 0x43;
    info->q_point[3] = 0x114;
    info->q_point[4] = (1 << internal_data.fuji_bits) - 1;
    info->min_value = 0x40;

    cur_val = -info->q_point[4];
    for (qt = info->q_table; cur_val<=info->q_point[4]; ++qt, ++cur_val)
    {
        if (cur_val<= -info->q_point[3])
            *qt = -4;
        else if (cur_val<= -info->q_point[2])
            *qt = -3;
        else if (cur_val<= -info->q_point[1])
            *qt = -2;
        else if (cur_val< 0)
            *qt = -1;
        else if (cur_val== 0)
            *qt = 0;
        else if (cur_val< info->q_point[1])
            *qt = 1;
        else if (cur_val< info->q_point[2])
            *qt = 2;
        else if (cur_val< info->q_point[3])
            *qt = 3;
        else
            *qt = 4;
    }

    // populting gradients
    if (info->q_point[4] == 0x3FFF)
    {
        info->total_values = 0x4000;
        info->raw_bits = 14;
        info->max_bits = 56;
        info->maxDiff = 256;
    }
    else if (info->q_point[4] == 0xFFF)
    {
        info->total_values = 4096;
        info->raw_bits = 12;
        info->max_bits = 48;
        info->maxDiff = 64;
    }
    else
        return; // Error
}


#define XTRANS_BUF_SIZE 0x10000

static inline void fuji_fill_buffer(struct xtrans_block *info)
{
    if (info->cur_pos >= info->cur_buf_size)
    {
        info->cur_pos = 0;
        info->cur_buf_offset += info->cur_buf_size;
        {
 //           info->input->seek(info->cur_buf_offset, SEEK_SET);
            info->input->setAbsoluteOffset(info->cur_buf_offset);

            // info->cur_buf_size=info->input->read(info->cur_buf, 1, _min(info->max_read_size,XTRANS_BUF_SIZE));

            int data = _min(info->max_read_size, XTRANS_BUF_SIZE);

            info->cur_buf_size = data;
            memcpy(info->cur_buf, info->input->getData(), data);
            info->input->skipBytes(data);

            /*
            for (int i = 0; i < data; i++) {
                info->cur_buf[i] = info->input->getByte();
            }
            */

            if(info->cur_buf_size<1) // nothing read
                return; // ERROR throw LIBRAW_EXCEPTION_IO_EOF;
            info->max_read_size -= info->cur_buf_size;

        }
    }
}

void RafDecoder::init_xtrans_block(struct xtrans_block* info, const struct xtrans_params *params, INT64 raw_offset, unsigned dsize)
{
    info->linealloc = (ushort*)calloc(sizeof(ushort),_ltotal*(params->line_width+2));
//    merror(info->linealloc, "init_xtrans_block()");

    INT64 fsize = internal_data.input->getOffset() + internal_data.input->getRemainSize();
    info->max_read_size = _min(unsigned(fsize-raw_offset),dsize+16); // Data size may be incorrect?

    info->input = internal_data.input;
    info->linebuf[_R0] = info->linealloc;
    for(int i = _R1; i<=_B4;i++)
        info->linebuf[i] = info->linebuf[i-1] + params->line_width + 2;

    // init buffer
    info->cur_buf = (uchar*)malloc(XTRANS_BUF_SIZE);
//    merror(info->cur_buf, "init_xtrans_block()");
    info->cur_bit = 0;
    info->cur_pos = 0;
    info->cur_buf_offset = raw_offset;
    for(int j=0; j < 3; j++)
        for (int i=0; i<41; i++)
        {
            info->grad_even[j][i].value1 = params->maxDiff;
            info->grad_even[j][i].value2 = 1;
            info->grad_odd[j][i].value1 = params->maxDiff;
            info->grad_odd[j][i].value2 = 1;
        }

    info->cur_buf_size = 0;
    fuji_fill_buffer(info);
}




void RafDecoder::copy_line_to_xtrans(struct xtrans_block* info, int cur_line, int cur_block, int cur_block_width)
{
    ushort *lineBufB[3];
    ushort *lineBufG[6];
    ushort *lineBufR[3];
    int pixel_count;
    ushort* line_buf;
    int index;

    int offset = internal_data.fuji_block_width*cur_block + 6 * raw_width * cur_line;

    ushort* raw_block_data = (ushort *) (mRaw->getData() + offset * sizeof(ushort));
    int row_count = 0;

    for(int i = 0; i<3; i++)
    {
        lineBufR[i] = info->linebuf[_R2+i] + 1;
        lineBufB[i] = info->linebuf[_B2+i] + 1;
    }
    for(int i = 0; i<6; i++)
        lineBufG[i] = info->linebuf[_G2+i] + 1;

    while (row_count < 6)
    {
        pixel_count = 0;
        while (pixel_count < cur_block_width)
        {
            int color = xtrans_abs[row_count][(pixel_count % 6)];
            switch (color)
            {
            case 0:     // red
                line_buf = lineBufR[row_count >> 1];
                break;

            case 1:     // green
                line_buf = lineBufG[row_count];
                break;
            case 2:     // blue
                line_buf = lineBufB[row_count >> 1];
                break;
            }

            // index = (((pixel_count*2/3) & 0x7FFFFFFE) | (pixel_count % 3) & 1) + ((pixel_count % 3) >> 1);

            index = (((pixel_count*2/3) & 0x7FFFFFFE) | ((pixel_count % 3) & 1)) + ((pixel_count % 3) >> 1);

            raw_block_data[pixel_count] = line_buf[index];

            ++pixel_count;
        }
        ++row_count;
        raw_block_data += raw_width;
    }
}


void RafDecoder::copy_line_to_bayer(struct xtrans_block *info, int cur_line, int cur_block, int cur_block_width)
{
  ushort *lineBufB[3];
  ushort *lineBufG[6];
  ushort *lineBufR[3];
  unsigned pixel_count;
  ushort *line_buf;

  int fuji_bayer[2][2];
  for (int r = 0; r < 2; r++)
    for (int c = 0; c < 2; c++)
      fuji_bayer[r][c] = FC(r, c); // We'll downgrade G2 to G below

  int offset = internal_data.fuji_block_width * cur_block + 6 * raw_width * cur_line;
  ushort* raw_block_data = (ushort *) (mRaw->getData() + offset * sizeof(ushort));

  int row_count = 0;

  for (int i = 0; i < 3; i++)
  {
    lineBufR[i] = info->linebuf[_R2 + i] + 1;
    lineBufB[i] = info->linebuf[_B2 + i] + 1;
  }
  for (int i = 0; i < 6; i++)
    lineBufG[i] = info->linebuf[_G2 + i] + 1;

  while (row_count < 6)
  {
    pixel_count = 0;
    while (pixel_count < cur_block_width)
    {
      switch (fuji_bayer[row_count & 1][pixel_count & 1])
      {
      case 0: // red
        line_buf = lineBufR[row_count >> 1];
        break;
      case 1:  // green
      case 3:  // second green
      default: // to make static analyzer happy
        line_buf = lineBufG[row_count];
        break;
      case 2: // blue
        line_buf = lineBufB[row_count >> 1];
        break;
      }

      raw_block_data[pixel_count] = line_buf[pixel_count >> 1];
      ++pixel_count;
    }
    ++row_count;
    raw_block_data += raw_width;
  }
}



#define fuji_quant_gradient(i,v1,v2) (9*i->q_table[i->q_point[4]+(v1)] + i->q_table[i->q_point[4]+(v2)])


static inline void fuji_zerobits(struct xtrans_block* info, int *count)
{
    uchar zero = 0;
    *count = 0;
    while (zero == 0)
    {
        zero = (info->cur_buf[info->cur_pos] >> (7 - info->cur_bit)) & 1;
        info->cur_bit++;
        info->cur_bit &= 7;
        if (!info->cur_bit)
        {
            ++info->cur_pos;
            fuji_fill_buffer(info);
        }
        if (zero)
            break;
        ++*count;
    }
}

static inline void fuji_read_code(struct xtrans_block* info, int *data, int bits_to_read)
{
    uchar bits_left = bits_to_read;
    uchar bits_left_in_byte = 8 - (info->cur_bit & 7);
    *data = 0;
    if (!bits_to_read)
        return;
    if (bits_to_read >= bits_left_in_byte)
    {
        do
        {
            *data <<= bits_left_in_byte;
            bits_left -= bits_left_in_byte;
            *data |= info->cur_buf[info->cur_pos] & ((1 << bits_left_in_byte) - 1);
            ++info->cur_pos;
            fuji_fill_buffer(info);
            bits_left_in_byte = 8;
        }
        while (bits_left >= 8);
    }
    if (!bits_left)
    {
        info->cur_bit = (8 - (bits_left_in_byte & 7)) & 7;
        return;
    }
    *data <<= bits_left;
    bits_left_in_byte -= bits_left;
    *data |= ((1 << bits_left) - 1) & ((unsigned)info->cur_buf[info->cur_pos] >> bits_left_in_byte);
    info->cur_bit = (8 - (bits_left_in_byte & 7)) & 7;
}

static inline int bitDiff(int value1, int value2)
{
    int decBits = 0;
    if ( value2 < value1 )
        while (decBits <= 12 &&
            (value2 << ++decBits)
            <  value1)
            ;
    return decBits;
}

static inline int fuji_decode_sample_even(struct xtrans_block* info, const struct xtrans_params * params, ushort* line_buf, int pos, struct int_pair* grads)
{
    int interp_val = 0;
    //ushort decBits;
    int errcnt=0;

    int sample=0, code=0;
    ushort* line_buf_cur = line_buf + pos;
    int Rb = line_buf_cur[-2 - params->line_width];
    int Rc = line_buf_cur[-3 - params->line_width];
    int Rd = line_buf_cur[-1 - params->line_width];
    int Rf = line_buf_cur[-4 - 2*params->line_width];

    int grad, gradient, diffRcRb, diffRfRb, diffRdRb;

    grad = fuji_quant_gradient(params, Rb - Rf, Rc - Rb);
    gradient = _abs(grad);
    diffRcRb = _abs(Rc - Rb);
    diffRfRb = _abs(Rf - Rb);
    diffRdRb = _abs(Rd - Rb);

    if ( diffRcRb > diffRfRb && diffRcRb > diffRdRb )
        interp_val = Rf + Rd + 2 * Rb;
    else if ( diffRdRb > diffRcRb && diffRdRb > diffRfRb )
        interp_val = Rf + Rc + 2 * Rb;
    else
        interp_val = Rd + Rc + 2 * Rb;


    fuji_zerobits(info, &sample);

    if (sample < params->max_bits - params->raw_bits - 1)
    {
        int decBits = bitDiff(grads[gradient].value1, grads[gradient].value2);
        fuji_read_code(info, &code, decBits);
        code += sample << decBits;
    }
    else
    {
        fuji_read_code(info, &code, params->raw_bits);
        code++;
    }

    if (code < 0 || code >= params->total_values)
        errcnt++;

    if (code & 1)
        code = -1 - code/2;
    else
        code /= 2;

    grads[gradient].value1 += _abs(code);
    if (grads[gradient].value2 == params->min_value )
    {
        grads[gradient].value1 >>= 1;
        grads[gradient].value2 >>= 1;
    }
    grads[gradient].value2++;
    if (grad < 0)
        interp_val = (interp_val >> 2) - code;
    else
        interp_val = (interp_val >> 2) + code;
    if ( interp_val < 0 )
        interp_val += params->total_values;
    else if (interp_val > params->q_point[4])
        interp_val -= params->total_values;

    if ( interp_val >= 0 )
        line_buf_cur[0] = _min(interp_val, params->q_point[4]);
    else
        line_buf_cur[0] = 0;
    return errcnt;
}

static inline int fuji_decode_sample_odd(struct xtrans_block* info, const struct xtrans_params * params, ushort* line_buf, int pos, struct int_pair* grads)
{
    int interp_val = 0;
    int errcnt = 0;

    int sample=0, code=0;
    ushort* line_buf_cur = line_buf + pos;
    int Ra = line_buf_cur[-1];
    int Rb = line_buf_cur[-2 - params->line_width];
    int Rc = line_buf_cur[-3 - params->line_width];
    int Rd = line_buf_cur[-1 - params->line_width];
    int Rg = line_buf_cur[1];

    int grad, gradient;

    grad = fuji_quant_gradient(params, Rb - Rc, Rc - Ra);
    gradient = _abs(grad);

    if ((Rb > Rc && Rb > Rd) || (Rb < Rc && Rb < Rd))
        interp_val = (Rg + Ra + 2 * Rb) >> 2;
    else
        interp_val = (Ra + Rg) >> 1;

    fuji_zerobits(info, &sample);

    if (sample < params->max_bits - params->raw_bits - 1)
    {
        int decBits = bitDiff(grads[gradient].value1, grads[gradient].value2);
        fuji_read_code(info, &code, decBits);
        code += sample << decBits;
    }
    else
    {
        fuji_read_code(info, &code, params->raw_bits);
        code++;
    }

    if (code < 0 || code >= params->total_values)
        errcnt++;

    if (code & 1)
        code = -1 - code/2;
    else
        code /= 2;

    grads[gradient].value1 += _abs(code);
    if (grads[gradient].value2 == params->min_value)
    {
        grads[gradient].value1 >>= 1;
        grads[gradient].value2 >>= 1;
    }
    grads[gradient].value2++;
    if (grad < 0)
        interp_val -= code;
    else
        interp_val += code;
    if ( interp_val < 0 )
        interp_val += params->total_values;
    else if (interp_val > params->q_point[4])
        interp_val -= params->total_values;

    if ( interp_val >= 0 )
        line_buf_cur[0] = _min(interp_val, params->q_point[4]);
    else
        line_buf_cur[0] = 0;
    return errcnt;
}

static void fuji_decode_interpolation_even(int line_width, ushort* line_buf, int pos)
{
    ushort* line_buf_cur = line_buf + pos;
    int Rb = line_buf_cur[-2 - line_width];
    int Rc = line_buf_cur[-3 - line_width];
    int Rd = line_buf_cur[-1 - line_width];
    int Rf = line_buf_cur[-4 - 2*line_width];
    int diffRcRb = _abs(Rc - Rb);
    int diffRfRb = _abs(Rf - Rb);
    int diffRdRb = _abs(Rd - Rb);
    if ( diffRcRb > diffRfRb && diffRcRb > diffRdRb )
        *line_buf_cur = (Rf + Rd + 2 * Rb) >> 2;
    else if ( diffRdRb > diffRcRb && diffRdRb > diffRfRb )
        *line_buf_cur = (Rf + Rc + 2 * Rb) >> 2;
    else
        *line_buf_cur = (Rd + Rc + 2 * Rb) >> 2;
}

static void xtrans_extend_generic(ushort *linebuf[_ltotal], int line_width, int start, int end)
{
    for(int i = start; i<= end; i++)
    {
        linebuf[i][0]             = linebuf[i-1][1];
        linebuf[i][line_width + 1] = linebuf[i-1][line_width];
    }
}

static void xtrans_extend_red(ushort *linebuf[_ltotal], int line_width)
{
    xtrans_extend_generic(linebuf,line_width,_R2,_R4);
}

static void xtrans_extend_green(ushort *linebuf[_ltotal], int line_width)
{
    xtrans_extend_generic(linebuf,line_width,_G2,_G7);
}

static void xtrans_extend_blue(ushort *linebuf[_ltotal], int line_width)
{
    xtrans_extend_generic(linebuf,line_width,_B2,_B4);
}



void RafDecoder::xtrans_decode_block(struct xtrans_block* info, const struct xtrans_params *params, int cur_line)
{
    int r_even_pos = 0, r_odd_pos = 1;
    int g_even_pos = 0, g_odd_pos = 1;
    int b_even_pos = 0, b_odd_pos = 1;

    int errcnt = 0;

    const int line_width = params->line_width;

    while (g_even_pos < line_width || g_odd_pos < line_width)
    {
        if (g_even_pos < line_width)
        {
            fuji_decode_interpolation_even(line_width, info->linebuf[_R2] + 1, r_even_pos);
            r_even_pos += 2;
            errcnt+=fuji_decode_sample_even(info, params, info->linebuf[_G2] + 1, g_even_pos, info->grad_even[0]);
            g_even_pos += 2;
        }
        if (g_even_pos > 8)
        {
            errcnt+=fuji_decode_sample_odd(info, params, info->linebuf[_R2] + 1, r_odd_pos, info->grad_odd[0]);
            r_odd_pos += 2;
            errcnt+=fuji_decode_sample_odd(info, params, info->linebuf[_G2] + 1, g_odd_pos, info->grad_odd[0]);
            g_odd_pos += 2;
        }
    }

    xtrans_extend_red(info->linebuf,line_width);
    xtrans_extend_green(info->linebuf,line_width);

    g_even_pos = 0, g_odd_pos = 1;

    while (g_even_pos < line_width || g_odd_pos < line_width)
    {
        if (g_even_pos < line_width)
        {
            errcnt+=fuji_decode_sample_even(info, params, info->linebuf[_G3] + 1, g_even_pos, info->grad_even[1]);
            g_even_pos += 2;
            fuji_decode_interpolation_even(line_width,info->linebuf[_B2] + 1, b_even_pos);
            b_even_pos += 2;
        }
        if (g_even_pos > 8)
        {
            errcnt+=fuji_decode_sample_odd(info,params, info->linebuf[_G3] + 1, g_odd_pos, info->grad_odd[1]);
            g_odd_pos += 2;
            errcnt+=fuji_decode_sample_odd(info,params, info->linebuf[_B2] + 1, b_odd_pos, info->grad_odd[1]);
            b_odd_pos += 2;
        }
    }

    xtrans_extend_green(info->linebuf,line_width);
    xtrans_extend_blue(info->linebuf,line_width);

    r_even_pos = 0, r_odd_pos = 1;
    g_even_pos = 0, g_odd_pos = 1;

    while (g_even_pos < line_width || g_odd_pos < line_width)
    {
        if (g_even_pos < line_width)
        {
            if (r_even_pos & 3)
                errcnt+=fuji_decode_sample_even(info, params, info->linebuf[_R3] + 1, r_even_pos, info->grad_even[2]);
            else
                fuji_decode_interpolation_even(line_width, info->linebuf[_R3] + 1, r_even_pos);
            r_even_pos += 2;
            fuji_decode_interpolation_even(line_width, info->linebuf[_G4] + 1, g_even_pos);
            g_even_pos += 2;
        }
        if (g_even_pos > 8)
        {
            errcnt+=fuji_decode_sample_odd(info, params,info->linebuf[_R3] + 1, r_odd_pos, info->grad_odd[2]);
            r_odd_pos += 2;
            errcnt+=fuji_decode_sample_odd(info,params, info->linebuf[_G4] + 1, g_odd_pos, info->grad_odd[2]);
            g_odd_pos += 2;
        }
    }

    xtrans_extend_red(info->linebuf,line_width);
    xtrans_extend_green(info->linebuf,line_width);

    g_even_pos = 0, g_odd_pos = 1;
    b_even_pos = 0, b_odd_pos = 1;

    while (g_even_pos < line_width || g_odd_pos < line_width)
    {
        if (g_even_pos < line_width)
        {
            errcnt+=fuji_decode_sample_even(info,params, info->linebuf[_G5] + 1, g_even_pos, info->grad_even[0]);
            g_even_pos += 2;
            if ((b_even_pos & 3) == 2)
                fuji_decode_interpolation_even(line_width, info->linebuf[_B3] + 1, b_even_pos);
            else
                errcnt+=fuji_decode_sample_even(info, params,info->linebuf[_B3] + 1, b_even_pos, info->grad_even[0]);
            b_even_pos += 2;
        }
        if (g_even_pos > 8)
        {
            errcnt+=fuji_decode_sample_odd(info, params,info->linebuf[_G5] + 1, g_odd_pos, info->grad_odd[0]);
            g_odd_pos += 2;
            errcnt+=fuji_decode_sample_odd(info, params,info->linebuf[_B3] + 1, b_odd_pos, info->grad_odd[0]);
            b_odd_pos += 2;
        }
    }

    xtrans_extend_green(info->linebuf,line_width);
    xtrans_extend_blue(info->linebuf,line_width);

    r_even_pos = 0, r_odd_pos = 1;
    g_even_pos = 0, g_odd_pos = 1;

    while (g_even_pos < line_width || g_odd_pos < line_width)
    {
        if (g_even_pos < line_width)
        {
            if ((r_even_pos & 3) == 2)
                fuji_decode_interpolation_even(line_width, info->linebuf[_R4] + 1, r_even_pos);
            else
                errcnt+=fuji_decode_sample_even(info,params, info->linebuf[_R4] + 1, r_even_pos, info->grad_even[1]);
            r_even_pos += 2;
            errcnt+=fuji_decode_sample_even(info,params, info->linebuf[_G6] + 1, g_even_pos, info->grad_even[1]);
            g_even_pos += 2;
        }
        if (g_even_pos > 8)
        {
            errcnt+=fuji_decode_sample_odd(info,params, info->linebuf[_R4] + 1, r_odd_pos, info->grad_odd[1]);
            r_odd_pos += 2;
            errcnt+=fuji_decode_sample_odd(info,params, info->linebuf[_G6] + 1, g_odd_pos, info->grad_odd[1]);
            g_odd_pos += 2;
        }
    }

    xtrans_extend_red(info->linebuf,line_width);
    xtrans_extend_green(info->linebuf,line_width);

    g_even_pos = 0, g_odd_pos = 1;
    b_even_pos = 0, b_odd_pos = 1;

    while (g_even_pos < line_width || g_odd_pos < line_width)
    {
        if (g_even_pos < line_width)
        {
            fuji_decode_interpolation_even(line_width, info->linebuf[_G7] + 1, g_even_pos);
            g_even_pos += 2;
            if (b_even_pos & 3)
                errcnt+=fuji_decode_sample_even(info,params, info->linebuf[_B4] + 1, b_even_pos, info->grad_even[2]);
            else
                fuji_decode_interpolation_even(line_width, info->linebuf[_B4] + 1, b_even_pos);
            b_even_pos += 2;
        }
        if (g_even_pos > 8)
        {
            errcnt+=fuji_decode_sample_odd(info, params,info->linebuf[_G7] + 1, g_odd_pos, info->grad_odd[2]);
            g_odd_pos += 2;
            errcnt+=fuji_decode_sample_odd(info, params,info->linebuf[_B4] + 1, b_odd_pos, info->grad_odd[2]);
            b_odd_pos += 2;
        }
    }

    xtrans_extend_green(info->linebuf,line_width);
    xtrans_extend_blue(info->linebuf,line_width);

    if(errcnt)
        return; // ERROR // derror();
}

void RafDecoder::fuji_bayer_decode_block(struct xtrans_block *info, const struct xtrans_params *params,
                                     int cur_line)
{
  int r_even_pos = 0, r_odd_pos = 1;
  int g_even_pos = 0, g_odd_pos = 1;
  int b_even_pos = 0, b_odd_pos = 1;

  int errcnt = 0;

  const int line_width = params->line_width;

  while (g_even_pos < line_width || g_odd_pos < line_width)
  {
    if (g_even_pos < line_width)
    {
      errcnt += fuji_decode_sample_even(info, params, info->linebuf[_R2] + 1, r_even_pos, info->grad_even[0]);
      r_even_pos += 2;
      errcnt += fuji_decode_sample_even(info, params, info->linebuf[_G2] + 1, g_even_pos, info->grad_even[0]);
      g_even_pos += 2;
    }
    if (g_even_pos > 8)
    {
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_R2] + 1, r_odd_pos, info->grad_odd[0]);
      r_odd_pos += 2;
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_G2] + 1, g_odd_pos, info->grad_odd[0]);
      g_odd_pos += 2;
    }
  }

  fuji_extend_red(info->linebuf, line_width);
  fuji_extend_green(info->linebuf, line_width);

  g_even_pos = 0, g_odd_pos = 1;

  while (g_even_pos < line_width || g_odd_pos < line_width)
  {
    if (g_even_pos < line_width)
    {
      errcnt += fuji_decode_sample_even(info, params, info->linebuf[_G3] + 1, g_even_pos, info->grad_even[1]);
      g_even_pos += 2;
      errcnt += fuji_decode_sample_even(info, params, info->linebuf[_B2] + 1, b_even_pos, info->grad_even[1]);
      b_even_pos += 2;
    }
    if (g_even_pos > 8)
    {
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_G3] + 1, g_odd_pos, info->grad_odd[1]);
      g_odd_pos += 2;
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_B2] + 1, b_odd_pos, info->grad_odd[1]);
      b_odd_pos += 2;
    }
  }

  fuji_extend_green(info->linebuf, line_width);
  fuji_extend_blue(info->linebuf, line_width);

  r_even_pos = 0, r_odd_pos = 1;
  g_even_pos = 0, g_odd_pos = 1;

  while (g_even_pos < line_width || g_odd_pos < line_width)
  {
    if (g_even_pos < line_width)
    {
      errcnt += fuji_decode_sample_even(info, params, info->linebuf[_R3] + 1, r_even_pos, info->grad_even[2]);
      r_even_pos += 2;
      errcnt += fuji_decode_sample_even(info, params, info->linebuf[_G4] + 1, g_even_pos, info->grad_even[2]);
      g_even_pos += 2;
    }
    if (g_even_pos > 8)
    {
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_R3] + 1, r_odd_pos, info->grad_odd[2]);
      r_odd_pos += 2;
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_G4] + 1, g_odd_pos, info->grad_odd[2]);
      g_odd_pos += 2;
    }
  }

  fuji_extend_red(info->linebuf, line_width);
  fuji_extend_green(info->linebuf, line_width);

  g_even_pos = 0, g_odd_pos = 1;
  b_even_pos = 0, b_odd_pos = 1;

  while (g_even_pos < line_width || g_odd_pos < line_width)
  {
    if (g_even_pos < line_width)
    {
      errcnt += fuji_decode_sample_even(info, params, info->linebuf[_G5] + 1, g_even_pos, info->grad_even[0]);
      g_even_pos += 2;
      errcnt += fuji_decode_sample_even(info, params, info->linebuf[_B3] + 1, b_even_pos, info->grad_even[0]);
      b_even_pos += 2;
    }
    if (g_even_pos > 8)
    {
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_G5] + 1, g_odd_pos, info->grad_odd[0]);
      g_odd_pos += 2;
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_B3] + 1, b_odd_pos, info->grad_odd[0]);
      b_odd_pos += 2;
    }
  }

  fuji_extend_green(info->linebuf, line_width);
  fuji_extend_blue(info->linebuf, line_width);

  r_even_pos = 0, r_odd_pos = 1;
  g_even_pos = 0, g_odd_pos = 1;

  while (g_even_pos < line_width || g_odd_pos < line_width)
  {
    if (g_even_pos < line_width)
    {
      errcnt += fuji_decode_sample_even(info, params, info->linebuf[_R4] + 1, r_even_pos, info->grad_even[1]);
      r_even_pos += 2;
      errcnt += fuji_decode_sample_even(info, params, info->linebuf[_G6] + 1, g_even_pos, info->grad_even[1]);
      g_even_pos += 2;
    }
    if (g_even_pos > 8)
    {
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_R4] + 1, r_odd_pos, info->grad_odd[1]);
      r_odd_pos += 2;
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_G6] + 1, g_odd_pos, info->grad_odd[1]);
      g_odd_pos += 2;
    }
  }

  fuji_extend_red(info->linebuf, line_width);
  fuji_extend_green(info->linebuf, line_width);

  g_even_pos = 0, g_odd_pos = 1;
  b_even_pos = 0, b_odd_pos = 1;

  while (g_even_pos < line_width || g_odd_pos < line_width)
  {
    if (g_even_pos < line_width)
    {
      errcnt += fuji_decode_sample_even(info, params, info->linebuf[_G7] + 1, g_even_pos, info->grad_even[2]);
      g_even_pos += 2;
      errcnt += fuji_decode_sample_even(info, params, info->linebuf[_B4] + 1, b_even_pos, info->grad_even[2]);
      b_even_pos += 2;
    }
    if (g_even_pos > 8)
    {
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_G7] + 1, g_odd_pos, info->grad_odd[2]);
      g_odd_pos += 2;
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_B4] + 1, b_odd_pos, info->grad_odd[2]);
      b_odd_pos += 2;
    }
  }

  fuji_extend_green(info->linebuf, line_width);
  fuji_extend_blue(info->linebuf, line_width);

}


void RafDecoder::xtrans_decode_strip(const struct xtrans_params* info_common, int cur_block, INT64 raw_offset, unsigned dsize)
{
    int cur_block_width, cur_line;
    unsigned line_size;
    struct xtrans_block info;

    init_xtrans_block(&info, info_common, raw_offset,dsize);
    line_size = sizeof(ushort)*(info_common->line_width+2);

    cur_block_width = internal_data.fuji_block_width;
    if (cur_block+1 == internal_data.fuji_total_blocks)
        cur_block_width = raw_width % internal_data.fuji_block_width;

    struct i_pair
    {
        int a,b;
    };
    const i_pair mtable[6] = { {_R0,_R3},{_R1,_R4},{_G0,_G6},{_G1,_G7},{_B0,_B3},{_B1,_B4}},
        ztable[3]= {{_R2,3},{_G2,6},{_B2,3}};
    for  (cur_line = 0; cur_line < internal_data.fuji_total_lines; cur_line++)
    {


//        info.lastlast = (cur_block == libraw_internal_data.unpacker_data.fuji_total_blocks-1) && (cur_line ==libraw_internal_data.unpacker_data.fuji_total_lines-1);
/*
    if (libraw_internal_data.unpacker_data.fuji_raw_type == 16)
      xtrans_decode_block(&info, info_common, cur_line);
    else
      fuji_bayer_decode_block(&info, info_common, cur_line);

    // copy data from line buffers and advance
    for (int i = 0; i < 6; i++)
      memcpy(info.linebuf[mtable[i].a], info.linebuf[mtable[i].b], line_size);

    if (libraw_internal_data.unpacker_data.fuji_raw_type == 16)
      copy_line_to_xtrans(&info, cur_line, cur_block, cur_block_width);
    else
      copy_line_to_bayer(&info, cur_line, cur_block, cur_block_width);
*/

        if ( internal_data.fuji_raw_type == 16 )
            xtrans_decode_block(&info, info_common, cur_line);
        else
            fuji_bayer_decode_block(&info, info_common, cur_line);

        // copy data from line buffers and advance
        for(int i=0; i < 6; i++)
            memcpy(info.linebuf[mtable[i].a], info.linebuf[mtable[i].b], line_size);

        if ( internal_data.fuji_raw_type == 16 )
            copy_line_to_xtrans(&info, cur_line, cur_block, cur_block_width);
        else
            copy_line_to_bayer(&info, cur_line, cur_block, cur_block_width);

        for(int i=0; i < 3; i++)
        {
            memset(info.linebuf[ztable[i].a], 0, ztable[i].b*line_size);
            info.linebuf[ztable[i].a][0]					= info.linebuf[ztable[i].a-1][1];
            info.linebuf[ztable[i].a][info_common->line_width + 1] = info.linebuf[ztable[i].a-1][info_common->line_width];
        }
    }

    // release data
    free(info.linealloc);
    free(info.cur_buf);
}



void RafDecoder::xtrans_compressed_load_raw(ByteStream *input)
{
    struct xtrans_params common_info;
    int cur_block;
    unsigned line_size, *block_sizes;
    INT64 raw_offset, *raw_block_offsets;
    //struct xtrans_block info;

    init_xtrans(&common_info);
    line_size = sizeof(ushort)*(common_info.line_width+2);

    // read block sizes
    block_sizes = (unsigned*) malloc(sizeof(unsigned)* internal_data.fuji_total_blocks);
//    merror(block_sizes, "xtrans_load_raw()");
    raw_block_offsets = (INT64*) malloc(sizeof(INT64)*internal_data.fuji_total_blocks);
//    merror(raw_block_offsets, "xtrans_load_raw()");

    raw_offset = sizeof(unsigned)*internal_data.fuji_total_blocks;
    if (raw_offset & 0xC)
        raw_offset +=  0x10 - (raw_offset & 0xC);

    raw_offset += internal_data.data_offset;

//    libraw_internal_data.internal_data.input->seek (libraw_internal_data.unpacker_data.data_offset, SEEK_SET);
//    libraw_internal_data.internal_data.input->read (block_sizes, 1, sizeof(unsigned)*libraw_internal_data.unpacker_data.fuji_total_blocks);


    uchar *data = (uchar *) block_sizes;
    for (int i = 0; i < sizeof(unsigned) * internal_data.fuji_total_blocks; i++) {
        data[i] = input->getByte();
    }


    raw_block_offsets[0] = raw_offset;
    // calculating raw block offsets
    for (cur_block = 0; cur_block < internal_data.fuji_total_blocks; cur_block++)
    {
        unsigned bsize = sgetn(4, (uchar *)(block_sizes+cur_block));
        block_sizes[cur_block] = bsize;
    }

    for (cur_block = 1; cur_block < internal_data.fuji_total_blocks; cur_block++)
        raw_block_offsets[cur_block] = raw_block_offsets[cur_block-1] + block_sizes[cur_block-1] ;

    xtrans_decode_loop(&common_info,internal_data.fuji_total_blocks,raw_block_offsets,block_sizes);

    free(block_sizes);
    free(raw_block_offsets);
    free(common_info.q_table);
}

void RafDecoder::xtrans_decode_loop(const struct xtrans_params* common_info, int count, INT64* raw_block_offsets, unsigned *block_sizes)
{
    int cur_block;
#ifdef LIBRAW_USE_OPENMP
#pragma omp parallel for private(cur_block)
#endif
    for (cur_block = 0; cur_block < count ; cur_block++)
    {
        xtrans_decode_strip(common_info, cur_block, raw_block_offsets[cur_block], block_sizes[cur_block]);
    }
}



bool RafDecoder::parse_xtrans_header(ByteStream *input)
{
    unsigned signature, version, h_raw_type, h_raw_bits,
        h_raw_height, h_raw_rounded_width, h_raw_width,
        h_block_size, h_blocks_in_row, h_total_lines;

    uchar header[16];

//    libraw_internal_data.internal_data.input->seek(libraw_internal_data.unpacker_data.data_offset,SEEK_SET);

    int offset = input->getOffset();

    for (int i = 0; i < 16; i++)
        header[i] = input->getByte();

//    libraw_internal_data.internal_data.input->read(header, 1, sizeof(header));

    // read all header
    signature = sgetn(2, header);
    version = header[2];
    h_raw_type = header[3];
    h_raw_bits = header[4];
    h_raw_height = sgetn(2, header+5);
    h_raw_rounded_width = sgetn(2, header+7);
    h_raw_width = sgetn(2, header+9);
    h_block_size = sgetn(2, header+11);
    h_blocks_in_row = header[13];
    h_total_lines = sgetn(2, header+14);

    // general validation
    if (signature != 0x4953
        || version != 1
        || h_raw_height > 0x3000
        || h_raw_height < 6
        || h_raw_height % 6
        || h_raw_width > 0x3000
        || h_raw_width < 0x300
        || h_raw_width % 24
        || h_raw_rounded_width > 0x3000
        || h_raw_rounded_width < h_block_size
        || h_raw_rounded_width % h_block_size
        || h_raw_rounded_width - h_raw_width >= h_block_size
        || h_block_size != 0x300
        || h_blocks_in_row > 0x10
        || h_blocks_in_row == 0
        || h_blocks_in_row != h_raw_rounded_width / h_block_size
        || h_total_lines > 0x800
        || h_total_lines == 0
        || h_total_lines != h_raw_height / 6
        || (h_raw_bits != 12 && h_raw_bits != 14)
        || h_raw_type != 16 && h_raw_type != 0) {

        return false;
    }
    // modify data
    internal_data.fuji_total_lines  = h_total_lines;
    internal_data.fuji_total_blocks = h_blocks_in_row;
    internal_data.fuji_block_width  = h_block_size;
    internal_data.fuji_bits         = h_raw_bits;
    internal_data.data_offset = offset + 16;
    internal_data.input = input;
    internal_data.fuji_raw_type = h_raw_type;

    raw_width         = h_raw_width;
    raw_height        = h_raw_height;

    return true;
}

#undef _abs
#undef _min

} // namespace RawSpeed

#ifdef NEWC
/* -*- C++ -*-
 * File: libraw_fuji_compressed.cpp
 * Copyright (C) 2016 Alexey Danilchenko
 *
 * Adopted to LibRaw by Alex Tutubalin, lexa@lexa.ru
 * LibRaw Fujifilm/compressed decoder
LibRaw is free software; you can redistribute it and/or modify
it under the terms of the one of two licenses as you choose:
1. GNU LESSER GENERAL PUBLIC LICENSE version 2.1
   (See file LICENSE.LGPL provided in LibRaw distribution archive for details).
2. COMMON DEVELOPMENT AND DISTRIBUTION LICENSE (CDDL) Version 1.0
   (See file LICENSE.CDDL provided in LibRaw distribution archive for details).
 */

// This file is #included in libraw_cxx

#ifdef _abs
#undef _abs
#undef _min
#undef _max
#endif
#define _abs(x) (((int)(x) ^ ((int)(x) >> 31)) - ((int)(x) >> 31))
#define _min(a, b) ((a) < (b) ? (a) : (b))
#define _max(a, b) ((a) > (b) ? (a) : (b))

struct int_pair
{
  int value1;
  int value2;
};

enum _xt_lines
{
  _R0 = 0,
  _R1,
  _R2,
  _R3,
  _R4,
  _G0,
  _G1,
  _G2,
  _G3,
  _G4,
  _G5,
  _G6,
  _G7,
  _B0,
  _B1,
  _B2,
  _B3,
  _B4,
  _ltotal
};

struct fuji_compressed_block
{
  int cur_bit;            // current bit being read (from left to right)
  int cur_pos;            // current position in a buffer
  INT64 cur_buf_offset;   // offset of this buffer in a file
  unsigned max_read_size; // Amount of data to be read
  int cur_buf_size;       // buffer size
  uchar *cur_buf;         // currently read block
  int fillbytes;          // Counter to add extra byte for block size N*16
  LibRaw_abstract_datastream *input;
  struct int_pair grad_even[3][41]; // tables of gradients
  struct int_pair grad_odd[3][41];
  ushort *linealloc;
  ushort *linebuf[_ltotal];
};

static unsigned sgetn(int n, uchar *s)
{
  unsigned result = 0;
  while (n-- > 0)
    result = (result << 8) | (*s++);
  return result;
}

void LibRaw::init_fuji_compr(struct fuji_compressed_params *info)
{
  int cur_val, i;
  char *qt;

  if ((libraw_internal_data.unpacker_data.fuji_block_width % 3 &&
       libraw_internal_data.unpacker_data.fuji_raw_type == 16) ||
      (libraw_internal_data.unpacker_data.fuji_block_width & 1 &&
       libraw_internal_data.unpacker_data.fuji_raw_type == 0))
    derror();

  info->q_table = (char *)malloc(32768);
  merror(info->q_table, "init_fuji_compr()");

  if (libraw_internal_data.unpacker_data.fuji_raw_type == 16)
    info->line_width = (libraw_internal_data.unpacker_data.fuji_block_width * 2) / 3;
  else
    info->line_width = libraw_internal_data.unpacker_data.fuji_block_width >> 1;

  info->q_point[0] = 0;
  info->q_point[1] = 0x12;
  info->q_point[2] = 0x43;
  info->q_point[3] = 0x114;
  info->q_point[4] = (1 << libraw_internal_data.unpacker_data.fuji_bits) - 1;
  info->min_value = 0x40;

  cur_val = -info->q_point[4];
  for (qt = info->q_table; cur_val <= info->q_point[4]; ++qt, ++cur_val)
  {
    if (cur_val <= -info->q_point[3])
      *qt = -4;
    else if (cur_val <= -info->q_point[2])
      *qt = -3;
    else if (cur_val <= -info->q_point[1])
      *qt = -2;
    else if (cur_val < 0)
      *qt = -1;
    else if (cur_val == 0)
      *qt = 0;
    else if (cur_val < info->q_point[1])
      *qt = 1;
    else if (cur_val < info->q_point[2])
      *qt = 2;
    else if (cur_val < info->q_point[3])
      *qt = 3;
    else
      *qt = 4;
  }

  // populting gradients
  if (info->q_point[4] == 0x3FFF)
  {
    info->total_values = 0x4000;
    info->raw_bits = 14;
    info->max_bits = 56;
    info->maxDiff = 256;
  }
  else if (info->q_point[4] == 0xFFF)
  {
    info->total_values = 4096;
    info->raw_bits = 12;
    info->max_bits = 48;
    info->maxDiff = 64;
  }
  else
    derror();
}

#define XTRANS_BUF_SIZE 0x10000

static inline void fuji_fill_buffer(struct fuji_compressed_block *info)
{
  if (info->cur_pos >= info->cur_buf_size)
  {
    info->cur_pos = 0;
    info->cur_buf_offset += info->cur_buf_size;
#ifdef LIBRAW_USE_OPENMP
#pragma omp critical
#endif
    {
#ifndef LIBRAW_USE_OPENMP
      info->input->lock();
#endif
      info->input->seek(info->cur_buf_offset, SEEK_SET);
      info->cur_buf_size = info->input->read(info->cur_buf, 1, _min(info->max_read_size, XTRANS_BUF_SIZE));
#ifndef LIBRAW_USE_OPENMP
      info->input->unlock();
#endif
      if (info->cur_buf_size < 1) // nothing read
      {
        if (info->fillbytes > 0)
        {
          int ls = _max(1, _min(info->fillbytes, XTRANS_BUF_SIZE));
          memset(info->cur_buf, 0, ls);
          info->fillbytes -= ls;
        }
        else
          throw LIBRAW_EXCEPTION_IO_EOF;
      }
      info->max_read_size -= info->cur_buf_size;
    }
  }
}

void LibRaw::init_fuji_block(struct fuji_compressed_block *info, const struct fuji_compressed_params *params,
                             INT64 raw_offset, unsigned dsize)
{
  info->linealloc = (ushort *)calloc(sizeof(ushort), _ltotal * (params->line_width + 2));
  merror(info->linealloc, "init_fuji_block()");

  INT64 fsize = libraw_internal_data.internal_data.input->size();
  info->max_read_size = _min(unsigned(fsize - raw_offset), dsize); // Data size may be incorrect?
  info->fillbytes = 1;

  info->input = libraw_internal_data.internal_data.input;
  info->linebuf[_R0] = info->linealloc;
  for (int i = _R1; i <= _B4; i++)
    info->linebuf[i] = info->linebuf[i - 1] + params->line_width + 2;

  // init buffer
  info->cur_buf = (uchar *)malloc(XTRANS_BUF_SIZE);
  merror(info->cur_buf, "init_fuji_block()");
  info->cur_bit = 0;
  info->cur_pos = 0;
  info->cur_buf_offset = raw_offset;
  for (int j = 0; j < 3; j++)
    for (int i = 0; i < 41; i++)
    {
      info->grad_even[j][i].value1 = params->maxDiff;
      info->grad_even[j][i].value2 = 1;
      info->grad_odd[j][i].value1 = params->maxDiff;
      info->grad_odd[j][i].value2 = 1;
    }

  info->cur_buf_size = 0;
  fuji_fill_buffer(info);
}

void LibRaw::copy_line_to_xtrans(struct fuji_compressed_block *info, int cur_line, int cur_block, int cur_block_width)
{
  ushort *lineBufB[3];
  ushort *lineBufG[6];
  ushort *lineBufR[3];
  unsigned pixel_count;
  ushort *line_buf;
  int index;

  int offset = libraw_internal_data.unpacker_data.fuji_block_width * cur_block + 6 * imgdata.sizes.raw_width * cur_line;
  ushort *raw_block_data = imgdata.rawdata.raw_image + offset;
  int row_count = 0;

  for (int i = 0; i < 3; i++)
  {
    lineBufR[i] = info->linebuf[_R2 + i] + 1;
    lineBufB[i] = info->linebuf[_B2 + i] + 1;
  }
  for (int i = 0; i < 6; i++)
    lineBufG[i] = info->linebuf[_G2 + i] + 1;

  while (row_count < 6)
  {
    pixel_count = 0;
    while (pixel_count < cur_block_width)
    {
      switch (imgdata.idata.xtrans_abs[row_count][(pixel_count % 6)])
      {
      case 0: // red
        line_buf = lineBufR[row_count >> 1];
        break;
      case 1:  // green
      default: // to make static analyzer happy
        line_buf = lineBufG[row_count];
        break;
      case 2: // blue
        line_buf = lineBufB[row_count >> 1];
        break;
      }

      index = (((pixel_count * 2 / 3) & 0x7FFFFFFE) | ((pixel_count % 3) & 1)) + ((pixel_count % 3) >> 1);
      raw_block_data[pixel_count] = line_buf[index];

      ++pixel_count;
    }
    ++row_count;
    raw_block_data += imgdata.sizes.raw_width;
  }
}

void LibRaw::copy_line_to_bayer(struct fuji_compressed_block *info, int cur_line, int cur_block, int cur_block_width)
{
  ushort *lineBufB[3];
  ushort *lineBufG[6];
  ushort *lineBufR[3];
  unsigned pixel_count;
  ushort *line_buf;

  int fuji_bayer[2][2];
  for (int r = 0; r < 2; r++)
    for (int c = 0; c < 2; c++)
      fuji_bayer[r][c] = FC(r, c); // We'll downgrade G2 to G below

  int offset = libraw_internal_data.unpacker_data.fuji_block_width * cur_block + 6 * imgdata.sizes.raw_width * cur_line;
  ushort *raw_block_data = imgdata.rawdata.raw_image + offset;
  int row_count = 0;

  for (int i = 0; i < 3; i++)
  {
    lineBufR[i] = info->linebuf[_R2 + i] + 1;
    lineBufB[i] = info->linebuf[_B2 + i] + 1;
  }
  for (int i = 0; i < 6; i++)
    lineBufG[i] = info->linebuf[_G2 + i] + 1;

  while (row_count < 6)
  {
    pixel_count = 0;
    while (pixel_count < cur_block_width)
    {
      switch (fuji_bayer[row_count & 1][pixel_count & 1])
      {
      case 0: // red
        line_buf = lineBufR[row_count >> 1];
        break;
      case 1:  // green
      case 3:  // second green
      default: // to make static analyzer happy
        line_buf = lineBufG[row_count];
        break;
      case 2: // blue
        line_buf = lineBufB[row_count >> 1];
        break;
      }

      raw_block_data[pixel_count] = line_buf[pixel_count >> 1];
      ++pixel_count;
    }
    ++row_count;
    raw_block_data += imgdata.sizes.raw_width;
  }
}

#define fuji_quant_gradient(i, v1, v2) (9 * i->q_table[i->q_point[4] + (v1)] + i->q_table[i->q_point[4] + (v2)])

static inline void fuji_zerobits(struct fuji_compressed_block *info, int *count)
{
  uchar zero = 0;
  *count = 0;
  while (zero == 0)
  {
    zero = (info->cur_buf[info->cur_pos] >> (7 - info->cur_bit)) & 1;
    info->cur_bit++;
    info->cur_bit &= 7;
    if (!info->cur_bit)
    {
      ++info->cur_pos;
      fuji_fill_buffer(info);
    }
    if (zero)
      break;
    ++*count;
  }
}

static inline void fuji_read_code(struct fuji_compressed_block *info, int *data, int bits_to_read)
{
  uchar bits_left = bits_to_read;
  uchar bits_left_in_byte = 8 - (info->cur_bit & 7);
  *data = 0;
  if (!bits_to_read)
    return;
  if (bits_to_read >= bits_left_in_byte)
  {
    do
    {
      *data <<= bits_left_in_byte;
      bits_left -= bits_left_in_byte;
      *data |= info->cur_buf[info->cur_pos] & ((1 << bits_left_in_byte) - 1);
      ++info->cur_pos;
      fuji_fill_buffer(info);
      bits_left_in_byte = 8;
    } while (bits_left >= 8);
  }
  if (!bits_left)
  {
    info->cur_bit = (8 - (bits_left_in_byte & 7)) & 7;
    return;
  }
  *data <<= bits_left;
  bits_left_in_byte -= bits_left;
  *data |= ((1 << bits_left) - 1) & ((unsigned)info->cur_buf[info->cur_pos] >> bits_left_in_byte);
  info->cur_bit = (8 - (bits_left_in_byte & 7)) & 7;
}

static inline int bitDiff(int value1, int value2)
{
  int decBits = 0;
  if (value2 < value1)
    while (decBits <= 12 && (value2 << ++decBits) < value1)
      ;
  return decBits;
}

static inline int fuji_decode_sample_even(struct fuji_compressed_block *info,
                                          const struct fuji_compressed_params *params, ushort *line_buf, int pos,
                                          struct int_pair *grads)
{
  int interp_val = 0;
  // ushort decBits;
  int errcnt = 0;

  int sample = 0, code = 0;
  ushort *line_buf_cur = line_buf + pos;
  int Rb = line_buf_cur[-2 - params->line_width];
  int Rc = line_buf_cur[-3 - params->line_width];
  int Rd = line_buf_cur[-1 - params->line_width];
  int Rf = line_buf_cur[-4 - 2 * params->line_width];

  int grad, gradient, diffRcRb, diffRfRb, diffRdRb;

  grad = fuji_quant_gradient(params, Rb - Rf, Rc - Rb);
  gradient = _abs(grad);
  diffRcRb = _abs(Rc - Rb);
  diffRfRb = _abs(Rf - Rb);
  diffRdRb = _abs(Rd - Rb);

  if (diffRcRb > diffRfRb && diffRcRb > diffRdRb)
    interp_val = Rf + Rd + 2 * Rb;
  else if (diffRdRb > diffRcRb && diffRdRb > diffRfRb)
    interp_val = Rf + Rc + 2 * Rb;
  else
    interp_val = Rd + Rc + 2 * Rb;

  fuji_zerobits(info, &sample);

  if (sample < params->max_bits - params->raw_bits - 1)
  {
    int decBits = bitDiff(grads[gradient].value1, grads[gradient].value2);
    fuji_read_code(info, &code, decBits);
    code += sample << decBits;
  }
  else
  {
    fuji_read_code(info, &code, params->raw_bits);
    code++;
  }

  if (code < 0 || code >= params->total_values)
    errcnt++;

  if (code & 1)
    code = -1 - code / 2;
  else
    code /= 2;

  grads[gradient].value1 += _abs(code);
  if (grads[gradient].value2 == params->min_value)
  {
    grads[gradient].value1 >>= 1;
    grads[gradient].value2 >>= 1;
  }
  grads[gradient].value2++;
  if (grad < 0)
    interp_val = (interp_val >> 2) - code;
  else
    interp_val = (interp_val >> 2) + code;
  if (interp_val < 0)
    interp_val += params->total_values;
  else if (interp_val > params->q_point[4])
    interp_val -= params->total_values;

  if (interp_val >= 0)
    line_buf_cur[0] = _min(interp_val, params->q_point[4]);
  else
    line_buf_cur[0] = 0;
  return errcnt;
}

static inline int fuji_decode_sample_odd(struct fuji_compressed_block *info,
                                         const struct fuji_compressed_params *params, ushort *line_buf, int pos,
                                         struct int_pair *grads)
{
  int interp_val = 0;
  int errcnt = 0;

  int sample = 0, code = 0;
  ushort *line_buf_cur = line_buf + pos;
  int Ra = line_buf_cur[-1];
  int Rb = line_buf_cur[-2 - params->line_width];
  int Rc = line_buf_cur[-3 - params->line_width];
  int Rd = line_buf_cur[-1 - params->line_width];
  int Rg = line_buf_cur[1];

  int grad, gradient;

  grad = fuji_quant_gradient(params, Rb - Rc, Rc - Ra);
  gradient = _abs(grad);

  if ((Rb > Rc && Rb > Rd) || (Rb < Rc && Rb < Rd))
    interp_val = (Rg + Ra + 2 * Rb) >> 2;
  else
    interp_val = (Ra + Rg) >> 1;

  fuji_zerobits(info, &sample);

  if (sample < params->max_bits - params->raw_bits - 1)
  {
    int decBits = bitDiff(grads[gradient].value1, grads[gradient].value2);
    fuji_read_code(info, &code, decBits);
    code += sample << decBits;
  }
  else
  {
    fuji_read_code(info, &code, params->raw_bits);
    code++;
  }

  if (code < 0 || code >= params->total_values)
    errcnt++;

  if (code & 1)
    code = -1 - code / 2;
  else
    code /= 2;

  grads[gradient].value1 += _abs(code);
  if (grads[gradient].value2 == params->min_value)
  {
    grads[gradient].value1 >>= 1;
    grads[gradient].value2 >>= 1;
  }
  grads[gradient].value2++;
  if (grad < 0)
    interp_val -= code;
  else
    interp_val += code;
  if (interp_val < 0)
    interp_val += params->total_values;
  else if (interp_val > params->q_point[4])
    interp_val -= params->total_values;

  if (interp_val >= 0)
    line_buf_cur[0] = _min(interp_val, params->q_point[4]);
  else
    line_buf_cur[0] = 0;
  return errcnt;
}

static void fuji_decode_interpolation_even(int line_width, ushort *line_buf, int pos)
{
  ushort *line_buf_cur = line_buf + pos;
  int Rb = line_buf_cur[-2 - line_width];
  int Rc = line_buf_cur[-3 - line_width];
  int Rd = line_buf_cur[-1 - line_width];
  int Rf = line_buf_cur[-4 - 2 * line_width];
  int diffRcRb = _abs(Rc - Rb);
  int diffRfRb = _abs(Rf - Rb);
  int diffRdRb = _abs(Rd - Rb);
  if (diffRcRb > diffRfRb && diffRcRb > diffRdRb)
    *line_buf_cur = (Rf + Rd + 2 * Rb) >> 2;
  else if (diffRdRb > diffRcRb && diffRdRb > diffRfRb)
    *line_buf_cur = (Rf + Rc + 2 * Rb) >> 2;
  else
    *line_buf_cur = (Rd + Rc + 2 * Rb) >> 2;
}

static void fuji_extend_generic(ushort *linebuf[_ltotal], int line_width, int start, int end)
{
  for (int i = start; i <= end; i++)
  {
    linebuf[i][0] = linebuf[i - 1][1];
    linebuf[i][line_width + 1] = linebuf[i - 1][line_width];
  }
}

static void fuji_extend_red(ushort *linebuf[_ltotal], int line_width)
{
  fuji_extend_generic(linebuf, line_width, _R2, _R4);
}

static void fuji_extend_green(ushort *linebuf[_ltotal], int line_width)
{
  fuji_extend_generic(linebuf, line_width, _G2, _G7);
}

static void fuji_extend_blue(ushort *linebuf[_ltotal], int line_width)
{
  fuji_extend_generic(linebuf, line_width, _B2, _B4);
}

void LibRaw::xtrans_decode_block(struct fuji_compressed_block *info, const struct fuji_compressed_params *params,
                                 int cur_line)
{
  int r_even_pos = 0, r_odd_pos = 1;
  int g_even_pos = 0, g_odd_pos = 1;
  int b_even_pos = 0, b_odd_pos = 1;

  int errcnt = 0;

  const int line_width = params->line_width;

  while (g_even_pos < line_width || g_odd_pos < line_width)
  {
    if (g_even_pos < line_width)
    {
      fuji_decode_interpolation_even(line_width, info->linebuf[_R2] + 1, r_even_pos);
      r_even_pos += 2;
      errcnt += fuji_decode_sample_even(info, params, info->linebuf[_G2] + 1, g_even_pos, info->grad_even[0]);
      g_even_pos += 2;
    }
    if (g_even_pos > 8)
    {
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_R2] + 1, r_odd_pos, info->grad_odd[0]);
      r_odd_pos += 2;
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_G2] + 1, g_odd_pos, info->grad_odd[0]);
      g_odd_pos += 2;
    }
  }

  fuji_extend_red(info->linebuf, line_width);
  fuji_extend_green(info->linebuf, line_width);

  g_even_pos = 0, g_odd_pos = 1;

  while (g_even_pos < line_width || g_odd_pos < line_width)
  {
    if (g_even_pos < line_width)
    {
      errcnt += fuji_decode_sample_even(info, params, info->linebuf[_G3] + 1, g_even_pos, info->grad_even[1]);
      g_even_pos += 2;
      fuji_decode_interpolation_even(line_width, info->linebuf[_B2] + 1, b_even_pos);
      b_even_pos += 2;
    }
    if (g_even_pos > 8)
    {
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_G3] + 1, g_odd_pos, info->grad_odd[1]);
      g_odd_pos += 2;
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_B2] + 1, b_odd_pos, info->grad_odd[1]);
      b_odd_pos += 2;
    }
  }

  fuji_extend_green(info->linebuf, line_width);
  fuji_extend_blue(info->linebuf, line_width);

  r_even_pos = 0, r_odd_pos = 1;
  g_even_pos = 0, g_odd_pos = 1;

  while (g_even_pos < line_width || g_odd_pos < line_width)
  {
    if (g_even_pos < line_width)
    {
      if (r_even_pos & 3)
        errcnt += fuji_decode_sample_even(info, params, info->linebuf[_R3] + 1, r_even_pos, info->grad_even[2]);
      else
        fuji_decode_interpolation_even(line_width, info->linebuf[_R3] + 1, r_even_pos);
      r_even_pos += 2;
      fuji_decode_interpolation_even(line_width, info->linebuf[_G4] + 1, g_even_pos);
      g_even_pos += 2;
    }
    if (g_even_pos > 8)
    {
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_R3] + 1, r_odd_pos, info->grad_odd[2]);
      r_odd_pos += 2;
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_G4] + 1, g_odd_pos, info->grad_odd[2]);
      g_odd_pos += 2;
    }
  }

  fuji_extend_red(info->linebuf, line_width);
  fuji_extend_green(info->linebuf, line_width);

  g_even_pos = 0, g_odd_pos = 1;
  b_even_pos = 0, b_odd_pos = 1;

  while (g_even_pos < line_width || g_odd_pos < line_width)
  {
    if (g_even_pos < line_width)
    {
      errcnt += fuji_decode_sample_even(info, params, info->linebuf[_G5] + 1, g_even_pos, info->grad_even[0]);
      g_even_pos += 2;
      if ((b_even_pos & 3) == 2)
        fuji_decode_interpolation_even(line_width, info->linebuf[_B3] + 1, b_even_pos);
      else
        errcnt += fuji_decode_sample_even(info, params, info->linebuf[_B3] + 1, b_even_pos, info->grad_even[0]);
      b_even_pos += 2;
    }
    if (g_even_pos > 8)
    {
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_G5] + 1, g_odd_pos, info->grad_odd[0]);
      g_odd_pos += 2;
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_B3] + 1, b_odd_pos, info->grad_odd[0]);
      b_odd_pos += 2;
    }
  }

  fuji_extend_green(info->linebuf, line_width);
  fuji_extend_blue(info->linebuf, line_width);

  r_even_pos = 0, r_odd_pos = 1;
  g_even_pos = 0, g_odd_pos = 1;

  while (g_even_pos < line_width || g_odd_pos < line_width)
  {
    if (g_even_pos < line_width)
    {
      if ((r_even_pos & 3) == 2)
        fuji_decode_interpolation_even(line_width, info->linebuf[_R4] + 1, r_even_pos);
      else
        errcnt += fuji_decode_sample_even(info, params, info->linebuf[_R4] + 1, r_even_pos, info->grad_even[1]);
      r_even_pos += 2;
      errcnt += fuji_decode_sample_even(info, params, info->linebuf[_G6] + 1, g_even_pos, info->grad_even[1]);
      g_even_pos += 2;
    }
    if (g_even_pos > 8)
    {
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_R4] + 1, r_odd_pos, info->grad_odd[1]);
      r_odd_pos += 2;
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_G6] + 1, g_odd_pos, info->grad_odd[1]);
      g_odd_pos += 2;
    }
  }

  fuji_extend_red(info->linebuf, line_width);
  fuji_extend_green(info->linebuf, line_width);

  g_even_pos = 0, g_odd_pos = 1;
  b_even_pos = 0, b_odd_pos = 1;

  while (g_even_pos < line_width || g_odd_pos < line_width)
  {
    if (g_even_pos < line_width)
    {
      fuji_decode_interpolation_even(line_width, info->linebuf[_G7] + 1, g_even_pos);
      g_even_pos += 2;
      if (b_even_pos & 3)
        errcnt += fuji_decode_sample_even(info, params, info->linebuf[_B4] + 1, b_even_pos, info->grad_even[2]);
      else
        fuji_decode_interpolation_even(line_width, info->linebuf[_B4] + 1, b_even_pos);
      b_even_pos += 2;
    }
    if (g_even_pos > 8)
    {
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_G7] + 1, g_odd_pos, info->grad_odd[2]);
      g_odd_pos += 2;
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_B4] + 1, b_odd_pos, info->grad_odd[2]);
      b_odd_pos += 2;
    }
  }

  fuji_extend_green(info->linebuf, line_width);
  fuji_extend_blue(info->linebuf, line_width);

  if (errcnt)
    derror();
}

void LibRaw::fuji_bayer_decode_block(struct fuji_compressed_block *info, const struct fuji_compressed_params *params,
                                     int cur_line)
{
  int r_even_pos = 0, r_odd_pos = 1;
  int g_even_pos = 0, g_odd_pos = 1;
  int b_even_pos = 0, b_odd_pos = 1;

  int errcnt = 0;

  const int line_width = params->line_width;

  while (g_even_pos < line_width || g_odd_pos < line_width)
  {
    if (g_even_pos < line_width)
    {
      errcnt += fuji_decode_sample_even(info, params, info->linebuf[_R2] + 1, r_even_pos, info->grad_even[0]);
      r_even_pos += 2;
      errcnt += fuji_decode_sample_even(info, params, info->linebuf[_G2] + 1, g_even_pos, info->grad_even[0]);
      g_even_pos += 2;
    }
    if (g_even_pos > 8)
    {
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_R2] + 1, r_odd_pos, info->grad_odd[0]);
      r_odd_pos += 2;
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_G2] + 1, g_odd_pos, info->grad_odd[0]);
      g_odd_pos += 2;
    }
  }

  fuji_extend_red(info->linebuf, line_width);
  fuji_extend_green(info->linebuf, line_width);

  g_even_pos = 0, g_odd_pos = 1;

  while (g_even_pos < line_width || g_odd_pos < line_width)
  {
    if (g_even_pos < line_width)
    {
      errcnt += fuji_decode_sample_even(info, params, info->linebuf[_G3] + 1, g_even_pos, info->grad_even[1]);
      g_even_pos += 2;
      errcnt += fuji_decode_sample_even(info, params, info->linebuf[_B2] + 1, b_even_pos, info->grad_even[1]);
      b_even_pos += 2;
    }
    if (g_even_pos > 8)
    {
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_G3] + 1, g_odd_pos, info->grad_odd[1]);
      g_odd_pos += 2;
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_B2] + 1, b_odd_pos, info->grad_odd[1]);
      b_odd_pos += 2;
    }
  }

  fuji_extend_green(info->linebuf, line_width);
  fuji_extend_blue(info->linebuf, line_width);

  r_even_pos = 0, r_odd_pos = 1;
  g_even_pos = 0, g_odd_pos = 1;

  while (g_even_pos < line_width || g_odd_pos < line_width)
  {
    if (g_even_pos < line_width)
    {
      errcnt += fuji_decode_sample_even(info, params, info->linebuf[_R3] + 1, r_even_pos, info->grad_even[2]);
      r_even_pos += 2;
      errcnt += fuji_decode_sample_even(info, params, info->linebuf[_G4] + 1, g_even_pos, info->grad_even[2]);
      g_even_pos += 2;
    }
    if (g_even_pos > 8)
    {
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_R3] + 1, r_odd_pos, info->grad_odd[2]);
      r_odd_pos += 2;
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_G4] + 1, g_odd_pos, info->grad_odd[2]);
      g_odd_pos += 2;
    }
  }

  fuji_extend_red(info->linebuf, line_width);
  fuji_extend_green(info->linebuf, line_width);

  g_even_pos = 0, g_odd_pos = 1;
  b_even_pos = 0, b_odd_pos = 1;

  while (g_even_pos < line_width || g_odd_pos < line_width)
  {
    if (g_even_pos < line_width)
    {
      errcnt += fuji_decode_sample_even(info, params, info->linebuf[_G5] + 1, g_even_pos, info->grad_even[0]);
      g_even_pos += 2;
      errcnt += fuji_decode_sample_even(info, params, info->linebuf[_B3] + 1, b_even_pos, info->grad_even[0]);
      b_even_pos += 2;
    }
    if (g_even_pos > 8)
    {
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_G5] + 1, g_odd_pos, info->grad_odd[0]);
      g_odd_pos += 2;
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_B3] + 1, b_odd_pos, info->grad_odd[0]);
      b_odd_pos += 2;
    }
  }

  fuji_extend_green(info->linebuf, line_width);
  fuji_extend_blue(info->linebuf, line_width);

  r_even_pos = 0, r_odd_pos = 1;
  g_even_pos = 0, g_odd_pos = 1;

  while (g_even_pos < line_width || g_odd_pos < line_width)
  {
    if (g_even_pos < line_width)
    {
      errcnt += fuji_decode_sample_even(info, params, info->linebuf[_R4] + 1, r_even_pos, info->grad_even[1]);
      r_even_pos += 2;
      errcnt += fuji_decode_sample_even(info, params, info->linebuf[_G6] + 1, g_even_pos, info->grad_even[1]);
      g_even_pos += 2;
    }
    if (g_even_pos > 8)
    {
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_R4] + 1, r_odd_pos, info->grad_odd[1]);
      r_odd_pos += 2;
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_G6] + 1, g_odd_pos, info->grad_odd[1]);
      g_odd_pos += 2;
    }
  }

  fuji_extend_red(info->linebuf, line_width);
  fuji_extend_green(info->linebuf, line_width);

  g_even_pos = 0, g_odd_pos = 1;
  b_even_pos = 0, b_odd_pos = 1;

  while (g_even_pos < line_width || g_odd_pos < line_width)
  {
    if (g_even_pos < line_width)
    {
      errcnt += fuji_decode_sample_even(info, params, info->linebuf[_G7] + 1, g_even_pos, info->grad_even[2]);
      g_even_pos += 2;
      errcnt += fuji_decode_sample_even(info, params, info->linebuf[_B4] + 1, b_even_pos, info->grad_even[2]);
      b_even_pos += 2;
    }
    if (g_even_pos > 8)
    {
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_G7] + 1, g_odd_pos, info->grad_odd[2]);
      g_odd_pos += 2;
      errcnt += fuji_decode_sample_odd(info, params, info->linebuf[_B4] + 1, b_odd_pos, info->grad_odd[2]);
      b_odd_pos += 2;
    }
  }

  fuji_extend_green(info->linebuf, line_width);
  fuji_extend_blue(info->linebuf, line_width);

  if (errcnt)
    derror();
}

void LibRaw::fuji_decode_strip(const struct fuji_compressed_params *info_common, int cur_block, INT64 raw_offset,
                               unsigned dsize)
{
  int cur_block_width, cur_line;
  unsigned line_size;
  struct fuji_compressed_block info;

  init_fuji_block(&info, info_common, raw_offset, dsize);
  line_size = sizeof(ushort) * (info_common->line_width + 2);

  cur_block_width = libraw_internal_data.unpacker_data.fuji_block_width;
  if (cur_block + 1 == libraw_internal_data.unpacker_data.fuji_total_blocks)
    cur_block_width = imgdata.sizes.raw_width % libraw_internal_data.unpacker_data.fuji_block_width;

  struct i_pair
  {
    int a, b;
  };
  const i_pair mtable[6] = {{_R0, _R3}, {_R1, _R4}, {_G0, _G6}, {_G1, _G7}, {_B0, _B3}, {_B1, _B4}},
               ztable[3] = {{_R2, 3}, {_G2, 6}, {_B2, 3}};
  for (cur_line = 0; cur_line < libraw_internal_data.unpacker_data.fuji_total_lines; cur_line++)
  {
    if (libraw_internal_data.unpacker_data.fuji_raw_type == 16)
      xtrans_decode_block(&info, info_common, cur_line);
    else
      fuji_bayer_decode_block(&info, info_common, cur_line);

    // copy data from line buffers and advance
    for (int i = 0; i < 6; i++)
      memcpy(info.linebuf[mtable[i].a], info.linebuf[mtable[i].b], line_size);

    if (libraw_internal_data.unpacker_data.fuji_raw_type == 16)
      copy_line_to_xtrans(&info, cur_line, cur_block, cur_block_width);
    else
      copy_line_to_bayer(&info, cur_line, cur_block, cur_block_width);

    for (int i = 0; i < 3; i++)
    {
      memset(info.linebuf[ztable[i].a], 0, ztable[i].b * line_size);
      info.linebuf[ztable[i].a][0] = info.linebuf[ztable[i].a - 1][1];
      info.linebuf[ztable[i].a][info_common->line_width + 1] = info.linebuf[ztable[i].a - 1][info_common->line_width];
    }
  }

  // release data
  free(info.linealloc);
  free(info.cur_buf);
}

void LibRaw::fuji_compressed_load_raw()
{
  struct fuji_compressed_params common_info;
  int cur_block;
  unsigned line_size, *block_sizes;
  INT64 raw_offset, *raw_block_offsets;
  // struct fuji_compressed_block info;

  init_fuji_compr(&common_info);
  line_size = sizeof(ushort) * (common_info.line_width + 2);

  // read block sizes
  block_sizes = (unsigned *)malloc(sizeof(unsigned) * libraw_internal_data.unpacker_data.fuji_total_blocks);
  merror(block_sizes, "fuji_compressed_load_raw()");
  raw_block_offsets = (INT64 *)malloc(sizeof(INT64) * libraw_internal_data.unpacker_data.fuji_total_blocks);
  merror(raw_block_offsets, "fuji_compressed_load_raw()");

  raw_offset = sizeof(unsigned) * libraw_internal_data.unpacker_data.fuji_total_blocks;
  if (raw_offset & 0xC)
    raw_offset += 0x10 - (raw_offset & 0xC);

  raw_offset += libraw_internal_data.unpacker_data.data_offset;

  libraw_internal_data.internal_data.input->seek(libraw_internal_data.unpacker_data.data_offset, SEEK_SET);
  libraw_internal_data.internal_data.input->read(
      block_sizes, 1, sizeof(unsigned) * libraw_internal_data.unpacker_data.fuji_total_blocks);

  raw_block_offsets[0] = raw_offset;
  // calculating raw block offsets
  for (cur_block = 0; cur_block < libraw_internal_data.unpacker_data.fuji_total_blocks; cur_block++)
  {
    unsigned bsize = sgetn(4, (uchar *)(block_sizes + cur_block));
    block_sizes[cur_block] = bsize;
  }

  for (cur_block = 1; cur_block < libraw_internal_data.unpacker_data.fuji_total_blocks; cur_block++)
    raw_block_offsets[cur_block] = raw_block_offsets[cur_block - 1] + block_sizes[cur_block - 1];

  fuji_decode_loop(&common_info, libraw_internal_data.unpacker_data.fuji_total_blocks, raw_block_offsets, block_sizes);

  free(block_sizes);
  free(raw_block_offsets);
  free(common_info.q_table);
}

void LibRaw::fuji_decode_loop(const struct fuji_compressed_params *common_info, int count, INT64 *raw_block_offsets,
                              unsigned *block_sizes)
{
  int cur_block;
#ifdef LIBRAW_USE_OPENMP
#pragma omp parallel for private(cur_block)
#endif
  for (cur_block = 0; cur_block < count; cur_block++)
  {
    fuji_decode_strip(common_info, cur_block, raw_block_offsets[cur_block], block_sizes[cur_block]);
  }
}

void LibRaw::parse_fuji_compressed_header()
{
  unsigned signature, version, h_raw_type, h_raw_bits, h_raw_height, h_raw_rounded_width, h_raw_width, h_block_size,
      h_blocks_in_row, h_total_lines;

  uchar header[16];

  libraw_internal_data.internal_data.input->seek(libraw_internal_data.unpacker_data.data_offset, SEEK_SET);
  libraw_internal_data.internal_data.input->read(header, 1, sizeof(header));

  // read all header
  signature = sgetn(2, header);
  version = header[2];
  h_raw_type = header[3];
  h_raw_bits = header[4];
  h_raw_height = sgetn(2, header + 5);
  h_raw_rounded_width = sgetn(2, header + 7);
  h_raw_width = sgetn(2, header + 9);
  h_block_size = sgetn(2, header + 11);
  h_blocks_in_row = header[13];
  h_total_lines = sgetn(2, header + 14);

  // general validation
  if (signature != 0x4953 || version != 1 || h_raw_height > 0x3000 || h_raw_height < 6 || h_raw_height % 6 ||
      h_raw_width > 0x3000 || h_raw_width < 0x300 || h_raw_width % 24 || h_raw_rounded_width > 0x3000 ||
      h_raw_rounded_width < h_block_size || h_raw_rounded_width % h_block_size ||
      h_raw_rounded_width - h_raw_width >= h_block_size || h_block_size != 0x300 || h_blocks_in_row > 0x10 ||
      h_blocks_in_row == 0 || h_blocks_in_row != h_raw_rounded_width / h_block_size || h_total_lines > 0x800 ||
      h_total_lines == 0 || h_total_lines != h_raw_height / 6 || (h_raw_bits != 12 && h_raw_bits != 14) ||
      (h_raw_type != 16 && h_raw_type != 0))
    return;


  // modify data
  libraw_internal_data.unpacker_data.fuji_total_lines = h_total_lines;
  libraw_internal_data.unpacker_data.fuji_total_blocks = h_blocks_in_row;
  libraw_internal_data.unpacker_data.fuji_block_width = h_block_size;
  libraw_internal_data.unpacker_data.fuji_bits = h_raw_bits;
  libraw_internal_data.unpacker_data.fuji_raw_type = h_raw_type;
  imgdata.sizes.raw_width = h_raw_width;
  imgdata.sizes.raw_height = h_raw_height;
  libraw_internal_data.unpacker_data.data_offset += 16;
  load_raw = &LibRaw::fuji_compressed_load_raw;
}

#undef _abs
#undef _min

#endif
