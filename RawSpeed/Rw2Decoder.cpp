#include "StdAfx.h"
#include "Rw2Decoder.h"

/*
    RawSpeed - RAW file decoder.

    Copyright (C) 2009-2014 Klaus Post
    Copyright (C) 2014 Pedro Côrte-Real

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

Rw2Decoder::Rw2Decoder(TiffIFD *rootIFD, FileMap* file) :
    RawDecoder(file), mRootIFD(rootIFD), input_start(0) {
      decoderVersion = 2;
}
Rw2Decoder::~Rw2Decoder(void) {
  if (input_start)
    delete input_start;
  input_start = 0;
  if (mRootIFD)
    delete mRootIFD;
  mRootIFD = NULL;
}

RawImage Rw2Decoder::decodeRawInternal() {

  vector<TiffIFD*> data = mRootIFD->getIFDsWithTag(PANASONIC_STRIPOFFSET);

  bool isOldPanasonic = FALSE;

  if (data.empty()) {
    if (!mRootIFD->hasEntryRecursive(STRIPOFFSETS))
      ThrowRDE("RW2 Decoder: No image data found");
    isOldPanasonic = TRUE;
    data = mRootIFD->getIFDsWithTag(STRIPOFFSETS);
  }

  TiffIFD* raw = data[0];
  uint32 height = raw->getEntry((TiffTag)3)->getShort();
  uint32 width = raw->getEntry((TiffTag)2)->getShort();

  if (isOldPanasonic) {
    TiffEntry *offsets = raw->getEntry(STRIPOFFSETS);

    if (offsets->count != 1) {
      ThrowRDE("RW2 Decoder: Multiple Strips found: %u", offsets->count);
    }
    int off = offsets->getInt();
    if (!mFile->isValid(off))
      ThrowRDE("Panasonic RAW Decoder: Invalid image data offset, cannot decode.");

    mRaw->dim = iPoint2D(width, height);
    mRaw->createData();

    uint32 size = mFile->getSize() - off;
    input_start = new ByteStream(mFile, off);

    if (size >= width*height*2) {
      // It's completely unpacked little-endian
      Decode12BitRawUnpacked(*input_start, width, height);
    } else if (size >= width*height*3/2) {
      // It's a packed format
      Decode12BitRawWithControl(*input_start, width, height);
    } else {
      // It's using the new .RW2 decoding method
      load_flags = 0;
      DecodeRw2();
    }
  } else {

    mRaw->dim = iPoint2D(width, height);
    mRaw->createData();
    TiffEntry *offsets = raw->getEntry(PANASONIC_STRIPOFFSET);

    if (offsets->count != 1) {
      ThrowRDE("RW2 Decoder: Multiple Strips found: %u", offsets->count);
    }

    load_flags = 0x2008;
    int off = offsets->getInt();

    if (!mFile->isValid(off))
      ThrowRDE("RW2 Decoder: Invalid image data offset, cannot decode.");

    input_start = new ByteStream(mFile, off);


    if (hints.find("packed14") != hints.end())
        decode14bitPacked(*input_start, width, height);
    else
        DecodeRw2();


  }
  // Read blacklevels
  if (raw->hasEntry((TiffTag)0x1c) && raw->hasEntry((TiffTag)0x1d) && raw->hasEntry((TiffTag)0x1e)) {
    mRaw->blackLevelSeparate[0] = raw->getEntry((TiffTag)0x1c)->getInt() + 15;
    mRaw->blackLevelSeparate[1] = mRaw->blackLevelSeparate[2] = raw->getEntry((TiffTag)0x1d)->getInt() + 15;
    mRaw->blackLevelSeparate[3] = raw->getEntry((TiffTag)0x1e)->getInt() + 15;
  }

  // Read WB levels
  if (raw->hasEntry((TiffTag)0x0024) && raw->hasEntry((TiffTag)0x0025) && raw->hasEntry((TiffTag)0x0026)) {
    mRaw->metadata.wbCoeffs[0] = (float) raw->getEntry((TiffTag)0x0024)->getShort();
    mRaw->metadata.wbCoeffs[1] = (float) raw->getEntry((TiffTag)0x0025)->getShort();
    mRaw->metadata.wbCoeffs[2] = (float) raw->getEntry((TiffTag)0x0026)->getShort();
  } else if (raw->hasEntry((TiffTag)0x0011) && raw->hasEntry((TiffTag)0x0012)) {
    mRaw->metadata.wbCoeffs[0] = (float) raw->getEntry((TiffTag)0x0011)->getShort();
    mRaw->metadata.wbCoeffs[1] = 256.0f;
    mRaw->metadata.wbCoeffs[2] = (float) raw->getEntry((TiffTag)0x0012)->getShort();
  }

  return mRaw;
}

void Rw2Decoder::DecodeRw2() {
  startThreads();
}


/*
int row, col, i, j, sh = 0, pred[2], nonz[2];
unsigned bytes[16];
ushort *raw_block_data;
int enc_blck_size = pana_bpp == 12 ? 10 : 9;

pana_data(0, 0);
if (pana_encoding == 5)
{
  for (row = 0; row < raw_height; row++)
  {
    raw_block_data = raw_image + row * raw_width;

#ifdef LIBRAW_LIBRARY_BUILD
    checkCancel();
#endif
    for (col = 0; col < raw_width; col += enc_blck_size)
    {
      pana_data(0, bytes);

      if (pana_bpp == 12)
      {
        raw_block_data[col] = ((bytes[1] & 0xF) << 8) + bytes[0];
        raw_block_data[col + 1] = 16 * bytes[2] + (bytes[1] >> 4);
        raw_block_data[col + 2] = ((bytes[4] & 0xF) << 8) + bytes[3];
        raw_block_data[col + 3] = 16 * bytes[5] + (bytes[4] >> 4);
        raw_block_data[col + 4] = ((bytes[7] & 0xF) << 8) + bytes[6];
        raw_block_data[col + 5] = 16 * bytes[8] + (bytes[7] >> 4);
        raw_block_data[col + 6] = ((bytes[10] & 0xF) << 8) + bytes[9];
        raw_block_data[col + 7] = 16 * bytes[11] + (bytes[10] >> 4);
        raw_block_data[col + 8] = ((bytes[13] & 0xF) << 8) + bytes[12];
        raw_block_data[col + 9] = 16 * bytes[14] + (bytes[13] >> 4);






      }
      else if (pana_bpp == 14)
      {
        raw_block_data[col] = bytes[0] + ((bytes[1] & 0x3F) << 8);
        raw_block_data[col + 1] = (bytes[1] >> 6) + 4 * (bytes[2]) +
                                  ((bytes[3] & 0xF) << 10);
        raw_block_data[col + 2] = (bytes[3] >> 4) + 16 * (bytes[4]) +
                                  ((bytes[5] & 3) << 12);
        raw_block_data[col + 3] = ((bytes[5] & 0xFC) >> 2) + (bytes[6] << 6);
        raw_block_data[col + 4] = bytes[7] + ((bytes[8] & 0x3F) << 8);
        raw_block_data[col + 5] = (bytes[8] >> 6) + 4 * bytes[9] + ((bytes[10] & 0xF) << 10);
        raw_block_data[col + 6] = (bytes[10] >> 4) + 16 * bytes[11] + ((bytes[12] & 3) << 12);
        raw_block_data[col + 7] = ((bytes[12] & 0xFC) >> 2) + (bytes[13] << 6);
        raw_block_data[col + 8] = bytes[14] + ((bytes[15] & 0x3F) << 8);
      }
    }
  }
}
else
{
  for (row = 0; row < raw_height; row++)
  {
#ifdef LIBRAW_LIBRARY_BUILD
    checkCancel();
#endif
    for (col = 0; col < raw_width; col++)
    {
      if ((i = col % 14) == 0)
        pred[0] = pred[1] = nonz[0] = nonz[1] = 0;
      if (i % 3 == 2)
        sh = 4 >> (3 - pana_data(2, 0));
      if (nonz[i & 1])
      {
        if ((j = pana_data(8, 0)))
        {
          if ((pred[i & 1] -= 0x80 << sh) < 0 || sh == 4)
            pred[i & 1] &= ~((~0u) << sh);
          pred[i & 1] += j << sh;
        }
      }
      else if ((nonz[i & 1] = pana_data(8, 0)) || i > 11)
        pred[i & 1] = nonz[i & 1] << 4 | pana_data(4, 0);
      if ((RAW(row, col) = pred[col & 1]) > 4098 && col < width && row < height)
        derror();
    }
  }
}
}
*/


void Rw2Decoder::decodeThreaded(RawDecoderThread * t) {
  int x, i, j, sh = 0, pred[2], nonz[2];
  int w = mRaw->dim.x / 14;
  uint32 y;

  bool zero_is_bad = true;
  if (hints.find("zero_is_not_bad") != hints.end())
    zero_is_bad = false;

  /* 9 + 1/7 bits per pixel */
  int skip = w * 14 * t->start_y * 9;
  skip += w * 2 * t->start_y;
  skip /= 8;

  PanaBitpump bits(new ByteStream(input_start));
  bits.load_flags = load_flags;
  bits.skipBytes(skip);

  vector<uint32> zero_pos;
  for (y = t->start_y; y < t->end_y; y++) {
	  
    if ( mCancelDecoder && *mCancelDecoder )
        break;
	  
    ushort16* dest = (ushort16*)mRaw->getData(0, y);
    for (x = 0; x < w; x++) {
      pred[0] = pred[1] = nonz[0] = nonz[1] = 0;
      int u = 0;
      for (i = 0; i < 14; i++) {
        // Even pixels
        if (u == 2)
        {
          sh = 4 >> (3 - bits.getBits(2));
          u = -1;
        }
        if (nonz[0]) {
          if ((j = bits.getBits(8))) {
            if ((pred[0] -= 0x80 << sh) < 0 || sh == 4)
              pred[0] &= ~(-1 << sh);
            pred[0] += j << sh;
          }
        } else if ((nonz[0] = bits.getBits(8)) || i > 11)
          pred[0] = nonz[0] << 4 | bits.getBits(4);
        *dest++ = pred[0];
        if (zero_is_bad && 0 == pred[0])
          zero_pos.push_back((y<<16) | (x*14+i));

        // Odd pixels
        i++;
        u++;
        if (u == 2)
        {
          sh = 4 >> (3 - bits.getBits(2));
          u = -1;
        }
        if (nonz[1]) {
          if ((j = bits.getBits(8))) {
            if ((pred[1] -= 0x80 << sh) < 0 || sh == 4)
              pred[1] &= ~(-1 << sh);
            pred[1] += j << sh;
          }
        } else if ((nonz[1] = bits.getBits(8)) || i > 11)
          pred[1] = nonz[1] << 4 | bits.getBits(4);
        *dest++ = pred[1];
        if (zero_is_bad && 0 == pred[1])
          zero_pos.push_back((y<<16) | (x*14+i));
        u++;
      }
    }
  }
  if (zero_is_bad && !zero_pos.empty()) {
    pthread_mutex_lock(&mRaw->mBadPixelMutex);
    mRaw->mBadPixelPositions.insert(mRaw->mBadPixelPositions.end(), zero_pos.begin(), zero_pos.end());
    pthread_mutex_unlock(&mRaw->mBadPixelMutex);
  }
}

void Rw2Decoder::checkSupportInternal(CameraMetaData *meta) {
  vector<TiffIFD*> data = mRootIFD->getIFDsWithTag(MODEL);
  if (data.empty())
    ThrowRDE("RW2 Support check: Model name found");

  string make = data[0]->getEntry(MAKE)->getString();
  string model = data[0]->getEntry(MODEL)->getString();
  if (!this->checkCameraSupported(meta, make, model, guessMode()))
    this->checkCameraSupported(meta, make, model, "");
}

void Rw2Decoder::decodeMetaDataInternal(CameraMetaData *meta) {
  mRaw->cfa.setCFA(iPoint2D(2,2), CFA_BLUE, CFA_GREEN, CFA_GREEN2, CFA_RED);
  vector<TiffIFD*> data = mRootIFD->getIFDsWithTag(MODEL);

  if (data.empty())
    ThrowRDE("RW2 Meta Decoder: Model name not found");
  if (!data[0]->hasEntry(MAKE))
    ThrowRDE("RW2 Support: Make name not found");

  string make = data[0]->getEntry(MAKE)->getString();
  string model = data[0]->getEntry(MODEL)->getString();
  string mode = guessMode();
  int iso = 0;
  if (mRootIFD->hasEntryRecursive(PANASONIC_ISO_SPEED))
    iso = mRootIFD->getEntryRecursive(PANASONIC_ISO_SPEED)->getInt();

  if (this->checkCameraSupported(meta, make, model, mode)) {
    setMetaData(meta, make, model, mode, iso);
  } else {
    mRaw->metadata.mode = mode;
    _RPT1(0, "Mode not found in DB: %s", mode.c_str());
    setMetaData(meta, make, model, "", iso);
  }
}


void Rw2Decoder::decode14bitPacked(ByteStream stream, int raw_width, int raw_height)
{
    unsigned bytes[16];
    ushort16 *raw_block_data;
    int enc_blck_size = 9;

    PanaBitpump bits(new ByteStream(input_start));
    bits.load_flags = load_flags;
    bits.bitPacked = true;

    for (int row = 0; row < raw_height; row++) {

      raw_block_data = (ushort16*)mRaw->getData(0, 0) + row * raw_width;

      for (int col = 0; col < raw_width; col += enc_blck_size) {

          for (int i = 0; i < 16; i++)
            bytes[i] = bits.getBits(8);

          raw_block_data[col] =   bytes[0] + ((bytes[1] & 0x3F) << 8);
          raw_block_data[col + 1] = (bytes[1] >> 6) + 4 * (bytes[2]) +
                                     ((bytes[3] & 0xF) << 10);
          raw_block_data[col + 2] = (bytes[3] >> 4) + 16 * (bytes[4]) +
                                ((bytes[5] & 3) << 12);
          raw_block_data[col + 3] = ((bytes[5] & 0xFC) >> 2) + (bytes[6] << 6);
          raw_block_data[col + 4] = bytes[7] + ((bytes[8] & 0x3F) << 8);
          raw_block_data[col + 5] = (bytes[8] >> 6) + 4 * bytes[9] + ((bytes[10] & 0xF) << 10);
          raw_block_data[col + 6] = (bytes[10] >> 4) + 16 * bytes[11] + ((bytes[12] & 3) << 12);
          raw_block_data[col + 7] = ((bytes[12] & 0xFC) >> 2) + (bytes[13] << 6);
          raw_block_data[col + 8] = bytes[14] + ((bytes[15] & 0x3F) << 8);
        }

    }

}

std::string Rw2Decoder::guessMode() {
  float ratio = 3.0f / 2.0f;  // Default

  if (!mRaw->isAllocated())
    return "";

  ratio = (float)mRaw->dim.x / (float)mRaw->dim.y;

  float min_diff = fabs(ratio - 16.0f / 9.0f);
  std::string closest_match = "16:9";

  float t = fabs(ratio - 3.0f / 2.0f);
  if (t < min_diff) {
    closest_match = "3:2";
    min_diff  = t;
  }

  t = fabs(ratio - 4.0f / 3.0f);
  if (t < min_diff) {
    closest_match =  "4:3";
    min_diff  = t;
  }

  t = fabs(ratio - 1.0f);
  if (t < min_diff) {
    closest_match = "1:1";
    min_diff  = t;
  }
  _RPT1(0, "Mode guess: '%s'\n", closest_match.c_str());
  return closest_match;
}

PanaBitpump::PanaBitpump(ByteStream* _input) : input(_input), vbits(0) {

    c = 0;
    bitPacked = false;
}

PanaBitpump::~PanaBitpump() {
  if (input)
    delete input;
  input = 0;
}

/*
 *
 *    for (byte = 0; byte < 16; byte++)
    {
      bytes[byte] = buf[vpos++];
      vpos &= 0x3FFF;
    }
    */

void PanaBitpump::skipBytes(int bytes) {
  int blocks = (bytes / 0x4000) * 0x4000;
  input->skipBytes(blocks);
  for (int i = blocks; i < bytes; i++)
    getBits(8);
}

uint32 PanaBitpump::getBits(int nbits) {
  int byte;

  if (!vbits) {
    /* On truncated files this routine will just return just for the truncated
    * part of the file. Since there is no chance of affecting output buffer
    * size we allow the decoder to decode this
    */
    if (input->getRemainSize() < 0x4000 - load_flags) {
      memcpy(buf + load_flags, input->getData(), input->getRemainSize());
      input->skipBytes(input->getRemainSize());
    } else {
      memcpy(buf + load_flags, input->getData(), 0x4000 - load_flags);
      input->skipBytes(0x4000 - load_flags);
      if (input->getRemainSize() < load_flags) {
        memcpy(buf, input->getData(), input->getRemainSize());
        input->skipBytes(input->getRemainSize());
      } else {
        memcpy(buf, input->getData(), load_flags);
        input->skipBytes(load_flags);
      }
    }
    c = 0;
  }

  if ( bitPacked ) {
      vbits = (vbits - 8) & 0x1ffff;
      uint32 t = buf[c];
      c++;
      c &= 0x3FFF;

      return t;

  } else {
      vbits = (vbits - nbits) & 0x1ffff;
      byte = vbits >> 3 ^ 0x3ff0;
      return (buf[byte] | buf[byte+1] << 8) >> (vbits & 7) & ~(-1 << nbits);

  }
}

} // namespace RawSpeed
