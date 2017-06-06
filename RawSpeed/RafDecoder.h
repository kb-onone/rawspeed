#ifndef RAF_DECODER_H
#define RAF_DECODER_H

#include "RawDecoder.h"
#include "TiffIFD.h"
#include "BitPumpPlain.h"
#include "TiffParser.h"
/*
    RawSpeed - RAW file decoder.

    Copyright (C) 2013 Klaus Post
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

#define INT64 uint64_t
#define uchar uint8_t
#define ushort unsigned short

struct int_pair {
    int value1;
    int value2;
};

enum _xt_lines
{
    _R0=0,_R1,_R2,_R3,_R4,
    _G0,_G1,_G2,_G3,_G4,_G5,_G6,_G7,
//    _G1,_G0,_G3,_G2,_G5,_G4,_G7,_G6,
    _B0,_B1,_B2,_B3,_B4,
    _ltotal
};



struct xtrans_params
  {
      char        *q_table;        /* quantization table */
      int         q_point[5];      /* quantization points */
      int         max_bits;
      int         min_value;
      int         raw_bits;
      int         total_values;
      int         maxDiff;
    ushort      line_width;
};

struct xtrans_block {
    int         cur_bit;         // current bit being read (from left to right)
    int         cur_pos;         // current position in a buffer
    INT64       cur_buf_offset;  // offset of this buffer in a file
    unsigned	max_read_size;	 // Amount of data to be read
    int         cur_buf_size;    // buffer size
    uchar       *cur_buf;        // currently read block
//    LibRaw_abstract_datastream *input;
    ByteStream *input;
    struct int_pair grad_even[3][41];    // tables of gradients
    struct int_pair grad_odd[3][41];
    ushort		*linealloc;
    ushort      *linebuf[_ltotal];
};

struct InternalCompressedFuji {
    unsigned fuji_total_lines;
    unsigned fuji_total_blocks;
    unsigned fuji_block_width;
    unsigned fuji_bits;
    unsigned fuji_raw_type;
    int data_offset;
    ByteStream *input;
};


class RafDecoder :
  public RawDecoder
{
    InternalCompressedFuji internal_data;
    int raw_width, raw_height;

public:
  RafDecoder(TiffIFD *rootIFD, FileMap* file);
  virtual ~RafDecoder(void);
  RawImage decodeRawInternal();
  virtual void decodeMetaDataInternal(CameraMetaData *meta);
  virtual void checkSupportInternal(CameraMetaData *meta);
  TiffIFD *mRootIFD;
  virtual TiffIFD* getRootIFD() {return mRootIFD;}
protected:
  virtual void decodeThreaded(RawDecoderThread* t);
  void DecodeRaf();
  bool alt_layout;

  bool parse_xtrans_header(ByteStream *);
  void xtrans_compressed_load_raw(ByteStream *);
  void xtrans_decode_loop(const struct xtrans_params* common_info, int count, INT64* raw_block_offsets, unsigned *block_sizes);
  void init_xtrans(struct xtrans_params* info);
  void xtrans_decode_strip(const struct xtrans_params* info_common, int cur_block, INT64 raw_offset, unsigned dsize);
  void xtrans_decode_block(struct xtrans_block* info, const struct xtrans_params *params, int cur_line);
  void fuji_bayer_decode_block(struct xtrans_block *info, const struct xtrans_params *params, int cur_line);

  void init_xtrans_block(struct xtrans_block* info, const struct xtrans_params *params, INT64 raw_offset, unsigned dsize);
  void copy_line_to_xtrans(struct xtrans_block* info, int cur_line, int cur_block, int cur_block_width);
  void copy_line_to_bayer(struct xtrans_block *info, int cur_line, int cur_block, int cur_block_width);

  string getMode();

};

} // namespace RawSpeed

#endif
