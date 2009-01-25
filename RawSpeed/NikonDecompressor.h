#pragma once
#include "ljpegdecompressor.h"
#include "BitPumpMSB.h"

class NikonDecompressor :
  public LJpegDecompressor
{
public:
  NikonDecompressor(FileMap* file, RawImage img );
public:
  virtual ~NikonDecompressor(void);
  void DecompressNikon(ByteStream &meta, guint w, guint h, guint bitsPS, guint offset, guint size);
private:
  void initTable(guint huffSelect);
  gint HuffDecodeNikon();
  guint curve[0xffff];
  BitPumpMSB *bits;
};

static const guchar nikon_tree[][32] = {
  { 0,1,5,1,1,1,1,1,1,2,0,0,0,0,0,0,	/* 12-bit lossy */
  5,4,3,6,2,7,1,0,8,9,11,10,12 },
  { 0,1,5,1,1,1,1,1,1,2,0,0,0,0,0,0,	/* 12-bit lossy after split */
  0x39,0x5a,0x38,0x27,0x16,5,4,3,2,1,0,11,12,12 },
  { 0,1,4,2,3,1,2,0,0,0,0,0,0,0,0,0,  /* 12-bit lossless */
  5,4,6,3,7,2,8,1,9,0,10,11,12 },
  { 0,1,4,3,1,1,1,1,1,2,0,0,0,0,0,0,	/* 14-bit lossy */
  5,6,4,7,8,3,9,2,1,0,10,11,12,13,14 },
  { 0,1,5,1,1,1,1,1,1,1,2,0,0,0,0,0,	/* 14-bit lossy after split */
  8,0x5c,0x4b,0x3a,0x29,7,6,5,4,3,2,1,0,13,14 },
  { 0,1,4,2,2,3,1,2,0,0,0,0,0,0,0,0,	/* 14-bit lossless */
  7,6,8,5,9,4,10,3,11,12,2,0,1,13,14 } };