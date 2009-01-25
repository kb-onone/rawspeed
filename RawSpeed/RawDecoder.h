#pragma once
#include "RawDecoderException.h"
#include "FileMap.h"
#include "BitPumpJPEG.h" // Includes bytestream
#include "RawImage.h"
#include "BitPumpMSB.h"
#include "BitPumpPlain.h"

class RawDecoder 
{
public:
  RawDecoder(FileMap* file);
  virtual ~RawDecoder(void);
  virtual RawImage decodeRaw() = 0;
  FileMap *mFile; 
  void readUncompressedRaw(ByteStream &input, iPoint2D& size, iPoint2D& offset, int inputPitch, int bitPerPixel, gboolean MSBOrder);
  RawImage mRaw; 
  vector<const char*> errors;
protected:
  void Decode12BitRaw(ByteStream &input, guint w, guint h);
};

