///////////////////////////////////////////////////////////////////////////////
// Name:        pdfencrypt.cpp
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2005-08-17
// RCS-ID:      $$
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence + RSA Data Security license
///////////////////////////////////////////////////////////////////////////////

/// \file pdfencrypt.cpp Implementation of the wxPdfEncrypt class

/*
 **********************************************************************
 ** Copyright (C) 1990, RSA Data Security, Inc. All rights reserved. **
 **                                                                  **
 ** License to copy and use this software is granted provided that   **
 ** it is identified as the "RSA Data Security, Inc. MD5 Message     **
 ** Digest Algorithm" in all material mentioning or referencing this **
 ** software or this function.                                       **
 **                                                                  **
 ** License is also granted to make and use derivative works         **
 ** provided that such works are identified as "derived from the RSA **
 ** Data Security, Inc. MD5 Message Digest Algorithm" in all         **
 ** material mentioning or referencing the derived work.             **
 **                                                                  **
 ** RSA Data Security, Inc. makes no representations concerning      **
 ** either the merchantability of this software or the suitability   **
 ** of this software for any particular purpose.  It is provided "as **
 ** is" without express or implied warranty of any kind.             **
 **                                                                  **
 ** These notices must be retained in any copies of any part of this **
 ** documentation and/or software.                                   **
 **********************************************************************
 */

// For compilers that support precompilation, includes <wx.h>.
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/wx.h>
#endif

// includes
#include <wx/log.h>

#include "wex/pdf/pdfencrypt.h"
#include "wex/pdf/pdfrijndael.h"
#include "wex/pdf/pdfutility.h"

// ----------------
// MD5 by RSA
// ----------------

// C headers for MD5
#include <sys/types.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#define MD5_HASHBYTES 16

/// Structure representing an MD5 context while ecrypting. (For internal use only)
typedef struct MD5Context
{
  unsigned int buf[4];
  unsigned int bits[2];
  unsigned char in[64];
} MD5_CTX;

static void  MD5Init(MD5_CTX *context);
static void  MD5Update(MD5_CTX *context, unsigned char const *buf, unsigned len);
static void  MD5Final(unsigned char digest[MD5_HASHBYTES], MD5_CTX *context);
static void  MD5Transform(unsigned int buf[4], unsigned int const in[16]);

#ifndef WORDS_BIGENDIAN
#define byteReverse(buf,len)   /* Nothing */
#else
static void byteReverse(unsigned char *buf, unsigned longs);

/*
 * Note: this code is harmless on little-endian machines.
 */
static void byteReverse(unsigned char *buf, unsigned longs)
{
  static int littleEndian = -1;
  if (littleEndian < 0)
  {
    // Are we little or big endian? This method is from Harbison & Steele.
    union
    {
      long l;
      char c[sizeof(long)];
    } u;
    u.l = 1;
    littleEndian = (u.c[0] == 1) ? 1 : 0;
  }

  if (littleEndian != 1)
  {
    unsigned int t;
    do
    {
      t = (unsigned int) ((unsigned) buf[3] << 8 | buf[2]) << 16 |
          ((unsigned) buf[1] << 8 | buf[0]);
      *(unsigned int *) buf = t;  
      buf += 4;
    }
    while (--longs);
  }
}
#endif

#if 0
static char* MD5End(MD5_CTX *, char *);

static char* MD5End(MD5_CTX *ctx, char *buf)
{
  int i;
  unsigned char digest[MD5_HASHBYTES];
  char hex[]="0123456789abcdef";

  if (!buf)
  {
    buf = (char *)malloc(33);
  }
    
  if (!buf)
  {
    return 0;
  }
    
  MD5Final(digest,ctx);
  for (i=0;i<MD5_HASHBYTES;i++)
  {
    buf[i+i] = hex[digest[i] >> 4];
    buf[i+i+1] = hex[digest[i] & 0x0f];
  }
  buf[i+i] = '\0';
  return buf;
}
#endif

/*
 * Final wrapup - pad to 64-byte boundary with the bit pattern
 * 1 0* (64-bit count of bits processed, MSB-first)
 */
static void MD5Final(unsigned char digest[16], MD5_CTX *ctx)
{
  unsigned count;
  unsigned char *p;

  /* Compute number of bytes mod 64 */
  count = (ctx->bits[0] >> 3) & 0x3F; 

  /* Set the first char of padding to 0x80.  This is safe since there is
     always at least one byte free */
  p = ctx->in + count;
  *p++ = 0x80;

  /* Bytes of padding needed to make 64 bytes */
  count = 64 - 1 - count;

  /* Pad out to 56 mod 64 */
  if (count < 8)
  {
    /* Two lots of padding:  Pad the first block to 64 bytes */
    memset(p, 0, count);
    byteReverse(ctx->in, 16);
    MD5Transform(ctx->buf, (unsigned int *) ctx->in);

    /* Now fill the next block with 56 bytes */
    memset(ctx->in, 0, 56);
  }
  else
  {
    /* Pad block to 56 bytes */
    memset(p, 0, count - 8);   
  }
  byteReverse(ctx->in, 14);

  /* Append length in bits and transform */
  ((unsigned int *) ctx->in)[14] = ctx->bits[0];
  ((unsigned int *) ctx->in)[15] = ctx->bits[1];

  MD5Transform(ctx->buf, (unsigned int *) ctx->in);
  byteReverse((unsigned char *) ctx->buf, 4);
  memcpy(digest, ctx->buf, 16);
  memset((char *) ctx, 0, sizeof(ctx));       /* In case it's sensitive */
}

static void MD5Init(MD5_CTX *ctx)
{
  ctx->buf[0] = 0x67452301;
  ctx->buf[1] = 0xefcdab89;
  ctx->buf[2] = 0x98badcfe;
  ctx->buf[3] = 0x10325476;

  ctx->bits[0] = 0;
  ctx->bits[1] = 0;
}

static void MD5Update(MD5_CTX *ctx, unsigned char const *buf, unsigned len)
{
  unsigned int t;

  /* Update bitcount */

  t = ctx->bits[0];
  if ((ctx->bits[0] = t + ((unsigned int) len << 3)) < t)
  {
        ctx->bits[1]++;         /* Carry from low to high */
  }
  ctx->bits[1] += len >> 29;

  t = (t >> 3) & 0x3f;        /* Bytes already in shsInfo->data */

  /* Handle any leading odd-sized chunks */

  if (t)
  {
    unsigned char *p = (unsigned char *) ctx->in + t;

    t = 64 - t;
    if (len < t)
    {
      memcpy(p, buf, len);
      return;
    }
    memcpy(p, buf, t);
    byteReverse(ctx->in, 16);
    MD5Transform(ctx->buf, (unsigned int *) ctx->in);
    buf += t;
    len -= t;
  }
  /* Process data in 64-byte chunks */

  while (len >= 64)
  {
    memcpy(ctx->in, buf, 64);
    byteReverse(ctx->in, 16);
    MD5Transform(ctx->buf, (unsigned int *) ctx->in);
    buf += 64;
    len -= 64;
  }

  /* Handle any remaining bytes of data. */

  memcpy(ctx->in, buf, len);
}


/* #define F1(x, y, z) (x & y | ~x & z) */
#define F1(x, y, z) (z ^ (x & (y ^ z)))   
#define F2(x, y, z) F1(z, x, y)
#define F3(x, y, z) (x ^ y ^ z)
#define F4(x, y, z) (y ^ (x | ~z))

/* This is the central step in the MD5 algorithm. */
#define MD5STEP(f, w, x, y, z, data, s) \
        ( w += f(x, y, z) + data,  w = w<<s | w>>(32-s),  w += x )

/*
 * The core of the MD5 algorithm, this alters an existing MD5 hash to
 * reflect the addition of 16 longwords of new data.  MD5Update blocks
 * the data and converts bytes into longwords for this routine.
 */
static void MD5Transform(unsigned int buf[4], unsigned int const in[16])
{
  register unsigned int a, b, c, d;

  a = buf[0];
  b = buf[1];
  c = buf[2];
  d = buf[3];

  MD5STEP(F1, a, b, c, d, in[0] + 0xd76aa478, 7); 
  MD5STEP(F1, d, a, b, c, in[1] + 0xe8c7b756, 12);
  MD5STEP(F1, c, d, a, b, in[2] + 0x242070db, 17);
  MD5STEP(F1, b, c, d, a, in[3] + 0xc1bdceee, 22);
  MD5STEP(F1, a, b, c, d, in[4] + 0xf57c0faf, 7); 
  MD5STEP(F1, d, a, b, c, in[5] + 0x4787c62a, 12);
  MD5STEP(F1, c, d, a, b, in[6] + 0xa8304613, 17);
  MD5STEP(F1, b, c, d, a, in[7] + 0xfd469501, 22); 
  MD5STEP(F1, a, b, c, d, in[8] + 0x698098d8, 7);  
  MD5STEP(F1, d, a, b, c, in[9] + 0x8b44f7af, 12); 
  MD5STEP(F1, c, d, a, b, in[10] + 0xffff5bb1, 17);
  MD5STEP(F1, b, c, d, a, in[11] + 0x895cd7be, 22);
  MD5STEP(F1, a, b, c, d, in[12] + 0x6b901122, 7); 
  MD5STEP(F1, d, a, b, c, in[13] + 0xfd987193, 12);
  MD5STEP(F1, c, d, a, b, in[14] + 0xa679438e, 17);
  MD5STEP(F1, b, c, d, a, in[15] + 0x49b40821, 22);

  MD5STEP(F2, a, b, c, d, in[1] + 0xf61e2562, 5);  
  MD5STEP(F2, d, a, b, c, in[6] + 0xc040b340, 9);  
  MD5STEP(F2, c, d, a, b, in[11] + 0x265e5a51, 14);
  MD5STEP(F2, b, c, d, a, in[0] + 0xe9b6c7aa, 20); 
  MD5STEP(F2, a, b, c, d, in[5] + 0xd62f105d, 5);  
  MD5STEP(F2, d, a, b, c, in[10] + 0x02441453, 9); 
  MD5STEP(F2, c, d, a, b, in[15] + 0xd8a1e681, 14);
  MD5STEP(F2, b, c, d, a, in[4] + 0xe7d3fbc8, 20); 
  MD5STEP(F2, a, b, c, d, in[9] + 0x21e1cde6, 5);  
  MD5STEP(F2, d, a, b, c, in[14] + 0xc33707d6, 9); 
  MD5STEP(F2, c, d, a, b, in[3] + 0xf4d50d87, 14); 
  MD5STEP(F2, b, c, d, a, in[8] + 0x455a14ed, 20); 
  MD5STEP(F2, a, b, c, d, in[13] + 0xa9e3e905, 5);
  MD5STEP(F2, d, a, b, c, in[2] + 0xfcefa3f8, 9);  
  MD5STEP(F2, c, d, a, b, in[7] + 0x676f02d9, 14);
  MD5STEP(F2, b, c, d, a, in[12] + 0x8d2a4c8a, 20);

  MD5STEP(F3, a, b, c, d, in[5] + 0xfffa3942, 4);
  MD5STEP(F3, d, a, b, c, in[8] + 0x8771f681, 11);
  MD5STEP(F3, c, d, a, b, in[11] + 0x6d9d6122, 16);
  MD5STEP(F3, b, c, d, a, in[14] + 0xfde5380c, 23);
  MD5STEP(F3, a, b, c, d, in[1] + 0xa4beea44, 4);  
  MD5STEP(F3, d, a, b, c, in[4] + 0x4bdecfa9, 11); 
  MD5STEP(F3, c, d, a, b, in[7] + 0xf6bb4b60, 16); 
  MD5STEP(F3, b, c, d, a, in[10] + 0xbebfbc70, 23);
  MD5STEP(F3, a, b, c, d, in[13] + 0x289b7ec6, 4); 
  MD5STEP(F3, d, a, b, c, in[0] + 0xeaa127fa, 11); 
  MD5STEP(F3, c, d, a, b, in[3] + 0xd4ef3085, 16); 
  MD5STEP(F3, b, c, d, a, in[6] + 0x04881d05, 23); 
  MD5STEP(F3, a, b, c, d, in[9] + 0xd9d4d039, 4);  
  MD5STEP(F3, d, a, b, c, in[12] + 0xe6db99e5, 11);
  MD5STEP(F3, c, d, a, b, in[15] + 0x1fa27cf8, 16);
  MD5STEP(F3, b, c, d, a, in[2] + 0xc4ac5665, 23); 

  MD5STEP(F4, a, b, c, d, in[0] + 0xf4292244, 6);
  MD5STEP(F4, d, a, b, c, in[7] + 0x432aff97, 10);
  MD5STEP(F4, c, d, a, b, in[14] + 0xab9423a7, 15);
  MD5STEP(F4, b, c, d, a, in[5] + 0xfc93a039, 21); 
  MD5STEP(F4, a, b, c, d, in[12] + 0x655b59c3, 6); 
  MD5STEP(F4, d, a, b, c, in[3] + 0x8f0ccc92, 10); 
  MD5STEP(F4, c, d, a, b, in[10] + 0xffeff47d, 15);
  MD5STEP(F4, b, c, d, a, in[1] + 0x85845dd1, 21); 
  MD5STEP(F4, a, b, c, d, in[8] + 0x6fa87e4f, 6);  
  MD5STEP(F4, d, a, b, c, in[15] + 0xfe2ce6e0, 10);
  MD5STEP(F4, c, d, a, b, in[6] + 0xa3014314, 15); 
  MD5STEP(F4, b, c, d, a, in[13] + 0x4e0811a1, 21);
  MD5STEP(F4, a, b, c, d, in[4] + 0xf7537e82, 6);  
  MD5STEP(F4, d, a, b, c, in[11] + 0xbd3af235, 10);
  MD5STEP(F4, c, d, a, b, in[2] + 0x2ad7d2bb, 15); 
  MD5STEP(F4, b, c, d, a, in[9] + 0xeb86d391, 21); 

  buf[0] += a;
  buf[1] += b;
  buf[2] += c;
  buf[3] += d;
}
 
// ---------------------------
// wxPdfEncrypt implementation
// ---------------------------

static unsigned char padding[] =
  "\x28\xBF\x4E\x5E\x4E\x75\x8A\x41\x64\x00\x4E\x56\xFF\xFA\x01\x08\x2E\x2E\x00\xB6\xD0\x68\x3E\x80\x2F\x0C\xA9\xFE\x64\x53\x69\x7A";

wxPdfEncrypt::wxPdfEncrypt(int revision, int keyLength)
{
  switch (revision)
  {
    case 4:
      m_rValue = 4;
      m_keyLength = 128 / 8;
      m_aes = new wxPdfRijndael();
      break;
    case 3:
      keyLength = keyLength - keyLength % 8;
      keyLength = (keyLength >= 40) ? ((keyLength <= 128) ? keyLength : 128) : 40;
      m_rValue = 3;
      m_keyLength = keyLength / 8;
      break;
    case 2:
    default:
      m_rValue = 2;
      m_keyLength = 40 / 8;
      break;
  }

  int j;
  for (j = 0; j < 16; j++)
  {
    m_rc4key[j] = 0;
  }
}

wxPdfEncrypt::~wxPdfEncrypt()
{
  if (m_rValue == 4)
  {
    delete m_aes;
  }
}

void
wxPdfEncrypt::PadPassword(const wxString& password, unsigned char pswd[32])
{
  unsigned int m = (unsigned int) password.Length();
  if (m > 32) m = 32;

  unsigned int j;
  unsigned int p = 0;
  wxString::const_iterator ch = password.begin();
  for (j = 0; j < m; j++)
  {
    pswd[p++] = (unsigned char) ((unsigned int) (*ch) & 0xff);
    ++ch;
  }
  for (j = 0; p < 32 && j < 32; j++)
  {
    pswd[p++] = padding[j];
  }
}

void
wxPdfEncrypt::GenerateEncryptionKey(const wxString& userPassword,
                                    const wxString& ownerPassword,
                                    int protection,
                                    const wxString& documentId)
{
  unsigned char userpswd[32];
  unsigned char ownerpswd[32];

  // Pad passwords
  PadPassword(userPassword, userpswd);
  PadPassword(ownerPassword, ownerpswd);

  // Compute P value
  m_pValue = -((protection ^ 255) + 1);

  // Compute O value
  ComputeOwnerKey(userpswd, ownerpswd, m_keyLength*8, m_rValue, false, m_oValue);

  // Compute encryption key and U value
  if (documentId.IsEmpty())
  {
    m_documentId = CreateDocumentId();
  }
  else
  {
    m_documentId = documentId;
  }
  ComputeEncryptionKey(m_documentId, userpswd,
                       m_oValue, m_pValue, m_keyLength*8, m_rValue, m_uValue);
}

bool
wxPdfEncrypt::Authenticate(const wxString& documentID, const wxString& password,
                           const wxString& uValue, const wxString& oValue,
                           int pValue, int lengthValue, int rValue)
{
  unsigned char userKey[32];
  bool ok = false;
  int j;
  wxString::const_iterator uChar = uValue.begin();
  wxString::const_iterator oChar = oValue.begin();
  for (j = 0; j < 32; j++)
  {
    m_uValue[j] = (unsigned char) ((unsigned int) (*uChar) & 0xff);
    m_oValue[j] = (unsigned char) ((unsigned int) (*oChar) & 0xff);
    ++uChar;
    ++oChar;
  }
  m_pValue = pValue;
  m_keyLength = lengthValue / 8;

  // Pad password
  unsigned char pswd[32];
  PadPassword(password, pswd);

  // Check password: 1) as user password, 2) as owner password
  ComputeEncryptionKey(documentID, pswd, m_oValue, pValue, lengthValue, rValue, userKey);
  ok = CheckKey(userKey, m_uValue);
  if (!ok)
  {
    unsigned char userpswd[32];
    ComputeOwnerKey(m_oValue, pswd, lengthValue, rValue, true, userpswd);
    ComputeEncryptionKey(documentID, userpswd, m_oValue, pValue, lengthValue, rValue, userKey);
    ok = CheckKey(userKey, m_uValue);
  }
  return ok;
}

void
wxPdfEncrypt::ComputeOwnerKey(unsigned char userPad[32], unsigned char ownerPad[32],
                              unsigned int keyLength, int revision, bool authenticate,
                              unsigned char ownerKey[32])
{
  unsigned char mkey[MD5_HASHBYTES];
  unsigned char digest[MD5_HASHBYTES];
  unsigned int length = keyLength / 8;

  MD5_CTX ctx;
  MD5Init(&ctx);
  MD5Update(&ctx, ownerPad, 32);
  MD5Final(digest,&ctx);

  if (revision == 3 || revision == 4)
  {
    // only use for the input as many bit as the key consists of
    unsigned int k;
    for (k = 0; k < 50; ++k)
    {
      MD5Init(&ctx);
      MD5Update(&ctx, digest, length);
      MD5Final(digest,&ctx);
    }
    memcpy(ownerKey, userPad, 32);
    unsigned int i;
    unsigned int j;
    for (i = 0; i < 20; ++i)
    {
      for (j = 0; j < length ; ++j)
      {
        if (authenticate)
        {
          mkey[j] = (digest[j] ^ (19-i));
        }
        else
        {
          mkey[j] = (digest[j] ^ i);
        }
      }
      RC4(mkey, length, ownerKey, 32, ownerKey);
    }
  }
  else
  {
    RC4(digest, 5, userPad, 32, ownerKey);
  }
}

void
wxPdfEncrypt::ComputeEncryptionKey(const wxString& documentId,
                                   unsigned char userPad[32], unsigned char ownerKey[32],
                                   int pValue, unsigned int keyLength, int revision,
                                   unsigned char userKey[32])
{
  unsigned int k;
  m_keyLength = keyLength / 8;

  MD5_CTX ctx;
  MD5Init(&ctx);
  MD5Update(&ctx, userPad, 32);
  MD5Update(&ctx, ownerKey, 32);

  unsigned char ext[4];
  ext[0] = (unsigned char) ( pValue        & 0xff);
  ext[1] = (unsigned char) ((pValue >>  8) & 0xff);
  ext[2] = (unsigned char) ((pValue >> 16) & 0xff);
  ext[3] = (unsigned char) ((pValue >> 24) & 0xff);
  MD5Update(&ctx, ext, 4);

  unsigned int docIdLength = (unsigned int) documentId.Length();
  unsigned char* docId = NULL;
  if (docIdLength > 0)
  {
    docId = new unsigned char[docIdLength];
    unsigned int j;
    wxString::const_iterator dChar = documentId.begin();
    for (j = 0; j < docIdLength; j++)
    {
      docId[j] = (unsigned char) ((unsigned int) (*dChar) & 0xff);
      ++dChar;
    }
    MD5Update(&ctx, docId, docIdLength);
  }
  
  // TODO: (Revision 3 or greater) If document metadata is not being encrypted,
  //       pass 4 bytes with the value 0xFFFFFFFF to the MD5 hash function.

  unsigned char digest[MD5_HASHBYTES];
  MD5Final(digest,&ctx);

  // only use the really needed bits as input for the hash
  if (revision == 3 || revision == 4)
  {
    for (k = 0; k < 50; ++k)
    {
      MD5Init(&ctx);
      MD5Update(&ctx, digest, m_keyLength);
      MD5Final(digest, &ctx);
    }
  }

  memcpy(m_encryptionKey, digest, m_keyLength);

  // Setup user key
  if (revision == 3 || revision == 4)
  {
    MD5Init(&ctx);
    MD5Update(&ctx, padding, 32);
    if (docId != NULL)
    {
      MD5Update(&ctx, docId, docIdLength);
    }
    MD5Final(digest, &ctx);
    memcpy(userKey, digest, 16);
    for (k = 16; k < 32; ++k)
    {
      userKey[k] = 0;
    }
    for (k = 0; k < 20; k++)
    {
      unsigned int j;
      for (j = 0; j < m_keyLength; ++j)
      {
        digest[j] = (unsigned char)(m_encryptionKey[j] ^ k);
      }
      RC4(digest, m_keyLength, userKey, 16, userKey);
    }
  }
  else
  {
    RC4(m_encryptionKey, m_keyLength, padding, 32, userKey);
  }
  if (docId != NULL)
  {
    delete [] docId;
  }
}

bool
wxPdfEncrypt::CheckKey(unsigned char key1[32], unsigned char key2[32])
{
  // Check whether the right password had been given
  bool ok = true;
  int k;
  int kmax = (m_rValue == 3) ? 16 : 32;
  for (k = 0; ok && k < kmax; k++)
  {
    ok = ok && (key1[k] == key2[k]);
  }
  return ok;
}

void
wxPdfEncrypt::Encrypt(int n, int g, wxString& str)
{
  unsigned int len = (unsigned int) str.Length();
  unsigned char* data = new unsigned char[len];
  unsigned int j;
  for (j = 0; j < len; j++)
  {
    data[j] = (unsigned char) str.GetChar(j);
  }
  Encrypt(n, g, data, len);
  for (j = 0; j < len; j++)
  {
    str.SetChar(j, data[j]);
  }
  delete [] data;
}

void
wxPdfEncrypt::Encrypt(int n, int g, unsigned char* str, unsigned int len)
{
  unsigned char objkey[MD5_HASHBYTES];
  unsigned char nkey[MD5_HASHBYTES+5+4];
  unsigned int nkeylen = m_keyLength + 5;
  unsigned int j;
  for (j = 0; j < m_keyLength; j++)
  {
    nkey[j] = m_encryptionKey[j];
  }
  nkey[m_keyLength+0] = 0xff &  n;
  nkey[m_keyLength+1] = 0xff & (n >> 8);
  nkey[m_keyLength+2] = 0xff & (n >> 16);
  nkey[m_keyLength+3] = 0xff &  g;
  nkey[m_keyLength+4] = 0xff & (g >> 8);

  if (m_rValue == 4)
  {
    // AES encryption needs some 'salt'
    nkeylen += 4;
    nkey[m_keyLength+5] = 0x73;
    nkey[m_keyLength+6] = 0x41;
    nkey[m_keyLength+7] = 0x6c;
    nkey[m_keyLength+8] = 0x54;
  }

  GetMD5Binary(nkey, nkeylen, objkey);
  int keylen = (m_keyLength <= 11) ? m_keyLength+5 : 16;
  switch (m_rValue)
  {
    case 4:
      AES(objkey, keylen, str, len, str);
      break;
    case 3:
    case 2:
    default:
      RC4(objkey, keylen, str, len, str);
      break;
  }
}

/**
* RC4 is the standard encryption algorithm used in PDF format
*/

void
wxPdfEncrypt::RC4(unsigned char* key, unsigned int keylen,
                  unsigned char* textin, unsigned int textlen,
                  unsigned char* textout)
{
  unsigned int i;
  unsigned int j;
  int t;
  unsigned char rc4[256];

  if (memcmp(key,m_rc4key,keylen) != 0)
  {
    for (i = 0; i < 256; i++)
    {
      rc4[i] = i;
    }
    j = 0;
    for (i = 0; i < 256; i++)
    {
      t = rc4[i];
      j = (j + t + key[i % keylen]) % 256;
      rc4[i] = rc4[j];
      rc4[j] = t;
    }
    memcpy(m_rc4key,key,keylen);
    memcpy(m_rc4last,rc4,256);
  }
  else
  {
    memcpy(rc4,m_rc4last,256);
  }

  int a = 0;
  int b = 0;
  unsigned char k;
  for (i = 0; i < textlen; i++)
  {
    a = (a + 1) % 256;
    t = rc4[a];
    b = (b + t) % 256;
    rc4[a] = rc4[b];
    rc4[b] = t;
    k = rc4[(rc4[a] + rc4[b]) % 256];
    textout[i] = textin[i] ^ k;
  }
}

void
wxPdfEncrypt::GetMD5Binary(const unsigned char* data, unsigned int length, unsigned char* digest)
{
  MD5_CTX ctx;
  MD5Init(&ctx);
  MD5Update(&ctx, data, length);
  MD5Final(digest,&ctx);
}

void
wxPdfEncrypt::AES(unsigned char* key, unsigned int keylen,
                  unsigned char* textin, unsigned int textlen,
                  unsigned char* textout)
{
  wxUnusedVar(keylen);
  GenerateInitialVector(textout);
  m_aes->init(wxPdfRijndael::CBC, wxPdfRijndael::Encrypt, key, wxPdfRijndael::Key16Bytes, textout);
  size_t offset = CalculateStreamOffset();
  int len = m_aes->padEncrypt(&textin[offset], textlen, &textout[offset]);
  
  // It is a good idea to check the error code
  if (len < 0)
  {
    wxLogError(wxString(wxT("wxPdfEncrypt::AES: ")) +
               wxString(_("Error on encrypting.")));
  }
}

void
wxPdfEncrypt::GenerateInitialVector(unsigned char iv[16])
{
  wxString keyString = wxPdfUtility::GetUniqueId();
#if wxUSE_UNICODE
  wxCharBuffer cb(keyString.ToAscii());
  const char* key = (const char*) cb;
#else
  const char* key = (const char*) keyString.c_str();
#endif
  GetMD5Binary((const unsigned char*) key, (unsigned int) keyString.Length(), iv);
}

size_t
wxPdfEncrypt::CalculateStreamLength(size_t length)
{
  size_t realLength = length;
  if (m_rValue == 4)
  {
//    realLength = (length % 0x7ffffff0) + 32;
    realLength = ((length + 15) & ~15) + 16;
    if (length % 16 == 0)
    {
      realLength += 16;
    }
  }
  return realLength;
}

size_t
wxPdfEncrypt::CalculateStreamOffset()
{
  size_t offset = 0;
  if (m_rValue == 4)
  {
    offset = 16;
  }
  return offset;
}

wxString
wxPdfEncrypt::CreateDocumentId()
{
  wxString documentId;
  unsigned char id[16];
  GenerateInitialVector(id);
  int k;
  for (k = 0; k < 16; k++)
  {
    documentId.Append(wxChar(id[k]));
  }
  return documentId;
}
