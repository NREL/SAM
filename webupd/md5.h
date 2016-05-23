/////////////////////////////////////////////////////////////////////////////
// Name:        md5.h
// Purpose:     MD5 file checksum
// Author:      Francesco Montorsi
// Created:     2005/07/13
// RCS-ID:      $Id: md5.h,v 1.4 2005/10/20 16:06:01 frm Exp $
// Copyright:   (c) 2005 Francesco Montorsi
// Licence:     wxWidgets licence + RDS Data Security license
/////////////////////////////////////////////////////////////////////////////

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
 

#ifndef _WX_MD5_H_
#define _WX_MD5_H_

// wxWidgets headers
#include "wx/string.h"
#include "wx/wfstream.h"



// 
// MD5 from RSA
// ------------

#define MD5_HASHBYTES 16

typedef struct MD5Context {
        unsigned int buf[4];
        unsigned int bits[2];
        unsigned char in[64];
} MD5_CTX;

void   MD5Init(MD5_CTX *context);
void   MD5Update(MD5_CTX *context, unsigned char const *buf, unsigned len);
void   MD5Final(unsigned char digest[MD5_HASHBYTES], MD5_CTX *context);
void   MD5Transform(unsigned int buf[4], unsigned int const in[16]);
char * MD5End(MD5_CTX *, char *);



//! A utility class to calculate MD5 checksums from files or strings.
class wxMD5
{
public:
	wxMD5() {}
	virtual ~wxMD5() {}

public:

	//! Returns the MD5 checksum for the given file
	static wxString GetFileMD5(wxInputStream &str);
	static wxString GetFileMD5(const wxString &filename);

	//! Returns the MD5 for the given string.
	static wxString GetMD5(const wxString &str);
};

#endif		// _WX_MD5_H_
