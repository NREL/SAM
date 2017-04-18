///////////////////////////////////////////////////////////////////////////////
// Name:        pdfencrypt.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2005-08-16
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfencrypt.h Interface of the wxPdfFont class

#ifndef _PDF_ENCRYPT_H_
#define _PDF_ENCRYPT_H_

// wxWidgets headers
#include <wx/string.h>

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"

// Forward declaration of exported classes
class WXDLLIMPEXP_FWD_PDFDOC wxPdfRijndael;

/// Class representing PDF encryption methods. (For internal use only)
class WXDLLIMPEXP_PDFDOC wxPdfEncrypt
{
public:
  /// Constructor
  /**
  * \param revision revision of the encryption algorithm
  * \param keyLength length of the key for the encryption
  */
  wxPdfEncrypt(int revision = 2, int keyLength = 40);

  /// Default destructor
  virtual ~wxPdfEncrypt();

  /// Generate encryption key from user and owner passwords and protection key
  /**
  * \param userPassword password of the user
  * \param ownerPassword password of the owner
  * \param protection protection flags
  * \param documentId optional document identification
  */
  void GenerateEncryptionKey(const wxString& userPassword,
                             const wxString& ownerPassword,
                             int protection,
                             const wxString& documentId = wxEmptyString);

  /// Authenticate a document
  /**
  * \param documentID the identification of the document
  * \param password the password given by the user
  * \param uValue the U value from the document's encryption dictionary
  * \param oValue the O value from the document's encryption dictionary
  * \param pValue the P value from the document's encryption dictionary
  * \param lengthValue the length value  from the document's encryption dictionary
  * \param rValue the R value from the document's encryption dictionary
  * \return TRUE if the document could be authenticated successfully, FALSE otherwise
  */
  bool Authenticate(const wxString& documentID, const wxString& password,
                    const wxString& uValue, const wxString& oValue,
                    int pValue, int lengthValue, int rValue);

  /// Get the U object value (user)
  const unsigned char* GetUValue() const { return m_uValue; }

  /// Get the O object value (owner)
  const unsigned char* GetOValue() const { return m_oValue; }

  /// Get the P object value (protection)
  int GetPValue() const { return m_pValue; }

  /// Get the revision number of the encryption method
  int GetRevision() const { return m_rValue; }

  /// Get the key length of the encryption key in bits
  unsigned int GetKeyLength() const { return m_keyLength*8; }

  /// Encrypt a wxString
  /**
  * \param n number of the associated PDF object
  * \param g generation of the associated PDF object
  * \param str string to encrypt
  */
  void Encrypt(int n, int g, wxString& str);

  /// Encrypt a character string
  /**
  * \param n number of the associated PDF object
  * \param g generation of the associated PDF object
  * \param str string to encrypt
  * \param len length of the string to encrypt
  */
  void Encrypt(int n, int g, unsigned char* str, unsigned int len);

  /// Calculate stream size
  /**
  * \param length lenght of the original stream
  * \return the length of the encrypted stream
  */
  size_t CalculateStreamLength(size_t length);

  /// Calculate stream offset
  /**
  * \return the offset of the stream
  */
  size_t CalculateStreamOffset();

  /// Create document id
  /**
  * \return the created document identifier
  */
  wxString CreateDocumentId();

  /// Get document id
  /**
  * \return the associated document identifier
  */
  wxString GetDocumentId() const { return m_documentId; }

protected:
  /// Pad a password to 32 characters
  void PadPassword(const wxString& password, unsigned char pswd[32]);

  /// Compute owner key
  void ComputeOwnerKey(unsigned char userPad[32], unsigned char ownerPad[32],
                       unsigned int keylength, int revision, bool authenticate,
                       unsigned char ownerKey[32]);

  /// Compute encryption key and user key
  void ComputeEncryptionKey(const wxString& documentID,
                            unsigned char userPad[32], unsigned char ownerKey[32],
                            int pValue, unsigned int keyLength, int revision,
                            unsigned char userKey[32]);

  /// Check two keys for equality
  bool CheckKey(unsigned char key1[32], unsigned char key2[32]);

  /// RC4 encryption
  void RC4(unsigned char* key, unsigned int keylen,
           unsigned char* textin, unsigned int textlen,
           unsigned char* textout);

  /// Calculate the binary MD5 message digest of the given data
  void GetMD5Binary(const unsigned char* data, unsigned int length, unsigned char* digest);

  /// AES encryption
  void AES(unsigned char* key, unsigned int keylen,
           unsigned char* textin, unsigned int textlen,
           unsigned char* textout);

  /// Generate initial vector
  void GenerateInitialVector(unsigned char iv[16]);

private:
  wxString       m_documentId;         ///< Document ID
  unsigned char  m_uValue[32];         ///< U entry in pdf document
  unsigned char  m_oValue[32];         ///< O entry in pdf document
  int            m_pValue;             ///< P entry in pdf document
  int            m_rValue;             ///< Revision
  unsigned char  m_encryptionKey[16];  ///< Encryption key
  unsigned int   m_keyLength;          ///< Length of encryption key
  unsigned char  m_rc4key[16];         ///< last RC4 key
  unsigned char  m_rc4last[256];       ///< last RC4 state table

  wxPdfRijndael* m_aes;                ///< AES encryptor
};

#endif
