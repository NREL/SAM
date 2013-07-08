/*
*	base.cpp
*	wxCURL
*
*	Created by Casey O'Donnell on Tue Jun 29 2004.
*	Copyright (c) 2004 Casey O'Donnell. All rights reserved.
*
*
*	Licence: wxWidgets Licence
*/

//////////////////////////////////////////////////////////////////////
// Headers
//////////////////////////////////////////////////////////////////////

#include "wx/wxprec.h"

#ifndef WX_PRECOMP
    #include "wx/wx.h"
#endif

#ifdef __WXMSW__
    #include <wx/msw/msvcrt.h>      // useful to catch memory leaks when compiling under MSVC 
#endif

#include <stdio.h>
#include <stdarg.h>

#include "wxcurlbase.h"
#include <wx/filename.h>


//////////////////////////////////////////////////////////////////////
// Constants
//////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////
// C Functions for LibCURL
//////////////////////////////////////////////////////////////////////

extern "C"
{
    int wxcurl_evt_progress_func(void* ptr, double rDlTotal, double rDlNow, 
                                 double rUlTotal, double rUlNow)
    {
        wxCurlBase *curl = wx_static_cast(wxCurlBase*, ptr);
        if(curl)
        {
            if (rUlTotal == 0 || rUlNow == 0)
            {
                /* should be a download event */
                wxCurlDownloadEvent evt(curl->GetId(), curl, rDlTotal, rDlNow, curl->GetURL());
                wxPostEvent(curl->GetEvtHandler(), evt);
            }

            if (rDlTotal == 0 || rDlNow == 0)
            {
                /* should be an upload event */
                wxCurlDownloadEvent evt(curl->GetId(), curl, rUlTotal, rUlNow, curl->GetURL());
                wxPostEvent(curl->GetEvtHandler(), evt);
            }

			wxThread *tt = curl->GetThread();
			if (tt && tt->TestDestroy())
			{
				// cancel thread
				return -1; // return some error code?
			}
        }

        return 0;
    }

    int wxcurl_verbose_stream_write(CURL * /*crlptr*/, curl_infotype info,
                                    char * cStrMessage, size_t msgSize, void * buffer)
    {
        wxString szMessage((const char*)cStrMessage, wxConvLibc, msgSize);
        wxStringOutputStream* pBuf = (wxStringOutputStream*)buffer;
        wxString szVerboseMessage;

        switch (info)
        {
            case CURLINFO_TEXT:
                szVerboseMessage = wxString(wxS("Text: ")) + szMessage + wxS("\n");
                break;
            case CURLINFO_HEADER_IN:
                szVerboseMessage = wxString(wxS("Header in: ")) + szMessage + wxS("\n");
                break;
            case CURLINFO_HEADER_OUT:
                szVerboseMessage = wxString(wxS("Header out: ")) + szMessage + wxS("\n");
                break;
            case CURLINFO_DATA_IN:
                szVerboseMessage = wxString(wxS("Data in: ")) + szMessage + wxS("\n");
                break;
            case CURLINFO_DATA_OUT:
                szVerboseMessage = wxString(wxS("Data out: ")) + szMessage + wxS("\n");
                break;
            case CURLINFO_END:
                szVerboseMessage = wxString(wxS("End: ")) + szMessage + wxS("\n");
                break;
            case CURLINFO_SSL_DATA_IN:
                szVerboseMessage = wxString(wxS("SSL Data in: ")) + szMessage + wxS("\n");
                break;
            case CURLINFO_SSL_DATA_OUT:
                szVerboseMessage = wxString(wxS("SSL Data out: ")) + szMessage + wxS("\n");
                break;
        }

        // here wxChar usage is correct as szVerboseMessage string is used:
#if wxCHECK_VERSION(2,9,0)
        pBuf->Write(szVerboseMessage.tchar_str<wxChar>(), szVerboseMessage.Len() * sizeof(wxChar));
#else   
		pBuf->Write(szVerboseMessage.c_str(), szVerboseMessage.Len() * sizeof(wxChar));
#endif

        return 0;
    }

    size_t wxcurl_header_func(void *ptr, size_t size, size_t nmemb, void *stream)
    {
        size_t iRealSize = size * nmemb;
        wxCharBuffer* pStr = (wxCharBuffer*) stream;

        if(pStr)
        {
            wxString str = wxCURL_BUF2STRING(*pStr) + wxString((const char*)ptr, wxConvLibc);
            *pStr = wxCURL_STRING2BUF(str);
        }

        return iRealSize;
    }

    /* writes to a string */
    size_t wxcurl_string_write(void* ptr, size_t size, size_t nmemb, void* pcharbuf)
    {
        size_t iRealSize = size * nmemb;
        wxCharBuffer* pStr = (wxCharBuffer*) pcharbuf;

        if(pStr)
        {
            wxString str = wxCURL_BUF2STRING(*pStr) + wxString((const char*)ptr, wxConvLibc);
            *pStr = wxCURL_STRING2BUF(str);
        }

        return iRealSize;
    }

    /* writes to a stream */
    size_t wxcurl_stream_write(void* ptr, size_t size, size_t nmemb, void* stream)
    {
        size_t iRealSize = size * nmemb;

        wxOutputStream* pBuf = (wxOutputStream*)stream;

        if(pBuf)
        {
            pBuf->Write(ptr, iRealSize);

            return pBuf->LastWrite();
        }

        return 0;
    }

    /* reads from a string */
    size_t wxcurl_string_read(void* ptr, size_t size, size_t nmemb, void* pcharbuf)
    {
        size_t iRealSize = size * nmemb;
        size_t iRetVal = 0;

        wxCharBuffer* pStr = (wxCharBuffer*) pcharbuf;
        size_t len = strlen(*pStr);

        if(pStr)
        {
            if(len >= iRealSize)
            {
                strncpy((char*)ptr, (const char*)(*pStr), iRealSize);
                iRetVal = iRealSize;
            }
            else
            {
                strncpy((char*)ptr, (const char*)(*pStr), len);
                iRetVal = len;
            }

            wxString remaining = wxCURL_BUF2STRING(pStr).Right(len - iRetVal);
            *pStr = wxCURL_STRING2BUF(remaining);
        }

        return iRetVal;
    }

    /* reads from a stream */
    size_t wxcurl_stream_read(void* ptr, size_t size, size_t nmemb, void* stream)
    {
        size_t iRealSize = size * nmemb;

        wxInputStream* pBuf = (wxInputStream*)stream;

        if(pBuf)
        {
            pBuf->Read(ptr, iRealSize);

            return pBuf->LastRead();
        }

        return 0;
    }
}


// base.cpp: implementation of the wxCurlProgressBaseEvent class.
//
//////////////////////////////////////////////////////////////////////

wxTimeSpan wxCurlProgressBaseEvent::GetElapsedTime() const
{
    // NOTE: we cannot trust libCURL's CURLINFO_TOTAL_TIME as the transfer may have
    //       been paused in one of libCURL's callbacks (and thus libCURL ignores it
    //       and won't remove the paused span from the return value).
    wxTimeSpan elapsed = m_dt - m_pCURL->GetBeginTransferSpan();

    // the elapsed time offset takes in count eventually-existing previous time spans
    // where the transfer took place (
    return elapsed + m_pCURL->GetElapsedTimeOffset();
}

wxTimeSpan wxCurlProgressBaseEvent::GetEstimatedTime() const
{
    volatile double nBytesPerSec = GetSpeed();
    if (nBytesPerSec == 0 || (nBytesPerSec != nBytesPerSec))
        return wxTimeSpan(0);       // avoid division by zero

    // compute remaining seconds; here we assume that the current
    // download speed will be constant also in future
    double secs = GetTotalBytes() / nBytesPerSec;

    return wxTimeSpan(int(secs/3600.0),     // hours
                    int(secs/60) % 60,    // minutes
                    int(secs) % 60,       // seconds
                    0);                   // milliseconds
}

wxTimeSpan wxCurlProgressBaseEvent::GetEstimatedRemainingTime() const
{
    wxTimeSpan est = GetEstimatedTime(),
            elapsed = GetElapsedTime();

    if (est.IsLongerThan(elapsed))
        return est - elapsed;
    return wxTimeSpan(0);       // probably est==0 because GetTotalBytes()==0
}

wxString wxCurlProgressBaseEvent::GetHumanReadableSpeed(const wxString &invalid, int precision) const
{
    volatile double speed = GetSpeed();
    if (speed == 0 || (speed)!=(speed))
        return invalid;

    wxULongLong ull((wxULongLong_t)speed);
    return wxFileName::GetHumanReadableSize(ull, invalid, precision) + wxS("/s");
}


// base.cpp: implementation of the wxCurlDownloadEvent class.
//
//////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

DEFINE_EVENT_TYPE(wxCURL_DOWNLOAD_EVENT);

IMPLEMENT_DYNAMIC_CLASS(wxCurlDownloadEvent, wxEvent);

wxCurlDownloadEvent::wxCurlDownloadEvent()
: wxCurlProgressBaseEvent(-1, wxCURL_DOWNLOAD_EVENT),
m_rDownloadNow(0.0), m_rDownloadTotal(0.0)
{
}

wxCurlDownloadEvent::wxCurlDownloadEvent(int id, wxCurlBase *originator,
                                        const double& rDownloadTotal, const double& rDownloadNow, 
                                        const wxString& szURL /*= wxEmptyString*/)
: wxCurlProgressBaseEvent(id, wxCURL_DOWNLOAD_EVENT, originator, szURL),
m_rDownloadTotal(rDownloadTotal), m_rDownloadNow(rDownloadNow)
{
}

wxCurlDownloadEvent::wxCurlDownloadEvent(const wxCurlDownloadEvent& event)
: wxCurlProgressBaseEvent(event)
{
    m_rDownloadNow = event.m_rDownloadNow;
    m_rDownloadTotal = event.m_rDownloadTotal;
}



// base.cpp: implementation of the wxCurlUploadEvent class.
//
//////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

DEFINE_EVENT_TYPE(wxCURL_UPLOAD_EVENT);

IMPLEMENT_DYNAMIC_CLASS(wxCurlUploadEvent, wxEvent);

wxCurlUploadEvent::wxCurlUploadEvent()
: wxCurlProgressBaseEvent(-1, wxCURL_UPLOAD_EVENT),
m_rUploadNow(0.0), m_rUploadTotal(0.0)
{
}

wxCurlUploadEvent::wxCurlUploadEvent(int id, wxCurlBase *originator,
                                        const double& rUploadTotal, const double& rUploadNow, 
                                        const wxString& szURL /*= wxEmptyString*/)
: wxCurlProgressBaseEvent(id, wxCURL_UPLOAD_EVENT, originator, szURL),
m_rUploadTotal(rUploadTotal), m_rUploadNow(rUploadNow)
{
}

wxCurlUploadEvent::wxCurlUploadEvent(const wxCurlUploadEvent& event)
: wxCurlProgressBaseEvent(event)
{
    m_rUploadNow = event.m_rUploadNow;
    m_rUploadTotal = event.m_rUploadTotal;
}



// wxCurlBase.cpp: implementation of the wxCurlBeginPerformEvent class.
//
//////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

DEFINE_EVENT_TYPE(wxCURL_BEGIN_PERFORM_EVENT);

IMPLEMENT_DYNAMIC_CLASS(wxCurlBeginPerformEvent, wxEvent);

wxCurlBeginPerformEvent::wxCurlBeginPerformEvent()
: wxEvent(-1, wxCURL_BEGIN_PERFORM_EVENT)
{
}

wxCurlBeginPerformEvent::wxCurlBeginPerformEvent(int id, const wxString& szURL)
: wxEvent(id, wxCURL_BEGIN_PERFORM_EVENT),
m_szURL(szURL)
{
}

wxCurlBeginPerformEvent::wxCurlBeginPerformEvent(const wxCurlBeginPerformEvent& event)
: wxEvent(event)
{
    m_szURL = event.m_szURL;
}

// wxCurlBase.cpp: implementation of the wxCurlEndPerformEvent class.
//
//////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

DEFINE_EVENT_TYPE(wxCURL_END_PERFORM_EVENT);

IMPLEMENT_DYNAMIC_CLASS(wxCurlEndPerformEvent, wxEvent);

wxCurlEndPerformEvent::wxCurlEndPerformEvent()
: wxEvent(-1, wxCURL_END_PERFORM_EVENT),
m_iResponseCode(0)
{
}

wxCurlEndPerformEvent::wxCurlEndPerformEvent(int id, const wxString& szURL, const long& iResponseCode)
: wxEvent(id, wxCURL_END_PERFORM_EVENT),
m_szURL(szURL),
m_iResponseCode(iResponseCode)
{
}

wxCurlEndPerformEvent::wxCurlEndPerformEvent(const wxCurlEndPerformEvent& event)
: wxEvent(event)
{
    m_szURL = event.m_szURL;
    m_iResponseCode = event.m_iResponseCode;
}

// wxCurlBase.cpp: implementation of the wxCurlBase class.
//
//////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

wxCurlBase::wxCurlBase(const wxString& szURL /*= wxEmptyString*/,
                    const wxString& szUserName /*= wxEmptyString*/,
                    const wxString& szPassword /*= wxEmptyString*/,
                    wxEvtHandler* pEvtHandler /*= NULL*/,
                    int id /*= wxID_ANY*/,
                    long flags /*=wxCURL_DEFAULT_FLAGS*/)
: m_szBaseURL(wxCURL_STRING2BUF(szURL)),
m_szCurrFullURL(wxCURL_STRING2BUF(szURL)),
m_szUsername(wxCURL_STRING2BUF(szUserName)),
m_szPassword(wxCURL_STRING2BUF(szPassword)),
m_iHostPort(-1), m_iResponseCode(-1),
m_bUseProxy(false), m_iProxyPort(-1),
m_pCURL(NULL), m_pHeaders(NULL), 
m_pEvtHandler(pEvtHandler), m_nId(id),
m_nFlags(flags),
m_bVerbose(false),
m_verifyHttps(false),
m_timeoutSec( -1 ),
m_thread( NULL )
{
    m_szDetailedErrorBuffer[0] = '\0';
    m_progressCallback = wxcurl_evt_progress_func;
    m_progressData = this;

    InitHandle();
}

wxCurlBase::~wxCurlBase()
{
    CleanupHandle();
    ResetHeaders();
}

//////////////////////////////////////////////////////////////////////
// LibCURL Abstraction Methods
//////////////////////////////////////////////////////////////////////

typedef int (*func_T)(void);
bool wxCurlBase::SetOpt(CURLoption option, ...)
{
    va_list arg;

    func_T param_func = (func_T)0;
    long param_long = 0;
    void *param_obj = NULL;
    curl_off_t param_offset = 0;

    va_start(arg, option);

    CURLcode res = CURLE_OK;

    // This code stolen from easy.c from LibCURL - It is needed to ensure that
    // types are maintained.
    if(option < CURLOPTTYPE_OBJECTPOINT) {
        /* This is a LONG type */
        param_long = va_arg(arg, long);
        res = curl_easy_setopt(m_pCURL, option, param_long);
    }
    else if(option < CURLOPTTYPE_FUNCTIONPOINT) {
        /* This is a object pointer type */
        param_obj = va_arg(arg, void *);
        res = curl_easy_setopt(m_pCURL, option, param_obj);
    }
    else if(option < CURLOPTTYPE_OFF_T) {
        /* This is a function pointer type */
        param_func = va_arg(arg, func_T );
        res = curl_easy_setopt(m_pCURL, option, param_func);
    } else {
        /* This is a curl_off_t type */
        param_offset = va_arg(arg, curl_off_t);
        res = curl_easy_setopt(m_pCURL, option, param_offset);
    }

    va_end(arg);

    DumpErrorIfNeed(res);
    return (res == CURLE_OK);
}

bool wxCurlBase::SetStringOpt(CURLoption option, const wxCharBuffer &str)
{
    // VERY IMPORTANT: the caller must ensure given wxCharBuffer is valid
    //                 for all the time it's owned by libCURL

    /*  FIXME: converting to plain ASCII is not always the Best Thing. E.g.
            for CURLOPT_USERPWD, we'd need to consult RFC2616 (HTTP) or 
            another RFC depending on the authentication system in use, etc etc
            For now we convert to pure ASCII which in 99% of the cases will
            Just Do the Work
    */

    return SetOpt(option, (const char*)str);
}

bool wxCurlBase::GetInfo(CURLINFO info, ...) const
{
    va_list arg;
    void* pParam;

    va_start(arg, info);
    pParam = va_arg(arg, void*);

    CURLcode res = CURLE_OK;

    res = curl_easy_getinfo(m_pCURL, info, pParam);

    DumpErrorIfNeed(res);
    return (res == CURLE_OK);
}

bool wxCurlBase::Perform()
{
    CURLcode res = CURLE_OK;

    if((m_nFlags & wxCURL_SEND_BEGINEND_EVENTS) && m_pEvtHandler)
    {
        wxCurlBeginPerformEvent bgnEvent(m_nId, wxCURL_BUF2STRING(m_szCurrFullURL));
        wxPostEvent(m_pEvtHandler, bgnEvent);
    }

    // reset time-related vars:
    m_tsElapsedOffset = 0;
    m_dtBeginTransferSpan = wxDateTime::Now();
	
	if ( !m_verifyHttps )
	{
		curl_easy_setopt(m_pCURL, CURLOPT_SSL_VERIFYPEER, 0L);
		curl_easy_setopt(m_pCURL, CURLOPT_SSL_VERIFYHOST, 0L);
	}

	if ( m_timeoutSec > 0 )
	{
		// CURLOPT_CONNECTTIMEOUT - The number of seconds to wait while trying to connect. Use 0 to wait indefinitely.
		// CURLOPT_TIMEOUT - The maximum number of seconds to allow cURL functions to execute.
		curl_easy_setopt(m_pCURL, CURLOPT_CONNECTTIMEOUT, m_timeoutSec );
		curl_easy_setopt(m_pCURL, CURLOPT_TIMEOUT, m_timeoutSec );
	}

	curl_easy_setopt(m_pCURL, CURLOPT_FOLLOWLOCATION, 1);

    // perform the operation:
    res = curl_easy_perform(m_pCURL);
	
    // get the response code of the server
    GetInfo(CURLINFO_RESPONSE_CODE, &m_iResponseCode);

    if((m_nFlags & wxCURL_SEND_BEGINEND_EVENTS) && m_pEvtHandler)
    {
        wxCurlEndPerformEvent endEvent(m_nId, wxCURL_BUF2STRING(m_szCurrFullURL), m_iResponseCode);
        wxPostEvent(m_pEvtHandler, endEvent);
    }

    DumpErrorIfNeed(res);
    return (res == CURLE_OK);
}

bool wxCurlBase::InitHandle()
{
    if(m_pCURL)
        return false;

    m_pCURL = curl_easy_init();

    return (m_pCURL != NULL);
}

bool wxCurlBase::ReInitHandle()
{
    CleanupHandle();
    return InitHandle();
}

bool wxCurlBase::CleanupHandle()
{
    if(m_pCURL)
    {
        curl_easy_cleanup(m_pCURL);
        m_pCURL = NULL;
    }

    return true;
}

bool wxCurlBase::ResetHandle()
{
    curl_easy_reset(m_pCURL);

    return true;
}

void wxCurlBase::DumpErrorIfNeed(CURLcode error) const
{
    // save the error description:
    wx_const_cast(wxCurlBase*, this)->m_szLastError = curl_easy_strerror(error);

    if (m_bVerbose && error != CURLE_OK)
    {
        // dump the error if needed:
        wxLogDebug(wxS("[wxCURL] %hs"), (const char*)m_szLastError);
    }
}

//////////////////////////////////////////////////////////////////////
// Member Data Access Methods
//////////////////////////////////////////////////////////////////////

bool wxCurlBase::SetEvtHandler(wxEvtHandler* pEvtHandler, int id)
{
    m_pEvtHandler = pEvtHandler;
    m_nId = id;

    return true;
}

wxEvtHandler* wxCurlBase::GetEvtHandler() const
{
    return m_pEvtHandler;
}

int wxCurlBase::GetId() const
{
    return m_nId;
}

void wxCurlBase::SetFlags(long flags)
{
    m_nFlags = flags;
}

long wxCurlBase::GetFlags() const
{
    return m_nFlags;
}

void wxCurlBase::SetBaseURL(const wxString& szBaseURL)
{
    m_szBaseURL = wxCURL_STRING2BUF(szBaseURL);
}

wxString wxCurlBase::GetBaseURL() const
{
    return wxCURL_BUF2STRING(m_szBaseURL);
}

void wxCurlBase::SetURL(const wxString& szRelativeURL)
{
    wxString str = wxCURL_BUF2STRING(m_szCurrFullURL) + szRelativeURL;
    m_szCurrFullURL = wxCURL_STRING2BUF(str);
}

wxString wxCurlBase::GetURL() const
{ 
    return wxCURL_BUF2STRING(m_szCurrFullURL);
}

void wxCurlBase::SetPort(const long& iPort)
{
    m_iHostPort = iPort;
}

long wxCurlBase::GetPort() const
{
    return m_iHostPort;
}

void wxCurlBase::SetUsername(const wxString& szUsername)
{
    m_szUsername = wxCURL_STRING2BUF(szUsername);
}

wxString wxCurlBase::GetUsername() const
{
    return wxCURL_BUF2STRING(m_szUsername);
}

void wxCurlBase::SetPassword(const wxString& szPassword)
{
    m_szPassword = wxCURL_STRING2BUF(szPassword);
}

wxString wxCurlBase::GetPassword() const
{
    return wxCURL_BUF2STRING(m_szPassword);
}

wxString wxCurlBase::GetResponseHeader() const
{
    return wxCURL_BUF2STRING(m_szResponseHeader);
}

wxString wxCurlBase::GetResponseBody() const
{
    return wxCURL_BUF2STRING(m_szResponseBody);
}

long wxCurlBase::GetResponseCode() const
{
    return m_iResponseCode;
}

wxString wxCurlBase::GetDetailedErrorString() const
{
    return wxString((const char*)m_szDetailedErrorBuffer, wxConvLibc);
}

wxString wxCurlBase::GetErrorString() const
{
    return wxCURL_BUF2STRING(m_szLastError);
}

void wxCurlBase::UseProxy(const bool& bUseProxy)
{
    m_bUseProxy = bUseProxy;
}

bool wxCurlBase::UseProxy() const
{
    return m_bUseProxy;
}

void wxCurlBase::SetProxyHost(const wxString& szProxyHost)
{
    m_szProxyHost = wxCURL_STRING2BUF(szProxyHost);
}

wxString wxCurlBase::GetProxyHost() const
{
    return wxCURL_BUF2STRING(m_szProxyHost);
}

void wxCurlBase::SetProxyUsername(const wxString& szProxyUsername)
{
    m_szProxyUsername = wxCURL_STRING2BUF(szProxyUsername);
}

wxString wxCurlBase::GetProxyUsername() const
{
    return wxCURL_BUF2STRING(m_szProxyUsername);
}

void wxCurlBase::SetProxyPassword(const wxString& szProxyPassword)
{
    m_szProxyPassword = wxCURL_STRING2BUF(szProxyPassword);
}

wxString wxCurlBase::GetProxyPassword() const
{
    return wxCURL_BUF2STRING(m_szProxyPassword);
}

void wxCurlBase::SetProxyPort(const long& iProxyPort)
{
    m_iProxyPort = iProxyPort;
}

long wxCurlBase::GetProxyPort() const
{
    return m_iProxyPort;
}

void wxCurlBase::SetVerbose(const bool& bVerbose)
{
    m_bVerbose = bVerbose;
}

bool wxCurlBase::IsVerbose() const
{
    return m_bVerbose;
}

bool wxCurlBase::GetVerboseStream(wxOutputStream& destStream) const
{
    if(m_bVerbose)
    {
        if(m_mosVerbose.IsOk())
        {
            size_t sz = m_mosVerbose.GetSize();
            wxString buffer = m_mosVerbose.GetString();

            destStream.Write(buffer.c_str(), sz);
            return destStream.IsOk();
        }
    }

    return false;
}

bool wxCurlBase::GetVerboseString(wxString& szStream) const
{
    if(m_bVerbose)
    {
        szStream.Append(m_mosVerbose.GetString());
        return true;
    }

    return false;
}

//////////////////////////////////////////////////////////////////////
// Helper Methods
//////////////////////////////////////////////////////////////////////

void wxCurlBase::SetCurlHandleToDefaults(const wxString& relativeURL)
{
    if (!relativeURL.empty())
        SetURL(relativeURL);        // update the m_szCurrFullURL string

    if(m_pCURL && ResetHandle())
    {
        ResetResponseVars();

        SetStringOpt(CURLOPT_URL, m_szCurrFullURL);

        SetOpt(CURLOPT_HEADERFUNCTION, wxcurl_header_func);
        SetOpt(CURLOPT_WRITEHEADER, &m_szResponseHeader);
        SetOpt(CURLOPT_ERRORBUFFER, m_szDetailedErrorBuffer);

        if(m_pEvtHandler && (m_nFlags & wxCURL_SEND_PROGRESS_EVENTS))
        {
            SetOpt(CURLOPT_NOPROGRESS, FALSE);
            SetOpt(CURLOPT_PROGRESSFUNCTION, m_progressCallback);
            SetOpt(CURLOPT_PROGRESSDATA, m_progressData);
        }

        if(!wxCURL_BUF_ISEMPTY(m_szUsername) || !wxCURL_BUF_ISEMPTY(m_szPassword))
        {
            wxString str = wxCURL_BUF2STRING(m_szUsername) + wxS(":") + wxCURL_BUF2STRING(m_szPassword);

            m_szUserPass = wxCURL_STRING2BUF(str);
            SetStringOpt(CURLOPT_USERPWD, m_szUserPass);

            SetOpt(CURLOPT_HTTPAUTH, CURLAUTH_ANY);
        }

        if(m_iHostPort != -1)
        {
            SetOpt(CURLOPT_PORT, m_iHostPort);
        }

        if(m_bUseProxy && !wxCURL_BUF_ISEMPTY(m_szProxyHost))
        {
            SetStringOpt(CURLOPT_PROXY, m_szProxyHost);
        }

        if(m_bUseProxy && (m_iProxyPort != -1))
        {
            SetOpt(CURLOPT_PROXYPORT, m_iProxyPort);
        }

        if(m_bUseProxy && (!wxCURL_BUF_ISEMPTY(m_szProxyUsername) || !wxCURL_BUF_ISEMPTY(m_szProxyPassword)))
        {
            wxString str = wxCURL_BUF2STRING(m_szProxyUsername) + wxS(":") + wxCURL_BUF2STRING(m_szProxyPassword);

            m_szProxyUserPass = wxCURL_STRING2BUF(str);
            SetStringOpt(CURLOPT_PROXYUSERPWD, m_szProxyUserPass);
        }

        if(m_bVerbose)
        {
            SetOpt(CURLOPT_VERBOSE, TRUE);
            SetOpt(CURLOPT_DEBUGFUNCTION, wxcurl_verbose_stream_write);
            SetOpt(CURLOPT_DEBUGDATA, (void*) &m_mosVerbose);
        }
    }
}

void wxCurlBase::SetHeaders()
{
    if(!m_arrHeaders.IsEmpty())
    {
        if(m_pHeaders)
        {
            curl_slist_free_all(m_pHeaders);

            m_pHeaders = NULL;

            SetOpt(CURLOPT_HTTPHEADER, NULL);
        }

        for(unsigned int i = 0; i < m_arrHeaders.Count(); i++)
        {
            m_pHeaders = curl_slist_append(m_pHeaders, wxCURL_STRING2BUF(m_arrHeaders[i]));
        }

        SetOpt(CURLOPT_HTTPHEADER, m_pHeaders);
    }
}

void wxCurlBase::ResetHeaders()
{
    m_arrHeaders.Clear();

    if(m_pHeaders)
    {
        curl_slist_free_all(m_pHeaders);

        m_pHeaders = NULL;

        SetOpt(CURLOPT_HTTPHEADER, NULL);
    }
}

void wxCurlBase::ResetResponseVars()
{
    m_szResponseHeader = "";
    m_szResponseBody = "";
    m_iResponseCode = -1;
}

//////////////////////////////////////////////////////////////////////
// Static Initialization and Shutdown Methods
//////////////////////////////////////////////////////////////////////

void wxCurlBase::Init()
{
    // Initialize LibCURL
    curl_global_init(CURL_GLOBAL_ALL);
}

void wxCurlBase::Shutdown()
{
    // Shutdown CurlLib
    curl_global_cleanup();
}

//////////////////////////////////////////////////////////////////////
// Static Utility Methods
//////////////////////////////////////////////////////////////////////

wxDateTime wxCurlBase::GetDateFromString(const wxString& szDate)
{
    time_t now = wxDateTime::Now().GetTicks();

    return wxDateTime(curl_getdate((const char*)(szDate.c_str()), &now));
}

wxString wxCurlBase::GetURLEncodedString(const wxString& szData)
{
    char* pszRetVal = curl_escape((const char*)(szData.c_str()), szData.Len());

    if(pszRetVal)
    {
        wxString szRetVal((const char*)pszRetVal, wxConvLibc);

        curl_free(pszRetVal);

        return szRetVal;
    }

    return wxEmptyString;
}

wxString wxCurlBase::GetStringFromURLEncoded(const wxString& szData)
{
    char* pszRetVal = curl_unescape((const char*)(szData.c_str()), szData.Len());

    if(pszRetVal)
    {
        wxString szRetVal = (wxChar*)pszRetVal;

        curl_free(pszRetVal);

        return szRetVal;
    }

    return wxEmptyString;
}

wxString wxCurlBase::GetCURLVersion()
{
    return wxString(curl_version(), wxConvUTF8);
}
