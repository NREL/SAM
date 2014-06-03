#ifndef __simplecurl_h
#define __simplecurl_h

#include <wx/event.h>
#include <wx/datetime.h>
#include <wx/sstream.h>


BEGIN_DECLARE_EVENT_TYPES()
    DECLARE_EVENT_TYPE(wxSIMPLECURL_EVENT, 7578)
END_DECLARE_EVENT_TYPES()

class wxSimpleCurlEvent : public wxEvent
{
public:
	enum { STARTED, PROGRESS, FINISHED };

    wxSimpleCurlEvent(int id, wxEventType type, int code, const wxString &msg = wxEmptyString, const wxString &url = wxEmptyString )
        : wxEvent(id, type) { m_code = code; m_msg = msg; m_dt = wxDateTime::Now(); m_url = url; m_bytes = m_total = 0; }
	wxSimpleCurlEvent( const wxSimpleCurlEvent &evt )
		: wxEvent( evt.GetId(), evt.GetEventType() ),
		m_code(evt.m_code),
		m_msg(evt.m_msg),
		m_dt(evt.m_dt),
		m_url(evt.m_url),
		m_bytes(evt.m_bytes),
		m_total(evt.m_total)
	{ }

	int GetStatusCode() const { return m_code; }
    wxString GetMessage() const { return m_msg; }	
    virtual wxEvent* Clone() const { return new wxSimpleCurlEvent(*this); }
	wxString GetUrl() const { return m_url; }
	double GetBytesTransferred() const { return m_bytes; }
	double GetBytesTotal() const { return m_total; }

	void SetBytesTransferred( double b ) { m_bytes = b; }
	void SetBytesTotal( double t ) { m_total = t; }

protected:
	int m_code;
	wxString m_msg;
    wxDateTime m_dt;
	wxString m_url;
	double m_bytes, m_total;
};

typedef void (wxEvtHandler::*wxSimpleCurlEventFunction)(wxSimpleCurlEvent&);

#define wxSimpleCurlEventFunction(func) \
    (wxObjectEventFunction)(wxEventFunction)wxStaticCastEvent(wxSimpleCurlEventFunction, &func)

#define EVT_SIMPLECURL(id, fn) \
    wx__DECLARE_EVT1(wxSIMPLECURL_EVENT, id, wxSimpleCurlEventFunction(fn))

class wxSimpleCurlDownloadThread : public wxObject
{
public:
	class DLThread;

	wxSimpleCurlDownloadThread( wxEvtHandler *handler = 0, int id = wxID_ANY );
	virtual ~wxSimpleCurlDownloadThread();

	void Start( const wxString &url, const wxString &post = wxEmptyString );
	wxString GetDataAsString();
	wxImage GetDataAsImage( int bittype = wxBITMAP_TYPE_JPEG );
	bool WriteDataToFile( const wxString &file );

	bool Finished();
	void Abort();

	DLThread *GetThread();
	int GetId();
	wxEvtHandler *GetEvtHandler();
	wxString GetUrl();

	bool IsStarted();

protected:
	DLThread *m_thread;
	wxEvtHandler *m_handler;
	int m_id;
};

// app-wide init and shutdown calls for underlying libcurl initialization
void wxSimpleCurlSetupProxy( const wxString &proxy );
void wxSimpleCurlInit();
void wxSimpleCurlShutdown();

#endif

