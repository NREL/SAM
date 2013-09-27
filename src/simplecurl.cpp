
#include <curl/curl.h>

#include "simplecurl.h"
#include <wx/wx.h>

DEFINE_EVENT_TYPE(wxSIMPLECURL_EVENT);

extern "C" {
    int simplecurl_progress_func(void* ptr, double rDlTotal, double rDlNow, 
                                 double rUlTotal, double rUlNow);
    size_t simplecurl_stream_write(void* ptr, size_t size, size_t nmemb, void* stream);

}; // extern "C"
class wxSimpleCurlDownloadThread::DLThread : public wxThread
{
private:
	wxEvtHandler *m_handler;
	int m_id;
	wxSimpleCurlDownloadThread *m_simpleCurl;
	wxString m_url;
	CURLcode m_resultCode;

	wxString m_data;
	wxMutex m_dataLock;
	wxStringOutputStream m_stream;

	bool m_threadDone;
	wxMutex m_threadDoneLock;

	bool m_canceled;
	wxMutex m_canceledLock;

public:
	DLThread( wxEvtHandler *handler, int id, 
		wxSimpleCurlDownloadThread *cobj, const wxString &url )
		: wxThread( wxTHREAD_JOINABLE ),
			m_handler(handler),
			m_id(id),
			m_simpleCurl(cobj),
			m_url(url),
			m_resultCode( CURLE_OK ),
			m_stream(&m_data),
			m_threadDone(false),
			m_canceled(false)
	{
	}

	void SetUrl( const wxString &url ) { m_url = url; }
	int GetHandlerId() { return m_id; }
	wxEvtHandler *GetEvtHandler() { return m_handler; }
	wxString GetUrl() { return m_url; }


	size_t Write( void *p, size_t len )
	{
		size_t last = 0;
		m_dataLock.Lock();
		m_stream.Write(p, len);
		last = m_stream.LastWrite();
		m_dataLock.Unlock();
		return last;
	}

	wxString GetData() {
		wxString d;
		m_dataLock.Lock();
		d = m_data;
		m_dataLock.Unlock();
		return d;
	}

	virtual void *Entry()
	{
		if( CURL *curl = curl_easy_init() )
		{
			curl_easy_setopt(curl, CURLOPT_URL, (const char*)m_url.c_str());
			
			curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0L);
			curl_easy_setopt(curl, CURLOPT_SSL_VERIFYHOST, 0L);

			curl_easy_setopt(curl, CURLOPT_WRITEDATA, this);
			curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, simplecurl_stream_write);
			
			curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 0 );
			curl_easy_setopt(curl, CURLOPT_PROGRESSDATA, this );
			curl_easy_setopt(curl, CURLOPT_PROGRESSFUNCTION, simplecurl_progress_func);	

			curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1);

			m_resultCode = curl_easy_perform(curl);
			curl_easy_cleanup(curl);

			// issue finished event
			wxSimpleCurlEvent evt( GetHandlerId(), wxSIMPLECURL_EVENT, 
				wxSimpleCurlEvent::FINISHED, "finished", m_url );
			wxPostEvent( GetEvtHandler(), evt );

			m_threadDoneLock.Lock();
			m_threadDone = true;
			m_threadDoneLock.Unlock();
		}
		
		return 0;
	}

	bool IsDone()
	{
		m_threadDoneLock.Lock();
		bool done = m_threadDone;
		m_threadDoneLock.Unlock();
		return done;
	}

	bool IsCanceled(){
		wxMutexLocker _ml( m_canceledLock );
		return m_canceled;
	}
	void Cancel()
	{
		wxMutexLocker _ml( m_canceledLock );
		m_canceled = true;
	}

	virtual bool TestDestroy() { return IsCanceled(); }
};



wxSimpleCurlDownloadThread::wxSimpleCurlDownloadThread( wxEvtHandler *handler, int id )
{
	m_thread = new DLThread( handler, id, this, wxEmptyString );
}

wxSimpleCurlDownloadThread::~wxSimpleCurlDownloadThread()
{
	if ( IsStarted() && !Finished())
		Abort();

	delete m_thread;
}

void wxSimpleCurlDownloadThread::Start( const wxString &url )
{
	m_thread->SetUrl( url );
	m_thread->Create();
	m_thread->Run();
	m_started = true;
}

bool wxSimpleCurlDownloadThread::IsStarted()
{
	return m_started;
}

wxString wxSimpleCurlDownloadThread::GetData()
{
	return m_thread->GetData();
}

bool wxSimpleCurlDownloadThread::Finished()
{
	return m_thread->IsDone() && !m_thread->IsRunning();
}

void wxSimpleCurlDownloadThread::Abort()
{
	if (IsStarted() && !Finished())
	{
		m_thread->Cancel();
		m_thread->Wait();
	}
}

extern "C" {
    int simplecurl_progress_func(void* userp, double rDlTotal, double rDlNow, 
                                 double rUlTotal, double rUlNow)
    {
        wxSimpleCurlDownloadThread::DLThread *tt = static_cast<wxSimpleCurlDownloadThread::DLThread*>(userp);
        if(tt)
        {
            if (rUlTotal == 0 || rUlNow == 0)
            {
				wxSimpleCurlEvent evt( tt->GetHandlerId(), wxSIMPLECURL_EVENT, 
					wxSimpleCurlEvent::PROGRESS, "progress", tt->GetUrl() );
				evt.SetBytesTotal( rDlTotal );
				evt.SetBytesTransferred( rDlNow );
				wxPostEvent( tt->GetEvtHandler(), evt );
            }
			
			if (tt->IsCanceled())
			{
				// cancel thread
				return -1;
			}
        }

        return 0;
    }

    size_t simplecurl_stream_write(void* ptr, size_t size, size_t nmemb, void* userp)
    {
        wxSimpleCurlDownloadThread::DLThread *tt = static_cast<wxSimpleCurlDownloadThread::DLThread*>(userp);
		if (tt) return tt->Write( ptr, size*nmemb );
        return 0;
    }

}; // extern "C"


void wxSimpleCurlInit()
{
	::curl_global_init( CURL_GLOBAL_ALL );
}
void wxSimpleCurlShutdown()
{
	::curl_global_cleanup();
}