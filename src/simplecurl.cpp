
#include <curl/curl.h>
#include <wx/ffile.h>
#include <wx/wfstream.h>
#include <wx/mstream.h>
#include <wx/buffer.h>

#include "simplecurl.h"
#include <wx/wx.h>

static wxString gs_curlProxyAddress;

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
	wxString m_post;
	CURLcode m_resultCode;

	wxMemoryBuffer m_data;
	wxMutex m_dataLock;

	bool m_threadDone;
	wxMutex m_threadDoneLock;

	bool m_canceled;
	wxMutex m_canceledLock;

public:
	DLThread( wxEvtHandler *handler, int id, 
		wxSimpleCurlDownloadThread *cobj )
		: wxThread( wxTHREAD_JOINABLE ),
			m_handler(handler),
			m_id(id),
			m_simpleCurl(cobj),
			m_resultCode( CURLE_OK ),
			m_threadDone(false),
			m_canceled(false)
	{
		

	}
	

	void SetUrl( const wxString &url ) { m_url = url; }
	void SetPost( const wxString &post ) { m_post = post; }
	int GetHandlerId() { return m_id; }
	wxEvtHandler *GetEvtHandler() { return m_handler; }
	wxString GetUrl() { return m_url; }


	size_t Write( void *p, size_t len )
	{
		m_dataLock.Lock();
		m_data.AppendData( p, len );
		m_dataLock.Unlock();
		return len;
	}

	wxString GetDataAsString()
	{
		wxString d;
		m_dataLock.Lock();
		
		wxStringOutputStream sstream(&d);
		sstream.Write( m_data.GetData(), m_data.GetDataLen() );

		m_dataLock.Unlock();
		return d;
	}

	wxImage GetDataAsImage( int bittype )
	{
		m_dataLock.Lock();
		wxMemoryInputStream stream( m_data.GetData(), m_data.GetDataLen() );
		wxImage img;
		img.LoadFile(stream, bittype);
		m_dataLock.Unlock();
		return img;
	}

	bool WriteDataToFile( const wxString &file )
	{
		m_dataLock.Lock();
		wxFFileOutputStream ff( file, "wb" );
		if ( ff.IsOk() )
			ff.Write( m_data.GetData(), m_data.GetDataLen() );
		m_dataLock.Unlock();
		return ff.IsOk();
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

			if ( !gs_curlProxyAddress.IsEmpty() )
				curl_easy_setopt(curl, CURLOPT_PROXY, (const char*)gs_curlProxyAddress.ToAscii() );

			if ( !m_post.IsEmpty() )
				curl_easy_setopt( curl, CURLOPT_POSTFIELDS, (const char*)m_post.ToAscii() );

			m_resultCode = curl_easy_perform(curl);
			curl_easy_cleanup(curl);


			m_threadDoneLock.Lock();
			m_threadDone = true;
			m_threadDoneLock.Unlock();

			// issue finished event
			wxSimpleCurlEvent evt( GetHandlerId(), wxSIMPLECURL_EVENT, 
				wxSimpleCurlEvent::FINISHED, "finished", m_url );
			wxPostEvent( GetEvtHandler(), evt );
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
	: m_handler( handler ), m_id( id ), m_thread( 0 )
{
	m_thread = 0;
}

wxSimpleCurlDownloadThread::~wxSimpleCurlDownloadThread()
{
	if ( m_thread && !m_thread->IsDone() )
		Abort();

	if ( m_thread ) delete m_thread;
}

void wxSimpleCurlDownloadThread::Start( const wxString &url, const wxString &post )
{
	if ( m_thread != 0 ) delete m_thread;

	m_thread = new DLThread( m_handler, m_id, this );
	m_thread->SetUrl( url );
	if ( ! post.IsEmpty() ) m_thread->SetPost( post );
	m_thread->Create();
	m_thread->Run();
}

bool wxSimpleCurlDownloadThread::IsStarted()
{
	return (m_thread!=0 && !m_thread->IsDone());
}

wxString wxSimpleCurlDownloadThread::GetDataAsString()
{
	return m_thread->GetDataAsString();
}

wxImage wxSimpleCurlDownloadThread::GetDataAsImage( int bittype )
{
	return m_thread->GetDataAsImage( bittype );
}

bool wxSimpleCurlDownloadThread::WriteDataToFile( const wxString &file )
{
	return m_thread->WriteDataToFile( file );
}

bool wxSimpleCurlDownloadThread::Finished()
{
	return (m_thread != 0 && m_thread->IsDone() && !m_thread->IsRunning() );
}

void wxSimpleCurlDownloadThread::Abort()
{
	if (m_thread != 0 && IsStarted() && !Finished())
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


void wxSimpleCurlSetupProxy( const wxString &proxy )
{
	gs_curlProxyAddress = proxy;
}

void wxSimpleCurlInit()
{
	::curl_global_init( CURL_GLOBAL_ALL );
}
void wxSimpleCurlShutdown()
{
	::curl_global_cleanup();
}