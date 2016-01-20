#include <string>

#include <wx/wx.h>
#include <wx/ffile.h>
#include <wx/wfstream.h>
#include <wx/mstream.h>
#include <wx/buffer.h>
#include <wx/uri.h>
#include <wx/log.h>

#include <curl/curl.h>

#include <wex/jsonreader.h>

#include "simplecurl.h"

#ifdef __WXMSW__
#include <winhttp.h>
#endif


static wxString gs_curlProxyAddress;
static wxArrayString gs_curlProxyAutodetectMessages;

DEFINE_EVENT_TYPE(wxSIMPLECURL_EVENT);

extern "C" {
    int simplecurl_progress_func(void* ptr, double rDlTotal, double rDlNow, 
                                 double rUlTotal, double rUlNow);
    size_t simplecurl_stream_write(void* ptr, size_t size, size_t nmemb, void* stream);

}; // extern "C"

class wxSimpleCurl::DLThread : public wxThread
{
public:
	wxSimpleCurl *m_sc;
	wxString m_url, m_proxy;
	CURLcode m_resultCode;

	wxMemoryBuffer m_data;
	wxMutex m_dataLock;

	bool m_threadDone;
	wxMutex m_threadDoneLock;

	bool m_canceled;
	wxMutex m_canceledLock;

	DLThread( wxSimpleCurl *cobj, const wxString &url, const wxString &proxy )
		: wxThread( wxTHREAD_JOINABLE ),
			m_sc(cobj),
			m_url(url),
			m_proxy(proxy),
			m_resultCode( CURLE_OK ),
			m_threadDone(false),
			m_canceled(false)
	{
		

	}

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
			struct curl_slist *chunk = NULL;
 
			wxArrayString headers = m_sc->m_httpHeaders;

			for( size_t i=0;i<headers.size();i++ )
				chunk = curl_slist_append(chunk, (const char*)headers[i].c_str() );
			
			curl_easy_setopt(curl, CURLOPT_HTTPHEADER, chunk);

			wxURI uri( m_url );
			wxString encoded = uri.BuildURI();
			curl_easy_setopt(curl, CURLOPT_URL, (const char*)encoded.c_str());

			if ( !m_sc->m_postData.IsEmpty() )
				curl_easy_setopt( curl, CURLOPT_POSTFIELDS, (const char*)m_sc->m_postData.c_str() );
			
			curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0L);
			curl_easy_setopt(curl, CURLOPT_SSL_VERIFYHOST, 0L);

			curl_easy_setopt(curl, CURLOPT_WRITEDATA, this);
			curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, simplecurl_stream_write);
			
			curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 0 );
			curl_easy_setopt(curl, CURLOPT_PROGRESSDATA, this );
			curl_easy_setopt(curl, CURLOPT_PROGRESSFUNCTION, simplecurl_progress_func);	

			curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1);
			
			if ( !m_proxy.IsEmpty() )
			{
				wxURI uri_proxy( m_proxy );
				wxString encoded_proxy = uri_proxy.BuildURI();
				curl_easy_setopt(curl, CURLOPT_PROXY, (const char*)encoded_proxy.ToAscii() );
			}
			
			m_resultCode = curl_easy_perform(curl);
			curl_easy_cleanup(curl);
			
			/* free the custom headers */ 
			curl_slist_free_all(chunk);


			m_threadDoneLock.Lock();
			m_threadDone = true;
			m_threadDoneLock.Unlock();

			
			// issue finished event
			
			wxSimpleCurlEvent evt( m_sc->m_id, wxSIMPLECURL_EVENT, 
				wxSimpleCurlEvent::FINISHED, "finished", m_url );

			if ( m_sc->m_handler != 0 )
				wxPostEvent( m_sc->m_handler, evt );

			if ( m_sc->m_callback != 0 )
				(* (m_sc->m_callback) )( evt, m_sc->m_userData );
		}
		
		return 0;
	}


	bool FinishedOk() {
		return m_resultCode == CURLE_OK;
	}

	wxString GetError() {
		return wxString( curl_easy_strerror(m_resultCode) );
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

	void IssueProgressEvent( double rDlTotal, double rDlNow )
	{
		wxSimpleCurlEvent evt( m_sc->m_id, wxSIMPLECURL_EVENT, 
			wxSimpleCurlEvent::PROGRESS, "progress", m_url );
		evt.SetBytesTotal( rDlTotal );
		evt.SetBytesTransferred( rDlNow );

		if ( m_sc->m_handler != 0 )
			wxPostEvent( m_sc->m_handler, evt );

		if ( m_sc->m_callback != 0 )
			( *(m_sc->m_callback) )( evt, m_sc->m_userData );
	}
};

extern "C" {
    int simplecurl_progress_func(void* userp, double rDlTotal, double rDlNow, 
                                 double rUlTotal, double rUlNow)
    {
        wxSimpleCurl::DLThread *tt = static_cast<wxSimpleCurl::DLThread*>(userp);
        if(tt)
        {
			if (tt->IsCanceled())
				return -1;

			tt->IssueProgressEvent( rDlTotal, rDlNow );			
        }

        return 0;
    }

    size_t simplecurl_stream_write(void* ptr, size_t size, size_t nmemb, void* userp)
    {
        wxSimpleCurl::DLThread *tt = static_cast<wxSimpleCurl::DLThread*>(userp);
		if (tt) return tt->Write( ptr, size*nmemb );
        return 0;
    }

}; // extern "C"




wxSimpleCurl::wxSimpleCurl( wxEvtHandler *handler, int id )
	: m_thread( 0 ), m_handler( handler ), 
	  m_id( id ), m_callback(0), m_userData(0)
{
}

wxSimpleCurl::~wxSimpleCurl()
{
	if ( m_thread && !m_thread->IsDone() )
		Abort();

	if ( m_thread ) delete m_thread;
}

bool wxSimpleCurl::Start( const wxString &url, bool synchronous )
{
	if ( m_thread != 0 )
	{
		if ( IsStarted() && !Finished() )
		{
			m_thread->Cancel();
			m_thread->Wait();
		}

		delete m_thread;
	}

	m_thread = new DLThread( this, url, GetProxyForURL(url) );
	m_thread->Create();
	m_thread->Run();
	
	if ( synchronous )
	{
		while( 1 )
		{
			if ( IsStarted() && !Finished() ) wxMilliSleep( 50 );
			else break;
		}

		return m_thread->FinishedOk();
	}
	else
		return true;
}

void wxSimpleCurl::SetEventHandler( wxEvtHandler *hh, int id )
{
	m_handler = hh;
	m_id = id;
}

void wxSimpleCurl::SetCallback( void (*function)( wxSimpleCurlEvent &, void * ), void *user_data )
{
	m_callback = function;
	m_userData = user_data;
}

bool wxSimpleCurl::Ok()
{
	return (m_thread!=0 && m_thread->FinishedOk());
}

wxString wxSimpleCurl::GetLastError()
{
	return m_thread!=0 ? m_thread->GetError() : wxEmptyString;
}

bool wxSimpleCurl::IsStarted()
{
	return (m_thread!=0 && !m_thread->IsDone());
}

wxString wxSimpleCurl::GetDataAsString()
{
	return m_thread ? m_thread->GetDataAsString() : wxEmptyString;
}

wxImage wxSimpleCurl::GetDataAsImage( int bittype )
{
	return m_thread ? m_thread->GetDataAsImage( bittype ) : wxNullImage;
}

bool wxSimpleCurl::WriteDataToFile( const wxString &file )
{
	return m_thread ? m_thread->WriteDataToFile( file ) : false;
}

bool wxSimpleCurl::Finished()
{
	return (m_thread != 0 && m_thread->IsDone() && !m_thread->IsRunning() );
}

void wxSimpleCurl::Abort()
{
	if (m_thread != 0 && IsStarted() && !Finished())
	{
		m_thread->Cancel();
		m_thread->Wait();
	}
}

void wxSimpleCurl::SetProxyAddress( const wxString &proxy )
{
	gs_curlProxyAddress = proxy;
}


static std::string wstr2str( const wchar_t *s )
{//Unicode to Punycode convertor! Someone? (or simply ignore the IDNA decision from 2003 :):
	std::string ret;
	for( ; 0 != *s; ++s )
		ret += char( *s );
	return ret;
}

static std::wstring str2wstr( const std::string &s )
{//Same comment as in wstr2str()
	std::wstring ret;
	for( std::string::const_iterator i = s.begin(); i != s.end(); ++i )
		ret += wchar_t( *i );
	return ret;
}

wxString wxSimpleCurl::GetProxyForURL( const wxString &url )
{
	wxString proxy( gs_curlProxyAddress );

#ifdef __WXMSW__
	
	if ( proxy.IsEmpty() )
	{
		gs_curlProxyAutodetectMessages.Clear();

		// try to autodetect proxy address for url, at least on windows
		HINTERNET hInter = ::WinHttpOpen( L"", WINHTTP_ACCESS_TYPE_DEFAULT_PROXY, WINHTTP_NO_PROXY_NAME, WINHTTP_NO_PROXY_BYPASS, 0 );
	
		gs_curlProxyAutodetectMessages.Add("Querying IE proxy settings...");

		WINHTTP_CURRENT_USER_IE_PROXY_CONFIG cfg;
		memset( &cfg, 0, sizeof(WINHTTP_CURRENT_USER_IE_PROXY_CONFIG) );
		if( ::WinHttpGetIEProxyConfigForCurrentUser( &cfg ) )
		{
			printf("autoDetect? %s\n", cfg.fAutoDetect ? "yes" : "no" );

			if ( cfg.lpszAutoConfigUrl )
				gs_curlProxyAutodetectMessages.Add("lpszAutoConfigUrl: " + wxString(cfg.lpszAutoConfigUrl) );
			
			if ( cfg.lpszProxy )
				gs_curlProxyAutodetectMessages.Add("lpszProxy: " + wxString(cfg.lpszProxy) );

			if ( cfg.lpszProxyBypass )
				gs_curlProxyAutodetectMessages.Add("lpszProxyBypass: " + wxString(cfg.lpszProxyBypass) );
		}
		else
		{
			gs_curlProxyAutodetectMessages.Add("Could not get IE proxy settings");

		}

		LPWSTR autoCfgUrl = cfg.lpszAutoConfigUrl;
		if ( cfg.fAutoDetect || autoCfgUrl )
		{
			WINHTTP_AUTOPROXY_OPTIONS autoOpts;
			memset( &autoOpts, 0, sizeof(WINHTTP_AUTOPROXY_OPTIONS) );
			autoOpts.fAutoLogonIfChallenged = TRUE;
			if( cfg.fAutoDetect )
			{
				autoOpts.dwAutoDetectFlags = WINHTTP_AUTO_DETECT_TYPE_DHCP | WINHTTP_AUTO_DETECT_TYPE_DNS_A;
				autoOpts.dwFlags = WINHTTP_AUTOPROXY_AUTO_DETECT;
			}
			if( autoCfgUrl )
			{
				autoOpts.lpszAutoConfigUrl = autoCfgUrl;
				autoOpts.dwFlags |= WINHTTP_AUTOPROXY_CONFIG_URL;
			}

			gs_curlProxyAutodetectMessages.Add( "Querying auto configuration proxy for url: " + url );

			WINHTTP_PROXY_INFO autoCfg;
			memset( &autoCfg, 0, sizeof(WINHTTP_PROXY_INFO) );
			if ( ::WinHttpGetProxyForUrl( hInter, url.ToStdWstring().c_str(), 
					&autoOpts, &autoCfg )
				&& WINHTTP_ACCESS_TYPE_NO_PROXY != autoCfg.dwAccessType )
			{
				if ( autoCfg.lpszProxy )
				{
					gs_curlProxyAutodetectMessages.Add( "autoCfg.lpszProxy: " + wxString( autoCfg.lpszProxy ) );
					proxy = wstr2str( autoCfg.lpszProxy );
				}
				else
					gs_curlProxyAutodetectMessages.Add("No autodetected proxy determined");
			}
			else
			{
				gs_curlProxyAutodetectMessages.Add("Connection method does not use a proxy");
			}

		}
		else
		{
			gs_curlProxyAutodetectMessages.Add("Proxy autodetection disabled or no autoconfiguration url found");

			if ( cfg.lpszProxy )
			{
				gs_curlProxyAutodetectMessages.Add("Using default: " + wxString(cfg.lpszProxy) );
				proxy = wxString( cfg.lpszProxy );
			}
		}

		::WinHttpCloseHandle( hInter );
	}
#endif

	return proxy;
}

wxArrayString wxSimpleCurl::GetProxyAutodetectMessages()
{
	return gs_curlProxyAutodetectMessages;
}

void wxSimpleCurl::Init()
{
	::curl_global_init( CURL_GLOBAL_ALL );
}
void wxSimpleCurl::Shutdown()
{
	::curl_global_cleanup();
}



// Google APIs:
// login to developer api console at: https://code.google.com/apis/console
// user name: aron.dobos@nrel.gov
// passwd: 1Ho************r
static wxString GOOGLE_API_KEY("AIzaSyCyH4nHkZ7FhBK5xYg4db3K7WN-vhpDxas");

// Bing Map APIs:
// login to account center at: https://www.bingmapsportal.com/
// user name: aron.dobos@nrel.gov
// passwd: 1Ho************r
static wxString BING_API_KEY("Av0Op8DvYGR2w07w_771JLum7-fdry0kBtu3ZA4uu_9jBJOUZgPY7mdbWhVjiORY");


bool wxSimpleCurl::GeoCode( const wxString &address, double *lat, double *lon, double *tz)
{
	wxBusyCursor _curs;
	bool latlonok = false;

	wxString plusaddr = address;
	plusaddr.Replace("   ", " ");
	plusaddr.Replace("  ", " ");
	plusaddr.Replace(" ", "+");
	
	wxString url( "https://maps.googleapis.com/maps/api/geocode/json?address=" + plusaddr + "&sensor=false&key=" + GOOGLE_API_KEY );
	
	wxSimpleCurl curl;
	wxBusyCursor curs;
	if ( !curl.Start( url, true ) )
		return false;

	wxJSONReader reader;
	wxJSONValue root;
	if (reader.Parse( curl.GetDataAsString(), &root )==0)
	{
		wxJSONValue loc = root.Item("results").Item(0).Item("geometry").Item("location");
		if (!loc.IsValid()) return false;
		*lat = loc.Item("lat").AsDouble();
		*lon = loc.Item("lng").AsDouble();
		
		if ( root.Item("status").AsString() != "OK" )
			return false;
	}
	else
		return false;

	if ( tz != 0 )
	{
		// get timezone from another service
		url = wxString::Format("https://maps.googleapis.com/maps/api/timezone/json?location=%.14lf,%.14lf&timestamp=1&sensor=false&key=",
			*lat, *lon) + GOOGLE_API_KEY;
		curl.Start( url, true );
		if (reader.Parse( curl.GetDataAsString(), &root )==0)
		{
			wxJSONValue val = root.Item("rawOffset");
			if ( val.IsDouble() ) *tz = val.AsDouble() / 3600.0;
			else *tz = val.AsInt() / 3600.0;

			return root.Item("status").AsString() == "OK";
		}
		else
			return false;
	} // if no tz argument given then return true
	else return true;

}

wxBitmap wxSimpleCurl::StaticMap( double lat, double lon, int zoom, MapProvider service )
{
	if ( zoom > 21 ) zoom = 21;
	if ( zoom < 1 ) zoom = 1;
	wxString zoomStr = wxString::Format("%d", zoom );
		

	wxString url;
	if ( service == GOOGLE_MAPS )
	{
		url = "https://maps.googleapis.com/maps/api/staticmap?center=" 
			+ wxString::Format("%.9lf,%.9lf", lat, lon) + "&zoom=" + zoomStr 
			+ "&size=800x800&maptype=hybrid&sensor=false&format=jpg-baseline&key=" + GOOGLE_API_KEY;
	}
	else
	{
		url = "http://dev.virtualearth.net/REST/v1/Imagery/Map/Aerial/"
			+ wxString::Format("%.15lf,%.15lf/%d", lat, lon, zoom)
			+ "?mapSize=800,800&format=jpeg&key=" + BING_API_KEY;
	}

	wxSimpleCurl curl;
	curl.Start( url, true );
	return wxBitmap( curl.GetDataAsImage(wxBITMAP_TYPE_JPEG) );
}
