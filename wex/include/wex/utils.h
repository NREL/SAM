#ifndef __util_h
#define __util_h

#include <vector>
#include <wx/string.h>
#include <wx/arrstr.h>
#include <wx/dc.h>

std::vector<int> wxCommaDashListToIndices(const wxString &value);
wxString wxLimitTextColumns(const wxString &str, size_t numcols);

wxString wxConvertToBase26(unsigned int val);
unsigned int wxConvertFromBase26(const wxString &val);
wxArrayString wxEnumerateAlphaIndex(const wxString &_start, const wxString &_end);

wxString wxWebHttpGet(const wxString &url, 
					  const wxString &addtlhdr_name=wxEmptyString, 
					  const wxString &addtlhdr_value=wxEmptyString);

bool wxWebHttpDownload(const wxString &url, 
					   const wxString &local_file,
					   int timeout = 10 /*seconds*/, 
					   const wxString &mime = "application/binary",
					   bool with_progress_dialog=true, 
					   void (*callback)(int bytes, int total, void *data)=NULL, 
					   void *data=NULL);

bool wxDecompressFile(const wxString &archive, const wxString &target);
bool wxUnzipFile(const wxString &archive, const wxString &target);
bool wxUntarFile(const wxString &archive, const wxString &target);
bool wxGunzipFile(const wxString &archive, const wxString &target);


enum wxArrowType { wxARROW_UP, wxARROW_DOWN, wxARROW_LEFT, wxARROW_RIGHT };

int wxDrawWordWrappedText(wxDC& dc, const wxString &str, int width, bool draw=false, int x=0, int y=0, wxArrayString *lines=NULL);
void wxDrawRaisedPanel(wxDC &dc, int x, int y, int width, int height);
void wxDrawSunkenPanel(wxDC &dc, int x, int y, int width, int height);
void wxDrawEngravedPanel(wxDC &dc, int x, int y, int width, int height, bool fill);
void wxDrawScrollBar(wxDC &dc, bool vertical, int x, int y, int width, int height);
void wxDrawArrowButton(wxDC &dc, wxArrowType type, int x, int y, int width, int height);
void wxDrawArrow(wxDC &dc, wxArrowType type, int x, int y, int width, int height);


int wxShowTextMessageDialog(const wxString &text, 
	const wxString &title = wxEmptyString, 
	wxWindow *parent = 0, 
	const wxSize &size = wxDefaultSize, 
	long buttons = wxOK );


/* time/date helper functions for working with generic years */

// number of days in each month
extern int wxNDay[12];

// returns 3 letter name of month, for 1 <= mo <= 12
wxString wxMonthName( int mo );

/* month: 1-12 time: hours, starting 0=jan 1st 12am, returns 1..nday*/
int wxDayOfMonth(int month, double time);

/* returns month number 1..12 given 
	time: hour index in year 0..8759 */
int wxMonthOf(double time);

/* converts 'time' (hours since jan 1st 12am, 0 index) to M(1..12), D(1..N), H(0..23), M(0..59) */
void wxTimeToMDHM( double time, int *mo, int *dy, int *hr, int *min = 0 );

/* converts M(1..12), D(1..N), H(0..23) M(0..59) to time in hours since jan 1st 12am, 0 index ) */
double wxMDHMToTime( int mo, int dy, int hr, int min = 0);

/* format a MDHM time into a pretty string */
wxString wxFormatMDHM( int mo, int dy, int hr, int min = 0, bool use_12_hr = true );
wxString wxFormatTime( double time, bool use_12_hr = true );
wxString wxFormatTime( size_t istep, size_t steps_per_hour, bool use_12_hr = true );


// return the scaling level for the current display
double wxGetScreenHDScale();
void wxGetScreenHDScale( double *xs, double *ys );
void wxDevicePPIToScale( const wxSize &ppi, double *xs, double *ys );
double wxGetDrawingDPI();

wxPoint wxScalePoint( const wxPoint &p, double xs, double ys );
wxSize wxScaleSize( const wxSize &s, double xs, double ys );
inline wxSize wxScaleSize( const wxSize &s, double sf ) { return wxScaleSize( s, sf, sf ); }
inline wxSize wxScaleSize( const wxSize &s ) { return wxScaleSize( s, wxGetScreenHDScale() ); }
inline wxSize wxScaleSize( int width, int height ) { return wxScaleSize( wxSize(width, height) ); }
wxRect wxScaleRect( const wxRect &r, double xs, double ys );
inline wxRect wxScaleRect( const wxRect &r, double sf ) { return wxScaleRect( r, sf, sf ); }
inline wxRect wxScaleRect( const wxRect &r ) { return wxScaleRect( r, wxGetScreenHDScale() ); }
inline wxRect wxScaleRect( int x, int y, int width, int height ) { return wxScaleRect( wxRect(x,y,width,height) ); }


// sort (n^2) names and labels together
void wxSortByLabels(wxArrayString &names, wxArrayString &labels);

// create a transparent overlay frame
wxFrame *wxCreateTransparentOverlay( wxWindow *parent );


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
//! Utilities to calculate MD5 checksums from files or strings.
//! Returns the MD5 checksum for the given file
wxString wxGetFileMD5(wxInputStream &str);
wxString wxGetFileMD5(const wxString &filename);
//! Returns the MD5 for the given string.
wxString wxGetMD5(const wxString &str);


// On windows, make sure process is DPI aware, regardless
// of whether wxWidgets does this.  ref: http://trac.wxwidgets.org/ticket/16116
// We don't use built-in icons or AUI, and rather have clean lines and text
// rather than blurry look.
// Call this from wxApp::OnInit()
void wxMakeProcessDPIAware();

#endif

