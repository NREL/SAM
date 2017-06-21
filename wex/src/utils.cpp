#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <cmath>

// C headers for MD5
#include <sys/types.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include <wx/wx.h>
#include <wx/busyinfo.h>
#include <wx/tokenzr.h>
#include <wx/zipstrm.h>
#include <wx/txtstrm.h>
#include <wx/dir.h>
#include <wx/wfstream.h>
#include <wx/sstream.h>
#include <wx/protocol/http.h>
#include <wx/uri.h>
#include <wx/progdlg.h>
#include <wx/tarstrm.h>
#include <wx/zstream.h>

#include "wex/utils.h"
/*
bool AllocReadLine(FILE *fp, wxString &buf, int prealloc)
{
	char c;

	buf = "";
	if (prealloc > 10)
		buf.Alloc( prealloc );

	// read the whole line, 1 character at a time, no concern about buffer length
	while ( (c=fgetc(fp)) != EOF && c != '\n' && c != '\r')
		buf += c;

	// handle windows <CR><LF>
	if (c == '\r')
	{
		if ( (c=fgetc(fp)) != '\n')
			ungetc(c,fp);
	}

	// handle a stray <CR>
	if (c == '\n')
	{
		if ( (c=fgetc(fp)) != '\r')
			ungetc(c,fp);
	}

	return !(buf.Len() == 0 && c == EOF);
}
*/

wxString wxLimitTextColumns(const wxString &str, size_t numcols)
{
	wxString buf;
	size_t len = (int)str.Len();
	size_t col=0;
	for (size_t i=0;i<len;i++)
	{
		if (col == numcols)
		{
			while (i < len && str[i] != ' ' && str[i] != '\t' && str[i] != '\n')
			{
				buf += str[i];
				i++;
			}
			
			while (i < len && (str[i] == ' ' || str[i] == '\t'))
				i++;

			if (i<len)
				buf += '\n';
			col = 0;
			i--;
		}
		else
		{
			buf += str[i];

			if (str[i] == '\n')
				col = 0;
			else
				col++;
		}
	}

	return buf;
}
/*
void SortByLabels(wxArrayString &names, wxArrayString &labels)
{
	// sort the selections by labels
	wxString buf;
	int count = (int)labels.Count();
	for (int i=0;i<count-1;i++)
	{
		int smallest = i;

		for (int j=i+1;j<count;j++)
			if ( labels[j] < labels[smallest] )
				smallest = j;

		// swap
		buf = labels[i];
		labels[i] = labels[smallest];
		labels[smallest] = buf;

		buf = names[i];
		names[i] = names[smallest];
		names[smallest] = buf;

	}
}

void SortByValues(wxArrayString &names, Vector<double> &values, bool useabs)
{
	if (names.Count() != values.length())
		return;

	// sort the selections by values
	wxString buf;
	double dval;
	int count = (int)values.length();
	for (int i=0;i<count-1;i++)
	{
		int smallest = i;

		for (int j=i+1;j<count;j++)
		{
			if (useabs)
			{
				if ( fabs(values[j]) < fabs(values[smallest]) )
					smallest = j;
			}
			else
			{
				if ( values[j] < values[smallest] )
					smallest = j;
			}
		}

		// swap
		dval = values[i];
		values[i] = values[smallest];
		values[smallest] = dval;

		buf = names[i];
		names[i] = names[smallest];
		names[smallest] = buf;

	}
}

void SortByValues(Vector<double> &sort_also, Vector<double> &values, bool useabs)
{
	if (sort_also.length() != values.length())
		return;

	// sort the selections by values
	double buf;
	double dval;
	int count = (int)values.length();
	for (int i=0;i<count-1;i++)
	{
		int smallest = i;

		for (int j=i+1;j<count;j++)
		{
			if (useabs)
			{
				if ( fabs(values[j]) < fabs(values[smallest]) )
					smallest = j;
			}
			else
			{
				if ( values[j] < values[smallest] )
					smallest = j;
			}
		}

		// swap
		dval = values[i];
		values[i] = values[smallest];
		values[smallest] = dval;

		buf = sort_also[i];
		sort_also[i] = sort_also[smallest];
		sort_also[smallest] = buf;

	}
}

*/


	/*
bool DeleteDirectory(const wxString &path, void (*cb)(const wxString &fn, void *data), void *data)
{
	// Can't use a stack allocated copy b/c 
	// wxWidgest doesn't provide a CloseDir method.
	// Must close the dir by deleting object
	// at end of function to be able to actually
	// remove the directory
	wxDir *dir = new wxDir( path );
	
	//applog("deldir: %s\n", path.c_str());
	if (!dir->IsOpened())
	{
		delete dir;
		return false;
	}
	
	// preload the file names - otherwise GetNext fails on unix/mac
	// because deleting stuff changes the dir contents
	wxString f;
	wxArrayString flist;
	bool hasmore = dir->GetFirst(&f);
	while (hasmore)
	{
		flist.Add( path + "/" + f );
		hasmore = dir->GetNext(&f);
	}
	
	delete dir; // closes the directory so we can remove at the end of the function

	for (int i=0;i<(int)flist.Count();i++)
	{
		f = flist[i];
		if ( wxDirExists(f) )
		{
			if (cb)	(*cb)( f, data );
			DeleteDirectory( f, cb, data);
		}
		else
		{
			if (cb)	(*cb)( f, data );
			wxRemoveFile( f );
		}
	}

	
	return wxRmdir( path );
}

wxULongLong DirectorySize(const wxString &path, int *count)
{
	wxDir dir( path );
	
	if (!dir.IsOpened())
		return 0;
	
	wxULongLong total=0,cur = 0;
	wxString f,item;
	bool hasmore = dir.GetFirst(&f);
	while (hasmore)
	{
		item = path + "/" + f;

		if ( wxDirExists(item) )
		{
			total += DirectorySize( item, count );
			if (count) (*count)++;
		}
		else
		{
			cur = wxFileName::GetSize( item );
			if (count) (*count)++;
			if (cur != wxInvalidSize)
				total += cur;
		}

		hasmore = dir.GetNext(&f);
	}
	
	return total;
}
*/

#define ALPHA_MIN 'a'
#define ALPHA_MAX 'z'
#define LASTCHAR(x) tolower(x[x.Len()-1])

wxString wxConvertToBase26(unsigned int val)
{
	wxString result;
	do
	{
		result.Prepend((char)( (val-1) % 26 + 'A'));
		val = (val-1)/26;
	}while(val>0);
	return result;
}

unsigned int wxConvertFromBase26(const wxString &val)
{
	unsigned int result = 0;
	const char *cval = val.c_str();
	
	while (cval && *cval)
		result = result*26 + toupper(*cval++)-'A'+1;

	return result;
}

wxArrayString wxEnumerateAlphaIndex(const wxString &_start, const wxString &_end)
{
	unsigned int istart = wxConvertFromBase26(_start);
	unsigned int iend = wxConvertFromBase26(_end);
	
	wxArrayString values;
	while (istart <= iend)
	{
		values.Add( wxConvertToBase26(istart) );
		istart++;
	}
	return values;
}

wxString wxWebHttpGet(const wxString &url, const wxString &addtlhdr_name, const wxString &addtlhdr_value)
{
	wxString server, file;

	wxURI uri(url);

	if (uri.GetScheme().Lower() != "http") return wxEmptyString;

	server = uri.GetServer();
	file = uri.GetPath();

	if (uri.HasQuery())
		file += "?" + uri.GetQuery();

	if (uri.HasFragment())
		file += "#" + uri.GetFragment();

	int port = 80;

	if (uri.HasPort())
		port = atoi( uri.GetPort().c_str() );

	wxHTTP get;
	get.SetHeader("Content-type", "text/plain");
	if (!addtlhdr_name.IsEmpty() && !addtlhdr_value.IsEmpty())
		get.SetHeader( addtlhdr_name, addtlhdr_value );

	get.SetTimeout(9);

	int ntries_left = 3;
	while (!get.Connect( server, port ) && ntries_left)
	{
		ntries_left--;
		wxSleep(1);
	}

	//if (!get.IsConnected()) return wxEmptyString;

	if (!wxApp::IsMainLoopRunning())
		return wxEmptyString;

	wxString result;

	wxInputStream *httpStream = get.GetInputStream( file );
	if (get.GetError() == wxPROTO_NOERR)
	{
		wxStringOutputStream out_stream(&result);
		httpStream->Read(out_stream);
	}

	wxDELETE(httpStream);
	get.Close();

	return result;
}

#define NRWBUFBYTES 4096

bool wxWebHttpDownload(const wxString &url, const wxString &local_file, 
					   int timeout,
				const wxString &mime,
				  bool with_progress_dialog, 
				  void (*callback)(int bytes, int total, void *data), void *data)
{
	wxString server, file;

	wxURI uri(url);

	if (uri.GetScheme().Lower() != "http") return false;

	server = uri.GetServer();
	file = uri.GetPath();

	if (uri.HasQuery())
		file += "?" + uri.GetQuery();

	if (uri.HasFragment())
		file += "#" + uri.GetFragment();

	int port = 80;

	if (uri.HasPort())
		port = atoi( uri.GetPort().c_str() );

	wxHTTP get;
	get.SetHeader("Content-type", mime);
	get.SetTimeout( timeout );

	int ntries_left = 3;
	while (!get.Connect( server, port ) && ntries_left)
	{
		ntries_left--;
		wxSleep(1);
	}

	//if (!get.IsConnected()) return false;

	if (!wxApp::IsMainLoopRunning())
		return false;

	wxInputStream *httpStream = get.GetInputStream(file);
	bool ok = false;
	if (get.GetError() == wxPROTO_NOERR)
	{
		int ntotal = httpStream->GetSize();

		if (ntotal < 1) ntotal = -1000;

		char rwbuf[NRWBUFBYTES];

		wxFileOutputStream fout( local_file );
		if (fout.IsOk())		
		{
			wxProgressDialog *prog = NULL;

			if (with_progress_dialog)
			{
				prog = new wxProgressDialog( "HTTP Download", url, abs(ntotal) );
				prog->Show();
				wxSafeYield( prog, true );
			}

			int ndown = 0;
			int yieldreq = 0;
			while(!httpStream->Eof())
			{
				int nread;
				httpStream->Read(rwbuf, NRWBUFBYTES);
				nread = httpStream->LastRead();
				fout.Write(rwbuf, nread);

				if (nread > 0) ndown += nread;

				if (callback) (*callback)(nread, ntotal, data);
				if (prog) 
				{
					if (ntotal > 0)
						prog->Update( ndown );
					else
						prog->Pulse();

					if (++yieldreq % 5 == 0) wxSafeYield( prog, true );
				}
			}

			ok = true;

			if (prog) prog->Destroy();
		}
	}

	return ok;

}

bool wxUnzipFile(const wxString &archive, const wxString &target)
{
	char rwbuf[NRWBUFBYTES];
	wxFFileInputStream in( archive );
	if (!in.IsOk()) return false;

	wxZipInputStream zip(in);
	if (!zip.IsOk()) return false;

	wxZipEntry *zf = zip.GetNextEntry();
	while (zf)
	{
		wxString fn = target + "/" + zf->GetName();
		if (!zf->IsDir())
		{
			// create the directory if needed
			wxString dirpath = wxPathOnly(fn);
			if (!wxDirExists(dirpath))
				wxFileName::Mkdir( dirpath, 511, wxPATH_MKDIR_FULL );

			wxFFileOutputStream out( fn );
			if (!out.IsOk())
			{
				wxDELETE(zf);
				return false;
			}

			while (!zip.Eof())
			{
				zip.Read(rwbuf, NRWBUFBYTES);
				out.Write(rwbuf, zip.LastRead());
			}				
		}
		wxDELETE(zf);
		zf = zip.GetNextEntry();
	}

	return true;
}

bool wxUntarFile(const wxString &archive, const wxString &target)
{
	char rwbuf[NRWBUFBYTES];
	wxFFileInputStream in( archive );
	if (!in.IsOk()) return false;

	wxTarInputStream tar(in);
	if (!tar.IsOk()) return false;

	wxTarEntry *te = tar.GetNextEntry();
	while (te)
	{
		wxString fn = target + "/" + te->GetName();
		if (!te->IsDir())
		{
			// create the directory if needed
			wxString dirpath = wxPathOnly(fn);
			if (!wxDirExists(dirpath))
				wxFileName::Mkdir( dirpath, 511, wxPATH_MKDIR_FULL );

			wxFFileOutputStream out( fn );
			if (!out.IsOk())
			{
				wxDELETE(te);
				return false;
			}

			while (!tar.Eof())
			{
				tar.Read(rwbuf, NRWBUFBYTES);
				out.Write(rwbuf, tar.LastRead());
			}				
		}
		wxDELETE(te);
		te = tar.GetNextEntry();
	}

	return true;
}

bool wxGunzipFile(const wxString &archive, const wxString &target)
{
	char rwbuf[NRWBUFBYTES];

	wxFFileInputStream in( archive );
	if (!in.IsOk()) return false;

	wxZlibInputStream gzp( in, wxZLIB_AUTO );
	if (!gzp.IsOk()) return false;

	wxFFileOutputStream out( target );
	if (!out.IsOk()) return false;

	while ( !gzp.Eof() )
	{
		gzp.Read( rwbuf, NRWBUFBYTES );
		size_t nread = gzp.LastRead();		
		if ( nread == 0 )
			return false; // error since couldn't read even though not at end of file.  sometimes happens in corrupt gzipped files

		out.Write( rwbuf, nread );
	}

	return true;
}

bool wxDecompressFile(const wxString &archive, const wxString &target)
{

	if (archive.Right(4).Lower() == ".zip")
	{
		return wxUnzipFile(archive, target);
	}
	else if (archive.Right(4).Lower() == ".tar")
	{
		return wxUntarFile(archive, target);
	}
	else if (archive.Right(7).Lower() == ".tar.gz")
	{
		wxString tempfile( wxFileName::CreateTempFileName( "gunzip" ) );
		if (!wxGunzipFile( archive, tempfile )) return false;
		return wxUntarFile(tempfile, target);
	}
	else if (archive.Right(3).Lower() == ".gz")
	{
		return wxGunzipFile(archive, target);
	}
	else
	{
		return false;
	}
}


// precalculate an array of ray numbers that we want to plot
std::vector<int> wxCommaDashListToIndices(const wxString &value)
{
	std::vector<int> list;
	wxArrayString parts = wxStringTokenize( value, "," );
	for (int i=0;i<(int)parts.Count();i++)
	{
		wxString s = parts[i];
		int hpos = s.Find('-');
		if (hpos < 0)
		{
			long num = 0;
			if (s.ToLong(&num))
				list.push_back( num );
		}
		else
		{
			long start=0, end=0;
			if (s.Mid(0,hpos).ToLong(&start)
				&& s.Mid(hpos+1).ToLong(&end)
				&& end >= start )
			{
				for (int j=start;j<=end;j++)
					list.push_back( j );
			}
		}

	}
	return list;
}


int wxDrawWordWrappedText(wxDC& dc, const wxString &str, int width, bool draw, int x, int y, wxArrayString *lines)
{
	int line = 0;
	int line_height = dc.GetCharHeight();
	wxString remaining = str;

	while ( !remaining.IsEmpty() )
	{
		wxString line_text = remaining;
		wxCoord line_width;
		dc.GetTextExtent(line_text, &line_width, NULL);
		while(line_width > 5 && line_width >= width-3 && line_text.Len() > 0)
		{
			int pos = line_text.Find(' ', true);
			if (pos < 0)
				line_text.Truncate( line_text.Len()-1 );
			else
				line_text.Truncate(pos);

			dc.GetTextExtent(line_text, &line_width, NULL);
		}

		if (line_text.IsEmpty() || line_width < 5)
			break;

		if (lines) lines->Add( line_text );
		
		if (draw)
			dc.DrawText(line_text, x, y+line*line_height);

		line++;

		remaining = remaining.Mid(line_text.Len());
		remaining.Trim(false).Trim();
	}

	return line*line_height;
}

void wxDrawRaisedPanel(wxDC &dc, int x, int y, int width, int height)
{	
	dc.DrawRectangle(x, y, width, height);
	
	wxPen savedPen = dc.GetPen();
	dc.SetPen(*wxWHITE_PEN);

	dc.DrawLine(x, 				y+1, 				x+width-1, 		y+1);
	dc.DrawLine(x, 				y+1, 					x, 				y+height-1);
	dc.DrawLine(x+1, 				y+1, 				x+width-2, 		y+1);
	
	dc.SetPen(*wxLIGHT_GREY_PEN);
	dc.DrawLine(x+1, 			y+height-2,			x+width-2, 		y+height-2);
	dc.DrawLine(x+width-2, 	y+2, 				x+width-2, 		y+height-2);
	
	dc.SetPen(*wxBLACK_PEN);
	dc.DrawLine(x, 				y+height-1, 	x+width-1, 		y+height-1);
	dc.DrawLine(x+width-1, 		y, 				x+width-1, 		y+height);	
	
	dc.SetPen(savedPen);
}


void wxDrawSunkenPanel(wxDC &dc, int x, int y, int width, int height)
{
	dc.DrawRectangle(x, y, width, height);

	wxPen savedPen = dc.GetPen();
	wxBrush savedBrush = dc.GetBrush();
	
	dc.SetBrush(*wxTRANSPARENT_BRUSH);
	dc.SetPen(*wxBLACK_PEN);
	dc.DrawRectangle(x, y, width-1, height-1);
	
	dc.SetPen(*wxGREY_PEN);
	dc.DrawRectangle(x+1, y+1, width-2, height-2);

	dc.SetBrush(savedBrush);
	dc.SetPen(savedPen);
}


void wxDrawEngravedPanel(wxDC &dc, int x, int y, int width, int height, bool fill)
{
	wxBrush savedBrush = dc.GetBrush();
	wxPen savedPen = dc.GetPen();
	
	if (fill)
	{
		dc.DrawRectangle(x, y, width, height);
	}

	dc.SetBrush(*wxTRANSPARENT_BRUSH);
	dc.SetPen(*wxGREY_PEN);
	dc.DrawRectangle(x, y, width-2, height-2);
	
	dc.SetPen(*wxWHITE_PEN);
	dc.DrawRectangle(x+1, y+1, width-2, height-2);
	dc.SetBrush(savedBrush);
	dc.SetPen(savedPen);
}


void wxDrawScrollBar(wxDC &dc, bool vertical, int x, int y, int width, int height)
{
	wxPen savedPen = dc.GetPen();
	wxBrush savedBrush = dc.GetBrush();
	
	dc.SetPen(*wxLIGHT_GREY_PEN);
	dc.SetBrush(*wxLIGHT_GREY_BRUSH);
	dc.DrawRectangle(x, y, width, height);	

	dc.SetBrush(savedBrush);
	dc.SetPen(savedPen);
	if (vertical)
	{
		wxDrawArrowButton(dc, wxARROW_UP, x, y, width, width);
		if (height > 2.5*width)
			wxDrawRaisedPanel(dc, x, y+width+1, width, 0.3*height);

		wxDrawArrowButton(dc, wxARROW_DOWN, x, y+height-width, width, width);
	}
	else
	{
		wxDrawArrowButton(dc, wxARROW_LEFT, x, y, height, height);
		if (width > 2.5*height)
			wxDrawRaisedPanel(dc, x+height+1, y, 0.3*width, height);

		wxDrawArrowButton(dc, wxARROW_RIGHT, x+width-height, y, height, height);
	}
	dc.SetBrush(savedBrush);
	dc.SetPen(savedPen);
}


void wxDrawArrowButton(wxDC &dc, wxArrowType type, int x, int y, int width, int height)
{
	int asize = width < height ? width/2 : height/2;
	
	wxBrush savedBrush = dc.GetBrush();
	wxPen savedPen = dc.GetPen();
	wxDrawRaisedPanel(dc, x, y, width, height);
	dc.SetBrush(*wxBLACK_BRUSH);
	dc.SetPen(*wxBLACK_PEN);
	
	switch(type)
	{
	case wxARROW_UP:
	case wxARROW_DOWN:
		wxDrawArrow(dc, type, x+(width-asize)/2, y+(height-asize)/2, asize, asize);
		break;
	default:
		wxDrawArrow(dc, type, x+(width-asize)/2, y+(height-asize)/2, asize, asize);
	}
	
	dc.SetBrush(savedBrush);
	dc.SetPen(savedPen);
}



void wxDrawArrow(wxDC &dc, wxArrowType type, int x, int y, int width, int height)
{
	wxPoint pts[3];
	switch(type)
	{
	case wxARROW_RIGHT:
      pts[0] = wxPoint(x,y);
      pts[1] = wxPoint(x, y+height);
      pts[2] = wxPoint(x+width, y+height/2);
      break;
   case wxARROW_LEFT:
      pts[0] = wxPoint(x+width,y);
      pts[1] = wxPoint(x+width, y+height);
      pts[2] = wxPoint(x, y+height/2);
      break;
   case wxARROW_UP:
      pts[0] = wxPoint(x,y+height);
      pts[1] = wxPoint(x+width, y+height);
      pts[2] = wxPoint(x+width/2, y);
      break;
   case wxARROW_DOWN:
      pts[0] = wxPoint(x,y);
      pts[1] = wxPoint(x+width, y);
      pts[2] = wxPoint(x+width/2, y+height);
      break;
   default:
   	return;
  	}

	dc.DrawPolygon(3, pts);
}


class TextMessageDialog : public wxDialog
{
public:
	TextMessageDialog(const wxString &text, const wxString &title, wxWindow *parent, const wxSize &size, long buttons)
		: wxDialog( parent, wxID_ANY, title, wxDefaultPosition, size, 
			wxRESIZE_BORDER|wxDEFAULT_DIALOG_STYLE )
	{

		wxTextCtrl *txtctrl = new wxTextCtrl(this, -1, text, 
			wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_DONTWRAP|wxTE_READONLY|wxBORDER_NONE);
		//txtctrl->SetFont( wxFont(10, wxMODERN, wxNORMAL, wxNORMAL) );
		
		wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
		sizer->Add( txtctrl, 1, wxALL|wxEXPAND, 0 );
		sizer->Add( CreateButtonSizer( buttons ), 0, wxALL|wxEXPAND, 4 );
		SetSizer( sizer );

		SetEscapeId( wxID_CANCEL );
	}

	void OnCommand( wxCommandEvent &evt )
	{
		switch( evt.GetId() )
		{
		case wxID_OK: EndModal( wxOK ); break;
		case wxID_YES: EndModal( wxYES ); break;
		case wxID_NO: EndModal( wxNO ); break;
		case wxID_CLOSE: EndModal( wxCLOSE ); break;
		case wxID_APPLY: EndModal( wxAPPLY ); break;
		case wxID_CANCEL: default:
			EndModal( wxCANCEL ); break;
		}
	}

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE( TextMessageDialog, wxDialog )
	EVT_BUTTON( wxID_YES, TextMessageDialog::OnCommand )
	EVT_BUTTON( wxID_NO, TextMessageDialog::OnCommand )
	EVT_BUTTON( wxID_CANCEL, TextMessageDialog::OnCommand )
	EVT_BUTTON( wxID_OK, TextMessageDialog::OnCommand )
	EVT_BUTTON( wxID_APPLY, TextMessageDialog::OnCommand )
	EVT_BUTTON( wxID_CLOSE, TextMessageDialog::OnCommand )
END_EVENT_TABLE()

int wxShowTextMessageDialog(const wxString &text, const wxString &title, wxWindow *parent, const wxSize &size, long buttons )
{
	wxSize sz(size);
	if ( sz == wxDefaultSize )
		sz.Set( 600, 400 );

	wxString tt(title);
	if( tt.IsEmpty() )
		tt = "Notice";
   TextMessageDialog dlg(text, tt, parent, sz, buttons);
   if( parent ) dlg.CenterOnParent();
   return dlg.ShowModal();  
}


int wxNDay[12] = {31,28,31,30,31,30,31,31,30,31,30,31};
	
/* returns month number 1..12 given 
	time: hour index in year 0..8759 */
int wxMonthOf(double time)
{
	if (time < 0) return 0;
	if (time < 744) return 1;
	if (time < 1416) return 2;
	if (time < 2160) return 3;
	if (time < 2880) return 4;
	if (time < 3624) return 5;
	if (time < 4344) return 6;
	if (time < 5088) return 7;
	if (time < 5832) return 8;
	if (time < 6552) return 9;
	if (time < 7296) return 10;
	if (time < 8016) return 11;
	if (time < 8760) return 12;
	return 0;
}
	/* month: 1-12 time: hours, starting 0=jan 1st 12am, returns 1-nday*/
int wxDayOfMonth(int month, double time)
{
	int daynum = ( ((int)(time/24.0)) + 1 );   // day goes 1-365
	switch(month)
	{
	case 1: return  daynum;
	case 2: return  daynum-31;
	case 3: return  daynum-31-28;
	case 4: return  daynum-31-28-31;
	case 5: return  daynum-31-28-31-30;
	case 6: return  daynum-31-28-31-30-31;
	case 7: return  daynum-31-28-31-30-31-30;
	case 8: return  daynum-31-28-31-30-31-30-31;
	case 9: return  daynum-31-28-31-30-31-30-31-31;
	case 10: return daynum-31-28-31-30-31-30-31-31-30;
	case 11: return daynum-31-28-31-30-31-30-31-31-30-31;
	case 12: return daynum-31-28-31-30-31-30-31-31-30-31-30; 
	default: break;
	}
	return daynum;
}

/* converts 'time' (hours since jan 1st 12am, 0 index) to M(1..12), D(1..N), H(0..23), M(0..59) */
void wxTimeToMDHM( double time, int *mo, int *dy, int *hr, int *min )
{
	*mo = wxMonthOf( time );
	*dy = wxDayOfMonth( *mo, time );
	*hr = (int)(((int)time)%24);

	if ( min != 0 )
	{
		double fraction = time - ((long)time);
		*min = (int)( fraction*60 );
	}
}

/* converts M(1..12), D(1..N), H(0..23) M(0..59) to time in hours since jan 1st 12am, 0 index ) */
double wxMDHMToTime( int mo, int dy, int hr, int min )
{
	// shift to zero index
	mo--;
	dy--;

	int time = hr + dy*24;

	for( int m=0;m<mo;m++ )
		time += wxNDay[m]*24;

	return time + min/60.0;
}

static const char *sg_months[12] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };


wxString wxMonthName( int mo )
{
	if ( mo >= 1 && mo <= 12 )
		return wxString( sg_months[mo-1] );
	else
		return wxEmptyString;
}

wxString wxFormatMDHM( int mo, int dy, int hr, int min, bool use_12_hr )
{
	
	if ( mo < 1 ) mo = 1;
	if ( mo > 12 ) mo = 12;

	if ( use_12_hr )
		return wxString::Format( "%s %d, %02d:%02d %s", sg_months[mo-1], dy, 
			 ( hr == 0 ? 12 : (hr > 12 ? hr-12 : hr) ),
			 min,
			 hr < 12 ? "am" : "pm" );
	else
		return wxString::Format( "%s %d, %d:%d", sg_months[mo-1], dy, hr, min );
}

wxString wxFormatTime( size_t istep, size_t steps_per_hour, bool use_12_hr )
{
	// M(1..12), D(1..N), H(0..23), M(0..59)	
	int hr = (int)( istep / steps_per_hour );
	int mo = wxMonthOf( (double)hr );
	int dy = wxDayOfMonth( mo, (int) hr );
	int min = (int)( ((double)(istep - hr*steps_per_hour)) / steps_per_hour * 60.0 );	
	return wxFormatMDHM( mo, dy, hr % 24, min, use_12_hr );
}

wxString wxFormatTime( double time, bool use_12_hr )
{
	int mo, dy, hr, min;
	wxTimeToMDHM( time, &mo, &dy, &hr, &min );
	return wxFormatMDHM( mo, dy, hr, min, use_12_hr );
}


void wxSortByLabels(wxArrayString &names, wxArrayString &labels)
{
	// sort the selections by labels
	wxString buf;
	int count = (int)labels.Count();
	for (int i=0;i<count-1;i++)
	{
		int smallest = i;

		for (int j=i+1;j<count;j++)
			if ( labels[j] < labels[smallest] )
				smallest = j;

		// swap
		buf = labels[i];
		labels[i] = labels[smallest];
		labels[smallest] = buf;

		buf = names[i];
		names[i] = names[smallest];
		names[smallest] = buf;

	}
}

double wxGetScreenHDScale()
{
	double xs, ys;
	wxGetScreenHDScale( &xs, &ys );
	return xs < ys ? xs : ys;
}


#if defined(__WXMSW__)
#define DPI_NOMINAL 96.0 // Windows
#else
#define DPI_NOMINAL 72.0 // OSX & Linux
#endif

void wxDevicePPIToScale( const wxSize &ppi, double *xs, double *ys )
{
// don't try to scale on linux...
#ifdef __WXGTK__
	*xs = *ys = 1.0;
#else
	*xs = ppi.x/DPI_NOMINAL;
	*ys = ppi.y/DPI_NOMINAL;
#endif
}

double wxGetDrawingDPI()
{
	return DPI_NOMINAL*wxGetScreenHDScale();
}

void wxGetScreenHDScale( double *xs, double *ys )
{
static wxSize dpi( -1, -1 );

// only calculate scale once per application run
// if the DPI changes while the application is running,
// we should handle a WM_DPICHANGE message on Windows
// in wxWidgets and recalculate the scale below, but
// that's not done currently...

	if ( dpi.x < 0 || dpi.y < 0 )
	{
		dpi.x = DPI_NOMINAL;
		dpi.y = DPI_NOMINAL;

		if ( wxWindowList::compatibility_iterator first = wxTopLevelWindows.GetFirst() )
		{
			dpi = wxClientDC( first->GetData() ).GetPPI();
		}
		else
		{
			wxFrame *frm = new wxFrame( 0, wxID_ANY, "no title" );
			dpi = wxClientDC(frm).GetPPI();
			frm->Destroy();
		}
	}

	wxDevicePPIToScale( dpi, xs, ys );
}


wxPoint wxScalePoint( const wxPoint &p, double xs, double ys )
{
	return wxPoint( (int)(p.x*xs), (int)(p.y*ys) );
}

wxSize wxScaleSize( const wxSize &s, double xs, double ys )
{
	return wxSize( (int)(s.x*xs), (int)(s.y*ys) );
}

wxRect wxScaleRect( const wxRect &r, double xs, double ys )
{
	return wxRect( (int)(r.x*xs), (int)(r.y*ys),
		(int)(r.width*xs), (int)(r.height*ys));
}


wxFrame *wxCreateTransparentOverlay( wxWindow *parent )
{
	wxPoint pos = parent->ClientToScreen( wxPoint(0,0) );
	wxSize size = parent->GetClientSize();

	wxFrame *trans = new wxFrame( parent, wxID_ANY, wxEmptyString,  pos, size, 
		wxBORDER_NONE | wxFRAME_FLOAT_ON_PARENT | wxFRAME_NO_TASKBAR );
	trans->SetBackgroundColour( *wxLIGHT_GREY );
	trans->SetTransparent( 230 );
	trans->Show();

	return trans;
}


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
// 
// MD5 from RSA
// ------------

#define MD5_HASHBYTES 16

typedef struct MD5Context {
        unsigned int buf[4];
        unsigned int bits[2];
        unsigned char in[64];
} MD5_CTX;

static void   MD5Init(MD5_CTX *context);
static void   MD5Update(MD5_CTX *context, unsigned char const *buf, unsigned len);
static void   MD5Final(unsigned char digest[MD5_HASHBYTES], MD5_CTX *context);
static void   MD5Transform(unsigned int buf[4], unsigned int const in[16]);
static char * MD5End(MD5_CTX *, char *);

// ----------------
// MD5 by RSA
// ----------------
	
static char * MD5End(MD5_CTX *ctx, char *buf)
{
    int i;
    unsigned char digest[MD5_HASHBYTES];
    char hex[]="0123456789abcdef";

    if (!buf)
    {
     	buf = (char *)malloc(33);
    }
    
    if (!buf)
        return 0;
    
    MD5Final(digest,ctx);
    for (i=0;i<MD5_HASHBYTES;i++) {
        buf[i+i] = hex[digest[i] >> 4];
        buf[i+i+1] = hex[digest[i] & 0x0f];
    }
    buf[i+i] = '\0';
    return buf;
}

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
    if (count < 8) {
        /* Two lots of padding:  Pad the first block to 64 bytes */
        memset(p, 0, count);
        //[TCC] byteReverse(ctx->in, 16);
        MD5Transform(ctx->buf, (unsigned int *) ctx->in);

        /* Now fill the next block with 56 bytes */
        memset(ctx->in, 0, 56);
    } else {
        /* Pad block to 56 bytes */
        memset(p, 0, count - 8);   
    }
    //[TCC] byteReverse(ctx->in, 14);

    /* Append length in bits and transform */
    ((unsigned int *) ctx->in)[14] = ctx->bits[0];
    ((unsigned int *) ctx->in)[15] = ctx->bits[1];

    MD5Transform(ctx->buf, (unsigned int *) ctx->in);
    //[TCC] byteReverse((unsigned char *) ctx->buf, 4);   
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
        ctx->bits[1]++;         /* Carry from low to high */
    ctx->bits[1] += len >> 29;

    t = (t >> 3) & 0x3f;        /* Bytes already in shsInfo->data */

    /* Handle any leading odd-sized chunks */

    if (t) {
        unsigned char *p = (unsigned char *) ctx->in + t;

        t = 64 - t;
        if (len < t) {
            memcpy(p, buf, len);
            return;
        }
        memcpy(p, buf, t);
        //[TCC] byteReverse(ctx->in, 16);
        MD5Transform(ctx->buf, (unsigned int *) ctx->in);
        buf += t;
        len -= t;
    }
    /* Process data in 64-byte chunks */

    while (len >= 64) {
        memcpy(ctx->in, buf, 64);
        //[TCC] byteReverse(ctx->in, 16);
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
 

#ifndef REVERSEBYTE
#define byteReverse(buf, len)   /* Nothing */
#else
static void byteReverse(unsigned char *buf, unsigned longs);

/*
 * Note: this code is harmless on little-endian machines.
 */
static void byteReverse(unsigned char *buf, unsigned longs)
{
    unsigned int t;
    do {
        t = (unsigned int) ((unsigned) buf[3] << 8 | buf[2]) << 16 |
            ((unsigned) buf[1] << 8 | buf[0]);
        *(unsigned int *) buf = t;  
        buf += 4;
    } while (--longs);
}
#endif



// --------------------------
// wxMD5 - static functions
// --------------------------

wxString wxGetMD5(const wxString &string)
{
	MD5_CTX ctx;
	char tmp[40];		// MD5 are fixed sized to 32 chars

	char *buf = strdup( (const char*)string.c_str() );

	MD5Init(&ctx);
	MD5Update(&ctx, (unsigned char*)buf, strlen(buf) );
	MD5End(&ctx, tmp);

	free(buf);

	return wxString(tmp, wxConvUTF8);
}

wxString wxGetFileMD5(wxInputStream &stream)
{
    unsigned char buffer[102400];
	char tmp[40];		// MD5 are fixed sized to 32 chars
    MD5_CTX ctx;

	if (!stream.IsOk())
		return wxEmptyString;

   	MD5Init(&ctx);

    do
    {
		if (stream.Read(buffer, sizeof(buffer)).LastRead() <= 0)
			return wxEmptyString;

    	MD5Update(&ctx, buffer, stream.LastRead());
    } while (!stream.Eof());

	MD5End(&ctx, tmp);
    return wxString(tmp, wxConvUTF8);
}

wxString wxGetFileMD5(const wxString &filename)
{ 
	wxFileInputStream stream(filename); 
	return wxGetFileMD5(stream); 
}

#ifdef __WXMSW__
#include <wx/dynlib.h>
#endif

void wxMakeProcessDPIAware() {
#ifdef __WXMSW__
    typedef BOOL (WINAPI *SetProcessDPIAware_t)(void); 
    wxDynamicLibrary dllUser32(wxT("user32.dll")); 
    SetProcessDPIAware_t pfnSetProcessDPIAware = 
        (SetProcessDPIAware_t)dllUser32.RawGetSymbol(wxT("SetProcessDPIAware")); 
    if ( pfnSetProcessDPIAware ) 
        pfnSetProcessDPIAware(); 
#endif
}