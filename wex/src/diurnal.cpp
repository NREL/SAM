#include <algorithm>

#include <wx/clipbrd.h>
#include <wx/dcbuffer.h>
#include <wx/dcclient.h>
#include <wx/tokenzr.h>

#ifdef __WXMSW__
#include <wx/msw/private.h>
#endif

#include "wex/utils.h"
#include "wex/diurnal.h"


BEGIN_EVENT_TABLE(wxDiurnalPeriodCtrl, wxWindow)
	EVT_PAINT(wxDiurnalPeriodCtrl::OnPaint)
	EVT_ERASE_BACKGROUND(wxDiurnalPeriodCtrl::OnErase)
	EVT_SIZE(wxDiurnalPeriodCtrl::OnResize)
	EVT_CHAR(wxDiurnalPeriodCtrl::OnChar)
	EVT_KEY_DOWN(wxDiurnalPeriodCtrl::OnKeyDown)
	EVT_LEFT_DOWN(wxDiurnalPeriodCtrl::OnMouseDown)
	EVT_LEFT_UP(wxDiurnalPeriodCtrl::OnMouseUp)
	EVT_MOTION(wxDiurnalPeriodCtrl::OnMouseMove)
	EVT_KILL_FOCUS(wxDiurnalPeriodCtrl::OnLostFocus)
END_EVENT_TABLE()

DEFINE_EVENT_TYPE(wxEVT_DIURNALPERIODCTRL_CHANGE)

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif
#ifndef MAX
#define MAX(a,b) (((a)>(b))?(a):(b))
#endif

#define SCHED_FONT wxFont(8, wxFONTFAMILY_DEFAULT, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_BOLD)

#define VALUE( r, c ) m_data[m_ncols*(r)+(c)]

wxDiurnalPeriodCtrl::wxDiurnalPeriodCtrl(wxWindow *parent, int id, const wxPoint &pos, const wxSize &sz)
: wxWindow(parent, id, pos, sz, wxWANTS_CHARS)
{
	SetBackgroundColour(*wxWHITE);
	SetBackgroundStyle(wxBG_STYLE_CUSTOM);

	m_mouseDown = false;
	m_rowHeaderSize = 30;
	m_colHeaderSize = 21;
	m_cellSize = 17;
	m_cols = 24;
	m_selStartR = m_selStartC = m_selEndR = m_selEndC = -1;
	m_min = 0;
	m_max = 9; // can be set to higher value in SetMinMax
	
	SetupTOUGrid();
}

wxDiurnalPeriodCtrl::~wxDiurnalPeriodCtrl()
{
	/* nothing to do */
}

bool wxDiurnalPeriodCtrl::Enable(bool enable)
{
	bool ret_val = wxWindow::Enable(enable);
	Refresh();
	return ret_val;
}

void wxDiurnalPeriodCtrl::AddColour(const wxColour &c)
{
	m_colours.push_back(c);
}

bool wxDiurnalPeriodCtrl::GetColour(int i, wxColour &c)
{
	if (i >= 0 && i < (int)m_colours.size())
	{
		c = m_colours[i];
		return true;
	}
	else
		return false;
}

void wxDiurnalPeriodCtrl::Set(size_t r, size_t c, int val)
{
	if (r < m_nrows && c < m_ncols)
		VALUE(r,c) = val;
}


void wxDiurnalPeriodCtrl::SetMin(int min)
{ 
	SetMinMax(min, m_max);
}

int wxDiurnalPeriodCtrl::GetMin()
{ 
	return m_min; 
}

void wxDiurnalPeriodCtrl::SetMax(int max)
{ 
	SetMinMax(m_min, max);
}

int wxDiurnalPeriodCtrl::GetMax()
{ 
	return m_max; 
}


void wxDiurnalPeriodCtrl::SetMinMax(int min, int max, bool clamp)
{
	m_min = min;
	m_max = max;

	if (!clamp)
		return;

	for (size_t r = 0; r < m_nrows; r++)
		for (size_t c = 0; c < m_ncols; c++)
		{
			if (VALUE(r,c) < min) VALUE(r,c) = min;
			if (VALUE(r,c) > max) VALUE(r,c) = max;
		}
}

int wxDiurnalPeriodCtrl::Get(size_t r, size_t c) const
{
	if (r < m_nrows && c < m_ncols)
		return VALUE(r,c);
	else
		return -1;
}

void wxDiurnalPeriodCtrl::Set(int val)
{
	for (size_t r = 0; r < m_nrows; r++)
		for (size_t c = 0; c < m_ncols; c++)
			VALUE(r,c) = val;
	Refresh();
}

void wxDiurnalPeriodCtrl::AddRowLabel(const wxString &s)
{
	m_rowLabels.Add(s);
}

void wxDiurnalPeriodCtrl::AddColLabel(const wxString &s)
{
	m_colLabels.Add(s);
}

void wxDiurnalPeriodCtrl::ClearLabels()
{
	m_rowLabels.Clear();
	m_colLabels.Clear();
}

void wxDiurnalPeriodCtrl::ClearRowLabels()
{
	m_rowLabels.Clear();
}

void wxDiurnalPeriodCtrl::ClearColLabels()
{
	m_colLabels.Clear();
}

void wxDiurnalPeriodCtrl::OnErase(wxEraseEvent &)
{
	/* nothing to do */
}

void wxDiurnalPeriodCtrl::OnPaint(wxPaintEvent &)
{
	wxAutoBufferedPaintDC dc(this);
	wxSize sz(GetClientSize());
	wxRect geom(0, 0, sz.GetWidth(), sz.GetHeight());

	dc.SetBackground(GetBackgroundColour());
	dc.Clear();

	int r, c;
	int rows = m_nrows;
	int cols = m_ncols;

	dc.SetFont(SCHED_FONT);

	dc.SetPen(*wxTRANSPARENT_PEN);
	for (r = 0; r<rows; r++)
	{
		for (c = 0; c<cols; c++)
		{
			int x = m_rowHeaderSize + c*m_cellSize;
			int y = m_colHeaderSize + r*m_cellSize;

			int selrs = MIN(m_selStartR, m_selEndR);
			int selcs = MIN(m_selStartC, m_selEndC);
			int selre = MAX(m_selStartR, m_selEndR);
			int selce = MAX(m_selStartC, m_selEndC);

			bool sel = (r >= selrs && r <= selre && c >= selcs && c <= selce);

			if (x >= geom.width || y >= geom.height)
				break;

			int val = VALUE(r,c);
			if (val >= 1 && val - 1 < (int)m_colours.size() || sel)
			{
				if (IsThisEnabled())
					dc.SetBrush(wxBrush(sel ? wxColour(0, 114, 198) : m_colours[val - 1]));
				else
					dc.SetBrush(wxBrush(*wxLIGHT_GREY));

				dc.DrawRectangle(geom.x + x, geom.y + y, m_cellSize, m_cellSize);
			}

			wxString buf;
			buf << val;
			int textW, textH;
			dc.GetTextExtent(buf, &textW, &textH);
			x += m_cellSize / 2 - textW / 2;
			y += m_cellSize / 2 - textH / 2;

			dc.SetTextForeground(sel ? *wxWHITE : *wxBLACK);
			dc.DrawText(buf, geom.x + x, geom.y + y);
		}
	}

	dc.SetPen( *wxWHITE_PEN );
	dc.SetTextForeground(wxColour(160, 160, 160));
	
	for (r = 0; r <= rows; r++)
	{
		dc.DrawLine(geom.x, geom.y + m_colHeaderSize + r*m_cellSize,
			geom.x + m_rowHeaderSize + cols*m_cellSize, geom.y + m_colHeaderSize + r*m_cellSize);

		if (r < (int)m_rowLabels.Count() && r < rows)
		{
			int yoff = m_cellSize / 2 - dc.GetCharHeight() / 2;
			dc.DrawText(m_rowLabels[r], geom.x + 2, geom.y + m_colHeaderSize + r*m_cellSize + yoff);
		}
	}

	for (c = 0; c <= cols; c++)
	{
		dc.DrawLine(geom.x + m_rowHeaderSize + c*m_cellSize, geom.y,
			geom.x + m_rowHeaderSize + c*m_cellSize, geom.y + m_colHeaderSize + rows*m_cellSize);

		if (c < (int)m_colLabels.Count() && c < cols)
		{
			int xoff = m_cellSize / 2 - dc.GetCharHeight() / 2;
			dc.DrawRotatedText(m_colLabels[c], geom.x + m_rowHeaderSize + c*m_cellSize + xoff, geom.y + m_colHeaderSize - 2, 90);
		}
	}

}

void wxDiurnalPeriodCtrl::OnResize(wxSizeEvent &)
{
	Refresh();
}
void wxDiurnalPeriodCtrl::UpdateLayout()
{	
	wxClientDC dc( const_cast<wxDiurnalPeriodCtrl*>(this) );
	dc.SetFont( SCHED_FONT );
	
	int r, c;
	int rows = m_nrows;
	int cols = m_ncols;

	m_rowHeaderSize = 0;
	for (r = 0; r<rows&&r<(int)m_rowLabels.Count(); r++)
	{
		wxSize tsz( dc.GetTextExtent(m_rowLabels[r]));
		if (tsz.x > m_rowHeaderSize)
			m_rowHeaderSize = tsz.x;
	}

	m_colHeaderSize = 0;
	for (c = 0; c<cols&&c<(int)m_colLabels.Count(); c++)
	{
		wxSize tsz( dc.GetTextExtent(m_colLabels[c]));
		if ( tsz.x > m_colHeaderSize )
			m_colHeaderSize = tsz.x;
	}
	
	double xScale, yScale;
	wxDevicePPIToScale( dc.GetPPI(), &xScale, &yScale );

	m_cellSize = (int)( 19*std::max( xScale, yScale ) );
	m_rowHeaderSize += (int)(6*yScale);
	m_colHeaderSize += (int)(6*xScale);
}

wxSize wxDiurnalPeriodCtrl::DoGetBestSize() const
{
	const_cast<wxDiurnalPeriodCtrl*>(this)->UpdateLayout();	
	return wxSize(m_rowHeaderSize + m_ncols*m_cellSize,
		m_colHeaderSize + m_nrows*m_cellSize);
}


int wxDiurnalPeriodCtrl::ScheduleCharToInt(char c)
{
	int ret = 0;
	switch (c)
	{
	case '1':
		ret = 1;
		break;
	case '2':
		ret = 2;
		break;
	case '3':
		ret = 3;
		break;
	case '4':
		ret = 4;
		break;
	case '5':
		ret = 5;
		break;
	case '6':
		ret = 6;
		break;
	case '7':
		ret = 7;
		break;
	case '8':
		ret = 8;
		break;
	case '9':
		ret = 9;
		break;
	case 'A':
	case 'a':
	case ':':
		ret = 10;
		break;
	case 'B':
	case 'b':
	case '=':
		ret = 11;
		break;
	case 'C':
	case 'c':
	case '<':
		ret = 12;
		break;
	}
	return ret;
}

char wxDiurnalPeriodCtrl::ScheduleIntToChar(int d)
{
	char ret = '0';
	switch (d)
	{
	case 1:
		ret = '1';
		break;
	case 2:
		ret = '2';
		break;
	case 3:
		ret = '3';
		break;
	case 4:
		ret = '4';
		break;
	case 5:
		ret = '5';
		break;
	case 6:
		ret = '6';
		break;
	case 7:
		ret = '7';
		break;
	case 8:
		ret = '8';
		break;
	case 9:
		ret = '9';
		break;
	case 10:
		ret = 'A';
		break;
	case 11:
		ret = 'B';
		break;
	case 12:
		ret = 'C';
		break;
	}
	return ret;
}


bool wxDiurnalPeriodCtrl::Schedule(const wxString &sched)
{
	if ((int)sched.Len() != m_nrows*m_ncols)
		return false;

	for (int r = 0; r<m_nrows; r++)
		for (int c = 0; c<m_ncols; c++)
			VALUE(r,c) = ScheduleCharToInt(sched[r*m_ncols + c]);

	Refresh();

	return true;
}

wxString wxDiurnalPeriodCtrl::Schedule() const
{
	wxString buf;
	for (int r = 0; r<m_nrows; r++)
		for (int c = 0; c<m_ncols; c++)
			buf << ScheduleIntToChar(VALUE(r,c));

	return buf;
}

void wxDiurnalPeriodCtrl::OnKeyDown(wxKeyEvent &evt)
{
	if (evt.GetModifiers() == wxMOD_CONTROL)
	{
		switch (evt.GetKeyCode())
		{
		case 'C':
			Copy();
			return;
		case 'V':
			Paste();
			return;
		default:
			break;
		}
	}
	evt.Skip();
}

void wxDiurnalPeriodCtrl::Copy()
{
	if (wxTheClipboard->Open())
	{
		// This data objects are held by the clipboard, 
		// so do not delete them in the app.
		wxString tsv;
		for (int r = 0; r<m_nrows; r++)
		{
			for (int c = 0; c<m_ncols; c++)
			{
				tsv += wxString::Format("%d", VALUE(r,c));
				if (c < m_ncols - 1)
					tsv += '\t';
			}
			tsv += '\n';
		}

		wxTheClipboard->SetData(new wxTextDataObject(tsv));
		wxTheClipboard->Close();
	}
}

void wxDiurnalPeriodCtrl::Paste()
{
	if (wxTheClipboard->Open())
	{
		wxTextDataObject tobj;
		wxTheClipboard->GetData(tobj);
		wxString sched = tobj.GetText();
		wxArrayString as = wxStringTokenize(sched, "\n\t");
		if (as.Count() >= (m_nrows * m_ncols))
		{
			int as_ndx = 0;
			for (int r = 0; r<m_nrows; r++)
			{
				for (int c = 0; c<m_ncols; c++)
				{
					long val = 0;
					if (as_ndx < as.Count())
						as[as_ndx].ToLong(&val);
					if ((val <= m_max) && (val >= m_min))
						VALUE(r,c) = val;
					as_ndx++;
				}
			}
		}
		wxCommandEvent change(wxEVT_DIURNALPERIODCTRL_CHANGE, this->GetId());
		change.SetEventObject(this);
		GetEventHandler()->ProcessEvent(change);
		Refresh();
	}
}

#ifdef __WXMSW__
bool wxDiurnalPeriodCtrl::MSWShouldPreProcessMessage(WXMSG* msg)
{
	// windows processing of ctrl+c and ctrl+v for copy and paste - from textctrl
	// check for our special keys here: if we don't do it and the parent frame
	// uses them as accelerators, they wouldn't work at all, so we disable
	// usual preprocessing for them
	if (msg->message == WM_KEYDOWN)
	{
		const WPARAM vkey = msg->wParam;
		if (HIWORD(msg->lParam) & KF_ALTDOWN)
		{
			// Alt-Backspace is accelerator for "Undo"
			if (vkey == VK_BACK)
				return false;
		}
		else // no Alt
		{
			// we want to process some Ctrl-foo and Shift-bar but no key
			// combinations without either Ctrl or Shift nor with both of them
			// pressed
			const int ctrl = wxIsCtrlDown();
			if (ctrl)
			{
				switch (vkey)
				{
				case 'C':
				case 'V':
					return false;
				}
			}
		}
	}

	return wxWindow::MSWShouldPreProcessMessage(msg);
}
#endif



void wxDiurnalPeriodCtrl::OnChar(wxKeyEvent &evt)
{
	int selrs = MIN(m_selStartR, m_selEndR);
	int selcs = MIN(m_selStartC, m_selEndC);
	int selre = MAX(m_selStartR, m_selEndR);
	int selce = MAX(m_selStartC, m_selEndC);

	int key = evt.GetKeyCode();
	if ((ScheduleCharToInt(key) >= m_min) && (ScheduleCharToInt(key) <= m_max) &&
		selrs >= 0 && selcs >= 0)
	{
		for (int r = selrs; r <= selre && r<m_nrows; r++)
			for (int c = selcs; c <= selce && c<m_ncols; c++)
				VALUE(r,c) = ScheduleCharToInt(key);

		Refresh();

		wxCommandEvent change(wxEVT_DIURNALPERIODCTRL_CHANGE, this->GetId());
		change.SetEventObject(this);
		GetEventHandler()->ProcessEvent(change);
	}
}

void wxDiurnalPeriodCtrl::OnMouseDown(wxMouseEvent &evt)
{
	m_selStartC = (evt.GetX() - m_rowHeaderSize) / m_cellSize;
	m_selStartR = (evt.GetY() - m_colHeaderSize) / m_cellSize;

	if (m_selStartC < 0 || m_selStartC >= m_ncols ||
		m_selStartR < 0 || m_selStartR >= m_nrows ||
		evt.GetX() < m_rowHeaderSize || evt.GetY() < m_colHeaderSize)
	{
		m_selStartC = m_selStartR = -1;
	}
	else
		this->SetFocus();

	m_selEndR = m_selStartR;
	m_selEndC = m_selStartC;
	m_mouseDown = true;
	Refresh();

}

void wxDiurnalPeriodCtrl::OnMouseUp(wxMouseEvent &)
{
	m_mouseDown = false;
}

void wxDiurnalPeriodCtrl::OnMouseMove(wxMouseEvent &evt)
{
	if (!m_mouseDown)
		return;

	int c = (evt.GetX() - m_rowHeaderSize) / m_cellSize;
	int r = (evt.GetY() - m_colHeaderSize) / m_cellSize;

	if (r >= 0 && r < m_nrows &&
		c >= 0 && c < m_ncols)
	{
		m_selEndR = r;
		m_selEndC = c;
		Refresh();
	}
}

void wxDiurnalPeriodCtrl::OnLostFocus(wxFocusEvent &)
{
	m_selEndR = m_selStartR = -1;
	m_selEndC = m_selStartC = -1;
	Refresh();
}

void wxDiurnalPeriodCtrl::SetData( float *data, size_t nr, size_t nc )
{
	if ( nr == m_nrows && nc == m_ncols )
	{
		memcpy( m_data, data, sizeof(float)*nr*nc );
		Refresh();
	}
}


float *wxDiurnalPeriodCtrl::GetData( size_t *nr, size_t *nc )
{
	*nr = m_nrows;
	*nc = m_ncols;
	return m_data;
}

void wxDiurnalPeriodCtrl::SetupTOUGrid()
{
	SetMinMax(1, 9, true);

	m_colours.clear();
	AddColour( wxColour( 143, 226, 170 ) );
	AddColour( wxColour( 128, 179, 179 ) );
	AddColour( wxColour( 196, 148, 49 ) );
	AddColour( wxColour( 44, 175, 133 ) );
	AddColour( wxColour( 219, 219, 112 ) );
	AddColour( wxColour( 206, 57, 57 ) );
	AddColour( wxColour( 94, 136, 81 ) );
	AddColour( wxColour( 225, 136, 225 ) );
	AddColour( wxColour( 255, 60, 157 ) );
	AddColour( wxColour( 86, 172, 214 ) );
	AddColour( wxColour( 226, 169, 141 ) );
	AddColour( wxColour( 254, 235, 97 ) );

	m_rowLabels.clear();
	AddRowLabel("Jan");
	AddRowLabel("Feb");
	AddRowLabel("Mar");
	AddRowLabel("Apr");
	AddRowLabel("May");
	AddRowLabel("Jun");
	AddRowLabel("Jul");
	AddRowLabel("Aug");
	AddRowLabel("Sep");
	AddRowLabel("Oct");
	AddRowLabel("Nov");
	AddRowLabel("Dec");

	m_colLabels.clear();
	AddColLabel("12am");
	AddColLabel("1am");
	AddColLabel("2am");
	AddColLabel("3am");
	AddColLabel("4am");
	AddColLabel("5am");
	AddColLabel("6am");
	AddColLabel("7am");
	AddColLabel("8am");
	AddColLabel("9am");
	AddColLabel("10am");
	AddColLabel("11am");
	AddColLabel("12pm");
	AddColLabel("1pm");
	AddColLabel("2pm");
	AddColLabel("3pm");
	AddColLabel("4pm");
	AddColLabel("5pm");
	AddColLabel("6pm");
	AddColLabel("7pm");
	AddColLabel("8pm");
	AddColLabel("9pm");
	AddColLabel("10pm");
	AddColLabel("11pm");

	UpdateLayout();
	InvalidateBestSize();
}
