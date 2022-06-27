/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <algorithm>

#include <wx/clipbrd.h>
#include <wx/dcbuffer.h>
#include <wx/filename.h>

#include <wex/numeric.h>
#include <wex/utils.h>
#include <wex/csv.h>

#include "ptlayoutctrl.h"


enum { ID_GRID = wxID_HIGHEST+134, ID_SPAN, ID_NUMROWS, ID_NUMCOLS, ID_COPY, ID_PASTE, ID_IMPORT, ID_EXPORT };

BEGIN_EVENT_TABLE( PTLayoutCtrl, wxPanel )
	EVT_NUMERIC( ID_SPAN, PTLayoutCtrl::OnSpanAngleChange )
	EVT_NUMERIC( ID_NUMROWS, PTLayoutCtrl::OnGridSizeChange )
	EVT_NUMERIC( ID_NUMCOLS, PTLayoutCtrl::OnGridSizeChange )
	EVT_GRID_CMD_CELL_CHANGED( ID_GRID, PTLayoutCtrl::OnGridCellChange)
	EVT_GRID_CMD_SELECT_CELL( ID_GRID, PTLayoutCtrl::OnGridCellSelect)

	EVT_BUTTON( ID_COPY, PTLayoutCtrl::OnButton )
	EVT_BUTTON( ID_PASTE, PTLayoutCtrl::OnButton )
	EVT_BUTTON( ID_IMPORT, PTLayoutCtrl::OnButton )
	EVT_BUTTON( ID_EXPORT, PTLayoutCtrl::OnButton )
END_EVENT_TABLE()

DEFINE_EVENT_TYPE( wxEVT_PTLAYOUT_CHANGE )


PTLayoutCtrl::PTLayoutCtrl(wxWindow *parent, int id, const wxPoint &pos, const wxSize &sz)
	: wxPanel(parent, id, pos, sz, wxWANTS_CHARS|wxCLIP_CHILDREN|wxTAB_TRAVERSAL)
{
	m_spanAngleEnabled = true;
	m_spanAngle = 360;

	m_numSpan = new wxNumericCtrl(this, ID_SPAN, 360.0);
	m_numRows = new wxNumericCtrl(this, ID_NUMROWS, 6, wxNUMERIC_INTEGER);
	m_numCols = new wxNumericCtrl(this, ID_NUMCOLS, 8, wxNUMERIC_INTEGER);

	m_data.resize_fill(12,12, 0.0);
	for (size_t r=0;r<m_data.nrows();r++)
		for (size_t c=0;c<m_data.ncols();c++)
			m_data.at(r,c) = 1;

	m_renderer = new PTLayoutRenderer(this);

	m_lblSpan = new wxStaticText(this, wxID_ANY, "Span Angle:");
	m_lblRows = new wxStaticText(this, wxID_ANY, "Radial Zones:");
	m_lblCols = new wxStaticText(this, wxID_ANY, "Azimuthal Zones:");

	m_grid = new wxGrid(this, ID_GRID);	
	m_grid->CreateGrid(6,8);
	m_grid->EnableEditing(true);
	m_grid->DisableDragCell();
	m_grid->DisableDragColSize();
	m_grid->DisableDragRowSize();
	m_grid->DisableDragColMove();
	m_grid->DisableDragGridSize();

	m_grid->SetRowLabelSize(wxGRID_AUTOSIZE);
	m_grid->SetRowLabelAlignment(wxALIGN_LEFT,wxALIGN_CENTRE);
	m_grid->SetColLabelSize(wxGRID_AUTOSIZE);
	m_grid->SetColLabelAlignment(wxALIGN_LEFT,wxALIGN_CENTRE);

	wxBoxSizer *boxctrls = new wxBoxSizer(wxHORIZONTAL);
	boxctrls->Add( m_lblSpan, 0, wxALL|wxALIGN_CENTER_VERTICAL, 4);
	boxctrls->Add( m_numSpan, 0, wxALL, 2);
	boxctrls->Add( m_lblRows, 0, wxALL|wxALIGN_CENTER_VERTICAL, 4);
	boxctrls->Add( m_numRows, 0, wxALL, 2);
	boxctrls->Add( m_lblCols, 0, wxALL|wxALIGN_CENTER_VERTICAL, 4);
	boxctrls->Add( m_numCols, 0, wxALL, 2);


	wxBoxSizer *boxtools = new wxBoxSizer(wxHORIZONTAL );
	boxtools->AddStretchSpacer();
	boxtools->Add( new wxButton( this, ID_COPY, "Copy" ), 0, wxALL|wxEXPAND, 2 );
	boxtools->Add( new wxButton( this, ID_PASTE, "Paste" ), 0, wxALL|wxEXPAND, 2 );
	boxtools->Add( new wxButton( this, ID_IMPORT, "Import" ), 0, wxALL|wxEXPAND, 2 );
	boxtools->Add( new wxButton( this, ID_EXPORT, "Export" ), 0, wxALL|wxEXPAND, 2 );

	wxBoxSizer *boxgrid = new wxBoxSizer(wxVERTICAL);
	boxgrid->Add( boxctrls, 0, wxALL|wxEXPAND, 0);
	boxgrid->Add( m_grid, 1, wxALL|wxEXPAND, 0);
	boxgrid->Add( boxtools,0, wxALL|wxEXPAND, 0);

	wxBoxSizer *boxmain = new wxBoxSizer(wxHORIZONTAL);
	boxmain->Add( m_renderer, 4, wxALL|wxEXPAND,1);
	boxmain->Add( boxgrid, 7, wxALL|wxEXPAND, 1);
	SetSizer( boxmain );
	
	ResizeGrid(12,12);
}

PTLayoutCtrl::~PTLayoutCtrl()
{
	/* nothing to do */
}

void PTLayoutCtrl::EnableSpanAngle(bool b)
{
	m_spanAngleEnabled = b;
	m_numSpan->Show( b );
	m_lblSpan->Show( b );

	Layout();

	if ( !m_spanAngleEnabled  && m_spanAngle != 360 )
	{
		m_spanAngle = 360;
		UpdateData();
	}
}

void PTLayoutCtrl::SetSpanAngle(double a)
{
	if (m_spanAngleEnabled  && a > 0 && a <= 360)
	{
		m_spanAngle = a;
		UpdateData();
	}
}

void PTLayoutCtrl::UpdateData()
{
	size_t nr = m_data.nrows();
	size_t nc = m_data.ncols();
	
	if ( m_spanAngle != 360 && nc > 2 )
	{
		size_t n_ecol = nc/2;
		for (size_t r=0;r<nr;r++)
			m_data.at(r,n_ecol) = 0;
	}

	m_grid->Freeze();

	if ( nr == (size_t)m_grid->GetNumberRows()
		&& nc == (size_t)m_grid->GetNumberCols() )
	{
		for (size_t r=0;r<nr;r++)
		{
			for (size_t c=0;c<nc;c++)
			{
				m_grid->SetCellValue( r,c, wxString::Format("%g", m_data.at(r,c)));
				m_grid->SetCellBackgroundColour(r,c,*wxWHITE);
			}
		}
	}
	else
	{
		m_grid->ClearGrid(); // this is an error - coming into this function grid and data should have same dimensions
		wxLogStatus("PTLayoutCtrl::UpdateData error: m_data.size != m_grid.size");
		return;
	}

	if (nc > 2)
	{
		
		int n_ecol = -1;
		double zspan = 360.0/nc;
		if (m_spanAngle != 360.0)
		{
			n_ecol = nc/2;
			zspan = m_spanAngle/(nc-1);
		}

		double angle = 0;
		for (int i=0;i<(int)nc;i++)
		{
			m_grid->SetColLabelValue(i, wxString::Format("%.1lf",angle));
			if (i!=n_ecol) angle += zspan;
			else angle += (360.0-m_spanAngle);
		}

		for (int i=0;i<(int)nr;i++)
			m_grid->SetRowLabelValue(i, wxString::Format("Rad.%d", i+1));

		m_lblRows->SetLabel("Radial Zones:");
		m_lblCols->SetLabel("Azimuthal Zones:");

		if (n_ecol > 0)
		{
			// grey out empty column
			m_grid->SetColLabelValue( n_ecol, "(empty)");			
			for (size_t r=0;r<m_data.nrows();r++)
				m_grid->SetCellBackgroundColour(r,n_ecol, wxColour(230,230,230));
		}

	}
	else
	{
		
		for (int i=0;i<(int)nr;i++)
			m_grid->SetRowLabelValue(i, wxString::Format("Heliostat %d", i+1));

		m_lblRows->SetLabel("# of Heliostats:");
		m_lblCols->SetLabel("(X-Y)");
		m_grid->SetColLabelValue(0,"X (m)");
		m_grid->SetColLabelValue(1,"Y (m)");
	}

	m_grid->SetRowLabelSize(wxGRID_AUTOSIZE);
	m_grid->SetColLabelSize(wxGRID_AUTOSIZE);
	m_grid->AutoSize();
	m_grid->Layout();
	m_grid->Refresh();
	m_grid->Thaw();
	m_renderer->Refresh();
	Layout();
}

void PTLayoutCtrl::FixDimensions( size_t &nr, size_t &nc )
{
	if (nr < 1) nr = 1;
	if (nc % 2) nc--;
	if (nc < 2) nc = 2;
	if (nc > 12) nc = 12;
	if (nr > 12 && nc > 2) nr = 12;
}


void PTLayoutCtrl::SetGrid( const matrix_t<double> &data )
{
	if ( data.nrows() < 1 || data.ncols() < 2 ) return;
	
	m_data = data;

	size_t nr = data.nrows();
	size_t nc = data.ncols();
	FixDimensions( nr, nc );
	ResizeGrid( nr, nc );
}


void PTLayoutCtrl::OnSpanAngleChange(wxCommandEvent &)
{
	if (m_spanAngleEnabled)
	{
		double a = m_numSpan->Value();
		if (a <= 0.0) a = 0.0;
		if (a >= 360.0) a = 360.0;
	
		m_spanAngle = a;
		UpdateData();
		m_numSpan->SetValue(m_spanAngle);
		DispatchEvent();
	}
}

void PTLayoutCtrl::OnGridSizeChange(wxCommandEvent &)
{
	size_t nr = (size_t)m_numRows->AsInteger();
	size_t nc = (size_t)m_numCols->AsInteger();
	
	FixDimensions(nr,nc);

	if ((int)nr != m_numRows->AsInteger()) m_numRows->SetValue(nr);
	if ((int)nc != m_numCols->AsInteger()) m_numCols->SetValue(nc);

	ResizeGrid(nr,nc);
	DispatchEvent();
}

void PTLayoutCtrl::ResizeGrid( size_t nrows, size_t ncols )
{
	m_grid->Freeze();

	if (m_grid->GetNumberRows() > (int)nrows)
		m_grid->DeleteRows( 0, m_grid->GetNumberRows() - (int)nrows );

	if (m_grid->GetNumberRows() < (int)nrows)
		m_grid->AppendRows( (int)nrows - m_grid->GetNumberRows() );

	if (m_grid->GetNumberCols() > (int)ncols)
		m_grid->DeleteCols( 0,m_grid->GetNumberCols() - (int)ncols );

	if (m_grid->GetNumberCols() < (int)ncols)
		m_grid->AppendCols( (int)ncols - m_grid->GetNumberCols() );


	m_grid->Thaw();

	matrix_t<double> old( m_data ); // create a copy of the old data

	m_data.resize_fill(nrows, ncols, 0.0f);

	for( size_t r = 0; r<old.nrows() && r < nrows; r++ )
		for( size_t c=0; c<old.ncols() && c < ncols; c++ )
			m_data.at(r,c) = old.at(r,c);
	
	if (m_numRows->AsInteger() != (int)m_data.nrows())
		m_numRows->SetValue( m_data.nrows() );

	if (m_numCols->AsInteger() != (int)m_data.ncols())
		m_numCols->SetValue( m_data.ncols() );

	UpdateData();
}

void PTLayoutCtrl::OnGridCellChange(wxGridEvent &evt)
{
	int r = evt.GetRow();
	int c = evt.GetCol();
	double val = atof( m_grid->GetCellValue(r,c).c_str() );

	if ( m_data.ncols() > 2 && val < 0 ) val = 0;

	if ( m_data.ncols() > 2 && r == 0 && val == 0 && m_data.nrows() > 2 && m_spanAngle==360.0)
	{
		wxMessageBox("The ring of radial zones closest to the tower are not allowed to have 0 heliostats.");
		m_grid->SetCellValue( r, c, wxString::Format("%g", m_data.at(r,c) ));
		return;
	}

	if (m_spanAngle != 360.0 && c == (int)m_data.ncols()/2 && m_data.ncols() > 2)
		val = 0;

	m_data.at(r,c) = val;
	m_grid->SetCellValue( r, c, wxString::Format("%g", val) );
	m_renderer->Refresh();

	DispatchEvent();
}

void PTLayoutCtrl::OnGridCellSelect(wxGridEvent &evt)
{
	int r = evt.GetRow();
	int c = evt.GetCol();

	m_renderer->Highlight(r,c);
	evt.Skip();
}

void PTLayoutCtrl::DispatchEvent()
{	
	wxCommandEvent change(wxEVT_PTLAYOUT_CHANGE, this->GetId() );
	change.SetEventObject( this );
	GetEventHandler()->ProcessEvent(change);
}


float PTLayoutCtrl::NumHeliostats()
{
	float sum = 0;
	if (IsZonal())
	{
		for (size_t r=0;r<m_data.nrows();r++)
			for (size_t c=0;c<m_data.ncols();c++)
				sum += m_data.at(r,c);
	}
	else
		sum = m_data.nrows();

	return sum;
}

void PTLayoutCtrl::OnButton( wxCommandEvent &evt )
{
	if ( evt.GetId() == ID_COPY  || evt.GetId() == ID_EXPORT )
	{
		wxCSVData csv;
		for( size_t r=0;r<m_data.nrows();r++ )
			for( size_t c=0;c<m_data.ncols();c++ )
				csv.Set( r, c, wxString::Format("%g", m_data(r,c)) );

		if ( evt.GetId() == ID_COPY )
		{
			if (wxTheClipboard->Open())
			{
				csv.SetSeparator('\t');
				wxTheClipboard->SetData(new wxTextDataObject(csv.WriteString()));
				wxTheClipboard->Close();
			}
		}
		else
		{
			wxFileDialog dlg( this, "Export to CSV file", wxEmptyString, 
				wxEmptyString, wxFileSelectorDefaultWildcardStr, wxFD_SAVE|wxFD_OVERWRITE_PROMPT );
			if ( dlg.ShowModal() == wxID_OK )
				if ( !csv.WriteFile( dlg.GetPath() ) )
					wxMessageBox("Error writing to file\n\n" + dlg.GetPath() );
		}
	}
	else
	{
		wxCSVData csv;		
		if ( evt.GetId() == ID_PASTE )
		{
			if (wxTheClipboard->Open())
			{
				wxTextDataObject textobj;
				if (wxTheClipboard->GetData( textobj ))
				{
					wxString data = textobj.GetText();
					if ( data.IsEmpty() ) return;
					csv.SetSeparator('\t');
					csv.ReadString( data );
					wxTheClipboard->Close();
				}
			}
		}
		else 
		{
			wxFileDialog dlg( this, "Import from CSV file", wxEmptyString, 
				wxEmptyString, wxFileSelectorDefaultWildcardStr, wxFD_OPEN );
			if ( dlg.ShowModal() == wxID_OK )
			{
				if (!csv.ReadFile( dlg.GetPath() ))
				{
					wxMessageBox("Could not read from file\n\n" + dlg.GetPath() );
					return;
				}
			}
		}

		size_t nr = csv.NumRows();
		size_t nc = csv.NumCols();
		if ( nr > 0 && nc > 0 )
		{
			matrix_t<double> mat( nr, nc, 0.0 );
			for( size_t r=0;r<nr;r++ )
				for( size_t c=0;c<nc;c++ )
					mat.at(r,c) = (double)wxAtof( csv.Get(r,c) );

			SetGrid( mat );
		}
	}
}

/**************************************/

BEGIN_EVENT_TABLE(PTLayoutRenderer, wxWindow)

EVT_PAINT( PTLayoutRenderer::OnPaint )
EVT_SIZE( PTLayoutRenderer::OnResize )
//EVT_CHAR( PTLayoutRenderer::OnChar )
//EVT_LEFT_DOWN( PTLayoutRenderer::OnMouseDown )
//EVT_LEFT_UP( PTLayoutRenderer::OnMouseUp )
//EVT_MOTION( PTLayoutRenderer::OnMouseMove )

END_EVENT_TABLE()

PTLayoutRenderer::PTLayoutRenderer(PTLayoutCtrl *parent)
	: wxWindow(parent,-1, wxDefaultPosition, wxDefaultSize, wxCLIP_CHILDREN|wxWANTS_CHARS)
{
	SetMinSize(wxScaleSize(200,200));
	hlRad=hlAzm=-1;

	SetBackgroundStyle(wxBG_STYLE_CUSTOM);
	mPTCtrl = parent;

	Colour1 = *wxWHITE;
	Colour2 = *wxRED;
}

void PTLayoutRenderer::Highlight(int rad, int azm)
{
	hlRad = rad;
	hlAzm = azm;
	Refresh();
}

void PTLayoutRenderer::OnResize(wxSizeEvent &)
{
	Refresh();
}

void PTLayoutRenderer::OnChar(wxKeyEvent &)
{
}

void PTLayoutRenderer::OnMouseDown(wxMouseEvent &)
{
	bMouseDown = true;
}


void PTLayoutRenderer::OnMouseUp(wxMouseEvent &)
{
	bMouseDown = false;
}

void PTLayoutRenderer::OnMouseMove(wxMouseEvent &)
{
}

void PTLayoutRenderer::OnPaint(wxPaintEvent &)
{
	wxAutoBufferedPaintDC pdc(this);

	int cw, ch;
	GetClientSize(&cw,&ch);
	wxRect geom(0,0,cw,ch);
	
	pdc.SetBrush(*wxWHITE_BRUSH);
	pdc.SetPen(*wxBLACK_PEN);
	pdc.DrawRectangle(0, 0, cw, ch);

	if (mPTCtrl->IsZonal()) DrawZonal(pdc,geom);
	else DrawXY(pdc,geom);

	//Draw rect with same pen but transparent fill here if border is drawn over.
}

void PTLayoutRenderer::ComputeColour(wxColour &c, int cntrIndex, int ncv)
{				
	c.Set(
		((ncv-cntrIndex) * Colour1.Red()   +
			cntrIndex * Colour2.Red())/ncv,

		((ncv-cntrIndex) * Colour1.Green() +
			cntrIndex * Colour2.Green())/ncv,

		((ncv-cntrIndex) * Colour1.Blue()  +
			cntrIndex * Colour2.Blue())/ncv );

}


void PTLayoutRenderer::DrawZonal(wxDC &dc, const wxRect &geom)
{
	matrix_t<double> &m_data = mPTCtrl->m_data;

	size_t nrad = m_data.nrows();
	size_t nazm = m_data.ncols();

	double fspan = mPTCtrl->m_spanAngle;
	double espan = 360-fspan;
	double zspan = 0.0;
	
	int n_empty = -1;

	if (fspan!=360.0)
	{
		n_empty = nazm/2;
		zspan = fspan/(nazm-1);
	}
	else
	{
		zspan = fspan/nazm;
	}

	wxFont f = *wxNORMAL_FONT;
	f.SetWeight(wxFONTWEIGHT_BOLD);
	dc.SetFont(f);

	double max_radius = std::min(geom.width, geom.height)/2-dc.GetCharHeight()-11;
	double min_radius = 20;
	int cir_x = geom.x+geom.width/2;
	int cir_y = geom.y+geom.height/2;
	int tw,th;

	wxPen p = wxPen(*wxLIGHT_GREY, 2, wxPENSTYLE_DOT);
	dc.SetPen(p);
	dc.DrawLine(cir_x, geom.y, cir_x, geom.y+geom.height);
	dc.DrawLine(geom.x, cir_y, geom.x+geom.width, cir_y);

	dc.SetPen(*wxBLACK_PEN);
	dc.SetBrush(*wxTRANSPARENT_BRUSH);

	dc.DrawCircle(cir_x, cir_y, (int)max_radius);

	int r,a;

	double maxval = -1e99;
	double rlo = 1.0, z;
	matrix_t<double> uf1;
	uf1.resize_fill(nrad, nazm, 0.0);

	for (r=0;r<(int)nrad;r++)
	{
		for (a=0;a<(int)nazm;a++)
		{
			if (r==0)
				z = 2.0/(rlo+1.5)*m_data.at(r,a);
			else if (r==(int)nrad-1)
				z = 2.0/(nrad-0.5+rlo)*m_data.at(r,a);
			else
				z = m_data.at(r,a)/(r+1.0+rlo);

			uf1.at(r,a) = z;
			if ( z > maxval ) maxval = z;
		}
	}

	int ncv = m_data.nrows() * m_data.ncols();

	double rdelta = (max_radius-min_radius)/nrad;
	double radius = max_radius;

	double adelta = zspan;
	double angle;

	for (r=nrad-1;r>=0;r--)
	{
		angle = -adelta/2+90;
		for (a=0;a<(int)nazm;a++)
		{
			int index = 0;
			if (maxval != 0.0)
				index = (int) ((double)ncv) * uf1.at(r,a)/maxval;
			
			wxColour cellcolour;
			ComputeColour(cellcolour, index, ncv);
			dc.SetPen(wxPen(cellcolour));
			dc.SetBrush(wxBrush(cellcolour));

			double astart = angle+zspan;
			double aend = angle;
			if ((int)a==n_empty)
			{
				astart=angle+zspan-espan;
				aend=astart+espan;
				dc.SetPen(wxPen(wxColour(230,230,230)));
				dc.SetBrush(wxBrush(wxColour(230, 230, 230)));
			}


			dc.DrawEllipticArc( 
				cir_x - (int)radius,
				cir_y - (int)radius,
				(int)(radius*2),
				(int)(radius*2),
				astart, aend);

			dc.SetBrush(*wxTRANSPARENT_BRUSH);


			// RENDER CELL HIGHLIGHT HERE
			if (r == hlRad && a == hlAzm)
			{
				dc.SetPen(wxPen(*wxBLUE, 4));
				dc.DrawEllipticArc( 
					cir_x - (int)radius,
					cir_y - (int)radius,
					(int)(radius*2),
					(int)(radius*2),
					astart, aend);

				dc.DrawEllipticArc( 
					cir_x - (int)radius + rdelta-1,
					cir_y - (int)radius + rdelta-1,
					(int)(radius*2 - rdelta*2+2),
					(int)(radius*2 - rdelta*2+2),
					astart, aend);
			}



			if (a==n_empty)
				angle -= espan;
			else
				angle -= adelta;
		}

		dc.SetPen(*wxBLACK_PEN);
		dc.SetBrush(*wxTRANSPARENT_BRUSH); //Should this still be transparent?
		dc.DrawCircle(cir_x, cir_y, (int) radius);
		radius -= rdelta;
	}

	dc.SetFont(wxFont(7, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_BOLD));
	if (dc.GetCharHeight() > rdelta)
		dc.SetFont(*wxSMALL_FONT);

	wxString buf;


	double dispangle = 0;
	angle = -adelta/2-90;
	for (a=0;a<(int)nazm;a++)
	{
		int end_x = cir_x + (int)(max_radius * cos( angle*M_PI/180 ));
		int end_y = cir_y + (int)(max_radius * sin( angle*M_PI/180 ));

		dc.DrawLine(cir_x, cir_y, end_x, end_y);
		
		end_x = cir_x + (int)(max_radius * cos( (angle+adelta/2)*M_PI/180 ));
		end_y = cir_y + (int)(max_radius * sin( (angle+adelta/2)*M_PI/180 ));

		if (a != n_empty)
		{
			buf = wxString::Format("%.1lf '",dispangle);
			dc.GetTextExtent(buf, &tw, &th);
			int offx = tw * ((dispangle<=180.0) ? 0 : -1);
			int offy = th * ((dispangle<=90.0||dispangle>=270.0) ? -1 : 0);
			dc.DrawText(buf, end_x + offx, end_y + offy);
			
			angle += adelta;
			dispangle += adelta;
		}
		else
		{
			angle += espan;
			dispangle += espan;
		}
	}

	dc.SetBrush(*wxBLACK_BRUSH);
	dc.SetPen(*wxBLACK_PEN);
	dc.DrawCircle(cir_x, cir_y, (int)min_radius);

	wxFont fb = *wxNORMAL_FONT;
	fb.SetWeight(wxFONTWEIGHT_BOLD);
	dc.SetFont(fb);
	dc.SetTextForeground(*wxWHITE);
	dc.GetTextExtent("T", &tw, &th);
	dc.DrawText("T", cir_x-tw/2, cir_y-th/2);

	dc.SetBrush(*wxBLACK_BRUSH);
	dc.SetPen(*wxBLACK_PEN);
	wxDrawArrow(dc, wxARROW_DOWN, geom.x+3, geom.y+4, 10, 10);
	dc.SetTextForeground( *wxBLACK );
	dc.DrawText("To Equator", geom.x+16, geom.y+3);


	// RENDER RADIAL ZONE NUMBERS in ZONE 1
	dc.SetTextForeground( *wxBLACK );
	dc.SetFont(wxFont(7, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_BOLD));
	if (dc.GetCharHeight() > rdelta)
		dc.SetFont(*wxSMALL_FONT);


	for ( r=0;r<(int)nrad;r++ )
	{
		buf = wxString::Format("%d", (int)(r+1));
		dc.GetTextExtent(buf, &tw, &th);
		dc.DrawText(buf, cir_x-tw/2, cir_y - (int)min_radius - r*rdelta - rdelta/2 - th/2);
	}
}

void PTLayoutRenderer::DrawXY(wxDC &dc, const wxRect &geom)
{
	matrix_t<double> &m_data = mPTCtrl->m_data;

	int nhel = m_data.nrows();
	int r;

	int cx = geom.x+geom.width/2;
	int cy = geom.y+geom.height/2;



	double xmin,xmax;
	double ymin,ymax;
	xmin=ymin=1e99;
	xmax=ymax=-1e99;
	for (r=0;r<nhel;r++)
	{
		if (m_data.at(r,0) < xmin) xmin = m_data.at(r,0);
		if (m_data.at(r,0) > xmax) xmax = m_data.at(r,0);
		if (m_data.at(r,1) < ymin) ymin = m_data.at(r,1);
		if (m_data.at(r,1) > ymax) ymax = m_data.at(r,1);
	}

	double xdiffmax = std::max( fabs(xmax), fabs(xmin) );
	double ydiffmax = std::max( fabs(ymax), fabs(ymin) );

	xmin = -xdiffmax;
	xmax = xdiffmax;
	ymin = -ydiffmax;
	ymax = ydiffmax;

	double xrange = xmax-xmin;
	double yrange = ymax-ymin;


	if (xrange == 0 || yrange == 0)
	{
		wxFont f = *wxNORMAL_FONT;
		f.SetWeight(wxFONTWEIGHT_BOLD);
		dc.SetFont(f);
		dc.SetTextForeground(*wxBLACK);
		dc.DrawText("Cannot render X-Y solar field.", 3, 3);
		return;
	}

	dc.SetBrush(wxBrush(*wxRED));
	dc.SetPen(wxPen(*wxRED));
	for (r=0;r<nhel;r++)
	{
		double x = m_data.at(r,0);
		double y = m_data.at(r,1);

		double propx = (x-xmin)/xrange;
		double propy = (y-ymin)/yrange;

		int px = geom.x+3 + (int)(geom.width-6)*propx;
		int py = geom.y+3 + geom.height-6 - (int)(geom.height-6)*propy;

		dc.DrawCircle(px,py,2);
	}

	wxPen p = *wxLIGHT_GREY_PEN;
	p.SetStyle(wxPENSTYLE_DOT);
	dc.SetPen(p);
	dc.DrawLine(cx,geom.y,cx,geom.y+geom.height);
	dc.DrawLine(geom.x, cy, geom.x+geom.width, cy);

	int tw,th;
	dc.SetPen(*wxBLACK_PEN);
	dc.SetBrush(*wxBLACK_BRUSH);
	dc.DrawCircle(cx,cy,8);
	wxFont f = *wxNORMAL_FONT;
	f.SetWeight(wxFONTWEIGHT_BOLD);
	dc.SetFont(f);
	dc.SetTextForeground(*wxWHITE);
	dc.GetTextExtent("T", &tw, &th);
	dc.DrawText("T", cx-tw/2, cy-th/2);
	
	dc.SetTextForeground(*wxBLACK);
	dc.DrawText("North ^", geom.x+4, geom.y+3);
}

