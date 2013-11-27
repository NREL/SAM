#include <algorithm>

#include <wx/clipbrd.h>
#include <wx/dcbuffer.h>
#include <wx/filename.h>

#include <wex/numeric.h>
#include <wex/utils.h>

#include "ptlayoutctrl.h"


enum { IDPT_GRID, IDPT_SPAN, IDPT_NUMROWS, IDPT_NUMCOLS };

BEGIN_EVENT_TABLE(PTLayoutCtrl, wxPanel)

EVT_TEXT_ENTER( IDPT_SPAN, PTLayoutCtrl::OnSpanAngleChange )
EVT_TEXT_ENTER( IDPT_NUMROWS, PTLayoutCtrl::OnGridSizeChange )
EVT_TEXT_ENTER( IDPT_NUMCOLS, PTLayoutCtrl::OnGridSizeChange )
EVT_GRID_CMD_CELL_CHANGE( IDPT_GRID, PTLayoutCtrl::OnGridCellChange)
EVT_GRID_CMD_SELECT_CELL( IDPT_GRID, PTLayoutCtrl::OnGridCellSelect)


END_EVENT_TABLE()

DEFINE_EVENT_TYPE( wxEVT_PTLAYOUT_CHANGE )


PTLayoutCtrl::PTLayoutCtrl(wxWindow *parent, int id, const wxPoint &pos, const wxSize &sz)
	: wxPanel(parent, id, pos, sz, wxWANTS_CHARS|wxCLIP_CHILDREN)
{
	bSpanAngleEnabled = true;
	mSpanAngle = 360.0;

	mNumSpan = new wxNumericCtrl(this, IDPT_SPAN, 360.0);
	mNumRows = new wxNumericCtrl(this, IDPT_NUMROWS, 6, wxNumericCtrl::INTEGER);
	mNumCols = new wxNumericCtrl(this, IDPT_NUMCOLS, 8, wxNumericCtrl::INTEGER);

	mData.resize_fill(12,12, 0.0);
	for (int r=0;r<mData.nrows();r++)
		for (int c=0;c<mData.ncols();c++)
			mData.at(r,c) = 1;

	mRenderer = new PTLayoutRenderer(this);

	mLblSpan = new wxStaticText(this, -1, "Span Angle:");
	mLblRows = new wxStaticText(this, -1, "Radial Zones:");
	mLblCols = new wxStaticText(this, -1, "Azimuthal Zones:");

	mGrid = new wxGrid(this, IDPT_GRID);	
	mGrid->CreateGrid(6,8);
	mGrid->EnableEditing(true);
	mGrid->DisableDragCell();
	mGrid->DisableDragColSize();
	mGrid->DisableDragRowSize();
	mGrid->DisableDragColMove();
	mGrid->DisableDragGridSize();

	mGrid->SetRowLabelSize(wxGRID_AUTOSIZE);
	mGrid->SetRowLabelAlignment(wxALIGN_LEFT,wxALIGN_CENTRE);
	mGrid->SetColLabelSize(wxGRID_AUTOSIZE);
	mGrid->SetColLabelAlignment(wxALIGN_LEFT,wxALIGN_CENTRE);

	wxBoxSizer *boxctrls = new wxBoxSizer(wxHORIZONTAL);
	boxctrls->Add( mLblSpan, 2, wxALL|wxEXPAND, 1);
	boxctrls->Add( mNumSpan, 1, wxALL|wxEXPAND, 1);
	boxctrls->Add( mLblRows, 2, wxALL|wxEXPAND, 1);
	boxctrls->Add( mNumRows, 1, wxALL|wxEXPAND, 1);
	boxctrls->Add( mLblCols, 2, wxALL|wxEXPAND, 1);
	boxctrls->Add( mNumCols, 1, wxALL|wxEXPAND, 1);


	wxBoxSizer *boxgrid = new wxBoxSizer(wxVERTICAL);
	boxgrid->Add( boxctrls, 0, wxALL|wxEXPAND,1);
	boxgrid->Add( mGrid, 1, wxALL|wxEXPAND, 1);

	wxBoxSizer *boxmain = new wxBoxSizer(wxHORIZONTAL);
	boxmain->Add( mRenderer, 4, wxALL|wxEXPAND,1);
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
	bSpanAngleEnabled = b;
	mNumSpan->Show( b );
	mLblSpan->Show( b );

	if ( !bSpanAngleEnabled  && mSpanAngle != 360.0 )
	{
		mSpanAngle = 360.0;
		UpdateData();
	}
}

bool PTLayoutCtrl::IsSpanAngleEnabled()
{
	return bSpanAngleEnabled;
}

bool PTLayoutCtrl::IsXY()
{
	return mData.ncols() == 2;
}

bool PTLayoutCtrl::IsZonal()
{
	return mData.ncols() > 2;
}

void PTLayoutCtrl::SetSpanAngle(double a)
{
	if (bSpanAngleEnabled  && a > 0 && a <= 360.0)
	{
		mSpanAngle = a;
		UpdateData();
	}
}

double PTLayoutCtrl::GetSpanAngle()
{
	return mSpanAngle;
}

void PTLayoutCtrl::UpdateData()
{
	int nr, nc;
	nr = mData.nrows();
	nc = mData.ncols();
	
	if (mSpanAngle != 360.0 && nc > 2)
	{
		int n_ecol = nc/2;
		for (int r=0;r<nr;r++)
			mData.at(r,n_ecol) = 0;
	}

	mGrid->Freeze();

	if (nr == mGrid->GetNumberRows()
		&& nc == mGrid->GetNumberCols())
	{
		for (int r=0;r<nr;r++)
		{
			for (int c=0;c<nc;c++)
			{
				mGrid->SetCellValue( r,c, wxString::Format("%lg", mData.at(r,c)));
				mGrid->SetCellBackgroundColour(r,c,*wxWHITE);
			}
		}
	}
	else
	{
		mGrid->ClearGrid();
	}

	if (nc > 2)
	{
		
		int n_ecol = -1;
		double zspan = 360.0/nc;
		if (mSpanAngle != 360.0)
		{
			n_ecol = nc/2;
			zspan = mSpanAngle/(nc-1);
		}

		double angle = 0;
		for (int i=0;i<nc;i++)
		{
			mGrid->SetColLabelValue(i, wxString::Format("%.1lf",angle));
			if (i!=n_ecol) angle += zspan;
			else angle += (360.0-mSpanAngle);
		}

		for (int i=0;i<nr;i++)
			mGrid->SetRowLabelValue(i, wxString::Format("Rad.%d", i+1));

		mLblRows->SetLabel("Radial Zones:");
		mLblCols->SetLabel("Azimuthal Zones:");

		if (n_ecol > 0)
		{
			// grey out empty column
			mGrid->SetColLabelValue( n_ecol, "(empty)");			
			for (int r=0;r<mData.nrows();r++)
				mGrid->SetCellBackgroundColour(r,n_ecol, wxColour(230,230,230));
		}

	}
	else
	{
		
		for (int i=0;i<nr;i++)
			mGrid->SetRowLabelValue(i, wxString::Format("Heliostat %d", i+1));

		mLblRows->SetLabel("# of Heliostats:");
		mLblCols->SetLabel("(X-Y)");
		mGrid->SetColLabelValue(0,"X (m)");
		mGrid->SetColLabelValue(1,"Y (m)");
	}

	mGrid->SetRowLabelSize(wxGRID_AUTOSIZE);
	mGrid->SetColLabelSize(wxGRID_AUTOSIZE);
	mGrid->AutoSize();
	mGrid->Thaw();
	mGrid->Layout();
	mGrid->Refresh();
	mRenderer->Refresh();
	Layout();
}

void PTLayoutCtrl::FixDimensions(int &nr, int &nc)
{
	if (nr < 1) nr = 1;
	if (nc % 2) nc--;
	if (nc < 2) nc = 2;
	if (nc > 12) nc = 12;
	if (nr > 12 && nc > 2) nr = 12;
}


void PTLayoutCtrl::Set( const matrix_t<float> &data )
{
	if ( data.nrows() < 1 || data.ncols() < 2 ) return;

	mData.resize_fill( data.nrows(), data.ncols(), 0.0f );
	for( size_t r=0;r<data.nrows();r++ )
		for( size_t c=0;c<data.ncols();c++ )
			mData.at(r,c) = (double) data.at(r,c);
	
	int nr = (int)data.nrows();
	int nc = (int)data.ncols();
	FixDimensions( nr, nc );
	ResizeGrid( nr, nc );
}

void PTLayoutCtrl::Get( matrix_t<float> *data )
{
	data->resize_fill( mData.nrows(), mData.ncols(), 0.0f );
	for( size_t r=0;r<mData.nrows();r++ )
		for( size_t c=0;c<mData.ncols();c++ )
			data->at(r,c) = (float) mData.at(r,c);
}

void PTLayoutCtrl::SetGrid(const matrix_t<double> &data)
{
	int nr, nc;
	mData = data;

	nr = mData.nrows();
	nc = mData.ncols();

	FixDimensions(nr,nc);
	
	ResizeGrid(nr,nc);
}

matrix_t<double> PTLayoutCtrl::GetGrid()
{
	return mData;
}

int PTLayoutCtrl::NRows()
{
	return mData.nrows();
}

int PTLayoutCtrl::NCols()
{
	return mData.ncols();
}


void PTLayoutCtrl::OnSpanAngleChange(wxCommandEvent &evt)
{
	if (bSpanAngleEnabled)
	{
		double a = mNumSpan->Value();
		if (a <= 0.0) a = 0.0;
		if (a >= 360.0) a = 360.0;
	
		mSpanAngle = a;
		UpdateData();
		mNumSpan->SetValue(mSpanAngle);
		DispatchEvent();
	}
}

void PTLayoutCtrl::OnGridSizeChange(wxCommandEvent &evt)
{
	int nr, nc;

	nr = mNumRows->AsInteger();
	nc = mNumCols->AsInteger();
	
	FixDimensions(nr,nc);

	if (nr != mNumRows->AsInteger()) mNumRows->SetValue(nr);
	if (nc != mNumCols->AsInteger()) mNumCols->SetValue(nc);

	ResizeGrid(nr,nc);
	DispatchEvent();
}

void PTLayoutCtrl::ResizeGrid(int nrows, int ncols)
{
	mGrid->Freeze();

	if (mGrid->GetNumberRows() > nrows)
		mGrid->DeleteRows( 0, mGrid->GetNumberRows() - nrows );

	if (mGrid->GetNumberRows() < nrows)
		mGrid->AppendRows( nrows - mGrid->GetNumberRows() );

	if (mGrid->GetNumberCols() > ncols)
		mGrid->DeleteCols( 0,mGrid->GetNumberCols() - ncols );

	if (mGrid->GetNumberCols() < ncols)
		mGrid->AppendCols( ncols - mGrid->GetNumberCols() );


	mGrid->Thaw();

	matrix_t<double> old( mData );

	mData.resize_fill(nrows, ncols, 0.0);

	for( size_t r = 0; r<old.nrows() && r < nrows; r++ )
		for( size_t c=0; c<old.ncols() && c < ncols; c++ )
			mData.at(r,c) = old.at(r,c);
	
	if (mNumRows->AsInteger() != mData.nrows())
		mNumRows->SetValue( mData.nrows() );

	if (mNumCols->AsInteger() != mData.ncols())
		mNumCols->SetValue( mData.ncols() );

	UpdateData();
}

void PTLayoutCtrl::OnGridCellChange(wxGridEvent &evt)
{
	int r = evt.GetRow();
	int c = evt.GetCol();
	double val = atof( mGrid->GetCellValue(r,c).c_str() );

	if ( mData.ncols() > 2 && val < 0 ) val = 0;

	if ( mData.ncols() > 2 && r == 0 && val == 0 && mData.nrows() > 2 && mSpanAngle==360.0)
	{
		wxMessageBox("The ring of radial zones closest to the tower are not allowed to have 0 heliostats.");
		mGrid->SetCellValue( r, c, wxString::Format("%lg", mData.at(r,c) ));
		return;
	}

	if (mSpanAngle != 360.0 && c == mData.ncols()/2 && mData.ncols() > 2)
		val = 0;

	mData.at(r,c) = val;
	mGrid->SetCellValue( r, c, wxString::Format("%lg", val) );
	mRenderer->Refresh();

	DispatchEvent();
}

void PTLayoutCtrl::OnGridCellSelect(wxGridEvent &evt)
{
	int r = evt.GetRow();
	int c = evt.GetCol();

	mRenderer->Highlight(r,c);
	evt.Skip();
}

void PTLayoutCtrl::DispatchEvent()
{	
	wxCommandEvent change(wxEVT_PTLAYOUT_CHANGE, this->GetId() );
	change.SetEventObject( this );
	GetEventHandler()->ProcessEvent(change);
}


double PTLayoutCtrl::NumHeliostats()
{
	double sum = 0;
	if (IsZonal())
	{
		for (int r=0;r<mData.nrows();r++)
			for (int c=0;c<mData.ncols();c++)
				sum += mData.at(r,c);
	}
	else
		sum = mData.nrows();

	return sum;
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
	SetMinSize(wxSize(200,200));
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

void PTLayoutRenderer::OnResize(wxSizeEvent &evt)
{
	Refresh();
}

void PTLayoutRenderer::OnChar(wxKeyEvent &evt)
{
}

void PTLayoutRenderer::OnMouseDown(wxMouseEvent &evt)
{
	bMouseDown = true;
}


void PTLayoutRenderer::OnMouseUp(wxMouseEvent &evt)
{
	bMouseDown = false;
}

void PTLayoutRenderer::OnMouseMove(wxMouseEvent &evt)
{
}

void PTLayoutRenderer::OnPaint(wxPaintEvent &evt)
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
	matrix_t<double> &mData = mPTCtrl->mData;

	int nrad = mData.nrows();
	int nazm = mData.ncols();

	double fspan = mPTCtrl->mSpanAngle;
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

	wxPen p = wxPen(*wxLIGHT_GREY, 2, wxDOT);
	dc.SetPen(p);
	dc.DrawLine(cir_x, geom.y, cir_x, geom.y+geom.height);
	dc.DrawLine(geom.x, cir_y, geom.x+geom.width, cir_y);

	dc.SetPen(*wxBLACK_PEN);
	dc.SetBrush(*wxTRANSPARENT_BRUSH);

	dc.DrawCircle(cir_x, cir_y, (int)max_radius);

	int i,r,c,a;

	double maxval = -1e99;
	double rlo = 1.0, save, z;
	matrix_t<double> uf1;
	uf1.resize_fill(nrad, nazm, 0.0);

	for (r=0;r<nrad;r++)
	{
		for (a=0;a<nazm;a++)
		{
			if (r==0)
				z = 2.0/(rlo+1.5)*mData.at(r,a);
			else if (r==nrad-1)
				z = 2.0/(nrad-0.5+rlo)*mData.at(r,a);
			else
				z = mData.at(r,a)/(r+1.0+rlo);

			uf1.at(r,a) = z;
			if ( z > maxval ) maxval = z;
		}
	}

	int ncv = mData.nrows() * mData.ncols();

	double rdelta = (max_radius-min_radius)/nrad;
	double radius = max_radius;

	double adelta = zspan;
	double angle;

	for (r=nrad-1;r>=0;r--)
	{
		angle = -adelta/2+90;
		for (a=0;a<nazm;a++)
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
			if (a==n_empty)
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

	dc.SetFont(wxFont(7, wxSWISS, wxNORMAL, wxBOLD));
	if (dc.GetCharHeight() > rdelta)
		dc.SetFont(*wxSMALL_FONT);

	wxString buf;


	double dispangle = 0;
	angle = -adelta/2-90;
	for (a=0;a<nazm;a++)
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
	dc.SetFont(wxFont(7, wxSWISS, wxNORMAL, wxBOLD));
	if (dc.GetCharHeight() > rdelta)
		dc.SetFont(*wxSMALL_FONT);


	for ( r=0;r<nrad;r++ )
	{
		buf = wxString::Format("%d", (int)(r+1));
		dc.GetTextExtent(buf, &tw, &th);
		dc.DrawText(buf, cir_x-tw/2, cir_y - (int)min_radius - r*rdelta - rdelta/2 - th/2);
	}
}

void PTLayoutRenderer::DrawXY(wxDC &dc, const wxRect &geom)
{
	matrix_t<double> &mData = mPTCtrl->mData;

	int nhel = mData.nrows();
	int i,r,c,a;

	int cx = geom.x+geom.width/2;
	int cy = geom.y+geom.height/2;



	double xmin,xmax;
	double ymin,ymax;
	xmin=ymin=1e99;
	xmax=ymax=-1e99;
	for (r=0;r<nhel;r++)
	{
		if (mData.at(r,0) < xmin) xmin = mData.at(r,0);
		if (mData.at(r,0) > xmax) xmax = mData.at(r,0);
		if (mData.at(r,1) < ymin) ymin = mData.at(r,1);
		if (mData.at(r,1) > ymax) ymax = mData.at(r,1);
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
		double x = mData.at(r,0);
		double y = mData.at(r,1);

		double propx = (x-xmin)/xrange;
		double propy = (y-ymin)/yrange;

		int px = geom.x+3 + (int)(geom.width-6)*propx;
		int py = geom.y+3 + geom.height-6 - (int)(geom.height-6)*propy;

		dc.DrawCircle(px,py,2);
	}

	wxPen p = *wxLIGHT_GREY_PEN;
	p.SetStyle(wxDOT);
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
