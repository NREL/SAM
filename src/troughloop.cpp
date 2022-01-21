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


#include <wx/clipbrd.h>
#include <wx/dcbuffer.h>
#include <wx/filename.h>

#include <wex/numeric.h>
#include <wex/utils.h>

#include "troughloop.h"


#ifndef MIN
#define MIN(a, b) (a < b ? a : b)
#endif

#ifndef MAX
#define MAX(a, b) (a > b ? a : b)
#endif

#ifndef CLAMP
#define CLAMP(x,min,max) (x<min ? min : (x>max?max:x))
#endif


enum { IDTRL_NSCAS = wxID_HIGHEST+591, IDTRL_EDITSCA, IDTRL_EDITHCE, IDTRL_EDITDF, IDTRL_RESETDF };

BEGIN_EVENT_TABLE(TRLoopCtrl, wxPanel)
	EVT_NUMERIC( IDTRL_NSCAS, TRLoopCtrl::OnNSCAChange )
	EVT_BUTTON( IDTRL_RESETDF, TRLoopCtrl::OnResetDefocus )
END_EVENT_TABLE()

DEFINE_EVENT_TYPE( wxEVT_TRLOOP_CHANGE )

TRLoopCtrl::TRLoopCtrl(wxWindow *parent, int id, const wxPoint &pos, const wxSize &sz)
 : wxPanel(parent, id, pos, sz, wxCLIP_CHILDREN)
{
	mLoop.resize(8);
	for (size_t i=0;i<mLoop.size();i++)
	{
		mLoop[i].ColT = 1;
		mLoop[i].HceT = 1;
		mLoop[i].Defocus = mLoop.size()-i;
	}

	mRenderer = new TRLoopRenderer(this);
	mNumSCAs = new wxNumericCtrl(this, IDTRL_NSCAS, mLoop.size(), wxNUMERIC_INTEGER);

	mEditModeSCA = new wxRadioButton(this, IDTRL_EDITSCA, "Edit SCAs", wxDefaultPosition, wxDefaultSize, wxRB_GROUP);
	mEditModeHCE = new wxRadioButton(this, IDTRL_EDITHCE, "Edit HCEs");
	mEditModeDF = new wxRadioButton(this, IDTRL_EDITDF, "Edit Defocus Order");

	mEditModeSCA->SetValue(true);

	wxButton *resetdf = new wxButton(this, IDTRL_RESETDF, "Reset Defocus");

	wxBoxSizer *box_horiz = new wxBoxSizer(wxHORIZONTAL);
	box_horiz->Add( new wxStaticText(this,-1,"Number of SCA/HCE assemblies per loop:"), 0, wxALL|wxEXPAND,2);
	box_horiz->Add( mNumSCAs,0, wxALL|wxEXPAND,2);
	box_horiz->Add( mEditModeSCA,0,wxALL|wxEXPAND, 2);
	box_horiz->Add( mEditModeHCE,0,wxALL|wxEXPAND, 2);
	box_horiz->Add( mEditModeDF,0,wxALL|wxEXPAND, 2);
	box_horiz->Add( resetdf,0,wxALL|wxEXPAND,2);

	wxBoxSizer *box_main = new wxBoxSizer(wxVERTICAL);
	box_main->Add( box_horiz, 0, wxALL|wxEXPAND,0 );
	box_main->Add( mRenderer, 1, wxALL|wxEXPAND,0 );

	SetSizer(box_main);
	mRenderer->RepositionAll();
}

TRLoopCtrl::~TRLoopCtrl()
{
	/* nothing to do */
}

void TRLoopCtrl::LoopData(const std::vector<int> &dat)
{
	if (dat.size() < 2) return;
	int nsca = dat[0];
	if ((int)dat.size() != nsca*3+1) return;

	mLoop.resize(nsca);
	for (size_t i=0;i<mLoop.size();i++)
	{
		int idx = i*3+1;
		mLoop[i].ColT = CLAMP( dat[idx], 1, 4 );
		mLoop[i].HceT = CLAMP( dat[idx+1], 1, 4 );
		mLoop[i].Defocus = CLAMP( dat[idx+2], 1, nsca );
	}

	if (mLoop.size() > 32)
		mLoop.resize(32);

	// update controls
	mNumSCAs->SetValue( mLoop.size() );
	mRenderer->RepositionAll();

}

std::vector<int> TRLoopCtrl::LoopData()
{
	std::vector<int> dat;
	dat.reserve( mLoop.size()*3+1 );

	dat.push_back( mLoop.size() );
	for (size_t i=0;i<mLoop.size();i++)
	{
		dat.push_back( mLoop[i].ColT );
		dat.push_back( mLoop[i].HceT );
		dat.push_back( mLoop[i].Defocus );
	}

	return dat;
}

int TRLoopCtrl::NumSCAs()
{
	return mLoop.size();
}

void TRLoopCtrl::ResetDefocus()
{	
	for (size_t i=0;i<mLoop.size();i++)
		mLoop[i].Defocus = mLoop.size()-i;
}

void TRLoopCtrl::OnResetDefocus(wxCommandEvent &)
{
	ResetDefocus();
	mRenderer->Refresh();
}

void TRLoopCtrl::DispatchEvent()
{
	wxCommandEvent change(wxEVT_TRLOOP_CHANGE, this->GetId() );
	change.SetEventObject( this );
	GetEventHandler()->ProcessEvent(change);
}

void TRLoopCtrl::OnNSCAChange(wxCommandEvent &)
{
	int nsca = CLAMP(mNumSCAs->AsInteger(),1,32);
	if (nsca != mNumSCAs->AsInteger()) mNumSCAs->SetValue(nsca);

	mLoop.resize(nsca);
	ResetDefocus();

	mRenderer->RepositionAll();
	DispatchEvent();

	//wxMessageBox( Format("Num scas = %d", nsca) );
}



/************ RENDERER ****************/

BEGIN_EVENT_TABLE( TRLoopRenderer, wxWindow )

	EVT_PAINT( TRLoopRenderer::OnPaint )
	EVT_SIZE( TRLoopRenderer::OnResize )
	EVT_CHAR( TRLoopRenderer::OnChar )
	EVT_LEFT_DOWN( TRLoopRenderer::OnMouseDown )
	EVT_LEFT_UP( TRLoopRenderer::OnMouseUp )
	EVT_MOTION( TRLoopRenderer::OnMouseMove )
	EVT_LEAVE_WINDOW( TRLoopRenderer::OnLeave )

END_EVENT_TABLE()

TRLoopRenderer::TRLoopRenderer(TRLoopCtrl *parent)
	: wxWindow(parent, -1, wxDefaultPosition, wxDefaultSize, wxCLIP_CHILDREN|wxWANTS_CHARS)
{
	mTRCtrl = parent;

	SetBackgroundStyle(wxBG_STYLE_CUSTOM);
	SetMinSize(wxScaleSize(600,200));

	bMouseDown = false;
	bMultiSelMode = false;
	mOrigX = mOrigY = mDiffX = mDiffY = 0;


	Colour1 = *wxBLUE;
	Colour2 = *wxRED;
	HighlightColour = wxColour(224,232,246);
	SelectColour = wxColour(193,210,238);

	ColourSet[0] = "grey";
	ColourSet[1] = "forest green";
	ColourSet[2] = "sienna";
	ColourSet[3] = "plum";
}

void TRLoopRenderer::RepositionAll()
{
	int cwidth, cheight;
	GetClientSize(&cwidth,&cheight);
	
	// minimum width for each trough should be 100 pixels + 30 (15 each side for tube gradient)
	// calculate number of troughs per row based on control size

	int numcols = cwidth / 130;

	int nrow=0;
	int ncol=0;
	int colincr=1;
	for (size_t i=0;i<mTRCtrl->mLoop.size();i++)
	{
		mTRCtrl->mLoop[i].nrow = nrow;
		mTRCtrl->mLoop[i].ncol = ncol;
		mTRCtrl->mLoop[i].geom = wxRect( ncol*130, nrow*60, 130, 60 );

		ncol += colincr;
		if (ncol == numcols || ncol < 0)
		{
			if (ncol == numcols) 
			{
				ncol--;
			}

			if (ncol < 0) 
			{
				ncol++;
			}
			colincr = 0-colincr;
			nrow++;
		}
	}


	Refresh();
}

void TRLoopRenderer::OnPaint(wxPaintEvent &)
{
	wxAutoBufferedPaintDC pdc(this);

	int cw, ch;
	GetClientSize(&cw,&ch);
	wxRect geom(0,0,cw,ch);
	
	pdc.SetBrush(*wxWHITE_BRUSH);
	pdc.SetPen(*wxWHITE_PEN);
	pdc.DrawRectangle(0, 0, cw, ch);

	// * render troughs sequence * //

	wxColour tubecolour(Colour1);

	bool left2right = true;

	for (size_t i=0;i<mTRCtrl->mLoop.size();i++)
	{
		TRLoopCtrl::LoopNode &lpn = mTRCtrl->mLoop[i];
		
		if (i>0 && lpn.nrow != mTRCtrl->mLoop[i-1].nrow)
				left2right = !left2right;

		if (lpn.selected)
		{
			pdc.SetPen(wxPen(SelectColour));
			pdc.SetBrush(wxBrush(SelectColour));
			pdc.DrawRectangle(lpn.geom.x, lpn.geom.y, lpn.geom.width, lpn.geom.height);
		}

		pdc.SetBrush(wxBrush(tubecolour));
		pdc.SetPen(wxPen(tubecolour));
		if (left2right)
			pdc.DrawRectangle(lpn.geom.x, lpn.geom.y+lpn.geom.height/2-5, 15, 10);
		else
			pdc.DrawRectangle(lpn.geom.x+lpn.geom.width-15, lpn.geom.y+lpn.geom.height/2-5, 15, 10);

		// increment tube color towards red

		wxColour prevtubecolour = tubecolour;

		int ncv = mTRCtrl->mLoop.size();
		int nidx = i+1;
		tubecolour.Set(
			((ncv-nidx) * Colour1.Red()   +
				nidx * Colour2.Red())/ncv,

			((ncv-nidx) * Colour1.Green() +
				nidx * Colour2.Green())/ncv,

			((ncv-nidx) * Colour1.Blue()  +
				nidx * Colour2.Blue())/ncv );

		pdc.SetBrush(wxBrush(tubecolour));
		pdc.SetPen(wxPen(tubecolour));

		if (left2right)
			pdc.DrawRectangle(lpn.geom.x+lpn.geom.width-15, lpn.geom.y+lpn.geom.height/2-5, 15, 10);
		else
			pdc.DrawRectangle(lpn.geom.x, lpn.geom.y+lpn.geom.height/2-5, 15, 10);

		if (i>0)
		{
			TRLoopCtrl::LoopNode &lpnp = mTRCtrl->mLoop[i-1];
			if (lpn.nrow != mTRCtrl->mLoop[i-1].nrow)
			{
				int xcoord = 0;
				if (lpn.ncol != 0) xcoord = lpn.geom.x+lpn.geom.width-10;

				if (!left2right)
				{
					pdc.SetBrush(wxBrush(prevtubecolour));
					pdc.SetPen(wxPen(prevtubecolour));
				}

				pdc.DrawRectangle(xcoord, lpnp.geom.y+lpnp.geom.height/2,
						10, lpn.geom.y-lpnp.geom.y);

			}
		}

		pdc.SetBrush(*wxWHITE_BRUSH);
		pdc.SetPen(*wxWHITE_PEN);
		pdc.DrawRectangle( lpn.geom.x+15, lpn.geom.y+4, 
			lpn.geom.width-30, lpn.geom.height-8);

		int halfh = lpn.geom.height/2-4;
		wxRect tophalf(lpn.geom.x+15, lpn.geom.y+4, 
			lpn.geom.width-30, halfh);
		wxRect bothalf = tophalf;
		bothalf.y += halfh;

		pdc.GradientFillLinear(tophalf, ColourSet[lpn.ColT-1], *wxWHITE, wxSOUTH);
		pdc.GradientFillLinear(bothalf, *wxWHITE, ColourSet[lpn.ColT-1], wxSOUTH);

		pdc.SetBrush(wxBrush(ColourSet[lpn.HceT-1]));
		wxRect hcebox(tophalf.x, tophalf.y+tophalf.height-3, tophalf.width, 6);
		pdc.DrawRectangle(hcebox.x, hcebox.y, hcebox.width, hcebox.height);

		pdc.SetPen(*wxBLACK_PEN);
		pdc.SetBrush(*wxTRANSPARENT_BRUSH);
		pdc.DrawRectangle( lpn.geom.x+15, lpn.geom.y+4, 
			lpn.geom.width-30, lpn.geom.height-8);


		pdc.SetTextForeground(*wxBLACK);
		wxFont f = *wxNORMAL_FONT;
		f.SetWeight(wxFONTWEIGHT_BOLD);
		pdc.SetFont(f);
		pdc.DrawText(wxString::Format("SCA: %d", lpn.ColT), tophalf.x+2, tophalf.y+2);
		pdc.DrawText(wxString::Format("HCE: %d", lpn.HceT), bothalf.x+2, bothalf.y+bothalf.height-pdc.GetCharHeight()-2);
		

		pdc.SetPen(*wxWHITE_PEN);
		pdc.SetBrush(*wxWHITE_BRUSH);
		
		pdc.SetFont(*wxNORMAL_FONT);

		wxString dftxt = wxString::Format("DF# %d", lpn.Defocus);
		int bxw;
		pdc.GetTextExtent("DF# MM", &bxw, NULL);
		bxw += 4;
		wxRect dftxtbox( tophalf.x+tophalf.width-bxw, bothalf.y+bothalf.height-pdc.GetCharHeight()-4, bxw, pdc.GetCharHeight()+2);

		pdc.DrawRectangle(dftxtbox.x, dftxtbox.y, dftxtbox.width, dftxtbox.height);
		pdc.SetPen(*wxBLACK_PEN);
		pdc.SetBrush(*wxTRANSPARENT_BRUSH);
		pdc.DrawRectangle(dftxtbox.x, dftxtbox.y, dftxtbox.width, dftxtbox.height);
		pdc.DrawText(dftxt, dftxtbox.x+2, dftxtbox.y+1);

		//gfx.DrawString(lpn.geom.x+30, lpn.geom.y+30, 
		//	wxString::Format("n: %d (r%d c%d)", i, lpn.nrow, lpn.ncol));

	}

	wxRect tube(0,ch/2-10,cw,20);
	//gfx.GetDC()->GradientFillLinear( tube, ColourCold, ColourHot );


	pdc.SetPen(*wxBLACK_PEN);
	pdc.SetBrush(*wxTRANSPARENT_BRUSH);
	//gfx.DrawRectangle(tube.x,tube.y,tube.width,tube.height,false);
	pdc.DrawRectangle(0, 0, cw, ch);
}

void TRLoopRenderer::OnResize(wxSizeEvent &)
{
	RepositionAll();
}

void TRLoopRenderer::OnChar(wxKeyEvent &evt)
{
	int key = evt.GetKeyCode();

	if (key == 'C')
	{
		if (wxTheClipboard->Open())
		{
			wxString buf;
			std::vector<int> data = mTRCtrl->LoopData();
			for( size_t i=0;i<data.size();i++ )
			{
				buf += wxString::Format("%d", data[i] );
				if ( i < data.size()-1 ) buf += ",";
			}

			wxTheClipboard->SetData( new wxTextDataObject( buf ) );
			wxTheClipboard->Close();
		}
		return;
	}

	if (key == 's' || key == 'S' || key == 'c' || key == 'C')
		mTRCtrl->mEditModeSCA->SetValue(true);
	else if (key == 'h' || key == 'H' || key == 'r' || key == 'R')
		mTRCtrl->mEditModeHCE->SetValue(true);
	else if (key == 'd' || key == 'D')
		mTRCtrl->mEditModeDF->SetValue(true);

	if (key < '0' || key > '9') return;

	int mode = 0;
	if (mTRCtrl->mEditModeHCE->GetValue()) mode = 1;
	if (mTRCtrl->mEditModeDF->GetValue()) mode = 2;

	int val = key-'0';

	bool modified = false;
	// adjust .selected parameters of items
	for (size_t i=0;i<mTRCtrl->mLoop.size();i++)
	{
		TRLoopCtrl::LoopNode &lpn = mTRCtrl->mLoop[i];
		if (lpn.selected)
		{
			switch(mode)
			{
			case 0: //sca
				lpn.ColT = CLAMP(val,1,4);
				modified = true;
				break;
			case 1: //hce
				lpn.HceT = CLAMP(val,1,4);
				modified = true;
				break;
			}
		}
	}

	if (modified)
		mTRCtrl->DispatchEvent();

	Refresh();
}

void TRLoopRenderer::OnMouseDown(wxMouseEvent &evt)
{
	this->SetFocus();

	bMouseDown = true;
	int mx = evt.GetX();
	int my = evt.GetY();
	mOrigX = mx;
	mOrigY = my;

	// clear selections
	for (size_t i=0;i<mTRCtrl->mLoop.size();i++)
	{
		TRLoopCtrl::LoopNode &lpn = mTRCtrl->mLoop[i];
		lpn.selected = false;
		if (lpn.geom.Contains(mx,my))
			lpn.selected = true;
	}

	Refresh();

	
	if (mTRCtrl->mEditModeDF->GetValue())
	{
		for (size_t i=0;i<mTRCtrl->mLoop.size();i++)
		{
			TRLoopCtrl::LoopNode &lpn = mTRCtrl->mLoop[i];
			if (lpn.selected)
			{
				wxString result = wxGetTextFromUser(
					wxString::Format("Enter defocus order for assembly #%d:\n(Collector: %d, Receiver: %d)",
						(int)(i+1), lpn.ColT, lpn.HceT),
					"Defocus Order", wxString::Format("%d", lpn.Defocus), this);

				if (result.IsEmpty()) return;
				lpn.Defocus = CLAMP( wxAtoi(result), 1, (int)mTRCtrl->mLoop.size() );
				mTRCtrl->DispatchEvent();
				Refresh();
			}
		}
	}
	else
	{
		bMultiSelMode = true;
		mDiffX = 0;
		mDiffY = 0;
		ClientToScreen(&mOrigX, &mOrigY);
	}
}

void TRLoopRenderer::OnMouseUp(wxMouseEvent &)
{
	if (bMultiSelMode)
	{
		wxRect selbox;	
		selbox.x = mDiffX<0 ? mOrigX + mDiffX : mOrigX;
		selbox.width = mDiffX<0 ? -mDiffX : mDiffX;
		selbox.y = mDiffY<0 ? mOrigY + mDiffY : mOrigY;
		selbox.height = mDiffY<0 ? -mDiffY : mDiffY;

		ScreenToClient(&selbox.x, &selbox.y);

		if (selbox.width > 0 && selbox.height > 0)
		{
			// adjust .selected parameters of items
			for (size_t i=0;i<mTRCtrl->mLoop.size();i++)
			{
				TRLoopCtrl::LoopNode &lpn = mTRCtrl->mLoop[i];
				lpn.selected = selbox.Intersects( lpn.geom );
			}
		}

		bMultiSelMode = false;
//		m_overlay.Reset();
		Refresh();
	}


	bMouseDown = false;
}

void TRLoopRenderer::OnMouseMove(wxMouseEvent &evt)
{
	int mx = evt.GetX();
	int my = evt.GetY();

	int xroot = mx;
	int yroot = my;
	ClientToScreen(&xroot, &yroot);

	if (bMultiSelMode)
	{
		mDiffX = xroot - mOrigX;
		mDiffY = yroot - mOrigY;
		DrawMultiSelBox();
	}
}

void TRLoopRenderer::OnLeave(wxMouseEvent &)
{
}

void TRLoopRenderer::DrawMultiSelBox()
{
	wxClientDC dc(this);
	wxDCOverlay overlaydc( m_overlay, &dc );
	overlaydc.Clear();
#ifdef __WXOSX__
	wxBrush brush( wxColour(240,240,240,130) );
#else
	wxBrush brush( *wxWHITE, wxBRUSHSTYLE_TRANSPARENT );
#endif
	wxPen pen( wxColour(90,90,90) );
	pen.SetCap(wxCAP_BUTT);
	pen.SetJoin(wxJOIN_MITER);

	dc.SetBrush(brush);
	dc.SetPen(pen);

	wxRect selbox;	
	selbox.x = mDiffX<0 ? mOrigX + mDiffX : mOrigX;
	selbox.width = mDiffX<0 ? -mDiffX : mDiffX;
	selbox.y = mDiffY<0 ? mOrigY + mDiffY : mOrigY;
	selbox.height = mDiffY<0 ? -mDiffY : mDiffY;

	ScreenToClient(&selbox.x, &selbox.y);
	
	dc.DrawRectangle(selbox);

}



