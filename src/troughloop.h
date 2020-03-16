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

#ifndef __trloopctrl_h
#define __trloopctrl_h

#include <vector>

#include <wx/wx.h>
#include <wx/grid.h>
#include <wx/overlay.h>

#include "object.h"

BEGIN_DECLARE_EVENT_TYPES()
	DECLARE_EVENT_TYPE( wxEVT_TRLOOP_CHANGE, 0)
END_DECLARE_EVENT_TYPES()

class wxNumericCtrl;
class TRLoopRenderer;
class VarValue;

#define EVT_TRLOOP(id,func) EVT_COMMAND(id, wxEVT_TRLOOP_CHANGE, func)

class TRLoopCtrl : public wxPanel
{
friend class TRLoopRenderer;
public:
	TRLoopCtrl(wxWindow *parent, int id, const wxPoint &pos=wxDefaultPosition, const wxSize &sz=wxDefaultSize);
	virtual ~TRLoopCtrl();

	void LoopData(const std::vector<int> &dat);
	std::vector<int> LoopData();
	
	int NumSCAs();
	
	void ResetDefocus();


private:

	void DispatchEvent();
	void OnNSCAChange(wxCommandEvent &evt);
	void OnResetDefocus(wxCommandEvent &evt);

	struct LoopNode
	{
		LoopNode() {
			ColT=HceT=Defocus=1;
			hovering=selected=false;
			nrow=ncol=-1;
		}

		int ColT;
		int HceT;
		int Defocus;

		wxRect geom;
		bool hovering;
		bool selected;
		int nrow,ncol;
	};

	std::vector<LoopNode> mLoop;

	wxNumericCtrl *mNumSCAs;
	TRLoopRenderer *mRenderer;
	wxRadioButton *mEditModeSCA;
	wxRadioButton *mEditModeHCE;
	wxRadioButton *mEditModeDF;

	DECLARE_EVENT_TABLE();
};


class TRLoopRenderer : public wxWindow
{
public:
	TRLoopRenderer(TRLoopCtrl *parent);

	void RepositionAll();

	void DrawMultiSelBox();

	void OnPaint(wxPaintEvent &evt);
	void OnResize(wxSizeEvent &evt);
	void OnChar(wxKeyEvent &evt);
	void OnMouseDown(wxMouseEvent &evt);
	void OnMouseUp(wxMouseEvent &evt);
	void OnMouseMove(wxMouseEvent &evt);
	void OnLeave(wxMouseEvent &evt);

	wxColour Colour1, Colour2;
	wxColour HighlightColour;
	wxColour SelectColour;
	wxColour ColourSet[4];

private:
	wxOverlay m_overlay;

	bool bMultiSelMode;
	int mOrigX, mOrigY, mDiffX, mDiffY;
	bool bMouseDown;
	TRLoopCtrl *mTRCtrl;
	DECLARE_EVENT_TABLE()
};

#endif

