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

	int nMaxTypes;
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

