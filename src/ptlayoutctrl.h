#ifndef __ptlayoutctrl_h
#define __ptlayoutctrl_h


#include <wx/wx.h>
#include <wx/listctrl.h>
#include <wx/splitter.h>
#include <wx/aui/auibook.h>
#include <wx/imaglist.h>

#include <wex/extgrid.h>
class wxNumericCtrl;

#include "object.h"


BEGIN_DECLARE_EVENT_TYPES()
	DECLARE_EVENT_TYPE( wxEVT_PTLAYOUT_CHANGE, 0)
END_DECLARE_EVENT_TYPES()

class PTLayoutRenderer;

#define EVT_PTLAYOUT(id, func) EVT_COMMAND(id, wxEVT_PTLAYOUT_CHANGE, func)

class PTLayoutCtrl : public wxPanel
{
friend class PTLayoutRenderer;
public:
	PTLayoutCtrl(wxWindow *parent, int id, const wxPoint &pos = wxDefaultPosition, const wxSize &sz = wxDefaultSize);
	virtual ~PTLayoutCtrl();
	
	float NumHeliostats();

	
	void SetGrid( const matrix_t<float> &data );
	matrix_t<float> GetGrid() { return m_data; }

	size_t NRows() { return m_data.nrows(); }
	size_t NCols() { return m_data.ncols(); }

	void EnableSpanAngle(bool b);
	bool IsSpanAngleEnabled() { return m_spanAngleEnabled; }
	void SetSpanAngle(float a);
	float GetSpanAngle() { return m_spanAngle; }

	bool IsXY() { return m_data.ncols() == 2; }
	bool IsZonal() { return m_data.ncols() > 2; } 

private:
	
	void FixDimensions(size_t &nr, size_t &nc);
	void DispatchEvent();
	void UpdateData();
	void ResizeGrid(size_t nrows, size_t ncols);

	void OnSpanAngleChange(wxCommandEvent &evt);
	void OnGridSizeChange(wxCommandEvent &evt);
	void OnGridCellChange(wxGridEvent &evt);
	void OnGridCellSelect(wxGridEvent &evt);

	void OnButton( wxCommandEvent & );

	matrix_t<float> m_data;
	float m_spanAngle;
	bool m_spanAngleEnabled;

	PTLayoutRenderer *m_renderer;

	wxGrid *m_grid;
	wxNumericCtrl *m_numRows;
	wxNumericCtrl *m_numCols;
	wxNumericCtrl *m_numSpan;
	wxStaticText *m_lblRows;
	wxStaticText *m_lblCols;
	wxStaticText *m_lblSpan;

	DECLARE_EVENT_TABLE()
};

class PTLayoutRenderer : public wxWindow
{
public:
	PTLayoutRenderer(PTLayoutCtrl *parent);

	void OnPaint(wxPaintEvent &evt);
	void OnResize(wxSizeEvent &evt);
	void OnChar(wxKeyEvent &evt);
	void OnMouseDown(wxMouseEvent &evt);
	void OnMouseUp(wxMouseEvent &evt);
	void OnMouseMove(wxMouseEvent &evt);

	wxColour Colour1, Colour2;

	void Highlight(int rad, int azm);	
	void ComputeColour(wxColour &c, int cntrIndex, int ncv);
	void DrawZonal(wxDC &dc, const wxRect &geom);
	void DrawXY(wxDC &dc, const wxRect &geom);
	
private:
	int hlRad, hlAzm;
	bool bMouseDown;
	PTLayoutCtrl *mPTCtrl;

	DECLARE_EVENT_TABLE()


};

#endif
