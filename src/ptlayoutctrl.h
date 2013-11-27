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

	void LoadFromFile();
	void SaveToFile();

	double NumHeliostats();

	void Set( const matrix_t<float> &data );
	void Get( matrix_t<float> *data );

	void SetGrid(const matrix_t<double> &data);
	matrix_t<double> GetGrid();
	int NRows();
	int NCols();

	void EnableSpanAngle(bool b);
	bool IsSpanAngleEnabled();
	void SetSpanAngle(double a);
	double GetSpanAngle(); 

	bool IsXY();
	bool IsZonal();

private:
	
	void FixDimensions(int &nr, int &nc);
	void DispatchEvent();
	void UpdateData();
	void ResizeGrid(int nrows, int ncols);

	void OnSpanAngleChange(wxCommandEvent &evt);
	void OnGridSizeChange(wxCommandEvent &evt);
	void OnGridCellChange(wxGridEvent &evt);
	void OnGridCellSelect(wxGridEvent &evt);

	matrix_t<double> mData;
	double mSpanAngle;

	bool bSpanAngleEnabled;

	PTLayoutRenderer *mRenderer;
	wxGrid *mGrid;
	wxNumericCtrl *mNumRows;
	wxNumericCtrl *mNumCols;
	wxNumericCtrl *mNumSpan;
	wxStaticText *mLblRows;
	wxStaticText *mLblCols;
	wxStaticText *mLblSpan;

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
