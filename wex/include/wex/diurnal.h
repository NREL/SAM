#ifndef __diurnal_h
#define __diurnal_h

#include <vector>
#include <wx/window.h>



// Diurnal Period control from sched control in wex

BEGIN_DECLARE_EVENT_TYPES()
	DECLARE_EVENT_TYPE(wxEVT_DIURNALPERIODCTRL_CHANGE, 0)
END_DECLARE_EVENT_TYPES()

#define EVT_DIURNALPERIODCTRL(id, func) EVT_COMMAND(id, wxEVT_DIURNALPERIODCTRL_CHANGE, func)

class wxDiurnalPeriodCtrl : public wxWindow
{
public:
	wxDiurnalPeriodCtrl(wxWindow *parent, int id, const wxPoint &pos = wxDefaultPosition, const wxSize &sz = wxDefaultSize);
	virtual ~wxDiurnalPeriodCtrl();

	bool Enable(bool enable = true);

	void SetupTOUGrid();
	void SetupDefaultColours();

	void AddColour(const wxColour &c);
	bool GetColour(int i, wxColour &c);
	void SetMinMax(int min, int max, bool clamp = false);
	void Set(size_t r, size_t c, int val);
	void Set(int val);
	int Get(size_t r, size_t c) const;
	void AddRowLabel(const wxString &s);
	void AddColLabel(const wxString &s);
	void ClearLabels();
	void ClearRowLabels();
	void ClearColLabels();
	void SetData( float *data, size_t nr, size_t nc );
	float *GetData( size_t *nr, size_t *nc );

	bool Schedule(const wxString &sched);
	wxString Schedule() const;

	void SetMin(int min); 
	int GetMin();

	void SetMax(int max); 
	int GetMax();
	
	static int ScheduleCharToInt(char c);
	static char ScheduleIntToChar(int d);
	
	virtual wxSize DoGetBestSize() const;

private:


	void OnErase(wxEraseEvent &);
	void OnPaint(wxPaintEvent &evt);
	void OnResize(wxSizeEvent &evt);
	void OnKeyDown(wxKeyEvent &evt);
	void OnChar(wxKeyEvent &evt);
	void OnMouseDown(wxMouseEvent &evt);
	void OnMouseUp(wxMouseEvent &evt);
	void OnMouseMove(wxMouseEvent &evt);
	void OnLostFocus(wxFocusEvent &evt);

#ifdef __WXMSW__
	virtual bool MSWShouldPreProcessMessage(WXMSG* msg);
#endif


	void Copy();
	void Paste();

	static const size_t m_nrows = 12;
	static const size_t m_ncols = 24;

	float m_data[m_nrows*m_ncols];
	int m_cols;

	bool m_hasFocus, m_mouseDown;
	int m_rowHeaderSize, m_colHeaderSize, m_cellSize;

	std::vector<wxColour> m_colours;
	int m_selStartR, m_selStartC, m_selEndR, m_selEndC;
	wxArrayString m_rowLabels;
	wxArrayString m_colLabels;
	bool m_colLabelsVertical;
	int m_min, m_max;

	void UpdateLayout();

	DECLARE_EVENT_TABLE()
};

#endif
