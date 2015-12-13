#ifndef __uiwidgets_h
#define __uiwidgets_h

#include <vector>

#include <wx/window.h>
#include <wx/panel.h>
#include <wx/button.h>
#include <wx/grid.h>

#include <wex/numeric.h>

#include "object.h"

class wxTextCtrl;
class wxStaticText;
class wxListBox;
class wxExtGridCtrl;
class AFDataMatrixTable;

static wxColour UIColorIndicatorFore(60, 60, 60);
static wxColour UIColorIndicatorBack(230, 230, 230);
static wxColour UIColorCalculatedFore(29, 80, 173);
static wxColour UIColorCalculatedBack(222, 233, 244);

class AFSchedNumeric : public wxWindow
{
public:
	AFSchedNumeric( wxWindow *parent, int id, const wxPoint &pos = wxDefaultPosition, const wxSize &size = wxDefaultSize);

	bool UseSchedule();
	void UseSchedule(bool b);
	bool ScheduleOnly();
	void ScheduleOnly(bool b);
	void SetLabel( const wxString &s ) { m_label = s; }
	wxString GetLabel() { return m_label; }
	void SetDescription( const wxString &desc ) { m_desc = desc; }
	wxString GetDescription() { return m_desc; }
	double GetValue();
	void SetValue(double d);
	void SetFormat( int deci, bool thousep, const wxString &pre, const wxString &post );
	std::vector<double> GetSchedule();
	void GetSchedule( std::vector<float> *vals );
	int GetSchedLen();
	void SetSchedule(const std::vector<double> &s);
	void SetSchedule( const std::vector<float> &s );
	void SetSchedLen(int len);
	void SetFixedLen(int len=-1);

private:
	void OnResize(wxSizeEvent &evt);
	void OnPaint(wxPaintEvent &evt);
	void OnClick(wxMouseEvent &evt);
	void OnEditSchedule(wxCommandEvent &evt);
	void OnNumChanged(wxCommandEvent &evt);

	void FireChangedEvent();

	bool bUseSchedule;
	bool bScheduleOnly;
	wxButton *mBtnEditSched;
	wxNumericCtrl *mFixedValue;
	std::vector<double> mSchedValues;
	wxString m_label;
	int m_fixedLen;
	wxString m_desc;
	int m_switchWidth;

	void UpdateSwitchWidth();

	DECLARE_EVENT_TABLE();
};


#define EVT_MONTHLYFACTOR(i,f) EVT_BUTTON(i,f)
class AFMonthlyFactorCtrl : public wxButton
{
public:
	AFMonthlyFactorCtrl( wxWindow *parent, int id, 
		const wxPoint &pos=wxDefaultPosition, const wxSize &size=wxDefaultSize);

	std::vector<float> Get( );
	void Set( const std::vector<float> &data );
	void SetDescription(const wxString &s);
	wxString GetDescription();
	void OnPressed(wxCommandEvent &evt);
private:
	wxString mDescription;
	float mData[12];
	DECLARE_EVENT_TABLE();
};

class AFSearchListBox : public wxPanel
{
public:
	AFSearchListBox( wxWindow *parent, int id, const wxPoint &pos = wxDefaultPosition, const wxSize &size = wxDefaultSize );
	void Append( const wxString &s );
	void Append( const wxArrayString &s );
	int GetSelection();
	size_t Count();
	wxString GetItem( size_t i );
	void Clear();
	wxString GetStringSelection();
	void SetSelection( size_t i );
	bool SetStringSelection(const wxString &s);

	void SetPromptText( const wxString & );

private:
	wxTextCtrl *m_txtFilter;
	wxStaticText *m_label;
	wxStaticText *m_notifyLabel;
	wxListBox *m_list;

	struct item {
		item() : str(wxEmptyString), shown(false) {  }
		item( const wxString &s, bool sh ) : str(s), shown(sh) {  }
		wxString str;
		bool shown;
	};
	std::vector<item> m_items;

	void UpdateView();
	void SendEvent();
	void OnFilter( wxCommandEvent & );
	void OnSelect( wxCommandEvent & );

	DECLARE_EVENT_TABLE();
};


#define EVT_DATAARRAYBUTTON(id, func)  EVT_BUTTON(id, func)

enum {
	DATA_ARRAY_8760_ONLY,
	DATA_ARRAY_8760_MULTIPLES,
	DATA_ARRAY_ANY };

class AFDataArrayButton : public wxButton
{
public:
	AFDataArrayButton(wxWindow *parent, int id, const wxPoint &pos=wxDefaultPosition, const wxSize &size=wxDefaultSize);

	void Set(const std::vector<float> &data);
	void Get(std::vector<float> &data);
	std::vector<float> Get() const { return mData; }

	void SetDataLabel(const wxString &s);
	wxString GetDataLabel();

	void SetDescription( const wxString &s ) { m_description = s; }
	wxString GetDescription() { return m_description; }

	void SetMode(int mode);
	int GetMode();

	void OnPressed(wxCommandEvent &evt);
private:
	wxString mDataLabel;
	int mMode;
	std::vector<float> mData;
	wxString m_description;

	DECLARE_EVENT_TABLE();
};

class wxExtGridCtrl;
 
/*
BEGIN_DECLARE_EVENT_TYPES()
DECLARE_EVENT_TYPE( wxEVT_AFDataMatrixCtrl_CHANGE, 0 )
END_DECLARE_EVENT_TYPES()

#define EVT_DATAMATRIX(id, func)  EVT_COMMAND(id, wxEVT_AFDataMatrixCtrl_CHANGE, func)

class AFDataMatrixCtrl : public wxPanel
{
public:
	AFDataMatrixCtrl( wxWindow *parent, int id, 
		const wxPoint &pos=wxDefaultPosition,
		const wxSize &sz=wxDefaultSize, 
		bool sidebuttons = false);

	void SetData( const matrix_t<float> &mat);
	void GetData( matrix_t<float> &mat );
	matrix_t<float> GetData() const { return m_data; }

	void SetValueLimits( float min=0.0, float max=0.0 );
	void GetValueLimits( float *min, float *max );

	bool Export(const wxString &file);
	bool Import(const wxString &file);

	// '#' = y2*(i/n) + y1*i + y0
	void SetRowLabelFormat( const wxString &val_fmt, double y2, double y1, double y0 );
	void SetColLabelFormat( const wxString &val_fmt, double y2, double y1, double y0 );

	void SetCaption(const wxString &cap);
	wxString GetCaption();

	void ShowRows(bool b);
	bool ShowRows();

	void ShowRowLabels(bool b);
	bool ShowRowLabels();

	void SetRowLabels(const wxString &rowLabels);
	wxString GetRowLabels();

	void ShadeR0C0(bool b);
	bool ShadeR0C0();

	void ShadeC0(bool b);
	bool ShadeC0();

	void ShowCols(bool b);
	bool ShowCols();

	void ShowColLabels(bool b);
	bool ShowColLabels();

	void SetColLabels(const wxString &colLabels);
	wxString GetColLabels();

	void SetNumRowsLabel(const wxString &numRowsLabel);
	wxString GetNumRowsLabel();

	void SetNumColsLabel(const wxString &numColsLabel);
	wxString GetNumColsLabel();

	void PasteAppendRows(bool b);
	bool PasteAppendRows();

private:

	wxString m_rowFormat;
	double m_rowY2, m_rowY1, m_rowY0;
	wxString m_colFormat;
	double m_colY2, m_colY1, m_colY0;

	matrix_t<float> m_data;
	float m_minVal, m_maxVal;
	wxNumericCtrl *m_numRows, *m_numCols;
	wxExtGridCtrl *m_grid;
	wxStaticText *m_caption, *m_labelCols, *m_labelRows;
	wxButton *m_btnImport, *m_btnExport, *m_btnCopy, *m_btnPaste;
	bool m_showrows;
	bool m_showRowLabels;
	wxString m_rowLabels;
	bool m_shadeR0C0;
	bool m_shadeC0;
	bool m_showcols;
	bool m_showColLabels;
	wxString m_colLabels;
	wxString m_numRowsLabel;
	wxString m_numColsLabel;
	bool m_pasteappendrows;

	void NormalizeToLimits();

	void OnCellChange(wxGridEvent &evt);
	void OnRowsColsChange(wxCommandEvent &evt);
	void OnCommand(wxCommandEvent &evt);


	void MatrixToGrid();

	DECLARE_EVENT_TABLE();
};

*/


/* Extended Data Matrix Control */

BEGIN_DECLARE_EVENT_TYPES()
DECLARE_EVENT_TYPE(wxEVT_AFDataMatrixCtrl_CHANGE, 0)
END_DECLARE_EVENT_TYPES()

#define EVT_DATAMATRIX(id, func)  EVT_COMMAND(id, wxEVT_AFDataMatrixCtrl_CHANGE, func)

class AFDataMatrixCtrl : public wxPanel
{
public:
	AFDataMatrixCtrl(wxWindow *parent, int id,
		const wxPoint &pos = wxDefaultPosition,
		const wxSize &sz = wxDefaultSize,
		bool sidebuttons = false,
		const wxString &collabels = wxEmptyString,
		const wxString &rowlabels = wxEmptyString,
		const wxString &choices = wxEmptyString,
		const int &choice_col = -1);

	void SetData(const matrix_t<float> &mat);
	void GetData(matrix_t<float> &mat);
//	matrix_t<float> GetData() const;
	matrix_t<float> GetData() const { return m_data; }

	void SetValueLimits(float min = 0.0, float max = 0.0);
	void GetValueLimits(float *min, float *max);

	bool Export(const wxString &file);
	bool Import(const wxString &file);

	// '#' = y2*(i/n) + y1*i + y0
	void SetRowLabelFormat(const wxString &val_fmt, double y2, double y1, double y0);
	void SetColLabelFormat(const wxString &val_fmt, double y2, double y1, double y0);

	void SetCaption(const wxString &cap);
	wxString GetCaption();

	void ShowRows(bool b);
	bool ShowRows();

	void ShowRowLabels(bool b);
	bool ShowRowLabels();

	void SetRowLabels(const wxString &rowLabels);
	wxString GetRowLabels();


	void ShadeR0C0(bool b);
	bool ShadeR0C0();

	void ShadeC0(bool b);
	bool ShadeC0();

	void ShowCols(bool b);
	bool ShowCols();

	void ShowColLabels(bool b);
	bool ShowColLabels();

	void SetColLabels(const wxString &colLabels);
	wxString GetColLabels();

	void SetNumRowsLabel(const wxString &numRowsLabel);
	wxString GetNumRowsLabel();

	void SetNumColsLabel(const wxString &numColsLabel);
	wxString GetNumColsLabel();

	void PasteAppendRows(bool b);
	bool PasteAppendRows();

	void SetChoices(const wxString &choices);
	wxString GetChoices();

	void SetChoiceColumn(const int &choiceColumn);
	int GetChoiceColumn();

private:

	wxString m_choices;
	int m_choiceColumn;
	AFDataMatrixTable *m_gridTable;

	wxString m_rowFormat;
	double m_rowY2, m_rowY1, m_rowY0;
	wxString m_colFormat;
	double m_colY2, m_colY1, m_colY0;

	matrix_t<float> m_data;
	float m_minVal, m_maxVal;
	wxNumericCtrl *m_numRows, *m_numCols;
	wxExtGridCtrl *m_grid;
	wxStaticText *m_caption, *m_labelCols, *m_labelRows;
	wxButton *m_btnImport, *m_btnExport, *m_btnCopy, *m_btnPaste;
	bool m_showrows;
	bool m_showRowLabels;
	wxString m_rowLabels;
	bool m_shadeR0C0;
	bool m_shadeC0;
	bool m_showcols;
	bool m_showColLabels;
	wxString m_colLabels;
	wxString m_numRowsLabel;
	wxString m_numColsLabel;
	bool m_pasteappendrows;

	void NormalizeToLimits();

	void OnCellChange(wxGridEvent &evt);
	void OnRowsColsChange(wxCommandEvent &evt);
	void OnCommand(wxCommandEvent &evt);


	void MatrixToGrid();

	DECLARE_EVENT_TABLE();
};







BEGIN_DECLARE_EVENT_TYPES()
DECLARE_EVENT_TYPE( wxEVT_VALUEMATRIXBUTTON_CHANGE, 0 )
END_DECLARE_EVENT_TYPES()

#define EVT_VALUEMATRIXBUTTON(id, func)  EVT_COMMAND(id, wxEVT_VALUEMATRIXBUTTON_CHANGE, func)

class AFValueMatrixButton : public wxWindow
{
public:
	AFValueMatrixButton(wxWindow *parent, int id, const wxPoint &pos = wxDefaultPosition, const wxSize &sz = wxDefaultSize);

	bool UseTable();
	void UseTable(bool b);

	void Set( const matrix_t<float> &mat );
	matrix_t<float> Get();

	float GetSingleValue();
	void SetSingleValue(float val);

	void GetTableData(matrix_t<float> *mat);
	void SetTableData(const matrix_t<float> &mat);

	void SetTableSize(int nr, int nc);
	void GetTableSize(int *nr, int *nc);
	void SetColLabels(const wxString &delimlist);
	void SetColLabels(const wxArrayString &labels);
	
	void DispatchEvent();
private:
	void OnResize(wxSizeEvent &evt);
	void OnPaint(wxPaintEvent &evt);
	void OnClick(wxMouseEvent &evt);
	void OnEditTable(wxCommandEvent &evt);
	void OnValChanged(wxCommandEvent &evt);

	bool bUseTable;

	wxButton *mBtnEditTable;
	wxNumericCtrl *mSingleValue;

	matrix_t<float> mTable;
	wxArrayString mColLabels;

	int m_switchWidth;
	void UpdateSwitchWidth();

	DECLARE_EVENT_TABLE();
};



BEGIN_DECLARE_EVENT_TYPES()
DECLARE_EVENT_TYPE( wxEVT_AFMonthByHourFactorCtrl_CHANGE, 0)
END_DECLARE_EVENT_TYPES()


#define EVT_MONTHBYHOURFACTOR(id, func) EVT_COMMAND(id, wxEVT_AFMonthByHourFactorCtrl_CHANGE, func)

class AFMonthByHourFactorCtrl : public wxPanel
{
public:
	AFMonthByHourFactorCtrl(wxWindow *parent, int id, const wxPoint &pos = wxDefaultPosition, const wxSize &sz = wxDefaultSize);
	virtual ~AFMonthByHourFactorCtrl();

	void SetData(const matrix_t<float> &data);
	void GetData( matrix_t<float> &mat );
	matrix_t<float> GetData();
	
	void SetTitle( const wxString &title);
	wxString GetTitle( );
	void SetLegend( const wxString &legend);
	wxString GetLegend( );

	wxColour Colour1;
	wxColour Colour2;

private:
	void UpdateCell(int r, int c);
	void UpdateGrid();

	void OnGridCellChange(wxGridEvent &evt);
	void OnGridCellSelect(wxGridEvent &evt);
	void OnGridEditorHidden(wxGridEvent &evt);
	void OnGridEditorShown(wxGridEvent &evt);
	void OnGridRangeSelect(wxGridRangeSelectEvent &evt);

	void OnCopyClipboard(wxCommandEvent &evt);
	void OnImport(wxCommandEvent &evt);
	void OnExport(wxCommandEvent &evt);
	void OnApply(wxCommandEvent &evt);

	void ApplyVal(int r, int c, double sf);
	void DispatchEvent();

	matrix_t<float> mData;
	wxNumericCtrl *mShadingVal;
	wxButton *mBtnApply;
	wxExtGridCtrl *mGrid;
	int mSelTopRow, mSelBottomRow;
	int mSelLeftCol, mSelRightCol;
	bool bSkipSelect;

	wxStaticText *m_title;
	wxStaticText *m_legend;

	DECLARE_EVENT_TABLE();
};

#endif

