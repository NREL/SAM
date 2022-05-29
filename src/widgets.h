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

#ifndef __uiwidgets_h
#define __uiwidgets_h

#include <vector>

#include <wx/window.h>
#include <wx/panel.h>
#include <wx/button.h>
#include <wx/grid.h>
#include <wx/stattext.h>

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
	void GetSchedule( std::vector<double> *vals );
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

typedef unordered_map<wxString, double, wxStringHash, wxStringEqual> KeyValueMap;

#define EVT_TABLEDATA(i,f) EVT_BUTTON(i,f)
class AFTableDataCtrl : public wxButton
{
public:
	AFTableDataCtrl( wxWindow *parent, int id,
		const wxPoint &pos=wxDefaultPosition, const wxSize &size=wxDefaultSize);

	void SetFields( const wxArrayString &list );
	wxArrayString GetFields();
	
	void SetExpandable( bool b );
	bool GetExpandable();
	
	void Clear();
	void Set( const wxString &var, double value );
	double Get( const wxString &var );
	
	void SetDescription(const wxString &s);
	wxString GetDescription();

	void OnPressed(wxCommandEvent &evt);

private:
	bool m_expandable;
	wxString m_description;
	KeyValueMap m_values;

	DECLARE_EVENT_TABLE();
};

#define EVT_MONTHLYFACTOR(i,f) EVT_BUTTON(i,f)
class AFMonthlyFactorCtrl : public wxButton
{
public:
	AFMonthlyFactorCtrl( wxWindow *parent, int id, 
		const wxPoint &pos=wxDefaultPosition, const wxSize &size=wxDefaultSize);

	std::vector<double> Get( );
	void Set( const std::vector<double> &data );
	void SetDescription(const wxString &s);
	wxString GetDescription();
	void OnPressed(wxCommandEvent &evt);
private:
	wxString mDescription;
	double mData[12];
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



#define EVT_DATALIFETIMEARRAYBUTTON(id, func)  EVT_BUTTON(id, func)

enum {
	DATA_LIFETIME_ARRAY_SUBHOURLY,
	DATA_LIFETIME_ARRAY_HOURLY,
	DATA_LIFETIME_ARRAY_DAILY,
	DATA_LIFETIME_ARRAY_WEEKLY,
	DATA_LIFETIME_ARRAY_MONTHLY,
	DATA_LIFETIME_ARRAY_ANNUAL,
	DATA_LIFETIME_ARRAY_SINGLEVALUE
};

class AFDataLifetimeArrayButton : public wxButton
{
public:
	AFDataLifetimeArrayButton(wxWindow *parent, int id, const wxPoint &pos = wxDefaultPosition, const wxSize &size = wxDefaultSize);

	void Set(const std::vector<double> &data);
	void Get(std::vector<double> &data);
	std::vector<double> Get() const { return mData; }

	void SetDataLabel(const wxString &s);
	wxString GetDataLabel();

	void SetColumnLabel(const wxString &s);
	wxString GetColumnLabel();

	void SetShowMode(const bool &b) { mShowMode = b; };
	bool GetShowMode() { return mShowMode; };

	void SetDescription(const wxString &s) { mDescription = s; }
	wxString GetDescription() { return mDescription; }

	void SetAnalysisPeriod(const size_t &p);
	size_t GetAnalysisPeriod() { return mAnalysisPeriod; }

	void SetMinPerHour(const size_t &p) { mMinPerHour = p; }
	size_t GetmMinPerHour() { return mMinPerHour; }

	void SetMode(const size_t &p) { mMode = p; }
	size_t GetMode() { return mMode; }

	void SetAnnualEnabled(const bool &e) { mAnnualEnabled = e; }
	bool GetAnnualEnabled() { return mAnnualEnabled; }

	void SetWeeklyEnabled(const bool &e) { mWeeklyEnabled = e; }
	bool GetWeeklyEnabled() { return mWeeklyEnabled; }

	void OnPressed(wxCommandEvent &evt);
private:
	wxString mDataLabel, mColumnLabel;
	size_t mAnalysisPeriod, mMinPerHour, mMode;
	std::vector<double> mData;
	wxString mDescription;
	bool mAnnualEnabled, mWeeklyEnabled, mShowMode;

	DECLARE_EVENT_TABLE();
};


#define EVT_DATALIFETIMEMATRIXBUTTON(id, func)  EVT_BUTTON(id, func)

enum {
	DATA_LIFETIME_MATRIX_SUBHOURLY,
	DATA_LIFETIME_MATRIX_HOURLY,
	DATA_LIFETIME_MATRIX_DAILY,
	DATA_LIFETIME_MATRIX_WEEKLY,
	DATA_LIFETIME_MATRIX_MONTHLY,
	DATA_LIFETIME_MATRIX_ANNUAL,
	DATA_LIFETIME_MATRIX_SINGLEVALUE
};

class AFDataLifetimeMatrixButton : public wxButton
{
public:
	AFDataLifetimeMatrixButton(wxWindow *parent, int id, const wxPoint &pos = wxDefaultPosition, const wxSize &size = wxDefaultSize);

	void Set(const matrix_t<double> &data, size_t analysis_period = 0);
	void Get(matrix_t<double> &data);
	void Set(const std::vector<double> &data);
	void Get(std::vector<double> &data);
	matrix_t<double> Get() const { return mData; }

	void SetDataLabel(const wxString &s);
	wxString GetDataLabel();

	void SetColumnLabels(const wxString &s);
	wxString GetColumnLabels();

	void SetShowMode(const bool &b) { mShowMode = b; };
	bool GetShowMode() { return mShowMode; };

	void SetDescription(const wxString &s) { mDescription = s; }
	wxString GetDescription() { return mDescription; }

	void SetAnalysisPeriod(const size_t &p);
	size_t GetAnalysisPeriod() { return mAnalysisPeriod; }

	void SetMinPerHour(const size_t &p) { mMinPerHour = p; }
	size_t GetmMinPerHour() { return mMinPerHour; }

	void SetMode(const size_t &p) { mMode = p; }
	size_t GetMode() { return mMode; }

	void SetAnnualEnabled(const bool &e) { mAnnualEnabled = e; }
	bool GetAnnualEnabled() { return mAnnualEnabled; }

	void SetWeeklyEnabled(const bool &e) { mWeeklyEnabled = e; }
	bool GetWeeklyEnabled() { return mWeeklyEnabled; }

	void OnPressed(wxCommandEvent &evt);
private:
	wxString mDataLabel;
	wxString mColumnLabels;
	size_t mAnalysisPeriod, mMinPerHour, mMode;
	matrix_t<double> mData;
	wxString mDescription;
	bool mAnnualEnabled, mWeeklyEnabled, mShowMode;

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

	void Set(const std::vector<double> &data);
	void Get(std::vector<double> &data);
	std::vector<double> Get() const { return mData; }

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
	std::vector<double> mData;
	wxString m_description;

	DECLARE_EVENT_TABLE();
};


#define EVT_DATASTRINGBUTTON(id, func)  EVT_BUTTON(id, func)

class AFStringArrayButton : public wxButton
{
public:
	AFStringArrayButton(wxWindow *parent, int id, const wxPoint &pos = wxDefaultPosition, const wxSize &size = wxDefaultSize);

	void Set(const wxString &data);
	void Get(wxString &data);
	wxString Get() const { return wxJoin(mData,'|'); }

	void SetStringLabel(const wxString &s);
	wxString GetStringLabel();

	void SetDescription(const wxString &s) { m_description = s; }
	wxString GetDescription() { return m_description; }


	void OnPressed(wxCommandEvent &evt);
private:
	wxString mStringLabel;
	wxArrayString mData;
	wxString m_description;

	DECLARE_EVENT_TABLE();
};





class wxVerticalLabel : public wxPanel
{
public:
	wxVerticalLabel(wxWindow* parent,
		wxWindowID id,
		const wxString& label,
		const wxPoint& pos = wxDefaultPosition,
		const wxSize& size = wxDefaultSize,
		long style = 0,
		const wxString& name = wxStaticTextNameStr);
	virtual ~wxVerticalLabel();
	void SetLabel(const wxString &label);
	wxString const &GetLabel() { return m_Label; }

protected:
	void OnPaint(wxPaintEvent& );

private:
	void UpdateSize();
	wxString m_Label;
};


class wxHorizontalLabel : public wxPanel
{
public:
	wxHorizontalLabel(wxWindow* parent,
		wxWindowID id,
		const wxString& label,
		const wxPoint& pos = wxDefaultPosition,
		const wxSize& size = wxDefaultSize,
		long style = 0,
		const wxString& name = wxStaticTextNameStr);
	virtual ~wxHorizontalLabel();
	void SetLabel(const wxString &label);
	wxString const &GetLabel() { return m_Label; }

protected:
	void OnPaint(wxPaintEvent& );

private:
	void UpdateSize();
	wxString m_Label;
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
		const int &choice_col = -1,
		bool bottombuttons = false,
		const wxString &horizontalLabel = wxEmptyString,
		const wxString &vericalLabel = wxEmptyString);

	void SetData(const matrix_t<double> &mat);
	void GetData(matrix_t<double> &mat);
//	matrix_t<double> GetData() const;
	matrix_t<double> GetData() const { return m_data; }

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

	void ShowButtons(bool b);
	bool ShowButtons();

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

	void SetR0C0Label(const wxString &R0C0Label);
	wxString GetR0C0Label();

	void PasteAppendRows(bool b);
	bool PasteAppendRows();

	void PasteAppendCols(bool b);
	bool PasteAppendCols();

	void SetChoices(const wxString &choices);
	wxString GetChoices();

	void SetChoiceColumn(const int &choiceColumn);
	int GetChoiceColumn();

	void SetColReadOnly(const int &col, bool readonly);
	void SetRowReadOnly(const int &row, bool readonly);

	void ShowCol(const int &col, bool show);
	void ShowRow(const int &row, bool show);

	void ColorMap(bool b);
	bool ColorMap();

	void UpdateColorMap();

private:

	wxString m_choices;
	int m_choiceColumn;
	AFDataMatrixTable *m_gridTable;

	wxString m_rowFormat;
	double m_rowY2, m_rowY1, m_rowY0;
	wxString m_colFormat;
	double m_colY2, m_colY1, m_colY0;

	matrix_t<double> m_data;
	float m_minVal, m_maxVal;
	wxNumericCtrl *m_numRows, *m_numCols;
	wxExtGridCtrl *m_grid;
	wxStaticText *m_caption, *m_labelCols, *m_labelRows;
	wxHorizontalLabel *m_horizontalLabel;
	wxVerticalLabel *m_verticalLabel;
	wxButton *m_btnImport, *m_btnExport, *m_btnCopy, *m_btnPaste;
	bool m_showButtons;
	bool m_showrows;
	bool m_showRowLabels;
	wxString m_rowLabels;
	bool m_colorMap;
	bool m_shadeR0C0;
	bool m_shadeC0;
	bool m_showcols;
	bool m_showColLabels;
	wxString m_colLabels;
	wxString m_numRowsLabel;
	wxString m_numColsLabel;
	bool m_pasteappendrows;
	bool m_pasteappendcols;

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

	void Set( const matrix_t<double> &mat );
	matrix_t<double> Get();

	float GetSingleValue();
	void SetSingleValue(double val);

	void GetTableData(matrix_t<double> *mat);
	void SetTableData(const matrix_t<double> &mat);

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

	matrix_t<double> mTable;
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

	void SetData(const matrix_t<double> &data);
	void GetData( matrix_t<double> &mat );
	matrix_t<double> GetData();
	
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

	void OnCopy(wxCommandEvent &evt);
	void OnPaste(wxCommandEvent &evt);
	void OnImport(wxCommandEvent &evt);
	void OnExport(wxCommandEvent &evt);
	void OnApply(wxCommandEvent &evt);

	void ApplyVal(int r, int c, double sf);
	void DispatchEvent();

	matrix_t<double> mData;
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

BEGIN_DECLARE_EVENT_TYPES()
	DECLARE_EVENT_TYPE(wxEVT_TOOLTIPCTRL_CHANGE, 0)
END_DECLARE_EVENT_TYPES()
#define EVT_TOOLTIPCTRL(id,func) EVT_COMMAND(id, wxEVT_TOOLTIPCTRL_CHANGE, func)


class AFToolTipCtrl : public wxPanel
{
	wxBitmap m_image;
public:
	AFToolTipCtrl(wxWindow* parent);

	void paintEvent(wxPaintEvent & evt);
	void paintNow();

	void render(wxDC& dc);

	void mouseDown(wxMouseEvent& event);

	DECLARE_EVENT_TABLE()

	
};


#endif

