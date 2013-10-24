#ifndef __basecase_h
#define __basecase_h

#include <wx/splitter.h>
#include "object.h"

class CaseWindow;
class Case;
class wxExtGridCtrl;
class wxDVPlotCtrl;
class wxPLPlotCtrl;
class wxTextCtrl;
class wxListBox;
class wxSimplebook;

class MetricsTable;


class BaseCase : public wxSplitterWindow
{
public:
	BaseCase( wxWindow *parent, CaseWindow *cw );
	virtual ~BaseCase();


private:
	void OnCommand( wxCommandEvent & );

	std::vector<wxPLPlotCtrl*> m_plot;
	MetricsTable *m_metrics;
	wxExtGridCtrl *m_cashFlowGrid;
	wxDVPlotCtrl *m_dview;
	wxMetroListBox *m_pageList;
	wxSimplebook *m_pageFlipper;
	

	DECLARE_EVENT_TABLE();
};

class MetricsTable : public wxWindow
{
public:
	MetricsTable(wxWindow *parent);
	void SetData(const matrix_t<wxString> &data);	
	virtual wxSize DoGetBestSize() const;
private:	
	wxColour mFillColour;
	wxColour mTableColour;
	int mBestHeight;
	int mRowHeight;
	matrix_t<wxString> mTable;

	void OnPaint(wxPaintEvent &evt);
	void OnResize(wxSizeEvent &evt);
	void OnRightClick(wxMouseEvent &evt);
	void OnContextMenu(wxCommandEvent &evt);
	DECLARE_EVENT_TABLE();
};

#endif
