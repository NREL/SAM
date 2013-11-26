#ifndef __uiwidgets_h
#define __uiwidgets_h

#include <vector>

#include <wx/window.h>
#include <wx/button.h>
#include <wex/numeric.h>

#include "object.h"


void RegisterUIWidgetsForSAM();

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
	double GetValue();
	void SetValue(double d);
	void SetFormat( int deci, bool thousep, const wxString &pre, const wxString &post );
	std::vector<double> GetSchedule();
	void GetSchedule( std::vector<float> *vals );
	int GetSchedLen();
	void SetSchedule(const std::vector<double> &s);
	void SetSchedule( const std::vector<float> &s );
	void SetSchedLen(int len);

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

	DECLARE_EVENT_TABLE();
};

#endif

