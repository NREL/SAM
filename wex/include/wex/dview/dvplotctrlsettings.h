#ifndef __DVPlotCtrlSettings_h
#define __DVPlotCtrlSettings_h

/*
 * wxDVPlotCtrlSettings.h
 * 
 * This class stores the configuration of wxDVPlotCtrl.
 * We keep track of things like current tab, axis position, etc so that they can be restored quickly.
 */

#include <wx/wx.h>

#include <unordered_map>
using std::unordered_map;
#pragma warning(disable: 4290)  // ignore warning: 'C++ exception specification ignored except to indicate a function is not __declspec(nothrow)'

class wxDVPlotCtrlSettings
{
public:
	wxDVPlotCtrlSettings();
	wxDVPlotCtrlSettings( const wxDVPlotCtrlSettings &cpy ) { m_properties = cpy.m_properties; }
	virtual ~wxDVPlotCtrlSettings();

	wxDVPlotCtrlSettings &operator=(const wxDVPlotCtrlSettings &rhs)
	{
		m_properties = rhs.m_properties;
		return *this;
	}

	void SetProperty(const wxString &prop, const wxString &value);
	void SetProperty(const wxString &prop, int value);
	void SetProperty(const wxString &prop, double value);
	void SetProperty(const wxString &prop, bool Value);
	wxString GetProperty(const wxString &prop);
		
private:
	unordered_map<wxString, wxString, wxStringHash, wxStringEqual> m_properties;
};

#endif

