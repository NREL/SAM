
#include "wex/dview/dvplotctrlsettings.h"


wxDVPlotCtrlSettings::wxDVPlotCtrlSettings()
{

}

wxDVPlotCtrlSettings::~wxDVPlotCtrlSettings()
{

}

void wxDVPlotCtrlSettings::SetProperty(const wxString& prop, const wxString &value)
{
	m_properties[prop] = value;
}

void wxDVPlotCtrlSettings::SetProperty(const wxString& prop, int value)
{
	m_properties[prop] = wxString::Format("%d", value);
}

void wxDVPlotCtrlSettings::SetProperty(const wxString& prop, double value)
{
	m_properties[prop] = wxString::Format("%lg", value);
}

void wxDVPlotCtrlSettings::SetProperty(const wxString& prop, bool value)
{
	m_properties[prop] = wxString::Format("%d", value ? 1 : 0);
}

wxString wxDVPlotCtrlSettings::GetProperty(const wxString &prop)
{
	return m_properties[ prop ];
}
