#include "wex/exttext.h"

#include <wx/wx.h>

BEGIN_EVENT_TABLE( wxExtTextCtrl, wxTextCtrl )
	EVT_TEXT_ENTER( wxID_ANY, wxExtTextCtrl::OnTextEnter )
	EVT_KILL_FOCUS( wxExtTextCtrl::OnLoseFocus )
	EVT_SET_FOCUS( wxExtTextCtrl::OnSetFocus )
END_EVENT_TABLE()

wxExtTextCtrl::wxExtTextCtrl( wxWindow *parent, int id, 
		const wxString &text,
		const wxPoint &pos,
		const wxSize &size,
		long style )
	: wxTextCtrl(parent, id, text, pos, size, 
			style|wxTE_PROCESS_ENTER)
{
	/* nothing to do */
}

void wxExtTextCtrl::OnTextEnter( wxCommandEvent &evt )
{
	if ( m_focusStrVal != GetValue() )
	{
		m_focusStrVal = GetValue();
		evt.Skip();
	}
}

void wxExtTextCtrl::OnSetFocus( wxFocusEvent &evt )
{
	m_focusStrVal = GetValue();
	SetSelection(0,m_focusStrVal.Len());
	evt.Skip();
}

void wxExtTextCtrl::OnLoseFocus( wxFocusEvent &evt )
{
	if ( m_focusStrVal != GetValue() )
	{
		wxCommandEvent enterpress(wxEVT_COMMAND_TEXT_ENTER, this->GetId() );
		enterpress.SetEventObject( this );
		enterpress.SetString( GetValue() );
		GetEventHandler()->ProcessEvent(enterpress);
	}
	evt.Skip();
}
