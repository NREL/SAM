#ifndef __wx_exttextctrl_h
#define __wx_exttextctrl_h

#include <wx/textctrl.h>

class wxExtTextCtrl : public wxTextCtrl
{
public:
	wxExtTextCtrl( wxWindow *parent, int id = wxID_ANY, 
		const wxString &text = wxEmptyString,
		const wxPoint &pos = wxDefaultPosition,
		const wxSize &size = wxDefaultSize,
		long style = 0 );

private:
	void OnTextEnter( wxCommandEvent & );
	void OnSetFocus( wxFocusEvent & );
	void OnLoseFocus( wxFocusEvent & );

	wxString m_focusStrVal;
	DECLARE_EVENT_TABLE()
};

#endif
