#ifndef __p50p90_h
#define __p50p90_h

#include <wx/panel.h>

class wxTextCtrl;
class wxSnapLayout;
class Case;
class wxExtGridCtrl;

class P50P90Form : public wxPanel
{
public:
	P50P90Form( wxWindow *parent, Case *cc );


protected:

	void OnSimulate( wxCommandEvent & );
	void OnSelectFolder( wxCommandEvent & );

private:
	Case *m_case;
	wxTextCtrl *m_folder;
	wxTextCtrl *m_output;
	wxExtGridCtrl *m_grid;
	wxSnapLayout *m_layout;
	std::vector<wxWindow*> m_graphs;

	DECLARE_EVENT_TABLE();
};


#endif
