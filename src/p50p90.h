<<<<<<< HEAD
#ifndef __p50p90_h
#define __p50p90_h

#include <wx/panel.h>

class wxTextCtrl;
class wxSnapLayout;
class Case;
class wxExtGridCtrl;
class wxPLPlotCtrl;
class wxNumericCtrl;

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
	wxExtGridCtrl *m_grid;
	std::vector<wxWindow*> m_graphs;
	wxNumericCtrl *m_puser;
	wxSnapLayout *m_layout;

	DECLARE_EVENT_TABLE();
};


#endif
=======
#ifndef __p50p90_h
#define __p50p90_h

#include <wx/panel.h>

class wxTextCtrl;
class wxSnapLayout;
class Case;
class wxExtGridCtrl;
class wxPLPlotCtrl;
class wxNumericCtrl;

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
	wxExtGridCtrl *m_grid;
	std::vector<wxWindow*> m_graphs;
	wxNumericCtrl *m_puser;
	wxSnapLayout *m_layout;

	DECLARE_EVENT_TABLE();
};


#endif
>>>>>>> 2c85b0ce6a18646fb532eb72a604d646517b67ae
