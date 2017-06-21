#ifndef __tcsscript_h
#define __tcsscript_h

#include <wex/lkscript.h>

class MyScriptCtrl;

class tcScriptEditor : public wxPanel
{
public:
	tcScriptEditor( wxWindow *parent );

	bool Write( const wxString &file );
	bool Load( const wxString &file );
	bool Save();
	bool SaveAs();
	bool IsModified();
	bool CloseDoc();
	wxString GetFileName() { return m_fileName; };

	void Exec();
private:
	void OnAction( wxCommandEvent & );
	void OnHelp( wxCommandEvent & );

	MyScriptCtrl *m_editor;
	wxString m_fileName;
	wxStaticText *m_statusLabel;
	wxButton *m_stopButton;

	DECLARE_EVENT_TABLE()
};

#endif
