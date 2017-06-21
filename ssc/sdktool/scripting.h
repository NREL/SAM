#ifndef __automation_h
#define __automation_h

#include <wx/wx.h>
#include "dllinvoke.h"

#include <lk/absyn.h>
#include <lk/env.h>

class wxLKScriptCtrl;


class EditorWindow : public wxPanel
{
private:
	wxLKScriptCtrl *m_editor;
	wxStaticText *m_statusLabel;
	wxString m_fileName;
	wxButton *m_stopButton;
	wxString m_lastFindStr;
public:
	EditorWindow( wxWindow *parent );
	virtual ~EditorWindow();
	
	wxString GetFileName() { return m_fileName; }
	void OnCommand( wxCommandEvent &evt );
	void Open();
	bool Save();
	bool SaveAs();
	bool CloseDoc();
	bool Write( const wxString &file );
	bool Load( const wxString &file );	
	void Exec();


	wxLKScriptCtrl *GetEditor() { return m_editor; }

	DECLARE_EVENT_TABLE()
};


#endif

