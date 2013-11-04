#ifndef __samide_h
#define __samide_h

#include <wx/frame.h>
#include <wex/uiform.h>

class wxMetroNotebook;
class wxLKScriptCtrl;

class IDEWindow : public wxFrame
{
public:
	IDEWindow( wxWindow *parent );
	virtual ~IDEWindow();

private:
	void OnClose( wxCloseEvent & );

	wxLKScriptCtrl *m_scriptCtrl;

	wxUIFormData m_formData;
	wxUIFormDesigner *m_uiFormEditor;
	wxUIPropertyEditor *m_uiPropEditor;

	wxMetroNotebook *m_notebook;

	DECLARE_EVENT_TABLE();
};

#endif
