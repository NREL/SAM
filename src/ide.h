#ifndef __samide_h
#define __samide_h

#include <wx/frame.h>

class wxMetroNotebook;
class wxLKScriptCtrl;

class IDEWindow : public wxFrame
{
public:
	IDEWindow( wxWindow *parent );
	virtual ~IDEWindow();

	wxLKScriptCtrl *m_scriptCtrl;
	wxMetroNotebook *m_notebook;
};

#endif
