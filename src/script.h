#ifndef __script_h
#define __script_h

#include <vector>
#include <wx/frame.h>
#include <wx/stc/stc.h>

#include <wex/lkscript.h>

class SamScriptCtrl;
class wxTextCtrl;
class wxMetroButton;

class SamScriptWindowFactory : public wxLKScriptWindowFactory
{
public:
	SamScriptWindowFactory();
	virtual ~SamScriptWindowFactory();
	virtual wxLKScriptWindow *Create();
};


class SamScriptWindow  : public wxLKScriptWindow
{
public:
	SamScriptWindow( wxWindow *parent, int id = wxID_ANY );

	static void CancelRunningSimulations();

protected:
	virtual void OnScriptStarted();
	virtual void OnScriptStopped();
	virtual void OnHelp();

private:
	void OnVariables( wxCommandEvent & );
	DECLARE_EVENT_TABLE();
};


// LK fcall functions that get/set input variables
// and operate on the currently active case
// Although primarily used from the script window,
// these also give access to case variables when 
// using the integrated 3D shading scripting
void fcall_set( lk::invoke_t &cxt );
void fcall_get( lk::invoke_t &cxt );


#endif

