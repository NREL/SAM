#ifndef __wx_mswfatal_h
#define __wx_mswfatal_h

#ifdef __WXMSW__

#include <wx/string.h>

// notes on using this:
//  - must link application to psapi.lib
//  - include dbghelp.dll in your application distribution
//  - ship stripped PDBs with your application 

// call this in the constructor of your wxApp
// to set up exception handling
void wxMSWSetupExceptionHandler( const wxString &appname, const wxString &version, const wxString &email );

// call this function from your wxApp::OnFatalException() function
// to generate a stack trace and show an exception dialog box
void wxMSWHandleApplicationFatalException();

// create a segmentation fault.  a normal application should never call this,
// but is useful for testing.  Note this should be issued by a wxButton event
// handler, since a menu event behaves as a timer
//     https://social.msdn.microsoft.com/Forums/vstudio/en-US/0caf88f7-e22b-49be-a7e9-8504c0312cb8/exception-no-more-propagated-within-few-win32-function-call?forum=vcgeneral
//     https://forums.wxwidgets.org/viewtopic.php?t=18742&p=81165
void wxMSWSegmentationFault();

	
#endif // __WXMSW__


#endif // __wx_mswfatal_h