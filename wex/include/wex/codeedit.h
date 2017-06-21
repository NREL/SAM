#ifndef __codedit_h
#define __codedit_h

#include <vector>



#include <unordered_map>
using std::unordered_map;
#pragma warning(disable: 4290)  // ignore warning: 'C++ exception specification ignored except to indicate a function is not __declspec(nothrow)'

#include <wx/vector.h>
#include <wx/fdrepdlg.h>
#include <wx/stc/stc.h>

class FRDialog;

class wxCodeEditCtrl : public wxStyledTextCtrl
{
public:
	wxCodeEditCtrl( wxWindow *parent, int id = wxID_ANY, 
		const wxPoint &pos = wxDefaultPosition, const wxSize &size = wxDefaultSize );
	
	virtual bool SetFont(const wxFont& font);

	bool ReadAscii( const wxString &file );
	bool WriteAscii( const wxString &file );
	enum Language { NONE, CPP, C, LK, VBA, HTML, TEXT, TRNSYS, PYTHON };

	void SetLanguage( const wxString &fileName );
	void SetLanguage( Language lang );
	Language GetLanguage();
	static Language GetLanguage( const wxString &fileName );
	void SetKnownIdentifiers( const wxString &names ); // space separated list of identifiers to highlight

	void EnableCallTips( bool en );
	void ClearCallTips();
	void ConfigureCallTips( wxUniChar start, wxUniChar end, bool case_sensitive );
	void AddCallTip( const wxString &key, const wxString &value );
	
	void ShowBreakpoints( bool show );
	void AddBreakpoint( int line );
	void RemoveBreakpoint( int line );
	void ToggleBreakpoint( int line );
	bool HasBreakpoint( int line );
	void ClearBreakpoints();
	std::vector<int> GetBreakpoints();
	int GetNextBreakpointAfter( int line );

	void ShowLineArrow( int line );
	void HideLineArrow();

	void ShowFindReplaceDialog();
	void FindNext();
	void ReplaceNext();
	void ReplaceAll();

	// note: this function must be called in advance of
	// 'ShowFindReplaceDialog()' to have the button show up
	void ShowFindInFilesButton( bool b ) { m_showFindInFilesButton = b; }
	virtual bool OnFindInFiles( const wxString &text, bool match_case, bool whole_word );
	
	bool IsTextSelected( const wxString &text, bool match_case );

	int	FindNext( const wxString &text, int fr_text_len = -1, /* used internally by replace - typically -1 is OK */ 
			bool match_case = true, bool whole_word = false, 
			bool wrap_around = true, int start_pos = -1 );
	int ReplaceNext( const wxString &text, const wxString &replace, bool stop_at_find = false, 
			bool match_case = true, bool whole_word = false, bool wrap_around = true );	
	int ReplaceAll( const wxString &text, const wxString &replace, 
			bool match_case = true, bool whole_word = false, bool show_message = true );
	
	void SelectLine( int line );
	void YankLine();
	void PutLine();

protected:
	
	static const int m_markCircle = 0;
	static const int m_markArrow = 1;
	static const int m_markLeftBox = 2;

	static const int m_lineNumMarginId = 0;
	static const int m_breakpointMarginId = 1;
	static const int m_syntaxCheckMarginId = 2;
	static const int m_foldingMarginId = 3;

private:	
	void OnMarginClick( wxStyledTextEvent &evt );
    void OnCharAdded( wxStyledTextEvent &evt );
	void OnUpdateUI( wxStyledTextEvent &evt );

	void DoBraceMatch();
	bool FindMatchingBracePosition( int &braceAtCaret, int &braceOpposite, bool sloppy );
	
	int m_lastFindPos;
	int m_lastReplacePos;
	

	bool m_showFindInFilesButton;
	FRDialog *m_frDialog;
	Language m_lang;
	bool m_callTipsEnabled;
	wxUniChar m_ctStart, m_ctEnd;
	bool m_ctCaseSensitive;
	wxArrayString m_ctStack;
	
	//std::vector<int> m_breakPoints;
	unordered_map< wxString, wxString, wxStringHash, wxStringEqual > m_callTips;

	wxString m_yankText;

    DECLARE_EVENT_TABLE()
};

#endif
