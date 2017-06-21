#ifndef __wex_radiochoice_h
#define __wex_radiochoice_h

#include <vector>
#include <wx/panel.h>
#include <wx/radiobut.h>

class wxRadioChoice : public wxPanel
{
public:
	wxRadioChoice( wxWindow *parent, int id, 
		const wxPoint &pos = wxDefaultPosition, 
		const wxSize &size = wxDefaultSize);
	
	virtual wxSize DoGetBestSize() const;

	void Add( const wxString &caption, bool arrange = true);
	void Add( const wxArrayString &list);
	int Find( const wxString &caption);
	void Remove( int idx);
	void Remove( const wxString &caption);
	void Clear();
	int GetCount();

	virtual bool Enable(bool b=true);
	void Enable(int idx, bool b=true);
	bool IsEnabled(int idx);

	void SetLabel( int idx, const wxString &lbl );
	wxString GetLabel( int idx );

	int GetSelection();
	wxString GetValue();
	void SetValue(const wxString &sel);
	void SetSelection(int idx);

	void Rearrange();

	void ShowCaptions(bool b);
	bool CaptionsShown();

	void SetHorizontal(bool b);
	bool IsHorizontal();

	void LayoutEvenly(bool b);

private:
	void OnRadio(wxCommandEvent &evt);
	void OnResize(wxSizeEvent &evt);

	bool m_showCaptions;
	bool m_horizontal;
	bool m_evenly;
	wxArrayString m_captions;
	std::vector<wxRadioButton*> m_buttons;
	int m_selection;

	DECLARE_EVENT_TABLE()
};

#endif
