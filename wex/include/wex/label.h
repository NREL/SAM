#ifndef __wexlabel_h
#define __wexlabel_h

#include <wx/window.h>



void wxLabelDraw( wxDC &dc, const wxRect &r, const wxFont &font, const wxString &text, const wxColour &col, 
				 bool alignTop, bool alignRight, bool isBold, bool wordWrap, int relSize );

class wxLabel : public wxWindow
{
public:
	wxLabel( wxWindow *parent, int id, const wxString &caption, 
		const wxPoint &pos = wxDefaultPosition, const wxSize &size = wxDefaultSize );
	
	virtual wxSize DoGetBestSize() const;

	void AlignRight( bool b = true );
	void AlignTop( bool b = true );

	void SetText(const wxString &txt);
	wxString GetText() { return m_text; }

	void SetColour(const wxColour &c);
	wxColour GetColour() { return m_colour; }
	
	void SetBold(bool b);
	bool IsBold() { return m_bold; }

	void SetWordWrap(bool b);
	bool IsWordWrapped() { return m_wordWrap; }

	void SetRelativeSize(int sz=0);
	int GetRelativeSize() { return m_relSize; }

private:
	bool m_bold;
	int m_relSize;
	bool m_alignTop;
	bool m_alignRight;
	bool m_wordWrap;
	wxString m_text;
	wxColour m_colour;
	
	void OnErase(wxEraseEvent &evt);
	void OnPaint(wxPaintEvent &evt);
	void OnResize(wxSizeEvent &evt);

	DECLARE_EVENT_TABLE()
	
};


#endif