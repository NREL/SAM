<<<<<<< HEAD
#ifndef __inputpagelist_h
#define __inputpagelist_h

#include <wx/bitmap.h>
#include <wx/scrolwin.h>

#include <vector>
class CaseWindow;

class InputPageList : public wxScrolledWindow
{
public:

	InputPageList( wxWindow *parent, int id=-1, 
		const wxPoint &pos=wxDefaultPosition, const wxSize &size=wxDefaultSize );
	virtual ~InputPageList();

	void Add(const wxString &item, bool geom_recalc=true, const wxString &resource="");
	void Add(const wxArrayString &item);
	int Find(const wxString &item);
	wxString GetItem(int idx);
	wxString GetValue();
	void Remove(int idx);
	void ClearItems();
	void Select(int idx);
	int GetSelection();
	wxString GetStringSelection();
	void RedrawItem(int idx);
	void SetCaseWindow(CaseWindow *cw);
	int FindItem(const wxString &name);
	wxArrayString GetItems();
	int Count();

	void Invalidate();
private:
	CaseWindow *m_caseWin;

	struct _item
	{
		wxString name;
		wxRect geom;
		wxString resource;
	};

	std::vector<_item> m_items;
	
	int m_selectedIdx;
	int m_hoverIdx;

	void OnResize(wxSizeEvent &evt);
	void OnLeftDown(wxMouseEvent &evt);
	void OnPaint(wxPaintEvent &evt);
	void OnErase(wxEraseEvent & );
	void OnMouseMove(wxMouseEvent &evt);
	void OnLeave(wxMouseEvent &evt);

	DECLARE_EVENT_TABLE()
};


#endif

=======
#ifndef __inputpagelist_h
#define __inputpagelist_h

#include <wx/bitmap.h>
#include <wx/scrolwin.h>

#include <vector>
class CaseWindow;

class InputPageList : public wxScrolledWindow
{
public:

	InputPageList( wxWindow *parent, int id=-1, 
		const wxPoint &pos=wxDefaultPosition, const wxSize &size=wxDefaultSize );
	virtual ~InputPageList();

	void Add(const wxString &item, bool geom_recalc=true, const wxString &resource="");
	void Add(const wxArrayString &item);
	int Find(const wxString &item);
	wxString GetItem(int idx);
	wxString GetValue();
	void Remove(int idx);
	void ClearItems();
	void Select(int idx);
	int GetSelection();
	wxString GetStringSelection();
	void RedrawItem(int idx);
	void SetCaseWindow(CaseWindow *cw);
	int FindItem(const wxString &name);
	wxArrayString GetItems();
	int Count();

	void Invalidate();
private:
	CaseWindow *m_caseWin;

	struct _item
	{
		wxString name;
		wxRect geom;
		wxString resource;
	};

	std::vector<_item> m_items;
	
	int m_selectedIdx;
	int m_hoverIdx;

	void OnResize(wxSizeEvent &evt);
	void OnLeftDown(wxMouseEvent &evt);
	void OnPaint(wxPaintEvent &evt);
	void OnErase(wxEraseEvent & );
	void OnMouseMove(wxMouseEvent &evt);
	void OnLeave(wxMouseEvent &evt);

	DECLARE_EVENT_TABLE()
};


#endif

>>>>>>> 2c85b0ce6a18646fb532eb72a604d646517b67ae
