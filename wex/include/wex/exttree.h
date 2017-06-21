#ifndef __exttree_h
#define __exttree_h

#include <wx/treectrl.h>

// emits ITEM_ACTIVATED for item check change
class wxExtTreeCtrl : public wxTreeCtrl
{
public:
	enum { ICON_CHECK_FALSE,
		ICON_CHECK_TRUE,
		ICON_JUMPTO,
		ICON_ADD,
		ICON_REMOVE,
		ICON_RARROW,
		ICON_JUSTIFY,
		ICON_FOLDER,
		ICON_FILE,
		ICON_BROKEN_LINK };

	wxExtTreeCtrl( wxWindow *parent, int id, const wxPoint &pos = wxDefaultPosition, const wxSize &size = wxDefaultSize, long style = 0);

	void EnableCheckMode(bool b);
	bool IsCheckMode();
	void Check(const wxTreeItemId &item, bool b);
	bool IsChecked(const wxTreeItemId &item);

private:
	bool m_checkMode;
	void OnLClick(wxMouseEvent &evt);

	DECLARE_EVENT_TABLE();
};

#endif
