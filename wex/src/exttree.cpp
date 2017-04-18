/****** wxExtTreeCtrl control *********/

#include <wx/imaglist.h>
#include <wx/bitmap.h>

#include "wex/exttree.h"

#include "wex/icons/checkbox_true_16.cpng"
#include "wex/icons/checkbox_false_16.cpng"
#include "wex/icons/stock_jump_to_16.cpng"
#include "wex/icons/stock_remove_16.cpng"
#include "wex/icons/stock_right_arrow_16.cpng"
#include "wex/icons/stock_directory_16.cpng"
#include "wex/icons/stock_add_16.cpng"
#include "wex/icons/stock_new_16.cpng"
#include "wex/icons/stock_align_justify_16.cpng"
#include "wex/icons/stock_broken_image_16.cpng"

BEGIN_EVENT_TABLE(wxExtTreeCtrl, wxTreeCtrl)
	EVT_LEFT_DOWN(wxExtTreeCtrl::OnLClick)
END_EVENT_TABLE()

wxExtTreeCtrl::wxExtTreeCtrl( wxWindow *parent, int id, const wxPoint &pos, const wxSize &size, long style)
	: wxTreeCtrl(parent, id, pos, size, wxTR_HAS_BUTTONS|wxTR_NO_LINES|wxTR_SINGLE|style)
{
	m_checkMode = true;
	wxImageList *images = new wxImageList( 16, 16 );
	images->Add( wxBITMAP_PNG_FROM_DATA(checkbox_false_16) );
	images->Add( wxBITMAP_PNG_FROM_DATA(checkbox_true_16) );
	images->Add( wxBITMAP_PNG_FROM_DATA(stock_jump_to_16) );
	images->Add( wxBITMAP_PNG_FROM_DATA(stock_add_16) );
	images->Add( wxBITMAP_PNG_FROM_DATA(stock_remove_16) );
	images->Add( wxBITMAP_PNG_FROM_DATA(stock_right_arrow_16) );
	images->Add( wxBITMAP_PNG_FROM_DATA(stock_align_justify_16) );
	images->Add( wxBITMAP_PNG_FROM_DATA(stock_directory_16) );
	images->Add( wxBITMAP_PNG_FROM_DATA(stock_new_16) );
	images->Add( wxBITMAP_PNG_FROM_DATA(stock_broken_image_16) );
	AssignImageList( images );

}

void wxExtTreeCtrl::Check(const wxTreeItemId &item, bool b)
{
	SetItemImage(item, b ? ICON_CHECK_TRUE : ICON_CHECK_FALSE);
}

bool wxExtTreeCtrl::IsChecked(const wxTreeItemId &item)
{
	return (GetItemImage(item)==ICON_CHECK_TRUE);
}
void wxExtTreeCtrl::EnableCheckMode(bool b)
{
	m_checkMode = b;
}

bool wxExtTreeCtrl::IsCheckMode()
{
	return m_checkMode;
}

void wxExtTreeCtrl::OnLClick(wxMouseEvent &evt)
{
	int flags=0;
	wxTreeItemId item = HitTest(evt.GetPosition(), flags);
	if (!item.IsOk()||!m_checkMode)
	{
		evt.Skip();
		return;
	}

	int state = GetItemImage(item);
	if (state == ICON_CHECK_TRUE || state == ICON_CHECK_FALSE)
	{
		SetItemImage(item, 1-state);
		wxTreeEvent tree_evt( ::wxEVT_COMMAND_TREE_ITEM_ACTIVATED , this, item );
		tree_evt.SetPoint( evt.GetPosition() );
		tree_evt.SetLabel( GetItemText(item) );
		ProcessEvent( tree_evt );
	}
	else
		evt.Skip();
}
