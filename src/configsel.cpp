#include <wx/textfile.h>
#include <wx/config.h>
#include <wx/log.h>
#include <wx/tokenzr.h>

#include <wex/choicetree.h>

#include "configsel.h"
#include "main.h"

enum { ID_TechTree = wxID_HIGHEST+98, ID_FinTree };

BEGIN_EVENT_TABLE(ConfigDialog, wxDialog)
	EVT_CHOICETREE_SELCHANGE( ID_TechTree, ConfigDialog::OnTechTree)
	EVT_CHOICETREE_DOUBLECLICK( ID_FinTree, ConfigDialog::OnDoubleClick)
	EVT_BUTTON( wxID_HELP, ConfigDialog::OnHelp )
END_EVENT_TABLE()

ConfigDialog::ConfigDialog( wxWindow *parent, const wxSize &size )
	: wxDialog( parent, wxID_ANY, wxEmptyString, wxDefaultPosition, size )
{
	CenterOnParent();
	SetEscapeId(wxID_CANCEL);
	
	m_pTech = new wxChoiceTree( this, ID_TechTree );
	m_pFin = new wxChoiceTree( this, ID_FinTree );

	wxBoxSizer *choice_sizer = new wxBoxSizer( wxHORIZONTAL );
	choice_sizer->Add( m_pTech, 1, wxALL|wxEXPAND, 0 );
	choice_sizer->Add( m_pFin, 1, wxALL|wxEXPAND, 0 );

	wxStaticText *label = new wxStaticText( this, wxID_ANY,
		"Project configuration: select a technology and then a financing option." );
	wxFont font = label->GetFont();
	font.SetWeight( wxFONTWEIGHT_BOLD );
	font.SetPointSize( font.GetPointSize() + 1 );
	label->SetFont( font );

	m_pChkUseDefaults = new wxCheckBox(this, wxID_ANY, "Reset new inputs to Tech/Market-specific default values" );	
	m_pChkUseDefaults->SetValue(true);

	wxBoxSizer *hbox = new wxBoxSizer (wxHORIZONTAL );
	hbox->Add( new wxButton(this, wxID_HELP, "Help..." ), 0, wxALL|wxEXPAND, 4 );
	hbox->Add( m_pChkUseDefaults, 0, wxALL|wxEXPAND|wxALIGN_CENTER_VERTICAL, 4 );
	hbox->AddStretchSpacer();
	hbox->Add( CreateButtonSizer( wxOK|wxCANCEL ), 0, wxALL, 4 );
	
	wxBoxSizer *vbox = new wxBoxSizer( wxVERTICAL );
	vbox->Add( label, 0, wxALL|wxEXPAND|wxALIGN_CENTER_VERTICAL, 8 );
	vbox->Add( choice_sizer, 1, wxALL|wxEXPAND, 0 );
	vbox->Add( hbox, 0, wxALL|wxEXPAND, 0 );	
	SetSizer( vbox );
	
	wxAcceleratorEntry entries[1];
	entries[0].Set( ::wxACCEL_NORMAL, WXK_F1, wxID_HELP );
	SetAcceleratorTable( wxAcceleratorTable(1,entries) );

	PopulateTech();

}

bool ConfigDialog::ResetToDefaults()
{
	return m_pChkUseDefaults->GetValue();
}
void ConfigDialog::SetConfiguration(const wxString &t, const wxString &f)
{
	m_t = t;
	m_f = f;
	
	wxArrayString techlist = SamApp::Config().GetTechnologies();
	PopulateTree( m_pTech, SamApp::Config().TechTree(), &techlist );
	SelectExpandByMetaTag( m_pTech, t );

	wxArrayString finlist = SamApp::Config().GetFinancingForTech( t );
	PopulateTree( m_pFin, SamApp::Config().FinTree(), &finlist );
	SelectExpandByMetaTag( m_pFin, f );
}

void ConfigDialog::ShowResetCheckbox(bool b)
{
	m_pChkUseDefaults->Show(b);
}

bool ConfigDialog::GetConfiguration(wxString &t, wxString &f)
{
	// must read current tree configuration
	wxChoiceTreeItem *tech_item = m_pTech->GetSelection();
	wxChoiceTreeItem *fin_item = m_pFin->GetSelection();

	if (!tech_item || !fin_item)
		return false;

	t = tech_item->MetaTag;
	f = fin_item->MetaTag;

	if ( t.IsEmpty() || f.IsEmpty() )
	{
		wxMessageBox("ConfigDialog::GetConfiguration internal selection error - please report.");
		return false;
	}

	return true;
}


void ConfigDialog::OnDoubleClick(wxCommandEvent &evt)
{
	if (!IsModal())
		wxMessageBox("Error! calling EndModal() on non-modal dialog");

	EndModal( wxID_OK );
}

void ConfigDialog::PopulateTech()
{
	wxArrayString metalist = SamApp::Config().GetTechnologies();
	std::vector<ConfigDatabase::TreeItem*> &tree = SamApp::Config().TechTree();
	PopulateTree( m_pTech, tree, &metalist );
	m_pFin->DeleteAll();
	m_pFin->Invalidate();
}

bool ConfigDialog::CheckItemForMetaTag(ConfigDatabase::TreeItem *item, wxArrayString *metalist)
{
	if ( !item || !metalist ) return false;

	for (size_t i=0;i<item->Children.size();i++)
		if ( CheckItemForMetaTag(item->Children[i], metalist) )
			return true;

	return (metalist->Index( item->TypeTag ) >= 0);
}

void ConfigDialog::AppendItem( wxChoiceTree *aftree, wxChoiceTreeItem *parent, 
							  ConfigDatabase::TreeItem *item, wxArrayString *metalist)
{
	// check that item or any children are in metalist
	if ( !CheckItemForMetaTag( item, metalist ) )
		return;

	//wxString pngfn = SamApp::GetRuntimePath() + "/png/" + item->BmpName + ".png";
	wxChoiceTreeItem *afitem = aftree->Add( 
		item->Label, 
		item->Description, 
		wxNullBitmap, //wxFileExists(pngfn) ? wxBitmap( pngfn, wxBITMAP_TYPE_PNG) : wxNullBitmap,
		parent);

	afitem->MetaTag = item->TypeTag;

	for ( size_t i=0; i<item->Children.size(); i++ )
		AppendItem( aftree, afitem, item->Children[i], metalist );
}

void ConfigDialog::PopulateTree( wxChoiceTree *tree, std::vector<ConfigDatabase::TreeItem*> &items, wxArrayString *metalist)
{
	tree->DeleteAll();

	for( size_t i=0; i<items.size(); i++ )
		AppendItem( tree, 0, items[i], metalist );

	tree->CollapseAll();
	tree->Invalidate();
}

void ConfigDialog::OnTechTree( wxCommandEvent &evt )
{
	
	if ( wxChoiceTreeItem *item = m_pTech->GetSelection() ) 
	{
		wxArrayString metalist = SamApp().Config().GetFinancingForTech( item->MetaTag );
		PopulateTree( m_pFin, SamApp::Config().FinTree(), &metalist );
	}
	else
	{	
		m_pFin->DeleteAll();
		m_pFin->Invalidate();
	}
}


wxChoiceTreeItem *ConfigDialog::FindTag( wxChoiceTreeItem *item, const wxString &tag )
{
	if ( item == 0 ) return 0;
	if ( item->MetaTag == tag ) return item;
	for( size_t i=0;i<item->Children.size();i++)
		if ( wxChoiceTreeItem *p = FindTag( item->Children[i], tag ) )
			return p;

	return 0;
}

void ConfigDialog::SelectExpandByMetaTag(wxChoiceTree *aftree, const wxString &tag)
{
	wxChoiceTreeItem *item = NULL;
	std::vector<wxChoiceTreeItem*> roots = aftree->GetRootItems();
	for (int i=0;i<roots.size();i++)
	{
		item = FindTag( roots[i], tag );
		if (item) break;
	}

	if (!item)
		aftree->CollapseAll();
	else
	{
		aftree->ExpandTo( item );
		aftree->Select( item );
	}

	aftree->Invalidate();
}

void ConfigDialog::OnHelp(wxCommandEvent &evt)
{
	SamApp::ShowHelp("Technology Market");
}

bool ShowConfigurationDialog( wxWindow *parent, wxString *tech, wxString *fin, bool *reset )
{
/*	if ( parent == 0 ) return false;

	wxPoint pos = parent->ClientToScreen( wxPoint(0,0) );
	wxSize size = parent->GetClientSize();

	wxFrame *trans = new wxFrame( parent, wxID_ANY, wxEmptyString,  pos, size, 
		wxBORDER_NONE | wxFRAME_FLOAT_ON_PARENT | wxFRAME_NO_TASKBAR );
	trans->SetBackgroundColour( *wxLIGHT_GREY );
	trans->SetTransparent( 200 );
	trans->Show();


	ConfigDialog *dlg = new ConfigDialog( parent );
	dlg->ShowResetCheckbox( *reset );
	if ( !tech->IsEmpty() && !fin->IsEmpty() )
		dlg->SetConfiguration( *tech, *fin );

	bool result = false;
	if ( dlg->ShowModal() == wxID_OK )
	{
		dlg->GetConfiguration( *tech, *fin );
		*reset = dlg->ResetToDefaults();
		result = true;
	}

	dlg->Destroy();
	trans->Destroy();
	return result;*/

	ConfigDialog dlg( parent );
	dlg.ShowModal();
	
	return false;
}
