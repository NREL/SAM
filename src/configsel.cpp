#include <wx/textfile.h>
#include <wx/config.h>
#include <wx/log.h>
#include <wx/tokenzr.h>

#include <wex/choicetree.h>

#include "configsel.h"
#include "main.h"

static ConfigTree sg_ConfigTree;


ConfigTree::ConfigTree()
{
}

ConfigTree::~ConfigTree()
{
	/* nothing to do */
}

bool ConfigTree::Load( const wxString &cfg_file )
{
	m_techTreeRoot.Clear();
	m_finTreeRoot.Clear();

	enum { TM_NONE, TM_TECHTREE, TM_FINTREE };
	int mode = TM_NONE;
	std::vector<TreeItem*> parent_stack;
	TreeItem *prev_tree_item = NULL;
	int cur_indent_level = 0;
	
	wxTextFile tf;
	if (!tf.Open( cfg_file )) return false;

	wxString ln;
	for ( ln = tf.GetFirstLine(); !tf.Eof(); ln = tf.GetNextLine() )
	{
		ln.Trim(true).Trim();
		if (ln.Len() < 1) continue;
		if (ln[0] == '\'') continue;

		if (ln == ".techtree")
		{
			mode = TM_TECHTREE;
			cur_indent_level = 0;
			prev_tree_item = NULL;
			parent_stack.clear();
			parent_stack.push_back( &m_techTreeRoot );
		}
		else if (ln == ".fintree")
		{
			mode = TM_FINTREE;
			cur_indent_level = 0;
			prev_tree_item = NULL;
			parent_stack.clear();
			parent_stack.push_back( &m_finTreeRoot );
		}
		else
		{
			if (mode == TM_TECHTREE	|| mode == TM_FINTREE)
			{
				int ilev = 0;
				while(ilev<(int)ln.Len() && ln[ilev] == '&')
					ilev++;

				ln = ln.Mid(ilev);
				wxArrayString parts = wxStringTokenize(ln,";");
				if (ilev > cur_indent_level)
				{
					// current parent is previous item
					if (prev_tree_item)
					{
						// prepend 'prev_tree_item'
						parent_stack.push_back(0);
						for( size_t i=parent_stack.size()-1;i>0;i-- )
							parent_stack[i] = parent_stack[i-1];
						parent_stack[0] = prev_tree_item;
					}
				}
				else if (ilev < cur_indent_level)
				{
					// revert to previous parent
					while (cur_indent_level > ilev)
					{
						if (parent_stack.size() > 0)
							parent_stack.erase( parent_stack.begin() );

						cur_indent_level--;
					}
				}

				// add a new tree item to current parent
				cur_indent_level = ilev;

				if (parent_stack.size() == 0 || parts.size() != 4 )
					return false;
				else
				{
					TreeItem *item = new TreeItem;
					item->Label = parts[0];
					item->Description = parts[1];
					if (item->Description == "*") item->Description = "";
					item->BmpName = parts[2];
					item->TypeTag = parts[3];
					if (item->TypeTag == "*") item->TypeTag = "";

					parent_stack[0]->Children.push_back( item );
					prev_tree_item = item;
				}
				
			}
		}
	}

	return true;
}

ConfigTree &ConfigTree::Get()
{
	return sg_ConfigTree;
}

enum { ID_TechTree = wxID_HIGHEST+98, ID_FinTree, ID_btnHelp };

BEGIN_EVENT_TABLE( ConfigCtrl, wxPanel )
	EVT_CHOICETREE_SELCHANGE( ID_TechTree, ConfigCtrl::OnTechTree)
	EVT_CHOICETREE_DOUBLECLICK( ID_TechTree, ConfigCtrl::OnDoubleClick)
	EVT_CHOICETREE_DOUBLECLICK( ID_FinTree, ConfigCtrl::OnDoubleClick)
END_EVENT_TABLE()

ConfigCtrl::ConfigCtrl(wxWindow *parent, int id, 
		const wxPoint &pos, const wxSize &sz, 
		long flags)
		: wxPanel( parent, id, pos, sz, flags )
{
	m_pTech = new wxChoiceTree( this, ID_TechTree );
	m_pTech->SetBackgroundColour(*wxWHITE);

	m_pFin = new wxChoiceTree( this, ID_FinTree );
	m_pFin->SetBackgroundColour(*wxWHITE);

	wxBoxSizer *sizer = new wxBoxSizer( wxHORIZONTAL );
	sizer->Add( m_pTech, 1, wxALL|wxEXPAND, 0 );
	sizer->Add( m_pFin, 1, wxALL|wxEXPAND, 0 );
	SetSizer( sizer );
}

void ConfigCtrl::OnDoubleClick(wxCommandEvent &evt)
{
	wxString t,f;
	if (GetConfiguration(t,f))
		if (wxDialog *dlg = dynamic_cast<wxDialog*>(GetParent()))
			dlg->EndModal( wxID_OK );
}

void ConfigCtrl::PopulateTech()
{
	wxArrayString metalist = SamApp::CfgDB().GetTechnologies();
	std::vector<ConfigTree::TreeItem*> tree = ConfigTree::Get().Tech();
	PopulateTree( m_pTech, tree, &metalist );
	m_pFin->DeleteAll();
	m_pFin->Invalidate();
}

bool ConfigCtrl::CheckItemForMetaTag(ConfigTree::TreeItem *item, wxArrayString *metalist)
{
	if ( !item || !metalist ) return false;

	for (size_t i=0;i<item->Children.size();i++)
		if ( CheckItemForMetaTag(item->Children[i], metalist) )
			return true;

	return (metalist->Index( item->TypeTag ) >= 0);
}

void ConfigCtrl::AppendItem( wxChoiceTree *aftree, wxChoiceTreeItem *parent, ConfigTree::TreeItem *item, wxArrayString *metalist)
{
	// check that item or any children are in metalist
	//if ( !CheckItemForMetaTag( item, metalist ) )
	//	return;

	wxString pngfn = SamApp::GetRuntimePath() + "/png/" + item->BmpName + ".png";
	wxChoiceTreeItem *afitem = aftree->Add( 
		item->Label, 
		item->Description, 
		wxFileExists(pngfn) ? wxBitmap( pngfn, wxBITMAP_TYPE_PNG) : wxNullBitmap,
		parent);

	afitem->MetaTag = item->TypeTag;

	for ( size_t i=0; i<item->Children.size(); i++ )
		AppendItem( aftree, afitem, item->Children[i], metalist );
}

void ConfigCtrl::PopulateTree( wxChoiceTree *tree, std::vector<ConfigTree::TreeItem*> &items, wxArrayString *metalist)
{
	tree->DeleteAll();

	for( size_t i=0; i<items.size(); i++ )
		AppendItem( tree, 0, items[i], metalist );

	tree->CollapseAll();
	tree->Invalidate();
}

void ConfigCtrl::OnTechTree( wxCommandEvent &evt )
{
	wxChoiceTreeItem *item = m_pTech->GetSelection();

	if ( !item ) 
	{
		m_pFin->DeleteAll();
		m_pFin->Invalidate();
	}
	else
	{	
		wxArrayString metalist = SamApp().CfgDB().GetFinancingForTech( item->MetaTag );
		std::vector<ConfigTree::TreeItem*> tree = ConfigTree::Get().Fin();
		PopulateTree( m_pFin, tree, &metalist );
	}
}


wxChoiceTreeItem *ConfigCtrl::FindTag(wxChoiceTreeItem *parent, const wxString &tag)
{
	if (!parent)
		return NULL;

	if (parent->MetaTag == tag)
		return parent;

	for (int i=0;i<parent->Children.size();i++)
	{
		if (parent->Children[i]->MetaTag == tag)
			return parent->Children[i];

		wxChoiceTreeItem *item = NULL;
		if ( (item = FindTag(parent->Children[i], tag)) )
			return item;
	}

	return NULL;
}

void ConfigCtrl::SelectExpandByMetaTag(wxChoiceTree *aftree, const wxString &tag)
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

void ConfigCtrl::SetConfiguration(const wxString &t, const wxString &f)
{
	m_t = t;
	m_f = f;
	
	std::vector<ConfigTree::TreeItem*> tree = ConfigTree::Get().Tech();
	PopulateTree( m_pTech, tree );
	SelectExpandByMetaTag( m_pTech, t );

	wxArrayString finlist = SamApp::CfgDB().GetFinancingForTech( t );
	tree = ConfigTree::Get().Fin();
	PopulateTree( m_pFin, tree, &finlist );
	SelectExpandByMetaTag( m_pFin, f );
}

bool ConfigCtrl::GetConfiguration(wxString &t, wxString &f)
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
		wxMessageBox("ConfigCtrl::GetConfiguration internal selection error - please report.");
		return false;
	}

	return true;
}

BEGIN_EVENT_TABLE(ConfigDialog, wxDialog)
EVT_BUTTON( wxID_OK, ConfigDialog::OnCommand )
EVT_BUTTON( wxID_CANCEL, ConfigDialog::OnCommand )
EVT_BUTTON( ID_btnHelp, ConfigDialog::OnCommand )
END_EVENT_TABLE()

ConfigDialog::ConfigDialog( wxWindow *parent, const wxSize &size )
	: wxDialog( parent, wxID_ANY, wxEmptyString, wxDefaultPosition, size, wxBORDER_NONE )
{
	CenterOnParent();
	SetEscapeId(wxID_CANCEL);

	m_cfgCtrl = new ConfigCtrl(this, -1, wxPoint(0,21), wxSize(700, 510));

	/* AFLabel *lbl = new AFLabel(this,-1,"1. Select a technology:", wxPoint(0,0), wxSize(350,21));
	lbl->SetBold(true);
	lbl->SetRelativeSize(1);

	lbl = new AFLabel(this,-1,"2. Select a financing option:", wxPoint(350,0), wxSize(350,21));
	lbl->SetBold(true);
	lbl->SetRelativeSize(1);
	*/
	wxStaticText *label = new wxStaticText( this, wxID_ANY,
		"Project configuration: select a technology and then a financing option." );
	wxFont font = label->GetFont();
	font.SetWeight( wxFONTWEIGHT_BOLD );
	font.SetPointSize( font.GetPointSize() + 1 );
	label->SetFont( font );

	wxButton *btnHelp = new wxButton(this, ID_btnHelp, "Help...", wxPoint(5, 542), wxSize(70,21));

	m_pChkUseDefaults = new wxCheckBox(this, wxID_ANY, "Reset new inputs to Tech/Market-specific default values", 
		wxPoint(80,542), wxSize( 450, 21));
	
	m_pChkUseDefaults->SetValue(true);

	wxBoxSizer *hbox = new wxBoxSizer (wxHORIZONTAL );
	hbox->Add( btnHelp, 0, wxALL|wxEXPAND, 4 );
	hbox->Add( m_pChkUseDefaults, 0, wxALL|wxEXPAND|wxALIGN_CENTER_VERTICAL, 4 );
	hbox->AddStretchSpacer();
	hbox->Add( CreateButtonSizer( wxOK|wxCANCEL ), 0, wxALL|wxEXPAND, 4 );
	
	wxBoxSizer *vbox = new wxBoxSizer( wxVERTICAL );
	vbox->Add( label, 0, wxALL|wxEXPAND|wxALIGN_CENTER_VERTICAL, 8 );
	vbox->Add( m_cfgCtrl, 1, wxALL|wxEXPAND, 0 );
	vbox->Add( hbox, 0, wxALL|wxEXPAND );
	
	SetSizer( vbox );
	Layout();

	m_cfgCtrl->PopulateTech();
	
	wxAcceleratorEntry entries[1];
	entries[0].Set( ::wxACCEL_NORMAL, WXK_F1, ID_btnHelp );
	SetAcceleratorTable( wxAcceleratorTable(1,entries) );

}

bool ConfigDialog::ResetToDefaults()
{
	return m_pChkUseDefaults->GetValue();
}
void ConfigDialog::SetConfiguration(const wxString &t, const wxString &f)
{
	m_cfgCtrl->SetConfiguration(t,f);
}

void ConfigDialog::ShowResetCheckbox(bool b)
{
	m_pChkUseDefaults->Show(b);
}

void ConfigDialog::GetConfiguration(wxString &t, wxString &f)
{
	m_cfgCtrl->GetConfiguration(t,f);
}

void ConfigDialog::OnCommand(wxCommandEvent &evt)
{
	if (evt.GetId() == wxID_OK)
	{

		wxString t,f;
		if (!m_cfgCtrl->GetConfiguration(t,f))
		{
			wxMessageBox("You must select both a technology and financing option!");
			return;
		}
		
		EndModal( wxID_OK );
	}
	else if (evt.GetId() == ID_btnHelp)
	{
		SamApp::ShowHelp("Technology Market");
	}
	else
	{
		EndModal( wxID_CANCEL );
	}
}


bool ShowConfigurationDialog( wxWindow *parent, wxString *tech, wxString *fin, bool *reset )
{
	if ( parent == 0 ) return false;

	wxPoint pos = parent->ClientToScreen( wxPoint(0,0) );
	wxSize size = parent->GetClientSize();

	wxFrame *trans = new wxFrame( parent, wxID_ANY, wxEmptyString,  pos, size, 
		wxBORDER_NONE | wxFRAME_FLOAT_ON_PARENT | wxFRAME_NO_TASKBAR );
	trans->SetBackgroundColour( *wxLIGHT_GREY );
	trans->SetTransparent( 200 );
	trans->Show();

	/*
	wxSize dialog_size( (int)(0.5*size.GetWidth()), (int)(0.75*size.GetHeight()) );

	if( dialog_size.GetWidth() < 600 ) dialog_size.SetWidth( 600 );
	if( dialog_size.GetHeight() < 400 ) dialog_size.SetHeight( 400 );

	if( dialog_size.GetWidth() > 900 ) dialog_size.SetWidth( 900 );
	if( dialog_size.GetHeight() > 900 ) dialog_size.SetHeight( 900 );
	*/

	ConfigDialog *dlg = new ConfigDialog( trans );
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
	return result;
}
