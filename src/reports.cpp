/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <algorithm>

#include <wx/datstrm.h>
#include <wx/splitter.h>
#include <wx/aui/aui.h>
#include <wx/wfstream.h>
#include <wx/filename.h>
#include <wx/statline.h>
#include <wx/tokenzr.h>
#include <wx/richtext/richtextsymboldlg.h>
#include <wx/filesys.h>

#include "main.h"
#include "case.h"
#include "reports.h"
#include "graph.h"
#include "casewin.h"
#include "lossdiag.h"

#include <wex/codeedit.h>
#include <wex/pagelayout.h>
#include <wex/pageobjects.h>
#include <wex/metro.h>
#include <wex/numeric.h>
#include <wex/extgrid.h>
#include <wex/lkscript.h>
#include <wex/exttext.h>
#include <wex/label.h>
#include <wex/utils.h>

#include <wex/icons/stock_dnd_32.cpng>

//#include <wex/plot/plaxis.h>

SamReportTemplate::SamReportTemplate()
{
	m_specificModelsOnly = false;
}

SamReportTemplate::~SamReportTemplate()
{
	Clear();
}


void SamReportTemplate::Copy( SamReportTemplate *src )
{
	Clear();
	for (int i=0;i<src->GetPageCount();i++)
		AddPage( src->GetPage(i)->Duplicate() );

	m_description = src->m_description;
	m_author = src->m_author;
	m_meta1 = src->m_meta1;
	m_meta2 = src->m_meta2;
	m_specificModelsOnly = src->m_specificModelsOnly;
	m_techModels = src->m_techModels;
	m_finModels = src->m_finModels;
	m_header = src->m_header;
	m_footer = src->m_footer;
}

void SamReportTemplate::AddPage( wxPageLayout *page )
{
	if ( std::find( m_pages.begin(), m_pages.end(), page ) == m_pages.end() )
		m_pages.push_back( page );
}

void SamReportTemplate::DeletePage( wxPageLayout *page )
{
	std::vector<wxPageLayout*>::iterator it = std::find( m_pages.begin(), m_pages.end(), page );
	if (  it != m_pages.end() )
	{
		m_pages.erase( it );
		delete page;
	}
}

std::vector<wxPageLayout*> SamReportTemplate::GetPages()
{
	return m_pages;
}

wxPageLayout *SamReportTemplate::GetPage( size_t idx )
{
	if ( idx < m_pages.size() ) return m_pages[idx];
	else return 0;
}

int SamReportTemplate::GetPageCount()
{
	return m_pages.size();
}

void SamReportTemplate::Clear()
{
	for (int i=0;i<(int)m_pages.size();i++)
		delete m_pages[i];
	m_pages.clear();
	m_description.Empty();
	m_author.Empty();
	m_meta1.Empty();
	m_meta2.Empty();
	m_specificModelsOnly = false;
	m_techModels.Clear();
	m_finModels.Clear();
	m_header.Empty();
	m_footer.Empty();
}

void SamReportTemplate::SetInfo(const wxString &desc, const wxString &author, const wxString &meta1, const wxString &meta2 )
{
	m_description = desc;
	m_author = author;
	m_meta1 = meta1;
	m_meta2 = meta2;
}

void SamReportTemplate::GetInfo( wxString *desc, wxString *author, wxString *meta1, wxString *meta2 )
{
	if (desc) *desc = m_description;
	if (author) *author = m_author;
	if (meta1) *meta1 = m_meta1;
	if (meta2) *meta2 = m_meta2;
}

void SamReportTemplate::SetModels( const wxArrayString &techmods, const wxArrayString &finmods )
{
	m_techModels = techmods;
	m_finModels = finmods;
}

void SamReportTemplate::GetModels( wxArrayString *techmods, wxArrayString *finmods )
{
	if (techmods) *techmods = m_techModels;
	if (finmods) *finmods = m_finModels;
}

void SamReportTemplate::SetHeaderFooter( const wxString &header, const wxString &footer )
{
	m_header = header;
	m_footer = footer;
}

void SamReportTemplate::GetHeaderFooter( wxString *header, wxString *footer )
{
	if (header) *header = m_header;
	if (footer) *footer = m_footer;
}

bool SamReportTemplate::Write( const wxString &file )
{
	wxFileOutputStream fout( file );
	if ( !fout.IsOk() )
		return false;

	wxDataOutputStream ds( fout );
	ds.Write16( 0xfe ); // start code for data file
	ds.Write8( 1 ); // version
	ds.WriteString( m_description ); // description;
	ds.WriteString( m_author ); // author
	ds.WriteString( m_meta1 ); // meta data tag 1
	ds.WriteString( m_meta2 ); // meta data tag 2
	ds.Write8( m_specificModelsOnly ? 1 : 0 ); // only allow specific models
	ds.WriteString( wxJoin( m_techModels, ',' ) ); // allowed technologies
	ds.WriteString( wxJoin( m_finModels, ',' ) ); // allowed financing types
	ds.WriteString( m_header ); // header text (with escapes)
	ds.WriteString( m_footer ); // footer text (with escapes)
	ds.Write32( m_pages.size() ); // number of pages

	for (int i=0;i<(int)m_pages.size();i++)
		if (!m_pages[i]->Write( fout ))
			return false;

	ds.Write16( 0xfe ); // finish code for data file

	return true;
}

bool SamReportTemplate::Read( const wxString &file )
{
	wxFileInputStream fin( file );
	if ( !fin.IsOk() )
		return false;

	Clear();
	wxDataInputStream ds( fin );
	unsigned short start_code = ds.Read16();
	ds.Read8(); // unsigned char ver;
	m_description = ds.ReadString();
	m_author = ds.ReadString();
	m_meta1 = ds.ReadString();
	m_meta2 = ds.ReadString();
	m_specificModelsOnly = ds.Read8() ? true : false;
	m_techModels = wxStringTokenize( ds.ReadString(), "," );
	m_finModels = wxStringTokenize( ds.ReadString(), "," );
	m_header = ds.ReadString();
	m_footer = ds.ReadString();
	size_t npages = ds.Read32();
	for (size_t i=0;i<npages;i++)
	{
		wxPageLayout *page = new wxPageLayout;
		if (!page->Read( fin ))
		{
			delete page;
			return false;
		}
		m_pages.push_back( page );
	}

	return (ds.Read16() == start_code); // check end code
}

void SamReportTemplate::SetCaseName( const wxString &cn )
{
	for (int i=0;i<(int)m_pages.size();i++)
	{
		int count = 0;
		wxPageObject **children = m_pages[i]->GetObjects( &count );
		for (int j=0;j<count;j++)
			if ( SamReportObject *sobj = dynamic_cast<SamReportObject*>( children[j] ) )
				sobj->SetCaseName( cn );
	}
}

void SamReportTemplate::SetMetaData( VarValue *meta )
{
	for (int i=0;i<(int)m_pages.size();i++)
	{
		int count = 0;
		wxPageObject **children = m_pages[i]->GetObjects( &count );
		for (int j=0;j<count;j++)
			if ( SamReportObject *sobj = dynamic_cast<SamReportObject*>( children[j] ) )
				sobj->SetMetaData( meta );
	}
}

bool SamReportTemplate::RenderPdf( const wxString &file, Case *c, VarValue *meta )
{
	if ( c != 0 ) SetCaseName( SamApp::Project().GetCaseName( c ) );
	if ( meta != 0 ) SetMetaData( meta );
	wxPagePdfRenderer pdf;
	for (int i=0;i<(int)m_pages.size();i++)
		pdf.AddPage( m_pages[i], EscapeHF(m_header), EscapeHF(m_footer) );


	bool ok = pdf.Render( file );
	SetMetaData( 0 ); // clear pointers
	return ok;
}

wxString SamReportTemplate::EscapeHF( wxString hf )
{
	hf.Replace("@SAMVER@", SamApp::VersionStr() );
	return hf;
}

enum { IDR_REPORT_COMMAND_FIRST = 12451,

	IDR_ADDPAGE, IDR_DELETEPAGE,
	IDR_ZOOMFIT, IDR_ZOOM100,
	IDR_NEW, IDR_PROPERTIES, IDR_SAVE, IDR_SAVEAS, IDR_OPEN, IDR_EXPORT, 

	IDR_REPORT_COMMAND_LAST,

	// controls
	IDR_PAGELIST, IDR_PAGELAYOUT, IDR_CASELIST,

	// objects
	IDR_CREATE_OBJECT, IDR_CREATE_OBJECT_LAST = IDR_CREATE_OBJECT+50};

BEGIN_EVENT_TABLE( SamReportWindow, wxPanel )
	EVT_COMMAND_RANGE( IDR_REPORT_COMMAND_FIRST, IDR_REPORT_COMMAND_LAST, wxEVT_COMMAND_BUTTON_CLICKED, SamReportWindow::OnCommand )
	EVT_COMMAND_RANGE( IDR_CREATE_OBJECT, IDR_CREATE_OBJECT_LAST, wxEVT_COMMAND_BUTTON_CLICKED, SamReportWindow::OnCreateObject )
	EVT_COMBOBOX( IDR_CASELIST, SamReportWindow::OnCaseSel )
	EVT_PAGELAYOUT_MODIFY( IDR_PAGELAYOUT, SamReportWindow::OnLayoutModify )
	EVT_PAGELAYOUT_CREATE( IDR_PAGELAYOUT, SamReportWindow::OnLayoutCreate )
	EVT_PAGELAYOUT_SELECT( IDR_PAGELAYOUT, SamReportWindow::OnLayoutSelect )
	EVT_LIST_ITEM_SELECTED( IDR_PAGELIST, SamReportWindow::OnPageSel )
	EVT_LIST_ITEM_DESELECTED( IDR_PAGELIST, SamReportWindow::OnPageDesel )
	EVT_LIST_ITEM_ACTIVATED( IDR_PAGELIST, SamReportWindow::OnPageSel )
	EVT_LIST_ITEM_RIGHT_CLICK( IDR_PAGELIST, SamReportWindow::OnPageRClick )
END_EVENT_TABLE()


void RegisterReportObjectTypes()
{
	static bool first_load = true;
	if (first_load)
	{
		wxPageObjectTypes::Register( new SamReportTextObject );
		wxPageObjectTypes::Register( new SamReportGraphObject );
		//wxPageObjectTypes::Register( new SamReportLossObject );
		wxPageObjectTypes::Register( new SamReportTableObject );
		wxPageObjectTypes::Register( new SamReportScriptObject );
		wxPageObjectTypes::Register( new wxPageImageObject );
		wxPageObjectTypes::Register( new LossDiagramObject( true, true ) ); 
		first_load = false;
	}
}

SamReportWindow::SamReportWindow( wxWindow *parent )
	: wxPanel( parent )
{
	m_modified = false;
	m_fileName.Empty();

	wxBoxSizer *tools = new wxBoxSizer( wxHORIZONTAL );

	static const int space = 2;

	tools->Add( new wxButton( this, IDR_NEW, "New" ), 0, wxALL|wxEXPAND, space );
	tools->Add( new wxButton( this, IDR_OPEN, "Open" ), 0, wxALL|wxEXPAND, space );
	tools->Add( new wxButton( this, IDR_SAVE, "Save" ), 0, wxALL|wxEXPAND, space );
	tools->Add( new wxButton( this, IDR_SAVEAS, "Save as" ), 0, wxALL|wxEXPAND, space );
	tools->Add( new wxButton( this, IDR_EXPORT, "Export PDF" ), 0, wxALL|wxEXPAND, space );
	tools->Add( new wxButton( this, IDR_PROPERTIES, "Properties" ), 0, wxALL|wxEXPAND, space );
	tools->Add( new wxButton( this, IDR_ADDPAGE, "Add page" ), 0, wxALL|wxEXPAND, space );
	tools->Add( new wxButton( this, IDR_DELETEPAGE, "Delete page" ), 0, wxALL|wxEXPAND, space );
	tools->Add( new wxButton( this, IDR_ZOOMFIT, "Zoom fit" ), 0, wxALL|wxEXPAND, space );
	tools->Add( new wxButton( this, IDR_ZOOM100, "Zoom 100 %" ), 0, wxALL|wxEXPAND, space );

	/*
	wxArrayString types = wxPageObjectTypes::AllTypes();
	for (int i=0;i<(int)types.size();i++)
	{
		wxString desc = wxPageObjectTypes::DescriptionOf( types[i] );
		tools->Add( new wxButton( this, IDR_CREATE_OBJECT+i, "New " + desc ), 0, wxALL|wxEXPAND, space );
	}
	*/

	m_caseListCtrl = new wxComboBox( this, IDR_CASELIST, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0, 0, wxCB_READONLY );
	tools->Add( m_caseListCtrl, 0, wxALL|wxALIGN_CENTER_VERTICAL, space );	
	tools->AddStretchSpacer();

	wxSplitterWindow *split = new wxSplitterWindow( this, wxID_ANY,
		wxDefaultPosition, wxDefaultSize, wxSP_LIVE_UPDATE ); 
	split->SetMinimumPaneSize(20);

	m_imageList.Create(32, 32);
	m_imageList.Add(  wxBITMAP_PNG_FROM_DATA( stock_dnd_32 ) );

	m_pagesCtrl = new wxListView(split, IDR_PAGELIST, wxDefaultPosition, wxDefaultSize,  
				wxLC_REPORT | wxLC_SINGLE_SEL | wxLC_ALIGN_LEFT);
	m_pagesCtrl->SetImageList(&m_imageList, wxIMAGE_LIST_SMALL); 
	m_pagesCtrl->InsertColumn(0, "Report Pages");
	m_pagesCtrl->SetColumnWidth(0, (int)(250*wxGetScreenHDScale()));

	m_layoutCtrl = new wxPageLayoutCtrl( split, IDR_PAGELAYOUT );

	split->SplitVertically( m_pagesCtrl, m_layoutCtrl, (int)(200*wxGetScreenHDScale()) );

	m_statusText = new wxStaticText( this, wxID_ANY, "Ready" );

	wxBoxSizer *sz = new wxBoxSizer( wxVERTICAL );
	sz->Add( tools, 0, wxALL|wxEXPAND, 0 );
	sz->Add( split, 1, wxALL|wxEXPAND, 0 );
	sz->Add( m_statusText, 0, wxALL|wxEXPAND, 1 );
	SetSizer(sz);

	UpdateCaseList();
	m_caseListCtrl->SetSelection(1);
}

SamReportWindow::~SamReportWindow()
{
	m_template.Clear();
}


void SamReportWindow::OnPageDesel( wxListEvent & )
{
	m_layoutCtrl->SetPage( 0 );
}

void SamReportWindow::OnPageSel( wxListEvent & )
{
	int idx = m_pagesCtrl->GetFirstSelected();
	if (idx >= 0) m_layoutCtrl->SetPage( m_template.GetPage( idx ) );
}

void SamReportWindow::OnPageRClick( wxListEvent & )
{
//	int idx = evt.GetSelection();
//	idx = 0;
}

bool SamReportWindow::Save()
{
	if (m_fileName.IsEmpty()) return SaveAs();
	else 
	{
		if (m_template.Write( m_fileName ))
		{
			Modified(false);
			return true;
		}
		else
			return false;
	}
}


bool SamReportWindow::SaveAs()
{
	wxString path = wxPathOnly( m_fileName );
	if (path.IsEmpty()) path = SamApp::GetRuntimePath() + "/reports";
	wxFileDialog dlg(this, "Save Report Template Data File", SamApp::GetRuntimePath() + "/reports",
		wxEmptyString, "SAM Report Template (*.samreport)|*.samreport", wxFD_SAVE|wxFD_OVERWRITE_PROMPT );
	if (dlg.ShowModal() == wxID_OK)
	{
		wxString file = dlg.GetPath();
		if (wxFileName(file).GetExt().Lower() != "samreport")
			file += ".samreport";

		if ( !m_template.Write( file ) )
		{
			wxMessageBox("Failed to save template data file:\n\n", file );
			return false;
		}
		else
		{
			m_fileName = file;
			Modified(false);
			return true;
		}
	}
	else
		return false;
}

void SamReportWindow::Modified( bool b )
{
	m_modified = b;
	UpdateStatusText();
}

void SamReportWindow::UpdateStatusText()
{
	wxString file = m_fileName;
	if (file.IsEmpty()) file = "untitled";
	if (m_modified) file += " (modified)";
	m_statusText->SetLabel( file );
}

void SamReportWindow::OnCommand( wxCommandEvent &evt )
{
	switch(evt.GetId())
	{
	case IDR_ADDPAGE:
		{
			wxPageLayout *page = new wxPageLayout;
			m_template.AddPage( page );
			UpdatePageList();
			m_pagesCtrl->Select( m_template.GetPageCount() - 1 );
			m_layoutCtrl->SetPage( page );
			Modified();
		}
		break;

	case IDR_DELETEPAGE:
		if ( m_pagesCtrl->GetFirstSelected() >= 0
			&& wxYES== wxMessageBox("Really delete the selected page?  This cannot be undone.", "Query", wxYES_NO))
		{
			wxPageLayout *page = m_template.GetPage( m_pagesCtrl->GetFirstSelected() );
			m_layoutCtrl->SetPage( 0 );
			m_template.DeletePage( page );
			UpdatePageList();
			Modified();
		}
		break;

	case IDR_ZOOM100:
		m_layoutCtrl->Zoom( 100 );
		break;

	case IDR_ZOOMFIT:
		m_layoutCtrl->Zoom();
		break;

	case IDR_NEW:
		{
			m_layoutCtrl->SetPage( 0 );
			m_template.Clear();
			m_fileName.Empty();
			m_statusText->SetLabel( wxEmptyString );
			Modified(false);
			UpdateCaseList();
			UpdatePageList();
		}
		break;

	case IDR_PROPERTIES:
		{
			ReportPropertyDialog dlg( this );
			dlg.CenterOnParent();
			dlg.SetData( &m_template );
			if ( dlg.ShowModal() == wxID_OK )
			{
				bool changed = dlg.GetData( &m_template );
				if (changed) Modified();
			}
		}
		break;

	case IDR_SAVE:
		Save();
		break;

	case IDR_SAVEAS:
		SaveAs();
		break;

	case IDR_OPEN:
		{
			wxString path = wxPathOnly( m_fileName );
			if (path.IsEmpty())	path = SamApp::GetRuntimePath() + "/reports";
			wxFileDialog dlg(this, "Open Report Template Data File",  path,
				wxFileNameFromPath( m_fileName ),
				"SAM Report Template (*.samreport)|*.samreport" );
			if (dlg.ShowModal() == wxID_OK)
			{
				m_layoutCtrl->SetPage( 0 );
				if (!m_template.Read( dlg.GetPath() ))
					wxMessageBox("Error reading template data file:\n\n" + dlg.GetPath() );
				else
				{
					m_fileName = dlg.GetPath();
					m_statusText->SetLabel( m_fileName );
					Modified(false);

					if (m_template.GetPageCount() > 0)
					{
						m_pagesCtrl->Select(0);
						m_layoutCtrl->SetPage( m_template.GetPage(0) );
					}

					AssignCaseNames();
				}

				UpdateCaseList();
				UpdatePageList();
			}
		}
		break;
	case IDR_EXPORT:
		{
			wxFileDialog dlg(this, "Export PDF Report", wxEmptyString,
				"report.pdf", "Portable Document Format (*.pdf)|*.pdf", wxFD_SAVE|wxFD_OVERWRITE_PROMPT );
			if (dlg.ShowModal() == wxID_OK)
			{
				wxString file = dlg.GetPath();


				if (wxFileName(file).GetExt().Lower() != "pdf")
					file += ".pdf";

				if (m_caseListCtrl->GetSelection() > 1)
					m_template.SetCaseName( m_caseListCtrl->GetStringSelection() );
				else
					m_template.SetCaseName( wxEmptyString );

				if (!m_template.RenderPdf( file ))
					wxMessageBox("Failed to render PDF report:\n\n" + file);
				else
				{
				// URL encode to address user support issue 7/20/15
					wxString new_file = wxFileSystem::FileNameToURL(file);
					//wxMessageBox("new file=" + new_file);
					wxLaunchDefaultBrowser(new_file, wxBROWSER_NEW_WINDOW);
				}
			}
		}
		break;
	}
}

void SamReportWindow::OnCreateObject( wxCommandEvent &evt )
{
	wxPageLayout *page = m_layoutCtrl->GetPage();
	if (!page) return;

	wxArrayString types = wxPageObjectTypes::AllTypes();
	int idx = evt.GetId() - IDR_CREATE_OBJECT;
	if (idx >= 0 && idx < (int)types.size())
	{
		wxPageObject *obj = wxPageObjectTypes::Create( types[idx] );
		if (obj)
		{
			obj->SetGeometry(1, 1, 4, 3);
			page->Add( obj );
			Modified();
			if ( SamReportObject *r = dynamic_cast<SamReportObject*>(obj) )
			{
				if ( m_caseListCtrl->GetSelection() > 1 )
					r->SetCaseName( m_caseListCtrl->GetStringSelection() );
			}
		}
	}
}

void SamReportWindow::OnLayoutCreate( wxPageLayoutEvent &evt )
{
	if ( SamReportObject *r = dynamic_cast<SamReportObject*>( evt.GetPageObject() ) )
	{
		if ( m_caseListCtrl->GetSelection() > 1 )
			r->SetCaseName( m_caseListCtrl->GetStringSelection() );
	}
}

void SamReportWindow::OnLayoutModify( wxPageLayoutEvent & )
{
	Modified();
}

void SamReportWindow::OnLayoutSelect( wxPageLayoutEvent & )
{
	//wxLogStatus("layout ctrl: select!\n");
}

void SamReportWindow::UpdatePageList()
{
	m_pagesCtrl->Freeze(); // speed up the insertion
	m_pagesCtrl->DeleteAllItems();
	
	for (int i=0;i<m_template.GetPageCount();i++)
		m_pagesCtrl->InsertItem(i, wxString::Format("Page %d", i+1), 0);
	
	m_pagesCtrl->Thaw();

}

void SamReportWindow::OnCaseSel( wxCommandEvent & )
{
	if ( m_caseListCtrl->GetSelection() == 0 )
	{
		UpdateCaseList();
		m_caseListCtrl->SetSelection(1);
		return;
	}

	AssignCaseNames();
}

void SamReportWindow::AssignCaseNames()
{
	wxString ssel = m_caseListCtrl->GetStringSelection();

	if (m_caseListCtrl->GetSelection() == 1)
		ssel.Empty();
	m_template.SetCaseName( ssel );
	m_layoutCtrl->Invalidate();
}

void SamReportWindow::UpdateCaseList()
{
	wxString sel = m_caseListCtrl->GetStringSelection();

	m_caseListCtrl->Clear();
	m_caseListCtrl->Append("Refresh list");
	m_caseListCtrl->Append("No active case");	
	m_caseListCtrl->Append( SamApp::Project().GetCaseNames() );

	if ( !sel.IsEmpty() )
	{
		m_caseListCtrl->SetStringSelection( sel );
		AssignCaseNames();
	}
}

Case *SamReportObject::GetCase()
{
	return SamApp::Project().GetCase( m_caseName );
}

static wxString InsertVariable( wxWindow *parent, bool with_curly = true )
{
	wxArrayString names, labels;
	
	std::vector<Case*> cur_cases = SamApp::Project().GetCases();

	if ( cur_cases.size() == 0 )
	{
		wxMessageBox("To see a list of variables, create one or more cases first." );
		return wxEmptyString;
	}
	
	for (int i=0;i<(int)cur_cases.size();i++)
	{
		wxString case_name( SamApp::Project().GetCaseName( cur_cases[i] ) );
		wxArrayString output_names, output_labels, output_groups;
		Simulation::ListAllOutputs( cur_cases[i]->GetConfiguration(), &output_names, &output_labels, &output_groups, NULL, 0 );

		for (int j=0;j<(int)output_labels.size();j++)
		{
			if (!output_labels[j].IsEmpty())
			{
				names.Add(output_names[j]);
				wxString G( output_groups[j] );
				if ( !G.IsEmpty() )
					G = " (" + G + ")";
				labels.Add("[" + case_name + "] Outputs" + G + "/" + output_labels[j]);
			}
		}

		ConfigInfo *ci = cur_cases[i]->GetConfiguration();
		VarInfoLookup &vil = ci->Variables;

		for( VarInfoLookup::iterator it = vil.begin(); it != vil.end(); ++it )
		{
			wxString name = it->first;
			VarInfo &vi = *(it->second);

			wxString label = vi.Label;
			if (!label.IsEmpty())
			{
				if ( !vi.Units.IsEmpty() ) 
					label += " (" + vi.Units + ")";

				label += "  ";
			}

			label += "{ " + name + " }";

			if ( vi.Group.IsEmpty() ) label = "-Unsorted-/" + label;
			else label = vi.Group + "/" + label;
		
			label = "[" +case_name + "] Inputs/" + label;

			labels.Add( label );
			names.Add( name );
		}
	}

	wxSortByLabels(names, labels);
	SelectVariableDialog dlg( parent, "Select variable" );
	dlg.SetItems( names, labels );
	if (dlg.ShowModal() == wxID_OK)
	{
		wxArrayString names = dlg.GetCheckedNames();
		wxString text;
		for (int i=0;i<(int)names.size();i++)
		{
			if (with_curly) text += "${" + names[i] + "}";
			else text += names[i];

			if (i < (int)names.size()-1)
				text += ", ";
		}
		return text;
	}

	return wxEmptyString;
}

/* ***********************************************************
   ***********************************************************
   *********************************************************** */

enum { IDSRT_INSERTVAR = 1559 };

class SamReportTextObjectEditDialog : public wxPageTextObjectEditDialog
{
private:
	DECLARE_EVENT_TABLE()
	void OnInsertVar( wxCommandEvent & )
	{
		wxString text = InsertVariable(this);
		if (!text.IsEmpty())
			m_txtData->WriteText( text );
	}

public:
	SamReportTextObjectEditDialog( wxWindow *parent )
		: wxPageTextObjectEditDialog( parent )
	{
		m_toolSizer->Add( new wxButton( this, IDSRT_INSERTVAR, "Insert variable..." ), 0, wxALL|wxRIGHT, 4 );
		Fit();
	}
};

BEGIN_EVENT_TABLE( SamReportTextObjectEditDialog, wxPageTextObjectEditDialog )
EVT_BUTTON( IDSRT_INSERTVAR, SamReportTextObjectEditDialog::OnInsertVar )
END_EVENT_TABLE()

wxPageObject *SamReportTextObject::Duplicate()
{
	SamReportTextObject *obj = new SamReportTextObject;
	obj->Copy( this );
	return obj;
}

bool SamReportTextObject::EditObject( wxPageLayoutCtrl *layout )
{
	SamReportTextObject save;
	save.Copy( this );

	SamReportTextObjectEditDialog dlg( 0 );
	dlg.SetData( this, layout );
	if (dlg.ShowModal() == wxID_OK)
	{
		dlg.GetData( this );
		return true;
	}
	else
	{
		Copy( &save );
		return false;
	}
}


wxString SamReportFormatVariable( double value, const wxString &fmt )
{
	if (fmt.Len() == 0) return wxString::Format("%lg", value);

	if (fmt[0] == '.' || fmt[0] == ',')
	{
		int dec = wxAtoi( fmt.Mid(1) );
		if (dec < 1) dec = 0;
		if (dec > 30) dec = 30;		
		return wxNumericFormat( value, wxNUMERIC_REAL, dec, fmt[0]==',', wxEmptyString, wxEmptyString );
	}
	else if (fmt[0] == 'i')
		return wxString::Format("%d", (int)value);
	else
		return wxString::Format("%lg", value);	
}

/*

class ReportValueProvider : public EvalLookupTableBase
{
private:
	DataPile *m_dataPile;
	SymTab *m_symTab;
public:
	ReportValueProvider( DataPile *dp, SymTab *sy )
	{
		m_dataPile = dp;
		m_symTab = sy;
	}

	virtual bool GetValue( const wxString &name, double &val )
	{
		std::vector<double> vals;
		VarInfo *v = m_symTab->Lookup(name);
		if (v != 0 && v->GetType() == VAR_INTEGER)
		{
			val = (double) v->GetInt();
			return true;
		}
		else if ( v != 0 && v->GetType() == VAR_DOUBLE )
		{
			val = (double) v->GetDouble();
			return true;
		}
		else if ( m_dataPile != 0 && m_dataPile->Get( name, vals ) && vals.length() == 1 )
		{
			val = vals[0];
			return true;
		}

		return false;
	}
};

*/

wxString SamReportEscapeString( const wxString &input, Case *c, VarValue *meta )
{
	if ( !c ) return input;

	wxString text;
	size_t pos = 0;
	StringHash args;
	while ( pos < input.Len() )
	{
		if ( pos < input.Len()-1
			&& input[pos] == wxChar('$')
			&& (input[pos+1] == wxChar('{')
			   || input[pos+1] == wxChar('|')) )
		{
//			bool eqn_mode = false;
			wxChar end_char = wxChar('}');
			if ( input[pos+1] == wxChar('|') )
			{
//				eqn_mode = true;
				end_char = wxChar('|');
			}

			size_t pos2 = pos+2;
			while (pos2 < input.Len() && input[pos2] != end_char)
				pos2++;

			wxString data = input.Mid( pos+2, pos2-pos-2 );
			wxString var_name, arg_list;
			int semipos = data.Find(';');
			if (semipos > 0)
			{
				var_name = data.Left(semipos);
				arg_list = data.Mid(semipos+1);
			}
			else
				var_name = data;

			args.clear();
			args.Split( arg_list, ';', '=' );
			double scaling = 1;
			wxString format = "g";
			wxArrayString labels;
			wxArrayString swilist;

			if (args.find("format") != args.end()) format = args["format"];
			if (args.find("scale") != args.end()) scaling = atof( args["scale"].c_str() );
			if (args.find("label") != args.end()) labels = wxStringTokenize(args["label"], ",");
			if (args.find("switch") != args.end()) swilist = wxStringTokenize(args["switch"], ",");

			VarValue *v = c->Values().Get( var_name );
			if ( !v && meta && meta->Type() == VV_TABLE )
				v  = meta->Table().Get( var_name );

			if ( v )
			{
				if (v->Type() == VV_NUMBER && labels.size() != 0)
				{
					size_t idx = (size_t)v->Integer();
					if (idx < labels.size())
						text += labels[idx];
					else
						text += "('" + v->AsString() + "')";
				}
				else if (v->Type() == VV_NUMBER && swilist.size() != 0)
				{
					size_t idx = (size_t)v->Integer();
					if (idx < swilist.size())
					{
						if (swilist[idx].Left(1) == "'"
							&& swilist[idx].Right(1) == "'")
						{
							text += swilist[idx].Mid(1, swilist[idx].Len()-2);
						}
						else
						{
							VarValue *swvar = c->Values().Get( swilist[idx] );
							if (swvar)
								text += swvar->AsString();
							else
								text += "<?" + swilist[idx] + ">";
						}
					}
					else
						text += "{'" + v->AsString() + "'}";
				}
				else if (v->Type() == VV_NUMBER)
					text += SamReportFormatVariable( v->Value()*scaling, format );
				else
					text += v->AsString();
			}
			else
			{
				Simulation &sim = c->BaseCase();
				std::vector<double> vals;
				if ( VarValue *var = sim.GetValue( var_name ) )
				{
					if ( var->Type() == VV_ARRAY )
						vals = var->Array();
					else if ( var->Type() == VV_NUMBER )
						vals.push_back( var->Value() );

					if (args.find("index") != args.end())
					{
						int idx = wxAtoi( args["index"] );
						if (idx < 0) idx = 0;
						if (idx >= (int)vals.size()) idx = vals.size()-1;

						text += SamReportFormatVariable( vals[idx]*scaling, format );
					}
					else
					{
						for (int k=0;k<(int)vals.size();k++)
						{
							text += SamReportFormatVariable( vals[k]*scaling, format );
							if (k < (int)vals.size()-1) text+= ", ";
						}
					}
				}
				else
					text += "N/A";
			}
			
			pos = pos2+1;
		}
		else
			text += input[pos++];
	}

	return text;
}

void SamReportTextObject::Render( wxPageOutputDevice &dv )
{
	Case *c = SamApp::Project().GetCase( GetCaseName() );

	if ( !c ) wxPageTextObject::Render(dv);
	else
	{
		wxString save_text = m_text;
		m_text = SamReportEscapeString( save_text, c, GetMetaData() );
		wxPageTextObject::Render(dv);
		m_text = save_text;
	}
}



wxPageObject *SamReportGraphObject::Duplicate()
{
	SamReportGraphObject *obj = new SamReportGraphObject;
	obj->Copy( this );
	return obj;
}
bool SamReportGraphObject::Copy( wxPageObject *obj )
{
	if ( SamReportGraphObject *rhs = dynamic_cast<SamReportGraphObject*>( obj ) )
	{
		m_gi.Copy( &rhs->m_gi );
		return true;
	}
	else return false;
}

enum { ID_PROPERTIES = wxID_HIGHEST +239 };

class GraphPropertiesDialog : public wxDialog
{
	GraphProperties *m_properties;
	GraphCtrl *m_graph;
	Graph m_gi;
	Simulation *m_sim;
public:
	GraphPropertiesDialog( wxWindow *parent, Simulation *sim )
		: wxDialog( parent, wxID_ANY, "Edit graph properties", wxDefaultPosition, 
		wxScaleSize(900, 550), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER )
	{
		m_sim = sim;

		m_properties = new GraphProperties( this, ID_PROPERTIES );
		m_graph = new GraphCtrl( this, wxID_ANY );
		
		m_properties->SetupVariables( m_sim );

		wxBoxSizer *sizer = new wxBoxSizer( wxHORIZONTAL );
		sizer->Add( m_properties, 1, wxALL|wxEXPAND, 0 );
		sizer->Add( m_graph, 3, wxALL|wxEXPAND, 0 );

		wxBoxSizer *szver = new wxBoxSizer( wxVERTICAL );
		szver->Add( sizer, 1, wxALL|wxEXPAND, 0 );
		szver->Add( CreateButtonSizer( wxOK|wxCANCEL ), 0, wxALL|wxEXPAND, 5 );
		SetSizer(szver);		
	}

	void OnPropsChange( wxCommandEvent & )
	{
		m_properties->Get( m_gi );
		m_graph->Display( m_sim, m_gi );
	}

	void Set( Graph &g )
	{
		m_gi.Copy( &g );
		m_properties->Set( m_gi );
		m_graph->Display( m_sim, m_gi );
	}

	void Get( Graph &g ) { g.Copy( &m_gi ); }

	DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE( GraphPropertiesDialog, wxDialog )
	EVT_GRAPH_PROPERTY_CHANGE( ID_PROPERTIES, GraphPropertiesDialog::OnPropsChange )
END_EVENT_TABLE()

bool SamReportGraphObject::EditObject( wxPageLayoutCtrl *plc )
{
	Case *c = GetCase();
	if (!c)
	{
		wxMessageBox("A case must be selected to edit the graph.");
		return false;
	}
	
	GraphPropertiesDialog dlg( plc, &(c->BaseCase()) );
	dlg.Set( m_gi );
	if ( dlg.ShowModal() == wxID_OK )
	{
		dlg.Get( m_gi );
		UpdateImage();
		return true;
	}
	else return false;
}

void SamReportGraphObject::SetCaseName( const wxString &c )
{
	SamReportObject::SetCaseName( c );
	UpdateImage();
}

void SamReportGraphObject::UpdateImage()
{
	if ( Case *c = GetCase() )
	{
		GraphCtrl *ctrl = new GraphCtrl( SamApp::Window(), wxID_ANY );
		ctrl->Display( &c->BaseCase(), m_gi );
		m_imgCache = ctrl->GetBitmap( 640, 480 ).ConvertToImage();
		ctrl->Destroy();
	}
	else
		m_imgCache.Destroy();
}
void SamReportGraphObject::Render( wxPageOutputDevice &dv )
{
	if (m_imgCache.IsOk())
	{
		dv.Image( m_imgCache , m_x, m_y, m_width, m_height );
	}
	else
	{
		dv.Color( *wxRED );
		dv.Rect( m_x, m_y, m_width, m_height );
		dv.Line( m_x, m_y, m_x+m_width, m_y+m_height );
		dv.Line( m_x+m_width, m_y, m_x, m_y+m_height );
		dv.Color( *wxBLACK );
		dv.Font( wxPageOutputDevice::SERIF, 10, true, false );
		dv.Text( m_x, m_y, "SAM Graph Object - only valid when a case is specified." );
	}
}

bool SamReportGraphObject::ReadData( wxInputStream &is )
{
	bool ok = m_gi.Read( is );
	if (!ok)
		wxMessageBox("Error reading graph info from binary data stream");
	return ok;
}

bool SamReportGraphObject::WriteData( wxOutputStream &os )
{
	return m_gi.Write( os );
}

/*

wxPageObject *SamReportLossObject::Duplicate()
{
	SamReportLossObject *obj = new SamReportLossObject;
	obj->Copy( this );
	return obj;
}
bool SamReportLossObject::Copy( wxPageObject *obj )
{
	if ( SamReportLossObject *rhs = dynamic_cast<SamReportLossObject*>( obj ) )
	{
		return true;
	}
	else return false;
}

const wxImage &SamReportLossObject::Icon()
{
	static const wxImage ico( loss_icon_xpm );
	return ico;
}

bool SamReportLossObject::EditObject( wxPageLayoutCtrl * )
{
	return true;
}

void SamReportLossObject::SetCaseName( const wxString &c )
{
	SamReportObject::SetCaseName( c );
	UpdateImage();
}

void SamReportLossObject::UpdateImage()
{
	if ( Case *c = GetCase() )
	{
		SAMLossDiagram *ctrl = new SAMLossDiagram( app_mdiframe, c );
		wxBitmap bmp( 640, 800 );
		wxMemoryDC memdc( bmp );
		wxRect rect = wxRect(0,0,bmp.GetWidth(), bmp.GetHeight());
		ctrl->UpdateAll ( memdc, rect);
		ctrl->Destroy();
		m_imgCache = bmp.ConvertToImage();
	}
	else
		m_imgCache.Destroy();
}
void SamReportLossObject::Render( wxPageOutputDevice &dv )
{
	if (m_imgCache.IsOk())
	{
		dv.Image( m_imgCache , m_x, m_y, m_width, m_height );
	}
	else
	{
		dv.Color( *wxRED );
		dv.Rect( m_x, m_y, m_width, m_height );
		dv.Line( m_x, m_y, m_x+m_width, m_y+m_height );
		dv.Line( m_x+m_width, m_y, m_x, m_y+m_height );
		dv.Color( *wxBLACK );
		dv.Font( wxPageOutputDevice::SERIF, 10, true, false );
		dv.Text( m_x, m_y, "SAM Loss Object - only valid when a case is specified." );
	}
}

bool SamReportLossObject::ReadData( wxInputStream &is )
{
	return true;
}

bool SamReportLossObject::WriteData( wxOutputStream &os )
{
	return true;
}
*/






wxPageObject *SamReportTableObject::Duplicate()
{
	SamReportTableObject *obj = new SamReportTableObject;
	obj->Copy( this );
	return obj;
}

bool SamReportTableObject::Copy( wxPageObject *obj )
{
	if (SamReportTableObject *src = dynamic_cast<SamReportTableObject*>(obj))
	{
		m_table = src->m_table;
		return true;
	}
	else return false;	
}


enum { IDTOED_ROWS = 3529, IDTOED_COLS, IDTOED_GRID, IDTOED_INSERTVAR };
class SamReportTableObjectEditDialog : public wxDialog
{
private:
	SamReportTableObject *m_tableObject;
	wxPageLayoutCtrl *m_layoutCtrl;
	wxExtGridCtrl *m_grid;
	wxNumericCtrl *m_numRows, *m_numCols;
public:
	SamReportTableObjectEditDialog( wxWindow *parent ) 
		: wxDialog( parent, wxID_ANY, "Edit Table Object",		
			wxDefaultPosition, wxDefaultSize, 
			wxCAPTION|wxRESIZE_BORDER|wxCLOSE_BOX|wxSYSTEM_MENU),
		m_tableObject(0),
		m_layoutCtrl(0)
	{
		SetEscapeId( wxID_CANCEL );
		SetMinSize( wxScaleSize(520, 470) );
		m_grid = new wxExtGridCtrl( this, IDTOED_GRID );
		m_grid->CreateGrid(2,2);
		wxSizer *buttons = CreateButtonSizer( wxOK|wxCANCEL );

		buttons->AddStretchSpacer();
		buttons->Add( new wxStaticText(this, wxID_ANY, "   Rows:"), 0, wxALL|wxEXPAND, 3 );
		buttons->Add( (m_numRows=new wxNumericCtrl(this, IDTOED_ROWS, 0, wxNUMERIC_INTEGER)), 0, wxALL|wxEXPAND, 3 );
		buttons->Add( new wxStaticText(this, wxID_ANY, "   Cols:"), 0, wxALL|wxEXPAND, 3 );
		buttons->Add( (m_numCols=new wxNumericCtrl(this, IDTOED_COLS, 0, wxNUMERIC_INTEGER)), 0, wxALL|wxEXPAND, 3 );
		buttons->Add( new wxButton(this, IDTOED_INSERTVAR, "Insert variable..."), 0, wxALL|wxEXPAND, 3);

		wxBoxSizer *vert = new wxBoxSizer(wxVERTICAL);
		vert->Add( m_grid, 1, wxALL|wxEXPAND, 5 );
		vert->Add( buttons, 0, wxALL|wxEXPAND, 5 );
		SetSizer( vert );
		Fit();
	}

	void SetData( SamReportTableObject *o, wxPageLayoutCtrl *lay = 0 )
	{
		m_layoutCtrl = lay;
		m_tableObject = o;

		int nr = o->m_table.nrows();
		int nc = o->m_table.ncols();
		m_numRows->SetValue( nr );
		m_numCols->SetValue( nc );
		m_grid->ResizeGrid( nr, nc );
		for (int r=0;r<nr;r++)
			for (int c=0;c<nc;c++)
				m_grid->SetCellValue( r, c, o->m_table.at(r,c) );

		m_grid->AutoSize();
	}
	
	void GetData( SamReportTableObject *o )
	{
		int nr = m_grid->GetNumberRows();
		int nc = m_grid->GetNumberCols();

		o->m_table.resize_fill(nr,nc, wxEmptyString);
		for (int r=0;r<nr;r++)
			for (int c=0;c<nc;c++)
				o->m_table.at(r,c) = m_grid->GetCellValue(r,c);
	}

	void OnRows( wxCommandEvent & )
	{
		int nr = m_numRows->AsInteger();
		if (nr < 1)
		{
			nr = 1;
			m_numRows->SetValue(nr);
		}
		m_grid->ResizeGrid( nr, m_grid->GetNumberCols() );
		UpdateDisplay();
	}

	void OnCols( wxCommandEvent & )
	{
		int nc = m_numCols->AsInteger();
		if (nc < 1)
		{
			nc = 1;
			m_numCols->SetValue(nc);
		}
		m_grid->ResizeGrid( m_grid->GetNumberRows(), nc );
		UpdateDisplay();
	}

	void OnGrid( wxGridEvent & )
	{
		UpdateDisplay();
	}

	void UpdateDisplay()
	{
		if ( m_layoutCtrl != 0 && m_tableObject != 0 )
		{
			GetData( m_tableObject );
			m_layoutCtrl->Invalidate( m_tableObject );
		}
	}

	void OnInsertVar( wxCommandEvent & )
	{
		wxString text = InsertVariable( this );
		if (!text.IsEmpty())
		{
			int r = m_grid->GetGridCursorRow();
			int c = m_grid->GetGridCursorCol();
			if ( r >= 0 && r < m_grid->GetNumberRows()
				&& c >= 0 && c < m_grid->GetNumberCols() )
			{
				m_grid->SetCellValue( r, c, text );
				UpdateDisplay();
			}
		}

	}

	DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE( SamReportTableObjectEditDialog, wxDialog )
EVT_NUMERIC( IDTOED_ROWS, SamReportTableObjectEditDialog::OnRows )
EVT_NUMERIC( IDTOED_COLS, SamReportTableObjectEditDialog::OnCols )
EVT_BUTTON( IDTOED_INSERTVAR, SamReportTableObjectEditDialog::OnInsertVar )
EVT_GRID_CMD_CELL_CHANGED( IDTOED_GRID, SamReportTableObjectEditDialog::OnGrid )
END_EVENT_TABLE()

bool SamReportTableObject::EditObject( wxPageLayoutCtrl *layout )
{

	matrix_t<wxString> copy = m_table;
	SamReportTableObjectEditDialog dlg( 0 );
	dlg.SetData( this, layout );
	if (dlg.ShowModal() == wxID_OK)
	{
		dlg.GetData(this);
		return true;
	}
	else
	{
		m_table = copy;
		return false;
	}
}

void SamReportTableObject::Render( wxPageOutputDevice &dv )
{
	float row_height = 14.0f/72.0f;
	dv.Font( wxPageOutputDevice::SANSERIF, 12, false, false );

	std::vector<float> col_widths( m_table.ncols() );

	matrix_t<wxString> T( m_table.nrows(), m_table.ncols(), wxEmptyString );
	for (int r=0;r<(int)T.nrows();r++)
		for (int c=0;c<(int)T.ncols();c++)
			T.at(r,c) = SamReportEscapeString( m_table.at(r,c), GetCase(), GetMetaData() );

	for (int c=0;c<(int)m_table.ncols();c++)
	{
		col_widths[c] = 0;
		for (int r=0;r<(int)m_table.nrows();r++)
		{
			float width;
			dv.Measure( T.at(r,c), &width, 0 );
			if (width > col_widths[c])
				col_widths[c] = width;
		}

		col_widths[c] *= 1.2f;
	}

	dv.Color( *wxBLACK );
	float ypos = m_y;
	for (int r=0;r<(int)m_table.nrows();r++)
	{
		dv.Line( m_x, ypos, m_x+m_width, ypos );
		float xpos = m_x;
		for (int c=0;c<(int)m_table.ncols();c++)
		{
			dv.Text( xpos + 0.07f*col_widths[c], ypos + row_height*0.07f, T.at(r,c) );
			xpos += col_widths[c];
			if (xpos > m_x + m_width) break;
		}

		ypos += row_height*1.15f;
		if (ypos > m_y + m_height) break;
	}

	float xpos = m_x;
	for (int c=0;c<(int)m_table.ncols();c++)
	{
		dv.Line( xpos, m_y, xpos, m_y + m_height );
		xpos += col_widths[c];
	}

	dv.Rect( m_x, m_y, m_width, m_height );
}

bool SamReportTableObject::ReadData( wxInputStream &is )
{
	wxDataInputStream in(is);
	unsigned short code = in.Read16();
	in.Read8(); // unsigned char ver 
	size_t nrows = in.Read32();
	size_t ncols = in.Read32();
	m_table.resize_fill( nrows, ncols, wxEmptyString );
	
	for (int r=0;r<(int)m_table.nrows();r++)
		for (int c=0;c<(int)m_table.ncols();c++)
			m_table.at(r,c) = in.ReadString();

	return ( code == in.Read16() );
}

bool SamReportTableObject::WriteData( wxOutputStream &os )
{
	wxDataOutputStream out(os);
	out.Write16( 0xef );
	out.Write8( 1 ); // version
	out.Write32( m_table.nrows() );
	out.Write32( m_table.ncols() );
	for (int r=0;r<(int)m_table.nrows();r++)
		for (int c=0;c<(int)m_table.ncols();c++)
			out.WriteString( m_table.at(r,c) );
	out.Write16( 0xef );
	return true;
}

/* ***************************************
	Script object
 *************************************** */

#include <lk/absyn.h>
#include <lk/env.h>
#include <lk/eval.h>
#include <lk/lex.h>
#include <lk/parse.h>
#include <lk/stdlib.h>


SamReportScriptObject::SamReportScriptObject()
{
	m_script = "outln('script output rendered here...');\n";
}

SamReportScriptObject::~SamReportScriptObject()
{
	/* nothing to do */
}

wxPageObject *SamReportScriptObject::Duplicate()
{
	SamReportScriptObject *obj = new SamReportScriptObject;
	obj->Copy( this );
	return obj;
}

bool SamReportScriptObject::Copy( wxPageObject *obj )
{
	if (SamReportScriptObject *rhs = dynamic_cast<SamReportScriptObject*>(obj))
	{
		m_script = rhs->m_script;
		return true;
	}
	else return false;
}


static void fcall_out( lk::invoke_t &cxt )
{
	LK_DOC("out", "Output text data to the page renderer.", "(...):none");
	SamReportScriptObject *so = (SamReportScriptObject*)cxt.user_data();
	if (!so) return;
	
	wxString dat;
	for (size_t i=0;i<cxt.arg_count();i++)
		dat += cxt.arg(i).as_string();

	so->RenderText( dat );
}

static void fcall_outln( lk::invoke_t &cxt )
{
	LK_DOC("outln", "Output text data to the page renderer, followed by a newline.", "(...):none");
	SamReportScriptObject *so = (SamReportScriptObject*)cxt.user_data();
	if (!so) return;
	
	wxString dat;
	for (size_t i=0;i<cxt.arg_count();i++)
		dat += cxt.arg(i).as_string();

	dat += "\n";
	so->RenderText( dat );
}

static void fcall_image( lk::invoke_t &cxt )
{
	LK_DOC( "image", "Output an image from a file to the page render.", "(string:file, [number:width, number:height]):none");
	SamReportScriptObject *so = (SamReportScriptObject*)cxt.user_data();
	if (!so) return;

	float width = -1, height = -1;

	if ( cxt.arg_count() > 1 ) width = (float) cxt.arg(1).as_number();
	if ( cxt.arg_count() > 2 ) height = (float) cxt.arg(2).as_number();

	wxString file( cxt.arg(0).as_string() );

	wxImage image;
	if( image.LoadFile( file ) )
	{
		if ( width < 0 ) width = image.GetWidth() / 72.0f;
		if ( height < 0 ) height = image.GetHeight() / 72.0f;
		so->RenderImage( image, width, height );
	}
}

static void fcall_table( lk::invoke_t &cxt )
{
	LK_DOC("table", "Render a table from the given 2x2 matrix of cells", "(matrix:cell data):none");
	SamReportScriptObject *so = (SamReportScriptObject*)cxt.user_data();
	if (!so) return;

	size_t nrows = cxt.arg(0).length();
	size_t ncols = 0;
	for (int r=0;r<(int)nrows;r++)
	{
		size_t len = cxt.arg(0).index(r)->length();
		if (len > ncols) ncols = len;
	}

	if (nrows > 0 && ncols > 0)
	{
		matrix_t<wxString> table(nrows, ncols, wxEmptyString);
		for (size_t r = 0;r<nrows;r++)
		{
			for (size_t c = 0;c<ncols;c++)
			{
				lk::vardata_t *row = cxt.arg(0).index(r);
				if (row->type() == lk::vardata_t::VECTOR
					&& c<row->length() )
					table.at(r,c) = row->index(c)->as_string();
			}
		}

		so->RenderTable( table );
	}
}

static void fcall_graph( lk::invoke_t &cxt )
{
	LK_DOC("graph", 
		"Render a bar graph from the given values. Options include xlabel, ylabel, title, show_values, width, height, decimals, color, show_yaxis_ticks.", 
		"(array:values [, array:labels, table:options]):none");
	SamReportScriptObject *so = (SamReportScriptObject*)cxt.user_data();
	if (!so) return;

	std::vector<double> values;
	wxArrayString labels;
	wxString xlabel, ylabel, title, ticks_format = wxEmptyString;
	bool show_values = false;
	bool show_yaxis_ticks = false;
	float width = 4.0f;
	float height = 3.0f;
	int decimals = 2;
	wxColour color(183,200,228);

	if (cxt.arg(0).type() == lk::vardata_t::VECTOR)
	{
		lk::vardata_t &data = cxt.arg(0);	
		values.resize( data.length() );
		for (size_t i=0;i<data.length(); i++)
			values[i] = (double)data.index(i)->as_number();
	}
	else
	{
		values.resize(10);
		values[0] = -1.4;
		values[1] = 0.1;
		values[2] = 4.1;
		values[3] = 11.9;
		values[4] = 7.1;
		values[5] = 4.1;
		values[6] = 3.5;
		values[7] = 3.1;
		values[8] = 2.9;
		values[9] = 2.8;
	}

	if ( cxt.arg_count() > 1 && cxt.arg(1).type() == lk::vardata_t::VECTOR )
	{
		lk::vardata_t &v = cxt.arg(1).deref();
		for (size_t i=0;i<v.length(); i++)
			labels.Add( v.index(i)->as_string() );
	}
	
	if ( cxt.arg_count() > 2 && cxt.arg(2).type() == lk::vardata_t::HASH )
	{
		lk::vardata_t &v = cxt.arg(2);
		lk::vardata_t *vv = 0;
		if ((vv=v.lookup("xlabel")))
			xlabel = vv->as_string();

		if ((vv=v.lookup("ylabel")))
			ylabel = vv->as_string();

		if ((vv=v.lookup("title")))
			title = vv->as_string();

		if ((vv = v.lookup("show_values")))
			show_values = vv->as_boolean();

		if ((vv = v.lookup("show_yaxis_ticks")))
			show_yaxis_ticks = vv->as_boolean();

		if ((vv = v.lookup("ticks_format")))
			ticks_format = vv->as_string();

		if ((vv=v.lookup("width")))
			width = vv->as_number();

		if ((vv=v.lookup("height")))
			height = vv->as_number();

		if ((vv=v.lookup("decimals")))
			decimals = vv->deref().as_integer();

		if ((vv=v.lookup("color")))
		{
			lk::vardata_t &vv2 = vv->deref();
			if (vv2.type() == lk::vardata_t::VECTOR 
				&& vv2.length() == 3)
			{
				color.Set(
					(wxColourBase::ChannelType)vv2.index(0)->as_integer(),
					(wxColourBase::ChannelType)vv2.index(1)->as_integer(),
					(wxColourBase::ChannelType)vv2.index(2)->as_integer() );
			}
			else
				color.Set( vv2.as_string() );
		}
	}

	so->RenderBarGraph( values, labels, 
		xlabel, ylabel, title, 
		show_values, width, height, 
		decimals, color, show_yaxis_ticks, ticks_format );
}

static void fcall_move_to( lk::invoke_t &cxt )
{
	LK_DOC("move_to", "Moves the current output cursor to the x y position specified in inches.", "(real:x, real:y):none");
	SamReportScriptObject *so = (SamReportScriptObject*)cxt.user_data();
	if (!so) return;
	so->MoveTo( (float)cxt.arg(0).as_number(), 
		(float)cxt.arg(1).as_number() );
}

static void fcall_line_to( lk::invoke_t &cxt )
{
	LK_DOC("line_to", "Draw a line from the current position to the new coordinates specified in inches using the current style.  Updates the current position also.", "(real:x, real:y):none");
	SamReportScriptObject *so = (SamReportScriptObject*)cxt.user_data();
	if (!so) return;

	so->LineTo( (float)cxt.arg(0).as_number(), (float)cxt.arg(1).as_number() );
}

static void fcall_cursor( lk::invoke_t &cxt )
{
	LK_DOC("cursor", "Retrieves the current x y position of the output cursor.", "(none):table");
	SamReportScriptObject *so = (SamReportScriptObject*)cxt.user_data();
	if (!so) return;

	float x=0, y=0;
	so->GetCursorPos( &x, &y );
	cxt.result().empty_hash();
	cxt.result().hash_item( "x", (double)x );
	cxt.result().hash_item( "y", (double)y );
}

static void fcall_measure( lk::invoke_t &cxt )
{
	LK_DOC("measure", "Calculates the width and height in inches of a piece of text in the current font.", "(string:text):table");
	SamReportScriptObject *so = (SamReportScriptObject*)cxt.user_data();
	if (!so) return;

	float x=0, y=0;
	so->Measure(cxt.arg(0).as_string(), &x, &y );
	cxt.result().empty_hash();
	cxt.result().hash_item( "width", (double)x );
	cxt.result().hash_item( "height", (double)y );
}

static void fcall_style( lk::invoke_t &cxt )
{
	LK_DOC("style", "Change the current formatting style. Faces: fixed,sanserif,serif Align:left,center,right Line Styles:solid,dotted", 
		"(table:options including face, size, bold, italic, align, color, line_style, and line_width):none");
	
	SamReportScriptObject *so = (SamReportScriptObject*)cxt.user_data();
	if (!so) return;

	/* text properties */
	int face = wxPageOutputDevice::FIXED, size=12, align=wxLEFT;
	bool bold=false, ital=false;
	wxColour colour = *wxBLACK;
	float line_width = 0.013f;
	int line_style = wxPageOutputDevice::SOLID;

	so->Style( &face, &size, &colour, &bold, &ital, &align, &line_width, &line_style );

	/* table properties */
    int hdrSize, hdrFace, hdrAlign, hdrLines;
	bool hdrBold;
	wxColour hdrColour;
	int cellAlign;
	bool gridLines, hdrLine, tabBorder;
	std::vector<float> rowSizes, colSizes;
    std::vector<int> bldLines;
    std::vector<int> totalLines;

	so->TableStyle( &hdrSize, &hdrLines, &hdrFace, &hdrAlign, &hdrBold, &hdrColour,
		&hdrLine, &cellAlign, &gridLines, &rowSizes, &colSizes, &bldLines, &totalLines, &tabBorder );

	lk::vardata_t &tab = cxt.arg(0);
	lk::vardata_t *v = 0;

	if ( (v=tab.lookup("face")) )
	{
		lk_string fs = v->as_string();
		fs.Lower();
		if (fs == "fixed") face = wxPageOutputDevice::FIXED;
		else if (fs == "sanserif") face = wxPageOutputDevice::SANSERIF;
		else if (fs == "serif") face = wxPageOutputDevice::SERIF;
		else throw lk::error_t("invalid font face specification.  allowed are 'fixed', 'serif', 'sanserif'");
	}

	if ((v=tab.lookup("size")))
	{
		size = v->as_integer();
		if (size < 1) size = 1;
		if (size > 300) size = 300;
	}

	if ((v=tab.lookup("bold")))
		bold = v->as_boolean();

	if ((v=tab.lookup("italic")))
		ital = v->as_boolean();

	if ((v=tab.lookup("align")))
	{
		lk_string al = v->as_string();
		al.Lower();
		if (al == "left") align = wxLEFT;
		else if (al == "right") align = wxRIGHT;
		else if (al == "center") align = wxCENTER;
		else throw lk::error_t("invalid alignment specification.  allowed are 'left', 'center', 'right'");
	}
	
	if ((v=tab.lookup("color")))
	{
		lk::vardata_t &vv2 = v->deref();
		if (vv2.type() == lk::vardata_t::VECTOR 
			&& vv2.length() == 3)
		{
			colour.Set(
				(wxColourBase::ChannelType)vv2.index(0)->as_integer(),
				(wxColourBase::ChannelType)vv2.index(1)->as_integer(),
				(wxColourBase::ChannelType)vv2.index(2)->as_integer() );
		}
		else
			colour.Set( vv2.as_string() );
	}

	if ((v=tab.lookup("line_width")))
	{
		line_width = (float)v->as_number();
		if (line_width < 0.01f) line_width = 0.01f;
		if (line_width > 10) line_width = 10.0f;
	}

	if ((v=tab.lookup("line_style")))
	{
		lk_string ssty = v->as_string();
		ssty.Lower();
		if (ssty == "solid") line_style = wxPageOutputDevice::SOLID;
		else if (ssty == "dotted") line_style = wxPageOutputDevice::DOTTED;
		else throw lk::error_t("invalid line style specification.  allowed are 'solid', 'dotted'");
	}

	so->Style( face, size, colour, bold, ital, align, line_width, line_style );


	if ((v=tab.lookup("header_size")))
	{
		hdrSize = v->as_integer();
		if (hdrSize < 1) hdrSize = 1;
		if (hdrSize > 300) hdrSize = 300;
	}

    if ((v = tab.lookup("header_lines")))
    {
        hdrLines = v->as_integer();
        if (hdrLines < 1) hdrLines = 1;
        if (hdrLines > 10) hdrLines = 10;
    }

    if ((v = tab.lookup("bold_lines")))
    {
        lk::vardata_t& vv2 = v->deref();
        if (vv2.type() == lk::vardata_t::VECTOR)
        {
            bldLines.resize(vv2.length());
            for (size_t i = 0; i < vv2.length(); i++)
                bldLines[i] = (float)vv2.index(i)->as_number();
        }

    }

	if ((v=tab.lookup("header_face")))
	{	
		lk_string fs = v->as_string();
		fs.Lower();
		if (fs == "fixed") hdrFace = wxPageOutputDevice::FIXED;
		else if (fs == "sanserif") hdrFace = wxPageOutputDevice::SANSERIF;
		else if (fs == "serif") hdrFace = wxPageOutputDevice::SERIF;
		else throw lk::error_t("invalid font face specification.  allowed are 'fixed', 'serif', 'sanserif'");
	}

	if ((v=tab.lookup("header_align")))
	{
		lk_string al = v->as_string();
		al.Lower();
		if (al == "left") hdrAlign = wxLEFT;
		else if (al == "right") hdrAlign = wxRIGHT;
		else if (al == "center") hdrAlign = wxCENTER;
		else throw lk::error_t("invalid alignment specification.  allowed are 'left', 'center', 'right'");
	}

	if ((v=tab.lookup("header_bold")))
		hdrBold = v->as_boolean();

	
	if ((v=tab.lookup("header_color")))
	{
		lk::vardata_t &vv2 = v->deref();
		if ( vv2.type() == lk::vardata_t::VECTOR 
			&& vv2.length() == 3)
		{
			hdrColour.Set(
				(wxColourBase::ChannelType)vv2.index(0)->as_integer(),
				(wxColourBase::ChannelType)vv2.index(1)->as_integer(),
				(wxColourBase::ChannelType)vv2.index(2)->as_integer() );
		}
		else
			hdrColour.Set( vv2.as_string() );
	}

	if ((v=tab.lookup("cell_align")))
	{
		lk_string al = v->as_string();
		al.Lower();
		if (al == "left") cellAlign = wxLEFT;
		else if (al == "right") cellAlign = wxRIGHT;
		else if (al == "center") cellAlign = wxCENTER;
		else throw lk::error_t("invalid alignment specification.  allowed are 'left', 'center', 'right'");

	}

	if ((v=tab.lookup("grid_lines")))
		gridLines = v->as_boolean();

	if ((v=tab.lookup("header_line")))
		hdrLine = v->as_boolean();

    if ((v = tab.lookup("total_lines"))) {
        lk::vardata_t& vv2 = v->deref();
        if (vv2.type() == lk::vardata_t::VECTOR)
        {
            totalLines.resize(vv2.length());
            for (size_t i = 0; i < vv2.length(); i++)
                totalLines[i] = (float)vv2.index(i)->as_number();
        }
    }

	if ((v=tab.lookup("col_sizes")))
	{
		lk::vardata_t &vv2 = v->deref();
		if (vv2.type() == lk::vardata_t::VECTOR)
		{
			colSizes.resize( vv2.length() );
			for (size_t i=0;i<vv2.length();i++)
				colSizes[i] = (float)vv2.index(i)->as_number();
		}
	}

	if ((v=tab.lookup("row_sizes")))
	{
		lk::vardata_t &vv2 = v->deref();
		if (vv2.type() == lk::vardata_t::VECTOR)
		{
			rowSizes.resize( vv2.length() );
			for (size_t i=0;i<vv2.length(); i++)
				rowSizes[i] = (float)vv2.index(i)->as_number();
		}
	}

	if ((v=tab.lookup("table_border")))
		tabBorder = v->as_boolean();

	so->TableStyle( hdrSize, hdrLines, hdrFace, hdrAlign, hdrBold,
		hdrColour, hdrLine, cellAlign, gridLines, rowSizes, colSizes, bldLines, totalLines,
		tabBorder);
}

static void fcall_var( lk::invoke_t &cxt )
{
	LK_DOC("var", "Retrieve a variable value from the current SAM case.", "(string:name):value");
	
	wxString var_name( cxt.arg(0).as_string() );

	SamReportScriptObject *so = (SamReportScriptObject*) cxt.user_data();
	if (!so) return;
	
	VarTable *vt = so->GetSymbols();
	if (!vt) return;
	
	VarValue *meta = so->GetMetaData();

	VarValue *vv = vt->Get( var_name );
	if ( !vv  && meta && meta->Type() == VV_TABLE )
		vv = meta->Table().Get( var_name );

	if ( !vv )
	{
		Simulation &sim = so->GetCase()->BaseCase();
		vv = sim.GetOutput( var_name );
	}

	if ( !vv )
		return;

	vv->Write( cxt.result() );
}

static void fcall_technology( lk::invoke_t &cxt )
{
	LK_DOC("technology", "Returns the current case's technology", "(none):string");
	SamReportScriptObject *so = (SamReportScriptObject*) cxt.user_data();
	if (!so) return;
	if ( Case *c = so->GetCase() )
		if ( ConfigInfo *ci = c->GetConfiguration() )
			cxt.result().assign( ci->Technology );
}

static void fcall_financing( lk::invoke_t &cxt )
{
	LK_DOC("financing", "Returns the current case's financing", "(none):string");
	SamReportScriptObject *so = (SamReportScriptObject*) cxt.user_data();
	if (!so) return;
	if ( Case *c = so->GetCase() )
		if ( ConfigInfo *ci = c->GetConfiguration() )
			cxt.result().assign( ci->Financing );
}
	
VarTable *SamReportScriptObject::GetSymbols()
{
	if (GetCase()) return & GetCase()->Values();
	else return 0;
}


enum { IDT_SCRIPT=1495, IDT_BOLD, IDT_ITALIC, IDT_FACE, IDT_SIZE, 
	IDT_COLOUR, IDT_SYMBOL, IDT_ALIGN, IDT_INSERTVAR, IDT_FIND, IDT_HELP,
	IDT_OUTPUT, IDT_TIMER };

static lk::fcall_t report_script_funcs[] = {
	fcall_out, fcall_outln, fcall_style, fcall_move_to, fcall_line_to, fcall_cursor,
	fcall_measure, fcall_image, fcall_table, fcall_graph, fcall_var, fcall_technology, fcall_financing,
	0 };

class SamScriptObjectEditDialog : public wxDialog
{
private:
	wxTimer m_timer;

	wxBoxSizer *m_toolSizer;
	wxLKScriptCtrl *m_editor;

	wxPageLayoutCtrl *m_layoutCtrl;
	SamReportScriptObject *m_scriptObject;

public:
	SamScriptObjectEditDialog( wxWindow *parent )
		: wxDialog( parent, wxID_ANY, wxString("Edit Script Object"),
			wxDefaultPosition, wxScaleSize(600, 900), 
			wxCAPTION|wxRESIZE_BORDER|wxCLOSE_BOX|wxSYSTEM_MENU),
			m_timer( this, IDT_TIMER )
	{
		m_scriptObject = 0;
		m_layoutCtrl = 0;

		SetEscapeId( wxID_CANCEL );
			
		m_editor = new wxLKScriptCtrl(this, IDT_SCRIPT, wxDefaultPosition, wxDefaultSize, 
			wxLK_STDLIB_BASIC|wxLK_STDLIB_SYSIO|wxLK_STDLIB_STRING|wxLK_STDLIB_MATH|wxLK_STDLIB_WXUI);
		
		m_editor->RegisterLibrary(report_script_funcs);

		m_toolSizer = new wxBoxSizer( wxHORIZONTAL );
		m_toolSizer->Add( new wxButton( this, IDT_INSERTVAR, "Variables", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT ), 0, wxRIGHT|wxALIGN_CENTER, 4 );
		m_toolSizer->Add( new wxButton( this, IDT_SYMBOL, "@", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT ), 0, wxRIGHT|wxALIGN_CENTER, 4 );
		m_toolSizer->Add( new wxButton( this, IDT_FIND, "Find", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT ), 0, wxRIGHT|wxALIGN_CENTER, 4 );
		m_toolSizer->Add( new wxButton( this, IDT_HELP, "Help", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT ), 0, wxRIGHT|wxALIGN_CENTER, 4 );

		wxBoxSizer *lay = new wxBoxSizer( wxVERTICAL );
		lay->Add( m_toolSizer, 0, wxEXPAND|wxALL, 4 );
		lay->Add( m_editor, 1, wxEXPAND|wxALL, 0 );
		lay->Add( CreateButtonSizer( wxOK|wxCANCEL ) , 0, wxEXPAND|wxALL, 4 );
		SetSizer(lay);
	}

	void SetData( SamReportScriptObject *obj, wxPageLayoutCtrl *layout )
	{
		m_editor->SetText( obj->GetScript() );
		m_editor->SetFocus();
	
		m_layoutCtrl = layout;
		m_scriptObject = obj;
	}

	void GetData( SamReportScriptObject *obj )
	{
		obj->SetScript( m_editor->GetText() );
	}

	void InvalidateObject()
	{
		if ( m_layoutCtrl != 0 && m_scriptObject != 0 )
		{
			GetData( m_scriptObject );
			m_layoutCtrl->Invalidate( m_scriptObject );
		}	
	}

	void OnChange( wxCommandEvent & )
	{
		InvalidateObject();
	}
		
	void OnInsertVar( wxCommandEvent & )
	{
		wxString text = InsertVariable(this, false);
		if (!text.IsEmpty())
			m_editor->InsertText( m_editor->GetCurrentPos(), text );
	}

	void OnSpin( wxSpinEvent & ) { InvalidateObject(); }
	void OnColour( wxColourPickerEvent & ) { InvalidateObject(); }
	
	void OnScriptChanged( wxStyledTextEvent & )
	{
		m_timer.Start( 700, true );
	}

	void OnTimer( wxTimerEvent & )
	{
		wxString output;
		lk::input_string p( m_editor->GetText() );
		lk::parser parse( p );	
		lk::node_t *tree = parse.script();
		if ( parse.error_count() == 0 
			&& parse.token() == lk::lexer::END)
		{
			InvalidateObject();
			output += "no syntax errors";
		}
		else
		{	
			int i=0;
			while ( i < parse.error_count() )
				output += parse.error(i++);
		}

		if (parse.token() != lk::lexer::END)
			output += "parsing did not reach end of input";

		if (tree) delete tree;
	}
	
	void OnSymbol( wxCommandEvent &e )
	{
		wxSymbolPickerDialog dlg( "*", wxEmptyString, wxEmptyString, this );
		if (dlg.ShowModal() == wxID_OK)
		{
			unsigned long ch = (unsigned long)dlg.GetSymbolChar();
			m_editor->WriteText( wxString::Format("\\u%04x", ch ) );
			OnChange(e);
		}
	}

	void OnFind( wxCommandEvent & )
	{
		m_editor->ShowFindReplaceDialog();
	}

	void OnHelp( wxCommandEvent & )
	{
		m_editor->ShowHelpDialog();
	}

	DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE( SamScriptObjectEditDialog, wxDialog )

	EVT_STC_CHANGE( IDT_SCRIPT,SamScriptObjectEditDialog::OnScriptChanged )	
	EVT_BUTTON( IDT_INSERTVAR, SamScriptObjectEditDialog::OnInsertVar )
	EVT_BUTTON( IDT_SYMBOL, SamScriptObjectEditDialog::OnSymbol )
	EVT_BUTTON( IDT_FIND, SamScriptObjectEditDialog::OnFind )
	EVT_BUTTON( IDT_HELP, SamScriptObjectEditDialog::OnHelp )
	EVT_TIMER( IDT_TIMER, SamScriptObjectEditDialog::OnTimer )
END_EVENT_TABLE()

bool SamReportScriptObject::EditObject( wxPageLayoutCtrl *lay )
{
	SamScriptObjectEditDialog dlg( lay );
	dlg.SetData( this, lay );
	dlg.ShowModal();
	return true;
}


void SamReportScriptObject::Render( wxPageOutputDevice &dv )
{
	// setup rendering state variables
	m_curXPos = m_x;
	m_curYPos = m_y;
	m_curDevice = &dv;
	
	// initialize styles
	Style( wxPageOutputDevice::SANSERIF, 12, *wxBLACK, false, false, wxLEFT, 0.013f, wxPageOutputDevice::SOLID );
	TableStyle( 12, 1, wxPageOutputDevice::SANSERIF, wxCENTER, true, *wxBLACK, true, wxLEFT, false, std::vector<float>(), std::vector<float>(), std::vector<int>(), std::vector<int>(), true );

	// run the script.
	// callback functions invoke rendering capabilities and state/style changes

	wxString errors;
	lk::input_string p( m_script );
	lk::parser parse( p );
	
	lk::node_t *tree = parse.script();

	if ( parse.error_count() != 0 
		|| parse.token() != lk::lexer::END)
	{
		errors += "parsing did not reach end of input\n";
	}
	else
	{
		lk::env_t env;			
		env.register_funcs( report_script_funcs, this );
		env.register_funcs( lk::stdlib_basic() );
		env.register_funcs( lk::stdlib_sysio() );
		env.register_funcs( lk::stdlib_string() );
		env.register_funcs( lk::stdlib_math() );
		env.register_funcs( lk::stdlib_wxui() );

		wxStopWatch sw;
		lk::eval e( tree, &env );
		if ( !e.run() )
			for (size_t i=0;i<e.error_count();i++)
				errors += e.get_error(i) + "\n";
	}
			
	int i=0;
	while ( i < parse.error_count() )
		errors += parse.error(i++);

	if (tree) delete tree;

	// print any errors;

	if (!errors.IsEmpty())
	{
		dv.Color( *wxRED );
		dv.Font( wxPageOutputDevice::SANSERIF, 12, false, false );
		wxArrayString lines = wxStringTokenize( errors, "\n\r", ::wxTOKEN_RET_EMPTY_ALL );
		float line_height = 0.2f; // inches (approximate for 12pt)
		dv.Measure("H", 0, &line_height);
		float y = m_curYPos;
		for (size_t i=0;i<lines.size();i++)
		{
			lines[i].Replace("\t", "     ");
			dv.Text( m_x, y, lines[i] );
			y += line_height;
		}
	}
}

void SamReportScriptObject::Style( int face, int size, const wxColour &c, bool bold, bool ital, int align, float line_width, int line_style )
{
	if (!m_curDevice) return;

	m_curFace = face;
	m_curSize = size;
	m_curColour = c;
	m_curBold = bold;
	m_curItalic = ital;
	m_curAlign = align;

	m_curLineWidth = line_width;
	m_curLineStyle = line_style;

	m_curDevice->Color( m_curColour );
	m_curDevice->Font( face, size, bold, ital );
	
	m_curLineHeight = 0.2f;
	m_curDevice->Measure("hy", 0, &m_curLineHeight);
}

void SamReportScriptObject::Style( int *face, int *size, wxColour *c, bool *b, bool *it, int *al, float *line_width, int *line_style )
{
	*face = m_curFace;
	*size = m_curSize;
	*c = m_curColour;
	*b = m_curBold;
	*it = m_curItalic;
	*al = m_curAlign;	
	*line_width = m_curLineWidth;
	*line_style = m_curLineStyle;
}

void SamReportScriptObject::TableStyle( int hdrSize, int hdrLines, int hdrFace, int hdrAlign, bool hdrBold, const wxColour &hdrColor,
	bool hdrLine, int cellAlign, bool gridLines, const std::vector<float> &rowSizes, const std::vector<float> &colSizes, const std::vector<int> &bldLines, const std::vector<int> &totalLines,
	bool tabBorder )
{
	m_headerSize = hdrSize;
    m_headerLines = hdrLines;
	m_headerFace = hdrFace;
	m_headerAlign = hdrAlign;
	m_headerBold = hdrBold;
	m_headerColour = hdrColor;
	m_headerLine = hdrLine;
	m_cellAlign = cellAlign;
	m_gridLines = gridLines;
	m_rowSizes = rowSizes;
	m_colSizes = colSizes;
    m_bldLines = bldLines;
    m_totalLines = totalLines;
	m_tableBorder = tabBorder;
}

void SamReportScriptObject::TableStyle( int *hdrSize, int *hdrLines, int *hdrFace, int *hdrAlign, bool *hdrBold, wxColour *hdrColor,
	bool *hdrLine, int *cellAlign, bool *gridLines, std::vector<float> *rowSizes, std::vector<float> *colSizes, std::vector<int> *bldLines, std::vector<int> *totalLines,
	bool *tabBorder)
{
	*hdrSize = m_headerSize;
    *hdrLines = m_headerLines;
	*hdrFace = m_headerFace;
	*hdrAlign = m_headerAlign; 
	*hdrBold = m_headerBold;
	*hdrColor = m_headerColour;
	*hdrLine = m_headerLine;
	*cellAlign = m_cellAlign;
	*gridLines = m_gridLines;
	*rowSizes = m_rowSizes;
	*colSizes = m_colSizes;
    *bldLines = m_bldLines;
    *totalLines = m_totalLines;
	*tabBorder = m_tableBorder;
}

void SamReportScriptObject::MoveTo( float x, float y )
{
	m_curXPos = x + m_x;
	m_curYPos = y + m_y;
}

void SamReportScriptObject::LineTo( float x, float y )
{
	if (!m_curDevice) return;
	m_curDevice->LineStyle( m_curLineWidth, m_curLineStyle );
	m_curDevice->Line( m_curXPos, m_curYPos, x+m_x, y+m_y );
	m_curXPos = x+m_x;
	m_curYPos = y+m_y;
}

void SamReportScriptObject::GetCursorPos( float *x, float *y )
{
	*x = m_curXPos-m_x;
	*y = m_curYPos-m_y;
}

void SamReportScriptObject::Measure( const wxString &s, float *w, float *h )
{
	if (!m_curDevice)
	{
		*w = *h = 0;
		return;
	}
	m_curDevice->Measure( s, w, h );
}

void SamReportScriptObject::RenderText( const wxString &str )
{
	if (!m_curDevice) return;

	
	char cur_delim[2] = {0,0};
	wxString::size_type m_pos = 0;
	wxString token;
	
	while (m_pos < str.length())
	{
		std::string::size_type pos = str.find_first_of('\n', m_pos);
		if (pos == std::string::npos)
		{
			cur_delim[0] = 0;
			token.assign(str, m_pos, wxString::npos);
			m_pos = str.length();
		}
		else
		{
			cur_delim[0] = str[pos];
			std::string::size_type len = pos - m_pos;			
			token.assign(str, m_pos, len);
			m_pos = pos + 1;
		}
				
		// write text
		
		// advance to next line if alignment is changed and the current X
		// position is not at the left of the object
		if (m_curAlign != wxLEFT && m_curXPos != m_x )
		{
			m_curXPos = m_x;
			m_curYPos += m_curLineHeight;
		}

		if (!token.IsEmpty())
		{		
			float width;
			m_curDevice->Measure( token, &width, 0 );
			if (m_curAlign == wxLEFT) m_curDevice->Text( m_curXPos, m_curYPos, token );
			else
			{
				if ( m_curAlign == wxCENTER ) m_curDevice->Text( m_x + m_width/2 - width/2, m_curYPos, token ); // center
				else m_curDevice->Text( m_curXPos + m_width - width, m_curYPos, token ); // right
			}
		
			m_curXPos += width;
		}

		// move to next line if needed
		if ( cur_delim[0] != 0 )
		{
			m_curXPos = m_x;
			m_curYPos += m_curLineHeight;
		}
	}
}

void SamReportScriptObject::RenderImage( const wxImage &img, float width, float height )
{
	if ( !m_curDevice ) return;
	m_curDevice->Image( img, m_curXPos, m_curYPos, width, height );
	m_curXPos += width;
}

struct cellgeom
{
	float width, height;
};

void SamReportScriptObject::RenderTable( const matrix_t<wxString> &tab )
{
	if (!m_curDevice) return;
		
	float tab_x = m_curXPos;
	float tab_y = m_curYPos;
	float tab_width = 0.0f;
	float tab_height = 0.0f;

	matrix_t<cellgeom> cellsize( tab.nrows(), tab.ncols(), cellgeom() );
	for (int r=0;r<(int)tab.nrows();r++)
	{
		if (r < m_headerLines) m_curDevice->Font( m_headerFace, m_headerSize, m_headerBold, false );
		else if (r == 1) m_curDevice->Font( m_curFace, m_curSize, m_curBold, m_curItalic );
        for (int b = 0; b < m_bldLines.size(); b++) {
            if (r == m_bldLines[b]) {
                m_curDevice->Font(m_headerFace, m_headerSize, true, false);
                break;
            }
            else m_curDevice->Font(m_headerFace, m_headerSize, m_curBold, m_curItalic);
        }
		for (int c=0;c<(int)tab.ncols();c++)
		{
			float width, height;
			m_curDevice->Measure( tab.at(r,c), &width, &height );
			cellsize.at(r,c).width = width;
			cellsize.at(r,c).height = height;
		}
	}
	
	std::vector<float> col_widths( tab.ncols() );
	for (int c=0;c<(int)tab.ncols();c++)
	{
		col_widths[c] = 0;
		for (int r=0;r<(int)tab.nrows();r++)
			if (cellsize.at(r,c).width > col_widths[c])
				col_widths[c] = cellsize.at(r,c).width;

		col_widths[c] *= 1.2f;

		if ( c < (int)m_colSizes.size()
			&& m_colSizes[c] > 0)
			col_widths[c] = m_colSizes[c];

		tab_width += col_widths[c];
	}

	std::vector<float> row_heights( tab.nrows() );
	for (int r=0;r<(int)tab.nrows();r++)
	{
		row_heights[r] = 0;
		for (int c=0;c<(int)tab.ncols();c++)
			if (cellsize.at(r,c).height > row_heights[r])
				row_heights[r] = cellsize.at(r,c).height;

		row_heights[r] *= 1.2f;

		if ( r < (int)m_rowSizes.size()
			&& m_rowSizes[r] > 0)
			row_heights[r] = m_rowSizes[r];

		tab_height += row_heights[r];
	}

	if (m_curAlign == wxCENTER)
		tab_x = m_x + m_width/2 - tab_width/2;
	else if (m_curAlign == wxRIGHT)
		tab_x = m_x + m_width - tab_width;

	m_curDevice->LineStyle(); // clear line style to default.
	
	for (int r=0;r<(int)tab.nrows();r++)
	{
		if ( r < m_headerLines )
		{ // configure header text properties
			m_curDevice->Color( m_headerColour );
			m_curDevice->Font( m_headerFace, m_headerSize, m_headerBold, false );
		}
		else if ( r == 1 )
		{ // return to normal text style
			m_curDevice->Color( m_curColour );
			m_curDevice->Font( m_curFace, m_curSize, m_curBold, m_curItalic );
		}
        else {
            for (int b = 0; b < m_bldLines.size(); b++) {
                if (r == m_bldLines[b]) {
                    m_curDevice->Font(m_curFace, m_curSize, true, false);
                    break;
                }
                else m_curDevice->Font(m_curFace, m_curSize, m_curBold, m_curItalic);
            }
        }
        

		float xpos = tab_x;
		for (int c=0;c<(int)tab.ncols();c++)
		{
			int align = (r==0) ? m_headerAlign : m_cellAlign;
			switch( align )
			{
			case wxLEFT:
				m_curDevice->Text( xpos + 0.07f*col_widths[c], 
					m_curYPos + row_heights[r]*0.07f, tab.at(r,c) );
				break;
			case wxCENTER:
				m_curDevice->Text( xpos + col_widths[c]/2 - cellsize.at(r,c).width/2,
					m_curYPos + row_heights[r]*0.07f, tab.at(r,c) );
				break;
			case wxRIGHT:
				m_curDevice->Text( xpos + 0.93f*col_widths[c] - cellsize.at(r,c).width,
					m_curYPos + row_heights[r]*0.07f, tab.at(r,c) );
				break;
			}
			xpos += col_widths[c];
			if (xpos > m_x + m_width) break;
		}

		m_curYPos += row_heights[r];
		if (m_curYPos > m_y + m_height) break;
	}
	
	if ( m_gridLines )
	{
		m_curDevice->Color( *wxLIGHT_GREY );
		float xpos = tab_x;
		for (int c=0;c<(int)tab.ncols();c++)
		{
			if (c > 0) m_curDevice->Line( xpos, tab_y, xpos, tab_y + tab_height*0.99 );
			xpos += col_widths[c];
		}
	
		float ypos = tab_y;
		for (int r=0;r<(int)tab.nrows();r++)
		{
			if (r > 0) m_curDevice->Line( tab_x, ypos, tab_x+tab_width, ypos );
			ypos += row_heights[r];
		}
	}
	
	m_curDevice->Color( *wxBLACK );
	if (m_headerLine && row_heights.size() > 0)
		m_curDevice->Line( tab_x, tab_y+row_heights[0], 
			tab_x+tab_width, tab_y+row_heights[0] );
    float ypos_total_lines = tab_y+row_heights[0];
    for (int r = 1; r < (int)tab.nrows(); r++) {
        ypos_total_lines += row_heights[r];
        for (int b = 0; b < m_totalLines.size(); b++) {
            if (r == m_totalLines[b]) {
                m_curDevice->Line(tab_x, ypos_total_lines - row_heights[r], tab_x + tab_width, ypos_total_lines - row_heights[r]);
                m_curDevice->Line(tab_x, ypos_total_lines, tab_x + tab_width, ypos_total_lines);
                break;
            }
            
        }
    }

	if (m_tableBorder)
		m_curDevice->Rect( tab_x, tab_y, tab_width, tab_height );

	m_curDevice->Color( m_curColour ); // restore original color, return to normal text style
	m_curDevice->Font( m_curFace, m_curSize, m_curBold, m_curItalic );
}

struct fRect
{
	fRect() { x=y=width=height=0; }
	fRect( float _x, float _y, float _w, float _h ) : x(_x), y(_y), width(_w), height(_h) { }
	float x, y, width, height;
};

void SamReportScriptObject::RenderBarGraph( const std::vector<double> &values, const wxArrayString &xlabels, const wxString &xlabel,
	const wxString &ylabel, const wxString &title, bool show_values, float xsize, float ysize, int decimals,
	const wxColour &color, bool show_yaxis_ticks, const wxString &ticks_format )
{
	if (!m_curDevice) return;

	float gr_x = m_curXPos;
	float gr_y = m_curYPos;
	float gr_width = xsize;
	float gr_height = ysize;
	
	if (m_curAlign == wxCENTER)
		gr_x = m_x + m_width/2 - gr_width/2;
	else if (m_curAlign == wxRIGHT)
		gr_x = m_x + m_width - gr_width;

	double ymin = 0;
	double ymax = 0;

	for (int i=0;i<(int)values.size();i++)
	{
		if (values[i] < ymin) ymin = values[i];
		if (values[i] > ymax) ymax = values[i];
	}

	if (ymin == 0 && ymax == 0) ymax = 1;

	ymin *= 1.1;
	ymax *= 1.1;
	
	wxPLLinearAxis yaxis( ymin, ymax );
	double physmax = gr_height*72;

	std::vector<wxPLAxis::TickData> ticks;
	if (show_yaxis_ticks)
		yaxis.GetAxisTicks( -1, physmax, ticks );
	else
		yaxis.GetAxisTicks(0, physmax, ticks);

	// save the current style data
	int saveFace, saveSize, saveAlign;
	wxColour saveColour;
	bool saveBold, saveItalic;
	float saveWidth;
	int saveStyle;
	Style(&saveFace, &saveSize, &saveColour, &saveBold, &saveItalic, &saveAlign, &saveWidth, &saveStyle );

	m_curDevice->LineStyle(); // clear line style
	
	m_curDevice->Font( wxPageOutputDevice::SANSERIF, 12, true, false );
	float yl_width = 0.0f, yl_height = 0.0f;
	if ( !ylabel.IsEmpty() )
		m_curDevice->Measure(ylabel, &yl_width, &yl_height);

	
	float xl_width = 0.0f, xl_height = 0.0f;
	if ( !xlabel.IsEmpty() )
		m_curDevice->Measure(xlabel, &xl_width, &xl_height);


	float ti_width = 0.0f, ti_height = 0.0f;
	if ( !title.IsEmpty() )
		m_curDevice->Measure(title, &ti_width, &ti_height);

	// draw labels & title
	m_curDevice->Color( *wxBLACK );
	if ( !title.IsEmpty() )
		m_curDevice->Text( gr_x+gr_width/2-ti_width/2, gr_y, title, 0 );

	m_curDevice->Font( wxPageOutputDevice::SANSERIF, 12, false, false ); // turn off bold for axis labels
	if ( !ylabel.IsEmpty() )
		m_curDevice->Text( gr_x, gr_y+gr_height/2+yl_width/2, ylabel, 90 );

	if ( !xlabel.IsEmpty() )
		m_curDevice->Text( gr_x+gr_width/2-xl_width/2, gr_y+gr_height-xl_height, xlabel, 0 );

	// calculate max y tick width
	m_curDevice->Font( wxPageOutputDevice::SANSERIF, 10, false, false ); // font for tick labels
	 
	float max_ytick_width = 0;
	std::vector<float> ytick_widths;
	wxArrayString ytick_labels;
	float ytick_height = 0.1f;
	m_curDevice->Measure("yh", 0, &ytick_height);
	for (size_t i=0;i<ticks.size();i++)
	{
		if ( ticks[i].size != wxPLAxis::TickData::LARGE )
			continue;

		wxString label;
		if (ticks_format != wxEmptyString)
			label = lk::format((const char *)ticks_format.c_str(), ticks[i].world);
		else if (decimals <= 0 && fabs(ticks[i].world)>999)
			label = wxNumericFormat( ticks[i].world, wxNUMERIC_REAL, wxNUMERIC_GENERIC, true, wxEmptyString, wxEmptyString );
		else if (decimals < 6)
			label = wxNumericFormat( ticks[i].world, wxNUMERIC_REAL, decimals, true, wxEmptyString, wxEmptyString );		
		else
			label = wxString::Format("%lg", ticks[i].world);

		float tw = 0.05f;
		m_curDevice->Measure(label, &tw, 0);
		if (tw > max_ytick_width) max_ytick_width = tw;

		ytick_labels.Add( label );
		ytick_widths.push_back( tw );
	}

	float max_xtick_width = 0;
	std::vector<float> xtick_widths;
	wxArrayString xtick_labels;
	for (size_t i=0;i<values.size();i++)
	{		
		wxString label;
		if ( i < xlabels.size() ) label = xlabels[i];
		float tw = 0.05f;
		m_curDevice->Measure( label, &tw, 0 );
		if (tw > max_xtick_width) max_xtick_width = tw;

		xtick_labels.Add( label );
		xtick_widths.push_back( tw );
	}

	float max_yvaltext_width = 0;
	float max_yvaltext_height = 0;
	std::vector<float> yvaltext_widths;
	wxArrayString yvaltext_labels;
	if (show_values)
	{
		for (int i=0;i<(int)values.size();i++)
		{
			wxString label;
			if (decimals <= 0 && fabs(values[i])>999)
				label = wxNumericFormat( values[i], wxNUMERIC_REAL, wxNUMERIC_GENERIC, true, wxEmptyString, wxEmptyString );
			else if (decimals < 6)
				label = wxNumericFormat( values[i], wxNUMERIC_REAL, decimals, true, wxEmptyString, wxEmptyString );		
			else
				label = wxString::Format("%lg", values[i] );

			float tw = 0.05f;
			float th = 0.01f;
			m_curDevice->Measure( label, &tw, &th );
			if (tw > max_yvaltext_width) max_yvaltext_width = tw;
			if (th > max_yvaltext_height) max_yvaltext_height = th;

			yvaltext_widths.push_back( tw );
			yvaltext_labels.Add( label );
		}
	}

	float max_xtick_height = ytick_height;

	fRect gbox;
	gbox.x = 0.1f + (ylabel.IsEmpty() ? 0 : yl_height) + max_ytick_width + 0.05f;
	gbox.y = (title.IsEmpty() ? 0 : yl_height );
	gbox.width = gr_width-gbox.x;
		
	float xspacing = gbox.width * 0.95f / ( (float)values.size() );
	float bar_width = xspacing*0.85f;

	bool vertical_xtick_text = false;
	float height_reduction = 0;
	if (ymin==0) height_reduction = max_xtick_height;
	if ( max_xtick_width > xspacing )
	{
		height_reduction = max_xtick_width;
		vertical_xtick_text = true;
	}

	gbox.height = gr_height - (xlabel.IsEmpty() ? 0 : xl_height ) - (title.IsEmpty() ? 0 : ti_height) - height_reduction;

	bool vertical_yvaltext_text = false;
	if ( show_values && max_yvaltext_width > xspacing )
	{
		gbox.y += max_yvaltext_width;
		gbox.height -= max_yvaltext_width; // try to keep yval labels from overlapping graph title
		vertical_yvaltext_text = true;
	}
	else if ( show_values && max_yvaltext_height > 0 )
	{
		gbox.y += max_yvaltext_height/2;
		gbox.height -= max_yvaltext_height/2;
	}

	m_curDevice->Color( *wxBLACK );

	float sfy = (gbox.height)/(ymax-ymin); // y scale: world_to_device
	float ory = ymax*sfy;

#define TO_DEVICE(y) (-1.0f*(y)*sfy + ory)
#define LINEOUT( x1, y1, x2, y2 ) m_curDevice->Line( gr_x+gbox.x+(x1), gr_y+gbox.y+(y1), gr_x+gbox.x+(x2), gr_y+gbox.y+(y2) )
#define RECTOUT( x1, y1, ww, hh ) m_curDevice->Rect( gr_x+gbox.x+(x1), gr_y+gbox.y+(y1), ww, hh )
#define FILLOUT( x1, y1, ww, hh ) m_curDevice->Rect( gr_x+gbox.x+(x1), gr_y+gbox.y+(y1), ww, hh, true )
#define TEXTOUT( xx, yy, ss, aa ) m_curDevice->Text( gr_x+gbox.x+(xx), gr_y+gbox.y+(yy), (ss), aa )

	for ( size_t i=0;i<ticks.size(); i++)
	{
		if ( ticks[i].size == wxPLAxis::TickData::LARGE )
		{
			float y = TO_DEVICE(ticks[i].world);
			LINEOUT( 0, y, 0.1f, y );
			TEXTOUT( -ytick_widths[i]-0.025f, y-ytick_height/2, ytick_labels[i], 0 );
		}
	}

	for ( size_t i=0;i<ticks.size(); i++)
	{
		if ( ticks[i].size == wxPLAxis::TickData::SMALL )
		{
			float y = TO_DEVICE(ticks[i].world);
			LINEOUT( 0, y, 0.05f, y );
		}
	}

	m_curDevice->Color( color);
	for ( size_t i=0;i<values.size();i++ )
	{
		float x = xspacing*0.8f + xspacing*i;
		float y0 = TO_DEVICE( values[i] );
		float y1 = TO_DEVICE( 0 );		
		FILLOUT( x-bar_width/2, y1 < y0 ? y1 : y0, bar_width, (float)fabs(y1-y0) );		
	}

	m_curDevice->Color( *wxBLACK );
	float y0 = TO_DEVICE( 0 );		
	for ( size_t i=0;i<values.size();i++ )
	{
		float x = xspacing*0.8f + xspacing*i;
		LINEOUT( x, y0, x, y0-0.07f );		
		if ( vertical_xtick_text ) TEXTOUT( x-max_xtick_height/2, y0+xtick_widths[i] + 0.025f, xtick_labels[i], 90);
		else TEXTOUT( x-xtick_widths[i]/2, y0 + 0.025f, xtick_labels[i], 0 );

		if (show_values)
		{
			float yy = TO_DEVICE( values[i] );
			if (vertical_yvaltext_text)
				TEXTOUT( x-max_xtick_height/2, values[i] > 0 ? yy - 0.025f : yy+yvaltext_widths[i] + 0.025f, yvaltext_labels[i], 90);
			else
				TEXTOUT( x-yvaltext_widths[i]/2, values[i] > 0 ? yy - max_xtick_height - 0.025f : yy + 0.025f , yvaltext_labels[i], 0 );
		}		
	}

	LINEOUT( 0, 0, 0, gbox.height );
	LINEOUT( 0, y0, gbox.width, y0 );
	

#undef TO_DEVICE
#undef LINEOUT
#undef RECTOUT
#undef TEXTOUT

	m_curXPos += gr_width;
	m_curYPos += gr_height;	

	// restore style information
	Style(saveFace, saveSize, saveColour, saveBold, saveItalic, saveAlign, saveWidth, saveStyle );
}

bool SamReportScriptObject::ReadData( wxInputStream &is )
{
	wxDataInputStream in( is );
	unsigned short id_code = in.Read16(); // ID CODE.
	in.Read8();// unsigned char version
	m_script = in.ReadString();
	return id_code == in.Read16();
}

bool SamReportScriptObject::WriteData( wxOutputStream &os )
{
	wxDataOutputStream out( os );
	out.Write16( 0xa3 );
	out.Write8( 1 ); // version
	out.WriteString( m_script );
	out.Write16( 0xaa );
	return true;
}


/*user.global.end*/
enum {
  ID_cklTechModels = wxID_HIGHEST+213,
  ID_GroupBox2,
  ID_GroupBox3,
  ID_GroupBox1,
  ID_cklFinModels,
  ID_Label31,
  ID_Label3,

  ID_Label6,
  ID_txtFooter,
  ID_txtHeader,
  ID_Label5,
  ID_Label4,
  ID_chkSpecificModelsOnly,
  ID_txtAuthor,
  ID_txtDescription,
  ID_Label2,
  ID_Label1 };

BEGIN_EVENT_TABLE( ReportPropertyDialog, wxDialog )
	EVT_CHECKLISTBOX( ID_cklTechModels, ReportPropertyDialog::OnCommand )
	EVT_CHECKBOX( ID_chkSpecificModelsOnly, ReportPropertyDialog::OnCommand )
END_EVENT_TABLE()

ReportPropertyDialog::ReportPropertyDialog(wxWindow *parent, int id)
	 : wxDialog( parent, id, "Report Template Properties" )
{
	SetClientSize( 598, 455 );
	GroupBox2 = new wxStaticBox(this, ID_GroupBox2, "Model Selection", wxPoint(9,96), wxSize(581,200));
	GroupBox3 = new wxStaticBox(this, ID_GroupBox3, "Headers and Footers", wxPoint(9,300), wxSize(581,119));
	GroupBox1 = new wxStaticBox(this, ID_GroupBox1, "Template Information", wxPoint(9,9), wxSize(581,80));
	txtDescription = new wxExtTextCtrl(this, ID_txtDescription, wxEmptyString, wxPoint(141,30), wxSize(244,21));
	txtAuthor = new wxExtTextCtrl(this, ID_txtAuthor, wxEmptyString, wxPoint(141,54), wxSize(244,21));
	chkSpecificModelsOnly = new wxCheckBox(this, ID_chkSpecificModelsOnly, "Choose specific technology/financing models for which this template is valid", wxPoint(21,117), wxSize(560,21));
	chkSpecificModelsOnly->SetValue( true );
	txtHeader = new wxExtTextCtrl(this, ID_txtHeader, wxEmptyString, wxPoint(135,321), wxSize(442,21));
	txtFooter = new wxExtTextCtrl(this, ID_txtFooter, wxEmptyString, wxPoint(135,345), wxSize(442,21));
	btnOK = new wxButton(this, wxID_OK, "OK", wxPoint(420,423), wxSize(80,21));
	btnCancel = new wxButton(this, wxID_CANCEL, "Cancel", wxPoint(507,423), wxSize(80,21));
	wxArrayString _data_cklFinModels;
	cklFinModels = new wxCheckListBox(this, ID_cklFinModels, wxPoint(300,168), wxSize(203,114), _data_cklFinModels, 0);
	wxArrayString _data_cklTechModels;
	cklTechModels = new wxCheckListBox(this, ID_cklTechModels, wxPoint(90,168), wxSize(203,114), _data_cklTechModels, 0);
	Label31 = new wxStaticText(this, ID_Label31, "Allowed Financial Models:", wxPoint(300,144), wxSize(203,21));
	Label3 = new wxStaticText(this, ID_Label3, "Allowed Technology Models:", wxPoint(90,144), wxSize(203,21));
	Label6 = new wxStaticText(this, ID_Label6, "Note: Headers and footers can contain the following escape sequences: @PAGENUM@, @PAGECOUNT@, @DATETIME@, @SAMVER@", wxPoint(54,372), wxSize(524,36));
	Label6->Wrap( 300 );
	Label5 = new wxStaticText(this, ID_Label5, "Footer text:", wxPoint(21,345), wxSize(110,21));
	Label4 = new wxStaticText(this, ID_Label4, "Header text:", wxPoint(21,321), wxSize(110,21));
	Label2 = new wxStaticText(this, ID_Label2, "Description:", wxPoint(21,30), wxSize(119,21));
	Label1 = new wxStaticText(this, ID_Label1, "Author:", wxPoint(21,54), wxSize(119,21));
}

ReportPropertyDialog::~ReportPropertyDialog()
{
}

void ReportPropertyDialog::SetData( SamReportTemplate *rt )
{

	rt->GetInfo(&m_origDesc, &m_origAuthor, 0, 0);
	txtDescription->SetValue(m_origDesc);
	txtAuthor->SetValue( m_origAuthor);
	m_origSpecMods = rt->GetSpecificModelsOnly();
	chkSpecificModelsOnly->SetValue( m_origSpecMods );
	cklTechModels->Enable( m_origSpecMods );
	cklFinModels->Enable( m_origSpecMods );
	rt->GetHeaderFooter( &m_origHeader, &m_origFooter );
	txtHeader->SetValue(m_origHeader);
	txtFooter->SetValue(m_origFooter);


	if (m_origSpecMods)
	{
		UpdateTechList();
		wxArrayString m_origTechMods, m_origFinMods;
		rt->GetModels( &m_origTechMods, &m_origFinMods );

		for ( size_t i=0;i<cklTechModels->GetCount();i++)
			if ( m_origTechMods.Index( cklTechModels->GetString(i) ) != wxNOT_FOUND )
				cklTechModels->Check( i, true );

		UpdateFinList();
		for ( size_t i=0;i<cklFinModels->GetCount();i++)
			if ( m_origFinMods.Index( cklFinModels->GetString(i) ) != wxNOT_FOUND )
				cklFinModels->Check( i, true );

	}
}

bool ReportPropertyDialog::GetData( SamReportTemplate *rt )
{
	rt->SetInfo( txtDescription->GetValue(),
		txtAuthor->GetValue(),
		wxEmptyString,
		wxEmptyString );
	rt->SetSpecificModelsOnly( chkSpecificModelsOnly->GetValue() );
	wxArrayString seltech = GetSelectedTechs();
	wxArrayString selfin = GetSelectedFins();
	rt->SetModels( seltech, selfin );
	rt->SetHeaderFooter( txtHeader->GetValue(), txtFooter->GetValue() );

	return m_origDesc != txtDescription->GetValue()
		|| m_origAuthor != txtAuthor->GetValue()
		|| m_origSpecMods != chkSpecificModelsOnly->GetValue()
		|| m_origTechMods != seltech
		|| m_origFinMods != selfin
		|| m_origHeader != txtHeader->GetValue()
		|| m_origFooter != txtFooter->GetValue();
}

void ReportPropertyDialog::UpdateTechList()
{
	wxArrayString sel = GetSelectedTechs();
	cklTechModels->Clear();
	cklTechModels->Append( SamApp::Config().GetTechnologies() );
	for ( size_t i=0;i<cklTechModels->GetCount();i++)
		cklTechModels->Check( i, sel.Index( cklTechModels->GetString(i) ) != wxNOT_FOUND );
}

void ReportPropertyDialog::UpdateFinList()
{
	wxArrayString sel = GetSelectedFins();
	cklFinModels->Clear();

	wxArrayString techs = GetSelectedTechs();
	wxArrayString fins;
	for (size_t i=0;i<techs.Count();i++)
	{
		wxArrayString list = SamApp::Config().GetFinancingForTech( techs[i] );
		for (size_t j=0;j<list.Count();j++)
			if ( fins.Index( list[j] ) == wxNOT_FOUND )
				fins.Add( list[j] );
	}
	
	cklFinModels->Append( fins );
	for ( size_t i=0;i<cklFinModels->GetCount();i++)
		cklFinModels->Check( i, sel.Index( cklFinModels->GetString(i) ) != wxNOT_FOUND );
}


wxArrayString ReportPropertyDialog::GetSelectedTechs()
{
	wxArrayString list;
	for ( size_t i=0;i<cklTechModels->GetCount();i++)
		if ( cklTechModels->IsChecked(i) )
			list.Add( cklTechModels->GetString( i ));
	return list;
}

wxArrayString ReportPropertyDialog::GetSelectedFins()
{
	wxArrayString list;
	for ( size_t i=0;i<cklFinModels->GetCount();i++)
		if ( cklFinModels->IsChecked(i) )
			list.Add( cklFinModels->GetString( i ));
	return list;
}

void ReportPropertyDialog::OnCommand( wxCommandEvent & evt)
{
	if (evt.GetId() == ID_chkSpecificModelsOnly)
	{
		bool b = chkSpecificModelsOnly->GetValue();
		cklTechModels->Enable( b );
		cklFinModels->Enable( b );
		if (b)
		{
			UpdateTechList();
			UpdateFinList();
		}
		else
		{
			cklTechModels->Clear();
			cklFinModels->Clear();
		}
	}
	else if ( evt.GetId() == ID_cklTechModels )
	{
		UpdateFinList();
	}
}
