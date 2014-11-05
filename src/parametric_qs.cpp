#include "parametric_qs.h"
#include "parametric.h"
#include "main.h"
#include "casewin.h"

#include <wex/utils.h>


enum {
  ID_GroupBox1,
  ID_Label3,
  ID_grpOutline,
  ID_lstValues,
  ID_lstVariables,
  ID_Label1,
  ID_btnEditValues,
  ID_Label2,
  ID_Cancel,
  ID_OK,
  ID_btnRemoveVar,
  ID_btnAddVar };

BEGIN_EVENT_TABLE( Parametric_QS, wxPanel )
	EVT_BUTTON( ID_btnAddVar, Parametric_QS::OnAddVariable )
	EVT_BUTTON( ID_btnRemoveVar, Parametric_QS::OnRemoveVariable )
	EVT_LISTBOX( ID_lstVariables, Parametric_QS::OnVariableSelect )
	EVT_LISTBOX_DCLICK( ID_lstVariables, Parametric_QS::OnVarDblClick)
	EVT_BUTTON( ID_btnEditValues, Parametric_QS::OnEditValues )
	EVT_LISTBOX_DCLICK( ID_lstValues, Parametric_QS::OnValueDblClick)
END_EVENT_TABLE()

Parametric_QS::Parametric_QS(wxWindow *parent, Case *c)
: wxPanel(parent), m_case(c)
{
	btnAddVar = new wxButton(this, ID_btnAddVar, "Add");
	btnRemoveVar = new wxButton(this, ID_btnRemoveVar, "Remove");
	btnEditValues = new wxButton(this, ID_btnEditValues, "Edit");
	wxArrayString _data_lstVariables;

	lstVariables = new wxListBox(this, ID_lstVariables, wxPoint(-1, -1), wxSize(-1, -1), _data_lstVariables, wxLB_SINGLE);
	wxArrayString _data_lstValues;
	lstValues = new wxListBox(this, ID_lstValues, wxPoint(-1, -1), wxSize(-1, -1), _data_lstValues, wxLB_SINGLE);

	Label1 = new wxStaticText(this, ID_Label1, "Variables:");
	Label2 = new wxStaticText(this, ID_Label2, "Selected Variable Values:");


	wxFlexGridSizer *fgs = new wxFlexGridSizer(2, 2, 5, 25);

	wxBoxSizer *bsvars = new wxBoxSizer(wxHORIZONTAL);
	bsvars->Add(Label1, 0, wxALIGN_CENTER, 0);
	bsvars->AddStretchSpacer();
	bsvars->Add(btnAddVar, 0);
	bsvars->Add(btnRemoveVar, 0);

	wxBoxSizer *bsvals = new wxBoxSizer(wxHORIZONTAL);
	bsvals->Add(Label2, 0, wxALIGN_CENTER, 0);
	bsvals->AddStretchSpacer();
	bsvals->Add(btnEditValues, 0);

	fgs->Add(bsvars, 3, wxEXPAND | wxALL, 0);
	fgs->Add(bsvals, 3, wxEXPAND | wxALL, 0);
	fgs->Add(lstVariables, 3, wxEXPAND | wxALL, 0);
	fgs->Add(lstValues, 3, wxEXPAND | wxALL, 0);

	fgs->AddGrowableCol(0, 1);
	fgs->AddGrowableCol(1, 1);
	fgs->AddGrowableRow(1, 1);

	SetSizer(fgs);

}


void Parametric_QS::UpdateFromParametricData()
{
	RefreshVariableList();
	RefreshValuesList();
}


void Parametric_QS::OnEditValues(wxCommandEvent &evt)
{
	if ( !m_case)
		return;

	int idx = lstVariables->GetSelection();
	if (idx < 0)
		wxMessageBox("No variable selected!");
	else
	{
		/*
		wxArrayString values = m_par.Variables[idx].VarValues;
		VarInfo *varinfo = m_case->GetSymTab()->Lookup( m_par->Variables[idx].VarName );
		if (varinfo)
		{
			if (m_caseWin->ShowEditValuesDialog(
					"Edit Parametric Values for '" + varinfo->GetLabel() +
					((varinfo->GetUnits()!="") ? (" ("+varinfo->GetUnits()+")'") :"'"),
					values, varinfo) )
			{
				m_par->Variables[idx].VarValues = values;
				RefreshValuesList();
			}
		}
		*/
	}
}



void Parametric_QS::OnValueDblClick(wxCommandEvent &evt)
{
	OnEditValues(evt);
}


void Parametric_QS::OnRemoveVariable(wxCommandEvent &evt)
{
	if ( !m_case)
		return;

	int idx = lstVariables->GetSelection();
	if (idx < 0)
		wxMessageBox("No variable selected!");
	else
	{
		/*
		wxString name = m_par->Variables[idx].VarName;
		if (m_par->Linkages.Index(name)>=0)
			m_par->Linkages.Remove(name);

		if (m_par->Linkages.Count() < 2)
			m_par->Linkages.Clear();

		m_par->Variables.remove( idx );
		*/
	}

	//m_caseWin->GetMDIParent()->FileModified();
	//UpdateFromSimInfo();

	if (lstVariables->GetCount() > 0)
		lstVariables->Select(idx-1 >= 0 ? idx-1 : idx );
}

void Parametric_QS::OnAddVariable(wxCommandEvent &evt)
{
	if ( !m_case )
		return;

	wxArrayString names, labels;
	wxString case_name(SamApp::Project().GetCaseName(m_case));

	ConfigInfo *ci = m_case->GetConfiguration();
	VarInfoLookup &vil = ci->Variables;

	for (VarInfoLookup::iterator it = vil.begin(); it != vil.end(); ++it)
	{
		wxString name = it->first;
		VarInfo &vi = *(it->second);

		// update to select only "Parametric" variables
		if (vi.Flags & VF_PARAMETRIC)
		{
			wxString label = vi.Label;
			if (label.IsEmpty())
				label = "{ " + name + " }";
			if (!vi.Units.IsEmpty())
				label += " (" + vi.Units + ")";
			if (!vi.Group.IsEmpty())
				label = vi.Group + "/" + label;

			labels.Add(label);
			names.Add(name);
		}
	}

	wxSortByLabels(names, labels);
	SelectVariableDialog dlg(this, "Select Inputs");
	dlg.SetItems(names, labels);
	dlg.SetCheckedNames(m_input_names);
	if (dlg.ShowModal() == wxID_OK)
	{
		m_input_names = dlg.GetCheckedNames();
		RefreshVariableList();
	}
}



void Parametric_QS::OnVariableSelect(wxCommandEvent &evt)
{
	RefreshValuesList();
}

void Parametric_QS::OnVarDblClick(wxCommandEvent &evt)
{
	RefreshValuesList();
	OnEditValues(evt);
}

void Parametric_QS::RefreshValuesList()
{
	if (!m_case)
		return;


	wxArrayString items;

	int idx = lstVariables->GetSelection();
	/*
	if (idx >= 0 && idx < m_par->Variables.count())
	{
		items = GetValuesList( m_par->Variables[idx].VarName );

		if (m_par->Linkages.Index( m_par->Variables[idx].VarName ) >= 0)
		{
			// append to items values of other linkages
			for (int i=0;i<(int)m_par->Linkages.Count();i++)
			{
				if (m_par->Linkages[i] == m_par->Variables[idx].VarName)
					continue;

				wxArrayString linkitems = GetValuesList( m_par->Linkages[i] );

				for (int k=0;k<(int)items.Count();k++)
				{
					if (k < (int)linkitems.Count())
						items[k] += " [" + linkitems[k] + "]";
					else
						items[k] += " [?]";
				}
			}
		}
	}
	*/
	lstValues->Freeze();
	lstValues->Clear();
	lstValues->Append(items);
	lstValues->Thaw();
}

wxArrayString Parametric_QS::GetValuesList(const wxString &varname)
{
	wxArrayString list;
	for (int i = 0; i < m_input_values.size(); i++)
	{
		if (m_input_values[i].Count() > 0 && m_input_values[i].Item(0) == varname)
		{
			for (int j = 1; j < m_input_values[i].Count(); j++)
				list.Add(m_input_values[i].Item(j));
			break;
		}
	}
	return list;
}

void Parametric_QS::RefreshVariableList()
{
	if (!m_case)
		return;

	lstVariables->Freeze();
	lstVariables->Clear();
	
	for (int i=0;i<m_input_names.Count();i++)
	{
		VarInfo *vi = m_case->Variables().Lookup(m_input_names[i]);
		if (!vi)
		{
			lstVariables->Append("<<Label Lookup Error>>");
			continue;
		}
		wxString suffix = "";

		if (!vi->Units.IsEmpty())
			suffix += " (" + vi->Units + ") ";

		if (!vi->Group.IsEmpty())
			lstVariables->Append(vi->Group + "/" + vi->Label + suffix);
		else
			lstVariables->Append( vi->Label + suffix);
	}
	

	lstVariables->Thaw();

	lstValues->Clear();
}



BEGIN_EVENT_TABLE( Parametric_QSDialog, wxDialog )
	EVT_CLOSE(Parametric_QSDialog::OnClose)
	EVT_BUTTON(ID_OK, Parametric_QSDialog::OnCommand)
	EVT_BUTTON(ID_Cancel, Parametric_QSDialog::OnCommand)
	END_EVENT_TABLE()

	Parametric_QSDialog::Parametric_QSDialog(wxWindow *parent, const wxString &title, Case *c)
	: wxDialog(parent, -1, title, wxDefaultPosition, wxDefaultSize, wxRESIZE_BORDER | wxDEFAULT_DIALOG_STYLE)
{
//	ParametricData par(c);// set par based on case
	mPanel = new Parametric_QS(this, c);
	wxBoxSizer *button_sizer = new wxBoxSizer(wxHORIZONTAL);
	button_sizer->AddStretchSpacer();
	button_sizer->Add(new wxButton(this, ID_OK, "OK"));
	button_sizer->Add(new wxButton(this, ID_Cancel, "Cancel"));
	wxBoxSizer *main_sizer = new wxBoxSizer(wxVERTICAL);
	main_sizer->Add(mPanel, 2, wxALL | wxEXPAND, 10);
	main_sizer->Add(button_sizer, 0, wxALIGN_RIGHT | wxRIGHT | wxBOTTOM, 10);
	SetSizerAndFit(main_sizer);
}


void Parametric_QSDialog::OnCommand(wxCommandEvent &evt)
{
	switch (evt.GetId())
	{
	case ID_Cancel:
		EndModal(wxID_CANCEL);
		Destroy();
		break;
	case ID_OK:
		EndModal(wxID_OK);
		Destroy();
		break;
	}
}

void Parametric_QSDialog::OnClose(wxCloseEvent &evt)
{
	EndModal(wxID_CANCEL);
	Destroy();
}


