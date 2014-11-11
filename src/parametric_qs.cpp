#include "parametric_qs.h"
#include "parametric.h"
#include "main.h"
#include "casewin.h"
#include "library.h"
#include <wex/utils.h>


enum { ID_lstValues = wxID_HIGHEST+495,
  ID_lstVariables,
  ID_btnEditValues,
  ID_btnRemoveVar,
  ID_btnAddVar };

BEGIN_EVENT_TABLE( Parametric_QS, wxDialog )
	EVT_BUTTON( ID_btnAddVar, Parametric_QS::OnAddVariable )
	EVT_BUTTON( ID_btnRemoveVar, Parametric_QS::OnRemoveVariable )
	EVT_LISTBOX( ID_lstVariables, Parametric_QS::OnVariableSelect )
	EVT_LISTBOX_DCLICK( ID_lstVariables, Parametric_QS::OnVarDblClick)
	EVT_BUTTON( ID_btnEditValues, Parametric_QS::OnEditValues )
	EVT_LISTBOX_DCLICK( ID_lstValues, Parametric_QS::OnValueDblClick)
	EVT_BUTTON( wxID_OK, Parametric_QS::OnCommand )
	EVT_BUTTON( wxID_HELP, Parametric_QS::OnCommand )
END_EVENT_TABLE()

Parametric_QS::Parametric_QS(wxWindow *parent, Case *c)
	: wxDialog(parent, wxID_ANY, "Parametric Quick Setup", wxDefaultPosition, wxSize(550,350), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER ), 
	m_case(c)
{
	lstVariables = new wxListBox(this, ID_lstVariables);
	lstValues = new wxListBox(this, ID_lstValues);
		

	wxBoxSizer *bsvars = new wxBoxSizer(wxHORIZONTAL);
	bsvars->Add( new wxStaticText(this, wxID_ANY, "Variables:"), 0, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, 3);
	bsvars->AddStretchSpacer();
	bsvars->Add( new wxButton(this, ID_btnAddVar, "Add", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL, 3);
	bsvars->Add( new wxButton(this, ID_btnRemoveVar, "Remove", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL, 3);

	wxBoxSizer *bsvals = new wxBoxSizer(wxHORIZONTAL);
	bsvals->Add( new wxStaticText(this, wxID_ANY, "Selected variable values:"), 0, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, 3);
	bsvals->AddStretchSpacer();
	bsvals->Add( new wxButton(this, ID_btnEditValues, "Edit", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL, 3);
	
	wxFlexGridSizer *fgs = new wxFlexGridSizer(2, 2, 5, 25);

	fgs->Add(bsvars, 3, wxEXPAND | wxALL, 3);
	fgs->Add(bsvals, 3, wxEXPAND | wxALL, 3);
	fgs->Add(lstVariables, 3, wxEXPAND | wxALL, 3);
	fgs->Add(lstValues, 3, wxEXPAND | wxALL, 3);

	fgs->AddGrowableCol(0, 1);
	fgs->AddGrowableCol(1, 1);
	fgs->AddGrowableRow(1, 1);

	wxBoxSizer *main_sizer = new wxBoxSizer( wxVERTICAL );
	main_sizer->Add( fgs, 1, wxALL|wxEXPAND, 5 );
	main_sizer->Add( CreateButtonSizer( wxOK|wxCANCEL|wxHELP ), 0, wxALL|wxEXPAND, 5 );
	SetSizer(main_sizer);
}


void Parametric_QS::OnCommand( wxCommandEvent &evt )
{
	if ( evt.GetId() == wxID_OK )
	{
		if (wxYES == wxMessageBox("Overwrite parametric table inputs with quick setup inputs?", "Overwrite table", wxYES_NO))
			UpdateCaseParametricData();

		EndModal( wxID_OK );
	}
	else if ( evt.GetId() == wxID_HELP )
		SamApp::ShowHelp( "parametric_quick_setup" );
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
	if (idx < 0 || idx > m_input_names.Count())
		wxMessageBox("No variable selected!");
	else
	{
		wxString name = m_input_names[idx];
		wxArrayString values = GetValuesList(name);
		VarInfo *varinfo = m_case->Variables().Lookup(name);
		if (varinfo)
		{
			if (ShowEditValuesDialog(
					"Edit Parametric Values for '" + varinfo->Label +
					((varinfo->Units !="") ? (" ("+ varinfo->Units +")'") :"'"),
					values, name) )
			{
				SetValuesList(name, values);
				RefreshValuesList();
			}
		}
		
	}
}

bool Parametric_QS::ShowFixedDomainDialog(const wxString &title,
	const wxArrayString &names, const wxArrayString &labels, wxArrayString &list,
	bool expand_all)
{
	SelectVariableDialog dlg(this, title);
	dlg.SetItems(names, labels);
	dlg.SetCheckedNames(list);
	if (expand_all)
		dlg.ShowAllItems();

	if (dlg.ShowModal() == wxID_OK)
	{
		wxArrayString names = dlg.GetCheckedNames();

		// remove any from list
		int i = 0;
		while (i<(int)list.Count())
		{
			if (names.Index(list[i]) < 0)
				list.RemoveAt(i);
			else
				i++;
		}

		// append any new ones
		for (i = 0; i<(int)names.Count(); i++)
		{
			if (list.Index(names[i]) < 0)
				list.Add(names[i]);
		}


		return true;
	}
	else
		return false;
}


bool Parametric_QS::ShowEditValuesDialog(const wxString &title,
	wxArrayString &values, const wxString &varname)
{

	VarInfo *vi = m_case->Variables().Lookup(varname);
	if (!vi)
		return false;
	VarValue *vv = m_case->Values().Get(varname);
	if (!vv)
		return false;

	int i;
	int vvtype = vv->Type();
	int vitype = vi->Type;
	unsigned long vf = vi->Flags;


	if (vvtype == VV_NUMBER
		&& vi->IndexLabels.Count() > 0)
	{
		// fixed domain selection (combo box, list, radio choice etc)
		wxArrayString fixed_items = vi->IndexLabels;
		wxArrayString cur_items;
		for (i = 0; i<(int)values.Count(); i++)
		{
			int item_i = atoi(values[i].c_str());
			if (item_i >= 0 && item_i < (int)fixed_items.Count())
				cur_items.Add(fixed_items[item_i]);
		}

		if (ShowFixedDomainDialog(title, fixed_items, fixed_items, cur_items, true))
		{
			// translate back to integer values
			values.Clear();
			for (int i = 0; i<(int)cur_items.Count(); i++)
				values.Add(wxString::Format("%d", fixed_items.Index(cur_items[i])));

			return true;
		}
		else
			return false;
	}
	else if (vf & VF_LIBRARY)
	{
		// get lib item list (climate or lib list)
		wxArrayString fixed_items;
		wxArrayString lib_fields = vi->IndexLabels;
		if (lib_fields.Count() > 0)
		{
			wxString name = lib_fields[0];
			if (Library *lib = Library::Find( name ))
				{
				fixed_items = lib->ListEntries();
				return ShowFixedDomainDialog(title, fixed_items, fixed_items, values, true);
			}
		}
	}
	else if (vvtype == VV_NUMBER)
	{
		return ShowNumericValuesDialog(title, values);
	}
	/*
	else if (vtype == VAR_STRING && v->GetDataSource() == ::VDSRC_INPUT && v->GetExpression() != "")
	{
		// STRING combo box
		wxArrayString fixed_items = Split(v->GetExpression(), ",");
		return ShowFixedDomainDialog(title, fixed_items, values);
	}
	*/

	wxMessageBox("Could not edit values for \"" + vi->Label + "\" (domain type error)");
	return false;
}


bool Parametric_QS::ShowNumericValuesDialog(const wxString &title,
	wxArrayString &values)
{
	NumericRangeDialog dlg( this, title );
	dlg.SetValues(values, false);

	if (dlg.ShowModal() == wxID_OK)
	{
		values = dlg.GetValues();
		return true;
	}
	else
		return false;
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
		wxString name = "";
		if ((idx > 0) && (idx < m_input_names.Count()))
			name = m_input_names[idx];

		for (std::vector<wxArrayString>::iterator it = m_input_values.begin();
			it != m_input_values.end(); ++it)
		{
			if ((*it).Item(0) == name)
			{
				m_input_values.erase(it);
				break;
			}
		}

		m_input_names.RemoveAt(idx);
	}

	RefreshVariableList();

	if (lstVariables->GetCount() > 0)
		lstVariables->Select(idx-1 >= 0 ? idx-1 : idx );

	RefreshValuesList();

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
	
	if (idx >= 0 && idx < m_input_names.Count())
	{
		wxString name = m_input_names[idx];
		items = GetValuesDisplayList( name );
		if (items.Count() == 0) // add base case value
		{
			wxArrayString values;
			values.Add(name);
			wxString val = GetBaseCaseValue(name);
			values.Add(val);
			m_input_values.push_back(values);
			items = GetValuesDisplayList(name);
		}
	}
	
	lstValues->Freeze();
	lstValues->Clear();
	lstValues->Append(items);
	lstValues->Thaw();
}


wxString Parametric_QS::GetBaseCaseValue(const wxString &varname)
{
	wxString val;
	VarValue *vv = m_case->Values().Get(varname);
	if (vv)
		val = vv->AsString();
	return val;
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

wxArrayString Parametric_QS::GetValuesDisplayList(const wxString &varname)
{
	wxArrayString list;

	VarInfo *vi = m_case->Variables().Lookup(varname);
	if (!vi)
		return list;
	VarValue *vv = m_case->Values().Get(varname);
	if (!vv)
		return list;

	int i;
	int vvtype = vv->Type();
	int vitype = vi->Type;


	if (vvtype == VV_NUMBER
		&& vi->IndexLabels.Count() > 0)
	{
		// fixed domain selection (combo box, list, radio choice etc)
		wxArrayString fixed_items = vi->IndexLabels;
		for (int i = 0; i < m_input_values.size(); i++)
		{
			if (m_input_values[i].Count() > 0 && m_input_values[i].Item(0) == varname)
			{
				for (int j = 1; j < m_input_values[i].Count(); j++)
				{
					int item_i = atoi(m_input_values[i].Item(j).c_str());
					if (item_i >= 0 && item_i < (int)fixed_items.Count())
						list.Add(fixed_items[item_i]);
				}
				break;
			}
		}
	}
	else
	{
		for (int i = 0; i < m_input_values.size(); i++)
		{
			if (m_input_values[i].Count() > 0 && m_input_values[i].Item(0) == varname)
			{
				for (int j = 1; j < m_input_values[i].Count(); j++)
					list.Add(m_input_values[i].Item(j));
				break;
			}
		}
	}
	return list;
}

void Parametric_QS::SetValuesList(const wxString &varname, const wxArrayString &values)
{
	int idx = -1;
	if (values.Count() <= 0) return;
	for (int i = 0; i < m_input_values.size(); i++)
	{
		if (m_input_values[i].Count() > 0 && m_input_values[i].Item(0) == varname)
		{
			idx = i;
			break;
		}
	}
	wxArrayString vals;
	vals.Add(varname);
	for (int i = 0; i < values.Count(); i++)
		vals.Add(values[i]);
	if (idx > -1)
		m_input_values[idx] = vals;
	else
		m_input_values.push_back(vals);
}

void Parametric_QS::UpdateCaseParametricData()
{
	ParametricData &par = m_case->Parametric();

	// save original outputs
	wxArrayString outputs;
	for (size_t i = 0; i < par.Setup.size(); i++)
	{
		if (VarValue *vv = m_case->Values().Get(par.Setup[i].Name))
			continue;
		outputs.Add(par.Setup[i].Name);
	}


	par.ClearRuns();
	par.Setup.clear();
	
	// combinations
	int num_runs = 1;
	for (int i = 0; i < m_input_values.size(); i++)
		num_runs *= m_input_values[i].Count() - 1;
	// create new inputs
	for (int i = 0; i < m_input_names.Count(); i++)
	{
		std::vector<VarValue> vvv;
		ParametricData::Var pv;
		for (int num_run = 0; num_run < num_runs; num_run++)
		{ // add values for inputs only
			if (VarValue *vv = m_case->Values().Get(m_input_names[i]))
				vvv.push_back(*vv);
		}
		pv.Name = m_input_names[i];
		pv.Values = vvv;
		par.Setup.push_back(pv);
	}
	for (int num_run =0; num_run < num_runs; num_run++)
	{
		Simulation *s = new Simulation(m_case, wxString::Format("Parametric #%d", (int)(num_run + 1)));
		par.Runs.push_back(s);
	}
	// set values - can do this once and set num_runs
	int repeat = 1;
	for (int col = 0; col < m_input_names.Count(); col++)
	{
		int row = 0;
		wxArrayString vals = GetValuesList(m_input_names[col]);
		while (row < num_runs - 1)
		{
			for (int j = 0; j < vals.Count(); j++)
			{
				for (int k = 0; k < repeat; k++)
				{
					wxString value = vals[j];
					VarValue *vv = &par.Setup[col].Values[row];
					VarValue::Parse(vv->Type(), value, *vv);
					row++;
				}
			}
		}
		repeat *= vals.Count();
	}



	// add original outputs back 
	for (int i = 0; i < outputs.Count(); i++)
	{
		std::vector<VarValue> vvv;
		ParametricData::Var pv;
		for (int num_run = 0; num_run < num_runs; num_run++)
		{ // add values for inputs only
			if (VarValue *vv = m_case->Values().Get(outputs[i]))
				vvv.push_back(*vv);
		}
		pv.Name = outputs[i];
		pv.Values = vvv;
		par.Setup.push_back(pv);
	}

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

