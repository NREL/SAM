#include "parametric_qs.h"
#include "parametric.h"
#include "case.h"
#include "casewin.h"


enum {
  ID_GroupBox1,
  ID_Label3,
  ID_grpOutline,
  ID_btnMoveDown,
  ID_btnMoveUp,
  ID_btnLinkages,
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
	EVT_BUTTON( ID_btnLinkages, Parametric_QS::OnEditLinkages)
	EVT_BUTTON( ID_btnMoveUp, Parametric_QS::OnMoveUpDown)
	EVT_BUTTON( ID_btnMoveDown, Parametric_QS::OnMoveUpDown)
	EVT_LISTBOX_DCLICK( ID_lstValues, Parametric_QS::OnValueDblClick)
	EVT_LISTBOX( ID_lstValues, Parametric_QS::OnValueSelection)
END_EVENT_TABLE()

Parametric_QS::Parametric_QS(wxWindow *parent, ParametricData &par, int id)
: wxPanel(parent, id), m_par(par)
{
	mCase = NULL;
//	SetClientSize(787, 272);
	SetClientSize(600, 272);
	grpOutline = new wxStaticBox(this, ID_grpOutline, "Parametric Simulation Setup", wxPoint(6, 6), wxSize(578, 260));
	btnAddVar = new wxButton(this, ID_btnAddVar, "Add", wxPoint(105, 27), wxSize(80, 21));
	btnRemoveVar = new wxButton(this, ID_btnRemoveVar, "Remove", wxPoint(189, 27), wxSize(80, 21));
	btnEditValues = new wxButton(this, ID_btnEditValues, "Edit", wxPoint(492, 27), wxSize(80, 21));
	wxArrayString _data_lstVariables;

	lstVariables = new wxListBox(this, ID_lstVariables, wxPoint(15, 54), wxSize(275, 114), _data_lstVariables, wxLB_SINGLE);
	wxArrayString _data_lstValues;
	lstValues = new wxListBox(this, ID_lstValues, wxPoint(297, 54), wxSize(275, 114), _data_lstValues, wxLB_SINGLE);
	btnLinkages = new wxButton(this, ID_btnLinkages, "Setup Linkages...", wxPoint(15, 174), wxSize(152, 21));
	btnMoveUp = new wxButton(this, ID_btnMoveUp, "Up", wxPoint(456, 174), wxSize(56, 21));
	btnMoveDown = new wxButton(this, ID_btnMoveDown, "Down", wxPoint(516, 174), wxSize(56, 21));


	Label1 = new wxStaticText(this, ID_Label1, "Variables:", wxPoint(15, 27), wxSize(86, 21));
	Label2 = new wxStaticText(this, ID_Label2, "Selected Variable Values:", wxPoint(297, 27), wxSize(167, 21));
	mCaseWin = NULL;
	mCase = NULL;
	m_par = NULL;
	btnMoveUp->Enable(false);
	btnMoveDown->Enable(false);
}

void Parametric_QS::InitForm( CaseWindow *cwin, ParametricData &par)
{
	mCaseWin = cwin;
	mCase = cwin? cwin->GetCase() : NULL;
	m_par = par;
}

void Parametric_QS::UpdateFromParametricData()
{
	RefreshVariableList();
	RefreshValuesList();
}

ParametricData &Parametric_QS::GetParametricData()
{
	return m_par;
}

void Parametric_QS::OnEditValues(wxCommandEvent &evt)
{
	if (!mCaseWin || !mCase)// || m_par.)
		return;

	int idx = lstVariables->GetSelection();
	if (idx < 0)
		wxMessageBox("No variable selected!");
	else
	{
		/*
		wxArrayString values = m_par.Variables[idx].VarValues;
		VarInfo *varinfo = mCase->GetSymTab()->Lookup( m_par->Variables[idx].VarName );
		if (varinfo)
		{
			if (mCaseWin->ShowEditValuesDialog(
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

void Parametric_QS::OnEditLinkages(wxCommandEvent &evt)
{
	if (!mCase)// || !m_par)
		return;

	wxArrayString names;
	wxArrayString labels;

	/*
	for (int i=0;i<m_par->Variables.count();i++)
	{
		names.Add( m_par->Variables[i].VarName );

		VarInfo *v = mCase->GetSymTab()->Lookup( names[i] );
		if (!v)
		{
			labels.Add("<<Label Lookup Error>>");
			continue;
		}
		if (v->GetContext() != "")
			labels.Add( v->GetContext() + "/" + v->GetLabel() );
		else
			labels.Add( v->GetLabel() );
	}

	mCaseWin->ShowSelectVariableDialog("Choose Linked Parametric Variables",
		names, labels, m_par->Linkages, true);

	if (m_par->Linkages.Count() < 2)
		m_par->Linkages.Clear();
*/

	RefreshVariableList();
	RefreshValuesList();

}

void Parametric_QS::OnMoveUpDown(wxCommandEvent &evt)
{
	if (!mCaseWin || !mCase )//|| !m_par)
		return;

	int n_vsel = lstVariables->GetSelection();
	if (n_vsel < 0)
		return;
	/*
	VarValueList &list = m_par->Variables[n_vsel];

	if (evt.GetId() == ID_btnMoveUp)
	{
		if (lstValues->GetCount() >= 2)
		{
			int isel = lstValues->GetSelection();
			if (isel >= 1)
			{
				wxString tmp = list.VarValues[isel - 1];
				list.VarValues[isel - 1] = list.VarValues[isel];
				list.VarValues[isel] = tmp;

				RefreshValuesList();
				lstValues->SetSelection( isel - 1 );
			}
		}
	}
	else // move down
	{
		if (lstValues->GetCount() >= 2)
		{
			int isel = lstValues->GetSelection();
			if (isel <= (int)lstValues->GetCount() - 2 && isel >= 0)
			{
				wxString tmp = list.VarValues[ isel + 1 ];
				list.VarValues[isel + 1] = list.VarValues[isel];
				list.VarValues[isel] = tmp;

				RefreshValuesList();
				lstValues->SetSelection( isel + 1 );
			}
		}
	}
	*/
}

void Parametric_QS::OnValueDblClick(wxCommandEvent &evt)
{
	OnEditValues(evt);
}

void Parametric_QS::OnValueSelection(wxCommandEvent &evt)
{
	btnMoveUp->Enable( lstValues->GetSelection() >= 0 );
	btnMoveDown->Enable( lstValues->GetSelection() >= 0 );
}

void Parametric_QS::OnRemoveVariable(wxCommandEvent &evt)
{
	if (!mCaseWin || !mCase)// || !m_par)
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

	//mCaseWin->GetMDIParent()->FileModified();
	//UpdateFromSimInfo();

	if (lstVariables->GetCount() > 0)
		lstVariables->Select(idx-1 >= 0 ? idx-1 : idx );
}

void Parametric_QS::OnAddVariable(wxCommandEvent &evt)
{
	if (!mCaseWin || !mCase )//|| !m_par)
		return;

	int i=0;
	wxArrayString varlist;

/*	for (i=0;i<m_par->Variables.count();i++)
		varlist.Add( m_par->Variables[i].VarName );

	if (mCaseWin->ChooseParametricsDialog( varlist ))
	{
		// remove any parametrics in paramsiminfo that are no longer in list
		while (i<m_par->Variables.count())
		{
			if ( varlist.Index( m_par->Variables[i].VarName ) < 0 )
				m_par->Variables.remove(i);// remove, do not increment i
			else
				i++;
		}

		bool v_added = false;
		// add any parametrics not already in paramsimlist
		for (i=0;i<(int)varlist.Count();i++)
		{
			bool found = false;
			for (int j=0;j<m_par->Variables.count();j++)
				if ( m_par->Variables[j].VarName == varlist[i] )
					found = true;

			if (!found)
			{
				VarValueList x;
				x.VarName = varlist[i];
				VarInfo *vptr = mCase->GetSymTab()->Lookup( varlist[i] );
				if (!vptr)
					continue;

				x.VarValues.Add( vptr->ValToString() );

				m_par->Variables.append( x );
				v_added = true;
			}
		}

		mCaseWin->GetMDIParent()->FileModified();
		RefreshVariableList();

		if (v_added)
		{
			lstVariables->Select( lstVariables->GetCount()-1 );
			RefreshValuesList();
		}
	}
	*/
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
	if (!mCase)// || !m_par)
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
	int i;
	/*
	VarValueList *item = NULL;
	for (i=0;i<(int)m_par->Variables.count();i++)
		if (m_par->Variables[i].VarName == varname)
			item = & m_par->Variables[i];

	if (!item)
		return list;

	VarInfo *v = mCase->GetSymTab()->Lookup( item->VarName );

	if (v)
	{
		if (v->GetType() == VAR_INTEGER && v->GetExpression() != "")
		{
			wxArrayString items = Split(v->GetExpression(), ",");
			for (i=0;i<(int)item->VarValues.Count();i++)
			{
				int item_i = atoi( item->VarValues[i].c_str() );
				if (item_i >= 0 && item_i < (int)items.Count())
					list.Add( items[item_i] );
				else
					list.Add( item->VarValues[i] );
			}
		}
		else
		{
			list = item->VarValues;
		}
	}
	*/
	return list;
}

void Parametric_QS::RefreshVariableList()
{
	if (!mCase)// || !m_par)
		return;

	lstVariables->Freeze();
	lstVariables->Clear();
	/*
	for (int i=0;i<m_par->Variables.count();i++)
	{
		VarInfo *v = mCase->GetSymTab()->Lookup( m_par->Variables[i].VarName );
		if (!v)
		{
			lstVariables->Append("<<Label Lookup Error>>");
			continue;
		}
		wxString suffix = "";

		if (v->GetUnits() != "")
			suffix += " (" + v->GetUnits() + ") ";

		if (m_par->Linkages.Index( m_par->Variables[i].VarName ) >= 0)
			suffix = " [Linked]";

		if (v->GetContext() != "")
			lstVariables->Append( v->GetContext() + "/" + v->GetLabel() + suffix );
		else
			lstVariables->Append( v->GetLabel() + suffix);
	}
	*/
	btnLinkages->Enable( lstVariables->GetCount() > 1 );

	lstVariables->Thaw();

	lstValues->Clear();
}



BEGIN_EVENT_TABLE( Parametric_QSDialog, wxDialog )
	EVT_CLOSE(Parametric_QSDialog::OnClose)
	EVT_BUTTON(ID_OK, Parametric_QSDialog::OnCommand)
	EVT_BUTTON(ID_Cancel, Parametric_QSDialog::OnCommand)
	END_EVENT_TABLE()

Parametric_QSDialog::Parametric_QSDialog(wxWindow *parent, const wxString &title, Case *c)
	 : wxDialog( parent, -1, title
	)
{
	ParametricData par(c);// set par based on case
	mPanel = new Parametric_QS(this,par);
//	wxSize _sz = mPanel->GetClientSize();
//	SetClientSize(_sz.GetWidth(), _sz.GetHeight());
	wxBoxSizer *button_sizer = new wxBoxSizer(wxHORIZONTAL);
	button_sizer->AddStretchSpacer();
	button_sizer->Add(new wxButton(this, ID_OK, "OK"), 0, wxALIGN_RIGHT, 2);
	button_sizer->Add(new wxButton(this, ID_Cancel, "Cancel"), 0, wxALIGN_RIGHT, 2);
	wxBoxSizer *main_sizer = new wxBoxSizer(wxVERTICAL);
	main_sizer->Add(mPanel);
	main_sizer->Add(button_sizer);
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


