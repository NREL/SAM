#include "defaultsmanager.h"

/*user.global.start*/

#include <wx/wfstream.h>

#include <wx/tokenzr.h>

/*user.global.end*/
enum {
  ID_btnRWAll,
  ID_GroupBox1,
  ID_Label9,
  ID_grdModifyTable,
  ID_btnModifyMultiple,
  ID_numTableVars,
  ID_btnClose,
  ID_Label8,
  ID_Label7,
  ID_txtOutput,
  ID_cklConfigurations,
  ID_btnUnselAll,
  ID_btnSelAll,
  ID_Label5,
  ID_Label4,
  ID_Label6,
  ID_btnApplyLibrary,
  ID_txtLibSuffix,
  ID_chkAdd,
  ID_btnRemove,
  ID_btnQuery,
  ID_btnModify,
  ID_chkModifyType,
  ID_cboVarType,
  ID_lstLibTypes,
  ID_lstLibItems,
  ID_txtVarValue,
  ID_txtVarName,
  ID_lblAllConfigurations,
  ID_btnLookup,
  ID_Label3,
  ID_Label2,
  ID_Label1 };

BEGIN_EVENT_TABLE( DefaultsManager, wxPanel )
/*user.eventtable.start*/

EVT_LISTBOX( ID_lstLibTypes, DefaultsManager::OnLibTypeSel )

EVT_BUTTON( ID_btnSelAll, DefaultsManager::OnSelUnselAll )
EVT_BUTTON( ID_btnUnselAll, DefaultsManager::OnSelUnselAll )
EVT_BUTTON( ID_btnLookup, DefaultsManager::OnLookup )
EVT_BUTTON( ID_btnModify, DefaultsManager::OnModify )
EVT_BUTTON( ID_btnApplyLibrary, DefaultsManager::OnApplyLib )
EVT_BUTTON( ID_btnQuery, DefaultsManager::OnQuery )
EVT_BUTTON( ID_btnRemove, DefaultsManager::OnRemove )
EVT_BUTTON( ID_btnModifyMultiple, DefaultsManager::OnModifyMultiple )
EVT_BUTTON( ID_btnRWAll, DefaultsManager::OnRWAll )

EVT_NUMERIC( ID_numTableVars, DefaultsManager::OnNumRowsChange )
EVT_GRID_CMD_CELL_LEFT_DCLICK( ID_grdModifyTable, DefaultsManager::OnGridDoubleClick )

/*user.eventtable.end*/
END_EVENT_TABLE()

DefaultsManager::DefaultsManager(wxWindow *parent, int id)
	 : wxPanel( parent, id )
{
/*user.klsinit.start*/
/*user.klsinit.end*/
	SetClientSize( 824, 641 );
	GroupBox1 = new wxStaticBox(this, ID_GroupBox1, "Actions on selected configurations", wxPoint(237,6), wxSize(581,404));
	btnLookup = new wxButton(this, ID_btnLookup, "Lookup", wxPoint(360,27), wxSize(53,21));
	txtVarName = new wxExtTextCtrl(this, ID_txtVarName, wxEmptyString, wxPoint(417,27), wxSize(268,21));
	txtVarName->ChangeValue("<enter variable name>");
	txtVarName->SetForegroundColour( wxColour(0, 0, 0) );
	txtVarName->SetBackgroundColour( wxColour(255, 255, 255) );
	txtVarValue = new wxExtTextCtrl(this, ID_txtVarValue, wxEmptyString, wxPoint(360,75), wxSize(325,21));
	txtVarValue->ChangeValue("");
	txtVarValue->SetForegroundColour( wxColour(0, 0, 0) );
	txtVarValue->SetBackgroundColour( wxColour(255, 255, 255) );
	wxArrayString _data_lstLibItems;
	lstLibItems = new wxListBox(this, ID_lstLibItems, wxPoint(393,129), wxSize(416,105), _data_lstLibItems, wxLB_SINGLE);
	wxArrayString _data_lstLibTypes;
	lstLibTypes = new wxListBox(this, ID_lstLibTypes, wxPoint(246,129), wxSize(143,132), _data_lstLibTypes, wxLB_SINGLE);
	wxArrayString _data_cboVarType;
	_data_cboVarType.Add("Integer");
	_data_cboVarType.Add("Integer Array");
	_data_cboVarType.Add("Double");
	_data_cboVarType.Add("Double Array");
	_data_cboVarType.Add("String");
	_data_cboVarType.Add("String Array");
	cboVarType = new wxComboBox(this, ID_cboVarType, "Integer", wxPoint(360,51), wxSize(145,21), _data_cboVarType, wxCB_READONLY);
	chkModifyType = new wxCheckBox(this, ID_chkModifyType, "Modify data type", wxPoint(516,51), wxSize(134,21));
	chkModifyType->SetValue( false );
	btnModify = new wxButton(this, ID_btnModify, "Modify", wxPoint(690,51), wxSize(71,21));
	btnQuery = new wxButton(this, ID_btnQuery, "Query", wxPoint(690,27), wxSize(119,21));
	btnRemove = new wxButton(this, ID_btnRemove, "Remove", wxPoint(690,75), wxSize(71,21));
	chkAdd = new wxCheckBox(this, ID_chkAdd, "Add", wxPoint(765,51), wxSize(44,21));
	chkAdd->SetValue( false );
	txtLibSuffix = new wxExtTextCtrl(this, ID_txtLibSuffix, wxEmptyString, wxPoint(564,240), wxSize(100,21));
	txtLibSuffix->ChangeValue("");
	txtLibSuffix->SetForegroundColour( wxColour(0, 0, 0) );
	txtLibSuffix->SetBackgroundColour( wxColour(255, 255, 255) );
	btnApplyLibrary = new wxButton(this, ID_btnApplyLibrary, "Apply library item", wxPoint(690,240), wxSize(119,21));
	btnSelAll = new wxButton(this, ID_btnSelAll, "Sel all", wxPoint(9,387), wxSize(80,21));
	btnUnselAll = new wxButton(this, ID_btnUnselAll, "Unsel all", wxPoint(93,387), wxSize(80,21));
	wxArrayString _data_cklConfigurations;
	cklConfigurations = new wxCheckListBox(this, ID_cklConfigurations, wxPoint(9,30), wxSize(221,354), _data_cklConfigurations, 0);
	txtOutput = new wxTextCtrl(this, ID_txtOutput, "", wxPoint(9,414), wxSize(807,198),wxTE_MULTILINE|wxTE_DONTWRAP|wxTE_PROCESS_TAB);
	txtOutput->SetFont(wxFont(10, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false, "courier"));
	txtOutput->ChangeValue("<messages>");
	txtOutput->SetEditable( false );
	btnClose = new wxButton(this, ID_btnClose, "Close", wxPoint(738,615), wxSize(80,21));
	numTableVars = new wxNumericCtrl(this, ID_numTableVars, 3, wxNumericCtrl::INTEGER, wxPoint(597,276), wxSize(37,21));
	numTableVars->SetFormat(0);
//	numTableVars->SetFormat( "%d");
//	numTableVars->SetInt( (int) 3 );
	numTableVars->SetValue(3);
	btnModifyMultiple = new wxButton(this, ID_btnModifyMultiple, "Modify Multiple", wxPoint(657,276), wxSize(152,21));
	btnRWAll = new wxButton(this, ID_btnRWAll, "RW all - no changes", wxPoint(690,99), wxSize(119,21));
	Label9 = new wxStaticText(this, ID_Label9, "Note: Multiple modify does NOT change data type or add a variable if it does not exist.", wxPoint(246,390), wxSize(563,15));
//	Label9->SetColour(wxColour(0, 0, 0));
//	Label9->SetRelativeSize(-1);
	grdModifyTable = new wxExtGridCtrl(this, ID_grdModifyTable, wxPoint(246,300), wxSize(563,87));
	grdModifyTable->CreateGrid(3,2);
	grdModifyTable->EnableEditing(true);
	grdModifyTable->DisableDragCell();
	grdModifyTable->DisableDragColSize();
	grdModifyTable->DisableDragRowSize();
	grdModifyTable->DisableDragColMove();
	grdModifyTable->DisableDragGridSize();
	grdModifyTable->SetRowLabelSize(23);
	grdModifyTable->SetColLabelSize(23);
	Label8 = new wxStaticText(this, ID_Label8, "Number of vars:", wxPoint(501,276), wxSize(95,21));
//	Label8->AlignRight();
//	Label8->SetColour(wxColour(0, 0, 0));
//	Label8->SetRelativeSize(0);
	Label7 = new wxStaticText(this, ID_Label7, "Modify multiple variable values", wxPoint(246,276), wxSize(251,21));
//	Label7->SetColour(wxColour(0, 0, 0));
//	Label7->SetBold( true );
//	Label7->SetRelativeSize(0);
	Label5 = new wxStaticText(this, ID_Label5, "Library Items for Selected Type:", wxPoint(393,108), wxSize(194,21));
//	Label5->SetColour(wxColour(0, 0, 0));
//	Label5->SetRelativeSize(0);
	Label4 = new wxStaticText(this, ID_Label4, "Library Types:", wxPoint(246,108), wxSize(110,21));
//	Label4->SetColour(wxColour(0, 0, 0));
//	Label4->SetRelativeSize(0);
	Label6 = new wxStaticText(this, ID_Label6, "Library variable name suffix:", wxPoint(393,240), wxSize(167,21));
//	Label6->AlignRight();
//	Label6->SetColour(wxColour(0, 0, 0));
//	Label6->SetRelativeSize(0);
	lblAllConfigurations = new wxStaticText(this, ID_lblAllConfigurations, "All Configurations", wxPoint(9,6), wxSize(221,21));
//	lblAllConfigurations->SetColour(wxColour(0, 0, 0));
//	lblAllConfigurations->SetRelativeSize(-1);
	Label3 = new wxStaticText(this, ID_Label3, "Default Value:", wxPoint(246,75), wxSize(110,21));
//	Label3->SetColour(wxColour(0, 0, 0));
//	Label3->SetRelativeSize(0);
	Label2 = new wxStaticText(this, ID_Label2, "Data Type:", wxPoint(246,51), wxSize(110,21));
//	Label2->SetColour(wxColour(0, 0, 0));
//	Label2->SetRelativeSize(0);
	Label1 = new wxStaticText(this, ID_Label1, "Variable Name:", wxPoint(246,27), wxSize(110,21));
//	Label1->SetColour(wxColour(0, 0, 0));
//	Label1->SetRelativeSize(0);
/*user.constructor.start*/

	txtOutput->SetFont(wxFont(8, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false, "lucida console"));
	txtOutput->SetForegroundColour( *wxLIGHT_GREY );
	txtOutput->SetBackgroundColour( "NAVY" );
	
	int num_defaults = 0;
	wxArrayString configs;
/*	wxArrayString techlist = DbGet()->GetTechnologies();
	for (int i=0;i<(int)techlist.Count();i++)
	{
		wxArrayString finlist = DbGet()->GetFinancingForTech(techlist[i]);
		for (int j=0;j<(int)finlist.Count();j++)
		{
			wxString item = techlist[i] + "," + finlist[j];
			
			mTechList.Add( techlist[i] );
			mFinList.Add( finlist[j] );

			if (wxFileExists(SamGetDefaultFile(techlist[i], finlist[j])))
				num_defaults++;
			else
				item = "(?)  " + item;

			configs.Add(item);
		}
	}
*/
	lblAllConfigurations->SetLabel( wxString::Format("%d total cfgs, %d def. files missing", configs.Count(), configs.Count()-num_defaults) ); 

	cklConfigurations->Append( configs );

//	LibraryManager *lm = LibGetManager();
//	Array<LibraryType*> types = lm->AllTypes();
//	for (int i=0;i<types.count();i++)
//		lstLibTypes->Append( types[i]->TypeName );

	grdModifyTable->SetColumnWidth( 0, 160 );
	grdModifyTable->SetColumnWidth( 1, 360 );
	grdModifyTable->SetColLabelValue( 0, "Variable" );
	grdModifyTable->SetColLabelValue( 1, "Value" );



/*user.constructor.end*/
}
DefaultsManager::~DefaultsManager()
{
/*user.destructor.start*/
/*user.destructor.end*/
}
/*user.class.start*/

void DefaultsManager::OnGridDoubleClick(wxGridEvent &evt)
{
	if (evt.GetCol() == 0)
	{
		wxString v = LookupVariable();
		if (!v.IsEmpty())
			grdModifyTable->SetCellValue( evt.GetRow(), evt.GetCol(), v );
	}
}

void DefaultsManager::OnNumRowsChange(wxCommandEvent &evt)
{
	int nrows = numTableVars->AsInteger();
	if (nrows > 0)
		grdModifyTable->ResizeGrid( nrows, 2 );
}

void DefaultsManager::OnLibTypeSel(wxCommandEvent &evt)
{
	lstLibItems->Clear();
//	lstLibItems->Append( LibGetEntriesForLibraryType( lstLibTypes->GetStringSelection() ) );
}

void DefaultsManager::OnRemove(wxCommandEvent &evt)
{
	if (wxYES!=wxMessageBox("Are you sure you want to remove '" + txtVarName->GetValue() + "' from selected configurations?", "Query", wxYES_NO))
		return;

	ClearLog();
/*
	for (int i=0;i<(int)cklConfigurations->GetCount();i++)
	{
		if (!cklConfigurations->IsChecked(i)) continue;
		wxString file = SamGetDefaultFile(mTechList[i], mFinList[i]);
		
		SymTab tab;
		if (!tab.ReadValues( file, true ))
		{
			Log("read error: " + file );
			continue;
		}

		VarInfo *v = tab.Lookup( txtVarName->GetValue() );
		if (v)
		{
			tab.Remove( v->GetName() );

			wxFFileOutputStream fos( file );
			if (fos.IsOk())
				tab.WriteValues( fos);
			else
				Log("Error writing: " + file );
			
			Log("Removed '" + txtVarName->GetValue() + "' from " + mTechList[i] + ", " + mFinList[i]);
		}
		else
			Log("Variable '" + txtVarName->GetValue() + "' not found in " + mTechList[i] + ", " + mFinList[i]);
	}
	*/
}

static wxString typestr(VarInfo *v)
{
	if (!v) return "<nullv>";
//	else return wxString(::var_type_names[ v->GetType() ]);
}

void DefaultsManager::OnModify(wxCommandEvent &evt)
{
	ClearLog();

	for (int i=0;i<(int)cklConfigurations->GetCount();i++)
	{
		if (!cklConfigurations->IsChecked(i)) continue;

		ModifyAdd(i, txtVarName->GetValue(),
			txtVarValue->GetValue(),
			chkModifyType->GetValue(),
			cboVarType->GetSelection(),
			chkAdd->GetValue() );
	}
}

void DefaultsManager::OnModifyMultiple(wxCommandEvent &evt)
{
	ClearLog();

	for (int j=0;j<grdModifyTable->GetNumberRows();j++)
	{
		wxString var = grdModifyTable->GetCellValue( j, 0 );
		wxString val = grdModifyTable->GetCellValue( j, 1 );

		if (var.IsEmpty() || val.IsEmpty()) continue;

		Log("Multiple modify: " + var + " = " + val );
		
		for (int i=0;i<(int)cklConfigurations->GetCount();i++)
		{
			if (!cklConfigurations->IsChecked(i)) continue;

			ModifyAdd(i, var, val, false, 0, false );
		}
	}
}


void DefaultsManager::ModifyAdd(int cfg_idx, const wxString &var_name,
	const wxString &var_value,
	bool en_type_change, int var_type,
	bool en_add )
{
/*
	wxString file = SamGetDefaultFile(mTechList[cfg_idx], mFinList[cfg_idx]);

	SymTab tab;
	if (!tab.ReadValues( file, true ))
	{
		Log("read error: " + file );
		return;
	}

	bool must_write = false;

	VarInfo *v = tab.Lookup( var_name );
	if (v)
	{
		if ( en_type_change )
			v->SetType( var_type );

		v->StringToVal( var_value );			
		
		Log("Modified '" + var_name + "' [" + typestr(v) + "] in " + mTechList[cfg_idx] + ", " + mFinList[cfg_idx] + " = " + v->ValToString());
		must_write = true;
	}
	else if (en_add)
	{
		v = tab.Create( var_name, var_type );
		if (v)
		{
			v->StringToVal( var_value );
			Log("Added '" + var_name + "' [" + typestr(v) + "] to " + mTechList[cfg_idx] + ", " + mFinList[cfg_idx] + " = " + v->ValToString());
			must_write = true;
		}
		else
			Log("Error adding '" + var_name + "' to " + mTechList[cfg_idx] + ", " + mFinList[cfg_idx]);
	}
	else
		Log("Did not modify or add '" + var_name + "' in " + mTechList[cfg_idx] + ", " + mFinList[cfg_idx]);

	if (must_write)
	{			

		wxFFileOutputStream fos( file );
		if (fos.IsOk())
			tab.WriteValues( fos );
		else
			Log("Error writing: " + file );
	}
	*/
}

void DefaultsManager::OnRWAll( wxCommandEvent & )
{
	ClearLog();
	
	for (int i=0;i<(int)cklConfigurations->GetCount();i++)
	{
		if (!cklConfigurations->IsChecked(i)) continue;

/*		
		wxString file = SamGetDefaultFile(mTechList[i], mFinList[i]);

		SymTab tab;
		if (!tab.ReadValues( file, true ))
		{
			Log("read error: " + file );
			return;
		}
		
		wxFFileOutputStream fos( file );
		if (fos.IsOk())
		{
			tab.WriteValues( fos );
			Log("r/w scan ok: " + file );
		}
		else
			Log("Error writing: " + file );
*/
	}
	
}

void DefaultsManager::OnApplyLib(wxCommandEvent &evt)
{
	ClearLog();
/*
	LibraryEntry *entry = ::LibLookupEntry( lstLibItems->GetStringSelection() );
	if (!entry)
	{
		wxMessageBox("Library entry not selected or not found.");
		return;
	}

	Log("Entry: " + entry->GetName() );
	wxArrayString list = entry->GetVarNames();
	for (int i=0;i<(int)list.Count();i++)
		Log("\t" + list[i]);

	for (int i=0;i<(int)cklConfigurations->GetCount();i++)
	{
		if (!cklConfigurations->IsChecked(i)) continue;
		wxString file = SamGetDefaultFile(mTechList[i], mFinList[i]);

		SymTab tab;
		if (!tab.ReadValues( file, true ))
		{
			Log("FPERR(r): " + file);
			continue;
		}
		
		wxArrayString updated;
		int count = ::LibApplyValues( entry, &tab, updated, txtLibSuffix->GetValue() );

		if ( count )
		{
			wxFFileOutputStream fos( file );
			if ( fos.IsOk() )
				tab.WriteValues( fos );
			else
				Log("Error writing: " + file);
			
			Log("Applied entry '" + entry->GetName() + "' (" + IntToStr(updated.Count()) + " vars) in" + mTechList[i] + ", " + mFinList[i]);
		}
		else
			Log("Did not apply entry '" + entry->GetName() + "' in " + mTechList[i] + ", " + mFinList[i]);
	}
*/
}

void DefaultsManager::OnQuery(wxCommandEvent &evt)
{
	ClearLog();

	for (int i=0;i<(int)cklConfigurations->GetCount();i++)
	{
		if (!cklConfigurations->IsChecked(i)) continue;
/*
		wxString file = SamGetDefaultFile(mTechList[i], mFinList[i]);

		SymTab tab;
		if (!tab.ReadValues( file, true ))			
		{
			Log("FPERR(r): " + file);
			continue;
		}

		// name=value query - remove whitespace from left and right
		wxString name="";
		wxString value="";
		
		wxStringTokenizer tokenizer(txtVarName->GetValue(), "=");
		if ( tokenizer.HasMoreTokens() ) name = tokenizer.GetNextToken().Trim();
		name = name.Trim(false);
		if ( tokenizer.HasMoreTokens() ) value = tokenizer.GetNextToken().Trim();
		value = value.Trim(false);
 
		VarInfo *v = tab.Lookup( name );
		wxString sval = "<not found>";
		if (v)
		{
			sval = v->ValToString();

			if ( value != "")
			{
				if ( sval == value )
					Log("'" + name + "' in " + mTechList[i] + ", " + mFinList[i] + " = " + sval);
			}
			else
				Log("'" + name + "' in " + mTechList[i] + ", " + mFinList[i] + " = " + sval);
		}
*/
	}
}

void DefaultsManager::ClearLog()
{
	txtOutput->Clear();
}

void DefaultsManager::Log(const wxString &s)
{
	txtOutput->AppendText( s + "\n");
}

wxString DefaultsManager::LookupVariable()
{
/*
	SymTab *syms = DbGet()->GetMasterSymTab();
	Array<VarInfo*> vars = syms->GetVariables(false);
	wxArrayString names, labels;
	for (int i=0;i<vars.count();i++)
	{
		if ( !vars[i]->GetAttr(VATTR_INDICATOR)
				&& vars[i]->GetDataSource() != VDSRC_CALCULATED )
		{
			wxString label = vars[i]->GetLabel();
			if (label.IsEmpty()) label = vars[i]->GetName();

			if (vars[i]->GetContext() == "") label = "Misc no context/" + label;
			else label = vars[i]->GetContext() + "/" + label;

			if (vars[i]->GetUnits() != "") 
				label += " (" + vars[i]->GetUnits() + ")";

			switch(vars[i]->GetType())
			{
			case VAR_INTEGER: label += " (INTEGER)"; break;
			case VAR_INTEGER_ARRAY: label += " (INTEGER ARRAY)"; break;
			case VAR_DOUBLE: label += " (DOUBLE)"; break;
			case VAR_DOUBLE_ARRAY: label += " (DOUBLE ARRAY)"; break;
			case VAR_STRING: label += " (STRING)"; break;
			case VAR_STRING_ARRAY: label += " (STRING ARRAY)"; break;
			}

			labels.Add( label );
			names.Add( vars[i]->GetName() );
		}
	}

	SortByLabels(names, labels);
	SelectVariableDialog dlg( this, "Browse Variables");
	dlg.SetItems( names, labels );
	
	if (dlg.ShowModal() == wxID_OK)
	{
		wxArrayString names = dlg.GetCheckedNames();
		if (names.Count() > 0)
			return names[0];
	}
*/
	return wxEmptyString;
}

void DefaultsManager::OnLookup(wxCommandEvent &evt)
{
	wxString v = LookupVariable();
	if (!v.IsEmpty())
		txtVarName->SetValue( v );
}

void DefaultsManager::OnSelUnselAll(wxCommandEvent &evt)
{
	bool sel = evt.GetId() == ID_btnSelAll;
	for (int i=0;i<(int)cklConfigurations->GetCount();i++)
		cklConfigurations->Check( i, sel );
}

/*user.class.end*/
BEGIN_EVENT_TABLE( DefaultsManagerDialog, wxDialog )
/*user.dialogevents.start*/
EVT_BUTTON( ID_btnClose, DefaultsManagerDialog::OnCommand )
/*user.dialogevents.end*/
EVT_CLOSE(DefaultsManagerDialog::OnClose)

END_EVENT_TABLE()

DefaultsManagerDialog::DefaultsManagerDialog(wxWindow *parent, const wxString &title, void *data)
	 : wxDialog( parent, -1, title 
/*user.dialogconstruct.start*/
/*user.dialogconstruct.end*/
	)
{
	mPanel = new DefaultsManager(this);
	wxSize _sz = mPanel->GetClientSize();
	SetClientSize(_sz.GetWidth(), _sz.GetHeight());
/*user.dialoginit.start*/
/*user.dialoginit.end*/
}

/*user.dialog.start*/
/*user.dialog.end*/

void DefaultsManagerDialog::OnCommand(wxCommandEvent &evt)
{
/*user.oncommand.start*/
	EndModal(wxID_OK);
/*user.oncommand.end*/
}

void DefaultsManagerDialog::OnClose(wxCloseEvent &evt)
{
/*user.onclose.start*/
	EndModal(wxID_OK);
/*user.onclose.end*/
}

/* end of DefaultsManager */

