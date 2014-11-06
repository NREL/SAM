#include "numericvareditform.h"
#include "main.h"


enum {
  ID_GroupBox1=6108,
  ID_GroupBox2=6109,
  ID_cmdCancel=6110,
  ID_btnHelp=6111,
  ID_cmdOk=6112,
  ID_cmdUpdateValues=6113,
  ID_lblNotification=6114,
  ID_cmdMoveDown=6115,
  ID_lstValues=6116,
  ID_cmdRemove=6117,
  ID_cmdAddBefore=6118,
  ID_cmdAddAfter=6119,
  ID_cmdMoveUp=6120,
  ID_Label3=6121,
  ID_Label2=6122,
  ID_Label1=6123,
  ID_numIncr=6124,
  ID_numEnd=6125,
  ID_numStart=6126 };

BEGIN_EVENT_TABLE( NumericVarEditForm, wxPanel )
EVT_BUTTON( ID_cmdAddAfter, NumericVarEditForm::OnCommand)
EVT_BUTTON( ID_cmdAddBefore, NumericVarEditForm::OnCommand)
EVT_BUTTON( ID_cmdMoveUp, NumericVarEditForm::OnCommand)
EVT_BUTTON( ID_cmdMoveDown, NumericVarEditForm::OnCommand)
EVT_BUTTON( ID_cmdRemove, NumericVarEditForm::OnCommand)
EVT_BUTTON( ID_cmdUpdateValues, NumericVarEditForm::OnCommand)
EVT_TEXT_ENTER( ID_numStart, NumericVarEditForm::OnCommand)
EVT_TEXT_ENTER( ID_numEnd, NumericVarEditForm::OnCommand)
EVT_TEXT_ENTER( ID_numIncr, NumericVarEditForm::OnCommand)
EVT_LISTBOX_DCLICK( ID_lstValues, NumericVarEditForm::OnCommand)
END_EVENT_TABLE()

NumericVarEditForm::NumericVarEditForm(wxWindow *parent, int id)
	 : wxPanel( parent, id )
{
	SetClientSize( 486, 323 );
	GroupBox1 = new wxStaticBox(this, ID_GroupBox1, "Define Range", wxPoint(246,9), wxSize(230,167));
	GroupBox2 = new wxStaticBox(this, ID_GroupBox2, "Variable Values", wxPoint(9,9), wxSize(224,272));
	numStart = new wxNumericCtrl(this, ID_numStart, 0, wxNumericCtrl::REAL, wxPoint(366,39), wxSize(100,21));
	numEnd = new wxNumericCtrl(this, ID_numEnd, 0, wxNumericCtrl::REAL, wxPoint(366, 66), wxSize(100, 21));
	numIncr = new wxNumericCtrl(this, ID_numIncr, 0, wxNumericCtrl::REAL, wxPoint(366, 93), wxSize(100, 21));
	cmdMoveUp = new wxButton(this, ID_cmdMoveUp, "Up", wxPoint(18,246), wxSize(56,21));
	cmdAddAfter = new wxButton(this, ID_cmdAddAfter, "Add After...", wxPoint(18,219), wxSize(98,21));
	cmdAddBefore = new wxButton(this, ID_cmdAddBefore, "Add Before...", wxPoint(123,219), wxSize(98,21));
	cmdRemove = new wxButton(this, ID_cmdRemove, "Remove", wxPoint(144,246), wxSize(77,21));
	wxArrayString _data_lstValues;
	lstValues = new wxListBox(this, ID_lstValues, wxPoint(18,27), wxSize(203,186), _data_lstValues, wxLB_SINGLE);
	cmdMoveDown = new wxButton(this, ID_cmdMoveDown, "Down", wxPoint(81,246), wxSize(56,21));
	cmdUpdateValues = new wxButton(this, ID_cmdUpdateValues, "Update", wxPoint(372,123), wxSize(89,21));
	cmdOk = new wxButton(this, ID_cmdOk, "OK", wxPoint(315,294), wxSize(80,21));
	btnHelp = new wxButton(this, ID_btnHelp, "Help...", wxPoint(9,294), wxSize(80,21));
	cmdCancel = new wxButton(this, ID_cmdCancel, "Cancel", wxPoint(399,294), wxSize(80,21));
	lblNotification = new wxStaticText(this, ID_lblNotification, "", wxPoint(252,147), wxSize(218,21));
	Label3 = new wxStaticText(this, ID_Label3, "Increment:", wxPoint(252,93), wxSize(110,21));
	Label2 = new wxStaticText(this, ID_Label2, "End Value:", wxPoint(252,66), wxSize(110,21));
	Label1 = new wxStaticText(this, ID_Label1, "Start Value:", wxPoint(252,39), wxSize(110,21));

	bIntOnly = false;
	cmdOk->SetDefault();
}

void NumericVarEditForm::SetValues(const wxArrayString &values, bool int_only)
{
	bIntOnly = int_only;

	lstValues->Clear();
	lstValues->Freeze();
	
	double min = 1e100, max=-1e100;

	for (int i=0;i<(int)values.Count();i++)
	{
		if (bIntOnly)
		{
			long x=0;
			values[i].ToLong(&x);
			lstValues->Append( wxString::Format("%d", x ));
			if ( x < min)
				min = x;
			if (x > max)
				max = x;
		}
		else
		{
			double x=0;
			values[i].ToDouble(&x);
			lstValues->Append( wxString::Format("%lg", x));
			if ( x < min)
				min = x;
			if (x > max)
				max = x;
		}
	}
	
	lstValues->Thaw();

	numStart->SetValue(min);
	numEnd->SetValue(max);
	if (values.Count() > 1)
		numIncr->SetValue((max-min)/(double)(values.Count()-1));
	CheckRanges();
}

void NumericVarEditForm::OnCommand(wxCommandEvent &evt)
{
	int nsel = 0;
	switch(evt.GetId())
	{
	case ID_lstValues:
		{
			nsel = lstValues->GetSelection();
			if (nsel < 0)
				return;

			wxString item = lstValues->GetString(nsel);
			item = wxGetTextFromUser("Change value:", "Edit", item);
			if (item != wxEmptyString)
			{
				if (bIntOnly) lstValues->SetString(nsel, wxString::Format("%d", (atoi(item.c_str()))));
				else lstValues->SetString(nsel, wxString::Format("%lg",( atof( item.c_str() ) ) ));
			}

		}
		break;
	case ID_cmdAddAfter:
	case ID_cmdAddBefore:
		{
			nsel = lstValues->GetSelection();
			if (nsel < 0)
				nsel = 0;

			wxString item = "1.0";
			if (nsel < (int) lstValues->GetCount())
				item = lstValues->GetString( nsel );

			wxString str = wxGetTextFromUser("Enter new value:", 
				evt.GetId() == ID_cmdAddAfter ? "Add Number After Selection" : "Add Number Before Selection",
				item);
			if (str != wxEmptyString)
			{
				int idxincr = (evt.GetId() == ID_cmdAddAfter && lstValues->GetCount()>0 )? 1 : 0;
				
				if (bIntOnly) lstValues->Insert(wxString::Format("%d", (atoi(str.c_str()))), nsel + idxincr);
				else lstValues->Insert(wxString::Format("%lg", (atof(str.c_str()))), nsel + idxincr);
			}
		}

		break;

	case ID_cmdRemove:
		if ( (nsel=lstValues->GetSelection()) >= 0)
		{
			lstValues->Delete( lstValues->GetSelection() );
			if (nsel >= (int)lstValues->GetCount())
				nsel = lstValues->GetCount() - 1;
			if (nsel >= 0)
				lstValues->SetSelection(nsel);
		}
		break;
	case ID_cmdMoveUp:
		if (lstValues->GetCount() >= 2)
		{
			int isel = lstValues->GetSelection();
			if (isel >= 1)
			{
				wxString tmp = lstValues->GetString( isel - 1 );
				lstValues->SetString(isel - 1, lstValues->GetString(isel) );
				lstValues->SetString(isel, tmp);
				lstValues->SetSelection( isel - 1 );
			}
		}
		break;

	case ID_cmdMoveDown:
		if (lstValues->GetCount() >= 2)
		{
			int isel = lstValues->GetSelection();
			if (isel <= (int)lstValues->GetCount() - 2 && isel >= 0)
			{
				wxString tmp = lstValues->GetString( isel + 1 );
				lstValues->SetString(isel + 1, lstValues->GetString(isel) );
				lstValues->SetString(isel, tmp);
				lstValues->SetSelection( isel + 1 );
			}
		}
		break;

	case ID_cmdUpdateValues:
	case ID_numStart:
	case ID_numEnd:
	case ID_numIncr:
		GenerateValues();
		break;
	}
}

bool NumericVarEditForm::CheckRanges()
{
	double start, end, incr;

	start = numStart->Value();
	end = numEnd->Value();
	incr = numIncr->Value();

	lblNotification->SetLabel("");

	if (incr == 0)
		lblNotification->SetLabel("Increment is 0.");

	if (end <= start && incr > 0)
		lblNotification->SetLabel("End < Start & Incr > 0");

	if (start < end && incr < 0)
		lblNotification->SetLabel("Start < End & Incr < 0");

	lblNotification->Refresh();

	return lblNotification->GetLabel() == "";
}

void NumericVarEditForm::GenerateValues()
{
	if (!CheckRanges())
		return;

	double start, end, incr;

	start = numStart->Value();
	end = numEnd->Value();
	incr = numIncr->Value();

	double curval = start;
	wxString endvalstr = bIntOnly?
		wxString::Format("%d", (int) end ) :
		wxString::Format("%lg", end );

#define NMAXVALS 200

	wxArrayString vals;
	int nadded = 0;
	while ( (incr > 0 && curval <= end)
			|| (incr < 0 && curval >= end ) )
	{
		if (vals.Index( endvalstr ) >= 0)
			break;

		if (nadded > NMAXVALS)
			break;

		nadded++;

		if (incr > 0 && curval > 1.001*end)
			break;

		if (incr < 0 && curval < 0.999*end)
			break;

		vals.Add( bIntOnly?
			wxString::Format("%d", (int) curval ) :
			wxString::Format("%lg", curval ) );
			
		curval += incr;
	}

	if (vals.Index(endvalstr) < 0 && nadded <= NMAXVALS)
		vals.Add(endvalstr);


	lstValues->Freeze();
	lstValues->Clear();
	lstValues->Append(vals);
	lstValues->Thaw();
}

/*user.class.end*/
BEGIN_EVENT_TABLE( NumericVarEditFormDialog, wxDialog )
/*user.dialogevents.start*/

EVT_BUTTON(ID_cmdOk, NumericVarEditFormDialog::OnCommand)
EVT_BUTTON(ID_cmdCancel, NumericVarEditFormDialog::OnCommand)
EVT_BUTTON(ID_btnHelp, NumericVarEditFormDialog::OnCommand)
/*user.dialogevents.end*/
EVT_CLOSE(NumericVarEditFormDialog::OnClose)

END_EVENT_TABLE()

NumericVarEditFormDialog::NumericVarEditFormDialog(wxWindow *parent, const wxString &title, void *data)
	 : wxDialog( parent, -1, title 
	)
{
	mPanel = new NumericVarEditForm(this);
	wxSize _sz = mPanel->GetClientSize();
	SetClientSize(_sz.GetWidth(), _sz.GetHeight());

	wxAcceleratorEntry entries[1];
	entries[0].Set( ::wxACCEL_NORMAL, WXK_F1, ID_btnHelp );
	SetAcceleratorTable( wxAcceleratorTable(1,entries) );
	SetEscapeId(ID_cmdCancel);
}


void NumericVarEditFormDialog::OnCommand(wxCommandEvent &evt)
{

	if (evt.GetId() == ID_cmdOk)
		EndModal( wxID_OK );
	else if (evt.GetId() == ID_btnHelp)
		SamApp::ShowHelp("Numeric Edit");
	else
		EndModal( wxID_CANCEL );
}

void NumericVarEditFormDialog::OnClose(wxCloseEvent &evt)
{
	EndModal(wxID_CANCEL);
}


