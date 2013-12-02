#include <wx/dcbuffer.h>
#include <wx/clipbrd.h>
#include <wx/tokenzr.h>
#include <wx/renderer.h>
#include <wx/statline.h>

#include <wex/uiform.h>
#include <wex/extgrid.h>


#include "ptlayoutctrl.h"
#include "materials.h"
#include "troughloop.h"

#include "uiwidgets.h"

/******************************* AFSchedNumeric **********************************/
enum { IDAFSN_NUMERIC=wxID_HIGHEST + 321, IDAFSN_SBUTTON };

BEGIN_EVENT_TABLE(AFSchedNumeric, wxWindow)
	EVT_PAINT( AFSchedNumeric::OnPaint )
	EVT_SIZE( AFSchedNumeric::OnResize )
	EVT_LEFT_DOWN( AFSchedNumeric::OnClick )
	EVT_BUTTON( IDAFSN_SBUTTON, AFSchedNumeric::OnEditSchedule)
	EVT_TEXT_ENTER( IDAFSN_NUMERIC, AFSchedNumeric::OnNumChanged)
END_EVENT_TABLE()

AFSchedNumeric::AFSchedNumeric( wxWindow *parent, int id, const wxPoint &pos, const wxSize &size)
	: wxWindow(parent, id, pos, size, wxCLIP_CHILDREN)
{
	SetBackgroundStyle(wxBG_STYLE_CUSTOM);

	for (double i=0;i<50;i++)
		mSchedValues.push_back( 0 );

	bUseSchedule = false;
	bScheduleOnly = false;

	mFixedValue = new wxNumericCtrl(this, IDAFSN_NUMERIC, 0.0, wxNumericCtrl::REAL, wxPoint(25,0), wxSize( size.GetWidth()-25, size.GetHeight()) );
	mBtnEditSched = new wxButton(this, IDAFSN_SBUTTON, "Edit...", wxPoint(25,0), wxSize( size.GetWidth()-25, size.GetHeight()) );
	mBtnEditSched->Show(false);
}

bool AFSchedNumeric::UseSchedule()
{
	return bUseSchedule;
}
void AFSchedNumeric::UseSchedule(bool b)
{	
	mFixedValue->Show( !b);
	mBtnEditSched->Show( b );
	bUseSchedule = b;
	Refresh();
}

bool AFSchedNumeric::ScheduleOnly()
{
	return bScheduleOnly;
}
void AFSchedNumeric::ScheduleOnly(bool b)
{	
	mFixedValue->Show( !b);
	mBtnEditSched->Show( b );
	bScheduleOnly = b;
	int cw, ch;
	GetClientSize(&cw,&ch);
	if (bScheduleOnly)
	{
		bUseSchedule = true;
		mBtnEditSched->SetSize(0,0,cw,ch);
	}
	else
	{
		mBtnEditSched->SetSize(25,0,cw-25,ch);
	}
	Refresh();
}


double AFSchedNumeric::GetValue()
{
	return mFixedValue->Value();
}
void AFSchedNumeric::SetValue(double d)
{
	mFixedValue->SetValue(d);
}
void AFSchedNumeric::SetFormat( int deci, bool thousep, const wxString &pre, const wxString &post)
{
	mFixedValue->SetFormat( deci, thousep, pre, post );
}

std::vector<double> AFSchedNumeric::GetSchedule()
{
	return mSchedValues;
}

void AFSchedNumeric::GetSchedule( std::vector<float> *vals )
{
	vals->clear();
	for( size_t i=0;i<mSchedValues.size();i++) vals->push_back( (float)mSchedValues[i] );
}
int AFSchedNumeric::GetSchedLen()
{
	return mSchedValues.size();
}
void AFSchedNumeric::SetSchedule(const std::vector<double> &s)
{
	mSchedValues = s;
}

void AFSchedNumeric::SetSchedule( const std::vector<float> &s )
{
	mSchedValues.clear();
	for( size_t i=0;i<s.size();i++ )
		mSchedValues.push_back( (double) s[i] );
}

void AFSchedNumeric::SetSchedLen(int len)
{
	mSchedValues.resize(len, 0);
}

void AFSchedNumeric::OnResize(wxSizeEvent &evt)
{
	int cw, ch;
	GetClientSize(&cw,&ch);
	if (bUseSchedule)
	{
		mBtnEditSched->SetSize(0,0,cw,ch);
	}
	else
	{
		mFixedValue->SetSize(25,0,cw-25,ch);
		mBtnEditSched->SetSize(25,0,cw-25,ch);
	}
}
void AFSchedNumeric::OnPaint(wxPaintEvent &evt)
{
	wxAutoBufferedPaintDC pdc(this);
	if (!bScheduleOnly)
	{
		int cwidth, cheight;
		GetClientSize(&cwidth,&cheight);

		wxFont f = wxFont(4, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, 
		wxFONTWEIGHT_BOLD, false, "arial"); 
		f.SetPointSize(5);

		pdc.SetBrush(wxBrush(*wxLIGHT_GREY));
		pdc.SetPen(wxPen(*wxLIGHT_GREY));
		pdc.DrawRectangle(0, 0, 25, cheight);

		pdc.SetBrush(wxBrush(*wxBLUE));
		pdc.SetPen(wxPen(*wxBLUE));
		pdc.DrawRectangle(0, bUseSchedule?cheight/2:0, 25, cheight/2);

		pdc.SetFont(f);
		pdc.SetTextForeground(*wxWHITE);
		pdc.DrawText(wxT("Value"), 1, 1);
		pdc.DrawText(wxT("Sched"), 1, cheight/2+1);
	}
}
void AFSchedNumeric::OnClick(wxMouseEvent &evt)
{
	SetFocus();
	
	if (!bScheduleOnly)
	{		
		if (evt.GetX() >= 25)
			return;

		UseSchedule( !bUseSchedule );
		FireChangedEvent();
	}
}

void AFSchedNumeric::FireChangedEvent()
{	
	wxCommandEvent enterpress(wxEVT_COMMAND_TEXT_ENTER, this->GetId() );
	enterpress.SetEventObject(this);
	wxString buf;
	for( size_t i=0;i<mSchedValues.size();i++ )
	{
		buf += wxString::Format("%lg", mSchedValues[i] );
		if ( i < mSchedValues.size()-1 ) buf += ',';
	}
	enterpress.SetString( buf );
	GetEventHandler()->ProcessEvent(enterpress);
}

enum {
  ID_btnPasteData = wxID_HIGHEST+144,
  ID_btnCopyData,
  ID_numValueCount };

class SchedNumericDialog : public wxDialog
{
	wxExtGridCtrl *m_grid;
	wxNumericCtrl *m_numVals;

public:
	SchedNumericDialog( wxWindow *parent, const wxString &title, const wxString &label )
		: wxDialog( parent, wxID_ANY, title, wxDefaultPosition, wxSize(470,400), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER )
	{
		m_grid = new wxExtGridCtrl( this, wxID_ANY );
		m_grid->CreateGrid(50,1);
		m_grid->SetColLabelValue( 0, label );
		m_grid->SetColumnWidth( 0, wxGRID_AUTOSIZE );
		m_grid->SetColLabelAlignment( wxALIGN_LEFT, wxALIGN_CENTER );
		
		wxBoxSizer *tools = new wxBoxSizer( wxVERTICAL );
		tools->Add( new wxStaticText( this, wxID_ANY, "# of values:"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
		tools->Add( m_numVals = new wxNumericCtrl( this, ID_numValueCount, 50, wxNumericCtrl::INTEGER ), 0, wxALL, 3 );
		tools->Add( new wxButton( this, ID_btnCopyData, "Copy" ), 0, wxALL, 3 );
		tools->Add( new wxButton( this, ID_btnPasteData, "Paste" ), 0, wxALL, 3 );
		tools->AddStretchSpacer();

		wxBoxSizer *center = new wxBoxSizer( wxHORIZONTAL );
		center->Add( m_grid, 1, wxALL|wxEXPAND, 0 );
		center->Add( tools, 0, wxALL, 5 );

		wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
		sizer->Add( center, 1, wxALL|wxEXPAND, 0 );
		sizer->Add( CreateButtonSizer(wxOK|wxCANCEL), 0, wxALL|wxEXPAND, 5 );
		SetSizer( sizer );
	}

	void OnCopyData(wxCommandEvent &evt)
	{
		if (wxTheClipboard->Open())
		{
			wxString data;
			for (int i=0;i<m_grid->GetNumberRows();i++)
			{
				data += m_grid->GetCellValue(i,0);
				if (i < m_grid->GetNumberRows()-1)
					data += '\n';
			}
			wxTheClipboard->SetData( new wxTextDataObject( data ) );
			wxTheClipboard->Close();
		}
	}

	void OnPasteData(wxCommandEvent &evt)
	{
		if (wxTheClipboard->Open())
		{
			wxString data;
			wxTextDataObject textobj;
			if (wxTheClipboard->GetData( textobj ))
			{
				wxArrayString items = wxStringTokenize(textobj.GetText(), ",\t\n");
				for (int i=0;i<m_grid->GetNumberRows() && i<(int)items.Count();i++)
					m_grid->SetCellValue(i,0,items[i].Trim().Trim(false));
			}

			wxTheClipboard->Close();
		}
	}

	void OnValueCount(wxCommandEvent &evt)
	{
		int nrows = m_numVals->AsInteger();

		if (nrows > 100) nrows = 100;
		if (nrows < 2) nrows = 2;

		m_grid->ResizeGrid(nrows,1);

		for (int i=0;i<nrows;i++)
		{
			double dval;
			m_grid->SetRowLabelValue(i,wxString::Format("%d", i+1));
			if (m_grid->GetCellValue(i,0).ToDouble(&dval))
				m_grid->SetCellValue(i,0,wxString::Format("%lg",dval));
			else
				m_grid->SetCellValue(i,0,"0");
		}

		m_numVals->SetValue( m_grid->GetNumberRows() );
	}

	wxExtGridCtrl *GetGrid() { return m_grid; }
	wxNumericCtrl *GetNumeric() { return m_numVals; }

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE( SchedNumericDialog, wxDialog )
	EVT_BUTTON( ID_btnCopyData, SchedNumericDialog::OnCopyData)
	EVT_BUTTON( ID_btnPasteData, SchedNumericDialog::OnPasteData)
	EVT_TEXT_ENTER( ID_numValueCount, SchedNumericDialog::OnValueCount)
END_EVENT_TABLE()

void AFSchedNumeric::OnEditSchedule(wxCommandEvent &evt)
{
	SchedNumericDialog dlg(this, "Edit Schedule", GetLabel());
	wxExtGridCtrl *grid = dlg.GetGrid();
	size_t i, nrows = mSchedValues.size();

	grid->ResizeGrid( nrows, 1 );
	for (i=0;i<nrows;i++)
	{
		grid->SetRowLabelValue(i,wxString::Format("%d", (int)(i+1)));
		grid->SetCellValue(i,0, wxString::Format("%lg", mSchedValues[i] ));
	}	
	dlg.GetNumeric()->SetValue( grid->GetNumberRows() );

	if (dlg.ShowModal() == wxID_OK)
	{
		mSchedValues.clear();
		nrows = grid->GetNumberRows();
		for (i=0;i<nrows;i++)
			mSchedValues.push_back( wxAtof( grid->GetCellValue(i,0) ));

		FireChangedEvent();
	}
}

void AFSchedNumeric::OnNumChanged(wxCommandEvent &evt)
{
	wxCommandEvent copyevt(evt);
	copyevt.SetEventObject(this);
	copyevt.SetId( this->GetId() );
	GetEventHandler()->ProcessEvent(copyevt);
}




BEGIN_EVENT_TABLE(AFMonthlyFactorCtrl, wxButton)
EVT_BUTTON(wxID_ANY, AFMonthlyFactorCtrl::OnPressed)
END_EVENT_TABLE()
AFMonthlyFactorCtrl::AFMonthlyFactorCtrl( wxWindow *parent, int id, 
	const wxPoint &pos, const wxSize &size)
	: wxButton( parent, id, "Edit values...", pos, size )
{
	for (int i=0;i<12;i++)
		mData[i] = 1.0f;
}

std::vector<float> AFMonthlyFactorCtrl::Get()
{
	std::vector<float> data( 12, 0.0f );
	for (int i=0;i<12;i++)
		data[i] = mData[i];
	return data;
}

void AFMonthlyFactorCtrl::Set( const std::vector<float> &data )
{
	for (size_t i=0;i<12 && i<data.size();i++)
		mData[i] = data[i];
}
void AFMonthlyFactorCtrl::SetDescription(const wxString &s)
{
	mDescription = s;
}
wxString AFMonthlyFactorCtrl::GetDescription()
{
	return mDescription;
}

enum { ID_grid = wxID_HIGHEST, ID_singleVal, ID_applyVal, ID_copy, ID_paste };

class MonthlyFactorDialog : public wxDialog
{
	wxNumericCtrl *numSV;
	wxStaticText *lblDescription;
	wxExtGridCtrl *grdData;
public:	
	MonthlyFactorDialog(wxWindow *parent, const wxString &title )
		: wxDialog( parent, wxID_ANY, title, wxDefaultPosition, wxSize(400, 500), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER )
	{
		lblDescription = new wxStaticText( this, wxID_ANY, "Values" );
		grdData = new wxExtGridCtrl( this, ID_grid );
		grdData->CreateGrid( 12, 1 );
		grdData->DisableDragCell();
		grdData->DisableDragColSize();
		grdData->DisableDragRowSize();
		grdData->DisableDragColMove();
		grdData->DisableDragGridSize();

		grdData->Freeze();
		grdData->SetRowLabelValue(0, "Jan");
		grdData->SetRowLabelValue(1, "Feb");
		grdData->SetRowLabelValue(2, "Mar");
		grdData->SetRowLabelValue(3, "Apr");
		grdData->SetRowLabelValue(4, "May");
		grdData->SetRowLabelValue(5, "Jun");
		grdData->SetRowLabelValue(6, "Jul");
		grdData->SetRowLabelValue(7, "Aug");
		grdData->SetRowLabelValue(8, "Sep");
		grdData->SetRowLabelValue(9, "Oct");
		grdData->SetRowLabelValue(10, "Nov");
		grdData->SetRowLabelValue(11, "Dec");
		grdData->SetColLabelValue(0, "Value");
	
		grdData->EnableEditing(true);
		grdData->EnableCopyPaste(true);

		grdData->SetRowLabelSize( wxGRID_AUTOSIZE );
		grdData->SetColLabelSize( wxGRID_AUTOSIZE );
	
		grdData->Thaw();

		float d[12];
		for (int i=0;i<12;i++) d[i] = 1;

		SetData(d);

		numSV = new wxNumericCtrl( this, ID_singleVal );

		wxBoxSizer *tools = new wxBoxSizer( wxVERTICAL );
		tools->Add( new wxButton( this, ID_copy, "Copy" ), 0, wxALL, 3 );
		tools->Add( new wxButton( this, ID_paste, "Paste" ), 0, wxALL, 3 );
		tools->Add( new wxStaticLine( this ), 0, wxALL|wxEXPAND, 2 );
		tools->Add( new wxStaticText( this, wxID_ANY, "Enter single value:" ), 0, wxALL|wxALIGN_CENTER_VERTICAL|wxALIGN_RIGHT, 5 );
		tools->Add( numSV, 0, wxALL, 3 );
		tools->Add( new wxButton( this, ID_applyVal, "Apply" ), 0, wxALL, 3 );
		tools->AddStretchSpacer();

		wxBoxSizer *center = new wxBoxSizer( wxHORIZONTAL );
		center->Add( grdData, 1, wxALL|wxEXPAND, 4 );
		center->Add( tools, 0, wxALL|wxEXPAND, 4 );

		wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
		sizer->Add( lblDescription, 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
		sizer->Add( center, 1, wxALL|wxEXPAND, 0 );
		sizer->Add( CreateButtonSizer( wxOK|wxCANCEL ), 0, wxALL|wxEXPAND, 10 );

		SetSizer( sizer );
	}

	void OnCopyPaste(wxCommandEvent &evt)
	{
		if (evt.GetId() == ID_paste)
			grdData->Paste(true);
		else
			grdData->Copy(true);
	}
	void SetData(float d[12])
	{
		grdData->Freeze();
		for (int i=0;i<12;i++)
			grdData->SetCellValue( i, 0, wxString::Format("%g", d[i]) );
		grdData->Thaw();
	}

	void GetData(float d[12])
	{
		for (int i=0;i<12;i++)
			d[i] = (float) wxAtof( grdData->GetCellValue(i,0).c_str() );
	}

	void SetDescription(const wxString &data)
	{
		lblDescription->SetLabel(data);
	}

	void OnApplySingleValue(wxCommandEvent &)
	{
		grdData->Freeze();
		for (int i=0;i<12;i++)
			grdData->SetCellValue( i, 0, wxString::Format("%lg", numSV->Value()) );
		grdData->Thaw();
	}

	DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE( MonthlyFactorDialog, wxDialog )
	EVT_BUTTON(ID_copy, MonthlyFactorDialog::OnCopyPaste )
	EVT_BUTTON(ID_paste, MonthlyFactorDialog::OnCopyPaste )
	EVT_BUTTON(ID_applyVal, MonthlyFactorDialog::OnApplySingleValue )
END_EVENT_TABLE()


void AFMonthlyFactorCtrl::OnPressed(wxCommandEvent &evt)
{
	MonthlyFactorDialog dlg(this, "Edit Values");
	
	dlg.SetDescription( mDescription );
	dlg.SetData( mData );

	if (dlg.ShowModal()==wxID_OK)
	{
		dlg.GetData( mData );		
		evt.Skip(); // allow event to propagate indicating underlying value changed
	}
}

class wxUISchedNumericObject : public wxUIObject
{
public:
	wxUISchedNumericObject() {
		AddProperty( "Label", new wxUIProperty("Value") );
		AddProperty( "UseSchedule", new wxUIProperty( true ) );
		AddProperty( "ScheduleOnly", new wxUIProperty( false ) );
		AddProperty( "TabOrder", new wxUIProperty( -1 ) );
	}
	virtual wxString GetTypeName() { return "SchedNumeric"; }
	virtual wxUIObject *Duplicate() { wxUIObject *o = new wxUISchedNumericObject; o->Copy( this ); return o; }
	virtual bool IsNativeObject() { return true; }
	virtual bool DrawDottedOutline() { return false; }
	virtual wxWindow *CreateNative( wxWindow *parent ) {
		AFSchedNumeric *sn = new AFSchedNumeric( parent, wxID_ANY ) ;
		sn->SetLabel( Property("Label").GetString() );
		return AssignNative( sn );
	}
	virtual void OnPropertyChanged( const wxString &id, wxUIProperty *p )
	{
		if ( AFSchedNumeric *sn = GetNative<AFSchedNumeric>() )
		{
			if ( id == "Label" ) sn->SetLabel( p->GetString() );
			else if ( id == "UseSchedule" ) sn->UseSchedule( p->GetBoolean() );
			else if ( id == "ScheduleOnly" ) sn->ScheduleOnly( p->GetBoolean() );
		}
	}
	
	virtual void Draw( wxWindow *win, wxDC &dc, const wxRect &geom )
	{
		if ( Property("ScheduleOnly").GetBoolean() )
		{
			wxRendererNative::Get().DrawPushButton( win, dc, geom );
			dc.SetFont( *wxNORMAL_FONT );
			dc.SetTextForeground( *wxBLACK );
			wxString label( "Edit..." );
			int x, y;
			dc.GetTextExtent( label, &x, &y );
			dc.DrawText( label, geom.x + geom.width/2-x/2, geom.y+geom.height/2-y/2 );
		}
		else
		{
			dc.SetBrush( wxBrush( *wxBLUE ) );
			dc.DrawRectangle( geom.x, geom.y+geom.height/2, 25, geom.height/2 );
			dc.SetPen( *wxLIGHT_GREY_PEN );
			dc.SetBrush( wxBrush( *wxWHITE ) );
			dc.DrawRectangle( geom.x+25, geom.y, geom.width-25, geom.height );
			dc.SetFont( *wxNORMAL_FONT );
			dc.SetTextForeground( *wxBLACK );
			wxString text = Property("Text").GetString();
			int x, y;
			dc.GetTextExtent( text, &x, &y );
			dc.DrawText( text, geom.x+27, geom.y+geom.height/2-y/2 );
		}
	}

	virtual void OnNativeEvent()
	{
		/* nothing to do here ... */
	}
};

class wxUIMonthlyFactorObject : public wxUIObject
{
public:
	wxUIMonthlyFactorObject() {
		AddProperty("Description", new wxUIProperty( wxString("Values") ) );
		AddProperty("TabOrder", new wxUIProperty( (int)-1 ) );
	}
	virtual wxString GetTypeName() { return "MonthlyFactor"; }
	virtual wxUIObject *Duplicate() { wxUIObject *o = new wxUIMonthlyFactorObject; o->Copy( this ); return o; }
	virtual bool IsNativeObject() { return true; }
	virtual bool DrawDottedOutline() { return false; }
	virtual wxWindow *CreateNative( wxWindow *parent ) {
		AFMonthlyFactorCtrl *mf = new AFMonthlyFactorCtrl( parent, wxID_ANY );
		mf->SetDescription( Property("Description").GetString() );
		return AssignNative( mf );
	}
	virtual void OnPropertyChanged( const wxString &id, wxUIProperty *p )
	{
		if ( AFMonthlyFactorCtrl *pt = GetNative<AFMonthlyFactorCtrl>() )
			if ( id == "Description" ) pt->SetDescription( p->GetString() );
	}
	virtual void Draw( wxWindow *win, wxDC &dc, const wxRect &geom )
	{
		wxRendererNative::Get().DrawPushButton( win, dc, geom );
		dc.SetFont( *wxNORMAL_FONT );
		dc.SetTextForeground( *wxBLACK );
		wxString label("Edit values...");
		int x, y;
		dc.GetTextExtent( label, &x, &y );
		dc.DrawText( label, geom.x + geom.width/2-x/2, geom.y+geom.height/2-y/2 );
	}
};


class wxUIPTLayoutObject : public wxUIObject
{
public:
	wxUIPTLayoutObject() {
		AddProperty("EnableSpan", new wxUIProperty( false ) );
		Property("Width").Set( 650 );
		Property("Height").Set( 300 );
	}
	virtual wxString GetTypeName() { return "PTLayout"; }
	virtual wxUIObject *Duplicate() { wxUIObject *o = new wxUIPTLayoutObject; o->Copy( this ); return o; }
	virtual bool IsNativeObject() { return true; }
	virtual bool DrawDottedOutline() { return false; }
	virtual wxWindow *CreateNative( wxWindow *parent ) {		
		PTLayoutCtrl *pt = new PTLayoutCtrl( parent, wxID_ANY );
		pt->EnableSpanAngle( Property("EnableSpan").GetBoolean() );
		return AssignNative( pt );
	}
	virtual void OnPropertyChanged( const wxString &id, wxUIProperty *p )
	{
		if ( PTLayoutCtrl *pt = GetNative<PTLayoutCtrl>() )
			if ( id == "EnableSpan" ) pt->EnableSpanAngle( p->GetBoolean() );
	}
};

class wxUIMatPropObject : public wxUIObject 
{
public:
	wxUIMatPropObject() {
		AddProperty("TabOrder", new wxUIProperty( (int)-1 ) );
	}
	virtual wxString GetTypeName() { return "MaterialProperties"; }
	virtual wxUIObject *Duplicate() { wxUIObject *o = new wxUIMatPropObject; o->Copy( this ); return o; }
	virtual bool IsNativeObject() { return true; }
	virtual bool DrawDottedOutline() { return false; }
	virtual wxWindow *CreateNative( wxWindow *parent ) {
		return AssignNative( new MatPropCtrl( parent, wxID_ANY ) );
	}
	virtual void Draw( wxWindow *win, wxDC &dc, const wxRect &geom )
	{
		wxRendererNative::Get().DrawPushButton( win, dc, geom );
		dc.SetFont( *wxNORMAL_FONT );
		dc.SetTextForeground( *wxBLACK );
		wxString label("Edit...");
		int x, y;
		dc.GetTextExtent( label, &x, &y );
		dc.DrawText( label, geom.x + geom.width/2-x/2, geom.y+geom.height/2-y/2 );
	}
};

class wxUITroughLoopObject : public wxUIObject
{
public:
	wxUITroughLoopObject() {
		Property("Width").Set( 650 );
		Property("Height").Set( 300 );
	}
	virtual wxString GetTypeName() { return "TroughLoop"; }
	virtual wxUIObject *Duplicate() { wxUIObject *o = new wxUITroughLoopObject; o->Copy( this ); return o; }
	virtual bool IsNativeObject() { return true; }
	virtual bool DrawDottedOutline() { return false; }
	virtual wxWindow *CreateNative( wxWindow *parent ) {		
		return AssignNative( new TRLoopCtrl( parent, wxID_ANY ) );
	}
};

class wxUIDividerObject : public wxUIObject
{
public:
	wxUIDividerObject() {
		AddProperty( "Orientation", new wxUIProperty( 0, "Horizontal,Vertical" ) );
		AddProperty( "Colour", new wxUIProperty( wxColour(120,120,120) ) );
		AddProperty( "Label", new wxUIProperty( wxString("") ) );
		AddProperty( "Bold", new wxUIProperty( true ) );
		Property("Width").Set( 300 );
		Property("Height").Set( 16 );
	}
	virtual wxString GetTypeName() { return "Divider"; }
	virtual wxUIObject *Duplicate() { wxUIObject *o = new wxUIDividerObject; o->Copy( this ); return o; }
	virtual bool IsNativeObject() { return false; }
	virtual bool DrawDottedOutline() { return false; }
	virtual void Draw( wxWindow *win, wxDC &dc, const wxRect &geom )
	{
		dc.SetPen( wxPen( Property("Colour").GetColour() ) );
		bool horiz = (Property("Orientation").GetInteger() == 0);
		if ( horiz ) dc.DrawLine( geom.x, geom.y+7, geom.x+geom.width, geom.y+7 );
		else dc.DrawLine( geom.x+4, geom.y, geom.x+4, geom.y+geom.height );
			
		wxString label = Property("Label").GetString();
		if ( !label.IsEmpty() )
		{
			wxFont font( *wxNORMAL_FONT );
			if ( Property("Bold").GetBoolean() ) font.SetWeight( wxFONTWEIGHT_BOLD );
			dc.SetFont( font );
			if ( horiz )
			{
				int x,y;
				dc.GetTextExtent( label, &x, &y );
				dc.SetBrush( wxBrush( win->GetBackgroundColour() ) );
				dc.SetPen( *wxTRANSPARENT_PEN );
				dc.DrawRectangle(geom.x+5, geom.y, x+2, y+1);
			}
			dc.SetTextForeground(wxSystemSettings::GetColour(wxSYS_COLOUR_BTNTEXT));
			dc.DrawText( label, geom.x+6, geom.y );
		}
	}
};

void RegisterUIWidgetsForSAM()
{
	wxUIObjectTypeProvider::Register( new wxUISchedNumericObject );
	wxUIObjectTypeProvider::Register( new wxUIPTLayoutObject );
	wxUIObjectTypeProvider::Register( new wxUIMatPropObject );
	wxUIObjectTypeProvider::Register( new wxUITroughLoopObject );
	wxUIObjectTypeProvider::Register( new wxUIMonthlyFactorObject );
	wxUIObjectTypeProvider::Register( new wxUIDividerObject );

/* TODO LIST 
{ GUI_ADD_CONTROL_ID+22, "PlotSurface",      "WPPlotSurface2D",        plotsurface_xpm, CTRL_NATIVE,  10, 13, 300, 300,  props_plotsurface,     NULL,       objinit_plotsurface,    objfree_plotsurface,    nativesetprop_plotsurface,    paint_plotsurface,    iswithin_default,   NULL,                     NULL,                     NULL,                     false  },
{ GUI_ADD_CONTROL_ID+26, "DataGridBtn",      "DataGridButton",         datagridbtn_xpm, CTRL_NATIVE,  10, 15, 100, 21,   props_datagridbutton,  NULL,       objinit_datagridbutton, objfree_datagridbutton, nativesetprop_datagridbutton, paint_datagridbutton, iswithin_default,   nativeevt_datagridbutton, vartoctrl_datagridbutton, ctrltovar_datagridbutton, false },
{ GUI_ADD_CONTROL_ID+28, "DataArrayButton",  "DataArrayButton",        dataarraybtn_xpm,CTRL_NATIVE,  10, 15, 110, 21,   props_dataarraybutton, NULL,       objinit_dataarraybutton,objfree_dataarraybutton,nativesetprop_dataarraybutton,paint_dataarraybutton,iswithin_default,   nativeevt_dataarraybutton,vartoctrl_dataarraybutton,ctrltovar_dataarraybutton,false },
{ GUI_ADD_CONTROL_ID+29, "DoubleMatrixCtrl", "DoubleMatrixCtrl",       dblmatctrl_xpm,  CTRL_NATIVE,  10, 15, 800, 400,  props_dblmatctrl,      NULL,       objinit_dblmatctrl,     objfree_dblmatctrl,     nativesetprop_dblmatctrl,     paint_dblmatctrl,     iswithin_default,   nativeevt_dblmatctrl,     vartoctrl_dblmatctrl,     ctrltovar_dblmatctrl,     false },
{ GUI_ADD_CONTROL_ID+30, "HLine",            "HLine",                  hline_xpm,       CTRL_CUSTOM,  10, 15, 250, 21,   props_hline,           NULL,       objinit_hline,          NULL,                   NULL,                         paint_hline,          iswithin_default,   NULL,                     NULL,                     NULL,                     false },
{ GUI_ADD_CONTROL_ID+32, "SearchListBox",    "AFSearchListBox",        searchlb_xpm,    CTRL_NATIVE,  10, 15, 110, 120,  props_searchlb,        NULL,       objinit_searchlb,       objfree_searchlb,       nativesetprop_searchlb,       paint_searchlb,       iswithin_default,   nativeevt_searchlb,       vartoctrl_searchlb,       ctrltovar_searchlb,       false },
{ GUI_ADD_CONTROL_ID+34, "ShadingButton",    "ShadingButtonCtrl",      shadingctrl_xpm, CTRL_NATIVE,  10, 15, 110, 21,   props_shadbtn,        NULL,        objinit_shadbtn,        objfree_shadbtn,        nativesetprop_shadbtn,        paint_shadbtn,        iswithin_default,   nativeevt_shadbtn,        vartoctrl_shadbtn,        ctrltovar_shadbtn,       false },
{ GUI_ADD_CONTROL_ID+35, "DoubleMatrixSideButtonsCtrl", "DoubleMatrixSideButtonsCtrl",       dblmatctrl_xpm,  CTRL_NATIVE,  10, 15, 800, 400,  props_dblmatsbctrl,      NULL,       objinit_dblmatsbctrl,     objfree_dblmatsbctrl,     nativesetprop_dblmatsbctrl,     paint_dblmatsbctrl,     iswithin_default,   nativeevt_dblmatsbctrl,     vartoctrl_dblmatsbctrl,     ctrltovar_dblmatsbctrl,     false },

// not ported??
{ GUI_ADD_CONTROL_ID+27, "MonthlySchedule",  "AFMonthlyScheduleCtrl",  monsched_xpm,    CTRL_NATIVE,  10, 15, 110, 21,   props_monthlyschedule, NULL,       objinit_monthlyschedule,objfree_monthlyschedule,nativesetprop_monthlyschedule,paint_monthlyschedule,iswithin_default,   nativeevt_monthlyschedule,vartoctrl_monthlyschedule,ctrltovar_monthlyschedule,false },
{ GUI_ADD_CONTROL_ID+21, "ShadingCtrl",      "ShadingCtrl",            shadingctrl_xpm, CTRL_NATIVE,  10, 15, 550, 300,  props_shadingctrl,     NULL,       objinit_shadingctrl,    objfree_shadingctrl,    nativesetprop_shadingctrl,    paint_shadingctrl,    iswithin_default,   nativeevt_shadingctrl,    vartoctrl_shadingctrl,    ctrltovar_shadingctrl,    false  },
{ GUI_ADD_CONTROL_ID+15, "DataGrid",         "WFGridCtrl",             datagrid_xpm,    CTRL_NATIVE,  10, 10, 350, 180,  props_datagrid,        NULL,       objinit_datagrid,       objfree_datagrid,       nativesetprop_datagrid,       paint_datagrid,       iswithin_default,   NULL,                     NULL,                     NULL,                     false  },
{ GUI_ADD_CONTROL_ID+17, "Gauge",            "wxGauge",                gauge_xpm,       CTRL_NATIVE,  10, 15, 110, 21,   props_gauge,           NULL,       objinit_gauge,          objfree_gauge,          nativesetprop_gauge,          paint_gauge,          iswithin_default,   NULL,                     vartoctrl_gauge,          ctrltovar_gauge,          false  },
{ GUI_ADD_CONTROL_ID+18, "TreeView",         "AFTreeView",             treeview_xpm,    CTRL_NATIVE,  10, 15, 110, 120,  props_treeview,        NULL,       objinit_treeview,       objfree_treeview,       nativesetprop_treeview,       paint_treeview,       iswithin_default,   NULL,                     NULL,                     NULL,                     false  },
{ GUI_ADD_CONTROL_ID+19, "HourlyData",       "AFHourlyDataCtrl",       hourlydata_xpm,  CTRL_NATIVE,  10, 15, 150, 33,   props_hourlydata,      NULL,       objinit_hourlydata,     objfree_hourlydata,     nativesetprop_hourlydata,     paint_hourlydata,     iswithin_default,   nativeevt_hourlydata,     vartoctrl_hourlydata,     ctrltovar_hourlydata,     false  },
{ GUI_ADD_CONTROL_ID+12, "Editor",           "wxTextCtrl",             editor_xpm,      CTRL_NATIVE,  10, 15, 120, 120,  props_editor,          NULL,       objinit_editor,         objfree_editor,         nativesetprop_editor,         paint_editor,         iswithin_default,   NULL,                     NULL,                     NULL,                     false  },  
{ GUI_ADD_CONTROL_ID+33, "TextEntryML",        "AFTextCtrl",             textctrl_xpm,    CTRL_NATIVE,  10, 15, 100, 21,   props_textentry,       NULL,       objinit_textentryml,      objfree_textentry,      nativesetprop_textentry,      paint_textentry,      iswithin_default,   nativeevt_textentry,      vartoctrl_textentry,      ctrltovar_textentry,      false  },

*/
}