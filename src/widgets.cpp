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

#include <wx/dcbuffer.h>
#include <wx/clipbrd.h>
#include <wx/tokenzr.h>
#include <wx/renderer.h>
#include <wx/statline.h>
#include <wx/arrstr.h>

#include <wex/uiform.h>
#include <wex/extgrid.h>
#include <wex/plot/plplotctrl.h>
#include <wex/csv.h>
#include <wex/utils.h>
#include <wex/extgrid.h>
#include <wex/plot/plcolourmap.h>

#ifndef S3D_STANDALONE
#include "main.h"
#endif

#include "widgets.h"
#include "../resource/info24.cpng"

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
	m_switchWidth = 26;
	UpdateSwitchWidth();

	m_fixedLen = -1;
	SetBackgroundStyle(wxBG_STYLE_CUSTOM);

	for (double i=0;i<50;i++)
		mSchedValues.push_back( 0 );

	bUseSchedule = false;
	bScheduleOnly = false;

	mFixedValue = new wxNumericCtrl(this, IDAFSN_NUMERIC, 0.0, wxNUMERIC_REAL, wxPoint(m_switchWidth,0), wxSize( size.GetWidth()-m_switchWidth, size.GetHeight()) );
	mBtnEditSched = new wxButton(this, IDAFSN_SBUTTON, "Edit...", wxPoint(m_switchWidth,0), wxSize( size.GetWidth()-m_switchWidth, size.GetHeight()) );
	mBtnEditSched->Show(false);
}

bool AFSchedNumeric::UseSchedule()
{
	return bUseSchedule;
}
void AFSchedNumeric::SetFixedLen( int len )
{
	m_fixedLen = len;
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
		mBtnEditSched->SetSize(m_switchWidth,0,cw-m_switchWidth,ch);
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

void AFSchedNumeric::GetSchedule( std::vector<double> *vals )
{
	vals->clear();
	for( size_t i=0;i<mSchedValues.size();i++) vals->push_back( (double)mSchedValues[i] );
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

#define SCHEDNUM_FONT wxFont( 5, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_BOLD, false, "arial" )


void AFSchedNumeric::OnResize(wxSizeEvent &)
{
	int cw, ch;
	GetClientSize(&cw,&ch);
	if (bScheduleOnly)
	{
		mBtnEditSched->SetSize(0,0,cw,ch);
	}
	else
	{
		mFixedValue->SetSize(m_switchWidth,0,cw-m_switchWidth,ch);
		mBtnEditSched->SetSize(m_switchWidth,0,cw-m_switchWidth,ch);
	}
}

void AFSchedNumeric::UpdateSwitchWidth()
{
	wxClientDC pdc(this);
	pdc.SetFont( SCHEDNUM_FONT );
	wxSize sz1( pdc.GetTextExtent( "Sched" ) );
	wxSize sz2( pdc.GetTextExtent( "Value" ) );
	m_switchWidth = 2 + ( sz1.x > sz2.x ? sz1.x : sz2.x );
}

void AFSchedNumeric::OnPaint(wxPaintEvent &)
{
	wxAutoBufferedPaintDC pdc(this);
	if (!bScheduleOnly)
	{
		pdc.SetFont( SCHEDNUM_FONT );
		wxSize sz1( pdc.GetTextExtent( "Sched" ) );
		wxSize sz2( pdc.GetTextExtent( "Value" ) );
		m_switchWidth = 2 + ( sz1.x > sz2.x ? sz1.x : sz2.x );

		int cwidth, cheight;
		GetClientSize(&cwidth,&cheight);

		pdc.SetBrush(wxBrush(*wxLIGHT_GREY));
		pdc.SetPen(wxPen(*wxLIGHT_GREY));
		pdc.DrawRectangle(0, 0, m_switchWidth, cheight);

		pdc.SetBrush(wxBrush(*wxBLUE));
		pdc.SetPen(wxPen(*wxBLUE));
		pdc.DrawRectangle(0, bUseSchedule?cheight/2:0, m_switchWidth, cheight/2);

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
		if (evt.GetX() >= m_switchWidth)
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
	SchedNumericDialog( wxWindow *parent, const wxString &title, const wxString &label, const wxString &desc, bool with_resize_options )
		: wxDialog( parent, wxID_ANY, title, wxDefaultPosition, wxScaleSize(470,400), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER )
	{
		m_grid = new wxExtGridCtrl( this, wxID_ANY );
		m_grid->CreateGrid( 50, 1);
		m_grid->SetColLabelValue( 0, label );
		m_grid->SetColSize( 0, wxGRID_AUTOSIZE );
		m_grid->SetColLabelAlignment( wxALIGN_LEFT, wxALIGN_CENTER );
		
		wxBoxSizer *tools = new wxBoxSizer( wxVERTICAL );
		m_numVals = 0;
		if ( with_resize_options )
		{
			tools->Add( new wxStaticText( this, wxID_ANY, "Number of values:"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
			tools->Add( m_numVals = new wxNumericCtrl( this, ID_numValueCount, 50, wxNUMERIC_INTEGER ), 0, wxALL, 3 );
		}
		
		tools->Add( new wxButton( this, ID_btnCopyData, "Copy" ), 0, wxALL, 3 );
		tools->Add( new wxButton( this, ID_btnPasteData, "Paste" ), 0, wxALL, 3 );
		tools->AddStretchSpacer();

		wxBoxSizer *center = new wxBoxSizer( wxHORIZONTAL );
		center->Add( m_grid, 1, wxALL|wxEXPAND, 0 );
		center->Add( tools, 0, wxALL, 5 );

		wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
		sizer->Add( center, 1, wxALL|wxEXPAND, 0 );
		if ( !desc.IsEmpty() )
		{
			wxStaticText *st = new wxStaticText( this, wxID_ANY, desc );
			st->Wrap(400);
			sizer->Add( st, 0, wxALL|wxCENTER, 5 );
		}

		sizer->Add( CreateButtonSizer( wxOK|wxCANCEL|wxHELP ), 0, wxALL|wxEXPAND, 5 );
		SetSizer( sizer );
	}

	void OnCopyData(wxCommandEvent &)
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

	void OnPasteData(wxCommandEvent &)
	{
		if (wxTheClipboard->Open())
		{
			wxString data;
			wxTextDataObject textobj;
			if (wxTheClipboard->GetData( textobj ))
			{
				wxArrayString items = wxStringTokenize(textobj.GetText(), ",\t\n");
				for (int i=0;i<m_grid->GetNumberRows() && i<(int)items.size();i++)
					m_grid->SetCellValue(i,0,items[i].Trim().Trim(false));
			}

			wxTheClipboard->Close();
		}
	}

	void OnValueCount(wxCommandEvent &)
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

	void OnHelp( wxCommandEvent & )
	{
#ifndef S3D_STANDALONE
		SamApp::ShowHelp( "edit_schedule" );
#endif
	}

	wxExtGridCtrl *GetGrid() { return m_grid; }
	wxNumericCtrl *GetNumeric() { return m_numVals; }

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE( SchedNumericDialog, wxDialog )
	EVT_BUTTON( ID_btnCopyData, SchedNumericDialog::OnCopyData)
	EVT_BUTTON( ID_btnPasteData, SchedNumericDialog::OnPasteData)
	EVT_TEXT_ENTER( ID_numValueCount, SchedNumericDialog::OnValueCount)
	EVT_BUTTON( wxID_HELP, SchedNumericDialog::OnHelp )
END_EVENT_TABLE()

void AFSchedNumeric::OnEditSchedule(wxCommandEvent &)
{
	SchedNumericDialog dlg(this, "Edit Schedule", GetLabel(), GetDescription(), m_fixedLen < 1 );
	wxExtGridCtrl *grid = dlg.GetGrid();

	if ( m_fixedLen > 0 )
		mSchedValues.resize( m_fixedLen );
	else
		dlg.GetNumeric()->SetValue( mSchedValues.size() );
	
	size_t i, nrows = mSchedValues.size();
	grid->ResizeGrid( nrows, 1 );
	for (i=0;i<nrows;i++)
	{
		grid->SetRowLabelValue(i,wxString::Format("%d", (int)(i+1)));
		grid->SetCellValue(i,0, wxString::Format("%lg", mSchedValues[i] ));
	}	


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

class AFTableDataDialog : public wxDialog
{
	wxExtGridCtrl *m_grid;

public:
	AFTableDataDialog( wxWindow *parent, const wxString &title )
		: wxDialog( parent, wxID_ANY, title, wxDefaultPosition, wxScaleSize(500, 300), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER )
	{
		m_grid = new wxExtGridCtrl( this, wxID_ANY );
		m_grid->CreateGrid( 10, 2 );
		m_grid->SetRowLabelSize( 0 );
		m_grid->SetColLabelValue( 0, "Name" );
		m_grid->SetColLabelValue( 1, "Value" );

		wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
		sizer->Add( m_grid, 1, wxALL|wxEXPAND, 0 );
		sizer->Add( CreateButtonSizer( wxOK|wxCANCEL ), 0, wxALL|wxEXPAND, 10 );
		SetSizer( sizer );
	}

	void SetData( const KeyValueMap &map )
	{
		if ( map.size() == 0 ) return;

		wxArrayString keys;
		for( KeyValueMap::const_iterator it = map.begin();
			it != map.end();
			++it )
			keys.Add( it->first );

		keys.Sort();

		m_grid->ResizeGrid( map.size(), 2 );
		for( size_t i=0;i<keys.size();i++ )
		{
			m_grid->SetCellValue( i, 0, keys[i] );
			KeyValueMap::const_iterator it = map.find( keys[i] );
			if ( it != map.end() )
				m_grid->SetCellValue( i, 1, wxString::Format("%lg", it->second ) );
		}
	}

	void GetData( KeyValueMap &map )
	{
		map.clear();
		for( int row=0;row<m_grid->GetNumberRows();row++ )
			map[ m_grid->GetCellValue( row, 0 ) ] = wxAtof( m_grid->GetCellValue( row, 1 ) );
	}

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE( AFTableDataDialog, wxDialog )
END_EVENT_TABLE()


BEGIN_EVENT_TABLE( AFTableDataCtrl, wxButton )
	EVT_BUTTON( wxID_ANY, AFTableDataCtrl::OnPressed )
END_EVENT_TABLE()
AFTableDataCtrl::AFTableDataCtrl( wxWindow *parent, int id,
	const wxPoint &pos, const wxSize &size)
	: wxButton( parent, id, "Edit values...", pos, size )
{
	m_expandable = false;
}

void AFTableDataCtrl::SetFields( const wxArrayString &list )
{
	m_values.clear();
	for( size_t i=0;i<list.size();i++ )
		m_values[ list[i] ] = 0.0f;
}

wxArrayString AFTableDataCtrl::GetFields()
{
	wxArrayString list;
	for( KeyValueMap::iterator it = m_values.begin();
		it != m_values.end();
		++it )
		list.Add( it->first );
	return list;
}

void AFTableDataCtrl::Clear()
{
	m_values.clear();
}
	
void AFTableDataCtrl::SetExpandable( bool b ){ m_expandable = b; }
bool AFTableDataCtrl::GetExpandable() { return m_expandable; }

void AFTableDataCtrl::Set( const wxString &var,double value )
{
	m_values[var]=value;
}

double AFTableDataCtrl::Get( const wxString &var )
{
	KeyValueMap::iterator it = m_values.find( var );
	if ( it == m_values.end() ) return std::numeric_limits<double>::quiet_NaN();
	else return m_values[ var ];
}
	
void AFTableDataCtrl::SetDescription(const wxString &s)
{
	m_description = s;
}

wxString AFTableDataCtrl::GetDescription()
{
	return m_description;
}

void AFTableDataCtrl::OnPressed(wxCommandEvent &evt)
{
	AFTableDataDialog dlg( this, "Edit values" );
	dlg.SetData( m_values );
	if ( wxID_OK == dlg.ShowModal() )
	{
		dlg.GetData( m_values );
		evt.Skip();  // allow event to propagate indicating underlying value changed
	}

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

std::vector<double> AFMonthlyFactorCtrl::Get()
{
	std::vector<double> data( 12, 0.0f );
	for (int i=0;i<12;i++)
		data[i] = mData[i];
	return data;
}

void AFMonthlyFactorCtrl::Set( const std::vector<double> &data )
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
		: wxDialog( parent, wxID_ANY, title, wxDefaultPosition, wxScaleSize(400, 500), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER )
	{
		lblDescription = new wxStaticText( this, wxID_ANY, "Values" );
		grdData = new wxExtGridCtrl( this, ID_grid );
		grdData->CreateGrid( 12, 1 );
		grdData->DisableDragCell();
		grdData->DisableDragColSize();
		grdData->DisableDragRowSize();
		grdData->DisableDragColMove();
		grdData->DisableDragGridSize();

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
	
		double d[12];
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
			grdData->Paste( wxExtGridCtrl::PASTE_ALL );
		else
			grdData->Copy(true);
	}
	void SetData(double d[12])
	{
		for (int i=0;i<12;i++)
			grdData->SetCellValue( i, 0, wxString::Format("%g", d[i]) );
	}

	void GetData(double d[12])
	{
		for (int i=0;i<12;i++)
			d[i] = (double) wxAtof( grdData->GetCellValue(i,0) );
	}

	void SetDescription(const wxString &data)
	{
		lblDescription->SetLabel(data);
	}

	void OnApplySingleValue(wxCommandEvent &)
	{
		for (int i=0;i<12;i++)
			grdData->SetCellValue( i, 0, wxString::Format("%lg", numSV->Value()) );
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

enum{ IDSLB_LIST = wxID_HIGHEST+598, IDSLB_FILTER };

BEGIN_EVENT_TABLE( AFSearchListBox, wxPanel )
	EVT_LISTBOX( IDSLB_LIST, AFSearchListBox::OnSelect )
	EVT_TEXT( IDSLB_FILTER, AFSearchListBox::OnFilter )
END_EVENT_TABLE()

AFSearchListBox::AFSearchListBox(wxWindow *parent, int id, const wxPoint &pos, const wxSize &size )
	: wxPanel( parent, id, pos, size, wxCLIP_CHILDREN )
{
	m_label = new wxStaticText( this, -1, " Filter:  ");

	
//		m_notifyLabel = new wxStaticText(this, -1, wxEmptyString);
	m_notifyLabel = new wxStaticText(this, -1, "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
	m_notifyLabel->SetForegroundColour(*wxRED);

	m_txtFilter = new wxTextCtrl( this, IDSLB_FILTER );
	wxSize szbest = m_txtFilter->GetBestSize();
//	szbest.SetWidth(2 * szbest.GetWidth());
	szbest.SetWidth(1.5 * szbest.GetWidth());
	m_txtFilter->SetInitialSize(szbest);

	m_list = new wxListBox( this, IDSLB_LIST, wxDefaultPosition, wxDefaultSize,
			0, 0,
			wxLB_SINGLE|wxLB_HSCROLL|wxLB_ALWAYS_SB );
	
	wxBoxSizer *szh = new wxBoxSizer(wxHORIZONTAL);
	szh->Add( m_label, 0, wxALL|wxEXPAND, 5  );
	szh->Add( m_txtFilter, 0, wxALL|wxEXPAND, 2 );
	szh->Add( m_notifyLabel, 1, wxALL|wxEXPAND, 5 );

	wxBoxSizer *szmain = new wxBoxSizer(wxVERTICAL);
	szmain->Add( szh, 0, wxALL|wxEXPAND, 2 );
	szmain->Add( m_list, 1, wxALL|wxEXPAND, 2);
	SetSizer(szmain);
	m_notifyLabel->SetLabel(wxEmptyString);
}

void AFSearchListBox::SetPromptText( const wxString &s )
{
	m_label->SetLabel( s );
	Layout();
}

void AFSearchListBox::Append( const wxString &s )
{
	m_items.push_back( item(s,true) );
	
	UpdateView();
}

void AFSearchListBox::Append( const wxArrayString &s )
{
	for (size_t i=0;i<s.size();i++)
		m_items.push_back( item(s[i],true) );

	UpdateView();
}

int AFSearchListBox::GetSelection()
{
	wxString sel = m_list->GetStringSelection();
	if (!sel.IsEmpty())
	{
		for (size_t i=0;i<m_items.size();i++)
			if (m_items[i].str == sel)
				return i;
	}
	return -1;
}

size_t AFSearchListBox::Count()
{
	return m_items.size();
}

wxString AFSearchListBox::GetItem( size_t i )
{
	if ( m_items.size() ) return m_items[i].str;
	else return wxEmptyString;
}

void AFSearchListBox::Clear()
{
	m_txtFilter->Clear();
	m_list->Clear();
	m_items.clear();
}

wxString AFSearchListBox::GetStringSelection()
{
	return m_list->GetStringSelection();
}
 
void AFSearchListBox::SetSelection( size_t i )
{
	if ( i < m_items.size() )
	{
		if ( !m_items[i].shown )
		{
			m_items[i].shown = true;
			UpdateView();
		}
	
		m_list->SetStringSelection( m_items[i].str );
	}
}

bool AFSearchListBox::SetStringSelection(const wxString &s)
{
	for (size_t i=0;i<m_items.size();i++)
	{
		if (m_items[i].str == s)
		{
			SetSelection(i);
			return true;
		}
	}

	return false;
}

void AFSearchListBox::OnFilter( wxCommandEvent & )
{
	wxString filter = m_txtFilter->GetValue().Lower();
	wxString sel = m_list->GetStringSelection();
	
	m_notifyLabel->SetLabel( wxEmptyString );

	if (filter.IsEmpty())
	{
		for (size_t i=0;i<m_items.size();i++)
			m_items[i].shown = true;
	}
	else
	{
		int count = 0;
		for (size_t i=0;i<m_items.size();i++)
		{
			if (filter.Len() <= 2 && m_items[i].str.Left( filter.Len() ).Lower() == filter)
			{
				m_items[i].shown = true;
				count++;
			}
			else if (m_items[i].str.Lower().Find( filter ) >= 0)
			{
				m_items[i].shown = true;
				count++;
			}
			else if (m_items[i].str.Lower().Find( filter ) == 0)
			{
				m_items[i].shown = true;
				count++;
			}
			else if (m_items[i].str == sel)
			{
				m_items[i].shown = true;
				count++;
			}
			else
				m_items[i].shown = false;
		}

		if (count == 0)
			m_notifyLabel->SetLabel("No matches.");
//		m_notifyLabel->SetLabel("No matches. (selected item shown)");
		else
			m_notifyLabel->SetLabel(wxString::Format("%d filtered.", count));
	}

	UpdateView();
	m_list->SetStringSelection( sel );
}

void AFSearchListBox::OnSelect( wxCommandEvent & )
{
	wxCommandEvent edup( wxEVT_COMMAND_LISTBOX_SELECTED, this->GetId() );
	edup.SetEventObject( this );
	edup.SetString( m_list->GetStringSelection() );
	edup.SetInt( GetSelection() );

	ProcessEvent( edup );
}

void AFSearchListBox::UpdateView()
{
	wxArrayString list;
	for (size_t i=0;i<m_items.size();i++)
		if (m_items[i].shown)
			list.Add( m_items[i].str );

	m_list->Clear();
	m_list->Append(list);

}


class AFFloatArrayTable : public wxGridTableBase
{
	std::vector<double> *d_arr;
	int mode;
	wxString label;

public:
	AFFloatArrayTable(std::vector<double> *da, int _mode, const wxString &_label)
	{
		label = _label;
		mode = _mode;
		d_arr = da;
	}

	void SetArray(std::vector<double> *da)
	{
		d_arr = da;
	}

	virtual int GetNumberRows()
	{
		if (!d_arr) return 0;

		return (int)d_arr->size();
	}

	virtual int GetNumberCols()
	{
		return 1;
	}

	virtual bool IsEmptyCell( int , int )
	{
		return false;
	}

	virtual wxString GetValue( int row, int  )
	{
		if (d_arr && row >= 0 && row < (int)d_arr->size())
			return wxString::Format("%g", d_arr->at(row));
		else
			return "-0.0";
	}

	virtual void SetValue( int row, int , const wxString& value )
	{
		if (d_arr && row >= 0 && row <  (int)d_arr->size())
			d_arr->at(row) = wxAtof( value );
	}

	virtual wxString GetRowLabelValue( int row )
	{
		if (d_arr && mode == DATA_ARRAY_8760_MULTIPLES)
		{
			int nmult = d_arr->size()/8760;
			if (nmult != 0)
			{
				double step = 1.0/((double)nmult);
				double tm = step*(row+1);
				double frac = tm - ((double)(int)tm);
				if (frac == 0.0)
					return wxString::Format("%lg",tm);
				else
					return wxString::Format("   .%lg",frac*60);
			}
		}

		return wxString::Format("%d",row+1);
	}

	virtual wxString GetColLabelValue( int  )
	{
		return label.IsEmpty()?"Value":label;
	}

	virtual wxString GetTypeName( int , int  )
	{
		return wxGRID_VALUE_STRING;
	}

	virtual bool CanGetValueAs( int , int , const wxString& typeName )
	{
		return typeName==wxGRID_VALUE_STRING;
	}

	virtual bool CanSetValueAs( int , int , const wxString& typeName )
	{
		return typeName==wxGRID_VALUE_STRING;
	}

	virtual bool AppendRows(size_t nrows)
	{
		if (d_arr && nrows > 0)
		{
			if (d_arr->size()+nrows > d_arr->capacity())
				d_arr->reserve( d_arr->size()+nrows );
		
			for (size_t i=0;i<nrows;i++)
				d_arr->push_back(0.0);

			
			if ( GetView() )
			{
				wxGridTableMessage msg( this,
										wxGRIDTABLE_NOTIFY_ROWS_APPENDED,
										nrows );

				GetView()->ProcessTableMessage( msg );
			}
		}

		return true;
	}

	virtual bool InsertRows(size_t pos, size_t nrows)
	{

		if (!d_arr) return true;

		if (pos > d_arr->size()) pos = d_arr->size();

		for (int i=0;i<(int)nrows;i++)
		{
			d_arr->insert( d_arr->begin(), 0.0 );
		}
	
		if ( GetView() )
		{
			wxGridTableMessage msg( this,
									wxGRIDTABLE_NOTIFY_ROWS_INSERTED,
									pos,
									nrows );

			GetView()->ProcessTableMessage( msg );
		}

		return true;
	}

	virtual bool DeleteRows(size_t pos, size_t nrows)
	{
		if (!d_arr) return true;

		if (nrows > d_arr->size() - pos)
			nrows = d_arr->size() - pos;

		//applog("2 Delete Rows[ %d %d ] RowCount %d\n", pos, nrows, Stage->ElementList.size());
		d_arr->erase(d_arr->begin()+pos, d_arr->begin()+pos+nrows);

		if ( GetView() )
		{
		//	applog("RowCount Post Delete %d :: %d\n", Stage->ElementList.size(), this->GetNumberRows());
			wxGridTableMessage msg( this,
									wxGRIDTABLE_NOTIFY_ROWS_DELETED,
									pos,
									nrows );

			GetView()->ProcessTableMessage( msg );
		}

		return true;
	}
};



class AFStringArrayTable : public wxGridTableBase
{
	wxArrayString *d_arr;
	int mode;
	wxString label;

public:
	AFStringArrayTable(wxArrayString *da, const wxString &_label)
	{
		label = _label;
		d_arr = da;
	}

	void SetArray( wxArrayString *da)
	{
		d_arr = da;
	}

	virtual int GetNumberRows()
	{
		return (int)d_arr->Count();
	}

	virtual int GetNumberCols()
	{
		return 1;
	}

	virtual bool IsEmptyCell(int, int)
	{
		return false;
	}

	virtual wxString GetValue(int row, int)
	{
		if (row >= 0 && row < (int)d_arr->Count())
			return  d_arr->Item(row);
		else
			return "";
	}

	virtual void SetValue(int row, int, const wxString& value)
	{
		if (row >= 0 && row <  (int)d_arr->Count())
			d_arr->Item(row)= value;
	}

	virtual wxString GetRowLabelValue(int row)
	{
		return wxString::Format("%d", row + 1);
	}

	virtual wxString GetColLabelValue(int)
	{
		return label.IsEmpty() ? "Value" : label;
	}

	virtual wxString GetTypeName(int, int)
	{
		return wxGRID_VALUE_STRING;
	}

	virtual bool CanGetValueAs(int, int, const wxString& typeName)
	{
		return typeName == wxGRID_VALUE_STRING;
	}

	virtual bool CanSetValueAs(int, int, const wxString& typeName)
	{
		return typeName == wxGRID_VALUE_STRING;
	}

	virtual bool AppendRows(size_t nrows)
	{
		if (nrows > 0)
		{
			for (size_t i = 0; i<nrows; i++)
				d_arr->Add("");


			if (GetView())
			{
				wxGridTableMessage msg(this,
					wxGRIDTABLE_NOTIFY_ROWS_APPENDED,
					nrows);

				GetView()->ProcessTableMessage(msg);
			}
		}

		return true;
	}

	virtual bool InsertRows(size_t pos, size_t nrows)
	{

		if (pos > d_arr->Count()) pos = d_arr->Count();

		for (int i = 0; i<(int)nrows; i++)
		{
			d_arr->Insert("", 0);
		}

		if (GetView())
		{
			wxGridTableMessage msg(this,
				wxGRIDTABLE_NOTIFY_ROWS_INSERTED,
				pos,
				nrows);

			GetView()->ProcessTableMessage(msg);
		}

		return true;
	}

	virtual bool DeleteRows(size_t pos, size_t nrows)
	{

		if (nrows > d_arr->Count() - pos)
			nrows = d_arr->Count() - pos;

		//applog("2 Delete Rows[ %d %d ] RowCount %d\n", pos, nrows, Stage->ElementList.size());
		d_arr->RemoveAt( pos,  nrows);

		if (GetView())
		{
			//	applog("RowCount Post Delete %d :: %d\n", Stage->ElementList.size(), this->GetNumberRows());
			wxGridTableMessage msg(this,
				wxGRIDTABLE_NOTIFY_ROWS_DELETED,
				pos,
				nrows);

			GetView()->ProcessTableMessage(msg);
		}

		return true;
	}
};











// testing
class AFDataMatrixTable : public wxGridTableBase
{

public:
	wxString label;
	matrix_t<double> *d_mat ;
	double def_val;
	int choice_col;
	wxString collabels;
	wxString rowlabels;

	AFDataMatrixTable(matrix_t<double> *da, const int &col, const wxString &_collabels, const wxString &_rowlabels)
	{
		SetMatrix(da);
		choice_col = col;
		collabels = _collabels;
		rowlabels = _rowlabels;
	}

	void SetMatrix(matrix_t<double> *da)
	{
		d_mat = da;
	}

	matrix_t<double> GetMatrix()
	{
		return *d_mat;
	}

	void SetChoiceCol(int &col)
	{
		choice_col = col;
	}

	int GetNumberRows()
	{
		if (!d_mat) return 0;

		return (int)d_mat->nrows();
	}

	int GetNumberCols()
	{
		if (!d_mat) return 0;

		return (int)d_mat->ncols();
	}

	bool IsEmptyCell(int , int )
	{ 
		return false;
	}

	wxString GetValue(int row, int col)
	{
		if (d_mat && row >= 0 && row < (int)d_mat->nrows() && col >= 0 && col < (int)d_mat->ncols())
			return wxString::Format("%g", d_mat->at(row, col));
		else
			return "-0.0";
	}

	void SetValue(int row, int col, const wxString& value)
	{
		if (d_mat && row >= 0 && row < (int)d_mat->nrows() && col >= 0 && col < (int)d_mat->ncols())
			d_mat->at(row, col) = wxAtof(value);
	}
	
	wxString GetRowLabelValue(int row)
	{
		if (d_mat)
		{
			wxArrayString as = wxStringTokenize(rowlabels, ",");
			if (row > -1 && row < (int)as.Count())
				return as[row];
		}
		return wxString::Format("%d", row + 1);
	}

	wxString GetColLabelValue(int col)
	{
		if (d_mat)
		{
			wxArrayString as = wxStringTokenize(collabels, ",");
			collabels.Replace("\\n", "\n");
			if (col > -1 && col < (int)as.Count())
				return as[col];
		}

		return wxString::Format("%d", col + 1);
	}

	wxString GetTypeName(int , int col)
	{
		if (col == choice_col)
			return "GridCellChoice";
		else
			return wxGRID_VALUE_STRING;
		
	}

	bool CanGetValueAs(int , int , const wxString& typeName)
	{
		return typeName == wxGRID_VALUE_STRING;
	}

	bool CanSetValueAs(int , int , const wxString& typeName)
	{
		return typeName == wxGRID_VALUE_STRING;
	}

	bool AppendRows(size_t nrows)
	{
		if (d_mat && nrows > 0)
		{
			size_t new_rows = d_mat->nrows() + nrows;
			d_mat->resize_preserve(new_rows, d_mat->ncols(), def_val);

			if (GetView())
			{
				wxGridTableMessage msg(this,
					wxGRIDTABLE_NOTIFY_ROWS_APPENDED,
					nrows);

				GetView()->ProcessTableMessage(msg);
			}
		}

		return true;
	}

	bool InsertRows(size_t pos, size_t nrows)
	{

		if (!d_mat) return true;

		if (pos > d_mat->nrows()) pos = d_mat->nrows();

		size_t new_rows = d_mat->nrows() + nrows;
		matrix_t<double> old(*d_mat);
		d_mat->resize_fill(new_rows, d_mat->ncols(), def_val);

		for (size_t r = 0; r < pos && r < old.nrows(); r++)
			for (size_t c = 0; c < old.ncols(); c++)
				d_mat->at(r, c) = old(r, c);

		// r-nrows>=0 since pos>=0
		for (size_t r = pos + nrows; r < new_rows && r - nrows < old.nrows(); r++)
			for (size_t c = 0; c < old.ncols(); c++)
				d_mat->at(r, c) = old(r - nrows, c);

		if (GetView())
		{
			wxGridTableMessage msg(this,
				wxGRIDTABLE_NOTIFY_ROWS_INSERTED,
				pos,
				nrows);

			GetView()->ProcessTableMessage(msg);
		}

		return true;
	}

	bool DeleteRows(size_t pos, size_t nrows)
	{
		if (!d_mat) return true;

		if (nrows > d_mat->nrows() - pos)
			nrows = d_mat->nrows() - pos;

		size_t new_rows = d_mat->nrows() - nrows;
		matrix_t<double> old(*d_mat);
		d_mat->resize_preserve(new_rows, d_mat->ncols(), def_val);

		for (size_t r = pos; r < new_rows && r + nrows < old.nrows(); r++)
			for (size_t c = 0; c < old.ncols(); c++)
				d_mat->at(r, c) = old(r + nrows, c);

		if (GetView())
		{
			wxGridTableMessage msg(this,
				wxGRIDTABLE_NOTIFY_ROWS_DELETED,
				pos,
				nrows);

			GetView()->ProcessTableMessage(msg);
		}

		return true;
	}

	bool AppendCols(size_t ncols)
	{
		if (d_mat && ncols > 0)
		{
			size_t new_cols = d_mat->ncols() + ncols;
			d_mat->resize_preserve(d_mat->nrows(), new_cols, def_val);

			if (GetView())
			{
				wxGridTableMessage msg(this,
					wxGRIDTABLE_NOTIFY_COLS_APPENDED,
					ncols);

				GetView()->ProcessTableMessage(msg);
			}
		}

		return true;
	}

	bool InsertCols(size_t pos, size_t ncols)
	{

		if (!d_mat) return true;

		if (pos > d_mat->ncols()) pos = d_mat->ncols();

		size_t new_cols = d_mat->ncols() + ncols;
		matrix_t<double> old(*d_mat);
		d_mat->resize_fill(d_mat->nrows(), new_cols, def_val);

		for (size_t r = 0; r < old.nrows(); r++)
			for (size_t c = 0; c < pos && c < old.ncols(); c++)
				d_mat->at(r, c) = old(r, c);

		// r-ncols>=0 since pos>=0
		for (size_t r = 0; r < old.nrows(); r++)
			for (size_t c = pos + ncols; c < new_cols && r - ncols < old.ncols(); c++)
				d_mat->at(r, c) = old(r, c - ncols);

		if (GetView())
		{
			wxGridTableMessage msg(this,
				wxGRIDTABLE_NOTIFY_COLS_INSERTED,
				pos,
				ncols);

			GetView()->ProcessTableMessage(msg);
		}

		return true;
	}

	bool DeleteCols(size_t pos, size_t ncols)
	{
		if (!d_mat) return true;

		if (ncols > d_mat->ncols() - pos)
			ncols = d_mat->ncols() - pos;

		size_t new_cols = d_mat->ncols() - ncols;
		matrix_t<double> old(*d_mat);
		d_mat->resize_preserve(d_mat->nrows(), new_cols, def_val);

		for (size_t r = pos; r < old.nrows(); r++)
			for (size_t c = pos; c < new_cols && c + ncols < old.nrows(); c++)
				d_mat->at(r, c) = old(r, c + ncols);

		if (GetView())
		{
			wxGridTableMessage msg(this,
				wxGRIDTABLE_NOTIFY_COLS_DELETED,
				pos,
				ncols);

			GetView()->ProcessTableMessage(msg);
		}

		return true;
	}
};









enum { IDDD_GRID=wxID_HIGHEST+945, IDDD_CHANGENUMROWS, IDDD_COPY, IDDD_PASTE, IDDD_IMPORT, IDDD_EXPORT };

class AFDataArrayDialog : public wxDialog
{
private:
	wxString mLabel;
	int mMode;
	std::vector<double> mData;
	wxExtGridCtrl *Grid;
	AFFloatArrayTable *GridTable;
	wxStaticText *ModeLabel;
	wxStaticText *Description;
	wxButton *ButtonChangeRows;

public:
	AFDataArrayDialog(wxWindow *parent, const wxString &title, const wxString &desc, const wxString &collabel)
		: wxDialog(parent, wxID_ANY, title, wxDefaultPosition, wxScaleSize(430,510), 
			wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER )
	{
		mLabel = collabel;

		GridTable = NULL;
		mMode = DATA_ARRAY_8760_MULTIPLES;

		wxButton *btn = NULL;
		Grid = new wxExtGridCtrl(this, IDDD_GRID);
		Grid->DisableDragCell();
		//Grid->DisableDragColSize();
		Grid->DisableDragRowSize();
		Grid->DisableDragColMove();
		Grid->DisableDragGridSize();
		Grid->SetRowLabelAlignment(wxALIGN_LEFT, wxALIGN_CENTER);

		wxBoxSizer *szh_top1 = new wxBoxSizer(wxHORIZONTAL);
		btn = new wxButton(this, IDDD_COPY, "Copy");
		szh_top1->Add(btn, 0, wxALL|wxEXPAND, 1);
		btn = new wxButton(this, IDDD_PASTE, "Paste");
		szh_top1->Add(btn, 0, wxALL|wxEXPAND, 1);
		btn = new wxButton(this, IDDD_IMPORT, "Import");
		szh_top1->Add(btn, 0, wxALL|wxEXPAND, 1);
		btn = new wxButton(this, IDDD_EXPORT, "Export");
		szh_top1->Add(btn, 0, wxALL|wxEXPAND, 1);
		szh_top1->AddStretchSpacer();

		wxBoxSizer *szh_top2 = new wxBoxSizer(wxHORIZONTAL);
		ButtonChangeRows = new wxButton(this, IDDD_CHANGENUMROWS, "Number of Values...");
		szh_top2->Add(ButtonChangeRows, 0, wxALL|wxEXPAND, 1);
		ModeLabel = new wxStaticText(this,-1,"");
		szh_top2->AddSpacer(3);
		szh_top2->Add(ModeLabel,0,wxALL|wxALIGN_CENTER_VERTICAL,3);
		szh_top2->AddStretchSpacer();
	
		wxBoxSizer *szv_main = new wxBoxSizer(wxVERTICAL);
		// reverse order per Paul email 2/10/15
		szv_main->Add(szh_top2, 0, wxALL|wxEXPAND, 4);	
		szv_main->Add(szh_top1, 0, wxALL | wxEXPAND, 4);
		szv_main->Add(Grid, 1, wxALL | wxEXPAND, 4);
		Description = 0;
		if ( !desc.IsEmpty() )
		{
			Description = new wxStaticText( this, wxID_ANY, desc );
			Description->Wrap( 350 );
			szv_main->Add( Description, 0, wxALL, 10 );
		}
		szv_main->Add( CreateButtonSizer( wxOK|wxCANCEL ), 0, wxALL|wxEXPAND, 10);
			
		SetMode(DATA_ARRAY_8760_MULTIPLES);
		
		SetSizer(szv_main);
	}

	void SetMode(int m)
	{
		mMode = m;
		wxString l;
		if (mMode == DATA_ARRAY_8760_ONLY)
		{
			ModeLabel->SetLabel("Hourly Values (8760)");
			ButtonChangeRows->Hide();
		}
		else if (mMode == DATA_ARRAY_8760_MULTIPLES)
		{
			ModeLabel->SetLabel("Subhourly Values (8760x1/TS)");
			ButtonChangeRows->SetLabel( "Change time step..." );
			ButtonChangeRows->Show();
		}
		else
		{
			ModeLabel->SetLabel("");
			ButtonChangeRows->SetLabel( "Number of values..." );
			ButtonChangeRows->Show();
		}
	}

	int GetMode()
	{
		return mMode;
	}

	void SetData(const std::vector<double> &data)
	{
		mData = data;

		if (GridTable) GridTable->SetArray(NULL);
		Grid->SetTable( NULL );

		GridTable = new AFFloatArrayTable(&mData, mMode, mLabel);
		GridTable->SetAttrProvider( new wxExtGridCellAttrProvider );

		Grid->SetTable( GridTable, true );
		Grid->SetColSize(0, (int)(130*wxGetScreenHDScale()));

		Grid->Layout();
		Grid->Refresh();

	}

	void GetData(std::vector<double> &data)
	{
		data = mData;
	}

	void SetDataLabel(const wxString &s)
	{
		mLabel = s;
	}

	wxString GetDataLabel()
	{
		return mLabel;
	}

	void OnCommand(wxCommandEvent &evt)
	{
		if (evt.GetId() == IDDD_CHANGENUMROWS)
		{
			long l=0;
			if (mMode == DATA_ARRAY_8760_MULTIPLES)
			{
				int nmult = mData.size()/8760;
				double tmstp = nmult != 0 ? 1.0/((double)nmult) : 1.0;

				wxString result = wxGetTextFromUser("Enter time step (minutes):", "Edit Table",
					wxString::Format("%lg", tmstp*60), this );

				if (result.IsEmpty())
					return;

				tmstp = wxAtof(result)/60.0;
				if (tmstp > 0 && (int)(1.0/tmstp) > 0)
				{
					l = 8760*(int)(1.0/tmstp);
				}
				else
				{
					wxMessageBox("Invalid time step.");
					return;
				}
			}
			else
			{
				wxString result = wxGetTextFromUser("Enter number of data rows", "Edit Table",
					wxString::Format("%d", Grid->GetNumberRows()), this );
				if (result.IsEmpty()) return;

				if (!result.ToLong(&l))
					return;
			}

			if ( l > 0 )
			{
				if (mMode == DATA_ARRAY_8760_ONLY)
				{
					l = 8760;
				}
				else if (mMode == DATA_ARRAY_8760_MULTIPLES)
				{
					int nmult = l/8760;
					l=nmult*8760;
					if (l < 8760) l = 8760;
				}

				Grid->ResizeGrid( l, 1 );
			}
			else
				wxMessageBox("Invalid number of rows or non-numeric entry.");
		}
		else if (evt.GetId() == IDDD_COPY)
			Grid->Copy(true);
		else if (evt.GetId() == IDDD_PASTE)
			Grid->Paste(wxExtGridCtrl::PASTE_ALL);
		else if (evt.GetId() == IDDD_IMPORT)
		{
			wxFileDialog dlg(this, "Select data file to import");
			if (dlg.ShowModal() != wxID_OK) return;
			FILE *fp = fopen(dlg.GetPath().c_str(), "r");
			if (!fp)
			{
				wxMessageBox("Could not open file for reading:\n\n" + dlg.GetPath());
				return;
			}

			std::vector<double> arr;
			arr.reserve( mData.size() );

			char buf[128];
			fgets(buf,127,fp); // skip header line

			bool error = false;
			for (int i=0;i<(int)mData.size();i++)
			{
				if (fgets(buf, 127, fp) == NULL)
				{
					wxMessageBox(wxString::Format("Data file does not contain %d data value lines, only %d found.\n\nNote that the first line in the file is considered a header label and is ignored.", mData.size(), i));
					error = true;
					break;
				}

				arr.push_back( (double) atof(buf) );
			}

			if (!error) SetData(arr);

			fclose(fp);
		}
		else if (evt.GetId() == IDDD_EXPORT)
		{
			wxFileDialog dlg(this, "Select data file to export to",wxEmptyString, wxEmptyString, "*.*", wxFD_SAVE|wxFD_OVERWRITE_PROMPT);
			if (dlg.ShowModal() != wxID_OK) return;

			FILE *fp = fopen(dlg.GetPath().c_str(), "w");
			if (!fp)
			{
				wxMessageBox("Could not open file for writing.");
				return;
			}

			fprintf(fp, "Exported Data (%d)\n", (int)mData.size());
			for (size_t i=0;i<mData.size();i++)
				fprintf(fp, "%g\n", mData[i]);
			fclose(fp);
		}
	}
	
	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE(AFDataArrayDialog, wxDialog)
	EVT_BUTTON( IDDD_COPY, AFDataArrayDialog::OnCommand )
	EVT_BUTTON( IDDD_PASTE, AFDataArrayDialog::OnCommand )
	EVT_BUTTON( IDDD_IMPORT, AFDataArrayDialog::OnCommand )
	EVT_BUTTON( IDDD_EXPORT, AFDataArrayDialog::OnCommand )
	EVT_BUTTON( IDDD_CHANGENUMROWS, AFDataArrayDialog::OnCommand )
END_EVENT_TABLE()

BEGIN_EVENT_TABLE(AFDataArrayButton, wxButton)
EVT_BUTTON(wxID_ANY, AFDataArrayButton::OnPressed)
END_EVENT_TABLE()

AFDataArrayButton::AFDataArrayButton( wxWindow *parent, int id, const wxPoint &pos, const wxSize &size)
	: wxButton(parent, id, "Edit time series data...", pos, size)
{
	mData.resize(8760, 0.0);
	mMode = DATA_ARRAY_8760_MULTIPLES;
}

void AFDataArrayButton::Get(std::vector<double> &data)
{
	data=mData;
}
void AFDataArrayButton::Set(const std::vector<double> &data)
{
	if (mMode == DATA_ARRAY_8760_ONLY)
	{
		if (data.size() != 8760)
		{
			mData.resize(8760, -999.0);
			return;
		}
	}
	else if (mMode == DATA_ARRAY_8760_MULTIPLES)
	{
		int nmult = data.size()/8760;
		if ( nmult*8760 != (int)data.size())
		{
			mData.resize(8760, -998.0);
			return;
		}
	}
	
	mData=data;
}
void AFDataArrayButton::SetDataLabel(const wxString &s)
{
	mDataLabel = s;
}
wxString AFDataArrayButton::GetDataLabel()
{
	return mDataLabel;
}

void AFDataArrayButton::SetMode(int mode)
{
	mMode = mode;
}

int AFDataArrayButton::GetMode()
{
	return mMode;
}

void AFDataArrayButton::OnPressed(wxCommandEvent &evt)
{
	AFDataArrayDialog dlg( this, "Edit Time Series Data", m_description, mDataLabel );
	
	dlg.SetDataLabel( mDataLabel );
	dlg.SetMode(mMode);
	dlg.SetData( mData );

	if (dlg.ShowModal()==wxID_OK)
	{
		dlg.GetData(mData);
		evt.Skip(); // allow event to propagate indicating underlying value changed
	}
}



class AFDataLifetimeArrayTable : public wxGridTableBase
{
	std::vector<double> *d_arr;
	int mode;
	wxString label;

public:
	AFDataLifetimeArrayTable(std::vector<double> *da, int _mode, const wxString &_label)
	{
		label = _label;
		mode = _mode;
		d_arr = da;
	}

	void SetArray(std::vector<double> *da)
	{
		d_arr = da;
	}

	virtual int GetNumberRows()
	{
		if (!d_arr) return 0;

		return (int)d_arr->size();
	}

	virtual int GetNumberCols()
	{
		return 1;
	}

	virtual bool IsEmptyCell(int, int)
	{
		return false;
	}

	virtual wxString GetValue(int row, int)
	{
		if (d_arr && row >= 0 && row < (int)d_arr->size())
			return wxString::Format("%g", d_arr->at(row));
		else
			return "-0.0";
	}

	virtual void SetValue(int row, int, const wxString& value)
	{
		if (d_arr && row >= 0 && row < (int)d_arr->size())
			d_arr->at(row) = wxAtof(value);
	}

	virtual wxString GetRowLabelValue(int row)
	{
		/* TODO - setup row labels based on year and mode 
		if (d_arr && mode == DATA_LIFETIME_SUBHOURLY)
		{
			int nmult = d_arr->size() / 8760;
			if (nmult != 0)
			{
				double step = 1.0 / ((double)nmult);
				double tm = step * (row + 1);
				double frac = tm - ((double)(int)tm);
				if (frac == 0.0)
					return wxString::Format("%lg", tm);
				else
					return wxString::Format("   .%lg", frac * 60);
			}
		}
		*/
		return wxString::Format("%d", row + 1);
	}

	virtual wxString GetColLabelValue(int)
	{
		return label.IsEmpty() ? "Value" : label;
	}

	virtual wxString GetTypeName(int, int)
	{
		return wxGRID_VALUE_STRING;
	}

	virtual bool CanGetValueAs(int, int, const wxString& typeName)
	{
		return typeName == wxGRID_VALUE_STRING;
	}

	virtual bool CanSetValueAs(int, int, const wxString& typeName)
	{
		return typeName == wxGRID_VALUE_STRING;
	}

	virtual bool AppendRows(size_t nrows)
	{
		if (d_arr && nrows > 0)
		{
			if (d_arr->size() + nrows > d_arr->capacity())
				d_arr->reserve(d_arr->size() + nrows);

			for (size_t i = 0; i < nrows; i++)
				d_arr->push_back(0.0);


			if (GetView())
			{
				wxGridTableMessage msg(this,
					wxGRIDTABLE_NOTIFY_ROWS_APPENDED,
					nrows);

				GetView()->ProcessTableMessage(msg);
			}
		}

		return true;
	}

	virtual bool InsertRows(size_t pos, size_t nrows)
	{

		if (!d_arr) return true;

		if (pos > d_arr->size()) pos = d_arr->size();

		for (int i = 0; i < (int)nrows; i++)
		{
			d_arr->insert(d_arr->begin(), 0.0);
		}

		if (GetView())
		{
			wxGridTableMessage msg(this,
				wxGRIDTABLE_NOTIFY_ROWS_INSERTED,
				pos,
				nrows);

			GetView()->ProcessTableMessage(msg);
		}

		return true;
	}

	virtual bool DeleteRows(size_t pos, size_t nrows)
	{
		if (!d_arr) return true;

		if (nrows > d_arr->size() - pos)
			nrows = d_arr->size() - pos;

		//applog("2 Delete Rows[ %d %d ] RowCount %d\n", pos, nrows, Stage->ElementList.size());
		d_arr->erase(d_arr->begin() + pos, d_arr->begin() + pos + nrows);

		if (GetView())
		{
			//	applog("RowCount Post Delete %d :: %d\n", Stage->ElementList.size(), this->GetNumberRows());
			wxGridTableMessage msg(this,
				wxGRIDTABLE_NOTIFY_ROWS_DELETED,
				pos,
				nrows);

			GetView()->ProcessTableMessage(msg);
		}

		return true;
	}
};




enum { ILDD_GRID = wxID_HIGHEST + 945, ILDD_MODEOPTIONS, ILDD_TIMESTEPS, ILDD_SINGLEVALUE, ILDD_COPY, ILDD_PASTE, ILDD_IMPORT, ILDD_EXPORT };

class AFDataLifetimeArrayDialog : public wxDialog
{
private:
	wxString mLabel;
	size_t mAnalysisPeriod, mMinPerHour, mMode;
	std::vector<double> mData;
	wxExtGridCtrl *Grid;
	AFDataLifetimeArrayTable *GridTable;
	wxStaticText *Description, *AnalysisPeriodLabel;
	wxStaticText *InputLabel, *TimestepsLabel;
	wxComboBox *ModeOptions;
	wxComboBox *Timesteps;
	wxNumericCtrl *SingleValue;
	wxNumericCtrl *AnalysisPeriodValue;

public:
	AFDataLifetimeArrayDialog(wxWindow *parent, const wxString &title, const wxString &desc, const wxString &inputLabel, const bool &optannual = false, const bool &optweekly=false)
		: wxDialog(parent, wxID_ANY, title, wxDefaultPosition, wxScaleSize(430, 510),
			wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
	{
		mLabel = inputLabel;

		GridTable = NULL;
		wxButton *btn = NULL;
		Grid = new wxExtGridCtrl(this, ILDD_GRID);
		Grid->DisableDragCell();
		//Grid->DisableDragColSize();
		Grid->DisableDragRowSize();
		Grid->DisableDragColMove();
		Grid->DisableDragGridSize();
		Grid->SetRowLabelAlignment(wxALIGN_LEFT, wxALIGN_CENTER);

		wxBoxSizer *szh_btns = new wxBoxSizer(wxHORIZONTAL);
		wxBoxSizer *szv_copypaste = new wxBoxSizer(wxVERTICAL);
		btn = new wxButton(this, ILDD_COPY, "Copy");
		szv_copypaste->Add(btn, 0, wxALL | wxEXPAND, 1);
		btn = new wxButton(this, ILDD_PASTE, "Paste");
		szv_copypaste->Add(btn, 0, wxALL | wxEXPAND, 1);
		wxBoxSizer *szv_exportimport = new wxBoxSizer(wxVERTICAL);
		btn = new wxButton(this, ILDD_EXPORT, "Export");
		szv_exportimport->Add(btn, 0, wxALL | wxEXPAND, 1);
		btn = new wxButton(this, ILDD_IMPORT, "Import");
		szv_exportimport->Add(btn, 0, wxALL | wxEXPAND, 1);
		szh_btns->Add(szv_copypaste);
		szh_btns->Add(szv_exportimport);
		szh_btns->AddStretchSpacer();


		wxBoxSizer *szh_top4 = new wxBoxSizer(wxHORIZONTAL);
		wxArrayString timestepmin;
		timestepmin.Add(" 1");
		timestepmin.Add(" 5");
		timestepmin.Add("10");
		timestepmin.Add("15");
		timestepmin.Add("20");
		timestepmin.Add("30");
		Timesteps = new wxComboBox(this, ILDD_TIMESTEPS, "30", wxDefaultPosition, wxDefaultSize, timestepmin);
		TimestepsLabel = new wxStaticText(this, -1, "Select timestep minutes");
		szh_top4->Add(TimestepsLabel, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
		szh_top4->AddSpacer(3);
		szh_top4->Add(Timesteps, 0, wxALL | wxEXPAND, 1);
		szh_top4->AddStretchSpacer();

		wxBoxSizer *szh_top3 = new wxBoxSizer(wxHORIZONTAL);
		wxArrayString modes;
		modes.Add("Single Value");
		modes.Add("Monthly");
		modes.Add("Daily");
		modes.Add("Hourly");
		modes.Add("Subhourly");
		if (optannual)	modes.Add("Annual");
		if (optweekly) modes.Add("Weekly");
		ModeOptions = new wxComboBox(this, ILDD_MODEOPTIONS, "Monthly", wxDefaultPosition, wxDefaultSize, modes);
		szh_top3->Add(new wxStaticText(this, -1, "Mode"), 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
		szh_top3->AddSpacer(3);
		szh_top3->Add(ModeOptions, 0, wxALL | wxEXPAND, 1);
		szh_top3->AddStretchSpacer();

		wxBoxSizer *szh_top1 = new wxBoxSizer(wxHORIZONTAL);
		InputLabel = new wxStaticText(this, wxID_ANY, mLabel);
		szh_top1->AddSpacer(3);
		szh_top1->Add(InputLabel, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
		szh_top1->AddStretchSpacer();

		wxBoxSizer *szh_top2 = new wxBoxSizer(wxHORIZONTAL);
		AnalysisPeriodValue = new wxNumericCtrl(this, wxID_ANY);
		AnalysisPeriodValue->Enable(false);
		AnalysisPeriodLabel = new wxStaticText(this, -1, "Analysis period");
		szh_top2->Add(AnalysisPeriodLabel, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
		szh_top2->AddSpacer(3);
		szh_top2->Add(AnalysisPeriodValue, 0, wxALL | wxEXPAND, 1);
		szh_top2->AddStretchSpacer();

		wxBoxSizer *szh_singlevalue = new wxBoxSizer(wxHORIZONTAL);
		SingleValue = new wxNumericCtrl(this, ILDD_SINGLEVALUE);
		szh_singlevalue->Add(new wxStaticText(this, -1, "Enter single value"), 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
		szh_singlevalue->AddSpacer(3);
		szh_singlevalue->Add(SingleValue, 0, wxALL | wxEXPAND, 1);
		szh_singlevalue->AddStretchSpacer();

		wxBoxSizer *szv_main_vert = new wxBoxSizer(wxVERTICAL);
		szv_main_vert->Add(szh_top1, 0, wxALL | wxEXPAND, 4);
		szv_main_vert->Add(szh_top2, 0, wxALL | wxEXPAND, 4);
		szv_main_vert->Add(szh_top3, 0, wxALL | wxEXPAND, 4);
		szv_main_vert->Add(szh_top4, 0, wxALL | wxEXPAND, 4);
		szv_main_vert->Add(szh_btns, 0, wxALL | wxEXPAND, 4);
		szv_main_vert->Add(new wxStaticText(this, -1, "_____________________"), 0, wxALL, 3);
		szv_main_vert->Add(szh_singlevalue, 0, wxALL | wxEXPAND, 4);
		szv_main_vert->AddStretchSpacer();
		szv_main_vert->Add(CreateButtonSizer(wxOK | wxCANCEL), 0, wxALL | wxEXPAND, 10);
		Description = 0;
		if (!desc.IsEmpty())
		{
			Description = new wxStaticText(this, wxID_ANY, desc);
			Description->Wrap(350);
			szv_main_vert->Add(Description, 0, wxALL, 10);
		}

		wxBoxSizer *szh_main = new wxBoxSizer(wxHORIZONTAL);
		szh_main->Add(szv_main_vert, 0, wxALL | wxEXPAND, 4);
		szh_main->Add(Grid, 3, wxALL | wxEXPAND, 4);

		SetSizer(szh_main);
	}

	void SetMode(int m)
	{
		mMode = m;
		size_t l;
		switch (mMode)
		{
		case DATA_LIFETIME_ARRAY_MONTHLY:
		{
			l = mAnalysisPeriod * 12;
			Grid->ResizeGrid(l, 1);
			break;
		}
		case DATA_LIFETIME_ARRAY_DAILY: // assume 365
		{
			l = mAnalysisPeriod * 365;
			Grid->ResizeGrid(l, 1);
			break;
		}
		case DATA_LIFETIME_ARRAY_HOURLY: // assume 8760
		{
			l = mAnalysisPeriod * 8760;
			Grid->ResizeGrid(l, 1);
			break;
		}
		case DATA_LIFETIME_ARRAY_SUBHOURLY: // assume 8760 * timesteps per hour
		{
			// error handling
			mMinPerHour = std::stoul(Timesteps->GetValue().ToStdString());
			l = mAnalysisPeriod * 8760 * (60 / mMinPerHour);
			Grid->ResizeGrid(l, 1);
			break;
		}
		case DATA_LIFETIME_ARRAY_ANNUAL:
		{
			l = mAnalysisPeriod;
			Grid->ResizeGrid(l, 1);
			break;
		}
		case DATA_LIFETIME_ARRAY_WEEKLY: // assume 52 weeks or 364 days?
		{
			l = mAnalysisPeriod * 8760 / (24 * 7);
			Grid->ResizeGrid(l, 1);
			break;
		}
		default: // single value - no grid resize
		{
			break;
		}
		}
		TimestepsLabel->Show((mMode == DATA_LIFETIME_ARRAY_SUBHOURLY));
		Timesteps->Show((mMode == DATA_LIFETIME_ARRAY_SUBHOURLY));
		SingleValue->Enable((mMode == DATA_LIFETIME_ARRAY_SINGLEVALUE));
		Grid->Enable((mMode != DATA_LIFETIME_ARRAY_SINGLEVALUE));
		ModeOptions->SetSelection(mMode);
		Layout();
	}

	int GetMode()
	{
		return mMode;
	}

	void SetData(const std::vector<double> &data)
	{
		mData = data;

		if (GridTable) GridTable->SetArray(NULL);
		Grid->SetTable(NULL);

		GridTable = new AFDataLifetimeArrayTable(&mData, mMode, mLabel);
		GridTable->SetAttrProvider(new wxExtGridCellAttrProvider);

		Grid->SetTable(GridTable, true);
		Grid->SetColSize(0, (int)(130 * wxGetScreenHDScale()));
		Grid->Layout();
		Grid->Refresh();

		// determine mode from data
		size_t dataSize = mData.size();
		if (dataSize < 1)
		{
			mData.push_back(0.0);
			dataSize = 1;
		}
		if (dataSize == 1)
			mMode = DATA_LIFETIME_ARRAY_SINGLEVALUE;
		else if (dataSize == (mAnalysisPeriod))
			mMode = DATA_LIFETIME_ARRAY_ANNUAL;
		else if (dataSize == (mAnalysisPeriod * 12))
			mMode = DATA_LIFETIME_ARRAY_MONTHLY;
		else if (dataSize == (mAnalysisPeriod * 8760 / (24 * 7)))
			mMode = DATA_LIFETIME_ARRAY_WEEKLY;
		else if (dataSize == (mAnalysisPeriod * 365))
			mMode = DATA_LIFETIME_ARRAY_DAILY;
		else if (dataSize == (mAnalysisPeriod * 8760))
			mMode = DATA_LIFETIME_ARRAY_HOURLY;
		else
		{
			mMode = DATA_LIFETIME_ARRAY_SUBHOURLY;
			size_t stepsPerHour = dataSize / (mAnalysisPeriod * 8760);
			switch (stepsPerHour)
			{
			case 2: // 30 minute
				Timesteps->SetSelection(5);
				break;
			case 3: // 20 minute
				Timesteps->SetSelection(4);
				break;
			case 4: // 15 minute
				Timesteps->SetSelection(3);
				break;
			case 6: // 10 minute
				Timesteps->SetSelection(2);
				break;
			case 12: // 5 minute
				Timesteps->SetSelection(1);
				break;
			default: // 1 minute
				Timesteps->SetSelection(0);
				break;
			}
		}

		SetMode(mMode);
	}

	void GetData(std::vector<double> &data)
	{
		data = mData;
	}

	void SetDataLabel(const wxString &s)
	{
		mLabel = s;
		InputLabel->SetLabel(s);
	}

	wxString GetDataLabel()
	{
		return mLabel;
	}

	void SetAnalysisPeriod(const size_t &p)
	{
		mAnalysisPeriod = p;
		AnalysisPeriodValue->SetValue((double)p);
//		SetMode(mMode);
	}

	size_t GetAnalysisPeriod()
	{
		return mAnalysisPeriod;
	}

	void SetMinPerHour(const size_t &p)
	{
		mMinPerHour = p;
	}

	size_t GetMinPerHour()
	{
		return mMinPerHour;
	}


	void OnCommand(wxCommandEvent &evt)
	{
		if (evt.GetId() == ILDD_MODEOPTIONS)
			SetMode(ModeOptions->GetSelection());
		else if (evt.GetId() == ILDD_TIMESTEPS)
			SetMode(ModeOptions->GetSelection());
		else if (evt.GetId() == ILDD_COPY)
			Grid->Copy(true);
		else if (evt.GetId() == ILDD_PASTE)
			Grid->Paste(wxExtGridCtrl::PASTE_ALL);
		else if (evt.GetId() == ILDD_IMPORT)
		{
			wxFileDialog dlg(this, "Select data file to import");
			if (dlg.ShowModal() != wxID_OK) return;
			FILE *fp = fopen(dlg.GetPath().c_str(), "r");
			if (!fp)
			{
				wxMessageBox("Could not open file for reading:\n\n" + dlg.GetPath());
				return;
			}

			std::vector<double> arr;
			arr.reserve(mData.size());

			char buf[128];
			fgets(buf, 127, fp); // skip header line

			bool error = false;
			for (int i = 0; i < (int)mData.size(); i++)
			{
				if (fgets(buf, 127, fp) == NULL)
				{
					wxMessageBox(wxString::Format("Data file does not contain %d data value lines, only %d found.\n\nNote that the first line in the file is considered a header label and is ignored.", mData.size(), i));
					error = true;
					break;
				}

				arr.push_back((double)atof(buf));
			}

			if (!error) SetData(arr);

			fclose(fp);
		}
		else if (evt.GetId() == ILDD_EXPORT)
		{
			wxFileDialog dlg(this, "Select data file to export to", wxEmptyString, wxEmptyString, "*.*", wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
			if (dlg.ShowModal() != wxID_OK) return;

			FILE *fp = fopen(dlg.GetPath().c_str(), "w");
			if (!fp)
			{
				wxMessageBox("Could not open file for writing.");
				return;
			}

			fprintf(fp, "Exported Data (%d)\n", (int)mData.size());
			for (size_t i = 0; i < mData.size(); i++)
				fprintf(fp, "%g\n", mData[i]);
			fclose(fp);
		}
	}

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE(AFDataLifetimeArrayDialog, wxDialog)
EVT_BUTTON(ILDD_COPY, AFDataLifetimeArrayDialog::OnCommand)
EVT_BUTTON(ILDD_PASTE, AFDataLifetimeArrayDialog::OnCommand)
EVT_BUTTON(ILDD_IMPORT, AFDataLifetimeArrayDialog::OnCommand)
EVT_BUTTON(ILDD_EXPORT, AFDataLifetimeArrayDialog::OnCommand)
EVT_COMBOBOX(ILDD_MODEOPTIONS, AFDataLifetimeArrayDialog::OnCommand)
EVT_COMBOBOX(ILDD_TIMESTEPS, AFDataLifetimeArrayDialog::OnCommand)
END_EVENT_TABLE()

BEGIN_EVENT_TABLE(AFDataLifetimeArrayButton, wxButton)
EVT_BUTTON(wxID_ANY, AFDataLifetimeArrayButton::OnPressed)
END_EVENT_TABLE()

AFDataLifetimeArrayButton::AFDataLifetimeArrayButton(wxWindow *parent, int id, const wxPoint &pos, const wxSize &size)
	: wxButton(parent, id, "Edit time series data...", pos, size)
{
	mAnalysisPeriod = 25;
	mMinPerHour = 30;
	mMode = DATA_LIFETIME_ARRAY_MONTHLY;
	mData.resize(12*mAnalysisPeriod, 0.0);
}

void AFDataLifetimeArrayButton::Get(std::vector<double> &data)
{
	data = mData;
}
void AFDataLifetimeArrayButton::Set(const std::vector<double> &data)
{
	mData = data;
	// resize based on potentially new analysis period and current mode
	size_t newSize = mAnalysisPeriod;
	switch (mMode)
	{
	case DATA_LIFETIME_ARRAY_MONTHLY:
	{
		newSize = mAnalysisPeriod * 12;
		break;
	}
	case DATA_LIFETIME_ARRAY_DAILY: // assume 365
	{
		newSize = mAnalysisPeriod * 365;
		break;
	}
	case DATA_LIFETIME_ARRAY_HOURLY: // assume 8760
	{
		newSize = mAnalysisPeriod * 8760;
		break;
	}
	case DATA_LIFETIME_ARRAY_SUBHOURLY: // assume 8760 * timesteps per hour
	{
		newSize = mAnalysisPeriod * 8760 * (60 / mMinPerHour);
		break;
	}
	case DATA_LIFETIME_ARRAY_ANNUAL:
	{
		newSize = mAnalysisPeriod;
		break;
	}
	case DATA_LIFETIME_ARRAY_WEEKLY: // assume 52 weeks or 364 days?
	{
		newSize = mAnalysisPeriod * 8760 / (24 * 7);
		break;
	}
	default: // single value - no grid resize
	{
		newSize = 1;
		break;
	}
	}
	if (mData.size() != newSize)
		mData.resize(newSize);
}
void AFDataLifetimeArrayButton::SetDataLabel(const wxString &s)
{
	mDataLabel = s;
}
wxString AFDataLifetimeArrayButton::GetDataLabel()
{
	return mDataLabel;
}


void AFDataLifetimeArrayButton::OnPressed(wxCommandEvent &evt)
{
	// resize based on potentially new analysis period and current mode
	size_t newSize = mAnalysisPeriod;
	switch (mMode)
	{
	case DATA_LIFETIME_ARRAY_MONTHLY:
	{
		newSize = mAnalysisPeriod * 12;
		break;
	}
	case DATA_LIFETIME_ARRAY_DAILY: // assume 365
	{
		newSize = mAnalysisPeriod * 365;
		break;
	}
	case DATA_LIFETIME_ARRAY_HOURLY: // assume 8760
	{
		newSize = mAnalysisPeriod * 8760;
		break;
	}
	case DATA_LIFETIME_ARRAY_SUBHOURLY: // assume 8760 * timesteps per hour
	{
		newSize = mAnalysisPeriod * 8760 * (60 / mMinPerHour);
		break;
	}
	case DATA_LIFETIME_ARRAY_ANNUAL:
	{
		newSize = mAnalysisPeriod;
		break;
	}
	case DATA_LIFETIME_ARRAY_WEEKLY: // assume 52 weeks or 364 days?
	{
		newSize = mAnalysisPeriod * 8760 / (24 * 7);
		break;
	}
	default: // single value - no grid resize
	{
		newSize = 1;
		break;
	}
	}
	if (mData.size() != newSize)
		mData.resize(newSize);

	AFDataLifetimeArrayDialog dlg(this, "Edit Time Series Data", mDescription, mDataLabel, mAnnualEnabled, mWeeklyEnabled);
	dlg.SetAnalysisPeriod(mAnalysisPeriod);
	dlg.SetData(mData);
	dlg.SetDataLabel(mDataLabel);


	if (dlg.ShowModal() == wxID_OK)
	{
		dlg.GetData(mData);
		mMode = dlg.GetMode();
		mMinPerHour = dlg.GetMinPerHour();
		evt.Skip(); // allow event to propagate indicating underlying value changed
	}
}




class AFDataLifetimeMatrixTable : public wxGridTableBase
{
	matrix_t<double> *d_mat;
	int mode;
	wxArrayString columnLabels;

public:
	AFDataLifetimeMatrixTable(matrix_t<double> *dm, int _mode, const wxArrayString &_columnLabels)
	{
		columnLabels = _columnLabels;
		mode = _mode;
		d_mat = dm;
	}

	void SetMatrix(matrix_t<double> *dm)
	{
		d_mat = dm;
	}

	virtual int GetNumberRows()
	{
		if (!d_mat) return 0;

		return (int)d_mat->nrows();
	}

	virtual int GetNumberCols()
	{
		return (int)d_mat->ncols();
	}

	virtual bool IsEmptyCell(int, int)
	{
		return false;
	}

	virtual wxString GetValue(int row, int col)
	{
		if (d_mat && row >= 0 && row < (int)d_mat->nrows() && col < (int)d_mat->ncols())
			return wxString::Format("%g", d_mat->at(row, col));
		else
			return "-0.0";
	}

	virtual void SetValue(int row, int col, const wxString& value)
	{
		if (d_mat && row >= 0 && row < (int)d_mat->nrows() && col < (int)d_mat->ncols())
			d_mat->at(row, col) = wxAtof(value);
	}

	virtual wxString GetRowLabelValue(int row)
	{
		/* TODO - setup row labels based on year and mode
		if (d_arr && mode == DATA_LIFETIME_SUBHOURLY)
		{
			int nmult = d_arr->size() / 8760;
			if (nmult != 0)
			{
				double step = 1.0 / ((double)nmult);
				double tm = step * (row + 1);
				double frac = tm - ((double)(int)tm);
				if (frac == 0.0)
					return wxString::Format("%lg", tm);
				else
					return wxString::Format("   .%lg", frac * 60);
			}
		}
		*/
		return wxString::Format("%d", row + 1);
	}

	virtual wxString GetColLabelValue(int col)
	{
		wxString label = "Value";
		if (col > -1 && col < (int)columnLabels.GetCount())
			label = columnLabels[col];
		return label;
	}

	virtual wxString GetTypeName(int, int)
	{
		return wxGRID_VALUE_STRING;
	}

	virtual bool CanGetValueAs(int, int, const wxString& typeName)
	{
		return typeName == wxGRID_VALUE_STRING;
	}

	virtual bool CanSetValueAs(int, int, const wxString& typeName)
	{
		return typeName == wxGRID_VALUE_STRING;
	}

	virtual bool AppendRows(size_t nrows)
	{
		if (d_mat && nrows > 0)
		{
			if (d_mat->nrows() + nrows > d_mat->nrows())
				d_mat->resize_preserve(d_mat->nrows() + nrows, d_mat->ncols(), 0.0);

			if (GetView())
			{
				wxGridTableMessage msg(this,
					wxGRIDTABLE_NOTIFY_ROWS_APPENDED,
					nrows);

				GetView()->ProcessTableMessage(msg);
			}
		}

		return true;
	}


	virtual bool AppendCols(size_t ncols)
	{
		if (d_mat && ncols > 0)
		{
			if (d_mat->ncols() + ncols > d_mat->ncols())
				d_mat->resize_preserve(d_mat->nrows(), d_mat->ncols() + ncols, 0.0);

			if (GetView())
			{
				wxGridTableMessage msg(this,
					wxGRIDTABLE_NOTIFY_COLS_APPENDED,
					ncols);

				GetView()->ProcessTableMessage(msg);
			}
		}

		return true;
	}


	virtual bool InsertRows(size_t pos, size_t nrows)
	{

		if (!d_mat) return true;

		if (pos > d_mat->nrows()) pos = d_mat->nrows();

		d_mat->resize_preserve(d_mat->nrows()+nrows, d_mat->ncols(), 0.0);

		for (size_t i = pos + nrows; i < d_mat->nrows(); i++)
			for (size_t j = 0; j < d_mat->ncols(); j++)
				d_mat->at(i, j) = d_mat->at(i-nrows,j);
		for (size_t i = pos; i < pos + nrows - 1; i++)
			for (size_t j = 0; j < d_mat->ncols(); j++)
				d_mat->at(i, j) = 0.0;

		if (GetView())
		{
			wxGridTableMessage msg(this,
				wxGRIDTABLE_NOTIFY_ROWS_INSERTED,
				pos,
				nrows);

			GetView()->ProcessTableMessage(msg);
		}

		return true;
	}

	virtual bool InsertCols(size_t pos, size_t ncols)
	{

		if (!d_mat) return true;

		if (pos > d_mat->ncols()) pos = d_mat->ncols();

		d_mat->resize_preserve(d_mat->nrows(), d_mat->ncols()+ ncols, 0.0);

		for (size_t i = 0; i < d_mat->nrows(); i++)
			for (size_t j = pos+ncols; j < d_mat->ncols(); j++)
				d_mat->at(i, j) = d_mat->at(i, j - ncols);
		for (size_t i = 0; i < d_mat->nrows(); i++)
			for (size_t j = pos; j < pos + ncols - 1; j++)
				d_mat->at(i, j) = 0.0;

		if (GetView())
		{
			wxGridTableMessage msg(this,
				wxGRIDTABLE_NOTIFY_COLS_INSERTED,
				pos,
				ncols);

			GetView()->ProcessTableMessage(msg);
		}

		return true;
	}

	virtual bool DeleteRows(size_t pos, size_t nrows)
	{
		if (!d_mat) return true;

		if (nrows > d_mat->nrows() - pos)
			nrows = d_mat->nrows() - pos;

		for (size_t i = pos; (i + nrows) < d_mat->nrows() && i < (pos + nrows); i++)
			for (size_t j = 0; j < d_mat->ncols(); j++)
				d_mat->at(i, j) = d_mat->at(i + nrows, j);

		d_mat->resize_preserve(d_mat->nrows() - nrows, d_mat->ncols(), 0.0);

		if (GetView())
		{
			//	applog("RowCount Post Delete %d :: %d\n", Stage->ElementList.size(), this->GetNumberRows());
			wxGridTableMessage msg(this,
				wxGRIDTABLE_NOTIFY_ROWS_DELETED,
				pos,
				nrows);

			GetView()->ProcessTableMessage(msg);
		}

		return true;
	}

	virtual bool DeleteCols(size_t pos, size_t ncols)
	{
		if (!d_mat) return true;

		if (ncols > d_mat->ncols() - pos)
			ncols = d_mat->ncols() - pos;

		for (size_t i = 0; i < d_mat->nrows(); i++)
			for (size_t j = pos; (j + ncols) < d_mat->ncols() && j < (pos + ncols); j++)
				d_mat->at(i, j) = d_mat->at(i, j+ncols);

		d_mat->resize_preserve(d_mat->nrows(), d_mat->ncols() - ncols, 0.0);

		if (GetView())
		{
			//	applog("RowCount Post Delete %d :: %d\n", Stage->ElementList.size(), this->GetNumberRows());
			wxGridTableMessage msg(this,
				wxGRIDTABLE_NOTIFY_COLS_DELETED,
				pos,
				ncols);

			GetView()->ProcessTableMessage(msg);
		}

		return true;
	}
};




enum { ILDM_GRID = wxID_HIGHEST + 945, ILDM_MODEOPTIONS, ILDM_TIMESTEPS, ILDM_SINGLEVALUE, ILDM_COPY, ILDM_PASTE, ILDM_IMPORT, ILDM_EXPORT };

class AFDataLifetimeMatrixDialog : public wxDialog
{
private:
	wxString mLabel;
	wxArrayString mColumnLabels;
	size_t mAnalysisPeriod, mMinPerHour, mMode, mNumCols;
	matrix_t<double> mData;
	wxExtGridCtrl *Grid;
	AFDataLifetimeMatrixTable *GridTable;
	wxStaticText *Description, *AnalysisPeriodLabel;
	wxStaticText *InputLabel, *TimestepsLabel;
	wxComboBox *ModeOptions;
	wxComboBox *Timesteps;
	wxNumericCtrl *AnalysisPeriodValue;

public:
	AFDataLifetimeMatrixDialog(wxWindow *parent, const wxString &title, const wxString &desc, const wxString &inputLabel, const wxString &columnLabels, const bool &optannual = false, const bool &optweekly = false)
		: wxDialog(parent, wxID_ANY, title, wxDefaultPosition, wxScaleSize(430, 510),
			wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
	{
		mLabel = inputLabel;
		mColumnLabels = wxSplit(columnLabels, ',');
		mNumCols = mColumnLabels.Count();
		if (mNumCols < 1) mNumCols = 1;
		
		GridTable = NULL;
		wxButton *btn = NULL;
		Grid = new wxExtGridCtrl(this, ILDM_GRID);
		Grid->DisableDragCell();
		//Grid->DisableDragColSize();
		Grid->DisableDragRowSize();
		Grid->DisableDragColMove();
		Grid->DisableDragGridSize();
		Grid->SetRowLabelAlignment(wxALIGN_LEFT, wxALIGN_CENTER);

		wxBoxSizer *szh_btns = new wxBoxSizer(wxHORIZONTAL);
		wxBoxSizer *szv_copypaste = new wxBoxSizer(wxVERTICAL);
		btn = new wxButton(this, ILDM_COPY, "Copy");
		szv_copypaste->Add(btn, 0, wxALL | wxEXPAND, 1);
		btn = new wxButton(this, ILDM_PASTE, "Paste");
		szv_copypaste->Add(btn, 0, wxALL | wxEXPAND, 1);
		wxBoxSizer *szv_exportimport = new wxBoxSizer(wxVERTICAL);
		btn = new wxButton(this, ILDM_EXPORT, "Export");
		szv_exportimport->Add(btn, 0, wxALL | wxEXPAND, 1);
		btn = new wxButton(this, ILDM_IMPORT, "Import");
		szv_exportimport->Add(btn, 0, wxALL | wxEXPAND, 1);
		szh_btns->Add(szv_copypaste);
		szh_btns->Add(szv_exportimport);
		szh_btns->AddStretchSpacer();


		wxBoxSizer *szh_top4 = new wxBoxSizer(wxHORIZONTAL);
		wxArrayString timestepmin;
		timestepmin.Add(" 1");
		timestepmin.Add(" 5");
		timestepmin.Add("10");
		timestepmin.Add("15");
		timestepmin.Add("20");
		timestepmin.Add("30");
		Timesteps = new wxComboBox(this, ILDM_TIMESTEPS, "30", wxDefaultPosition, wxDefaultSize, timestepmin);
		TimestepsLabel = new wxStaticText(this, -1, "Select timestep minutes");
		szh_top4->Add(TimestepsLabel, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
		szh_top4->AddSpacer(3);
		szh_top4->Add(Timesteps, 0, wxALL | wxEXPAND, 1);
		szh_top4->AddStretchSpacer();

		wxBoxSizer *szh_top3 = new wxBoxSizer(wxHORIZONTAL);
		wxArrayString modes;
		modes.Add("Single Value");
		modes.Add("Monthly");
		modes.Add("Daily");
		modes.Add("Hourly");
		modes.Add("Subhourly");
		if (optannual)	modes.Add("Annual");
		if (optweekly) modes.Add("Weekly");
		ModeOptions = new wxComboBox(this, ILDM_MODEOPTIONS, "Monthly", wxDefaultPosition, wxDefaultSize, modes);
		szh_top3->Add(new wxStaticText(this, -1, "Mode"), 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
		szh_top3->AddSpacer(3);
		szh_top3->Add(ModeOptions, 0, wxALL | wxEXPAND, 1);
		szh_top3->AddStretchSpacer();

		wxBoxSizer *szh_top1 = new wxBoxSizer(wxHORIZONTAL);
		InputLabel = new wxStaticText(this, wxID_ANY, mLabel);
		szh_top1->AddSpacer(3);
		szh_top1->Add(InputLabel, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
		szh_top1->AddStretchSpacer();

		wxBoxSizer *szh_top2 = new wxBoxSizer(wxHORIZONTAL);
		AnalysisPeriodValue = new wxNumericCtrl(this, wxID_ANY);
		AnalysisPeriodValue->Enable(false);
		AnalysisPeriodLabel = new wxStaticText(this, -1, "Analysis period");
		szh_top2->Add(AnalysisPeriodLabel, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
		szh_top2->AddSpacer(3);
		szh_top2->Add(AnalysisPeriodValue, 0, wxALL | wxEXPAND, 1);
		szh_top2->AddStretchSpacer();


		wxBoxSizer *szv_main_vert = new wxBoxSizer(wxVERTICAL);
		szv_main_vert->Add(szh_top1, 0, wxALL | wxEXPAND, 4);
		szv_main_vert->Add(szh_top2, 0, wxALL | wxEXPAND, 4);
		szv_main_vert->Add(szh_top3, 0, wxALL | wxEXPAND, 4);
		szv_main_vert->Add(szh_top4, 0, wxALL | wxEXPAND, 4);
		szv_main_vert->Add(szh_btns, 0, wxALL | wxEXPAND, 4);
		szv_main_vert->AddStretchSpacer();
		szv_main_vert->Add(CreateButtonSizer(wxOK | wxCANCEL), 0, wxALL | wxEXPAND, 10);
		Description = 0;
		if (!desc.IsEmpty())
		{
			Description = new wxStaticText(this, wxID_ANY, desc);
			Description->Wrap(350);
			szv_main_vert->Add(Description, 0, wxALL, 10);
		}

		wxBoxSizer *szh_main = new wxBoxSizer(wxHORIZONTAL);
		szh_main->Add(szv_main_vert, 0, wxALL | wxEXPAND, 4);
		szh_main->Add(Grid, 3, wxALL | wxEXPAND, 4);

		SetSizer(szh_main);
		SetSizeHints(wxSize(1000, 800));
	}

	void SetMode(int m)
	{
		mMode = m;
		size_t l;
	//	Grid->Freeze();
		switch (mMode)
		{
		case DATA_LIFETIME_MATRIX_MONTHLY:
		{
			l = mAnalysisPeriod * 12;
			Grid->ResizeGrid(l, mNumCols);
			break;
		}
		case DATA_LIFETIME_MATRIX_DAILY: // assume 365
		{
			l = mAnalysisPeriod * 365;
			Grid->ResizeGrid(l, mNumCols);
			break;
		}
		case DATA_LIFETIME_MATRIX_HOURLY: // assume 8760
		{
			l = mAnalysisPeriod * 8760;
			Grid->ResizeGrid(l, mNumCols);
			break;
		}
		case DATA_LIFETIME_MATRIX_SUBHOURLY: // assume 8760 * timesteps per hour
		{
			// error handling
			mMinPerHour = std::stoul(Timesteps->GetValue().ToStdString());
			l = mAnalysisPeriod * 8760 * (60 / mMinPerHour);
			Grid->ResizeGrid(l, mNumCols);
			break;
		}
		case DATA_LIFETIME_MATRIX_ANNUAL:
		{
			l = mAnalysisPeriod;
			Grid->ResizeGrid(l, mNumCols);
			break;
		}
		case DATA_LIFETIME_MATRIX_WEEKLY: // assume 52 weeks or 364 days?
		{
			l = mAnalysisPeriod * 8760 / (24 * 7);
			Grid->ResizeGrid(l, mNumCols);
			break;
		}
		default: // single value - no grid resize
		{
			l = 1;
			Grid->ResizeGrid(l, mNumCols);
			break;
		}
		}
		TimestepsLabel->Show((mMode == DATA_LIFETIME_MATRIX_SUBHOURLY));
		Timesteps->Show((mMode == DATA_LIFETIME_MATRIX_SUBHOURLY));
		ModeOptions->SetSelection(mMode);

//		Grid->AutoSize();
		Grid->Layout();
		Grid->Refresh();
//		GetSizer()->Layout();
//		Grid->Thaw();
		Layout();
//		Move(GetPosition().x - 1, GetPosition().y - 1);
		//	Fit(); // entire screen height - set max height
	}

	int GetMode()
	{
		return mMode;
	}

	void SetData(matrix_t<double> &data)
	{
		mData = data;

		if (GridTable) GridTable->SetMatrix(NULL);
		Grid->SetTable(NULL);

		GridTable = new AFDataLifetimeMatrixTable(&mData, mMode, mColumnLabels);
		GridTable->SetAttrProvider(new wxExtGridCellAttrProvider);

		Grid->SetTable(GridTable, true);
		// can use max text width from column labels
		for (size_t ic=0; ic<(size_t)Grid->GetNumberCols(); ic++)
			Grid->SetColSize(ic, (int)(140 * wxGetScreenHDScale()));


//		Grid->SetColSize(0, (int)(130 * wxGetScreenHDScale()));

		// determine mode from data
		size_t dataSize = mData.nrows();
		if (dataSize < 1)
		{
			mData.at(0,0) = 0.0;
			dataSize = 1;
		}
		if (dataSize == 1)
			mMode = DATA_LIFETIME_MATRIX_SINGLEVALUE;
		else if (dataSize == (mAnalysisPeriod))
			mMode = DATA_LIFETIME_MATRIX_ANNUAL;
		else if (dataSize == (mAnalysisPeriod * 12))
			mMode = DATA_LIFETIME_MATRIX_MONTHLY;
		else if (dataSize == (mAnalysisPeriod * 8760 / (24 * 7)))
			mMode = DATA_LIFETIME_MATRIX_WEEKLY;
		else if (dataSize == (mAnalysisPeriod * 365))
			mMode = DATA_LIFETIME_MATRIX_DAILY;
		else if (dataSize == (mAnalysisPeriod * 8760))
			mMode = DATA_LIFETIME_MATRIX_HOURLY;
		else
		{
			mMode = DATA_LIFETIME_MATRIX_SUBHOURLY;
			size_t stepsPerHour = dataSize / (mAnalysisPeriod * 8760);
			switch (stepsPerHour)
			{
			case 2: // 30 minute
				Timesteps->SetSelection(5);
				break;
			case 3: // 20 minute
				Timesteps->SetSelection(4);
				break;
			case 4: // 15 minute
				Timesteps->SetSelection(3);
				break;
			case 6: // 10 minute
				Timesteps->SetSelection(2);
				break;
			case 12: // 5 minute
				Timesteps->SetSelection(1);
				break;
			default: // 1 minute
				Timesteps->SetSelection(0);
				break;
			}
		}

		SetMode(mMode);
		Move(GetPosition().x + 1, GetPosition().y + 1);
		Move(GetPosition().x - 1, GetPosition().y - 1);

	}

	void GetData(matrix_t<double> &data)
	{
		data = mData;
	}

	void SetDataLabel(const wxString &s)
	{
		mLabel = s;
		InputLabel->SetLabel(s);
	}

	wxString GetDataLabel()
	{
		return mLabel;
	}

	void SetAnalysisPeriod(const size_t &p)
	{
		mAnalysisPeriod = p;
		AnalysisPeriodValue->SetValue((double)p);
		//		SetMode(mMode);
	}

	size_t GetAnalysisPeriod()
	{
		return mAnalysisPeriod;
	}

	void SetMinPerHour(const size_t &p)
	{
		mMinPerHour = p;
	}

	size_t GetMinPerHour()
	{
		return mMinPerHour;
	}


	void OnCommand(wxCommandEvent &evt)
	{
		if (evt.GetId() == ILDM_MODEOPTIONS)
			SetMode(ModeOptions->GetSelection());
		else if (evt.GetId() == ILDM_TIMESTEPS)
			SetMode(ModeOptions->GetSelection());
		else if (evt.GetId() == ILDM_COPY)
			Grid->Copy(true);
		else if (evt.GetId() == ILDM_PASTE)
			Grid->Paste(wxExtGridCtrl::PASTE_ALL);
		else if (evt.GetId() == ILDM_IMPORT)
		{
			wxFileDialog dlg(this, "Select data file to import");
			if (dlg.ShowModal() != wxID_OK) return;
			FILE *fp = fopen(dlg.GetPath().c_str(), "r");
			if (!fp)
			{
				wxMessageBox("Could not open file for reading:\n\n" + dlg.GetPath());
				return;
			}

			matrix_t<double> arr;
			arr.resize(mData.nrows(), mData.ncols());

			char buf[128];
			fgets(buf, 127, fp); // skip header line

			bool error = false;
			for (int i = 0; i < (int)mData.nrows(); i++)
			{
				if (fgets(buf, 127, fp) == NULL)
				{
					wxMessageBox(wxString::Format("Data file does not contain %d data value lines, only %d found.\n\nNote that the first line in the file is considered a header label and is ignored.", mData.nrows(), i));
					error = true;
					break;
				}

//				arr.push_back((double)atof(buf));
			}

			if (!error) SetData(arr);

			fclose(fp);
		}
		else if (evt.GetId() == ILDM_EXPORT)
		{
			wxFileDialog dlg(this, "Select data file to export to", wxEmptyString, wxEmptyString, "*.*", wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
			if (dlg.ShowModal() != wxID_OK) return;

			FILE *fp = fopen(dlg.GetPath().c_str(), "w");
			if (!fp)
			{
				wxMessageBox("Could not open file for writing.");
				return;
			}
/*
			fprintf(fp, "Exported Data (%d)\n", (int)mData.size());
			for (size_t i = 0; i < mData.size(); i++)
				fprintf(fp, "%g\n", mData[i]);
			fclose(fp);
*/		}
	}

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE(AFDataLifetimeMatrixDialog, wxDialog)
EVT_BUTTON(ILDM_COPY, AFDataLifetimeMatrixDialog::OnCommand)
EVT_BUTTON(ILDM_PASTE, AFDataLifetimeMatrixDialog::OnCommand)
EVT_BUTTON(ILDM_IMPORT, AFDataLifetimeMatrixDialog::OnCommand)
EVT_BUTTON(ILDM_EXPORT, AFDataLifetimeMatrixDialog::OnCommand)
EVT_COMBOBOX(ILDM_MODEOPTIONS, AFDataLifetimeMatrixDialog::OnCommand)
EVT_COMBOBOX(ILDM_TIMESTEPS, AFDataLifetimeMatrixDialog::OnCommand)
END_EVENT_TABLE()

BEGIN_EVENT_TABLE(AFDataLifetimeMatrixButton, wxButton)
EVT_BUTTON(wxID_ANY, AFDataLifetimeMatrixButton::OnPressed)
END_EVENT_TABLE()

AFDataLifetimeMatrixButton::AFDataLifetimeMatrixButton(wxWindow *parent, int id, const wxPoint &pos, const wxSize &size)
	: wxButton(parent, id, "Edit time series data...", pos, size)
{
	mAnalysisPeriod = 25;
	mMinPerHour = 30;
	mMode = DATA_LIFETIME_MATRIX_MONTHLY;
	mData.resize_preserve(12 * mAnalysisPeriod, 2, 0.0);
}

void AFDataLifetimeMatrixButton::Get(matrix_t<double> &data)
{
	data = mData;
}
void AFDataLifetimeMatrixButton::Set(const matrix_t<double> &data)
{
	mData = data;
	// resize based on potentially new analysis period and current mode
	size_t newSize = mAnalysisPeriod;
	switch (mMode)
	{
	case DATA_LIFETIME_MATRIX_MONTHLY:
	{
		newSize = mAnalysisPeriod * 12;
		break;
	}
	case DATA_LIFETIME_MATRIX_DAILY: // assume 365
	{
		newSize = mAnalysisPeriod * 365;
		break;
	}
	case DATA_LIFETIME_MATRIX_HOURLY: // assume 8760
	{
		newSize = mAnalysisPeriod * 8760;
		break;
	}
	case DATA_LIFETIME_MATRIX_SUBHOURLY: // assume 8760 * timesteps per hour
	{
		newSize = mAnalysisPeriod * 8760 * (60 / mMinPerHour);
		break;
	}
	case DATA_LIFETIME_MATRIX_ANNUAL:
	{
		newSize = mAnalysisPeriod;
		break;
	}
	case DATA_LIFETIME_MATRIX_WEEKLY: // assume 52 weeks or 364 days?
	{
		newSize = mAnalysisPeriod * 8760 / (24 * 7);
		break;
	}
	default: // single value - no grid resize
	{
		newSize = 1;
		break;
	}
	}
	if (mData.nrows() != newSize)
		mData.resize(newSize, mData.ncols());
}
void AFDataLifetimeMatrixButton::SetDataLabel(const wxString &s)
{
	mDataLabel = s;
}
wxString AFDataLifetimeMatrixButton::GetDataLabel()
{
	return mDataLabel;
}

void AFDataLifetimeMatrixButton::SetColumnLabels(const wxString &s)
{
	mColumnLabels = s;
}
wxString AFDataLifetimeMatrixButton::GetColumnLabels()
{
	return mColumnLabels;
}


void AFDataLifetimeMatrixButton::OnPressed(wxCommandEvent &evt)
{
	// resize based on potentially new analysis period and current mode
	size_t newSize = mAnalysisPeriod;
	switch (mMode)
	{
	case DATA_LIFETIME_MATRIX_MONTHLY:
	{
		newSize = mAnalysisPeriod * 12;
		break;
	}
	case DATA_LIFETIME_MATRIX_DAILY: // assume 365
	{
		newSize = mAnalysisPeriod * 365;
		break;
	}
	case DATA_LIFETIME_MATRIX_HOURLY: // assume 8760
	{
		newSize = mAnalysisPeriod * 8760;
		break;
	}
	case DATA_LIFETIME_MATRIX_SUBHOURLY: // assume 8760 * timesteps per hour
	{
		newSize = mAnalysisPeriod * 8760 * (60 / mMinPerHour);
		break;
	}
	case DATA_LIFETIME_MATRIX_ANNUAL:
	{
		newSize = mAnalysisPeriod;
		break;
	}
	case DATA_LIFETIME_MATRIX_WEEKLY: // assume 52 weeks or 364 days?
	{
		newSize = mAnalysisPeriod * 8760 / (24 * 7);
		break;
	}
	default: // single value - no grid resize
	{
		newSize = 1;
		break;
	}
	}
	if (mData.nrows() != newSize)
		mData.resize_preserve(newSize, mData.ncols(), 0.0);

	AFDataLifetimeMatrixDialog dlg(this, "Edit Time Series Data", mDescription, mDataLabel, mColumnLabels, mAnnualEnabled, mWeeklyEnabled);
	dlg.SetAnalysisPeriod(mAnalysisPeriod);
	dlg.SetData(mData);
	dlg.SetDataLabel(mDataLabel);


	if (dlg.ShowModal() == wxID_OK)
	{
		dlg.GetData(mData);
		mMode = dlg.GetMode();
		mMinPerHour = dlg.GetMinPerHour();
		evt.Skip(); // allow event to propagate indicating underlying value changed
	}
}






enum { IDSD_GRID = wxID_HIGHEST + 945, IDSD_CHANGENUMROWS, IDSD_COPY, IDSD_PASTE, IDSD_IMPORT, IDSD_EXPORT };

class AFStringArrayDialog : public wxDialog
{
private:
	wxString mLabel;
	int mMode;
	wxArrayString *mData;
	wxExtGridCtrl *Grid;
	AFStringArrayTable *GridTable;
	wxStaticText *ModeLabel;
	wxStaticText *Description;
	wxButton *ButtonChangeRows;

public:
	AFStringArrayDialog(wxWindow *parent, const wxString &title, const wxString &desc, const wxString &collabel)
		: wxDialog(parent, wxID_ANY, title, wxDefaultPosition, wxScaleSize(430, 510),
			wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
	{
		mLabel = collabel;

		GridTable = NULL;
		wxButton *btn = NULL;
		Grid = new wxExtGridCtrl(this, IDSD_GRID);
		Grid->DisableDragCell();
		//Grid->DisableDragColSize();
		Grid->DisableDragRowSize();
		Grid->DisableDragColMove();
		Grid->DisableDragGridSize();
		Grid->SetRowLabelAlignment(wxALIGN_LEFT, wxALIGN_CENTER);

		wxBoxSizer *szh_top1 = new wxBoxSizer(wxHORIZONTAL);
		btn = new wxButton(this, IDSD_COPY, "Copy");
		szh_top1->Add(btn, 0, wxALL | wxEXPAND, 1);
		btn = new wxButton(this, IDSD_PASTE, "Paste");
		szh_top1->Add(btn, 0, wxALL | wxEXPAND, 1);
		btn = new wxButton(this, IDSD_IMPORT, "Import");
		szh_top1->Add(btn, 0, wxALL | wxEXPAND, 1);
		btn = new wxButton(this, IDSD_EXPORT, "Export");
		szh_top1->Add(btn, 0, wxALL | wxEXPAND, 1);
		szh_top1->AddStretchSpacer();

		wxBoxSizer *szh_top2 = new wxBoxSizer(wxHORIZONTAL);
		ButtonChangeRows = new wxButton(this, IDSD_CHANGENUMROWS, "Number of Values...");
		szh_top2->Add(ButtonChangeRows, 0, wxALL | wxEXPAND, 1);
		ModeLabel = new wxStaticText(this, -1, "");
		szh_top2->AddSpacer(3);
		szh_top2->Add(ModeLabel, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
		szh_top2->AddStretchSpacer();

		wxBoxSizer *szv_main = new wxBoxSizer(wxVERTICAL);
		// reverse order per Paul email 2/10/15
		szv_main->Add(szh_top2, 0, wxALL | wxEXPAND, 4);
		szv_main->Add(szh_top1, 0, wxALL | wxEXPAND, 4);
		szv_main->Add(Grid, 1, wxALL | wxEXPAND, 4);
		Description = 0;
		if (!desc.IsEmpty())
		{
			Description = new wxStaticText(this, wxID_ANY, desc);
			Description->Wrap(350);
			szv_main->Add(Description, 0, wxALL, 10);
		}
		szv_main->Add(CreateButtonSizer(wxOK | wxCANCEL), 0, wxALL | wxEXPAND, 10);


		SetSizer(szv_main);
	}


	void SetData( wxArrayString *data)
	{
		mData = data;

		Grid->SetTable(NULL);

		GridTable = new AFStringArrayTable(mData, mLabel);
		GridTable->SetAttrProvider(new wxExtGridCellAttrProvider);

		Grid->SetTable(GridTable, true);
		Grid->SetColSize(0, (int)(130 * wxGetScreenHDScale()));

		Grid->Layout();
		Grid->Refresh();
	}

	wxArrayString *GetData()
	{
		return mData;
	}

	void SetStringLabel(const wxString &s)
	{
		mLabel = s;
	}

	wxString GetStringLabel()
	{
		return mLabel;
	}

	void OnCommand(wxCommandEvent &evt)
	{
		if (evt.GetId() == IDSD_CHANGENUMROWS)
		{
			long l = 0;
				wxString result = wxGetTextFromUser("Enter number of data rows", "Edit Table",
					wxString::Format("%d", Grid->GetNumberRows()), this);
				if (result.IsEmpty()) return;

				if (!result.ToLong(&l))
					return;

			if (l > 0)
			{
				Grid->ResizeGrid(l, 1);
			}
			else
				wxMessageBox("Invalid number of rows or non-numeric entry.");
		}
		else if (evt.GetId() == IDSD_COPY)
			Grid->Copy(true);
		else if (evt.GetId() == IDSD_PASTE)
			Grid->Paste(wxExtGridCtrl::PASTE_ALL_RESIZE_ROWS);
		else if (evt.GetId() == IDSD_IMPORT)
		{
			wxFileDialog dlg(this, "Select data file to import");
			if (dlg.ShowModal() != wxID_OK) return;
			FILE *fp = fopen(dlg.GetPath().c_str(), "r");
			if (!fp)
			{
				wxMessageBox("Could not open file for reading:\n\n" + dlg.GetPath());
				return;
			}
			wxArrayString *arr = GetData();
			arr->Clear();
			char buf[128];
			while (fgets(buf, 127, fp) != NULL)
			{
				buf[strcspn(buf, "\r\n")] = 0;
				arr->push_back(buf);
			}
			fclose(fp);
			SetData(arr);
		}
		else if (evt.GetId() == IDSD_EXPORT)
		{
			wxFileDialog dlg(this, "Select data file to export to", wxEmptyString, wxEmptyString, "*.*", wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
			if (dlg.ShowModal() != wxID_OK) return;

			FILE *fp = fopen(dlg.GetPath().c_str(), "w");
			if (!fp)
			{
				wxMessageBox("Could not open file for writing.");
				return;
			}

			for (size_t i = 0; i<mData->Count(); i++)
				fprintf(fp, "%s\n", (const char *)mData->Item(i).c_str());
			fclose(fp);
		}
	}

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE(AFStringArrayDialog, wxDialog)
EVT_BUTTON(IDSD_COPY, AFStringArrayDialog::OnCommand)
EVT_BUTTON(IDSD_PASTE, AFStringArrayDialog::OnCommand)
EVT_BUTTON(IDSD_IMPORT, AFStringArrayDialog::OnCommand)
EVT_BUTTON(IDSD_EXPORT, AFStringArrayDialog::OnCommand)
EVT_BUTTON(IDSD_CHANGENUMROWS, AFStringArrayDialog::OnCommand)
END_EVENT_TABLE()

BEGIN_EVENT_TABLE(AFStringArrayButton, wxButton)
EVT_BUTTON(wxID_ANY, AFStringArrayButton::OnPressed)
END_EVENT_TABLE()

AFStringArrayButton::AFStringArrayButton(wxWindow *parent, int id, const wxPoint &pos, const wxSize &size)
	: wxButton(parent, id, "Edit time series data...", pos, size)
{
}

void AFStringArrayButton::Get(wxString &data)
{
	data = wxJoin(mData, '|');
}
void AFStringArrayButton::Set(const wxString &data)
{
	mData = wxSplit(data, '|');
}
void AFStringArrayButton::SetStringLabel(const wxString &s)
{
	mStringLabel = s;
}
wxString AFStringArrayButton::GetStringLabel()
{
	return mStringLabel;
}


void AFStringArrayButton::OnPressed(wxCommandEvent &evt)
{
	AFStringArrayDialog dlg(this, "Edit TIme Series Data", m_description, mStringLabel);

	dlg.SetStringLabel(mStringLabel);
	dlg.SetData(&mData);

	if (dlg.ShowModal() == wxID_OK)
	{
		mData = *dlg.GetData();
		evt.Skip(); // allow event to propagate indicating underlying value changed
	}
}





wxVerticalLabel::wxVerticalLabel(wxWindow* parent,
	wxWindowID id,
	const wxString& label,
	const wxPoint& pos,
	const wxSize& size,
	long style,
	const wxString& name)
	: wxPanel(parent, id, pos, size, style, name),
	m_Label(label)
{
	SetBackgroundStyle(wxBG_STYLE_PAINT);
	UpdateSize();
	Connect(GetId(), wxEVT_PAINT, wxPaintEventHandler(wxVerticalLabel::OnPaint));
}

wxVerticalLabel::~wxVerticalLabel()
{

}

void wxVerticalLabel::SetLabel(const wxString &label)
{
	m_Label = label;
	UpdateSize();
}

void wxVerticalLabel::UpdateSize()
{
	wxSize size = GetTextExtent(m_Label);
	this->SetMinSize(wxSize(size.y, size.x + 5));
}

void wxVerticalLabel::OnPaint(wxPaintEvent& )
{
	wxAutoBufferedPaintDC dc(this);
	dc.Clear();
	wxSize size = GetMinSize();
	dc.DrawRotatedText(m_Label, 0, size.y, 90);
}


wxHorizontalLabel::wxHorizontalLabel(wxWindow* parent,
	wxWindowID id,
	const wxString& label,
	const wxPoint& pos,
	const wxSize& size,
	long style,
	const wxString& name)
	: wxPanel(parent, id, pos, size, style, name),
	m_Label(label)
{
	SetBackgroundStyle(wxBG_STYLE_PAINT);
	UpdateSize();
	Connect(GetId(), wxEVT_PAINT, wxPaintEventHandler(wxHorizontalLabel::OnPaint));
}

wxHorizontalLabel::~wxHorizontalLabel()
{

}

void wxHorizontalLabel::SetLabel(const wxString &label)
{
	m_Label = label;
	UpdateSize();
}

void wxHorizontalLabel::UpdateSize()
{
	wxSize size = GetTextExtent(m_Label);
	this->SetMinSize(wxSize(size.x, size.y));
}

void wxHorizontalLabel::OnPaint(wxPaintEvent& )
{
	wxAutoBufferedPaintDC dc(this);
	dc.Clear();
	wxSize size = GetMinSize();
	dc.DrawText(m_Label, 0, 0);
}













/* Extended Data Matrix Control with choice column */

DEFINE_EVENT_TYPE(wxEVT_AFDataMatrixCtrl_CHANGE)

enum { IDEDMC_NUMROWS = wxID_HIGHEST + 857, IDEDMC_NUMCOLS, IDEDMC_GRID, IDEDMC_IMPORT, IDEDMC_EXPORT, IDEDMC_COPY, IDEDMC_PASTE };

BEGIN_EVENT_TABLE(AFDataMatrixCtrl, wxPanel)
EVT_NUMERIC(IDEDMC_NUMROWS, AFDataMatrixCtrl::OnRowsColsChange)
EVT_NUMERIC(IDEDMC_NUMCOLS, AFDataMatrixCtrl::OnRowsColsChange)
EVT_BUTTON(IDEDMC_IMPORT, AFDataMatrixCtrl::OnCommand)
EVT_BUTTON(IDEDMC_EXPORT, AFDataMatrixCtrl::OnCommand)
EVT_BUTTON(IDEDMC_COPY, AFDataMatrixCtrl::OnCommand)
EVT_BUTTON(IDEDMC_PASTE, AFDataMatrixCtrl::OnCommand)
EVT_GRID_CMD_CELL_CHANGED(IDEDMC_GRID, AFDataMatrixCtrl::OnCellChange)
END_EVENT_TABLE()

AFDataMatrixCtrl::AFDataMatrixCtrl(wxWindow *parent, int id,
const wxPoint &pos,
const wxSize &sz,
bool sidebuttons,
const wxString &collabels,
const wxString &rowlabels,
const wxString &choices,
const int &choice_col,
bool bottombuttons, 
const wxString &horizontalLabel,
const wxString &verticalLabel)
: wxPanel(parent, id, pos, sz)
{
	m_pasteappendrows = false;
	m_pasteappendcols = false;
	m_colLabels = collabels;
	m_rowLabels = rowlabels;
	m_showColLabels = false;
	m_showRowLabels = false;
	m_shadeR0C0 = true;
	m_shadeC0 = true;
	m_showcols = true;
	m_colorMap = false;
	m_rowY2 = m_rowY1 = m_rowY0 = 0.0;
	m_colY2 = m_colY1 = m_colY0 = 0.0;

	m_rowFormat = "r#";
	m_rowY1 = 1.0;
	m_colFormat = "c#";
	m_colY1 = 1.0;

	m_minVal = m_maxVal = 0.0f;
	m_caption = new wxStaticText(this, wxID_ANY, "");
	wxFont f(wxFontInfo(10).Bold(true).Family(wxFONTFAMILY_DEFAULT));
	m_horizontalLabel = new wxHorizontalLabel(this, wxID_ANY, horizontalLabel);
	m_horizontalLabel->SetBackgroundColour(*wxWHITE);
	m_horizontalLabel->SetForegroundColour(*wxBLACK);
	m_horizontalLabel->SetFont(f);
	m_horizontalLabel->SetLabel(horizontalLabel);
	m_verticalLabel = new wxVerticalLabel(this, wxID_ANY, verticalLabel);
	m_verticalLabel->SetBackgroundColour(*wxWHITE);
	m_verticalLabel->SetForegroundColour(*wxBLACK);
	m_verticalLabel->SetFont(f);
	m_verticalLabel->SetLabel(verticalLabel);

	m_data.resize_fill(8, 6, 0.0f);

	m_numRows = new wxNumericCtrl(this, IDEDMC_NUMROWS, m_data.nrows(), wxNUMERIC_INTEGER);
	m_numCols = new wxNumericCtrl(this, IDEDMC_NUMCOLS, m_data.ncols(), wxNUMERIC_INTEGER);
	m_labelRows = new wxStaticText(this, wxID_ANY, "Rows:");
	m_labelCols = new wxStaticText(this, wxID_ANY, "Cols:");

	m_btnImport = new wxButton(this, IDEDMC_IMPORT, "Import...");
	m_btnExport = new wxButton(this, IDEDMC_EXPORT, "Export...");
	m_btnCopy = new wxButton(this, IDEDMC_COPY, "Copy");
	m_btnPaste = new wxButton(this, IDEDMC_PASTE, "Paste");


	m_grid = new wxExtGridCtrl(this, IDEDMC_GRID);
	m_grid->CreateGrid(8, 12);
	m_grid->EnableCopyPaste(true);
	m_grid->EnablePasteEvent(true);
	m_grid->DisableDragCell();
	m_grid->DisableDragRowSize();
	m_grid->DisableDragColMove();
	m_grid->DisableDragGridSize();
	m_grid->SetRowLabelAlignment(wxALIGN_LEFT, wxALIGN_CENTER);
	m_grid->GetTable()->SetAttrProvider(new wxExtGridCellAttrProvider(m_shadeR0C0, true, m_shadeC0, !m_colorMap));

#ifndef S3D_STANDALONE
	m_grid->RegisterDataType("GridCellChoice", new GridCellChoiceRenderer(choices), new GridCellChoiceEditor(choices));
#endif

	m_gridTable = new AFDataMatrixTable(&m_data, choice_col, collabels, rowlabels);

	m_grid->SetTable(m_gridTable);


	m_caption = new wxStaticText(this, wxID_ANY, "");
	m_caption->SetFont(*wxNORMAL_FONT);

	if (sidebuttons)
	{
		// for side buttons layout
		wxBoxSizer *v_tb_sizer = new wxBoxSizer(wxVERTICAL);
		v_tb_sizer->Add(m_caption, 0, wxALL | wxEXPAND, 3);
		v_tb_sizer->Add(m_btnImport, 0, wxALL | wxEXPAND, 2);
		v_tb_sizer->Add(m_btnExport, 0, wxALL | wxEXPAND, 2);
		v_tb_sizer->Add(m_btnCopy, 0, wxALL | wxEXPAND, 2);
		v_tb_sizer->Add(m_btnPaste, 0, wxALL | wxEXPAND, 2);
		v_tb_sizer->Add(m_labelRows, 0, wxALL, 2);
		v_tb_sizer->Add(m_numRows, 0, wxALL | wxEXPAND, 2);
		v_tb_sizer->Add(m_labelCols, 0, wxALL , 2);
		v_tb_sizer->Add(m_numCols, 0, wxALL | wxEXPAND, 2);
		v_tb_sizer->AddStretchSpacer();

		wxBoxSizer *h_sizer = new wxBoxSizer(wxHORIZONTAL);
		h_sizer->Add(v_tb_sizer, 0, wxALL | wxEXPAND, 1);
		if (!verticalLabel.IsEmpty())
			h_sizer->Add(m_verticalLabel, 0, wxALL | wxALIGN_CENTER_VERTICAL, 2);
		if (!horizontalLabel.IsEmpty())
		{
			wxBoxSizer *v_lb_sizer = new wxBoxSizer(wxVERTICAL);
			v_lb_sizer->Add(m_horizontalLabel, 0, wxALL | wxALIGN_CENTER_HORIZONTAL, 2);
			v_lb_sizer->Add(m_grid, 1, wxALL | wxEXPAND, 1);
			h_sizer->Add(v_lb_sizer, 1, wxALL | wxEXPAND, 1);
		}
		else
			h_sizer->Add(m_grid, 1, wxALL | wxEXPAND, 1);

		SetSizer(h_sizer);
	}
	else
	{
		// for top buttons layout (default)
		wxBoxSizer *h_tb_sizer = new wxBoxSizer(wxHORIZONTAL);
		h_tb_sizer->Add(m_caption, 0, wxALL | wxEXPAND | wxALIGN_CENTER_VERTICAL, 3);
		h_tb_sizer->AddStretchSpacer();
		if (!bottombuttons)
		{
			h_tb_sizer->Add(m_btnImport, 0, wxALL | wxEXPAND, 2);
			h_tb_sizer->Add(m_btnExport, 0, wxALL | wxEXPAND, 2);
			h_tb_sizer->Add(new wxStaticLine(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxVERTICAL), 0, wxALL | wxEXPAND, 1);
			h_tb_sizer->Add(m_btnCopy, 0, wxALL | wxEXPAND, 2);
			h_tb_sizer->Add(m_btnPaste, 0, wxALL | wxEXPAND, 2);
			h_tb_sizer->Add(new wxStaticLine(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxVERTICAL), 0, wxALL | wxEXPAND, 1);
		}
		h_tb_sizer->Add(m_labelRows, 0, wxALL | wxALIGN_CENTER_VERTICAL, 2);
		h_tb_sizer->Add(m_numRows, 0, wxALL | wxEXPAND, 2);
		h_tb_sizer->Add(m_labelCols, 0, wxALL | wxALIGN_CENTER_VERTICAL, 2);
		h_tb_sizer->Add(m_numCols, 0, wxALL | wxEXPAND, 2);

		wxBoxSizer *v_sizer = new wxBoxSizer(wxVERTICAL);
		v_sizer->Add(h_tb_sizer, 0, wxALL | wxEXPAND, 1);
		if (!horizontalLabel.IsEmpty())
			v_sizer->Add(m_horizontalLabel, 0, wxALL | wxALIGN_CENTER_HORIZONTAL, 2);
		if (!verticalLabel.IsEmpty())
		{
			wxBoxSizer *h_lb_sizer = new wxBoxSizer(wxHORIZONTAL);
			h_lb_sizer->Add(m_verticalLabel, 0, wxALL | wxALIGN_CENTER_VERTICAL, 2);
			h_lb_sizer->Add(m_grid, 1, wxALL | wxEXPAND, 1);
			v_sizer->Add(h_lb_sizer, 1, wxALL | wxEXPAND, 1);
		}
		else
			v_sizer->Add(m_grid, 1, wxALL | wxEXPAND, 1);

		wxBoxSizer *h_bb_sizer = new wxBoxSizer(wxHORIZONTAL);
		if (bottombuttons)
		{
			h_bb_sizer->Add(m_btnImport, 0, wxALL | wxEXPAND, 2);
			h_bb_sizer->Add(m_btnExport, 0, wxALL | wxEXPAND, 2);
			h_bb_sizer->Add(new wxStaticLine(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxVERTICAL), 0, wxALL | wxEXPAND, 1);
			h_bb_sizer->Add(m_btnCopy, 0, wxALL | wxEXPAND, 2);
			h_bb_sizer->Add(m_btnPaste, 0, wxALL | wxEXPAND, 2);
			v_sizer->Add(h_bb_sizer, 0, wxALL | wxEXPAND, 1);
		}

		SetSizer(v_sizer, false);
	}


	if (m_caption->GetLabel().Length() == 0)
		m_caption->Show(false);
	else
		m_caption->Show(true);
	if (m_horizontalLabel->GetLabel().Length() == 0)
		m_horizontalLabel->Show(false);
	else
		m_horizontalLabel->Show(true);
	if (m_verticalLabel->GetLabel().Length() == 0)
		m_verticalLabel->Show(false);
	else
		m_verticalLabel->Show(true);

	MatrixToGrid();
}


void AFDataMatrixCtrl::SetColLabels(const wxString &colLabels)
{
	m_colLabels = colLabels;
	m_colLabels.Replace("\\n", "\n");
	MatrixToGrid();
}

wxString AFDataMatrixCtrl::GetColLabels()
{
	return m_colLabels;
}

void AFDataMatrixCtrl::SetRowLabels(const wxString &rowLabels)
{
	m_rowLabels = rowLabels;
	m_rowLabels.Replace("\\n", "\n");
	MatrixToGrid();
}

wxString AFDataMatrixCtrl::GetRowLabels()
{
	return m_rowLabels;
}


void AFDataMatrixCtrl::SetNumRowsLabel(const wxString &numRowsLabel)
{
	m_numRowsLabel = numRowsLabel;
	MatrixToGrid();
}

wxString AFDataMatrixCtrl::GetNumRowsLabel()
{
	return m_numRowsLabel;
}

void AFDataMatrixCtrl::SetNumColsLabel(const wxString &numColsLabel)
{
	m_numColsLabel = numColsLabel;
	MatrixToGrid();
}

wxString AFDataMatrixCtrl::GetNumColsLabel()
{
	return m_numColsLabel;
}

bool AFDataMatrixCtrl::PasteAppendRows()
{
	return m_pasteappendrows;
}
void AFDataMatrixCtrl::PasteAppendRows(bool b)
{
	m_pasteappendrows = b;
}

bool AFDataMatrixCtrl::PasteAppendCols()
{
	return m_pasteappendcols;
}
void AFDataMatrixCtrl::PasteAppendCols(bool b)
{
	m_pasteappendcols = b;
}


void AFDataMatrixCtrl::ShowColLabels(bool b)
{
	m_showColLabels = b;
	MatrixToGrid();
}

bool AFDataMatrixCtrl::ShowColLabels()
{
	return m_showColLabels;
}

void AFDataMatrixCtrl::ShowRowLabels(bool b)
{
	m_showRowLabels = b;
	MatrixToGrid();
}

bool AFDataMatrixCtrl::ShowRowLabels()
{
	return m_showRowLabels;
}

void AFDataMatrixCtrl::ShowCols(bool b)
{
	m_showcols = b;
	m_numCols->Show(m_showcols);
	m_labelCols->Show(m_showcols);
	this->Layout();
}

bool AFDataMatrixCtrl::ShowCols()
{
	return m_showcols;
}

void AFDataMatrixCtrl::ShowButtons(bool b)
{
	m_showButtons = b;
	m_btnCopy->Show(m_showButtons);
	m_btnPaste->Show(m_showButtons);
	m_btnImport->Show(m_showButtons);
	m_btnExport->Show(m_showButtons);
	this->Layout();
}

bool AFDataMatrixCtrl::ShowButtons()
{
	return m_showButtons;
}

void AFDataMatrixCtrl::SetR0C0Label(const wxString &R0C0Label)
{
	m_grid->SetRowLabelValue(-1, R0C0Label);
}

wxString AFDataMatrixCtrl::GetR0C0Label()
{
	return m_grid->GetCellValue(0, 0);
}

void AFDataMatrixCtrl::ShowRows(bool b)
{
	m_showrows = b;
	m_numRows->Show(m_showrows);
	m_labelRows->Show(m_showrows);
	this->Layout();
}

bool AFDataMatrixCtrl::ShowRows()
{
	return m_showrows;
}

void AFDataMatrixCtrl::ShowRow(const int &row, bool show)
{
	if (row > -1 && row < m_grid->GetNumberRows())
	{
		if (show)
			m_grid->ShowRow(row);
		else
			m_grid->HideRow(row);
		this->Layout();
	}
}

void AFDataMatrixCtrl::ShowCol(const int &col, bool show)
{
	if (col > -1 && col < m_grid->GetNumberCols())
	{
		if (show)
			m_grid->ShowCol(col);
		else
			m_grid->HideCol(col);
		this->Layout();
	}
}

void AFDataMatrixCtrl::SetColReadOnly(const int &col, bool readonly)
{
	if (col > -1 && col < m_grid->GetNumberCols())
	{
		for (int i = 0; i < m_grid->GetNumberRows(); i++)
			m_grid->SetReadOnly(i, col, readonly);
		this->Layout();
	}
}

void AFDataMatrixCtrl::SetRowReadOnly(const int &row, bool readonly)
{
	if (row > -1 && row < m_grid->GetNumberRows())
	{
		for (int i = 0; i < m_grid->GetNumberCols(); i++)
			m_grid->SetReadOnly(row, i, readonly);
		this->Layout();
	}
}

void AFDataMatrixCtrl::ShadeR0C0(bool b)
{
	m_shadeR0C0 = b;
	m_grid->GetTable()->SetAttrProvider(new wxExtGridCellAttrProvider(b, m_shadeR0C0, b || m_shadeC0, !m_colorMap));
	MatrixToGrid();
}

bool AFDataMatrixCtrl::ShadeR0C0()
{
	return m_shadeR0C0;
}


void AFDataMatrixCtrl::ColorMap(bool b)
{
	m_colorMap = b;
	m_grid->GetTable()->SetAttrProvider(new wxExtGridCellAttrProvider(m_shadeR0C0, m_shadeR0C0, m_shadeC0 || m_shadeR0C0, !m_colorMap));
	MatrixToGrid();
}

bool AFDataMatrixCtrl::ColorMap()
{
	return m_colorMap;
}


void AFDataMatrixCtrl::ShadeC0(bool b)
{
	m_shadeC0 = b;
	m_grid->GetTable()->SetAttrProvider(new wxExtGridCellAttrProvider(m_shadeR0C0, m_shadeR0C0, b || m_shadeR0C0, !m_colorMap));
	MatrixToGrid();
}

bool AFDataMatrixCtrl::ShadeC0()
{
	return m_shadeC0;
}


void AFDataMatrixCtrl::SetData(const matrix_t<double> &mat)
{
	m_data = mat;
	NormalizeToLimits();
	m_gridTable->SetMatrix(&m_data);
	MatrixToGrid();
}


void AFDataMatrixCtrl::NormalizeToLimits()
{
	if (m_minVal != m_maxVal)
	{
		for (size_t r = 0; r<m_data.nrows(); r++)
		{
			for (size_t c = 0; c<m_data.ncols(); c++)
			{
				if (m_data.at(r, c) < m_minVal) m_data.at(r, c) = m_minVal;
				if (m_data.at(r, c) > m_maxVal) m_data.at(r, c) = m_maxVal;
			}
		}
	}
}

void AFDataMatrixCtrl::GetData(matrix_t<double> &mat)
{
	mat = m_data;
}


void AFDataMatrixCtrl::SetValueLimits(float min, float max)
{
	m_minVal = min;
	m_maxVal = max;
}

void AFDataMatrixCtrl::GetValueLimits(float *min, float *max)
{
	if (min) *min = m_minVal;
	if (max) *max = m_maxVal;
}

bool AFDataMatrixCtrl::Export(const wxString &file)
{
	wxCSVData csv;
	for (size_t r = 0; r<m_data.nrows(); r++)
		for (size_t c = 0; c<m_data.ncols(); c++)
			csv(r, c) = wxString::Format("%g", m_data(r, c));

	return csv.WriteFile(file);
}

bool AFDataMatrixCtrl::Import(const wxString &file)
{
	wxCSVData csv;
	if (!csv.ReadFile(file)) return false;

	m_data.resize_fill(csv.NumRows(), csv.NumCols(), 0.0f);

	for (size_t r = 0; r<m_data.nrows(); r++)
		for (size_t c = 0; c<m_data.ncols(); c++)
			m_data.at(r, c) = (float)wxAtof(csv(r, c));

	NormalizeToLimits();
	MatrixToGrid();

	wxCommandEvent evt(wxEVT_AFDataMatrixCtrl_CHANGE, this->GetId());
	evt.SetEventObject(this);
	GetEventHandler()->ProcessEvent(evt);

	return true;
}

void AFDataMatrixCtrl::OnCellChange(wxGridEvent &evt)
{
	int irow = evt.GetRow();
	int icol = evt.GetCol();

	float val = (float)wxAtof(m_grid->GetCellValue(irow, icol).c_str());

	if (m_minVal != m_maxVal)
	{
		if (val < m_minVal) val = m_minVal;
		if (val > m_maxVal) val = m_maxVal;
	}

	if (irow < (int)m_data.nrows() && icol < (int)m_data.ncols()
		&& irow >= 0 && icol >= 0)
		m_data.at(irow, icol) = val;

	m_gridTable->SetMatrix(&m_data);
	m_grid->SetCellValue(irow, icol, wxString::Format("%g", val));

	UpdateColorMap();

	wxCommandEvent dmcevt(wxEVT_AFDataMatrixCtrl_CHANGE, this->GetId());
	dmcevt.SetEventObject(this);
	GetEventHandler()->ProcessEvent(dmcevt);
}


void AFDataMatrixCtrl::UpdateColorMap()
{

	if (m_colorMap)
	{
		size_t nr = m_data.nrows();
		size_t nc = m_data.ncols();
		size_t r, c;
		double zmin = 1e38, zmax = -1e38;
		for (r = 1; r < nr; r++)
		{
			for (c = 1; c < nc; c++)
			{
				if (m_data(r, c) < zmin) zmin = m_data.at(r, c);
				if (m_data(r, c) > zmax) zmax = m_data.at(r, c);
			}
		}
		double diff = zmax - zmin;
		wxPLJetColourMap jet = wxPLJetColourMap(zmin - 0.35*diff, zmax+0.1*diff);
//		wxPLCoarseRainbowColourMap jet = wxPLCoarseRainbowColourMap(zmin - 0.5*(zmax - zmin), zmax);
		for (r = 1; r < nr; r++)
		{
			for (c = 1; c < nc; c++)
			{
				wxColour clr = jet.ColourForValue(m_data.at(r, c));
				m_grid->SetCellBackgroundColour(r, c, clr);
				//				m_grid->SetCellTextColour(r, c, *wxLIGHT_GREY);
			}
		}
		m_grid->Refresh();
	}

}

void AFDataMatrixCtrl::OnRowsColsChange(wxCommandEvent &)
{
	size_t rows = (size_t)m_numRows->AsInteger();
	size_t cols = (size_t)m_numCols->AsInteger();

	if (rows < 1) rows = 1;
	if (cols < 1) cols = 1;

	m_data.resize_preserve(rows, cols, 0.0f);

	MatrixToGrid();

	wxCommandEvent dmcevt(wxEVT_AFDataMatrixCtrl_CHANGE, this->GetId());
	dmcevt.SetEventObject(this);
	GetEventHandler()->ProcessEvent(dmcevt);
}

void AFDataMatrixCtrl::OnCommand(wxCommandEvent &evt)
{
	switch (evt.GetId())
	{
	case IDEDMC_COPY:
		m_grid->Copy(true);
		break;
	case IDEDMC_PASTE:
	{
		if (m_pasteappendrows || m_pasteappendcols)
		{
			if (wxTheClipboard->Open())
			{
				wxString data;
				wxTextDataObject textobj;
				if (wxTheClipboard->GetData(textobj))
				{
					data = textobj.GetText();
					wxTheClipboard->Close();
				}
				if (data.IsEmpty()) return;

#ifdef __WXMAC__
				wxArrayString lines = wxStringTokenize(data, "\r", ::wxTOKEN_RET_EMPTY_ALL);
#else
				wxArrayString lines = wxStringTokenize(data, "\n", ::wxTOKEN_RET_EMPTY_ALL);
#endif
				int ncols = m_grid->GetNumberCols();
				int nrows = m_grid->GetNumberRows();
				if (m_pasteappendrows && lines.Count()>1)
					nrows = lines.Count() - 1;
				if (m_pasteappendcols && lines.Count() > 1)
				{
					wxArrayString col_vals = wxStringTokenize(lines[0], "\t,", ::wxTOKEN_RET_EMPTY_ALL);
					ncols = col_vals.Count();
				}
				m_grid->ResizeGrid(nrows, ncols);
				m_data.resize_preserve(nrows, ncols, 0.0);
			}
		}

		m_grid->Paste( wxExtGridCtrl::PASTE_ALL );

		for (size_t r = 0; r<m_data.nrows(); r++)
			for (size_t c = 0; c<m_data.ncols(); c++)
				m_data.at(r, c) = atof(m_grid->GetCellValue(r, c).c_str());

		MatrixToGrid();

		wxCommandEvent dmcevt(wxEVT_AFDataMatrixCtrl_CHANGE, this->GetId());
		dmcevt.SetEventObject(this);
		GetEventHandler()->ProcessEvent(dmcevt);
	}
	break;
	case IDEDMC_IMPORT:
	{
		wxFileDialog dlg(this, "Select data matrix file to import");
		if (dlg.ShowModal() == wxID_OK)
			if (!Import(dlg.GetPath()))
				wxMessageBox("Error import data file:\n\n" + dlg.GetPath());
	}
	break;
	case IDEDMC_EXPORT:
	{
		wxFileDialog dlg(this, "Select file for data export", wxEmptyString, wxEmptyString, wxFileSelectorDefaultWildcardStr, wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
		if (dlg.ShowModal() == wxID_OK)
			if (!Export(dlg.GetPath()))
				wxMessageBox("Error exporting data to file:\n\n" + dlg.GetPath());
	}
	break;
	}
}


void AFDataMatrixCtrl::MatrixToGrid()
{
	m_data = m_gridTable->GetMatrix();
	int r, nr = m_data.nrows();
	int c, nc = m_data.ncols();

	m_grid->Freeze();
	m_grid->SetTable(m_gridTable);

	m_numRows->SetValue(nr);
	m_numCols->SetValue(nc);

	
	//m_grid->ResizeGrid(nr, nc);
	/*
	for (r = 0; r<nr; r++)
		for (c = 0; c<nc; c++)
			m_grid->SetCellValue(r, c, wxString::Format("%g", m_data.at(r, c)));
	*/
	if (!m_rowFormat.IsEmpty())
	{
		for (r = 0; r<nr; r++)
		{
			wxString label = m_rowFormat;
			label.Replace("#", wxString::Format("%lg", m_rowY2*((double)r) / ((double)nr) + r*m_rowY1 + m_rowY0));
			m_grid->SetRowLabelValue(r, label);
		}
	}

	if (!m_colFormat.IsEmpty())
	{
		for (c = 0; c<nc; c++)
		{
			wxString label = m_colFormat;
			label.Replace("#", wxString::Format("%lg", m_colY2*((double)c) / ((double)nc) + c*m_colY1 + m_colY0));
			m_grid->SetColLabelValue(c, label);
		}
	}

	if (m_showRowLabels)
	{
		wxArrayString as = wxStringTokenize(m_rowLabels, ",");
		for (r = 0; r<(int)as.Count() && r < m_grid->GetNumberRows(); r++)
		{
			m_grid->SetRowLabelValue(r, as[r]);
		}
		m_grid->SetRowLabelSize(wxGRID_AUTOSIZE);
	}
	else
	{
		m_grid->SetRowLabelSize(1);
	}

	if (m_showColLabels)
	{
		wxArrayString as = wxStringTokenize(m_colLabels, ",");
		for (c = 0; c<(int)as.Count() && c < m_grid->GetNumberCols(); c++)
		{
			m_grid->SetColLabelValue(c, as[c]);
			m_grid->AutoSizeColLabelSize(c);
		}
		m_grid->SetColLabelSize(wxGRID_AUTOSIZE);
	}
	else
	{
		m_grid->SetColLabelSize(1);
	}

	m_labelRows->SetLabel(m_numRowsLabel);
	m_labelCols->SetLabel(m_numColsLabel);

	UpdateColorMap();


	Layout();
	m_grid->Thaw();
	m_grid->Refresh();

}



void AFDataMatrixCtrl::SetRowLabelFormat(const wxString &val_fmt, double y2, double y1, double y0)
{
	m_rowFormat = val_fmt;
	m_rowY2 = y2;
	m_rowY1 = y1;
	m_rowY0 = y0;
	MatrixToGrid();
}

void AFDataMatrixCtrl::SetColLabelFormat(const wxString &val_fmt, double y2, double y1, double y0)
{
	m_colFormat = val_fmt;
	m_colY2 = y2;
	m_colY1 = y1;
	m_colY0 = y0;
	MatrixToGrid();
}

void AFDataMatrixCtrl::SetCaption(const wxString &cap)
{
	m_caption->SetLabel(cap);
	this->Layout();
}

wxString AFDataMatrixCtrl::GetCaption()
{
	return m_caption->GetLabel();
}





/* data grid dialog */

class DataGridDialog : public wxDialog
{
	wxExtGridCtrl *Grid;
public:

	DataGridDialog(wxWindow *parent, const wxString &title)
		: wxDialog(parent, -1, title, wxDefaultPosition, wxScaleSize(330,410), 
			wxCAPTION|wxRESIZE_BORDER|wxMAXIMIZE_BOX|wxCLOSE_BOX|wxSYSTEM_MENU )
	{
		wxButton *btn = NULL;
		Grid = new wxExtGridCtrl(this, IDDD_GRID);
		Grid->CreateGrid(10,2);
		Grid->DisableDragCell();
		//Grid->DisableDragColSize();
		Grid->DisableDragRowSize();
		Grid->DisableDragColMove();
		Grid->DisableDragGridSize();

		wxBoxSizer *szh_top = new wxBoxSizer(wxHORIZONTAL);
		btn = new wxButton(this, IDDD_CHANGENUMROWS, "Rows...");
		szh_top->Add(btn, 0, wxALL|wxEXPAND, 1);
		btn = new wxButton(this, IDDD_COPY, "Copy");
		szh_top->Add(btn, 0, wxALL|wxEXPAND, 1);
		btn = new wxButton(this, IDDD_PASTE, "Paste");
		szh_top->Add(btn, 0, wxALL|wxEXPAND, 1);
		szh_top->AddStretchSpacer();
	
		wxBoxSizer *szv_main = new wxBoxSizer(wxVERTICAL);
		szv_main->Add(szh_top, 0, wxALL|wxEXPAND, 1);
		szv_main->Add(Grid, 1, wxALL|wxEXPAND, 1);
		szv_main->Add(CreateButtonSizer( wxOK|wxCANCEL), 0, wxALL|wxEXPAND, 5);

		SetSizer(szv_main);
	}

	void SetData(const matrix_t<double> &data, wxArrayString *collabels)
	{
		if (data.nrows() < 1 || data.ncols() < 1)
			return;

		Grid->ResizeGrid( data.nrows(), data.ncols() );

		for (size_t r=0;r<data.nrows();r++)
			for (size_t c=0;c<data.ncols();c++)
				Grid->SetCellValue( r, c, wxString::Format("%g", data.at(r,c)) );

		if (collabels != NULL)
		{
			for (size_t i=0;i<data.ncols() && i < collabels->Count();i++)
				Grid->SetColLabelValue( i, collabels->Item(i) );
		}

		Grid->SetRowLabelSize(wxGRID_AUTOSIZE);
		Grid->SetRowLabelAlignment(wxALIGN_LEFT,wxALIGN_CENTRE);
		//Grid->SetColLabelSize(wxGRID_AUTOSIZE);
		Grid->SetColLabelAlignment(wxALIGN_LEFT,wxALIGN_CENTRE);
		//Grid->AutoSize();

		Grid->Layout();
		Grid->GetParent()->Layout();
		Grid->Refresh();

	}

	void GetData(matrix_t<double> &data)
	{
		int nr = Grid->GetNumberRows();
		int nc = Grid->GetNumberCols();
		data.resize_fill( nr, nc, 0.0f );
		for (int r=0;r<nr;r++)
			for (int c=0;c<nc;c++)
				data.at(r,c) = (double)wxAtof( Grid->GetCellValue(r,c) );
	}
	
	void OnCellChange(wxGridEvent &evt)
	{
		int r = evt.GetRow();
		int c = evt.GetCol();

		if (r >= 0 && c >= 0)
		{
			float val = (float)wxAtof( Grid->GetCellValue(r,c) );
			Grid->SetCellValue( r, c, wxString::Format("%g", val) );
		}
	}

	void OnCommand(wxCommandEvent &evt)
	{
		if (evt.GetId() == IDDD_CHANGENUMROWS)
		{
			wxString result = wxGetTextFromUser("Enter number of data rows", "Edit Table",
				wxString::Format("%d", Grid->GetNumberRows()), this );
			if (result.IsEmpty()) return;

			long l=0;
			if ( result.ToLong(&l)  && l > 0 )
			{
				Grid->ResizeGrid( l, Grid->GetNumberCols() );
			}
			else
				wxMessageBox("Invalid number of rows or non-numeric entry.");
		}
		else if (evt.GetId() == IDDD_COPY)
			Grid->Copy(true);
		else if (evt.GetId() == IDDD_PASTE)
			Grid->Paste( wxExtGridCtrl::PASTE_ALL );
	}

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE(DataGridDialog, wxDialog)
	EVT_BUTTON( IDDD_COPY, DataGridDialog::OnCommand )
	EVT_BUTTON( IDDD_PASTE, DataGridDialog::OnCommand )
	EVT_BUTTON( IDDD_CHANGENUMROWS, DataGridDialog::OnCommand )
	EVT_GRID_CMD_CELL_CHANGED( IDDD_GRID, DataGridDialog::OnCellChange )
END_EVENT_TABLE()




enum {IDDGB_NUMERIC=wxID_HIGHEST+239, IDDGB_BUTTON};

DEFINE_EVENT_TYPE( wxEVT_VALUEMATRIXBUTTON_CHANGE )


BEGIN_EVENT_TABLE(AFValueMatrixButton, wxWindow)
	EVT_PAINT( AFValueMatrixButton::OnPaint )
	EVT_SIZE( AFValueMatrixButton::OnResize )
	EVT_LEFT_DOWN( AFValueMatrixButton::OnClick )
	EVT_BUTTON( IDDGB_BUTTON, AFValueMatrixButton::OnEditTable)
	EVT_NUMERIC( IDDGB_NUMERIC, AFValueMatrixButton::OnValChanged)
END_EVENT_TABLE()

AFValueMatrixButton::AFValueMatrixButton(wxWindow *parent, int id, const wxPoint &pos, const wxSize &sz)
	: wxWindow(parent, id, pos, sz, wxCLIP_CHILDREN)
{
	m_switchWidth = 26;
	UpdateSwitchWidth();
	SetBackgroundStyle(wxBG_STYLE_CUSTOM);

	bUseTable = false;

	mSingleValue = new wxNumericCtrl(this, IDDGB_NUMERIC, 0.0, wxNUMERIC_REAL, wxPoint(m_switchWidth,0), wxSize( sz.GetWidth()-m_switchWidth, sz.GetHeight()) );
	mBtnEditTable = new wxButton(this, IDDGB_BUTTON, "Table...", wxPoint(m_switchWidth,0), wxSize( sz.GetWidth()-m_switchWidth, sz.GetHeight()) );
	mBtnEditTable->Show(false);

	mTable.resize_fill(10,2, 0.0f);
}

void AFValueMatrixButton::UpdateSwitchWidth()
{
	wxClientDC pdc(this);
	pdc.SetFont( SCHEDNUM_FONT );
	wxSize sz1( pdc.GetTextExtent( "Sched" ) );
	wxSize sz2( pdc.GetTextExtent( "Table" ) );
	m_switchWidth = 2 + ( sz1.x > sz2.x ? sz1.x : sz2.x );
}

bool AFValueMatrixButton::UseTable()
{
	return bUseTable;
}

void AFValueMatrixButton::UseTable(bool b)
{
	mSingleValue->Show( !b );
	mBtnEditTable->Show( b );
	bUseTable = b;
	Refresh();
}

float AFValueMatrixButton::GetSingleValue()
{
	return (float)mSingleValue->Value();
}

void AFValueMatrixButton::SetSingleValue(double val)
{
	mSingleValue->SetValue((double)val);
}

void AFValueMatrixButton::GetTableData(matrix_t<double> *mat)
{
	if (mat) *mat = mTable;
}

void AFValueMatrixButton::SetTableData(const matrix_t<double> &mat)
{
	mTable = mat;
}

void AFValueMatrixButton::SetTableSize(int nr, int nc)
{
	mTable.resize_preserve(nr,nc, 0.0f);
}

void AFValueMatrixButton::GetTableSize(int *nr, int *nc)
{
	if (nr) *nr = mTable.nrows();
	if (nc) *nc = mTable.ncols();
}


void AFValueMatrixButton::SetColLabels(const wxString &delimlist)
{
	SetColLabels(wxStringTokenize(delimlist, ",")); 
}

void AFValueMatrixButton::SetColLabels(const wxArrayString &labels)
{
	mColLabels = labels;
	for( size_t i=0;i<mColLabels.size();i++ )
		mColLabels[i].Replace("\\n", "\n");
}

void AFValueMatrixButton::OnResize(wxSizeEvent &)
{
	int cw, ch;
	GetClientSize(&cw,&ch);
	mSingleValue->SetSize(m_switchWidth,0,cw-m_switchWidth,ch);
	mBtnEditTable->SetSize(m_switchWidth,0,cw-m_switchWidth,ch);
}

void AFValueMatrixButton::OnPaint(wxPaintEvent &)
{
	wxAutoBufferedPaintDC pdc(this);

	int cwidth, cheight;
	GetClientSize(&cwidth,&cheight);
	
	pdc.SetBrush(wxBrush(*wxLIGHT_GREY));
	pdc.SetPen(wxPen(*wxLIGHT_GREY));
	pdc.DrawRectangle(0, 0, m_switchWidth, cheight);

	pdc.SetBrush(wxBrush(wxTheColourDatabase->Find("FOREST GREEN")));
	pdc.SetPen(wxPen(wxTheColourDatabase->Find("FOREST GREEN")));
	pdc.DrawRectangle(0, bUseTable?cheight/2:0, m_switchWidth, cheight/2);

	pdc.SetFont( SCHEDNUM_FONT );
	pdc.SetTextForeground(*wxWHITE);
	pdc.DrawText(wxT("Value"), 1, 1);
	pdc.DrawText(wxT("Table"), 1, cheight/2+1); 
}

void AFValueMatrixButton::OnClick(wxMouseEvent &evt)
{
	SetFocus();
	
	if (evt.GetX() >= m_switchWidth)
		return;

	UseTable( !bUseTable );
	DispatchEvent();
}

void AFValueMatrixButton::OnEditTable(wxCommandEvent &)
{
	DataGridDialog dlg(this, "Edit Tabular Data");
	dlg.SetData( mTable, &mColLabels );
	if (dlg.ShowModal() == wxID_OK)
	{
		dlg.GetData( mTable );
		DispatchEvent();
	}
}

void AFValueMatrixButton::OnValChanged(wxCommandEvent &)
{
	DispatchEvent();
}

void AFValueMatrixButton::DispatchEvent()
{
	wxCommandEvent evt(wxEVT_VALUEMATRIXBUTTON_CHANGE, this->GetId() );
	evt.SetEventObject(this);
	GetEventHandler()->ProcessEvent(evt);
}

void AFValueMatrixButton::Set( const matrix_t<double> &mat )
{
	if ( mat.nrows() == 1 && mat.ncols() == 1 )
	{
		UseTable( false );
		mSingleValue->SetValue( mat(0,0) );
	}
	else
	{
		UseTable( true );
		mTable = mat;
	}
}

matrix_t<double> AFValueMatrixButton::Get()
{
	if ( bUseTable ) return mTable;
	else 
	{
		matrix_t<double> mat;
		mat.resize_fill( 1, 1, (double)mSingleValue->Value() );
		return mat;
	}
}




enum { IDSF_GRID = wxID_HIGHEST+495, IDSF_SHADINGVAL, IDSF_APPLY, IDSF_IMPORT, IDSF_EXPORT, IDSF_COPY, IDSF_PASTE };

BEGIN_EVENT_TABLE(AFMonthByHourFactorCtrl, wxPanel)

	EVT_GRID_CMD_CELL_CHANGED( IDSF_GRID, AFMonthByHourFactorCtrl::OnGridCellChange)
	EVT_GRID_CMD_SELECT_CELL( IDSF_GRID, AFMonthByHourFactorCtrl::OnGridCellSelect)
	EVT_GRID_CMD_RANGE_SELECT( IDSF_GRID, AFMonthByHourFactorCtrl::OnGridRangeSelect)
	EVT_GRID_CMD_EDITOR_HIDDEN( IDSF_GRID, AFMonthByHourFactorCtrl::OnGridEditorHidden)
	EVT_GRID_CMD_EDITOR_SHOWN( IDSF_GRID, AFMonthByHourFactorCtrl::OnGridEditorShown)

	EVT_BUTTON( IDSF_IMPORT, AFMonthByHourFactorCtrl::OnImport)
	EVT_BUTTON( IDSF_EXPORT, AFMonthByHourFactorCtrl::OnExport)
	EVT_BUTTON(IDSF_COPY, AFMonthByHourFactorCtrl::OnCopy)
	EVT_BUTTON(IDSF_PASTE, AFMonthByHourFactorCtrl::OnPaste)

	EVT_BUTTON( IDSF_APPLY, AFMonthByHourFactorCtrl::OnApply )
	EVT_NUMERIC( IDSF_SHADINGVAL, AFMonthByHourFactorCtrl::OnApply )

END_EVENT_TABLE()

DEFINE_EVENT_TYPE( wxEVT_AFMonthByHourFactorCtrl_CHANGE )

#define SFROWS 12
#define SFCOLS 24

static const char *row_labels[SFROWS] = 
{ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

static const char *col_labels[SFCOLS] =
{ "12am", "1am", "2am", "3am", "4am", "5am", "6am", "7am", "8am", "9am", "10am", "11am", 
  "12pm", "1pm", "2pm", "3pm", "4pm", "5pm", "6pm", "7pm", "8pm", "9pm", "10pm", "11pm" };

AFMonthByHourFactorCtrl::AFMonthByHourFactorCtrl(wxWindow *parent, int id, const wxPoint &pos, const wxSize &sz)
	: wxPanel(parent, id, pos, sz, wxCLIP_CHILDREN|wxTAB_TRAVERSAL|wxBORDER_NONE)
{
	Colour1 = *wxWHITE;
	Colour2 = *wxRED;

	bSkipSelect = false;

	mSelTopRow = mSelBottomRow = -1;
	mSelLeftCol = mSelRightCol = -1;

	mShadingVal = new wxNumericCtrl(this, IDSF_SHADINGVAL);
	mData.resize_fill(SFROWS, SFCOLS, 0.0f);

	int r,c;

	for (r=0;r<SFROWS;r++)
		for (c=0;c<SFCOLS;c++)
			mData.at(r,c) = 0.0;

	m_title = new wxStaticText(this, -1, "Beam Shading Loss");
	wxFont fbold( *wxNORMAL_FONT );
	fbold.SetWeight( wxFONTWEIGHT_BOLD );
	m_title->SetFont( fbold );

	m_legend = new wxStaticText(this, -1, "0%=No Shading, 100%=Fully Shaded");
	mBtnApply = new wxButton(this, IDSF_APPLY, "Apply to selected cells");

	mGrid = new wxExtGridCtrl(this, IDSF_GRID);
	mGrid->EnableCopyPaste( true );
	mGrid->CreateGrid(SFROWS, SFCOLS);
	mGrid->EnableEditing(true);
	mGrid->DisableDragCell();
	mGrid->DisableDragColSize();
	mGrid->DisableDragRowSize();
	mGrid->DisableDragColMove();
	mGrid->DisableDragGridSize();


	for (r=0;r<SFROWS;r++)
		mGrid->SetRowLabelValue(r, row_labels[r]);

	for (c=0;c<SFCOLS;c++)
		mGrid->SetColLabelValue(c, col_labels[c]);

	mGrid->SetRowLabelSize(wxGRID_AUTOSIZE);
	mGrid->SetRowLabelAlignment(wxALIGN_LEFT,wxALIGN_CENTRE);
	mGrid->SetColLabelSize(wxGRID_AUTOSIZE);
	mGrid->SetColLabelAlignment(wxALIGN_LEFT,wxALIGN_CENTRE);

	wxBoxSizer *topctrls = new wxBoxSizer(wxHORIZONTAL);
	topctrls->Add(m_title, 1, wxALL|wxEXPAND, 1);
	topctrls->Add(m_legend, 1, wxALL|wxEXPAND, 1);
	topctrls->Add(mShadingVal, 0, wxALL|wxEXPAND, 1);
	topctrls->Add(mBtnApply, 0, wxALL|wxEXPAND, 1);
	
	wxBoxSizer *bottomctrls = new wxBoxSizer(wxHORIZONTAL);
	bottomctrls->Add(new wxButton(this, IDSF_IMPORT, "Import..."), 0, wxALL|wxEXPAND, 1);
	bottomctrls->Add(new wxButton(this, IDSF_EXPORT, "Export..."), 0, wxALL|wxEXPAND, 1);
	bottomctrls->Add(new wxStaticLine(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxVERTICAL), 0, wxALL | wxEXPAND, 1);
	bottomctrls->Add(new wxButton(this, IDSF_COPY, "Copy"), 0, wxALL | wxEXPAND, 1);
	bottomctrls->Add(new wxButton(this, IDSF_PASTE, "Paste"), 0, wxALL | wxEXPAND, 1);
	bottomctrls->AddStretchSpacer();
	
	wxBoxSizer *mainsz = new wxBoxSizer(wxVERTICAL);
	mainsz->Add( topctrls, 0, wxALL|wxEXPAND, 1);
	mainsz->Add( mGrid, 1, wxALL|wxEXPAND, 1);
	mainsz->Add( bottomctrls, 0, wxALL|wxEXPAND, 1);

	SetSizer(mainsz);

	UpdateGrid();
}

AFMonthByHourFactorCtrl::~AFMonthByHourFactorCtrl()
{
	/* nothing to do */
}

void AFMonthByHourFactorCtrl::SetTitle( const wxString &title )
{
	m_title->SetLabel( title );
}

wxString AFMonthByHourFactorCtrl::GetTitle()
{
	return m_title->GetLabel();
}

void AFMonthByHourFactorCtrl::SetLegend( const wxString &legend )
{
	m_legend->SetLabel( legend );
}

wxString AFMonthByHourFactorCtrl::GetLegend()
{
	return m_legend->GetLabel();
}


void AFMonthByHourFactorCtrl::SetData(const matrix_t<double> &data)
{
	for (size_t r=0;r<SFROWS;r++)
	{
		for (size_t c=0;c<SFCOLS;c++)
		{
			double val = 0;
			if (r < data.nrows() && c < data.ncols())
				val = data.at(r,c);

			mData.at(r,c) = val;
		}
	}
	UpdateGrid();
}

void  AFMonthByHourFactorCtrl::GetData( matrix_t<double> &mat )
{
	mat = mData;
}

matrix_t<double> AFMonthByHourFactorCtrl::GetData()
{
	 return mData; 
}

void AFMonthByHourFactorCtrl::UpdateGrid()
{

	int r,c;
	for (r=0;r<SFROWS;r++)
		for (c=0;c<SFCOLS;c++)
			UpdateCell(r,c);

	mGrid->SetRowLabelSize(wxGRID_AUTOSIZE);
	mGrid->SetColLabelSize(wxGRID_AUTOSIZE);
	mGrid->AutoSize();
	mGrid->Layout();
	mGrid->ForceRefresh();
	Layout();
}

void AFMonthByHourFactorCtrl::UpdateCell(int r, int c)
{
	double sf = mData.at(r,c);
	if (sf<0.0) sf = 0.0;
	if (sf>100) sf = 100;

	mGrid->SetCellValue( r, c, wxString::Format("%lg", sf ) );
	int cntrIndex = (int)(sf);
	int ncv = 100;

	wxColour shadc;								
	shadc.Set(
		((ncv-cntrIndex) * Colour1.Red()   +
			cntrIndex * Colour2.Red())/ncv,

		((ncv-cntrIndex) * Colour1.Green() +
			cntrIndex * Colour2.Green())/ncv,

		((ncv-cntrIndex) * Colour1.Blue()  +
			cntrIndex * Colour2.Blue())/ncv );

	mGrid->SetCellBackgroundColour(r,c, shadc);

}

void AFMonthByHourFactorCtrl::OnGridCellChange(wxGridEvent &evt)
{
	int r = evt.GetRow();
	int c = evt.GetCol();
	if ( r < 0 || c < 0 )
	{
		// paste event
		for( int ir=0;ir<mGrid->GetNumberRows();ir++ )
		{
			for( int ic=0;ic<mGrid->GetNumberCols();ic++ )
			{
				mData.at(ir,ic) = wxAtof( mGrid->GetCellValue(ir,ic) );
				UpdateCell(ir,ic);
			}
		}
	}
	else
	{
		double sf = wxAtof( mGrid->GetCellValue(r,c) );

		ApplyVal(r,c,sf);

		mSelTopRow = -1;
		mSelBottomRow = -1;
		mSelLeftCol = -1;
		mSelRightCol = -1;
	}
}

void AFMonthByHourFactorCtrl::ApplyVal(int r, int c, double sf)
{
	if (sf<0.0) sf = 0.0;
	if (sf>100) sf = 100;

	if (mSelTopRow >= 0 && mSelLeftCol >= 0)
	{
		for (r=mSelTopRow; r <= mSelBottomRow; r++)
		{
			for (c=mSelLeftCol; c <= mSelRightCol; c++)
			{
				mData.at(r,c) = sf;
				UpdateCell(r,c);
			}
		}
		DispatchEvent();
	}
	else if (r>=0 && c>=0)
	{	
		mData.at(r,c) = sf;
		UpdateCell(r,c);
		DispatchEvent();
	}

	mGrid->ForceRefresh();
}

void AFMonthByHourFactorCtrl::OnGridRangeSelect(wxGridRangeSelectEvent &evt)
{
	if (evt.CmdDown())
	{
		evt.Veto();
		return;
	}

	if (evt.Selecting() && !bSkipSelect)
	{
		
		mSelTopRow = evt.GetTopRow();
		mSelBottomRow = evt.GetBottomRow();
		mSelLeftCol = evt.GetLeftCol();
		mSelRightCol = evt.GetRightCol();
	}
	evt.Skip();
}

void AFMonthByHourFactorCtrl::OnGridEditorShown(wxGridEvent &)
{
	bSkipSelect = true;
}

void AFMonthByHourFactorCtrl::OnGridEditorHidden(wxGridEvent &)
{
	bSkipSelect = false;
}

void AFMonthByHourFactorCtrl::OnGridCellSelect(wxGridEvent &evt)
{
	if (evt.CmdDown())
	{
		evt.Veto();
		return;
	}

	if (!bSkipSelect)
	{
		mSelTopRow = evt.GetRow();
		mSelBottomRow = mSelTopRow;
		mSelLeftCol = evt.GetCol();
		mSelRightCol = mSelLeftCol;
	}
	evt.Skip();
}

void AFMonthByHourFactorCtrl::OnApply(wxCommandEvent &)
{
	double sf = mShadingVal->Value();
	if (sf <= -1)
	{
		if (wxTheClipboard->Open())
		{
			wxCSVData csv;
			csv.SetSeparator( '\t' );
			for( size_t i=0;i<mData.nrows();i++ )
				for( size_t j=0;j<mData.ncols();j++)
					csv.Set(i,j, wxString::Format("%g", mData(i,j) ) );
		// This data objects are held by the clipboard, 
		// so do not delete them in the app.
			wxTheClipboard->SetData( new wxTextDataObject( csv.WriteString() ) );
			wxTheClipboard->Close();
		}
	}

	ApplyVal(-1,-1,sf);
}

void AFMonthByHourFactorCtrl::OnImport(wxCommandEvent &)
{
	wxFileDialog fdlg(this, "Import Shading Factors", "", "",
		"CSV Files (*.csv)|*.csv|All Files (*.*)|*.*", wxFD_OPEN );

	if (fdlg.ShowModal() == wxID_OK)
	{
		wxCSVData csv;
		if ( !csv.ReadFile( fdlg.GetPath() ))
		{
			wxMessageBox("Could not open file for reading:\n\n" + fdlg.GetPath(), "Error", wxICON_ERROR|wxOK);
			return;
		}

		if (csv.NumRows() != 12 || csv.NumCols() != 24)
		{
			wxMessageBox("Invalid shading factor data. Must have 12 rows and 24 columns.");
			return;
		}

		matrix_t<double> grid;
		grid.resize_fill( csv.NumRows(), csv.NumCols(), 0.0f );

		for (size_t r=0;r<grid.nrows();r++)
			for (size_t c=0;c<grid.ncols();c++)
				grid.at(r,c) = (float)wxAtof( csv(r,c) );

		SetData( grid );
		DispatchEvent();
	}
}

void AFMonthByHourFactorCtrl::OnCopy(wxCommandEvent &)
{
	wxBusyCursor busycurs;
	wxString sdata;
	for (size_t r=0;r<mData.nrows();r++)
	{
		for (size_t c=0;c<mData.ncols();c++)
		{
			sdata += wxString::Format("%g", mData(r,c) );
			if ( c < mData.ncols()-1 ) sdata += '\t';
			else sdata += '\n';
		}
	}
	
	if (wxTheClipboard->Open())
	{
		wxTheClipboard->SetData( new wxTextDataObject( sdata ) );
		wxTheClipboard->Close();
	}
}

void AFMonthByHourFactorCtrl::OnPaste(wxCommandEvent &)
{
	wxBusyCursor busycurs;
	// resize rows per data pasted
	if (wxTheClipboard->Open())
	{
		wxString data;
		wxTextDataObject textobj;
		if (wxTheClipboard->GetData(textobj))
		{
			data = textobj.GetText();
			wxTheClipboard->Close();
		}
		if (data.IsEmpty()) return;

#ifdef __WXMAC__
		wxArrayString lines = wxStringTokenize(data, "\r", ::wxTOKEN_RET_EMPTY_ALL);
#else
		wxArrayString lines = wxStringTokenize(data, "\n", ::wxTOKEN_RET_EMPTY_ALL);
#endif

		int ncols = 0;
		if (lines.Count() > 0)
		{
			matrix_t<double> grid;
			grid.resize_fill(mData.nrows(), mData.ncols(), 0.0f);

			for (size_t r = 0; r < grid.nrows(); r++)
				for (size_t c = 0; c < grid.ncols(); c++)
					grid.at(r, c) = (float)mData.at(r, c);

			for (size_t r = 0; r < mData.nrows() && r < lines.Count(); r++)
			{
				wxArrayString vals = wxStringTokenize(lines[r], "\t", ::wxTOKEN_RET_EMPTY_ALL);
				ncols = vals.Count();
				for (int c = 0; c < (int)mData.ncols() && c < ncols; c++)
					grid.at(r, c) = (float)wxAtof(vals[c].c_str());
			}

			SetData(grid);
			DispatchEvent();
		}
	}

}



void AFMonthByHourFactorCtrl::OnExport(wxCommandEvent &)
{
	wxFileDialog fdlg(this, "Export Shading Factors", "", "shading_factors.csv",
		"CSV Files (*.csv)|*.csv|All Files (*.*)|*.*", wxFD_SAVE | wxFD_OVERWRITE_PROMPT );

	if (fdlg.ShowModal() == wxID_OK)
	{
		wxCSVData csv;
		for (size_t r=0;r<mData.nrows();r++)
			for (size_t c=0;c<mData.ncols();c++)
				csv.Set( r,c, wxString::Format("%g", mData(r,c) ) );

		if ( !csv.WriteFile( fdlg.GetPath() ) )
			wxMessageBox("Could not write file:\n\n"+fdlg.GetPath(), "Error", wxICON_ERROR|wxOK);
	}
}


void AFMonthByHourFactorCtrl::DispatchEvent()
{	
	wxCommandEvent change(wxEVT_AFMonthByHourFactorCtrl_CHANGE, this->GetId() );
	change.SetEventObject( this );
	GetEventHandler()->ProcessEvent(change);
}


BEGIN_EVENT_TABLE(AFToolTipCtrl, wxPanel)
	EVT_LEFT_DOWN(AFToolTipCtrl::mouseDown)
	EVT_PAINT(AFToolTipCtrl::paintEvent)
END_EVENT_TABLE()

DEFINE_EVENT_TYPE(wxEVT_TOOLTIPCTRL_CHANGE)

AFToolTipCtrl::AFToolTipCtrl(wxWindow* parent) :
	wxPanel(parent)
{
	m_image = wxBITMAP_PNG_FROM_DATA(info24);
}

void AFToolTipCtrl::mouseDown(wxMouseEvent& ) 
{
	wxCommandEvent change(wxEVT_TOOLTIPCTRL_CHANGE, this->GetId());
	change.SetEventObject(this);
	GetEventHandler()->ProcessEvent(change);
}

void AFToolTipCtrl::paintEvent(wxPaintEvent & )
{
	wxPaintDC dc(this);
	render(dc);
}

void AFToolTipCtrl::paintNow()
{
	wxClientDC dc(this);
	render(dc);
}

void AFToolTipCtrl::render(wxDC&  dc)
{
	dc.DrawBitmap(m_image, 0, 0, false);
}

