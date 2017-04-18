#include <wx/wx.h>
#include <wx/file.h>

#include "wex/utils.h"
#include "wex/metro.h"
#include "wex/tpdlg.h"

BEGIN_EVENT_TABLE( wxThreadProgressDialog, wxDialog )
	EVT_BUTTON( wxID_SAVE, wxThreadProgressDialog::OnSaveLog )
	EVT_BUTTON( wxID_CANCEL, wxThreadProgressDialog::OnCancel )
	EVT_CLOSE( wxThreadProgressDialog::OnDialogClose )
END_EVENT_TABLE( )

wxThreadProgressDialog::wxThreadProgressDialog(wxWindow *parent, int nthreads, bool border)
	: wxDialog( parent, wxID_ANY, wxEmptyString, wxDefaultPosition, 
	wxScaleSize(625, 475), (border ? wxBORDER_SIMPLE : wxBORDER_NONE )
#if defined(__WXOSX__)||defined(__WXGTK__)
	|wxSTAY_ON_TOP // on OSX/GTK+ for some reason, we need this for the dialog show up on top of the transparent pane which is the parent
#endif
	 )
{
	SetBackgroundColour( *wxWHITE );
	CenterOnParent();

	m_canceled = false;
	m_button = new wxMetroButton(this, wxID_CANCEL, "Cancel");//, wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT);
	
	wxBoxSizer *szv = new wxBoxSizer(wxVERTICAL);

	m_status = new wxStaticText( this, wxID_ANY, "Processing..." );
	m_status->SetFont( wxMetroTheme::Font( wxMT_LIGHT, 15 ) );
	m_status->SetForegroundColour( wxMetroTheme::Colour( wxMT_TEXT ) );

	szv->Add( m_status, 0, wxALL|wxEXPAND, 20 );

	for (int i=0;i<nthreads;i++)
	{
			
		wxStaticText *label = new wxStaticText(this, wxID_ANY, wxString::Format("Process %d", i+1));
		label->SetForegroundColour( wxMetroTheme::Colour( wxMT_TEXT ) );

		wxGauge *gauge = new wxGauge(this, wxID_ANY, 100);
		wxTextCtrl *text = new wxTextCtrl(this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_READONLY|wxBORDER_NONE);
		text->SetBackgroundColour( *wxWHITE );
		text->SetForegroundColour( wxMetroTheme::Colour( wxMT_TEXT ) );
		
		wxBoxSizer *sizer = new wxBoxSizer( wxHORIZONTAL );
		sizer->Add( label, 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
		sizer->Add( gauge, 1, wxALL|wxEXPAND, 5 );
		sizer->Add( text, 0, wxALL|wxEXPAND, 5 );

		m_labels.push_back(label);
		m_progbars.push_back(gauge);
		m_percents.push_back(text);
			
		szv->Add( sizer, 0, wxEXPAND|wxALL, 5 );
	}
	
	m_log = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxBORDER_NONE );
	m_log->SetForegroundColour( wxMetroTheme::Colour( wxMT_TEXT ) );		
	szv->Add( m_log, 1, wxALL|wxEXPAND, 10 );

	szv->Add( m_button, 0, wxCENTER|wxLEFT|wxRIGHT|wxBOTTOM, 10);

	
	// not in a sizer.  will be manually positioned when ShowSaveLogButton is called.
	m_saveLog = new wxMetroButton(this, wxID_SAVE, "Save log");
	m_saveLog->Hide();
	
	SetSizer(szv);
}

void wxThreadProgressDialog::SetButtonText( const wxString &text )
{
	m_button->SetLabel( text );
	Layout();
	wxYield();
}

void wxThreadProgressDialog::ShowSaveLogButton()
{
	wxSize client( GetClientSize() );
	wxSize best( m_saveLog->GetBestSize() );

	// TODO - check on high DPI screens if sizer spacing aligns with 10 or
	// if this offset value needs to be scaled by wxGetScreenHDScale()
	m_saveLog->SetSize( 10, client.y - best.y - 10, best.x, best.y );
	m_saveLog->Show();
	Layout();
	wxYield();
}

void wxThreadProgressDialog::Status( const wxString &s )
{
	m_status->SetLabel( s );
}

void wxThreadProgressDialog::Reset()
{
	for( size_t i=0;i<m_progbars.size();i++ )
	{
		m_labels[i]->SetLabel(wxString::Format("Process %d", (int)(i + 1)));
		m_progbars[i]->SetValue(0);
		m_percents[i]->ChangeValue( wxEmptyString );
	}

	Layout();
}

void wxThreadProgressDialog::ShowBars( int n )
{
	if ( n < 0 ) n = m_progbars.size();
	for( size_t i=0;i<m_progbars.size();i++ )
	{
		bool show = (i < n);
		m_labels[i]->Show( show );
		m_progbars[i]->Show( show );
		m_percents[i]->Show( show );
	}

	Layout();
}

void wxThreadProgressDialog::Log( const wxArrayString &list )
{
	for (size_t i=0;i<list.Count();i++)
		Log(list[i]);
}

bool wxThreadProgressDialog::HasMessages()
{
	return ( m_log->GetValue().Len() > 0 );
}

wxString wxThreadProgressDialog::GetMessages()
{
	return m_log->GetValue();
}

void wxThreadProgressDialog::Log( const wxString &text )
{
	m_log->AppendText( text + "\n" );
}

void wxThreadProgressDialog::Update(int ThreadNum, float percent, const wxString &text)
{
	if (ThreadNum >= 0 && ThreadNum < m_progbars.size())
	{
		m_progbars[ThreadNum]->SetValue( (int)percent );
		m_percents[ThreadNum]->ChangeValue( wxString::Format("%.1f %%", percent) );
		if ( !text.IsEmpty() )
		{
			m_labels[ThreadNum]->SetLabel( text );
			Layout();
		} else {
			wxString label( wxString::Format("Process %d", ThreadNum+1) );
			if ( m_labels[ThreadNum]->GetLabel() != label )
			{
				m_labels[ThreadNum]->SetLabel( label );
				Layout();
			}
		}
	}
}

void wxThreadProgressDialog::OnSaveLog( wxCommandEvent & )
{
	wxFileDialog dialog( this, "Save messages", wxEmptyString, "log.txt", "Text files (*.txt)|*.txt", wxFD_SAVE|wxFD_OVERWRITE_PROMPT );
	if ( wxID_OK == dialog.ShowModal() )
	{
		wxFile fp( dialog.GetPath(), wxFile::write );
		if ( fp.IsOpened() )
			fp.Write( m_log->GetValue() );
		else
			wxMessageBox("Could not write to file:\n\n" + dialog.GetPath() );
	}
}

void wxThreadProgressDialog::Finalize( const wxString &title )
{
	if ( HasMessages() )
	{
		if ( title.IsEmpty() ) Status( "Finished with notices." );
		else Status( title );

		ShowBars( 0 );
		SetButtonText( "OK" );
		ShowSaveLogButton();
		Hide();
		ShowModal();
	}
}
	
void wxThreadProgressDialog::OnCancel(wxCommandEvent &evt)
{
	m_canceled = true;
	if ( IsModal() )
		EndModal( wxID_CANCEL );
}

void wxThreadProgressDialog::OnDialogClose(wxCloseEvent &evt)
{
	m_canceled = true;
	evt.Skip();
}

