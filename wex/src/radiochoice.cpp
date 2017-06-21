#include "wex/radiochoice.h"
#include "wex/utils.h"

enum { ID_CHILD_RADIO = wxID_HIGHEST + 938 };

BEGIN_EVENT_TABLE(wxRadioChoice, wxPanel)
	EVT_RADIOBUTTON( ID_CHILD_RADIO, wxRadioChoice::OnRadio)
	EVT_SIZE( wxRadioChoice::OnResize )
END_EVENT_TABLE()

static int s_minHeight = -1;

wxRadioChoice::wxRadioChoice( wxWindow *parent, int id, const wxPoint &pos, const wxSize &size)
	: wxPanel( parent, id, pos, size, wxTAB_TRAVERSAL )
{
	if ( s_minHeight < 0 ) s_minHeight = (int)(21.0*wxGetScreenHDScale());

	SetBackgroundColour( parent->GetBackgroundColour() );
	m_showCaptions = true;
	m_horizontal = false;
	m_evenly = false;
}

bool wxRadioChoice::Enable(bool b)
{
	for (size_t i=0;i<m_buttons.size();i++)
		m_buttons[i]->Enable(b);
	return wxWindow::Enable(b);
}

void wxRadioChoice::Enable(int idx, bool b)
{
	if (idx >= 0 && idx < (int)m_buttons.size())
		m_buttons[idx]->Enable(b);
}

bool wxRadioChoice::IsEnabled(int idx)
{
	if (idx >= 0 && idx < (int)m_buttons.size())
		return m_buttons[idx]->IsEnabled();
	else
		return false;
}

void wxRadioChoice::OnRadio(wxCommandEvent &evt)
{
	wxCommandEvent radioclick(wxEVT_COMMAND_RADIOBUTTON_SELECTED , GetId() );
	radioclick.SetEventObject(this);
	radioclick.SetString(GetValue());
	radioclick.SetInt(GetSelection());
	GetEventHandler()->ProcessEvent(radioclick);
}

void wxRadioChoice::OnResize(wxSizeEvent &)
{
	Rearrange();
}

void wxRadioChoice::Add(const wxString &caption, bool arrange)
{
	wxString label;
	if (m_showCaptions) label = caption;

	wxRadioButton *b = new wxRadioButton(this, ID_CHILD_RADIO, label );
	b->SetBackgroundColour( GetParent()->GetBackgroundColour() );
	m_buttons.push_back(b);
	m_captions.Add(caption);
	
	if (arrange)
		Rearrange();
}

void wxRadioChoice::Add(const wxArrayString &list)
{
	for (int i=0;i<(int)list.size();i++)
		Add( list[i], false );

	Rearrange();
}

int wxRadioChoice::Find(const wxString &caption)
{
	for (size_t i=0;i<m_buttons.size();i++)
		if (m_captions[i] == caption)
			return i;

	return -1;
}

int wxRadioChoice::GetCount()
{
	return m_buttons.size();
}

void wxRadioChoice::Remove(int idx)
{
	if (idx >= 0 && idx < (int)m_buttons.size())
	{
		m_buttons[idx]->Destroy();
		m_buttons.erase( m_buttons.begin() + idx );
		m_captions.RemoveAt( idx );
		Rearrange();
	}
}

void wxRadioChoice::Remove(const wxString &caption)
{
	Remove( Find(caption) );
}

wxSize wxRadioChoice::DoGetBestSize() const
{
	wxSize size( 0, 0 );
	if ( m_horizontal )
	{
		for( size_t i=0;i<m_buttons.size();i++ )
		{
			m_buttons[i]->InvalidateBestSize();
			wxSize s( m_buttons[i]->GetBestSize() );
			if ( s.y < s_minHeight ) s.y = s_minHeight;
			if ( s.y > size.y ) size.y = s.y;
			size.x += s.x;
		}
	}
	else
	{
		// vertical layout
		for( size_t i=0;i<m_buttons.size();i++ )
		{
			m_buttons[i]->InvalidateBestSize();
			wxSize s( m_buttons[i]->GetBestSize() );
			if ( s.y < s_minHeight ) s.y = s_minHeight;
			if( s.x > size.x ) size.x = s.x;
			size.y += s.y;
		}
	}

	return size;
}

void wxRadioChoice::Clear()
{
	for (size_t i=0;i<m_buttons.size();i++)
		m_buttons[i]->Destroy();

	m_captions.Clear();
	m_buttons.clear();
	Rearrange();
}

int wxRadioChoice::GetSelection()
{
	for (size_t i=0;i<m_buttons.size();i++)
		if (m_buttons[i]->GetValue())
			return i;

	return -1;
}

wxString wxRadioChoice::GetValue()
{
	int idx = GetSelection();
	if (idx >= 0)
		return m_buttons[idx]->GetLabel();

	return wxEmptyString;
}

void wxRadioChoice::SetValue(const wxString &sel)
{
	int idx = Find(sel);
	if (idx >= 0) m_buttons[idx]->SetValue(true);
}

void wxRadioChoice::SetSelection(int id)
{
	if (id >= 0 && id < (int)m_buttons.size())
		m_buttons[id]->SetValue(true);
}

void wxRadioChoice::Rearrange()
{
	InvalidateBestSize();

	int c_width, c_height;

	GetClientSize(&c_width, &c_height);

	if ( m_horizontal )
	{
		int b_width = c_width;
		if ( m_buttons.size() > 0 )
			b_width /= m_buttons.size();
		int x = 0;
		for (size_t i=0;i<m_buttons.size();i++)
		{
			m_buttons[i]->SetSize(x,0,b_width, c_height);
			x += b_width;
		}
	}
	else
	{
		int b_height = s_minHeight;
		if ( m_buttons.size() > 0 )
		{
			int best_height = m_buttons[0]->GetBestSize().GetHeight();
			if ( best_height > s_minHeight )
				b_height = best_height;

			if ( m_evenly );
				b_height = c_height / m_buttons.size();
		}

		int y = 0;
		for (size_t i=0;i<m_buttons.size();i++)
		{
			m_buttons[i]->SetSize(0,y,c_width, b_height);
			y += b_height;
		}
	}
}

void wxRadioChoice::ShowCaptions(bool b)
{
	if (b == m_showCaptions)
		return;

	m_showCaptions = b;
	if (m_captions.size() != m_buttons.size())
		return;

	for (size_t i=0;i<m_buttons.size();i++)
	{
		wxString label;
		if (m_showCaptions) label = m_captions[i];
		m_buttons[i]->SetLabel( label );
	}

	Refresh();
}

bool wxRadioChoice::CaptionsShown()
{
	return m_showCaptions;
}

void wxRadioChoice::SetHorizontal( bool b )
{
	m_horizontal = b;
	Rearrange();
}

bool wxRadioChoice::IsHorizontal()
{
	return m_horizontal;
}

void wxRadioChoice::LayoutEvenly( bool b )
{
	m_evenly = b;
	Rearrange();
}

void wxRadioChoice::SetLabel( int idx, const wxString &lbl )
{
	if ( idx < (int) m_buttons.size() )
	{
		m_buttons[idx]->SetLabel( lbl );
		InvalidateBestSize();
	}
}

wxString wxRadioChoice::GetLabel( int idx )
{
	if ( idx < (int) m_buttons.size() ) return m_buttons[idx]->GetLabel();
	else return wxEmptyString;
}
