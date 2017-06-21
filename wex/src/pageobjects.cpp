#include <wx/tokenzr.h>
#include <wx/datstrm.h>
#include <wx/sstream.h>
#include <wx/image.h>
#include <wx/clrpicker.h>
#include <wx/dialog.h>
#include <wx/spinctrl.h>
#include <wx/sizer.h>
#include <wx/richtext/richtextsymboldlg.h>

#include "wex/pageobjects.h"


enum { IDT_TEXT=1495, IDT_BOLD, IDT_ITALIC, IDT_FACE, IDT_SIZE, IDT_COLOUR, IDT_SYMBOL, IDT_ALIGN };

BEGIN_EVENT_TABLE( wxPageTextObjectEditDialog, wxDialog )
	EVT_COLOURPICKER_CHANGED( IDT_COLOUR, wxPageTextObjectEditDialog::OnColour )
	EVT_TEXT( IDT_TEXT, wxPageTextObjectEditDialog::OnChange )
	EVT_SPINCTRL( IDT_SIZE, wxPageTextObjectEditDialog::OnSpin )
	EVT_CHECKBOX( IDT_BOLD, wxPageTextObjectEditDialog::OnChange )
	EVT_CHECKBOX( IDT_ITALIC, wxPageTextObjectEditDialog::OnChange )
	EVT_COMBOBOX( IDT_FACE, wxPageTextObjectEditDialog::OnChange )
	EVT_COMBOBOX( IDT_ALIGN, wxPageTextObjectEditDialog::OnChange )
	EVT_BUTTON( IDT_SYMBOL, wxPageTextObjectEditDialog::OnSymbol )
END_EVENT_TABLE()

wxPageTextObjectEditDialog::wxPageTextObjectEditDialog( wxWindow *parent )
	: wxDialog( parent, wxID_ANY, wxString("Edit Text Object"),
		wxDefaultPosition, wxDefaultSize, 
		wxCAPTION|wxRESIZE_BORDER|wxCLOSE_BOX|wxSYSTEM_MENU)
{
	m_textObject = 0;
	m_layoutCtrl = 0;

	SetEscapeId( wxID_CANCEL );

	m_txtData = new wxTextCtrl( this, IDT_TEXT, wxEmptyString, 
		wxDefaultPosition, wxDefaultSize, 
		wxTE_MULTILINE|wxTE_PROCESS_TAB|wxTE_PROCESS_ENTER|wxTE_DONTWRAP );
	m_txtData->SetMinSize( wxSize( 500, 350 ) );

	m_chkBold = new wxCheckBox( this, IDT_BOLD, "Bold" );
	m_chkItalic = new wxCheckBox( this, IDT_ITALIC, "Italic" );
	wxString faces[3] = { "Serif", "Sanserif", "Fixed" };
	m_cboFace = new wxComboBox( this, IDT_FACE, "Serif", 
		wxDefaultPosition, wxDefaultSize, 3, faces, wxCB_READONLY );

	m_spnSize = new wxSpinCtrl( this, IDT_SIZE );
	m_spnSize->SetRange( 1, 100 );
	m_spnSize->SetValue( 12 );


	wxString alignments[3] = { "Left", "Center", "Right" };
	m_cboAlign = new wxComboBox( this, IDT_ALIGN, "Left",
		wxDefaultPosition, wxDefaultSize, 3, alignments, wxCB_READONLY );

	m_cpcColour = new wxColourPickerCtrl( this, IDT_COLOUR, *wxGREEN, wxDefaultPosition, wxDefaultSize/*, wxCLRP_SHOW_LABEL */ );

	m_toolSizer = new wxBoxSizer( wxHORIZONTAL );
	m_toolSizer->Add( m_cboFace, 0, wxRIGHT|wxALIGN_CENTER, 4  );
	m_toolSizer->Add( m_spnSize, 0, wxRIGHT|wxALIGN_CENTER, 4   );
	m_toolSizer->Add( m_chkBold, 0, wxRIGHT|wxALIGN_CENTER, 4   );
	m_toolSizer->Add( m_chkItalic, 0, wxRIGHT|wxALIGN_CENTER, 4   );
	m_toolSizer->Add( m_cboAlign, 0, wxRIGHT|wxALIGN_CENTER, 4   );
	m_toolSizer->Add( m_cpcColour, 0, wxRIGHT|wxALIGN_CENTER, 4   );
	m_toolSizer->Add( new wxButton( this, IDT_SYMBOL, "@...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT ), 0, wxRIGHT|wxALIGN_CENTER, 4 );

	wxBoxSizer *lay = new wxBoxSizer( wxVERTICAL );
	lay->Add( m_toolSizer, 0, wxEXPAND|wxALL, 4 );
	lay->Add( m_txtData, 1, wxEXPAND|wxALL, 4 );
	lay->Add( CreateButtonSizer( wxOK|wxCANCEL ) , 0, wxEXPAND|wxALL, 4 );

	SetSizer(lay);
	lay->SetSizeHints( this );
}

void wxPageTextObjectEditDialog::SetData( wxPageTextObject *obj, wxPageLayoutCtrl *layout )
{

	m_txtData->SetValue( obj->GetText() );
	m_chkBold->SetValue( obj->GetBold() );
	m_chkItalic->SetValue( obj->GetItalic() );
	m_cboFace->SetSelection( obj->GetFace() );
	m_spnSize->SetValue( obj->GetSize() );
	m_cpcColour->SetColour( obj->GetColour() );

	switch( obj->GetAlign() )
	{
	case wxLEFT: m_cboAlign->SetSelection(0); break;
	case wxCENTER: m_cboAlign->SetSelection(1); break;
	default: m_cboAlign->SetSelection(2); break;
	}

	m_txtData->SetFocus();
	
	m_layoutCtrl = layout;
	m_textObject = obj;
}

void wxPageTextObjectEditDialog::GetData( wxPageTextObject *obj )
{
	obj->SetText( m_txtData->GetValue() );
	obj->SetBold( m_chkBold->GetValue() );
	obj->SetItalic( m_chkItalic->GetValue() );
	obj->SetFace( m_cboFace->GetSelection() );
	obj->SetSize( m_spnSize->GetValue() );
	obj->SetColour( m_cpcColour->GetColour() );
	int align = wxRIGHT;
	if (m_cboAlign->GetSelection() == 0) align = wxLEFT;
	else if (m_cboAlign->GetSelection() == 1) align = wxCENTER;
	obj->SetAlign( align );
}

void wxPageTextObjectEditDialog::OnChange( wxCommandEvent & )
{
	if ( m_layoutCtrl != 0 && m_textObject != 0 )
	{
		GetData( m_textObject );
		m_layoutCtrl->Invalidate( m_textObject );
	}
}

void wxPageTextObjectEditDialog::OnSymbol( wxCommandEvent &e )
{
	wxSymbolPickerDialog dlg( "*", wxEmptyString, wxEmptyString, this );
	if (dlg.ShowModal() == wxID_OK)
	{
		m_txtData->WriteText( dlg.GetSymbol() );
		OnChange(e);
	}
}

/**************************************************/

wxPageTextObject::wxPageTextObject()
{
	m_text = "Double-click to edit...";
	m_face = wxPageOutputDevice::SERIF;
	m_size = 12;
	m_colour = *wxBLACK;
	m_bold = false;
	m_italic = false;
	m_align = wxLEFT;
}

wxPageObject *wxPageTextObject::Duplicate()
{
	wxPageTextObject *o = new wxPageTextObject;
	o->Copy(this);
	return o;
}

bool wxPageTextObject::Copy( wxPageObject *obj )
{
	if (wxPageTextObject *rhs = dynamic_cast< wxPageTextObject* >( obj ))
	{
		m_text = rhs->m_text;
		m_face = rhs->m_face;
		m_size = rhs->m_size;
		m_colour = rhs->m_colour;
		m_bold = rhs->m_bold;
		m_italic = rhs->m_italic;
		m_align = rhs->m_align;
		return true;
	}
	else return false;
}

bool wxPageTextObject::EditObject( wxPageLayoutCtrl *layout )
{
	wxString saveText = m_text;
	bool saveBold = m_bold;
	bool saveItalic = m_italic;
	int saveFace = m_face;
	int saveSize = m_size;
	wxColour saveColour = m_colour;
	int saveAlign = m_align;

	wxPageTextObjectEditDialog dlg( 0 );
	dlg.SetData( this, layout );
	if (dlg.ShowModal() == wxID_OK)
	{
		dlg.GetData( this );
		return true;
	}
	else
	{
		m_text = saveText;
		m_bold = saveBold;
		m_italic = saveItalic;
		m_face = saveFace;
		m_size = saveSize;
		m_colour = saveColour;
		m_align = saveAlign;
		return false;
	}
}

void wxPageTextObject::Render( wxPageOutputDevice &dv )
{
	dv.Color( m_colour );
	dv.Font( m_face, m_size, m_bold, m_italic );
	wxArrayString lines = wxStringTokenize( m_text, "\n\r", ::wxTOKEN_RET_EMPTY_ALL );
	float line_height = 0.2f; // inches (approximate for 12pt)
	dv.Measure("H", 0, &line_height);

	float y = m_y;
	for (size_t i=0;i<lines.Count();i++)
	{
		lines[i].Replace("\t", "     ");
		if (m_align == wxLEFT) dv.Text( m_x, y, lines[i] );
		else
		{
			float width;
			dv.Measure( lines[i], &width, 0 );
			if ( m_align == wxCENTER ) dv.Text( m_x + m_width/2 - width/2, y, lines[i] ); // center
			else dv.Text( m_x + m_width - width, y, lines[i] ); // right
		}

		y += line_height;
	}
}


bool wxPageTextObject::ReadData( wxInputStream &is )
{
	if ( !is.IsOk() ) return false;

	wxDataInputStream in(is);

	unsigned short code = in.Read16();
	unsigned char ver = in.Read8();
	if (ver > 0)
	{
		m_face = in.Read8();
		m_size = in.Read8();
		unsigned char red = in.Read8();
		unsigned char green = in.Read8();
		unsigned char blue = in.Read8();
		m_colour.Set( red, green, blue );
		m_bold = in.Read8() ? true : false;
		m_italic = in.Read8() ? true : false;
		m_text = in.ReadString();

		if (ver > 1)
		{
			m_align = in.Read8();
		}

		return in.Read16() == code;
	}
	else return false;
}

bool wxPageTextObject::WriteData( wxOutputStream &os )
{
	if ( !os.IsOk() ) return false;
	wxDataOutputStream out(os);
	out.Write16( 0xaa );
	out.Write8( 2 ); // version
	out.Write8( m_face );
	out.Write8( m_size );
	out.Write8( m_colour.Red() );
	out.Write8( m_colour.Green() );
	out.Write8( m_colour.Blue() );
	out.Write8( m_bold ? 1 : 0 );
	out.Write8( m_italic ? 1 : 0 );
	out.WriteString( m_text );
	out.Write8( m_align );
	out.Write16( 0xaa );
	return true;
}

void wxPageTextObject::SetText( const wxString &text )
{
	m_text = text;
	m_text.Replace("\r", "");
	m_text.Replace("\v", "");
}

void wxPageTextObject::SetSize( int points )
{
	if (points < 1) points = 1;
	if (points > 300) points = 300;
	m_size = points;
}


/* ******************************************************************** */

bool wxPageImageObject::EditObject( wxPageLayoutCtrl *parent )
{
	wxFileDialog dlg( parent, "Choose Image File", wxEmptyString,
		wxEmptyString, "All Images|*.png;*.jpg;*.bmp;*.gif;*.xpm;*.tiff");
	if (dlg.ShowModal() == wxID_OK)
	{
		m_image.LoadFile( dlg.GetPath() );
		return true;
	}
	else
		return false;
}

wxPageObject *wxPageImageObject::Duplicate()
{
	wxPageImageObject *o = new wxPageImageObject;
	o->Copy(this);
	return o;
}

bool wxPageImageObject::Copy( wxPageObject *obj )
{
	if (wxPageImageObject *rhs = dynamic_cast<wxPageImageObject*>( obj ))
	{
		m_image = rhs->m_image;
		return true;
	}
	else return false;
}

void wxPageImageObject::Render( wxPageOutputDevice &dv )
{
	if (!m_image.IsOk())
	{
		dv.Color( *wxBLACK );
		dv.Rect( m_x, m_y, m_width, m_height, false );
		dv.Font( wxPageOutputDevice::SERIF, 12, true, false );
		dv.Text( m_x+0.1, m_y+0.1,  "Double-click to choose image..." );
	}
	else
		dv.Image( m_image, m_x, m_y );
}

bool wxPageImageObject::ReadData( wxInputStream &is )
{
	if ( !is.IsOk() ) return false;
	wxDataInputStream in(is);
	unsigned short code = in.Read16();
	unsigned char version = in.Read8();
	bool imgok = wxPNGHandler().LoadFile( &m_image, is, false );
	return in.Read16() == code && imgok && version > 0;
}

bool wxPageImageObject::WriteData( wxOutputStream &os )
{
	if ( !os.IsOk() ) return false;
	wxDataOutputStream out(os);
	out.Write16( 0xab ); // code
	out.Write8( 1 ); // version
	bool imgok = wxPNGHandler().SaveFile( &m_image, os, false );
	out.Write16( 0xab );
	return imgok;
}
