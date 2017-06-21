#include <wx/textctrl.h>
#include <wx/datstrm.h>
#include <wx/dcbuffer.h>
#include <wx/clrpicker.h>
#include <wx/splitter.h>
#include <wx/tokenzr.h>
#include <wx/renderer.h>
#include <wx/hyperlink.h>

#include <wex/extgrid.h>
#include <wex/label.h>
#include <wex/radiochoice.h>
#include <wex/numeric.h>
#include <wex/exttext.h>
#include <wex/utils.h>
#include <wex/uiform.h>
#include <wex/diurnal.h>
#include <algorithm>

static wxColour g_uiSelectColor( 135, 135, 135 );

class wxUIButtonObject : public wxUIObject
{
public:
	wxUIButtonObject() {
		AddProperty( "Caption", new wxUIProperty( "Button" ) );
		AddProperty( "TabOrder", new wxUIProperty( -1 ) );
	}
	virtual wxString GetTypeName() { return "Button"; }
	virtual wxUIObject *Duplicate() { wxUIObject *b = new wxUIButtonObject; b->Copy( this ); return b; }	
	virtual bool IsNativeObject() { return true; }
	virtual wxWindow *CreateNative( wxWindow *parent ) {
		return AssignNative( new wxButton(parent, wxID_ANY, Property("Caption").GetString(), GetPosition(), GetSize() ) );
	}
	virtual void OnPropertyChanged( const wxString &id, wxUIProperty *p )
	{
		if ( wxButton *b = GetNative<wxButton>() )
			if ( id == "Caption" ) b->SetLabel( p->GetString() );
	}
	virtual void Draw( wxWindow *win, wxDC &dc, const wxRect &geom )
	{
		wxRendererNative::Get().DrawPushButton( win, dc, geom );
		dc.SetFont( *wxNORMAL_FONT );
		dc.SetTextForeground( *wxBLACK );
		wxString label = Property("Caption").GetString();
		int x, y;
		dc.GetTextExtent( label, &x, &y );
		dc.DrawText( label, geom.x + geom.width/2-x/2, geom.y+geom.height/2-y/2 );
	}
};

class wxUICheckBoxObject : public wxUIObject
{
public:
	wxUICheckBoxObject() {
		AddProperty( "Caption", new wxUIProperty( "CheckBox" ) );
		AddProperty( "State", new wxUIProperty( true ) );
		AddProperty( "TabOrder", new wxUIProperty( -1 ) );
	}
	virtual wxString GetTypeName() { return "CheckBox"; }
	virtual wxUIObject *Duplicate() { wxUIObject *c = new wxUICheckBoxObject; c->Copy( this ); return c; }
	virtual bool IsNativeObject() { return true; }
	virtual bool DrawDottedOutline() { return true; }
	virtual wxWindow *CreateNative( wxWindow *parent ) {
		wxCheckBox *cb = new wxCheckBox(parent, wxID_ANY, Property("Caption").GetString(), GetPosition(), GetSize() );
		cb->SetValue( Property("State").GetBoolean() );
		return AssignNative( cb );
	}
	virtual void OnPropertyChanged( const wxString &id, wxUIProperty *p )
	{
		if ( wxCheckBox *c = GetNative<wxCheckBox>() )
		{
			if ( id == "Caption" ) c->SetLabel( p->GetString() );
			else if ( id == "State" ) c->SetValue( p->GetBoolean() );
		}
	}
	virtual void Draw( wxWindow *win, wxDC &dc, const wxRect &geom )
	{
		int flags = Property("State").GetBoolean() ? wxCONTROL_CHECKED : 0;
		wxSize size = wxRendererNative::Get().GetCheckBoxSize( win );
		wxRendererNative::Get().DrawCheckBox( win, dc, wxRect( geom.x, geom.y+geom.height/2-size.y/2, size.x, size.y), flags );
		wxString label = Property("Caption").GetString();
		int x, y;
		dc.SetFont( *wxNORMAL_FONT );
		dc.SetTextForeground( *wxBLACK );
		dc.GetTextExtent( label, &x, &y );
		dc.DrawText( label, geom.x + size.x + 2, geom.y+geom.height/2-y/2 );
	}
	virtual void OnNativeEvent()
	{
		if ( wxCheckBox *c = GetNative<wxCheckBox>() )
			Property("State").Set( c->GetValue() );
	}
};

class wxUIChoiceObject : public wxUIObject
{
public:
	wxUIChoiceObject() {
		AddProperty( "Items", new wxUIProperty( wxArrayString() ) );
		AddProperty( "Selection", new wxUIProperty(-1) );
		AddProperty( "TabOrder", new wxUIProperty( -1 ) );
	}
	virtual wxString GetTypeName() { return "Choice"; }
	virtual wxUIObject *Duplicate() { wxUIObject *c = new wxUIChoiceObject; c->Copy( this ); return c; }
	virtual bool IsNativeObject() { return true; }
	virtual wxWindow *CreateNative( wxWindow *parent ) {
		wxArrayString items = Property("Items").GetStringList();
		wxChoice *cbo = new wxChoice( parent, wxID_ANY, GetPosition(), GetSize(), items );
		int sel = Property("Selection").GetInteger();
		if ( sel >= 0 && sel < items.Count() ) cbo->SetSelection( sel );
		return AssignNative( cbo );
	}
	virtual void OnPropertyChanged( const wxString &id, wxUIProperty *p )
	{
		if ( wxChoice *cbo = GetNative<wxChoice>() )
		{
			if ( id == "Selection" && p->GetInteger() >= 0 && p->GetInteger() < cbo->GetCount() )
				cbo->SetSelection( p->GetInteger() );
			else if ( id == "Items" )
			{
				int sel = cbo->GetSelection();
				cbo->Clear();
				cbo->Append( p->GetStringList() );
				if( sel >= 0 && sel < cbo->GetCount() ) cbo->SetSelection( sel );
			}
		}
	}
	virtual void Draw( wxWindow *win, wxDC &dc, const wxRect &geom )
	{
		wxRendererNative::Get().DrawComboBox( win, dc, geom );
		int sel = Property("Selection").GetInteger();
		wxArrayString items = Property("Items").GetStringList();
		if ( sel >= 0 && sel < items.Count() )
		{
			dc.SetFont( *wxNORMAL_FONT );
			dc.SetTextForeground( *wxBLACK );
			int x,y;
			dc.GetTextExtent( items[sel], &x, &y );
			dc.DrawText( items[sel], geom.x+2, geom.y + geom.height/2-y/2 );
		}
	}
	virtual void OnNativeEvent()
	{
		if ( wxComboBox *cbo = GetNative<wxComboBox>() )
			Property("Selection").Set( cbo->GetSelection() );
	}
};

class wxUIGroupBoxObject : public wxUIObject
{
public:
	wxUIGroupBoxObject() {
		AddProperty( "Caption", new wxUIProperty("Group box") );
		AddProperty( "Bold", new wxUIProperty( true ) );
		Property("Width").Set(360);
		Property("Height").Set(180);
	}
	virtual wxString GetTypeName() { return "GroupBox"; }
	virtual wxUIObject *Duplicate() { wxUIObject *gb = new wxUIGroupBoxObject; gb->Copy( this ); return gb; }
	virtual bool IsNativeObject() { return false; }
	virtual bool IsWithin( int x, int y )
	{
		wxRect geom = GetGeometry();
		if (x >= geom.x+4 && x < geom.x+geom.width-4 &&
				y >= geom.y+15 && y < geom.y+geom.height-4 ) return false;
		else return wxUIObject::IsWithin( x, y );
	}
	virtual void Draw( wxWindow *win, wxDC &dc, const wxRect &geom )
	{
		wxString caption = Property("Caption").GetString();
				
		int x=0,y=7;

		if ( !caption.IsEmpty() )
		{
			wxFont font( *wxNORMAL_FONT );
			if ( Property("Bold").GetBoolean() ) font.SetWeight( wxFONTWEIGHT_BOLD );
			dc.SetFont( font );
			dc.GetTextExtent( caption, &x, &y );
		}

		dc.SetBrush( *wxTRANSPARENT_BRUSH );
		dc.SetPen( wxPen( wxColour(135,135,135) ) );
		dc.DrawRectangle( geom.x, geom.y+y/2, geom.width, geom.height-y/2-1 );
		
		if ( !caption.IsEmpty() )
		{
			dc.SetBrush( wxBrush( win->GetBackgroundColour() ) );
			dc.SetPen( *wxTRANSPARENT_PEN );
			dc.DrawRectangle(geom.x+5, geom.y, x+2, y+1);			
			dc.SetTextForeground(wxSystemSettings::GetColour(wxSYS_COLOUR_BTNTEXT));
			dc.DrawText( caption , geom.x+6, geom.y);
		}
	}
};

class wxUIListBoxObject : public wxUIObject
{
public:
	wxUIListBoxObject() {
		AddProperty( "Items", new wxUIProperty( wxArrayString() ) );
		AddProperty( "Selection", new wxUIProperty(-1) );
		AddProperty( "TabOrder", new wxUIProperty( -1 ) );
		Property("Width").Set( 180 );
		Property("Height").Set( 90 );
	}
	virtual wxString GetTypeName() { return "ListBox"; }
	virtual wxUIObject *Duplicate() { wxUIObject *c = new wxUIListBoxObject; c->Copy( this ); return c; }
	virtual bool IsNativeObject() { return true; }
	virtual wxWindow *CreateNative( wxWindow *parent ) {
		wxArrayString items = Property("Items").GetStringList();
		wxListBox *list = new wxListBox( parent, wxID_ANY, GetPosition(), GetSize(), items, wxLB_SINGLE|wxLB_ALWAYS_SB );
		int sel = Property("Selection").GetInteger();
		if ( sel >= 0 && sel < items.Count() ) list->SetSelection( sel );
		return AssignNative( list );
	}
	virtual void OnPropertyChanged( const wxString &id, wxUIProperty *p )
	{
		if ( wxListBox *cbo = GetNative<wxListBox>() )
		{
			if ( id == "Selection" && p->GetInteger() >= 0 && p->GetInteger() < cbo->GetCount() )
				cbo->SetSelection( p->GetInteger() );
			else if ( id == "Items" )
			{
				int sel = cbo->GetSelection();
				cbo->Clear();
				cbo->Append( p->GetStringList() );
				if( sel >= 0 && sel < cbo->GetCount() ) cbo->SetSelection( sel );
			}
		}
	}
	virtual void Draw( wxWindow *win, wxDC &dc, const wxRect &geom )
	{
		dc.SetBrush( *wxWHITE_BRUSH );
		dc.SetPen( wxPen( wxColour(135,135,135) ) );
		dc.DrawRectangle( geom );
		int sel = Property("Selection").GetInteger();
		wxArrayString items = Property("Items").GetStringList();
		dc.SetFont( *wxNORMAL_FONT );
		dc.SetTextForeground( *wxBLACK );
		int y=geom.y+2;
		
		dc.SetBrush( *wxLIGHT_GREY_BRUSH );
		dc.SetPen( *wxTRANSPARENT_PEN );
		for (size_t i=0;i<items.Count() && y < geom.y+geom.height;i++)
		{
			if ( i==sel ) dc.DrawRectangle( geom.x+1, y-1, geom.width-2, dc.GetCharHeight()+2 );

			dc.DrawText( items[i], geom.x+3, y );
			y += dc.GetCharHeight() + 2;
		}
		dc.SetBrush( wxBrush(wxColour(235,235,235) ) );		
		dc.DrawRectangle( geom.x+geom.width-10, geom.y+1, 9, geom.height-2 );
	}
	virtual void OnNativeEvent()
	{
		if ( wxListBox *cbo = GetNative<wxListBox>() )
			Property("Selection").Set( cbo->GetSelection() );
	}
};

class wxUICheckListBoxObject : public wxUIObject
{
public:
	wxUICheckListBoxObject() {
		AddProperty( "Items", new wxUIProperty( wxArrayString() ) );
		AddProperty( "Checked", new wxUIProperty( "" ) );
		AddProperty( "TabOrder", new wxUIProperty( -1 ) );
		Property("Width").Set( 180 );
		Property("Height").Set( 90 );
	}
	virtual wxString GetTypeName() { return "CheckListBox"; }
	virtual wxUIObject *Duplicate() { wxUIObject *o = new wxUICheckListBoxObject; o->Copy(this); return o; }
	virtual bool IsNativeObject() { return true; }
	void UpdateNativeChecks( wxCheckListBox *clb ) {
		for ( size_t i=0;i<clb->GetCount();i++ ) clb->Check( i, false );
		wxArrayString checked = wxStringTokenize( Property("Checked").GetString(), "," );
		for ( size_t i=0;i<checked.size();i++ )	{
			int idx = atoi( checked[i].c_str() );
			if ( idx >= 0 && idx < clb->GetCount() ) clb->Check( idx );
		}
	}
	virtual wxWindow *CreateNative( wxWindow *parent ) {
		wxArrayString items = Property("Items").GetStringList();
		wxCheckListBox *clb = new wxCheckListBox( parent, wxID_ANY, GetPosition(), GetSize(), items );
		UpdateNativeChecks( clb );
		return AssignNative( clb );
	}
	virtual void OnPropertyChanged( const wxString &id, wxUIProperty *p )	{
		if ( wxCheckListBox *clb = GetNative<wxCheckListBox>() ) {
			if ( id == "Items" ) {
				int sel = clb->GetSelection();
				clb->Clear();
				clb->Append( p->GetStringList() );
			}

			// update checks when either 'Checked' or 'Items' property changes
			UpdateNativeChecks( clb );
		}
	}
	virtual void Draw( wxWindow *win, wxDC &dc, const wxRect &geom ) {
		dc.SetBrush( *wxWHITE_BRUSH );
		dc.SetPen( wxPen( wxColour(135,135,135) ) );
		dc.DrawRectangle( geom );
		wxArrayString checked = wxStringTokenize( Property("Checked").GetString(), "," );
		wxArrayString items = Property("Items").GetStringList();
		dc.SetFont( *wxNORMAL_FONT );
		dc.SetTextForeground( *wxBLACK );
		int y=geom.y+2;
		int itemheight = dc.GetCharHeight() + 2;		
		dc.SetBrush( *wxLIGHT_GREY_BRUSH );
		dc.SetPen( *wxTRANSPARENT_PEN );
		wxSize chksize = wxRendererNative::Get().GetCheckBoxSize( win );
		for (size_t i=0;i<items.Count() && y < geom.y+geom.height;i++)
		{
			int flags = 0;
			if ( checked.Index( wxString::Format("%d",i) ) != wxNOT_FOUND ) flags = wxCONTROL_CHECKED;
			wxRendererNative::Get().DrawCheckBox( win, dc, wxRect( geom.x+1, y + itemheight/2-chksize.y/2, chksize.x, chksize.y ), flags );
			dc.DrawText( items[i], geom.x+chksize.x+3, y );
			y += itemheight;
		}
		dc.SetBrush( wxBrush(wxColour(235,235,235) ) );
		dc.DrawRectangle( geom.x+geom.width-10, geom.y+1, 9, geom.height-2 );
	}
	virtual void OnNativeEvent() {
		if ( wxCheckListBox *clb = GetNative<wxCheckListBox>() ) {
			wxArrayString checked;
			for( size_t i=0;i<clb->GetCount();i++)
				if ( clb->IsChecked( i ) )
					checked.Add( wxString::Format("%d", i ) );
			Property("Checked").Set( wxJoin( checked, ',' ) );
		}
	}
};

class wxUILabelObject : public wxUIObject
{
public:
	wxUILabelObject() {
		AddProperty( "Caption", new wxUIProperty( "" ) );
		AddProperty( "TextColour", new wxUIProperty( *wxBLACK ) );
		AddProperty( "Bold", new wxUIProperty( false ) );
		AddProperty( "FontSize", new wxUIProperty( 0 ) );
		AddProperty( "WordWrap", new wxUIProperty( false ) );
		AddProperty( "AlignRight", new wxUIProperty( false ) );
		AddProperty( "AlignTop", new wxUIProperty( false ) );
	}
	virtual wxString GetTypeName() { return "Label"; }
	virtual wxUIObject *Duplicate() { wxUIObject *o = new wxUILabelObject; o->Copy(this); return o; }
	virtual bool IsNativeObject() { return false; }
	virtual bool DrawDottedOutline() { return true; }
	virtual void Draw( wxWindow *win, wxDC &dc, const wxRect &r ) {
		if ( IsVisible() )
		{
			wxLabelDraw( dc, r, *wxNORMAL_FONT, Property("Caption").GetString(),
				Property("TextColour").GetColour(), Property("AlignTop").GetBoolean(),
				Property("AlignRight").GetBoolean(), Property("Bold").GetBoolean(),
				Property("WordWrap").GetBoolean(), Property("FontSize").GetInteger() );
		}
	}
};

class wxUIRadioChoiceObject : public wxUIObject
{
public:
	wxUIRadioChoiceObject() {
		AddProperty( "Selection", new wxUIProperty( 0 ) );
		AddProperty( "Items", new wxUIProperty( wxArrayString() ) );
		AddProperty( "ShowCaptions", new wxUIProperty( true ) );
		AddProperty( "Horizontal", new wxUIProperty( false ) );
		AddProperty( "TabOrder", new wxUIProperty( -1 ) );
		Property("Width").Set( 180 );
		Property("Height").Set( 90 );
	}
	virtual wxString GetTypeName() { return "RadioChoice"; }
	virtual wxUIObject *Duplicate() { wxUIObject *o = new wxUIRadioChoiceObject; o->Copy(this); return o; }
	virtual bool IsNativeObject() { return true; }
	virtual bool DrawDottedOutline() { return true; }
	
	virtual wxWindow *CreateNative( wxWindow *parent ) {
		wxArrayString items = Property("Items").GetStringList();
		wxRadioChoice *rc = new wxRadioChoice( parent, wxID_ANY, GetPosition(), GetSize() );
		rc->Add( items );
		int sel = Property("Selection").GetInteger();
		if ( sel >= 0 && sel < items.Count() ) rc->SetSelection( sel );
		rc->ShowCaptions( Property("ShowCaptions").GetBoolean() );
		rc->SetHorizontal( Property("Horizontal").GetBoolean() );
		return AssignNative( rc );
	}
	virtual void OnPropertyChanged( const wxString &id, wxUIProperty *p )
	{
		if ( wxRadioChoice *rc = GetNative<wxRadioChoice>() )
		{
			if ( id == "Selection" && p->GetInteger() >= 0 && p->GetInteger() < rc->GetCount() )
				rc->SetSelection( p->GetInteger() );
			else if ( id == "Items" )
			{
				int sel = rc->GetSelection();
				rc->Clear();
				rc->Add( p->GetStringList() );
				if( sel >= 0 && sel < rc->GetCount() ) rc->SetSelection( sel );
			}
			else if ( id == "ShowCaptions" )
				rc->ShowCaptions( p->GetBoolean() );
			else if ( id == "Horizontal" )
				rc->SetHorizontal( p->GetBoolean() );
		}
	}
	virtual void Draw( wxWindow *win, wxDC &dc, const wxRect &geom )
	{
		wxArrayString items = Property("Items").GetStringList();
		if ( items.size() == 0 ) return;
		int sel = Property("Selection").GetInteger();
		bool showCaps = Property("ShowCaptions").GetBoolean();
		dc.SetFont( *wxNORMAL_FONT );
		dc.SetTextForeground( *wxBLACK );
		wxSize optsize = wxRendererNative::Get().GetCheckBoxSize( win );
		if ( Property("Horizontal").GetBoolean() == false )
		{
			int y=geom.y+2;		
			int itemheight = dc.GetCharHeight() + 2;
			if ( itemheight < 21 ) itemheight = 21;
			for (size_t i=0;i<items.Count() && y < geom.y+geom.height;i++)
			{
				int flags = 0;
				if ( i==sel ) flags = wxCONTROL_CHECKED;
				wxRendererNative::Get().DrawRadioBitmap( win, dc, wxRect( geom.x+1, y + itemheight/2-optsize.y/2, optsize.x, optsize.y ), flags );
				if ( showCaps ) dc.DrawText( items[i], geom.x+optsize.GetWidth()+3, y + itemheight/2-dc.GetCharHeight()/2 );
				y += itemheight;
			}
		}
		else
		{
			int itemwidth = geom.width/items.size();
			for( size_t i=0;i<items.size();i++ )
			{
				int flags = 0;
				if ( i==sel ) flags = wxCONTROL_CHECKED;
				wxRendererNative::Get().DrawRadioBitmap( win, dc, wxRect( geom.x+i*itemwidth, geom.y + geom.height/2-optsize.y/2, optsize.x, optsize.y ), flags );
				if ( showCaps ) dc.DrawText( items[i], geom.x+i*itemwidth + optsize.x + 1, geom.y+geom.height/2-dc.GetCharHeight()/2 );
			}
		}
	}
	virtual void OnNativeEvent()
	{
		if ( wxRadioChoice *cbo = GetNative<wxRadioChoice>() )
			Property("Selection").Set( cbo->GetSelection() );
	}
};

class wxUITextEntryObject : public wxUIObject
{
public:
	wxUITextEntryObject() {
		AddProperty( "Text", new wxUIProperty( "" ) );
		AddProperty( "Editable", new wxUIProperty( true ) );
		AddProperty( "ForeColour", new wxUIProperty( *wxBLACK ) );
		AddProperty( "BackColour", new wxUIProperty( *wxWHITE ) );
		AddProperty( "TabOrder", new wxUIProperty( -1 ) );
	}
	virtual wxString GetTypeName() { return "TextEntry"; }
	virtual wxUIObject *Duplicate() { wxUIObject *o = new wxUITextEntryObject; o->Copy(this); return o; }
	virtual bool IsNativeObject() { return true; }
	virtual wxWindow *CreateNative( wxWindow *parent ) {
		wxExtTextCtrl *txt = new wxExtTextCtrl( parent, wxID_ANY, Property("text").GetString(), GetPosition(), GetSize() );
		txt->SetForegroundColour( Property("ForeColour").GetColour() );
		txt->SetBackgroundColour( Property("BackColour").GetColour() );
		txt->SetEditable( Property("Editable").GetBoolean() );
		return AssignNative( txt );
	}
	virtual void OnPropertyChanged( const wxString &id, wxUIProperty *p )
	{
		if ( wxTextCtrl *txt = GetNative<wxTextCtrl>() )
		{
			if ( id == "Text" ) txt->ChangeValue( p->GetString() );
			else if ( id == "ForeColour" ) txt->SetForegroundColour( p->GetColour() );
			else if ( id == "BackColour" ) txt->SetBackgroundColour( p->GetColour() );
			else if ( id == "Editable" ) txt->SetEditable( p->GetBoolean() );
		}
	}
	virtual void Draw( wxWindow *win, wxDC &dc, const wxRect &geom )
	{
		dc.SetPen( *wxLIGHT_GREY_PEN );
		dc.SetBrush( wxBrush( Property("BackColour").GetColour() ) );
		dc.DrawRectangle( geom );
		dc.SetFont( *wxNORMAL_FONT );
		dc.SetTextForeground( Property("ForeColour").GetColour() );
		wxString text = Property("Text").GetString();
		int x, y;
		dc.GetTextExtent( text, &x, &y );
		dc.DrawText( text, geom.x+2, geom.y+geom.height/2-y/2 );
	}
	virtual void OnNativeEvent()
	{
		if ( wxTextCtrl *txt = GetNative<wxTextCtrl>() )
			Property("Text").Set( txt->GetValue() );
	}
};

class wxUIMultilineTextObject : public wxUITextEntryObject
{
public:
	wxUIMultilineTextObject() { }
	virtual wxString GetTypeName() { return "MultilineText"; }
	virtual wxUIObject *Duplicate() { wxUIObject *o = new wxUIMultilineTextObject; o->Copy(this); return o; }
	virtual wxWindow *CreateNative( wxWindow *parent ) {
		wxExtTextCtrl *txt = new wxExtTextCtrl(parent, wxID_ANY, Property("text").GetString(), GetPosition(), GetSize(), wxTE_MULTILINE);
		txt->SetForegroundColour( Property("ForeColour").GetColour() );
		txt->SetBackgroundColour( Property("BackColour").GetColour() );
		txt->SetEditable( Property("Editable").GetBoolean() );
		return AssignNative( txt );
	}
	virtual void Draw( wxWindow *win, wxDC &dc, const wxRect &geom )
	{
		dc.SetPen( *wxLIGHT_GREY_PEN );
		dc.SetBrush( wxBrush( Property("BackColour").GetColour() ) );
		dc.DrawRectangle( geom );
		dc.SetTextForeground( Property("ForeColour").GetColour() );
		dc.SetFont( *wxNORMAL_FONT );
		wxString text = Property("Text").GetString();
		wxDrawWordWrappedText( dc, text, geom.width-10,true,geom.x+2, geom.y+2 );
		dc.SetPen( *wxTRANSPARENT_PEN );
		dc.SetBrush( wxBrush(wxColour(235,235,235) ) );
		dc.DrawRectangle( geom.x+geom.width-10, geom.y+1, 9, geom.height-2 );
	}
};

class wxUINumericObject : public wxUIObject
{
public:
	wxUINumericObject() {
		AddProperty( "Value", new wxUIProperty( 0.0 ) );
		AddProperty( "Mode", new wxUIProperty( 1, "Integer,Real" ) );
		AddProperty( "Format", new wxUIProperty( 0, "Fixed,Generic,Exponential,Hexadecimal" ) );
		AddProperty( "Decimals", new wxUIProperty( 3 ) );
		AddProperty( "Prefix", new wxUIProperty( "" ) );
		AddProperty( "Suffix", new wxUIProperty( "" ) );
		AddProperty( "ThousandsSep", new wxUIProperty( true ) );
		AddProperty( "Editable", new wxUIProperty( true ) );
		AddProperty( "ForeColour", new wxUIProperty( *wxBLACK ) );
		AddProperty( "BackColour", new wxUIProperty( *wxWHITE ) );
		AddProperty( "TabOrder", new wxUIProperty( -1 ) );
	}
	virtual wxString GetTypeName() { return "Numeric"; }
	virtual wxUIObject *Duplicate() { wxUIObject *o = new wxUINumericObject; o->Copy(this); return o; }
	virtual bool IsNativeObject() { return true; }
	int GetDecimals()
	{	
		int deci = Property("Decimals").GetInteger();
		int pf = Property("Format").GetInteger();
		
		if( pf == 1 ) deci = wxNUMERIC_GENERIC;
		else if( pf == 2 ) deci = wxNUMERIC_EXPONENTIAL;
		else if( pf == 3 ) deci = wxNUMERIC_HEXADECIMAL;

		return deci;
	}
	void UpdateFormat( wxNumericCtrl *num )
	{	
		bool tsep = Property("ThousandsSep").GetBoolean();
		num->SetFormat( GetDecimals(), tsep, 
			Property("Prefix").GetString(), Property("Suffix").GetString() );
	}
	virtual wxWindow *CreateNative( wxWindow *parent ) {		
		wxNumericCtrl *num = new wxNumericCtrl( parent, wxID_ANY, Property("Value").GetDouble(), 
			Property("Mode").GetInteger() == 0 ? wxNUMERIC_INTEGER : wxNUMERIC_REAL,
			GetPosition(), GetSize() );
		UpdateFormat( num );
		num->SetForegroundColour( Property("ForeColour").GetColour() );
		num->SetBackgroundColour( Property("BackColour").GetColour() );
		num->SetEditable( Property("Editable").GetBoolean() );
		return AssignNative( num );
	}
	virtual void OnPropertyChanged( const wxString &id, wxUIProperty *p )
	{
		if ( wxNumericCtrl *num = GetNative<wxNumericCtrl>() )
		{
			if ( id == "Value" ) num->SetValue( p->GetDouble() );
			else if ( id == "Mode" ) num->SetMode( p->GetInteger()==0 ? wxNUMERIC_INTEGER : wxNUMERIC_REAL );
			else if ( id == "Format" || id == "Decimals" || id == "Prefix" || id == "Suffix" || id == "ThousandsSep" ) UpdateFormat( num );
			else if ( id == "ForeColour" ) num->SetForegroundColour( p->GetColour() );
			else if ( id == "BackColour" ) num->SetBackgroundColour( p->GetColour() );
			else if ( id == "Editable" ) num->SetEditable( p->GetBoolean() );
		}
	}
	virtual void Draw( wxWindow *win, wxDC &dc, const wxRect &geom )
	{
		dc.SetPen( *wxLIGHT_GREY_PEN );
		dc.SetBrush( wxBrush( Property("BackColour").GetColour() ) );
		dc.DrawRectangle( geom );
		dc.SetFont( *wxNORMAL_FONT );
		dc.SetTextForeground( Property("ForeColour").GetColour() );
		wxString text = wxNumericFormat( Property("Value").GetDouble(),
			Property("Mode").GetInteger() == 0 ? wxNUMERIC_INTEGER : wxNUMERIC_REAL,
			GetDecimals(), Property("ThousandsSep").GetBoolean(),
			Property("Prefix").GetString(), Property("Suffix").GetString() );

		int x, y;
		dc.GetTextExtent( text, &x, &y );
		dc.DrawText( text, geom.x+geom.width-2-x, geom.y+geom.height/2-y/2 );
	}
	virtual void OnNativeEvent()
	{
		if ( wxNumericCtrl *num = GetNative<wxNumericCtrl>() )
			Property("Value").Set( num->AsDouble() );
	}
};

class wxUIImageObject : public wxUIObject
{
public:
	wxUIImageObject() {
		AddProperty( "Image", new wxUIProperty( wxImage() ) );
		AddProperty( "Centered", new wxUIProperty( true ) );
	}
	virtual wxString GetTypeName() { return "Image"; }
	virtual wxUIObject *Duplicate() { wxUIObject *o = new wxUIImageObject; o->Copy(this); return o; }
	virtual bool IsNativeObject() { return false; }
	virtual bool DrawDottedOutline() { return true; }
	virtual void Draw( wxWindow *win, wxDC &dc, const wxRect &geom )
	{
		wxImage img = Property("Image").GetImage();
		if ( img.IsOk() )
		{
			wxBitmap bit( img );
			int x = geom.x;
			int y = geom.y;
			if ( Property("Centered").GetBoolean() )
			{
				x = geom.x + geom.width/2 - bit.GetWidth()/2;
				y = geom.y + geom.height/2 - bit.GetHeight()/2;
			}
			dc.DrawBitmap( bit, x, y );
		}
		else
		{
			dc.SetBrush( *wxWHITE_BRUSH );
			dc.SetPen( *wxRED_PEN );
			dc.DrawRectangle( geom );
			dc.SetTextForeground( *wxRED );
			dc.SetFont( *wxNORMAL_FONT );
			dc.DrawText( "Invalid image.", geom.x+2, geom.y+2 );
		}
	}
};

class wxUISliderObject : public wxUIObject
{
public:
	wxUISliderObject() {
		AddProperty( "Value", new wxUIProperty( 1 ) );
		AddProperty( "Min", new wxUIProperty( 0 ) );
		AddProperty( "Max", new wxUIProperty( 10 ) );
		AddProperty( "TabOrder", new wxUIProperty( -1 ) );
	}
	virtual wxString GetTypeName() { return "Slider"; }
	virtual wxUIObject *Duplicate() { wxUIObject *o = new wxUISliderObject; o->Copy(this); return o; }
	virtual bool IsNativeObject() { return true; }
	virtual bool DrawDottedOutline() { return true; }
	virtual wxWindow *CreateNative( wxWindow *parent ) {
		return AssignNative( new wxSlider( parent, wxID_ANY, Property("Value").GetInteger(), 
			Property("Min").GetInteger(), Property("Max").GetInteger()) );
	}
	virtual void OnPropertyChanged( const wxString &id, wxUIProperty *p )
	{
		if ( wxSlider *sli = GetNative<wxSlider>() )
		{
			if ( id == "Value" ) sli->SetValue( p->GetInteger() );
			else if ( id == "Min" ) sli->SetMin( p->GetInteger() );
			else if ( id == "Max" ) sli->SetMax( p->GetInteger() );
		}
	}
	virtual void Draw( wxWindow *win, wxDC &dc, const wxRect &geom )
	{
		dc.SetBrush(wxBrush(wxSystemSettings::GetColour(wxSYS_COLOUR_BTNFACE)));
		dc.SetPen(wxPen(wxSystemSettings::GetColour(wxSYS_COLOUR_BTNFACE)));
		wxDrawEngravedPanel(dc, geom.x+1, geom.y+geom.height/2-2, geom.width-2, 4, true);
		int min = Property("Min").GetInteger();
		int max = Property("Max").GetInteger();
		int val = Property("Value").GetInteger();
		float percent = ((float)(val-min))/( (float)(max-min) );
		int xoff = 2;
		int barx = geom.x+xoff + (geom.width-xoff-xoff)*percent;
		wxDrawRaisedPanel(dc, barx-4, geom.y+2, 8, geom.height-4);
	}
	virtual void OnNativeEvent()
	{
		if ( wxSlider *sli = GetNative<wxSlider>() )
			Property("Value").Set( sli->GetValue() );
	}
};

class wxUIHyperlinkObject : public wxUIObject
{
public:
	wxUIHyperlinkObject() {
		AddProperty( "Caption", new wxUIProperty("<url>") );
		AddProperty( "URL", new wxUIProperty("<url>") );
		AddProperty( "TabOrder", new wxUIProperty( -1 ) );
	}
	virtual wxString GetTypeName() { return "Hyperlink"; }
	virtual wxUIObject *Duplicate() { wxUIObject *o = new wxUIHyperlinkObject; o->Copy( this ); return o; }
	virtual bool IsNativeObject() { return true; }
	virtual bool DrawDottedOutline() { return true; }
	virtual wxWindow *CreateNative( wxWindow *parent ) {
		return AssignNative( new wxHyperlinkCtrl( parent, wxID_ANY, Property("Caption").GetString(),
			Property("URL").GetString() ) );
	}
	virtual void OnPropertyChanged( const wxString &id, wxUIProperty *p )
	{
		if ( wxHyperlinkCtrl *hy = GetNative<wxHyperlinkCtrl>() )
		{
			if ( id == "Caption" ) hy->SetLabel( p->GetString() );
			if ( id == "URL" ) hy->SetURL( p->GetString() );
		}
	}
	
	virtual void Draw( wxWindow *win, wxDC &dc, const wxRect &geom )
	{
		dc.SetFont(*wxNORMAL_FONT);
		dc.SetTextForeground(*wxBLUE);
		wxString label = Property("Caption").GetString();
		int sz_width, sz_height;
		dc.GetTextExtent(label, &sz_width, &sz_height );

		dc.DrawText(label,
			geom.x + 2,
			geom.y + (geom.height - dc.GetCharHeight()) / 2);
	}

	virtual void OnNativeEvent()
	{
		/* nothing to do here ... */
	}
};

class wxUIDiurnalPeriodObject : public wxUIObject
{
public:
	wxUIDiurnalPeriodObject() {
		AddProperty("TabOrder", new wxUIProperty((int)-1));
		AddProperty("Schedule", new wxUIProperty(wxString("")));
		AddProperty("Max", new wxUIProperty((int)9));
		AddProperty("Min", new wxUIProperty((int)1));

		Property("Width").Set( 514 );
		Property("Height").Set( 272 );
	}
	virtual wxString GetTypeName() { return "DiurnalPeriod"; }
	virtual wxUIObject *Duplicate() { wxUIObject *o = new wxUIDiurnalPeriodObject; o->Copy(this); return o; }
	virtual bool IsNativeObject() { return true; }
	virtual wxWindow *CreateNative(wxWindow *parent) {
		wxDiurnalPeriodCtrl *dp = new wxDiurnalPeriodCtrl(parent, wxID_ANY, wxDefaultPosition, wxDefaultSize);
		dp->SetupTOUGrid();
		dp->Schedule(Property("Schedule").GetString());
		dp->SetMinMax(Property("Min").GetInteger(),Property("Max").GetInteger());
		return AssignNative(dp);
	}
	virtual void OnPropertyChanged(const wxString &id, wxUIProperty *p)
	{
		if (wxDiurnalPeriodCtrl *dp = GetNative<wxDiurnalPeriodCtrl>())
		{
			if (id == "Schedule") dp->Schedule(p->GetString());
			if (id == "Min") dp->SetMin(p->GetInteger());
			if (id == "Max") dp->SetMax(p->GetInteger());
		}
	}
	virtual void Draw(wxWindow *win, wxDC &dc, const wxRect &geom)
	{
		dc.SetPen(*wxBLACK_PEN);
		dc.SetBrush(*wxLIGHT_GREY_BRUSH);
		dc.DrawRectangle(geom);
		dc.SetFont(*wxNORMAL_FONT);
		dc.SetTextForeground(*wxBLUE);
		dc.DrawText("Diurnal Period", geom.x + 2, geom.y + 2);
	}

};


wxUIProperty::wxUIProperty()
{
	Init();
}

wxUIProperty::wxUIProperty( const wxUIProperty &copy )
  : m_type( copy.m_type ),
	m_pReference( copy.m_pReference ),
	m_doubleVal( copy.m_doubleVal ),
	m_boolVal( copy.m_boolVal ),
	m_intVal( copy.m_intVal ),
	m_colour( copy.m_colour ),
	m_string( copy.m_string ),
	m_image( copy.m_image ),
	m_strList( copy.m_strList ),
	m_namedOptions( copy.m_namedOptions )
{
	// Do not copy over the update interface list
}

wxUIProperty::wxUIProperty( double dp )
{
	Init();
	m_type = DOUBLE;
	m_doubleVal = dp;
}

wxUIProperty::wxUIProperty( bool bp )
{
	Init();
	m_type = BOOLEAN;
	m_boolVal = bp;
}

wxUIProperty::wxUIProperty( int ip )
{
	Init();
	m_type = INTEGER;
	m_intVal = ip;
}

wxUIProperty::wxUIProperty( int i, const wxArrayString &named_options )
{
	Init();
	m_type = INTEGER,
	m_intVal = i;
	m_namedOptions = named_options;
}

wxUIProperty::wxUIProperty( int i, const wxString &commasep_options )
{
	Init();
	m_type = INTEGER,
	m_intVal = i;
	m_namedOptions = wxStringTokenize(commasep_options, ",");
}

wxUIProperty::wxUIProperty( const wxColour &cp )
{
	Init();
	m_type = COLOUR;
	m_colour = cp;
}

wxUIProperty::wxUIProperty( const char *str )
{
	Init();
	m_type = STRING;
	m_string = wxString(str);
}

wxUIProperty::wxUIProperty( const wxString &sp )
{
	Init();
	m_type = STRING;
	m_string = sp;
}

wxUIProperty::wxUIProperty( const wxImage &img )
{
	Init();
	m_type = IMAGE;
	m_image = img;
}

wxUIProperty::wxUIProperty( const wxArrayString &list )
{
	Init();
	m_type = STRINGLIST;
	m_strList = list;
}
int wxUIProperty::GetType()
{
	if (m_pReference != 0) return m_pReference->GetType();
	else return m_type;
}

void wxUIProperty::Set( double d )
{
	if ( m_pReference ) m_pReference->Set( d );	
	else { m_doubleVal = d; ValueChanged(); }
}

void wxUIProperty::Set( bool b )
{
	if (m_pReference ) m_pReference->Set( b );
	else { m_boolVal = b; ValueChanged(); }
}

void wxUIProperty::Set( int i )
{
	if ( m_pReference ) m_pReference->Set( i );
	else { m_intVal = i; ValueChanged(); }
}

void wxUIProperty::Set( const wxColour &c )
{
	if ( m_pReference ) m_pReference->Set( c );
	else { m_colour = c; ValueChanged(); }
}

void wxUIProperty::Set( const wxString &s )
{
	if ( m_pReference ) m_pReference->Set( s );
	else { m_string = s; ValueChanged(); }
}

void wxUIProperty::Set( const wxArrayString &list)
{
	if ( m_pReference ) m_pReference->Set( list );
	else { m_strList = list; ValueChanged(); }
}

void wxUIProperty::Set( const wxImage &img )
{
	if ( m_pReference ) m_pReference->Set( img );
	else { m_image = img; ValueChanged(); }
}

void wxUIProperty::SetNamedOptions( const wxArrayString &opts, int selection )
{
	if ( m_pReference ) m_pReference->SetNamedOptions( opts, selection );
	else
	{
		m_namedOptions = opts;
		if ( selection >= 0 && selection < (int)opts.Count() )
		{
			m_intVal = selection;
			ValueChanged();
		}
	}
}

wxArrayString wxUIProperty::GetNamedOptions()
{
	if ( m_pReference ) return m_pReference->GetNamedOptions();
	else return m_namedOptions;
}


int wxUIProperty::GetInteger()
{
	if ( m_pReference ) return m_pReference->GetInteger();
	else return m_intVal;
}

bool wxUIProperty::GetBoolean()
{
	if ( m_pReference ) return m_pReference->GetBoolean();
	else return m_boolVal;
}

double wxUIProperty::GetDouble()
{
	if ( m_pReference ) return m_pReference->GetDouble();
	else return m_doubleVal;
}

wxColour wxUIProperty::GetColour()
{
	if ( m_pReference ) return m_pReference->GetColour();
	else return m_colour;
}

wxString wxUIProperty::GetString()
{
	if ( m_pReference ) return m_pReference->GetString();
	else return m_string;
}

wxString wxUIProperty::AsString()
{
	int ty = GetType();
	switch( ty )
	{
	case DOUBLE: return wxString::Format("%lg", GetDouble() );
	case BOOLEAN: return GetBoolean() ? "true" : "false";
	case INTEGER:
		if ( m_namedOptions.size() > 0 ) return wxString::Format("%d", GetInteger() ) + ": " + wxJoin(m_namedOptions, ',');
		else return wxString::Format("%d", GetInteger() );
	case COLOUR: return wxString::Format("color[%d,%d,%d]", GetColour().Red(), GetColour().Green(), GetColour().Blue() );
	case STRING: return GetString();
	case STRINGLIST: return wxJoin(GetStringList(), ',');
	case IMAGE: return "<image>";
	default: return "<invalid>";
	}
}

wxArrayString wxUIProperty::GetStringList()
{
	if ( m_pReference ) return m_pReference->GetStringList();
	else return m_strList;
}

wxImage wxUIProperty::GetImage()
{
	if ( m_pReference ) return m_pReference->GetImage();
	else return m_image;
}

void wxUIProperty::Write( wxOutputStream &_o )
{
	wxDataOutputStream out( _o );
	int type = GetType();
	out.Write8( 0x1d );
	out.Write16( (wxUint16)type );
	switch( type )
	{
	case DOUBLE: out.WriteDouble( GetDouble() ); break;
	case BOOLEAN: out.Write8( GetBoolean() ? 1 : 0 ); break;
	case INTEGER: out.Write32( GetInteger() ); break;
	case STRING: out.WriteString( GetString() ); break;
	case COLOUR:
		{
			wxColour c = GetColour();
			out.Write8( c.Red() );
			out.Write8( c.Green() );
			out.Write8( c.Blue() );
			out.Write8( c.Alpha() );
		}
		break;
	case STRINGLIST:
		{
			wxArrayString list = GetStringList();
			out.Write32( list.Count() );
			for ( size_t i=0;i<list.Count(); i++ )
				out.WriteString( list[i] );
		}
		break;
	case IMAGE:
		{
			wxImage img = GetImage();
			wxPNGHandler().SaveFile( &img, _o, false );			
		}
		break;
	}
	
	out.Write8(0x1d);
}

bool wxUIProperty::Read( wxInputStream &_i )
{
	wxDataInputStream in(_i);

	wxUint8 code = in.Read8();
	wxUint16 type = in.Read16();

	if ( m_pReference )
		m_pReference->m_type = type;
	else
		m_type = type;

	wxUint8 r,g,b,a;
	switch( type )
	{
	case DOUBLE: Set(in.ReadDouble()); break;
	case BOOLEAN: Set( in.Read8() != 0 ? true : false ); break;
	case INTEGER: Set( (int)in.Read32()); break;
	case STRING: Set(in.ReadString()); break;
	case COLOUR: 
		r = in.Read8();
		g = in.Read8();
		b = in.Read8();
		a = in.Read8();
		Set( wxColour(r,g,b,a) );
		break;
	case STRINGLIST:
		{
			wxArrayString list;
			size_t count = in.Read32();
			for (size_t i=0;i<count;i++ )
				list.Add( in.ReadString() );
			Set( list );
		}
		break;
	case IMAGE:
		{
			wxImage img;
			wxPNGHandler().LoadFile( &img, _i, false );
			Set( img );
		}
		break;
	}

	return ( code == in.Read8() );
}

void wxUIProperty::ValueChanged()
{
	for( size_t i=0;i<m_updateInterfaceList.size(); i++ )
		m_updateInterfaceList[i].pui->OnPropertyChanged( m_updateInterfaceList[i].id, this );
}

void wxUIProperty::AddUpdateInterface( const wxString &id, wxUIPropertyUpdateInterface *pui )
{
	puidata x;
	x.pui = pui;
	x.id = id;
	m_updateInterfaceList.push_back( x );
}

void wxUIProperty::RemoveUpdateInterface( wxUIPropertyUpdateInterface *pui )
{
	size_t i=0;
	while ( i < m_updateInterfaceList.size() )
	{
		if ( m_updateInterfaceList[i].pui == pui )
			m_updateInterfaceList.erase( m_updateInterfaceList.begin() + i );
		else
			i++;
	}
}

void wxUIProperty::ClearUpdateInterfaces()
{
	m_updateInterfaceList.clear();
}

void wxUIProperty::Init()
{
	m_type = INVALID;
	m_pReference = 0;
	m_doubleVal = 0.0;
	m_intVal = 0;
	m_boolVal = false;
}

wxUIObject::wxUIObject( )
{
static int g_idCounter = 0;

	m_nativeObject = 0;
	m_visible = true;
	AddProperty( "Name", new wxUIProperty( wxString::Format("object %d", ++g_idCounter ) ) );
	AddProperty( "X", new wxUIProperty( (int) 9 ) );
	AddProperty( "Y", new wxUIProperty( (int) 9 ) );
	AddProperty( "Width", new wxUIProperty( (int) 90 ) );
	AddProperty( "Height", new wxUIProperty( (int) 24 ) );	
}

wxUIObject::~wxUIObject()
{
	DestroyNative();
	DeleteProperties();
}

void wxUIObject::DeleteProperties()
{
	for( size_t i=0;i<m_properties.size();i++ )
		delete m_properties[i].prop;
	m_properties.clear();
}

wxWindow *wxUIObject::AssignNative( wxWindow *win )
{
	if ( m_nativeObject != win )
		DestroyNative();
	m_nativeObject = win;
	if ( win )
		SetGeometry( GetGeometry() ); // updates native geometry with proper scale

	return m_nativeObject;
}

void wxUIObject::OnNativeEvent()
{
	// nothing to do here... override in descendants
}

void wxUIObject::DestroyNative()
{
	if ( m_nativeObject != 0 )
	{
		m_nativeObject->Destroy();
		m_nativeObject = 0;
	}
}

bool wxUIObject::IsWithin( int xx, int yy )
{
	int x = Property("X").GetInteger();
	int y = Property("Y").GetInteger();
	int width = Property("Width").GetInteger();
	int height = Property("Height").GetInteger();

	return ( xx >= x && xx <= x+width
		&& yy >= y && yy < y+height) ;
}

void wxUIObject::Draw( wxWindow *win, wxDC &dc, const wxRect &geom )
{
	dc.SetPen( *wxBLACK_PEN );
	dc.SetBrush( *wxLIGHT_GREY_BRUSH );
	dc.DrawRectangle( geom );
	
	dc.SetFont( *wxNORMAL_FONT );
	dc.DrawText( Property("Name").GetString() + " (" + GetTypeName() + ")", geom.x + 2, geom.y + 2 );
}

bool wxUIObject::Copy( wxUIObject *rhs )
{
	DeleteProperties();
	for( size_t i=0;i<rhs->m_properties.size();i++ )
		AddProperty( rhs->m_properties[i].name, new wxUIProperty( *(rhs->m_properties[i].prop) ) );

	return true;
}

void wxUIObject::Show( bool b )
{
	m_visible = b;
	if ( m_nativeObject ) m_nativeObject->Show( b );
}

void wxUIObject::SetName( const wxString &name )
{
	Property("Name").Set( name );

	if ( m_nativeObject != 0 ) m_nativeObject->SetName( name );
}

wxString wxUIObject::GetName()
{
	return Property("Name").GetString();
}

void wxUIObject::SetGeometry( const wxRect &r )
{
	Property("X").Set( r.x );
	Property("Y").Set( r.y );
	Property("Width").Set( r.width );
	Property("Height").Set( r.height );

	if ( m_nativeObject != 0 )
	{
		double xs, ys;
		wxDevicePPIToScale( wxClientDC(m_nativeObject).GetPPI(), &xs, &ys );
		m_nativeObject->SetSize( wxScaleRect(r, xs, ys) );
	}
}

wxRect wxUIObject::GetGeometry()
{
	return wxRect(
		Property("X").GetInteger(),
		Property("Y").GetInteger(),
		Property("Width").GetInteger(),
		Property("Height").GetInteger() );
}

wxPoint wxUIObject::GetPosition()
{
	wxRect r = GetGeometry();
	return wxPoint( r.x, r.y );
}

wxSize wxUIObject::GetSize()
{
	wxRect r = GetGeometry();
	return wxSize( r.width, r.height );
}

static wxUIProperty gs_nullProp;

wxUIProperty &wxUIObject::Property( const wxString &name )
{

	wxString lowered = name.Lower();
	for( size_t i=0;i<m_properties.size();i++ )
		if ( lowered == m_properties[i].lowered )
			return *m_properties[i].prop;

	return gs_nullProp;
}

bool wxUIObject::HasProperty( const wxString &name )
{
	wxString lowered = name.Lower();
	for( size_t i=0;i<m_properties.size();i++ )
		if ( lowered == m_properties[i].lowered )
			return true;

	return false;
}

int wxUIObject::GetTabOrder()
{
	wxUIProperty &p = Property("TabOrder");
	if( &p == &gs_nullProp ) return 99999;
	else return p.GetInteger();
}

wxArrayString wxUIObject::Properties()
{
	wxArrayString list;
	for( size_t i=0;i<m_properties.size();i++ )
		list.Add( m_properties[i].name );

	return list;
}

void wxUIObject::Write( wxOutputStream &_o )
{
	wxDataOutputStream out(_o);
	out.Write8( 0xaf ); // start code
	out.Write8( 1 ); // version

	out.Write8( m_visible ? 1 : 0 );

	out.Write32( m_properties.size() );
	for( size_t i=0;i<m_properties.size(); i++ )
	{
		out.WriteString( m_properties[i].name );
		m_properties[i].prop->Write( _o );
	}

	out.Write8( 0xaf );
}

bool wxUIObject::Read( wxInputStream &_i )
{
	wxDataInputStream in(_i);
	wxUint8 code = in.Read8();
	in.Read8(); // version

	m_visible = in.Read8() != 0;

	size_t n = in.Read32();
	for( size_t i=0;i<n;i++)
	{
		wxString name = in.ReadString();
		Property(name).Read( _i );
	}

	return in.Read8() == code;
}


void wxUIObject::AddProperty( const wxString &name, wxUIProperty *prop )
{
	prop->AddUpdateInterface( name, this );
	
	propdata x;
	x.name = name;
	x.lowered = name.Lower();
	x.prop = prop;
	m_properties.push_back( x );
}

void wxUIObject::OnPropertyChanged( const wxString &, wxUIProperty * )
{
	/* nothing to do here */
}



enum { ID_SE_Add = wxID_HIGHEST+445, ID_SE_Remove, ID_SE_Clear, ID_SE_ListBox, ID_SE_InsertBefore, ID_SE_InsertAfter };

class StringListDialog : public wxDialog
{
	DECLARE_EVENT_TABLE();
	wxListBox *m_list;
	wxButton *m_addButton;
public:
	StringListDialog( wxWindow *parent )
		: wxDialog(parent, wxID_ANY, "Edit strings",
			wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE )
	{
		m_list = new wxListBox( this, ID_SE_ListBox );

		wxBoxSizer *tools = new wxBoxSizer( wxVERTICAL );
		tools->Add( m_addButton=new wxButton( this, ID_SE_Add, "Add" ), 0, wxALL|wxEXPAND, 2 );
		tools->Add( new wxButton( this, ID_SE_InsertBefore, "Insert before" ), 0, wxALL|wxEXPAND, 2 );
		tools->Add( new wxButton( this, ID_SE_InsertAfter, "Insert after" ), 0, wxALL|wxEXPAND, 2 );
		tools->Add( new wxButton( this, ID_SE_Remove, "Remove" ), 0, wxALL|wxEXPAND, 2 );
		tools->Add( new wxButton( this, ID_SE_Clear, "Clear all" ), 0, wxALL|wxEXPAND, 2 );

		wxBoxSizer *hsize = new wxBoxSizer( wxHORIZONTAL );
		hsize->Add( m_list, 2, wxALL|wxEXPAND, 3 );
		hsize->Add( tools, 1, wxALL|wxEXPAND, 3 );

		wxBoxSizer *vsize = new wxBoxSizer( wxVERTICAL );
		vsize->Add( hsize, 1, wxALL|wxEXPAND, 3 );
		vsize->Add( CreateButtonSizer( wxOK|wxCANCEL ), 0, wxALL|wxEXPAND, 3 );

		SetSizerAndFit( vsize );
		m_addButton->SetFocus();
	}

	void OnListDClick(wxCommandEvent &evt)
	{
		int idx = evt.GetSelection();
		wxString str = m_list->GetString(idx);
	
		str = wxGetTextFromUser("Edit string", "Property", str, this);
		if (!str.IsEmpty())
			m_list->SetString(idx, str);
	}

	bool RunDialog(wxArrayString &arr)
	{
		unsigned int i;
		for (i=0;i<arr.Count();i++)
			m_list->Append(arr[i]);

		CenterOnParent();

		if ( ShowModal() == wxID_OK )
		{
			arr.Clear();
			for (i=0;i<(unsigned int)m_list->GetCount();i++)
				arr.Add(m_list->GetString(i));

			return true;
		}
		else
			return false;
	}

	void OnCommand(wxCommandEvent &evt)
	{
		wxString str;
		int id;
		switch(evt.GetId())
		{
		case ID_SE_Add:
			str = wxGetTextFromUser("Enter string value:", "Add String", "", this);
			if (str != wxEmptyString)
			{
				m_list->Append(str);
				m_addButton->SetFocus();
			}
			break;
		case ID_SE_InsertAfter:
		case ID_SE_InsertBefore:
			str = wxGetTextFromUser("Enter string value:", "Add String", "", this);
			if (str != wxEmptyString)
			{
				id = m_list->GetSelection();
				if (id >= 0 && id < (int)m_list->GetCount())
				{
					int idx = evt.GetId() == ID_SE_InsertAfter ? id+1 : id;
					m_list->InsertItems(1, &str, idx);
				}
			}
			break;
		case ID_SE_Remove:
			id = m_list->GetSelection();
			if (id >= 0 && id < (int) m_list->GetCount())
				m_list->Delete(id);
	
			break;
		case ID_SE_Clear:
			m_list->Clear();
			break;
		}
	}
};

BEGIN_EVENT_TABLE( StringListDialog, wxDialog )
	EVT_BUTTON(ID_SE_Add, StringListDialog::OnCommand)
	EVT_BUTTON(ID_SE_Remove, StringListDialog::OnCommand)
	EVT_BUTTON(ID_SE_InsertBefore, StringListDialog::OnCommand)
	EVT_BUTTON(ID_SE_InsertAfter, StringListDialog::OnCommand)
	EVT_BUTTON(ID_SE_Clear, StringListDialog::OnCommand)
	EVT_LISTBOX_DCLICK( ID_SE_ListBox, StringListDialog::OnListDClick)
END_EVENT_TABLE()



BEGIN_EVENT_TABLE( wxUIPropertyEditor, wxPanel )
	EVT_TEXT_ENTER( wxID_ANY, wxUIPropertyEditor::OnChange )
	EVT_TEXT( wxID_ANY, wxUIPropertyEditor::OnChange )
	EVT_CHECKBOX( wxID_ANY, wxUIPropertyEditor::OnChange )
	EVT_COMBOBOX( wxID_ANY, wxUIPropertyEditor::OnChange )
	EVT_COLOURPICKER_CHANGED( wxID_ANY, wxUIPropertyEditor::OnColourPicker )
	EVT_BUTTON( wxID_ANY, wxUIPropertyEditor::OnButton )
END_EVENT_TABLE();


wxUIPropertyEditor::wxUIPropertyEditor( wxWindow *parent, int id, const wxPoint &pos, const wxSize &size )
	: wxPanel( parent, id, pos, size, wxTAB_TRAVERSAL )
{
	m_curObject = 0;
	wxFlexGridSizer *sizer = new wxFlexGridSizer(2,0,0);
	sizer->AddGrowableCol(1);
	SetSizer( sizer );
}

wxUIPropertyEditor::~wxUIPropertyEditor()
{
	m_curObject = 0;
}

void wxUIPropertyEditor::SetObject( wxUIObject *obj )
{
	if ( obj == m_curObject ) return;

	// clear the property grid
	for ( size_t i=0;i<m_curProps.size();i++ )
	{
		m_curProps[i].label->Destroy();
		m_curProps[i].editor->Destroy();
	}

	GetSizer()->Layout();
	m_curProps.clear();
	
	m_curObject = obj;

	if ( m_curObject == 0 ) return;

	wxArrayString list = obj->Properties();
	for ( size_t i=0; i<list.Count();i++ )
	{
		wxUIProperty &p = obj->Property( list[i] );
	
		wxStaticText *label = new wxStaticText( this, wxID_ANY, list[i] );
		wxWindow *editor = 0;

		switch( p.GetType() )
		{
		case wxUIProperty::DOUBLE:
			editor = new wxNumericCtrl( this, wxID_ANY, 0.0, wxNUMERIC_REAL );
			break;
		case wxUIProperty::INTEGER:
			if ( p.GetNamedOptions().Count() > 0 )
				editor = new wxComboBox( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, p.GetNamedOptions(), wxCB_READONLY );
			else
				editor = new wxNumericCtrl( this, wxID_ANY, 0.0, wxNUMERIC_INTEGER );
			break;
		case wxUIProperty::STRING:
			editor = new wxTextCtrl( this, wxID_ANY, wxEmptyString );
			break;
		case wxUIProperty::BOOLEAN:
			editor = new wxCheckBox( this, wxID_ANY, wxEmptyString );
			break;
		case wxUIProperty::COLOUR:
			editor = new wxColourPickerCtrl( this, wxID_ANY );
			break;
		case wxUIProperty::STRINGLIST:
			editor = new wxButton( this, wxID_ANY, "Edit..." );
			break;
		case wxUIProperty::IMAGE:
			editor = new wxButton( this, wxID_ANY, "Select..." );
			break;
		}

		if ( editor != 0 )
		{
			pgpinfo x;
			x.name = list[i];
			x.type = p.GetType();
			x.label = label;
			x.editor = editor;
			m_curProps.push_back( x );

			GetSizer()->Add( x.label, 0, wxALL|wxALIGN_CENTRE_VERTICAL, 4 );
//			GetSizer()->Add(x.editor, 1, wxALL | wxALIGN_CENTER_VERTICAL | wxEXPAND, 2);
			GetSizer()->Add(x.editor, 1, wxALL | wxEXPAND, 2);
			ValueToPropGrid(x);
		}
	}

	GetSizer()->Layout();

}

wxUIPropertyEditor::pgpinfo *wxUIPropertyEditor::Find( wxObject *editor )
{
	for ( size_t i=0;i<m_curProps.size();i++ )
		if ( m_curProps[i].editor == editor )
			return &m_curProps[i];

	return 0;
}

void wxUIPropertyEditor::OnButton( wxCommandEvent &evt )
{
	pgpinfo *pi = Find( evt.GetEventObject() );
	if ( !pi ) return;

	if ( pi->type == wxUIProperty::IMAGE )
	{
		wxString file = wxFileSelector("Select image file", "", "", "", "PNG Files (*.png)|*.png|BMP Files (*.bmp)|*.bmp|All Files (*.*)|*.*");
		if ( !file.IsEmpty() )
		{
			wxImage image_data;
			if ( image_data.LoadFile( file ) )
			{
				m_curObject->Property( pi->name ).Set( image_data );				
			}
			else
				wxMessageBox("Could not load the selected image file:\n\n" + file);
		}
	}
	else if ( pi->type == wxUIProperty::STRINGLIST )
	{
		StringListDialog dlg(this);
		wxArrayString items = m_curObject->Property( pi->name ).GetStringList();
		if ( dlg.RunDialog( items ) )
			m_curObject->Property( pi->name ).Set( items );
	}
}

void wxUIPropertyEditor::OnChange( wxCommandEvent &evt )
{
	pgpinfo *pi = Find( evt.GetEventObject() );
	if ( !pi ) return;
	PropGridToValue( *pi );
}

void wxUIPropertyEditor::OnColourPicker( wxColourPickerEvent &evt )
{
	pgpinfo *pi = Find( evt.GetEventObject() );
	if ( !pi ) return;
	PropGridToValue( *pi );
}

void wxUIPropertyEditor::UpdatePropertyValues()
{
	for ( size_t i=0;i<m_curProps.size(); i++ )
		ValueToPropGrid( m_curProps[i] );
}

void wxUIPropertyEditor::ValueToPropGrid( pgpinfo &p )
{
	if ( !m_curObject ) return;

	wxUIProperty &vp = m_curObject->Property( p.name );

	switch( vp.GetType() )
	{
	case wxUIProperty::BOOLEAN:
		static_cast<wxCheckBox*>( p.editor )->SetValue( vp.GetBoolean() );
		break;
	case wxUIProperty::DOUBLE:
		static_cast<wxNumericCtrl*>( p.editor )->SetValue( vp.GetDouble() );
		break;
	case wxUIProperty::INTEGER:
		if ( vp.GetNamedOptions().Count() > 0 )
			static_cast<wxComboBox*>( p.editor )->SetSelection( vp.GetInteger() );
		else
			static_cast<wxNumericCtrl*>( p.editor )->SetValue( vp.GetInteger() );
		break;
	case wxUIProperty::STRING:
		static_cast<wxTextCtrl*>( p.editor )->ChangeValue( vp.GetString() );
		break;
	case wxUIProperty::COLOUR:
		static_cast<wxColourPickerCtrl*>( p.editor )->SetColour( vp.GetColour() );
		break;
	case wxUIProperty::STRINGLIST:
		// nothing to do here - button doesn't change
		break;
	case wxUIProperty::IMAGE:
		// nothing to do here - button doesn't change
		break;
	}
}

void wxUIPropertyEditor::PropGridToValue( pgpinfo &p )
{
	if ( !m_curObject ) return;
		
	wxUIProperty &vp = m_curObject->Property(p.name);
	
	if ( vp.GetType() == wxUIProperty::INVALID ) return;

	switch( vp.GetType() )
	{
	case wxUIProperty::BOOLEAN:
		vp.Set( static_cast<wxCheckBox*>( p.editor )->GetValue() );
		break;
	case wxUIProperty::DOUBLE:
		vp.Set( static_cast<wxNumericCtrl*>( p.editor )->AsDouble() );
		break;
	case wxUIProperty::INTEGER:
		if ( vp.GetNamedOptions().Count() > 0 )
			vp.Set( static_cast<wxComboBox*>( p.editor )->GetSelection() );
		else
			vp.Set( static_cast<wxNumericCtrl*>( p.editor )->AsInteger() );
		break;
	case wxUIProperty::STRING:
		vp.Set( static_cast<wxTextCtrl*>( p.editor )->GetValue() );
		break;
	case wxUIProperty::COLOUR:
		vp.Set( static_cast<wxColourPickerCtrl*>( p.editor )->GetColour() );
		break;
	case wxUIProperty::STRINGLIST:
		// nothing to do here - button doesn't change
		break;
	case wxUIProperty::IMAGE:
		// nothing to do here - button doesn't change
		break;
	}
}


static std::vector<wxUIObject*> g_registeredTypes;

void wxUIObjectTypeProvider::Register( wxUIObject *obj )
{
	for( size_t i=0;i<g_registeredTypes.size();i++ )
		if ( g_registeredTypes[i]->GetTypeName() == obj->GetTypeName() )
			return;

	g_registeredTypes.push_back( obj );
}

wxUIObject *wxUIObjectTypeProvider::Create( const wxString &type )
{
	for( size_t i=0;i<g_registeredTypes.size();i++ )
		if ( g_registeredTypes[i]->GetTypeName() == type )
			return g_registeredTypes[i]->Duplicate();

	return 0;
}

std::vector<wxUIObject*> wxUIObjectTypeProvider::GetTypes()
{
	return g_registeredTypes;
}

void wxUIObjectTypeProvider::RegisterBuiltinTypes()
{
	wxUIObjectTypeProvider::Register( new wxUIButtonObject );
	wxUIObjectTypeProvider::Register( new wxUICheckBoxObject );
	wxUIObjectTypeProvider::Register( new wxUIChoiceObject );
	wxUIObjectTypeProvider::Register( new wxUIGroupBoxObject );
	wxUIObjectTypeProvider::Register( new wxUIListBoxObject );
	wxUIObjectTypeProvider::Register( new wxUICheckListBoxObject );
	wxUIObjectTypeProvider::Register( new wxUILabelObject );
	wxUIObjectTypeProvider::Register( new wxUIRadioChoiceObject );
	wxUIObjectTypeProvider::Register( new wxUITextEntryObject );
	wxUIObjectTypeProvider::Register( new wxUIMultilineTextObject );
	wxUIObjectTypeProvider::Register( new wxUINumericObject );
	wxUIObjectTypeProvider::Register( new wxUIImageObject );
	wxUIObjectTypeProvider::Register( new wxUISliderObject );
	wxUIObjectTypeProvider::Register( new wxUIHyperlinkObject );
	wxUIObjectTypeProvider::Register( new wxUIDiurnalPeriodObject );
}

wxUIFormData::wxUIFormData()
{
	m_formWindow = 0;
	m_name = "untitled form";
	m_width = 500;
	m_height = 300;
}

wxUIFormData::wxUIFormData( const wxUIFormData &rhs )
{
	Copy( rhs );
}

wxUIFormData::~wxUIFormData()
{
	DeleteAll(); // this will detach from the form too
}


void wxUIFormData::Copy( const wxUIFormData &rhs )
{
	Detach();
	DeleteAll();

	m_name = rhs.m_name;
	m_width = rhs.m_width;
	m_height = rhs.m_height;
	for( size_t i=0;i<rhs.m_objects.size(); i++ )
		m_objects.push_back( rhs.m_objects[i]->Duplicate() );
}

wxUIFormData *wxUIFormData::Duplicate() const
{
	return new wxUIFormData( *this );
}


bool wxUIFormData::GetMetaData( const wxString &,
	wxString *, wxString *, wxColour * )
{
	return false;
}

// build/destroy native interface as needed
void wxUIFormData::Attach( wxWindow *form )
{
	Detach();

	m_formWindow = form;
	if ( m_formWindow )
	{
		// first sort by "TabOrder" property
		std::vector<wxUIObject*> list = m_objects;

		int len = (int)list.size();
		for( int i=0;i<len-1;i++ )
		{
			int smallest = i;

			for( int j=i+1;j<len;j++ )
				if ( list[j]->GetTabOrder() < list[smallest]->GetTabOrder() )
					smallest = j;

			// swap
			wxUIObject *ptr = list[i];
			list[i] = list[smallest];
			list[smallest] = ptr;
		}
		
		// then create the object
		for( size_t i=0;i<list.size();i++ )
			list[i]->CreateNative( m_formWindow );
	}

}

void wxUIFormData::Detach()
{
	if ( m_formWindow )
	{
		for( size_t i=0;i<m_objects.size();i++ )
			m_objects[i]->DestroyNative();

		m_formWindow = 0;
	}
}
	
// load/save form definition
void wxUIFormData::Write( wxOutputStream &_O )
{
	wxDataOutputStream out(_O);

	out.Write8( 0xd7 ); // code
	out.Write8( 1 ); // version

	out.WriteString( m_name );
	out.Write32( m_width );
	out.Write32( m_height );

	out.Write32( m_objects.size() );

	for ( size_t i=0;i<m_objects.size();i++ )
	{
		out.WriteString( m_objects[i]->GetTypeName() );
		m_objects[i]->Write( _O );
	}

	out.Write8( 0xd7 );
}

bool wxUIFormData::Read( wxInputStream &_I )
{
	DeleteAll();

	wxDataInputStream in(_I);

	wxUint8 code = in.Read8();
	in.Read8(); // version

	m_name = in.ReadString();
	m_width = in.Read32();
	m_height = in.Read32();

	bool ok = true;
	size_t n = in.Read32();
	for ( size_t i=0;i<n;i++ )
	{
		wxString type = in.ReadString();
		if (wxUIObject *obj = Create( type ))
			ok = ok && obj->Read( _I );
		else
			ok = false;
	}

	return ( in.Read8() == code && ok );
}

// methods to create/edit UI objects
wxUIObject *wxUIFormData::Create( const wxString &type )
{
	wxUIObject *obj = wxUIObjectTypeProvider::Create( type );
	if ( obj != 0 ) Add( obj );
	return obj;
}

wxUIObject *wxUIFormData::Create( const wxString &type, const wxRect &geom, const wxString &name )
{
	wxUIObject *obj = Create( type );
	if ( obj )
	{
		obj->SetGeometry( geom );
		if ( !name.IsEmpty() ) obj->SetName( name );
	}
	return obj;
}

void wxUIFormData::Add( wxUIObject *obj )
{
	if (std::find( m_objects.begin(), m_objects.end(), obj ) == m_objects.end() )
	{
		m_objects.push_back( obj );
		if ( m_formWindow != 0 )
			obj->CreateNative( m_formWindow );
	}
}

void wxUIFormData::Delete( wxUIObject *obj )
{
	std::vector<wxUIObject*>::iterator it = std::find( m_objects.begin(), m_objects.end(), obj );
	if ( it != m_objects.end() )
	{
		delete (*it);
		m_objects.erase( it );
	}
}

void wxUIFormData::DeleteAll()
{
	for ( size_t i=0;i<m_objects.size();i++ )
		delete m_objects[i];
	m_objects.clear();
}

wxUIObject *wxUIFormData::Find( const wxString &name )
{
	for( size_t i=0;i<m_objects.size();i++ )
		if ( m_objects[i]->GetName() == name )
			return m_objects[i];

	return 0;
}

std::vector<wxUIObject*> wxUIFormData::GetObjects()
{
	return m_objects;
}

wxUIObject **wxUIFormData::GetObjects( size_t *n )
{
	if ( n != 0 ) *n = m_objects.size();
	if ( m_objects.size() > 0 ) return &m_objects[0];
	else return 0;
}

void wxUIFormData::Raise( wxUIObject *obj )
{
	if ( m_objects.size() <= 1 ) return;
	int selfindex = -1;	
	for (size_t i=0;i<m_objects.size();i++)
	{
		if (m_objects[i] == obj)
		{
			selfindex = i;
			break;
		}
	}

	if (selfindex <= 0) return;

	for( int i=selfindex; i > 0; i-- )
		m_objects[i] = m_objects[i-1];

	m_objects[0] = obj;
}

// form properties
void wxUIFormData::SetName( const wxString &name )
{
	m_name = name;
}

wxString wxUIFormData::GetName()
{
	return m_name;
}

void wxUIFormData::SetSize( int width, int height )
{
	m_width = width;
	m_height = height;
	
	if ( m_formWindow != 0 )
	{
		double xs, ys;
		wxDevicePPIToScale( wxClientDC( m_formWindow ).GetPPI(), &xs, &ys );
		m_formWindow->SetClientSize( wxScaleSize( wxSize(width,height), xs, ys ) );
	}
}

wxSize wxUIFormData::GetSize()
{
	return wxSize( m_width, m_height );
}



DEFINE_EVENT_TYPE( wxEVT_UIFORM_SELECT )

wxUIObjectCopyBuffer::wxUIObjectCopyBuffer()
{
	/* nothing to do */
}

wxUIObjectCopyBuffer::~wxUIObjectCopyBuffer()
{
	Clear();
}

void wxUIObjectCopyBuffer::Clear()
{
	for (size_t i=0;i<m_copyList.size();i++)
		delete m_copyList[i];
	m_copyList.clear();
}
void wxUIObjectCopyBuffer::Assign( std::vector<wxUIObject*> &objlist )
{
	Clear();
	m_copyList = objlist;
}

std::vector<wxUIObject*> wxUIObjectCopyBuffer::Get()
{
	return m_copyList;
}

int wxUIObjectCopyBuffer::Count()
{
	return m_copyList.size();
}

enum {	ID_CREATE_CONTROL = wxID_HIGHEST + 985,	ID_nMaxControls = ID_CREATE_CONTROL+100,
		ID_DUPLICATE, ID_DELETE, ID_COPY, ID_PASTE, ID_CLEARALL, ID_TABORDERMODE,		
		ID_ALIGNTOP, ID_ALIGNLEFT, ID_ALIGNRIGHT, ID_ALIGNBOTTOM
};

BEGIN_EVENT_TABLE( wxUIFormEditor, wxWindow )

	EVT_MENU_RANGE( ID_CREATE_CONTROL, ID_CREATE_CONTROL+100, wxUIFormEditor::OnCreateCtrl )

	EVT_MENU( ID_TABORDERMODE, wxUIFormEditor::OnPopup)
	EVT_MENU( ID_DELETE, wxUIFormEditor::OnPopup )
	EVT_MENU( ID_CLEARALL, wxUIFormEditor::OnPopup )
	EVT_MENU( ID_DUPLICATE, wxUIFormEditor::OnPopup )
	EVT_MENU( ID_COPY, wxUIFormEditor::OnPopup )
	EVT_MENU( ID_PASTE, wxUIFormEditor::OnPopup )
	EVT_MENU( ID_ALIGNTOP, wxUIFormEditor::OnPopup )
	EVT_MENU( ID_ALIGNLEFT, wxUIFormEditor::OnPopup )
	EVT_MENU( ID_ALIGNRIGHT, wxUIFormEditor::OnPopup )
	EVT_MENU( ID_ALIGNBOTTOM, wxUIFormEditor::OnPopup )

	EVT_LEFT_DOWN( wxUIFormEditor::OnLeftDown )
	EVT_LEFT_DCLICK( wxUIFormEditor::OnDoubleClick )
	EVT_LEFT_UP( wxUIFormEditor::OnLeftUp )
	EVT_RIGHT_DOWN( wxUIFormEditor::OnRightDown )
	EVT_MOTION( wxUIFormEditor::OnMouseMove )
	EVT_PAINT( wxUIFormEditor::OnPaint )
	EVT_SIZE( wxUIFormEditor::OnSize )

END_EVENT_TABLE()

#define RSZBOXW 6
#define BOX_NONE 0
#define BOX_TOPLEFT 1
#define BOX_TOPRIGHT 2
#define BOX_BOTTOMLEFT 3
#define BOX_BOTTOMRIGHT 4
#define BOX_TOP 5
#define BOX_LEFT 6
#define BOX_RIGHT 7
#define BOX_BOTTOM 8


wxUIFormEditor::wxUIFormEditor( wxWindow *parent, int id, const wxPoint &pos, const wxSize &size )
	: wxWindow(parent, id, pos, size, wxCLIP_CHILDREN)
{
	SetBackgroundStyle( wxBG_STYLE_CUSTOM );

	m_enableScaling = true;
	m_scaleX = m_scaleY = 1.0;
	m_form = 0;
	m_copyBuffer = 0;
	m_propEditor = 0;

	m_viewMode = false;
	m_snapSpacing = 3;

	m_tabOrderCounter = 1;
	m_tabOrderMode = false;

	SetBackgroundColour( *wxWHITE );
	SetCursor( wxCursor( wxCURSOR_ARROW ) );
	
	m_multiSelMode = false;
	m_multiSelModeErase = false;

	m_moveMode = false;
	m_moveModeErase = false;
	m_origX = m_origY = m_diffX = m_diffY = 0;
	m_popupX = m_popupY = 0;

	m_resizeMode = false;
	m_resizeModeErase = false;
	m_resizeBox = BOX_NONE;	
}

wxUIObject *wxUIFormEditor::CreateObject( const wxString &type )
{
	if ( m_form != 0 )
	{
		wxUIObject *obj = wxUIObjectTypeProvider::Create( type );
		if ( !obj ) return 0;

		m_form->Add( obj );
		if ( obj->GetNative() != 0 )
			obj->Show( m_viewMode );

		Refresh();
		return obj;
	}
	else return 0;
}

void wxUIFormEditor::ClearSelections()
{
	if ( m_propEditor ) m_propEditor->SetObject( 0 ) ;
	m_selected.clear();
	Refresh();
}

std::vector<wxUIObject*> wxUIFormEditor::GetSelections()
{
	return m_selected;
}

bool wxUIFormEditor::IsSelected( wxUIObject *obj )
{
	return std::find( m_selected.begin(), m_selected.end(), obj ) != m_selected.end();
}

bool wxUIFormEditor::IsSelected( const wxString &name )
{
	if ( !m_form ) return false;
	else return IsSelected( m_form->Find( name ) );
}

void wxUIFormEditor::SetFormData( wxUIFormData *form )
{
	if ( m_propEditor ) m_propEditor->SetObject( 0 );
	m_form = form;
}

void wxUIFormEditor::SetCopyBuffer( wxUIObjectCopyBuffer *cpbuf )
{
	m_copyBuffer = cpbuf;
}

void wxUIFormEditor::SetPropertyEditor( wxUIPropertyEditor *pe )
{
	m_propEditor = pe;
}

void wxUIFormEditor::SetViewMode( bool b )
{
	m_viewMode = b;
	if ( m_form != 0 )
	{
		std::vector<wxUIObject*> objs = m_form->GetObjects();
		for( size_t i=0;i<objs.size();i++ )
		{
			if ( objs[i]->GetNative() != 0 )
				objs[i]->Show( m_viewMode );
		}

		Refresh();
	}
}

void wxUIFormEditor::Snap(int *x, int *y, int spacing )
{
	if ( spacing < 0 ) spacing = m_snapSpacing;

	*x = Snap(*x, spacing);
	*y = Snap(*y, spacing);
}

int wxUIFormEditor::Snap( int v, int spacing )
{
	int incr = (v<0) ? -1 : 1;

	int multiples = (int)( ((double)v) / ((double)spacing) );
	double dist1 = fabs(spacing*multiples - (double)v);
	double dist2 = fabs(spacing*(multiples+incr) - (double)v);
	
	if ( dist1 < dist2 ) return (int)spacing*multiples;
	else return (int)(spacing*(multiples+incr));
}

void wxUIFormEditor::EnableTabOrderMode( bool b )
{
	if (b)
	{
		wxString result = wxGetTextFromUser("Enter starting tab index number:", wxEmptyString, "1", this);
		if (!result.IsEmpty())
		{
			m_tabOrderCounter = atoi( result.c_str() );
			if (m_tabOrderCounter < 1) m_tabOrderCounter = 1;
		}
		else		
			m_tabOrderCounter = 1;
	}
	m_selected.clear();
	if (m_propEditor) m_propEditor->SetObject( 0 );
	m_tabOrderMode = b;
	Refresh();
}

int wxUIFormEditor::IsOverResizeBox(int x, int y, wxUIObject *obj)
{
	// note:  coordinates here are all unscaled values!

	wxRect rct = obj->GetGeometry();
	
	if (x >= rct.x-RSZBOXW && x <= rct.x &&
			y >= rct.y-RSZBOXW && y <= rct.y)
		return BOX_TOPLEFT;	
	
	if (x >= rct.x-RSZBOXW && x <= rct.x &&
			y >= rct.y + rct.height/2 - RSZBOXW/2 && y <= rct.y + rct.height/2 + RSZBOXW/2)
		return BOX_LEFT;

	if (x >= rct.x-RSZBOXW && x <= rct.x &&
			y >= rct.y + rct.height && y <= rct.y + rct.height + RSZBOXW)
		return BOX_BOTTOMLEFT;

	
	if (x >= rct.x+rct.width/2-RSZBOXW/2 && x <= rct.x+rct.width/2+RSZBOXW/2 &&
			y >= rct.y-RSZBOXW && y <= rct.y)
		return BOX_TOP;
	
	if (x >= rct.x+rct.width/2-RSZBOXW/2 && x <= rct.x+rct.width/2+RSZBOXW/2 &&
			y >= rct.y + rct.height && y <= rct.y + rct.height + RSZBOXW)
		return BOX_BOTTOM;


	if (x >= rct.x+rct.width && x <= rct.x+rct.width+RSZBOXW &&
			y >= rct.y-RSZBOXW && y <= rct.y)
		return BOX_TOPRIGHT;	
	
	if (x >= rct.x+rct.width && x <= rct.x+rct.width+RSZBOXW &&
			y >= rct.y + rct.height/2 - RSZBOXW/2 && y <= rct.y + rct.height/2 + RSZBOXW/2)
		return BOX_RIGHT;

	if (x >= rct.x+rct.width && x <= rct.x+rct.width+RSZBOXW &&
			y >= rct.y + rct.height && y <= rct.y + rct.height + RSZBOXW)
		return BOX_BOTTOMRIGHT;

	return BOX_NONE;
}

void wxUIFormEditor::OnDoubleClick( wxMouseEvent & )
{
	m_enableScaling = !m_enableScaling;
	Refresh();
}

void wxUIFormEditor::OnLeftDown(wxMouseEvent &evt)
{
	if ( !HasCapture() )
		CaptureMouse();

	if ( m_form == 0 ) return;

	if ( m_viewMode ) return;

	// edit mode, enable selections and moving
	int mx = evt.GetX();
	int my = evt.GetY();

	m_popupX = mx;
	m_popupY = my;

	m_origX = mx;
	m_origY = my;

	ClientToScreen( &m_origX, &m_origY );

	if ( m_tabOrderMode )
	{
		std::vector<wxUIObject*> objs = m_form->GetObjects();
		for ( size_t i=0;i<objs.size();i++ )
		{
			wxRect rct = objs[i]->GetGeometry();
			if ( objs[i]->IsWithin( (int)(mx/m_scaleX), (int)(my/m_scaleY) ) )
			{
				objs[i]->Property("TabOrder").Set(m_tabOrderCounter++);
				Refresh();
				break;
			}
		}
			
		if (m_tabOrderCounter > (int)objs.size())
		{
			m_tabOrderMode = false;
			Refresh();
			return;
		}
	}
	else if (m_selected.size() == 1 && 
		IsOverResizeBox( (int)(mx/m_scaleX), (int)(my/m_scaleY), m_selected[0]) > 0 )
	{
		m_resizeBox = IsOverResizeBox( (int)(mx/m_scaleX), (int)(my/m_scaleY),m_selected[0]);
		
		// start a resize
		m_diffX = 0;
		m_diffY = 0;
		m_diffW = 0;
		m_diffH = 0;
		m_resizeMode = true;
		m_resizeModeErase = false;
	}
	else
	{
		// handle a selection procedure	
		wxUIObject *select_obj = 0;
		wxUIObject *move_obj = 0;
		std::vector<wxUIObject*> objs = m_form->GetObjects();
		for (size_t i=0;i<objs.size();i++)
		{
			if ( objs[i]->IsWithin( (int)(mx/m_scaleX), (int)(my/m_scaleY) ) )
			{
				select_obj = objs[i];
				move_obj = objs[i];
				m_form->Raise( select_obj );
				break;
			}
		}

		bool redraw = false;

		if ( select_obj != 0 
			&& std::find( m_selected.begin(), 
					m_selected.end(), 
					select_obj) == m_selected.end())
		{
			if (!evt.ShiftDown())
			{
				m_selected.clear();

				// callback for single item selection
				wxUIFormEvent evt( select_obj, wxEVT_UIFORM_SELECT, this->GetId() );
				evt.SetEventObject( this );
				ProcessEvent( evt );
			}

			m_selected.push_back(select_obj);
			redraw = true;
		}
		else if (evt.ShiftDown() && select_obj != 0 
			&&  std::find( m_selected.begin(), m_selected.end(), select_obj) != m_selected.end())
		{
			m_selected.erase( std::find( m_selected.begin(), m_selected.end(), select_obj) );
			move_obj = 0;
			redraw = true;
		}
		else if (select_obj == 0)
		{
			if (m_selected.size() > 0)
				redraw = true;
			m_selected.clear();
		}


		if (select_obj)
		{
			m_moveMode = true;
			m_moveModeErase = false;
			m_diffX = 0;
			m_diffY = 0;
			m_diffW = 0;
			m_diffH = 0;
		}
		else
		{
			// if clicked in blank space, start a multiple selection box
			m_diffX = 0;
			m_diffY = 0;
			m_multiSelMode = true;
			m_multiSelModeErase = true;
		}

		if (redraw)
			Refresh();
			
		if (m_propEditor)
			m_propEditor->SetObject( m_selected.size() == 1 ? select_obj : 0 ); 
	}
}

void wxUIFormEditor::OnLeftUp(wxMouseEvent &)
{
	if ( HasCapture() )
		ReleaseMouse();

	if ( m_form == 0 ) return;
	
	if ( m_viewMode ) return;

	// allow interface editing
	if (m_moveMode)
	{
		size_t count = m_selected.size();
		for (size_t i=0;i<count;i++)
		{
			wxRect rct = m_selected[i]->GetGeometry();
			rct.x += (int)(m_diffX/m_scaleX);
			rct.y += (int)(m_diffY/m_scaleY);
			Snap(&rct.x, &rct.y);
			m_selected[i]->SetGeometry(rct);
		}
		
		if (m_propEditor)
			m_propEditor->UpdatePropertyValues();

		m_moveMode = false;

#ifdef wxUI_USE_OVERLAY
		m_overlay.Reset();
#else
		if ( m_moveModeErase )
			DrawMoveResizeOutlines();
#endif
		m_moveModeErase = false;
		
		Refresh();
	}
	else if (m_multiSelMode)
	{
		wxRect selbox;	
		selbox.x = m_diffX<0 ? m_origX + m_diffX : m_origX;
		selbox.width = m_diffX<0 ? -m_diffX : m_diffX;
		selbox.y = m_diffY<0 ? m_origY + m_diffY : m_origY;
		selbox.height = m_diffY<0 ? -m_diffY : m_diffY;

		ScreenToClient(&selbox.x, &selbox.y);

		std::vector<wxUIObject*> objs = m_form->GetObjects();
		for ( size_t i=0;i<objs.size();i++ )
		{
			wxRect rct = ScaleRect( objs[i]->GetGeometry() );
			if ( selbox.Contains(rct) )
			{
				if ( std::find( m_selected.begin(), m_selected.end(), objs[i]) == m_selected.end() )
					m_selected.push_back( objs[i] );
				else
					m_selected.erase( std::find( m_selected.begin(), m_selected.end(), objs[i]) );
			}
		}

		m_multiSelMode = false;
#ifdef wxUI_USE_OVERLAY
		m_overlay.Reset();
#else
		if ( m_multiSelModeErase )
			DrawMultiSelBox();
#endif
		m_multiSelModeErase = false;
		
		Refresh();
	}
	else if (m_resizeMode && m_selected.size() == 1)
	{	
		wxRect rct = m_selected[0]->GetGeometry();
		rct.x += (int)(m_diffX/m_scaleX);
		rct.y += (int)(m_diffY/m_scaleY);
		rct.width  += (int)(m_diffW/m_scaleX);
		rct.height += (int)(m_diffH/m_scaleY);

		if (rct.width < 5)
			rct.width = 5;
		if (rct.height < 5)
			rct.height = 5;

		m_selected[0]->SetGeometry(rct);

		if (m_propEditor)
			m_propEditor->UpdatePropertyValues();

		m_resizeMode = false;
#ifdef wxUI_USE_OVERLAY
		m_overlay.Reset();
#else
		if ( m_resizeModeErase )
			DrawMoveResizeOutlines();
#endif
		m_resizeModeErase = false;

		Refresh();
	}
		
	SetCursor(wxCursor( wxCURSOR_ARROW ));
}

void wxUIFormEditor::DrawMultiSelBox()
{
	if (!m_multiSelMode)
		return;

	wxClientDC dc(this);

#ifdef wxUI_USE_OVERLAY
	wxDCOverlay overlaydc( m_overlay, &dc );
	overlaydc.Clear();
#ifdef __WXOSX__
	wxBrush brush( wxColour(240,240,240,130) );
#else
	wxBrush brush( *wxWHITE, wxTRANSPARENT );
#endif
	wxPen pen( wxColour(90,90,90) );
#else
	dc.SetLogicalFunction( wxINVERT );
	wxBrush brush( *wxWHITE, wxTRANSPARENT );
	wxPen pen(*wxBLACK, 2, wxSOLID);
#endif

	pen.SetCap(wxCAP_BUTT);
	pen.SetJoin(wxJOIN_MITER);
	dc.SetBrush(brush);
	dc.SetPen(pen);

	wxRect selbox;	
	selbox.x = m_diffX<0 ? m_origX + m_diffX : m_origX;
	selbox.width = m_diffX<0 ? -m_diffX : m_diffX;
	selbox.y = m_diffY<0 ? m_origY + m_diffY : m_origY;
	selbox.height = m_diffY<0 ? -m_diffY : m_diffY;

	ScreenToClient(&selbox.x, &selbox.y);
	
	dc.DrawRectangle(selbox);

}


void wxUIFormEditor::DrawMoveResizeOutlines()
{
	wxClientDC dc(this);
	
#ifdef wxUI_USE_OVERLAY
	wxDCOverlay overlaydc( m_overlay, &dc );
	overlaydc.Clear();
#ifdef __WXOSX__
	wxBrush brush( wxColour(240,240,240,130) );
#else
	wxBrush brush( *wxWHITE, wxTRANSPARENT );
#endif

#else
	dc.SetLogicalFunction( wxINVERT );
	wxBrush brush( *wxWHITE, wxTRANSPARENT );
#endif

	wxPen pen( wxColour(90,90,90), 1, wxSOLID);
	pen.SetCap(wxCAP_BUTT);
	pen.SetJoin(wxJOIN_MITER);

	dc.SetBrush(brush);
	dc.SetPen(pen);

	int i, count;
	count = m_selected.size();
	for (i=0;i<count;i++)
	{
		wxRect rct = ScaleRect(m_selected[i]->GetGeometry());

		rct.x = rct.x + m_diffX;
		rct.y = rct.y + m_diffY;
		rct.width = rct.width + m_diffW;
		rct.height = rct.height + m_diffH;
		Snap(&rct.x, &rct.y);

		dc.DrawRectangle(rct);
	}

}

void wxUIFormEditor::SetResizeCursor(int pos)
{
	if (pos < 0) pos = m_resizeBox;

	switch(pos)
	{
	case BOX_TOPLEFT:
	case BOX_BOTTOMRIGHT:
		SetCursor( wxCursor( wxCURSOR_SIZENWSE ) );
		break;
	case BOX_TOPRIGHT:
	case BOX_BOTTOMLEFT:
		SetCursor( wxCursor( wxCURSOR_SIZENESW ) );
		break;
	case BOX_TOP:
	case BOX_BOTTOM:
		SetCursor( wxCursor( wxCURSOR_SIZENS ) );
		break;
	case BOX_LEFT:
	case BOX_RIGHT:
		SetCursor( wxCursor( wxCURSOR_SIZEWE ) );
		break;
	default:
		SetCursor( wxCursor( wxCURSOR_SIZING ) );
	}
}

void wxUIFormEditor::OnMouseMove(wxMouseEvent &evt)
{
	if ( m_viewMode ) return;

	int mx = evt.GetX();
	int my = evt.GetY();

	int xroot = mx;
	int yroot = my;
	ClientToScreen(&xroot, &yroot);

	if (m_moveMode)
	{
#ifndef wxUI_USE_OVERLAY
		if (m_moveModeErase)
			DrawMoveResizeOutlines();
#endif

		m_diffX = xroot - m_origX;
		m_diffY = yroot - m_origY;

		Snap(&m_diffX, &m_diffY);

		DrawMoveResizeOutlines();
		m_moveModeErase = true;
	}
	else if (m_resizeMode)
	{
		SetResizeCursor();

#ifndef wxUI_USE_OVERLAY
		if (m_resizeModeErase)
			DrawMoveResizeOutlines();
#endif
		int diffx = xroot - m_origX;
		int diffy = yroot - m_origY;

		switch(m_resizeBox)
		{
		case BOX_TOPLEFT:
			m_diffX = diffx;
			m_diffY = diffy;
			m_diffW = -diffx;
			m_diffH = -diffy;
			break;
		case BOX_TOPRIGHT:
			m_diffX = 0;
			m_diffY = diffy;
			m_diffW = diffx;
			m_diffH = -diffy;
			break;
		case BOX_BOTTOMLEFT:
			m_diffX = diffx;
			m_diffY = 0;
			m_diffW = -diffx;
			m_diffH = diffy;
			break;
		case BOX_BOTTOMRIGHT:
			m_diffX = 0;
			m_diffY = 0;
			m_diffW = diffx;
			m_diffH = diffy;
			break;
		case BOX_TOP:
			m_diffX = 0;
			m_diffY = diffy;
			m_diffW = 0;
			m_diffH = -diffy;				
			break;
		case BOX_LEFT:
			m_diffX = diffx;
			m_diffY = 0;
			m_diffW = -diffx;
			m_diffH = 0;
			break;
		case BOX_RIGHT:
			m_diffX = 0;
			m_diffY = 0;
			m_diffW = diffx;
			m_diffH = 0;
			break;
		case BOX_BOTTOM:
			m_diffX = 0;
			m_diffY = 0;
			m_diffW = 0;
			m_diffH = diffy;
			break;
		default:
			break;
		}

		Snap(&m_diffX, &m_diffY);
		Snap(&m_diffW, &m_diffH);

		DrawMoveResizeOutlines();
		m_resizeModeErase = true;
	}
	else if (m_multiSelMode)
	{
#ifndef wxUI_USE_OVERLAY
		if (m_multiSelModeErase)
			DrawMultiSelBox();
#endif

		m_diffX = xroot - m_origX;
		m_diffY = yroot - m_origY;

		DrawMultiSelBox();
		m_multiSelModeErase = true;
	}
	else if (m_selected.size() == 1)
	{
		int box = IsOverResizeBox((int)(mx/m_scaleX), (int)(my/m_scaleY), m_selected[0]);
		if (box)
			SetResizeCursor(box);
		else
			SetCursor( wxCursor( wxCURSOR_ARROW ) );
	}
}

void wxUIFormEditor::OnSize(wxSizeEvent &)
{
	Refresh();
}


void wxUIFormEditor::OnRightDown(wxMouseEvent &evt)
{
	if ( evt.ShiftDown() )
	{
		SetViewMode( !m_viewMode );
		return;
	}
	
	m_popupX = evt.GetX();
	m_popupY = evt.GetY();


	wxMenu popup;	
	
	for( size_t i=0; i<m_selected.size();i++ )
	{
		wxMenuItem *it = popup.Append( wxID_ANY, "[ " + m_selected[i]->GetTypeName() + ": " + m_selected[i]->GetName() + " ]" );
		it->Enable( false );
	}

	if ( m_selected.size() > 0 )
		popup.AppendSeparator();

	std::vector<wxUIObject*> ctrls = wxUIObjectTypeProvider::GetTypes();
	for ( size_t i=0;i<ctrls.size();i++ )
		popup.Append( ID_CREATE_CONTROL+i, "Create '" + ctrls[i]->GetTypeName() + "'");

	popup.AppendSeparator();
	popup.AppendCheckItem( ID_TABORDERMODE, "Tab order mode" );
	popup.Check( ID_TABORDERMODE, m_tabOrderMode );
	popup.AppendSeparator();

	wxMenu *align_menu = new wxMenu;
	align_menu->Append( ID_ALIGNTOP, "Top Edges");
	align_menu->Append( ID_ALIGNLEFT, "Left Edges");
	align_menu->Append( ID_ALIGNRIGHT, "Right Edges");
	align_menu->Append( ID_ALIGNBOTTOM, "Bottom Edges");
	popup.AppendSubMenu( align_menu, "Align");
	
	popup.Append( ID_DUPLICATE, "Duplicate");
	popup.Append( ID_DELETE, "Delete");
	popup.Append( ID_CLEARALL, "Clear all");

	popup.AppendSeparator();	
	popup.Append( ID_COPY, "Copy");
	popup.Append( ID_PASTE, "Paste");

	PopupMenu( &popup );
}

void wxUIFormEditor::OnPaint(wxPaintEvent &)
{
	wxAutoBufferedPaintDC dc(this);
	
	wxSize sz = GetSize();
	dc.SetBackground( GetBackgroundColour() );
	dc.Clear();

	wxSize dpi( dc.GetPPI() );	
	if ( m_enableScaling ) wxDevicePPIToScale( dpi, &m_scaleX, &m_scaleY );
	else m_scaleX = m_scaleY = 1.0;

	dc.SetTextForeground( *wxLIGHT_GREY );
	dc.SetFont( *wxNORMAL_FONT );
	dc.DrawText( wxString::Format("dpi(%d,%d) Xs: %lg Ys: %lg", dpi.x, dpi.y, m_scaleX, m_scaleY ), 10, 10 );

	dc.SetPen( *wxBLACK_PEN );
	dc.DrawLine( 10, 0, 20, 10 );

	dc.SetPen( *wxRED_PEN );
	dc.DrawLine( wxPoint(0, 0), wxPoint((int)(10*m_scaleX), (int)(10*m_scaleY)) );
	
	double scale = m_scaleX < m_scaleY ? m_scaleX : m_scaleY;	

	if ( !m_viewMode )
	{
		dc.SetPen( wxColour(210,210,210) );

		int spacing = m_snapSpacing * 3 * scale;

		for (int i=spacing;i<sz.GetWidth();i+=spacing)
			for(int j=spacing;j<sz.GetHeight();j+=spacing)
				dc.DrawPoint(i , j);
	}

	if ( m_form == 0 ) return;

	// paint the children
	wxRect rct;
	std::vector<wxUIObject*> objs = m_form->GetObjects();
	for ( int i=(int)objs.size()-1;i>=0;i-- )
	{
		if ( !objs[i]->IsNativeObject() || !m_viewMode )
		{
			rct = ScaleRect(objs[i]->GetGeometry());
			dc.SetClippingRegion(rct);			
			if ( objs[i]->DrawDottedOutline() && !m_viewMode )
			{
				wxPen p = wxPen(*wxBLACK, 1, wxDOT);
				p.SetCap(wxCAP_BUTT);
				p.SetJoin(wxJOIN_MITER);
				dc.SetPen(p);
				dc.SetBrush(*wxTRANSPARENT_BRUSH);
				dc.DrawRectangle(rct.x, rct.y, rct.width, rct.height);
			}

			
			objs[i]->Draw( this, dc, rct );
			dc.DestroyClippingRegion();
		}

		wxString label, units;
		wxColour colour;
		if ( m_form->GetMetaData( objs[i]->GetName(),
				&label, &units, &colour ) )
		{
			dc.SetFont(*wxNORMAL_FONT);
			rct = ScaleRect( objs[i]->GetGeometry() );
			dc.SetTextForeground( colour );

			int sw, sh;
			dc.GetTextExtent(label, &sw, &sh);
			dc.DrawText(label, rct.x - sw - 3, rct.y+ rct.height/2-sh/2);

			dc.GetTextExtent(units, &sw, &sh);
			dc.DrawText(units, rct.x + rct.width + 2, rct.y+ rct.height/2-sh/2);
		}
	}

	if ( !m_viewMode )
	{
		// paint any selection handles
		dc.SetBrush(wxBrush( g_uiSelectColor ));
		dc.SetPen(wxPen( g_uiSelectColor ));
		for (size_t i=0;i<m_selected.size();i++)
		{
			wxRect rct = m_selected[i]->GetGeometry();
			
			// left side
			dc.DrawRectangle(ScaleRect(wxRect(rct.x - RSZBOXW, rct.y - RSZBOXW, RSZBOXW, RSZBOXW)));
			dc.DrawRectangle(ScaleRect(wxRect(rct.x - RSZBOXW, rct.y + rct.height/2 - RSZBOXW/2, RSZBOXW, RSZBOXW)));
			dc.DrawRectangle(ScaleRect(wxRect(rct.x - RSZBOXW, rct.y + rct.height, RSZBOXW, RSZBOXW)));

			// right side
			dc.DrawRectangle(ScaleRect(wxRect(rct.x + rct.width, rct.y - RSZBOXW, RSZBOXW, RSZBOXW)));
			dc.DrawRectangle(ScaleRect(wxRect(rct.x + rct.width, rct.y + rct.height/2 - RSZBOXW/2, RSZBOXW, RSZBOXW)));
			dc.DrawRectangle(ScaleRect(wxRect(rct.x + rct.width, rct.y + rct.height, RSZBOXW, RSZBOXW)));
			
			// bottom
			dc.DrawRectangle(ScaleRect(wxRect(rct.x + rct.width/2 - RSZBOXW/2, rct.y + rct.height, RSZBOXW, RSZBOXW)));

			// top
			dc.DrawRectangle(ScaleRect(wxRect(rct.x + rct.width/2 - RSZBOXW/2, rct.y - RSZBOXW, RSZBOXW, RSZBOXW)));
		}

		if (m_tabOrderMode)
		{
			wxFont f = *wxNORMAL_FONT;
			f.SetWeight(wxFONTWEIGHT_BOLD);
			dc.SetFont(f);
			for (size_t i=0;i<objs.size();i++)
			{
				if ( objs[i]->Property("TabOrder").IsValid() )
				{
					rct = ScaleRect(objs[i]->GetGeometry());
				
					int tw, th;
					wxString tabnum = wxString::Format("%d",objs[i]->Property("TabOrder").GetInteger());
					dc.GetTextExtent(tabnum, &tw, &th);

					dc.SetBrush(*wxLIGHT_GREY_BRUSH);
					dc.SetPen(*wxLIGHT_GREY_PEN);
					dc.DrawRectangle(rct.x, rct.y, tw+4, th+4);

					dc.SetTextForeground(*wxRED);
					dc.DrawText(tabnum, rct.x+2, rct.y+2);

					dc.SetBrush(*wxTRANSPARENT_BRUSH);
					dc.SetPen(wxPen(*wxBLUE));
					dc.DrawRectangle(rct.x, rct.y, tw+4, th+4);
					dc.DrawRectangle(rct.x-2, rct.y-2, rct.width+4, rct.height+4);
				}
			}
		}
	}
	else
	{
		dc.SetTextForeground( *wxLIGHT_GREY );
		dc.SetFont( *wxNORMAL_FONT );
		dc.DrawText( "View mode.  Shift-right-click to return to editing.", 5, sz.GetHeight() - 5 - dc.GetCharHeight() );
	}
}

void wxUIFormEditor::OnPopup(wxCommandEvent &evt)
{
	if ( m_form == 0 || m_viewMode ) return;

	if ( evt.GetId() == ID_COPY && m_copyBuffer != 0 )
	{
		std::vector<wxUIObject*> list;
		for( size_t i=0; i<m_selected.size(); i++ )
			list.push_back( m_selected[i]->Duplicate() );

		m_copyBuffer->Assign( list );
	}
	else if ( evt.GetId() == ID_PASTE && m_copyBuffer != 0 )
	{
		std::vector<wxUIObject*> topaste = m_copyBuffer->Get();
		m_selected.clear();
		for ( size_t i=0;i<topaste.size();i++)
		{
			wxUIObject *obj = topaste[i]->Duplicate();
			m_form->Add( obj );
			if ( obj->GetNative() != 0 )
				obj->Show( m_viewMode );

			m_selected.push_back(obj);
		}
		Refresh();
	}
	else if ( evt.GetId() == ID_DUPLICATE )
	{
		if ( m_selected.size() > 0 )
		{
			std::vector<wxUIObject*> added;
			
			int x0 = 0, y0 = 0, dx, dy;
			for ( size_t i=0;i<m_selected.size();i++)
			{
				wxUIObject *tocopy = m_selected[i];
				wxRect geom = tocopy->GetGeometry();
				if (i==0)
				{
					x0 = geom.x;
					y0 = geom.y;
				}

				dx = geom.x - x0;
				dy = geom.y - y0;

				wxUIObject *obj = tocopy->Duplicate();
				obj->SetGeometry( wxRect( (int)(m_popupX/m_scaleX)+dx, 
					(int)(m_popupY/m_scaleY)+dy, 
					geom.width, geom.height ) );

				m_form->Add( obj );
				if ( obj->GetNative() != 0 )
					obj->Show( m_viewMode );

				added.push_back( obj );
			}

			m_selected = added;
			Refresh();
		}
	}
	else if ( evt.GetId() == ID_TABORDERMODE )
	{
		EnableTabOrderMode( !m_tabOrderMode );
		Refresh();
	}
	else if ( evt.GetId() == ID_DELETE )
	{
		if ( m_selected.size() > 0 )
		{
			if (m_propEditor) m_propEditor->SetObject( 0 );
			
			for (size_t i=0;i<m_selected.size();i++)
				m_form->Delete( m_selected[i] );
			m_selected.clear();
			Refresh();
		}
	}
	else if ( evt.GetId() == ID_CLEARALL )
	{
		if (wxMessageBox("Really clear the form?", "Query", wxYES_NO|wxICON_EXCLAMATION) == wxYES)
		{
			if (m_propEditor) m_propEditor->SetObject( 0 );
			m_selected.clear();
			m_form->DeleteAll();
			Refresh();
		}
	}
	else if ( evt.GetId() == ID_ALIGNTOP
		|| evt.GetId() == ID_ALIGNLEFT
		|| evt.GetId() == ID_ALIGNRIGHT
		|| evt.GetId() == ID_ALIGNBOTTOM )
	{
		if ( m_selected.size() > 1)
		{
			wxRect geom = m_selected[0]->GetGeometry();
			for ( size_t i=1;i<m_selected.size();i++)
			{
				wxRect objgeom = m_selected[i]->GetGeometry();

				if (evt.GetId() == ID_ALIGNTOP)
					objgeom.y = geom.y;
				else if (evt.GetId() == ID_ALIGNLEFT)
					objgeom.x = geom.x;
				else if (evt.GetId() == ID_ALIGNRIGHT)
					objgeom.x = (geom.x+geom.width)-objgeom.width;
				else if (evt.GetId() == ID_ALIGNBOTTOM)
					objgeom.y = (geom.y+geom.height)-objgeom.height;

				m_selected[i]->SetGeometry(objgeom);
			}

			Refresh();
		}
	}
}

void wxUIFormEditor::OnCreateCtrl( wxCommandEvent &evt )
{
	if ( !m_form ) return;

	size_t id = evt.GetId() - ID_CREATE_CONTROL;
	std::vector<wxUIObject*> types = wxUIObjectTypeProvider::GetTypes();
	if ( id < types.size() )
	{
		wxUIObject *obj = types[id]->Duplicate();
		wxRect rct = obj->GetGeometry();
		rct.x = (int)((m_popupX - rct.width/2)/m_scaleX);
		rct.y = (int)((m_popupY - rct.height/2)/m_scaleY);
		obj->SetGeometry( rct );
		m_form->Add( obj );	
		if ( obj->GetNative() != 0 )
			obj->Show( m_viewMode );

		m_selected.clear();
		if ( m_propEditor ) m_propEditor->SetObject( 0 );
		Refresh();
	}
}

void wxUIFormEditor::GetScale( double *xs, double *ys )
{
	*xs = m_scaleX;
	*ys = m_scaleY;
}

wxSize wxUIFormEditor::ScaleSize( const wxSize &s ) { return wxScaleSize( s, m_scaleX, m_scaleY ); }
wxRect wxUIFormEditor::ScaleRect( const wxRect &r ) { return wxScaleRect( r, m_scaleX, m_scaleY ); }

BEGIN_EVENT_TABLE( wxUIFormDesigner, wxScrolledWindow )
	EVT_PAINT( wxUIFormDesigner::OnPaint )
	EVT_SIZE( wxUIFormDesigner::OnResize )
	EVT_LEFT_DOWN( wxUIFormDesigner::OnMouseDown )
	EVT_LEFT_UP( wxUIFormDesigner::OnMouseUp )
	EVT_MOTION( wxUIFormDesigner::OnMouseMove )
END_EVENT_TABLE()

wxUIFormDesigner::wxUIFormDesigner(wxWindow *parent, int id, const wxPoint &pos, const wxSize &size )
	: wxScrolledWindow( parent, wxID_ANY, pos, size, wxHSCROLL|wxVSCROLL|wxWANTS_CHARS  )
{
	SetBackgroundStyle( wxBG_STYLE_CUSTOM );
//	SetBackgroundColour( *wxWHITE );
	m_formData = 0;
	m_editor = new wxUIFormEditor( this, id, wxPoint(1,1), wxScaleSize(400,300) );
	m_diffX = m_diffY = 0;
	m_mouseDown = false;
	UpdateScrollbars();
}

void wxUIFormDesigner::SetFormData( wxUIFormData *form )
{
	m_editor->ClearSelections();

	if( m_formData != 0 )
		m_formData->Detach();

	m_formData = form;
	
	m_editor->SetFormData( m_formData );
	if( m_formData == 0 ) return;
	
	m_formData->Attach( m_editor );
	m_editor->SetViewMode( false );	
	m_editor->SetClientSize( m_editor->ScaleSize(m_formData->GetSize()) );
	UpdateScrollbars();

	Refresh();
}

wxUIFormData *wxUIFormDesigner::GetFormData()
{
	return m_formData;
}

void wxUIFormDesigner::SetFormSize( int width, int height )
{
	if ( m_formData ) m_formData->SetSize( width, height );
	else m_editor->SetClientSize( m_editor->ScaleSize( wxSize(width, height) ) );
}

wxSize wxUIFormDesigner::GetFormSize()
{
	wxSize sz( m_editor->GetClientSize() );
	double xs, ys;
	m_editor->GetScale( &xs, &ys );	
	return wxSize( (int)(sz.x/xs), (int)(sz.y/ys) );
}

void wxUIFormDesigner::OnPaint(wxPaintEvent &)
{
	wxAutoBufferedPaintDC pdc(this);
	PrepareDC(pdc);
	pdc.SetDeviceClippingRegion( GetUpdateRegion() );

	pdc.SetBackground( wxBrush( GetBackgroundColour() ));
	pdc.Clear();

	int fx,fy,fw,fh;
	m_editor->GetPosition(&fx,&fy);
	m_editor->GetClientSize(&fw,&fh);

	int vx,vy;
	GetViewStart(&vx,&vy);

	pdc.SetPen(wxPen( g_uiSelectColor ));
	pdc.SetBrush(*wxTRANSPARENT_BRUSH);
	pdc.DrawRectangle(vx+fx-1, vy+fy-1, fw+2, fh+2);

	pdc.SetBrush(wxBrush(g_uiSelectColor));
	int rct = (int)(10.0*wxGetScreenHDScale());
	pdc.DrawRectangle(vx+fx+fw, vy+fy+fh, rct, rct);

	pdc.DestroyClippingRegion();
}

void wxUIFormDesigner::OnResize(wxSizeEvent &)
{
	Refresh();
}

void wxUIFormDesigner::OnMouseDown(wxMouseEvent &evt)
{
	int vx,vy;
	GetViewStart(&vx,&vy);
	int mx = evt.GetPosition().x + vx;
	int my = evt.GetPosition().y + vy;

	int mw,mh;
	m_editor->GetClientSize(&mw,&mh);

	int rct = (int)(10.0*wxGetScreenHDScale());

	if ( mx >= mw+1 && mx < mw+rct+1
		&& my >= mh+1 && my < mh+rct+1 )
	{
		m_diffX = mx - mw;
		m_diffY = my - mh;
		m_mouseDown = true;
		if ( !HasCapture() )
			CaptureMouse();
	}
}

void wxUIFormDesigner::UpdateScrollbars()
{
	int w,h;
	m_editor->GetClientSize(&w,&h);
	m_editor->Move( wxPoint(1, 1 ) );
	SetScrollbars(1,1, w+20, h+20, 0, 0);
}

void wxUIFormDesigner::OnMouseUp(wxMouseEvent &)
{
	m_mouseDown = false;
	if ( HasCapture()) this->ReleaseMouse();
	
	UpdateScrollbars();
}

void wxUIFormDesigner::OnMouseMove(wxMouseEvent &evt)
{
	if ( m_mouseDown )
	{
		int vx,vy;
		GetViewStart(&vx,&vy);
		int mx = evt.GetPosition().x + vx;
		int my = evt.GetPosition().y + vy;
		if (mx > 20 && my > 20)
		{

			if ( m_formData ) 
			{
				double xs, ys;
				m_editor->GetScale( &xs, &ys );
				m_formData->SetSize( (int)((mx-m_diffX)/xs), (int)((my-m_diffY)/ys) );
			}
			else
				m_editor->SetClientSize(mx-m_diffX, my-m_diffY);

			Refresh();
		}
	}
}
