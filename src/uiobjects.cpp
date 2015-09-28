#include <wx/dcbuffer.h>
#include <wx/clipbrd.h>
#include <wx/tokenzr.h>
#include <wx/renderer.h>
#include <wx/statline.h>

#include <wex/uiform.h>
#include <wex/extgrid.h>
#include <wex/plot/plplotctrl.h>
#include <wex/csv.h>
#include <wex/utils.h>

#include "ptlayoutctrl.h"
#include "materials.h"
#include "troughloop.h"
#include "shadingfactors.h"
#include "library.h"
#include "lossadj.h"
#include "widgets.h"
#include "uiobjects.h"



class wxUISchedNumericObject : public wxUIObject
{
public:
	wxUISchedNumericObject() {
		AddProperty( "Label", new wxUIProperty("Value") );
		AddProperty( "UseSchedule", new wxUIProperty( true ) );
		AddProperty( "ScheduleOnly", new wxUIProperty( false ) );
		AddProperty( "FixedLength", new wxUIProperty( (int)-1 ) );
		AddProperty( "Description", new wxUIProperty( wxString("") ) );
		AddProperty( "TabOrder", new wxUIProperty( -1 ) );
	}
	virtual wxString GetTypeName() { return "SchedNumeric"; }
	virtual wxUIObject *Duplicate() { wxUIObject *o = new wxUISchedNumericObject; o->Copy( this ); return o; }
	virtual bool IsNativeObject() { return true; }
	virtual bool DrawDottedOutline() { return false; }
	virtual wxWindow *CreateNative( wxWindow *parent ) {
		AFSchedNumeric *sn = new AFSchedNumeric( parent, wxID_ANY ) ;
		sn->SetLabel( Property("Label").GetString() );
		sn->UseSchedule( Property("UseSchedule").GetBoolean() );
		sn->ScheduleOnly( Property("ScheduleOnly").GetBoolean() );
		sn->SetFixedLen( Property("FixedLength").GetInteger() );
		sn->SetDescription( Property("Description").GetString() );
		return AssignNative( sn );
	}
	virtual void OnPropertyChanged( const wxString &id, wxUIProperty *p )
	{
		if ( AFSchedNumeric *sn = GetNative<AFSchedNumeric>() )
		{
			if ( id == "Label" ) sn->SetLabel( p->GetString() );
			else if ( id == "UseSchedule" ) sn->UseSchedule( p->GetBoolean() );
			else if ( id == "ScheduleOnly" ) sn->ScheduleOnly( p->GetBoolean() );
			else if ( id == "FixedLength" ) sn->SetFixedLen( p->GetInteger() );
			else if ( id == "Description" ) sn->SetDescription( p->GetString() );
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
		AddProperty( "Caption", new wxUIProperty( wxString("") ) );
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
		wxSize tsz( 10, 10 ); // text extents
		wxString capt = Property("Caption").GetString();
		if (!capt.IsEmpty() )
		{
			wxFont font( *wxNORMAL_FONT );
			if ( Property("Bold").GetBoolean() ) font.SetWeight( wxFONTWEIGHT_BOLD );
			dc.SetFont( font );
			tsz = dc.GetTextExtent( capt );
		}
		
		dc.SetPen( wxPen( Property("Colour").GetColour() ) );
		bool horiz = (Property("Orientation").GetInteger() == 0);
		if ( horiz ) dc.DrawLine( geom.x, geom.y+1+tsz.y/2, geom.x+geom.width, geom.y+1+tsz.y/2 );
		else dc.DrawLine( geom.x+4, geom.y, geom.x+4, geom.y+geom.height );
			
		if ( !capt.IsEmpty() )
		{
			if ( horiz )
			{
				dc.SetBrush( wxBrush( win->GetBackgroundColour() ) );
				dc.SetPen( *wxTRANSPARENT_PEN );
				dc.DrawRectangle(geom.x+5, geom.y, tsz.x+2, tsz.y+1);
			}
			dc.SetTextForeground(wxSystemSettings::GetColour(wxSYS_COLOUR_BTNTEXT));
			dc.DrawText( capt, geom.x+6, geom.y );
		}
	}
};

class wxUIPlotObject : public wxUIObject
{
public:
	wxUIPlotObject() {
		Property("Width").Set(400);
		Property("Height").Set(300);
	}
	virtual wxString GetTypeName() { return "Plot"; }
	virtual wxUIObject *Duplicate() { wxUIObject *o = new wxUIPlotObject; o->Copy( this ); return o; }
	virtual bool IsNativeObject() { return true; }
	virtual wxWindow *CreateNative( wxWindow *parent ) {		
		wxPLPlotCtrl *plot = new wxPLPlotCtrl( parent, wxID_ANY );
		plot->SetBackgroundColour( *wxWHITE );
		return AssignNative( plot );
	}
};

class wxUISearchListBoxObject : public wxUIObject
{
public:
	wxUISearchListBoxObject() {
		AddProperty( "Prompt", new wxUIProperty( wxString("Search for:") ) );
		AddProperty( "Items", new wxUIProperty( wxArrayString() ) );
		AddProperty( "Selection", new wxUIProperty(-1) );
		AddProperty( "TabOrder", new wxUIProperty( -1 ) );
		Property("Width").Set( 500 );
		Property("Height").Set( 100 );
	}
	virtual wxString GetTypeName() { return "SearchListBox"; }
	virtual wxUIObject *Duplicate() { wxUIObject *o = new wxUISearchListBoxObject; o->Copy( this ); return o; }
	virtual bool IsNativeObject() { return true; }
	virtual wxWindow *CreateNative( wxWindow *parent ) {
		wxArrayString items = Property("Items").GetStringList();
		AFSearchListBox *list = new AFSearchListBox( parent, wxID_ANY );
		list->Append( Property("Items").GetStringList() );
		int sel = Property("Selection").GetInteger();
		if ( sel >= 0 && sel < items.size() ) list->SetSelection( sel );
		return AssignNative( list );
	}
	virtual void OnPropertyChanged( const wxString &id, wxUIProperty *p )
	{
		if ( AFSearchListBox *list = GetNative<AFSearchListBox>() )
		{
			if ( id == "Selection" && p->GetInteger() >= 0 && p->GetInteger() < list->Count() )
				list->SetSelection( p->GetInteger() );
			else if ( id == "Items" )
			{
				int sel = list->GetSelection();
				list->Clear();
				list->Append( p->GetStringList() );
				if( sel >= 0 && sel < list->Count() ) list->SetSelection( sel );
			}
			else if ( id == "Prompt" )
				list->SetPromptText( p->GetString() );
		}
	}
	virtual void Draw( wxWindow *win, wxDC &dc, const wxRect &geom )
	{
		dc.SetBrush( *wxWHITE_BRUSH );
		dc.SetPen( wxPen( wxColour(135,135,135) ) );
		dc.SetFont( *wxNORMAL_FONT );
		dc.SetTextForeground( *wxBLACK );
		wxString prompt = Property("Prompt").GetString();
		int tw = dc.GetTextExtent( prompt ).GetWidth();
		dc.DrawText( prompt, geom.x+2, geom.y+2 );
		dc.DrawRectangle( geom.x + tw + 4, geom.y, geom.width-tw-4, 19 );
		dc.DrawRectangle( geom.x, geom.y+21, geom.width, geom.height-21 );
		int sel = Property("Selection").GetInteger();
		wxArrayString items = Property("Items").GetStringList();
		int y=geom.y+23;
		
		dc.SetBrush( *wxLIGHT_GREY_BRUSH );
		dc.SetPen( *wxTRANSPARENT_PEN );
		for (size_t i=0;i<items.size() && y < geom.y+geom.height;i++)
		{
			if ( i==sel ) dc.DrawRectangle( geom.x+1, y-1, geom.width-2, dc.GetCharHeight()+2 );

			dc.DrawText( items[i], geom.x+3, y );
			y += dc.GetCharHeight() + 2;
		}
		dc.SetBrush( wxBrush(wxColour(235,235,235) ) );
		dc.DrawRectangle( geom.x+geom.width-10, geom.y+22, 9, geom.height-23 );
	}
	virtual void OnNativeEvent()
	{
		if ( wxListBox *cbo = GetNative<wxListBox>() )
			Property("Selection").Set( cbo->GetSelection() );
	}
};



class wxUIDataArrayObject : public wxUIObject 
{
public:
	wxUIDataArrayObject() {
		AddProperty("Mode", new wxUIProperty( 0, "Fixed 8760 Values,Multiples of 8760 Values,Variable Length"));
		AddProperty("Label", new wxUIProperty( wxString("") ) );
		AddProperty("Description", new wxUIProperty( wxString("") ) );
		AddProperty("TabOrder", new wxUIProperty( (int)-1 ) );
	}
	virtual wxString GetTypeName() { return "DataArray"; }
	virtual wxUIObject *Duplicate() { wxUIObject *o = new wxUIDataArrayObject; o->Copy( this ); return o; }
	virtual bool IsNativeObject() { return true; }
	virtual bool DrawDottedOutline() { return false; }
	virtual wxWindow *CreateNative( wxWindow *parent ) {
		AFDataArrayButton *da = new AFDataArrayButton( parent, wxID_ANY );
		da->SetMode( Property("Mode").GetInteger() );
		da->SetDescription( Property("Description").GetString() );
		da->SetDataLabel( Property("Label").GetString() );
		return AssignNative( da );
	}
	virtual void Draw( wxWindow *win, wxDC &dc, const wxRect &geom )
	{
		wxRendererNative::Get().DrawPushButton( win, dc, geom );
		dc.SetFont( *wxNORMAL_FONT );
		dc.SetTextForeground( *wxBLACK );
		wxString label("Data array...");
		int x, y;
		dc.GetTextExtent( label, &x, &y );
		dc.DrawText( label, geom.x + geom.width/2-x/2, geom.y+geom.height/2-y/2 );
	}
	virtual void OnPropertyChanged( const wxString &id, wxUIProperty *p )
	{
		if ( AFDataArrayButton *da = GetNative<AFDataArrayButton>() )
		{
			if ( id == "Mode" ) da->SetMode( p->GetInteger() );
			if ( id == "Label" ) da->SetDataLabel( p->GetString() );
			if ( id == "Description" ) da->SetDescription( p->GetString() );
		}
	}

};


class wxUIDataMatrixObject : public wxUIObject
{
public:
	wxUIDataMatrixObject() {
		AddProperty( "PasteAppendRows", new wxUIProperty( false ) );
		AddProperty( "ShowLabels", new wxUIProperty( false ) );
		AddProperty( "ShadeR0C0", new wxUIProperty( false ) );
		AddProperty( "ShowCols", new wxUIProperty( true ) );
		AddProperty( "ShowColLabels", new wxUIProperty( true ) );
		AddProperty( "ColLabels", new wxUIProperty( wxString("") ) );
		AddProperty( "Layout", new wxUIProperty( 0, "Buttons on top,Buttons on side" ) );

		Property("Width").Set(400);
		Property("Height").Set(300);
	}
	virtual wxString GetTypeName() { return "DataMatrix"; }
	virtual wxUIObject *Duplicate() { wxUIObject *o = new wxUIDataMatrixObject; o->Copy( this ); return o; }
	virtual bool IsNativeObject() { return true; }
	virtual wxWindow *CreateNative( wxWindow *parent ) {
		AFDataMatrixCtrl *dm = new AFDataMatrixCtrl( parent, wxID_ANY, wxDefaultPosition, wxDefaultSize, Property("Layout").GetInteger() == 1 );
		dm->PasteAppendRows( Property("PasteAppendRows").GetBoolean() );
		dm->ShowLabels( Property("ShowLabels").GetBoolean() );
		dm->ShadeR0C0( Property("ShadeR0C0").GetBoolean() );
		dm->ShowCols( Property( "ShowCols" ).GetBoolean() );
		dm->ShowColLabels( Property("ShowColLabels").GetBoolean() );
		dm->SetColLabels( Property("ColLabels").GetString() );
		return AssignNative( dm );
	}
	virtual void OnPropertyChanged( const wxString &id, wxUIProperty *p )
	{
		if ( AFDataMatrixCtrl *dm = GetNative<AFDataMatrixCtrl>() )
		{
			if ( id == "PasteAppendRows" ) dm->PasteAppendRows( p->GetBoolean() );
			if ( id == "ShadeR0C0" ) dm->ShadeR0C0( p->GetBoolean() );
			if ( id == "ShowCols" ) dm->ShowCols( p->GetBoolean() );
			if ( id == "ShowLabels" ) dm->ShowLabels( p->GetBoolean() );
			if ( id == "ShowColLabels" ) dm->ShowColLabels( p->GetBoolean() );
			if ( id == "ColLabels" ) dm->SetColLabels( p->GetString() );
		}
	}
};

class wxUIShadingFactorsObject : public wxUIObject 
{
public:
	wxUIShadingFactorsObject() {
		AddProperty("ShowDBOptions", new wxUIProperty(false));
		AddProperty("TabOrder", new wxUIProperty((int)-1));
	}
	virtual wxString GetTypeName() { return "ShadingFactors"; }
	virtual wxUIObject *Duplicate() { wxUIObject *o = new wxUIShadingFactorsObject; o->Copy( this ); return o; }
	virtual bool IsNativeObject() { return true; }
	virtual bool DrawDottedOutline() { return false; }
	virtual wxWindow *CreateNative( wxWindow *parent ) {
		return AssignNative(new ShadingButtonCtrl(parent, wxID_ANY, Property("ShowDBOptions").GetBoolean()));
	}
	virtual void Draw( wxWindow *win, wxDC &dc, const wxRect &geom )
	{
		wxRendererNative::Get().DrawPushButton( win, dc, geom );
		dc.SetFont( *wxNORMAL_FONT );
		dc.SetTextForeground( *wxBLACK );
		wxString label("Edit shading...");
		int x, y;
		dc.GetTextExtent( label, &x, &y );
		dc.DrawText( label, geom.x + geom.width/2-x/2, geom.y+geom.height/2-y/2 );
	}
};

class wxUIValueMatrixObject : public wxUIObject
{
public:
	wxUIValueMatrixObject() {
		AddProperty( "NumRows", new wxUIProperty( (int) 10 ) );
		AddProperty( "NumCols", new wxUIProperty( (int) 2 ) );
		AddProperty( "ColLabels", new wxUIProperty( wxString("") ) );
		AddProperty("TabOrder", new wxUIProperty( (int)-1 ) );
	}
	virtual wxString GetTypeName() { return "ValueMatrix"; }
	virtual wxUIObject *Duplicate() { wxUIObject *o = new wxUIValueMatrixObject; o->Copy( this ); return o; }
	virtual bool IsNativeObject() { return true; }
	virtual bool DrawDottedOutline() { return false; }
	virtual wxWindow *CreateNative( wxWindow *parent ) {
		AFValueMatrixButton *vm = new AFValueMatrixButton( parent, wxID_ANY );
		vm->SetTableSize( Property("NumRows").GetInteger(), Property("NumCols").GetInteger() );
		vm->SetColLabels( Property("ColLabels").GetString() );
		return AssignNative( vm );
	}
	virtual void Draw( wxWindow *win, wxDC &dc, const wxRect &geom )
	{
		dc.SetBrush( wxBrush( "Forest Green" ) );
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

	virtual void OnPropertyChanged( const wxString &id, wxUIProperty *p )
	{
		if ( AFValueMatrixButton *vm = GetNative<AFValueMatrixButton>() )
		{
			int nr, nc;
			vm->GetTableSize( &nr, &nc );
			if ( id == "NumRows") vm->SetTableSize( p->GetInteger(), nc );
			if ( id == "NumCols") vm->SetTableSize( nr, p->GetInteger() );
			if ( id == "ColLabels" ) vm->SetColLabels( p->GetString() );
		}
	}
};


class wxUIMonthByHourFactorCtrl : public wxUIObject
{
public:
	wxUIMonthByHourFactorCtrl() {
		AddProperty( "Title", new wxUIProperty( wxString("Factors") ) );
		AddProperty( "Legend", new wxUIProperty( wxString( "0=off, 1=on" ) ) );
		Property("Width").Set(400);
		Property("Height").Set(300);
	}
	virtual wxString GetTypeName() { return "MonthByHourFactors"; }
	virtual wxUIObject *Duplicate() { wxUIObject *o = new wxUIMonthByHourFactorCtrl; o->Copy( this ); return o; }
	virtual bool IsNativeObject() { return true; }
	virtual wxWindow *CreateNative( wxWindow *parent ) {
		AFMonthByHourFactorCtrl *mxh = new AFMonthByHourFactorCtrl( parent, wxID_ANY );
		mxh->SetTitle( Property("Title").GetString() );
		mxh->SetLegend( Property("Legend").GetString() );
		return AssignNative( mxh );
	}
	virtual void OnPropertyChanged( const wxString &id, wxUIProperty *p )
	{
		if ( AFMonthByHourFactorCtrl *mxh = GetNative<AFMonthByHourFactorCtrl>() )
		{
			if ( id == "Title" ) mxh->SetTitle( p->GetString() );
			if ( id == "Legend" ) mxh->SetLegend( p->GetString() );
		}
	}
};

class wxUILibraryCtrl : public wxUIObject
{
public:
	wxUILibraryCtrl() {
		AddProperty( "Library", new wxUIProperty( wxString("Library Name") ) );
		AddProperty( "Fields", new wxUIProperty( wxString("*") ) );
	}
	virtual wxString GetTypeName() { return "Library"; }
	virtual wxUIObject *Duplicate() { wxUIObject *o = new wxUILibraryCtrl; o->Copy( this ); return o; }
	virtual bool IsNativeObject() { return true; }
	virtual wxWindow *CreateNative( wxWindow *parent ) {
		LibraryCtrl *ll = new LibraryCtrl( parent, wxID_ANY );
		ll->SetLibrary( Property("Library").GetString(), Property("Fields").GetString() );
		return AssignNative( ll );
	}
	virtual void OnPropertyChanged( const wxString &id, wxUIProperty *p )
	{
		if ( LibraryCtrl *ll = GetNative<LibraryCtrl>() )
			if ( id == "Library" || id == "Fields" )
				ll->SetLibrary( Property("Library").GetString(), Property("Fields").GetString() );
	}

};


class wxUILossAdjustmentCtrl : public wxUIObject 
{
public:
	wxUILossAdjustmentCtrl() {
		AddProperty("TabOrder", new wxUIProperty( (int)-1 ) );
		Property("Width").Set(270);
		Property("Height").Set(70);
	}
	virtual wxString GetTypeName() { return "LossAdjustment"; }
	virtual wxUIObject *Duplicate() { wxUIObject *o = new wxUILossAdjustmentCtrl; o->Copy( this ); return o; }
	virtual bool IsNativeObject() { return true; }
	virtual bool DrawDottedOutline() { return false; }
	virtual wxWindow *CreateNative( wxWindow *parent ) {
		return AssignNative( new AFLossAdjustmentCtrl( parent, wxID_ANY ) );
	}
	virtual void Draw( wxWindow *win, wxDC &dc, const wxRect &geom )
	{
		dc.SetPen( *wxWHITE_PEN );
		dc.SetBrush( *wxWHITE_BRUSH );
		dc.DrawRectangle( geom );
		wxSize button( 87,24 );
		wxRendererNative::Get().DrawPushButton( win, dc, wxRect( geom.x, geom.y/*+geom.height/2-button.y/2*/, button.x, button.y ) );
		dc.SetFont( *wxNORMAL_FONT );
		dc.SetTextForeground( *wxBLACK );
		wxString label("Edit losses...");
		int x, y;
		dc.GetTextExtent( label, &x, &y );
		//int yc = geom.y+geom.height/2;
		dc.DrawText( label, geom.x + button.x/2-x/2, geom.y + button.y/2-y/2/*-y/2*/ );
		dc.SetTextForeground( wxColour(29,80,173) );
		dc.DrawText( "Constant loss: n.nn", geom.x+button.x+4, geom.y/*yc-y/2-dc.GetCharHeight()-2*/ );
		dc.DrawText( "Hourly losses: Avg = n.nn", geom.x+button.x+4, geom.y+dc.GetCharHeight()/*yc-y/2*/ );
		dc.DrawText( "Custom periods: n", geom.x + button.x + 4, geom.y+2*dc.GetCharHeight()/*yc + y / 2 + 2*/);
	}
};

void RegisterUIObjectsForSAM()
{
	wxUIObjectTypeProvider::Register( new wxUISchedNumericObject );
	wxUIObjectTypeProvider::Register( new wxUIPTLayoutObject );
	wxUIObjectTypeProvider::Register( new wxUIMatPropObject );
	wxUIObjectTypeProvider::Register( new wxUITroughLoopObject );
	wxUIObjectTypeProvider::Register( new wxUIMonthlyFactorObject );
	wxUIObjectTypeProvider::Register( new wxUIDividerObject );
	wxUIObjectTypeProvider::Register( new wxUIPlotObject );
	wxUIObjectTypeProvider::Register( new wxUISearchListBoxObject );
	wxUIObjectTypeProvider::Register( new wxUIDataArrayObject );
	wxUIObjectTypeProvider::Register( new wxUIDataMatrixObject );
	wxUIObjectTypeProvider::Register( new wxUIShadingFactorsObject );
	wxUIObjectTypeProvider::Register( new wxUIValueMatrixObject );
	wxUIObjectTypeProvider::Register( new wxUIMonthByHourFactorCtrl );
	wxUIObjectTypeProvider::Register( new wxUILibraryCtrl );
	wxUIObjectTypeProvider::Register( new wxUILossAdjustmentCtrl );
}
