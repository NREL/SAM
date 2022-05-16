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
#include <wx/wfstream.h>
#include <wx/ffile.h>
#include <wx/filename.h>
#include <wx/txtstrm.h>

#include <wex/lkscript.h>
#include <wex/numeric.h>
#include <wex/exttext.h>
#include <wex/extgrid.h>
#include <wex/radiochoice.h>
#include <wex/diurnal.h>
#include <wex/utils.h>

#include <lk/parse.h>
#include <lk/lex.h>
#include <lk/eval.h>
#include <lk/stdlib.h>

#include "main.h"
#include "inputpage.h"
#include "invoke.h"
#include "casewin.h"

// UI widgets
#include "widgets.h"
#include "lossadj.h"
#include "ptlayoutctrl.h"
#include "troughloop.h"
#include "materials.h"
#include "shadingfactors.h"
#include "library.h"

UICallbackContext::UICallbackContext( ActiveInputPage *ip, const wxString &desc )
	: CaseCallbackContext( ip->GetCase(), desc ), m_inputPage(ip)
{
	// nothing to do
}
ActiveInputPage *UICallbackContext::InputPage() { return m_inputPage; }
CaseWindow *UICallbackContext::GetCaseWindow() { return m_inputPage->GetCaseWindow(); }
	
void UICallbackContext::SetupLibraries( lk::env_t *env )
{
	env->register_funcs( invoke_uicallback_funcs(), this );
	env->register_funcs( wxLKPlotFunctions() );
	env->register_funcs( wxLKMiscFunctions() );	
	env->register_funcs( wxLKFileFunctions() );
}
 

BEGIN_EVENT_TABLE( ActiveInputPage, wxPanel )
	EVT_BUTTON( wxID_ANY, ActiveInputPage::OnNativeEvent )
	EVT_CHECKBOX( wxID_ANY, ActiveInputPage::OnNativeEvent )
	EVT_CHOICE( wxID_ANY, ActiveInputPage::OnNativeEvent )
	EVT_LISTBOX( wxID_ANY, ActiveInputPage::OnNativeEvent )
	EVT_CHECKLISTBOX( wxID_ANY, ActiveInputPage::OnNativeEvent )
	EVT_RADIOBUTTON( wxID_ANY, ActiveInputPage::OnNativeEvent )
	EVT_TEXT_ENTER( wxID_ANY, ActiveInputPage::OnNativeEvent )
	EVT_NUMERIC( wxID_ANY, ActiveInputPage::OnNativeEvent )
	EVT_SLIDER( wxID_ANY, ActiveInputPage::OnNativeEvent )
	EVT_PTLAYOUT( wxID_ANY, ActiveInputPage::OnNativeEvent )
	EVT_MATPROPCTRL( wxID_ANY, ActiveInputPage::OnNativeEvent )
	EVT_TRLOOP( wxID_ANY, ActiveInputPage::OnNativeEvent )
	EVT_MONTHLYFACTOR( wxID_ANY, ActiveInputPage::OnNativeEvent )
	EVT_DATAARRAYBUTTON( wxID_ANY, ActiveInputPage::OnNativeEvent )
	EVT_DATAMATRIX(wxID_ANY, ActiveInputPage::OnNativeEvent)
	EVT_DIURNALPERIODCTRL(wxID_ANY, ActiveInputPage::OnNativeEvent)
	EVT_MONTHBYHOURFACTOR(wxID_ANY, ActiveInputPage::OnNativeEvent)
	EVT_SHADINGBUTTON( wxID_ANY, ActiveInputPage::OnNativeEvent )
	EVT_VALUEMATRIXBUTTON( wxID_ANY, ActiveInputPage::OnNativeEvent )
	EVT_LIBRARYCTRL( wxID_ANY, ActiveInputPage::OnNativeEvent )
	EVT_TABLEDATA( wxID_ANY, ActiveInputPage::OnNativeEvent )
	EVT_TOOLTIPCTRL(wxID_ANY, ActiveInputPage::OnNativeEvent)

	EVT_ERASE_BACKGROUND( ActiveInputPage::OnErase )
	EVT_PAINT( ActiveInputPage::OnPaint )
END_EVENT_TABLE()

ActiveInputPage::ActiveInputPage( wxWindow *parent, wxUIFormData *form, CaseWindow *cw,
	int id, const wxPoint &pos, const wxSize &size )
	: wxPanel( parent, id, pos, size, wxTAB_TRAVERSAL|wxCLIP_CHILDREN ),
  	m_formData( form ), m_cwin(cw), m_case(cw->GetCase())
{
	m_scaleX = m_scaleY = 1.0;
	UpdateScale( NULL );

	Show( false ); // by default form is not visible to hide the attach process

	SetBackgroundColour( *wxWHITE );
	SetBackgroundStyle( wxBG_STYLE_PAINT );

	m_formDataOwned = false;

	if ( m_formData == 0 )
	{
		m_formData = new wxUIFormData;
		m_formDataOwned = true;
	}

	m_formData->Attach( this );
	SetClientSize( ScaleSize( m_formData->GetSize() ) );
}

ActiveInputPage::~ActiveInputPage()
{
	// explicitly detach so that
	// destructors don't get mixed up twice deleting "owned"
	// native objects.

	m_formData->Detach();

	if ( m_formDataOwned )
		delete m_formData;
}


wxRect ActiveInputPage::ScaleRect( const wxRect &r )
{
	return wxRect((int)(r.x * m_scaleX), (int)(r.y * m_scaleY),
		(int)(r.width * m_scaleX), (int)(r.height * m_scaleY));
}

wxSize ActiveInputPage::ScaleSize( const wxSize &s )
{
	return wxSize( (int)(s.x*m_scaleX), (int)(s.y*m_scaleY) );
}

void ActiveInputPage::UpdateScale( wxDC *dc )
{
	
	wxSize dpi;
	
    if ( NULL != dc ) {
		dpi = dc->GetPPI();
    }
	else {
		wxClientDC dc(this);
		dpi = dc.GetPPI();
	}
    
	wxDevicePPIToScale( dpi, &m_scaleX, &m_scaleY );
}

bool ActiveInputPage::LoadFile( const wxString &file )
{
	m_formData->Detach();

	wxFFileInputStream is( file );
	if ( is.IsOk() && m_formData->Read( is ) )
	{
		m_formData->Attach( this );
		SetClientSize( ScaleSize(m_formData->GetSize()) ); // resize self to specified form data
		return true;
	}
	return false;
}

// moved to widgets.h
//static wxColour UIColorIndicatorFore(60,60,60);
//static wxColour UIColorIndicatorBack(230,230,230);
//static wxColour UIColorCalculatedFore(29,80,173);
//static wxColour UIColorCalculatedBack(222,233,244);

void ActiveInputPage::Initialize()
{
	VarInfoLookup &vdb = GetVariables();
	VarTable &vals = GetValues();

	// "fix" for controls dependent on analysis period
	size_t analysis_period = 0;
	VarValue* vv_ap = m_case->Values().Get("analysis_period");
	if (vv_ap) analysis_period = vv_ap->Integer();


	std::vector<wxUIObject*> objs = m_formData->GetObjects();
	for( size_t i=0;i<objs.size();i++ )
	{
		wxString type = objs[i]->GetTypeName();
		wxString name = objs[i]->GetName();
		wxString toolTip = objs[i]->GetTip();
		if ( VarInfo *vv = vdb.Lookup( name ) )
		{
			objs[i]->SetTip(toolTip);
			
			if ( vv->Type == VV_NUMBER && vv->IndexLabels.size() > 0 
				&& ( type == "Choice" || type == "ListBox" || type == "CheckListBox" || type == "RadioChoice" ) )
			{
				objs[i]->Property( "Items" ).SetNamedOptions( vv->IndexLabels, 0 );
				// RadioChoice not a wxItemContainer descendant
				if (wxItemContainer *ic = objs[i]->GetNative<wxItemContainer>())
				{
					ic->Clear();
					ic->Append(vv->IndexLabels);
				}
				else if (wxRadioChoice *rc = objs[i]->GetNative<wxRadioChoice>())
				{
					rc->Clear();
					rc->Add(vv->IndexLabels);
				}
			}

			if ( vv->Type == VV_TABLE && vv->IndexLabels.size() > 0 
				&& ( type == "TableData" ) )
			{
				objs[i]->Property( "Fields" ).Set( vv->IndexLabels );
				if ( AFTableDataCtrl *td = objs[i]->GetNative<AFTableDataCtrl>() )
					td->SetFields( vv->IndexLabels );
			}


			wxTextCtrl *tc = objs[i]->GetNative<wxTextCtrl>();
			if ( tc != 0 && (vv->Flags & VF_CALCULATED || vv->Flags & VF_INDICATOR ) )
			{
				tc->SetEditable( false );
				/*if ( vv->Flags & VF_CALCULATED )
				{*/
					tc->SetForegroundColour(UIColorCalculatedFore);
					tc->SetBackgroundColour(UIColorCalculatedBack);
					/*
				}
				else 
				{
					tc->SetForegroundColour(UIColorIndicatorFore);
					tc->SetBackgroundColour(UIColorIndicatorBack);
				}*/
			}

			if ( vv->Type == VV_STRING && vv->Flags & VF_LIBRARY 
				&& type == "SearchListBox" && vv->IndexLabels.size() == 2 )
			{
				if ( Library *lib = Library::Find(vv->IndexLabels[0]) )
				{
					if ( AFSearchListBox *slb = objs[i]->GetNative<AFSearchListBox>() )
					{
						slb->Clear();
						slb->Append( lib->ListEntries() );
					}
				}
			}

			if ( VarValue *vval = vals.Get( name ) )
				DataExchange( objs[i], *vval, VAR_TO_OBJ, analysis_period, m_case->m_oldAnalysisPeriod );
		}
	}

	// lookup and run any callback functions.
	if ( lk::node_t *root = m_case->QueryCallback( "on_load", m_formData->GetName() ) )
	{
		UICallbackContext cbcxt( this, m_formData->GetName() + "->on_load" );
		if ( cbcxt.Invoke( root, &m_case->CallbackEnvironment() ) )
		  {
			wxLogStatus("callback script " + m_formData->GetName() + "->on_load succeeded");
		  }
	}
}

wxUIObject *ActiveInputPage::Find( const wxString &name )
{
	return m_formData->Find( name );
}

wxUIObject *ActiveInputPage::FindActiveObject( const wxString &name, ActiveInputPage **page )
{
	return m_cwin->FindActiveObject( name, page );
}

std::vector<wxUIObject*> ActiveInputPage::GetObjects() { return m_formData->GetObjects(); }
VarInfoLookup &ActiveInputPage::GetVariables()	{ return m_case->Variables(); }
EqnFastLookup &ActiveInputPage::GetEquations() { return m_case->Equations(); }
VarTable &ActiveInputPage::GetValues() { return m_case->Values(); }
Case *ActiveInputPage::GetCase() { return m_case; }
CaseWindow *ActiveInputPage::GetCaseWindow() { return m_cwin; }
void ActiveInputPage::OnErase( wxEraseEvent & )
{
	/* nothing to do */
}

void ActiveInputPage::OnPaint( wxPaintEvent & )
{
	wxAutoBufferedPaintDC dc( this );	
	UpdateScale( &dc );

	dc.SetBackground( GetBackgroundColour() );
	dc.Clear();

	VarInfoLookup &vdb = GetVariables();

	wxRect rct;
	std::vector<wxUIObject*> objs = m_formData->GetObjects();
	for ( int i=(int)objs.size()-1;i>=0;i-- )
	{
		// draw any non-native objects
		if ( !objs[i]->IsNativeObject() )
		{
			rct = ScaleRect(objs[i]->GetGeometry());
			objs[i]->Draw( this, dc, rct );
			dc.DestroyClippingRegion();
		}

		if ( VarInfo *vv = vdb.Lookup( objs[i]->GetName() ) )
		{	// fix this
			int sw, sh;
			if (!objs[i]->IsVisible())
			{
				rct = ScaleRect(objs[i]->GetGeometry());
				wxString blk = "";
				if (vv->Label.Len() > 0)
				{
					blk = "";
					blk.Pad(vv->Label.Len());
					dc.GetTextExtent(vv->Label, &sw, &sh);
					dc.DrawText(blk, rct.x - sw - 3, rct.y + rct.height / 2 - sh / 2);
				}

				if (vv->Units.Len() > 0)
				{
					blk = "";
					blk.Pad(vv->Units.Len());
					dc.GetTextExtent(vv->Units, &sw, &sh);
					dc.DrawText(blk, rct.x + rct.width + 2, rct.y + rct.height / 2 - sh / 2);
				}
			}
			else if ( !(vv->Flags & VF_HIDE_LABELS) && objs[i]->IsVisible() )
			{
				dc.SetFont(*wxNORMAL_FONT);
				wxColour colour( *wxBLACK );
				dc.SetTextForeground( colour );
				rct = ScaleRect(objs[i]->GetGeometry());

				if ( vv->Label.Len() > 0 )
				{
					dc.GetTextExtent( vv->Label, &sw, &sh);
					dc.DrawText( vv->Label, rct.x - sw - 3, rct.y+ rct.height/2-sh/2);
				}

				if ( vv->Units.Len() > 0 )
				{
					dc.GetTextExtent( vv->Units, &sw, &sh);
					dc.DrawText( vv->Units, rct.x + rct.width + 2, rct.y+ rct.height/2-sh/2);
				}
			}
		}
	}
}

// handler(s) for child widget changes
void ActiveInputPage::OnNativeEvent( wxCommandEvent &evt )
{
	wxUIObject *obj = 0;
	std::vector<wxUIObject*> objs = m_formData->GetObjects();
	for( size_t i=0;i<objs.size();i++ )
	{
		if ( evt.GetEventObject() == objs[i]->GetNative() )
		{
			obj = objs[i];
			break;
		}
	}

	if ( obj == 0 ) 
	{
		// couldn't find the ui object, some sort of spurious event??
		wxLogStatus( "unhandled input page event!");
		return;
	}


	// this method will sync any changes in the native control
	// to the properties stored in the wxUIObject base.
	obj->OnNativeEvent(); 

	// transfer the data from the UI object to the variable (DDX) 
	// then notify the case that the variable was changed
	// within the case, the calculations will be redone as needed
	// and then the casewindow will be notified by event that
	// other UI objects (calculated ones) need to be updated
	if( VarValue *vval = GetValues().Get( obj->GetName() ) )
	{
		// tracking analysis period changes to update analysis period dependent widgets
		if (obj->GetName() == "analysis_period")
			m_case->m_oldAnalysisPeriod = vval->Integer();
		if ( DataExchange( obj, *vval, OBJ_TO_VAR ) )
		{
			wxLogStatus( "Variable " + obj->GetName() + " changed by user interaction, case notified." );
			
			m_case->Recalculate( obj->GetName() );

			// send value changed whenever recalculate is called to update other windows
			// for example the VariableGrid
			m_case->SendEvent(CaseEvent(CaseEvent::VALUE_USER_INPUT, obj->GetName()));
		}
		else
			wxMessageBox("ActiveInputPage >> data exchange fail: " + obj->GetName() );
	}

	// lookup and run any callback functions, even if no data was exchanged.
	if ( lk::node_t *root = m_case->QueryCallback( "on_change", obj->GetName() ) )
	{
		UICallbackContext cbcxt( this, obj->GetName() + "->on_change" );
		if ( cbcxt.Invoke( root, &m_case->CallbackEnvironment() ) )
		  {
			wxLogStatus("callback script " + obj->GetName() + "->on_change succeeded");
		  }
	}
}

bool ActiveInputPage::DataExchange( wxUIObject *obj, VarValue &val, DdxDir dir, size_t AnalysisPeriod, size_t AnalysisPeriodOld)
{
	if ( wxNumericCtrl *num = obj->GetNative<wxNumericCtrl>() )
	{
		if( dir == VAR_TO_OBJ )	num->SetValue( val.Value() );
		else val.Set( num->Value() );
	}
	else if ( wxItemContainerImmutable *ici = obj->GetNative<wxItemContainerImmutable>() )
	{
		// handles:  wxListBox, wxCheckListBox, wxChoice and wxComboBox
		if( dir == VAR_TO_OBJ )
		{
			if ( val.Type() == VV_STRING ) ici->SetStringSelection( val.String() );
			else ici->SetSelection( val.Integer() );
		}
		else
		{
			if ( val.Type() == VV_STRING ) val.Set( ici->GetStringSelection() );
			else val.Set( ici->GetSelection() );
		}
	}
	else if ( wxCheckBox *chk = obj->GetNative<wxCheckBox>() )
	{
		if ( dir == VAR_TO_OBJ ) chk->SetValue( val.Integer() ? true : false );
		else val.Set( chk->GetValue() ? 1 : 0 );
	}
	else if ( wxRadioChoice *rdc = obj->GetNative<wxRadioChoice>() )
	{
		if ( dir == VAR_TO_OBJ ) rdc->SetSelection( val.Integer() );
		else val.Set( rdc->GetSelection() );
	}
	else if ( wxTextCtrl *txt = obj->GetNative<wxTextCtrl>() )
	{
		if ( dir == VAR_TO_OBJ ) txt->SetValue( val.String() );
		else val.Set( txt->GetValue() );
	}
	else if( wxSlider *sli = obj->GetNative<wxSlider>() )
	{
		if ( dir == VAR_TO_OBJ ) sli->SetValue( val.Integer() );
		else val.Set( sli->GetValue() );
	}
	else if( AFSchedNumeric *sn = obj->GetNative<AFSchedNumeric>() )
	{
		if ( dir == VAR_TO_OBJ )
		{
			std::vector<double> vals = val.Array();
			bool bScheduleOnly = obj->Property("ScheduleOnly").GetBoolean();
			if (!bScheduleOnly)
				sn->UseSchedule( vals.size() > 1 );
			else
				sn->UseSchedule( true );

			if (vals.size() > 0)
			{
				sn->SetValue( vals[0] );
				if (vals.size() > 1)
					sn->SetSchedule( vals );
			}
		}
		else
		{
			std::vector<double> vals;
			if ((sn->UseSchedule()) || (sn->ScheduleOnly()))
				sn->GetSchedule( &vals );
			else
				vals.push_back( sn->GetValue());

			val.Set( &vals[0], vals.size() );
		}
	} 
	else if ( PTLayoutCtrl *pt = obj->GetNative<PTLayoutCtrl>() )
	{
		if ( dir == VAR_TO_OBJ )
		{ 
			VarTable &tab = val.Table();
			if ( VarValue *v = tab.Get("grid") ) pt->SetGrid( v->Matrix() );
			if ( VarValue *v = tab.Get("span") ) pt->SetSpanAngle( v->Value() );
		}
		else
		{
			VarTable tab;
			tab.Set( "grid", VarValue( pt->GetGrid() ) );
			tab.Set( "span", VarValue( (float)pt->GetSpanAngle()) );
			val.Set( tab );
		}
	}
	else if ( MatPropCtrl *mp = obj->GetNative<MatPropCtrl>() )
	{
		if ( dir == VAR_TO_OBJ ) mp->SetData( val.Matrix() );
		else val.Set( mp->GetData() ); 
	}
	else if ( TRLoopCtrl *tr = obj->GetNative<TRLoopCtrl>() )
	{
		if ( dir == VAR_TO_OBJ ) tr->LoopData( val.IntegerArray() );
		else val.Set( tr->LoopData() );
	}
	else if ( AFMonthlyFactorCtrl *mf = obj->GetNative<AFMonthlyFactorCtrl>() )
	{
		if ( dir == VAR_TO_OBJ ) mf->Set( val.Array() );
		else val.Set( mf->Get() );
	}
	else if (AFDataArrayButton *da = obj->GetNative<AFDataArrayButton>())
	{
		if (dir == VAR_TO_OBJ) da->Set(val.Array());
		else val.Set(da->Get());
	}
	else if (AFDataLifetimeArrayButton *dl = obj->GetNative<AFDataLifetimeArrayButton>())
	{
	if (dir == VAR_TO_OBJ) dl->Set(val.Array());
	else val.Set(dl->Get());
	}
	else if (AFDataLifetimeMatrixButton *dl = obj->GetNative<AFDataLifetimeMatrixButton>())
	{
		if (dir == VAR_TO_OBJ) {
			dl->Set(val.Matrix(), AnalysisPeriod, AnalysisPeriodOld);
		}
		else val.Set(dl->Get());
	}
	else if (AFStringArrayButton *sa = obj->GetNative<AFStringArrayButton>())
	{
		if (dir == VAR_TO_OBJ) sa->Set(val.String());
		else val.Set(sa->Get());
	}
	else if (AFDataMatrixCtrl *dm = obj->GetNative<AFDataMatrixCtrl>())
	{
		if (dir == VAR_TO_OBJ) dm->SetData(val.Matrix());
		else val.Set(dm->GetData());
	}
	else if (AFMonthByHourFactorCtrl *dm = obj->GetNative<AFMonthByHourFactorCtrl>())
	{
		if ( dir == VAR_TO_OBJ ) dm->SetData( val.Matrix() );
		else val.Set( dm->GetData() );
	}
	else if ( ShadingButtonCtrl *sb = obj->GetNative<ShadingButtonCtrl>() )
	{
		if ( dir == VAR_TO_OBJ ) sb->Read( &val );
		else sb->Write( &val );
	}
	else if ( AFValueMatrixButton *vm = obj->GetNative<AFValueMatrixButton>() )
	{
		if ( dir == VAR_TO_OBJ ) vm->Set( val.Matrix() );
		else val.Set( vm->Get() );
	}
	else if ( AFSearchListBox *slb = obj->GetNative<AFSearchListBox>() )
	{
		if ( dir == VAR_TO_OBJ )
		{
			if ( !slb->SetStringSelection( val.String() ) )
				wxMessageBox( "Error: '" + val.String() + "' was not found among the available choices.  Please make a different selection before proceeding.", "Error", wxOK|wxICON_WARNING );
		}
		else val.Set( slb->GetStringSelection() );
	}
	else if ( LibraryCtrl *ll = obj->GetNative<LibraryCtrl>() )
	{
		if ( dir == VAR_TO_OBJ )
		{
			if ( !ll->SetEntrySelection(val.String()))
			if ((ll->GetLibrary() != "SolarResourceData") && !ll->SetEntrySelection( val.String() ))
				wxMessageBox(  "Error: '" + val.String() + "' is not available in the library. Choose a different item.", "Error", wxOK|wxICON_WARNING );
		}
		else val.Set( ll->GetEntrySelection() );
	}
	else if ( AFLossAdjustmentCtrl *la = obj->GetNative<AFLossAdjustmentCtrl>() )
	{
		if ( dir == VAR_TO_OBJ ) la->Read( &val );
		else la->Write( &val );
	}
	else if ( wxDiurnalPeriodCtrl *dp = obj->GetNative<wxDiurnalPeriodCtrl>())
	{
		if ( val.Type() == VV_STRING )
		{
			if ( dir == VAR_TO_OBJ ) dp->Schedule( val.String() );
			else val.Set( dp->Schedule() );
		}
		else if ( val.Type() == VV_MATRIX )
		{
			size_t nr, nc;
			double *p;
			if (dir == VAR_TO_OBJ)
			{
				p = val.Matrix( &nr, &nc );
				dp->SetData(p, nr, nc );
			}
			else
			{
				// fundamental incompatability in current function requirements
				// upcasting is pointless, already at a float precision.
				p = dp->GetData( &nr, &nc );
				val.Set(p, nr, nc );
			}
		}
	}
	else if ( AFTableDataCtrl *td = obj->GetNative<AFTableDataCtrl>() )
	{
		if ( val.Type() == VV_TABLE )
		{
			VarTable &T = val.Table();
			if ( dir == VAR_TO_OBJ ) {
				wxArrayString list = T.ListAll();
				for( size_t i=0;i<list.size();i++ )
					td->Set( list[i], T.Get(list[i])->Value() );
			}
			else
			{
				T.clear();
				wxArrayString list = td->GetFields();
				for( size_t i=0;i<list.size();i++ )
					T.Set( list[i], VarValue( (float)td->Get(list[i]) ) );
			}
		}
	}
	else return false; // object data exch not handled for this type

	return true;  // all ok!
}

UIFormDatabase::UIFormDatabase()
{
}

UIFormDatabase::~UIFormDatabase()
{
	Clear();
}

void UIFormDatabase::Clear()
{
	for ( FormDataHash::iterator it = m_hash.begin();
		it != m_hash.end();
		++it )
		delete (*it).second;

	m_hash.clear();
}

void UIFormDatabase::Add( const wxString &name, wxUIFormData *ui )
{
	FormDataHash::iterator it = m_hash.find( name );
	if ( it != m_hash.end() )
	{
		delete it->second;
		it->second = ui;
	}
	else
		m_hash[ name ] = ui;
}

wxUIFormData *UIFormDatabase::Lookup( const wxString &name )
{
	FormDataHash::iterator it = m_hash.find( name );
	return ( it != m_hash.end() ) ? it->second : NULL;
}

