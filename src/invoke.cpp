#include <algorithm>
#include <wx/log.h>

#include <wex/plot/plplotctrl.h>
#include <wex/lkscript.h>
#include <wex/dview/dvplotctrl.h>

#ifdef __WXMSW__
#include <wex/ole/excelauto.h>
#endif

#include <ssc/sscapi.h>

#include "main.h"
#include "case.h"
#include "casewin.h"
#include "materials.h" // used to call functions for substance density and specific heat

#include "invoke.h"

static void fcall_dview(lk::invoke_t &cxt)
{
	LK_DOC("dview", "Creates a separate dview viewer for viewing specified data.", "( number:num_datasets, , number:timestep, string:window name, string:data_name1, string:data_units1, number:multiplier1, variant:data1, [ string:data_name2, string:data_units2, number:multiplier2, variant:data2], ...):none");


	int num_datasets = (int)cxt.arg(0).as_number();
	double timestep = cxt.arg(1).as_number(); // fraction of hour
	wxString win_name = cxt.arg(2).as_string();

	size_t data_length = 8760;
	if (timestep > 0)
		data_length /= timestep;

	size_t ndx = 3;
	if ((4 * num_datasets + ndx) != cxt.arg_count()) return;

	wxDialog *frame;
	wxDVPlotCtrl *dview;
	if (wxWindow::FindWindowByLabel(win_name))
	{
		frame = (wxDialog*)wxWindow::FindWindowByLabel(win_name);
		dview = (wxDVPlotCtrl *)frame->FindWindowByName("DVIEW_" + win_name);
	}
	else
	{
		frame = new wxDialog(wxTheApp->GetTopWindow(), wxID_ANY, win_name, wxDefaultPosition, wxSize(900, 700));
		dview = new wxDVPlotCtrl(frame, wxID_ANY);
		dview->SetName("DVIEW_" + win_name);
	}
	// reset data - an compare each dataset if changed instead
	dview->RemoveAllDataSets();

	while (ndx < cxt.arg_count())
	{ 
		wxString data_name = cxt.arg(ndx++).as_string();
		wxString data_units = cxt.arg(ndx++).as_string();
		double mult = cxt.arg(ndx++).as_number();
		lk::vardata_t &data = cxt.arg(ndx++);
		if (data.length() != data_length) return;

		std::vector<double> plot_data(8760);
		for (size_t i = 0; i < 8760; i++)
			plot_data[i] = mult * data.index(i)->as_number();

		dview->AddDataSet(new wxDVArrayDataSet(data_name, data_units, timestep, plot_data));
	}

	dview->SelectDataIndex(0);

	frame->Show();
}

static void fcall_logmsg( lk::invoke_t &cxt )
{
	LK_DOC("logmsg", "Output a data line to the SAM log.", "(...):none");	
	wxString output;
	for (size_t i=0;i<cxt.arg_count();i++)
		output += cxt.arg(i).as_string();
	wxLogStatus( output );
}

static void fcall_browse( lk::invoke_t &cxt )
{
	LK_DOC("browse", "Open a URL, local file, or folder using the default browser.", "(string:url):none");
	::wxLaunchDefaultBrowser( cxt.arg(0).as_string() );
}

static void fcall_appdir( lk::invoke_t &cxt )
{
	LK_DOC("appdir", "Return the application folder.", "(none):string");
	cxt.result().assign( SamApp::GetAppPath() );
}
static void fcall_runtimedir( lk::invoke_t &cxt )
{
	LK_DOC("runtimedir", "Return the application runtime data folder.", "(none):string");
	cxt.result().assign( SamApp::GetRuntimePath() );
}
static void fcall_userlocaldatadir( lk::invoke_t &cxt )
{
	LK_DOC("userlocaldatadir", "Return the user's local data folder in their home folder.", "(none):string");
	cxt.result().assign( SamApp::GetUserLocalDataDir() );
}

static void fcall_addconfig( lk::invoke_t &cxt )
{
	LK_DOC("addconfig", "Add a technology+financing options", "( string:tech, array:financings ):none" );

	wxArrayString finlist;
	lk::vardata_t &fins = cxt.arg(1);
	for( size_t i=0;i<fins.length();i++ )
		finlist.Add( fins.index(i)->as_string() );
	
	SamApp::Config().Add( cxt.arg(0).as_string(), finlist );

	wxLogStatus( "Configuration: " + cxt.arg(0).as_string() + "  -> [ " + wxJoin(finlist,';') + " ]" );
}

static void fcall_setconfig( lk::invoke_t &cxt )
{
	LK_DOC("setconfig", "Sets the currently active configuration for editing", "(string:Tech, string:Financing):none");
	SamApp::Config().SetConfig( cxt.arg(0).as_string(), cxt.arg(1).as_string() );
}

static void fcall_setmodules( lk::invoke_t &cxt )
{
	LK_DOC("setmodules", "Sets the simulation models for the currently active configuration", "(array:module names):none");
	
	wxArrayString list;
	lk::vardata_t &m = cxt.arg(0);
	for( size_t i=0;i<m.length();i++ )
		list.Add( m.index(i)->as_string() );

	SamApp::Config().SetModules( list );
}

static void fcall_addpage( lk::invoke_t &cxt )
{
	LK_DOC("addpage", "Add an input page group to the currently active configuration (may have multiple pages).", "(array:pages, table:caption,help,exclusive,exclusive_var):none" );
	
	std::vector< std::vector<PageInfo> > pages;
	lk::vardata_t &grps = cxt.arg(0);
	for( size_t i=0;i<grps.length();i++ )
	{
		pages.push_back( std::vector<PageInfo>() );

		for( size_t j=0;j<grps.index(i)->deref().length();j++ )
		{
			PageInfo x;

			lk::vardata_t &item = grps.index(i)->deref().index(j)->deref();

			if ( item.type() == lk::vardata_t::HASH )
			{
				if ( lk::vardata_t *name = item.lookup( "name" ) )
					x.Name = name->as_string();

				x.Caption = x.Name; // by default, caption=name
				x.CollapsedByDefault = true;
			
				if ( lk::vardata_t *capt = item.lookup( "caption" ) )
					x.Caption = capt->as_string();

				if ( lk::vardata_t *is_collap = item.lookup( "collapsible" ) )
					x.Collapsible = is_collap->as_boolean();

				if ( lk::vardata_t *var = item.lookup( "collapsible_var" ) )
					x.CollapsiblePageVar = var->as_string();

				if ( lk::vardata_t *initial = item.lookup( "collapsed_by_default" ) )
					x.CollapsedByDefault = initial->as_boolean();

				if ( lk::vardata_t *shlabel = item.lookup( "label" ) )
					x.ShowHideLabel = shlabel->as_string();
			}
			else
			{
				x.Name = item.as_string();
				x.Caption = x.Name;
			}

			if ( !x.Name.IsEmpty() )
				pages[i].push_back( x );
		}
	}

	if ( pages.size() == 0 || pages[0].size() == 0 ) return;
	
	wxString sidebar = pages[0][0].Name;
	wxString help = sidebar;
	wxString exclusive_var;
	
	if ( cxt.arg_count() > 1 )
	{
		lk::vardata_t &props = cxt.arg(1).deref();

		if( lk::vardata_t *x = props.lookup("sidebar") )
			sidebar = x->as_string();

		if ( lk::vardata_t *x = props.lookup("help") )
			help = x->as_string();
		
		if ( lk::vardata_t *x = props.lookup("exclusive_var") )
			exclusive_var = x->as_string();
	}

	SamApp::Config().AddInputPageGroup( pages, sidebar, help, exclusive_var );
}

static void fcall_setting( lk::invoke_t &cxt )
{
	LK_DOC( "setting", "Sets a setting field for the current configuration", "(string:name, string:value -or- table:name/value pairs):none");

	if ( ConfigInfo *ci = SamApp::Config().CurrentConfig() )
	{
		if( cxt.arg_count() == 2 )
			ci->Settings[ cxt.arg(0).as_string() ] = cxt.arg(1).as_string();
		else if ( cxt.arg_count() == 1 && cxt.arg(0).deref().type() == lk::vardata_t::HASH )
		{
			lk::varhash_t *hash = cxt.arg(0).deref().hash();
			for( lk::varhash_t::iterator it = hash->begin();
				it != hash->end();
				++it )
				ci->Settings[ it->first ] = it->second->deref().as_string();
		}
	}
}

static void fcall_technology( lk::invoke_t &cxt )
{
	LK_DOC( "technology", "Return the current technology option name", "(void):string" );
	CaseCallbackContext &cc = *static_cast<CaseCallbackContext*>( cxt.user_data() ); 
	cxt.result().assign( cc.GetCase().GetTechnology() );
}

static void fcall_financing( lk::invoke_t &cxt )
{
	LK_DOC( "financing", "Return the current financing option name", "(void):string" );
	CaseCallbackContext &cc = *static_cast<CaseCallbackContext*>( cxt.user_data() ); 
	cxt.result().assign( cc.GetCase().GetFinancing() );
}

static void fcall_metric( lk::invoke_t &cxt )
{
	LK_DOC("metric", "Add an output metric to the current configuration. Options include mode,deci,thousep,pre,post,label,scale", "(string:variable, [table:options]):none");
	 
	if ( CaseCallbackContext *ci = static_cast<CaseCallbackContext*>(cxt.user_data()) )
	{
		CaseCallbackContext::MetricData md;
		md.var = cxt.arg(0).as_string();
		
		if (cxt.arg_count() > 1 )
		{
			lk::vardata_t &opts = cxt.arg(1).deref();
			if ( lk::vardata_t *x = opts.lookup("mode") )
			{
				wxString mm = x->as_string();
				mm.MakeLower();
				if ( mm == "f" ) md.mode = 'f';
				else if ( mm == "e" ) md.mode = 'e';
				else if ( mm == "h" ) md.mode = 'h';			
			}

			if ( lk::vardata_t *x = opts.lookup("deci") )
				md.deci = x->as_integer();

			if ( lk::vardata_t *x = opts.lookup("thousep") )
				md.thousep = x->as_boolean();

			if ( lk::vardata_t *x = opts.lookup("pre") )
				md.pre = x->as_string();

			if ( lk::vardata_t *x = opts.lookup("post") )
				md.post = x->as_string();

			if ( lk::vardata_t *x = opts.lookup("label") )
				md.label = x->as_string();

			if ( lk::vardata_t *x = opts.lookup("scale") )
				md.scale = x->as_number();
		}

		ci->Metrics.push_back( md );
	}
}

static void fcall_cfline( lk::invoke_t &cxt )
{
	LK_DOC("cfline", "Add one or more cashflow line items to the current configuration. Names can be comma-separated list. For spacer use name='', for header use digits=0, for generic format use digits=-1, for integer cast, use digits=-2.", "( string:names, [number:digits], [number:scale] ):none" );
	
	if ( CaseCallbackContext *ci = static_cast<CaseCallbackContext*>(cxt.user_data()) )
	{
		wxString name = cxt.arg(0).as_string();

		int type = CaseCallbackContext::CashFlowLine::VARIABLE;
		int digit = 2;
		float scale = 1.0f;
		if ( cxt.arg_count() == 2 )
			digit = cxt.arg(1).as_integer();

		if ( cxt.arg_count() == 3 )
			scale = (float)cxt.arg(2).as_number();

		if ( name.IsEmpty() ) type = CaseCallbackContext::CashFlowLine::SPACER;
		if ( digit == 0 ) type = CaseCallbackContext::CashFlowLine::HEADER;
		
		wxArrayString list = wxSplit(name, ',');
		if ( list.size() == 0 ) list.Add( name ); // handle empty string

		for( size_t i=0;i<list.Count();i++ )
		{
			CaseCallbackContext::CashFlowLine cl;
			cl.type = type;
			cl.digits = digit;
			cl.name = list[i];
			cl.scale = scale;
			ci->CashFlow.push_back( cl );
		}
	}
}


void fcall_value( lk::invoke_t &cxt )
{
	LK_DOC("value", "Gets or sets the value of a variable by name", "(string:name [,variant:value]):[variant]");
	
	CaseCallbackContext &cc = *(CaseCallbackContext*)cxt.user_data();
	wxString name = cxt.arg(0).as_string();
	if ( VarValue *vv = cc.GetValues().Get( name ) )
	{
		if ( cxt.arg_count() == 2 )
		{
			vv->Read( cxt.arg(1) );
			cc.GetCase().VariableChanged( name );		
		}
		else
		{
			vv->Write( cxt.result() );
		}
	}
}

	
static void plottarget( UICallbackContext &cc, const wxString &name )
{
	wxLKSetPlotTarget( 0 );
	if ( wxUIObject *obj = cc.InputPage()->Find(name) )
		if ( wxPLPlotCtrl *plot = obj->GetNative<wxPLPlotCtrl>() )
			wxLKSetPlotTarget( plot );
}

void fcall_setplot( lk::invoke_t &cxt )
{
	LK_DOC("setplot", "Sets the current plot target by name", "(string:name):boolean");
	
	plottarget( *(UICallbackContext*)cxt.user_data(), cxt.arg(0).as_string() );
}

void fcall_clearplot( lk::invoke_t &cxt )
{
	LK_DOC("clearplot", "Clears the current plot, and optionally switches the plot target.", "([string:plot name]):none");
	
	if (cxt.arg_count() > 0)
		plottarget( *(UICallbackContext*)cxt.user_data(), cxt.arg(0).as_string() );

	if ( wxPLPlotCtrl *plot = wxLKGetPlotTarget() )
	{
		plot->DeleteAllPlots();
		plot->Refresh();
	}
}

void fcall_refresh( lk::invoke_t &cxt )
{
	LK_DOC("refresh", "Refresh the current form or a specific widget", "([string:name]):none" );
	
	UICallbackContext &cc = *(UICallbackContext*)cxt.user_data();
	if ( cxt.arg_count() == 0 )
		cc.InputPage()->Refresh();
	else
	{
		if ( wxUIObject *obj = cc.InputPage()->FindActiveObject( cxt.arg(0).as_string(), 0 ) )
			if ( wxWindow *win = obj->GetNative() )
				win->Refresh();
	}
}

void fcall_property( lk::invoke_t &cxt )
{
	LK_DOC("property", "Set or get a user interface widget property", "(string:name, string:property[, variant:value]):variant");

	UICallbackContext &cc = *(UICallbackContext*)cxt.user_data();
	ActiveInputPage *aip = 0;
	wxUIObject *obj = cc.InputPage()->FindActiveObject( cxt.arg(0).as_string(), &aip );
	if ( !obj ) return;
	if ( !obj->HasProperty( cxt.arg(1).as_string() ) ) return;

	wxUIProperty &p = obj->Property( cxt.arg(1).as_string() );

	if ( cxt.arg_count() == 2 )
	{
		switch( p.GetType() )
		{
		case wxUIProperty::BOOLEAN:
			cxt.result().assign( (double) p.GetBoolean() ? 1.0 : 0.0 );
			break;
		case wxUIProperty::DOUBLE:
			cxt.result().assign( p.GetDouble() );
			break;
		case wxUIProperty::INTEGER:
			cxt.result().assign( (double)p.GetInteger() );
			break;
		case wxUIProperty::COLOUR:
			cxt.result().empty_vector();
			cxt.result().vec_append( p.GetColour().Red() );
			cxt.result().vec_append( p.GetColour().Green() );
			cxt.result().vec_append( p.GetColour().Blue() );
			break;
		case wxUIProperty::STRING:
			cxt.result().assign( p.GetString() );
			break;
		case wxUIProperty::STRINGLIST:
		{
			cxt.result().empty_vector();
			wxArrayString list = p.GetStringList();
			for( size_t i=0;i<list.size();i++ )
				cxt.result().vec_append( list[i] );
		}
			break;
		default:
			break;
		}
	}
	else if ( cxt.arg_count() == 3 )
	{
		lk::vardata_t &val = cxt.arg(2).deref();
		switch( p.GetType() )
		{
		case wxUIProperty::BOOLEAN:
			p.Set( (bool) val.as_boolean() );
			break;
		case wxUIProperty::DOUBLE:
			p.Set( (double) val.as_number() );
			break;
		case wxUIProperty::INTEGER:
			p.Set( (int) val.as_integer() );
			break;
		case wxUIProperty::COLOUR:
			if ( val.type() == lk::vardata_t::VECTOR
				&& val.length() == 3 )
			{
				p.Set( wxColour(
					val.index(0)->as_integer(),
					val.index(1)->as_integer(),
					val.index(2)->as_integer() ) );
			}
			else
			{
				p.Set( wxColour( val.as_string() ) );
			}
			break;
		case wxUIProperty::STRING:
			p.Set( val.as_string() );
			break;
		case wxUIProperty::STRINGLIST:
			if ( val.type() == lk::vardata_t::VECTOR )
			{
				wxArrayString list;
				for( size_t i=0;i<val.length();i++ )
					list.Add( val.index(i)->as_string() );
				p.Set( list );
			}
			break;
		}

		if ( !obj->IsNativeObject() && aip != 0 )
			aip->Refresh(); // repaint the active input page if the object is non-nnative.
	}
}

void fcall_enable( lk::invoke_t &cxt )
{
	LK_DOC("enable", "Enable or disable a user interface widget", "(string:name, boolean:enable):none");

	UICallbackContext &cc = *(UICallbackContext*)cxt.user_data();
	if ( wxUIObject *obj = cc.InputPage()->FindActiveObject( cxt.arg(0).as_string(), 0 ) )
		if ( wxWindow *native = obj->GetNative() )
			native->Enable( cxt.arg(1).as_boolean() );
}
static void fcall_show( lk::invoke_t &cxt )
{
	LK_DOC("show", "Show or hide a user interface widget.", "(string:name, boolean:show):none");

	UICallbackContext &cc = *(UICallbackContext*)cxt.user_data();
	if ( wxUIObject *obj = cc.InputPage()->FindActiveObject( cxt.arg(0).as_string(), 0 ) )
		obj->Show( cxt.arg(1).as_boolean() );
}

#ifdef __WXMSW__

class lkXLObject : public lk::objref_t
{
	wxExcelAutomation m_xl;
public: 
	lkXLObject() { };
	virtual ~lkXLObject() { };
	virtual lk_string type_name() { return "ole-excel-automation"; }
	wxExcelAutomation &Excel() { return m_xl; }
	operator wxExcelAutomation&() { return m_xl; }
	wxExcelAutomation &operator*() { return m_xl; }
};

static void fcall_xl_create( lk::invoke_t &cxt )
{
	LK_DOC("xl_create", "Create a new Excel OLE automation object", "( [string: optional file to open] ):xl-object-ref" );

	lkXLObject *xl = new lkXLObject;
	xl->Excel().StartExcel();
	if ( cxt.arg_count() > 0 )
		xl->Excel().OpenFile( cxt.arg(0).as_string() );
	
	xl->Excel().Show( true );
	cxt.result().assign( cxt.env()->insert_object( xl ) );
}

static void fcall_xl_free( lk::invoke_t &cxt )
{
	LK_DOC("xl_free", "Frees up an Excel OLE automation object", "( xl-obj-ref:xl ):none" );
	
	if ( lkXLObject *xl = dynamic_cast<lkXLObject*>( cxt.env()->query_object( cxt.arg(0).as_integer() ) ) )
		cxt.env()->destroy_object( xl );
}

static void fcall_xl_open(lk::invoke_t &cxt)
{
	LK_DOC("xl_open", "Opens an Excel file on Windows platform.", "( xl-obj-ref:xl, string:file_name):none");

	if ( lkXLObject *xl = dynamic_cast<lkXLObject*>( cxt.env()->query_object( cxt.arg(0).as_integer() ) ) )
	{
		xl->Excel().OpenFile( cxt.arg(1).as_string() );
		xl->Excel().Show( true );
		wxString file_name = cxt.arg(0).as_string();	
	}
}

static void fcall_xl_autosizecols( lk::invoke_t &cxt )
{
	LK_DOC( "xl_autosizecols", "Automatically size columns in the current Excel file", "( xl-obj-ref:xl ):none" );
	
	if ( lkXLObject *xl = dynamic_cast<lkXLObject*>( cxt.env()->query_object( cxt.arg(0).as_integer() ) ) )
		xl->Excel().AutoFitColumns();
}

static void fcall_xl_wkbook( lk::invoke_t &cxt )
{
	LK_DOC( "xl_wkbook", "Create a new empty workbook in the Excel file", "( xl-obj-ref:xl ):none" );
	
	if ( lkXLObject *xl = dynamic_cast<lkXLObject*>( cxt.env()->query_object( cxt.arg(0).as_integer() ) ) )
		xl->Excel().NewWorkbook();
}

static void fcall_xl_sheet( lk::invoke_t &cxt )
{
	LK_DOC( "xl_sheet", "Create a new worksheet in the current workbook", "( xl-obj-ref:xl, [string: optional name] ):none");
	
	if ( lkXLObject *xl = dynamic_cast<lkXLObject*>( cxt.env()->query_object( cxt.arg(0).as_integer() ) ) )
	{
		if ( xl->Excel().AddWorksheet() && cxt.arg_count() == 2 )
			xl->Excel().SetWorksheetName( cxt.arg(1).as_string() );
	}
}

static void fcall_xl_set( lk::invoke_t &cxt )
{
	LK_DOC("xl_set", "Sets a value in an Excel file", "( xl-obj-ref:xl, variant:value, [string:named range -or- row, col] ):none");
	
	if ( lkXLObject *xl = dynamic_cast<lkXLObject*>( cxt.env()->query_object( cxt.arg(0).as_integer() ) ) )
	{	
		if ( cxt.arg_count() == 4 )
			xl->Excel().SetCellValue( cxt.arg(2).as_integer(), cxt.arg(3).as_integer(), cxt.arg(1).as_string() );
		else if ( cxt.arg_count() == 3 && cxt.arg(1).type() == lk::vardata_t::VECTOR )
		{
			wxArrayString list;
			for( size_t i=0;i<cxt.arg(1).length();i++ )
				list.Add( cxt.arg(1).index( i )->deref().as_string() );
			xl->Excel().SetNamedRangeArray( cxt.arg(2).as_string(), list );
		}
		else if ( cxt.arg_count() == 3 )
			xl->Excel().SetNamedRangeValue( cxt.arg(2).as_string(), cxt.arg(1).as_string() );
	}
}

static void fcall_xl_get( lk::invoke_t &cxt )
{
	LK_DOC( "xl_get", "Gets a value from the current Excel file", "( xl-obj-ref:xl, [string:named range -or- row, col] ):variant" );
	
	if ( lkXLObject *xl = dynamic_cast<lkXLObject*>( cxt.env()->query_object( cxt.arg(0).as_integer() ) ) )
	{	
		wxString val;
		if ( cxt.arg_count() == 2 )
			xl->Excel().GetNamedRangeValue( cxt.arg(1).as_string(), val );
		else if ( cxt.arg_count() == 3 )
			xl->Excel().GetCellValue( cxt.arg(1).as_integer(), cxt.arg(2).as_integer(), val );

		cxt.result().assign( val );
	}
}

#endif



/*
static bool sscvar_to_lkvar( lk::vardata_t &out, var_data *vv)
{
	if (!vv) return false;

	switch( vv->type )
	{
	case SSC_NUMBER:
		out.assign( (double) vv->num );
		break;
	case SSC_STRING:
		out.assign( vv->str.c_str() );
		break;
	case SSC_ARRAY:
		out.empty_vector();
		out.vec()->reserve( (size_t) vv->num.length() );
		for (int i=0;i<vv->num.length();i++)
			out.vec_append( vv->num[i] );
		break;
	case SSC_MATRIX:
		out.empty_vector();
		out.vec()->reserve( vv->num.nrows() );
		for (int i=0;i<vv->num.nrows();i++)
		{
			out.vec()->push_back( lk::vardata_t() );
			out.vec()->at(i).empty_vector();
			out.vec()->at(i).vec()->reserve( vv->num.ncols() );
			for (int j=0;j<vv->num.ncols();j++)
				out.vec()->at(i).vec_append( vv->num.at(i,j) );
		}
		break;
	case SSC_TABLE:
		{
			out.empty_hash();
			const char *key = vv->table.first();
			while (key != 0)
			{
				var_data *x = vv->table.lookup( key );
				lk::vardata_t &xvd = out.hash_item( lk_string(key) );
				sscvar_to_lkvar( xvd, x );
				key = vv->table.next();
			}
		}
		break;
	}

	return true;
}
*/

static bool lkvar_to_sscvar( ssc_data_t p_dat, const char *name, lk::vardata_t &val )
{	
	switch (val.type())
	{
	case lk::vardata_t::NUMBER:
		ssc_data_set_number( p_dat, name, (ssc_number_t)val.as_number() );
		break;
	case lk::vardata_t::STRING:
		ssc_data_set_string( p_dat, name, (const char*)val.as_string().c_str() );
		break;
	case lk::vardata_t::VECTOR:
		{
			size_t dim1 = val.length(), dim2 = 0;
			for (size_t i=0;i<val.length();i++)
			{
				lk::vardata_t *row = val.index(i);
				if (row->type() == lk::vardata_t::VECTOR && row->length() > dim2 )
					dim2 = row->length();
			}

			if (dim2 == 0 && dim1 > 0)
			{
				ssc_number_t *vec = new ssc_number_t[ dim1 ];
				for ( size_t i=0;i<dim1;i++)
					vec[i] = (ssc_number_t)val.index(i)->as_number();

				ssc_data_set_array( p_dat, name, vec, dim1 );
				delete [] vec;
			}
			else if ( dim1 > 0 && dim2 > 0 )
			{
				
				ssc_number_t *mat = new ssc_number_t[ dim1*dim2 ];
				for ( size_t i=0;i<dim1;i++)
				{
					for ( size_t j=0;j<dim2;j++ )
					{
						ssc_number_t x = 0;
						if ( val.index(i)->type() == lk::vardata_t::VECTOR
							&& j < val.index(i)->length() )
							x = (ssc_number_t)val.index(i)->index(j)->as_number();

						mat[ i*dim2 + j ] = x;
					}
				}

				ssc_data_set_matrix( p_dat, name, mat, dim1, dim2 );
				delete [] mat;
			}
		}
		break;
	case lk::vardata_t::HASH:		
		{
			ssc_data_t table = ssc_data_create();

			lk::varhash_t &hash = *val.hash();
			for ( lk::varhash_t::iterator it = hash.begin();
				it != hash.end();
				++it )
				lkvar_to_sscvar( table, (const char*)(*it).first.c_str(), *(*it).second );

			ssc_data_set_table( p_dat, name, table );
			ssc_data_free( table );
		}
		break;
	}

	return true;
}

void sscvar_to_lkvar( lk::vardata_t &out, const char *name, ssc_data_t p_dat )
{
	int ty = ssc_data_query( p_dat, name );
	switch( ty )
	{
	case SSC_NUMBER:
		{
			ssc_number_t num;
			if ( ssc_data_get_number( p_dat, name, &num ) )
			out.assign( (double) num );
		}
		break;
	case SSC_STRING:
		if ( const char *ss = ssc_data_get_string( p_dat, name ) )
			out.assign( lk_string(ss) );
		break;
	case SSC_ARRAY:
	{
		ssc_number_t *vv;
		int n = 0;
		vv = ssc_data_get_array( p_dat, name, &n );
		if ( vv && n > 0 )
		{
			out.empty_vector();
			out.vec()->reserve( (size_t) n );
			for (int i=0;i<n;i++)
				out.vec_append( vv[i] );
		}
	}
		break;
	case SSC_MATRIX:
	{
		ssc_number_t *mat;
		int nr, nc;
		mat = ssc_data_get_matrix( p_dat, name, &nr, &nc );
		if ( mat && nr > 0 && nc > 0 )
		{
			out.empty_vector();
			out.vec()->reserve( nr );
			for (int i=0;i<nr;i++)
			{
				out.vec()->push_back( lk::vardata_t() );
				out.vec()->at(i).empty_vector();
				out.vec()->at(i).vec()->reserve( nc );
				for (int j=0;j<nc;j++)
					out.vec()->at(i).vec_append( mat[ i*nc +j ] );
			}
		}
	}
		break;
	case SSC_TABLE:
		if ( ssc_data_t table = ssc_data_get_table( p_dat, name ) )
		{
			out.empty_hash();
			const char *key = ::ssc_data_first( table );
			while ( key != 0 )
			{
				lk::vardata_t &xvd = out.hash_item( lk_string(key) );
				sscvar_to_lkvar( xvd, key, table );
				key = ssc_data_next( p_dat );
			}
		}
		break;
	}
}



class lkSSCdataObj
{
	ssc_data_t m_data;
public:

	lkSSCdataObj() {
		m_data = ssc_data_create();
	}
	
	~lkSSCdataObj() {
		ssc_data_free( m_data );
	}
	
	operator ssc_data_t() { 
		return m_data;
	}
};

static lkSSCdataObj sg_sscData;


void fcall_ssc_var( lk::invoke_t &cxt )
{
	LK_DOC2( "ssc_var", "Sets or gets a variable value in the SSC data set.", 
		"Set a variable value.", "(string:name, variant:value):none", 
		"Get a variable value", "(string:name):variant" );

	wxString name = cxt.arg(0).as_string();
	if (cxt.arg_count() == 1)
		sscvar_to_lkvar( cxt.result(), (const char*)name.ToUTF8(), sg_sscData );
	else if (cxt.arg_count() == 2)
		lkvar_to_sscvar( sg_sscData, (const char*)name.ToUTF8(), cxt.arg(1).deref() );
}
void fcall_ssc_reset( lk::invoke_t &cxt )
{
	LK_DOC( "ssc_reset", "Reset the SSC variables", "( none ):none" );

	ssc_data_clear( sg_sscData );
}

void fcall_ssc_exec( lk::invoke_t &cxt )
{
	LK_DOC( "ssc_exec", "Run a compute module with the provided data context. returns zero if successful", "( string:module ):variant" );

	cxt.result().assign( -999.0 );

	if ( ssc_module_t mod = ssc_module_create( cxt.arg(0).as_string().c_str() ) )
	{
		if( ssc_module_exec( mod, sg_sscData ) )
		{
			cxt.result().assign( 0.0 );
		}
		else
		{
			lk_string errors;
			int idx=0;
			int ty = 0;
			float tm = 0;
			while ( const char *msg = ssc_module_log( mod, idx++, &ty, &tm ) )
			{
				errors += lk_string(msg);
			}

			cxt.result().assign( errors );
		}

		ssc_module_free( mod );
	}
}

void fcall_substance_density(lk::invoke_t &cxt)
{
	LK_DOC("substance_density", "Return the density given a substance ID and temperature in C", "(variant:substanceID, variant:tempC):variant");
	size_t substanceID = cxt.arg(0).as_unsigned();
	double tempC = cxt.arg(1).as_number();
	cxt.result().assign(substance_dens(substanceID, tempC));
}

void fcall_substance_specific_heat(lk::invoke_t &cxt)
{
	LK_DOC("substance_specific_heat", "Return the specific heat given a substance ID and temperature in C", "(variant:substanceID, variant:tempC):variant");
	size_t substanceID = cxt.arg(0).as_unsigned();
	double tempC = cxt.arg(1).as_number();
	cxt.result().assign(substance_sph(substanceID, tempC));
}

void fcall_snlinverter( lk::invoke_t &cxt )
{
	LK_DOC( "snlinverter", "Calculates the sandia inverter AC power from DC and specs", "(number:pdc, number:vdc, number:vdco, number:pdco, number:pso, number:paco, number:c0, number:c1, number:c2, number:c3):number" );

	double pdc = cxt.arg(0).as_number();
	double vdc = cxt.arg(1).as_number();
	double vdco = cxt.arg(2).as_number();
	double pdco = cxt.arg(3).as_number();
	double pso = cxt.arg(4).as_number();
	double paco = cxt.arg(5).as_number();
	double c0 = cxt.arg(6).as_number();
	double c1 = cxt.arg(7).as_number();
	double c2 = cxt.arg(8).as_number();
	double c3 = cxt.arg(9).as_number();

	// Pac = {(Paco / (A - B)) – C · (A - B)}· (Pdc- B) + C · (Pdc - B)**2

	
	  double vdcminusvdco = vdc - vdco;
	  double A = pdco * (1 + c1 * vdcminusvdco);
	  double B = pso * (1 + c2 * vdcminusvdco);
	  // Steve Miller email 12/16/10 - check for psc < 0 see Inverters_12_23_10.docx
	  B = (B<0)? 0:B;

	  double C = c0 * (1 + c3 * vdcminusvdco);
	  double pac = ((paco / (A- B)) - C * (A - B)) * (pdc - B) + C * (pdc - B) * (pdc - B);

	  cxt.result().assign( pac );
}

void fcall_current_at_voltage_cec(lk::invoke_t &cxt)
{
	LK_DOC("current_at_voltage_cec", "Calculates the CEC module model voltage from module current and specs", "(number:Vmodule, number:IL_ref, number:IO_ref, number:RS, number:A_ref, number:RSH_ref, number:I_mp_ref):number");

	double Vmodule = cxt.arg(0).as_number();
	double IL_ref = cxt.arg(1).as_number();
	double IO_ref = cxt.arg(2).as_number();
	double RS = cxt.arg(3).as_number();
	double A_ref = cxt.arg(4).as_number();
	double RSH_ref = cxt.arg(5).as_number();
	double I_mp_ref = cxt.arg(6).as_number();

	double F = 0, Fprime = 0;
	double Iold = 0.0;
	double Inew = I_mp_ref;

	while (abs(Inew - Iold) > 1.0e-4)
	{
		Iold = Inew;

		F = IL_ref - Iold - IO_ref *
			(exp((Vmodule + Iold * RS) / A_ref) - 1.0) -
			(Vmodule + Iold * RS) / RSH_ref;

		Fprime = -1.0 - IO_ref * (RS / A_ref) *
			exp((Vmodule + Iold * RS) / A_ref) -
			(RS / RSH_ref);

		Inew = std::max(0.0, (Iold - (F / Fprime)));
	}

	cxt.result().assign(Inew);
}

void fcall_current_at_voltage_sandia(lk::invoke_t &cxt)
{
	LK_DOC("current_at_voltage_sandia", "Calculates the Sandia module model voltage from module current and specs", "(number:V, number:VmaxPow, number:ImaxPow, number:Voc, number:Isc):number");

	double V = cxt.arg(0).as_number(); 
	double VmaxPow = cxt.arg(1).as_number(); 
	double ImaxPow = cxt.arg(2).as_number();
	double Voc = cxt.arg(3).as_number();
	double Isc = cxt.arg(4).as_number();

	double Itrw = 0, C_1 = 0, C_2 = 0;
	if ((Isc > 0) && (Voc > 0)) {
		if (ImaxPow < Isc) C_2 = (VmaxPow / Voc - 1.0) / log(1.0 - ImaxPow / Isc);

		if (C_2 > 0) {
			C_1 = (1.0 - ImaxPow / Isc) * exp(-VmaxPow / C_2 / Voc);
			Itrw = Isc*(1.0 - C_1 * (exp(V / C_2 / Voc) - 1.0));
		}
		else {
			Itrw = 0.0;
		}
	}
	if (Itrw < 0) Itrw = 0;
	cxt.result().assign(Itrw);
}

lk::fcall_t* invoke_general_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_logmsg,
		fcall_browse,
		fcall_appdir,
		fcall_runtimedir,
		fcall_userlocaldatadir,
#ifdef __WXMSW__
		fcall_xl_create,
		fcall_xl_free,
		fcall_xl_open,
		fcall_xl_wkbook,
		fcall_xl_sheet,
		fcall_xl_set,
		fcall_xl_get,
		fcall_xl_autosizecols,
#endif
		0 };
	return (lk::fcall_t*)vec;
}

lk::fcall_t* invoke_ssc_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_ssc_reset,
		fcall_ssc_var,
		fcall_ssc_exec,
		0 };
	return (lk::fcall_t*)vec;
}

lk::fcall_t* invoke_config_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_addconfig,
		fcall_setconfig,
		fcall_addpage,
		fcall_setting,
		fcall_setmodules,
		0 };
	return (lk::fcall_t*)vec;
}

lk::fcall_t* invoke_equation_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_substance_density,
		fcall_substance_specific_heat,
		fcall_snlinverter,
		fcall_current_at_voltage_cec,
		fcall_current_at_voltage_sandia,
		0 };
	return (lk::fcall_t*)vec;
}

lk::fcall_t* invoke_casecallback_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_value,
		fcall_metric,
		fcall_cfline,
		fcall_technology,
		fcall_financing,
		0 };
	return (lk::fcall_t*)vec;
}

lk::fcall_t* invoke_uicallback_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_setplot,
		fcall_clearplot,
		fcall_enable,
		fcall_show,
		fcall_property,
		fcall_refresh,
		fcall_dview,
		fcall_substance_density,
		fcall_substance_specific_heat,
		fcall_snlinverter,
		fcall_current_at_voltage_cec,
		fcall_current_at_voltage_sandia,
		0 };
	return (lk::fcall_t*)vec;
}

