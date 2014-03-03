#include <wx/log.h>

#include <wex/plot/plplotctrl.h>
#include <wex/lkscript.h>
#include <wex/dview/dvplotctrl.h>

#include <ssc/sscapi.h>

#include "main.h"
#include "case.h"

#include "invoke.h"

#ifndef MAX
#define MAX(a, b) ((a) > (b) ? a : b)
#endif

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

static void fcall_metric( lk::invoke_t &cxt )
{
	LK_DOC("metric", "Add an output metric to the current configuration. Options include mode,deci,thousep,pre,post,label,scale", "(string:variable, [table:options]):none");
	 
	if ( ConfigInfo *ci = SamApp::Config().CurrentConfig() )
	{
		ConfigInfo::MetricData md;
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
	


static void plottarget( CallbackContext &cc, const wxString &name )
{
	wxLKSetPlotTarget( 0 );
	if ( wxUIObject *obj = cc.InputPage()->Find(name) )
		if ( wxPLPlotCtrl *plot = obj->GetNative<wxPLPlotCtrl>() )
			wxLKSetPlotTarget( plot );
}

void fcall_setplot( lk::invoke_t &cxt )
{
	LK_DOC("setplot", "Sets the current plot target by name", "(string:name):boolean");
	
	plottarget( *(CallbackContext*)cxt.user_data(), cxt.arg(0).as_string() );
}

void fcall_clearplot( lk::invoke_t &cxt )
{
	LK_DOC("clearplot", "Clears the current plot, and optionally switches the plot target.", "([string:plot name]):none");
	
	if (cxt.arg_count() > 0)
		plottarget( *(CallbackContext*)cxt.user_data(), cxt.arg(0).as_string() );

	if ( wxPLPlotCtrl *plot = wxLKGetPlotTarget() )
	{
		plot->DeleteAllPlots();
		plot->Refresh();
	}
}

void fcall_value( lk::invoke_t &cxt )
{
	LK_DOC("value", "Gets or sets the value of a variable by name", "(string:name [,variant:value]):[variant]");
	
	CallbackContext &cc = *(CallbackContext*)cxt.user_data();
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

void fcall_refresh( lk::invoke_t &cxt )
{
	LK_DOC("refresh", "Refresh the current form or a specific widget", "([string:name]):none" );
	
	CallbackContext &cc = *(CallbackContext*)cxt.user_data();
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

	CallbackContext &cc = *(CallbackContext*)cxt.user_data();
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

	CallbackContext &cc = *(CallbackContext*)cxt.user_data();
	if ( wxUIObject *obj = cc.InputPage()->FindActiveObject( cxt.arg(0).as_string(), 0 ) )
		if ( wxWindow *native = obj->GetNative() )
			native->Enable( cxt.arg(1).as_boolean() );
}
static void fcall_show( lk::invoke_t &cxt )
{
	LK_DOC("show", "Show or hide a user interface widget.", "(string:name, boolean:show):none");

	CallbackContext &cc = *(CallbackContext*)cxt.user_data();
	if ( wxUIObject *obj = cc.InputPage()->FindActiveObject( cxt.arg(0).as_string(), 0 ) )
		obj->Show( cxt.arg(1).as_boolean() );
}

static void fcall_technology( lk::invoke_t &cxt )
{
	LK_DOC( "technology", "Return the current technology option name", "(void):string" );
	CallbackContext &cc = *static_cast<CallbackContext*>( cxt.user_data() ); 
	cxt.result().assign( cc.GetCase().GetTechnology() );
}

static void fcall_financing( lk::invoke_t &cxt )
{
	LK_DOC( "financing", "Return the current financing option name", "(void):string" );
	CallbackContext &cc = *static_cast<CallbackContext*>( cxt.user_data() ); 
	cxt.result().assign( cc.GetCase().GetFinancing() );
}

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

static void fcall_substance_density(lk::invoke_t &cxt)
{
	LK_DOC("substance_density", "Return the density given a substance ID and temperature in C", "(variant:substanceID, variant:tempC):variant");
	/*
	This function accepts as inputs temperature [K] and pressure [Pa]
	This function outputs density in units of [kg/m^3]
	*/
	size_t substanceID = cxt.arg(0).as_unsigned();
	double tempC = cxt.arg(0).as_number();

	double T = tempC + 273.15; // C to K
	double v, R_air;
	double P = 1.0; // default pressure used 1Pa
	double density = 1.0; // default - Type 229=0, Type 805=1

	switch (substanceID)
	{
	case 1: //   1.) Air
		R_air = 287; // Gas constant [J/kg-K]
		v = R_air*T / P;
		density = 1.0 / v;
		break;
	case 2: //   2.) Stainless_AISI316
		density = 8349.38 - 0.341708*T - 0.0000865128*T*T; // !EES
		break;
	case 3: //   3.) Water (liquid)
		break;
	case 4: //   4.) Steam
		break;
	case 5: //   5.) CO2
		break;
	case 6: //   6.) Salt (68% KCl, 32% MgCl2)
		density = 1E-10*T*T*T - 3E-07*T*T - 0.4739*T + 2384.2;
		break;
	case 7: //   7.) Salt (8% NaF, 92% NaBF4)
		density = 8E-09*T*T*T - 2E-05*T*T - 0.6867*T + 2438.5;
		break;
	case 8: //   8.) Salt (25% KF, 75% KBF4)
		density = 2E-08*T*T*T - 6E-05*T*T - 0.7701*T + 2466.1;
		break;
	case 9: //   9.) Salt (31% RbF, 69% RbBF4)
		density = -1E-08*T*T*T + 4E-05*T*T - 1.0836*T + 3242.6;
		break;
	case 10: //   10.) Salt (46.5% LiF, 11.5%NaF, 42%KF)
		density = -2E-09*T*T*T + 1E-05*T*T - 0.7427*T + 2734.7;
		break;
	case 11: //   11.) Salt (49% LiF, 29% NaF, 29% ZrF4)
		density = -2E-11*T*T*T + 1E-07*T*T - 0.5172*T + 3674.3;
		break;
	case 12: //   12.) Salt (58% KF, 42% ZrF4)
		density = -6E-10*T*T*T + 4E-06*T*T - 0.8931*T + 3661.3;
		break;
	case 13: //   13.) Salt (58% LiCl, 42% RbCl)
		density = -8E-10*T*T*T + 1E-06*T*T - 0.689*T + 2929.5;
		break;
	case 14: //   14.) Salt (58% NaCl, 42% MgCl2)
		density = -5E-09*T*T*T + 2E-05*T*T - 0.5298*T + 2444.1;
		break;
	case 15: //   15.) Salt (59.5% LiCl, 40.5% KCl)
		density = 1E-09*T*T*T - 5E-06*T*T - 0.864*T + 2112.6;
		break;
	case 16: //   16.) Salt (59.5% NaF, 40.5% ZrF4)
		density = -5E-09*T*T*T + 2E-05*T*T - 0.9144*T + 3837.0;
		break;
	case 17: //   17.) Salt (60% NaNO3, 40% KNO3)
		density = -1E-07*T*T*T + 0.0002*T*T - 0.7875*T + 2299.4;
		density = MAX(density, 1000.0);
		break;
		/*
		case(18:25) !  18-25.) Call trough properties
		density = Dens_fluid((T-273.15),int(Fnumd)) !Trough calcs take fluid properties in degC, not K
		*/
	case 18: //   18.) Nitrate Salt** type 805 dens_salt(Tc)
		density = 2090 - 0.636 * tempC;
		density = MAX(density, 1000.0);
		break;
	case 19: //   19.) Caloria HT 43** type 805 dens_caloria(tempC)
		density = 885 - 0.6617 * tempC - 0.0001265 * tempC*tempC;
		density = MAX(density, 100.0);
		break;
	case 20: //   20.) Hitec XL** type 805 dens_salt_xl(tempC)
		density = 2240 - 0.8266 * tempC;
		density = MAX(density, 800.0);
		break;
	case 21: //   21.) Therminol VP-1** type 805 dens_therminol(tempC)
		density = 1074.0 - 0.6367 * tempC - 0.0007762 * tempC*tempC;
		density = MAX(density, 400.0);
		break;
	case 22: //   22.) Hitec** type 805 dens_salt_hitec(tempC)
		density = 2080 - 0.733 * tempC;
		density = MAX(density, 1000.0);
		break;
	case 23: //   23.) Dowtherm Q** type 805 dens_Dowtherm_Q(tempC)
		density = -0.757332 * tempC + 980.787;
		density = MAX(density, 100.0);
		break;
	case 24: //   24.) Dowtherm RP** type 805 dens_Dowtherm_RP(tempC)
		//density = -0.000186495 * tempC*tempC - 0.668337 * T + 1042.11;
		density = -0.000186495 * tempC*tempC - 0.668337 * tempC + 1042.11; // changed from above when the function was added to SAMnt (as per email from Ty, Feb 23, 2014)
		density = MAX(density, 200.0);
		break;
	case 25: //   25.) Salt XL** type 805 dens_salt_xl(tempC)
		density = 2240 - 0.8266 * tempC;
		density = MAX(density, 800.0);
		break;
	case 26: //   26.) Argon (ideal gas properties)
		density = P / (208.13*T);
		density = MAX(density, 1e-10);
		break;
	case 27: //   27.) Hydrogen (ideal gas properties)
		density = P / (4124.*T);
		density = MAX(density, 1e-10);
		break;
	case 28: //   28.) T-91 Steel: "Thermo hydraulic optimisation of the EURISOL DS target" - Paul Scherrer Institut
		density = -0.3289*tempC + 7742.5;
		break;
	case 29: //   29.) Therminol 66: Reference: Therminol Reference Disk by Solutia: www.therminol.com/pages/tools/toolscd.asp
		density = -0.7146*tempC + 1024.8;
		break;
	case 30: //   30.) Therminol 59: Reference: Therminol Reference Disk by Solutia: www.therminol.com/pages/tools/toolscd.asp
		density = -0.0003*tempC*tempC - 0.6963*tempC + 988.44;
		break;
	case 31: //   31.) -blank-
	case 32: //   32.) -blank-
	case 33: //   33.) -blank-
	case 34: //   34.) -blank-
	case 35: //   35.) -blank-
		break;
	case 36: //   36+) User specified (lookup tables)
		/*
		!Call the user-defined property table
		lb=fl_bounds(fnum-35)
		ub=fl_bounds(fnum-35+1)-1
		if(ub.lt.lb) ub=size(fprop(1,:))
		dxx(:)=fprop(1,lb:ub)
		dyy(:)=fprop(3,lb:ub)
		call interp(T,size(dxx),dxx,dyy,Gjsav,Density)
		if((Gjsav.eq.ub).or.(Gjsav.eq.lb)) dum=t_warn(T,dxx(lb),dxx(ub),"User-specified fluid")
		*/
		break;
	}
	cxt.result().assign(density);
}

lk::fcall_t* invoke_general_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_logmsg,
		fcall_browse,
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
		fcall_metric,
		fcall_setting,
		fcall_setmodules,
		0 };
	return (lk::fcall_t*)vec;
}

lk::fcall_t* invoke_other_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_substance_density,
		0 };
	return (lk::fcall_t*)vec;
}

lk::fcall_t* invoke_uicallback_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_setplot,
		fcall_clearplot,
		fcall_value,
		fcall_enable,
		fcall_show,
		fcall_property,
		fcall_refresh,
		fcall_technology,
		fcall_financing,
		fcall_dview,
		fcall_substance_density,
		0 };
	return (lk::fcall_t*)vec;
}

