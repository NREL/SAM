#include <wx/log.h>

#include <wex/plot/plplotctrl.h>
#include <wex/lkscript.h>
#include <wex/dview/dvplotctrl.h>

#include <ssc/sscapi.h>

#include "main.h"
#include "case.h"

#include "invoke.h"


enum{ID_DVIEWWIN = wxID_HIGHEST + 256, ID_DVIEWCTRL};

static void fcall_dview(lk::invoke_t &cxt)
{
	LK_DOC("dview", "Creates a separate dview viewer for viewing specified data.", "( variant:num_datasets, string:window name, string:data_name1, string:data_units1, variant:multiplier1, variant:data1, [ string:data_name2, string:data_units2, variant:multiplier2, variant:data2], ...):none");

	wxString win_name = cxt.arg(0).as_string();
	wxString var_name1 = cxt.arg(1).as_string();
	wxString var_units1 = cxt.arg(2).as_string();
	wxString var_name2 = cxt.arg(3).as_string();
	wxString var_units2 = cxt.arg(4).as_string();

	lk::vardata_t &plot_data1 = cxt.arg(5);
	if (plot_data1.length() != 8760) return;
	lk::vardata_t &plot_data2 = cxt.arg(6);
	if (plot_data2.length() != 8760) return;

	std::vector<double> data1(8760);
	std::vector<double> data2(8760);

	for (size_t i = 0; i < 8760; i++)
	{
		data1[i] = plot_data1.index(i)->as_number();
		data2[i] = plot_data2.index(i)->as_number();
	}
	
	wxDialog *frame;
	wxDVPlotCtrl *dview;
//	if (wxWindow::FindWindowById(ID_DVIEWWIN))
	if (wxWindow::FindWindowByLabel(win_name))
		{
		frame = (wxDialog*)wxWindow::FindWindowByLabel(win_name);
		dview = (wxDVPlotCtrl *)wxWindow::FindWindowById(ID_DVIEWCTRL);
//		frame = (wxDialog*)wxWindow::FindWindowById(ID_DVIEWWIN);
//		dview = (wxDVPlotCtrl *)wxWindow::FindWindowById(ID_DVIEWCTRL);
	}
	else
	{
		frame = new wxDialog(wxTheApp->GetTopWindow(), ID_DVIEWWIN, win_name, wxDefaultPosition, wxSize(900, 700));
		dview = new wxDVPlotCtrl(frame, ID_DVIEWCTRL);
		double timestep = 1;
		dview->RemoveAllDataSets();
		dview->AddDataSet(new wxDVArrayDataSet(var_name1, var_units1, timestep, data1));
		dview->AddDataSet(new wxDVArrayDataSet(var_name2, var_units2, timestep, data2));

		dview->SelectDataIndex(0);
	}


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


class lkSSCdataObj : public lk::objref_t
{
	ssc_data_t m_data;
public:

	lkSSCdataObj() {
		m_data = ssc_data_create();
	}
	virtual ~lkSSCdataObj() {
		ssc_data_free( m_data );
	}

	virtual lk_string type_name() {
		return lk_string("lkSSCdataObj");
	}

	operator ssc_data_t() { 
		return m_data;
	}
};

static void fcall_ssc_data_create( lk::invoke_t &cxt )
{
	LK_DOC( "ssc_data_create", "Creates a new SSC data context", "(void):reference");
	cxt.result().assign( (double)cxt.env()->insert_object( new lkSSCdataObj ) );
}

static void fcall_ssc_data_free( lk::invoke_t &cxt )
{
	LK_DOC( "ssc_data_free", "Frees an exist SSC data context", "(reference):none");
	if ( lkSSCdataObj *p = dynamic_cast<lkSSCdataObj*>(cxt.env()->query_object( cxt.arg(0).as_integer() ) ) )
		cxt.env()->destroy_object( p );
}

void fcall_ssc_var( lk::invoke_t &cxt )
{
	LK_DOC2( "ssc_var", "Sets or gets a variable value in the SSC data set.", 
		"Set a variable value.", "(reference:data, string:name, variant:value):none", 
		"Get a variable value", "(reference:data, string:name):variant" );
	
	if ( lkSSCdataObj *p = dynamic_cast<lkSSCdataObj*>(cxt.env()->query_object( cxt.arg(0).as_integer() ) ) )
	{
		wxString name = cxt.arg(1).as_string();
		if (cxt.arg_count() == 1)
			sscvar_to_lkvar( cxt.result(), name, *p );
		else if (cxt.arg_count() == 2)
			lkvar_to_sscvar( *p, name, cxt.arg(2).deref() );
	}
}

void fcall_ssc_exec( lk::invoke_t &cxt )
{
	LK_DOC( "ssc_exec", "Run a compute module with the provided data context. returns zero if successful", "( string:modules, reference:data ):variant" );

	cxt.result().assign( -999.0 );
	if ( lkSSCdataObj *data = dynamic_cast<lkSSCdataObj*>(cxt.env()->query_object( cxt.arg(0).as_integer() ) ) )
	{
		if ( ssc_module_t mod = ssc_module_create( cxt.arg(0).as_string().c_str() ) )
		{
			if( ssc_module_exec( mod, *data ) )
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
}

lk::fcall_t* invoke_general_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_logmsg,
		fcall_browse,
		fcall_ssc_data_create,
		fcall_ssc_data_free,
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
		fcall_setmodules,
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
		0 };
	return (lk::fcall_t*)vec;
}
