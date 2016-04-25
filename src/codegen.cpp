#include <algorithm>

#include <wx/datstrm.h>
#include <wx/gauge.h>
#include <wx/progdlg.h>
#include <wx/thread.h>
#include <wx/statline.h>
#include <wx/stattext.h>
#include <wx/file.h>
#include <wx/ffile.h>

#include <wx/filedlg.h>
#include <wx/filefn.h>

#include <wex/metro.h>
#include <wex/utils.h>

#include <lk/absyn.h>
#include <lk/stdlib.h>
#include <lk/eval.h>

#include <ssc/sscapi.h>

#include "codegen.h"
#include "main.h"
#include "equations.h"
#include "case.h"


static bool VarValueToSSC( VarValue *vv, ssc_data_t pdata, const wxString &sscname )
{
	switch( vv->Type() )
	{
	case VV_NUMBER:
		ssc_data_set_number( pdata, sscname.c_str(), (ssc_number_t)vv->Value() );
		break;
	case VV_ARRAY:
	{
		size_t n;
		float *p = vv->Array( &n );
		if ( sizeof(ssc_number_t) == sizeof( float ) )
			ssc_data_set_array( pdata, sscname.c_str(), p, n );
		else
		{
			ssc_number_t *pp = new ssc_number_t[n];
			for( size_t i=0;i<n;i++ )
				pp[i] = p[i];

			ssc_data_set_array( pdata, sscname.c_str(), pp, n );

			delete [] pp;
		}
	}
		break;
	case VV_MATRIX:
	{
		matrix_t<float> &fl = vv->Matrix();
		if ( sizeof(ssc_number_t) == sizeof(float) )
		{
			ssc_data_set_matrix( pdata, sscname.c_str(), fl.data(), fl.nrows(), fl.ncols() );
		}
		else
		{
			ssc_number_t *pp = new ssc_number_t[ fl.nrows() * fl.ncols() ];
			size_t n = 0;
			for( size_t r = 0; r < fl.nrows(); r++ )
				for( size_t c=0;c<fl.ncols();c++)
					pp[n++] = (ssc_number_t)fl(r,c);

			ssc_data_set_matrix( pdata, sscname.c_str(), pp, fl.nrows(), fl.ncols() );
			delete [] pp;
		}
	}
		break;
	case VV_STRING:
		ssc_data_set_string( pdata, sscname.c_str(), vv->String().c_str() );
		break;
	case VV_TABLE:
	{
		ssc_data_t tab = ssc_data_create();
		VarTable &vt = vv->Table();
		for( VarTable::iterator it = vt.begin();
			it != vt.end();
			++it )
		{
			VarValueToSSC( it->second, tab, it->first );
		}

		ssc_data_set_table( pdata, sscname.c_str(), tab );

		ssc_data_free( tab ); // ssc_data_set_table above makes a deep copy, so free this here
	}
		break;


	case VV_INVALID:
	default:
		return false;
	}

	return true;
}


static void write_array_string(wxDataOutputStream &out, wxArrayString &list)
{
	out.Write32(list.size());
	for (size_t i = 0; i<list.size(); i++)
		out.WriteString(list[i]);
}

static void read_array_string(wxDataInputStream &in, wxArrayString &list)
{
	list.Clear();
	size_t n = in.Read32();
	for (size_t i = 0; i<n; i++)
		list.Add(in.ReadString());
}

// end of prototype from simulation.cpp



CodeGen_Base::CodeGen_Base( Case *cc, const wxString &name )
	: m_case( cc ), m_name( name )
{
}


bool CodeGen_Base::GenerateCode(wxOutputStream &os)
{
	// write language specific header
	Header(os);
	// create ssc data container 
	wxString data_name = "data";
	CreateSSCData(os, data_name);
	// get list of compute modules from case configuration
	// go through each module and create and set inputs and run and free

	// list of outputs from metrics - go through and call output for each
	wxString out_name = "";
	Output(os, out_name);
	// clean up
	FreeSSCData(os,data_name);
	return true;
}

bool CodeGen_Base::Ok()
{
	return m_errors.size() == 0;
}


// prototype for each language
static void dump_variable( FILE *fp, ssc_data_t p_data, const char *name )
{
	ssc_number_t value;
	ssc_number_t *p;
	int len, nr, nc;
	wxString str_value;
	double dbl_value;
	int type = ::ssc_data_query( p_data, name );
	switch( type )
	{
	case SSC_STRING:
		str_value = wxString::FromUTF8(::ssc_data_get_string( p_data, name ));
		str_value.Replace("\\", "/" );
		fprintf(fp, "var( '%s', '%s' );\n", name, (const char*)str_value.c_str() );
		break;
	case SSC_NUMBER:
		::ssc_data_get_number( p_data, name, &value );
		dbl_value = (double)value;
		if ( dbl_value > 1e38 ) dbl_value = 1e38;
		fprintf(fp, "var( '%s', %lg );\n", name, dbl_value );
		break;
	case SSC_ARRAY:
		p = ::ssc_data_get_array( p_data, name, &len );
		fprintf(fp, "var( '%s', [", name);
		for ( int i=0;i<(len-1);i++ )
		{
			dbl_value = (double)p[i];
			if ( dbl_value > 1e38 ) dbl_value = 1e38;
			fprintf(fp, " %lg,", dbl_value );
		}
		dbl_value = (double)p[len-1];
		if ( dbl_value > 1e38 ) dbl_value = 1e38;
		fprintf(fp, " %lg ] );\n", dbl_value );
		break;
	case SSC_MATRIX:
		p = ::ssc_data_get_matrix( p_data, name, &nr, &nc );
		len = nr*nc;
		fprintf( fp, "var( '%s', \n[ [", name );
		for (int k=0;k<(len-1);k++)
		{
			dbl_value = (double)p[k];
			if ( dbl_value > 1e38 ) dbl_value = 1e38;
			if ( (k+1)%nc == 0 )
				fprintf(fp, " %lg ], \n[", dbl_value);
			else
				fprintf(fp, " %lg,", dbl_value);
		}
		dbl_value = (double)p[len-1];
		if ( dbl_value > 1e38 ) dbl_value = 1e38;
		fprintf(fp, " %lg ] ] );\n", dbl_value);
	}
}



CodeGen_BaseDialog::CodeGen_BaseDialog( const wxString &message )
{
	m_transp = wxCreateTransparentOverlay( SamApp::Window() );
	wxYield();
}

CodeGen_BaseDialog::~CodeGen_BaseDialog()
{
	m_transp->Destroy();
}

