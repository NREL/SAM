#include <wx/datstrm.h>
#include <wx/wfstream.h>
#include <wx/zstream.h>

#include "project.h"
#include "case.h"
#include "main.h"
#include "invoke.h"
#include <lk_stdlib.h>

ProjectFile::ProjectFile()
{
	m_verMajor = SamApp::VersionMajor();
	m_verMinor = SamApp::VersionMinor();
	m_verMicro = SamApp::VersionMicro();
	m_saveHourlyData = false;
	m_modified = false;
}

ProjectFile::ProjectFile( const ProjectFile &rhs )
{
	Copy( rhs );
}

ProjectFile::~ProjectFile()
{
	SendEvent(ProjectFileEvent::PROJECTFILE_DELETED);
	ClearListeners();
	Clear();
}

void ProjectFile::Copy( const ProjectFile &rhs, bool listeners_too )
{
	// note that objectcollection::Copy() performs deep copy with Object::Duplicate() method.
	m_cases.Copy( rhs.m_cases );
	m_objects.Copy( rhs.m_objects );
	m_properties = rhs.m_properties;
	m_lastError = rhs.m_lastError;

	m_saveHourlyData = rhs.m_saveHourlyData;
	m_modified = rhs.m_modified;
	
	if ( listeners_too )
		m_listeners = rhs.m_listeners;

	m_verMajor = rhs.m_verMajor;
	m_verMinor = rhs.m_verMinor;
	m_verMicro = rhs.m_verMicro;
}

void ProjectFile::Clear()
{
	SendEvent(ProjectFileEvent::PROJECTFILE_DELETED);
	m_cases.Clear();
	m_objects.Clear();
	m_properties.clear();
}


void ProjectFile::AddListener(ProjectFileEventListener *pel)
{
	m_listeners.push_back(pel);
}

void ProjectFile::RemoveListener(ProjectFileEventListener *pel)
{
	if (m_listeners.size() > 0)
	{
		for (size_t i = 0; i < m_listeners.size(); i++)
		{
			if (m_listeners[i] == pel)
			{
				m_listeners.erase(m_listeners.begin() + i);
				break;
			}
		}
	}
}


void ProjectFile::ClearListeners()
{
	m_listeners.clear();
}

void ProjectFile::SendEvent(ProjectFileEvent e)
{
	for (size_t i = 0; i<m_listeners.size(); i++)
		if (m_listeners[i])	m_listeners[i]->OnProjectFileEvent(this, e);
}


// managing cases
void ProjectFile::AddCase( const wxString &name, Case *c )
{
	m_cases.Add( name, c );
	SendEvent(ProjectFileEvent(ProjectFileEvent::CASE_ADDED, name));
	m_modified = true;
}

Case *ProjectFile::AddCase( const wxString &name )
{
	Case *c = new Case;
	AddCase( name, c );
	return c;
}

bool ProjectFile::DeleteCase( const wxString &name )
{
	SendEvent(ProjectFileEvent(ProjectFileEvent::CASE_DELETED, name));
	if (m_cases.Delete(name))
	{
		m_modified = true;
		return true;
	}
	else return false;
}

bool ProjectFile::RenameCase( const wxString &old_name, const wxString &new_name )
{
	if (m_cases.Rename(old_name, new_name))
	{
		SendEvent(ProjectFileEvent(ProjectFileEvent::CASE_RENAMED, old_name, new_name));
		return true;
	}
	else return false;
}

std::vector<Case*> ProjectFile::GetCases()
{
	return m_cases.GetObjects<Case>();
}

wxArrayString ProjectFile::GetCaseNames()
{
	return m_cases.GetNames();
}

Case *ProjectFile::GetCase( const wxString &name )
{
	return dynamic_cast<Case*>( m_cases.Lookup( name ) );
}

wxString ProjectFile::GetCaseName( Case *c )
{
	for( ObjectCollection::iterator it = m_cases.begin();
		it != m_cases.end();
		++it )
		if ( it->second == c )
			return it->first;

	return wxEmptyString;
}

// simple project file properties
wxString ProjectFile::GetProperty( const wxString &name )
{
	if ( m_properties.find( name ) != m_properties.end() )
		return m_properties[ name ];
	else
		return wxEmptyString;
}

void ProjectFile::SetProperty( const wxString &name, const wxString &value )
{
	m_properties[ name ] = value;
	m_modified = true;
}

wxArrayString ProjectFile::GetProperties()
{
	wxArrayString list;
	for ( StringHash::iterator it = m_properties.begin();
		it != m_properties.end();
		++it )
		list.Add( it->first );
	return list;
}

// general purpose data storage objects
void ProjectFile::AddObject( const wxString &name, Object *obj )
{
	m_objects.Add( name, obj );
	m_modified = true;
}

void ProjectFile::DeleteObject( const wxString &name )
{
	m_objects.Delete( name );
	m_modified = true;
}

bool ProjectFile::RenameObject( const wxString &old_name, const wxString &new_name )
{
	return m_cases.Rename( old_name, new_name );
}

Object *ProjectFile::GetObject( const wxString &name )
{
	return m_objects.Lookup( name );
}

wxArrayString ProjectFile::GetObjects()
{
	return m_objects.GetNames();
}

void ProjectFile::Write( wxOutputStream &output )
{
	wxDataOutputStream out(output);

	out.Write16( 0x3c ); // identifier code
	out.Write16( 2 ); // data format version
	out.Write16( m_verMajor );
	out.Write16( m_verMinor );
	out.Write16( m_verMicro );
	
	m_properties.Write( output );
	m_cases.Write( output );
	m_objects.Write( output );

	out.Write8( m_saveHourlyData ? 1 : 0 );
	
	out.Write16( 0x3c ); // identifier code to finish
} 

bool ProjectFile::Read( wxInputStream &input )
{
	m_lastError.Clear();
	Clear();

	wxDataInputStream in(input);

	wxUint16 code = in.Read16();
	wxUint16 ver = in.Read16();
	m_verMajor = (int)in.Read16();
	m_verMinor = (int)in.Read16();
	m_verMicro = (int)in.Read16();

	if ( !m_properties.Read( input ) ) m_lastError = "could not read project properties";
	if ( !m_cases.Read( input ) ) m_lastError = "could not read case data" ;
	if ( !m_objects.Read( input ) ) m_lastError = "could not read objects" ;

	m_modified = false;

	if ( ver >= 2 )
		m_saveHourlyData = ( in.Read8() != 0 );
	else
		m_saveHourlyData = false;

	return (in.Read16() == code );
}

bool ProjectFile::WriteArchive( const wxString &file )
{
	wxFFileOutputStream out( file );
	if ( ! out.IsOk() ) return false;

	// write two bytes of uncompressed identifiers
	wxUint8 code = 0x41;
	out.Write( &code, 1 );
	code = 0xb9;
	out.Write( &code, 1 );

	// write one byte to indicate compression format
	// currently only zlib (=1) supported
	code = 1;
	out.Write( &code, 1 );

	wxZlibOutputStream zout( out );
	Write( zout );
	return true;
}

bool ProjectFile::ReadArchive( const wxString &file )
{
	wxFFileInputStream in( file );
	if ( !in.IsOk() ) return false;
	
	wxUint8 code1, code2, comp;
	in.Read( &code1, 1 );
	in.Read( &code2, 1 );

	if ( code1 != 0x41 || code2 != 0xb9 )
	{
		// old format, uncompressed
		in.Ungetch( code2 );
		in.Ungetch( code1 );
		
		return Read( in );
	}
	else
	{
		// determine compression format
		in.Read(&comp, 1);
		if ( comp != 1 ) // unknown compression type
			return false;

		wxZlibInputStream zin( in );
		return Read( zin );
	}
}

void ProjectFile::SetVersionInfo( int maj, int min, int mic )
{
	m_verMajor = maj;
	m_verMinor = min;
	m_verMicro = mic;
}

size_t ProjectFile::GetVersionInfo( int *maj, int *min, int *mic )
{
	if( maj ) *maj = m_verMajor;
	if( min ) *min = m_verMinor;
	if( mic ) *mic = m_verMicro;
	return VERSION_VALUE( m_verMajor, m_verMinor, m_verMicro );
}

static void fcall_vuc_case_name( lk::invoke_t &cxt )
{
	LK_DOC( "case_name", "Return the current case's name.", "(none):string");
	if ( VersionUpgrade *vuc = static_cast<VersionUpgrade*>(cxt.user_data()) )
		cxt.result().assign( vuc->GetName() );
}

static void fcall_vuc_config( lk::invoke_t &cxt )
{
	LK_DOC( "config", "Set or get the current case's configuration", "(string:tech, string:fin):boolean or (none):table");
	if ( VersionUpgrade *vuc = static_cast<VersionUpgrade*>(cxt.user_data()) )
	{
		wxString tech( vuc->GetCase()->GetTechnology() );
		wxString fin( vuc->GetCase()->GetFinancing() );

		if (cxt.arg_count() == 0 )
		{
			cxt.result().empty_hash();
			cxt.result().hash_item("tech").assign( tech );
			cxt.result().hash_item("fin").assign( fin );
		}
		else
		{
			wxString tech1( cxt.arg(0).as_string() );
			wxString fin1( cxt.arg(1).as_string() );
			vuc->GetLog( vuc->GetName() ).push_back( VersionUpgrade::log( 
				VersionUpgrade::CONFIG_CHANGE, "Updated internal configuration name from " + tech + "/" + fin + " to " + tech1 + "/" + fin1 + "." ) );
			cxt.result().assign( vuc->GetCase()->SetConfiguration( tech1, fin1 ) );
		}
	}

}

static void fcall_vuc_value( lk::invoke_t &cxt )
{
	LK_DOC("value", "Set or get a variable's value. New variables may be created.", "(string:name, [variant:value]):variant");
	if ( VersionUpgrade *vuc = static_cast<VersionUpgrade*>(cxt.user_data()) )
	{
		wxString name( cxt.arg(0).as_string() );
		VarTable &vt = vuc->GetCase()->Values();
		if ( cxt.arg_count() == 1 )
		{
			if ( VarValue *vv = vt.Get( name ) )
				vv->Write( cxt.result() );
		}
		else
		{
			int ty0, ty1, bnew = 0;
			VarValue *vv = vt.Get( name );
			if ( !vv )
			{
				vv = vt.Create( name );
				bnew = 1;
			}

			ty0 = vv->Type();
			vv->Read( cxt.arg(1), true );
			ty1 = vv->Type();

			wxString label( name );
			if ( 0 != vuc->GetCase()->Variables().Lookup( name ) )
				label = vuc->GetCase()->Variables().Label(name);

			if ( bnew )
			{
				vuc->GetLog( vuc->GetName() ).push_back( 
					VersionUpgrade::log( VersionUpgrade::VAR_ADDED, 
						"Added new variable '" + label + "' (" + vv_strtypes[ty1] + ") = " + vv->AsString( ',', '|' ) ) );
			}
			else if ( ty0 != ty1 )
			{
				vuc->GetLog( vuc->GetName() ).push_back( 
					VersionUpgrade::log( VersionUpgrade::VAR_CHANGED, 
						"Updated variable '" + label + "' to data type " + vv_strtypes[ty1] + ", value: " + vv->AsString( ',', '|' ) ) );
			}
			else
			{
				vuc->GetLog( vuc->GetName() ).push_back( 
					VersionUpgrade::log( VersionUpgrade::VAR_CHANGED, 
						"Changed variable '" + label + "' value to: " + vv->AsString( ',', '|' ) ) );
			}
		}
	}
}

static void fcall_vuc_varinfo( lk::invoke_t &cxt )
{
	LK_DOC("varinfo", "Obtain information about a variable.", "(string:name):table");
	if ( VersionUpgrade *vuc = static_cast<VersionUpgrade*>(cxt.user_data()) )
		invoke_get_var_info( vuc->GetCase(), cxt.arg(0).as_string(), cxt.result() );
}

static void fcall_vuc_message( lk::invoke_t &cxt )
{
	LK_DOC("message", "Issue an upgrade-related message. Type can be error, warning, notice (default)", "(string:message, [string:type]):none");
	if ( VersionUpgrade *vuc = static_cast<VersionUpgrade*>(cxt.user_data()) )
	{
		int ty = VersionUpgrade::NOTICE;
		if ( cxt.arg_count() > 1)
		{
			wxString sty( cxt.arg(1).as_string().Lower() );
			if ( sty == "error" ) ty = VersionUpgrade::FAIL;
			else if ( sty == "warning" ) ty = VersionUpgrade::WARNING;
		}

		vuc->GetLog( ty >= 0 ? vuc->GetName() : wxEmptyString ).push_back( VersionUpgrade::log( ty, cxt.arg(0).as_string() ) );
	}
}


VersionUpgrade::VersionUpgrade() 
	: m_case( 0 )
{		
	m_env.register_funcs( lk::stdlib_basic() );
	m_env.register_funcs( lk::stdlib_math() );
	m_env.register_funcs( lk::stdlib_string() );
	m_env.register_funcs( lk::stdlib_wxui() );
	m_env.register_funcs( invoke_general_funcs() );
	m_env.register_funcs( invoke_ssc_funcs() );

	m_env.register_funcs( invoke_functions(), this );
}


lk::fcall_t* VersionUpgrade::invoke_functions()
{
	static const lk::fcall_t vec[] = {
		fcall_vuc_value,
		fcall_vuc_varinfo,
		fcall_vuc_case_name,
		fcall_vuc_config,
		fcall_vuc_message,
		0 };
	return (lk::fcall_t*)vec;
}


bool VersionUpgrade::Invoke( Case *c, const wxString &name, lk::node_t *root )
{
	m_case = c;
	m_name = name;

	lk::env_t local_env( &m_env );
	
	try {
		lk::eval e( root, &local_env );
		if ( !e.run() )
		{
			wxString text = "Version upgrade function error:\n";
			for (size_t i=0;i<e.error_count();i++)
				text += e.get_error(i);

			GetLog(name).push_back( log(ERROR, text) );
			return false;
		}
		
	} catch(std::exception &e ){
		GetLog(name).push_back( log(ERROR, "Version upgrade function error: " + wxString("\n") + e.what()) );
		return false;
	}
	
	return true;
}

bool VersionUpgrade::Run( ProjectFile &pf )
{
	// load the version upgrade LK script
	ScriptDatabase sd;
	if ( !sd.LoadFile( SamApp::GetRuntimePath() + "/versions.lk" ) )
	{
		GetLog().push_back( log(FAIL, "Could not open project file version upgrade script.") );
		return false;
	}
		
	std::vector<Case*> cases = pf.GetCases();

	int major, minor, micro;
	size_t file_ver = pf.GetVersionInfo( &major, &minor, &micro );
	
	int sammajor, samminor, sammicro;
	size_t sam_ver = SamApp::Version( &sammajor, &samminor, &sammicro );

	int nr = 0; // find release number for SAM version used to create project file
	while( sam_ver != 0 && sam_ver != file_ver )
		sam_ver = SamApp::Version( &sammajor, &samminor, &sammicro, nr++ );

	if ( sam_ver == 0 )
	{
		// project file was not created by
		// a known previous version of sam...
		// presumably this is an error, but could it be handled
		// for our internal development versions?
		wxString errtext( wxString::Format("Project file version %d.%d.%d was not created with a known SAM release version.", major, minor, micro));
		if ( wxNO == wxMessageBox( errtext, "Version error", wxYES_NO ) )
		{
			GetLog().push_back( log(FAIL, errtext) );
			return false;
		}
		else
			GetLog().push_back( log(WARNING, errtext));
	}
	
	m_env.set_parent( sd.GetEnv() );
	
	for( nr = nr-1; nr >= 0; nr-- )
	{
		sam_ver = SamApp::Version( &sammajor, &samminor, &sammicro, nr );
		if ( lk::node_t *cb = sd.Lookup( "version_upgrade", 
				wxString::Format("%d.%d.%d", sammajor, samminor, sammicro) ) )
			// call the script once for each case
			for( size_t i=0;i<cases.size();i++ )
				Invoke( cases[i], pf.GetCaseName( cases[i] ), cb );
	}

	// don't retain a pointer to the script database environment
	m_env.set_parent( 0 );
	
	return true;
}


std::vector<VersionUpgrade::log> &VersionUpgrade::GetLog( const wxString &name )
{
	return m_log[name];
}

wxString VersionUpgrade::CreateHtmlReport()
{
	wxString html( "<html><body>\n<h3>Project file version upgrade to " + SamApp::VersionStr() + " report.</h3>\n<br>\n" );

	
	if ( m_generalLog.size() > 0 )
		WriteHtml( "General", m_generalLog, html );

	for( LogInfo::iterator it = m_log.begin();
		it != m_log.end();
		++it )
	{
		html += "\n<br><br>\n\n";
		WriteHtml( it->first, it->second, html );
	}

	html += "</body></html>";
	return html;

}

void VersionUpgrade::WriteHtml( const wxString &section, const std::vector<log> &LL, wxString &html )
{
	html += "<table bgcolor=#999999 width=100% cellspacing=1 cellpadding=0><tr width=100%><td><font size=+1 color=#ffffff><b>" + section + "</b></font></td></tr>\n";

	for( int i=LL.size()-1;i>=0;i-- )
	{
		wxString bgcolor("#ffffff");
		switch( LL[i].type )
		{
		case FAIL: bgcolor="#ff0000"; break;
		case WARNING: bgcolor="#ff9933"; break;
		case CONFIG_CHANGE: bgcolor="#f7d1ff"; break;
		case VAR_CHANGED: bgcolor="#d1eba9"; break;
		case VAR_ADDED: bgcolor="#e8d3a7"; break;
		}

		html += "<tr width=100% bgcolor=" + bgcolor + "><td>" + LL[i].message + "</td></tr>\n";
	}

	html += "</table><br>\n";

}