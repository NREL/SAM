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

#include <cmath>
#include <numeric>

#include <wx/simplebook.h>
#include <wx/datstrm.h>
#include <wx/wfstream.h>
#include <wx/zstream.h>
#include <wx/tokenzr.h>

#include <wx/window.h>
#include <wx/html/htmlwin.h>

#include <wex/metro.h>
#include <wex/lkscript.h>

#include <lk/stdlib.h>

#include "project.h"
#include "case.h"
#include "main.h"
#include "invoke.h"


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
	out.Write16( 3 ); // data format version
	out.Write16( (wxUint16)m_verMajor );
	out.Write16((wxUint16)m_verMinor );
	out.Write16((wxUint16)m_verMicro );
	out.Write16((wxUint16)m_verPatch );

	m_properties["ssc_version"] = wxString::Format("%d", ssc_version());
	m_properties["ssc_build_info"] = wxString(ssc_build_info());
	
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

	// suppress wxLogging and system errors from wxLogError in wxWidgets and handle project file reading issues here - see https://github.com/NREL/SAM/issues/393
	// regular logging restored when wxLogNull object goes out of scope.
	wxLogNull logNo; 

	wxDataInputStream in(input);

	wxUint16 code = in.Read16();
	wxUint16 ver = in.Read16();
	m_verMajor = (int)in.Read16();
	m_verMinor = (int)in.Read16();
	m_verMicro = (int)in.Read16();

	// set then reset if fails
	SamApp::Project().m_verMajor = m_verMajor;
	SamApp::Project().m_verMinor = m_verMinor;
	SamApp::Project().m_verMicro = m_verMicro;


	if ( ver > 2 )
		m_verPatch = (int)in.Read16();

	if ( !m_properties.Read( input ) ) m_lastError += "Project properties error.\n";
	if (!m_cases.Read(input)) m_lastError += "Case data error: " + m_cases.GetLastError() + "\n";
	if ( !m_objects.Read( input ) ) m_lastError += "Objects data error: " + m_objects.GetLastError() + "\n";

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
	
	bool ret = false;
	wxUint8 code1, code2, comp;
	in.Read( &code1, 1 );
	in.Read( &code2, 1 );

	if ( code1 != 0x41 || code2 != 0xb9 )
	{
		// old format, uncompressed
		in.Ungetch( code2 );
		in.Ungetch( code1 );
		
		ret = Read(in);
	}
	else
	{
		// determine compression format
		in.Read(&comp, 1);
		if ( comp != 1 ) // unknown compression type
			return false;

		wxZlibInputStream zin( in );
		bool bReadin = Read(zin);
		//		return Read( zin );
		// skips reading in prject files with no cases
		//return bReadin && (m_cases.size() > 0);
		// prompt user to read in project files with no cases
		bool bReadProjectFile = (m_cases.size() > 0);
		if (!bReadProjectFile)
			bReadProjectFile = (wxMessageBox("Load project file with no cases?", "Query", wxYES_NO | wxICON_EXCLAMATION) == wxYES);
		return bReadin && bReadProjectFile;
	}
	return ret;
}

void ProjectFile::SetVersionInfo( int maj, int min, int mic, int patch )
{
	m_verMajor = maj;
	m_verMinor = min;
	m_verMicro = mic;
	m_verPatch = patch;
}

size_t ProjectFile::GetVersionInfo( int *maj, int *min, int *mic, int *patch )
{
	if( maj ) *maj = m_verMajor;
	if( min ) *min = m_verMinor;
	if( mic ) *mic = m_verMicro;
	if( patch ) *patch = m_verPatch;
	return VERSION_VALUE( m_verMajor, m_verMinor, m_verMicro );
}

static void fcall_vuc_case_name( lk::invoke_t &cxt )
{
	LK_DOC( "case_name", "Return the current case's name.", "(none):string");
	if ( VersionUpgrade *vuc = static_cast<VersionUpgrade*>(cxt.user_data()) )
		cxt.result().assign( vuc->GetName() );
}

static void fcall_vuc_config_update_with_old_values(lk::invoke_t& cxt)
{
	LK_DOC("config_update_with_old_values", "Update current case with old values", "(none):table");
	if (VersionUpgrade* vuc = static_cast<VersionUpgrade*>(cxt.user_data()))
	{
		VarTable& vt_old = vuc->GetCase()->OldValues();
		cxt.result().empty_hash();
		for (VarTable::iterator it = vt_old.begin(); it != vt_old.end(); ++it)
		{
			if (VarValue* vv = vuc->GetCase()->Values().Get(it->first))
			{
				if (vv->Type() == (it->second)->Type()) // only update if old variable is of same type
				{
					cxt.result().hash_item(it->first).assign(vv->AsString() + "=" + it->second->AsString() + "\n");
					vv->Copy(*(it->second));
				}
			}
		}
	}

}



static void fcall_vuc_config( lk::invoke_t &cxt )
{
	LK_DOC( "config", "Set or get the current case's configuration", "(string:tech, string:fin, [string:reason]):boolean or (none):table");
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
			wxString reason;
			if ( cxt.arg_count() > 2 ) reason = cxt.arg(2).as_string();
			vuc->GetLog( vuc->GetName() ).push_back( VersionUpgrade::log( 
				VersionUpgrade::CONFIG_CHANGE, "Updated internal configuration name from " + tech + "/" + fin + " to " + tech1 + "/" + fin1 + ".", reason ) );
			cxt.result().assign( vuc->GetCase()->SetConfiguration( tech1, fin1 ) );
		}
	}

}

static void fcall_vuc_value( lk::invoke_t &cxt )
{
	LK_DOC("value", "Set or get a variable's value. New variables may be created.", "(string:name, [variant:value], [string:reason]):variant");
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
			bool bnew = false;
			VarValue *vv = vt.Get( name );
			if ( !vv )
			{
				vv = vt.Create( name );
				bnew = true;
			}

			int type = vv->Type();
			bool ok = vv->Read( cxt.arg(1) );

			wxString reason;
			if ( cxt.arg_count() > 2 ) reason = cxt.arg(2).as_string();

			wxString label( name );
			if ( 0 != vuc->GetCase()->Variables().Lookup( name ) )
				label = vuc->GetCase()->Variables().Label(name);

			wxString valstr(  vv->AsString( ',', '|' ) );
			if ( valstr.Len() > 100 )
			{
				valstr.Truncate( 100 );
				valstr += "...";
			}

			if ( bnew )
			{
				vuc->GetLog( vuc->GetName() ).push_back( 
					VersionUpgrade::log( VersionUpgrade::VAR_ADDED, 
						"Added '" + label + "', " + vv_strtypes[type] + ": " + valstr, reason ) );
			}
			else
			{
				vuc->GetLog( vuc->GetName() ).push_back( 
					VersionUpgrade::log( VersionUpgrade::VAR_CHANGED, 
						"Changed '" + label + "' to: " + valstr, reason ) );
			}

			cxt.result().assign( ok ? 1.0 : 0.0 );
		}
	}
}

static void fcall_vuc_oldvalue( lk::invoke_t &cxt )
{
	LK_DOC( "oldvalue", "Retrieve a variable value from the project file that is no longer valid in the current case configuration, either due to wrong type or name change.", "(string:name):variant" );
	if ( VersionUpgrade *vuc = static_cast<VersionUpgrade*>(cxt.user_data()) )
	{
		VarTable &vt = vuc->GetCase()->OldValues();
		if ( VarValue *vv = vt.Get( cxt.arg(0).as_string() ) )
			vv->Write( cxt.result() );
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

		vuc->GetLog( ty >= 0 ? vuc->GetName() : wxString(wxEmptyString) ).push_back( VersionUpgrade::log( ty, cxt.arg(0).as_string() ) );
	}
}

static void fcall_vuc_retire_tech(lk::invoke_t &cxt)
{
	LK_DOC("retire_tech", "Retire technologies", "(none):none");
	if (VersionUpgrade *vuc = static_cast<VersionUpgrade*>(cxt.user_data()))
	{
        wxString tech(vuc->GetCase()->GetTechnology());
        wxString fin(vuc->GetCase()->GetFinancing());
        Case* retired_case = vuc->GetCase();
        wxString TP(SamApp::Config().Options(tech).TreeParent);
		//if (tech == "DSPT" || tech == "Dish Stirling");
        if (TP == "Retired")
		{
			//Do something to delete the case from the file
            cxt.result().assign(vuc->GetCase()->SetConfiguration("Retired", "None"));
		}
	}
}


VersionUpgrade::VersionUpgrade() 
	: m_case( 0 )
{		
	m_env.register_funcs( lk::stdlib_basic() );
	m_env.register_funcs( lk::stdlib_sysio() );
	m_env.register_funcs( lk::stdlib_math() );
	m_env.register_funcs( lk::stdlib_string() );
	m_env.register_funcs( lk::stdlib_wxui() );
	m_env.register_funcs( invoke_general_funcs() );
	m_env.register_funcs( invoke_ssc_funcs() );
	m_env.register_funcs( wxLKMiscFunctions() );
	m_env.register_funcs( wxLKFileFunctions() );
	m_env.register_funcs( invoke_functions(), this );
}


lk::fcall_t* VersionUpgrade::invoke_functions()
{
	static const lk::fcall_t vec[] = {
		fcall_vuc_value,
		fcall_vuc_oldvalue,
		fcall_vuc_varinfo,
		fcall_vuc_case_name,
		fcall_vuc_config,
		fcall_vuc_config_update_with_old_values,
		fcall_vuc_message,
		fcall_vuc_retire_tech,
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

			GetLog(name).push_back( log(VersionUpgrade::FAIL, text) );
			return false;
		}
		
	} catch(std::exception &e ){
		GetLog(name).push_back( log(VersionUpgrade::FAIL, "Version upgrade function error: " + wxString("\n") + e.what()) );
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

	size_t file_ver = pf.GetVersionInfo( &m_pfMajor, &m_pfMinor, &m_pfMicro );
	
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
		wxString errtext( wxString::Format("Project file version %d.%d.%d was not created with a known SAM release version.", 
			m_pfMajor, m_pfMinor, m_pfMicro));

		if ( wxNO == wxMessageBox( errtext, "Version error", wxYES_NO ) )
		{
			GetLog().push_back( log(FAIL, errtext) );
			return false;
		}
		else
			GetLog().push_back( log(WARNING, errtext));
	}
	
	m_env.set_parent( sd.GetEnv() );

    size_t check_retired_cases = cases.size();
	
	for (nr = nr - 2; nr >= 0; nr--)
	{
		sam_ver = SamApp::Version( &sammajor, &samminor, &sammicro, nr );
		if ( lk::node_t *cb = sd.Lookup( "version_upgrade", 
				wxString::Format("%d.%d.%d", sammajor, samminor, sammicro) ) )
			// call the script once for each case
			for( size_t i=0;i<cases.size();i++ )
			{
				Invoke( cases[i], pf.GetCaseName( cases[i] ), cb );

				wxString tech(cases[i]->GetTechnology());
				if (tech == "Retired")
				{
                    check_retired_cases--;
                    pf.DeleteCase(pf.GetCaseName(cases[i]));
                    
				}
				else {
					// recalculate equations in each case for each consecutive upgrade
					// to make sure all variables are in sync
					if (cases[i]->RecalculateAll(true) < 0)
						GetLog().push_back(log(WARNING, "Error updating calculated values in '" + pf.GetCaseName(cases[i]) + "' during upgrade process.  Please resolve any errors, save the project file, and reopen it."));
				}
			}
	}

	// don't retain a pointer to the script database environment
	m_env.set_parent( 0 );
	
	return true;
}


std::vector<VersionUpgrade::log> &VersionUpgrade::GetLog( const wxString &name )
{
	return m_log[name];
}

wxString VersionUpgrade::CreateHtmlReport( const wxString &file )
{
	wxString html( "<html><body>\n<h3>Version Upgrade Report</h3>\n"
		"<p><font color=#7a7a7a>" + file 
		+ "<br><br>You are opening this file with SAM Version " + SamApp::VersionStr(true) 
		+ ", SSC " + wxString::Format("%d", ssc_version()) + ".<br>\n"
		+ "The file was last saved in SAM Version " + wxString::Format("%d.%d.%d", m_pfMajor, m_pfMinor, m_pfMicro ) + ".<br>\n"
		+ "The table(s) below list input variables that have changed between versions.<br>\n"
		+ "</font></p><br>\n" );

	if ( m_generalLog.size() > 0 )
		WriteHtml( "General", m_generalLog, html );

	if ( m_log.size() > 0 )
	{
		for( LogInfo::iterator it = m_log.begin();
			it != m_log.end();
			++it )
		{
			html += "\n<br><br>\n\n";
			WriteHtml( it->first, it->second, html );
		}
	}
	else
		html += "\n<br><p>No changes detected.</p></br>";

	html += "</body></html>";
	return html;

}

void VersionUpgrade::WriteHtml( const wxString &section, const std::vector<log> &LL, wxString &html )
{
	html += "<table bgcolor=#545454 width=100% cellspacing=1 cellpadding=3><tr width=100%><td><font size=+1 color=#ffffff><b>" + section + "</b></font></td></tr>\n";

	for( int i=0;i<(int)LL.size();i++ )
	{
		wxString bgcolor("#EFEFEF");
		switch( LL[i].type )
		{
		case FAIL: bgcolor="#FDB3B3"; break;
		case WARNING: bgcolor="#FFDAA8"; break;
		case CONFIG_CHANGE: bgcolor="#e8d3a7"; break;
		case VAR_CHANGED: bgcolor="#d1eba9"; break;
		case VAR_ADDED: bgcolor="#cce1e6"; break;
		case VAR_DELETED: bgcolor="#e0ced7"; break;
		}

		wxString reason;
		if ( !LL[i].reason.IsEmpty())
			reason = "<br><b>" + LL[i].reason + "</b>";

		html += "<tr width=100% bgcolor=" + bgcolor + "><td>" + LL[i].message + reason + "</td></tr>\n";
	}

	html += "</table><br>\n";

}

class UpgradeReportDialog : public wxDialog
{
	wxString m_htmlSrc;
public:
	UpgradeReportDialog( const wxString &src ) 
		: wxDialog( SamApp::Window(), wxID_ANY, "Version Upgrade Report", 
		wxDefaultPosition, wxScaleSize(800,700), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER ),
		m_htmlSrc( src )
	{
		SetBackgroundColour( wxMetroTheme::Colour( wxMT_FOREGROUND ) );

		wxHtmlWindow *html = new wxHtmlWindow( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxHW_DEFAULT_STYLE|wxBORDER_NONE );
		html->SetPage( m_htmlSrc );
		html->Show();

		wxBoxSizer *buttons = new wxBoxSizer( wxHORIZONTAL );
		buttons->Add( new wxMetroButton( this, wxID_SAVE, "Save report..." ), 0, wxALL, 0 );
		buttons->AddStretchSpacer();
		buttons->Add( new wxMetroButton( this, wxID_CLOSE, "Close" ), 0, wxALL, 0 );

		wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
		sizer->Add( html, 1, wxALL|wxEXPAND, 0 );
		sizer->Add( buttons, 0, wxALL|wxEXPAND, 0 );
		SetSizer( sizer );

		SetEscapeId( wxID_CANCEL );

		CenterOnParent();
	}

	void OnCommand( wxCommandEvent &evt )
	{
		if ( evt.GetId() == wxID_SAVE )
		{
			wxFileDialog dlg( this, "Save upgrade report file", wxEmptyString, "report.html", "HTML Files (*.html)|*.html", wxFD_SAVE|wxFD_OVERWRITE_PROMPT );
			if ( wxID_OK == dlg.ShowModal() )
			{
				if ( FILE *fp = fopen( (const char*) dlg.GetPath().c_str(), "w" ) )
				{
					fputs( (const char*)m_htmlSrc.c_str(), fp );
					fclose( fp );
				}
				else
					wxMessageBox("Could not open " + dlg.GetPath() + " for writing.");
			}
		}
		else if ( evt.GetId() == wxID_CLOSE )
		{
			if ( IsModal() ) EndModal( wxID_CANCEL );
			else Close();
		}
	}

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE( UpgradeReportDialog, wxDialog )
	EVT_BUTTON( wxID_SAVE, UpgradeReportDialog::OnCommand )
	EVT_BUTTON( wxID_CLOSE, UpgradeReportDialog::OnCommand )
END_EVENT_TABLE()

void VersionUpgrade::ShowReportDialog( const wxString &file, bool modal )
{
	if ( modal )
	{
		UpgradeReportDialog dlg( CreateHtmlReport(file) );
		dlg.ShowModal();
	}
	else
		(new UpgradeReportDialog( CreateHtmlReport( file ) ))->Show();
}
