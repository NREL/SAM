
#include <wx/clipbrd.h>
#include <wx/dcbuffer.h>
#include <wx/filename.h>
#include <wx/notebook.h>
#include <wx/statline.h>
#include <wx/tokenzr.h>
#include <wx/textfile.h>

#include <wex/csv.h>
#include <regex>

#include "widgets.h"

#include "variables.h"
#include "shadingfactors.h"

#include <fstream>

/*********  SHADING BUTTON CTRL ************/

ShadingInputData::ShadingInputData()
{
	clear();
}

void ShadingInputData::save( std::vector<float> &data )
{
	data.clear();
	data.push_back( 2.0 ); // version number of data format - allows for expansion of options in future.

	data.push_back( (en_hourly && hourly.size() == 8760) ? 1.0 : 0.0 );
	data.push_back( (en_mxh && mxh.nrows() == 12 && mxh.ncols() == 24 ) ? 1.0 : 0.0 );
	data.push_back( en_azal ? 1.0 : 0.0 );
	data.push_back( en_diff ? 1.0 : 0.0 );
	data.push_back( -1.0 );
	data.push_back( -1.0 );
	data.push_back( -1.0 );

	for (int i=0;i<8760;i++)
		data.push_back( i < hourly.size() ? hourly[i] : 0.0 );
	
	if ( mxh.nrows() != 12 || mxh.ncols() != 24 )
		mxh.resize_fill(12, 24, 0.0);

	for (int r=0;r<12;r++)
		for (int c=0;c<24;c++)
			data.push_back( mxh.at(r,c) );
	
	data.push_back( azal.nrows() );
	data.push_back( azal.ncols() );
	for (int r=0;r<azal.nrows();r++)
		for (int c=0;c<azal.ncols();c++)
			data.push_back( azal.at(r,c) );
	
	data.push_back( diff );

	data.push_back( data.size() + 1 ); // verification flag that size is consistent
}

void ShadingInputData::clear()
{
	en_hourly = en_mxh = en_azal = en_diff = false;

	hourly.resize( 8760, 0 );

	mxh.resize_fill(12,24, 0.0);

	azal.resize_fill(10, 18, 0.0);

	for ( int c=0;c<18;c++ )
		azal.at(0, c) = c*20;
	for ( int r=0;r<10;r++ )
		azal.at(r, 0) = r*10;
	
	diff = 0.0;
}

bool ShadingInputData::load( const std::vector<float> &data )
{
	clear();

	if (data.size() < 3) return false;
	if (data.size() != (int)data[ data.size() - 1 ]) return false; // verification check
	
	int idx = 0; // indexer to step through data

	int ver = (int)data[idx++];	
	if (ver == 2)
	{
		en_hourly = data[idx++] > 0 ? true : false;
		en_mxh = data[idx++] > 0 ? true : false;
		en_azal = data[idx++] > 0 ? true : false;
		en_diff = data[idx++] > 0 ? true : false;
		idx++; // skip unused -1
		idx++; // skip unused -1
		idx++; // skip unused -1

		hourly.clear();
		hourly.reserve(8760);
		for (int i=0;i<8760;i++)
			hourly.push_back( data[idx++] );

		for (int r=0;r<12;r++)
			for (int c=0;c<24;c++)
				mxh.at(r,c) = data[idx++];		

		int nr = (int)data[idx++];
		int nc = (int)data[idx++];
		azal.resize_fill( nr, nc, 1.0 );
		for (int r=0;r<nr;r++)
			for (int c=0;c<nc;c++)
				azal.at(r,c) = data[idx++];
		
		diff = data[idx++];
		
		int verify = data[idx++];

		return idx == verify;
	}

	return false;
}

void ShadingInputData::write( VarValue *vv )
{
	vv->SetType( VV_TABLE );
	VarTable &tab = vv->Table();
	tab.Set( "en_hourly", VarValue( (bool)en_hourly ) );
	tab.Set( "hourly", VarValue( hourly ) );
	tab.Set( "en_mxh", VarValue( (bool)en_mxh ) );
	tab.Set( "mxh", VarValue( mxh ) );
	tab.Set( "en_azal", VarValue( (bool)en_azal ) );
	tab.Set( "azal", VarValue( azal ) );
	tab.Set( "en_diff", VarValue( (bool)en_diff ) );
	tab.Set( "diff", VarValue( (float)diff ) );
}

bool ShadingInputData::read( VarValue *root )
{
	clear();
	if ( root->Type() == VV_TABLE )
	{
		VarTable &tab = root->Table();
		if ( VarValue *vv = tab.Get( "en_hourly" ) ) en_hourly = vv->Boolean();
		if ( VarValue *vv = tab.Get("hourly") ) hourly = vv->Array();
		if ( VarValue *vv = tab.Get("en_mxh") ) en_mxh = vv->Boolean();
		if ( VarValue *vv = tab.Get("mxh") ) mxh = vv->Matrix();
		if ( VarValue *vv = tab.Get("en_azal") ) en_azal = vv->Boolean();
		if ( VarValue *vv = tab.Get("azal") ) azal = vv->Matrix();
		if ( VarValue *vv = tab.Get("en_diff") ) en_diff = vv->Boolean();
		if ( VarValue *vv = tab.Get("diff") ) diff = vv->Value();
		return true;
	}
	else
		return false;
}


static const char *hourly_text = "The Hourly 8760 option is appropriate if you have a set of hourly beam shading losses for each of the 8,760 hours in a year. ";
static const char *mxh_text = "The Month by Hour option allows you to specify a set of 288 (12 months x 24 hours) beam shading losses that apply to the 24 hours of the day for each month of the year. Select a cell or group of cells and type a number between 0% and 100% to assign values to the table by hand. Click Import to import a table of values from a properly formatted text file. ";
static const char *azal_text = "The Azimuth by Altitude option allows you to specify a set of beam shading losses for different sun positions.\n"
  "1. Define the size of the table by entering values for the number of rows and columns.\n"
  "2. Enter solar azimuth values from 0 to 360 degrees in the first row of the table, where 0 = north, 90 = east, 180 = south, and 270 = west.\n"
  "3. Enter solar altitude values from 0 to 90 degrees in the first column of the table, where zero is on the horizon.\n"
  "4. Enter shading losses as the shaded percentage of the beam component of the incident radiation in the remaining table cells.\n"
  "Click Paste to populate the table from your computer\'s clipboard, or click Import to import a table of values from a properly formatted text file.  ";
static const char *diff_text = "The constant sky diffuse shading loss reduces the overall diffuse irradiance available by the specified loss.  Valid values are between 0% and 100%.";

enum { ID_ENABLE_HOURLY = ::wxID_HIGHEST+999,
	ID_ENABLE_MXH, ID_ENABLE_AZAL, ID_ENABLE_DIFF,
	ID_IMPORT_PVSYST_NEAR_SHADING,
	ID_IMPORT_SUNEYE_HOURLY,
	ID_IMPORT_SUNEYE_OBSTRUCTIONS,
	ID_IMPORT_SOLPATH_MXH};

class ShadingDialog : public wxDialog
{
	wxScrolledWindow *m_scrollWin;

	wxCheckBox *m_enableHourly;
	AFDataArrayButton *m_hourly;
	wxStaticText *m_textHourly;
	 
	wxCheckBox *m_enableMxH;
	AFMonthByHourFactorCtrl *m_mxh;
	wxStaticText *m_textMxH;

	wxCheckBox *m_enableAzal;
	AFDataMatrixCtrl *m_azal;
	wxStaticText *m_textAzal;

	wxCheckBox *m_enableDiffuse;
	wxNumericCtrl *m_diffuseFrac;
	wxStaticText *m_textDiffuse;

public:
	ShadingDialog( wxWindow *parent, const wxString &descText )
		: wxDialog( parent, wxID_ANY, 
			wxString("Edit shading data") + wxString( (!descText.IsEmpty() ? ": " : "") ) + descText, 
			wxDefaultPosition, wxDefaultSize, 
			wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER)
	{
		SetEscapeId( wxID_CANCEL );

		SetClientSize( 870, 600 );

		m_scrollWin = new wxScrolledWindow( this, wxID_ANY );
		m_scrollWin->SetScrollRate( 50, 50 );

		m_enableHourly = new wxCheckBox( m_scrollWin, ID_ENABLE_HOURLY, "Enable hourly beam irradiance shading losses" );
		m_hourly = new AFDataArrayButton( m_scrollWin, wxID_ANY );
		m_hourly->SetMode( DATA_ARRAY_8760_ONLY );

		m_enableMxH = new wxCheckBox( m_scrollWin, ID_ENABLE_MXH, "Enable month by hour beam irradiance shading losses" );
		m_mxh = new AFMonthByHourFactorCtrl( m_scrollWin, wxID_ANY );
		m_mxh->SetInitialSize( wxSize(900,330) );

		m_enableAzal = new wxCheckBox( m_scrollWin, ID_ENABLE_AZAL, "Enable solar azimuth by altitude beam irradiance shading loss table" );
		m_azal = new AFDataMatrixCtrl( m_scrollWin, wxID_ANY );
		m_azal->SetInitialSize( wxSize(900,280) );
		m_azal->ShowLabels( false );

		matrix_t<float> data(10, 18, 1.0);
		for ( int c=0;c<18;c++ )
			data.at(0, c) = c*20;
		for ( int r=0;r<10;r++ )
			data.at(r, 0) = r*10;
		m_azal->SetData( data );
		
		m_enableDiffuse = new wxCheckBox( m_scrollWin, ID_ENABLE_DIFF, "Enable sky diffuse shading loss (constant)" );
		m_diffuseFrac = new wxNumericCtrl( m_scrollWin, wxID_ANY, 0.0 );
		
		wxSizer *import_tools = new wxStaticBoxSizer( wxHORIZONTAL, m_scrollWin, "Import shading data from external tools");
		import_tools->Add( new wxButton( m_scrollWin, ID_IMPORT_PVSYST_NEAR_SHADING, "PVsyst near shading..." ), 0, wxALL, 3 );
		import_tools->Add( new wxButton( m_scrollWin, ID_IMPORT_SUNEYE_HOURLY, "SunEye hourly shading..." ), 0, wxALL, 3 );
		import_tools->Add( new wxButton( m_scrollWin, ID_IMPORT_SUNEYE_OBSTRUCTIONS, "SunEye obstructions table..." ), 0, wxALL, 3 );
		import_tools->Add( new wxButton( m_scrollWin, ID_IMPORT_SOLPATH_MXH, "SolarPathfinder month by hour shading..." ), 0, wxALL, 3 );

		wxColour text_color( 0, 128, 192 );
		int wrap_width = 700;

		wxSizer *scroll = new wxBoxSizer( wxVERTICAL );
		scroll->Add( import_tools, 0, wxALL, 5 );
		scroll->Add( new wxStaticLine( m_scrollWin ), 0, wxALL|wxEXPAND );

		scroll->Add( m_enableHourly, 0, wxALL|wxEXPAND, 5 );
		scroll->Add( m_textHourly = new wxStaticText( m_scrollWin, wxID_ANY, hourly_text ), 0, wxALL|wxEXPAND, 10 );
		scroll->Add( m_hourly, 0, wxALL, 5 );
		m_textHourly->Wrap( wrap_width );
		m_textHourly->SetForegroundColour( text_color );
		
		scroll->Add( new wxStaticLine( m_scrollWin ), 0, wxALL|wxEXPAND );
		scroll->Add( m_enableMxH, 0, wxALL|wxEXPAND, 5 );
		scroll->Add( m_textMxH = new wxStaticText( m_scrollWin, wxID_ANY, mxh_text ), 0, wxALL|wxEXPAND, 10 );
		scroll->Add( m_mxh, 0, wxALL, 5  );
		m_textMxH->Wrap( wrap_width );
		m_textMxH->SetForegroundColour( text_color );
		
		scroll->Add( new wxStaticLine( m_scrollWin ), 0, wxALL|wxEXPAND );
		scroll->Add( m_enableAzal, 0, wxALL|wxEXPAND, 5 );
		scroll->Add( m_textAzal = new wxStaticText( m_scrollWin, wxID_ANY, azal_text ), 0, wxALL|wxEXPAND, 10 );
		scroll->Add( m_azal, 0, wxALL, 5  );
		m_textAzal->Wrap( wrap_width );
		m_textAzal->SetForegroundColour( text_color );		

		scroll->Add( new wxStaticLine( m_scrollWin ), 0, wxALL|wxEXPAND );
		scroll->Add( m_enableDiffuse, 0, wxALL|wxEXPAND, 5 );
		scroll->Add( m_textDiffuse = new wxStaticText( m_scrollWin, wxID_ANY, diff_text ), 0, wxALL|wxEXPAND, 10 );
		scroll->Add( m_diffuseFrac, 0, wxALL, 5  );
		m_textDiffuse->Wrap( wrap_width );
		m_textDiffuse->SetForegroundColour( text_color );	

		m_scrollWin->SetSizer( scroll );

		wxSizer *buttons = new wxBoxSizer( wxHORIZONTAL );
		buttons->AddStretchSpacer(1);
		buttons->Add( new wxButton(this, wxID_OK, "OK" ), 0, wxALL|wxEXPAND, 3 );
		buttons->Add( new wxButton(this, wxID_CANCEL, "Cancel" ), 0, wxALL|wxEXPAND, 3  );

		wxSizer *box = new wxBoxSizer(wxVERTICAL);
		box->Add( m_scrollWin, 1, wxALL|wxEXPAND );
		box->Add( buttons, 0, wxALL|wxEXPAND );
		SetSizer( box );

		UpdateVisibility();
	}

	void UpdateVisibility()
	{
		m_hourly->Show( m_enableHourly->IsChecked() );
		m_textHourly->Show( m_enableHourly->IsChecked() );
		
		m_mxh->Show( m_enableMxH->IsChecked() );
		m_textMxH->Show( m_enableMxH->IsChecked() );

		m_azal->Show( m_enableAzal->IsChecked() );
		m_textAzal->Show( m_enableAzal->IsChecked() );

		m_diffuseFrac->Show( m_enableDiffuse->IsChecked() );
		m_textDiffuse->Show( m_enableDiffuse->IsChecked() );

		m_scrollWin->FitInside();
		m_scrollWin->Refresh();
	}
	
	void ImportData( wxCommandEvent &e )
	{
		ShadingInputData dat;
		switch( e.GetId() )
		{
		case ID_IMPORT_PVSYST_NEAR_SHADING:
			if ( ImportPVsystNearShading( dat, this ) )
			{
				wxMessageBox( Load( dat, false ) );
				UpdateVisibility();
			}
			break;
		case ID_IMPORT_SUNEYE_HOURLY:
			if ( ImportSunEyeHourly( dat, this ) )
			{
				wxMessageBox( Load( dat, false ) );
				UpdateVisibility();
			}
			break;
		case ID_IMPORT_SUNEYE_OBSTRUCTIONS:
			if ( ImportSunEyeObstructions( dat, this ) )
			{
				wxMessageBox( Load( dat, false ) );
				UpdateVisibility();
			}
			break;
		case ID_IMPORT_SOLPATH_MXH:
			if ( ImportSolPathMonthByHour( dat, this ) )
			{
				wxMessageBox( Load( dat, false ) );
				UpdateVisibility();
			}
			break;
		}
	}
	
	void OnCommand( wxCommandEvent &e )
	{
		switch( e.GetId() )
		{
		case ID_ENABLE_HOURLY:
		case ID_ENABLE_MXH:
		case ID_ENABLE_AZAL:
		case ID_ENABLE_DIFF:
			UpdateVisibility();
			break;
		}
	}
	
	
	void OnClose( wxCloseEvent & )
	{
		EndModal( wxID_CANCEL );
	}

	wxString Load( ShadingInputData &sh, bool all = true )
	{
		wxString stat;

		if ( all || sh.en_hourly )
		{
			m_enableHourly->SetValue( sh.en_hourly );
			m_hourly->Set( sh.hourly );
			stat += "Updated hourly beam shading losses.\n";
		}

		if ( all || sh.en_mxh )
		{
			m_enableMxH->SetValue( sh.en_mxh );
			m_mxh->SetData( sh.mxh );
			stat += "Updated month-by-hour beam shading loss table.\n";
		}

		if ( all || sh.en_azal )
		{
			m_enableAzal->SetValue( sh.en_azal );
			m_azal->SetData( sh.azal );
			stat += "Updated azimuth-by-altitude beam shading factor table.\n";
		}

		if ( all || sh.en_diff )
		{
			m_enableDiffuse->SetValue( sh.en_diff );
			m_diffuseFrac->SetValue( sh.diff );
			stat += "Updated constant sky diffuse factor.\n";
		}

		UpdateVisibility();

		return stat;
	}

	void Save( ShadingInputData &sh )
	{
		sh.en_hourly = m_enableHourly->IsChecked();
		m_hourly->Get( sh.hourly );

		sh.en_mxh = m_enableMxH->IsChecked();
		sh.mxh.copy( m_mxh->GetData() );

		sh.en_azal = m_enableAzal->IsChecked();
		m_azal->GetData( sh.azal );

		sh.en_diff = m_enableDiffuse->IsChecked();
		sh.diff = m_diffuseFrac->Value();
	}

	DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE( ShadingDialog, wxDialog )
	EVT_CLOSE( ShadingDialog::OnClose )
	EVT_CHECKBOX( ID_ENABLE_HOURLY, ShadingDialog::OnCommand )
	EVT_CHECKBOX( ID_ENABLE_MXH, ShadingDialog::OnCommand )
	EVT_CHECKBOX( ID_ENABLE_AZAL, ShadingDialog::OnCommand )
	EVT_CHECKBOX( ID_ENABLE_DIFF, ShadingDialog::OnCommand )
	
	EVT_BUTTON( ID_IMPORT_PVSYST_NEAR_SHADING, ShadingDialog::ImportData )
	EVT_BUTTON( ID_IMPORT_SUNEYE_HOURLY, ShadingDialog::ImportData )
	EVT_BUTTON( ID_IMPORT_SUNEYE_OBSTRUCTIONS, ShadingDialog::ImportData )
	EVT_BUTTON( ID_IMPORT_SOLPATH_MXH, ShadingDialog::ImportData )
END_EVENT_TABLE()



BEGIN_EVENT_TABLE(ShadingButtonCtrl, wxButton)
EVT_BUTTON(wxID_ANY, ShadingButtonCtrl::OnPressed)
END_EVENT_TABLE()

ShadingButtonCtrl::ShadingButtonCtrl( wxWindow *parent, int id, 
	const wxPoint &pos, const wxSize &size)
	: wxButton( parent, id, "Edit shading...", pos, size )
{
}

void ShadingButtonCtrl::Write( VarValue *vv )
{
	m_shad.write( vv );
}

bool ShadingButtonCtrl::Read( VarValue *vv )
{
	return m_shad.read( vv );
}

void ShadingButtonCtrl::OnPressed(wxCommandEvent &evt)
{
	ShadingDialog dlg( this, m_descText );
	dlg.Load( m_shad );
	
	if (dlg.ShowModal()==wxID_OK)
	{
		dlg.Save( m_shad );
		evt.Skip(); // allow event to propagate indicating underlying value changed
	}
}


/***************************************************************
  Utility functions to import externally generated shading data
  **************************************************************/


bool ImportPVsystNearShading( ShadingInputData &dat, wxWindow *parent )
{
	//ask about version of PVsyst (5 versus 6) due to change in shading convention
	bool new_version = true;
	wxString msg = "Is this shading file from PVsyst version 6 or newer?";
	msg += "\n\nPVsyst changed their shading convention starting in Version 6 and later such that 0 now equals no shade and 1 equals full shade. ";
	msg += "To import a file from PVsyst versions 6 or newer, click Yes below. However, you may still import a file from versions 5 and older by selecting No below.";
	int ret = wxMessageBox(msg, "Important Notice", wxICON_EXCLAMATION | wxYES_NO, parent);
	if (ret == wxNO)
		new_version = false;

	//read in the file
	wxString buf;
	double diffuse = 0.0;
	int i;
	int j;

	bool readdata = false;
	bool readok = true;
	bool headingok = true;
	bool colok = true;
	int linesok = 0;

		
	matrix_t<float> azaltvals;
	azaltvals.resize_fill(11,20, 0.0f);
	azaltvals.at(0,0) = 0.0f;

	for (i=1;i<20;i++) azaltvals.at(0,i) = 20*(i-1);  // removed -180 degree offset to account for new azimuth convention (180=south) 4/2/2012, apd
	for (i=1;i<10;i++) azaltvals.at(i,0) = 10*i;
	azaltvals.at(10,0) = 2.0f;

	wxFileDialog fdlg(parent, "Import PVsyst Near Shading File");
	if (fdlg.ShowModal() != wxID_OK) return false;
	wxString file = fdlg.GetPath();

	wxTextFile tf;
	if ( !tf.Open( file ) )
	{
		wxMessageBox("Could not open file for read:\n\n" + file);
		return false;
	}

	j = 0;
	buf = tf.GetFirstLine();
	while( !tf.Eof() )
	{
		wxArrayString lnp = wxStringTokenize( buf, ";:,\t" );
		if (readdata == false && j == 0)
		{
			if (lnp.Count() > 0)
			{
				if (lnp.Item(0) == "Height") readdata = true;
			}
		}
		else if (j < 10)
		{
			j++;
			if (lnp.Count() != 20)
			{
				colok = false;
				readok = false;
				break;
			}
			else
			{
				for (i = 0; i<20; i++) // read in Altitude in column zero
				{
					if (i == 0) azaltvals.at(j, i) = (float)wxAtof(lnp[i]);		//do not change azimuth values
					else
					{
						if (lnp.Item(i) == "Behind")
							azaltvals.at(j, i) = 100;	//"Behind" means it is fully behind another obstruction
						else if (new_version) //PVsyst versions 6 and newer: 0 means no shade, 1 means full shade
							azaltvals.at(j, i) = (float)wxAtof(lnp[i]) * 100; //convert to percentage
						else //PVsyst versions 5 and older: 1 means no shade, 0 means full shade
							azaltvals.at(j, i) = (1 - (float)wxAtof(lnp[i])) * 100;	//convert from factor to loss
					}
				}
			}
		}
		else if (j == 10)
		{
			if (lnp.Count()== 3)
			{
				if (new_version) //PVsyst versions 6 and newer: 0 means no shade, 1 means full shade
					diffuse = (float)wxAtof(lnp[1]) * 100; //convert to percentage
				else //PVsyst versions 5 and older: 1 means no shade, 0 means full shade
					diffuse = (1 - (float)wxAtof(lnp[1])) * 100; //convert from factor to loss
			}
			else
			{
				readok = false;
				colok = false;
				break;
			}
			j++;
		}
		else j++;

		buf = tf.GetNextLine();
	}
	
	if (j < 11)
	{
		readok = false;
		linesok = -1;
	}
	else if (j > 11)
	{
		readok = false;
		linesok = 1;
	}
	if (readdata != true)
	{
		readok = false;
		headingok = false;
	}
	
	if (readok)
	{
		// re-sort from small altitude to large, if necessary 
		if (azaltvals.at(10, 0) < azaltvals.at(1, 0))
		{
			azaltvals.resize_preserve(12, 20, 1.0);

			for (j = 1; j < 12 / 2; j++)
			{
				for (i = 0; i < 20; i++) azaltvals.at(11, i) = azaltvals.at(j, i);
				for (i = 0; i < 20; i++) azaltvals.at(j, i) = azaltvals.at(11-j, i);
				for (i = 0; i < 20; i++) azaltvals.at(11-j, i) = azaltvals.at(11, i);
			}

			azaltvals.resize_preserve(11, 20, 1.0);
		}

		dat.clear();
		dat.en_azal = true;
		dat.azal.copy( azaltvals );

		dat.en_diff = true;
		dat.diff = diffuse;

		return true;
	}
	else
	{
		wxString m = "Invalid file format.\n\n";
		if (!headingok)	m.Append("Invalid heading format.\n");
		if (!colok) m.Append("Invalid number of columns.\n");
		if (linesok == -1) m.Append("File contains fewer lines than expected.\n");
		if (linesok == 1) m.Append("File contains more lines than expected.\n");
		wxMessageBox(m);

		return false;
	}
}
	
bool ImportSunEyeHourly( ShadingInputData &dat, wxWindow *parent )
{
	wxFileDialog fdlg(parent, "Import SunEye Shading File");
	if (fdlg.ShowModal() != wxID_OK) return false;
	
	wxTextFile tf;
	if ( !tf.Open( fdlg.GetPath() ) )
	{
		wxMessageBox("Could not open file for read:\n\n" + fdlg.GetPath());
		return false;
	}

	wxString buf;
	int i;
	bool readdata = false;
	bool readok = true;
	bool headingok = true;
	bool colok = true;
	int linesok = 0;
	int day = 0;
	int start_minute=0;
	int start_hour=0;
	int end_minute=0;
	int end_hour=0;
	int hour_duration=0; // how many hours (including incomplete hours) in the Suneye file
	double beam[8760];
	for (i=0;i<8760;i++) beam[i]=0.0;

	buf = tf.GetFirstLine();
	while( !tf.Eof() )
	{
		wxArrayString lnp = wxStringTokenize(buf, ",", wxTOKEN_RET_EMPTY_ALL);
		if (readdata == false)
		{
			if (lnp.Count() > 0)
			{
				if (lnp.Item(0) == "begin data") 
				{
					readdata = true;
					int iend = lnp.Count()-1;
					int icolon = 0;
					icolon = lnp.Item(1).find(":");
					if (icolon>0) 
					{
						start_minute = wxAtoi(lnp.Item(1).substr(icolon+1,2));
						start_hour = wxAtoi(lnp.Item(1).substr(0,icolon));
					}
					icolon = lnp.Item(iend).find(":");
					if (icolon>0) 
					{
						end_minute = wxAtoi(lnp.Item(iend).substr(icolon+1,2));
						end_hour = wxAtoi(lnp.Item(iend).substr(0,icolon));
					}
					// check for valid duration
					if ((start_hour==0) || (end_hour==0))
					{
						readdata=false;
						break;
					}
					hour_duration = end_hour - start_hour + 1;
				}
			}
		}
		else
		{
			// shj update 5/25/11 - read in begin data and to end - no fixed count
			// assume that 15 minute intervals and use start and end time and adjust to hour
			// JMF update 10/17/2014- average all values for an hour instead of taking the midpoint of the hour
			int index = 1; //keep track of where you are in the row- starts at 1 because of the date column.
			for (i=0;i<hour_duration;i++)
			{
				//compute which hour to enter the shading factor into
				int x = day*24+start_hour+i;
				if (x >= 8760)
				{
					readok = false;
					break;
				}

				//how many 15-min entries are in this hour?
				int count = 0;
				if (i == 0) //first hour
					count = (60 - start_minute) / 15;
				else if (i == hour_duration - 1) //last hour
					count = end_minute / 15 + 1;
				else //whole hours in between
					count = 4;

				//loop through the correct number of 15-minute entries and to calculate an average shading value
				double total = 0;
				for (int j = 0; j < count; j++)
				{
					if (lnp.Item(index).IsEmpty()) total += 0;
					else total += wxAtof(lnp.Item(index));
					index++; //don't forget to increment the index so that you read the next cell
				}
				
				//compute average and assign it to the appropriate hour
				beam[x] = (1 - (total / count)) * 100; //don't forget to convert it to a loss factor
			}
			day++;
		}

		buf = tf.GetNextLine();
	}

	if (day !=365)
	{
		readok = false;
		if (day - 365 < 0 && day != 0) linesok = -1;
		if (day - 365 > 0 && day != 0) linesok = 1;
	}

	if (readdata == false)
	{
		readok = false;
		headingok = false;
	}
	
	if (readok)
	{
		dat.clear();
		dat.en_hourly = true;
		dat.hourly.clear();
		dat.hourly.reserve( 8760 );
		for( size_t i=0;i<8760;i++ ) 
			dat.hourly.push_back( beam[i] );
		return true;
	}
	else
	{
		wxString m = "Invalid file format.\n\n";
		if (!headingok)	m.Append("Invalid heading format.\n");
		if (!colok) m.Append("Invalid number of columns.\n");
		if (linesok == -1) m.Append("File contains fewer lines than expected.\n");
		if (linesok == 1) m.Append("File contains more lines than expected.\n");
		wxMessageBox(m);
		return false;
	}

}
	
bool ImportSunEyeObstructions( ShadingInputData &dat, wxWindow *parent )
{


	wxFileDialog fdlg(parent, "Import SunEye Obstruction Elevations File");
	if (fdlg.ShowModal() != wxID_OK) return false;
	wxString file = fdlg.GetPath();
	wxTextFile tf;
	if ( !tf.Open( file ) )
	{
		wxMessageBox("Could not open file for read:\n\n" + file);
		return false;
	}

	wxString buf;
	int j = -2;
	bool readdata = false;
	bool readok = true;
	bool headingok = true;
	bool colok = true;
	int linesok = 0;
	double azi[361];
	double alt[361];
		
	matrix_t<float> azaltvals;
	azaltvals.resize_fill(91,362, 0.0);
	azaltvals.at(0,0) = 0.;

	buf = tf.GetFirstLine();
	while( !tf.Eof() )
	{
		wxArrayString lnp = wxStringTokenize( buf, "," );
		if (readdata == false)
		{
			if (lnp.Count() > 0 && j == -2)
			{
				if (lnp.Item(0) == "begin data")
				{
					readdata = true;
					j++;
				}
			}
		}
		else if (readdata == true && j == -1) j++;
		else
		{
			if (j <= 360)
			{
// Modify to read in ObstructionElevation.csv (average in column 3 and then max and then value for each skyline
// Works with individual skyline obstrucitons (e.g. Sky01ObstrucitonElevations.csv) or with average.
//				if (lnp.Count() != 3)
				if (lnp.Count() < 3)
				{
					colok = false;
					readok = false;
					break;
				}
				else
				{
					azi[j] = wxAtof(lnp[0]); //first column contains compass heading azimuth values (0=north, 90=east)
					alt[j] = wxAtof(lnp[2]);
				}
				j++;
			}
			else j++;
		}
		buf = tf.GetNextLine();
	}

	if (j < 361)
	{
		readok = false;
		linesok = -1;
	}
	else if (j > 361)
	{
		readok = false;
		linesok = 1;
	}
	if (readdata != true)
	{
		readok = false;
		headingok = false;
	}


	if (readok)
	{
		//copy azimuth/compass values into the first row
		for (int i=1;i<362;i++)
			azaltvals.at(0,i) = azi[i-1];

		//elevation always goes from 1-90 degrees
		for (int i=1;i<91;i++)
			azaltvals.at(i,0) = i;

		//loop through all azimuth values
		for (int j=0;j<362;j++)
		{
			//check for a valid elevation value
			if (alt[j]<0 || alt[j]>90) //changed from && to || 6/18/15 jmf
			{
				wxMessageBox("Error: Elevations Must be less than 90 degrees and greater than 0 degrees");
				return false;
			}
			else
			{
				//changed jmf 6/18/15- values UP TO the altitude value should be fully shaded.
				for (int i = 1; i <= alt[j]; i++)
					azaltvals.at(i, j + 1) = 100;
			}
		}

		dat.clear();

		dat.en_azal = true;
		dat.azal = azaltvals;

		return true;
	}
	else
	{
		wxString m = "Invalid file format.\n\n";
		if (!headingok)	m.Append("Invalid heading format.\n");
		if (!colok) m.Append("Invalid number of columns.\n");
		if (linesok == -1) m.Append("File contains fewer lines than expected.\n");
		if (linesok == 1) m.Append("File contains more lines than expected.\n");
		wxMessageBox(m);

		return false;
	}
}

bool ImportSolPathMonthByHour( ShadingInputData &dat, wxWindow *parent )
{
//	std::fstream fs;
//	fs.open("C:\\Users\\dryberg\\Desktop\\SAM\\weekly\\150615\\solarpathfinder\\output.txt", std::fstream::out);

	wxFileDialog fdlg(parent, "Import Solar Pathfinder Month By Hour Shading File");
	if (fdlg.ShowModal() != wxID_OK) return false;
	wxString file = fdlg.GetPath();

	wxTextFile tf;
	if ( !tf.Open( file ) )
	{
		wxMessageBox("Could not open file for read:\n\n" + file);
		return false;
	}

	// read in month by hour grid from first image in shading file - Oliver Hellwig - Solar Pathfinder programmer - every half hour - read 2 value and average for now
	// 12:00 AM	12:30 AM	1:00 AM	1:30 AM	2:00 AM	2:30 AM	3:00 AM	3:30 AM	4:00 AM	4:30 AM	5:00 AM	5:30 AM	6:00 AM	6:30 AM	7:00 AM	7:30 AM	8:00 AM	8:30 AM	9:00 AM	9:30 AM	10:00 AM	10:30 AM	11:00 AM	11:30 AM	12:00 PM	12:30 PM	1:00 PM	1:30 PM	2:00 PM	2:30 PM	3:00 PM	3:30 PM	4:00 PM	4:30 PM	5:00 PM	5:30 PM	6:00 PM	6:30 PM	7:00 PM	7:30 PM	8:00 PM	8:30 PM	9:00 PM	9:30 PM	10:00 PM	10:30 PM	11:00 PM	11:30 PM
	// 12,24,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
	// double array 12rowsx24columns Solar pathfinder percentages to fractions

	wxString buf;
	int i, imageCount = 0;
	bool readdata = false;
	bool readok = true;
	bool headingok = true;
	int month=0; // 
	double beam[290];
	for (i=0;i<290;i++) beam[i]=0.0;
	beam[0]=12;
	beam[1]=24;

	buf = tf.GetFirstLine();

// data at half hour is recorded for hour in 8760 shading file - e.g. Jan-1 5:30 data recoded at hour 5
	while( !tf.Eof() )
	{
		wxArrayString lnp = wxStringTokenize(buf, ",", wxTOKEN_RET_EMPTY_ALL);
		if (readdata == false)
		{
			if (lnp.Count() > 0)
			{
				if (std::regex_match( (std::string)lnp.Item(0), std::regex("Image Layout Number [0-9]") ) )
				{
					imageCount++;
					month = 0;
					buf = tf.GetNextLine();
					readdata = true;
				}
			}
		}
		else
		{
			if (month==11)
			{
				readdata = false;
			}
			for (i=0;i<24;i++)
			{
				int ndex=i+month*24+2; // skip 12x24 array size
				if (ndex > 289) 
				{
					readok=false;
					break;
				}
				// average hour and half hour values starting at midnight (skip row label)
				if (imageCount==1){
					beam[ndex] = 100- (wxAtof(lnp.Item(2*i+1))+wxAtof(lnp.Item(2*i+1+1)))/2.0;	//convert from a factor to a loss
				} else {
					beam[ndex] += 100- (wxAtof(lnp.Item(2*i+1))+wxAtof(lnp.Item(2*i+1+1)))/2.0;	//convert from a factor to a loss
				}
			}
			month++;
		}
		buf = tf.GetNextLine();
		if (tf.Eof() ) readdata = true;
	}

	if (readdata == false || imageCount == 0)
	{
		readok = false;
		headingok = false;
	}

	if (readok)
	{
		dat.clear();
		dat.en_mxh = true;
		dat.mxh.resize_fill(12,24, 0.0);
		for (int r=0;r<12;r++)
			for (int c=0;c<24;c++)
				dat.mxh.at(r,c) = beam[ 24*r+c+2 ] / imageCount;
		return true;
	}
	else
	{
		wxString m = "Invalid file format.\n\n";
		wxMessageBox(m);
		return false;
	}

}
