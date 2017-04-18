#include <stdio.h>
#include <wx/string.h>

#include "wex/dview/dvtimeseriesdataset.h"
#include "wex/dview/dvfilereader.h"
#include "wex/dview/dvplotctrl.h"

#include <wx/wfstream.h>
#include <wx/txtstrm.h>
#include <wx/tokenzr.h>


static bool AllocReadLine(FILE *fp, wxString &buf, int prealloc = 256)
{
	char c;

	buf = "";
	if (prealloc > 10)
		buf.Alloc( prealloc );

	// read the whole line, 1 character at a time, no concern about buffer length
	while ( (c=fgetc(fp)) != EOF && c != '\n' && c != '\r')
		buf += c;

	// handle windows <CR><LF>
	if (c == '\r')
	{
		if ( (c=fgetc(fp)) != '\n')
			ungetc(c,fp);
	}

	// handle a stray <CR>
	if (c == '\n')
	{
		if ( (c=fgetc(fp)) != '\r')
			ungetc(c,fp);
	}

	return !(buf.Len() == 0 && c == EOF);
}

struct WFHeader
{
	WFHeader();

	wxString stationID;
	wxString city;
	wxString state;
	double timezone;
	double latitude;
	double longitude;
	double elevation;
	wxString swrfCreationDate;
	wxString swrfCreationTime;
	wxString swrfServer;
	double lat_requested;
	double lon_requested;
	double dist_from_request;
	int time_step_secs;
};

#define WF_ERR 0
#define WF_TM2 2
#define WF_EPW 3
#define WF_TM3 7
#define WF_SWRF 8
#define WF_SMW 9

#include <stdio.h>
#include <stdlib.h>

#include <wx/filefn.h>
#include <wx/filename.h>
#include <wx/tokenzr.h>


WFHeader::WFHeader()
{
	city = state = "INVALID";
	timezone = latitude = longitude = elevation = -1.0;
	time_step_secs = 60;
}

static double ConvertDegMinSec(double degrees, 
								double minutes,
								double seconds, 
								char direction)
{
	double dd = degrees + minutes/60.0 + seconds/3600.0;
	if ( tolower(direction) == 's' || tolower(direction) == 'w')
		dd = 0 - dd;

	return dd;
}

static bool ParseTM2Header(const wxString &file, WFHeader &info, FILE **wfretfp)
{
	//  0     1                      2    3 4  5 6  7 8    9   10
	//  23183 PHOENIX                AZ  -7 N 33 26 W 112  1   339
	FILE *fp = fopen(file.c_str(), "r");
	if (!fp)
		return false;
	
	double deg, min;
	char dir;
	wxString line, buf;
	AllocReadLine(fp, line);
	wxStringTokenizer tokens(line, " \t");

	
	buf = tokens.GetNextToken(); // skip the first one
	// LOCATION
	info.city = tokens.GetNextToken();
	// State is sometimes blank - email from Paul 11/17/09 - need formatted input char by char
	info.state = tokens.GetNextToken();
	if (info.state.IsNumber()) {
		buf = info.state;
		info.state = "";
	} else {
	// TIMEZONE
		buf = tokens.GetNextToken();
	}
	sscanf(buf.c_str(), "%lg", &info.timezone );
	// LATITUDE
	buf = tokens.GetNextToken(); if (buf.Len() > 0) dir = buf[0]; else return false;
	buf = tokens.GetNextToken(); sscanf(buf.c_str(), "%lg", &deg );
	buf = tokens.GetNextToken(); sscanf(buf.c_str(), "%lg", &min );
	info.latitude = ConvertDegMinSec(deg, min, 0, dir);
	// LONGITUDE
	buf = tokens.GetNextToken(); if (buf.Len() > 0) dir = buf[0]; else return false;
	buf = tokens.GetNextToken(); sscanf(buf.c_str(), "%lg", &deg );
	buf = tokens.GetNextToken(); sscanf(buf.c_str(), "%lg", &min );
	info.longitude = ConvertDegMinSec(deg, min, 0, dir);
	// ELEVATION
	buf = tokens.GetNextToken();
	sscanf(buf.c_str(), "%lg", &info.elevation );
	
	// no lines to skip, all remaining lines are data

	if (wfretfp)
		*wfretfp = fp;
	else
		fclose(fp);

	return true;
}

static bool ParseTM3Header(const wxString &file, WFHeader &info, FILE **wfretfp)
{
	//724880,"RENO TAHOE INTERNATIONAL AP",NV,-8.0,39.483,-119.767,1342
	FILE *fp = fopen(file.c_str(), "r");
	if (!fp)
		return false;
	
	wxString line, buf;
	AllocReadLine(fp, line);
	wxStringTokenizer tokens(line, ",");
	info.stationID = tokens.GetNextToken(); 
	// LOCATION
	buf = tokens.GetNextToken();
	if (buf.Left(1) == "\"" && buf.Right(1) == "\"")
		buf = buf.Mid(1, buf.Len()-2);
	info.city = buf;
	info.state = tokens.GetNextToken();
	buf = tokens.GetNextToken(); sscanf(buf.c_str(), "%lg", &info.timezone );
	buf = tokens.GetNextToken(); sscanf(buf.c_str(), "%lg", &info.latitude );
	buf = tokens.GetNextToken(); sscanf(buf.c_str(), "%lg", &info.longitude );
	buf = tokens.GetNextToken(); sscanf(buf.c_str(), "%lg", &info.elevation );

	// skip over column header (units) line
	AllocReadLine(fp, line);

	// remaining lines are data

	if (wfretfp)
		*wfretfp = fp;
	else
		fclose(fp);

	return true;
}

static bool ParseEPWHeader(const wxString &file, WFHeader &info, FILE **wfretfp)
{
	//  LOCATION,PHOENIX,AZ,USA,TMY2-23183,722780,33.43,-112.02,-7.0,339.0
	FILE *fp = fopen(file.c_str(), "r");
	if (!fp)
		return false;

	wxString line, buf;
	AllocReadLine(fp, line);
	wxStringTokenizer tokens(line, ",");

	buf = tokens.GetNextToken(); // skip the first one 
	info.city = tokens.GetNextToken();  // CITY
	info.state = tokens.GetNextToken();  // STATE
	buf = tokens.GetNextToken(); // skip country
	buf = tokens.GetNextToken(); // skip source
	buf = tokens.GetNextToken(); // skip id number
	buf = tokens.GetNextToken(); // LATITUDE
	sscanf(buf.c_str(), "%lg", &info.latitude );
	buf = tokens.GetNextToken(); // LONGITUDE
	sscanf(buf.c_str(), "%lg", &info.longitude );
	buf = tokens.GetNextToken(); // TIMEZONE
	sscanf(buf.c_str(), "%lg", &info.timezone );
	buf = tokens.GetNextToken(); // ELEVATION
	sscanf(buf.c_str(), "%lg", &info.elevation );

	// skip over header lines

	AllocReadLine(fp, line); // DESIGN CONDITIONS
	AllocReadLine(fp, line); // TYPICAL/EXTREME PERIODS
	AllocReadLine(fp, line); // GROUND TEMPERATURES
	AllocReadLine(fp, line); // HOLIDAY/DAYLIGHT SAVINGS
	AllocReadLine(fp, line); // COMMENTS 1
	AllocReadLine(fp, line); // COMMENTS 2
	AllocReadLine(fp, line); // DATA PERIODS
	//DATA PERIODS,N periods, N records/hr, A period 1 name, A start day of week, start date, end date
	//DATA PERIODS,1,1,TMY2 Year,Sunday,1,365

	// remaining lines are data
	
	if (wfretfp)
		*wfretfp = fp;
	else
		fclose(fp);

	return true;
}


static int GetWeatherFileType(const wxString &weather_file)
{
/*
	FileLines.Add('* WFType indicates the weather file type');
C     MODE 1: TMY FILE FORMAT  
C     MODE 2: TMY2 FORMAT
C     MODE 3: ENERGYPLUS FORMAT
C     MODE 4: IWEC (INTERNATIONAL WEATHER FOR ENERGY CALCULATIONS)
C     MODE 5: CWEC (CANADIAN WEATHER FOR ENERGY CALCULATIONS)
C     MODE 6: METEONORM (TMY2 FILE FORMAT)
C     MODE 7: TMY3 FORMAT
C     MODE 8: Sam Wind Resource File (tab delimited)
*/
	wxString ext;
	wxFileName::SplitPath( weather_file, NULL, NULL, NULL, &ext);
	if (ext.Lower() == "tm2")
		return WF_TM2;
	else if (ext.Lower() == "epw")
		return WF_EPW;
	else if (ext.Lower() == "tm3" || ext.Lower() == "csv")
		return WF_TM3;
	else if (ext.Lower() == "swrf")
		return 	WF_SWRF;
	else if (ext.Lower() == "smw")
		return WF_SMW;
	else
		return WF_ERR; // error!;
}

static int cstrlocate(char *buf, char **colidx, int colmax, char delim)
{
	char *p = buf;
	int ncols = 0;
	colidx[0] = p;
	int i = 1;
	while (p && *p && i < colmax)
	{
		p = strchr(p, delim);
//		if (p) colidx[i++] = ++p;
		if ((p) && (*(++p)!=delim)) colidx[i++] = p;
	}

	ncols = i;

	while (i<colmax) colidx[i++] = 0;

	return ncols;
}

/*bool ReadWeatherFileLine(FILE *fp, int type, 
					int &year, int &month, int &day, int &hour,
					double &dn, double &df, double &ambt, double &wind)*/
static bool ReadWeatherFileLine(FILE *fp, int type, 
					int &year, int &month, int &day, int &hour,
					double &gh, double &dn, double &df,  // Wh/m2, Wh/m2, Wh/m2
					double &wind, double &drytemp, double &wettemp, // m/s, 'C, 'C
					double &relhum, double &pressure, // %, mbar
					double &winddir, double &snowdepth ) // deg, cm

{
	char buf[1024];
	char *cols[128], *p;

	if (!fp)
		return false;

	wxString line;
	if (type == WF_TM2)
	{
		/* taken from PVWatts */
		int yr,mn,dy,hr,ethor,etdn;
		int d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21;      // which of these are used? d3, d10, d15 & d20
		int u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13,u14,u15,u16,u17,u18,u19,u20,u21;  // are any of these ever used?? - no!
		int w1,w2,w3,w4,w5,w6,w7,w8,w9,w10;     
		char f1[2],f2[2],f3[2],f4[2],f5[2],f6[2],f7[2],f8[2],f9[2],f10[2],f11[2],f12[2],f13[2],f14[2],f15[2],f16[2],f17[2],f18[2],f19[2],f20[2],f21[2];
	
		int nread = fscanf(fp,
		 "%2d%2d%2d%2d"
		 "%4d%4d"
		 "%4d%1s%1d%4d%1s%1d%4d%1s%1d%4d%1s%1d%4d%1s%1d%4d%1s%1d%4d%1s%1d"
		 "%2d%1s%1d%2d%1s%1d%4d%1s%1d%4d%1s%1d%3d%1s%1d%4d%1s%1d%3d%1s%1d"
		 "%3d%1s%1d%4d%1s%1d%5ld%1s%1d%1d%1d%1d%1d%1d%1d%1d%1d%1d%1d%3d%1s%1d%3d%1s%1d%3d%1s%1d%2d%1s%1d\n",
			 &yr,&mn,&dy,&hr,
			 &ethor, // extraterrestrial horizontal radiation
			 &etdn, // extraterrestrial direct normal radiation
			 &d1,&f1,&u1, // GH data value 0-1415 Wh/m2, Source, Uncertainty
			 &d2,&f2,&u2, // DN data value 0-1200 Wh/m2, Source, Uncertainty
			 &d3,&f3,&u3, // DF data value 0-700 Wh/m2, Source, Uncertainty
			 &d4,&f4,&u4, // GH illum data value, Source, Uncertainty
			 &d5,&f5,&u5, // DN illum data value, Source, Uncertainty
			 &d6,&f6,&u6, // DF illum data value, Source, Uncertainty
			 &d7,&f7,&u7, // Zenith illum data value, Source, Uncertainty
			 &d8,&f8,&u8, // Total sky cover
			 &d9,&f9,&u9, // opaque sky cover
			 &d10,&f10,&u10, // dry bulb temp -500 to 500 = -50.0 to 50.0 'C
			 &d11,&f11,&u11, // dew point temp -600 to 300 = -60.0 to 30.0 'C
			 &d12,&f12,&u12, // relative humidity 0-100
			 &d13,&f13,&u13, // pressure millibars
			 &d14,&f14,&u14, // wind direction
			 &d15,&f15,&u15, // wind speed 0 to 400 = 0.0 to 40.0 m/s
			 &d16,&f16,&u16, // visibility
			 &d17,&f17,&u17, // ceiling height
			 &w1,&w2,&w3,&w4,&w5,&w6,&w7,&w8,&w9,&w10, // present weather
			 &d18,&f18,&u18, // precipitable water
			 &d19,&f19,&u19, // aerosol optical depth
			 &d20,&f20,&u20, // snow depth 0-150 cm
			 &d21,&f21,&u21 ); // days since last snowfall 0-88

		year = yr + 1900;
		month = mn;
		day = dy;
		hour = hr;

		gh = (double)d1*1.0;
		dn=d2*1.0;           /* Direct radiation */
		df=d3*1.0;           /* Diffuse radiation */
		drytemp=d10/10.0;       /* Ambient dry bulb temperature(C) */
		wettemp = (double)d11/10.0;
		wind=d15/10.0;       /* Wind speed(m/s) */
		relhum = (double)d12;
		pressure = (double)d13;
		winddir = (double)d14;
		snowdepth = (double)d20;

		return nread==79;
	}
	else if (type == WF_TM3)
	{
		fgets(buf, 1024, fp);
		int ncols = cstrlocate(buf, cols, 128, ',');

		if (ncols < 68)
			return false;

		p = cols[0];

		month = atoi( p );
		p = strchr(p, '/');
		if (!p)
			return false;
		p++;
		day = atoi( p );
		p = strchr(p, '/');
		if (!p) return false;
		p++;
		year = atoi( p );

		hour = atoi( cols[1] );

		gh = (double)atof( cols[4] );
		dn = (double)atof( cols[7] );
		df = (double)atof( cols[10] );

		drytemp = (double)atof( cols[31] );
		wettemp = (double)atof( cols[34] );

		wind = (double)atof( cols[46] );

		relhum = (double)atof( cols[37] );
		pressure = (double)atof( cols[40] );


		winddir = (double)atof( cols[ 43 ] );
		snowdepth = 999;

		return true;
	}
	else if (type == WF_EPW)
	{
		fgets(buf, 1024, fp);
		int ncols = cstrlocate(buf, cols, 128, ',');

		if (ncols < 32)
			return false;

		year = atoi(cols[0]);
		month = atoi(cols[1]);
		day = atoi(cols[2]);
		hour = atoi(cols[3]);

		dn = (double)atof(cols[14]);
		df = (double)atof(cols[15]);
		drytemp = (double)atof(cols[6]);
		wind = (double)atof(cols[21]);

		gh = (double)atof(cols[13]);		
		wettemp = (double)atof(cols[7]);
		relhum = (double)atof( cols[8] );
		pressure = (double)atof( cols[9] ) * 0.01; // convert Pa in to mbar

		winddir = (double) atof( cols[20] );
		snowdepth = (double) atof( cols[30] );

		return true;
	}
	else
		return false;
}

bool wxDVFileReader::FastRead(wxDVPlotCtrl *plotWin, const wxString& filename, int prealloc_data, int prealloc_lnchars)
{
	wxString fExtension = filename.Right(3);
	if (fExtension.CmpNoCase("tm2") == 0 ||
		fExtension.CmpNoCase("epw") == 0 ||
		fExtension.CmpNoCase("smw") == 0)
	{
		return ReadWeatherFile(plotWin, filename);
	}

	wxStopWatch sw;
    sw.Start();

    FILE *inFile = fopen(filename.c_str(), "r"); //r is for read mode.
    if (!inFile)
            return false;

    int lnchars = prealloc_lnchars > 0 ? prealloc_lnchars : 1024;
	lnchars *= 2;  //Give ourselves extra room

	std::vector<wxDVArrayDataSet*> dataSets;
	std::vector<wxString> groupNames;
	std::vector<double> timeCounters;
	int columns = 0;
	bool CommaDelimiters = false;

	wxString firstLine;
	AllocReadLine(inFile, firstLine, lnchars); //Read a line from inFile preallocating lnchars length.

	// if the string with header names is really long,
	// make sure the line reading buffer is plenty long
	// assuming here that the text length of a data line
	// is no more than twice the text length of the header line
	if ( firstLine.Len() > lnchars )
		lnchars = firstLine.Len()*2;

	if (firstLine.Left(19) == wxT("wxDVFileHeaderVer.1"))
	{
		wxString titleStr, offsetStr, tStepStr, unitStr;

		AllocReadLine(inFile, titleStr, 1024);
		AllocReadLine(inFile, offsetStr, 1024);
		AllocReadLine(inFile, tStepStr, 1024);
		AllocReadLine(inFile, unitStr, 1024);

		wxStringTokenizer tkz_titles(titleStr, wxT(",")); //Our format only allows comma csv
		wxStringTokenizer tkz_offsets(offsetStr, wxT(","));
		wxStringTokenizer tkz_tStep(tStepStr, wxT(","));
		wxStringTokenizer tkz_units(unitStr, wxT(","));

		double entry;
		while(tkz_titles.HasMoreTokens() 
			&& tkz_offsets.HasMoreTokens()
			&& tkz_tStep.HasMoreTokens()
			&& tkz_units.HasMoreTokens())
		{
			wxDVArrayDataSet *ds = new wxDVArrayDataSet();
			wxString titleToken = tkz_titles.GetNextToken();
			ds->SetSeriesTitle(titleToken.AfterLast('|'));
			tkz_offsets.GetNextToken().ToDouble(&entry);
			timeCounters.push_back(entry);
			tkz_tStep.GetNextToken().ToDouble(&entry);
			ds->SetTimeStep(entry);
			ds->SetUnits(tkz_units.GetNextToken());
//			ds->SetXLabel("Hours since 00:00 Jan 1");
//			ds->SetYLabel(ds->GetSeriesTitle() + " (" + ds->GetUnits() + ")");

			dataSets.push_back(ds);
			groupNames.push_back(titleToken.BeforeLast('|'));
			columns++;
		}
	}
	else
	{
		wxString names, units;
		names = firstLine; 

		AllocReadLine(inFile, units, lnchars);

		wxStringTokenizer tkz_names(names, wxT(" \t\r\n")); //String tokenizer breaks up cols.
		wxStringTokenizer tkz_units(units, wxT(" \t\r\n"));

		wxStringTokenizer tkz_names_commas(names, wxT(",")); //If it has commas, use them and nothing else.
		wxStringTokenizer tkz_units_commas(units, wxT(","));
		if (tkz_names_commas.CountTokens() > 1)
		{
			tkz_names = tkz_names_commas;
			tkz_units = tkz_units_commas;
			CommaDelimiters = true;
		}

		int count_names, count_units;
		if ((count_names=tkz_names.CountTokens()) != (count_units=tkz_units_commas.CountTokens()))
		{
			//Check if its a tmy3 with a csv extension. 
			//We couldn't catch this earlier because not all csvs are tmy3s.
			if (count_names == 7 && count_units == 68 && fExtension.CmpNoCase("csv") == 0) //Its a tmy3.
			{
				fclose(inFile);
				return ReadWeatherFile(plotWin, filename);
			}
			else
			{
				fclose(inFile);
				return false;
			}
		}

		// Try to read header.
		// If first line contains doubles then ignore title and units.
		// If second line contains doubles ignore units.
		dataSets.reserve(count_names);
		bool firstRowContainsTitles = true;
		bool secondRowContainsUnits = true;
		bool isEnergyPlusOutput = true; // Remains true if first row contains titles, second row contains units, and first value in first row is "Date/Time"
		double entry;

		wxDVArrayDataSet *ds = new wxDVArrayDataSet();
		dataSets.push_back(ds);
		wxString title = tkz_names.GetNextToken();
		timeCounters.push_back(0.5);
		if (IsNumeric(title) || IsDate(title))
		{
			firstRowContainsTitles = false;
			isEnergyPlusOutput = false;
			ds->SetSeriesTitle(wxT("-no name-"));
			groupNames.push_back("");
			title.ToDouble(&entry);
			ds->Append(wxRealPoint(timeCounters[0], entry));
			timeCounters[0] += 1.0;
		}
		else
		{
			ds->SetSeriesTitle(title.AfterLast('|'));
			groupNames.push_back(title.BeforeLast('|'));
//			ds->SetXLabel("Hours since 00:00 Jan 1");
//			ds->SetYLabel(title);
			if (title != "Date/Time")
				isEnergyPlusOutput = false;
		}
	
		if (firstRowContainsTitles)
		{
			wxString units = tkz_units.GetNextToken();
			if (IsNumeric(units) || IsDate(units))
			{
				secondRowContainsUnits = false;
				ds->SetUnits(wxT("-no units-"));
				units.ToDouble(&entry);
				ds->Append(wxRealPoint(timeCounters[0], entry));
				timeCounters[0] += 1.0;
			}
			else
			{
				isEnergyPlusOutput = false;
				ds->SetUnits(units);
//				ds->SetYLabel(title + " (" + units + ")");
			}
		}
		ds->SetTimeStep(1.0, false);

		columns = 1;
		while(columns < count_names)
		{
			timeCounters.push_back(0.5);
			wxDVArrayDataSet *ds = new wxDVArrayDataSet();
			wxString titleToken = tkz_names.GetNextToken();
			if (isEnergyPlusOutput)
			{
				wxString tt = titleToken.AfterLast('|');
				int fstart = tt.First('[');
				int fend = tt.First(']');
				if (fstart != -1 && fend != -1 && fstart < fend)
				{
					// Convert, e.g., "Variable [Units](Hourly)" to "Variable (Hourly)" with units "Units"
					wxString units = tt.SubString(fstart+1, fend-1);
					tt.Replace("[" + units + "]", "");
					ds->SetSeriesTitle(tt);
					ds->SetUnits(units);
				}
				else
				{
					ds->SetSeriesTitle(tt);
				}
			}
			else if (firstRowContainsTitles)
				ds->SetSeriesTitle(titleToken.AfterLast('|'));
			else
			{
				titleToken.ToDouble(&entry);
				ds->SetSeriesTitle(wxT("-no name-"));
				ds->Append(wxRealPoint(timeCounters[columns], entry));
				timeCounters[columns] += 1.0;
			}

			if (secondRowContainsUnits)
			{
				wxString next_units = tkz_units.GetNextToken();
				if (next_units.length() > 0)
					ds->SetUnits(next_units);
				else
					ds->SetUnits(wxT("-no units-"));
			}
			else if (!isEnergyPlusOutput)
			{
				tkz_units.GetNextToken().ToDouble(&entry);
				ds->SetUnits(wxT("-no units-"));
				ds->Append(wxRealPoint(timeCounters[columns], entry));
				timeCounters[columns] += 1.0;
			}
			ds->SetTimeStep(1.0, false);
			dataSets.push_back(ds);
			groupNames.push_back(titleToken.BeforeLast('|'));
			columns++;
		}
	}
       
    if (prealloc_data > 0)
    {
        // preallocate data
        for (int i=0; i<dataSets.size(); i++)
			dataSets[i]->Alloc(prealloc_data);
    }


    int line = 0, ncol, ndbuf;
    char dblbuf[128], *p, *bp; //Position, buffer position
    char *buf = new char[lnchars];
    char *ret = NULL;
    while (true)
    {
            ret = fgets(buf,lnchars-1,inFile);
            if (ret == NULL)
				break; //EOF
			if (buf[0] == 'E' && buf[1] == 'O' && buf[2] == 'F')
				break;

            p = buf;
            ncol = 0;
            while (*p && ncol < columns)
            {
                    bp = dblbuf;
                    ndbuf = 0;
					while(*p && (*p==' '||*p=='\t')) p++; // skip white space
					while (*p && *p != ',' && (CommaDelimiters || (*p != '\t' && *p != ' ')) && ++ndbuf < 127) *bp++ = *p++; // read in number
					*bp = '\0'; // terminate string
					if ( strlen( dblbuf ) > 0 )
					{
						dataSets[ncol]->Append(wxRealPoint(timeCounters[ncol], atof(dblbuf))); // convert number and add data point.
						timeCounters[ncol] += dataSets[ncol]->GetTimeStep();
					}
					if ( *p ) p++; // skip the comma or delimiter
					ncol++;
            }
            line++;
    }

    delete [] buf;


    fclose(inFile);

	//Done reading data; add it to the plotCtrl.
	plotWin->Freeze();
	for (int i=0; i<dataSets.size(); i++)
	{
		dataSets[i]->SetGroupName( groupNames[i].size() > 1 ? groupNames[i] : wxFileNameFromPath( filename ));		
		plotWin->AddDataSet(dataSets[i] , (i==dataSets.size()-1) /* update_ui ? */ );
	}
	plotWin->SelectDataOnBlankTabs();
	plotWin->GetStatisticsTable()->RebuildDataViewCtrl();	//We must do this only after all datasets have been added
	plotWin->Thaw();

	wxLogStatus("Read %i lines of data points.\n", line);
	wxLogDebug("wxDVFileReader::FastRead [ncol=%d nalloc = %d lnchars=%d] = %d msec\n", columns, prealloc_data, lnchars, (int)sw.Time());
    return true;

}

void wxDVFileReader::ReadDataFromCSV(wxDVPlotCtrl *plotWin, const wxString& filename, wxChar separator)
{
	//Deprecated.  Use FastRead.  This method may work if for some reason fastread is broken.
	wxFileInputStream infile(filename);
	if (!infile.IsOk())
	{
		wxMessageBox("Could not read file");
		return;
	}

	wxTextInputStream intext(infile);

	std::vector<wxDVArrayDataSet*> dataSets;
	wxString currentLine = intext.ReadLine();
	if (currentLine.Left(19) != wxT("wxDVFileHeaderVer 1"))
	{
		wxMessageBox("Invalid filetype (header is missing)");
		return;
	}

	currentLine = intext.ReadLine(); // Set Titles From 1st Line
	do
	{
		dataSets.push_back(new wxDVArrayDataSet());
		wxString seriesTitle = currentLine.BeforeFirst(separator);
		dataSets[dataSets.size()-1]->SetSeriesTitle(seriesTitle);
		if (currentLine.size() == seriesTitle.size())
			currentLine = wxT("");
		else
			currentLine = currentLine.Right(currentLine.size() - seriesTitle.size() - 1);
	}
	while (currentLine.size() > 0);

	std::vector<double> timeCounters;
	currentLine = intext.ReadLine(); //Offsets from second line.
	for (int i=0; i<dataSets.size(); i++)
	{
		wxString offsetStr = currentLine.BeforeFirst(separator);
		double offsetDouble;
		offsetStr.ToDouble(&offsetDouble);
		timeCounters.push_back(offsetDouble);
		currentLine = currentLine.Right(currentLine.size() - offsetStr.size() - 1);
	}

	currentLine = intext.ReadLine(); // Time Steps from third line.
	for (int i=0; i<dataSets.size(); i++)
	{
		wxString timeStepStr = currentLine.BeforeFirst(separator);
		double timeStepDouble;
		timeStepStr.ToDouble(&timeStepDouble);
		dataSets[i]->SetTimeStep(timeStepDouble);
		currentLine = currentLine.Right(currentLine.size() - timeStepStr.size() - 1);
	}

	currentLine = intext.ReadLine(); //Units from 4th line.
	for (int i=0; i<dataSets.size(); i++)
	{
		wxString units = currentLine.BeforeFirst(separator);
		dataSets[i]->SetUnits(units);
		currentLine = currentLine.Right(currentLine.size() - units.size() - 1);
	}

	wxLogStatus("Reading file for %i data sets \n", dataSets.size());

	//Start Reading Data Points.
	wxString dataStr;
	double dataDouble;
	bool keepGoing = true;
	do
	{
		currentLine = intext.ReadLine();
		for (int i=0; i<dataSets.size(); i++)
		{
			dataStr = currentLine.BeforeFirst(separator);
			if (!dataStr.ToDouble(&dataDouble))
			{
				keepGoing = false;
				break;
			}
			dataSets[i]->Append(wxRealPoint(timeCounters[i], dataDouble));
			timeCounters[i] += dataSets[i]->GetTimeStep();
			currentLine = currentLine.Right(currentLine.size() - dataStr.size() - 1);
		}
	}
	while (!infile.Eof() && keepGoing);


	//Done reading data; add it to the plotCtrl.
	for (int i=0; i<dataSets.size(); i++)
	{
		dataSets[i]->SetGroupName( wxFileNameFromPath( filename ) );
		plotWin->AddDataSet(dataSets[i], (i==dataSets.size()-1) );
	}
	plotWin->SelectDataOnBlankTabs();
	plotWin->GetStatisticsTable()->RebuildDataViewCtrl();	//We must do this only after all datasets have been added
}

bool wxDVFileReader::ReadWeatherFile(wxDVPlotCtrl* plotWin, const wxString& filename)
{
	int wfType = GetWeatherFileType(filename);

	// Set up data sets for all of the variables that are going to be read.
	std::vector<wxDVArrayDataSet*> dataSets;
	wxDVArrayDataSet *ds = new wxDVArrayDataSet();
	ds->SetSeriesTitle("Global Horizontal");
	ds->SetUnits("Wh/m2");
	dataSets.push_back(ds);

	ds = new wxDVArrayDataSet();
	ds->SetSeriesTitle("Direct Normal");
	ds->SetUnits("Wh/m2");
	dataSets.push_back(ds);

	ds = new wxDVArrayDataSet();
	ds->SetSeriesTitle("Diffuse");
	ds->SetUnits("Wh/m2");
	dataSets.push_back(ds);

	ds = new wxDVArrayDataSet();
	ds->SetSeriesTitle("Wind");
	ds->SetUnits("m/s");
	dataSets.push_back(ds);

	ds = new wxDVArrayDataSet();
	ds->SetSeriesTitle("Dry Temp");
	ds->SetUnits("'C");
	dataSets.push_back(ds);

	ds = new wxDVArrayDataSet();
	ds->SetSeriesTitle("Wet Temp");
	ds->SetUnits("'C");
	dataSets.push_back(ds);

	ds = new wxDVArrayDataSet();
	ds->SetSeriesTitle("Relative Humidity");
	ds->SetUnits("%");
	dataSets.push_back(ds);

	ds = new wxDVArrayDataSet();
	ds->SetSeriesTitle("Pressure");
	ds->SetUnits("mbar");
	dataSets.push_back(ds);

	ds = new wxDVArrayDataSet();
	ds->SetSeriesTitle("WindDir");
	ds->SetUnits("deg");
	dataSets.push_back(ds);

	ds = new wxDVArrayDataSet();
	ds->SetSeriesTitle("Snow Depth");
	ds->SetUnits("cm");
	dataSets.push_back(ds);
	
	for (int i=0; i<dataSets.size(); i++)
		dataSets.at(i)->SetTimeStep( 1.0 ); //All have 1 hr tstep.

	//int year, month, day, hour;
	//double gh, dn, df, wind, drytemp, wettemp, relhum, pressure, winddir, snowdepth;

	// Loop over lines in file, reading into data sets array.
	WFHeader head_info;
	FILE *wFile;
	switch(wfType)
	{
	case WF_TM2:
		if (!ParseTM2Header(filename, head_info, &wFile)) { return false; }
		if (!Read8760WFLines(dataSets, wFile, wfType)) { return false; }
		break;

	case WF_TM3:
		if (!ParseTM3Header(filename, head_info, &wFile)) { return false; }
		if (!Read8760WFLines(dataSets, wFile, wfType)) { return false; }
		fclose(wFile);
		break;

	case WF_EPW:
		if (!ParseEPWHeader(filename, head_info, &wFile)) { return false; }
		if (!Read8760WFLines(dataSets, wFile, wfType)) { return false; }
		fclose(wFile);
		break;

	case WF_ERR:
	default:
		return false;
	}

	//Done reading data; add it to the plotCtrl.
	for (int i=0; i<dataSets.size(); i++)
	{
		dataSets[i]->SetGroupName( wxFileNameFromPath(filename) );
		plotWin->AddDataSet(dataSets[i], (i==dataSets.size()-1) );
	}
	plotWin->SelectDataOnBlankTabs();
	plotWin->GetStatisticsTable()->RebuildDataViewCtrl();	//We must do this only after all datasets have been added

	return true;
}

bool wxDVFileReader::Read8760WFLines(std::vector<wxDVArrayDataSet*> &dataSets, FILE* infile, int wfType)
{
	int year, month, day, hour;
	double gh, dn, df, wind, drytemp, wettemp, relhum, pressure, winddir, snowdepth;

	for(int i=0; i<8760; i++)
	{
		if(!ReadWeatherFileLine(infile, wfType, year, month, day, hour, gh, dn, df, wind, drytemp, wettemp, 
			relhum, pressure, winddir, snowdepth)) 
		{
			return false;
		}

		double hr = ((double)i) + 0.5;

		dataSets[0]->Append(wxRealPoint( hr, gh));
		dataSets[1]->Append(wxRealPoint( hr, dn));
		dataSets[2]->Append(wxRealPoint( hr, df));
		dataSets[3]->Append(wxRealPoint( hr, wind));
		dataSets[4]->Append(wxRealPoint( hr, drytemp));
		dataSets[5]->Append(wxRealPoint( hr, wettemp));
		dataSets[6]->Append(wxRealPoint( hr, relhum));
		dataSets[7]->Append(wxRealPoint( hr, pressure));
		dataSets[8]->Append(wxRealPoint( hr, winddir));
		dataSets[9]->Append(wxRealPoint( hr, snowdepth));
	}

	return true;
}
bool wxDVFileReader::IsNumeric(wxString stringToCheck)
{
	double entry;
	
	if (stringToCheck.ToDouble(&entry)) { return true; }

	return false;
}

bool wxDVFileReader::IsDate(wxString stringToCheck)
{
	char c;
	size_t AMPMposition = 0;
	wxString dummy = stringToCheck.Trim().Trim(false);

	dummy.Replace("\t", " ");
	dummy.Replace("\r", " ");
	dummy.Replace("\n", " ");

	while (dummy.Contains("  "))
	{
		dummy.Replace("  ", " ");
	}

	dummy.Replace(" /", "/");
	dummy.Replace("/ ", "/");
	dummy.Replace(" -", "-");
	dummy.Replace("- ", "-");
	dummy.Replace(" :", ":");
	dummy.Replace(": ", ":");
	dummy.Replace("p", "P");
	dummy.Replace("a", "A");
	dummy.Replace("m", "M");
	dummy.Replace(" P", "P");
	dummy.Replace("P ", "P");
	dummy.Replace(" A", "A");
	dummy.Replace("A ", "A");
	dummy.Replace(" M", "M");
	dummy.Replace("M ", "M");

	if (dummy.length() > 22) { return false; }

	std::string str = dummy.ToStdString();

	for (size_t i = 0; i < str.length(); i++)
	{
		c = str.at(i);

		if (AMPMposition = 0 && (c == 'a' || c == 'p' || c == 'm' || c == 'A' || c == 'P' || c == 'M')) { AMPMposition = i; }

		if (AMPMposition > 0 && i > AMPMposition + 1) { return false; }

		if (i == 0 && c != '0' && c != '1' && c != '2' && c != '3' && c != '4' && c != '5' && c != '6' && c != '7' && c != '8' && c != '9') { return false; }
		if (i >= 1 && i <= 2 && c != '0' && c != '1' && c != '2' && c != '3' && c != '4' && c != '5' && c != '6' && c != '7' && c != '8' && c != '9' && c != '/' && c != '-') { return false; }
		if (i >= 3 && i <= 4 && c != '0' && c != '1' && c != '2' && c != '3' && c != '4' && c != '5' && c != '6' && c != '7' && c != '8' && c != '9' && c != '/' && c != '-' && c != ' ') { return false; }
		if (i >= 5 && i <= 8 && c != '0' && c != '1' && c != '2' && c != '3' && c != '4' && c != '5' && c != '6' && c != '7' && c != '8' && c != '9' && c != '/' && c != '-' && c != ' ' && c != ':') { return false; }
		if (i >= 9 && c != '0' && c != '1' && c != '2' && c != '3' && c != '4' && c != '5' && c != '6' && c != '7' && c != '8' && c != '9' && c != '/' && c != '-' && c != ' ' && c != ':' && c != 'A' && c != 'P' && c != 'M') { return false; }
	}

	return true;
}