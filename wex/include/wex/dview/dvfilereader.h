#ifndef __DVFileDataSet_h
#define __DVFileDataSet_h

/*
 * wxDVFileDataSet.h
 * 
 * For the stand-alone application, we provide the ability to load a data set from a file.
 * Other applications that implement DViewLib will handle wxDVTimeSeriesDataSet in their own way.
 *
 * This class implements wxDVTimeSeriesDataSet.  This class is used to read a 
 * data set from files.  Right now we support csv and txt.
 */
#include <stdio.h>
#include <vector>
#include <wx/string.h>

class wxDVPlotCtrl;
class wxDVArrayDataSet;

class wxDVFileReader
{
public:
	static void ReadDataFromCSV( wxDVPlotCtrl* plotWin, const wxString& filename, wxChar separator = ',' );
	static bool FastRead( wxDVPlotCtrl* plotWin, const wxString& filename, int prealloc_data = 8760, int prealloc_lnchars = 1024 );
	static bool Read8760WFLines( std::vector<wxDVArrayDataSet*> &dataSets, FILE* infile, int wfType );
	static bool ReadWeatherFile( wxDVPlotCtrl* plotWin, const wxString& filename );
	static bool IsNumeric(wxString stringToCheck);
	static bool IsDate(wxString stringToCheck);
};

#endif

