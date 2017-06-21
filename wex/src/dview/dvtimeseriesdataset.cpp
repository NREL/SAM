
#include "wex/dview/dvtimeseriesdataset.h"


wxDVTimeSeriesDataSet::wxDVTimeSeriesDataSet()
{
	
}

wxDVTimeSeriesDataSet::~wxDVTimeSeriesDataSet()
{
}

wxString wxDVTimeSeriesDataSet::GetLabel() const
{
	return GetTitleWithUnits();
}

/* Helper Functions */
wxString wxDVTimeSeriesDataSet::GetTitleWithUnits() const
{
	wxString units( GetUnits() );
	if ( units.IsEmpty() ) return GetSeriesTitle();
	else return GetSeriesTitle() + " (" + units + ")";
}

wxRealPoint wxDVTimeSeriesDataSet::operator[] (size_t i) const
{
	return At(i);
}

double wxDVTimeSeriesDataSet::GetMinHours()
{
	return At(0).x;
}
double wxDVTimeSeriesDataSet::GetMaxHours()
{
	return At(Length()-1).x;
}

double wxDVTimeSeriesDataSet::GetTotalHours()
{
	return GetMaxHours() - GetMinHours();
}

void wxDVTimeSeriesDataSet::GetMinAndMaxInRange(double* min, double* max, 
												  size_t startIndex, size_t endIndex)
{
	if(endIndex > Length())
		endIndex = Length();
	if(startIndex < 0)
		startIndex = 0;

	double myMin = At(startIndex).y;
	double myMax = At(startIndex).y;

	for(size_t i=startIndex+1; i<endIndex; i++)
	{
		if (At(i).y < myMin)
			myMin = At(i).y;
		if (At(i).y > myMax)
			myMax = At(i).y;
	}

	if (min)
		*min = myMin;
	if (max)
		*max = myMax;
}

void wxDVTimeSeriesDataSet::GetMinAndMaxInRange(double* min, double* max, double startHour, double endHour)
{
	if (startHour < At(0).x)
		startHour = At(0).x;
	if (endHour < At(0).x)
		endHour = At(0).x;
	size_t startIndex = size_t((startHour - At(0).x)/GetTimeStep());
	size_t endIndex = size_t((endHour - At(0).x)/GetTimeStep() + 2);

	if (startIndex < 0)
		startIndex = 0;
	if (startIndex > Length())
		return;
	if (endIndex > Length())
		endIndex = Length();

	GetMinAndMaxInRange(min, max, startIndex, endIndex);
}

void wxDVTimeSeriesDataSet::GetDataMinAndMax(double* min, double* max)
{
	GetMinAndMaxInRange(min, max, size_t(0), Length());
}

std::vector<wxRealPoint> wxDVTimeSeriesDataSet::GetDataVector()
{
	std::vector<wxRealPoint> pp;
	pp.reserve( Length() );
	for (size_t i=0;i<Length();i++)
		pp.push_back( At(i) );
	return pp;
}



// ******** Array data set *********** //


wxDVArrayDataSet::wxDVArrayDataSet()
	: m_timestep(1), m_offset(0)
{
}

wxDVArrayDataSet::wxDVArrayDataSet( const wxString &var, const std::vector<double> &data )
	: m_varLabel(var), m_timestep(1), m_offset(0)
{
	Copy( data );	
}

wxDVArrayDataSet::wxDVArrayDataSet( const wxString &var, const std::vector<wxRealPoint> &data )
	: m_varLabel(var), m_pData(data), m_timestep(1), m_offset(0)
{
}

wxDVArrayDataSet::wxDVArrayDataSet( const wxString &var, const wxString &units, const double &timestep )
	: m_varLabel(var), m_varUnits(units), m_timestep(timestep), m_offset(0)
{
}

wxDVArrayDataSet::wxDVArrayDataSet( const wxString &var, const wxString &units, const double &timestep, const std::vector<double> &data )
	: m_varLabel(var), m_varUnits(units), m_timestep(timestep), m_offset(0)
{
	Copy( data );
}

wxDVArrayDataSet::wxDVArrayDataSet( const wxString &var, const wxString &units, const double &offset, const double &timestep, const std::vector<double> &data )
	: m_varLabel(var), m_varUnits(units), m_timestep(timestep), m_offset(offset)
{
	Copy( data );
}


wxRealPoint wxDVArrayDataSet::At(size_t i) const
{
	if ((i<m_pData.size())&&(i>=0))
		return wxRealPoint( m_pData[i].x, m_pData[i].y );
	else
		return wxRealPoint(m_offset+i*m_timestep, 0.0 );
}

size_t wxDVArrayDataSet::Length() const
{
	return m_pData.size();
}

double wxDVArrayDataSet::GetTimeStep() const
{
	return m_timestep;
}

double wxDVArrayDataSet::GetOffset() const
{
	return m_offset;
}

wxString wxDVArrayDataSet::GetSeriesTitle() const
{
	return m_varLabel;
}

wxString wxDVArrayDataSet::GetUnits() const
{
	return m_varUnits;
}

void wxDVArrayDataSet::Clear()
{
	m_pData.clear();
}

void wxDVArrayDataSet::Copy( const std::vector<double> &data )
{
	m_pData.clear();
	if ( data.size() > 0 )
	{
		m_pData.resize( data.size() );
		for( size_t i=0;i<data.size();i++ )
			m_pData[i] = wxRealPoint( m_offset+i*m_timestep, data[i] );
	}
}

void wxDVArrayDataSet::Alloc( size_t n )
{
	m_pData.reserve( n );
}

void wxDVArrayDataSet::Append( const wxRealPoint &p )
{
	m_pData.push_back( p );
}

void wxDVArrayDataSet::Set( size_t i, double x, double y )
{
	if ( i < m_pData.size() )
		m_pData[i] = wxRealPoint( x, y );
}

void wxDVArrayDataSet::SetY( size_t i, double y )
{
	if ( i < m_pData.size() )
		m_pData[i].y = y;
}

void wxDVArrayDataSet::SetSeriesTitle( const wxString &title )
{
	m_varLabel = title;
}

void wxDVArrayDataSet::SetUnits( const wxString &units )
{
	m_varUnits = units;
}

void wxDVArrayDataSet::SetTimeStep( double ts, bool recompute_x )
{
	m_timestep = ts;
	if ( recompute_x ) RecomputeXData();
}

void wxDVArrayDataSet::SetOffset( double off, bool recompute_x )
{
	m_offset = off;
	if ( recompute_x ) RecomputeXData();
}

void wxDVArrayDataSet::RecomputeXData()
{
	for( size_t i=0;i<m_pData.size();i++ )
		m_pData[i].x = m_offset+i*m_timestep;
}


// ******** Statistics data set *********** //

wxDVStatisticsDataSet::wxDVStatisticsDataSet(wxDVTimeSeriesDataSet *d)
{
	baseDataset = d;

	double MinHrs = d->GetMinHours();
	double MaxHrs = d->GetMaxHours();
	double Offset = d->GetOffset();

	//Create monthly and total statistics from m_data - start with very large number (2 billion) for Min variables and -2 billion for Max's, since we'll be looking for anything lower than this in the data.
	wxString name = "";
	double nextDay = 0.0;
	double nextMonth = 0.0;
	double currentMonth = 0.0;
	double year = 0.0;
	double counter = 0.0;
	double sum = 0.0;
	double avg = 0.0;
	double min = 2000000000.0;
	double max = -2000000000.0;
	double StDev = 0.0;
	double AvgDailyMin = 2000000000.0;
	double AvgDailyMax = -2000000000.0;
	int DayNumber = 0;
	double DayCounter = 0.0;
	double StDevCounter = 0.0;
	double DayMins[31] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
	double DayMaxs[31] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
	int FirstOrdinalOfMonth = 0;
	double totalcounter = 0.0;
	double totalsum = 0.0;
	double totalmin = 2000000000.0;
	double totalmax = -2000000000.0;
	std::vector<wxRealPoint> totalDayStats;
	StatisticsPoint sp;
	bool MultiYear = false;
	int yrNum = 1;

	while (MinHrs > 8760.0)
	{
		year += 8760.0;
		MinHrs -= 8760.0;
		MaxHrs -= 8760.0;
	}

	if (MaxHrs > 8760) { MultiYear = true; }	//Determine if the dataset contains data for more than one year

	while (nextDay <= MinHrs)
	{
		nextDay += 24.0;
	}

	if (MinHrs >= 0.0 && MinHrs < 744.0) { currentMonth = year; nextMonth = year + 744.0; name = "Jan"; }
	else if (MinHrs >= 744.0 && MinHrs < 1416.0) { currentMonth = year + 744.0; nextMonth = year + 1416.0; name = "Feb"; }
	else if (MinHrs >= 1416.0 && MinHrs < 2160.0) { currentMonth = year + 1416.0; nextMonth = year + 2160.0; name = "Mar"; }
	else if (MinHrs >= 2160.0 && MinHrs < 2880.0) { currentMonth = year + 2160.0; nextMonth = year + 2880.0; name = "Apr"; }
	else if (MinHrs >= 2880.0 && MinHrs < 3624.0) { currentMonth = year + 2880.0; nextMonth = year + 3624.0; name = "May"; }
	else if (MinHrs >= 3624.0 && MinHrs < 4344.0) { currentMonth = year + 3624.0; nextMonth = year + 4344.0; name = "Jun"; }
	else if (MinHrs >= 4344.0 && MinHrs < 5088.0) { currentMonth = year + 4344.0; nextMonth = year + 5088.0; name = "Jul"; }
	else if (MinHrs >= 5088.0 && MinHrs < 5832.0) { currentMonth = year + 5088.0; nextMonth = year + 5832.0; name = "Aug"; }
	else if (MinHrs >= 5832.0 && MinHrs < 6552.0) { currentMonth = year + 5832.0; nextMonth = year + 6552.0; name = "Sep"; }
	else if (MinHrs >= 6552.0 && MinHrs < 7296.0) { currentMonth = year + 6552.0; nextMonth = year + 7296.0; name = "Oct"; }
	else if (MinHrs >= 7296.0 && MinHrs < 8016.0) { currentMonth = year + 7296.0; nextMonth = year + 8016.0; name = "Nov"; }
	else if (MinHrs >= 8016.0 && MinHrs < 8760.0) { currentMonth = year + 8016.0; nextMonth = year + 8760.0; name = "Dec"; }

	if (MultiYear) { name = "Year 1, " + name; }	//If the dataset contains data for more than one year then append the year number to the name

	for (size_t i = 0; i < d->Length(); i++)
	{
		if (d->At(i).x >= nextDay)
		{
			if (i != 0)
			{
				DayMins[DayNumber] = (AvgDailyMin == 2000000000.0) ? 0.0 : AvgDailyMin;
				DayMaxs[DayNumber] = (AvgDailyMax == -2000000000.0) ? 0.0 : AvgDailyMax;
				totalDayStats.push_back(wxRealPoint(AvgDailyMin, AvgDailyMax));
				DayNumber++;
				AvgDailyMin = 2000000000.0;
				AvgDailyMax = -2000000000.0;
				nextDay += 24.0;
			}
		}

		if (d->At(i).x >= nextMonth)
		{
			if (i != 0 && counter != 0)
			{
				AvgDailyMin = 0.0;
				AvgDailyMax = 0.0;
				avg = sum / counter;

				//Calculate standard deviation for the month's data
				StDev = 0.0;
				StDevCounter = 0.0;
				for (size_t j = FirstOrdinalOfMonth; j < i; j++)
				{
					StDev += (d->At(j).y - avg) * (d->At(j).y - avg);
					StDevCounter += 1.0;
				}
				if (StDevCounter > 0.0)
				{
					StDev = StDev / StDevCounter;
					StDev = sqrt(StDev);
				}

				//Summarize the daily Min and Max values into average daily min and max values for the month.
				for (size_t j = 0; j < 31; j++)
				{
					if (DayMaxs[j] > 0)	//Not every month will have 31 days and endpoints may be missing some days in the month, as denoted by a 0 maximum value
					{
						AvgDailyMin += DayMins[j];
						AvgDailyMax += DayMaxs[j];
						DayCounter += 1.0;
					}
				}
				if (DayCounter > 0.0)
				{
					AvgDailyMax = AvgDailyMax / DayCounter;
					AvgDailyMin = AvgDailyMin / DayCounter;
				}

				sp = StatisticsPoint();
				if (Offset < 672.0 && Offset > 0.0)	//xOffset is within number of hours in the shortest month
				{
					sp.x = currentMonth + Offset;
				}
				else
				{
					sp.x = currentMonth + ((nextMonth - currentMonth) / 2.0);	//Make x the middle of the month
				}
				sp.name = name;
				sp.Max = RoundSignificant(max);
				sp.Min = RoundSignificant(min);
				sp.Sum = RoundSignificant(sum);
				sp.Mean = RoundSignificant(avg);
				sp.StDev = RoundSignificant(StDev);
				sp.AvgDailyMax = RoundSignificant(AvgDailyMax);
				sp.AvgDailyMin = RoundSignificant(AvgDailyMin);

				Append(sp);

				currentMonth = nextMonth;
				if (nextMonth == 744.0 + year) { nextMonth = 1416.0 + year; name = "Feb"; }
				else if (nextMonth == 1416.0 + year) { nextMonth = 2160.0 + year; name = "Mar"; }
				else if (nextMonth == 2160.0 + year) { nextMonth = 2880.0 + year; name = "Apr"; }
				else if (nextMonth == 2880.0 + year) { nextMonth = 3624.0 + year; name = "May"; }
				else if (nextMonth == 3624.0 + year) { nextMonth = 4344.0 + year; name = "Jun"; }
				else if (nextMonth == 4344.0 + year) { nextMonth = 5088.0 + year; name = "Jul"; }
				else if (nextMonth == 5088.0 + year) { nextMonth = 5832.0 + year; name = "Aug"; }
				else if (nextMonth == 5832.0 + year) { nextMonth = 6552.0 + year; name = "Sep"; }
				else if (nextMonth == 6552.0 + year) { nextMonth = 7296.0 + year; name = "Oct"; }
				else if (nextMonth == 7296.0 + year) { nextMonth = 8016.0 + year; name = "Nov"; }
				else if (nextMonth == 8016.0 + year) { nextMonth = 8760.0 + year; name = "Dec"; }
				else if (nextMonth == 8760.0 + year) { year += 8760.0; nextMonth = 744.0 + year; name = "Jan"; yrNum += 1; }

				if (MultiYear) { name = "Year " + wxString::Format("%d", yrNum) + ", " + name; }	//If the dataset contains data for more than one year then prepend the year number to the name
			}

			counter = 0.0;
			sum = 0.0;
			avg = 0.0;
			min = 2000000000.0;
			max = -2000000000.0;
			StDev = 0.0;
			AvgDailyMin = 2000000000.0;
			AvgDailyMax = -2000000000.0;
			DayNumber = 0;
			DayCounter = 0.0;
			FirstOrdinalOfMonth = i;
		}

		counter += 1.0;
		totalcounter += 1.0;
		sum += d->At(i).y;
		totalsum += d->At(i).y;
		if (min > d->At(i).y) { min = d->At(i).y; }
		if (totalmin > d->At(i).y) { totalmin = d->At(i).y; }
		if (max < d->At(i).y) { max = d->At(i).y; }
		if (totalmax < d->At(i).y) { totalmax = d->At(i).y; }
		if (AvgDailyMin > d->At(i).y) { AvgDailyMin = d->At(i).y; }
		if (AvgDailyMax < d->At(i).y) { AvgDailyMax = d->At(i).y; }
	}

	if (MaxHrs > 0.0 && fmod(MaxHrs, 8760.0) != 0)	//Prevent appending the final point if it represents 12/31 24:00, which the system interprets as 1/1 0:00 and creates a point for January of the next year
	{
		DayMins[DayNumber] = AvgDailyMin;
		DayMaxs[DayNumber] = AvgDailyMax;
		totalDayStats.push_back(wxRealPoint(AvgDailyMin, AvgDailyMax));
		AvgDailyMin = 0.0;
		AvgDailyMax = 0.0;

		avg = sum / counter;

		//Calculate standard deviation for the month's data
		StDev = 0.0;
		StDevCounter = 0.0;
		for (size_t j = FirstOrdinalOfMonth; j < d->Length(); j++)
		{
			StDev += (d->At(j).y - avg) * (d->At(j).y - avg);
			StDevCounter += 1.0;
		}
		if (StDevCounter > 0.0)
		{
			StDev = StDev / StDevCounter;
			StDev = sqrt(StDev);
		}

		//Summarize the daily Min and Max values into average daily min and max values for the month.
		for (size_t j = 0; j < 31; j++)
		{
			if (DayMaxs[j] > 0)	//Not every month will have 31 days and endpoints may be missing some days in the month, as denoted by a 0 maximum value
			{
				AvgDailyMin += DayMins[j];
				AvgDailyMax += DayMaxs[j];
				DayCounter += 1.0;
			}
		}
		if (DayCounter > 0.0)
		{
			AvgDailyMax = AvgDailyMax / DayCounter;
			AvgDailyMin = AvgDailyMin / DayCounter;
		}

		sp = StatisticsPoint();
		if (Offset < 672.0 && Offset > 0.0)	//xOffset is within number of hours in the shortest month
		{
			sp.x = currentMonth + Offset;
		}
		else
		{
			sp.x = currentMonth + ((nextMonth - currentMonth) / 2.0);	//Make x the middle of the month
		}
		sp.name = name;
		sp.Max = RoundSignificant(max);
		sp.Min = RoundSignificant(min);
		sp.Sum = RoundSignificant(sum);
		sp.Mean = RoundSignificant(avg);
		sp.StDev = RoundSignificant(StDev);
		sp.AvgDailyMax = RoundSignificant(AvgDailyMax);
		sp.AvgDailyMin = RoundSignificant(AvgDailyMin);

		Append(sp);

	}

	//Append StatisticsPoint for totals over all months

	AvgDailyMin = 0.0;
	AvgDailyMax = 0.0;
	avg = totalsum / totalcounter;

	//Calculate standard deviation for the month's data
	StDev = 0.0;
	StDevCounter = 0.0;
	for (size_t j = 0; j < d->Length(); j++)
	{
		StDev += (d->At(j).y - avg) * (d->At(j).y - avg);
		StDevCounter += 1.0;
	}
	if (StDevCounter > 0.0)
	{
		StDev = StDev / StDevCounter;
		StDev = sqrt(StDev);
	}

	//Summarize the daily Min and Max values into average daily min and max values for the month.
	DayCounter = 0.0;
	for (size_t j = 0; j < totalDayStats.size(); j++)
	{
		AvgDailyMin += totalDayStats[j].x;
		AvgDailyMax += totalDayStats[j].y;
		DayCounter += 1.0;
	}
	if (DayCounter > 0.0)
	{
		AvgDailyMax = AvgDailyMax / DayCounter;
		AvgDailyMin = AvgDailyMin / DayCounter;
	}

	sp = StatisticsPoint();
	sp.x = d->At(d->Length() - 1).x + 1.0;	//Make x one greater than the last x value in the dataset
	sp.name = "Total";
	sp.Max = RoundSignificant(totalmax);
	sp.Min = RoundSignificant(totalmin);
	sp.Sum = RoundSignificant(totalsum);
	sp.Mean = RoundSignificant(avg);
	sp.StDev = RoundSignificant(StDev);
	sp.AvgDailyMax = RoundSignificant(AvgDailyMax);
	sp.AvgDailyMin = RoundSignificant(AvgDailyMin);

	Append(sp); 
}

double wxDVStatisticsDataSet::RoundSignificant(double ValueToRound, size_t NumSignifDigits)
{
	double roundedValue = ValueToRound;
	double multiplier = pow(10, (int)NumSignifDigits);	//set the multiplier to be 10 to the power of NumSignifDigits

	roundedValue = roundedValue * multiplier;	//Move the digits we want to keep to the right of the decimal point
	roundedValue = round(roundedValue);			//Round to the nearest integer (truncate digits to the left of the decimal point)
	roundedValue = roundedValue / multiplier;	//Reset the decimal point to its original position

	//TODO:   Implement more sophisticated rounding logic using significant digits:
	//Round double values to 4 significant digits by only displaying three more digits after the first non-zero digit to the right of the decimal. 
	//If there are no non-zero digits to the right of the decimal then we should display the number as in integer. 
	//We must also be careful not to loose any exponential notation (XX E YY) during the rounding. The E and characters after it should be appended to the rounded value.

	return roundedValue;
}

StatisticsPoint wxDVStatisticsDataSet::At(size_t i) const
{
	StatisticsPoint p = StatisticsPoint();

	if ((i < m_sData.size()) && (i >= 0))
	{
		p.name = m_sData[i].name;
		p.x = m_sData[i].x;
		p.Sum = m_sData[i].Sum;
		p.Min = m_sData[i].Min;
		p.Max = m_sData[i].Max;
		p.Mean = m_sData[i].Mean;
		p.StDev = m_sData[i].StDev;
		p.AvgDailyMin = m_sData[i].AvgDailyMin;
		p.AvgDailyMax = m_sData[i].AvgDailyMax;
	}
	else
	{
		p.name = "";
		p.x = baseDataset->GetOffset() + (i * baseDataset->GetTimeStep());
		p.Sum = 0.0;
		p.Min = 0.0;
		p.Max = 0.0;
		p.Mean = 0.0;
		p.StDev = 0.0;
		p.AvgDailyMin = 0.0;
		p.AvgDailyMax = 0.0;
	}

	return p;
}

size_t wxDVStatisticsDataSet::Length() const
{
	return m_sData.size();
}

bool wxDVStatisticsDataSet::IsSourceDataset(wxDVTimeSeriesDataSet *d)
{
	return d == baseDataset;
}

void wxDVStatisticsDataSet::Clear()
{
	m_sData.clear();
}

void wxDVStatisticsDataSet::Alloc(size_t n)
{
	m_sData.reserve(n);
}

void wxDVStatisticsDataSet::Append(const StatisticsPoint &p)
{
	m_sData.push_back(p);
}

wxString wxDVStatisticsDataSet::GetSeriesTitle() const
{
	return baseDataset->GetSeriesTitle();
}

wxString wxDVStatisticsDataSet::GetUnits() const
{
	return baseDataset->GetUnits();
}

double wxDVStatisticsDataSet::GetTimeStep() const
{
	return baseDataset->GetTimeStep();
}

double wxDVStatisticsDataSet::GetOffset() const
{
	return baseDataset->GetOffset();
}

double wxDVStatisticsDataSet::GetMinHours()
{
	return baseDataset->At(0).x;
}

double wxDVStatisticsDataSet::GetMaxHours()
{
	return baseDataset->At(Length() - 1).x;
}

void wxDVStatisticsDataSet::GetDataMinAndMax(double* min, double* max)
{
	baseDataset->GetMinAndMaxInRange(min, max, size_t(0), baseDataset->Length());
}

void wxDVStatisticsDataSet::GetMinAndMaxInRange(double* min, double* max, double startHour, double endHour)
{
	baseDataset->GetMinAndMaxInRange(min, max, startHour, endHour);
}
