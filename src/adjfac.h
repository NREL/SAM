#ifndef __adjfac_h
#define __adjfac_h

#include <vector>

#include <wx/window.h>
#include <wx/button.h>

#include "object.h"


/* Hourly adjustment factors:
	example SSC variables:

	adjust:factor
	adjust:en_hourly
	adjust:hourly
	adjust:en_periods
	adjust:periods
*/

class VarValue;

#define EVT_HOURLYFACTORS(i,f) EVT_BUTTON(i,f)

class AFHourlyFactorCtrl : public wxButton
{
public:
	AFHourlyFactorCtrl( wxWindow *parent, int id,
		const wxPoint &pos=wxDefaultPosition, const wxSize &size=wxDefaultSize);

	void Write( VarValue * );
	bool Read( VarValue * );

	struct FactorData
	{
		float factor;

		bool en_hourly;
		std::vector<float> hourly;

		bool en_periods;
		matrix_t<float> periods; // stored as n x 3 matrix: columns: [start hour] [length hour] [factor]
	};

private:
	void OnPressed( wxCommandEvent & );
	FactorData m_data;
	DECLARE_EVENT_TABLE();
};

#endif

