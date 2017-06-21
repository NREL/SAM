<<<<<<< HEAD
#ifndef __adjfac_h
#define __adjfac_h

#include <vector>

#include <wx/panel.h>


#include "object.h"


/* Hourly adjustment factors:
	example SSC variables:

	adjust:factor
	adjust:en_hourly
	adjust:hourly
	adjust:en_periods
	adjust:periods
*/

class wxButton;
class wxTextCtrl;
class wxStaticText;

class VarValue;

#define EVT_HOURLYFACTORS(i,f) EVT_BUTTON(i,f)

class AFLossAdjustmentCtrl : public wxPanel
{
public:
	AFLossAdjustmentCtrl( wxWindow *parent, int id,
		const wxPoint &pos=wxDefaultPosition, const wxSize &size=wxDefaultSize);

	void Write( VarValue * );
	bool Read( VarValue * );

	struct FactorData
	{
		float constant; // updated from "factor" to "constant" 6/19/15

		bool en_hourly;
		std::vector<float> hourly;

		bool en_periods;
		matrix_t<float> periods; // stored as n x 3 matrix: columns: [start hour] [length hour] [factor]
	};

	bool DoEdit();

private:
	void OnPressed(wxCommandEvent &);
	void UpdateText();
	
	wxButton *m_button;
	wxStaticText *m_label;
	FactorData m_data;

	DECLARE_EVENT_TABLE();
};

#endif

=======
#ifndef __adjfac_h
#define __adjfac_h

#include <vector>

#include <wx/panel.h>


#include "object.h"


/* Hourly adjustment factors:
	example SSC variables:

	adjust:factor
	adjust:en_hourly
	adjust:hourly
	adjust:en_periods
	adjust:periods
*/

class wxButton;
class wxTextCtrl;
class wxStaticText;

class VarValue;

#define EVT_HOURLYFACTORS(i,f) EVT_BUTTON(i,f)

class AFLossAdjustmentCtrl : public wxPanel
{
public:
	AFLossAdjustmentCtrl( wxWindow *parent, int id,
		const wxPoint &pos=wxDefaultPosition, const wxSize &size=wxDefaultSize);

	void Write( VarValue * );
	bool Read( VarValue * );

	struct FactorData
	{
		float constant; // updated from "factor" to "constant" 6/19/15

		bool en_hourly;
		std::vector<float> hourly;

		bool en_periods;
		matrix_t<float> periods; // stored as n x 3 matrix: columns: [start hour] [length hour] [factor]
	};

	bool DoEdit();

private:
	void OnPressed(wxCommandEvent &);
	void UpdateText();
	
	wxButton *m_button;
	wxStaticText *m_label;
	FactorData m_data;

	DECLARE_EVENT_TABLE();
};

#endif

>>>>>>> 2c85b0ce6a18646fb532eb72a604d646517b67ae
