#ifndef __parametric_h
#define __parametric_h

#include <wx/datstrm.h>
#include <wx/panel.h>

#include "simulation.h"
#include "object.h"

class Case;

class ParametricData
{
public:
	ParametricData( Case *c );
	~ParametricData();

	void ClearRuns();

	struct Var {
		wxString Name;
		std::vector<VarValue> Values;
	};
	std::vector<Var> Setup;
	std::vector<Simulation*> Runs;

	void Write( wxOutputStream & );
	bool Read( wxInputStream & );

private:
	Case *m_case;
};


class ParametricViewer : public wxPanel
{
public:
	ParametricViewer( wxWindow *parent, Case *cc );

private:
	Case *m_case;
	ParametricData &m_par;
};

#endif

