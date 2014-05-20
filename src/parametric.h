#ifndef __parametric_h
#define __parametric_h

#include <wx/datstrm.h>
#include <wx/panel.h>
#include <wx/grid.h>

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

	Case *GetCase() { return m_case; }
private:
	Case *m_case;
};


class ParametricGridData : public wxGridTableBase
{
public:
	ParametricGridData(ParametricData &par);
	virtual int GetNumberRows();
	virtual int GetNumberCols();
	virtual bool IsEmptyCell(int row, int col);
	virtual wxString GetValue(int row, int col);
	virtual void SetValue(int row, int col, const wxString& value);
	virtual wxString GetColLabelValue(int col);
	virtual void SetColLabelValue(int col, const wxString &label);
	virtual wxString GetTypeName(int row, int col);

	// for choice controls
	wxString GetChoices(int row, int col);

	VarInfo* GetVarInfo(int row, int col);
	void SetVarInfo(int row, int col, VarInfo *vi);
	VarValue* GetVarValue(int row, int col);
	void SetVarValue(int row, int col, VarValue *vv);

private:
	void Init();
	int m_rows;
	int m_cols;
	wxArrayString m_col_hdrs;
	ParametricData *m_par;
	wxArrayString m_var_names;
	wxArrayString m_var_labels;
	VarTable* m_var_table;
	VarInfoLookup* m_var_info_lookup;
};



class ParametricViewer : public wxPanel
{
public:
	ParametricViewer( wxWindow *parent, Case *cc );

private:
	void OnCommand(wxCommandEvent &evt);
	void Configure();
	void UpdateGrid();

	wxGrid *m_grid;
	Case *m_case;
	ParametricData &m_par;

	DECLARE_EVENT_TABLE();
};


#endif

