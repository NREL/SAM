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

#include <wx/sizer.h>
#include <wx/checklst.h>
#include <wx/checkbox.h>
#include <wx/spinctrl.h>

#include "casewin.h"
#include "combinecases.h"
#include "main.h"
#include "script.h"

enum {
	ID_chlCases,
    ID_chkOverwriteCapital,
	ID_spndDegradation
};

BEGIN_EVENT_TABLE( CombineCasesDialog, wxDialog )
	EVT_CHECKLISTBOX(ID_chlCases, CombineCasesDialog::OnEvt)
	EVT_CHECKBOX(ID_chkOverwriteCapital, CombineCasesDialog::OnEvt)
	//EVT_SPINCTRLDOUBLE(ID_spndDegradation, CombineCasesDialog::OnEvt)
	EVT_BUTTON(wxID_OK, CombineCasesDialog::OnEvt)
	EVT_BUTTON(wxID_HELP, CombineCasesDialog::OnEvt)
END_EVENT_TABLE()

CombineCasesDialog::CombineCasesDialog(wxWindow* parent, const wxString& title, lk::invoke_t& cxt)
    : wxDialog(parent, wxID_ANY, title, wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
{
	// Initializations
	m_result_code = -1;
	m_generic_case = SamApp::Window()->GetCurrentCase();
	m_generic_case_name = SamApp::Window()->Project().GetCaseName(m_generic_case);
	m_generic_case_window = SamApp::Window()->GetCaseWindow(m_generic_case);
	if (m_generic_case->Values().Get("system_use_lifetime_output")->Boolean()) {
		m_generic_degradation = m_generic_case->Values().Get("generic_degradation")->Array();
	}
	else {
		m_generic_degradation = m_generic_case->Values().Get("degradation")->Array();
	}

	// Text at top of window
	wxString msg = "Select open cases, simulate those cases and combine their generation\n";
	msg += "profiles into a single profile to be used with this generic case.\n\n";
	msg += "SAM will switch to each case in the project and run a simulation.\n";
	msg += "Depending on the configuration, SAM may appear to be temporarily unresponsive.";

	// Case selection list
	m_chlCases = new wxCheckListBox(this, ID_chlCases, wxDefaultPosition, wxSize(400, 200)); // populate with active cases
	this->GetOpenCases();
	this->RefreshList(0.);
	wxBoxSizer* szcases = new wxBoxSizer(wxVERTICAL);
	szcases->Add(new wxStaticText(this, wxID_ANY, "1. Select cases:"), 0, wxALL, 2);
	szcases->Add(m_chlCases, 10, wxALL | wxEXPAND, 1);

	// Overwrite capital checkbox
	m_chkOverwriteCapital = new wxCheckBox(this, ID_chkOverwriteCapital, "Overwrite Installation Costs with combined cases costs");

	// Annual AC degradation
	// Due to complexity of AC and DC degradation and lifetime and single year simulations, require user to provide an
	// AC degradation rate for the combined project and ignore degradation rate inputs of individual system cases.
	// TODO: maybe add note: 'The Year 1 hourly generation profile is used for each system. Please'
	//						 '\n provide an annual AC degradation rate to use for the combined project,'
	//						 '\n or enter a zero to ignore degradation.';
	// TODO: make this a SchedNumeric instead so a schedule could be used
	m_spndDegradation = new wxSpinCtrlDouble(this, ID_spndDegradation, "Annual AC Degradation", wxDefaultPosition, wxSize(54, 22),
		wxSP_ARROW_KEYS, 0, 100, m_generic_degradation[0], 0.1, "wxspndDegradation");
	wxString degradation_label = "%/year  Annual AC degradation rate";
	wxBoxSizer* szdegradation = new wxBoxSizer(wxHORIZONTAL);
	szdegradation->Add(m_spndDegradation, 0, wxALL, 1);
	szdegradation->Add(new wxStaticText(this, wxID_ANY, degradation_label), 0, wxALL | wxALIGN_CENTER, 1);

	// Combine all into main vertical sizer
	wxBoxSizer* szmain = new wxBoxSizer(wxVERTICAL);
	szmain->Add(new wxStaticText(this, wxID_ANY, msg), 0, wxALL | wxEXPAND, 10);
	szmain->Add(szcases, 10, wxALL | wxEXPAND, 1);
	szmain->Add(m_chkOverwriteCapital, 0, wxALL, 5);
	szmain->Add(szdegradation, 0, wxALL, 1);
	szmain->Add(CreateButtonSizer(wxHELP | wxOK | wxCANCEL), 0, wxALL | wxEXPAND, 10);

	SetSizer(szmain);
	Fit();
	m_chlCases->SetFocus();
}

void CombineCasesDialog::OnEvt(wxCommandEvent& e)
{
	switch (e.GetId())
	{
		case wxID_HELP:
			SamApp::ShowHelp("combine_cases");
			break;
		case ID_chlCases:
			{
				for (size_t i = 0; i < m_cases.size(); i++)	{
					if (m_cases[i].display_name == m_chlCases->GetString(e.GetInt())) {
						m_cases[i].is_selected = m_chlCases->IsChecked(e.GetInt());
					}
				}
			}
			break;
		case wxID_OK:
			{
				// See which cases are selected
				wxArrayInt arychecked;
				for (size_t i = 0; i < m_cases.size(); i++)	{
					if (m_cases[i].is_selected) {
						arychecked.push_back((int)i);
					}
				}

				if (arychecked.Count() >= 2) {

					// Get analysis period and inflation from generic case
					// TODO: Move some of this to constructor?
					wxString technology_name = m_generic_case->GetTechnology();
					wxString financial_name = m_generic_case->GetFinancing();
					double analysis_period = std::numeric_limits<double>::quiet_NaN();
					double inflation = std::numeric_limits<double>::quiet_NaN();
					if (financial_name == "LCOE Calculator") {
						analysis_period = m_generic_case->Values().Get("c_lifetime")->Value();
						inflation = m_generic_case->Values().Get("c_inflation")->Value();
					}
					else if (financial_name != "None") {
						analysis_period = m_generic_case->Values().Get("analysis_period")->Value();
						inflation = m_generic_case->Values().Get("inflation_rate")->Value();
					}

					// Allocate and initialize variables to run through the cases to combine
					double nameplate = 0.;
					matrix_t<double> hourly_energy(1, 8760, 0.);
					double annual_energy = 0.;
					double total_installed_cost = 0.;
					std::vector<double> om_fixed(analysis_period, 0.);
					bool is_notices = false;

					// Get dialog inputs
					bool overwrite_capital = m_chkOverwriteCapital->IsChecked();
					double degradation = m_spndDegradation->GetValue();				// TODO: change this to a SchedNumeric

					// Run each simulation
					for (size_t i = 0; i < arychecked.Count(); i++) {
						// Switch to case
						SamApp::Window()->SwitchToCaseWindow(m_cases[arychecked[i]].name);
						Case* current_case = SamApp::Window()->GetCurrentCase();
						CaseWindow* case_window = SamApp::Window()->GetCaseWindow(current_case);
						wxString case_page_orig = case_window->GetInputPage();
						Simulation& bcsim = current_case->BaseCase();
						wxString technology_name = current_case->GetTechnology();
						wxString financial_name = current_case->GetFinancing();

						// Set degradation, saving original value and value/sched property
						// TODO: grab more than just the numeric if it's a schedule
						// case_window->SwitchToInputPage("Lifetime and Degradation");
						// wxUIObject* degradation_obj = case_window->FindActiveObject("degradation", &aip);	// works only if page containing control is active
						ActiveInputPage* aip = 0;
						wxUIObject* degradation_obj = case_window->FindObject("degradation", &aip);
						assert(degradation_obj && aip && degradation_obj->HasProperty("UseSchedule"));
						bool degradation_orig_usesched = degradation_obj->Property("UseSchedule").GetBoolean();
						if (degradation_orig_usesched) {
							degradation_obj->Property("UseSchedule").Set(false);
							//aip->Refresh();										// no effect
							//wxWindow* win;
							//if (win = degradation_obj->GetNative());
							//	win->Refresh();										// no effect
							//current_case->VariableChanged("degradation");			// no effect
							//case_window->Refresh();								// no effect
						}
						VarValue* degradation_vv = current_case->Values().Get("degradation");
						std::vector<double> degradation_orig_vec = degradation_vv->Array();
						VarValue degradation_orig(*degradation_vv);		// redundant here
						degradation_vv->Set(new double[1]{degradation}, 1);

						// Deal with inflation
						VarValue* inflation_vv = nullptr;
						double inflation_orig = std::numeric_limits<double>::quiet_NaN();
						if (financial_name != "None") {
							if (financial_name == "LCOE Calculator") {
								inflation_vv = current_case->Values().Get("c_inflation");
							}
							else {
								inflation_vv = current_case->Values().Get("inflation_rate");
							}
							inflation_orig = inflation_vv->Value();
							inflation_vv->Set(inflation);
						}

						// Simulate
						bcsim.Clear();
						bool ok = bcsim.Invoke();

						// check that the case ran
						if (!ok) {
							m_result_code = 1;
							m_generic_case_window->UpdateResults();
							case_window->SwitchToPage("results:notices");
							wxArrayString messages = current_case->BaseCase().GetAllMessages();
							wxMessageBox("Error in " + technology_name + "\n\n"
								+ technology_name + " returned the following error:\n\n + "
								+ messages.Last(),
								"Combine Cases Message", wxOK, this);
							EndModal(wxID_OK);
							return;
						}

						// optionally display messages returned from the simulation
						wxArrayString messages = current_case->BaseCase().GetAllMessages();
						if (!messages.IsEmpty()) {
							is_notices = true;
						}

						// Add the performance parameters of this case
						double nameplate_this = current_case->Values().Get("system_capacity")->Value();
						nameplate += nameplate_this;
						matrix_t<double> hourly_energy_this = bcsim.GetOutput("gen")->Matrix();		// TODO: what is returned here if it doesn't generate hourly data?

						// need annual energy
						double annual_energy_this = std::numeric_limits<double>::quiet_NaN();
						if (technology_name == "Geothermal Power") {
							annual_energy_this = bcsim.GetOutput("first_year_output")->Value();
						}
						else {
							annual_energy_this = bcsim.GetOutput("annual_energy")->Value();
						}
						annual_energy += annual_energy_this;

						// for lifetime simulation, truncate results to first 8760 values
						double analysis_period_this = std::numeric_limits<double>::quiet_NaN();
						if (financial_name == "LCOE Calculator") {
							analysis_period_this = current_case->Values().Get("c_lifetime")->Value();
						}
						else if (financial_name != "None") {
							analysis_period_this = current_case->Values().Get("analysis_period")->Value();
						}

						// determine hourly generation profile of current case
						bool constant_generation = false;
						bool lifetime = false;
						if (hourly_energy_this.ncells() <= 1) {
							constant_generation = true;		// TODO: Report this somewhere?: "Model does not generate hourly generation data. Calculating constant generation profile from annual energy."
						}
						else if (hourly_energy_this.ncells() == 8760 * analysis_period_this) {
							lifetime = true;				// TODO: Report this somewhere?: "Model runs the simulation over the analysis period. Only Year 1 data will be combined with other cases."
						}
						else if (hourly_energy_this > 8760 * analysis_period_this) {
							m_result_code = 1;
							m_generic_case_window->UpdateResults();
							SamApp::Window()->SwitchToCaseWindow(m_generic_case_name);
							m_generic_case_window->SwitchToInputPage("Power Plant");
							wxMessageBox("Subhourly simulations unsupported\n\n"
								"The subhourly simulation for case " + technology_name + " is not supported.",
								"Combine Cases Message", wxOK, this);
							return;
						}

						for (int i = 0; i < 8760; i++) {
							if (constant_generation) {
								hourly_energy[i] += annual_energy_this / 8760;
							}
							else {
								hourly_energy[i] += hourly_energy_this[i];
							}
						}

						// Add the financial parameters of this case, if applicable
						double total_installed_cost_this = std::numeric_limits<double>::quiet_NaN();
						std::vector<double> om_total_this(analysis_period, std::numeric_limits<double>::quiet_NaN());
						if (financial_name == "LCOE Calculator") {
							total_installed_cost_this = current_case->Values().Get("capital_cost")->Value();
							total_installed_cost += total_installed_cost_this;
							double om_fixed_this = current_case->Values().Get("fixed_operating_cost")->Value();
							double om_variable = current_case->Values().Get("variable_operating_cost")->Value() * annual_energy;
							std::fill(om_total_this.begin(), om_total_this.end(), om_fixed_this + om_variable);
							for (int i = 0; i < analysis_period; i++) {
								om_fixed[i] += om_total_this[i];
							}
						}
						else if (financial_name == "None" || financial_name == "Third Party") {
							total_installed_cost_this = 0.;
							std::fill(om_total_this.begin(), om_total_this.end(), 0.);
						}
						else if (financial_name != "None" && analysis_period > std::numeric_limits<double>::epsilon()) {
							total_installed_cost_this = current_case->Values().Get("total_installed_cost")->Value();
							total_installed_cost += total_installed_cost_this;
							// O&M costs are taken from the cash flows of each system. All types of O&M costs
							// are entered in as fixed O&M costs in the financial case. This accounts for several things:
							// (a) the escalation of O&M costs
							// (b) the O&M costs by capacity and by generation, appropriately weighted by the system size/generation,
							// (c) allows for inclusion of fuel costs that are found in some technologies but not others (fuel costs)
							// TODO: go through whole LK script and ensure all important comments are being carried over
							matrix_t<double> om_fixed_this = bcsim.GetOutput("cf_om_fixed_expense")->Matrix();					// O&M fixed
							matrix_t<double> om_capacity = bcsim.GetOutput("cf_om_capacity_expense")->Matrix();					// O&M capacity based
							matrix_t<double> om_production = bcsim.GetOutput("cf_om_production_expense")->Matrix();				// O&M production based
							matrix_t<double> cf_om_fuel = bcsim.GetOutput("cf_om_fuel_expense")->Matrix();						// O&M fuel
							matrix_t<double> cf_opt_fuel_1 = bcsim.GetOutput("cf_om_opt_fuel_1_expense")->Matrix();				// O&M biomass
							matrix_t<double> cf_opt_fuel_2 = bcsim.GetOutput("cf_om_opt_fuel_2_expense")->Matrix();				// O&M coal

							// Battery costs
							matrix_t<double> om_fixed_this_1(1, analysis_period + 1, 0.);
							matrix_t<double> om_capacity_1(1, analysis_period + 1, 0.);
							matrix_t<double> om_production_1(1, analysis_period + 1, 0.);
							matrix_t<double> cf_batt_repl(1, analysis_period + 1, 0.);
							if (bcsim.GetOutput("cf_om_fixed1_expense")) {
								om_fixed_this_1 = bcsim.GetOutput("cf_om_fixed1_expense")->Matrix();							// battery fixed
								om_capacity_1 = bcsim.GetOutput("cf_om_capacity1_expense")->Matrix();							// battery capacity based
								om_production_1 = bcsim.GetOutput("cf_om_production1_expense")->Matrix();						// battery production based
								cf_batt_repl = bcsim.GetOutput("cf_battery_replacement_cost")->Matrix();						// battery replacement
							}

							// Fuel cell costs
							matrix_t<double> om_fixed_this_2(1, analysis_period + 1, 0.);
							matrix_t<double> om_capacity_2(1, analysis_period + 1, 0.);
							matrix_t<double> om_production_2(1, analysis_period + 1, 0.);
							matrix_t<double> cf_fuelcell_repl(1, analysis_period + 1, 0.);
							if (bcsim.GetOutput("cf_om_fixed2_expense")) {
								om_fixed_this_2 = bcsim.GetOutput("cf_om_fixed2_expense")->Matrix();							// fuel cell fixed
								om_capacity_2 = bcsim.GetOutput("cf_om_capacity2_expense")->Matrix();							// fuel cell capacity based
								om_production_2 = bcsim.GetOutput("cf_om_production2_expense")->Matrix();						// fuel cell production based
								cf_fuelcell_repl = bcsim.GetOutput("cf_fuelcell_replacement_cost")->Matrix();					// fuel cell replacement
							}

							//in cash flows, the first entry in the array is "Year 0", so must call j+1 in loop		
							for (int i = 0; i < analysis_period; i++) {
								// for display in System Costs page note
								om_total_this[i] = om_fixed_this[i + 1] + om_capacity[i + 1] + om_production[i + 1]
									+ om_fixed_this_1[i + 1] + om_capacity_1[i + 1] + om_production_1[i + 1]
									+ om_fixed_this_2[i + 1] + om_capacity_2[i + 1] + om_production_2[i + 1]
									+ cf_om_fuel[i + 1] + cf_opt_fuel_1[i + 1] + cf_opt_fuel_2[i + 1]
									+ cf_batt_repl[i + 1] + cf_fuelcell_repl[i + 1];

								om_fixed[i] += om_total_this[i];
							}
						}

						// put inflation rate back and reinstate other original values
						if (financial_name != "None") {
							inflation_vv->Set(inflation_orig);
						}
						degradation_obj->Property("UseSchedule").Set(degradation_orig_usesched);
						degradation_vv->Set(new double[1]{degradation_orig.Value()}, 1);

						// Update UI with results
						case_window->UpdateResults();
						//case_window->SwitchToInputPage("case_page_orig");		// TODO: switch to original page (how to get current page?)
						case_window->SwitchToPage("results");
					}

					//For user benefit, change the fixed O&M schedule back to a single value if all entries are the same
					bool constant1 = true;
					if (financial_name != "None") {
						for (int i = 1; i < analysis_period; i++) { //don't start at zero because comparing j-1
							if (om_fixed[i] != om_fixed[i - 1]) {
								constant1 = false;
							}
						}
						if (constant1) {
							om_fixed.resize(1);
						}
					}

					// Set the generic system performance parameters
					m_generic_case->Values().Get("system_capacity")->Set(nameplate);
					m_generic_case->Values().Get("spec_mode")->Set(2);		// specify the third radio button
					//m_generic_case->Values().Get("derate")->Set(0);
					//m_generic_case->Values().Get("heat_rate")->Set(0);
					m_generic_case->Values().Get("energy_output_array")->Set(hourly_energy.data(), hourly_energy.ncells());
					m_generic_case->VariableChanged("energy_output_array"); // triggers UI update

					if (financial_name == "LCOE Calculator") {
						if (!constant1) {
							m_result_code = 1;
							m_generic_case_window->UpdateResults();
							SamApp::Window()->SwitchToCaseWindow(m_generic_case_name);
							m_generic_case_window->SwitchToInputPage("Power Plant");
							wxMessageBox("LCOE calculator error\n\n"
								"Single annualized fixed operating costs must be used.\n\n"
								"Check O&M inputs in case " + technology_name,
								"Combine Cases Message", wxOK, this);
							return;
						}
						else {
							m_generic_case->Values().Get("fixed_operating_cost")->Set(om_fixed);
						}
					}

					if (financial_name != "None") {
						//set cost parameters					
						m_generic_case->Values().Get("fixed_plant_input")->Set(total_installed_cost);
						m_generic_case->Values().Get("genericsys.cost.per_watt")->Set(0);
						m_generic_case->Values().Get("genericsys.cost.contingency_percent")->Set(0);
						m_generic_case->Values().Get("genericsys.cost.epc.percent")->Set(0);
						m_generic_case->Values().Get("genericsys.cost.epc.fixed")->Set(0);
						m_generic_case->Values().Get("genericsys.cost.plm.percent")->Set(0);
						m_generic_case->Values().Get("genericsys.cost.plm.fixed")->Set(0);
						m_generic_case->Values().Get("genericsys.cost.sales_tax.percent")->Set(0);

						//set O&M parameters- all are zero except for fixed (see explanation above)
						m_generic_case->Values().Get("om_fixed")->Set(om_fixed);
						m_generic_case->Values().Get("om_capacity")->Set(new double[1]{0}, 1);
						m_generic_case->Values().Get("om_production")->Set(new double[1]{0}, 1);
						//O&M escalation rates are also zeroed because they are accounted for in the fixed O&M costs
						m_generic_case->Values().Get("om_fixed_escal")->Set(0);
						m_generic_case->Values().Get("om_capacity_escal")->Set(0);
						m_generic_case->Values().Get("om_production_escal")->Set(0);

						if (m_generic_case->Values().Get("om_fuel_cost")) {
							m_generic_case->Values().Get("om_fuel_cost")->Set(new double[1]{0}, 1);
							m_generic_case->Values().Get("om_fuel_cost_escal")->Set(0);
						}

						if (m_generic_case->Values().Get("om_replacement_cost1")) {
							m_generic_case->Values().Get("om_replacement_cost1")->Set(new double[1]{0}, 1);
							m_generic_case->Values().Get("om_replacement_cost_escal")->Set(0);
						}
					}

					// Update UI with results
					m_result_code = 0;	// 0=success
					m_generic_case_window->UpdateResults();
					SamApp::Window()->SwitchToCaseWindow(m_generic_case_name);
					m_generic_case_window->SwitchToInputPage("Power Plant");
					ActiveInputPage* aip = 0;
					//wxUIObject* energy_output_array = m_generic_case_window->FindActiveObject("energy_output_array", &aip);	// works only if page containing control is active
					if (is_notices) {
						wxMessageBox("Notices\n\n"
							"At least one of the models generated notices.\n\n"
							"View these messages or warnings on the Notices pane of the Results page.",
							"Combine Cases Message", wxOK, this);
					}
					EndModal(wxID_OK);

					// open up data array dialog
					wxUIObject* energy_output_array = m_generic_case_window->FindObject("energy_output_array", &aip);
					// do assert for non-null
					if (AFDataArrayButton* btn_energy_output_array = energy_output_array->GetNative<AFDataArrayButton>()) {
						// see inputpage.cpp line 421
						btn_energy_output_array->OnPressed(e);
					}
				}
				else if (arychecked.Count() == 1) {
					wxMessageBox("Not enough cases selected.\n\n"
						"Choose at least two cases to combine.",
						"Combine Cases Message", wxOK, this);
				}
				else {
					wxMessageBox("No cases selected.\n\n"
						"Choose at least two cases to combine.",
						"Combine Cases Message", wxOK, this);
				}
			}
			break;
	}
}

// Remake checklist widget with info in cases vector
void CombineCasesDialog::RefreshList(size_t first_item)
{
	m_chlCases->Freeze();
	m_chlCases->Clear();
	for (size_t i = 0; i < m_cases.size(); i++)
	{
		// Exclude generic case from displaying in case list
		// TODO: If excluding geothermal, do here.
		if (m_cases[i].display_name != m_generic_case_name) {
			int ndx = m_chlCases->Append(m_cases[i].display_name);
			if (m_cases[i].is_selected) {
				m_chlCases->Check(ndx, true);
			}
			else {
				m_chlCases->Check(ndx, false);
			}
		}
	}
	m_chlCases->Thaw();
	m_chlCases->SetFirstItem(first_item);
}

void CombineCasesDialog::GetOpenCases()
{
	m_cases.clear();
	wxArrayString names = SamApp::Window()->Project().GetCaseNames();
	for (size_t i = 0; i < names.size(); i++) {
		m_cases.push_back(CaseInfo(names[i], names[i]));
	}
}

std::vector<double> CombineCasesDialog::matrix_to_vector(matrix_t<double> &matrix)
{
	std::vector<double> vector;
	std::copy(matrix.data(), matrix.data() + matrix.ncells(), back_inserter(vector));
	return vector;
}
