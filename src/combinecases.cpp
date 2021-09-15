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

CombineCasesDialog::CombineCasesDialog(wxWindow* parent, const wxString& title)
    : wxDialog(parent, wxID_ANY, title, wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
{
	
	// Text at top of window
	wxString msg = "Select open cases, simulate those cases and combine their generation profiles into a single profile to be used with this generic case.";

	// Case selection
	m_chlCases = new wxCheckListBox(this, ID_chlCases, wxDefaultPosition, wxSize(800, 200)); // populate with active cases
	this->GetOpenCases();
	this->RefreshList(0.);
	wxBoxSizer* szcases = new wxBoxSizer(wxVERTICAL);
	szcases->Add(new wxStaticText(this, wxID_ANY, "1. Select cases:"), 0, wxALL, 2);
	szcases->Add(m_chlCases, 10, wxALL | wxEXPAND, 1);

	// Overwrite capital checkbox
	m_chkOverwriteCapital = new wxCheckBox(this, ID_chkOverwriteCapital, "Overwrite Capital Expenses");

	// Annual AC degradation
	m_spndDegradation = new wxSpinCtrlDouble(this, ID_spndDegradation, "Annual AC Degradation");

	// Combine all into main vertical sizer
	wxBoxSizer* szmain = new wxBoxSizer(wxVERTICAL);
	szmain->Add(new wxStaticText(this, wxID_ANY, msg), 0, wxALL | wxEXPAND, 10);
	szmain->Add(szcases, 10, wxALL | wxEXPAND, 1);
	szmain->Add(m_chkOverwriteCapital);
	szmain->Add(m_spndDegradation);
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
				for (size_t i = 0; i < m_cases.size(); i++)
				{
					if (m_cases[i].display_name == m_chlCases->GetString(e.GetInt()))
					{
						m_cases[i].is_selected = m_chlCases->IsChecked(e.GetInt());
					}
				}
			}
			break;
		case wxID_OK:
			{
				wxArrayInt arychecked;
				// Find which cases are selected
				for (size_t i = 0; i < m_cases.size(); i++)	{
					if (m_cases[i].is_selected) {
						arychecked.push_back((int)i);
					}
				}
				if (arychecked.Count() >= 1) {
					bool overwrite_capital = m_chkOverwriteCapital->IsChecked();
					double degradation = m_spndDegradation->GetValue();

					// Simulate each case
					for (size_t i = 0; i < arychecked.Count(); i++) {
						// Switch to case
						SamApp::Window()->SwitchToCaseWindow(m_cases[arychecked[i]].name);
						Case* current_case = SamApp::Window()->GetCurrentCase();
						CaseWindow* case_window = SamApp::Window()->GetCaseWindow(current_case);
						//cw->UpdateConfiguration();
						Simulation& bcsim = current_case->BaseCase();

						// Grab inputs needed for combined case in generic model
						// -degradation value and 
						double degradation_orig = bcsim.GetInput("degradation")->Value();						// 'SchedNumeric', toggle 'UseSchedule'
						ActiveInputPage* aip = 0;
                        // ** this works only if page that contains the control is selected in the "current_case" **
						// case_window->SwitchToInputPage("Lifetime and Degradation");
						// wxUIObject* degradation_obj = case_window->FindActiveObject("degradation", &aip);
						wxUIObject* degradation_obj = case_window->FindObject("degradation", &aip);
						assert(degradation_obj && aip && degradation_obj->HasProperty("UseSchedule"));
						bool use_schedule_orig = degradation_obj->Property("UseSchedule").GetBoolean();



						// Set Annual AC degradation rate to the combine cases input value
						//  save whether Value or Sched, and save Value value



						// Simulate current case
						bcsim.Clear();
						bool result = bcsim.Invoke();

						// Get outputs and notices
						//double annual_energy = bcsim.GetOutput("annual_energy")->Value();
						//matrix_t<double> gen = bcsim.GetOutput("gen")->Matrix();
						//wxArrayString messages = c->BaseCase().GetAllMessages();

						// Reinstate original values
						degradation_obj->Property("UseSchedule").Set(use_schedule_orig);

						// Update UI with results
						case_window->UpdateResults();
						// Maybe just return the value instead of setting the table value directly?

						// Set array
						//if (auto pvv = current_case->Values().Get("degradation")) {
						//	double v[] = { 1.0, 2.0, 3.0, 4.0 };
						//	pvv->Set(v, 4);
						//	current_case->VariableChanged("degradation"); // triggers UI update
						//}
						int x = 1;
					}

					EndModal(wxID_OK);
				}
				else if (arychecked.Count() == 1) {
					wxMessageBox("Not enough cases selected.\n\nChoose at least two cases to combine.", "Combine Cases Message", wxOK, this);
				}
				else {
					wxMessageBox("No cases selected.\n\nChoose at least two cases to combine.", "Combine Cases Message", wxOK, this);
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
		int ndx = m_chlCases->Append(m_cases[i].display_name);
		if (m_cases[i].is_selected) {
			m_chlCases->Check(ndx, true);
		}
		else {
			m_chlCases->Check(ndx, false);
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