#ifndef geothermaltough_H
#define geothermaltough_H


#include <wx/wx.h>
#include <windows.h>
#include <string>
#include <vector>
#include "object.h"


// --- Main application Window ---
class geothermaltough : public wxDialog {
public:
    geothermaltough(wxWindow* parent, const wxString& title = "GeoTOUGH Multi-Physics Simulation Studio",
        const wxPoint& pos = wxDefaultPosition,
        const wxSize& size = wxSize(1024, 768), long style = (wxCAPTION | wxCLOSE_BOX | wxCLIP_CHILDREN | wxRESIZE_BORDER));
    virtual ~geothermaltough() {};

    matrix_t<double> GetResults() { return m_results; }

    void SetParameters(const double& length, const double& width, const double& depth, const double& porosity, const double& permeability, const double& temperature, const double& pressure, const wxString& tough_exe);

private:

    void TestData(const std::string &absoluteJsonPath);
    void RunExample7Simulation(const std::string &absoluteJsonPath);
    void SetTOUGHParameters(const std::string& absoluteJsonPath) const;
    void OnRunTask(wxCommandEvent& event);
    void OnProgressTick(wxThreadEvent& event);
    void OnThreadCompletion(wxThreadEvent& event);
    void ExportSimulationReport(const std::string& filename,
                                                const std::vector<double>& timeSteps, 
                                                const std::vector<double>& temperatures, 
                                                const std::vector<double>& pressures);
    void PlotOutput(const std::vector<double>& timeSteps,
        const std::vector<double>& temperatures,
        const std::vector<double>& pressures);


    wxButton* m_runButton;
    wxGauge* m_progressBar;
    wxTextCtrl* m_outputText;
    wxPanel* m_panel;

	matrix_t<double> m_results;

	double m_length;
	double m_width;
	double m_depth;
	double m_porosity;
	double m_permeability;
	double m_temperature;
	double m_pressure;
    wxString m_tough_exe;

};

#endif // geothermaltough_H
