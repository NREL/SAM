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

private:

    void TestData(const std::string &absoluteJsonPath);
    void RunExample7Simulation(const std::string &absoluteJsonPath);
    void OnRunTask(wxCommandEvent& event);
    void OnProgressTick(wxThreadEvent& event);
    void OnThreadCompletion(wxThreadEvent& event);
    void ExportSimulationReport(const std::string& filename,
                                                const std::vector<double>& timeSteps, 
                                                const std::vector<double>& temperatures, 
                                                const std::vector<double>& pressures);

    wxButton* m_runButton;
    wxGauge* m_progressBar;
    wxTextCtrl* m_outputText;

	matrix_t<double> m_results;
};

#endif // geothermaltough_H
