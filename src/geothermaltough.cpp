#include <wx/wx.h>
#include <wx/progdlg.h> // For elegant native pop-up progress bars
#include <windows.h>
#include <string>
#include <vector>
#include <thread>
#include <sstream>
#include <fstream>
#include <iostream>

// RapidJSON headers
#include "rapidjson/document.h"
#include "rapidjson/stringbuffer.h"
#include "rapidjson/writer.h"
#include "rapidjson/error/en.h"
#include "rapidjson/prettywriter.h"
#include "geothermaltough.h"
#include "object.h"
// SamApp
#include "main.h"

// Progress bar updating
wxDEFINE_EVENT(wxEVT_PYTHON_PROGRESS, wxThreadEvent);

// Define a unique identifier for our background thread completion event
wxDEFINE_EVENT(wxEVT_PYTHON_THREAD_COMPLETED, wxThreadEvent);

// --- The Core Worker Function ---
// [Identical to prior solution, but returns string payload]
std::string CallPyInstallerWithProgressAndJSON(const std::string& exePath, const std::string& inputJsonStr, wxEvtHandler* eventSink) {
// ---- FIXED: ASYNCHRONOUS HIGH-CAPACITY PIPE STREAM BALANCER ----
    SECURITY_ATTRIBUTES saAttr = { sizeof(SECURITY_ATTRIBUTES), NULL, TRUE };
    HANDLE hChildStd_IN_Rd, hChildStd_IN_Wr, hChildStd_OUT_Rd, hChildStd_OUT_Wr;
    
    if (!CreatePipe(&hChildStd_OUT_Rd, &hChildStd_OUT_Wr, &saAttr, 0) || !SetHandleInformation(hChildStd_OUT_Rd, HANDLE_FLAG_INHERIT, 0)) return "";
    if (!CreatePipe(&hChildStd_IN_Rd, &hChildStd_IN_Wr, &saAttr, 0) || !SetHandleInformation(hChildStd_IN_Wr, HANDLE_FLAG_INHERIT, 0)) { CloseHandle(hChildStd_OUT_Rd); CloseHandle(hChildStd_OUT_Wr); return ""; }
    
// .... (Inside CallPyInstallerWithProgress in main.cpp) ....

    PROCESS_INFORMATION piProcInfo; STARTUPINFOA siStartInfo; 
    ZeroMemory(&piProcInfo, sizeof(PROCESS_INFORMATION)); ZeroMemory(&siStartInfo, sizeof(STARTUPINFOA));
    siStartInfo.cb = sizeof(STARTUPINFOA); 
    siStartInfo.hStdOutput = hChildStd_OUT_Wr; 
    siStartInfo.hStdError = hChildStd_OUT_Wr;
    siStartInfo.dwFlags |= STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW; 
    siStartInfo.wShowWindow = SW_HIDE;
    
    // ---- FIXED: ALIGNED DIRECTORY OVERRIDES ----
    // Force the path mapping to point explicitly inside the PyInstaller onedir subdirectory,
    // ensuring the Windows loader successfully locates the true compiled binary artifact!
           // Strip the executable filename to get the directory path
    size_t lastSlash = exePath.find_last_of("\\/");
    std::string appDir = (lastSlash != std::string::npos) ? exePath.substr(0, lastSlash) : "";


    std::string workingDir = appDir;
    
    std::string cmdLine = "\"" + exePath + "\""; 
    std::vector<char> cmdLineBuf(cmdLine.begin(), cmdLine.end()); 
    cmdLineBuf.push_back('\0');
    
    if (!CreateProcessA(
            NULL, 
            cmdLineBuf.data(), 
            NULL, 
            NULL, 
            TRUE, 
            0, 
            NULL, 
            workingDir.c_str(), // Execute directly inside the folder containing all shared DLL assets
            &siStartInfo, 
            &piProcInfo)) 
    { 
        CloseHandle(hChildStd_OUT_Rd); 
        CloseHandle(hChildStd_OUT_Wr); 
        return ""; 
    }
    
    // Close the write handle on the parent side natively so EOF can propagate later
    CloseHandle(hChildStd_OUT_Wr); 

// .... Keep the rest of your standard ReadFile stream accumulator loops below ....
    CloseHandle(hChildStd_IN_Rd);
    
    // Write out the initial request configurations if needed
    if (!inputJsonStr.empty()) {
        DWORD dwWritten;
        WriteFile(hChildStd_IN_Wr, inputJsonStr.c_str(), (DWORD)inputJsonStr.length(), &dwWritten, NULL);
    }
    CloseHandle(hChildStd_IN_Wr); 

    std::string finalJsonOutput = "";
    std::vector<char> chBuf(65536); // Upgrade buffer tracking capacity to 64KB per chunk
    DWORD dwRead;
    std::string lineAccumulator = "";

    // FIXED LOOP: ReadFile will continue pulling blocks until the pipe closes natively (returns FALSE)
    // This guarantees that massive high-density JSON strings are fully collected without cutoffs.
    while (ReadFile(hChildStd_OUT_Rd, chBuf.data(), (DWORD)chBuf.size() - 1, &dwRead, NULL) && dwRead > 0) {
        chBuf[dwRead] = '\0';
        lineAccumulator.append(chBuf.data(), dwRead);
        
        size_t pos;
        while ((pos = lineAccumulator.find('\n')) != std::string::npos) {
            std::string line = lineAccumulator.substr(0, pos);
            lineAccumulator.erase(0, pos + 1);
            if (!line.empty() && line.back() == '\r') line.pop_back();

            if (line.rfind("PROGRESS:", 0) == 0) {
                try {
                    int percent = std::stoi(line.substr(9));
                    wxThreadEvent progEvt(wxEVT_PYTHON_PROGRESS);
                    progEvt.SetInt(percent);
                    wxQueueEvent(eventSink, progEvt.Clone());
                } catch (...) {}
            } 
            else if (line.rfind("MATRIX_DATA:", 0) == 0) {
                finalJsonOutput = line.substr(12); // Capture the high-density payload string
            }
        }
    }
    
    // ---- FIXED: COMPRESSION AND CACHE RESILIENT EXIT TEARDOWN ----
    // Programmatically ensure that child handles are wiped cleanly, but NEVER
    // allow a secondary process handle check to override or strip out a successfully 
    // collected in-memory finalJsonOutput payload string.
    if (piProcInfo.hProcess != NULL) {
        // Wait with INFINITE to ensure Windows flushes the system registers natively
        WaitForSingleObject(piProcInfo.hProcess, INFINITE);
        CloseHandle(piProcInfo.hProcess);
    }
    if (piProcInfo.hThread != NULL) {
        CloseHandle(piProcInfo.hThread);
    }
    
    CloseHandle(hChildStd_OUT_Rd);
    
    // Explicitly print out a quick diagnostic size log to your IDE compiler console 
    // to confirm that the thousands of high-density nodes are packed in-memory safely.
    OutputDebugStringA(("[System] Pipeline data extraction complete. Buffer bytes: " + 
                        std::to_string(finalJsonOutput.length()) + "\n").c_str());
    
    // Return the string directly!    
    return finalJsonOutput;
}

geothermaltough::geothermaltough(wxWindow* parent, const wxString& title,const wxPoint& pos, const wxSize& size, long style) : wxDialog(parent, wxID_ANY, title, pos, size, style) {
//        wxPanel* panel = new wxPanel(this, wxID_ANY);

        // Layout UI Controls
        wxBoxSizer* sizer = new wxBoxSizer(wxVERTICAL);

        m_runButton = new wxButton(this, wxID_ANY, "Execute PyInstaller Task");

        // Add a clean structural layout gauge bar
        m_progressBar = new wxGauge(this, wxID_ANY, 100, wxDefaultPosition, wxDefaultSize, wxGA_HORIZONTAL);
 
        m_outputText = new wxTextCtrl(this, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE | wxTE_READONLY);

        sizer->Add(m_runButton, 0, wxALL | wxEXPAND, 10);
        sizer->Add(m_progressBar, 0, wxALL | wxEXPAND, 10);

        sizer->Add(m_outputText, 1, wxALL | wxEXPAND, 10);

        sizer->Add(CreateButtonSizer(wxHELP | wxOK | wxCANCEL), 0, wxALL | wxEXPAND, 10);

        SetSizerAndFit(sizer);

        // Bind UI Actions
        m_runButton->Bind(wxEVT_BUTTON, &geothermaltough::OnRunTask, this);
       
        // Bind worker thread synchronization notifications safely
        this->Bind(wxEVT_PYTHON_PROGRESS, &geothermaltough::OnProgressTick, this);

        // Bind the custom thread completion event back to our handler
        this->Bind(wxEVT_PYTHON_THREAD_COMPLETED, &geothermaltough::OnThreadCompletion, this);
    }


void geothermaltough::TestData(const std::string &absoluteJsonPath)
    {

        // Simulate an MxN matrix (e.g., 500 time-steps x 4 reservoir depths)
        const int rows = 500;
        const int cols = 4;
        std::vector<double> matrixData(rows * cols);

        // Populate with dummy simulation data
        for (int r = 0; r < rows; ++r) {
            for (int c = 0; c < cols; ++c) {
                matrixData[r * cols + c] = (r * 0.1) + (c * 0.5); // Row-major index
            }
        }

        // ---- RapidJSON High-Performance Serialization ----
        rapidjson::Document doc;
        doc.SetObject();
        rapidjson::Document::AllocatorType& allocator = doc.GetAllocator();

        // matrix metadata and values passed to the Python executable through rapidJSON serialization
        doc.AddMember("action", "run_georeservoir_sim", allocator);
        doc.AddMember("matrix_rows", rows, allocator);
        doc.AddMember("matrix_cols", cols, allocator);

        // Reserve capacity in the array to prevent incremental allocations
        rapidjson::Value flatArray(rapidjson::kArrayType);
        flatArray.Reserve((rapidjson::SizeType)matrixData.size(), allocator);

        for (double val : matrixData) {
            flatArray.PushBack(val, allocator);
        }
        doc.AddMember("matrix_data", flatArray, allocator);

        // parameters to run TOUGH through the Python wrapper using geophires_x
        doc.AddMember("lifetime", 30, allocator);
        doc.AddMember("depth", 3.5, allocator);
        doc.AddMember("flow_rate", 50.0, allocator);
        doc.AddMember("timesteps_per_year", 4, allocator);  


        // Serialize to string out through your active pipeline
        rapidjson::StringBuffer buffer;
        rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
        doc.Accept(writer);
 //       std::string serializedJson = buffer.GetString();
        std::string jsonRequestString = buffer.GetString();

        // 4. Open and write out the JSON file using the absolute path target
        std::ofstream reqFile;
        reqFile.open(absoluteJsonPath, std::ios::out | std::ios::trunc);
        if (!reqFile.is_open()) {
            m_outputText->SetValue(wxString::Format(L"[Error] Cannot open absolute target path:\n%s\n", absoluteJsonPath));
            m_runButton->Enable(true);
            return;
        }
        reqFile << jsonRequestString;
        reqFile.close();
    }


void geothermaltough::RunExample7Simulation(const std::string &absoluteJsonPath)
    {
        // Create an active RapidJSON memory allocator node frame
        rapidjson::Document doc;
        doc.SetObject();
        rapidjson::Document::AllocatorType& allocator = doc.GetAllocator();

        // --- 1. SUBSURFACE TECHNICAL PARAMETERS ---
        doc.AddMember("Reservoir Model", 6, allocator);
        
        // Alphanumeric strings must explicitly specify memory buffers to prevent scope dropouts
        doc.AddMember("TOUGH2 Model/File Name", rapidjson::Value("Doublet", allocator).Move(), allocator);
        doc.AddMember("depth", 3.0, allocator); // Reservoir Depth [km]
        doc.AddMember("Number of Segments", 1, allocator);
        doc.AddMember("Gradient 1", 50.0, allocator); // [deg.C/km]
        doc.AddMember("Maximum Temperature", 375.0, allocator); // [deg.C]
        doc.AddMember("Number of Production Wells", 1, allocator);
        doc.AddMember("Number of Injection Wells", 1, allocator);
        doc.AddMember("Production Well Diameter", 8.0, allocator); // [inch]
        doc.AddMember("Injection Well Diameter", 8.0, allocator); // [inch]
        doc.AddMember("Ramey Production Wellbore Model", 1, allocator);
        doc.AddMember("Injection Wellbore Temperature Gain", 0.0, allocator); // [deg.C]
        doc.AddMember("flow_rate", 50.0, allocator); // Production Flow Rate per Well [kg/s]

        // --- 2. FRACTURE GEOMETRY & RESERVOIR PROPERTIES ---
        doc.AddMember("Reservoir Volume Option", 4, allocator);
        doc.AddMember("Reservoir Volume", 1000000000.0, allocator); // 1E9 [m3]
        doc.AddMember("Water Loss Fraction", 0.0, allocator);
        doc.AddMember("Reservoir Impedance", 0.05, allocator); // [GPa*s/m3]
        doc.AddMember("Injection Temperature", 70.0, allocator); // [deg.C]
        doc.AddMember("Reservoir Heat Capacity", 1050.0, allocator); // [J/kg/K]
        doc.AddMember("Reservoir Density", 2700.0, allocator); // [kg/m3]
        doc.AddMember("Reservoir Thermal Conductivity", 3.0, allocator); // [W/m/K]
        doc.AddMember("Reservoir Porosity", 0.05, allocator);
        doc.AddMember("Reservoir Permeability", 6E-13, allocator); // [m2]
        doc.AddMember("Reservoir Thickness", 250.0, allocator); // [m]
        doc.AddMember("Reservoir Width", 500.0, allocator); // [m]
        doc.AddMember("Well Separation", 900.0, allocator); // [m]

        // --- 3. SURFACE TECHNICAL PARAMETERS ---
        doc.AddMember("End-Use Option", 2, allocator); // Direct-Use Heat
        doc.AddMember("Circulation Pump Efficiency", 0.8, allocator);
        doc.AddMember("Utilization Factor", 0.9, allocator);
        doc.AddMember("End-Use Efficiency Factor", 0.9, allocator);
        doc.AddMember("Surface Temperature", 15.0, allocator); // [deg.C]
        doc.AddMember("Ambient Temperature", 15.0, allocator); // [deg.C]

        // --- 4. ECONOMIC & FINANCIAL PARAMETERS ---
        doc.AddMember("lifetime", 30, allocator); // Plant Lifetime [years]
        doc.AddMember("Economic Model", 2, allocator); // Standard LCOE/LCOH model
        doc.AddMember("Discount Rate", 0.05, allocator);
        doc.AddMember("Inflation Rate During Construction", 0.0, allocator);
        doc.AddMember("Well Drilling and Completion Capital Cost Adjustment Factor", 1.0, allocator);
        doc.AddMember("Well Drilling Cost Correlation", 1, allocator);
        doc.AddMember("Reservoir Stimulation Capital Cost Adjustment Factor", 0.0, allocator);
        doc.AddMember("Surface Plant Capital Cost Adjustment Factor", 1.0, allocator);
        doc.AddMember("Field Gathering System Capital Cost Adjustment Factor", 1.0, allocator);
        doc.AddMember("Exploration Capital Cost Adjustment Factor", 1.0, allocator);
        doc.AddMember("Wellfield O&M Cost Adjustment Factor", 1.0, allocator);
        doc.AddMember("Surface Plant O&M Cost Adjustment Factor", 1.0, allocator);
        doc.AddMember("Water Cost Adjustment Factor", 1.0, allocator);
        doc.AddMember("Electricity Rate", 0.07, allocator); // [$/kWh]

        // --- 5. SIMULATION PARAMETERS ---
        doc.AddMember("Print Output to Console", 1, allocator);
        doc.AddMember("Time steps per year", 4, allocator);
        
        // Retain hydrostatic single-phase safety floor
        doc.AddMember("pressure", 4200.0, allocator); 
        // TODO this is read in by the macro 
        doc.AddMember("TOUGH2 Executable Path", rapidjson::Value("C:\\Users\\janzo\\xt2_eos1.exe", allocator).Move(), allocator);


        // --- 6. SERIALIZATION AND WRITER STREAM PIPELINE ---
        rapidjson::StringBuffer buffer;
        rapidjson::PrettyWriter<rapidjson::StringBuffer> writer(buffer);
        doc.Accept(writer);

        // Flush the unrolled text buffer string cleanly out onto the physical workspace
        std::ofstream ofs(absoluteJsonPath);
        if (ofs.is_open()) {
            ofs << buffer.GetString();
            ofs.flush();
            ofs.close();
        }
    }

void geothermaltough::OnRunTask(wxCommandEvent& event) {
        m_runButton->Enable(false);
        m_progressBar->SetValue(0);

        m_outputText->AppendText("Serializing outbound JSON and launching worker thread...\n");


        // 2. Define the strict, absolute file targets
        wxString absoluteJsonPath = SamApp::GetRuntimePath() + "bin\\dist\\cpp_request.json";
        wxString absolutePyExePath = SamApp::GetRuntimePath() + "bin\\dist\\myscript_v2.exe";


//        TestData(absoluteJsonPath);
        RunExample7Simulation(absoluteJsonPath.ToStdString());

//        m_outputText->AppendText(wxString::Format(L"--- matrix(250,2) sent ---\n%lg\n---------------------------\n", matrixData[249+rows*1]));
//        m_outputText->AppendText(wxString::Format(L"--- RAW INPUT SENT ---\n%s\n---------------------------\n", wxString::FromUTF8(serializedJson)));


        // 2. Spawn a native C++ worker thread to handle the blocking process execution
        DWORD timeoutMs = 30000;
/*
        std::thread worker([this, exePath, serializedJson, timeoutMs]() {
            // Run the synchronous pipe function inside the background context
            //std::string rawResult = CallPyInstallerWithJson(exePath, serializedJson, timeoutMs);
            std::string rawResult = CallPyInstallerWithProgressAndJSON(exePath, serializedJson, this);
*/
        std::thread worker([this, absolutePyExePath]() {
            // Pass a dummy or empty string since we are transferring data via file now
            std::string rawResult = CallPyInstallerWithProgressAndJSON(absolutePyExePath.ToStdString(), "", this);

            // Create a safe event container to carry the raw string response to the main thread
            wxThreadEvent threadEvt(wxEVT_PYTHON_THREAD_COMPLETED);
            // Convert std::string to std::wstring inline via standard MultiByteToWideChar conversion
            int wchars_num = MultiByteToWideChar(CP_UTF8, 0, rawResult.c_str(), -1, NULL, 0);
            std::vector<wchar_t> wstr_buf(wchars_num);
            MultiByteToWideChar(CP_UTF8, 0, rawResult.c_str(), -1, wstr_buf.data(), wchars_num);

            threadEvt.SetString(wxString(wstr_buf.data()));


            // Queue the event into the wxWidgets main loop thread safely
            wxQueueEvent(this, threadEvt.Clone());
            });

        // Detach the thread handle so it cleans up after its context function finishes executing
        worker.detach();
    }


void geothermaltough::OnProgressTick(wxThreadEvent& event) {
        int progression = event.GetInt();
        m_progressBar->SetValue(progression);
        m_outputText->AppendText(wxString::Format(L"Executing simulation steps... %d%% complete\n", progression));
    }


void geothermaltough::OnThreadCompletion(wxThreadEvent& event) {
        m_runButton->Enable(true);
        m_progressBar->SetValue(100);

        wxString rawResult = event.GetString();
        std::string jsonStr = std::string(rawResult.mb_str(wxConvUTF8));

		//TODO : Adjust after PYInstaller added to SAM build process, for now we can hardcode the path to the output file
        //std::string targetFilePath = "C:\\Projects\\Github\\NREL\\GeoSAM\\wxWidgetsTOUGHExample\\build\\Release\\dist\\py_response.json";
        wxString targetFilePath = SamApp::GetRuntimePath() + "bin\\dist\\py_response.json";



        // On Windows, give the operating system a split second to clear background 
        // thread write locks before attempting to open the payload file.
        bool fileReadSuccess = false;
        for (int retry = 0; retry < 5; ++retry) {
            std::ifstream resFile(targetFilePath.ToStdString());
            if (resFile.is_open()) {
                std::string fileContent((std::istreambuf_iterator<char>(resFile)), std::istreambuf_iterator<char>());
                resFile.close();
                
                if (!fileContent.empty() && fileContent.back() == '}') {
                    jsonStr = fileContent;
                    fileReadSuccess = true;
                    break;
                }
            }
            wxMilliSleep(200); // Brief pause before checking filesystem handle flags again
        }

        if (fileReadSuccess) {
            // Clean out file asset cleanly so next simulation runs fresh
            std::remove(targetFilePath.c_str());
        }

        // 1. Initialize RapidJSON Document parser
        rapidjson::Document doc;
        doc.Parse(jsonStr.c_str());

        if (doc.HasParseError() || !doc.HasMember("status") || std::string(doc["status"].GetString()) != "success") {
            m_outputText->AppendText(L"[Error] Malformed payload payload. Missing matrix array tracking container.\n");
            m_outputText->AppendText(L"--- RAW PIPELINE RESPONSE SECTOR ---\n");
            m_outputText->AppendText(wxString::FromUTF8(jsonStr.c_str()) + L"\n");
            
            wxMessageBox(L"Task timed out or process execution failed.", L"Execution Error", wxOK | wxICON_ERROR);
            return;
        }

        // 3. Extract and validate matrix structural layout constraints
        if (!doc.HasMember("matrix_data") || !doc["matrix_data"].IsArray()) {
            m_outputText->AppendText(L"[Error] Malformed payload payload. Missing matrix array tracking container.\n");
            return;
        }

        int totalRows = doc.HasMember("matrix_rows") ? doc["matrix_rows"].GetInt() : 0;
        int totalCols = doc.HasMember("matrix_cols") ? doc["matrix_cols"].GetInt() : 3; // Defaults to 3 [Step, Temp, Press]
        
        const rapidjson::Value& flatDataArray = doc["matrix_data"];
        
        // Safety verification check: Matrix size must match rows * columns parameters
        if (flatDataArray.Size() != static_cast<rapidjson::SizeType>(totalRows * totalCols)) {
            m_outputText->AppendText(L"[Error] Serialization mismatch. Total flattened indices do not line up with bounds.\n");
            return;
        }

        m_outputText->AppendText(wxString::Format(L"Successfully parsed %d timesteps from TOUGH simulation model.\n\n", totalRows));
        m_outputText->AppendText(L"Timestep\tTemp (°C)\tPressure (kPa)\n");
        m_outputText->AppendText(L"--------------------------------------------------\n");

        // 4. Extract data vectors structures line-by-line sequentially
        // We isolate data back to standard native memory structures
        std::vector<double> timeSteps;
        std::vector<double> temperatures;
        std::vector<double> pressures;

        timeSteps.reserve(totalRows);
        temperatures.reserve(totalRows);
        pressures.reserve(totalRows);

        for (int i = 0; i < totalRows; ++i) {
            // Unroll mapping offsets out of the row-major flat block index positions
            int baseIndex = i * totalCols;
            
            double step  = flatDataArray[baseIndex + 0].GetDouble();
            double temp  = flatDataArray[baseIndex + 1].GetDouble();
            double press = flatDataArray[baseIndex + 2].GetDouble();

            timeSteps.push_back(step);
            temperatures.push_back(temp);
            pressures.push_back(press);

            // Append logging data metrics to the text control display view panel
            m_outputText->AppendText(wxString::Format(L"%.2f\t\t%.2f\t\t%.2f\n", step, temp, press));
        }

    // ---- Data Ready For Engineering Analysis ----
    // You now possess clean, separate vectors (timeSteps, temperatures, pressures)
    // ready to link into mathematical processors or visualization charts.
       m_outputText->AppendText(L"\nPipeline transaction completed smoothly.\n");    
    
    // Auto-dump spreadsheet record to disk
        std::string reportPath = "Simulation_Report.csv";
        ExportSimulationReport(reportPath, timeSteps, temperatures, pressures);

		// setup a matrix_t<double> to hold the results for further processing if needed
        m_results = matrix_t<double>(timeSteps.size(), 3);
        for (size_t i = 0; i < timeSteps.size(); ++i) {
            m_results(i, 0) = timeSteps[i];
            m_results(i, 1) = temperatures[i];
            m_results(i, 2) = pressures[i];
		}


    }

void geothermaltough::ExportSimulationReport(const std::string& filename,
                                                const std::vector<double>& timeSteps, 
                                                const std::vector<double>& temperatures, 
                                                const std::vector<double>& pressures) {
        
        // Open standard file output stream (handles Unicode file paths via standard library)
        std::ofstream csvFile(filename);
        if (!csvFile.is_open()) {
            wxLogError(L"Failed to initialize file writing permissions for target report path.");
            return;
        }

        // 1. Output Metadata Header Blocks
        csvFile << "GEOPHIRES-X / TOUGH Coupled Reservoir Simulation Report\n";
        csvFile << "Execution Framework,wxWidgets Application Pipeline\n";
        csvFile << "Simulated Profile Columns,3\n\n";

        // 2. Output Data Column Mapping Headers
        csvFile << "Timestep (Years),Production Temperature (deg C),Production Pressure (kPa)\n";

        // 3. Write Matrix Profiles Iteratively Line-By-Line
        for (size_t i = 0; i < timeSteps.size(); ++i) {
            csvFile << timeSteps[i] << "," 
                    << temperatures[i] << "," 
                    << pressures[i] << "\n";
        }

        csvFile.close();
        m_outputText->AppendText(L"\n[System] Automated CSV simulation report saved successfully.\n");
    }
