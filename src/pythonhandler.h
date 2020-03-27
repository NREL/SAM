
#ifndef SYSTEM_ADVISOR_MODEL_PYTHONHANDLER_H
#define SYSTEM_ADVISOR_MODEL_PYTHONHANDLER_H

#include <string>
#include <vector>
#include <fstream>

#include <ssc/sscapi.h>
#include <json/json.h>

struct PythonConfig {
    std::string pythonVersion;
    std::string minicondaVersion;
    std::string execPath;
    std::string pipPath;
    std::vector<std::string> packages;
    std::unordered_map<std::string, std::string> options;
};

PythonConfig ReadPythonConfig(const std::string& configPath);

void WritePythonConfig(const std::string& configPath, const PythonConfig& config);

bool CheckPythonInstalled(const PythonConfig& config);

bool InstallPythonWindows(const std::string& path, const PythonConfig& config);

bool InstallPythonUnix(const std::string& path, const PythonConfig& config);

struct PythonPackageConfig {
    std::string name;
    std::string minPythonVersion;
    std::string runCmd;
    std::string version;
};

PythonPackageConfig ReadPythonPackageConfig(const std::string& name, const std::string& configFile);

bool CheckPythonPackageInstalled(const std::string& package, const PythonConfig& config);

#ifdef __WXMSW__
int InstallFromPipWindows(const std::string& pip_exec, const PythonPackageConfig& package);
#endif

int InstallFromPip(const std::string& pip_exec, const PythonPackageConfig& package);

#endif //SYSTEM_ADVISOR_MODEL_PYTHONHANDLER_H
