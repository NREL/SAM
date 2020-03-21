
#ifndef SYSTEM_ADVISOR_MODEL_PYTHONINSTALL_H
#define SYSTEM_ADVISOR_MODEL_PYTHONINSTALL_H

#include <string>
#include <fstream>

#include <ssc/sscapi.h>
#include <json/json.h>

struct PythonConfig {
    std::string pythonVersion;
    std::string minicondaVersion;
    std::string execPath;
    std::string pipPath;
};

PythonConfig ReadPythonConfig(const std::string& configPath);

bool CheckPythonInstalled(const PythonConfig& config);

bool InstallPythonWindows(const std::string& path, const PythonConfig& config);

bool InstallPythonUnix(const std::string& path, const PythonConfig& config);

struct pythonPackageConfig {
    std::string name;
    std::string minPythonVersion;
    std::string runCmd;
    std::string version;
};

pythonPackageConfig ReadPythonPackageConfig(const std::string& name, const std::string& configFile);

bool InstallFromPip(const std::string& pip_exec, const pythonPackageConfig& package);

#endif //SYSTEM_ADVISOR_MODEL_PYTHONINSTALL_H
