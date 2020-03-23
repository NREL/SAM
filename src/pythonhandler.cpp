#include <future>
#include <algorithm>

#ifdef __WXMSW__
#include <windows.h>
#endif

#include "pythonhandler.h"

PythonConfig ReadPythonConfig(const std::string& configPath) {
    // load python configuration
    Json::Value python_config_root;
    std::ifstream python_config_doc(configPath);
    if (python_config_doc.fail())
        throw std::runtime_error("Could not open " + configPath );

    python_config_doc >> python_config_root;

    if (!python_config_root.isMember("miniconda_version"))
        throw std::runtime_error("Missing key 'miniconda_version' in " + configPath);
    if (!python_config_root.isMember("python_version"))
        throw std::runtime_error("Missing key 'python_version' in " + configPath);
    if (!python_config_root.isMember("exec_path"))
        throw std::runtime_error("Missing key 'exec_path' in " + configPath);
    if (!python_config_root.isMember("pip_path"))
        throw std::runtime_error("Missing key 'pip_path' in " + configPath);
    if (!python_config_root.isMember("packages"))
        throw std::runtime_error("Missing key 'packages' in " + configPath);

    std::vector<std::string> packages;
    for (auto &i : python_config_root["packages"])
        packages.push_back(i.asString());

    PythonConfig config = {python_config_root["python_version"].asString(),
                           python_config_root["miniconda_version"].asString(),
                           python_config_root["exec_path"].asString(),
                           python_config_root["pip_path"].asString(),
                           packages};

    return config;
}

void WritePythonConfig(const std::string& configPath, const PythonConfig& config){
    std::ofstream configFile;
    configFile.open(configPath);

    Json::Value configObj;
    configObj["python_version"] = config.pythonVersion;
    configObj["miniconda_version"] = config.minicondaVersion;
    configObj["exec_path"] = config.execPath;
    configObj["pip_path"] = config.execPath;
    configObj["packages"] = Json::arrayValue;
    for (auto &i : config.packages)
        configObj["packages"].append(i);

    Json::StreamWriterBuilder builder;
    std::unique_ptr<Json::StreamWriter> writer(builder.newStreamWriter());
    writer -> write(configObj, &configFile);
    configFile.close();
}

bool CheckPythonInstalled(const PythonConfig& config){
    return !config.execPath.empty() && !config.pipPath.empty();
}

bool InstallPythonWindows(const std::string& path, const PythonConfig& config){
    std::string cmd = "cd " + path + " && powershell.exe -windowstyle hidden -ExecutionPolicy Bypass -File install_python.ps1 -version " + config.pythonVersion + " -config " + path;
    int rvalue = system(cmd.c_str());
    return (bool)rvalue;
}

bool InstallPythonUnix(const std::string& path, const PythonConfig& config){
    std::string cmd = "cd " + path + " && ./install_python.sh " + config.minicondaVersion + " " + config.pythonVersion + " " + path;
    int rvalue = system(cmd.c_str());
    return (bool)rvalue;
}

PythonPackageConfig ReadPythonPackageConfig(const std::string& name, const std::string& configFile){
    // load python configuration
    Json::Value python_config_root;
    std::ifstream python_config_doc(configFile);
    if (python_config_doc.fail())
        throw std::runtime_error("Could not open " + configFile );

    python_config_doc >> python_config_root;

    if (!python_config_root.isMember("min_python_version"))
        throw std::runtime_error("Missing key 'min_python_version' in " + configFile);
    if (!python_config_root.isMember("run_cmd"))
        throw std::runtime_error("Missing key 'run_cmd' in " + configFile);
    if (!python_config_root.isMember("version"))
        throw std::runtime_error("Missing key 'version' in " + configFile);

    PythonPackageConfig config = {name,
                                  python_config_root["min_python_version"].asString(),
                                  python_config_root["run_cmd"].asString(),
                                  python_config_root["version"].asString()};

    return config;
}

bool CheckPythonPackageInstalled(const std::string& package, const PythonConfig& config){
    auto it = std::find(config.packages.begin(), config.packages.end(), package);
    return (it != config.packages.end());
}

bool InstallFromPip(const std::string& pip_exec, const PythonPackageConfig& package){
    std::string cmd = pip_exec + " install " + package.name + "==" + package.version;
    int rvalue = system(cmd.c_str());
    return (bool)rvalue;
}
