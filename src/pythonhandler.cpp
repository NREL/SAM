#include <unordered_map>
#include <future>
#include <algorithm>

#ifdef __WXMSW__
#include "AtlBase.h"
#include "AtlConv.h"
#endif

#include "pythonhandler.h"

PythonConfig ReadPythonConfig(const std::string& configPath) {
    // load python configuration
    Json::Value python_config_root;
    std::ifstream python_config_doc(configPath);
    if (python_config_doc.fail())
        throw std::runtime_error("Could not open " + configPath );

#ifdef __WXMSW__
	// check for byte-order mark indicating UTF-8 and skip if it exists since it's not JSON-compatible
	char a, b, c;
	a = python_config_doc.get();
	b = python_config_doc.get();
	c = python_config_doc.get();
	if (a != (char)0xEF || b != (char)0xBB || c != (char)0xBF) {
		python_config_doc.seekg(0);
	}
#endif

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

    std::unordered_map<std::string, std::string> options;
    if (python_config_root.isMember("options")){
        Json::Value json_val = python_config_root["options"];
        Json::Value::Members members = json_val.getMemberNames();
        for (auto const &name : members) {
            options.insert({name, json_val[name].asString()});
        }
    }

    PythonConfig config = {python_config_root["python_version"].asString(),
                           python_config_root["miniconda_version"].asString(),
                           python_config_root["exec_path"].asString(),
                           python_config_root["pip_path"].asString(),
                           packages,
                           options};

    return config;
}

void WritePythonConfig(const std::string& configPath, const PythonConfig& config){
    std::ofstream configFile;
    configFile.open(configPath);

    Json::Value configObj;
    configObj["python_version"] = config.pythonVersion;
    configObj["miniconda_version"] = config.minicondaVersion;
    configObj["exec_path"] = config.execPath;
    configObj["pip_path"] = config.pipPath;
    configObj["packages"] = Json::arrayValue;
    for (auto &i : config.packages)
        configObj["packages"].append(i);
    for (auto &i : config.options)
        configObj["options"][i.first] = i.second;

    Json::StreamWriterBuilder builder;
    std::unique_ptr<Json::StreamWriter> writer(builder.newStreamWriter());
    writer -> write(configObj, &configFile);
    configFile.close();
}

bool CheckPythonInstalled(const PythonConfig& config){
    return !config.execPath.empty() && !config.pipPath.empty();
}

bool InstallPythonWindows(const std::string& path, const PythonConfig& config){
    std::string cmd = "powershell.exe -windowstyle hidden -ExecutionPolicy Bypass -File " + path + "/install_python.ps1 -version " + config.pythonVersion + " -config " + path;
    int rvalue = system(cmd.c_str());
    return (bool)rvalue;
}

bool InstallPythonUnix(const std::string& path, const PythonConfig& config){
    std::string cmd = path + "/install_python.sh " + config.minicondaVersion + " " + config.pythonVersion + " " + path;
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

#ifdef __WXMSW__
int InstallFromPipWindows(const std::string& pip_exec, const PythonPackageConfig& package)
{
	std::string args = " install " + package.name + "==" + package.version;
	PROCESS_INFORMATION p_info;
	STARTUPINFO s_info;
	DWORD ReturnValue;
	CA2T programpath(pip_exec.c_str());
	CA2T programargs(args.c_str());

	memset(&s_info, 0, sizeof(s_info));
	memset(&p_info, 0, sizeof(p_info));
	s_info.cb = sizeof(s_info);

	if (CreateProcess(programpath, programargs, NULL, NULL, 0, CREATE_NO_WINDOW, NULL, NULL, &s_info, &p_info)) {
		WaitForSingleObject(p_info.hProcess, INFINITE);
		GetExitCodeProcess(p_info.hProcess, &ReturnValue);
		CloseHandle(p_info.hProcess);
		CloseHandle(p_info.hThread);
	}
	return ReturnValue;
}
#endif

int InstallFromPip(const std::string& pip_exec, const PythonPackageConfig& package){
    std::string cmd = pip_exec + " install " + package.name + "==" + package.version;
    int rvalue = system(cmd.c_str());
    return rvalue;
}
