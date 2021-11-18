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

#include <unordered_map>
#include <future>
#include <algorithm>
#include <sstream>


#ifdef __WXMSW__
#pragma warning(disable: 4191)
#include "AtlBase.h"
#include "AtlConv.h"
#endif

#include "pythonhandler.h"
#include <ssc/../rapidjson/document.h>
#include <ssc/../rapidjson/istreamwrapper.h>
#include <ssc/../rapidjson/stringbuffer.h>
#include <ssc/../rapidjson/prettywriter.h>



PythonConfig ReadPythonConfig(const std::string& configPath) {
    // load python configuration
    rapidjson::Document python_config_root;
    std::ifstream python_config_doc(configPath);
    if (python_config_doc.fail())
        throw std::runtime_error("Could not open " + configPath );


#ifdef __WXMSW__
	// check for byte-order mark indicating UTF-8 and skip if it exists since it's not JSON-compatible
	char a, b, c;
	a = (char)python_config_doc.get();
	b = (char)python_config_doc.get();
	c = (char)python_config_doc.get();
	if (a != (char)0xEF || b != (char)0xBB || c != (char)0xBF) {
		python_config_doc.seekg(0);
	}
#endif

    std::ostringstream tmp;
    tmp << python_config_doc.rdbuf();
    python_config_root.Parse(tmp.str().c_str());

    if (!python_config_root.HasMember("miniconda_version"))
        throw std::runtime_error("Missing key 'miniconda_version' in " + configPath);
    if (!python_config_root.HasMember("python_version"))
        throw std::runtime_error("Missing key 'python_version' in " + configPath);
    if (!python_config_root.HasMember("exec_path"))
        throw std::runtime_error("Missing key 'exec_path' in " + configPath);
    if (!python_config_root.HasMember("pip_path"))
        throw std::runtime_error("Missing key 'pip_path' in " + configPath);
    if (!python_config_root.HasMember("packages"))
        throw std::runtime_error("Missing key 'packages' in " + configPath);

    std::vector<std::string> packages;
    for (auto &i : python_config_root["packages"].GetArray())
        packages.push_back(i.GetString());

    std::unordered_map<std::string, std::string> options;
 
    if (python_config_root.HasMember("options")){
        rapidjson::Value json_val = python_config_root["options"].GetObject();
        for (rapidjson::Value::ConstMemberIterator itr = json_val.MemberBegin(); itr != json_val.MemberEnd(); ++itr) {
            options.insert({ itr->name.GetString(),itr->value.GetString() });
        }
    }

    PythonConfig config = {python_config_root["python_version"].GetString(),
                           python_config_root["miniconda_version"].GetString(),
                           python_config_root["exec_path"].GetString(),
                           python_config_root["pip_path"].GetString(),
                           packages,
                           options};

    return config;
}

void WritePythonConfig(const std::string& configPath, const PythonConfig& config){
    std::ofstream configFile;
    configFile.open(configPath);

    rapidjson::Document configObj;
    configObj.SetObject();
    configObj.AddMember(rapidjson::Value("python_version", configObj.GetAllocator()).Move(), rapidjson::Value(config.pythonVersion.c_str(), configObj.GetAllocator()).Move(), configObj.GetAllocator());
    configObj.AddMember(rapidjson::Value("miniconda_version", configObj.GetAllocator()).Move(), rapidjson::Value(config.minicondaVersion.c_str(), configObj.GetAllocator()).Move(), configObj.GetAllocator());
    configObj.AddMember(rapidjson::Value("exec_path", configObj.GetAllocator()).Move(), rapidjson::Value(config.execPath.c_str(), configObj.GetAllocator()).Move(), configObj.GetAllocator());
    configObj.AddMember(rapidjson::Value("pip_path", configObj.GetAllocator()).Move(), rapidjson::Value(config.pipPath.c_str(), configObj.GetAllocator()).Move(), configObj.GetAllocator());
    // array
    rapidjson::Value json_val;
    json_val.SetArray();
    for (auto& i : config.packages) {
        json_val.PushBack(rapidjson::Value(i.c_str(), configObj.GetAllocator()), configObj.GetAllocator());
    }
    configObj.AddMember(rapidjson::Value("packages", configObj.GetAllocator()).Move(), json_val.Move(), configObj.GetAllocator());

    json_val.Clear();
    json_val.SetObject();
    for (auto& i : config.options) {
        json_val.AddMember(rapidjson::Value(i.first.c_str(), configObj.GetAllocator()).Move(), rapidjson::Value(i.second.c_str(), configObj.GetAllocator()).Move(), configObj.GetAllocator());
    }
    configObj.AddMember(rapidjson::Value("options", configObj.GetAllocator()).Move(), json_val.Move(), configObj.GetAllocator());

     rapidjson::StringBuffer buffer;
    buffer.Clear();
    rapidjson::PrettyWriter<rapidjson::StringBuffer> writer(buffer);
    configObj.Accept(writer);
    configFile << buffer.GetString();
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
    rapidjson::Document python_config_root;
    std::ifstream python_config_doc(configFile);
    if (python_config_doc.fail())
        throw std::runtime_error("Could not open " + configFile );

    std::ostringstream tmp;
    tmp << python_config_doc.rdbuf();
    python_config_root.Parse(tmp.str().c_str());


    if (!python_config_root.HasMember("min_python_version"))
        throw std::runtime_error("Missing key 'min_python_version' in " + configFile);
    if (!python_config_root.HasMember("run_cmd"))
        throw std::runtime_error("Missing key 'run_cmd' in " + configFile);
    if (!python_config_root.HasMember("version"))
        throw std::runtime_error("Missing key 'version' in " + configFile);

    PythonPackageConfig config = {name,
                                  python_config_root["min_python_version"].GetString(),
                                  python_config_root["run_cmd"].GetString(),
                                  python_config_root["version"].GetString()};

    return config;
}

bool CheckPythonPackageInstalled(const std::string& package, const PythonConfig& config){
    auto it = std::find(config.packages.begin(), config.packages.end(), package);
    return (it != config.packages.end());
}

#ifdef __WXMSW__
int InstallFromPipWindows(const std::string& pip_exec, const PythonPackageConfig& package){
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
