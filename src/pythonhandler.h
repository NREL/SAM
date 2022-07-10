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

#ifndef SYSTEM_ADVISOR_MODEL_PYTHONHANDLER_H
#define SYSTEM_ADVISOR_MODEL_PYTHONHANDLER_H

#include <string>
#include <vector>
#include <fstream>

#include <ssc/sscapi.h>

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
