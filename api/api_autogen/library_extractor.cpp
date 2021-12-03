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

#include <utility>
#include <fstream>

#if defined(_WIN32)
#include <Windows.h>
#endif

#include "library_extractor.h"
#include "lk_eval.h"

#if defined(_WIN32)
std::wstring string_to_wstring(const std::string& s)
{
	int len;
	int slength = (int)s.length() + 1;
	len = MultiByteToWideChar(CP_ACP, 0, s.c_str(), slength, 0, 0);
	wchar_t* buf = new wchar_t[len];
	MultiByteToWideChar(CP_ACP, 0, s.c_str(), slength, buf, len);
	std::wstring r(buf);
	delete[] buf;
	return r;
}
#endif


std::vector<std::string> get_files_in_directory(const std::string& library_dir) {
    std::vector<std::string> names;

#if !defined(_WIN32)
    DIR *dir = opendir(library_dir.c_str());
    if (!dir)
        return names;
    struct dirent *ent;
    while ((ent = readdir(dir)) != nullptr) {
        names.emplace_back(ent->d_name);
    }
    closedir(dir);
#else
    std::string search_path = library_dir + "/*.*";
	std::wstring stemp = string_to_wstring(search_path);
    WIN32_FIND_DATA fd;
    HANDLE hFind = ::FindFirstFile(stemp.c_str(), &fd);
    if (hFind != INVALID_HANDLE_VALUE) {
        do {
            // read all (real) files in current folder
            // , delete '!' read other 2 default folder . and ..
            if (!(fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)) {
				std::wstring ws = std::wstring(fd.cFileName);
				names.emplace_back(std::string(ws.begin(), ws.end()));
            }
        } while (::FindNextFile(hFind, &fd));
        ::FindClose(hFind);
    }
#endif
    return names;
}

bool copy_file(const std::string& source, const std::string& destination)
{
    std::ifstream src(source, std::ios::binary);
    std::ofstream dest(destination, std::ios::binary);
    dest << src.rdbuf();
    return src && dest;
}

std::set<std::string> get_options_from_library(const std::string& cmod, const std::string& defaults_dir) {
    std::string cmod_symbol = format_as_symbol(cmod);
    std::string library_dir = std::string(getenv("SAMNTDIR")) + "/deploy/libraries/" + cmod_symbol;

    std::vector<std::string> filenames = get_files_in_directory(library_dir);

    std::set<std::string> config_names;
    for (auto &i : filenames) {
        if (i.find(cmod_symbol) == std::string::npos)
            continue;

        copy_file(library_dir + "/" + i, defaults_dir + "/" + i);

        size_t pos = i.find('_');

        config_names.insert(i.substr(pos + 1, i.find(".json") - pos - 1));
    }
    return config_names;
}
