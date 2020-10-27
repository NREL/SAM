#include <utility>
#include <fstream>

#if defined(_WIN32)
#include <Windows.h>
#endif

#include "library_extractor.h"
#include "lk_eval.h"

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
    WIN32_FIND_DATA fd;
    HANDLE hFind = ::FindFirstFile(search_path.c_str(), &fd);
    if (hFind != INVALID_HANDLE_VALUE) {
        do {
            // read all (real) files in current folder
            // , delete '!' read other 2 default folder . and ..
            if (!(fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)) {
                names.push_back(fd.cFileName);
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
