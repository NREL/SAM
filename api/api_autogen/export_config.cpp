#include <iostream>
#include <sstream>
#include <fstream>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <set>

#include <ssc/sscapi.h>
#include <shared/lib_util.h>

#include "startup_extractor.h"
#include "ui_form_extractor.h"
#include "equation_extractor.h"
#include "config_extractor.h"
#include "builder_generator.h"
#include "data_structures.h"
#include "export_config.h"

#include "test.h"

#if defined(_WIN32)
#include <windows.h>
#include <conio.h>


#include <locale>
#include <codecvt>
#include <tchar.h>
#include <shellapi.h>

bool DeleteDirectory(LPCTSTR lpszDir, bool noRecycleBin = true)
{
    int len = _tcslen(lpszDir);
    TCHAR *pszFrom = new TCHAR[len + 2];
    _tcscpy(pszFrom, lpszDir);
    pszFrom[len] = 0;
    pszFrom[len + 1] = 0;

    SHFILEOPSTRUCT fileop;
    fileop.hwnd = NULL;    // no status display
    fileop.wFunc = FO_DELETE;  // delete operation
    fileop.pFrom = pszFrom;  // source file name as double null terminated string
    fileop.pTo = NULL;    // no destination needed
    fileop.fFlags = FOF_NOCONFIRMATION | FOF_SILENT;  // do not prompt the user

    if (!noRecycleBin)
        fileop.fFlags |= FOF_ALLOWUNDO;

    fileop.fAnyOperationsAborted = FALSE;
    fileop.lpszProgressTitle = NULL;
    fileop.hNameMappings = NULL;

    int ret = SHFileOperation(&fileop);
    delete[] pszFrom;
    return (ret == 0);
}
#endif


std::unordered_map<std::string, std::vector<std::string>> SAM_cmod_to_inputs;
std::string active_config;

void create_empty_subdirectories(std::string dir, std::vector<std::string> folders) {
    mode_t nMode = 0733; // UNIX style permissions
    // create directory first if it doesn't exist
    int nError = 0;
    struct stat info;
    if (stat(dir.c_str(), &info) != 0) {
#if defined(_WIN32)
        nError = _mkdir(dir.c_str());
#else
        nError = mkdir(dir.c_str(), nMode);
#endif
        if (nError != 0) {
            throw std::runtime_error("Couldn't create directory: " + dir);
        }
    }
    for (auto &name : folders) {
        std::string sPath = dir + '/' + name;

        // check if directory already exists
        if (stat(sPath.c_str(), &info) == 0) {
#if defined(_WIN32)
            std::wstring_convert<std::codecvt_utf8_utf16<wchar_t>> converter;
            std::wstring wide = converter.from_bytes(sPath);
            DeleteDirectory(wide.c_str());
#else
            system(std::string("rm -rf " + sPath).c_str());
#endif
        }

#if defined(_WIN32)
        nError = _mkdir(sPath.c_str());
#else
        nError = mkdir(sPath.c_str(), nMode);
#endif
        if (nError != 0) {
            throw std::runtime_error("Couldn't create subdirectory: " + sPath);
        }

    }
}

int main(int argc, char *argv[]) {

    // set default input & output file paths
    char *pPath;
    pPath = getenv("SAMNTDIR");
    std::string sam_path = std::string(pPath);

    std::string startup_file = sam_path + "/deploy/runtime/startup.lk";
    std::string runtime_path = sam_path + "/deploy/runtime";
    std::string graph_path = sam_path + "/api/api_autogen/Graphs/Files";
    std::string api_path = sam_path + "/api/api_autogen/library/C";
    std::string defaults_path = sam_path + "/api/api_autogen/library/defaults";
    std::string pysam_path = sam_path + "/api/api_autogen/library/PySAM";

    // replace file paths with command line arguments
    for (int i = 1; i < argc; i += 2) {
        if (std::strcmp(argv[i], "--startup") == 0) {
            startup_file = argv[i + 1];
        }
        if (std::strcmp(argv[i], "--runtime") == 0) {
            runtime_path = argv[i + 1];
        }
        if (std::strcmp(argv[i], "--graph") == 0) {
            graph_path = argv[i + 1];
        }
        if (std::strcmp(argv[i], "--api") == 0) {
            api_path = argv[i + 1];
        }
        if (std::strcmp(argv[i], "--defaults") == 0) {
            defaults_path = argv[i + 1];
        }
        if (std::strcmp(argv[i], "--pysam") == 0) {
            pysam_path = argv[i + 1];
        }
    }

    create_empty_subdirectories(pysam_path, std::vector<std::string>({"modules", "include"}));
    create_empty_subdirectories(pysam_path + "/docs", std::vector<std::string>({"include", "modules"}));
    create_empty_subdirectories(api_path, std::vector<std::string>({"include", "modules"}));

    std::cout << "Exporting C API files to " << api_path << "\n";
    std::cout << "Exporting default JSON files to " << defaults_path << "\n";
    std::cout << "Exporting PySAM files to " << pysam_path << "\n";

    // from startup script, load file and extract information for each config
    std::cout << "Reading startup script...\n";
    std::ifstream ifs(startup_file.c_str());
    if (!ifs.is_open()) {
        std::cout << "Cannot open startup file at " << startup_file << "\n";
        return 1;
    }

    std::string content = static_cast<std::stringstream const &>(std::stringstream() << ifs.rdbuf()).str();

    startup_extractor su_e;
    su_e.load_startup_script(content);
    std::vector<std::string> unique_ui_form_names = su_e.get_unique_ui_forms();

    // get all the SSC_INPUT & SSC_INOUT for all used compute_modules
    load_primary_cmod_inputs();


    // from each ui_form file, extract the config-independent defaults, equations and callback scripts
    std::cout << "Reading ui forms and defaults...\n";

    SAM_ui_extracted_db.populate_ui_data(runtime_path + "/ui/", unique_ui_form_names);

    // keep track of all compute_modules that have been made into PySAM modules
    std::set<std::string> processed_cmods;

    // parsing the callbacks requires all ui forms in a config
    active_config = "";

    // do technology configs with None first
    for (auto it = SAM_config_to_primary_modules.begin(); it != SAM_config_to_primary_modules.end(); ++it) {
        active_config = it->first;

        if (active_config.find("None") == std::string::npos && active_config != "MSPT-Single Owner"
            && active_config != "DSPT-Single Owner") {
            continue;
        }

        // no defaults
        if (active_config.find("Independent Power Producer") != std::string::npos
            || active_config.find("Commercial PPA") != std::string::npos) {
            continue;
        }

        std::vector<std::string> primary_cmods = SAM_config_to_primary_modules[active_config];

        config_extractor ce(it->first, runtime_path + "/defaults/");
//        ce.map_equations();
//        ce.register_callback_functions();
        //        std::cout << "\n\n\n\n";
//        SAM_config_to_variable_graph[active_config]->print_dot(graph_path);

        // modules and modules_order will need to be reset per cmod
        for (size_t i = 0; i < primary_cmods.size(); i++) {
            // get all the expressions
            std::cout << "Exporting for " << it->first << ": " << primary_cmods[i] << "... ";

            builder_generator b_gen(&ce);
            b_gen.create_all(primary_cmods[i], defaults_path, api_path, pysam_path);
            //b_gen.print_subgraphs(graph_path);
            processed_cmods.insert(primary_cmods[i]);
        }
        SAM_config_to_variable_graph.erase(active_config);

    }
    // do all configs
    for (auto it = SAM_config_to_primary_modules.begin(); it != SAM_config_to_primary_modules.end(); ++it) {
        active_config = it->first;

        // no defaults
        if (active_config.find("Independent Power Producer") != std::string::npos
            || active_config.find("Commercial PPA") != std::string::npos) {
            continue;
        }


        std::vector<std::string> primary_cmods = SAM_config_to_primary_modules[active_config];

        config_extractor ce(it->first, runtime_path + "/defaults/");
//        ce.map_equations();
//        ce.register_callback_functions();
//        SAM_config_to_variable_graph[active_config]->print_dot(graph_path);

        // modules and modules_order will need to be reset per cmod
        for (size_t i = 0; i < primary_cmods.size(); i++) {
            std::cout << "Exporting for " << it->first << ": " << primary_cmods[i] << "... ";
            // get all the expressions
            builder_generator b_gen(&ce);
            b_gen.create_all(primary_cmods[i], defaults_path, api_path, pysam_path);
            //b_gen.print_subgraphs(graph_path);
            processed_cmods.insert(util::lower_case(primary_cmods[i]));
        }
        SAM_config_to_variable_graph.erase(active_config);

    }

    // produce remaining compute_modules
    std::cout << "Remaining cmods: \n";
    active_config = "";
    int i = 0;
    ssc_entry_t p_entry = ssc_module_entry(i);
    while (p_entry) {
        const char *name = ssc_entry_name(p_entry);
        const char *desc = ssc_entry_description(p_entry);
        p_entry = ssc_module_entry(++i);

        if (processed_cmods.count(util::lower_case(name)) != 0)
            continue;

        builder_generator cm_bg;
        cm_bg.config_name = name;
        cm_bg.create_all(name, defaults_path, api_path, pysam_path, false);

        // print for pasting into pysam/docs/Models.rst
        std::cout << "\t* - :doc:`modules/" << format_as_symbol(name) << "`\n";
        std::cout << "\t* - " << desc << "\n";

    }

    std::cout << "Complete... Exiting\n";

    return 0;


}

