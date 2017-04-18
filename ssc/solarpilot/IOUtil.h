#ifndef _IOUTIL_
#define _IOUTIL_ 1

#include <string>
#include "interop.h"
#include "definitions.h"

namespace ioutil
{
	/*This namespace contains all of the required utility functions for IO operations.
	-> File access
	-> File read/write
	*/
	
	//--File directory and name functions--
	bool file_exists( const char *file );
	bool dir_exists( const char *path );
	bool remove_file( const char *path );
	bool mkdir( const char *path, bool make_full = false); 
	std::string path_only( const std::string &path );
	std::string name_only( const std::string &path );
	std::string ext_only( const std::string &path );
	char path_separator();
	std::string get_cwd();
	bool set_cwd( const std::string &path );
	//--

	//--File reading functions--
	void read_chars( FILE *fp, std::string &text, int nchars=256);
	bool read_line( FILE *fp, std::string &text, int prealloc = 256 );
	void read_file( const string &fname, string &file, string &eol_marker);
	//void parseInputFile(const string &fname, var_map &V, var_map &Defs);	//return a structure with a map of variable names and information
    void parseXMLInputFile(const string &fname,var_map &V, parametric &par_data, optimization &opt_data);
	//void parseDefinitionArray(var_map &V, string disabled_mods = ""); //no longer needed
	bool saveXMLInputFile(const string &fname, var_map &V, parametric &par_data, optimization &opt_data, const string &version);
	string getDelimiter(std::string &text);	//Return the delimiter separating the text
	//--
};

#endif