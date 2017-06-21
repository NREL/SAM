#include <stdio.h>
#include <string>
#include <vector>
#include "tcstype.h"

class stdfile
{
public:
	stdfile() : p(0) {  }
	stdfile(const char *file, const char *mode) { p = fopen(file, mode); }
	stdfile(const std::string &file, const char *mode) { p = fopen(file.c_str(), mode); }
//	stdfile(const wxString &file, const char *mode) { p = fopen((const char*)file.c_str(), mode); }
	~stdfile() { close(); }
	bool open(const char *file, const char *mode) { close(); p = fopen(file, mode); return ok(); }
	bool open(const std::string &file, const char *mode) { return open(file.c_str(), mode); }
	bool ok() { return 0 != p; }
	operator FILE*() const { return p; }
	void close() { if (p) ::fclose(p); p = 0; }
private:
	FILE *p;
};


static void debug_log_init_call(const char* unit_name, tcstypeinfo *type, std::vector<tcsvalue> &values)
{
	std::string fn ("C:\\Projects\\SAM\\Documentation\\CSP\\Linear Fresnel\\Molten Salt\\tcsdbg.txt");
	stdfile dbgout(fn, "a");

	fprintf(dbgout, "\n\n%s\n", unit_name);

	for (int i = 0; i < values.size(); i++)
	{
		fprintf(dbgout, "%s = ", type->variables[i].name);
		switch (values[i].type)
		{
		case TCS_NUMBER:
			fprintf(dbgout, "%lg\n", values[i].data.value);
			break;
		case TCS_ARRAY:
			fprintf(dbgout, "[ ");
			for (int j = 0; j < values[i].data.array.length-1; j++)
				fprintf(dbgout, "%lg, ", values[i].data.array.values[j]);
			fprintf(dbgout, "%lg ]\n", values[i].data.array.values[values[i].data.array.length - 1]);
			break;
		case TCS_MATRIX:
			fprintf(dbgout, "[\n[");
			for (int nr = 0; nr < values[i].data.matrix.nrows - 1; nr++)
			{
				for (int nc = 0; nc < values[i].data.matrix.ncols - 1; nc++)
					fprintf(dbgout, "%lg, ", values[i].data.matrix.values[nc + nr*values[i].data.matrix.ncols]);
				fprintf(dbgout, "%lg ],\n[", values[i].data.matrix.values[values[i].data.matrix.ncols - 1 + nr*values[i].data.matrix.ncols]);
			}
			for (int nc = 0; nc < values[i].data.matrix.ncols - 1; nc++)
				fprintf(dbgout, "%lg, ", values[i].data.matrix.values[nc + (values[i].data.matrix.nrows - 1 )* values[i].data.matrix.ncols]);
			fprintf(dbgout, "%lg ]\n]\n", values[i].data.matrix.values[values[i].data.matrix.ncols - 1 + (values[i].data.matrix.nrows - 1)*values[i].data.matrix.ncols]);
			break;
		case TCS_STRING:
			fprintf(dbgout, "%s\n", values[i].data.cstr);
			break;
		}
	}

	dbgout.close();
}
