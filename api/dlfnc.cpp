#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <windows.h>

static struct {
	long lasterror;
	const char *err_rutin;
} var = {
	0,
	NULL
};

void *dlopen(const char *filename, int flags)
{
	HINSTANCE hInst;

	hInst = LoadLibrary(filename);
	if (hInst == NULL) {
		var.lasterror = GetLastError();
		var.err_rutin = "dlopen";
	}
	return hInst;
}

int dlclose(void *handle)
{
	BOOL ok;
	int rc = 0;

	ok = FreeLibrary((HINSTANCE)handle);
	if (!ok) {
		var.lasterror = GetLastError();
		var.err_rutin = "dlclose";
		rc = -1;
	}
	return rc;
}

void *dlsym(void *handle, const char *name)
{
	FARPROC fp;

	fp = GetProcAddress((HINSTANCE)handle, name);
	if (!fp) {
		var.lasterror = GetLastError();
		var.err_rutin = "dlsym";
	}
	return (void *)(intptr_t)fp;
}

const char *dlerror(void)
{
	static char errstr[88];

	if (var.lasterror) {
		sprintf(errstr, "%s error #%ld", var.err_rutin, var.lasterror);
		return errstr;
	}
	else {
		return NULL;
	}
}