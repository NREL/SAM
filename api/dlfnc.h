// reference https ://stackoverflow.com/questions/53530566/loading-dll-in-windows-c-for-cross-platform-design
#ifndef DLFCN_H
#define DLFCN_H

#define RTLD_GLOBAL 0x100 /* do not hide entries in this module */
#define RTLD_LOCAL  0x000 /* hide entries in this module */

#define RTLD_LAZY   0x000 /* accept unresolved externs */
#define RTLD_NOW    0x001 /* abort if module has unresolved externs */

/*
   How to call in Windows:

   void *h = dlopen ("path\\library.dll", flags)
   void (*fun)() = dlsym (h, "entry")
*/

#ifdef __cplusplus
extern "C" {
#endif

	void *dlopen(const char *filename, int flag);
	int   dlclose(void *handle);

	void *dlsym(void *handle, const char *name);

	const char *dlerror(void);

#ifdef __cplusplus
}
#endif

#endif