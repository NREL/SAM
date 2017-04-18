#include "sscapi.h"
#include <stdio.h>

int main()
{
	int version = ssc_version();
	const char *build = ssc_build_info();

	printf("ssc version %d: %s\n", version, build);
	return 0;
}
