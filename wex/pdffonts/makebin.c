#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[])
{
	char name[256];
	char buf[256];
	char *p;
	
	strcpy( buf, argv[1] );
	
	p = buf + strlen(buf) - 1;
	while ( p > buf && *p != '.' )
		*p-- = 0;
	
	
	if ( p == buf ) return;
	
	*p = 0;
	
	strcpy( name, buf );	
	strcat( buf, ".c" );
		
	FILE *fp = fopen( argv[1], "rb" );
	FILE *out = fopen( buf, "w" );


    fprintf(out,"char %s[] = {\n", name);
    unsigned long n = 0;
    unsigned char c;
	fread(&c, 1, 1, fp);
    while(!feof(fp)) {
		
        fprintf(out,"0x%.2X", (int)c);
        ++n;
		
        if(fread(&c, 1, 1, fp) == 0) break;
		
		fprintf(out, "," );
        if(n % 10 == 0) fprintf(out,"\n");
    }
    fprintf(out,"};\n");
	
	printf( "wrote %d bytes to: %s\n", n, buf );
	
	fprintf(out,"static const int %s_len = %d;\n", name, n );
	
	return 0;
}