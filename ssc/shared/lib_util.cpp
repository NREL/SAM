#include <cstdio>
#include <cstdarg>
#include <cstring>
#include <cstdlib>
#include <limits>
#include <numeric>

#ifdef _WIN32
#include <direct.h>
#include <Windows.h>
#else
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#endif

#include "lib_util.h"

#include <cmath>
#ifdef _MSC_VER
/* taken from wxMSW-2.9.1/include/wx/defs.h - appropriate for Win32/Win64 */
#define va_copy(d, s) ((d)=(s))
#endif

std::vector< std::string > util::split( const std::string &str, const std::string &delim, bool ret_empty, bool ret_delim )
{
	std::vector< std::string > list;

	char cur_delim[2] = {0,0};
	std::string::size_type m_pos = 0;
	std::string token;
	
	while (m_pos < str.length())
	{
		std::string::size_type pos = str.find_first_of(delim, m_pos);
		if (pos == std::string::npos)
		{
			cur_delim[0] = 0;
			token.assign(str, m_pos, std::string::npos);
			m_pos = str.length();
		}
		else
		{
			cur_delim[0] = str[pos];
			std::string::size_type len = pos - m_pos;			
			token.assign(str, m_pos, len);
			m_pos = pos + 1;
		}
		
		if (token.empty() && !ret_empty)
			continue;

		list.push_back( token );
		
		if ( ret_delim && cur_delim[0] != 0 && m_pos < str.length() )
			list.push_back( std::string( cur_delim ) );
	}
	
	return list;
}

std::string util::join( const std::vector< std::string > &list, const std::string &delim )
{
	std::string str;
	for (std::vector<std::string>::size_type i=0;i<list.size();i++)
	{
		str += list[i];
		if (i < list.size()-1)
			str += delim;
	}
	return str;		
}

size_t util::replace( std::string &s, const std::string &old_text, const std::string &new_text)
{
	const size_t uiOldLen = old_text.length();
	const size_t uiNewLen = new_text.length();

	std::string::size_type pos = 0;
	size_t uiCount = 0;
	while(1)
	{
		pos = s.find(old_text, pos);
		if ( pos == std::string::npos )
			break;

		// replace this occurrence of the old string with the new one
		s.replace(pos, uiOldLen, new_text.c_str(), uiNewLen);

		// move past the string that was replaced
		pos += uiNewLen;

		// increase replace count
		uiCount++;
	}

	return uiCount;
}


bool util::to_integer(const std::string &str, int *x)
{
	const char *startp = str.c_str();
	char *endp = NULL;
	*x = ::strtol( startp, &endp, 10 );	
	return !*endp && (endp!=startp);
}

bool util::to_float(const std::string &str, float *x)
{
	double val;
	bool ok = to_double(str, &val);
	*x = (float) val;
	return ok;
}

bool util::to_double(const std::string &str, double *x)
{
	const char *startp = str.c_str();
	char *endp = NULL;
	*x = ::strtod( startp, &endp );	
	return !*endp && (endp!=startp);
}

std::string util::to_string( int x, const char *fmt )
{
	char buf[64];
	sprintf(buf, fmt, x);
	return std::string(buf);
}

std::string util::to_string( double x, const char *fmt )
{
	char buf[256];
	sprintf(buf, fmt, x);
	return std::string(buf);
}

std::string util::lower_case( const std::string &in )
{
	std::string ret(in);
	for (std::string::size_type i=0;i<ret.length();i++)
		ret[i] = (char)tolower(ret[i]);
	return ret;
}

std::string util::upper_case( const std::string &in )
{
	std::string ret(in);
	for (std::string::size_type i=0;i<ret.length();i++)
		ret[i] = (char)toupper(ret[i]);
	return ret;
}

bool util::file_exists( const char *file )
{
#ifdef _WIN32
	// from wxWidgets: must use GetFileAttributes instead of ansi C 
	// b/c can cope with network (unc) paths
	DWORD ret = ::GetFileAttributesA( file );
	return (ret != (DWORD)-1) && !(ret & FILE_ATTRIBUTE_DIRECTORY);
#else
	struct stat st;
	return stat(file, &st) == 0 && S_ISREG(st.st_mode);
#endif
}

bool util::dir_exists( const char *path )
{
#ifdef _WIN32
	// Windows fails to find directory named "c:\dir\" even if "c:\dir" exists,
	// so remove all trailing backslashes from the path - but don't do this for
	// the paths "d:\" (which are different from "d:") nor for just "\"
	char *wpath = strdup( path );
	if (!wpath) return false;

	int pos = strlen(wpath)-1;
	while (pos > 1 && (wpath[pos] == '/' || wpath[pos] == '\\'))
	{
		if (pos == 3 && wpath[pos-1] == ':') break;

		wpath[pos] = 0;
		pos--;
	}

	DWORD ret = ::GetFileAttributesA(wpath);
    bool exists =  (ret != (DWORD)-1) && (ret & FILE_ATTRIBUTE_DIRECTORY);

	free( wpath );

	return exists;
#else
	struct stat st;
	return ::stat(path, &st) == 0 && S_ISDIR(st.st_mode);
#endif
}

bool util::remove_file( const char *path )
{
	return 0 == ::remove( path );
}

#ifdef _WIN32
#define make_dir(x) ::mkdir(x)
#else
#define make_dir(x) ::mkdir(x, 0777)
#endif

bool util::mkdir( const char *path, bool make_full )
{
	if (make_full)
	{
		std::vector<std::string> parts = split( path, "/\\" );
	
		if (parts.size() < 1) return false;
		
		std::string cur_path = parts[0] + path_separator();
		
		for (size_t i=1;i<parts.size();i++)
		{
			cur_path += parts[i];

			if ( !dir_exists(cur_path.c_str()) )
				if (0 != make_dir( cur_path.c_str() ) ) return false;
						
			cur_path += path_separator();
		}

		return true;
	}
	else
		return 0 == make_dir( path );
}

std::string util::path_only( const std::string &path )
{
	std::string::size_type pos = path.find_last_of("/\\");
	if (pos==std::string::npos) return path;
	else return path.substr(0, pos);
}

std::string util::name_only( const std::string &path )
{
	std::string::size_type pos = path.find_last_of("/\\");
	if (pos==std::string::npos) return path;
	else return path.substr(pos+1);
}

std::string util::ext_only( const std::string &path )
{
	std::string::size_type pos = path.find_last_of('.');
	if (pos==std::string::npos) return path;
	else return path.substr(pos+1);
}
	
char util::path_separator()
{
#ifdef _WIN32
	return '\\';
#else
	return '/';
#endif
}

std::string util::get_cwd()
{
	char buf[2048];
#ifdef _WIN32
	::GetCurrentDirectoryA( 2047, buf );
#else
	::getcwd(buf, 2047);
#endif
	buf[2047] = 0;
	return std::string(buf);
}

bool util::set_cwd( const std::string &path )
{
#ifdef _WIN32
	return ::SetCurrentDirectoryA( path.c_str() ) != 0;
#else
	return ::chdir( path.c_str() ) == 0;
#endif
}

std::string util::read_file( const std::string &file )
{
	std::string buf;
	char c;
	FILE *fp = fopen(file.c_str(), "r");
	if (fp)
	{
		while ( (c=fgetc(fp))!=EOF )
			buf += c;
		fclose(fp);
	}
	return buf;
}

bool util::read_line( FILE *fp, std::string &buf, int prealloc )
{
	int c;

	buf = "";
	if (prealloc > 10)
		buf.reserve( (size_t)prealloc );

	// read the whole line, 1 character at a time, no concern about buffer length
	while ( (c=fgetc(fp)) != EOF && c != '\n' && c != '\r')
		buf += (char)c;

	// handle windows <CR><LF>
	if (c == '\r')
	{
		if ( (c=fgetc(fp)) != '\n')
			ungetc(c,fp);
	}

	// handle a stray <CR>
	if (c == '\n')
	{
		if ( (c=fgetc(fp)) != '\r')
			ungetc(c,fp);
	}

	return !(buf.length() == 0 && c == EOF);
}


#ifdef _WIN32

int util::sync_piped_process::spawn(const std::string &command, const std::string &workdir)
{
	int result = 0;

	std::string lastwd;
	if ( !workdir.empty() )
	{
		lastwd = util::get_cwd();
		util::set_cwd( workdir );
	}

	SECURITY_ATTRIBUTES sa;
	sa.nLength= sizeof(SECURITY_ATTRIBUTES);
	sa.lpSecurityDescriptor = NULL;
	sa.bInheritHandle = TRUE;


	HANDLE hStdoutReadEnd, hStdoutWriteEnd;

	hStdoutReadEnd = hStdoutWriteEnd = INVALID_HANDLE_VALUE;
	

	if (!CreatePipe( &hStdoutReadEnd, &hStdoutWriteEnd, &sa, 0 ))
		return -90;
	
	// prep and launch redirected child here
	PROCESS_INFORMATION pi;
	STARTUPINFOA si;

	ZeroMemory(&pi, sizeof(PROCESS_INFORMATION));
	pi.hProcess = INVALID_HANDLE_VALUE;
	pi.hThread = INVALID_HANDLE_VALUE;

	// Set up the start up info struct.
	ZeroMemory(&si,sizeof(STARTUPINFO));
	si.cb = sizeof(STARTUPINFO);
	si.dwFlags = STARTF_USESTDHANDLES;
	si.hStdOutput = hStdoutWriteEnd;

	// Use this if you want to hide the child:
	//     si.wShowWindow = SW_HIDE;
	// Note that dwFlags must include STARTF_USESHOWWINDOW if you want to
	// use the wShowWindow flags.


	// Launch the process that you want to redirect (in this case,
	// Child.exe). Make sure Child.exe is in the same directory as
	// redirect.c launch redirect from a command line to prevent location
	// confusion.
	if (result == 0 && !CreateProcessA(NULL,(char*)command.c_str(),NULL,NULL,TRUE,
					 //CREATE_NEW_CONSOLE|CREATE_NO_WINDOW|NORMAL_PRIORITY_CLASS
					 //CREATE_NEW_CONSOLE
					 CREATE_NO_WINDOW,
					 NULL,
					 NULL, /*workdir.IsEmpty()?NULL:(char*)workdir.c_str(),*/
					 &si,&pi))
	{
		result = -99;
	}

	// read childs output

	CHAR lpBuffer[256];
	DWORD nBytesRead;
//	DWORD nCharsWritten;

	std::string line;
	while (  WaitForSingleObject( pi.hProcess, 1 ) == WAIT_TIMEOUT
		&& hStdoutReadEnd != INVALID_HANDLE_VALUE )
	{
		line = "";
		// read a text line from the output
		while( result == 0 && hStdoutReadEnd != INVALID_HANDLE_VALUE)
		{
			// wait for something to appear
			DWORD navail = 0, rc;
			int npeek = 0;
			while( hStdoutReadEnd != INVALID_HANDLE_VALUE )
			{
				rc = PeekNamedPipe( hStdoutReadEnd, NULL, 0, NULL, &navail, NULL );
				if (!rc)
				{
					CloseHandle( hStdoutReadEnd );
					hStdoutReadEnd = INVALID_HANDLE_VALUE;
					result = -97;
					break;
				}

				if (navail > 0)
					break;

				// make sure somehow the process didn't end a while ago
				// and we're still in this loop for some reason
				if (WaitForSingleObject( pi.hProcess, 1 ) != WAIT_TIMEOUT 
					|| npeek++ > 500 )
				{
					CloseHandle( hStdoutReadEnd );
					hStdoutReadEnd = INVALID_HANDLE_VALUE;
					break;
				}
				
				::Sleep( 5 );
			}
		
			if ( hStdoutReadEnd == INVALID_HANDLE_VALUE
				|| !ReadFile(hStdoutReadEnd,lpBuffer, 1, &nBytesRead,NULL)
				|| nBytesRead == 0)
			{
					CloseHandle( hStdoutReadEnd );
					hStdoutReadEnd = INVALID_HANDLE_VALUE;
					break; // pipe done 
			}
			
			if (lpBuffer[0] != 0 && lpBuffer[0] != '\r' && lpBuffer[0] != '\n')
				line += lpBuffer[0];

			if (lpBuffer[0] == '\n' || lpBuffer[0] == 0)
				break; // line finished
		}
		
		on_stdout( line );

	}

	// make sure process ended
	if (pi.hProcess!=INVALID_HANDLE_VALUE)
		WaitForSingleObject( pi.hProcess, INFINITE );
			
	DWORD exitcode = 0;
	GetExitCodeProcess(pi.hProcess, &exitcode);
	if (result >= 0) 
		result = exitcode;

	if (pi.hProcess!=INVALID_HANDLE_VALUE) CloseHandle( pi.hProcess );
	if (pi.hThread!=INVALID_HANDLE_VALUE) CloseHandle( pi.hThread );

	if (hStdoutReadEnd!=INVALID_HANDLE_VALUE) CloseHandle( hStdoutReadEnd );
	
	if ( !lastwd.empty() )
		util::set_cwd( lastwd );

	return result;
}
#else
int util::sync_piped_process::spawn(const std::string &command, const std::string &workdir)
{
	std::string line;

	std::string lastwd;
	if ( !workdir.empty() )
	{
		lastwd = util::get_cwd();
		util::set_cwd( workdir );
	}

	FILE *fp = popen( command.c_str(), "r" );
	if (!fp)
		return -99;

	while ( util::read_line(fp, line) )
		on_stdout(line);

	if ( !lastwd.empty() )
		util::set_cwd( lastwd );

	return pclose( fp );
}

#endif

std::string util::format(const char *fmt, ...)
{
	if (!fmt || *fmt == 0) return "";

	va_list arglist;
	va_start( arglist, fmt );

	size_t ret = 0;

	int size = 512;
	char *buffer = new char[size];
	if (!buffer)
		return "";

	do
	{
		va_list argptr_copy;
		va_copy( argptr_copy, arglist );
		ret = util::format_vn(buffer,size-1,fmt,argptr_copy);
		va_end( argptr_copy );

		if (ret == 0)
		{
			delete [] buffer;
			size *= 2;
			buffer = new char[size];
			if (!buffer)
				return "";
		}
		
	}
	while (ret < 0);
	
	va_end(arglist);

	std::string s(buffer);
	if (buffer)
		delete [] buffer;
		
	return s;
}

size_t util::format_vn(char *buffer, int maxlen, const char *fmt, va_list arglist)
{
	char *p = (char*)fmt, *bp = buffer, *tp;
	char *bpmax = buffer+maxlen-1;
	int i;
	
	char arg_char;
	char *arg_str;
	int arg_int;
	unsigned int arg_uint;
	double arg_double;
	
#define TEMPLEN 256
	char temp[TEMPLEN];
	char tempfmt[TEMPLEN];
	char *decpt;
	size_t ndigit;
	int with_precision;
	char *with_comma;
	char prev;
	
	if (!p)
	{
		*bp = 0;
		return 0;
	}
	
	while( *p && bp<bpmax )
	{
		if (*p != '%')	*bp++ = *p++;
		else
		{
			p++;			
			switch (*p)
			{
			case 'd':
			case 'D':
			/* handle simple signed integer format */
				p++;
				arg_int = va_arg(arglist, int);
				sprintf(temp, "%d", arg_int);
				tp = temp;
				while (*tp && bp<bpmax)
					*bp++ = *tp++;					
				break;					
			
			case 'u':
			case 'U':
			/* handle simple unsigned integer format */
				p++;
				arg_uint = va_arg(arglist, unsigned int);
				sprintf(temp, "%u", arg_uint);
				tp = temp;
				while (*tp && bp<bpmax)
					*bp++ = *tp++;	
				break;
				
			case 'x':
			case 'X':
			/* handle hexadecimal unsigned integer format */
				p++;
				arg_uint = va_arg(arglist, unsigned int);
				sprintf(temp, "%x", arg_uint);
				tp = temp;
				while (*tp && bp<bpmax)
					*bp++ = *tp++;	
				break;
			
			case 'c':
			case 'C':
			/* handle simple char format */
				arg_char = (char)va_arg(arglist, int);
				if ( bp+1<bpmax ) *bp++ = arg_char;
				p++;
				break;
			
			case 's':
			case 'S':
			/* handle simple string format */
				p++;
				arg_str = va_arg(arglist, char*);
				tp = arg_str;
				while (*tp && bp<bpmax)
					*bp++ = *tp++;
				break;
			
			case '%':
				if (bp+1<bpmax)	*bp++ = *p++;
				break;
								
			
			case 'l':
			case 'L':
			case 'f':
			case 'F':
			case 'g':
			case 'G':
			case '.':
				with_precision = 0;
				with_comma = 0;
				tp = tempfmt;			
				*tp++ = '%';
				if (*p == '.')
				{ /* accumulate the precision */	
					with_precision = 1;
					*tp++ = *p++;
					if (*p == '0') with_precision = 2;
					while ( *p && isdigit(*p) )
						*tp++ = *p++;
				}
				*tp++ = 'l';
				if (*p == 'l' || *p == 'L')	p++;// skip lL
				if (*p == ',') // comma separated
				{
					*tp++ = 'f'; p++;
					with_comma = (char*)1;
				}
				else // fFgG
					*tp++ = *p++;

				*tp = '\0'; // end format string
				arg_double = va_arg(arglist, double);
				
				sprintf(temp, tempfmt, (double)arg_double);
				
				i=0;
				if (with_comma)
				{
					decpt = strchr(temp, '.');
					if (!decpt) ndigit = strlen(temp);
					else ndigit = (int)(decpt-temp);
					i=0-ndigit%3;
				}

				if ((!with_precision || with_comma!=NULL) && 
					!strchr(tempfmt,'g') &&
					!strchr(tempfmt,'G') &&
					(!(with_precision == 2)) )
				{
					tp = temp+strlen(temp)-1;
					while (tp > temp && *tp == '0')
						*tp-- = 0;
					if (*tp == '.')
						*tp-- = 0;					
				}
				
				tp = temp; decpt = 0; prev = 0;
				while (*tp && bp<bpmax)
				{
					if (*tp == '.') decpt = (char*)1;
					if (with_comma != NULL && isdigit(prev) && i%3==0 && !decpt && bp<bpmax) *bp++ = ',';
					prev = *tp;
					if (bp<bpmax) *bp++ = *tp++;
					i++;
				}
				
				break;			
			
			/* handle comma or money format (double precision) */
			case 'm':
			case 'M':
			case ',':
				arg_double = va_arg(arglist, double);
				if (*p == ',')
				{
					sprintf(temp, "%lf", arg_double);
					if (strchr(temp,'e')!=NULL) sprintf(temp, "%d", (int)arg_double);
				}
				else sprintf(temp, "%.2lf",  arg_double);
				
				decpt = strchr(temp, '.');
				if (!decpt) ndigit = strlen(temp);
				else ndigit = (int)(decpt-temp);	
							
				if (*p == ',')
				{
					tp = temp+strlen(temp)-1;
					while (tp > temp && *tp == '0') *tp-- = 0;
						
					if (*tp == '.') *tp-- = 0;
				}					
				
				i=0-(ndigit%3); tp = temp; decpt = 0; prev = 0;
				while (*tp)
				{
					if (*tp == '.')	decpt = (char*)1;
					if ( isdigit(prev) && i%3==0 && !decpt && bp<bpmax) *bp++ = ',';	
					prev = *tp;
					if (bp<bpmax) *bp++ = *tp;
					tp++; i++;
				}
				p++;
				break;
			}
			
		}
		
	}

	*bp = 0;

#undef TEMPLEN

	if (bp==bpmax) return 0;
	else return (bp-buffer);
}

int util::hours_in_month(int month)
{	// month=1 for January, 12 for December
	return ( (month<1) || (month>12) ) ? 0 : nday[month-1]*24;
}

double util::percent_of_year(int month, int hours)
{	// month=1 for January, 12 for December
	if (month<1) return 0.0;
	if (month>12) return 1.0;

	int hours_from_months = 0;
	for (unsigned int i=0; i<month-1; i++)
		hours_from_months += (nday[i] * 24);
	return (hours_from_months + hours)/8760.0;
}

int util::month_of(double time)
{
	/* returns month number 1..12 given 
	   time: hour index in year 0..8759 */
	if (time < 0) return 0;
	if (time < 744) return 1;
	if (time < 1416) return 2;
	if (time < 2160) return 3;
	if (time < 2880) return 4;
	if (time < 3624) return 5;
	if (time < 4344) return 6;
	if (time < 5088) return 7;
	if (time < 5832) return 8;
	if (time < 6552) return 9;
	if (time < 7296) return 10;
	if (time < 8016) return 11;
	if (time < 8760) return 12;
	return 0;
}

int util::days_in_month(int month)
{
	std::vector<int> days_in_months = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
	return days_in_months[month];
}

int util::day_of_month(int month, double time)
{
	int daynum = ( ((int)(time/24.0)) + 1 );   // day goes 1-365
	switch(month)
	{
	case 1: return  daynum;
	case 2: return  daynum-31;
	case 3: return  daynum-31-28;
	case 4: return  daynum-31-28-31;
	case 5: return  daynum-31-28-31-30;
	case 6: return  daynum-31-28-31-30-31;
	case 7: return  daynum-31-28-31-30-31-30;
	case 8: return  daynum-31-28-31-30-31-30-31;
	case 9: return  daynum-31-28-31-30-31-30-31-31;
	case 10: return daynum-31-28-31-30-31-30-31-31-30;
	case 11: return daynum-31-28-31-30-31-30-31-31-30-31;
	case 12: return daynum-31-28-31-30-31-30-31-31-30-31-30; 
	default: break;
	}
	return daynum;
}

void util::month_hour(int hour_of_year, int & out_month, int & out_hour)
{
	int tmpSum = 0;
	int hour = 0;
	int month;

	for (month = 1; month <= 12; month++)
	{
		int hours_in_month = util::hours_in_month(month);
		tmpSum += hours_in_month;

		// found the month
		if (hour_of_year + 1 <= tmpSum)
		{
			// get the day of the month
			int tmp = floor((float)(hour_of_year) / 24);
			hour = (hour_of_year + 1) - (tmp * 24);
			break;
		}
	}
	out_month = month;
	out_hour = hour;
}

int util::hour_of_day(int hour_of_year)
{
	return (hour_of_year) % 24;
}

bool util::weekday(int hour_of_year)
{
	int day_of_year = floor((float)(hour_of_year) / 24);
	int day_of_week = day_of_year;

	if (day_of_week > 6)
		day_of_week = day_of_year % 7;

	if (day_of_week < 5)
		return true;
	else
		return false;
}

int util::schedule_char_to_int( char c )
{
	int ret = 0;
	switch (c)
	{
		case '1':
			ret = 1;
			break;
		case '2':
			ret = 2;
			break;
		case '3':
			ret = 3;
			break;
		case '4':
			ret = 4;
			break;
		case '5':
			ret = 5;
			break;
		case '6':
			ret = 6;
			break;
		case '7':
			ret = 7;
			break;
		case '8':
			ret = 8;
			break;
		case '9':
			ret = 9;
			break;
		case 'A':
		case 'a':
		case ':':
			ret = 10;
			break;
		case 'B':
		case 'b':
		case '=':
			ret = 11;
			break;
		case 'C':
		case 'c':
		case '<':
			ret = 12;
			break;
	}
	return ret;
}


std::string util::schedule_int_to_month( int m )
{
	std::string ret = "";
	switch (m)
	{
		case 0: ret = "jan"; break;
		case 1: ret = "feb"; break;
		case 2: ret = "mar"; break;
		case 3: ret = "apr"; break;
		case 4: ret = "may"; break;
		case 5: ret = "jun"; break;
		case 6: ret = "jul"; break;
		case 7: ret = "aug"; break;
		case 8: ret = "sep"; break;
		case 9: ret = "oct"; break;
		case 10: ret = "nov"; break;
		case 11: ret = "dec"; break;
	}
	return ret;
}

bool util::translate_schedule( int tod[8760], const char *wkday, const char *wkend, int min_val, int max_val)
{
	int i=0;
	if (!wkday || !wkend || strlen(wkday) != 288 || strlen(wkend) != 288)
	{
		for (i=0;i<8760;i++) tod[i] = min_val;
		return false;
	}

	int wday = 5;
	for (int m=0;m<12;m++)
	{
		for (int d=0;d<nday[m];d++)
		{
			const char *sptr = (wday<=0) ? wkend : wkday;

			if (wday >= 0) wday--;
			else wday = 5;

			for (int h=0;h<24;h++)
			{
				tod[i] = schedule_char_to_int(sptr[ m*24 + h ])-1;
				if (tod[i] < min_val) tod[i] = min_val;
				if (tod[i] > max_val) tod[i] = max_val;
				i++;
			}
		}
	}

	return true;
}


bool util::translate_schedule(int tod[8760], const matrix_t<float> &wkday, const matrix_t<float> &wkend, int min_val, int max_val)
{
	int i = 0;
	if ((wkday.nrows() != 12) || (wkend.nrows() != 12) || (wkday.ncols() != 24) || (wkend.ncols() != 24) )
	{
		for (i = 0; i<8760; i++) tod[i] = min_val;
		return false;
	}

	int wday = 5; // start on Monday
	bool is_weekday = true;
	for (int m = 0; m<12; m++)
	{
		for (int d = 0; d<nday[m]; d++)
		{
			is_weekday = (wday > 0);

			if (wday >= 0) wday--;
			else wday = 5;

			for (int h = 0; h<24; h++)
			{
				if (is_weekday)
					tod[i] = wkday.at(m, h);
				else
					tod[i] = wkend.at(m, h);

				if (tod[i] < min_val) tod[i] = min_val;
				if (tod[i] > max_val) tod[i] = max_val;
				i++;
			}
		}
	}

	return true;
}


double util::bilinear( double rowval, double colval, const matrix_t<double> &mat )
{
	if (mat.nrows() < 3 || mat.ncols() < 3)
		return std::numeric_limits<double>::quiet_NaN();
	
	int ridx=2; // find row position
	while( ridx < mat.nrows() && rowval > mat.at(ridx, 0) )
		ridx++;
	
	int cidx=2; // find col position
	while( cidx < mat.ncols() && colval > mat.at(0, cidx) )
		cidx++;

	if ( ridx == mat.nrows() ) ridx--;
	if ( cidx == mat.ncols() ) cidx--;

	double r1,c1,r2,c2;
	
	r1 = mat.at(ridx-1, 0);
	r2 = mat.at(ridx, 0);

	c1 = mat.at(0, cidx-1);
	c2 = mat.at(0, cidx);

	double denom = (r2-r1)*(c2-c1);

	return mat.at(ridx-1, cidx-1) * (r2-rowval)*(c2-colval) / denom
		+  mat.at(ridx,   cidx-1) * (rowval-r1)*(c2-colval) / denom
		+  mat.at(ridx-1, cidx  ) * (r2-rowval)*(colval-c1) / denom
		+  mat.at(ridx,   cidx  ) * (rowval-r1)*(colval-c1) / denom;
}

// this will interpolate or extrapolate as necessary
// if slope is infinite (x1 = x2), it will just return the first Y value
double util::interpolate(double x1, double y1, double x2, double y2, double xValueToGetYValueFor)
{
	if (x1 == x2) return y1;
	if (y1 == y2) return y1;

	double slope = (y2 - y1)/(x2 - x1);
	double inter = y1 - (slope * x1);
	return (slope*xValueToGetYValueFor) + inter;
}

double util::linterp_col( const util::matrix_t<double> &mat, size_t ixcol, double xval, size_t iycol )
{
	// NOTE:  must assume values in ixcol are in increasing sorted order!!

	size_t n = mat.nrows();

	// basic checks
	if ( ixcol >= mat.ncols() 
		|| iycol >= mat.ncols() 
		|| n < 2 )
		return std::numeric_limits<double>::quiet_NaN();


	double last = mat( 0, ixcol );
	size_t i = 1;
	while( i < n )
	{
		double x = mat( i, ixcol );
		// check that values in ixcol are in increasing sorted order
		if ( x < last ) 
			return std::numeric_limits<double>::quiet_NaN(); 

		if ( x > xval )
			break;

		last = x;
		i++;
	}

	// at this point 'i' represents row index
	// with X value just greater than interpolation value

	// if at the end of the list, interpolate with last two values
	if ( i == n ) 
		i--;

	// now do linear interpolation between current row value
	// and previous row value.  xval is between these row X values.
	return util::interpolate( 
			mat( i-1, ixcol ), mat( i-1, iycol ), 
			mat( i,   ixcol ), mat( i,   iycol ),
			xval );
}
