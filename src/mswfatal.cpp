#include <stdio.h>
#include <Windows.h>
#include <Psapi.h>
#include "../vc2013_wx3/dbghelp-latest.h"


static HMODULE myDbgHelp = 0;

static BOOL  ( __stdcall *mySymGetModuleInfo64)(
    _In_ HANDLE hProcess,
    _In_ DWORD64 qwAddr,
    _Out_ PIMAGEHLP_MODULE64 ModuleInfo
    ) = 0;
static BOOL ( __stdcall *myStackWalk64)(
    _In_ DWORD MachineType,
    _In_ HANDLE hProcess,
    _In_ HANDLE hThread,
    _Inout_ LPSTACKFRAME64 StackFrame,
    _Inout_ PVOID ContextRecord,
    _In_opt_ PREAD_PROCESS_MEMORY_ROUTINE64 ReadMemoryRoutine,
    _In_opt_ PFUNCTION_TABLE_ACCESS_ROUTINE64 FunctionTableAccessRoutine,
    _In_opt_ PGET_MODULE_BASE_ROUTINE64 GetModuleBaseRoutine,
    _In_opt_ PTRANSLATE_ADDRESS_ROUTINE64 TranslateAddress
    ) = 0;

static DWORD ( __stdcall *mySymSetOptions)(
    _In_ DWORD   SymOptions
    ) = 0;

static DWORD ( __stdcall *mySymGetOptions) (
    VOID
    ) = 0;

static BOOL ( __stdcall *mySymInitialize)(
    _In_ HANDLE hProcess,
    _In_opt_ PCSTR UserSearchPath,
    _In_ BOOL fInvadeProcess
    ) = 0;

static BOOL ( __stdcall *mySymCleanup)(
    _In_ HANDLE hProcess
    ) = 0;

static DWORD64 ( __stdcall *mySymGetModuleBase64)(
    _In_ HANDLE hProcess,
    _In_ DWORD64 qwAddr
    ) = 0;

static PVOID ( __stdcall *mySymFunctionTableAccess64)(
    _In_ HANDLE hProcess,
    _In_ DWORD64 AddrBase
    ) = 0;

static BOOL ( __stdcall *mySymGetLineFromAddr64)(
    _In_ HANDLE hProcess,
    _In_ DWORD64 qwAddr,
    _Out_ PDWORD pdwDisplacement,
    _Out_ PIMAGEHLP_LINE64 Line64
    ) = 0;
static BOOL ( __stdcall *mySymGetSymFromAddr64)(
    _In_ HANDLE hProcess,
    _In_ DWORD64 qwAddr,
    _Out_opt_ PDWORD64 pdwDisplacement,
    _Inout_ PIMAGEHLP_SYMBOL64  Symbol
    ) = 0;

static DWORD64 ( __stdcall *mySymLoadModule64)(
    _In_ HANDLE hProcess,
    _In_opt_ HANDLE hFile,
    _In_opt_ PCSTR ImageName,
    _In_opt_ PCSTR ModuleName,
    _In_ DWORD64 BaseOfDll,
    _In_ DWORD SizeOfDll
    ) = 0;

static char sg_dbgHelpPath[256];

static int load_dbghelp( char *image_file )
{
	strcpy( sg_dbgHelpPath, image_file );
	char *plast = strrchr( sg_dbgHelpPath, '\\' );
	strcpy( plast+1, "dbghelp.dll" );
	
	if ( myDbgHelp = LoadLibraryA( sg_dbgHelpPath ) )
	{
		mySymGetModuleInfo64 = ( BOOL (__stdcall *)(HANDLE, DWORD64, PIMAGEHLP_MODULE64) ) GetProcAddress( myDbgHelp, "SymGetModuleInfo64" );
		myStackWalk64 = ( BOOL (__stdcall *)(DWORD, HANDLE, HANDLE, LPSTACKFRAME64, PVOID, PREAD_PROCESS_MEMORY_ROUTINE64, PFUNCTION_TABLE_ACCESS_ROUTINE64, PGET_MODULE_BASE_ROUTINE64, PTRANSLATE_ADDRESS_ROUTINE64 ) ) GetProcAddress( myDbgHelp, "StackWalk64" );
		mySymSetOptions = ( DWORD (__stdcall *)( DWORD ) ) GetProcAddress( myDbgHelp, "SymSetOptions" );
		mySymGetOptions = ( DWORD (__stdcall *)( VOID ) ) GetProcAddress( myDbgHelp, "SymGetOptions" );
		mySymInitialize = ( BOOL (__stdcall *)( HANDLE, PCSTR, BOOL ) ) GetProcAddress( myDbgHelp, "SymInitialize" );
		mySymCleanup = ( BOOL (__stdcall *)( HANDLE ) ) GetProcAddress( myDbgHelp, "SymCleanup" );
		mySymGetModuleBase64 = ( DWORD64 (__stdcall *)( HANDLE, DWORD64 ) ) GetProcAddress( myDbgHelp, "SymGetModuleBase64" );
		mySymFunctionTableAccess64 = ( PVOID (__stdcall *)( HANDLE, DWORD64 ) ) GetProcAddress( myDbgHelp, "SymFunctionTableAccess64" );
		mySymGetLineFromAddr64 = ( BOOL (__stdcall *)( HANDLE, DWORD64, PDWORD, PIMAGEHLP_LINE64 ) ) GetProcAddress( myDbgHelp, "SymGetLineFromAddr64" );
		mySymGetSymFromAddr64 = ( BOOL (__stdcall *)( HANDLE, DWORD64, PDWORD64, PIMAGEHLP_SYMBOL64 ) ) GetProcAddress( myDbgHelp, "SymGetSymFromAddr64" );
		mySymLoadModule64 = ( DWORD64 (__stdcall *)( HANDLE, HANDLE, PCSTR, PCSTR, DWORD64, DWORD ) ) GetProcAddress( myDbgHelp, "SymLoadModule64" );
	}

	return ( myDbgHelp != 0
		&& mySymGetModuleInfo64 != 0 
		&& myStackWalk64 != 0
		&& mySymSetOptions != 0
		&& mySymGetOptions != 0
		&& mySymInitialize != 0
		&& mySymCleanup != 0
		&& mySymGetModuleBase64 != 0
		&& mySymFunctionTableAccess64 != 0
		&& mySymGetLineFromAddr64 != 0 
		&& mySymGetSymFromAddr64 != 0
		&& mySymLoadModule64 != 0 );
}

static int get_module_info( char *msg, int ichr, const char *lpFilename, DWORD64 ModBase )
{
	ichr+=sprintf(msg+ichr,"module: %s\n", lpFilename);
			
	IMAGEHLP_MODULE64 ModuleInfo; 
	memset(&ModuleInfo, 0, sizeof(ModuleInfo) ); 
	ModuleInfo.SizeOfStruct = sizeof(ModuleInfo);
	BOOL bRet = mySymGetModuleInfo64( GetCurrentProcess(), ModBase, &ModuleInfo ); 	

	if( !bRet ) 
	{
		ichr+=sprintf(msg+ichr, ("Error: SymGetModuleInfo64() failed. Error code: %u \n"), ::GetLastError());
	}
	else
	{
		ichr+=sprintf(msg+ichr,"Module base: 0x%08x\n", (DWORD)ModBase);
		ichr+=sprintf(msg+ichr,"Base of image: 0x%08x\n", (DWORD)ModuleInfo.BaseOfImage);
		ichr+=sprintf(msg+ichr,"Size of image: 0x%08x (%d)\n",(DWORD)( ModuleInfo.ImageSize  ));
		ichr+=sprintf(msg+ichr,"Address range 0x%08x - 0x%08x\n",(DWORD)ModBase, (DWORD)( ModBase+ModuleInfo.ImageSize  ));

	
		// Display information about symbols 

			// Kind of symbols 

		switch( ModuleInfo.SymType ) 
		{
			case SymNone: 
				ichr+=sprintf(msg+ichr, ("No symbols available for the module.\n") ); 
				break; 

			case SymExport: 
				ichr+=sprintf(msg+ichr, ("Loaded symbols: Exports\n") ); 
				break; 

			case SymCoff: 
				ichr+=sprintf(msg+ichr, ("Loaded symbols: COFF\n") ); 
				break; 

			case SymCv: 
				ichr+=sprintf(msg+ichr, ("Loaded symbols: CodeView\n") ); 
				break; 

			case SymSym: 
				ichr+=sprintf(msg+ichr, ("Loaded symbols: SYM\n") ); 
				break; 

			case SymVirtual: 
				ichr+=sprintf(msg+ichr, ("Loaded symbols: Virtual\n") ); 
				break; 

			case SymPdb: 
				ichr+=sprintf(msg+ichr, ("Loaded symbols: PDB\n") ); 
				break; 

			case SymDia: 
				ichr+=sprintf(msg+ichr, ("Loaded symbols: DIA\n") ); 
				break; 

			case SymDeferred: 
				ichr+=sprintf(msg+ichr, ("Loaded symbols: Deferred\n") ); // not actually loaded 
				break; 

			default: 
				ichr+=sprintf(msg+ichr, ("Loaded symbols: Unknown format.\n") ); 
				break; 
		}

			// Image name 

		if( strlen( ModuleInfo.ImageName ) > 0 ) 
		{
			ichr+=sprintf(msg+ichr, ("Image name: %s \n"), ModuleInfo.ImageName ); 
		}

			// Loaded image name 

		if( strlen( ModuleInfo.LoadedImageName ) > 0 ) 
		{
			ichr+=sprintf(msg+ichr, ("Loaded image name: %s \n"), ModuleInfo.LoadedImageName ); 
		}

			// Loaded PDB name 

		if( strlen( ModuleInfo.LoadedPdbName ) > 0 ) 
		{
			ichr+=sprintf(msg+ichr, ("PDB file name: %s \n"), ModuleInfo.LoadedPdbName ); 
		}

			// Is debug information unmatched ? 
			// (It can only happen if the debug information is contained 
			// in a separate file (.DBG or .PDB) 

		if( ModuleInfo.PdbUnmatched || ModuleInfo.DbgUnmatched ) 
		{
			ichr+=sprintf(msg+ichr, ("Warning: Unmatched symbols. \n") ); 
		}

			// Contents 

				// Line numbers available ? 

		ichr+=sprintf(msg+ichr, ("Line numbers: %s \n"), ModuleInfo.LineNumbers ? ("Available") : ("Not available") ); 

				// Global symbols available ? 

		ichr+=sprintf(msg+ichr, ("Global symbols: %s \n"), ModuleInfo.GlobalSymbols ? ("Available") : ("Not available") ); 

				// Type information available ? 

		ichr+=sprintf(msg+ichr, ("Type information: %s \n"), ModuleInfo.TypeInfo ? ("Available") : ("Not available") ); 

				// Source indexing available ? 

		ichr+=sprintf(msg+ichr, ("Source indexing: %s \n"), ModuleInfo.SourceIndexed ? ("Yes") : ("No") ); 

				// Public symbols available ? 

		ichr+=sprintf(msg+ichr, ("Public symbols: %s \n"), ModuleInfo.Publics ? ("Available") : ("Not available") ); 

	}

	return ichr;
}

static void handle_fatal_from_ep( char *msg, EXCEPTION_POINTERS *pExPtrs )
{

	  const HANDLE hProcess = ::GetCurrentProcess();

		char lpFilename[512];
		GetModuleFileNameExA(
		  hProcess,
		  NULL,
		  lpFilename,
		  511
		);


	if ( !load_dbghelp( lpFilename ) )
	{
		sprintf(msg, "failed to load %s", sg_dbgHelpPath );
		return;
	}


	  *msg = 0;
	  int ichr = 0;

	  ichr += sprintf(msg+ichr, "dbghelp: %s\n", sg_dbgHelpPath );

	  BOOL fInvadeProcess = FALSE;

	  
	mySymSetOptions( SYMOPT_LOAD_LINES | SYMOPT_UNDNAME );

    if ( FALSE == mySymInitialize(
                            hProcess,
                            NULL,   // use default symbol search path
                            fInvadeProcess    // load symbols for all loaded modules?
                        ) )
    {
        ichr+=sprintf(msg+ichr,"SymInitialize error %d\n", (int)GetLastError());
        return;
    }

	DWORD64 ModBase = 0;
	
	if ( !fInvadeProcess )
	{
	//	int len = strlen(lpFilename);
	//	strcpy( &lpFilename[len-3], "pdb" );

	//	size_t size = get_file_size( lpFilename );

		DWORD nmod = 0, dwTotal;
		EnumProcessModules( hProcess, NULL, 0, &nmod );
		HMODULE *Modules = new HMODULE[nmod];
		EnumProcessModules( hProcess, Modules, nmod*sizeof(HMODULE), &dwTotal );
		ichr+=sprintf(msg+ichr, "EnumProcessModules: %d\n", dwTotal );

		for ( size_t i=0;i<nmod;i++ )
		{
			GetModuleFileNameA( Modules[i], lpFilename, sizeof(lpFilename) );
				
			// In order to get the symbol engine to work outside a
			// debugger, it needs a handle to the image.  Yes, this
			// will leak but the OS will close it down when the process
			// ends.
			HANDLE hFile = CreateFileA ( lpFilename       ,
										GENERIC_READ    ,
										FILE_SHARE_READ ,
										NULL            ,
										OPEN_EXISTING   ,
										0               ,
										0                ) ;


			ModBase = mySymLoadModule64(hProcess,    // target process 
							hFile,        // handle to image - not used
							lpFilename, // name of image file
							NULL,        // name of module - not required
							(DWORD64)Modules[i], //0x10000000,  // base address
							0 //size,           // size of image - not required
							);
							
			if ( !ModBase )          // flags - not required
			{
			//	ichr+=sprintf(msg+ichr,"SymLoadModule returned error : %d  (%s)\n", (int)GetLastError(), lpFilename);
			//	mySymCleanup( hProcess );
			//	return;
			}
			else
			{
				ichr += sprintf(msg+ichr, "Loaded %s @ 0x%08x\n", lpFilename, (DWORD)Modules[i] );
				if ( i== 0 )
					ichr = get_module_info(msg, ichr, lpFilename, ModBase);
			}
		}
	}

	// must create a local copy because StackWalk() modifies cxt
	// at each call
    CONTEXT ctx = *(pExPtrs->ContextRecord);

	ichr+=sprintf(msg+ichr, "Exception Code: 0x%08x\nException Address: 0x%08x\n", 
		pExPtrs->ExceptionRecord->ExceptionCode,
		pExPtrs->ExceptionRecord->ExceptionAddress );
	

    // initialize the initial frame: currently we can do it for x86 only
    STACKFRAME64 sf;
	::ZeroMemory(&sf, sizeof(STACKFRAME64));
    DWORD64 dwMachineType;

#if defined(_M_AMD64)
    sf.AddrPC.Offset       = ctx->Rip;
    sf.AddrPC.Mode         = AddrModeFlat;
    sf.AddrStack.Offset    = ctx->Rsp;
    sf.AddrStack.Mode      = AddrModeFlat;
    sf.AddrFrame.Offset    = ctx->Rbp;
    sf.AddrFrame.Mode      = AddrModeFlat;

    dwMachineType = IMAGE_FILE_MACHINE_AMD64;
#elif  defined(_M_IX86)
    sf.AddrPC.Offset       = ctx.Eip;
    sf.AddrPC.Mode         = AddrModeFlat;
    sf.AddrStack.Offset    = ctx.Esp;
    sf.AddrStack.Mode      = AddrModeFlat;
    sf.AddrFrame.Offset    = ctx.Ebp;
    sf.AddrFrame.Mode      = AddrModeFlat;

    dwMachineType = IMAGE_FILE_MACHINE_I386;
#else
    #error "Need to initialize STACKFRAME on non x86"
#endif // _M_IX86
	
	
	ichr+=sprintf(msg+ichr, "\nep.PC.offset=0x%08x\nep.SP.Offset=0x%08x\nep.FR.Offset=0x%08x\n\n", (DWORD)sf.AddrPC.Offset, (DWORD)sf.AddrStack.Offset, (DWORD)sf.AddrFrame.Offset );	

    // iterate over all stack frames
    for ( size_t nLevel = 0; nLevel < 50; nLevel++ )
    {
		    // get the next stack frame
        BOOL bSWRet = myStackWalk64( dwMachineType,
                                hProcess,
                                GetCurrentThread(),
                                &sf,
                                &ctx,
                                NULL,       // read memory function (default)
                                (PFUNCTION_TABLE_ACCESS_ROUTINE64)mySymFunctionTableAccess64,
                                (PGET_MODULE_BASE_ROUTINE64)mySymGetModuleBase64,
                                NULL        // address translator for 16 bit
								);

		if ( FALSE == bSWRet
			||  0 == sf.AddrFrame.Offset)
            break;

		DWORD64 dwModBase = mySymGetModuleBase64( hProcess, sf.AddrPC.Offset );
		if ( dwModBase == 0 )
		{
			ichr+=sprintf(msg+ichr,"PC offset address 0x%08x returned by StackWalk does not exist in module (err %d).", sf.AddrPC.Offset, (int)GetLastError() );	
			break;
		}

		DWORD64 symDisplacement = 0;


	    BYTE symbolBuffer[sizeof(IMAGEHLP_SYMBOL64) + 1024];
		::ZeroMemory(symbolBuffer, sizeof(symbolBuffer));

		PIMAGEHLP_SYMBOL64 pSymbol = (PIMAGEHLP_SYMBOL64)symbolBuffer;
		pSymbol->SizeOfStruct = sizeof(SYMBOL_INFO);
		pSymbol->MaxNameLength = 1024;

		char *symname = "<?>";

		if ( FALSE == mySymGetSymFromAddr64( hProcess, sf.AddrPC.Offset, &symDisplacement, pSymbol ) )
		{
			ichr+=sprintf( msg+ichr, "SymFromAddr error %d for address 0x%08x\n", (int)GetLastError(), (unsigned int)sf.AddrPC.Offset );
		}
		else
		{
			symname = pSymbol->Name;
			
			DWORD dwLineDisplacement;
			// get the source line for this stack frame entry
			IMAGEHLP_LINE64 lineInfo;
			memset( &lineInfo, 0, sizeof(IMAGEHLP_LINE64) );
			lineInfo.SizeOfStruct= sizeof(IMAGEHLP_LINE64);
			if ( !mySymGetLineFromAddr64
								(
									hProcess,
									sf.AddrPC.Offset,
									&dwLineDisplacement,
									&lineInfo
								) )
			{
				// it is normal that we don't have source info for some symbols,
				// notably all the ones from the system DLLs...
				ichr+=sprintf(msg+ichr,"%d [0x%08x] %s() : <?>\n", nLevel, (unsigned int)sf.AddrPC.Offset, symname );
				
			}
			else			
			{
				ichr+=sprintf(msg+ichr,"%d [0x%08x] %s() : at line %d in\n\t%s\n", 
					nLevel, 
					(unsigned int) sf.AddrPC.Offset, 
					symname,
					lineInfo.LineNumber, 
					(const char*)lineInfo.FileName );
			}
		}		
    
    }
    
    if ( !mySymCleanup(hProcess) )
    {
        ichr+=sprintf(msg+ichr,"SymCleanup error %d", (int)GetLastError());
    }
}

static char sg_fatalMessageBuffer[16000];


#include <wx/wx.h>
#include <wex/utils.h>

LONG __stdcall MSW_CrashHandlerExceptionFilter( EXCEPTION_POINTERS *pExPtrs )
{
	LONG lRet = EXCEPTION_CONTINUE_SEARCH;

	if ( EXCEPTION_STACK_OVERFLOW == pExPtrs->ExceptionRecord->ExceptionCode )
	{
		::MessageBoxA( 0, "EXCEPTION_STACK_OVERFLOW occurred!\n", "Unresolvable", 0 );
		::OutputDebugStringA( "EXCEPTION_STACK_OVERFLOW occurred!\n" );
	}
		
	handle_fatal_from_ep( sg_fatalMessageBuffer, pExPtrs );
	//::MessageBoxA( 0, sg_fatalMessageBuffer, "Notice", 0 );
	::wxShowTextMessageDialog( sg_fatalMessageBuffer );

	return lRet;
}


void MSW_HandleFatalException()
{
extern EXCEPTION_POINTERS *wxGlobalSEInformation;
	MSW_CrashHandlerExceptionFilter( wxGlobalSEInformation );	
}