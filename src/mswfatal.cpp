#include <stdio.h>
#include <string>
#include <Windows.h>
#include <Psapi.h>

#include "../vc2013_wx3/dbghelp-latest.h"


#include <wx/wx.h>
#include <wex/metro.h>
#include <wex/utils.h>
#include <wx/busyinfo.h>
#include <wx/clipbrd.h>

#include <ssc/sscapi.h>

#include "../resource/exception.cpng"

static HMODULE myDbgHelpDll = 0;

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

// global storage for debugging message generation
static char sg_dbgHelpPath[MAX_PATH];
static std::string sg_message;
static char sg_buf[512];

static void writemsg( const char *fmt, ... )
{
	va_list ap;
	va_start(ap, fmt);
	_vsnprintf(sg_buf, 511, fmt, ap);
	va_end(ap);
	sg_message += std::string(sg_buf);
}

static int load_dbghelp_dll( const char *image_file )
{
	strncpy( sg_dbgHelpPath, image_file, MAX_PATH-12 ); // make sure there's room for 'dbghelp.dll'
	char *plast = strrchr( sg_dbgHelpPath, '\\' );
	if ( plast > sg_dbgHelpPath ) strcpy( plast+1, "dbghelp.dll" );
	
	if ( myDbgHelpDll = LoadLibraryA( sg_dbgHelpPath ) )
	{
		mySymGetModuleInfo64 = ( BOOL (__stdcall *)(HANDLE, DWORD64, PIMAGEHLP_MODULE64) ) GetProcAddress( myDbgHelpDll, "SymGetModuleInfo64" );
		myStackWalk64 = ( BOOL (__stdcall *)(DWORD, HANDLE, HANDLE, LPSTACKFRAME64, PVOID, PREAD_PROCESS_MEMORY_ROUTINE64, PFUNCTION_TABLE_ACCESS_ROUTINE64, PGET_MODULE_BASE_ROUTINE64, PTRANSLATE_ADDRESS_ROUTINE64 ) ) GetProcAddress( myDbgHelpDll, "StackWalk64" );
		mySymSetOptions = ( DWORD (__stdcall *)( DWORD ) ) GetProcAddress( myDbgHelpDll, "SymSetOptions" );
		mySymGetOptions = ( DWORD (__stdcall *)( VOID ) ) GetProcAddress( myDbgHelpDll, "SymGetOptions" );
		mySymInitialize = ( BOOL (__stdcall *)( HANDLE, PCSTR, BOOL ) ) GetProcAddress( myDbgHelpDll, "SymInitialize" );
		mySymCleanup = ( BOOL (__stdcall *)( HANDLE ) ) GetProcAddress( myDbgHelpDll, "SymCleanup" );
		mySymGetModuleBase64 = ( DWORD64 (__stdcall *)( HANDLE, DWORD64 ) ) GetProcAddress( myDbgHelpDll, "SymGetModuleBase64" );
		mySymFunctionTableAccess64 = ( PVOID (__stdcall *)( HANDLE, DWORD64 ) ) GetProcAddress( myDbgHelpDll, "SymFunctionTableAccess64" );
		mySymGetLineFromAddr64 = ( BOOL (__stdcall *)( HANDLE, DWORD64, PDWORD, PIMAGEHLP_LINE64 ) ) GetProcAddress( myDbgHelpDll, "SymGetLineFromAddr64" );
		mySymGetSymFromAddr64 = ( BOOL (__stdcall *)( HANDLE, DWORD64, PDWORD64, PIMAGEHLP_SYMBOL64 ) ) GetProcAddress( myDbgHelpDll, "SymGetSymFromAddr64" );
		mySymLoadModule64 = ( DWORD64 (__stdcall *)( HANDLE, HANDLE, PCSTR, PCSTR, DWORD64, DWORD ) ) GetProcAddress( myDbgHelpDll, "SymLoadModule64" );
	}

	return ( myDbgHelpDll != 0
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

static int write_module_info( const char *lpFilename, DWORD64 ModBase )
{			
	IMAGEHLP_MODULE64 ModuleInfo; 
	memset(&ModuleInfo, 0, sizeof(ModuleInfo) ); 
	ModuleInfo.SizeOfStruct = sizeof(ModuleInfo);
	BOOL bRet = mySymGetModuleInfo64( GetCurrentProcess(), ModBase, &ModuleInfo ); 	
	
	writemsg("Loaded %s @ 0x%08x:", lpFilename, (DWORD)ModBase );

	if( !bRet ) 
	{
		writemsg( ("error %d: SymGetModuleInfo64('%s')\n"), (int)::GetLastError(), lpFilename);
		return 0;
	}
	else
	{
		/*
		writemsg("Module base: 0x%08x\n", (DWORD)ModBase);
		writemsg("Base of image: 0x%08x\n", (DWORD)ModuleInfo.BaseOfImage);
		writemsg("Size of image: 0x%08x (%d)\n",(DWORD)( ModuleInfo.ImageSize  ));
		*/
		switch( ModuleInfo.SymType ) 
		{
		case SymNone: writemsg( ("  No symbols available for the module.\n") ); break; 
		case SymExport: writemsg( ("  Loaded symbols: Exports\n") ); break; 
		case SymCoff: writemsg( ("  Loaded symbols: COFF\n") ); break; 
		case SymCv: writemsg( ("  Loaded symbols: CodeView\n") ); break; 
		case SymSym: writemsg( ("  Loaded symbols: SYM\n") ); break; 
		case SymVirtual: writemsg( ("  Loaded symbols: Virtual\n") ); break; 
		case SymPdb: writemsg( ("  Loaded symbols: PDB\n") ); break; 
		case SymDia: writemsg( ("  Loaded symbols: DIA\n") ); break; 
		case SymDeferred: writemsg( ("  Loaded symbols: Deferred\n") ); break; 
		default: writemsg( ("  Loaded symbols: Unknown format.\n") ); break; 
		}

		//if( strlen( ModuleInfo.ImageName ) > 0 ) 
		//	writemsg( ("Image name: %s \n"), ModuleInfo.ImageName ); 
		
		//if( strlen( ModuleInfo.LoadedImageName ) > 0 ) 
		//	writemsg( ("Loaded image name: %s \n"), ModuleInfo.LoadedImageName ); 

		if( strlen( ModuleInfo.LoadedPdbName ) > 0 ) 
		{
			writemsg( ("\tPDB file: %s \n"), ModuleInfo.LoadedPdbName ); 

			// Is debug information unmatched ? 
			// (It can only happen if the debug information is contained 
			// in a separate file (.DBG or .PDB)
			if( ModuleInfo.PdbUnmatched || ModuleInfo.DbgUnmatched ) 
				writemsg( ("\tWarning: Unmatched symbols. \n") ); 

			writemsg( ("\tLine numbers: %s \n"), ModuleInfo.LineNumbers ? ("Available") : ("Not available") ); 
			writemsg( ("\tGlobal symbols: %s \n"), ModuleInfo.GlobalSymbols ? ("Available") : ("Not available") ); 
			writemsg( ("\tType information: %s \n"), ModuleInfo.TypeInfo ? ("Available") : ("Not available") ); 
			writemsg( ("\tPublic symbols: %s \n"), ModuleInfo.Publics ? ("Available") : ("Not available") ); 
		}
	}

	return 1;
}

const char *ConvertSimpleException ( DWORD dwExcept )
{
    switch ( dwExcept )
    {
    case EXCEPTION_ACCESS_VIOLATION         :
        return "EXCEPTION_ACCESS_VIOLATION";
    break ;

    case EXCEPTION_DATATYPE_MISALIGNMENT    :
        return "EXCEPTION_DATATYPE_MISALIGNMENT";
    break ;

    case EXCEPTION_BREAKPOINT               :
        return "EXCEPTION_BREAKPOINT";
    break ;

    case EXCEPTION_SINGLE_STEP              :
        return "EXCEPTION_SINGLE_STEP";
    break ;

    case EXCEPTION_ARRAY_BOUNDS_EXCEEDED    :
        return "EXCEPTION_ARRAY_BOUNDS_EXCEEDED";
    break ;

    case EXCEPTION_FLT_DENORMAL_OPERAND     :
        return "EXCEPTION_FLT_DENORMAL_OPERAND";
    break ;

    case EXCEPTION_FLT_DIVIDE_BY_ZERO       :
        return "EXCEPTION_FLT_DIVIDE_BY_ZERO";
    break ;

    case EXCEPTION_FLT_INEXACT_RESULT       :
        return "EXCEPTION_FLT_INEXACT_RESULT";
    break ;

    case EXCEPTION_FLT_INVALID_OPERATION    :
        return "EXCEPTION_FLT_INVALID_OPERATION";
    break ;

    case EXCEPTION_FLT_OVERFLOW             :
        return "EXCEPTION_FLT_OVERFLOW";
    break ;

    case EXCEPTION_FLT_STACK_CHECK          :
        return "EXCEPTION_FLT_STACK_CHECK";
    break ;

    case EXCEPTION_FLT_UNDERFLOW            :
        return "EXCEPTION_FLT_UNDERFLOW";
    break ;

    case EXCEPTION_INT_DIVIDE_BY_ZERO       :
        return "EXCEPTION_INT_DIVIDE_BY_ZERO";
    break ;

    case EXCEPTION_INT_OVERFLOW             :
        return "EXCEPTION_INT_OVERFLOW";
    break ;

    case EXCEPTION_PRIV_INSTRUCTION         :
        return "EXCEPTION_PRIV_INSTRUCTION";
    break ;

    case EXCEPTION_IN_PAGE_ERROR            :
        return "EXCEPTION_IN_PAGE_ERROR";
    break ;

    case EXCEPTION_ILLEGAL_INSTRUCTION      :
        return "EXCEPTION_ILLEGAL_INSTRUCTION";
    break ;

    case EXCEPTION_NONCONTINUABLE_EXCEPTION :
        return "EXCEPTION_NONCONTINUABLE_EXCEPTION";
    break ;

    case EXCEPTION_STACK_OVERFLOW           :
        return "EXCEPTION_STACK_OVERFLOW";
    break ;

    case EXCEPTION_INVALID_DISPOSITION      :
        return "EXCEPTION_INVALID_DISPOSITION";
    break ;

    case EXCEPTION_GUARD_PAGE               :
        return "EXCEPTION_GUARD_PAGE";
    break ;

    case EXCEPTION_INVALID_HANDLE           :
        return "EXCEPTION_INVALID_HANDLE";
    break ;

    default :
        return ( "UNKNOWN EXCEPTION CODE" ) ;
    break ;
    }
}

extern int g_verMajor;
extern int g_verMinor;
extern int g_verMicro;
int ssc_version();

class ExceptionDialog : public wxDialog
{
	wxTextCtrl *m_txtctrl;
public:
	ExceptionDialog(const wxString &text, const wxString &title,const wxSize &size)
		: wxDialog( 0, wxID_ANY, title, wxDefaultPosition, size, 
			wxRESIZE_BORDER|wxDEFAULT_DIALOG_STYLE )
	{
		SetBackgroundColour( *wxWHITE );
		
		wxBitmap excbit( wxBITMAP_PNG_FROM_DATA( exception ) );
		wxStaticBitmap *bitmap = new wxStaticBitmap( this, wxID_ANY, excbit );
				
		wxString body;		
		int nbit = (sizeof(void*) == 8) ? 64 : 32;
		body += "Context information:\n\n";
		body += wxString::Format("SAM %d.%d.%d, %d bit using SSC %d and wxWidgets %d.%d.%d\n", 
			g_verMajor, g_verMinor, g_verMicro, nbit, ssc_version(), wxMAJOR_VERSION, wxMINOR_VERSION, wxRELEASE_NUMBER );
		body += "User name: " + wxGetUserName() + "\n";
		body += "Home dir: " + wxGetHomeDir() + "\n";
		body += "OS: " + wxGetOsDescription() + "\n";
		body += "Little endian? " + wxString::Format("%s",wxIsPlatformLittleEndian()?"Yes":"No") + "\n";
		body += "64-bit platform? " + wxString::Format("%s",wxIsPlatform64Bit()?"Yes":"No") + "\n";
		body += "Free memory: " + wxString::Format( "%d MB", (int)(wxGetFreeMemory().ToDouble()/1000000) ) + "\n";
		
		m_txtctrl = new wxTextCtrl(this, -1, text + "\n" + body, 
			wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_READONLY);
		//txtctrl->SetFont( wxFont(10, wxMODERN, wxNORMAL, wxNORMAL) );
		
		wxStaticText *label = new wxStaticText( this, wxID_ANY, "We're very sorry that SAM crashed." );
		label->SetFont( wxMetroTheme::Font( wxMT_LIGHT, 22 ) );
		label->SetForegroundColour( wxColour(90,90,90) );

		
		wxStaticText *label2 = new wxStaticText( this, wxID_ANY, "Please send this crash report to sam.support@nrel.gov by copying it into an email or attaching a saved version of the report." );
		label2->SetFont( wxMetroTheme::Font( wxMT_NORMAL, 11 ) );
		label2->SetForegroundColour( wxColour(90,90,90) );
		label2->Wrap( 450 );



		wxBoxSizer *topsizer = new wxBoxSizer( wxHORIZONTAL );
		topsizer->Add( bitmap, 0, wxALL|wxALIGN_CENTER_VERTICAL, 10 );
		topsizer->Add( label, 0, wxALL|wxALIGN_CENTER_VERTICAL, 10 );

		wxPanel *bpanel = new wxPanel( this );
		bpanel->SetBackgroundColour( wxMetroTheme::Colour( wxMT_FOREGROUND ) );

		wxBoxSizer *butsizer = new wxBoxSizer( wxHORIZONTAL );
		butsizer->Add( new wxMetroButton( bpanel, wxID_COPY, "Copy to clipboard" ), 0, wxALL|wxEXPAND, 0 );
		butsizer->Add( new wxMetroButton( bpanel, wxID_SAVE, "Save to file..." ), 0, wxALL|wxEXPAND, 0 );
		butsizer->AddStretchSpacer();
		butsizer->Add( new wxMetroButton( bpanel, wxID_CANCEL, "Close" ), 0, wxALL|wxEXPAND, 0 );
		bpanel->SetSizer( butsizer );

		wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
		sizer->Add( topsizer, 0, wxALL, 10 );
		sizer->Add( label2, 0, wxCENTER|wxALL, 5 );
		sizer->Add( m_txtctrl, 1, wxALL|wxEXPAND, 10 );
		sizer->Add( bpanel, 0, wxALL|wxEXPAND, 0 );
		SetSizer( sizer );

		SetEscapeId( wxID_CANCEL );
	}

	void OnCommand( wxCommandEvent &evt )
	{
		switch( evt.GetId() )
		{
		case wxID_COPY: 			
			if (wxTheClipboard->Open())
			{
				wxBusyInfo info("Copying crash report to clipboard...");
				wxMilliSleep( 300 );
				wxTheClipboard->SetData(new wxTextDataObject(m_txtctrl->GetValue()));
				wxTheClipboard->Close();
			}
			break;
		case wxID_SAVE:
		{
			wxFileDialog dialog( this, "Save crash report", wxEmptyString, "crash.txt", "Text Files (*.txt)|*.txt", wxFD_SAVE|wxFD_OVERWRITE_PROMPT );
			if ( wxID_OK == dialog.ShowModal() )
			{
				if ( FILE *fp = fopen( dialog.GetPath().c_str(), "w" ) )
				{
					fputs( (const char*)m_txtctrl->GetValue().c_str(), fp );
					fclose( fp );
				}
				else
					wxMessageBox("Could not save crash report to " + dialog.GetPath() );				
			}
		}
			break;
		case wxID_CANCEL: default:
			EndModal( wxCANCEL ); break;
		}
	}

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE( ExceptionDialog, wxDialog )
	EVT_BUTTON( wxID_OK, ExceptionDialog::OnCommand )
	EVT_BUTTON( wxID_CLOSE, ExceptionDialog::OnCommand )
	EVT_BUTTON( wxID_SAVE, ExceptionDialog::OnCommand )
	EVT_BUTTON( wxID_COPY, ExceptionDialog::OnCommand )
END_EVENT_TABLE()

LONG __stdcall MSW_CrashHandlerExceptionFilter( EXCEPTION_POINTERS *pExPtrs )
{
	LONG lRet = EXCEPTION_CONTINUE_SEARCH;

	// really can't do much in the case of a stack overflow since the
	// rest of this handler code probably won't execute, 
	// but just try somehow to alert the user
	if ( EXCEPTION_STACK_OVERFLOW == pExPtrs->ExceptionRecord->ExceptionCode )
	{
		::MessageBoxA( 0, "EXCEPTION_STACK_OVERFLOW occurred!\n", "Unresolvable", 0 );
		::OutputDebugStringA( "EXCEPTION_STACK_OVERFLOW occurred!\n" );
		writemsg( "EXCEPTION_STACK_OVERFLOW occurred!\n" );
	}

	wxBusyInfo *busyinfo = new wxBusyInfo("Unhandled exception, generating report...");
	
	// must create a local copy because StackWalk() modifies cxt
	// at each call
    CONTEXT ctx = *(pExPtrs->ContextRecord);

	writemsg( "Exception Code: 0x%08x (%s)\nException Address: 0x%08x\n\n", 
		pExPtrs->ExceptionRecord->ExceptionCode, 
		ConvertSimpleException( pExPtrs->ExceptionRecord->ExceptionCode ),
		pExPtrs->ExceptionRecord->ExceptionAddress );


	const HANDLE hProcess = ::GetCurrentProcess();

	char lpFilename[512];
	GetModuleFileNameExA(
			hProcess,
			NULL,
			lpFilename,
			511 );
	
	if ( !load_dbghelp_dll( lpFilename ) )
	{
		writemsg( "failed to load %s", sg_dbgHelpPath );
		return lRet;
	}
	else
		writemsg( "Using %s\n", sg_dbgHelpPath );

	mySymSetOptions( SYMOPT_LOAD_LINES | SYMOPT_UNDNAME );

	// for some unknown reason, SymInitialize seems to fail if the
	// third parameter (fInvadeProcess) is TRUE and the stack
	// cannot be properly walked.  If the modules are explicitly
	// enumerated and symbols loaded, it the trace appears to work.
    if ( FALSE == mySymInitialize( hProcess, NULL, FALSE  ) )
    {
        writemsg("SymInitialize error %d\n", (int)GetLastError());
        return lRet;
    }

	DWORD nModules = 0, dwTotal;
	if ( FALSE == EnumProcessModules( hProcess, NULL, 0, &nModules ) )
	{
		writemsg( "EnumProcessModules failed with code %d\n", (int)GetLastError() );
		return lRet;
	}

	// allocate space for HMODULES - don't worry about
	// freeing this memory later since the process will exit anyways
	// when the exception handler finishes
	HMODULE *Modules = new HMODULE[nModules];
	if ( Modules == 0 || FALSE == EnumProcessModules( hProcess, Modules, nModules*sizeof(HMODULE), &dwTotal ) )
	{
		writemsg( "EnumProcessModules failed with code %d\n", (int)GetLastError() );
		return lRet;
	}

	writemsg( "\nProcess modules:\n\n" );

	for ( size_t i=0;i<nModules;i++ )
	{
		if ( Modules[i] == 0 )
			continue;

		if ( FALSE == GetModuleFileNameA( Modules[i], lpFilename, sizeof(lpFilename) ) )
			continue;
				
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

		DWORD64 ModBase = mySymLoadModule64(hProcess,    // target process 
						hFile,        // handle to image - not used
						lpFilename, // name of image file
						NULL,        // name of module - not required
						(DWORD64)Modules[i], //0x10000000,  // base address
						0 //size,           // size of image - not required
						);
							
		if ( !ModBase )          // flags - not required
		{
			writemsg("Failed to load module '%s' @ 0x%08x with code %d\n", lpFilename, (DWORD)Modules[i], (int)GetLastError());
			continue;
		}
		else
		{
			 write_module_info( lpFilename, ModBase );
		}
	}

    // initialize the initial frame: currently we can do it for x86 only
    STACKFRAME64 sf;
	::ZeroMemory(&sf, sizeof(STACKFRAME64));
    DWORD64 dwMachineType;

#if defined(_M_AMD64)
    sf.AddrPC.Offset       = ctx.Rip;
    sf.AddrPC.Mode         = AddrModeFlat;
    sf.AddrStack.Offset    = ctx.Rsp;
    sf.AddrStack.Mode      = AddrModeFlat;
    sf.AddrFrame.Offset    = ctx.Rbp;
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
	
	writemsg( "\nStack trace:\n\n" );

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
			writemsg("PC offset address 0x%08x returned by StackWalk does not exist in module (err %d).", sf.AddrPC.Offset, (int)GetLastError() );	
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
			writemsg("SymFromAddr error %d for address 0x%08x\n", (int)GetLastError(), (unsigned int)sf.AddrPC.Offset );
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
				writemsg("%d [0x%08x] %s()\n", nLevel, (unsigned int)sf.AddrPC.Offset, symname );				
			}
			else			
			{
				writemsg("%d [0x%08x] %s() : at line %d in\n\t%s\n", 
					nLevel, 
					(unsigned int) sf.AddrPC.Offset, 
					symname,
					lineInfo.LineNumber, 
					(const char*)lineInfo.FileName );
			}
		}		
    
    }
    
    if ( !mySymCleanup(hProcess) )
        writemsg("SymCleanup error %d", (int)GetLastError());

	delete busyinfo; // hide the busy info dialog before proceeding
	
	ExceptionDialog dialog( sg_message.c_str(), "Unhandled Fatal Exception", wxSize(650, 550) );
	dialog.CenterOnScreen();
	dialog.ShowModal();
	return lRet;
}


void MSW_HandleFatalException()
{
extern EXCEPTION_POINTERS *wxGlobalSEInformation;
	MSW_CrashHandlerExceptionFilter( wxGlobalSEInformation );	
}