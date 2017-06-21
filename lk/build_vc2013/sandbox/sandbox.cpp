#include <algorithm>

#define LK_USE_WXWIDGETS 1

#include <wx/wx.h>
#include <wx/frame.h>
#include <wx/stc/stc.h>
#include <wx/webview.h>
#include <wx/statbmp.h>
#include <wx/numformatter.h>
#include <wx/tokenzr.h>
#include <wx/grid.h>
#include <wx/zstream.h>
#include <wx/app.h>
#include <wx/busyinfo.h>
#include <wx/stc/stc.h>

#include <lk/absyn.h>
#include <lk/env.h>
#include <lk/stdlib.h>
#include <lk/eval.h>
#include <lk/lex.h>
#include <lk/invoke.h>
#include <lk/parse.h>
#include <lk/codegen.h>
#include <lk/vm.h>

#include "mtrand.h"


enum { ID_CODE = wxID_HIGHEST+149, ID_SAVE, ID_PARSE, ID_IBRK, ID_ASM, ID_BYTECODE, ID_OUTPUT,
	ID_LOAD, ID_RESET, ID_STEP_ASM, ID_DEBUG, ID_STEP_LINE, ID_EVAL };

class VMTestFrame : public wxFrame
{
	lk::env_t *m_runEnv;
	static const int m_markCircle = 0;
	static const int m_markArrow = 1;
	static const int m_markLeftBox = 2;
	static const int m_lineNumMarginId = 0;
	static const int m_syntaxCheckMarginId = 1;
	static const int m_breakpointMarginId = 2;
	static const int m_foldingMarginId = 3;

	wxStyledTextCtrl *m_code;
	wxTextCtrl *m_parse, *m_bytecode, *m_output, *m_error, *m_debug, *m_ibrk;
	wxListBox *m_asm;
	lk::vm vm;
	lk::bytecode bc;

public:
	void ResetRunEnv()
	{
		if ( m_runEnv ) delete m_runEnv;
		m_runEnv = new lk::env_t;
		m_runEnv->register_func( output_cb, this );
		m_runEnv->register_func( rand_cb );
		m_runEnv->register_funcs( lk::stdlib_basic() );
		m_runEnv->register_funcs( lk::stdlib_sysio() );
		m_runEnv->register_funcs( lk::stdlib_math() );
		m_runEnv->register_funcs( lk::stdlib_string() );
		m_runEnv->register_funcs( lk::stdlib_wxui() );
		vm.initialize( m_runEnv );
	}

	VMTestFrame() : wxFrame( NULL, wxID_ANY, "LK-VM", wxDefaultPosition, wxSize(1200,900) )
	{
		m_runEnv = 0;
		
		wxFont font( 12, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false, "Consolas" );

		m_ibrk = new wxTextCtrl( this, ID_IBRK, "0");
		m_ibrk->SetFont( font );
		m_ibrk->SetForegroundColour( *wxGREEN );


		wxBoxSizer *buttons = new wxBoxSizer( wxHORIZONTAL );
		buttons->Add( new wxButton( this, ID_SAVE, "save code"), 0, wxALL|wxEXPAND, 3 );
		buttons->Add( new wxButton( this, ID_LOAD, "load bytecode"), 0, wxALL|wxEXPAND, 3 );
		buttons->Add( new wxButton( this, ID_RESET, "reset"), 0, wxALL|wxEXPAND, 3 );
		buttons->Add( new wxButton( this, ID_STEP_ASM, "stepasm"), 0, wxALL|wxEXPAND, 3 );
		buttons->Add( new wxButton( this, ID_DEBUG, "debug"), 0, wxALL|wxEXPAND, 3 );
		buttons->Add( new wxButton( this, ID_STEP_LINE, "stepln"), 0, wxALL|wxEXPAND, 3 );
		buttons->Add( new wxButton( this, ID_EVAL, "interpret"), 0, wxALL|wxEXPAND, 3 );
		buttons->Add( m_error=new wxTextCtrl( this, wxID_ANY, "ready."), 1, wxALL|wxALIGN_CENTER_VERTICAL, 3 );
		buttons->Add( new wxStaticText( this, wxID_ANY, "   brk: "), 0, wxALL|wxALIGN_CENTER_VERTICAL, 3 );
		buttons->Add( m_ibrk, 0, wxALL|wxEXPAND, 3 );
		m_error->SetForegroundColour( *wxRED );

		m_code = new wxStyledTextCtrl( this, ID_CODE );
		SetupCodeEditorStyle();

		m_parse = new wxTextCtrl( this, ID_PARSE, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE );
		m_parse->SetFont( font );
		m_parse->SetForegroundColour( *wxBLUE );

		m_asm = new wxListBox( this, ID_ASM, wxDefaultPosition, wxDefaultSize, 0, 0, wxLB_HSCROLL );
		m_asm->SetFont( font );
		m_asm->SetForegroundColour( "Forest green" );
		
		m_bytecode = new wxTextCtrl( this, ID_BYTECODE, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE );
		m_bytecode->SetFont( font );
		m_bytecode->SetForegroundColour( "Maroon" );
		
		m_output = new wxTextCtrl( this, ID_OUTPUT, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE );
		m_output->SetForegroundColour( "Dark green" );
		
		m_debug = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE );
		m_debug->SetFont( font );
		m_debug->SetForegroundColour( "Maroon" );

		wxBoxSizer *hsizer = new wxBoxSizer( wxHORIZONTAL );
		hsizer->Add( m_code, 3, wxALL|wxEXPAND, 0 );
		hsizer->Add( m_parse, 1, wxALL|wxEXPAND, 0 );
		hsizer->Add( m_asm, 2, wxALL|wxEXPAND, 0 );
		hsizer->Add( m_bytecode, 1, wxALL|wxEXPAND, 0 );


		wxBoxSizer *tsizer = new wxBoxSizer( wxHORIZONTAL );
		tsizer->Add( m_debug, 2, wxALL|wxEXPAND, 0 );
		tsizer->Add( m_output, 1, wxALL|wxEXPAND, 0 );

		
		wxBoxSizer *vsizer = new wxBoxSizer( wxVERTICAL );
		vsizer->Add( buttons, 0, wxALL|wxEXPAND, 0 );		
		vsizer->Add( hsizer, 3, wxALL|wxEXPAND, 0 );
		vsizer->Add( tsizer, 1, wxALL|wxEXPAND, 0 );
		SetSizer(vsizer);

		m_code->LoadFile( wxGetHomeDir() + "/.lk-vm-code" );
		ParseAndGenerateAssembly();
	}
	virtual ~VMTestFrame() {
		if (m_runEnv) delete m_runEnv;
	}
	
	void SetupCodeEditorStyle()
	{
		static  char *LKWordlist1  =	
	"if while for return exit break continue "
	"function const enum else elseif define "
	"this typeof global true false null import";

		m_code->SetScrollWidthTracking( true );

		m_code->SetStyleBits( 8 );
		m_code->SetLayoutCache( wxSTC_CACHE_PAGE );
		m_code->SetLexer( wxSTC_LEX_NULL );
		
		wxFont font ( 12,
			wxFONTFAMILY_MODERN,
			wxFONTSTYLE_NORMAL,
			wxFONTWEIGHT_NORMAL,
			false,
			"Consolas" );

		m_code->SetFont( font );
		m_code->StyleSetFont (wxSTC_STYLE_DEFAULT, font);
		
		wxFont fontslant( font );
		fontslant.SetStyle( wxFONTSTYLE_ITALIC );
		
		wxFont fontsmall( font );
		fontsmall.SetPointSize( fontsmall.GetPointSize() - 1 );
	
		m_code->SetViewEOL( false );
		m_code->SetIndentationGuides( false );
		m_code->SetEdgeMode( wxSTC_EDGE_NONE );
		m_code->SetViewWhiteSpace( wxSTC_WS_INVISIBLE );
		m_code->SetOvertype( false );
		m_code->SetReadOnly( false );
		m_code->SetWrapMode( wxSTC_WRAP_NONE );
		m_code->StyleSetForeground( wxSTC_STYLE_DEFAULT, *wxBLACK );
		m_code->StyleSetBackground( wxSTC_STYLE_DEFAULT, *wxWHITE );
		m_code->StyleSetForeground( wxSTC_STYLE_INDENTGUIDE, *wxLIGHT_GREY );
		m_code->SetFoldFlags(0);

		// set spaces and indentation
		m_code->SetTabWidth( 4 );
		m_code->SetUseTabs( true );
		m_code->SetTabIndents( true );
		m_code->SetBackSpaceUnIndents( true );
		m_code->SetIndent( 4 );
		m_code->SetEdgeColumn( 80 );
		m_code->SetEdgeColour( wxColour(255,187,187) );
		m_code->SetEdgeMode( wxSTC_EDGE_LINE );
    
		// set visibility
		m_code->SetVisiblePolicy (wxSTC_VISIBLE_STRICT|wxSTC_VISIBLE_SLOP, 1);
		m_code->SetXCaretPolicy (wxSTC_CARET_EVEN|wxSTC_VISIBLE_STRICT|wxSTC_CARET_SLOP, 1);
		m_code->SetYCaretPolicy (wxSTC_CARET_EVEN|wxSTC_VISIBLE_STRICT|wxSTC_CARET_SLOP, 1);
	
		m_code->SetSelForeground( true, *wxWHITE );
		m_code->SetSelBackground( true, *wxBLACK );

		// markers
		m_code->MarkerDefine( wxSTC_MARKNUM_FOLDER,        wxSTC_MARK_DOTDOTDOT, *wxBLACK, *wxBLACK);
		m_code->MarkerDefine( wxSTC_MARKNUM_FOLDEROPEN,    wxSTC_MARK_ARROWDOWN, *wxBLACK, *wxBLACK);
		m_code->MarkerDefine( wxSTC_MARKNUM_FOLDERSUB,     wxSTC_MARK_EMPTY,     *wxBLACK, *wxBLACK);
		m_code->MarkerDefine( wxSTC_MARKNUM_FOLDEREND,     wxSTC_MARK_DOTDOTDOT, *wxBLACK, *wxWHITE);
		m_code->MarkerDefine( wxSTC_MARKNUM_FOLDEROPENMID, wxSTC_MARK_ARROWDOWN, *wxBLACK, *wxWHITE);
		m_code->MarkerDefine( wxSTC_MARKNUM_FOLDERMIDTAIL, wxSTC_MARK_EMPTY,     *wxBLACK, *wxBLACK);
		m_code->MarkerDefine( wxSTC_MARKNUM_FOLDERTAIL,    wxSTC_MARK_EMPTY,     *wxBLACK, *wxBLACK);
		
		m_code->CallTipUseStyle( 30 );
		wxFont fontnormal (*wxNORMAL_FONT) ;
		m_code->StyleSetFont( wxSTC_STYLE_CALLTIP, fontnormal );
		m_code->StyleSetForeground( wxSTC_STYLE_CALLTIP, *wxBLACK );
		m_code->StyleSetBackground( wxSTC_STYLE_CALLTIP, wxColour(247,240,210) );
		
		// set up line number margin
		m_code->SetMarginType( m_lineNumMarginId, wxSTC_MARGIN_NUMBER );
		m_code->StyleSetForeground( wxSTC_STYLE_LINENUMBER, wxColour(80,80,80) );
		m_code->StyleSetBackground( wxSTC_STYLE_LINENUMBER, wxColour(230,230,230) );
		int lineNrMarginWidth = m_code->TextWidth (wxSTC_STYLE_LINENUMBER, _T("_99999"));
		m_code->SetMarginWidth( m_lineNumMarginId, lineNrMarginWidth );

		// breakpoint margin	
		m_code->MarkerDefine( m_markCircle, wxSTC_MARK_CIRCLE );
		m_code->MarkerDefine( m_markArrow, wxSTC_MARK_SHORTARROW );
		m_code->SetMarginType( m_breakpointMarginId, wxSTC_MARGIN_SYMBOL );
		m_code->SetMarginWidth( m_breakpointMarginId, 0 );
		m_code->SetMarginSensitive( m_breakpointMarginId, false );

		m_code->SetLexer( wxSTC_LEX_CPP );
		 
		m_code->StyleSetForeground(wxSTC_C_COMMENT, wxColour(0x00, 0xaf, 0x00));
		m_code->StyleSetForeground(wxSTC_C_COMMENTLINE, wxColour(0x00, 0xaf, 0x00));
		m_code->StyleSetForeground(wxSTC_C_COMMENTDOC, wxColour(0xaf, 0xaf, 0xaf));
	
		m_code->StyleSetFont( wxSTC_STYLE_DEFAULT, font );
		m_code->StyleSetFont( wxSTC_C_DEFAULT, font );
		m_code->StyleSetFont( wxSTC_C_COMMENT, fontslant );
		m_code->StyleSetFont( wxSTC_C_COMMENTLINE, fontslant );
		m_code->StyleSetFont( wxSTC_C_COMMENTDOC, fontslant );

		m_code->StyleSetForeground(wxSTC_C_WORD, wxColour("red"));
		m_code->StyleSetForeground(wxSTC_C_WORD2,  wxColour(0,128,192));
		
		m_code->StyleSetForeground(wxSTC_C_NUMBER,  wxColour(0x00, 0x7f, 0x7f));

		wxColour cLiteral( "maroon" );
		m_code->StyleSetForeground(wxSTC_C_STRING, cLiteral );
		m_code->StyleSetForeground(wxSTC_C_STRINGEOL, cLiteral );
		m_code->StyleSetForeground(wxSTC_C_VERBATIM, cLiteral );
		m_code->StyleSetForeground(wxSTC_C_STRINGRAW, cLiteral );
		m_code->StyleSetForeground(wxSTC_C_TRIPLEVERBATIM, cLiteral );
		m_code->StyleSetForeground(wxSTC_C_HASHQUOTEDSTRING, cLiteral );
		
		m_code->StyleSetForeground(wxSTC_C_CHARACTER,  wxColour(0x7f, 0x00, 0x7f));
		m_code->StyleSetForeground(wxSTC_C_UUID,  wxColour(0x00, 0x7f, 0x7f));
		m_code->StyleSetForeground(wxSTC_C_PREPROCESSOR,  wxColour(0x7f, 0x7f, 0x7f));
		m_code->StyleSetForeground(wxSTC_C_OPERATOR, wxColour("blue"));
		m_code->StyleSetForeground(wxSTC_C_IDENTIFIER, wxColour(0x00, 0x00, 0x00));

		m_code->StyleSetBackground(wxSTC_STYLE_BRACELIGHT, *wxLIGHT_GREY );
		m_code->StyleSetForeground(wxSTC_STYLE_BRACELIGHT, *wxWHITE );
		

		m_code->SetKeyWords(wxSTC_C_DEFAULT, LKWordlist1 );
		m_code->SetMarginWidth(m_foldingMarginId, 0);
		m_code->SetProperty(wxT("fold"), "0");
	}

	static void rand_cb( lk::invoke_t &cxt )
	{
		LK_DOC("rand", "Generate a random number between 0 and 1.", "(none):number");
	static wxMTRand rng;
		cxt.result().assign( rng.rand() );
	}
	static void output_cb( lk::invoke_t &cxt )
	{
		LK_DOC("outln", "output data", "none" );
		VMTestFrame *frm = (VMTestFrame*)cxt.user_data();
		frm->m_output->AppendText( cxt.arg(0).as_string() + "\n");
	}

	void UpdateVMView()
	{
		size_t ip = vm.get_ip();
		size_t iasmsz = m_asm->GetCount();
		if ( ip  <  iasmsz )
			m_asm->SetSelection( ip );

		if ( ip < bc.debuginfo.size() )
		{
			int line = bc.debuginfo[ip].stmt;
			if ( line > 0 && line <= m_code->GetNumberOfLines() )
			{
				int ifirst = m_code->GetFirstVisibleLine();
				int nnl = m_code->LinesOnScreen();

				int ln_to_scroll = line - nnl/2 - 1;
				m_code->ScrollToLine( ln_to_scroll );
				m_code->MarkerDeleteAll(m_markArrow );
				m_code->MarkerAdd( line-1, m_markArrow );
			}
		}
		
		size_t sp = 0;
		lk::vardata_t *stack = vm.get_stack( &sp );
		wxString sout = wxString::Format("[%d]:\n", (int)sp);
		for( size_t i=0;i<sp;i++ )
		{
			lk::vardata_t &sval = stack[sp-i-1];
			sout += "\t" + sval.as_string() + "\t\t(" + sval.typestr() + ")\n";
		}
		sout += "----------------\n\n";

		size_t nfrm = 0;
		lk::vm::frame **frames = vm.get_frames( &nfrm );
		for( size_t i=0;i<nfrm;i++ )
		{
			lk::vm::frame &F = *frames[nfrm-i-1];
			sout += wxString::Format( F.id + "().frame[%d] ret=%d fp=%d iarg=%d narg=%d %s\n", 
				(int)(nfrm-i-1), (int)F.retaddr, (int)F.fp, (int)F.iarg, (int)F.nargs, F.thiscall ? "(thiscall)" : "" );
			lk_string key;
			lk::vardata_t *val;
			bool has_more = F.env.first( key, val );
			while( has_more )
			{
				sout += "\t" + key + "=" + val->as_string() + "\t\t(" + val->typestr() + ")\n";
				has_more = F.env.next( key, val );
			}
		}

		std::vector< size_t > opcnt( (size_t)lk::__MaxOp, 0 );
		vm.get_opcount( &opcnt[0] );
		
		sout += "\nopcode frequency:\n";
		for( size_t i=0;i<(size_t)lk::__MaxOp;i++ )
			sout += wxString::Format("%s\t%d\n", lk::op_table[i].name, (int)opcnt[i]);
		sout+="\n";

		m_debug->ChangeValue( sout );
	}

	void ParseAndGenerateAssembly()
	{
		wxString output, assembly, bytecode_text;
		lk::input_string input( m_code->GetValue() );
		lk::parser parse( input );
		if ( lk::node_t *node = parse.script() )
		{
			if ( parse.error_count() == 0 )
			{
				lk::pretty_print( output, node, 0 );
				lk::codegen cg;
				if ( cg.generate( node ) ) {
					cg.textout( assembly, bytecode_text );
					cg.get( bc );
				}
				else assembly = "error in assembly generation";
			}
			else
			{
				for( int i=0;i<parse.error_count();i++ )
					output += parse.error(i) + "\n";
			}
			
			delete node;
		}
		else
			output = "invalid parse: no nodes obtained";


		m_parse->ChangeValue( output );
		m_asm->Freeze();
		m_asm->Clear();
		wxArrayString asmlines( wxStringTokenize(assembly, "\n") );
		//if ( asmlines.Count() != program.size() )
		//	wxMessageBox( wxString::Format("Error in number of assembly lines: %d vs %d", asmlines.Count(), program.size()));
		m_asm->Append( asmlines );
		m_asm->Thaw();
		m_bytecode->ChangeValue( bytecode_text );

	}

	void OnClose( wxCloseEvent &evt )
	{
		SaveCode();
		wxTheApp->ScheduleForDestruction( this );
	}

	void SaveCode()
	{
		wxString file(wxGetHomeDir() + "/.lk-vm-code");
		wxBusyInfo inf("writing " + file );
		if (! m_code->SaveFile( file ) )
			wxMessageBox("error saving code to file:\n\n" + file );
		wxYield();
		wxMilliSleep(150);
	}

	void OnCommand( wxCommandEvent &evt )
	{
		switch( evt.GetId() )
		{
		case ID_SAVE:			
			SaveCode();
			break;
		case ID_CODE:
			ParseAndGenerateAssembly();
			break;
		case ID_LOAD:
			ResetRunEnv();
			vm.load( &bc );
			m_debug->ChangeValue(
				wxString::Format("vm loaded %d instructions, %d constants, %d identifiers.\n",
					(int) bc.program.size(), (int)bc.constants.size(), (int)bc.identifiers.size() ) );
			m_output->Clear();
			vm.initialize(m_runEnv);
			UpdateVMView();
			break;
		case ID_RESET:
			m_ibrk->ChangeValue( "0" );
			ResetRunEnv();
			vm.initialize(m_runEnv);
			UpdateVMView();
			break;
		case ID_STEP_ASM:
			vm.run( lk::vm::SINGLE );
			m_error->ChangeValue( vm.error() );			
			UpdateVMView();
			break;
		case ID_DEBUG:
		{
			int ln = atoi( m_ibrk->GetValue().c_str() );
			if ( ln > 0 )
			{
				ln = vm.setbrk( ln, "main" );
				m_ibrk->ChangeValue( wxString::Format("%d", ln) );
			}
			wxStopWatch sw;
			vm.run( lk::vm::DEBUG);
			if( ln > 0 )
			{
				ln = vm.setbrk( ln+1, "main" );
				m_ibrk->ChangeValue( wxString::Format("%d", ln) );
			}

			long ms = sw.Time();
			m_error->ChangeValue( vm.error() + wxString::Format("  (elapsed %d ms)\n", ms ) );			
			UpdateVMView();
			break;
		}
		case ID_STEP_LINE:
		{
			wxStopWatch sw;
			vm.run( lk::vm::STEP );
			long ms = sw.Time();
			m_error->ChangeValue( vm.error() + wxString::Format("  (elapsed %d ms)\n", ms ) );		
			UpdateVMView();
			break;
		}

		case ID_EVAL:
			{
				lk::input_string input( m_code->GetValue() );
				lk::parser parse( input, "main" );
				if ( lk::node_t *node = parse.script() )
				{
					if ( parse.error_count() == 0 )
					{
						ResetRunEnv();
						lk::env_t myenv( m_runEnv );
						lk::eval ev( node, &myenv );
						wxStopWatch sw;
						bool ok = ev.run();
						long ms = sw.Time();
						if ( !ok )
						{
							for( size_t i=0;i<ev.error_count();i++ )
								m_output->AppendText( ev.get_error(i) + "\n");
						}
						m_output->AppendText( wxString::Format("\ncompleted in %d ms\n", ms ) );
					}
					else
						m_output->AppendText(" parse errors? ");

					delete node;
				}
			}
			break;
		}
	}

	void OnCodeModify( wxStyledTextEvent &evt )
	{
		if ( evt.GetModificationType() & wxSTC_MOD_INSERTTEXT 
			|| evt.GetModificationType() & wxSTC_MOD_DELETETEXT )
			ParseAndGenerateAssembly();
	}

	DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE( VMTestFrame, wxFrame )
	EVT_CLOSE( VMTestFrame::OnClose )
	EVT_STC_MODIFIED( ID_CODE, VMTestFrame::OnCodeModify )
	EVT_BUTTON( wxID_ANY, VMTestFrame::OnCommand )
END_EVENT_TABLE()


class VMTestApp : public wxApp
{
public:
	VMTestApp() { }

	virtual bool OnInit()
	{
		(new VMTestFrame())->Show();
		return true;
	}
};

IMPLEMENT_APP(VMTestApp);
