#include <wx/tokenzr.h>
#include <wx/choicdlg.h>
#include <wx/dir.h>
#include <wex/plot/pltext.h>

struct escape_sequence
{
	wxString code;
	wxUniChar value;
};

static escape_sequence escape_codes[] = {

	{ "Alpha",   L'\x0391' },
	{ "Beta",    L'\x0392' },
	{ "Gamma",   L'\x0393' },
	{ "Delta",   L'\x0394' },
	{ "Epsilon", L'\x0395' },
	{ "Zeta",    L'\x0396' },
	{ "Eta",     L'\x0397' },
	{ "Theta",   L'\x0398' },
	{ "Iota",    L'\x0399' },
	{ "Kappa",   L'\x039a' },
	{ "Lambda",  L'\x039b' },
	{ "Mu",      L'\x039c' },
	{ "Nu",      L'\x039d' },
	{ "Xi",      L'\x039e' },
	{ "Omicron", L'\x039f' },
	{ "Pi",      L'\x03a0' },
	{ "Rho",     L'\x03a1' },
	{ "Sigma",   L'\x03a3' },
	{ "Tau",     L'\x03a4' },
	{ "Upsilon", L'\x03a5' },
	{ "Phi",     L'\x03a6' },
	{ "Chi",     L'\x03a7' },
	{ "Psi",     L'\x03a8' },
	{ "Omega",   L'\x03a9' },

	{ "alpha",   L'\x03b1' },
	{ "beta",    L'\x03b2' },
	{ "gamma",   L'\x03b3' },
	{ "delta",   L'\x03b4' },
	{ "epsilon", L'\x03b5' },
	{ "zeta",    L'\x03b6' },
	{ "eta",     L'\x03b7' },
	{ "theta",   L'\x03b8' },
	{ "iota",    L'\x03b9' },
	{ "kappa",   L'\x03ba' },
	{ "lambda",  L'\x03bb' },
	{ "mu",      L'\x03bc' },
	{ "nu",      L'\x03bd' },
	{ "xi",      L'\x03be' },
	{ "omicron", L'\x03bf' },
	{ "pi",      L'\x03c0' },
	{ "rho",     L'\x03c1' },
	{ "fsigma",  L'\x03c3' },
	{ "sigma",   L'\x03c3' },
	{ "tau",     L'\x03c4' },
	{ "upsilon", L'\x03c5' },
	{ "phi",     L'\x03c6' },
	{ "chi",     L'\x03c7' },
	{ "psi",     L'\x03c8' },
	{ "omega",   L'\x03c9' },

	{ "emph",    L'\x00a1' },
	{ "qmark",   L'\x00bf' },
	{ "cent",    L'\x00a2' },
	{ "pound",   L'\x00a3' },
	{ "euro",    L'\x20ac' },
	{ "section", L'\x00a7' },
	{ "dot",     L'\x00b7' },
	{ "mult",    L'\x00d7' },
	{ "copy",    L'\x00a9' },
	{ "reg",     L'\x00ae' },
	{ "deg",     L'\x00b0' },
	{ "pm",      L'\x00b1' },
	{ "ne",      L'\x2260' },
	{ "approx",  L'\x2248' },

	{ wxEmptyString, 0 },
};

static const int FontPointAdjust = 2;

wxPLTextLayout::wxPLTextLayout( wxPLOutputDevice &dc, const wxString &text, TextAlignment ta )
	: m_bounds(0, 0)
{
	if ( text.IsEmpty() ) return;

	// get current font state for subsequent relative adjustments
	double fontPoints = dc.TextPoints();

	// split text into lines, and parse each one into text pieces after resolving escape sequences
	wxArrayString lines = wxStringTokenize( text, "\r\n" );
	for (size_t i=0;i<lines.Count();i++)
		m_lines.push_back( Parse( Escape(lines[i]) ) );

	if ( m_lines.size() == 0 ) return;
		
	// compute extents of each text piece with the right font
	for ( size_t i=0;i<m_lines.size(); i++ )
	{
		for ( size_t j=0;j<m_lines[i].size(); j++ )
		{
			text_piece &tp = m_lines[i][j];
			double width, height;
			dc.TextPoints( tp.state == text_piece::NORMAL ? fontPoints : fontPoints-FontPointAdjust );
			dc.Measure( tp.text, &width, &height );
			tp.size.x = width;
			tp.size.y = height;
		}
	}

	// obtain the approximate heights for normal and small text
	dc.TextPoints( fontPoints-FontPointAdjust );
	double height_small = fontPoints-FontPointAdjust;
	dc.Measure( "0", NULL, &height_small );

	dc.TextPoints( fontPoints );
	double height_normal = fontPoints;
	dc.Measure( "0", NULL, &height_normal );

	// sequentially calculate the origins of each text piece
		
	const double offset = 0.25*height_small; // amount to raise/lower super/sub-scripts
	double y = 0;
		
	for ( size_t i=0;i<m_lines.size(); i++ )
	{
		double x = 0;
		bool has_sup = false, has_sub = false;
		// layout this line's X positions, keep track of whether it has super/subs
		for ( size_t j=0; j< m_lines[i].size(); j++ )
		{
			text_piece &tp = m_lines[i][j];
			if ( tp.state == text_piece::SUPERSCRIPT ) has_sup = true;
			else if (tp.state == text_piece::SUBSCRIPT ) has_sub = true;
				
			// save original relative alignment from x 
			// allows future realignment of text pieces if needed
			tp.aligned_x = x;

			tp.origin.x = x;
			x += tp.size.x;

		}

		// save the line width as the maximum bounds
		if ( x > m_bounds.x )
			m_bounds.x = x;

		// layout this line's Y positions
		if ( has_sup ) y += offset;
		for ( size_t j=0;j<m_lines[i].size(); j++ )
		{
			text_piece &tp = m_lines[i][j];
			if ( tp.state == text_piece::NORMAL )
				tp.origin.y = y;
			else if (tp.state == text_piece::SUPERSCRIPT )
				tp.origin.y = y - offset;
			else if (tp.state == text_piece::SUBSCRIPT )
				tp.origin.y = y + height_normal - 3*offset;
		}

		y += height_normal + offset/3;
	}

	if ( ta != LEFT )
		Align( ta );

	// save the final y position as the maximum height
	m_bounds.y = y;
}

void wxPLTextLayout::Align( TextAlignment ta )
{
	// realign x offsets for differently aligned text
	// now that we know the total bounds width
	// note: does not require recalculating text sizes since they are cached
	for ( size_t i=0;i<m_lines.size();i++ )
	{
		double line_width = 0;
		for ( size_t j=0;j<m_lines[i].size();j++ )
		{
			// restore original aligned positions for each text piece
			m_lines[i][j].origin.x = m_lines[i][j].aligned_x;
			line_width += m_lines[i][j].size.x;
		}

		double offset = 0;
		if ( ta == CENTER ) offset = 0.5*(m_bounds.x - line_width);
		else if ( ta == RIGHT ) offset = m_bounds.x - line_width;

		if ( offset != 0 ) // only do this for center/right alignments
			for ( size_t j=0;j<m_lines[i].size();j++ )
				m_lines[i][j].origin.x += offset;
	}
}

void wxPLTextLayout::Render( wxPLOutputDevice &dc, double x, double y, double rotationDegrees, bool drawBounds )
{
	if ( m_lines.size() == 0 ) return;

	bool aa = dc.GetAntiAliasing();
	if ( drawBounds ) dc.SetAntiAliasing( false );

	double fontPoints = dc.TextPoints();

	if ( drawBounds )
	{
		dc.Pen( *wxLIGHT_GREY, 0.5 );
		dc.NoBrush();
	}

	// layout has already been calculated, assuming the same font.
	// render the text directly given the starting coordinates
	if ( rotationDegrees == 0.0 )
	{
		for ( size_t i=0;i<m_lines.size();i++ )
		{
			for ( size_t j=0;j<m_lines[i].size();j++ )
			{
				text_piece &tp = m_lines[i][j];
				dc.TextPoints( tp.state == text_piece::NORMAL ? fontPoints : fontPoints-FontPointAdjust );
				dc.Text( tp.text, x + tp.origin.x, y + tp.origin.y );				
				if ( drawBounds )
					dc.Rect( x+tp.origin.x, y+tp.origin.y, tp.size.x, tp.size.y );
			}
		}
	}
	else
	{
		double theta = -M_PI/180*rotationDegrees;
		double sintheta = sin(theta);
		double costheta = cos(theta);
		for ( size_t i=0;i<m_lines.size();i++ )
		{
			for ( size_t j=0;j<m_lines[i].size();j++ )
			{
				text_piece &tp = m_lines[i][j];
				dc.TextPoints( tp.state == text_piece::NORMAL ? fontPoints : fontPoints-FontPointAdjust );
				double rotx = tp.origin.x*costheta - tp.origin.y*sintheta;
				double roty = tp.origin.x*sintheta + tp.origin.y*costheta;
				dc.Text( tp.text, x + rotx, y + roty, rotationDegrees );
			}
		}
	}

	// restore font state
	dc.TextPoints( fontPoints );
	if ( drawBounds ) dc.SetAntiAliasing( aa );
}

wxString wxPLTextLayout::Escape( const wxString &text )
{
	wxString result;
	result.Alloc( text.Length() ); 

	bool last_char_slash = false;
	wxString::const_iterator it = text.begin();
	while( it != text.end() )
	{
		if ( *it == '\\'  && !last_char_slash )
		{
			++it; // skip the slash
			wxString code;
			while ( it != text.end()
				&& (*it) != '\\'
				&& wxIsalpha( (*it) ) )
			{
				code += *it;
				++it;
			}

			last_char_slash = ( it != text.end() 
				&& code.Len() == 0 
				&& *it == '\\' );

			if ( it != text.end()  
				&& (*it == ' ' || *it == '\t' ) )
				it++; // skip next space if it exists.  assume is for delineating codes

								
			wxUniChar value( 0 );
			size_t idx = 0;
			while ( ::escape_codes[idx].value != 0 )
			{
				if ( ::escape_codes[idx].code == code )
				{
					value = ::escape_codes[idx].value;
					break;
				}
				else idx++;
			}

			if ( value != 0 ) result += value;
			else if ( !last_char_slash ) result << L'\x275a';
		}
		else
		{
			last_char_slash = false;
			result += *it;
			++it;
		}
	}

	return result;
}

std::vector<wxPLTextLayout::text_piece> wxPLTextLayout::Parse( const wxString &text )
{
	std::vector<text_piece> list;

	text_piece tp;

	wxString::const_iterator it = text.begin();
	while( it != text.end() )
	{
		if ( *it == '^' || *it == '_' )
		{
			wxUniChar modifier = *it;
			it++; // skip modifier to get the next character
			if ( it == text.end() || *it == modifier )
			{
				// if we have a double modifier,
				// simply add it as normal text
				tp.text += *it;
			}
			else 
			{
				if ( !tp.text.IsEmpty() )
				{
					tp.state = text_piece::NORMAL;
					list.push_back( tp );
					tp.text.clear();
				}
				
				tp.state = (modifier == '^') ? text_piece::SUPERSCRIPT : text_piece::SUBSCRIPT;
				if ( it != text.end() && *it == '{' )
				{
					it++; // skip the {
					while ( it != text.end() && *it != '}' )
					{
						tp.text += *it;
						it++;
					}
					
					// go until } encountered
					if ( it != text.end() && *it == '}' )
						it++;
				}
				else
				{
					while ( it != text.end() 
						&& *it != ' ' 
						&& *it != '/'
						&& *it != '\t'
						&& *it != '^'
						&& *it != '_'
						&& *it != '('
						&& *it != '{'
						&& *it != '['
						&& *it != '='
						&& *it != ','
						&& *it != ';' )
					{
						tp.text += *it;
						it++;
					}

					if ( it != text.end() && *it != ' ' )
						--it; // return the character that ended the current mode
				}

				if ( tp.text.Len() > 0 )
				{
					list.push_back( tp );
					tp.text.clear();
				}
			}
		}
		else
		{
			tp.text += *it;
		}

		if ( it != text.end() )
			++it;
	}

	// push back final text piece if needed		
	if ( !tp.text.IsEmpty() )
	{
		tp.state = text_piece::NORMAL;
		list.push_back( tp );
	}

	return list;
}





#include <wx/dc.h>
#include <wx/dcgraph.h>
#include <wx/dcbuffer.h>

BEGIN_EVENT_TABLE( wxPLTextLayoutDemo, wxWindow )
	EVT_PAINT( wxPLTextLayoutDemo::OnPaint )
	EVT_SIZE( wxPLTextLayoutDemo::OnSize )
END_EVENT_TABLE()


wxPLTextLayoutDemo::wxPLTextLayoutDemo( wxWindow *parent )
	: wxWindow( parent, wxID_ANY )
{
	SetBackgroundStyle( wxBG_STYLE_CUSTOM );
}

std::vector<double> wxPLTextLayoutDemo::Draw( wxPLOutputDevice &dc, const wxPLRealRect &geom )
{
	std::vector<double> vlines;
	
	// test 1
	{
		dc.TextPoints( 0 );
		wxPLTextLayout t1( dc, "basic text, nothing special" );
		t1.Render( dc, geom.x+20, geom.y+20, 0.0, true );
		vlines.push_back( geom.x+20 );
		vlines.push_back( geom.x+20+t1.Width() );
	}

	// test 2
	{
		dc.TextPoints( 2 );
		//gc->SetFont( wxFont(18, wxFONTFAMILY_ROMAN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false, "Century Schoolbook"), *wxBLACK );
		wxPLTextLayout t2( dc, "escape^sup_sub \\\\  \\  \\euro \\badcode \\Omega~\\Phi\\ne\\delta, but\n\\Kappa\\approx\\zeta \\emph\\qmark, and this is the end of the text!. Cost was 10 \\pound, or 13.2\\cent" );
		t2.Render( dc, geom.x+20, geom.y+120, 0.0, true );
	}
	
	// test 3
	{
		dc.TextPoints( -1);
		dc.TextColour(*wxRED );
		//gc->SetFont( wxFont(10, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false, "Consolas"), *wxBLACK );
		wxPLTextLayout t3( dc, "super^2 ,not_\\rho\\gamma f hing_{special,great,best}\n\\alpha^^\\beta c^\\delta  efjhijkl__mnO^25 pq_0 r\\Sigma tuvwxyz\nABCDEFGHIJKL^^MNOPQRSTUVWXZY" );
		t3.Render( dc, geom.x+20, geom.y+420, 90, true );
		t3.Render( dc, geom.x+200, geom.y+350, 45.0, true );
		t3.Render( dc, geom.x+400, geom.y+300, 0.0, true );
		vlines.push_back( geom.x+400 );
		vlines.push_back( geom.x+400+t3.Width() );
	}
	
	// test 4
	{
		dc.TextPoints(-2);
		dc.TextColour( *wxBLUE );
		//gc->SetFont( wxFont(16, wxFONTFAMILY_ROMAN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL), *wxBLACK );
		wxPLTextLayout t4( dc, "x_1^2_3 abc=y_2^4" );
		t4.Render( dc, geom.x+200, geom.y+70, 0, false );
	}

	// test 5
	{
		dc.TextPoints(+3 );
		//gc->SetFont( wxFont(7, wxFONTFAMILY_ROMAN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL), *wxBLACK );
		wxPLTextLayout t5( dc, "small (7): x_1^2_3 abc=y_2^4" );
		t5.Render( dc, geom.x+500, geom.y+50, 0, true );
	}
	
	// test 6
	{
		dc.TextPoints(-3 );
		//gc->SetFont( wxFont(8, wxFONTFAMILY_ROMAN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL), *wxBLACK );
		wxPLTextLayout t6( dc, "small (8): x_1^2_3 abc=y_2^4" );
		t6.Render( dc, geom.x+500, geom.y+80, 0, false );
	}
	
	// test 7
	{
		dc.TextPoints(-2 );
		//gc->SetFont( wxFont(9, wxFONTFAMILY_ROMAN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL), *wxBLACK );
		wxPLTextLayout t7( dc, "small (9): x_1^2_3 abc=y_2^4" );
		t7.Render( dc, geom.x+500, geom.y+100, 0, false );
	}
	
	// test 8
	{
		dc.TextPoints(-1);
		dc.TextColour(*wxGREEN );
		//gc->SetFont( wxFont(10, wxFONTFAMILY_ROMAN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL), *wxBLACK );
		//wxPLGraphicsOutputDevice dc( gc );
		wxPLTextLayout t8( dc, "small (10): x_1^2_3 abc=y_2^4" );
		t8.Render( dc, geom.x+500, geom.y+120, 0, false );
	}

	vlines.push_back( geom.x+500 );
	
	return vlines;
}

void wxPLTextLayoutDemo::OnPaint( wxPaintEvent & )
{
	wxAutoBufferedPaintDC pdc( this );
	
	int width, height;
	GetClientSize( &width, &height );

	pdc.SetBackground( *wxWHITE_BRUSH );
	pdc.Clear();

	wxGraphicsContext *gc = wxGraphicsContext::Create( pdc );
	wxPLGraphicsOutputDevice dc( gc, 1.0, 12.0 );
	Draw( dc, wxPLRealRect( 0, 0, width, height/2 ) );
	delete gc;
}

void wxPLTextLayoutDemo::OnSize( wxSizeEvent & )
{
	Refresh();
}

#include <wx/buffer.h>
#include <wx/zstream.h>
#include <wx/msgdlg.h>

#include <wex/utils.h>
#include <wex/pdf/pdffont.h>
#include <wex/pdf/pdffontmanager.h>

#include "fontdata.h"

static std::vector< std::vector<unsigned char>* > gs_inflatedFontData;

static bool inflate_builtin_fonts()
{
	size_t nfonts = 0;
	while( builtinfonts[nfonts].data )
		nfonts++;
	
	if ( gs_inflatedFontData.size() == nfonts )
		return true;
	
	for( size_t i=0;i<gs_inflatedFontData.size();i++ )
		if ( gs_inflatedFontData[i] != 0 )
			delete gs_inflatedFontData[i];

	gs_inflatedFontData.clear();

	wxString errors;
	for( size_t i=0;i<nfonts;i++ )
	{
		wxMemoryInputStream in( (const void*)builtinfonts[i].data, (size_t)builtinfonts[i].len );
		wxZlibInputStream zin( in );

		std::vector<unsigned char> *buf = new std::vector<unsigned char>;
		buf->reserve( builtinfonts[i].len * 3 );
		
		while( zin.CanRead() )
			buf->push_back( (unsigned char)zin.GetC() );

		gs_inflatedFontData.push_back( buf );
		
		/*
		wxPdfFontManager *fmgr = wxPdfFontManager::GetFontManager();
		int pdfstyle = wxPDF_FONTSTYLE_REGULAR;
		if ( builtinfonts[i].bold ) pdfstyle |= wxPDF_FONTSTYLE_BOLD;
		if ( builtinfonts[i].italic ) pdfstyle |= wxPDF_FONTSTYLE_ITALIC;
		if ( !fmgr->GetFont( builtinfonts[i].face, pdfstyle ).IsValid() )
		{
			wxPdfFont font = fmgr->RegisterFont( buf, builtinfonts[i].face, 0 );
			if ( !font.IsValid() )
				errors += "failed to register builtin freetype/pdf font: " + wxString(builtinfonts[i].face) + "\n";
		}*/
	}

	if ( errors.size() > 0 )
		wxShowTextMessageDialog( errors );

	return gs_inflatedFontData.size() == nfonts;
}


#include <ft2build.h>
#include FT_FREETYPE_H

#define SUBPIXEL_RENDERING 1
//#define GAMMA_CORRECTION 1

#ifdef SUBPIXEL_RENDERING
#include FT_LCD_FILTER_H
#endif

static FT_Library ft_library = 0;
struct ft_face_info {
	FT_Face face;
	wxString font;
	wxString file;
	font_data *builtin;
	wxMemoryBuffer data;
};

static std::vector<ft_face_info> ft_faces;

static bool check_freetype_init()
{
	if ( ft_library == 0 )
	{
		FT_Error err = FT_Init_FreeType( &ft_library );


		if ( inflate_builtin_fonts() )
		{
			int i=0;
			while( builtinfonts[i].data )
			{
				FT_Face face;
				FT_Error err = FT_New_Memory_Face( ft_library, 
					&(*gs_inflatedFontData[i])[0], gs_inflatedFontData[i]->size(), 
					0, &face );
			
				if ( 0 == err )
				{
					ft_face_info fi;
					fi.face = face;
					fi.font = builtinfonts[i].face;
					fi.file = wxEmptyString;
					fi.builtin = &builtinfonts[i];
					ft_faces.push_back( fi );
				}

				i++;
			}
		}

#ifdef SUBPIXEL_RENDERING
		if ( err==0 ) FT_Library_SetLcdFilter( ft_library, FT_LCD_FILTER_DEFAULT  );
#endif
		return err==0;
	}
	else 
		return true;
}


int wxFreeTypeLoadAllFonts( const wxString &path )
{
	wxString dir;
	if ( path.IsEmpty() )
	{
		wxGetEnv( "WEXDIR", &dir );
		dir += "/pdffonts";
	}
	else
		dir = path;

	int n = 0;
	wxArrayString files;
	wxDir::GetAllFiles( dir, &files, wxEmptyString, wxDIR_FILES );
	for( size_t k=0;k<files.size();k++ )
	{
		wxFileName file( files[k] );
		wxString ext( file.GetExt().Lower() );
		wxString name( file.GetName() );
		if ( ext == "ttf" || ext == "otf" )
		{
			int face = wxFreeTypeLoadFont( files[k] );
			if ( face >= 0 )
				n++;
		}
	}
	return n;
}

int wxFreeTypeLoadFont( const wxString &font_file )
{
	if ( !check_freetype_init() ) return -1;

	for( size_t i=0;i<ft_faces.size();i++ )
		if ( wxFileName(ft_faces[i].file).SameAs( wxFileName(font_file) ) )
			return i;

	FT_Face face;
	FT_Error err = FT_New_Face( ft_library, font_file.c_str(), 0, &face );
	if ( 0 == err )
	{
		ft_face_info fi;
		fi.face = face;
		fi.font = wxFileName(font_file).GetName();
		fi.file = font_file;
		fi.builtin = NULL;
		ft_faces.push_back( fi );
		return ft_faces.size()-1;
	}
	else
		return -1;
}

wxArrayString wxFreeTypeListFonts()
{
	check_freetype_init();
	wxArrayString list;
	for( size_t i=0;i<ft_faces.size();i++ )
		list.Add( ft_faces[i].font );
	return list;
}

wxString wxFreeTypeFontFile( int ifnt )
{
	if ( ifnt >= 0 && ifnt < (int)ft_faces.size() )
		return ft_faces[ifnt].file;
	else
		return wxEmptyString;
}

wxString wxFreeTypeFontName( int fnt )
{
	if ( fnt >= 0 && fnt < (int) ft_faces.size() )
		return ft_faces[fnt].font;
	else
		return wxEmptyString;
}


bool wxFreeTypeFontStyle( int ifnt, bool *bold, bool *italic )
{
	if ( ifnt >= 0 && ifnt < (int) ft_faces.size() )
	{
		if( bold ) *bold = ft_faces[ifnt].face->style_flags & FT_STYLE_FLAG_BOLD;
		if( italic ) *italic = ft_faces[ifnt].face->style_flags & FT_STYLE_FLAG_ITALIC;
		return true;
	}
	else
		return false;
}

unsigned char *wxFreeTypeFontData( int ifnt, size_t *len )
{
	if ( ifnt >= 0 && ifnt < (int) ft_faces.size() )
	{
		if ( ft_faces[ifnt].builtin && ifnt < gs_inflatedFontData.size() )
		{
			if ( len ) *len = gs_inflatedFontData[ifnt]->size();
			return &(*gs_inflatedFontData[ifnt])[0];
		}
	}

	return 0;

}

int wxFreeTypeFindFont( const wxString &font )
{
	wxString ll(font.Lower());
	for( size_t i=0;i<ft_faces.size();i++ )
		if ( ft_faces[i].font.Lower() == ll )
			return (int)i;

	return -1;
}

// see: https://bel.fi/alankila/lcd/
// and: https://bel.fi/alankila/lcd/alpcor.html
// also: https://www.freetype.org/freetype2/docs/text-rendering-general.html

unsigned char scale( int alpha, int color )
{
	return (unsigned char)( alpha * color / 255 );
}

unsigned char blend( int alpha, int color1, int color2 )
{
	return (unsigned char)( ( color1 * alpha + (255-alpha)*color2 )/255 );
}

// blending uncorrected formula (same as GAMMA=1.0):
   // double output = input1 * alpha + input2 * (1.0 - alpha);


//#define GAMMA 1.2
/* input1 = source color 1, 0 .. 1
* input2 = source color 2, 0 .. 1
* alpha = alpha blend factor, 0 .. 1
*/
//    uncorrected formula (same as GAMMA=1.0):
// double output = input1 * alpha + input2 * (1.0 - alpha);
//    corrected formula:
// double tmp = pow(input1, GAMMA) * alpha + pow(input2, GAMMA) * (1.0 - alpha);
// double output = pow(tmp, 1.0/GAMMA);


static void wxFreeTypeGlyph( unsigned char *rgb, unsigned char *alpha, 
	size_t width, size_t height,
	unsigned char R, unsigned char G, unsigned char B,
	FT_Bitmap *bitmap, int x, int y )
{
	FT_Int  i, j, p, q;
	FT_Int  x_max = x + bitmap->width
#ifdef SUBPIXEL_RENDERING
		/ 3
#endif
		;
	FT_Int  y_max = y + bitmap->rows;

	for ( i = x, p = 0; i < x_max; i++, p++ )
	{
		for ( j = y, q = 0; j < y_max; j++, q++ )
		{
			if ( i < 0      || j < 0       ||
				i >= width || j >= height )
				continue;

			int index = j*width + i;
			/*
			//double input = bitmap->buffer[q * bitmap->width + p];
			//alpha[ j*width + i ] = (unsigned char)(255.0/pow(255.0,1.0/GAMMA)*pow(input,1.0/GAMMA));

			unsigned char A = bitmap->buffer[q * bitmap->width + p];

			//rgb[ index + 0 ] = R;// alpha_correct( A, R );
			//rgb[ index + 1 ] = G;// alpha_correct( A, G );
			//rgb[ index + 2 ] = B;//alpha_correct( A, B );
			alpha[ index ] =  A;// alpha_correct( A, A );
			*/
#ifdef SUBPIXEL_RENDERING
			int pixpos = (q * bitmap->pitch + 3*p) ;
			unsigned char f_r = bitmap->buffer[ pixpos ];
			unsigned char f_g = bitmap->buffer[ pixpos+1 ];
			unsigned char f_b = bitmap->buffer[ pixpos+2 ];
			
			// use green as alpha for overall intensity: http://alienryderflex.com/sub_pixel/

#ifdef GAMMA_CORRECTION

			rgb[ 3*index   ] = gamma_correct( 255-f_r, R );
			rgb[ 3*index+1 ] = gamma_correct( 255-f_g, G );
			rgb[ 3*index+2 ] = gamma_correct( 255-f_b, B );
			alpha[index] =  255;
#else
			if ( f_g > 0 || f_g > 0 || f_b > 0 )
			{
#if SUBPIXEL_BLACK_TEXT_ONLY
				// produces good looking black text on horizontally oriented LCDs.
				rgb[ 3*index   ] = 255-f_r;
				rgb[ 3*index+1 ] = 255-f_g;
				rgb[ 3*index+2 ] = 255-f_b;	
#else
				unsigned char A = f_g;
				// colored text looks messed up
				rgb[ 3*index   ] = blend( 255-f_r, 255-f_r, R );
				rgb[ 3*index+1 ] = blend( 255-f_g, 255-f_g, G );
				rgb[ 3*index+2 ] = blend( 255-f_b, 255-f_b, B );
#endif
				alpha[index] =  A;
			}
#endif

#else
			unsigned char A = bitmap->buffer[q * bitmap->pitch + p];
			if ( A > 0 )
			{
				rgb[ 3*index ]   = R;
				rgb[ 3*index+1 ] = G;
				rgb[ 3*index+2 ] = B;
				alpha[index]     = A;//blend( A, A, alpha[index] );
			}
#endif
		}
	}
}

// see also : http://jcgt.org/published/0002/01/04/paper.pdf

static wxRealPoint rotate2d(
	const wxRealPoint &P, 
	double angle )
{
	double rad = angle*M_PI/180.0;
	return wxRealPoint(
		cos(rad)*P.x - sin(rad)*P.y,
		sin(rad)*P.x + cos(rad)*P.y );
}

void wxFreeTypeDraw( wxDC &dc, const wxPoint &pos, int ifnt, double points, unsigned int dpi,
	const wxString &text, const wxColour &c, double angle )
{
	wxRealPoint offset(0,0);

	wxImage img( wxFreeTypeDraw( &offset, ifnt, points, dpi, text, c, angle ) );

	if ( img.IsOk() )
	{
		wxPoint offpt( pos.x - (int)offset.x, pos.y - (int)offset.y );
		dc.DrawBitmap( wxBitmap(img), offpt );
		/*
		dc.SetPen( *wxLIGHT_GREY_PEN );
		dc.SetBrush( *wxTRANSPARENT_BRUSH );
		dc.DrawRectangle( offpt.x, offpt.y, img.GetWidth(), img.GetHeight() );
		*/
	}
}

#include <wex/utils.h>

void wxFreeTypeDraw( wxGraphicsContext &gc, const wxPoint &pos, int ifnt, double points, unsigned int dpi,
	const wxString &text, const wxColour &c, double angle )
{
	wxRealPoint offset(0,0);

	wxImage img( wxFreeTypeDraw( &offset, ifnt, points, dpi, text, c, angle ) );
	if ( img.IsOk() )
	{
		wxRealPoint offpt( pos.x - offset.x, pos.y - offset.y );
		gc.DrawBitmap( wxBitmap(img), offpt.x, offpt.y, img.GetWidth(), img.GetHeight() );

		/*
		gc.SetPen( *wxLIGHT_GREY_PEN );
		gc.SetBrush( *wxTRANSPARENT_BRUSH );
		gc.DrawRectangle( offpt.x, offpt.y, img.GetWidth(), img.GetHeight() );
		*/
	}
}

wxImage wxFreeTypeDraw( wxRealPoint *offset, int ifnt, double points, unsigned int dpi, const wxString &text, const wxColour &c, double angle )
{
	if ( !check_freetype_init() || ft_faces.size() == 0 || text.IsEmpty() || ifnt < 0  ) 
		return wxNullImage;

	if ( ifnt >= (int)ft_faces.size() )
		ifnt = 0; // use default if invalid font

	// first measure the text
	wxSize size( wxFreeTypeMeasure( ifnt, points, dpi, text ) );
	if ( size.x == 0 || size.y == 0 )
		return wxNullImage;

	wxRealPoint pp[3] = { wxRealPoint(0, -size.y), wxRealPoint( size.x, -size.y ), wxRealPoint( size.x, 0 ) };
	if ( angle != 0.0 )
		for( size_t i=0;i<3;i++ )
			pp[i] = rotate2d( pp[i], angle );

	// find bounding box of rotated text
	wxRealPoint min(0,0), max(0,0);
	for( size_t i=0;i<3;i++ )
	{
		if ( pp[i].x < min.x ) min.x = pp[i].x;
		if ( pp[i].x > max.x ) max.x = pp[i].x;

		if ( pp[i].y < min.y ) min.y = pp[i].y;
		if ( pp[i].y > max.y ) max.y = pp[i].y;
	}

	// create image surface of appropriate size
	wxSize bounds( abs(max.x-min.x), abs(max.y-min.y) );
	wxImage img( bounds, false );
	
	// find offset coordinate for top-left placement
	*offset = wxPoint( -min.x, max.y );

	// render the text
	wxFreeTypeDraw( &img, true, *offset, ifnt, points, dpi, text, c, angle );
	
	return img;
}

void wxFreeTypeDraw( wxImage *img, bool init_img, const wxPoint &pos, 
	int ifnt, double points, unsigned int dpi, 
	const wxString &text, const wxColour &c, double angle )
{
	if ( !check_freetype_init() || ft_faces.size() == 0 || text.IsEmpty() || ifnt < 0 || !img ) 
		return;

	if ( ifnt >= (int)ft_faces.size() )
		ifnt = 0;
	
	wxSize size( img->GetWidth(), img->GetHeight() );

	if ( !img->HasAlpha() )
		img->InitAlpha();

	FT_Face face = ft_faces[ifnt].face;
	FT_Error err = FT_Set_Char_Size( face, (int)(points*64.0), 0, dpi, dpi );
	
	
	unsigned char R = c.Red();
	unsigned char G = c.Green();
	unsigned char B = c.Blue();
	unsigned char *rgb = img->GetData();
	unsigned char *alpha = img->GetAlpha();

	if ( init_img )
	{
		// initialize rgb and alpha
		for( size_t i=0;i<size.x*size.y;i++ )
		{
			rgb[3*i] = R;
			rgb[3*i+1] = G;
			rgb[3*i+2] = B;
			alpha[i] = 0;
		}
	}

	

	angle *= M_PI/180;

	double pix_ascent = ((double)face->ascender)/((double)face->units_per_EM) * points * dpi/72.0;
	
	wxRealPoint origin( pos.x, pos.y );

	origin.x += pix_ascent*sin(angle);
	origin.y += pix_ascent*cos(angle);
	
	FT_Matrix  matrix;
	matrix.xx = (FT_Fixed)( cos( angle ) * 0x10000L );
	matrix.xy = (FT_Fixed)(-sin( angle ) * 0x10000L );
	matrix.yx = (FT_Fixed)( sin( angle ) * 0x10000L );
	matrix.yy = (FT_Fixed)( cos( angle ) * 0x10000L );
	
	bool use_kerning = FT_HAS_KERNING( face );
	FT_UInt previous = 0, glyph_index;
	
	FT_Vector pen;
	pen.x = (int)(origin.x * 64);
	pen.y = (int)( (size.y - origin.y) * 64.0 );

	for( wxString::const_iterator it = text.begin(); it != text.end(); ++it )
	{
		FT_Set_Transform( face, &matrix, &pen );
		
		glyph_index = FT_Get_Char_Index( face, *it );

		/* retrieve kerning distance and move pen position */
		if ( use_kerning && previous && glyph_index )
		{
			FT_Vector  delta;
			FT_Get_Kerning( face, previous, glyph_index, FT_KERNING_DEFAULT, &delta );
			FT_Vector_Transform( &delta, &matrix );
			pen.x += delta.x >> 6;
			pen.y += delta.y >> 6;
		}

		unsigned int mode = FT_LOAD_DEFAULT;

		if ( angle != 0.0 ) // see: http://chanae.walon.org/pub/ttf/ttf_glyphs.htm
			mode |= FT_LOAD_NO_HINTING;
		
#ifdef SUBPIXEL_RENDERING
		mode |= FT_LOAD_TARGET_LCD;
#endif
		err = FT_Load_Glyph( face, glyph_index, mode );
		if ( err ) continue;

		err = FT_Render_Glyph( face->glyph, 
#ifdef SUBPIXEL_RENDERING
			FT_RENDER_MODE_LCD
#else
			FT_RENDER_MODE_NORMAL
#endif
			);

		if ( err ) continue;

		wxFreeTypeGlyph( rgb, alpha, size.x, size.y, R, G, B,
			&face->glyph->bitmap, 
			face->glyph->bitmap_left,
			size.y - face->glyph->bitmap_top );

		// increment pen pos
		pen.x += face->glyph->advance.x;
		pen.y += face->glyph->advance.y;

		previous = glyph_index;
	}
}



wxSize wxFreeTypeMeasure( int fnt, double points, unsigned int dpi, const wxString &text )
{
	if ( !check_freetype_init() || fnt < 0 || ft_faces.size() == 0  ) 
		return wxSize( std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN() );

	if ( fnt >= (int)ft_faces.size() )
		fnt = 0;

	FT_Face face = ft_faces[fnt].face;
	
	FT_Error err = FT_Set_Char_Size( face, (int)(points*64.0), 0, dpi, dpi );
	if ( err ) 
		return wxSize(0,0);

	FT_Set_Transform( face, 0, 0 );

	FT_UInt previous, glyph_index;
	int pen_x = 0;
	int pen_y = 0;
	int px_ascent = ((double)face->ascender)/((double)face->units_per_EM) * points * dpi/72.0;
	bool use_kerning = FT_HAS_KERNING( face );
	previous = 0;
	for( wxString::const_iterator it = text.begin(); it != text.end(); ++it )
	{
		FT_ULong uchar = (*it);
		glyph_index = FT_Get_Char_Index( face, uchar );

		/* retrieve kerning distance and move pen position */
		if ( use_kerning && previous && glyph_index )
		{
			FT_Vector  delta;
			FT_Get_Kerning( face, previous, glyph_index, FT_KERNING_DEFAULT, &delta );
			pen_x += delta.x >> 6;
		}

		err = FT_Load_Glyph( face, glyph_index, FT_LOAD_DEFAULT );
		if ( err ) continue;

		// increment pen pos
		pen_x += face->glyph->advance.x >> 6;
		pen_y += face->glyph->advance.y >> 6; // not useful for now (?)

		// save current glyph for next kerning
		previous = glyph_index;
	}
	
	return wxSize( abs(pen_x), 
		((double)(face->ascender-face->descender))/((double)face->units_per_EM) * points * dpi/72.0 );
}

#include <wx/dcbuffer.h>
#include <wx/dir.h>
#include <wx/wfstream.h>
#include <wx/msgdlg.h>

#include <wex/pdf/pdfdoc.h>
#include <wex/pdf/pdffontmanager.h>

BEGIN_EVENT_TABLE( wxFreeTypeDemo, wxWindow )
	EVT_PAINT( wxFreeTypeDemo::OnPaint )
	EVT_SIZE( wxFreeTypeDemo::OnSize )
END_EVENT_TABLE()



wxFreeTypeDemo::wxFreeTypeDemo( wxWindow *parent )
	: wxWindow( parent, wxID_ANY )
{
	SetBackgroundStyle( wxBG_STYLE_PAINT );

	wxString dir;
	wxGetEnv( "WEXDIR", &dir );

	dir += "/pdffonts";
	
	wxPdfDocument doc( wxPORTRAIT, "pt", wxPAPER_A5 );
	doc.AddPage( wxPORTRAIT, 72*8, 72*11.5 );
	doc.SetFont( "Helvetica", 0, 8 );
	doc.SetTextColour( *wxBLACK );

	int ypos = 36;
	wxArrayString files;
	wxDir::GetAllFiles( dir, &files, wxEmptyString, wxDIR_FILES );
	for( size_t k=0;k<files.size();k++ )
	{
		wxFileName file( files[k] );
		wxString ext( file.GetExt().Lower() );
		wxString name( file.GetName() );
		if ( ext == "ttf" || ext == "otf" )
		{
			int face = wxFreeTypeLoadFont( files[k] );
			if ( face >= 0 )
				faces.push_back( face );
			
			bool bold, italic;
			wxFreeTypeFontStyle( face, &bold, &italic );
			int pdfstyle = wxPDF_FONTSTYLE_REGULAR;
			if (bold) pdfstyle |= wxPDF_FONTSTYLE_BOLD;
			if (italic) pdfstyle |= wxPDF_FONTSTYLE_ITALIC;

			doc.SetFont( "Helvetica", 0, 8 );
			wxPdfFont font = wxPdfFontManager::GetFontManager()->GetFont( name, pdfstyle );
			if ( !font.IsValid() )
				font = wxPdfFontManager::GetFontManager()->RegisterFont( files[k], name );

			if ( font.IsValid() )
			{
				wxString text(name + ": The quick brown fox...");
				if ( doc.SetFont( name, pdfstyle, 14 ) )
					doc.Text( 36, ypos, text );
				else
				{
					doc.SetFont( "Helvetica", 0, 8 );
					doc.Text( 36, ypos, "style error with font: " + name );
				}
			}
			else
				doc.Text( 36, ypos, "error with font: " + name );
							
			ypos += 14;
		}
	}
	
	wxString pdffile( dir + "/demo.pdf" );
	const wxMemoryOutputStream &data = doc.CloseAndGetBuffer();
	wxFileOutputStream fp( pdffile );
	if ( fp.IsOk())
	{

		wxMemoryInputStream tmpis( data );
		fp.Write( tmpis );
		fp.Close();

		wxLaunchDefaultBrowser( pdffile );
	}
	else
		wxMessageBox("Could not write PDF output file" );


}

int wxFreeTypeDemo::Face(int i )
{
	if ( i <= faces.size() )
		return i;
	else
		return faces.size()-1;
}

void wxFreeTypeDemo::OnPaint( wxPaintEvent & )
{
	wxAutoBufferedPaintDC dc(this);
	dc.SetBackground( *wxWHITE_BRUSH );
	dc.Clear();

	if ( faces.size() == 0 )
	{
		dc.DrawText( "No fonts loaded", 10, 10 );
		return;
	}

	wxString text( wxPLTextLayout::Escape( "AV Ta ^sup_sub\\\\euro \\badcode \\Omega~\\Phi\\ne\\delta, but\n\\Kappa\\approx\\zeta \\emph\\qmark, and this is the end of the text!. Cost was 10 \\pound, or 13.2\\cent" ) );
		
	wxSize size(GetClientSize());
	dc.SetPen( wxPen( "yellow", 3 ) );
	dc.DrawLine( wxPoint(0,0), wxPoint(size.x,size.y) );
	dc.SetBrush( *wxYELLOW_BRUSH );
	dc.DrawRectangle( 100, 10, 100, 100 );
	dc.SetBrush( wxBrush( wxColour(90,90,90) ) );
	dc.SetPen( *wxTRANSPARENT_PEN );
	dc.DrawRectangle( 0, 140, 300, 100 );

	wxImage img( size.x, size.y );
	unsigned int dpi = wxGetDrawingDPI();
	wxString demotext( text + " (" + wxFreeTypeFontName( Face(1) ) + ")" );
	wxSize bbox = wxFreeTypeMeasure( Face(1), 14, dpi, demotext );
	wxFreeTypeDraw( &img, true,  wxPoint(0,0), Face(1), 14,     dpi, demotext, *wxBLACK );
	wxFreeTypeDraw( &img, false, wxPoint(10,200),Face(2), 12,   dpi, text + " (" + wxFreeTypeFontName(Face(2)) + ")", *wxRED, 45.0 );
	wxFreeTypeDraw( &img, false, wxPoint(10,200),Face(1), 14,   dpi, text + " (" + wxFreeTypeFontName(Face(2)) + ")", *wxBLACK, -45.0 );
	wxFreeTypeDraw( &img, false, wxPoint(150,150), Face(1), 26, dpi, "Vertical text", *wxBLUE, 90 );
	wxFreeTypeDraw( &img, false, wxPoint(200,200), Face(2), 26, dpi, "Vertical text", "Dark Green", -90 );
	wxFreeTypeDraw( &img, false, wxPoint(150,150), Face(1), 20, dpi, "WHITE TEXT", *wxWHITE, 10 );
	wxFreeTypeDraw( &img, false, wxPoint(300,200), Face(2), 16, dpi, "Flipped backwards super text", *wxCYAN, 180 );
	
	wxPoint p1(350,200);
	dc.SetPen( *wxBLACK_PEN );
	dc.SetBrush( *wxGREEN_BRUSH );
	int dist = wxFreeTypeMeasure( Face(2), 12, dpi, "text to rotate").x;
	for( double angle = 0;angle<=270;angle+=22.5 )
	{
		wxPoint vec( dist*cos(angle*M_PI/180),dist*sin(-angle*M_PI/180) );
		dc.DrawLine( p1, p1+vec  );
		dc.DrawCircle( p1+vec, 3 );
		wxFreeTypeDraw( &img, false, wxPoint(350,200), Face(2), 12, dpi, "text to rotate", "Forest Green", angle );
	}

	wxFreeTypeDraw( &img, false, wxPoint(350,170), Face(2), 12, dpi, "Text positioning", *wxBLACK, 0 );
	wxFreeTypeDraw( &img, false, wxPoint(650,170), Face(3), 12, dpi, "Text positioning", *wxBLACK, 0 );
	wxFreeTypeDraw( &img, false, wxPoint(350,570), Face(4), 12, dpi, "Text positioning", *wxBLACK, 0 );
	wxFreeTypeDraw( &img, false, wxPoint(650,570), Face(5), 12, dpi, "Text positioning", *wxBLACK, 0 );
	
	int yp = 0;
	for( size_t i=0;i<faces.size();i++ )
	{
		wxString thetext( wxFreeTypeFontName(i) + ": The quick brown fox jumped..." );
		wxSize sz = wxFreeTypeMeasure( i, 14, dpi, thetext );
		wxFreeTypeDraw( &img, false, wxPoint(700,yp), i, 14, dpi, thetext, *wxBLACK, 0 );
		yp += sz.y;
	}

	dc.DrawBitmap( wxBitmap(img), wxPoint(0,0) );

	dc.SetPen( *wxBLACK_PEN );
	dc.SetBrush( *wxBLACK_BRUSH );
	dc.DrawRectangle( 320,170,30,30) ;
	dc.DrawRectangle( 620,170,30,30) ;
	dc.DrawRectangle( 320,570,30,30) ;
	dc.DrawRectangle( 620,570,30,30) ;

	dc.SetPen( *wxCYAN_PEN );
	dc.DrawPoint( 350,200 );
	dc.SetBrush( *wxTRANSPARENT_BRUSH );
	dc.SetPen( *wxLIGHT_GREY_PEN );
	dc.DrawRectangle( 0, 0, bbox.x, bbox.y );

	dc.SetBrush( *wxWHITE_BRUSH );
	dc.DrawRectangle( 0, 0, 500, 400 );
	//dc.SetBrush( *wxLIGHT_GREY_BRUSH );
	//dc.DrawCircle(200,200,4);

	wxFreeTypeDraw( dc, wxPoint(300,200), Face(23), 12, dpi,  "The quick brown fox jumped...", *wxBLUE, 0.0 );
	wxFreeTypeDraw( dc, wxPoint(300,200), Face(24), 10, dpi, "...over the lazy dog.", *wxRED, 45 );
	wxFreeTypeDraw( dc, wxPoint(300,200), Face(22), 14, dpi, "Jumping dogs over lazy foxes.", "Forest Green", 195 );
	dc.SetBrush( *wxBLACK_BRUSH );
	dc.DrawRectangle( 0, 0, 300, 200 );
	wxFreeTypeDraw( dc, wxPoint(300,200), Face(27), 18, dpi, "White on black.", *wxWHITE, 135 );
	wxFreeTypeDraw( dc, wxPoint(300,200), Face(0), 12, dpi, "TOP TO BOTTOM.", *wxBLACK, 270 );
}

void wxFreeTypeDemo::OnSize( wxSizeEvent & )
{
	Refresh();
}




void wxFreeTypeDemo::GenerateTTFBinaryFontData()
{
	wxArrayString msgs;
	wxString dir;
	wxGetEnv( "WEXDIR", &dir );
	wxArrayString files;
	wxDir::GetAllFiles( dir + "/pdffonts", &files, wxEmptyString, wxDIR_FILES );
	
	
	wxString hfile( dir + "/pdffonts/fontdata.h" );
	FILE *fp = fopen( hfile.c_str(), "w" );
	
	wxArrayString names, cnames;
	
	wxArrayInt sel;
	int nn = wxGetSelectedChoices( sel, "regenerate binary .h font data file for built-in fonts?", "Query", files );
	if ( nn >= 0  )
	{
		for( size_t kk=0;kk<sel.size();kk++) 
		{
			int i = sel[kk];

			wxFileName fn(files[i]);
			wxString ext( fn.GetExt().Lower() );
			if ( ext == "ttf" || ext == "otf" )
			{
				wxFFileInputStream in( files[i] );
				if ( !in.IsOk() ) continue;

				wxFFileOutputStream out( dir + "/pdffonts/" + fn.GetName() + ".z" );
				wxZlibOutputStream zout( out );
				zout.Write( in );
				zout.Close();
				out.Close();
			
				wxString cname( fn.GetName() );
				cname.Replace( " ", "" );
				cname.Replace( "-", "_" );
				cname.MakeLower();

				unsigned long n = 0;
				fprintf(fp,"static unsigned char %s[] = {\n", (const char*)cname.c_str());

				wxFFileInputStream in2( dir + "/pdffonts/" + fn.GetName() + ".z" );
				unsigned char byte;
				in2.Read(&byte,1);
				while( in2.LastRead() == 1 )
				{
					fprintf(fp,"0x%.2X", (int)byte);
					++n;

					in2.Read(&byte,1);
					if ( in2.LastRead() != 1 )
						break;
					
					fprintf(fp, "," );
					if(n % 20 == 0) fprintf(fp,"\n");
				}
				
				fprintf(fp,"};\n");
				fprintf(fp,"static const int %s_len = %d;\n", (const char*)cname.c_str(), n );

				cnames.Add( cname );
				names.Add( fn.GetName() );
				msgs.Add( "loaded: " + fn.GetName() );
			}

		}

	
	
		fprintf( fp, "\nstruct font_data\n" );
		fprintf( fp, "{\n");
		fprintf( fp, "  unsigned char *data;\n");
		fprintf( fp, "  unsigned int len;\n");
		fprintf( fp, "  const char *face;\n");
		fprintf( fp, "  const char *family;\n" );
		fprintf( fp, "  int bold;\n");
		fprintf( fp, "  int italic;\n");
		fprintf( fp, "};\n\n");
		fprintf(fp,"font_data builtinfonts[] = {\n" );
		for( size_t i=0;i<names.size();i++ )
		{
			wxCStrData name = names[i].c_str();
			wxString family( name );
			int dash = family.Find( "-" );
			if ( dash != wxNOT_FOUND )
			{
				family.Truncate( dash );
				family = family.Trim().Trim(false);
			}


			wxCStrData cname = cnames[i];
			int bold = (cnames[i].Find( "bold" ) != wxNOT_FOUND );
			int italic = (cnames[i].Find( "italic" ) != wxNOT_FOUND );
			fprintf(fp, "  { %s, %s_len, \"%s\", \"%s\", %d, %d },\n",
				(const char*)cname, 
				(const char*)cname, 
				(const char*)name,
				(const char*)family.c_str(),
				bold, 
				italic );
		}
		fprintf(fp,"  { 0, 0, 0, 0, 0, 0 }\n" );
		fprintf(fp,"};\n\n" );

		fclose(fp);
		wxShowTextMessageDialog( wxJoin( msgs, '\n' ) );
	}
}
