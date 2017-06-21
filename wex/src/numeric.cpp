#include "wex/numeric.h"
#include <cmath>
#include <wx/wx.h>
#include <wx/valtext.h>

BEGIN_EVENT_TABLE( wxNumericCtrl, wxTextCtrl )
	EVT_TEXT_ENTER( wxID_ANY, wxNumericCtrl::OnTextEnter )
	EVT_KILL_FOCUS( wxNumericCtrl::OnLoseFocus )
	EVT_SET_FOCUS( wxNumericCtrl::OnSetFocus )
END_EVENT_TABLE()

wxNumericCtrl::wxNumericCtrl( wxWindow *parent, int id, 
		double value, wxNumericMode m,
		const wxPoint &pos,
		const wxSize &size )
	: wxTextCtrl(parent, id, wxEmptyString, pos, size, 
			wxTE_PROCESS_ENTER|wxTE_RIGHT)
{
	m_min = m_max = 0.0;
	m_mode = m;
	m_decimals = wxNUMERIC_GENERIC;
	m_thouSep = false;
	
	SetupValidator();
	
	SetValue( value );
}

void wxNumericCtrl::OnTextEnter( wxCommandEvent &evt )
{
	if ( m_focusStrVal != GetValue() )
	{
		m_focusStrVal = GetValue();
		Translate();
		SetSelection(0,this->GetValue().Len());
		evt.Skip();
	}
}

void wxNumericCtrl::OnSetFocus( wxFocusEvent &evt )
{
	m_focusStrVal = GetValue();
	SetSelection(0,m_focusStrVal.Len());
	evt.Skip();
}

void wxNumericCtrl::OnLoseFocus( wxFocusEvent &evt )
{
	if ( m_focusStrVal != GetValue() )
	{
		Translate();
		wxCommandEvent enterpress(wxEVT_COMMAND_TEXT_ENTER, this->GetId() );
		enterpress.SetEventObject( this );
		enterpress.SetString( GetValue() );
		GetEventHandler()->ProcessEvent(enterpress);
	}
	evt.Skip();
}

void wxNumericCtrl::SetupValidator()
{
	wxArrayString excludes;
	excludes.Add( wxString(wxChar(',')) ); // thousands separator

	if ( m_mode == wxNUMERIC_INTEGER || m_mode == wxNUMERIC_UNSIGNED )
	{
		excludes.Add("+");
		excludes.Add("e");
		excludes.Add("E");
		excludes.Add(".");
		
		if ( m_mode == wxNUMERIC_UNSIGNED )
			excludes.Add( '-' );
	}


	wxTextValidator val( wxFILTER_NUMERIC|wxFILTER_EXCLUDE_CHAR_LIST );
	val.SetExcludes( excludes );
	SetValidator( val );
}

static bool is_valid_char( wxNumericMode mode, wxUniChar c, bool additional, wxUniChar c1 )
{
	if ( mode == wxNUMERIC_INTEGER ) return wxIsdigit(c) || c == '-' || c=='+' || (additional && c == c1);
	else if ( mode == wxNUMERIC_UNSIGNED ) return wxIsdigit(c) || (additional && c == c1);
	else return wxIsdigit(c) || c == '-' || c=='+' || c == '.' || c == 'e' || c == 'E' || (additional && c == c1);
}

void wxNumericCtrl::Translate()
{
	wxString buf;
	wxString strval = GetValue();
	int len = strval.Len();
	int i;
	
	wxUniChar decimsep('.');

	// find start of number (all numbers start like integers or a dot)
	i=0;
	while(i<len && !is_valid_char( m_mode == wxNUMERIC_UNSIGNED ? wxNUMERIC_UNSIGNED : wxNUMERIC_INTEGER,
									strval[i], true, decimsep) )
		i++;
	
	wxUniChar thousep(','); // default thousands separator

	// get all valid number characters
	while(i<len && is_valid_char( m_mode, strval[i], true, thousep ))
	{
		if ( strval[i]!=thousep ) buf += strval[i];
		i++;
	}

	if ( m_mode == wxNUMERIC_INTEGER ) SetValue( (int)wxAtoi(buf) );
	else if ( m_mode == wxNUMERIC_UNSIGNED )
	{
		char *pEnd = 0;
		unsigned long long xval = strtoull( buf.c_str(), &pEnd, 10 );
		SetValue( (size_t)xval );
	}
	else
		SetValue( (double) wxAtof(buf) );
}

static void AddThousandsSeparators(wxString& s)
{
    wxChar thousandsSep(',');

    size_t pos = s.find( wxChar('.') );
    if ( pos == wxString::npos )
    {
        // Start grouping at the end of an integer number.
        pos = s.length();
    }

    // End grouping at the beginning of the digits -- there could be at a sign
    // before their start.
    const size_t start = s.find_first_of("0123456789");

    // We currently group digits by 3 independently of the locale. This is not
    // the right thing to do and we should use lconv::grouping (under POSIX)
    // and GetLocaleInfo(LOCALE_SGROUPING) (under MSW) to get information about
    // the correct grouping to use. This is something that needs to be done at
    // wxLocale level first and then used here in the future (TODO).
    const size_t GROUP_LEN = 3;

    while ( pos > start + GROUP_LEN )
    {
        pos -= GROUP_LEN;
        s.insert(pos, thousandsSep);
    }
}

template<typename T> static wxString format_number( T val, wxNumericMode mode, int deci, bool thousep, const wxString &pre, const wxString &post )
{
	wxString buf;


	if ( mode == wxNUMERIC_INTEGER )
	{
		if ( deci == wxNUMERIC_HEXADECIMAL ) buf.Printf( "0x%llx", (unsigned long long)val );
		else
		{
			buf.Printf( "%d", (int)val );
			if ( thousep ) AddThousandsSeparators( buf );
		}
	}
	else if ( mode == wxNUMERIC_UNSIGNED )
	{
		if ( deci == wxNUMERIC_HEXADECIMAL ) buf.Printf( "0x%llx", (unsigned long long)val );
		else
		{
			buf.Printf( "%llu", (unsigned long long)val );
			if ( thousep ) AddThousandsSeparators( buf );
		}
	}
	else
	{
		if ( std::isnan((double)val) ) return "NaN";
		if ( std::isinf((double)val) ) return "Inf";
		if ( deci == wxNUMERIC_GENERIC ) buf.Printf( "%lg", (double)val );
		else if ( deci == wxNUMERIC_EXPONENTIAL ) buf.Printf( "%le", (double)val );
		else
		{
			wxString fmt;
			fmt.Printf( "%%.%dlf", deci );
			buf.Printf( fmt, (double)val );
			if ( thousep ) AddThousandsSeparators( buf );
		}
	}

	return pre + buf + post;
}

wxString wxNumericFormat( double val, wxNumericMode mode, int deci, bool thousep, const wxString &pre, const wxString &post )
{
	return format_number<double>( val, mode, deci, thousep, pre, post );
}

void wxNumericCtrl::DoFormat()
{	
	wxString text;
	if ( m_mode == wxNUMERIC_INTEGER ) text = format_number<int>( m_value.Int, m_mode, m_decimals, m_thouSep, m_preText, m_postText);
	else if ( m_mode == wxNUMERIC_UNSIGNED ) text = format_number<size_t>( m_value.Unsigned, m_mode, m_decimals, m_thouSep, m_preText, m_postText );
	else text = format_number<double>( m_value.Real, m_mode, m_decimals, m_thouSep, m_preText, m_postText );

	ChangeValue( text );
}

void wxNumericCtrl::SetRange( double min, double max )
{
	m_min = min;
	m_max = max;
	if ( m_mode == wxNUMERIC_INTEGER ) SetValue( m_value.Int );
	else if ( m_mode == wxNUMERIC_UNSIGNED ) SetValue( m_value.Unsigned );
	else SetValue( m_value.Real );
}

template<typename T> static void set_clamp( T &val, T newval, double min, double max )
{
	val = (T)newval;

	if ( min != max )
	{
		if ( val < (T)min ) val = (T)min;
		if ( val > (T)max ) val = (T)max;
	}
}

#define DO_SET_VALUE 	if ( m_mode == wxNUMERIC_INTEGER ) set_clamp<int>( m_value.Int, (int)val, m_min, m_max ); \
						else if ( m_mode == wxNUMERIC_UNSIGNED ) set_clamp<size_t>( m_value.Unsigned, (size_t)val, m_min, m_max ); \
						else set_clamp<double>( m_value.Real, (double)val, m_min, m_max ); \
						DoFormat();

void wxNumericCtrl::SetValue( int val )
{
	DO_SET_VALUE
}

void wxNumericCtrl::SetValue( size_t val )
{
	DO_SET_VALUE
}

void wxNumericCtrl::SetValue( double val )
{
	DO_SET_VALUE
}


double wxNumericCtrl::Value() const
{
	return AsDouble();
}

double wxNumericCtrl::AsDouble() const
{
	if ( m_mode == wxNUMERIC_INTEGER ) return (double)m_value.Int;
	else if ( m_mode == wxNUMERIC_UNSIGNED ) return (double)m_value.Unsigned;
	else return m_value.Real;
}

int wxNumericCtrl::AsInteger() const
{
	if ( m_mode == wxNUMERIC_INTEGER ) return m_value.Int;
	else if ( m_mode == wxNUMERIC_UNSIGNED ) return (int)m_value.Unsigned;
	else return (int)m_value.Real;
}

size_t wxNumericCtrl::AsUnsigned() const
{
	if ( m_mode == wxNUMERIC_INTEGER ) return (size_t)m_value.Int;
	else if ( m_mode == wxNUMERIC_UNSIGNED ) return m_value.Unsigned;
	else return (size_t)m_value.Real;
}

void wxNumericCtrl::SetMode( wxNumericMode m )
{
	if ( m == m_mode ) return;

	ValueType old = m_value;

	if ( m_mode == wxNUMERIC_INTEGER )
	{
		if ( m == wxNUMERIC_UNSIGNED ) m_value.Unsigned = (size_t)old.Int;
		else if ( m == wxNUMERIC_REAL ) m_value.Real = (double)old.Int;
	}
	else if ( m_mode == wxNUMERIC_UNSIGNED )
	{
		if ( m == wxNUMERIC_INTEGER ) m_value.Int = (int)old.Unsigned;
		else if ( m == wxNUMERIC_REAL ) m_value.Real = (double)old.Unsigned;
	}
	else 
	{
		if ( m == wxNUMERIC_INTEGER ) m_value.Int = (int)old.Real;
		else if ( m == wxNUMERIC_UNSIGNED ) m_value.Unsigned = (size_t)old.Real;
	}

	m_mode = m;
	
	SetupValidator();
	DoFormat();
}


void wxNumericCtrl::SetFormat( int decimals,
		bool thousands_sep,
		const wxString &pre,
		const wxString &post )
{
	m_decimals = decimals;
	m_thouSep = thousands_sep;
	m_preText = pre;
	m_postText = post;

	DoFormat();
}
