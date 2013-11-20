#include <algorithm>

#include <wx/wx.h>
#include <wx/dcbuffer.h>

#include <wex/metro.h>

#include "main.h"
#include "ipagelist.h"
#include "casewin.h"

#include "../resource/notes.cpng"

#define SCRL_RATE 25

#ifndef MAX
#define MAX(a,b) ((a)<(b)?(b):(a))
#endif

BEGIN_EVENT_TABLE(InputPageList, wxScrolledWindow)
	EVT_SIZE( InputPageList::OnResize )	
	EVT_LEFT_DOWN( InputPageList::OnLeftDown )
	EVT_PAINT( InputPageList::OnPaint )
	EVT_MOTION( InputPageList::OnMouseMove )
	EVT_LEAVE_WINDOW( InputPageList::OnLeave )
	EVT_ERASE_BACKGROUND( InputPageList::OnErase )
END_EVENT_TABLE()

static wxBitmap g_notesBitmap;


InputPageList::InputPageList(wxWindow *parent, int id, const wxPoint &pos, const wxSize &size)
	: wxScrolledWindow(parent,id, pos, size, wxBORDER_NONE)
{
	SetBackgroundStyle(wxBG_STYLE_CUSTOM);
	SetBackgroundColour( *wxWHITE );
	
	if ( !g_notesBitmap.IsOk() )
		g_notesBitmap = wxBITMAP_PNG_FROM_DATA( notes );

	m_caseWin = 0;
	m_selectedIdx = -1;
	m_hoverIdx = -1;
	
}

InputPageList::~InputPageList()
{
	/* nothing to do */
}


void InputPageList::Add(const wxString &item, bool geom_recalc, const wxString &resource)
{
	if (Find(item) >= 0)
		return;
	
	_item x;
	x.name = item;
	x.bitmap = wxNullBitmap;
	x.resource = resource.IsEmpty()?item:resource;

	wxString res_name = (!resource.IsEmpty()?resource:item);
	res_name.Replace("/","");

	wxString fn = SamApp::GetRuntimePath() + "/ipagelist/32x32/" + res_name + ".png";
	if (wxFileExists(fn))
	{
		wxBitmap png( fn, wxBITMAP_TYPE_PNG);
		if (png.IsOk())
			x.bitmap = png;
	}


	FILE *fp = fopen( wxString(SamApp::GetRuntimePath() + "/ipagelist/"+res_name+".txt").c_str(), "r");
	if (fp)
	{
		char buf[256];
		while( fgets( buf, 255, fp ) != 0 )
			x.format.Add( buf );
		fclose(fp);
	}

	m_items.push_back( x );

	if (geom_recalc)
		Invalidate();
}

void InputPageList::SetCaseWindow(CaseWindow *cw)
{
	m_caseWin = cw;
}

void InputPageList::Add(const wxArrayString &item)
{
	for (int i=0;i<(int)item.size();i++)
		Add(item[i], (i==(int)(item.size()-1)) );
}

int InputPageList::Find(const wxString &item)
{
	for (size_t i=0;i<m_items.size();i++)
		if (m_items[i].name == item)
			return i;
	return -1;
}

wxString InputPageList::GetItem(int idx)
{
	if (idx >= 0 && idx < m_items.size())
		return m_items[idx].name;
	else
		return wxEmptyString;
}

wxString InputPageList::GetValue()
{
	return GetItem(m_selectedIdx);
}

void InputPageList::Remove(int idx)
{
	if (idx >= 0 && idx < m_items.size())
	{
		m_items.erase( m_items.begin() + idx );
		Invalidate();
	}
}

void InputPageList::ClearItems()
{
	m_items.clear();
	Invalidate();
}

int InputPageList::Count()
{
	return m_items.size();
}

int InputPageList::FindItem(const wxString &name)
{
	for (int i=0;i<m_items.size();i++)
		if (m_items[i].name == name)
			return i;
	return -1;
}

wxArrayString InputPageList::GetItems()
{
	wxArrayString list;
	for (int i=0;i<m_items.size();i++)
		list.Add( m_items[i].name );

	return list;
}

void InputPageList::Select(int idx)
{
	m_selectedIdx = idx;
	Refresh();
}

int InputPageList::GetSelection()
{
	return m_selectedIdx;
}

wxString InputPageList::GetStringSelection()
{
	if ( m_selectedIdx >= 0 && m_selectedIdx < m_items.size() )
		return m_items[m_selectedIdx].name;
	else
		return wxEmptyString;
}


#define VSPACE 0
#define FMTHEIGHT 16

void InputPageList::Invalidate()
{
	int hpos, vpos;
	GetViewStart( &hpos, &vpos );
	hpos *= SCRL_RATE;
	vpos *= SCRL_RATE;

	wxSize sz = GetClientSize();
	int width,height;
	width=height=0;

	int y = 0;
	for (int i=0;i<m_items.size();i++)
	{
		if (m_items[i].name == "System Summary")
			continue;
		else
		{
			int height = MAX(37, 34+m_items[i].format.size()*FMTHEIGHT);
			m_items[i].geom.x = 0;
			m_items[i].geom.y = y;
			m_items[i].geom.width = sz.GetWidth()+1;
			m_items[i].geom.height = height;

			y += height + VSPACE;
		}
	}

	SetScrollbars(1,1,sz.GetWidth(),y, hpos, vpos);
	SetScrollRate( SCRL_RATE, SCRL_RATE );
	Refresh();
}

void InputPageList::OnResize(wxSizeEvent &evt)
{
	Invalidate();
}

#define SGP_VSPACE 15
#define SGP_XLEFT 15
#define SGP_XRIGHT 5
#define SCRLW 15




// EX: "Inverter Max: !pv.inv.sandia.vmax!%lg! V"
void InputPageList::DrawFormatLine(wxDC &dc, int x, int y, const wxString &fmt)
{
	//if (mSymTab == NULL)
	//{
		dc.DrawText(fmt, x, y);
	//	return;
	//}

	
	int fmtlen = fmt.length();
	int i=0;
	// parse formats and variable lookup
	while(i<fmtlen)
	{
		// write out first section
		wxString buf;
		while (i < fmtlen && fmt[i] != '!')
			buf += fmt[i++];

		dc.DrawText(buf,x,y);
		x += dc.GetTextExtent(buf).GetWidth();
		i++; // skip '!'

		wxString varname;
		// collect the variable name
		while (i < fmtlen && fmt[i] != '!')
			varname += fmt[i++];
		i++; // skip '!'

		wxString varformat;
		// collect the format string
		while (i < fmtlen && fmt[i] != '!')
			varformat += fmt[i++];
		i++; // skip '!'
		
		wxString output;
			/*
		if ( varformat.Left(7) != "special" && !varname.IsEmpty() )
		{
			VarInfo *v = mSymTab->Lookup( varname );
			if (v)
			{
				if (v->GetType() == VAR_INTEGER)
					if ( !v->GetExpression().IsEmpty() ) // choice selection - comma delimited
					{
						wxArrayString as = Split( v->GetExpression(), ",");
						output = as[v->GetInt()];
					}
					else
						output = CommaFormat(varformat.c_str(), v->GetInt());
				else if (v->GetType() == VAR_DOUBLE)
					output = CommaFormat(varformat.c_str(), v->GetDouble());
				else
					output = v->ValToString();
			}
			
		}
		else
		{
			// special format
			wxString special = varformat.Mid(8);
			//wxLogStatus("SPECIAL('%s')\n", special.c_str());
			
			if (special == "taxincentives")
			{
				// list any enabled tax credits
				wxArrayString txc_enabled;

				if (mSymTab->AreAnyNonZero("txc.itc.fed.amount.value,txc.itc.fed.percentage.value"))
					txc_enabled.Add("Fed. ITC");

				if (mSymTab->AreAnyNonZero("txc.itc.state.amount.value,txc.itc.state.percentage.value"))
					txc_enabled.Add("State ITC");

				if (mSymTab->AreAnyNonZero("txc.ptc.fed.amountperkwh"))
					txc_enabled.Add("Fed. PTC");

				if (mSymTab->AreAnyNonZero("txc.ptc.state.amountperkwh"))
					txc_enabled.Add("State PTC");

				output = Unsplit(txc_enabled, ", ");

				if ( output.IsEmpty() ) output = "No tax credits";

			}

			else if (special == "depreciation")
			{
				
				if ( m_caseWin && m_caseWin->GetCase() )
				{
					wxString t,f;
					m_caseWin->GetCase()->GetConfiguration(t, f);

					if ((f == "Leveraged Partnership Flip") || (f == "All Equity Partnership Flip")  || (f == "Sale Leaseback")  || (f == "Single Owner"))
					{
						if ( mSymTab->DVal( "depr.alloc.none" ) < 100.0 )
						{
							output = "Depreciation allocations defined";
						}
						else 
						{
							output = "No depreciation";
						}
					}
				}
			}

			else if (special == "depreciationstate")
			{
				
				if ( m_caseWin && m_caseWin->GetCase() )
				{
					wxString t,f;
					m_caseWin->GetCase()->GetConfiguration(t, f);

					if ((f == "Independent Power Producer") || (f == "Commercial PPA") || (f == "Commercial") )
					{
						if (mSymTab->IVal( "depreciation.state.type" ) > 0 )
						{
							VarInfo *v = mSymTab->Lookup("depreciation.state.type");
							if (v)
							{
								wxArrayString ary = Split(v->GetExpression(),",");
								if (ary.size() > v->GetInt())
									output = ary[v->GetInt()] + " (State)";
							}
						}
						else if (f != "Residential")
						{
							output += "\nNo depreciation (State)";
						}
					}
				}
			}

			else if (special == "depreciationfederal")
			{
				
				if ( m_caseWin && m_caseWin->GetCase() )
				{
					wxString t,f;
					m_caseWin->GetCase()->GetConfiguration(t, f);

					if ((f == "Independent Power Producer") || (f == "Commercial PPA") || (f == "Commercial") )
					{
						if (mSymTab->IVal( "depreciation.fed.type" ) > 0 )
						{
							VarInfo *v = mSymTab->Lookup("depreciation.fed.type");
							if (v)
							{
								wxArrayString ary = Split(v->GetExpression(),",");
								if (ary.size() > v->GetInt())
									output = ary[v->GetInt()] + " (Federal)";
							}
						}
						else if (f != "Residential")
						{
							output = "No depreciation (Federal)";
						}
					}
				}
			}


			else if (special == "cashincentives")
			{
				// list any enabled incentives
				wxArrayString incen_enabled;

				if (mSymTab->AreAnyNonZero("incen.ibi.fed.amount.value,incen.ibi.state.amount.value,incen.ibi.utility.amount.value,incen.ibi.other.amount.value,incen.ibi.fed.percentage.value,incen.ibi.state.percentage.value,incen.ibi.utility.percentage.value,incen.ibi.other.percentage.value"))
					incen_enabled.Add("IBI");
				if (mSymTab->AreAnyNonZero("incen.cbi.fed.value,incen.cbi.state.value,incen.cbi.utility.value,incen.cbi.other.value"))
					incen_enabled.Add("CBI");
				if (mSymTab->AreAnyNonZero("incen.pbi.fed.amountperkwh,incen.pbi.state.amountperkwh,incen.pbi.utility.amountperkwh,incen.pbi.other.amountperkwh"))
					incen_enabled.Add("PBI");

				output = Unsplit(incen_enabled, ", ");

				if ( output.IsEmpty() ) output = "No cash incentives";
			}
			else if (special == "utilityrate")
			{
				int net_metering = mSymTab->IVal("ur.buy_eq_sell");
				if ( net_metering == 1 )
					output = "Net Metering? Yes";
				else
					output = "Net Metering? No";
			}
			else if (special == "timeofdelivery")
			{
				wxString disp_sched = LibQueryForMatchWithInputs( "EnergyPaymentDispatch", mSymTab, "" );

				if ( !disp_sched.IsEmpty() )
				{
					wxArrayString as = Split( disp_sched, "/" );
					if ( as.size() > 0 )
						output = as[ as.size() - 1];
					if ( output == "Uniform Dispatch" )
						output = "None";
				}
				else
					output = "Custom";
			}
			else if (special == "invertername")
			{
				int inv_type = mSymTab->IVal( "pv.inv.model_type" );

				if ( inv_type == 1 ) // Datasheet Data
				{
					output = "Inverter Datasheet";
				}
				else if ( inv_type == 2 ) // Partload Curve
				{
					output = "Inverter Part Load Curve";
				}
				else //if ( inv_type == 0 ) // CEC
				{
					wxString inv_name = mSymTab->SVal( "pv.inv.sandia.inverter_name" );

					if ( !inv_name.IsEmpty() )
					{
						wxArrayString as = Split( inv_name, "/" );
						if ( as.size() > 0 )
						{
							output = as[ as.size() - 1];
							if ( !output.IsEmpty() ) // remove "cec[nnnn]" postfix
							{
								as = Split( output, "[" );
								if ( as.size() > 0 )
									output = as[ 0 ];
							}
						}
					}
				}
			}
			else if (special == "modulename")
			{
				int mod_type = mSymTab->IVal( "pv.mod.model_type" );
				if ( mod_type == 0 )
				{
					output = "Simple Efficiency Module Model";
				}
				else if ( mod_type == 1 )
				{
					wxString mod_name = mSymTab->SVal( "pv.mod.cec.module_name" );

					if ( !mod_name.IsEmpty() )
					{
						wxArrayString as = Split( mod_name, "/" );
						if ( as.size() > 0 )
							output = as[ as.size() - 1];
						if ( !output.IsEmpty() ) 
						{
							as = Split( output, "[" );
							if ( as.size() > 0 )
								output = as[ 0 ];
						}
					}
				}
				else if ( mod_type == 2 )
				{
					output = "User Entered Specifications";
				}
				else if ( mod_type == 3 )
				{
					wxString inv_name = mSymTab->SVal( "pv.mod.sandia.module_name" );

					if ( !inv_name.IsEmpty() )
					{
						wxArrayString as = Split( inv_name, "/" );
						if ( as.size() > 0 )
							output = as[ as.size() - 1];
						if ( !output.IsEmpty() )
						{
							as = Split( output, "[" );
							if ( as.size() > 0 )
								output = as[ 0 ];
						}
					}
				}
			}
			else if (special == "windresource")
			{
				int wind_reource_type = mSymTab->IVal("wind_climate.model_choice");
				if ( wind_reource_type == 0 ) // location
				{
					double speed = mSymTab->DVal("wind_climate.avg_wind_speed_closest_to_hub_ht");
					double height = mSymTab->DVal("wind_climate.windspeed_height");
					output = Format("Average annual wind speed: %.1f m/s at %.1f m", speed, height);
				}
				else if ( wind_reource_type == 1 )// characteristics
				{
					double speed = mSymTab->DVal("wind.resource.class");
					double weibullK = mSymTab->DVal("wind.resource.weibullK");
					output = Format("Average annual wind speed: %.1f m/s, Weibull K: %.1f", speed, weibullK);
				}
			}
			else if (special == "annualperformanceavailability")
			{
				int count;
				double *darr = mSymTab->DAVal("system.availability", &count);
				if (count == 1)
					output = Format("Percent of annual output: %lg %%", darr[0]);
				else if (count == 2)
						output = Format("Percent of annual output: %lg, %lg %%", darr[0], darr[1]);
				else if (count == 3)
						output = Format("Percent of annual output: %lg, %lg, %lg %%", darr[0], darr[1], darr[2]);
				else if (count == 4)
						output = Format("Percent of annual output: %lg, %lg, %lg, %lg %%", darr[0], darr[1], darr[2], darr[3]);
				else if (count == 5)
						output = Format("Percent of annual output: %lg, %lg, %lg, %lg, %lg %%", darr[0], darr[1], darr[2], darr[3], darr[4]);
				else if (count > 5)
						output = Format("Percent of annual output: %lg, %lg, %lg, %lg, %lg, ... %%", darr[0], darr[1], darr[2], darr[3], darr[4]);
			}
			else if (special == "annualperformancedegradation")
			{
				int count;
				double *darr = mSymTab->DAVal("system.degradation", &count);
				if (count == 1)
					output = Format("Year-to-year decline: %lg %% per year", darr[0]);
				else if (count == 2)
						output = Format("Year-to-year decline: %lg, %lg %% per year", darr[0], darr[1]);
				else if (count == 3)
						output = Format("Year-to-year decline: %lg, %lg, %lg %% per year", darr[0], darr[1], darr[2]);
				else if (count == 4)
						output = Format("Year-to-year decline: %lg, %lg, %lg, %lg %% per year", darr[0], darr[1], darr[2], darr[3]);
				else if (count == 5)
						output = Format("Year-to-year decline: %lg, %lg, %lg, %lg, %lg %% per year", darr[0], darr[1], darr[2], darr[3], darr[4]);
				else if (count > 5)
						output = Format("Year-to-year decline: %lg, %lg, %lg, %lg, %lg, ... %% per year", darr[0], darr[1], darr[2], darr[3], darr[4]);
			}
		}
		*/

		if ( !output.IsEmpty() )
		{
			dc.DrawText(output,x,y);
			x += dc.GetTextExtent(output).GetWidth();
		}
	}
}

static wxColour HighlightColour(231,231,231);
static wxColour SelectColour(212,212,212);
static wxColour BackColour(243,243,243);

// outlook 'metro' grayscales
//static wxColour SelectColour = wxColour(212,212,212);
//static wxColour HighlightColour = wxColour(231,231,231);

#define TXTXOFF 4

void InputPageList::DrawItem( wxDC &dc, int i, bool with_separator )
{
	if (i < 0 || i >= m_items.size())
		return;

	wxRect r = m_items[i].geom;

	wxColour c( GetBackgroundColour() );
	if(i==m_hoverIdx) c=HighlightColour;
	if(i==m_selectedIdx) c=SelectColour;

	dc.SetClippingRegion( r );

	dc.SetPen( wxPen(c) );
	dc.SetBrush( wxBrush(c, wxSOLID) );
	dc.DrawRectangle( r.x, r.y, r.width, r.height);

	dc.SetTextForeground( *wxBLACK );
	dc.SetFont( wxMetroTheme::Font( wxMT_LIGHT, 13 ));
	dc.DrawText( m_items[i].name, r.x+TXTXOFF, r.y+TXTXOFF );
	dc.SetBackground( wxBrush( GetBackgroundColour() ) );

	if (g_notesBitmap.IsOk() /*&& m_caseWin 
		&& m_caseWin->HasPageNote( m_items[i].resource )*/)
	{
		int tx_w = dc.GetTextExtent( m_items[i].name ).GetWidth();
		dc.DrawBitmap(g_notesBitmap, r.x+TXTXOFF+tx_w+4,r.y+TXTXOFF +dc.GetCharHeight()/2-g_notesBitmap.GetHeight()/2);
	}

	dc.SetFont( wxMetroTheme::Font( wxMT_LIGHT, 9 ) );
	dc.SetTextForeground( wxColour(140,140,140) );

	for (int k=0;k<(int)m_items[i].format.size();k++)		
		DrawFormatLine(dc, r.x+TXTXOFF, r.y+2+30+k*FMTHEIGHT, m_items[i].format[k]);

	if (!m_items[i].bitmap.IsNull())
		dc.DrawBitmap( m_items[i].bitmap, r.x+r.width-3-m_items[i].bitmap.GetWidth(), r.y+TXTXOFF);

	
	if ( with_separator )
	{
		dc.SetPen( wxPen(HighlightColour, 1) );
		dc.DrawLine( r.x, r.y, r.x+r.width, r.y );
	}
	

	dc.DestroyClippingRegion();
}

void InputPageList::OnPaint(wxPaintEvent &evt)
{
	wxAutoBufferedPaintDC dc(this);
	DoPrepareDC( dc );
	
	wxColour bg = GetBackgroundColour();
	dc.SetBrush(wxBrush(bg));
	dc.SetPen(wxPen(bg,1));
	wxRect windowRect( wxPoint(0,0), GetClientSize() );
	CalcUnscrolledPosition(windowRect.x, windowRect.y,
		&windowRect.x, &windowRect.y);
	dc.DrawRectangle(windowRect);

	for (int i=0;i<m_items.size();i++)	
		DrawItem(dc, i, i>0);
}

void InputPageList::OnErase( wxEraseEvent & )
{
	/* nothing to do */
}

void InputPageList::OnLeftDown(wxMouseEvent &evt)
{
	int vsx, vsy;
	GetViewStart( &vsx, &vsy );
	vsx *= SCRL_RATE;
	vsy *= SCRL_RATE;

	SetFocus();

	for (int i=0;i<m_items.size();i++)
	{
		if (evt.GetY()+vsy > m_items[i].geom.y 
			&& evt.GetY()+vsy < m_items[i].geom.y+m_items[i].geom.height )
		{
			
			m_selectedIdx = i;
			Refresh();
				
			wxCommandEvent selevt(wxEVT_COMMAND_LISTBOX_SELECTED, this->GetId() );
			selevt.SetEventObject(this);
			selevt.SetInt(i);
			selevt.SetString(GetValue());
			GetEventHandler()->ProcessEvent(selevt);
			return;
		}
	}
}

void InputPageList::OnMouseMove(wxMouseEvent &evt)
{	
	int vsx, vsy;
	GetViewStart( &vsx, &vsy );
	vsx *= SCRL_RATE;
	vsy *= SCRL_RATE;

	for (int i=0;i<m_items.size();i++)
	{
		if (evt.GetY()+vsy > m_items[i].geom.y 
			&& evt.GetY()+vsy < m_items[i].geom.y+m_items[i].geom.height
			&& m_hoverIdx != i )
		{
			
			m_hoverIdx = i;
			Refresh();
			return;
		}
	}
}

void InputPageList::OnLeave(wxMouseEvent &evt)
{
	m_hoverIdx = -1;
	Refresh();
}

