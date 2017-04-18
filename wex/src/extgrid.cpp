
#include <wx/clipbrd.h>
#include <wx/dcbuffer.h>
#include <wx/tokenzr.h>
#include <wx/busyinfo.h>

#include "wex/csv.h"
#include "wex/extgrid.h"

wxExtGridCellAttrProvider::wxExtGridCellAttrProvider(bool highlight_r0, bool hide_00, bool highlight_c0)
{
	m_attrForOddRows = new wxGridCellAttr;
	m_attrForOddRows->SetBackgroundColour(wxColour(240, 240, 240));
	m_attrRow0 = NULL;
	if (highlight_r0)
	{
		m_attrRow0 = new wxGridCellAttr;
//		m_attrRow0->SetBackgroundColour(wxColour("SLATE BLUE"));
//		m_attrRow0->SetTextColour(*wxWHITE);
//		m_attrRow0->SetFont(*wxNORMAL_FONT);
		m_attrRow0->SetBackgroundColour(wxColour(240, 240, 240));
		m_attrRow0->SetTextColour(*wxBLACK);
		wxFont f = wxFont(*wxNORMAL_FONT);
		f.MakeBold();
		m_attrRow0->SetFont(f);
	}

	m_attrCol0 = NULL;
	if (highlight_c0)
	{
		m_attrCol0 = new wxGridCellAttr;
//		m_attrCol0->SetBackgroundColour(wxColour("SLATE BLUE"));
//		m_attrCol0->SetTextColour(*wxWHITE);
//		m_attrCol0->SetFont(*wxNORMAL_FONT);
		m_attrCol0->SetBackgroundColour(wxColour(240, 240, 240));
		m_attrCol0->SetTextColour(*wxBLACK);
		wxFont f = wxFont(*wxNORMAL_FONT);
		f.MakeBold();
		m_attrCol0->SetFont(f);
	}

	m_attrCell00 = NULL;
	if (hide_00)
	{
		m_attrCell00 = new wxGridCellAttr;
//		m_attrCell00->SetBackgroundColour(wxColour("SLATE BLUE"));
//		m_attrCell00->SetTextColour(wxColour("SLATE BLUE"));
		m_attrCell00->SetBackgroundColour(wxColour(240, 240, 240));
		m_attrCell00->SetTextColour(wxColour(240, 240, 240));
	}

}

wxExtGridCellAttrProvider::~wxExtGridCellAttrProvider()
{
    m_attrForOddRows->DecRef();
	if (m_attrRow0)
		m_attrRow0->DecRef();
	if (m_attrCell00)
		m_attrCell00->DecRef();
	if (m_attrCol0)
		m_attrCol0->DecRef();
}

wxGridCellAttr *wxExtGridCellAttrProvider::GetAttr(int row, int col,
                           wxGridCellAttr::wxAttrKind  kind /* = wxGridCellAttr::Any */) const
{
    wxGridCellAttr *attr = wxGridCellAttrProvider::GetAttr(row, col, kind);

	if ( m_attrCell00 && row==0 && col==0 )
    {
        if ( !attr )
        {
            attr = m_attrCell00;
            attr->IncRef();
        }
        else
        {
            if ( !attr->HasBackgroundColour() )
            {
                wxGridCellAttr *attrNew = attr->Clone();
                attr->DecRef();
                attr = attrNew;
				//				attr->SetBackgroundColour(wxColour("SLATE BLUE"));
				//				attr->SetTextColour(wxColour("SLATE BLUE"));
				attr->SetBackgroundColour(wxColour(240, 240, 240));
				attr->SetTextColour(wxColour(240, 240, 240));
			}
        }

		return attr;
    }


	if (m_attrRow0 && row == 0 )
		{
        if ( !attr )
        {
            attr = m_attrRow0;
            attr->IncRef();
        }
        else
        {
            if ( !attr->HasBackgroundColour() )
            {
                wxGridCellAttr *attrNew = attr->Clone();
                attr->DecRef();
                attr = attrNew;
				//attr->SetBackgroundColour(wxColour("SLATE BLUE"));
				//attr->SetTextColour(*wxWHITE);
				//attr->SetFont( *wxNORMAL_FONT );
				attr->SetBackgroundColour(wxColour(240, 240, 240));
				attr->SetTextColour(*wxBLACK);
				wxFont f = wxFont(*wxNORMAL_FONT);
				f.MakeBold();
				attr->SetFont(f);
            }
        }

		return attr;
    }

	if (m_attrCol0 && col == 0 )
	{
		if (!attr)
		{
			attr = m_attrCol0;
			attr->IncRef();
		}
		else
		{
			if (!attr->HasBackgroundColour())
			{
				wxGridCellAttr *attrNew = attr->Clone();
				attr->DecRef();
				attr = attrNew;
				//attr->SetBackgroundColour(wxColour("SLATE BLUE"));
				//attr->SetTextColour(*wxWHITE);
				//attr->SetFont(*wxNORMAL_FONT);
				attr->SetBackgroundColour(wxColour(240, 240, 240));
				attr->SetTextColour(*wxBLACK);
				wxFont f = wxFont(*wxNORMAL_FONT);
				f.MakeBold();
				attr->SetFont(f);
			}
		}

		return attr;
	}

    if ( row % 2 )
    {
        if ( !attr )
        {
            attr = m_attrForOddRows;
            attr->IncRef();
        }
        else
        {
            if ( !attr->HasBackgroundColour() )
            {
                wxGridCellAttr *attrNew = attr->Clone();
                attr->DecRef();
                attr = attrNew;
                attr->SetBackgroundColour(*wxLIGHT_GREY);
            }
        }
    }

    return attr;
}


BEGIN_EVENT_TABLE(wxExtGridCtrl, wxGrid)
	EVT_GRID_CELL_CHANGED( wxExtGridCtrl::OnGridCellChange)
	EVT_GRID_SELECT_CELL( wxExtGridCtrl::OnGridCellSelect)
	EVT_GRID_RANGE_SELECT( wxExtGridCtrl::OnGridRangeSelect)
	EVT_GRID_EDITOR_HIDDEN( wxExtGridCtrl::OnGridEditorHidden)
	EVT_GRID_EDITOR_SHOWN( wxExtGridCtrl::OnGridEditorShown)
	EVT_GRID_LABEL_LEFT_CLICK( wxExtGridCtrl::OnGridLabelClick )
	EVT_KEY_DOWN( wxExtGridCtrl::OnGridKey )
END_EVENT_TABLE()

wxExtGridCtrl::wxExtGridCtrl(wxWindow *parent, int id, 
	const wxPoint &pos, const wxSize &sz)
	: wxGrid(parent, id, pos, sz)
{
	m_sendPasteEvent = true;
	m_enableCopyPaste = true;
	m_skipSelect = false;

	m_selTopRow = m_selBottomRow = -1;
	m_selLeftCol = m_selRightCol = -1;

	m_lastSelTopRow = -1;
	m_lastSelBottomRow = -1;
	m_lastSelLeftCol = -1;
	m_lastSelRightCol = -1;
}

void wxExtGridCtrl::EnablePasteEvent(bool b)
{
	m_sendPasteEvent = b;
}

void wxExtGridCtrl::GetSelRange(int *top, int *bottom, int *left, int *right)
{
	if (top) *top = m_selTopRow;
	if (bottom) *bottom = m_selBottomRow;
	if (left) *left = m_selLeftCol;
	if (right) *right = m_selRightCol;
}

void wxExtGridCtrl::GetLastSelRange(int *top, int *bottom, int *left, int *right)
{
	if (top) *top = m_lastSelTopRow;
	if (bottom) *bottom = m_lastSelBottomRow;
	if (left) *left = m_lastSelLeftCol;
	if (right) *right = m_lastSelRightCol;
}

size_t wxExtGridCtrl::NumCellsSelected() const
{
	int nr = abs(m_selBottomRow - m_selTopRow + 1);
	int nc = abs(m_selRightCol -  m_selLeftCol + 1);
	if ( nr < 0 ) nr = 0;
	if ( nc < 0 ) nc = 0;
	return (size_t)( nr * nc );
}

void wxExtGridCtrl::OnGridKey(wxKeyEvent &evt)
{
	if (m_enableCopyPaste && evt.CmdDown())
	{
		int key = tolower(evt.GetKeyCode());
		if (key=='c')
			Copy();
		else if (key=='v')
			Paste();
		else
			evt.Skip();
	}
	else
		evt.Skip();
}

void wxExtGridCtrl::OnGridLabelClick(wxGridEvent &evt)
{
	int col = evt.GetCol();

	m_selLeftCol = col;
	m_selRightCol = col;
	m_selTopRow = 0;
	m_selBottomRow = GetNumberRows()-1;

	if ( m_enableCopyPaste ) 
	{
		SetFocus();
		if ( col >= 0 )
			SetGridCursor( 0, col );
	}

	evt.Skip();
}

void wxExtGridCtrl::OnGridCellChange(wxGridEvent &evt)
{	
	m_lastSelTopRow = m_selTopRow;
	m_lastSelBottomRow = m_selBottomRow;
	m_lastSelLeftCol = m_selLeftCol;
	m_lastSelRightCol = m_selRightCol;

	m_selTopRow = -1;
	m_selBottomRow = -1;
	m_selLeftCol = -1;
	m_selRightCol = -1;

	if (m_enableCopyPaste && evt.GetRow() >= 0 && evt.GetCol() >= 0 && !m_skipSelect )
	{
		wxString cell = GetCellValue(evt.GetRow(),evt.GetCol());
		cell.Replace("\n", "");
		cell.Replace("\t", "");
		SetCellValue(evt.GetRow(), evt.GetCol(), cell );
	}

	evt.Skip();
}
void wxExtGridCtrl::ResetLastSelRange()
{
	m_lastSelTopRow = -1;
	m_lastSelBottomRow = -1;
	m_lastSelLeftCol = -1;
	m_lastSelRightCol = -1;
}

void wxExtGridCtrl::OnGridCellSelect(wxGridEvent &evt)
{
	if (evt.CmdDown())
	{
		evt.Veto();
		return;
	}

	if ( evt.Selecting() && !m_skipSelect )
	{
		m_selTopRow = evt.GetRow();
		m_selBottomRow = m_selTopRow;
		m_selLeftCol = evt.GetCol();
		m_selRightCol = m_selLeftCol;
	}

	evt.Skip();
}

void wxExtGridCtrl::OnGridEditorHidden(wxGridEvent &evt)
{
	m_skipSelect = false;
	evt.Skip();
}

void wxExtGridCtrl::OnGridEditorShown(wxGridEvent &evt)
{
	m_skipSelect = true;
	evt.Skip();
}

void wxExtGridCtrl::OnGridRangeSelect(wxGridRangeSelectEvent &evt)
{
	if (evt.CmdDown())
	{
		evt.Veto();
		return;
	}

	if (evt.Selecting() && !m_skipSelect)
	{
		m_selTopRow = evt.GetTopRow();
		m_selBottomRow = evt.GetBottomRow();
		m_selLeftCol = evt.GetLeftCol();
		m_selRightCol = evt.GetRightCol();
	}

	evt.Skip();
}

void wxExtGridCtrl::EnableCopyPaste(bool b)
{
	m_enableCopyPaste = b;
}

bool wxExtGridCtrl::IsCopyPasteEnabled()
{
	return m_enableCopyPaste;
}

#define BUSY_DELAY_MS 75

void wxExtGridCtrl::Copy( bool all, bool with_headers )
{
	wxBusyInfo info( "Copying to clipboard...", this );
	wxYield();

	int minrow=m_selTopRow, maxrow=m_selBottomRow;
	int mincol=m_selLeftCol, maxcol=m_selRightCol;

	if ( minrow == maxrow && minrow < 0 )
	{
		minrow = 0;
		maxrow = GetNumberRows()-1;
	}
	
	if ( mincol == mincol && mincol < 0 )
	{
		mincol = 0;
		maxcol = GetNumberCols()-1;
	}
	
	if ( all || m_selTopRow < 0 || m_selLeftCol < 0 )
	{
		minrow = 0;
		maxrow = GetNumberRows()-1;
		mincol = 0;
		maxcol = GetNumberCols()-1;
	}

	wxString data;

	if (with_headers)
	{
		for (int c=mincol;c<=maxcol;c++)
			data += this->GetColLabelValue( c )  + wxString(c==maxcol?'\n':'\t');
	}

	for (int r = minrow;r<=maxrow;r++)
	{
		for (int c= mincol;c<=maxcol;c++)
		{
			wxString cell = GetCellValue(r,c);
			cell.Replace("\n", "");
			cell.Replace("\t", "");
			data += cell + wxString(c==maxcol?'\n':'\t');
		}
	}

	if (wxTheClipboard->Open())
	{
		wxTheClipboard->SetData( new wxTextDataObject(data) );
		wxTheClipboard->Close();

		wxMilliSleep( BUSY_DELAY_MS );
	}
}

void wxExtGridCtrl::Paste( PasteMode mode )
{
	wxBusyInfo info( "Pasting from clipboard...", this );
	wxYield();

	if (wxTheClipboard->Open())
	{
		wxString data;
		wxTextDataObject textobj;
		if (wxTheClipboard->GetData( textobj ))
		{
			data = textobj.GetText();
			wxTheClipboard->Close();
		}
		if (data.IsEmpty()) return;

		int currow = GetGridCursorRow();
		int curcol = GetGridCursorCol();

		if ( mode == PASTE_ALL
			|| mode == PASTE_ALL_RESIZE
			|| mode == PASTE_ALL_RESIZE_ROWS ) currow = curcol = 0;

		wxCSVData csv;
		csv.SetSeparator( wxUniChar( '\t' ) );
		csv.ReadString( data );

		if ( mode == PASTE_ALL_RESIZE )
			ResizeGrid( csv.NumRows(), csv.NumCols() );
		else if ( mode == PASTE_ALL_RESIZE_ROWS )
			ResizeGrid( csv.NumRows(), GetNumberCols() );

		for (size_t i=0;i<csv.NumRows();i++)
		{
			for (size_t p=0;p<csv.NumCols();p++)
			{
				int r = currow + i;
				int c = curcol + p;
				if (r < GetNumberRows() && c < GetNumberCols() && r >=0 && c >= 0 )
				{
					if (!IsReadOnly( r, c ))
					{
						SetCellValue( r, c, csv(i,p) );
					}
				}
			}
		}

		wxMilliSleep( BUSY_DELAY_MS );

		if (m_sendPasteEvent)
		{
			wxGridEvent evt(this->GetId(), ::wxEVT_GRID_CELL_CHANGED, this, -1, -1);
			this->ProcessEvent(evt);
		}
	}
}

void wxExtGridCtrl::ResizeGrid(int nrows, int ncols)
{
	Freeze();

	if (GetNumberRows() > nrows)
		DeleteRows( nrows, GetNumberRows() - nrows );

	if (GetNumberRows() < nrows)
		AppendRows( nrows - GetNumberRows() );

	if (GetNumberCols() > ncols)
		DeleteCols( ncols, GetNumberCols() - ncols );

	if (GetNumberCols() < ncols)
		AppendCols( ncols - GetNumberCols() );

	Thaw();
}
