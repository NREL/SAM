/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef __inputpagelist_h
#define __inputpagelist_h

#include <wx/bitmap.h>
#include <wx/scrolwin.h>

#include <vector>
class CaseWindow;

class InputPageList : public wxScrolledWindow
{
public:

	InputPageList( wxWindow *parent, int id=-1, 
		const wxPoint &pos=wxDefaultPosition, const wxSize &size=wxDefaultSize );
	virtual ~InputPageList();

	void Add(const wxString &item, bool geom_recalc=true, const wxString &resource="");
	void Add(const wxArrayString &item);
	int Find(const wxString &item);
	wxString GetItem(int idx);
	wxString GetValue();
	void Remove(int idx);
	void ClearItems();
	void Select(int idx);
	int GetSelection();
	wxString GetStringSelection();
	void RedrawItem(int idx);
	void SetCaseWindow(CaseWindow *cw);
	int FindItem(const wxString &name);
	wxArrayString GetItems();
	int Count();

	void Invalidate();
private:
	CaseWindow *m_caseWin;

	struct _item
	{
		wxString name;
		wxRect geom;
		wxString resource;
	};

	std::vector<_item> m_items;
	
	int m_selectedIdx;
	int m_hoverIdx;

	void OnResize(wxSizeEvent &evt);
	void OnLeftDown(wxMouseEvent &evt);
	void OnPaint(wxPaintEvent &evt);
	void OnErase(wxEraseEvent & );
	void OnMouseMove(wxMouseEvent &evt);
	void OnLeave(wxMouseEvent &evt);

	DECLARE_EVENT_TABLE()
};


#endif

