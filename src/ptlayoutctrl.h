/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (“Alliance”) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as “System Advisor Model” or “SAM”. Except
*  to comply with the foregoing, the terms “System Advisor Model”, “SAM”, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

#ifndef __ptlayoutctrl_h
#define __ptlayoutctrl_h


#include <wx/wx.h>
#include <wx/listctrl.h>
#include <wx/splitter.h>
#include <wx/aui/auibook.h>
#include <wx/imaglist.h>

#include <wex/extgrid.h>
class wxNumericCtrl;

#include "object.h"


BEGIN_DECLARE_EVENT_TYPES()
	DECLARE_EVENT_TYPE( wxEVT_PTLAYOUT_CHANGE, 0)
END_DECLARE_EVENT_TYPES()

class PTLayoutRenderer;

#define EVT_PTLAYOUT(id, func) EVT_COMMAND(id, wxEVT_PTLAYOUT_CHANGE, func)

class PTLayoutCtrl : public wxPanel
{
friend class PTLayoutRenderer;
public:
	PTLayoutCtrl(wxWindow *parent, int id, const wxPoint &pos = wxDefaultPosition, const wxSize &sz = wxDefaultSize);
	virtual ~PTLayoutCtrl();
	
	float NumHeliostats();

	
	void SetGrid( const matrix_t<float> &data );
	matrix_t<float> GetGrid() { return m_data; }

	size_t NRows() { return m_data.nrows(); }
	size_t NCols() { return m_data.ncols(); }

	void EnableSpanAngle(bool b);
	bool IsSpanAngleEnabled() { return m_spanAngleEnabled; }
	void SetSpanAngle(float a);
	float GetSpanAngle() { return m_spanAngle; }

	bool IsXY() { return m_data.ncols() == 2; }
	bool IsZonal() { return m_data.ncols() > 2; } 

private:
	
	void FixDimensions(size_t &nr, size_t &nc);
	void DispatchEvent();
	void UpdateData();
	void ResizeGrid(size_t nrows, size_t ncols);

	void OnSpanAngleChange(wxCommandEvent &evt);
	void OnGridSizeChange(wxCommandEvent &evt);
	void OnGridCellChange(wxGridEvent &evt);
	void OnGridCellSelect(wxGridEvent &evt);

	void OnButton( wxCommandEvent & );

	matrix_t<float> m_data;
	float m_spanAngle;
	bool m_spanAngleEnabled;

	PTLayoutRenderer *m_renderer;

	wxGrid *m_grid;
	wxNumericCtrl *m_numRows;
	wxNumericCtrl *m_numCols;
	wxNumericCtrl *m_numSpan;
	wxStaticText *m_lblRows;
	wxStaticText *m_lblCols;
	wxStaticText *m_lblSpan;

	DECLARE_EVENT_TABLE()
};

class PTLayoutRenderer : public wxWindow
{
public:
	PTLayoutRenderer(PTLayoutCtrl *parent);

	void OnPaint(wxPaintEvent &evt);
	void OnResize(wxSizeEvent &evt);
	void OnChar(wxKeyEvent &evt);
	void OnMouseDown(wxMouseEvent &evt);
	void OnMouseUp(wxMouseEvent &evt);
	void OnMouseMove(wxMouseEvent &evt);

	wxColour Colour1, Colour2;

	void Highlight(int rad, int azm);	
	void ComputeColour(wxColour &c, int cntrIndex, int ncv);
	void DrawZonal(wxDC &dc, const wxRect &geom);
	void DrawXY(wxDC &dc, const wxRect &geom);
	
private:
	int hlRad, hlAzm;
	bool bMouseDown;
	PTLayoutCtrl *mPTCtrl;

	DECLARE_EVENT_TABLE()


};

#endif
