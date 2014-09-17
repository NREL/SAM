#ifndef __lossdiag_h
#define __lossdiag_h

#include <wex/pagelayout.h>
#include "reports.h"


class LossDiagramObject : public wxPageObject, public SamReportObject
{
public:
	LossDiagramObject();	

	virtual wxString TypeName() { return "SamLossDiagramObject"; }
	virtual wxString Description()  { return "Loss Diagram"; }
	virtual wxPageObject *Duplicate();
	virtual bool Copy( wxPageObject *obj );
	virtual bool EditObject( wxPageLayoutCtrl *layout_window ); /* should return true if object was modified, false otherwise */
	virtual void Render( wxPageOutputDevice & );
	virtual bool ReadData( wxInputStream &is );
	virtual bool WriteData( wxOutputStream &os );	
	
	void Configure( bool from_case );
	void SetupFromCase();
	void Clear();
	void NewBaseline( double value, const wxString &text );
	void AddLossTerm( double percent, const wxString &text );

protected:
	struct ld_item {
		bool baseline;
		double value;
		wxString text;
	};

	bool m_createFromCase;
	std::vector<ld_item> m_list;

};


class LossDiagramCtrl : public wxWindow, public wxPageScaleInterface
{
	float m_ppi;
	LossDiagramObject m_lossDiagram;

public:
	LossDiagramCtrl( wxWindow *parent );
	LossDiagramObject &GetDiagram() { return m_lossDiagram; }
	
	virtual float GetPPI() { return m_ppi; }
	virtual void PageToScreen( float x, float y, int *px, int *py );
	virtual void ScreenToPage( int px, int py, float *x, float *y );

protected:
	void OnSize( wxSizeEvent & );
	void OnPaint( wxPaintEvent & );

	DECLARE_EVENT_TABLE();
};



#endif
