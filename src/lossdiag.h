#ifndef __lossdiag_h
#define __lossdiag_h

#include <wex/pagelayout.h>

#include "reports.h"
#include "case.h"

class Case;
class Simulation;

class LossDiagramObject : public wxPageObject, public SamReportObject
{
public:
	LossDiagramObject();
	LossDiagramObject( bool from_case, bool set_scale );

	virtual wxString TypeName() { return "SamLossDiagramObject"; }
	virtual wxString Description()  { return "Loss Diagram"; }
	virtual wxPageObject *Duplicate();
	virtual bool Copy( wxPageObject *obj );
	virtual bool EditObject( wxPageLayoutCtrl *layout_window ); /* should return true if object was modified, false otherwise */
	virtual void Render( wxPageOutputDevice & );
	virtual bool ReadData( wxInputStream &is );
	virtual bool WriteData( wxOutputStream &os );	
	
	virtual void SetCaseName( const wxString &c ); // also recreates the loss diagram from the case

	void Configure( bool from_case, bool set_scale );
	bool SetupFromCase();
	void Clear();
	void NewBaseline( double value, const wxString &text );
	void AddLossTerm( double percent, const wxString &text );
	size_t Size() const;

	wxRealPoint EstimateSize( double height_char ) const;

protected:
	struct ld_item {
		bool baseline;
		double value;
		wxString text;
	};

	bool m_scaleToGeometry;
	bool m_createFromCase;
	std::vector<ld_item> m_list;

};

class LossDiagCallbackContext : public CaseCallbackContext
{
private:
	LossDiagramObject *m_lossDiag;
	Simulation *m_sim;
public:
	LossDiagCallbackContext( Case *c, 
		Simulation *sim, 
		LossDiagramObject *ld, 
		const wxString &desc );

	LossDiagramObject &GetDiagram();
	Simulation &GetSimulation();
	
protected:
	virtual void SetupLibraries( lk::env_t *env );
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
	
	virtual wxSize DoGetBestSize() const;
	wxBitmap GetBitmap();

	void OnRightDown( wxMouseEvent & );
	void OnContextMenu( wxCommandEvent & );

protected:
	void OnSize( wxSizeEvent & );
	void OnPaint( wxPaintEvent & );

	DECLARE_EVENT_TABLE();
};



#endif
