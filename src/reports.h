#ifndef __reports_h
#define __reports_h

#include <vector>

#include <wx/wx.h>
#include <wx/listctrl.h>
#include <wx/imaglist.h>

#include <wex/pageobjects.h>


#include "object.h"
#include "graph.h"

// forward
class Case;
class wxAuiToolBar;
class wxListCtrl;
class VarTable;
class wxExtTextCtrl;
class wxStaticText;

void RegisterReportObjectTypes();

class SamReportTemplate
{
public:
	SamReportTemplate();
	~SamReportTemplate();

	void Copy( SamReportTemplate *src );
	void SetCaseName( const wxString &cn );

	void AddPage( wxPageLayout *page );
	void DeletePage( wxPageLayout *page );
	std::vector<wxPageLayout*> GetPages();
	wxPageLayout *GetPage( size_t idx );
	int GetPageCount();
	void Clear();

	void SetSpecificModelsOnly( bool b ) { m_specificModelsOnly = b; }
	bool GetSpecificModelsOnly() { return m_specificModelsOnly; }
	void SetInfo( const wxString &desc, const wxString &author, const wxString &meta1, const wxString &meta2 );
	void GetInfo( wxString *desc, wxString *author, wxString *meta1, wxString *meta2 );
	void SetModels( const wxArrayString &techmods, const wxArrayString &finmods );
	void GetModels( wxArrayString *techmods, wxArrayString *finmods );
	void SetHeaderFooter( const wxString &header, const wxString &footer );
	void GetHeaderFooter( wxString *header, wxString *footer );

	bool Write( const wxString &file );
	bool Read( const wxString &file );

	bool RenderPdf( const wxString &file, Case *c = 0 );

protected:

	wxString EscapeHF( wxString hf );

	std::vector< wxPageLayout* > m_pages;
	wxString m_description;
	wxString m_author;
	wxString m_meta1;
	wxString m_meta2;
	bool m_specificModelsOnly;
	wxArrayString m_techModels;
	wxArrayString m_finModels;
	wxString m_header;
	wxString m_footer;
};

class SamReportWindow : public wxPanel
{
public:
	SamReportWindow( wxWindow *parent );
	virtual ~SamReportWindow();
	
	void ClearData();

	bool Save();
	bool SaveAs();

	void OnCaseSel( wxCommandEvent & );
	void OnPageSel( wxListEvent & );
	void OnPageDesel( wxListEvent & );
	void OnPageRClick( wxListEvent & );
	void OnCommand( wxCommandEvent & );
	void OnCreateObject( wxCommandEvent & );
private:

	void OnLayoutCreate( wxPageLayoutEvent & );
	void OnLayoutModify( wxPageLayoutEvent & );
	void OnLayoutSelect( wxPageLayoutEvent & );

	void UpdateCaseList();
	void UpdatePageList();
	void AssignCaseNames();

	SamReportTemplate m_template;

	wxComboBox *m_caseListCtrl;
	wxListView *m_pagesCtrl;
	wxImageList m_imageList;
	wxPageLayoutCtrl *m_layoutCtrl;
	wxStaticText *m_statusText;

	wxString m_fileName;
	bool m_modified;

	void Modified( bool b=true );
	void UpdateStatusText();

	DECLARE_EVENT_TABLE()
};

class SamReportObject
{
private:
	wxString m_caseName;
public:
	virtual void SetCaseName( const wxString &c ) { m_caseName = c; };
	wxString GetCaseName() { return m_caseName; }	
	Case *GetCase();
};



wxString SamReportFormatVariable( double value, const wxString &fmt );
wxString SamReportEscapeString( const wxString &input, Case *c );

class SamReportTextObject : public wxPageTextObject, public SamReportObject
{
public:
	virtual wxString TypeName() { return "SamReportTextObject"; }
	virtual wxString Description() { return "Text/Variable Object"; }
	virtual wxPageObject *Duplicate();
	virtual bool EditObject( wxPageLayoutCtrl * );
	virtual void Render( wxPageOutputDevice &dv );
};

class SamReportGraphObject : public wxPageObject, public SamReportObject
{
public:
	SamReportGraphObject() {  }
	virtual ~SamReportGraphObject() {  }

	virtual void SetCaseName( const wxString &c );
	virtual wxString TypeName() { return "SamReportGraphObject"; }
	virtual wxString Description() { return "Graph Object"; }
	virtual wxPageObject *Duplicate();
	virtual bool Copy( wxPageObject *obj );
	virtual bool EditObject( wxPageLayoutCtrl * );
	virtual void Render( wxPageOutputDevice & );
	virtual bool ReadData( wxInputStream &is );
	virtual bool WriteData( wxOutputStream &os );

protected:
	void UpdateImage();
	wxImage m_imgCache;
	Graph m_gi;
};


/*

class SamReportLossObject : public wxPageObject, public SamReportObject
{
public:
	SamReportLossObject() {  }
	virtual ~SamReportLossObject() {  }

	virtual void SetCaseName( const wxString &c );
	virtual wxString TypeName() { return "SamReportLossObject"; }
	virtual wxString Description() { return "Loss Object"; }
	virtual wxPageObject *Duplicate();
	virtual bool Copy( wxPageObject *obj );
	virtual bool EditObject( wxPageLayoutCtrl * );
	virtual void Render( wxPageOutputDevice & );
	virtual bool ReadData( wxInputStream &is );
	virtual bool WriteData( wxOutputStream &os );

protected:
	void UpdateImage();
	wxImage m_imgCache;
};

*/


class SamReportTableObjectEditDialog;
class SamReportTableObject : public wxPageObject, public SamReportObject
{
friend class SamReportTableObjectEditDialog;
public:
	SamReportTableObject() {  }
	virtual ~SamReportTableObject() {  }

	virtual wxString TypeName() { return "SamReportTableObject"; }
	virtual wxString Description() { return "Table Object"; }
	virtual wxPageObject *Duplicate();
	virtual bool Copy( wxPageObject *obj );
	virtual bool EditObject( wxPageLayoutCtrl * );
	virtual void Render( wxPageOutputDevice & );
	virtual bool ReadData( wxInputStream &is );
	virtual bool WriteData( wxOutputStream &os );

private:
	matrix_t<wxString> m_table;
};

class SamReportScriptObject : public wxPageObject, public SamReportObject
{
public:
	SamReportScriptObject();
	virtual ~SamReportScriptObject();

	virtual wxString TypeName() { return "SamReportScriptObject"; }
	virtual wxString Description() { return "Script Object"; }
	virtual wxPageObject *Duplicate();
	virtual bool Copy( wxPageObject *obj );
	virtual bool EditObject( wxPageLayoutCtrl * );
	virtual void Render( wxPageOutputDevice & );
	virtual bool ReadData( wxInputStream &is );
	virtual bool WriteData( wxOutputStream &os );

	VarTable *GetSymbols();
	wxString GetScript() { return m_script; }
	void SetScript( const wxString &script ) { m_script = script; }

	void Style( int face, int size, const wxColour &c, 
		bool bold, bool ital, int align, float line_width, int line_style );
	void Style( int *face, int *size, wxColour *c, 
		bool *b, bool *it, int *al, float *line_width, int *line_style );
	void TableStyle( int hdrSize, int hdrFace, int hdrAlign, bool hdrBold, const wxColour &hdrColor,
		bool hdrLine, int cellAlign, bool gridLines, const std::vector<float> &rowSizes, const std::vector<float> &colSizes,
		bool tabBorder );
	void TableStyle( int *hdrSize, int *hdrFace, int *hdrAlign, bool *hdrBold, wxColour *hdrColor,
		bool *hdrLine, int *cellAlign, bool *gridLines, std::vector<float> *rowSizes, std::vector<float> *colSizes,
		bool *tabBorder );

	void MoveTo( float x, float y );
	void LineTo( float x, float y );
	void GetCursorPos( float *x, float *y );
	void Measure( const wxString &s, float *w, float *h );

	void RenderText( const wxString &s );
	void RenderTable( const matrix_t<wxString> &tab );
	void RenderBarGraph( const std::vector<double> &values, const wxArrayString & xlabels, const wxString &xlabel,
		const wxString &ylabel, const wxString &title, bool show_values, float xsize, float ysize,
		int decimals,
		const wxColour &color );
private:
	float m_curXPos, m_curYPos, m_curLineHeight;
	wxPageOutputDevice *m_curDevice;

	// text properties
	int m_curFace, m_curSize, m_curAlign;
	wxColour m_curColour;
	bool m_curBold, m_curItalic;
	float m_curLineWidth;
	int m_curLineStyle;

	// table properties
	int m_headerSize;
	int m_headerFace;
	int m_headerAlign;
	bool m_headerBold;
	wxColour m_headerColour;
	int m_cellAlign;
	bool m_gridLines;
	bool m_headerLine;
	std::vector<float> m_rowSizes;	
	std::vector<float> m_colSizes;
	bool m_tableBorder;

	wxString m_script;

};


class ReportPropertyDialog : public wxDialog
{
public:
	ReportPropertyDialog(wxWindow *parent, int id=-1);
	virtual ~ReportPropertyDialog();

	wxCheckListBox *cklTechModels;
	wxStaticBox *GroupBox2;
	wxStaticBox *GroupBox3;
	wxStaticBox *GroupBox1;
	wxCheckListBox *cklFinModels;
	wxStaticText *Label31;
	wxStaticText *Label3;
	wxButton *btnCancel;
	wxButton *btnOK;
	wxStaticText *Label6;
	wxExtTextCtrl *txtFooter;
	wxExtTextCtrl *txtHeader;
	wxStaticText *Label5;
	wxStaticText *Label4;
	wxCheckBox *chkSpecificModelsOnly;
	wxExtTextCtrl *txtAuthor;
	wxExtTextCtrl *txtDescription;
	wxStaticText *Label2;
	wxStaticText *Label1;

	wxString m_origDesc, m_origAuthor;
	bool m_origSpecMods;
	wxArrayString m_origTechMods, m_origFinMods;
	wxString m_origHeader, m_origFooter;


	void OnCommand( wxCommandEvent & );
	void SetData( SamReportTemplate * );
	bool GetData( SamReportTemplate * );

	void UpdateTechList();
	void UpdateFinList();

	wxArrayString GetSelectedTechs();
	wxArrayString GetSelectedFins();

	DECLARE_EVENT_TABLE()
};

#endif
