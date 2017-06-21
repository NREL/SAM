#ifndef __inputpage_h
#define __inputpage_h

#include <wx/panel.h>
#include <wex/uiform.h>

#include <lk/env.h>

#include "variables.h"
#include "equations.h"
#include "case.h"

class Case;
class CaseWindow;
class ActiveInputPage;

class UICallbackContext  : public CaseCallbackContext
{
	ActiveInputPage *m_inputPage;
public:
	UICallbackContext( ActiveInputPage *ip, const wxString &desc = wxEmptyString );	
	ActiveInputPage *InputPage();
	CaseWindow *GetCaseWindow();
protected:
	virtual void SetupLibraries( lk::env_t *env );
};

class ActiveInputPage : public wxPanel
{
public:
	ActiveInputPage( wxWindow *parent, wxUIFormData *form, CaseWindow *cw,
		int id = wxID_ANY, const wxPoint &pos = wxDefaultPosition, const wxSize &size = wxDefaultSize );
	virtual ~ActiveInputPage();
	
	// initialize by running any existing callbacks for it
	void Initialize();

	// Find() will look for an object on this specific input page
	wxUIObject *Find( const wxString &name );

	// FindActiveObject() can be overridden ( eg ActiveInputPage, casewin.cpp )
	// to locate an object that may reside on a different page
	virtual wxUIObject *FindActiveObject( const wxString &name, ActiveInputPage **page );
	wxString GetName() const { return m_formData->GetName(); }

	std::vector<wxUIObject*> GetObjects();
	VarInfoLookup &GetVariables();
	EqnFastLookup &GetEquations();
	VarTable &GetValues();
	Case *GetCase();
	CaseWindow *GetCaseWindow();	

	// This one is called when a UI event occurs, 
	// as when a user changes the value in an input control.
	// The implementation of this virtual method
	// in descendent classes is responsible
	// for handling the data exchange between the object
	// and any associated variable.
	// if any variable values are changed in the table
	// this function must also call any methods to (i.e. Case->Changed(...) )
	// propagate the changes (i.e. equations) to affected variables
	
	// Note: when a variable is changed programmatically (i.e. UI callback, or otherwise)
	// and the UI subsystem needs to be notified to update the
	// widget corresponding to a variable accordingly, the correct
	// way to handle this is to call GetCase()->VariableChanged(..)
	void OnUserInputChanged( wxUIObject *obj );

	
	// data exchange from UI object to data value and vice versa
	enum DdxDir { OBJ_TO_VAR, VAR_TO_OBJ };
	static bool DataExchange( wxUIObject *obj, VarValue &val, DdxDir dir );

protected:
	
	// to draw labels & units
	void OnErase( wxEraseEvent & );
	void OnPaint( wxPaintEvent & );

	// handler(s) for child widget changes
	void OnNativeEvent( wxCommandEvent & );
	

	bool LoadFile( const wxString &file );
	wxUIFormData *m_formData;
	bool m_formDataOwned;

	double m_scaleX, m_scaleY;
	void UpdateScale( wxDC *dc );
	wxSize ScaleSize( const wxSize &s );
	wxRect ScaleRect( const wxRect &r );

	CaseWindow *m_cwin;
	Case *m_case;
	
	DECLARE_EVENT_TABLE();
};

typedef unordered_map<wxString, wxUIFormData*, wxStringHash, wxStringEqual> FormDataHash;
class UIFormDatabase
{
public:
	UIFormDatabase();
	~UIFormDatabase();
	void Add( const wxString &name, wxUIFormData *data );
	wxUIFormData *Lookup( const wxString &name );
	void Clear();
private:
	FormDataHash m_hash;
};
#endif
