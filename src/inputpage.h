#ifndef __inputpage_h
#define __inputpage_h

#include <wx/panel.h>
#include <wex/uiform.h>

#include <lk_env.h>

#include "variables.h"
#include "equations.h"


class CallbackDatabase
{
public:
	CallbackDatabase();
	virtual ~CallbackDatabase();

	bool LoadFile( const wxString &file );
	void ClearAll();

	lk::node_t *Lookup( const wxString &method_name, const wxString &obj_name );
	lk::env_t *GetEnv() { return &m_cbenv; }
	
protected:
	struct cb_data{ lk::node_t *tree; wxString source; };
	std::vector<cb_data*> m_cblist;
	lk::env_t m_cbenv;
};

class InputPageBase;

class CallbackContext
{
	InputPageBase *m_inputPage;
	VarTable *m_varTable;
	lk::node_t *m_root;
	lk::env_t *m_parentEnv;
	wxString m_desc;

public:
	CallbackContext( InputPageBase *ip, VarTable *vt, lk::node_t *root, const wxString &desc = wxEmptyString );	
	InputPageBase *GetInputPage() { return m_inputPage; }
	VarTable *GetVarTable() { return m_varTable; }
	virtual bool Invoke(  );
};

typedef unordered_map<wxString, wxUIFormData*, wxStringHash, wxStringEqual> FormDataHash;

class FormDatabase
{
public:
	FormDatabase();
	~FormDatabase();

	void Add( const wxString &name, wxUIFormData *data );
	wxUIFormData *Lookup( const wxString &name );
	void Clear();

	bool LoadFile( const wxString &file );
private:
	FormDataHash m_hash;
};

class InputPageBase : public wxPanel
{
public:
	InputPageBase( wxWindow *parent, wxUIFormData *form, int id, const wxPoint &pos = wxDefaultPosition,
		const wxSize &size = wxDefaultSize );
	virtual ~InputPageBase();
	
	// initialize by running any existing callbacks for it
	void Initialize();

	// Find() will look for an object on this specific input page
	wxUIObject *Find( const wxString &name );

	// FindActiveObject() can be overridden ( eg ActiveInputPage, casewin.cpp )
	// to locate an object that may reside on a different page
	virtual wxUIObject *FindActiveObject( const wxString &name, InputPageBase **page );
	wxString GetName() const { return m_formData->GetName(); }

	std::vector<wxUIObject*> GetObjects() { return m_formData->GetObjects(); }

	// must be overridden to support rendering, equation calculation, callbacks, and
	// interaction with variable value tables
	virtual VarInfoLookup &GetVariables() = 0;
	virtual EqnFastLookup &GetEquations() = 0;
	virtual CallbackDatabase &GetCallbacks() = 0;
	virtual VarTable &GetValues() = 0;


	// This one is called when a UI event occurs, 
	// as when a user changes the value in an input control.
	// The implementation of this virtual method
	// in descendent classes is responsible
	// for handling the data exchange between the object
	// and any associated variable.
	// if any variable values are changed in the table
	// this function must also call any methods to (i.e. Case->Changed(...) )
	// propagate the changes (i.e. equations) to affected variables
	virtual void OnUserInputChanged( wxUIObject *obj ) = 0;

	// This method should be called when the value
	// of a variable is changed programmatically (i.e. UI callback, or otherwise)
	// and the UI subsystem needs to be notified to update the
	// widget corresponding to a variable accordingly.
	// This should not be called for each 'updated' value
	// after an equation set has been solved
	virtual void OnVariableChanged( const wxString &name ) = 0; 
	
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
	
	DECLARE_EVENT_TABLE();
};

#endif
