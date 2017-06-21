<<<<<<< HEAD
#ifndef __codegencallback_h
#define __codegencallback_h

#include <wx/stream.h>
#include <wx/dialog.h>

#include <wex/tpdlg.h>

#include <lk/env.h>

#include <ssc/sscapi.h>
#include "case.h"


class Case;
class CaseWindow;
class VarTable;
class CodeGen_Base;


class CodeGenCallbackContext : public CaseCallbackContext
{
	CodeGen_Base *m_cgb;
public:
	CodeGenCallbackContext(CodeGen_Base *c, const wxString &desc = wxEmptyString);
	CodeGen_Base *GetCodeGen_Base();
protected:
	virtual void SetupLibraries(lk::env_t *env);
};

=======
#ifndef __codegencallback_h
#define __codegencallback_h

#include <wx/stream.h>
#include <wx/dialog.h>

#include <wex/tpdlg.h>

#include <lk/env.h>

#include <ssc/sscapi.h>
#include "case.h"


class Case;
class CaseWindow;
class VarTable;
class CodeGen_Base;


class CodeGenCallbackContext : public CaseCallbackContext
{
	CodeGen_Base *m_cgb;
public:
	CodeGenCallbackContext(CodeGen_Base *c, const wxString &desc = wxEmptyString);
	CodeGen_Base *GetCodeGen_Base();
protected:
	virtual void SetupLibraries(lk::env_t *env);
};

>>>>>>> 2c85b0ce6a18646fb532eb72a604d646517b67ae
#endif