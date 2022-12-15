/*
BSD 3-Clause License

Copyright (c) Alliance for Sustainable Energy, LLC. See also https://github.com/NREL/SAM/blob/develop/LICENSE
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <wx/sizer.h>
#include <wx/checklst.h>
#include <wx/checkbox.h>
#include <wx/spinctrl.h>

#include <ptesdesignptdialog.h>
#include "main.h"
#include "script.h"

//BEGIN_EVENT_TABLE(PTESDesignPtDialog, wxDialog)
//    EVT_BUTTON(wxID_OK, PTESDesignPtDialog::OnEvt)
//    EVT_BUTTON(wxID_HELP, PTESDesignPtDialog::OnEvt)
//END_EVENT_TABLE()

PTESDesignPtDialog::PTESDesignPtDialog(wxWindow* parent, const wxString& title, lk::invoke_t& cxt)
    : wxDialog(parent, wxID_ANY, title, wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
{
    // Initialize
    m_result_code = -1;

    // Combine all into main vertical sizer
    wxBoxSizer* szmain = new wxBoxSizer(wxVERTICAL);

    wxString msg = "Hello World";
    szmain->Add(new wxStaticText(this, wxID_ANY, msg), 0, wxALL | wxEXPAND, 10);
    SetSizer(szmain);
    Fit();
}

void PTESDesignPtDialog::OnEvt(wxCommandEvent& e)
{
}
