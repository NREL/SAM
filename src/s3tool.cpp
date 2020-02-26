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

#include <vector>
#include <stdio.h>
#include <math.h>

#include <wx/simplebook.h>
#include <wx/spinctrl.h>
#include <wx/datstrm.h>
#include <wx/fontenum.h>
#include <wx/dcbuffer.h>
#include <wx/dcgraph.h>
#include <wx/slider.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/statline.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/clrpicker.h>
#include <wx/checklst.h>
#include <wx/statbmp.h>
#include <wx/generic/statbmpg.h>
#include <wx/xml/xml.h>
#include <wx/ffile.h>
#include <wx/wfstream.h>
#include <wx/sstream.h>
#include <wx/tglbtn.h>
#include <wx/busyinfo.h>
#include <wx/statbmp.h>
#include <wx/clipbrd.h>
#include <wx/generic/statbmpg.h>
#include <wx/mstream.h>

#if defined(__WXMSW__) || defined(__WXOSX__)

#include <wx/webview.h>

#endif

#include <wx/propgrid/propgrid.h>
#include <wx/propgrid/advprops.h>
#include <wx/splitter.h>

#include <wex/numeric.h>
#include <wex/plot/plplotctrl.h>
#include <wex/plot/pllineplot.h>
#include <wex/dview/dvplotctrl.h>
#include <wex/utils.h>
#include <wex/jsonreader.h>
#include <wex/metro.h>
#include <wex/easycurl.h>

#include <wex/icons/cirplus_12.cpng>
#include <wex/icons/cirminus_12.cpng>
#include <wex/icons/left_arrow_13.cpng>
#include <wex/icons/right_arrow_13.cpng>
#include <wex/icons/up_arrow_13.cpng>
#include <wex/icons/down_arrow_13.cpng>

#include "widgets.h"

#include "s3tool.h"
#include "s3view.h"

#ifndef S3D_STANDALONE

#include "main.h"

#endif

enum {
    ID_ADDRESS = wxID_HIGHEST + 239, ID_CURL, ID_LOOKUP_ADDRESS, ID_LATITUDE, ID_LONGITUDE, ID_TIMEZONE,
    ID_GET_MAP, ID_GO_UP, ID_GO_DOWN, ID_GO_LEFT, ID_GO_RIGHT, ID_ZOOM_IN, ID_ZOOM_OUT, ID_UNDERLAY_MAP,
    ID_REMOVE_UNDERLAY, ID_LOAD_MAP_IMAGE, ID_PASTE_MAP_IMAGE, ID_MANUAL_SCALE
};


BEGIN_EVENT_TABLE(LocationSetup, wxPanel)
                EVT_BUTTON(ID_LOOKUP_ADDRESS, LocationSetup::OnAddressChange)
                EVT_TEXT_ENTER(ID_ADDRESS, LocationSetup::OnAddressChange)

                EVT_BUTTON(ID_GET_MAP, LocationSetup::OnGetMap)
                EVT_BUTTON(ID_GO_LEFT, LocationSetup::OnMapChange)
                EVT_BUTTON(ID_GO_RIGHT, LocationSetup::OnMapChange)
                EVT_BUTTON(ID_GO_UP, LocationSetup::OnMapChange)
                EVT_BUTTON(ID_GO_DOWN, LocationSetup::OnMapChange)
                EVT_BUTTON(ID_ZOOM_IN, LocationSetup::OnMapChange)
                EVT_BUTTON(ID_ZOOM_OUT, LocationSetup::OnMapChange)
                EVT_BUTTON(ID_LOAD_MAP_IMAGE, LocationSetup::OnImportMapImage)
                EVT_BUTTON(ID_PASTE_MAP_IMAGE, LocationSetup::OnImportMapImage)
                EVT_BUTTON(ID_MANUAL_SCALE, LocationSetup::OnManualScale)
                EVT_BUTTON(ID_UNDERLAY_MAP, LocationSetup::OnUnderlayMap)
                EVT_BUTTON(ID_REMOVE_UNDERLAY, LocationSetup::OnRemoveUnderlay)
                EVT_EASYCURL(ID_CURL, LocationSetup::OnCurl)
END_EVENT_TABLE()


LocationSetup::LocationSetup(wxWindow *parent, ShadeTool *st)
        : wxPanel(parent),
          m_shadeTool(st),
          m_curl(this, ID_CURL) {
    SetBackgroundColour(*wxWHITE);


    m_scrollWin = new wxScrolledWindow(this, wxID_ANY, wxDefaultPosition, wxDefaultSize,
                                       wxScrolledWindowStyle | wxBORDER_NONE);
    m_bitmapCtrl = new wxGenericStaticBitmap(m_scrollWin, wxID_ANY, wxNullBitmap);
    m_address = new wxTextCtrl(this, ID_ADDRESS, "Denver, CO", wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER);

    m_zoomLevel = 19;
    m_mpp = 0.01;

//	wxMetroButton *btn;

    m_lat = new wxNumericCtrl(this, ID_LATITUDE, 39.7375670);
    m_lat->SetFormat(7);
    m_lon = new wxNumericCtrl(this, ID_LONGITUDE, -104.9847179);
    m_lon->SetFormat(7);
    m_tz = new wxNumericCtrl(this, ID_TIMEZONE, -7);

    wxBoxSizer *tools1 = new wxBoxSizer(wxHORIZONTAL);
    tools1->Add(new wxStaticText(this, wxID_ANY, "Address:"), 0, wxLEFT | wxTOP | wxBOTTOM | wxALIGN_CENTER_VERTICAL,
                10);
    tools1->Add(m_address, 1, wxALL | wxALIGN_CENTER_VERTICAL, 3);
    tools1->Add(new wxStaticText(this, wxID_ANY, "Latitude:"), 0, wxLEFT | wxTOP | wxBOTTOM | wxALIGN_CENTER_VERTICAL,
                10);
    tools1->Add(m_lat, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
    tools1->Add(new wxStaticText(this, wxID_ANY, "Longitude:"), 0, wxLEFT | wxTOP | wxBOTTOM | wxALIGN_CENTER_VERTICAL,
                10);
    tools1->Add(m_lon, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
    tools1->Add(new wxStaticText(this, wxID_ANY, "Time zone:"), 0, wxLEFT | wxTOP | wxBOTTOM | wxALIGN_CENTER_VERTICAL,
                10);
    tools1->Add(m_tz, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);

    wxPanel *panel_map_tools = new wxPanel(this);
    panel_map_tools->SetBackgroundColour(wxMetroTheme::Colour(wxMT_FOREGROUND));

    wxBoxSizer *tools3 = new wxBoxSizer(wxHORIZONTAL);

    tools3->Add(new wxMetroButton(panel_map_tools, ID_LOOKUP_ADDRESS, "Lookup address", wxNullBitmap, wxDefaultPosition,
                                  wxDefaultSize, wxMB_SMALLFONT));
    tools3->Add(new wxMetroButton(panel_map_tools, ID_GET_MAP, "Update map from coordinates", wxNullBitmap,
                                  wxDefaultPosition, wxDefaultSize, wxMB_SMALLFONT));
    tools3->Add(new wxMetroButton(panel_map_tools, ID_LOAD_MAP_IMAGE, "Load image", wxNullBitmap, wxDefaultPosition,
                                  wxDefaultSize, wxMB_SMALLFONT));
    tools3->Add(new wxMetroButton(panel_map_tools, ID_PASTE_MAP_IMAGE, "Paste image", wxNullBitmap, wxDefaultPosition,
                                  wxDefaultSize, wxMB_SMALLFONT));
    tools3->Add(new wxMetroButton(panel_map_tools, ID_MANUAL_SCALE, "Manual scale", wxNullBitmap, wxDefaultPosition,
                                  wxDefaultSize, wxMB_SMALLFONT));

    tools3->Add(new wxMetroButton(panel_map_tools, ID_ZOOM_IN, wxEmptyString, wxBITMAP_PNG_FROM_DATA(cirplus_12)), 0,
                wxALL | wxEXPAND, 0);
    tools3->Add(new wxMetroButton(panel_map_tools, ID_ZOOM_OUT, wxEmptyString, wxBITMAP_PNG_FROM_DATA(cirminus_12)), 0,
                wxALL | wxEXPAND, 0);
    tools3->Add(new wxMetroButton(panel_map_tools, ID_GO_LEFT, wxEmptyString, wxBITMAP_PNG_FROM_DATA(left_arrow_13)), 0,
                wxALL | wxEXPAND, 0);
    tools3->Add(new wxMetroButton(panel_map_tools, ID_GO_RIGHT, wxEmptyString, wxBITMAP_PNG_FROM_DATA(right_arrow_13)),
                0, wxALL | wxEXPAND, 0);
    tools3->Add(new wxMetroButton(panel_map_tools, ID_GO_UP, wxEmptyString, wxBITMAP_PNG_FROM_DATA(up_arrow_13)), 0,
                wxALL | wxEXPAND, 0);
    tools3->Add(new wxMetroButton(panel_map_tools, ID_GO_DOWN, wxEmptyString, wxBITMAP_PNG_FROM_DATA(down_arrow_13)), 0,
                wxALL | wxEXPAND, 0);

    tools3->Add(new wxMetroButton(panel_map_tools, ID_UNDERLAY_MAP, "Underlay this map in the scene", wxNullBitmap,
                                  wxDefaultPosition, wxDefaultSize, wxMB_SMALLFONT));
    tools3->Add(new wxMetroButton(panel_map_tools, ID_REMOVE_UNDERLAY, "Remove underlay in scene", wxNullBitmap,
                                  wxDefaultPosition, wxDefaultSize, wxMB_SMALLFONT));

    tools3->AddStretchSpacer();

    panel_map_tools->SetSizer(tools3);

    wxBoxSizer *main = new wxBoxSizer(wxVERTICAL);
    main->Add(tools1, 0, wxALL | wxEXPAND, 2);
    main->Add(panel_map_tools, 0, wxALL | wxEXPAND, 0);
    main->Add(m_scrollWin, 1, wxALL | wxEXPAND, 0);
    SetSizer(main);
}


void LocationSetup::OnCurl(wxEasyCurlEvent &) {
    // could do a progress thing...?
}

void LocationSetup::OnAddressChange(wxCommandEvent &) {
    m_lat->SetValue(std::numeric_limits<double>::quiet_NaN());
    m_lon->SetValue(std::numeric_limits<double>::quiet_NaN());
    m_tz->SetValue(std::numeric_limits<double>::quiet_NaN());

    wxYield();

    double lat, lon, tz;
    if (!wxEasyCurl::GeoCodeDeveloper(m_address->GetValue(), &lat, &lon, &tz)) {
        wxMessageBox("failed to geocode address");
        return;
    }

    m_lat->SetValue(lat);
    m_lon->SetValue(lon);
    m_tz->SetValue(tz);

    DownloadMap();
}

void LocationSetup::OnGetMap(wxCommandEvent &) {
    DownloadMap();
}

void LocationSetup::DownloadMap() {
    m_bitmap = wxEasyCurl::StaticMap(m_lat->Value(), m_lon->Value(), m_zoomLevel, wxEasyCurl::BING_MAPS);
    if (!m_bitmap.IsOk()) {
        wxMessageBox("Invalid image data file");
        return;
    }

    m_unannotatedBitmap = m_bitmap;

    // Map resolution = 156543.04 meters/pixel * cos(latitude) / (2 ^ zoomlevel)
    // http://msdn.microsoft.com/en-us/library/aa940990.aspx
    m_mpp = 156543.04 * cos(m_lat->Value() * 3.15926 / 180) / pow(2, m_zoomLevel);

    UpdateMap();
}


void LocationSetup::OnMapChange(wxCommandEvent &evt) {
    double lat = m_lat->Value();
    double lon = m_lon->Value();

    if (lat == -999 || lon == -999) {
        wxMessageBox("please obtain a map first");
        return;
    }

    double incr = 0.00005 * (22 - m_zoomLevel);

    switch (evt.GetId()) {
        case ID_GO_UP:
            lat += incr;
            break;
        case ID_GO_DOWN:
            lat -= incr;
            break;
        case ID_GO_LEFT:
            lon -= incr;
            break;
        case ID_GO_RIGHT:
            lon += incr;
            break;
        case ID_ZOOM_IN:
            if (m_zoomLevel < 21) m_zoomLevel++;
            break;
        case ID_ZOOM_OUT:
            if (m_zoomLevel > 1) m_zoomLevel--;
            break;
    }

    m_lat->SetValue(lat);
    m_lon->SetValue(lon);

    DownloadMap();
}

int LocationSetup::GetZoomLevel() { return m_zoomLevel; }

wxString LocationSetup::GetAddress() { return m_address->GetValue(); }


void LocationSetup::SetLocation(const wxString &address, double lat, double lon, double tz) {
    m_address->ChangeValue(address);
    m_lat->SetValue(lat);
    m_lon->SetValue(lon);
    m_tz->SetValue(tz);
}

void LocationSetup::GetLocation(double *lat, double *lon, double *tz) {
    if (lat) *lat = m_lat->Value();
    if (lon) *lon = m_lon->Value();
    if (tz) *tz = m_tz->Value();
}

wxBitmap LocationSetup::GetMap(double *lat, double *lon, double *tz, double *mpp) {
    GetLocation(lat, lon, tz);
    if (mpp) *mpp = m_mpp;
    return m_bitmap;
}

void
LocationSetup::SetMap(const wxBitmap &bit, const wxString &addrstr, double lat, double lon, double tz, double mpp) {
    m_lat->SetValue(lat);
    m_lon->SetValue(lon);
    m_tz->SetValue(tz);
    m_address->SetValue(addrstr);

    m_mpp = mpp;
    m_unannotatedBitmap = bit;

    UpdateMap();
}

void LocationSetup::OnUnderlayMap(wxCommandEvent &) {
    if (m_bitmap.IsOk()) {
        m_shadeTool->SwitchTo(PG_SCENE);
        m_shadeTool->GetView()->SetMode(View3D::TOP_VIEW);
        wxYield();
        wxMilliSleep(50);
        m_shadeTool->GetView()->ChangeMap(m_bitmap, m_mpp);
        m_shadeTool->GetView()->Refresh();
    } else
        wxMessageBox("Please setup a map image first.");
}

void LocationSetup::OnRemoveUnderlay(wxCommandEvent &) {
    m_shadeTool->SwitchTo(PG_SCENE);
    m_shadeTool->GetView()->SetMode(View3D::TOP_VIEW);
    wxYield();
    wxMilliSleep(50);
    m_shadeTool->GetView()->ChangeMap(wxNullBitmap, 1);
    m_shadeTool->GetView()->Refresh();
}

void LocationSetup::OnImportMapImage(wxCommandEvent &evt) {
    wxBitmap map(wxNullBitmap);
    if (evt.GetId() == ID_LOAD_MAP_IMAGE) {
        wxFileDialog dlg(this, "Select a map image file", wxEmptyString, wxEmptyString,
                         "Image Files (*.bmp;*.jpg;*.png)|*.bmp;*.jpg;*.png", wxFD_OPEN);

        if (dlg.ShowModal() != wxID_OK)
            return;

        if (!map.LoadFile(dlg.GetPath(), wxBITMAP_TYPE_ANY)) {
            wxMessageBox("Error loading selected image file:\n\n" + dlg.GetPath());
            return;
        }
    } else {
        // paste clipboard
        if (wxTheClipboard->Open()) {
            wxBitmapDataObject bitobj;
            if (wxTheClipboard->GetData(bitobj)) {
                map = bitobj.GetBitmap();
                wxTheClipboard->Close();
            }
        }

        if (map.IsNull()) {
            wxMessageBox("No image data is in the clipboard");
            return;
        }
    }

    m_unannotatedBitmap = map;
    UpdateMap();
    UpdateScale();
}

void LocationSetup::UpdateMap() {
    m_bitmap = m_unannotatedBitmap;

    wxMemoryDC dc(m_bitmap);
    wxFont font(*wxNORMAL_FONT);
    font.SetWeight(wxFONTWEIGHT_BOLD);
    dc.SetFont(font);
    dc.SetTextForeground(*wxWHITE);
    dc.SetPen(wxPen(*wxWHITE, 2));
    dc.DrawLine(2, 2, 102, 2);
    dc.DrawLine(2, 2, 2, 6);
    dc.DrawLine(102, 2, 102, 6);
    dc.DrawText(wxString::Format("%0.2lf m", m_mpp * 100), 5, 3);

    m_bitmapCtrl->SetBitmap(m_bitmap);
    m_bitmapCtrl->Refresh();

    m_scrollWin->SetScrollbars(1, 1, m_bitmap.GetWidth(), m_bitmap.GetHeight());
    m_scrollWin->SetScrollRate(20, 20);
}

void LocationSetup::UpdateScale() {
    wxString text = wxGetTextFromUser("Please enter image scale in meters per 100 pixels", "Scale",
                                      wxString::Format("%lg", m_mpp * 100));
    if (text.IsEmpty())
        return;

    m_mpp = wxAtof(text) / 100.0;
    if (m_mpp < 0.0001) m_mpp = 0.0001;
    if (m_mpp > 10) m_mpp = 10;

    UpdateMap();
}

void LocationSetup::OnManualScale(wxCommandEvent &) {
    UpdateScale();
}


void LocationSetup::Write(wxOutputStream &os) {
    wxDataOutputStream out(os);
    out.Write8(0x94);
    out.Write8(1);

    out.WriteString(m_address->GetValue());
    out.WriteDouble(m_lat->Value());
    out.WriteDouble(m_lon->Value());
    out.WriteDouble(m_tz->Value());
    out.WriteDouble(m_mpp);
    out.Write32(m_zoomLevel);

    out.Write8(m_unannotatedBitmap.IsNull() ? 0 : 1); // has map or not
    if (!m_unannotatedBitmap.IsNull()) {
        wxImage img = m_unannotatedBitmap.ConvertToImage();
        wxPNGHandler().SaveFile(&img, os, false);
    }

    out.Write8(0x94);
}

bool LocationSetup::Read(wxInputStream &is) {
    wxDataInputStream in(is);

    wxUint8 code = in.Read8();
    in.Read8(); //ver

    m_address->ChangeValue(in.ReadString());
    m_lat->SetValue(in.ReadDouble());
    m_lon->SetValue(in.ReadDouble());
    m_tz->SetValue(in.ReadDouble());
    m_mpp = in.ReadDouble();
    m_zoomLevel = in.Read32();

    wxUint8 has_map = in.Read8();
    if (has_map != 0) {
        wxImage img;
        wxPNGHandler().LoadFile(&img, is, false);
        m_unannotatedBitmap = wxBitmap(img);
    }

    UpdateMap();

    return code == in.Read8();
}

enum {
    ID_PROPGRID = wxID_HIGHEST + 959, ID_OBJLIST, ID_DUPLICATE, ID_DELETE
};

BEGIN_EVENT_TABLE(ObjectEditor, wxPanel)
                EVT_PG_CHANGED(ID_PROPGRID, ObjectEditor::OnPropertyGridChange)
                EVT_PG_CHANGING(ID_PROPGRID, ObjectEditor::OnPropertyGridChanging)
                EVT_LISTBOX(ID_OBJLIST, ObjectEditor::OnObjectList)
                EVT_CHECKLISTBOX(ID_OBJLIST, ObjectEditor::OnObjectCheckList)
                EVT_BUTTON(ID_DUPLICATE, ObjectEditor::OnCommand)
                EVT_BUTTON(ID_DELETE, ObjectEditor::OnCommand)
END_EVENT_TABLE()


#define SPACING 2

ObjectEditor::ObjectEditor(wxWindow *parent, int id, View3D *view,
                           const wxPoint &pos, const wxSize &size)
        : wxPanel(parent, id, pos, size, wxTAB_TRAVERSAL),
          m_view(view),
          m_curObject(0) {
    SetBackgroundColour(wxMetroTheme::Colour(wxMT_FOREGROUND));

    m_objList = new wxCheckListBox(this, ID_OBJLIST, wxDefaultPosition, wxDefaultSize, 0, 0, wxBORDER_NONE);
    m_propGrid = new wxPropertyGrid(this, ID_PROPGRID, wxDefaultPosition, wxDefaultSize,
                                    wxPG_SPLITTER_AUTO_CENTER | wxBORDER_NONE);

    m_duplicate = new wxMetroButton(this, ID_DUPLICATE, "Duplicate", wxNullBitmap, wxDefaultPosition, wxDefaultSize,
                                    wxMB_SMALLFONT);
    m_delete = new wxMetroButton(this, ID_DELETE, "Delete", wxNullBitmap, wxDefaultPosition, wxDefaultSize,
                                 wxMB_SMALLFONT);

    wxBoxSizer *objtools = new wxBoxSizer(wxHORIZONTAL);
    objtools->Add(m_duplicate, 0, wxALL, 0);
    objtools->Add(m_delete, 0, wxALL, 0);
    objtools->AddStretchSpacer();

    m_duplicate->Hide();
    m_delete->Hide();

    wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
    sizer->Add(m_objList, 1, wxALL | wxEXPAND, 0);
    sizer->Add(objtools, 0, wxALL | wxEXPAND, 0);
    sizer->Add(m_propGrid, 1, wxALL | wxEXPAND, 0);
    SetSizer(sizer);
}

ObjectEditor::~ObjectEditor() {
    SetObject(0);
}


void ObjectEditor::UpdateObjectList() {
    m_objList->Clear();

    if (!m_view) return;

    wxString sel = m_objList->GetStringSelection();
    std::vector<VObject *> list = m_view->GetObjects();
    for (size_t i = 0; i < list.size(); i++) {
        m_objList->Append(list[i]->Property("Name").GetString() + " (" + list[i]->GetTypeName() + ")");
        m_objList->Check(m_objList->GetCount() - 1, list[i]->IsVisible());
    }

    if (!sel.IsEmpty()) m_objList->SetStringSelection(sel);
}


void ObjectEditor::UpdatePropertyValues() {
    for (size_t i = 0; i < m_curProps.size(); i++)
        ValueToPropGrid(m_curProps[i]);
}

void ObjectEditor::SetObject(VObject *obj) {
    m_curObject = 0;

    m_duplicate->Hide();
    m_delete->Hide();

    // clear the property grid
    m_curProps.clear();
    m_propGrid->Clear();
    m_objList->SetSelection(m_objList->GetSelection(), false);

    if (obj == 0) {
        Layout();
        return;
    }

    m_curObject = obj;

    if (m_view != 0) {
        int index = m_view->GetObjectIndex(obj);
        if (m_objList->GetSelection() != index)
            m_objList->SetSelection(index);
    }

    wxArrayString list = obj->Properties();
    for (size_t i = 0; i < list.Count(); i++) {
        VProperty &p = obj->Property(list[i]);

        wxPGProperty *pg = 0;
        switch (p.GetType()) {
            case VProperty::DOUBLE:
                pg = new wxFloatProperty(list[i], wxPG_LABEL);
                break;
            case VProperty::INTEGER:
                if (p.GetChoices().size() == 0)
                    pg = new wxIntProperty(list[i], wxPG_LABEL);
                else
                    pg = new wxEnumProperty(list[i], wxPG_LABEL, p.GetChoices());
                break;
            case VProperty::STRING:
                pg = new wxStringProperty(list[i], wxPG_LABEL);
                break;
            case VProperty::BOOLEAN:
                pg = new wxBoolProperty(list[i], wxPG_LABEL);
                break;
            case VProperty::COLOUR:
                pg = new wxColourProperty(list[i], wxPG_LABEL);
                pg->SetAttribute("HasAlpha", true);
                break;
        }

        if (pg != 0) {
            m_propGrid->Append(pg);
            pgpinfo x;
            x.name = list[i];
            x.type = p.GetType();
            x.pgp = pg;
            m_curProps.push_back(x);
            ValueToPropGrid(x);
        }
    }

    m_duplicate->Show();
    m_delete->Show();
    Layout();
}

void ObjectEditor::ValueToPropGrid(pgpinfo &p) {
    if (!m_curObject) return;

    VProperty &vp = m_curObject->Property(p.name);

    switch (vp.GetType()) {
        case VProperty::BOOLEAN:
            m_propGrid->SetPropertyValue(p.name, wxVariant(vp.GetBoolean()));
            break;
        case VProperty::DOUBLE:
            m_propGrid->SetPropertyValue(p.name, wxVariant(vp.GetDouble()));
            break;
        case VProperty::INTEGER:
            m_propGrid->SetPropertyValue(p.name, wxVariant(vp.GetInteger()));
            break;
        case VProperty::STRING:
            m_propGrid->SetPropertyValue(p.name, wxVariant(vp.GetString()));
            break;
        case VProperty::COLOUR:
            wxVariant variant;
            variant << vp.GetColour();
            m_propGrid->SetPropertyValue(p.name, variant);
            break;
    }
}

void ObjectEditor::PropGridToValue(pgpinfo &p) {
    if (!m_curObject) return;

    VProperty &vp = m_curObject->Property(p.name);

    wxAny value = p.pgp->GetValue();

    if (vp.GetType() == VProperty::INVALID) return;

    switch (vp.GetType()) {
        case VProperty::BOOLEAN:
            vp.Set(wxANY_AS(value, bool));
            break;
        case VProperty::DOUBLE:
            vp.Set(wxANY_AS(value, double));
            break;
        case VProperty::INTEGER:
            vp.Set(wxANY_AS(value, int));
            break;
        case VProperty::STRING:
            vp.Set(wxANY_AS(value, wxString));
            break;
        case VProperty::COLOUR:
            vp.Set(wxANY_AS(value, wxColour));
            break;
    }

    if (m_view) {
        m_view->UpdateModel(m_curObject);
        m_view->Refresh();

        if (p.name.Lower() == "name") {
            int idx = m_view->GetObjectIndex(m_curObject);
            if (idx >= 0)
                m_objList->SetString(idx, vp.GetString() + " (" + m_curObject->GetTypeName() + ")");
        }
    }
}


void ObjectEditor::OnPropertyGridChange(wxPropertyGridEvent &evt) {
    wxPGProperty *p = evt.GetProperty();
    for (size_t i = 0; i < m_curProps.size(); i++)
        if (m_curProps[i].pgp == p)
            PropGridToValue(m_curProps[i]);
}

void ObjectEditor::OnPropertyGridChanging(wxPropertyGridEvent &) {
}

void ObjectEditor::OnCommand(wxCommandEvent &evt) {
    if (!m_view) return;

    switch (evt.GetId()) {
        case ID_DELETE:
            m_view->DeleteSelected();
            SetObject(0);
            break;
        case ID_DUPLICATE:
            m_view->DuplicateSelected();
            break;
    }
}

void ObjectEditor::OnObjectList(wxCommandEvent &) {
    if (!m_view) return;

    if (VObject *obj = m_view->GetObject((size_t) m_objList->GetSelection()))
        m_view->Select(obj);
}

void ObjectEditor::OnObjectCheckList(wxCommandEvent &evt) {
    if (!m_view) return;

    size_t sel = (size_t) evt.GetSelection();
    if (VObject *obj = m_view->GetObject(sel)) {
        obj->Show(m_objList->IsChecked(sel));
        m_view->UpdateHandles(obj);
        m_view->Refresh();
    }
}


enum {

    ID_GENERATE_DIURNAL = wxID_HIGHEST + 321, ID_GENERATE_TIMESERIES, ID_GENERATE_DIFFUSE
};


BEGIN_EVENT_TABLE(ShadeAnalysis, wxPanel)
                EVT_BUTTON(ID_GENERATE_DIURNAL, ShadeAnalysis::OnGenerateDiurnal)
                EVT_BUTTON(ID_GENERATE_TIMESERIES, ShadeAnalysis::OnGenerateTimeSeries)
                EVT_BUTTON(ID_GENERATE_DIFFUSE, ShadeAnalysis::OnGenerateDiffuse)
END_EVENT_TABLE()

ShadeAnalysis::ShadeAnalysis(wxWindow *parent, ShadeTool *st)
        : wxPanel(parent, wxID_ANY),
          m_shadeTool(st) {
    SetBackgroundColour(*wxWHITE);

    m_diffuseResults = new wxTextCtrl(this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize,
                                      wxTE_READONLY | wxBORDER_NONE);
    m_diffuseResults->SetBackgroundColour(*wxWHITE);

    wxBoxSizer *tools = new wxBoxSizer(wxHORIZONTAL);
    tools->Add(new wxButton(this, ID_GENERATE_DIURNAL, "Diurnal analysis"), 0, wxALL | wxALIGN_CENTER_VERTICAL, 2);
    tools->Add(new wxButton(this, ID_GENERATE_TIMESERIES, "Time series analysis"), 0, wxALL | wxALIGN_CENTER_VERTICAL,
               2);
    tools->Add(new wxButton(this, ID_GENERATE_DIFFUSE, "Diffuse analysis"), 0, wxALL | wxALIGN_CENTER_VERTICAL, 2);
    tools->Add(m_diffuseResults, 1, wxALL | wxALIGN_CENTER_VERTICAL, 1);

    m_scroll = new wxScrolledWindow(this, wxID_ANY, wxDefaultPosition, wxDefaultSize,
                                    wxScrolledWindowStyle | wxBORDER_NONE);

    wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
    sizer->Add(tools, 0, wxALL | wxEXPAND, 2);
    sizer->Add(m_scroll, 1, wxALL | wxEXPAND, 0);
    SetSizer(sizer);
}


void ShadeAnalysis::OnGenerateDiffuse(wxCommandEvent &) {
    std::vector<surfshade> shade;
    SimulateDiffuse(shade, false);
}

bool ShadeAnalysis::SimulateDiffuse(std::vector<surfshade> &shade, bool save) {
    m_diffuseResults->Clear();

    wxProgressDialog pdlg("Diffuse shade calculation", "Computing...", 100, m_shadeTool,
                          wxPD_SMOOTH | wxPD_CAN_ABORT | wxPD_APP_MODAL | wxPD_AUTO_HIDE);
#ifdef __WXMSW__
    pdlg.SetIcon(wxICON(appicon));
#endif

    pdlg.Show();

    double lat, lon, tz;
    m_shadeTool->GetLocationSetup()->GetLocation(&lat, &lon, &tz);

    s3d::transform tr;
    tr.set_scale(SF_ANALYSIS_SCALE);

    s3d::scene sc(m_shadeTool->GetView()->GetScene());
    std::vector<s3d::shade_result> shresult;

    wxStopWatch sw;
    bool stopped = false;

    size_t azi_min, azi_max, azi_step;
    size_t alt_min, alt_max, alt_step;
    azi_min = 0;
    azi_max = 359;
    azi_step = 1;

    alt_min = 1;
    alt_max = 89;
    alt_step = 1;

    size_t num_alt = 1 + (alt_max - alt_min) / alt_step;
    size_t num_azi = 1 + (azi_max - azi_min) / azi_step;
    size_t num_scenes = num_alt * num_azi;

    InitializeSections(num_scenes, shade);

    size_t c = 0;
    for (size_t azi = azi_min; azi <= azi_max && !stopped; azi += azi_step) {
        for (size_t alt = alt_min; alt <= alt_max && !stopped; alt += alt_step) {
            if (c % 1000 == 0) {
                int percent = (int) (100.0 * c / num_scenes);
                if (!pdlg.Update(percent)) {
                    stopped = true;
                    break;
                }
                wxYieldIfNeeded();
            }

            tr.rotate_azal((double) azi, (double) alt);
            sc.build(tr);

            std::vector<s3d::shade_result> shresult;
            sc.shade(shresult);

            for (size_t k = 0; k < shresult.size(); k++) {
                int id = shresult[k].id;
                // find the correct shade group for this 'id'
                // and accumulate the total shaded and active areas
                for (size_t n = 0; n < shade.size(); n++) {
                    std::vector<int> &ids = shade[n].ids;
                    if (std::find(ids.begin(), ids.end(), id) != ids.end()) {
                        shade[n].shaded[c] += shresult[k].shade_area;
                        shade[n].active[c] += shresult[k].active_area;
                        shade[n].aoisum[c] += shresult[k].aoi;

                        // accumulate number of surfaces included - only include shaded portion if greater than zero
                        shade[n].nsurf[c]++;
                    }
                }
            }

            // compute each group's shading factor from the overall areas
            // solid angle for spherical integral has sin(theta) term - include here.
            for (size_t n = 0; n < shade.size(); n++) {
                shade[n].sfac[c] = 0;
                if (shade[n].nsurf[c] > 0 && shade[n].active[c] > 0.0) {
//					double aoi = shade[n].aoisum[c] / shade[n].nsurf[c]; // average AOI for surfaces in this group

                    shade[n].sfac[c] = 100.0 * shade[n].shaded[c] / shade[n].active[c]
                                       * sin((90 - alt) * M_PI / 180); // differential when integrating over a sphere
                    //* cos( aoi * M_PI/180 );  // directional dependence of diffuse
                }
            }

            c++;
        }
    }

    num_scenes = c;

    // average shading factor over skydome
    m_diffuseShadeCount.clear();
    m_diffuseShadeFactor.clear();
    m_diffuseShadePercent.clear();
    m_diffuseName.Clear();
    for (size_t j = 0; j < shade.size(); j++) {
        size_t count = 0;
        double average = 0;
        for (size_t i = 0; i < num_scenes; i++) {
            if (shade[j].nsurf[i] > 0) {
                average += shade[j].sfac[i];
                count++;
            }
        }
        m_diffuseShadeFactor.push_back(average);
        m_diffuseShadeCount.push_back(count);
        if (count > 0) average /= count;
        else average = 0; // if there were no surfaces in this piece (i.e. facing away from sun), no diffuse blocking

        m_diffuseShadePercent.push_back(average);
        m_diffuseName.push_back(shade[j].group);
//		m_diffuseName.push_back(GetGroupDisplayName(shade[j].group));
    }

    wxString difftext("Diffuse shading: ");
    for (size_t i = 0; i < m_diffuseShadePercent.size(); i++) {
        difftext += GetGroupDisplayName(shade[i].group) + wxString::Format(": %.2lf%%", m_diffuseShadePercent[i]);
        //difftext += m_diffuseName[i] + wxString::Format(": %.2lf%%", m_diffuseShadePercent[i]);
        //		difftext += m_diffuseName[i] + wxString::Format(": %lg %%", m_diffuseShadePercent[i]);
        if (i < m_diffuseShadePercent.size() - 1) difftext += ", ";
    }

    m_diffuseResults->ChangeValue(difftext);



//	wxMessageBox(wxString::Format("Diffuse shading (%d (%d) scenes in %d ms)", c, num_scenes,sw.Time()));
    if (save) {
        wxFileDialog dlg(this, "Diffuse Shading File Export", wxEmptyString, "diffuse_shade.csv", "*.*",
                         wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
        if (wxID_OK == dlg.ShowModal()) {
            if (FILE *fp = fopen((const char *) dlg.GetPath().c_str(), "w")) {
                fprintf(fp, "azimuth,altitude,");
                for (size_t i = 0; i < shade.size(); i++)
                    fprintf(fp, "%s %c", (const char *) shade[i].group.c_str(), i + 1 < shade.size() ? ',' : '\n');

                c = 0;
                for (size_t azi = azi_min; azi <= azi_max; azi += azi_step) {
                    for (size_t alt = alt_min; alt <= alt_max; alt += alt_step) {
                        fprintf(fp, "%d,%d,", (int) azi, (int) alt);
                        for (size_t j = 0; j < shade.size(); j++)
                            fprintf(fp, "%lg%c", shade[j].sfac[c], j + 1 < shade.size() ? ',' : '\n');
                        c++;
                    }
                }
                fclose(fp);
            } else
                wxMessageBox("Could not write to file:\n\n" + dlg.GetPath());
        }
    }

    return true;
}

wxString ShadeAnalysis::GetGroupDisplayName(const wxString &group) {
    wxString name = group;
    wxArrayString name_ary = wxSplit(group, '.');
    if (name_ary.Count() == 2) {
        name = wxString::Format("Subarray %d, String %d", atoi(name_ary[0].c_str()) + 1, atoi(name_ary[1].c_str()) + 1);
    }
    return name;
}


bool ShadeAnalysis::SimulateTimeseries(int minute_step, std::vector<surfshade> &shade) {
    int allowed_steps[] = {1, 3, 5, 10, 15, 30, 60, 0};
    int ii = 0;
    bool tsok = false;
    while (allowed_steps[ii] != 0) {
        if (minute_step == allowed_steps[ii++]) {
            tsok = true;
            break;
        }
    }

    if (!tsok) return false;


    wxProgressDialog pdlg("Time series shade calculation", "Computing...", 100, m_shadeTool,
                          wxPD_SMOOTH | wxPD_CAN_ABORT | wxPD_APP_MODAL | wxPD_AUTO_HIDE);
#ifdef __WXMSW__
    pdlg.SetIcon(  wxICON( appicon) );
#endif
    pdlg.Show();

    double lat, lon, tz;
    m_shadeTool->GetLocationSetup()->GetLocation(&lat, &lon, &tz);

    double azi, zen, alt;
    const int *ndays = ::wxNDay;
    int m, d, h, jj, c = 0;

    int step_per_hour = 60 / minute_step;
    int npoints = surfshade::nvalues(minute_step);
    InitializeSections(npoints, shade);

    s3d::transform tr;
    tr.set_scale(SF_ANALYSIS_SCALE);

    s3d::scene sc(m_shadeTool->GetView()->GetScene());
    std::vector<s3d::shade_result> shresult;

    int last_percent = 0.0;
    bool stopped = false;

    for (m = 0; m < 12 && !stopped; m++) {
        for (d = 0; d < ndays[m] && !stopped; d++) {
            for (h = 0; h < 24 && !stopped; h++) {
                for (jj = 0; jj < step_per_hour; jj++) {
                    int percent = (int) (100.0 * c / npoints);
                    if (last_percent != percent) {
                        if (!pdlg.Update(percent)) {
                            stopped = true;
                            break;
                        } else
                            wxYieldIfNeeded();

                        last_percent = percent;
                    }

                    s3d::sun_pos(1970, m + 1, d + 1, h, jj * minute_step + 0.5 * minute_step, lat, lon, tz, &azi, &zen);
                    alt = 90 - zen;

                    // for nighttime full shading (fraction=1 and factor=0)
                    // consistent with SAM shading factor of zero for night time
                    //	double sf = 0;
                    if (alt > 0) {
                        tr.rotate_azal(azi, alt);
                        sc.build(tr);

                        std::vector<s3d::shade_result> shresult;
                        sc.shade(shresult);

                        for (size_t k = 0; k < shresult.size(); k++) {
                            int id = shresult[k].id;
                            // find the correct shade group for this 'id'
                            // and accumulate the total shaded and active areas
                            for (size_t n = 0; n < shade.size(); n++) {
                                std::vector<int> &ids = shade[n].ids;
                                if (std::find(ids.begin(), ids.end(), id) != ids.end()) {
                                    shade[n].shaded[c] += shresult[k].shade_area;
                                    shade[n].active[c] += shresult[k].active_area;
                                }
                            }
                        }
                    }

                    // compute each group's shading factor from the overall areas
                    for (size_t n = 0; n < shade.size(); n++) {
                        double sf = 1;
                        if (shade[n].active[c] != 0.0)
                            sf = shade[n].shaded[c] / shade[n].active[c];

                        shade[n].sfac[c] = 100.0f * sf;
                    }

                    c++;
                }
            }
        }
    }

    return true;
}

void ShadeAnalysis::OnGenerateTimeSeries(wxCommandEvent &) {
    std::vector<surfshade> shade;

    wxStopWatch sw;
    int min = atoi(wxGetTextFromUser("Enter time step in minutes:\n\nAllowed values: 1, 5, 10, 15, 20, 30, 60",
                                     "Time series calculation", "60").c_str());

    if (!SimulateTimeseries(min, shade)) {
        wxMessageBox("Error or cancellation in time series shade calculation");
        return;
    }

    long time = sw.Time();

    int nstep = surfshade::nvalues(min);

    wxMessageBox(wxString::Format("Time series shading (%d scenes in %d ms)", nstep, time));


    wxString outputfile(wxFileName(m_shadeTool->GetFileName()).GetName());
    if (outputfile.IsEmpty()) outputfile = "shade";
    outputfile += wxString::Format("_%dmin.csv", min);
    wxFileDialog dlg(this, "Time series shading file output", wxEmptyString, outputfile, "*.*",
                     wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
    if (wxID_OK == dlg.ShowModal()) {
        if (FILE *fp = fopen((const char *) dlg.GetPath().c_str(), "w")) {

#ifdef S3D_STANDALONE
            extern wxString g_appTitle;
#else
            wxString g_appTitle("Integrated SAM Shade Calculator");
#endif
            fprintf(fp, "Shading results generated by %s on %s in %.3lf seconds\n", (const char *) g_appTitle.c_str(),
                    (const char *) wxNow().c_str(), 0.001 * time);
            fprintf(fp, "Geometry file: %s\n", (const char *) m_shadeTool->GetFileName().c_str());
            wxString addr(m_shadeTool->GetLocationSetup()->GetAddress());
            addr.Replace(",", " ");
            fprintf(fp, "Site address: %s\n", (const char *) addr.c_str());
            double lat, lon, tz;
            m_shadeTool->GetLocationSetup()->GetLocation(&lat, &lon, &tz);
            fprintf(fp, "Latitude,%lg,Longitude,%lg,Time zone,%lg\n", lat, lon, tz);
            fputs("Month,Day,Hour,Minute,", fp);
            for (size_t i = 0; i < shade.size(); i++)
                fprintf(fp, "%s %%%c", (const char *) shade[i].group.c_str(), i + 1 < shade.size() ? ',' : '\n');

            int c = 0;
            int step_per_hour = 60 / min;
            for (int m = 1; m <= 12; m++) {
                for (int d = 1; d <= wxNDay[m - 1]; d++) {
                    for (int h = 0; h < 24; h++) {
                        for (int jj = 0; jj < step_per_hour; jj++) {
                            double fminval = jj * min + 0.5 * min;
                            fprintf(fp, "%d,%d,%d,%lg,", m, d, h, fminval);
                            for (size_t j = 0; j < shade.size(); j++)
                                fprintf(fp, "%.3lf%c", shade[j].sfac[c], j + 1 < shade.size() ? ',' : '\n');

                            c++;
                        }
                    }
                }
            }

            fclose(fp);
        } else
            wxMessageBox("Could not write to file:\n\n" + dlg.GetPath());
    }

    if (wxYES == wxMessageBox("View time series shading factor results?", "Query", wxYES_NO, this)) {
        wxFrame *frame = new wxFrame(0, wxID_ANY,
                                     wxString::Format("Shade fractions (computation in %d ms)", time),
                                     wxDefaultPosition, wxScaleSize(900, 700));

        wxDVPlotCtrl *dview = new wxDVPlotCtrl(frame);
        std::vector<double> data(nstep);
        for (size_t j = 0; j < shade.size(); j++) {
            for (int i = 0; i < nstep; i++)
                data[i] = shade[j].sfac[i];

            wxDVArrayDataSet *dset = new wxDVArrayDataSet(GetGroupDisplayName(shade[j].group), "% Shaded", min / 60.0,
                                                          data);
//			wxDVArrayDataSet *dset = new wxDVArrayDataSet(shade[j].group, "% Shaded", min / 60.0, data);
            dset->SetOffset(0.5 * min / 60.0);
            dview->AddDataSet(dset);
        }
        dview->SelectDataOnBlankTabs();
        frame->Show();
    }

}


void ShadeAnalysis::OnGenerateDiurnal(wxCommandEvent &) {
    SimulateDiurnal();
}


void ShadeAnalysis::InitializeSections(int mode, std::vector<surfshade> &shade) {
    shade.clear();

    surfshade ungrouped(mode, wxEmptyString); // for any ungrouped array sections

    // setup shading result storage for each group
    std::vector<VObject *> objs = m_shadeTool->GetView()->GetObjects();
    for (size_t i = 0; i < objs.size(); i++) {
        if (VActiveSurfaceObject *surf = dynamic_cast<VActiveSurfaceObject *>( objs[i] )) {
// update to use subarray and string dropdown property as requested by Chris
            wxString grp = "";
            if (surf->Property("Subarray").GetType() == VProperty::INTEGER)
                grp = wxString::Format("%d", surf->Property("Subarray").GetInteger());
            if (surf->Property("String").GetType() == VProperty::INTEGER)
                grp += wxString::Format(".%d", surf->Property("String").GetInteger());

            // keep backwards compatibility
            if (grp.Len() < 1) {
                grp = surf->Property("Group").GetString().Trim().Trim(false);
                // implicitly update to appropriate subarray
            }
            surfshade *ss = 0;

            if (!grp.IsEmpty()) {
                int index = -1;
                for (int k = 0; k < (int) shade.size(); k++)
                    if (shade[k].group == grp)
                        index = k;

                if (index < 0) {
                    shade.push_back(surfshade(mode, grp));
                    index = shade.size() - 1;
                }

                ss = &shade[index];
            } else
                ss = &ungrouped;

            ss->surfaces.push_back(surf);
            if (std::find(ss->ids.begin(), ss->ids.end(), surf->GetId()) == ss->ids.end())
                ss->ids.push_back(surf->GetId());
        }
    }


    if (ungrouped.surfaces.size() > 0) {
        if (shade.size() > 0) ungrouped.group = "Ungrouped active surfaces"; // subarrays defined
        else ungrouped.group = "Array"; // no subarrays defined

        shade.push_back(ungrouped);
    }

}

bool ShadeAnalysis::SimulateDiurnal() {
    bool success = false;
    wxProgressDialog pdlg("Shade calculation", "Computing...", 288, m_shadeTool,
                          wxPD_SMOOTH | wxPD_CAN_ABORT | wxPD_APP_MODAL | wxPD_AUTO_HIDE);
#ifdef __WXMSW__
    pdlg.SetIcon( wxICON( appicon) );
#endif
    pdlg.Show();

    wxYield();

    double lat, lon, tz;
    m_shadeTool->GetLocationSetup()->GetLocation(&lat, &lon, &tz);

    double azi, zen, alt;
    size_t m, d, h;

    std::vector<surfshade> shade;
    InitializeSections(surfshade::DIURNAL, shade);

    s3d::transform tr;
    tr.set_scale(SF_ANALYSIS_SCALE);

    s3d::scene sc(m_shadeTool->GetView()->GetScene());

    wxStopWatch sw;
    bool stopped = false;
    int i = 0;
    for (m = 0; m < 12 && !stopped; m++) {
        d = 14;
        for (h = 0; h < 24 && !stopped; h++) {
            pdlg.Update(i++);
            wxYieldIfNeeded();

            s3d::sun_pos(1970, m + 1, d + 1, h, 30.0, lat, lon, tz, &azi, &zen);
            alt = 90 - zen;

            // for nighttime full shading (fraction=1 and factor=0)
            // consistent with SAM shading factor of zero for night time
            //	double sf = 0;
//			double scene_sf = 1;
            if (alt > 0) {
                tr.rotate_azal(azi, alt);
                sc.build(tr);

                std::vector<s3d::shade_result> shresult;
                sc.shade(shresult);

                for (size_t k = 0; k < shresult.size(); k++) {
                    int id = shresult[k].id;

                    // find the correct shade group for this 'id'
                    // and accumulate the total shaded and active areas
                    for (size_t n = 0; n < shade.size(); n++) {
                        std::vector<int> &ids = shade[n].ids;
                        if (std::find(ids.begin(), ids.end(), id) != ids.end()) {
                            shade[n].shaded(m, h) += shresult[k].shade_area;
                            shade[n].active(m, h) += shresult[k].active_area;
                        }
                    }
                }
            }

            // compute each group's shading factor from the overall areas
            for (size_t n = 0; n < shade.size(); n++) {
                double sf = 1;
                if (shade[n].active(m, h) != 0.0)
                    sf = shade[n].shaded(m, h) / shade[n].active(m, h);

                shade[n].sfac(m, h) = 100.0f * sf;
            }


            if (pdlg.WasCancelled())
                stopped = true;
        }
    }

    int y = 0;
    for (size_t i = 0; i < shade.size(); i++) {
        if (i >= m_mxhList.size())
            m_mxhList.push_back(new AFMonthByHourFactorCtrl(m_scroll, wxID_ANY));

        AFMonthByHourFactorCtrl *mxh = m_mxhList[i];
        mxh->SetSize(0, y, 1300, 360);
//		mxh->SetTitle(shade[i].group);
        mxh->SetTitle(GetGroupDisplayName(shade[i].group));
        mxh->SetData(shade[i].sfac);
        mxh->SetLegend("Shade Loss (%): 0=no shade, 100=fully shaded");
        y += 360;
    }

    while (m_mxhList.size() > shade.size()) {
        m_mxhList[m_mxhList.size() - 1]->Destroy();
        m_mxhList.erase(m_mxhList.end() - 1);
    }

    m_scroll->SetScrollbars(1, 1, 1100, y);
    success = true;
    return success;
}

size_t ShadeAnalysis::GetDiurnalCount() {
    return m_mxhList.size();
}

size_t ShadeAnalysis::GetDiffuseCount() {
    return m_diffuseShadePercent.size();
}

void ShadeAnalysis::GetDiurnal(size_t i, matrix_t<float> *mxh, wxString *name) {
    if (i < m_mxhList.size()) {
        AFMonthByHourFactorCtrl *c = m_mxhList[i];
        (*mxh) = c->GetData();
        (*name) = c->GetTitle();
    }
}

void
ShadeAnalysis::GetDiffuse(size_t i, double *shade_percent, double *shade_factor, double *shade_count, wxString *name) {
    if ((i < m_diffuseShadePercent.size()) && (i < m_diffuseShadeFactor.size()) && (i < m_diffuseShadeCount.size()) &&
        (i < m_diffuseName.Count())) {
        (*shade_percent) = m_diffuseShadePercent[i];
        (*shade_factor) = m_diffuseShadeFactor[i];
        (*shade_count) = m_diffuseShadeCount[i];
        (*name) = m_diffuseName[i];
    }
}

#include <wex/lkscript.h>

class ShadeScripting::MyScriptCtrl : public wxLKScriptCtrl {
    wxTextCtrl *m_output;
public:
    MyScriptCtrl(wxTextCtrl *output, wxWindow *parent, int id = wxID_ANY)
            : wxLKScriptCtrl(parent, id, wxDefaultPosition, wxDefaultSize, wxLK_STDLIB_ALL | wxLK_STDLIB_SOUT) {
        m_output = output;
    }

    virtual void OnOutput(const wxString &tt) {
        m_output->AppendText(tt);
    }

    virtual void OnSyntaxCheck(int, const wxString &err) {
        m_output->Clear();
        m_output->AppendText(err);
    }
};

static void fcall_load_scene(lk::invoke_t &cxt) {
    LK_DOC("load_scene", "Load a 3D scene from a .s3d file", "(string:filename):boolean");
    cxt.result().assign(((ShadeTool *) cxt.user_data())->LoadFromFile(cxt.arg(0).as_string()) ? 1.0 : 0.0);
}

static void fcall_save_scene(lk::invoke_t &cxt) {
    LK_DOC("save_scene", "Saves the 3D scene to a .s3d file", "(string:filename):boolean");
    cxt.result().assign(((ShadeTool *) cxt.user_data())->WriteToFile(cxt.arg(0).as_string()) ? 1.0 : 0.0);

}

static void fcall_diffuse_shade(lk::invoke_t &cxt) {
    LK_DOC("diffuse_shade",
           "Calculate the diffuse shading on the scene.  Returns the diffuse shade percent on each segment, or null on an error.",
           "(none):table");

    std::vector<ShadeTool::diffuse> result;
    if (((ShadeTool *) cxt.user_data())->SimulateDiffuse(result, true)) {
        cxt.result().empty_hash();
        for (size_t i = 0; i < result.size(); i++) {
            wxString N(((ShadeTool *) cxt.user_data())->GetAnalysis()->GetGroupDisplayName(result[i].name));
            cxt.result().hash_item(N).assign(result[i].shade_percent);
        }
    }
}

static void fcall_direct_shade(lk::invoke_t &cxt) {
    LK_DOC("direct_shade",
           "Calculate the direct (beam) shade loss on the scene.  If a timestep (minutes) is specified, the time series shade loss is calculated.  Otherwise, a diurnal table is calculated.",
           "([number:time step minutes]):table");

    int min = 0;
    if (cxt.arg_count() == 1) {
        min = cxt.arg(0).as_integer();
        if (min < 1 || min > 60)
            min = 0;
    }

    if (min != 0) {
        std::vector<ShadeTool::shadets> result;
        if (((ShadeTool *) cxt.user_data())->SimulateTimeseries(min, result, true)) {
            cxt.result().empty_hash();
            for (size_t i = 0; i < result.size(); i++) {
                lk::vardata_t &v = cxt.result().hash_item(result[i].name);
                v.empty_vector();
                v.resize(result[i].ts.size());
                for (size_t j = 0; j < result[i].ts.size(); j++)
                    v.index(j)->assign((double) result[i].ts[j]);
            }
        }
    } else {
        std::vector<ShadeTool::diurnal> result;
        if (((ShadeTool *) cxt.user_data())->SimulateDiurnal(result)) {
            cxt.result().empty_hash();
            for (size_t i = 0; i < result.size(); i++) {
                lk::vardata_t &M = cxt.result().hash_item(result[i].name);
                M.empty_vector();
                M.resize(result[i].mxh.nrows());
                for (size_t r = 0; r < result[i].mxh.nrows(); r++) {
                    M.index(r)->empty_vector();
                    for (size_t c = 0; c < result[i].mxh.ncols(); c++)
                        M.index(r)->vec_append((double) result[i].mxh(r, c));
                }
            }
        }
    }
}

static void fcall_switch_to(lk::invoke_t &cxt) {
    LK_DOC("switch_to", "Switches pages in the 3D shade editor.", "(number:page num):none");
    ((ShadeTool *) cxt.user_data())->SwitchTo(cxt.arg(0).as_integer());
}

static void fcall_location(lk::invoke_t &cxt) {
    LK_DOC("location", "Set or get location information.",
           "(number:lat, number:lon, number:tz, [string:addr]):none  or  (none):table");

    ShadeTool *st = (ShadeTool *) cxt.user_data();
    if (cxt.arg_count() == 0) {
        double lat, lon, tz;
        st->GetLocationSetup()->GetLocation(&lat, &lon, &tz);
        cxt.result().empty_hash();
        cxt.result().hash_item("lat").assign(lat);
        cxt.result().hash_item("lon").assign(lon);
        cxt.result().hash_item("tz").assign(tz);
        cxt.result().hash_item("addr").assign(st->GetLocationSetup()->GetAddress());
    } else {
        wxString addr;
        if (cxt.arg_count() > 3)
            addr = cxt.arg(3).as_string();

        st->GetLocationSetup()->SetLocation(addr,
                                            cxt.arg(0).as_number(),
                                            cxt.arg(1).as_number(),
                                            cxt.arg(2).as_number());
    }
}

static void fcall_clear_scene(lk::invoke_t &cxt) {
    LK_DOC("clear_scene", "Clear the 3D scene.", "(none):none");
    ((ShadeTool *) cxt.user_data())->GetView()->DeleteAll();
    ((ShadeTool *) cxt.user_data())->GetView()->Refresh();
    ((ShadeTool *) cxt.user_data())->GetObjectEditor()->SetObject(NULL);
}

static void fcall_create(lk::invoke_t &cxt) {
    LK_DOC("create", "Create a new object in the 3d scene.", "(string:object type):name");

    if (VObject *obj = ((ShadeTool *) cxt.user_data())->GetView()->CreateObject(cxt.arg(0).as_string())) {
        cxt.result().assign(obj->Property("Name").GetString());
        ((ShadeTool *) cxt.user_data())->GetView()->Refresh();
        ((ShadeTool *) cxt.user_data())->GetObjectEditor()->UpdateObjectList();
    }
}

static void fcall_objects(lk::invoke_t &cxt) {
    LK_DOC("objects", "Returns a list of the names of all the objects in the scene.", "(none):array");
    std::vector<VObject *> list = ((ShadeTool *) cxt.user_data())->GetView()->GetObjects();

    cxt.result().empty_vector();
    for (size_t i = 0; i < list.size(); i++)
        cxt.result().vec_append(list[i]->Property("Name").GetString());
}

static void fcall_property(lk::invoke_t &cxt) {
    LK_DOC("property",
           "Sets or gets properties of an object.",
           "(string:name [, table:properties=values]):[table]");

    VObject *obj = ((ShadeTool *) cxt.user_data())->GetView()->FindObjectByName(cxt.arg(0).as_string());
    if (!obj) return;

    if (cxt.arg_count() > 1
        && cxt.arg(1).type() == lk::vardata_t::HASH) {

        lk::varhash_t *H = cxt.arg(1).hash();

        wxColour col;
        for (lk::varhash_t::const_iterator it = H->begin();
             it != H->end();
             ++it) {
            wxString key(it->first);
            lk::vardata_t *val(it->second);

            VProperty &p(obj->Property(key));
            switch (p.GetType()) {
                case VProperty::BOOLEAN:
                    p.Set(val->as_boolean());
                    break;
                case VProperty::DOUBLE:
                    p.Set(val->as_number());
                    break;
                case VProperty::INTEGER:
                    p.Set(val->as_integer());
                    break;
                case VProperty::STRING:
                    p.Set(val->as_string());
                    break;
                case VProperty::COLOUR:
                    if (val->type() == lk::vardata_t::STRING)
                        col.Set(val->as_string());
                    else if (val->type() == lk::vardata_t::VECTOR && val->length() == 3)
                        col.Set((unsigned char) val->index(0)->as_unsigned(),
                                (unsigned char) val->index(1)->as_unsigned(),
                                (unsigned char) val->index(2)->as_unsigned());

                    p.Set(col);
                default:
                    break;
            }
        }

        ((ShadeTool *) cxt.user_data())->GetView()->UpdateModel(obj);
        ((ShadeTool *) cxt.user_data())->GetObjectEditor()->UpdateObjectList();
        ((ShadeTool *) cxt.user_data())->GetView()->Refresh();
    } else {
        wxArrayString list(obj->Properties());
        cxt.result().empty_hash();
        for (size_t i = 0; i < list.size(); i++) {
            VProperty &p(obj->Property(list[i]));
            switch (p.GetType()) {
                case VProperty::BOOLEAN:
                    cxt.result().hash_item(list[i]).assign(p.GetBoolean() ? 1.0 : 0.0);
                    break;
                case VProperty::DOUBLE:
                    cxt.result().hash_item(list[i]).assign(p.GetDouble());
                    break;
                case VProperty::INTEGER:
                    cxt.result().hash_item(list[i]).assign((double) p.GetInteger());
                    break;
                case VProperty::COLOUR: {
                    lk::vardata_t &cc = cxt.result().hash_item(list[i]);
                    cc.empty_vector();
                    cc.vec_append((double) p.GetColour().Red());
                    cc.vec_append((double) p.GetColour().Green());
                    cc.vec_append((double) p.GetColour().Blue());
                }
                    break;
                case VProperty::STRING:
                    cxt.result().hash_item(list[i]).assign(p.GetString());
                default:
                    break;
            }
        }
    }
}

static void fcall_delete(lk::invoke_t &cxt) {
    LK_DOC("delete", "Delete an object from the scene.", "(string:name):none");

    ShadeTool *st = ((ShadeTool *) cxt.user_data());
    if (VObject *obj = st->GetView()->FindObjectByName(cxt.arg(0).as_string())) {
        st->GetView()->DeleteObject(obj);
        st->GetObjectEditor()->SetObject(NULL);
        st->GetObjectEditor()->UpdateObjectList();
        st->GetView()->DeleteObject(obj);
        st->GetView()->Refresh();
    }

}

static void fcall_sunpos(lk::invoke_t &cxt) {
    LK_DOC("sunpos", "Return solar azimuth and altitude angles given time of day and location.",
           "(year, month, hour, day, minute, lat, lon, tz):table");
    double azi, zen;
    s3d::sun_pos(
            cxt.arg(0).as_integer(),
            cxt.arg(1).as_integer(),
            cxt.arg(2).as_integer(),
            cxt.arg(3).as_integer(),
            cxt.arg(4).as_integer(),
            cxt.arg(5).as_number(),
            cxt.arg(6).as_number(),
            cxt.arg(7).as_number(),
            &azi, &zen);

    cxt.result().empty_hash();
    cxt.result().hash_item("azi").assign(azi);
    cxt.result().hash_item("alt").assign(90.0 - zen);

}


static void fcall_rotate(lk::invoke_t &cxt) {
    LK_DOC("rotate", "Rotate the 3D scene to the desired sun angles", "(number:azimuth, number:altitude):none");

    ShadeTool *st = ((ShadeTool *) cxt.user_data());
    st->GetView()->SetRotation(cxt.arg(0).as_number(), cxt.arg(1).as_number());
    st->GetView()->Refresh();
}

static void fcall_shadef(lk::invoke_t &cxt) {
    LK_DOC("shadef", "Return the current shaded area fraction at the 3D scene rotation", "(none):number");
    ShadeTool *st = ((ShadeTool *) cxt.user_data());
    cxt.result().assign(st->GetView()->GetShadeFraction());
}

#ifndef S3D_STANDALONE

#include "script.h"

#endif

lk::fcall_t *shade_tool_funcs() {
    static const lk::fcall_t vec[] = {
            fcall_load_scene,
            fcall_save_scene,
            fcall_clear_scene,
            fcall_switch_to,
            fcall_diffuse_shade,
            fcall_direct_shade,
            fcall_location,
            fcall_create,
            fcall_delete,
            fcall_objects,
            fcall_property,
            fcall_rotate,
            fcall_shadef,
            fcall_sunpos,
#ifndef S3D_STANDALONE
            fcall_set,
            fcall_get,
#endif
            0};

    return (lk::fcall_t *) vec;
}


ShadeScripting::ShadeScripting(wxWindow *parent, ShadeTool *st)
        : wxPanel(parent, wxID_ANY, wxDefaultPosition, wxDefaultSize),
          m_shadeTool(st) {
    SetBackgroundColour(*wxWHITE);

    wxBoxSizer *szdoc = new wxBoxSizer(wxHORIZONTAL);
    szdoc->Add(new wxButton(this, wxID_NEW, "New", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0,
               wxALL | wxEXPAND, 2);
    szdoc->Add(new wxButton(this, wxID_OPEN, "Open", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0,
               wxALL | wxEXPAND, 2);
    szdoc->Add(new wxButton(this, wxID_SAVE, "Save", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0,
               wxALL | wxEXPAND, 2);
    szdoc->Add(new wxButton(this, wxID_SAVEAS, "Save as", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0,
               wxALL | wxEXPAND, 2);
    szdoc->Add(new wxButton(this, wxID_FIND, "Find", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0,
               wxALL | wxEXPAND, 2);
    szdoc->Add(new wxButton(this, wxID_FORWARD, "Find next", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0,
               wxALL | wxEXPAND, 2);
    szdoc->Add(new wxButton(this, wxID_HELP, "Functions", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0,
               wxALL | wxEXPAND, 2);
    szdoc->Add(new wxButton(this, wxID_EXECUTE, "Run", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0,
               wxALL | wxEXPAND, 2);
    szdoc->Add(m_stopButton = new wxButton(this, wxID_STOP, "Stop", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0,
               wxALL | wxEXPAND, 2);
    szdoc->AddStretchSpacer();
    m_stopButton->SetForegroundColour(*wxRED);
    m_stopButton->Hide();


    m_output = new wxTextCtrl(this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize,
                              wxTE_READONLY | wxTE_MULTILINE | wxBORDER_NONE);

    m_script = new MyScriptCtrl(m_output, this, wxID_ANY);
    m_script->RegisterLibrary(shade_tool_funcs(), "Shade Tool Functions", m_shadeTool);

    wxBoxSizer *szedit = new wxBoxSizer(wxVERTICAL);
    szedit->Add(szdoc, 0, wxALL | wxEXPAND, 2);
    szedit->Add(m_script, 4, wxALL | wxEXPAND, 0);
    szedit->Add(m_statusLabel = new wxStaticText(this, wxID_ANY, wxEmptyString), 0, wxALL | wxEXPAND, 0);
    szedit->Add(m_output, 1, wxALL | wxEXPAND, 0);

    SetSizer(szedit);

}

ShadeScripting::~ShadeScripting() {
}

bool ShadeScripting::Save() {
    if (m_fileName.IsEmpty())
        return SaveAs();
    else
        return Write(m_fileName);
}

bool ShadeScripting::SaveAs() {
    wxFileDialog dlg(this, "Save as...",
                     wxPathOnly(m_fileName),
                     wxFileNameFromPath(m_fileName),
                     "LK Script Files (*.lk)|*.lk", wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
    if (dlg.ShowModal() == wxID_OK)
        return Write(dlg.GetPath());
    else
        return false;
}

bool ShadeScripting::CloseDoc() {
    if (m_script->IsScriptRunning()) {
        if (wxYES == wxMessageBox("A script is running. Cancel it?", "Query", wxYES_NO))
            m_script->Stop();

        return false;
    }

    if (m_script->GetModify()) {
        Raise();
        wxString id = m_fileName.IsEmpty() ? "untitled" : m_fileName;
        int result = wxMessageBox("Script modified. Save it?\n\n" + id, "Query", wxYES_NO | wxCANCEL);
        if (result == wxCANCEL
            || (result == wxYES && !Save()))
            return false;
    }

    m_script->SetText(wxEmptyString);
    m_script->EmptyUndoBuffer();
    m_script->SetSavePoint();
    m_fileName.Clear();
    m_statusLabel->SetLabel(m_fileName);
    return true;
}

void ShadeScripting::Open() {
    CloseDoc();
    wxFileDialog dlg(this, "Open", wxEmptyString, wxEmptyString,
                     "LK Script Files (*.lk)|*.lk",
                     wxFD_OPEN | wxFD_FILE_MUST_EXIST | wxFD_CHANGE_DIR);

    if (dlg.ShowModal() == wxID_OK)
        if (!Load(dlg.GetPath()))
            wxMessageBox("Could not load file:\n\n" + dlg.GetPath());
}


bool ShadeScripting::Write(const wxString &file) {
    wxBusyInfo info("Saving script file...");
    wxMilliSleep(120);

    if (((wxStyledTextCtrl *) m_script)->SaveFile(file)) {
        m_fileName = file;
        m_statusLabel->SetLabel(m_fileName);
        return true;
    } else return false;
}


bool ShadeScripting::Load(const wxString &file) {
    FILE *fp = fopen(file.c_str(), "r");
    if (fp) {
        wxString str;
        char buf[1024];
        while (fgets(buf, 1023, fp) != 0)
            str += buf;

        fclose(fp);
        m_script->SetText(str);
        m_script->EmptyUndoBuffer();
        m_script->SetSavePoint();
        m_fileName = file;
        m_statusLabel->SetLabel(m_fileName);
        return true;
    } else return false;
}

#include <wx/app.h>

void ShadeScripting::Exec() {
    m_output->Clear();
    m_stopButton->Show();
    Layout();
    wxTheApp->Yield(true);

    wxLKSetToplevelParentForPlots(m_shadeTool);
    wxLKSetPlotTarget(NULL);


    wxString svar;
    if (!m_fileName.IsEmpty()) {
        svar = wxPathOnly(m_fileName);
        m_script->SetWorkDir(wxPathOnly(m_fileName));
    } else {
        svar = wxEmptyString;
    }

    m_script->Execute();

    if (m_stopButton->IsShown()) {
        m_stopButton->Hide();
        Layout();
    }
}

void ShadeScripting::OnCommand(wxCommandEvent &evt) {
    switch (evt.GetId()) {
        case wxID_NEW:
            CloseDoc();
            break;
        case wxID_OPEN:
            Open();
            break;
        case wxID_SAVE:
            Save();
            break;
        case wxID_SAVEAS:
            SaveAs();
            break;
        case wxID_UNDO:
            m_script->Undo();
            break;
        case wxID_REDO:
            m_script->Redo();
            break;
        case wxID_CUT:
            m_script->Cut();
            break;
        case wxID_COPY:
            m_script->Copy();
            break;
        case wxID_PASTE:
            m_script->Paste();
            break;
        case wxID_SELECTALL:
            m_script->SelectAll();
            break;
        case wxID_FIND:
            m_script->ShowFindReplaceDialog();
            break;
        case wxID_FORWARD:
            m_script->FindNext();
            break;
        case wxID_HELP:
            m_script->ShowHelpDialog(this);
            break;
        case wxID_EXECUTE:
            Exec();
            break;
        case wxID_STOP:
            m_script->Stop();
            m_stopButton->Hide();
            Layout();
            break;
    }
}

BEGIN_EVENT_TABLE(ShadeScripting, wxPanel)
                EVT_BUTTON(wxID_NEW, ShadeScripting::OnCommand)
                EVT_BUTTON(wxID_OPEN, ShadeScripting::OnCommand)
                EVT_BUTTON(wxID_SAVE, ShadeScripting::OnCommand)
                EVT_BUTTON(wxID_SAVEAS, ShadeScripting::OnCommand)
                EVT_BUTTON(wxID_HELP, ShadeScripting::OnCommand)

                EVT_BUTTON(wxID_FIND, ShadeScripting::OnCommand)
                EVT_BUTTON(wxID_FORWARD, ShadeScripting::OnCommand)

                EVT_BUTTON(wxID_STOP, ShadeScripting::OnCommand)
                EVT_BUTTON(wxID_EXECUTE, ShadeScripting::OnCommand)
                EVT_BUTTON(wxID_HELP, ShadeScripting::OnCommand)

END_EVENT_TABLE()


enum {
    ID_SLIDER = wxID_HIGHEST + 441,
    ID_LAST_SLIDER = ID_SLIDER + 100,
    ID_LOCATION,
    ID_CREATE,
    ID_GRAPHICS,
    ID_ANALYSIS,
    ID_SCRIPTING,
    ID_BSPTREE,
    ID_VIEW_XYZ,
    ID_VIEW_XY,
    ID_VIEW_XZ,
    ID_FEEDBACK,
    ID_OBJECT_ID,
    ID_OBJECT_IDMAX = ID_OBJECT_ID + 100,

    // debugging
            ID_AZIMUTH,
    ID_ALTITUDE,
    ID_SCALE,
    ID_TRI_TEST,
    ID_EXPORT_HOURLY,
    ID_EXPORT_DIURNAL
};

BEGIN_EVENT_TABLE(ShadeTool, wxPanel)
                EVT_VIEW3D_UPDATE_OBJECTS(ID_GRAPHICS, ShadeTool::OnUpdateObjectList)
                EVT_VIEW3D_UPDATE_PROPERTIES(ID_GRAPHICS, ShadeTool::OnUpdateProperties)
                EVT_VIEW3D_UPDATE_SELECTION(ID_GRAPHICS, ShadeTool::OnUpdateSelection)

#ifdef S3D_STANDALONE
                                                                                                                                EVT_BUTTON(wxID_NEW, ShadeTool::OnCommand)
	EVT_BUTTON(ID_BSPTREE, ShadeTool::OnCommand)
#endif
                EVT_BUTTON(wxID_OPEN, ShadeTool::OnCommand)
                EVT_BUTTON(wxID_SAVE, ShadeTool::OnCommand)
                EVT_BUTTON(ID_ANALYSIS, ShadeTool::OnCommand)
                EVT_BUTTON(ID_SCRIPTING, ShadeTool::OnCommand)
                EVT_BUTTON(ID_LOCATION, ShadeTool::OnCommand)
                EVT_BUTTON(ID_CREATE, ShadeTool::OnCommand)
                EVT_BUTTON(ID_VIEW_XYZ, ShadeTool::OnCommand)
                EVT_BUTTON(ID_VIEW_XY, ShadeTool::OnCommand)
                EVT_BUTTON(ID_VIEW_XZ, ShadeTool::OnCommand)
                EVT_BUTTON(ID_FEEDBACK, ShadeTool::OnCommand)
                EVT_BUTTON(wxID_HELP, ShadeTool::OnCommand)


                EVT_MENU_RANGE(ID_OBJECT_ID, ID_OBJECT_IDMAX, ShadeTool::OnCreateObject)

                // handlers for debugging tools
                EVT_TEXT_ENTER(ID_AZIMUTH, ShadeTool::OnDebugCommand)
                EVT_TEXT_ENTER(ID_ALTITUDE, ShadeTool::OnDebugCommand)
                EVT_TEXT_ENTER(ID_SCALE, ShadeTool::OnDebugCommand)
                EVT_BUTTON(ID_TRI_TEST, ShadeTool::OnDebugCommand)
                EVT_BUTTON(ID_EXPORT_HOURLY, ShadeTool::OnDebugCommand)
                EVT_BUTTON(ID_EXPORT_DIURNAL, ShadeTool::OnDebugCommand)
END_EVENT_TABLE()


ShadeTool::ShadeTool(wxWindow *parent, int id, const wxString &data_path)
        : wxPanel(parent, id) {
    m_dataPath = data_path;
    SetBackgroundColour(wxMetroTheme::Colour(wxMT_FOREGROUND));

    wxBoxSizer *sizer_tool = new wxBoxSizer(wxHORIZONTAL);
#ifdef S3D_STANDALONE
                                                                                                                            sizer_tool->Add( new wxMetroButton( this, wxID_NEW, "New" ), 0, wxALL|wxEXPAND, 0 );
	sizer_tool->Add( new wxMetroButton( this, wxID_OPEN, "Open" ), 0, wxALL|wxEXPAND, 0 );
	sizer_tool->Add( new wxMetroButton( this, wxID_SAVE, "Save" ), 0, wxALL|wxEXPAND, 0 );
	sizer_tool->Add( new wxMetroButton( this, ID_BSPTREE, "BSP Tree"), 0, wxALL | wxEXPAND, 0);
#endif
    sizer_tool->Add(new wxMetroButton(this, ID_LOCATION, "Location"), 0, wxALL | wxEXPAND, 0);
    sizer_tool->Add(new wxMetroButton(this, ID_CREATE, "Create", wxNullBitmap, wxDefaultPosition, wxDefaultSize,
                                      wxMB_DOWNARROW), 0, wxALL | wxEXPAND, 0);
    sizer_tool->Add(new wxMetroButton(this, ID_VIEW_XYZ, "3D scene"), 0, wxALL | wxEXPAND, 0);
    sizer_tool->Add(new wxMetroButton(this, ID_VIEW_XY, "Bird's eye"), 0, wxALL | wxEXPAND, 0);
    sizer_tool->Add(new wxMetroButton(this, ID_VIEW_XZ, "Elevations"), 0, wxALL | wxEXPAND, 0);
    sizer_tool->Add(new wxMetroButton(this, ID_ANALYSIS, "Analyze"), 0, wxALL | wxEXPAND, 0);
    sizer_tool->Add(new wxMetroButton(this, ID_SCRIPTING, "Scripting"), 0, wxALL | wxEXPAND, 0);

#ifndef S3D_STANDALONE
    sizer_tool->Add(new wxMetroButton(this, wxID_OPEN, "Import"), 0, wxALL | wxEXPAND, 0);
    sizer_tool->Add(new wxMetroButton(this, wxID_SAVE, "Export"), 0, wxALL | wxEXPAND, 0);
#endif

    sizer_tool->AddStretchSpacer();


#ifdef S3D_STANDALONE
    sizer_tool->Add( new wxMetroButton( this, ID_FEEDBACK, "Feedback" ), 0, wxALL|wxEXPAND, 0 );
#endif

    sizer_tool->Add(new wxMetroButton(this, wxID_HELP, "Help"), 0, wxALL | wxEXPAND, 0);

#ifndef S3D_STANDALONE
    sizer_tool->Add(new wxMetroButton(this, wxID_OK, "Save and close"), 0, wxALL | wxEXPAND, 0);
#endif

    m_book = new wxSimplebook(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE);

    m_location = new LocationSetup(m_book, this);

    m_split = new wxSplitterWindow(m_book);
    m_view = new View3D(m_split, ID_GRAPHICS);
    m_sceneParams = new ObjectEditor(m_split, wxID_ANY, m_view);
    m_split->SetMinimumPaneSize(20);
    m_split->SplitVertically(m_sceneParams, m_view, (int) (200 * wxGetScreenHDScale()));


    // debugging tool panel
    m_debugPanel = new wxPanel(this);
    m_debugPanel->SetBackgroundColour(*wxWHITE);
    wxBoxSizer *debug_sizer = new wxBoxSizer(wxHORIZONTAL);

    debug_sizer->Add(new wxStaticText(m_debugPanel, wxID_ANY, "Azi:"), 0, wxALIGN_CENTER_VERTICAL | wxALL, 4);
    debug_sizer->Add(m_txtAzi = new wxTextCtrl(m_debugPanel, ID_AZIMUTH, "143", wxDefaultPosition, wxScaleSize(70, 24),
                                               wxTE_PROCESS_ENTER), 0, wxALL | wxEXPAND, 2);

    debug_sizer->Add(new wxStaticText(m_debugPanel, wxID_ANY, "Alt:"), 0, wxALIGN_CENTER_VERTICAL | wxALL, 4);
    debug_sizer->Add(m_txtAlt = new wxTextCtrl(m_debugPanel, ID_ALTITUDE, "28", wxDefaultPosition, wxScaleSize(70, 24),
                                               wxTE_PROCESS_ENTER), 0, wxALL | wxEXPAND, 2);

    debug_sizer->Add(new wxStaticText(m_debugPanel, wxID_ANY, "scale:"), 0, wxALIGN_CENTER_VERTICAL | wxALL, 4);
    debug_sizer->Add(m_txtScale = new wxTextCtrl(m_debugPanel, ID_SCALE, "4", wxDefaultPosition, wxScaleSize(70, 24),
                                                 wxTE_PROCESS_ENTER), 0, wxALL | wxEXPAND, 2);

    debug_sizer->Add(new wxButton(m_debugPanel, ID_EXPORT_HOURLY, "Export Hourly"), 0, wxALL | wxEXPAND, 0);
    debug_sizer->Add(new wxButton(m_debugPanel, ID_EXPORT_DIURNAL, "Export Diurnal"), 0, wxALL | wxEXPAND, 0);
    debug_sizer->Add(new wxButton(m_debugPanel, ID_TRI_TEST, "Triangle Test"), 0, wxALL | wxEXPAND, 0);

    m_debugPanel->SetSizer(debug_sizer);
    m_debugPanel->Show(false);

    m_analysis = new ShadeAnalysis(m_book, this);

    m_scripting = new ShadeScripting(m_book, this);

#ifdef S3D_STANDALONE

                                                                                                                            #if defined(__WXMSW__)||defined(__WXOSX__)
	wxString help_index( "file:///" + m_dataPath + "/help/index.html" );
	m_helpViewer = wxWebView::New( m_book, wxID_ANY, help_index,
		wxDefaultPosition, wxDefaultSize, wxWebViewBackendDefault, wxBORDER_NONE );
#endif

#endif

    wxBoxSizer *sizer_main = new wxBoxSizer(wxVERTICAL);
    sizer_main->Add(sizer_tool, 0, wxALL | wxEXPAND, 0);
    sizer_main->Add(m_book, 1, wxALL | wxEXPAND, 0);
    sizer_main->Add(m_debugPanel, 0, wxALL | wxEXPAND, 0);
    SetSizer(sizer_main);

    m_book->AddPage(m_location, "Location");
    m_book->AddPage(m_split, "Scene Editor");
    m_book->AddPage(m_analysis, "Analysis");
    m_book->AddPage(m_scripting, "Scripting");

#ifdef S3D_STANDALONE
                                                                                                                            #if defined(__WXMSW__)||defined(__WXOSX__)
	m_book->AddPage( m_helpViewer, "Help" );
#endif
#else
    m_book->SetSelection(PG_SCENE);
#endif
}

View3D *ShadeTool::GetView() {
    return m_view;
}

LocationSetup *ShadeTool::GetLocationSetup() {
    return m_location;
}

void ShadeTool::SwitchTo(int page) {
    m_book->SetSelection(page);
}


void ShadeTool::Save() {
    wxFileDialog dlg(this, "Save scene to file", wxPathOnly(m_fileName),
                     m_fileName, "Scene file (*.s3d)|*.s3d", wxFD_SAVE);
    if (dlg.ShowModal() == wxID_OK) {
        if (m_fileName != dlg.GetPath()
            && wxFileExists(dlg.GetPath())
            && wxNO == wxMessageBox("Overwrite existing file?\n\n" + dlg.GetPath(), "Query", wxYES_NO))
            return;

        if (WriteToFile(dlg.GetPath()))
            m_fileName = dlg.GetPath();
        else
            wxMessageBox("Could not open file for writing:\n\n" + dlg.GetPath());
    }
}

bool ShadeTool::Load() {
    wxFileDialog dlg(this, "Load scene from file", wxPathOnly(m_fileName),
                     wxEmptyString, "Scene file (*.s3d)|*.s3d", wxFD_OPEN);
    if (dlg.ShowModal() == wxID_OK) {
        if (LoadFromFile(dlg.GetPath())) {
            m_fileName = dlg.GetPath();
            return true;
        } else
            wxMessageBox("Could not open file for reading:\n\n" + dlg.GetPath());
    }

    return false;
}

bool ShadeTool::WriteToFile(const wxString &file) {
    wxBusyInfo busy("Writing shading data to file: " + wxFileNameFromPath(file), this);
    wxYield();
    wxMilliSleep(50);
    wxFFileOutputStream fos(file);
    if (!fos.IsOk()) return false;
    Write(fos);
    return true;
}

bool ShadeTool::LoadFromFile(const wxString &file) {
    wxFFileInputStream fis(file);
    if (!fis.IsOk() || !Read(fis)) return false;
    else return true;
}

void ShadeTool::Write(wxOutputStream &os) {
    wxDataOutputStream out(os);
    out.Write8(0x84);
    out.Write8(1);
    m_location->Write(os);
    m_view->Write(os);
    out.Write8(0x84);
}

bool ShadeTool::Read(wxInputStream &is) {
    wxDataInputStream in(is);
    wxUint8 code = in.Read8();
    in.Read8();

    bool ok1 = m_location->Read(is);
    bool ok2 = m_view->Read(is);

    return in.Read8() == code && ok1 && ok2;
}

bool ShadeTool::SimulateTimeseries(int &minute_timestep, std::vector<shadets> &result, bool use_groups) {
    result.clear();
    std::vector<ShadeAnalysis::surfshade> shade;
    if (m_analysis->SimulateTimeseries(minute_timestep, shade)) {
        size_t n = shade.size();
        int nstep = ShadeAnalysis::surfshade::nvalues(minute_timestep);
        if (use_groups) {
            for (size_t j = 0; j < n; j++) {
                result.push_back(shadets());
                shadets &d = result[result.size() - 1];
                d.name = shade[j].group;
                std::vector<float> data(nstep);
                for (int i = 0; i < nstep; i++)
                    data[i] = shade[j].sfac[i];
                d.ts = data;
            }
        } else // return single array with all active surfaces shading factors
        {
            result.push_back(shadets());
            shadets &d = result[result.size() - 1];
            d.name = "all active surfaces";
            std::vector<float> data(nstep);
            for (int i = 0; i < nstep; i++) {
                double sf = 1;
                double shaded = 0;
                double active = 0;
                for (size_t j = 0; j < n; j++) {
                    shaded += shade[j].shaded[i];
                    active += shade[j].active[i];
                }
                if (active != 0.0)
                    sf = shaded / active;
                data[i] = 100.0f * sf;
            }
            d.ts = data;
        }
        return true;
    } else
        return false;
}


bool ShadeTool::SimulateDiurnal(std::vector<diurnal> &result) {
    result.clear();
    if (m_analysis->SimulateDiurnal()) {
        size_t n = m_analysis->GetDiurnalCount();
        for (size_t i = 0; i < n; i++) {
            result.push_back(diurnal());
            diurnal &d = result[result.size() - 1];
            m_analysis->GetDiurnal(i, &d.mxh, &d.name);
        }
        return true;
    } else
        return false;
}

bool ShadeTool::SimulateDiffuse(std::vector<diffuse> &result, bool use_groups) {
    result.clear();
    std::vector<ShadeAnalysis::surfshade> shade;
    if (m_analysis->SimulateDiffuse(shade)) {
        size_t n = m_analysis->GetDiffuseCount();
        std::vector<diffuse> diff_group;
        for (size_t i = 0; i < n; i++) {
            diff_group.push_back(diffuse());
            diffuse &d = diff_group[diff_group.size() - 1];
            m_analysis->GetDiffuse(i, &d.shade_percent, &d.shade_factor, &d.shade_count, &d.name);
        }
        if (use_groups) {
            result = diff_group;
        } else // single diffuse using all grouped values
        {
            result.push_back(diffuse());
            diffuse &d = result[result.size() - 1];
            d.name = "all active surfaces";
            d.shade_percent = 0;
            d.shade_factor = 0;
            d.shade_count = 0;
            n = diff_group.size();
            for (size_t i = 0; i < n; i++) {
                d.shade_factor += diff_group[i].shade_factor;
                d.shade_count += diff_group[i].shade_count;
            }
            if (d.shade_count > 0) {
                d.shade_percent = d.shade_factor / d.shade_count;
            }
        }
        return true;
    } else
        return false;
}

void ShadeTool::OnUpdateObjectList(wxCommandEvent &) {
    m_sceneParams->UpdateObjectList();
}

void ShadeTool::OnUpdateProperties(wxCommandEvent &) {
    m_sceneParams->UpdatePropertyValues();
}

void ShadeTool::OnUpdateSelection(wxCommandEvent &) {
    m_sceneParams->SetObject(m_view->GetFirstSelectedObject());
}

void ShadeTool::OnCommand(wxCommandEvent &evt) {
    if (!m_view) return;

    switch (evt.GetId()) {
#ifdef S3D_STANDALONE
                                                                                                                                case wxID_NEW:
		m_view->DeleteAll();
		break;
	case ID_BSPTREE:
		m_view->RebuildBSPTree();
		break;
#endif
        case wxID_SAVE:
            Save();
            break;
        case wxID_OPEN:
            if (Load()) m_book->SetSelection(PG_SCENE);
            break;
        case ID_LOCATION:
            m_book->SetSelection(PG_LOCATION);
            break;
        case ID_ANALYSIS:
            m_book->SetSelection(PG_ANALYSIS);
            break;
        case ID_SCRIPTING:
            m_book->SetSelection(PG_SCRIPTING);
            break;
        case ID_CREATE: {
#ifdef __WXGTK__
            wxMenu menu;
#else
            wxMetroPopupMenu menu;
#endif
            wxArrayString types = m_view->GetRegisteredTypes();
            for (size_t i = 0; i < types.size(); i++)
                menu.Append(ID_OBJECT_ID + i, types[i]);

#ifdef __WXGTK__
            this->PopupMenu( &menu );
#else

            wxPoint pos(wxDefaultPosition);
            if (wxWindow *win = dynamic_cast<wxWindow *>(evt.GetEventObject())) {
                pos = win->GetScreenPosition();
                pos.y += win->GetClientSize().y;
            }
            menu.Popup(this, pos/*, wxBOTTOM|wxRIGHT*/ );
#endif
            break;
        }

        case ID_VIEW_XYZ:
            m_book->SetSelection(PG_SCENE);
            m_view->SetMode(m_view->SPIN_VIEW);
            break;
        case ID_VIEW_XY:
            m_book->SetSelection(PG_SCENE);
            m_view->SetMode(m_view->TOP_VIEW);
            break;
        case ID_VIEW_XZ:
            m_book->SetSelection(PG_SCENE);
            m_view->SetMode(m_view->Z_VIEW);
            break;
        case ID_FEEDBACK:
            wxLaunchDefaultBrowser("mailto://sam.support@nrel.gov?subject=Shade Calculator - Beta Feedback");
            break;
        case wxID_HELP:
#ifdef S3D_STANDALONE

                                                                                                                                    #if defined(__WXMSW__)||defined(__WXOSX__)
		m_book->SetSelection( PG_HELP );
#else
		wxLaunchDefaultBrowser( "file:///" + m_dataPath + "/help/index.html" );
#endif

#else
            SamApp::ShowHelp("3d_shade_calculator");
#endif
            break;
    }
}

void ShadeTool::OnCreateObject(wxCommandEvent &evt) {
    size_t idx = evt.GetId() - ID_OBJECT_ID;
    wxArrayString types = m_view->GetRegisteredTypes();
    if (idx < types.Count()) {
        if (m_book->GetSelection() != PG_SCENE)
            m_book->SetSelection(PG_SCENE);

        m_view->CreateObject(types[idx]);
        m_view->Render();
        m_view->Refresh();
    }
}

void ShadeTool::OnDebugCommand(wxCommandEvent &evt) {
    switch (evt.GetId()) {
        case ID_AZIMUTH:
        case ID_ALTITUDE: {
            double az, al;
            m_txtAzi->GetValue().ToDouble(&az);
            m_txtAlt->GetValue().ToDouble(&al);
            m_view->SetAzAl(az, al);
        }
            break;
        case ID_SCALE: {
            double scale;
            m_txtScale->GetValue().ToDouble(&scale);
            m_view->SetScale(scale);
        }
            break;
        case ID_EXPORT_HOURLY:
            wxMessageBox("do analysis and save hourly factors");
            break;
        case ID_EXPORT_DIURNAL:
            wxMessageBox("do analysis and copy to clipboard 12x24 factors table");
            break;
    }
}
