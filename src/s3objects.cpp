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

#include <wx/datstrm.h>
#include <algorithm>

#include "s3objects.h"


#ifndef DTOR
#define DTOR 0.0174532925199433
#endif

VProperty::VProperty() {
    Init();
}

VProperty::VProperty(VProperty *ref) {
    Init();
    m_type = INVALID;
    m_pReference = ref;
}

VProperty::VProperty(double dp, int dim) {
    Init();
    m_type = DOUBLE;
    m_doubleVal = dp;
    m_dimension = dim;
}

VProperty::VProperty(bool bp) {
    Init();
    m_type = BOOLEAN;
    m_boolVal = bp;
}

VProperty::VProperty(int ip, int dim) {
    Init();
    m_type = INTEGER;
    m_intVal = ip;
    m_dimension = dim;
}

VProperty::VProperty(int i, const wxArrayString &choices) {
    Init();
    m_type = INTEGER;
    m_intVal = i;
    m_dimension = NONDIM;
    m_choices = choices;
}

VProperty::VProperty(const wxColour &cp) {
    Init();
    m_type = COLOUR;
    m_colour = cp;
}

VProperty::VProperty(const wxString &sp) {
    Init();
    m_type = STRING;
    m_string = sp;
}

int VProperty::GetType() {
    if (m_pReference != 0) return m_pReference->GetType();
    else return m_type;
}

void VProperty::Set(double d) {
    if (m_pReference) m_pReference->Set(d);
    else m_doubleVal = d;
}

void VProperty::Set(bool b) {
    if (m_pReference) m_pReference->Set(b);
    else m_boolVal = b;
}

void VProperty::Set(int i) {
    if (m_pReference) m_pReference->Set(i);
    else m_intVal = i;
}

void VProperty::Set(const wxColour &c) {
    if (m_pReference) m_pReference->Set(c);
    else m_colour = c;
}

void VProperty::Set(const wxString &s) {
    if (m_pReference) m_pReference->Set(s);
    else m_string = s;
}


int VProperty::GetInteger() {
    if (m_pReference) return m_pReference->GetInteger();
    else return m_intVal;
}

bool VProperty::GetBoolean() {
    if (m_pReference) return m_pReference->GetBoolean();
    else return m_boolVal;
}

double VProperty::GetDouble() {
    if (m_pReference) return m_pReference->GetDouble();
    else return m_doubleVal;
}

wxColour VProperty::GetColour() {
    if (m_pReference) return m_pReference->GetColour();
    else return m_colour;
}

wxString VProperty::GetString() {
    if (m_pReference) return m_pReference->GetString();
    else return m_string;
}

wxArrayString &VProperty::GetChoices() {
    if (m_pReference) return m_pReference->GetChoices();
    else return m_choices;
}

void VProperty::Write(wxOutputStream &_o) {
    wxDataOutputStream out(_o);
    int type = GetType();
    out.Write8(0x1d);
    out.Write16((wxUint16) type);
    switch (type) {
        case DOUBLE:
            out.WriteDouble(GetDouble());
            break;
        case BOOLEAN:
            out.Write8(GetBoolean() ? 1 : 0);
            break;
        case INTEGER:
            out.Write32(GetInteger());
            break;
        case STRING:
            out.WriteString(GetString());
            break;
        case COLOUR: {
            wxColour c = GetColour();
            out.Write8(c.Red());
            out.Write8(c.Green());
            out.Write8(c.Blue());
            out.Write8(c.Alpha());
        }
            break;
    }
    out.Write8(0x1d);
}

bool VProperty::Read(wxInputStream &_i) {
    wxDataInputStream in(_i);

    wxUint8 code = in.Read8();
    wxUint16 type = in.Read16();

    if (m_pReference)
        m_pReference->m_type = type;
    else
        m_type = type;

    wxUint8 r, g, b, a;
    switch (type) {
        case DOUBLE:
            Set(in.ReadDouble());
            break;
        case BOOLEAN:
            Set(in.Read8() != 0 ? true : false);
            break;
        case INTEGER:
            Set((int) in.Read32());
            break;
        case STRING:
            Set(in.ReadString());
            break;
        case COLOUR:
            r = in.Read8();
            g = in.Read8();
            b = in.Read8();
            a = in.Read8();
            Set(wxColour(r, g, b, a));
            break;
    }

    return (code == in.Read8());
}

void VProperty::Init() {
    m_dimension = NONDIM;
    m_type = INVALID;
    m_pReference = 0;
    m_doubleVal = 0.0;
    m_intVal = 0;
    m_boolVal = false;
}

int VProperty::GetDimension() {
    return m_dimension;
}

static int gs_idCounter = 0;

VObject::VObject() {
    ++gs_idCounter;
    m_id = gs_idCounter;
    m_visible = true;
    AddProperty("Name", new VProperty(wxString::Format("untitled %d", m_id)));
}

VObject::~VObject() {
    DeleteProperties();
    DeleteHandles();
}

void VObject::DeleteHandles() {
    for (size_t i = 0; i < m_handles.size(); i++)
        delete m_handles[i];
    m_handles.clear();
}

void VObject::DeleteProperties() {
    for (size_t i = 0; i < m_properties.size(); i++)
        delete m_properties[i].prop;
    m_properties.clear();
}

void VObject::SetupHandles(VPlaneType) {
    // nothing to do
}

bool VObject::OnHandleMoved(VHandle *, VPlaneType) {
    return false;
}

bool VObject::IsWithin(double, double, VPlaneType) {
    return false;
}

void VObject::DrawOnPlane(VRenderer2D &, VPlaneType) {
    // nothing to do
}

bool VObject::Copy(VObject *rhs) {
    DeleteProperties();
    for (size_t i = 0; i < rhs->m_properties.size(); i++) {
        propdata x;
        x.name = rhs->m_properties[i].name;
        x.lowered = rhs->m_properties[i].lowered;
        x.prop = new VProperty(*rhs->m_properties[i].prop);
        m_properties.push_back(x);
    }

    return true;
}

int VObject::GetId() {
    return m_id;
}

VProperty &VObject::Property(const wxString &name) {
    static VProperty s_nullProp;

    wxString lowered = name.Lower();
    for (size_t i = 0; i < m_properties.size(); i++)
        if (lowered == m_properties[i].lowered)
            return *m_properties[i].prop;

    return s_nullProp;
}

wxArrayString VObject::Properties() {
    wxArrayString list;
    for (size_t i = 0; i < m_properties.size(); i++)
        list.Add(m_properties[i].name);

    return list;
}

std::vector<VHandle *> VObject::GetHandles() {
    return m_handles;
}

void VObject::Write(wxOutputStream &_o) {
    wxDataOutputStream out(_o);
    out.Write8(0xaf); // start code
//	out.Write8(1); // version
    out.Write8(2); // version - change from Group to Subarray and String

    out.Write8(m_visible ? 1 : 0);

    out.Write32(m_properties.size());
    for (size_t i = 0; i < m_properties.size(); i++) {
        out.WriteString(m_properties[i].name);
        m_properties[i].prop->Write(_o);
    }

    out.Write8(0xaf);
}

bool VObject::Read(wxInputStream &_i) {
    wxDataInputStream in(_i);
    wxUint8 code = in.Read8();
    wxUint8 ver = in.Read8(); // version

    m_visible = in.Read8() != 0;

    size_t n = in.Read32();
    for (size_t i = 0; i < n; i++) {
        wxString name = in.ReadString();
        Property(name).Read(_i);
        if (ver == 1 && name.Lower() == "group") { // update to subarray and string
            wxString grp = Property(name).GetString();
            int ndx = wxNOT_FOUND;
            wxString num_string = "0123456789";
            wxString nn;
            for (size_t j = 0; j < grp.Len(); j++) {
                ndx = num_string.Find(grp.Mid(j, 1));
                if (ndx != wxNOT_FOUND)
                    nn += grp.Mid(j, 1);
            }
            // default subarray is 1 with range 1 to 4 (zero index)
            // default string is 1 with range 1 to 8 (zero index)
            // no string entries before version 2
            int nsub = wxAtoi(nn);
            if (nsub < 1) nsub = 1;
            if (nsub > 4) nsub = 1;

            Property("Subarray").Set(nsub - 1);
            Property("String").Set(0);
        }
    }

    return in.Read8() == code;
}


void VObject::AddProperty(const wxString &name, VProperty *prop) {
    propdata x;
    x.name = name;
    x.lowered = name.Lower();
    x.prop = prop;
    m_properties.push_back(x);
}

VHandle *VObject::AddHandle(int id, double x, double yz, const wxCursor &curs, const wxString &name) {
    VHandle *h = new VHandle(this, id, x, yz, curs, name);
    m_handles.push_back(h);
    return h;
}

VHandle::VHandle(VObject *o, int id, double x, double yz, const wxCursor &curs, const wxString &name) {
    m_object = o;
    m_id = id;
    m_x = m_origX = x;
    m_yz = m_origYZ = yz;
    m_name = name;
    m_cursor = curs;
}

VObject *VHandle::GetObject() { return m_object; }

void VHandle::GetCurrentPos(double *x, double *yz) {
    *x = m_x;
    *yz = m_yz;
}

double VHandle::GetX() {
    return m_x;
}

double VHandle::GetYZ() {
    return m_yz;
}

double VHandle::GetDeltaX() {
    return m_x - m_origX;
}

double VHandle::GetDeltaYZ() {
    return m_yz - m_origYZ;
}

double VHandle::GetDistance() {
    double dx = GetDeltaX();
    double dyz = GetDeltaYZ();
    return sqrt(dx * dx + dyz * dyz);
}

wxString VHandle::GetName() {
    return m_name;
}

int VHandle::GetId() {
    return m_id;
}

wxCursor VHandle::GetCursor() {
    return m_cursor;
}

void VHandle::MoveTo(double x, double yz) {
    m_x = x;
    m_yz = yz;
}


VTreeObject::VTreeObject() {
    AddProperty("X", new VProperty(0.0, LENGTH));
    AddProperty("Y", new VProperty(0.0, LENGTH));
    AddProperty("Diameter", new VProperty(28.0, LENGTH));
    AddProperty("Height", new VProperty(66.0, LENGTH));
    AddProperty("Top Diameter", new VProperty(8.0, LENGTH));
    AddProperty("Trunk Height", new VProperty(10.0, LENGTH));
    wxArrayString shapes;
    shapes.Add("Rounded");
    shapes.Add("Conical");
    AddProperty("Shape", new VProperty(ROUNDED, shapes));
}

wxString VTreeObject::GetTypeName() {
    return "Tree";
}

VObject *VTreeObject::Duplicate() {
    VTreeObject *tree = new VTreeObject;
    tree->Copy(this);
    return tree;
}

void VTreeObject::BuildModel(s3d::scene &sc) {
    int shape = Property("Shape").GetInteger();
    double X = Property("X").GetDouble();
    double Y = Property("Y").GetDouble();
    double D = Property("Diameter").GetDouble();
    double H = Property("Height").GetDouble();
    double TD = Property("Top Diameter").GetDouble();
    double TH = Property("Trunk Height").GetDouble();
    double TRR = D / 8;

    static const s3d::rgba ctrunk(128, 64, 0, 155);
    static const s3d::rgba cfoliage(0, 186, 107, 155);

    if (shape == ROUNDED) {
        int id = GetId();

        sc.reset();
        sc.colors(ctrunk, ctrunk);
        sc.conical(id, X, Y, 0, TH, TRR, TRR, 10, true, false);

        double TREMU = (H - TH) / 5;

        sc.colors(cfoliage, cfoliage);
        sc.conical(id, X, Y, TH, TREMU, TRR, D / 2, 10, false, false);
        sc.conical(id, X, Y, TH + TREMU, TREMU * 3, D / 2, D / 2, 10, false, false);
        sc.conical(id, X, Y, H - TREMU, TREMU, D / 2, TD / 2, 10, false, true);
    } else {

        sc.reset();
        sc.colors(ctrunk, ctrunk);
        sc.conical(GetId(), X, Y, 0, TH, D / 6, D / 6, 10, true, false);
        sc.colors(cfoliage, cfoliage);
        sc.conical(GetId(), X, Y, TH, H - TH, D / 2, TD / 2, 10, true, true);
    }
}

void VTreeObject::SetupHandles(VPlaneType plane) {
    double xc = Property("X").GetDouble();
    double diam = Property("Diameter").GetDouble();
    double topDiam = Property("Top Diameter").GetDouble();

    // top view
    if (plane == PLANE_XY) {
        double yc = Property("Y").GetDouble();
        AddHandle(HH_MOVE, xc, yc);
        AddHandle(HH_DIAM, xc + diam / 2, yc, wxCURSOR_PENCIL);
        AddHandle(HH_TOPDIAM, xc + topDiam / 2, yc, wxCURSOR_PENCIL);
    }
        // side view
    else if (plane == PLANE_XZ) {
        double trunk = Property("Trunk Height").GetDouble();
        double height = Property("Height").GetDouble();
        AddHandle(HH_MOVE, xc, 0);
        AddHandle(HH_DIAM, xc + diam / 2, trunk, wxCURSOR_PENCIL);
        AddHandle(HH_TOPDIAM, xc + topDiam / 2, height, wxCURSOR_PENCIL);
        AddHandle(HH_TRUNK, xc, trunk, wxCURSOR_PENCIL);
        AddHandle(HH_HEIGHT, xc, height, wxCURSOR_PENCIL);
    }
}

bool VTreeObject::OnHandleMoved(VHandle *h, VPlaneType plane) {
    int id = h->GetId();
    if (id == HH_MOVE) {
        Property("X").Set(h->GetX());
        if (plane == PLANE_XY) Property("Y").Set(h->GetYZ());
        return true;
    }
        // Deal with both the bottom and top diameters
    else if (id == HH_DIAM || id == HH_TOPDIAM) {
        if (plane == PLANE_XY) {
            double xc = Property("X").GetDouble();
            double yc = Property("Y").GetDouble();
            double x, y;
            h->GetCurrentPos(&x, &y);
            double radius = sqrt((x - xc) * (x - xc) + (y - yc) * (y - yc));
            (id == HH_DIAM) ? Property("Diameter").Set(radius * 2.0) : Property("Top Diameter").Set(radius * 2.0);
        } else if (plane == PLANE_XZ) {
            double xc = Property("X").GetDouble();
            (id == HH_DIAM) ? Property("Diameter").Set(fabs(h->GetX() - xc) * 2.0) : Property("Top Diameter").Set(
                    fabs(h->GetX() - xc) * 2.0);
        }
        return true;
    } else if (id == HH_TRUNK) {
        Property("Trunk Height").Set(h->GetYZ());
        return true;
    } else if (id == HH_HEIGHT) {
        Property("Height").Set(h->GetYZ());
        return true;
    }

    return false;
}

size_t VTreeObject::GetXZPoints(double x[10], double z[10]) {
    int shape = Property("Shape").GetInteger();
    double X = Property("X").GetDouble();
//	double Y = Property("Y").GetDouble();
    double D = Property("Diameter").GetDouble();
    double H = Property("Height").GetDouble();
    double TD = Property("Top Diameter").GetDouble();
    double TH = Property("Trunk Height").GetDouble();
    double TRR = D / 8;

    if (ROUNDED == shape) {
        double TREMU = (H - TH) / 5;

        // left top
        x[0] = X - TD / 2;
        z[0] = H;

        // left upper
        x[1] = X - D / 2;
        z[1] = H - TREMU;

        // left lower
        x[2] = X - D / 2;
        z[2] = TH + TREMU;

        // left inner
        x[3] = X - TRR;
        z[3] = TH;

        // left bottom
        x[4] = X - TRR;
        z[4] = 0;

        // right bottom
        x[5] = X + TRR;
        z[5] = 0;

        // right inner
        x[6] = X + TRR;
        z[6] = TH;

        // right lower
        x[7] = X + D / 2;
        z[7] = TH + TREMU;

        // right upper
        x[8] = X + D / 2;
        z[8] = H - TREMU;

        // right top
        x[9] = X + TD / 2;
        z[9] = H;

        return 10;
    } else {    // Conical Tree

        // top left point
        x[0] = X - TD / 2;
        z[0] = H;

        // left point
        x[1] = X - D / 2;
        z[1] = TH;

        // inner left point
        x[2] = X - TRR;
        z[2] = TH;

        // bottom left point
        x[3] = X - TRR;
        z[3] = 0.0;

        // bottom right point
        x[4] = X + TRR;
        z[4] = 0.0;

        // inner right point
        x[5] = X + TRR;
        z[5] = TH;

        // right point
        x[6] = X + D / 2;
        z[6] = TH;

        if (TD > 0) {
            // top right point
            x[7] = X + TD / 2;
            z[7] = H;
        }

        return TD > 0 ? 8 : 7;
    }
}

void VTreeObject::DrawOnPlane(VRenderer2D &dc, VPlaneType plane) {

    if (plane == PLANE_XY) {
        double xc = Property("X").GetDouble();
        double yc = Property("Y").GetDouble();
        double diam = Property("Diameter").GetDouble();
        double top_diam = Property("Top Diameter").GetDouble();
        dc.Circ(xc, yc, diam / 2);
        dc.Circ(xc, yc, top_diam / 2);

    } else if (plane == PLANE_XZ) {
        double x[10], z[10];
        size_t n = GetXZPoints(x, z);
        dc.Poly(x, z, n);
    }
}

bool VTreeObject::IsWithin(double x, double y, VPlaneType plane) {
    if (plane == PLANE_XY) {
        double xc = Property("X").GetDouble();
        double yc = Property("Y").GetDouble();
        double diam = Property("Diameter").GetDouble();

        return (sqrt((x - xc) * (x - xc) + (y - yc) * (y - yc)) < diam / 2);
    } else {
        double XX[10], ZZ[10];
        size_t n = GetXZPoints(XX, ZZ);
        return s3d::inpoly(XX, ZZ, n, x, y);
    }
}


/*

VRoundTreeObject::VRoundTreeObject()
{
	AddProperty( "X", new VProperty( 0.0, LENGTH ) );
	AddProperty( "Y", new VProperty( 0.0, LENGTH ) );
	AddProperty( "Diameter", new VProperty( 35.0, LENGTH ) );
	AddProperty( "Height", new VProperty( 45.0, LENGTH ) );
}

wxString VRoundTreeObject::GetTypeName()
{
	return "Tree, round";
}

VObject *VRoundTreeObject::Duplicate()
{
	VRoundTreeObject *t = new VRoundTreeObject;
	t->Copy( this );
	return t;
}

void VRoundTreeObject::BuildModel( s3d::scene &sc )
{
	double X = Property("X").GetDouble();
	double Y = Property("Y").GetDouble();
	double D = Property("Diameter").GetDouble();
	double H = Property("Height").GetDouble();
	int id = GetId();

	double TRr = D/8;

	sc.reset();
	sc.colors(s3d::rgba(128, 64, 0, 155), s3d::rgba(128, 64, 0, 155));
	sc.conical( id, X, Y, 0, H/6, TRr, TRr, 10, true, false);

	sc.colors( s3d::rgba(0, 186, 107, 155 ), s3d::rgba(0, 186, 107, 155 ) );

	sc.conical( id, X, Y, H/6, H/6, TRr, D/2, 10, false, false );
	sc.conical( id, X, Y, H/3, H/2, D/2, D/2, 10, false, false );
	sc.conical( id, X, Y, 5*H/6, H/6, D/2, TRr, 10, false, true );
}

void VRoundTreeObject::SetupHandles( VPlaneType plane )
{
	double X = Property("X").GetDouble();
	double D = Property("Diameter").GetDouble();

	// top view
	if ( plane == PLANE_XY )
	{
		double Y = Property("Y").GetDouble();
		AddHandle( HH_MOVE, X, Y );
		AddHandle( HH_DIAM, X + D/2, Y, wxCURSOR_PENCIL );
	}
	// side view
	else if ( plane == PLANE_XZ )
	{
		double H = Property("Height").GetDouble();
		AddHandle( HH_MOVE, X, 0 );
		AddHandle( HH_DIAM, X+D/2, H/2, wxCURSOR_PENCIL );
		AddHandle( HH_HEIGHT, X, H, wxCURSOR_PENCIL );
	}
}

bool VRoundTreeObject::OnHandleMoved( VHandle *h, VPlaneType plane )
{

	int id = h->GetId();
	if ( id == HH_MOVE )
	{
		Property("X").Set( h->GetX() );
		if ( plane == PLANE_XY ) Property("Y").Set( h->GetYZ() );
		return true;
	}
	// Deal with both the bottom and top diameters
	else if ( id == HH_DIAM )
	{
		if ( plane == PLANE_XY )
		{
			double xc = Property("X").GetDouble();
			double yc = Property("Y").GetDouble();
			double x, y;
			h->GetCurrentPos( &x, &y );
			double radius = sqrt( (x-xc)*(x-xc) + (y-yc)*(y-yc) );
			Property("Diameter").Set( radius * 2.0 );
		}
		else if ( plane == PLANE_XZ )
		{
			double xc = Property("X").GetDouble();
			Property("Diameter").Set( fabs(h->GetX() - xc)*2.0 );
		}
		return true;
	}
	else if ( id == HH_HEIGHT )
	{
		Property("Height").Set( h->GetYZ() );
		return true;
	}

	return false;
}

void VRoundTreeObject::DrawOnPlane( VRenderer2D &dc, VPlaneType plane )
{
	if ( plane == PLANE_XY )
	{
		double XX = Property("X").GetDouble();
		double YY = Property("Y").GetDouble();
		double D = Property("Diameter").GetDouble();
		dc.Circ( XX, YY, D/2 );
	}
	else if ( plane == PLANE_XZ )
	{
		double XX[10], YY[10];
		GetXZPoints( XX, YY );
		dc.Poly( XX, YY, 10 );
	}
}

bool VRoundTreeObject::IsWithin( double x, double y, VPlaneType plane )
{
	if ( plane == PLANE_XY )
	{
		double XX = Property("X").GetDouble();
		double YY = Property("Y").GetDouble();
		double D = Property("Diameter").GetDouble();
		return ( sqrt( (x-XX)*(x-XX) + (y-YY)*(y-YY) ) < D/2 );

	}
	else if ( plane == PLANE_XZ )
	{
		double XX[10], YY[10];
		GetXZPoints( XX, YY );
		return s3d::inpoly( XX, YY, 10, x, y );
	}

	return false;
}
*/

VBoxObject::VBoxObject() {
    AddProperty("X", new VProperty(0.0, LENGTH));
    AddProperty("Y", new VProperty(0.0, LENGTH));
    AddProperty("Z", new VProperty(0.0, LENGTH));
    AddProperty("Width", new VProperty(15.0, LENGTH));
    AddProperty("Length", new VProperty(15.0, LENGTH));
    AddProperty("Height", new VProperty(15.0, LENGTH));
    AddProperty("Rotation", new VProperty(0.0));
    AddProperty("Color", new VProperty(wxColour(155, 0, 0, 144)));
    AddProperty("Sides only", new VProperty(false));
}

wxString VBoxObject::GetTypeName() {
    return "Box";
}

VObject *VBoxObject::Duplicate() {
    VBoxObject *box = new VBoxObject;
    box->Copy(this);
    return box;
}

void VBoxObject::BuildModel(s3d::scene &sc) {
    double x = Property("X").GetDouble();
    double y = Property("Y").GetDouble();
    double z = Property("Z").GetDouble();
    double w = Property("Width").GetDouble();
    double l = Property("Length").GetDouble();
    double h = Property("Height").GetDouble();
    double r = Property("Rotation").GetDouble();
    bool sides = Property("Sides only").GetBoolean();

    wxColour cc = Property("Color").GetColour();
    s3d::rgba scc1(cc.Red(), cc.Green(), cc.Blue(), cc.Alpha());
    s3d::rgba scc2(cc.Red(), cc.Green(), cc.Blue(), 255);
    sc.reset();
    sc.colors(scc1, scc2);
    sc.type(s3d::scene::OBSTRUCTION);

    unsigned int faces = s3d::ALL_FACES;
    if (sides) {
        sc.nocull(true);
        faces = s3d::SIDES;
    }
    sc.box(GetId(), x, y, z, r, w, l, h, faces);

}

void VBoxObject::SetupHandles(VPlaneType plane) {

    double x = Property("X").GetDouble();
    double y = Property("Y").GetDouble();
    double z = Property("Z").GetDouble();
    double w = Property("Width").GetDouble();
    double l = Property("Length").GetDouble();
    double h = Property("Height").GetDouble();
    double r = Property("Rotation").GetDouble();

    double xx[5], yy[5];
    xx[0] = x + w;
    yy[0] = y + l / 2;    //HH_RIGHT
    xx[1] = x + w / 2;
    yy[1] = y + l;      // HH_TOP
    xx[2] = x + 3 * w / 4;
    yy[2] = y;      // HH_ROTATE_XY
    xx[3] = x;
    yy[3] = y + l / 2;    //HH_LEFT
    xx[4] = x + w / 2;
    yy[4] = y;   // HH_BOTTOM
    s3d::rotate2dxz(x, y, xx, yy, r, 5);

    if (plane == PLANE_XY) {
        AddHandle(HH_MOVE, x, y);
        AddHandle(HH_RIGHT, xx[0], yy[0], wxCURSOR_PENCIL);
        AddHandle(HH_TOP, xx[1], yy[1], wxCURSOR_PENCIL);
        AddHandle(HH_ROTATE_XY, xx[2], yy[2], wxCURSOR_BULLSEYE);
        AddHandle(HH_LEFT, xx[3], yy[3], wxCURSOR_PENCIL);
        AddHandle(HH_BOTTOM, xx[4], yy[4], wxCURSOR_PENCIL);

    } else if (plane == PLANE_XZ) {
        AddHandle(HH_MOVE, x, z);
        AddHandle(HH_TOP, x + w / 2, z + h, wxCURSOR_PENCIL);
        AddHandle(HH_BOTTOM, x + w / 2, z, wxCURSOR_PENCIL);
    }
}

static void rot2(double &x, double &y, double rot) {
    double xx = x;
    double yy = y;
    double r = rot * M_PI / 180.0;
    x = xx * cos(r) - yy * sin(r);
    y = xx * sin(r) + yy * cos(r);
}

static double proj(double x0, double y0, double x1, double y1, double mx, double my) {
    /* get the length of the projection along a line L
        defined by (x0,y0)->(x1,y1)
        from the point (mx,my) */
    double xx0 = x1 - x0;
    double yy0 = y1 - y0;

    double xx1 = mx - x0;
    double yy1 = my - y0;

    double scale = (xx1 * xx0 + yy1 * yy0) / (xx0 * xx0 + yy0 * yy0);
    double projx = scale * xx0;
    double projy = scale * yy0;
    double len = sqrt(projx * projx + projy * projy);
    if (len < 0.1) len = 0.1;

    return len;
}

bool VBoxObject::OnHandleMoved(VHandle *hh, VPlaneType plane) {
    // extract previous properties
    double bx = Property("X").GetDouble();
    double by = Property("Y").GetDouble();
    double bz = Property("Z").GetDouble();
    double bw = Property("Width").GetDouble();
    double bl = Property("Length").GetDouble();
    double bh = Property("Height").GetDouble();
    double br = Property("Rotation").GetDouble();

    int id = hh->GetId();
    double x = hh->GetX();
    double y = hh->GetYZ();
    double z = hh->GetYZ();


    if (id == HH_MOVE) {
        Property("X").Set(x);
        if (plane == PLANE_XY) Property("Y").Set(y);
        else Property("Z").Set(z);
        return true;
    } else if (id == HH_RIGHT) {
        double xx = bw;
        double yy = 0;
        rot2(xx, yy, br);
        double len = proj(bx, by, bx + xx, by + yy, x, y);
        Property("Width").Set(len);
        return true;
    } else if (id == HH_TOP) {
        // compute new center and height/depth
        if (plane == PLANE_XY) {
            double xx = 0;
            double yy = bh;
            rot2(xx, yy, br);
            double len = proj(bx, by, bx + xx, by + yy, x, y);
            Property("Length").Set(len);
        } else
            Property("Height").Set(z - bz);

        return true;
    } else if (id == HH_LEFT) {
        double xx = bw;
        double yy = 0;
        rot2(xx, yy, br);
        double len = proj(bx + xx, by + yy, bx, by, x, y);
        Property("X").Set(bx - (len - bw) * cos(br * M_PI / 180));
        Property("Y").Set(by - (len - bw) * sin(br * M_PI / 180));
        Property("Width").Set(len);
        return true;
    } else if (id == HH_BOTTOM) {
        if (plane == PLANE_XY) {
            double xx = 0;
            double yy = bl;
            rot2(xx, yy, br);
            double len = proj(bx + xx, by + yy, bx, by, x, y);
            Property("X").Set(bx + (len - bl) * sin(br * M_PI / 180));
            Property("Y").Set(by - (len - bl) * cos(br * M_PI / 180));
            Property("Length").Set(len);
        } else {
            double z0 = bz + bh;
            double h = fabs(z - z0);
            Property("Z").Set(z0 - h);
            Property("Height").Set(h);
        }

        return true;
    } else if (id == HH_ROTATE_XY) {
        double r = (180.0 / M_PI) * atan2(hh->GetYZ() - by, hh->GetX() - bx);
        Property("Rotation").Set(r /*deg*/);
    }

    return false;
}

void VBoxObject::DrawOnPlane(VRenderer2D &dc, VPlaneType plane) {
    double x = Property("X").GetDouble();
    double y = Property("Y").GetDouble();
    double z = Property("Z").GetDouble();
    double w = Property("Width").GetDouble();
    double l = Property("Length").GetDouble();
    double h = Property("Height").GetDouble();
    double r = Property("Rotation").GetDouble();

    if (plane == PLANE_XY) {
        double xx[4], yy[4];
        s3d::get_rotated_box_points(x, y, w, l, r, xx, yy);
        dc.Poly(xx, yy, 4);
    } else if (plane == PLANE_XZ) {
        double minDim = std::min(w, l);
        double maxDim = std::max(w, l);
        double dDim = maxDim - minDim;
        double xDim = minDim + fabs(cos(r * M_PI / 180)) * dDim;

        dc.Rect(x, z, xDim, h);
    }
}

bool VBoxObject::IsWithin(double px, double pyz, VPlaneType plane) {
    double x = Property("X").GetDouble();
    double y = Property("Y").GetDouble();
    double z = Property("Z").GetDouble();
    double w = Property("Width").GetDouble();
    double l = Property("Length").GetDouble();
    double h = Property("Height").GetDouble();
    double r = Property("Rotation").GetDouble();

    if (plane == PLANE_XY) {
        double xx[4], yy[4];
        s3d::get_rotated_box_points(x, y, w, l, r, xx, yy);
        return s3d::inpoly(xx, yy, 4, px, pyz);

    } else if (plane == PLANE_XZ) {
        return (px >= x && px <= x + w
                && pyz >= z && pyz <= z + h);
    }

    return false;
}


VActiveSurfaceObject::VActiveSurfaceObject() {
    wxArrayString subarrays;
    subarrays.Add("1");
    subarrays.Add("2");
    subarrays.Add("3");
    subarrays.Add("4");
    AddProperty("Subarray", new VProperty(0, subarrays));
    wxArrayString strings;
    strings.Add("1");
    strings.Add("2");
    strings.Add("3");
    strings.Add("4");
    strings.Add("5");
    strings.Add("6");
    strings.Add("7");
    strings.Add("8");
    AddProperty("String", new VProperty(0, strings));
    AddProperty("Group", new VProperty(wxString("")));
    AddProperty("X", new VProperty(0.0, LENGTH));
    AddProperty("Y", new VProperty(0.0, LENGTH));
    AddProperty("Z", new VProperty(0.0, LENGTH));
    AddProperty("Width", new VProperty(25.0, LENGTH));
    AddProperty("Length", new VProperty(10.0, LENGTH));
    AddProperty("Azimuth", new VProperty(180.0));
    AddProperty("Tilt", new VProperty(30.0));
    wxArrayString shapes;
    shapes.Add("Rectangle");
    shapes.Add("Triangle");
    AddProperty("Shape", new VProperty(0, shapes));
}

wxString VActiveSurfaceObject::GetTypeName() {
    return "Active surface";
}

VObject *VActiveSurfaceObject::Duplicate() {
    VActiveSurfaceObject *p = new VActiveSurfaceObject;
    p->Copy(this);
    return p;
}


void VActiveSurfaceObject::TiltAndAzimuth(double n_points, double tilt, double azimuth,
                                          double x0, double y0, double z0,
                                          double x[], double y[], double z[]) {
    // offset and then tilt about x-axis
    s3d::rotate2dxz(y0, z0, y, z, -tilt, n_points);
    // center and rotate about z-axis
    s3d::rotate2dxz(x0, y0, x, y, -azimuth, n_points);
}

void VActiveSurfaceObject::GetPoints(double xx[4], double yy[4], double zz[4]) {
    double x = Property("X").GetDouble();
    double y = Property("Y").GetDouble();
    double z = Property("Z").GetDouble();
    double w = Property("Width").GetDouble();
    double l = Property("Length").GetDouble();
    double azimuth = Property("Azimuth").GetDouble();
    double tilt = Property("Tilt").GetDouble();

    xx[0] = x;
    yy[0] = y;
    zz[0] = z;
    xx[1] = x;
    yy[1] = y - l;
    zz[1] = z;
    xx[2] = x - w;
    yy[2] = y - l;
    zz[2] = z;
    xx[3] = x - w;
    yy[3] = y;
    zz[3] = z;
    TiltAndAzimuth(4, tilt, azimuth, x, y, z, xx, yy, zz);
}

void VActiveSurfaceObject::BuildModel(s3d::scene &sc) {
    double xx[4], yy[4], zz[4];
    GetPoints(xx, yy, zz);
    sc.reset();

    std::vector<s3d::point3d> points;

    int shape = Property("Shape").GetInteger();
    if (shape == 0) { // rectangular
        for (int i = 0; i < 4; i++) points.push_back(s3d::point3d(xx[i], yy[i], zz[i]));
    } else if (shape == 1) { // triangular
        points.push_back(s3d::point3d(0.5 * (xx[2] + xx[1]),
                                      0.5 * (yy[2] + yy[1]),
                                      0.5 * (zz[2] + zz[1])));
        points.push_back(s3d::point3d(xx[3], yy[3], zz[3]));
        points.push_back(s3d::point3d(xx[0], yy[0], zz[0]));
    }

    // generate front of panel
    sc.poly(GetId(), s3d::scene::ACTIVE, s3d::rgba(0, 107, 186, 170), s3d::rgba(0, 88, 153, 170), 1, false, points);

    // generate back of panel
    std::reverse(points.begin(), points.end());
    sc.poly(GetId(), s3d::scene::OBSTRUCTION, s3d::rgba(0, 50, 100, 170), s3d::rgba(0, 50, 100, 170), 1, false, points);

}

void VActiveSurfaceObject::SetupHandles(VPlaneType plane) {
    double x0 = Property("X").GetDouble();
    double y0 = Property("Y").GetDouble();
    double z0 = Property("Z").GetDouble();
    double w = Property("Width").GetDouble();
    double l = Property("Length").GetDouble();
    double azimuth = Property("Azimuth").GetDouble();
    double tilt = Property("Tilt").GetDouble();

    double x[6], y[6], z[6];

    for (int i = 0; i < 6; i++) z[i] = z0;

    x[0] = x0;
    y[0] = y0;       // HH_MOVE
    x[1] = x0;
    y[1] = y0 - l / 2;   // HH_LEFT
    x[2] = x0 - w / 2;
    y[2] = y0;       // HH_BOTTOM
    x[3] = x0 - 3 * w / 4;
    y[3] = y0;       // HH_AZIMUTH
    x[4] = x0 - w;
    y[4] = y0 - l / 2;   // HH_RIGHT
    x[5] = x0 - w / 2;
    y[5] = y0 - l;     // HH_TOP

    // Rotate the default handle positions by tilt and azimuth
    TiltAndAzimuth(6, tilt, azimuth, x0, y0, z0, x, y, z);

    if (plane == PLANE_XY) {
        AddHandle(HH_MOVE, x[0], y[0]);
        AddHandle(HH_LEFT, x[1], y[1], wxCURSOR_PENCIL);
        AddHandle(HH_AZIMUTH, x[3], y[3], wxCURSOR_BULLSEYE);
        AddHandle(HH_RIGHT, x[4], y[4], wxCURSOR_PENCIL);
        AddHandle(HH_TOP, x[5], y[5], wxCURSOR_PENCIL);
    } else if (plane == PLANE_XZ) {
        AddHandle(HH_MOVE, x[0], z[0]);
    }
}

bool VActiveSurfaceObject::OnHandleMoved(VHandle *h, VPlaneType plane) {
    int id = h->GetId();
    if (id == HH_MOVE) {
        Property("X").Set(h->GetX());
        if (plane == PLANE_XY) Property("Y").Set(h->GetYZ());
        else Property("Z").Set(h->GetYZ());
        return true;
    }

    // Other properties occur only in XY plane
    if (plane == PLANE_XY) {
        double X = Property("X").GetDouble();
        double Y = Property("Y").GetDouble();
//		double Z = Property("Z").GetDouble();
        double W = Property("Width").GetDouble();
//		double L = Property("Length").GetDouble();
        double A = Property("Azimuth").GetDouble();
        double T = Property("Tilt").GetDouble();

        double xx[4], yy[4], zz[4];
        GetPoints(xx, yy, zz);

        if (id == HH_TOP) {
            double len = proj(0.5 * (xx[3] + xx[0]), 0.5 * (yy[3] + yy[0]),
                              0.5 * (xx[1] + xx[2]), 0.5 * (yy[1] + yy[2]),
                              h->GetX(), h->GetYZ());
            if (T < 89)
                Property("Length").Set(len / cos(T * M_PI / 180.0));
        } else if (id == HH_RIGHT) {
            double len = proj(0.5 * (xx[1] + xx[0]), 0.5 * (yy[1] + yy[0]),
                              0.5 * (xx[3] + xx[2]), 0.5 * (yy[3] + yy[2]),
                              h->GetX(), h->GetYZ());
            Property("Width").Set(len);
        } else if (id == HH_LEFT) {
            double len = proj(0.5 * (xx[3] + xx[2]), 0.5 * (yy[3] + yy[2]),
                              0.5 * (xx[1] + xx[0]), 0.5 * (yy[1] + yy[0]),
                              h->GetX(), h->GetYZ());

            double r = 180 - A;

            Property("X").Set(X - (len - W) * cos(r * M_PI / 180));
            Property("Y").Set(Y - (len - W) * sin(r * M_PI / 180));
            Property("Width").Set(len);
        } else if (id == HH_AZIMUTH) {
            double dx = h->GetX() - X;
            double dy = h->GetYZ() - Y;
            double azimuth = (180 / M_PI) * atan2(-dy, dx) + 180;
            if (azimuth < 0) azimuth += 360;
            Property("Azimuth").Set(azimuth);
        }
    }

    return false;
}

void VActiveSurfaceObject::DrawOnPlane(VRenderer2D &dc, VPlaneType plane) {
    double xx[4], yy[4], zz[4];
    GetPoints(xx, yy, zz);
    if (plane == PLANE_XY) dc.Poly(xx, yy, 4);
    else dc.Poly(xx, zz, 4);
}

bool VActiveSurfaceObject::IsWithin(double xt, double yt, VPlaneType plane) {
    double xx[4], yy[4], zz[4];
    GetPoints(xx, yy, zz);
    if (plane == PLANE_XY) return s3d::inpoly(xx, yy, 4, xt, yt);
    else return s3d::inpoly(xx, zz, 4, xt, yt);
}


VCylinderObject::VCylinderObject() {
    AddProperty("X", new VProperty(0.0, LENGTH));
    AddProperty("Y", new VProperty(0.0, LENGTH));
    AddProperty("Z", new VProperty(0.0, LENGTH));
    AddProperty("Diameter", new VProperty(6.0, LENGTH));
    AddProperty("Height", new VProperty(15.0, LENGTH));
    AddProperty("Color", new VProperty(wxColour(255, 127, 39, 155)));
}

wxString VCylinderObject::GetTypeName() {
    return "Cylinder";
}

VObject *VCylinderObject::Duplicate() {
    VCylinderObject *p = new VCylinderObject;
    p->Copy(this);
    return p;
}

void VCylinderObject::BuildModel(s3d::scene &sc) {
    double x = Property("X").GetDouble();
    double y = Property("Y").GetDouble();
    double z = Property("Z").GetDouble();
    double diam = Property("Diameter").GetDouble();
    double height = Property("Height").GetDouble();
    wxColour cc = Property("Color").GetColour();
    s3d::rgba scc1(cc.Red(), cc.Green(), cc.Blue(), cc.Alpha());
    sc.reset();
    sc.colors(scc1, scc1);
    sc.conical(GetId(), x, y, z, height, diam / 2, diam / 2);
}

void VCylinderObject::SetupHandles(VPlaneType plane) {
    double x = Property("X").GetDouble();
    double diam = Property("Diameter").GetDouble();

    if (plane == PLANE_XY) {
        double y = Property("Y").GetDouble();

        AddHandle(HH_MOVE, x, y);
        AddHandle(HH_DIAM, x + diam / 2, y, wxCURSOR_PENCIL);
    } else if (plane == PLANE_XZ) {
        double z = Property("Z").GetDouble();
        double height = Property("Height").GetDouble();

        AddHandle(HH_MOVE, x, z + height / 2);
        AddHandle(HH_DIAM, x + diam / 2, z + height / 2, wxCURSOR_PENCIL);
        AddHandle(HH_BOTTOM, x, z, wxCURSOR_PENCIL);
        AddHandle(HH_TOP, x, z + height, wxCURSOR_PENCIL);
    }
}

bool VCylinderObject::OnHandleMoved(VHandle *h, VPlaneType plane) {
    int id = h->GetId();
    double X = Property("X").GetDouble();
    double Z = Property("Z").GetDouble();
    double H = Property("Height").GetDouble();
    double mx = h->GetX();
    double myz = h->GetYZ();

    if (id == HH_DIAM) {
        Property("Diameter").Set(2 * (mx - X));
    } else if (plane == PLANE_XY) {
        if (id == HH_MOVE) {
            Property("X").Set(mx);
            Property("Y").Set(myz);
        }
    } else if (plane == PLANE_XZ) {
        if (id == HH_MOVE) {
            Property("X").Set(mx);
            Property("Z").Set(myz - H / 2);
        } else if (id == HH_BOTTOM) {
            double ztop = Z + H;
            double hnew = fabs(myz - ztop);
            Property("Z").Set(ztop - hnew);
            Property("Height").Set(hnew);
        } else if (id == HH_TOP) {
            Property("Height").Set(myz - Z);
        }
    }

    return true;
}

void VCylinderObject::DrawOnPlane(VRenderer2D &dc, VPlaneType plane) {
    double xc = Property("X").GetDouble();
    double diam = Property("Diameter").GetDouble();

    if (plane == PLANE_XY) {
        double yc = Property("Y").GetDouble();
        dc.Circ(xc, yc, diam / 2);
    } else if (plane == PLANE_XZ) {
        double height = Property("Height").GetDouble();
        double zc = Property("Z").GetDouble();
        dc.Rect(xc - diam / 2, zc, diam, height);
    }
}

bool VCylinderObject::IsWithin(double x, double yz, VPlaneType plane) {
    double xc = Property("X").GetDouble();
    double diam = Property("Diameter").GetDouble();

    if (plane == PLANE_XY) {
        double yc = Property("Y").GetDouble();
        return (sqrt((x - xc) * (x - xc) + (yz - yc) * (yz - yc)) < diam / 2);
    } else if (plane == PLANE_XZ) {
        double zc = Property("Z").GetDouble();
        double height = Property("Height").GetDouble();
        return ((x >= xc - diam / 2) && (x <= xc + diam / 2) &&
                (yz >= zc) && (yz <= zc + height));
    }
    return false;
}

/* *******************************************************************************************************
VRoofObject
******************************************************************************************************** */
VRoofObject::VRoofObject() {
    AddProperty("X", new VProperty(0.0, LENGTH));
    AddProperty("Y", new VProperty(0.0, LENGTH));
    AddProperty("Z", new VProperty(15.0, LENGTH));
    AddProperty("Width", new VProperty(17.0, LENGTH));
    AddProperty("Length", new VProperty(30.0, LENGTH));
    AddProperty("Height", new VProperty(5.0, LENGTH));
    AddProperty("Rotation", new VProperty(0.0));
    AddProperty("Pitch Angle 1", new VProperty(45.0));
    AddProperty("Pitch Angle 2", new VProperty(45.0));
    AddProperty("Color", new VProperty(wxColour(102, 51, 0, 155)));
}

wxString VRoofObject::GetTypeName() {
    return "Roof";
}

VObject *VRoofObject::Duplicate() {
    VRoofObject *p = new VRoofObject;
    p->Copy(this);
    return p;
}

void VRoofObject::BuildModel(s3d::scene &sc) {
    double x = Property("X").GetDouble();
    double y = Property("Y").GetDouble();
    double z = Property("Z").GetDouble();
    double width = Property("Width").GetDouble();
    double length = Property("Length").GetDouble();
    double height = Property("Height").GetDouble();
    double rot = Property("Rotation").GetDouble();
    double pitch1 = Property("Pitch Angle 1").GetDouble();
    double pitch2 = Property("Pitch Angle 2").GetDouble();

    wxColour cc = Property("Color").GetColour();
    s3d::rgba scc1(cc.Red(), cc.Green(), cc.Blue(), cc.Alpha());

    sc.reset();
    sc.colors(scc1, scc1);
    sc.roof(GetId(), x, y, z, width, length, height, pitch1, pitch2, rot);
}

void VRoofObject::SetupHandles(VPlaneType plane) {
    double x = Property("X").GetDouble();
    double y = Property("Y").GetDouble();
    double z = Property("Z").GetDouble();
    double w = Property("Width").GetDouble();
    double l = Property("Length").GetDouble();
    double h = Property("Height").GetDouble();
    double r = Property("Rotation").GetDouble();

    double xx[5], yy[5];
    xx[0] = x + w;
    yy[0] = y + l / 2;    //HH_RIGHT
    xx[1] = x + w / 2;
    yy[1] = y + l;      // HH_TOP
    xx[2] = x + 3 * w / 4;
    yy[2] = y;      // HH_ROTATE_XY
    xx[3] = x;
    yy[3] = y + l / 2;    //HH_LEFT
    xx[4] = x + w / 2;
    yy[4] = y;   // HH_BOTTOM
    s3d::rotate2dxz(x, y, xx, yy, r, 5);

    if (plane == PLANE_XY) {
        AddHandle(HH_MOVE, x, y);
        AddHandle(HH_RIGHT, xx[0], yy[0], wxCURSOR_PENCIL);
        AddHandle(HH_TOP, xx[1], yy[1], wxCURSOR_PENCIL);
        AddHandle(HH_ROTATE_XY, xx[2], yy[2], wxCURSOR_BULLSEYE);
        AddHandle(HH_LEFT, xx[3], yy[3], wxCURSOR_PENCIL);
        AddHandle(HH_BOTTOM, xx[4], yy[4], wxCURSOR_PENCIL);

    } else if (plane == PLANE_XZ) {
        AddHandle(HH_MOVE, x, z);
        AddHandle(HH_TOP, x + w / 2, z + h, wxCURSOR_PENCIL);
        AddHandle(HH_BOTTOM, x + w / 2, z, wxCURSOR_PENCIL);
    }

}

bool VRoofObject::OnHandleMoved(VHandle *hh, VPlaneType plane) {
    // extract previous properties
    double bx = Property("X").GetDouble();
    double by = Property("Y").GetDouble();
    double bz = Property("Z").GetDouble();
    double bw = Property("Width").GetDouble();
    double bl = Property("Length").GetDouble();
    double bh = Property("Height").GetDouble();
    double br = Property("Rotation").GetDouble();

    int id = hh->GetId();
    double x = hh->GetX();
    double y = hh->GetYZ();
    double z = hh->GetYZ();


    if (id == HH_MOVE) {
        Property("X").Set(x);
        if (plane == PLANE_XY) Property("Y").Set(y);
        else Property("Z").Set(z);
        return true;
    } else if (id == HH_RIGHT) {
        double xx = bw;
        double yy = 0;
        rot2(xx, yy, br);
        double len = proj(bx, by, bx + xx, by + yy, x, y);
        Property("Width").Set(len);
        return true;
    } else if (id == HH_TOP) {
        // compute new center and height/depth
        if (plane == PLANE_XY) {
            double xx = 0;
            double yy = bh;
            rot2(xx, yy, br);
            double len = proj(bx, by, bx + xx, by + yy, x, y);
            Property("Length").Set(len);
        } else
            Property("Height").Set(z - bz);

        return true;
    } else if (id == HH_LEFT) {
        double xx = bw;
        double yy = 0;
        rot2(xx, yy, br);
        double len = proj(bx + xx, by + yy, bx, by, x, y);
        Property("X").Set(bx - (len - bw) * cos(br * M_PI / 180));
        Property("Y").Set(by - (len - bw) * sin(br * M_PI / 180));
        Property("Width").Set(len);
        return true;
    } else if (id == HH_BOTTOM) {
        if (plane == PLANE_XY) {
            double xx = 0;
            double yy = bl;
            rot2(xx, yy, br);
            double len = proj(bx + xx, by + yy, bx, by, x, y);
            Property("X").Set(bx + (len - bl) * sin(br * M_PI / 180));
            Property("Y").Set(by - (len - bl) * cos(br * M_PI / 180));
            Property("Length").Set(len);
        } else {
            double z0 = bz + bh;
            double h = fabs(z - z0);
            Property("Z").Set(z0 - h);
            Property("Height").Set(h);
        }

        return true;
    } else if (id == HH_ROTATE_XY) {
        double r = (180.0 / M_PI) * atan2(hh->GetYZ() - by, hh->GetX() - bx);
        Property("Rotation").Set(r /*deg*/);
    }

    return false;
}

void VRoofObject::DrawOnPlane(VRenderer2D &dc, VPlaneType plane) {
    double xc = Property("X").GetDouble();
    double yc = Property("Y").GetDouble();
    double width = Property("Width").GetDouble();
    double length = Property("Length").GetDouble();
    double rot = Property("Rotation").GetDouble();

    double xr[4], yr[4];

    if (plane == PLANE_XY) {
        s3d::get_rotated_box_points(xc, yc, width, length, rot, xr, yr);
        dc.Poly(xr, yr, 4);
    } else if (plane == PLANE_XZ) {
        double xd[4], zd[4];
        GetXZPoints(xd, zd);
        dc.Poly(xd, zd, 4);
    }
}

bool VRoofObject::IsWithin(double x, double yz, VPlaneType plane) {
    double xc = Property("X").GetDouble();
    double yc = Property("Y").GetDouble();
    double width = Property("Width").GetDouble();
    double length = Property("Length").GetDouble();
    double rot = Property("Rotation").GetDouble();

    if (plane == PLANE_XY) {
        double xr[4], yr[4];
        s3d::get_rotated_box_points(xc, yc, width, length, rot, xr, yr);
        return s3d::inpoly(xr, yr, 4, x, yz);
    } else if (plane == PLANE_XZ) {
        double xd[4], zd[4];
        GetXZPoints(xd, zd);
        return s3d::inpoly(xd, zd, 4, x, yz);

    }
    return false;
}

void VRoofObject::GetXZPoints(double xd[4], double zd[4]) {
    double x = Property("X").GetDouble();
    double y = Property("Y").GetDouble();
    double z = Property("Z").GetDouble();
    double width = Property("Width").GetDouble();
    double length = Property("Length").GetDouble();
    double rot = Property("Rotation").GetDouble();
    double height = Property("Height").GetDouble();
    double pitch1 = Property("Pitch Angle 1").GetDouble();
    double pitch2 = Property("Pitch Angle 2").GetDouble();

    double xr[4], yr[4];
    s3d::get_rotated_box_points(x, y, width, length, 0.0, xr, yr);

    // compute xy pitch points
    double xp[2], yp[2];
    xp[0] = 0.5 * (xr[0] + xr[2]);
    xp[1] = xp[0];
    yp[0] = yr[0] + height / (tan(pitch1 * DTOR));
    yp[1] = yr[1] - height / (tan(pitch2 * DTOR));
    s3d::rotate2dxz(x, y, xp, yp, rot, 2);

    // draw points
    s3d::rotate2dxz(x, y, xr, yr, rot, 4);

    if (sin(rot * DTOR) >= 0. && cos(rot * DTOR) >= 0.0) {
        xd[0] = xr[0];
        zd[0] = z;
        xd[1] = xp[0];
        zd[1] = z + height;
        xd[2] = xp[1];
        zd[2] = z + height;
        xd[3] = xr[2];
        zd[3] = z;
    } else if (sin(rot * DTOR) >= 0.0 && cos(rot * DTOR) < 0.) {
        xd[0] = xr[3];
        zd[0] = z;
        xd[1] = xp[0];
        zd[1] = z + height;
        xd[2] = xp[1];
        zd[2] = z + height;
        xd[3] = xr[1];
        zd[3] = z;
    } else if (sin(rot * DTOR) < 0.0 && cos(rot * DTOR) < 0.) {
        xd[0] = xr[2];
        zd[0] = z;
        xd[1] = xp[1];
        zd[1] = z + height;
        xd[2] = xp[0];
        zd[2] = z + height;
        xd[3] = xr[0];
        zd[3] = z;
    } else {
        xd[0] = xr[1];
        zd[0] = z;
        xd[1] = xp[1];
        zd[1] = z + height;
        xd[2] = xp[0];
        zd[2] = z + height;
        xd[3] = xr[3];
        zd[3] = z;
    }
}
