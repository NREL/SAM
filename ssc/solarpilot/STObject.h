#ifndef _ST_OBJECT_
#define _ST_OBJECT_ 1
#include "definitions.h"

#ifdef SP_USE_SOLTRACE

#include <vector>
#include <string>
#include <exception>

#include "stapi.h"
#include "mtrand.h"
//#include "hpvm.h"
#include "SolarField.h"

class ST_OpticalProperties
{
public:
	ST_OpticalProperties();
	ST_OpticalProperties &operator=(const ST_OpticalProperties &rhs);
	
	char DistributionType;
	int OpticSurfNumber;
	int ApertureStopOrGratingType;
	int DiffractionOrder;
	double Reflectivity;
	double Transmissivity;
	double RMSSlopeError;
	double RMSSpecError;

	//double RefractiveIndex[4];
	double Grating[4];	
	void Write(FILE *fdat);
};

class ST_OpticalPropertySet
{
public:
	std::string Name;
	ST_OpticalProperties Front;
	ST_OpticalProperties Back;	
	void Write(FILE *fdat);
};

struct ST_Element
{
	ST_Element();
	
	bool Enabled;
	void Write(FILE *fdat);
	/////////// ORIENTATION PARAMETERS ///////////////
	double Origin[3];
	double AimPoint[3];
	double ZRot;
	double Euler[3]; // calculated
	double RRefToLoc[3][3]; // calculated
	double RLocToRef[3][3]; // calculated
	void UpdateRotationMatrix();
	
	/////////// APERTURE PARAMETERS ///////////////
	char ShapeIndex;
	double Ap_A, Ap_B, Ap_C, Ap_D, Ap_E, Ap_F, Ap_G, Ap_H;	//APERTURE PARAMETERS
	
	double ApertureArea; // calculated
	double ZAperture; // calculated 
	
	/////////// SURFACE PARAMETERS ///////////////
	char SurfaceIndex;
	// SurfaceType; // calculated
	double Su_A, Su_B, Su_C, Su_D, Su_E, Su_F, Su_G, Su_H;
	//std::string SurfaceFile;
	
	/*double Kappa;
	double Alpha[5];
	double VertexCurvX;
	double VertexCurvY;
	double AnnularRadius;
	double CrossSectionRadius;
	double ConeHalfAngle;
	double CurvOfRev;
		
	int FitOrder;
	*/

	// Zernike (*.mon) monomial coeffs
	// (also used for VSHOT Zernike fits)
	//HPM2D BCoefficients;

	// Rotationally symmetric polynomial coeffs
	//std::vector< double > PolyCoeffs;
	
	// Rotationally symmetric cubic spline
	//std::vector< double > CubicSplineXData;
	//std::vector< double > CubicSplineYData; 
	//std::vector< double > CubicSplineY2Data;   
	//double CubicSplineDYDXbc1;
	//double CubicSplineDYDXbcN;
	
	// VSHOT file data
	//HPM2D VSHOTData;
	//double VSHOTRMSSlope;
	//double VSHOTRMSScale;
	//double VSHOTRadius;
	//double VSHOTFocLen;
	//double VSHOTTarDis;
	
	// Finite ST_Element data coeffs
	//HPM2D FEData;	
	
	/////////// OPTICAL PARAMETERS ///////////////
	int InteractionType;
	ST_OpticalPropertySet *Optics;
	std::string OpticName;

	std::string Comment;	
};

struct ST_Sun
{
	ST_Sun();
	void Reset();
	
	char ShapeIndex;	
	double Sigma;
	bool PointSource;

	std::vector<double> SunShapeAngle;
	std::vector<double> SunShapeIntensity;
	
	double Origin[3];
	void Write(FILE *fdat);
};

class ST_RayData
{
public:
	ST_RayData();
	~ST_RayData();

	struct ray_t
	{
		double pos[3];
		double cos[3];
		int element;
		int stage;
		unsigned int raynum;
	};

	ray_t *Append( double pos[3],
					 double cos[3],
					 int element,
					 int stage,
					 unsigned int raynum );

	bool Overwrite( unsigned int idx,
					double pos[3],
					double cos[3],
					int element,
					int stage,
					unsigned int raynum);

	bool Query( unsigned int idx,
					double pos[3],
					double cos[3],
					int *element,
					int *stage,
					unsigned int *raynum);

	void Merge( ST_RayData &dest );

	void Clear();

	void Print();

	st_uint_t Count();

	ray_t *Index(st_uint_t i, bool write_access);

private:
	static const unsigned int block_size = 16384;


	struct block_t
	{
		ray_t data[block_size];
		st_uint_t count;
	};

	std::vector<block_t*> m_blockList;
	st_uint_t m_dataCount;
	st_uint_t m_dataCapacity;
};


class ST_IntersectionData
{
public:
    double *hitx;
	double *hity;
	double *hitz;
	double *cosx;
	double *cosy;
	double *cosz;
	int *emap;	//corresponding element number
	int *smap;	//corresponding stage number
	int *rnum;	//ray numbers
    int nint;   //number of intersections
    int nsunrays;
    double q_ray;   //power per ray
    double bounds[5]; //land bounds

    ST_IntersectionData();
    ~ST_IntersectionData();
    void AllocateArrays(int size);
};


struct ST_Stage
{
	ST_Stage();
	~ST_Stage();
		
	bool MultiHitsPerRay;
	bool Virtual;
	bool TraceThrough;
	
	double Origin[3];
	double AimPoint[3];
	double ZRot;

	std::vector<ST_Element*> ElementList;
	
	// calculated
	double Euler[3];
	double RRefToLoc[3][3];
	double RLocToRef[3][3];
	string Name;
	//ST_RayData ST_RayData;
	void Write(FILE *fdat);
};

struct ST_System
{
	ST_System();
	~ST_System();

	void ClearAll();
	
	ST_Sun Sun;
	std::vector<ST_OpticalPropertySet*> OpticsList;
	std::vector<ST_Stage*> StageList;


	// system simulation context data
	int sim_raycount;
	int sim_raymax;
	bool sim_errors_sunshape;
	bool sim_errors_optical;

	// simulation outputs
	ST_RayData AllRayData;
	st_uint_t SunRayCount;
    ST_IntersectionData IntData;

	//method for loading the solar field geometry into the ST_System object
	bool CreateSTSystem(SolarField &SF, Hvector &helios, Vect &sunvect);

	static void LoadIntoContext(ST_System *System, st_context_t spcxt);
	void Write(FILE *fdat);

};

#endif

#endif