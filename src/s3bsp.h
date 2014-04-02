#ifndef __s3bsp_h
#define __s3bsp_h

#include <fstream>
#include <unordered_map>

#include "s3engine.h"

typedef unsigned short ushort;
typedef unsigned long ulong;

class CPoint
{
public:
	double x, y, z;

	CPoint();
	CPoint( double _x, double _y, double _z);

	void Read( s3d::point3d& point );
	void Write( s3d::point3d& point ) const;

	void Read( std::ifstream& Input );
	void Write( std::ofstream& Output ) const;

	double Magnitude( void ) const;
	void Normalize( void );
	double DotProduct( const CPoint& Pnt ) const;
	CPoint CrossProduct( const CPoint& Pnt ) const;
	short operator==( const CPoint& Pnt ) const;
	CPoint operator-( const CPoint& Pnt ) const;
};



class BSPNode
{
#ifdef _DEBUG
public:
#endif

	size_t Index;
	BSPNode *FrontNode, *BackNode;

	std::vector<CPoint> PntList;

	CPoint Center;
	CPoint Normal;
	double D;

	// additional polygon information
	bool m_as_line;
	s3d::rgba m_border;
	s3d::rgba m_fill;
	int m_id;
	int m_thick;
	int m_type;

	bool m_rendered;


	ulong _SplitPoly( BSPNode *Plane, std::vector<CPoint> &SplitPnts, bool savepoints=true );
	void _ComputeCenter( void );
	void _ComputeNormal( void );
	void _ComputeD( void );

public:
	BSPNode();
	~BSPNode();

	bool GetRendered() { return m_rendered;}

	void ReadPoly(const s3d::polygon3d& poly );
	s3d::polygon3d* WritePoly() const;

	void ReadPoly( std::ifstream& Input );

	CPoint GetCenter( void )				{ return Center; }
	CPoint GetNormal( void )				{ return Normal; }

	bool Intersects( BSPNode *Plane );
	BSPNode *Split( BSPNode *Plane );

	BSPNode *GetFront( void )			{ return FrontNode; }
	BSPNode *GetBack( void )			{ return BackNode; }

	void SetFront( BSPNode *Node )		{ FrontNode = Node; }
	void SetBack( BSPNode *Node)		{ BackNode = Node; }

	void Traverse( const CPoint& CameraLoc );
	void Traverse( const CPoint& CameraLoc, std::vector<s3d::polygon3d*>& polys );

	double GetMinZ();
};



class BSPTree
{
private:
	std::unordered_map<int, float> m_id_minz;
	std::vector<BSPNode*> m_nodes;
	std::vector<BSPNode*> m_listnodes; // for deletion
	BSPNode *m_root;

	BSPNode *_FindRoot( std::vector<BSPNode*>& List );
	BSPNode *_BuildBSPTree( std::vector<BSPNode*>& List );

public:
	BSPTree() {};
	BSPTree( std::vector<s3d::polygon3d*>& polys, double x_viewport, double y_viewport, double z_viewport);
	~BSPTree();
	void Traverse( CPoint& CameraLoc, std::vector<s3d::polygon3d*>& polys );

	void ReadPolyList(const std::vector<s3d::polygon3d*>& polys );

	void ReadPolyList( std::ifstream& Input );
	void ReadTree( std::ifstream& Input );
	void WriteTree( std::ofstream& Output );

	void BuildTree( void );
	//void Traverse( const CPoint& CameraLoc );
};

#endif