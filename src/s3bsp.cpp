#include <iostream>
#include <sstream>
#include <assert.h>
#include <algorithm>

#include "s3bsp.h"



#ifdef _DEBUG
//#define _CRTDBG_MAP_ALLOC
#define _INC_CRTDBG
#include <crtdbg.h>

// File for windows debugging
// Usage:  DBOUT(" x = " << x)
#include <windows.h>

#define DBOUT( s )            \
{                             \
   std::wostringstream os_;    \
   os_ << s;                   \
   OutputDebugStringW( os_.str().c_str() );  \
}


// debugging for memory leaks
#ifdef __WXMSW__
    #include <wx/msw/msvcrt.h>      // redefines the new() operator 
#endif

#if !defined(_INC_CRTDBG) || !defined(_CRTDBG_MAP_ALLOC)
//    #error Debug CRT functions have not been included!
#endif

static void debug_out( char* prefix, CPoint point)
{
	DBOUT( prefix << ": (" << point.x << ","  << point.y << ","  << point.z << ")\n");
}


// debugging
static void debug_out( char* prefix, BSPNode* node)
{
	DBOUT( prefix << ": m_id=" << node->m_id << " , m_type=" << node->m_type << ", m_fill.r=" << node->m_fill.r <<  ", m_fill.g=" << node->m_fill.g << ", m_border=" << node->m_border.r << ", m_thick=" << node->m_thick << ", m_as_line=" << node->m_as_line << ", point.size()=" << node->PntList.size() << ", D=" << node->D << "\n");
		for( size_t i=0; i< node->PntList.size(); i++ )
			debug_out("i=",node->PntList[i]);
	debug_out( "\tCenter", node->Center);
	debug_out( "\tNormal", node->Normal);
}


#endif



#define MAX_DELTA		0.03491
//#define MAX_DELTA		0.00000001



/*
	Implementation of top level BSPTree class
*/
//--------------- Private Functions


BSPNode *BSPTree::_FindRoot( std::vector<BSPNode*>& List )
// Returns node which splits the least other nodes
{
//	short BestCount = 0x7fff;
//	int BestCount = -1;


	int BestCount = List.size();
	BSPNode *BestRoot = NULL;

	// testing different roots
//	if ( List.size() > 0) BestRoot = List[0 ];
//	if ( List.size() > 0) BestRoot = List[List.size() / 2 ];
//	if ( List.size() > 0) BestRoot = List[List.size() - 1 ];

//	return BestRoot;
	//



	std::vector<BSPNode*>::iterator Iter = List.begin();
	BSPNode *TestNode = *Iter;
	
	while( Iter != List.end() )
	{
		TestNode = *Iter;
		std::vector<BSPNode*>::iterator Iter2 = List.begin();
		BSPNode *CheckNode = *Iter2;
		int Count = 0;
		
		while( Iter2 != List.end() )
		{
			CheckNode = *Iter2;
			if( CheckNode != TestNode )
				if( CheckNode->Intersects( TestNode ) )
					Count++;
			Iter2++;					
		}
		
		if( Count < BestCount )
		{
			if( !Count )
				return TestNode;

			BestCount = Count;
			BestRoot = TestNode;
		}

		Iter++;
	}
	
	return BestRoot;
}				



BSPNode *BSPTree::_BuildBSPTree( std::vector<BSPNode*>& List )
{
	CPoint Delta;
	std::vector<BSPNode*> Front, Back;

	BSPNode *Root = _FindRoot( List );

	if (Root==NULL) 
		return Root;
	// remove root node from list
	List.erase(std::remove(List.begin(), List.end(), Root), List.end());

	std::vector<BSPNode*>::iterator Iter = List.begin();
	BSPNode *TestNode;
	
	while( List.size() > 0 )
	{
		TestNode = *Iter;			
		if( TestNode->Intersects( Root ) )
		{
			BSPNode *NewNode = TestNode->Split( Root );

			assert(NewNode);

			m_listnodes.push_back( NewNode );

			
			Delta = TestNode->GetCenter() - Root->GetCenter();

			if( Delta.DotProduct( Root->GetNormal() ) > 0.0 )
			{
				Front.push_back( TestNode );
				Back.push_back( NewNode );
			}
			else
			{
				Back.push_back( TestNode );
				Front.push_back( NewNode );
			}
		}
		else
		{
			Delta = TestNode->GetCenter() - Root->GetCenter();
/*
			if( Delta.DotProduct( Root->GetNormal() ) > 0.0 )
				Front.push_back( TestNode );
			else
				Back.push_back( TestNode );
*/
			if( Delta.DotProduct( Root->GetNormal() ) > MAX_DELTA )
				Front.push_back( TestNode );
			else if( Delta.DotProduct( Root->GetNormal() ) < MAX_DELTA )
				Back.push_back( TestNode );
			else
			{
				if ( Delta.z < 0 )
					Front.push_back( TestNode );
				else
					Back.push_back( TestNode );
			}
		}
		// remove test node from list
		List.erase(std::remove(List.begin(), List.end(), TestNode), List.end());
		Iter = List.begin();
	}
	
	Root->SetFront( (Front.size()==0) ? NULL : _BuildBSPTree( Front ) );
	Root->SetBack( (Back.size()==0) ? NULL : _BuildBSPTree( Back ) );
	
	return Root;
}


//--------------- Public Functions

BSPTree::BSPTree( std::vector<s3d::polygon3d*>& polys, double x_viewport, double y_viewport, double z_viewport)
{
	ReadPolyList( polys );
	polys.clear();

	BuildTree();

	CPoint viewport;
	viewport.x = x_viewport;
	viewport.y = y_viewport;
	viewport.z = z_viewport;

	Traverse( viewport, polys);
}

BSPTree::~BSPTree()
{
	for (std::vector<BSPNode*>::iterator it = m_listnodes.begin(); it != m_listnodes.end();++it )
		delete *it;
	m_listnodes.clear();
	m_nodes.clear();
}

void BSPTree::BuildTree()
{
	m_root = _BuildBSPTree( m_nodes );
	
//#ifdef _DEBUG
//	DBOUT("Root: m_id=" << m_root->m_id << " , m_type=" << m_root->m_type << ", m_fill.r=" << m_root->m_fill.r <<  ", m_fill.g=" << m_root->m_fill.g << ", m_border=" << m_root->m_border.r << ", m_thick=" << m_root->m_thick << ", m_as_line=" << m_root->m_as_line << ", point.size()=" << m_root->PntList.size() << "\n");
//		for( size_t i=0; i< m_root->PntList.size(); i++ )
//			DBOUT("i=" << i << " , x=" << m_root->PntList[i].x << ", y=" << m_root->PntList[i].y <<  ", z=" <<  m_root->PntList[i].z <<  "\n");
	//for (size_t j=0; j< m_listnodes.size(); j++)
	//{
	//DBOUT("BuildTree m_listnode[" << j << "]: m_id=" << m_listnodes[j]->m_id << " , m_type=" <<  m_listnodes[j]->m_type << ", m_fill.r=" <<  m_listnodes[j]->m_fill.r <<  ", m_fill.g=" <<  m_listnodes[j]->m_fill.g << ", m_border=" <<  m_listnodes[j]->m_border.r << ", m_thick=" <<  m_listnodes[j]->m_thick << ", m_as_line=" <<  m_listnodes[j]->m_as_line << ", point.size()=" <<  m_listnodes[j]->PntList.size() << "\n");
	//	for( size_t i=0; i<  m_listnodes[j]->PntList.size(); i++ )
	//		DBOUT("i=" << i << " , x=" <<  m_listnodes[j]->PntList[i].x << ", y=" <<  m_listnodes[j]->PntList[i].y <<  ", z=" <<   m_listnodes[j]->PntList[i].z <<  "\n");
	//}
//#endif
}

void BSPTree::ReadPolyList( const std::vector<s3d::polygon3d*>& polys )
{
	for (size_t i=0; i<polys.size(); i++)
	{
		if (polys[i]->points.size() < 3)
			continue;
		BSPNode *new_node = new BSPNode;
		s3d::polygon3d poly = *polys[i];
		new_node->ReadPoly( poly );

		m_nodes.push_back( new_node );
		m_listnodes.push_back( new_node );
	}

//#ifdef _DEBUG
	//for (size_t j=0; j< m_listnodes.size(); j++)
	//{
	//DBOUT("ReadPolyList m_listnode[" << j << "]: m_id=" << m_listnodes[j]->m_id << " , m_type=" <<  m_listnodes[j]->m_type << ", m_fill.r=" <<  m_listnodes[j]->m_fill.r <<  ", m_fill.g=" <<  m_listnodes[j]->m_fill.g << ", m_border=" <<  m_listnodes[j]->m_border.r << ", m_thick=" <<  m_listnodes[j]->m_thick << ", m_as_line=" <<  m_listnodes[j]->m_as_line << ", point.size()=" <<  m_listnodes[j]->PntList.size() << "\n");
	//	for( size_t i=0; i<  m_listnodes[j]->PntList.size(); i++ )
	//		DBOUT("i=" << i << " , x=" <<  m_listnodes[j]->PntList[i].x << ", y=" <<  m_listnodes[j]->PntList[i].y <<  ", z=" <<   m_listnodes[j]->PntList[i].z <<  "\n");
	//}
//#endif
}



/*
void BSPTree::Traverse( const CPoint& CameraLoc )
{
	if( m_root )
		m_root->Traverse( CameraLoc );
}
*/

void BSPTree::Traverse( CPoint& CameraLoc, std::vector<s3d::polygon3d*>& polys )
{
	if( m_root )
		m_root->Traverse( CameraLoc, polys );

#ifdef _DEBUG
	int count =0;
	for (std::vector<BSPNode*>::iterator it = m_listnodes.begin(); it != m_listnodes.end();++it )
	{
		if (!(*it)->GetRendered()) count++;
	}
	if (count > 0)
		DBOUT("Traverse: " << count << " nodes not added from BSP tree \n");
#endif
}


/*
	Implementation of BSPNode class
*/

//-------------- Private Functions
/*
//ulong BSPNode::_SplitPoly( BSPNode *Plane, CPoint *SplitPnts )
ulong BSPNode::_SplitPoly( BSPNode *Plane, std::vector<CPoint> &SplitPnts, bool savepoints )
// This is limited to convex polygons with no more than 32 (unsigned long 32 bits) sides
// unsigned integer is used for mask operations in Split function
// splitPoly checks each side of the BSPNode polygon against the plane
// a line-plan split occurs and the "Sides" unsigned long contains a 1 whereever splits occur ( along polygon side segment ).
// 
{
	SplitPnts.clear();
	SplitPnts.reserve(PntList.size());
	for ( size_t i = 0; i<PntList.size(); i++)
		SplitPnts.push_back( CPoint( 0,0,0));

	ulong Sides = 0;

	int count=0;

//#ifdef _DEBUG
	//if (PntList.size() > 32)
	//	DBOUT("_SplitPoly: PntCnt = " << PntList.size() << "\n");

//#endif
	if (PntList.size() > 32) return 0;

	bool LastSideParallel = false;

	// if polygon normal = plane normal then skip
	// this means that the polygon is parallel or in same plane and the "Plane" input
	if( !( Normal == Plane->Normal ) )
	{
		CPoint EdgeDelta;
		double numer, denom, t;


		// check each edge of polygon against plane

		for( ushort vertex=0; vertex<PntList.size(); vertex++ )
		{
			ushort prevVertex = vertex ? vertex - 1 : (ushort)(PntList.size() - 1);

			EdgeDelta = PntList[ vertex ] - PntList[ prevVertex ];
			denom = EdgeDelta.DotProduct( Plane->Normal );

#ifdef _DEBUG
//			DBOUT("_SplitPoly: denom=" << denom << "\n");
//			debug_out("_SplitPoly: Plane:", Plane);
#endif

// Dot product of line segment and plane non-zero so not parallel 
// lines segment and plane potentially intersects at one point
//			if( denom )
			if( fabs(denom) > MAX_DELTA )
			{
				numer = PntList[ prevVertex ].DotProduct( Plane->Normal ) +	Plane->D;
				t = - numer / denom;

#ifdef _DEBUG
			DBOUT("_SplitPoly: numer=" << numer << ", denom=" << denom << ", t=" << t << "\n");
#endif
		
//				if( !( LastSideParallel && (t == 0) ) )
				if( !( LastSideParallel && (fabs(t) <= MAX_DELTA) ) )
				{
//					if( t >= 0.0 && t < 0.999999 )		
					if( (t > MAX_DELTA) && (t < (1.0 - MAX_DELTA) ))
//					if( (t > -MAX_DELTA) && (t < (1.0 + MAX_DELTA) ))
					{
						Sides |= 1 << vertex;
						count++;
						if (savepoints)
						{
//							SplitPnts.push_back( CPoint( 
//								PntList[ prevVertex ].x + t * EdgeDelta.x,
//								PntList[ prevVertex ].y + t * EdgeDelta.y,
//								PntList[ prevVertex ].z + t * EdgeDelta.z ));
							SplitPnts[vertex].x=PntList[ prevVertex ].x + t * EdgeDelta.x;
							SplitPnts[vertex].y=PntList[ prevVertex ].y + t * EdgeDelta.y;
							SplitPnts[vertex].z=PntList[ prevVertex ].z + t * EdgeDelta.z;
						}
					}
				}
			}
//			LastSideParallel = ( denom == 0 );
			LastSideParallel = ( fabs(denom) <= MAX_DELTA );
		}
	}
	
#ifdef _DEBUG
	if ((count != 0) && (count !=2) )
	//if (count > 2 )
		DBOUT("_SplitPoly: count=" << count << "\n");
#endif
	if ((count != 0) && (count !=2)) 
	{
		Sides=0;
		SplitPnts.clear();
	}

	return Sides;
}
*/

ulong BSPNode::_SplitPoly( BSPNode *Plane, std::vector<CPoint> &SplitPnts, bool savepoints )
// This is limited to convex polygons with no more than 32 (unsigned long 32 bits) sides
// unsigned integer is used for mask operations in Split function
// splitPoly checks each side of the BSPNode polygon against the plane
// a line-plan split occurs and the "Sides" unsigned long contains a 1 whereever splits occur ( along polygon side segment ).
// A convex polygon can only be split in two points
{
	SplitPnts.clear();
	SplitPnts.reserve(PntList.size());
	for ( size_t i = 0; i<PntList.size(); i++)
		SplitPnts.push_back( CPoint( 0,0,0));

	ulong Sides = 0;

	int count=0;

//#ifdef _DEBUG
	//if (PntList.size() > 32)
	//	DBOUT("_SplitPoly: PntCnt = " << PntList.size() << "\n");

//#endif
	if (PntList.size() > 32) return 0;

	bool LastSideParallel = false;

	// if polygon normal = plane normal then skip
	// this means that the polygon is parallel or in same plane as the "Plane" input
	if( !( Normal == Plane->Normal ) )
	{
		CPoint u,w;
		double numer, denom, si;


		// check each edge of polygon against plane

		for( ushort vertex=0; vertex<PntList.size(); vertex++ )
		{
			ushort prevVertex = vertex ? vertex - 1 : (ushort)(PntList.size() - 1);

			u = PntList[ vertex ] - PntList[ prevVertex ];
			denom = u.DotProduct( Plane->Normal );

#ifdef _DEBUG
//			DBOUT("_SplitPoly: denom=" << denom << "\n");
//			debug_out("_SplitPoly: Plane:", Plane);
#endif

// Dot product of line segment and plane non-zero so not parallel 
// lines segment and plane potentially intersects at one point
//			if( denom != 0)
			if( fabs(denom) > MAX_DELTA )
			{
//				numer = PntList[ prevVertex ].DotProduct( Plane->Normal ) +	Plane->D;

				w = PntList[prevVertex] - Plane->Center;
				numer = w.DotProduct( Plane->Normal );
				si = - numer / denom;

#ifdef _DEBUG
//			DBOUT("_SplitPoly: numer=" << numer << ", denom=" << denom << ", t=" << si << "\n");
#endif
		
//				if( !( LastSideParallel && (t == 0) ) )
//				if( !( LastSideParallel && (si == 0) ) )
				if( !( LastSideParallel && (si < MAX_DELTA) ) )
				{

					


//					if( t >= 0.0 && t < 0.999999 )		
					if( (si >= 0) && (si < 1.0))
//					if( (si > 0) && (si < 1.0))
//					if( (si >-MAX_DELTA) && (si < (1.0-MAX_DELTA)))
//					if( (si >-MAX_DELTA) && (si < (1.0-MAX_DELTA)))
////					if( (si > MAX_DELTA) && (si < (1.0 - MAX_DELTA) ))
//					if( (si > -MAX_DELTA) && (si < (1.0 + MAX_DELTA) ))
					{
						Sides |= 1 << vertex;
						count++;
						if (savepoints)
						{
//							SplitPnts.push_back( CPoint( 
//								PntList[ prevVertex ].x + t * EdgeDelta.x,
//								PntList[ prevVertex ].y + t * EdgeDelta.y,
//								PntList[ prevVertex ].z + t * EdgeDelta.z ));

							//if ( si < MAX_DELTA )
							//{
							//	SplitPnts[vertex].x=PntList[ prevVertex ].x;
							//	SplitPnts[vertex].y=PntList[ prevVertex ].y;
							//	SplitPnts[vertex].z=PntList[ prevVertex ].z;
							//}
							//else if ( si > (1.0 - MAX_DELTA))
							//{
							//	SplitPnts[vertex].x=PntList[ vertex ].x;
							//	SplitPnts[vertex].y=PntList[ vertex ].y;
							//	SplitPnts[vertex].z=PntList[ vertex ].z;
							//}
							//else
							{
							SplitPnts[vertex].x=PntList[ prevVertex ].x + si * u.x;
							SplitPnts[vertex].y=PntList[ prevVertex ].y + si * u.y;
							SplitPnts[vertex].z=PntList[ prevVertex ].z + si * u.z;
							}
						}
					}
				}
			}
//			LastSideParallel = ( denom == 0 );
			LastSideParallel = ( fabs(denom) <= MAX_DELTA );
		}
	}
	
#ifdef _DEBUG
	if ((count != 0) && (count !=2)  )
	{
	//if (count > 2 )
		DBOUT("_SplitPoly: count=" << count << std::hex << ", Sides=" <<  std::hex << Sides << ", PntList.size()=" << std::dec << PntList.size() << "\n");
		debug_out( "Plane=", Plane);
		debug_out( "This=", this);
		for(size_t i=0;i<SplitPnts.size();i++) debug_out("SplitPnts=", SplitPnts[i]);
	}
#endif
	if ((count != 0) && (count !=2)) 
//	if ((count < 2)) 
	{
		Sides=0;
		SplitPnts.clear();
	}

	return Sides;
}


void BSPNode::_ComputeCenter( void )
{
	Center.x = Center.y = Center.z = 0.0;

	for( size_t i=0; i<PntList.size(); i++ )
	{
		Center.x += PntList[ i ].x;
		Center.y += PntList[ i ].y;
		Center.z += PntList[ i ].z;
	}

	Center.x /= PntList.size();
	Center.y /= PntList.size();
	Center.z /= PntList.size();
}


void BSPNode::_ComputeNormal( void )
{
	CPoint a, b;

	assert( PntList.size() >= 3 );

	a = PntList[ 0 ] - PntList[ 1 ];
	b = PntList[ 2 ] - PntList[ 1 ];

	Normal = a.CrossProduct( b );
	Normal.Normalize();
}


void BSPNode::_ComputeD( void )
{
	D = -Normal.DotProduct( Center );
}

//-------------- Public Functions

BSPNode::BSPNode() :
	FrontNode( NULL ),
	BackNode( NULL )
{
	m_rendered = false;
	PntList.clear();
}


BSPNode::~BSPNode()
{
	PntList.clear();
	FrontNode = NULL;
	BackNode = NULL;
};


void BSPNode::ReadPoly(const s3d::polygon3d& poly )
{
	assert( poly.points.size() >= 3 );
	PntList.clear();

	m_as_line = poly.as_line;
	m_border = poly.border;
	m_fill = poly.fill;
	m_id = poly.id;
	m_thick = poly.thick;
	m_type = poly.type;


	for( size_t i=0; i<poly.points.size(); i++ )
		PntList.push_back( CPoint( poly.points[i]._x, poly.points[i]._y, poly.points[i]._z));

	_ComputeCenter();
	_ComputeNormal();
	_ComputeD();


}

s3d::polygon3d* BSPNode::WritePoly() const
{
	std::vector<s3d::point3d> points;

	for( size_t i=0; i<PntList.size(); i++ )
	{
		s3d::point3d point( (float)PntList[i].x, (float)PntList[i].y, (float)PntList[i].z);
		point._x = point.x;
		point._y = point.y;
		point._z = point.z;
		points.push_back( point );
	}

	if ( points.size() > 0 )
	{
//#ifdef __DEBUG__
		//DBOUT("m_id=" << m_id << " , m_type=" << m_type << ", m_fill.r=" << m_fill.r <<  ", m_fill.g=" << m_fill.g << ", m_border=" << m_border.r << ", m_thick=" << m_thick << ", m_as_line=" << m_as_line << ", point.size()=" << points.size() << "\n");
		//for( size_t i=0; i< points.size(); i++ )
		//	DBOUT("i=" << i << " , x=" << points[i]._x << ", y=" << points[i]._y <<  ", z=" <<  points[i]._z <<  "\n");
//#endif
		return new s3d::polygon3d( m_id, m_type, m_fill, m_border, m_thick, m_as_line, points);
	}
	else
		return NULL;
}



bool BSPNode::Intersects( BSPNode *Plane )
{
	std::vector<CPoint> points;

	return ( _SplitPoly( Plane, points, false ) != 0 );
//	return ( _SplitPoly( Plane, points ) != 0 );
}


BSPNode *BSPNode::Split( BSPNode *Plane )
{
	BSPNode *NewNode=NULL;
	ulong Splits;
//	size_t split_ndx=0;
 	std::vector<CPoint> SplitPnts;
	Splits = _SplitPoly( Plane, SplitPnts );

//#ifdef __DEBUG__
		//DBOUT("Split: m_id=" << m_id << " , m_type=" << m_type << ", m_fill.r=" << m_fill.r <<  ", m_fill.g=" << m_fill.g << ", m_border=" << m_border.r << ", m_thick=" << m_thick << ", m_as_line=" << m_as_line << ", point.size()=" << PntList.size() << ", Normal=" << Normal.Magnitude() << ", Center=" << Center.Magnitude() << ", D=" << D << "\n");
		//for( size_t i=0; i< PntList.size(); i++ )
		//	DBOUT("Split: PntList[" << i << "], x=" << PntList[i].x << ", y=" << PntList[i].y <<  ", z=" <<  PntList[i].z <<  "\n");
		//DBOUT("Split: Splits=" << Splits << "\n");
		//for( size_t i=0; i< SplitPnts.size(); i++ )
		//	DBOUT("Split: SplitPnts[" << i << "], x=" << SplitPnts[i].x << ", y=" << SplitPnts[i].y <<  ", z=" <<  SplitPnts[i].z <<  "\n");
//#endif



	if( Splits )
	{
		ushort Destination = 0;

		std::vector<CPoint> NewPoly1;
		std::vector<CPoint> NewPoly2;

		for( ushort i=0; i<PntList.size(); i++ )
		{
			// Handle split points
			if( Splits & ( 1 << i ) )
			{
				//if ( split_ndx < SplitPnts.size())
				//{
				//	NewPoly1.push_back(SplitPnts[ split_ndx ]);
				//	NewPoly2.push_back(SplitPnts[ split_ndx ]);
				//	split_ndx++;
				//}
//					if (std::find( NewPoly1.begin(), NewPoly1.end(), SplitPnts[i]) == NewPoly1.end())
						NewPoly1.push_back(SplitPnts[ i ]);
//				if (std::find( NewPoly2.begin(), NewPoly2.end(), SplitPnts[i]) == NewPoly2.end())
						NewPoly2.push_back(SplitPnts[ i ]);
			
				Destination ^= 1;
			}

			if( Destination )
			{
//				if (std::find( NewPoly1.begin(), NewPoly1.end(), PntList[i]) == NewPoly1.end())
					NewPoly1.push_back( PntList[ i ]);
			}
			else
			{
//				if (std::find( NewPoly2.begin(), NewPoly2.end(), PntList[i]) == NewPoly2.end())
					NewPoly2.push_back(PntList[ i ]);
			}
		}

//#ifdef _DEBUG
//		if ( (NewPoly1.size()+NewPoly2.size()) != PntList.size())
			//DBOUT("(NewPoly1.size() " << NewPoly1.size() << " + NewPoly2.size() " << NewPoly2.size() << " ) != PntList.size()=" << PntList.size() << "\n");
/*		// check for duplicate points
			DBOUT("NewPoly1\n");
		for (size_t i=0;i<NewPoly1.size(); i++)
			DBOUT( "(" << NewPoly1[i].x << "," << NewPoly1[i].y << "," << NewPoly1[i].z << "),");
			DBOUT("\n");
			DBOUT("NewPoly2\n");
		for (size_t i=0;i<NewPoly2.size(); i++)
			DBOUT( "(" << NewPoly2[i].x << "," << NewPoly2[i].y << "," << NewPoly2[i].z << "),");
			DBOUT("\n");
*/
		// check that all original vertices are in new polygons
		//for (size_t i=0; i<PntList.size(); i++)
		//{
		//	if ( 
		//	(std::find(NewPoly1.begin(), NewPoly1.end(), PntList[i]) == NewPoly1.end()) &&(std::find(NewPoly2.begin(), NewPoly2.end(), PntList[i]) == NewPoly2.end()))
		//		DBOUT("Vertex, (" << PntList[i].x << "," << PntList[i].y << "," << PntList[i].z << "), from PntList not in either Split\n");
		//}

//#endif
		// Make New node
		NewNode = new BSPNode;
		NewNode->PntList = NewPoly1;
		NewNode->Normal = Normal;
		NewNode->_ComputeCenter();
		NewNode->_ComputeNormal();//???
		NewNode->_ComputeD();//???

		// polygon information
		NewNode->m_as_line = m_as_line;
		NewNode->m_id = m_id;
		NewNode->m_type = m_type;
		NewNode->m_thick = m_thick;
		NewNode->m_fill = m_fill;
		NewNode->m_border = m_border;

		PntList.clear();
		PntList = NewPoly2;
		_ComputeCenter();
		_ComputeNormal();
		_ComputeD(); //???


	}

	return NewNode;
}


void BSPNode::Traverse( const CPoint& CameraLoc )
{
	CPoint VecToCam = CameraLoc - Center;

	if( VecToCam.DotProduct( Normal ) < 0 )
	{
		if( FrontNode )
			FrontNode->Traverse( CameraLoc );

		// Process 'this'  i.e. render it to screen

		if( BackNode )
			BackNode->Traverse( CameraLoc );
	}
	else
	{
		if( BackNode )
			BackNode->Traverse( CameraLoc );

		// Process 'this'  i.e. render it to screen

		if( FrontNode )
			FrontNode->Traverse( CameraLoc );
	}
}


void BSPNode::Traverse( const CPoint& CameraLoc, std::vector<s3d::polygon3d*>& polys )
{
	CPoint VecToCam = CameraLoc - Center;
	s3d::polygon3d* new_poly;
#ifdef __DEBUG__
	DBOUT("\n\nTraverse\n");
#endif
	if( VecToCam.DotProduct( Normal ) < 0 )
	{
		if( FrontNode )
			FrontNode->Traverse( CameraLoc, polys );

#ifdef __DEBUG__
		DBOUT("Next Node dot < 0\n");
#endif
		new_poly = WritePoly();
		if (new_poly != NULL)
		{
				polys.push_back(new_poly);
				m_rendered = true;
		}

		if( BackNode )
			BackNode->Traverse( CameraLoc, polys );
	}
	else
	{
		if( BackNode )
			BackNode->Traverse( CameraLoc, polys );

#ifdef __DEBUG__
		DBOUT("Next Node dot >= 0\n");
#endif
		new_poly = WritePoly();
		if (new_poly != NULL)
		{
				polys.push_back(new_poly);
				m_rendered = true;
		}

		if( FrontNode )
			FrontNode->Traverse( CameraLoc, polys );
	}
}




/*
	point.cpp
*/




CPoint::CPoint()
{
	x = y = z = 0.0;
}

CPoint::CPoint( double _x, double _y, double _z)
{
	x = _x;
	y = _y;
	z = _z;
}


void CPoint::Read( s3d::point3d& point )
{
	x = point._x;
	y = point._y;
	z = point._z;
}

void CPoint::Write( s3d::point3d& point ) const
{
	point._x = (float)x;
	point._y = (float)y;
	point._z = (float)z;
}


void CPoint::Read( std::ifstream& Input )
{
	Input >> x >> y >> z;
}


void CPoint::Write( std::ofstream& Output ) const
{
	Output << x << ' ' << y << ' ' << z << '\n' ;
}


double CPoint::Magnitude( void ) const
{
	return sqrt( this->DotProduct( *this ) );
}

void CPoint::Normalize( void )
{
	double Length = Magnitude();

	if( Length )
	{
		x /= Length;
		y /= Length;
		z /= Length;	
	}
	else
		x = y = z = 0.0;
}


double CPoint::DotProduct( const CPoint& Pnt ) const
{
	return x * Pnt.x + y * Pnt.y + z * Pnt.z;
}


CPoint CPoint::CrossProduct( const CPoint& Pnt ) const
{
	CPoint Result;

	Result.x = y * Pnt.z - z * Pnt.y;
	Result.y = z * Pnt.x - x * Pnt.z;
	Result.z = x * Pnt.y - y * Pnt.x;

	return Result;
}


short CPoint::operator==( const CPoint& Pnt ) const
{
	CPoint Result = *this - Pnt;

	return ( MAX_DELTA >= Result.Magnitude() );
}


CPoint CPoint::operator-( const CPoint& Pnt ) const
{
	CPoint Result;

	Result.x = x - Pnt.x;
	Result.y = y - Pnt.y;
	Result.z = z - Pnt.z;
	
	return Result;
}



