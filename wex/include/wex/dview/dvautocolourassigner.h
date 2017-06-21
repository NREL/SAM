#ifndef __DVAutoColourAssigner_h
#define __DVAutoColourAssigner_h


#include <wx/wx.h>
#include <vector>

class wxDVAutoColourAssigner
{
public:
	wxDVAutoColourAssigner();
	virtual ~wxDVAutoColourAssigner();

	wxColour GetColourForIndex(int index);
	bool IsColourAssigned( int index );

	virtual wxColour AssignLineColour(int index);
	virtual void DeAssignLineColour(int index);

	void ResetColourList();
	void DeAssignAll();	
private:

	struct ColourCounter
	{
		ColourCounter() { useCount = 0; }
		ColourCounter( const wxColour &c ) : colour(c), useCount(0) { }
		wxColour colour;
		int useCount;
	};

	struct ColourPair
	{
		int index;
		wxColour colour;
	};

	std::vector<ColourCounter> mAvailableColours;
	std::vector<ColourPair> mAssignedColours;
};

#endif

