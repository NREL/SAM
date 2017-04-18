#include "wex/dview/dvautocolourassigner.h"


wxDVAutoColourAssigner::wxDVAutoColourAssigner()
{
	ResetColourList();
}

wxDVAutoColourAssigner::~wxDVAutoColourAssigner()
{
}

wxColour wxDVAutoColourAssigner::GetColourForIndex(int index)
{
	for (int i=0; i<mAssignedColours.size(); i++)
	{
		if (mAssignedColours[i].index == index)
			return mAssignedColours[i].colour;
	}

	return *wxRED;
}

bool wxDVAutoColourAssigner::IsColourAssigned( int index )
{
	for (int i=0; i<mAssignedColours.size(); i++)
		if (mAssignedColours[i].index == index)
			return true;

	return false;
}

wxColour wxDVAutoColourAssigner::AssignLineColour(int index)
{
	ColourPair cp;
	wxColour tempColor = *wxRED;
	int tempUseCount = 1000000000;	//Start with an impossibly high number of uses.  We'll reduce it until we find the lowest use count.
	int colorIndex;

	for(int i = 0; i < mAvailableColours.size(); i++)
	{
		if(tempUseCount > mAvailableColours[i].useCount)
		{
			colorIndex = i;
			tempUseCount = mAvailableColours[i].useCount;
			tempColor = mAvailableColours[i].colour;
			if(tempUseCount == 0) { break; }
		}
	}

	mAvailableColours[colorIndex].useCount++;

	cp.index = index;
	cp.colour = tempColor;
	
	mAssignedColours.push_back(cp);
	
	return cp.colour;
}

void wxDVAutoColourAssigner::ResetColourList()
{
	ColourCounter cc;

	mAvailableColours.clear();

	/*
	mAvailableColours.push_back( ColourCounter("red") );
	mAvailableColours.push_back( ColourCounter("forest green") );
	mAvailableColours.push_back( ColourCounter("blue") );
	mAvailableColours.push_back( ColourCounter("purple") );
	mAvailableColours.push_back( ColourCounter("salmon") );
	mAvailableColours.push_back( ColourCounter("magenta") );
	mAvailableColours.push_back( ColourCounter("grey") );
	mAvailableColours.push_back( ColourCounter("aquamarine") );
	mAvailableColours.push_back( ColourCounter("brown") );
	*/

	mAvailableColours.push_back( ColourCounter( wxColour( 0, 114, 198 ) ) );
	mAvailableColours.push_back( ColourCounter( wxColour( 255,150,64) ) );
	mAvailableColours.push_back( ColourCounter("FIREBRICK") );
	mAvailableColours.push_back( ColourCounter("DARK SLATE GREY") );
	mAvailableColours.push_back( ColourCounter("PALE GREEN") );
	mAvailableColours.push_back( ColourCounter("MEDIUM VIOLET RED") );
	mAvailableColours.push_back( ColourCounter("GOLDENROD") );
	mAvailableColours.push_back( ColourCounter("dark orchid") );
}

void wxDVAutoColourAssigner::DeAssignAll()
{
	mAssignedColours.clear();
	ResetColourList();
}

void wxDVAutoColourAssigner::DeAssignLineColour(int index)
{
	for (int i = 0; i < mAssignedColours.size(); i++)
	{
		if (mAssignedColours[i].index == index)
		{
			for(int j = 0; j < mAvailableColours.size(); j++)
			{
				if(mAvailableColours[j].colour == mAssignedColours[i].colour) 
				{
					mAssignedColours.erase( mAssignedColours.begin() + i );
					mAvailableColours[j].useCount--;
					if(mAvailableColours[j].useCount < 0) { mAvailableColours[j].useCount = 0; }

					return;
				}
			}
		}
	}
}