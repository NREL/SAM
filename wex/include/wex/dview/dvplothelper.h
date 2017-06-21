#ifndef __DVPlotHelper_h
#define __DVPlotHelper_h

/*
 * This class just contains some helper functions that may be useful in several different places in the code.
 */

#include <wx/wx.h>

#include <vector>

namespace wxDVPlotHelper
{
	static int recursionDepth;

	void ZoomFactor(double* worldMin, double* worldMax, double factor, double shiftPercent = 0);
	void MouseWheelZoom(double* worldMin, double* worldMax, wxCoord center, wxCoord physMin, wxCoord physMax, int step);

	void SetRangeEndpointsToDays(double* min, double* max);
	void RoundToNearest(double* numToRound, const double interval);
	void RoundUpToNearest(double* numToRound, const double interval);
	void RoundDownToNearest(double* numToRound, const double interval);

	template <typename T> void Swap(T* a, T* b);
	template <typename T> void BubbleSort(std::vector<T>* data); //Must have > defined.

	template <typename T> void SelectionSort(std::vector<T> &p);

	template <typename T> void QuickSort(std::vector<T>* data, int left, int right);
	template <typename T> void QuickSort(std::vector<T>* data);

	//*** TEMPLATED SORTING ALGORITHMS ***//
	template <typename T>
	void Swap(T* a, T* b)
	{
		T temp = *a;
		*a = *b;
		*b = temp;
	}

	template <typename T>
	void BubbleSort(std::vector<T>* data)
	{
		int n = data->length();
		do
		{
			int newN = 0;
			for (int i=0; i<n-1; i++)
			{
				if (data->at(i) > data->at(i+1))
				{
					Swap(&(data->at(i)), &(data->at(i+1)));
					newN = i+1;
				}
			}
			n = newN;
		}
		while (n > 1);
	}

	template<typename T>
	void SelectionSort(std::vector<T> &p)
	{
		int n = p.size();
		for (int i=0;i<n-1;i++)
		{
			int smallest = i;
			for (int j=i+1;j<n;j++)
				if (p[j] < p[smallest])
					smallest = j;

			// swap
			T temp = p[i];
			p[i] = p[smallest];
			p[smallest] = temp;
		}
	}

	//This crashes if you have many items that are equal in your array.
	template <typename T>
	void QuickSort(std::vector<T>* data, int left, int right)
	{
		int i, j, pivot, temp;

		if(left == right) return; 
		i = left; 
		j = right;
		pivot = data->at((left+right)/2); 

		/* Split the array into two parts */
		do 
		{    
			while (data->at(i) < pivot) i++; 
			while (data->at(j) > pivot) j--;
			if (i<=j) 
			{
				temp= data->at(i);
				data->at(i) = data->at(j);
				data->at(j) = temp;
				i++;
				j--;
			}
		} 
		while (i<=j);
    
		if (left < j) QuickSort(data, left, j);
		if (i < right) QuickSort(data, i, right);
	}

	template <typename T>
	void QuickSort(std::vector<T>* data)
	{
		QuickSort(data, 0, data->length() - 1);
	}
}

#endif
