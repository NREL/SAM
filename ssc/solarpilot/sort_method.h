#ifndef _SORT_
#define _SORT_ 1

#include <algorithm>
#include <vector>
using namespace std;

template <typename Comparable, typename Tag>
void insertionSort( vector<Comparable> & a, vector<Tag> & b, int left, int right );
/**
 * Quicksort algorithm (driver).
 */

 /*------------------ Quicksort for two vectors -------------------- */


/**
 * Return median of left, center, and right.
 * Order these and hide the pivot.
 */
template <typename Comparable, typename Tag>
const Comparable & median3( vector<Comparable> & a, vector<Tag> &b, int left, int right )
{
    int center = ( left + right ) / 2;
    if( a[ center ] < a[ left ] ){
        swap( a[ left ], a[ center ] );
		swap( b[ left ], b[ center ] );}
    if( a[ right ] < a[ left ] ){
        swap( a[ left ], a[ right ] );
		swap( b[ left ], b[ right ] );}
    if( a[ right ] < a[ center ] ){
        swap( a[ center ], a[ right ] );
		swap( b[ center ], b[ right ] );}

        // Place pivot at position right - 1
    swap( a[ center ], a[ right - 1 ] );
	swap( b[ center ], b[ right - 1 ] );
    return a[ right - 1 ];
}

/**
 * Internal insertion sort routine for subarrays
 * that is used by quicksort.
 * a is an array of Comparable items.
 * left is the left-most index of the subarray.
 * right is the right-most index of the subarray.
 */
template <typename Comparable, typename Tag>
void insertionSort( vector<Comparable> & a, vector<Tag> & b, int left, int right )
{
    for( int p = left + 1; p <= right; p++ )
    {
        Comparable tmp = a[ p ];
		Tag tmp2 = b[ p ];
        int j;

        for( j = p; j > left && tmp < a[ j - 1 ]; j-- ){
            a[ j ] = a[ j - 1 ];
			b[ j ] = b[ j - 1 ];
		}
        a[ j ] = tmp;
		b[ j ] = tmp2;
    }
}

/**
 * Internal quicksort method that makes recursive calls.
 * Uses median-of-three partitioning and a cutoff of 10.
 * a is an array of Comparable items.
 * left is the left-most index of the subarray.
 * right is the right-most index of the subarray.
 */
template <typename Comparable, typename Tag>
void quicksort( vector<Comparable> & a, vector<Tag> & b, int left, int right )
{
    if( left + 10 <= right )
    {
        Comparable pivot = median3( a, b, left, right );

            // Begin partitioning
        int i = left, j = right - 1;
        for( ; ; )
        {
            while( a[ ++i ] < pivot ) { }
            while( pivot < a[ --j ] ) { }
            if( i < j ){
                swap( a[ i ], a[ j ] );
				swap( b[ i ], b[ j ] );}
			else {break;}
        }

        swap( a[ i ], a[ right - 1 ] );  // Restore pivot
		swap( b[ i ], b[ right - 1 ] );

        quicksort( a, b, left, i - 1 );     // Sort small elements
		quicksort( a, b, i + 1, right );    // Sort large elements
		
    }
    else { // Do an insertion sort on the subarray
        insertionSort( a, b, left, right );
	}
}




/**
 * Return median of left, center, and right.
 * Order these and hide the pivot.
 */
template <typename Comparable>
const Comparable & median3( vector<Comparable> & a, int left, int right )
{
    int center = ( left + right ) / 2;
    if( a[ center ] < a[ left ] ){
        swap( a[ left ], a[ center ] );
		}
    if( a[ right ] < a[ left ] ){
        swap( a[ left ], a[ right ] );
		}
    if( a[ right ] < a[ center ] ){
        swap( a[ center ], a[ right ] );
		}

        // Place pivot at position right - 1
    swap( a[ center ], a[ right - 1 ] );
	
    return a[ right - 1 ];
}

/**
 * Internal insertion sort routine for subarrays
 * that is used by quicksort.
 * a is an array of Comparable items.
 * left is the left-most index of the subarray.
 * right is the right-most index of the subarray.
 */
template <typename Comparable>
void insertionSort( vector<Comparable> & a, int left, int right )
{
    for( int p = left + 1; p <= right; p++ )
    {
        Comparable tmp = a[ p ];
		
        int j;

        for( j = p; j > left && tmp < a[ j - 1 ]; j-- ){
            a[ j ] = a[ j - 1 ];
			
		}
        a[ j ] = tmp;
		
    }
}

/**
 * Internal quicksort method that makes recursive calls.
 * Uses median-of-three partitioning and a cutoff of 10.
 * a is an array of Comparable items.
 * left is the left-most index of the subarray.
 * right is the right-most index of the subarray.
 */
template <typename Comparable>
void quicksort( vector<Comparable> & a, int left, int right )
{
    if( left + 10 <= right )
    {
        Comparable pivot = median3( a, left, right );

            // Begin partitioning
        int i = left, j = right - 1;
        for( ; ; )
        {
            while( a[ ++i ] < pivot ) { }
            while( pivot < a[ --j ] ) { }
            if( i < j ){
                swap( a[ i ], a[ j ] );
				}
			else {break;}
        }

        swap( a[ i ], a[ right - 1 ] );  // Restore pivot
		

        quicksort( a, left, i - 1 );     // Sort small elements
		quicksort( a, i + 1, right );    // Sort large elements
		
    }
    else { // Do an insertion sort on the subarray
        insertionSort( a, left, right );
	}
}

template <typename Comparable, typename Tag>
void quicksort( vector<Comparable> & a, vector<Tag> & b)
{
    quicksort( a, b, 0, a.size( ) - 1 );
}

/*------------------ Quicksort for one vector -------------------- */

template <typename Comparable>
void quicksort( vector<Comparable> & a)
{
    quicksort( a, 0, (int)a.size( ) - 1 );
}
#endif
