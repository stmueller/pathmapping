#include<stdio.h>
#include <R.h>


//This calculates the area of a simple polygon, using the 'shoelace' formula.
//Implementation adapted from description here:
//http://www.dreamincode.net/forums/topic/367070-Area-Of-Polygon/



// Function that calculates the absolute value
// of a double type.
double _Abs(double num)
{

    double inv = num * -1;
    return (num <= 0) ? inv : num;
}


/*
void shoelace(double ** x, double ** y,
             unsigned int * length, double * ans);

 Interface for GNU implementation below
 * 
 * Arguments:
 * 	vec_1, vec_2		The point vectors to compare
 * 	length_1, length_2		Length of strvec_1 and strvec_2
 * 	ans	                 return pointer, 
 */

void shoelace(double * x, 
	       double * y,
               unsigned int * length, double * ans)
{


  
  
  double area = 0; // Total Area
  double diff = 0; // Difference Of Y{i + 1} - Y{i - 1}
  unsigned int last = *length -1 ; // Size Of Vector - 1
  
  /* Given vertices from 1 to n, we first loop through
     the vertices 2 to n - 1. We will take into account
	    vertex 1 and vertex n sepereately */
  for(unsigned int i = 1; i < last; i++)
    {
      diff =y[i + 1] - y[i - 1];
      area += x[i] * diff;
    }
  
  /* Now We Consider The Vertex 1 And The Vertex N */
  diff = y[1] - y[last];
  area += x[0] * diff; // Vertex 1

  diff = y[0] - y[last - 1];
  area += x[last] * diff; // Vertex N

  /* Calculate The Final Answer */
  area = 0.5 * _Abs(area);
  (*ans) = area;
  

  
}


