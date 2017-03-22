#include<stdio.h>
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>



//This calculates the area of a simple polygon, using the 'shoelace' formula.
//Implementation adapted from description here:
//http://www.dreamincode.net/forums/topic/367070-Area-Of-Polygon/

void shoelace(double * x, double * y,
             unsigned int * length, double * ans);



static const R_CMethodDef CEntries[]  = {
  {"shoelace", (DL_FUNC) &shoelace,4},
  {NULL, NULL, 0}
};


void R_init_shoelace(DllInfo*info)
{
  R_registerRoutines(info, CEntries, NULL, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info,  TRUE);

  R_RegisterCCallable("pathmapping", "shoelace", (DL_FUNC)shoelace);
}

// Function that calculates the absolute value
// of a double type.
double _Abs(double num)
{

    double inv = num * -1;
    return (num <= 0) ? inv : num;
}



/*

 Interface for GNU implementation below
 * 
 * Arguments:
 * 	vec_1, vec_2		The point vectors to compare
 * 	length_1, length_2		Length of strvec_1 and strvec_2
 * 	ans	                 return pointer, 
 */

void shoelace(double * x, 
	       double * y,
               unsigned int * length,
	      double * ans)
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

//This section is commented out; it is an attempt to translate the area-cost to c for improved speed. It is incomplete and will not work.
#if 0

bool even(int i)
{
  return (i %% 2) == 0;
}

void odd(int i)
{
    return (i %% 2) == 1;
}

void Cost_Area(double * px, 
	       double * py,
               unsigned int * length,
	       int * ii, int * jj,
	       int* ppi, int* ppj,
               bool *opposite,
	      double * ans)

{

  int i = *i;
  int j = *j;
  int pi = *ppi;
  int pj = *ppj;
    

  //## The cost of any unconnected transition is infinite. We will represent that as ???.
  if(pi<=0 | pj<=0 | !connected(pi,pj,i,j)) //need compiled implementation of connected?
    {
      return (Inf);
    }

  
  if(odd(i) & odd(j))
    {

      l1key = (i+1)/2; // Guesses for what these are; 3==5
      l2key = (j+1)/2; //



							// Let's consider first the mapping from pi=i-1; j=j
      if(pi==(i-1)&pj==j)
        {
          //##opp is really if the x1 segment is opposite the x2 point
          opp = opposite[i-1,j];
          //## We are doing a triangle, connecting a segment of i to a point on j.
          //## The cost is determined by the relative position (recorded in opp)
          //## of i to segment on j.  

	  vecstart = (xy1[l1key-1,]);
          veccurr =  (xy1[l1key,]);
          point = (xy2[l2key,]) ;// ##Point on j series
          vecdelt = veccurr-vecstart;
          vecstart1 = vecstart + opp * vecdelt;

	  cost = surveyors.3(rbind(vecstart1,
				   veccurr,
				   point)) ;
	  
        }else if(pi==i & pj == (j-1))
	{
            //ditto above, but with j-(j-1) segment and point i

            opp = opposite[i,j-1];
            vecstart = unlist(xy2[l2key-1,]);
            veccurr =  unlist(xy2[l2key,]);
            point = unlist(xy1[l1key,]);//  ##Point on j series
            vecdelt = veccurr-vecstart;
            vecstart1 = vecstart + opp * vecdelt;
            cost = surveyors.3(rbind(vecstart1,
                                    veccurr,
				      point)) ;
            
          }else if((pi==(i-2)) &(pj ==(j-2)) )
            {

              vec1start = unlist(xy1[l1key-1,]);
              vec1curr =  unlist(xy1[l1key,]);

              vec2start = unlist(xy2[l2key-1,]);
	      vec2curr =  unlist(xy2[l2key,]);

              /*## Because this is a 4-sider, there might
              ## be a crossover, so see if the different-order
              ## points are larger.  If so, we have a crossover
              ## and the area is half the full quadrilateral.
              ## if cost2 is smaller, then cost is cost1. else cost2/2
              ## this uses the 'shoelace' formula aka surveyors
	      */
								 
              cost1 = surveyors.4(rbind(vec1start,
                                      vec1curr,
                                      vec2curr,
					 vec2start)) ;

              cost2 = surveyors.4(rbind(vec1start,
                                       vec1curr,
                                       vec2start,
                                       vec2curr
					 )) ;

              if(cost2>cost1)
                {
		  //  ##uh-oh.  The polygon is 'crossed'
                  //##cost should be probably cost2, but it depends
                  //##on how they cross.  
                  cost = cost2;

                  
                }else{
		cost = cost1;
                }
              
            }
          else{
            //##otherwise, the cost is infinite--no legal direct path.
            cost = Inf
          }
      
    }else if(even(i) & odd(j))
      {

        //##the current mapping is between an edge of i and a point on j.
        //##the area of this segment depends on whether j is opposite i.
	
        //## we could have gotten here from two directions:
	//        ## directly up (i-1), which is the first point on i;
	//        ## or  two to the left (j-2), which is the next 'pie' segment


        l1keya = i/2 ;//  ## i is even; grid node i=4 is the connection xy=2-3
        l1keyb = i/2+1 ;//## i is even; grid node i=4 is the connection xy=2-3

        l2key = (j+1)/2 ;//## 5=3, e.g.

        if(pi == (i-1) & j==pj)
          {
            //##This is finding the cost of moving from a point-point mapping to
            //##a point-segment mapping, along i.  The cost is the area of the triangle,
            //##regardless of the 'opposite' status (hopefully)
            opp = opposite[i,j];
            vecstart = unlist(xy1[l1keya,]);
            vecdelta = unlist(xy1[l1keyb,])-vecstart;
            newend = vecstart + vecdelta *opp;
      
                                
            cost = surveyors.3(rbind(vecstart,
                                    newend,
				      unlist(xy2[l2key,])));
          }else if(pi == i & (j-2)==(pj))
            {
              //##i is even, so the i path is a segment:
              //##j is odd, so it is just a single point
              //##jump from (i,j-2)

              vecstart = unlist(xy1[l1keya,]);
              vecdelta = unlist(xy1[l1keyb,])-vecstart;
              point = unlist(xy2[l2key,]);
              lastpoint = unlist(xy2[l2key-1,]);

              opp = opposite[i,j];
              
              //##in this case, the current tentative mapping should be the orthogonal line.
	      //##connected to the point, rather than the whole segment (next point).
              //##Thus, we have a newpoint along l1ab.
              //vecend = vecstart + vecdelta * opp ##the proportional distance.

              
              lastopp = opposite[i,j-2];

              if(lastopp>0)
                {

                  //##the previous mapping was 'opposite'.  This means
		  //##we need to find the two points in the i vector, and
                  //##the current and previous point on the j vector, and
                  //##compute the area of the polygon.
                  
                  vecprevend = vecstart + vecdelta * lastopp;
                  
                  //##Now, if the current point is not opposite, we need
                  //##to measure the following 3-gon:
                  
                  if(opp)
                    {
                      cost = surveyors.4(rbind(vecprevend,
                                              vecend,
                                              point,
						lastpoint));
                      //##this is a quadrilateral, but I don't think we need to
                      //##try both mappings and do the max.  Otherwise, crossovers
                      //##would be a problem.
                      

                                    
                    }else{
		    //
                      cost = surveyors.4(rbind(vecprevend,
                                              vecend,
                                              point,
						lastpoint));
                      
                    }
                
                }else{
		//##if that previous point was not 'opposite', we shouldn't use this route;
                  
                  cost = Inf;

                }
            }else{
              cost = Inf;
            }
                                 
      
        
       }else if(odd(i) & even(j))
         {

           //##The cost here is symmetric to the even/odd one above.
           
           //##the current mapping is between an edge of i and a point on j.
           //##the area of this segment depends on whether j is opposite i.
           
           //## we could have gotten here from two directions:
           //## directly up (i-1), which is the first point on i;
           //## or  two to the left (j-2), which is the next 'pie' segment
           

           l2keya = j/2;// ## i is even; grid node i=4 is the connection xy=2-3
	   l2keyb = j/2+1;// ## i is even; grid node i=4 is the connection xy=2-3

           l1key = (i+1)/2 //## 5=3, e.g.


           if(pj == (j-1) & i==pi)
             {
               //##This is finding the cost of moving from a point-point mapping to
               //##a point-segment mapping, along j.  The cost is the area of the triangle,
               //##regardless of the 'opposite' status (hopefully)
               opp = opposite[i,j];
               vecstart = unlist(xy2[l2keya,]);
               vecdelta = unlist(xy2[l2keyb,])-vecstart;
               newend = vecstart + vecdelta *opp;
               cost = surveyors.3(rbind(vecstart,
                                       newend,
					 unlist(xy1[l1key,])));

             }else if(pj == j & (i-2)==(pi))
               {
                 //##j is even, so the j path is a segment:
                 //##i is odd, so it is just a single point
                 //##jump from (i-2,j)

                 opp = opposite[i,j];
                 
                 vecstart = unlist(xy2[l2keya,]);
                 vecdelta = unlist(xy2[l2keyb,])-vecstart;
                 point = unlist(xy1[l1key,]);
                 lastpoint = unlist(xy1[l1key-1,]);
                 
                 //##in this case, the current tentative mapping should be the orthogonal line.
                 //##connected to the point, rather than the whole segment (next point).
                 //##Thus, we have a newpoint along l1ab.
                 vecend = vecstart + vecdelta * opp;// ##the proportional distance.
                 
                 lastopp = opposite[i-2,j]
               
                 if(lastopp>0)
                   {
		     
                     //##the previous mapping was 'opposite'.  This means
                     //##we need to find the two points in the i vector, and
                     //##the current and previous point on the j vector, and
                     //##compute the area of the polygon.
                     
                     vecprevend = vecstart + vecdelta * lastopp;

                  
		     //##Now, if the current point is not opposite, we need
		     //##to measure the following 3-gon:
                     
                     if(opp)
                       {
			 
                         cost = surveyors.4(rbind(vecprevend,
						   vecend,
						   point,
						   lastpoint));
			 //##this is a quadrilateral, but I don't think we need to
			 //##try both mappings and do the max.  Otherwise, crossovers
			 //##would be a problem.
                         

                         
                       }else{
		       
		       cost = surveyors.4(rbind(vecprevend,
                                                 vecend,
                                                 point,
                                                 lastpoint));
		       
		     }
                   }else {
		   cost = Inf;
			                    
		 }
               }else{
	     //##if that previous point was not 'opposite', we shouldn't use this route;
                 
	     cost = Inf;
                 
               }
                       

           

         } else {
    cost = Inf;
         }

  return cost;
  
}
#endif
