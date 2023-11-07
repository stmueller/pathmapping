#include<stdio.h>
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>

//uncomment to print out debugging information to check against R version.
//#define PATHMAP_DEBUG=1

//this is a fast implementation of shoelace that requires regural polygons:
void shoelace_c(double * x, double * y,
             unsigned int * length, double * ans);


int even(int i);
int odd(int i);
int isconnected(int r1, int c1, int r2, int c2);

double surveyors3(double x1, double y1,
		  double x2, double y2,
		  double x3, double y3);


// This is probably 20x faster than the general surveyor's formula;
// using the 'shoelace' formula
double surveyors4(double x1, double y1,
		  double x2, double y2,
                  double x3, double y3,
		  double x4, double y4);


SEXP Cost_Area_c(SEXP XY1, 
	       SEXP XY2,
	       SEXP I, SEXP J,
	       SEXP P_I, SEXP P_J,
	       SEXP OPPOSITE);


R_CMethodDef CEntries[]  = {
  {"shoelace_c", (DL_FUNC) &shoelace_c,4},
  {NULL, NULL, 0}
};


 	

R_CallMethodDef callMethods[]  = {
  {"Cost_Area_c", (DL_FUNC) &Cost_Area_c, 7},
//,{REALSXP,REALSXP,  //type definition is not currently supported, it appears.
//  INTSXP,INTSXP,    //unlike for R_CMethodDef
// INTSXP,INTSXP,REALSXP}},
  {NULL, NULL, 0}
};


void R_init_pathmapping(DllInfo*info)
{
  R_registerRoutines(info, CEntries, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info,  TRUE);

   R_RegisterCCallable("pathmapping", "shoelace_c", (DL_FUNC)shoelace_c);
   R_RegisterCCallable("pathmapping", "Cost_Area_c", (DL_FUNC)Cost_Area_c);

}

// Function that calculates the absolute value
// of a double type.
double _Abs(double num)
{

    double inv = num * -1;
    return (num <= 0) ? inv : num;
}



//This calculates the area of a simple polygon, using the 'shoelace' formula.
//Implementation adapted from description here:
//http://www.dreamincode.net/forums/topic/367070-Area-Of-Polygon/
/*

 Interface:
 * 
 * Arguments:
 * 	vec_1, vec_2		The point vectors to compare
 * 	length_1, length_2		Length of strvec_1 and strvec_2
 * 	ans	                 return pointer, 
 */

void shoelace_c(double * x, 
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


int isconnected(int r1, int c1, int r2, int c2)
{

  int connect=0;

  //##odd row 
  if(odd(r2))
    {
      //odd column
      if(odd(c2))
	{
	  //right now, we map a single point on the left (row)
	  //to a single point on the right. (col)
	  //We can only have gotten here from:
	  // previous  odd-even
	  // previous even-odd
	  // previous odd-odd (a 4-gon) match
                
                
	  connect = (((r2-r1)==1) & (c2==c1)) |
	    (((c2-c1) ==1) & (r2==r1)) |
	    ( ((c2-c1) ==2) & ((r2-r1)==2)) ;

                           
                
	}else{
	//even column
	//right now, we map single point on left (row)
	//to segment or the right (column)
	//We could move to either the next point on the right,
	//or the next line segment, sticking on the left.
	//we can only move on the left if we also move on the right,
              
	//this is c-2,r=0  
	//         c-1,r=0
	// but c2-2,r=0 can be done in two jumps through c-1
	
	connect = (((c2-c1)==1) & (r2==r1)) | (((r2-r1)==2) & r2>1 & (c2==c1));
                             

      }
    }  else{
    //Even row
    if( odd(c2)) //odd column
      {
	//right now, we map onto a single point on the right (col)
	// and a segment on the left (row)
	// we can either move onto a single line or a line segment on the
	// right; no single move on the left is possible.
        
	connect = (((r2-r1)==1) & (c2==c1))| (((c2-c1)==2) & c2>1 & (r2==r1));
	
      } else {    //#even column
      
	connect = 0;
    }
  }
  return( connect  );
  
}


//This section is commented out; it is an attempt to translate the area-cost to c for improved speed. It is incomplete and will not work.

int even(int i)
{
  return (i % 2) == 0;
}

int odd(int i)
{
    return (i % 2) == 1;
}


// px/py are 
//


// This is a compiled version of the R code. 
//  
// Here, I,J, etc. are 1-based, but 
// access to xy1, xy2, and opposite need to be 0-based.

SEXP Cost_Area_c(SEXP xy1, 
	       SEXP xy2,
	       SEXP I, SEXP J,
	       SEXP P_I, SEXP P_J,
	       SEXP OPPOSITE)
{

//xy1 is a matrix of x/y values
//xy2 is a matrix of x/y values.

#ifdef PATHMAP_DEBUG
  Rprintf("Calling Cost_Area compiled code\n");
#endif

    int i,j,pi,pj, size1,size2;
    int connected;
    double cost1,cost2,cost;
    double opp,lastopp;
    
    int l1key,l2key,l1keya,l1keyb,l2keya,l2keyb;

    //xy points that we will use for calculating
    //areas of triangles/quadrilaterals
    double x1,y1,x2,y2,x3,y3,x4,y4, xp,yp,xd,yd;

    
    i  = INTEGER(I)[0];
    j  = INTEGER(J)[0];
    pi = INTEGER(P_I)[0];
    pj = INTEGER(P_J)[0];
    
#ifdef PATHMAP_DEBUG
    Rprintf("%d,%d   %d,%d\n",i,j,pi,pj);
#endif



    //Get size of path1 and path2
    SEXP attribs;

    PROTECT(attribs = getAttrib(xy1,R_DimSymbol));
    size1 = INTEGER(attribs)[0];
    UNPROTECT(1);


    PROTECT(attribs = getAttrib(xy2,R_DimSymbol));
    size2 = INTEGER(attribs)[0];
    UNPROTECT(1);





 
    PROTECT(attribs=getAttrib(OPPOSITE,R_DimSymbol));
     //we only need one of these.
     int oppwidth = INTEGER(attribs)[1];
     int oppheight = INTEGER(attribs)[0];
    UNPROTECT(1);


    

    //xy1 and xy2 are coerced into vectors column-wise, so be sure to
    //access them this way..
    xy1 = coerceVector(xy1,REALSXP);
    xy2 = coerceVector(xy2,REALSXP);
    

    //    for(int iii = 0; iii<2*size1;iii++)
    //      Rprintf("v1|%d--%f\n",iii,REAL(xy1)[iii]);

    //    for(int iii = 0; iii<2*size2;iii++)
    //      Rprintf("v2|%d--%f\n",iii,REAL(xy2)[iii]);


    //    for(int iii = 0; iii<oppwidth*oppheight;iii++)
    //        Rprintf("opp|%d--%d|%d|%f\n",iii,oppwidth,oppheight,REAL(OPPOSITE)[iii]);

    
   connected = isconnected(pi,pj,i,j);
  //## The cost of any unconnected transition is infinite.


   if(pi<=0 | pj<=0 | !connected)
    {
      SEXP ans;
      PROTECT(ans = allocVector(REALSXP,1));
      REAL(ans)[0] = INFINITY;
      UNPROTECT(1);
      return ans;
    }


  if(odd(i) & odd(j))
    {

      l1key = (i+1)/2; // Guesses for what these are; 3==5
      l2key = (j+1)/2; //

      // Let's consider first the mapping from pi=i-1; j=j
      if(pi==(i-1) & pj==j)
        {

	  //opp is really if the x1 segment is opposite the x2 point
          //opp <- opposite[i-1,j]
          
	  opp = REAL(OPPOSITE)[(i-1) +(j-1)*oppheight-1];

	  
	  
          // We are doing a triangle, connecting a segment of i to a point on j.
          // The cost is determined by the relative position (recorded in opp)
          // of i to segment on j.

	  //vecstart = xy1[l1key-1,];
  	  x1=REAL(xy1)[(l1key-1)-1];
	  y1=REAL(xy1)[(l1key-1)-1+size1];

	  //veccurr =  xy1[l1key,];
	  x2 = REAL(xy1)[l1key-1];
	  y2 = REAL(xy1)[l1key-1+size1];
	  
          //point = (xy2[l2key,]) ;// ##Point on j series
	  xp = REAL(xy2)[l2key-1];
	  yp = REAL(xy2)[l2key-1+size2];

	  
	  //vecdelt = veccurr-vecstart;
	  xd = x2-x1;
	  yd = y2-y1;

	  
          //vecstart1 = vecstart + opp * vecdelt;
	  x1 = x1 + opp * xd;
	  y1 = y1 + opp * yd;


#ifdef PATHMAP_DEBUG

	  Rprintf("A: %f %f %f %f %f %f|%f\n",x1,y1,x2,y2,xp,yp,opp);
#endif

	  cost = surveyors3(x1,y1,x2,y2,xp,yp);

#ifdef PATHMAP_DEBUG
	  Rprintf("PATH A %f\n",cost);
#endif
	  
        }else if(pi==i & pj == (j-1))
	{
	  //ditto above, but with j-(j-1) segment and point i
	  
	  //opp = opposite[i,j-1];
	  opp = REAL(OPPOSITE)[i +(j-2)*oppheight-1];
	  
	  //vecstart:
	  //vecstart = unlist(xy2[l2key-1,]);
	  x1 = REAL(xy2)[(l2key-1)-1];
	  y1 = REAL(xy2)[(l2key-1)-1+size2];
	  
	  //veccur:
	  //veccurr =  unlist(xy2[l2key,]);
	  x2 = REAL(xy2)[(l2key)-1];
	  y2 = REAL(xy2)[(l2key)-1+size2];
	  
	  //point:
	  //point = unlist(xy1[l1key,]);//  ##Point on j series	    
	  xp = REAL(xy1)[l1key-1];
	  yp = REAL(xy1)[l1key-1+size1];

	  
	  
	  //vecdelt = veccurr-vecstart;
	  xd = x2 - x1;
	  yd = y2 - y1;
	  
	  //vecstart1 = vecstart + opp * vecdelt;
	  x1 = x1 + opp *xd;
	  y1 = y1 + opp *yd;
	  
#ifdef PATHMAP_DEBUG
	  Rprintf("B: %f %f %f %f %f %f|%f\n",x1,y1,x2,y2,xp,yp,opp);
#endif

	  cost = surveyors3(x1,y1,
			    x2,y2,
			    xp,yp);
#ifdef PATHMAP_DEBUG
	  Rprintf("PATH B %f\n",cost);
#endif
	  


	  
	}else if((pi==(i-2)) &(pj ==(j-2)) )
	{
	  
	  //vec1start = unlist(xy1[l1key-1,]);
	  x1 = REAL(xy1)[(l1key-1)-1];
	  y1 = REAL(xy1)[(l1key-1)-1+size1];
	  
	  //vec1curr =  unlist(xy1[l1key,]);
	  x2 = REAL(xy1)[(l1key)-1];
	  y2 = REAL(xy1)[(l1key)-1+size1];
	  
	      
	  //vec2start = unlist(xy2[l2key-1,]);
	  x3 = REAL(xy2)[(l2key-1)-1];
	  y3 = REAL(xy2)[(l2key-1)-1+size2];
	  
	  
	  //vec2curr =  unlist(xy2[l2key,]);
	  x4 = REAL(xy2)[l2key-1];
	  y4 = REAL(xy2)[l2key-1+size2];
	  
	  /*## Because this is a 4-sider, there might
	    ## be a crossover, so see if the different-order
	    ## points are larger.  If so, we have a crossover
	    ## and the area is half the full quadrilateral.
	    ## if cost2 is smaller, then cost is cost1. else cost2/2
	    ## this uses the 'shoelace' formula aka surveyors
	      */
	  

	  cost1 = surveyors4(x1,y1,x2,y2,
			     x4,y4,x3,y3);
	  
	  
	  cost2 = surveyors4(x1,y1,x2,y2,
			     x3,y3,x4,y4);
	  

              if(cost2>cost1)
                {
		  //uh-oh.  The polygon is 'crossed'
                  //cost should be probably cost2, but it depends
                  //on how they cross.  
                  cost = cost2;

                  
                }else
		{
		  cost = cost1;

                }
#ifdef PATHMAP_DEBUG
	      Rprintf("PATH C %f\n",cost);
#endif



            }
          else{

            //##otherwise, the cost is infinite--no legal direct path.
            cost = INFINITY;
#ifdef PATHMAP_DEBUG
	    Rprintf("PATH D %f\n",cost);
#endif


          }
      
    }else if(even(i) & odd(j))
      {

        //##the current mapping is between an edge of i and a point on j.
        //##the area of this segment depends on whether j is opposite i.
	
        //## we could have gotten here from two directions:
	//        ## directly up (i-1), which is the first point on i;
	//        ## or  two to the left (j-2), which is the next 'pie' segment


        l1keya = i/2;     // i is even; grid node i=4 is the connection xy=2-3
        l1keyb = i/2+1;   // i is even; grid node i=4 is the connection xy=2-3

        l2key = (j+1)/2;  // 5=3, e.g.

        if(pi == (i-1) & j==pj)
          {
            //##This is finding the cost of moving from a point-point mapping to
            //##a point-segment mapping, along i.  The cost is the area of the triangle,
            //##regardless of the 'opposite' status (hopefully)

            //opp <- opposite[i,j]
	    opp = REAL(OPPOSITE)[i +(j-1)*oppheight-1];
	    
            //vecstart = unlist(xy1[l1keya,]);
	    x1 = REAL(xy1)[l1keya-1];
	    y1 = REAL(xy1)[l1keya-1+size1];
	    
            //vecdelta = unlist(xy1[l1keyb,])-vecstart;
	    xd = REAL(xy1)[l1keyb-1] - x1;
	    yd = REAL(xy1)[l1keyb-1+size1] - y1;
	    
            //newend = vecstart + vecdelta *opp;
	    x2 = x1 + xd * opp;
	    y2 = y1 + yd * opp;
	    

	    x3 = REAL(xy2)[l2key-1];
	    y3 = REAL(xy2)[l2key-1+size2];

#ifdef PATHMAP_DEBUG
	    Rprintf("E: %f %f %f %f %f %f|%f\n",x1,y1,x2,y2,x3,y3,opp);
#endif

            cost = surveyors3(x1,y1,x2,y2,x3,y3);
#ifdef PATHMAP_DEBUG
	    Rprintf("PATH E %f\n",cost);
#endif
	    


	  }else if(pi == i & (j-2)==(pj))
            {
              //i is even, so the i path is a segment:
              //j is odd, so it is just a single point
              //jump from (i,j-2)

              //vecstart = unlist(xy1[l1keya,]);
	      x1=REAL(xy1)[l1keya-1];
	      y1=REAL(xy1)[l1keya-1+size1];
	      
              //vecdelta = unlist(xy1[l1keyb,])-vecstart;
	      xd = REAL(xy1)[l1keyb-1] - x1;
	      yd = REAL(xy1)[l1keyb-1+size1] - y1;
	      	      
              //point = unlist(xy2[l2key,]);
	      xp = REAL(xy2)[l2key-1];
	      yp = REAL(xy2)[l2key-1+size2];

	      
              //lastpoint = unlist(xy2[l2key-1,]);
	      x2=REAL(xy2)[(l2key-1)-1];
	      y2=REAL(xy2)[(l2key-1)-1+size2];
	      
              //opp <- opposite[i,j]
	      opp = REAL(OPPOSITE)[i +(j-1)*oppheight-1];
              //##in this case, the current tentative mapping should be the orthogonal line.
	      //##connected to the point, rather than the whole segment (next point).
              //##Thus, we have a newpoint along l1ab.

              //vecend = vecstart + vecdelta * opp ##the proportional distance.
	      x3 = x1 + xd * opp;
	      y3 = y1 + yd * opp;

	      //lastopp <- opposite[i,j-2]
	      lastopp = REAL(OPPOSITE)[i +(j-2-1)*oppheight-1];
	      
              if(lastopp>0)
                {

                  //the previous mapping was 'opposite'.  This means
		  //we need to find the two points in the i vector, and
                  //the current and previous point on the j vector, and
                  //compute the area of the polygon.
                  
		  //    vecprevend = vecstart + vecdelta * lastopp;
		  x4 = x1 + xd * lastopp;
		  y4 = y1 + yd * lastopp;
		  
		  
                  //##Now, if the current point is not opposite, we need
                  //##to measure the following 3-gon:
                  
                  if(opp)
                    {
		      
#ifdef PATHMAP_DEBUG
		      Rprintf("F: %f %f %f %f %f %f %f %f|%f\n",x4,y4,x3,y3,xp,yp,x2,y2,opp);
#endif

                      cost = surveyors4(x4,y4,
					x3,y3,
					xp,yp,
					x2,y2);
#ifdef PATHMAP_DEBUG
		      Rprintf("PATH F %f\n",cost);
#endif


                      //##this is a quadrilateral, but I don't think we need to
                      //##try both mappings and do the max.  Otherwise, crossovers
                      //##would be a problem.
                      

                                    
                    }else{
		    //

		    cost = surveyors4(x4,y4,x3,y3,xp,yp,x2,y2);
#ifdef PATHMAP_DEBUG
		    Rprintf("PATH G %f\n",cost);
#endif

                    }
                
                }else{
		//if that previous point was not 'opposite', we shouldn't use this route;
                  


		cost = INFINITY;
#ifdef PATHMAP_DEBUG
		Rprintf("PATH H %f\n",cost);
#endif

                }
            }else{


	  cost = INFINITY;
#ifdef PATHMAP_DEBUG
	  Rprintf("PATH I %f\n",cost);
#endif

            }
                                 
      
        
       }else if(odd(i) & even(j))
         {

           //##The cost here is symmetric to the even/odd one above.
           
           //##the current mapping is between an edge of i and a point on j.
           //##the area of this segment depends on whether j is opposite i.
           
           //## we could have gotten here from two directions:
           //## directly up (i-1), which is the first point on i;
           //## or  two to the left (j-2), which is the next 'pie' segment
           

           l2keya = j/2;  //  i is even; grid node i=4 is the connection xy=2-3
	   l2keyb = j/2+1;//  i is even; grid node i=4 is the connection xy=2-3

           l1key = (i+1)/2 ; //## 5=3, e.g.


           if(pj == (j-1) & i==pi)
             {
               //##This is finding the cost of moving from a point-point mapping to
               //##a point-segment mapping, along j.  The cost is the area of the triangle,
               //##regardless of the 'opposite' status (hopefully)

               //opp <- opposite[i,j]
#ifdef PATHMAP_DEBUG
#endif
	       opp = REAL(OPPOSITE)[i +(j-1)*oppheight-1];
               //vecstart = unlist(xy2[l2keya,]);
	       x1 = REAL(xy2)[l2keya-1];
	       y1 = REAL(xy2)[l2keya-1+size2];

	       
               //vecdelta = unlist(xy2[l2keyb,])-vecstart;
	       xd=REAL(xy2)[l2keyb-1]-x1;
	       yd=REAL(xy2)[l2keyb-1+size2]-y1;
	       
               //newend = vecstart + vecdelta *opp;
	       x2 = x1 + xd * opp;
	       y2 = y1 + yd * opp;

	       //this might be off-by-one:
	       x3 = REAL(xy1)[l1key-1];
	       y3 = REAL(xy1)[l1key-1+size1];

#ifdef PATHMAP_DEBUG
	       Rprintf("j: %f %f %f %f %f %f|%f\n",x1,y1,x2,y2,x3,y3,opp);
#endif

               cost = surveyors3(x1,y1,
				 x2,y2,
				 x3,y3);
#ifdef PATHMAP_DEBUG
	       Rprintf("PATH J %f\n",cost);
#endif


	       
             }else if(pj == j & (i-2)==(pi))
               {
                 //##j is even, so the j path is a segment:
                 //##i is odd, so it is just a single point
                 //##jump from (i-2,j)

		 //opp <- opposite[i,j]
		 opp = REAL(OPPOSITE)[i +(j-1)*oppheight-1];


		 //vecstart = unlist(xy2[l2keya,]);
		 x1 = REAL(xy2)[l2keya-1];
		 y1 = REAL(xy2)[l2keya-1+size2];
		 
                 //vecdelta = unlist(xy2[l2keyb,])-vecstart;
		 xd = REAL(xy2)[l2keyb-1]-x1;
		 yd = REAL(xy2)[l2keyb-1+size2]-y1;
		 
                 //point = unlist(xy1[l1key,]);
		 xp = REAL(xy1)[l1key-1];
		 yp = REAL(xy1)[l1key-1+size1];
		 
                 //lastpoint = unlist(xy1[l1key-1,]);
                 x2 = REAL(xy1)[(l1key-1)-1];
		 y2 = REAL(xy1)[(l1key-1)-1+size1];
		   
                 //##in this case, the current tentative mapping should be the orthogonal line.
                 //##connected to the point, rather than the whole segment (next point).
                 //##Thus, we have a newpoint along l1ab.
		 
                 //vecend = vecstart + vecdelta * opp;// ##the proportional distance.

		 x3 = x1 + xd * opp;
		 y3 = y1 + yd * opp;
		 
		 //lastopp <- opposite[i-2,j]		                 
		 lastopp = REAL(OPPOSITE)[i-2 +(j-1)*oppheight-1];
		  
                 if(lastopp>0)
                   {
		     
                     //##the previous mapping was 'opposite'.  This means
                     //##we need to find the two points in the i vector, and
                     //##the current and previous point on the j vector, and
                     //##compute the area of the polygon.
                     
                     //vecprevend = vecstart + vecdelta * lastopp;
		     x4 = x1 + xd * lastopp;
		     y4 = y1 + yd * lastopp;
		     
                  
		     //##Now, if the current point is not opposite, we need
		     //##to measure the following 3-gon:
                     
                     if(opp)
                       {
			 
                         cost = surveyors4(x4,y4,x3,y3,xp,yp,x2,y2);

			 //##this is a quadrilateral, but I don't think we need to
			 //##try both mappings and do the max.  Otherwise, crossovers
			 //##would be a problem.
                         
#ifdef PATHMAP_DEBUG
			 Rprintf("PATH K %f\n",cost);
#endif

                         
                       }else{

#ifdef PATHMAP_DEBUG
         	       Rprintf("L: %f,%f %f,%f %f,%f | %f\n",x4,y4,x3,y3,xp,yp,x2,y2,opp);
#endif


		       cost = surveyors4(x4,y4,x3,y3,xp,yp,x2,y2);
#ifdef PATHMAP_DEBUG
		       Rprintf("PATH L %f\n",cost);
#endif



		       
		     }
                   }else {
		   cost = INFINITY;
#ifdef PATHMAP_DEBUG
             	     Rprintf("PATH MM %f\n",cost);   
#endif

		 }
               }else{
	     //##if that previous point was not 'opposite', we shouldn't use this route;
             
	     cost = INFINITY;
#ifdef PATHMAP_DEBUG
	     Rprintf("PATH M %f\n",cost);
#endif

	   }
	   
	   
           
	   
         } else {
    cost = INFINITY;
#ifdef PATHMAP_DEBUG
    Rprintf("PATH N %f\n",cost);
#endif

  }


   SEXP ans;
   PROTECT(ans = allocVector(REALSXP,1));
   REAL(ans)[0] = cost;
   UNPROTECT(1);
   return ans;

}





// This is probably 20x faster than the general surveyor's formula;
// using the 'shoelace' formula
double surveyors3(double x1, double y1,
		  double x2, double y2,
		  double x3, double y3)
  {
    return _Abs((x1*y2 + x2*y3 + x3*y1
		 -x2*y1 - x3*y2 - x1*y3)/2);
  }


// This is probably 20x faster than the general surveyor's formula;
// using the 'shoelace' formula
double surveyors4(double x1, double y1,
		  double x2, double y2,
		    double x3, double y3,
		    double x4, double y4)
		    
  {
    return _Abs((x1*y2 + x2*y3 + x3*y4 + x4 * y1
		 -x2*y1 - x3*y2 - x4*y3 - x1*y4)/2);
    
  }
