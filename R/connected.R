## This determines whether two nodes in the lattice are
## connected; i.e., there is a legal transition.
##

connected <-
function(r1,c1,r2,c2)
  {
   # width <- ncol(nodes)
   # height <- nrow(nodes)
    
#    if(r1<1 | c1<1 | r2<1 | c2<1)
#      {
#        connect <-  0
#      } else {

        if(T){

        ##odd row 
        if(odd(r2))
          {
            ##odd column
            if(odd(c2))
              {
                ##right now, we map a single point on the left (row)
                ##to a single point on the right. (col)
                ##We can only have gotten here from:
                ## previous  odd-even
                ## previous even-odd
                ## previous odd-odd (a 4-gon) match
                
                
                connect <- (((r2-r1)==1) & (c2==c1)) |
                           (((c2-c1) ==1) & (r2==r1)) |
                           ( ((c2-c1) ==2) & ((r2-r1)==2)) 
                ##(((r2-r1)==2) & ((c2-c1)==2))  ##don't use two back diag.  Compose this of small steps.
                           
                
              }else{
                ##even column
                ##right now, we map single point on left (row)
                ##to segment or the right (column)
                ##We could move to either the next point on the right,
                ##or the next line segment, sticking on the left.
                ##we can only move on the left if we also move on the right,
              
                ##this is c-2,r=0  
                #         c-1,r=0
                # but c2-2,r=0 can be done in two jumps through c-1
                 connect <- (((c2-c1)==1) & (r2==r1)) | (((r2-r1)==2) & r2>1 & (c2==c1))
                             

              }}
        else{#Even row
            
          if( odd(c2))  #odd column
            {
              ##right now, we map onto a single point on the right (col)
              ## and a segment on the left (row)
              ##we can either move onto a single line or a line segment on the
              ##right; no single move on the left is possible.
              
              connect <- (((r2-r1)==1) & (c2==c1))| (((c2-c1)==2) & c2>1 & (r2==r1))
              
            } else{    #even column


              connect <- F
            }
        }

      }
        return( connect  )
  }
