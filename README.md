pathmapping
===========

pathmapping package for the R statistical computing language.

Developed by Shane T. Mueller and Brandon Perelman
Dept of Cognitive and Learning Sciences
Michigan Technological University


This package allows you to measure the area-based deviation between pairs of paths.  It uses an optimization approach to provide robust area-based measures.  It produces both an area-measure and a mapping between paths. 

More details are found at:

https://sites.google.com/a/mtu.edu/mapping/

Source code repository at:

https://github.com/stmueller/pathmapping/


Mueller, S. T., Perelman, B. S., & Veinott, E. S. (2016). An optimization approach for mapping and measuring the divergence and correspondence between paths. Behavior research methods, 48(1), 53-71. <DOI:10.3758/s13428-015-0562-7>.

# prepdat 1.0.1: March 2017

-  Documentation improved.
- Fixed a problem with array dimension names in CreateMap
- Added compiled function shoelace formula.  This is not used, because it is a bit slower than special-purpose surveyor's formulae for 3/4 points, but is a lot faster than the general-purpose surveyor's formula.
- Version number updated.

# prepdat 1.0: 2014/2015.
- First release