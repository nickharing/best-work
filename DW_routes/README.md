# DWroutes
Optimization of sampling routes for Drinking Water sampling staff

Links:  
https://www.r-bloggers.com/the-travelling-salesman-portrait/  
http://toddwschneider.com/posts/traveling-salesman-with-simulated-annealing-r-and-shiny/

Potential Code
1. QA addresses and lat/longs of sampling locations 
2. Pull in street map data for San Diego 
3. Add sample locations to the map. 
4. Calculate least distance (probably a traveling salesman package) to complete all sample locations in a single route 
5. Use current sample routes and above calculated route to inform splitting into probably 4 sub-routes. 
6. Calculate least distances for each sub-route. 
7. Compare R-generated routes with current routes. 
8. Get feedback from samplers and/or partial ride along to ground truth route changes. 

Notes from Mike:  
4 separate routes on Monday, Tuesday and Thursday.  
Only 3 separate routes on Wednesday due to Wednesday having 3 separate monthly routes (CST 1, CST 2, LSI)  
