# HeurEvCost

In order to run the tests the bash shell script file 'scriptTestsByDQ.sh' has to be executed. 
The script runs a whole batch of tests for Deactivation Quota values of 1, 10, 20 and 30.

It has one argument: 

  - Path of Prolog files 'scriptPrologTestsByDQ.pl', 'heurEvCostImpl.pl' and the .csv output files (to be created) 

OBS: 	
   - It is assumed that the two prolog files are in the same directory (Path)
   - Path is assumed not to contain the final '/'

------------------------
Example of use:

>> sh scriptTestsByDQ.sh /User/Desktop/Implementation
