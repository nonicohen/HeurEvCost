# scriptTestsByDQ.sh arguments:
#
# 1) Path of 'scriptPrologTestsByDQ.pl',  'heurEvCostImpl.pl' and files to be created (stats).
#
# OBS: 	- It is assumed that the two prolog files are in the same directory (Path)
#       - Path is assumed not to contain the final ‘/‘
#
# Example:
#
# sh scriptTestsByDQ.sh /User/Desktop/Implementation

#—————————————
# Remove the stats files (if existing)
# Call the Prolog script varying Deactivation Quota parameter = 1, 10, 20, 30
# For each batch of tests, the output is saved into a .csv file

rm -f $1/statsDQ30.csv

swipl -s $1/scriptPrologTestsByDQ $1 30 >> $1/statsDQ30.csv &

rm -f $1/statsDQ20.csv

swipl -s $1/scriptPrologTestsByDQ $1 20 >> $1/statsDQ20.csv &

rm -f $1/statsDQ10.csv

swipl -s $1/scriptPrologTestsByDQ $1 10 >> $1/statsDQ10.csv &

rm -f $1/statsDQ1.csv

swipl -s $1/scriptPrologTestsByDQ $1 1 >> $1/statsDQ1.csv &
