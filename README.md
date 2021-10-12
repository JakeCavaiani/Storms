# Storms
Storm analysis/plots
The purpose of this script is to delineate individual storm events and prepare data for storm analysis such as HI/FI
# Step 1: Load in processed SUNA and EXO data from DoD_XXXX script  
# Step 2: fill gaps in nitrate, fDOM, SpCond, and turbidity data
# Step 3: Define baseflow in each catchment.
# Step 4: Set criteria for storm delineation for each catchment based on some percentage over baseflow (more than 5mm in 24 hour period with a response in Q and chem
# Step 5: Delineate storms in each catchment.
# Step 6: IN PYTHON: convert R discharge df to pandas df containing a datetime column named 'valuedatetime', and discharge values in a column 'datavalue'
# Step 7: IN PYTHON: convert R response df(s) to pandas df(s) containing a datetime column named 'valuedatetime', and response values in a column 'datavalue'
