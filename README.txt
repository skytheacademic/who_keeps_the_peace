R Scripts
	- clean_data.R contains all code to clean and prepare data for analysis
	- match_data.R contains all code to match data for main paper, as well as code for matching loveplot and matching for cutoff robustness in Appendix. 
	- analyze_data.R contains all code to run all models in main paper, tables and figures, and for all tables in appendix.
	- plot_data.R contains all code for non-model figures, as well as code for job talk figures

UCDP data
	- Contains violence data from the Uppsala Conflict Data Program
	- Observed at event level in areas where more than 5 deaths occured in a given calendar year.
	- Downloaded from https://ucdp.uu.se/downloads/index.html#ged_global on January 6, 2023.
	- "GEDEvent_v22_1.csv" is the original csv file downloaded on violence.
	- "ucdp-actor-221.csv" is the original csv file downloaded on actors classified by type.

PRIO data:
	- Contains geographic and spatial data.
	- Observed at the grid-cell level for static variables, and grid-cell-year level for yearly data.
	- Downloaded from https://grid.prio.org/#/download on June 3, 2022.
	- "PRIO-GRID Static Variables - 2022-06-03.csv" is the original static CSV file
	- "PRIO-GRID Yearly Variables for 1999-2014 - 2022-06-03.csv" is the original yearly CSV file.
	- The "priogrid_cellshp" shapefiles were downloaded from https://grid.prio.org/#/extensions on June 3, 2022 and contain all of the shapefile information.

RADPKO data:
	- Contains data on peacekeeping operation deployments.
	- Observed at the grid-cell-month level.
	- Downloaded from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/BQU5VD (version 4) on June 3, 2022.
	- "radpko_grid.csv" is the original CSV file downloaded.