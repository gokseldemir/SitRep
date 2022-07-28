# First Nations Health Authority's Monthly Community Situation Report on Toxic Drug Overdose  

This project generates a monthly "Community Situation Reports" regarding toxic drug overdose cases and deaths. 

Find one of the reports in the following link; https://www.fnha.ca/Documents/FNHA-Toxic-Drug-Poisonings-Community-Situation-Report-January-2022.pdf

This project uses R Markdown to generate a word document that includes various toxic drug overdose related information, graphs etc.

To produce this reports, one simply needs to 'knit' 02_report_draft_2, and the script will call other scripts and generate a word document. It is the same document in the above link.

In sum, this project;

1) connects to database
2) pulls up overdose data from BC Ministry of Health, BC Centre for Disease Control, BC census data and First Nations client file, to identify First Nations and others who had an overdose in a given time frame.
3) prepares data for a descriptive analysis 
4) generates texts and visualization for different sub-topics such as overdose deaths by health region or spatial distribution of paramedic attended overdose events