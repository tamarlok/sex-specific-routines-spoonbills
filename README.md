## Authors - 	Tamar Lok, M. van der Geest, P. de Goeij, E. Rakhimberdiev, T. Piersma
## Paper - 	Sex-specific nest attendance rhythm and foraging habitat use in a colony-breeding waterbird
## Institutes - NIOZ Royal Netherlands Institute for Sea Research and University of Groningen
## Date - 	2024-03-08

------------------------------------------------------------------------------------------------------------ 

General
=======
This readme.md file briefly describes the contents of the datapackage required to process, 
analyse and plot the data that produce the results reported in the paper.  

- - -

Dataset Attribution and Usage
-----------------------------

* Dataset Title: Data for the article "Sex-specific nest attendance rhythm and foraging habitat use in a colony-breeding waterbird"

* Persistent Identifier: https://doi.org/xxxx/zenodo.xxxx

* Dataset Contributors: 
	
	* Creators: Tamar Lok, M. van der Geest, P. de Goeij, E. Rakhimberdiev, T. Piersma

* Date of Issue: 2024-03-08

- - -

Methodological Information
==========================

* Methods of data collection: see article for details
* Methods of data processing: see article for details and R-scripts in the folder '1.Data_processing'. These scripts import the raw tracking (GPS and accelerometer) data from Movebank and process the data for analysis.

- - -

Project Structure
===================

.
├── README.md
├── data				<- Data used in the analyses
├── master.R			<- Code that calls the different scripts for data preparation, analyses and visualisation
├── functions.R		<- Code for the functions used in the different scripts
├── 1_data_preparation <- Scripts to import and process data
├── 2_analyses         <- Scripts to reproduce analyses 
└── 3_visualisation    <- Scripts for visualisation of results

  

