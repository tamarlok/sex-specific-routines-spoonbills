Reference Information
=====================

* Authors: 			Tamar Lok, M. van der Geest, P. de Goeij, E. Rakhimberdiev, T. Piersma
* Datapackage Title: 		Data for the article "Sex-specific nest attendance rhythm and foraging habitat use in a colony-breeding waterbird"
* Institutes:			NIOZ Royal Netherlands Institute for Sea Research and University of Groningen
* Study area:			Schiermonnikoog (53°29’N, 6°15’E) and surrounding foraging grounds, The Netherlands
* Study period:			May 2012 - September 2019
* Persistent Identifier:	https://doi.org/10.5281/zenodo.10803827
* Date of Issue:		2024-03-08

This readme.md file briefly describes the contents of the datapackage required to process, 
analyse and plot the data that produce the results reported in the paper.  

- - -

Methodological Information
==========================

* Methods of data collection: see article for details
* Methods of data processing: see article for details and R-scripts in the folder '1_data_preparation'. These scripts import the raw tracking (GPS and accelerometer) data from Movebank and process the data for analysis.

- - -

Project Structure
===================

```
├── README.md
├── data		<- Data used in the analyses
├── master.R		<- Code that calls the different scripts for data preparation, analyses and visualisation
├── functions.R		<- Code for the functions used in the different scripts
├── 1_data_preparation	<- Scripts to import and process data
├── 2_analyses		<- Scripts to reproduce analyses 
└── 3_visualisation	<- Scripts for visualisation of results
```
  
- - -

Data files
==========
The raw tracking data generated and analyzed in this study are stored at Movebank (www.movebank.org) in the study “SPOONBILL_METAWAD - Eurasian Spoonbills (Platalea leucorodia, Threskiornithidae) breeding on Schiermonnikoog, The Netherlands” (Movebank study ID 2596955604) and will be made available upon reasonable request.
Other datafiles as well as the processed tracking data required to run the scripts in the folders '2_analyses' and '3_visualisation' are available in the folder 'data'. 

Details for: bird.data.csv
--------------------------
* Description: a comma-delimited file containing the information of the birds and their tracking devices.

* Format: .csv

* Size: 4 kB

* Variables:
	* tagID: unique identifier of tracking device
	* birdID: unique identifier of bird
	* ringnumber: unique metal ring number attached to the tibia of the bird. If NA, no metal ring was used
	* colourcode: unique combination of coloured and/or inscription rings attached to the tibia of the bird
	* sex: M=male, F=female
	* start_deployment: date and time that the bird received its tracking device
	* end_deployment: date and time that the tracking device was removed from the bird. When 31/12/2099 is written, the device was never removed.
	* date_death: date that the bird died (assessed from change in accelerometer data signal)
	* bodymass: mass (g) measured during deployment
	* headbill: combined length (mm) of head and bill
	* head: length (mm) of head
	* P8: length (mm) of 8th primary feather
	* tarsus: length (mm) of tarsus
	* tracker_mass: mass (g) of tracking device

Details for: breeding.data.csv
------------------------------
* Description: a comma-delimited file containing the breeding timing and success per bird per year.

* Format: .csv

* Size: 3 kB

* Variables:
	* year: year of breeding
	* birdID: unique identifier of bird
	* nestID: unique identifier of nest as entered in the spoonbill monitoring database of the Werkgroep Lepelaar (from 2016 onward)
	* breeding_attempt: 0=bird did not breed; 1=bird did breed
	* hatchday: day of the year that the first chick of the first breeding attempt hatched
	* hatchday2: day of the year that the first chick of the second breeding attempt hatched. In most cases, there was only one breeding attempt and no information is filled in here
	* used: 0=not used for analysis because of unreliable data, no breeding attempt or no information on breeding attempt because the bird did not breed in the study area; 1=used for analyses.

Details for: bs_data_tagged_untagged.csv
----------------------------------------
* Description: a comma-delimited file containing the breeding success of tagged and untagged birds as monitored by the Werkgroep Lepelaar (data accessed from the spoonbill monitoring database of the Werkgroep Lepelaar). 

* Format: .csv

* Size: 9 kB

* Variables:
	* year: year of breeding
	* nestID: unique identifier of nest as entered in the spoonbill monitoring database of the Werkgroep Lepelaar
	* tagged: no=not tagged, yes=tagged
	* tagID: unique identifier of tracking device
	* BS: number of chicks that survived until the age of colour-ringing (on average 25 days)

Details for: ch_data_tagged_untagged_adults.csv
-----------------------------------------------
* Description: a comma-delimited file containing the breeding success of tagged and untagged birds as monitored by the Werkgroep Lepelaar (data accessed from the spoonbill monitoring database of the Werkgroep Lepelaar). 

* Format: .csv

* Size: 6 kB

* Variables:
	* birdID: unique identifier of bird
	* ch: binary encounter history running from 2012 until 2020. 0=bird is not seen between March and October in The Netherlands; 1=bird is seen between March and Octoberin The Netherlands.
	* tag: yes=tag, no=no tag

Details for: gps.breeding.data.behav.csv
---------------------------------------- 
* Description: a comma-delimited file containing the processed tracking data of 9 adult females and 13 adult male spoonbills in the study area.

* Format: .csv

* Size: 24,586 kB

* Variables:
	* birdID: unique identifier of bird
	* year: year of breeding
	* date_time_CEST: date and time in CEST (Europe/Amsterdam summertime)	
	* longitude: longitude (decimal degrees, WGS84)
	* latitude: latitude (decimal degrees, WGS84)
	* habitat: habitat type based on the habitats as described under the datafolder 'study_area_shapefile' containing the shapefile of the study area. Some habitat types consist of a combination of habitats as defined in the shapefile: 
		* waddenzee=Wadgeulen_Diep+Wadgeulen_Ondiep+Wadplaten+Wad_Kweldergeul_Brak (Wadden Sea)
		* Schier_brak=Schier_Kweldergeul_Brak+Schier_Brak_Rest (brackish water on Schiermonnikoog)
		* wal_rest_zoet=Wal_Zoet_Ondiep+Wal_Zoet_Diep (freshwater on mainland except Lauwersmeer)
		* wal_rest_land=Wal_Kwelder+Wal_Land_Rest (land on mainland expect Lauwersmeer)
		* LM_land=LG_Land_Rest+LG_Moeras (land within Lauwersmeer)
		* LM_zoet=LG_Zoet_Ondiep_LG_Zoet_Diep (freshwater within Lauwersmeer)
	* breeding.phase: 1.pre-breeding=pre-breeding phase, 3.eggs=egg incubation phase, 4.chicks=chick-rearing phase, 5.post.breeding.successful=post-breeding phase with at least one chick surviving until colour-ringing, 6.post.breeding.unsuccessful=post-breeding phase after unsuccessful breeding attempt
	* sex: M=male, F=female
	* behaviour: behaviour as classified using the random forest model developed in Lok et al. 2023. In the current analysis, we distinguished foraging (searching, handling or ingesting), resting (sitting or standing), flying (flapping or soaring) or other (walking or drinking). 
	* lat.nest = latitude of the nest (decimal degrees, WGS84)
	* lon.nest = longitude of the nest (decimal degrees, WGS84)
	* distance.to.nest = distance (m) from the current location of the bird to the nest
	* diel_rad = time of the day, standardized to radians (0=2π=midnight, π=noon)  
	* tidal_stage_rad = phase within the tidal cycle, standardized to radians (0=2π=high tide, π=low tide)
	
Details for: tide_data_schiermonnikoog_2010-2019.txt
---------------------------------------- 
* Description: a comma-delimited file containing the dates and times of low and high tide at Schiermonnikoog in the years 2010-2019. 

* Format: .txt

* Size: 469 kB

* Variables:
	* date: date in CET (Europe/Amsterdam wintertime)
	* time: time in CET (Europe/Amsterdam wintertime)
	* lowhigh: 1=high tide, 2=low tide
	* waterheight: waterheight (cm) relative to NAP

Details for: map_NL_greyscale.png
---------------------------------
* Description: graphic showing the contours of The Netherlands in grey, as used in Figure 1 of the paper.

* Format: .png

* Size: 469 kB

Details for: rf.model.RData
----------------------------
* Description: random forest model created with the code from Lok et al. 2023 to classify behaviour on the basis of accelerometer data, using a set of summary statistics calculated over segments of 0.4 s.

* Format: .RData

* Size: 24,137 kB

Details for: study_area_shapefile
---------------------------------
* Description: files associated with the habitat map of the study area. Habitat types within the study area have been assigned on the basis of the BRT TOP10NL topographic map from 2011 (resolution 1:20.000; https://www.kadaster.nl/zakelijk/producten/geo-informatie/topnl) using expert knowledge and by changing lines into polygons for small waterbodies (e.g., ditches, small gullies).

* Formats: .dbf, .prj, .shp, .shx

* Sizes: 14,840 kB, 1 kB, 41,724 kB, 695 kB

* Habitats distinguished within seperately distinguished geographical areas:
	* Lauwersmeer
		* LG_Land_Rest: land 
		* LG_Moeras: marshes
		* LG_Zoet_Diep: deep freshwater
		* LG_Zoet_Ondiep: shallow freshwater
	* Schiermonnikoog
		* Schier_Kwelder: saltmarsh
		* Schier_Land_Rest: land other than saltmarsh
		* Schier_Kweldergeul_Brak: saltmarsh gullies
		* Schier_Brak_Rest: brackish water other than saltmarsh gullies
		* Schier_Zoet: freshwater
		* Schier_Restgroep: other habitats within Schiermonnikoog than the above defined
	* Vasteland: mainland
		* Wal_Kwelder: saltmarsh
		* Wal_Kweldergeul_Brak: saltmarsh gullies
		* Wal_Land_Rest: land other than saltmarsh		
		* Wal_Moeras: marsh
		* Wal_Zoet_Diep: deep freshwater 
		* Wal_Zoet_Ondiep: shallow freshwater
	* Zee: sea
		* Eilanden_Rest: other islands
		* Noordzee: North Sea
		* Wadgeulen_Diep: deep gullies in the Wadden Sea
		* Wadgeulen_Ondiep: shallow gullies in the Wadden Sea
		* Wadplaten: intertidal flats (exposed around low tide only)

- - -

References
==========

Lok T, van der Geest M, Bom RA, de Goeij P, Piersma T, Bouten W, 2023. Prey ingestion rates revealed by back-mounted accelerometers in Eurasian spoonbills. Animal Biotelemetry 11:5. https://doi.org/10.1186/s40317-022-00315-w. 

Lok T, van der Geest M, de Goeij P, Rakhimberdiev E, Piersma T, 2024. Sex-specific nest attendance rhythm and foraging habitat use in a colony-breeding waterbird. Behavioral Ecology 35:arae020. https://doi.org/10.1093/beheco/arae020. 