# https://cran.r-project.org/web/packages/move/vignettes/browseMovebank.html

loginStored <- movebankLogin(username="tamarlok", password="Vlieland12")

# search for study name with keywords:
searchMovebankStudies("poonbill", login=loginStored) # this gives different results than when searching in Movebank itself... Moreover, it is case sensitive. Therefore, remove first letter to see both ways of writing.

spoonbill_metawad_ID <- getMovebankID("SPOONBILL_METAWAD - Eurasian Spoonbills (Platalea leucorodia, Threskiornithidae) breeding on Schiermonnikoog, The Netherlands", login=loginStored)

# download all location data:

all_metawad_data <- getMovebankData(spoonbill_metawad_ID, login=loginStored)

data_736 = getMovebankData(spoonbill_metawad_ID, animalName="736", login=loginStored)
head(data_736)

# make a loop and store the GPS and ACC data in seperate lists:
