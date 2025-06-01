#########Greenhousegas Images ################

setwd("C:/Users/Tobyd/OneDrive - Aarhus universitet/Uni-livet/Kandidatgrad/10. Semester/Specialet/Data/Final data/Images and figures/Matching/Greenhouse gas emitters/")

# install.packages("magick")  # if you don’t already have it
library(magick)

files <- c(
  "Ch4GeoPhase3.png","Ch4GeoPhase4.png",
  "Co2ExclBioGeoPhase3.png","Co2ExclBioGeoPhase4.png",
  "Co2GeoPhase3.png",      "Co2GeoPhase4.png",
  "N2oGeoPhase3.png",      "N2oGeoPhase4.png"
)

FileNames <- c(
  "Ch4 Phase3","Ch4 Phase4",
  "Co2ExclBio Phase3","Co2ExclBio Phase4",
  "Co2 Phase3",      "Co2 Phase4",
  "N2o Phase3",      "N2o Phase4"
)

# 1. Read in all files (returns a magick-image object containing 8 frames)
imgs <- image_read(files)

# 2. Annotate each frame with its figure number
#    vectorized = TRUE ensures you annotate each frame in one shot
imgs <- image_annotate(
  imgs,
  text     = paste0(seq_along(files), " - ", FileNames),
  size     = 50,
  gravity  = "northwest",
  location = "+10+10",
  color    = "black",
)

# 3. Montage into a 4×2 grid
combined <- image_montage(
  imgs,                # now a single magick object
  tile     = "4x2",
  geometry = "+5+5"
)

# 4. Write out as JPEG (avoids the PNG‐delegate issue)
image_write(combined, path = "CombinedGeoGHG.jpg", format = "jpeg")

##Love Plot
files <- c(
  "LovePlotCh4Phase3.png","LovePlotCh4Phase4.png",
  "LovePlotCo2ExclBioPhase3.png","LovePlotCo2ExclBioPhase4.png",
  "LovePlotCo2Phase3.png",      "LovePlotCo2Phase4.png",
  "LovePlotN2oPhase3.png",      "LovePlotN2oPhase4.png"
)

FileNames <- c(
  "Ch4 Phase3","Ch4 Phase4",
  "Co2ExclBio Phase3","Co2ExclBio Phase4",
  "Co2 Phase3",      "Co2 Phase4",
  "N2o Phase3",      "N2o Phase4"
)

# 1. Read in all files (returns a magick-image object containing 8 frames)
imgs <- image_read(files)

# 2. Annotate each frame with its figure number
#    vectorized = TRUE ensures you annotate each frame in one shot
imgs <- image_annotate(
  imgs,
  text     = paste0(seq_along(files), " - ", FileNames),
  size     = 50,
  gravity  = "northwest",
  location = "+5+5",
  color    = "black",
)

# 3. Montage into a 4×2 grid
combined <- image_montage(
  imgs,                # now a single magick object
  tile     = "2x4",
  geometry = "+5+5"
)

# 4. Write out as JPEG (avoids the PNG‐delegate issue)
image_write(combined, path = "CombinedLovePlotGHG.jpg", format = "jpeg")

##Time trends##
files <- c(
  "TimeTrendCh4Phase3.png","TimeTrendCh4Phase4.png",
  "TimeTrendCo2ExclBioPhase3.png","TimeTrendCo2ExclBioPhase4.png",
  "TimeTrendCo2Phase3.png",      "TimeTrendCo2Phase4.png",
  "TimeTrendN2oPhase3.png",      "TimeTrendN2oPhase4.png"
)

# 1. Read in all files (returns a magick-image object containing 8 frames)
imgs <- image_read(files)

# 2. Annotate each frame with its figure number
#    vectorized = TRUE ensures you annotate each frame in one shot
imgs <- image_annotate(
  imgs,
  text     = paste0(seq_along(files)),
  size     = 50,
  gravity  = "northwest",
  location = "+10+10",
  color    = "black",
)

# 3. Montage into a 4×2 grid
combined <- image_montage(
  imgs,                # now a single magick object
  tile     = "2x4",
  geometry = "+5+5"
)

# 4. Write out as JPEG (avoids the PNG‐delegate issue)
image_write(combined, path = "CombinedTimeTrendGHG.jpg", format = "jpeg")



#########Local air and water polluters Images ################

setwd("C:/Users/Tobyd/OneDrive - Aarhus universitet/Uni-livet/Kandidatgrad/10. Semester/Specialet/Data/Final data/Images and figures/Matching/Local air and water polluters/")

# install.packages("magick")  # if you don’t already have it
library(magick)

files <- c(
  "Nickel_AirGeoPhase3.png","Nickel_AirGeoPhase4.png",
  "Nickel_WaterGeoPhase3.png","Nickel_WaterGeoPhase4.png",
  "Zn_AirGeoPhase3.png",      "Zn_AirGeoPhase4.png",
  "Zn_WaterGeoPhase3.png",      "Zn_WaterGeoPhase4.png"
)

FileNames <- c(
  "Nickel_Air Phase3","Nickel_Air Phase4",
  "Nickel_Water Phase3","Nickel_Water Phase4",
  "Zn_Air Phase3",      "Zn_Air Phase4",
  "Zn_Water Phase3",      "Zn_Water Phase4"
)

# 1. Read in all files (returns a magick-image object containing 8 frames)
imgs <- image_read(files)

# 2. Annotate each frame with its figure number
#    vectorized = TRUE ensures you annotate each frame in one shot
imgs <- image_annotate(
  imgs,
  text     = paste0(seq_along(files), " - ", FileNames),
  size     = 50,
  gravity  = "northwest",
  location = "+10+10",
  color    = "black",
)

# 3. Montage into a 4×2 grid
combined <- image_montage(
  imgs,                # now a single magick object
  tile     = "4x2",
  geometry = "+5+5"
)

# 4. Write out as JPEG (avoids the PNG‐delegate issue)
image_write(combined, path = "CombinedGeoLAW.jpg", format = "jpeg")

##Love Plot
files <- c(
  "LovePlotNickel_AirPhase3.png","LovePlotNickel_AirPhase4.png",
  "LovePlotNickel_WaterPhase3.png","LovePlotNickel_WaterPhase4.png",
  "LovePlotZn_AirPhase3.png",      "LovePlotZn_AirPhase4.png",
  "LovePlotZn_WaterPhase3.png",      "LovePlotZn_WaterPhase4.png"
)

FileNames <- c(
  "Nickel_Air Phase3","Nickel_Air Phase4",
  "Nickel_Water Phase3","Nickel_Water Phase4",
  "Zn_Air Phase3",      "Zn_Air Phase4",
  "Zn_Water Phase3",      "Zn_Water Phase4"
)

# 1. Read in all files (returns a magick-image object containing 8 frames)
imgs <- image_read(files)

# 2. Annotate each frame with its figure number
#    vectorized = TRUE ensures you annotate each frame in one shot
imgs <- image_annotate(
  imgs,
  text     = paste0(seq_along(files), " - ", FileNames),
  size     = 50,
  gravity  = "northwest",
  location = "+5+5",
  color    = "black",
)

# 3. Montage into a 4×2 grid
combined <- image_montage(
  imgs,                # now a single magick object
  tile     = "2x4",
  geometry = "+5+5"
)

# 4. Write out as JPEG (avoids the PNG‐delegate issue)
image_write(combined, path = "CombinedLovePlotLAW.jpg", format = "jpeg")

##Time trends##
files <- c(
  "TimeTrendNickel_AirPhase3.png","TimeTrendNickel_AirPhase4.png",
  "TimeTrendNickel_WaterPhase3.png","TimeTrendNickel_WaterPhase4.png",
  "TimeTrendZn_AirPhase3.png",      "TimeTrendZn_AirPhase4.png",
  "TimeTrendZn_WaterPhase3.png",      "TimeTrendZn_WaterPhase4.png"
)

# 1. Read in all files (returns a magick-image object containing 8 frames)
imgs <- image_read(files)

# 2. Annotate each frame with its figure number
#    vectorized = TRUE ensures you annotate each frame in one shot
imgs <- image_annotate(
  imgs,
  text     = paste0(seq_along(files)),
  size     = 50,
  gravity  = "northwest",
  location = "+10+10",
  color    = "black",
)

# 3. Montage into a 4×2 grid
combined <- image_montage(
  imgs,                # now a single magick object
  tile     = "2x4",
  geometry = "+5+5"
)

# 4. Write out as JPEG (avoids the PNG‐delegate issue)
image_write(combined, path = "CombinedTimeTrendLAW.jpg", format = "jpeg")

#########Local air polluters Images ################

setwd("C:/Users/Tobyd/OneDrive - Aarhus universitet/Uni-livet/Kandidatgrad/10. Semester/Specialet/Data/Final data/Images and figures/Matching/Local air polluters/")

# install.packages("magick")  # if you don’t already have it
library(magick)

files <- c(
  "Nh3GeoPhase3.png","Nh3GeoPhase4.png",
  "NoxGeoPhase3.png","NoxGeoPhase4.png",
  "SoxGeoPhase3.png",      "SoxGeoPhase4.png")

FileNames <- c(
  "Nh3 Phase3","Nh3 Phase4",
  "Nox Phase3","Nox Phase4",
  "Sox Phase3",      "Sox Phase4")

# 1. Read in all files (returns a magick-image object containing 8 frames)
imgs <- image_read(files)

# 2. Annotate each frame with its figure number
#    vectorized = TRUE ensures you annotate each frame in one shot
imgs <- image_annotate(
  imgs,
  text     = paste0(seq_along(files), " - ", FileNames),
  size     = 50,
  gravity  = "northwest",
  location = "+10+10",
  color    = "black",
)

# 3. Montage into a 4×2 grid
combined <- image_montage(
  imgs,                # now a single magick object
  tile     = "3x2",
  geometry = "+5+5"
)

# 4. Write out as JPEG (avoids the PNG‐delegate issue)
image_write(combined, path = "CombinedGeoLA.jpg", format = "jpeg")

##Love Plot
files <- c(
  "LovePlotNh3Phase3.png","LovePlotNh3Phase4.png",
  "LovePlotNoxPhase3.png","LovePlotNoxPhase4.png",
  "LovePlotSoxPhase3.png",      "LovePlotSoxPhase4.png")

FileNames <- c(
  "Nh3 Phase3","Nh3 Phase4",
  "Nox Phase3","Nox Phase4",
  "Sox Phase3",      "Sox Phase4")

# 1. Read in all files (returns a magick-image object containing 8 frames)
imgs <- image_read(files)

# 2. Annotate each frame with its figure number
#    vectorized = TRUE ensures you annotate each frame in one shot
imgs <- image_annotate(
  imgs,
  text     = paste0(seq_along(files), " - ", FileNames),
  size     = 50,
  gravity  = "northwest",
  location = "+5+5",
  color    = "black",
)

# 3. Montage into a 4×2 grid
combined <- image_montage(
  imgs,                # now a single magick object
  tile     = "2x3",
  geometry = "+5+5"
)

# 4. Write out as JPEG (avoids the PNG‐delegate issue)
image_write(combined, path = "CombinedLovePlotLA.jpg", format = "jpeg")

##Time trends##
files <- c(
  "TimeTrendNh3Phase3.png","TimeTrendNh3Phase4.png",
  "TimeTrendNoxPhase3.png","TimeTrendNoxPhase4.png",
  "TimeTrendSoxPhase3.png",      "TimeTrendSoxPhase4.png")

# 1. Read in all files (returns a magick-image object containing 8 frames)
imgs <- image_read(files)

# 2. Annotate each frame with its figure number
#    vectorized = TRUE ensures you annotate each frame in one shot
imgs <- image_annotate(
  imgs,
  text     = paste0(seq_along(files)),
  size     = 50,
  gravity  = "northwest",
  location = "+10+10",
  color    = "black",
)

# 3. Montage into a 4×2 grid
combined <- image_montage(
  imgs,                # now a single magick object
  tile     = "2x3",
  geometry = "+5+5"
)

# 4. Write out as JPEG (avoids the PNG‐delegate issue)
image_write(combined, path = "CombinedTimeTrendLA.jpg", format = "jpeg")

#########Local water polluters Images ################

setwd("C:/Users/Tobyd/OneDrive - Aarhus universitet/Uni-livet/Kandidatgrad/10. Semester/Specialet/Data/Final data/Images and figures/Matching/Local water polluters/")

# install.packages("magick")  # if you don’t already have it
library(magick)

files <- c(
  "TNitrogen_waterGeoPhase3.png","TNitrogen_waterGeoPhase4.png",
  "Tph_WaterGeoPhase3.png","Tph_WaterGeoPhase4.png",
  "Toc_WaterGeoPhase3.png",      "Toc_WaterGeoPhase4.png")

FileNames <- c(
  "Total Nitrogen Phase3","Total Nitrogen Phase4",
  "Total Phosphorus Phase3","Total Phosphorus Phase4",
  "TOC Phase3",      "TOC Phase4")

# 1. Read in all files (returns a magick-image object containing 8 frames)
imgs <- image_read(files)

# 2. Annotate each frame with its figure number
#    vectorized = TRUE ensures you annotate each frame in one shot
imgs <- image_annotate(
  imgs,
  text     = paste0(seq_along(files), " - ", FileNames),
  size     = 50,
  gravity  = "northwest",
  location = "+10+10",
  color    = "black",
)

# 3. Montage into a 4×2 grid
combined <- image_montage(
  imgs,                # now a single magick object
  tile     = "3x2",
  geometry = "+5+5"
)

# 4. Write out as JPEG (avoids the PNG‐delegate issue)
image_write(combined, path = "CombinedGeoLW.jpg", format = "jpeg")

##Love Plot
files <- c(
  "LovePlotTNitrogen_waterPhase3.png","LovePlotTNitrogen_waterPhase4.png",
  "LovePlotTph_WaterPhase3.png","LovePlotTph_WaterPhase4.png",
  "LovePlotToc_WaterPhase3.png",      "LovePlotToc_WaterPhase4.png")

FileNames <- c(
  "Total Nitrogen Phase3","Total Nitrogen Phase4",
  "Total Phosphorus Phase3","Total Phosphorus Phase4",
  "TOC Phase3",      "TOC Phase4")

# 1. Read in all files (returns a magick-image object containing 8 frames)
imgs <- image_read(files)

# 2. Annotate each frame with its figure number
#    vectorized = TRUE ensures you annotate each frame in one shot
imgs <- image_annotate(
  imgs,
  text     = paste0(seq_along(files), " - ", FileNames),
  size     = 50,
  gravity  = "northwest",
  location = "+5+5",
  color    = "black",
)

# 3. Montage into a 4×2 grid
combined <- image_montage(
  imgs,                # now a single magick object
  tile     = "2x3",
  geometry = "+5+5"
)

# 4. Write out as JPEG (avoids the PNG‐delegate issue)
image_write(combined, path = "CombinedLovePlotLW.jpg", format = "jpeg")

##Time trends##
files <- c(
  "TimeTrendTNitrogen_waterPhase3.png","TimeTrendTNitrogen_waterPhase4.png",
  "TimeTrendTph_WaterPhase3.png","TimeTrendTph_WaterPhase4.png",
  "TimeTrendToc_WaterPhase3.png",      "TimeTrendToc_WaterPhase4.png")

# 1. Read in all files (returns a magick-image object containing 8 frames)
imgs <- image_read(files)

# 2. Annotate each frame with its figure number
#    vectorized = TRUE ensures you annotate each frame in one shot
imgs <- image_annotate(
  imgs,
  text     = paste0(seq_along(files)),
  size     = 50,
  gravity  = "northwest",
  location = "+10+10",
  color    = "black",
)

# 3. Montage into a 4×2 grid
combined <- image_montage(
  imgs,                # now a single magick object
  tile     = "2x3",
  geometry = "+5+5"
)

# 4. Write out as JPEG (avoids the PNG‐delegate issue)
image_write(combined, path = "CombinedTimeTrendLW.jpg", format = "jpeg")

setwd("C:/Users/Tobyd/OneDrive - Aarhus universitet/Uni-livet/Kandidatgrad/10. Semester/Specialet/Data/Final data/")

#Eventtime
files <- c(
  "Ch4_eventstudy.png","Co2_eventstudy.png",
  "N2o_eventstudy.png","Nh3_eventstudy.png",
  "Sox_eventstudy.png", "Zn_Water_eventstudy.png")

# 1. Read in all files (returns a magick-image object containing 8 frames)
imgs <- image_read(files)

# 2. Annotate each frame with its figure number
#    vectorized = TRUE ensures you annotate each frame in one shot
imgs <- image_annotate(
  imgs,
  text     = paste0(seq_along(files)),
  size     = 50,
  gravity  = "northwest",
  location = "+10+10",
  color    = "black",
)

# 3. Montage into a 4×2 grid
combined <- image_montage(
  imgs,                # now a single magick object
  tile     = "2x3",
  geometry = "+5+5"
)

# 4. Write out as JPEG (avoids the PNG‐delegate issue)
image_write(combined, path = "Eventtime.jpg", format = "jpeg")

#Eventtime 2
files <- c(
  "Co2ExclBio_eventstudy.png","Nickel_Water_eventstudy.png",
  "Nickel_Air_eventstudy.png","Nox_eventstudy.png",
  "TNitrogen_water_eventstudy.png", "Tph_Water_eventstudy.png",
  "Toc_Water_eventstudy.png","Zn_Air_eventstudy.png")

# 1. Read in all files (returns a magick-image object containing 8 frames)
imgs <- image_read(files)

# 2. Annotate each frame with its figure number
#    vectorized = TRUE ensures you annotate each frame in one shot
imgs <- image_annotate(
  imgs,
  text     = paste0(seq_along(files)),
  size     = 50,
  gravity  = "northwest",
  location = "+10+10",
  color    = "black",
)

# 3. Montage into a 4×2 grid
combined <- image_montage(
  imgs,                # now a single magick object
  tile     = "2x4",
  geometry = "+5+5"
)

# 4. Write out as JPEG (avoids the PNG‐delegate issue)
image_write(combined, path = "Eventtime2.jpg", format = "jpeg")
