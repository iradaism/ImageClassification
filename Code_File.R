library("sp")
library("raster")
library("sf")

AOI <- shapefile("C:/WORK_FOLDER/Teaching/GIS_for_env_applications/Land_Cover_classification/AOI.shp")


B2 <- raster("T32UPU_20210814T102031_B02_10m_extent.grd") 
B3 <- raster("T32UPU_20210814T102031_B03_10m_extent.grd") 
B4 <- raster("T32UPU_20210814T102031_B04_10m_extent.grd")
B5 <- raster("T32UPU_20210814T102031_B05_20m_extent.grd")
B6 <- raster("T32UPU_20210814T102031_B06_20m_extent.grd")
B7 <- raster("T32UPU_20210814T102031_B07_20m_extent.grd")
B8 <- raster("T32UPU_20210814T102031_B08_10m_extent.grd")
B8a <- raster("T32UPU_20210814T102031_B8A_20m_extent.grd")
B11 <- raster("T32UPU_20210814T102031_B11_20m_extent.grd")
B12 <- raster("T32UPU_20210814T102031_B12_20m_extent.grd")


rasterfiles <- list.files(path = path, pattern = ".jp2")

patternmatching <- gsub("\\.jp2$", "_clipped.jp2", rasterfiles)

for (i in 1:length(rasterfiles)){
  print(i)
  gc()  
  r <- raster(rasterfiles[i])
  clip <- raster::crop(r, AOI, filename = patternmatching[i], 
                       overwrite=TRUE)                        
}

clipped <- list.files(path = path, pattern = "_clipped.grd")

patternmatching2 <- gsub("\\_clipped.grd$", "_reproj.tif$", clipped)

newproj<- '+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs'

for (i in 1:length(clipped)){
  print(i)
  gc()
  b <- raster(clipped[i])
  prj <- projectRaster(b, crs=newproj, method = 'bilinear',res=10,  
                       filename = patternmatching2[i], overwrite=TRUE)
}

files <- list.files(path = path, pattern = "_reproj.grd")
patternmatching3 <- gsub("\\_reproj.grd$", "_extent.tif$", files)

example <- raster("T32UPU_20210814T102031_B02_10m_reproj.grd")

newextent <- extent(example)

for (i in 1:length(files)){
  print(i)
  gc()
  b <- raster(files[i])
  x <- setExtent(b, newextent, keepres=TRUE)
  writeRaster(x, filename = patternmatching3[i], 	overwrite= TRUE)
}


file2 <- list.files(path = path, pattern = "_extent.grd")
all_bands <- stack(file2)

names(all_bands) <- c("B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A", "B11", "B12")
plot(all_bands)


NDVI <- (B8 - B4) / (B8 + B4)

NDBI <- (B6 - B5) / (B6 + B5)

NDWI <- (B3 - B8) / (B3 + B8)


Rasstack <- stack(B2, B3, B4, B5, B6, B7, B8, B8a, B11, B12, NDVI, NDBI, NDWI)

names(Rasstack) <- c("B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A", "B11", "B12", "NDVI", "NDBI", "NDWI")

kml <- read_sf("TrainingData.kml")

kml_prj <- st_transform(kml, crs = crs(AOI))

training.Data <- extract(Rasstack, kml_prj, df = TRUE)

data <- cbind(training.Data, kml_prj$Name)

sum(is.na(data))
data[is.na(data)] <- 0


levels(as.factor(kml_prj$Name))

for (i in 1:length(unique(kml_prj$Name))) {
  cat(paste0(i, " ", levels(as.factor(kml_prj$Name))[i]), sep="\n")
  
}

names(data)

training.Data$cl <- as.factor(kml_prj$Name[match(training.Data$ID, seq(nrow(kml_prj)))])

training.Data <- training.Data[-1]

summary(training.Data$cl)
str(training.Data)


sp <- aggregate( . ~ cl, data = training.Data, FUN = mean, na.rm = TRUE )

plot(0,
     ylim = c(min(sp[2:ncol(sp)]), max(sp[2:ncol(sp)])), 
     xlim = c(1, ncol(training.Data)-1), 
     type = 'n', 
     xlab = "L8 bands", 
     ylab = "reflectance [% * 100]"
)


mycolors <- c("#fbf793", "#006601", "#bfe578", "#d00000", "#fa6700")

for (i in 1:nrow(sp)){
  lines(as.numeric(sp[i, -1]), 
        lwd = 4, 
        col = mycolors[i]
  )
}

grid()

legend(as.character(sp$cl),
       x = "topleft",
       col = mycolors,
       lwd = 5,
       bty = "n"
)



library("randomForest")

smp.size <- rep(min(summary(training.Data$cl)), nlevels(training.Data$cl))
smp.size

training.Data[is.na(training.Data)] <- 0

rfmodel <- tuneRF(x = training.Data[-ncol(training.Data)],
                  y = training.Data$cl,
                  sampsize = smp.size,
                  strata = training.Data$cl,
                  ntree = 250,
                  importance = TRUE,
                  doBest = TRUE
)

rfmodel

varImpPlot(rfmodel)
plot(rfmodel)


result <- predict(Rasstack,
                  rfmodel,
                  filename = "classification.tif",
                  overwrite = TRUE
)


plot(result, 
     axes = FALSE, 
     box = FALSE,
     col = c("#ffff66",  "#ff9933",  "#00994c", "#404040", "#9999ff"),
     #legend=c("Buildup", "CropFields", "GreenAreas", "Roads", "Water")
)