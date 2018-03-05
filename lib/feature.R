#############################################################
### Construct visual features for training/testing images ###
#############################################################

### Authors: Yuting Ma/Tian Zheng
### Project 3
### ADS Spring 2017

feature <- function(img_dir, set_name, data_name="data", export=T){
  
  ### Construct process features for training/testing images
  ### Sample simple feature: Extract row average raw pixel values as features
  
  ### Input: a directory that contains images ready for processing
  ### Output: an .RData file contains processed features for the images
  
  ### load libraries
  library("EBImage")
  library(grDevices)
  
  
  ### Define the b=number of R, G and B
  nR <- 10
  nG <- 12
  nB <- 12 
  rBin <- seq(0, 1, length.out=nR)
  gBin <- seq(0, 1, length.out=nG)
  bBin <- seq(0, 1, length.out=nB)
  mat=array()
  freq_rgb=array()
  rgb_feature=matrix(nrow=2000, ncol=nR*nG*nB)
  
  n_files <- length(list.files(img_dir))
  
  
  ### determine img dimensions
  img0 <-  readImage(paste0(img_dir, "pet", 1:2000, ".jpg"))
  mat1 <- as.matrix(img0)
  n_r  <- nrow(img0)
  
  # ### store vectorized pixel values of images
  # dat <- matrix(NA, n_files, n_r) 
  # for(i in 1:n_files){
  #   img     <- readImage(paste0(img_dir,  "pet", i, ".jpg"))
  #   dat[i,] <- rowMeans(img)
  # }
  
  ########extract RGB features############
  for (i in 1:2000){
    mat <- imageData(readImage(paste0(img_dir, "pet", i, ".jpg")))
    mat_as_rgb <-array(c(mat,mat,mat),dim = c(nrow(mat),ncol(mat),3))
    freq_rgb <- as.data.frame(table(factor(findInterval(mat_as_rgb[,,1], rBin), levels=1:nR), 
                                    factor(findInterval(mat_as_rgb[,,2], gBin), levels=1:nG),
                                    factor(findInterval(mat_as_rgb[,,3], bBin), levels=1:nB)))
    rgb_feature[i,] <- as.numeric(freq_rgb$Freq)/(ncol(mat)*nrow(mat)) # normalization
    
    mat_rgb <-mat_as_rgb
    dim(mat_rgb) <- c(nrow(mat_as_rgb)*ncol(mat_as_rgb), 3)
  }
  
  # ### output constructed features -- may not need this? try checking
  # if(export){
  #   save(dat, file = paste0("../output/feature_", data_name, "_", set_name, ".RData"))
  # }
  # return(dat)
  
  ### output constructed features for RGB
  if(export){
    saveRDS(rgb_feature, file = "../output/rgb_feature_new.RData")
  }
  return(data.frame(rgb_feature))
}
