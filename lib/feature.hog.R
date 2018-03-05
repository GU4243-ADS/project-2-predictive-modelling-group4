feature.hog <- function(img_dir,data_name, set_name,export=T,cell,orientation){
  
  #########read image#########
  n_files <- length(list.files(img_dir))
  df<-list()
  for (i in 1:n_files){
    n<-nchar(as.character(i))
    path<-paste0(img_dir,"/img_",paste(rep(0,4-n),collapse=""),i,".jpg")
    df[[i]]<-readImage(path)
  }
  
  #########extract hog feature#######
  hog <- vector()
  for (i in 1:n_files){
    hog <- rbind(hog,HOG(df[[i]],cells=cell,orientations = orientation))
  }
  
  
  ### output constructed features
  if(export){
    #save(hog, file=paste0("../output/hog_feature_",cell,orientation,"_", data_name, "_", set_name, ".RData"))
    #write.csv(hog,file=paste0("../output/hog_feature_",cell,orientation,"_", data_name, "_", set_name, ".csv"))
    write.table(hog,file=paste0("../output/hog_feature_",cell,orientation,"_", data_name, "_", set_name, ".csv"),
                row.names = F,col.names = F,sep=",")
  }
  
  return(hog)
}