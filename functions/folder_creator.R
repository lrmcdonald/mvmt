# check if folder exists, if not create it
folder_creator = function(folder){
     status = function(folder, created = "TRUE"){
          if(created){
               print(paste("Folder", folder, "Created"))
          } else{
               print(paste("Folder", folder, "Exists"))
          }
     }
     if(!dir.exists(folder)){ dir.create(folder); status(folder)
     } else{ status(folder, created = FALSE)}
}
# get locations of most recent files in input directory
# folder flow - data / data_type / data_date in YY_MM_DD format
get_output = function(inputDir, outputDir){
     # Set locations of most recent files ####
     df = file.info(list.dirs(inputDir, recursive = F))
     moose_loc_folder = rownames(df)[which.max(df$mtime)]
     # Set location of output files ####
     date_folder = str_split(moose_loc_folder, pattern = "/")
     date_folder = date_folder[[1]][length(date_folder[[1]])]
     newdir = paste(outputDir, date_folder, "/", sep = "")
     folder_creator(newdir)
     return(newdir)
}