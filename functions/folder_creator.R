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