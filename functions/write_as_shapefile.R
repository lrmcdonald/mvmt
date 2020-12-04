
write_as_shapefile = function(fn, newdir){
     get_mid = function(fn){
          parts = str_split(fn, "MVC")[[1]][2]
          parts = str_split(parts, ".c")[[1]][1]
          mid = paste0("MVC", parts)
          return(mid)
     }
     st_drop_geometry <- function(x) {
          if(inherits(x,"sf")) {
               x <- st_set_geometry(x, NULL)
               class(x) <- 'data.frame'
          }
          return(x)
     }
     # Get mid
     MID = get_mid(fn)
     # Read and remove NAs from coordinate data
     df = read.csv(fn) %>% filter(!is.na(longitude)) %>% filter(!is.na(latitude))
     # Convert and Save
     if(nrow(df) > 1){
          # Convert to simple features friendly coordinates ####
          sf_conversion = function(df){
               # get x and y values from simple features geometry
               getxy = function(geom, wh = "x"){
                    xy = st_coordinates(geom)
                    if(wh == "x"){
                         out = xy[,1]
                    }
                    if(wh == "y"){
                         out = xy[,2]
                    }
                    return(out)
               }
               # projections
               wgs = st_crs("EPSG:4326")
               aea = st_crs("ESRI:102006")
               # convert df to sf; get wgs coords, convert to aea, get aea coords 
               df = st_as_sf(df, coords = c("longitude", "latitude"), crs = wgs)%>%
                    mutate(x_wgs = getxy(geometry), y_wgs = getxy(geometry, "y")) %>%
                    mutate(wgs = "EPSG:4326") %>%
                    st_transform(crs = aea) %>%
                    mutate(x_aea = getxy(geometry), y_aea = getxy(geometry, "y")) %>%
                    mutate(aea = "ESRI:102006")
               return(df)
          } 
          df = sf_conversion(df)
          df = df %>% select(mid, cid, dateAK, no, x_wgs, y_wgs, x_aea, y_aea,
                             deploy, event, age, sex, model, mort, dop, perr, temp, dateGMT)
          df = st_drop_geometry(df)
          fn2 = paste0(newdir, MID, ".csv")
          write.csv(df, file = fn2, row.names = F)
          print(paste0(fn, " written."))
     }
     if(nrow(df) < 1){
          print(paste0(MID, " skipped; has no rows"))
     }
}