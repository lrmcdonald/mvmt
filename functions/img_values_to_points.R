# get values of image and extract to sf points data.frame
img_values_to_points = function(points, img, res, bands, id_column){
        # convert sf points to ee features
        feat = st_transform(points, "EPSG:4326") %>% sf_as_ee()
        #  select bands from image, 
        # sample regions - values pulled from image will be what intersects with the feature (feat)
        #  do not keep geometries, unless you want pixel centriods
        imgvals_to_feat = img$select(bands)$sampleRegions(
                collection = feat,
                properties = list(id_column),
                scale = res,
                geometries = FALSE
                )
        # convert to sf - saves to temporary file unless otherwise specified
        imgvals = ee_as_sf(imgvals_to_feat, via = 'getInfo', 
                              overwrite = T, "./tempfiles/temp.csv") %>% 
                st_drop_geometry()
        # join imgvals to original points
        points = left_join(points, imgvals, by = id_column)
        return(points)
}