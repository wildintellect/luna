# If you know the item you want, try to get it from Google, then AWS, then the suggested source by the search results
#
# Documentation of GCS and AWS structures
# https://krstn.eu/landsat-batch-download-from-google/
# https://docs.opendata.aws/landsat-pds/readme.html
#
# Example scene ID for Landsat LC81690562019135LGN00
# LC8 - Landsat8
# 169 Path
# 056 Row
# 2019135

split_landsat <- function(scene){
  scene <- "LC08_L1TP_167056_20190517_20190521_01_T1"
  scene_parts <- unlist(strsplit(scene, "_"))
  path <- substr(scene_parts[3],1,3)
  row <- substr(scene_parts[3],4,6)
  sensor <- scene_parts[1]
  collection <- scene_parts[6] # Not sure if this is the folder part
  
  return(c(collection=collection,path=path,row=row,sensor=sensor))
}

find_google <- function(scene){
  # Construct the google url to a file in the public bucket
  
  # Does this really require gsutil? No
  # However this does appear to require google authentication
  # Example url
  # verify <- https://storage.cloud.google.com/gcp-public-data-landsat/LC08/01/167/056/LC08_L1TP_167056_20190517_20190521_01_T1/LC08_L1TP_167056_20190517_20190521_01_T1_B1.TIF
  
  scene = "LC08_L1TP_167056_20190517_20190521_01_T1"
  root_url <- "https://storage.cloud.google.com/gcp-public-data-landsat"
  url_parts <- split_landsat(scene)
  
  bands <- paste0("_B",seq(1:11),".TIF")
  
  # Generate urls for all the bands not knowing which ones to get yet.
  # TODO: what about metadata files?
  final_urls <- file.path(root_url, 
                          url_parts["sensor"], 
                          url_parts["collection"], 
                          url_parts["path"], 
                          url_parts["row"], 
                          scene, 
                          paste0(scene, bands)
                          ) 

  return(final_urls)
}

find_aws <- function(scene = "LC08_L1TP_167056_20190517_20190521_01_T1"){
  # https://landsatonaws.com/L8/168/060/LC08_L1TP_168060_20190625_20190705_01_T1
  # http://landsat-pds.s3.amazonaws.com/c1/L8/168/060/LC08_L1TP_168060_20190625_20190705_01_T1/LC08_L1TP_168060_20190625_20190705_01_T1_B1.TIF
  
  root_url <- "landsat-pds.s3.amazonaws.com"
  scene = "LC08_L1TP_167056_20190517_20190521_01_T1"
  url_parts <- split_landsat(scene)
    
  bands <- paste0("_B",seq(1:11),".TIF")

  final_urls <- file.path(root_url,
                          "c1",
                          "L8", 
                          url_parts["path"], 
                          url_parts["row"], 
                          scene, 
                          paste0(scene, bands)
  )
}

# 'https://earthexplorer.usgs.gov/download/external/options/LANDSAT_8_C1/LC81690562019135LGN00/INVSVC/'
#url <- 'https://earthexplorer.usgs.gov/download/12864/LC81690562019135LGN00/STANDARD/INVSVC'
#outpath <- "LC81690562019135LGN00.tif"
#usgs <- httr::GET(url, httr::authenticate(USERNAME, PASSWORD), progress(), httr::write_disk(outpath))
# https://dds.cr.usgs.gov/ltaauth/hsm/lsat1/collection01/oli_tirs/T1/2019/167/056/LC08_L1TP_167056_20190517_20190521_01_T1.tar.gz?id=h9vm02tm0erbaij7ctlmkorali&iid=LC81670562019137LGN00&did=515840583&ver=production