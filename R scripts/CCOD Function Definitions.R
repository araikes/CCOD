#### Header Information ####

#### Function Definitions ####

#### Redcap Import ####
redcapImport <- function(uri, key) {
  # Special thanks Will Gray for the redcapExport function and JoAnn Rudd
  # Alvarez for the code for this function. This code was available publicly
  # online at
  # http://biostat.mc.vanderbilt.edu/wiki/pub/Main/JoAnnAlvarez/api.pdf.
  require(RCurl)
  
  eval(expr = parse(text =
                      getURL("https://raw.githubusercontent.com/graywh/redcap/master/redcapExport.R")))
  chartAbstraction <- redcapExport(APIKEY = key,
                                   URI = uri,
                                   labels = TRUE,
                                   forms = NULL)
}