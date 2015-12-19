# netprintlocations.R
# Noah Rubin
# December 19, 2015

# change this when using on local machine
filepath = "~/Documents/CODI/map-data/netprintlocations"
setwd(filepath)

# import needed libraries
library(rvest, quietly = T)
library(purrr, quietly = T)
library(ggmap, quietly = T)
# library(stringr)

netprint = read.csv("netprints.csv")

# netprintURL = "https://net-print.cit.cornell.edu/netprintx-cgi/qfeatures.cgi"
# html = read_html(netprintURL)
# 
# rows = html %>%
#     html_nodes("tr")
# 
# cells = rows %>%
#     map(function(row) html_nodes(row, "td") %>%
#             flatmap(html_text))
# 
# cellframe = data.frame(
#     Queue_Name = c(),
#     Printer_Name = c(),
#     Printer_Model = c(),
#     Color = c(),
#     DPI = c(),
#     Duplex = c(),
#     stringsAsFactors = F
# )
# 
# for (i in 2:(length(cells))) {
#     cellframe = rbind(cellframe, unlist(cells[i]))
# }