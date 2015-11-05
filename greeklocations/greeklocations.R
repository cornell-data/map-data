# Cornell University Greek Locations Data Extraction
# Noah Rubin
# November 3, 2015

# change this when using on local machine
filepath = "~/Documents/CODI//greeklocations";
setwd(filepath);

# import needed libraries
library(xml2);
library(rvest);
library(purrr);
# library(ggplot2);
# library(ggmap);
library(stringr);

# Set URL and get html from URL
greekURL = "http://dos.cornell.edu/fraternity-sorority-life/greek-system-cornell/chapters";
html = read_html(greekURL);

# Get link nodes, refs, and text (greek names)
linkNodes = html %>%
  html_nodes("a");

links = linkNodes %>%
  html_attr("href");
links = unlist(links);

labels = linkNodes %>%
  html_text() %>%
  as.character;
labels = unlist(labels);

# combine labels and links into dataframe for analysis and manipulation
greeklocations = data.frame(cbind(labels = labels, links = links));
greeklocations = greeklocations[50:113,];

# fix few links that have http://https:// issue
greeklocations$links = gsub("http://", "", greeklocations$links);

# create function to assign categories to greek house
assignCategory = function(house) {
  index = which(greeklocations$labels == house);
  ifc_upper = which(greeklocations$labels == "Zeta Psi");
  multi_upper = which(greeklocations$labels == "Sigma Lambda Upsilon");
  if (index <= ifc_upper) {
    return("Interfraternity Council");
  } else if (index <= multi_upper) {
    return("Multicultural Greek Letter Council");
  } else {
    return("Panhellenic Association");
  }
}

# assign categories to greek houses
# possible categories are: Interfraternity Council,
#                          Multicultural Greek Letter Council,
#                          or Panhellenic Association
greeklocations$category = greeklocations$labels %>%
  map(assignCategory) %>%
  unlist;

# get locations, separate lat and lon
# locales = greeklocations$labels %>%	  |## This method does not work and wastes computing power,
#  paste(., "Cornell University") %>%   |## going to manually enter
#  map(geocode);
locales = greeklocations$labels %>%
  map(function(label) data.frame(lat=42, lon=-76));

latitudes = locales %>%
  map(function(position) position$lat) %>%
  unlist;

longitudes = locales %>%
  map(function(position) position$lon) %>%
  unlist;

# create vectors to correct lat and lons
######### set coords for all multicultural organizations to CU #########
lat_alts = c(.4586036, .4502219, .4540142,
             .4557758, .48300, .4521457,
             .454119, .4558816, .443698,
             .4529883, .4467306, .4454083,
             .4452216, .48300, .4576611,
             .4497605, .4435775, .4549374,
             .4514881, .4459575, .4535282,
             .4494606, .4444925, .4554078,
             .4464173, .4532908, .4553728,
             .4566836, .4542451, .4516039,
             .4459458, .4501199, .4497427,
             .4500504, .4540839, 4540936,
             .48300, .48300, .48300,
             .48300, .48300, .48300,
             .48300, .48300, .48300,
             .48300, .48300, .48300,
             .48300, .48300, .48300,
             .4571348, .4564305, .453293,
             .4553835, .4553791, .4554264,
             .4445631, .454349, .453918,
             .4488836, .445036, .4581989,
             .45622);

lon_alts = c(.489127, .4917276, .4900639,
             .4877388, .44702, .4930897,
             .4894237, .4916989, .490971,
             .4909266, .4940093, .491215,
             .4892434, .44702, .4889061,
             .4935841, .4899733, .4919368,
             .4914453, .4916144, .4911802,
             .4922256, .4905024, .4906627,
             .4909314, .4876203, .4818937,
             .4933248, .483571, .4516039,
             .4906946, .4909934, .4902801,
             .4898899, .4947196, .4840855,
             .44702, .44702, .44702,
             .44702, .44702, .44702,
             .44702, .44702, .44702,
             .44702, .44702, .44702,
             .44702, .44702, .44702,
             .4891294, .487534, .4860807,
             .4917488, .4835657, .4827318,
             .4910697, .4811736, .4853347,
             .4903225, .4900327, .4847997,
             .4905346);

# fix latitude and longitudes, add to greeklocations
# latitudes = round(latitudes, 0) + lat_alts;
# longitudes = round(longitudes, 0) - lon_alts;
latitudes = latitudes + lat_alts;
longitudes = longitudes - lon_alts;
greeklocations$Latitude = latitudes;
greeklocations$Longitude = longitudes;

# get other information for each house
chapterPages = greeklocations$links %>%
  map(read_html);

responseNodes = chapterPages %>%
  map(html_nodes, ".response");

responses = responseNodes %>%
  map(html_text) %>%
  map(function(responses) {
    responses = unlist(responses) %>%
      map(function(item) gsub("[[:space:]]", "", item)) %>%
      map(function(item) gsub("([[:lower:]]|:|Email)([[:upper:]]|http)", "\\1 \\2", item));
    return(unlist(responses));
  });

getInfo = function(responseSet, pattern) {
  responseSet = unlist(responseSet);
  index = grep(pattern, responseSet);
  if (length(index) >= 1) {
    return(
      str_trim(
        unlist(
          str_split(responseSet[index], pattern)
        )[2]
      )
    );
  }
  return("");
}

category = responses %>%
  map(getInfo, "Category") %>%
  unlist;

website = responses %>%
  map(getInfo, "Website|website") %>%
  unlist;

council = responses %>%
  map(getInfo, "Council") %>%
  unlist;

presidentName = responses %>%
  map(getInfo, "President: Name") %>%
  unlist;

presidentEmail = responses %>%
  map(getInfo, "President: Email") %>%
  unlist;

socialName = responses %>%
  map(getInfo, "Social Chair: Name") %>%
  unlist;

socialEmail = responses %>%
  map(getInfo, "Social Chair: Email") %>%
  unlist;

advisorName = responses %>%
  map(getInfo, "Advisor Name|Advisor: Name") %>%
  unlist;

advisorEmail = responses %>%
  map(getInfo, "Advisor Email|Advisor: Email") %>%
  unlist;

chapterInformation = data.frame(cbind(labels = greeklocations$labels,
                                      category = category,
                                      website = website,
                                      council = council,
                                      pres_name = presidentName,
                                      pres_email = presidentEmail,
                                      social_name = socialName,
                                      social_email = socialEmail,
                                      advisor_name = advisorName,
                                      advisor_email = advisorEmail
                                      )
                                );

# save memory, remove un-needed variables (html stuff)
rm(advisorEmail, advisorName, category, council, html, labels,
   lat_alts, latitudes, linkNodes, links, locales, lon_alts, longitudes,
   presidentEmail, presidentName, responseNodes, socialEmail, socialName, website);

# write files to working directory
write.csv(greeklocations, file="greeklocations.csv");
write.csv(chapterInformation, file="chapterinformation.csv");
