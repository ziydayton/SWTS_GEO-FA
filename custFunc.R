# Helper functions to support GOE_SWTS

#Loctions listed as countries are often regions.  Region mapping does not mix with country mapping.  This function removes regions and returns 'clean' data frame
removeRegion <- function(df, sort.col, val = TRUE){
   #Regions idenitified
   region <- c("Africa (South of Sahara)", "America (North & Central America)", "America (South America)", "Asia (Far East Asia)", "Asia (Middle East Asia)", "Asia (South & Central Asia)", "Europe", "Oceania", "Worldwide")
   
   if(val) {
      df <- df[!(sort.col %in% region), ]
   } else {
      df <- df[sort.col %in% region, ]
   }
   return(df)
}

#Create helper function to get 'Activities' from Foreign Assistance.gov; returns a data frame of activities
getFAActivities <- function(fundingType, agency, year) {
   pt <- proc.time()
   #Check for previous completion of this API webscraping task:
   if(file.exists(paste0("dfActivity_", Sys.Date(), ".csv"))) {
      df <- read.csv(paste0("dfActivity_", Sys.Date(), ".csv"), stringsAsFactors = FALSE)
      print(paste0("TOTAL Elapsed Time: ", (proc.time() - pt)[3]))
      return(df)
   } else {
      
   #set up a blank data frame
   df <- data.frame()
   contentErrorMsg <- "No records found for the specified criteria"
   
   for (fTS in fundingType) {
      for (aS in agency) {
         for (yS in year) {
            
            #set the URL name
            getURL <- "https://fagov-apim-prd.azure-api.us/public/api/public/activities?format=json&fundingType="
            getURL <- paste0(getURL, fTS, "&filterType=agency&filterValue=", 
                             aS, "&year=", yS)
            
            ##GET the info from the URL
            getdf <- GET(getURL)
            # print(paste0("GetDf status = ", getdf[[2]]))
            
            if(getdf[[2]] != 200) {count <- 0} else {count <- 10}
            while (getdf[[2]] != 200 & count <= 9) {
               count <- count + 1
               Sys.sleep(5)
               getdf <- GET(getURL)
               if(getdf[[2]] == 200) {count <- 10}
               # print(paste0("GetDf status = ", getdf[[2]],
               #              "; Count = ", count))
            } #/ end while
            
            #Parse the JSON file
            jsonContent <- content(getdf, as = "text", encoding = "UTF-8")
            #No data found/avail case
            if (jsonContent == contentErrorMsg) {
               parsed <- NULL
               df2 <- data.frame()
            #Data found case   
            } else {
               parsed <- jsonlite::fromJSON(jsonContent, simplifyVector = FALSE)
               
               #set the data frome
               df2 <- as.data.frame(matrix(unlist(parsed),nrow = length(parsed), byrow = TRUE))
               colnames(df2) <- names(parsed[[1]])
               df2$fundingType <- fTS
               df2$amount <- as.numeric(df2$amount)
               df2$year <- as.integer(df2$year)
               
               #Group and summarize data frame
               df2 <- df2 %>%
                  group_by(agencyName, category, year, benefitingLocation, sector, fundingType) %>%
                  summarise(amount = sum(amount), initiatives = n())
               
               #Combine both data frames
               df <- rbind(df, df2)
            }
            
            # print(paste0("Funding Type = ", fTS, "; Agency = ", aS,
            #              "; Year= ", yS, "; Rows Added = ", dim(df2)[1], 
            #       "; Elapsed Time: ", (proc.time() - pt)[3]))
            Sys.sleep(2.5)
         } #end for yS
      } #/ end for aS
      print(paste0("Elapsed Time: ", (proc.time() - pt)[3]))
   } #/end for fTS
   
   print(paste0("TOTAL Elapsed Time: ", (proc.time() - pt)[3]))
   write.csv(df, file = paste0("dfActivity_", Sys.Date(), ".csv"),
             row.names = FALSE)
   return(df)
   }
} #/end getFAActivities


###Create helper function to get 'Planned' from Foreign Assistance.gov; returns a data frame of planned events
getFAPlanned <- function(fundingType, agency, year) {
   pt <- proc.time()
   #Check for previous completion of this API webscraping task:
   if(file.exists(paste0("dfPlanned_", Sys.Date(), ".csv"))) {
      df <- read.csv(paste0("dfPlanned_", Sys.Date(), ".csv"), stringsAsFactors = FALSE)
      print(paste0("TOTAL Elapsed Time: ", (proc.time() - pt)[3]))
      return(df)
   } else {

   #set up a blank data frame
   df <- data.frame()
   contentErrorMsg <- "No records found for the specified criteria"
   
   for (fTS in fundingType) {
      for (aS in agency) {
         for (yS in year) {
            #set the URL name
            getURL <- "https://fagov-apim-prd.azure-api.us/public/api/public/planned?format=json&fundingType="
            getURL <- paste0(getURL, fTS, "&filterType=agency&filterValue=", 
                             aS, "&year=", yS)
            
            ##GET the info from the URL
            getdf <- GET(getURL)
            # print(paste0("GetDf status = ", getdf[[2]]))
            
            if(getdf[[2]] != 200) {count <- 0} else {count <- 10}
            while (getdf[[2]] != 200 & count <= 9) {
               count <- count + 1
               Sys.sleep(5)
               getdf <- GET(getURL)
               if(getdf[[2]] == 200) {count <- 10}
               # print(paste0("GetDf status = ", getdf[[2]], 
               #              "; Count = ", count))
            } #/ end while
            
            #Parse the JSON file
            jsonContent <- content(getdf, as = "text", encoding = "UTF-8")
            #No data found/avail case
            if (jsonContent == contentErrorMsg) {
               parsed <- NULL
               df2 <- data.frame()
               #Data found case   
            } else {
               parsed <- jsonlite::fromJSON(jsonContent, simplifyVector = FALSE)
               
               #set the data frome
               df2 <- as.data.frame(matrix(unlist(parsed), nrow = length(parsed), byrow = TRUE))
               colnames(df2) <- names(parsed[[1]])
               df2$fundingType <- fTS
               df2$amount <- as.numeric(df2$amount)
               df2$year <- as.integer(df2$year)
               
               #Group and summarize data frame
               df2 <- df2 %>%
                  group_by(agencyName, category, year, benefitingLocation, sector, fundingType) %>%
                  summarise(amount = sum(amount), initiatives = n())
               
               #Combine both data frames
               df <- rbind(df, df2)
            }
            
            # print(paste0("Funding Type = ", fTS, "; Agency = ", aS,
            #              "; Year= ", yS, "; Rows Added = ", dim(df2)[1], 
            #              "; Elapsed Time: ", (proc.time() - pt)[3]))
            Sys.sleep(2.5)
         } #end for yS
      } #/ end for aS
      print(paste0("Elapsed Time: ", (proc.time() - pt)[3]))
   } #/end for fTS
   
   print(paste0("TOTAL Elapsed Time: ", (proc.time() - pt)[3]))
   write.csv(df, file = paste0("dfPlanned_", Sys.Date(), ".csv"), 
             row.names = FALSE)
   return(df)
   }
} #/end getFAPlanned

