library(readr)
jan_2013 <- read_csv("RStudio/Chess/2013_Jan.csv")
feb_2013 <- read_csv("RStudio/Chess/2013_Feb.csv")
mar_2013 <- read_csv("RStudio/Chess/2013_Mar.csv")
april_2013 <- read_csv("RStudio/Chess/2013_April.csv")
may_2013 <- read_csv("RStudio/Chess/2013_May.csv")
june_2013 <- read_csv("RStudio/Chess/2013_June.csv")
july_2013 <- read_csv("RStudio/Chess/2013_July.csv")
august_2013 <- read_csv("RStudio/Chess/2013_Aug.csv")
september_2013 <- read_csv("RStudio/Chess/2013_Sept.csv")
october_2013 <- read_csv("RStudio/Chess/2013_Oct.csv")
november_2013 <- read_csv("RStudio/Chess/2013_Nov.csv")
december_2013 <- read_csv("RStudio/Chess/2013_Dec.csv")
jan_2014 <- read_csv("RStudio/Chess/2014_Jan.csv")

x2013 <- rbind(jan_2013, feb_2013, mar_2013, april_2013, may_2013, june_2013, july_2013,
               august_2013, september_2013, october_2013, november_2013, december_2013,
               jan_2014)
               
write.csv(x2013, "jan_2013-jan_2014.csv")

                                   