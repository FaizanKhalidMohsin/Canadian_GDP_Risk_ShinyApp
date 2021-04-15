

# function_server R Script File. 



########### Script File Description ################################################################################
#
#
#
#
#
#####################################################################################################################

# The following R libraries are used:

 
# packages = c("zoo", "quantreg", "ggridges",  "viridis", "fitdistrplus", "sn", "tidyverse", "lubridate", "readxl")
# ## Now load or install&load all necessary packages
# package.check <- lapply(
#   packages,
#   FUN = function(x) {
#     if (!require(x, character.only = TRUE)) {
#       install.packages(x, dependencies = TRUE)
#       library(x, character.only = TRUE)
#     }
#   }
# )

library(zoo)               # Date package
library(quantreg)          # Quantile regression
library(ggridges)          # Ridgeline plots
library(viridis)           # Colours for graphs
library(fitdistrplus)      # Package to fit parametric distributions
library(sn)                # Skew-t distribution
library(tidyverse)         # Standard
library(lubridate)         # Standard Tidyverse
library(readxl)            # Standard




############################################## 
 ###   ####   ###  Script  ####   ####  #### 
##############################################


#####################################################################################################################
# Script



  
#   #Read data
#   GDP <- read_excel("GDP.xlsm", 
#                     sheet = "Can_real_GDP", 
#                     range = cell_cols("A:C"), 
#                     col_names = c("period", "level", "rate"))
#   
#   #Clean data
#   GDP_final <- GDP %>% 
#     mutate(Date = as.Date(as.yearqtr(period, format = "Q%q %Y")), 
#            Growth = as.numeric(rate)) %>% 
#     select(Date, Growth) %>% 
#     mutate(Growth_1 = lag(Growth, 1), 
#            Growth_4 = lag(Growth, 4)) %>%
#     select(Date, Growth, Growth_1, Growth_4) %>% 
#     drop_na()
#   
#   #GDP_final
#   
#   
#   ## 3. Selecting a forward-looking, financial indicator variable
#   
#   #Read data
#   Canada_FSI <- read_excel("FSI_Canada_norm.xlsx", 
#                            sheet = "Quarterly_FSI", 
#                            range = cell_cols("A:B"), 
#                            col_types = c("date", rep("text", 1)), 
#                            col_names = c("Date", "Canada_FSI")) %>% 
#     mutate(FSI = as.numeric(Canada_FSI)) %>% 
#     select(Date, FSI) %>% 
#     mutate(Average_Stress = FSI/FSI - 1)
#   
#   Canada_FSI <- Canada_FSI[-(1:2), ]
#   
#   
#   #Canada_FSI
#   
#   
#   ######################################################################################################################
#   # Function 3:
#   
#   # We calculate and graph the inverse of the CSTO FSI.  For the quantile regression (below) we will 
#   # regress GDP growth on the inverse FSI (when risk is "up", GaR is "down", defined as further in the left tail)
#   
#   #Read data
#   Inverse_FSI <- read_excel("FSI_Canada_norm.xlsx", 
#                             sheet = "Quarterly_FSI",
#                             range = cell_cols("A:D"),
#                             col_types = c("date", rep("text", 3)),
#                             col_names = c("Period", "Q_FSI", "Inverse", "Inverse_FSI")) %>% 
#     mutate(Date = as.Date(Period), Q_FSI = as.numeric(Q_FSI), Inverse_FSI = as.numeric(Inverse_FSI)) %>% 
#     select(Date, Q_FSI, Inverse_FSI) %>% 
#     mutate(Average_Stress = Q_FSI/Q_FSI -1) %>% 
#     drop_na()
#   
#   
#   ######################################################################################################################
#   #Function 4:
#   
#   
#   # Read data.
#   FSI <- read_excel("FSI_Canada_norm.xlsx", 
#                     sheet = "Quarterly_FSI",
#                     range = cell_cols("A:D"),
#                     col_types = c("date", rep("text", 3)),
#                     col_names = c("Period", "Q_FSI", "Inverse", "Inverse_FSI")) %>% mutate(Date = as.Date(Period), FSI = as.numeric(Inverse_FSI)) %>% 
#     select(Date, FSI) 
#   
#   #Clean data
#   FSI <- FSI[-(1:2), ] %>% 
#     drop_na()
#   
#   #FSI
#   
#   #The inverse FSI is selected as the independent variable that drives the conditional GDP growth distribution. 
#   
#   #Clean data
#   FSI_final <- FSI %>% 
#     select(Date, FSI) %>% 
#     mutate(Date = floor_date(Date, unit = "quarter")) %>% 
#     arrange(Date)
#   
#   
#   # Like the GDP growth rate, the Inverse FSI is lagged by one and four periods.  
#   # The GDP and FSI data are merged into one table:
#   
#   data_join <- left_join(GDP_final, FSI_final, by = "Date") %>% 
#     mutate(FSI_1 = lag(FSI, 1), 
#            FSI_4 = lag(FSI, 4)) %>% 
#     drop_na()
#   
#   #data_join
#   
#   
#   # if( is.null(dates_start_input) & is.null(dates_end_input) ) {
#   # 
#   #       data_join =   subset(data_join, Date < dates_start_input & Date > dates_end_input)
#   # }
#   
#   
#   #####################################################################################################################
#   # Function 5: 
#   
#   ## 5. Regression equation and estimates
#   
#   #The regression equation used here is:  eqn.q = Growth ~ Growth_1 + FSI_1. Note that we are projecting one period ahead (fcast = fcast_1). 
#   
#   #Tobias Adrian discusses the rationale for regressing GDP growth on both GDP growth and a finanacial conditions index. He notes the interesting fact that economic conditions are not significant and that all variability in the left tail is explained by the financial conditions index (Tobias Adrian et al, 2017, p. 8).
#   
#   
#   fcast <- 1
#   inccg <- 1
#   
#   if (inccg > 0) {
#     eqn.q <- formula(paste0("Growth ~ Growth_", fcast, " + FSI_", fcast))
#   } else {
#     eqn.q <- formula(paste0("Growth ~ Growth_", fcast))  
#   }
#   
#   q.inst <- rq(eqn.q, data = data_join, tau = seq(0.05, 0.95, 0.025))
#   #summary(q.inst)
#   
#   
#   
#   
#   ######################################################################################################################
#   # Function 6: 
#   
#   ## 6. Non-parametric regression results
#   
#   #The result of the quantile regression are 37 in-sample, empirical (non-parametric) probability density estimates which can be interpolated and plotted as ridgeline plots. In essence, we can think of this as simply smoothing the 37 empirical quantiles that we estimated to back out a ridgeline plot that looks like a PDF.  
#   
#   #To do so, we pull the predicted values into a matrix ("predict(q.inst)"), rename by date, and pivot longer for graphing in a ridgeline plot.  The results are shown in the following graph of the conditional GDP growth distributions for each quarter back to 2001.
#   
#   q.predict <- t(predict(q.inst)) %>%           
#     as_tibble(.name_repair = "unique") %>% 
#     rename_with(~ as.character(data_join$Date)) %>%
#     pivot_longer(everything(), names_to = "Date", values_to = "Vals") %>%
#     mutate(Date = as.Date(Date)) %>% 
#    filter( 2000 < lubridate::year(Date))
#   
#   
#   # # The start and end dates
#   # date_start = data_join$Date[1]
#   # date_end = data_join$Date[nrow(data_join)]
#   # return( list(q.predict, c(date_start, date_end ))  ) 
#   
# 
# 
# 
# # head(data_join)
# # data_join$Date[1]
# # date_end = data_join$Date[nrow(data_join)]
# 
# # dates_df =  density_plots_data_prep()
# # dates_start = dates_df[1]
# # dates_end = dates_df[2]




######################################################################################################################




############################################## 
###   ####   ###  Functions  ####   ####  #### 
##############################################




######################################################################################################################

# Main Function:


density_plots = function(dates_start_input = "2001-01-01", dates_end_input = "2020-04-01") {###


      # Function 1:
       
      #Read data
      GDP <- read_excel("GDP.xlsm", 
                        sheet = "Can_real_GDP", 
                        range = cell_cols("A:C"), 
                        col_names = c("period", "level", "rate"))
      
      #Clean data
      GDP_final <- GDP %>% 
        mutate(Date = as.Date(as.yearqtr(period, format = "Q%q %Y")), 
               Growth = as.numeric(rate)) %>% 
        select(Date, Growth) %>% 
        mutate(Growth_1 = lag(Growth, 1), 
               Growth_4 = lag(Growth, 4)) %>%
        select(Date, Growth, Growth_1, Growth_4) %>% 
        drop_na()
      
      #GDP_final
      
      
      ## 3. Selecting a forward-looking, financial indicator variable
      
      #Read data
      Canada_FSI <- read_excel("FSI_Canada_norm.xlsx", 
                               sheet = "Quarterly_FSI", 
                               range = cell_cols("A:B"), 
                               col_types = c("date", rep("text", 1)), 
                               col_names = c("Date", "Canada_FSI")) %>% 
        mutate(FSI = as.numeric(Canada_FSI)) %>% 
        select(Date, FSI) %>% 
        mutate(Average_Stress = FSI/FSI - 1)
      
      Canada_FSI <- Canada_FSI[-(1:2), ]
      
      
      #Canada_FSI
      
      
      ######################################################################################################################
      # Function 3:
      
      # We calculate and graph the inverse of the CSTO FSI.  For the quantile regression (below) we will 
      # regress GDP growth on the inverse FSI (when risk is "up", GaR is "down", defined as further in the left tail)
      
      #Read data
      Inverse_FSI <- read_excel("FSI_Canada_norm.xlsx", 
                                sheet = "Quarterly_FSI",
                                range = cell_cols("A:D"),
                                col_types = c("date", rep("text", 3)),
                                col_names = c("Period", "Q_FSI", "Inverse", "Inverse_FSI")) %>% 
        mutate(Date = as.Date(Period), Q_FSI = as.numeric(Q_FSI), Inverse_FSI = as.numeric(Inverse_FSI)) %>% 
        select(Date, Q_FSI, Inverse_FSI) %>% 
        mutate(Average_Stress = Q_FSI/Q_FSI -1) %>% 
        drop_na()
      
      
      ######################################################################################################################
      #Function 4:
      
      
      # Read data.
      FSI <- read_excel("FSI_Canada_norm.xlsx", 
                        sheet = "Quarterly_FSI",
                        range = cell_cols("A:D"),
                        col_types = c("date", rep("text", 3)),
                        col_names = c("Period", "Q_FSI", "Inverse", "Inverse_FSI")) %>% mutate(Date = as.Date(Period), FSI = as.numeric(Inverse_FSI)) %>% 
        select(Date, FSI) 
      
      #Clean data
      FSI <- FSI[-(1:2), ] %>% 
        drop_na()
      
      #FSI
      
      #The inverse FSI is selected as the independent variable that drives the conditional GDP growth distribution. 
      
      #Clean data
      FSI_final <- FSI %>% 
        select(Date, FSI) %>% 
        mutate(Date = floor_date(Date, unit = "quarter")) %>% 
        arrange(Date)
      
      
      # Like the GDP growth rate, the Inverse FSI is lagged by one and four periods.  
      # The GDP and FSI data are merged into one table:
      
      data_join <- left_join(GDP_final, FSI_final, by = "Date") %>% 
        mutate(FSI_1 = lag(FSI, 1), 
               FSI_4 = lag(FSI, 4)) %>% 
        drop_na()
      
      #data_join
      
      
      # if( is.null(dates_start_input) & is.null(dates_end_input) ) {
      # 
      #       data_join =   subset(data_join, Date < dates_start_input & Date > dates_end_input)
      # }
      
       
      #####################################################################################################################
      # Function 5: 
      
      ## 5. Regression equation and estimates
      
      #The regression equation used here is:  eqn.q = Growth ~ Growth_1 + FSI_1. Note that we are projecting one period ahead (fcast = fcast_1). 
      
      #Tobias Adrian discusses the rationale for regressing GDP growth on both GDP growth and a finanacial conditions index. He notes the interesting fact that economic conditions are not significant and that all variability in the left tail is explained by the financial conditions index (Tobias Adrian et al, 2017, p. 8).
      
      
      fcast <- 1
      inccg <- 1
      
      if (inccg > 0) {
            eqn.q <- formula(paste0("Growth ~ Growth_", fcast, " + FSI_", fcast))
            } else {
            eqn.q <- formula(paste0("Growth ~ Growth_", fcast))  
      }
      
      q.inst <- rq(eqn.q, data = data_join, tau = seq(0.05, 0.95, 0.025))
      #summary(q.inst)
      
      
      
      
      ######################################################################################################################
      # Function 6: 
      
      ## 6. Non-parametric regression results
      
      #The result of the quantile regression are 37 in-sample, empirical (non-parametric) probability density estimates which can be interpolated and plotted as ridgeline plots. In essence, we can think of this as simply smoothing the 37 empirical quantiles that we estimated to back out a ridgeline plot that looks like a PDF.  
      
      #To do so, we pull the predicted values into a matrix ("predict(q.inst)"), rename by date, and pivot longer for graphing in a ridgeline plot.  The results are shown in the following graph of the conditional GDP growth distributions for each quarter back to 2001.
      
      q.predict <- t(predict(q.inst)) %>%           
            as_tibble(.name_repair = "unique") %>% 
            rename_with(~ as.character(data_join$Date)) %>%
            pivot_longer(everything(), names_to = "Date", values_to = "Vals") %>%
            mutate(Date = as.Date(Date)) %>% 
            filter( as.Date(dates_start_input) < Date & Date < as.Date(dates_end_input) )
            
            #filter( 2000 < lubridate::year(Date) & lubridate::year(Date) < 2010 )
            
      #2004-01-01
      #q.predict  =   filter(q.predict, Date <= dates_start_input & Date >= dates_end_input)
      
      
      #```{r nonpara2, fig.width=12, fig.height=12, message=FALSE}
      plot4 = ggplot(q.predict, aes(x = Vals, y = Date, group = Date)) + 
            geom_density_ridges(scale = 25, colour = "grey77", fill = "slateblue1", alpha = 0.5) +
            theme_ridges() + 
            labs(x = "", y = "", title = "Canada's GDP@Risk: Non-parametric density estimates, 2001-2020")

      print(plot4)
      #return(plot4)
      
      
      # # The start and end dates
      # date_start = data_join$Date[1]
      # date_end = data_join$Date[nrow(data_join)]
      # return( list(q.predict, c(date_start, date_end ))  ) 
      
}###


# head(data_join)
# data_join$Date[1]
# date_end = data_join$Date[nrow(data_join)]

# dates_df =  density_plots_data_prep()
# dates_start = dates_df[1]
# dates_end = dates_df[2]




