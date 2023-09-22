#### Creating the long format of the energy data set  

I iterated over each csv file to generate two new columns based on the file name: 
`region` and `generation_type` and then used `rbind()` to vertically merge the 
data sets. The code to do so is shown below.  


```
data_files <- list.files(path='/path/to/data/files', full.names = FALSE, recursive = TRUE)
```

```
energy_list <- lapply(data_files, function(x){
  data <- readr::read_csv(file.path("./Data", x), col_names = c("datetime", "value"), skip = 5)
  data <- mutate(data, datetime = lubridate::mdy_h(datetime, tz = "UTC"),
                     region = str_split(x, pattern = '_')[[1]][6],
                     generation_type = str_split(x, pattern = '_')[[1]][4])
```

```
energy_long <- bind_rows(energy_list)
```

I used this long format of the data set to then create various other wide and
long format versions.  

#### Additional Resources  
[California Profile Analysis](https://www.eia.gov/state/analysis.php?sid=CA#:~:text=Coal%20fuels%20only%20a%20small,state's%20utility%2Dscale%20net%20generation.&text=California%20imports%20more%20electricity%20than%20any%20other%20state.)

[Wind Seasonal Patterns Across the U.S.](https://www.eia.gov/todayinenergy/detail.php?id=20112#:~:text=Capacity%20factors%20for%20most%20regions,from%20that%20point%20through%20December.)

#### Further extensions for this project  
1. Look into ways for allowing the user to select multiple options from a
 single menu.  
2. Separate the energy generation types into renewable and non-rewable energy.  
3. Continue working on understanding the maths behind PCA.  
4. Add another tab on the PCA page to show the summary statistics of original 
features for clustered groups.  
5. Look into shinyLP for making interactive landing pages.  
6. Add a weekly tab to the time series page with raw and proportions data
