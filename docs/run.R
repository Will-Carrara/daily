library(rmarkdown)

# ignore warnings
options(warn=-1)

# script code
source('../utils/utils.R')
source('../data/polygons.R')

# read data
data = polygons

# generate home page
rmarkdown::render("index.Rmd", output_file='index.html') 

# models used for comparison
models = c('eeMETRIC', 'geeSEBAL', 'DisALEXI', 'PTJPL', 'SSEBop')

# request year
year = 2020

# generate website
for (model in models) {
    # empty containers
    count = 0
    dfs = c()
    crops = c()
    states = c()
    
    for (i in 1:nrow(data)) {
        count = count + 1
    
        # get row information
        row = data[i,]
        
        # get crop name
        crop = row$name
        
        # get state 
        state = row$state
        
        # title to pass to html
        title = model
        
        # get spatial coordinates
        geometry = unlist(row$geometry)
        
        # make api request for monthly data 
        monthly = request(geometry, model, crop_type, interval='monthly', best_effort='True', year)
        
        # make api request for daily non-pixel interpolated data
        daily = request(geometry, model, crop_type, interval='daily', best_effort='True', year)
        
        # make api request for daily pixel interpolated data
        pixel = request_pixel(geometry, model, crop_type, interval='daily', year)
        
        # aggregate daily data 
        daily_agg = rowsum(daily$et, format(daily$time, "%Y-%m"), na.rm = TRUE)
        monthly$daily = daily_agg
        
        # aggregate daily pixel data 
        pixel_agg = rowsum(pixel$et, format(pixel$time, "%Y-%m"), na.rm = TRUE)
        monthly$pixel = pixel_agg
        
        # create new storage object
        df = monthly
        names(df) = c("time", "monthly", "daily", "pixel")
        
        # calculate absolute difference (daily approximation)
        df$abs_diff = round(abs(df$monthly - df$daily), 2)
        # calculate mean percent difference (daily approximation)
        df$perc_diff = round(abs(((df$monthly - df$daily) / (df$monthly))*100), 2)
        
        # calculate absolute difference (daily pixel interpolation)
        df$abs_diff_pixel = round(abs(df$monthly - df$pixel), 2)
        # calculate mean percent difference (daily pixel interpolation)
        df$perc_diff_pixel = round(abs(((df$monthly - df$pixel) / (df$monthly))*100), 2)
        
        # store all the data
        dfs[[count]] = df
        crops = c(crops, crop)
        states = c(states, state)
    }
    # generate report
    rmarkdown::render("template.Rmd", output_file=paste0(tolower(model), '.html')) 
}

