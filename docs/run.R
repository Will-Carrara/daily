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
models = c('eeMETRIC', 'geeSEBAL', 'DisALEXI', 'PTJPL', 'SIMS', 'SSEBop')

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
        
        # crop type 
        cdl = row$cdl
        
        # make api request for monthly data
        monthly = request(geometry, model, crop_type, interval='monthly', year)
        
        # make api request for daily data
        daily = request(geometry, model, crop_type, interval='daily', year)
        
        # aggregate daily data 
        daily_agg = rowsum(daily$et, format(daily$time, "%Y-%m"))
        monthly$daily = daily_agg
        
        # create new storage object
        df = monthly
        names(df) = c("time", "monthly", "daily")
        
        # calculate absolute difference 
        df$abs_diff = abs(df$monthly - df$daily)
        
        # calculate mean percent difference
        df$perc_diff = abs(round(((df$monthly - df$daily) / (df$monthly))*100, 2))
        
        # store all the data
        dfs[[count]] = df
        crops = c(crops, crop)
        states = c(states, state)
    }
    # generate report
    rmarkdown::render("template.Rmd", output_file=paste0(tolower(model), '.html')) 
}

