library(rmarkdown)

# ignore warnings
options(warn=-1)

# script code
source('../utils/utils.R')

# read data
data = read.csv("../data/polygons.csv", header=TRUE)

# generate home page
rmarkdown::render("index.Rmd", output_file='index.html') 

# models used for comparison
models = c('eeMETRIC', 'geeSEBAL', 'DisALEXI', 'PTJPL', 'SIMS', 'SSEBop')

# request year
year = 2020

# generate website 
for (i in 1:nrow(data)) {
    for (model in models) {
    
        # get row information
        row <- data[i,]
        
        # get crop name
        crop = row['name']
        
        # get state 
        state = row['state']
        
        # title to pass to html
        title = model
        
        # get spatial coordinates
        geometry = row['geometry']
        
        # crop type 
        cdl = row['cdl']
        
        geom = as.list(strsplit(geometry[[1]], ",")[[1]])
        
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
        df$abs_diff = df$monthly - df$daily
        
        # calculate mean percent difference
        df$perc_diff = round(((df$monthly - df$daily) / (df$monthly))*100, 2)
        
        # generate report
        rmarkdown::render("template.Rmd", output_file=paste0(tolower(model), '.html')) 
    }
}

