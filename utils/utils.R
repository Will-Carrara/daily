# requests
library(httr)

# api call (non-pixel based)
request = function (geometry, model, crop_type, interval, best_effort, year) {
    
    # dates for comparison
    start_date = paste0(year, '-01-01')
    end_date = paste0(year, '-12-31')

    # api server address
    #server = 'https://openet-raster-api.org/'
    server = 'http://127.0.0.1:8000/'
    
    # create query and make request
    response = POST(
        url = paste0(server, 'timeseries/polygon'), 
        query = list(admin_key = 'hello'),
        body = list(
            start_date = start_date,
            end_date = end_date,
            interval = interval,
            geometry = geometry,
            model = tolower(model),
            variable = 'et',
            ref_et_source = 'gridmet',
            pixel_aggregation = 'mean',
            moving_average = 0,
            units = 'metric',
            provisional = 'False',
            best_effort = best_effort,
            output_file_format = 'csv'
        ),
        encode = 'json')
    
    # obtain content of api return 
    content = as.data.frame(content(response, as="parsed", type="text/csv"))

    return (content)
}

# create empty dataframe
full <- data.frame()

# api call (pixel based)
request_pixel = function (geometry, model, crop_type, interval, year) {
    
    # monthly date ranges
    jan = c(paste0(year, '-01-01'), paste0(year, '-01-31'))
    if (year == 2020 || year == 2016){
        feb = c(paste0(year, '-02-01'), paste0(year, '-02-29'))
    } else{
        feb = c(paste0(year, '-02-01'), paste0(year, '-02-28'))
    }
    mar = c(paste0(year, '-03-01'), paste0(year, '-03-31'))
    apr = c(paste0(year, '-04-01'), paste0(year, '-04-30'))
    may = c(paste0(year, '-05-01'), paste0(year, '-05-31'))
    jun = c(paste0(year, '-06-01'), paste0(year, '-06-30'))
    jul = c(paste0(year, '-07-01'), paste0(year, '-07-31'))
    aug = c(paste0(year, '-08-01'), paste0(year, '-08-31'))
    sep = c(paste0(year, '-09-01'), paste0(year, '-09-30'))
    oct = c(paste0(year, '-10-01'), paste0(year, '-10-31'))
    nov = c(paste0(year, '-11-01'), paste0(year, '-11-30'))
    dec = c(paste0(year, '-12-01'), paste0(year, '-12-31'))
    
    # list of months
    months = list(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)
    
    # request one month at a time
    for (m in months) {

        # dates for comparison
        start_date = m[1]
        end_date = m[2]
        
        # api server address
        #server = 'https://openet-raster-api.org/'
        server = 'http://127.0.0.1:8000/'
        
        # create query and make request
        response = POST(
            url = paste0(server, 'timeseries/polygon'), 
            query = list(admin_key = 'hello'),
            body = list(
                start_date = start_date,
                end_date = end_date,
                interval = interval,
                geometry = geometry,
                model = tolower(model),
                variable = 'et',
                ref_et_source = 'gridmet',
                pixel_aggregation = 'mean',
                moving_average = 0,
                units = 'metric',
                provisional = 'False',
                best_effort = 'False',
                output_file_format = 'csv'
            ),
            encode = 'json')
        
        # obtain content of api return 
        content = as.data.frame(content(response, as="parsed", type="text/csv"))
        
        # combine data
        full = rbind(full, content)
    }
    return (full)
}


# beautiful plotting 
visualize = function (df, crop, state, year, model) {
    
    # set margins
    par(mar=c(8,3,3.5,1), oma=c(0,4,0,0))
    
    # set variable for empty plot 
    x = c(0,12)
    y = c(0,300)
    
    # x-axis for plotting
    months = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec")
    
    # set color parameters
    daily_col = "tan1"
    pixel_col = "steelblue1"
    monthly_col = "tomato1"
    
    # make empty plot
    plot(x~y, xlim=x, ylim=y, xlab="", ylab="", xaxt="n", las=1, cex.axis=1.8, col="white")
    grid()
    
    # add graph labels for title
    title(main=paste(crop, "in", state, "for", year), cex.main=2.1, line=2)
    
    # add graph labels for axis
    title(xlab="Month", cex.lab=2, line=4)
    title(ylab="ET (mm)", cex.lab=2, line=2, outer=TRUE, adj=.56)
    
    # reformat x-axis
    xtick<-seq(0, 11, by=1)
    axis(side=1, at=xtick, labels=FALSE)
    text(x=xtick, par("usr")[3], labels=months, pos=1, cex=1.8, offset=1.1, xpd=TRUE)
    
    # add graph data
    lines(daily~c(0:11), data=df, col=daily_col, lty=1, lwd=3.5)
    lines(pixel~c(0:11), data=df, col=pixel_col, lty=1, lwd=3.5)
    lines(monthly~c(0:11), data=df, col=monthly_col, lty=1, lwd=3.5)

    # add legend
    legend("topright", inset=c(.008,.02), legend=c("Approximation", "Pixel Interpolation", "Monthly"), 
           col=c(daily_col, pixel_col, monthly_col), pch=16, xpd=TRUE, bty="n", cex=1.8)
}