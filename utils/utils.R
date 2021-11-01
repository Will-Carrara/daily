# requests
library(httr)

# table package
library(kableExtra)

# api call
request = function (geometry, model, crop_type, interval, year) {
    
    # dates for comparison
    start_date = paste0(year, '-01-01')
    end_date = paste0(year, '-12-31')

    # api server address
    server = 'https://openet-raster-api.org/'
    
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
            output_file_format = 'csv'
        ),
        encode = 'json')
    
    # obtain content of api return 
    content = as.data.frame(content(response, as="parsed", type="text/csv"))

    return (content)
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
    
    # set color parameters for models
    if (model == 'eeMETRIC') {
        daily_col = '#000CC8'
        monthly_col = '#31609E'
    } else if (model == 'geeSEBAL') {
        daily_col = '#E8A417'
        monthly_col = '#E7B518'
    } else if (model == 'DisALEXI') {
        daily_col = '#9D702D'
        monthly_col = '#63471D'
    } else if (model == 'PTJPL') {
        daily_col = '#DA6960'
        monthly_col = '#DA302E'
    } else if (model == 'SIMS') {
        daily_col = '#78D481'
        monthly_col = '#518F57'
    } else if (model == 'SSEBop') {
        daily_col = '#9CD0FF'
        monthly_col = '#5CA1FF'
    } 
    
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
    lines(monthly~c(0:11), data=df, col=monthly_col, lty=1, lwd=3.5)

    # add legend
    legend("topright", inset=c(.008,.02), legend=c("Daily", "Monthly"), 
           col=c(daily_col, monthly_col), pch=16, xpd=TRUE, bty="n", cex=1.8)
    
    ## BAR PLOT 
    # create fake barplot then plot over to avoid gridlines
    barplot(df$perc_diff~months, names.arg=months, col="white", xlab="", ylab="", xaxt="n", yaxt="n", las=1)
    grid()
    barplot(df$perc_diff~months, names.arg=months, col=monthly_col, add=TRUE, cex.axis=1.8, cex.names=1.8)

    # add barplot labels for title
    title(main="Percent Difference by Month", cex.main=2.1, line=2)
    
    # add graph labels for axis
    title(xlab="Month", cex.lab=2, line=4)
    title(ylab="% Error", cex.lab=2, line=2, outer=TRUE, adj=.56)
}

# beautiful table 
nice_table = function (df) {
    df %>%
        kbl() %>%
        kable_styling()
}