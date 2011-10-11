library(SVGMapping)
library(ggplot2)
library(splines)

wunder_station_daily <- function(station, date)
  {
  base_url <- 'http://www.wunderground.com/weatherstation/WXDailyHistory.asp?'
  
  # parse date
  m <- as.integer(format(date, '%m'))
  d <- as.integer(format(date, '%d'))
  y <- format(date, '%Y')
  
  # compose final url
  final_url <- paste(base_url,
  'ID=', station,
  '&month=', m,
  '&day=', d, 
  '&year=', y,
  '&format=1', sep='')
  
  # reading in as raw lines from the web server
  # contains <br> tags on every other line
  u <- url(final_url)
  the_data <- readLines(u)
  close(u)
  
  # only keep records with more than 5 rows of data
  if(length(the_data) > 5 )
        {
        # remove the first and last lines
        the_data <- the_data[-c(1, length(the_data))]
        
        # remove odd numbers starting from 3 --> end
        the_data <- the_data[-seq(3, length(the_data), by=2)]
        
        # extract header and cleanup
        the_header <- the_data[1]
        the_header <- make.names(strsplit(the_header, ',')[[1]])
        
        # convert to CSV, without header
        tC <- textConnection(paste(the_data, collapse='\n'))
        the_data <- read.csv(tC, as.is=TRUE, row.names=NULL, header=FALSE, skip=1)
        close(tC)
        
        # remove the last column, created by trailing comma
        the_data <- the_data[, -ncol(the_data)]
        
        # assign column names
        names(the_data) <- the_header
        
        # convert Time column into properly encoded date time
        the_data$Time <- as.POSIXct(strptime(the_data$Time, format='%Y-%m-%d %H:%M:%S'))
        
        # remove UTC and software type columns
        the_data$DateUTC.br. <- NULL
        the_data$SoftwareType <- NULL
        
        # sort and fix rownames
        the_data <- the_data[order(the_data$Time), ]
        row.names(the_data) <- 1:nrow(the_data)
        
        # done
        return(the_data)
        }
}




## Load template
mySVG <- loadSVG("weather-board.svg")

## get weather informations
w <- wunder_station_daily('IESSONNE9', Sys.Date())

## Set Rplot title
setTextSVG(mySVG,"title", "Weather in Orsay Today..")

## Plot temperature historic data..
devSVGMapping(mySVG,attribute.value="rplot.temp")
p <- ggplot(w,aes(Time,TemperatureC)) + geom_point(alpha=0.55)
p <- p + stat_smooth(method="lm", formula="y~ns(x,6)", colour="red")
p <- p + xlab('')
p <- p + ylab("Temperature (°C)")
print(p)
dev.off()

## Plot relative Humidity historical data
devSVGMapping(mySVG,attribute.value="rplot.hum")
p <- ggplot(w,aes(Time,Humidity)) + geom_point(alpha=0.55)
p <- p + stat_smooth(method="lm", formula="y~ns(x,6)", colour="blue")
p <- p + xlab('')
p <- p + ylab("Rel. Humidity (%)")
print(p)
dev.off()

## Update thermometer..
currentTemp <- tail(w$TemperatureC, n=1)
names(currentTemp) <- c("thermometer")
setTextSVG(mySVG,"temperature", paste(currentTemp,"°C"))
mapDataSVG(mySVG,(currentTemp + 20)/50, mode="partial-fill")

## Update the compass
currentWindDir <-  tail(w$WindDirectionDegrees, n=1)
currentWindDir <- currentWindDir/360
names(currentWindDir) <- c("compass.needle")
mapDataSVG(mySVG, currentWindDir, mode="rotate", angle=c(0,360))

## Update Wind-Speed
currentWindSpeed <- tail(w$WindSpeedKMH, n=1)
currentWindSpeed <- paste(currentWindSpeed,"Km/h")
setTextSVG(mySVG,"wind.speed", currentWindSpeed)

## Update Pressure 
currentPressure <- tail(w$PressurehPa, n=1)
names(currentPressure) <- c("pressure.needle")
setTextSVG(mySVG,"pressure.name", "ATM. Pressure")
setTextSVG(mySVG,"pressure.min", "900")
setTextSVG(mySVG,"pressure.max", "1100")
setTextSVG(mySVG,"pressure.value", paste(currentPressure, "hPa"))
mapDataSVG(mySVG,(currentPressure-900)/200, mode="rotate", angleRange=c(0,360-90))

## Update Humidity
currentHumidity <- tail(w$Humidity,n=1)
names(currentHumidity) <- c("humidity.needle")
setTextSVG(mySVG,"humidity.name", "Rel. Humidity")
setTextSVG(mySVG,"humidity.value",paste(currentHumidity,"%"))
mapDataSVG(mySVG,currentHumidity/100, mode="rotate", angleRange=c(0,360-90))

## Set current time
ctime <- format(Sys.time(),"%H:%M")
setTextSVG(mySVG,"clock",ctime)

## Show final result
showSVG(mySVG)
