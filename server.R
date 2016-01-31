### Ajay Pillarisetti, University of California, Berkeley, 2015
### V1.0G
### ADD HORIZONTAL LINE AT 85C
## 1115M, 1228M, 
## Write out to RDS (?)
## ksums diagostics


Sys.setenv(TZ="GMT")

shinyServer(function(input, output) {

	output$fileList <- renderUI(function(){
		# change path here. No trailing slash.
		files<-list.files(paste(path_to_dropbox, '/Ghana_adoption_data_SHARED/serverTest/kSUMs', sep=""), recursive=T, include.dirs=T, full.names=T, pattern='txt')
		files <- grep('.pdf', files, invert=T, value=T)
		selectInput("dataset", "Choose a File:", choices = sort(basename(files)))
	})	

	#read in data
	datasetInput <- reactive({
	  inFile <- input$dataset
 		if (is.null(inFile)){return(NULL)} 
 		# change path here. Trailing slash required.
 		inFile <- paste(path_to_dropbox, '/Ghana_adoption_data_SHARED/serverTest/kSUMs/', inFile, sep='')
		dta<- read.kSUM(inFile)
	})

	datasetName <- reactive({
	    inFile <- input$files
    	if (is.null(inFile)){return(NULL)} 
		inFile$name[1]
	})

	data_cleaned <- reactive({
		if (is.null(datasetInput())) return(NULL)
		data_d <- datasetInput()[,with=F]
		data_d
	})

	data_cleaned_long <- reactive({
		if (is.null(datasetInput())) return(NULL)
		data_d <- melt(datasetInput(), id.var='datetime', measure.var=c('t0', 't1', 't2', 't3'))
		data_d[,Date:=as.Date(ymd(substring(datetime,1,10)))]
	})

	####################
	##### datasets ##### 
	####################
	dataXTS.plainplot <- reactive({
		dta<-data_cleaned()
		dta <- dta[, lapply(.SD, as.numeric), by=datetime]
		as.xts(dta[,c('datetime','t0','t1','t2','t3'), with=F])
	})

	filename <- reactive({datasetName()})

	output$filename <- renderText({input$dataset})

	####################
	##### dygraphs ##### interactivity - to subset data to user-selected range
	####################
	#threshold plots
	from <- reactive({
		if (!is.null(input$thresholdPlot_date_window))
		ymd_hms(strftime(gsub(".000Z","",gsub("T"," ",input$thresholdPlot_date_window[[1]])), "%Y-%m-%d %H:%M:%S"))
	})
  
	to <- reactive({
		if (!is.null(input$thresholdPlot_date_window))
		ymd_hms(strftime(gsub(".000Z","",gsub("T"," ",input$thresholdPlot_date_window[[2]])), "%Y-%m-%d %H:%M:%S"))
	})  

	output$from <- renderText({from()})
  	output$to <- renderText({to()})
	output$graphTitle <- renderText({paste("Time Series Graph:", paste(from(), to(), sep=" to ")," ")}) 

	#UI OUTPUTS
	fileMin <- reactive({data_cleaned_long()[,min(temp), by='variable']})

	fileMax <- reactive({data_cleaned_long()[,max(temp), by='variable']})

	fileSamplingInterval <- reactive({as.numeric(data_cleaned()[10,'datetime',with=F]-data_cleaned()[9,'datetime',with=F])})

	####################
	####### Boxes ###### 
	####################
	# Overview Page 	
	output$maxTemp <- renderInfoBox({
		if (is.null(datasetInput())) return(NULL)
		maxTemp <- data_cleaned_long()[,max(value)]
		lead_problems <- data_cleaned_long()[value>500, paste(unique(variable), collapse=" "),]
		maxThreshold <- 500
		infoBox(
			value = formatC(maxTemp, digits = 2, format = "f"),
			title = if (maxTemp >= maxThreshold) "Warning: 500C Exceeded" else "Max Temp",
			icon = if (maxTemp >= maxThreshold)icon("warning") else icon("chevron-circle-up"),
			color = if (maxTemp >= maxThreshold) "red" else "green"
		)
	})

	output$minTemp <- renderInfoBox({
		if (is.null(datasetInput())) return(NULL)
		minTemp <- data_cleaned_long()[,min(value)]
		lead_problems <- data_cleaned_long()[value<0, paste(unique(variable), collapse=" "),]
		minThreshold <- 0
		infoBox(
			value = formatC(minTemp, digits = 2, format = "f"),
			title = if (minTemp <= minThreshold) paste("Warning: Values Below 0 for leads", lead_problems, sep = " ") else "Min Temp",
			icon = if (minTemp <= minThreshold) icon("warning") else icon("chevron-circle-down"),
			color = if (minTemp <= minThreshold) "red" else "green"
		)
	})

	output$avgDailyRange <- renderInfoBox({
		if (is.null(datasetInput())) return(NULL)
		data_cleaned_long()[,value:=as.numeric(value)]
		data_cleaned_long()[value>0 & value<500,dailyRange:=max(value, na.rm=T)-min(value, na.rm=T),by='Date']
		avgDailyRange <- data_cleaned_long()[,mean(dailyRange, na.rm=T)]
		dailyRangeThreshold <- 15
		# deviceIDs <- data_cleaned()[avgDailyRange<=dailyRangeThreshold,unique(device_id)]
		infoBox(
			value = formatC(avgDailyRange, digits = 2, format = "f"),
			title = if (avgDailyRange <= dailyRangeThreshold) paste("Warning: Avg Daily Range ", formatC(avgDailyRange, digits = 2, format = "f"), sep="") else "Avg Daily Range",
			icon = if (avgDailyRange <= dailyRangeThreshold) icon("warning") else icon("chevron-circle-down"),
			color = if (avgDailyRange <= dailyRangeThreshold) "red" else "green"
		)
	})

	####################
	###### Tables ###### 
	####################
	output$allDataTable<-renderPrint({
		orig <- options(width = 1000)
		print(melt(data_cleaned(),  id.var=c('datetime','device_id', 'location', 'description'), measure.var='temp'), 1000)
		options(orig)
	})

	diagnostics <- reactive({
		data_cleaned_long()[,
			list(
				min=min(value),
				max=max(value),
				mean=round(mean(value), 2),
				median=median(value)
			),by='variable,Date']
	})

	output$diagnosticsOutput <- renderDataTable(diagnostics(), options=list(searchable = FALSE, searching = FALSE, pageLength = 20,rowCallback = I('function(row, data) {
			if (data[2] < 0) 
				$("td", row).css({"background" : "red", "color" : "white"});
			else if (data[6] > 500) 
				$("td", row).css({"background" : "red", "color" : "white"});
			;}')))
	# ,columnDefs = list(list(width = '100px', targets = c(0:4)))

	####################
	####### PLOTS ###### 
	####################
	output$plainPlot<- 
	renderDygraph({
		# if (is.null(datasetInput())) return(NULL)
		dygraph(dataXTS.plainplot()) %>% 
	    dyOptions(axisLineWidth = 1.5, fillGraph = F, drawGrid = FALSE, useDataTimezone=T, strokeWidth=1, connectSeparatedPoints=T) %>%
	    dyAxis("y", label = "Temp C") %>%
	    dyAxis("x", label = "Date & Time") %>%
        dyLegend(labelsDiv = "legendARY")
	})	
})