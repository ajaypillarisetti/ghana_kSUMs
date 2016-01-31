dashboardPage(
    dashboardHeader(title = "CheckSUMs"),
    dashboardSidebar(disable=FALSE,
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Raw Data", icon = icon("th"), tabName = "rawdata")
        )
    ),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
            tags$script(type='text/javascript', src='dygraph-extra.js'),
            tags$script(type='text/javascript', src='scripties.js')
            ),
        tabItems(
            tabItem(tabName = 'dashboard',
                fluidRow(
                    box(width=6, collapsible = TRUE, status="info", solidHeader=TRUE, title="Introduction",
                        h5('Welcome to CheckSUM!'),
                        p("CheckSUM is a simple tool to visualize and quickly check the status of stove use monitors in the field.")
                    ),
                    column(width=6,
                        box(width=NULL, collapsible = FALSE, status='info', solidHeader=T, title='Select File',
                        # fileInput('files', 'Select a kSUM file', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), multiple=F)
                        uiOutput('fileList')
                        )
                    )
                ),
                fluidRow(
                    box(
                        width = 8, height='510px',
                        status = "info", solidHeader = TRUE,
                        title = textOutput("filename"),
                        dygraphOutput('plainPlot', height='445px')
                    ),
                    box(textOutput("legendARY"), title = "Legend", collapsible = TRUE, status='info', solidHeader=TRUE, width=4),
                    infoBoxOutput('maxTemp'),
                    infoBoxOutput('minTemp'),
                    infoBoxOutput('avgDailyRange'),
                    infoBoxOutput('sampleInterval')
                ),
                fluidRow(
                    box(
                        width = 12,
                        status = "info", solidHeader = TRUE,
                        title = "Summary Data",
                        dataTableOutput("diagnosticsOutput")
                    )
                )
            ),
        tabItem(tabName = 'rawdata', verbatimTextOutput("allDataTable"))
        )           	
	)
)