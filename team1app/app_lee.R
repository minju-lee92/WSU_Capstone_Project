#librarys
library(ggplot2)
library(dbplyr)
library(shinydashboard)
library(sjmisc)
library(dplyr)
library(ggplot2)
library(RSNNS)
library(randomForest)
library(tidyr)

options(shiny.maxRequestSize=50*1024^2)
vars =  c("Overall.Customer.ID", "average_size", "average_contract_length", "tblA_average_trans_day_type1", "tblA_average_trans_day_type0", "tblD_average_trans_day_type1",
          "tblD_average_trans_day_type0", "total_requests", "total_early", "total_late", "total_ontime", "first_contract_date", "delivery_score", "labels") 

file_input = fluidPage(
    titlePanel("Input Files"),
    fluidRow(
        column(4,
               fileInput("tblcust", "Customer Info", multiple = FALSE, accept = NULL,
                         width = NULL, buttonLabel = "Browse...",
                         placeholder = "No file selected")
        ),
        column(4,
               fileInput("tbla", "Table A", multiple = FALSE, accept = NULL,
                         width = NULL, buttonLabel = "Browse...",
                         placeholder = "No file selected")
        ),
        column(4,
               fileInput("tblb", "Table B", multiple = FALSE, accept = NULL,
                         width = NULL, buttonLabel = "Browse...",
                         placeholder = "No file selected")
        ),
        column(4,
               fileInput("tbld", "Table D", multiple = FALSE, accept = NULL,
                         width = NULL, buttonLabel = "Browse...",
                         placeholder = "No file selected")
        ),
        column(4,
               fileInput("tble", "Table E", multiple = FALSE, accept = NULL,
                         width = NULL, buttonLabel = "Browse...",
                         placeholder = "No file selected")
        ),
        column(4,
               actionButton("run_feature_list", "Build Feature List")
        )
    )
)



# individual tab sections generally we should be able to throw what ever shiny dashboard we want to in this section


first_tab = fluidPage(
    titlePanel("Basic DataTable"),
    
    # Create a new Row in the UI for selectInputs
    fluidRow(
        column(4,
               selectInput("custsize",
                           "Customer Size:",
                           c("All",
                             0,1,2,3)))
    ),
    # Create a new row for the table.
    DT::dataTableOutput("customerInfo")
)


sec_tab = fluidPage(
    titlePanel("Basic DataTable"),
    
    # Create a new Row in the UI for selectInputs
    fluidRow(
        column(4,
               selectInput("atype",
                           "type:",
                           c("All",
                             0:1))
        ),
        column(4,
               selectInput("amonth",
                           "Month:",
                           c("All",
                             1:12))
               
        ),
        column(4,
               selectInput("aday",
                           "Day:",
                           c("All",
                             1:31))
               
        ),
        column(4,
               selectInput("ayear",
                           "year:",
                           c("All",
                             2010:2021))
               
        )
    ),
    # Create a new row for the table.
    DT::dataTableOutput("tableA")
)

third_tab = fluidPage(
    titlePanel("Basic DataTable"),
    
    # Create a new Row in the UI for selectInputs
    fluidRow(
        column(4,
               selectInput("bmonth",
                           "Month:",
                           c("All",
                             1:12))
               
        ),
        column(4,
               selectInput("bday",
                           "Day:",
                           c("All",
                             1:31))
               
        ),
        column(4,
               selectInput("byear",
                           "Year:",
                           c("All",
                             2010:2021))
               
        )
    ),
    # Create a new row for the table.
    DT::dataTableOutput("tableB")
)

fourth_tab = fluidPage(
    titlePanel("Basic DataTable"),
    
    # Create a new Row in the UI for selectInputs
    fluidRow(
        column(4,
               selectInput("dtype",
                           "type:",
                           c("All",
                             0:1))
        ),
        column(4,
               selectInput("dmonth",
                           "Month:",
                           c("All",
                             1:12))
               
        ),
        column(4,
               selectInput("dday",
                           "Day:",
                           c("All",
                             1:31))
               
        ),
        column(4,
               selectInput("dyear",
                           "Year:",
                           c("All",
                             2010:2021))
               
        )
    ),
    # Create a new row for the table.
    DT::dataTableOutput("tableD")
)

fifth_tab = fluidPage(
    titlePanel("Basic DataTable"),
    
    # Create a new Row in the UI for selectInputs
    fluidRow(
        
        column(4,
               selectInput("eomonth",
                           "Open Month:",
                           c("All",
                             1:12))
               
        ),
        column(4,
               selectInput("eoday",
                           "Open Day:",
                           c("All",
                             1:31))
               
        ),
        column(4,
               selectInput("eoyear",
                           "Open Year:",
                           c("All",
                             2010:2021))
        ),
        column(4,
               selectInput("ecmonth",
                           "Close Month:",
                           c("All",
                             1:12))
               
        ),
        column(4,
               selectInput("ecday",
                           "Close Day:",
                           c("All",
                             1:31))
               
        ),
        column(4,
               selectInput("ecyear",
                           "Close Year:",
                           c("All",
                             2010:2021))
               
        )
    ),
    # Create a new row for the table.
    DT::dataTableOutput("tableE")
)
#--------------------------------------------------------------
Stat2_tab = fluidPage(
    title = "Delivery Data",

    sidebarLayout(
        sidebarPanel(
            dateRangeInput(inputId='dateRange', 
                           label='Select the date', 
                           start =min('2014-06-06'),
                           end = max('2017-06-19'),
                           min = min('2014-06-06'), 
                           max = max('2017-06-19'),
                           separator = " - ", format = 'dd/mm/yyyy',
                           startview = 'Year', weekstart = 1
            ),
            selectInput('Time_unit', 'Time_unit', choices = c('Week','Month','Year'), selected='Year'),
            selectInput('group_by', 'choose a group by option:', choices=c('NA','Customer.Size')),
            checkboxGroupInput("delivery","Choose a delivery status:", choices = c('Early','On Time', 'Late'), selected =c('Early','On Time', 'Late'))
            
        ),
        mainPanel(plotOutput("Delivery_Plot"))
    )
    
)
#---------------------------------------------------------------------
Stat_tab = fluidPage(
    titlePanel("Open date vs. Closed date"),
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("plot_cust_size","Customer Size",choices = c(0:3),selected = c(0:3))
        ),
        mainPanel(plotOutput("ovcplot"))
    )
)


ML_tab = fluidPage(
    title = "Machine Learning",
    sidebarLayout(
        sidebarPanel(
            selectInput("mlType",
                        "type:",
                        c(
                            "Linear Regression","Multilayer Perceptron", "Random Forest")),
            selectInput('yLabel',"Predict", vars[!vars %in% c("labels","Overall.Customer.ID")], selectize=TRUE),
            checkboxInput('removeNull',"Remove Null values?", value=TRUE),
            sliderInput(inputId = "tts",
                        label = "Train/test split:",
                        min = 0.1,
                        max = 0.9,
                        value = 0.8),
            checkboxGroupInput("myFeatureList", "Feature List:",
                               vars[!vars %in% c("labels","Overall.Customer.ID")], vars[!vars %in% c("labels","Overall.Customer.ID")],
            ),
            actionButton("runML", "Run")
        ),
        
        mainPanel(
            h4("Model Summary"),
            verbatimTextOutput("mlSummary"),
            h4("predictions"),
            verbatimTextOutput("mlPredict")
        )
    )
)



# this code creates the boby, and side bar


sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("File Input", icon = icon("folder"), tabName = "fileInput"),
        menuItem("Customer Info", icon = icon("archive"), tabName = "customerInfo"),
        menuItem("Table A", icon = icon("archive"), tabName = "tabA"),
        menuItem("Table B", icon = icon("archive"), tabName = "tabB"),
        menuItem("Table D", icon = icon("archive"), tabName = "tabD"),
        menuItem("Table E", icon = icon("archive"), tabName = "tabE"),
        menuItem("Statistics", icon = icon("dashboard"), tabName = "stat1"),
        menuItem("Statistics2", icon = icon("dashboard"), tabName = "stat2"),
        menuItem("Machine Learning", icon = icon("hdd"), tabName = "ML1")
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "fileInput",
                h2(file_input)
        ),
        tabItem(tabName = "customerInfo",
                h2(first_tab)
        ),
        
        tabItem(tabName = "tabA",
                h2(sec_tab)
        ),
        tabItem(tabName = "tabB",
                h2(third_tab)
        ),
        tabItem(tabName = "tabD",
                h2(fourth_tab)
        ),
        tabItem(tabName = "tabE",
                h2(fifth_tab)
        ),
        tabItem(tabName = "stat1",
                h2(Stat_tab)
        ),
        tabItem(tabName = "stat2",
                h2(Stat2_tab)
        ),
        tabItem(tabName = "ML1",
                h2(ML_tab)
        )
    )
)

ui = dashboardPage(
    dashboardHeader(title = "Team 1"),
    sidebar,
    body
)

server =  function(input, output, session) {

    #update inputs for each ML type
    observe({
        x <- input$mlType
        if(x=="Multilayer Perceptron"){
            updateCheckboxGroupInput(session, "myFeatureList",
                                     label = "Feature List:",
                                     choices = vars[!vars %in% c("labels","Overall.Customer.ID")],
                                     selected = vars[!vars %in% c("labels","Overall.Customer.ID")]
            )
            updateSelectInput(session, "yLabel",
                              label = "delivery score:",
                              choices = "labels",
                              selected = "labels"
            )
        }
        if(x=="Linear Regression"){
            updateCheckboxGroupInput(session, "myFeatureList",
                                     label = "Feature List:",
                                     choices = vars[!vars %in% c("labels","Overall.Customer.ID")],
                                     selected = vars[!vars %in% c("labels","Overall.Customer.ID")]
            )
            updateSelectInput(session, "yLabel",
                              label = "Predict:",
                              choices = vars[!vars %in% c("labels","Overall.Customer.ID")],
                              selected = vars[!vars %in% c("labels","Overall.Customer.ID")]
            )
        }
    })
    
    output$customerInfo <- DT::renderDataTable(
        DT::datatable({
            if(is.null(input$tblcust)) {
                data <- NULL
            }
            else {
                data <- read.csv(input$tblcust$datapath)
            }
            if (input$custsize != "All") {
                data <- data[data$Customer.Size == input$custsize,]
            }
            
            data
        })
    )

#------------------------------------------------------------------------------------    
    output$Delivery_Plot <- renderPlot({
        if(is.null(input$tblcust) ||is.null(input$tblb)){
            tblB_CustInfo <- NULL
        }
        else{
            customerInfo = read.csv(input$tblcust$datapath)
            tableB = read.csv(input$tblb$datapath)
            colnames(tableB)[colnames(tableB) == "Customer.Key"] = "Customer.ID"
            tblB_CustInfo = merge(customerInfo,tableB,by="Customer.ID")
            tblB_CustInfo$Customer.Size = as.factor(tblB_CustInfo$Customer.Size)
            tblB_CustInfo$Date = as.Date(tblB_CustInfo$Date)
            tblB_CustInfo <-tblB_CustInfo %>%
                            mutate(Week = format(Date, "%Y-%m-%U"), Month = format(Date, "%Y-%m"), Year = format(Date, "%Y"))
            
        }
        
        if(input$group_by != 'NA'){
            filtered <-tblB_CustInfo %>%
                        filter(Date >= input$dateRange[1] & Date <= input$dateRange[2]) %>%
                        group_by(as.factor(input$Time_unit),as.factor(input$group_by)) %>%
                        summarise( 
                            totalRequests = sum(Requested),
                            totalEarly = sum(Early),
                            totalLate = sum(Late),
                            totalOntime = sum(On.Time))
                        
        }
        else{
            filtered <- tblB_CustInfo %>%
                         filter(Date >= input$dateRange[1] & Date <= input$dateRange[2]) %>%
                         group_by(as.factor(input$Time_unit)) %>%
                         summarise(
                             totalRequests = sum(Requested),
                             totalEarly = sum(Early),
                             totalLate = sum(Late),
                             totalOntime = sum(On.Time))
                
        }
        p <-ggplot(data=filtered, aes(x=as.factor(input$Time_unit))) + geom_line(aes(y=totalEarly), color='blue',group =1 ) +
            geom_line(aes(y=totalLate), color='grey',group=1) + geom_line(aes(y=totalOntime), color='darkred',group=1) + 
            ylab(label = "count") + xlab("Time") 
            
        p + scale_x_date(limits = as.Date(c(input$dateRange[1],input$dateRange[2])))
        p
    })
    
#------------------------------------------------------------------------------------------------------------------------------------------
        output$ovcplot <- renderPlot({
        if(is.null(input$tblcust) || is.null(input$tble)) {
            tblE_CustInfo <- NULL
        }
        else{
            customerInfo = read.csv(input$tblcust$datapath)
            tableE = read.csv(input$tble$datapath)
            colnames(tableE)[colnames(tableE) == "Company"] = "Customer.ID"
            tblE_CustInfo = merge(customerInfo,tableE,by="Customer.ID")
            tblE_CustInfo$Customer.Size = as.factor(tblE_CustInfo$Customer.Size)
            tblE_CustInfo$Open = as.Date.factor(tblE_CustInfo$Open)
            tblE_CustInfo$Closed = as.Date.factor(tblE_CustInfo$Closed)
        }
        ggplot(tblE_CustInfo[tblE_CustInfo$Customer.Size == input$plot_cust_size,], aes(x = Open, y = Closed, shape = Customer.Size, color = Customer.Size)) + geom_point()
    })
    
    output$tableA <- DT::renderDataTable(DT::datatable({
        if(is.null(input$tbla)) {
            data <- NULL
        }
        else {
            data <- read.csv(input$tbla$datapath)
        }
        if (input$atype != "All") {
            data <- data[data$Type == input$atype,]
        }
        if (input$aday != "All") {
            data <- data[data$Day == input$aday,]
        }
        if (input$amonth != "All") {
            data <- data[data$Month == input$amonth,]
        }
        if (input$ayear != "All") {
            data <- data[data$Year == input$ayear,]
        }
        
        data
    }))
    
    output$tableB <- DT::renderDataTable(DT::datatable({
        if(is.null(input$tblb)) {
            data <- NULL
        }
        else {
            data <- read.csv(input$tblb$datapath)
        }
        if (input$bday != "All") {
            data <- data[data$Day == input$bday,]
        }
        if (input$bmonth != "All") {
            data <- data[data$Month == input$bmonth,]
        }
        if (input$byear != "All") {
            data <- data[data$Year == input$byear,]
        }
        
        data
    }))
    
    output$tableD <- DT::renderDataTable(DT::datatable({
        if(is.null(input$tbld)) {
            data <- NULL
        }
        else {
            data <- read.csv(input$tbld$datapath)
        }
        if (input$dtype != "All") {
            data <- data[data$Type == input$dtype,]
        }
        if (input$dday != "All") {
            data <- data[data$Day == input$dday,]
        }
        if (input$dmonth != "All") {
            data <- data[data$Month == input$dmonth,]
        }
        if (input$dyear != "All") {
            data <- data[data$Year == input$dyear,]
        }
        
        data
    }))
    
    output$tableE <- DT::renderDataTable(DT::datatable({
        if(is.null(input$tble)) {
            data <- NULL
        }
        else {
            data <- read.csv(input$tble$datapath)
        }
        if (input$eoday != "All") {
            data <- data[data$Open_Day == input$eoday,]
        }
        if (input$eomonth != "All") {
            data <- data[data$Open_Month == input$eomonth,]
        }
        if (input$eoyear != "All") {
            data <- data[data$Open_Year == input$eoyear,]
        }
        if (input$ecday != "All") {
            data <- data[data$Closed_Day == input$ecday,]
        }
        if (input$ecmonth != "All") {
            data <- data[data$Closed_Month == input$ecmonth,]
        }
        if (input$ecyear != "All") {
            data <- data[data$Closed_Year == input$ecyear,]
        }
        
        data
    }))
    
    complete_features_list <- 
        eventReactive(input$run_feature_list, {
            
            tableA = read.csv(input$tbla$datapath) 
            tableB = read.csv(input$tblb$datapath)
            tableD = read.csv(input$tbld$datapath)
            tableE = read.csv(input$tble$datapath)
            customerInfo = read.csv(input$tblcust$datapath)
            
            colnames(tableA)[colnames(tableA) == "CustomerLocation"] = "Customer.ID"
            colnames(tableB)[colnames(tableB) == "Customer.Key"] = "Customer.ID"
            colnames(tableD)[colnames(tableD) == "Customer"] = "Customer.ID"
            colnames(tableE)[colnames(tableE) == "Company"] = "Customer.ID"
            
            by_cust_id <- tableB %>% group_by(Customer.ID)
            tbl_b_features <- by_cust_id %>% summarise(
                totalRequests = sum(Requested),
                totalEarly = sum(Early),
                totalLate = sum(Late),
                totalOntime = sum(On.Time)
            )
            
            for_classification_build <- merge(customerInfo,tbl_b_features,by="Customer.ID") %>% select(Overall.Customer.ID,Customer.Size,totalRequests,totalEarly,totalLate,totalOntime)
            
            Overall_uncalculated_scores <- for_classification_build  %>% 
                group_by(Overall.Customer.ID)  %>% summarise(
                    totalRequests = sum(totalRequests),
                    totalEarly = sum(totalEarly),
                    totalLate = sum(totalLate),
                    totalOntime = sum(totalOntime)
                )
            
            calculated_scores = within(Overall_uncalculated_scores, delivery_score <- (((2*totalEarly+1*totalOntime)-(3*totalLate))/totalRequests)+3) %>% select(Overall.Customer.ID,delivery_score)
            
            
            calculated_scores$labels = split_var(calculated_scores$delivery_score, n= 5)
            
            by_date_custID <- tableA %>% group_by(Customer.ID, Date)
            by_custID <- by_date_custID %>% summarise(tblA_total_trans_day_type1=sum(Type==1),tblA_total_trans_day_type0=sum(Type==0))
            
            tblA_features <- by_custID %>% summarise(tblA_average_trans_day_type1=mean(tblA_total_trans_day_type1),tblA_average_trans_day_type0=mean(tblA_total_trans_day_type0))
            
            all_feature_list = merge(tblA_features, tbl_b_features, by="Customer.ID", all=TRUE)
            
            by_date_custID <- tableD %>% group_by(Customer.ID, Date)
            by_custID <- by_date_custID %>% summarise(tblD_total_trans_day_type1=sum(Type==1),tblD_total_trans_day_type0=sum(Type==0))
            tblD_features <- by_custID %>% summarise(tblD_average_trans_day_type1=mean(tblD_total_trans_day_type1),tblD_average_trans_day_type0=mean(tblD_total_trans_day_type0))
            
            all_feature_list = merge(tblD_features, all_feature_list, by="Customer.ID", all=TRUE)
            
            by_custID <- tableE %>% group_by(Customer.ID)
            tblE_features = by_custID %>% summarize(average_contract_length = mean(as.numeric(Closed)-as.numeric(Open)), first_contract_date = min(as.numeric(Open)))
            
            all_feature_list = merge(tblE_features, all_feature_list, by="Customer.ID", all=TRUE)
            
            by_custID = merge(all_feature_list,customerInfo,by="Customer.ID",all=TRUE) %>% group_by(Overall.Customer.ID)
            
            complete_features_list = by_custID %>% summarize(average_size=mean(Customer.Size),
                                                             average_contract_length=mean(average_contract_length),
                                                             tblA_average_trans_day_type1=mean(tblA_average_trans_day_type1),
                                                             tblA_average_trans_day_type0=mean(tblA_average_trans_day_type0),
                                                             tblD_average_trans_day_type1=mean(tblA_average_trans_day_type1),
                                                             tblD_average_trans_day_type0=mean(tblA_average_trans_day_type0),
                                                             total_requests=sum(totalRequests),
                                                             total_early=sum(totalEarly),
                                                             total_late=sum(totalLate),
                                                             total_ontime=sum(totalOntime),
                                                             first_contract_date=min(first_contract_date)
            )
            
            merge(complete_features_list,calculated_scores,by="Overall.Customer.ID",all=TRUE)
            
            
            
        })
    
    ml_type <-  eventReactive(input$runML, {
        if(input$yLabel %in% input$myFeatureList){
            data_0 = complete_features_list()[,input$myFeatureList]
        }
        else {
            data_0 = complete_features_list()[,append(input$myFeatureList,input$yLabel)]
        }
        
        if(input$removeNull) {
            data_0 = tidyr::drop_na(data_0)
        }
        split_on = floor(input$tts*nrow(data_0))
        set.seed(123)
        train_ind = sample(seq_len(nrow(data_0)), size = split_on)
        data_0 = data_0[train_ind,]
        
        switch(input$mlType,
               "Linear Regression" = lm(get(input$yLabel) ~. -get(input$yLabel), data = data_0),
               "Multilayer Perceptron" = mlp(x=data_0[,input$myFeatureList], decodeClassLabels(data_0[,input$yLabel]), size=4, maxit=500),
               "Random Forest" = randomForest(get(input$yLabel) ~. -get(input$yLabel), data = data_0)
        )
    }, ignoreNULL = FALSE)
    
    output$mlSummary <- renderPrint({
        ml_build = ml_type()
        summary(ml_build)
    })
    
    output$mlPredict <- renderPrint({
        
        ml_build = ml_type()
        
        
        if(input$mlType=="Multilayer Perceptron") {
            data_0 = complete_features_list()[,append(input$myFeatureList,input$yLabel)]
            if(input$removeNull) {
                data_0 = tidyr::drop_na(data_0)
            }
            split_on = floor(input$tts*nrow(data_0))
            set.seed(123)
            train_ind = sample(seq_len(nrow(data_0)), size = split_on)
            data_0 = data_0[-train_ind,]
            #summary(data_0)
            pred_list = predict(ml_build, newdata = data_0[,input$myFeatureList])
            outputp = confusionMatrix(data_0[,input$yLabel],pred_list)
        }
        
        if(input$mlType=="Random Forest") {
            data_0 = complete_features_list()[,append(input$myFeatureList,input$yLabel)]
            if(input$removeNull) {
                data_0 = tidyr::drop_na(data_0)
            }
            split_on = floor(input$tts*nrow(data_0))
            set.seed(123)
            train_ind = sample(seq_len(nrow(data_0)), size = split_on)
            data_0 = data_0[-train_ind,]
            #summary(data_0)
            pred_list = predict(ml_build, newdata = data_0[,input$myFeatureList])
            outputp = confusionMatrix(data_0[,input$yLabel],pred_list)
        }
        
        if(input$mlType=="Linear Regression") {
            
            if(input$yLabel %in% input$myFeatureList){
                data_0 = complete_features_list()[,input$myFeatureList]
            }
            else {
                data_0 = complete_features_list()[,append(input$myFeatureList,input$yLabel)]
            }
            
            
            if(input$removeNull) {
                data_0 = tidyr::drop_na(data_0)
            }
            split_on = floor(input$tts*nrow(data_0))
            set.seed(123)
            train_ind = sample(seq_len(nrow(data_0)), size = split_on)
            
            data_0 = data_0[-train_ind,]
            #summary(data_0)
            outputp = predict(ml_build, newdata = data_0, interval="confidence")
        }
        outputp
    })
    
}





shinyApp(ui, server)
