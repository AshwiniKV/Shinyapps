library(shiny)
library(Hmisc)
library(haven)
library(tables)
library(knitr)
library(stargazer)
library(mapproj)
library(grid)
library(ggmap)
library(gridExtra)
library(data.table)

data <- read_dta("GSF_education.dta", encoding=NULL) 
#names(data)
data1<-data[,c(-(26:47), -50, -51,-52)]
data1<-data1[,-(1:2)]
lat<-c(52.70076, 52.13066, 56.49067,  54.64237)
lon<-c(-1.249077,  -3.783712, -4.202646, -6.592054)
data1$lon<-lon[unlist(data1[,"countr"])]
data1$lat<-lat[unlist(data1[,"countr"])]

data2<-data1[complete.cases(data1),]
data21<-data2[,-c(19,26, 27)]
data3<-data2[,c("sex", "compu", "internet", "place", "hand", "ethn6", "countr", "sibl_fl", "grand", "poor", "class","econstat", "lang_home")]
data3<-lapply(data2, as.factor)
data4<-data2[,c("tv", "games", "social", "engl", "mths", "scien", "phyed", "hmwk", "help", "best", "inter")]
data5<-lapply(data4, as.numeric)  
data6<-append(data3, data5)

uk_center = as.numeric(geocode("United Kingdom"))

UKMap = ggmap(get_googlemap(center=uk_center, scale=2, zoom=6, color = "bw"), extent="normal", color = "bw")
data2<-data.frame(data2)

Mode <- function(x) {
  ux <- table(x)
  as.numeric(names(ux[which.max(ux)]))
}

#ESWI
ColAttr <- function(x, attrC, ifIsNull) {
  # Returns column attribute named in attrC, if present, else isNullC.
  atr <- attr(x, attrC, exact = TRUE)
  atr <- if (is.null(atr)) {ifIsNull} else {atr}
  atr
}

AtribLst <- function(df, attrC, isNullC){
  # Returns list of values of the col attribute attrC, if present, else isNullC
  lapply(df, ColAttr, attrC=attrC, ifIsNull=isNullC)
}

variables <- AtribLst(data1, attrC="labels", isNullC=NA)

ui <- fluidPage(
  
  # Application title
  titlePanel("Millennium Cohort study"),
  
  tabsetPanel(
    tabPanel("Distribution", fluid = TRUE,
             sidebarPanel(selectInput("selection", "Covariates:", choices = names(data2), selected = "engl"), plotOutput("Explanation"), width = 3),
             mainPanel(plotOutput("distPlot"), width =9)
    ),
    
    tabPanel("Comparison", fluid = TRUE,
             sidebarPanel(selectInput("cov1", "Covariate 1", choices = names(data21)), selectInput("cov2", "Covariate 2", choices = names(data21)), verbatimTextOutput("Explanation1"), verbatimTextOutput("Explanation2")),
             mainPanel(plotOutput("compPlot"))),
    
    tabPanel("Regression", fluid = TRUE,
             sidebarPanel(
                          selectInput("select2", "Core Outcomes:", choices = c("engl", "mths", "scien", "phyed")),
                          actionButton("bivar", "Bivariate"),
                          actionButton("mulvar", "Multivariate"),
                          checkboxGroupInput("select3", "Covariates:", choices = c("sex", "countr","sibl_fl", "poor", "hand", "games", "compu", "internet", "social", "hmwk",
                                                                      "help", "place","inter", "grand","ethn6","best","tv", "class", "edu_par", "econstat", "lang_home"), selected = "sex")
                          ),
              splitLayout(cellWidths = c("50%", "50%"),
              mainPanel(htmlOutput("regression1"),
                       htmlOutput("regression2")))
             ),
    tabPanel("Maps", fluid = TRUE,
             sidebarPanel(selectInput("select", "Covariates", choices = names(data2)[-c(19, 26, 27)], selected = "tv"),  plotOutput("Explanation5"), width = 3),
             mainPanel(plotOutput("ukplot"), width = 9)
             )
       )
  )


server<- function(input, output) {
    
  output$distPlot<-renderPlot({
    barplot(prop.table(table(data6[input$selection])), main = label(data1[input$selection]), ylim= c(0, 1), beside = TRUE, cex.main = 1.5, cex.lab = 1.5)
    mp<-barplot(prop.table(table(data6[input$selection])), main = label(data1[input$selection]), ylim= c(0, 1),beside = TRUE, plot = FALSE)
    text(x=mp,y=prop.table(table(data6[input$selection])), cex = 1.5,labels=paste0(round(prop.table(table(as.numeric(unlist(data1[input$selection])))),2)*100, "%"), pos=3, adj = 2,xpd=NA)
  }, height = 600)

  
  output$Explanation<-renderPlot({
    id<-which(names(data1) == input$selection)
    if(!is.na(variables[id])){
      grid.draw(tableGrob(data.frame(variables[id])))}
  })
  
  output$compPlot<-renderPlot({
    counts<-as.data.frame(table(unlist(data21[,input$cov1]), unlist(data21[,input$cov2])))
    names(counts)<-c(input$cov1, input$cov2, "frequency")
    counts1<-data.table(counts)
    counts1[, percen := sum(frequency), by=eval(input$cov1)]
    counts1[, percen := paste0(round(frequency/percen*100, 0), "%")]
    ggplot(counts1, aes_string(x = input$cov1, y = "frequency", fill = input$cov2,label = "percen"))+
      geom_bar(stat = "identity") + ggtitle(paste0(label(data[,input$cov1]), " and ", label(data[, input$cov2]) ))+
      geom_text(size = 7, position = position_stack(vjust = 0.5))+theme(axis.text=element_text(size=12, face = "bold"),
                                                                        axis.title=element_text(size=14,face="bold"))+ theme(plot.title = element_text(size=22))
    
  }, height = 800, width = 1000)
  
  output$Explanation1<-renderPrint({
    id<-which(names(data1) == input$cov1)
    if(!is.na(variables[id])){
      data.frame(variables[id])}
  })
  
  output$Explanation2<-renderPrint({
    id<-which(names(data1) == input$cov2)
    if(!is.na(variables[id])){
      data.frame(variables[id])}
  })
  
  output$table<-renderPlot({
  grid.draw(tableGrob(table(unlist(data2[,input$cov1]), unlist(data2[,input$cov2]))))
  }, height = 250)
  
  # regression formula
  regFormula <- reactive({
    as.formula(paste(input$select2, '~', c(input$select3)))
  })
  
  datasetInput <- reactive({
    data2[,c(input$select2, input$select3)]
  })
  
  #Multi- Regression 
  model1<-eventReactive(input$mulvar,{
    lm(reformulate(input$select3, c(input$select2)), data =datasetInput())
    })

  
  # bivariate model
  model2 <- eventReactive(input$bivar,{
    lm(regFormula(), data = datasetInput())
  })
  
  output$regression1 <- renderUI({
    HTML(stargazer(model1(),  type = "html", dep.var.labels = input$select2, align = TRUE,style = "default"))
  })
  
  output$regression2 <- renderUI({
    HTML(stargazer(model2(),  type = "html", dep.var.labels = input$select2, align = TRUE,style = "default"))
  })
  
  output$Explanation5<-renderPlot({
    id<-which(names(data2) == input$select)
    if(!is.na(variables[id])){
      grid.draw(tableGrob(data.frame(variables[id])))}
  })
  
  output$ukplot<-renderPlot({
    
    data2$id<-NA
    data2$id.name<-NA
    id1<-Mode(data2[which(data2[,"countr"] == 1),input$select])
    data2[which(data2[,"countr"] == 1), "id"]<-id1
    data2[which(data2[,"countr"] == 1), "id.name"]<-names(which(unlist(variables[input$select])  == id1))
    
    id2<-Mode(data2[which(data2[,"countr"] == 2),input$select])
    data2[which(data2[,"countr"] == 2), "id"]<-id2
    data2[which(data2[,"countr"] == 2), "id.name"]<-names(which(unlist(variables[input$select])  == id2))
    
    id3<-Mode(data2[which(data2[,"countr"] == 3),input$select])
    data2[which(data2[,"countr"] == 3), "id"]<-id3
    data2[which(data2[,"countr"] == 3), "id.name"]<-names(which(unlist(variables[input$select])  == id3))
    
    id4<-Mode(data2[which(data2[,"countr"] == 4),input$select])
    data2[which(data2[,"countr"] == 4), "id"]<-id4
    data2[which(data2[,"countr"] == 4), "id.name"]<-names(which(unlist(variables[input$select])  == id4))
    
    UKMap + geom_point(aes(x = lon, y = lat), data = data2, size = 40,alpha = 0.002, col = (as.numeric(as.factor(data2$id))+1))+ggtitle(label(data2[input$select]))+
      geom_text(aes(x = lon, y = lat, label = id, size = 40),show.legend = F, data= data2, alpha = 1, color = "black") +theme(plot.title = element_text(hjust = 0.5))+
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y= element_blank())+
      theme(plot.title = element_text(size=22))
    
}, height = 800, width = 900)

}
shinyApp(ui = ui, server = server)

