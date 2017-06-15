library(haven)
library(Hmisc)
library(ggplot2)
library(gridExtra)
library(grid)
library(data.table)
library(stargazer)

data <- read_dta("2014 Scottish Social Attitudes Short.dta", encoding=NULL)
data<-data[,-10]
data1<-data[,-c(1, 6)]
data2<-data[complete.cases(data),]
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
variables <- AtribLst(data, attrC="labels", isNullC=NA)

my_max <- 5

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Scottish Voting attitudes"),
  
  tabsetPanel(
    tabPanel("Distribution", fluid = TRUE,
             sidebarPanel(selectInput("selection", "Covariates:", choices = names(data), selected = "education"),   plotOutput("Explanation"), width = 3),
             mainPanel(plotOutput("distPlot"), 
                       width = 9)
    ),
    
    tabPanel("Comparison", fluid = TRUE,
             sidebarPanel(selectInput("cov1", "Covariate 1", choices = names(data1)), selectInput("cov2", "Covariate 2", choices = names(data1)), verbatimTextOutput("Explanation1"), verbatimTextOutput("Explanation2")),
             mainPanel(plotOutput("compPlot"))),
    
    
    tabPanel("Generalised Linear Model", fluid = TRUE,
             sidebarPanel(checkboxGroupInput("show_vars", "Covariates:", selected ="education", choices = names(data)[-11])),
             mainPanel(uiOutput("glmodel")
             )
    ),
    
    tabPanel("Prediction probability", fluid = TRUE,
             sidebarPanel(actionButton("goButton", "Mean values"), selectInput("plotvalue", "Covariates", choices = c("age", "gender", "education", "scot", "income")), verbatimTextOutput("Exp1"),
                          verbatimTextOutput("Exp2"), verbatimTextOutput("Exp3"),verbatimTextOutput("Exp4"), verbatimTextOutput("Exp5"), width = 3),
             mainPanel(verbatimTextOutput("predprob"),
                       dataTableOutput("regoutput")
             )
      )
    )
)
  

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  selectedData<-reactive({
    as.numeric(unlist(data[,input$selection]))
  })
  
  output$distPlot <- renderPlot({
    plot.data<-round(selectedData(),2)
    barplot(prop.table(table(plot.data)), main = label(data[,input$selection]), ylim = c(0, 1), xlab = " ", cex.axis = 2, cex.main = 2)
    mp<-barplot(prop.table(table(plot.data)), main = label(data[,input$selection]), ylim= c(0, 1),beside = TRUE, plot = FALSE)
    text(x=mp,y=prop.table(table(data[,input$selection])),labels=paste0(round(prop.table(table(as.numeric(unlist(data[,input$selection])))),2)*100, "%"), pos=3, adj = 2,xpd=NA)
  }, height = 600)
  
  output$Explanation<-renderPlot({
    id<-which(names(data) == input$selection)
    if(!is.na(variables[id])){
      grid.draw(tableGrob(data.frame(variables[id])))}
  })
  
  output$indplot<-renderPlot({
    plot(data[,input$indiv], data[,"vote"])
  })
  
  output$Explanation1<-renderPrint({
    variables <- AtribLst(data1, attrC="labels", isNullC=NA)
    id<-which(names(data1) == input$cov1)
    if(!is.na(variables[id])){
      data.frame(variables[id])}
  })
  
  output$Explanation2<-renderPrint({
    variables <- AtribLst(data1, attrC="labels", isNullC=NA)
    id<-which(names(data1) == input$cov2)
    if(!is.na(variables[id])){
      data.frame(variables[id])}
  })
  
  output$compPlot<-renderPlot({
    counts<-as.data.frame(table(unlist(data1[,input$cov1]), unlist(data1[,input$cov2])))
    names(counts)<-c(input$cov1, input$cov2, "frequency")
    counts1<-data.table(counts)
    counts1[, percen := sum(frequency), by=eval(input$cov1)]
    counts1[, percen := paste0(round(frequency/percen*100, 0), "%")]
    ggplot(counts1, aes_string(x = input$cov1, y = "frequency", fill = input$cov2,label = "percen"))+
      geom_bar(stat = "identity") + ggtitle(paste0(label(data[,input$cov1]), " and ", label(data[, input$cov2]) ))+
      geom_text(size = 7, position = position_stack(vjust = 0.5))+theme(axis.text=element_text(size=12, face = "bold"),
                                                                        axis.title=element_text(size=14,face="bold"))+ theme(plot.title = element_text(size=22))
    
  }, height = 800, width = 1000)
  
  observe({
    if(length(input$show_vars) > my_max)
    {
      updateCheckboxGroupInput(session, "show_vars", selected= tail(input$show_vars,my_max))
    }
  })
  
  selectedData2<-reactive({
    data[, c("vote", input$show_vars), drop = FALSE]
  })
  
  output$glmodel<-renderUI({
    model.1 <- glm(vote ~., 
                   family=binomial(link="logit"), data = selectedData2())
    HTML(stargazer(model.1,  type = "html", dep.var.labels = input$select2, align = TRUE,style = "default"))
  })
  
  df<-eventReactive(input$goButton, {
    colMeans(data[, c("age", "education", "gender", "scot","income")], na.rm= TRUE)
  })
  
  output$predprob<-renderPrint({
    df()
  })
  
###########
  output$regoutput<-renderDataTable({
    model.1 <- glm(vote ~ age + education + gender + scot+income, 
                           family=binomial(link="logit"), data=data)
    sdata<-data
    mean.covar<-apply(sdata,2,mean,na.rm=T)
    model.mean.covar<-mean.covar[names(mean.covar)%in%names(model.1$coefficients)[-1]]
    temp<-!names(model.mean.covar)%in%input$plotvalue
    model.mean.covar<-model.mean.covar[temp]
    ind<-which(names(data)==input$plotvalue)
    r<-sample(unlist(data[!is.nan(unlist(data[,input$plotvalue])), input$plotvalue]), 100)
    newdata1<-cbind(r,matrix(model.mean.covar,100,length(model.mean.covar),byrow=T))
    newdata1<-as.data.frame(newdata1)
    names(newdata1)<-c(input$plotvalue, names(model.mean.covar))
    newdata1a <- cbind(newdata1, predict(model.1, newdata = newdata1, type="link", se=TRUE))
    newdata1a <- within(newdata1a, {
      PredictedProb <- plogis(fit)})
    round(newdata1a,2)
  })
 
  output$Exp1<-renderPrint({
    variables <- AtribLst(data, attrC="labels", isNullC=NA)
    id<-which(names(data) == "education")
    if(!is.na(variables[id])){
      data.frame(variables[id])}
  })
  
  output$Exp2<-renderPrint({
    variables <- AtribLst(data, attrC="labels", isNullC=NA)
    id<-which(names(data) == "employmentdum")
    if(!is.na(variables[id])){
      data.frame(variables[id])}
  })
  
  output$Exp3<-renderPrint({
    variables <- AtribLst(data, attrC="labels", isNullC=NA)
    id<-which(names(data) == "scot")
    if(!is.na(variables[id])){
      data.frame(variables[id])}
  })
  
  output$Exp4<-renderPrint({
    variables <- AtribLst(data, attrC="labels", isNullC=NA)
    id<-which(names(data) == "income")
    if(!is.na(variables[id])){
      data.frame(variables[id])}
  })
  
  output$Exp5<-renderPrint({
    variables <- AtribLst(data, attrC="labels", isNullC=NA)
    id<-which(names(data) == "gender")
    if(!is.na(variables[id])){
      data.frame(variables[id])}
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

