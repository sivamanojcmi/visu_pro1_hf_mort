

library(shiny)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(readr)
library(readxl)

## reading csv file and converting into data frame
#setwd("C:/Users/win/Desktop/Data_Science/1st_sem/PBSR/Rstudio_handson/shiny_project_dire/visu_shiny_pro")
hf_mort=read_csv("hf_mortality_dataset.csv")

total_list=colnames(hf_mort)

num_list=c("age","heart rate","BMI","Systolic blood pressure","Diastolic blood pressure",
           "Respiratory rate","temperature","SP O2","Urine output","hematocrit","RBC",
           "MCH","MCHC","MCV","RDW","Leucocyte","Platelets","Neutrophils","Basophils",
           "Lymphocyte","PT","INR","NT-proBNP","Creatine kinase","Creatinine","Urea nitrogen",
           "glucose","Blood potassium","Blood sodium","Blood calcium","Chloride","Anion gap",
           "Magnesium ion","PH","Bicarbonate","Lactic acid","PCO2","EF")
cat_list=c("gender","hypertensive","atrialfibrillation","CHD with no MI","diabetes","deficiencyanemias",
           "depression","Hyperlipemia","Renal failure","COPD")

bar_perc_categorical2 <- function(data,var_req){
  data <- data %>% drop_na("outcome",var_req)
  k=grep(var_req,colnames(data))
  outcome=c(rep("Total",2),rep("Yes",2),rep("No",2))
  cat_var=rep(c("Survived","Died"),3)
  survived=data[data$outcome==0,]
  died=data[data$outcome==1,]
  survived_yes=survived[survived[k]==0,]
  died_yes=died[died[k]==0,]
  total=nrow(data)
  total_sur=nrow(survived)
  total_died=total-total_sur
  sur_yes=nrow(survived_yes)
  sur_no=total_sur-sur_yes
  died_yes=nrow(died_yes)
  died_no=total_died-died_yes
  total_yes=sur_yes+died_yes
  total_no=sur_no+died_no
  
  data_all=round(c(total_sur*100/total,total_died*100/total,
                   sur_yes*100/total_yes,died_yes*100/total_yes,
                   sur_no*100/total_no,died_no*100/total_no),2)
  df=data.frame(outcome,cat_var,data_all)
  ggplot(df,aes(fill=cat_var,y=data_all,x=outcome,label=paste0(data_all,"%")))+
    geom_bar(position="stack",stat="identity")+
    ggtitle(paste(" impact of ",var_req," on mortality"))+
    xlab(var_req)+
    ylab("percentage of people")+
    scale_x_discrete(limits = outcome)+
    geom_text(fontface = "bold", vjust = 1.5,
              position = position_stack(0.9), size = 5.8)+
    scale_fill_discrete(name = var_req)+
    theme(plot.title = element_text(hjust = 0.5,size=18,face="bold"), 
          axis.title.x = element_text(face="bold", colour="black", size = 15),
          axis.title.y = element_text(face="bold", colour="black", size = 15),
          legend.title = element_text(face="bold", size = 14))
}

bar_perc_gender <- function(data,var_req){
  data <- data %>% drop_na("outcome",var_req)
  k=grep(var_req,colnames(data))
  outcome=c(rep("Total",2),rep("Female",2),rep("Male",2))
  cat_var=rep(c("Survived","Died"),3)
  survived=data[data$outcome==0,]
  died=data[data$outcome==1,]
  survived_yes=survived[survived[k]==1,]
  died_yes=died[died[k]==1,]
  total=nrow(data)
  total_sur=nrow(survived)
  total_died=total-total_sur
  sur_yes=nrow(survived_yes)
  sur_no=total_sur-sur_yes
  died_yes=nrow(died_yes)
  died_no=total_died-died_yes
  total_yes=sur_yes+died_yes
  total_no=sur_no+died_no
  
  data_all=round(c(total_sur*100/total,total_died*100/total,
                   sur_yes*100/total_yes,died_yes*100/total_yes,
                   sur_no*100/total_no,died_no*100/total_no),2)
  df=data.frame(outcome,cat_var,data_all)
  ggplot(df,aes(fill=cat_var,y=data_all,x=outcome,label=paste0(data_all,"%")))+
    geom_bar(position="stack",stat="identity")+
    ggtitle(paste(" impact of gender on mortality"))+
    xlab("Gender")+
    ylab("percentage of people")+
    scale_x_discrete(limits = outcome)+
    geom_text(fontface = "bold", vjust = 1.5,
              position = position_stack(.9), size = 5.8)+
    scale_fill_discrete(name = var_req)+
    theme(plot.title = element_text(hjust = 0.5,size=18,face="bold"), 
          axis.title.x = element_text(face="bold", colour="black", size = 15),
          axis.title.y = element_text(face="bold", colour="black", size = 15),
          legend.title = element_text(face="bold", size = 14))
}


hist_cont_var <- function(data,var_req){
  
  data <- data %>% drop_na(outcome)
  data <- data %>% drop_na(var_req)
  hf_dies=data.frame()
  k=grep(var_req,colnames(data))
  j=1
  for (i in 1:nrow(data)){
    if(data[i,3]==1){
      if (data[i,5]==1){
        hf_dies[j,1]="Female"
      }
      else{
        hf_dies[j,1]="Male"
      }
      hf_dies[j,2]=data[i,k]
      j=j+1
    }
  }
  hist_values=hf_dies[,2]
  hist_fem=hf_dies[hf_dies$V1=="Female",]
  hist_fem=hist_fem[,2]
  hist_male=hf_dies[hf_dies$V1=="Male",]
  hist_male=hist_male[,2]
  ggplot(hf_dies,aes(hist_values))+
    geom_histogram( aes(y=..density..),fill="#ff930f", color="Brown",bins=25)+
    geom_density(alpha=.5,fill="#fff95b")+
    theme_get()+
    geom_label(aes(median(hist_values),0.0), label = "Median", show.legend = TRUE)+
    geom_vline(aes(xintercept = median(hist_values)),
               color = "#ff0000", size = 1.25, linetype="dashed") +
    xlab(var_req)+
    ylab("proportion of people")+
    scale_x_continuous(breaks = round(c(seq(min(hist_values), 
                                            max(hist_values), length.out=25),0)))+
    ggtitle(paste(var_req," histogram for deceased patients"))+
    theme(plot.title = element_text(hjust = 0.5,size=18,face="bold"), 
          axis.title.x = element_text(face="bold", colour="black", size = 15),
          axis.title.y = element_text(face="bold", colour="black", size = 15))
}

bivariate <- function(data,cat_var,num_var,survival){
  
  data <- data %>% drop_na("outcome",cat_var,num_var)
  data <- as.data.frame(data)
  if (survival=="Yes"){
    data=data[data$outcome==0,]
  }
  if (survival=="No"){
    data=data[data$outcome==1,]
  }
  k_num=grep(num_var,colnames(data))
  k_cat=grep(cat_var,colnames(data))
  hf_dies_num=data[,k_num]
  legnd=c("Yes","No")
  if (cat_var == "gender"){
    legnd=c("Female","Male")
  }
  ggplot(data,aes(x=factor(data[,k_cat]), y=data[,k_num], fill=factor(data[,k_cat])))+
    geom_boxplot()+
    theme_get()+
    xlab(cat_var)+
    ylab(num_var)+
    scale_x_discrete(labels=legnd)+
    scale_fill_discrete(labels=legnd)+
    labs(fill=cat_var)+
    ggtitle(paste("Box plot for ",num_var," and ",cat_var))+
    theme(plot.title = element_text(hjust = 0.5,size=18,face="bold"), 
          axis.title.x = element_text(face="bold", colour="black", size = 15),
          axis.title.y = element_text(face="bold", colour="black", size = 15),
          legend.title = element_text(face="bold", size = 14))
}

box_cts_BMI <- function(data,var_req){
  data_heart = data %>% drop_na(var_req,"outcome","BMI")
  data_heart_BMI=data.frame()
  k=grep(var_req,colnames(data_heart))
  j=1
  for (i in 1:nrow(data_heart)){
    if (data_heart[i,3]==1){
      data_heart_BMI[j,1]=round(data_heart[i,k],2)
      if (data_heart[i,6]<18.5){
        data_heart_BMI[j,2]="Under weight"
      }
      else if (data_heart[i,6]<25){
        data_heart_BMI[j,2]="Normal weight"
      }
      else if (data_heart[i,6]<30){
        data_heart_BMI[j,2]="Over weight"
      }
      else{
        data_heart_BMI[j,2]="Obese"
      }
      j=j+1
    }
  }
  ggplot(data_heart_BMI,aes(x=factor(V2),y=data_heart_BMI[,1],fill=factor(V2)))+
    geom_boxplot()+
    xlab("BMI Classification")+
    xlim(c("Under weight","Normal weight","Over weight","Obese"))+
    ylab(var_req)+
    ggtitle(paste("Box plot of ",var_req,"for deceased patients on BMI classification"))+
    stat_summary(fun=mean,geom="point",size=2,color="white",shape=15)+
    theme( legend.position = "none" )+
    theme(plot.title = element_text(hjust = 0.5,size=18,face="bold"), 
          axis.title.x = element_text(face="bold", colour="black", size = 15),
          axis.title.y = element_text(face="bold", colour="black", size = 15))
}

Intro1 <- h4(p("The project's primary goal is to determine the impact of various 
              factors on in-hospital mortality rate.",align="justify",style="line-height: 27px;"))

Intro2 <-h4(p("The state of being mortal is referred to as mortality. 
              In the medical field, mortality is also known as the death rate. 
              In this data set, in-hospital mortality is the death rate among 
              heart failure patients admitted to the ICU.",br(),br(),
              "The in-hospital mortality rate is 13.52% for the given data and it is varying based on
              categoriacl variable behavior. So we are analysing the variation of in-hospital mortality rate
              based on categorical variable in this analysis",br(),br(),
              "Univariate analysis is classified by type of variable as Categorical and Numerical Varible.
              For categorical varibales stacked bar plot used mainly for 3 categroies of
              patient condition i.e.,
              1.Patient Survived
              2.Patient Died
              3.All patients
              and for numerical variable
              histogram with densityplot is used for deceased patients only.",br(),br(),
              "In Bivariate annalysis Box plot is used for analysis of 2 selected variables. The bivariate analysis 
              can be done for all patients or deceased patients or survived patients based on user selection.",br(),br(),
              "Analysing the Distribution of numerical variables based on BMI category in Analysis (BMI Classification) tab",
              align="justify",style="line-height: 27px;"))


data_description1 <- h4(p("The data was compiled 
              from hospitals where heart failure patients were admitted to the 
              intensive care unit for treatment. Patients with a diagnosis of 
              Heart Failure, identified by manual review of ICD-9 codes, and 
              whose age is more than 15 years at the time of ICU admission were 
              included in the study. ",br(),br(),"        The dataset conatains 51 columns and 1177 rows where as
              in this analysis we have used 49 columns out of which 11 columns are categorical
              variables and 38 columns are numerical variables. In the outcome variable 
              0 means patient survived and 1 means patient died, In gender category 1 means 
              Female and 2 means Male and in all other categorical variables 0 means Yes
              and 1 means No. ",br(),br(),
              "The data have some NA values so the data is preprocessed dynamically based on variable selected by user.
              The following are the attributes used in this analysis",align="justify",style="line-height: 27px;"))

data_description2 <- 
  h4(HTML("
    <ol>
      <li>Categorical Variables :-
          <ul type=\"disc\">
            <li>outcome</li>
            <li>gender</li>
            <li>hypertensive</li>
            <li>atrialfibrillation</li>
            <li>CHD with no MI</li>
            <li>diabetes</li>
            <li>deficiencyanemias</li>
            <li>depression</li>
            <li>atrialfibrillation</li>
            <li>Hyperlipemia</li>
            <li>Renal failure</li>
            <li>COPD</li></ul>
    
      <li>Numerical Variables :-
          <ul type=\"disc\">
            <li>age</li>
            <li>heart rate</li>
            <li>Systolic blood pressure</li>
            <li>Diastolic blood pressure</li>
            <li>Respiratory rate</li>
            <li>temperature</li>
            <li>SP O2</li>
            <li>Urine output</li>
            <li>hematocrit</li>
            <li>RBC</li>
            <li>MCH</li>
            <li>MCHC</li>
            <li>MCV</li>
            <li>RDW</li>
            <li>Leucocyte</li>
            <li>Platelets</li>
            <li>Neutrophils</li>
            <li>Basophils</li>
            <li>Lymphocyte</li>
            <li>PT</li>
            <li>INR</li>
            <li>NT-proBNP</li>
            <li>Creatine kinase</li>
            <li>Creatinine</li>
            <li>Urea nitrogen</li>
            <li>glucose</li>
            <li>Blood potassium</li>
            <li>Blood sodium</li>
            <li>Blood calcium</li>
            <li>Chloride</li>
            <li>Anion gap</li>
            <li>Magnesium ion</li>
            <li>PH</li>
            <li>Bicarbonate</li>
            <li>Lactic acid</li>
            <li>PCO2</li>
            <li>EF</li>
  "),style="line-height: 27px;")

univariate <- h4(p("In this analysis two types of variables are considered, 1) Categorical
                   2) Numerical ",align="justify",style="line-height: 27px;"))

BMI_intro <- h4(p("Analysing the distribution of numerical variable based on BMI classification.",br(),
                  "BMI is classified as 4 categories.",br(),
"if BMI < 18.5  it comes under the category of Under weight",br(),
"if BMI is 18.5 to 25 it comes under Normal weight",br(),
"if BMI is 25 to 30 it comes under Over weight",br(),
"if BMI is over 30  it comes under Obese",align="justify",style="line-height: 27px;"))

obs2<- h3(p("Some of the key insights are"))
obs1 <- h4(HTML("
      <ol>
        <li>Impact of Categorical Variables on in-hospital mortality rate:-
          <ul type=\"disc\">
          <li>More Impact</li>
            <ul>
              <li>Hyper tension</li>
              <li>Renal failure</li>
              <li>Anaemia deficiency</li></ul>
          <li>Less Impact</li>
            <ul>
              <li>Diabetes</li>
              <li>Gender</li>
              <li>Depression</li>
              <li>hyperlipemia</li></ul>
          <li>No Impact</li>
            <ul>
              <li>CHD with no MI</li>
              <li>COPD</li></ul>
          </li></ul>
        <li>In hospital mortality rate is higher for the following Numerical Variables:-

      <ul type=\"disc\">
      <li>Age group of 65 to 90</li>
      <li>BMI 21 and 33</li>
      <li>Heart rate 79 to 104</li>
      <li>Systolic Blood Pressure 95 to 125</li>
      <li>Diastolic Blood Pressure 48 to 66</li>
      <li>Respiratory rate between 18 to 21 and 23 to 25.5</li>

"),align="justify",style="line-height: 27px;")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel(
    fluidRow(
      div("In Hospital Mortality Rate Analysis",style=" color:#f94144; font-size:30px"),
      align="center"
    )
  ),
  mainPanel(navbarPage("",theme=shinytheme("united"),
                       tabPanel("About",value=1),
                       tabPanel("Dataset Description",value=8),
                       tabPanel("Univariate Analysis",value=2),
                       tabPanel("Bivariate Analysis",value=3),
                       tabPanel("Analysis (BMI classification)",value=4),
                       tabPanel("Observations",value=5),
                       id="tabselected"),
            conditionalPanel(condition="input.tabselected==1",br(),Intro1,Intro2),
            conditionalPanel(condition="input.tabselected==8",br(),data_description1,br(),data_description2),
            conditionalPanel(condition="input.tabselected==2",br(),
                             sidebarLayout(
                               sidebarPanel("Select type of Variable",verticalTabsetPanel(
                                 verticalTabPanel("Categorical Variable",value=6,box_height = "45px"),
                                 verticalTabPanel("Numerical variable",value=7,box_height = "45px"),
                                 id="tabselected1",
                                 color="#f94144",
                                 contentWidth=12)),
                               
                               mainPanel(univariate,conditionalPanel(condition="input.tabselected1==6",
                                                          mainPanel(selectInput("var_cat","Select the variable",cat_list)),
                                                          mainPanel(plotOutput("barplot_cat"),width="100%")),
                                         
                                         
                                         conditionalPanel(condition="input.tabselected1==7",
                                                          mainPanel(selectInput("var_num","Select the variable",num_list)),
                                                          mainPanel(plotOutput("hist_num"),width="100%")))
                               
                             )
            ),
            conditionalPanel(condition="input.tabselected==4",br(),
                             sidebarLayout(
                               sidebarPanel(selectInput("var_bmi","Select the variable",num_list)),
                               mainPanel(BMI_intro,plotOutput("box_plot")))),
            
            conditionalPanel(condition="input.tabselected==3",br(),
                             sidebarLayout(
                               sidebarPanel(radioButtons("cond","Patient condition",
                                                         inline = TRUE,
                                                         c("Survived" = "Yes",
                                                           "Deceased" = "No",
                                                           "All" = "total"))),
                               mainPanel(selectInput("num_var_biv","Select the 1st variable",num_list),
                                         selectInput("cat_var_biv","Select the 2nd variable",cat_list),
                                         plotOutput("biv_plot")))),
            
            conditionalPanel(condition="input.tabselected==5",br(),obs2,br(),obs1),
            width=12)
)

server <- function(input, output) {
  
  output$barplot_cat <- renderPlot({
    if (input$var_cat=="gender"){
      bar_perc_gender(hf_mort,"gender")
    }
    else{
      bar_perc_categorical2(hf_mort,input$var_cat)
    }
  })
  output$hist_num <- renderPlot(hist_cont_var(hf_mort,input$var_num))
  output$box_plot <- renderPlot(box_cts_BMI(hf_mort,input$var_bmi))
  output$biv_plot <- renderPlot(bivariate(hf_mort,input$cat_var_biv,input$num_var_biv,input$cond))
  
}

shinyApp(ui=ui,server=server)

