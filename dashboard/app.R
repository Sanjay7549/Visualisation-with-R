library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tm)
library(stringr)
library(corrplot)
library(tidyverse)
library(DT)
library(psych)


data = read.csv(file = "Bengaluru_House_Data.csv", header = T)
df = data.frame(data)

# Finding missing values and cleaning data frame
df <- replace(df, df=='', NA)

missing_values = colSums(is.na(df))# missing values in each column
#missing_values

df[,c(2,5)] = list(NULL)
df = na.omit(df)

# Pre-Processing the data

# create BHK from size column in numeric type

df$BHK <- 0
df$BHK = extract_numeric(df$size)  # using tidyr
df = df[,-3]


# total_sqft column in required format


df = subset(df, removeNumbers(data$total_sqft) == "")  
df$area_sqft = as.numeric(gsub(".*?([0-9]+).*", "\\1", df$total_sqft))
df = df[,-3]

df$price_per_sqft = as.integer((df$price / df$area_sqft ) * 100 * 1000) # In Rs

# Restricting dataset

# delete rows where bath > bhk
df = subset(df, df$bath <= df$BHK)

# delete rows where bhk > 5
df = subset(df, df$BHK <= 5)

# delete rows where area_sqft > 4000
df = subset(df, df$area_sqft <= 4000)

# delete rows where area per bhk < 300 sqft
df = subset(df, df$area_sqft/df$BHK >= 300)


# delete rows where price per sqft > 30000
df = subset(df, df$price_per_sqft < 30000)

dfnum = df

# Reconverting BHK, bath, balcony into catregorical form
df$BHK = as.character(df$BHK)
df$bath = as.character(df$bath)
df$balcony = as.character(df$balcony)
rownames(df) = NULL # reset index


tab = data.frame(sort(table(df$location), decreasing = TRUE))
newtab = tab[tab$Freq >= 50,]
top_locations = as.vector(newtab$Var1)

newtab1 = tab[tab$Freq >= 80,]
top_20 = as.vector(newtab1$Var1)


intro = "In this project we analyse the key things that a potential home buyer considers before purchasing a house.
The location, the size of the property, vicinity to offices, schools and the most important factor - the price.
Buying a house has always been a tricky choice, especially in a metro city like Bengaluru. With its millennial crowd,
vibrant culture, great climate and a slew of job opportunities, it is difficult to ascertain the price of a house."

details = "The dataset has details of some of the houses in Bengaluru spread across different locations.
It has 8 variables out of which 5 (area_type, BHK, bath, balcony and location) are categorical and rest 3
(price, area_sqft, price_per_sqft) are numerical"


header= dashboardHeader(
                       title = "Bengaluru House Price Dashboard", titleWidth = 400,              
                       tags$li(class= 'dropdown', 
                               tags$a(href='https://github.com/Sanjay7549/Visualisation-with-R',
                                      icon('github'), "Source Code", target="_blank"
                                      )
                               ),
                       
                       tags$li(class='dropdown',
                               tags$a(href='https://www.youtube.com/channel/UCah3ELJFTicZufOEqL2Dtpw',
                                      icon('youtube'), "Youtube", target="_blank"
                                      )
                               )
                       )
        
        
sidebar = dashboardSidebar(
                           sidebarMenu(
                                       id ='sidebar',
                                       menuItem('Dataset', tabName = 'data', icon = icon('database')),
                                       menuItem('Visualization', icon = icon('chart-line'),
                                                menuItem('Overview', tabName = 'overview'),
                                                menuItem('In-Depth', tabName = 'viz')
                                                ),
                                       menuItem('Conclusion', tabName = 'conclude', icon = icon("refresh", lib = "glyphicon"))
                                      )
                           )

        
body = dashboardBody(
                    tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }'))),
                    tabItems(
                             tabItem(tabName='data',
                                     tabBox(id='t11', width=12,
                                            tabPanel('Introduction', icon= icon('address-card'),
                                                     fluidRow(column(width=8, img(src='house.jpg', width=550, height=450),br(),
                                                                     a(href='https://www.pexels.com/photo/aerial-view-of-cityscape-2116721/',
                                                                       'Photo by Tom Fisk on Pexels'
                                                                       ), align='center'
                                                                     ),
                                                              column(width=4, tags$br(),h3('About'),p(intro),br(),br(),
                                                                     p(details),
                                                                     a(href='https://www.kaggle.com/amitabhajoy/bengaluru-house-price-data'
                                                                       ,'Source : Kaggle')
                                                                     )
                                                              )
                                                      
                                                     ),
                                            tabPanel('Data', icon= icon('table'), dataTableOutput('dataT')),
                                            tabPanel('Structure', icon= icon('uncharted'), verbatimTextOutput('structure')),
                                            tabPanel('Data Summary', icon= icon('list-alt'),
                                                     tabsetPanel(
                                                            tabPanel('Summary-stats', verbatimTextOutput('sumstats')),
                                                            tabPanel('Bath And Balcony Composition in BHK',
                                                                     fluidRow(column(width=6,h3('Bath'),  tableOutput('databath')),
                                                                              column(width=6,h3('Balcony'), tableOutput('databalcony'))
                                                                              )
                                                                     ),
                                                            tabPanel('Price And Area corresponding to BHK',
                                                                     fluidRow(column(width=6,h3('Price'),  tableOutput('dataprice')),
                                                                              column(width=6,h3('Area'),  tableOutput('dataarea'))
                                                                     )
                                                                     ),
                                                            tabPanel('Locationwise Averge Price', dataTableOutput('datalocprice'))
                                                            )
                                                    )
                                           )
                                     ),
                             tabItem(tabName = 'overview',
                                     tabBox(id='t21', width = 12,
                                            fluidRow(column(width=6,plotOutput('barbhkbalcony')),
                                                     column(width=6,plotOutput('barbhkbath'))
                                                     ),br(),br(),
                                            fluidRow(column(plotOutput('barlocarea'), width=12))
                                            )
                                     ),
                             
                             tabItem(tabName='viz',
                                     tabBox(id='t22', width=12,
                                            tabPanel('Pie-Chart', value='pie',icon = icon('pie-chart'),
                                                     fluidRow(column(selectInput(inputId ='var1', label= 'Select Loaction',
                                                                                 choices = top_locations,selected='Whitefield'
                                                                                 ), 
                                                                     plotOutput('pieplot'),width=12
                                                                     )
                                                              )
                                                     ),
                                            tabPanel('Scatterplot', value = 'scatter',icon = icon('uncharted'),
                                                     fluidRow(column(selectInput(inputId ='var2', label= 'Select Location', 
                                                                                 choices = top_locations, selected='Sarjapur  Road'
                                                                                 ),
                                                                     plotOutput('scatterplot'),width=12
                                                                     )
                                                              )
                                                     ),
                                            tabPanel('Boxplot', value='box',icon = icon("barplot"),
                                                     fluidRow(column(selectInput(inputId ='var31', label= 'Select Location',
                                                                                 choices= top_locations,selected='Kanakpura Road'
                                                                                 ),
                                                                     selectInput(inputId ='var32', label= 'Select Area Type', choices = 
                                                                                   unique(df$area_type),selected='Super built-up  Area'
                                                                                 ),
                                                                     plotOutput('boxplot'),width=12
                                                                     )
                                                              )
                                    
                                                     ),
                                            tabPanel('Geom Line', value='line', icon = icon('chart-line'),
                                                     fluidRow(column(selectInput(inputId ='var4', label='Select BHK',
                                                                                 choices= sort(unique(df$BHK)),selected='3'
                                                                                 ),
                                                                     plotOutput('geomline'),width=12
                                                                     )
                                                              )
                                                    )
                                            )
                                     ),
                             tabItem(tabName = 'conclude', fluidRow(uiOutput('con')
                                                                    )
                                     )
                             
                             )
                    )








server = function(input, output,session){
  # Structure
  output$structure = renderPrint(str(df))

  
  # Data Table
  output$dataT = renderDataTable(df)    # using DT library
  
  # Data Summary
  output$sumstats = renderPrint(describe(dfnum, fast = TRUE)) # using psych library
  output$databath = renderTable(df %>% group_by(BHK,bath) %>% summarise(count = n()))    
  output$databalcony = renderTable(df %>% group_by(BHK,balcony) %>% summarise(count = n()))    
  output$dataprice = renderTable(df %>% group_by(BHK) %>% summarise(avg_price_lacs = round(sum(price)/n(),digits=2)))   
  output$dataarea = renderTable(df %>% group_by(BHK) %>% summarise(avg_area_sqft = as.integer(sum(area_sqft)/n()))) 
  output$datalocprice = renderDataTable({
    df2 = df %>% group_by(location) %>% summarise(price_per_sqft = as.integer((sum(price)*100000)/sum(area_sqft)), count=n()) 
    df2[df2$count>=30,1:2]      # using DT library
    }
    )
  
  #overview
  
  output$barbhkbalcony = renderPlot(ggplot(df,aes(x=BHK, fill=as.factor(balcony)))+
                                      geom_bar() + labs(fill='Balcony') +labs(title="No. of Balconies in houses of different BHK")+
                                      theme(plot.title =element_text(face ='bold', hjust = 0.5, size=18))
                                    )
  
  output$barbhkbath = renderPlot(ggplot(df,aes(x=BHK, fill=as.factor(bath)))+
                                   geom_bar() + labs(fill='Bath') + labs(title="No. of Bathrooms in houses of different BHK")+
                                   theme(plot.title =element_text(face ='bold', hjust = 0.5, size=18))
                                 )
  
  output$barlocarea = renderPlot({
    df4 = df[df$location %in% top_20,]
    ggplot(df4,aes(x=location, fill=as.factor(area_type)))+
      geom_bar() + labs(fill='Area_type', title ='Area type of Houses in Different Locations') +
      theme(axis.text.x =element_text(hjust= 1.1, vjust =1,angle=60), plot.title =element_text(face ='bold', hjust = 0.5, size=18),
            axis.title =  element_text(size=15), axis.text = element_text(size=14)
           )
  }
  )
  
  # pie-chart
  output$pieplot = renderPlot({
    
    
    # Pie Chart of BHK
    dftemp = df[df$location == input$var1,]
    n_data_pts = nrow(dftemp)
    df1= dftemp %>% group_by(BHK) %>% summarise(count=n())
    df1$count = df1$count/sum(df1$count)*100  # percentage
    df1$count = round(df1$count, digits = 2)  # roundoff
    df1$BHK = factor(df1$BHK,levels= as.character(1:5))
    ggplot(df1, aes(x="",y=count, fill=BHK)) +  geom_bar(stat = 'identity', color='white')+
      labs(caption= paste("No. of data points : ", n_data_pts))+
      geom_text(aes(x=1.56,label= paste(count,'%')), size=3.5, angle = 60,position = position_stack(vjust=0.5))+
      coord_polar(theta='y', direction = -1)+theme_void() +ggtitle('Pie Chart of BHK')+ 
      #scale_fill_brewer(palette = 'Set1') +
      theme(plot.title = element_text(face='bold', hjust=0.5, size=16),
            plot.caption = element_text(hjust = 0.5, size=12)
            )
  }
  )
  
  # Scatter plot
  output$scatterplot = renderPlot({
    dftemp = df[df$location == input$var2,]
    n_data_pts = nrow(dftemp)
    mean_price = sum(dftemp$price)/(100*n_data_pts)
    mean_price = round(mean_price, digits = 2)
    ggplot(dftemp, aes(x= area_sqft, y=price/100, color=area_type)) +
      geom_point(size=2, shape=8) + # ylim(0,5) +
      #facet_wrap(~area_type) + 
      labs(x='Area (sq. ft)', y='Price (in cr)', title='Area v/s Price Scatterplot',
           caption= paste('Avg. Price(cr) : ', mean_price,"       ","No. of data points : ", n_data_pts)
           )+
      theme_light(base_size = 15) +
      theme(plot.title = element_text(face= 'bold', hjust=0.5, size=18), axis.title = element_text(size=16),
            plot.caption = element_text(hjust = 0.5, size=12)
            )
      
       #+ theme(legend.position = 'None') 
  }
  )
  
  observeEvent(input$var31,{
    
    area = data.frame(sort(table(df[df$location == input$var31,]$area_type), decreasing = TRUE))
    area= area[area$Freq >=10,] 
    top_area_type = as.vector(area$Var1)
    
    updateSelectInput(session,"var32",choices = top_area_type)
  }
  )
  
  # Box Plot
  output$boxplot = renderPlot({
    dftemp = df[df$location == input$var31 & df$area_type == input$var32,]
    n_data_pts = nrow(dftemp)
    ggplot(dftemp, aes(x = BHK , y = price/100, fill = BHK)) +
      theme_bw() +
      theme(legend.position = 'None')+
      xlab("BHK")+ ylab("Price (in cr)") +
      labs(caption= paste("No. of data points : ", n_data_pts))+
      geom_boxplot(outlier.color = 'red', outlier.shape = 5, outlier.alpha = 0.5) +
      stat_boxplot(geom = 'errorbar', width = 0.2)+
      ggtitle('BHK v/s Price Boxplot') +  # ylim(0,500)+
      theme(plot.title = element_text(face ='bold', hjust = 0.5, size=18), axis.text =  element_text(size=14),
            axis.title =  element_text(size=16), plot.caption = element_text(hjust = 0.5, size=12)
            )
  }
  )
  
  # Geomline
  output$geomline = renderPlot({
    df1 = df[ df$BHK == input$var4 ,]
    dfgroup = df1 %>% group_by(location) %>% summarise(mean = (sum(price)*100000)/sum(area_sqft))
  
    
    df2 = filter(dfgroup, location %in% top_20)
    ggplot(df2, aes(x=location, y= mean, group=1))+ geom_line() + geom_point() +
      labs(x='Location', y= 'Avg. Price_per_sqft', title='Average Price_per_sqft of Houses over Locations ')+
      theme(axis.text.x =element_text(hjust= 1.1, vjust =1,angle=60), plot.title =element_text(face ='bold', hjust = 0.5, size=18),
            axis.title =  element_text(size=16), axis.text = element_text(size=14)
            )
  }
  )
  
  # Conclusion
  output$con= renderUI(
    tags$div(
             tags$br(),
             tags$br(),
             tags$ul(
                    tags$li(h4("Most of the houses are either 2-BHK or 3-BHK")),
                    tags$ul(
                            tags$li(h4("Number of bathrooms in a house is approximately equal to the BHK of the house")),
                            tags$li(h4("Number of balcony in a house is approximately 2")),
                            tags$li(h4("If a person has a budget of 1 cr, he can comfortably buy a 3 BHK house")),
                            tags$li(h4("Price per sqft of 2 and 3 BHK houses are optimal and is around 5500 across locations. 
                                       Others have higher prices")
                                    )
                            ), 
                    tags$br(),
                    
                    
      
                    tags$li(h4("Area type of most of the houses is 'Super Built up Area'. 
                               It indicates that they are a part of an apartment")
                            ),
                    tags$ul(
                            tags$li(h4("Here Carpet Area = Net Usable Area of the Apartment (excluding external walls, Terrace Area,
                                        Balcony area, & Verandah Area) + Areas of the Internal Partition Walls")),
                            tags$li(h4("The built-up area is calculated by adding Carpet Area, Both Interior & Exterior Wall areas,
                                       Exclusive Balcony, and Corridor if any.")),
                            tags$li(h4("Super built-up area is calculated by adding the built-up area and the proportionate common area.")),
                            tags$li(h4("Plot Area is the total area of the land on which the house is built"))
                           ),
                    tags$br(),
                    
                    
                    
                    
                    tags$li(h4("From the scatterplot we observe that the houses which are listed in plot area type costs way more than
                            their counterparts.")),
                    tags$ul(
                            tags$li(h4("This is due to the fact that these are individual houses(duplex type) unlike apartments.
                                       And whole plot area belongs to a single owner ,i.e., there is no shared space")
                                    ), 
                            tags$li(h4("We also observe that price of the house increases nearly quadratically as the area of the
                                       house increases")
                                    )
                           ),
                    tags$br(),
                    
                    
                    
                    tags$li(h4("From the boxplot we observe that from 3 bhk to 4bhk there is a huge jump in the price. 
                            So its better not to go for the 4 BHK houses")),
                    tags$ul(
                            tags$li(h4("In some locations price of 5-BHK houses are less than that of 4-BHK.")),
                            tags$li(h4("The reason is rooms are smaller in 5 BHK. As mean area sqft of 5 bhk is less than that
                                       of 4 bhk in that location ")),
                            tags$li(h4("As BHK increases price also increases. If one is looking for
                                       bigger home he should go for 5 bhk instead of 4 bhk"))
                            )
                    
                    )
             )
    )
  
}

ui = dashboardPage(skin= 'purple',header,sidebar,body)


shinyApp(ui, server)