library(shiny)

shinyUI(navbarPage(title=div(img(src="SF.png", width="27%",align="right"),"Crime In San Francisco"),
                   tabsetPanel(
                     tabPanel("Crime By District",
                              sidebarLayout(
                                sidebarPanel(
                                p("The plot shows you description of 10 important crimes in all the police districets
                                  in the city of San Francisco. These crimes are over a period of 14 years, 
                                  extending from January 2003 to Sep, 2017. "),
                                width=2,
                                selectInput("name", "Choose a district of your choice:", 
                                            choices = choices = list("NONE","BAYVIEW", "CENTRAL", "INGLESIDE", "MISSION", "NORTHERN", "PARK", 
                                                                     "RICHMOND", "SOUTHERN", "TARAVAL", "TENDERLOIN"),
                                            selected = "NONE"
                                )          
                                ),
                                mainPanel(
                                  
                                  plotOutput("crimeDist")
                                )
                              )
                              ),
                     tabPanel("Heatmap of Crimes in Districts",
                              sidebarLayout(
                                sidebarPanel(
                                  p("The heatmap shows the intensity of crimes in different
                                    police districts in the city of San Francisco. Select districts
                                    from the drop down menu below (you can select multiple
                                    inputs). You can also vary the parameters of the heatmap by varying
                                    the appropriate sliders."),
                                  hr(),
                                  selectInput("district","Select District",
                                              choices=levels(SF$PdDistrict),
                                              multiple=TRUE,
                                              selected = "BAYVIEW"),
                                  sliderInput("blur","Amount of blur to apply",
                                              min=1,max=40,value=20),
                                  sliderInput("max","Maximum point intensity",
                                              min=0.01,max=1,value=0.05),
                                  sliderInput("radius","Radius of each point of the heatmap",
                                              min=5,max=30,value=15)
                                ),
                                mainPanel(
                                  
                                  leafletOutput("heatmap",height=800)
                                )
                              )
                              )
                   )
                   ))
