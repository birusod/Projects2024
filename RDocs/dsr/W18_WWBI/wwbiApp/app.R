pacman::p_load(shiny, shinyWidgets, shinydashboard,
               tidyverse, plotly, reactable, ggflags, ggtext,
               kableExtra, xtable, DT
               )
# Individuals with tertiary Education as a share of public paid employees, by occupation

dd <- read_csv('edu_job.csv') |> 
    select(-a3) |> 
    mutate(
        a2 = replace_na(a2, 'na')) |> 
    filter(value > 0)
    

costum_theme <- function(){ 
    theme_minimal() %+replace%    
        theme(
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = 'black'),
            plot.background = element_rect(fill = 'black'),
            plot.title = element_text(color = 'white', face = 'bold', size = 20),
            plot.subtitle = element_text(color = 'white', face = 'bold', size = 16),
            plot.caption =  element_textbox(
                #family     = caption_font,
                hjust      = 1,
                color      = 'grey60',
                #size       = 1.5 * tsize,
                lineheight = 0.3,
                margin     = margin(.5,0,0,0, unit = 'cm'))
        )
}

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "WWBI DASH"),
    dashboardSidebar(
        subtitle = h5("Sub Saharan Africa", style = "color:orange"),
        sidebarMenu(
            menuItem("Viz to compare countries", tabName = "dashboard"),
            menuItem("Country drilldown", tabName = "countrydata")
            ),
        pickerInput(
            inputId = "country",
            label = "Select a country:", 
            choices = unique(dd$name),
            options = pickerOptions(container = "body", 
                                    liveSearch = TRUE),
            width = "100%"
            ),
        prettyRadioButtons(
            inputId = "group",
            label = "Choose a category:", 
            choices = c('Medical workers', 'Education', 'Health', 'Teachers')
            )
        ),
    
    dashboardBody(
            tabItems(
                tabItem(
                    "countrydata",
                    fluidRow(
                        box(
                            width = 12, status = "info",
                            title = "Country Drilldown",
                            dataTableOutput("country")
                            )
                        )
                    ),
                tabItem(
                    "dashboard",
                    fluidRow(
                        box(
                            width = 12, height = 20, status = "info", solidHeader = TRUE,
                            title = "Comparison between countries",
                            plotOutput("barPlot", width = "100%", height = 600)
                            )
                        )
                    )
                )
            )
    )
                


# Define server logic required to draw a histogram
server <- function(input, output) {

    
    output$country <- renderDataTable({
        dd |> 
            filter(name == input$country,
                   class == 'Public') |> 
            mutate(year = as.character(year)) |> 
            select(group, year, value) |>
            mutate(value = round(value * 100, 1)) |> 
            filter(group  %in% c('Medical workers', 'Education', 'Health', 'Teachers')) |> 
            rename_with(~c('OCCUPATION', 'YEAR', 'SHARE')) #|> 
            #kable() |> 
            #kable_styling(latex_options = 'striped')
            #xtable()
            
    }) # , digits = 1
    
    
    output$barPlot <- renderPlot({
        
        dat <- reactive({
            dd |> 
                filter(class == 'Public', 
                       edu_level == 'Tertiary Education',
                       group != 'Overall') |> 
                select(year, a2, name, group, value) |> 
                group_by(group, name) |> 
                
                filter(value == max(value)) |> 
                ungroup() |> 
                mutate(pct = round(value * 100, 0)) |> 
                filter(group == input$group,
                       value > 0) 
        })
        
        #height = dat() |> select(name) |> n_distinct()
        #width = max(dat() |> select(value), na.rm = FALSE)
        
        dat() |> 
            ggplot(aes(value, fct_reorder(name, value))) +
            geom_col(width  = .05, fill = 'grey99', alpha = .7) +
            geom_point(size = 12, alpha = 1, color = 'dodgerblue') +
            geom_text(aes(label = paste0(name, ', ', year)), 
                      x = 0.03,
                      hjust = 0, vjust = -1, 
                      color = 'white', size = 5,
                      fontface = 'bold.italic') +
            geom_text(aes(label = paste0(pct, '%'), x = value), 
                      hjust = .5, vjust = .5, 
                      color = 'white',
                      size = 5, fontface = 'bold') +
            geom_flag(aes(country = a2),
                      x = 0, 
                      size = rel(12)
                      ) +
            labs(x = NULL, y = NULL,
                 title = 'BUREAUCRACY INDICATORS', 
                 subtitle = 'Individuals with tertiary Education as a share of\npublic paid employees',
                 caption = 'by occupation') +
            costum_theme()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
