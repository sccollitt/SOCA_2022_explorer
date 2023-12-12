library(shiny)
library(ggplot2)
library(stringr)
library(rsconnect)
library(ggiraph) # for interactive ggplot

load("completed_data.rdata") # note: grey color corresponds with low confidence in estimate

data <- completed_data

# Function to wrap text
wrap_axis_labels <- function(text, width = 30) {
        str_wrap(text, width)
}

wrap_facet_labels <- function(text, width = 10) {
        str_wrap(text, width)
}

wrap_question <- function(text, width = 40) {
        str_wrap(text, width)
}


# list of questions
question_list <- c("Age group" = "agecat",
                   "Education" = "edu_5cat",
                   "Gender" = "gender3c",
                   "Region" = "region",
                   "Birthplace" = "birthplace_4cat",
                   "U.S. citizenship" = "citizenship_3cat",
                   "English proficiency" = "english_4cat",
                   "Household income group" = "hhi_5cat",
                   "Where did you mostly live when you were 12 years old?" = "upbringing", # CHAPTER 2
                   "What language(s) do you speak at home?" = "lang",
                   "I feel that I am accepted in the American society." = "accepted_american",
                   "I feel that I am accepted in the Asian American community" = "accepted_asian",
                   "I feel that I am accepted in the Chinese American community." = "accepted_chinese",
                   "I feel part of a combined culture." = "culture_combined",
                   "I feel like someone moving between two cultures." = "culture_between",
                   "I feel conflicted between the American and Chinese ways of doing things." = "culture_conflicted",
                   "Racial discrimination experiences in the past 12 months" = "dmat",
                   "Racism-related vigilance experiences in the past 12 months" = "dmat_vigilance",
                   "Everyday discrimination experiences in the past 12 months" = "dmat_everyday",
                   "Bias and hate incident experiences in the past 12 months" = "dmat_bias_hate",
                   "What are the most important problems facing the U.S. today?" = "mip", # CHAPTER 3
                   "Are you registered to vote?" = "vote_reg",
                   "Turnout in the 2020 presidential general election" = "turnout_2020_2",
                   "In general, how would you describe the current relationship between the U.S. and China?" = "us_china_relations_exno",
                   "In what areas do you think that the U.S. and China would benefit most from working together?" = "us_china_collab",
                   "Thinking about the U.S. economic policy toward China, which is more important?" = "us_china_stance_exno",
                   "In the past 12 months, did you or anyone in your household receive any of the following?" = "assistance", # CHAPTER 4
                   "In general, how would you rate your physical health?" = "phealth",
                   "In general, how would you rate your mental health, including your mood and your ability to think?" = "mhealth",
                   "Health disadvantage indicator" = "Health disadvantage",
                   "Do you have a disability that prevents you from working or limits the kind or amount of work you can do?" = "disability",
                   "Life satisfaction indicator" = "lifesat_3cat",
                   "Psychological distress indicator" = "psych_3cat",
                   "What kinds of health insurance or health care coverage do you have?" = "hinsurance_10cat",
                   "During the past 12 months, did you delay or not get the medical care you needed?" = "medcare",
                   "In the past 12 months, about how often did your household run out of food or worried food would run out before having money to buy more?" = "foodworry",
                   "In the past 12 months, about how often did your household not pay the full amount of rent or mortgage or bills because there wasn't enough money?" = "norent",
                   "Economic hardship indicator" = "hardship"
)

# list of demographics
demographic_list <- c("Overall" = "all",
                      "Age group" = "agecat",
                      "Education" = "edu_5cat",
                      "Gender" = "gender3c",
                      "Region" = "region",
                      "Birthplace" = "birthplace_4cat",
                      "U.S. citizenship" = "citizenship_3cat",
                      "English proficiency" = "english_4cat",
                      "Household income group" = "hhi_5cat"
)

ui <- fluidPage(
        tags$head(
                includeCSS("www/styles.css")
        ),
        fluidRow(
                column(8,
                       selectInput("question", "Select a Question:", width = "90%",
                                   choices = list("Demographics" = list("Age group" = "agecat",
                                                                        "Education" = "edu_5cat",
                                                                        "Gender" = "gender3c",
                                                                        "Region" = "region",
                                                                        "Birthplace" = "birthplace_4cat",
                                                                        "U.S. citizenship" = "citizenship_3cat",
                                                                        "English proficiency" = "english_4cat",
                                                                        "Household income group" = "hhi_5cat"),
                                                  "Identity and Discrimination" = list("Where did you mostly live when you were 12 years old?" = "upbringing",
                                                                                       "What language(s) do you speak at home?" = "lang",
                                                                                       "I feel that I am accepted in the American society." = "accepted_american",
                                                                                       "I feel that I am accepted in the Asian American community" = "accepted_asian",
                                                                                       "I feel that I am accepted in the Chinese American community." = "accepted_chinese",
                                                                                       "I feel part of a combined culture." = "culture_combined",
                                                                                       "I feel like someone moving between two cultures." = "culture_between",
                                                                                       "I feel conflicted between the American and Chinese ways of doing things." = "culture_conflicted",
                                                                                       "Racial discrimination experiences in the past 12 months" = "dmat",
                                                                                       "Racism-related vigilance experiences in the past 12 months" = "dmat_vigilance",
                                                                                       "Everyday discrimination experiences in the past 12 months" = "dmat_everyday",
                                                                                       "Bias and hate incident experiences in the past 12 months" = "dmat_bias_hate"),
                                                  "Political Engagement" = list("What are the most important problems facing the U.S. today?" = "mip",
                                                                                "Are you registered to vote?" = "vote_reg",
                                                                                "Turnout in the 2020 presidential general election" = "turnout_2020_2",
                                                                                "In general, how would you describe the current relationship between the U.S. and China?" = "us_china_relations_exno",
                                                                                "In what areas do you think that the U.S. and China would benefit most from working together?" = "us_china_collab",
                                                                                "Thinking about the U.S. economic policy toward China, which is more important?" = "us_china_stance_exno"),
                                                  "Disadvantage and Needs" = list("In the past 12 months, did you or anyone in your household receive any of the following?" = "assistance",
                                                                                  "In general, how would you rate your physical health?" = "phealth",
                                                                                  "In general, how would you rate your mental health, including your mood and your ability to think?" = "mhealth",
                                                                                  "Health disadvantage indicator" = "Health disadvantage",
                                                                                  "Do you have a disability that prevents you from working or limits the kind or amount of work you can do?" = "disability",
                                                                                  "Life satisfaction indicator" = "lifesat_3cat",
                                                                                  "Psychological distress indicator" = "psych_3cat",
                                                                                  "What kinds of health insurance or health care coverage do you have?" = "hinsurance_10cat",
                                                                                  "During the past 12 months, did you delay or not get the medical care you needed?" = "medcare",
                                                                                  "In the past 12 months, about how often did your household run out of food or worried food would run out before having money to buy more?" = "foodworry",
                                                                                  "In the past 12 months, about how often did your household not pay the full amount of rent or mortgage or bills because there wasn't enough money?" = "norent",
                                                                                  "Economic hardship indicator" = "hardship")
                                   ))),
                column(4,
                       selectInput("demographic", "Filter by Demographic:",
                                   choices = c("Overall" = "all",
                                               "Age group" = "agecat",
                                               "Education" = "edu_5cat",
                                               "Gender" = "gender3c",
                                               "Region" = "region",
                                               "Birthplace" = "birthplace_4cat",
                                               "U.S. citizenship" = "citizenship_3cat",
                                               "English proficiency" = "english_4cat",
                                               "Household income group" = "hhi_5cat"))
                )),
        fluidRow(
                column(12,
                       htmlOutput("textTitle")
                )),
        fluidRow(
                column(12,
                       girafeOutput("barPlot")
                )
        )
)

server <- function(input, output) {
        
        # row of displayed selection
        output$textTitle <- renderText({
                
                # Look up the user-friendly names for the selected question and demographic
                question_title <- names(question_list[question_list == input$question])
                demographic_title <- names(demographic_list[demographic_list == input$demographic])
                
                # Wrap questions
                wrapped_question <- sapply(unique(question_title), wrap_question)
                names(wrapped_question) <- as.character(unique(question_title))
                
                HTML(paste("Response distribution for:", "<b>", wrapped_question, "</b> <br> By: <b>", demographic_title, "</b>"))
        })
        
        output$barPlot <- renderGirafe({
                
                # Check if the selected question and demographic are the same
                if (input$question == input$demographic) {
                        # Show a validation message instead of a plot
                        validate(
                                need(FALSE, "Cannot select the same question and demographic filter.")
                        )
                }
                
                # Filter data based on user selections
                filtered_data <- data[data$question == input$question & data$group == input$demographic, ]
                
                # Reverse the order of levels for the factor variable 'response'
                filtered_data$response <- factor(filtered_data$response, levels = rev(levels(filtered_data$response)))
                
                # Wrap x axis ticks
                wrapped_responses <- sapply(filtered_data$response, wrap_axis_labels)
                names(wrapped_responses) <- filtered_data$response
                
                # Wrap facet labels
                wrapped_group_levels <- sapply(unique(filtered_data$group_levels), wrap_facet_labels)
                names(wrapped_group_levels) <- as.character(unique(filtered_data$group_levels))
                
                # Calculate maximum proportion for setting y-axis limits
                max_proportion <- max(filtered_data$proportion)
                
                p <- ggplot(filtered_data, aes(x = response, y = proportion, fill = bar_color)) +
                        geom_bar(stat = "identity", position = "dodge") +
                        geom_text(aes(label = paste0(round(proportion, 0), "%"), color = text_color),
                                  hjust = -0.1,
                                  position = position_dodge(0.9), size = 2.5) +
                        scale_fill_identity() +
                        scale_color_identity() + 
                        labs(title = NULL,
                             y = NULL,
                             x = NULL) +
                        facet_wrap(~ group_levels, scales = "free", ncol = 2,  labeller = labeller(group_levels = wrapped_group_levels)) +
                        scale_x_discrete(labels = wrapped_responses) +
                        scale_y_continuous(limits = c(0, max_proportion + 20)) +
                        theme(panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(), 
                              panel.background = element_blank(), 
                              axis.line = element_blank(),
                              legend.position = "none",
                              strip.background = element_blank(),
                              axis.text.y = element_text(vjust = 0.5, size = 7.5), 
                              axis.text.x = element_blank(),
                              axis.ticks.y = element_blank(),
                              axis.ticks.x = element_blank(),
                              axis.title.x = element_text(),            
                              axis.title.y = element_text(),            
                              plot.title = element_text(hjust = 0), 
                              strip.text = element_text(size = 9, hjust = 0)) +
                        coord_flip(expand = F)
                
                girafe(ggobj = p, width_svg = 5, height_svg = 5)
                
                
        })
        

}

shinyApp(ui = ui, server = server)

