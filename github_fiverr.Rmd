library(ggplot2)
library(dplyr)
library(plotly)
fiverr <- read.csv("C:/Users/kevin/project02/fiverr/csv/fiverr.csv")
# dropping $ for Start_Cost
fiverr$Start_Cost <- as.numeric(gsub('[$,]', '', fiverr$Start_Cost))
summary(fiverr$Start_Cost)

# fixing Member_Since in only 'years'

fiverr$Member_Since <- substr(fiverr$Member_Since, 1,2)
fiverr$Member_Since <- paste0("20", fiverr$Member_Since)


# converting factors to character for Category and Skills
library(stringr)
fiverr$Category <- as.character(fiverr$Category)
fiverr$Skills <- as.character(fiverr$Skills)
fiverr$Location <- as.character(fiverr$Location)
fiverr$Member_Since <- as.character(fiverr$Member_Since)
fiverr$Title <- as.character(fiverr$Title)
fiverr$Response.hr. <- as.character(fiverr$Response.hr.)
class(fiverr$Category)
class(fiverr$Skills)



fiverr$Category <- trimws(fiverr$Category)
fiverr$Sub_Category <- trimws(fiverr$Category)
#Category distribution
category <- fiverr %>%
  group_by(Category, Member_Since) %>%
  summarise(SUM_CATEGORY = n())
category_year <- ggplot(data = category, aes(x = Main_Category, y = SUM_CATEGORY, col = Member_Since))


category_year <- plot_ly(category, x = ~Main_Category, y = ~SUM_CATEGORY, color = ~Member_Since, colors = "Set1")#mode = "markers"
category_year2 = plotly_POST(category_year, filename="scatter/colorbrewer")
chart_link
filename = "scatter/color"


#define; start cost>50 by countries
Basic_Cost<- fiverr %>%
  group_by(Location) %>%
  arrange(Start_Cost) %>%
  filter(Start_Cost > 50)

#Plot; start cost>50 by countries
plot_ly(Basic_Cost, x = ~Location, y = ~Start_Cost)


#CONFIRMED!!!#####################################################
#define; number of freelancers by Location
by_Location <- fiverr %>%
  group_by(Location) %>%
  summarise(Freelancers = n()) %>%
  filter(Freelancers > 50)
p <- plot_ly(by_Location, x = ~Location, y = ~Freelancers, type = 'bar',
             marker = list(color =c('rgb(85, 131, 140)', 'rgb(119, 123, 124)',
                                    'rgb(119, 123, 124)', 'rgb(49,130,189)',
                                    'rgb(119, 123, 124)','rgb(119, 123, 124)',
                                    'rgb(119, 123, 124)','rgba(222,45,38,0.8)',
                                    'rgb(119, 123, 124)','rgb(119, 123, 124)',
                                    'rgb(119, 123, 124)','rgb(119, 123, 124)',
                                    'rgb(119, 123, 124)'))) %>%
  layout(title="Freelancers by Location")
p
#END################################################################
#################################################################


#CONFIRMED!!#######################################################
######Plot; Number of freelancers in each category by year ###########
by_num <- fiverr %>%
  group_by(Member_Since, Category) %>%
  summarise(Freelancers = length(Category)) %>%
  top_n(15)
p0 <- plot_ly(
  by_num, x = ~Category, y = ~Freelancers,
  color = ~Member_Since
) %>%
  layout(title = "Freelancers in Programming & Tech")
p0
#END#####################################################################
######################################################################


##CONFIRMED##########################################################################
### Reviews meaming Customers by Year
by_review_year <- fiverr %>%
  group_by(Member_Since, Category) %>%
  summarise(Reviews = sum(Review))

p1 <- ggplot(by_review_year, aes(Member_Since, Reviews)) + #, color = Category))+
  geom_line(aes(color = Category, group = Category, size = 0.7))+
  geom_point(aes(color = Category, size = 3)) +
  ggtitle("Customers reached")

p1
p_plotly <- ggplotly(p1)

#END#####################################################################
######################################################################

####CONFIRMED##########################################################
#####Location vs cost#############################################
by_cost <- fiverr %>%
  group_by(Category) %>%
  filter(Start_Cost > 50)
#group_by(Category, Start_Cost) %>%
#summarise(AVG_Cost = mean(Start_Cost)) %>%
plot_ly(by_cost, x = ~Category, y = ~Start_Cost, color = ~Category, type = "box") %>%
  layout(title = "Package Costs")

#END###########################################################################
############################################################################


############################################################################
##WORDCLOUD##
install.packages('tm')
install.packages('wordcloud')
install.packages('RColorBrewer')
library(data.table)
library(wordcloud)
library(RColorBrewer)
library(wordcloud)
library(wordcloud2)
library(tm)

##########WORDCLOUD##################
##########1.CHATBOT###################
Chatbot_txt <- fiverr %>%
  select(Category, Skills) %>%
  filter(str_detect(Category, "Chatbots")) %>%
  select(Skills) %>%
  filter (!str_detect(Skills, "no description"))

write.table(Chatbot_txt, "Chatbot_txt.txt")
wc_Chatbot <- readLines('Chatbot_txt.txt')

wcChatbot <- Corpus(VectorSource(wc_Chatbot))
inspect(wcChatbot)[1:10]

wcChatbot_data<-tm_map(wcChatbot,stripWhitespace)
wcChatbot_data<-tm_map(wcChatbot_data,tolower)
wcChatbot_data<-tm_map(wcChatbot_data,removeNumbers)
wcChatbot_data<-tm_map(wcChatbot_data,removePunctuation)
wcChatbot_data<-tm_map(wcChatbot_data,removeWords, stopwords("english"))

tdm_wcChatbot <- TermDocumentMatrix(wcChatbot_data)
TDM1 <- as.matrix(tdm_wcChatbot)
v1 = sort(rowSums(TDM1), decreasing = TRUE)
summary(v1)

wc1 <- wordcloud(wcChatbot_data, scale=c(5,0.5), max.words=300,
                 random.order=FALSE, rot.per=0.35, use.r.layout=FALSE,
                 colors=brewer.pal(8, "Set2"))
######END##########################

##########WORDCLOUD##################
##########2.DataAnalysis###################

DataAnalysis_txt <- fiverr %>%
  select(Category, Skills) %>%
  filter(str_detect(Category, "Data Analysis & Reports")) %>%
  select(Skills) %>%
  filter (!str_detect(Skills, "no description"))

write.table(DataAnalysis_txt, "DataAnalysis_txt.txt")
wc_DataAnalysis <- readLines('DataAnalysis_txt.txt')

wcDataAnalysis <- Corpus(VectorSource(wc_DataAnalysis))
inspect(wcChatbot)[1:10]

wcDataAnalysis_data<-tm_map(wcDataAnalysis,stripWhitespace)
wcDataAnalysis_data<-tm_map(wcDataAnalysis_data,tolower)
wcDataAnalysis_data<-tm_map(wcDataAnalysis_data,removeNumbers)
wcDataAnalysis_data<-tm_map(wcDataAnalysis_data,removePunctuation)
wcDataAnalysis_data<-tm_map(wcDataAnalysis_data,removeWords, stopwords("english"))

tdm_wcDataAnalysis <- TermDocumentMatrix(wcDataAnalysis_data)
TDM2 <- as.matrix(tdm_wcDataAnalysis)
v2 = sort(rowSums(TDM2), decreasing = TRUE)
summary(v2)

wc2 = wordcloud(wcDataAnalysis_data, scale=c(5,0.5), max.words=300,
                random.order=FALSE, rot.per=0.35, use.r.layout=FALSE,
                colors=brewer.pal(8, "Dark2"))
######END##########################

##########WORDCLOUD##################
##########3. DATABASES###################

DB_txt <- fiverr %>%
  select(Category, Skills) %>%
  filter(str_detect(Category, "Databases")) %>%
  select(Skills) %>%
  filter (!str_detect(Skills, "no description"))

write.table(DB_txt, "DB_txt.txt")
wc_DB <- readLines('DB_txt.txt')

wcDB <- Corpus(VectorSource(wc_DB))
inspect(wcDB)[1:10]

wcDB_data<-tm_map(wcDB,stripWhitespace)
wcDB_data<-tm_map(wcDB_data,tolower)
wcDB_data<-tm_map(wcDB_data,removeNumbers)
wcDB_data<-tm_map(wcDB_data,removePunctuation)
wcDB_data<-tm_map(wcDB_data,removeWords, stopwords("english"))

tdm_wcDB <- TermDocumentMatrix(wcDB_data)
TDM3 <- as.matrix(tdm_wcDB)
v3 = sort(rowSums(TDM3), decreasing = TRUE)
summary(v3)

wc3 <-wordcloud(wcDB, scale=c(5,0.5), max.words=300,
                random.order=FALSE, rot.per=0.35, use.r.layout=FALSE,
                colors=brewer.pal(8, "Set2"))
######END##########################

##########WORDCLOUD##################
##########4. Desktop###################

Desktop_txt <- fiverr %>%
  select(Category, Skills) %>%
  filter(str_detect(Category, "Desktop")) %>%
  select(Skills) %>%
  filter (!str_detect(Skills, "no description"))

write.table(Desktop_txt, "Desktop_txt.txt")
wc_Desktop <- readLines('Desktop_txt.txt')

wcDesktop <- Corpus(VectorSource(wc_Desktop))
inspect(wcDesktop)[1:10]

wcDesktop_data<-tm_map(wcDesktop,stripWhitespace)
wcDesktop_data<-tm_map(wcDesktop_data,tolower)
wcDesktop_data<-tm_map(wcDesktop_data,removeNumbers)
wcDesktop_data<-tm_map(wcDesktop_data,removePunctuation)
wcDesktop_data<-tm_map(wcDesktop_data,removeWords, stopwords("english"))

tdm_wcDesktop <- TermDocumentMatrix(wcDesktop_data)
TDM4 <- as.matrix(tdm_wcDesktop)
v4 = sort(rowSums(TDM4), decreasing = TRUE)
summary(v4)

wc4 <-wordcloud(wcDesktop, scale=c(5,0.5), max.words=300,
                random.order=FALSE, rot.per=0.35, use.r.layout=FALSE,
                colors=brewer.pal(8, "Dark2"))
######END##########################

##########WORDCLOUD##################
##########5. Ecommerce###################

Ecom_txt <- fiverr %>%
  select(Category, Skills) %>%
  filter(str_detect(Category, "Ecommerce")) %>%
  select(Skills) %>%
  filter (!str_detect(Skills, "no description"))

write.table(Ecom_txt, "Ecom_txt.txt")
wc_Ecom <- readLines('Ecom_txt.txt')

wcEcom <- Corpus(VectorSource(wc_Ecom))
inspect(wcEcom)[1:10]

wcEcom_data<-tm_map(wcEcom,stripWhitespace)
wcEcom_data<-tm_map(wcEcom_data,tolower)
wcEcom_data<-tm_map(wcEcom_data,removeNumbers)
wcEcom_data<-tm_map(wcEcom_data,removePunctuation)
wcEcom_data<-tm_map(wcEcom_data,removeWords, stopwords("english"))

tdm_wcEcom <- TermDocumentMatrix(wcEcom_data)
TDM5 <- as.matrix(tdm_wcEcom)
v5 = sort(rowSums(TDM4), decreasing = TRUE)
summary(v5)

wc5 <-wordcloud(wcEcom, scale=c(5,0.5), max.words=300,
                random.order=FALSE, rot.per=0.35, use.r.layout=FALSE,
                colors=brewer.pal(8, "Set2"))
######END##########################
##########WORDCLOUD##################

##########6.Mobile App & Web###################

Mobile_txt <- fiverr %>%
  select(Category, Skills) %>%
  filter(str_detect(Category, "Mobile")) %>%
  select(Skills) %>%
  filter (!str_detect(Skills, "no description"))

write.table(Mobile_txt, "Mobile_txt.txt")
wc_Mobile <- readLines('Mobile_txt.txt')

wcMobile <- Corpus(VectorSource(wc_Mobile))
inspect(wcMobile)[1:10]

wcMobile_data<-tm_map(wcMobile,stripWhitespace)
wcMobile_data<-tm_map(wcMobile_data,tolower)
wcMobile_data<-tm_map(wcMobile_data,removeNumbers)
wcMobile_data<-tm_map(wcMobile_data,removePunctuation)
wcMobile_data<-tm_map(wcMobile_data,removeWords, stopwords("english"))

tdm_wcMobile <- TermDocumentMatrix(wcMobile_data)
TDM6 <- as.matrix(tdm_wcMobile)
v6 = sort(rowSums(TDM6), decreasing = TRUE)
summary(v6)

wc6 <-wordcloud(wcDB, scale=c(5,0.5), max.words=300,
                random.order=FALSE, rot.per=0.35, use.r.layout=FALSE,
                colors=brewer.pal(8, "Dark2"))
######END##########################

##########WORDCLOUD##################
##########7.QA###################

QA_txt <- fiverr %>%
  select(Category, Skills) %>%
  filter(str_detect(Category, "QA")) %>%
  select(Skills) %>%
  filter (!str_detect(Skills, "no description"))

write.table(QA_txt, "QA_txt.txt")
wc_QA <- readLines('QA_txt.txt')

wcQA <- Corpus(VectorSource(wc_QA))
inspect(wcQA)[1:10]

wcQA_data<-tm_map(wcQA,stripWhitespace)
wcQA_data<-tm_map(wcQA_data,tolower)
wcQA_data<-tm_map(wcQA_data,removeNumbers)
wcQA_data<-tm_map(wcQA_data,removePunctuation)
wcQA_data<-tm_map(wcQA_data,removeWords, stopwords("english"))

tdm_wcQA <- TermDocumentMatrix(wcQA_data)
TDM7 <- as.matrix(tdm_wcQA)
v7 = sort(rowSums(TDM7), decreasing = TRUE)
summary(v7)

wc7 <-wordcloud(wcQA, scale=c(5,0.5), max.words=300,
                random.order=FALSE, rot.per=0.35, use.r.layout=FALSE,
                colors=brewer.pal(8, "Set2"))
######END##########################

##########WORDCLOUD##################
##########8.WebProgramming###################

Web_txt <- fiverr %>%
  select(Category, Skills) %>%
  filter(str_detect(Category, "Web Programming")) %>%
  select(Skills) %>%
  filter (!str_detect(Skills, "no description"))

write.table(Web_txt, "Web_txt.txt")
wc_Web <- readLines('Web_txt.txt')

wcWeb <- Corpus(VectorSource(wc_Web))
inspect(wcWeb)[1:10]

wcWeb_data<-tm_map(wcWeb,stripWhitespace)
wcWeb_data<-tm_map(wcWeb_data,tolower)
wcWeb_data<-tm_map(wcWeb_data,removeNumbers)
wcWeb_data<-tm_map(wcWeb_data,removePunctuation)
wcWeb_data<-tm_map(wcWeb_data,removeWords, stopwords("english"))

tdm_wcWeb <- TermDocumentMatrix(wcWeb_data)
TDM8 <- as.matrix(tdm_wcWeb)
v8 = sort(rowSums(TDM8), decreasing = TRUE)
summary(v8)

wc8 <-wordcloud(wcWeb, scale=c(5,0.5), max.words=300,
                random.order=FALSE, rot.per=0.35, use.r.layout=FALSE,
                colors=brewer.pal(8, "Dark2"))
######END##########################

##########WORDCLOUD##################
##########9.Website Building###################

Website_txt <- fiverr %>%
  select(Category, Skills) %>%
  filter(str_detect(Category, "Website Builders")) %>%
  select(Skills) %>%
  filter (!str_detect(Skills, "no description"))

write.table(Website_txt, "Website_txt.txt")
wc_Website <- readLines('Website_txt.txt')

wcWebsite <- Corpus(VectorSource(wc_Website))
inspect(wcWebsite)[1:10]

wcWebsite_data<-tm_map(wcWebsite,stripWhitespace)
wcWebsite_data<-tm_map(wcWebsite_data,tolower)
wcWebsite_data<-tm_map(wcWebsite_data,removeNumbers)
wcWebsite_data<-tm_map(wcWebsite_data,removePunctuation)
wcWebsite_data<-tm_map(wcWebsite_data,removeWords, stopwords("english"))

tdm_wcWebsite <- TermDocumentMatrix(wcWebsite_data)
TDM9 <- as.matrix(tdm_wcWebsite)
v9 = sort(rowSums(TDM9), decreasing = TRUE)
summary(v9)

wc9 <-wordcloud(wcWebsite, scale=c(5,0.5), max.words=300,
                random.order=FALSE, rot.per=0.35, use.r.layout=FALSE,
                colors=brewer.pal(8, "Set2"))
######END##########################

##########WORDCLOUD##################
##########10.Wordpress###################

WordPress_txt <- fiverr %>%
  select(Category, Skills) %>%
  filter(str_detect(Category, "WordPress")) %>%
  select(Skills) %>%
  filter (!str_detect(Skills, "no description"))

write.table(WordPress_txt, "WordPress_txt.txt")
wc_WordPress <- readLines('WordPress_txt.txt')

wcWordPress <- Corpus(VectorSource(wc_WordPress))
inspect(wcWordPress)[1:10]

wcWordPress_data<-tm_map(wcWordPress,stripWhitespace)
wcWordPress_data<-tm_map(wcWordPress_data,tolower)
wcWordPress_data<-tm_map(wcWordPress_data,removeNumbers)
wcWordPress_data<-tm_map(wcWordPress_data,removePunctuation)
wcWordPress_data<-tm_map(wcWordPress_data,removeWords, stopwords("english"))

tdm_wcWordPress <- TermDocumentMatrix(wcWordPress_data)
TDM10 <- as.matrix(tdm_wcWordPress)
v10 = sort(rowSums(TDM10), decreasing = TRUE)
summary(v10)

wc10 <-wordcloud(wcWordPress, scale=c(5,0.5), max.words=300,
                 random.order=FALSE, rot.per=0.35, use.r.layout=FALSE,
                 colors=brewer.pal(8, "Dark2"))
######END##########################
###########EXTRA. EXTRACTING TOP 10 FREQUENTLY USED WORDS########
words <- fiverr %>%
  select(Skills) %>%
  filter (!str_detect(Skills, "no description"))
write.table(words, "words.txt")
words2 <- readLines('words.txt')
words_c <- Corpus(VectorSource(words2))


words_c_data<-tm_map(words_c,stripWhitespace)
words_c_data<-tm_map(words_c_data,tolower)
words_c_data<-tm_map(words_c_data,removeNumbers)
words_c_data<-tm_map(words_c_data,removePunctuation)
words_c_data<-tm_map(words_c_data,removeWords, stopwords("english"))

tdm_words_c <- TermDocumentMatrix(words_c_data)
TDM11 <- as.matrix(tdm_words_c)
v11 = sort(rowSums(TDM11), decreasing = TRUE)
summary(v11)
dtm <- DocumentTermMatrix(words_c_data)
sparse <- removeSparseTerms(dtm, 0.80)
freq <- findFreqTerms(dtm,2)
head(freq, 10)

dtm <- TermDocumentMatrix(words_c_d)
m <- as.matrix(tdm_words_c)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

word_plot <- barplot(d[1:20,]$freq, las = 2, names.arg = d[1:20,]$word,
                     col ="skyblue", main ="Most frequent words",
                     ylab = "Word frequencies")

word_plot
ggplotly(word_plot)

plot_ly(d, x = wordnames, y = freq,
        mode = "markers", color = word, size = word)

###################################WORDCLOUD END############################
############################################################################
############################################################################

#CONFIRMED###########################################################################
############################################################################
by_Review <- fiverr %>%
  filter(Review < 4000) %>%
  group_by(Review, Start_Cost, Rate)# %>%

#CONFIRMED###########################################################################
############################################################################
##3DPLOT; cost, review by category
p <- plot_ly(by_Review, x = ~Response.hr., y = ~Review, z = ~Start_Cost,
             marker = list(color = ~Rate, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Response Time'),
                      yaxis = list(title = 'Number of Review'),
                      zaxis = list(title = 'Start_Cost')),
         annotations = list(
           x = 1.13,
           y = 1.05,
           text = 'Rate',
           xref = 'paper',
           yref = 'paper2',
           showarrow = FALSE
         ))
p
#END###########################################################################
############################################################################

#CONFIRMED###########################################################################
############################################################################
###FACETWRAP
pg <- ggplot(data = by_Review, aes(x = Start_Cost, y = Rate)) +
  geom_point(size = 1) +
  geom_smooth(aes(colour = Category, fill = Category)) + facet_wrap(~ Category)

gg <- ggplotly(pg)
gg
############################################################################
############################################################################




######################################################################
## joining location list with Code for mapping
world$Country <- trimws(world$Country)
colnames(world)[colnames(world)=="Country"] <- "Location"
joined <- left_join(by_location_all, world, by ="Location")

library(maps)
####Creating world map
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

world <- plot_geo(joined) %>%
  add_trace(
    z = ~allnumber, color = ~allnumber, colors = 'Blues',
    text = ~Location, locations = ~Code, marker = list(line = l)
  ) %>%
  colorbar(title = 'Number of freelancers') %>%
  layout(
    title = 'Fiverr World Freelancers',
    geo = g
  )
world



#define; freelancers by categories
by_category <- fiverr %>%
  group_by(Category) %>%
  summarise(number_cat = n())
plot_ly(by_category, x = ~Category, y = ~number_cat)

#Main Category growth by year
by_year <- fiverr %>%
  group_by(Category, Member_Since)
plot_ly(by_year, x = ~Category, color = ~Member_Since)


