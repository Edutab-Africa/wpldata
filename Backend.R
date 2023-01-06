suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(writexl))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(mudata2))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library("rstudioapi"))
suppressPackageStartupMessages(library(openxlsx))
suppressPackageStartupMessages(library(viridis))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lattice))
suppressPackageStartupMessages(library(formattable))
suppressPackageStartupMessages(library(wordcloud2))
suppressPackageStartupMessages(library(ggwordcloud))



setwd(dirname(getActiveDocumentContext()$path))
##### 
Completed_math4<-read_excel("progress.maths-all (4).xlsx",sheet = "progress.maths-all (4)")
completed_sel4 <- read_excel("progress.sel-all (3).xlsx" , sheet =  "progress.sel-all (3)")


#####################################################################################################################3
Somali_math4<-Completed_math4 %>%
  rename("Email" ="Email address","Kite"="1. This Kite is Too Heavy (Somali)",
         "Circles" ="2. Do Circles Make Good Wheels (Somali)","Limbo"="3. Under the Limbo Pole (Somali)",
         "Flower"="4. How Tall is This Flower (Somali)","Triangle"="5. Triangle Training (Somali)",
         "Turtle" ="6. Slow Turtle, Fast Banana  (Somali)","Squares"="7. My Cousins are Squares  (Somali)",
         "Bench"="8. This Bench Isn't Wide Enough  (Somali)","Chick"="9. Big, Chick, Little Chick (Somali)",
         "Play"="10. Let's Play More or Less  (Somali)","Painting"="11. Painting in Between  (Somali)",
         "Mango"="12. Put the Mango in the Cart  (Somali)","Dancing"="13. Dancing Fast, Dancing Slow (Somali)",
         "Heavier"="14.What's Heavier Than a Mango (Somali)","Leaves"="15. More Leaves on the Tree (Somali)",
         "Ears"="16. Big Ears, Small Ears (Somali)","Hen"="17. Height of the Hen House (Somali)",
         "Chickens"="18. The Chickens are Fast Asleep (Somali)","Bubble"="19. How Big is This Bubble (Somali)",
         "Heaviest"="20. Who's the Heaviest? (Somali)","Triangle_explorers"="21. Triangle Explorers (Somali)",                                     
         "Story"="22. Three Sides to Every Story (Somali)","Trains"="23.Between Two Trains (Somali)",
         "Box"="24. Flowers in the Box (Somali)","Table"="25. Chickens Under the Table (Somali)",
         "Tall_turtle"="26. How Tall is Your Turtle? (Somali)","More_less"="27. More and Less (Somali)",
         "Width"="28. Width(Somali)","over_under"="29. Over and Under (Somali)","over_under2"="30. Over and Under (Somali)",
         "Circle"="31. Circle (Somali)","Circle2"="32. Circle (Somali)","Between"="33. Between (Somali)",
         "Big_small"="34. Big and Small (Somali)","Squares2"="35. Squares (Somali)","Squares3"="36. Squares (Somali)",
         "Width2"="37. Width (Somali)","Inside_outside"="38. Inside and Outside (Somali)") %>%
  select(Email,Kite,Circles,Limbo,Flower,Triangle,Turtle,Squares,Bench,
         Chick,Play,Painting,Mango,Dancing,Heavier,Leaves,Ears,Hen,Chickens,
         Bubble,Heaviest,Triangle_explorers,Story,Trains,Box,Table,Tall_turtle,
         More_less,Width,over_under,over_under2,Circle,Circle2,Between,
         Big_small,Squares2,Squares3,Width2,
         Inside_outside) %>%
  pivot_longer (!c(Email), names_to = "Videos", values_to = "Completed") %>%
  filter(Completed != "Not completed") %>%
  group_by(Videos) %>%
  summarise(cnt = n())
as.data.frame(Somali_math4)
write.csv(Somali_math4, "Somali_math4.csv")

###########################################################################################################
English_math4 <- Completed_math4 %>%
  rename("Email" ="Email address",
         "Kite"="1. This Kite is Too Heavy (English)","Circles"="2. Do Circles Make Good Wheels? (English)",
         "Limbo"="3. Under The Limbo Pole (English)","Flower"="4. How Tall Is This Flower? (English)",
         "Training"="5. Triangle Training (English)","Squares"="7. My Cousins are Squares (English)",
         "Bench"="8. This Bench Isn't Wide Enough (English)","Chick"="9. Big, Chick, Little Chick (English)",
         "More_less"="10. Let's Play More or Less (English)","Heavier"="14. What's Heavier Than a Mango (English)",
         "Leaves"="15. More Leaves on the Tree (English)","Ears"="16. Big Ears, Small Ears (English)",
         "Height"="17. Height of the Hen House (English)","Bubble"="19. How Big is This Bubble? (English)",
         "Heaviest"="20. Who's the Heaviest? (English)","Triangle_Explorers"="21.Triangle Explorers (English)",
         "Story"="22. Three Sides to Every Story (English)","Tall_turtle"="26. How Tall is Your Turtle? (English)",
         "More"="27. More and Less (English)","Width"="28. Width (English)","Circle"="31. Circle (English)",
         "Circle2"="32. Circle (English)","Big_small"="34. Big and Small (English)","Squares2"="35. Squares (English)", 
         "Squares3"="36. Squares (English)","Width2"="37. Width (English)") %>%
  select(Email,Kite,Circles,Limbo,Flower,Training,Squares,Bench,Chick,More_less,
         Heavier,Leaves,Ears,Height,Bubble,Heaviest,Triangle_Explorers,Story,Tall_turtle,
         More,Width,Circle,Circle2,Big_small,Squares2,Squares3,Width2) %>%
  pivot_longer (!c(Email), names_to = "Videos", values_to = "Completed") %>%
  filter(Completed != "Not completed") %>%
  group_by(Videos) %>%
  summarise(cnt = n())
as.data.frame(English_math4)
write.csv(English_math4, "English_math4.csv")


##############################################################################################
completed_sel4 <- read_excel("progress.sel-all (3).xlsx" , sheet =  "progress.sel-all (3)")
##############################################################################
English_SEL4 <- completed_sel4 %>% 
  rename("email" = "Email address","cook_frust"="1. Cookie is Frustrated (English)","zoe_jeal"="2. Zoe is Jealous (English)",
         "cook_sad"="3. Cookie is Sad (English)","eleph_fear"="4. Elephant is Fearful (English)",
         "cook_deter"="5. Cookie is Determined (English)","zoe_ang"="6. Zoe is Angry (English)",
         "Ele_nerv"="7. Elephant is Nervous (English)","elmo_sad"="8. Elmo is Sad (English)",
         "zoe_exci"="9. Zoe is Excited (English)","elmo_jeal"="10. Elmo is Jealous (English)",
         "Grover_frust"="11. Grover is Frustrated (English)","elmo_fear"= "12. Elmo is Fearful (English)",
         "cook_fear"="13. Cookie is Fearful (English)","elmo_ang"="14. Elmo is Angry (English)",
         "Grove_exci"="15. Grover is Excited (English)","eleph_exci"="16. Elephant is Excited (English)", 
         "zoe_nerv"="17. Zoe is Nervous (English)","elmo_nerv"="18. Elmo is Nervous (English)",
         "grove_ang"="19. Grover is Angry (English)","turt_deter"="20. Turtle is Determined (English)",
         "zoe_deter"="21. Zoe is Determined (English)", "turt_jeal"="22. Turtle is Jealous (English)",
         "grove_sad"="23. Grover is Sad (English)", "turt_ang"="24. Turtle is Angry (English)",
         "grove_nerv"="25. Grover is Nervous (English)","elmo_frust"="26. Elmo is Frustrated (English)")%>%
  select(email,cook_frust,zoe_jeal,cook_sad,eleph_fear,cook_deter,zoe_ang,Ele_nerv,elmo_sad,zoe_exci,
         elmo_jeal,Grover_frust,elmo_fear,cook_fear,elmo_ang,Grove_exci,eleph_exci,zoe_nerv,elmo_nerv,
         grove_ang,turt_deter,zoe_deter,turt_jeal,grove_sad,turt_ang,grove_nerv,elmo_frust) %>%
  pivot_longer (!c(email), names_to = "Videos", values_to = "Completed") %>%
  filter(Completed != "Not completed") %>%
  group_by(Videos) %>%
  summarise(cnt = n())
as.data.frame(English_SEL4)
write.csv(English_SEL4, "English_SEL4.csv")



####################################################################################################3333


Somali_SEL4<- completed_sel4 %>%
  rename("email"="Email address","Elmo_angry"="14. Elmo is angry (Somali)",
         "Turtle_jealous"="22. Turtle is jealous (Somali)",
         "elmo_frustrated"="26. Elmo is frustrated (Somali)",
         "Ameera_sad"="29. Ameera is sad (Somali)",
         "Cookie_nervous"="30. Cookie is nervous (Somali)",
         "Ameera_frustrated"="36. Ameera is Frustrated (Somali)",
         "cookie_frustrated"="1.  Cookie is frustrated (Somali)",
         "zoe_jealous"="2.  Zoe is jealous (Somali)",
         "cookie_sad"="3. Cookie is sad (Somali)",
         "elephant_fearful"="4. Elephant is fearful (Somali)",
         "cookie_determined"="5. Cookie is determined (Somali)",
         "zoe_angry"="6. Zoe is angry (Somali)",
         "elephant_nervous"="7. Elephant is Nervous (Somali)",
         "elmo_sad"="8. Elmo is sad (Somali)",
         "zoe_excited"="9. Zoe is excited (Somali)",
         "elmo_jealous"="10. Elmo is Jealous (Somali)",
         "grover_frustrated"="11. Grover is frustrated (Somali)",
         "elmo_fearful"="12. Elmo is Fearful (Somali)",
         "cookie_fearful"="13. Cookie is fearful (Somali)",
         "grover_excited"="15. Grover is excited (Somali)",
         "elephant_excited"="16. Elephant is excited (Somali)",
         "zoe_nervous"="17.  Zoe is nervous (Somali)",
         "elmo_nervous"="18.  Elmo is nervous (Somali)",
         "grover_angry"="19.Grover is Angry (Somali)",
         "turtle_determined"="20. Turtle is determined (Somali)",
         "zoe_determined"="21. Zoe is determined (Somali)",
         "grover_sad"="23. Grover is sad (Somali)",
         "turtle_angry"="24. Turtle is angry (Somali)",
         "grover_nervous"="25. Grover is nervous (Somali)",
         "cook_excited"="27. Cook is excited (Somali)",
         "Grover_determined"="28. Grover is determined (Somali)",
         "Zoe_fearful"="31. Zoe is Fearful (Somali)",
         "Tutle_sad"="32. Turtle is Sad (Somali)",
         "Elephant_frustrated"="33. Elephant is Frustrated (Somali)",
         "Ameera_angry"="34. Ameera is Angry (Somali)",
         "ameera_excited"="35. Ameera is Excited (Somali)",
         "ameera_jealous"="37. Ameera is Jealous (Somali)",
         "turtle_fearful"="38. Turtle is fearful (Somali)") %>%
  select(email,Elmo_angry,Turtle_jealous,elmo_frustrated,Ameera_sad,Cookie_nervous,
         Ameera_frustrated,cookie_frustrated,zoe_jealous,cookie_sad,elephant_fearful,
         cookie_determined,zoe_angry,elephant_nervous,elmo_sad,zoe_excited,elmo_jealous,
         grover_frustrated,elmo_fearful,cookie_fearful,grover_excited,elephant_excited,
         zoe_nervous,elmo_nervous,grover_angry,turtle_determined,zoe_determined,
         grover_sad,turtle_angry,grover_nervous,cook_excited,Grover_determined,
         Zoe_fearful,Tutle_sad,Elephant_frustrated,Ameera_angry,ameera_excited,
         ameera_jealous,turtle_fearful) %>%
  pivot_longer (!c(email), names_to = "Videos", values_to = "Completed") %>%
  filter(Completed != "Not completed") %>%
  group_by(Videos) %>%
  summarise(cnt = n())
as.data.frame(Somali_SEL4)
write.csv(Somali_SEL4, "Somali_SEL4.csv")
 

######################################################################################################
