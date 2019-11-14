#Data Cleaning 
df = read.csv(file="WV.csv", header=T)
colnames(df)[colnames(df) == "Life.expectancy.at.birth"] = "Life Expectancy_Males"
colnames(df)[colnames(df) == "X..."] = "Countries"
colnames(df)[colnames(df) == "X"] = "Life_Expectancy_Females"
df = df[-c(5:9)]
colnames(df)[5] = "Firms_Female_Ownership"
colnames(df)[6] = "Female_Share_Middle_Senior_Management"
colnames(df)[7] = "Female_Share_Parliaments"
colnames(df)[8] = "Gender_Non_Discrimination_Constitution"
df = df[-c(1:3),]
df = df[c(1:230), ]
df[df==".."] = NA

library(ggplot2)

#Visualising Female Share Middle Senior  Management
senior_manage = df[c(1, 2,6)]
senior_manage = na.omit(senior_manage)
attach(senior_manage)
senior_manage$Female_Share_Middle_Senior_Management = as.numeric(levels(Female_Share_Middle_Senior_Management)[Female_Share_Middle_Senior_Management])
senior_manage = senior_manage[order(Female_Share_Middle_Senior_Management),]
senior_manage = na.omit(senior_manage)
senior_manage_top= senior_manage[c(81:93, 1:12),]
senior_manage_top = na.omit(senior_manage_top)

detach(senior_manage)
attach(senior_manage_top)

bar = ggplot(senior_manage_top, aes(x= reorder(Countries, Female_Share_Middle_Senior_Management), y=Female_Share_Middle_Senior_Management, fill = Continent)) + geom_bar(stat = "identity") + coord_flip()
bar = bar + scale_fill_manual("legend", values = c("Africa" = "palevioletred3", "Asia" = "lightsalmon3", "Europe" = "thistle3", "N.America" = "lightgoldenrod3", "Oceania" = "darkseagreen3"))
bar = bar + geom_hline(aes(yintercept=30.4))
bar = bar + labs(title = "Countries with the highest and lowest proportion \nof female middle and senior managers", subtitle = "Proportion calculated by dividing female managers by total working females, Data from 2011 - 2017", caption = "International Labour Organisation (ILO)")
bar = bar + labs(y="Proportion of Female Middle & Senior Management (%)")
bar = bar + labs(x = NULL)
bar = bar + geom_text(x =1, y =33, label = "Average among 92 countries", size =3)
bar = bar + theme(plot.title = element_text(size = 15, face = 'bold')) + theme(plot.subtitle = element_text(size = 8)) + theme(plot.caption = element_text(size = 8)) + theme(legend.position = c(0.89, 0.17))
detach(senior_manage_top)


#Visualising Female Ownership of Firms
fem_owner = df[c(1,2,5)]
fem_owner = na.omit(fem_owner)
fem_owner = fem_owner[c(1:108),]
attach(fem_owner)
fem_owner$Firms_Female_Ownership = as.numeric(levels(Firms_Female_Ownership)[Firms_Female_Ownership])
fem_owner = fem_owner[order(Firms_Female_Ownership),]
fem_owner_top= fem_owner[c(96:108, 1:12),]
detach(fem_owner)
attach(fem_owner_top)
bar1 = ggplot(fem_owner_top, aes(x= reorder(Countries, Firms_Female_Ownership), y=Firms_Female_Ownership, fill = Continent)) + geom_bar(stat = "identity") + coord_flip()
bar1 = bar1 + scale_fill_manual("Continent", values = c("Africa" = "palevioletred3", "Asia" = "lightsalmon3", "Europe" = "thistle3", "S.America" = "aquamarine3"))
bar1 = bar1 + geom_hline(aes(yintercept=33.7), linetype = "dashed")
bar1 = bar1 + labs(title = "Countries with the highest and lowest share of firms \nwhich counts a female as one of its principal owners",   subtitle = "Data from 2011 to 2018" )
bar1 = bar1 + labs(y="Firms with Female Ownership (%)")
bar1 = bar1 + labs(x = NULL, caption = "Source: World Bank and Enterprise Surveys")
bar1 = bar1 + theme(plot.title = element_text(size = 15.5, face = 'bold')) + theme(plot.subtitle = element_text(size = 8)) + theme(plot.caption = element_text(size = 8)) + theme(legend.position = c(0.90, 0.15))
bar1 = bar1 + geom_text(x =1, y =35, label = "Average among 108 countries", size =3)
common_bottom = merge(fem_owner_top, senior_manage_top)
detach(fem_owner_top)

#Visualising Female Share in Parliament 
fem_par = df[c(1,2,7)]
fem_par = na.omit(fem_par)
fem_par = fem_par[c(1:193),]
attach(fem_par)
fem_par$Female_Share_Parliaments = as.numeric(levels(fem_par$Female_Share_Parliaments)[fem_par$Female_Share_Parliaments])
fem_par = fem_par[order(fem_par$Female_Share_Parliaments),]
fem_par = na.omit(fem_par)
fem_par_top = fem_par[c(181:193, 1:12),]
detach(fem_par)
attach(fem_par_top)
fem_par_top$Female_Share_Parliaments = as.numeric(levels(fem_par_top$Female_Share_Parliaments)[fem_par_top$Female_Share_Parliaments])
bar2 = ggplot(fem_par_top, aes(x= reorder(Countries, Female_Share_Parliaments), y=Female_Share_Parliaments, fill = Continent)) + geom_bar(stat = "identity") + coord_flip()
bar2 = bar2 + scale_fill_manual("Continent", values = c("Africa" = "palevioletred3", "Asia" = "lightsalmon3", "Europe" = "thistle3","N.America" = "lightgoldenrod3", "S.America" = "aquamarine3", "Oceania" = "darkseagreen3"))
bar2 = bar2 + geom_hline(aes(yintercept=21.8), linetype = "dashed")
bar2 = bar2 + labs(title = "Countries with the highest and lowest female share \nof national parliament seats", subtitle = "Data from 2018" , caption = "Source: Inter-Parliamentary Union")
bar2 = bar2 + labs(y="Female Share of National Parliament Seats (%)")
bar2 = bar2 + labs(x = NULL)
bar2 = bar2 + theme(plot.title = element_text(size = 15.2, face = 'bold')) + theme(plot.subtitle = element_text(size = 8)) + theme(plot.caption = element_text(size = 8)) + theme(legend.position = c(0.89, 0.24))
bar2 = bar2 + geom_text(x =1, y =28, label = "Average among 196 countries", size =3)

