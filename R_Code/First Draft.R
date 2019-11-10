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

#Visualising Female Share Middle Senior Management
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
bar = bar + scale_fill_brewer(palette = "Pastel1")
bar = bar + geom_hline(aes(yintercept=30.4))
bar = bar + labs(title = "Top & Bottom 12 Countries")
bar = bar + labs(y="Female Share of Middle & Senior Management (%)")
bar = bar + labs(x = NULL)
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
bar1 = bar1 + scale_fill_brewer(palette = "Pastel1")
bar1 = bar1 + geom_hline(aes(yintercept=33.7))
bar1 = bar1 + labs(title = "Top & Bottom 12 Countries")
bar1 = bar1 + labs(y="Female Share of Firm Ownership (%)")
bar1 = bar1 + labs(x = NULL)
common_bottom = merge(fem_owner_top, senior_manage_top)
detach(fem_owner_top)

#Visualising Female Share in Parliament 
fem_par = df[c(1,2,7)]
fem_par = na.omit(fem_par)
fem_par = fem_par[c(1:193),]
attach(fem_par)
Female_Share_Parliaments = as.numeric(levels(Female_Share_Parliaments)[Female_Share_Parliaments])
fem_par = fem_par[order(Female_Share_Parliaments),]
fem_par = na.omit(fem_par)
fem_par_top = fem_par[c(181:193, 1:12),]
detach(fem_par)
attach(fem_par_top)
fem_par_top$Female_Share_Parliaments = as.numeric(levels(fem_par_top$Female_Share_Parliaments)[fem_par_top$Female_Share_Parliaments])
bar2 = ggplot(fem_par_top, aes(x= reorder(Countries, Female_Share_Parliaments), y=Female_Share_Parliaments, fill = Continent)) + geom_bar(stat = "identity") + coord_flip()
bar2 = bar2 + scale_fill_brewer(palette = "Pastel1")
bar2 = bar2 + geom_hline(aes(yintercept=21.8))
bar2 = bar2 + labs(title = "Top & Bottom 12 Countries")
bar2 = bar2 + labs(x = NULL)

