#load data
ATP_W116 <- read_sav("~/Desktop/Spring 2024/Data Analysis/09-presentations/ATP W116.sav")

#data transformation
ATP1 <-
  ATP_W116 |>
  mutate(TrumpTherm = as.numeric(THERMTRUMP_W116)
  )

tab = 
  ATP_W116 |>
  count(DRLEAD_W116, ALLIES_W116) |>
  mutate(PartyPref = factor(DRLEAD_W116, 
                            levels = c("1","2","3","99"), ordered = TRUE),
         Allies = factor(ALLIES_W116,
                         levels = c("1", "2", "99"), ordered = TRUE)) |>
  select(PartyPref, Allies, n) 

tab1 = as.data.frame(tab)

tab2 =
  ATP_W116 |>
  count(DRLEAD_W116, SUPERPWR_W116) |>
  mutate(Superpower = factor(SUPERPWR_W116, 
                             levels = c("1", "2", "99"), ordered = TRUE),
         PartyPref = factor(DRLEAD_W116, 
                            levels = c("1","2","3","99"), ordered = TRUE)) |>
  select(PartyPref, Superpower, n)

tab3 =
  ATP1 |>
  select(DRLEAD_W116, ALLIES_W116, TrumpTherm) |>
  mutate(PartyPref = factor(DRLEAD_W116, 
                            levels = c("1","2","3","99"), ordered = TRUE),
         Allies = factor(ALLIES_W116,
                         levels = c("1", "2", "99"), ordered = TRUE)) |>
  filter(
    PartyPref == "1") |>
  mutate(TrumpApprove = 
           case_when(
             TrumpTherm < 50 ~ "0",
             TrumpTherm > 50 ~ "1"
           )) |>
  count(Allies, TrumpApprove) |>
  select(Allies, TrumpApprove, n)

P3 =
  tab2 |>
  filter(PartyPref != "99",
         Superpower != "99") |>
  ggplot(aes(fill = Superpower, y = n, x = PartyPref)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  theme(panel.background = element_rect(color = "black")) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank())

#super power visualization
P_Superpower =
  P3 +
  scale_fill_manual(
    values = c("#EEBAB4", "#7CA1CC"),
    name = "In the future, do you think...",
    breaks = c("1", "2"),
    labels = c("U.S. policies should try to keep it so\nAmerica is the only military superpower (1)", 
               "It would be acceptable if another country\nbecame as militarily powerful as the U.S. (2)")) +
  theme(legend.position = c(.74, .835)) +
  theme(legend.spacing.y = unit(1, "mm")) +
  guides(fill = guide_legend(byrow = TRUE)) +
  scale_y_continuous(limits = c(0, 1800),
                     name = NULL,
                     expand = c(0,0)) +
  scale_x_discrete(breaks = c("1", "2", "3"),
                   labels = c("Favors Republicans", "Favors Democrats", "Unsure"),
                   name = NULL) +
  labs(title = "All Respondents") +
  theme(
    axis.text = element_text(color = "black", size = 7),
    legend.text = element_text(color = "black", size = 5),
    legend.title = element_text(color = "black", size = 5.5),
    plot.title = element_text(hjust = .5),
    axis.ticks.x = element_blank()
  ) 

P_Superpower

#Allies visualization
P1 =
  tab1 |>
  filter(PartyPref != "99",
         Allies != "99") |>
  ggplot(aes(fill = Allies, y = n, x = PartyPref)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw() +
  theme(panel.background = element_rect(color = "black")) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(name = NULL,
                     expand = c(0,0),
                     limits = c(0, 1800)) +
  scale_x_discrete(breaks = c("1", "2", "3"),
                   labels = c("Favors R", "Favors D", "Unsure"),
                   name = NULL) +
  scale_fill_manual(values = c("#F05039", "#1F449C"),
                    guide = FALSE) +
  theme(axis.text = element_text(color = "black", size = 8)) +
  theme(
    plot.title = element_text(hjust = .5),
    axis.ticks.x = element_blank()) +
  labs(title = "All Respondents")

P2 =
  tab3 |>
  filter(Allies != "99",
         TrumpApprove != "NA") |>
  ggplot(aes(fill = Allies, y = n, x = TrumpApprove)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw() +
  theme(panel.background = element_rect(color = "black")) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(name = NULL,
                     expand = c(0,0),
                     limits = c(0, 950)) +
  scale_x_discrete(breaks = c("0", "1"),
                   labels = c("Trump\nDisapprove","Trump\nApprove"),
                   name = NULL) +
  labs(title = "Republicans") +
  scale_fill_manual(values = c("#F05039", "#1F449C"),
                    name = "The U.S...",
                    breaks = c("1", "2"),
                    labels = c("should take allies'\ninterests into account (1)",
                               "should follow its own\nnational interests (2)")) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.text = element_text(size = 7),
    axis.text = element_text(color = "black", size = 8),
    plot.title = element_text(hjust = .5))

layout <- 
  "AAAABB
  AAAABB
  AAAABB
  AAAA##"

P_allies =
  P1 + P2 +
  plot_layout(
    design = layout)