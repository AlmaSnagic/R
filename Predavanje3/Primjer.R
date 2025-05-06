ggplot(Titanic, aes(x = Pclass)) +
  geom_bar(fill = "steelblue")+
  labs(Titanic="Pclass distribution")

ggplot(Titanic, aes(x = Fare)) +
  geom_histogram(binwidth = 50)

ggplot(Titanic, aes(x = Pclass, fill = factor(Sex))) +
  geom_bar(position = "dodge")

ggplot(Titanic, aes(x = Age, y= Fare)) +
  geom_point(color = "blue")+
  geom_smooth(method = "lm",
              color = "red", se = FALSE)

