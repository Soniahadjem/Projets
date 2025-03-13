library(tidyverse)

# Importation des données
data <- read.csv('students.csv')
# calculer la moyenne
data <- data %>%
  mutate(Moyenne = (Note_Math + Note_Français) / 2)

# Filtrer les étudiants ayant une moyenne > 14
students_filtered <- data %>%
  filter(Moyenne > 14)

print(students_filtered)

# Trier les étudiants par moyenne décroissante
students_sorted <- data %>%
  arrange(desc(Moyenne))

print(students_sorted)

# Histogramme des notes de mathématiques
ggplot(data, aes(x = Note_Math)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogramme des notes de mathématiques",
       x = "Note en Mathématiques",
       y = "Fréquence")

# Diagramme en barres des moyennes par sexe
data %>%
  group_by(Sexe) %>%
  summarise(Moyenne = mean(Moyenne)) %>%
  ggplot(aes(x = Sexe, y = Moyenne)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Moyennes par sexe",
       x = "Sexe",
       y = "Moyenne")

# Nuage de points des notes de math vs notes de français
ggplot(data, aes(x = Note_Math, y = Note_Français)) +
  geom_point(color = "red") +
  labs(title = "Notes de Math vs Notes de Français",
       x = "Note en Mathématiques",
       y = "Note en Français")


# Calculer la moyenne générale des étudiants
mean_generale <- mean(data$Moyenne, na.rm = TRUE)
print(mean_generale)

# Moyenne par sexe
moyenne_par_sexe <- data %>%
  group_by(Sexe) %>%
  summarise(Moyenne = mean(Moyenne))

print(moyenne_par_sexe)

# Test de corrélation
cor.test(data$Note_Math, data$Note_Français)

# Fonction de classification
classify_student <- function(moyenne) {
  if (moyenne >= 16) {
    return("Excellent")
  } else if (moyenne >= 12) {
    return("Bien")
  } else {
    return("Médiocre")
  }
}
# Appliquer la fonction et créer la colonne "Categorie"
data <- data %>%
  mutate(Categorie = sapply(Moyenne, classify_student))

# Afficher les catégories
print(data)

# Exporter les données
write.csv(data, "students_analyse.csv", row.names = FALSE)
 