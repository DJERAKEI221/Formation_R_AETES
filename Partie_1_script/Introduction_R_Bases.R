
# Introduction à R : Création d'objets et bases du langage

# ---- Variables simples ----
# Création de variables de différents types
x <- 5                # Variable numérique
y <- "Bonjour"         # Variable de type caractère (texte)
z <- TRUE              # Variable booléenne (logique : TRUE/FALSE)

# Affichage des variables
x
y
z

# ---- Vecteurs ----
# Un vecteur est une séquence de données du même type


vec <- c(1, 2, 3, 4, 5)

vec

# Affiche la longeur

fruits <- c("pomme", "banane", "mangue")

fruits

length(fruits)  
liste <- list(nom ='Mike' , Age ='12',)

# ---- Matrices ----
# Une matrice est un tableau à deux dimensions
mat <- matrix(1:9, nrow=3, ncol=3)
mat


# ---- Listes ----
# Une liste peut contenir des objets de types différents
liste <- list(Nom="Votre nom", Age=25, Score=c(90, 85, 88))

liste



# Vecteur de niveaux d'éducation
education <- c("Licence", "Master", "Doctorat", "Licence", "Master")

# Facteur avec un ordre logique
education_facteur <- factor(education)
# Afficher
education_facteur

# ---- Data frames ----
# Structure de données en forme de tableau (colonnes de types différents)
df <- data.frame(Prenom=c("Ahmat", "Paul", "Alain"), Age=c(25, 30, 25), Ville=c("Ndjam", "Abeche", "Sarh"))
print(df)















# ---- Fonction personnalisée ----

# Définir une fonction qui additionne deux nombres
somme_deux_nombres <- function(a, b) {
  return(a + b)
}

# Utilisation de la fonction
resultat <- somme_deux_nombres(10, 20)
print(resultat)


# Créer un dossier depuis R
dir.create("chemin/vers/MonProjet_R")  # Remplacez par votre chemin



# dir.create("C:\\Users\\yvesd\\Desktop\\AETES\\Formation_R_AETES\\Mon_Projet_R")

# dir.create("C:/Users/yvesd/Desktop/AETES/Formation_R_AETES/Mon_Projet_R")


# Définir comme répertoire de travail Avec la Commande setwd()

setwd("chemin/vers/MonProjet_R")

here()  # Montre le chemin racine
getwd()


