"""
==============================================
  EXERCICES PYTHON — Niveau Lycée
  Fichier 5 : Fonctions
==============================================
"""

# ===========================================================
# COURS RAPIDE
# ===========================================================
# Une fonction = un bloc de code réutilisable avec un nom.
#
#   def nom_fonction(parametre1, parametre2):
#       ...
#       return resultat
#
# Appel :
#   resultat = nom_fonction(valeur1, valeur2)
#
# - "def"    déclare la fonction
# - "return" renvoie une valeur (optionnel)
# - On peut avoir 0, 1 ou plusieurs paramètres
# ===========================================================


# -----------------------------------------------------------
# EXERCICE 1 — Carré d'un nombre
# -----------------------------------------------------------
# Écris une fonction carre(n) qui retourne n au carré.
# Teste-la avec plusieurs valeurs.

print("--- Exercice 1 ---")

# Ton code ici :


# ✅ CORRECTION
# def carre(n):
#     return n * n
#
# print(carre(3))    # 9
# print(carre(7))    # 49
# print(carre(-4))   # 16


# -----------------------------------------------------------
# EXERCICE 2 — Maximum de deux nombres
# -----------------------------------------------------------
# Écris une fonction maximum(a, b) qui retourne
# le plus grand des deux nombres (sans utiliser max()).

print("\n--- Exercice 2 ---")

# Ton code ici :


# ✅ CORRECTION
# def maximum(a, b):
#     if a >= b:
#         return a
#     else:
#         return b
#
# print(maximum(5, 8))    # 8
# print(maximum(10, 3))   # 10
# print(maximum(7, 7))    # 7


# -----------------------------------------------------------
# EXERCICE 3 — Factorielle
# -----------------------------------------------------------
# La factorielle de n (notée n!) est le produit de tous
# les entiers de 1 à n. Exemple : 4! = 1×2×3×4 = 24
# Écris une fonction factorielle(n) qui calcule n!

print("\n--- Exercice 3 ---")

# Ton code ici :


# ✅ CORRECTION
# def factorielle(n):
#     resultat = 1
#     for i in range(1, n + 1):
#         resultat *= i          # équivalent à resultat = resultat * i
#     return resultat
#
# print(factorielle(0))   # 1
# print(factorielle(5))   # 120
# print(factorielle(10))  # 3628800


# -----------------------------------------------------------
# EXERCICE 4 — Palindrome
# -----------------------------------------------------------
# Un palindrome se lit pareil dans les deux sens.
# Exemples : "radar", "kayak", "elle"
# Écris une fonction est_palindrome(mot) qui retourne
# True si le mot est un palindrome, False sinon.

print("\n--- Exercice 4 ---")

# Ton code ici :


# ✅ CORRECTION
# def est_palindrome(mot):
#     mot = mot.lower()          # on ignore la casse
#     return mot == mot[::-1]    # [::-1] inverse la chaîne
#
# print(est_palindrome("radar"))   # True
# print(est_palindrome("python"))  # False
# print(est_palindrome("Elle"))    # True


# -----------------------------------------------------------
# EXERCICE 5 — Moyenne d'une liste
# -----------------------------------------------------------
# Écris une fonction moyenne(liste_notes) qui prend une liste
# de notes et retourne leur moyenne.
# Si la liste est vide, retourne 0.

print("\n--- Exercice 5 ---")

# Ton code ici :


# ✅ CORRECTION
# def moyenne(liste_notes):
#     if len(liste_notes) == 0:
#         return 0
#     return sum(liste_notes) / len(liste_notes)
#
# notes = [12, 15, 8, 17, 10]
# print(f"Moyenne : {moyenne(notes):.2f}")   # 12.40
# print(f"Liste vide : {moyenne([])}")        # 0


# -----------------------------------------------------------
# EXERCICE 6 — Valeur absolue
# -----------------------------------------------------------
# Écris une fonction valeur_absolue(n) qui retourne
# la valeur absolue de n SANS utiliser abs().

print("\n--- Exercice 6 ---")

# Ton code ici :


# ✅ CORRECTION
# def valeur_absolue(n):
#     if n < 0:
#         return -n
#     return n
#
# print(valeur_absolue(-7))   # 7
# print(valeur_absolue(5))    # 5
# print(valeur_absolue(0))    # 0
