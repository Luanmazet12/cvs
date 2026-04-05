"""
==============================================
  EXERCICES PYTHON — Niveau Lycée
  Fichier 4 : Listes
==============================================
"""

# ===========================================================
# COURS RAPIDE
# ===========================================================
# Une liste = une collection ordonnée d'éléments.
#
#   notes = [12, 15, 9, 18, 11]
#
# Accès par index (commence à 0) :
#   notes[0]   -> 12   (premier élément)
#   notes[-1]  -> 11   (dernier élément)
#
# Méthodes utiles :
#   notes.append(14)   -> ajoute 14 à la fin
#   notes.remove(9)    -> supprime la valeur 9
#   notes.sort()       -> trie la liste
#   len(notes)         -> nombre d'éléments
#   sum(notes)         -> somme des éléments
#   min(notes)         -> plus petite valeur
#   max(notes)         -> plus grande valeur
# ===========================================================


# -----------------------------------------------------------
# EXERCICE 1 — Afficher une liste
# -----------------------------------------------------------
# Crée une liste de 5 fruits et affiche-les un par un
# avec une boucle for.

print("--- Exercice 1 ---")

# Ton code ici :


# ✅ CORRECTION
# fruits = ["pomme", "banane", "cerise", "mangue", "fraise"]
# for fruit in fruits:
#     print(fruit)


# -----------------------------------------------------------
# EXERCICE 2 — Saisie de notes
# -----------------------------------------------------------
# Demande 5 notes à l'utilisateur, stocke-les dans une liste,
# puis affiche la moyenne, la meilleure note et la plus mauvaise.

print("\n--- Exercice 2 ---")

# Ton code ici :


# ✅ CORRECTION
# notes = []
# for i in range(5):
#     n = float(input(f"Note {i+1} : "))
#     notes.append(n)
#
# moyenne = sum(notes) / len(notes)
# print(f"Moyenne   : {moyenne:.2f}")   # :.2f = 2 décimales
# print(f"Meilleure : {max(notes)}")
# print(f"Pire      : {min(notes)}")


# -----------------------------------------------------------
# EXERCICE 3 — Filtrer une liste
# -----------------------------------------------------------
# À partir de la liste ci-dessous, crée une nouvelle liste
# contenant uniquement les nombres pairs.

print("\n--- Exercice 3 ---")
nombres = [3, 8, 15, 2, 7, 10, 21, 4, 13, 6]

# Ton code ici :


# ✅ CORRECTION
# pairs = []
# for n in nombres:
#     if n % 2 == 0:
#         pairs.append(n)
# print("Nombres pairs :", pairs)
#
# # Version compacte (liste en compréhension) — niveau avancé :
# pairs = [n for n in nombres if n % 2 == 0]
# print("Nombres pairs :", pairs)


# -----------------------------------------------------------
# EXERCICE 4 — Inverser une liste
# -----------------------------------------------------------
# Inverse l'ordre de la liste suivante SANS utiliser .reverse()

print("\n--- Exercice 4 ---")
lettres = ["a", "b", "c", "d", "e"]

# Ton code ici :


# ✅ CORRECTION
# inversee = []
# for i in range(len(lettres) - 1, -1, -1):
#     inversee.append(lettres[i])
# print(inversee)
#
# # Astuce Python (slicing) :
# print(lettres[::-1])


# -----------------------------------------------------------
# EXERCICE 5 — Recherche dans une liste
# -----------------------------------------------------------
# Demande un mot à l'utilisateur.
# Vérifie s'il est dans la liste de mots interdits ci-dessous.

print("\n--- Exercice 5 ---")
mots_interdits = ["spam", "pub", "virus", "hack"]

# Ton code ici :


# ✅ CORRECTION
# mot = input("Entrez un mot : ").lower()
# if mot in mots_interdits:
#     print("Ce mot est interdit !")
# else:
#     print("Mot autorisé.")


# -----------------------------------------------------------
# EXERCICE 6 — Doublons
# -----------------------------------------------------------
# À partir de la liste suivante, crée une nouvelle liste
# sans doublons (sans utiliser set()).

print("\n--- Exercice 6 ---")
avec_doublons = [1, 3, 2, 3, 5, 1, 4, 2, 6, 5]

# Ton code ici :


# ✅ CORRECTION
# sans_doublons = []
# for n in avec_doublons:
#     if n not in sans_doublons:
#         sans_doublons.append(n)
# print(sans_doublons)
