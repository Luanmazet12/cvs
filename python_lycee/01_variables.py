"""
==============================================
  EXERCICES PYTHON — Niveau Lycée
  Fichier 1 : Variables, types et affichage
==============================================
"""

# ===========================================================
# COURS RAPIDE
# ===========================================================
# Une variable = une boîte qui stocke une valeur.
# Python détecte automatiquement le type.
#
#   nom    = "Alice"   -> str   (chaîne de caractères)
#   age    = 17        -> int   (entier)
#   note   = 14.5      -> float (décimal)
#   reussi = True      -> bool  (vrai ou faux)
#
# Pour afficher : print(...)
# Pour saisir   : input(...)
# ===========================================================


# -----------------------------------------------------------
# EXERCICE 1 — Présentation
# -----------------------------------------------------------
# Demande à l'utilisateur son prénom et son âge,
# puis affiche : "Bonjour [prénom], tu as [âge] ans !"
#
# AIDE : utilise input() pour récupérer une valeur.
#        N'oublie pas de convertir l'âge en entier avec int().

print("--- Exercice 1 ---")

# Ton code ici :
# prenom = ...
# age    = ...
# print(...)


# ✅ CORRECTION (décommente pour voir)
# prenom = input("Quel est ton prénom ? ")
# age    = int(input("Quel est ton âge ? "))
# print(f"Bonjour {prenom}, tu as {age} ans !")


# -----------------------------------------------------------
# EXERCICE 2 — Calcul de périmètre
# -----------------------------------------------------------
# Demande le côté d'un carré (en cm) et affiche son périmètre.
# Rappel : périmètre = 4 × côté

print("\n--- Exercice 2 ---")

# Ton code ici :


# ✅ CORRECTION
# cote = float(input("Côté du carré (cm) : "))
# perimetre = 4 * cote
# print(f"Le périmètre est {perimetre} cm")


# -----------------------------------------------------------
# EXERCICE 3 — Conversion °C → °F
# -----------------------------------------------------------
# Demande une température en Celsius et affiche-la en Fahrenheit.
# Formule : F = C × 9/5 + 32

print("\n--- Exercice 3 ---")

# Ton code ici :


# ✅ CORRECTION
# celsius    = float(input("Température en °C : "))
# fahrenheit = celsius * 9 / 5 + 32
# print(f"{celsius}°C = {fahrenheit}°F")


# -----------------------------------------------------------
# EXERCICE 4 — Échange de variables
# -----------------------------------------------------------
# Crée deux variables a = 5 et b = 10.
# Échange leurs valeurs SANS utiliser une troisième variable.
# Astuce Python : a, b = b, a

print("\n--- Exercice 4 ---")

# Ton code ici :


# ✅ CORRECTION
# a, b = 5, 10
# print(f"Avant : a={a}, b={b}")
# a, b = b, a
# print(f"Après : a={a}, b={b}")
