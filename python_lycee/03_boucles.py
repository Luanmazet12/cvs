"""
==============================================
  EXERCICES PYTHON — Niveau Lycée
  Fichier 3 : Boucles (for / while)
==============================================
"""

# ===========================================================
# COURS RAPIDE
# ===========================================================
#
# Boucle FOR — répéter un nombre de fois précis
#   for i in range(5):      -> i prend les valeurs 0,1,2,3,4
#   for i in range(1, 6):   -> i prend les valeurs 1,2,3,4,5
#   for i in range(0, 10, 2): -> i prend 0,2,4,6,8 (pas de 2)
#
# Boucle WHILE — répéter tant qu'une condition est vraie
#   while condition:
#       ...
#
# Mots-clés utiles :
#   break    -> quitte la boucle immédiatement
#   continue -> passe à l'itération suivante
# ===========================================================


# -----------------------------------------------------------
# EXERCICE 1 — Table de multiplication
# -----------------------------------------------------------
# Demande un nombre n et affiche sa table de multiplication
# de 1 à 10.
# Exemple pour n=3 : "3 x 1 = 3", "3 x 2 = 6", ...

print("--- Exercice 1 ---")

# Ton code ici :


# ✅ CORRECTION
# n = int(input("Table de quel nombre ? "))
# for i in range(1, 11):
#     print(f"{n} x {i} = {n * i}")


# -----------------------------------------------------------
# EXERCICE 2 — Somme des entiers
# -----------------------------------------------------------
# Calcule et affiche la somme des entiers de 1 à n.
# Exemple : pour n=5, la somme est 1+2+3+4+5 = 15

print("\n--- Exercice 2 ---")

# Ton code ici :


# ✅ CORRECTION
# n = int(input("Entrez n : "))
# somme = 0
# for i in range(1, n + 1):
#     somme += i          # équivalent à somme = somme + i
# print(f"Somme de 1 à {n} = {somme}")


# -----------------------------------------------------------
# EXERCICE 3 — Compte à rebours
# -----------------------------------------------------------
# Affiche un compte à rebours de 10 à 0, puis "Décollage !"

print("\n--- Exercice 3 ---")

# Ton code ici :


# ✅ CORRECTION
# for i in range(10, -1, -1):
#     print(i)
# print("Décollage !")


# -----------------------------------------------------------
# EXERCICE 4 — Validation de saisie (while)
# -----------------------------------------------------------
# Demande un mot de passe jusqu'à ce que l'utilisateur
# entre "python123". Affiche le nombre de tentatives.

print("\n--- Exercice 4 ---")

# Ton code ici :


# ✅ CORRECTION
# tentatives = 0
# mdp = ""
# while mdp != "python123":
#     mdp = input("Mot de passe : ")
#     tentatives += 1
# print(f"Accès accordé après {tentatives} tentative(s) !")


# -----------------------------------------------------------
# EXERCICE 5 — FizzBuzz (classique !)
# -----------------------------------------------------------
# Affiche les nombres de 1 à 30.
# Mais :
#   - Si le nombre est divisible par 3  → affiche "Fizz"
#   - Si le nombre est divisible par 5  → affiche "Buzz"
#   - Si divisible par 3 ET par 5       → affiche "FizzBuzz"

print("\n--- Exercice 5 (FizzBuzz) ---")

# Ton code ici :


# ✅ CORRECTION
# for i in range(1, 31):
#     if i % 15 == 0:       # divisible par 3 et 5 → vérifier en premier !
#         print("FizzBuzz")
#     elif i % 3 == 0:
#         print("Fizz")
#     elif i % 5 == 0:
#         print("Buzz")
#     else:
#         print(i)


# -----------------------------------------------------------
# EXERCICE 6 — Étoiles en triangle
# -----------------------------------------------------------
# Affiche un triangle d'étoiles de hauteur n.
# Exemple pour n=4 :
#   *
#   **
#   ***
#   ****

print("\n--- Exercice 6 ---")

# Ton code ici :


# ✅ CORRECTION
# n = int(input("Hauteur du triangle : "))
# for i in range(1, n + 1):
#     print("*" * i)
