"""
==============================================
  EXERCICES PYTHON — Niveau Lycée
  Fichier 2 : Conditions (if / elif / else)
==============================================
"""

# ===========================================================
# COURS RAPIDE
# ===========================================================
# if condition :          <- si
#     ...
# elif autre_condition :  <- sinon si
#     ...
# else :                  <- sinon
#     ...
#
# Opérateurs de comparaison :
#   ==  égal        !=  différent
#   >   supérieur   <   inférieur
#   >=  sup ou égal <=  inf ou égal
#
# Opérateurs logiques :
#   and  (et)    or  (ou)    not  (non)
# ===========================================================


# -----------------------------------------------------------
# EXERCICE 1 — Bulletin scolaire
# -----------------------------------------------------------
# Demande une note (sur 20).
# Affiche :
#   - "Très bien"       si note >= 16
#   - "Bien"            si note >= 14
#   - "Assez bien"      si note >= 12
#   - "Passable"        si note >= 10
#   - "Insuffisant"     sinon

print("--- Exercice 1 ---")

# Ton code ici :


# ✅ CORRECTION
# note = float(input("Entrez votre note : "))
# if note >= 16:
#     print("Très bien")
# elif note >= 14:
#     print("Bien")
# elif note >= 12:
#     print("Assez bien")
# elif note >= 10:
#     print("Passable")
# else:
#     print("Insuffisant")


# -----------------------------------------------------------
# EXERCICE 2 — Parité
# -----------------------------------------------------------
# Demande un entier et dis si c'est pair ou impair.
# AIDE : un nombre est pair si n % 2 == 0  (% = modulo = reste de la division)

print("\n--- Exercice 2 ---")

# Ton code ici :


# ✅ CORRECTION
# n = int(input("Entrez un entier : "))
# if n % 2 == 0:
#     print(f"{n} est pair")
# else:
#     print(f"{n} est impair")


# -----------------------------------------------------------
# EXERCICE 3 — Comparaison de deux nombres
# -----------------------------------------------------------
# Demande deux nombres a et b.
# Affiche lequel est le plus grand, ou "Égaux" s'ils sont égaux.

print("\n--- Exercice 3 ---")

# Ton code ici :


# ✅ CORRECTION
# a = float(input("Nombre a : "))
# b = float(input("Nombre b : "))
# if a > b:
#     print(f"{a} est plus grand que {b}")
# elif b > a:
#     print(f"{b} est plus grand que {a}")
# else:
#     print("Les deux nombres sont égaux")


# -----------------------------------------------------------
# EXERCICE 4 — Année bissextile
# -----------------------------------------------------------
# Une année est bissextile si elle est divisible par 4
# MAIS pas par 100, SAUF si elle est divisible par 400.
# Demande une année et affiche si elle est bissextile.

print("\n--- Exercice 4 ---")

# Ton code ici :


# ✅ CORRECTION
# annee = int(input("Entrez une année : "))
# if (annee % 4 == 0 and annee % 100 != 0) or (annee % 400 == 0):
#     print(f"{annee} est bissextile")
# else:
#     print(f"{annee} n'est pas bissextile")


# -----------------------------------------------------------
# EXERCICE 5 — Calculatrice simple
# -----------------------------------------------------------
# Demande deux nombres et une opération (+, -, *, /).
# Affiche le résultat. Attention à la division par zéro !

print("\n--- Exercice 5 ---")

# Ton code ici :


# ✅ CORRECTION
# a  = float(input("Premier nombre  : "))
# op = input("Opération (+, -, *, /) : ")
# b  = float(input("Deuxième nombre : "))
#
# if op == "+":
#     print(f"Résultat : {a + b}")
# elif op == "-":
#     print(f"Résultat : {a - b}")
# elif op == "*":
#     print(f"Résultat : {a * b}")
# elif op == "/":
#     if b == 0:
#         print("Erreur : division par zéro !")
#     else:
#         print(f"Résultat : {a / b}")
# else:
#     print("Opération inconnue")
