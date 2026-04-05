"""
==============================================
  EXERCICES PYTHON — Niveau Lycée
  Fichier 6 : Mini-projets complets
==============================================
Chaque projet utilise tout ce qu'on a vu :
  variables, conditions, boucles, listes, fonctions.

Lance ce fichier et choisis un projet !
"""

import random   # module pour les nombres aléatoires


# ===========================================================
# PROJET 1 — JEU DU NOMBRE MYSTÈRE
# ===========================================================
# L'ordinateur choisit un nombre entre 1 et 100.
# Le joueur doit le deviner. À chaque tentative,
# l'ordinateur dit "Trop grand" ou "Trop petit".
# Affiche le nombre de tentatives à la fin.

def jeu_nombre_mystere():
    print("\n========== JEU DU NOMBRE MYSTÈRE ==========")
    mystere    = random.randint(1, 100)
    tentatives = 0
    trouve     = False

    print("Je pense à un nombre entre 1 et 100...")

    while not trouve:
        essai = int(input("Votre proposition : "))
        tentatives += 1

        if essai < mystere:
            print("Trop petit !")
        elif essai > mystere:
            print("Trop grand !")
        else:
            trouve = True
            print(f"Bravo ! Vous avez trouvé {mystere} en {tentatives} tentative(s) !")


# ===========================================================
# PROJET 2 — CALCULATRICE
# ===========================================================
# Une calculatrice en boucle.
# L'utilisateur entre deux nombres et une opération.
# Il peut continuer ou quitter en tapant "q".

def calculatrice():
    print("\n========== CALCULATRICE ==========")
    print("Tapez 'q' à la place d'un nombre pour quitter.\n")

    while True:
        saisie_a = input("Premier nombre  : ")
        if saisie_a.lower() == "q":
            break

        op = input("Opération (+, -, *, /) : ")

        saisie_b = input("Deuxième nombre : ")
        if saisie_b.lower() == "q":
            break

        a = float(saisie_a)
        b = float(saisie_b)

        if op == "+":
            print(f"= {a + b}\n")
        elif op == "-":
            print(f"= {a - b}\n")
        elif op == "*":
            print(f"= {a * b}\n")
        elif op == "/":
            if b == 0:
                print("Erreur : division par zéro !\n")
            else:
                print(f"= {a / b:.4f}\n")
        else:
            print("Opération non reconnue.\n")

    print("Au revoir !")


# ===========================================================
# PROJET 3 — GESTION DE NOTES DE CLASSE
# ===========================================================
# Programme qui :
#   - permet d'entrer les notes d'une classe
#   - affiche : moyenne, meilleure note, pire note
#   - affiche la liste triée
#   - dit combien d'élèves ont la moyenne (>= 10)

def gestion_notes():
    print("\n========== GESTION DE NOTES ==========")
    notes = []

    nb = int(input("Combien d'élèves ? "))
    for i in range(nb):
        note = float(input(f"Note de l'élève {i+1} : "))
        notes.append(note)

    if len(notes) == 0:
        print("Aucune note saisie.")
        return

    moyenne   = sum(notes) / len(notes)
    ont_moy   = sum(1 for n in notes if n >= 10)
    notes_tri = sorted(notes)

    print(f"\nNotes triées  : {notes_tri}")
    print(f"Moyenne       : {moyenne:.2f}/20")
    print(f"Meilleure note: {max(notes)}/20")
    print(f"Pire note     : {min(notes)}/20")
    print(f"Ont la moyenne: {ont_moy}/{nb} élève(s)")


# ===========================================================
# PROJET 4 — PENDU (simplifié)
# ===========================================================
# Le programme choisit un mot au hasard.
# Le joueur propose des lettres. Il a 6 essais.
# Les lettres déjà essayées sont affichées.

def pendu():
    print("\n========== PENDU ==========")
    mots = ["python", "lycee", "ordinateur", "programme",
            "variable", "fonction", "boucle", "algorithme"]

    mot        = random.choice(mots)
    lettres_ok = []    # lettres trouvées
    erreurs    = []    # mauvaises lettres
    max_erreurs = 6

    while len(erreurs) < max_erreurs:
        # Afficher le mot masqué
        affichage = ""
        for lettre in mot:
            if lettre in lettres_ok:
                affichage += lettre + " "
            else:
                affichage += "_ "

        print(f"\nMot    : {affichage}")
        print(f"Erreurs ({len(erreurs)}/{max_erreurs}) : {' '.join(erreurs)}")

        # Vérifier si le mot est trouvé
        if "_" not in affichage:
            print(f"Félicitations ! Le mot était '{mot}' !")
            return

        proposition = input("Proposez une lettre : ").lower()
        if len(proposition) != 1 or not proposition.isalpha():
            print("Entrez une seule lettre.")
            continue
        if proposition in lettres_ok or proposition in erreurs:
            print("Lettre déjà proposée !")
            continue

        if proposition in mot:
            lettres_ok.append(proposition)
            print("Bonne lettre !")
        else:
            erreurs.append(proposition)
            print("Mauvaise lettre !")

    print(f"\nPerdu ! Le mot était '{mot}'.")


# ===========================================================
# MENU PRINCIPAL
# ===========================================================
def menu():
    print("╔══════════════════════════════════╗")
    print("║     MINI-PROJETS PYTHON LYCÉE    ║")
    print("╠══════════════════════════════════╣")
    print("║  1. Jeu du nombre mystère        ║")
    print("║  2. Calculatrice                 ║")
    print("║  3. Gestion de notes             ║")
    print("║  4. Pendu                        ║")
    print("║  0. Quitter                      ║")
    print("╚══════════════════════════════════╝")

    choix = input("Votre choix : ")

    if choix == "1":
        jeu_nombre_mystere()
    elif choix == "2":
        calculatrice()
    elif choix == "3":
        gestion_notes()
    elif choix == "4":
        pendu()
    elif choix == "0":
        print("À bientôt !")
    else:
        print("Choix invalide.")


# Point d'entrée du programme
menu()
