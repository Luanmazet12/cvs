import tkinter as tk


def dire_bonjour():
    nom = entry_nom.get().strip()
    if nom:
        label_message.config(text=f"Bonjour, {nom} ! 👋", fg="#2e7d32")
    else:
        label_message.config(text="Bonjour, monde ! 👋", fg="#2e7d32")


# Fenêtre principale
fenetre = tk.Tk()
fenetre.title("Interface Bonjour")
fenetre.geometry("400x220")
fenetre.resizable(False, False)
fenetre.configure(bg="#f5f5f5")

# Titre
label_titre = tk.Label(
    fenetre,
    text="👋 Dis Bonjour !",
    font=("Helvetica", 18, "bold"),
    bg="#f5f5f5",
    fg="#1a237e",
)
label_titre.pack(pady=(20, 10))

# Champ de saisie du prénom
frame_saisie = tk.Frame(fenetre, bg="#f5f5f5")
frame_saisie.pack()

label_nom = tk.Label(frame_saisie, text="Ton prénom :", font=("Helvetica", 12), bg="#f5f5f5")
label_nom.pack(side=tk.LEFT, padx=(0, 8))

entry_nom = tk.Entry(frame_saisie, font=("Helvetica", 12), width=20)
entry_nom.pack(side=tk.LEFT)
entry_nom.bind("<Return>", lambda event: dire_bonjour())

# Bouton
bouton = tk.Button(
    fenetre,
    text="Dire Bonjour",
    font=("Helvetica", 12, "bold"),
    bg="#1a237e",
    fg="white",
    activebackground="#3949ab",
    activeforeground="white",
    relief="flat",
    padx=12,
    pady=6,
    cursor="hand2",
    command=dire_bonjour,
)
bouton.pack(pady=14)

# Message de salutation
label_message = tk.Label(
    fenetre,
    text="",
    font=("Helvetica", 14, "italic"),
    bg="#f5f5f5",
    fg="#2e7d32",
)
label_message.pack()

fenetre.mainloop()
