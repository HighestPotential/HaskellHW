import numpy as np

P = np.array([
    [0.5, 0.5, 0.0],
    [0.25, 0.5, 0.25],
    [0.0, 0.5, 0.5]
])

# Starte mit Einheitsmatrix
Pn = np.eye(3)

# Multipliziere mehrfach mit P
for n in range(1, 15):
    Pn = np.dot(Pn, P)
    print(f"P^{n} =\n", np.round(Pn, 4))
