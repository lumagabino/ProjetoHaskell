# Luma Gabino Vasconcelos RA: 202495
# Pietro Ruy Pugliesi RA: 185921

import math
# Distancia euclidiana 
def distance(coord1, coord2):
    soma = 0
    for i in range(len(coord1)):
	    soma += math.pow(coord1[i] - coord2[i], 2)
    return soma

if __name__ == "__main__":
    # Leitura nomes e coordenadas
    coords  = []
    coord  = [str(z) if any(i.isalpha() for i in z) else float(z) for z in input().split()]
    while coord != []:
        coords.append(coord)
        coord  = [str(z) if any(i.isalpha() for i in z) else float(z) for z in input().split()]

    # Leitura nomes e labels
    labels = []
    while True:
        try:
            label  = [str(z) if any(i.isalpha() for i in z) else float(z) for z in input().split()]
            labels.append(label)
        except EOFError:
            break

    #Separa pontos com de sem label 
    withLabels = []
    withoutLabel = coords.copy()
    for coord in coords:
        for nome, label in labels:
            if coord[0] == nome:
                withoutLabel.remove(coord)
                withLabels.append((label, nome, coord[1::]))

    while withoutLabel != []:
        # Calcula a distancia minima entre todos os pontos com e sem label
        distances = []
        menor_dist = float("inf")
        for x in withLabels:
            for y in withoutLabel:
                dist = distance(x[2],y[1::])
                if dist < menor_dist:
                    menor_dist = dist
                    label = x[0]
                    y_menor = y

        # Atribui label, adiciona em withLabel e remove de withouLabel
        withLabels.append((label, y_menor[0], y_menor[1::]))
        for i in withoutLabel:
            if i[0] == y_menor[0]:
                withoutLabel.remove(i)
    
    print(withLabels)







