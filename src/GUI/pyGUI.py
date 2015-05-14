import pygame
 
ANT = (0, 0, 0) #1
PLAIN = (50, 100, 0) #2 gräääs
FOOD = (100, 50, 0) #3 Maaat
FOODANT = (0, 0, 255) #hungrigMYRA

NEST = (0, 255, 0) 
FEROMONE = (255, 0, 0)
FOOD = (0, 0, 255) 

WIDTH = 10
HEIGHT = 10
MARGIN = 0

gridArray = []

testList = ["food", "ant", "foodant", "plain", "foodant", "food", "plain", "plain", "plain", "foodant", "foodant", "ant", "plain", "plain", "plain", "food", "ant", "plain", "plain", "foodant", "food", "ant", "ant", "plain", "food"]


def createGrid(x , y): 
 
   global xSize
   global ySize
   xSize = x
   ySize = y
   
   for row in range(xSize):
       
        gridArray.append([])
        for column in range(ySize):
            gridArray[row].append(0)

def updateGrid(Grid,List):

    for row in range (xSize):

        for column in range (ySize):
            
            gridArray[row][column] = updateGridAux(List[column + row * ySize])


def updateGridAux(input):

    if (input == "ant"):
        return 1

    elif (input == "plain"):
        return 2

    elif (input == "food"):
        return 3

    elif (input == "foodant"):
        return 4



def drawGrid(list):

    pygame.init()

    windowSize = [1000, 1000]
    display = pygame.display.set_mode(windowSize)

    pygame.display.set_caption("ANTS ARE LIFE")

    done = False

    clock = pygame.time.Clock()

    updateGrid(gridArray, list)
    
    

    while not done:

        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                done = True

        display.fill(PLAIN)
                
        for row in range(xSize):
            for column in range(ySize):
                cellType = PLAIN
                if gridArray[row][column] == 1:
                    cellType = ANT
                    pygame.draw.rect(display,
                             cellType,
                             [(MARGIN + WIDTH) * column + MARGIN,
                              (MARGIN + HEIGHT) * row + MARGIN,
                              WIDTH,
                              HEIGHT])

                elif gridArray[row][column] == 2:
                    cellType = PLAIN
                    pygame.draw.rect(display,
                             cellType,
                             [(MARGIN + WIDTH) * column + MARGIN,
                              (MARGIN + HEIGHT) * row + MARGIN,
                              WIDTH,
                              HEIGHT])
                
                elif gridArray[row][column] == 3:
                    cellType = FOOD
                    pygame.draw.rect(display,
                             cellType,
                             [(MARGIN + WIDTH) * column + MARGIN,
                              (MARGIN + HEIGHT) * row + MARGIN,
                              WIDTH,
                              HEIGHT])
                elif gridArray[row][column] == 4:
                    cellType = FOODANT
                    pygame.draw.rect(display,
                             cellType,
                             [(MARGIN + WIDTH) * column + MARGIN,
                              (MARGIN + HEIGHT) * row + MARGIN,
                              WIDTH,
                              HEIGHT])
                
                                
                

    
        clock.tick(60)

        pygame.display.flip()

createGrid(5, 5)
drawGrid(testList)


pygame.quit()







