import pygame, sys, random
from pygame.locals import *
from erlport.erlterms import Atom,List
from erlport.erlang import set_message_handler, cast
 
ANT = (0, 0, 0) #1
SEARCHING = (255, 0, 0) #11
RETURNING = (0, 0, 255) #10
IDLING = (255, 255, 255) #12
PLAIN = (50, 100, 0) #2 gräääs
PLAIN1 = pygame.image.load('grass.jpg')
FOOD = (0, 100, 255) #3 Maaat
FOODANT = (0, 100, 200) #hungrigMYRA



NEST = (0, 255, 0) 
BLOCK = (255,102,0)

WIDTH = 10
HEIGHT = 10
MARGIN = 0

gridArray = []

testList = ["plain", "ant", "plain", "foodant", "foodant", "foodant", "plain", "plain", "foodant", "foodant", "foodant", "ant", "foodant", "plain", "plain", "plain", "ant", "plain", "plain", "foodant", "food", "plain", "foodant", "plain", "food", "plain", "ant", "plain", "plain", "foodant", "plain", "plain", "plain", "foodant", "foodant", "foodant", "ant", "plain", "foodant", "plain", "plain", "ant", "plain", "plain", "plain", "food", "plain", "plain", "plain", "food", "foodant", "ant", "plain", "foodant", "plain", "plain", "plain", "plain", "plain", "foodant", "foodant", "ant", "plain", "plain", "foodant", "plain", "ant", "foodant", "plain", "foodant", "food", "plain", "foodant", "plain", "food", "foodant", "ant", "foodant", "plain", "foodant", "foodant", "foodant", "plain", "foodant", "foodant", "foodant", "ant", "plain", "plain", "foodant", "foodant", "ant", "foodant", "plain", "foodant", "food", "plain", "foodant", "plain", "food"]


def randomList(amount):
    
    variables = ['food', 'plain', 'foodant', 'plain', 'plain', 'plain', 'plain', 'plain']
    #global randList
    randList = []

    for x in range (amount):
        randList.append([])

    y = len(randList)
    print(y)

    for y in range (amount):
        randList[y] = random.choice(variables)

   
    return randList

#xSize = 99
#ySize = 99

def createGrid(x , y): 
 
   global xSize
   global ySize
   xSize = x
   ySize = y
 
   print("created grid")
   print(xSize)
   print(ySize)
  
   for row in range(xSize):
       
        gridArray.append([])
        for column in range(ySize):
            gridArray[row].append(0)

def updateGrid(Grid,List):

   for row in range (xSize):

      for column in range (ySize):
            
         gridArray[row][column] = updateGridAux(List[column + row * ySize])


def updateGridAux(input):

    if (input== "nest"):
        return 5
    elif (input== "block"):
        return 6
    elif (input == "ant"):
        return 1
    elif (input == "returning"):
        return 10
    elif (input == "searching"):
        return 11
    elif (input == "idling"):
        return 12
    elif (input == "foodant"):
        return 4
    elif (input == "food"):
        return 3
    elif (input == "plain"):
        return 2
        

def drawGridAux():
    
                    
        for row in range(xSize):
            for column in range(ySize):
                #cellType = PLAIN
                values = gridArray[row][column]
                if values == 1:
                    cellType = ANT
                    pygame.draw.rect(display,
                             cellType,
                             [(MARGIN + WIDTH) * column + MARGIN,
                              (MARGIN + HEIGHT) * row + MARGIN,
                              WIDTH,
                              HEIGHT])

                elif values == 2:
                    cellType = PLAIN
                    display.blit(PLAIN1, (column * WIDTH, row * HEIGHT))
                    
                    
#                    pygame.draw.rect(display,
 #                            cellType,
  #                           [(MARGIN + WIDTH) * column + MARGIN,
   #                           (MARGIN + HEIGHT) * row + MARGIN,
    #                          WIDTH,
     #                         HEIGHT])
                
                elif values == 3:
                    cellType = FOOD
                    pygame.draw.rect(display,
                             cellType,
                             [(MARGIN + WIDTH) * column + MARGIN,
                              (MARGIN + HEIGHT) * row + MARGIN,
                              WIDTH,
                              HEIGHT])
                elif values == 5:
                    cellType = NEST
                    pygame.draw.rect(display,
                             cellType,
                             [(MARGIN + WIDTH) * column + MARGIN,
                              (MARGIN + HEIGHT) * row + MARGIN,
                              WIDTH,
                              HEIGHT])
                elif values == 6:
                        cellType = BLOCK
                        pygame.draw.rect(display,
                                         cellType,
                                         [(MARGIN + WIDTH) * column + MARGIN,
                                          (MARGIN + HEIGHT) * row + MARGIN,
                                          WIDTH,
                                          HEIGHT])
                elif values == 4:
                    cellType = FOODANT
                    pygame.draw.rect(display,
                                     cellType,
                                     [(MARGIN + WIDTH) * column + MARGIN,
                                      (MARGIN + HEIGHT) * row + MARGIN,
                                      WIDTH,
                                      HEIGHT])
                elif values == 10:
                    cellType = RETURNING
                    pygame.draw.rect(display,
                                     cellType,
                                     [(MARGIN + WIDTH) * column + MARGIN,
                                      (MARGIN + HEIGHT) * row + MARGIN,
                                      WIDTH,
                                      HEIGHT])
                                     
                elif values == 11:
                    cellType = SEARCHING
                    pygame.draw.rect(display,
                                     cellType,
                                     [(MARGIN + WIDTH) * column + MARGIN,
                                      (MARGIN + HEIGHT) * row + MARGIN,
                                      WIDTH,
                                      HEIGHT])
                elif values == 12:
                    cellType = IDLING
                    pygame.draw.rect(display,
                                     cellType,
                                     [(MARGIN + WIDTH) * column + MARGIN,
                                      (MARGIN + HEIGHT) * row + MARGIN,
                                      WIDTH,
                                      HEIGHT])
       

    

def register_handler(dest):

    pygame.init()

    windowSize = [1000, 1000]
    global display
    display = pygame.display.set_mode(windowSize)

    pygame.display.set_caption("ANTS ARE LIFE")
    display.fill(PLAIN)

    clock = pygame.time.Clock()
    clock.tick(60)
    set_message_handler(handler)
    print("WE ARE HERE NOW :(")
    

def handler(message):
    
    print(List.to_string(list(message)[0]))
    Lol = list(map(lambda x : List.to_string(x),list(message)))
   # print(Lol)
    updateGrid(gridArray, Lol) 

   
    drawGridAux()
    pygame.display.flip()
            






