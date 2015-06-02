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

#    global total_base 
#    global total_food
total_food = 1
total_base = 1

NEST = (0, 255, 0) 
BLOCK = (255,102,0)

WIDTH = 10
HEIGHT = 10
MARGIN = 0

gridArray = []
feremone_array = []
#total_base = 1.0
#total_food = 1.0

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

xSize = 0
ySize = 0

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
            
    for row in range(xSize):
        
        feremone_array.append([])
        for column in range(ySize):
            feremone_array[row].append(0)
            


def updateGrid(Grid,List):

    for row in range (xSize):
        for column in range (ySize):
                  
            gridArray[row][column] = updateGridAux(List[column + row * ySize])


def updateGridAux(input):

    if (input== b"nest"):
        return 5
    elif (input== b"block"):
        return 6
    elif (input == b"ant"):
        return 1
    elif (input == b"returning"):
        return 10
    elif (input == b"searching"):
        return 11
    elif (input == b"idling"):
        return 12
    elif (input == b"foodant"):
        return 4
    elif (input == b"food"):
        return 3
    elif (input == b'plain'):
        return 2
    #else:
    #    print("ODD THING CAME: " + str(bytes(input)) + " AS ARGUMENT")
        

def drawCell(cellType,row,column):
    pygame.draw.rect(display,
         cellType,
         [(MARGIN + WIDTH) * column + MARGIN,
          (MARGIN + HEIGHT) * row + MARGIN,
          WIDTH,
          HEIGHT])
    
def drawTinyCell(color,row,column):
    pygame.draw.rect(display,
         color,
         [WIDTH*column + 2,
          HEIGHT*row + 2,
          6,
          6])

def uppdateCell(row,column):

    values = gridArray[row][column]
    if values == 1:
        cellType = computeColor(feremone_array[row][column])
        drawCell(cellType, row, column)
        drawTinyCell(ANT, row, column)

    elif values == 2:
        #cellType = PLAIN
        cellType = computeColor(feremone_array[row][column])
        #print(str(cellType) +" from  "+ str(feremone_array[row][column])+ " totals are ("+str(total_base)+","+str(total_food)+") " )
        try:
            drawCell(cellType, row, column)
        except:
            print("fasdklfjalkdsjflkasdö " + str(cellType))
        
    elif values == 3:
        cellType = FOOD
        drawCell(cellType, row, column)
    elif values == 5:
        cellType = NEST
        drawCell(cellType, row, column)
    elif values == 6:
        cellType = BLOCK
        drawCell(cellType, row, column)
    elif values == 4:
        cellType = FOODANT
        drawCell(cellType, row, column)
    elif values == 10:
        cellType = computeColor(feremone_array[row][column])
        try:
            drawCell(cellType, row, column)
        except:
            print("fasdklfjalkdsjflkasdö " + str(cellType))
        drawTinyCell(RETURNING, row, column)

                         
    elif values == 11:
        cellType = computeColor(feremone_array[row][column])
        try:
            drawCell(cellType, row, column)
        except:
            print("fasdklfjalkdsjflkasdö " + str(cellType))
            
        drawTinyCell(SEARCHING, row, column)

        
    elif values == 12:
        cellType = IDLING
        drawCell(cellType, row, column)


    

def register_handler(dest):

    pygame.init()
    



    windowSize = [xSize*WIDTH, ySize*HEIGHT]
    global display
    display = pygame.display.set_mode(windowSize)

    pygame.display.set_caption("ANTS ARE LIFE")
    display.fill((127,133,145))

    clock = pygame.time.Clock()
    clock.tick(60)
    set_message_handler(handler)
    print("WE ARE HERE NOW :(")
    

def handler(message):
    
    #print(message)
    
    #print(List.to_string(list(message)[0]))
    #Lol = list(map(lambda x : List.to_string(x),list(message)))
    #print(Lol)
    #updateGrid(gridArray, Lol)
    messageFilter(list(message)) 

    
    #drawGridAux()
    pygame.display.flip()
            
def messageFilter(message):
    #print(message)
    global total_food
    global total_base

    #total_base_t = 0.001
    #total_food_t = 0.001  
    #print(message)
    #quit()
    row = message[0][1]
    column = message[0][0]
           
    cell_attributes = message[1]
    #print(str(cell_attributes) + " lololo " + str(cell_attributes[1]))
    
    if len(cell_attributes) == 2:
        gridArray[row][column] = updateGridAux(cell_attributes[0])
        #total_base = max(total_base, cell_attributes[1][0])
        #total_food = max(total_food, cell_attributes[1][1])
        feremone_array[row][column] = cell_attributes[1]
    else:
        gridArray[row][column] = updateGridAux(cell_attributes)

    uppdateCell(row, column)
         
def computeColor(feremones):
    base = sigmoidal(feremones[0],0.01)
    food = sigmoidal(feremones[1],0.01)
    #print(str(feremones[0])+"/"+str(total_base))
    thingy_base = base*(255+126)
    if thingy_base <= 255:
        base_color = (thingy_base,0,0)
    else:
        base_color = (255,thingy_base-255,0)
    
    thingy_food = food*(255+126)
    if thingy_food <= 255:
        food_color = (0,0,thingy_food)
    else:
        food_color = (0,thingy_food-255,255)
        
    return (int(base_color[0]),int(base_color[1]+food_color[1]),int(food_color[2]))

def sigmoidal(val,scale):
    return scale*val/(1+abs(scale*val))

