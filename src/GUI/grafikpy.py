import pygame
 
pygame.init()
 
screen = pygame.display.set_mode((1000, 1000))
pygame.display.set_caption('Myror')

background = pygame.Surface(screen.get_size())
background = background.convert()
background.fill((51, 255, 153))
 
screen.blit(background, (0, 0))
pygame.display.flip()
 
xAnt = 400
yAnt = 400


 
def drawAnt(xAnt, yAnt):
 
    done = False
 
    while not done:
        
    
        pygame.draw.rect(screen, (0, 128, 255), pygame.Rect(xAnt, yAnt, 5, 5))
 
        pygame.display.flip()
 
        #pygame.event.wait()

    for event in pygame.event.get():
       
        
        if event.type == pygame.QUIT:
 
            done = True
 
 
 
 
drawAnt(xAnt, yAnt)


