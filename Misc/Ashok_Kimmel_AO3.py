#import pygame
#import collections
#import pygame.event 
#
#def init():
#    pygame.init()
#    screen = pygame.display.set_mode((320, 280))
#    pygame.display.set_caption("AO3 pt 1")
#    return screen, pygame.time.Clock()
#
#def addVector(v1,v2):
#    (x1,y1) = v1
#    (x2,y2) = v2
#    return (x1+x2,y1+y2)
#QUIT_COORDS = (-100,-100)
#keyHandler = collections.defaultdict(lambda: lambda a: a)
#keyHandler[pygame.K_UP] = lambda a: addVector(a,((0,-10)))
#keyHandler[pygame.K_DOWN] = lambda a: addVector(a,((0,10)))
#keyHandler[pygame.K_LEFT] = lambda a: addVector(a,((-10,0)))
#keyHandler[pygame.K_RIGHT] = lambda a: addVector(a,((10,0)))
#keyHandler[pygame.K_q] = lambda _ : QUIT_COORDS
#
#eventHandler = collections.defaultdict(lambda: lambda _, b: b)
#eventHandler["KeyDown"] = lambda a,b: (b[0]+[a.key],b[1])  
#eventHandler["KeyUp"] = lambda a,b: ([x for x in b[0] if x != a.key],b[1])  
#eventHandler["MouseButtonDown"] = lambda a,_: ([],a.pos)
#def draw_handler(screen, model):
#    bgcolor = pygame.color.Color("cadetblue1")
#    screen.fill(bgcolor)
#
#    r = pygame.Rect(0, 0, 30, 30)
#    r.center = model
#
#    player_color = pygame.color.Color("red")
#    pygame.draw.rect(screen, player_color, r)
#def tick_handler(model):
#    (a,b) = model 
#    for i in a:
#        
#        b = keyHandler[i](b)
#    return (a,b)
#def main():
#    screen, clock = init()
#    running = True
#    model = ([],(100, 50))
#
#    while running:
#        model = tick_handler(model)
#        
#        for event in pygame.event.get():
#            if event.type == pygame.QUIT or model[1] == QUIT_COORDS:
#                running = False
#            else:
#                model = eventHandler[pygame.event.event_name(event.type)](event,model)      
#                print(model)
#                
#        draw_handler(screen, model[1])
#        pygame.display.flip()
#        clock.tick(15)
#    pygame.quit()
#print(pygame.event.event_name(pygame.KEYDOWN))
#if __name__ == '__main__':
#    main()
#

#q3

#import pygame
#import collections
#import pygame.event 
#
#def init():
#    pygame.init()
#    screen = pygame.display.set_mode((320, 280))
#    pygame.display.set_caption("AO3 pt 1")
#    return screen, pygame.time.Clock()
#
#def addVector(v1,v2):
#    (x1,y1) = v1[-1]
#    (x2,y2) = v2
#    return v1[1:] + [(x1+x2,y1+y2)]
#QUIT_COORDS = list(map(lambda _: (-100,-100),range(49))) + [(-100,-100)]
#keyHandler = collections.defaultdict(lambda: lambda a: a)
#keyHandler[pygame.K_UP] = lambda a: addVector(a,((0,-10)))
#keyHandler[pygame.K_DOWN] = lambda a: addVector(a,((0,10)))
#keyHandler[pygame.K_LEFT] = lambda a: addVector(a,((-10,0)))
#keyHandler[pygame.K_RIGHT] = lambda a: addVector(a,((10,0)))
#keyHandler[pygame.K_q] = lambda _ : QUIT_COORDS
###############################################################################################################
#eventHandler = collections.defaultdict(lambda: lambda _, b: b)
#eventHandler["KeyDown"] = lambda a,b: (b[0]+[a.key],b[1])  
#eventHandler["KeyUp"] = lambda a,b: ([x for x in b[0] if x != a.key],b[1])  
#eventHandler["MouseButtonDown"] = lambda a,b: ([],b[1][1:]+[a.pos])
#def draw_handler(screen, models):
#    bgcolor = pygame.color.Color("cadetblue1")
#    screen.fill(bgcolor)
#
#    for model in models:
#        r = pygame.Rect(0, 0, 30, 30)
#        r.center = model
#    
#        player_color = pygame.color.Color("red")
#        pygame.draw.rect(screen, player_color, r)
#def tick_handler(model):
#    (a,b) = model 
#    for i in a:
#        
#        b = keyHandler[i](b)
#    return (a,b)
#def main():
#    screen, clock = init()
#    running = True
#    model = ([],list(map(lambda _: (-100,-100),range(49))) + [(100, 50)])
#
#    while running:
#        model = tick_handler(model)
#        
#        for event in pygame.event.get():
#            if event.type == pygame.QUIT or model[1][-1] == QUIT_COORDS:
#                running = False
#            else:
#                model = eventHandler[pygame.event.event_name(event.type)](event,model)      
#
#        draw_handler(screen, model[1])
#        pygame.display.flip()
#        clock.tick(15)
#    pygame.quit()
#if __name__ == '__main__':
#    main()


#
#import pygame
#import collections
#import pygame.event 
#import random
#import pygame.font
#
#def init():
#    pygame.init()
#    screen = pygame.display.set_mode((12 * widthsize, 12 * heightsize))
#    pygame.display.set_caption("AO3 pt 1")
#    return screen, pygame.time.Clock()
#bounds = (12,12)
#def addVector(v1,v2):
#    (x1,y1) = v1[-1]
#    (x2,y2) = v2
#    return v1[1:] + [(x1+x2,y1+y2)]
#QUIT_COORDS = [(-100,-100)]
#widthsize = 25
#heightsize = 25
#
#def to_pos(x):
#    (a,b)=x.pos
#    return (a // widthsize,b // heightsize)
#eventHandler = collections.defaultdict(lambda: lambda _, b: b)
#eventHandler["MouseButtonDown"] = lambda a,b: [x for x in b if x != to_pos(a)]
#bgcolor = pygame.color.Color("blue")
#def draw_end_handler(screen,model):
#    screen.fill(bgcolor)
#    print("Draw_end_handler ran")
#    x = pygame.font.Font.render(pygame.font.Font(None,64),"You won!",True,pygame.color.Color("red"))
#    screen.blit(x,(50,50)) 
#
#def draw_handler(screen, model):
#    screen.fill(bgcolor)
#    for i,j in model:
#        r = pygame.Rect(0, 0, widthsize, heightsize)        
#        r.topleft = (i *widthsize,j*heightsize) 
#        player_color = pygame.color.Color("orange")
#        pygame.draw.rect(screen, player_color, r)
#def main():
#    screen, clock = init()
#    running = True
#    model = random.sample([(a,b) for a in range(bounds[0]) for b in range(bounds[1])],13)
#    def gameOver():
#        return model == []
#    while running:
#        for event in pygame.event.get():
#            if event.type == pygame.QUIT or model == QUIT_COORDS or gameOver():
#                running = False
#            
#            else:
#              #  print(model)
#                model = eventHandler[pygame.event.event_name(event.type)](event,model)
#            
#        draw_handler(screen, model)
#        pygame.display.flip()
#        clock.tick(15)
#    if(gameOver()):
#        print("ENDLOOPHAPPENEd")
#        while True:
#            
#            draw_end_handler(screen, model)
#            pygame.display.flip()
#    pygame.quit()
#
#if __name__ == '__main__':
#    main()

#import math
#import sys, pygame
#import random
#import collections
#
#size = width, height = 400, 400
#boxwidth,boxheight = 50,50
#def workCoord(a,b): 
#    c = a / b 
#    d = round(c)
#    return (abs(c - d),d)
#def getCoords(a):
#    (b,c) = a
#    (d,e) = workCoord(b,boxwidth)
#    (f,g) = workCoord(c,boxheight)
#    xcloser = d <= f
#    if d < .3 or f < .3:
#        if xcloser:
#            return {((e,g),3),((e-1,g),1)}
#        else:
#            return {((e,g),2),((e,g-1),0)}    
#    else: 
#        return {}
#def init():
#	global clock, screen
#	pygame.init()
#	clock = pygame.time.Clock()
#	screen = pygame.display.set_mode((320,280))
#	pygame.display.set_caption("demo progtam1") 
#QUIT_COORDS= ({(-100,-100)},collections.defaultdict(set))
#def createRect(coords,mybool):
#    (a,b) = coords
#    if mybool:
#        r = pygame.Rect(a*boxwidth,b*boxheight,boxwidth,1)
#        return r
#    else:
#        r = pygame.Rect(a*boxwidth,b*boxheight,1,boxheight)
#        return r
#def addWall(model):
#    (a,b)=model
#    for i,j in a:
#        b[i].add(j)
#    return (a,b)
#makeRect = {}
#makeRect[0] = lambda a: createRect((a[0],a[1]+1),True)
#makeRect[1] = lambda a: createRect((a[0]+1,a[1]),False)
#makeRect[2] = lambda a: createRect(a,True)
#makeRect[3] = lambda a: createRect(a,False)
#
#def draw_handler(screen,model):
#    bgcolor = pygame.color.Color("cadetblue1")
#    screen.fill(bgcolor)
#    (a,b)=model
#    for (i,j) in a:
#        r = makeRect[j](i)
#        player_color = pygame.color.Color("red")
#        pygame.draw.rect(screen, player_color, r)
#    for i,sets in b.items():
#        for j in sets:r = makeRect[j](i)
#        player_color = pygame.color.Color("red")
#        pygame.draw.rect(screen, player_color, r)
#            r = makeRect[j](i)
#            player_color = pygame.color.Color("black")
#            pygame.draw.rect(screen, player_color, r)
#
#eventHandler = collections.defaultdict(lambda: lambda _, b: b)
#eventHandler["MouseMotion"] = lambda a,b: (getCoords(a.pos),b[1])
#eventHandler["MouseButtonDown"] = lambda _,b: addWall(b)
#
#def main():
#    init()
#    running = True
#    model = ({},collections.defaultdict(set))
#    while running:
#        for event in pygame.event.get():
#            if event.type == pygame.QUIT or model == QUIT_COORDS:
#                running = False
#            
#            else:
#                model = eventHandler[pygame.event.event_name(event.type)](event,model)
#        draw_handler(screen, model)
#        pygame.display.flip()
#        clock.tick(15)
#
#if __name__ == '__main__':
#  main()
#