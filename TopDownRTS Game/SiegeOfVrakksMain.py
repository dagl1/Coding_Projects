import pygame
from pygame.locals import *
import VrakksGamemanager
#print('d')
import VrakksMouse
import Units
import VrakksProjectiles
#Import functions
clock = pygame.time.Clock()


#Pygame initialization
pygame.init()
Width, Heigth = VrakksGamemanager.GameManager.displayWidth, VrakksGamemanager.GameManager.displayHeight
Window = pygame.display.set_mode([Width,Heigth])
pygame.font.init()
# SpriteSuperSet = LoadFilesList.LoadImages()



#Map
	#check if display is initiatlized
	#pygame.display.get_init

#Background
Background_Unscaled = pygame.image.load("E:/Python/WeedGrow/backgrounddetailed3.png")
GUISize = Width/3
BG = pygame.transform.scale(Background_Unscaled,(round(Width-(Width/3)), Heigth))

class exampleUnit():
    def __init__(self):
        self.picture = "picture location"
        self.commandOptions = "List of options, their cooldown/availability, and reference to the picture for each"
        self.stats = "list of wounded, stats and other thigns"
        self.x = 300
        self.y = 300
        self.pictureShape = pygame.Rect(int(2*(Width/3)+125),int(Heigth-9*(Heigth/10)),350,350)


class GUI():
    lastSelectedUnit = None

    def drawUnitMainPicture(currentlySelectedUnitOptions):

        pygame.draw.rect(Window, (255, 0, 0), currentlySelectedUnitOptions.pictureShape, 1)
    def drawUnitCommandOptions(currentlySelectedUnitOptions):
        pass
    def drawUnitCommandEffects(currentlySelectedUnitOptions):
        pass

    def drawCircleAroundSelectedUnit(currentlySelectedUnitOptions):
        pass
    # def checkIfStillOnSameUnit(currentlySelectedUnitOptions):
    #     if GUI.lastSelectedUnit == currentlySelectedUnitOptions:
    #         GUI.lastSelectedUnit =currentlySelectedUnitOptions
    #         return True
    #     else:
    #         GUI.lastSelectedUnit = currentlySelectedUnitOptions
    #         return False



    def load_GUI(overlaySettings,currentlySelectedUnitOptions):

        GUI.drawUnitMainPicture(currentlySelectedUnitOptions)
        GUI.drawUnitCommandOptions(currentlySelectedUnitOptions)
        GUI.drawUnitCommandEffects(currentlySelectedUnitOptions)

    def draw_GUI(listOfGUIElements):
        pass

class unitGraphics():

    def drawSelectedIndicator(self):
        yDisplayModifier = VrakksGamemanager.GameManager.yDisplayModifier
        xDisplayModifier = VrakksGamemanager.GameManager.xDisplayModifier
        size = 20
        x = self.rect.x - xDisplayModifier
        y = self.rect.y - yDisplayModifier
        pygame.draw.line(Window, (255,255,0),   (x + 10, y + 10),(x + 10 + size, y + 10 + size))
        pygame.draw.line(Window, (255, 255, 0), (x - 10, y + 10),(x - 10 - size, y + 10 + size))
        pygame.draw.line(Window, (255, 255, 0), (x - 10, y - 10),(x - 10 - size, y - 10 - size))
        pygame.draw.line(Window, (255, 255, 0), (x + 10, y - 10),(x + 10 + size, y - 10 - size))

    def drawUnits():
        for unit in Units.baseUnit.UnitList:
            yDisplayModifier = VrakksGamemanager.GameManager.yDisplayModifier
            xDisplayModifier = VrakksGamemanager.GameManager.xDisplayModifier
            x = unit.rect.x - xDisplayModifier
            y = unit.rect.y - yDisplayModifier
            toDrawRect = pygame.Rect((x,y),(unit.size,unit.size))
            pygame.draw.rect(Window, (unit.color1, unit.color2, unit.color3),toDrawRect,0)
            if unit.selected == True:
                unitGraphics.drawSelectedIndicator(unit)

    def drawProjectiles():
        for i in range(len(VrakksGamemanager.GameManager.ProjectilesList)):
            yDisplayModifier = VrakksGamemanager.GameManager.yDisplayModifier
            xDisplayModifier = VrakksGamemanager.GameManager.xDisplayModifier
            x = VrakksGamemanager.GameManager.ProjectilesList[i].x-xDisplayModifier
            y = VrakksGamemanager.GameManager.ProjectilesList[i].y-yDisplayModifier
            pygame.draw.line(Window, (255, 255, 0), (int(x) , int(y)),
                             (int(x + ( VrakksGamemanager.GameManager.ProjectilesList[i].length*VrakksGamemanager.GameManager.ProjectilesList[i].vector[0])),
                              int(y +( VrakksGamemanager.GameManager.ProjectilesList[i].length*VrakksGamemanager.GameManager.ProjectilesList[i].vector[1]))))

    def selectionRectangle():
        if VrakksGamemanager.GameManager.mouse1IsPressed:
            yDisplayModifier = VrakksGamemanager.GameManager.yDisplayModifier
            xDisplayModifier = VrakksGamemanager.GameManager.xDisplayModifier
            pos = pygame.mouse.get_pos()
            left = VrakksGamemanager.GameManager.mouseRectanglePos[0] - xDisplayModifier
            top  = VrakksGamemanager.GameManager.mouseRectanglePos[1] - yDisplayModifier
            if pos[0] < left:
                left = pos[0]
            if pos[1] < top:
                top = pos[1]
            pygame.draw.rect(Window, (255, 0, 0), (
            left, top,
            abs(pos[0] + xDisplayModifier - VrakksGamemanager.GameManager.mouseRectanglePos[0]),
            abs(pos[1] + yDisplayModifier - VrakksGamemanager.GameManager.mouseRectanglePos[1])), 2)


def main():
    run = True
    FPS = 60
    Main_font = pygame.font.SysFont("arial",50)
    VrakksGamemanager.BigGrids.mapChopIntoBigGrid(Width*10, Heigth*10)

    def redraw_window():
        Window.blit(BG, (0, 0))
        GUI.load_GUI(2,exampleUnit())
        unitGraphics.drawUnits()
        unitGraphics.drawProjectiles()
        unitGraphics.selectionRectangle()
        pygame.display.update()

    while run:
        clock.tick(FPS)
        VrakksGamemanager.GameManager.IngameTimer += 1  # the actual time is 60 ingameTimer ticks for 1 second
        timer = VrakksGamemanager.GameManager.IngameTimer
        Units.unitMovement.doMovementThingsToUnits()
        VrakksProjectiles.projectiles.moveProjectiles()
        Units.baseUnit.checkIfUnitsCollideWithProjectilesAndAct()
        Units.baseUnit.killUnits()
        VrakksProjectiles.projectiles.deleteProjectiles()
        Units.shooting.findShootTarget()
        for i in range(len(Units.baseUnit.UnitList)):
            if Units.baseUnit.UnitList[i].currentlyShooting == True:
                Units.shooting.shootBulletAtShootTarget(Units.baseUnit.UnitList[i],timer)



        VrakksMouse.checkKeysAndMouseAndAct()



        redraw_window()

if __name__ == '__main__':
	main()