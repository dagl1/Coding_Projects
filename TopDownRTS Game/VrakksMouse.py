import pygame
from pygame.locals import *
import VrakksGamemanager
import Units
import statistics


def checkKeysAndMouseAndAct():
    yDisplayModifier = VrakksGamemanager.GameManager.yDisplayModifier
    xDisplayModifier = VrakksGamemanager.GameManager.xDisplayModifier
    keys = pygame.key.get_pressed()
    pos = pygame.mouse.get_pos()
    pos = (pos[0]+xDisplayModifier,pos[1]+yDisplayModifier)
    for event in pygame.event.get():
        if event.type == pygame.MOUSEBUTTONDOWN:
            if event.button == 1 and VrakksGamemanager.GameManager.mouse1IsPressed != True:
                VrakksGamemanager.GameManager.mouse1IsPressed = True
                VrakksGamemanager.GameManager.mouseRectanglePos = pos
        if event.type == pygame.MOUSEBUTTONUP:
            if keys[K_LCTRL]:
                if len(VrakksGamemanager.GameManager.CurrentlySelectedUnit) != 0:
                    for CurrentlySelectedUnit in VrakksGamemanager.GameManager.CurrentlySelectedUnit:
                        if event.button == 3:
                            inrange = True
                            Units.shooting.getShootTarget(CurrentlySelectedUnit, pos[0], pos[1])
                            CurrentlySelectedUnit.shootTargetUnit = 1
                            if inrange:
                                CurrentlySelectedUnit.currentlyShooting = True
                        # VrakksGamemanager.GameManager.CurrentlySelectedUnit.targetx = VrakksGamemanager.GameManager.CurrentlySelectedUnit.x
                        # VrakksGamemanager.GameManager.CurrentlySelectedUnit.targety = VrakksGamemanager.GameManager.CurrentlySelectedUnit.y
            elif event.button == 1 and VrakksGamemanager.GameManager.mouse1IsPressed == True:
                selectUnitsInRectangle(pos)
                VrakksGamemanager.GameManager.mouse1IsPressed = False
            elif event.button == 2:
                print("middle mouse button")
            elif event.button == 3:
                decideMovementOrShootTargetAcquisition(pos)
            elif event.button == 4:
                print("mouse wheel up")
            elif event.button == 5:
                print("mouse wheel down")
        if event.type == pygame.QUIT:
            run = False
            pygame.display.quit()
            pygame.quit()
        elif event.type == pygame.KEYDOWN:
            if event.key == pygame.K_ESCAPE:
                pygame.quit()
                return
            elif event.key == pygame.K_UP:
                VrakksGamemanager.GameManager.yDisplayModifier -= VrakksGamemanager.GameManager.yDisplayJumpRange
                if VrakksGamemanager.GameManager.yDisplayModifier < 0:
                    VrakksGamemanager.GameManager.yDisplayModifier = 0
                print(f"yDisplayModifier is {VrakksGamemanager.GameManager.yDisplayModifier}")
            elif event.key == pygame.K_DOWN:
                VrakksGamemanager.GameManager.yDisplayModifier += VrakksGamemanager.GameManager.yDisplayJumpRange
                print(f"yDisplayModifier is {VrakksGamemanager.GameManager.yDisplayModifier}")
            elif event.key == pygame.K_RIGHT:
                VrakksGamemanager.GameManager.xDisplayModifier += VrakksGamemanager.GameManager.xDisplayJumpRange
                print(f"xDisplayModifier is {VrakksGamemanager.GameManager.xDisplayModifier}")
            elif event.key == pygame.K_LEFT:
                VrakksGamemanager.GameManager.xDisplayModifier -= VrakksGamemanager.GameManager.xDisplayJumpRange
                if VrakksGamemanager.GameManager.xDisplayModifier < 0:
                    VrakksGamemanager.GameManager.xDisplayModifier = 0
                print(f"xDisplayModifier is {VrakksGamemanager.GameManager.xDisplayModifier}")
    #return mouse1IsPressed
def selectUnitsInRectangle(pos):
    if len(VrakksGamemanager.GameManager.CurrentlySelectedUnit) != 0:
        for CurrentlySelectedUnit in VrakksGamemanager.GameManager.CurrentlySelectedUnit:
            print(CurrentlySelectedUnit)
            CurrentlySelectedUnit.selected = False

    left = VrakksGamemanager.GameManager.mouseRectanglePos[0]
    top  = VrakksGamemanager.GameManager.mouseRectanglePos[1]
    if pos[0]<left:
        left = pos[0]
    if pos[1]<top:
        top = pos[1]
    selectionRect = (left, top,
     abs(pos[0] - VrakksGamemanager.GameManager.mouseRectanglePos[0]), abs(pos[1] - VrakksGamemanager.GameManager.mouseRectanglePos[1]))
    for unit in Units.baseUnit.UnitList: ##########add in code to check only relevant grids!!!!!!!!!!!!
        if unit.rect.colliderect(selectionRect) == True:
            checkIfUnitIsSelectable(unit)

def selectUnit(unit):

    VrakksGamemanager.GameManager.CurrentlySelectedUnit += [unit]
    unit.selected = True
    print("just selected", str(unit))

def checkIfUnitIsSelectable(unit):

    selectUnit(unit)
    pass



def setUnitShootTarget(unit,pos):
    Units.shooting.getShootTarget(unit, pos[0], pos[1])

    checkIfNecessaryToMove(unit,pos)
    pass

def checkIfNecessaryToMove(unit,pos):
    #Units.unitMovement.setUnitTarget(unit,pos[0],pos[1])
    pass

def setSquadTargetLocations(pos):
    squadSize = len(VrakksGamemanager.GameManager.CurrentlySelectedUnit)
    currentLocations = []
    for CurrentlySelectedUnit in VrakksGamemanager.GameManager.CurrentlySelectedUnit:
        currentLocations += [(CurrentlySelectedUnit.x, CurrentlySelectedUnit.y)]
        print(currentLocations)
    vector = calculateAverageVectorToTarget(currentLocations, pos)
    calculateFormation(squadSize, vector)

def calculateAverageVectorToTarget(currentLocations,pos):
    vectorList =[]
    for coordinate in currentLocations:
        print(coordinate)

        xDistance = coordinate[0] - pos[0]
        yDistance = coordinate[1] - pos[1]
        if (xDistance != 0 and yDistance != 0):
            if abs(xDistance) >= abs(yDistance):
                yVector = yDistance / abs(xDistance)
                if xDistance > 0:
                    xVector = 1
                elif xDistance < 0:
                    xVector = -1
            else:
                xVector = xDistance / abs(yDistance)
                if yDistance > 0:
                    yVector = 1
                elif yDistance < 0:
                    yVector = -1
            vectorList += [(xVector, yVector)]
    return (statistics.mean(x for x,y in vectorList),statistics.mean(y for x, y in vectorList))

def calculateFormation(squadSize, vector, **formation):
    formation = "Rectangle"
    if formation == "Rectangle":
        pass
    if formation == "FoxholePositions":
        pass


def decideMovementOrShootTargetAcquisition(pos):
    if len(VrakksGamemanager.GameManager.CurrentlySelectedUnit) != 0:
        isTrue = False
        for i in range(len(Units.baseUnit.UnitList)):
            if VrakksGamemanager.GameManager.CurrentlySelectedUnit[0].team != Units.baseUnit.UnitList[i].team:
               if Units.baseUnit.UnitList[i].rect.collidepoint(pos) == True:
                  for CurrentlySelectedUnit in VrakksGamemanager.GameManager.CurrentlySelectedUnit:
                        print("d")
                        setUnitShootTarget(CurrentlySelectedUnit, pos)
                        CurrentlySelectedUnit.shootTargetUnit = Units.baseUnit.UnitList[i]
                        Units.shooting.getShootTarget(Units.baseUnit.UnitList[i],pos[0],pos[1])
                        CurrentlySelectedUnit.currentlyShooting = True
                        isTrue = True
                        break

        if isTrue == False:
            setSquadTargetLocations(pos)
            #Units.unitMovement.setUnitTarget(CurrentlySelectedUnit,pos[0], pos[1])



# pos = pygame.mouse.get_pos()
