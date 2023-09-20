import math
class GameManager():
    IngameTimer = 0
    CurrentlySelectedUnit = []
    ProjectilesList = []
    mouse1IsPressed = False
    mouseRectanglePos = (0,0)
    displayWidth, displayHeight =1800,1200
    xDisplayJumpRange = 200
    yDisplayJumpRange = 200
    xDisplayModifier = 0
    yDisplayModifier = 0


Width1, Heigth1 = 1800,1200
class BigGrids():
    bigGridSizeParamater = 100
    bigGridList = []
    bigGridSizeParamaterList = [20,10]
    amountOfGridsToFillHorizontal = math.ceil(GameManager.displayWidth / bigGridSizeParamater)
    amountOfGridsToFillVertical = math.ceil(GameManager.displayHeight / bigGridSizeParamater)
    currentlyDisplayedGrids = []

    def __init__(self,i,j):
        self.minY = i*BigGrids.bigGridSizeParamater
        self.minX = j*BigGrids.bigGridSizeParamater
        self.maxY = (1+i)*BigGrids.bigGridSizeParamater
        self.maxX = (1+j)*BigGrids.bigGridSizeParamater
        self.centerPos = ((self.minX+self.maxX)/2,(self.minY+self.maxY)/2)


    def mapChopIntoBigGrid(Width,Height):
        BigGrids.bigGridList = [[BigGrids(i,j) for j in range(int(Height/BigGrids.bigGridSizeParamater))] for i in range(int(Width/BigGrids.bigGridSizeParamater))]

    def initialiseDisplayedGrids():
        BigGrids.currentlyDisplayedGrids = [[BigGrids[i][j] for j in range(math.ceil(GameManager.displayHeight / BigGrids.bigGridSizeParamater)+1)] for i in
                                   range(math.ceil(GameManager.displayWidth / BigGrids.bigGridSizeParamater)+1)]
    def setNewDisplayedGrids(translocationVector):
        iModifier = round(translocationVector[1] / GameManager.bigGridSizeParamater)
        jModifier = round(translocationVector[0] / GameManager.bigGridSizeParamater)
        BigGrids.currentlyDisplayedGrids = [[BigGrids[i+iModifier][j+jModifier] for j in range(math.ceil(GameManager.displayHeight / BigGrids.bigGridSizeParamater) + 1)] for i in
                                            range(math.ceil(GameManager.displayWidth / BigGrids.bigGridSizeParamater) + 1)]
    def isInBounds(x,y, boundX, boundY , size):
        if InBounds:
            comparisonValue = size/2
            centreX = boundX + comparisonValue
            centreY = boundY + comparisonValue
            # for x = 60
            # centreX = 150
            # size = 100
            # xBounds = [100,200]
            # comparisonValue = 50
            if abs(centreX-x)>comparisonValue and abs(centreY-y)>comparisonValue:
                pass


def main():
    BigGrids.mapChopIntoBigGrid(Width1-Width1/3, Heigth1)
    for i in range((int((Width1-Width1/3) / BigGrids.bigGridSizeParamater))):
        print(BigGrids.bigGridList[0][i].minX)


if __name__ == '__main__':
	main()


