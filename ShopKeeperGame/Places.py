import Weeds
import random
import pygame
import LoadFilesList
import math
import Menu
import Store
import GameManager
class Places():


    pass

class WeedPlaces(Places):
    def __init__(self,x,y):
        self.isOccupied = True
        self.x= x
        self.y= y
        self.rect = pygame.Rect((self.x,self.y,50,50))
        PlaceManager.OpenGrowingPlaces += [self]
    def drawPlace(self,window):
        x = self.x
        y = self.y
        pygame.draw.rect(window,(144,2,244),(x,y,50,50),3)
        if self.isOccupied ==True:
            pass #for testing
            #window.blit(WeedPlaces.WeedImages[LoadedWeed.ImageList][LoadedWeed.growthCycleNumber],(math.ceil(self.x), math.ceil(self.y))) #real one

    def centerImageToSpace(self):
        return

class PlaceMenus():

    def CreateSeedMenuScreen(self):
        x = self.x
        y = self.y
        xrange = 100
        yrange = 160
        Counter =0
        for iterator in range(len(Weeds.WeedManager.Seedlist)):
            if len(Weeds.WeedManager.Seedlist[iterator])>0:
                newMenuPos = (x+xrange,y+(Counter)*yrange)
                Counter += 1
                MainMenu = SeedMenuAtPlace(Weeds.WeedManager.Seedlist[iterator],newMenuPos,self)
                PlaceMenus.CheckConditionsForAvailableMenusSeed(MainMenu)
                PlaceMenus.CreateSubMenusSeed(MainMenu)

    def CheckConditionsForAvailableMenusSeed(self):
        PlaceManager.PossiblePlaceSubMenuOptions =1
        return
    def CreateSubMenusSeed(self):
        for iterator in range((PlaceManager.PossiblePlaceSubMenuOptions)):
            PlantSubMenuOption(self, iterator)


    def IsClickedChangeMenuScreenSeed(self):
        PlaceMenus.CreateSeedMenuScreen(self)
        PlaceMenus.RemoveNonInteractableMenusUnderneathMenus(self)
        PlaceMenus.MakeAvailableMenusInteractable(self)
        Menu.MenuManager.SubMenusBool = True

    def CreateGrowingWeedMenuScreen(self):
        x = self.x
        y = self.y

        xrange = 100
        yrange = 160
        newMenuPos = (x+xrange,y)
        MainMenu = WeedMenuAtGrowingPlace(newMenuPos,self)

        PlaceMenus.CheckConditionsForAvailableMenusWeed(MainMenu)
        PlaceMenus.CreateSubMenusWeed(MainMenu)

    def CheckConditionsForAvailableMenusWeed(MainMenu):
        PlaceManager.PossiblePlaceSubMenuOptions = 0
        Weed = Weeds.WeedManager.WeedPlaceDictionary[MainMenu.SelectedPlace]
        iterator = 0
        IngameTimer = GameManager.GameManager.IngameTimer
        if IngameTimer - Weed.timePlanted > Weed.growTime[Weed.growthCycleNumber] + Weed.WaterDelay and Weed.growthCycleNumber == Weed.growCycles - 1:
            HarvestSubMenuOption(MainMenu, iterator)
            iterator += 1

        WaterSubMenuOption(MainMenu, iterator)


    def CreateSubMenusWeed(MainMenu):
        for iterator in range((PlaceManager.PossiblePlaceSubMenuOptions)):
            HarvestSubMenuOption(MainMenu, iterator)
        return

    def RemoveNonInteractableMenusUnderneathMenus(self):
        return
    def MakeAvailableMenusInteractable(self):
        return
    def ShowAvailableMenus():
        return



class WeedMenuAtGrowingPlace():

    def __init__(self,newMenuPos,SelectedPlace):
        self.Width = 350
        self.Height = 150
        self.LineWidth = 3
        self.x = newMenuPos[0]
        self.y = newMenuPos[1]
        self.rect = pygame.Rect(self.x, self.y, self.Width, self.Height)
        self.font = pygame.font.SysFont("arial", 50)
        self.SelectedPlace = SelectedPlace
        self.text = ""
        self.medium = "Soil"
        Main_font = pygame.font.SysFont("arial", 50)
        self.Label = Main_font.render(self.text, 1, (255, 255, 255))
        Menu.MenuManager.ShownMenuList += [self]
        Menu.MenuManager.TemporaryMenus += [self]
        Menu.MenuManager.SubMenusBool = True


class SeedMenuAtPlace():

    def __init__(self,SeedListOfSeed,newMenuPos,SelectedPlace):
        self.Width = 350
        self.Height = 150
        self.LineWidth = 3
        self.x = newMenuPos[0]
        self.y = newMenuPos[1]
        self.rect = pygame.Rect(self.x, self.y, self.Width, self.Height)
        self.font = pygame.font.SysFont("arial", 50)
        self.SelectedPlace = SelectedPlace
        self.Seed = SeedListOfSeed[0]
        self.text = self.Seed.name
        Main_font = pygame.font.SysFont("arial", 50)
        self.Label = Main_font.render(self.text, 1, (255, 255, 255))
        Menu.MenuManager.ShownMenuList += [self]
        Menu.MenuManager.TemporaryMenus += [self]

class PlaceManager():
    OpenGrowingPlaces =[]
    InUseGrowingPlaces =[]
    ActivePlaceSubMenuOptions=[]
    PossiblePlaceSubMenuOptions =0

class PlantSubMenuOption():
    def __init__(self,MainMenu,iterator):
        self.Width = 150
        self.Height = 100
        self.LineWidth = 3
        self.MainMenu = MainMenu
        if iterator <1:
            self.x = MainMenu.x
        else:
            self.x =  PlaceManager.ActivePlaceSubMenuOptions[iterator-1].x + PlaceManager.ActivePlaceSubMenuOptions[iterator-1].Width
        self.y = MainMenu.y +50
        self.rect = pygame.Rect(self.x, self.y, self.Width, self.Height)
        self.text = "Plant Seed"
        Main_font = pygame.font.SysFont("arial", 34)
        self.Label = Main_font.render(self.text, 1, (255, 255, 255))
        PlaceManager.ActivePlaceSubMenuOptions += [self]
        Menu.MenuManager.ShownMenuList += [self]
        Menu.MenuManager.ActiveMenuList += [self]
        Menu.MenuManager.TemporaryMenus += [self]

    def RunIfClicked(self):
        IngameTimer = GameManager.GameManager.IngameTimer

        Weeds.Seeds.putPlantInSelectedPlace(self.MainMenu.Seed,self.MainMenu.SelectedPlace,IngameTimer)
        Menu.MenuManager.CloseSubMenus()
        return




class WaterSubMenuOption():
    def __init__(self,MainMenu,iterator):

        self.Width = 150
        self.Height = 100
        self.LineWidth = 3
        self.MainMenu = MainMenu
        if iterator < 1:
            self.x = MainMenu.x
        else:

            self.x = PlaceManager.ActivePlaceSubMenuOptions[iterator - 1].x + PlaceManager.ActivePlaceSubMenuOptions[iterator - 1].Width
        self.y = MainMenu.y + 50
        self.rect = pygame.Rect(self.x, self.y, self.Width, self.Height)
        self.text = "Water Plant"
        Main_font = pygame.font.SysFont("arial", 34)
        self.Nutrient = "mix1 placeholder for nutrient mechanics"
        self.Label = Main_font.render(self.text, 1, (255, 255, 255))
        PlaceManager.ActivePlaceSubMenuOptions += [self]
        Menu.MenuManager.ShownMenuList += [self]
        Menu.MenuManager.ActiveMenuList += [self]
        Menu.MenuManager.TemporaryMenus += [self]

    def RunIfClicked(self):
        IngameTimer = GameManager.GameManager.IngameTimer
        Weed = Weeds.WeedManager.WeedPlaceDictionary[self.MainMenu.SelectedPlace]
        Weeds.GrowingWeeds.WaterPlant(Weed)
        Menu.MenuManager.CloseSubMenus()


class HarvestSubMenuOption():
    def __init__(self, MainMenu,iterator):
        self.Width = 150
        self.Height = 100
        self.LineWidth = 3
        self.MainMenu = MainMenu
        if iterator < 1:
            self.x = MainMenu.x

        else:
            self.x = PlaceManager.ActivePlaceSubMenuOptions[iterator - 1].x + PlaceManager.ActivePlaceSubMenuOptions[ iterator - 1].Width
        self.y = MainMenu.y + 50
        self.rect = pygame.Rect(self.x, self.y, self.Width, self.Height)
        self.text = "Harvest Weed"
        Main_font = pygame.font.SysFont("arial", 34)
        self.Label = Main_font.render(self.text, 1, (255, 255, 255))
        PlaceManager.ActivePlaceSubMenuOptions += [self]
        Menu.MenuManager.ShownMenuList += [self]
        Menu.MenuManager.ActiveMenuList += [self]
        Menu.MenuManager.TemporaryMenus += [self]

    def RunIfClicked(self):
        IngameTimer = GameManager.GameManager.IngameTimer
        Weed = Weeds.WeedManager.WeedPlaceDictionary[self.MainMenu.SelectedPlace]
        Weeds.Weeds.harvestWeed(Weed)
        Menu.MenuManager.CloseSubMenus()
        return




