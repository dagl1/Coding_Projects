import pygame
import Places
import Menu
import Weeds
import GameManager

def CheckTargetAndSetTarget(pos):
    IfSubMenusActiveClickOutsideToExit(pos)
    print(pos)
    for iterator in range(len(Menu.MenuManager.ActiveMenuList)):
        if Menu.MenuManager.ActiveMenuList[iterator].rect.collidepoint(pos)==True:
            Menu.Menu.IsClickedAndActivate(Menu.MenuManager.ActiveMenuList[iterator])
            return

    if len(Places.PlaceManager.OpenGrowingPlaces)>0:
        for iterator in range(len(Places.PlaceManager.OpenGrowingPlaces)):
            if Places.PlaceManager.OpenGrowingPlaces[iterator].rect.collidepoint(pos)==True:
                Places.PlaceMenus.IsClickedChangeMenuScreenSeed(Places.PlaceManager.OpenGrowingPlaces[iterator])
                return
    if len(Places.PlaceManager.InUseGrowingPlaces) > 0:
        for iterator in range(len(Places.PlaceManager.InUseGrowingPlaces)):
            if Places.PlaceManager.InUseGrowingPlaces[iterator].rect.collidepoint(pos)==True:
                Places.PlaceMenus.CreateGrowingWeedMenuScreen(Places.PlaceManager.InUseGrowingPlaces[iterator])
                return

def CheckIfMouseAndAct(event,pos):
    if event.type == pygame.MOUSEBUTTONDOWN:
        CheckTargetAndSetTarget(pos)
        return True

def IfSubMenusActiveClickOutsideToExit(pos):
    if Menu.MenuManager.SubMenusBool == True:
        ClickedInsideBool = False
        for iterator in range(len(Menu.MenuManager.ActiveMenuList)):
            if Menu.MenuManager.ActiveMenuList[iterator].rect.collidepoint(pos)==True:
                ClickedInsideBool = True
                try:
                    if GameManager.GameManager.CurrentTarget.rect.collidepoint(pos) != True:
                        Menu.MenuManager.CloseSubMenus()
                        GameManager.GameManager.CurrentTarget = None
                except:
                    pass


        if ClickedInsideBool == False:
            Menu.MenuManager.CloseSubMenus()
            GameManager.GameManager.CurrentTarget = None



def MouseCheckerAndAction(event,pos):
    CheckIfMouseAndAct(event,pos)





# pos = pygame.mouse.get_pos()
