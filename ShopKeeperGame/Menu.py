import pygame
import Places

class Menu():
    def __init__(self):
        pass
    def DrawMenus(Window):
        for iterator in range(len(MenuManager.ShownMenuList)):
            pygame.draw.rect(Window,(255,100,100), MenuManager.ShownMenuList[iterator].rect,MenuManager.ShownMenuList[iterator].LineWidth)
            Window.blit(MenuManager.ShownMenuList[iterator].Label, (MenuManager.ShownMenuList[iterator].x, MenuManager.ShownMenuList[iterator].y))

    def IsClickedAndActivate(self):
        self.RunIfClicked()

class MenuManager():
    ActiveMenuList = []
    ShownMenuList = []
    SubMenusBool = False
    TemporaryMenus = []

    def CloseSubMenus():
        for iterator in range(len(MenuManager.TemporaryMenus)):
            try:
                MenuManager.ActiveMenuList.remove(MenuManager.TemporaryMenus[iterator])
            except:
                pass
            try:
                MenuManager.ShownMenuList.remove(MenuManager.TemporaryMenus[iterator])
            except:
                pass
        for iterator in range(len(Places.PlaceManager.ActivePlaceSubMenuOptions)):
            try:
                Places.PlaceManager.ActivePlaceSubMenuOptions= []
            except:
                pass

class InventoryMenu(Menu):
    def __init__(self):
        self.Width =550
        self.Height = 350
        self.x = 1800-self.Width
        self.y = 1200-self.Height
        self.LineWidth = 4
        self.rect = pygame.Rect(self.x,self.y,self.Width,self.Height)
        self.text ="InventoryMenu"
        Main_font = pygame.font.SysFont("arial", 50)
        self.Label = Main_font.render(self.text, 1, (255, 255, 255))
        MenuManager.ActiveMenuList += [self]
        MenuManager.ShownMenuList += [self]

