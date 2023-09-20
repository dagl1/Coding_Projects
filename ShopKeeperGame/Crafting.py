import Store
import Weed
class Crafting():

    def AddMainCola(Weed, MainColaQuantity):
        YieldQualityRatio = 1
        MainColaWeed(Weed, MainColaQuantity, YieldQualityRatio)

    def AddSecondaryCola(Weed, SecondaryColaQuantity):
        YieldQualityRatio = 0.8
        SecondaryColaWeed(Weed,SecondaryColaQuantity,YieldQualityRatio)

    def AddWeedWaste(Weed, WeedWasteQuantity):
        YieldQualityRatio = 0.2
        WeedWaste(Weed,WeedWasteQuantity,YieldQualityRatio)

class CraftingComponents():

    def __init__(self,Weed,Quantity,YieldQualityRatio):
        self.quantity = Quantity
        self.flavourTypes = Weed.flavourTypes
        self.SativaPercentage = Weed.SativaPercentage
        self.IndicaPercentage = Weed.IndicaPercentage
        CraftingComponents.setFlavourQualitiesAndTHCContent(self,Weed,YieldQualityRatio)


        CraftingManager.CraftingComponentsList += [self]

    def DryWeed(self):
        return


    def setFlavourQualitiesAndTHCContent(self,Weed,YieldQualityRatio):
        self.flavourQualities = Weed.flavourQualies[:]
        for iterator in range(len(Weed.flavourQualities)):
            self.flavourQualities[iterator] = self.flavourQualities[iterator] * YieldQualityRatio
            self.tHCContent = Weed.tHCContent * YieldQualityRatio
            self.cBDContent = Weed.cBDContent * YieldQualityRatio
    def CraftingOptionCutWeed(self,AmountToCut):
        if self.CraftableToWeed == True:
            Store.SellableWeed(self,AmountToCut)
            self.quantity = self.quantity - AmountToCut
            #add timer system



    def CraftingOptionBakeBrownie(Weed,*otherWeeds):
        return


class MainColaWeed(CraftingComponents):
    def __init__(self,Weed,Quantity,YieldQualityRatio):
        CraftingComponents.__init__(self,Weed,Quantity,YieldQualityRatio)
        self.CraftableToWeed = True
        self.CraftableToSpaceCake = True

class SecondaryColaWeed(CraftingComponents):
    def __init__(self,Weed,Quantity,YieldQualityRatio):
        CraftingComponents.__init__(self,Weed,Quantity,YieldQualityRatio)
        self.CraftableToWeed = True
        self.CraftableToSpaceCake = True
        self.quantity = self.quantity * YieldQualityRatio

class WeedWaste(CraftingComponents):
    def __init__(self,Weed,Quantity,YieldQualityRatio):
        CraftingComponents.__init__(self,Weed,Quantity,YieldQualityRatio)
        self.CraftableToWeed = False
        self.CraftableToSpaceCake = True




class CraftingManager():
    CraftingComponentsList = []
