import numpy
import random
import math
import Places
import LoadFilesList
import pygame
import GameManager


class Weeds():

    def __init__(self):
        self.growthCycleNumber= 0
        self.Watered = True
        self.WateringIntervalTime = 600
        self.WateringTime = 0
        self.WaterDelay = 0



    def binomialDistribution():
        RandomNumber =numpy.random.binomial(100, 0.5, size=1)/100
        if RandomNumber > 0.5:
            RandomNumber = 1*((RandomNumber-0.5)*2.0)
        else:
            RandomNumber = -1*((0.5-RandomNumber)*2.0)
        return RandomNumber

    def binomialDistributionSkewed(SkewRatio=0.05): #Skew between -0.5 and 0.5
        RandomNumber =numpy.random.binomial(100, 0.5+SkewRatio, size=1)/100
        if RandomNumber > 0.5:
            RandomNumber = 1*((RandomNumber-0.5)*2.0)
        else:
            RandomNumber = -1*((0.5-RandomNumber)*2.0)
        return RandomNumber

    def randomizeParameters(self):
        for iterator in range(len(self.flavourQualities)):
            RandomNumber = Weeds.binomialDistributionSkewed()
            self.flavourQualities[iterator] = self.flavourQualities[iterator]+self.flavourQualities[iterator]*float(RandomNumber)
        self.tHCContent = self.tHCContent + self.tHCContent*float(Weeds.binomialDistributionSkewed())
        self.cBDContent = self.cBDContent + self.cBDContent*float(Weeds.binomialDistributionSkewed())
        self.quantity = self.quantity  + self.quantity *float(Weeds.binomialDistributionSkewed())
        for iterator in range(self.growCycles):
            self.growTime[iterator]   = self.growTime[iterator] + self.growTime[iterator] * float(Weeds.binomialDistribution())
        for iterator in range(len(self.ColaRatio)):
            self.ColaRatio[iterator] = self.ColaRatio[iterator] + (self.ColaRatio[iterator] *float(Weeds.binomialDistributionSkewed()))

    def AddYieldsToCraftingOptions(self):
        AverageQuantityPerDivision = self.quantity/len(self.ColaRatio)
        TotalQuantityPerDivision = []
        LargestColaRatio = 0
        for iterator in range(len(self.ColaRatio)):
            if LargestColaRatio < self.ColaRatio[iterator]:
                LargestColaRatio = self.ColaRatio[iterator]

        TemporaryColaRatio = self.ColaRatio[:]
        for iterator in range(len(self.ColaRatio)):
            TemporaryColaRatio[iterator] = self.ColaRatio[iterator]/LargestColaRatio
            TotalQuantityPerDivision += [AverageQuantityPerDivision*TemporaryColaRatio[iterator]]

        Crafting.Crafting.AddMainCola(self,TotalQuantityPerDivision[0])
        Crafting.Crafting.AddSecondaryCola(self, TotalQuantityPerDivision[1])
        Crafting.Crafting.AddWeedWaste(self, TotalQuantityPerDivision[2])


    def harvestWeed(self):
            #determine 1 or 2 parents
            n =random.randint(0,10)
            TempList =[]
            if self.mate == None:
                ParentWeed1 = self
                ParentWeed2 = None
            else:
                ParentWeed1 = self
                ParentWeed2 = self.mate
            SeedStack = len(WeedManager.Seedlist)
            for iterator in range(n):
                Seed1 = Seeds(ParentWeed1, SeedStack, ParentWeed2)
                TempList += [Seed1]
            WeedManager.Seedlist += [TempList]
            #Weeds.AddYieldToCraftingOptions(self)
            WeedManager.WeedsList.remove(self)
            Places.PlaceManager.InUseGrowingPlaces.remove(self.SelectedPlace)
            Places.PlaceManager.OpenGrowingPlaces += [self.SelectedPlace]
            del WeedManager.WeedPlaceDictionary[self.SelectedPlace]


    def setMate(self):
        WeedsThatCanBeFertilized = []
        for iterator in range(len(WeedManager.WeedsList)):

            Distance = math.sqrt(abs(WeedManager.WeedsList[iterator].SelectedPlace.x-self.x)**2 + abs(WeedManager.WeedsList[iterator].SelectedPlace.y-self.y)**2)
            if Distance <250 and WeedManager.WeedsList[iterator]!=self and WeedManager.WeedsList[iterator].Male >0:
                RandomNumber =random.randint(100,100)*0.01
                if RandomNumber > WeedManager.WeedsList[iterator].Male:
                    WeedsThatCanBeFertilized += [WeedManager.WeedsList[iterator]]

        if len(WeedsThatCanBeFertilized) >0:
            RandomNumber = random.randint(0,len(WeedsThatCanBeFertilized)-1)
            self.mate = WeedsThatCanBeFertilized[RandomNumber]
        else:
            self.mate = None


    def drawWeedImage(window):
        for iterator in range(len(WeedManager.WeedsList)):
            window.blit(WeedManager.WeedsImageList[0][WeedManager.WeedsList[iterator].growthCycleNumber], (math.ceil(WeedManager.WeedsList[iterator].x), math.ceil(WeedManager.WeedsList[iterator].y)))

    def centerImageToSpace(self):
        return



class Seeds():
    def __init__(self,ParentWeed1,SeedStack,ParentWeed2=None):
        self.lineageTypes = []
        self.lineageQualities = []
        self.lineageDominanceScores = []
        self.flavourTypes = []
        self.flavourQualities = []
        self.DominanceScore =[]
        if ParentWeed2 == None:
            self.name = str(ParentWeed1.name)+" Child "
            self.growCycles = ParentWeed1.growCycles
        else:
            self.name =  str(ParentWeed1.name)+ " x "+ str(ParentWeed2.name) + " "
            self.growCycles = int(numpy.around((ParentWeed1.growCycles + ParentWeed2.growCycles + Weeds.binomialDistributionSkewed(-0.2)) / 2))

        Seeds.SetLineageTraits(self,ParentWeed1,ParentWeed2)
        Seeds.setGrowTime(self,ParentWeed1,ParentWeed2)
        Seeds.setFlavourTypes(self,ParentWeed1,ParentWeed2)
        Seeds.RandomizeMaleness(self,ParentWeed1,ParentWeed2)
        self.tHCContent = 0.2
        self.cBDContent = 0.15
        Seeds.setTHCANDCBDCONTENT(self,ParentWeed1,ParentWeed2)
        self.quantity = 600
        self.ColaRatio = [2, 1.5, 1.5]
        self.AestheticScore = 1
        Seeds.setQuantityAndColaRatioAndAestheticScore(self,ParentWeed1,ParentWeed2)
        Seeds.SetLineageTraits(self,ParentWeed1,ParentWeed2)
        self.SativaPercentage = 0.90
        self.IndicaPercentage = 0.10
        self.AutoFlowerPercentage = 0.00
        Seeds.calculateBreedPercentage(self,ParentWeed1,ParentWeed2)
        self.SeedStack = SeedStack

    def RandomizeMaleness(self,ParentWeed1,ParentWeed2):
        if ParentWeed2 == None:
            Randomnumber = random.randint(0,10)
            if Randomnumber >2:
                self.Male = ParentWeed1.Male + 0.05
            else:
                self.Male = ParentWeed1.Male
        else:
            Randomnumber = random.randint(0, 10)
            if Randomnumber > 2:
                self.Male = (ParentWeed1.Male+ParentWeed2.Male)/2 + 0.08
            else:
                self.Male = (ParentWeed1.Male+ParentWeed2.Male)/2
        # print(self.Male)
    def SetLineageTraits(self,ParentWeed1,ParentWeed2):
        for iterator in range(len(ParentWeed1.flavourQualities)):
            self.lineageTypes += [ParentWeed1.flavourTypes[iterator]]
            self.lineageQualities += [ParentWeed1.flavourQualities[iterator]]
            self.lineageDominanceScores += [ParentWeed1.DominanceScore[iterator]]
        if len(ParentWeed1.lineage) >0:
            for iterator in range(len(ParentWeed1.lineageTypes)):
                self.lineageTypes += [ParentWeed1.lineageTypes[iterator]]
                self.lineageQualities += [ParentWeed1.lineageQualities[iterator]]
                self.lineageDominanceScores += [ParentWeed1.lineageDominanceScores[iterator]]
        if ParentWeed2 != None:
            for iterator in range(len(ParentWeed2.flavourQualities)):
                self.lineageTypes += [ParentWeed2.flavourTypes[iterator]]
                self.lineageQualities += [ParentWeed2.flavourQualities[iterator]]
                self.lineageDominanceScores += [ParentWeed2.DominanceScore[iterator]]

    def setFlavourTypes(self,ParentWeed1,ParentWeed2):
        ParentFlavourTypes = []
        ParentDominanceScores =[]
        ParentFlavourQualities = []
        if ParentWeed2 == None:
            ParentTraitLenghts = len(ParentWeed1.flavourTypes)
            RandomNumber = random.randint(1, 100)
            if RandomNumber < 6:
                ChildTraitLenght = ParentTraitLenghts - 1
            elif RandomNumber > 95:
                ChildTraitLenght = ParentTraitLenghts + 1
            if RandomNumber < 51:
                ChildTraitLenght = math.ceil(ParentTraitLenghts)
            else:
                ChildTraitLenght = math.floor(ParentTraitLenghts)
        else:
            ParentTraitLenghts = (len(ParentWeed1.flavourTypes)+len(ParentWeed2.flavourTypes))/2
            RandomNumber =random.randint(1,100)
            if RandomNumber <6:
                ChildTraitLenght = ParentTraitLenghts-1
            elif RandomNumber>95:
                ChildTraitLenght = ParentTraitLenghts+1
            if RandomNumber <51:
                ChildTraitLenght = math.ceil(ParentTraitLenghts)
            else:
                ChildTraitLenght = math.floor(ParentTraitLenghts)
            for iterator in range(len(ParentWeed2.flavourTypes)):
                ParentFlavourTypes += [ParentWeed2.flavourTypes[iterator]]
                ParentDominanceScores += [ParentWeed2.DominanceScore[iterator]]
                ParentFlavourQualities += [ParentWeed2.flavourQualities[iterator]]

        for iterator in range(len(ParentWeed1.flavourTypes)):
            ParentFlavourTypes += [ParentWeed1.flavourTypes[iterator]]
            ParentDominanceScores += [ParentWeed1.DominanceScore[iterator]]
            ParentFlavourQualities += [ParentWeed1.flavourQualities[iterator]]



        for iterator in range((ChildTraitLenght)):
            RandomNumber =random.randint(1,100)
            if RandomNumber < 6:
                AddedflavourType = random.choice(WeedManager.AllFlavourTypes)
                self.flavourTypes += [AddedflavourType]
                notDone = True
                IsInLineage =False
                while notDone == True:
                    for iterator2 in range(len(self.lineageTypes)):
                        if AddedflavourType == self.lineageTypes[iterator2]:
                            IsInLineage =True

                    if IsInLineage==True:
                        for iterator2 in range(len(self.lineageTypes)):
                            if AddedflavourType == self.lineageTypes[iterator2]:
                                RandomNumber2 =random.randint(1,100)
                                if RandomNumber2 >90:
                                    self.flavourQualities += [self.lineageQualities[iterator2]]
                                    self.DominanceScore += [self.lineageDominanceScores[iterator2]]
                                    notDone=False
                                    break
                    else:
                        self.flavourQualities += [random.randint(1,10)]
                        self.DominanceScore += [random.randint(1,10)]
                        notDone=False


            else:
                ["Fruit", "Moldy","Earthy", "Citrus", "Moldy","Lavendel"]

                for iterator2 in range(len(ParentFlavourTypes)):
                   ParentDominanceScores[iterator2] = ParentDominanceScores[iterator2]+random.randint(1,100)
                max = 0
                for iterator2 in range(len(ParentDominanceScores)):
                    if max < ParentDominanceScores[iterator2]:
                        max = iterator2
                self.DominanceScore += [ParentDominanceScores[max]]
                self.flavourTypes += [ParentFlavourTypes[max]]
                self.flavourQualities += [ParentFlavourQualities[max]]
                ParentFlavourTypes.remove(ParentFlavourTypes[max])
                ParentDominanceScores.remove(ParentDominanceScores[max])
                ParentFlavourQualities.remove(ParentFlavourQualities[max])

    def setTHCANDCBDCONTENT(self,ParentWeed1,ParentWeed2):
        self.tHCContent = ((ParentWeed1.tHCContent + ParentWeed2.tHCContent)/2)+((ParentWeed1.tHCContent + ParentWeed2.tHCContent)/2)* Weeds.binomialDistributionSkewed()
        self.cBDontent = ((ParentWeed1.cBDContent + ParentWeed2.cBDContent) / 2) + ((ParentWeed1.cBDContent + ParentWeed2.cBDContent) / 2) * Weeds.binomialDistributionSkewed()

    def setflavourQualities(self): #randomize FlavourQualities  see Weeds.RandomizeParamters
        return
    def calculateBreedPercentage(self,ParentWeed1,ParentWeed2):
        return
    def setQuantityAndColaRatioAndAestheticScore (self,ParentWeed1,ParentWeed2):
        return

    def setGrowTime(self,ParentWeed1,ParentWeed2):
        GrowTimes1 = []
        GrowTimes2 = []
        self.growTime = [0]*self.growCycles
        # print(ParentWeed1.growCycles,self.growCycles)
        if ParentWeed1.growCycles > self.growCycles:
            for iterator in range(self.growCycles):
                GrowTimes1 += [ParentWeed1.growTime[iterator]]
        elif ParentWeed1.growCycles < self.growCycles:
            GrowTimes1 = ParentWeed1.growTime[:]
            ExtraGrowTimesNeeded = self.growCycles-len(GrowTimes1)
            for iterator in range(ExtraGrowTimesNeeded):
                GrowTimes1 += [(3600/self.growCycles)+(3600/self.growCycles)*float(Weeds.binomialDistribution())]
                Seeds.lookUpLineageGrowTimes()
        else:
            GrowTimes1 = ParentWeed1.growTime[:]
        if ParentWeed2 != None:
            if ParentWeed2.growCycles > self.growCycles:
                for iterator in range(self.growCycles):
                    GrowTimes2 += [ParentWeed2.growTime[iterator]]
            elif ParentWeed2.growCycles < self.growCycles:
                GrowTimes2 = ParentWeed2.growTime[:]
                ExtraGrowTimesNeeded = self.growCycles - len(GrowTimes2)
                for iterator in range(ExtraGrowTimesNeeded):
                    GrowTimes2 += [
                        (3600 / self.growCycles) + (3600 / self.growCycles) * float(Weeds.binomialDistribution())]
                    Seeds.lookUpLineageGrowTimes()
            else:
                GrowTimes2 = ParentWeed2.growTime[:]
        else:
            GrowTimes2 = GrowTimes1

        for iterator in range(self.growCycles):
            # print(self.growTime,GrowTimes1,GrowTimes2)
            self.growTime[iterator] = int (((GrowTimes1[iterator]+GrowTimes2[iterator])+(GrowTimes1[iterator]+GrowTimes2[iterator])*float(Weeds.binomialDistribution()))/2)

    def putPlantInSelectedPlace(self,SelectedPlace,IngameTimer):
        Seeds.plantSeed(self,IngameTimer,SelectedPlace)
        SelectedPlace.isOccupied = True

        # print(Places.PlaceManager.OpenGrowingPlaces)
        Places.PlaceManager.OpenGrowingPlaces.remove(SelectedPlace)
        Places.PlaceManager.InUseGrowingPlaces += [SelectedPlace]
        # print(WeedManager.WeedsList)



    def plantSeed(self,IngameTimer,SelectedPlace):
        Seeds.adjustForPlantingConditions(self)
        Weed = GrowingWeeds(self,IngameTimer,SelectedPlace)
        WeedManager.WeedsList += [Weed]
        WeedManager.WeedPlaceDictionary[SelectedPlace] = Weed
        # print(WeedManager.WeedPlaceDictionary)
        #set x and y position

        WeedManager.Seedlist[self.SeedStack].remove(self)

        return Weed
    def adjustForPlantingConditions(self):
        return



class Amnesia_HazeSeed(Seeds):
    def __init__(self):
        self.name = "Amnesia_Haze"
        self.quantity = 400
        self.ColaRatio = [1,1.5,1]
        self.growCycles = 5 # each cycle will take 1 growtime
        self.growTime = [(250/self.growCycles)]*self.growCycles #3600 equals 1 day
        self.flavourTypes = ["Fruity","Herby","Pungent"]
        self.flavourQualities = [2.2, 3.3, 4.5]
        self.tHCContent = 0.2
        self.cBDContent = 0.10
        self.lineage= self.name
        self.SativaPercentage = 0.10
        self.IndicaPercentage = 0.90
        self.AutoFlowerPercentage = 0.00
        self.DominanceScore = [10, 10, 10]
        self.lineageTypes = []
        self.lineageQualities = []
        self.lineageDominanceScores = []
        self.Male = 0.05
        self.AestheticScore = 1.1
        Weeds.randomizeParameters(self)
        self.SeedStack = len(WeedManager.Seedlist)
        WeedManager.Seedlist += [[self]]

class WhiteWidowSeed(Seeds):
    def __init__(self):
        self.name = "WhiteWidow"
        self.quantity = 600
        self.ColaRatio = [2, 1.5, 1.5]
        self.growCycles = 5 # each cycle will take 1 growtime
        self.growTime = [(120/self.growCycles)]*self.growCycles #3600 equals 1 day
        self.flavourTypes = ["Fruity","Woody","Earthy"]
        self.flavourQualities = [2.2, 3.3, 4.5]
        self.tHCContent = 0.1
        self.cBDContent = 0.2
        self.lineage= self.name
        self.SativaPercentage = 0.90
        self.IndicaPercentage = 0.10
        self.AutoFlowerPercentage = 0.00
        self.DominanceScore = [10, 20, 30]
        self.lineageTypes = []
        self.lineageQualities = []
        self.lineageDominanceScores = []
        self.Male = 0.05
        self.AestheticScore = 1.4
        Weeds.randomizeParameters(self)
        self.SeedStack = len(WeedManager.Seedlist)
        WeedManager.Seedlist += [[self]]


class FourtyPercentAutoflowerSeed(Seeds):
    def __init__(self):
        self.name = "FourtyPercentAutoflower"
        self.quantity = 0
        self.ColaRatio = [1, 1, 1.5]
        self.growCycles = 5 # each cycle will take 1 growtime
        self.growTime = [(3600/self.growCycles)]*self.growCycles #3600 equals 1 day
        self.flavourTypes = ["Fruity","Herby","Pungent"]
        self.flavourQualities = [2.2, 3.3, 4.5]
        self.tHCContent = 0.2
        self.cBDContent = 0.15
        self.lineage= self.name
        self.SativaPercentage = 0.90
        self.IndicaPercentage = 0.10
        self.AutoFlowerPercentage = 0.00
        self.lineageTypes = []
        self.lineageQualities = []
        self.lineageDominanceScores = []
        self.Male = 0.05
        self.AestheticScore = 0.2
        Weeds.randomizeParameters(self)
        self.DominanceScore = [10, 200, 300]
        self.SeedStack = len(WeedManager.Seedlist)
        WeedManager.Seedlist += [[self]]

class GrowingWeeds(Weeds):
    def __init__(self,Seed,IngameTimer,SelectedPlace):
        self.name = Seed.name
        self.growCycles = Seed.growCycles
        self.growTime = Seed.growTime[:]
        self.flavourTypes = Seed.flavourTypes[:]
        self.flavourQualities = Seed.flavourQualities[:]
        self.tHCContent = Seed.tHCContent
        self.cBDContent = Seed.cBDContent
        self.lineage = Seed.name
        self.SativaPercentage = Seed.SativaPercentage
        self.IndicaPercentage = Seed.IndicaPercentage
        self.AutoFlowerPercentage = Seed.AutoFlowerPercentage
        self.lineageTypes =  Seed.lineageTypes
        self.lineageQualities = Seed.lineageQualities
        self.lineageDominanceScores = Seed.lineageDominanceScores
        self.Male = Seed.Male
        self.ColaRatio = Seed.ColaRatio
        self.quantity = Seed.quantity
        self.AestheticScore = Seed.AestheticScore
        GrowingWeeds.adjustForGrowingConditions(self)
        Weeds.__init__(self)
        self.timePlanted = IngameTimer
        self.SelectedPlace = SelectedPlace
        self.x = self.SelectedPlace.x
        self.y = self.SelectedPlace.y
        self.DominanceScore = Seed.DominanceScore

    def adjustForGrowingConditions(self):
        return

    def IsWatered():
        for iterator in range(len(WeedManager.WeedsList)):
            if GameManager.GameManager.IngameTimer - WeedManager.WeedsList[iterator].WateringTime > WeedManager.WeedsList[iterator].WateringIntervalTime:
                WeedManager.WeedsList[iterator].Watered = False
                WeedManager.WeedsList[iterator].WaterDelay +=1

    def WaterPlant(self):
        self.Watered =True
        self.WateringTime = GameManager.GameManager.IngameTimer

    def GrowingCycle(IngameTimer):
        GrowingWeeds.IsWatered()
        for iterator in range(len(WeedManager.WeedsList)):
            if IngameTimer-WeedManager.WeedsList[iterator].timePlanted >WeedManager.WeedsList[iterator].growTime[WeedManager.WeedsList[iterator].growthCycleNumber]+WeedManager.WeedsList[iterator].WaterDelay and WeedManager.WeedsList[iterator].growthCycleNumber <WeedManager.WeedsList[iterator].growCycles-1:
                if WeedManager.WeedsList[iterator].growthCycleNumber == WeedManager.WeedsList[iterator].growCycles-2:
                    Weeds.setMate(WeedManager.WeedsList[iterator])
                WeedManager.WeedsList[iterator].timePlanted = IngameTimer
                WeedManager.WeedsList[iterator].growthCycleNumber +=1

                WeedManager.WeedsList[iterator].WaterDelay = 0
                #print(WeedManager.WeedsList[iterator].growthCycleNumber)
        AllRemoved =False
        iterator = 0
        # while AllRemoved == False:
        #     if len(WeedManager.WeedsList)==iterator:
        #         AllRemoved =True
        #     elif IngameTimer-WeedManager.WeedsList[iterator].timePlanted >WeedManager.WeedsList[iterator].growTime[WeedManager.WeedsList[iterator].growthCycleNumber]+IngameTimer-WeedManager.WeedsList[iterator].WaterDelay:
        #         Weeds.harvestWeed(WeedManager.WeedsList[iterator])
        #         iterator -= 1
        #     iterator +=1
class WeedManager():
    WeedsList = []
    WeedsImageList = []
    WeedsImageList = LoadFilesList.loadWeedImages()
    Seedlist =[]
    AllFlavourTypes = ["Earthy","Citrus","Woody","Lavendel","Fresh","Fruity","Herby","Pungent","Autoflower"]
    RegularFlavourTypes = ["Earthy","Woody","Lavendel","Fresh","Fruity","Herby","Pungent","Autoflower"]
    ExoticFlavourTypes = ["Citrus"]
    WeedPlaceDictionary = {}



if __name__ == '__main__':
    AmnesiaSeed1 = Amnesia_HazeSeed()
    WhiteWidowSeed1 = WhiteWidowSeed()
    Amnesia1 = Seeds.plantSeed(AmnesiaSeed1)
    WhiteWidow1 = Seeds.plantSeed(WhiteWidowSeed1)
    print(WeedManager.Seedlist,Amnesia1.name,WeedManager.WeedsList)
    Weeds.harvestWeed(Amnesia1)
    print(WeedManager.Seedlist,WeedManager.WeedsList)
    # Weeds.harvestWeed(WhiteWidow1)
