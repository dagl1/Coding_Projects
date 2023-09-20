import math
import Weeds
import random
import GameManager

class Level1Settings():
    def __init__(self):
        self.DifficultySetting = 1
        self.TimeScalingFactor = 1
        self.ScalingFactor = self.DifficultySetting + self.TimeScalingFactor
        self.AmountOfCustomersPerDay = 80
        self.AmountOfCustomersPerDayScalingFactor =1
        self.RegularCustomersMoney = 100
        self.AmountOfNeedsPerCustomer = 2
        self.AmountOfNeedsPerCustomerScalingFactor = 1
        self.PercentageOfNeedyRegularCustomers = 0.2
        self.PercentageOfNeedyRegularCustomersScalingFactor =1
        self.DaytimeNightTimeIntervalRatio = [5,3]
        self.AmountsOfCustomerPerDaytimeSliceRatio = [1,2,1]
        self.RegularNeedQualityLevel = 3
        self.THCContentLevel = 0.1
        self.CBDContentLevel = 0.1
        self.RegularNeedQualityLevelScalingFactor = 1
        self.PercentageOfExoticNeedsCustomers = 0.3
        self.PercentageOfExoticNeedsCustomersScalingFactor = 0.1
        self.AmountOfNeedyNeeds = 1
        self.PercentageOfNeedyExoticNeedsCustomers = 0.8
        self.PercentageOfNeedyExoticNeedsCustomersScalingFactor = 1

        self.AmountOfExoticNeedsPerCustomer = 1
        self.PercentageOfNeedyExoticNeedsCustomersScalingFactor =1

        self.ExoticNeedsCustomersMoney = 150
        self.AmountOfRegularCustomerNeedsProfiles = 10
        self.AmountOfRegularCustomerNeedsProfilesTimeScaling = 0.3
        self.AmountOfExoticNeedsCustomerNeedsProfiles = 2
        self.AmountOfExoticNeedsCustomerNeedsProfilesTimeScaling = 0.2

def setCustomerNeedsProfile(Customer, Level):
    
    RandomNumber = random.randint(1,100)
    if RandomNumber <= Level.PercentageOfExoticNeedsCustomers*100:
        NeedsProfile = random.choice(CustomerNeedsOptionsManager.ExoticNeedsProfilesList)
    else:
        NeedsProfile = random.choice(CustomerNeedsOptionsManager.RegularNeedsProfilesList)
    Customer.NeedsProfile = NeedsProfile
    print("setCustomerNeeds")
    randomizeCustomerNeedsAndAddPriorities(Customer, Level)
        #choose customerneedsprofile, potentially based on level/scaling settings
        #chance to alter CustomerNeedsProfile to randomize things slightly and Add priorities
            #if exotic, very high chance to set exotic quality to needy
                #small chance to set another quality that is not exotic to needy
            #else set a random quality or attribute to needy
        
        #CustomerCaresAboutAesthetic
        #caremoreaboutAmountThanQuality
            # then they want to get more quantity and so in buyoffer will find the weed closest to their NeedsProfile that gives them the specific amount

def randomizeCustomerNeedsAndAddPriorities(Customer, Level):
    for iterator in range(len(Customer.NeedsProfile.Needslist)):
        if Customer.NeedsProfile.NeedsLabelsList[iterator] != "FlavourTypeQualityCombination" and Customer.NeedsProfile.NeedsLabelsList[iterator] !="ExoticFlavourTypeQualityCombination":
            Customer.NeedsProfile.Needslist[iterator] = Customer.NeedsProfile.Needslist[iterator] + Customer.NeedsProfile.Needslist[iterator]*Weeds.Weeds.binomialDistribution()
        else:
            Customer.NeedsProfile.Needslist[iterator] = (Customer.NeedsProfile.Needslist[iterator][0] + Customer.NeedsProfile.Needslist[iterator][0] * Weeds.Weeds.binomialDistribution(),Customer.NeedsProfile.Needslist[iterator][1])

    RandomAmountOfNeedyNeeds = random.choice(0,Level.AmountOfNeedyNeeds)
    if Customer.NeedsProfile.Exotic == False:
        PrevousChoices = []
        for iterator in range(RandomAmountOfNeedyNeeds):
            NotDone = True
            while NotDone ==True:
                RandomNeedyNeedChoiceIterator = random.randint(0,len(Customer.NeedsProfile.Needslist))
                if RandomNeedyNeedChoiceIterator in PrevousChoices:
                    pass
                else:
                    PrevousChoices += [RandomNeedyNeedChoiceIterator]
                    Customer.NeedsProfile.NeedyNeedsList += [RandomNeedyNeedChoiceIterator]
    else:
        PrevousChoices = []
        counter = 0
        ExoticIterators =[]
        for iterator in range(len(Customer.NeedsProfile.NeedsList)):
            if Customer.NeedsProfile.NeedsLabelsList[iterator] == "ExoticFlavourTypeQualityCombination" or Customer.NeedsProfile.NeedsLabelsList[iterator] == "ExoticFlavourType":
                counter+=1
                ExoticIterators += [iterator]
        for iterator in range(RandomAmountOfNeedyNeeds):
            RandomNumber1 = random.randint(1, 100)
            NotDone = True
            if RandomNumber1 > 20:
                while NotDone == True:
                    RandomNeedyExoticNeedChoiceIterator = random.randint(0,len(ExoticIterators))
                    if RandomNeedyExoticNeedChoiceIterator in PrevousChoices:
                        pass
                    else:
                        PreviousChoices += ExoticIterators[RandomNeedyExoticNeedChoiceIterator]
                        Customer.NeedsProfile.NeedyNeedsList += ExoticIterators[RandomNeedyExoticNeedChoiceIterator]
            else:
                while NotDone == True:
                    RandomNeedyNeedChoiceIterator = random.randint(0, len(Customer.NeedsProfile.Needslist))
                    if RandomNeedyNeedChoiceIterator in PrevousChoices or RandomNeedyNeedChoiceIterator in ExoticIterators:
                        pass
                    else:
                        PrevousChoices += [RandomNeedyNeedChoiceIterator]
                        Customer.NeedsProfile.NeedyNeedsList += [RandomNeedyNeedChoiceIterator]

        #If exotic then have higher chance to set ExoticToNeedy



def setCustomerNeeds(Customer,Level,Scaling=None):
    setCustomerNeedsProfile(Customer,Level)
    randomizeCustomerNeedsAndAddPriorities(Customer, Level)


def setLevelNeedsAndUpdate(level):

    if len(CustomerNeedsOptionsManager.LevelFlavourTypesRegularNeedsList) < math.floor(level.AmountOfNeedsPerCustomer):
        Difference =(math.floor(level.AmountOfNeedsPerCustomer-len(CustomerNeedsOptionsManager.LevelFlavourTypesRegularNeedsList)))*3
        CustomerNeedsOptionsManager.ListOfRegularNeedsIterators = []
        for iterator in range(Difference):
            NotDone = True
            while NotDone == True:
                NoNumberFound = True
                RandomNumber = random.randint(0, len(Weeds.WeedManager.RegularFlavourTypes)-1)
                if len(CustomerNeedsOptionsManager.ListOfRegularNeedsIterators) > 0:
                    for iterator2 in range(len(CustomerNeedsOptionsManager.ListOfRegularNeedsIterators)):
                        if RandomNumber == CustomerNeedsOptionsManager.ListOfRegularNeedsIterators [iterator2]:
                            NoNumberFound = False
                if NoNumberFound ==True:
                    CustomerNeedsOptionsManager.LevelFlavourTypesRegularNeedsList += [Weeds.WeedManager.RegularFlavourTypes [RandomNumber]]
                    CustomerNeedsOptionsManager.ListOfRegularNeedsIterators += [RandomNumber]
                    NotDone = False


    if len(CustomerNeedsOptionsManager.LevelExoticFlavourTypesNeedsList) < math.floor(level.AmountOfExoticNeedsPerCustomer):
        Difference = (len(CustomerNeedsOptionsManager.LevelExoticFlavourTypesNeedsList) - math.floor(level.AmountOfExoticNeedsPerCustomer))
        CustomerNeedsOptionsManager.ListOfExoticNeedsIterators = []
        for iterator in range(Difference):
            NotDone = True
            while NotDone == True:
                NoNumberFound = True
                RandomNumber = random.randint(0, len(Weeds.WeedManager.ExoticFlavourTypes)-1)
                if len(CustomerNeedsOptionsManager.ListOfExoticNeedsIterators) > 0:
                    for iterator2 in range(len(CustomerNeedsOptionsManager.ListOfExoticNeedsIterators)):
                        if RandomNumber == CustomerNeedsOptionsManager.ListOfExoticNeedsIterators[iterator2]:
                            NoNumberFound = False
                if NoNumberFound == True:
                    CustomerNeedsOptionsManager.LevelExoticFlavourTypesNeedsList += [Weeds.WeedManager.ExoticFlavourTypes[RandomNumber]]
                    CustomerNeedsOptionsManager.ListOfExoticNeedsIterators += [RandomNumber]
                    NotDone = False

def createCustomerNeedProfilesAndUpdate(Level):
    Level.AmountOfRegularCustomerNeedsProfilesTimeScaling = 0.3
    print(Level.AmountOfRegularCustomerNeedsProfiles)
    if len(CustomerNeedsOptionsManager.RegularNeedsProfilesList) < math.floor(Level.AmountOfRegularCustomerNeedsProfiles):
        DifferenceInProfiles = len(CustomerNeedsOptionsManager.RegularNeedsProfilesList) -math.floor(Level.AmountOfRegularCustomerNeedsProfiles)
        print(Level.PercentageOfExoticNeedsCustomers)
        ExoticNeedAmount= round(abs(Level.PercentageOfExoticNeedsCustomers * DifferenceInProfiles))
        RegularNeedAmount =  ExoticNeedAmount -DifferenceInProfiles
        print(ExoticNeedAmount,RegularNeedAmount)
        for iterator in range(RegularNeedAmount):
            AmountOfNeeds = Level.AmountOfNeedsPerCustomer
            if AmountOfNeeds > 0:
                RandomNumber = random.randint(1, 100)
                if RandomNumber > 95:
                    AmountOfNeeds = 1
                elif RandomNumber <= 5 and AmountOfNeeds != 1:
                    AmountOfNeeds -= 1
                elif RandomNumber > 90 and RandomNumber <= 95:
                    AmountOfNeeds += 1
                elif RandomNumber > 5 and RandomNumber <= 7:
                    AmountOfNeeds += 2
            else:
                AmountOfNeeds = 1
            Exotic = False
            print('running2')
            CustomerNeedsProfileBase(Level,AmountOfNeeds,GameManager.GameManager.IngameTimer,Exotic)
        
        for iterator in range(ExoticNeedAmount):
            AmountOfNeeds = Level.AmountOfNeedsPerCustomer
            if AmountOfNeeds > 0:
                RandomNumber = random.randint(1, 100)
                if RandomNumber > 95:
                    AmountOfNeeds = 1
                elif RandomNumber <= 5 and AmountOfNeeds != 1:
                    AmountOfNeeds -= 1
                elif RandomNumber > 90 and RandomNumber <= 95:
                    AmountOfNeeds += 1
                elif RandomNumber > 5 and RandomNumber <= 7:
                    AmountOfNeeds += 2
            else:
                AmountOfNeeds = 1
            print("Runing3")
            Exotic = True
            CustomerNeedsProfileBase(Level,AmountOfNeeds,GameManager.GameManager.IngameTimer,Exotic)
            print('hey')





    #flavourqualities * flavourtypes
    # thcContent
    # CBDcontent
    #if exotic set exotic qualities (exotic flavourquality or high quality/thc content)
    #AestheticScore (then the code will only use this if CustomerCaresAboutAesthetic is true)

def createCustomerNeedProfilesLevelStart(Level):
    print("running")
    setLevelNeedsAndUpdate(Level)
    createCustomerNeedProfilesAndUpdate(Level)

class CustomerNeedsProfileBase():
    def __init__(self,Level,AmountOfNeeds,IngameTimer = None, DifficultyScore = None,Exotic = False):
        self.Needslist =[]
        self.NeedsLabelsList = []
        self.NeedyNeedsList = []
        print(self.Needslist)

        if Exotic == False:
            self.Exotic = False
            CustomerNeedsProfileBase.setRegularNeeds(self,Level,AmountOfNeeds)
            CustomerNeedsOptionsManager.RegularNeedsProfilesList += [self]

        else:
            self.Exotic = True
            CustomerNeedsProfileBase.setExoticNeeds(self,Level,AmountOfNeeds)
            CustomerNeedsOptionsManager.ExoticNeedsProfilesList += [self]

    def setExoticNeeds(self,Level,AmountOfNeeds,IngameTimer = None, DifficultyScore = None):
        AmountOfCreatedNeeds = 0
        AmountOfOptions = 2
        AmountOfExoticNeedsPerCustomer = Level.AmountOfExoticNeedsPerCustomer
        if AmountOfExoticNeedsPerCustomer > AmountOfNeeds:
            AmountOfExoticNeedsPerCustomer = AmountOfNeeds

        while AmountOfCreatedNeeds < int(AmountOfExoticNeedsPerCustomer):
            if AmountOfNeeds - AmountOfCreatedNeeds > 1:
                RandomNumber = random.randint(1,AmountOfOptions)
                if RandomNumber == 1:
                    NotDone = True
                    while NotDone == True:
                        RandomNumber2 = random.randint(0, len(CustomerNeedsOptionsManager.LevelExoticFlavourTypesNeedsList)-1)
                        if not CustomerNeedsOptionsManager.LevelExoticFlavourTypesNeedsList[RandomNumber2] in self.Needslist:
                            self.Needslist += [CustomerNeedsOptionsManager.LevelExoticFlavourTypesNeedsList[RandomNumber2]]
                            self.NeedsLabelsList += ["ExoticFlavourType"]
                            AmountOfCreatedNeeds += 1
                            NotDone = False
                if RandomNumber == 2:
                    NotDone = True
                    while NotDone == True:
                        RandomNumber2 = random.randint(0, len(
                            CustomerNeedsOptionsManager.LevelExoticFlavourTypesNeedsList))
                        if not CustomerNeedsOptionsManager.LevelExoticFlavourTypesNeedsList[RandomNumber2] in self.Needslist:
                            RandomNumber3 = random.randint(1, 200)
                            if RandomNumber3 > 100:
                                RandomNumber3 = (RandomNumber3 - 100) / 100
                            else:
                                RandomNumber3 = RandomNumber3 / -100
                        QualityLevel = Level.RegularNeedQualityLevel + Level.RegularNeedQualityLevel * RandomNumber3
                        Tuple = (QualityLevel,CustomerNeedsOptionsManager.LevelExoticFlavourTypesNeedsList[RandomNumber2])
                        self.Needslist += [Tuple]
                        self.NeedsLabelsList += ["ExoticFlavourTypeQualityCombination"]
                        AmountOfCreatedNeeds += 2
                        NotDone = False
            else:
                NotDone = True
                while NotDone == True:
                    RandomNumber2 = random.randint(0, len(CustomerNeedsOptionsManager.LevelExoticFlavourTypesNeedsList))
                    if not CustomerNeedsOptionsManager.LevelExoticFlavourTypesNeedsList[RandomNumber2] in self.Needslist:
                        self.Needslist += [CustomerNeedsOptionsManager.LevelExoticFlavourTypesNeedsList[RandomNumber2]]
                        self.NeedsLabelsList += ["ExoticFlavourType"]
                        AmountOfCreatedNeeds += 1
                        NotDone = False

        AmountOfOptions = 4
        while AmountOfCreatedNeeds < AmountOfNeeds:
            if AmountOfNeeds - AmountOfCreatedNeeds > 1:
                RandomNumber = random.randint(1, AmountOfOptions)
                if RandomNumber == 1:  # Flavour
                    NotDone = True
                    while NotDone == True:
                        RandomNumber2 = random.randint(0, len(CustomerNeedsOptionsManager.LevelFlavourTypesRegularNeedsList)-1)
                        if not CustomerNeedsOptionsManager.LevelFlavourTypesRegularNeedsList[
                                   RandomNumber2] in self.Needslist:
                            self.Needslist += [
                                CustomerNeedsOptionsManager.LevelFlavourTypesRegularNeedsList[RandomNumber2]]
                            self.NeedsLabelsList += ["FlavourType"]
                            AmountOfCreatedNeeds += 1
                            NotDone = False
                if RandomNumber == 2:
                    NotDone = True
                    while NotDone == True:
                        RandomNumber2 = random.randint(0, len(
                            CustomerNeedsOptionsManager.LevelFlavourTypesRegularNeedsList)-1)
                        if not CustomerNeedsOptionsManager.LevelFlavourTypesRegularNeedsList[
                                   RandomNumber2] in self.Needslist:
                            RandomNumber3 = random.randint(1, 200)
                            if RandomNumber3 > 100:
                                RandomNumber3 = (RandomNumber3 - 100) / 100
                            else:
                                RandomNumber3 = RandomNumber3 / -100
                            QualityLevel = Level.RegularNeedQualityLevel + Level.RegularNeedQualityLevel * RandomNumber3
                            TempTuple = (
                            QualityLevel, CustomerNeedsOptionsManager.LevelFlavourTypesRegularNeedsList[RandomNumber2])
                            self.Needslist += [TempTuple]
                            self.NeedsLabelsList += ["FlavourTypeQualityCombination"]
                            AmountOfCreatedNeeds += 2
                            NotDone = False

                if RandomNumber == 3 and not "THCContent" in self.NeedsLabelsList:
                    # THC
                    NotDone = True
                    while NotDone == True:
                        RandomNumber2 = RandomNumber3 = random.randint(1, 200)
                        if RandomNumber2 > 100:
                            RandomNumber2 = (RandomNumber2 - 100) / 100
                        else:
                            RandomNumber2 = RandomNumber2 / -100
                        THCLevel = Level.THCContentLevel * RandomNumber2
                        self.Needslist += [THCLevel]
                        self.NeedsLabelsList += ["THCContent"]
                        AmountOfCreatedNeeds += 1
                        NotDone = False
                if RandomNumber == 4 and not "CBDContent" in self.NeedsLabelsList:  # CBD
                    # THC
                    NotDone = True
                    while NotDone == True:
                        RandomNumber2 = RandomNumber3 = random.randint(1, 200)
                        if RandomNumber2 > 100:
                            RandomNumber2 = (RandomNumber2 - 100) / 100
                        else:
                            RandomNumber2 = RandomNumber2 / -100
                        CBDLevel = Level.CBDContentLevel * RandomNumber2
                        self.Needslist += [CBDLevel]
                        self.NeedsLabelsList += ["CBDContent"]
                        AmountOfCreatedNeeds += 1
                        NotDone = False

            elif AmountOfNeeds - AmountOfCreatedNeeds == 1:  # flavour, flavour quality combination (counts as 2), THC, CBD, Totalquality
                RandomNumber = random.randint(1, AmountOfOptions - 1)
                if RandomNumber == 1:  # Flavour
                    NotDone = True
                    while NotDone == True:
                        RandomNumber2 = random.randint(0, len(CustomerNeedsOptionsManager.LevelFlavourTypesRegularNeedsList)-1)
                        if not CustomerNeedsOptionsManager.LevelFlavourTypesRegularNeedsList[
                                   RandomNumber2] in self.Needslist:
                            self.Needslist += [
                                CustomerNeedsOptionsManager.LevelFlavourTypesRegularNeedsList[RandomNumber2]]
                            self.NeedsLabelsList += ["FlavourType"]
                            AmountOfCreatedNeeds += 1
                            NotDone = False

                if RandomNumber == 2 and not "THCContent" in self.NeedsLabelsList:
                    # THC
                    NotDone = True
                    while NotDone == True:
                        RandomNumber2 = RandomNumber3 = random.randint(1, 200)
                        if RandomNumber2 > 100:
                            RandomNumber2 = (RandomNumber2 - 100) / 100
                        else:
                            RandomNumber2 = RandomNumber2 / -100
                        THCLevel = Level.THCContentLevel * RandomNumber2
                        self.Needslist += [THCLevel]
                        self.NeedsLabelsList += ["THCContent"]
                        AmountOfCreatedNeeds += 1
                        NotDone = False
                if RandomNumber == 3 and not "CBDContent" in self.NeedsLabelsList:  # CBD
                    # THC
                    NotDone = True
                    while NotDone == True:
                        RandomNumber2 = RandomNumber3 = random.randint(1, 200)
                        if RandomNumber2 > 100:
                            RandomNumber2 = (RandomNumber2 - 100) / 100
                        else:
                            RandomNumber2 = RandomNumber2 / -100
                        CBDLevel = Level.CBDContentLevel * RandomNumber2
                        self.Needslist += [CBDLevel]
                        self.NeedsLabelsList += ["CBDContent"]
                        AmountOfCreatedNeeds += 1
                        NotDone = False


    def setRegularNeeds(self,Level,AmountOfNeeds,IngameTimer = None, DifficultyScore = None):
       AmountOfCreatedNeeds = 0
       AmountOfOptions = 4
       while AmountOfCreatedNeeds < AmountOfNeeds:
           if AmountOfNeeds-AmountOfCreatedNeeds >1:
                RandomNumber = random.randint(1,AmountOfOptions)
                if RandomNumber == 1: # Flavour
                    NotDone = True
                    while NotDone == True:
                        RandomNumber2 = random.randint(0,len(CustomerNeedsOptionsManager.LevelFlavourTypesRegularNeedsList)-1)
                        if not CustomerNeedsOptionsManager.LevelFlavourTypesRegularNeedsList[RandomNumber2] in self.Needslist:
                            self.Needslist += [CustomerNeedsOptionsManager.LevelFlavourTypesRegularNeedsList[RandomNumber2]]
                            self.NeedsLabelsList += ["FlavourType"]
                            AmountOfCreatedNeeds +=1
                            NotDone=False
                if RandomNumber == 2:
                    NotDone = True
                    while NotDone == True:
                        RandomNumber2 = random.randint(0, len(CustomerNeedsOptionsManager.LevelFlavourTypesRegularNeedsList)-1)
                        if not CustomerNeedsOptionsManager.LevelFlavourTypesRegularNeedsList[RandomNumber2] in self.Needslist:
                            RandomNumber3 = random.randint(1,200)
                            if RandomNumber3 >100:
                                RandomNumber3 = (RandomNumber3-100) /100
                            else:
                                RandomNumber3 = RandomNumber3/-100
                            QualityLevel = Level.RegularNeedQualityLevel+Level.RegularNeedQualityLevel*RandomNumber3
                            TempTuple = (QualityLevel,CustomerNeedsOptionsManager.LevelFlavourTypesRegularNeedsList[RandomNumber2])
                            self.Needslist += [TempTuple]
                            self.NeedsLabelsList += ["FlavourTypeQualityCombination"]
                            AmountOfCreatedNeeds += 2
                            NotDone = False

                if RandomNumber == 3 and not "THCContent" in self.NeedsLabelsList:
                    # THC
                    NotDone = True
                    while NotDone == True:
                        RandomNumber2 = RandomNumber3 = random.randint(1,200)
                        if RandomNumber2 > 100:
                            RandomNumber2 = (RandomNumber2 - 100) / 100
                        else:
                            RandomNumber2 = RandomNumber2 / -100
                        THCLevel = Level.THCContentLevel+Level.THCContentLevel * RandomNumber2
                        self.Needslist += [THCLevel]
                        self.NeedsLabelsList += ["THCContent"]
                        AmountOfCreatedNeeds += 1
                if RandomNumber == 4 and not "CBDContent" in self.NeedsLabelsList:  # CBD
                    # THC
                    NotDone = True
                    while NotDone == True:
                        RandomNumber2 = RandomNumber3 = random.randint(1, 200)
                        if RandomNumber2 > 100:
                            RandomNumber2 = (RandomNumber2 - 100) / 100
                        else:
                            RandomNumber2 = RandomNumber2 / -100
                        CBDLevel = Level.CBDContentLevel+Level.CBDContentLevel * RandomNumber2
                        self.Needslist += [CBDLevel]
                        self.NeedsLabelsList += ["CBDContent"]
                        AmountOfCreatedNeeds += 1
                        NotDone = False
                    
           elif AmountOfNeeds-AmountOfCreatedNeeds == 1:      # flavour, flavour quality combination (counts as 2), THC, CBD, Totalquality
               RandomNumber = random.randint(1, AmountOfOptions-1)
               if RandomNumber == 1:  # Flavour
                   NotDone = True
                   while NotDone == True:
                       RandomNumber2 = random.randint(0, len(
                           CustomerNeedsOptionsManager.LevelFlavourTypesRegularNeedsList)-1)
                       if not CustomerNeedsOptionsManager.LevelFlavourTypesRegularNeedsList[
                                  RandomNumber2] in self.Needslist:
                           self.Needslist += [
                               CustomerNeedsOptionsManager.LevelFlavourTypesRegularNeedsList[RandomNumber2]]
                           self.NeedsLabelsList += ["FlavourType"]
                           AmountOfCreatedNeeds += 1
                           NotDone = False

               if RandomNumber == 2 and not "THCContent" in self.NeedsLabelsList:
                   # THC
                   NotDone = True
                   while NotDone == True:
                       RandomNumber2 = RandomNumber3 = random.randint(1, 200)
                       if RandomNumber2 > 100:
                           RandomNumber2 = (RandomNumber2 - 100) / 100
                       else:
                           RandomNumber2 = RandomNumber2 / -100
                       THCLevel = Level.THCContentLevel+Level.THCContentLevel * RandomNumber2
                       self.Needslist += [THCLevel]
                       self.NeedsLabelsList += ["THCContent"]
                       AmountOfCreatedNeeds += 1
                       NotDone =False
               if RandomNumber == 3 and not "CBDContent" in self.NeedsLabelsList:  # CBD
                   # THC
                   NotDone = True
                   while NotDone == True:
                       RandomNumber2 = RandomNumber3 = random.randint(1, 200)
                       if RandomNumber2 > 100:
                           RandomNumber2 = (RandomNumber2 - 100) / 100
                       else:
                           RandomNumber2 = RandomNumber2 / -100
                       CBDLevel = Level.CBDContentLevel+Level.CBDContentLevel * RandomNumber2
                       self.Needslist += [CBDLevel]
                       self.NeedsLabelsList += ["CBDContent"]
                       AmountOfCreatedNeeds += 1
                       NotDone=False
                
       
        



class CustomerNeedsOptionsManager():
    RegularNeedsProfilesList = []
    ExoticNeedsProfilesList =[]
    LevelFlavourTypesRegularNeedsList = []
    LevelExoticFlavourTypesNeedsList = []
    ListOfRegularNeedsIterators = []
    ListOfExoticNeedsIterators = []
    
