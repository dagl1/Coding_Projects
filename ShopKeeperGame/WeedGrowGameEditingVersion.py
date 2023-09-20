#import statements
import GameManager
import numpy
import pygame
import random
import os
import time
import LoadFilesList
from CharacterMovement import Customer #this contains the full Customer class including images, requires LoadFilesList to be in the same folder
from CharacterMovement import CustomerManager
import math
import Places
import Weeds
import Mouse
import Menu
import Store
import CustomerNeedsOptions
#Game ideas:

#1 Bulk sellers, selling in bulk is illegal, so when a police character is around youw ill get fined for every bulk sale, so you can set your policiy to never sell to them, or manage your map carefully for police
#2 happiness meter per customer dependent on what they want, grow weed and choose the right thing for the customer
#3 begin selling yourself, can hire employees later to do some of your work
#4 set default sales patterns for customers's needs based on your current plant repository, new needs will come later in the game, those new needs employees will ask you for on advice
#5 Bulk buyers, having more than x amount of grams is illegal, those that smell with green gas/smoke around them already have a lot of weed, selling to them might be risky as you can also get fined for that
#6 Buy weed growing equipment + analyzers of stats+clones+zaadjes
#7 Persistent rewards that go into next levels: [keep 1 (increasing) seed(s), based on money: keep items from moving, get persistent rewards from reputation]

#Import functions
clock = pygame.time.Clock()


#Pygame initialization
pygame.init()
Width, Heigth = 1800,1200
Window = pygame.display.set_mode([Width,Heigth])
pygame.font.init()
# SpriteSuperSet = LoadFilesList.LoadImages()



#Map
	#check if display is initiatlized 
	#pygame.display.get_init

#Background
Background_Unscaled = pygame.image.load("E:/Python/WeedGrow/backgrounddetailed3.png")
BG = pygame.transform.scale(Background_Unscaled,(Width, Heigth))


# # class Offer:


class Weed():


	def __init__(self,name,cost):
		self.name = name
		self.cost = cost

Level = CustomerNeedsOptions.Level1Settings()
print(dir(Level))

CustomerAmountTesting =20

def RemoveCustomersOutOfMap():
	for iterator in range(len(CustomerManager.CustomerList)):
		if CustomerManager.CustomerList[iterator].x <-10 or CustomerManager.CustomerList[iterator].x > Width+10 or CustomerManager.CustomerList[iterator].y <-10 or CustomerManager.CustomerList[iterator].y > Heigth+10:
			print("customer is out of bounds")
			return #do something
			#Customer.deleteCustomer(self)

def main():
	run = True
	FPS = 60
	Main_font = pygame.font.SysFont("arial",50)
	Money = 100

	def redraw_window():
		Window.blit(BG, (0,0))
		Money_label = Main_font.render(f"Money : {Money}", 1, (255,255,255))
		Window.blit(Money_label,(Width-Money_label.get_width()-10,10))
		Store.Store.DrawStore(Window)
		Store.Store.CustomerInStore()
		Weeds.Weeds.drawWeedImage(Window)
		Menu.Menu.DrawMenus(Window)
		Places.PlaceMenus.ShowAvailableMenus()
		for iterator in range(len(CustomerManager.CustomerList)):
			CustomerManager.CustomerList[iterator].draw(Window)

		for iterator in range(len(Places.PlaceManager.OpenGrowingPlaces)):
			Places.WeedPlaces.drawPlace(Places.PlaceManager.OpenGrowingPlaces[iterator],Window)

		for iterator in range(len(Places.PlaceManager.InUseGrowingPlaces)):
			Places.WeedPlaces.drawPlace(Places.PlaceManager.InUseGrowingPlaces[iterator], Window)
		pygame.display.update()


	def CreateCustomerStartTimesPerDay(Level):
		totaltime = 1600
		if GameManager.GameManager.IngameTimer % totaltime == 0:
			GameManager.GameManager.CustomerDayTimeSpawnIterator = 0
			totalcustomers = int(Level.AmountOfCustomersPerDay * Level.AmountOfCustomersPerDayScalingFactor)  # different for each level
			IntervalRatio = Level.DaytimeNightTimeIntervalRatio
			TotalTimePerDayTime = (totaltime/(IntervalRatio[0]+IntervalRatio[1]))*IntervalRatio[0]
			DayTimeSplices = 3 #morning afternoon, evening
			TimePerSlice=int(TotalTimePerDayTime/DayTimeSplices)
			CustomersPerSlice = []
			for iterator in range(len(Level.AmountsOfCustomerPerDaytimeSliceRatio)):
				CustomersPerSlice += [int(totalcustomers/sum(Level.AmountsOfCustomerPerDaytimeSliceRatio)*Level.AmountsOfCustomerPerDaytimeSliceRatio[iterator])]

			CurrentTime =GameManager.GameManager.IngameTimer

			CustomerStartTimeList = []
			CustomerStartTimeListTemp = []
			for iterator in range(len(CustomersPerSlice)):
				CustomerStartTimeList += [[]]
				for iterator2 in range(CustomersPerSlice[iterator]):
					CustomerStartTimeList[iterator] += [random.randint(CurrentTime+TimePerSlice*iterator,CurrentTime+TimePerSlice*(1+iterator))]
				CustomerStartTimeListTemp +=CustomerStartTimeList[iterator]

			CustomerStartTimeListTemp = numpy.sort(CustomerStartTimeListTemp)
			GameManager.GameManager.CustomerStartTimeList = CustomerStartTimeListTemp
			print(GameManager.GameManager.CustomerStartTimeList)
	def CreateNextCustomerInDay(Level):
		try:
			if GameManager.GameManager.IngameTimer == GameManager.GameManager.CustomerStartTimeList[GameManager.GameManager.CustomerDayTimeSpawnIterator]and GameManager.GameManager.CustomerDayTimeSpawnIterator !=Level.AmountOfCustomersPerDay:
				create_customer(Level)
				print("is this running")
				if GameManager.GameManager.CustomerDayTimeSpawnIterator<len(GameManager.GameManager.CustomerStartTimeList)-1:
					if GameManager.GameManager.CustomerStartTimeList[GameManager.GameManager.CustomerDayTimeSpawnIterator] == GameManager.GameManager.CustomerStartTimeList[GameManager.GameManager.CustomerDayTimeSpawnIterator+1]:
						GameManager.GameManager.CustomerDayTimeSpawnIterator += 2
					else:
						GameManager.GameManager.CustomerDayTimeSpawnIterator += 1
		except:
			pass
	def create_customer(Level):#
		Cust = Customer(GameManager.GameManager.IngameTimer)
		CustomerNeedsOptions.setCustomerNeeds(Cust,Level)
		print('created')
		return Cust


		# CustomerManager.CustomerList += [Customer]
		#create customer using f print so we can do : create 20 (and fprint will enable us to call them customer1 customer2 etc) But maybe this should be outside of this code and be another thing 
																									#that calls create customer to do it x amount of times


	# for i in range(CustomerAmountTesting):
	# 	Customer1= create_customer()
	AmnesiaSeed1 = Weeds.Amnesia_HazeSeed()
	WhiteWidowSeed1 = Weeds.WhiteWidowSeed()
	# WhiteWidowSeed2 = Weeds.WhiteWidowSeed()
	# Amnesia1 = Weeds.Seeds.plantSeed(AmnesiaSeed1)

	Place1 = Places.WeedPlaces(200,200)
	Place2 = Places.WeedPlaces(400,200)
	Place3 = Places.WeedPlaces(200,400)
	Place4 = Places.WeedPlaces(400,400)
	Place5 = Places.WeedPlaces(800,200)
	Weeds.Seeds.putPlantInSelectedPlace(AmnesiaSeed1,Place1,GameManager.GameManager.IngameTimer)
	InventoryMenu = Menu.InventoryMenu()
	while run:
		clock.tick(FPS)
		CustomerNeedsOptions.createCustomerNeedProfilesLevelStart(Level)
		CreateCustomerStartTimesPerDay(Level)
		CreateNextCustomerInDay(Level)
		GameManager.GameManager.IngameTimer += 1 # the actual time is 60 ingameTimer ticks for 1 second
		#check if window is initialized, otherwise intialize window, should help with accidently closing the window without closing the program
		Customer.customerMovement(GameManager.GameManager.IngameTimer)
		RemoveCustomersOutOfMap()
		Weeds.GrowingWeeds.GrowingCycle(GameManager.GameManager.IngameTimer)
		redraw_window()

		# if GameManager.GameManager.IngameTimer  <1000 and GameManager.GameManager.IngameTimer%10 ==1:
		# 	create_customer()




		for event in pygame.event.get():
			pos = pygame.mouse.get_pos()
			Mouse.MouseCheckerAndAction(event,pos)

			if event.type == pygame.QUIT:
				run = False
				pygame.display.quit()
				pygame.quit()
			elif event.type == pygame.KEYDOWN:
				if event.key ==pygame.K_ESCAPE:
					pygame.quit()
					return

if __name__ == '__main__':
	main()
