import pygame
import random
import math
import LoadFilesList
from Weeds import Weeds
SpriteSuperSet = LoadFilesList.LoadSpriteImages()
class Character(pygame.sprite.Sprite):

	def __init__(self, x, y):
		pygame.sprite.Sprite.__init__(self)
		self.x = x #movement should change these values of the object
		self.y = y 

	def draw(self, window):
		window.blit(self.image,(math.ceil(self.x), math.ceil(self.y)))

class CustomerManager():
	CustomerList = []
	WaypointList1=[]

class Customer(Character): 
	SpriteSuperSet = SpriteSuperSet
	# temp1 = (int((Width/2)-50), int((Heigth/2)-50))
	# temp2 = (int((Width/2)-10), int((Heigth/2)+50))
	# temp3 = (int((Width/2)+20), int((Heigth/2)-40))
	WaypointprogramList =[[(67, 68), (114, 157), (195, 273), (294, 383), (387, 499), (525, 603), (635, 606), (742, 610), (793, 653), (859, 697), (962, 711), (1016, 682), (1020, 602), (959, 601)],[(650, 108), (654, 230), (766, 328), (943, 350), (1095, 411), (1152, 548), (1046, 611), (960, 607)],[(1466, 1085), (1557, 1001), (1565, 784), (1235, 683), (1004, 609)],[(765, 1123), (823, 997), (861, 832), (1007, 730), (1071, 596), (957, 604)],[(26, 883), (88, 862), (183, 824), (378, 769), (713, 801), (952, 798), (999, 603), (948, 601)],[(1583, 131), (1529, 181), (1472, 226), (1398, 283), (1240, 384), (1107, 477), (1071, 552), (1050, 601), (982, 605), (950, 602)],[(864, 569), (865, 636), (934, 573), (932, 629), (870, 567), (938, 567)]]
	CustomerList =[]


	def __init__(self,IngameTimer):
		Customer.choose_startLocation(self)
		Character.__init__(self,self.x,self.y)
		Customer.Decide_spriteSet(self)
		Customer.produce_name(self)
		self.speed = 1.7
		self.chanceToBuy = 1
		self.rect = pygame.Rect(self.x,self.y,20,20)
		self.starttime = IngameTimer
		self.Imagenumber =0
		self.TargetReached = False
		self.WaypointNumber =0
		self.target =Customer.WaypointprogramList[self.WaypointprogramNumber][0]
		self.StopTime = 0
		self.StopDuration = 0
		CustomerManager.CustomerList += [self]
		Customer.setMovementVector(self)
	
	def produce_name(self):
		Last_name_list= ["Rogers","Banner","May","Coulson","Mackenzy","Andrews","Whitehall"]
		First_name_list= ["Phil","Mellinda","Buck","Steve","Bruce","Reinhart","Jack"]
		First_name = First_name_list[random.randrange(0,len(First_name_list))]
		Last_name = Last_name_list[random.randrange(0,len(Last_name_list))]
		self.name = First_name + " " + Last_name

	def setRandomStartPosition(self):
		Width, Height = 1800,1200 # these should be set to width and Height, currently hardcoded because i don't want to add in 2 extra inputs, but might make them optional

		Done = False
		while Done == False:
			RandomNumber = random.randrange(0, (Width * 2 + Height * 2))
			if RandomNumber < Width:
				self.x, self.y = RandomNumber, 0
			elif RandomNumber >= Width and RandomNumber<Width+Height:
				self.x, self.y = 0, RandomNumber-Width
			elif RandomNumber >= Width+Height and RandomNumber<(2*Width)+Height:
				self.x, self.y = RandomNumber - Width-Height, Height
			else:
				self.x, self.y = Width, RandomNumber-(2*Width)-Height
			Done = True
			for iterator in range(len(CustomerManager.CustomerList)):
				distanceX = abs(CustomerManager.CustomerList[iterator].x-self.x)
				distanceY = abs(CustomerManager.CustomerList[iterator].y-self.y)
				distance = math.sqrt((distanceX*distanceX) + (distanceY * distanceY))
				if distance < 30:
					Done = False

	def decideWaypointProgram(self):
		distance1 = 100000
		WaypointIterator = 0
		for iterator in range(len(Customer.WaypointprogramList)):

			distanceX = abs(Customer.WaypointprogramList[iterator][0][0] - self.x)
			distanceY = abs(Customer.WaypointprogramList[iterator][0][1] - self.y)
			distance2 = math.sqrt((distanceX * distanceX) + (distanceY * distanceY))
			if distance2 <distance1 and iterator <len(Customer.WaypointprogramList)-1:
				distance1 = distance2
				WaypointIterator = iterator

		self.WaypointprogramNumber = WaypointIterator
		return

	def choose_startLocation(self):
		#self.WaypointprogramNumber =1 #for testing
		self.WaypointprogramNumber = random.randrange(0,1)

		if self.WaypointprogramNumber == 0:
			self.x = 400
			self.y = 400
		if self.WaypointprogramNumber == 1:
			self.x = 100
			self.y = 100
		Customer.setRandomStartPosition(self)
		Customer.decideWaypointProgram(self)


	def Decide_spriteSet(self):
		self.SpriteSetNumber = random.randrange(0,len(Customer.SpriteSuperSet))
		self.image = Customer.SpriteSuperSet[self.SpriteSetNumber][0][0]
      
	def deleteCustomer(self):
		return
	def draw_customer():
		Character.draw()

	def isTargetReached(self):
		x = self.x
		y = self.y
		targetX= self.target[0]
		targetY= self.target[1]

		if abs(x-targetX)<math.ceil(2.5*self.speed) and abs(y-targetY)<math.ceil(1.5*self.speed):
			return True
		else:
			return False

	def teleportToTarget(self):
		self.x = self.target[0]
		self.y = self.target[1]

	def idleAtPosition(self):
		self.SpriteDirectionNumber = 0
		AnimationSpriteList = Customer.SpriteSuperSet[self.SpriteSetNumber][self.SpriteDirectionNumber]
		self.image = AnimationSpriteList[0]

	def checkIfCustomerDone(self):
		return False

	def findNewTarget(self): #in this function we can later add a check to see if the customer is in store, in which case it triggers ssomething else

		LengthWaypointList = Customer.WaypointprogramList[self.WaypointprogramNumber]
		if  self.WaypointNumber < len(LengthWaypointList)-1 and self.WaypointprogramNumber < len(Customer.WaypointprogramList)-1:
			self.WaypointNumber +=1
		elif self.WaypointprogramNumber < len(Customer.WaypointprogramList)-1: #check if we aren't in store
			self.WaypointprogramNumber =len(Customer.WaypointprogramList)-1
			self.WaypointNumber = 0
		else:
			if Customer.checkIfCustomerDone(self) == False:
				self.WaypointNumber += 1
				self.WaypointNumber = self.WaypointNumber%len(LengthWaypointList)
		self.target = Customer.WaypointprogramList[self.WaypointprogramNumber][self.WaypointNumber]

	def modifyParametersBasedOnWaypointProgram(self): #here we can place code that will modify things based on if they enter a specific waypoint program.
		return

	def setMovementVector(self):
		x = self.x
		y = self.y
		targetX= self.target[0]
		targetY= self.target[1]
		xDistance = targetX - x
		yDistance = targetY - y

		if abs(xDistance)>= abs(yDistance):
			yVector = yDistance/abs(xDistance)
			if xDistance>0:
				xVector = 1
			elif xDistance<0:
				xVector=-1
		else:
			xVector = xDistance/abs(yDistance)
			if yDistance>0:
				yVector = 1
			elif yDistance<0:
				yVector = -1
		self.vector = (xVector,yVector)

	def addRandomnessToVector(self,IngameTimer):
		RandomNumber = random.randint(0,1)
		if RandomNumber ==0:
			self.vector = (self.vector[0]+0.5*float(Weeds.binomialDistribution()), self.vector[1])
		elif RandomNumber == 1:
			self.vector = (self.vector[0],self.vector[1]+0.5*float(Weeds.binomialDistribution()))


	def setMovementDirection(self):
		if self.vector[0]>0:
			self.SpriteDirectionNumber = 1
		elif self.vector[0]<=0:
			self.SpriteDirectionNumber = 2

	def moveCustomer(self):
		self.x += self.vector[0]*self.speed
		self.y += self.vector[1]*self.speed

	def customerAnimation(self, IngameTimer):
		Time = IngameTimer - self.starttime
		AnimationSpriteList = Customer.SpriteSuperSet[self.SpriteSetNumber][self.SpriteDirectionNumber]
		TotalAnimationFrames = 30
		SpriteNumberInAnimation = len(AnimationSpriteList)
			
		if Time % (int(TotalAnimationFrames/SpriteNumberInAnimation)) == 1:
			self.Imagenumber +=1
			self.image = AnimationSpriteList[self.Imagenumber%SpriteNumberInAnimation]


	def customerMovement(IngameTimer):
		RandomnessDelayInterval = 60
		for Iterator in range(len(CustomerManager.CustomerList)):
			if Customer.isTargetReached(CustomerManager.CustomerList[Iterator]) ==True:
				if abs(IngameTimer) >= CustomerManager.CustomerList[Iterator].StopTime + CustomerManager.CustomerList[Iterator].StopDuration and CustomerManager.CustomerList[Iterator].StopTime != 0 :
					Customer.findNewTarget(CustomerManager.CustomerList[Iterator])
					Customer.modifyParametersBasedOnWaypointProgram(CustomerManager.CustomerList[Iterator])
					CustomerManager.CustomerList[Iterator].StopTime = 0
					CustomerManager.CustomerList[Iterator].StopDuration = CustomerManager.CustomerList[Iterator].StopDuration+ (CustomerManager.CustomerList[Iterator].StopDuration *float(Weeds.binomialDistribution()))
					Customer.setMovementVector(CustomerManager.CustomerList[Iterator])
					Customer.setMovementDirection(CustomerManager.CustomerList[Iterator])
					Customer.customerAnimation(CustomerManager.CustomerList[Iterator], IngameTimer)
				elif CustomerManager.CustomerList[Iterator].StopTime == 0:
					Customer.teleportToTarget(CustomerManager.CustomerList[Iterator]) # this may be potentially unnecessary
					if CustomerManager.CustomerList[Iterator].StopDuration != 0:
						Customer.idleAtPosition(CustomerManager.CustomerList[Iterator])
					CustomerManager.CustomerList[Iterator].StopTime = IngameTimer

			else:
				if (IngameTimer - CustomerManager.CustomerList[Iterator].starttime) %RandomnessDelayInterval == 1:
					Customer.setMovementVector(CustomerManager.CustomerList[Iterator])
					#Customer.addRandomnessToVector(CustomerManager.CustomerList[Iterator],IngameTimer)
					Customer.setMovementDirection(CustomerManager.CustomerList[Iterator])
				Customer.moveCustomer(CustomerManager.CustomerList[Iterator])
				Customer.customerAnimation(CustomerManager.CustomerList[Iterator],IngameTimer)


