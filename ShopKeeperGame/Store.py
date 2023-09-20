import pygame
import CharacterMovement #import  #this contains the full Customer class including images, requires LoadFilesList to be in the same folder
Width, Heigth = 1800,1200
class Store():#probably also another module, store probably does not need to be an object
	StoreRect = (int((Width/2)-50), int((Heigth/2)-50), 550,400)
	def DrawStore(Window):
		pygame.draw.rect(Window,(255,0,0),Store.StoreRect,1)

	def CustomerInStore():
		for customer in CharacterMovement.CustomerManager.CustomerList: #have to make this into customer list later, because we don't want our player character to be buying things
			if customer.rect.colliderect(Store.StoreRect):
				customer.CustomerInStore = True
				#customer.WaypointprogramNumber = 2 #set to wander in store

	def CustomerIsBuying(customer):
		if customer.CustomerInStore == True: #and is not leaving
			#add timer for when customer stops staying in store, then set a new waypoint program, similar to when a customer has bought something
			return #show then display customers name and needs on screen, and a button to select transaction and the weed you will sell.


class Offers():
	def __init__(self,FinishedComponent):
		self.amount = 0
		self.price = 0

	def SellToCustomer(SellOffer,CustomerBuyOffer):
		return
		#check if offer amount is equal or bigger than buyoffer quantity, otherwise sell max amount and get the option to sell the customer other weed
		#add money to Character
		# set CustomerBuyOffer to complete, which should set the Customer on his way and then removes the buy offer
		#remove selloffer amount from offerslist




class SellableWeed(Offers):
	def __init__(self,FinishedComponent,AmountToCut):
		Offers.__init__(self,FinishedComponent)
		self.amount = AmountToCut
		self.flavourTypes = FinishedComponent.flavourTypes
		self.flavourQualities = FinishedComponent.flavourQualities
		self.tHCContent = FinishedComponent.tHCContent
		self.cBDContent = FinishedComponent.cBDContent
		self.SativaPercentage = FinishedComponent.SativaPercentage
		self.IndicaPercentage = FinishedComponent.IndicaPercentage
		SellableWeed.setPricePerGram(self)

		ItemsForSale += [self]

	def setPricePerGram(self):
		TotalFlavourQualityScore = 0
		for iterator in range(len(self.flavourQualities)):
			TotalFlavourQualityScore += self.flavourQualities[iterator]
		if self.SativaPercentage > self.IndicaPercentage:
			ContentModifier = self.SativaPercentage-0.1
		else:
			ContentModifier = self.IndicaPercentage - 0.1
		PricePerGram = TotalFlavourQualityScore *(1+2*(ContentModifier))
		self.price = PricePerGram
		# add in a lot more code to get better value, people look for specific things in their weed so the price should reflect specific flavour profiles


class StoreManager():

	ItemsForSale = []
