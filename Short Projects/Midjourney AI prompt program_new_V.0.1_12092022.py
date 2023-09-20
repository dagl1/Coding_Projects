import random
import math

class Prompt():
	Colours = [["golden", "blue","green","orange", "red"]]
	vowelList = [ "a", "o", "u", "i", "e"]

	MainSubjectPluralityDecisionList = ("a  the  a group of  many  some  few  a crowd of  two  three").split("  ")
	MainSubjectPluralityDecisionSingularList=("a  the").split("  ")
	MainSubjectPluralityDecisionPercentagesList = [10,5,2,2,2,2,2,2,2] #[10,5,5]
	MainSubjectPluralityDownstreamEffectsList = [["the"],["a","two", "three"],["many","few","a group of"],["a crowd"]]#\
	MainSubjectAdjectivesWithChoice = [["beatiful", "ugly"],"evil", "devil", "sneaky","steal","victim","dark", "seduce","seductive","wretched", "cruel", "arrogant", "servant", "cultist","important","powerful", "rich", "elegant", "scary", "charismatic", "arrogant", "employees", "servants", "bodyguards", "noble", "henchmen", "red","blood", ["elegant", "bulky"],["scary","cute"],["poor", "rich"], ["important","unimportant"]]+ Colours
	MainSubjectAdjectivesNoChoice = ["powerful", "lazy"]
	MainSubjectAdjectivesList  =  MainSubjectAdjectivesNoChoice + MainSubjectAdjectivesWithChoice 
	MainSubjectAdjectivesCombinationsEffectsList = [[["lazy","unimportant","poor","scary"],["important","powerful", "rich", "elegant", "scary"]],[["beatiful","elegant","cute"],["ugly","bulky","scary"]]]
	MainSubjectAdjectivesPercentagesList = [1,1,1,1,1,1,1,1]
	MainSubjectGenderList = ["","male", "female"]
	MainSubjectGenderPercentagesList = [3,1,1]
	MainSubjectOccupationList = ["man", "woman","Doctor", "Druid", "Mercenary", "Enteratiner", "glass" ]


	ItemSingularCatagoryList = []
	ItemPluralCatagoryList = ["coin","chair"]

	FighterCatagoryList = ["brave","strong","fast", "sword", "bodyguard", "blood"]
	BeatifulCatagoryList = ["beatiful","elegant","cute"]
	UglyCatagoryList = ["ugly","bulky","scary"]
	RichCatagoryList = ["important","powerful", "rich", "elegant", "scary", "charismatic", "arrogant", "employees", "servants", "bodyguards", "noble"]
	PoorCatagoryList = ["lazy","unimportant","poor","scary", "peasant", "urchin", "slums", "alley", "homeless", "starving", "starve"]
	GoodCatagoryList = ["good", "angel", "light","save", "help","rescue","hero","folkhero", "caring", "care", "priest"]
	EvilCatagoryList = ["evil", "devil", "sneaky","steal","victim","dark", "seduce","seductive","wretched", "cruel", "arrogant", "servant", "cultist", "henchmen", "red","blood"]
	SingularCatagoryList = ["baron", "lord", "king", "queen", "dragon", "leader"] + ItemSingularCatagoryList
	CatagoryForWordCombinationList = UglyCatagoryList + BeatifulCatagoryList + PoorCatagoryList + RichCatagoryList + FighterCatagoryList+ GoodCatagoryList + EvilCatagoryList

	def __init__(self, chanceToAddAdjective, chanceToAddOccupation):	
		self.chanceToAddAdjective = chanceToAddAdjective
		self.chanceToAddOccupation = chanceToAddOccupation

		self.MainSubjectAdjectivesChanceReductionPerIteration = 0.3
		
		self.prompt = str()
		self.plural = "NA"
		self.MainSubjectGender = "NA"
		self.MainSubjectPossesive = "NA"
		self.adjustPercentagesToAmountOfOptionsInList()


		self.decideMainMainSubjectPlurality()
		self.decideOnGender()
		self.decideOnMainSubjectAdjectivesAndAddGender()
		
		self.decideOnOccupation()

		self.decideIfMainSubjectBecomesPossesiveNounOfMainSubject()
		self.actOnPossesiveNounOrNot()

		self.decideIfMainSubjectHasPossesion() 
		self.adjustAToAn()

		
	def adjustPercentagesToAmountOfOptionsInList(self):
		
		defaultWeightValue = 1

		for i, Percentages in enumerate(dir(self)):
			# print(Objects)
			if "Percentages" in Percentages:
				for j, Options in enumerate(dir(Prompt)):
					isNotChecked = 0
					while isNotChecked < 3:
						if Percentages.replace("Percentages",'') == Options:
							if len(getattr(Prompt, Percentages))>len(getattr(Prompt, Options)):
								getattr(Prompt, Percentages).pop()
								print("Hey there, there was a problem in the amount of values in %s.\nRemoved the last weight/percentage value."%Percentages)
								
							elif len(getattr(Prompt, Options))>len(getattr(Prompt, Percentages)):
								setattr(Prompt, Percentages,getattr(Prompt, Percentages) + [defaultWeightValue])
								print("Hey there, there was a problem in the amount of values in %s.\nAdded %s as the last weight/percentage value."%(Percentages,defaultWeightValue))
							else: 
								isNotChecked +=1
						else:
							isNotChecked =5

	def addToPrompt(self, inputForPrompt):
		if inputForPrompt != "" or inputForPrompt != " ":
			self.prompt += inputForPrompt + " "

	def adjustTextToPlural(self):
		pass

	def adjustTextToPossesiveForm(self, wordToChangeInPrompt):
		tempword = wordToChangeInPrompt

		if tempword.split()[len(tempword.split())-1] != "s":
			newWordForPrompt = wordToChangeInPrompt+"'s"
			tempPrompt = self.prompt.split()
			tempPrompt[len(tempPrompt)-1] = newWordForPrompt
			tempPrompt = ' '.join(tempPrompt)
			self.prompt = tempPrompt



	def findLastWord(self):
		tempPrompt = self.prompt
		tempPrompt = tempPrompt.split()
		return tempPrompt[len(tempPrompt)-1]

	def adjustAToAn(self):
		tempPrompt = self.prompt
		tempPrompt = tempPrompt.split()
		for i in range(len(tempPrompt)):

			if i>=1:

				if tempPrompt[i-1] == "a":

					letterListOfPromptInput = [*tempPrompt[i]]
					if len(letterListOfPromptInput)>0:

						if letterListOfPromptInput[0] in Prompt.vowelList:
							tempPrompt[i-1] = "an"
							tempPrompt = ' '.join(tempPrompt)
							self.prompt = tempPrompt

	def findChoiceCombinationCatagory(self,adjectivesAdded, listOfCombinationsThatRequireAdjustment):
		
		iAndjOfChoiceToReturn = []
		if adjectivesAdded:
			for h in range(len(adjectivesAdded)):
				iAndjOfChoice = []
				for i in range(len(listOfCombinationsThatRequireAdjustment)):
					if len(listOfCombinationsThatRequireAdjustment[i])>1:
						for j in range(len(listOfCombinationsThatRequireAdjustment[i])):
							if len(listOfCombinationsThatRequireAdjustment[i][j])>1:
								# print(adjectivesAdded[h],listOfCombinationsThatRequireAdjustment[i])
								if adjectivesAdded[h] in listOfCombinationsThatRequireAdjustment[i][j]:
									iAndjOfChoice += [(i,j)]
									# print(iAndjOfChoice)
				if iAndjOfChoiceToReturn:
					iAndjOfChoiceToReturn += [iAndjOfChoice]
				else:
					iAndjOfChoiceToReturn = [iAndjOfChoice]
				# print(iAndjOfChoiceToReturn)
			return iAndjOfChoiceToReturn
		else:
			return []
	def createListOfWordsWithDifferentChances(self,iAndjOfChoice):
		WordsThatHaveHigherChance = []
		WordsThatHaveLowerChance = []
			# ["lazy","unimportant","poor","scary"]
			# ["important","powerful", "rich", "elegant", "scary"]
			# ["ugly","bulky","scary"]
			# ["beatiful","elegant","cute"]
			# ["important","powerful", "rich", "elegant", "scary"]
			# ["beatiful","elegant","cute"]
			# ["ugly","bulky","scary"]
			# ["lazy","unimportant","poor","scary"]
		if iAndjOfChoice: 
			
			for i in range(len(self.MainSubjectAdjectivesCombinationsEffectsList)):
				for j in range(len(self.MainSubjectAdjectivesCombinationsEffectsList[i])):
					counter = True
					for k in iAndjOfChoice:
						for l in k:
							if l:
								(a,b) = l
								if (i,j) == (a,b):
									print([self.MainSubjectAdjectivesCombinationsEffectsList[a][b]])
									WordsThatHaveHigherChance += [self.MainSubjectAdjectivesCombinationsEffectsList[i][j]]
									counter = False
									
					if counter == True:
						WordsThatHaveLowerChance += [self.MainSubjectAdjectivesCombinationsEffectsList[i][j]]

		print(WordsThatHaveHigherChance)
		print(len(WordsThatHaveHigherChance))	
			# for i in iAndjOfChoice:
			# 	for j in i:
			# 		if j: 
			# 			(a,b) = j
			# 			for k in self.MainSubjectAdjectivesCombinationsEffectsList[a][b]:
			# 				if k in WordsThatHaveLowerChance:
			# 					WordsThatHaveLowerChance.remove(k)
			# 					WordsThatHaveHigherChance += [k]
		# 				moreInSameCatagory = True
		# 				counter = 0
		# 				while moreInSameCatagory ==True:
		# 					counter +=1
		# 					if index2+counter<len(i):
		# 						if i[index2+counter][0] ==a:
		# 							print('ye')
		# 						else:
		# 							moreInSameCatagory =False


						# if len(self.MainSubjectAdjectivesCombinationsEffectsList[a])>len(:
						# 	pass# print(self.MainSubjectAdjectivesCombinationsEffectsList[a])

					

	def adjustNextAdjectiveBasedOnCurrentAdjectives(self,adjectivesAdded):
		
		WordsThatHaveHigherChance = []
		WordsThatHaveLowerChance = []
		
		
		####Ajectives
		self.findChoiceCombinationCatagory(adjectivesAdded,self.MainSubjectAdjectivesCombinationsEffectsList)
		self.createListOfWordsWithDifferentChances(self.findChoiceCombinationCatagory(adjectivesAdded,self.MainSubjectAdjectivesCombinationsEffectsList))



		# self.findIfChoiceHadAlternative(choice,listOfChoicesUsedByRandomChoices)


		if self.MainSubjectGender == "male": 
			pass
		elif self.MainSubjectGender == "female":
			pass
		else: 
			pass
	def decideMainMainSubjectPlurality(self):
		choice1 = random.choices(Prompt.MainSubjectPluralityDecisionList,Prompt.MainSubjectPluralityDecisionPercentagesList)
		
		for i in range(len(Prompt.MainSubjectPluralityDownstreamEffectsList)):
			if choice1[0] in Prompt.MainSubjectPluralityDownstreamEffectsList[i]:
				self.PluralityDownstreamEffectCatagory = i

		if choice1[0] in Prompt.MainSubjectPluralityDecisionSingularList:
			self.plural = False
		else: 
			self.plural = True

		self.addToPrompt(choice1[0])

	def decideOnGender(self):
		choice1 = random.choices(Prompt.MainSubjectGenderList, Prompt.MainSubjectGenderPercentagesList)[0]
		self.MainSubjectGender = choice1

		

	def decideOnMainSubjectAdjectivesAndAddGender(self):		
		keepAddingAdjectives = True
		AdjectivesAddedTemp = []

		while keepAddingAdjectives == True:
			


			if random.random() <= self.chanceToAddAdjective:
				self.adjustNextAdjectiveBasedOnCurrentAdjectives(AdjectivesAddedTemp)

				tempMainSubjectAdjectivesPercentagesList = Prompt.MainSubjectAdjectivesPercentagesList
				
				choice1 = random.choices(Prompt.MainSubjectAdjectivesList, Prompt.MainSubjectAdjectivesPercentagesList)[0]


				for i in range(len(Prompt.MainSubjectAdjectivesList)):
					if choice1 in Prompt.MainSubjectAdjectivesList:
						Prompt.MainSubjectAdjectivesList.remove(choice1)
						Prompt.MainSubjectAdjectivesPercentagesList.remove(Prompt.MainSubjectAdjectivesPercentagesList[i])		
				if len(Prompt.MainSubjectAdjectivesList) < 1:
					keepAddingAdjectives = False
				elif type(choice1) == list:
					choice1 = random.choice(choice1)

				# self.adjustNextWordChancesByPreviousUsedWords(choice1,Prompt.MainSubjectAdjectivesList,Prompt.MainSubjectAdjectivesCombinationsEffectsList)	
				self.addToPrompt(choice1)
				AdjectivesAddedTemp += [choice1]	#this needs to become a choices, where the choice is based on the previously chosen adjectives, 
																#based on the catagories and some weight calculations
				self.chanceToAddAdjective -= self.MainSubjectAdjectivesChanceReductionPerIteration
			else:
				keepAddingAdjectives = False

		self.addToPrompt(self.MainSubjectGender)

	def decideOnOccupation(self):

		self.occupationStatus = False

	def decideIfMainSubjectHasPossesion(self):
		pass

	def decideIfMainSubjectBecomesPossesiveNounOfMainSubject(self):
		if self.occupationStatus == False:
			if self.plural == False: 
				if self.MainSubjectGender == "male" or self.MainSubjectGender == "female":
					self.adjustTextToPossesiveForm(self.MainSubjectGender)
					self.MainSubjectPossesive = True

	def actOnPossesiveNounOrNot(self):
		if self.MainSubjectPossesive == True:
			pass #start new sort of main subject thing but with emphasis on objects, no people as subject
		else:
			pass



FirstPrompt = Prompt( 1.3,0.8)

print(FirstPrompt.prompt)
print(FirstPrompt.plural)
# SecondPrompt = Prompt( 25,0.8)





Colours = [["golden", "blue","Green","orange", "red"]]

Scenarios = []
Places1 = ["elven", "dwarven", "orcish","Greek", "Roman", "Chinese", "Forgotten Coasts", "Arabic", "south american", "indian", "native american", "indiginous"]
Environments = ["desert", "woodlands", "lakeside", "mountains", "coastal", "steppes", "savanannah"]
Places2_1 = ["City", "village", "outpost", "ferry", "fortress", "hut", "tomb"]
Places2_2 = ["Near a river", "in a forest", "at the coast"]
Places3_1 = ["bakery", "weavershop", "fishing boats"]
PlacesModifiers = ["dusty","ancient"]



Happenings = [["Burning", "flames", "inferno"],["Demon invasion", "gate from hell"],["People having fun", "drinking","eating","laughing"],["Building things","construction"],["Training","physical excersion"]]
Times = ["at night","with full moon", " ", "during the day", " in the shade", "during a storm", "during a stormy night","during a blizzard", "in quiet times"]
Moods = [["dark","Eerie", "evil"],["peaceful","relaxed", "tranquil"],["excited","happy","great feelings"],["Scary"," dangerous", "immersive"],["exhausted", "lazy" ,"difficult"]]
FantasyPreset = ["Fantasy", "Medieval"]
PhysicalModifiers = [["beatiful figure", "wlop", "perfect fit body" ],["large eyes","cute face", "interesting aesthetic"],["Voluptous, seductive, charismatic"]]
DynamicModifiers =[["unreal engine", "cryengine", "unity engine"],["backlighting","sidelighting"],["Octane Render","Octane Render","Octane Render","deviantart", "pinterest","artstation","Pixiv"]]
ConstantModifiers = ["extremely detailed", "hyperrealistic", "octane render"]
OptionalModifiers = ["full body", "Epic", "Happy", "Relaxed", "great shot", "cinematic", "vibrant", "8k",
"pastel colours","lumen reflections","intricate", "atmospheric", "ambient", "raytracing", "wide shot",
"DOF", "Aesthetically pleasing", "lush", "tetrachromatic", "beatiful"]
Artists = ["Renato Mucillo", "andreas rocha", "Greg Rutkowski", "Asher brown Durand"]
Parameters = ["--ar 16:9 --quality 2.5 --stylize randomize!!!!!!! --chaos 50"]


# Bakery <in> Elven desert village near a river <where> a group of dwarven women drink at night 
# beatiful/strong/great/amazing/golden Elven female paladin wearing golden heavy plate <in> mountains at night, beatiful figure 
# [PeopleAdjective],[Race],[Gender],[occupation],[[ClothesAdjective],[clothes]]
# [PeopleAdjective],[Race],[Gender],[occupation],[[ClothesAdjective],[clothes]]					  [location],[when],[additional modifiers]
# [PeopleAdjective],[Race],[Gender],[occupation],[[ClothesAdjective],[clothes]],[action],[actionAdverb],[Location],[when],[additional modifiers]


def makeLocationPromt(): 
	Places1, Environments, Places2_1, Places2_2, Places3_1


People1 = ["a","a","a","a","a","a","a group of ", "a group of ","many","two","three"]
People2 = [" ", "Elven", "Dwarven", "Tiefling", "Orc","Halfling", "Gnome", "Human", "Giant", "Human"]
People3 = [" ", "woman", "man", "men", "women"]
PeopleModifiers = [["beatiful", "ugly"],"evil", "devil", "sneaky","steal","victim","dark", "seduce","seductive","wretched", "cruel", "arrogant", "servant", "cultist","important","powerful", "rich", "elegant", "scary", "charismatic", "arrogant", "employees", "servants", "bodyguards", "noble", "henchmen", "red","blood", ["elegant", "bulky"],["scary","cute"],["poor", "rich"], ["important","unimportant"]]+ Colours
Classes = ["druid","doctor","shaman","fighter","soldier","mercenary","guard","baker","craftsman","smith","teacher","cleaner", "peasant","entertainer","servant","bandit","maid","miner","farmer","fishermen","shopkeeper", "magic wand seller", "bathhouse attendant", "inspector", "evil henchmen", "killer"]
Clothing = ["heavy armour"]
ClothingModifiers = ["glimmering", "rusty", "beatiful", "golden "]

# a group of dark lords fighting inside the castle against the soldiers

def makeCharacterPrompt(n, chanceToAddAdjective, chanceToAddOccupation):
	for i in range(n):
		ClassesToUse = Classes
		People3ToUse = People3
		#[PeopleAdjective],[Race],[Gender],[occupation],[[ClothesAdjective],[clothes]]
		prompt = "" 
		choice2 = random.choice(People1)
		prompt = choice2
		################################ Adjectives
		listOfNumbers = range(random.randint(1,len(PeopleModifiers)))
		listOfNumbersToUse = list(listOfNumbers)
		keepAddingAdjectives = True
		while keepAddingAdjectives == True:
			if random.random() <= chanceToAddAdjective:
				choice1 = random.choice(listOfNumbersToUse)
				listOfNumbersToUse.remove(choice1)
				if len(listOfNumbersToUse) < 1:
					keepAddingAdjectives = False
				if type(PeopleModifiers[choice1]) != list:
					prompt += " "+ PeopleModifiers[choice1]
				elif type(PeopleModifiers[choice1]) == list:
					prompt += " "+ random.choice(PeopleModifiers[choice1])

			else:
				keepAddingAdjectives = False
		#################################
		
		#################################
		if choice2 != "a":
			People3ToUse = [" ","men", "women"]
			ClassesToUse = [occupation +"s" for occupation in Classes]
		else:
			People3ToUse = [" ", "woman", "man"]
			ClassesToUse = Classes
		#################################


		if random.random() <= chanceToAddOccupation:
			choice3 = random.choice(ClassesToUse)
			People3ToUse = [" ", "female", "male", "mixed"]
			prompt += " " + random.choice(People2) +" "+ random.choice(People3ToUse) + " " + choice3
		else: 
			try: 
				People3ToUse.remove(" ")
			except:
				pass
			prompt += " " + random.choice(People2) +" "+ random.choice(People3ToUse)
		choice = random.randint(1,3)
		if choice == 1:
			#[PeopleAdjective],[Race],[Gender],[occupation],[[ClothesAdjective],[clothes]]
			pass
		elif choice ==2:
			pass
			#[PeopleAdjective],[Race],[Gender],[occupation],[[ClothesAdjective],[clothes]]					  [location],[when],[additional modifiers]
		else:
			pass
			#[PeopleAdjective],[Race],[Gender],[occupation],[[ClothesAdjective],[clothes]],[action],[actionAdverb],[Location],[when],[additional modifiers]


		prompt = "/imagine prompt:" +  " " + prompt + " " + ",Fantasy, "+ "Medieval, " + "DnD, "+ "extremely detailed, "+ "hyperrealistic, "+ "octane render "+" --ar 16:9 --quality 2.5 --stylize 3100 --chaos 50"
		print(prompt)
	#People1, People2, People3, Classes, Clothing + "in" + Location + 
makeCharacterPrompt(100, 0.5, 0.8)
def makeScenarioPrompt():
	pass

# Places 
# Thingshappening + Charachters
# ThingsHappening + Places
# ThingsHappening + Characters + places
# CurrentModifiers = []





# ModifierPresetHappy = []





# class sentenceClass():


# 	SetFirstWord