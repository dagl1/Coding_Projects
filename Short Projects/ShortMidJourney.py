import random

prompt1 = "A group of travelers on the road in the forest "

def addNothingToChoice(choiceList):
	tempList = choiceList[:]
	tempList += [""]
	return tempList


def addRandomOptionsToPrompt(n, prompt):


	
	timePeriodList = ["medieval","prehistoric", "iron age", "stone age","medieval","medieval","medieval"]
	fantasyList = ["fantasy"]
	timeOfDay = ["morning", "daytime", "evening", "nighttime", "full moon night"]
	engineList = ["unreal engine", "cryengine"]
	renderList = ["octane render"]
	realisticList = ["realistic","hyperrealistic"]
	detailedList = ["very detailed","detailed","extremely detailed","intricate"]
	volumetricLightingList = ["volumetric lighting",]
	cinematogrophyList = ["cinematic","wide shot", "close-up", "interesting angle"]
	coloursList = ["grim", "colourful", "grayish","shadows","vibrant"]
	flavourList = ["grimdark", "eerie", "scary", "beatiful", "happy", "relaxed", "epic", "music", "smoke", "shadows", "magic"]
	flavourList2 = ["ancient", "royal", "rich", "dominating", "great", "old", "new", "interesting"]
	civelazationList = ["","","","","babolyian", "persian", "roman", "chinese", "latin", "african", "","",""]
	qualityList = ["--q 2", "--q 3"]
	stylizeList = ["--stylize 1500", "--stylize 2500", "--stylize 3500", "--stylize 5000"]
	chaosList = ["--chaos 10","--chaos 20", "--chaos 30", "--chaos 40", "--chaos 50"] 

	totalList = [timePeriodList,fantasyList, timeOfDay, engineList,renderList,realisticList,detailedList,volumetricLightingList,cinematogrophyList,coloursList,flavourList,flavourList2,civelazationList,qualityList,stylizeList,chaosList]

	Number = 8
	InbetweenNumber = 4

	for index1 in range(n): 
		tempObject = prompt
		for i, v in enumerate(totalList):
			v1 = addNothingToChoice(v)
			if i>Number and i<InbetweenNumber:
				#print(v1)
				choice = random.sample(v1,2)
				choice = ", ".join(choice)
				
			else:
				choice = random.choice(v1)
			if choice == "":
				tempObject 
			elif i<(InbetweenNumber+1):
				tempObject += ", " + str(choice)
			else: 
				tempObject += " "+  str(choice)

		tempObject = "/imagine prompt:" +  tempObject
		print(tempObject)


addRandomOptionsToPrompt(10,prompt1)