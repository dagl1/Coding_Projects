import os 
import glob
import pygame
import random



def LoadSpriteImages():
	Filelist = []
	Filelistloaded = []
	Currentpath =os.path.dirname(os.path.abspath(__file__))
	CharactersPath = str(Currentpath)+'/WeedGrow/Characters'

	Counter = 0
	for iterator in glob.iglob(str(CharactersPath)+"/"+'**/*.png', recursive= True):
		Counter += 1




	SpriteSuperSet = []
	Counter2=0
	Counter3=0
	for CharacterMaps in os.listdir(CharactersPath):
		CharacterAnimationPath= CharactersPath+"/"+str(CharacterMaps)
		SpriteSuperSet+= [[]]
		Counter3=0
		for AnimationSets in os.listdir(CharacterAnimationPath):
			CurrentAnimationSet = CharacterAnimationPath+"/"+str(AnimationSets)
			SpriteSuperSet[Counter2]+= [[]]
			for filename in glob.iglob(CurrentAnimationSet + '**/*.png', recursive=True): #to get all .png files in a specific folder (add / to look inside the folders it contains)
				x = pygame.transform.scale(pygame.image.load(filename),(40,80))
				SpriteSuperSet[Counter2][Counter3] += [x]
				#SpriteSuperSet =
			Counter3+=1
		Counter2+=1
	return(SpriteSuperSet)

def loadWeedImages():
	WeedImageList =[]
	Currentpath = os.path.dirname(os.path.abspath(__file__))
	CharactersPath = str(Currentpath) + '/WeedGrow/Weeds'
	for filename in glob.iglob(CharactersPath + '**/*.png', recursive=True):
		x = pygame.transform.scale(pygame.image.load(filename), (40, 40))
		WeedImageList += [[x]]
		LengthImageList = len(WeedImageList)
		for iterator2 in range(8):
			scale =iterator2+2
			x = pygame.transform.scale(pygame.image.load(filename), (40*scale, 40*scale))
			WeedImageList[LengthImageList-1] += [x]

	return WeedImageList
# SpriteDictionary = {}
# for iterator in range(len(Filelistloaded)):
# 	SpriteDictionary[Filelist[iterator]] = Filelistloaded[iterator]

#print(SpriteDictionary[1])

#Customer.SpriteSuperSet = [[[1Idle],[Sprite1RightWalk1, Sprite1RightWalk2, Sprite1RightWalk3],[Sprite1LeftWalk1, Sprite1LeftWalk2, Sprite1LeftWalk3]], [[Sprite2RightWalk1,Sprite2RightWalk2,Sprite2RightWalk3],[Sprite2LeftWalk1, Sprite2LeftWalk2, Sprite2LeftWalk3]]]
# for i in range (10): # for iterator in range(len(mapje))

# 	fprint(f

