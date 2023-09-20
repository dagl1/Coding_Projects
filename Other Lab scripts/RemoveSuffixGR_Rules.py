
#####################
import math
GR_Rules = ['','','', '1591.1','','( 4967.2 and 1738.1 and 8050.1 and 1743.1 ) or ( 4967.8 and 1738.5 and 8050.1 and 1743.8 )','89874.1','117247.1','221.1','','','4128.9 or 4129.1','39.1' ] #list mimicking model.GXRRULES 

RulesList2 = [] #create empty list to save pruned variables
String = "1591.1"


for iterator in range(len(GR_Rules)):
	SeperatedString = GR_Rules[iterator].split() #in matlab this is split(GR_Rules(iterator)," ") #or something similar to this.  https://www.mathworks.com/help/matlab/ref/split.html#:~:text=newStr%20%3D%20split(%20str%20)%20divides,array%2C%20then%20so%20is%20newStr%20.
	#SeperatedString is now a list of strings if there were spaces " " in the element

	if len(SeperatedString) > 1: #check if seperated by whitespace (' '), the list is longer than 1
		TempListString = [] #make a temporary list for only this iteration of the largest for loop (so each time iterator goes up, we make a new templist if the seperatredString is >1)
		for iterator2 in range(len(SeperatedString)): #cycle through the list of seperated strings

			try:  #we cannot make a float out of a "(" so that would give us an error, try is almost like an if sentence, where it will attempt to run the lines, but if they give an error go to except
				FloatString = float(SeperatedString[iterator2]) # make it into a float (this will raise an error if done for things like 'and', in which case we go to the except part at line 23
				FloatString = math.floor(FloatString) #floor() function in MATLAB 
				TempListString.append(FloatString) #append our rounded down float to templist

			except: #if an error is raised when attempting to convert string to float, this runs
				TempListString.append(SeperatedString[iterator2]) #append the value that couldn't be made into a float directly without editing



		for iterator2 in range(len(TempListString)): #this is outside of the previous for loop
			TempListString[iterator2] =str(TempListString[iterator2]) #we convert each variable in our templist to str again (as that was what it was like in the original GR_Rules)

		TempListString = " ".join(TempListString) #then we do the opposite of .split function, where we add each element in our list (TempListString) and put them together with spaces in between
		RulesList2.append(TempListString) #add string with removed suffices into the right position in, RulesList2



	else: # if there was no whitespace in GR_Rules[iterator], then we are only dealing with a single value
		try: #we attempt to make that value into a float (as it can either be '1591.1' or ''; the latter we can't make into a float, so we use try and except again)
			TempRule = float(GR_Rules[iterator]) #this will raise an error at '' and move to except
			TempRule = math.floor(TempRule) #we round down the value since it was a number
			RulesList2.append(str(TempRule)) #then append it directly into the new RulesList2

		except: #if an error is raised when attempting to convert string to float, this runs
			RulesList2.append((GR_Rules[iterator])) #then append the empty value directly (as the only possibility for this value should be '', as it is at the end of our if-else and at the end of try-except)

					
print(RulesList2) #this should now be exactly the same as GR_Rules, but with suffixes removed. Each individual ' surrounding values should remain, each '' should be the same and the larger and/or should contain () in the right positions
print(GR_Rules) #compare with RulesList2, if exactly the same but with suffixes, then this can run on the model.GR_Ruless thing
			
			