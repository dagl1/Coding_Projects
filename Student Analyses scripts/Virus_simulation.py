import stdio # imports sys, re (contains write and read functions designed to reduce compiling errors)
import sys
import math
import random
import statistics
import stdarray
import stddraw
import stdaudio

SimListsInfected = []
SimListsNewlyInfected = []
SimListsImmune = []
ChangedVariable =[]
stddraw.setCanvasSize() #Set some lists to save data in later
for g in range(3):  #We run the simulation 3 times with different parameters, with g affecting the RadiusOfInfection

    a = 0 #We run a simulation at least 7 times before saving the results and checking if they are not too different from eachother
    while a < 6: #These simulations all have the exact same parameters
        Results = []

        PeopleNumber = 100 
        SizeEnvironment = 100
        ChanceOfInfectionPerTimestep = 0.002
        LengthOfInfection = 5
        RadiusOfInfection = 20 * (1+g)
        MoveLength = 10
        SimulationLength = LengthOfInfection*3
        People = stdarray.create2D(PeopleNumber,4,0) #         ]
        Infected = 2
        CanvasRefreshTime = 20 #most of these variables do not change and could be put outside this loop
        #this way however, allows for quick editing to allow g to affect other variables

        stddraw.setXscale(0, SizeEnvironment)
        stddraw.setYscale(0, SizeEnvironment)
        for i in range(len(People)): #We give each person a X and Y position
            People[i][0]=random.randrange(0,SizeEnvironment) #X position
            People[i][1]=random.randrange(0,SizeEnvironment) #Y position

        for i in range(Infected):
            People[i][2]=LengthOfInfection #set the first people (equal to Infected) to be infected

        CurrentlyInfected = Infected
        ImmuneCount = 0
        CurrentlyInfectedList = []
        ImmuneList = []
        NewlyInfectedList = []


        for i in range(SimulationLength):
            stddraw.setXscale(0,SizeEnvironment)
            stddraw.setYscale(0,SizeEnvironment)
            for i in range(len(People)):
                if People[i][2] >0:
                    stddraw.setPenColor(stddraw.RED)
                    stddraw.filledCircle(People[i][0],People[i][1],2)
                    stddraw.circle(People[i][0], People[i][1], RadiusOfInfection)
                elif People[i][2] == 0 and People[i][3]==1:
                    stddraw.setPenColor(stddraw.BLUE)
                    stddraw.filledCircle(People[i][0],People[i][1],2)
                else:
                    stddraw.setPenColor(stddraw.BLACK)
                    stddraw.filledCircle(People[i][0], People[i][1],2)
            stddraw.show(CanvasRefreshTime)
            stddraw.clear()
            for k in range(len(People)):
                if People[k][2] > 0:
                    for l in range(len(People)):
                        if People[l][2] == 0 and People[l][3] != 1:
                            x1 = People[k][0]
                            y1 = People[k][1]
                            x2 = People[l][0]
                            y2 = People[l][1]
                            Distance = math.sqrt(((abs(x1-x2))**2)+((abs(y1-y2))**2))
                            if Distance<=RadiusOfInfection:
                                DiseaseChance = random.randrange(0,100)/100
                                if ChanceOfInfectionPerTimestep >= DiseaseChance:
                                    People[l][2] = -1
            
            


            for k in range(len(People)):
                if People[k][2] > 0: #set infection duration 1 day back
                    People[k][2] -= 1
                    if People[k][2] == 0: #if they go down to 0, make them immune (col 4 of People indicated immunity)
                        People[k][3] = 1
                        ImmuneCount += 1
                        CurrentlyInfected -= 1

            NewlyInfectedCount = 0
            for k in range(len(People)):
                if People[k][2] == -1: #set newly infected
                    People[k][2] = LengthOfInfection #
                    NewlyInfectedCount += 1
                    CurrentlyInfected += 1

            CurrentlyInfectedList += [CurrentlyInfected]
            ImmuneList += [ImmuneCount]
            NewlyInfectedList += [NewlyInfectedCount]


            for k in range(len(People)): #check olut htis
                People[k][0] += random.randrange(-100,100)/100*MoveLength
                if People[k][0] < 0:
                    People[k][0] += 2*MoveLength
                if People[k][0] > SizeEnvironment:
                    People[k][0] -= 2*MoveLength
                People[k][1] += random.randrange(-100, 100)/100*MoveLength
                if People[k][1] < 0:
                    People[k][1] += 2*MoveLength
                if People[k][1] > SizeEnvironment:
                    People[k][1] -= 2*MoveLength


                # while People[k][0] < 0 or People[k][0] > SizeEnvironment:
                #     People[k][0] += random.randrange(-100, 100)/100*MoveLength
                # People[k][1] += random.randrange(-100, 100)/100*MoveLength
                # while People[k][1] < 0 or People[k][1] > SizeEnvironment:
                #     People[k][1] += random.randrange(-100, 100)/100*MoveLength






        Results += [ImmuneCount]
        if a >= 5:
            Sum = 0
            for i in range(len(Results)):
                Sum += Results[i]
            Mean = Sum/len(Results)
            Variance = 0
            for i in range(len(Results)):
                Variance += (Results[i]-Mean)**2
            Variance = Variance/len(Results)
            Stdev = math.sqrt(Variance)
            for i in range(len(Results)):
                if abs(Results[i]-Mean) >(1*Stdev):
                    a -= 1
                else:
                    SimListsInfected += [CurrentlyInfectedList]
                    SimListsImmune += [ImmuneList]
                    SimListsNewlyInfected += [NewlyInfectedList]
                    ChangedVariable += [RadiusOfInfection]

        a += 1

print(SimListsImmune)
print(SimListsInfected)
print(SimListsNewlyInfected)

ColourList = [stddraw.ORANGE,stddraw.BLACK,stddraw.GREEN,stddraw.BLUE,stddraw.PINK]

for i in range(SimulationLength-1):
    stddraw.setXscale(0, SizeEnvironment)
    stddraw.setYscale(0, SizeEnvironment)
    Spacer = SizeEnvironment/(SimulationLength+1)
    for j in range(len(SimListsInfected)):
        stddraw.setPenColor(ColourList[j])
        # stddraw.line(Spacer * i, SimListsImmune[j][i] + 2, Spacer * (i + 1),SimListsImmune[j][i + 1] + 2)


        stddraw.line(Spacer*i,SimListsNewlyInfected[j][i]+10,Spacer*(i+1),SimListsNewlyInfected[j][i+1]+10)
        stddraw.show(100)
stddraw.show()
        
        















































#
# # def Virus_Sim(InfectionRate, InfectionRadius, People, time):
# #     Infected = 3
# #     for i in range(time):
# #
# #         Infected = Infected ** (InfectionRate * InfectionRadius)
# #         People = People - Infected
# #         if People <= 0:
# #             break
# #
# #     return Infected
# #
# #
# # print(Virus_Sim(0.2, 8, 100, 20))
# #
# # List = []
# # List2 = []
# # for i in range(5): # python starts from 0, that is why at the 5th iteration,
# #     List.append(Virus_Sim(0.2, i*2, 100, 20)) # the value is 90,009, which is equal to a InfectionRadius of 8
# #     List2 += [i*2]
# # # print(List)
# # # print(List2)
# # # # for i in range(5):
# # # #     print(i)
# # # #
# # # #
# # # # #
# # # # List1
# # # # List2
# # # # time = 20
# # # # for x in range(5):
# # # #     radius = x *0.2
# # # #     Infected = 0
# # # #     for i in range(time):#so this would be the virus simulation for you
# # # #         #some stuff
# # # #         #at the end of this you must have an Infected value for the end of the simulation
# # # #     out of the loop we now
# #
# # # List1= [1,2,3,4,5]
# # # List2= [List1,List1,List1]
# # # print(List2)
# time = 20
# BigList = []
# for i in range(5):
#     Radius = 1 *i
#     Infected = 3
#     List = []
#     for j in range(time):
#         NewlyInfected = int(Infected*Radius)
#         List += [NewlyInfected] # [0] [0]
#         #List.append(NewlyInfected)
#         Infected = Infected+NewlyInfected
#     BigList += [List]
#
# for i in range(len(BigList)):
#     print(BigList[i])