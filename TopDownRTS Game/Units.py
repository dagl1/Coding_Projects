import pygame
import math
import random
import VrakksGamemanager
import VrakksProjectiles

class baseUnit():
    UnitList = []

    def __init__(self,spawnsettings):
        self.targetx = 0
        self.targety = 0
        self.vector = (0, 0)  # between 0 and 360
        self.speed = 1
        self.team = spawnsettings[2]
        self.x = spawnsettings[0]
        self.y = spawnsettings[1]
        self.atPosition = False
        self.squad = 0
        self.formation = 0
        self.formationTargetSet = False
        self.sightRange = 100
        self.size = 10
        baseUnit.setColor(self)
        self.shootTargetx = 0
        self.shootTargety = 0
        self.rect = pygame.Rect((spawnsettings[0],spawnsettings[1]),(self.size,self.size))
        self.selected = False
        self.shootTimer = random.randint(0,60)
        self.currentlyShooting = True
        self.hp = 20
        self.shootTargetUnit = 0
        self.shootLatency = random.randint(40,60)
        self.shootBurst = 3
        self.burstMode = [1,1,2,0]
        self.burstModeCounter = 0
        self.burstLatency = 5
        self.burstCounter = 0
        self.burststatus = False
        self.moving = False
    def setColor(self):
        if self.team == 0:
            self.color1 = 255
            self.color2 = 0
            self.color3 = 0
        if self.team == 1:
            self.color1 = 100
            self.color2 = 90
            self.color3 = 20
    def spawnUnit(spawnsettings):
        baseUnit.UnitList += [baseUnit(spawnsettings)]
    def checkIfUnitsCollideWithProjectilesAndAct():
        projectileList = VrakksGamemanager.GameManager.ProjectilesList
        for unit in baseUnit.UnitList:
            for projectile in projectileList:
                if projectile.team == unit.team:
                    if abs(projectile.startx-projectile.x) >10 or abs(projectile.starty-projectile.y)>10:
                        if unit.rect.colliderect(pygame.Rect(projectile.x+(projectile.length*projectile.vector[0]),projectile.y+(projectile.length*projectile.vector[1]),0,projectile.length)):
                            unit.projectileHits(projectile)
                            projectile.alive = False
                else:
                    if unit.rect.colliderect(pygame.Rect(projectile.x + (projectile.length * projectile.vector[0]),
                                                         projectile.y + (projectile.length * projectile.vector[1]), 0,
                                                         projectile.length)):
                        unit.projectileHits(projectile)
                        projectile.alive = False

    def projectileHits(self,projectile):
        self.bounceFromBullet(projectile)
        self.takeDamage(projectile)


    def takeDamage(self,projectile):
        self.hp = self.hp - projectile.damage
        #if killed remove from shootlist, this should probably go somewhere else at some point, but it doesnt work too well in kill units as it would run too often
        if self.hp<=0:
            for unit in baseUnit.UnitList:
                if unit.shootTargetUnit == self:
                    unit.shootTargetUnit = 0 #can be used to set a point to attack in general without seeing enemies (default back to this coordinate)

    def killUnits():
        baseUnit.UnitList = [unit for unit in baseUnit.UnitList if unit.hp>0]

    def bounceFromBullet(self,projectile):
        self.x += projectile.knockback*projectile.vector[0]
        self.y += projectile.knockback*projectile.vector[1]




class unitMovement():

    def setUnitTarget(self,targetx, targety):
        self.targetx = targetx
        self.targety = targety
        unitMovement.calculateVectorToTarget(self)
        self.atPosition = False

    def calculateVectorToTarget(self):
        xDistance = self.targetx - self.rect.x
        yDistance = self.targety - self.rect.y
        if (xDistance != 0 and yDistance != 0):
            if abs(xDistance) >= abs(yDistance):
                yVector = yDistance / abs(xDistance)
                if xDistance > 0:
                    xVector = 1
                elif xDistance < 0:
                    xVector = -1
            else:
                xVector = xDistance / abs(yDistance)
                if yDistance > 0:
                    yVector = 1
                elif yDistance < 0:
                    yVector = -1
            self.vector = (xVector, yVector)
            self.atPosition = False
        # else:
        #     self.vector = (0, 0)
        #     self.atPosition = True

    def targetReached(self):
        if abs(self.rect.x - self.targetx) < (2.5 * self.speed) and abs(self.rect.y - self.targety) < (2.5 * self.speed):
            return True
        else:
            return False

    def teleportToTarget(self):
        self.rect.x = self.targetx
        self.rect.y = self.targety
        self.vector = (0,0)
        self.atPosition = True

    def moveUnit(self):
        if self.currentlyShooting == True:
            self.x += (self.vector[0] * self.speed*0.5)
            self.y += (self.vector[1] * self.speed*0.5)
        else:
            self.x += (self.vector[0] * self.speed)
            self.y += (self.vector[1] * self.speed)
        if self.vector[1] != 0 and self.vector[0] !=0:
            self.moving = True
        self.rect.x = round(self.x)
        self.rect.y = round(self.y)
        #self.currentlyShooting = False

    def doMovementThingsToUnits(): # every frame
        for i in range(len(baseUnit.UnitList)):
            shooting.hasShootTargetMoved(baseUnit.UnitList[i])
            if baseUnit.UnitList[i].atPosition == False:
                if unitMovement.targetReached(baseUnit.UnitList[i])==True:
                    unitMovement.teleportToTarget(baseUnit.UnitList[i])
                    baseUnit.UnitList[i].moving = False
                else:
                    unitMovement.moveUnit(baseUnit.UnitList[i])





class shooting():

    def findInRange(unit,target):
        xrange = abs(unit.x-target.x)
        yrange = abs(unit.y - target.y)
        if xrange < unit.sightRange and yrange < unit.sightRange:
            return target





    def findShootTarget():

        for unit in baseUnit.UnitList:
            targetlist = []
            if unit.shootTargetUnit == 0:# and unit.moving == False:
                for target in baseUnit.UnitList:
                    if unit.team != target.team:
                        if shooting.findInRange(unit,target) != None:
                            targetlist += [shooting.findInRange(unit,target)]
                if len(targetlist) != 0:
                    unit.shootTargetUnit = targetlist[random.randint(0,len(targetlist)-1)]
                    shooting.getShootTarget(unit,unit.shootTargetUnit.x,unit.shootTargetUnit.y)
                    unit.currentlyShooting = True
                else:
                    unit.currentlyShooting = False
            elif unit.shootTargetUnit == 1:
                unit.currentlyShooting = True
                pass
            else:
                for target in baseUnit.UnitList:
                    xrange = abs(unit.x - target.x)
                    yrange = abs(unit.y - target.y)
                    if xrange > unit.sightRange+30 and yrange > unit.sightRange+30:
                        unit.shootTargetUnit = 0
                        unit.currentlyShooting = False



    def getShootTarget(self,targetx, targety):
        self.shootTargetx = targetx
        self.shootTargety = targety
        shooting.isInRange(self)
        #if True:
        self.currentlyShooting = True
    def isInRange(self):
        pass

    def shootBulletAtShootTarget(self,timer):
        inrange = True
        if inrange:

            if (self.shootTimer + timer) % self.shootLatency ==0:
                startx = self.rect.x
                starty = self.rect.y
                targetx = self.shootTargetx
                targety = self.shootTargety
                VrakksProjectiles.projectiles.createSimpleBullet(startx, starty, targetx, targety, self.team) # int and give int back
                self.burststatus = True
            elif self.burstMode != False:
                if self.burststatus == True:
                    if self.burstMode[self.burstModeCounter] == 1:

                        if (self.shootTimer + timer % self.shootLatency) % self.burstLatency == 0:
                            startx = self.rect.x
                            starty = self.rect.y
                            targetx = self.shootTargetx
                            targety = self.shootTargety
                            VrakksProjectiles.projectiles.createSimpleBullet(startx, starty, targetx, targety, self.team) #outputs
                            self.burstCounter = (self.burstCounter +1)%(self.shootBurst-1)
                            if self.burstCounter == 0:
                                self.burstModeCounter = (self.burstModeCounter + 1) % (len(self.burstMode) )
                                self.burststatus = False
                    elif self.burstMode[self.burstModeCounter] == 2:
                        if (self.shootTimer + timer % self.shootLatency) % self.burstLatency == 0:
                            startx = self.rect.x
                            starty = self.rect.y
                            targetx = self.shootTargetx
                            targety = self.shootTargety
                            VrakksProjectiles.projectiles.createSimpleBullet(startx, starty, targetx, targety, self.team)
                            self.burstCounter = (self.burstCounter +1)%(self.shootBurst*3)
                            if self.burstCounter == 0:
                                self.burststatus = False
                                self.burstModeCounter = (self.burstModeCounter+1)%(len(self.burstMode))
                                #self.shootLatency += 20
                    elif self.burstMode[self.burstModeCounter] ==0:
                        if (self.shootTimer + timer % self.shootLatency) % self.burstLatency == 0:
                            self.burstCounter = (self.burstCounter + 1) % (self.shootBurst * 3)
                            if self.burstCounter == 0:
                                self.burststatus = False
                                self.burstModeCounter = (self.burstModeCounter + 1) % (len(self.burstMode))





    def hasShootTargetMoved(self):
        try:
            self.shootTargetx = self.shootTargetUnit.x
            self.shootTargety = self.shootTargetUnit.y
        except AttributeError:
            pass

            # self.shootTargetx = self.shootTargetUnit
            # self.shootTargety = self.shootTargetUnit

            #take time to re aim function

    #Teams
    #firing solutions
        #if no specific target clicked:
            #find position of nearest priority target
            #shoot
    #shoot
        #shootanimation
        #bulletcollision
            #no close quarter friendlyfire
        #health


    #spawn unit with no target or with waypoint
    #ifNewTarget from command

        #if enemy
            #setMovementTarget
            #set firing solutions
        #if location only
            #setmovementTarget
            #calculateformation
            #if targetreached
                #teleport and set atPosition True
            #   else:
                #calculateVector
                #set at poistion False

    #for each unit
        #check if atPosition else:
            #check ifTargetMoved
                #setMovementTarget
                #calculateformation
                #calculateVector
            #check if targetreached else
                #execute movement


for i in range(3):
    x = random.randint(500, 1000)
    y = random.randint(0, 500)
    spawnsettings = [x, y, 0]
    baseUnit.spawnUnit(spawnsettings)

# for i in range(1):
#     x = random.randint(500, 1000)
#     y = random.randint(0, 500)
#     spawnsettings = [x, y, 1]
#     baseUnit.spawnUnit(spawnsettings)

