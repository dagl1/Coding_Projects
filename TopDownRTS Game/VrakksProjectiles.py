import pygame
import math
import random
import VrakksGamemanager



class projectiles():
    def __init__(self,startx, starty, targetx, targety,team):
        self.x = startx
        self.y = starty
        self.startx = startx
        self.starty = starty
        self.targetx = targetx
        self.targety = targety
        self.alive = True
        self.team = team
        projectiles.calculateBulletVector(self)

    def calculateBulletVector(self):
        xDistance = self.targetx - self.x
        yDistance = self.targety - self.y
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
            self.vector = ( xVector,yVector)
        else:
            self.vector = (0, 0)

    def createSimpleBullet(startx, starty, targetx, targety,team):
        VrakksGamemanager.GameManager.ProjectilesList += [simpleBullet(startx, starty, targetx, targety,team)]

    def moveProjectiles():
        for i in range(len(VrakksGamemanager.GameManager.ProjectilesList)):
            VrakksGamemanager.GameManager.ProjectilesList[i].x += VrakksGamemanager.GameManager.ProjectilesList[i].vector[0] * VrakksGamemanager.GameManager.ProjectilesList[i].speed
            VrakksGamemanager.GameManager.ProjectilesList[i].y += VrakksGamemanager.GameManager.ProjectilesList[i].vector[1] * VrakksGamemanager.GameManager.ProjectilesList[i].speed
            #checkWhich



    def calculateBulletOffset(self):
         self.vector = (random.uniform(-self.bulletOffset[0], self.bulletOffset[0]) + self.vector[0], random.uniform(-self.bulletOffset[1], self.bulletOffset[1]) + self.vector[1])

    def deleteProjectiles():
        VrakksGamemanager.GameManager.ProjectilesList = [projectile for projectile in VrakksGamemanager.GameManager.ProjectilesList if projectile.alive ==True]

    def OutOfBoundProjectiles(self):
        if self.x <0 or self.y <0 or self.x > 1200 or self.y >1800:
            self.alive = False

class simpleBullet(projectiles):
    def __init__(self,startx, starty, targetx, targety,team):
        super().__init__(startx, starty, targetx, targety,team)
        self.speed = 3
        self.length = 5
        self.width = 1
        self.color = (255,255,255)
        self.bulletOffset = (0.05,0.05)
        self.effectiveRange = 300
        self.damage = 9
        self.knockback = 5
        projectiles.calculateBulletOffset(self)



