import random
import sys
import stdarray
import stddraw

def uniformFloat(lo, hi):
    """
    Return a number chosen uniformly from the range [lo, hi).
    """
    return random.uniform(lo, hi)

def discrete(a):
    """
    Return a float from a discrete distribution: i with probability
    a[i].  Precondition: the elements of array a sum to 1.
    """
    r = uniformFloat(0.0, sum(a))
    subtotal = 0.0
    for i in range(len(a)):
        subtotal += a[i]
        if subtotal > r:
            return i
    #return len(a) - 1

def main():
    n = 100000

    dist = stdarray.readFloat1D()
    cx = stdarray.readFloat2D()
    cy = stdarray.readFloat2D()
    x = 0.0
    y = 0.0
    stddraw.setPenRadius(0.0)
    for i in range(n):
        r = discrete(dist)
        x0 = cx[r][0]*x + cx[r][1]*y + cx[r][2]
        y0 = cy[r][0]*x + cy[r][1]*y + cy[r][2]
        x = x0
        y = y0
        stddraw.point(x, y)
    stddraw.show()


n = 1000000

WinCounter = 0
LossCounter = 0 

for i in range(n):
	DiceSum = random.randint(1,6) + random.randint(1,6)
	# print('DiceSum',DiceSum)

	if DiceSum == 7 or DiceSum == 2:
		WonLost = True
		WinCounter +=1
	elif DiceSum == 2 or DiceSum ==3 or DiceSum ==12 or DiceSum ==11:
		WonLost =True
		LossCounter +=1
	else:
		WonLost = False
		while WonLost == False: 
			DiceSum2 = random.randint(1,6) + random.randint(1,6)
			# print('DiceSum2',DiceSum2)
			if DiceSum2 == 7:
				WonLost =True
				LossCounter +=1
			elif DiceSum2 == DiceSum:
				WonLost = True
				WinCounter +=1


print(WinCounter, LossCounter)