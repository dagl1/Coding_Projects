import matplotlib.pyplot as plt
from skimage import io
from skimage.filters.rank import entropy
from skimage.morphology import disk
import numpy as np
from skimage.filters import threshold_otsu
from PIL import Image
from skimage import data
import pylab as p
import glob
import cv2
from skimage import img_as_ubyte

time = 0
time_list=[]
area_list=[]
path = "E:/Downloads/Scratch/*.*"
for file in glob.glob(path):
    print(file)
    dict={}
    image = Image.open(file)
    #img = data.camera()
    #print(img.ndim)
    #entropy_img = entropy(img,disk(5))
    #new_image = image.resize((1200, 1200))
    #new_image.save('myimage_500.jpg')
    img=io.imread(file)
    #img = rgb2gray(img)
    # Convert to grayscale
    gray_image = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
    img = img_as_ubyte(gray_image)
    
    height, width = gray_image.shape
    crop_left = int(0.3 * width)
    crop_right = int(0.7 * width)
    crop_top = int(0.6 * height)
    crop_bottom = int(0.8 * height)
    cropped_image = gray_image[crop_top:crop_bottom, crop_left:crop_right]

    plt.imshow(cropped_image, cmap='gray')
    plt.show()

    entropy_img = entropy(img,disk(10))

    thresh = threshold_otsu(entropy_img)
    binary = entropy_img <= thresh
    scratch_area = np.sum(binary == 1)
    #print("time=", time, "hr  ", "Scratch area=", scratch_area, "pix\N{SUPERSCRIPT TWO}")
    time_list.append(time)
    area_list.append(scratch_area)
    #print(scratch_area)
    print( 'done image')
    time += 1

print(time_list, area_list)
plt.plot(time_list, area_list, 'bo')  #Print blue dots scatter plot
p.show()
# # # plt.savefig(a)
# # #Print slope, intercept
# from scipy.stats import linregress
# print(linregress(time_list, area_list))

# slope, intercept, r_value, p_value, std_err = linregress(time_list, area_list)
# print("y = ",slope, "x", " + ", intercept  )
# print("R\N{SUPERSCRIPT TWO} = ", r_value**2)
#print("r-squared: %f" % r_value**2)
