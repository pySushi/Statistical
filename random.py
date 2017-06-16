#___________________________________________________
#Custom functions used for the module Random Numbers
#The output will be as many values as "samsize" between "lowNumer" and "highNumber" with initial seed value "seedNumber"
#___________________________________________________

# Import dependencies
import os
import math
import random

#Custom function to round a number to its nearest integer
#iround(<number>) --> <integer>
def iround(x):
    return int(round(x) - .5) + (x > 0)
    
#Custom function to convert a hex string to a raw bit string
#rawBit(<hex string>) --> <bit string of type str>
def rawBit(hexString):
    bitString= ''
    for letter in hexString[len(hexString)/2:]:
        if letter ==0 or letter =='0':
            bitString = bitString + '00000000'
        elif letter ==1 or letter =='1':
            bitString = bitString + '01000000'
        elif letter ==2 or letter =='2':
            bitString = bitString + '00010000'
        elif letter ==3 or letter =='3':
            bitString = bitString + '01010000'
        elif letter ==4 or letter =='4':
            bitString = bitString + '00000100'
        elif letter ==5 or letter =='5':
            bitString = bitString + '01000100'
        elif letter ==6 or letter =='6':
            bitString = bitString + '00010100'
        elif letter ==7 or letter =='7':
            bitString = bitString + '01010100'
        elif letter ==8 or letter =='8':
            bitString = bitString + '00000001'
        elif letter ==9 or letter =='9':
            bitString = bitString + '01000001'
        elif letter =='A' or letter =='a':
            bitString = bitString + '00010001'
        elif letter =='B' or letter =='b':
            bitString = bitString + '01010001'
        elif letter =='C' or letter =='c':
            bitString = bitString + '00000101'
        elif letter =='D' or letter =='d':
            bitString = bitString + '01000101'
        elif letter =='E' or letter =='e':
            bitString = bitString + '00010101'
        elif letter =='F' or letter =='f':
            bitString = bitString + '01010101'
    return bitString

#Custom function to convert a floating type number to a 64 bit binary string
#float_to_bin(<floating number>) --> <64 bit binary string representing the input>
def float_to_bin(x):
    if x == 0:
        return "0" * 64
    w, sign = (float.hex(x), 0) if x > 0 else (float.hex(x)[1:], 1)
    mantissa, exp = int(w[4:17], 16), int(w[18:])
    return "{}{:011b}{:052b}".format(sign, exp + 1023, mantissa)

#Custom function to reverse a hex string
#float_to_bin(<floating number>) --> <64 bit binary string representing the input>
def pairwise_rev_hex(stringInput):
    groupLen = 1
    return '' if not stringInput else pairwise_rev_hex(stringInput[groupLen:]) + stringInput[:groupLen]

#Replicates the operation of Rnd() from Visual Basic 6 when a positive input parameter is used.
#excelRand64(<seed number>, <max number>) --> <64 bit binary string representing the input>
def excelRand64(excelSeed, outputMax):
    #Initialize value of three seed variables and other variables
    varA = excelSeed
    varB = 1140671485
    varC = 12820163
    firstFlag= True
    aValue = 0
    
    #Calculate values for seed number
    while(aValue>=outputMax or firstFlag):
        firstFlag = False
        varA = excelSeed
        excelSeed = int((varA*varB + varC)%16777216)
        aValue = 1+(outputMax*(float(excelSeed)/16777216))
    
    #Update output values
    stepRes = excelSeed
    outputRes = float(excelSeed)/16777216
    seedRes = aValue
    
    #Returns the output values to the calling function
    return stepRes, outputRes, seedRes



#Replicates the operation of the Visual Based 6 function Randomize()
def excelRandomize64(userSeed,excelSeed):
    import struct # import dependency
    print 'userSeed:', userSeed
    #_______________________________________________________
    #convert user seed to raw bits
    #_______________________________________________________
    testHelpTemp = float_to_bin(float(userSeed))
    testHelpTempHex = str(hex(int(testHelpTemp, 2)))[2:-1]
    testHelpTemp = str(pairwise_rev_hex(testHelpTempHex))
    bitList=rawBit(testHelpTemp) 
    
    bitSeed = '{0:032b}'.format(int(excelSeed))[::-1] #truncated to include only the first 32 bits
    
    #_______________________________________________________
    #first 16 bits and the last 16 bits of the bit string in Step 3 are then XOR together in raw string format
    #_______________________________________________________
    bitSeed = bitSeed.replace('0', 'XX')
    bitSeed = bitSeed.replace('1', '01')
    bitSeed = bitSeed.replace('X', '0')
    leftBits = (bitList)[:32]+'00000000000000000000000000000000'    
    rightBits = (bitList)[32:]+'00000000000000000000000000000000' 
    seedXOR = '{0:064b}'.format(int(leftBits, 2)^int(rightBits, 2))

    bitSeedRight = bitSeed[48:]
    
    #_______________________________________________________
    #The last 8 bits of the bit string (bitSeed) are appended to the end of the 16 bit string (seedXOR)
    #Eight 0 value bits are then appended to the front of the bit string to result in the full 32 bit string in raw format)
    #_______________________________________________________
    finalMosh = '{0:064b}'.format(int(('0000000000000000' + seedXOR[:32] + '0000000000000000'), 2)| int((bitSeed[:16] + '000000000000000000000000000000000000000000000000'), 2))
    
    #_______________________________________________________
    #the raw bits now converted to integer
    #_______________________________________________________
    finalMoshTemp = finalMosh
    finalMoshTemp = finalMoshTemp.replace('01', 'X')
    finalMoshTemp = finalMoshTemp.replace('00', '0')
    finalMoshTemp = finalMoshTemp.replace('X', '1')
    finalMoshTemp =finalMoshTemp[::-1]
    newSeed =  0xFFFFFFFF & (int(finalMoshTemp, 2))
    #print 'newSeed:', newSeed
    return newSeed

#Replicates the operation of the Visual Basic 6 function Rnd() when a negative input parameter is used.  
def excelRandInit(initVar):
    testHelp = float_to_bin(float(int(initVar)))
    initValue = str(str(testHelp[24:32])+'000000000000000000000000') + str(str(testHelp[1:24])+ '00000000')
    return int(initValue)

# Call to custom function for random number list generation passing high number, low number, sample size, and the three seeds A, B, C as input, based on Wichmann Hill Algorithm
def randomNumGen1(highNumber, lowNumber, samsiz, result_A, result_B, result_C):
    universe = (highNumber - lowNumber + 1) # Calculate size of universe
    randomNumber = [None]*samsiz # Initialize random number list to 'None'
    repCheck = [0]*universe # Parameter to check if number generated is a repeat, initialized to 0
    count = 0 #Initialize of count of unique random numbers generated
    covered=set()
    while count < samsiz: # Loop to generate random numbers based on Wichmann Hill Algorithm
        repFlag = True
        while repFlag:
            term_1 = math.floor(result_A/177)
            term_2 = result_A - (177*term_1)
            result_A = 171*term_2 - 2*term_1
            if result_A <= 0:
                result_A = result_A +30269

            term_1 = math.floor(result_B/176)
            term_2 = result_B - (176*term_1)
            result_B = 172*term_2 - 35*term_1
            if result_B <= 0:
                result_B = result_B +30307

            term_1 = math.floor(result_C/178)
            term_2 = result_C - (178*term_1)
            result_C = 170*term_2 - 63*term_1
            if result_C <= 0:
                result_C = result_C +30323

            term_4 = result_A/30269 +  result_B/30307 + result_C/30323

            randomNumber[count] = int(math.floor((term_4 - math.floor(term_4))*(highNumber-lowNumber+1))+lowNumber)  # Calculate random number
            
            #_________________________________________________
            # check if number generated is a repeat and increment value of count if its unique
            #_________________________________________________
            if randomNumber[count] not in covered:
                covered.add(randomNumber[count])
                count= count+1
                repFlag = False
            else:
                repFlag = True
    return randomNumber
	
#Input parameters
seedNumber=0 # seed value
highNumber=1000 # upper limit
lowNumber=10 # lower limit
samsiz=100 # number of random numbers to be generated

currentSeed = 3758214  # Initialize current seed value
currentSeed = excelRandomize64(seedNumber,currentSeed) # call to custom function to get updated seed value based on inputs
stepResA, outputResA, seedResA = excelRand64(currentSeed,30269)  #  call to custom function to get 1st seed value A
stepResB, outputResB, seedResB = excelRand64(stepResA,30307)  #  call to custom function to get 2nd seed value B
stepResC, outputResC, seedResC = excelRand64(stepResB,30323)  #  call to custom function to get 3rd seed value C
            
randomNumberList = randomNumGen1(highNumber, lowNumber, samsiz, int(seedResA), int(seedResB), int(seedResC)) # Call to custom function for random number list generation passing high number, low number, sample size, and the three seeds A, B, C as input
print 'randomNumberList:', randomNumberList
