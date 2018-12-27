###############################################################################
#  CSC 455 Spring 2017
#  Take Home Final Part 3
#  Kari Palmier
###############################################################################

import sqlite3
from sqlite3 import OperationalError
import time
import json

asciiCharVals = list(range(65,91)) + list(range(97,123)) # ASCII vals for A-Z and a-z
numChars = len(asciiCharVals)                                     

totNumTweets = 1000000

###############################################################################
# Part 3a - Create SQL INSERT statements for every entry of the Users table in
#              the SQLite database.
###############################################################################
outFileNameTxt = 'UserInserts_FromSQL.txt'

outFileTxt = open(outFileNameTxt,'w')

startTime = time.time()

conn = sqlite3.connect('CSC455_Final.db')
c = conn.cursor()

queryStr = "SELECT * FROM Users;"
try:
    selRtn = c.execute(queryStr)
    selTbl = selRtn.fetchall()
except OperationalError as msg:
    print ("User table not in database.  Cannot generate statements. ", msg)

baseInsert = "INSERT INTO Users VALUES("

# loop over the rows returned, then loop over each attribute in each row  
insertCnt = 0    
dig0 = 0
dig1 = 0
dig2 = 0
dig3 = 0     
for x in range(len(selTbl)):
    
    # Create new character ID by incrementing from the last
    if dig2 == (numChars - 1):
        dig2 = 0
        dig1 = 0
        dig0 = 0
        dig3 += 1
    if dig1 == (numChars - 1):
        dig1 = 0
        dig0 = 0
        dig2 += 1
    if dig0 == (numChars - 1):
        dig0 = 0
        dig1 += 1
    else:
        dig0 += 1

    newID = ''.join([chr(asciiCharVals[dig3]),chr(asciiCharVals[dig2]),chr(asciiCharVals[dig1]),chr(asciiCharVals[dig0])])
    newIdStr = "'" + newID + "'"
    
    row = selTbl[x]
    id = row[0]
    name = row[1]
    screenName = row[2]
    desc = row[3]
    friendCnt = row[4]
        
    if name == None:
        nameTxt = 'NULL'
    elif '\\x' in str(name.encode('UTF8')):
         tempStr = str(name.encode('UTF8')) # Convert string from UTF8 to \x format
         tempStr = tempStr[2:-1]
         nameTxt = "'" + tempStr + "'"
    else:
         nameTxt = "'" + name + "'"
        
    if screenName == None:
        screenNameTxt = 'NULL'
    elif '\\x' in str(screenName.encode('UTF8')):
         tempStr = str(screenName.encode('UTF8')) # Convert string from UTF8 to \x format
         tempStr = tempStr[2:-1]
         screenNameTxt = "'" + tempStr + "'"
    else:
         screenNameTxt = "'" + screenName + "'"
    
    if desc == None:
        descTxt = 'NULL'
    elif '\\x' in str(desc.encode('UTF8')):
         tempStr = str(desc.encode('UTF8')) # Convert string from UTF8 to \x format
         tempStr = tempStr[2:-1]
         descTxt = "'" + tempStr + "'"
    else:
         descTxt = "'" + desc + "'"
         
    if friendCnt == None:
        friendCnt = 'NULL'
         
    # Create complete insert string and print
    insertStringTxt =  baseInsert + ', '.join([newIdStr, str(id), nameTxt, screenNameTxt, 
                                               descTxt, str(friendCnt)]) + ");\n"

    outFileTxt.write(insertStringTxt)
    
    insertCnt += 1
    
c.close()
conn.commit()
conn.close()

outFileTxt.close()

endTime = time.time()
elapsedTime = endTime - startTime
print('\n')
print('The number of insert statements created from SQLite =', insertCnt)
print('The time to create a text file with insert statements for every SQLite Users table row:', 
      elapsedTime, 'seconds.')

###############################################################################
# Part 3b - Create SQL INSERT statements for the entries of the Users dict
#             in the text file with all of the Tweets.
#           Use Insert Or Ignore instead of keeping track of unique IDs
###############################################################################
outFileNameTxt = 'UserInserts_FromFile.txt'

outFileTxt = open(outFileNameTxt,'w')

startTime = time.time()

tweetFile = open('Final_Tweets.txt','rb')

baseInsert = "INSERT OR IGNORE INTO Users VALUES("

# loop over the rows returned, then loop over each attribute in each row  
insertCnt = 0
dig0 = 0
dig1 = 0
dig2 = 0
dig3 = 0 
for x in range(totNumTweets):
    line = tweetFile.readline()
    tweet = line.decode('UTF8')
    
    try:
        tDict = json.loads(tweet)
    except:
        if tweet != '':
            print('JSON Conversion Error found in tweet ',x,'. Tweet not added to table.',sep='')
            continue
    
    # Create new character ID by incrementing from the last
    if dig2 == (numChars - 1):
        dig2 = 0
        dig1 = 0
        dig0 = 0
        dig3 += 1
    if dig1 == (numChars - 1):
        dig1 = 0
        dig0 = 0
        dig2 += 1
    if dig0 == (numChars - 1):
        dig0 = 0
        dig1 += 1
    else:
        dig0 += 1

    newID = ''.join([chr(asciiCharVals[dig3]),chr(asciiCharVals[dig2]),chr(asciiCharVals[dig1]),chr(asciiCharVals[dig0])])
    newIdStr = "'" + newID + "'"
    
    id = tDict['user']['id']
    name = tDict['user']['name']
    screenName = tDict['user']['screen_name']
    desc = tDict['user']['description']
    friendCnt = tDict['user']['friends_count']
      
    if name != None:
        if '\\x' in str(name.encode('UTF8')):
             tempStr = str(name.encode('UTF8')) # Convert string from UTF8 to \x format
             tempStr = tempStr[2:-1]
             nameTxt = "'" + tempStr + "'"
        else:
             nameTxt = "'" + name + "'"
    else:
        nameTxt = 'NULL'
        
    if screenName != None:
        if '\\x' in str(screenName.encode('UTF8')):
             tempStr = str(screenName.encode('UTF8')) # Convert string from UTF8 to \x format
             tempStr = tempStr[2:-1]
             screenNameTxt = "'" + tempStr + "'"
        else:
             screenNameTxt = "'" + screenName + "'"
    else:
        screenNameTxt = 'NULL'
    
    if desc != None:
        if '\\x' in str(desc.encode('UTF8')):
             tempStr = str(desc.encode('UTF8')) # Convert string from UTF8 to \x format
             tempStr = tempStr[2:-1]
             descTxt = "'" + tempStr + "'"
        else:
             descTxt = "'" + desc + "'"
    else:
        descTxt = 'NULL'
        
    if friendCnt == None:
        friendCnt = 'NULL'
         
    # Create complete insert string and print
    insertStringTxt =  baseInsert + ', '.join([newIdStr, str(id), nameTxt, screenNameTxt, 
                                               descTxt, str(friendCnt)]) + ");\n"

    outFileTxt.write(insertStringTxt)
    
    insertCnt += 1
    
outFileTxt.close()

endTime = time.time()
elapsedTime = endTime - startTime
print('\n')
print('The number of insert statements with ignore option created from the tweet file =', insertCnt)
print('The time to create a text file with insert statements wiht ignore option for every tweet file Users dict entry:', 
      elapsedTime, 'seconds.')

###############################################################################
# Part 3b - Create SQL INSERT statements for the entries of the Users dict
#             in the text file with all of the Tweets.
#           Use Insert and keep track of unique IDs (only create Inser for unique)
###############################################################################
outFileNameTxt = 'UserInserts_FromFile_Uniq.txt'

outFileTxt = open(outFileNameTxt,'w')

startTime = time.time()

tweetFile = open('Final_Tweets.txt','rb')

baseInsert = "INSERT INTO Users VALUES("

# loop over the rows returned, then loop over each attribute in each row  
insertCnt = 0
dig0 = 0
dig1 = 0
dig2 = 0
dig3 = 0 
uniqIds = []    
for x in range(totNumTweets):
    line = tweetFile.readline()
    tweet = line.decode('UTF8')
    
    try:
        tDict = json.loads(tweet)
    except:
        if tweet != '':
            print('JSON Conversion Error found in tweet ',x,'. Tweet not added to table.',sep='')
            continue

    if tDict['user']['id'] in uniqIds:
        continue
    else:
        uniqIds.append(tDict['user']['id'])
        
        # Create new character ID by incrementing from the last
        if dig2 == (numChars - 1):
            dig2 = 0
            dig1 = 0
            dig0 = 0
            dig3 += 1
        if dig1 == (numChars - 1):
            dig1 = 0
            dig0 = 0
            dig2 += 1
        if dig0 == (numChars - 1):
            dig0 = 0
            dig1 += 1
        else:
            dig0 += 1
    
        newID = ''.join([chr(asciiCharVals[dig3]),chr(asciiCharVals[dig2]),chr(asciiCharVals[dig1]),chr(asciiCharVals[dig0])])
        newIdStr = "'" + newID + "'"
        
        id = tDict['user']['id']
        name = tDict['user']['name']
        screenName = tDict['user']['screen_name']
        desc = tDict['user']['description']
        friendCnt = tDict['user']['friends_count']
          
        if name != None:
            if '\\x' in str(name.encode('UTF8')):
                 tempStr = str(name.encode('UTF8')) # Convert string from UTF8 to \x format
                 tempStr = tempStr[2:-1]
                 nameTxt = "'" + tempStr + "'"
            else:
                 nameTxt = "'" + name + "'"
        else:
            nameTxt = 'NULL'
            
        if screenName != None:
            if '\\x' in str(screenName.encode('UTF8')):
                 tempStr = str(screenName.encode('UTF8')) # Convert string from UTF8 to \x format
                 tempStr = tempStr[2:-1]
                 screenNameTxt = "'" + tempStr + "'"
            else:
                 screenNameTxt = "'" + screenName + "'"
        else:
            screenNameTxt = 'NULL'
        
        if desc != None:
            if '\\x' in str(desc.encode('UTF8')):
                 tempStr = str(desc.encode('UTF8')) # Convert string from UTF8 to \x format
                 tempStr = tempStr[2:-1]
                 descTxt = "'" + tempStr + "'"
            else:
                 descTxt = "'" + desc + "'"
        else:
            descTxt = 'NULL'
            
        if friendCnt == None:
            friendCnt = 'NULL'
             
        # Create complete insert string and print
        insertStringTxt =  baseInsert + ', '.join([newIdStr, str(id), nameTxt, screenNameTxt, 
                                                   descTxt, str(friendCnt)]) + ");\n"
    
        outFileTxt.write(insertStringTxt)
        
        insertCnt += 1
    
outFileTxt.close()

endTime = time.time()
elapsedTime = endTime - startTime
print('\n')
print('The number of insert statements created from the tweet file =', insertCnt)
print('The time to create a text file with insert statements for every tweet file Users dict entry:', 
      elapsedTime, 'seconds.')

