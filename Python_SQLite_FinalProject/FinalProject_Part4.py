###############################################################################
#  CSC 455 Spring 2017
#  Take Home Final Part 4
#  Kari Palmier
###############################################################################

import sqlite3
from sqlite3 import OperationalError
import time
import math

def deg2rad(deg):
    return deg * (math.pi/180)

def getDistanceFromLatLonInM(lat1,lon1,lat2,lon2):
    R = 6371; # Radius of the earth in km
    dLat = deg2rad(lat2-lat1);  # deg2rad below
    dLon = deg2rad(lon2-lon1); 
    a = math.sin(dLat/2) * math.sin(dLat/2) + math.cos(deg2rad(lat1)) * math.cos(deg2rad(lat2)) * math.sin(dLon/2) * math.sin(dLon/2)

    c = 2 * math.atan2(math.sqrt(a), math.sqrt(1-a)); 
    dKm = R * c; # Distance in km
    dMiles = dKm * 0.62137119
    return dMiles
    
def getEuclideanDistance(lat1,lon1,lat2,lon2):
    dist = math.sqrt((lat2-lat1)**2 + (lon2-lon1)**2)
    return dist

###############################################################################
# Part 4a - Create an output file with Geo data, including a new column with
#            the distance from (41.878668,-87.625555), columns separated with |
###############################################################################
outFileNameTxt = 'Geo_Contents.txt'

outFileTxt = open(outFileNameTxt,'w')

startTime = time.time()

conn = sqlite3.connect('CSC455_Final.db')
c = conn.cursor()

queryStr = "SELECT * FROM Geo;"
try:
    selRtn = c.execute(queryStr)
    selTbl = selRtn.fetchall()
except OperationalError as msg:
    print ("Geo table not in database.", msg)

# loop over the rows returned, then loop over each attribute in each row  
insertCnt = 0                                              
for x in range(len(selTbl)):
    
    row = list(selTbl[x])
    
    geoType = row[1]
    geoLat = row[2]
    geoLong = row[3]
    
    distMiles = getDistanceFromLatLonInM(geoLat, geoLong, 41.878668, -87.625555)
    eucDist = getEuclideanDistance(geoLat, geoLong, 41.878668, -87.625555)
    
    outStr = '|'.join([geoType, str(round(geoLat,4)), str(round(geoLong,4)), str(round(eucDist, 4)), str(round(distMiles, 4))]) + '\n'
    outFileTxt.write(outStr)
    
c.close()
conn.commit()
conn.close()

outFileTxt.close()

endTime = time.time()
elapsedTime = endTime - startTime
print('\n')
print('The number of Geo table rows written to the file =', len(selTbl))
print('The time to write Geo table rows to the file:', elapsedTime, 'seconds.')

###############################################################################
# Part 4b - Create an output file with tweet data, including new columns with
#            the user name and screen name, columns separated with |.
#            Report the number of locations known and unknown.
###############################################################################
outFileNameTxt = 'Tweets_Contents.txt'

outFileTxt = open(outFileNameTxt,'w')

startTime = time.time()

conn = sqlite3.connect('CSC455_Final.db')
c = conn.cursor()

queryStr = """SELECT created_at, id_str, text, source, in_reply_to_user_id, 
                 in_reply_to_screen_name, in_reply_to_status_id, retweet_count,
                 contributors, geo_id_str, name, screen_name
              FROM Tweets INNER JOIN Users
                      ON user_id = id;"""

try:
    selRtn = c.execute(queryStr)
    selTbl = selRtn.fetchall()
except OperationalError as msg:
    print ("Tweets or Users table not in database.", msg)

# loop over the rows returned, then loop over each attribute in each row  
insertCnt = 0     
geoKnown = 0
geoUnknown = 0                                         
for x in range(len(selTbl)):
    
    row = list(selTbl[x])
    
    newParams = []
    newParamsTxt = []
    for x in row:
        if type(x) == str:
            newParams.append(x)
            if '\\x' in str(x.encode('UTF8')):
                tempStr = str(x.encode('UTF8')) # Convert string from UTF8 to \x format
                newParamsTxt.append(tempStr[2:-1])
            else:
                newParamsTxt.append(x)
        elif type(x) == int:
            newParams.append(str(x))
            newParamsTxt.append(str(x))

    if row[9] == 'NULL' or row[9] == None:
        geoUnknown += 1
    else:
        geoKnown += 1
            
    txtOutStr = '|'.join(newParamsTxt) + '\n'
    outFileTxt.write(txtOutStr)
    
c.close()
conn.commit()
conn.close()

outFileTxt.close()

endTime = time.time()
elapsedTime = endTime - startTime
print('\n')
print('The number of Tweets table rows written to the file =', len(selTbl))
print('The number of known locations =', geoKnown)
print('The number of unknown locations =', geoUnknown)
print('The percentage of unknown locations =', ((geoKnown/len(selTbl)*100)))
print('The time to write Tweets table rows to the file:', elapsedTime, 'seconds.')

###############################################################################
# Part 4c - Create an output file with user data, including a new column for 
#            number of tweets for the user, columns separated with |.
#            Report the user name with the most tweets.
###############################################################################
outFileNameTxt = 'Users_Contents.txt'

outFileTxt = open(outFileNameTxt,'w')

startTime = time.time()

conn = sqlite3.connect('CSC455_Final.db')
c = conn.cursor()

queryStr = """SELECT id, name, screen_name, description, friends_count, COUNT(id_str)
              FROM Users INNER JOIN Tweets
                 ON id = user_id
              GROUP BY id;"""
              
try:
    selRtn = c.execute(queryStr)
    selTbl = selRtn.fetchall()
except OperationalError as msg:
    print ("Tweets or Users table not in database.", msg)

# loop over the rows returned, then loop over each attribute in each row  
insertCnt = 0     
geoKnown = 0
geoUnknown = 0                                         
maxTweetCnt = 0 
maxTweetName = ''                                        
for x in range(len(selTbl)):
    
    row = list(selTbl[x])
    
    newParams = []
    newParamsTxt = []
    for x in row:
        if type(x) == str:
            newParams.append(x)
            if '\\x' in str(x.encode('UTF8')):
                tempStr = str(x.encode('UTF8')) # Convert string from UTF8 to \x format
                newParamsTxt.append(tempStr[2:-1])
            else:
                newParamsTxt.append(x)
        elif type(x) == int:
            newParams.append(str(x))
            newParamsTxt.append(str(x))

    
    if row[5] > maxTweetCnt:
        maxTweetCnt = row[5]
        maxTweetName = row[1]

    txtOutStr = '|'.join(newParamsTxt) + '\n'
    outFileTxt.write(txtOutStr)
    
c.close()
conn.commit()
conn.close()

outFileTxt.close()

endTime = time.time()
elapsedTime = endTime - startTime
print('\n')
print('The number of Users table rows written to the file =', len(selTbl))
print('The maximum number of tweets per user =', maxTweetCnt)
print('The user with the most tweets =', maxTweetName)
print('The time to write Users table rows to the file:', elapsedTime, 'seconds.')

