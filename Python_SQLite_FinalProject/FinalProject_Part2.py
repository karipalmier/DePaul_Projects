###############################################################################
#  CSC 455 Spring 2017
#  Part 2
#  Kari Palmier
###############################################################################

import sqlite3
from sqlite3 import OperationalError
import time
import json

totNumTweets = 1000000

# Open SQLite database
conn = sqlite3.connect('CSC455_Final.db')
c = conn.cursor()

###############################################################################
# Part 2a i - Find tweets where tweet id_str contains either 44 or 77
###############################################################################
id44_77Query = """SELECT id_str
                 FROM Tweets
                 WHERE id_str LIKE '%44%' or id_str LIKE '%77%';"""

startTime = time.time()
try:
    selTbl = c.execute(id44_77Query).fetchall()
except OperationalError as msg:
    print('OperationalError occured while trying to find tweet ids with 44 or 77.')
    print(msg)
endTime = time.time()
elapsedTime = endTime - startTime

print('Number of tweet ID strings with either 44 of 77 in them:',len(selTbl))
if len(selTbl) > 0:
    if len(selTbl) < 5:
        printNum = len(selTbl)
    else:
        printNum = 5
    print('The first', printNum, 'tweet IDs with 44 or 77 in them from SQL are:')
    for x in range(printNum):
        print('Matching entry', x, 'ID =', selTbl[x][0])
print('Time to run query to find tweet id_str entriess with 44 or 77: {:.10f} seconds.'.format(elapsedTime))

###############################################################################
# Part 2a ii - Find number of tweets with unique in_reply_to_user_id entries
###############################################################################
uniqInRplyIDQuery = """SELECT COUNT(DISTINCT in_reply_to_user_id)
                 FROM Tweets;"""

startTime = time.time()
try:
    selTbl = c.execute(uniqInRplyIDQuery).fetchall()
except OperationalError as msg:
    print('OperationalError occured while trying to find unique in_reply_to_user_id entries.')
    print(msg)
endTime = time.time()
elapsedTime = endTime - startTime

print('\n')
print('Number of tweets with unique in_reply_to_user_id entries:',selTbl[0][0])
print('Time to run query to number of tweets with unique in_reply_to_user_id entries: {:.10f} seconds.'.format(elapsedTime))
    
###############################################################################
# Part 2a iii - Find the tweets with shortest, longest, and avg length of text 
###############################################################################
# Return the shortest text messages in SQL
shortTextQuery = """SELECT text
                    FROM Tweets
                    WHERE LENGTH(text) = (SELECT MIN(LENGTH(text))
                                           FROM Tweets);"""

startTime = time.time()
try:
    selTbl = c.execute(shortTextQuery).fetchall()
except OperationalError as msg:
    print('OperationalError occured while trying to find tweets with shortest text length.')
    print(msg)
endTime = time.time()
elapsedTime = endTime - startTime
    
print('\n')
print('Number of tweets with text entry lengths equal to the shortest text length:',len(selTbl))
print('Length of shortest text messages:',len(selTbl[0][0]))
if len(selTbl) > 0:
    if len(selTbl) < 5:
        printNum = len(selTbl)
    else:
        printNum = 5
    print('The first', printNum, 'shortest text messages from SQL are:')
    for x in range(printNum):
        print('Text message', x, '=', selTbl[x][0])
print('Time to run query to find tweets with shortest text length: {:.10f} seconds.'.format(elapsedTime))

# Return the longest text messages in SQL
longTextQuery = """SELECT text
                    FROM Tweets
                    WHERE LENGTH(text) = (SELECT MAX(LENGTH(text))
                                           FROM Tweets);"""

startTime = time.time()
try:
    selTbl = c.execute(longTextQuery).fetchall()
except OperationalError as msg:
    print('OperationalError occured while trying to find tweets with longest text length.')
    print(msg)
endTime = time.time()
elapsedTime = endTime - startTime
    
print('\n')
print('Number of tweets with text entry lengths equal to the longest text length:',len(selTbl))
print('Length of longest text messages:',len(selTbl[0][0]))
if len(selTbl) > 0:
    if len(selTbl) < 5:
        printNum = len(selTbl)
    else:
        printNum = 5
    print('The first', printNum, 'longest text messages from SQL are:')
    for x in range(printNum):
        print('Text message', x, '=', selTbl[x][0])
print('Time to run query to find tweets with longest text length: {:.10f} seconds.'.format(elapsedTime))

avgTextQuery = """SELECT text
                    FROM Tweets
                    WHERE LENGTH(text) = (SELECT CAST(AVG(LENGTH(text)) AS INT)
                                           FROM Tweets);"""

startTime = time.time()
try:
    selTbl = c.execute(avgTextQuery).fetchall()
except OperationalError as msg:
    print('OperationalError occured while trying to find tweets with average text length.')
    print(msg)
endTime = time.time()
elapsedTime = endTime - startTime
    
print('\n')
print('Number of tweets with text entry lengths equal to the average text length:',len(selTbl))
print('Length of average text messages:',len(selTbl[0][0]))
if len(selTbl) > 0:
    if len(selTbl) < 5:
        printNum = len(selTbl)
    else:
        printNum = 5
    print('The first', printNum, 'average text messages from SQL are:')
    for x in range(printNum):
        print('Text message', x, '=', selTbl[x][0])
print('Time to run query to find tweets with average text length: {:.10f} seconds.'.format(elapsedTime))

###############################################################################
# Part 2a iv - Find the average latitude and longitude per user 
###############################################################################
avgLatLonQuery = """SELECT id, name, AVG(latitude), AVG(longitude) 
                    FROM Tweets INNER JOIN GEO
                      ON id_Str = tweet_id_str
                    INNER JOIN Users
                      ON user_id == id
                    GROUP BY user_id;"""

startTime = time.time()
try:
    selTbl = c.execute(avgLatLonQuery).fetchall()
except OperationalError as msg:
    print('OperationalError occured while trying to the average latitude and longitude per user.')
    print(msg)
endTime = time.time()
elapsedTime = endTime - startTime
    
print('\n')
print('Number of tweets with unique user ID and average latitude and longitude:',len(selTbl))
if len(selTbl) > 0:
    if len(selTbl) < 5:
        printNum = len(selTbl)
    else:
        printNum = 5
    print('The first', printNum, 'user names, ids, and average latitude and longitude values from SQL are:')
    for x in range(printNum):
        print('User ID: ', selTbl[x][0], ', User Name:', selTbl[x][1], ', Average Latitude: ', selTbl[x][2], ', Average Longitude: ', selTbl[x][3], sep='')
print('Time to run query to average latitude and longitude per user: {:.10f} seconds.'.format(elapsedTime))

###############################################################################
# Part 2a v - Times of running avg latitude and longitude per user 10x and 100x
###############################################################################
avgLatLonQuery = """SELECT id, name, AVG(latitude), AVG(longitude) 
                    FROM Tweets INNER JOIN GEO
                      ON id_Str = tweet_id_str
                    INNER JOIN Users
                      ON user_id = id
                    GROUP BY user_id;"""

startTime = time.time()
for x in range(10):
    try:
        selTbl = c.execute(avgLatLonQuery).fetchall()
    except OperationalError as msg:
        print('OperationalError occured while trying to the average latitude and longitude per user.')
        print(msg)
endTime = time.time()
elapsedTime = endTime - startTime
    
print('\n')
print('Time to run query to average latitude and longitude per user 10x: {:.10f} seconds.'.format(elapsedTime))

startTime = time.time()
for x in range(100):
    try:
        selTbl = c.execute(avgLatLonQuery).fetchall()
    except OperationalError as msg:
        print('OperationalError occured while trying to the average latitude and longitude per user.')
        print(msg)
endTime = time.time()
elapsedTime = endTime - startTime

print('Time to run query to average latitude and longitude per user 100x: {:.10f} seconds.'.format(elapsedTime))

c.close()
conn.commit()
conn.close()

###############################################################################
# Part 2b - Python version of i (find tweets with id_str containing 44 or 77)
#              and ii (number of unique in_reply_to_user_id entries)
###############################################################################

# Find tweet id_str containing 44 or 77
startTime = time.time()
tweetFile = open('Final_Tweets.txt','rb')

idList = []
for x in range(totNumTweets):

    line = tweetFile.readline()
    tweet = line.decode('UTF8')
    
    try:
        tDict = json.loads(tweet)
    except:
        if tweet != '':
            print('JSON Conversion Error found in tweet ',x)
            continue
        
    if ('44' in tDict["id_str"] or '77' in tDict["id_str"]) and (tDict["id_str"] not in idList):
        idList.append(tDict["id_str"])

tweetFile.close()

endTime = time.time()
elapsedTime = endTime - startTime
print('\n')
print('Number of tweet ID strings with either 44 of 77 in them from file using Python:',len(idList))
if len(idList) > 0:
    if len(selTbl) < 5:
        printNum = len(selTbl)
    else:
        printNum = 5
    print('The first', printNum, 'tweet IDs with 44 or 77 in them read from file using Python are:')
    for x in range(printNum):
        print('Matching entry', x, 'ID =', selTbl[x][0])
print('Time to run query to find tweet id_str entriess with 44 or 77 from file using Python: {:.10f} seconds.'.format(elapsedTime))
       
# Find number of unique in_reply_to_user_id entries
startTime = time.time()
tweetFile = open('Final_Tweets.txt', 'rb')

inReplyIdList = []
for x in range(totNumTweets):

    line = tweetFile.readline()
    tweet = line.decode('UTF8')
    
    try:
        tDict = json.loads(tweet)
    except:
        if tweet != '':
            print('JSON Conversion Error found in tweet ',x)
            continue
        
    if tDict["in_reply_to_user_id"] not in inReplyIdList and str(tDict["in_reply_to_user_id"]).isnumeric():
        inReplyIdList.append(tDict["in_reply_to_user_id"])

tweetFile.close()

endTime = time.time()
elapsedTime = endTime - startTime
print('\n')
print('Number of unique in_reply_to_user_id entries from file using Python:',len(inReplyIdList))
print('Time to run query to find number of unique in_reply_to_user_id entries from file using Python: {:.10f} seconds.'.format(elapsedTime))
    



