###############################################################################
#  CSC 455 Spring 2017
#  Part 1
#  Kari Palmier
###############################################################################

import urllib
import json
import sqlite3
import time

totNumTweets = 1000000
webUrl = "http://rasinsrv07.cstcis.cti.depaul.edu/CSC455/OneDayOfTweets.txt"


###############################################################################
#   Part 1a - Creating Tables - Note that this is called in another function
###############################################################################
def createTables(dbCursor):
    # Drop Tweets table if it already exists in the database
    try:
        dbCursor.execute("DROP TABLE Tweets;")
    except:
        print ("Drop Tweets table command skipped")
    
    # Drop Users table if it already exists in the database
    try:
        dbCursor.execute("DROP TABLE Users;")
    except:
        print ("Drop Users table command skipped")
    
        # Drop Tweets table if it already exists in the database
    try:
        dbCursor.execute("DROP TABLE Geo;")
    except:
        print ("Drop Geo table command skipped")
    
    # Create string to create Users table in SQLite database
    createUsersTable = """ CREATE TABLE Users(
                         id                INTEGER,
                         name              VARCHAR(75),
                         screen_name       VARCHAR(75),
                         description       VARCHAR(150),
                         friends_count     INTEGER,
    
                         CONSTRAINT UsersPK PRIMARY KEY(id)
                      );"""
    
    try:
        dbCursor.execute(createUsersTable)
    except:
        print ("Create Users table command skipped")
    
    # Create string to create Users table in SQLite database
    # Only saw point Geo types but made larger just in case some were present I didn't see
    createGeoTable = """ CREATE TABLE Geo(
                         tweet_id_str      CHAR(18),
                         type              VARCHAR(20), 
                         latitude          REAL,
                         longitude         REAL,
    
                         CONSTRAINT GeoPK PRIMARY KEY(tweet_id_str)
                      );"""
    
    try:
        dbCursor.execute(createGeoTable)
    except:
        print ("Create Geo table command skipped")
    
    # Create string to create Tweets table in SQLite database
    # Left date as a string due to how it was formatted
    createTweetsTable = """ CREATE TABLE Tweets(
                         created_at                VARCHAR(35),
                         id_str                    CHAR(18),
                         text                      VARCHAR(140),
                         source                    VARCHAR(60),
                         in_reply_to_user_id       INTEGER,
                         in_reply_to_screen_name   VARCHAR(50),
                         in_reply_to_status_id     INTEGER,
                         retweet_count             INTEGER,
                         contributors              VARCHAR(200),
                         user_id                   INTEGER,
                         geo_id_str                CHAR(18),
                         
                         CONSTRAINT TweetsPK PRIMARY KEY(id_str)
                         CONSTRAINT UserIdFK FOREIGN KEY(user_id)
                            REFERENCES Users(id)
                         CONSTRAINT GeoIdFK FOREIGN KEY(geo_id_str)
                            REFERENCES Geo(tweet_id_str)
                      );"""
    
    try:
        dbCursor.execute(createTweetsTable)
    except:
        print ("Create Tweets table command skipped")

def convertNoneParams(params):
    numParams = len(params)
    newParams = []
    for y in range(numParams):
        
        if params[y] in ['NULL', 'null', 'Null', 'NONE', 'none', 'None', '', 'NA', 'na', 'Na', []]:
            newParams.append(None)
        else:
            newParams.append(params[y])

    return newParams

def WriteTweetsToFile(url, fileName, numTweets):
    startTime = time.time()
    
    # Open a pointer to the website
    webFD=urllib.request.urlopen(url)
    
    # Open a pointer to the text file
    testFile = open(fileName,'wb')
    
    # Loop over the number of tweets 
    for x in range(numTweets):
        
        # Read next line of file as a bytes type
        line = webFD.readline()
        
        # write the line as a bytes type (writing to a binary file)
        testFile.write(line)
                  
    webFD.close()    
    testFile.close()
    
    endTime = time.time()
    
    elapsedTime = endTime - startTime
    return elapsedTime
    
 
def ReadTweetsFromFile(fileName, numTweets, numBatch):
    
    startTime = time.time()   
    
            
    if 'http' not in fileName:
        # Open a pointer to the text file
        inData = open(fileName,'rb')
    else:
        inData = urllib.request.urlopen(fileName)
    
    # Open SQLite database
    conn = sqlite3.connect('CSC455_Final.db')
    c = conn.cursor()
    
    createTables(c)
    
    # Loop over the number of tweets 
    tweetList = []
    userList = []
    geoList = []
    numJsonErrors = 0
    numErrors = 0
    numTotEntries = 0
    for x in range(numTweets):
        line = inData.readline()
        tweet = line.decode('UTF8')
        
        try:
            tDict = json.loads(tweet)
        except:
            if tweet != '':
                numJsonErrors += 1
                continue
            
        numTotEntries += 1
       
       # Extract attribute values desired
        tweetParams = [tDict["created_at"], tDict["id_str"], tDict["text"], 
                       tDict["source"], tDict["in_reply_to_user_id"],
                       tDict["in_reply_to_screen_name"], tDict["in_reply_to_status_id"],
                       tDict["retweet_count"], tDict["contributors"],
                       tDict['user']['id']]
        # If there is geo present, get the geo parameters and add to the geoList
        if tDict['geo'] != None:
            tweetParams.append(tDict["id_str"])
            geoParams = [tDict["id_str"], tDict['geo']['type'], tDict['geo']['coordinates'][0], 
                         tDict['geo']['coordinates'][1]]
            geoParams = convertNoneParams(geoParams)
            geoList.append(geoParams)
        # Else assign geo primary key to None
        else:
            tweetParams.append(None)
    
        # Format tweet parameters and add to tweetList
        tweetParams = convertNoneParams(tweetParams) 
        tweetList.append(tweetParams)
    
        # If the user ID is unique, get user parameters and add to userList
        userParams = [tDict['user']['id'],tDict['user']['name'],tDict['user']['screen_name'],
                      tDict['user']['description'],tDict['user']['friends_count']]
        userParams = convertNoneParams(userParams)
        userList.append(userParams)
        
        # If the number of tweets in tweetList is equal to the batch number, write to db
        if (len(tweetList) == numBatch) or ((x+1) == numTweets) :
            # Insert user table params into table - use into or ignore because 
            # the same user ID can be present in multiple tweets (same person tweeting)
            # If user ID is already present, current information is not added to table
            insertUserStr = 'INSERT OR IGNORE INTO Users VALUES(' + ', '.join(len(userList[0])*['?']) + ');'
            try:
                if numBatch == 1:
                    selRtn = c.execute(insertUserStr,userList[0])
                else:
                    selRtn = c.executemany(insertUserStr,userList)                
            except:
                numErrors += 1
            userList = []
        
            # Insert geo table params into table - use into or ignore because 
            # the same user ID can be present in multiple tweets (same person tweeting)
            # If user ID is already present, current information is not added to table
            if geoList != []:
                insertGeoStr = 'INSERT OR IGNORE INTO Geo VALUES(' + ', '.join(len(geoList[0])*['?']) + ');'
                try:
                    if numBatch == 1:
                        selRtn = c.execute(insertGeoStr,geoList[0])
                    else:
                        selRtn = c.executemany(insertGeoStr,geoList)                
                except:
                    numErrors += 1
                geoList = []
    
    
            # Insert tweet table params into table
            insertTweetStr = 'INSERT OR IGNORE INTO Tweets VALUES(' + ', '.join(len(tweetList[0])*['?']) + ');'
            try:
                if numBatch == 1:
                    selRtn = c.execute(insertTweetStr,tweetList[0])
                else:
                    selRtn = c.executemany(insertTweetStr,tweetList)                
            except:
                numErrors += 1
            tweetList = []
          
          
    inData.close()
    
    endTime = time.time()
    elapsedTime = endTime - startTime
    
    # Verify Tweet table contents
    selRtn = c.execute("SELECT * FROM Tweets;")
    selTbl = selRtn.fetchall()
    
    numRows = len(selTbl)
    print('\n')
    print('Number of rows in Tweets table returned by select all query:', numRows)
    
    # Print first 5 entries to verify syntax and rows written
    if numRows > 0:
        print('\n')
        print('The first 5 rows of the Tweets table are:')
        for x in range(5):
            print(selTbl[x])
    
    # Verify User table contents
    selRtn = c.execute("SELECT * FROM Users;")
    selTbl = selRtn.fetchall()
    
    numRows = len(selTbl)
    print('\n')
    print('Number of rows in Users table returned by select all query:', numRows)
    
    # Print first 5 entries to verify syntax and rows written
    if numRows > 0:
        print('\n')
        print('The first 5 rows of the Users table are:')
        for x in range(5):
            print(selTbl[x])
    
    # Verify Geo table contents
    selRtn = c.execute("SELECT * FROM Geo;")
    selTbl = selRtn.fetchall()
    
    numRows = len(selTbl)
    print('\n')
    print('Number of rows in Geo table returned by select all query:', numRows)
    
    # Print first 5 entries to verify syntax and rows written
    if numRows > 0:
        print('\n')
        print('The first 5 rows of the Geo table are:')
        for x in range(5):
            print(selTbl[x])
    
    c.close()
    conn.commit()
    conn.close()
      
    print('\n')
    print('Number of Tweets read:', numTotEntries)
    print('Number of Tweets entered into SQLite:', (numTotEntries - (numJsonErrors+numErrors)))
    print('Number of Tweets with JSON errors:',numJsonErrors)
    print('Number of Tweets with SQLite errors:', numErrors)
   
    return elapsedTime
   
###############################################################################
#   Part 1b - Download and write webpage tweets to local text file
###############################################################################
print('Downloading tweets from web and storing to a text file...')
writeTime = WriteTweetsToFile(webUrl, 'Final_Tweets.txt', totNumTweets)
print('Time to download tweets from webpage and save to text file:', writeTime, 'seconds.')

################################################################################
##   Part 1c - Download and write tweet info to SQLite
################################################################################
print('\n')
print('Reading tweets from web and storing to SQLite...')
dnloadSqlTime = ReadTweetsFromFile(webUrl, totNumTweets, 1)
print('Time to download tweets from webpage and save to SQLite:', dnloadSqlTime, 'seconds.')

################################################################################
##   Part 1d - Read tweets from text file and write tweet info to SQLite
################################################################################
print('\n')
print('Reading tweets from text file and storing to SQLite...')
readSqlTime = ReadTweetsFromFile('Final_Tweets.txt', totNumTweets, 1)
print('Time to upload tweets from text file and save to SQLite:', readSqlTime, 'seconds.')

###############################################################################
#   Part 1e - Read tweets from text file and write tweet info to SQLite
#             Write to SQL using batches of 1000
###############################################################################
print('\n')
print('Reading tweets from text file and storing to SQLite in batches of 1000...')
batchReadSqlTime = ReadTweetsFromFile('Final_Tweets.txt', totNumTweets, 1000)
print('Time to upload tweets from text file and save to SQLite using 1000 tweet batches:', batchReadSqlTime, 'seconds.')

###############################################################################
#   Report times per step
###############################################################################
print('\n')
print('Summary:')
print('Time to download tweets from webpage and save to text file:', writeTime, 'seconds.')
print('Time to download tweets from webpage and save to SQLite:', dnloadSqlTime, 'seconds.')
print('Time to upload tweets from text file and save to SQLite:', readSqlTime, 'seconds.')
print('Time to upload tweets from text file and save to SQLite using 1000 tweet batches:', batchReadSqlTime, 'seconds.')
