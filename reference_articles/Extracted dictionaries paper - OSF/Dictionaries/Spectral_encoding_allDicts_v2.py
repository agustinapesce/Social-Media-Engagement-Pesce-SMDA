#Dict-based spectral encoding of given traits based on dictionary word-matching!
#Dominic Burkart (edited by Julian Wills)

#use: take in all given .txt wordlists in the directory this file is saved infor a given
#   trait (eg emotionality) and encode a given set of tweets with the wordcount from the
#   given dictionary and the ratio of words in the tweet also in the dictionary/total #
#   of words in thte tweet.

#for MEC project current version: 20 November 2015 (version 6)
#assumes that words in tweet are separated by spaces/punctuation to allow for tokenization
#no error checking for faulty input. 

#get filepath for data + content index
inputfiledir = input("data file directory: ") #"C:/Users/Julian/GDrive/1 Twitter Project/Julian/MEC/M-E-P-N_encoding/without_emoji/tweet_in.csv"
tw_content_indx = int(input("tweet text index in input file (10 for summer data, 7 for latest, 4 for processed): "))
print("\n")

#using standard modules
import csv
import os

#code for cleaning up strings (in dictionaries and in tweets)
punctuation = '''!"#$%&'()*+,-./:;<=>?[\]^_`{|}~'''#missing @ at the request of Julian
def clean(instring, spaces = True): #removes punctuation and double spaces, replacing them w/ single spaces
    instring.replace("\n"," ")
    for x in punctuation:
            instring = instring.replace(x, " ")
    if spaces:
        while instring.find("  ") > -1:
            instring = instring.replace("  ", " ")
    else:
        while instring.find(" ") > -1:
            instring = instring.replace(" ","")
    instring = instring.lower()
    return instring

#gets dictionaries
curlist = os.listdir(os.getcwd())
temp = []
wordlists = [] #will hold individual words
stemlists = [] #will hold stems (eg funn*)
listnames = [] #will hold the names of keyword files (to be used as variable names)
i = 0
for fname in curlist:
    if fname.endswith(".txt"): #new list of keywords!
        wordlists.append([])
        stemlists.append([])
        temp.append(open(fname, encoding = "utf-8").read().splitlines())
        i_of_x = 0
        for x in temp[i]:
            if temp[i][i_of_x].find("*") > -1:
                stemlists[i].append(clean(temp[i][i_of_x], spaces = False))
            else:
                wordlists[i].append(clean(temp[i][i_of_x], spaces = False))
            i_of_x += 1
        uncheckedSpace = True
        uncheckedBlank = True
        while uncheckedSpace or uncheckedBlank:
            try:
                wordlists[i].remove(" ")
            except ValueError:
                uncheckedSpace = False
            try:
                wordlists[i].remove("")
            except ValueError:
                uncheckedBlank = False
        print("Imported dictionary: "+fname)
        i += 1
        listnames.append(fname.split(".")[0])
print("\n")

#creates list of output datafield names based on wordlist file names
temp = []
for x in listnames:
    temp.append(x+"Count")
for x in listnames:
    temp.append(x+"Ratio")
temp.append("shared")
listnames = temp

#removes duplicates and redundant stems
for x in range(len(wordlists)):
    for word in wordlists[x][:]:
        if any(word.startswith(stem) for stem in stemlists[x]):
            wordlists[x].remove(word)
    wordlists[x] = set(wordlists[x])

#opens our data and output files
indoc = open(inputfiledir, encoding = "utf-8")
outdoc= csv.writer(open("no_emoji_out.csv", mode = "w", encoding = "utf-8"), lineterminator ="\n")

#takes a line from the in data and encodes it
def findInTweet(line, wordlists):
    content = clean(line[tw_content_indx]).split(" ")
    counts = []
    ratios = []
    shared = []
    sharedStem = []
    for x in range(len(content)):
        shared.append(0)
        sharedStem.append(0)
    for x in range(len(wordlists)):
        counts.append(0) #populates number of variables (eg emotionality)
        ratios.append(0)
    listidx = 0 #keep track of which dictionary
    for lists in wordlists: #start by grabbing words
        for word in lists:
            counts[wordlists.index(lists)] += content.count(word)
            if (content.count(word)> 0) and (listidx < 2): #limit shared count to moral and affect dicts.
                indices = [i for i, token in enumerate(content) if token == word]
                for i in indices:
                    shared[i] += 1 #add 1 to token index associated w/ moral and affect
        listidx += 1
    listidx = 0
    for lists in stemlists:
        for stem in lists:
            idx = 0 #needed since token indexing will confuse repeat tokens
            for token in content:
                if token.startswith(stem):
                    counts[stemlists.index(lists)] += 1
                    if listidx < 2:
                        sharedStem[idx] += 1 #add 1 to stem index associated w/ moral and affect
                idx += 1
        listidx += 1
    for x in range(len(counts)): #same as len(wordlists)
        ratios[x] = counts[x]/len(content)
    sharedFull = [x + y for x, y in zip(shared, sharedStem)]
    sharedCount = sum(s > 1 for s in sharedFull);
    counts[:] = [x - sharedCount for x in counts] #remove shared words from other two columns
    line.extend(counts)
    line.extend(ratios)
    line.append(sharedCount) #return number of words counted in both lists
    outdoc.writerow(line)
      
#iterates through the input file, calling the methods to find and write output.
cnt = 0 # counter loop to indicate progress
inheader = True 
for line in csv.reader(indoc):
    cnt += 1
    if inheader: #to copy over header to the new doc + add the new columns :)
        line.extend(listnames)
        outdoc.writerow(line)
        print("populating output file, please wait.")
        inheader = False
    else: #to count words + ratios for each tweet and then right those values to out :)
        findInTweet(line,wordlists)
        if cnt%500==0:
            print(cnt)

print("\nencoding complete.")
        

                
