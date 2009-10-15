#!/usr/bin/env python
import sys
import os

def addBookmark(url, keyword, title):
        f = open(os.path.join(os.environ['XDG_DATA_HOME'],'uzbl/keywordBookmarks.txt'), 'a')
        f.write("%s %s %s\n" % (url, keyword, title))
        f.close()
        createBookmarkpage();

def getBookmark(keyword):
        f = open(os.path.join(os.environ['XDG_DATA_HOME'],'uzbl/keywordBookmarks.txt'), 'r')
        newKeyword = " %s" % keyword 
        for line in f:
                words = line.split(" ", 2)
                if(words[1] == keyword):
                        return words[0]

def createBookmarkpage():
        f = open(os.path.join(os.environ['XDG_DATA_HOME'],'uzbl/keywordBookmarks.html'), 'w')
        htmlStart = """ <html>
        <head>
                <title>Bookmarks</title>
                <link href="bookmarks.css" rel="stylesheet" type="text/css" />
        </head>
        <body>
                <h1>Bookmarks</h1>
        """
        f.write(htmlStart);
        bookmarks = open(os.path.join(os.environ['XDG_DATA_HOME'],'uzbl/keywordBookmarks.txt'), 'r')
        for line in bookmarks:
                bmark = line.split(" ", 2);
                base = bmark[0].split("/", 3);
                print "http://%s/favicon.ico" % base[2]
                f.write("\t\t<p class=\"link\"><a href=%s><img src=\"http://%s/favicon.ico\"> %s</a></p> <p class=\"keyword\">%s</p><br />\n" % (bmark[0],base[2],bmark[2],bmark[1]))
        f.write("</body>\n</html>")
        f.close()

newurl = getBookmark(sys.argv[9])

if(sys.argv[8] == "add" and newurl == None):
        addBookmark(sys.argv[6], sys.argv[9], sys.argv[7])
elif(sys.argv[8] == "get"): 
        if newurl != None:
                os.system("uzblctrl -s %s -c \"uri %s\"" % (sys.argv[5], newurl))
elif(sys.argv[8] == "open"):
        if newurl != None:
                os.system("uzblctrl -s %s -c \"uri %s\"" % (sys.argv[5], newurl))
        else:
                os.system("uzblctrl -s %s -c \"uri %s\"" % (sys.argv[5], sys.argv[9]))
elif(sys.argv[8] == "tab"):
        if newurl != None:
                os.system("uzbl --uri %s" % newurl)
        else:
                os.system("uzbl --uri %s" % sys.argv[9])
