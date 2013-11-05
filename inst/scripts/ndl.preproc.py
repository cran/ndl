#!/usr/bin/python
#
# This pre-processing script is a sample of a pre-processor for the ndl package in R
# by Cyrus Shaoul, version 0.01, April 1st, 2013 
#
# Features: setting the context size on the command line... size 1, 2 or 3 are reccomended.
# Also: Unicode characters are supported, but support for non-alphabetic scripts is 
# not available yet.

# Main object that creates NDL events from a text corpus.
class ndlpreproc(object):

  # parameter n is the size of the desired window
  def __init__(self, w, n):
    self.w = w
    self.n = n
    self.events = dict()

  # feed method calls tokenize to break the given string up into units
  def tokenize(self, text):
    clean = ' '.join(text.split())
    return clean.split(" ")

  # Extract letter n-gram cues from text. Add the word separator "#".
  def ortho(self, text, size):
    text = u'#' + text.replace(u'_',u'#') + u'#'
    g = []
    for i in range(len(text) - size + 1):
      g.append(text[i:i+size])
    cues = u'_'.join(g)
    return cues 

  # feed method takes text, tokenizes it, and visits every group of n tokens
  # in turn, adding the group to self.events or incrementing count in same
  def feed(self, text):
    tokens = self.tokenize(unicode(text,"utf-8"))
    for i in range(len(tokens) - self.w + 1):
      outcomes = u'_'.join(tokens[i:i+self.w])
      event = (outcomes , self.ortho(outcomes,self.n))
      if event in self.events:
        self.events[event] += 1
      else:
        self.events[event] = 1

  def get_events(self):
    return self.events

# Return scipt usage error message
def usage():
  print("Usage: ndl.preproc.py [W] [N] < corpus > preprocessed.corpus\nWhere W is an integer that represents the context window size in words and\nN is the size of the letter cues (1, 2 or 3).\n")

############################################################################
#        Main Program
############################################################################
if __name__ == '__main__':
  import sys

  # Test for valid input, two arguments, both integers in the correct range.
  try:
    w = int(sys.argv[1])
    n = int(sys.argv[2])
    arglength = len(sys.argv)
    # Check for valid arguments
    if ((w >= 0 and w < 10) and (n >= 1 and n <= 3) and arglength == 3):
      # create an ndlpreproc object and feed data to it
      sys.stderr.write("Starting to process corpus from the standard input.\n")
      ndl = ndlpreproc(w,n)
      print("Cues\tOutcomes\tFrequency")
      for line in sys.stdin:
        line = line.strip()
        ndl.feed(line)
      # get events from input. Print these to standard output in the NDL format.
      events = ndl.get_events()
      for event in events.keys():
        count = events[event]
        output= u"%s\t%s\t%d" % ( event[1], event[0], count)
        print(output.encode("utf-8"))
      sys.stderr.write("Preprocessing is complete.\n")
    else:
      print("**** There was a problem with required numerical arguments W (%d) and/or N (%d)." % (w,n))  
      usage()
  except:
    usage()
