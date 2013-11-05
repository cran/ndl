#include <omp.h>
#include <cstdio>
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <fstream>
#include <iterator>
#include <set>
#include <vector>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <dirent.h>
#include <RcppCommon.h>
#include <Rcpp.h>

//Removed due to issues with MacOsX and Windoze
//#include <sys/sysinfo.h>

// Namespaces to search
using namespace std;
using namespace Rcpp;

// Associative maps
typedef map<string,size_t> Dict;
typedef map<size_t,size_t> CMap;

// The core event data structure: a pair of variable-length vectors of unsigned integers
struct Event
{
  vector<size_t> Cues;
  vector<size_t> Outcomes;
};

// Another core data structure: An ordered list of Events
typedef vector<Event> Events;

// equality operator for Events
bool operator==(const Event& lhs, const Event& rhs)
{
  return lhs.Cues==rhs.Cues && lhs.Outcomes==rhs.Outcomes;
}

// Cyrus says: Yes, this is my own non-portable binary data
// serialization scheme.  I created it to avoid any dependencies on
// external libraries, such as BOOST_SERIALIZATION.
// 
// WARNING: It is completely platform dependent. Please do not attempt
// to read in files created on a different system. It might not work
// due to byte ordering issues or unsigned integer length issues.
//
// To implement this format yourself, here is the what you need:
// The size of an unsigned integer. On most modern systems this is 4 bytes.
//
// The first 4 bytes are the number of events in the file (NumEvents)
// After that, you follow the following steps:
// Read the next 4 bytes. That is the number of Cues in the event (NumCues).
// Then read that many numbers, 4 bytes each. Each number is a CueID
// The next 4-bytes is the number of Outcomes (NumOutcomes).
// Next read that many numbers. Each number is an OutcomeID.
// The next 4 bytes is the number of cues in the next event, and so on.
// Repeat until you have read all the events (NumEvents of them).
// CueID and OutcomeID files should be stored separately.
//
Events readEvents(const string ifilename) {
  Events myEvents;
  myEvents.clear();
  ifstream is(ifilename.c_str(), ios::in | ios::binary);
  size_t i,j,NumCues,NumOutcomes,NumEvents,num;
  is.read(reinterpret_cast<char *>(&num), sizeof(num));
  NumEvents = num;
  Event myEvent;
  for (i = 0; i < NumEvents; i++) {
    is.read(reinterpret_cast<char *>(&num), sizeof(num));
    NumCues = num;
    for (j = 0; j < NumCues; j++) {
      is.read(reinterpret_cast<char *>(&num), sizeof(num));
      myEvent.Cues.push_back(num);
    }
    is.read(reinterpret_cast<char *>(&num), sizeof(num));
    NumOutcomes = num;
    for (j = 0; j < NumOutcomes; j++) {
      is.read(reinterpret_cast<char *>(&num), sizeof(num));
      myEvent.Outcomes.push_back(num);
    }
    myEvents.push_back(myEvent);
    myEvent.Cues.clear();
    myEvent.Outcomes.clear();
  }
  is.close();
  return(myEvents);
}


// Convert a string to an Interger
static size_t stringToInt(const string& str, const bool verbose)
{
    char * pEnd;
    const char * c_str = str.c_str();
    size_t result = static_cast<size_t>(strtol(c_str, &pEnd, 10));
    // If there are no leftover parts of the string after conversion
    if (pEnd == c_str+str.length()) {
        return result;
    }
    // else
    if (verbose) Rcerr << "While reading input: '"  <<  str  << "' is not an number!\n";
    stop("Ending Exectution");
}

// Split a string on a character delimiter
vector<string> &split(const string &s, char delim, vector<string> &elems) {
    stringstream ss(s);
    string item;
    while(getline(ss, item, delim)) {
        elems.push_back(item);
    }
    return elems;
}

// wrapper for split function
vector<string> split(const string &s, char delim) {
    vector<string> elems;
    return split(s, delim, elems);
}

// Extract a vector of ID's from a set of text-based cues
static vector<size_t> extract(string item, Dict D) { 
  vector<size_t> output;
  vector<string> elems = split(item,'_');
  for (size_t i = 0; i < elems.size(); i++) {
    output.push_back(D[elems[i]]);
  }
  return(output);
}

// Process Event Data stored in the legacy format (three vectors,
// Cues, Outcomes and Freq)
static void ProcessLegacyData(
			      NumericMatrix& CoMat, 
			      NumericMatrix& CuMat, 
			      Dict& CueDict, 
			      Dict& OutcomeDict, 
			      StringVector& Cues, 
			      StringVector& Outcomes, 
			      NumericVector& Frequency, 
			      const bool RemoveDuplicates, 
			      const bool verbose)
{
  vector<size_t> EvCues, EvOutcomes;
  string CueStr, OutcomeStr;
  size_t h,i,j,lenCues, lenOutcomes;
  size_t count = 0;
  int id;
  if (verbose) Rcerr << "Starting to Process Events. Number of events processed:\n";
  for (h=0; h < Frequency.size(); h++) {
    //    Rcerr << "Working on Event #" << h << endl;
    count++;
    if ((count +1) % 1000 == 0) {
      if (verbose) Rcerr << count + 1 << " ";
    }
    CueStr = Cues[h];
    EvCues = extract(CueStr,CueDict);
    OutcomeStr= Outcomes[h];
    EvOutcomes = extract(OutcomeStr,OutcomeDict);
    //  remove duplicate outcomes
    sort( EvOutcomes.begin(), EvOutcomes.end() );
    EvOutcomes.erase( unique( EvOutcomes.begin(), EvOutcomes.end() ), EvOutcomes.end() );
    // If requested, remove duplicate cues
    if (RemoveDuplicates) {
      sort( EvCues.begin(), EvCues.end() );
      EvCues.erase( unique( EvCues.begin(), EvCues.end() ), EvCues.end() );
    }
    // Find all cue-cue combinations and add them to the CueMat
    lenCues = EvCues.size();
    for (i = 0; i < lenCues; i++)  {
      for (j = i; j < lenCues; j++)  {
	//Increase count for this cue-cue co-occurrene, including Frequency info
	if ((EvCues[i] == EvCues[j]) & (i!=j)) {
	  continue;
	}
	CuMat(EvCues[i],EvCues[j]) = CuMat(EvCues[i],EvCues[j]) + Frequency[h];
	if (EvCues[i] != EvCues[j]) {
	  CuMat(EvCues[j],EvCues[i]) = CuMat(EvCues[i],EvCues[j]);
	} 
	//	Rcerr << "At: " << i << "," << j <<": Added cooc for cues: " << EvCues[j] << " with " << EvCues[i] << ",new count=" << CuMat(EvCues[i],EvCues[j])  << endl;
      }
    }
    // Find all the cue-outcome combinations and add them to the CoMat
    lenOutcomes = EvOutcomes.size();
    for (i = 0; i < lenCues; i++)	    {
      for (j = 0; j < lenOutcomes; j++)		{
	//Increase count for this cue-outcome co-occurrence, including Frequency info
	CoMat(EvCues[i],EvOutcomes[j]) = CoMat(EvCues[i],EvOutcomes[j]) + Frequency[h];
	//	Rcerr << "Added cooc for cue and outcome: " << EvCues[i] << " with " << EvOutcomes[j] << endl;
      }
    }
    // add background rates to Environ... 
    for (j = 0; j < lenOutcomes; j++) {
      CoMat(CueDict["Environ"],EvOutcomes[j]) += Frequency[h];
      //      Rcerr << "Added background : " << Frequency[h] << " to " << EvOutcomes[j]  << " environ = " << CueDict["Environ"] << endl;
    }
  }
  if (verbose) Rcerr << endl;
}

// Process data stored in the new binary, serialized format.
static void ProcessData( vector<string> files, 
			 NumericMatrix& CoMat, 
			 NumericMatrix& CuMat, 
			 const bool RemoveDuplicates, 
			 const bool verbose, 
			 const size_t MaxEvents, 
			 CMap& CueMap, 
			 CMap& OutcomeMap, 
			 const bool addBackground, 
			 NumericVector& CoFreq ){
  // Local Variables
  vector<size_t> EvCues, EvOutcomes;
  string CueStr, OutcomeStr;
  size_t g,h,i,j,lenCues, lenOutcomes;
  size_t count = 0;
  Events theEvents;
  if (verbose) Rcerr <<  "Starting to Process Event Files:"
		     << " (One dot per million events.)\n";
  // loop over all the event data files
  for (g = 0; g < files.size(); g++) {
    // Stop adding events if we are over our event limit.
    if (count > MaxEvents) {
      if (verbose) Rcerr << "Hit MaxEvents. Stopping learning at " 
			 << count << " events." << endl << flush;
      break;
    }
    if (verbose) Rcerr << g << " ";
    // read the current set of events.
    theEvents = readEvents(files[g]);
    // Loop through a series of events.
    const size_t numEvents = theEvents.size();
    //    Rcerr << "Size of theEvents =  " << numEvents << endl;
    for (h = 0; h < numEvents; h++) {
      count++;
      if (count % 1000000 == 0) {
	if (verbose) Rcerr << "." << flush;
      }
      EvCues = theEvents[h].Cues;
      EvOutcomes = theEvents[h].Outcomes;
      // remove duplicate outcomes.
      sort( EvOutcomes.begin(), EvOutcomes.end() );
      EvOutcomes.erase( unique( EvOutcomes.begin(), EvOutcomes.end() ), 
			EvOutcomes.end() );
      // If requested, remove duplicate cues
      if (RemoveDuplicates) {
	sort( EvCues.begin(), EvCues.end() );
	EvCues.erase( unique( EvCues.begin(), EvCues.end() ), 
		      EvCues.end() );
      }
      lenCues = EvCues.size();      
      //      Rcerr << "Length of Event Cues=" << lenCues << endl;
      lenOutcomes = EvOutcomes.size();
      //      Rcerr << "Length of Outcomes=" << lenOutcomes << endl;
      // Remap Cues
      for (i = 0; i < lenCues; i++) {
	//	Rcerr << "Mapping " << EvCues[i] << " to " << CueMap[EvCues[i]] << endl;
	EvCues[i] = CueMap[EvCues[i]];
      }
      // Remap Outcomes
      for (i = 0; i < lenOutcomes; i++) {
	//	Rcerr << "Mapping " << EvOutcomes[i] << " to " << OutcomeMap[EvOutcomes[i]] << endl;
	EvOutcomes[i] = OutcomeMap[EvOutcomes[i]];
      }
      // Find all cue-cue combinations and add them to the CueMat
      for (i = 0; i < lenCues; i++) {
	for (j = i; j < lenCues; j++) {
	  // Only count identical cues once
	  if ((EvCues[i] == EvCues[j]) & (i != j)) {
	    continue;
	  }
	  CuMat(EvCues[i],EvCues[j])++;
	  if (EvCues[i] != EvCues[j]) {
	    CuMat(EvCues[j],EvCues[i])++;
	  }
	}
      }
      for (i = 0; i < lenCues; i++) {
	  for (j = 0; j < lenOutcomes; j++) {
	    //Increase count for this cue-cue co-occurrence
	    CoMat(EvCues[i],EvOutcomes[j])++;
	  }
      }
      // Add background rates to Environ, if requested
      if (addBackground) {
	for (j = 0; j < lenOutcomes; j++) {
	  CoFreq(EvOutcomes[j])++;
	}
      }
    }
    theEvents.clear();
  }
  if (verbose) Rcerr << endl << "Processed a total of " << count << " discrete events." << endl;
}


// Extract all the unique elements from a vector and return them in a vector.
void extractUnique(StringVector &items, Dict &output, StringVector &Uniq) {
  set<string> uniq;
  vector<string> elems;
  string str;
  // split all strings on the underscore and insert into a set (forcing uniqueness).
  for (size_t i=0; i < items.size(); i++) {
    str = items[i];
    elems = split(str,'_');
    copy( elems.begin(), elems.end(), inserter( uniq, uniq.end() ) );
  }
  set<string>::iterator theIterator;
  size_t id = 0;
  // return a list of items with sequential id numbers.
  for( theIterator = uniq.begin(); theIterator != uniq.end(); theIterator++ ) {
    output[*theIterator] = id;
    id++;
    Uniq.push_back(*theIterator);
  }
}

StringVector readInput(const string filename, CMap& MyMap) {
  StringVector output;
  size_t ItemID = 0;
  string Item = "";
  string line = "";
  ifstream ifs(filename.c_str());
  size_t count=0;
  vector<string> elems;
  while ( getline(ifs,line) )  {
    if (ifs.fail()) {
      ostringstream buffer;
      buffer << "Error Reading Input File: " << filename;
      stop(buffer.str());
    }
    elems = split(line,'\t');
    if (elems.size() != 2) {
      ostringstream buffer;
      buffer << "Bad formating in file: " << filename 
	     << ", line # " <<  count  
	     << ". Please check input and try again."<< endl;
      stop(buffer.str());
      }
    Item = elems[0];
    ItemID = stringToInt(elems[1],true);
    output.push_back(Item);
    MyMap[ItemID] = count;
    count++;
    line.clear();
  }
  ifs.close();
  return(output);
}


// get all files in a directory
void getDir (string dir, vector<string> &files)
{
    DIR *dp;
    struct dirent *dirp;
    if((dp = opendir(dir.c_str())) == NULL) {
      ostringstream buffer;
      buffer << "Fatal error opening directory " << dir << " . Does the directory exist?";
      stop(buffer.str());
    }
    while ((dirp = readdir(dp)) != NULL) {
      string path = dir + "/" + string(dirp->d_name);
      size_t found = path.find(".dat");
      if (found!=string::npos) {
	files.push_back(path);
      }
    }
    closedir(dp);
}

// Learn the cue-outcome relationships from individual exposure events.

// [[Rcpp::export]]
List learn(std::string data, const bool RemoveDuplicates, const bool verbose, const size_t MaxEvents, const bool addBackground)
{
  if (verbose) Rcerr << "Entering c++ learning module." << endl;  // main loop

  try
    {
      // Start timing execution.
      //      time_t start = time(NULL);

      StringVector Cues, Outcomes;
      string CueFile = data + ".cues";
      string OutcomeFile = data + ".outcomes";
      string EventDir = data + ".events";
      CMap CueMap;
      CMap OutcomeMap;

      Cues = readInput(CueFile,CueMap);
      Outcomes = readInput(OutcomeFile,OutcomeMap);

      size_t NumCues = Cues.size();
      size_t NumOutcomes = Outcomes.size();
      if (verbose)  Rcerr << "Loaded the data and got " << NumCues 
			   << " unique cues and " << NumOutcomes 
			   << " unique outcomes.\n"; 
      //Initialize matrices
      NumericMatrix CoMat(NumCues,NumOutcomes);
      if (verbose) Rcerr << "Created a Cue-Outcome matrix that is " 
			 << NumCues << " by " << NumOutcomes 
			 << " in size." << endl;
      NumericMatrix CuMat(NumCues,NumCues);
      if (verbose) Rcerr << "Created a Cue-Cue matrix that is " 
			 << NumCues << " by " << NumCues 
			 << " in size." << endl;

      NumericVector CoFreq(NumOutcomes);

      // Get list of event files
      string dir = EventDir;
      vector<string> files = vector<string>();
      getDir(dir,files);
      // Main loop to process learning events.
      ProcessData(files, CoMat, CuMat, RemoveDuplicates, 
		  verbose, MaxEvents, CueMap, 
		  OutcomeMap, addBackground, CoFreq);

      // Create dimname lists
      List CuDims = List::create(Cues, Cues);
      List CoDims = List::create(Cues, Outcomes);

      // and assign it to the matrices
      CuMat.attr("dimnames") = CuDims;
      CoMat.attr("dimnames") = CoDims;

      if (verbose) Rcerr << "Finished processing learning events." << endl;
      if (verbose) Rcerr << "Leaving c++ learning module." << endl;

      return List::create(Named("CueCue") = CuMat,
			  Named("CueOutcome") = CoMat,
			  Named("OutcomeFreq") = CoFreq
			  );
    }
  catch (const bad_alloc& x)
    {
      Rcerr << " Out of memory error: " << x.what() << endl;
      return List::create(1);
    }
  catch(const std::exception &ex ) 
    {             // or use END_RCPP macro
    forward_exception_to_r( ex );
    } 
  catch(...) 
    { 
    ::Rf_error( "c++ exception (unknown reason)" ); 
    }
}

// Use the legacy format, accept a DataFrame and return the correct Matrices.

// [[Rcpp::export]]
List learnLegacy(SEXP DFin, const bool RemoveDuplicates, const bool verbose)
{
  try
    {
      // Start timing execution.
      //      time_t start = time(NULL);

      if (verbose) Rcerr << "Entering c++ learning module." << endl;
      if (verbose) Rcerr << "Making Lists of unique Cue and Outcome types.\n";
      // Get incoming DataFrame
      DataFrame DF = DataFrame(DFin);
      StringVector Cues = DF["Cues"];
      StringVector Outcomes = DF["Outcomes"];
      NumericVector Frequency = DF["Frequency"];
      // Maps to look up strings
      Dict UniqCues, UniqOutcomes;
      // Vector to hold IDs
      StringVector CueList, OutcomeList;
      // Extract Cues from the dataframe.
      extractUnique(Cues,UniqCues,CueList);
      // add background rate Cue to Cue List.
      CueList.push_back("Environ");
      // index for Environ Cue....
      UniqCues["Environ"] = UniqCues.size() - 1;
      // Extract Outcomes from the dataframe.
      extractUnique(Outcomes,UniqOutcomes,OutcomeList);

      size_t NumCues = UniqCues.size();
      size_t NumOutcomes = UniqOutcomes.size();
      if (verbose) Rcerr << "Loaded data and got " << NumCues << " unique cues and " << NumOutcomes << " unique outcomes.\n";

      if (verbose) Rcerr << "Starting Memory allocation for the Cue-Outcome matrix. "  << endl;
      NumericMatrix CoMat(NumCues,NumOutcomes);
      if (verbose) Rcerr << "Created a Cue-Out matrix that is " << NumCues << " by " << NumOutcomes << " in size." << endl;
      
      if (verbose) Rcerr << "Starting Memory allocation for the Cue-Cue matrix. "  << endl;
      // //Initialize matrix
      NumericMatrix CuMat(NumCues,NumCues);
      if (verbose) Rcerr << "Created a Cue-Cue matrix that is " << NumCues << " by " << NumCues << " in size." << endl;
      
      // // Main loop to process learning events.
      ProcessLegacyData(CoMat, CuMat, UniqCues, UniqOutcomes, Cues, Outcomes, Frequency, RemoveDuplicates, verbose);
 
      //add row and column names from these Lists.      
      List CuDims = List::create(CueList, CueList);
      List CoDims = List::create(CueList, OutcomeList);

      // and assign it to the matrix 2x
      CuMat.attr("dimnames") = CuDims;
      CoMat.attr("dimnames") = CoDims;

      if (verbose) Rcerr << "Finished processing all learning events" << endl;
      //      if (verbose) Rcerr << "Processing All events took: " <<  (time(NULL) - start) / 60.0 << " minutes. (Walltime)"<<  endl;
      if (verbose) Rcerr << "Leaving c++ learning module." << endl;

      return List::create(Named("CueCue") = CuMat,
			  Named("CueOutcome") = CoMat) ;

    }
  catch (const bad_alloc& x)
    {
      Rcerr << " Out of memory error: " << x.what() << endl;
    }
  catch(const std::exception &ex ) 
    {             // or use END_RCPP macro
      forward_exception_to_r( ex );
    } 
  catch(...) 
    { 
      ::Rf_error( "c++ exception (unknown reason)" ); 
    }
}



// Read in the tsv files
// static StringVector readFile(string filename) {
//   ifstream inFile(filename.c_str()); 
//   size_t mycount = count(istreambuf_iterator<char>(inFile), 
// 			 istreambuf_iterator<char>(), '\n');
//   inFile.close();
//   string line;
//   vector<string> elems;
//   StringVector output(mycount);
//   unsigned int wordID;
//   char wordC[100];
//   int result;
//   ifstream ifs(filename.c_str());
//   while( getline(ifs, line) ) {
//     result = sscanf(line.c_str(), "%[^\t]\t%u", wordC, &wordID);
//     wordID = static_cast<size_t>(wordID);
//     if (result == 2) {
//       string word = string(wordC);
//       if (word != "") {
// 	output[wordID] = word;
//       }
//       //      cout << "Got " << output[wordID] << " with ID " << wordID << endl; 
//     } else {
//       Rcerr << "Bad formating in file: " << filename << " . Cannot continue. Here is the problematic line: "<< endl;
//       Rcerr << line << endl;
//       throw 20;
//     }
//   } 
//   ifs.close();
//   return(output);
// }


// // deprecated.
// static StringVector OldreadFile(string filename) {
//   // count number of items.
//   ifstream inFile(filename.c_str()); 
//   size_t mycount = count(istreambuf_iterator<char>(inFile), 
// 			 istreambuf_iterator<char>(), '\n');
//   inFile.close();
//   StringVector output(mycount);
//   string word;
//   size_t wordID;
//   ifstream ifs(filename.c_str());
//   do {
//     // get UTF8 encoded string
//     ifs >> word;
//     // get item id
//     ifs >> wordID;
//     // place the item in the correct place in the vector
//     output[wordID] = word;
//     cout << "Got " << word << " with ID " << wordID << endl; 
//   } while(ifs);
//   return(output);
// }
