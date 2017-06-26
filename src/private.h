//#define __BETARELEASE__ 1  // comment this line out to disable beta option
//#define __BETAWILLEXPIRE__ 1 // comment this line out to disable expiration of beta
//#define __BETAEXPIRE_DAY__ 31
//#define __BETAEXPIRE_MONTH__ wxDateTime::Jul
//#define __BETAEXPIRE_YEAR__ 2017

// can be used to indicate specialized releases for particular testers, i.e. 'iscc-ge'
// by default, should be NULL
static const char *version_label = 0; //"iscc-ge"; 

// API keys for SAM to use with developer.nrel.gov services.
const char *sam_api_key =
"rJzFOTOJhNHcLOnPmW2TNCLV8I4HHLgKddAycGpn"   // production (sam.support@nrel.gov)
//"yXv3dcb6f5piO0abUMrrTuQvLDFgWvnBz52TJmDJ" // staging (aron.dobos@nrel.gov)
;


// Google APIs:
// login to developer api console at: https://code.google.com/apis/console
// user name: aron.dobos@nrel.gov
// passwd: 1H*****....******r
static const char *GOOGLE_API_KEY = "AIzaSyCyH4nHkZ7FhBK5xYg4db3K7WN-vhpDxas";

// Bing Map APIs:
// login to account center at: https://www.bingmapsportal.com/
// user name: aron.dobos@nrel.gov
// passwd: 1H*****....******r
static const char *BING_API_KEY = "Av0Op8DvYGR2w07w_771JLum7-fdry0kBtu3ZA4uu_9jBJOUZgPY7mdbWhVjiORY";


static const char *beta_disclaimer =
"Notice: Beta versions of SAM are provided as-is and may change without notice."
	"  Beta software may not work in the same way as a final version, and features and functionality may be changed, enhanced, or removed without notice."
	"  The software is considered generally stable and useful but may produce incorrect results, crash, or behave otherwise unexpectedly."
	"  There is no guarantee that files opened, saved, or created with this beta software will be usable with other versions of SAM."
	"  This notice appears in addition to the full disclaimer of warranty, accessible in Help/About."
	"\n\nThank you for trying SAM Beta.  We look forward to your feedback.";

