/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (�Alliance�) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as �System Advisor Model� or �SAM�. Except
*  to comply with the foregoing, the terms �System Advisor Model�, �SAM�, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

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

