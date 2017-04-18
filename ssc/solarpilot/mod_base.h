#ifndef _MOD_BASE_
#define _MOD_BASE_ 1

/*
Forms the base class for the various components of the solar field. 
These components can use the methods and variable declarations provided here.
*/
#include <sstream>

#include <unordered_map>
using std::unordered_map;

#include <map>
#include <vector>
#include <string>
#include <time.h>
#include <algorithm>

//#include "std::string_util.h"
#include "Toolbox.h"
#include "string_util.h"

//using namespace std;

#ifdef _MSC_VER
#pragma warning(disable: 4290)  // ignore warning: 'C++ exception specification ignored except to indicate a function is not __declspec(nothrow)'
struct Point;
struct var_map;


#pragma warning(disable:4503)	//warning for name length - VS2010 compiler
#pragma warning(disable:4505)	//warning for removing unused method
#endif
 

//The land boundary arrays should be described with sets of polygons for inclusions and exclusions
//typedef std::vector<std::vector<Point> > bounds_array;

template<typename T> static std::string my_to_string(const T &value) {
	ostringstream x;
	x << value;
	return x.str();
}

class simulation_info
{
	/* 
	This object provides information to the calling program on the status of the 
	simulations underway.
	*/
	bool (*_callback)(simulation_info* siminfo, void *data);
	void *_callback_data;

	//void (SPFrame::*_fupdate)(simulation_info* siminfo);	//Pointer to the parent's update function
	//SPFrame *_parent;
	double
		_sim_progress;
	std::string
		_sim_notice;
	int
		_current_simulation,
		_total_sim_count;
	bool
		_is_active;

public:
	simulation_info();
	
	bool isEnabled();	//Indicates whether any simulation is currently in progress (that's being tracked)
	int getCurrentSimulation();	//Index of the current simulation
	int getTotalSimulationCount();	//Total number of expected simulations in this batch
	double getSimulationProgress();	//Fractional progress [0..1] of the simulation
	void getSimulationInfo(int &current, int &total, double &progress);
	std::string *getSimulationNotices();	//Returns a pointer to the std::vector of simulation notices 

	void ResetValues();
	void Reset();

	//Sets
	void setCallbackFunction(bool (*updateFunc)(simulation_info* siminfo, void *data), void *cdata);
	bool setCurrentSimulation(int val);
	bool setTotalSimulationCount(int val);
	void clearSimulationNotices();
	bool addSimulationNotice(std::string &notice);
	bool addSimulationNotice(std::string notice);
	void isEnabled(bool state);
};

class simulation_error
{
	/*
	This class serves as a method for providing the calling program with events and info on 
	simulation (run-time) errors and warnings.
	*/

	void (*_callback)(simulation_error* simerror, void *data);
	void *_callback_data;

	std::string _message_log;
	bool _is_connected;		//has this been tied to a parent error handler?
	bool _is_fatal;
	bool _force_display;	//Force the message to pop 
	bool _terminate_status;	//Set by the calling program. Simulations should terminate if true

public:
	simulation_error();

	void setCallbackFunction(void (*errorFunc)(simulation_error* simerror, void *data), void *cdata);

	bool isFatal();
	bool isDisplayNow();
	std::string *getSimulationErrors();
	bool checkForErrors();

	void Reset();

	//If any error has already been recorded, don't reset the error flag.
	void addSimulationError(std::string error, bool is_fatal=false, bool force_display=false);
	void addRangeError(double val, std::string varname, std::string range);
	void setTerminateStatus(bool do_terminate);
	void clearErrorLog();
	
};

enum SP_DATTYPE { SP_INT, SP_DOUBLE, SP_STRING, SP_BOOL, SP_MATRIX_T, SP_DVEC_POINT, SP_VEC_DOUBLE, SP_VEC_INTEGER, SP_WEATHERDATA, SP_VOIDPTR};

class spbase
{

protected:

    bool _setv(std::string &SV, void *Vp)
    {
        //don't do anything with a void argument
        (void)SV;
        (void)Vp;
        return true;
    }

    bool _setv(std::string &SV, int &Vp)
    {
        return to_integer(SV, &Vp);
    }; 

    bool _setv(std::string &SV, double &Vp)
    {
        return to_double(SV, &Vp);
    }; 

    bool _setv(std::string &SV, std::string &Vp)
    {
        Vp = SV;
        return true;
    };

    //bool _setv(std::string &SV, char* Vp)
    //{
    //    sprintf(Vp, "%s", SV.c_str());
    //    return true;
    //};

    bool _setv(std::string &SV, bool &Vp)
    {
        return to_bool(SV, Vp);
    };

    bool _setv(std::string &SV, matrix_t<double> &Vp)
    {
        try
        {
            std::vector<std::string> content = split(SV, ";");
		    int nrows = (int)content.size();
		    if(nrows == 0) { Vp.resize_fill(1,2,0.0); return true; }
		    std::vector<std::string> line;
		    line = split(content.at(0), ",");
		    int rowlen = (int)line.size();
		    Vp.resize(nrows, rowlen);
		    for (int i=0; i<nrows; i++){
			    line = split(content.at(i), ",");
			    for (int j=0; j<rowlen; j++){
				    to_double(line.at(j), &Vp.at(i, j));
			    }
		    }
        }
        catch(...)
        {
            return false;
        }
        return true;
    };

    bool _setv(std::string &SV, std::vector< Point > &Vp)
    {
        //splits a set of 3d points into a vector<Point>
	    //should be [P]x1,y1,z1[P]x2,y2,z2...
        try
        {
		    std::vector<std::string> content = split(SV, "[P]");
		    std::vector<std::string> line;
		    double x, y, z;
		    int nrows = (int)content.size();
		    Vp.resize(nrows);
		    for(int i=0; i<nrows; i++) {
			    //split each text by comma
			    line = split(content.at(i), ",");
			    to_double(line.at(0), &x);
			    to_double(line.at(1), &y);
			    to_double(line.at(2), &z);
			    Vp.at(i).Set(x, y, z);
		    }
	    }
        catch(...)
        {
            return false;
        }
        return true;
    };

    bool _setv(std::string &SV, std::vector< double > &Vp )
    {
        try{

            std::vector< std::string > svals = split(SV, ",");

            Vp.resize( svals.size() );

            for(size_t i=0; i<svals.size(); i++)
                to_double( svals.at(i), &Vp.at(i) );
        }
        catch(...)
        {
            return false;
        }
        return true;
    };

    bool _setv(std::string &SV, std::vector< int > &Vp)
    {
        try
        {
            std::vector< std::string > svals = split(SV, ",");

            Vp.resize( svals.size() );

            for(size_t i=0; i<svals.size(); i++)
                to_integer( svals.at(i), &Vp.at(i) );
        }
        catch(...)
        {
            return false; 
        }
        return true;
    };

    bool _setv(std::string &SV, WeatherData &Vp)
    {
        try
        {
            std::vector<std::string> vals;
			std::vector<std::string> entries = split(SV, "[P]");

		    int nrows = (int)entries.size();
		    int nv, i, j;
		    Vp.resizeAll(nrows, 0.0);

            //day, hour, month, dni, tdb, pres, vwind, step_weight
		    vector<vector<double>*> *wdvars = Vp.getEntryPointers();

		    for(i=0; i<nrows; i++){
			    vals = split(entries.at(i), ",");
			    nv = (int)(vals.size() < wdvars->size() ? vals.size() : wdvars->size()); 
			    for(j=0; j<nv; j++){
				    to_double(vals.at(j), &wdvars->at(j)->at(i));
			    }
		    }
        }
        catch(...)
        {
            return false;
        }
        return true;
    };

    bool _setv(std::string &SV, std::vector< std::vector< Point > > &Vp )
    {
        
        /* 
        [POLY] separates entries
        [P] separates points within a polygon
        ',' separates x,y,z within a point
        */

        Vp.clear();

        if( SV.empty() ) return true;

        std::vector< std::string > polys = split(SV, "[POLY]");

        Vp.resize(polys.size() );

        for(size_t i=0; i<polys.size(); i++)
        {
            std::vector< std::string > pts = split(polys.at(i), "[P]");

            Vp.at(i).resize( pts.size(), Point() );

            for( size_t j=0; j<pts.size(); j++ )
            {
                std::vector< std::string > vals = split(pts.at(j), ",");

                double x;
                for( size_t k=0; k<vals.size(); k++ )
                {
                    to_double(vals.at(k), &x);
                    Vp.at(i).at(j)[(int)k] = x;
                }
            }
        }

        return true;
    };

    //----------------------------------------------------------------------------------------
    void _as_str(std::string &vout, void* v)
    {
        //don't do anything with a void argument
        (void)vout;
        (void)v;
    }

    void _as_str(std::string &vout, int &v)
    {
        vout = my_to_string(v);
    };

    void _as_str(std::string &vout, std::string &v)
    {
        vout = v;
    };

    void _as_str(std::string &vout, double &v)
    {
        vout = my_to_string(v);
    };

    void _as_str(std::string &vout, bool &v)
    {
        vout = v ? "true" : "false";
    };

    void _as_str(std::string &vout,  matrix_t<double> &v)
    {
        vout.clear();
        for(size_t i=0; i<v.nrows(); i++)
        {
            for(size_t j=0; j<v.ncols(); j++)
            {
                vout.append( my_to_string(v.at(i,j)) );
                if( j < v.ncols()-1 )
                    vout.append(",");
            }
            vout.append(";");
        }
    };

    void _as_str(std::string &vout, std::vector< Point > &v)
    {
        vout.clear();

        for(size_t i=0; i<v.size(); i++)
            vout.append("[P]" + my_to_string(v.at(i).x) + "," + my_to_string(v.at(i).y) + "," + my_to_string(v.at(i).z) );
    };

    void _as_str(std::string &vout, std::vector< double > &v)
    {
        vout.clear();
        for(size_t i=0; i<v.size(); i++)
        {
            vout.append( my_to_string(v.at(i)) );
            if(i<v.size()-1)
                vout.append(",");
        }
    };

    void _as_str(std::string &vout, std::vector< int > &v)
    {
        vout.clear();
        for(size_t i=0; i<v.size(); i++)
        {
            vout.append( my_to_string(v.at(i)) );
            if(i<v.size()-1)
                vout.append(",");
        }
    };

    void _as_str(std::string &vout, WeatherData &v)
    {
        vout.clear();

        std::stringstream S;

        vector<vector<double>*> *wp = v.getEntryPointers();

        for(size_t i=0; i<wp->front()->size(); i++)
        {
            S << "[P]";

            for(size_t j=0; j<wp->size(); j++)
            {
                S << wp->at(j)->at(i);
                if( j<wp->size()-1 )
                    S << ",";
                //vout.append( std::to_string(wp->at(j)->at(i)) );
                //if( j<wp->size()-1 )
                    //vout.append(",");
            }
        }
        vout = S.str();
    };

    void _as_str(std::string &vout, vector< vector< Point > > &v)
    {
        /* 
        [POLY] separates entries
        [P] separates points within a polygon
        ',' separates x,y,z within a point
        */

        vout.clear();

        for( size_t i=0; i<v.size(); i++)
        {
            vout.append("[POLY]");

            for( size_t j=0; j<v.at(i).size(); j++)
            {
                vout.append("[P]");

                for(int k=0; k<3; k++)
                {
                    vout.append(my_to_string(v.at(i).at(j)[k]));
                    if( k<2 )
                        vout.append(",");
                }
            }
        }

        return;
    };

    //int cselect;	//Current selection for a combo, integer corresponding to the mapped options (not the choices vector)


public:

    std::string	name;	    //Formal variable name
	std::string units;	    //units for the variable
	std::string ctype;	    //Control type
	SP_DATTYPE dattype;    //data type DATTYPE enum
	std::string short_desc; //Short description
	std::string long_desc;	//Long description
	
	
    bool is_param;	//Is this variable parameterizable?
	bool is_disabled;	//Is this variable disabled (overridden)?

    //virtual bool set_from_string(std::string &Val){ (void)Val; return false;};
    virtual bool set_from_string(const char* Val){(void)Val; return false;};
    //virtual bool set_from_string(std::string Val){ (void)Val; return false;};
    virtual void as_string(std::string &ValAsStr){ (void)ValAsStr; throw spexception("Virtual method as_string cannot be executed in base class");};
    virtual std::string as_string(){throw spexception("Virtual method as_string cannot be executed in base class");};
    virtual bool combo_select(std::string choice){ (void)choice; throw spexception("Virtual method combo_select cannot be executed in base class"); };
    virtual bool combo_select_by_choice_index(int index){ (void)index; throw spexception("Virtual method combo_select_by_choice_index cannot be executed in base class");};
    virtual bool combo_select_by_mapval(int mapval){ (void)mapval; throw spexception("Virtual method combo_select_by_mapval cannot be executed in base class");};
    //virtual std::string combo_get_current_label(){throw spexception("Virtual method combo_get_current_label cannot be executed in base class");};
    virtual std::vector<std::string> combo_get_choices(){throw spexception("Virtual method combo_get_choices cannot be executed in base class");};
    //virtual std::string combo_get_key(int index){(void)index; throw spexception("Virtual method combo_get_key cannot be executed in base class");};
    virtual int combo_get_count(){throw spexception("Virtual method combo_get_count cannot be executed in base class");};
    //virtual int Cselect(){throw spexception("Virtual method Cselect cannot be executed in base class"); };
    virtual int mapval(){throw spexception("Virtual method combo_get_current_mapval cannot be executed in base class");};
    virtual int combo_get_current_index(){throw spexception("Virtual method combo_get_current_index cannot be executed in base class");};
    virtual SP_DATTYPE get_data_type(){ return dattype; }
};

template <typename T>
class spvar : public spbase
{

    struct combo_choices
    {
        std::vector<std::string> _choices;
        std::vector<int> _intvals;

        std::string &at_index(int ind){return _choices.at(ind); };
        int at(std::string name){ 
            int ind = index(name);
            if(ind < _intvals.size() )
                return _intvals.at( index(name) ); 
            else 
                throw spexception("Could not locate combo value " + name);
        };
        int index( std::string name) { 
            return (int)(find(_choices.begin(), _choices.end(), name) - _choices.begin()); 
        };
        void clear()
        {
            _choices.clear();
            _intvals.clear();
        };
        
    };
    combo_choices choices;

public:
    T val;

    /* ------- combo stuff ------------*/
    void combo_clear()
    {
        choices.clear();
    };
    //std::string combo_get_key(int index)
    //{
    //    return choices._keys.at(index);
    //};
    
    std::vector<std::string> combo_get_choices()
    {
        int nv = (int)choices._choices.size();
        std::vector<std::string> rv(nv);
        for(int i=0; i<nv; i++)
        {
            _as_str(rv.at(i), choices._choices.at(i));
        }

        return rv;
    };
    
    void combo_add_choice(std::string &name, string &mval)
    {
        int mapint;
        to_integer(mval, &mapint);
        choices._choices.push_back(name);
        choices._intvals.push_back(mapint);
    };

    bool combo_select_by_choice_index(int index)
    {
        _setv(choices._choices.at(index), val);
        return true;
    };

    bool combo_select_by_mapval(int mapval)
    {
        int index = (int)(find(choices._intvals.begin(), choices._intvals.end(), mapval) - choices._intvals.begin());
        if( index < choices._intvals.size() )
            _setv(choices._choices.at(index), val);
        else
            return false;
        
        return true;
    };

    bool combo_select(std::string choice)
    {
        int ind = (int)(find(choices._choices.begin(), choices._choices.end(), choice) - choices._choices.begin());
        if( ind < choices._choices.size() )
            _setv(choice, val);
        else
            throw spexception("Invalid combo value specified: " + choice);

        return true;
    };
    //std::string combo_get_current_label()
    //{
    //    return combo_get_keys().at(cselect);
    //};
    
    //int Cselect()
    //{
    //    return cselect;
    //};
    int mapval()
    {
        std::string valstr; 
        _as_str(valstr, val);
        return choices._intvals.at( choices.index( valstr ) );
    };

    int combo_get_current_index()
    {
        std::string valstr; 
        _as_str(valstr, val);
        return choices.index( valstr );
    };

    int combo_get_count()
    {
        return (int)choices._choices.size();
    };
    /* ------------------------------- */

    
    //bool set_from_string(std::string &Val)
    //{
    //    return _setv(Val, val);
    //};
    
    bool set_from_string(const char* Val)
    {
    	std::string sval = Val;
    	return _setv(sval, val);
    };
    
    //bool set_from_string(std::string Val)
    //{
    //	return _setv(Val, val);
    //};

    void as_string(std::string &ValAsStr)
    {
        _as_str(ValAsStr, val);
    }
    std::string as_string()
    {
        std::string vstr;
        _as_str(vstr, val);
        return vstr;
    }

    void set( 
        std::string Address, 
        SP_DATTYPE Dtype, 
        std::string Value, 
        std::string Units, 
        bool Is_param, 
        std::string Ctrl, 
        std::string Special,
        bool UI_disable,
        std::string Label,
        std::string Description)
    {
        /* 
        Parse and set the variable to it's value from a string argument
        */

        //first set supplementary info
	    name = Address;	//Formal variable name
	    units = Units;	//units for the variable
	    ctype = Ctrl;	//Control type
	    dattype = Dtype; //data type DATTYPE enum
	    short_desc = Label; //Short description
	    long_desc = Description;	//Long description
        is_param = Is_param;	//Is this variable parameterizable?
	    is_disabled = UI_disable;	//Is this variable disabled (overridden)?

        choices.clear();
        if( ctype == "combo" )
        {
            std::vector<std::string> ckeys = split(Special, ";");
            for(int i=0; i<ckeys.size(); i++)
            {
                std::vector<std::string> pair = split(ckeys.at(i), "=");

                combo_add_choice( pair.front(), pair.back() );
            }

            //when loading a combo, the value will be specified by looking for an index in "Value" and setting val equal to the choice at that index
            int val_index;
            to_integer(Value, &val_index);

            if(! Special.empty() )
                combo_select_by_choice_index( val_index );

        }
        else
        {
            //parse value
            bool parseok = _setv(Value, val);

            //check for parsing errors
            if(! parseok)
                throw spexception("An error occurred while assigning input to the internal variable structure. {" + Address + " << " + Value + "}");
        }
        
    };




};

template <typename T>
class spout : public spbase  //need public inheritance of spbase to allow conversion to base class
{
private:
    // override public inheritance of members to be private in spout class
    using spbase::name;	    //Formal variable name
	using spbase::units;	    //units for the variable
	using spbase::ctype;	    //Control type
	using spbase::dattype;    //data type DATTYPE enum
	using spbase::short_desc; //Short description
	using spbase::long_desc;	//Long description
	
    using spbase::is_param;	//Is this variable parameterizable?
	using spbase::is_disabled;	//Is this variable disabled (overridden)?

    T _val;
public:

    bool set_from_string(const char* Val)
    {
        std::string sval = Val;
        return _setv(sval, _val);
    };
    void as_string(std::string &ValAsStr)
    {
        _as_str(ValAsStr, _val);
    }
    std::string as_string()
    {
        std::string vstr;
        _as_str(vstr, _val);
        return vstr;
    }
    void setup( 
        std::string Address, 
        SP_DATTYPE Dtype, 
        std::string Units, 
        bool Is_param, 
        std::string Ctrl, 
        std::string Special,
        bool UI_disable,
        std::string Label,
        std::string Description)
    {
        //set(Address, Dtype, "", Units, Range, IsParam, Ctrl, Special, UI_disable, Label, Description);

        //first set supplementary info
	    name = Address;	//Formal variable name
	    units = Units;	//units for the variable
	    ctype = Ctrl;	//Control type
	    dattype = Dtype; //data type DATTYPE enum
	    short_desc = Label; //Short description
	    long_desc = Description;	//Long description

        if( ! ctype.empty() )
            throw spexception("Special controls are not allowed for spout objects");

        /*if( ctype == "combo" )
        {
            choices = split(Special, ",");
        }
        else
        {
            choices = std::vector<std::string>(0);
        }*/

        //cselect = 0;	//Current selection for a combo, integer corresponding to the mapped options (not the choices std::vector)
    	
        is_param = Is_param;	//Is this variable parameterizable?
	    is_disabled = UI_disable;	//Is this variable disabled (overridden)?

    }

    //create methods for the variable members to emphasize that these cannot be modified
    T& Val() { return _val; };	//get variable value
    void Setval(T v) { _val = v; };  //set variable value
    std::string	Name() { return name; };	    //Formal variable name
    std::string Units() { return units; };	    //units for the variable
    std::string Ctype() { return ctype; };	    //Control type
    std::string Dattype() { return dattype; };    //data type DATTYPE enum
    std::string Short_desc() { return short_desc; }; //Short description
    std::string Long_desc() { return long_desc; };	//Long description
    //spvar<T>::combo_choices *Choices() { return &choices; };
    //int Cselect() { return cselect; };	//Current selection for a combo, integer corresponding to the mapped options (not the choices std::vector)
    bool Is_param() { return is_param; };	//Is this variable parameterizable?
    bool Is_disabled() { return is_disabled; };	//Is this variable disabled (overridden)?
};



class mod_base
{
	//var_map *vars;
	
public:
	std::string _working_dir;
	//mod_base();	//default constructor

	bool checkRange(std::string range, int &val, int *flag = NULL);
	bool checkRange(std::string range, double &val, int *flag = NULL);

	std::string *getWorkingDir();
	void setWorkingDir(std::string &dir);

	//error handling
	//Type = "notice","warning","fatal"
	//Message = "Indication of what went wrong"
	//void error(std::string type, std::string message){return;};
	//void error_range(int variable, std::string varname, std::string range){error_range(double(variable),varname, range);};
	//void error_range(double variable, std::string varname, std::string range){
	//	error("ERROR","Variable "+varname+" out of range with value "+to_std::string(variable)+". Valid range is "+range+".\n");
	//};

};


#endif
