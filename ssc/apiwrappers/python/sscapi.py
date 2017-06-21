# #####################################################################
#
#   System Simulation Core (SSC) Python Wrapper
#   Author: Aron Dobos @ NREL and Steven Janzou @ NREL
#
# #####################################################################


import string, sys, struct, os, numpy
from ctypes import *

c_number = c_float # must be c_double or c_float depending on how defined in sscapi.h
class PySSC:

	def __init__(self):
		
		if sys.platform == 'win32' or sys.platform == 'cygwin':
			if 8*struct.calcsize("P") == 64:
				self.pdll = CDLL("ssc.dll") 
			else:
				self.pdll = CDLL("ssc.dll") 
		elif sys.platform == 'darwin':
			self.pdll = CDLL("ssc.dylib") 
		elif sys.platform == 'linux2':
			self.pdll = CDLL('ssc.so')   # instead of relative path, require user to have on LD_LIBRARY_PATH
		else:
			print "Platform not supported ", sys.platform


	INVALID=0
	STRING=1
	NUMBER=2
	ARRAY=3
	MATRIX=4

	INPUT=1
	OUTPUT=2
	INOUT=3

	def version(self):
		self.pdll.ssc_version.restype = c_int
		return self.pdll.ssc_version()

	def data_create(self):
		self.pdll.ssc_data_create.restype = c_void_p
		return self.pdll.ssc_data_create()

	def data_free(self, p_data):
		self.pdll.ssc_data_free( c_void_p(p_data) )

	def data_clear(self, p_data):
		self.pdll.ssc_data_clear( c_void_p(p_data) )

	def data_unassign(self, p_data, name):
		self.pdll.ssc_data_unassign( c_void_p(p_data), c_char_p(name) )

	def data_query(self, p_data, name):
		self.pdll.ssc_data_query.restype = c_int
		return self.pdll.ssc_data_query( c_void_p(p_data), c_char_p(name) )

	def data_first(self, p_data):
		self.pdll.ssc_data_first.restype = c_char_p
		return self.pdll.ssc_data_first( c_void_p(p_data) )

	def data_next(self, p_data):
		self.pdll.ssc_data_next.restype = c_char_p
		return self.pdll.ssc_data_next( c_void_p(p_data) )

	def data_set_string(self, p_data, name, value):
		self.pdll.ssc_data_set_string( c_void_p(p_data), c_char_p(name), c_char_p(value) )

	def data_set_number(self, p_data, name, value):
		self.pdll.ssc_data_set_number( c_void_p(p_data), c_char_p(name), c_number(value) )

	def data_set_array(self,p_data,name,parr):
		count = len(parr)
		arr = (c_number*count)()
		arr[:] = parr # set all at once instead of looping
			
		return self.pdll.ssc_data_set_array( c_void_p(p_data), c_char_p(name),pointer(arr), c_int(count))

	def data_set_matrix(self,p_data,name,mat):
		nrows = len(mat)
		ncols = len(mat[0])
		size = nrows*ncols
		arr = (c_number*size)()
		idx=0
		for r in range(nrows):
			for c in range(ncols):
				arr[idx] = c_number(mat[r][c])
				idx=idx+1
		return self.pdll.ssc_data_set_matrix( c_void_p(p_data), c_char_p(name),pointer(arr), c_int(nrows), c_int(ncols))

	def data_set_table(self,p_data,name,tab):
		return self.pdll.ssc_data_set_table( c_void_p(p_data), c_char_p(name), c_void_p(tab) );

	def data_get_string(self, p_data, name):
		self.pdll.ssc_data_get_string.restype = c_char_p
		return self.pdll.ssc_data_get_string( c_void_p(p_data), c_char_p(name) )

	def data_get_number(self, p_data, name):
		val = c_number(0)
		self.pdll.ssc_data_get_number( c_void_p(p_data), c_char_p(name), byref(val) )
		return val.value

	def data_get_array(self,p_data,name):
		count = c_int()
		self.pdll.ssc_data_get_array.restype = POINTER(c_number)
		parr = self.pdll.ssc_data_get_array( c_void_p(p_data), c_char_p(name), byref(count))
		arr = parr[0:count.value] # extract all at once			
		return arr

	def data_get_matrix(self,p_data,name):
		nrows = c_int()
		ncols = c_int()
		self.pdll.ssc_data_get_matrix.restype = POINTER(c_number)
		parr = self.pdll.ssc_data_get_matrix( c_void_p(p_data), c_char_p(name), byref(nrows), byref(ncols) )
		idx = 0
		mat = []
		for r in range(nrows.value):
			row = []
			for c in range(ncols.value):
				row.append( float(parr[idx]) )
				idx = idx + 1
			mat.append(row)
		return mat
		
	# don't call data_free() on the result, it's an internal
	# pointer inside SSC
	def data_get_table(self,p_data,name): 
		return self.pdll.ssc_data_get_table( c_void_p(p_data), name );

	def module_entry(self,index):
		self.pdll.ssc_module_entry.restype = c_void_p
		return self.pdll.ssc_module_entry( c_int(index) )

	def entry_name(self,p_entry):
		self.pdll.ssc_entry_name.restype = c_char_p
		return self.pdll.ssc_entry_name( c_void_p(p_entry) )

	def entry_description(self,p_entry):
		self.pdll.ssc_entry_description.restype = c_char_p
		return self.pdll.ssc_entry_description( c_void_p(p_entry) )

	def entry_version(self,p_entry):
		self.pdll.ssc_entry_version.restype = c_int
		return self.pdll.ssc_entry_version( c_void_p(p_entry) )

	def module_create(self,name):
		self.pdll.ssc_module_create.restype = c_void_p
		return self.pdll.ssc_module_create( c_char_p(name) )

	def module_free(self,p_mod):
		self.pdll.ssc_module_free( c_void_p(p_mod) )

	def module_var_info(self,p_mod,index):
		self.pdll.ssc_module_var_info.restype = c_void_p
		return self.pdll.ssc_module_var_info( c_void_p(p_mod), c_int(index) )

	def info_var_type( self, p_inf ):
		return self.pdll.ssc_info_var_type( c_void_p(p_inf) )

	def info_data_type( self, p_inf ):
		return self.pdll.ssc_info_data_type( c_void_p(p_inf) )

	def info_name( self, p_inf ):
		self.pdll.ssc_info_name.restype = c_char_p
		return self.pdll.ssc_info_name( c_void_p(p_inf) )

	def info_label( self, p_inf ):
		self.pdll.ssc_info_label.restype = c_char_p
		return self.pdll.ssc_info_label( c_void_p(p_inf) )

	def info_units( self, p_inf ):
		self.pdll.ssc_info_units.restype = c_char_p
		return self.pdll.ssc_info_units( c_void_p(p_inf) )

	def info_meta( self, p_inf ):
		self.pdll.ssc_info_meta.restype = c_char_p
		return self.pdll.ssc_info_meta( c_void_p(p_inf) )

	def info_group( self, p_inf ):
		self.pdll.ssc_info_group.restype = c_char_p
		return self.pdll.ssc_info_group( c_void_p(p_inf) )

	def info_uihint( self, p_inf ):
		self.pdll.ssc_info_uihint.restype = c_char_p
		return self.pdll.ssc_info_uihint( c_void_p(p_inf) )

	def module_exec( self, p_mod, p_data ):
		self.pdll.ssc_module_exec.restype = c_int
		return self.pdll.ssc_module_exec( c_void_p(p_mod), c_void_p(p_data) )
		ssc_module_exec_simple_nothread
		
	def module_exec_simple_no_thread( self, modname, data ):
		self.pdll.ssc_module_exec_simple_nothread.restype = c_char_p;
		return self.pdll.ssc_module_exec_simple_nothread( c_char_p(modname), c_void_p(data) );

	def module_log( self, p_mod, index ):
		log_type = c_int()
		time = c_float()
		self.pdll.ssc_module_log.restype = c_char_p
		return self.pdll.ssc_module_log( c_void_p(p_mod), c_int(index), byref(log_type), byref(time) )


	def module_exec_set_print( self, prn ):
		return self.pdll.ssc_module_exec_set_print( c_int(prn) );

if __name__ == "__main__":

	def setup_wind(ssc, data):
		ssc.data_set_number( data, 'wind_resource_shear', 0.14 );
		ssc.data_set_number( data, 'wind_resource_turbulence_coeff', 0.1 );
		ssc.data_set_number( data, 'system_capacity', 2.4 );
		ssc.data_set_number( data, 'wind_resource_model_choice', 0 );
		ssc.data_set_number( data, 'wind_characteristics_weibullK', 2.1 );
		ssc.data_set_number( data, 'wind_characteristics_class', 7.6 );
		ssc.data_set_number( data, 'wind_turbine_rotor_diameter', 3.7 );
		ssc.data_set_array( data, 'wind_turbine_powercurve_windspeeds', [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40 ] );
		ssc.data_set_array( data, 'wind_turbine_powercurve_powerout', [ 0, 0, 0, 0, 0.08, 0.2, 0.35, 0.6, 1, 1.6, 2, 2.25, 2.35, 2.4, 2.4, 2.37, 2.3, 2.09, 2, 2, 2, 2, 2, 1.98, 1.95, 1.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ] );
		ssc.data_set_number( data, 'wind_turbine_cutin', 4 );
		ssc.data_set_number( data, 'wind_turbine_hub_ht', 15 );
		ssc.data_set_number( data, 'wind_turbine_max_cp', 0.42 );
		ssc.data_set_array( data, 'wind_farm_xCoordinates', [ 0 ] );
		ssc.data_set_array( data, 'wind_farm_yCoordinates', [ 0 ] );
		ssc.data_set_number( data, 'wind_farm_losses_percent', 0 );
		ssc.data_set_number( data, 'wind_farm_wake_model', 0 );
		ssc.data_set_number( data, 'adjust:constant', 0 );	
		


	def run_windmodel( ssc, data ):
		# run PV system simulation
		mod = ssc.module_create("windpower")	
		ssc.module_exec_set_print( 0 );
		if ssc.module_exec(mod, data) == 0:
			print 'wind power simulation error'
			idx = 1
			msg = ssc.module_log(mod, 0)
			while (msg != None):
				print '\t: ' + msg
				msg = ssc.module_log(mod, idx)
				idx = idx + 1
		else:
			ann = ssc.data_get_number(data, "annual_energy")
			print 'wind power Simulation ok, annual energy (kwh) =', ann

		ssc.module_free(mod)

	
	def run_test1():
		wf = 'C:/Users/adobos/Projects/SAMnt/deploy/wind_resource/WY Southern-Flat Lands.srw';
		print wf
		
		ssc = PySSC()
		dat = ssc.data_create()
		setup_wind(ssc,dat);
		ssc.data_set_string( dat, 'wind_resource_filename', wf );
		run_windmodel( ssc, dat );
		
		
		ssc.data_clear(dat);
		
		# read a weather file for this example program
		# and extract the data from it into a bunch of Python variables
		# note: this weather data could come from any source
			
		# create an SSC data with a bunch of fields
		wfd = ssc.data_create();
		ssc.data_set_number( wfd, 'lat', 0);
		ssc.data_set_number( wfd, 'lon', 0);
		ssc.data_set_number( wfd, 'elev',  2088);
		
		# setup column data types: temp=1,pres=2,3=3,dir=4	
		ssc.data_set_array( wfd, 'fields', [1,2,4,3,1,2,4,3,1,2,4,3,1,2,4,3] );
		
		# setup column measurement heights (meters)
		ssc.data_set_array( wfd, 'heights', [ 50,50,50,50,80,80,80,80,110,110,110,110,140,140,140,140 ] );
		
		# read in the matrix of data corresponding to fields and heights above (should have 8760 rows)	
		data = numpy.loadtxt(open(wf,"rb"),delimiter=",",skiprows=5);
		ssc.data_set_matrix( wfd, 'data', data );
		
		# instead of setting a string weather file, simply
		# set the table variable that contains the various fields
		# with solar resource data
		ssc.data_set_table( dat, 'wind_resource_data', wfd );	
		
		# we can free the resource data table now, since
		# the previous line copies it all into SSC
		ssc.data_free(wfd);
		
		# set up other PV parameters and run
		setup_wind( ssc,dat );
		run_windmodel( ssc, dat );
		
		ssc.data_free(dat)


	def setup_pv(ssc, data):
		ssc.data_set_number( data, 'system_capacity', 4 )
		ssc.data_set_number( data, 'module_type', 0 )
		ssc.data_set_number( data, 'array_type', 0 )
		ssc.data_set_number( data, 'losses', 14 )
		ssc.data_set_number( data, 'tilt', 15 )
		ssc.data_set_number( data, 'azimuth', 180 )
		ssc.data_set_number( data, 'adjust:constant', 0 )

	def run_pvwattsv5( ssc, data ):
		# run PV system simulation
		mod = ssc.module_create("pvwattsv5")	
		ssc.module_exec_set_print( 0 );
		if ssc.module_exec(mod, data) == 0:
			print 'PVWatts V5 simulation error'
			idx = 1
			msg = ssc.module_log(mod, 0)
			while (msg != None):
				print '\t: ' + msg
				msg = ssc.module_log(mod, idx)
				idx = idx + 1
		else:
			ann = ssc.data_get_number(data, "ac_annual")
			print 'PVWatts V5 Simulation ok, e_net (annual kW)=', ann

		ssc.module_free(mod)

	def run_test2():
		#wf = 'c:/Users/adobos/Projects/SAMnt/tests/Weather Files/user-germany-potsdam-2011-1-min-samcsv.csv' ;
		wf = 'C:/Users/adobos/Projects/SAMnt/deploy/solar_resource/USA NC Greensboro (TMY2).csv';

		print wf
		
		ssc = PySSC()
		dat = ssc.data_create()
		setup_pv(ssc,dat);
		ssc.data_set_string( dat, 'solar_resource_file', wf );
		run_pvwattsv5( ssc, dat );
		
		
		ssc.data_clear(dat);
		
		# read a weather file for this example program
		# and extract the data from it into a bunch of Python variables
		# note: this weather data could come from any source
		ssc.data_set_string( dat, 'file_name', wf );
		ssc.module_exec_simple_no_thread( 'wfreader', dat );	
		lat = ssc.data_get_number(dat, 'lat');
		lon = ssc.data_get_number(dat, 'lon');
		tz = ssc.data_get_number(dat, 'tz');
		elev = ssc.data_get_number(dat, 'elev');
		year = ssc.data_get_array(dat, 'year');
		month = ssc.data_get_array(dat, 'month')
		day = ssc.data_get_array(dat, 'day');
		hour = ssc.data_get_array(dat, 'hour');
		minute = ssc.data_get_array(dat, 'minute');
		beam = ssc.data_get_array(dat, 'beam');
		diffuse = ssc.data_get_array(dat, 'diffuse');
		wspd = ssc.data_get_array(dat, 'wspd');
		tdry = ssc.data_get_array(dat, 'tdry');
		albedo = ssc.data_get_array(dat, 'albedo');	
		ssc.data_clear( dat );
		
		# create an SSC data with a bunch of fields
		wfd = ssc.data_create();
		ssc.data_set_number( wfd, 'lat', lat);
		ssc.data_set_number( wfd, 'lon', lon);
		ssc.data_set_number( wfd, 'tz',  tz);
		ssc.data_set_number( wfd, 'elev',  elev);
		
		ssc.data_set_array( wfd, 'year',  year);
		ssc.data_set_array( wfd, 'month',  month);
		ssc.data_set_array( wfd, 'day',  day);
		ssc.data_set_array( wfd, 'hour', hour);
		
		# note: if using an hourly TMY file with integrated/averaged
		# values, do not set the minute column here. otherwise
		# SSC will assume it is instantaneous data and will not adjust
		# the sun position in sunrise and sunset hours appropriately
		# however, if using subhourly data or instantaneous NSRDB data
		# do explicitly provide the minute data column for sunpos calcs
		
		# ssc.data_set_array( wfd, 'minute', minute);
		
		
		ssc.data_set_array( wfd, 'dn', beam);
		ssc.data_set_array( wfd, 'df', diffuse);
		ssc.data_set_array( wfd, 'wspd', wspd);
		ssc.data_set_array( wfd, 'tdry', tdry);
		ssc.data_set_array( wfd, 'albedo', albedo);
		
		# instead of setting a string weather file, simply
		# set the table variable that contains the various fields
		# with solar resource data
		ssc.data_set_table( dat, 'solar_resource_data', wfd );	
		
		# we can free the resource data table now, since
		# the previous line copies it all into SSC
		ssc.data_free(wfd);
		
		# set up other PV parameters and run
		setup_pv( ssc,dat );
		run_pvwattsv5( ssc, dat );
		
		
		ssc.data_free(dat);


	run_test1();
	run_test2();



