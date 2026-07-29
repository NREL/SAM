Weather Data and LK
===================

Use the following variables and functions to work with weather files from an :doc:`LK script <../reference/macros>`.

To specify a file that is in your solar resource library, set ```solar_resource``` to the file name as it appears in the library with no extension:

```set( "solar_resource" , "phoenix_az_33.450495_-111.983688_psmv3_60_tmy");```

To generate a list of files in your solar resource library as an array, and specify the third file in the library:

```weather_file_list = library( "SolarResourceData" );```
 ```set( "solar_resource" , weather_file_list[3] );```

To add a folder to your solar resource library:

```folder_list = get_settings( "solar_data_paths" );```
 ```new_folder = choose_dir( cwd(), "Choose Weather File" );```
 ```new_folder_list = folder_list + ";" + new_folder;```
 ```set_settings( "solar_data_paths" , new_folder_list );```
 ```rescanlibrary( "solar" );```

For a weather file that is not in your solar resource library, you can also use the ```user_specified_weather_file``` variable:

```set( "use_specific_weather_file" , 1 );```
 ```weather_file = choose_file( cwd(), "Choose Weather File", "SAM CSV (*.csv)|*.csv" );```
 ```set( "user_specified_weather_file" , weather_file );```

.. note:: The ```user_specified_weather_file``` and ```use_specific_weather_file``` variables are not available on the Location and Resource input page. If you use these variables in a script to set the weather file and then go to the Location and Resource page, you should see the file listed as the current weather file for simulations along with a message with a button that you can use to add the weather file's folder to your solar resource library if you want to be able to access the file directly from the Solar Resource input page later.


|SS_SolarResource-user-variable|

.. |SS_SolarResource-user-variable| image:: ../images/SS_SolarResource-user-variable.png
