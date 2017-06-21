
import java.lang.reflect.Field;
import java.util.Arrays;

public class TestSSCAPI {

/**
* Adds the specified path to the java library path
*
* @param pathToAdd the path to add
* @throws Exception
*/
    public static void addLibraryPath(String pathToAdd) throws Exception{
        final Field usrPathsField = ClassLoader.class.getDeclaredField("usr_paths");
        usrPathsField.setAccessible(true);

        //get array of paths
        final String[] paths = (String[])usrPathsField.get(null);

        //check if the path to add is already present
        for(String path : paths) {
            if(path.equals(pathToAdd)) {
                return;
            }
        }

        //add the new path
        final String[] newPaths = Arrays.copyOf(paths, paths.length + 1);
        newPaths[newPaths.length-1] = pathToAdd;
        usrPathsField.set(null, newPaths);
    }


    public static void TestArrays()
    {
        SSC.Data sscData = new SSC.Data();
        float[] arr = new float[10];
        for (int i = 0; i < arr.length; i++)
        {
            arr[i] = i / 10.0f;
        }
        sscData.setArray("TestArray", arr);

        float[] retArray = sscData.getArray("TestArray");

        System.out.println("\nTesting SetArray and GetArray");
        for (int i = 0; i < retArray.length; i++)
        {
            System.out.println("\treturned array element: " + i + " = " + retArray[i]);
        }

    }

    public static void TestMatrices()
    {
        SSC.Data sscData = new SSC.Data();
        float[][] matrix = { { 1, 2, 3 }, new float[]{ 4, 5, 6 }, new float[]{ 7, 8, 9f } };
        sscData.setMatrix("TestMatrix", matrix);

        float[][] retMatrix = sscData.getMatrix("TestMatrix");

        System.out.println("\nTesting SetMatrix and GetMatrix");
        for (int r = 0; r < retMatrix.length; r++)
        {
            for (int c = 0; c < retMatrix[r].length; c++)
            {
                System.out.println("\treturned matrix element: (" + r + "," + c + ") = " + retMatrix[r][c]);
            }
        }
    }

    public static void PVWatts()
    {
        System.out.println("\nPVWatts example");
        SSC.Data data = new SSC.Data();
        data.setString("solar_resource_file", "abilene.tm2");
	    data.setNumber("system_capacity", 4.0f);
	    data.setNumber("dc_ac_ratio", 1.1f);
	    data.setNumber("inv_eff", 96f );
	    data.setNumber("losses", 14.0757f );
	    data.setNumber("array_type", 0 );
	    data.setNumber("tilt", 20f );
	    data.setNumber("azimuth", 180f );
	    data.setNumber("gcr", 0.4f );
	    data.setNumber("adjust:constant", 1f );

        SSC.Module mod = new SSC.Module("pvwattsv5");
        if (mod.exec(data))
        {
            float tot = data.getNumber("ac_annual");
            float[] ac = data.getArray("ac_monthly");
            for (int i = 0; i < ac.length; i++)
                System.out.println("[" + i + "]: " + ac[i] + " kWh");
            System.out.println("AC total: " + tot);
            System.out.println("PVWatts test OK\n");
        }
        else
        {
            int idx = 0;
            String msg="";
            int[] type={0};
            float[] time={0};
            while (mod.log(idx, msg, type, time))
            {
                String stype = "NOTICE";
                if (type[0] == SSC.API.WARNING) stype = "WARNING";
                else if (type[0] == SSC.API.ERROR) stype = "ERROR";
                System.out.println("[ " + stype + " at time:" + time[0] + " ]: " + msg);
                idx++;
            }
            System.out.println("PVWatts example failed");
        }
    }

    public static void PVWattsFunc()
    {
        System.out.println("\nPVWatts func example");
        SSC.Module sscModule = new SSC.Module("pvwattsv1_1ts");
        SSC.Data sscData = new SSC.Data();
        sscData.setNumber("year", 1970); // general year (tiny effect in sun position)
        sscData.setNumber("month", 1); // 1-12
        sscData.setNumber("day", 1); //1-number of days in month
        sscData.setNumber("hour", 9); // 0-23
        sscData.setNumber("minute", 30); // minute of the hour (typically 30 min for midpoint calculation)
        sscData.setNumber("lat", 33.4f); // latitude, degrees
        sscData.setNumber("lon", -112); // longitude, degrees
        sscData.setNumber("tz", -7); // timezone from gmt, hours
        sscData.setNumber("time_step", 1); // time step, hours

        // solar and weather data
        sscData.setNumber("beam", 824); // beam (DNI) irradiance, W/m2
        sscData.setNumber("diffuse", 29); // diffuse (DHI) horizontal irradiance, W/m2
        sscData.setNumber("tamb", 9.4f); // ambient temp, degree C
        sscData.setNumber("wspd", 2.1f); // wind speed, m/s
        sscData.setNumber("snow", 0); // snow depth, cm (0 is default - when there is snow, ground reflectance is increased.  assumes panels have been cleaned off)

        // system specifications
        sscData.setNumber("system_size", 4); // system DC nameplate rating (kW)
        sscData.setNumber("derate", 0.77f); // derate factor
        sscData.setNumber("track_mode", 0); // tracking mode 0=fixed, 1=1axis, 2=2axis
        sscData.setNumber("azimuth", 180); // azimuth angle 0=north, 90=east, 180=south, 270=west
        sscData.setNumber("tilt", 20); // tilt angle from horizontal 0=flat, 90=vertical


        // previous timestep values of cell temperature and POA
        sscData.setNumber("tcell", 6.94f); // calculated cell temperature from previous timestep, degree C, (can default to ambient for morning or if you don't know)
        sscData.setNumber("poa", 84.5f); // plane of array irradiance (W/m2) from previous time step

        if (sscModule.exec(sscData))
        {
            float poa = sscData.getNumber("poa");
            float tcell = sscData.getNumber("tcell");
            float dc = sscData.getNumber("dc");
            float ac = sscData.getNumber("ac");
            System.out.println("poa: " + poa + " W/m2");
            System.out.println("tcell: " + tcell + " C");
            System.out.println("dc: " + dc + " W");
            System.out.println("ac: " + ac + " W");
        }
    }



    public static void ModulesAndVariables()
    {
        SSC.Entry sscEntry = new SSC.Entry();
        int moduleIndex = 0;
        while (sscEntry.get())
        {
            String moduleName = sscEntry.name();
            String description = sscEntry.description();
            int version = sscEntry.version();
            System.out.println("\nModule: " + moduleName + ", version: " + version);
            System.out.println(" " + description + "\n");
            moduleIndex++;

            SSC.Module sscModule = new SSC.Module(moduleName);
            SSC.Info sscInfo = new SSC.Info(sscModule);

            while (sscInfo.get())
            {
                System.out.println("\t" + sscInfo.varType() + ": \"" + sscInfo.name() + "\" " + " [" + sscInfo.dataType() + "] " + sscInfo.label() + " (" + sscInfo.units() + ")");
            }
        }
    }


    public static void Version()
    {
        System.out.println("\nVersion begin");
        SSC.API sscObj = new SSC.API();
        System.out.println("ssc version = " + sscObj.Version());
        System.out.println("ssc build info = " + sscObj.BuildInfo());
        System.out.println("Version end");
    }



    public static void ModuleList()
    {
        System.out.println("\nModule list begin");
        SSC.Entry sscEntry = new SSC.Entry();
        while( sscEntry.get())
        {
            String module_name = sscEntry.name();
            String description = sscEntry.description();
            int version = sscEntry.version();
            System.out.println( "Module: " + module_name + ", version: " + version);
            System.out.println( "    " + description );
        }
        System.out.println("Module list end");
    }



    public static void main(String[] args) throws Exception
    {
        // address dll path issues (if necessary)
        //e.g. addLibraryPath( "C:\\Projects\\SAM\\VS2012\\ssc\\java");
        System.loadLibrary("SSCAPIJNI");
        Version();
        ModuleList();
        ModulesAndVariables();
        TestArrays();
        TestMatrices();
        PVWatts();
        PVWattsFunc();
    }
}
