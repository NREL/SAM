
import java.lang.reflect.Field;
import java.util.Arrays;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;

public class TestBelpe {

// Used for the 64k Java code limitation and array initialization ("code too large" error)
	public static ArrayList<String> OutputNames = new ArrayList<String>();
	public static ArrayList<ArrayList<Float>> OutputData = new ArrayList<ArrayList<Float>>();
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

	public static void GetData() throws FileNotFoundException
	{
		 Scanner sFile = null;
         try
         {
			sFile = new Scanner(new File("TestBelpe.data"));

			while(sFile.hasNextLine())
			{
				String line = sFile.nextLine();

				Scanner scanner = new Scanner(line);
				scanner.useDelimiter(", ");
				if (scanner.hasNext())
				{
					OutputNames.add(scanner.next());
				}
				ArrayList<Float> data = new ArrayList<Float>();
				while(scanner.hasNextFloat())
				{
					data.add(scanner.nextFloat());
				}
				OutputData.add(data);
				scanner.close();
			}
		 }
		 finally
		 {
            if (sFile != null)
            {
                sFile.close();
			}
        }
	}

	public static float[] GetFloatArray(String name)
	{
		int pos = OutputNames.indexOf(name);
		if (pos >= 0)
		{
			float[] data = new float[OutputData.get(pos).size()];
			int i = 0;
			for (Float f : OutputData.get(pos))
			{
			    data[i++] = (f != null ? f : Float.NaN);
			}
			return data;
		}
		else
		{
			return null;
		}

	}


    public static void Belpe() throws FileNotFoundException
    {
        System.out.println("\nbelpe example using PV Residential defaults from 2014.11.24 release");
        SSC.Module mod = new SSC.Module("belpe");

		GetData();
        System.out.println("belpe example names and data length " + OutputNames.size() + "," + OutputData.size());
		for (int i = 0; i<OutputData.size(); i++)
		{
            System.out.println("belpe example data " + i + " length " + OutputData.get(i).size());
		}

		SSC.Data data = new SSC.Data();
		data.setNumber( "en_belpe", 0 );

		data.setArray("e_load",GetFloatArray("e_load"));
		data.setArray("p_load",GetFloatArray("p_load"));

		data.setString( "solar_resource_file", "USA AZ Phoenix (TMY2).csv" );
		data.setNumber( "floor_area", 2000 );
		data.setNumber( "Stories", 2 );
		data.setNumber( "YrBuilt", 1980 );
		data.setNumber( "Retrofits", 0 );
		data.setNumber( "Occupants", 4 );
		data.setArray( "Occ_Schedule", new float[] { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 } );
		data.setNumber( "THeat", 68 );
		data.setNumber( "TCool", 76 );
		data.setNumber( "THeatSB", 68 );
		data.setNumber( "TCoolSB", 76 );
		data.setArray( "T_Sched", new float[] { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 } );
		data.setNumber( "en_heat", 1 );
		data.setNumber( "en_cool", 1 );
		data.setNumber( "en_fridge", 1 );
		data.setNumber( "en_range", 1 );
		data.setNumber( "en_dish", 1 );
		data.setNumber( "en_wash", 1 );
		data.setNumber( "en_dry", 1 );
		data.setNumber( "en_mels", 1 );
		data.setArray( "Monthly_util", new float[] {725, 630, 665, 795, 1040, 1590, 1925, 1730, 1380, 1080, 635, 715 } );

       	if (mod.exec(data))
        {
            float[] eload = data.getArray("e_load");
            for (int i = 0; i < eload.length; i++)
                System.out.println("[" + i + "]: " + eload[i] + " kWh");
            float[] pload = data.getArray("p_load");
            for (int i = 0; i < pload.length; i++)
                System.out.println("[" + i + "]: " + pload[i] + "kW");
	     	System.out.println("belpe example OK");
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
            System.out.println("belpe example failed");
        }
    }

    public static void main(String[] args) throws Exception
    {
        // address dll path issues (if necessary)
        //e.g. addLibraryPath( "C:\\Projects\\SAM\\VS2012\\ssc\\java");
        System.loadLibrary("SSCAPIJNI");
		Belpe();
    }


}
