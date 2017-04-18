
import java.lang.reflect.Field;
import java.util.Arrays;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;

public class TestUtilityRate3 {

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
			sFile = new Scanner(new File("TestUtilityRate3.data"));

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


    public static void UtilityRate3() throws FileNotFoundException
    {
        System.out.println("\nutilityrate3 example using PV Residential defaults from 2014.11.24 release");
        SSC.Module mod = new SSC.Module("utilityrate3");

		SSC.Data data = new SSC.Data();
		GetData();
        System.out.println("utilityrate3 example names and data length " + OutputNames.size() + "," + OutputData.size());
		for (int i = 0; i<OutputData.size(); i++)
		{
            System.out.println("utilityrate3 example data " + i + " length " + OutputData.get(i).size());
		}


		data.setNumber( "analysis_period", 25 );
		data.setArray("hourly_energy",GetFloatArray("hourly_energy"));
		data.setArray("e_load",GetFloatArray("e_load"));
		data.setArray("p_load",GetFloatArray("p_load"));
		data.setNumber( "inflation_rate", 2.5f );
		data.setArray( "degradation", new float[]{ 0.5f } );
		data.setArray( "load_escalation", new float[]{ 0 } );
		data.setArray( "rate_escalation", new float[]{ 0 } );
		data.setNumber( "ur_enable_net_metering", 1 );
		data.setNumber( "ur_nm_yearend_sell_rate", 0.02789f );
		data.setNumber( "ur_monthly_fixed_charge", 16.68f );
		data.setNumber( "ur_flat_buy_rate", 0 );
		data.setNumber( "ur_flat_sell_rate", 0 );
		data.setNumber( "ur_monthly_min_charge", 0 );
		data.setNumber( "ur_annual_min_charge", 0 );
		data.setNumber( "ur_ec_enable", 1 );
		data.setMatrix( "ur_ec_sched_weekday", new float[][]
		{ { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4 },
		{ 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4 },
		{ 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4 },
		{ 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4 },
		{ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2 },
		{ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2 },
		{ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2 },
		{ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2 },
		{ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2 },
		{ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2 },
		{ 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4 },
		{ 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4 } } );
		data.setMatrix( "ur_ec_sched_weekend", new float[][]
		{ { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4 },
		{ 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4 },
		{ 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4 },
		{ 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4 },
		{ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 },
		{ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 },
		{ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 },
		{ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 },
		{ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 },
		{ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 },
		{ 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4 },
		{ 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4 } } );
		data.setNumber( "ur_ec_p1_t1_br", 0.26687f );
		data.setNumber( "ur_ec_p1_t1_sr", 0 );
		data.setNumber( "ur_ec_p1_t1_ub", 1e+038f );
		data.setNumber( "ur_ec_p1_t2_br", 0 );
		data.setNumber( "ur_ec_p1_t2_sr", 0 );
		data.setNumber( "ur_ec_p1_t2_ub", 1e+038f );
		data.setNumber( "ur_ec_p1_t3_br", 0 );
		data.setNumber( "ur_ec_p1_t3_sr", 0 );
		data.setNumber( "ur_ec_p1_t3_ub", 1e+038f );
		data.setNumber( "ur_ec_p1_t4_br", 0 );
		data.setNumber( "ur_ec_p1_t4_sr", 0 );
		data.setNumber( "ur_ec_p1_t4_ub", 1e+038f );
		data.setNumber( "ur_ec_p1_t5_br", 0 );
		data.setNumber( "ur_ec_p1_t5_sr", 0 );
		data.setNumber( "ur_ec_p1_t5_ub", 1e+038f );
		data.setNumber( "ur_ec_p1_t6_br", 0 );
		data.setNumber( "ur_ec_p1_t6_sr", 0 );
		data.setNumber( "ur_ec_p1_t6_ub", 1e+038f );
		data.setNumber( "ur_ec_p2_t1_br", 0.08328f );
		data.setNumber( "ur_ec_p2_t1_sr", 0 );
		data.setNumber( "ur_ec_p2_t1_ub", 1e+038f );
		data.setNumber( "ur_ec_p2_t2_br", 0 );
		data.setNumber( "ur_ec_p2_t2_sr", 0 );
		data.setNumber( "ur_ec_p2_t2_ub", 1e+038f );
		data.setNumber( "ur_ec_p2_t3_br", 0 );
		data.setNumber( "ur_ec_p2_t3_sr", 0 );
		data.setNumber( "ur_ec_p2_t3_ub", 1e+038f );
		data.setNumber( "ur_ec_p2_t4_br", 0 );
		data.setNumber( "ur_ec_p2_t4_sr", 0 );
		data.setNumber( "ur_ec_p2_t4_ub", 1e+038f );
		data.setNumber( "ur_ec_p2_t5_br", 0 );
		data.setNumber( "ur_ec_p2_t5_sr", 0 );
		data.setNumber( "ur_ec_p2_t5_ub", 1e+038f );
		data.setNumber( "ur_ec_p2_t6_br", 0 );
		data.setNumber( "ur_ec_p2_t6_sr", 0 );
		data.setNumber( "ur_ec_p2_t6_ub", 1e+038f );
		data.setNumber( "ur_ec_p3_t1_br", 0.22057f );
		data.setNumber( "ur_ec_p3_t1_sr", 0 );
		data.setNumber( "ur_ec_p3_t1_ub", 1e+038f );
		data.setNumber( "ur_ec_p3_t2_br", 0 );
		data.setNumber( "ur_ec_p3_t2_sr", 0 );
		data.setNumber( "ur_ec_p3_t2_ub", 1e+038f );
		data.setNumber( "ur_ec_p3_t3_br", 0 );
		data.setNumber( "ur_ec_p3_t3_sr", 0 );
		data.setNumber( "ur_ec_p3_t3_ub", 1e+038f );
		data.setNumber( "ur_ec_p3_t4_br", 0 );
		data.setNumber( "ur_ec_p3_t4_sr", 0 );
		data.setNumber( "ur_ec_p3_t4_ub", 1e+038f );
		data.setNumber( "ur_ec_p3_t5_br", 0 );
		data.setNumber( "ur_ec_p3_t5_sr", 0 );
		data.setNumber( "ur_ec_p3_t5_ub", 1e+038f );
		data.setNumber( "ur_ec_p3_t6_br", 0 );
		data.setNumber( "ur_ec_p3_t6_sr", 0 );
		data.setNumber( "ur_ec_p3_t6_ub", 1e+038f );
		data.setNumber( "ur_ec_p4_t1_br", 0.08326f );
		data.setNumber( "ur_ec_p4_t1_sr", 0 );
		data.setNumber( "ur_ec_p4_t1_ub", 1e+038f );
		data.setNumber( "ur_ec_p4_t2_br", 0 );
		data.setNumber( "ur_ec_p4_t2_sr", 0 );
		data.setNumber( "ur_ec_p4_t2_ub", 1e+038f );
		data.setNumber( "ur_ec_p4_t3_br", 0 );
		data.setNumber( "ur_ec_p4_t3_sr", 0 );
		data.setNumber( "ur_ec_p4_t3_ub", 1e+038f );
		data.setNumber( "ur_ec_p4_t4_br", 0 );
		data.setNumber( "ur_ec_p4_t4_sr", 0 );
		data.setNumber( "ur_ec_p4_t4_ub", 1e+038f );
		data.setNumber( "ur_ec_p4_t5_br", 0 );
		data.setNumber( "ur_ec_p4_t5_sr", 0 );
		data.setNumber( "ur_ec_p4_t5_ub", 1e+038f );
		data.setNumber( "ur_ec_p4_t6_br", 0 );
		data.setNumber( "ur_ec_p4_t6_sr", 0 );
		data.setNumber( "ur_ec_p4_t6_ub", 1e+038f );
		data.setNumber( "ur_ec_p5_t1_br", 0 );
		data.setNumber( "ur_ec_p5_t1_sr", 0 );
		data.setNumber( "ur_ec_p5_t1_ub", 1e+038f );
		data.setNumber( "ur_ec_p5_t2_br", 0 );
		data.setNumber( "ur_ec_p5_t2_sr", 0 );
		data.setNumber( "ur_ec_p5_t2_ub", 1e+038f );
		data.setNumber( "ur_ec_p5_t3_br", 0 );
		data.setNumber( "ur_ec_p5_t3_sr", 0 );
		data.setNumber( "ur_ec_p5_t3_ub", 1e+038f );
		data.setNumber( "ur_ec_p5_t4_br", 0 );
		data.setNumber( "ur_ec_p5_t4_sr", 0 );
		data.setNumber( "ur_ec_p5_t4_ub", 1e+038f );
		data.setNumber( "ur_ec_p5_t5_br", 0 );
		data.setNumber( "ur_ec_p5_t5_sr", 0 );
		data.setNumber( "ur_ec_p5_t5_ub", 1e+038f );
		data.setNumber( "ur_ec_p5_t6_br", 0 );
		data.setNumber( "ur_ec_p5_t6_sr", 0 );
		data.setNumber( "ur_ec_p5_t6_ub", 1e+038f );
		data.setNumber( "ur_ec_p6_t1_br", 0 );
		data.setNumber( "ur_ec_p6_t1_sr", 0 );
		data.setNumber( "ur_ec_p6_t1_ub", 1e+038f );
		data.setNumber( "ur_ec_p6_t2_br", 0 );
		data.setNumber( "ur_ec_p6_t2_sr", 0 );
		data.setNumber( "ur_ec_p6_t2_ub", 1e+038f );
		data.setNumber( "ur_ec_p6_t3_br", 0 );
		data.setNumber( "ur_ec_p6_t3_sr", 0 );
		data.setNumber( "ur_ec_p6_t3_ub", 1e+038f );
		data.setNumber( "ur_ec_p6_t4_br", 0 );
		data.setNumber( "ur_ec_p6_t4_sr", 0 );
		data.setNumber( "ur_ec_p6_t4_ub", 1e+038f );
		data.setNumber( "ur_ec_p6_t5_br", 0 );
		data.setNumber( "ur_ec_p6_t5_sr", 0 );
		data.setNumber( "ur_ec_p6_t5_ub", 1e+038f );
		data.setNumber( "ur_ec_p6_t6_br", 0 );
		data.setNumber( "ur_ec_p6_t6_sr", 0 );
		data.setNumber( "ur_ec_p6_t6_ub", 1e+038f );
		data.setNumber( "ur_ec_p7_t1_br", 0 );
		data.setNumber( "ur_ec_p7_t1_sr", 0 );
		data.setNumber( "ur_ec_p7_t1_ub", 1e+038f );
		data.setNumber( "ur_ec_p7_t2_br", 0 );
		data.setNumber( "ur_ec_p7_t2_sr", 0 );
		data.setNumber( "ur_ec_p7_t2_ub", 1e+038f );
		data.setNumber( "ur_ec_p7_t3_br", 0 );
		data.setNumber( "ur_ec_p7_t3_sr", 0 );
		data.setNumber( "ur_ec_p7_t3_ub", 1e+038f );
		data.setNumber( "ur_ec_p7_t4_br", 0 );
		data.setNumber( "ur_ec_p7_t4_sr", 0 );
		data.setNumber( "ur_ec_p7_t4_ub", 1e+038f );
		data.setNumber( "ur_ec_p7_t5_br", 0 );
		data.setNumber( "ur_ec_p7_t5_sr", 0 );
		data.setNumber( "ur_ec_p7_t5_ub", 1e+038f );
		data.setNumber( "ur_ec_p7_t6_br", 0 );
		data.setNumber( "ur_ec_p7_t6_sr", 0 );
		data.setNumber( "ur_ec_p7_t6_ub", 1e+038f );
		data.setNumber( "ur_ec_p8_t1_br", 0 );
		data.setNumber( "ur_ec_p8_t1_sr", 0 );
		data.setNumber( "ur_ec_p8_t1_ub", 1e+038f );
		data.setNumber( "ur_ec_p8_t2_br", 0 );
		data.setNumber( "ur_ec_p8_t2_sr", 0 );
		data.setNumber( "ur_ec_p8_t2_ub", 1e+038f );
		data.setNumber( "ur_ec_p8_t3_br", 0 );
		data.setNumber( "ur_ec_p8_t3_sr", 0 );
		data.setNumber( "ur_ec_p8_t3_ub", 1e+038f );
		data.setNumber( "ur_ec_p8_t4_br", 0 );
		data.setNumber( "ur_ec_p8_t4_sr", 0 );
		data.setNumber( "ur_ec_p8_t4_ub", 1e+038f );
		data.setNumber( "ur_ec_p8_t5_br", 0 );
		data.setNumber( "ur_ec_p8_t5_sr", 0 );
		data.setNumber( "ur_ec_p8_t5_ub", 1e+038f );
		data.setNumber( "ur_ec_p8_t6_br", 0 );
		data.setNumber( "ur_ec_p8_t6_sr", 0 );
		data.setNumber( "ur_ec_p8_t6_ub", 1e+038f );
		data.setNumber( "ur_ec_p9_t1_br", 0 );
		data.setNumber( "ur_ec_p9_t1_sr", 0 );
		data.setNumber( "ur_ec_p9_t1_ub", 1e+038f );
		data.setNumber( "ur_ec_p9_t2_br", 0 );
		data.setNumber( "ur_ec_p9_t2_sr", 0 );
		data.setNumber( "ur_ec_p9_t2_ub", 1e+038f );
		data.setNumber( "ur_ec_p9_t3_br", 0 );
		data.setNumber( "ur_ec_p9_t3_sr", 0 );
		data.setNumber( "ur_ec_p9_t3_ub", 1e+038f );
		data.setNumber( "ur_ec_p9_t4_br", 0 );
		data.setNumber( "ur_ec_p9_t4_sr", 0 );
		data.setNumber( "ur_ec_p9_t4_ub", 1e+038f );
		data.setNumber( "ur_ec_p9_t5_br", 0 );
		data.setNumber( "ur_ec_p9_t5_sr", 0 );
		data.setNumber( "ur_ec_p9_t5_ub", 1e+038f );
		data.setNumber( "ur_ec_p9_t6_br", 0 );
		data.setNumber( "ur_ec_p9_t6_sr", 0 );
		data.setNumber( "ur_ec_p9_t6_ub", 1e+038f );
		data.setNumber( "ur_ec_p10_t1_br", 0 );
		data.setNumber( "ur_ec_p10_t1_sr", 0 );
		data.setNumber( "ur_ec_p10_t1_ub", 1e+038f );
		data.setNumber( "ur_ec_p10_t2_br", 0 );
		data.setNumber( "ur_ec_p10_t2_sr", 0 );
		data.setNumber( "ur_ec_p10_t2_ub", 1e+038f );
		data.setNumber( "ur_ec_p10_t3_br", 0 );
		data.setNumber( "ur_ec_p10_t3_sr", 0 );
		data.setNumber( "ur_ec_p10_t3_ub", 1e+038f );
		data.setNumber( "ur_ec_p10_t4_br", 0 );
		data.setNumber( "ur_ec_p10_t4_sr", 0 );
		data.setNumber( "ur_ec_p10_t4_ub", 1e+038f );
		data.setNumber( "ur_ec_p10_t5_br", 0 );
		data.setNumber( "ur_ec_p10_t5_sr", 0 );
		data.setNumber( "ur_ec_p10_t5_ub", 1e+038f );
		data.setNumber( "ur_ec_p10_t6_br", 0 );
		data.setNumber( "ur_ec_p10_t6_sr", 0 );
		data.setNumber( "ur_ec_p10_t6_ub", 1e+038f );
		data.setNumber( "ur_ec_p11_t1_br", 0 );
		data.setNumber( "ur_ec_p11_t1_sr", 0 );
		data.setNumber( "ur_ec_p11_t1_ub", 1e+038f );
		data.setNumber( "ur_ec_p11_t2_br", 0 );
		data.setNumber( "ur_ec_p11_t2_sr", 0 );
		data.setNumber( "ur_ec_p11_t2_ub", 1e+038f );
		data.setNumber( "ur_ec_p11_t3_br", 0 );
		data.setNumber( "ur_ec_p11_t3_sr", 0 );
		data.setNumber( "ur_ec_p11_t3_ub", 1e+038f );
		data.setNumber( "ur_ec_p11_t4_br", 0 );
		data.setNumber( "ur_ec_p11_t4_sr", 0 );
		data.setNumber( "ur_ec_p11_t4_ub", 1e+038f );
		data.setNumber( "ur_ec_p11_t5_br", 0 );
		data.setNumber( "ur_ec_p11_t5_sr", 0 );
		data.setNumber( "ur_ec_p11_t5_ub", 1e+038f );
		data.setNumber( "ur_ec_p11_t6_br", 0 );
		data.setNumber( "ur_ec_p11_t6_sr", 0 );
		data.setNumber( "ur_ec_p11_t6_ub", 1e+038f );
		data.setNumber( "ur_ec_p12_t1_br", 0 );
		data.setNumber( "ur_ec_p12_t1_sr", 0 );
		data.setNumber( "ur_ec_p12_t1_ub", 1e+038f );
		data.setNumber( "ur_ec_p12_t2_br", 0 );
		data.setNumber( "ur_ec_p12_t2_sr", 0 );
		data.setNumber( "ur_ec_p12_t2_ub", 1e+038f );
		data.setNumber( "ur_ec_p12_t3_br", 0 );
		data.setNumber( "ur_ec_p12_t3_sr", 0 );
		data.setNumber( "ur_ec_p12_t3_ub", 1e+038f );
		data.setNumber( "ur_ec_p12_t4_br", 0 );
		data.setNumber( "ur_ec_p12_t4_sr", 0 );
		data.setNumber( "ur_ec_p12_t4_ub", 1e+038f );
		data.setNumber( "ur_ec_p12_t5_br", 0 );
		data.setNumber( "ur_ec_p12_t5_sr", 0 );
		data.setNumber( "ur_ec_p12_t5_ub", 1e+038f );
		data.setNumber( "ur_ec_p12_t6_br", 0 );
		data.setNumber( "ur_ec_p12_t6_sr", 0 );
		data.setNumber( "ur_ec_p12_t6_ub", 1e+038f );
		data.setNumber( "ur_dc_enable", 0 );
		data.setMatrix( "ur_dc_sched_weekday", new float[][]
		{ { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
		{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
		{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
		{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
		{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
		{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
		{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
		{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
		{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
		{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
		{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
		{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 } } );
		data.setMatrix( "ur_dc_sched_weekend", new float[][]
		{ { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
		{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
		{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
		{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
		{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
		{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
		{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
		{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
		{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
		{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
		{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
		{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 } } );
		data.setNumber( "ur_dc_p1_t1_dc", 0 );
		data.setNumber( "ur_dc_p1_t1_ub", 1e+038f );
		data.setNumber( "ur_dc_p1_t2_dc", 0 );
		data.setNumber( "ur_dc_p1_t2_ub", 1e+038f );
		data.setNumber( "ur_dc_p1_t3_dc", 0 );
		data.setNumber( "ur_dc_p1_t3_ub", 1e+038f );
		data.setNumber( "ur_dc_p1_t4_dc", 0 );
		data.setNumber( "ur_dc_p1_t4_ub", 1e+038f );
		data.setNumber( "ur_dc_p1_t5_dc", 0 );
		data.setNumber( "ur_dc_p1_t5_ub", 1e+038f );
		data.setNumber( "ur_dc_p1_t6_dc", 0 );
		data.setNumber( "ur_dc_p1_t6_ub", 1e+038f );
		data.setNumber( "ur_dc_p2_t1_dc", 0 );
		data.setNumber( "ur_dc_p2_t1_ub", 1e+038f );
		data.setNumber( "ur_dc_p2_t2_dc", 0 );
		data.setNumber( "ur_dc_p2_t2_ub", 1e+038f );
		data.setNumber( "ur_dc_p2_t3_dc", 0 );
		data.setNumber( "ur_dc_p2_t3_ub", 1e+038f );
		data.setNumber( "ur_dc_p2_t4_dc", 0 );
		data.setNumber( "ur_dc_p2_t4_ub", 1e+038f );
		data.setNumber( "ur_dc_p2_t5_dc", 0 );
		data.setNumber( "ur_dc_p2_t5_ub", 1e+038f );
		data.setNumber( "ur_dc_p2_t6_dc", 0 );
		data.setNumber( "ur_dc_p2_t6_ub", 1e+038f );
		data.setNumber( "ur_dc_p3_t1_dc", 0 );
		data.setNumber( "ur_dc_p3_t1_ub", 1e+038f );
		data.setNumber( "ur_dc_p3_t2_dc", 0 );
		data.setNumber( "ur_dc_p3_t2_ub", 1e+038f );
		data.setNumber( "ur_dc_p3_t3_dc", 0 );
		data.setNumber( "ur_dc_p3_t3_ub", 1e+038f );
		data.setNumber( "ur_dc_p3_t4_dc", 0 );
		data.setNumber( "ur_dc_p3_t4_ub", 1e+038f );
		data.setNumber( "ur_dc_p3_t5_dc", 0 );
		data.setNumber( "ur_dc_p3_t5_ub", 1e+038f );
		data.setNumber( "ur_dc_p3_t6_dc", 0 );
		data.setNumber( "ur_dc_p3_t6_ub", 1e+038f );
		data.setNumber( "ur_dc_p4_t1_dc", 0 );
		data.setNumber( "ur_dc_p4_t1_ub", 1e+038f );
		data.setNumber( "ur_dc_p4_t2_dc", 0 );
		data.setNumber( "ur_dc_p4_t2_ub", 1e+038f );
		data.setNumber( "ur_dc_p4_t3_dc", 0 );
		data.setNumber( "ur_dc_p4_t3_ub", 1e+038f );
		data.setNumber( "ur_dc_p4_t4_dc", 0 );
		data.setNumber( "ur_dc_p4_t4_ub", 1e+038f );
		data.setNumber( "ur_dc_p4_t5_dc", 0 );
		data.setNumber( "ur_dc_p4_t5_ub", 1e+038f );
		data.setNumber( "ur_dc_p4_t6_dc", 0 );
		data.setNumber( "ur_dc_p4_t6_ub", 1e+038f );
		data.setNumber( "ur_dc_p5_t1_dc", 0 );
		data.setNumber( "ur_dc_p5_t1_ub", 1e+038f );
		data.setNumber( "ur_dc_p5_t2_dc", 0 );
		data.setNumber( "ur_dc_p5_t2_ub", 1e+038f );
		data.setNumber( "ur_dc_p5_t3_dc", 0 );
		data.setNumber( "ur_dc_p5_t3_ub", 1e+038f );
		data.setNumber( "ur_dc_p5_t4_dc", 0 );
		data.setNumber( "ur_dc_p5_t4_ub", 1e+038f );
		data.setNumber( "ur_dc_p5_t5_dc", 0 );
		data.setNumber( "ur_dc_p5_t5_ub", 1e+038f );
		data.setNumber( "ur_dc_p5_t6_dc", 0 );
		data.setNumber( "ur_dc_p5_t6_ub", 1e+038f );
		data.setNumber( "ur_dc_p6_t1_dc", 0 );
		data.setNumber( "ur_dc_p6_t1_ub", 1e+038f );
		data.setNumber( "ur_dc_p6_t2_dc", 0 );
		data.setNumber( "ur_dc_p6_t2_ub", 1e+038f );
		data.setNumber( "ur_dc_p6_t3_dc", 0 );
		data.setNumber( "ur_dc_p6_t3_ub", 1e+038f );
		data.setNumber( "ur_dc_p6_t4_dc", 0 );
		data.setNumber( "ur_dc_p6_t4_ub", 1e+038f );
		data.setNumber( "ur_dc_p6_t5_dc", 0 );
		data.setNumber( "ur_dc_p6_t5_ub", 1e+038f );
		data.setNumber( "ur_dc_p6_t6_dc", 0 );
		data.setNumber( "ur_dc_p6_t6_ub", 1e+038f );
		data.setNumber( "ur_dc_p7_t1_dc", 0 );
		data.setNumber( "ur_dc_p7_t1_ub", 1e+038f );
		data.setNumber( "ur_dc_p7_t2_dc", 0 );
		data.setNumber( "ur_dc_p7_t2_ub", 1e+038f );
		data.setNumber( "ur_dc_p7_t3_dc", 0 );
		data.setNumber( "ur_dc_p7_t3_ub", 1e+038f );
		data.setNumber( "ur_dc_p7_t4_dc", 0 );
		data.setNumber( "ur_dc_p7_t4_ub", 1e+038f );
		data.setNumber( "ur_dc_p7_t5_dc", 0 );
		data.setNumber( "ur_dc_p7_t5_ub", 1e+038f );
		data.setNumber( "ur_dc_p7_t6_dc", 0 );
		data.setNumber( "ur_dc_p7_t6_ub", 1e+038f );
		data.setNumber( "ur_dc_p8_t1_dc", 0 );
		data.setNumber( "ur_dc_p8_t1_ub", 1e+038f );
		data.setNumber( "ur_dc_p8_t2_dc", 0 );
		data.setNumber( "ur_dc_p8_t2_ub", 1e+038f );
		data.setNumber( "ur_dc_p8_t3_dc", 0 );
		data.setNumber( "ur_dc_p8_t3_ub", 1e+038f );
		data.setNumber( "ur_dc_p8_t4_dc", 0 );
		data.setNumber( "ur_dc_p8_t4_ub", 1e+038f );
		data.setNumber( "ur_dc_p8_t5_dc", 0 );
		data.setNumber( "ur_dc_p8_t5_ub", 1e+038f );
		data.setNumber( "ur_dc_p8_t6_dc", 0 );
		data.setNumber( "ur_dc_p8_t6_ub", 1e+038f );
		data.setNumber( "ur_dc_p9_t1_dc", 0 );
		data.setNumber( "ur_dc_p9_t1_ub", 1e+038f );
		data.setNumber( "ur_dc_p9_t2_dc", 0 );
		data.setNumber( "ur_dc_p9_t2_ub", 1e+038f );
		data.setNumber( "ur_dc_p9_t3_dc", 0 );
		data.setNumber( "ur_dc_p9_t3_ub", 1e+038f );
		data.setNumber( "ur_dc_p9_t4_dc", 0 );
		data.setNumber( "ur_dc_p9_t4_ub", 1e+038f );
		data.setNumber( "ur_dc_p9_t5_dc", 0 );
		data.setNumber( "ur_dc_p9_t5_ub", 1e+038f );
		data.setNumber( "ur_dc_p9_t6_dc", 0 );
		data.setNumber( "ur_dc_p9_t6_ub", 1e+038f );
		data.setNumber( "ur_dc_p10_t1_dc", 0 );
		data.setNumber( "ur_dc_p10_t1_ub", 1e+038f );
		data.setNumber( "ur_dc_p10_t2_dc", 0 );
		data.setNumber( "ur_dc_p10_t2_ub", 1e+038f );
		data.setNumber( "ur_dc_p10_t3_dc", 0 );
		data.setNumber( "ur_dc_p10_t3_ub", 1e+038f );
		data.setNumber( "ur_dc_p10_t4_dc", 0 );
		data.setNumber( "ur_dc_p10_t4_ub", 1e+038f );
		data.setNumber( "ur_dc_p10_t5_dc", 0 );
		data.setNumber( "ur_dc_p10_t5_ub", 1e+038f );
		data.setNumber( "ur_dc_p10_t6_dc", 0 );
		data.setNumber( "ur_dc_p10_t6_ub", 1e+038f );
		data.setNumber( "ur_dc_p11_t1_dc", 0 );
		data.setNumber( "ur_dc_p11_t1_ub", 1e+038f );
		data.setNumber( "ur_dc_p11_t2_dc", 0 );
		data.setNumber( "ur_dc_p11_t2_ub", 1e+038f );
		data.setNumber( "ur_dc_p11_t3_dc", 0 );
		data.setNumber( "ur_dc_p11_t3_ub", 1e+038f );
		data.setNumber( "ur_dc_p11_t4_dc", 0 );
		data.setNumber( "ur_dc_p11_t4_ub", 1e+038f );
		data.setNumber( "ur_dc_p11_t5_dc", 0 );
		data.setNumber( "ur_dc_p11_t5_ub", 1e+038f );
		data.setNumber( "ur_dc_p11_t6_dc", 0 );
		data.setNumber( "ur_dc_p11_t6_ub", 1e+038f );
		data.setNumber( "ur_dc_p12_t1_dc", 0 );
		data.setNumber( "ur_dc_p12_t1_ub", 1e+038f );
		data.setNumber( "ur_dc_p12_t2_dc", 0 );
		data.setNumber( "ur_dc_p12_t2_ub", 1e+038f );
		data.setNumber( "ur_dc_p12_t3_dc", 0 );
		data.setNumber( "ur_dc_p12_t3_ub", 1e+038f );
		data.setNumber( "ur_dc_p12_t4_dc", 0 );
		data.setNumber( "ur_dc_p12_t4_ub", 1e+038f );
		data.setNumber( "ur_dc_p12_t5_dc", 0 );
		data.setNumber( "ur_dc_p12_t5_ub", 1e+038f );
		data.setNumber( "ur_dc_p12_t6_dc", 0 );
		data.setNumber( "ur_dc_p12_t6_ub", 1e+038f );
		data.setNumber( "ur_dc_jan_t1_dc", 0 );
		data.setNumber( "ur_dc_jan_t1_ub", 1e+038f );
		data.setNumber( "ur_dc_jan_t2_dc", 0 );
		data.setNumber( "ur_dc_jan_t2_ub", 1e+038f );
		data.setNumber( "ur_dc_jan_t3_dc", 0 );
		data.setNumber( "ur_dc_jan_t3_ub", 1e+038f );
		data.setNumber( "ur_dc_jan_t4_dc", 0 );
		data.setNumber( "ur_dc_jan_t4_ub", 1e+038f );
		data.setNumber( "ur_dc_jan_t5_dc", 0 );
		data.setNumber( "ur_dc_jan_t5_ub", 1e+038f );
		data.setNumber( "ur_dc_jan_t6_dc", 0 );
		data.setNumber( "ur_dc_jan_t6_ub", 1e+038f );
		data.setNumber( "ur_dc_feb_t1_dc", 0 );
		data.setNumber( "ur_dc_feb_t1_ub", 1e+038f );
		data.setNumber( "ur_dc_feb_t2_dc", 0 );
		data.setNumber( "ur_dc_feb_t2_ub", 1e+038f );
		data.setNumber( "ur_dc_feb_t3_dc", 0 );
		data.setNumber( "ur_dc_feb_t3_ub", 1e+038f );
		data.setNumber( "ur_dc_feb_t4_dc", 0 );
		data.setNumber( "ur_dc_feb_t4_ub", 1e+038f );
		data.setNumber( "ur_dc_feb_t5_dc", 0 );
		data.setNumber( "ur_dc_feb_t5_ub", 1e+038f );
		data.setNumber( "ur_dc_feb_t6_dc", 0 );
		data.setNumber( "ur_dc_feb_t6_ub", 1e+038f );
		data.setNumber( "ur_dc_mar_t1_dc", 0 );
		data.setNumber( "ur_dc_mar_t1_ub", 1e+038f );
		data.setNumber( "ur_dc_mar_t2_dc", 0 );
		data.setNumber( "ur_dc_mar_t2_ub", 1e+038f );
		data.setNumber( "ur_dc_mar_t3_dc", 0 );
		data.setNumber( "ur_dc_mar_t3_ub", 1e+038f );
		data.setNumber( "ur_dc_mar_t4_dc", 0 );
		data.setNumber( "ur_dc_mar_t4_ub", 1e+038f );
		data.setNumber( "ur_dc_mar_t5_dc", 0 );
		data.setNumber( "ur_dc_mar_t5_ub", 1e+038f );
		data.setNumber( "ur_dc_mar_t6_dc", 0 );
		data.setNumber( "ur_dc_mar_t6_ub", 1e+038f );
		data.setNumber( "ur_dc_apr_t1_dc", 0 );
		data.setNumber( "ur_dc_apr_t1_ub", 1e+038f );
		data.setNumber( "ur_dc_apr_t2_dc", 0 );
		data.setNumber( "ur_dc_apr_t2_ub", 1e+038f );
		data.setNumber( "ur_dc_apr_t3_dc", 0 );
		data.setNumber( "ur_dc_apr_t3_ub", 1e+038f );
		data.setNumber( "ur_dc_apr_t4_dc", 0 );
		data.setNumber( "ur_dc_apr_t4_ub", 1e+038f );
		data.setNumber( "ur_dc_apr_t5_dc", 0 );
		data.setNumber( "ur_dc_apr_t5_ub", 1e+038f );
		data.setNumber( "ur_dc_apr_t6_dc", 0 );
		data.setNumber( "ur_dc_apr_t6_ub", 1e+038f );
		data.setNumber( "ur_dc_may_t1_dc", 0 );
		data.setNumber( "ur_dc_may_t1_ub", 1e+038f );
		data.setNumber( "ur_dc_may_t2_dc", 0 );
		data.setNumber( "ur_dc_may_t2_ub", 1e+038f );
		data.setNumber( "ur_dc_may_t3_dc", 0 );
		data.setNumber( "ur_dc_may_t3_ub", 1e+038f );
		data.setNumber( "ur_dc_may_t4_dc", 0 );
		data.setNumber( "ur_dc_may_t4_ub", 1e+038f );
		data.setNumber( "ur_dc_may_t5_dc", 0 );
		data.setNumber( "ur_dc_may_t5_ub", 1e+038f );
		data.setNumber( "ur_dc_may_t6_dc", 0 );
		data.setNumber( "ur_dc_may_t6_ub", 1e+038f );
		data.setNumber( "ur_dc_jun_t1_dc", 0 );
		data.setNumber( "ur_dc_jun_t1_ub", 1e+038f );
		data.setNumber( "ur_dc_jun_t2_dc", 0 );
		data.setNumber( "ur_dc_jun_t2_ub", 1e+038f );
		data.setNumber( "ur_dc_jun_t3_dc", 0 );
		data.setNumber( "ur_dc_jun_t3_ub", 1e+038f );
		data.setNumber( "ur_dc_jun_t4_dc", 0 );
		data.setNumber( "ur_dc_jun_t4_ub", 1e+038f );
		data.setNumber( "ur_dc_jun_t5_dc", 0 );
		data.setNumber( "ur_dc_jun_t5_ub", 1e+038f );
		data.setNumber( "ur_dc_jun_t6_dc", 0 );
		data.setNumber( "ur_dc_jun_t6_ub", 1e+038f );
		data.setNumber( "ur_dc_jul_t1_dc", 0 );
		data.setNumber( "ur_dc_jul_t1_ub", 1e+038f );
		data.setNumber( "ur_dc_jul_t2_dc", 0 );
		data.setNumber( "ur_dc_jul_t2_ub", 1e+038f );
		data.setNumber( "ur_dc_jul_t3_dc", 0 );
		data.setNumber( "ur_dc_jul_t3_ub", 1e+038f );
		data.setNumber( "ur_dc_jul_t4_dc", 0 );
		data.setNumber( "ur_dc_jul_t4_ub", 1e+038f );
		data.setNumber( "ur_dc_jul_t5_dc", 0 );
		data.setNumber( "ur_dc_jul_t5_ub", 1e+038f );
		data.setNumber( "ur_dc_jul_t6_dc", 0 );
		data.setNumber( "ur_dc_jul_t6_ub", 1e+038f );
		data.setNumber( "ur_dc_aug_t1_dc", 0 );
		data.setNumber( "ur_dc_aug_t1_ub", 1e+038f );
		data.setNumber( "ur_dc_aug_t2_dc", 0 );
		data.setNumber( "ur_dc_aug_t2_ub", 1e+038f );
		data.setNumber( "ur_dc_aug_t3_dc", 0 );
		data.setNumber( "ur_dc_aug_t3_ub", 1e+038f );
		data.setNumber( "ur_dc_aug_t4_dc", 0 );
		data.setNumber( "ur_dc_aug_t4_ub", 1e+038f );
		data.setNumber( "ur_dc_aug_t5_dc", 0 );
		data.setNumber( "ur_dc_aug_t5_ub", 1e+038f );
		data.setNumber( "ur_dc_aug_t6_dc", 0 );
		data.setNumber( "ur_dc_aug_t6_ub", 1e+038f );
		data.setNumber( "ur_dc_sep_t1_dc", 0 );
		data.setNumber( "ur_dc_sep_t1_ub", 1e+038f );
		data.setNumber( "ur_dc_sep_t2_dc", 0 );
		data.setNumber( "ur_dc_sep_t2_ub", 1e+038f );
		data.setNumber( "ur_dc_sep_t3_dc", 0 );
		data.setNumber( "ur_dc_sep_t3_ub", 1e+038f );
		data.setNumber( "ur_dc_sep_t4_dc", 0 );
		data.setNumber( "ur_dc_sep_t4_ub", 1e+038f );
		data.setNumber( "ur_dc_sep_t5_dc", 0 );
		data.setNumber( "ur_dc_sep_t5_ub", 1e+038f );
		data.setNumber( "ur_dc_sep_t6_dc", 0 );
		data.setNumber( "ur_dc_sep_t6_ub", 1e+038f );
		data.setNumber( "ur_dc_oct_t1_dc", 0 );
		data.setNumber( "ur_dc_oct_t1_ub", 1e+038f );
		data.setNumber( "ur_dc_oct_t2_dc", 0 );
		data.setNumber( "ur_dc_oct_t2_ub", 1e+038f );
		data.setNumber( "ur_dc_oct_t3_dc", 0 );
		data.setNumber( "ur_dc_oct_t3_ub", 1e+038f );
		data.setNumber( "ur_dc_oct_t4_dc", 0 );
		data.setNumber( "ur_dc_oct_t4_ub", 1e+038f );
		data.setNumber( "ur_dc_oct_t5_dc", 0 );
		data.setNumber( "ur_dc_oct_t5_ub", 1e+038f );
		data.setNumber( "ur_dc_oct_t6_dc", 0 );
		data.setNumber( "ur_dc_oct_t6_ub", 1e+038f );
		data.setNumber( "ur_dc_nov_t1_dc", 0 );
		data.setNumber( "ur_dc_nov_t1_ub", 1e+038f );
		data.setNumber( "ur_dc_nov_t2_dc", 0 );
		data.setNumber( "ur_dc_nov_t2_ub", 1e+038f );
		data.setNumber( "ur_dc_nov_t3_dc", 0 );
		data.setNumber( "ur_dc_nov_t3_ub", 1e+038f );
		data.setNumber( "ur_dc_nov_t4_dc", 0 );
		data.setNumber( "ur_dc_nov_t4_ub", 1e+038f );
		data.setNumber( "ur_dc_nov_t5_dc", 0 );
		data.setNumber( "ur_dc_nov_t5_ub", 1e+038f );
		data.setNumber( "ur_dc_nov_t6_dc", 0 );
		data.setNumber( "ur_dc_nov_t6_ub", 1e+038f );
		data.setNumber( "ur_dc_dec_t1_dc", 0 );
		data.setNumber( "ur_dc_dec_t1_ub", 1e+038f );
		data.setNumber( "ur_dc_dec_t2_dc", 0 );
		data.setNumber( "ur_dc_dec_t2_ub", 1e+038f );
		data.setNumber( "ur_dc_dec_t3_dc", 0 );
		data.setNumber( "ur_dc_dec_t3_ub", 1e+038f );
		data.setNumber( "ur_dc_dec_t4_dc", 0 );
		data.setNumber( "ur_dc_dec_t4_ub", 1e+038f );
		data.setNumber( "ur_dc_dec_t5_dc", 0 );
		data.setNumber( "ur_dc_dec_t5_ub", 1e+038f );
		data.setNumber( "ur_dc_dec_t6_dc", 0 );
		data.setNumber( "ur_dc_dec_t6_ub", 1e+038f );


       	if (mod.exec(data))
        {
            float[] salespurchases = data.getArray("year1_monthly_salespurchases");
            float ns = data.getNumber("savings_year1");
	        System.out.println("Year 1 monthly sales/purchases with system : ");
            for (int i = 0; i < salespurchases.length; i++)
                System.out.println("[" + i + "]: $" + salespurchases[i] );

            System.out.println("Net savings : $" + ns);
	     	System.out.println("utilityrate3 example OK");
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
            System.out.println("utilityrate3 example failed");
        }
    }

    public static void main(String[] args) throws Exception
    {
        // address dll path issues (if necessary)
        //e.g. addLibraryPath( "C:\\Projects\\SAM\\VS2012\\ssc\\java");
        System.loadLibrary("SSCAPIJNI");
		UtilityRate3();
    }


}
