
import java.lang.reflect.Field;
import java.util.Arrays;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;

public class TestCashLoan {

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
			sFile = new Scanner(new File("TestCashLoan.data"));

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


    public static void CashLoan() throws FileNotFoundException
    {
        System.out.println("\ncashloan example using PV Residential defaults from 2014.11.24 release");
        SSC.Module mod = new SSC.Module("cashloan");

		GetData();
        System.out.println("cashloan example names and data length " + OutputNames.size() + "," + OutputData.size());
		for (int i = 0; i<OutputData.size(); i++)
		{
            System.out.println("cashloan example data " + i + " length " + OutputData.get(i).size());
		}

		SSC.Data data = new SSC.Data();


		data.setNumber( "analysis_period", 25 );
		data.setNumber( "federal_tax_rate", 30 );
		data.setNumber( "state_tax_rate", 7 );
		data.setNumber( "property_tax_rate", 1 );
		data.setNumber( "prop_tax_cost_assessed_percent", 100 );
		data.setNumber( "prop_tax_assessed_decline", 0 );
		data.setNumber( "sales_tax_rate", 5 );
		data.setNumber( "real_discount_rate", 5.5f );
		data.setNumber( "inflation_rate", 2.5f );
		data.setNumber( "insurance_rate", 1 );
		data.setNumber( "system_capacity", 3.8745f );
		data.setNumber( "loan_term", 25 );
		data.setNumber( "loan_rate", 5 );
		data.setNumber( "debt_fraction", 100 );
		data.setArray( "om_fixed", new float[] {0} );
		data.setNumber( "om_fixed_escal", 0 );
		data.setArray( "om_production", new float[] { 0 } );
		data.setNumber( "om_production_escal", 0 );
		data.setArray( "om_capacity", new float[] { 20 } );
		data.setNumber( "om_capacity_escal", 0 );
		data.setArray( "om_fuel_cost", new float[] { 0 } );
		data.setNumber( "om_fuel_cost_escal", 0 );
		data.setNumber( "itc_fed_amount", 0 );
		data.setNumber( "itc_fed_amount_deprbas_fed", 1 );
		data.setNumber( "itc_fed_amount_deprbas_sta", 1 );
		data.setNumber( "itc_sta_amount", 0 );
		data.setNumber( "itc_sta_amount_deprbas_fed", 0 );
		data.setNumber( "itc_sta_amount_deprbas_sta", 0 );
		data.setNumber( "itc_fed_percent", 30 );
		data.setNumber( "itc_fed_percent_maxvalue", 1e+038f );
		data.setNumber( "itc_fed_percent_deprbas_fed", 1 );
		data.setNumber( "itc_fed_percent_deprbas_sta", 1 );
		data.setNumber( "itc_sta_percent", 25 );
		data.setNumber( "itc_sta_percent_maxvalue", 1e+038f );
		data.setNumber( "itc_sta_percent_deprbas_fed", 0 );
		data.setNumber( "itc_sta_percent_deprbas_sta", 0 );
		data.setArray( "ptc_fed_amount", new float[] { 0 } );
		data.setNumber( "ptc_fed_term", 10 );
		data.setNumber( "ptc_fed_escal", 0 );
		data.setArray( "ptc_sta_amount", new float[] { 0 } );
		data.setNumber( "ptc_sta_term", 10 );
		data.setNumber( "ptc_sta_escal", 0 );
		data.setNumber( "ibi_fed_amount", 0 );
		data.setNumber( "ibi_fed_amount_tax_fed", 1 );
		data.setNumber( "ibi_fed_amount_tax_sta", 1 );
		data.setNumber( "ibi_fed_amount_deprbas_fed", 0 );
		data.setNumber( "ibi_fed_amount_deprbas_sta", 0 );
		data.setNumber( "ibi_sta_amount", 0 );
		data.setNumber( "ibi_sta_amount_tax_fed", 1 );
		data.setNumber( "ibi_sta_amount_tax_sta", 1 );
		data.setNumber( "ibi_sta_amount_deprbas_fed", 0 );
		data.setNumber( "ibi_sta_amount_deprbas_sta", 0 );
		data.setNumber( "ibi_uti_amount", 0 );
		data.setNumber( "ibi_uti_amount_tax_fed", 1 );
		data.setNumber( "ibi_uti_amount_tax_sta", 1 );
		data.setNumber( "ibi_uti_amount_deprbas_fed", 0 );
		data.setNumber( "ibi_uti_amount_deprbas_sta", 0 );
		data.setNumber( "ibi_oth_amount", 0 );
		data.setNumber( "ibi_oth_amount_tax_fed", 1 );
		data.setNumber( "ibi_oth_amount_tax_sta", 1 );
		data.setNumber( "ibi_oth_amount_deprbas_fed", 0 );
		data.setNumber( "ibi_oth_amount_deprbas_sta", 0 );
		data.setNumber( "ibi_fed_percent", 0 );
		data.setNumber( "ibi_fed_percent_maxvalue", 1e+038f );
		data.setNumber( "ibi_fed_percent_tax_fed", 1 );
		data.setNumber( "ibi_fed_percent_tax_sta", 1 );
		data.setNumber( "ibi_fed_percent_deprbas_fed", 0 );
		data.setNumber( "ibi_fed_percent_deprbas_sta", 0 );
		data.setNumber( "ibi_sta_percent", 0 );
		data.setNumber( "ibi_sta_percent_maxvalue", 1e+038f );
		data.setNumber( "ibi_sta_percent_tax_fed", 1 );
		data.setNumber( "ibi_sta_percent_tax_sta", 1 );
		data.setNumber( "ibi_sta_percent_deprbas_fed", 0 );
		data.setNumber( "ibi_sta_percent_deprbas_sta", 0 );
		data.setNumber( "ibi_uti_percent", 0 );
		data.setNumber( "ibi_uti_percent_maxvalue", 1e+038f );
		data.setNumber( "ibi_uti_percent_tax_fed", 1 );
		data.setNumber( "ibi_uti_percent_tax_sta", 1 );
		data.setNumber( "ibi_uti_percent_deprbas_fed", 0 );
		data.setNumber( "ibi_uti_percent_deprbas_sta", 0 );
		data.setNumber( "ibi_oth_percent", 0 );
		data.setNumber( "ibi_oth_percent_maxvalue", 1e+038f );
		data.setNumber( "ibi_oth_percent_tax_fed", 1 );
		data.setNumber( "ibi_oth_percent_tax_sta", 1 );
		data.setNumber( "ibi_oth_percent_deprbas_fed", 0 );
		data.setNumber( "ibi_oth_percent_deprbas_sta", 0 );
		data.setNumber( "cbi_fed_amount", 0 );
		data.setNumber( "cbi_fed_maxvalue", 1e+038f );
		data.setNumber( "cbi_fed_tax_fed", 1 );
		data.setNumber( "cbi_fed_tax_sta", 1 );
		data.setNumber( "cbi_fed_deprbas_fed", 0 );
		data.setNumber( "cbi_fed_deprbas_sta", 0 );
		data.setNumber( "cbi_sta_amount", 0 );
		data.setNumber( "cbi_sta_maxvalue", 1e+038f );
		data.setNumber( "cbi_sta_tax_fed", 1 );
		data.setNumber( "cbi_sta_tax_sta", 1 );
		data.setNumber( "cbi_sta_deprbas_fed", 0 );
		data.setNumber( "cbi_sta_deprbas_sta", 0 );
		data.setNumber( "cbi_uti_amount", 0 );
		data.setNumber( "cbi_uti_maxvalue", 1e+038f );
		data.setNumber( "cbi_uti_tax_fed", 1 );
		data.setNumber( "cbi_uti_tax_sta", 1 );
		data.setNumber( "cbi_uti_deprbas_fed", 0 );
		data.setNumber( "cbi_uti_deprbas_sta", 0 );
		data.setNumber( "cbi_oth_amount", 0 );
		data.setNumber( "cbi_oth_maxvalue", 1e+038f );
		data.setNumber( "cbi_oth_tax_fed", 1 );
		data.setNumber( "cbi_oth_tax_sta", 1 );
		data.setNumber( "cbi_oth_deprbas_fed", 0 );
		data.setNumber( "cbi_oth_deprbas_sta", 0 );
		data.setArray( "pbi_fed_amount", new float[] { 0 } );
		data.setNumber( "pbi_fed_term", 0 );
		data.setNumber( "pbi_fed_escal", 0 );
		data.setNumber( "pbi_fed_tax_fed", 1 );
		data.setNumber( "pbi_fed_tax_sta", 1 );
		data.setArray( "pbi_sta_amount", new float[] { 0 } );
		data.setNumber( "pbi_sta_term", 0 );
		data.setNumber( "pbi_sta_escal", 0 );
		data.setNumber( "pbi_sta_tax_fed", 1 );
		data.setNumber( "pbi_sta_tax_sta", 1 );
		data.setArray( "pbi_uti_amount", new float[] { 0 } );
		data.setNumber( "pbi_uti_term", 0 );
		data.setNumber( "pbi_uti_escal", 0 );
		data.setNumber( "pbi_uti_tax_fed", 1 );
		data.setNumber( "pbi_uti_tax_sta", 1 );
		data.setArray( "pbi_oth_amount", new float[] { 0 } );
		data.setNumber( "pbi_oth_term", 0 );
		data.setNumber( "pbi_oth_escal", 0 );
		data.setNumber( "pbi_oth_tax_fed", 1 );
		data.setNumber( "pbi_oth_tax_sta", 1 );
		data.setNumber( "market", 0 );
		data.setNumber( "mortgage", 1 );
		data.setNumber( "total_installed_cost", 12746.7f );
		data.setNumber( "salvage_percentage", 0 );
		data.setArray("annual_energy_value",GetFloatArray("annual_energy_value"));
		data.setArray("hourly_energy",GetFloatArray("hourly_energy"));
		data.setArray( "degradation", new float[] { 0.5f } );
		data.setNumber( "system_use_lifetime_output", 0 );

       	if (mod.exec(data))
        {
 		     float lcoe = data.getNumber("lcoe_nom");
		     float npv = data.getNumber("npv");
		     System.out.println("Levelized COE (nominal) : " + lcoe + " cents/kWh");
		     System.out.println("Net present value : $" + npv);
	     	System.out.println("cashloan example OK");
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
            System.out.println("cashloan example failed");
        }
    }

    public static void main(String[] args) throws Exception
    {
        // address dll path issues (if necessary)
        //e.g. addLibraryPath( "C:\\Projects\\SAM\\VS2012\\ssc\\java");
        System.loadLibrary("SSCAPIJNI");
		CashLoan();
    }


}
