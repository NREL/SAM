package SSC;

public class API 
{
    // constants for return value of Info.VarType() (see sscapi.h)
     public static int INPUT = 1;
     public static int OUTPUT = 2;
     public static int INOUT = 3;


     // constants for out integer type in Module.Log() method (see sscapi.h)
     public static int NOTICE = 1;
     public static int WARNING = 2;
     public static int ERROR = 3;


     // constants for return value of Data.Query() and Info.DataType() (see sscapi.h)
     public static int INVALID = 0;
     public static int STRING = 1;
     public static int NUMBER = 2;
     public static int ARRAY = 3;
     public static int MATRIX = 4;
     public static int TABLE = 5;

     public int Version()
     {
        return SSC.SSCAPIJNI.ssc_version();
     }

     public String BuildInfo()
     {
        return SSC.SSCAPIJNI.ssc_build_info();
     }

}
