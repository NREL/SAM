package SSC;

public class Module 
{
    private long m_mod;

    public Module(String name)
    {
        m_mod = SSC.SSCAPIJNI.ssc_module_create(name);
    }

    protected void finalize() 
    {
       delete();
    }

    public synchronized void delete() 
    {
      if (m_mod != 0)
          SSC.SSCAPIJNI.ssc_module_free(m_mod);
    }

    public boolean isOk()
    {
        return m_mod != 0;
    }

    public long getModuleHandle()
    {
        return m_mod;
    }

    public boolean exec( Data data )
    {
        return (SSC.SSCAPIJNI.ssc_module_exec(m_mod, data.getDataHandle()) != 0);
    }

    public boolean log(int idx, String msg, int[] type, float[] time)
    {
        msg = SSC.SSCAPIJNI.ssc_module_log(m_mod, idx, type, time);
        if (msg != null)
            return true;
        else
            return false;
    }
}
