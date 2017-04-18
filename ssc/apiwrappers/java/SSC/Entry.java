package SSC;

public class Entry 
{
    private long m_entry;
    private int m_idx;

    public Entry()
    {
        m_idx = 0;
    }

    public void reset()
    {
        m_idx = 0;
    }

    public boolean get()
    {
        m_entry = SSC.SSCAPIJNI.ssc_module_entry(m_idx);
        if (m_entry == 0)
        {
            reset();
            return false;
        }

        m_idx++;
        return true;
    }

    public String name()
    {
        if (m_entry != 0)
            return SSC.SSCAPIJNI.ssc_entry_name(m_entry);
        else 
            return null;
    }

    public String description()
    {
        if (m_entry != 0)
            return SSC.SSCAPIJNI.ssc_entry_description(m_entry);
        else
            return null;
    }

    public int version()
    {
        if (m_entry != 0)
            return SSC.SSCAPIJNI.ssc_entry_version(m_entry);
        else
            return -1;
    }
    
}
