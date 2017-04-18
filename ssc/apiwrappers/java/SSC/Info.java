package SSC;

public class Info 
{
    private long m_inf;
    private SSC.Module m_mod;
    private int m_idx;

    public Info(SSC.Module m)
    {
        m_mod = m;
        m_idx = 0;
    }

    public void reset()
    {
        m_idx = 0;
    }

    public boolean get()
    {
        m_inf = SSC.SSCAPIJNI.ssc_module_var_info(m_mod.getModuleHandle(), m_idx);
        if (m_inf == 0)
        {
            reset();
            return false;
        }

        m_idx++;
        return true;
    }

    public String name()
    {
        if (m_inf == 0) return null;
        return SSC.SSCAPIJNI.ssc_info_name(m_inf);
    }

    public int varType()
    {
        if (m_inf == 0) return -1;
        return SSC.SSCAPIJNI.ssc_info_var_type(m_inf);
    }

    public int dataType()
    {
        if (m_inf == 0) return -1;
        return SSC.SSCAPIJNI.ssc_info_data_type(m_inf);
    }

    public String label()
    {
        if (m_inf == 0) return null;
        return SSC.SSCAPIJNI.ssc_info_label(m_inf);
    }

    public String units()
    {
        if (m_inf == 0) return null;
        return SSC.SSCAPIJNI.ssc_info_units(m_inf);
    }

    public String meta()
    {
        if (m_inf == 0) return null;
        return SSC.SSCAPIJNI.ssc_info_meta(m_inf);
    }

    public String group()
    {
        if (m_inf == 0) return null;
        return SSC.SSCAPIJNI.ssc_info_group(m_inf);
    }

    public String required()
    {
        if (m_inf == 0) return null;
        return SSC.SSCAPIJNI.ssc_info_required(m_inf);
    }

    public String constraints()
    {
        if (m_inf == 0) return null;
        return SSC.SSCAPIJNI.ssc_info_constraints(m_inf);
    }
}
