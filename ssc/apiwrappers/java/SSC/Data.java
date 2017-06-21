package SSC;

public class Data 
{
    private long m_data;
    private boolean m_owned;


    public Data()
    {
        m_data = SSC.SSCAPIJNI.ssc_data_create();
        m_owned = true;
    }

    public Data( long dataRefNotOwned )
    {
        m_data = dataRefNotOwned;
        m_owned = false;
    }

    protected void finalize() 
    {
       delete();
    }

    public synchronized void delete() 
    {
      if (m_owned && m_data != 0)
          SSC.SSCAPIJNI.ssc_module_free(m_data);
    }

    public void clear()
    {
        SSC.SSCAPIJNI.ssc_data_clear(m_data);
    }

    public String first()
    {
        return SSC.SSCAPIJNI.ssc_data_first(m_data);
    }

    public String next()
    {
        return SSC.SSCAPIJNI.ssc_data_next(m_data);
    }

    public int query(String name)
    {
        return SSC.SSCAPIJNI.ssc_data_query(m_data, name);
    }

    public void setNumber(String name, float value)
    {
        SSC.SSCAPIJNI.ssc_data_set_number(m_data, name, value);
    }

    public float getNumber(String name)
    {
        float[] value = {0};
        int ret = SSC.SSCAPIJNI.ssc_data_get_number(m_data, name, value);
        if (ret==0)
        {
            value[0] = Float.NaN;
        }
        return value[0];
    }

    public void setString(String name, String value)
    {
        SSC.SSCAPIJNI.ssc_data_set_string(m_data, name, value);
    }

    public String getString(String name)
    {
        return SSC.SSCAPIJNI.ssc_data_get_string(m_data, name);
    }

    public void setArray(String name, float[] data)
    {
        SSC.SSCAPIJNI.ssc_data_set_array(m_data, name, data, data.length);
    }

    public float[] getArray(String name)
    {
        return SSC.SSCAPIJNI.ssc_data_get_array(m_data, name);
    }

    public void setMatrix(String name, float[][] mat)
    {
        int nRows = mat.length;
        if (nRows<1) return;
        int nCols = mat[0].length;
        float[] jniMat = new float[nRows*nCols];
        for (int r=0; r<mat.length; r++)
        {
            for (int c=0; c<mat[r].length; c++)
            {
                jniMat[r*nCols+c]=mat[r][c];
            }
        }
        SSC.SSCAPIJNI.ssc_data_set_matrix(m_data, name, jniMat, nRows, nCols);
    }

    public float[][] getMatrix(String name)
    {
        int[] nDims = {0,0};
        float[] arr = SSC.SSCAPIJNI.ssc_data_get_matrix(m_data, name, nDims);
        if (nDims[0] * nDims[1] > 0)
        {
            float[][] mat = new float[nDims[0]][nDims[1]];
            for (int r = 0; r < nDims[0]; r++)
            {
                for (int c = 0; c < nDims[1]; c++)
                {
                    mat[r][c] = arr[r * nDims[1] + c];
                }
            }
            return mat;
        }
        else
        {
            return null;
        }
    }

    public void setTable(String name, Data table)
    {
        SSC.SSCAPIJNI.ssc_data_set_table(m_data, name, table.getDataHandle());
    }

    public Data getTable(String name)
    {
        long p = SSC.SSCAPIJNI.ssc_data_get_table(m_data, name);
        if (p==0)
            return null;
        else
            return new Data( p );
    }

    public long getDataHandle()
    {
        return m_data;
    }
}
