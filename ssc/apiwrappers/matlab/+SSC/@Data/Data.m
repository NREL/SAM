classdef Data < handle
    %Data object from SSC
    %   Object used to manipulate ssc data - get and set values, arrays and tables
    
    properties (SetAccess = private)
        m_data
        m_owned
    end
    
    methods
        function obj = Data(varargin)
            if nargin > 2,
                obj.m_data = varargin{1};
                obj.m_owned = false;
            else
                obj.m_data = SSC.ssccall('data_create');
                obj.m_owned = true;
            end
        end
        function delete(obj)
            if ( obj.m_owned && (obj.m_data ~= 0)),
                SSC.ssccall('data_free', obj.m_data);
            end
        end
        function Clear(obj)
            SSC.ssccall('data_clear', obj.m_data);
        end
        
        function result = First(obj)
            result = SSC.ssccall('data_first', obj.m_data);
        end
        function result = Next(obj)
            result = SSC.ssccall('data_next', obj.m_data);
        end
        function result = Query(obj, name)
            result = SSC.ssccall('data_query', obj.m_data, name);
        end
        function SetNumber(obj, name, value)
           SSC.ssccall('data_set_number', obj.m_data, name, value);
        end
        function result = GetNumber(obj, name)
            result = NaN('single');
            result = SSC.ssccall('data_get_number', obj.m_data, name);
        end
        function SetString(obj, name, value)
            SSC.ssccall('data_set_string', obj.m_data, name, value);
        end
        function result = GetString(obj, name)
            result = SSC.ssccall('data_get_string', obj.m_data, name);
        end
        function SetArray(obj, name, data)
            SSC.ssccall('data_set_array', obj.m_data, name, data);
        end
        function result = GetArray(obj, name)
            result = SSC.ssccall('data_get_array', obj.m_data, name);
        end  
        function SetMatrix(obj, name, mat)
            SSC.ssccall('data_set_matrix', obj.m_data, name, mat);
        end
        function result = GetMatrix(obj, name)
            result = SSC.ssccall('data_get_matrix', obj.m_data, name);
        end        
        function SetTable(obj, name, table)
            SSC.ssccall('data_set_table', obj.m_data, name, table);
        end
        function result = GetTable(obj, name)
            result = SSC.ssccall('data_get_table', obj.m_data, name);
        end
        function result = GetHandle(obj)
            result = obj.m_data;
        end         
    end
    
end

